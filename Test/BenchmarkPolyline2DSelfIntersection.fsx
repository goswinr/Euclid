#I "../bin/Release/net6.0"
#r "Euclid.dll"
#r "nuget: BenchmarkDotNet"

// Run from the repository root:
//   dotnet fsi Test\BenchmarkPolyline2DSelfIntersection.fsx
//
// This script compares the current Polyline2D.tryFindSelfIntersection implementation
// against the older/simple version copied below, which does not prefilter segment pairs
// with BRect overlap checks.
//
// Expected trend from a ShortRun on circular closed polylines:
// | Method                       | PointCount | Mean          | Error        | StdDev       | Ratio | RatioSD | Gen0   | Allocated | Alloc Ratio |
// |----------------------------- |----------- |--------------:|-------------:|-------------:|------:|--------:|-------:|----------:|------------:|
// | TryFindSelfIntersectionSmall | 5          |      21.73 ns |     42.50 ns |     2.330 ns |  1.01 |    0.13 | 0.0039 |     120 B |        1.00 |
// | TryFindSelfIntersectionBig   | 5          |      41.11 ns |     51.55 ns |     2.826 ns |  1.91 |    0.20 | 0.0128 |     432 B |        3.60 |
// |                              |            |               |              |              |       |         |        |           |             |
// | TryFindSelfIntersectionSmall | 20         |     236.69 ns |     78.37 ns |     4.296 ns |  1.00 |    0.02 | 0.0019 |     360 B |        1.00 |
// | TryFindSelfIntersectionBig   | 20         |     243.67 ns |    400.91 ns |    21.975 ns |  1.03 |    0.08 | 0.0517 |    1632 B |        4.53 |
// |                              |            |               |              |              |       |         |        |           |             |
// | TryFindSelfIntersectionSmall | 30         |     572.11 ns |  1,091.18 ns |    59.811 ns |  1.01 |    0.13 | 0.0172 |     520 B |        1.00 |
// | TryFindSelfIntersectionBig   | 30         |     454.98 ns |    384.14 ns |    21.056 ns |  0.80 |    0.08 | 0.0744 |    2432 B |        4.68 |
// |                              |            |               |              |              |       |         |        |           |             |
// | TryFindSelfIntersectionSmall | 50         |   1,465.43 ns |    400.31 ns |    21.942 ns |  1.00 |    0.02 | 0.0267 |     840 B |        1.00 |
// | TryFindSelfIntersectionBig   | 50         |   1,179.77 ns |  1,912.03 ns |   104.805 ns |  0.81 |    0.06 | 0.0210 |    4032 B |        4.80 |
// |                              |            |               |              |              |       |         |        |           |             |
// | TryFindSelfIntersectionSmall | 200        |  23,138.75 ns |  1,863.28 ns |   102.133 ns |  1.00 |    0.01 |      - |    3240 B |        1.00 |
// | TryFindSelfIntersectionBig   | 200        |  16,793.25 ns |  4,796.17 ns |   262.894 ns |  0.73 |    0.01 | 0.0610 |   16032 B |        4.95 |
// |                              |            |               |              |              |       |         |        |           |             |
// | TryFindSelfIntersectionSmall | 500        | 150,486.46 ns | 59,879.18 ns | 3,282.180 ns |  1.00 |    0.03 |      - |    8041 B |        1.00 |
// | TryFindSelfIntersectionBig   | 500        |  94,515.68 ns | 34,150.20 ns | 1,871.888 ns |  0.63 |    0.02 | 0.1221 |   40032 B |        4.98 |

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Toolchains.InProcess.Emit
open Euclid



[<MemoryDiagnoser>]
type Polyline2DSelfIntersectionBenchmarks() =

    let mutable circle = Unchecked.defaultof<Polyline2D>

    [<Params(5, 20, 30 , 50, 200,  500)>]
    member val PointCount = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let radius = 100.0
        let segmentCount = this.PointCount - 1

        let points =
            [| for i in 0 .. segmentCount ->
                let angle = 2.0 * Math.PI * float i / float segmentCount
                Pt(radius * cos angle, radius * sin angle) |]

        circle <- Polyline2D.create points

    [<Benchmark(Baseline = true)>]
    member _.TryFindSelfIntersectionSmall() =
        Polyline2D.tryFindSelfIntersectionSmall circle

    [<Benchmark>]
    member _.TryFindSelfIntersectionBig() =
        Polyline2D.tryFindSelfIntersectionBig circle

let config =
    DefaultConfig.Instance
        .AddJob(Job.ShortRun.WithToolchain(InProcessEmitToolchain.Instance))

let args =
    let rawArgs = Environment.GetCommandLineArgs()
    match rawArgs |> Array.tryFindIndex (fun arg -> arg.EndsWith(".fsx", StringComparison.OrdinalIgnoreCase)) with
    | Some scriptIndex when scriptIndex + 1 < rawArgs.Length ->
        rawArgs
        |> Array.skip (scriptIndex + 1)
        |> Array.filter ((<>) "--")
    | _ ->
        Array.empty

if args.Length = 0 then
    BenchmarkRunner.Run<Polyline2DSelfIntersectionBenchmarks>(config) |> ignore
else
    BenchmarkSwitcher.FromTypes([| typeof<Polyline2DSelfIntersectionBenchmarks> |]).Run(args, config) |> ignore
