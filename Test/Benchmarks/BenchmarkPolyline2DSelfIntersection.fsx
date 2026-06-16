
// before running this benchmark, uncomment tryFindSelfIntersectionSmallOLD in Polyline2D.fs
//
// and build in release mode to include the old version as a third method for comparison.


#I "../../bin/Release/net6.0"
#r "Euclid.dll" // needs to be compiled with BENCHMARK compilation symbol
#r "nuget: BenchmarkDotNet"

// Run from the repository root:
//   dotnet fsi Test\BenchmarkPolyline2DSelfIntersection.fsx
//
// This script compares the current Polyline2D.tryFindSelfIntersection implementation
// against the older/simple version copied below, which does not prefilter segment pairs
// with BRect overlap checks.

(*
| Method                               | PointCount | Mean          | Error         | StdDev       | Ratio | RatioSD | Gen0   | Allocated | Alloc Ratio |
|------------------------------------- |----------- |--------------:|--------------:|-------------:|------:|--------:|-------:|----------:|------------:|
| TryFindSelfIntersection              | 5          |      27.92 ns |     13.252 ns |     0.726 ns |  1.00 |    0.03 | 0.0025 |     120 B |        1.00 |
| TryFindSelfIntersectionWithBRect     | 5          |      64.12 ns |     21.205 ns |     1.162 ns |  2.30 |    0.06 | 0.0024 |     432 B |        3.60 |
| TryFindSelfIntersectionWithPtObjects | 5          |      27.88 ns |      9.563 ns |     0.524 ns |  1.00 |    0.03 | 0.0025 |     120 B |        1.00 |
|                                      |            |               |               |              |       |         |        |           |             |
| TryFindSelfIntersection              | 20         |     363.00 ns |    114.009 ns |     6.249 ns |  1.00 |    0.02 | 0.0014 |     360 B |        1.00 |
| TryFindSelfIntersectionWithBRect     | 20         |     430.48 ns |     46.492 ns |     2.548 ns |  1.19 |    0.02 | 0.0095 |    1632 B |        4.53 |
| TryFindSelfIntersectionWithPtObjects | 20         |     356.73 ns |    139.230 ns |     7.632 ns |  0.98 |    0.02 | 0.0014 |     360 B |        1.00 |
|                                      |            |               |               |              |       |         |        |           |             |
| TryFindSelfIntersection              | 30         |     774.91 ns |     41.756 ns |     2.289 ns |  1.00 |    0.00 | 0.0019 |     520 B |        1.00 |
| TryFindSelfIntersectionWithBRect     | 30         |     804.70 ns |    226.044 ns |    12.390 ns |  1.04 |    0.01 | 0.0134 |    2432 B |        4.68 |
| TryFindSelfIntersectionWithPtObjects | 30         |     755.04 ns |    106.448 ns |     5.835 ns |  0.97 |    0.01 | 0.0019 |     520 B |        1.00 |
|                                      |            |               |               |              |       |         |        |           |             |
| TryFindSelfIntersection              | 50         |   2,239.62 ns |    337.260 ns |    18.486 ns |  1.00 |    0.01 | 0.0038 |     840 B |        1.00 |
| TryFindSelfIntersectionWithBRect     | 50         |   2,025.44 ns |    319.147 ns |    17.494 ns |  0.90 |    0.01 | 0.0191 |    4032 B |        4.80 |
| TryFindSelfIntersectionWithPtObjects | 50         |   2,128.01 ns |    320.310 ns |    17.557 ns |  0.95 |    0.01 | 0.0038 |     840 B |        1.00 |
|                                      |            |               |               |              |       |         |        |           |             |
| TryFindSelfIntersection              | 200        |  34,362.80 ns |  1,848.270 ns |   101.310 ns |  1.00 |    0.00 |      - |    3240 B |        1.00 |
| TryFindSelfIntersectionWithBRect     | 200        |  25,535.79 ns |  5,037.963 ns |   276.148 ns |  0.74 |    0.01 | 0.0610 |   16032 B |        4.95 |
| TryFindSelfIntersectionWithPtObjects | 200        |  34,415.68 ns |  2,756.574 ns |   151.097 ns |  1.00 |    0.00 |      - |    3240 B |        1.00 |
|                                      |            |               |               |              |       |         |        |           |             |
| TryFindSelfIntersection              | 500        | 212,771.84 ns | 57,775.557 ns | 3,166.873 ns |  1.00 |    0.02 |      - |    8040 B |        1.00 |
| TryFindSelfIntersectionWithBRect     | 500        | 153,395.91 ns | 46,676.555 ns | 2,558.499 ns |  0.72 |    0.01 |      - |   40033 B |        4.98 |
| TryFindSelfIntersectionWithPtObjects | 500        | 211,986.58 ns |  4,591.390 ns |   251.670 ns |  1.00 |    0.01 |      - |    8041 B |        1.00 |

*)
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

        circle <- Polyline2D.createFromPts points

    [<Benchmark(Baseline = true)>]
    member _.TryFindSelfIntersection() =
        Polyline2D.tryFindSelfIntersection circle

    [<Benchmark>]
    member _.TryFindSelfIntersectionWithBRect() =
        Polyline2D.tryFindSelfIntersectionWithBRect circle

    [<Benchmark>]
    member _.TryFindSelfIntersectionWithPtObjects() =
        Polyline2D.tryFindSelfIntersectionWithPtObjects circle


// Lighter than Job.ShortRun: 1 warmup + 2 measured iterations (keeps a StdDev),
// and a 100 ms per-iteration target instead of the default.
let config =
    DefaultConfig.Instance
        .AddJob(
            Job.ShortRun
                .WithToolchain(InProcessEmitToolchain.Instance)
                .WithWarmupCount(1)
                .WithIterationCount(2)
                .WithIterationTime(Perfolizer.Horology.TimeInterval.FromMilliseconds 120.0))


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
