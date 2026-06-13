

// Run from the repository root:
//   dotnet fsi Test\Benchmarks\BenchmarkTopology2DSortToLoopWithReversing.fsx


(*


| Method                                                | Count | Mean          | Error | Ratio | Gen0   | Allocated | Alloc Ratio |
|------------------------------------------------------ |------ |--------------:|------:|------:|-------:|----------:|------------:|
| SortToLoopWithReversing_LineInT                       | 5     |      47.57 ns |    NA |  1.00 |      - |         - |          NA |
| SortToLoopWithReversing_Cached_LineInT                | 5     |      49.42 ns |    NA |  1.04 | 0.0036 |     184 B |          NA |
| SortToLoopWithReversing_WithPtObjects_LineInT         | 5     |      49.43 ns |    NA |  1.04 |      - |         - |          NA |
| SortToLoopWithReversing_LineConstructed               | 5     |      50.68 ns |    NA |  1.07 |      - |         - |          NA |
| SortToLoopWithReversing_Cached_LineConstructed        | 5     |      67.83 ns |    NA |  1.43 | 0.0034 |     184 B |          NA |
| SortToLoopWithReversing_WithPtObjects_LineConstructed | 5     |      56.20 ns |    NA |  1.18 |      - |         - |          NA |
|                                                       |       |               |       |       |        |           |             |
| SortToLoopWithReversing_LineInT                       | 20    |     496.31 ns |    NA |  1.00 |      - |         - |          NA |
| SortToLoopWithReversing_Cached_LineInT                | 20    |     388.67 ns |    NA |  0.78 | 0.0133 |     664 B |          NA |
| SortToLoopWithReversing_WithPtObjects_LineInT         | 20    |     514.53 ns |    NA |  1.04 |      - |         - |          NA |
| SortToLoopWithReversing_LineConstructed               | 20    |     536.86 ns |    NA |  1.08 |      - |         - |          NA |
| SortToLoopWithReversing_Cached_LineConstructed        | 20    |     471.86 ns |    NA |  0.95 | 0.0120 |     664 B |          NA |
| SortToLoopWithReversing_WithPtObjects_LineConstructed | 20    |     546.31 ns |    NA |  1.10 |      - |         - |          NA |
|                                                       |       |               |       |       |        |           |             |
| SortToLoopWithReversing_LineInT                       | 30    |   1,043.98 ns |    NA |  1.00 |      - |         - |          NA |
| SortToLoopWithReversing_Cached_LineInT                | 30    |     782.44 ns |    NA |  0.75 | 0.0186 |     984 B |          NA |
| SortToLoopWithReversing_WithPtObjects_LineInT         | 30    |   1,067.24 ns |    NA |  1.02 |      - |         - |          NA |
| SortToLoopWithReversing_LineConstructed               | 30    |   1,062.75 ns |    NA |  1.02 |      - |         - |          NA |
| SortToLoopWithReversing_Cached_LineConstructed        | 30    |     884.59 ns |    NA |  0.85 | 0.0149 |     984 B |          NA |
| SortToLoopWithReversing_WithPtObjects_LineConstructed | 30    |   1,113.40 ns |    NA |  1.07 |      - |         - |          NA |
|                                                       |       |               |       |       |        |           |             |
| SortToLoopWithReversing_LineInT                       | 50    |   2,616.46 ns |    NA |  1.00 |      - |         - |          NA |
| SortToLoopWithReversing_Cached_LineInT                | 50    |   1,778.33 ns |    NA |  0.68 | 0.0292 |    1624 B |          NA |
| SortToLoopWithReversing_WithPtObjects_LineInT         | 50    |   2,563.38 ns |    NA |  0.98 |      - |         - |          NA |
| SortToLoopWithReversing_LineConstructed               | 50    |   2,707.89 ns |    NA |  1.03 |      - |         - |          NA |
| SortToLoopWithReversing_Cached_LineConstructed        | 50    |   2,094.59 ns |    NA |  0.80 | 0.0345 |    1624 B |          NA |
| SortToLoopWithReversing_WithPtObjects_LineConstructed | 50    |   2,818.48 ns |    NA |  1.08 |      - |         - |          NA |
|                                                       |       |               |       |       |        |           |             |
| SortToLoopWithReversing_LineInT                       | 200   |  44,013.38 ns |    NA |  1.00 |      - |       1 B |        1.00 |
| SortToLoopWithReversing_Cached_LineInT                | 200   |  30,340.70 ns |    NA |  0.69 |      - |    6425 B |    6,425.00 |
| SortToLoopWithReversing_WithPtObjects_LineInT         | 200   |  49,987.21 ns |    NA |  1.14 |      - |       1 B |        1.00 |
| SortToLoopWithReversing_LineConstructed               | 200   |  44,795.77 ns |    NA |  1.02 |      - |       1 B |        1.00 |
| SortToLoopWithReversing_Cached_LineConstructed        | 200   |  31,597.35 ns |    NA |  0.72 |      - |    6425 B |    6,425.00 |
| SortToLoopWithReversing_WithPtObjects_LineConstructed | 200   |  45,662.56 ns |    NA |  1.04 |      - |       1 B |        1.00 |
|                                                       |       |               |       |       |        |           |             |
| SortToLoopWithReversing_LineInT                       | 500   | 260,624.25 ns |    NA |  1.00 |      - |       5 B |        1.00 |
| SortToLoopWithReversing_Cached_LineInT                | 500   | 157,425.46 ns |    NA |  0.60 |      - |   16028 B |    3,205.60 |
| SortToLoopWithReversing_WithPtObjects_LineInT         | 500   | 268,719.64 ns |    NA |  1.03 |      - |       6 B |        1.20 |
| SortToLoopWithReversing_LineConstructed               | 500   | 288,446.18 ns |    NA |  1.11 |      - |       6 B |        1.20 |
| SortToLoopWithReversing_Cached_LineConstructed        | 500   | 171,503.74 ns |    NA |  0.66 |      - |   16028 B |    3,205.60 |
| SortToLoopWithReversing_WithPtObjects_LineConstructed | 500   | 311,610.57 ns |    NA |  1.20 |      - |       8 B |        1.60 |

// * Legends *
  Count       : Value of the 'Count' parameter
  Mean        : Arithmetic mean of all measurements
  Error       : Half of 99.9% confidence interval
  Ratio       : Mean of the ratio distribution ([Current]/[Baseline])
  Gen0        : GC Generation 0 collects per 1000 operations
  Allocated   : Allocated memory per single operation (managed only, inclusive, 1KB = 1024B)
  Alloc Ratio : Allocated memory ratio distribution ([Current]/[Baseline])
  1 ns        : 1 Nanosecond (0.000000001 sec)

*)


#r "nuget: BenchmarkDotNet"
#I "../../bin/Release/net6.0"
#r "Euclid.dll" // needs to be compiled with BENCHMARK compilation symbol
open Euclid


open System
open System.Collections.Generic
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Toolchains.InProcess.Emit
open Perfolizer.Horology

/// Mutable reference-typed record used for the Segment benchmark scenario.
/// reverseInPlace flips Start/End in place; getLine constructs a Line2D from
/// the current property values.
type Segment ={
    mutable StartX : float
    mutable StartY : float
    mutable EndX   : float
    mutable EndY   : float
    }

[<MemoryDiagnoser>]
type Topology2DSortToLoopWithReversingBenchmarks() =

    // --- Line2D scenario: identity getLine, slot-replacing reverseInPlace.
    // Frozen input: built once in [<GlobalSetup>], never mutated by the sorts.
    let mutable original : Line2D[] = [||]
    // Pre-allocated working buffer refilled from `original` before each call.
    let mutable buf : ResizeArray<Line2D> = ResizeArray()

    // --- Segment scenario: Line2D-constructing getLine, in-place property mutation.
    // Frozen Segment instances; their properties are restored before each call
    // (they get mutated in place by reverseInPlace).
    let mutable originalSeg : Segment[] = [||]
    // Float snapshot of the original Start/End coords, laid out as
    // [StartX; StartY; EndX; EndY] per element. Used to reset Segment state.
    let mutable segOrigCoords : float[] = [||]
    let mutable segBuf : ResizeArray<Segment> = ResizeArray()

    // Closures hoisted as fields so each benchmark invocation doesn't allocate
    // a fresh delegate / closure object. reverseInPlace closes over `buf`
    // (a reference type), and `buf` itself is never reassigned after Setup,
    // so this captures the right instance for every call.
    let mutable getLine : Line2D -> Line2D = id
    let mutable reverseInPlace : int -> Line2D -> unit = fun _ _ -> ()
    let mutable getLineSeg : Segment -> Line2D = fun _ -> Unchecked.defaultof<Line2D>
    let mutable reverseInPlaceSeg : int -> Segment -> unit = fun _ _ -> ()

    [<Params(5, 20, 30, 50, 200, 500)>]
    member val Count = 0 with get, set

    [<GlobalSetup>]
    member this.Setup() =
        let rng = Random(42 + this.Count)
        let radius = 100.0
        let n = this.Count
        let pts =
            [| for i in 0 .. n - 1 ->
                let a = 2.0 * Math.PI * float i / float n
                Pt(radius * cos a, radius * sin a) |]
        let lines = Array.zeroCreate<Line2D> n
        for i in 0 .. n - 1 do
            let p = pts.[i]
            let q = pts.[(i + 1) % n]
            if rng.NextDouble() < 0.5 then
                lines.[i] <- Line2D(q.X, q.Y, p.X, p.Y)
            else
                lines.[i] <- Line2D(p.X, p.Y, q.X, q.Y)
        // Fisher–Yates shuffle so the sort actually has work to do.
        for i in n - 1 .. -1 .. 1 do
            let j = rng.Next(i + 1)
            let t = lines.[i]
            lines.[i] <- lines.[j]
            lines.[j] <- t
        original <- lines
        buf <- ResizeArray<Line2D>(n)
        for ln in original do buf.Add ln
        getLine <- fun (ln: Line2D) -> ln
        reverseInPlace <- fun i (ln: Line2D) -> buf.[i] <- Line2D(ln.ToX, ln.ToY, ln.FromX, ln.FromY)

        // Build the parallel Segment scenario from the same Line2D ordering and
        // orientations, so both scenarios sort identical input geometry.
        let segs = Array.init n (fun _ -> { StartX = 0.0; StartY = 0.0; EndX = 0.0; EndY = 0.0 })
        let snap = Array.zeroCreate<float> (n * 4)
        for i in 0 .. n - 1 do
            let ln = lines.[i]
            segs.[i].StartX <- ln.FromX
            segs.[i].StartY <- ln.FromY
            segs.[i].EndX   <- ln.ToX
            segs.[i].EndY   <- ln.ToY
            let o = i * 4
            snap.[o    ] <- ln.FromX
            snap.[o + 1] <- ln.FromY
            snap.[o + 2] <- ln.ToX
            snap.[o + 3] <- ln.ToY
        originalSeg <- segs
        segOrigCoords <- snap
        segBuf <- ResizeArray<Segment>(n)
        for s in segs do segBuf.Add s
        getLineSeg <- fun (s: Segment) -> Line2D(s.StartX, s.StartY, s.EndX, s.EndY)
        reverseInPlaceSeg <- fun _ (s: Segment) ->
            let sx = s.StartX
            let sy = s.StartY
            s.StartX <- s.EndX
            s.StartY <- s.EndY
            s.EndX <- sx
            s.EndY <- sy

    member private this.ResetBuf() =
        // ResizeArray.Clear keeps Capacity, so Add does no reallocation.
        buf.Clear()
        for ln in original do buf.Add ln

    member private this.ResetSegBuf() =
        // Restore slot order AND each Segment's properties — reverseInPlace
        // mutates the shared Segment instances so a deep reset is required.
        segBuf.Clear()
        let n = originalSeg.Length
        for i in 0 .. n - 1 do
            let s = originalSeg.[i]
            let o = i * 4
            s.StartX <- segOrigCoords.[o    ]
            s.StartY <- segOrigCoords.[o + 1]
            s.EndX   <- segOrigCoords.[o + 2]
            s.EndY   <- segOrigCoords.[o + 3]
            segBuf.Add s

    [<Benchmark(Baseline = true)>]
    member this.SortToLoopWithReversing_LineInT() =
        this.ResetBuf()
        Topology2D.sortToLoopWithReversing(getLine, reverseInPlace, buf)

    [<Benchmark>]
    member this.SortToLoopWithReversing_Cached_LineInT() =
        this.ResetBuf()
        Topology2D.sortToLoopWithReversingCached(getLine, reverseInPlace, buf)

    [<Benchmark>]
    member this.SortToLoopWithReversing_WithPtObjects_LineInT() =
        this.ResetBuf()
        Topology2D.sortToLoopWithReversingWithPtObjects(getLine, reverseInPlace, buf)

    [<Benchmark>]
    member this.SortToLoopWithReversing_LineConstructed() =
        this.ResetSegBuf()
        Topology2D.sortToLoopWithReversing(getLineSeg, reverseInPlaceSeg, segBuf)

    [<Benchmark>]
    member this.SortToLoopWithReversing_Cached_LineConstructed() =
        this.ResetSegBuf()
        Topology2D.sortToLoopWithReversingCached(getLineSeg, reverseInPlaceSeg, segBuf)

    [<Benchmark>]
    member this.SortToLoopWithReversing_WithPtObjects_LineConstructed() =
        this.ResetSegBuf()
        Topology2D.sortToLoopWithReversingWithPtObjects(getLineSeg, reverseInPlaceSeg, segBuf)


// Lighter than Job.ShortRun: 1 warmup + 2 measured iterations (keeps a StdDev),
// and a 100 ms per-iteration target instead of the default.
let config =
    DefaultConfig.Instance
        .AddJob(
            Job.ShortRun
                .WithToolchain(InProcessEmitToolchain.Instance)
                .WithWarmupCount(1)
                .WithIterationCount(2)
                .WithIterationTime(TimeInterval.FromMilliseconds 120.0))

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
    BenchmarkRunner.Run<Topology2DSortToLoopWithReversingBenchmarks>(config) |> ignore
else
    BenchmarkSwitcher.FromTypes([| typeof<Topology2DSortToLoopWithReversingBenchmarks> |]).Run(args, config) |> ignore
