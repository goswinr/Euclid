module Euclid.Benchmarks.Program

open System
open Euclid

// The variants, see README.md:
// A : the shipped Offset3D: OffsetDirection is a class record of 3 UnitVec, read with ResizeArr.getIdx
// A0: RecordChecked, the shipped code as it was before it used ResizeArr.getIdx
// B : StructRecord, the shipped code with a struct record
// C : Flat, one flat ResizeArray<float> with 9 floats per point, read with ResizeArr.getIdx
// C0: FlatChecked, like C, read with the ResizeArray indexer

/// The time in milliseconds.
let now () : float =
#if FABLE_COMPILER
    Fable.Core.JsInterop.emitJsExpr () "performance.now()"
#else
    float (Diagnostics.Stopwatch.GetTimestamp()) * 1000.0 / float Diagnostics.Stopwatch.Frequency
#endif

/// A full garbage collection. In JS only if Node runs with --expose-gc.
let collect () =
#if FABLE_COMPILER
    Fable.Core.JsInterop.emitJsStatement () "if (globalThis.gc) { globalThis.gc(); }"
#else
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()
#endif

/// The bytes allocated so far on this thread. Always 0 in JS.
let allocatedBytes () : int64 =
#if FABLE_COMPILER
    0L
#else
    GC.GetAllocatedBytesForCurrentThread()
#endif

/// The bytes in use on the heap, after a full garbage collection in .NET.
let heapUsed () : float =
#if FABLE_COMPILER
    Fable.Core.JsInterop.emitJsExpr () "process.memoryUsage().heapUsed"
#else
    float (GC.GetTotalMemory(true))
#endif

let runtime =
#if FABLE_COMPILER
    "JS (Fable, Release) on Node " + Fable.Core.JsInterop.emitJsExpr () "process.version"
#else
    ".NET " + Environment.Version.ToString()
#endif

/// Results are added here, so that no benchmarked call can be optimized away.
let mutable sink = 0.0

/// Reads ResizeArrays of six element kinds through the indexer, so that the indexer function of fable-library
/// is megamorphic before any variant is optimized, as in an app that indexes ResizeArrays of many types.
/// Without this, the timings in JS depend on the order in which V8 optimizes the functions.
let polluteIndexerFeedback () =
#if FABLE_COMPILER
    let holeyInts : ResizeArray<int> = Fable.Core.JsInterop.emitJsExpr () "(() => { const a = new Array(8); for (let i = 0; i < 8; i++) a[i] = i; return a; })()"
    let holeyFloats : ResizeArray<float> = Fable.Core.JsInterop.emitJsExpr () "(() => { const a = new Array(8); for (let i = 0; i < 8; i++) a[i] = i + 0.5; return a; })()"
    let holeyStrings : ResizeArray<string> = Fable.Core.JsInterop.emitJsExpr () "(() => { const a = new Array(8); for (let i = 0; i < 8; i++) a[i] = 'h' + i; return a; })()"
#else
    let holeyInts = ResizeArray [ for i in 0 .. 7 -> i ]
    let holeyFloats = ResizeArray [ for i in 0 .. 7 -> float i + 0.5 ]
    let holeyStrings = ResizeArray [ for i in 0 .. 7 -> "h" + string i ]
#endif
    let ints = ResizeArray<int>()
    let floats = ResizeArray<float>()
    let strings = ResizeArray<string>()
    for i = 0 to 7 do
        ints.Add i
        floats.Add (float i + 0.5)
        strings.Add ("s" + string i)
    let mutable acc = 0.0
    for _ in 1 .. 20000 do
        for i = 0 to 7 do
            acc <- acc + float ints.[i] + floats.[i] + float strings.[i].Length + float holeyInts.[i] + holeyFloats.[i] + float holeyStrings.[i].Length
    sink <- sink + acc

/// An open 3D helix, rotated about a skew axis and moved away from the origin.
/// Every third point is a corner of about 17 degrees, the two points between are collinear.
/// ptCount must be 3k+1, so the last point is a corner.
let makePolyline (ptCount:int) : ResizeArray<float> =
    let ax, ay, az = 1.0 / sqrt 14.0, 2.0 / sqrt 14.0, 3.0 / sqrt 14.0
    let ca, sa = cos 0.7, sin 0.7
    let xyzs = ResizeArray<float>(ptCount * 3)
    let cornerX c = (10.0 + 2.0 * sin (float c * 0.05)) * cos (float c * 0.3)
    let cornerY c = (10.0 + 2.0 * sin (float c * 0.05)) * sin (float c * 0.3)
    let cornerZ c = float c * 0.4
    for i = 0 to ptCount - 1 do
        let c = i / 3
        let f = float (i % 3) / 3.0
        let x = cornerX c + (cornerX (c + 1) - cornerX c) * f
        let y = cornerY c + (cornerY (c + 1) - cornerY c) * f
        let z = cornerZ c + (cornerZ (c + 1) - cornerZ c) * f
        // Rodrigues rotation about (ax, ay, az), then a translation
        let d = ax * x + ay * y + az * z
        let cx = ay * z - az * y
        let cy = az * x - ax * z
        let cz = ax * y - ay * x
        xyzs.Add (x * ca + cx * sa + ax * d * (1.0 - ca) + 1000.0)
        xyzs.Add (y * ca + cy * sa + ay * d * (1.0 - ca) - 2000.0)
        xyzs.Add (z * ca + cz * sa + az * d * (1.0 - ca) + 3000.0)
    xyzs

/// The helix axis after the same rotation as in makePolyline.
let refNormal =
    let ax, ay, az = 1.0 / sqrt 14.0, 2.0 / sqrt 14.0, 3.0 / sqrt 14.0
    let ca, sa = cos 0.7, sin 0.7
    let d = az
    UnitVec.create (ay * sa + ax * d * (1.0 - ca)) (-ax * sa + ay * d * (1.0 - ca)) (ca + az * d * (1.0 - ca))

let sameFloats (a:ResizeArray<float>) (b:ResizeArray<float>) =
    a.Count = b.Count && Seq.forall2 (fun (x:float) (y:float) -> x = y) a b

/// The 9 floats per point of the record directions, to compare them with the flat ones.
let flatten (dirs:ResizeArray<Offset3D.OffsetDirection>) =
    let r = ResizeArray<float>()
    for d in dirs do
        for v in [ d.PreviousInPlane; d.NextInPlane; d.Perpendicular ] do
            r.Add v.X
            r.Add v.Y
            r.Add v.Z
    r

let median (xs: float[]) =
    let s = Array.sort xs
    s.[s.Length / 2]

/// Calls f iters times and returns the milliseconds per call.
let timeRound (f: unit -> unit) (iters:int) : float =
    let t0 = now ()
    for _ in 1 .. iters do f ()
    (now () - t0) / float iters

/// Benchmarks the variants of one case with interleaved rounds.
/// Returns the median milliseconds per call and the bytes allocated by one call of each variant.
let bench (variants: (unit -> unit)[]) : float[] * int64[] =
    let targetMs = 20.0
    let rounds = 15
    // calibrate and warm up each variant
    let iters =
        variants |> Array.map (fun f ->
            let mutable n = 1
            let mutable t = timeRound f n * float n
            while t < 5.0 do
                n <- n * 2
                t <- timeRound f n * float n
            let it = max 1 (int (targetMs / (t / float n)))
            // warm up for about 10 rounds, so that the JIT or TurboFan has optimized it
            let t0 = now ()
            while now () - t0 < 10.0 * targetMs do timeRound f it |> ignore
            it)
    let bytes =
        variants |> Array.map (fun f ->
            let b0 = allocatedBytes ()
            f ()
            allocatedBytes () - b0)
    collect ()
    let samples = Array.init variants.Length (fun _ -> ResizeArray<float>())
    for r = 0 to rounds - 1 do
        for k = 0 to variants.Length - 1 do
            let v = (k + r) % variants.Length // rotate the order each round
            samples.[v].Add (timeRound variants.[v] iters.[v])
    samples |> Array.map (fun s -> median (s.ToArray())), bytes

let c25 = Cosine.``2.5``
let c175 = Cosine.``175.0``
let vdp = Offset3D.VarDistParallel.Proportional

/// Times all cases and variants for one point count. Prints them if report is TRUE.
let runSize (report:bool) (n:int) =
    let xyzs = makePolyline n
    let uvs = Offset3D.getSegmentUnitVectors xyzs
    let dirsA = Offset3D.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)
    let dirsA0 = RecordChecked.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)
    let dirsB = StructRecord.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)
    let dirsC = Flat.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)
    let dirsC0 = FlatChecked.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)
    // the 3 segments of one helix edge share a distance, so the collinear points are offset exactly, the corners use setOffCornerVar
    let dIn = ResizeArray<float>(Array.init (n - 1) (fun s -> 1.0 + 0.25 * float ((s / 3) % 3)))
    let dPerp = ResizeArray<float>(Array.init n (fun i -> 0.5 + 0.1 * float (i % 4)))

    // all variants must give bit-identical results
    let offA = Offset3D.offsetWithDirections(xyzs, dirsA, 1.0, 0.5)
    let varA = Offset3D.offsetVariableWithDirections(xyzs, uvs, dirsA, dIn, dPerp, vdp, c25)
    let ok =
        sameFloats (flatten dirsA) (flatten dirsA0) && sameFloats (flatten dirsA) dirsC && sameFloats dirsC dirsC0
        && sameFloats offA (RecordChecked.offsetWithDirections(xyzs, dirsA0, 1.0, 0.5))
        && sameFloats offA (StructRecord.offsetWithDirections(xyzs, dirsB, 1.0, 0.5))
        && sameFloats offA (Flat.offsetWithDirections(xyzs, dirsC, 1.0, 0.5))
        && sameFloats offA (FlatChecked.offsetWithDirections(xyzs, dirsC0, 1.0, 0.5))
        && sameFloats varA (RecordChecked.offsetVariableWithDirections(xyzs, uvs, dirsA0, dIn, dPerp, vdp, c25))
        && sameFloats varA (StructRecord.offsetVariableWithDirections(xyzs, uvs, dirsB, dIn, dPerp, vdp, c25))
        && sameFloats varA (Flat.offsetVariableWithDirections(xyzs, uvs, dirsC, dIn, dPerp, vdp, c25))
        && sameFloats varA (FlatChecked.offsetVariableWithDirections(xyzs, uvs, dirsC0, dIn, dPerp, vdp, c25))
    if not ok then
        failwith $"The variants give different results for {n} points."

    let cases : (string * (unit -> unit)[])[] = [|
        "getOffsetDirections", [|
            (fun () -> sink <- sink + float (Offset3D.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)).Count)
            (fun () -> sink <- sink + float (RecordChecked.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)).Count)
            (fun () -> sink <- sink + float (StructRecord.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)).Count)
            (fun () -> sink <- sink + float (Flat.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)).Count)
            (fun () -> sink <- sink + float (FlatChecked.getOffsetDirections(xyzs, uvs, refNormal, c25, c175)).Count) |]
        "offsetWithDirections", [|
            (fun () -> sink <- sink + (Offset3D.offsetWithDirections(xyzs, dirsA, 1.0, 0.5)).[0])
            (fun () -> sink <- sink + (RecordChecked.offsetWithDirections(xyzs, dirsA0, 1.0, 0.5)).[0])
            (fun () -> sink <- sink + (StructRecord.offsetWithDirections(xyzs, dirsB, 1.0, 0.5)).[0])
            (fun () -> sink <- sink + (Flat.offsetWithDirections(xyzs, dirsC, 1.0, 0.5)).[0])
            (fun () -> sink <- sink + (FlatChecked.offsetWithDirections(xyzs, dirsC0, 1.0, 0.5)).[0]) |]
        "offsetVariableWithDirections", [|
            (fun () -> sink <- sink + (Offset3D.offsetVariableWithDirections(xyzs, uvs, dirsA, dIn, dPerp, vdp, c25)).[0])
            (fun () -> sink <- sink + (RecordChecked.offsetVariableWithDirections(xyzs, uvs, dirsA0, dIn, dPerp, vdp, c25)).[0])
            (fun () -> sink <- sink + (StructRecord.offsetVariableWithDirections(xyzs, uvs, dirsB, dIn, dPerp, vdp, c25)).[0])
            (fun () -> sink <- sink + (Flat.offsetVariableWithDirections(xyzs, uvs, dirsC, dIn, dPerp, vdp, c25)).[0])
            (fun () -> sink <- sink + (FlatChecked.offsetVariableWithDirections(xyzs, uvs, dirsC0, dIn, dPerp, vdp, c25)).[0]) |]
        "full offset", [| // the segment vectors, the directions and the offset, like Offset3D.offsetByNormalCustom
            (fun () ->
                let u = Offset3D.getSegmentUnitVectors xyzs
                sink <- sink + (Offset3D.offsetWithDirections(xyzs, Offset3D.getOffsetDirections(xyzs, u, refNormal, c25, c175), 1.0, 0.5)).[0])
            (fun () ->
                let u = Offset3D.getSegmentUnitVectors xyzs
                sink <- sink + (RecordChecked.offsetWithDirections(xyzs, RecordChecked.getOffsetDirections(xyzs, u, refNormal, c25, c175), 1.0, 0.5)).[0])
            (fun () ->
                let u = Offset3D.getSegmentUnitVectors xyzs
                sink <- sink + (StructRecord.offsetWithDirections(xyzs, StructRecord.getOffsetDirections(xyzs, u, refNormal, c25, c175), 1.0, 0.5)).[0])
            (fun () ->
                let u = Offset3D.getSegmentUnitVectors xyzs
                sink <- sink + (Flat.offsetWithDirections(xyzs, Flat.getOffsetDirections(xyzs, u, refNormal, c25, c175), 1.0, 0.5)).[0])
            (fun () ->
                let u = Offset3D.getSegmentUnitVectors xyzs
                sink <- sink + (FlatChecked.offsetWithDirections(xyzs, FlatChecked.getOffsetDirections(xyzs, u, refNormal, c25, c175), 1.0, 0.5)).[0]) |]
    |]
    for name, variants in cases do
        let ms, bytes = bench variants
        if report then
            let ns = ms |> Array.map (fun m -> m * 1e6 / float n)
            let r k = ns.[k] / ns.[0]
            let line = sprintf "%-28s %6d %9.2f %6.2f %6.2f %6.2f %6.2f" name n ns.[0] (r 1) (r 2) (r 3) (r 4)
#if FABLE_COMPILER
            printfn "%s" line
            ignore bytes
#else
            printfn "%s %12d %12d" line bytes.[0] bytes.[3]
#endif
    if report then printfn ""

/// Prints the timings of all cases and variants.
let timings (pollute:bool) =
    if pollute then polluteIndexerFeedback ()
    printfn "Offset3D direction storage benchmark on %s%s" runtime (if pollute then ", indexer made megamorphic first" else "")
    printfn "A: record (shipped)   A0: record, indexer   B: struct record   C: flat   C0: flat, indexer"
    printfn "A is the median of 15 interleaved rounds in ns per point, the others are relative to A, below 1.00 is faster."
    printfn ""
    let header = sprintf "%-28s %6s %9s %6s %6s %6s %6s" "case" "points" "A ns/pt" "A0/A" "B/A" "C/A" "C0/A"
#if FABLE_COMPILER
    printfn "%s" header
#else
    printfn "%s %12s %12s" header "A bytes" "C bytes"
#endif
    // run all code paths once unreported, so that no reported case is measured while the JIT or TurboFan still compiles
    runSize false 256
    runSize false 4096
    for n in [| 16; 256; 4096; 65536 |] do // all 3k+1
        runSize true n
    printfn "(checksum %g)" sink

/// The heap bytes retained by the result of make, after a full garbage collection.
let retainedBytes (make: unit -> obj) : float =
    collect ()
    let h0 = heapUsed ()
    let keep = make ()
    collect ()
    let h1 = heapUsed ()
    if isNull keep then sink <- sink + 1.0 // keeps it alive until after h1
    h1 - h0

/// Prints the heap bytes per point that the offset directions of each layout retain.
let memory () =
    printfn "Retained heap of the offset directions on %s, in bytes per point, median of 5" runtime
    let n = 65536
    let xyzs = makePolyline n
    let uvs = Offset3D.getSegmentUnitVectors xyzs
    let perPoint (make: unit -> obj) = median (Array.init 5 (fun _ -> retainedBytes make)) / float n
    printfn "A  record:        %6.1f" (perPoint (fun () -> box (Offset3D.getOffsetDirections(xyzs, uvs, refNormal, c25, c175))))
    printfn "B  struct record: %6.1f" (perPoint (fun () -> box (StructRecord.getOffsetDirections(xyzs, uvs, refNormal, c25, c175))))
    printfn "C  flat:          %6.1f" (perPoint (fun () -> box (Flat.getOffsetDirections(xyzs, uvs, refNormal, c25, c175))))

[<EntryPoint>]
let main argv =
    if argv |> Array.contains "--memory" then
        memory ()
    else
        timings (argv |> Array.contains "--pollute")
    0
