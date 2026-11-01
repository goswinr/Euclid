# Offset3D direction storage benchmark

Would `Offset3D` be faster if `getOffsetDirections` returned one flat, interleaved `ResizeArray<float>` with 9 floats per point, instead of a `ResizeArray<OffsetDirection>` of records with three `UnitVec` each?

This benchmark answers that for .NET and for JS through Fable.
It is a manual benchmark, not part of the test run.

## Findings

- **In .NET the layout makes little difference.**
  Up to 4,096 points, the struct record and the flat buffer take 0.93 to 1.13 times as long as the shipped record, which is about the noise of this benchmark.
  At 65,536 points a full offset is about 30% faster without one object per point, because the garbage collector has to promote the records that survive a gen0 collection.
  A `[<Struct>]` record gets the same gain as the flat buffer.
- **In JS, how a `ResizeArray` is read matters more than the layout.**
  Fable compiles the indexer `xs.[i]` to a call of `item(i, xs)` from fable-library, which checks the bounds.
  Every `ResizeArray` read in an app goes through this one function, so V8 sees arrays of all kinds there and uses a slow generic load.
  With the indexer, the flat buffer needs 9 such calls per point instead of 1.
  Up to 4,096 points it is then 2.5 to 2.8 times as slow as the record read the same way in `offsetWithDirections`, and 1.4 to 1.5 times as slow in `offsetVariableWithDirections`.
  The indexer of an `IList`, like the distances of the variable offsets, compiles to the same `item` call.
- **Reading with `getIdx` made `Offset3D` 1.8 to 4.2 times as fast in JS, and a full offset about twice as fast.**
  `ResizeArr.getIdx` and `IList.getIdx` compile to a plain `xs[i]` in Fable Release, so each read gets its own fast load.
  `Offset2D` and `Offset3D` now read their coordinates, segment vectors, directions and distances this way.
  In .NET and in Fable Debug they are the plain indexer, so nothing changes there.
- **Read with `getIdx`, the flat buffer only pays off for long polylines in JS.**
  Up to 4,096 points it takes 0.78 to 1.13 times as long as the record: faster in `offsetWithDirections`, a bit slower in `getOffsetDirections`.
  At 65,536 points it takes 0.46 of the time of the record in `offsetWithDirections`, 0.38 in `offsetVariableWithDirections` and 0.88 in a full offset.
- **Memory:** in JS the directions of one point take 346 bytes as a record and 107 bytes as a flat buffer.
  In .NET they take 96 bytes as a class record and 72 bytes as a struct record or a flat buffer.

So `Offset3D` reads with `getIdx`, and `OffsetDirection` is still a record.
A flat buffer or a struct record would change the public type of `Offset3D.getOffsetDirections`, for a gain that only shows with tens of thousands of points.

## Variants

| | File | Offset directions | Reads of coordinates, directions and distances |
|---|---|---|---|
| A | `Src/Offset3D.fs`, shipped | class record of 3 `UnitVec` | `ResizeArr.getIdx` and `IList.getIdx` |
| A0 | `RecordChecked.fs` | class record of 3 `UnitVec` | indexers, like `Offset3D` before this benchmark |
| B | `StructRecord.fs` | `[<Struct>]` record of 3 `UnitVec` | `ResizeArr.getIdx` and `IList.getIdx` |
| C | `Flat.fs` | `ResizeArray<float>`, 9 floats per point | `ResizeArr.getIdx` and `IList.getIdx` |
| C0 | `FlatChecked.fs` | `ResizeArray<float>`, 9 floats per point | indexers |

The variants are copies of the shipped algorithm. They differ only in how they store the directions and how they read the buffers.
`Helpers.fs` holds copies of the private helpers of `Offset3D`, once with the indexer in `Checked` and once with `getIdx` in `Unchecked`.
`Flat.fs` and `FlatChecked.fs` differ only in their header.
Before timing, the benchmark checks that all variants return bit-identical directions and offsets.
Run the JS timings from a Release build, as the npm scripts do: in Fable Debug, `getIdx` is the checked indexer.

Three variants also serve as controls:
in .NET, `getIdx` is the plain indexer, so A0 runs the same code as A, and C0 the same code as C.
In JS, a struct record compiles to the same class as a record, so B runs the same code as A.
Their ratios show the noise of the measurement.

## How to run

Once, from the repository root: `dotnet tool restore`, for Fable.
Then, in this folder:

| Command | Measures |
|---|---|
| `npm run dotnet` | the timings in .NET |
| `npm run dotnet-memory` | the retained memory in .NET |
| `npm run js` | the timings in JS, after making the indexer megamorphic |
| `npm run js-unpolluted` | the timings in JS, without that |
| `npm run js-memory` | the retained memory in JS |

npm only runs the scripts of `package.json`; there is nothing to install.
The JS output goes to `dist/`, which git ignores.
A timing run takes one to two minutes.

## Method

- **Polyline:** an open helix, rotated about a skew axis and moved away from the origin.
  Each edge of the helix is split into 3 segments, so a third of the points are corners of about 17 degrees and two thirds are collinear.
  This runs both ways of `getOffsetDirections` to find a direction.
  The in-plane distances change from edge to edge, so the corners use the variable distance formula, and each point has its own perpendicular distance.
- **Sizes:** 16, 256, 4,096 and 65,536 points.
- **Cases:** `getOffsetDirections`, then `offsetWithDirections` and `offsetVariableWithDirections` with precomputed directions,
  and a full offset of `getSegmentUnitVectors`, `getOffsetDirections` and `offsetWithDirections`, like `Offset3D.offsetByNormalCustom`.
- **Timing:** each variant runs in rounds of about 20 ms, after 200 ms of warm-up.
  The 15 rounds of the variants of a case are interleaved, in a different order in each round, and the median is reported in nanoseconds per point.
  Before the reported sizes, all cases run once at 256 and 4,096 points, so that the .NET JIT or V8's TurboFan has compiled everything.
- **Memory:** the heap that the directions of 65,536 points keep alive after a full garbage collection, the median of 5 measurements.
  In .NET the timings also show the bytes that one call allocates.
- **`--pollute` in JS:** before timing, the benchmark reads arrays of six element kinds through the indexer, so `item` is megamorphic, as in any app that indexes `ResizeArray`s of different types.
  Without this, the timings depend on which function V8 optimizes first.
  In the run without it, B, which is the same JS code as A, took 1.9 to 2.5 times as long as A in `offsetWithDirections` up to 4,096 points.

## Results

Measured on 2026-11-03 on an AMD Ryzen 5 9600X with Windows 11, .NET 10.0.14, Node 26.12.1 and Fable 5.18.0.
Each value is the mean of two runs.
A is in nanoseconds per point. The other columns are relative to A, so a value below 1.00 is faster than A.

### JS, with `--pollute`

| Case | Points | A ns/pt | A0/A | B/A | C/A | C0/A |
|---|---:|---:|---:|---:|---:|---:|
| getOffsetDirections | 16 | 149.6 | 1.90 | 1.02 | 1.10 | 1.99 |
| offsetWithDirections | 16 | 8.6 | 3.05 | 0.98 | 0.91 | 8.21 |
| offsetVariableWithDirections | 16 | 30.8 | 3.75 | 1.07 | 1.02 | 5.66 |
| full offset | 16 | 172.1 | 1.86 | 1.00 | 1.13 | 2.21 |
| getOffsetDirections | 256 | 120.7 | 2.17 | 0.98 | 1.07 | 2.28 |
| offsetWithDirections | 256 | 10.4 | 2.84 | 0.99 | 0.83 | 7.02 |
| offsetVariableWithDirections | 256 | 33.7 | 4.02 | 0.98 | 0.96 | 5.66 |
| full offset | 256 | 170.2 | 2.02 | 1.01 | 1.05 | 2.42 |
| getOffsetDirections | 4096 | 125.2 | 2.31 | 1.00 | 1.00 | 2.27 |
| offsetWithDirections | 4096 | 9.9 | 2.87 | 0.93 | 0.78 | 8.00 |
| offsetVariableWithDirections | 4096 | 30.0 | 4.19 | 1.04 | 1.02 | 5.98 |
| full offset | 4096 | 162.9 | 2.15 | 0.99 | 0.93 | 2.30 |
| getOffsetDirections | 65536 | 267.9 | 1.78 | 1.24 | 0.96 | 1.48 |
| offsetWithDirections | 65536 | 16.5 | 2.16 | 1.02 | 0.46 | 4.42 |
| offsetVariableWithDirections | 65536 | 68.9 | 2.41 | 1.03 | 0.38 | 3.42 |
| full offset | 65536 | 312.9 | 1.81 | 1.16 | 0.88 | 1.55 |

`offsetVariableWithDirections` gained the most because it also reads two or three distances per point.
Reading only the coordinates and directions with `getIdx` made it 2.2 to 2.4 times as fast up to 4,096 points, and reading the distances with `IList.getIdx` too brought that to 3.8 to 4.2.

### .NET

| Case | Points | A ns/pt | A0/A | B/A | C/A | C0/A |
|---|---:|---:|---:|---:|---:|---:|
| getOffsetDirections | 16 | 26.4 | 1.10 | 1.08 | 1.11 | 1.16 |
| offsetWithDirections | 16 | 4.0 | 1.00 | 0.96 | 0.97 | 0.96 |
| offsetVariableWithDirections | 16 | 11.5 | 1.03 | 1.00 | 1.12 | 1.15 |
| full offset | 16 | 34.2 | 1.10 | 1.05 | 1.09 | 1.15 |
| getOffsetDirections | 256 | 26.9 | 1.09 | 1.10 | 1.10 | 1.09 |
| offsetWithDirections | 256 | 3.5 | 0.98 | 0.94 | 0.95 | 0.96 |
| offsetVariableWithDirections | 256 | 11.7 | 1.00 | 0.97 | 1.10 | 1.12 |
| full offset | 256 | 33.5 | 1.06 | 1.04 | 1.06 | 1.08 |
| getOffsetDirections | 4096 | 33.5 | 1.29 | 0.94 | 0.93 | 0.92 |
| offsetWithDirections | 4096 | 3.9 | 1.02 | 0.97 | 0.99 | 0.99 |
| offsetVariableWithDirections | 4096 | 11.7 | 1.00 | 0.99 | 1.13 | 1.13 |
| full offset | 4096 | 41.1 | 1.02 | 0.96 | 0.95 | 0.97 |
| getOffsetDirections | 65536 | 39.4 | 1.36 | 0.90 | 0.93 | 0.91 |
| offsetWithDirections | 65536 | 4.7 | 1.01 | 1.00 | 0.99 | 0.99 |
| offsetVariableWithDirections | 65536 | 14.2 | 0.97 | 0.93 | 1.03 | 1.05 |
| full offset | 65536 | 71.2 | 1.09 | 0.69 | 0.70 | 0.68 |

A0 runs the same .NET code as A, so its column shows the noise.
From 4,096 points on, the times of A and A0 vary with the timing of the garbage collections:
a full offset of 65,536 points with A took 90 ns per point in one run and 53 in the other, while B, C and C0 took 44 to 48 in both.

### Memory

Heap bytes per point that the directions of 65,536 points keep alive:

| | Record (A) | Struct record (B) | Flat (C) |
|---|---:|---:|---:|
| .NET | 96 | 72 | 72 |
| JS | 346 | 346 | 107 |

## Why

- **Fable's indexer.** In JS, a `ResizeArray` is a plain JS array, but its indexer compiles to `item(i, xs)`, which checks the bounds and then reads `xs[i]`.
  The indexer of an `IList` compiles to the same call; in Fable an `IList` is a JS array, or a typed array like a `Float64Array` for an array of numbers.
  V8 collects the type feedback for that read once, inside `item`, for all callers.
  In an app that indexes arrays of numbers, objects and strings, that read becomes megamorphic and costs a few nanoseconds.
  In Fable Release, `ResizeArr.getIdx` and `IList.getIdx` emit `xs[i]` at the call site, which gets its own feedback and stays fast.
  Before this benchmark, every read of the coordinates and distances in `Offset3D` went through `item`.
- **Layout in JS.** The record needs one read of the array per point and then reads its fields; the flat buffer needs nine reads of the array.
  With `getIdx` both are cheap, so they are about equal while the data fits in the CPU caches.
  The record keeps four objects per point, the record and its three `UnitVec`, and the 346 bytes are consistent with a separate heap number for each of the 9 coordinates.
  The flat buffer stores 72 bytes of unboxed floats per point, plus the spare capacity of a growing JS array.
  So with tens of thousands of points the flat buffer is faster: there is less memory to read, and fewer objects for the garbage collector.
- **Layout in .NET.** `UnitVec` is a struct, so a record is one object per point: 72 bytes of fields, a 16-byte object header and an 8-byte reference in the list.
  A struct record or a flat buffer stores the 72 bytes inline.
  Reading costs about the same; up to 4,096 points the flat buffer is about 10% slower in `offsetVariableWithDirections`, because `List<T>` checks the bounds of each of its nine reads per point.
  The extra objects only cost time when many of them survive a garbage collection, as with 65,536 points.

## When to revisit

If polylines with tens of thousands of points become common in JS, a flat buffer read with `getIdx` (variant C) is the faster layout there.
For .NET alone, a `[<Struct>]` record gives the same gain as a flat buffer and keeps the named fields.
Both change the public type of `Offset3D.getOffsetDirections`, so either one is a breaking change.
