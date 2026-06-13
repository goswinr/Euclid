# Benchmarks


The benchmarks need to be compiled with the `BENCHMARKS` conditional compilation symbol defined,
so that previously versions of some algorithms are included in the build.

## Summary
Structs are great in .NET, but in Fable they are JS classes.
So avoiding them and using just x, y (and z) floats is much more efficient.

look at `TryFindSelfIntersectionWithPtObjects` in JS. its 600% slower when allocating 4 Pt structs/objects in the loop.

## Topology2D.sortToLoopWithReversing

### .NET

```bash
dotnet build Euclid.fsproj --configuration Release /p:DefineConstants=BENCHMARKS
dotnet fsi Test/Benchmarks/BenchmarkTopology2DSortToLoopWithReversing.fsx
```

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
  PointCount  : Value of the 'PointCount' parameter
  Mean        : Arithmetic mean of all measurements
  Error       : Half of 99.9% confidence interval
  StdDev      : Standard deviation of all measurements
  Ratio       : Mean of the ratio distribution ([Current]/[Baseline])
  RatioSD     : Standard deviation of the ratio distribution ([Current]/[Baseline])
  Gen0        : GC Generation 0 collects per 1000 operations
  Allocated   : Allocated memory per single operation (managed only, inclusive, 1KB = 1024B)
  Alloc Ratio : Allocated memory ratio distribution ([Current]/[Baseline])
  1 ns        : 1 Nanosecond (0.000000001 sec)

### Fable

```bash
cd Test
dotnet tool restore
npm install
dotnet fable ../Euclid.fsproj --outDir dist/testsRelease --noCache --configuration Release --define BENCHMARKS
cd ..
node Test/Benchmarks/BenchmarkTopology2DSortToLoopWithReversing.js
```

| Method                                                  |    Count |           Mean |         StdDev |    Ratio
|---------------------------------------------------------|----------|----------------|----------------|---------
| SortToLoopWithReversing                                 |        5 |       71.17 ns |        0.93 ns |     1.00
| SortToLoopWithReversingCached                           |        5 |      447.97 ns |       14.72 ns |     6.29
| SortToLoopWithReversingWithPtObjects                    |        5 |      216.23 ns |        8.38 ns |     3.04
| SortToLoopWithReversing_LineConstructed                 |        5 |      197.77 ns |        3.33 ns |     1.00
| SortToLoopWithReversingCached_LineConstructed           |        5 |      447.02 ns |        4.61 ns |     2.26
| SortToLoopWithReversingWithPtObjects_LineConstructed    |        5 |      411.49 ns |        7.11 ns |     2.08
|                                                         |          |                |                |
| SortToLoopWithReversing                                 |       20 |        1.54 us |       31.69 ns |     1.00
| SortToLoopWithReversingCached                           |       20 |        4.08 us |       68.54 ns |     2.65
| SortToLoopWithReversingWithPtObjects                    |       20 |        2.92 us |       40.50 ns |     1.89
| SortToLoopWithReversing_LineConstructed                 |       20 |        2.39 us |       19.50 ns |     1.00
| SortToLoopWithReversingCached_LineConstructed           |       20 |        3.95 us |       76.72 ns |     1.65
| SortToLoopWithReversingWithPtObjects_LineConstructed    |       20 |        5.06 us |      105.84 ns |     2.11
|                                                         |          |                |                |
| SortToLoopWithReversing                                 |       30 |        3.04 us |       25.34 ns |     1.00
| SortToLoopWithReversingCached                           |       30 |        7.98 us |      126.81 ns |     2.63
| SortToLoopWithReversingWithPtObjects                    |       30 |        5.82 us |      103.58 ns |     1.92
| SortToLoopWithReversing_LineConstructed                 |       30 |        4.96 us |       43.24 ns |     1.00
| SortToLoopWithReversingCached_LineConstructed           |       30 |        7.99 us |       63.03 ns |     1.61
| SortToLoopWithReversingWithPtObjects_LineConstructed    |       30 |       10.65 us |      208.80 ns |     2.15
|                                                         |          |                |                |
| SortToLoopWithReversing                                 |       50 |        8.13 us |      121.59 ns |     1.00
| SortToLoopWithReversingCached                           |       50 |       18.33 us |      622.58 ns |     2.25
| SortToLoopWithReversingWithPtObjects                    |       50 |       16.03 us |      518.57 ns |     1.97
| SortToLoopWithReversing_LineConstructed                 |       50 |       15.14 us |      774.99 ns |     1.00
| SortToLoopWithReversingCached_LineConstructed           |       50 |       18.92 us |      538.86 ns |     1.25
| SortToLoopWithReversingWithPtObjects_LineConstructed    |       50 |       29.89 us |        1.83 us |     1.97
|                                                         |          |                |                |
| SortToLoopWithReversing                                 |      200 |      164.87 us |        2.28 us |     1.00
| SortToLoopWithReversingCached                           |      200 |      221.53 us |        4.99 us |     1.34
| SortToLoopWithReversingWithPtObjects                    |      200 |      209.07 us |        6.54 us |     1.27
| SortToLoopWithReversing_LineConstructed                 |      200 |      222.87 us |        6.45 us |     1.00
| SortToLoopWithReversingCached_LineConstructed           |      200 |      215.56 us |        3.15 us |     0.97
| SortToLoopWithReversingWithPtObjects_LineConstructed    |      200 |      390.61 us |       10.38 us |     1.75
|                                                         |          |                |                |
| SortToLoopWithReversing                                 |      500 |        1.01 ms |        7.58 us |     1.00
| SortToLoopWithReversingCached                           |      500 |        1.23 ms |       16.68 us |     1.23
| SortToLoopWithReversingWithPtObjects                    |      500 |        1.27 ms |       70.71 us |     1.27
| SortToLoopWithReversing_LineConstructed                 |      500 |        1.41 ms |       76.79 us |     1.00
| SortToLoopWithReversingCached_LineConstructed           |      500 |        1.23 ms |       23.89 us |     0.88
| SortToLoopWithReversingWithPtObjects_LineConstructed    |      500 |        2.70 ms |      128.89 us |     1.92


### Points2D.tryFindSelfIntersection

### .NET

```bash
dotnet build Euclid.fsproj --configuration Release /p:DefineConstants=BENCHMARKS
dotnet fsi Test/Benchmarks/BenchmarkPolyline2DSelfIntersection.fsx
```

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

## Fable

```bash
cd Test
dotnet tool restore
npm install
dotnet fable ../Euclid.fsproj --outDir dist/testsRelease --noCache --configuration Release --define BENCHMARKS
cd ..
node Test/Benchmarks/BenchmarkPolyline2DSelfIntersection.js
```

| Method                                  | PointCount |           Mean |         StdDev |    Ratio
| ----------------------------------------|------------|----------------|----------------|---------
| TryFindSelfIntersection                 |          5 |       35.45 ns |        0.39 ns |     1.00
| TryFindSelfIntersectionWithBRect        |          5 |      118.18 ns |        1.54 ns |     3.33
| TryFindSelfIntersectionWithPtObjects    |          5 |       90.55 ns |        0.37 ns |     2.55
|                                         |            |                |                |
| TryFindSelfIntersection                 |         20 |      460.46 ns |        4.95 ns |     1.00
| TryFindSelfIntersectionWithBRect        |         20 |      848.51 ns |        8.12 ns |     1.84
| TryFindSelfIntersectionWithPtObjects    |         20 |        3.10 us |       57.50 ns |     6.74
|                                         |            |                |                |
| TryFindSelfIntersection                 |         30 |        1.03 us |       13.11 ns |     1.00
| TryFindSelfIntersectionWithBRect        |         30 |        1.67 us |       24.86 ns |     1.62
| TryFindSelfIntersectionWithPtObjects    |         30 |        7.22 us |      107.26 ns |     6.99
|                                         |            |                |                |
| TryFindSelfIntersection                 |         50 |        2.88 us |       44.20 ns |     1.00
| TryFindSelfIntersectionWithBRect        |         50 |        4.16 us |      235.61 ns |     1.44
| TryFindSelfIntersectionWithPtObjects    |         50 |       21.15 us |      801.15 ns |     7.34
|                                         |            |                |                |
| TryFindSelfIntersection                 |        200 |       50.11 us |        2.08 us |     1.00
| TryFindSelfIntersectionWithBRect        |        200 |       51.82 us |        2.02 us |     1.03
| TryFindSelfIntersectionWithPtObjects    |        200 |      333.49 us |       12.53 us |     6.66
|                                         |            |                |                |
| TryFindSelfIntersection                 |        500 |      296.74 us |       17.07 us |     1.00
| TryFindSelfIntersectionWithBRect        |        500 |      315.52 us |       12.80 us |     1.06
| TryFindSelfIntersectionWithPtObjects    |        500 |        2.22 ms |      122.00 us |     7.50



