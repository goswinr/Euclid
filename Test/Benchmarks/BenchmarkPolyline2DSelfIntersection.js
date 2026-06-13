
// before running this benchmark, build Test/dist/testsRelease with BENCHMARKS enabled
// so Polyline2D.tryFindSelfIntersectionWithPtObjects is exported.



// Run from the repository root:
//   node Test/BenchmarkPolyline2DSelfIntersection.js
//
// JS counterpart of BenchmarkPolyline2DSelfIntersection.fsx.
// Compares the Fable-compiled
// Polyline2D_tryFindSelfIntersection_Z5A89AEF5 (baseline),
// Polyline2D_tryFindSelfIntersectionWithBRect_Z5A89AEF5, and
// Polyline2D_tryFindSelfIntersectionWithPtObjects_Z5A89AEF5 on closed circular
// polylines of increasing point count.
//
// Requires the Release JS build to exist at Test/dist/testsRelease/. Build it with:
//   cd Test && npm run buildJS

/*
node Test/Benchmarks/BenchmarkPolyline2DSelfIntersection.js


It mirrors the F# benchmark: builds closed circular polylines at the same point counts
(5/20/30/50/200/500), then times
Polyline2D_tryFindSelfIntersection_Z5A89AEF5,
Polyline2D_tryFindSelfIntersectionWithBRect_Z5A89AEF5, and
Polyline2D_tryFindSelfIntersectionWithPtObjects_Z5A89AEF5
from the Fable-compiled dist/testsRelease/Src/Polyline2D.js.
Each method is auto-calibrated to ~250 ms per rep, warmed up, then sampled over
7 reps for mean/stddev. The baseline is always ratio 1.00.

One caveat: this is a quick performance.now() harness, not BenchmarkDotNet - no GC
isolation, no allocation accounting. Good enough for relative comparisons, not for
publication numbers.

Method                                  | PointCount |           Mean |         StdDev |    Ratio
----------------------------------------+------------+----------------+----------------+---------
TryFindSelfIntersection                 |          5 |       35.45 ns |        0.39 ns |     1.00
TryFindSelfIntersectionWithBRect        |          5 |      118.18 ns |        1.54 ns |     3.33
TryFindSelfIntersectionWithPtObjects    |          5 |       90.55 ns |        0.37 ns |     2.55
                                        |            |                |                |
TryFindSelfIntersection                 |         20 |      460.46 ns |        4.95 ns |     1.00
TryFindSelfIntersectionWithBRect        |         20 |      848.51 ns |        8.12 ns |     1.84
TryFindSelfIntersectionWithPtObjects    |         20 |        3.10 us |       57.50 ns |     6.74
                                        |            |                |                |
TryFindSelfIntersection                 |         30 |        1.03 us |       13.11 ns |     1.00
TryFindSelfIntersectionWithBRect        |         30 |        1.67 us |       24.86 ns |     1.62
TryFindSelfIntersectionWithPtObjects    |         30 |        7.22 us |      107.26 ns |     6.99
                                        |            |                |                |
TryFindSelfIntersection                 |         50 |        2.88 us |       44.20 ns |     1.00
TryFindSelfIntersectionWithBRect        |         50 |        4.16 us |      235.61 ns |     1.44
TryFindSelfIntersectionWithPtObjects    |         50 |       21.15 us |      801.15 ns |     7.34
                                        |            |                |                |
TryFindSelfIntersection                 |        200 |       50.11 us |        2.08 us |     1.00
TryFindSelfIntersectionWithBRect        |        200 |       51.82 us |        2.02 us |     1.03
TryFindSelfIntersectionWithPtObjects    |        200 |      333.49 us |       12.53 us |     6.66
                                        |            |                |                |
TryFindSelfIntersection                 |        500 |      296.74 us |       17.07 us |     1.00
TryFindSelfIntersectionWithBRect        |        500 |      315.52 us |       12.80 us |     1.06
TryFindSelfIntersectionWithPtObjects    |        500 |        2.22 ms |      122.00 us |     7.50
                                        |            |                |                |




*/

import { performance } from "node:perf_hooks";
import * as Polyline2D from "../dist/testsRelease/Src/Polyline2D.js"; /// needs to be compiled with BENCHMARK compilation symbol

const Polyline2D_createDirectly_EF0D882 = Polyline2D.Polyline2D_createDirectly_EF0D882;

const Polyline2D_tryFindSelfIntersection_Z5A89AEF5 =
    Polyline2D.Polyline2D_tryFindSelfIntersection_Z5A89AEF5 ??
    Polyline2D.Polyline2D_tryFindSelfIntersectionBig_Z5A89AEF5;
const Polyline2D_tryFindSelfIntersectionWithBRect_Z5A89AEF5 =
    Polyline2D.Polyline2D_tryFindSelfIntersectionWithBRect_Z5A89AEF5 ??
    Polyline2D.Polyline2D_tryFindSelfIntersectionBig_Z5A89AEF5;
const Polyline2D_tryFindSelfIntersectionWithPtObjects_Z5A89AEF5 =
    Polyline2D.Polyline2D_tryFindSelfIntersectionWithPtObjects_Z5A89AEF5 ??
    Polyline2D.Polyline2D_tryFindSelfIntersectionSmall_Z5A89AEF5;

if (!Polyline2D_tryFindSelfIntersection_Z5A89AEF5) {
    throw new Error(
        "Polyline2D.tryFindSelfIntersection is not exported by ../dist/testsRelease/Src/Polyline2D.js. " +
            "Rebuild Test/dist/testsRelease with BENCHMARKS enabled."
    );
}
if (!Polyline2D_tryFindSelfIntersectionWithBRect_Z5A89AEF5) {
    throw new Error(
        "Polyline2D.tryFindSelfIntersectionWithBRect is not exported by ../dist/testsRelease/Src/Polyline2D.js. " +
            "Rebuild Test/dist/testsRelease with BENCHMARKS enabled."
    );
}
if (!Polyline2D_tryFindSelfIntersectionWithPtObjects_Z5A89AEF5) {
    throw new Error(
        "Polyline2D.tryFindSelfIntersectionWithPtObjects is not exported by ../dist/testsRelease/Src/Polyline2D.js. " +
            "Rebuild Test/dist/testsRelease with BENCHMARKS enabled."
    );
}

const POINT_COUNTS = [5, 20, 30, 50, 200, 500];

// One repetition target ~ 250 ms of work, then average over REPS reps.
const TARGET_MS_PER_REP = 250;
const REPS = 7;
const WARMUP_REPS = 2;

function makeCircleXYs(pointCount) {
    const segmentCount = pointCount - 1;
    const radius = 100.0;
    const xys = new Array(pointCount * 2);
    for (let i = 0; i <= segmentCount; i++) {
        const a = (2 * Math.PI * i) / segmentCount;
        xys[2 * i] = radius * Math.cos(a);
        xys[2 * i + 1] = radius * Math.sin(a);
    }
    return xys;
}

// Calibrate how many inner-loop iterations of fn(arg) take ~targetMs.
function calibrate(fn, arg, targetMs) {
    let iters = 1;
    while (true) {
        const t0 = performance.now();
        for (let i = 0; i < iters; i++) fn(arg);
        const dt = performance.now() - t0;
        if (dt >= targetMs / 4 || iters >= 1 << 24) {
            // Extrapolate to the full target.
            const scale = Math.max(1, Math.round((targetMs / Math.max(dt, 0.01)) * iters));
            return scale;
        }
        iters *= 2;
    }
}

// Returns ns-per-call samples (one per rep).
function measure(fn, arg) {
    const iters = calibrate(fn, arg, TARGET_MS_PER_REP);

    for (let w = 0; w < WARMUP_REPS; w++) {
        for (let i = 0; i < iters; i++) fn(arg);
    }

    const samples = new Array(REPS);
    for (let r = 0; r < REPS; r++) {
        const t0 = performance.now();
        for (let i = 0; i < iters; i++) fn(arg);
        const dt = performance.now() - t0;
        samples[r] = (dt * 1e6) / iters; // ns per call
    }
    return samples;
}

function stats(samples) {
    const n = samples.length;
    const mean = samples.reduce((a, b) => a + b, 0) / n;
    const variance = samples.reduce((a, b) => a + (b - mean) ** 2, 0) / Math.max(1, n - 1);
    return { mean, stddev: Math.sqrt(variance) };
}

function fmtNs(ns) {
    if (ns >= 1e6) return `${(ns / 1e6).toFixed(2)} ms`;
    if (ns >= 1e3) return `${(ns / 1e3).toFixed(2)} us`;
    return `${ns.toFixed(2)} ns`;
}

function pad(s, w, left = false) {
    s = String(s);
    if (s.length >= w) return s;
    const fill = " ".repeat(w - s.length);
    return left ? fill + s : s + fill;
}

function enclose(txt) {
    return `| ${txt} |`;
}

const header = ["Method", "PointCount", "Mean", "StdDev", "Ratio"];
const widths = [39, 10, 14, 14, 8];
console.log(enclose(header.map((h, i) => pad(h, widths[i], i > 0)).join(" | ")));
console.log(enclose(widths.map((w) => "-".repeat(w)).join("-|-")));


for (const pc of POINT_COUNTS) {
    const xys = makeCircleXYs(pc);
    // Rebuild the polyline once per method (cheap) so neither benefits from
    // hidden state on a shared instance.
    const plBaseline = Polyline2D_createDirectly_EF0D882(xys.slice());
    const plWithBRect = Polyline2D_createDirectly_EF0D882(xys.slice());
    const plWithPtObjects = Polyline2D_createDirectly_EF0D882(xys.slice());

    const baseline = stats(measure(Polyline2D_tryFindSelfIntersection_Z5A89AEF5, plBaseline));
    const withBRect = stats(measure(Polyline2D_tryFindSelfIntersectionWithBRect_Z5A89AEF5, plWithBRect));
    const withPtObjects = stats(measure(Polyline2D_tryFindSelfIntersectionWithPtObjects_Z5A89AEF5, plWithPtObjects));

    const ratioBaseline = 1.0;
    const ratioWithBRect = withBRect.mean / baseline.mean;
    const ratioWithPtObjects = withPtObjects.mean / baseline.mean;

    const rows = [
        ["TryFindSelfIntersection", pc, fmtNs(baseline.mean), fmtNs(baseline.stddev), ratioBaseline.toFixed(2)],
        ["TryFindSelfIntersectionWithBRect", pc, fmtNs(withBRect.mean), fmtNs(withBRect.stddev), ratioWithBRect.toFixed(2)],
        ["TryFindSelfIntersectionWithPtObjects", pc, fmtNs(withPtObjects.mean), fmtNs(withPtObjects.stddev), ratioWithPtObjects.toFixed(2)],
    ];
    for (const row of rows) {
        console.log(enclose(row.map((c, i) => pad(c, widths[i], i > 0)).join(" | ")));
    }
    console.log(enclose(widths.map((w) => " ".repeat(w)).join(" | ")));
}
