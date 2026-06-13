
// Both sortToLoopWithReversing and sortToLoopWithReversingOLD_IMPLEMENTATION are
// already exposed in Topology2D.fs (and the Fable-compiled Topology2D.js), so no
// uncommenting is required to run this benchmark.
//
// Build the Release JS once before running:
//   cd Test && npm run buildJS
//
// Run from the repository root:
//   node Test/Benchmarks/BenchmarkTopology2DSortToLoopWithReversing.js
//
// JS counterpart of BenchmarkTopology2DSortToLoopWithReversing.fsx. see the fsx file for the tested source code.
//
// Compares three implementations of Topology2D.sortToLoopWithReversing:
//   NEW     — Topology2D_sortToLoopWithReversing_Z36DB21F5: calls getLine
//             once per element up front, then sorts via a parallel
//             flat-float64 coords block (single pass per i tracking both
//             From-to-To and To-to-To minima).
//   NOPTOBJ — Topology2D_sortToLoopWithReversingNOPTOBJ_Z36DB21F5: a single
//             minIndexByFrom scan per i, comparing the per-candidate
//             min(FromDist, ToDist) using a raw-float sqDist on the
//             Line2D's From{X,Y}/To{X,Y} — so no Pt object allocations,
//             but getLine is still invoked on every comparison.
//   OLD     — Topology2D_sortToLoopWithReversingOLD_IMPLEMENTATION_Z36DB21F5:
//             two minIndexByFrom scans per i, invoking getLine on every
//             comparison and allocating Pt objects for Pt.sqDist.
//
// Test geometry (shared between both scenarios): a closed loop of
// straight-line segments around a circle of `count` points, with a
// deterministic ~half of them reversed and Fisher–Yates shuffled, so both
// the matched-From branch and the reverse-To branch of the sort are
// exercised. Built deterministically via mulberry32(42 + count).
//
// Two element shapes are sorted over the same geometry:
//   1. Line2D path — getLine is the identity (no per-call construction);
//      reverseInPlace writes a fresh Line2D into the slot.
//   2. Segment (Record) path — input is a mutable JS object with
//      StartX/StartY/EndX/EndY properties; getLine constructs a fresh
//      Line2D from those properties on every call; reverseInPlace flips
//      Start/End properties on the element in place. This stresses the OLD
//      impl, which calls getLine O(n²) times per sort — each call now
//      allocating a Line2D.
//
// Per-call reset (paid equally by NEW and OLD within each scenario):
//   * Line2D: refill the working array slots from the frozen `original`.
//   * Segment: refill working slots AND restore each shared Segment's
//     Start/End from a Float64Array snapshot (reverseInPlace mutates the
//     Segment instances, so a deep reset is required).
//
// Methodology: each runner auto-calibrates to ~250 ms per rep, warms up
// twice, then samples over 7 reps for mean/stddev. Within each pair the
// NEW impl is the baseline; the printed Ratio is OLD / NEW.


/*
node Test/Benchmarks/BenchmarkTopology2DSortToLoopWithReversing.js
Method                                                  |    Count |           Mean |         StdDev |    Ratio
--------------------------------------------------------+----------+----------------+----------------+---------
SortToLoopWithReversing                                 |        5 |       71.17 ns |        0.93 ns |     1.00
SortToLoopWithReversingCached                           |        5 |      447.97 ns |       14.72 ns |     6.29
SortToLoopWithReversingWithPtObjects                    |        5 |      216.23 ns |        8.38 ns |     3.04
SortToLoopWithReversing_LineConstructed                 |        5 |      197.77 ns |        3.33 ns |     1.00
SortToLoopWithReversingCached_LineConstructed           |        5 |      447.02 ns |        4.61 ns |     2.26
SortToLoopWithReversingWithPtObjects_LineConstructed    |        5 |      411.49 ns |        7.11 ns |     2.08
                                                        |          |                |                |
SortToLoopWithReversing                                 |       20 |        1.54 us |       31.69 ns |     1.00
SortToLoopWithReversingCached                           |       20 |        4.08 us |       68.54 ns |     2.65
SortToLoopWithReversingWithPtObjects                    |       20 |        2.92 us |       40.50 ns |     1.89
SortToLoopWithReversing_LineConstructed                 |       20 |        2.39 us |       19.50 ns |     1.00
SortToLoopWithReversingCached_LineConstructed           |       20 |        3.95 us |       76.72 ns |     1.65
SortToLoopWithReversingWithPtObjects_LineConstructed    |       20 |        5.06 us |      105.84 ns |     2.11
                                                        |          |                |                |
SortToLoopWithReversing                                 |       30 |        3.04 us |       25.34 ns |     1.00
SortToLoopWithReversingCached                           |       30 |        7.98 us |      126.81 ns |     2.63
SortToLoopWithReversingWithPtObjects                    |       30 |        5.82 us |      103.58 ns |     1.92
SortToLoopWithReversing_LineConstructed                 |       30 |        4.96 us |       43.24 ns |     1.00
SortToLoopWithReversingCached_LineConstructed           |       30 |        7.99 us |       63.03 ns |     1.61
SortToLoopWithReversingWithPtObjects_LineConstructed    |       30 |       10.65 us |      208.80 ns |     2.15
                                                        |          |                |                |
SortToLoopWithReversing                                 |       50 |        8.13 us |      121.59 ns |     1.00
SortToLoopWithReversingCached                           |       50 |       18.33 us |      622.58 ns |     2.25
SortToLoopWithReversingWithPtObjects                    |       50 |       16.03 us |      518.57 ns |     1.97
SortToLoopWithReversing_LineConstructed                 |       50 |       15.14 us |      774.99 ns |     1.00
SortToLoopWithReversingCached_LineConstructed           |       50 |       18.92 us |      538.86 ns |     1.25
SortToLoopWithReversingWithPtObjects_LineConstructed    |       50 |       29.89 us |        1.83 us |     1.97
                                                        |          |                |                |
SortToLoopWithReversing                                 |      200 |      164.87 us |        2.28 us |     1.00
SortToLoopWithReversingCached                           |      200 |      221.53 us |        4.99 us |     1.34
SortToLoopWithReversingWithPtObjects                    |      200 |      209.07 us |        6.54 us |     1.27
SortToLoopWithReversing_LineConstructed                 |      200 |      222.87 us |        6.45 us |     1.00
SortToLoopWithReversingCached_LineConstructed           |      200 |      215.56 us |        3.15 us |     0.97
SortToLoopWithReversingWithPtObjects_LineConstructed    |      200 |      390.61 us |       10.38 us |     1.75
                                                        |          |                |                |
SortToLoopWithReversing                                 |      500 |        1.01 ms |        7.58 us |     1.00
SortToLoopWithReversingCached                           |      500 |        1.23 ms |       16.68 us |     1.23
SortToLoopWithReversingWithPtObjects                    |      500 |        1.27 ms |       70.71 us |     1.27
SortToLoopWithReversing_LineConstructed                 |      500 |        1.41 ms |       76.79 us |     1.00
SortToLoopWithReversingCached_LineConstructed           |      500 |        1.23 ms |       23.89 us |     0.88
SortToLoopWithReversingWithPtObjects_LineConstructed    |      500 |        2.70 ms |      128.89 us |     1.92

*/

import { performance } from "node:perf_hooks";
import {
    Topology2D_sortToLoopWithReversing_Z36DB21F5,
    Topology2D_sortToLoopWithReversingCached_Z36DB21F5,
    Topology2D_sortToLoopWithReversingWithPtObjects_Z36DB21F5,
} from "../dist/testsRelease/Src/Topology2D.js"; // needs to be compiled with BENCHMARK compilation symbol
import { Line2D_$ctor_77D16AC0 } from "../dist/testsRelease/Src/Line2D.js";

const COUNTS = [5, 20, 30, 50, 200, 500];

const TARGET_MS_PER_REP = 150;
const REPS = 5;
const WARMUP_REPS = 1;

// Deterministic seeded RNG (mulberry32) so both methods see identical input.
function mulberry32(seed) {
    let s = seed >>> 0;
    return function () {
        s = (s + 0x6D2B79F5) >>> 0;
        let t = Math.imul(s ^ (s >>> 15), 1 | s);
        t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
        return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
}

// Build a closed-loop set of segments on a circle, then randomly reverse ~half
// of them and shuffle the array. This ensures both the matched-From and the
// reversed (matched-To) branch of sortToLoopWithReversing are hit.
function makeOriginal(count) {
    const rng = mulberry32(42 + count);
    const radius = 100.0;
    const pts = new Array(count);
    for (let i = 0; i < count; i++) {
        const a = (2 * Math.PI * i) / count;
        pts[i] = { x: radius * Math.cos(a), y: radius * Math.sin(a) };
    }
    const lines = new Array(count);
    for (let i = 0; i < count; i++) {
        const p = pts[i];
        const q = pts[(i + 1) % count];
        if (rng() < 0.5) {
            lines[i] = Line2D_$ctor_77D16AC0(q.x, q.y, p.x, p.y);
        } else {
            lines[i] = Line2D_$ctor_77D16AC0(p.x, p.y, q.x, q.y);
        }
    }
    // Fisher–Yates shuffle so neighbouring indices are not already loop-adjacent.
    for (let i = count - 1; i > 0; i--) {
        const j = Math.floor(rng() * (i + 1));
        const t = lines[i]; lines[i] = lines[j]; lines[j] = t;
    }
    return lines;
}

const getLine = (ln) => ln;

// Build a per-method runner: each call resets a private working buffer from the
// shared `original` array (the sort mutates in place, so a fresh copy is
// required each invocation) and then calls the sort. The reverseInPlace
// closure captures the working buffer once, so no per-call allocation there.
function makeRunner(sortFn, original) {
    const n = original.length;
    const buf = new Array(n);
    const reverseInPlace = (idx, ln) => {
        buf[idx] = Line2D_$ctor_77D16AC0(ln.ToX, ln.ToY, ln.FromX, ln.FromY);
    };
    return function run() {
        for (let i = 0; i < n; i++) buf[i] = original[i];
        sortFn(getLine, reverseInPlace, buf);
    };
}

// Mutable record-style element with Start/End coord properties. Mirrors the
// F# `Segment` class. Properties are written by reverseInPlace so the same
// instance is reused across calls — the runner restores them from a snapshot.
class Segment {
    constructor(sx, sy, ex, ey) {
        this.StartX = sx;
        this.StartY = sy;
        this.EndX   = ex;
        this.EndY   = ey;
    }
}

// Build the parallel Segment scenario from the same Line2D ordering and
// orientations, so both scenarios sort identical input geometry.
function makeOriginalSegments(originalLines) {
    const n = originalLines.length;
    const segs = new Array(n);
    for (let i = 0; i < n; i++) {
        const ln = originalLines[i];
        segs[i] = new Segment(ln.FromX, ln.FromY, ln.ToX, ln.ToY);
    }
    return segs;
}

// Segment runner: getLine constructs a fresh Line2D each call, reverseInPlace
// flips the Start/End properties of the element in place. Each call restores
// both slot order AND segment properties (a deep reset) because reverseInPlace
// mutates the shared Segment instances.
function makeRunnerSegments(sortFn, originalSegs) {
    const n = originalSegs.length;
    const buf = new Array(n);
    // Snapshot of original Start/End coords, laid out as [SX, SY, EX, EY] per i.
    const snap = new Float64Array(n * 4);
    for (let i = 0; i < n; i++) {
        const s = originalSegs[i];
        const o = i * 4;
        snap[o    ] = s.StartX;
        snap[o + 1] = s.StartY;
        snap[o + 2] = s.EndX;
        snap[o + 3] = s.EndY;
    }
    const getLineSeg = (s) => Line2D_$ctor_77D16AC0(s.StartX, s.StartY, s.EndX, s.EndY);
    const reverseInPlaceSeg = (_idx, s) => {
        const sx = s.StartX, sy = s.StartY;
        s.StartX = s.EndX;
        s.StartY = s.EndY;
        s.EndX = sx;
        s.EndY = sy;
    };
    return function run() {
        for (let i = 0; i < n; i++) {
            const s = originalSegs[i];
            const o = i * 4;
            s.StartX = snap[o    ];
            s.StartY = snap[o + 1];
            s.EndX   = snap[o + 2];
            s.EndY   = snap[o + 3];
            buf[i] = s;
        }
        sortFn(getLineSeg, reverseInPlaceSeg, buf);
    };
}

function calibrate(fn, targetMs) {
    let iters = 1;
    while (true) {
        const t0 = performance.now();
        for (let i = 0; i < iters; i++) fn();
        const dt = performance.now() - t0;
        if (dt >= targetMs / 4 || iters >= 1 << 24) {
            const scale = Math.max(1, Math.round((targetMs / Math.max(dt, 0.01)) * iters));
            return scale;
        }
        iters *= 2;
    }
}

function measure(fn) {
    const iters = calibrate(fn, TARGET_MS_PER_REP);

    for (let w = 0; w < WARMUP_REPS; w++) {
        for (let i = 0; i < iters; i++) fn();
    }

    const samples = new Array(REPS);
    for (let r = 0; r < REPS; r++) {
        const t0 = performance.now();
        for (let i = 0; i < iters; i++) fn();
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
const header = ["Method", "Count", "Mean", "StdDev", "Ratio"];
const widths = [55, 8, 14, 14, 8];
console.log(enclose(header.map((h, i) => pad(h, widths[i], i > 0)).join(" | ")));
console.log(enclose(widths.map((w) => "-".repeat(w)).join("-|-")));

for (const n of COUNTS) {
    const original = makeOriginal(n);
    const originalSegs = makeOriginalSegments(original);

    const runNew        = makeRunner(Topology2D_sortToLoopWithReversing_Z36DB21F5, original);
    const runCached     = makeRunner(Topology2D_sortToLoopWithReversingCached_Z36DB21F5, original);
    const runWithPt     = makeRunner(Topology2D_sortToLoopWithReversingWithPtObjects_Z36DB21F5, original);
    const runNewSeg     = makeRunnerSegments(Topology2D_sortToLoopWithReversing_Z36DB21F5, originalSegs);
    const runCachedSeg  = makeRunnerSegments(Topology2D_sortToLoopWithReversingCached_Z36DB21F5, originalSegs);
    const runWithPtSeg  = makeRunnerSegments(Topology2D_sortToLoopWithReversingWithPtObjects_Z36DB21F5, originalSegs);

    const sNew        = stats(measure(runNew));
    const sCached     = stats(measure(runCached));
    const sWithPt     = stats(measure(runWithPt));
    const sNewSeg     = stats(measure(runNewSeg));
    const sCachedSeg  = stats(measure(runCachedSeg));
    const sWithPtSeg  = stats(measure(runWithPtSeg));

    const rows = [
        ["SortToLoopWithReversing",                     n, fmtNs(sNew.mean),        fmtNs(sNew.stddev),        (1.0).toFixed(2)],
        ["SortToLoopWithReversingCached",               n, fmtNs(sCached.mean),     fmtNs(sCached.stddev),     (sCached.mean     / sNew.mean   ).toFixed(2)],
        ["SortToLoopWithReversingWithPtObjects",        n, fmtNs(sWithPt.mean),     fmtNs(sWithPt.stddev),     (sWithPt.mean     / sNew.mean   ).toFixed(2)],
        ["SortToLoopWithReversing_LineConstructed",              n, fmtNs(sNewSeg.mean),     fmtNs(sNewSeg.stddev),     (1.0).toFixed(2)],
        ["SortToLoopWithReversingCached_LineConstructed",        n, fmtNs(sCachedSeg.mean),  fmtNs(sCachedSeg.stddev),  (sCachedSeg.mean  / sNewSeg.mean).toFixed(2)],
        ["SortToLoopWithReversingWithPtObjects_LineConstructed", n, fmtNs(sWithPtSeg.mean),  fmtNs(sWithPtSeg.stddev),   (sWithPtSeg.mean / sNewSeg.mean).toFixed(2)],
    ];
    for (const row of rows) {
        console.log(enclose(row.map((c, i) => pad(c, widths[i], i > 0)).join(" | ")));
    }
    console.log(enclose(widths.map((w) => " ".repeat(w)).join(" | ")));
}
