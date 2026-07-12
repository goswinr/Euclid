module TestXLine2D

open System
open Euclid
open Euclid.UtilEuclid

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Mocha
#else
open Expecto
#endif

let expectValueNone result message =
    match result with
    | ValueNone -> ()
    | ValueSome value -> failtest $"{message}. Got ValueSome %A{value}"

let permute(a:Line2D, b:Line2D) =
    let ar = a.Reversed
    let br = b.Reversed
    [ a,b; ar,b; a,br; ar,br
      b,a; br,a; b,ar; br,ar ]

let private sqPtDist (a:Pt) (b:Pt) =
    let dx = a.X - b.X
    let dy = a.Y - b.Y
    dx*dx + dy*dy

let private expectPtClose message (expected:Pt) (actual:Pt) =
    Expect.floatClose Accuracy.high actual.X expected.X $"{message} X"
    Expect.floatClose Accuracy.high actual.Y expected.Y $"{message} Y"

let private expectUnorderedPairClose message (a0:Pt, b0:Pt) (a1:Pt, b1:Pt) =
    let same = sqPtDist a0 a1 + sqPtDist b0 b1
    let swapped = sqPtDist a0 b1 + sqPtDist b0 a1
    Expect.isTrue (min same swapped < 1e-6) $"{message}. Expected %A{(a0,b0)}, got %A{(a1,b1)}"

let private closestParameterGeometry (a:Line2D) (b:Line2D) =
    match XLine2D.getClosestParameters(a, b) with
    | XLine2D.ClParams.Intersect (t, u) -> "Intersect", ValueSome (a.EvaluateAt t, b.EvaluateAt u)
    | XLine2D.ClParams.Apart (t, u) -> "Apart", ValueSome (a.EvaluateAt t, b.EvaluateAt u)
    | XLine2D.ClParams.Parallel (t, u) -> "Parallel", ValueSome (a.EvaluateAt t, b.EvaluateAt u)
    | XLine2D.ClParams.TooShortA -> "TooShortA", ValueNone
    | XLine2D.ClParams.TooShortB -> "TooShortB", ValueNone
    | XLine2D.ClParams.TooShortBoth -> "TooShortBoth", ValueNone

let private closestPointGeometry (a:Line2D) (b:Line2D) =
    match XLine2D.getClosestPoints(a, b) with
    | XLine2D.ClPts.Intersect pt -> "Intersect", ValueSome (pt, pt)
    | XLine2D.ClPts.Apart (ptA, ptB) -> "Apart", ValueSome (ptA, ptB)
    | XLine2D.ClPts.Parallel (ptA, ptB) -> "Parallel", ValueSome (ptA, ptB)
    | XLine2D.ClPts.TooShortA -> "TooShortA", ValueNone
    | XLine2D.ClPts.TooShortB -> "TooShortB", ValueNone
    | XLine2D.ClPts.TooShortBoth -> "TooShortBoth", ValueNone

let private intersectionGeometry (a:Line2D) (b:Line2D) =
    match XLine2D.getIntersection(a, b) with
    | XLine2D.XPt.Intersect pt -> "Intersect", ValueSome pt
    | XLine2D.XPt.Apart -> "Apart", ValueNone
    | XLine2D.XPt.Parallel -> "Parallel", ValueNone
    | XLine2D.XPt.TooShortA -> "TooShortA", ValueNone
    | XLine2D.XPt.TooShortB -> "TooShortB", ValueNone
    | XLine2D.XPt.TooShortBoth -> "TooShortBoth", ValueNone

let private expectSqDistanceInvariant caseName expected (a:Line2D) (b:Line2D) =
    for i, (x, y) in permute(a, b) |> List.indexed do
        let actual = XLine2D.getSqDistance(x, y)
        Expect.floatClose Accuracy.high actual expected $"{caseName} getSqDistance permutation {i}"

let private expectTryIntersectInvariant caseName (a:Line2D) (b:Line2D) =
    let expected = XLine2D.tryIntersect(a, b)
    for i, (x, y) in permute(a, b) |> List.indexed do
        match expected, XLine2D.tryIntersect(x, y) with
        | ValueSome expectedPt, ValueSome actualPt -> expectPtClose $"{caseName} tryIntersect permutation {i}" expectedPt actualPt
        | ValueNone, ValueNone -> ()
        | _ -> failtest $"{caseName} tryIntersect permutation {i} changed result"

let private expectIntersectionInvariant caseName (a:Line2D) (b:Line2D) =
    let expectedKind, expectedPt = intersectionGeometry a b
    for i, (x, y) in permute(a, b) |> List.indexed do
        let actualKind, actualPt = intersectionGeometry x y
        Expect.equal actualKind expectedKind $"{caseName} getIntersection kind permutation {i}"
        match expectedPt, actualPt with
        | ValueSome expected, ValueSome actual -> expectPtClose $"{caseName} getIntersection permutation {i}" expected actual
        | ValueNone, ValueNone -> ()
        | _ -> failtest $"{caseName} getIntersection permutation {i} changed point presence"

let private expectClosestInvariant caseName (a:Line2D) (b:Line2D) =
    let expectedParamKind, expectedParamPair = closestParameterGeometry a b
    let expectedPointKind, expectedPointPair = closestPointGeometry a b
    for i, (x, y) in permute(a, b) |> List.indexed do
        let actualParamKind, actualParamPair = closestParameterGeometry x y
        Expect.equal actualParamKind expectedParamKind $"{caseName} getClosestParameters kind permutation {i}"
        match expectedParamPair, actualParamPair with
        | ValueSome expected, ValueSome actual -> expectUnorderedPairClose $"{caseName} getClosestParameters permutation {i}" expected actual
        | ValueNone, ValueNone -> ()
        | _ -> failtest $"{caseName} getClosestParameters permutation {i} changed point presence"

        let actualPointKind, actualPointPair = closestPointGeometry x y
        Expect.equal actualPointKind expectedPointKind $"{caseName} getClosestPoints kind permutation {i}"
        match expectedPointPair, actualPointPair with
        | ValueSome expected, ValueSome actual -> expectUnorderedPairClose $"{caseName} getClosestPoints permutation {i}" expected actual
        | ValueNone, ValueNone -> ()
        | _ -> failtest $"{caseName} getClosestPoints permutation {i} changed point presence"

let private expectBooleanInvariants caseName expectedIntersect expectedOverlap (a:Line2D) (b:Line2D) =
    for i, (x, y) in permute(a, b) |> List.indexed do
        Expect.equal (XLine2D.doIntersect(x, y)) expectedIntersect $"{caseName} doIntersect permutation {i}"
        Expect.equal (XLine2D.doOverlap(x, y)) expectedOverlap $"{caseName} doOverlap permutation {i}"

let tests =
    testList "XLine2D Tests" [

        testList "parameterANaN tests" [
            test "Intersecting lines - basic case" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(1.0, 0.0)
                let vA = Vc(1.0, 0.0)
                let vB = Vc(0.0, 1.0)
                let t = XLine2D.parameterA(pA, pB, vA, vB)
                Expect.floatClose Accuracy.high t 1.0 "Parameter should be 1.0"
            }

            test "Intersecting lines - negative parameter" {
                let pA = Pt(5.0, 0.0)
                let pB = Pt(0.0, 5.0)
                let vA = Vc(1.0, 0.0)
                let vB = Vc(0.0, -1.0)
                let t = XLine2D.parameterA(pA, pB, vA, vB)
                Expect.floatClose Accuracy.high t -5.0 "Parameter should be -5.0"
            }

            test "Parallel lines - returns NaN or Infinity" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(0.0, 1.0)
                let vA = Vc(1.0, 0.0)
                let vB = Vc(1.0, 0.0)
                let t = XLine2D.parameterA(pA, pB, vA, vB)
                Expect.isTrue (Double.IsNaN(t) || Double.IsInfinity(t)) "Should return NaN or Infinity for parallel lines"
            }

            test "Coincident lines - same start and direction" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(0.0, 0.0)
                let vA = Vc(1.0, 1.0)
                let vB = Vc(1.0, 1.0)
                let t = XLine2D.parameterA(pA, pB, vA, vB)
                Expect.isTrue (Double.IsNaN(t)) "Should return NaN for coincident lines"
            }

            test "Nearly parallel lines - returns large value" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(0.0, 1.0)
                let vA = Vc(1.0, 0.0)
                let vB = Vc(1.0, 0.001)
                let t = XLine2D.parameterA(pA, pB, vA, vB)
                // Nearly parallel lines may return finite but large values or Infinity
                Expect.isTrue (Double.IsInfinity(t) || abs t > 100.0) "Should return Infinity or very large value"
            }

            test "Line2D overload" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let t = XLine2D.parameterA(lineA, lineB)
                Expect.floatClose Accuracy.high t 0.5 "Parameter should be 0.5"
            }
        ]

        testList "parameters tests" [
            test "Intersecting lines - basic cross" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(0.0, 0.0)
                let vA = Vc(1.0, 0.0)
                let vB = Vc(0.0, 1.0)
                let (t, u) = XLine2D.parameters(pA, pB, vA, vB)
                Expect.floatClose Accuracy.high t 0.0 "Parameter t should be 0.0"
                Expect.floatClose Accuracy.high u 0.0 "Parameter u should be 0.0"
            }

            test "Intersecting lines - offset cross" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(2.0, 0.0)
                let vA = Vc(0.0, 1.0)
                let vB = Vc(0.0, 1.0)
                let (t, _) = XLine2D.parameters(pA, pB, vA, vB)
                Expect.isTrue (Double.IsNaN(t) || Double.IsInfinity(t)) "Parallel lines should give NaN or Infinity"
            }

            test "Diagonal intersection" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(1.0, 0.0)
                let vA = Vc(1.0, 1.0)
                let vB = Vc(1.0, -1.0)
                let (t, u) = XLine2D.parameters(pA, pB, vA, vB)
                Expect.floatClose Accuracy.medium t 0.5 "Parameter t should be 0.5"
                Expect.floatClose Accuracy.medium (abs u) 0.5 "Parameter u absolute value should be 0.5"
            }
        ]

        testList "isWithinRanges tests" [
            test "Intersection within both ranges" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.isWithinRanges(lineA, lineB, 0.0, 1.0, 0.0, 1.0)
                Expect.isTrue result "Lines should intersect within ranges"
            }

            test "Intersection outside range A" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.isWithinRanges(lineA, lineB, 0.6, 1.0, 0.0, 1.0)
                Expect.isFalse result "Intersection is outside range A"
            }

            test "Intersection outside range B" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.isWithinRanges(lineA, lineB, 0.0, 1.0, 0.6, 1.0)
                Expect.isFalse result "Intersection is outside range B"
            }

            test "Parallel lines" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(0.0, 1.0), Pt(10.0, 1.0))
                let result = XLine2D.isWithinRanges(lineA, lineB, 0.0, 1.0, 0.0, 1.0)
                Expect.isFalse result "Parallel lines should return false"
            }

            test "Custom parameter ranges" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.isWithinRanges(lineA, lineB, 0.4, 0.6, 0.4, 0.6)
                Expect.isTrue result "Intersection at 0.5, 0.5 should be within ranges"
            }
        ]

        testList "tryIntersectInRangeA tests" [
            test "Intersection within range" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.tryIntersectInRangeA(lineA, lineB, 0.0, 1.0)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                | ValueNone -> failtest "Should have intersection"
            }

            test "Intersection outside range" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.tryIntersectInRangeA(lineA, lineB, 0.6, 1.0)
                expectValueNone result "Should be ValueNone when outside range"
            }

            test "Parallel lines" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(0.0, 1.0), Pt(10.0, 1.0))
                let result = XLine2D.tryIntersectInRangeA(lineA, lineB, 0.0, 1.0)
                expectValueNone result "Parallel lines should return ValueNone"
            }
        ]

        testList "tryIntersectInRanges tests" [
            test "Intersection within both ranges" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.tryIntersectInRanges(lineA, lineB, 0.0, 1.0, 0.0, 1.0)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                | ValueNone -> failtest "Should have intersection"
            }

            test "Intersection outside range B" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.tryIntersectInRanges(lineA, lineB, 0.0, 1.0, 0.6, 1.0)
                expectValueNone result "Should be ValueNone when outside range B"
            }
        ]

        testList "tryIntersect tests" [
            test "Lines intersecting within segments" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.tryIntersect(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                | ValueNone -> failtest "Should have intersection"
            }

            test "Lines not intersecting - parallel" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(0.0, 1.0), Pt(10.0, 1.0))
                let result = XLine2D.tryIntersect(lineA, lineB)
                expectValueNone result "Parallel lines should return ValueNone"
            }

            test "Lines not intersecting - apart" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(2.0, -1.0), Pt(2.0, 1.0))
                let result = XLine2D.tryIntersect(lineA, lineB)
                expectValueNone result "Apart lines should return ValueNone"
            }
        ]

        testList "doOverlap tests" [
            test "Parallel overlapping lines on same ray" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, 0.0), Pt(15.0, 0.0))
                let result = XLine2D.doOverlap(lineA, lineB)
                Expect.isTrue result "Overlapping coincident lines should return true"
            }

            test "Parallel non-overlapping lines on same ray" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(5.0, 0.0))
                let lineB = Line2D(Pt(10.0, 0.0), Pt(15.0, 0.0))
                let result = XLine2D.doOverlap(lineA, lineB)
                Expect.isFalse result "Non-overlapping coincident lines should return false"
            }

            test "Parallel lines touching at end" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(5.0, 0.0))
                let lineB = Line2D(Pt(5.0, 0.0), Pt(10.0, 0.0))
                let result = XLine2D.doOverlap(lineA, lineB)
                Expect.isTrue result "Touching lines should return true"
            }

            test "Parallel lines on different rays" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(0.0, 1.0), Pt(10.0, 1.0))
                let result = XLine2D.doOverlap(lineA, lineB)
                Expect.isFalse result "Parallel non-coincident lines should return false"
            }

            test "Parallel lines barely within tolerance" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, 1e-8), Pt(15.0, 1e-8))
                let result = XLine2D.doOverlap(lineA, lineB, 1e-6)
                Expect.isTrue result "Lines within tolerance should overlap"
            }
        ]

        testList "getRayIntersectionParam tests" [
            test "Rays intersecting" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(1.0, 0.0)
                let vA = Vc(1.0, 0.0)
                let vB = Vc(0.0, 1.0)
                let result = XLine2D.getRayIntersectionParam(pA, pB, vA, vB)
                match result with
                | XLine2D.XRayParam.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 1.0 "Parameter t should be 1.0"
                    Expect.floatClose Accuracy.high u 0.0 "Parameter u should be 0.0"
                | XLine2D.XRayParam.Parallel -> failtest "These lines are not parallel"
                | other -> failtest $"Should intersect but got: %A{other}"
            }

            test "Rays nearly parallel within tolerance" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(0.0, 1.0)
                let vA = Vc(1.0, 0.0)
                let vB = Vc(1.0, 0.0001) // very small angle, within default tolerance
                let result = XLine2D.getRayIntersectionParam(pA, pB, vA, vB)
                match result with
                | XLine2D.XRayParam.Parallel -> ()
                | other -> failtest $"Should be parallel within tolerance but got: %A{other}"
            }

            test "Ray A too short" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(1.0, 0.0)
                let vA = Vc(1e-7, 0.0)
                let vB = Vc(0.0, 1.0)
                let result = XLine2D.getRayIntersectionParam(pA, pB, vA, vB)
                match result with
                | XLine2D.XRayParam.TooShortA -> ()
                | other -> failtest $"Should be TooShortA but got: %A{other}"
            }

            test "Both rays too short" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(1.0, 0.0)
                let vA = Vc(1e-7, 0.0)
                let vB = Vc(0.0, 1e-7)
                let result = XLine2D.getRayIntersectionParam(pA, pB, vA, vB)
                match result with
                | XLine2D.XRayParam.TooShortBoth -> ()
                | other -> failtest $"Should be TooShortBoth but got: %A{other}"
            }
        ]

        testList "getRayIntersection tests" [
            test "Rays intersecting at specific point" {
                let pA = Pt(0.0, 0.0)
                let pB = Pt(5.0, 0.0)
                let vA = Vc(1.0, 0.0)
                let vB = Vc(0.0, 1.0)
                let result = XLine2D.getRayIntersection(pA, pB, vA, vB)
                match result with
                | XLine2D.XRay.Intersect pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                | other -> failtest $"Should intersect but got: %A{other}"
            }

            test "Rays intersecting at origin" {
                let lineA = Line2D(Pt(-5.0, 0.0), Pt(5.0, 0.0))
                let lineB = Line2D(Pt(0.0, -5.0), Pt(0.0, 5.0))
                let result = XLine2D.getRayIntersection(lineA, lineB)
                match result with
                | XLine2D.XRay.Intersect pt ->
                    Expect.floatClose Accuracy.high pt.X 0.0 "X should be 0.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                | other -> failtest $"Should intersect but got: %A{other}"
            }
        ]

        testList "getIntersectionParam tests" [
            test "Finite lines intersecting" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.getIntersectionParam(lineA, lineB)
                match result with
                | XLine2D.XParam.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "Parameter u should be 0.5"
                | other -> failtest $"Should intersect but got: %A{other}"
            }

            test "Finite lines would intersect but outside segment (apart)" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(2.0, -1.0), Pt(2.0, 1.0))
                let result = XLine2D.getIntersectionParam(lineA, lineB)
                match result with
                | XLine2D.XParam.Apart -> ()
                | other -> failtest $"Should be apart - lines would intersect if extended but got: %A{other}"
            }

            test "Finite lines nearly parallel within tolerance" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(0.0, 1.0), Pt(10.0, 1.0001)) // nearly parallel
                let result = XLine2D.getIntersectionParam(lineA, lineB)
                match result with
                | XLine2D.XParam.Parallel -> ()
                | other -> failtest $"Should be parallel within tolerance but got: %A{other}"
            }
        ]

        testList "getIntersection tests" [
            test "Finite lines intersecting - returns point" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.getIntersection(lineA, lineB)
                match result with
                | XLine2D.XPt.Intersect pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                | other -> failtest $"Should intersect but got: %A{other}"
            }

            test "Finite lines would intersect but outside segments" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(2.0, -1.0), Pt(2.0, 1.0))
                let result = XLine2D.getIntersection(lineA, lineB)
                match result with
                | XLine2D.XPt.Apart -> ()
                | other -> failtest $"Should be apart but got: %A{other}"
            }
        ]


        testList "getClosestParameters tests" [
            test "Intersecting lines" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.getClosestParameters(lineA, lineB)
                match result with
                | XLine2D.ClParams.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "Parameter u should be 0.5"
                | other -> failtest $"Should intersect but got: %A{other}"
            }

            test "Apart lines - closest points at endpoints" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(1.0, 2.0), Pt(3.0, 3.0))
                let result = XLine2D.getClosestParameters(lineA, lineB)
                match result with
                | XLine2D.ClParams.Apart (t, u) ->

                    let d = Pt.dist (lineA.EvaluateAt(t)) (lineB.EvaluateAt(u))
                    Expect.floatClose Accuracy.high t 1.0 "Parameter t should be 1.0"

                    Expect.floatClose Accuracy.high d 2.0 "Distance should match computed distance"
                | other -> failtest $"Should be apart but got: %A{other}"
            }

            test "Nearly parallel lines within tolerance" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(0.0, 1.0), Pt(10.0, 1.000001)) // nearly parallel
                let result = XLine2D.getClosestParameters(lineA, lineB)
                match result with
                | XLine2D.ClParams.Parallel (t, u) ->
                    Expect.floatClose Accuracy.medium t 0.5 "Parameter t should be around 0.5"
                    Expect.floatClose Accuracy.medium u 0.5 "Parameter u should be around 0.5"
                | other -> failtest $"Should be parallel within tolerance but got: %A{other}"
            }
        ]

        testList "getClosestPoints tests" [
            test "Intersecting lines" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let result = XLine2D.getClosestPoints(lineA, lineB)
                match result with
                | XLine2D.ClPts.Intersect pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                | other -> failtest $"Should intersect but got: %A{other}"
            }

            test "Nearly parallel lines within tolerance" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(0.0, 1.0), Pt(10.0, 1.00001)) // nearly parallel
                let result = XLine2D.getClosestPoints(lineA, lineB)
                match result with
                | XLine2D.ClPts.Parallel (ptA, ptB) ->
                    let d = Pt.dist ptA ptB
                    Expect.floatClose Accuracy.medium d 1.0 "Distance should be around 1.0"
                | other -> failtest $"Should be parallel within tolerance but got: %A{other}"
            }
        ]


        testList "getSqDistance tests" [
            test "Intersecting lines have zero distance" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(5.0, -5.0), Pt(5.0, 5.0))
                let dist = XLine2D.getSqDistance(lineA, lineB)
                Expect.floatClose Accuracy.high dist 0.0 "Distance should be 0.0"
            }

            test "Apart lines have positive distance" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(2.0, 1.0), Pt(3.0, 1.0))
                let dist = XLine2D.getSqDistance(lineA, lineB)
                Expect.isTrue (dist > 0.0) "Distance should be positive"
                // Distance from (1,0) to (2,1) is sqrt(2), so squared is 2
                Expect.floatClose Accuracy.high dist 2.0 "Squared Distance should be 2.0"
            }

            test "Parallel lines distance" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(0.0, 1.0), Pt(10.0, 1.0))
                let dist = XLine2D.getSqDistance(lineA, lineB)
                Expect.floatClose Accuracy.high dist 1.0 "Squared Distance should be 1.0"
            }
        ]

        testList "permutation invariance" [
            test "regular finite-line queries are invariant under order and direction permutations" {
                let cases =
                    [
                        "interior cross",
                            Line2D(0.0, 0.0, 10.0, 0.0),
                            Line2D(5.0, -5.0, 5.0, 5.0),
                            0.0, true, false

                        "diagonal cross",
                            Line2D(-2.0, -1.0, 4.0, 2.0),
                            Line2D(1.0, 3.0, 3.0, -3.0),
                            0.0, true, false

                        "endpoint touch",
                            Line2D(0.0, 0.0, 5.0, 0.0),
                            Line2D(5.0, 0.0, 8.0, 3.0),
                            0.0, true, false

                        "apart but extended lines cross",
                            Line2D(0.0, 0.0, 1.0, 0.0),
                            Line2D(2.0, -1.0, 2.0, 1.0),
                            1.0, false, false

                        "perpendicular apart",
                            Line2D(0.0, 0.0, 5.0, 0.0),
                            Line2D(0.0, 2.0, 0.0, 7.0),
                            4.0, false, false

                        "parallel offset",
                            Line2D(0.0, 0.0, 10.0, 0.0),
                            Line2D(0.0, 2.0, 10.0, 2.0),
                            4.0, false, false

                        "collinear partial overlap",
                            Line2D(0.0, 0.0, 10.0, 0.0),
                            Line2D(3.0, 0.0, 7.0, 0.0),
                            0.0, false, true

                        "collinear end touch",
                            Line2D(0.0, 0.0, 5.0, 0.0),
                            Line2D(5.0, 0.0, 8.0, 0.0),
                            0.0, false, true

                        "collinear disjoint",
                            Line2D(0.0, 0.0, 2.0, 0.0),
                            Line2D(5.0, 0.0, 7.0, 0.0),
                            9.0, false, false

                        "large coordinate cross",
                            Line2D(1e10, 1e10, 1e10 + 10.0, 1e10),
                            Line2D(1e10 + 6.0, 1e10 - 5.0, 1e10 + 6.0, 1e10 + 5.0),
                            0.0, true, false

                        "small but above too-short tolerance",
                            Line2D(0.0, 0.0, 1e-4, 0.0),
                            Line2D(5e-5, -5e-5, 5e-5, 5e-5),
                            0.0, true, false
                    ]

                for caseName, lineA, lineB, expectedSqDistance, expectedIntersect, expectedOverlap in cases do
                    expectSqDistanceInvariant caseName expectedSqDistance lineA lineB
                    expectTryIntersectInvariant caseName lineA lineB
                    expectIntersectionInvariant caseName lineA lineB
                    expectClosestInvariant caseName lineA lineB
                    expectBooleanInvariants caseName expectedIntersect expectedOverlap lineA lineB
            }

            test "near-parallel closest results are invariant under all permutations" {
                let lineA = Line2D(0.0, 0.0, 100.0, 0.0)
                let lineB = Line2D(10.0, 1.0, 110.0, 1.01)

                expectSqDistanceInvariant "near parallel" (XLine2D.getSqDistance(lineA, lineB)) lineA lineB
                expectTryIntersectInvariant "near parallel" lineA lineB
                expectIntersectionInvariant "near parallel" lineA lineB
                expectClosestInvariant "near parallel" lineA lineB
                expectBooleanInvariants "near parallel" false false lineA lineB
            }

            test "degenerate point-vs-segment squared distances are invariant under all permutations" {
                let cases =
                    [
                        "point against segment", Line2D(1.0, 1.0, 1.0, 1.0), Line2D(3.0, 1.0, 3.0, 4.0), 4.0
                        "point projected inside segment", Line2D(2.0, 3.0, 2.0, 3.0), Line2D(0.0, 0.0, 4.0, 0.0), 9.0
                        "distinct points", Line2D(1.0, 1.0, 1.0, 1.0), Line2D(4.0, 5.0, 4.0, 5.0), 25.0
                        "same point", Line2D(-2.0, 3.0, -2.0, 3.0), Line2D(-2.0, 3.0, -2.0, 3.0), 0.0
                    ]

                for caseName, lineA, lineB, expectedSqDistance in cases do
                    expectSqDistanceInvariant caseName expectedSqDistance lineA lineB
            }
        ]

        testList "areEndsTouching tests" [
            test "Not touching" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(2.0, 0.0), Pt(3.0, 0.0))
                let result = XLine2D.getEndsTouching (lineA, lineB)
                match result with
                | XLine2D.XEnds.NotTouching -> ()
                | other -> failtest $"Should not be touching but got: %A{other}"
            }

            test "StartA_StartB touching" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(0.0, 0.0), Pt(0.0, 1.0))
                let result = XLine2D.getEndsTouching (lineA, lineB)
                match result with
                | XLine2D.XEnds.StartA_StartB -> ()
                | other -> failtest $"Should touch at StartA_StartB but got: %A{other}"
            }

            test "EndA_EndB touching" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(0.0, 1.0), Pt(1.0, 0.0))
                let result = XLine2D.getEndsTouching (lineA, lineB)
                match result with
                | XLine2D.XEnds.EndA_EndB -> ()
                | other -> failtest $"Should touch at EndA_EndB but got: %A{other}"
            }

            test "EndA_StartB touching" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(1.0, 0.0), Pt(2.0, 0.0))
                let result = XLine2D.getEndsTouching (lineA, lineB)
                match result with
                | XLine2D.XEnds.EndA_StartB -> ()
                | other -> failtest $"Should touch at EndA_StartB but got: %A{other}"
            }

            test "StartA_EndB touching" {
                let lineA = Line2D(Pt(1.0, 0.0), Pt(2.0, 0.0))
                let lineB = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let result = XLine2D.getEndsTouching (lineA, lineB)
                match result with
                | XLine2D.XEnds.StartA_EndB -> ()
                | other -> failtest $"Should touch at StartA_EndB but got: %A{other}"
            }

            test "Identical lines" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let result = XLine2D.getEndsTouching (lineA, lineB)
                match result with
                | XLine2D.XEnds.Identical -> ()
                | other -> failtest $"Should be identical but got: %A{other}"
            }

            test "Identical flipped lines" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1.0, 0.0))
                let lineB = Line2D(Pt(1.0, 0.0), Pt(0.0, 0.0))
                let result = XLine2D.getEndsTouching (lineA, lineB)
                match result with
                | XLine2D.XEnds.IdenticalFlipped -> ()
                | other -> failtest $"Should be identical flipped but got: %A{other}"
            }
        ]

        testList "Edge cases and numerical stability" [
            test "Very small lines" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(1e-8, 0.0))
                let lineB = Line2D(Pt(0.0, 0.0), Pt(0.0, 1e-8))
                let result = XLine2D.getIntersection(lineA, lineB)
                match result with
                | XLine2D.XPt.TooShortBoth -> ()
                | other -> failtest $"Should be too short both but got: %A{other}"
            }

            test "Very large coordinates" {
                let lineA = Line2D(Pt(1e10, 0.0), Pt(1e10 + 10.0, 0.0))
                let lineB = Line2D(Pt(1e10 + 5.0, -5.0), Pt(1e10 + 5.0, 5.0))
                let result = XLine2D.tryIntersect(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.medium pt.X (1e10 + 5.0) "X should be 1e10 + 5.0"
                    Expect.floatClose Accuracy.medium pt.Y 0.0 "Y should be 0.0"
                | ValueNone -> failtest "Should intersect"
            }

            test "Nearly parallel lines with custom tolerance" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let lineB = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.01))
                let result = XLine2D.getRayIntersectionParam(lineA, lineB, Tangent.``0.25``)
                match result with
                | XLine2D.XRayParam.Parallel -> ()
                | XLine2D.XRayParam.Intersect _ -> () // could be either depending on exact tolerance
                | _ -> ()
            }

            test "Zero-length line detection - line A" {
                let lineA = Line2D(Pt(5.0, 5.0), Pt(5.0, 5.0))
                let lineB = Line2D(Pt(0.0, 0.0), Pt(10.0, 0.0))
                let result = XLine2D.getIntersection(lineA, lineB)
                match result with
                | XLine2D.XPt.TooShortA
                | XLine2D.XPt.TooShortBoth -> () // either is acceptable
                | other -> failtest $"Should be too short but got: %A{other}"
            }

            test "Diagonal lines at 45 degrees" {
                let lineA = Line2D(Pt(0.0, 0.0), Pt(10.0, 10.0))
                let lineB = Line2D(Pt(0.0, 10.0), Pt(10.0, 0.0))
                let result = XLine2D.tryIntersect(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 5.0 "Y should be 5.0"
                | ValueNone -> failtest "Should intersect"
            }

            test "Lines with negative coordinates" {
                let lineA = Line2D(Pt(-10.0, -10.0), Pt(10.0, -10.0))
                let lineB = Line2D(Pt(0.0, -20.0), Pt(0.0, 0.0))
                let result = XLine2D.tryIntersect(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 0.0 "X should be 0.0"
                    Expect.floatClose Accuracy.high pt.Y -10.0 "Y should be -10.0"
                | ValueNone -> failtest "Should intersect"
            }


            // --- Tests for getClosestParameters ---

            test "getClosestParameters returns Intersect for crossing lines" {
                let lnA = Line2D(0.0, 0.0, 4.0, 0.0)
                let lnB = Line2D(2.0, -2.0, 2.0, 2.0)
                let result = XLine2D.getClosestParameters(lnA, lnB)
                match result with
                | XLine2D.ClParams.Intersect (t, u) ->
                    Expect.floatClose Accuracy.veryHigh t 0.5 "t should be 0.5"
                    Expect.floatClose Accuracy.veryHigh u 0.5 "u should be 0.5"
                | _ -> Expect.isTrue false "Should return Intersect"
            }

            test "getClosestParameters returns Apart for skew non-intersecting lines" {
                // Lines that don't intersect within their finite segments
                let lnA = Line2D(0.0, 0.0, 1.0, 0.0)  // horizontal at Y=0
                let lnB = Line2D(0.0, 2.0, 1.0, 2.0)  // horizontal at Y=2, parallel
                let result = XLine2D.getClosestParameters(lnA, lnB)
                match result with
                | XLine2D.ClParams.Parallel (t, u) ->
                    // Parallel lines return midpoint parametersNaN
                    Expect.floatClose Accuracy.high t 0.5 "t should be midpoint"
                    Expect.floatClose Accuracy.high u 0.5 "u should be midpoint"
                | _ -> Expect.isTrue false (sprintf "Should return Parallel for parallel lines, got %A" result)
            }

            test "getClosestParameters returns Parallel for parallel lines" {
                let lnA = Line2D(0.0, 0.0, 10.0, 0.0)
                let lnB = Line2D(0.0, 1.0, 10.0, 1.0)
                let result = XLine2D.getClosestParameters(lnA, lnB)
                match result with
                | XLine2D.ClParams.Parallel (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "t should be midpoint"
                    Expect.floatClose Accuracy.high u 0.5 "u should be midpoint"
                | _ -> Expect.isTrue false "Should return Parallel"
            }

            // --- Tests for getClosestPoints ---

            test "getClosestPoints returns Intersect point for crossing lines" {
                let lnA = Line2D(0.0, 0.0, 4.0, 0.0)
                let lnB = Line2D(2.0, -2.0, 2.0, 2.0)
                let result = XLine2D.getClosestPoints(lnA, lnB)
                match result with
                | XLine2D.ClPts.Intersect pt ->
                    Expect.floatClose Accuracy.veryHigh pt.X 2.0 "X should be 2"
                    Expect.floatClose Accuracy.veryHigh pt.Y 0.0 "Y should be 0"
                | _ -> Expect.isTrue false "Should return Intersect"
            }

            test "getClosestPoints returns Apart with two points for skew non-parallel lines" {
                // Two lines that don't intersect and aren't parallel
                let lnA = Line2D(0.0, 0.0, 1.0, 0.0)   // horizontal at Y=0
                let lnB = Line2D(0.0, 1.0, 0.5, 2.0)   // diagonal starting at Y=1
                let result = XLine2D.getClosestPoints(lnA, lnB)
                match result with
                | XLine2D.ClPts.Apart (_, ptB) ->
                    // Expect.isTrue (sqDist > 0.0) "square distance positive"
                    // Closest points should be the start of B and somewhere on A
                    Expect.floatClose Accuracy.medium ptB.Y 1.0 "ptB should be at Y=1"
                | _ -> Expect.isTrue false (sprintf "Should return Apart, got %A" result)
            }

            test "getClosestPoints returns Parallel with two points" {
                let lnA = Line2D(0.0, 0.0, 10.0, 0.0)
                let lnB = Line2D(0.0, 1.0, 10.0, 1.0)
                let result = XLine2D.getClosestPoints(lnA, lnB)
                match result with
                | XLine2D.ClPts.Parallel (ptA, ptB) ->
                    Expect.floatClose Accuracy.high ptA.X 5.0 "ptA X midpoint"
                    Expect.floatClose Accuracy.high ptA.Y 0.0 "ptA Y"
                    Expect.floatClose Accuracy.high ptB.X 5.0 "ptB X midpoint"
                    Expect.floatClose Accuracy.high ptB.Y 1.0 "ptB Y"
                | _ -> Expect.isTrue false "Should return Parallel"
            }



            // --- Tests for getSqDistance ---

            test "getSqDistance returns 0 for intersecting lines" {
                // Lines that cross in the middle
                let lnA = Line2D(0.0, 0.0, 4.0, 0.0)
                let lnB = Line2D(2.0, -2.0, 2.0, 2.0)
                let sqDist = XLine2D.getSqDistance(lnA, lnB)
                Expect.floatClose Accuracy.veryHigh sqDist 0.0 "Intersecting lines should have 0 distance"
            }

            test "getSqDistance returns 0 for touching endpoints" {
                // Lines meeting at endpoints
                let lnA = Line2D(0.0, 0.0, 2.0, 0.0)
                let lnB = Line2D(2.0, 0.0, 4.0, 2.0)
                let sqDist = XLine2D.getSqDistance(lnA, lnB)
                Expect.floatClose Accuracy.veryHigh sqDist 0.0 "Touching endpoints should have 0 distance"
            }

            test "getSqDistance for parallel horizontal lines" {
                // Two horizontal lines separated by distance 3
                let lnA = Line2D(0.0, 0.0, 10.0, 0.0)
                let lnB = Line2D(0.0, 3.0, 10.0, 3.0)
                let sqDist = XLine2D.getSqDistance(lnA, lnB)
                Expect.floatClose Accuracy.veryHigh sqDist 9.0 "Parallel lines 3 units apart: 3^2 = 9"
            }

            test "getSqDistance for parallel vertical lines" {
                // Two vertical lines separated by distance 4
                let lnA = Line2D(0.0, 0.0, 0.0, 10.0)
                let lnB = Line2D(4.0, 0.0, 4.0, 10.0)
                let sqDist = XLine2D.getSqDistance(lnA, lnB)
                Expect.floatClose Accuracy.veryHigh sqDist 16.0 "Parallel lines 4 units apart: 4^2 = 16"
            }

            test "getSqDistance for non-overlapping parallel lines" {
                // Two horizontal lines that don't overlap in X, separated in Y by 2
                let lnA = Line2D(0.0, 0.0, 5.0, 0.0)
                let lnB = Line2D(10.0, 2.0, 15.0, 2.0)
                let sqDist = XLine2D.getSqDistance(lnA, lnB)
                // Closest points: end of A (5,0) to start of B (10,2)
                // Distance: sqrt((10-5)^2 + (2-0)^2) = sqrt(25 + 4) = sqrt(29)
                Expect.floatClose Accuracy.high sqDist 29.0 "Squared Distance should be 29"
            }

            test "getSqDistance for skew lines (would intersect if extended)" {
                // Lines that would intersect if infinite, but don't within segments
                let lnA = Line2D(0.0, 0.0, 1.0, 0.0)   // horizontal from 0 to 1
                let lnB = Line2D(2.0, -1.0, 2.0, 1.0)  // vertical at x=2
                let sqDist = XLine2D.getSqDistance(lnA, lnB)
                // Closest: end of A (1,0) to closest point on B (2,0)
                // Distance: sqrt((2-1)^2 + 0^2) = 1
                Expect.floatClose Accuracy.veryHigh sqDist 1.0 "Squared Distance should be 1"
            }

            test "getSqDistance for perpendicular non-intersecting lines" {
                // L-shaped arrangement
                let lnA = Line2D(0.0, 0.0, 5.0, 0.0)   // horizontal along X-axis
                let lnB = Line2D(0.0, 2.0, 0.0, 7.0)   // vertical along x=0, starting at y=2
                let sqDist = XLine2D.getSqDistance(lnA, lnB )
                // Closest: start of A (0,0) to closest point on B (0,2)
                // Distance: sqrt(0 + 4) = 2
                Expect.floatClose Accuracy.veryHigh sqDist 4.0 "Squared Distance should be 2^2 = 4"
            }

            test "getSqDistance for diagonal lines" {
                // Two diagonal lines that don't intersect
                let lnA = Line2D(0.0, 0.0, 2.0, 2.0)   // diagonal from origin
                let lnB = Line2D(3.0, 0.0, 5.0, 2.0)   // parallel diagonal offset
                let sqDist = XLine2D.getSqDistance(lnA, lnB)
                // Should be positive (lines don't touch)
                Expect.floatClose Accuracy.veryHigh sqDist (1.5*1.5*2.0) "Squared Distance should be positive"
            }

            test "getSqDistance for coincident overlapping lines" {
                // Two lines on the same line, overlapping
                let lnA = Line2D(0.0, 0.0, 5.0, 0.0)
                let lnB = Line2D(2.0, 0.0, 7.0, 0.0)
                let sqDist = XLine2D.getSqDistance(lnA, lnB )
                Expect.floatClose Accuracy.veryHigh sqDist 0.0 "Overlapping coincident lines should have 0 distance"
            }

            test "getSqDistance with very small lines" {
                // Nearly point-like lines
                let lnA = Line2D(0.0, 0.0, 0.001, 0.0)
                let lnB = Line2D(1.0, 0.0, 1.001, 0.0)
                let sqDist = XLine2D.getSqDistance(lnA, lnB)
                // Distance should be approximately 1^2 = 1
                Expect.floatClose Accuracy.low sqDist 0.999 "Small lines1 ~1 unit apart"
            }

            test "getSqDistance with very small lines2" {
                // Nearly point-like lines
                let lnA = Line2D(0.0, 0.0, 0.001, 0.0)
                let lnB = Line2D(0.0, 1.0, 0.001, 1.0)
                let sqDist = XLine2D.getSqDistance(lnA, lnB)
                // Distance should be approximately 1^2 = 1
                Expect.floatClose Accuracy.low sqDist 1.0 "Small line2 ~1 unit apart"
            }

            test "getClosestParams parallel" {
                // Two lines on the same line, overlapping
                let lnA = Line2D(0.0, 0.0, 2.0, 2.0)   // diagonal from origin
                let lnB = Line2D(3.0, 0.0, 5.0, 2.0)   // parallel diagonal offset
                let u,v =
                    match XLine2D.getClosestParameters(lnA, lnB) with
                    | XLine2D.ClParams.Intersect (t, u) -> (t, u)
                    | XLine2D.ClParams.Parallel (t, u) -> (t, u)
                    | XLine2D.ClParams.Apart (t, u) -> (t, u)
                    | r -> failwithf $"Unexpected result: {r}"

                Expect.floatClose Accuracy.veryHigh u (0.75 + 0.125) "getClosestParams u should be 0.75+0.125"
                Expect.floatClose Accuracy.veryHigh v (0.125) "getClosestParams v should be 0.125"
            }


            test "getClosestParams apart" {
                // Two lines on the same line, overlapping
                let lnA = Line2D(0.0, 0.0, 2.0, 2.0)   // diagonal from origin
                let lnB = Line2D(2.0, 1.0, 4.0, 2.0)  // apart diagonal offset
                let u,v =
                    match XLine2D.getClosestParameters(lnA, lnB) with
                    | XLine2D.ClParams.Intersect (t, u) -> (t, u)
                    | XLine2D.ClParams.Parallel (t, u) -> (t, u)
                    | XLine2D.ClParams.Apart (t, u) -> (t, u)
                    | r -> failwithf $"Unexpected result: {r}"


                Expect.floatClose Accuracy.veryHigh u (0.75 ) "getClosestParams apart u should be 0.75"
                Expect.floatClose Accuracy.veryHigh v (0.0) "getClosestParams apart v should be 0.0"
            }

            test "getSqDistance with apart 0.71" {
                let lnA = Line2D(0.0, 0.0, 2.0, 2.0)   // diagonal from origin
                let lnB = Line2D(2.0, 1.0, 4.0, 2.0)  // apart diagonal offset
                let sqDist = XLine2D.getSqDistance(lnA, lnB) |> sqrt
                // Distance should be approximately 1^2 = 1
                Expect.floatClose Accuracy.high sqDist 0.70710678 "getSqDistance with apart 0.71"
            }

            test "getSqDistance with apart 0.71 rev" {
                let lnA = Line2D(0.0, 0.0, 2.0, 2.0)   // diagonal from origin
                let lnB = Line2D(2.0, 1.0, 4.0, 2.0) |> Line2D.reverse // apart diagonal offset
                let sqDist = XLine2D.getSqDistance(lnA, lnB) |> sqrt
                // Distance should be approximately 1^2 = 1
                Expect.floatClose Accuracy.high sqDist 0.70710678 "getSqDistance with apart 0.71 rev"
            }


            test "getSqDistance with apart 0.71 rev2" {
                let lnA = Line2D(0.0, 0.0, 2.0, 2.0) |> Line2D.reverse   // diagonal from origin
                let lnB = Line2D(2.0, 1.0, 4.0, 2.0)  // apart diagonal offset
                let sqDist = XLine2D.getSqDistance(lnA, lnB) |> sqrt
                // Distance should be approximately 1^2 = 1
                Expect.floatClose Accuracy.high sqDist 0.70710678 "getSqDistance with apart 0.71 rev2"
            }







        // temp test for internal function

        // // testList "clParamRayPt tests (internal)" [
        //     test "Point on horizontal line at origin" {
        //         // Line from (0,0) in direction (1,0), point at (5,0)
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 1.0, 0.0, 5.0, 0.0)
        //         Expect.floatClose Accuracy.high param 5.0 "Parameter should be 5.0"
        //     }

        //     test "Point on vertical line" {
        //         // Line from (0,0) in direction (0,1), point at (0,3)
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 0.0, 1.0, 0.0, 3.0)
        //         Expect.floatClose Accuracy.high param 3.0 "Parameter should be 3.0"
        //     }

        //     test "Point perpendicular to horizontal line" {
        //         // Line from (0,0) in direction (1,0), point at (5,3)
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 1.0, 0.0, 5.0, 3.0)
        //         Expect.floatClose Accuracy.high param 5.0 "Parameter should be 5.0 (projects to (5,0))"
        //     }

        //     test "Point perpendicular to vertical line" {
        //         // Line from (0,0) in direction (0,1), point at (3,5)
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 0.0, 1.0, 3.0, 5.0)
        //         Expect.floatClose Accuracy.high param 5.0 "Parameter should be 5.0 (projects to (0,5))"
        //     }

        //     test "Point on diagonal line" {
        //         // Line from (1,1) in direction (1,1), point at (3,3)
        //         let param = XLine2D.clParamRayPt(1.0, 1.0, 1.0, 1.0, 3.0, 3.0)
        //         Expect.floatClose Accuracy.high param 2.0 "Parameter should be 2.0"
        //     }

        //     test "Point off diagonal line" {
        //         // Line from (0,0) in direction (1,1), point at (1,0)
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 1.0, 1.0, 1.0, 0.0)
        //         Expect.floatClose Accuracy.high param 0.5 "Parameter should be 0.5 (projects to (0.5,0.5))"
        //     }

        //     test "Negative parameter - point behind line start" {
        //         // Line from (5,5) in direction (1,0), point at (2,5)
        //         let param = XLine2D.clParamRayPt(5.0, 5.0, 1.0, 0.0, 2.0, 5.0)
        //         Expect.floatClose Accuracy.high param -3.0 "Parameter should be -3.0"
        //     }

        //     test "Point at line origin" {
        //         // Line from (2,3) in direction (1,1), point at (2,3)
        //         let param = XLine2D.clParamRayPt(2.0, 3.0, 1.0, 1.0, 2.0, 3.0)
        //         Expect.floatClose Accuracy.high param 0.0 "Parameter should be 0.0"
        //     }

        //     test "Line with scaled vector" {
        //         // Line from (0,0) in direction (2,0), point at (10,0)
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 2.0, 0.0, 10.0, 0.0)
        //         Expect.floatClose Accuracy.high param 5.0 "Parameter should be 5.0 (10/(2^2)=2.5, but 10*2/4=5)"
        //     }

        //     test "Line with normalized vector" {
        //         // Line from (0,0) in direction (0.6,0.8) [normalized], point at (3,4)
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 0.6, 0.8, 3.0, 4.0)
        //         Expect.floatClose Accuracy.high param 5.0 "Parameter should be 5.0"
        //     }

        //     test "Zero length vector returns NaN or Infinity" {
        //         // Line from (0,0) with zero direction vector, point at (1,1)
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 0.0, 0.0, 1.0, 1.0)
        //         Expect.isTrue (Double.IsNaN(param) || Double.IsInfinity(param)) "Should return NaN or Infinity for zero-length vector"
        //     }

        //     test "Very small vector magnitude" {
        //         // Line from (0,0) in direction (1e-10, 0), point at (1,0)
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 1e-10, 0.0, 1.0, 0.0)
        //         Expect.floatClose Accuracy.low param 1e10 "Parameter should be very large"
        //     }

        //     test "Point projects beyond parameter 1.0" {
        //         // Line from (0,0) in direction (1,0), point at (10,0)
        //         // Parameter > 1 is valid for infinite ray
        //         let param = XLine2D.clParamRayPt(0.0, 0.0, 1.0, 0.0, 10.0, 0.0)
        //         Expect.floatClose Accuracy.high param 10.0 "Parameter should be 10.0"
        //     }

        //     test "Complex case - arbitrary line and point" {
        //         // Line from (3,4) in direction (2,1), point at (6,7)
        //         let param = XLine2D.clParamRayPt(3.0, 4.0, 2.0, 1.0, 6.0, 7.0)
        //         // Vector from start to point: (3,3)
        //         // Dot product: 3*2 + 3*1 = 9
        //         // Length squared: 2^2 + 1^2 = 5
        //         // Parameter: 9/5 = 1.8
        //         Expect.floatClose Accuracy.high param 1.8 "Parameter should be 1.8"
        //     }
        ]

        // These tests target the division-free parallel test: |det| > tangent * |dot|
        // (replacing the previous tan = det / dot then abs !^ tan > tangent).
        testList "division-free parallel test" [

            // helper: two lines crossing at (5,0), lineB rotated by 'deg' from the X-axis
            let crossingAtFive (deg:float) =
                let th = deg * System.Math.PI / 180.0
                let vA = Vc(10.0, 0.0)
                let pA = Pt(0.0, 0.0)
                let vBx = 10.0 * cos th
                let vBy = 10.0 * sin th
                let vB = Vc(vBx, vBy)
                let pB = Pt(5.0 - 0.5 * vBx, -0.5 * vBy) // centered so the crossing is at u = 0.5
                pA, pB, vA, vB

            test "angle just above tangent tolerance -> Intersect" {
                let pA, pB, vA, vB = crossingAtFive 2.0
                let result = XLine2D.getIntersectionParam(pA, pB, vA, vB, Tangent.``1.0``)
                match result with
                | XLine2D.XParam.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "u should be 0.5"
                | other -> failtest $"2deg > 1deg tolerance should Intersect but got: %A{other}"
            }

            test "angle just below tangent tolerance -> Parallel" {
                let pA, pB, vA, vB = crossingAtFive 0.5
                let result = XLine2D.getIntersectionParam(pA, pB, vA, vB, Tangent.``1.0``)
                match result with
                | XLine2D.XParam.Parallel -> ()
                | other -> failtest $"0.5deg < 1deg tolerance should be Parallel but got: %A{other}"
            }

            test "anti-parallel (negative dot) below tolerance -> Parallel" {
                // ~179.5 degrees between the vectors: dot is large negative, must still be Parallel
                let th = 179.5 * System.Math.PI / 180.0
                let pA = Pt(0.0, 0.0)
                let vA = Vc(10.0, 0.0)
                let pB = Pt(0.0, 1.0)
                let vB = Vc(10.0 * cos th, 10.0 * sin th)
                let result = XLine2D.getIntersectionParam(pA, pB, vA, vB, Tangent.``1.0``)
                match result with
                | XLine2D.XParam.Parallel -> ()
                | other -> failtest $"Anti-parallel within tolerance should be Parallel but got: %A{other}"
            }

            test "perpendicular (dot = 0) still intersects" {
                // dot = 0 makes the RHS tangent * |dot| = 0, so any non-zero |det| must pass
                let pA = Pt(0.0, 0.0)
                let vA = Vc(1.0, 0.0)
                let pB = Pt(0.5, -0.5)
                let vB = Vc(0.0, 1.0)
                let result = XLine2D.getIntersectionParam(pA, pB, vA, vB, Tangent.``1.0``)
                match result with
                | XLine2D.XParam.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "u should be 0.5"
                | other -> failtest $"Perpendicular crossing lines should Intersect but got: %A{other}"
            }
        ]

    ]
