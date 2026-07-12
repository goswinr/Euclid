module TestXLine3D

open System
open Euclid
open Euclid.UtilEuclid

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Mocha
#else
open Expecto
#endif

let tests =
    testList "XLine3D Tests" [

        testList "parameterA tests" [
            test "Intersecting lines - basic case" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(1.0, 0.0, 0.0)
                let vA = Vec(1.0, 0.0, 0.0)
                let vB = Vec(0.0, 1.0, 0.0)
                let t = XLine3D.parameterA(pA, pB, vA, vB)
                Expect.floatClose Accuracy.high t 1.0 "Parameter should be 1.0"
            }

            test "Intersecting lines - vertical and horizontal" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(5.0, 5.0, 0.0)
                let vA = Vec(0.0, 1.0, 0.0)
                let vB = Vec(1.0, 0.0, 0.0)
                let t = XLine3D.parameterA(pA, pB, vA, vB)
                Expect.floatClose Accuracy.high t 5.0 "Parameter should be 5.0"
            }

            test "Skew lines in 3D" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(1.0, 0.0, 1.0)
                let vA = Vec(1.0, 0.0, 0.0)
                let vB = Vec(0.0, 1.0, 0.0)
                let t = XLine3D.parameterA(pA, pB, vA, vB)
                Expect.floatClose Accuracy.high t 1.0 "Parameter should be 1.0 for closest point"
            }

            test "Parallel lines - returns NaN or Infinity" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(0.0, 1.0, 0.0)
                let vA = Vec(1.0, 0.0, 0.0)
                let vB = Vec(1.0, 0.0, 0.0)
                let t = XLine3D.parameterA(pA, pB, vA, vB)
                Expect.isTrue (Double.IsNaN(t) || Double.IsInfinity(t)) "Should return NaN or Infinity for parallel lines"
            }

            test "Line3D overload - intersecting" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let t = XLine3D.parameterA(lineA, lineB)
                Expect.floatClose Accuracy.high t 0.5 "Parameter should be 0.5"
            }

            test "3D crossing lines" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(0.0, 0.0, 5.0)
                let vA = Vec(0.0, 0.0, 1.0)
                let vB = Vec(1.0, 0.0, 0.0)
                let t = XLine3D.parameterA(pA, pB, vA, vB)
                Expect.floatClose Accuracy.high t 5.0 "Parameter should be 5.0"
            }
        ]

        testList "parameters tests" [
            test "Intersecting lines - basic" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(0.0, 0.0, 0.0)
                let vA = Vec(1.0, 0.0, 0.0)
                let vB = Vec(0.0, 1.0, 0.0)
                let (t, u) = XLine3D.parameters(pA, pB, vA, vB)
                Expect.floatClose Accuracy.high t 0.0 "Parameter t should be 0.0"
                Expect.floatClose Accuracy.high u 0.0 "Parameter u should be 0.0"
            }

            test "Skew lines - closest approach" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(1.0, 1.0, 1.0)
                let vA = Vec(1.0, 0.0, 0.0)
                let vB = Vec(0.0, 1.0, 0.0)
                let (t, u) = XLine3D.parameters(pA, pB, vA, vB)
                Expect.floatClose Accuracy.medium t 1.0 "Parameter t should be 1.0"
                Expect.floatClose Accuracy.medium u -1.0 "Parameter u should be -1.0"
            }

            test "Parallel lines - returns NaN or Infinity" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(0.0, 1.0, 0.0)
                let vA = Vec(1.0, 0.0, 0.0)
                let vB = Vec(1.0, 0.0, 0.0)
                let (t, _) = XLine3D.parameters(pA, pB, vA, vB)
                Expect.isTrue (Double.IsNaN(t) || Double.IsInfinity(t)) "Parallel lines should give NaN or Infinity"
            }

            test "Line3D overload" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let (t, u) = XLine3D.parameters(lineA, lineB)
                Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                Expect.floatClose Accuracy.high u 0.5 "Parameter u should be 0.5"
            }
        ]

        testList "doOverlap tests" [
            test "Parallel overlapping lines on same ray" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 0.0), Pnt(15.0, 0.0, 0.0))
                let result = XLine3D.doOverlap(lineA, lineB)
                Expect.isTrue result "Overlapping coincident lines should return true"
            }

            test "Parallel non-overlapping lines on same ray" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(5.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(10.0, 0.0, 0.0), Pnt(15.0, 0.0, 0.0))
                let result = XLine3D.doOverlap(lineA, lineB)
                Expect.isFalse result "Non-overlapping coincident lines should return false"
            }

            test "Parallel lines touching at end" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(5.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let result = XLine3D.doOverlap(lineA, lineB)
                Expect.isTrue result "Touching lines should return true"
            }

            test "Parallel lines on different rays" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.doOverlap(lineA, lineB)
                Expect.isFalse result "Parallel non-coincident lines should return false"
            }

            test "3D parallel lines on same ray" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(5.0, 5.0, 5.0))
                let lineB = Line3D(Pnt(3.0, 3.0, 3.0), Pnt(8.0, 8.0, 8.0))
                let result = XLine3D.doOverlap(lineA, lineB)
                Expect.isTrue result "Overlapping 3D coincident lines should return true"
            }
        ]

        testList "tryIntersectRay tests" [
            test "Rays intersecting" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.tryIntersectRay(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                    Expect.floatClose Accuracy.high pt.Z 0.0 "Z should be 0.0"
                | ValueNone -> failtest "Should have intersection"
            }

            test "Parallel rays" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.tryIntersectRay(lineA, lineB)
                Expect.isTrue result.IsNone "Parallel lines should return None"
            }

            test "Skew rays - returns closest if within tolerance" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 0.001), Pnt(5.0, 10.0, 0.001))
                let result = XLine3D.tryIntersectRay(lineA, lineB, 1e-5)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.medium pt.X 5.0 "X should be 5.0"
                | ValueNone -> () // acceptable if skew distance exceeds tolerance
            }
        ]

        testList "tryIntersect tests" [
            test "Lines intersecting within segments" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.tryIntersect(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                    Expect.floatClose Accuracy.high pt.Z 0.0 "Z should be 0.0"
                | ValueNone -> failtest "Should have intersection"
            }

            test "Lines not intersecting - parallel" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.tryIntersect(lineA, lineB)
                Expect.isTrue result.IsNone "Parallel lines should return None"
            }

            test "Lines not intersecting - apart" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(2.0, -1.0, 0.0), Pnt(2.0, 1.0, 0.0))
                let result = XLine3D.tryIntersect(lineA, lineB)
                Expect.isTrue result.IsNone "Apart lines should return None"
            }

            test "3D coplanar intersection" {
                let lineA = Line3D(Pnt(0.0, 0.0, 5.0), Pnt(10.0, 0.0, 5.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 5.0), Pnt(5.0, 5.0, 5.0))
                let result = XLine3D.tryIntersect(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                    Expect.floatClose Accuracy.high pt.Z 5.0 "Z should be 5.0"
                | ValueNone -> failtest "Should have intersection"
            }
        ]

        testList "getRayClosestParam tests" [
            test "Rays intersecting" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.getRayClosestParam(lineA, lineB)
                match result with
                | XLine3D.XRayParam.SkewOrX (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "Parameter u should be 0.5"
                | _ -> failtest "Should return SkewOrX"
            }

            test "Parallel rays" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.getRayClosestParam(lineA, lineB)
                match result with
                | XLine3D.XRayParam.Parallel -> ()
                | _ -> failtest "Should return Parallel"
            }

            test "Skew rays in 3D" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 1.0), Pnt(5.0, 10.0, 1.0))
                let result = XLine3D.getRayClosestParam(lineA, lineB)
                match result with
                | XLine3D.XRayParam.SkewOrX (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.0 "Parameter u should be 0.0"
                | _ -> failtest "Should return SkewOrX for skew lines"
            }
        ]

        testList "getRayIntersection tests" [
            test "Rays intersecting at specific point" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.getRayIntersection(lineA, lineB)
                match result with
                | XLine3D.XRay.Intersect pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                    Expect.floatClose Accuracy.high pt.Z 0.0 "Z should be 0.0"
                | _ -> failtest "Should intersect"
            }

            test "Skew rays with small distance" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 0.001), Pnt(5.0, 10.0, 0.001))
                let result = XLine3D.getRayIntersection(lineA, lineB, 1e-5)
                match result with
                | XLine3D.XRay.Intersect pt ->
                    Expect.floatClose Accuracy.medium pt.X 5.0 "X should be around 5.0"
                | XLine3D.XRay.Skew _ -> () // acceptable
                | _ -> failtest "Should be intersect or skew"
            }

            test "Parallel rays" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.getRayIntersection(lineA, lineB)
                match result with
                | XLine3D.XRay.Parallel -> ()
                | _ -> failtest "Should be parallel"
            }
        ]

        testList "getIntersectionParam tests" [
            test "Finite lines intersecting" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.getIntersectionParam(lineA, lineB)
                match result with
                | XLine3D.XParam.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "Parameter u should be 0.5"
                | _ -> failtest "Should intersect"
            }

            test "Finite lines apart" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(2.0, -1.0, 0.0), Pnt(2.0, 1.0, 0.0))
                let result = XLine3D.getIntersectionParam(lineA, lineB)
                match result with
                | XLine3D.XParam.Apart -> ()
                | _ -> failtest "Should be apart"
            }

            test "Finite lines parallel" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.getIntersectionParam(lineA, lineB)
                match result with
                | XLine3D.XParam.Parallel -> ()
                | _ -> failtest "Should be parallel"
            }

            test "Skew lines - closest within segments" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 1.0), Pnt(5.0, 10.0, 1.0))
                let result = XLine3D.getIntersectionParam(lineA, lineB, 2.0)
                match result with
                | XLine3D.XParam.Intersect (t, u) ->
                    // The lines are considered intersecting because skew distance (1.0) is within tolerance (2.0)
                    Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.0 "Parameter u should be 0.0"
                | XLine3D.XParam.Skew (t, u, dist) ->
                    Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.0 "Parameter u should be 0.0"
                    Expect.floatClose Accuracy.high dist 1.0 "Squared distance should be 1.0"
                | _ -> failtest "Should be skew or intersect"
            }
        ]

        testList "getIntersection tests" [
            test "Finite lines intersecting - returns point" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.getIntersection(lineA, lineB)
                match result with
                | XLine3D.XPnt.Intersect pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                    Expect.floatClose Accuracy.high pt.Z 0.0 "Z should be 0.0"
                | _ -> failtest "Should intersect"
            }

            test "Finite lines apart" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(2.0, -1.0, 0.0), Pnt(2.0, 1.0, 0.0))
                let result = XLine3D.getIntersection(lineA, lineB)
                match result with
                | XLine3D.XPnt.Apart -> ()
                | _ -> failtest "Should be apart"
            }

            test "Skew lines - returns closest points" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 1.0), Pnt(5.0, 10.0, 1.0))
                let result = XLine3D.getIntersection(lineA, lineB, 2.0)
                match result with
                | XLine3D.XPnt.Intersect pt ->
                    // The lines are considered intersecting because skew distance (1.0) is within tolerance (2.0)
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be around 5.0"
                | XLine3D.XPnt.Skew (ptA, ptB, dist) ->
                    Expect.floatClose Accuracy.high ptA.X 5.0 "ptA X should be 5.0"
                    Expect.floatClose Accuracy.high ptB.Z 1.0 "ptB Z should be 1.0"
                    Expect.floatClose Accuracy.high dist 1.0 "Squared distance should be 1.0"
                | _ -> failtest "Should be skew or intersect"
            }
        ]


        testList "getClosestParameters tests" [
            test "Intersecting lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.getClosestParameters(lineA, lineB)
                match result with
                | XLine3D.ClParams.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "Parameter u should be 0.5"
                | _ -> failtest "Should intersect"
            }

            test "Apart lines - closest points at endpoints" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(1.0, 2.0, 0.0), Pnt(3.0, 3.0, 0.0))
                let result = XLine3D.getClosestParameters(lineA, lineB)
                match result with
                | XLine3D.ClParams.Apart (t, _u, sqdist) ->
                    let dist = Math.Sqrt(sqdist)
                    Expect.floatClose Accuracy.high t 1.0 "Parameter t should be 1.0"
                    Expect.floatClose Accuracy.high dist 2.0 "Distance should be 2.0"
                | _ -> failtest "Should be apart"
            }

            test "Parallel lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.getClosestParameters(lineA, lineB)
                match result with
                | XLine3D.ClParams.Parallel (t, u) ->
                    Expect.floatClose Accuracy.medium t 0.5 "Parameter t should be around 0.5"
                    Expect.floatClose Accuracy.medium u 0.5 "Parameter u should be around 0.5"
                | _ -> failtest "Should be parallel"
            }

            test "Skew lines in 3D" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 1.0), Pnt(5.0, 10.0, 1.0))
                let result = XLine3D.getClosestParameters(lineA, lineB)
                match result with
                | XLine3D.ClParams.Skew (t, u, dist) ->
                    Expect.floatClose Accuracy.high t 0.5 "Parameter t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.0 "Parameter u should be 0.0"
                    Expect.floatClose Accuracy.high dist 1.0 "Squared distance should be 1.0"
                | _ -> failtest "Should be skew"
            }
        ]

        testList "getClosestPoints tests" [
            test "Intersecting lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.getClosestPoints(lineA, lineB)
                match result with
                | XLine3D.ClPts.Intersect pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                    Expect.floatClose Accuracy.high pt.Z 0.0 "Z should be 0.0"
                | _ -> failtest "Should intersect"
            }

            test "Parallel lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.getClosestPoints(lineA, lineB)
                match result with
                | XLine3D.ClPts.Parallel (ptA, ptB) ->
                    let d = Pnt.dist ptA ptB
                    Expect.floatClose Accuracy.medium d 1.0 "Distance should be 1.0"
                | _ -> failtest "Should be parallel"
            }

            test "Skew lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 1.0), Pnt(5.0, 10.0, 1.0))
                let result = XLine3D.getClosestPoints(lineA, lineB)
                match result with
                | XLine3D.ClPts.Skew (ptA, ptB, sqdist) ->
                    Expect.floatClose Accuracy.high ptA.X 5.0 "ptA X should be 5.0"
                    Expect.floatClose Accuracy.high ptB.X 5.0 "ptB X should be 5.0"
                    Expect.floatClose Accuracy.high sqdist 1.0 "Squared distance should be 1.0"
                | _ -> failtest "Should be skew"
            }
        ]

        testList "getSqDistance tests" [
            test "Intersecting lines have zero distance" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let dist = XLine3D.getSqDistance(lineA, lineB)
                Expect.floatClose Accuracy.high dist 0.0 "Distance should be 0.0"
            }

            test "Apart lines have positive distance" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(2.0, 1.0, 0.0), Pnt(3.0, 1.0, 0.0))
                let dist = XLine3D.getSqDistance(lineA, lineB)
                Expect.isTrue (dist > 0.0) "Distance should be positive"
                Expect.floatClose Accuracy.high dist 2.0 "Squared distance should be 2.0"
            }

            test "Parallel lines distance" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let dist = XLine3D.getSqDistance(lineA, lineB)
                Expect.floatClose Accuracy.high dist 1.0 "Squared distance should be 1.0"
            }

            test "Skew lines distance" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 2.0), Pnt(5.0, 10.0, 2.0))
                let dist = XLine3D.getSqDistance(lineA, lineB)
                Expect.floatClose Accuracy.high dist 4.0 "Squared distance should be 4.0"
            }
        ]



        testList "getEndsTouching tests" [
            test "Not touching" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(2.0, 0.0, 0.0), Pnt(3.0, 0.0, 0.0))
                let result = XLine3D.getEndsTouching(lineA, lineB)
                match result with
                | XLine3D.XEnds.NotTouching -> ()
                | _ -> failtest "Should not be touching"
            }

            test "StartA_StartB touching" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(0.0, 1.0, 0.0))
                let result = XLine3D.getEndsTouching(lineA, lineB)
                match result with
                | XLine3D.XEnds.StartA_StartB -> ()
                | _ -> failtest "Should touch at StartA_StartB"
            }

            test "EndA_EndB touching" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let result = XLine3D.getEndsTouching(lineA, lineB)
                match result with
                | XLine3D.XEnds.EndA_EndB -> ()
                | _ -> failtest "Should touch at EndA_EndB"
            }

            test "EndA_StartB touching" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(1.0, 0.0, 0.0), Pnt(2.0, 0.0, 0.0))
                let result = XLine3D.getEndsTouching(lineA, lineB)
                match result with
                | XLine3D.XEnds.EndA_StartB -> ()
                | _ -> failtest "Should touch at EndA_StartB"
            }

            test "StartA_EndB touching" {
                let lineA = Line3D(Pnt(1.0, 0.0, 0.0), Pnt(2.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let result = XLine3D.getEndsTouching(lineA, lineB)
                match result with
                | XLine3D.XEnds.StartA_EndB -> ()
                | _ -> failtest "Should touch at StartA_EndB"
            }

            test "Identical lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let result = XLine3D.getEndsTouching(lineA, lineB)
                match result with
                | XLine3D.XEnds.Identical -> ()
                | _ -> failtest "Should be identical"
            }

            test "Identical flipped lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(1.0, 0.0, 0.0), Pnt(0.0, 0.0, 0.0))
                let result = XLine3D.getEndsTouching(lineA, lineB)
                match result with
                | XLine3D.XEnds.IdenticalFlipped -> ()
                | _ -> failtest "Should be identical flipped"
            }

            test "3D lines touching at end" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 1.0, 1.0))
                let lineB = Line3D(Pnt(1.0, 1.0, 1.0), Pnt(2.0, 2.0, 2.0))
                let result = XLine3D.getEndsTouching(lineA, lineB)
                match result with
                | XLine3D.XEnds.EndA_StartB -> ()
                | _ -> failtest "Should touch at EndA_StartB"
            }
        ]

        testList "Edge cases and numerical stability" [
            test "Very small lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1e-8, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(0.0, 1e-8, 0.0))
                let result = XLine3D.getIntersection(lineA, lineB)
                match result with
                | XLine3D.XPnt.TooShortBoth -> ()
                | _ -> failtest "Should be too short both"
            }

            test "Very large coordinates" {
                let lineA = Line3D(Pnt(1e10, 0.0, 0.0), Pnt(1e10 + 10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(1e10 + 5.0, -5.0, 0.0), Pnt(1e10 + 5.0, 5.0, 0.0))
                let result = XLine3D.tryIntersect(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.medium pt.X (1e10 + 5.0) "X should be 1e10 + 5.0"
                    Expect.floatClose Accuracy.medium pt.Y 0.0 "Y should be 0.0"
                | ValueNone -> failtest "Should intersect"
            }

            test "Diagonal lines at 45 degrees in 3D" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 10.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 10.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let result = XLine3D.tryIntersect(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 5.0 "Y should be 5.0"
                    Expect.floatClose Accuracy.high pt.Z 0.0 "Z should be 0.0"
                | ValueNone -> failtest "Should intersect"
            }

            test "Lines with negative coordinates" {
                let lineA = Line3D(Pnt(-10.0, -10.0, -10.0), Pnt(10.0, -10.0, -10.0))
                let lineB = Line3D(Pnt(0.0, -20.0, -10.0), Pnt(0.0, 0.0, -10.0))
                let result = XLine3D.tryIntersect(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 0.0 "X should be 0.0"
                    Expect.floatClose Accuracy.high pt.Y -10.0 "Y should be -10.0"
                    Expect.floatClose Accuracy.high pt.Z -10.0 "Z should be -10.0"
                | ValueNone -> failtest "Should intersect"
            }

            test "True 3D skew lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 10.0, 10.0), Pnt(10.0, 10.0, 10.0))
                let sqDist = XLine3D.getSqDistance(lineA, lineB)
                Expect.floatClose Accuracy.high sqDist 200.0 "Squared distance should be 200.0 (10^2 + 10^2)"
            }

            test "Nearly coplanar skew lines" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 0.001), Pnt(5.0, 10.0, 0.001))
                let result = XLine3D.getIntersection(lineA, lineB, 1e-5)
                match result with
                | XLine3D.XPnt.Intersect _
                | XLine3D.XPnt.Skew _ -> () // both acceptable
                | _ -> failtest "Should be intersect or skew"
            }
        ]

        testList "doRaysIntersect tests" [
            test "Rays intersecting within tolerance" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.doRaysIntersect(lineA, lineB)
                Expect.isTrue result "Rays should intersect"
            }

            test "Rays parallel - not intersecting" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.doRaysIntersect(lineA, lineB)
                Expect.isFalse result "Parallel rays should not intersect"
            }

            test "Skew rays - distance exceeds tolerance" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 10.0), Pnt(5.0, 10.0, 10.0))
                let result = XLine3D.doRaysIntersect(lineA, lineB, 1e-6)
                Expect.isFalse result "Skew rays with large distance should not intersect"
            }

            test "Skew rays - within tolerance" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 0.001), Pnt(5.0, 10.0, 0.001))
                let result = XLine3D.doRaysIntersect(lineA, lineB, 0.01)
                Expect.isTrue result "Skew rays within tolerance should be considered intersecting"
            }

            test "Zero length line" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(0.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let result = XLine3D.doRaysIntersect(lineA, lineB)
                Expect.isFalse result "Zero length line should not intersect"
            }

            test "Pnt/Vec overload" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(5.0, -5.0, 0.0)
                let vA = Vec(10.0, 0.0, 0.0)
                let vB = Vec(0.0, 10.0, 0.0)
                let result = XLine3D.doRaysIntersect(pA, pB, vA, vB)
                Expect.isTrue result "Rays should intersect"
            }

            test "Float components overload" {
                let result = XLineXYZ.doRaysIntersect(0.0, 0.0, 0.0, 5.0, -5.0, 0.0, 10.0, 0.0, 0.0, 0.0, 10.0, 0.0, 1e-6)
                Expect.isTrue result "Rays should intersect"
            }
        ]

        testList "tryClosestParameterRayA tests" [
            test "Intersecting rays - parameter in range" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.tryClosestParameterRayA(lineA, lineB)
                match result with
                | ValueSome t -> Expect.floatClose Accuracy.high t 0.5 "Parameter should be 0.5"
                | ValueNone -> failtest "Should return a parameter"
            }

            test "Parallel rays - returns None" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.tryClosestParameterRayA(lineA, lineB)
                Expect.isTrue result.IsNone "Parallel rays should return None"
            }

            test "Skew rays - returns parameter" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 1.0), Pnt(5.0, 10.0, 1.0))
                let result = XLine3D.tryClosestParameterRayA(lineA, lineB)
                match result with
                | ValueSome t -> Expect.floatClose Accuracy.high t 0.5 "Parameter should be 0.5"
                | ValueNone -> failtest "Should return a parameter"
            }

            test "Parameter too large - returns None" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(1.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(1e15, -5.0, 0.0), Pnt(1e15, 5.0, 0.0))
                let result = XLine3D.tryClosestParameterRayA(lineA, lineB)
                Expect.isTrue result.IsNone "Parameter too large should return None"
            }

            test "Pnt/Vec overload" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(5.0, -5.0, 0.0)
                let vA = Vec(10.0, 0.0, 0.0)
                let vB = Vec(0.0, 10.0, 0.0)
                let result = XLine3D.tryClosestParameterRayA(pA, pB, vA, vB)
                match result with
                | ValueSome t -> Expect.floatClose Accuracy.high t 0.5 "Parameter should be 0.5"
                | ValueNone -> failtest "Should return a parameter"
            }

            test "Float components overload" {
                let result = XLineXYZ.tryClosestParameterRayA(0.0, 0.0, 0.0, 5.0, -5.0, 0.0, 10.0, 0.0, 0.0, 0.0, 10.0, 0.0)
                match result with
                | ValueSome t -> Expect.floatClose Accuracy.high t 0.5 "Parameter should be 0.5"
                | ValueNone -> failtest "Should return a parameter"
            }
        ]

        testList "tryClosestPntRayA tests" [
            test "Intersecting rays - returns point" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, -5.0, 0.0), Pnt(5.0, 5.0, 0.0))
                let result = XLine3D.tryClosestPntRayA(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                    Expect.floatClose Accuracy.high pt.Z 0.0 "Z should be 0.0"
                | ValueNone -> failtest "Should return a point"
            }

            test "Parallel rays - returns None" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(0.0, 1.0, 0.0), Pnt(10.0, 1.0, 0.0))
                let result = XLine3D.tryClosestPntRayA(lineA, lineB)
                Expect.isTrue result.IsNone "Parallel rays should return None"
            }

            test "Skew rays - returns closest point on A" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(10.0, 0.0, 0.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 1.0), Pnt(5.0, 10.0, 1.0))
                let result = XLine3D.tryClosestPntRayA(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                    Expect.floatClose Accuracy.high pt.Z 0.0 "Z should be 0.0"
                | ValueNone -> failtest "Should return a point"
            }

            test "3D rays - returns point" {
                let lineA = Line3D(Pnt(0.0, 0.0, 0.0), Pnt(0.0, 0.0, 10.0))
                let lineB = Line3D(Pnt(5.0, 0.0, 5.0), Pnt(-5.0, 0.0, 5.0))
                let result = XLine3D.tryClosestPntRayA(lineA, lineB)
                match result with
                | ValueSome pt ->
                    Expect.floatClose Accuracy.high pt.X 0.0 "X should be 0.0"
                    Expect.floatClose Accuracy.high pt.Y 0.0 "Y should be 0.0"
                    Expect.floatClose Accuracy.high pt.Z 5.0 "Z should be 5.0"
                | ValueNone -> failtest "Should return a point"
            }

            test "Pnt/Vec overload" {
                let pA = Pnt(0.0, 0.0, 0.0)
                let pB = Pnt(5.0, -5.0, 0.0)
                let vA = Vec(10.0, 0.0, 0.0)
                let vB = Vec(0.0, 10.0, 0.0)
                let result = XLine3D.tryClosestPntRayA(pA, pB, vA, vB)
                match result with
                | ValueSome pt -> Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                | ValueNone -> failtest "Should return a point"
            }

            test "Float components overload" {
                let result = XLineXYZ.tryClosestPntRayA(0.0, 0.0, 0.0, 5.0, -5.0, 0.0, 10.0, 0.0, 0.0, 0.0, 10.0, 0.0)
                match result with
                | ValueSome pt -> Expect.floatClose Accuracy.high pt.X 5.0 "X should be 5.0"
                | ValueNone -> failtest "Should return a point"
            }
        ]

        // These tests target the sqrt-free / Lagrange-identity intersection math:
        // the parallel test is crossMagSq > tangent² * dot² and the t,u numerators
        // come from the Lagrange identity instead of two cross products.
        testList "near-parallel and Lagrange numerator tests" [

            // helper: two coplanar (z=0) lines crossing at (5,0,0), lineB rotated by 'deg' from the X-axis
            let crossingAtFive (deg:float) =
                let th = deg * System.Math.PI / 180.0
                let vA = Vec(10.0, 0.0, 0.0)
                let pA = Pnt(0.0, 0.0, 0.0)
                let vBx = 10.0 * cos th
                let vBy = 10.0 * sin th
                let vB = Vec(vBx, vBy, 0.0)
                let pB = Pnt(5.0 - 0.5 * vBx, -0.5 * vBy, 0.0) // centered so the crossing is at u = 0.5
                pA, pB, vA, vB

            test "angle just above tangent tolerance -> Intersect" {
                // 2 degrees apart, tolerance 1 degree: the squared tangent test must enter the branch
                let pA, pB, vA, vB = crossingAtFive 2.0
                let result = XLine3D.getIntersectionParam(pA, pB, vA, vB, 1e-6, Tangent.``1.0``, 1e-6)
                match result with
                | XLine3D.XParam.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "u should be 0.5"
                | _ -> failtest "2deg > 1deg tolerance should classify as Intersect, not Parallel"
            }

            test "angle just below tangent tolerance -> Parallel" {
                // 0.5 degrees apart, tolerance 1 degree: the squared tangent test must reject as parallel
                let pA, pB, vA, vB = crossingAtFive 0.5
                let result = XLine3D.getIntersectionParam(pA, pB, vA, vB, 1e-6, Tangent.``1.0``, 1e-6)
                match result with
                | XLine3D.XParam.Parallel -> ()
                | _ -> failtest "0.5deg < 1deg tolerance should classify as Parallel"
            }

            test "anti-parallel (negative dot) below tolerance -> Parallel" {
                // ~179.5 degrees: dot is large negative, so the dot*dot form must still treat it as parallel
                let vA = Vec(10.0, 0.0, 0.0)
                let pA = Pnt(0.0, 0.0, 0.0)
                let th = 179.5 * System.Math.PI / 180.0
                let vB = Vec(10.0 * cos th, 10.0 * sin th, 0.0)
                let pB = Pnt(0.0, 1.0, 0.0)
                let result = XLine3D.getIntersectionParam(pA, pB, vA, vB, 1e-6, Tangent.``1.0``, 1e-6)
                match result with
                | XLine3D.XParam.Parallel -> ()
                | _ -> failtest "Anti-parallel within tolerance should be Parallel (negative dot handled)"
            }

            test "perpendicular (dot = 0) still intersects" {
                // dot = 0 makes the RHS tangent² * dot² = 0, so any non-zero crossMagSq must pass the test
                let pA = Pnt(0.0, 0.0, 0.0)
                let vA = Vec(1.0, 0.0, 0.0)
                let pB = Pnt(0.5, -0.5, 0.0)
                let vB = Vec(0.0, 1.0, 0.0)
                let result = XLine3D.getIntersectionParam(pA, pB, vA, vB, 1e-6, Tangent.``1.0``, 1e-6)
                match result with
                | XLine3D.XParam.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "u should be 0.5"
                | _ -> failtest "Perpendicular crossing lines should Intersect"
            }

            test "oblique (non-axis-aligned) numerators - coplanar Intersect" {
                // vectors not aligned with any axis exercise the general Lagrange numerators
                let pA = Pnt(0.0, 0.0, 0.0)
                let vA = Vec(4.0, 2.0, 0.0)
                let pB = Pnt(0.0, 2.0, 0.0)
                let vB = Vec(4.0, -2.0, 0.0)
                let result = XLine3D.getIntersectionParam(pA, pB, vA, vB, 1e-6, Tangent.``0.25``, 1e-6)
                match result with
                | XLine3D.XParam.Intersect (t, u) ->
                    Expect.floatClose Accuracy.high t 0.5 "t should be 0.5 at (2,1,0)"
                    Expect.floatClose Accuracy.high u 0.5 "u should be 0.5 at (2,1,0)"
                | _ -> failtest "Oblique coplanar lines should Intersect at (2,1,0)"
            }

            test "oblique skew numerators - Skew params and squared distance" {
                // same oblique crossing but lineB lifted by 3 in Z: closest approach at t=u=0.5, sqDist=9
                let pA = Pnt(0.0, 0.0, 0.0)
                let vA = Vec(4.0, 2.0, 0.0)
                let pB = Pnt(0.0, 2.0, 3.0)
                let vB = Vec(4.0, -2.0, 0.0)
                let result = XLine3D.getIntersectionParam(pA, pB, vA, vB, 2.0, Tangent.``0.25``, 1e-6)
                match result with
                | XLine3D.XParam.Skew (t, u, sqDist) ->
                    Expect.floatClose Accuracy.high t 0.5 "t should be 0.5"
                    Expect.floatClose Accuracy.high u 0.5 "u should be 0.5"
                    Expect.floatClose Accuracy.high sqDist 9.0 "squared distance should be 9.0"
                | _ -> failtest "Lifted oblique lines should be Skew with sqDist 9.0"
            }
        ]

    ]
