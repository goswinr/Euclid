module TestPoints

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPt a b = Pt.distance a b < 1e-9
let inline eqPnt a b = Pnt.distance a b < 1e-9

let tol = Accuracy.veryHigh

let tests =
    testList "Points2D and Points3D" [

        testList "Points2D" [
            test "closestPointIdx finds closest point" {
                let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(2,0)])
                let target = Pt(0.9, 0.1)
                let idx = Points2D.closestPointIdx(pts, target)
                "closest point is at index 1" |> Expect.equal idx 1
            }

            test "closestPoint returns correct point" {
                let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(2,0)])
                let target = Pt(0.9, 0.1)
                let closest = Points2D.closestPoint(pts, target)
                "closest point is (1,0)" |> Expect.isTrue (eqPt closest (Pt(1,0)))
            }

            test "closestPointIdx on empty list throws" {
                let pts = ResizeArray<Pt>([])
                let target = Pt(0,0)
                let f() = Points2D.closestPointIdx(pts, target) |> ignore
                "throws on empty list" |> Expect.throws f
            }

            test "closestOfTwo returns closer point" {
                let pt1 = Pt(0,0)
                let pt2 = Pt(10,0)
                let ref = Pt(1,0)
                let closest = Points2D.closestOfTwo pt1 pt2 ref
                "closer point is pt1" |> Expect.isTrue (eqPt closest pt1)
            }

            test "closestOfTwo with equal distances returns first" {
                let pt1 = Pt(1,0)
                let pt2 = Pt(-1,0)
                let ref = Pt(0,0)
                let closest = Points2D.closestOfTwo pt1 pt2 ref
                "equal distance returns first" |> Expect.isTrue (eqPt closest pt1)
            }

            test "closestPointsIdx finds closest pair" {
                let xs = ResizeArray([Pt(0,0); Pt(1,0)])
                let ys = ResizeArray([Pt(10,10); Pt(0.1, 0.1)])
                let (xi, yi) = Points2D.closestPointsIdx(xs, ys)
                "x index is 0" |> Expect.equal xi 0
                "y index is 1" |> Expect.equal yi 1
            }

            test "minDistBetweenPointSets calculates correct distance" {
                let xs = ResizeArray([Pt(0,0); Pt(1,0)])
                let ys = ResizeArray([Pt(0,1); Pt(0,2)])
                let dist = Points2D.minDistBetweenPointSets(xs, ys)
                "min distance is 1.0" |> Expect.floatClose tol dist 1.0
            }

            test "mostDistantPointIdx finds most lonely point" {
                let findFrom = ResizeArray([Pt(0,0); Pt(10,0)])
                let checkAgainst = ResizeArray([Pt(0,1)])
                let (fromIdx, _againstIdx) = Points2D.mostDistantPointIdx(findFrom, checkAgainst)
                "most distant is at index 1" |> Expect.equal fromIdx 1
            }

            test "getSignedArea for CCW square" {
                // Unit square, CCW winding
                let pts = ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1); Pt(0,1); Pt(0,0)])
                let area = Points2D.getSignedArea(pts)
                "CCW square has positive area" |> Expect.isTrue (area > 0.0)
                "area is approximately 1.0" |> Expect.floatClose (Accuracy.medium) (abs area) 1.0
            }

            test "getSignedArea for CW square" {
                // Unit square, CW winding
                let pts = ResizeArray([Pt(0,0); Pt(0,1); Pt(1,1); Pt(1,0); Pt(0,0)])
                let area = Points2D.getSignedArea(pts)
                "CW square has negative area" |> Expect.isTrue (area < 0.0)
                "area magnitude is approximately 1.0" |> Expect.floatClose (Accuracy.medium) (abs area) 1.0
            }

            test "getSignedArea for triangle" {
                // Triangle with base 2, height 1
                let pts = ResizeArray([Pt(0,0); Pt(2,0); Pt(1,1); Pt(0,0)])
                let area = Points2D.getSignedArea(pts)
                "triangle area is approximately 1.0" |> Expect.floatClose (Accuracy.medium) (abs area) 1.0
            }

            test "closestPointIdx with single point" {
                let pts = ResizeArray([Pt(5,5)])
                let target = Pt(10,10)
                let idx = Points2D.closestPointIdx(pts, target)
                "single point returns index 0" |> Expect.equal idx 0
            }

            test "closestPointsIdx on empty xs throws" {
                let xs = ResizeArray<Pt>([])
                let ys = ResizeArray([Pt(0,0)])
                let f() = Points2D.closestPointsIdx(xs, ys) |> ignore
                "throws on empty xs" |> Expect.throws f
            }

            test "closestPointsIdx on empty ys throws" {
                let xs = ResizeArray([Pt(0,0)])
                let ys = ResizeArray<Pt>([])
                let f() = Points2D.closestPointsIdx(xs, ys) |> ignore
                "throws on empty ys" |> Expect.throws f
            }

            test "dist to line is 3.0" {
                let pt = Pt(1,1)
                let lineStart = Pt(4,0)
                let lineEnd = Pt(4,9)
                let ln = Line2D(lineStart, lineEnd)
                let dist = pt.DistanceToLine ln
                "distance is 3.0" |> Expect.floatClose tol dist 3.0
            }
        ]

        testList "Points3D" [
            test "closestPointIdx finds closest point" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(2,0,0)])
                let target = Pnt(0.9, 0.1, 0.1)
                let idx = Points3D.closestPointIdx(pts, target)
                "closest point is at index 1" |> Expect.equal idx 1
            }

            test "closestPoint returns correct point" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(2,0,0)])
                let target = Pnt(0.9, 0.1, 0.1)
                let closest = Points3D.closestPoint(pts, target)
                "closest point is (1,0,0)" |> Expect.isTrue (eqPnt closest (Pnt(1,0,0)))
            }

            test "closestPointIdx on empty list throws" {
                let pts = ResizeArray<Pnt>([])
                let target = Pnt(0,0,0)
                let f() = Points3D.closestPointIdx(pts, target) |> ignore
                "throws on empty list" |> Expect.throws f
            }

            test "closestOfTwo returns closer point" {
                let pt1 = Pnt(0,0,0)
                let pt2 = Pnt(10,0,0)
                let ref = Pnt(1,0,0)
                let closest = Points3D.closestOfTwo pt1 pt2 ref
                "closer point is pt1" |> Expect.isTrue (eqPnt closest pt1)
            }

            test "closestOfTwo in 3D space" {
                let pt1 = Pnt(1,1,1)
                let pt2 = Pnt(-1,-1,-1)
                let ref = Pnt(0,0,0)
                let closest = Points3D.closestOfTwo pt1 pt2 ref
                // Both equidistant, should return first
                "equal distance returns first" |> Expect.isTrue (eqPnt closest pt1)
            }

            test "closestPointsIdx finds closest pair" {
                let xs = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let ys = ResizeArray([Pnt(10,10,10); Pnt(0.1, 0.1, 0.1)])
                let (xi, yi) = Points3D.closestPointsIdx(xs, ys)
                "x index is 0" |> Expect.equal xi 0
                "y index is 1" |> Expect.equal yi 1
            }

            test "minDistBetweenPointSets calculates correct distance" {
                let xs = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let ys = ResizeArray([Pnt(0,0,1); Pnt(0,0,2)])
                let dist = Points3D.minDistBetweenPointSets(xs, ys)
                "min distance is 1.0" |> Expect.floatClose tol dist 1.0
            }

            test "minDistBetweenPointSets with diagonal distance" {
                let xs = ResizeArray([Pnt(0,0,0)])
                let ys = ResizeArray([Pnt(1,1,1)])
                let dist = Points3D.minDistBetweenPointSets(xs, ys)
                "distance is sqrt(3)" |> Expect.floatClose tol dist (sqrt 3.0)
            }

            test "mostDistantPointIdx finds most lonely point" {
                let findFrom = ResizeArray([Pnt(0,0,0); Pnt(10,0,0)])
                let checkAgainst = ResizeArray([Pnt(0,0,1)])
                let (fromIdx, _againstIdx) = Points3D.mostDistantPointIdx(findFrom, checkAgainst)
                "most distant is at index 1" |> Expect.equal fromIdx 1
            }

            test "mostDistantPoint returns correct point" {
                let findFrom = ResizeArray([Pnt(0,0,0); Pnt(10,10,10)])
                let checkAgainst = ResizeArray([Pnt(0,0,0)])
                let pt = Points3D.mostDistantPoint(findFrom, checkAgainst)
                "most distant point is (10,10,10)" |> Expect.isTrue (eqPnt pt (Pnt(10,10,10)))
            }

            test "closestPointIdx with single point" {
                let pts = ResizeArray([Pnt(5,5,5)])
                let target = Pnt(10,10,10)
                let idx = Points3D.closestPointIdx(pts, target)
                "single point returns index 0" |> Expect.equal idx 0
            }

            test "closestPointsIdx on empty xs throws" {
                let xs = ResizeArray<Pnt>([])
                let ys = ResizeArray([Pnt(0,0,0)])
                let f() = Points3D.closestPointsIdx(xs, ys) |> ignore
                "throws on empty xs" |> Expect.throws f
            }

            test "closestPointsIdx on empty ys throws" {
                let xs = ResizeArray([Pnt(0,0,0)])
                let ys = ResizeArray<Pnt>([])
                let f() = Points3D.closestPointsIdx(xs, ys) |> ignore
                "throws on empty ys" |> Expect.throws f
            }

            test "mostDistantPointIdx on empty findFrom throws" {
                let findFrom = ResizeArray<Pnt>([])
                let checkAgainst = ResizeArray([Pnt(0,0,0)])
                let f() = Points3D.mostDistantPointIdx(findFrom, checkAgainst) |> ignore
                "throws on empty findFrom" |> Expect.throws f
            }

            test "mostDistantPointIdx on empty checkAgainst throws" {
                let findFrom = ResizeArray([Pnt(0,0,0)])
                let checkAgainst = ResizeArray<Pnt>([])
                let f() = Points3D.mostDistantPointIdx(findFrom, checkAgainst) |> ignore
                "throws on empty checkAgainst" |> Expect.throws f
            }

            test "closestPointIdx with identical points" {
                let pts = ResizeArray([Pnt(1,1,1); Pnt(1,1,1); Pnt(1,1,1)])
                let target = Pnt(1,1,1)
                let idx = Points3D.closestPointIdx(pts, target)
                "returns first matching point" |> Expect.equal idx 0
            }


            test "dist to line is 3.0" {
                let pt = Pnt(1,1,7)
                let lineStart = Pnt(4,0,7)
                let lineEnd = Pnt(4,9,7)
                let ln = Line3D(lineStart, lineEnd)
                let dist = pt.DistanceToLine ln
                "distance is 3.0" |> Expect.floatClose tol dist 3.0
            }
        ]
    ]
