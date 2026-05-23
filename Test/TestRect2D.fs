module TestRect2D

open Euclid

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pt.dist a b < 1e-9
let inline eqf (a:float) (b:float) = abs(a-b) < 1e-9
let inline eqv (a:Vc) (b:Vc) = Vc.length (a-b) < 1e-9
let inline eqLine (ln:Line2D) (a:Pt) (b:Pt) = eq ln.From a && eq ln.To b

let o = Pt.Origin
let x = Vc.Xaxis
let y = Vc.Yaxis
let rect = Rect2D.createFromVectors(o,x,y)

// A canonical test rectangle that is NOT at the world origin and IS rotated.
// Origin (5,3); X-axis (8,6) of length 10; Y-axis (-3,4) of length 5; rotation ~36.87 degrees.
// The (8,6)/(-3,4) pair is exactly perpendicular and counter-clockwise, with exact corner coordinates:
//   p0 = (5,3)   p1 = (13,9)   p2 = (10,13)   p3 = (2,7)   center = (7.5,8)
let ro = Pt(5., 3.)
let rvx = Vc(8., 6.)   // length 10
let rvy = Vc(-3., 4.)  // length 5
let rr = Rect2D.createFromVectors(ro, rvx, rvy)
let p0 = Pt(5., 3.)
let p1 = Pt(13., 9.)
let p2 = Pt(10., 13.)
let p3 = Pt(2., 7.)
let rCenter = Pt(7.5, 8.)

let tests =
    testList "Rect2D" [

        test "Rect2D.grid" {
            let grid = Rect2D.grid (rect, 2, 2)
            "2-Rect2D.grid: array outer length" |> Expect.equal grid.Length 2
            "2-Rect2D.grid: array inner length" |> Expect.equal grid[0].Length 2
            "2-Corner1,1" |> Expect.isTrue (eq grid.[1].[1] (Pt(1,1)))
            "2-Corner0,0" |> Expect.isTrue (eq grid.[0].[0] (Pt(0,0)))

            let grid = Rect2D.grid (rect, 3, 3)
            "Rect2D.grid: array outer length" |> Expect.equal grid.Length 3
            "Rect2D.grid: array inner length" |> Expect.equal grid[0].Length 3
            "3-Corner0,0" |> Expect.isTrue (eq grid.[0].[0] (Pt(0,0)))
            "3-Corner1,1" |> Expect.isTrue (eq grid.[1].[1] (Pt(0.5,0.5)))
            "3-Corner2,2" |> Expect.isTrue (eq grid.[2].[2] (Pt(1,1)))
            "3-Corner0,0" |> Expect.isTrue (eq grid.[0].[2] (Pt(0,1)))


            let grid = Rect2D.gridMaxLength (rect, 0.4, 0.4)
            "Rect2D.gridMaxLength: 0.4 -> 4 points" |> Expect.equal grid.Length 4
            let grid = Rect2D.gridMaxLength (rect, 0.5, 0.5)
            "Rect2D.gridMaxLength: 0.5 -> 3 points" |> Expect.equal grid.Length 3
            let grid = Rect2D.gridMaxLength (rect, 0.6, 0.6)
            "Rect2D.gridMaxLength: 0.6 -> 3 points" |> Expect.equal grid.Length 3

            let grid = Rect2D.gridMinLength (rect, 0.4, 0.4)
            "Rect2D.gridMinLength: 1/0.4 -> 3 points" |> Expect.equal grid.Length 3
            let grid = Rect2D.gridMinLength (rect, 0.5, 0.5)
            "Rect2D.gridMinLength: 1/0.5 -> 3 points" |> Expect.equal grid.Length 3
            let grid = Rect2D.gridMinLength (rect, 0.6, 0.6)
            "Rect2D.gridMinLength: 1/0.6 -> 2 points" |> Expect.equal grid.Length 2

            let grid = Rect2D.subDivideMaxLength (rect, 0.4, 0.4, 0., 0.)
            "Rect2D.subDivideMaxLength: 0.4 -> 3 rects" |> Expect.equal grid.Length 3
            let grid = Rect2D.subDivideMaxLength (rect, 0.5, 0.5, 0., 0.)
            "Rect2D.subDivideMaxLength: 0.5 -> 2 rects" |> Expect.equal grid.Length 2
            let grid = Rect2D.subDivideMaxLength (rect, 0.6, 0.6, 0., 0.)
            "Rect2D.subDivideMaxLength: 0.6 -> 2 rects" |> Expect.equal grid.Length 2

            let grid = Rect2D.subDivideMinLength (rect, 0.4, 0.4, 0., 0.)
            "Rect2D.subDivideMinLength: 1/0.4 -> 2 rects" |> Expect.equal grid.Length 2
            let grid = Rect2D.subDivideMinLength (rect, 0.5, 0.5, 0., 0.)
            "Rect2D.subDivideMinLength: 1/0.5 -> 2 rects" |> Expect.equal grid.Length 2
            let grid = Rect2D.subDivideMinLength (rect, 0.6, 0.6, 0., 0.)
            "Rect2D.subDivideMinLength: 1/0.6 -> 1 rects" |> Expect.equal grid.Length 1


        }

        // ---------------------------------------------------------------------
        // Sizes, area and diagonal of a rotated, off-origin rectangle
        // ---------------------------------------------------------------------
        testList "Sizes and area" [
            test "SizeX / SizeY" {
                "SizeX"      |> Expect.isTrue (eqf rr.SizeX 10.)
                "SizeY"      |> Expect.isTrue (eqf rr.SizeY 5.)
                "sizeX stat" |> Expect.isTrue (eqf (Rect2D.sizeX rr) 10.)
                "sizeY stat" |> Expect.isTrue (eqf (Rect2D.sizeY rr) 5.)
            }
            test "SizeXSq / SizeYSq" {
                "SizeXSq" |> Expect.isTrue (eqf rr.SizeXSq 100.)
                "SizeYSq" |> Expect.isTrue (eqf rr.SizeYSq 25.)
            }
            test "Area" {
                "Area"      |> Expect.isTrue (eqf rr.Area 50.)
                "area stat" |> Expect.isTrue (eqf (Rect2D.area rr) 50.)
            }
            test "Longest / Shortest edge" {
                "LongestEdge"    |> Expect.isTrue (eqf rr.LongestEdge 10.)
                "ShortestEdge"   |> Expect.isTrue (eqf rr.ShortestEdge 5.)
                "LongestEdgeSq"  |> Expect.isTrue (eqf rr.LongestEdgeSq 100.)
                "ShortestEdgeSq" |> Expect.isTrue (eqf rr.ShortestEdgeSq 25.)
            }
            test "Diagonal vector" {
                "Diagonal" |> Expect.isTrue (eqv rr.Diagonal (Vc(5., 10.)))
            }
            test "Center" {
                "Center"      |> Expect.isTrue (eq rr.Center rCenter)
                "center stat" |> Expect.isTrue (eq (Rect2D.center rr) rCenter)
            }
            test "Xaxis / Yaxis unit vectors" {
                let xu = rr.XaxisUnit
                let yu = rr.YaxisUnit
                "XaxisUnit X" |> Expect.isTrue (eqf xu.X 0.8)
                "XaxisUnit Y" |> Expect.isTrue (eqf xu.Y 0.6)
                "YaxisUnit X" |> Expect.isTrue (eqf yu.X -0.6)
                "YaxisUnit Y" |> Expect.isTrue (eqf yu.Y 0.8)
            }
        ]

        // ---------------------------------------------------------------------
        // Corner points
        // ---------------------------------------------------------------------
        testList "Corners and points" [
            test "Pt0..Pt3" {
                "Pt0" |> Expect.isTrue (eq rr.Pt0 p0)
                "Pt1" |> Expect.isTrue (eq rr.Pt1 p1)
                "Pt2" |> Expect.isTrue (eq rr.Pt2 p2)
                "Pt3" |> Expect.isTrue (eq rr.Pt3 p3)
            }
            test "Named corners" {
                "Origin"    |> Expect.isTrue (eq rr.Origin p0)
                "XCorner"   |> Expect.isTrue (eq rr.XCorner p1)
                "FarCorner" |> Expect.isTrue (eq rr.FarCorner p2)
                "YCorner"   |> Expect.isTrue (eq rr.YCorner p3)
            }
            test "Points array" {
                let pts = rr.Points
                "Points length" |> Expect.equal pts.Length 4
                "Points 0" |> Expect.isTrue (eq pts.[0] p0)
                "Points 1" |> Expect.isTrue (eq pts.[1] p1)
                "Points 2" |> Expect.isTrue (eq pts.[2] p2)
                "Points 3" |> Expect.isTrue (eq pts.[3] p3)
            }
            test "PointsLooped array" {
                let pts = rr.PointsLooped
                "PointsLooped length" |> Expect.equal pts.Length 5
                "PointsLooped first=last" |> Expect.isTrue (eq pts.[0] pts.[4])
                "PointsLooped 0" |> Expect.isTrue (eq pts.[0] p0)
            }
        ]

        // ---------------------------------------------------------------------
        // Edges
        // ---------------------------------------------------------------------
        testList "Edges" [
            test "Edge01..Edge30" {
                "Edge01" |> Expect.isTrue (eqLine rr.Edge01 p0 p1)
                "Edge12" |> Expect.isTrue (eqLine rr.Edge12 p1 p2)
                "Edge23" |> Expect.isTrue (eqLine rr.Edge23 p2 p3)
                "Edge30" |> Expect.isTrue (eqLine rr.Edge30 p3 p0)
            }
            test "EdgeX / EdgeY" {
                "EdgeX is 0->1" |> Expect.isTrue (eqLine rr.EdgeX p0 p1)
                "EdgeY is 0->3" |> Expect.isTrue (eqLine rr.EdgeY p0 p3)
            }
            test "DiagonalLine 0->2" {
                "DiagonalLine" |> Expect.isTrue (eqLine rr.DiagonalLine p0 p2)
            }
            test "GetEdge 0..3" {
                "GetEdge 0" |> Expect.isTrue (eqLine (rr.GetEdge 0) p0 p1)
                "GetEdge 1" |> Expect.isTrue (eqLine (rr.GetEdge 1) p1 p2)
                "GetEdge 2" |> Expect.isTrue (eqLine (rr.GetEdge 2) p2 p3)
                "GetEdge 3" |> Expect.isTrue (eqLine (rr.GetEdge 3) p3 p0)
            }
            test "GetEdge out of range throws" {
                "GetEdge 4" |> Expect.throws (fun () -> rr.GetEdge 4 |> ignore)
            }
            test "Edges array" {
                let e = rr.Edges
                "Edges length" |> Expect.equal e.Length 4
                "Edges 0" |> Expect.isTrue (eqLine e.[0] p0 p1)
                "Edges 1" |> Expect.isTrue (eqLine e.[1] p1 p2)
                "Edges 2" |> Expect.isTrue (eqLine e.[2] p2 p3)
                "Edges 3" |> Expect.isTrue (eqLine e.[3] p3 p0)
            }
        ]

        // ---------------------------------------------------------------------
        // Evaluation
        // ---------------------------------------------------------------------
        testList "Evaluation" [
            test "EvaluateAt corners and center" {
                "EvaluateAt 0,0" |> Expect.isTrue (eq (rr.EvaluateAt(0., 0.)) p0)
                "EvaluateAt 1,0" |> Expect.isTrue (eq (rr.EvaluateAt(1., 0.)) p1)
                "EvaluateAt 1,1" |> Expect.isTrue (eq (rr.EvaluateAt(1., 1.)) p2)
                "EvaluateAt 0,1" |> Expect.isTrue (eq (rr.EvaluateAt(0., 1.)) p3)
                "EvaluateAt 0.5,0.5" |> Expect.isTrue (eq (rr.EvaluateAt(0.5, 0.5)) rCenter)
            }
            test "evaluateAt static" {
                "evaluateAt" |> Expect.isTrue (eq (Rect2D.evaluateAt 1. 1. rr) p2)
            }
            test "EvaluateDist by absolute distance" {
                // full X size, full Y size -> far corner
                "EvaluateDist 10,5" |> Expect.isTrue (eq (rr.EvaluateDist(10., 5.)) p2)
                // half of X size, none of Y -> midpoint of edge 0-1
                "EvaluateDist 5,0" |> Expect.isTrue (eq (rr.EvaluateDist(5., 0.)) (Pt(9., 6.)))
            }
        ]

        // ---------------------------------------------------------------------
        // Contains
        // ---------------------------------------------------------------------
        testList "Contains" [
            test "Center is contained" {
                "center" |> Expect.isTrue (rr.Contains rCenter)
            }
            test "Corners are contained (on edge counts as inside)" {
                "p0" |> Expect.isTrue (rr.Contains p0)
                "p1" |> Expect.isTrue (rr.Contains p1)
                "p2" |> Expect.isTrue (rr.Contains p2)
                "p3" |> Expect.isTrue (rr.Contains p3)
            }
            test "Edge midpoint is contained" {
                "edge 0-1 midpoint" |> Expect.isTrue (rr.Contains (Pt(9., 6.)))
            }
            test "Outside points are not contained" {
                "world origin" |> Expect.isFalse (rr.Contains (Pt(0., 0.)))
                "just outside far corner" |> Expect.isFalse (rr.Contains (Pt(11., 14.)))
            }
            test "contains static" {
                "contains center" |> Expect.isTrue (Rect2D.contains rCenter rr)
            }
        ]

        // ---------------------------------------------------------------------
        // Bounding rectangle
        // ---------------------------------------------------------------------
        testList "BRect" [
            test "Axis-aligned bounds of rotated rect" {
                let b = rr.BRect
                "BRect MinPt" |> Expect.isTrue (eq b.MinPt (Pt(2., 3.)))
                "BRect MaxPt" |> Expect.isTrue (eq b.MaxPt (Pt(13., 13.)))
                "BRect SizeX" |> Expect.isTrue (eqf b.SizeX 11.)
                "BRect SizeY" |> Expect.isTrue (eqf b.SizeY 10.)
            }
        ]

        // ---------------------------------------------------------------------
        // Constructors
        // ---------------------------------------------------------------------
        testList "Constructors" [
            test "createFromVectors rejects non-perpendicular" {
                "non perpendicular" |> Expect.throws (fun () -> Rect2D.createFromVectors(ro, Vc(8., 6.), Vc(1., 1.)) |> ignore)
            }
            test "createFromVectors rejects clockwise order" {
                "clockwise" |> Expect.throws (fun () -> Rect2D.createFromVectors(ro, Vc(8., 6.), Vc(3., -4.)) |> ignore)
            }
            test "createFromVectors rejects zero-length axis" {
                "zero x" |> Expect.throws (fun () -> Rect2D.createFromVectors(ro, Vc(0., 0.), rvy) |> ignore)
            }
            test "createFromXVectorAndWidth uses absolute Y size" {
                let r = Rect2D.createFromXVectorAndWidth(ro, rvx, 5.)
                "SizeX from vector" |> Expect.isTrue (eqf r.SizeX 10.)
                "SizeY absolute"    |> Expect.isTrue (eqf r.SizeY 5.)
                "equals canonical"  |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
            }
            test "createFromCenterAndVector uses absolute Y size and centers" {
                let r = Rect2D.createFromCenterAndVector(rCenter, rvx, 5.)
                "SizeX from vector" |> Expect.isTrue (eqf r.SizeX 10.)
                "SizeY absolute"    |> Expect.isTrue (eqf r.SizeY 5.)
                "center kept"       |> Expect.isTrue (eq r.Center rCenter)
                "equals canonical"  |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
            }
            test "createFromDirectionAndSizes" {
                let dir = (Vc(8., 6.)).Unitized
                let r = Rect2D.createFromDirectionAndSizes(ro, dir, 10., 5.)
                "equals canonical" |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
            }
            test "createFromDirectionAndSizes rejects negative size" {
                let dir = (Vc(8., 6.)).Unitized
                "negative sizeX" |> Expect.throws (fun () -> Rect2D.createFromDirectionAndSizes(ro, dir, -1., 5.) |> ignore)
            }
            test "createFromCenterAndDirection" {
                let dir = (Vc(8., 6.)).Unitized
                let r = Rect2D.createFromCenterAndDirection(rCenter, dir, 10., 5.)
                "equals canonical" |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
            }
            test "createFromLine with right and left offset" {
                let line = Line2D(ro, p1) // vector (8,6), length 10
                let r = Rect2D.createFromLine(line, 1., 4.)
                "SizeX = line length" |> Expect.isTrue (eqf r.SizeX 10.)
                "SizeY = offRight+offLeft" |> Expect.isTrue (eqf r.SizeY 5.)
                "origin shifted to right side" |> Expect.isTrue (eq r.Origin (Pt(5.6, 2.2)))
            }
            test "createFromLine rejects flipped offsets" {
                let line = Line2D(ro, p1)
                "flipped" |> Expect.throws (fun () -> Rect2D.createFromLine(line, -4., 1.) |> ignore)
            }
            test "createFromBRect round-trips an axis-aligned box" {
                let r = Rect2D.createFromBRect rr.BRect
                "origin = min" |> Expect.isTrue (eq r.Origin (Pt(2., 3.)))
                "SizeX" |> Expect.isTrue (eqf r.SizeX 11.)
                "SizeY" |> Expect.isTrue (eqf r.SizeY 10.)
                "X axis aligned" |> Expect.isTrue (eqv r.Xaxis (Vc(11., 0.)))
                "Y axis aligned" |> Expect.isTrue (eqv r.Yaxis (Vc(0., 10.)))
            }
            test "createFrom3Points with Y on the left keeps origin" {
                let r = Rect2D.createFrom3Points(ro, p1, p3)
                "equals canonical" |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
            }
            test "createFrom3Points with Y on the right reverses X axis" {
                // y-point on the right side of the X-axis -> origin moves to the x-point
                let yRight = Pt(8., -1.)
                let r = Rect2D.createFrom3Points(ro, p1, yRight)
                "origin at x-point" |> Expect.isTrue (eq r.Origin p1)
                "SizeX" |> Expect.isTrue (eqf r.SizeX 10.)
                "SizeY" |> Expect.isTrue (eqf r.SizeY 5.)
            }
            test "createFrom3Points rejects colinear points" {
                "colinear" |> Expect.throws (fun () -> Rect2D.createFrom3Points(ro, p1, Pt(9., 6.)) |> ignore)
            }
            test "tryCreateFrom3Points returns Some for valid points" {
                match Rect2D.tryCreateFrom3Points(ro, p1, p3) with
                | Some r -> "equals canonical" |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
                | None   -> "should be Some" |> Expect.isTrue false
            }
            test "tryCreateFrom3Points returns None for colinear points" {
                let r = Rect2D.tryCreateFrom3Points(ro, p1, Pt(9., 6.))
                "None for colinear" |> Expect.isTrue (Option.isNone r)
            }
            test "createFromDirAndPoints fits the 4 corners" {
                let pts = System.Collections.Generic.List([p0; p1; p2; p3])
                let r = Rect2D.createFromDirAndPoints rvx pts
                "equals canonical" |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
            }
            test "createFromDirAndPoints with fewer than 2 points throws" {
                let pts = System.Collections.Generic.List([p0])
                "single point" |> Expect.throws (fun () -> Rect2D.createFromDirAndPoints rvx pts |> ignore)
            }
            test "createFromDirAndPoints with zero-length direction throws" {
                let pts = System.Collections.Generic.List([p0; p1; p2; p3])
                "zero dir" |> Expect.throws (fun () -> Rect2D.createFromDirAndPoints (Vc(0., 0.)) pts |> ignore)
            }
        ]

        // ---------------------------------------------------------------------
        // fitToPoints
        // ---------------------------------------------------------------------
        testList "fitToPoints" [
            test "fits two opposite corners back to the full rect" {
                let pts = System.Collections.Generic.List([p0; p2])
                let r = Rect2D.fitToPoints pts rr
                "equals canonical" |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
            }
            test "fits interior points to a smaller rect, same orientation" {
                let pts = System.Collections.Generic.List([rCenter; p1])
                let r = Rect2D.fitToPoints pts rr
                "origin" |> Expect.isTrue (eq r.Origin (Pt(9., 6.)))
                "SizeX"  |> Expect.isTrue (eqf r.SizeX 5.)
                "SizeY"  |> Expect.isTrue (eqf r.SizeY 2.5)
                "same X orientation" |> Expect.isTrue (eqf (Vc.angle90 rr.Xaxis r.Xaxis) 0.)
            }
        ]

        // ---------------------------------------------------------------------
        // equals / notEquals
        // ---------------------------------------------------------------------
        testList "equals / notEquals" [
            test "equal to itself" {
                "equals 0 tol" |> Expect.isTrue (Rect2D.equals 0.0 rr rr)
                "notEquals false" |> Expect.isFalse (Rect2D.notEquals 0.0 rr rr)
            }
            test "within tolerance" {
                let r2 = Rect2D.translate (Vc(1e-10, 0.)) rr
                "equals within tol" |> Expect.isTrue (Rect2D.equals 1e-9 rr r2)
            }
            test "not equal when moved" {
                let r2 = Rect2D.translate (Vc(1., 0.)) rr
                "notEquals true"  |> Expect.isTrue (Rect2D.notEquals 1e-9 rr r2)
                "equals false"    |> Expect.isFalse (Rect2D.equals 1e-9 rr r2)
            }
        ]

        // ---------------------------------------------------------------------
        // Validity predicates (rect is always valid by construction)
        // ---------------------------------------------------------------------
        testList "Validity predicates" [
            test "Valid rect predicates" {
                "IsValid"        |> Expect.isTrue rr.IsValid
                "HasArea"        |> Expect.isTrue rr.HasArea
                "IsZero false"   |> Expect.isFalse rr.IsZero
                "IsPoint false"  |> Expect.isFalse rr.IsPoint
                "IsLine false"   |> Expect.isFalse rr.IsLine
                "CountZeroSides" |> Expect.equal rr.CountZeroSides 0
            }
        ]

        // ---------------------------------------------------------------------
        // Scaling
        // ---------------------------------------------------------------------
        testList "Scaling" [
            test "Scale about world origin" {
                let r = rr.Scale 2.
                "origin scaled" |> Expect.isTrue (eq r.Origin (Pt(10., 6.)))
                "SizeX scaled"  |> Expect.isTrue (eqf r.SizeX 20.)
                "SizeY scaled"  |> Expect.isTrue (eqf r.SizeY 10.)
                "center scaled" |> Expect.isTrue (eq r.Center (Pt(15., 16.)))
            }
            test "ScaleOn a center keeps that center fixed" {
                let r = rr.ScaleOn rCenter 2.
                "center kept"  |> Expect.isTrue (eq r.Center rCenter)
                "SizeX scaled" |> Expect.isTrue (eqf r.SizeX 20.)
                "SizeY scaled" |> Expect.isTrue (eqf r.SizeY 10.)
            }
            test "scaleOn static" {
                let r = Rect2D.scaleOn rCenter 0.5 rr
                "center kept"   |> Expect.isTrue (eq r.Center rCenter)
                "SizeX shrunk"  |> Expect.isTrue (eqf r.SizeX 5.)
            }
        ]

        // ---------------------------------------------------------------------
        // Translation
        // ---------------------------------------------------------------------
        testList "Translation" [
            test "translate / move are equal" {
                let a = Rect2D.translate (Vc(1., 2.)) rr
                let b = Rect2D.move (Vc(1., 2.)) rr
                "origin moved" |> Expect.isTrue (eq a.Origin (Pt(6., 5.)))
                "axes unchanged X" |> Expect.isTrue (eqv a.Xaxis rvx)
                "axes unchanged Y" |> Expect.isTrue (eqv a.Yaxis rvy)
                "translate=move" |> Expect.isTrue (Rect2D.equals 1e-9 a b)
            }
            test "translateLocalX moves along the local X-axis" {
                let r = Rect2D.translateLocalX 10. rr
                "moved by full X size" |> Expect.isTrue (eq r.Origin p1)
                "axes unchanged" |> Expect.isTrue (eqv r.Xaxis rvx && eqv r.Yaxis rvy)
            }
            test "translateLocalY moves along the local Y-axis" {
                let r = Rect2D.translateLocalY 5. rr
                "moved by full Y size" |> Expect.isTrue (eq r.Origin p3)
                "axes unchanged" |> Expect.isTrue (eqv r.Xaxis rvx && eqv r.Yaxis rvy)
            }
        ]

        // ---------------------------------------------------------------------
        // Rotation
        // ---------------------------------------------------------------------
        testList "Rotation" [
            test "rotate 90 degrees about world origin" {
                let rot = Rotation2D.createFromDegrees 90.
                let r = Rect2D.rotate rot rr
                "origin rotated" |> Expect.isTrue (eq r.Origin (Pt(-3., 5.)))
                "center rotated" |> Expect.isTrue (eq r.Center (Pt(-8., 7.5)))
                "SizeX kept" |> Expect.isTrue (eqf r.SizeX 10.)
                "SizeY kept" |> Expect.isTrue (eqf r.SizeY 5.)
            }
            test "rotateWithCenter keeps the center fixed" {
                let rot = Rotation2D.createFromDegrees 90.
                let r = Rect2D.rotateWithCenter rCenter rot rr
                "center kept" |> Expect.isTrue (eq r.Center rCenter)
                "SizeX kept"  |> Expect.isTrue (eqf r.SizeX 10.)
            }
            test "four 90 degree rotations return to start" {
                let rot = Rotation2D.createFromDegrees 90.
                let r = rr |> Rect2D.rotate rot |> Rect2D.rotate rot |> Rect2D.rotate rot |> Rect2D.rotate rot
                "back to start" |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
            }
        ]

        // ---------------------------------------------------------------------
        // Expand
        // ---------------------------------------------------------------------
        testList "Expand" [
            test "expand grows all sides and keeps center" {
                let r = Rect2D.expand 1. rr
                "SizeX +2" |> Expect.isTrue (eqf r.SizeX 12.)
                "SizeY +2" |> Expect.isTrue (eqf r.SizeY 7.)
                "center kept" |> Expect.isTrue (eq r.Center rCenter)
            }
            test "expand inwards (negative) shrinks" {
                let r = Rect2D.expand -1. rr
                "SizeX -2" |> Expect.isTrue (eqf r.SizeX 8.)
                "SizeY -2" |> Expect.isTrue (eqf r.SizeY 3.)
                "center kept" |> Expect.isTrue (eq r.Center rCenter)
            }
            test "expand too far inwards throws" {
                "underflow" |> Expect.throws (fun () -> Rect2D.expand -3. rr |> ignore)
            }
            test "expandXY with different distances" {
                let r = Rect2D.expandXY 1. 2. rr
                "SizeX +2" |> Expect.isTrue (eqf r.SizeX 12.)
                "SizeY +4" |> Expect.isTrue (eqf r.SizeY 9.)
                "center kept" |> Expect.isTrue (eq r.Center rCenter)
            }
            test "expandRel scales relative and keeps center" {
                let r = Rect2D.expandRel 2. rr
                "SizeX x2" |> Expect.isTrue (eqf r.SizeX 20.)
                "SizeY x2" |> Expect.isTrue (eqf r.SizeY 10.)
                "center kept" |> Expect.isTrue (eq r.Center rCenter)
            }
            test "expandRel rejects negative factor" {
                "negative factor" |> Expect.throws (fun () -> Rect2D.expandRel -0.5 rr |> ignore)
            }
            test "expandRelXY scales each axis" {
                let r = Rect2D.expandRelXY 2. 3. rr
                "SizeX x2" |> Expect.isTrue (eqf r.SizeX 20.)
                "SizeY x3" |> Expect.isTrue (eqf r.SizeY 15.)
                "center kept" |> Expect.isTrue (eq r.Center rCenter)
            }
        ]

        // ---------------------------------------------------------------------
        // Offset
        // ---------------------------------------------------------------------
        testList "Offset" [
            test "offset inwards keeps center and shrinks both sizes" {
                let r = Rect2D.offset 1. rr
                "SizeX -2" |> Expect.isTrue (eqf r.SizeX 8.)
                "SizeY -2" |> Expect.isTrue (eqf r.SizeY 3.)
                "center kept" |> Expect.isTrue (eq r.Center rCenter)
            }
            test "offset outwards (negative) grows" {
                let r = Rect2D.offset -1. rr
                "SizeX +2" |> Expect.isTrue (eqf r.SizeX 12.)
                "SizeY +2" |> Expect.isTrue (eqf r.SizeY 7.)
            }
            test "offset too far throws" {
                "too small" |> Expect.throws (fun () -> Rect2D.offset 3. rr |> ignore)
            }
            test "offsetVar uniform equals offset" {
                let a = Rect2D.offsetVar [|1.;1.;1.;1.|] rr
                let b = Rect2D.offset 1. rr
                "equal" |> Expect.isTrue (Rect2D.equals 1e-9 a b)
            }
            test "offsetVar with distinct edge distances" {
                // distances for Edge01, Edge12, Edge23, Edge30
                let r = Rect2D.offsetVar [|0.5; 1.0; 1.5; 2.0|] rr
                "SizeX = 10-(1+2)" |> Expect.isTrue (eqf r.SizeX 7.)
                "SizeY = 5-(0.5+1.5)" |> Expect.isTrue (eqf r.SizeY 3.)
                "origin" |> Expect.isTrue (eq r.Origin (Pt(6.3, 4.6)))
            }
            test "offsetVar rejects wrong array length" {
                "wrong length" |> Expect.throws (fun () -> Rect2D.offsetVar [|1.;1.;1.|] rr |> ignore)
            }
            test "offsetCorner at corner 0" {
                let r = Rect2D.offsetCorner(rr, 0, 1., 1., 2., 1.)
                "origin" |> Expect.isTrue (eq r.Origin (Pt(5.2, 4.4)))
                "SizeX = xWidth" |> Expect.isTrue (eqf r.SizeX 2.)
                "SizeY = yHeight" |> Expect.isTrue (eqf r.SizeY 1.)
            }
            test "offsetCorner keeps axis orientation" {
                let r = Rect2D.offsetCorner(rr, 2, 1., 1., 2., 1.)
                "same X orientation" |> Expect.isTrue (eqf (Vc.angle90 rr.Xaxis r.Xaxis) 0.)
            }
            test "offsetCorner out of range throws" {
                "corner 4" |> Expect.throws (fun () -> Rect2D.offsetCorner(rr, 4, 1., 1., 2., 1.) |> ignore)
            }
            test "offsetEdge along edge 0" {
                let r = Rect2D.offsetEdge(rr, 0, 0., 1., 1., 2.)
                "origin" |> Expect.isTrue (eq r.Origin (Pt(5.8, 3.6)))
                "SizeX = lx-offStart-offEnd" |> Expect.isTrue (eqf r.SizeX 7.)
                "SizeY = width" |> Expect.isTrue (eqf r.SizeY 1.)
            }
            test "offsetEdge zero width throws" {
                "zero width" |> Expect.throws (fun () -> Rect2D.offsetEdge(rr, 0, 0., 0., 1., 2.) |> ignore)
            }
        ]

        // ---------------------------------------------------------------------
        // Reorientation (appearance preserved, internal representation changes)
        // ---------------------------------------------------------------------
        testList "RotateOrientation" [
            test "RotateOrientation90CW" {
                let r = rr.RotateOrientation90CW
                "origin at p3" |> Expect.isTrue (eq r.Origin p3)
                "center kept"  |> Expect.isTrue (eq r.Center rCenter)
                "area kept"    |> Expect.isTrue (eqf r.Area 50.)
                "SizeX swapped" |> Expect.isTrue (eqf r.SizeX 5.)
                "SizeY swapped" |> Expect.isTrue (eqf r.SizeY 10.)
            }
            test "RotateOrientation180" {
                let r = rr.RotateOrientation180
                "origin at p2" |> Expect.isTrue (eq r.Origin p2)
                "center kept"  |> Expect.isTrue (eq r.Center rCenter)
                "SizeX kept"   |> Expect.isTrue (eqf r.SizeX 10.)
                "SizeY kept"   |> Expect.isTrue (eqf r.SizeY 5.)
            }
            test "RotateOrientation90CCW" {
                let r = rr.RotateOrientation90CCW
                "origin at p1" |> Expect.isTrue (eq r.Origin p1)
                "center kept"  |> Expect.isTrue (eq r.Center rCenter)
                "SizeX swapped" |> Expect.isTrue (eqf r.SizeX 5.)
                "SizeY swapped" |> Expect.isTrue (eqf r.SizeY 10.)
            }
            test "four CW reorientations return to start" {
                let r = rr.RotateOrientation90CW.RotateOrientation90CW.RotateOrientation90CW.RotateOrientation90CW
                "back to start" |> Expect.isTrue (Rect2D.equals 1e-9 r rr)
            }
        ]

        // ---------------------------------------------------------------------
        // Grid and subdivision on a rotated, off-origin rect
        // ---------------------------------------------------------------------
        testList "Grid and subdivision (rotated)" [
            test "grid 2x2 returns the 4 corners" {
                let g = Rect2D.grid (rr, 2, 2)
                "g[0][0]" |> Expect.isTrue (eq g.[0].[0] p0)
                "g[1][0]" |> Expect.isTrue (eq g.[1].[0] p1)
                "g[1][1]" |> Expect.isTrue (eq g.[1].[1] p2)
                "g[0][1]" |> Expect.isTrue (eq g.[0].[1] p3)
            }
            test "grid 3x3 center point" {
                let g = Rect2D.grid (rr, 3, 3)
                "g[1][1] = center" |> Expect.isTrue (eq g.[1].[1] rCenter)
            }
            test "grid rejects count < 2" {
                "count 1" |> Expect.throws (fun () -> Rect2D.grid (rr, 1, 3) |> ignore)
            }
            test "subDivide 2x1 splits along X" {
                let g = Rect2D.subDivide (rr, 2, 1, 0., 0.)
                "outer length" |> Expect.equal g.Length 2
                "inner length" |> Expect.equal g.[0].Length 1
                "first sub SizeX" |> Expect.isTrue (eqf g.[0].[0].SizeX 5.)
                "first sub SizeY" |> Expect.isTrue (eqf g.[0].[0].SizeY 5.)
                "first sub origin" |> Expect.isTrue (eq g.[0].[0].Origin p0)
                "second sub origin" |> Expect.isTrue (eq g.[1].[0].Origin (Pt(9., 6.)))
            }
            test "subDivide with gap reduces sub-rect size" {
                let g = Rect2D.subDivide (rr, 2, 1, 2., 0.)
                // total X = 10, one gap of 2 -> each sub is (10-2)/2 = 4
                "sub SizeX with gap" |> Expect.isTrue (eqf g.[0].[0].SizeX 4.)
            }
            test "subDivideMinLength bigger than side throws" {
                "min too big" |> Expect.throws (fun () -> Rect2D.subDivideMinLength (rr, 20., 1., 0., 0.) |> ignore)
            }
        ]

        // ---------------------------------------------------------------------
        // String / code representations
        // ---------------------------------------------------------------------
        testList "String representations" [
            test "ToString contains type name" {
                let s = rr.ToString()
                "has type" |> Expect.stringContains s "Rect2D"
            }
            test "AsString contains size separator" {
                let s = rr.AsString
                "has x" |> Expect.stringContains s "x"
            }
            test "AsFSharpCode references createUnchecked" {
                let s = rr.AsFSharpCode
                "has constructor" |> Expect.stringContains s "Rect2D.createUnchecked"
            }
        ]
    ]
