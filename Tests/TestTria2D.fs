module TestTria2D

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pt.distance a b < 1e-9


let tol = Accuracy.veryHigh

let tests =
        testList "Tria2D " [

        test "det" {
            let a = Pt(0, 0)
            let b = Pt(1, 0)
            let c = Pt(0, 1)
            let d = Pt(1, 1)
            "det is 0" |> Expect.floatClose tol (Tria2D.det(a, a,a)) 0.0
            "det is 0a" |> Expect.floatClose tol (Tria2D.det(a, b,a)) 0.0
            "det is 1.0" |> Expect.floatClose tol (Tria2D.det(a,b,c)) 1.0
            "det is 1.0b" |> Expect.floatClose tol (Tria2D.det(a,b,d)) 1.0
            "det negative orientation" |> Expect.floatClose tol (Tria2D.det(c,b,a)) -1.0
        }

        test "offset distance 0 returns original points (CCW right triangle)" {
            let a = Pt(0,0)
            let b = Pt(1,0)
            let c = Pt(1,1)
            let (oa, ob, oc) = Tria2D.offset(a,b,c, 0.0)
            "oa==a" |> Expect.isTrue (eq oa a)
            "ob==b" |> Expect.isTrue (eq ob b)
            "oc==c" |> Expect.isTrue (eq oc c)
        }

        test "offset distance 0 returns original points (CW orientation)" {
            let a = Pt(1,1)
            let b = Pt(1,0)
            let c = Pt(0,0)
            let (oa, ob, oc) = Tria2D.offset(a,b,c, 0.0)
            "oa==a (cw)" |> Expect.isTrue (eq oa a)
            "ob==b (cw)" |> Expect.isTrue (eq ob b)
            "oc==c (cw)" |> Expect.isTrue (eq oc c)
        }

        test "offsetPt distance 0 returns same point (90-degree corner)" {
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            let o = Tria2D.offsetPt(pt, prev, next, 0.0)
            "offsetPt(0) == pt" |> Expect.isTrue (eq o pt)
        }

        test "offsetVarByNormals with zero distances returns original point" {
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            match Tria2D.offsetPtVar(pt, prev, next, 0.0, 0.0) with
            | ValueSome o -> "offsetVarByNormals(0,0)==pt" |> Expect.isTrue (eq o pt)
            | ValueNone -> failwith "expected point for zero distances"
        }



        test "area, areaDouble, areaSigned" {
            // Right triangle with legs 3 and 4 has area 6, double area 12
            let a = Pt(0, 0)
            let b = Pt(4, 0)
            let c = Pt(0, 3)
            "areaDouble = 12" |> Expect.floatClose tol (Tria2D.areaDouble(a,b,c)) 12.0
            "area = 6" |> Expect.floatClose tol (Tria2D.area(a,b,c)) 6.0
            "areaSigned = +6" |> Expect.floatClose tol (Tria2D.areaSigned(a,b,c)) 6.0
            // reverse orientation
            "areaSigned = -6 (reversed)" |> Expect.floatClose tol (Tria2D.areaSigned(a,c,b)) -6.0
        }

        test "isLinearFast collinear and non-collinear" {
            let a = Pt(0,0)
            let b = Pt(1,1)
            let c = Pt(2,2)
            // exactly collinear -> double area = 0 < tol
            "collinear should be linear (fast)" |> Expect.isTrue (Tria2D.isLinearFast(a,b,c, 1e-6))
            // non-collinear unit right triangle: double area = 1
            let a' = Pt(0,0)
            let b' = Pt(1,0)
            let c' = Pt(0,1)
            "non-collinear should not be linear for small tol (fast)" |> Expect.isFalse (Tria2D.isLinearFast(a',b',c', 0.1))
            "non-collinear can be considered linear for huge tol (fast)" |> Expect.isTrue (Tria2D.isLinearFast(a',b',c', 2.0))
        }

        test "isLinear robust, including duplicate points" {
            // non-collinear -> false
            let a = Pt(0,0)
            let b = Pt(1,0)
            let c = Pt(0,1)
            "right triangle should be non-linear" |> Expect.isFalse (Tria2D.isLinear(a,b,c, 1e-6))
            // collinear -> true
            let a2 = Pt(0,0)
            let b2 = Pt(1,1)
            let c2 = Pt(2,2)
            "diagonal points should be linear" |> Expect.isTrue (Tria2D.isLinear(a2,b2,c2, 1e-6))
            // duplicate points: function returns true by design when any segment shorter than tolerance
            let a3 = Pt(0,0)
            let b3 = Pt(0,0)
            let c3 = Pt(1,0)
            "duplicate points treated as linear" |> Expect.isTrue (Tria2D.isLinear(a3,b3,c3, 1e-6))
        }

        test "offsetPt at 90-degree corner (equal distances)" {
            // Corner at (1,0) with prev=(0,0) and next=(1,1) is CCW; inward offset should be (1-d, d)
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            let d = 0.25
            let o = Tria2D.offsetPt(pt, prev, next, d)
            let expected = Pt(1.0 - d, d)
            sprintf "offsetPt expected %A, got %A" expected o |> Expect.isTrue (eq o expected)
        }

        test "offsetPt 180-degree U-turn guard" {
            // prev -> pt -> next makes a U-turn: vPrev=(1,0), vNext=(-1,0)
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(0,0)
            let o = Tria2D.offsetPt(pt, prev, next, 1.0)
            // We don't assert the magic factor, just sanity: very large X shift, Y ~ 0
            sprintf "expected large X due to U-turn, got %A" o |> Expect.isTrue (o.X > 10.0)
            "Y ~ 0 at U-turn" |> Expect.floatClose tol o.Y 0.0
        }

        test "offsetVarByNormals equal distances matches offsetPt at 90-degree corner" {
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            let d = 0.37
            let nPrev = (pt - prev) |> Vc.unitize |> UnitVc.rotate90CCW
            let nNext = (next - pt) |> Vc.unitize |> UnitVc.rotate90CCW
            match Tria2D.offsetPtVarByNormals(pt, nPrev, nNext, d, d) with
            | ValueSome o ->
            let expected = Pt(1.0 - d, d)
            sprintf "offsetVarByNormals(expected=%A, got=%A)" expected o |> Expect.isTrue (eq o expected)
            | ValueNone -> failwith "offsetVarByNormals returned None unexpectedly"
        }

        test "offsetVarByNormals with different distances (90-degree corner)" {
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            let dPrev = 0.2
            let dNext = 0.7
            let nPrev = (pt - prev) |> Vc.unitize |> UnitVc.rotate90CCW
            let nNext = (next - pt) |> Vc.unitize |> UnitVc.rotate90CCW
            match Tria2D.offsetPtVarByNormals(pt, nPrev, nNext, dPrev, dNext) with
            | ValueSome o ->
            let expected = Pt(1.0 - dNext, dPrev)
            sprintf "offsetVarByNormals(expected=%A, got=%A)" expected o |> Expect.isTrue (eq o expected)
            | ValueNone -> failwith "offsetVarByNormals returned None unexpectedly"
        }


        test "offsetPtVar with different distances (90-degree corner)" {
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            let dPrev = 0.2
            let dNext = 0.7
            match Tria2D.offsetPtVar(pt, prev, next, dPrev, dNext) with
            | ValueSome o ->
            let expected = Pt(1.0 - dNext, dPrev)
            sprintf "offsetVarByNormals(expected=%A, got=%A)" expected o |> Expect.isTrue (eq o expected)
            | ValueNone -> failwith "offsetVarByNormals returned None unexpectedly"
        }


        test "offsetPtVar with different distances (90-degree corner), rev order" {
            let prev = Pt(1,1)
            let pt   = Pt(1,0)
            let next = Pt(0,0)
            let dPrev = 0.2
            let dNext = 0.7
            match Tria2D.offsetPtVar(pt, prev, next,  dPrev, dNext) with
            | ValueSome o ->
            let expected = Pt(1.0 + dPrev, -dNext)
            sprintf "offsetVarByNormals(expected=%A, got=%A)" expected o |> Expect.isTrue (eq o expected)
            | ValueNone -> failwith "offsetVarByNormals returned None unexpectedly"
        }


        test "offset equals per-vertex offsetVarByNormals (right triangle)" {
            let a = Pt(0,0)
            let b = Pt(1,0)
            let c = Pt(1,1)
            let d = 0.37
            // normals per vertex (edge directions CCW-rotated)
            let na = (b - a) |> Vc.unitize |> UnitVc.rotate90CCW
            let nb = (c - b) |> Vc.unitize |> UnitVc.rotate90CCW
            let nc = (a - c) |> Vc.unitize |> UnitVc.rotate90CCW
            // expected via line-line intersection approach
            let ea = match Tria2D.offsetPtVarByNormals(a, na, nc, d, d) with ValueSome p -> p | _ -> failwith "expected ea"
            let eb = match Tria2D.offsetPtVarByNormals(b, na, nb, d, d) with ValueSome p -> p | _ -> failwith "expected eb"
            let ec = match Tria2D.offsetPtVarByNormals(c, nb, nc, d, d) with ValueSome p -> p | _ -> failwith "expected ec"
            let (oa, ob, oc) = Tria2D.offset(a,b,c, d)
            "oa matches" |> Expect.isTrue (eq oa ea)
            "ob matches" |> Expect.isTrue (eq ob eb)
            "oc matches" |> Expect.isTrue (eq oc ec)
        }

        test "offset reversed orientation and negative distance" {
            let a = Pt(1,1)
            let b = Pt(1,0)
            let c = Pt(0,0)
            let d = -0.2 // negative flips direction
            let na = (b - a) |> Vc.unitize |> UnitVc.rotate90CCW
            let nb = (c - b) |> Vc.unitize |> UnitVc.rotate90CCW
            let nc = (a - c) |> Vc.unitize |> UnitVc.rotate90CCW
            let ea = match Tria2D.offsetPtVarByNormals(a, na, nc, d, d) with ValueSome p -> p | _ -> failwith "expected ea"
            let eb = match Tria2D.offsetPtVarByNormals(b, na, nb, d, d) with ValueSome p -> p | _ -> failwith "expected eb"
            let ec = match Tria2D.offsetPtVarByNormals(c, nb, nc, d, d) with ValueSome p -> p | _ -> failwith "expected ec"
            let (oa, ob, oc) = Tria2D.offset(a,b,c, d)
            "oa matches (rev/neg)" |> Expect.isTrue (eq oa ea)
            "ob matches (rev/neg)" |> Expect.isTrue (eq ob eb)
            "oc matches (rev/neg)" |> Expect.isTrue (eq oc ec)
        }

        test "offset positive then negative returns to original (CCW right triangle)" {
            let a = Pt(0,0)
            let b = Pt(1,0)
            let c = Pt(1,1)
            let d = 0.25
            // First offset by positive distance
            let (oa1, ob1, oc1) = Tria2D.offset(a,b,c, d)
            // Then offset the result by negative distance to return to original
            let (oa2, ob2, oc2) = Tria2D.offset(oa1, ob1, oc1, -d)
            "a returns to original after +d then -d" |> Expect.isTrue (eq oa2 a)
            "b returns to original after +d then -d" |> Expect.isTrue (eq ob2 b)
            "c returns to original after +d then -d" |> Expect.isTrue (eq oc2 c)
        }

        test "offset negative then positive returns to original (CCW right triangle)" {
            let a = Pt(0,0)
            let b = Pt(1,0)
            let c = Pt(1,1)
            let d = 0.25
            // First offset by negative distance
            let (oa1, ob1, oc1) = Tria2D.offset(a,b,c, -d)
            // Then offset the result by positive distance to return to original
            let (oa2, ob2, oc2) = Tria2D.offset(oa1, ob1, oc1, d)
            "a returns to original after -d then +d" |> Expect.isTrue (eq oa2 a)
            "b returns to original after -d then +d" |> Expect.isTrue (eq ob2 b)
            "c returns to original after -d then +d" |> Expect.isTrue (eq oc2 c)
        }

        test "offset positive then negative returns to original (CW triangle)" {
            let a = Pt(1,1)
            let b = Pt(1,0)
            let c = Pt(0,0)
            let d = 0.11
            // First offset by positive distance
            let (oa1, ob1, oc1) = Tria2D.offset(a,b,c, d)
            // Then offset the result by negative distance to return to original
            let (oa2, ob2, oc2) = Tria2D.offset(oa1, ob1, oc1, -d)
            "a returns to original after +d then -d (CW)" |> Expect.isTrue (eq oa2 a)
            "b returns to original after +d then -d (CW)" |> Expect.isTrue (eq ob2 b)
            "c returns to original after +d then -d (CW)" |> Expect.isTrue (eq oc2 c)
        }

        test "offset negative then positive returns to original (CW triangle)" {
            let a = Pt(1,1)
            let b = Pt(1,0)
            let c = Pt(0,0)
            let d = 0.12
            // First offset by negative distance
            let (oa1, ob1, oc1) = Tria2D.offset(a,b,c, -d)
            // Then offset the result by positive distance to return to original
            let (oa2, ob2, oc2) = Tria2D.offset(oa1, ob1, oc1, d)
            "'a' returns to original after -d then +d (CW)" |> Expect.isTrue (eq oa2 a)
            "'b' returns to original after -d then +d (CW)" |> Expect.isTrue (eq ob2 b)
            "'c' returns to original after -d then +d (CW)" |> Expect.isTrue (eq oc2 c)
        }

        test "offset positive then negative returns to original (equilateral triangle)" {
            let sqrt3 = sqrt 3.0
            let a = Pt(0,0)
            let b = Pt(2,0)
            let c = Pt(1, sqrt3)
            let d = 0.111
            // First offset by positive distance
            let (oa1, ob1, oc1) = Tria2D.offset(a,b,c, d)
            // Then offset the result by negative distance to return to original
            let (oa2, ob2, oc2) = Tria2D.offset(oa1, ob1, oc1, -d)
            "a returns to original after +d then -d (equilateral)" |> Expect.isTrue (eq oa2 a)
            "b returns to original after +d then -d (equilateral)" |> Expect.isTrue (eq ob2 b)
            "c returns to original after +d then -d (equilateral)" |> Expect.isTrue (eq oc2 c)
        }

        test "offsetPtVar positive then negative returns to original (equal distances)" {
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            let d = 0.25
            // First offset by positive distance (equal distances)
            match Tria2D.offsetPtVar(pt, prev, next, d, d) with
            | ValueSome o1 ->
                // Then offset back by negative distance
                match Tria2D.offsetPtVar(o1, prev + (o1 - pt), next + (o1 - pt), -d, -d) with
                | ValueSome o2 ->
                    "offsetPtVar point returns to original after +d then -d (equal)" |> Expect.isTrue (eq o2 pt)
                | ValueNone -> failwith "second offsetPtVar returned None"
            | ValueNone -> failwith "first offsetPtVar returned None"
        }

        test "offsetPtVar negative then positive returns to original (equal distances)" {
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            let d = 0.25
            // First offset by negative distance (equal distances)
            match Tria2D.offsetPtVar(pt, prev, next, -d, -d) with
            | ValueSome o1 ->
                // Then offset back by positive distance
                match Tria2D.offsetPtVar(o1, prev + (o1 - pt), next + (o1 - pt), d, d) with
                | ValueSome o2 ->
                    "offsetPtVar point returns to original after -d then +d (equal)" |> Expect.isTrue (eq o2 pt)
                | ValueNone -> failwith "second offsetPtVar returned None"
            | ValueNone -> failwith "first offsetPtVar returned None"
        }

        test "offsetPtVar positive then negative returns to original (different distances)" {
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            let dPrev = 0.2
            let dNext = 0.3
            // First offset by positive distances
            match Tria2D.offsetPtVar(pt, prev, next, dPrev, dNext) with
            | ValueSome o1 ->
                // Calculate new prev and next positions relative to offset point
                let newPrev = prev + (o1 - pt)
                let newNext = next + (o1 - pt)
                // Then offset back by negative distances
                match Tria2D.offsetPtVar(o1, newPrev, newNext, -dPrev, -dNext) with
                | ValueSome o2 ->
                    "offsetPtVar point returns to original after +d then -d (different)" |> Expect.isTrue (eq o2 pt)
                | ValueNone -> failwith "second offsetPtVar returned None"
            | ValueNone -> failwith "first offsetPtVar returned None"
        }

        test "offsetPtVar negative then positive returns to original (different distances)" {
            let prev = Pt(0,0)
            let pt   = Pt(1,0)
            let next = Pt(1,1)
            let dPrev = 0.15
            let dNext = 0.35
            // First offset by negative distances
            match Tria2D.offsetPtVar(pt, prev, next, -dPrev, -dNext) with
            | ValueSome o1 ->
                // Calculate new prev and next positions relative to offset point
                let newPrev = prev + (o1 - pt)
                let newNext = next + (o1 - pt)
                // Then offset back by positive distances
                match Tria2D.offsetPtVar(o1, newPrev, newNext, dPrev, dNext) with
                | ValueSome o2 ->
                    "offsetPtVar point returns to original after -d then +d (different)" |> Expect.isTrue (eq o2 pt)
                | ValueNone -> failwith "second offsetPtVar returned None"
            | ValueNone -> failwith "first offsetPtVar returned None"
        }

        test "offset inward CCW triangle reduces area" {
            // Equilateral triangle CCW
            let sqrt3 = sqrt 3.0
            let a = Pt(0, 0)
            let b = Pt(2, 0)
            let c = Pt(1, sqrt3)
            let originalArea = Tria2D.area(a, b, c)
            let d = 0.1 // positive offset = inward for CCW
            let (oa, ob, oc) = Tria2D.offset(a, b, c, d)
            let offsetArea = Tria2D.area(oa, ob, oc)
            "inward offset reduces area" |> Expect.isTrue (offsetArea < originalArea)
        }

        test "offset outward CCW triangle increases area" {
            // Equilateral triangle CCW
            let sqrt3 = sqrt 3.0
            let a = Pt(0, 0)
            let b = Pt(2, 0)
            let c = Pt(1, sqrt3)
            let originalArea = Tria2D.area(a, b, c)
            let d = -0.1 // negative offset = outward for CCW
            let (oa, ob, oc) = Tria2D.offset(a, b, c, d)
            let offsetArea = Tria2D.area(oa, ob, oc)
            "outward offset increases area" |> Expect.isTrue (offsetArea > originalArea)
        }

        test "offset inward CW triangle increases area (offset goes outward)" {
            // Right triangle CW orientation
            let a = Pt(1, 1)
            let b = Pt(1, 0)
            let c = Pt(0, 0)
            let originalArea = Tria2D.area(a, b, c)
            let d = 0.1 // positive offset = outward for CW
            let (oa, ob, oc) = Tria2D.offset(a, b, c, d)
            let offsetArea = Tria2D.area(oa, ob, oc)
            "positive offset on CW triangle increases area" |> Expect.isTrue (offsetArea > originalArea)
        }

        test "offset equilateral triangle with known geometry" {
            // Equilateral triangle CCW, centered at origin for simpler math
            let sqrt3 = sqrt 3.0
            let a = Pt(-1, -sqrt3 / 3.0)
            let b = Pt(1, -sqrt3 / 3.0)
            let c = Pt(0, 2.0 * sqrt3 / 3.0)
            let d = 0.1
            let (oa, ob, oc) = Tria2D.offset(a, b, c, d)
            // For an equilateral triangle, all offset points should move toward centroid by same distance
            let centroid = Pt((a.X + b.X + c.X) / 3.0, (a.Y + b.Y + c.Y) / 3.0)
            let distA = Pt.distance a centroid
            let distOa = Pt.distance oa centroid
            let distB = Pt.distance b centroid
            let distOb = Pt.distance ob centroid
            let distC = Pt.distance c centroid
            let distOc = Pt.distance oc centroid
            "oa is closer to centroid than a" |> Expect.isTrue (distOa < distA)
            "ob is closer to centroid than b" |> Expect.isTrue (distOb < distB)
            "oc is closer to centroid than c" |> Expect.isTrue (distOc < distC)
            // All offset points should be equidistant from centroid (equilateral symmetry)
            "offset triangle is equilateral (oa-ob)" |> Expect.floatClose tol distOa distOb
            "offset triangle is equilateral (ob-oc)" |> Expect.floatClose tol distOb distOc
        }

        test "offset preserves orientation CCW" {
            let a = Pt(0, 0)
            let b = Pt(2, 0)
            let c = Pt(1, 1)
            let originalDet = Tria2D.det(a, b, c)
            let (oa, ob, oc) = Tria2D.offset(a, b, c, 0.1)
            let offsetDet = Tria2D.det(oa, ob, oc)
            "CCW orientation preserved after offset" |> Expect.isTrue (sign originalDet = sign offsetDet)
        }

        test "offset preserves orientation CW" {
            let a = Pt(1, 1)
            let b = Pt(2, 0)
            let c = Pt(0, 0)
            let originalDet = Tria2D.det(a, b, c)
            let (oa, ob, oc) = Tria2D.offset(a, b, c, 0.1)
            let offsetDet = Tria2D.det(oa, ob, oc)
            "CW orientation preserved after offset" |> Expect.isTrue (sign originalDet = sign offsetDet)
        }

    ]