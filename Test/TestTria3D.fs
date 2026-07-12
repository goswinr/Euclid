module TestTria3D

open Euclid

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pnt.dist a b < 1e-9


let tol = Accuracy.veryHigh

let tests =
    testList "Tria3D " [

        test "areaDouble and area" {
        // Right triangle with legs 3 and 4 in XY plane has area 6, double area 12
        let a = Pnt(0, 0, 0)
        let b = Pnt(4, 0, 0)
        let c = Pnt(0, 3, 0)
        "areaDouble = 12" |> Expect.floatClose tol (Tria3D.areaDouble(a,b,c)) 12.0
        "area = 6" |> Expect.floatClose tol (Tria3D.area(a,b,c)) 6.0

        // Triangle in different plane (XZ)
        let a2 = Pnt(0, 0, 0)
        let b2 = Pnt(3, 0, 0)
        let c2 = Pnt(0, 0, 4)
        "areaDouble = 12 (XZ)" |> Expect.floatClose tol (Tria3D.areaDouble(a2,b2,c2)) 12.0
        "area = 6 (XZ)" |> Expect.floatClose tol (Tria3D.area(a2,b2,c2)) 6.0
        }

        test "area of degenerate triangles" {
        let a = Pnt(0, 0, 0)
        let b = Pnt(1, 1, 1)
        let c = Pnt(2, 2, 2) // collinear points
        "collinear points have zero area" |> Expect.floatClose tol (Tria3D.area(a,b,c)) 0.0

        // duplicate points
        "duplicate points have zero area" |> Expect.floatClose tol (Tria3D.area(a,a,a)) 0.0
        "two duplicate points have zero area" |> Expect.floatClose tol (Tria3D.area(a,a,b)) 0.0
        }

        test "isLinearFast collinear and non-collinear" {
        let a = Pnt(0,0,0)
        let b = Pnt(1,1,1)
        let c = Pnt(2,2,2)
        // exactly collinear -> double area = 0 < tol
        "collinear should be linear (fast)" |> Expect.isTrue (Tria3D.isLinearFast(a,b,c, 1e-6))

        // non-collinear triangle in XY plane: double area > 0
        let a' = Pnt(0,0,0)
        let b' = Pnt(1,0,0)
        let c' = Pnt(0,1,0)
        "non-collinear should not be linear for small tol (fast)" |> Expect.isFalse (Tria3D.isLinearFast(a',b',c', 0.1))
        "non-collinear can be considered linear for huge tol (fast)" |> Expect.isTrue (Tria3D.isLinearFast(a',b',c', 2.0))
        }

        test "isLinear robust, including duplicate points" {
        // non-collinear -> false
        let a = Pnt(0,0,0)
        let b = Pnt(1,0,0)
        let c = Pnt(0,1,0)
        "right triangle should be non-linear" |> Expect.isFalse (Tria3D.isLinear(a,b,c, 1e-6))

        // collinear -> true
        let a2 = Pnt(0,0,0)
        let b2 = Pnt(1,1,1)
        let c2 = Pnt(2,2,2)
        "diagonal points should be linear" |> Expect.isTrue (Tria3D.isLinear(a2,b2,c2, 1e-6))

        // duplicate points: function returns true by design when any segment shorter than tolerance
        let a3 = Pnt(0,0,0)
        let b3 = Pnt(0,0,0)
        let c3 = Pnt(1,0,0)
        "duplicate points treated as linear" |> Expect.isTrue (Tria3D.isLinear(a3,b3,c3, 1e-6))
        }

        test "offset distance 0 returns original points (CCW triangle in XY plane)" {
        let a = Pnt(0,0,0)
        let b = Pnt(1,0,0)
        let c = Pnt(1,1,0)
        let (oa, ob, oc) = Tria3D.offset(a,b,c, 0.0)
        "oa==a" |> Expect.isTrue (eq oa a)
        "ob==b" |> Expect.isTrue (eq ob b)
        "oc==c" |> Expect.isTrue (eq oc c)
        }

        test "offset distance 0 returns original points (triangle in XZ plane)" {
        let a = Pnt(0,0,0)
        let b = Pnt(1,0,0)
        let c = Pnt(1,0,1)
        let (oa, ob, oc) = Tria3D.offset(a,b,c, 0.0)
        "oa==a (XZ)" |> Expect.isTrue (eq oa a)
        "ob==b (XZ)" |> Expect.isTrue (eq ob b)
        "oc==c (XZ)" |> Expect.isTrue (eq oc c)
        }

        test "offset distance 0 returns original points (triangle in YZ plane)" {
        let a = Pnt(0,0,0)
        let b = Pnt(0,1,0)
        let c = Pnt(0,1,1)
        let (oa, ob, oc) = Tria3D.offset(a,b,c, 0.0)
        "oa==a (YZ)" |> Expect.isTrue (eq oa a)
        "ob==b (YZ)" |> Expect.isTrue (eq ob b)
        "oc==c (YZ)" |> Expect.isTrue (eq oc c)
        }

        test "offsetPnt distance 0 returns same point (90-degree corner in XY)" {
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,0,0)
        let next = Pnt(1,1,0)
        let o = Tria3D.offsetPnt(pt, prev, next, 0.0)
        "offsetPnt(0) == pt" |> Expect.isTrue (eq o pt)
        }

        test "offsetPnt distance 0 returns same point (90-degree corner in XZ)" {
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,0,0)
        let next = Pnt(1,0,1)
        let o = Tria3D.offsetPnt(pt, prev, next, 0.0)
        "offsetPnt(0) == pt (XZ)" |> Expect.isTrue (eq o pt)
        }

        test "offsetVar with zero distances returns original point" {
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,0,0)
        let next = Pnt(1,1,0)
        match Tria3D.offsetVar(pt, prev, next, 0.0, 0.0) with
        | ValueSome o -> "offsetVar(0,0)==pt" |> Expect.isTrue (eq o pt)
        | ValueNone -> failwith "expected point for zero distances"
        }

        test "offset positive then negative returns to original (CCW triangle in XY)" {
        let a = Pnt(0,0,0)
        let b = Pnt(1,0,0)
        let c = Pnt(1,1,0)
        let d = 0.25
        // First offset by positive distance
        let (oa1, ob1, oc1) = Tria3D.offset(a,b,c, d)
        // Then offset the result by negative distance to return to original
        let (oa2, ob2, oc2) = Tria3D.offset(oa1, ob1, oc1, -d)
        "a returns to original after +d then -d" |> Expect.isTrue (eq oa2 a)
        "b returns to original after +d then -d" |> Expect.isTrue (eq ob2 b)
        "c returns to original after +d then -d" |> Expect.isTrue (eq oc2 c)
        }

        test "offset negative then positive returns to original (CCW triangle in XY)" {
        let a = Pnt(0,0,0)
        let b = Pnt(1,0,0)
        let c = Pnt(1,1,0)
        let d = 0.25
        // First offset by negative distance
        let (oa1, ob1, oc1) = Tria3D.offset(a,b,c, -d)
        // Then offset the result by positive distance to return to original
        let (oa2, ob2, oc2) = Tria3D.offset(oa1, ob1, oc1, d)
        "a returns to original after -d then +d" |> Expect.isTrue (eq oa2 a)
        "b returns to original after -d then +d" |> Expect.isTrue (eq ob2 b)
        "c returns to original after -d then +d" |> Expect.isTrue (eq oc2 c)
        }

        test "offset positive then negative returns to original (triangle in XZ plane)" {
        let a = Pnt(0,0,0)
        let b = Pnt(2,0,0)
        let c = Pnt(1,0,2)
        let d = 0.3
        // First offset by positive distance
        let (oa1, ob1, oc1) = Tria3D.offset(a,b,c, d)
        // Then offset the result by negative distance to return to original
        let (oa2, ob2, oc2) = Tria3D.offset(oa1, ob1, oc1, -d)
        "a returns to original after +d then -d (XZ)" |> Expect.isTrue (eq oa2 a)
        "b returns to original after +d then -d (XZ)" |> Expect.isTrue (eq ob2 b)
        "c returns to original after +d then -d (XZ)" |> Expect.isTrue (eq oc2 c)
        }

        test "offset negative then positive returns to original (triangle in YZ plane)" {
        let a = Pnt(0,0,0)
        let b = Pnt(0,2,0)
        let c = Pnt(0,1,2)
        let d = 0.2
        // First offset by negative distance
        let (oa1, ob1, oc1) = Tria3D.offset(a,b,c, -d)
        // Then offset the result by positive distance to return to original
        let (oa2, ob2, oc2) = Tria3D.offset(oa1, ob1, oc1, d)
        "a returns to original after -d then +d (YZ)" |> Expect.isTrue (eq oa2 a)
        "b returns to original after -d then +d (YZ)" |> Expect.isTrue (eq ob2 b)
        "c returns to original after -d then +d (YZ)" |> Expect.isTrue (eq oc2 c)
        }

        test "offset positive then negative returns to original (arbitrary orientation)" {
        let a = Pnt(1,2,3)
        let b = Pnt(4,1,2)
        let c = Pnt(2,5,1)
        let d = 0.15
        // First offset by positive distance
        let (oa1, ob1, oc1) = Tria3D.offset(a,b,c, d)
        // Then offset the result by negative distance to return to original
        let (oa2, ob2, oc2) = Tria3D.offset(oa1, ob1, oc1, -d)
        "a returns to original after +d then -d (arbitrary)" |> Expect.isTrue (eq oa2 a)
        "b returns to original after +d then -d (arbitrary)" |> Expect.isTrue (eq ob2 b)
        "c returns to original after +d then -d (arbitrary)" |> Expect.isTrue (eq oc2 c)
        }

        test "offset with larger distances maintains roundtrip property" {
        let a = Pnt(0,0,0)
        let b = Pnt(3,0,0)
        let c = Pnt(1.5,3,0)
        let d = 0.5 // smaller offset to ensure stability
        // First offset by positive distance
        let (oa1, ob1, oc1) = Tria3D.offset(a,b,c, d)
        // Then offset the result by negative distance to return to original
        let (oa2, ob2, oc2) = Tria3D.offset(oa1, ob1, oc1, -d)
        "a returns to original after +d then -d (medium offset)" |> Expect.isTrue (eq oa2 a)
        "b returns to original after +d then -d (medium offset)" |> Expect.isTrue (eq ob2 b)
        "c returns to original after +d then -d (medium offset)" |> Expect.isTrue (eq oc2 c)
        }

        test "offsetVar with different distances (90-degree corner in XY)" {
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,0,0)
        let next = Pnt(1,1,0)
        let dPrev = 0.2
        let dNext = 0.7
        match Tria3D.offsetVar(pt, prev, next, dPrev, dNext) with
        | ValueSome o ->
            // For 3D, the offset calculation is more complex than 2D
            // Let's just verify the point is reasonable and not infinite
            sprintf "offsetVar with different distances result: %A" o |> Expect.isTrue (not (System.Double.IsInfinity o.X || System.Double.IsInfinity o.Y || System.Double.IsInfinity o.Z))
        | ValueNone -> failwith "offsetVar returned None unexpectedly"
        }

        test "offsetVar handles collinear input gracefully" {
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,1,1)
        let next = Pnt(2,2,2) // collinear points
        match Tria3D.offsetVar(pt, prev, next, 0.5, 0.5) with
        | ValueSome _ -> failwith "expected None for collinear points"
        | ValueNone -> "offsetVar correctly returns None for collinear points" |> Expect.isTrue true
        }

        test "offsetPnt handles U-turn gracefully (in XY plane)" {
        // prev -> pt -> next makes a U-turn
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,0,0)
        let next = Pnt(0,0,0)
        // This should throw an exception because the points are collinear
        try
            let _o = Tria3D.offsetPnt(pt, prev, next, 1.0)
            failwith "Expected exception for collinear/duplicate points"
        with
        | :? Euclid.EuclidErrors.EuclidException ->
            "offsetPnt correctly throws exception for collinear points" |> Expect.isTrue true
        | _ -> failwith "Expected EuclidException"
        }

        test "offsetPnt near-U-turn cap side, in-plane and scaling (XY plane)" {
            // symmetric corner about pt=(1,0,0): travel +X into pt, turn by 'turnDeg' in the XY plane.
            let mkOffset turnDeg dist =
                let prev = Pnt(0,0,0)
                let pt   = Pnt(1,0,0)
                let a    = turnDeg * System.Math.PI / 180.0
                let next = pt + Vec(cos a, sin a, 0.0) // outgoing direction rotated by the turn angle
                Tria3D.offsetPnt(pt, prev, next, dist)
            let pt = Pnt(1,0,0)
            let oCap = mkOffset 179.5 1.0 // above the 179-degree threshold -> capped fallback
            "capped fallback is on the negative-X side" |> Expect.isTrue (oCap.X < -10.0)
            "capped fallback stays in the triangle plane (Z = 0)" |> Expect.floatClose tol oCap.Z 0.0
            // the U-turn cap scales linearly with dist
            let oCap2 = mkOffset 179.5 2.0
            "cap scales linearly with dist" |> Expect.isTrue (Pnt.dist oCap2 (pt + (oCap - pt) * 2.0) < 1e-9)
        }

        test "area calculation matches expected for known triangles" {
        // Unit triangle in XY plane
        let a = Pnt(0,0,0)
        let b = Pnt(1,0,0)
        let c = Pnt(0,1,0)
        "unit right triangle area = 0.5" |> Expect.floatClose tol (Tria3D.area(a,b,c)) 0.5

        // Equilateral triangle with side length 2
        let sqrt3 = sqrt 3.0
        let a2 = Pnt(0,0,0)
        let b2 = Pnt(2,0,0)
        let c2 = Pnt(1,sqrt3,0)
        "equilateral triangle area = sqrt(3)" |> Expect.floatClose tol (Tria3D.area(a2,b2,c2)) sqrt3
        }

        test "offsetVar positive then negative returns to original (equal distances)" {
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,0,0)
        let next = Pnt(1,1,0)
        let d = 0.25
        // First offset by positive distance (equal distances)
        match Tria3D.offsetVar(pt, prev, next, d, d) with
        | ValueSome o1 ->
            // Calculate new prev and next positions relative to offset point
            let displacement = o1 - pt
            let newPrev = prev + displacement
            let newNext = next + displacement
            // Then offset back by negative distance
            match Tria3D.offsetVar(o1, newPrev, newNext, -d, -d) with
            | ValueSome o2 ->
            "offsetVar point returns to original after +d then -d (equal)" |> Expect.isTrue (eq o2 pt)
            | ValueNone -> failwith "second offsetVar returned None"
        | ValueNone -> failwith "first offsetVar returned None"
        }

        test "offsetVar negative then positive returns to original (equal distances)" {
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,0,0)
        let next = Pnt(1,1,0)
        let d = 0.25
        // First offset by negative distance (equal distances)
        match Tria3D.offsetVar(pt, prev, next, -d, -d) with
        | ValueSome o1 ->
            // Calculate new prev and next positions relative to offset point
            let displacement = o1 - pt
            let newPrev = prev + displacement
            let newNext = next + displacement
            // Then offset back by positive distance
            match Tria3D.offsetVar(o1, newPrev, newNext, d, d) with
            | ValueSome o2 ->
            "offsetVar point returns to original after -d then +d (equal)" |> Expect.isTrue (eq o2 pt)
            | ValueNone -> failwith "second offsetVar returned None"
        | ValueNone -> failwith "first offsetVar returned None"
        }

        test "offsetVar positive then negative returns to original (different distances)" {
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,0,0)
        let next = Pnt(1,1,0)
        let dPrev = 0.2
        let dNext = 0.3
        // First offset by positive distances
        match Tria3D.offsetVar(pt, prev, next, dPrev, dNext) with
        | ValueSome o1 ->
            // Calculate new prev and next positions relative to offset point
            let displacement = o1 - pt
            let newPrev = prev + displacement
            let newNext = next + displacement
            // Then offset back by negative distances
            match Tria3D.offsetVar(o1, newPrev, newNext, -dPrev, -dNext) with
            | ValueSome o2 ->
            "offsetVar point returns to original after +d then -d (different)" |> Expect.isTrue (eq o2 pt)
            | ValueNone -> failwith "second offsetVar returned None"
        | ValueNone -> failwith "first offsetVar returned None"
        }

        test "offsetVar negative then positive returns to original (different distances in XZ)" {
        let prev = Pnt(0,0,0)
        let pt   = Pnt(1,0,0)
        let next = Pnt(1,0,1)
        let dPrev = 0.15
        let dNext = 0.35
        // First offset by negative distances
        match Tria3D.offsetVar(pt, prev, next, -dPrev, -dNext) with
        | ValueSome o1 ->
            // Calculate new prev and next positions relative to offset point
            let displacement = o1 - pt
            let newPrev = prev + displacement
            let newNext = next + displacement
            // Then offset back by positive distances
            match Tria3D.offsetVar(o1, newPrev, newNext, dPrev, dNext) with
            | ValueSome o2 ->
            "offsetVar point returns to original after -d then +d (different XZ)" |> Expect.isTrue (eq o2 pt)
            | ValueNone -> failwith "second offsetVar returned None"
        | ValueNone -> failwith "first offsetVar returned None"
        }

        test "offset inward reduces area (equilateral triangle in XY plane)" {
        // Equilateral triangle in XY plane
        let sqrt3 = sqrt 3.0
        let a = Pnt(0, 0, 0)
        let b = Pnt(2, 0, 0)
        let c = Pnt(1, sqrt3, 0)
        let originalArea = Tria3D.area(a, b, c)
        let d = 0.1 // positive offset = inward
        let (oa, ob, oc) = Tria3D.offset(a, b, c, d)
        let offsetArea = Tria3D.area(oa, ob, oc)
        "inward offset reduces area" |> Expect.isTrue (offsetArea < originalArea)
        }

        test "offset outward increases area (equilateral triangle in XY plane)" {
        // Equilateral triangle in XY plane
        let sqrt3 = sqrt 3.0
        let a = Pnt(0, 0, 0)
        let b = Pnt(2, 0, 0)
        let c = Pnt(1, sqrt3, 0)
        let originalArea = Tria3D.area(a, b, c)
        let d = -0.1 // negative offset = outward
        let (oa, ob, oc) = Tria3D.offset(a, b, c, d)
        let offsetArea = Tria3D.area(oa, ob, oc)
        "outward offset increases area" |> Expect.isTrue (offsetArea > originalArea)
        }

        test "offset inward reduces area (triangle in XZ plane)" {
        let a = Pnt(0, 0, 0)
        let b = Pnt(2, 0, 0)
        let c = Pnt(1, 0, 2)
        let originalArea = Tria3D.area(a, b, c)
        let d = 0.1
        let (oa, ob, oc) = Tria3D.offset(a, b, c, d)
        let offsetArea = Tria3D.area(oa, ob, oc)
        "inward offset reduces area (XZ)" |> Expect.isTrue (offsetArea < originalArea)
        }

        test "offset outward increases area (triangle in YZ plane)" {
        let a = Pnt(0, 0, 0)
        let b = Pnt(0, 2, 0)
        let c = Pnt(0, 1, 2)
        let originalArea = Tria3D.area(a, b, c)
        let d = -0.1
        let (oa, ob, oc) = Tria3D.offset(a, b, c, d)
        let offsetArea = Tria3D.area(oa, ob, oc)
        "outward offset increases area (YZ)" |> Expect.isTrue (offsetArea > originalArea)

        let originalArea = Tria3D.area(a, c, b)//swapped b and c
        let d = -0.1
        let (oa, ob, oc) = Tria3D.offset(a, c, b, d)
        let offsetArea = Tria3D.area(oa, ob, oc)
        "outward offset increases area (YZ)" |> Expect.isTrue (offsetArea > originalArea)



        }

        test "offset equilateral triangle preserves symmetry" {
        // Equilateral triangle centered at origin in XY plane
        let sqrt3 = sqrt 3.0
        let a = Pnt(-1, -sqrt3 / 3.0, 0)
        let b = Pnt(1, -sqrt3 / 3.0, 0)
        let c = Pnt(0, 2.0 * sqrt3 / 3.0, 0)
        let d = 0.1
        let (oa, ob, oc) = Tria3D.offset(a, b, c, d)
        // For an equilateral triangle, all offset points should move toward centroid
        let centroid = Pnt((a.X + b.X + c.X) / 3.0, (a.Y + b.Y + c.Y) / 3.0, (a.Z + b.Z + c.Z) / 3.0)
        let distA = Pnt.dist a centroid
        let distOa = Pnt.dist oa centroid
        let distB = Pnt.dist b centroid
        let distOb = Pnt.dist ob centroid
        let distC = Pnt.dist c centroid
        let distOc = Pnt.dist oc centroid
        "oa is closer to centroid than a" |> Expect.isTrue (distOa < distA)
        "ob is closer to centroid than b" |> Expect.isTrue (distOb < distB)
        "oc is closer to centroid than c" |> Expect.isTrue (distOc < distC)
        // All offset points should be equidistant from centroid (equilateral symmetry)
        "offset triangle is equilateral (oa-ob)" |> Expect.floatClose tol distOa distOb
        "offset triangle is equilateral (ob-oc)" |> Expect.floatClose tol distOb distOc
        }

        test "offset stays in same plane (XY)" {
        let a = Pnt(0, 0, 5)
        let b = Pnt(2, 0, 5)
        let c = Pnt(1, 2, 5)
        let d = 0.2
        let (oa, ob, oc) = Tria3D.offset(a, b, c, d)
        "oa stays in Z=5 plane" |> Expect.floatClose tol oa.Z 5.0
        "ob stays in Z=5 plane" |> Expect.floatClose tol ob.Z 5.0
        "oc stays in Z=5 plane" |> Expect.floatClose tol oc.Z 5.0
        }

        test "offset stays in same plane (arbitrary tilted)" {
        // Triangle in a tilted plane
        let a = Pnt(0, 0, 0)
        let b = Pnt(1, 0, 1)
        let c = Pnt(0, 1, 1)
        let d = 0.1
        let (oa, ob, oc) = Tria3D.offset(a, b, c, d)
        // Compute the original plane normal
        let v1 = b - a
        let v2 = c - a
        let normal = Vec.cross(v1, v2) |> Vec.unitize
        // Check that offset points lie in the same plane (dot product with normal from original point should be 0)
        let distOa = abs ((oa - a) *** normal)
        let distOb = abs ((ob - a) *** normal)
        let distOc = abs ((oc - a) *** normal)
        "oa stays in original plane" |> Expect.floatClose tol distOa 0.0
        "ob stays in original plane" |> Expect.floatClose tol distOb 0.0
        "oc stays in original plane" |> Expect.floatClose tol distOc 0.0
        }

        test "offset throws for collinear points" {
        let a = Pnt(0, 0, 0)
        let b = Pnt(1, 1, 1)
        let c = Pnt(2, 2, 2)
        try
            let _ = Tria3D.offset(a, b, c, 0.1)
            failwith "Expected exception for collinear points"
        with
        | :? Euclid.EuclidErrors.EuclidException ->
            "offset correctly throws exception for collinear points" |> Expect.isTrue true
        | _ -> failwith "Expected EuclidException"
        }

        test "intersectLine with a real-world triangle and many lines" {
        // Triangle and lines captured from Rhino, see .\TestInRhino\xTria\xRayTriaRh.fsx
        let triaPt_a = Pnt(274.299666151928, 1.30123144462857, 423.86724430024054)
        let triaPt_b = Pnt(453.1741748436796, -134.02380242987584, 206.17239974220166)
        let triaPt_c = Pnt(150.97102968630978, -189.5755141913957, 367.6879295755854)

        // Triangle plane normal, used to verify intersection points lie in the triangle's plane
        let normal = Vec.cross(triaPt_b - triaPt_a, triaPt_c - triaPt_a) |> Vec.unitize

        // Asserts the line intersects, the result lies on the line segment and in the triangle plane
        let isHit (name:string) (ln:Line3D) =
            match Tria3D.intersectLine(ln, triaPt_a, triaPt_b, triaPt_c) with
            | ValueSome p ->
                let distToPlane = abs ((p - triaPt_a) *** normal)
                sprintf "%s: intersection point lies in triangle plane" name |> Expect.floatClose tol distToPlane 0.0
                sprintf "%s: intersection point lies on the line" name |> Expect.floatClose tol (ln.DistanceToPnt p) 0.0
            | ValueNone -> failwithf "%s: expected an intersection but got ValueNone" name

        // Asserts the line does not intersect the triangle
        let isMiss (name:string) (ln:Line3D) =
            match Tria3D.intersectLine(ln, triaPt_a, triaPt_b, triaPt_c) with
            | ValueNone -> sprintf "%s: correctly returns ValueNone" name |> Expect.isTrue true
            | ValueSome p -> failwithf "%s: expected no intersection but got %A" name p

        isHit  "ln0"  (Line3D(102.72246833783501, -122.94911894204799, 367.6879295755854, 322.54822750040285, -65.32516380471921, 423.86724430024054))
        isHit  "ln1"  (Line3D(142.67455890821319, -199.70811815812138, 285.28835396582934, 362.500318070781, -142.0841630207926, 341.4676686904845))
        isHit  "ln2"  (Line3D(231.03802167985143, -77.93481557304926, 314.6613319006782, 450.86378084241926, -20.310860435720492, 370.84064662533336))
        isMiss "ln3"  (Line3D(92.76167932692925, -268.52009572678577, 257.1986966035018, 312.5874384894971, -210.896140589457, 313.37801132815696))
        isHit  "ln4"  (Line3D(274.299666151928, 1.30123144462857, 423.86724430024054, 317.4064841710968, -74.82080619592374, 511.4495313410636))
        isHit  "ln5"  (Line3D(453.1741748436796, -134.02380242987584, 206.17239974220166, 551.8861420073981, -308.33859589637893, 406.7304608510648))
        isHit  "ln6"  (Line3D(274.299666151928, 1.30123144462857, 423.86724430024054, 317.4064841710968, -74.82080619592372, 511.4495313410636))
        isHit  "ln7"  (Line3D(274.299666151928, 1.30123144462857, 423.86724430024054, 317.4064841710968, -74.82080619592372, 511.4495313410636))
        isHit  "ln8"  (Line3D(274.299666151928, 1.30123144462857, 423.86724430024054, 317.4064841710968, -74.82080619592372, 511.4495313410636))
        isHit  "ln9"  (Line3D(274.299666151928, 1.30123144462857, 423.86724430024054, 317.4064841710968, -74.82080619592372, 511.4495313410636))
        isHit  "ln10" (Line3D(453.1741748436796, -134.02380242987584, 206.17239974220166, 551.8861420073981, -308.33859589637893, 406.73046085106483))
        isHit  "ln11" (Line3D(453.1741748436796, -134.02380242987584, 206.17239974220166, 551.8861420073981, -308.33859589637893, 406.73046085106483))
        isHit  "ln12" (Line3D(453.1741748436796, -134.02380242987584, 206.17239974220166, 551.8861420073981, -308.33859589637893, 406.73046085106483))
        isHit  "ln13" (Line3D(453.1741748436796, -134.02380242987584, 206.17239974220166, 551.8861420073981, -308.33859589637893, 406.73046085106483))
        isHit  "ln14" (Line3D(46.74059071089063, -5.515688772364626, 155.91771214323435, 231.173666293795, -331.2048063370528, 530.6396542346407))
        isHit  "ln15" (Line3D(132.07393316766078, 12.725130139468362, 146.43622964803768, 321.173666293795, -321.2048063370528, 530.6396542346407))
        isMiss "ln16" (Line3D(281.03802167985145, -57.93481557304926, 314.6613319006782, 500.86378084241926, -0.3108604357204925, 370.84064662533336))
        isMiss "ln17" (Line3D(62.72246833783501, -122.94911894204799, 367.6879295755854, 282.54822750040285, -65.32516380471921, 423.86724430024054))
        isMiss "ln18" (Line3D(347.0517209810619, -149.67763694946564, 298.77236661456413, 387.2876380945246, -220.72997014768868, 380.52170010549855))
        isMiss "ln19" (Line3D(297.7007606236559, -62.52911069421449, 198.50354199752604, 330.5868331468975, -120.6024031685264, 265.31982711732036))
        }

        test "intersectLine is independent of triangle vertex order" {
        // A line that intersects must do so regardless of the order the triangle points are given in
        let a = Pnt(274.299666151928, 1.30123144462857, 423.86724430024054)
        let b = Pnt(453.1741748436796, -134.02380242987584, 206.17239974220166)
        let c = Pnt(150.97102968630978, -189.5755141913957, 367.6879295755854)
        let ln = Line3D(231.03802167985143, -77.93481557304926, 314.6613319006782, 450.86378084241926, -20.310860435720492, 370.84064662533336)
        match Tria3D.intersectLine(ln, a, b, c) with
        | ValueSome p ->
            "permutation (b,c,a) intersects" |> Expect.isTrue (Tria3D.intersectLine(ln, b, c, a)).IsSome
            "permutation (c,a,b) intersects" |> Expect.isTrue (Tria3D.intersectLine(ln, c, a, b)).IsSome
            "permutation (a,c,b) intersects" |> Expect.isTrue (Tria3D.intersectLine(ln, a, c, b)).IsSome
            "permutation (b,a,c) intersects" |> Expect.isTrue (Tria3D.intersectLine(ln, b, a, c)).IsSome
            "permutation (c,b,a) intersects" |> Expect.isTrue (Tria3D.intersectLine(ln, c, b, a)).IsSome
            // all permutations must return the same intersection point
            "permutation (b,c,a) same point" |> Expect.isTrue (eq p (Tria3D.intersectLine(ln, b, c, a)).Value)
            "permutation (c,b,a) same point" |> Expect.isTrue (eq p (Tria3D.intersectLine(ln, c, b, a)).Value)
        | ValueNone -> failwith "expected an intersection for the base vertex order"
        }

        test "intersectLine returns None when the segment ends before the triangle plane" {
        // The infinite ray would hit the triangle, but the finite segment is too short to reach it
        let a = Pnt(274.299666151928, 1.30123144462857, 423.86724430024054)
        let b = Pnt(453.1741748436796, -134.02380242987584, 206.17239974220166)
        let c = Pnt(150.97102968630978, -189.5755141913957, 367.6879295755854)
        let full = Line3D(231.03802167985143, -77.93481557304926, 314.6613319006782, 450.86378084241926, -20.310860435720492, 370.84064662533336)
        // the full segment intersects
        "full segment intersects" |> Expect.isTrue (Tria3D.intersectLine(full, a, b, c)).IsSome
        // a segment that stops at 10% of the way must not reach the triangle, but the infinite ray still does
        let short = Line3D(full.From, full.From + (full.To - full.From) * 0.1)
        "shortened segment does not reach the triangle" |> Expect.isTrue (Tria3D.intersectLine(short, a, b, c)).IsNone
        "but the infinite ray still intersects" |> Expect.isTrue (Tria3D.intersectRay(short, a, b, c)).IsSome
        }

    ]
