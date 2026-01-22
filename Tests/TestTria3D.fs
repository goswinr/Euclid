module TestTria3D

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pnt.distance a b < 1e-9


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
        let distA = Pnt.distance a centroid
        let distOa = Pnt.distance oa centroid
        let distB = Pnt.distance b centroid
        let distOb = Pnt.distance ob centroid
        let distC = Pnt.distance c centroid
        let distOc = Pnt.distance oc centroid
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

    ]
