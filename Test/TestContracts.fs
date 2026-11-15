module TestContracts

open Euclid
open EuclidTestSupport

// Tests for the failure and tolerance contracts:
// 'operation' fails, 'tryOperation' returns a voption,
// 'equalsWithin' compares each coordinate within a tolerance and 'equalsExact' compares exactly.

// Test shapes are rotated and moved, so that they are not axis aligned at the origin.
let private q = Quaternion.createFromDegreesVec (Vec(1., 2., 3.)) 33.
let private shift = Vec(12.5, -7.25, 3.5)
let private place (p:Pnt) = Pnt.rotate q p + shift
let private placeXYZ x y z = place (Pnt(x, y, z))
let private placeLn (ax, ay, az) (bx, by, bz) = Line3D(placeXYZ ax ay az, placeXYZ bx by bz)
let private placeVec (v:Vec) = place (Pnt(v.X, v.Y, v.Z)) - place Pnt.Origin

let private r2 = Rotation2D.createFromDegrees 27.
let private shift2 = Vc(8.5, -3.25)
let private place2 x y = Pt.rotate r2 (Pt(x, y)) + shift2
let private placeLn2 (ax, ay) (bx, by) = Line2D(place2 ax ay, place2 bx by)
let private placeVc x y = place2 x y - place2 0. 0.

let inline private eqPnt a b = Pnt.distanceTo a b < 1e-9
let inline private eqPt a b = Pt.distanceTo a b < 1e-9
let private close = Accuracy.veryHigh

let tests =
    testList "Contracts" [

        testList "equalsWithin and equalsExact" [

            test "Pnt.equalsWithin applies the tolerance to each coordinate, not to the distance" {
                let a = placeXYZ 1. 2. 3.
                let b = Pnt(a.X + 0.9, a.Y - 0.9, a.Z + 0.9)
                "each coordinate is within 1.0" |> Expect.isTrue (Pnt.equalsWithin 1.0 a b)
                "although the distance is bigger than 1.0" |> Expect.isTrue (Pnt.distanceTo a b > 1.0)
                "a coordinate is outside 0.8" |> Expect.isFalse (Pnt.equalsWithin 0.8 a b)
                "notEqualsWithin 0.8" |> Expect.isTrue (Pnt.notEqualsWithin 0.8 a b)
                "notEqualsWithin 1.0" |> Expect.isFalse (Pnt.notEqualsWithin 1.0 a b)
                "only one coordinate outside" |> Expect.isFalse (Pnt.equalsWithin 1.0 a (Pnt(a.X, a.Y, a.Z + 1.1)))
            }

            test "Pt.equalsWithin applies the tolerance to each coordinate, not to the distance" {
                let a = place2 1. 2.
                let b = Pt(a.X + 0.9, a.Y - 0.9)
                "each coordinate is within 1.0" |> Expect.isTrue (Pt.equalsWithin 1.0 a b)
                "although the distance is bigger than 1.0" |> Expect.isTrue (Pt.distanceTo a b > 1.0)
                "a coordinate is outside 0.8" |> Expect.isFalse (Pt.equalsWithin 0.8 a b)
                "notEqualsWithin 0.8" |> Expect.isTrue (Pt.notEqualsWithin 0.8 a b)
            }

            test "Pnt.equalsExact" {
                let a = placeXYZ 1. 2. 3.
                "same point" |> Expect.isTrue (Pnt.equalsExact a a)
                "tiny difference" |> Expect.isFalse (Pnt.equalsExact a (Pnt(a.X, a.Y, a.Z + 1e-12)))
                "negative zero equals zero" |> Expect.isTrue (Pnt.equalsExact (Pnt(0., 0., 0.)) (Pnt(-0., 0., -0.)))
            }

            test "equalsExact on other types" {
                let v = placeVec (Vec(1., 2., 3.))
                let w = Vec(v.X, v.Y + 1e-9, v.Z)
                "Vec same" |> Expect.isTrue (Vec.equalsExact v v)
                "Vec different" |> Expect.isFalse (Vec.equalsExact v w)
                "Vec within" |> Expect.isTrue (Vec.equalsWithin 1e-6 v w)
                let u = Vec.unitize v
                "UnitVec same" |> Expect.isTrue (UnitVec.equalsExact u u)
                "UnitVec flipped" |> Expect.isFalse (UnitVec.equalsExact u -u)
                let ln = placeLn (1., 2., 3.) (4., 5., 6.)
                "Line3D same" |> Expect.isTrue (Line3D.equalsExact ln ln)
                "Line3D reversed" |> Expect.isFalse (Line3D.equalsExact ln ln.Reversed)
                let ln2 = placeLn2 (1., 2.) (4., 5.)
                "Line2D same" |> Expect.isTrue (Line2D.equalsExact ln2 ln2)
                "Line2D reversed" |> Expect.isFalse (Line2D.equalsExact ln2 ln2.Reversed)
                let b = BBox.create (Pnt(10., 20., 30.)) (Pnt(14., 25., 33.))
                "BBox same" |> Expect.isTrue (BBox.equalsExact b b)
                "BBox expanded" |> Expect.isFalse (BBox.equalsExact b (BBox.expand 1e-9 b))
                "BBox expanded within" |> Expect.isTrue (BBox.equalsWithin 1e-6 b (BBox.expand 1e-9 b))
                let pl = PPlane.createFrom3Pnts (placeXYZ 0. 0. 0.) (placeXYZ 1. 0. 0.) (placeXYZ 0. 1. 0.)
                "PPlane same" |> Expect.isTrue (PPlane.equalsExact pl pl)
                "PPlane moved" |> Expect.isFalse (PPlane.equalsExact pl (PPlane.move (Vec(0., 0., 1e-9)) pl))
                "Matrix same" |> Expect.isTrue (Matrix.equalsExact Matrix.Identity Matrix.Identity)
                "Matrix translated" |> Expect.isFalse (Matrix.equalsExact Matrix.Identity (Matrix.createTranslation(Vec(0., 0., 1e-9))))
                "Rotation2D same" |> Expect.isTrue (Rotation2D.equalsExact r2 r2)
                "Quaternion same" |> Expect.isTrue (Quaternion.equalsExact q q)
            }

            test "Polyline3D.equalsWithin and equalsExact" {
                let pts = [ placeXYZ 0. 0. 0.; placeXYZ 5. 0. 0.; placeXYZ 5. 4. 0. ]
                let a = Polyline3D.createFromPnts pts
                let b = Polyline3D.createFromPnts (pts |> List.map (fun p -> Pnt(p.X + 0.5, p.Y - 0.5, p.Z + 0.5)))
                "within 0.6 per coordinate" |> Expect.isTrue (Polyline3D.equalsWithin 0.6 a b)
                "not within 0.4" |> Expect.isFalse (Polyline3D.equalsWithin 0.4 a b)
                "not exact" |> Expect.isFalse (Polyline3D.equalsExact a b)
                "exact copy" |> Expect.isTrue (Polyline3D.equalsExact a (Polyline3D.createFromPnts pts))
                "different point count" |> Expect.isFalse (Polyline3D.equalsWithin 1.0 a (Polyline3D.createFromPnts pts.Tail))
                // the constructors of Pnt and Vec reject NaN in DEBUG builds, but a Polyline3D can hold it
                let n1 = Polyline3D.createFromXYZs (ResizeArray [ nan; 1.; 2. ])
                let n2 = Polyline3D.createFromXYZs (ResizeArray [ nan; 1.; 2. ])
                "NaN is never exactly equal" |> Expect.isFalse (Polyline3D.equalsExact n1 n2)
                "NaN is never equal within a tolerance" |> Expect.isFalse (Polyline3D.equalsWithin 1.0 n1 n2)
                "but the same instance is equal to itself" |> Expect.isTrue (Polyline3D.equalsExact n1 n1)
            }

            test "Polyline2D.equalsWithin and equalsExact" {
                let pts = [ place2 0. 0.; place2 5. 0.; place2 5. 4. ]
                let a = Polyline2D.createFromPts pts
                let b = Polyline2D.createFromPts (pts |> List.map (fun p -> Pt(p.X + 0.5, p.Y - 0.5)))
                "within 0.6 per coordinate" |> Expect.isTrue (Polyline2D.equalsWithin 0.6 a b)
                "not within 0.4" |> Expect.isFalse (Polyline2D.equalsWithin 0.4 a b)
                "not exact" |> Expect.isFalse (Polyline2D.equalsExact a b)
                "exact copy" |> Expect.isTrue (Polyline2D.equalsExact a (Polyline2D.createFromPts pts))
            }

            test "Quaternion.equalsRotationWithin accepts the negated quaternion" {
                let n = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
                "not component-wise equal" |> Expect.isFalse (Quaternion.equalsWithin 1e-9 q n)
                "same rotation" |> Expect.isTrue (Quaternion.equalsRotationWithin 1e-9 q n)
            }
        ]

        testList "tryUnitize" [

            test "Vec.tryUnitize" {
                let v = placeVec (Vec(3., -4., 12.))
                match Vec.tryUnitize v with
                | ValueSome u -> "same as unitize" |> Expect.isTrue (UnitVec.equalsExact u (Vec.unitize v))
                | ValueNone -> failtest "expected ValueSome"
                "zero length" |> Expect.isValueNone (Vec.tryUnitize (Vec(0., 0., 0.)))
                "shorter than 1e-12" |> Expect.isValueNone (Vec.tryUnitize (Vec(1e-13, 0., 0.)))
                "longer than 1e-12" |> Expect.isValueSome (Vec.tryUnitize (Vec(1e-11, 0., 0.)))
                "unitize fails where tryUnitize returns ValueNone" |> Expect.throws (fun () -> Vec.unitize (Vec(1e-13, 0., 0.)) |> ignore)
            }

            test "Vc.tryUnitize" {
                let v = placeVc 3. -4.
                match Vc.tryUnitize v with
                | ValueSome u -> "same as unitize" |> Expect.isTrue (UnitVc.equalsExact u (Vc.unitize v))
                | ValueNone -> failtest "expected ValueSome"
                "zero length" |> Expect.isValueNone (Vc.tryUnitize (Vc(0., 0.)))
                "shorter than 1e-12" |> Expect.isValueNone (Vc.tryUnitize (Vc(0., 1e-13)))
                "longer than 1e-12" |> Expect.isValueSome (Vc.tryUnitize (Vc(0., 1e-11)))
                "unitize fails where tryUnitize returns ValueNone" |> Expect.throws (fun () -> Vc.unitize (Vc(0., 1e-13)) |> ignore)
            }
        ]

        testList "operation fails where tryOperation returns ValueNone" [

            test "PPlane intersections" {
                let a = PPlane.createFrom3Pnts (placeXYZ 0. 0. 0.) (placeXYZ 1. 0. 0.) (placeXYZ 0. 1. 0.)
                let b = PPlane.createFrom3Pnts (placeXYZ 0. 0. 0.) (placeXYZ 1. 0. 0.) (placeXYZ 0. 0. 1.)
                let parallelPl = PPlane.createFrom3Pnts (placeXYZ 0. 0. 5.) (placeXYZ 1. 0. 5.) (placeXYZ 0. 1. 5.)
                match PPlane.tryIntersect a b with
                | ValueSome ln -> "intersect same as tryIntersect" |> Expect.isTrue (Line3D.equalsExact ln (PPlane.intersect a b))
                | ValueNone -> failtest "expected ValueSome"
                "parallel planes" |> Expect.isValueNone (PPlane.tryIntersect a parallelPl)
                "parallel planes fail" |> Expect.throws (fun () -> PPlane.intersect a parallelPl |> ignore)

                let ray = placeLn (2., 3., -4.) (2., 3., 6.)
                "intersectRay" |> Expect.floatClose close (PPlane.intersectRay ray a) 0.4
                "tryIntersectRay" |> Expect.equal (PPlane.tryIntersectRay ray a) (ValueSome (PPlane.intersectRay ray a))
                let t, x, y = PPlane.intersectRayAllParameters ray a
                "all parameters, line" |> Expect.floatClose close t 0.4
                "all parameters, X" |> Expect.floatClose close x 2.0
                "all parameters, Y" |> Expect.floatClose close y 3.0
                "intersectLine" |> Expect.isTrue (eqPnt (PPlane.intersectLine ray a) (placeXYZ 2. 3. 0.))

                let parallelRay = placeLn (0., 0., 1.) (5., 2., 1.)
                "parallel ray" |> Expect.isValueNone (PPlane.tryIntersectRay parallelRay a)
                "parallel ray fails" |> Expect.throws (fun () -> PPlane.intersectRay parallelRay a |> ignore)
                "parallel ray all parameters fails" |> Expect.throws (fun () -> PPlane.intersectRayAllParameters parallelRay a |> ignore)
                let short = placeLn (2., 3., 1.) (2., 3., 6.)
                "line not reaching the plane" |> Expect.isValueNone (PPlane.tryIntersectLine short a)
                "line not reaching the plane fails" |> Expect.throws (fun () -> PPlane.intersectLine short a |> ignore)
            }

            test "NPlane intersections" {
                let o = placeXYZ 0. 0. 0.
                let a = NPlane.createFromNormal (placeVec (Vec(0., 0., 1.))) o
                let b = NPlane.createFromNormal (placeVec (Vec(0., 1., 0.))) o
                let parallelPl = NPlane.createFromNormal (placeVec (Vec(0., 0., 1.))) (placeXYZ 0. 0. 5.)
                match NPlane.tryIntersect a b with
                | ValueSome ln -> "intersect same as tryIntersect" |> Expect.isTrue (Line3D.equalsExact ln (NPlane.intersect a b))
                | ValueNone -> failtest "expected ValueSome"
                "parallel planes" |> Expect.isValueNone (NPlane.tryIntersect a parallelPl)
                "parallel planes fail" |> Expect.throws (fun () -> NPlane.intersect a parallelPl |> ignore)

                let ray = placeLn (2., 3., -4.) (2., 3., 6.)
                "intersectRay" |> Expect.floatClose close (NPlane.intersectRay ray a) 0.4
                "intersectLine" |> Expect.isTrue (eqPnt (NPlane.intersectLine ray a) (placeXYZ 2. 3. 0.))
                let parallelRay = placeLn (0., 0., 1.) (5., 2., 1.)
                "parallel ray" |> Expect.isValueNone (NPlane.tryIntersectRay parallelRay a)
                "parallel ray fails" |> Expect.throws (fun () -> NPlane.intersectRay parallelRay a |> ignore)
                let short = placeLn (2., 3., 1.) (2., 3., 6.)
                "line not reaching the plane" |> Expect.isValueNone (NPlane.tryIntersectLine short a)
                "line not reaching the plane fails" |> Expect.throws (fun () -> NPlane.intersectLine short a |> ignore)
            }

            test "BBox intersections" {
                let a = BBox.create (Pnt(10., 20., 30.)) (Pnt(14., 25., 33.))
                let b = BBox.create (Pnt(12., 22., 31.)) (Pnt(18., 28., 40.))
                let apart = BBox.create (Pnt(100., 100., 100.)) (Pnt(101., 101., 101.))
                let expected = BBox.create (Pnt(12., 22., 31.)) (Pnt(14., 25., 33.))
                "intersection" |> Expect.isTrue (BBox.equalsExact (BBox.intersection a b) expected)
                "Intersection" |> Expect.isTrue (BBox.equalsExact (a.Intersection b) expected)
                match BBox.tryIntersection a b with
                | ValueSome r -> "tryIntersection" |> Expect.isTrue (BBox.equalsExact r expected)
                | ValueNone -> failtest "expected ValueSome"
                "apart" |> Expect.isValueNone (BBox.tryIntersection a apart)
                "apart fails" |> Expect.throws (fun () -> BBox.intersection a apart |> ignore)
                "apart fails, instance member" |> Expect.throws (fun () -> a.Intersection apart |> ignore)

                let ray = Line3D(Pnt(12., 23., 0.), Pnt(12., 23., 1.))
                let tEntry, tExit = BBox.intersectRay ray a
                "entry" |> Expect.floatClose close tEntry 30.0
                "exit" |> Expect.floatClose close tExit 33.0
                "IntersectRay" |> Expect.equal (a.IntersectRay ray) (tEntry, tExit)
                "tryIntersectRay" |> Expect.equal (BBox.tryIntersectRay ray a) (ValueSome (tEntry, tExit))
                let miss = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 1.))
                "miss" |> Expect.isValueNone (BBox.tryIntersectRay miss a)
                "miss fails" |> Expect.throws (fun () -> BBox.intersectRay miss a |> ignore)
                let zero = Line3D(Pnt(12., 23., 31.), Pnt(12., 23., 31.))
                "zero length ray fails" |> Expect.throws (fun () -> a.IntersectRay zero |> ignore)
            }

            test "BRect intersections" {
                let a = BRect.create (Pt(10., 20.)) (Pt(14., 25.))
                let b = BRect.create (Pt(12., 22.)) (Pt(18., 28.))
                let apart = BRect.create (Pt(100., 100.)) (Pt(101., 101.))
                let expected = BRect.create (Pt(12., 22.)) (Pt(14., 25.))
                "intersection" |> Expect.isTrue (BRect.equalsExact (BRect.intersection a b) expected)
                "Intersection" |> Expect.isTrue (BRect.equalsExact (a.Intersection b) expected)
                "apart" |> Expect.isValueNone (BRect.tryIntersection a apart)
                "apart fails" |> Expect.throws (fun () -> BRect.intersection a apart |> ignore)

                let ray = Line2D(Pt(0., 23.), Pt(1., 23.))
                let tEntry, tExit = BRect.intersectRay ray a
                "entry" |> Expect.floatClose close tEntry 10.0
                "exit" |> Expect.floatClose close tExit 14.0
                "IntersectRay" |> Expect.equal (a.IntersectRay ray) (tEntry, tExit)
                let miss = Line2D(Pt(0., 0.), Pt(1., 0.))
                "miss" |> Expect.isValueNone (a.TryIntersectRay miss)
                "miss fails" |> Expect.throws (fun () -> BRect.intersectRay miss a |> ignore)
            }

            test "Box.intersectRay" {
                let o = placeXYZ 0. 0. 0.
                let box = Box.create (placeVec (Vec(4., 0., 0.))) (placeVec (Vec(0., 3., 0.))) (placeVec (Vec(0., 0., 2.))) o
                let ray = placeLn (1., 1., -1.) (1., 1., 1.)
                let tEntry, tExit = Box.intersectRay ray box
                "entry" |> Expect.floatClose close tEntry 0.5
                "exit" |> Expect.floatClose close tExit 1.5
                "IntersectRay" |> Expect.equal (box.IntersectRay ray) (tEntry, tExit)
                "tryIntersectRay" |> Expect.equal (Box.tryIntersectRay ray box) (ValueSome (tEntry, tExit))
                let miss = placeLn (10., 10., -1.) (10., 10., 1.)
                "miss" |> Expect.isValueNone (box.TryIntersectRay miss)
                "miss fails" |> Expect.throws (fun () -> Box.intersectRay miss box |> ignore)
            }

            test "Rect2D intersections" {
                let rect = Rect2D.createFromVcs (placeVc 4. 0.) (placeVc 0. 3.) (place2 0. 0.)
                let ray = placeLn2 (-1., 1.) (1., 1.)
                let tEntry, tExit = Rect2D.intersectRay ray rect
                "entry" |> Expect.floatClose close tEntry 0.5
                "exit" |> Expect.floatClose close tExit 2.5
                let clipped = Rect2D.intersectLine ray rect
                "intersectLine start" |> Expect.isTrue (eqPt clipped.From (place2 0. 1.))
                "intersectLine end" |> Expect.isTrue (eqPt clipped.To (place2 1. 1.))
                "clipLine" |> Expect.isTrue (Line2D.equalsExact (Rect2D.clipLine ray rect) clipped)
                match Rect2D.tryClipLine ray rect with
                | ValueSome ln -> "tryClipLine" |> Expect.isTrue (Line2D.equalsExact ln clipped)
                | ValueNone -> failtest "expected ValueSome"
                let miss = placeLn2 (-1., 10.) (5., 10.)
                "miss" |> Expect.isValueNone (Rect2D.tryIntersectRay miss rect)
                "miss fails" |> Expect.throws (fun () -> Rect2D.intersectRay miss rect |> ignore)
                "miss line" |> Expect.isValueNone (Rect2D.tryIntersectLine miss rect)
                "miss line fails" |> Expect.throws (fun () -> Rect2D.intersectLine miss rect |> ignore)
                "miss clip fails" |> Expect.throws (fun () -> Rect2D.clipLine miss rect |> ignore)
            }

            test "Rect3D intersections" {
                let rect = Rect3D.createFromVecs (placeVec (Vec(4., 0., 0.))) (placeVec (Vec(0., 3., 0.))) (placeXYZ 0. 0. 0.)
                let ray = placeLn (1., 1., -1.) (1., 1., 1.)
                "intersectRay" |> Expect.floatClose close (Rect3D.intersectRay ray rect) 0.5
                "intersectLine" |> Expect.isTrue (eqPnt (Rect3D.intersectLine ray rect) (placeXYZ 1. 1. 0.))
                let parallelLn = placeLn (1., 1., 1.) (2., 1., 1.)
                "parallel" |> Expect.isValueNone (Rect3D.tryIntersectRay parallelLn rect)
                "parallel fails" |> Expect.throws (fun () -> Rect3D.intersectRay parallelLn rect |> ignore)
                let outside = placeLn (10., 10., -1.) (10., 10., 1.)
                "outside the rectangle, but the ray hits its plane" |> Expect.floatClose close (Rect3D.intersectRay outside rect) 0.5
                "outside" |> Expect.isValueNone (Rect3D.tryIntersectLine outside rect)
                "outside fails" |> Expect.throws (fun () -> Rect3D.intersectLine outside rect |> ignore)
            }

            test "Tria2D offsets" {
                let pt = place2 0. 0.
                let prev = place2 -5. 0.
                let next = place2 0. 5.
                "offsetPtVariable" |> Expect.isTrue (eqPt (Tria2D.offsetPtVariable 1.0 2.0 prev next pt) (place2 -2. 1.))
                match Tria2D.tryOffsetPtVariable 1.0 2.0 prev next pt with
                | ValueSome p -> "tryOffsetPtVar" |> Expect.isTrue (Pt.equalsExact p (Tria2D.offsetPtVariable 1.0 2.0 prev next pt))
                | ValueNone -> failtest "expected ValueSome"
                let nPrev = Vc.unitize (placeVc 0. 1.)
                let nNext = Vc.unitize (placeVc -1. 0.)
                "offsetPtVariableByNormals" |> Expect.isTrue (eqPt (Tria2D.offsetPtVariableByNormals 1.0 2.0 nPrev nNext pt) (place2 -2. 1.))
                let straight = place2 5. 0.
                "collinear with equal distances" |> Expect.isTrue (eqPt (Tria2D.offsetPtVariable 1.0 1.0 prev straight pt) (place2 0. 1.))
                "collinear with different distances" |> Expect.isValueNone (Tria2D.tryOffsetPtVariable 1.0 2.0 prev straight pt)
                "collinear with different distances fails" |> Expect.throws (fun () -> Tria2D.offsetPtVariable 1.0 2.0 prev straight pt |> ignore)
                "parallel normals fail" |> Expect.throws (fun () -> Tria2D.offsetPtVariableByNormals 1.0 2.0 nPrev nPrev pt |> ignore)
            }

            test "Tria3D offsets and intersections" {
                let pnt = placeXYZ 0. 0. 0.
                let prev = placeXYZ -5. 0. 0.
                let next = placeXYZ 0. 5. 0.
                match Tria3D.tryOffsetPntVariable 1.0 2.0 prev next pnt with
                | ValueSome p -> "offsetPntVariable same as tryOffsetPntVar" |> Expect.isTrue (Pnt.equalsExact p (Tria3D.offsetPntVariable 1.0 2.0 prev next pnt))
                | ValueNone -> failtest "expected ValueSome"
                let straight = placeXYZ 5. 0. 0.
                "collinear" |> Expect.isValueNone (Tria3D.tryOffsetPntVariable 1.0 2.0 prev straight pnt)
                "collinear fails" |> Expect.throws (fun () -> Tria3D.offsetPntVariable 1.0 2.0 prev straight pnt |> ignore)

                let a = placeXYZ 0. 0. 0.
                let b = placeXYZ 4. 0. 0.
                let c = placeXYZ 0. 4. 0.
                let ray = placeLn (1., 1., -1.) (1., 1., 1.)
                let t = Tria3D.intersectRay a b c ray
                "intersectRay" |> Expect.floatClose close t 0.5
                "intersectRayXYZ" |> Expect.equal (Tria3D.intersectRayXYZ(ray.FromX, ray.FromY, ray.FromZ, ray.ToX, ray.ToY, ray.ToZ, a.X, a.Y, a.Z, b.X, b.Y, b.Z, c.X, c.Y, c.Z)) t
                let e1 = b - a
                let e2 = c - a
                let v = ray.Vector
                "intersectTriaVectorsWithRay" |> Expect.equal (Tria3D.intersectTriaVectorsWithRay(ray.FromX, ray.FromY, ray.FromZ, v.X, v.Y, v.Z, a.X, a.Y, a.Z, e1.X, e1.Y, e1.Z, e2.X, e2.Y, e2.Z)) t
                "intersectLine" |> Expect.isTrue (eqPnt (Tria3D.intersectLine a b c ray) (placeXYZ 1. 1. 0.))

                let miss = placeLn (5., 5., -1.) (5., 5., 1.)
                "miss" |> Expect.isValueNone (Tria3D.tryIntersectRay a b c miss)
                "miss fails" |> Expect.throws (fun () -> Tria3D.intersectRay a b c miss |> ignore)
                "miss fails XYZ" |> Expect.throws (fun () -> Tria3D.intersectRayXYZ(miss.FromX, miss.FromY, miss.FromZ, miss.ToX, miss.ToY, miss.ToZ, a.X, a.Y, a.Z, b.X, b.Y, b.Z, c.X, c.Y, c.Z) |> ignore)
                let short = placeLn (1., 1., 1.) (1., 1., 2.)
                "short line, but the ray hits" |> Expect.floatClose close (Tria3D.intersectRay a b c short) -1.0
                "short line" |> Expect.isValueNone (Tria3D.tryIntersectLine a b c short)
                "short line fails" |> Expect.throws (fun () -> Tria3D.intersectLine a b c short |> ignore)
            }

            test "Line3D ray closest parameters and points" {
                let a = placeLn (0., 0., 0.) (1., 0., 0.)
                let b = placeLn (0., 5., 3.) (0., 6., 3.)
                let s, u = Line3D.rayClosestParameters a b
                "parameter on a" |> Expect.floatClose close s 0.0
                "parameter on b" |> Expect.floatClose close u -5.0
                "RayClosestParameters" |> Expect.equal (a.RayClosestParameters b) (s, u)
                "tryRayClosestParameters" |> Expect.equal (Line3D.tryRayClosestParameters a b) (ValueSome (s, u))
                let pa, pb = Line3D.rayClosestPoints a b
                "closest point on a" |> Expect.isTrue (eqPnt pa (placeXYZ 0. 0. 0.))
                "closest point on b" |> Expect.isTrue (eqPnt pb (placeXYZ 0. 0. 3.))
                let pa2, pb2 = a.RayClosestPoints b
                "RayClosestPoints" |> Expect.isTrue (Pnt.equalsExact pa pa2 && Pnt.equalsExact pb pb2)
                let parallelLn = placeLn (0., 1., 0.) (1., 1., 0.)
                "parallel" |> Expect.isValueNone (a.TryRayClosestParameters parallelLn)
                "parallel fails" |> Expect.throws (fun () -> Line3D.rayClosestParameters a parallelLn |> ignore)
                "parallel points fail" |> Expect.throws (fun () -> a.RayClosestPoints parallelLn |> ignore)
                let zero = placeLn (0., 5., 3.) (0., 5., 3.)
                "zero length fails" |> Expect.throws (fun () -> Line3D.rayClosestPoints a zero |> ignore)
            }

            test "Line3D.intersectCone" {
                // the cone axis is the world Z-axis, so the ray is moved off the axis instead
                let ray = Line3D(Pnt(-10., 0.5, 2.), Pnt(10., 0.5, 2.))
                let t1, t2 = Line3D.intersectCone 5.0 0.0 10.0 ray
                "tryIntersectCone" |> Expect.equal (Line3D.tryIntersectCone 5.0 0.0 10.0 ray) (ValueSome (t1, t2))
                // at Z = 2.0 the cone has a radius of 4.0
                let radiusAt t = let p = ray.EvaluateAt t in sqrt (p.X*p.X + p.Y*p.Y)
                "first hit on the cone" |> Expect.floatClose Accuracy.medium (radiusAt t1) 4.0
                "second hit on the cone" |> Expect.floatClose Accuracy.medium (radiusAt t2) 4.0
                let miss = Line3D(Pnt(-10., 20., 2.), Pnt(10., 20., 2.))
                "miss" |> Expect.isValueNone (Line3D.tryIntersectCone 5.0 0.0 10.0 miss)
                "miss fails" |> Expect.throws (fun () -> Line3D.intersectCone 5.0 0.0 10.0 miss |> ignore)
            }
        ]
    ]
