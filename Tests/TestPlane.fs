module TestPlane

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPnt a b = Pnt.distance a b < 1e-9
let inline eqVec a b = Vec.length (a - b) < 1e-9
let inline eqFloat a b = abs(a - b) < 1e-9

let tests =
    testList "Planes" [

        testList "NPlane - Construction" [
            test "create from point and vector" {
                let origin = Pnt(0., 0., 0.)
                let normal = Vec(0., 0., 1.)
                let plane = NPlane.create(origin, normal)
                Expect.isTrue (eqPnt plane.Origin origin) "Origin should match"
                Expect.isTrue (eqVec plane.Normal.AsVec (Vec(0., 0., 1.))) "Normal should be unitized"
            }

            test "create unitizes the normal vector" {
                let origin = Pnt(0., 0., 0.)
                let normal = Vec(0., 0., 10.)
                let plane = NPlane.create(origin, normal)
                Expect.isTrue (eqFloat (Vec.length plane.Normal.AsVec) 1.0) "Normal should be unit length"
            }

            test "create rejects zero-length normal" {
                let origin = Pnt(0., 0., 0.)
                let normal = Vec.Zero
                Expect.throws (fun () -> NPlane.create(origin, normal) |> ignore) "Should throw for zero normal"
            }

            test "create from UnitVec" {
                let origin = Pnt(1., 2., 3.)
                let normal = UnitVec.Zaxis
                let plane = NPlane.create(origin, normal)
                Expect.isTrue (eqPnt plane.Origin origin) "Origin should match"
                Expect.isTrue (eqVec plane.Normal.AsVec UnitVec.Zaxis.AsVec) "Normal should match"
            }

            test "createFrom3Points" {
                let a = Pnt(0., 0., 0.)
                let b = Pnt(1., 0., 0.)
                let c = Pnt(0., 1., 0.)
                let plane = NPlane.createFrom3Points a b c
                Expect.isTrue (eqPnt plane.Origin a) "Origin should be at first point"
                // Normal should be perpendicular to both (b-a) and (c-a)
                let normal = plane.Normal
                Expect.isTrue (eqFloat (normal *** (b-a)) 0.0) "Normal should be perpendicular to edge 1"
                Expect.isTrue (eqFloat (normal *** (c-a)) 0.0) "Normal should be perpendicular to edge 2"
            }

            test "createFrom3Points rejects colinear points" {
                let a = Pnt(0., 0., 0.)
                let b = Pnt(1., 0., 0.)
                let c = Pnt(2., 0., 0.)
                Expect.throws (fun () -> NPlane.createFrom3Points a b c |> ignore) "Should throw for colinear points"
            }

            test "xyPlane is at world origin with Z normal" {
                let plane = NPlane.xyPlane
                Expect.isTrue (eqPnt plane.Origin Pnt.Origin) "Origin should be at world origin"
                Expect.isTrue (eqVec plane.Normal.AsVec (Vec(0., 0., 1.))) "Normal should be Z-axis"
            }
        ]

        testList "NPlane - Distance and Projection" [
            test "DistanceToPt for point above plane" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let pt = Pnt(5., 5., 10.)
                let dist = plane.DistanceToPt pt
                Expect.equal dist 10. "Distance should be 10"
            }

            test "DistanceToPt for point below plane" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let pt = Pnt(5., 5., -10.)
                let dist = plane.DistanceToPt pt
                Expect.equal dist -10. "Distance should be -10 (signed)"
            }

            test "DistanceToPt for point on plane" {
                let plane = NPlane.create(Pnt(0., 0., 5.), Vec(0., 0., 1.))
                let pt = Pnt(10., 10., 5.)
                let dist = plane.DistanceToPt pt
                Expect.isTrue (eqFloat dist 0.0) "Distance should be 0"
            }

            test "ClosestPoint on plane" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let pt = Pnt(5., 5., 10.)
                let closest = plane.ClosestPoint pt
                Expect.isTrue (eqPnt closest (Pnt(5., 5., 0.))) "Closest point should be (5, 5, 0)"
            }

            test "ClosestPoint for point already on plane" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let pt = Pnt(5., 5., 0.)
                let closest = plane.ClosestPoint pt
                Expect.isTrue (eqPnt closest pt) "Closest point should be same as input"
            }

            test "PlaneAtClPt" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let pt = Pnt(5., 5., 10.)
                let newPlane = plane.PlaneAtClPt pt
                Expect.isTrue (eqPnt newPlane.Origin (Pnt(5., 5., 0.))) "New plane origin should be at closest point"
                Expect.isTrue (eqVec newPlane.Normal.AsVec plane.Normal.AsVec) "Normal should be unchanged"
            }
        ]

        testList "NPlane - Angles" [
            test "Angle90ToPlane for parallel planes" {
                let a = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(0., 0., 10.), Vec(0., 0., 1.))
                let angle = a.Angle90ToPlane b
                Expect.isTrue (eqFloat angle 0.0) "Parallel planes should have 0 angle"
            }

            test "Angle90ToPlane for perpendicular planes" {
                let a = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(0., 0., 0.), Vec(1., 0., 0.))
                let angle = a.Angle90ToPlane b
                Expect.isTrue (eqFloat angle 90.0) "Perpendicular planes should have 90 degree angle"
            }

            test "Angle90ToVec for parallel vector" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let vec = Vec(1., 0., 0.)
                let angle = plane.Angle90ToVec vec
                Expect.isTrue (eqFloat angle 0.0) "Parallel vector should have 0 angle"
            }

            test "Angle90ToVec for perpendicular vector" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let vec = Vec(0., 0., 1.)
                let angle = plane.Angle90ToVec vec
                Expect.isTrue (eqFloat angle 90.0) "Perpendicular vector should have 90 degree angle"
            }

            test "Angle90ToLine for parallel line" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(10., 0., 0.))
                let angle = plane.Angle90ToLine line
                Expect.isTrue (eqFloat angle 0.0) "Parallel line should have 0 angle"
            }

            test "Angle90ToLine for perpendicular line" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 10.))
                let angle = plane.Angle90ToLine line
                Expect.isTrue (eqFloat angle 90.0) "Perpendicular line should have 90 degree angle"
            }
        ]

        testList "NPlane - Coincidence" [
            test "IsCoincidentTo for same plane" {
                let a = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                Expect.isTrue (a.IsCoincidentTo b) "Same plane should be coincident"
            }

            test "IsCoincidentTo for parallel planes at same height" {
                let a = NPlane.create(Pnt(0., 0., 5.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(10., 10., 5.), Vec(0., 0., 1.))
                Expect.isTrue (a.IsCoincidentTo b) "Parallel planes at same height should be coincident"
            }

            test "IsCoincidentTo for parallel but separated planes" {
                let a = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(0., 0., 10.), Vec(0., 0., 1.))
                Expect.isFalse (a.IsCoincidentTo b) "Separated parallel planes should not be coincident"
            }

            test "IsCoincidentTo for non-parallel planes" {
                let a = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(0., 0., 0.), Vec(1., 0., 0.))
                Expect.isFalse (a.IsCoincidentTo b) "Non-parallel planes should not be coincident"
            }

            test "areCoincident static method" {
                let a = NPlane.create(Pnt(0., 0., 5.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(10., 10., 5.), Vec(0., 0., 1.))
                Expect.isTrue (NPlane.areCoincident a b) "Static method should work"
            }
        ]

        testList "NPlane - Intersection" [
            test "intersect two planes" {
                let a = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(0., 0., 0.), Vec(0., 1., 0.))
                match NPlane.intersect a b with
                | Some line ->
                    // Line should be along X-axis
                    Expect.isTrue (eqPnt line.From Pnt.Origin) "Intersection line should pass through origin"
                | None -> failwith "Should have intersection"
            }

            test "intersect parallel planes" {
                let a = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(0., 0., 10.), Vec(0., 0., 1.))
                match NPlane.intersect a b with
                | Some _ -> failwith "Parallel planes should not intersect"
                | None -> ()
            }

            test "intersectLineParameter for intersecting line" {
                let plane = NPlane.create(Pnt(0., 0., 5.), Vec(0., 0., 1.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 10.))
                match NPlane.intersectLineParameter line plane with
                | Some t ->
                    Expect.isTrue (eqFloat t 0.5) "Should intersect at t=0.5"
                | None -> failwith "Should have intersection"
            }

            test "intersectLineParameter for parallel line" {
                let plane = NPlane.create(Pnt(0., 0., 5.), Vec(0., 0., 1.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(10., 0., 0.))
                match NPlane.intersectLineParameter line plane with
                | Some _ -> failwith "Parallel line should not intersect"
                | None -> ()
            }

            test "intersectRay returns point" {
                let plane = NPlane.create(Pnt(0., 0., 5.), Vec(0., 0., 1.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 10.))
                match NPlane.intersectRay line plane with
                | Some pt ->
                    Expect.isTrue (eqPnt pt (Pnt(0., 0., 5.))) "Intersection should be at (0, 0, 5)"
                | None -> failwith "Should have intersection"
            }

            test "doLinePlaneIntersect for finite line intersecting" {
                let plane = NPlane.create(Pnt(0., 0., 5.), Vec(0., 0., 1.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 10.))
                Expect.isTrue (NPlane.doLinePlaneIntersect line plane) "Line should intersect plane"
            }

            test "doLinePlaneIntersect for finite line not intersecting" {
                let plane = NPlane.create(Pnt(0., 0., 5.), Vec(0., 0., 1.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 3.))
                Expect.isFalse (NPlane.doLinePlaneIntersect line plane) "Line should not intersect plane"
            }
        ]

        testList "NPlane - Transformation" [
            test "Flipped reverses normal" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let flipped = plane.Flipped
                Expect.isTrue (eqVec flipped.Normal.AsVec (Vec(0., 0., -1.))) "Normal should be flipped"
                Expect.isTrue (eqPnt flipped.Origin plane.Origin) "Origin should be unchanged"
            }

            test "offset along normal" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let offset = NPlane.offset 5. plane
                Expect.isTrue (eqPnt offset.Origin (Pnt(0., 0., 5.))) "Origin should be offset"
                Expect.isTrue (eqVec offset.Normal.AsVec plane.Normal.AsVec) "Normal should be unchanged"
            }

            test "offsetInDir towards point" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let dirPt = Pnt(0., 0., 10.)
                let offset = NPlane.offsetInDir dirPt 5. plane
                Expect.isTrue (eqPnt offset.Origin (Pnt(0., 0., 5.))) "Should offset towards dirPt"
            }

            test "offsetInDir away from point" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let dirPt = Pnt(0., 0., -10.)
                let offset = NPlane.offsetInDir dirPt 5. plane
                Expect.isTrue (eqPnt offset.Origin (Pnt(0., 0., -5.))) "Should offset away from dirPt"
            }

            test "scale from world origin" {
                let plane = NPlane.create(Pnt(1., 2., 3.), Vec(0., 0., 1.))
                let scaled = NPlane.scale 2. plane
                Expect.isTrue (eqPnt scaled.Origin (Pnt(2., 4., 6.))) "Origin should be scaled"
                Expect.isTrue (eqVec scaled.Normal.AsVec plane.Normal.AsVec) "Normal should be unchanged"
            }

            test "move by vector" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let moved = NPlane.move (Vec(5., 5., 5.)) plane
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 5., 5.))) "Origin should be moved"
                Expect.isTrue (eqVec moved.Normal.AsVec plane.Normal.AsVec) "Normal should be unchanged"
            }

            test "translate is alias for move" {
                let plane = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let moved = NPlane.translate (Vec(5., 5., 5.)) plane
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 5., 5.))) "Origin should be moved"
            }
        ]

        testList "NPlane - Equality" [
            test "equals with exact match" {
                let a = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                Expect.isTrue (NPlane.equals 0.0 a b) "Exact planes should be equal"
            }

            test "equals with tolerance" {
                let a = NPlane.create(Pnt(0., 0., 0.), Vec(0., 0., 1.))
                let b = NPlane.create(Pnt(0.001, 0.001, 0.001), Vec(0., 0., 1.))
                Expect.isTrue (NPlane.equals 0.01 a b) "Planes should be equal within tolerance"
                Expect.isFalse (NPlane.equals 0.0001 a b) "Planes should not be equal with small tolerance"
            }
        ]

        testList "PPlane - Construction" [
            test "createOriginXaxisYaxis" {
                let origin = Pnt(0., 0., 0.)
                let xAxis = Vec(1., 0., 0.)
                let yAxis = Vec(0., 1., 0.)
                let plane = PPlane.createOriginXaxisYaxis(origin, xAxis, yAxis)
                Expect.isTrue (eqPnt plane.Origin origin) "Origin should match"
                Expect.isTrue (eqVec plane.Xaxis.AsVec (Vec(1., 0., 0.))) "Xaxis should be unitized"
                Expect.isTrue (eqVec plane.Yaxis.AsVec (Vec(0., 1., 0.))) "Yaxis should be unitized"
            }

            test "createOriginXaxisYaxis rejects zero X-axis" {
                let origin = Pnt(0., 0., 0.)
                Expect.throws (fun () -> PPlane.createOriginXaxisYaxis(origin, Vec.Zero, Vec.Yaxis) |> ignore) "Should throw for zero X-axis"
            }

            test "createOriginXaxisYaxis rejects zero Y-axis" {
                let origin = Pnt(0., 0., 0.)
                Expect.throws (fun () -> PPlane.createOriginXaxisYaxis(origin, Vec.Xaxis, Vec.Zero) |> ignore) "Should throw for zero Y-axis"
            }

            test "createOriginXaxisYaxis rejects parallel axes" {
                let origin = Pnt(0., 0., 0.)
                let xAxis = Vec(1., 0., 0.)
                let yAxis = Vec(2., 0., 0.)
                Expect.throws (fun () -> PPlane.createOriginXaxisYaxis(origin, xAxis, yAxis) |> ignore) "Should throw for parallel axes"
            }

            test "WorldXY plane" {
                let plane = PPlane.WorldXY
                Expect.isTrue (eqPnt plane.Origin Pnt.Origin) "Origin should be at world origin"
                Expect.isTrue (eqVec plane.Xaxis.AsVec (Vec(1., 0., 0.))) "Xaxis should be world X"
                Expect.isTrue (eqVec plane.Yaxis.AsVec (Vec(0., 1., 0.))) "Yaxis should be world Y"
                Expect.isTrue (eqVec plane.Zaxis.AsVec (Vec(0., 0., 1.))) "Zaxis should be world Z"
            }

            test "WorldTop is same as WorldXY" {
                let xy = PPlane.WorldXY
                let top = PPlane.WorldTop
                Expect.isTrue (PPlane.equals 0.0 xy top) "WorldTop should equal WorldXY"
            }
        ]

        testList "PPlane - Distance and Evaluation" [
            test "DistanceToPt for point above plane" {
                let plane = PPlane.WorldXY
                let pt = Pnt(5., 5., 10.)
                let dist = plane.DistanceToPt pt
                Expect.equal dist 10. "Distance should be 10"
            }

            test "EvaluateAt origin (0, 0, 0)" {
                let plane = PPlane.createOriginXaxisYaxis(Pnt(1., 2., 3.), Vec.Xaxis, Vec.Yaxis)
                let pt = plane.EvaluateAt(0., 0., 0.)
                Expect.isTrue (eqPnt pt plane.Origin) "Should return Origin at (0, 0, 0)"
            }

            test "EvaluateAt with positive parameters" {
                let plane = PPlane.WorldXY
                let pt = plane.EvaluateAt(5., 3., 2.)
                Expect.isTrue (eqPnt pt (Pnt(5., 3., 2.))) "Should evaluate correctly"
            }

            test "EvaluateAtXY" {
                let plane = PPlane.WorldXY
                let pt = plane.EvaluateAtXY(5., 3.)
                Expect.isTrue (eqPnt pt (Pnt(5., 3., 0.))) "Should evaluate in XY only"
            }

            test "PointParameters" {
                let plane = PPlane.WorldXY
                let pt = Pnt(5., 3., 2.)
                let x, y, z = plane.PointParameters pt
                Expect.equal x 5. "X parameter should be 5"
                Expect.equal y 3. "Y parameter should be 3"
                Expect.equal z 2. "Z parameter should be 2"
            }
        ]

        testList "PPlane - Angles" [
            test "Angle90ToPlane for parallel planes" {
                let a = PPlane.WorldXY
                let b = PPlane.createOriginXaxisYaxis(Pnt(0., 0., 10.), Vec.Xaxis, Vec.Yaxis)
                let angle = a.Angle90ToPlane b
                Expect.isTrue (eqFloat angle 0.0) "Parallel planes should have 0 angle"
            }

            test "Angle90ToPlane for perpendicular planes" {
                let a = PPlane.WorldXY
                let b = PPlane.WorldFront
                let angle = a.Angle90ToPlane b
                Expect.isTrue (eqFloat angle 90.0) "Perpendicular planes should have 90 degree angle"
            }

            test "Angle90ToVec for parallel vector" {
                let plane = PPlane.WorldXY
                let vec = Vec(1., 0., 0.)
                let angle = plane.Angle90ToVec vec
                Expect.isTrue (eqFloat angle 0.0) "Parallel vector should have 0 angle"
            }

            test "Angle90ToLine" {
                let plane = PPlane.WorldXY
                let line = Line3D(Pnt.Origin, Pnt(10., 0., 0.))
                let angle = plane.Angle90ToLine line
                Expect.isTrue (eqFloat angle 0.0) "Parallel line should have 0 angle"
            }
        ]

        testList "PPlane - Coincidence" [
            test "IsCoincidentTo for same plane" {
                let a = PPlane.WorldXY
                let b = PPlane.WorldXY
                Expect.isTrue (a.IsCoincidentTo b) "Same plane should be coincident"
            }

            test "IsCoincidentTo for coincident but rotated planes" {
                let a = PPlane.createOriginXaxisYaxis(Pnt.Origin, Vec.Xaxis, Vec.Yaxis)
                let b = PPlane.createOriginXaxisYaxis(Pnt(10., 10., 0.), Vec.Xaxis, Vec.Yaxis)
                Expect.isTrue (a.IsCoincidentTo b) "Planes at same height should be coincident"
            }

            test "areCoincident static method" {
                let a = PPlane.WorldXY
                let b = PPlane.createOriginXaxisYaxis(Pnt(10., 10., 0.), Vec.Xaxis, Vec.Yaxis)
                Expect.isTrue (PPlane.areCoincident 1e-6 a b) "Static method should work"
            }
        ]

        testList "PPlane - Transformation" [
            test "scale from world origin" {
                let plane = PPlane.createOriginXaxisYaxis(Pnt(1., 2., 3.), Vec.Xaxis, Vec.Yaxis)
                let scaled = PPlane.scale 2. plane
                Expect.isTrue (eqPnt scaled.Origin (Pnt(2., 4., 6.))) "Origin should be scaled"
                Expect.isTrue (eqVec scaled.Xaxis.AsVec plane.Xaxis.AsVec) "Axes should be unchanged"
            }

            test "move by vector" {
                let plane = PPlane.WorldXY
                let moved = PPlane.move (Vec(5., 5., 5.)) plane
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 5., 5.))) "Origin should be moved"
                Expect.isTrue (eqVec moved.Xaxis.AsVec (UnitVec.Xaxis.AsVec)) "Axes should be unchanged"
            }

            test "translate is alias for move" {
                let plane = PPlane.WorldXY
                let moved = PPlane.translate (Vec(5., 5., 5.)) plane
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 5., 5.))) "Origin should be moved"
            }
        ]

        testList "PPlane - Equality" [
            test "equals with exact match" {
                let a = PPlane.WorldXY
                let b = PPlane.WorldXY
                Expect.isTrue (PPlane.equals 0.0 a b) "Exact planes should be equal"
            }

            test "equals with tolerance" {
                let a = PPlane.WorldXY
                let b = PPlane.createOriginXaxisYaxis(Pnt(0.001, 0.001, 0.001), Vec.Xaxis, Vec.Yaxis)
                Expect.isTrue (PPlane.equals 0.01 a b) "Planes should be equal within tolerance"
                Expect.isFalse (PPlane.equals 0.0001 a b) "Planes should not be equal with small tolerance"
            }

            test "notEquals" {
                let a = PPlane.WorldXY
                let b = PPlane.createOriginXaxisYaxis(Pnt(1., 1., 1.), Vec.Xaxis, Vec.Yaxis)
                Expect.isTrue (PPlane.notEquals 0.5 a b) "Different planes should not be equal"
            }
        ]

        testList "PPlane - Projection and Closest Point" [
            test "ClosestPoint on plane" {
                let plane = PPlane.WorldXY
                let pt = Pnt(5., 5., 10.)
                let closest = plane.ClosestPoint pt
                Expect.isTrue (eqPnt closest (Pnt(5., 5., 0.))) "Closest point should be (5, 5, 0)"
            }

            test "PlaneAtClPt" {
                let plane = PPlane.WorldXY
                let pt = Pnt(5., 5., 10.)
                let newPlane = plane.PlaneAtClPt pt
                Expect.isTrue (eqPnt newPlane.Origin (Pnt(5., 5., 0.))) "New plane origin should be at closest point"
            }
        ]
    ]
