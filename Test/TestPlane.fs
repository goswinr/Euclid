module TestPlane

open Euclid

open EuclidTestSupport

let inline eqPnt a b = Pnt.distanceTo a b < 1e-9
let inline eqVec a b = Vec.length (a - b) < 1e-9
let inline eqFloat a b = abs(a - b) < 1e-9

let tests =
    testList "Planes" [

        testList "NPlane - Construction" [
            test "create from point and vector" {
                let origin = Pnt(0., 0., 0.)
                let normal = Vec(0., 0., 1.)
                let plane = NPlane.createFromNormal normal origin
                Expect.isTrue (eqPnt plane.Origin origin) "Origin should match"
                Expect.isTrue (eqVec plane.Normal.AsVec (Vec(0., 0., 1.))) "Normal should be unitized"
            }

            test "create unitizes the normal vector" {
                let origin = Pnt(0., 0., 0.)
                let normal = Vec(0., 0., 10.)
                let plane = NPlane.createFromNormal normal origin
                Expect.isTrue (eqFloat (Vec.length plane.Normal.AsVec) 1.0) "Normal should be unit length"
            }

            test "create rejects zero-length normal" {
                let origin = Pnt(0., 0., 0.)
                let normal = Vec.Zero
                Expect.throws (fun () -> NPlane.createFromNormal normal origin |> ignore) "Should throw for zero normal"
            }

            test "create from UnitVec" {
                let origin = Pnt(1., 2., 3.)
                let normal = UnitVec.ZAxis
                let plane = NPlane.createFromUnitNormal normal origin
                Expect.isTrue (eqPnt plane.Origin origin) "Origin should match"
                Expect.isTrue (eqVec plane.Normal.AsVec UnitVec.ZAxis.AsVec) "Normal should match"
            }

            test "createFrom3Points" {
                let a = Pnt(0., 0., 0.)
                let b = Pnt(1., 0., 0.)
                let c = Pnt(0., 1., 0.)
                let plane = NPlane.createFrom3Pnts a b c
                Expect.isTrue (eqPnt plane.Origin a) "Origin should be at first point"
                // Normal should be perpendicular to both (b-a) and (c-a)
                let normal = plane.Normal
                Expect.isTrue (eqFloat (normal *** (b-a)) 0.0) "Normal should be perpendicular to edge 1"
                Expect.isTrue (eqFloat (normal *** (c-a)) 0.0) "Normal should be perpendicular to edge 2"
            }

            test "createFrom3Points rejects collinear points" {
                let a = Pnt(0., 0., 0.)
                let b = Pnt(1., 0., 0.)
                let c = Pnt(2., 0., 0.)
                Expect.throws (fun () -> NPlane.createFrom3Pnts a b c |> ignore) "Should throw for collinear points"
            }

            test "WorldXY is at world origin with Z normal" {
                let plane = NPlane.WorldXY
                Expect.isTrue (eqPnt plane.Origin Pnt.Origin) "Origin should be at world origin"
                Expect.isTrue (eqVec plane.Normal.AsVec (Vec(0., 0., 1.))) "Normal should be Z-axis"
            }
        ]

        testList "NPlane - Distance and Projection" [
            test "DistanceToPnt for point above plane" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let pt = Pnt(5., 5., 10.)
                let dist = plane.DistanceToPnt pt
                Expect.equal dist 10. "Distance should be 10"
            }

            test "DistanceToPnt for point below plane returns absolute distance" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let pt = Pnt(5., 5., -10.)
                let dist = plane.DistanceToPnt pt
                Expect.equal dist 10. "Distance should be 10"
            }

            test "DistanceToPntSigned for point below plane" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let pt = Pnt(5., 5., -10.)
                let dist = plane.DistanceToPntSigned pt
                Expect.equal dist -10. "Signed distance should be -10"
            }

            test "DistanceToPnt for point on plane" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 5.))
                let pt = Pnt(10., 10., 5.)
                let dist = plane.DistanceToPnt pt
                Expect.isTrue (eqFloat dist 0.0) "Distance should be 0"
            }

            test "ClosestPoint on plane" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let pt = Pnt(5., 5., 10.)
                let closest = plane.ClosestPoint pt
                Expect.isTrue (eqPnt closest (Pnt(5., 5., 0.))) "Closest point should be (5, 5, 0)"
            }

            test "ClosestPoint for point already on plane" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let pt = Pnt(5., 5., 0.)
                let closest = plane.ClosestPoint pt
                Expect.isTrue (eqPnt closest pt) "Closest point should be same as input"
            }

            test "PlaneAtClosestPnt" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let pt = Pnt(5., 5., 10.)
                let newPlane = plane.PlaneAtClosestPnt pt
                Expect.isTrue (eqPnt newPlane.Origin (Pnt(5., 5., 0.))) "New plane origin should be at closest point"
                Expect.isTrue (eqVec newPlane.Normal.AsVec plane.Normal.AsVec) "Normal should be unchanged"
            }
        ]

        testList "NPlane - Angles" [
            test "Angle90ToPlane for parallel planes" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 10.))
                let angle = a.Angle90ToPlane b
                Expect.isTrue (eqFloat angle 0.0) "Parallel planes should have 0 angle"
            }

            test "Angle90ToPlane for perpendicular planes" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(1., 0., 0.)) (Pnt(0., 0., 0.))
                let angle = a.Angle90ToPlane b
                Expect.isTrue (eqFloat angle 90.0) "Perpendicular planes should have 90 degree angle"
            }

            test "Angle90ToVec for parallel vector" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let vec = Vec(1., 0., 0.)
                let angle = plane.Angle90ToVec vec
                Expect.isTrue (eqFloat angle 0.0) "Parallel vector should have 0 angle"
            }

            test "Angle90ToVec for perpendicular vector" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let vec = Vec(0., 0., 1.)
                let angle = plane.Angle90ToVec vec
                Expect.isTrue (eqFloat angle 90.0) "Perpendicular vector should have 90 degree angle"
            }

            test "Angle90ToLine for parallel line" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(10., 0., 0.))
                let angle = plane.Angle90ToLine line
                Expect.isTrue (eqFloat angle 0.0) "Parallel line should have 0 angle"
            }

            test "Angle90ToLine for perpendicular line" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 10.))
                let angle = plane.Angle90ToLine line
                Expect.isTrue (eqFloat angle 90.0) "Perpendicular line should have 90 degree angle"
            }
        ]

        testList "NPlane - Coincidence" [
            test "IsCoincidentTo for same plane" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                Expect.isTrue (a.IsCoincidentTo b) "Same plane should be coincident"
            }

            test "IsCoincidentTo for parallel planes at same height" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 5.))
                let b = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(10., 10., 5.))
                Expect.isTrue (a.IsCoincidentTo b) "Parallel planes at same height should be coincident"
            }

            test "IsCoincidentTo for parallel but separated planes" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 10.))
                Expect.isFalse (a.IsCoincidentTo b) "Separated parallel planes should not be coincident"
            }

            test "IsCoincidentTo for parallel planes separated on negative side" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., -10.))
                Expect.isFalse (a.IsCoincidentTo b) "Separated parallel planes below should not be coincident"
                Expect.isFalse (NPlane.isCoincidentTo 1e-6 Cosine.``0.25`` a b) "Static coincidence check should not treat negative signed distance as coincident"
            }

            test "IsCoincidentTo for non-parallel planes" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(1., 0., 0.)) (Pnt(0., 0., 0.))
                Expect.isFalse (a.IsCoincidentTo b) "Non-parallel planes should not be coincident"
            }

            test "isCoincidentTo static method" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 5.))
                let b = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(10., 10., 5.))
                Expect.isTrue (NPlane.isCoincidentTo 1e-6 Cosine.``0.25`` a b) "Static method should work"
            }
        ]

        testList "NPlane - Intersection" [
            test "intersect two planes" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(0., 1., 0.)) (Pnt(0., 0., 0.))
                match NPlane.tryIntersect a b with
                | ValueSome line ->
                    // Line should be along X-axis
                    Expect.isTrue (eqPnt line.From Pnt.Origin) "Intersection line should pass through origin"
                | ValueNone -> failwith "Should have intersection"
            }

            test "intersect two planes random example" {
                let a = NPlane.createFromNormal (Vec(870., 0., 1.)) (Pnt(83., 1., 40))
                let b = NPlane.createFromNormal (Vec(90., 1., -68.)) (Pnt(-6., -8., 9.))
                match NPlane.tryIntersect a b with
                | ValueSome line ->
                    Expect.isTrue (a.DistanceToPnt line.From < 1e-9) "Intersection line should be on plane A"
                    Expect.isTrue (b.DistanceToPnt line.From < 1e-9) "Intersection line should be on plane B"
                    Expect.isTrue (a.DistanceToPnt line.To < 1e-9) "Intersection line should be on plane A"
                    Expect.isTrue (b.DistanceToPnt line.To < 1e-9) "Intersection line should be on plane B"

                | ValueNone -> failwith "Should have intersection"
            }

            test "PPlane intersect two planes random example" {
                let a = PPlane.createFromNormal (Vec(870., 0., 1.)) (Pnt(83., 1., 40))
                let b = PPlane.createFromNormal (Vec(90., 1., -68.)) (Pnt(-6., -8., 9.))
                match PPlane.tryIntersect a b with
                | ValueSome line ->
                    Expect.isTrue (a.DistanceToPnt line.From < 1e-9) "Intersection line should be on plane A"
                    Expect.isTrue (b.DistanceToPnt line.From < 1e-9) "Intersection line should be on plane B"
                    Expect.isTrue (a.DistanceToPnt line.To < 1e-9) "Intersection line should be on plane A"
                    Expect.isTrue (b.DistanceToPnt line.To < 1e-9) "Intersection line should be on plane B"

                | ValueNone -> failwith "Should have intersection"
            }



            test "intersect parallel planes" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 10.))
                match NPlane.tryIntersect a b with
                | ValueSome _ -> failwith "Parallel planes should not intersect"
                | ValueNone -> ()
            }

            test "intersectRay for intersecting line" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 5.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 10.))
                match NPlane.tryIntersectRay line plane with
                | ValueSome t ->
                    Expect.isTrue (eqFloat t 0.5) "Should intersect at t=0.5"
                | ValueNone -> failwith "Should have intersection"
            }

            test "intersectRay for parallel line" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 5.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(10., 0., 0.))
                match NPlane.tryIntersectRay line plane with
                | ValueSome _ -> failwith "Parallel line should not intersect"
                | ValueNone -> ()
            }

            test "intersectLine returns point" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 5.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 10.))
                match NPlane.tryIntersectLine line plane with
                | ValueSome pt ->
                    Expect.isTrue (eqPnt pt (Pnt(0., 0., 5.))) "Intersection should be at (0, 0, 5)"
                | ValueNone -> failwith "Should have intersection"
            }

            test "doLinePlaneIntersect for finite line intersecting" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 5.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 10.))
                Expect.isTrue (NPlane.doLinePlaneIntersect line plane) "Line should intersect plane"
            }

            test "doLinePlaneIntersect for finite line not intersecting" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 5.))
                let line = Line3D(Pnt(0., 0., 0.), Pnt(0., 0., 3.))
                Expect.isFalse (NPlane.doLinePlaneIntersect line plane) "Line should not intersect plane"
            }
        ]

        testList "NPlane - Transformation" [
            test "Flipped reverses normal" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let flipped = plane.Flipped
                Expect.isTrue (eqVec flipped.Normal.AsVec (Vec(0., 0., -1.))) "Normal should be flipped"
                Expect.isTrue (eqPnt flipped.Origin plane.Origin) "Origin should be unchanged"
            }

            test "offset along normal" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let offset = NPlane.offset 5. plane
                Expect.isTrue (eqPnt offset.Origin (Pnt(0., 0., 5.))) "Origin should be offset"
                Expect.isTrue (eqVec offset.Normal.AsVec plane.Normal.AsVec) "Normal should be unchanged"
            }

            test "offsetInDirection towards point" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let dirPt = Pnt(0., 0., 10.)
                let offset = NPlane.offsetInDirection dirPt 5. plane
                Expect.isTrue (eqPnt offset.Origin (Pnt(0., 0., 5.))) "Should offset towards dirPt"
            }

            test "offsetInDirection away from point" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let dirPt = Pnt(0., 0., -10.)
                let offset = NPlane.offsetInDirection dirPt 5. plane
                Expect.isTrue (eqPnt offset.Origin (Pnt(0., 0., -5.))) "Should offset away from dirPt"
            }

            test "scale from world origin" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(1., 2., 3.))
                let scaled = NPlane.scale 2. plane
                Expect.isTrue (eqPnt scaled.Origin (Pnt(2., 4., 6.))) "Origin should be scaled"
                Expect.isTrue (eqVec scaled.Normal.AsVec plane.Normal.AsVec) "Normal should be unchanged"
            }

            test "move by vector" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let moved = NPlane.move (Vec(5., 5., 5.)) plane
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 5., 5.))) "Origin should be moved"
                Expect.isTrue (eqVec moved.Normal.AsVec plane.Normal.AsVec) "Normal should be unchanged"
            }

            test "translate is alias for move" {
                let plane = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let moved = NPlane.move (Vec(5., 5., 5.)) plane
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 5., 5.))) "Origin should be moved"
            }
        ]

        testList "NPlane - Equality" [
            test "equals with exact match" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                Expect.isTrue (NPlane.equalsExact a b) "Exact planes should be equal"
            }

            test "equals with tolerance" {
                let a = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0., 0., 0.))
                let b = NPlane.createFromNormal (Vec(0., 0., 1.)) (Pnt(0.001, 0.001, 0.001))
                Expect.isTrue (NPlane.equalsWithin 0.01 a b) "Planes should be equal within tolerance"
                Expect.isFalse (NPlane.equalsWithin 0.0001 a b) "Planes should not be equal with small tolerance"
            }
        ]

        testList "PPlane - Construction" [
            test "createOriginXaxisYaxis" {
                let origin = Pnt(0., 0., 0.)
                let xAxis = Vec(1., 0., 0.)
                let yAxis = Vec(0., 1., 0.)
                let plane = PPlane.createFromXAxisAndYAxis xAxis yAxis origin
                Expect.isTrue (eqPnt plane.Origin origin) "Origin should match"
                Expect.isTrue (eqVec plane.XAxis.AsVec (Vec(1., 0., 0.))) "Xaxis should be unitized"
                Expect.isTrue (eqVec plane.YAxis.AsVec (Vec(0., 1., 0.))) "Yaxis should be unitized"
            }

            test "createOriginXaxisYaxis rejects zero X-axis" {
                let origin = Pnt(0., 0., 0.)
                Expect.throws (fun () -> PPlane.createFromXAxisAndYAxis Vec.Zero Vec.YAxis origin |> ignore) "Should throw for zero X-axis"
            }

            test "createOriginXaxisYaxis rejects zero Y-axis" {
                let origin = Pnt(0., 0., 0.)
                Expect.throws (fun () -> PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.Zero origin |> ignore) "Should throw for zero Y-axis"
            }

            test "createOriginXaxisYaxis rejects parallel axes" {
                let origin = Pnt(0., 0., 0.)
                let xAxis = Vec(1., 0., 0.)
                let yAxis = Vec(2., 0., 0.)
                Expect.throws (fun () -> PPlane.createFromXAxisAndYAxis xAxis yAxis origin |> ignore) "Should throw for parallel axes"
            }

            test "WorldXY plane" {
                let plane = PPlane.WorldXY
                Expect.isTrue (eqPnt plane.Origin Pnt.Origin) "Origin should be at world origin"
                Expect.isTrue (eqVec plane.XAxis.AsVec (Vec(1., 0., 0.))) "Xaxis should be world X"
                Expect.isTrue (eqVec plane.YAxis.AsVec (Vec(0., 1., 0.))) "Yaxis should be world Y"
                Expect.isTrue (eqVec plane.ZAxis.AsVec (Vec(0., 0., 1.))) "Zaxis should be world Z"
            }

            test "WorldTop is same as WorldXY" {
                let xy = PPlane.WorldXY
                let top = PPlane.WorldTop
                Expect.isTrue (PPlane.equalsExact xy top) "WorldTop should equal WorldXY"
            }
        ]

        testList "PPlane - Distance and Evaluation" [
            test "DistanceToPnt for point above plane" {
                let plane = PPlane.WorldXY
                let pt = Pnt(5., 5., 10.)
                let dist = plane.DistanceToPnt pt
                Expect.equal dist 10. "Distance should be 10"
            }

            test "DistanceToPnt for point below plane returns absolute distance" {
                let plane = PPlane.WorldXY
                let pt = Pnt(5., 5., -10.)
                let dist = plane.DistanceToPnt pt
                Expect.equal dist 10. "Distance should be 10"
            }

            test "DistanceToPntSigned for point below plane" {
                let plane = PPlane.WorldXY
                let pt = Pnt(5., 5., -10.)
                let dist = plane.DistanceToPntSigned pt
                Expect.equal dist -10. "Signed distance should be -10"
            }

            test "EvaluateAt origin (0, 0, 0)" {
                let plane = PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.YAxis (Pnt(1., 2., 3.))
                let pt = plane.EvaluateAtXYZ(0., 0., 0.)
                Expect.isTrue (eqPnt pt plane.Origin) "Should return Origin at (0, 0, 0)"
            }

            test "EvaluateAt with positive parameters" {
                let plane = PPlane.WorldXY
                let pt = plane.EvaluateAtXYZ(5., 3., 2.)
                Expect.isTrue (eqPnt pt (Pnt(5., 3., 2.))) "Should evaluate correctly"
            }

            test "EvaluateAtXY" {
                let plane = PPlane.WorldXY
                let pt = plane.EvaluateAt(5., 3.)
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
                let b = PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.YAxis (Pnt(0., 0., 10.))
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
                let a = PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.YAxis Pnt.Origin
                let b = PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.YAxis (Pnt(10., 10., 0.))
                Expect.isTrue (a.IsCoincidentTo b) "Planes at same height should be coincident"
            }

            test "isCoincidentTo static method" {
                let a = PPlane.WorldXY
                let b = PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.YAxis (Pnt(10., 10., 0.))
                Expect.isTrue (PPlane.isCoincidentTo 1e-6 Cosine.``0.1`` a b) "Static method should work"
            }

            test "IsCoincidentTo for parallel planes separated on negative side" {
                let a = PPlane.WorldXY
                let b = PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.YAxis (Pnt(0., 0., -10.))
                Expect.isFalse (a.IsCoincidentTo b) "Separated parallel planes below should not be coincident"
                Expect.isFalse (PPlane.isCoincidentTo 1e-6 Cosine.``0.25`` a b) "Static coincidence check should not treat negative signed distance as coincident"
            }
        ]

        testList "PPlane - Transformation" [
            test "scale from world origin" {
                let plane = PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.YAxis (Pnt(1., 2., 3.))
                let scaled = PPlane.scale 2. plane
                Expect.isTrue (eqPnt scaled.Origin (Pnt(2., 4., 6.))) "Origin should be scaled"
                Expect.isTrue (eqVec scaled.XAxis.AsVec plane.XAxis.AsVec) "Axes should be unchanged"
            }

            test "move by vector" {
                let plane = PPlane.WorldXY
                let moved = PPlane.move (Vec(5., 5., 5.)) plane
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 5., 5.))) "Origin should be moved"
                Expect.isTrue (eqVec moved.XAxis.AsVec (UnitVec.XAxis.AsVec)) "Axes should be unchanged"
            }

        ]

        testList "PPlane - Equality" [
            test "equals with exact match" {
                let a = PPlane.WorldXY
                let b = PPlane.WorldXY
                Expect.isTrue (PPlane.equalsExact a b) "Exact planes should be equal"
            }

            test "equals with tolerance" {
                let a = PPlane.WorldXY
                let b = PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.YAxis (Pnt(0.001, 0.001, 0.001))
                Expect.isTrue (PPlane.equalsWithin 0.01 a b) "Planes should be equal within tolerance"
                Expect.isFalse (PPlane.equalsWithin 0.0001 a b) "Planes should not be equal with small tolerance"
            }

            test "notEquals" {
                let a = PPlane.WorldXY
                let b = PPlane.createFromXAxisAndYAxis Vec.XAxis Vec.YAxis (Pnt(1., 1., 1.))
                Expect.isTrue (PPlane.notEqualsWithin 0.5 a b) "Different planes should not be equal"
            }
        ]

        testList "PPlane - WithOrigin" [
            test "withOrigin moves the origin and keeps the axes" {
                let pl = PPlane.WorldXY
                let moved = PPlane.withOrigin (Pnt(1., 2., 3.)) pl
                Expect.isTrue (eqPnt moved.Origin (Pnt(1., 2., 3.))) "Origin should be the given point"
                Expect.isTrue (eqVec moved.XAxis.AsVec pl.XAxis.AsVec) "Xaxis should be unchanged"
                Expect.isTrue (eqVec moved.YAxis.AsVec pl.YAxis.AsVec) "Yaxis should be unchanged"
                Expect.isTrue (eqVec moved.ZAxis.AsVec pl.ZAxis.AsVec) "Zaxis should be unchanged"
                Expect.isTrue (eqPnt pl.Origin Pnt.Origin) "Input plane should be unchanged"
            }

            test "withOriginX changes only the X value of the origin" {
                let pl = PPlane.withOrigin (Pnt(1., 2., 3.)) PPlane.WorldXY
                let moved = PPlane.withOriginX 9. pl
                Expect.isTrue (eqPnt moved.Origin (Pnt(9., 2., 3.))) "Only X should change"
            }

            test "withOriginY changes only the Y value of the origin" {
                let pl = PPlane.withOrigin (Pnt(1., 2., 3.)) PPlane.WorldXY
                let moved = PPlane.withOriginY 9. pl
                Expect.isTrue (eqPnt moved.Origin (Pnt(1., 9., 3.))) "Only Y should change"
            }

            test "withOriginZ changes only the Z value of the origin" {
                let pl = PPlane.withOrigin (Pnt(1., 2., 3.)) PPlane.WorldXY
                let moved = PPlane.withOriginZ 9. pl
                Expect.isTrue (eqPnt moved.Origin (Pnt(1., 2., 9.))) "Only Z should change"
            }

            test "withOrigin on a rotated plane keeps the rotated axes" {
                let pl = PPlane.createFromXAxisAndYAxis (Vec(0., 1., 0.)) (Vec(-1., 0., 0.)) (Pnt(5., 5., 5.))
                let moved = PPlane.withOrigin Pnt.Origin pl
                Expect.isTrue (eqPnt moved.Origin Pnt.Origin) "Origin should be at world origin"
                Expect.isTrue (eqVec moved.XAxis.AsVec (Vec(0., 1., 0.))) "Xaxis should stay rotated"
            }
        ]

        testList "PPlane - Projection and Closest Point" [
            test "ClosestPoint on plane" {
                let plane = PPlane.WorldXY
                let pt = Pnt(5., 5., 10.)
                let closest = plane.ClosestPoint pt
                Expect.isTrue (eqPnt closest (Pnt(5., 5., 0.))) "Closest point should be (5, 5, 0)"
            }

            test "PlaneAtClosestPnt" {
                let plane = PPlane.WorldXY
                let pt = Pnt(5., 5., 10.)
                let newPlane = plane.PlaneAtClosestPnt pt
                Expect.isTrue (eqPnt newPlane.Origin (Pnt(5., 5., 0.))) "New plane origin should be at closest point"
            }
        ]

        testList "PPlane - move, scale, rotate and transform grammar" [
            // a plane away from the origin and not aligned with the world axes
            let pl = PPlane.createFromXAxisAndYAxis (Vec(2., 1., 0.5)) (Vec(-1., 2., 0.3)) (Pnt(13., 27., 39.))
            let cen = Pnt(4., -7., 11.)
            let q = Quaternion.createFromDegreesVec (Vec(1., 2., 3.)) 37.0
            let eqPl (a:PPlane) (b:PPlane) = PPlane.equalsWithin 1e-9 a b

            test "Move, MoveX, MoveY and MoveZ only change the origin" {
                let v = Vec(1.5, -2.5, 3.5)
                let moved = pl.Move v
                Expect.isTrue (eqPnt moved.Origin (pl.Origin + v)) "Move"
                Expect.isTrue (eqVec moved.XAxis.AsVec pl.XAxis.AsVec && eqVec moved.YAxis.AsVec pl.YAxis.AsVec && eqVec moved.ZAxis.AsVec pl.ZAxis.AsVec) "axes unchanged"
                Expect.isTrue (eqPl moved (PPlane.move v pl)) "static move"
                Expect.isTrue (eqPnt (pl.MoveX 2.).Origin (Pnt(15., 27., 39.))) "MoveX"
                Expect.isTrue (eqPnt (pl.MoveY 2.).Origin (Pnt(13., 29., 39.))) "MoveY"
                Expect.isTrue (eqPnt (pl.MoveZ 2.).Origin (Pnt(13., 27., 41.))) "MoveZ"
                Expect.isTrue (eqPl (pl.MoveX 2.) (PPlane.moveX 2. pl)) "static moveX"
                Expect.isTrue (eqPl (pl.MoveY 2.) (PPlane.moveY 2. pl)) "static moveY"
                Expect.isTrue (eqPl (pl.MoveZ 2.) (PPlane.moveZ 2. pl)) "static moveZ"
            }

            test "Scale and ScaleOn only change the origin" {
                Expect.isTrue (eqPl (pl.Scale 1.5) (PPlane.scale 1.5 pl)) "Scale matches static"
                let scaled = pl.ScaleOn cen 1.5
                Expect.isTrue (eqPnt scaled.Origin (Pnt.scaleOn cen 1.5 pl.Origin)) "ScaleOn origin"
                Expect.isTrue (eqVec scaled.XAxis.AsVec pl.XAxis.AsVec && eqVec scaled.YAxis.AsVec pl.YAxis.AsVec) "axes unchanged"
                Expect.isTrue (eqPl scaled (PPlane.scaleOn cen 1.5 pl)) "static scaleOn"
                Expect.isTrue (eqPl (pl.ScaleOn pl.Origin 7.) pl) "scaling on its own origin changes nothing"
            }

            test "Rotate and RotateAround rotate the origin and all axes" {
                let r = pl.RotateAround cen q
                Expect.isTrue (eqPnt r.Origin (Pnt.rotateAround cen q pl.Origin)) "origin"
                Expect.isTrue (eqVec r.XAxis.AsVec (UnitVec.rotate q pl.XAxis).AsVec) "X axis"
                Expect.isTrue (eqVec r.YAxis.AsVec (UnitVec.rotate q pl.YAxis).AsVec) "Y axis"
                Expect.isTrue (eqVec r.ZAxis.AsVec (UnitVec.rotate q pl.ZAxis).AsVec) "Z axis"
                Expect.isTrue (eqPl r (PPlane.rotateAround cen q pl)) "static rotateAround"
                let r0 = pl.Rotate q
                Expect.isTrue (eqPnt r0.Origin (Pnt.rotate q pl.Origin)) "Rotate origin"
                Expect.isTrue (eqPl r0 (pl.RotateAround Pnt.Origin q)) "Rotate is RotateAround the world origin"
                Expect.isTrue (eqPl r0 (PPlane.rotate q pl)) "static rotate"
                Expect.isTrue (eqPl (pl.Rotate Quaternion.Identity) pl) "identity rotation"
                // a point given in plane parameters keeps its parameters
                let p = pl.EvaluateAt(3., -2.)
                Expect.isTrue (eqPnt (r.EvaluateAt(3., -2.)) (Pnt.rotateAround cen q p)) "points on the plane rotate with it"
            }

            test "TransformRigid matches the static member and a rotation followed by a move" {
                let v = Vec(5., -6., 7.)
                let m = RigidMatrix.createFromTranslationAndQuaternion q v
                let t = pl.TransformRigid m
                Expect.isTrue (eqPl t (PPlane.transformRigid m pl)) "static transformRigid"
                Expect.isTrue (eqPl t ((pl.Rotate q).Move v)) "rotate then move"
            }

            test "rotateOnLocalZDegrees and rotateOnLocalZRadians" {
                let r = PPlane.rotateOnLocalZDegrees 90. pl
                Expect.isTrue (eqPnt r.Origin pl.Origin) "origin stays"
                Expect.isTrue (eqVec r.XAxis.AsVec pl.YAxis.AsVec) "X axis turns to the old Y axis"
                Expect.isTrue (eqVec r.YAxis.AsVec (-pl.XAxis.AsVec)) "Y axis turns to the reversed old X axis"
                Expect.isTrue (eqVec r.ZAxis.AsVec pl.ZAxis.AsVec) "Z axis stays"
                Expect.isTrue (eqPl r (PPlane.rotateOnLocalZRadians (System.Math.PI * 0.5) pl)) "radians partner"
                Expect.isTrue (eqPl (PPlane.rotateOnLocalZDegrees 0. pl) pl) "zero angle"
                Expect.isTrue (eqPl (PPlane.rotateOnLocalZDegrees 180. pl) (PPlane.rotateOnLocalZ180 pl)) "180 degrees"
                // the same as a rotation around the Z axis through the origin of the plane
                let viaQuat = pl.RotateAround pl.Origin (Quaternion.createFromDegreesUnitVec pl.ZAxis 33.)
                Expect.isTrue (eqPl (PPlane.rotateOnLocalZDegrees 33. pl) viaQuat) "matches a quaternion rotation about the local Z axis"
            }
        ]

        testList "NPlane - move, scale, rotate and transform grammar" [
            // a plane away from the origin and not aligned with the world axes
            let pl = NPlane.createFromNormal (Vec(1., -2., 3.)) (Pnt(13., 27., 39.))
            let cen = Pnt(4., -7., 11.)
            let q = Quaternion.createFromDegreesVec (Vec(1., 2., 3.)) 37.0
            let eqPl (a:NPlane) (b:NPlane) = NPlane.equalsWithin 1e-9 a b

            test "Move, MoveX, MoveY and MoveZ only change the origin" {
                let v = Vec(1.5, -2.5, 3.5)
                let moved = pl.Move v
                Expect.isTrue (eqPnt moved.Origin (pl.Origin + v)) "Move"
                Expect.isTrue (eqVec moved.Normal.AsVec pl.Normal.AsVec) "normal unchanged"
                Expect.isTrue (eqPl moved (NPlane.move v pl)) "static move"
                Expect.isTrue (eqPnt (pl.MoveX 2.).Origin (Pnt(15., 27., 39.))) "MoveX"
                Expect.isTrue (eqPnt (pl.MoveY 2.).Origin (Pnt(13., 29., 39.))) "MoveY"
                Expect.isTrue (eqPnt (pl.MoveZ 2.).Origin (Pnt(13., 27., 41.))) "MoveZ"
                Expect.isTrue (eqPl (pl.MoveX 2.) (NPlane.moveX 2. pl)) "static moveX"
                Expect.isTrue (eqPl (pl.MoveY 2.) (NPlane.moveY 2. pl)) "static moveY"
                Expect.isTrue (eqPl (pl.MoveZ 2.) (NPlane.moveZ 2. pl)) "static moveZ"
            }

            test "Scale and ScaleOn only change the origin" {
                Expect.isTrue (eqPl (pl.Scale 1.5) (NPlane.scale 1.5 pl)) "Scale matches static"
                let scaled = pl.ScaleOn cen 1.5
                Expect.isTrue (eqPnt scaled.Origin (Pnt.scaleOn cen 1.5 pl.Origin)) "ScaleOn origin"
                Expect.isTrue (eqVec scaled.Normal.AsVec pl.Normal.AsVec) "normal unchanged"
                Expect.isTrue (eqPl scaled (NPlane.scaleOn cen 1.5 pl)) "static scaleOn"
            }

            test "Rotate and RotateAround rotate the origin and the normal" {
                let r = pl.RotateAround cen q
                Expect.isTrue (eqPnt r.Origin (Pnt.rotateAround cen q pl.Origin)) "origin"
                Expect.isTrue (eqVec r.Normal.AsVec (UnitVec.rotate q pl.Normal).AsVec) "normal"
                Expect.isTrue (eqPl r (NPlane.rotateAround cen q pl)) "static rotateAround"
                let r0 = pl.Rotate q
                Expect.isTrue (eqPnt r0.Origin (Pnt.rotate q pl.Origin)) "Rotate origin"
                Expect.isTrue (eqPl r0 (pl.RotateAround Pnt.Origin q)) "Rotate is RotateAround the world origin"
                Expect.isTrue (eqPl r0 (NPlane.rotate q pl)) "static rotate"
                Expect.isTrue (eqPl (pl.Rotate Quaternion.Identity) pl) "identity rotation"
                // a point that is on the plane stays on the rotated plane
                let p = pl.ClosestPoint (Pnt(20., 20., 20.))
                Expect.isTrue (eqFloat (r.DistanceToPnt (Pnt.rotateAround cen q p)) 0.0) "points on the plane rotate with it"
            }

            test "the NPlane of a rotated PPlane is the rotated NPlane" {
                let pp = PPlane.createFromXAxisAndYAxis (Vec(2., 1., 0.5)) (Vec(-1., 2., 0.3)) (Pnt(13., 27., 39.))
                let np = NPlane.createFromPPlane pp
                Expect.isTrue (eqPl (NPlane.createFromPPlane (pp.RotateAround cen q)) (np.RotateAround cen q)) "RotateAround"
                let m = RigidMatrix.createFromTranslationAndQuaternion q (Vec(5., -6., 7.))
                Expect.isTrue (eqPl (NPlane.createFromPPlane (pp.TransformRigid m)) (np.TransformRigid m)) "TransformRigid"
            }

            test "TransformRigid matches the static member and a rotation followed by a move" {
                let v = Vec(5., -6., 7.)
                let m = RigidMatrix.createFromTranslationAndQuaternion q v
                let t = pl.TransformRigid m
                Expect.isTrue (eqPl t (NPlane.transformRigid m pl)) "static transformRigid"
                Expect.isTrue (eqPl t ((pl.Rotate q).Move v)) "rotate then move"
                Expect.isTrue (eqFloat (Vec.length t.Normal.AsVec) 1.0) "normal stays unitized"
            }

            test "static isCoincidentTo takes both tolerances" {
                let flippedAndShifted = (pl.Move (pl.Normal.AsVec * 0.01)).Flipped
                Expect.isTrue  (NPlane.isCoincidentTo 0.1 Cosine.``0.25`` pl flippedAndShifted) "within the distance tolerance, normal orientation is ignored"
                Expect.isFalse (NPlane.isCoincidentTo 0.001 Cosine.``0.25`` pl flippedAndShifted) "outside the distance tolerance"
                let tilted = pl.RotateAround pl.Origin (Quaternion.createFromDegreesVec (Vec(3., 0., -1.)) 1.0) // the axis is in the plane
                Expect.isFalse (NPlane.isCoincidentTo 0.1 Cosine.``0.25`` pl tilted) "tilted by 1 degree, angle tolerance 0.25 degrees"
                Expect.isTrue  (NPlane.isCoincidentTo 0.1 Cosine.``5.0`` pl tilted) "tilted by 1 degree, angle tolerance 5 degrees"
                Expect.equal (NPlane.isCoincidentTo 1e-6 Cosine.``0.25`` pl flippedAndShifted) (pl.IsCoincidentTo flippedAndShifted) "default tolerances of the instance member"
            }
        ]
    ]
