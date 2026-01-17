module TestBox

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
    testList "Box" [

        testList "Constructor and Basic Properties" [
            test "createUnchecked from origin and axes" {
                let origin = Pnt(0., 0., 0.)
                let xAxis = Vec(10., 0., 0.)
                let yAxis = Vec(0., 5., 0.)
                let zAxis = Vec(0., 0., 3.)
                let box = Box.createUnchecked(origin, xAxis, yAxis, zAxis)
                Expect.isTrue (eqPnt box.Origin origin) "Origin should match"
                Expect.isTrue (eqVec box.Xaxis xAxis) "Xaxis should match"
                Expect.isTrue (eqVec box.Yaxis yAxis) "Yaxis should match"
                Expect.isTrue (eqVec box.Zaxis zAxis) "Zaxis should match"
            }

            test "SizeX, SizeY, SizeZ" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.equal box.SizeX 10. "SizeX should be 10"
                Expect.equal box.SizeY 5. "SizeY should be 5"
                Expect.equal box.SizeZ 3. "SizeZ should be 3"
            }

            test "SizeXSq, SizeYSq, SizeZSq" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.equal box.SizeXSq 100. "SizeXSq should be 100"
                Expect.equal box.SizeYSq 25. "SizeYSq should be 25"
                Expect.equal box.SizeZSq 9. "SizeZSq should be 9"
            }

            test "FarCorner" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let expected = Pnt(11., 7., 6.)
                Expect.isTrue (eqPnt box.FarCorner expected) "FarCorner should be sum of origin and all axes"
            }

            test "Diagonal" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let expected = Vec(10., 5., 3.)
                Expect.isTrue (eqVec box.Diagonal expected) "Diagonal should be sum of all axes"
            }

            test "Center" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let expected = Pnt(5., 5., 5.)
                Expect.isTrue (eqPnt box.Center expected) "Center should be at (5, 5, 5)"
            }

            test "Volume" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.equal box.Volume 150. "Volume should be 150"
            }
        ]

        testList "Creation Methods" [
            test "createFromPlane" {
                let pl = PPlane.createOriginXaxisYaxis(Pnt.Origin, Vec.Xaxis, Vec.Yaxis)
                let box = Box.createFromPlane 10. 5. 3. pl
                Expect.equal box.SizeX 10. "SizeX should be 10"
                Expect.equal box.SizeY 5. "SizeY should be 5"
                Expect.equal box.SizeZ 3. "SizeZ should be 3"
            }

            test "createFromBoundingBox" {
                let bbox = BBox.create(Pnt(0., 0., 0.), Pnt(10., 5., 3.))
                let box = Box.createFromBoundingBox bbox
                Expect.equal box.SizeX 10. "SizeX should be 10"
                Expect.equal box.SizeY 5. "SizeY should be 5"
                Expect.equal box.SizeZ 3. "SizeZ should be 3"
                Expect.isTrue (eqPnt box.Origin bbox.MinPnt) "Origin should be at BBox min point"
            }

            test "createFromRect2D" {
                let rect = Rect2D.createFromDirectionAndSizes(Pt.Origin, UnitVc.Xaxis, 10., 5.)
                let box = Box.createFromRect2D 2. 8. rect
                Expect.equal box.SizeX 10. "SizeX should be 10"
                Expect.equal box.SizeY 5. "SizeY should be 5"
                Expect.equal box.SizeZ 6. "SizeZ should be 6"
            }

            test "createFromRect3D" {
                let rect = Rect3D.createFromVectors(Pnt.Origin, Vec.Xaxis*10., Vec.Yaxis*5.)
                let box = Box.createFromRect3D 2. 8. rect
                Expect.equal box.SizeX 10. "SizeX should be 10"
                Expect.equal box.SizeY 5. "SizeY should be 5"
                Expect.equal box.SizeZ 6. "SizeZ should be 6"
            }

            test "createFromDirsAndPoints with multiple points" {
                let pts = ResizeArray([Pnt(0., 0., 0.); Pnt(10., 0., 0.); Pnt(0., 5., 0.); Pnt(0., 0., 3.)])
                let box = Box.createFromDirsAndPoints Vec.Xaxis Vec.Yaxis pts
                Expect.isTrue (box.SizeX >= 0.) "Should create valid box"
            }

            test "createFromDirsAndPoints rejects too few points" {
                let pts = ResizeArray([Pnt(0., 0., 0.)])
                Expect.throws (fun () -> Box.createFromDirsAndPoints Vec.Xaxis Vec.Yaxis pts |> ignore) "Should throw with just 1 point"
            }

            test "createFromDirsAndPoints rejects zero-length dirX" {
                let pts = ResizeArray([Pnt(0., 0., 0.); Pnt(10., 0., 0.)])
                Expect.throws (fun () -> Box.createFromDirsAndPoints Vec.Zero Vec.Yaxis pts |> ignore) "Should throw with zero dirX"
            }

            test "createFromDirsAndPoints rejects zero-length dirY" {
                let pts = ResizeArray([Pnt(0., 0., 0.); Pnt(10., 0., 0.)])
                Expect.throws (fun () -> Box.createFromDirsAndPoints Vec.Xaxis Vec.Zero pts |> ignore) "Should throw with zero dirY"
            }
        ]

        testList "Unit Axis Methods" [
            test "XaxisUnit for standard box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let unitX = box.XaxisUnit
                Expect.isTrue (eqVec unitX.AsVec (Vec(1., 0., 0.))) "XaxisUnit should be (1, 0, 0)"
            }

            test "YaxisUnit for standard box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let unitY = box.YaxisUnit
                Expect.isTrue (eqVec unitY.AsVec (Vec(0., 1., 0.))) "YaxisUnit should be (0, 1, 0)"
            }

            test "ZaxisUnit for standard box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let unitZ = box.ZaxisUnit
                Expect.isTrue (eqVec unitZ.AsVec (Vec(0., 0., 1.))) "ZaxisUnit should be (0, 0, 1)"
            }

            test "XaxisUnit throws for zero-length axis" {
                let box = Box.createUnchecked(Pnt.Origin, Vec.Zero, Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.throws (fun () -> box.XaxisUnit |> ignore) "Should throw for zero-length Xaxis"
            }
        ]

        testList "Evaluation" [
            test "EvaluateAt origin (0, 0, 0)" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let pt = box.EvaluateAt(0., 0., 0.)
                Expect.isTrue (eqPnt pt box.Origin) "Should return Origin at (0, 0, 0)"
            }

            test "EvaluateAt far corner (1, 1, 1)" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let pt = box.EvaluateAt(1., 1., 1.)
                Expect.isTrue (eqPnt pt box.FarCorner) "Should return FarCorner at (1, 1, 1)"
            }

            test "EvaluateAt center (0.5, 0.5, 0.5)" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let pt = box.EvaluateAt(0.5, 0.5, 0.5)
                Expect.isTrue (eqPnt pt (Pnt(5., 5., 5.))) "Should return center at (0.5, 0.5, 0.5)"
            }
        ]

        testList "Size Methods" [
            test "LongestEdge" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.equal box.LongestEdge 10. "Longest edge should be 10"
            }

            test "ShortestEdge" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.equal box.ShortestEdge 3. "Shortest edge should be 3"
            }

            test "LongestEdgeSq" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.equal box.LongestEdgeSq 100. "Longest edge squared should be 100"
            }

            test "ShortestEdgeSq" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.equal box.ShortestEdgeSq 9. "Shortest edge squared should be 9"
            }
        ]

        testList "Validation Methods" [
            test "IsZero for tiny box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(1e-13, 0., 0.), Vec(0., 1e-13, 0.), Vec(0., 0., 1e-13))
                Expect.isTrue box.IsZero "Tiny box should be zero"
            }

            test "IsPoint is same as IsZero" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(1e-13, 0., 0.), Vec(0., 1e-13, 0.), Vec(0., 0., 1e-13))
                Expect.equal box.IsPoint box.IsZero "IsPoint should equal IsZero"
            }

            test "IsLine for box with one dimension" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 1e-13, 0.), Vec(0., 0., 1e-13))
                Expect.isTrue box.IsLine "Box with one dimension should be a line"
            }

            test "IsFlat for box with two dimensions" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 1e-13))
                Expect.isTrue box.IsFlat "Box with two dimensions should be flat"
            }

            test "IsValid for normal box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.isTrue box.IsValid "Normal box should be valid"
            }

            test "HasVolume for normal box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.isTrue box.HasVolume "Normal box should have volume"
            }

            test "CountZeroSides for normal box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.equal box.CountZeroSides 0 "Normal box should have 0 zero sides"
            }

            test "CountZeroSides for flat box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 1e-13))
                Expect.equal box.CountZeroSides 1 "Flat box should have 1 zero side"
            }

            test "CountZeroSides for line box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 1e-13, 0.), Vec(0., 0., 1e-13))
                Expect.equal box.CountZeroSides 2 "Line box should have 2 zero sides"
            }

            test "CountZeroSides for point box" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(1e-13, 0., 0.), Vec(0., 1e-13, 0.), Vec(0., 0., 1e-13))
                Expect.equal box.CountZeroSides 3 "Point box should have 3 zero sides"
            }
        ]

        testList "Transformation Methods" [
            test "move by vector" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = Box.move (Vec(5., 3., 2.)) box
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 3., 2.))) "Origin should be moved"
                Expect.isTrue (eqVec moved.Xaxis box.Xaxis) "Xaxis should be unchanged"
            }

            test "Move instance method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = box.Move(Vec(5., 3., 2.))
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 3., 2.))) "Origin should be moved"
                Expect.isTrue (eqVec moved.Xaxis box.Xaxis) "Xaxis should be unchanged"
            }

            test "MoveX instance method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = box.MoveX(5.)
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 0., 0.))) "Origin should be moved in X"
            }

            test "MoveY instance method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = box.MoveY(3.)
                Expect.isTrue (eqPnt moved.Origin (Pnt(0., 3., 0.))) "Origin should be moved in Y"
            }

            test "MoveZ instance method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = box.MoveZ(2.)
                Expect.isTrue (eqPnt moved.Origin (Pnt(0., 0., 2.))) "Origin should be moved in Z"
            }

            test "moveX static method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = Box.moveX 5. box
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 0., 0.))) "Origin should be moved in X"
            }

            test "moveY static method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = Box.moveY 3. box
                Expect.isTrue (eqPnt moved.Origin (Pnt(0., 3., 0.))) "Origin should be moved in Y"
            }

            test "moveZ static method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = Box.moveZ 2. box
                Expect.isTrue (eqPnt moved.Origin (Pnt(0., 0., 2.))) "Origin should be moved in Z"
            }

            test "translate static method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = Box.translate (Vec(5., 3., 2.)) box
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 3., 2.))) "Origin should be translated"
            }

            test "Transform with identity matrix" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let transformed = box.Transform(Matrix.Identity)
                Expect.isTrue (eqPnt transformed.Origin box.Origin) "Origin should be unchanged with identity"
                Expect.isTrue (eqVec transformed.Xaxis box.Xaxis) "Xaxis should be unchanged"
            }

            test "Transform with translation matrix" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                let transformed = box.Transform(m)
                Expect.isTrue (eqPnt transformed.Origin (Pnt(5., 3., 2.))) "Origin should be translated"
            }

            test "transform static method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                let transformed = Box.transform m box
                Expect.isTrue (eqPnt transformed.Origin (Pnt(5., 3., 2.))) "Origin should be translated"
            }

            test "TransformRigid instance method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                let transformed = box.TransformRigid(m)
                Expect.isTrue (eqPnt transformed.Origin (Pnt(5., 3., 2.))) "Origin should be translated"
            }

            test "transformRigid static method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                let transformed = Box.transformRigid m box
                Expect.isTrue (eqPnt transformed.Origin (Pnt(5., 3., 2.))) "Origin should be translated"
            }

            test "Rotate with identity quaternion" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let rotated = box.Rotate(Quaternion.Identity)
                Expect.isTrue (eqPnt rotated.Origin box.Origin) "Origin should be unchanged with identity quaternion"
                Expect.isTrue (eqVec rotated.Xaxis box.Xaxis) "Xaxis should be unchanged"
            }

            test "Rotate 90 degrees around Z axis" {
                let box = Box.createUnchecked(Pnt(1., 0., 0.), Vec(1., 0., 0.), Vec(0., 1., 0.), Vec(0., 0., 1.))
                let q = Quaternion.createFromAxisAngle(UnitVec.Zaxis, UtilEuclid.``Math.PI/2``)
                let rotated = box.Rotate(q)
                Expect.isTrue (eqPnt rotated.Origin (Pnt(0., 1., 0.))) "Origin should be rotated 90 degrees"
            }

            test "rotate static method" {
                let box = Box.createUnchecked(Pnt(1., 0., 0.), Vec(1., 0., 0.), Vec(0., 1., 0.), Vec(0., 0., 1.))
                let q = Quaternion.createFromAxisAngle(UnitVec.Zaxis, UtilEuclid.``Math.PI/2``)
                let rotated = Box.rotate q box
                Expect.isTrue (eqPnt rotated.Origin (Pnt(0., 1., 0.))) "Origin should be rotated 90 degrees"
            }

            test "RotateWithCenter keeps center point fixed" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                let center = box.Center
                let q = Quaternion.createFromAxisAngle(UnitVec.Zaxis, UtilEuclid.``Math.PI/2``)
                let rotated = box.RotateWithCenter(center, q)
                Expect.isTrue (eqPnt rotated.Center center) "Center should remain fixed"
            }

            test "rotateWithCenter static method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                let center = box.Center
                let q = Quaternion.createFromAxisAngle(UnitVec.Zaxis, UtilEuclid.``Math.PI/2``)
                let rotated = Box.rotateWithCenter center q box
                Expect.isTrue (eqPnt rotated.Center center) "Center should remain fixed"
            }

            test "translateLocalX" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = Box.translateLocalX 5. box
                Expect.isTrue (eqPnt moved.Origin (Pnt(5., 0., 0.))) "Should move along X-axis"
            }

            test "translateLocalY" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = Box.translateLocalY 3. box
                Expect.isTrue (eqPnt moved.Origin (Pnt(0., 3., 0.))) "Should move along Y-axis"
            }

            test "translateLocalZ" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let moved = Box.translateLocalZ 2. box
                Expect.isTrue (eqPnt moved.Origin (Pnt(0., 0., 2.))) "Should move along Z-axis"
            }

            test "Scale from world origin" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let scaled = box.Scale 2.
                Expect.isTrue (eqPnt scaled.Origin (Pnt(2., 4., 6.))) "Origin should be scaled"
                Expect.equal scaled.SizeX 20. "SizeX should be scaled"
                Expect.equal scaled.SizeY 10. "SizeY should be scaled"
                Expect.equal scaled.SizeZ 6. "SizeZ should be scaled"
            }

            test "ScaleOn center point" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let scaled = box.ScaleOn (Pnt(5., 5., 5.)) 2.
                Expect.isTrue (eqPnt scaled.Center box.Center) "Center should remain at same position"
                Expect.equal scaled.SizeX 20. "SizeX should be scaled"
            }

            test "scale static method" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let scaled = Box.scale 2. box
                Expect.equal scaled.SizeX 20. "SizeX should be scaled"
            }
        ]

        testList "Expansion Methods" [
            test "expand by positive distance" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let expanded = Box.expand 1. box
                Expect.isTrue (box.SizeX < expanded.SizeX) "Should expand in X"
                Expect.isTrue (box.SizeY < expanded.SizeY) "Should expand in Y"
                Expect.isTrue (box.SizeZ < expanded.SizeZ) "Should expand in Z"
            }

            test "expand throws on underflow" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                Expect.throws (fun () -> Box.expand -2. box |> ignore) "Should throw when shrinking causes underflow"
            }

            test "expandXYZ with different distances" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let expanded = Box.expandXYZ 1. 2. 3. box
                Expect.isTrue (box.SizeX < expanded.SizeX) "Should expand in X"
                Expect.isTrue (box.SizeY < expanded.SizeY) "Should expand in Y"
                Expect.isTrue (box.SizeZ < expanded.SizeZ) "Should expand in Z"
            }

            test "expandRel with factor 1.5" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let expanded = Box.expandRel 1.5 box
                Expect.isTrue (eqPnt expanded.Center box.Center) "Center should remain the same"
                Expect.equal expanded.SizeX 15. "SizeX should be 15"
                Expect.equal expanded.SizeY 15. "SizeY should be 15"
                Expect.equal expanded.SizeZ 15. "SizeZ should be 15"
            }

            test "expandRel rejects negative factor" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                Expect.throws (fun () -> Box.expandRel -0.5 box |> ignore) "Should throw on negative factor"
            }

            test "expandRelXYZ with different factors" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let expanded = Box.expandRelXYZ 1.5 0.5 2.0 box
                Expect.isTrue (eqPnt expanded.Center box.Center) "Center should remain the same"
                Expect.equal expanded.SizeX 15. "SizeX should be 15"
                Expect.equal expanded.SizeY 5. "SizeY should be 5"
                Expect.equal expanded.SizeZ 20. "SizeZ should be 20"
            }
        ]

        testList "Containment" [
            test "Contains point inside" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let pt = Pnt(5., 5., 5.)
                Expect.isTrue (box.Contains pt) "Box should contain point inside"
            }

            test "Contains point on boundary" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let pt = Pnt(10., 5., 5.)
                Expect.isTrue (box.Contains pt) "Box should contain point on boundary"
            }

            test "Contains point outside" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let pt = Pnt(11., 5., 5.)
                Expect.isFalse (box.Contains pt) "Box should not contain point outside"
            }

            test "contains static method" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let pt = Pnt(5., 5., 5.)
                Expect.isTrue (Box.contains pt box) "Static method should work"
            }
        ]

        testList "BBox Conversion" [
            test "BBox for axis-aligned box" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let bbox = box.BBox
                Expect.equal bbox.MinX 1. "BBox MinX should be 1"
                Expect.equal bbox.MinY 2. "BBox MinY should be 2"
                Expect.equal bbox.MinZ 3. "BBox MinZ should be 3"
                Expect.equal bbox.MaxX 11. "BBox MaxX should be 11"
                Expect.equal bbox.MaxY 7. "BBox MaxY should be 7"
                Expect.equal bbox.MaxZ 6. "BBox MaxZ should be 6"
            }

            test "BBox for rotated box" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 10., 0.), Vec(-10., 10., 0.), Vec(0., 0., 10.))
                let bbox = box.BBox
                Expect.isTrue (bbox.MinX <= 0.) "BBox should contain origin"
                Expect.isTrue (bbox.MaxX >= 0.) "BBox should contain all corners"
            }
        ]

        testList "Plane Conversion" [
            test "Plane from box" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let plane = box.Plane
                Expect.isTrue (eqPnt plane.Origin box.Origin) "Plane origin should match box origin"
            }
        ]

        testList "Points and Edges" [
            test "Points array has 8 points" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let pts = box.Points
                Expect.equal pts.Length 8 "Should have 8 points"
            }

            test "Pt0 is Origin" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.isTrue (eqPnt box.Pt0 box.Origin) "Pt0 should equal Origin"
            }

            test "Pt6 is FarCorner" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                Expect.isTrue (eqPnt box.Pt6 box.FarCorner) "Pt6 should equal FarCorner"
            }

            test "Edges array has 12 edges" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let edges = box.Edges
                Expect.equal edges.Length 12 "Should have 12 edges"
            }

            test "Edge0 is parallel to Xaxis" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let edge = box.Edge0
                Expect.isTrue (eqPnt edge.From box.Pt0) "Edge0 should start at Pt0"
            }

            test "Faces array has 6 faces" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let faces = box.Faces
                Expect.equal faces.Length 6 "Should have 6 faces"
            }

            test "BottomFace has correct origin" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let bottom = box.BottomFace
                Expect.isTrue (eqPnt bottom.Origin box.Origin) "Bottom face should start at origin"
            }

            test "TopFace has correct origin" {
                let box = Box.createUnchecked(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let top = box.TopFace
                Expect.isTrue (eqPnt top.Origin (box.Origin + box.Zaxis)) "Top face should be offset by Zaxis"
            }
        ]

        testList "Equality Methods" [
            test "equals with exact match" {
                let a = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let b = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                Expect.isTrue (Box.equals 0.0 a b) "Exact boxes should be equal"
            }

            test "equals with tolerance" {
                let a = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let b = Box.createUnchecked(Pnt(0.001, 0.001, 0.001), Vec(10.001, 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                Expect.isTrue (Box.equals 0.01 a b) "Boxes should be equal within tolerance"
                Expect.isFalse (Box.equals 0.0001 a b) "Boxes should not be equal with small tolerance"
            }

            test "notEquals" {
                let a = Box.createUnchecked(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                let b = Box.createUnchecked(Pnt(1., 1., 1.), Vec(10., 0., 0.), Vec(0., 10., 0.), Vec(0., 0., 10.))
                Expect.isTrue (Box.notEquals 0.5 a b) "Different boxes should not be equal"
            }
        ]

        testList "Face Area Methods" [
            test "AreaOfBiggestFace" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let area = box.AreaOfBiggestFace
                Expect.equal area 50. "Biggest face should be X*Y = 50"
            }

            test "AreaOfSmallestFace" {
                let box = Box.createUnchecked(Pnt.Origin, Vec(10., 0., 0.), Vec(0., 5., 0.), Vec(0., 0., 3.))
                let area = box.AreaOfSmallestFace
                Expect.equal area 15. "Smallest face should be Y*Z = 15"
            }
        ]

        testList "Box.createFromPlaneAndPoints" [
            test "all positive projections" {
                // Plane at origin with standard axes
                let pl = PPlane.WorldXY
                // Points all in positive octant
                let pts = [| Pnt(1., 1., 1.); Pnt(2., 1., 1.); Pnt(2., 2., 1.); Pnt(1., 2., 1.)
                             Pnt(1., 1., 2.); Pnt(2., 1., 2.); Pnt(2., 2., 2.); Pnt(1., 2., 2.) |]
                let box = Box.createFromPlaneAndPoints pl pts
                "positive - origin X" |> Expect.isTrue (eqFloat box.Origin.X 1.0)
                "positive - origin Y" |> Expect.isTrue (eqFloat box.Origin.Y 1.0)
                "positive - origin Z" |> Expect.isTrue (eqFloat box.Origin.Z 1.0)
                "positive - SizeX" |> Expect.isTrue (eqFloat box.SizeX 1.0)
                "positive - SizeY" |> Expect.isTrue (eqFloat box.SizeY 1.0)
                "positive - SizeZ" |> Expect.isTrue (eqFloat box.SizeZ 1.0)
            }

            test "all negative projections" {
                // Plane at origin with standard axes
                let pl = PPlane.WorldXY
                // Points all in negative octant
                let pts = [| Pnt(-2., -2., -2.); Pnt(-1., -2., -2.); Pnt(-1., -1., -2.); Pnt(-2., -1., -2.)
                             Pnt(-2., -2., -1.); Pnt(-1., -2., -1.); Pnt(-1., -1., -1.); Pnt(-2., -1., -1.) |]
                let box = Box.createFromPlaneAndPoints pl pts
                "negative - origin X" |> Expect.isTrue (eqFloat box.Origin.X -2.0)
                "negative - origin Y" |> Expect.isTrue (eqFloat box.Origin.Y -2.0)
                "negative - origin Z" |> Expect.isTrue (eqFloat box.Origin.Z -2.0)
                "negative - SizeX" |> Expect.isTrue (eqFloat box.SizeX 1.0)
                "negative - SizeY" |> Expect.isTrue (eqFloat box.SizeY 1.0)
                "negative - SizeZ" |> Expect.isTrue (eqFloat box.SizeZ 1.0)
            }

            test "mixed projections" {
                // Plane at origin with standard axes
                let pl = PPlane.WorldXY
                // Points spanning across origin
                let pts = [| Pnt(-1., -1., -1.); Pnt(2., -1., -1.); Pnt(2., 1., -1.); Pnt(-1., 1., -1.)
                             Pnt(-1., -1., 1.); Pnt(2., -1., 1.); Pnt(2., 1., 1.); Pnt(-1., 1., 1.) |]
                let box = Box.createFromPlaneAndPoints pl pts
                "mixed - origin X" |> Expect.isTrue (eqFloat box.Origin.X -1.0)
                "mixed - origin Y" |> Expect.isTrue (eqFloat box.Origin.Y -1.0)
                "mixed - origin Z" |> Expect.isTrue (eqFloat box.Origin.Z -1.0)
                "mixed - SizeX" |> Expect.isTrue (eqFloat box.SizeX 3.0)
                "mixed - SizeY" |> Expect.isTrue (eqFloat box.SizeY 2.0)
                "mixed - SizeZ" |> Expect.isTrue (eqFloat box.SizeZ 2.0)
            }

            test "offset plane" {
                // Plane offset from origin
                let pl = PPlane.createOriginXaxisYaxis(Pnt(10., 10., 10.), Vec.Xaxis, Vec.Yaxis)
                // Points relative to offset
                let pts = [| Pnt(11., 11., 11.); Pnt(13., 11., 11.); Pnt(13., 12., 11.); Pnt(11., 12., 11.)
                             Pnt(11., 11., 12.); Pnt(13., 11., 12.); Pnt(13., 12., 12.); Pnt(11., 12., 12.) |]
                let box = Box.createFromPlaneAndPoints pl pts
                "offset - origin X" |> Expect.isTrue (eqFloat box.Origin.X 11.0)
                "offset - origin Y" |> Expect.isTrue (eqFloat box.Origin.Y 11.0)
                "offset - origin Z" |> Expect.isTrue (eqFloat box.Origin.Z 11.0)
                "offset - SizeX" |> Expect.isTrue (eqFloat box.SizeX 2.0)
                "offset - SizeY" |> Expect.isTrue (eqFloat box.SizeY 1.0)
                "offset - SizeZ" |> Expect.isTrue (eqFloat box.SizeZ 1.0)
            }
        ]

        testList "IntersectRay" [
            test "ray through center of axis-aligned box" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                let ray = Line3D(Pnt(-5., 1., 1.), Pnt(5., 1., 1.))
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsSome "Should intersect"
                let tEntry, tExit = result.Value
                Expect.isTrue (eqFloat tEntry 0.5) $"Entry parameter should be 0.5, got {tEntry}"
                Expect.isTrue (eqFloat tExit 0.7) $"Exit parameter should be 0.7, got {tExit}"
            }

            test "ray missing box" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                let ray = Line3D(Pnt(-5., 10., 10.), Pnt(5., 10., 10.))
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsNone "Should not intersect"
            }

            test "ray starting inside box" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                let ray = Line3D(Pnt(1., 1., 1.), Pnt(3., 1., 1.))
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsSome "Should intersect"
                let tEntry, tExit = result.Value
                Expect.isTrue (tEntry < 0.0) $"Entry should be negative (behind ray origin), got {tEntry}"
                Expect.isTrue (tExit > 0.0) $"Exit should be positive, got {tExit}"
            }

            test "ray parallel to box face but outside" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                let ray = Line3D(Pnt(-1., 5., 1.), Pnt(3., 5., 1.)) // parallel to X, but Y=5 is outside
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsNone "Should not intersect (ray parallel but outside)"
            }

            test "ray parallel to box face inside slab" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                let ray = Line3D(Pnt(-5., 1., 1.), Pnt(5., 1., 1.)) // parallel to X, Y and Z inside box
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsSome "Should intersect"
            }

            test "rotated box ray intersection" {
                // Create a box rotated 45 degrees around Z axis
                let sqrt2over2 = sqrt 2.0 / 2.0
                let xAxis = Vec(sqrt2over2, sqrt2over2, 0.) * 2.0
                let yAxis = Vec(-sqrt2over2, sqrt2over2, 0.) * 2.0
                let zAxis = Vec(0., 0., 2.)
                let box = Box.createUnchecked(Pnt(0., 0., 0.), xAxis, yAxis, zAxis)
                // Ray along world X through center height
                let ray = Line3D(Pnt(-5., 0., 1.), Pnt(5., 0., 1.))
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsSome "Should intersect rotated box"
            }

            test "ray too short (zero length)" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                let ray = Line3D(Pnt(1., 1., 1.), Pnt(1., 1., 1.))
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsNone "Should return None for zero-length ray"
            }

            test "ray grazing box corner" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                // Ray that just touches the corner at (2, 2, 2)
                let ray = Line3D(Pnt(2., 2., 0.), Pnt(2., 2., 4.))
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsSome "Should intersect at corner edge"
            }

            test "ray along box edge" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                // Ray along the edge from (0,0,0) to (0,0,2)
                let ray = Line3D(Pnt(0., 0., -1.), Pnt(0., 0., 3.))
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsSome "Should intersect along edge"
            }

            test "static intersectRay function" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.), Vec(0., 0., 2.))
                let ray = Line3D(Pnt(-5., 1., 1.), Pnt(5., 1., 1.))
                let result = Box.intersectRay ray box
                Expect.isTrue result.IsSome "Static function should also work"
            }

            test "intersection points can be computed from parameters" {
                let box = Box.createUnchecked(Pnt(0., 0., 0.), Vec(4., 0., 0.), Vec(0., 4., 0.), Vec(0., 0., 4.))
                let ray = Line3D(Pnt(-2., 2., 2.), Pnt(6., 2., 2.))
                let result = box.IntersectRay(ray)
                Expect.isTrue result.IsSome "Should intersect"
                let tEntry, tExit = result.Value
                // Compute actual intersection points
                let entryPt = ray.From + ray.Direction * tEntry
                let exitPt = ray.From + ray.Direction * tExit
                // Entry should be at x=0, exit at x=4
                Expect.isTrue (eqFloat entryPt.X 0.0) $"Entry X should be 0, got {entryPt.X}"
                Expect.isTrue (eqFloat exitPt.X 4.0) $"Exit X should be 4, got {exitPt.X}"
            }
        ]
    ]
