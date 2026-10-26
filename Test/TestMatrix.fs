module TestMatrix

open Euclid

open EuclidTestSupport

let inline eq a b = Pnt.distanceTo a b < 1e-12

let inline expectEqual a b msg =
    let same = eq a b
    if not same then
        $"{msg} expected: \n{b.AsString}, got: \n{a.AsString}" |> Expect.isTrue same
    else
        msg |> Expect.isTrue same


let tests =
    testList "Matrix transformations" [

        test "Matrix translate " {
            let m = Matrix.createTranslation (Vec(2,3,4)) |> Matrix.addTranslation(Vec(4,1,-1))
            let a = Pnt(1,2,3)
            let b = a *** m
            "translation ok" |> expectEqual b (Pnt(7,6,6))
            let c = a |> Pnt.transform m
            "transform ok" |> expectEqual c (Pnt(7,6,6))


            let m1 = Matrix.createTranslation (Vec(2,3,4))
            let m2 = Matrix.createTranslation (Vec(4,1,-1))
            let m3 = Matrix.multiply( m1, m2)
            let d = a *** m3
            "multiply ok" |> expectEqual d (Pnt(7,6,6))
            let e = a |> Pnt.transform m3
            "transform multiply ok" |> expectEqual e (Pnt(7,6,6))
        }


        test "Matrix inverse id " {

            let m = Matrix.identity
            let inv =  m.Inverse
            let a = Pnt(1,2,3)
            let ax = a *** m
            let a0 = ax *** inv
            "inverse identity ok" |> expectEqual a0 a
        }

        test "Matrix inverse transform ***" {
                let m = Matrix.createTranslation (Vec(-2,-3,4))
                let inv = m.Inverse
                let a = Pnt(1,2,3)
                let r = a  |> Pnt.transform m |> Pnt.transform inv
                "inverse transform ok" |> expectEqual a r
        }

        test "Matrix inverse transform  " {
                let m = Matrix.createTranslation (Vec(-2,-3,4))
                let inv = m.Inverse
                let a = Pnt(1,2,3)
                let ax = a *** m
                let a0 = ax *** inv
                "inverse *** ok" |> expectEqual a0 a
            }


        test "Matrix inverse rotate " {
            let q = Quaternion.createFromVecDegrees (Vec.zAxis*9.0) 90.
            let m = Matrix.createFromQuaternion q
            let a = Pnt(9,0,3)
            let inv = m.Inverse
            let c = a |> Pnt.transform m |> Pnt.transform inv
            "rotateByQuaternion inverse" |> expectEqual a c
        }

        test "Matrix inverse transform rot ***" {
            let t = Matrix.createTranslation (Vec(-2,-3,4))
            let q = Quaternion.createFromVecDegrees (Vec.zAxis*9.0) 90.
            let r = Matrix.createFromQuaternion q
            let m = t *** r
            let inv = m.Inverse
            let a = Pnt(1,2,3)
            let r = a  |> Pnt.transform m |> Pnt.transform inv
            "inverse transformRigid ok" |> expectEqual a r
        }

        test "Matrix inverse transform rot inv ***" {
            let t = Matrix.createTranslation (Vec(-2,-3,4))
            let q = Quaternion.createFromVecDegrees (Vec.zAxis*9.0) 90.
            let r = Matrix.createFromQuaternion q
            let m = r *** t
            let inv = m.Inverse
            let a = Pnt(1,2,3)
            let r = a  |> Pnt.transform m |> Pnt.transform inv
            "inverse transformRigid ok" |> expectEqual a r
        }



        test "Matrix scaling non-uniform" {
            let m = Matrix.createScale 2.0 3.0 0.5
            let a = Pnt(1,2,4)
            let b = a *** m
            "non-uniform scaling ok" |> expectEqual b (Pnt(2,6,2))

            let inv = m.Inverse
            let c = b *** inv
            "scaling inverse ok" |> expectEqual c a
        }

        test "Matrix rotation X axis" {
            let m = Matrix.createRotationXDegrees(90.0)
            let a = Pnt(1,1,0)
            let b = a *** m
            "rotation X 90 degrees" |> expectEqual b (Pnt(1,0,1))

            let inv = m.Inverse
            let c = b *** inv
            "rotation X inverse" |> expectEqual c a
        }

        test "Matrix rotation Y axis" {
            let m = Matrix.createRotationYDegrees(90.0)
            let a = Pnt(1,0,1)
            let b = a *** m
            "rotation Y 90 degrees" |> expectEqual b (Pnt(1,0,-1))

            let inv = m.Inverse
            let c = b *** inv
            "rotation Y inverse" |> expectEqual c a
        }

        test "Matrix rotation Z axis" {
            let m = Matrix.createRotationZDegrees(90.0)
            let a = Pnt(1,0,0)
            let b = a *** m
            "rotation Z 90 degrees" |> expectEqual b (Pnt(0,1,0))

            let inv = m.Inverse
            let c = b *** inv
            "rotation Z inverse" |> expectEqual c a
        }

        test "Matrix rotation arbitrary axis" {
            let axis = Vec(1,1,1)
            let m = Matrix.createRotationFromVecDegrees axis 120.0
            let a = Pnt(1,0,0)
            let b = a *** m
            let c = b *** m
            let d = c *** m
            "rotation 120 degrees x3 = identity" |> expectEqual d a
        }

        test "Matrix rotation with center" {
            let axis = Vec.zAxis
            let center = Pnt(1,1,0)
            let m = Matrix.createRotationFromVecDegreesAround center axis 90.0
            let a = Pnt(2,1,0)
            let b = a *** m
            "rotation around center" |> expectEqual b (Pnt(1,2,0))
        }

        test "Matrix shear transformation" {
            let m = Matrix.createShear(0.5, 3.0, 0.0, 0.0, 0.0, 0.0)
            let a = Pnt(2,0,0)
            let b = a *** m
            "shear XY" |> expectEqual b (Pnt(2,1,6))

            let det = m.Determinant
            "shear determinant is 1" |> Expect.isTrue (abs(det - 1.0) < 1e-12)
        }

        test "Matrix combined transformations TRS" {
            let t = Matrix.createTranslation(Vec(1,2,3))
            let r = Matrix.createRotationZDegrees(90.0)
            let s = Matrix.createScale 2.0 2.0 2.0

            // Apply in order: Scale, then Rotate, then Translate
            let m = t *** r *** s
            let a = Pnt(1,0,0)
            let b = a *** m
            "TRS transformation" |> expectEqual b (Pnt(-4,4,6))
        }


        test "Matrix vec to vec same direction" {
            let v = UnitVec.create 1 1 1
            let m = Matrix.createRotationFromUnitVecs v v
            "same vector gives identity" |> Expect.isTrue m.IsIdentity
        }

        test "Matrix properties identity" {
            let m = Matrix.identity
            "identity IsIdentity" |> Expect.isTrue m.IsIdentity
            "identity IsAffine" |> Expect.isTrue m.IsAffine
            "identity IsOrthogonal" |> Expect.isTrue m.IsOrthogonal
            "identity not IsScaling" |> Expect.isFalse m.IsScaling
            "identity not IsTranslating" |> Expect.isFalse m.IsTranslating
            "identity determinant is 1" |> Expect.isTrue (abs(m.Determinant - 1.0) < 1e-12)
        }

        test "Matrix properties translation" {
            let m = Matrix.createTranslation(Vec(1,2,3))
            "translation not IsIdentity" |> Expect.isFalse m.IsIdentity
            "translation IsAffine" |> Expect.isTrue m.IsAffine
            "translation IsTranslating" |> Expect.isTrue m.IsTranslating
            "translation IsOnlyTranslating" |> Expect.isTrue m.IsOnlyTranslating
            "translation not IsScaling" |> Expect.isFalse m.IsScaling
            "translation determinant is 1" |> Expect.isTrue (abs(m.Determinant - 1.0) < 1e-12)
        }

        test "Matrix properties scaling" {
            let m = Matrix.createScale 2.0 3.0 4.0
            "scaling not IsIdentity" |> Expect.isFalse m.IsIdentity
            "scaling IsAffine" |> Expect.isTrue m.IsAffine
            "scaling IsScaling" |> Expect.isTrue m.IsScaling
            "scaling not IsTranslating" |> Expect.isFalse m.IsTranslating
            "scaling determinant is 24" |> Expect.isTrue (abs(m.Determinant - 24.0) < 1e-12)
        }

        test "Matrix properties rotation" {
            let m = Matrix.createRotationZDegrees(45.0)
            "rotation not IsIdentity" |> Expect.isFalse m.IsIdentity
            "rotation IsAffine" |> Expect.isTrue m.IsAffine
            "rotation IsOrthogonal" |> Expect.isTrue m.IsOrthogonal
            "rotation not IsScaling" |> Expect.isFalse m.IsScaling
            "rotation determinant is 1" |> Expect.isTrue (abs(m.Determinant - 1.0) < 1e-12)
        }

        test "Matrix properties mirroring" {
            let m = Matrix.createScale (-1.0) 1.0 1.0
            "mirroring IsAffine" |> Expect.isTrue m.IsAffine
            "mirroring IsMirroring" |> Expect.isTrue m.IsMirroring
            "mirroring IsReflecting" |> Expect.isTrue m.IsReflecting
            "mirroring determinant is -1" |> Expect.isTrue (abs(m.Determinant + 1.0) < 1e-12)
        }

        test "Matrix from quaternion" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 90.0
            let m = Matrix.createFromQuaternion(q)
            let a = Pnt(1,0,0)
            let b = a *** m
            "quaternion to matrix rotation" |> expectEqual b (Pnt(0,1,0))
        }

        test "Matrix column vectors" {
            let m = Matrix.createRotationZDegrees(90.0)
            let col1 = m.ColumnVector1
            let col2 = m.ColumnVector2
            let col3 = m.ColumnVector3
            "column 1 after Z rotation" |> expectEqual col1.AsPnt (Pnt(0,1,0))
            "column 2 after Z rotation" |> expectEqual col2.AsPnt (Pnt(-1,0,0))
            "column 3 after Z rotation" |> expectEqual col3.AsPnt (Pnt(0,0,1))
        }

        test "Matrix transpose" {
            let m = Matrix(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
            let mt = Matrix.transpose(m)
            let expected = Matrix(1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16)
            "matrix transpose" |> Expect.isTrue (Matrix.equalsWithin 1e-12 mt expected)
        }

        test "Matrix multiply associativity" {
            let a = Matrix.createTranslation(Vec(1,0,0))
            let b = Matrix.createRotationZDegrees(90.0)
            let c = Matrix.createScale 2.0 2.0 2.0

            let abc1 = (a *** b) *** c
            let abc2 = a *** (b *** c)
            "matrix multiplication associativity" |> Expect.isTrue (Matrix.equalsWithin 1e-12 abc1 abc2)
        }

        test "Matrix determinant scaling" {
            let m = Matrix.createScale 2.0 3.0 4.0
            let det = m.Determinant
            "scaling determinant" |> Expect.isTrue (abs(det - 24.0) < 1e-12)
        }

        test "Matrix arrays conversion" {
            let m = Matrix(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
            let rowArray = m.ToArrayByRows
            let colArray = m.ToArrayByColumns

            let mFromRows = Matrix.createFromRowMajorArray(rowArray)
            let mFromCols = Matrix.createFromColumnMajorArray(colArray)

            "matrix from row array" |> Expect.isTrue (Matrix.equalsWithin 1e-12 m mFromRows)
            "matrix from column array" |> Expect.isTrue (Matrix.equalsWithin 1e-12 m mFromCols)
        }

        test "Matrix add translation" {
            let m = Matrix.createRotationZDegrees(90.0)
            let m2 = Matrix.addTranslation (Vec(1,2,3)) m
            let a = Pnt(1,0,0)
            let b = a *** m2
            "add translation to rotation" |> expectEqual b (Pnt(1,3,3))
        }

        test "Matrix very small transformation" {
            let tiny = 1e-10
            let m = Matrix.createTranslation(Vec(tiny, tiny, tiny))
            let a = Pnt(0,0,0)
            let b = a *** m
            "very small translation" |> expectEqual b (Pnt(tiny, tiny, tiny))
        }

        // Edge case tests for createVecToVec
        test "Matrix vec to vec opposite vectors should fail" {
            let v1 = UnitVec.xAxis
            let v2 = -UnitVec.xAxis
            "opposite vectors should throw" |> Expect.throws (fun () -> Matrix.createRotationFromUnitVecs v1 v2 |> ignore)
        }

        test "Matrix vec to vec nearly same vectors" {
            let v1 = UnitVec.create 1.0 0.0 0.0
            let v2 = UnitVec.create 1.0 1e-13 0.0 // very close
            let m = Matrix.createRotationFromUnitVecs v1 v2
            "nearly same vectors should give identity" |> Expect.isTrue m.IsIdentity
        }

        test "Matrix vec to vec with zero-length Vec should fail" {
            let v1 = Vec(0.0, 0.0, 0.0)
            let v2 = Vec(1.0, 0.0, 0.0)
            "zero-length from vector should throw" |> Expect.throws (fun () -> Matrix.createRotationFromVecs v1 v2 |> ignore)
        }

        test "Matrix vec to vec with zero-length target Vec should fail" {
            let v1 = Vec(1.0, 0.0, 0.0)
            let v2 = Vec(0.0, 0.0, 0.0)
            "zero-length to vector should throw" |> Expect.throws (fun () -> Matrix.createRotationFromVecs v1 v2 |> ignore)
        }


        test "Matrix createVecToVec is inverse of reverse rotation" {
            let from = UnitVec.create 1 1 0
            let to_ = UnitVec.create 0 1 1
            let m1 = Matrix.createRotationFromUnitVecs from to_
            let m2 = Matrix.createRotationFromUnitVecs to_ from

            // m2 should be the inverse of m1
            let composed = m1 *** m2
            "forward then reverse gives identity" |> Expect.isTrue composed.IsIdentity
        }


        // Edge case tests for Inverse
        test "Matrix inverse of zero determinant should fail" {
            // Create a singular matrix (all zeros)
            let m = Matrix(0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0)
            "zero determinant should throw" |> Expect.throws (fun () -> m.Inverse |> ignore)
        }

        test "Matrix inverse of nearly singular should fail" {
            // Create a matrix with very small determinant
            let m = Matrix(1,0,0,0, 0,1,0,0, 0,0,1e-17,0, 0,0,0,1)
            "nearly zero determinant should throw" |> Expect.throws (fun () -> m.Inverse |> ignore)
        }

        // Edge case tests for createRotationAxis
        test "Matrix rotation axis zero-length should fail" {
            let axis = Vec(0.0, 0.0, 0.0)
            "zero-length axis should throw" |> Expect.throws (fun () -> Matrix.createRotationFromVecDegrees axis 45.0 |> ignore)
        }

        test "Matrix rotation axis very short should fail" {
            let axis = Vec(1e-13, 0.0, 0.0)
            "very short axis should throw" |> Expect.throws (fun () -> Matrix.createRotationFromVecDegrees axis 45.0 |> ignore)
        }

        // Edge case tests for IsProjecting
        test "Matrix perspective projection IsProjecting" {
            let m = Matrix.createPerspectiveAlongNegZ(800.0, 600.0, 0.1, 100.0)
            "perspective matrix should be projecting" |> Expect.isTrue m.IsProjecting
            "perspective matrix should not be affine" |> Expect.isFalse m.IsAffine
        }

        // Edge case tests for createPerspective
        test "Matrix perspective negative near plane should fail" {
            "negative near plane should throw" |> Expect.throws (fun () -> Matrix.createPerspectiveAlongNegZ(800.0, 600.0, -0.1, 100.0) |> ignore)
        }

        test "Matrix perspective zero near plane should fail" {
            "zero near plane should throw" |> Expect.throws (fun () -> Matrix.createPerspectiveAlongNegZ(800.0, 600.0, 0.0, 100.0) |> ignore)
        }

        test "Matrix perspective negative far plane should fail" {
            "negative far plane should throw" |> Expect.throws (fun () -> Matrix.createPerspectiveAlongNegZ(800.0, 600.0, 0.1, -100.0) |> ignore)
        }

        test "Matrix perspective near >= far should fail" {
            "near >= far should throw" |> Expect.throws (fun () -> Matrix.createPerspectiveAlongNegZ(800.0, 600.0, 100.0, 50.0) |> ignore)
        }

        test "Matrix perspective near == far should fail" {
            "near == far should throw" |> Expect.throws (fun () -> Matrix.createPerspectiveAlongNegZ(800.0, 600.0, 100.0, 100.0) |> ignore)
        }

        test "Matrix perspective valid parameters" {
            let m = Matrix.createPerspectiveAlongNegZ(2.0, 2.0, 1.0, 10.0)
            "valid perspective should not throw" |> Expect.isTrue m.IsProjecting
            "near corner" |> expectEqual (Pnt(1,1,-1) *** m) (Pnt(1,1,0))
            "perspective divide" |> expectEqual (Pnt(1,1,-2) *** m) (Pnt(0.5,0.5,5.0/9.0))
            "far depth" |> expectEqual (Pnt(1,1,-10) *** m) (Pnt(0.1,0.1,1))
        }

        test "Matrix perspective named axes" {
            let negZ = Matrix.createPerspectiveAlongNegZ(4., 2., 1., 10.)
            let posY = Matrix.createPerspectiveAlongPosY(4., 2., 1., 10.)
            "NegZ right and up" |> expectEqual (Pnt(2,1,-1) *** negZ) (Pnt(1,1,0))
            "PosY right and Z up" |> expectEqual (Pnt(2,1,1) *** posY) (Pnt(1,1,0))
            "PosY perspective divide" |> expectEqual (Pnt(2,2,1) *** posY) (Pnt(0.5,0.5,5.0/9.0))
            "PosY far depth" |> expectEqual (Pnt(0,10,0) *** posY) (Pnt(0,0,1))
        }

        test "Matrix named perspective infinite far plane" {
            let negZ = Matrix.createPerspectiveAlongNegZ(2., 2., 1., infinity)
            let posY = Matrix.createPerspectiveAlongPosY(2., 2., 1., infinity)
            "NegZ infinite far" |> expectEqual (Pnt(1,1,-2) *** negZ) (Pnt(0.5,0.5,0.5))
            "PosY infinite far" |> expectEqual (Pnt(1,2,1) *** posY) (Pnt(0.5,0.5,0.5))
        }

        test "Matrix look at translates and rotates camera" {
            let eye = Pnt(10,-10,5)
            let view = Matrix.createLookAt (Vec(0,0,1)) eye Pnt.Origin
            "eye becomes origin" |> expectEqual (eye *** view) Pnt.Origin
            "target lies along local negative Z" |> expectEqual (Pnt.Origin *** view) (Pnt(0,0,-15))
            "view is orthogonal and right-handed" |> Expect.isTrue (view.IsOrthogonal && not view.IsMirroring)
            let point = Pnt(3,4,7)
            "view round trip" |> expectEqual ((point *** view) *** view.Inverse) point
        }

        test "Matrix look at respects custom up" {
            let view = Matrix.createLookAt (Vec(1,0,0)) Pnt.Origin (Pnt(0,0,-1))
            "world X becomes screen up" |> expectEqual (Pnt(1,0,-2) *** view) (Pnt(0,1,-2))
            "world negative Y becomes screen right" |> expectEqual (Pnt(0,-1,-2) *** view) (Pnt(1,0,-2))
        }

        test "Matrix perspective look at expected camera coordinates" {
            let eye = Pnt(10,20,30)
            let camera = Matrix.createPerspectiveLookAt(eye, Pnt(10,21,30), Vec(0,0,1), 2., 2., 1., 10.)
            "target at near center" |> expectEqual (Pnt(10,21,30) *** camera) Pnt.Origin
            "translated right and up" |> expectEqual (Pnt(11,22,31) *** camera) (Pnt(0.5,0.5,5.0/9.0))
            "translated far center" |> expectEqual (Pnt(10,30,30) *** camera) (Pnt(0,0,1))
        }

        test "Matrix Z up camera has no roll while pitching" {
            let eye = Pnt(10,-10,5)
            let view = Matrix.createLookAtZUp eye Pnt.Origin
            let aboveEye = (eye + Vec(0,0,1)) *** view
            "world Z has no screen-right component" |> Expect.isTrue (abs aboveEye.X < 1e-12)
            "world Z points toward screen up" |> Expect.isTrue (aboveEye.Y > 0.)
            "target direction includes pitch" |> expectEqual (Pnt.Origin *** view) (Pnt(0,0,-15))
            let camera = Matrix.createPerspectiveLookAtZUp(eye, Pnt.Origin, 2., 2., 1., 100.)
            "oblique target centered with correct depth" |> expectEqual (Pnt.Origin *** camera) (Pnt(0,0,280.0/297.0))
            let point = Pnt(2,3,4)
            let sequential = (point *** view) *** Matrix.createPerspectiveAlongNegZ(2., 2., 1., 100.)
            "combined view and projection" |> expectEqual (point *** camera) sequential
        }

        test "Matrix Z up vertical views use Y up fallback" {
            let eye = Pnt(3,4,5)
            for direction in [Vec(0,0,-1); Vec(0,0,1)] do
                let target = eye + direction
                let view = Matrix.createLookAtZUp eye target
                "vertical target" |> expectEqual (target *** view) (Pnt(0,0,-1))
                "world Y is fallback screen up" |> expectEqual ((eye + Vec(0,1,0)) *** view) (Pnt(0,1,0))
                "vertical view is orthogonal and right-handed" |> Expect.isTrue (view.IsOrthogonal && not view.IsMirroring)
                let camera = Matrix.createPerspectiveLookAtZUp(eye, target, 2., 2., 1., 10.)
                "vertical near center" |> expectEqual (target *** camera) Pnt.Origin
            let down = Matrix.createPerspectiveLookAtZUp(Pnt.Origin, Pnt(0,0,-1), 2., 2., 1., 10.)
            "down matches NegZ" |> Expect.isTrue (Matrix.equalsWithin 1e-12 down (Matrix.createPerspectiveAlongNegZ(2., 2., 1., 10.)))
            let forward = Matrix.createPerspectiveLookAtZUp(Pnt.Origin, Pnt(0,1,0), 2., 2., 1., 10.)
            "forward matches PosY" |> Expect.isTrue (Matrix.equalsWithin 1e-12 forward (Matrix.createPerspectiveAlongPosY(2., 2., 1., 10.)))
        }

        test "Matrix Z up nearly vertical fallback stays valid" {
            for horizontal in [1e-13; 1e-10] do
                let target = Pnt(horizontal,0,-1)
                let view = Matrix.createLookAtZUp Pnt.Origin target
                "nearly vertical view is orthogonal and right-handed" |> Expect.isTrue (view.IsOrthogonal && not view.IsMirroring)
                "nearly vertical target" |> expectEqual (target *** view) (Pnt(0,0,-1))
        }

        test "Matrix look at rejects degenerate input" {
            for target in [Pnt.Origin; Pnt(1e-14,0,0); Pnt(1e300,0,0)] do
                "invalid viewing direction" |> Expect.throws (fun () -> Matrix.createLookAt (Vec(0,0,1)) Pnt.Origin target |> ignore)
                "invalid ZUp viewing direction" |> Expect.throws (fun () -> Matrix.createLookAtZUp Pnt.Origin target |> ignore)
            for up in [Vec(0,0,0); Vec(0,1,0); Vec(0,-1,0); Vec(1e-14,1,0); Vec(1e300,0,0)] do
                "invalid up vector" |> Expect.throws (fun () -> Matrix.createLookAt up Pnt.Origin (Pnt(0,1,0)) |> ignore)
        }

        // Edge case tests for array creation
        test "Matrix from row array wrong length should fail" {
            let wrongArray = [| 1.0; 2.0; 3.0 |] // only 3 elements
            "wrong array length should throw" |> Expect.throws (fun () -> Matrix.createFromRowMajorArray(wrongArray) |> ignore)
        }

        test "Matrix from column array wrong length should fail" {
            let wrongArray = [| 1.0; 2.0; 3.0; 4.0; 5.0 |] // only 5 elements
            "wrong array length should throw" |> Expect.throws (fun () -> Matrix.createFromColumnMajorArray(wrongArray) |> ignore)
        }

        test "Matrix from row array too many elements should fail" {
            let wrongArray = Array.init 20 float // 20 elements
            "too many array elements should throw" |> Expect.throws (fun () -> Matrix.createFromRowMajorArray(wrongArray) |> ignore)
        }

        // Edge case tests for createMirror
        test "Matrix mirror on XY plane" {
            let plane = PPlane.WorldXY
            let m = Matrix.createMirror(plane)
            let a = Pnt(1,2,3)
            let b = a *** m
            "mirror on XY plane" |> expectEqual b (Pnt(1,2,-3))
            "mirror determinant is -1" |> Expect.isTrue (abs(m.Determinant + 1.0) < 1e-12)
            "mirror IsMirroring" |> Expect.isTrue m.IsMirroring
        }

        test "Matrix mirror on Right plane" {
            let plane = PPlane.WorldRight // YZ plane (X=0)
            let m = Matrix.createMirror plane
            let a = Pnt(3,2,1)
            let b = a *** m
            "mirror on Right plane flips X" |> expectEqual b (Pnt(-3,2,1))
        }

        // Edge case tests for createPlaneToPlane
        test "Matrix plane to plane transformation" {
            let fromPlane = PPlane.WorldXY
            let toPlane = PPlane.createFromAxesUnchecked(Pnt(1,2,3), UnitVec.xAxis, UnitVec.yAxis, UnitVec.zAxis)
            let m = Matrix.createPlaneToPlane fromPlane toPlane
            let a = Pnt(0,0,0) // origin of fromPlane
            let b = a *** m
            "plane to plane origin" |> expectEqual b (Pnt(1,2,3)) // should map to origin of toPlane
        }

        test "Matrix plane to plane with rotation" {
            let fromPlane = PPlane.WorldXY
            let toPlane = PPlane.createFromAxesUnchecked(Pnt.Origin, UnitVec.yAxis, -UnitVec.xAxis, UnitVec.zAxis) // 90° rotation
            let m = Matrix.createPlaneToPlane fromPlane toPlane
            let a = Pnt(1,0,0) // X axis point
            let b = a *** m
            "plane to plane rotation" |> expectEqual b (Pnt(0,1,0))
        }

        // Edge case tests for shear with multiple parameters
        test "Matrix shear multiple components" {
            let m = Matrix.createShear(0.5, 0.3, 0.2, 0.4, 0.1, 0.6)
            let a = Pnt(1,1,1)
            let b = a *** m
            let expected = Pnt(1.0 + 0.2 + 0.1, 1.0 + 0.5 + 0.6, 1.0 + 0.3 + 0.4)
            "multiple shear components" |> expectEqual b expected
        }

        // verify docstring for shear:

        // createShear(xy, xz, yx, yz, zx, zy)
        // xy - the amount to shear a X unit-vector a long Y-axis.
        // xz - the amount to shear a X unit-vector a long Z-axis.
        // yx - the amount to shear a Y unit-vector a long X-axis.
        // yz - the amount to shear a Y unit-vector a long Z-axis.
        // zx - the amount to shear a Z unit-vector a long X-axis.
        // zy - the amount to shear a Z unit-vector a long Y-axis.
        test "Matrix shear docstring verification xy=3" {
            let m = Matrix.createShear(3.,0.,0.,0.,0.,0.)
            let a = Pnt(2,1,3)
            let b = a *** m
            let expected = Pnt(2., 7. , 3.)
            "shear docstring verification xy=3" |> expectEqual b expected
        }

        test "Matrix shear docstring verification zx=0.5" {
            let zx = 0.5
            let m = Matrix.createShear(0.,0.,0.,0., zx, 0.)
            let a = Pnt(2,1,3)
            let b = a *** m
            let expected = Pnt( a.X + zx * a.Z,
                                a.Y,
                                a.Z)
            "shear docstring verification zx=0.5" |> expectEqual b expected
        }

        test "Matrix Radians factories match their Degrees partners" {
            let center = Pnt(3.5, -7.25, 11.0)
            let axis = Vec(1.0, -2.0, 0.5)
            let uAxis = axis.Unitized
            for deg in [ 0.0; 17.3; -123.4; 90.0; 271.5 ] do
                let rad = UtilEuclid.toRadians deg
                "X" |> Expect.isTrue (Matrix.equalsExact (Matrix.createRotationXDegrees deg) (Matrix.createRotationXRadians rad))
                "Y" |> Expect.isTrue (Matrix.equalsExact (Matrix.createRotationYDegrees deg) (Matrix.createRotationYRadians rad))
                "Z" |> Expect.isTrue (Matrix.equalsExact (Matrix.createRotationZDegrees deg) (Matrix.createRotationZRadians rad))
                "Vec" |> Expect.isTrue (Matrix.equalsExact (Matrix.createRotationFromVecDegrees axis deg) (Matrix.createRotationFromVecRadians axis rad))
                "UnitVec" |> Expect.isTrue (Matrix.equalsExact (Matrix.createRotationFromUnitVecDegrees uAxis deg) (Matrix.createRotationFromUnitVecRadians uAxis rad))
                "Around Vec" |> Expect.isTrue (Matrix.equalsExact (Matrix.createRotationFromVecDegreesAround center axis deg) (Matrix.createRotationFromVecRadiansAround center axis rad))
                "Around UnitVec" |> Expect.isTrue (Matrix.equalsExact (Matrix.createRotationFromUnitVecDegreesAround center uAxis deg) (Matrix.createRotationFromUnitVecRadiansAround center uAxis rad))
        }

        test "Matrix rotation around a center keeps the center fixed" {
            let center = Pnt(3.5, -7.25, 11.0)
            let axis = Vec(1.0, -2.0, 0.5)
            let m = Matrix.createRotationFromVecDegreesAround center axis 37.0
            "center is fixed" |> expectEqual (center *** m) center
            let p = Pnt(-4.0, 2.0, 9.0)
            let expected = center + Vec.rotate (Quaternion.createFromVecDegrees axis 37.0) (p - center)
            "rotates around center" |> expectEqual (p *** m) expected
        }

        test "Matrix curried factories support partial application" {
            let translateX = Matrix.createTranslationXYZ 5.0
            let m = translateX 6.0 7.0
            "partial translation" |> expectEqual (Pnt(1, 2, 3) *** m) (Pnt(6, 8, 10))
            let scaleXY = Matrix.createScale 2.0 3.0
            "partial scale" |> expectEqual (Pnt(1, 2, 3) *** scaleXY 4.0) (Pnt(2, 6, 12))
            let lookUp = Matrix.createLookAt Vec.zAxis (Pnt(10, -5, 3))
            "partial look at" |> Expect.isTrue (Matrix.equalsExact (lookUp (Pnt(1, 2, 3))) (Matrix.createLookAt Vec.zAxis (Pnt(10, -5, 3)) (Pnt(1, 2, 3))))
        }
        test "Matrix.andThen applies the current matrix first" {
            let rot = Matrix.createRotationFromVecDegreesAround (Pnt(3.0, -2.0, 5.0)) (Vec(1.0, 2.0, -0.5)) 33.0
            let move = Matrix.createTranslationXYZ 7.0 -4.0 2.5
            let p = Pnt(-6.0, 11.0, 4.0)
            let viaPipe = rot |> Matrix.andThen move
            "andThen equals Then" |> Expect.isTrue (Matrix.equalsExact viaPipe (rot.Then move))
            "rotation first, then translation" |> expectEqual (p *** viaPipe) ((p *** rot) *** move)
            "order matters" |> Expect.isFalse (Pnt.distanceTo (p *** viaPipe) (p *** (move |> Matrix.andThen rot)) < 1e-9)
        }

        test "Matrix.decompose finds scale, shear, rotation and translation" {
            let q = Quaternion.createFromVecDegrees (Vec(1.0, -2.0, 0.5)) 37.0
            let t = Vec(12.5, -7.25, 30.0)
            let m =
                Matrix.createScale 2.0 3.0 0.5
                *** Matrix.createShear(0, 0, 0.3, 0, -0.2, 0.4)
                *** Matrix.createFromQuaternion q
                *** Matrix.createTranslation t
            let d = Matrix.decompose m
            "translation" |> Expect.isTrue (Vec.equalsWithin 1e-12 d.Translation t)
            "rotation" |> Expect.isTrue (Quaternion.equalsRotationWithin 1e-12 d.Rotation q)
            "scale" |> Expect.isTrue (Vec.equalsWithin 1e-12 d.Scale (Vec(2.0, 3.0, 0.5)))
            Expect.floatClose Accuracy.veryHigh d.ShearYX 0.3 "shear YX"
            Expect.floatClose Accuracy.veryHigh d.ShearZX -0.2 "shear ZX"
            Expect.floatClose Accuracy.veryHigh d.ShearZY 0.4 "shear ZY"
            "recomposed" |> Expect.isTrue (Matrix.equalsWithin 1e-12 d.ToMatrix m)
            "createFromDecomposition" |> Expect.isTrue (Matrix.equalsExact (Matrix.createFromDecomposition d) d.ToMatrix)
            "instance member" |> Expect.isTrue (MatrixDecomposition.equalsExact (m.Decompose()) d)
            let p = Pnt(-3.0, 4.5, 8.0)
            "transforms points like the matrix" |> expectEqual (p *** d.ToMatrix) (p *** m)
        }

        test "Matrix.decompose of identity" {
            let d = Matrix.decompose Matrix.identity
            "identity" |> Expect.isTrue (MatrixDecomposition.equalsWithin 1e-15 d MatrixDecomposition.identity)
            "identity recomposed" |> Expect.isTrue (MatrixDecomposition.identity.ToMatrix.IsIdentity)
        }

        test "Matrix.decompose round-trips strong shear and tiny scale" {
            let q = Quaternion.createFromVecDegrees (Vec(-0.3, 0.8, 1.0)) -128.0
            let strongShear =
                Matrix.createShear(0.7, -1.5, 50.0, 2.0, -3.0, 0.1)
                *** Matrix.createFromQuaternion q
                *** Matrix.createTranslationXYZ -40.0 15.0 7.5
            "strong shear" |> Expect.isTrue (Matrix.equalsWithin 1e-9 (Matrix.decompose strongShear).ToMatrix strongShear)
            let tiny = Matrix.createScale 1e-9 2e-9 3e-9 *** Matrix.createFromQuaternion q
            "tiny scale" |> Expect.isTrue (Matrix.equalsWithin 1e-20 (Matrix.decompose tiny).ToMatrix tiny)
        }

        test "Matrix.decompose puts mirroring into a negative X scale" {
            let pl = PPlane.createFromXAxisAndYAxis (Vec(1.0, 1.0, 0.2)) (Vec(-0.5, 1.0, 2.0)) (Pnt(3.0, -2.0, 7.0))
            let m = Matrix.createMirror pl *** Matrix.createScale 1.5 1.5 1.5
            let d = Matrix.decompose m
            "negative X scale" |> Expect.isTrue (d.ScaleX < 0.0)
            "positive Y scale" |> Expect.isTrue (d.ScaleY > 0.0)
            "positive Z scale" |> Expect.isTrue (d.ScaleZ > 0.0)
            "recomposed" |> Expect.isTrue (Matrix.equalsWithin 1e-12 d.ToMatrix m)
            let flipX = Matrix.createScale -2.0 1.0 1.0
            let dx = Matrix.decompose flipX
            "only X flipped" |> Expect.isTrue (Vec.equalsWithin 1e-15 dx.Scale (Vec(-2.0, 1.0, 1.0)))
            "no rotation" |> Expect.isTrue (Quaternion.equalsRotationWithin 1e-15 dx.Rotation Quaternion.identity)
        }

        test "Matrix.tryDecompose rejects projecting and singular matrices" {
            let persp = Matrix.createPerspectiveLookAtZUp(Pnt(10, -5, 3), Pnt(0, 0, 0), 2.0, 1.5, 0.1, 100.0)
            "projecting" |> Expect.isValueNone (Matrix.tryDecompose persp)
            "projecting throws" |> Expect.throws (fun () -> Matrix.decompose persp |> ignore)
            "zero X scale" |> Expect.isValueNone (Matrix.tryDecompose (Matrix.createScale 0.0 1.0 1.0))
            "zero Z scale" |> Expect.isValueNone (Matrix.tryDecompose (Matrix.createScale 1.0 1.0 0.0))
            let parallelColumns = Matrix(1, 2, 0, 0,  1, 2, 0, 0,  0, 0, 1, 0,  0, 0, 0, 1)
            "parallel columns" |> Expect.isValueNone (Matrix.tryDecompose parallelColumns)
            "parallel columns instance" |> Expect.isValueNone (parallelColumns.TryDecompose())
        }

        test "Matrix.interpolate returns the exact end matrices" {
            let a = Matrix.createScale 2.0 1.0 0.5 *** Matrix.createRotationFromVecDegreesAround (Pnt(3, -2, 5)) (Vec(1, 2, -0.5)) 33.0
            let b = Matrix.createShear(0, 0, 0.2, 0, 0, 0) *** Matrix.createTranslationXYZ 7.0 -4.0 2.5
            "factor 0" |> Expect.isTrue (Matrix.equalsExact (Matrix.interpolate a b 0.0) a)
            "factor 1" |> Expect.isTrue (Matrix.equalsExact (Matrix.interpolate a b 1.0) b)
            "instance" |> Expect.isTrue (Matrix.equalsExact (a.Interpolate(b, 0.3)) (Matrix.interpolate a b 0.3))
        }

        test "Matrix.interpolate interpolates rotation angle, scale and translation" {
            let axis = Vec(1.0, -2.0, 0.5)
            let make s deg (t:Vec) = Matrix.createScale s s s *** Matrix.createRotationFromVecDegrees axis deg *** Matrix.createTranslation t
            let a = make 1.0 10.0 (Vec(10, 20, -5))
            let b = make 3.0 70.0 (Vec(-30, 4, 15))
            let expected = make 2.0 40.0 (Vec(-10, 12, 5))
            "middle" |> Expect.isTrue (Matrix.equalsWithin 1e-12 (Matrix.interpolate a b 0.5) expected)
            let quarter = make 1.5 25.0 (Vec(0, 16, 0))
            "quarter" |> Expect.isTrue (Matrix.equalsWithin 1e-12 (Matrix.interpolate a b 0.25) quarter)
            let beyond = make 4.0 100.0 (Vec(-50, -4, 25))
            "extrapolated" |> Expect.isTrue (Matrix.equalsWithin 1e-12 (Matrix.interpolate a b 1.5) beyond)
        }

        test "Matrix.interpolate keeps rotations rigid" {
            let a = Matrix.createRotationZDegrees 0.0
            let b = Matrix.createRotationZDegrees 170.0
            let m = Matrix.interpolate a b 0.5
            Expect.floatClose Accuracy.veryHigh m.ColumnVector1.Length 1.0 "no shrinking"
            "rotation by 85 degrees" |> Expect.isTrue (Matrix.equalsWithin 1e-12 m (Matrix.createRotationZDegrees 85.0))
            let c = Matrix.createRotationZDegrees 270.0
            "shortest rotation" |> Expect.isTrue (Matrix.equalsWithin 1e-12 (Matrix.interpolate a c 0.5) (Matrix.createRotationZDegrees -45.0))
        }

        test "Matrix.interpolate fails for invalid input" {
            let a = Matrix.createTranslationXYZ 1.0 2.0 3.0
            let persp = Matrix.createPerspectiveLookAtZUp(Pnt(10, -5, 3), Pnt(0, 0, 0), 2.0, 1.5, 0.1, 100.0)
            "NaN factor" |> Expect.throws (fun () -> Matrix.interpolate a a nan |> ignore)
            "infinite factor" |> Expect.throws (fun () -> Matrix.interpolate a a infinity |> ignore)
            "projecting start" |> Expect.throws (fun () -> Matrix.interpolate persp a 0.5 |> ignore)
            "singular finish" |> Expect.throws (fun () -> Matrix.interpolate a (Matrix.createScale 1.0 0.0 1.0) 0.0 |> ignore)
        }

        test "MatrixDecomposition.interpolate interpolates shear linearly" {
            let a = MatrixDecomposition.create (Vec(1, 2, 3)) Quaternion.identity (Vec(1, 1, 1)) 0.0 0.2 -0.4
            let b = MatrixDecomposition.create (Vec(3, 2, 1)) Quaternion.identity (Vec(2, 1, 0.5)) 1.0 0.4 0.4
            let m = MatrixDecomposition.interpolate a b 0.5
            let expected = MatrixDecomposition.create (Vec(2, 2, 2)) Quaternion.identity (Vec(1.5, 1, 0.75)) 0.5 0.3 0.0
            "middle" |> Expect.isTrue (MatrixDecomposition.equalsWithin 1e-15 m expected)
            "NaN factor" |> Expect.throws (fun () -> MatrixDecomposition.interpolate a b nan |> ignore)
        }
    ]
