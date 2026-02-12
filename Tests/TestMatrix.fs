module TestMatrix

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pnt.distance a b < 1e-12

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
            let q = Quaternion.createFromDegree(Vec.Zaxis*9.0, 90.)
            let m = Matrix.createFromQuaternion q
            let a = Pnt(9,0,3)
            let inv = m.Inverse
            let c = a |> Pnt.transform m |> Pnt.transform inv
            "rotateByQuaternion inverse" |> expectEqual a c
        }

        test "Matrix inverse transform rot ***" {
            let t = Matrix.createTranslation (Vec(-2,-3,4))
            let q = Quaternion.createFromDegree(Vec.Zaxis*9.0, 90.)
            let r = Matrix.createFromQuaternion q
            let m = t *** r
            let inv = m.Inverse
            let a = Pnt(1,2,3)
            let r = a  |> Pnt.transform m |> Pnt.transform inv
            "inverse transformRigid ok" |> expectEqual a r
        }

        test "Matrix inverse transform rot inv ***" {
            let t = Matrix.createTranslation (Vec(-2,-3,4))
            let q = Quaternion.createFromDegree(Vec.Zaxis*9.0, 90.)
            let r = Matrix.createFromQuaternion q
            let m = r *** t
            let inv = m.Inverse
            let a = Pnt(1,2,3)
            let r = a  |> Pnt.transform m |> Pnt.transform inv
            "inverse transformRigid ok" |> expectEqual a r
        }

        test "Matrix scaling uniform" {
            let m = Matrix.createScale(2.0, 2.0, 2.0)
            let a = Pnt(1,2,3)
            let b = a *** m
            "uniform scaling ok" |> expectEqual b (Pnt(2,4,6))

            let v = Vec(1,1,1)
            let vb = v *** m
            "uniform scaling vector ok" |> expectEqual vb.AsPnt (Vec(2,2,2).AsPnt)
        }

        test "Matrix scaling non-uniform" {
            let m = Matrix.createScale(2.0, 3.0, 0.5)
            let a = Pnt(1,2,4)
            let b = a *** m
            "non-uniform scaling ok" |> expectEqual b (Pnt(2,6,2))

            let inv = m.Inverse
            let c = b *** inv
            "scaling inverse ok" |> expectEqual c a
        }

        test "Matrix rotation X axis" {
            let m = Matrix.createRotationX(90.0)
            let a = Pnt(1,1,0)
            let b = a *** m
            "rotation X 90 degrees" |> expectEqual b (Pnt(1,0,1))

            let inv = m.Inverse
            let c = b *** inv
            "rotation X inverse" |> expectEqual c a
        }

        test "Matrix rotation Y axis" {
            let m = Matrix.createRotationY(90.0)
            let a = Pnt(1,0,1)
            let b = a *** m
            "rotation Y 90 degrees" |> expectEqual b (Pnt(1,0,-1))

            let inv = m.Inverse
            let c = b *** inv
            "rotation Y inverse" |> expectEqual c a
        }

        test "Matrix rotation Z axis" {
            let m = Matrix.createRotationZ(90.0)
            let a = Pnt(1,0,0)
            let b = a *** m
            "rotation Z 90 degrees" |> expectEqual b (Pnt(0,1,0))

            let inv = m.Inverse
            let c = b *** inv
            "rotation Z inverse" |> expectEqual c a
        }

        test "Matrix rotation arbitrary axis" {
            let axis = Vec(1,1,1)
            let m = Matrix.createRotationAxis(axis, 120.0)
            let a = Pnt(1,0,0)
            let b = a *** m
            let c = b *** m
            let d = c *** m
            "rotation 120 degrees x3 = identity" |> expectEqual d a
        }

        test "Matrix rotation with center" {
            let axis = Vec.Zaxis
            let center = Pnt(1,1,0)
            let m = Matrix.createRotationAxisCenter(axis, center, 90.0)
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
            let r = Matrix.createRotationZ(90.0)
            let s = Matrix.createScale(2.0, 2.0, 2.0)

            // Apply in order: Scale, then Rotate, then Translate
            let m = t *** r *** s
            let a = Pnt(1,0,0)
            let b = a *** m
            "TRS transformation" |> expectEqual b (Pnt(-4,4,6))
        }


        test "Matrix combined transformations TRS on Vec" {
            let t = Matrix.createTranslation(Vec(1,2,3)) // Vec ignores translation
            let r = Matrix.createRotationZ(90.0)
            let s = Matrix.createScale(2.0, 2.0, 2.0)

            // Apply in order: Scale, then Rotate, then Translate
            let m = t *** r *** s
            let a = Vec(1,0,0)
            let b = a *** m
            "TRS transformation" |> expectEqual b.AsPnt (Pnt(0,2,0))
        }


        test "Matrix vec to vec rotation" {
            let v1 = UnitVec.Xaxis
            let v2 = UnitVec.Yaxis
            let m = Matrix.createVecToVec(v1, v2)
            let result = v1 *** m
            "vec to vec rotation" |> expectEqual result.AsPnt (Vec(v2.X, v2.Y, v2.Z).AsPnt)
        }

        test "Matrix vec to vec same direction" {
            let v = UnitVec.create(1,1,1)
            let m = Matrix.createVecToVec(v, v)
            "same vector gives identity" |> Expect.isTrue m.IsIdentity
        }

        test "Matrix look-at Z-up" {
            let position = Pnt(2,3,4)
            let target = Pnt(3,3,4)
            let m = Matrix.createLookAt(position, target)

            let origin = Pnt(0,0,0) *** m
            "look-at origin to position" |> expectEqual origin position

            let forwardPoint = Pnt(0,0,1) *** m
            "look-at forward point" |> expectEqual forwardPoint (position + Vec(1,0,0))

            let rightVec = Vec(1,0,0) *** m
            "look-at right axis" |> expectEqual rightVec.AsPnt (Vec(0,1,0).AsPnt)

            let upVec = Vec(0,1,0) *** m
            "look-at up axis" |> expectEqual upVec.AsPnt (Vec(0,0,1).AsPnt)
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
            let m = Matrix.createScale(2.0, 3.0, 4.0)
            "scaling not IsIdentity" |> Expect.isFalse m.IsIdentity
            "scaling IsAffine" |> Expect.isTrue m.IsAffine
            "scaling IsScaling" |> Expect.isTrue m.IsScaling
            "scaling not IsTranslating" |> Expect.isFalse m.IsTranslating
            "scaling determinant is 24" |> Expect.isTrue (abs(m.Determinant - 24.0) < 1e-12)
        }

        test "Matrix properties rotation" {
            let m = Matrix.createRotationZ(45.0)
            "rotation not IsIdentity" |> Expect.isFalse m.IsIdentity
            "rotation IsAffine" |> Expect.isTrue m.IsAffine
            "rotation IsOrthogonal" |> Expect.isTrue m.IsOrthogonal
            "rotation not IsScaling" |> Expect.isFalse m.IsScaling
            "rotation determinant is 1" |> Expect.isTrue (abs(m.Determinant - 1.0) < 1e-12)
        }

        test "Matrix properties mirroring" {
            let m = Matrix.createScale(-1.0, 1.0, 1.0)
            "mirroring IsAffine" |> Expect.isTrue m.IsAffine
            "mirroring IsMirroring" |> Expect.isTrue m.IsMirroring
            "mirroring IsReflecting" |> Expect.isTrue m.IsReflecting
            "mirroring determinant is -1" |> Expect.isTrue (abs(m.Determinant + 1.0) < 1e-12)
        }

        test "Matrix from quaternion" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 90.0)
            let m = Matrix.createFromQuaternion(q)
            let a = Pnt(1,0,0)
            let b = a *** m
            "quaternion to matrix rotation" |> expectEqual b (Pnt(0,1,0))
        }

        test "Matrix column vectors" {
            let m = Matrix.createRotationZ(90.0)
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
            "matrix transpose" |> Expect.isTrue (Matrix.equals 1e-12 mt expected)
        }

        test "Matrix multiply associativity" {
            let a = Matrix.createTranslation(Vec(1,0,0))
            let b = Matrix.createRotationZ(90.0)
            let c = Matrix.createScale(2.0, 2.0, 2.0)

            let abc1 = (a *** b) *** c
            let abc2 = a *** (b *** c)
            "matrix multiplication associativity" |> Expect.isTrue (Matrix.equals 1e-12 abc1 abc2)
        }

        test "Matrix determinant scaling" {
            let m = Matrix.createScale(2.0, 3.0, 4.0)
            let det = m.Determinant
            "scaling determinant" |> Expect.isTrue (abs(det - 24.0) < 1e-12)
        }

        test "Matrix arrays conversion" {
            let m = Matrix(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
            let rowArray = m.ToArrayByRows
            let colArray = m.ToArrayByColumns

            let mFromRows = Matrix.createFromRowMajorArray(rowArray)
            let mFromCols = Matrix.createFromColumMajorArray(colArray)

            "matrix from row array" |> Expect.isTrue (Matrix.equals 1e-12 m mFromRows)
            "matrix from column array" |> Expect.isTrue (Matrix.equals 1e-12 m mFromCols)
        }

        test "Matrix add translation" {
            let m = Matrix.createRotationZ(90.0)
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
            let v1 = UnitVec.Xaxis
            let v2 = -UnitVec.Xaxis
            "opposite vectors should throw" |> Expect.throws (fun () -> Matrix.createVecToVec(v1, v2) |> ignore)
        }

        test "Matrix vec to vec nearly same vectors" {
            let v1 = UnitVec.create(1.0, 0.0, 0.0)
            let v2 = UnitVec.create(1.0, 1e-13, 0.0) // very close
            let m = Matrix.createVecToVec(v1, v2)
            "nearly same vectors should give identity" |> Expect.isTrue m.IsIdentity
        }

        test "Matrix vec to vec with zero-length Vec should fail" {
            let v1 = Vec(0.0, 0.0, 0.0)
            let v2 = Vec(1.0, 0.0, 0.0)
            "zero-length from vector should throw" |> Expect.throws (fun () -> Matrix.createVecToVec(v1, v2) |> ignore)
        }

        test "Matrix vec to vec with zero-length target Vec should fail" {
            let v1 = Vec(1.0, 0.0, 0.0)
            let v2 = Vec(0.0, 0.0, 0.0)
            "zero-length to vector should throw" |> Expect.throws (fun () -> Matrix.createVecToVec(v1, v2) |> ignore)
        }

        // Additional comprehensive tests for createVecToVec
        test "Matrix createVecToVec 90 degree rotation" {
            let from = UnitVec.Xaxis
            let to_ = UnitVec.Yaxis
            let m = Matrix.createVecToVec(from, to_)
            let result = from *** m

            "90° rotation X to Y" |> expectEqual result.AsPnt to_.AsPnt
        }

        test "Matrix createVecToVec 180 degree rotation alternative" {
            // Not exactly opposite, but very close to 180°
            let from = UnitVec.create(1, 0, 0)
            let to_ = UnitVec.create(-0.99999, 0.00001, 0)
            let m = Matrix.createVecToVec(from, to_)
            let result = from *** m

            // Should rotate successfully even though angle is very close to 180°
            "near-180° rotation works" |> Expect.floatClose Accuracy.low (Pnt.distance result.AsPnt to_.AsPnt) 0.0
        }

        test "Matrix createVecToVec with perpendicular vectors" {
            let from = UnitVec.Xaxis
            let to_ = UnitVec.Zaxis
            let m = Matrix.createVecToVec(from, to_)
            let result = from *** m

            "perpendicular rotation X to Z" |> expectEqual result.AsPnt to_.AsPnt
        }

        test "Matrix createVecToVec with arbitrary unit vectors" {
            let from = UnitVec.create(1, 2, 3)
            let to_ = UnitVec.create(3, 1, 2)
            let m = Matrix.createVecToVec(from, to_)
            let result = from *** m

            "arbitrary unit vector rotation" |> expectEqual result.AsPnt to_.AsPnt
        }

        test "Matrix createVecToVec Vec overload preserves orthogonality" {
            let from = Vec(2, 0, 0)
            let to_ = Vec(0, 3, 0)
            let m = Matrix.createVecToVec(from, to_)

            // Check that perpendicular vector stays perpendicular
            let perp = Vec(0, 0, 1)
            let rotatedPerp = perp *** m

            "perpendicular vector stays perpendicular" |> Expect.floatClose Accuracy.high (from *** rotatedPerp) 0.0
        }

        test "Matrix createVecToVec small angle rotation numerical stability" {
            // Test numerical stability with very small rotation angle
            let from = UnitVec.Xaxis
            let to_ = UnitVec.create(0.99999999, 0.0001, 0.0)  // ~0.57 degree angle
            let m = Matrix.createVecToVec(from, to_)
            let result = from *** m

            "small angle rotation numerically stable" |> Expect.floatClose Accuracy.medium (Pnt.distance result.AsPnt to_.AsPnt) 0.0
        }

        test "Matrix createVecToVec with different length vectors" {
            let from = Vec(5, 0, 0)
            let to_ = Vec(0, 0, 3)
            let m = Matrix.createVecToVec(from, to_)
            let result = from *** m

            // Should preserve the length of 'from' but rotate direction to 'to'
            "rotation preserves from vector length" |> Expect.floatClose Accuracy.high result.Length 5.0
            // Direction should match 'to'
            let resultDir = result.Unitized
            let toDir = to_.Unitized
            "rotation aligns direction" |> expectEqual resultDir.AsPnt toDir.AsPnt
        }

        test "Matrix createVecToVec is inverse of reverse rotation" {
            let from = UnitVec.create(1, 1, 0)
            let to_ = UnitVec.create(0, 1, 1)
            let m1 = Matrix.createVecToVec(from, to_)
            let m2 = Matrix.createVecToVec(to_, from)

            // m2 should be the inverse of m1
            let composed = m1 *** m2
            "forward then reverse gives identity" |> Expect.isTrue composed.IsIdentity
        }

        test "Matrix createVecToVec maintains handedness" {
            let from = UnitVec.Xaxis
            let to_ = UnitVec.Yaxis
            let m = Matrix.createVecToVec(from, to_)

            // Check that Z-axis rotates appropriately (preserves right-handedness)
            let zTransformed = UnitVec.Zaxis *** m
            "maintains right-handed coordinate system" |> Expect.floatClose Accuracy.high (zTransformed *** UnitVec.Zaxis) 1.0
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
            "zero-length axis should throw" |> Expect.throws (fun () -> Matrix.createRotationAxis(axis, 45.0) |> ignore)
        }

        test "Matrix rotation axis very short should fail" {
            let axis = Vec(1e-13, 0.0, 0.0)
            "very short axis should throw" |> Expect.throws (fun () -> Matrix.createRotationAxis(axis, 45.0) |> ignore)
        }

        // Edge case tests for IsProjecting
        test "Matrix perspective projection IsProjecting" {
            let m = Matrix.createPerspective(800.0, 600.0, 0.1, 100.0)
            "perspective matrix should be projecting" |> Expect.isTrue m.IsProjecting
            "perspective matrix should not be affine" |> Expect.isFalse m.IsAffine
        }

        // Edge case tests for createPerspective
        test "Matrix perspective negative near plane should fail" {
            "negative near plane should throw" |> Expect.throws (fun () -> Matrix.createPerspective(800.0, 600.0, -0.1, 100.0) |> ignore)
        }

        test "Matrix perspective zero near plane should fail" {
            "zero near plane should throw" |> Expect.throws (fun () -> Matrix.createPerspective(800.0, 600.0, 0.0, 100.0) |> ignore)
        }

        test "Matrix perspective negative far plane should fail" {
            "negative far plane should throw" |> Expect.throws (fun () -> Matrix.createPerspective(800.0, 600.0, 0.1, -100.0) |> ignore)
        }

        test "Matrix perspective near >= far should fail" {
            "near >= far should throw" |> Expect.throws (fun () -> Matrix.createPerspective(800.0, 600.0, 100.0, 50.0) |> ignore)
        }

        test "Matrix perspective near == far should fail" {
            "near == far should throw" |> Expect.throws (fun () -> Matrix.createPerspective(800.0, 600.0, 100.0, 100.0) |> ignore)
        }

        test "Matrix perspective valid parameters" {
            let m = Matrix.createPerspective(800.0, 600.0, 0.1, 100.0)
            "valid perspective should not throw" |> Expect.isTrue m.IsProjecting
            let a = Pnt(1,1,10)
            let b = a *** m
            "perspective transformation should work" |> Expect.isTrue (Pnt.distance b Pnt.Origin > 0.0)
        }

        // Edge case tests for array creation
        test "Matrix from row array wrong length should fail" {
            let wrongArray = [| 1.0; 2.0; 3.0 |] // only 3 elements
            "wrong array length should throw" |> Expect.throws (fun () -> Matrix.createFromRowMajorArray(wrongArray) |> ignore)
        }

        test "Matrix from column array wrong length should fail" {
            let wrongArray = [| 1.0; 2.0; 3.0; 4.0; 5.0 |] // only 5 elements
            "wrong array length should throw" |> Expect.throws (fun () -> Matrix.createFromColumMajorArray(wrongArray) |> ignore)
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
            let toPlane = PPlane.createUnchecked(Pnt(1,2,3), UnitVec.Xaxis, UnitVec.Yaxis, UnitVec.Zaxis)
            let m = Matrix.createPlaneToPlane(fromPlane, toPlane)
            let a = Pnt(0,0,0) // origin of fromPlane
            let b = a *** m
            "plane to plane origin" |> expectEqual b (Pnt(1,2,3)) // should map to origin of toPlane
        }

        test "Matrix plane to plane with rotation" {
            let fromPlane = PPlane.WorldXY
            let toPlane = PPlane.createUnchecked(Pnt.Origin, UnitVec.Yaxis, -UnitVec.Xaxis, UnitVec.Zaxis) // 90° rotation
            let m = Matrix.createPlaneToPlane(fromPlane, toPlane)
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
    ]