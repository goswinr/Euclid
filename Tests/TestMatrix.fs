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

#nowarn "44" // deprecated
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

  ]