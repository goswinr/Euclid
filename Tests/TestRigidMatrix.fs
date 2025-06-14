module TestRigidMatrix

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

let notEqual a b msg =
    let same = eq a b
    if same then
        $"{msg} expected: \n{b.AsString}, got: \n{a.AsString}" |> Expect.isFalse same
    else
        msg |> Expect.isFalse same

#nowarn "44" // deprecated
let tests =
  testList "RigidMatrix transformations" [

    test "RigidMatrix Determinant " {
        let m = RigidMatrix.createTranslation (Vec(-9,3,188))
        let a = Accuracy.veryHigh
        "Determinant is one " |> Expect.floatClose Accuracy.high m.Determinant 1.0
    }

    test "RigidMatrix Determinant  rot" {
        let m = RigidMatrix.createFromQuaternion (Quaternion.createFromDegree(Vec(2,5,6), 16.))
        let a = Accuracy.veryHigh
        "Determinant is one " |> Expect.floatClose Accuracy.high m.Determinant 1.0
    }

    test "RigidMatrix translate " {
        let m = RigidMatrix.createTranslation (Vec(2,3,4)) |> RigidMatrix.addTranslation(Vec(4,1,-1))
        let a = Pnt(1,2,3)
        let b = a *** m
        "translation ok" |> expectEqual b (Pnt(7,6,6))
        let c = a |> Pnt.transformRigid m
        "transform ok" |> expectEqual c (Pnt(7,6,6))


        let m1 = RigidMatrix.createTranslation (Vec(2,3,4))
        let m2 = RigidMatrix.createTranslation (Vec(4,1,-1))
        let m3 = RigidMatrix.multiply( m1, m2)
        let d = a *** m3
        "multiply ok" |> expectEqual d (Pnt(7,6,6))
        let e = a |> Pnt.transformRigid m3
        "transform multiply ok" |> expectEqual e (Pnt(7,6,6))
    }

    test "RigidMatrix inverse id " {

        let m = RigidMatrix.identity
        let inv =  m.Inverse
        let a = Pnt(1,2,3)
        let ax = a *** m
        let a0 = ax *** inv
        "inverse identity ok" |> expectEqual a0 a
    }

    test "RigidMatrix inverse transform rot ***" {
            let t = RigidMatrix.createTranslation (Vec(-2,-3,4))
            let q = Quaternion.createFromDegree(Vec.Zaxis*9.0, 90.)
            let r = RigidMatrix.createFromQuaternion q
            let m = t *** r
            let inv = m.Inverse
            let a = Pnt(1,2,3)
            let r = a  |> Pnt.transformRigid m |> Pnt.transformRigid inv
            "inverse transformRigid ok" |> expectEqual a r
    }

    test "RigidMatrix inverse transform rot inv ***" {
            let t = RigidMatrix.createTranslation (Vec(-2,-3,4))
            let q = Quaternion.createFromDegree(Vec.Zaxis*9.0, 90.)
            let r = RigidMatrix.createFromQuaternion q
            let m = r *** t
            let inv = m.Inverse
            let a = Pnt(1,2,3)
            let r = a  |> Pnt.transformRigid m |> Pnt.transformRigid inv
            "inverse transformRigid ok" |> expectEqual a r
    }

    test "RigidMatrix inverse transformRigid  " {
            let m = RigidMatrix.createTranslation (Vec(-2,-3,4))
            let inv = m.Inverse
            let a = Pnt(1,2,3)
            let ax = a *** m
            let a0 = ax *** inv
            "inverse *** ok" |> expectEqual a0 a
        }


    test "RigidMatrix inverse rotate " {
        let q = Quaternion.createFromDegree(Vec.Zaxis*9.0, 90.)
        let m = RigidMatrix.createFromQuaternion q
        let a = Pnt(9,0,3)
        let inv = m.Inverse
        let c = a |> Pnt.transformRigid m |> Pnt.transformRigid inv
        "rotateByQuaternion inverse" |> expectEqual a c
    }

    test "RigidMatrix rotation X axis" {
        let m = RigidMatrix.createRotationX(90.0)
        let p = Pnt(0, 1, 0)
        let result = p *** m
        "90° rotation around X-axis" |> expectEqual result (Pnt(0, 0, 1))
    }

    test "RigidMatrix rotation Y axis" {
        let m = RigidMatrix.createRotationY(90.0)
        let p = Pnt(1, 0, 0)
        let result = p *** m
        "90° rotation around Y-axis" |> expectEqual result (Pnt(0, 0, -1))
    }

    test "RigidMatrix rotation Z axis" {
        let m = RigidMatrix.createRotationZ(90.0)
        let p = Pnt(1, 0, 0)
        let result = p *** m
        "90° rotation around Z-axis" |> expectEqual result (Pnt(0, 1, 0))
    }

    test "RigidMatrix rotation arbitrary axis" {
        let axis = Vec(1, 1, 1)
        let m = RigidMatrix.createRotationAxis(axis, 120.0)
        let p = Pnt(1, 0, 0)
        let result = p *** m
        let resultBack = result *** m *** m
        "120° rotation around (1,1,1) three times returns to start" |> expectEqual resultBack p
    }

    test "RigidMatrix rotation with center point" {
        let axis = Vec.Zaxis
        let center = Pnt(1, 1, 0)
        let m = RigidMatrix.createRotationAxisCenter(axis, center, 90.0)
        let p = Pnt(2, 1, 0)
        let result = p *** m
        "90° rotation around Z at center (1,1,0)" |> expectEqual result (Pnt(1, 2, 0))
    }

    test "RigidMatrix vec to vec rotation" {
        let fromVec = UnitVec.Xaxis
        let toVec = UnitVec.Yaxis
        let m = RigidMatrix.createVecToVec(fromVec, toVec)
        let result = fromVec *** m
        "rotation from X to Y axis" |> expectEqual result.AsPnt toVec.AsPnt
    }

    test "RigidMatrix vec to vec same direction" {
        let vec = UnitVec.create(1, 2, 3)
        let m = RigidMatrix.createVecToVec(vec, vec)
        "same direction should give identity" |> Expect.isTrue m.IsIdentity
    }

    test "RigidMatrix from quaternion identity" {
        let q = Quaternion.identity
        let m = RigidMatrix.createFromQuaternion(q)
        "quaternion identity creates matrix identity" |> Expect.isTrue m.IsIdentity
    }

    test "RigidMatrix composition order" {
        let t1 = RigidMatrix.createTranslation(Vec(1, 0, 0))
        let r1 = RigidMatrix.createRotationZ(90.0)
        let p = Pnt(1, 0, 0)

        let m1 = t1 *** r1  // translate then rotate
        let result1 = p *** m1

        let m2 = r1 *** t1  // rotate then translate
        let result2 = p *** m2

        "composition order matters" |> notEqual result1 result2
        "translate then rotate" |> expectEqual result1 (Pnt(0, 2, 0))
        "rotate then translate" |> expectEqual result2 (Pnt(1, 1, 0))
    }

    test "RigidMatrix vector transformation ignores translation" {
        let m = RigidMatrix.createTranslation(Vec(10, 20, 30))
        let v = Vec(1, 2, 3)
        let result = v *** m
        "vector transformation ignores translation" |> expectEqual result.AsPnt v.AsPnt
    }

    test "RigidMatrix unit vector transformation preserves length" {
        let m = RigidMatrix.createRotationAxis(Vec(1, 1, 1), 45.0)
        let uv = Vec.create(1, 2, 3)
        let result = uv *** m
        "unit vector length preserved" |> Expect.floatClose Accuracy.high result.Length uv.Length
    }

    test "RigidMatrix remove translation" {
        let m = RigidMatrix.createTranslation(Vec(5, 6, 7))
        let m2 = RigidMatrix.removeTranslation(m)
        "remove translation creates identity" |> Expect.isTrue m2.IsIdentity
    }

    test "RigidMatrix add translation" {
        let m1 = RigidMatrix.createRotationZ(45.0)
        let m2 = RigidMatrix.addTranslation(Vec(1, 2, 3)) m1
        let expected = m1 *** RigidMatrix.createTranslation(Vec(1, 2, 3))
        "add translation equivalent to multiplication" |> Expect.isTrue (RigidMatrix.equals 1e-12 m2 expected)
    }

    test "RigidMatrix to/from matrix conversion" {
        let rm = RigidMatrix.createTranslation(Vec(1, 2, 3)) *** RigidMatrix.createRotationZ(30.0)
        let matrix = rm.Matrix
        let rmBack = RigidMatrix.createFromMatrix(matrix)
        "round trip conversion preserves matrix" |> Expect.isTrue (RigidMatrix.equals 1e-12 rm rmBack)
    }

    test "RigidMatrix plane transformations" {
        let plane = PPlane.WorldXY
        let m = RigidMatrix.createToPlane(plane)
        "to world XY plane is identity" |> Expect.isTrue m.IsIdentity
    }

    test "RigidMatrix plane to plane" {
        let fromPlane = PPlane.WorldXY
        let toPlane = PPlane.createOriginXaxisYaxis(Pnt.Origin, UnitVec.Zaxis, UnitVec.Xaxis)
        let m = RigidMatrix.createPlaneToPlane(fromPlane, toPlane)
        let p = Pnt(1, 0, 0)
        let result = p *** m
        "transform from XY to YZ plane" |> expectEqual result (Pnt(0, 0, 1))
    }

    test "RigidMatrix determinant is one" {
        let matrices = [
            RigidMatrix.identity
            RigidMatrix.createTranslation(Vec(1, 2, 3))
            RigidMatrix.createRotationX(45.0)
            RigidMatrix.createRotationAxis(Vec(1, 2, 3), 67.0)
            RigidMatrix.createFromQuaternion(Quaternion.createFromDegree(Vec(1, 1, 1), 30.0))
        ]
        for m in matrices do
            "determinant is 1.0" |> Expect.floatClose Accuracy.high m.Determinant 1.0
    }

    test "RigidMatrix column vectors are orthonormal" {
        let m = RigidMatrix.createRotationAxis(Vec(1, 2, 3), 42.0)
        let c1 = m.ColumnVector1
        let c2 = m.ColumnVector2
        let c3 = m.ColumnVector3

        "column 1 is unit length" |> Expect.floatClose Accuracy.high c1.Length 1.0
        "column 2 is unit length" |> Expect.floatClose Accuracy.high c2.Length 1.0
        "column 3 is unit length" |> Expect.floatClose Accuracy.high c3.Length 1.0
        "columns 1&2 orthogonal" |> Expect.floatClose Accuracy.high (c1 *** c2) 0.0
        "columns 1&3 orthogonal" |> Expect.floatClose Accuracy.high (c1 *** c3) 0.0
        "columns 2&3 orthogonal" |> Expect.floatClose Accuracy.high (c2 *** c3) 0.0
    }

    test "RigidMatrix multiple transformations preserve properties" {
        let m1 = RigidMatrix.createTranslation(Vec(1, 2, 3))
        let m2 = RigidMatrix.createRotationX(30.0)
        let m3 = RigidMatrix.createRotationY(45.0)
        let m4 = RigidMatrix.createRotationZ(60.0)
        let combined = m1 *** m2 *** m3 *** m4

        "combined determinant is 1.0" |> Expect.floatClose Accuracy.high combined.Determinant 1.0
        let inv = combined.Inverse
        let identity = combined *** inv
        "combined with inverse is identity" |> Expect.isTrue identity.IsIdentity
    }

    test "RigidMatrix equals with tolerance" {
        let m1 = RigidMatrix.createRotationZ(45.0)
        let m2 = RigidMatrix.createRotationZ(45.0000001)
        let m3 = RigidMatrix.createRotationZ(46.0)

        "matrices equal within tolerance" |> Expect.isTrue (RigidMatrix.equals 1e-6 m1 m2)
        "matrices not equal outside tolerance" |> Expect.isFalse (RigidMatrix.equals 1e-6 m1 m3)
    }

    test "RigidMatrix zero rotation angle" {
        let m1 = RigidMatrix.createRotationX(0.0)
        let m2 = RigidMatrix.createRotationY(0.0)
        let m3 = RigidMatrix.createRotationZ(0.0)

        "zero X rotation is identity" |> Expect.isTrue m1.IsIdentity
        "zero Y rotation is identity" |> Expect.isTrue m2.IsIdentity
        "zero Z rotation is identity" |> Expect.isTrue m3.IsIdentity
    }

    test "RigidMatrix 360 degree rotation" {
        let p = Pnt(1, 2, 3)
        let m1 = RigidMatrix.createRotationX(360.0)
        let m2 = RigidMatrix.createRotationY(360.0)
        let m3 = RigidMatrix.createRotationZ(360.0)

        "360° X rotation returns to start" |> expectEqual (p *** m1) p
        "360° Y rotation returns to start" |> expectEqual (p *** m2) p
        "360° Z rotation returns to start" |> expectEqual (p *** m3) p
    }

    test "RigidMatrix very small rotation angles" {
        let smallAngle = 1e-10
        let p = Pnt(1, 0, 0)
        let m = RigidMatrix.createRotationZ(smallAngle)
        let result = p *** m

        "very small rotation preserves approximately same position" |> Expect.floatClose Accuracy.medium (Pnt.distance p result) 0.0
    }

    test "RigidMatrix large rotation angles" {
        let largeAngle = 7200.0  // 20 full rotations
        let p = Pnt(1, 0, 0)
        let m = RigidMatrix.createRotationZ(largeAngle)
        let result = p *** m

        "large angle rotation equivalent to modulo 360" |> expectEqual result p
    }

    test "RigidMatrix negative rotation angles" {
        let p = Pnt(1, 0, 0)
        let m1 = RigidMatrix.createRotationZ(90.0)
        let m2 = RigidMatrix.createRotationZ(-90.0)
        let result1 = p *** m1
        let result2 = p *** m2

        "negative rotation is opposite direction" |> expectEqual result1 (Pnt(0, 1, 0))
        "negative 90° Z rotation" |> expectEqual result2 (Pnt(0, -1, 0))
    }

    test "RigidMatrix rotation axis too short throws exception" {
        let tinyAxis = Vec(1e-15, 1e-15, 1e-15)
        Expect.throws (fun () -> RigidMatrix.createRotationAxis(tinyAxis, 45.0) |> ignore) "Should throw for tiny axis"
    }

    test "RigidMatrix vec to vec opposite vectors throws exception" {
        let vec1 = UnitVec.Xaxis
        let vec2 = -vec1
        Expect.throws (fun () -> RigidMatrix.createVecToVec(vec1, vec2) |> ignore) "Should throw for opposite vectors"
    }

    test "RigidMatrix vec to vec with zero vectors throws exception" {
        let zeroVec = Vec(0, 0, 0)
        let normalVec = Vec(1, 0, 0)
        Expect.throws (fun () -> RigidMatrix.createVecToVec(zeroVec, normalVec) |> ignore) "Should throw for zero from vector"
        Expect.throws (fun () -> RigidMatrix.createVecToVec(normalVec, zeroVec) |> ignore) "Should throw for zero to vector"
    }

    test "RigidMatrix createFromMatrix with scaling matrix fails" {
        let scalingMatrix = Matrix.createScale(2.0, 2.0, 2.0)
        let result = RigidMatrix.tryCreateFromMatrix(scalingMatrix)
        "scaling matrix should return None" |> Expect.isNone result

        Expect.throws (fun () -> RigidMatrix.createFromMatrix(scalingMatrix) |> ignore) "Should throw for scaling matrix"
    }

    test "RigidMatrix createFromMatrix with projection matrix fails" {
        let projectionMatrix = Matrix.createPerspective(90.0, 1.0, 0.1, 100.0)
        let result = RigidMatrix.tryCreateFromMatrix(projectionMatrix)
        "projection matrix should return None" |> Expect.isNone result
    }

    test "RigidMatrix createFromMatrix with shear matrix fails" {
        // Create a shear matrix manually
        let shearMatrix = Matrix(
            1.0, 0.5, 0.0, 0.0,
            0.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 0.0, 1.0)
        let result = RigidMatrix.tryCreateFromMatrix(shearMatrix)
        "shear matrix should return None" |> Expect.isNone result
    }

    test "RigidMatrix extreme translation values" {
        let largeTranslation = Vec(1e10, -1e10, 1e10)
        let m = RigidMatrix.createTranslation(largeTranslation)
        let p = Pnt.Origin
        let result = p *** m

        "large translation works correctly" |> expectEqual result (Pnt(1e10, -1e10, 1e10))
        "determinant still 1.0 with large translation" |> Expect.floatClose Accuracy.high m.Determinant 1.0
    }

    test "RigidMatrix very small translation values" {
        let tinyTranslation = Vec(1e-15, 1e-15, 1e-15)
        let m = RigidMatrix.createTranslation(tinyTranslation)
        let p = Pnt(1, 1, 1)
        let result = p *** m

        "tiny translation preserved" |> Expect.floatClose Accuracy.veryHigh (result.X - p.X) 1e-15
    }

    test "RigidMatrix numerical precision with multiple operations" {
        let p = Pnt(1, 2, 3)
        let mutable current = p
        let rotation = RigidMatrix.createRotationZ(1.0)  // 1 degree

        // Apply 360 small rotations (should return to start)
        for i in 1..360 do
            current <- current *** rotation

        "360 small rotations return approximately to start" |> Expect.floatClose Accuracy.low (Pnt.distance p current) 0.0
    }

    test "RigidMatrix inverse numerical stability" {
        let matrices = [
            RigidMatrix.createRotationAxis(Vec(1, 1, 1), 179.9)  // Nearly 180 degrees
            RigidMatrix.createRotationX(0.001)  // Very small angle
            RigidMatrix.createTranslation(Vec(1e-10, 1e-10, 1e-10))  // Tiny translation
        ]

        for m in matrices do
            let inv = m.Inverse
            let identity = m *** inv
            "inverse is numerically stable" |> Expect.isTrue identity.IsIdentity
    }

    test "RigidMatrix addTranslation with zero vector" {
        let m1 = RigidMatrix.createRotationZ(45.0)
        let m2 = RigidMatrix.addTranslation(Vec.Zero) m1
        "adding zero translation doesn't change matrix" |> Expect.isTrue (RigidMatrix.equals 1e-15 m1 m2)
    }

    test "RigidMatrix removeTranslation from identity" {
        let m1 = RigidMatrix.identity
        let m2 = RigidMatrix.removeTranslation(m1)
        "removing translation from identity gives identity" |> Expect.isTrue m2.IsIdentity
    }

    test "RigidMatrix createRotationAxisCenter with origin center" {
        let axis = Vec(0, 0, 1)
        let center = Pnt.Origin
        let angle = 90.0
        let m1 = RigidMatrix.createRotationAxisCenter(axis, center, angle)
        let m2 = RigidMatrix.createRotationZ(angle)

        "rotation around origin same as simple rotation" |> Expect.isTrue (RigidMatrix.equals 1e-15 m1 m2)
    }

    test "RigidMatrix equals with zero tolerance" {
        let m1 = RigidMatrix.createRotationZ(45.0)
        let m2 = RigidMatrix.createRotationZ(45.0)
        let m3 = RigidMatrix.createRotationZ(45.0000001)

        "identical matrices equal with zero tolerance" |> Expect.isTrue (RigidMatrix.equals 0.0 m1 m2)
        "slightly different matrices not equal with zero tolerance" |> Expect.isFalse (RigidMatrix.equals 0.0 m1 m3)
    }

    test "RigidMatrix column vectors form right-handed system" {
        let m = RigidMatrix.createRotationAxis(Vec(1, 2, 3), 42.0)
        let c1 = m.ColumnVector1
        let c2 = m.ColumnVector2
        let c3 = m.ColumnVector3

        let cross = Vec.cross(c1, c2)
        let dotWithC3 = cross *** c3

        "cross product of first two columns equals third (right-handed)" |> Expect.floatClose Accuracy.high dotWithC3 1.0
    }

    test "RigidMatrix transformation preserves distances" {
        let m = RigidMatrix.createRotationAxis(Vec(1, 1, 1), 67.0) *** RigidMatrix.createTranslation(Vec(5, -3, 2))
        let p1 = Pnt(1, 2, 3)
        let p2 = Pnt(4, 5, 6)
        let originalDistance = Pnt.distance p1 p2

        let tp1 = p1 *** m
        let tp2 = p2 *** m
        let transformedDistance = Pnt.distance tp1 tp2

        "rigid transformation preserves distances" |> Expect.floatClose Accuracy.high transformedDistance originalDistance
    }

    test "RigidMatrix transformation preserves angles" {
        let m = RigidMatrix.createRotationY(30.0) *** RigidMatrix.createTranslation(Vec(1, 2, 3))
        let v1 = Vec(1, 0, 0)
        let v2 = Vec(0, 1, 0)
        let originalAngle = Vec.angle180 v1 v2

        let tv1 = v1 *** m
        let tv2 = v2 *** m
        let transformedAngle = Vec.angle180 tv1 tv2

        "rigid transformation preserves angles" |> Expect.floatClose Accuracy.high transformedAngle originalAngle
    }

  ]