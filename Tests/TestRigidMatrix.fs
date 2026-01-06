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


let tests =
    testList "RigidMatrix transformations" [


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
            for _ = 1 to 360 do
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

        // ===== Tests for Non-Unit Vector Overloads =====

        test "RigidMatrix createRotationAxis with non-unit Vec" {
            let axis = Vec(2, 4, 6)  // Non-unit vector
            let m = RigidMatrix.createRotationAxis(axis, 90.0)
            let p = Pnt(1, 0, 0)
            let result = p *** m

            // Should give same result as unitized axis
            let unitAxis = UnitVec.create(2, 4, 6)
            let mUnit = RigidMatrix.createRotationAxis(unitAxis, 90.0)
            let resultUnit = p *** mUnit

            "non-unit axis gives same result as unitized" |> expectEqual result resultUnit
        }

        test "RigidMatrix createRotationAxisCenter with non-unit Vec" {
            let axis = Vec(1, 2, 1)  // Non-unit vector
            let center = Pnt(2, 3, 4)
            let m = RigidMatrix.createRotationAxisCenter(axis, center, 45.0)

            // Should give same result as unitized axis
            let unitAxis = UnitVec.create(1, 2, 1)
            let mUnit = RigidMatrix.createRotationAxisCenter(unitAxis, center, 45.0)

            "non-unit axis center rotation same as unitized" |> Expect.isTrue (RigidMatrix.equals 1e-12 m mUnit)
        }

        test "RigidMatrix createVecToVec with non-unit Vec" {
            let fromVec = Vec(3, 0, 0)  // Non-unit
            let toVec = Vec(0, 5, 0)    // Non-unit
            let m = RigidMatrix.createVecToVec(fromVec, toVec)

            // The rotation rotates the direction, but doesn't change the length
            let result = fromVec *** m
            // Result should be in Y direction with same length as fromVec (which is 3)
            let expected = Vec(0, 3, 0)

            "non-unit vec to vec rotation preserves length" |> expectEqual result.AsPnt expected.AsPnt
        }

        // ===== Tests for Reflection Detection =====

        test "RigidMatrix create with reflection matrix fails" {
            // Create a reflection matrix (determinant = -1)
            let reflectX = -1.0
            Expect.throws (fun () ->
                RigidMatrix.create(
                    reflectX, 0.0, 0.0, 0.0,
                    0.0, 1.0, 0.0, 0.0,
                    0.0, 0.0, 1.0, 0.0) |> ignore
            ) "Should throw for reflection matrix"
        }

        test "RigidMatrix tryCreateFromMatrix with reflection fails" {
            let reflectionMatrix = Matrix(
                -1.0, 0.0, 0.0, 0.0,
                0.0, 1.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                0.0, 0.0, 0.0, 1.0)
            let result = RigidMatrix.tryCreateFromMatrix(reflectionMatrix)
            "reflection matrix should return None" |> Expect.isNone result
        }

        // ===== Tests for Nearly-Identical Vectors =====

        test "RigidMatrix createVecToVec with nearly identical vectors" {
            let vec1 = UnitVec.Xaxis
            let vec2 = UnitVec.create(1.0, 1e-13, 0.0)  // Almost identical
            let m = RigidMatrix.createVecToVec(vec1, vec2)

            "nearly identical vectors give identity" |> Expect.isTrue m.IsIdentity
        }

        test "RigidMatrix createVecToVec Vec overload with nearly identical vectors" {
            let vec1 = Vec(5, 0, 0)
            let vec2 = Vec(5.0, 1e-13, 0.0)  // Almost identical when unitized
            let m = RigidMatrix.createVecToVec(vec1, vec2)

            "nearly identical non-unit vectors give identity" |> Expect.isTrue m.IsIdentity
        }

        // ===== Tests for Individual Axis Translations =====

        test "RigidMatrix createTranslationX" {
            let m = RigidMatrix.createTranslationX(5.0)
            let p = Pnt(1, 2, 3)
            let result = p *** m
            "X translation only" |> expectEqual result (Pnt(6, 2, 3))
        }

        test "RigidMatrix createTranslationY" {
            let m = RigidMatrix.createTranslationY(7.0)
            let p = Pnt(1, 2, 3)
            let result = p *** m
            "Y translation only" |> expectEqual result (Pnt(1, 9, 3))
        }

        test "RigidMatrix createTranslationZ" {
            let m = RigidMatrix.createTranslationZ(-2.0)
            let p = Pnt(1, 2, 3)
            let result = p *** m
            "Z translation only" |> expectEqual result (Pnt(1, 2, 1))
        }

        test "RigidMatrix addTranslationXYZ" {
            let m1 = RigidMatrix.createRotationZ(45.0)
            let m2 = RigidMatrix.addTranslationXYZ 1.0 2.0 3.0 m1
            let expected = RigidMatrix.addTranslation(Vec(1, 2, 3)) m1
            "addTranslationXYZ same as addTranslation Vec" |> Expect.isTrue (RigidMatrix.equals 1e-15 m2 expected)
        }

        // ===== Tests for Array Conversions =====

        test "RigidMatrix ToArrayByColumns ordering" {
            // Use identity matrix (which is a valid RigidMatrix)
            let m = RigidMatrix.identity
            let arr = m.ToArrayByColumns
            // Columns: (1,0,0), (0,1,0), (0,0,1), (0,0,0)
            let expected = [| 1.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0; 1.0; 0.0; 0.0; 0.0 |]

            "column-major ordering correct" |> Expect.equal arr expected
        }

        test "RigidMatrix ToArrayByRows ordering" {
            // Use a rotation matrix with translation
            let m = RigidMatrix.createRotationZ(90.0) *** RigidMatrix.createTranslation(Vec(1, 2, 3))
            let arr = m.ToArrayByRows
            // Should be in row order: M11, M21, M31, X41, M12, M22, M32, Y42, M13, M23, M33, Z43

            "row-major ordering length correct" |> Expect.equal arr.Length 12
            // Verify it can be reconstructed
            let mBack = RigidMatrix.create(arr.[0], arr.[1], arr.[2], arr.[3], arr.[4], arr.[5], arr.[6], arr.[7], arr.[8], arr.[9], arr.[10], arr.[11])
            "reconstructed matrix equals original" |> Expect.isTrue (RigidMatrix.equals 1e-15 m mBack)
        }

        // ===== Tests for Translation Property and UnitVec Preservation =====

        test "RigidMatrix Translation property" {
            let trans = Vec(10, 20, 30)
            let m = RigidMatrix.createTranslation(trans)
            let result = m.Translation

            "Translation property returns correct vector" |> expectEqual result.AsPnt trans.AsPnt
        }

        test "RigidMatrix UnitVec transformation preserves unit length explicitly" {
            let m = RigidMatrix.createRotationAxis(Vec(1, 1, 1), 67.0) *** RigidMatrix.createTranslation(Vec(5, -3, 2))
            let uv = UnitVec.create(2, 3, 4)
            let result = uv *** m

            // UnitVec is always unit length by design, verify it's still a valid UnitVec
            "UnitVec transformation returns UnitVec" |> Expect.floatClose Accuracy.high result.AsVec.Length 1.0
        }

        // ===== Additional Edge Cases =====

        test "RigidMatrix large translation values" {
            let largeTrans = Vec(1e10, -1e10, 1e10)
            let m = RigidMatrix.createTranslation(largeTrans)
            let p = Pnt(1, 2, 3)
            let result = p *** m

            "large translation works" |> expectEqual result (Pnt(1e10 + 1.0, -1e10 + 2.0, 1e10 + 3.0))
        }

        test "RigidMatrix multiple matrix compositions" {
            let m1 = RigidMatrix.createRotationX(30.0)
            let m2 = RigidMatrix.createRotationY(45.0)
            let m3 = RigidMatrix.createRotationZ(60.0)
            let m4 = RigidMatrix.createTranslation(Vec(1, 2, 3))

            let combined = m1 *** m2 *** m3 *** m4
            let p = Pnt(1, 0, 0)
            let result = p *** combined

            // Should apply transformations in order
            let expected = p *** m1 *** m2 *** m3 *** m4
            "multiple compositions work correctly" |> expectEqual result expected
        }

        test "RigidMatrix column vectors extraction" {
            let m = RigidMatrix.createRotationZ(45.0)
            let _c1 = m.ColumnVector1
            let _c2 = m.ColumnVector2
            let c3 = m.ColumnVector3

            // For Z rotation, third column should be Z-axis
            "third column is Z-axis for Z rotation" |> expectEqual c3.AsPnt Vec.Zaxis.AsPnt
        }

        // ===== Additional createVecToVec Edge Case Tests =====

        test "RigidMatrix createVecToVec 90 degree rotation" {
            let from = UnitVec.Xaxis
            let to_ = UnitVec.Yaxis
            let m = RigidMatrix.createVecToVec(from, to_)
            let result = from *** m

            "90° rotation X to Y" |> expectEqual result.AsPnt to_.AsPnt
        }

        test "RigidMatrix createVecToVec 180 degree rotation alternative" {
            // Not exactly opposite, but very close to 180°
            let from = UnitVec.create(1, 0, 0)
            let to_ = UnitVec.create(-0.99999, 0.00001, 0)
            let m = RigidMatrix.createVecToVec(from, to_)
            let result = from *** m

            // Should rotate successfully even though angle is very close to 180°
            "near-180° rotation works" |> Expect.floatClose Accuracy.low (Pnt.distance result.AsPnt to_.AsPnt) 0.0
        }

        test "RigidMatrix createVecToVec with perpendicular vectors" {
            let from = UnitVec.Xaxis
            let to_ = UnitVec.Zaxis
            let m = RigidMatrix.createVecToVec(from, to_)
            let result = from *** m

            "perpendicular rotation X to Z" |> expectEqual result.AsPnt to_.AsPnt
        }

        test "RigidMatrix createVecToVec with arbitrary unit vectors" {
            let from = UnitVec.create(1, 2, 3)
            let to_ = UnitVec.create(3, 1, 2)
            let m = RigidMatrix.createVecToVec(from, to_)
            let result = from *** m

            "arbitrary unit vector rotation" |> expectEqual result.AsPnt to_.AsPnt
        }

        test "RigidMatrix createVecToVec Vec overload preserves orthogonality" {
            let from = Vec(2, 0, 0)
            let to_ = Vec(0, 3, 0)
            let m = RigidMatrix.createVecToVec(from, to_)

            // Check that perpendicular vector stays perpendicular
            let perp = Vec(0, 0, 1)
            let rotatedPerp = perp *** m

            "perpendicular vector stays perpendicular" |> Expect.floatClose Accuracy.high (from *** rotatedPerp) 0.0
        }

        test "RigidMatrix createVecToVec small angle rotation numerical stability" {
            // Test numerical stability with very small rotation angle
            let from = UnitVec.Xaxis
            let to_ = UnitVec.create(0.99999999, 0.0001, 0.0)  // ~0.57 degree angle
            let m = RigidMatrix.createVecToVec(from, to_)
            let result = from *** m

            "small angle rotation numerically stable" |> Expect.floatClose Accuracy.medium (Pnt.distance result.AsPnt to_.AsPnt) 0.0
        }

        test "RigidMatrix createVecToVec with different length vectors" {
            let from = Vec(5, 0, 0)
            let to_ = Vec(0, 0, 3)
            let m = RigidMatrix.createVecToVec(from, to_)
            let result = from *** m

            // Should preserve the length of 'from' but rotate direction to 'to'
            "rotation preserves from vector length" |> Expect.floatClose Accuracy.high result.Length 5.0
            // Direction should match 'to'
            let resultDir = result.Unitized
            let toDir = to_.Unitized
            "rotation aligns direction" |> expectEqual resultDir.AsPnt toDir.AsPnt
        }

        test "RigidMatrix createVecToVec is transpose of reverse rotation" {
            let from = UnitVec.create(1, 1, 0)
            let to_ = UnitVec.create(0, 1, 1)
            let m1 = RigidMatrix.createVecToVec(from, to_)
            let m2 = RigidMatrix.createVecToVec(to_, from)

            // m2 should be the inverse of m1
            let composed = m1 *** m2
            "forward then reverse gives identity" |> Expect.isTrue composed.IsIdentity
        }

        test "RigidMatrix createVecToVec maintains handedness" {
            let from = UnitVec.Xaxis
            let to_ = UnitVec.Yaxis
            let m = RigidMatrix.createVecToVec(from, to_)

            // Check that Z-axis stays Z-axis (preserves right-handedness)
            let zTransformed = UnitVec.Zaxis *** m
            "maintains right-handed coordinate system" |> Expect.floatClose Accuracy.high (zTransformed *** UnitVec.Zaxis) 1.0
        }

    ]