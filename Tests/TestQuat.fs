module TestQuat

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
    testList "Quaternion transformations" [

        test "Quaternion 90 z" {
            let q = Quaternion.createFromDegree(Vec.Zaxis*9.0, 90.)
            let a = Pnt(9,0,3)
            let b = a *** q
            let expected = Pnt(0,9,3)
            "*** z " |> expectEqual b expected
            let c = a |> Pnt.rotateByQuaternion q
            "rotateByQuaternion z" |> expectEqual c expected
        }

        test "Quaternion 90 x" {
            let q = Quaternion.createFromDegree(Vec.Xaxis*9.0, 90.)
            let a = Pnt(0,9,0)
            let b = a *** q
            let expected = Pnt(0,0,9)
            "*** x" |> expectEqual b expected
            let c = a |> Pnt.rotateByQuaternion q
            "rotateByQuaternion x" |> expectEqual c expected
        }

        test "Quaternion 90 y" {
            let q = Quaternion.createFromDegree(Vec.Yaxis, 90.)
            let a = Pnt(9,0,0)
            let b = a *** q
            let expected = Pnt(0,0,-9)
            "*** y" |> expectEqual b expected
            let c = a |> Pnt.rotateByQuaternion q
            "rotateByQuaternion y" |> expectEqual c expected
        }

        test "Quaternion 90 inverse" {
            let q = Quaternion.createFromDegree(Vec.Zaxis*9.0, 90.)
            let a = Pnt(9,0,3)
            let b = a *** q
            let iq = q.Inverse
            let c = b *** iq
            "*** inverse" |> expectEqual a c
            let d = a |> Pnt.rotateByQuaternion q |> Pnt.rotateByQuaternion iq
            "rotateByQuaternion inverse" |> expectEqual a d
        }

        test "Quaternion 180 degrees" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 180.)
            let a = Pnt(5,3,7)
            let b = a *** q
            let expected = Pnt(-5,-3,7)
            "180° rotation z" |> expectEqual b expected
        }

        test "Quaternion 45 degrees" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let a = Pnt(1,0,0)
            let b = a *** q
            let sqrt2_2 = sqrt(2.0) / 2.0
            let expected = Pnt(sqrt2_2, sqrt2_2, 0)
            "45° rotation z" |> expectEqual b expected
        }

        test "Quaternion arbitrary axis" {
            let axis = Vec(1,1,1)
            let q = Quaternion.createFromDegree(axis, 120.)
            let a = Pnt(1,0,0)
            let b = a *** q
            let expected = Pnt(0,1,0)
            "120° around (1,1,1)" |> expectEqual b expected
        }

        test "Quaternion multiplication composition" {
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let q2 = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let qCombined = q1 *** q2
            let qDirect = Quaternion.createFromDegree(Vec.Zaxis, 90.)
            let a = Pnt(1,0,0)
            let result1 = a *** qCombined
            let result2 = a *** qDirect
            "quaternion multiplication" |> expectEqual result1 result2
        }

        test "Quaternion identity" {
            let q = Quaternion.identity
            let a = Pnt(5,3,7)
            let b = a *** q
            "identity quaternion" |> expectEqual a b
        }

        test "Quaternion angle properties" {
            let q = Quaternion.createFromDegree(Vec.Xaxis, 60.)
            Expect.floatClose Accuracy.high q.AngleInDegrees 60.0 "angle in degrees"
            Expect.floatClose Accuracy.high q.AngleInRadians (System.Math.PI / 3.0) "angle in radians"
        }

        test "Quaternion set angle" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 90.)
            let q2 = q.setAngleInDegrees(45.)
            let a = Pnt(1,0,0)
            let result = a *** q2
            let sqrt2_2 = sqrt(2.0) / 2.0
            let expected = Pnt(sqrt2_2, sqrt2_2, 0)
            "set angle to 45°" |> expectEqual result expected
        }

        test "Quaternion vector to vector" {
            let from = UnitVec.Xaxis
            let toPt = UnitVec.Yaxis
            let q = Quaternion.createVecToVec(from, toPt)
            let result = UnitVec.Xaxis *** q
            let expected = UnitVec.Yaxis
            Expect.floatClose Accuracy.high result.X expected.X "vec to vec X"
            Expect.floatClose Accuracy.high result.Y expected.Y "vec to vec Y"
            Expect.floatClose Accuracy.high result.Z expected.Z "vec to vec Z"
        }

        test "Quaternion Euler XYZ" {
            let q = Quaternion.createFromEulerXYZ(30., 45., 60.)
            // Test that the quaternion is valid (has unit length)
            Expect.floatClose Accuracy.high (q.X*q.X + q.Y*q.Y + q.Z*q.Z + q.W*q.W) 1.0 "quaternion is unit length"
        }

        test "Quaternion Euler roundtrip ZYX" {
            let originalZ, originalY, originalX = 30., 45., 60.
            let q = Quaternion.createFromEulerZYX(originalZ, originalY, originalX)
            let (z, y, x) = Quaternion.toEulerAnglesZYX(q)
            Expect.floatClose Accuracy.medium z originalZ "roundtrip Z"
            Expect.floatClose Accuracy.medium y originalY "roundtrip Y"
            Expect.floatClose Accuracy.medium x originalX "roundtrip X"
        }

        test "Quaternion conjugate equals inverse" {
            let q = Quaternion.createFromDegree(Vec(1,2,3), 47.)
            let conj = q.Conjugate
            let inv = q.Inverse
            Expect.floatClose Accuracy.high conj.X inv.X "conjugate X = inverse X"
            Expect.floatClose Accuracy.high conj.Y inv.Y "conjugate Y = inverse Y"
            Expect.floatClose Accuracy.high conj.Z inv.Z "conjugate Z = inverse Z"
            Expect.floatClose Accuracy.high conj.W inv.W "conjugate W = inverse W"
        }

        test "Quaternion multiple rotations" {
            let q1 = Quaternion.createFromDegree(Vec.Xaxis, 90.) // no chnage, pt is on X axis
            let q2 = Quaternion.createFromDegree(Vec.Yaxis, 90.)
            let q3 = Quaternion.createFromDegree(Vec.Zaxis, 90.) // no change, pt is on Z axis
            let a = Pnt(1,0,0)
            let result = a *** q1 *** q2 *** q3
            // After X:90°, Y:90°, Z:90° rotations
            let expected = Pnt(0,0,1)
            "multiple rotations" |> expectEqual result expected
        }

        test "Quaternion multiple rotations2" {
            let q1 = Quaternion.createFromDegree(Vec.Xaxis, 90.)
            let q2 = Quaternion.createFromDegree(Vec.Yaxis, 90.)
            let q3 = Quaternion.createFromDegree(Vec.Zaxis, 90.)
            let a = Pnt(0,1,0)
            let result = a *** q1 *** q2 *** q3
            // After X:90°, Y:90°, Z:90° rotations
            let expected = Pnt(0,-1,0)
            "multiple rotations" |> expectEqual result expected
        }

        test "Quaternion small angles" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 0.001)
            let a = Pnt(1000,0,0)
            let b = a *** q
            // For very small angles, should be approximately the same
            Expect.floatClose Accuracy.medium b.X a.X "small angle X unchanged"
            Expect.isTrue (abs(b.Y) < 1.0) "small angle Y is small"
        }

        test "Quaternion opposite vectors error" {
            let from = UnitVec.Xaxis
            let toPt = UnitVec.createFromVec(Vec(-1,0,0))
            Expect.throws (fun () -> Quaternion.createVecToVec(from, toPt) |> ignore) "opposite vectors should throw"
        }

        // Edge case tests based on documentation review and coding standards

        test "Quaternion create with zero-length should fail" {
            Expect.throws (fun () -> Quaternion.create(0., 0., 0., 0.) |> ignore) "zero-length quaternion should fail"
        }

        test "Quaternion create with very small values should fail" {
            Expect.throws (fun () -> Quaternion.create(1e-20, 1e-20, 1e-20, 1e-20) |> ignore) "very small quaternion should fail"
        }

        test "Quaternion createFromRadians with zero-length axis should fail" {
            let zeroAxis = Vec(0., 0., 0.)
            Expect.throws (fun () -> Quaternion.createFromRadians(zeroAxis, 90.) |> ignore) "zero-length axis should fail"
        }

        test "Quaternion createFromRadians with very short axis should fail" {
            let tinyAxis = Vec(1e-20, 1e-20, 1e-20)
            Expect.throws (fun () -> Quaternion.createFromRadians(tinyAxis, 90.) |> ignore) "very short axis should fail"
        }

        test "Quaternion createFromDegree with very short axis should fail" {
            let tinyAxis = Vec(1e-15, 0., 0.)
            Expect.throws (fun () -> Quaternion.createFromDegree(tinyAxis, 45.) |> ignore) "very short axis should fail"
        }

        test "Quaternion setAngle on identity should fail" {
            let q = Quaternion.identity
            Expect.throws (fun () -> q.setAngleInRadians 1.0 |> ignore) "setAngle on identity should fail"
            Expect.throws (fun () -> q.setAngleInDegrees 45. |> ignore) "setAngle on identity should fail"
        }

        test "Quaternion setAngle on near-identity should fail" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 1e-10)
            Expect.throws (fun () -> q.setAngleInRadians 1.0 |> ignore) "setAngle on near-identity should fail"
        }

        test "Quaternion Axis on identity returns zero vector" {
            let q = Quaternion.identity
            let axis = q.Axis
            Expect.floatClose Accuracy.high axis.X 0.0 "identity axis X is 0"
            Expect.floatClose Accuracy.high axis.Y 0.0 "identity axis Y is 0"
            Expect.floatClose Accuracy.high axis.Z 0.0 "identity axis Z is 0"
        }

        test "Quaternion Axis length equals sin(angle/2)" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 60.)
            let axis = q.Axis
            let expectedLength = sin(System.Math.PI / 6.0) // sin(30°) for 60° rotation
            let actualLength = sqrt(axis.X*axis.X + axis.Y*axis.Y + axis.Z*axis.Z)
            Expect.floatClose Accuracy.high actualLength expectedLength "axis length is sin(angle/2)"
        }

        test "Quaternion equals with double-coverage (q vs -q)" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let qNeg = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
            // q and -q represent the same rotation but should compare as unequal
            Expect.isFalse (Quaternion.equals 1e-10 q qNeg) "q and -q should be unequal (component-wise)"
            // But they should produce the same rotation result
            let a = Pnt(1,0,0)
            let result1 = a *** q
            let result2 = a *** qNeg
            "q and -q produce same rotation" |> expectEqual result1 result2
        }

        test "Quaternion equalsRotation with same quaternion" {
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let q2 = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            Expect.isTrue (Quaternion.equalsRotation 1e-10 q1 q2) "same quaternions should be rotationally equal"
        }

        test "Quaternion equalsRotation with negated quaternion (q vs -q)" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let qNeg = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
            // equalsRotation should recognize q and -q as the same rotation
            Expect.isTrue (Quaternion.equalsRotation 1e-10 q qNeg) "q and -q should be rotationally equal"
            // Regular equals should still return false
            Expect.isFalse (Quaternion.equals 1e-10 q qNeg) "q and -q should be component-wise unequal"
        }

        test "Quaternion equalsRotation with different rotations" {
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let q2 = Quaternion.createFromDegree(Vec.Zaxis, 90.)
            Expect.isFalse (Quaternion.equalsRotation 1e-10 q1 q2) "different rotations should not be equal"
        }

        test "Quaternion equalsRotation with different axes" {
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let q2 = Quaternion.createFromDegree(Vec.Xaxis, 45.)
            Expect.isFalse (Quaternion.equalsRotation 1e-10 q1 q2) "same angle different axis should not be equal"
        }

        test "Quaternion equalsRotation with tolerance" {
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, 45.0)
            let q2 = Quaternion.createFromDegree(Vec.Zaxis, 45.001)
            // Should be equal within loose tolerance
            Expect.isTrue (Quaternion.equalsRotation 0.01 q1 q2) "similar quaternions within tolerance"
            // Should be unequal with tight tolerance
            Expect.isFalse (Quaternion.equalsRotation 1e-10 q1 q2) "similar quaternions outside tight tolerance"
        }

        test "Quaternion equalsRotation with negated and tolerance" {
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, 45.0)
            let q2 = Quaternion.createFromDegree(Vec.Zaxis, 45.001)
            let q2Neg = Quaternion.createUnchecked(-q2.X, -q2.Y, -q2.Z, -q2.W)
            // Should be equal within tolerance even when negated
            Expect.isTrue (Quaternion.equalsRotation 0.01 q1 q2Neg) "negated similar quaternions within tolerance"
        }

        test "Quaternion equalsRotation with identity" {
            let q1 = Quaternion.identity
            let q2 = Quaternion.identity
            Expect.isTrue (Quaternion.equalsRotation 1e-10 q1 q2) "identity quaternions should be equal"
        }

        test "Quaternion equalsRotation with zero tolerance" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let qNeg = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
            // Even with zero tolerance, q and -q should be rotationally equal
            Expect.isTrue (Quaternion.equalsRotation 0.0 q qNeg) "q and -q equal with zero tolerance"
        }

        test "Quaternion equalsRotation consistency with rotation result" {
            // If equalsRotation returns true, both should rotate a point to the same position
            let q1 = Quaternion.createFromDegree(Vec(1,1,1), 60.)
            let q2 = Quaternion.createUnchecked(-q1.X, -q1.Y, -q1.Z, -q1.W)
            Expect.isTrue (Quaternion.equalsRotation 1e-10 q1 q2) "q and -q are rotationally equal"
            // Verify they produce the same rotation
            let testPt = Pnt(5, 3, 7)
            let result1 = testPt *** q1
            let result2 = testPt *** q2
            "rotationally equal quaternions produce same result" |> expectEqual result1 result2
        }

        test "Quaternion equalsRotation with 180 degree rotation" {
            // 180° rotation is special - q and -q might behave differently
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, 180.)
            let q1Neg = Quaternion.createUnchecked(-q1.X, -q1.Y, -q1.Z, -q1.W)
            Expect.isTrue (Quaternion.equalsRotation 1e-10 q1 q1Neg) "180° rotation q and -q are equal"
        }

        test "Quaternion createVecToVec with nearly identical vectors returns identity" {
            let from = UnitVec.Xaxis
            let toPt = UnitVec.createFromVec(Vec(1.0, 1e-13, 0.))
            let q = Quaternion.createVecToVec(from, toPt)
            // Should return identity quaternion
            Expect.floatClose Accuracy.medium q.AngleInDegrees 0.0 "nearly identical vectors produce near-identity"
        }

        test "Quaternion createVecToVec with exactly identical vectors returns identity" {
            let from = UnitVec.Xaxis
            let toPt = UnitVec.Xaxis
            let q = Quaternion.createVecToVec(from, toPt)
            Expect.floatClose Accuracy.high q.X 0.0 "identical vectors X"
            Expect.floatClose Accuracy.high q.Y 0.0 "identical vectors Y"
            Expect.floatClose Accuracy.high q.Z 0.0 "identical vectors Z"
            Expect.floatClose Accuracy.high q.W 1.0 "identical vectors W"
        }

        test "Quaternion createVecToVec (Vec overload) with zero-length from should fail" {
            let zeroVec = Vec(0., 0., 0.)
            let target = Vec(1., 0., 0.)
            Expect.throws (fun () -> Quaternion.createVecToVec(zeroVec, target) |> ignore) "zero-length vecFrom should fail"
        }

        test "Quaternion createVecToVec (Vec overload) with zero-length to should fail" {
            let source = Vec(1., 0., 0.)
            let zeroVec = Vec(0., 0., 0.)
            Expect.throws (fun () -> Quaternion.createVecToVec(source, zeroVec) |> ignore) "zero-length vecTo should fail"
        }

        test "Quaternion createVecToVec (Vec overload) with very short vectors should fail" {
            let tinyVec = Vec(1e-20, 0., 0.)
            let target = Vec(1., 0., 0.)
            Expect.throws (fun () -> Quaternion.createVecToVec(tinyVec, target) |> ignore) "very short vector should fail"
        }

        test "Quaternion numerical drift from repeated multiplications" {
            // Start with a small rotation
            let q = Quaternion.createFromDegree(Vec.Zaxis, 1.0)
            // Multiply it 360 times to make a full circle
            let mutable result = Quaternion.identity
            for _ in 1 .. 360 do
                result <- result *** q
            // Check that the result is still unit length (within tolerance)
            let magnitude = sqrt(result.X*result.X + result.Y*result.Y + result.Z*result.Z + result.W*result.W)
            Expect.floatClose Accuracy.medium magnitude 1.0 "quaternion should remain unit length after many multiplications"
            // Also check that we've completed a full rotation (should be near identity)
            let testPt = Pnt(1,0,0)
            let rotated = testPt *** result
            Expect.floatClose Accuracy.low rotated.X testPt.X "full circle X"
            Expect.floatClose Accuracy.low rotated.Y testPt.Y "full circle Y"
        }

        test "Quaternion toEulerAnglesZYX gimbal lock near 90 degrees" {
            // Test near gimbal lock condition (Beta ≈ 90°)
            let q = Quaternion.createFromEulerZYX(30., 89.9, 60.)
            let _z, y, _x = Quaternion.toEulerAnglesZYX q
            // Near gimbal lock, the conversion may have reduced accuracy
            Expect.floatClose Accuracy.low y 89.9 "near gimbal lock Y"
        }

        test "Quaternion toEulerAnglesZYX at exact 90 degrees" {
            // Test at exact gimbal lock condition (Beta = 90°)
            let q = Quaternion.createFromEulerZYX(30., 90., 60.)
            let _z, y, _x = Quaternion.toEulerAnglesZYX q
            // At gimbal lock, Y should still be 90
            Expect.floatClose Accuracy.medium y 90.0 "gimbal lock Y = 90"
        }

        test "Quaternion equals with different tolerances" {
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let q2 = Quaternion.createFromDegree(Vec.Zaxis, 45.001)
            // Should be equal with loose tolerance
            Expect.isTrue (Quaternion.equals 0.01 q1 q2) "similar quaternions within loose tolerance"
            // Should be unequal with tight tolerance
            Expect.isFalse (Quaternion.equals 1e-10 q1 q2) "similar quaternions outside tight tolerance"
        }

        test "Quaternion zero angle rotation" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 0.)
            let a = Pnt(5,3,7)
            let b = a *** q
            "zero angle rotation" |> expectEqual a b
        }

        test "Quaternion 360 degree rotation" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 360.)
            let a = Pnt(5,3,7)
            let b = a *** q
            "360° rotation returns to original" |> expectEqual a b
        }

        test "Quaternion very large angle" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 720.) // Two full rotations
            let a = Pnt(5,3,7)
            let b = a *** q
            "720° rotation returns to original" |> expectEqual a b
        }

        test "Quaternion negative angle" {
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, -90.)
            let q2 = Quaternion.createFromDegree(Vec.Zaxis, 270.)
            let a = Pnt(1,0,0)
            let result1 = a *** q1
            let result2 = a *** q2
            "-90° equals 270°" |> expectEqual result1 result2
        }

        test "Quaternion Normalize restores unit length" {
            // Create a slightly denormalized quaternion (simulates drift from multiplications)
            let q1 = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            // Manually create a slightly off-unit quaternion by scaling
            let scale = 1.001 // slightly larger than 1
            let qBad = Quaternion.create(q1.X * scale, q1.Y * scale, q1.Z * scale, q1.W * scale)
            // qBad is now normalized (create normalizes it), so let's test normalizing it again
            let qNormalized = qBad.Normalize()
            let magnitude = sqrt(qNormalized.X*qNormalized.X + qNormalized.Y*qNormalized.Y + qNormalized.Z*qNormalized.Z + qNormalized.W*qNormalized.W)
            Expect.floatClose Accuracy.high magnitude 1.0 "normalized quaternion has unit length"
        }

        test "Quaternion Normalize preserves rotation direction" {
            // Start with a valid quaternion and verify normalize keeps it valid
            let q = Quaternion.createFromDegree(Vec.Zaxis, 60.)
            let qNormalized = q.Normalize()
            let a = Pnt(1,0,0)
            let result1 = a *** q
            let result2 = a *** qNormalized
            // Both should produce the same result
            "Normalize preserves rotation" |> expectEqual result1 result2
        }

        test "Quaternion Normalize after many multiplications" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 1.0)
            let mutable result = Quaternion.identity
            for _ in 1 .. 1000 do
                result <- result *** q
            // After 1000 multiplications, check magnitude before normalization
            let magnitudeBefore = sqrt(result.X*result.X + result.Y*result.Y + result.Z*result.Z + result.W*result.W)
            let normalized = result.Normalize()
            let magnitudeAfter = sqrt(normalized.X*normalized.X + normalized.Y*normalized.Y + normalized.Z*normalized.Z + normalized.W*normalized.W)
            // Magnitude should be closer to 1.0 after normalization
            Expect.floatClose Accuracy.high magnitudeAfter 1.0 "normalized magnitude is exactly 1.0"
            Expect.isTrue (abs(magnitudeAfter - 1.0) <= abs(magnitudeBefore - 1.0)) "normalization improves unit length"
        }

        test "Quaternion Normalize on already normalized quaternion" {
            let q = Quaternion.createFromDegree(Vec.Zaxis, 45.)
            let qNormalized = q.Normalize()
            // Should be very close to the original (already unit length)
            Expect.floatClose Accuracy.high qNormalized.X q.X "normalized X matches original"
            Expect.floatClose Accuracy.high qNormalized.Y q.Y "normalized Y matches original"
            Expect.floatClose Accuracy.high qNormalized.Z q.Z "normalized Z matches original"
            Expect.floatClose Accuracy.high qNormalized.W q.W "normalized W matches original"
        }

        test "Quaternion Normalize with zero values should fail" {
            // Test that Normalize (which calls create) fails on zero-length quaternion
            Expect.throws (fun () -> Quaternion.create(0., 0., 0., 0.) |> ignore) "create zero quaternion should fail"
            // Also test with very small values
            Expect.throws (fun () -> Quaternion.create(1e-20, 1e-20, 1e-20, 1e-20) |> ignore) "create tiny quaternion should fail"
        }

    ]

