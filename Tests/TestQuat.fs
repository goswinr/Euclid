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
        let a = Pnt(1,0,0)
        let b = a *** q
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

  ]

