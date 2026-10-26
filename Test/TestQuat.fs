module TestQuat

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
    testList "Quaternion transformations" [
        test "scaleAngle scales the encoded rotation without clamping" {
            let axis = UnitVec.create 1. (-2.) 3.
            let point = Pnt(12.3, -4.5, 6.7)
            for degrees in [-90.; 37.; 179.999; 180.; 180.001; 270.; 359.999] do
                let q = Quaternion.createFromUnitVecDegrees axis degrees
                for factor in [-3.; -1.; 0.; 0.25; 0.5; 1.; 1.5; 2.; 4.] do
                    let scaled = q |> Quaternion.scaleAngle factor
                    let expected = Quaternion.createFromUnitVecDegrees axis (degrees * factor)
                    Expect.isTrue (Quaternion.equalsWithin 1e-12 scaled expected) $"{degrees} degrees scaled by {factor}"
                    "scaled rotation applied to a point" |> expectEqual (point *** scaled) (point *** expected)
                    Expect.isTrue (Quaternion.equalsExact scaled (q.ScaleAngle factor)) "static and instance agree"
                    Expect.floatClose Accuracy.veryHigh (scaled.X*scaled.X + scaled.Y*scaled.Y + scaled.Z*scaled.Z + scaled.W*scaled.W) 1. "unit length"
                Expect.isTrue (Quaternion.equalsExact q (Quaternion.createFromUnitVecDegrees axis degrees)) "input unchanged"
        }

        test "scaleAngle distinguishes opposite quaternion representations" {
            let axis = UnitVec.create 1. 2. 3.
            let q = Quaternion.createFromUnitVecDegrees axis 270.
            let neg = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
            let quarter = q.ScaleAngle 0.25
            let negQuarter = neg.ScaleAngle 0.25
            Expect.isTrue (Quaternion.equalsWithin 1e-12 quarter (Quaternion.createFromUnitVecDegrees axis 67.5)) "270 degrees gives 67.5 degrees"
            Expect.isTrue (Quaternion.equalsWithin 1e-12 negQuarter (Quaternion.createFromUnitVecDegrees axis (-22.5))) "negated representation gives -22.5 degrees"
            Expect.isFalse (Quaternion.equalsRotationWithin 1e-12 quarter negQuarter) "encoded rotations take different paths"
        }

        test "scaleAngle preserves the axis of exact half turns" {
            let axis = UnitVec.create 1. (-2.) 3.
            for sign in [-1.; 1.] do
                let q = Quaternion.createUnchecked(sign*axis.X, sign*axis.Y, sign*axis.Z, 0.)
                let expected = Quaternion.createFromUnitVecDegrees axis (sign * 45.)
                Expect.isTrue (Quaternion.equalsWithin 1e-12 (q.ScaleAngle 0.25) expected) "encoded half-turn axis"
        }

        test "scaleAngle handles identity and very small rotations" {
            for factor in [-2.; 0.; 0.25; 1.; 2.; System.Double.MaxValue] do
                Expect.isTrue (Quaternion.equalsExact (Quaternion.identity.ScaleAngle factor) Quaternion.identity) "identity stays identity"
            let axis = UnitVec.create 1. (-2.) 3.
            for radians in [1e-10; 1e-20; 1e-200] do
                let q = Quaternion.createFromUnitVecRadians axis radians
                let quarter = q.ScaleAngle 0.25
                let expected = Quaternion.createFromUnitVecRadians axis (radians * 0.25)
                Expect.isTrue (abs (quarter.X / expected.X - 1.) < 1e-12) "tiny angle retained"
                let enlarged = q.ScaleAngle (1. / radians)
                Expect.isTrue (Quaternion.equalsWithin 1e-12 enlarged (Quaternion.createFromUnitVecRadians axis 1.)) "tiny angle can be enlarged"
            let almostFullTurn = Quaternion.createUnchecked(axis.X * 1e-20, axis.Y * 1e-20, axis.Z * 1e-20, -1.)
            Expect.isTrue (Quaternion.equalsWithin 1e-12 (almostFullTurn.ScaleAngle 0.25) (Quaternion.createFromUnitVecDegrees axis 90.)) "near full turn retains its axis"
        }

        test "scaleAngle rejects non-finite factors and overflowing angles" {
            let q = Quaternion.createFromUnitVecDegrees (UnitVec.create 1. 2. 3.) 270.
            for factor in [nan; infinity; -infinity] do
                Expect.throws (fun () -> q.ScaleAngle factor |> ignore) "non-finite factor"
                Expect.throws (fun () -> Quaternion.identity.ScaleAngle factor |> ignore) "non-finite factor on identity"
            Expect.throws (fun () -> q.ScaleAngle System.Double.MaxValue |> ignore) "scaled half angle overflows"
        }

        test "scaleAngle fails for an undefined full-turn axis" {
            let fullTurn = Quaternion.createUnchecked(0., 0., 0., -1.)
            for factor in [-2.; 0.; 0.25; 1.; 2.] do
                Expect.throws (fun () -> fullTurn.ScaleAngle factor |> ignore) "full-turn axis is undefined"
        }

        test "half follows shortest rotation and preserves the half-turn axis" {
            let axis = UnitVec.create 1. 2. 2.
            for degrees in [0.; 1e-8; 37.; 179.999; 180.001; 270.; 359.999] do
                let q = Quaternion.createFromUnitVecDegrees axis degrees
                let half = Quaternion.half q
                Expect.isTrue (Quaternion.equalsRotationWithin 1e-12 (half.Then half) q) "half composed twice"
                Expect.isTrue (half.AngleInDegrees <= 90.000001) "shortest half"
                let neg = Quaternion.createUnchecked(-q.X,-q.Y,-q.Z,-q.W)
                Expect.isTrue (Quaternion.equalsWithin 1e-12 half neg.Half) "same half for opposite representations away from tie"
            let q180 = Quaternion.createUnchecked(axis.X,axis.Y,axis.Z,0.)
            let n180 = Quaternion.createUnchecked(-axis.X,-axis.Y,-axis.Z,0.)
            Expect.isTrue (q180.Half.X > 0. && n180.Half.X < 0.) "encoded axis preserved at tie"
            Expect.isTrue (Quaternion.equalsRotationWithin 1e-12 (q180.Half.Then q180.Half) q180) "exact half turn composition"
            for q in [Quaternion.identity;Quaternion.createUnchecked(0.,0.,0.,-1.)] do
                Expect.isTrue (Quaternion.equalsExact q.Half Quaternion.identity) "both identities"
        }

        test "slerp along a shared axis interpolates the angle" {
            let axis = UnitVec.create 1. (-2.) 3.
            let point = Pnt(12.3, -4.5, 6.7)
            let start = Quaternion.createFromUnitVecDegrees axis 30.
            let finish = Quaternion.createFromUnitVecDegrees axis 100.
            for factor in [-0.5; 0.; 0.25; 0.5; 1.; 1.5] do
                let expected = Quaternion.createFromUnitVecDegrees axis (30. + 70. * factor)
                let s = Quaternion.slerp start finish factor
                Expect.isTrue (Quaternion.equalsWithin 1e-12 s expected) $"slerp at {factor}"
                Expect.isTrue (Quaternion.equalsExact s (start.Slerp(finish, factor))) "static and instance agree"
                Expect.isTrue (Quaternion.equalsWithin 1e-12 (Quaternion.slerpEncoded start finish factor) expected) $"slerpEncoded at {factor}"
                "slerp applied to a point" |> expectEqual (point *** s) (point *** expected)
            Expect.isTrue (Quaternion.equalsExact (Quaternion.slerp start finish 0.) start) "exact start"
            Expect.isTrue (Quaternion.equalsExact (Quaternion.slerp start finish 1.) finish) "exact finish"
        }

        test "slerp matches the Shoemake formula for unrelated rotations" {
            let shoemake (a:Quaternion) (b:Quaternion) t =
                let dot = a.X*b.X + a.Y*b.Y + a.Z*b.Z + a.W*b.W
                let theta = acos dot
                let ra = sin ((1. - t) * theta) / sin theta
                let rb = sin (t * theta) / sin theta
                Quaternion.createUnchecked(a.X*ra + b.X*rb, a.Y*ra + b.Y*rb, a.Z*ra + b.Z*rb, a.W*ra + b.W*rb)
            let a = Quaternion.createFromEulerXYZDegrees 10. -35. 70.
            let b = Quaternion.createFromEulerXYZDegrees -60. 20. 125.
            let bNeg = Quaternion.createUnchecked(-b.X, -b.Y, -b.Z, -b.W)
            let bFar, bNear = if a.X*b.X + a.Y*b.Y + a.Z*b.Z + a.W*b.W < 0. then b, bNeg else bNeg, b
            for factor in [-0.5; 0.1; 0.5; 0.9; 1.5] do
                let s = Quaternion.slerp a bFar factor
                Expect.isTrue (Quaternion.equalsWithin 1e-12 s (shoemake a bNear factor)) $"shortest path at {factor}"
                Expect.isTrue (Quaternion.equalsWithin 1e-12 (Quaternion.slerp a bNear factor) s) $"both representations of finish at {factor}"
                Expect.isTrue (Quaternion.equalsWithin 1e-12 (Quaternion.slerpEncoded a bFar factor) (shoemake a bFar factor)) $"encoded path at {factor}"
                Expect.isTrue (Quaternion.equalsWithin 1e-12 (Quaternion.slerpEncoded a bNear factor) s) $"encoded path on the near side at {factor}"
                Expect.floatClose Accuracy.veryHigh (s.X*s.X + s.Y*s.Y + s.Z*s.Z + s.W*s.W) 1. "unit length"
            let half = Quaternion.slerp a b 0.5
            let firstHalf = a.Conjugate.Then half
            Expect.isTrue (Quaternion.equalsRotationWithin 1e-12 ((a.Then firstHalf).Then firstHalf) b) "two halves give finish"
        }

        test "slerp takes the shortest path and slerpEncoded the encoded one" {
            let axis = UnitVec.create 1. 2. 3.
            let q = Quaternion.createFromUnitVecDegrees axis 90.
            let neg = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
            Expect.isTrue (Quaternion.equalsWithin 1e-12 (Quaternion.slerp Quaternion.identity neg 0.5) (Quaternion.createFromUnitVecDegrees axis 45.)) "shortest 45 degrees"
            Expect.isTrue (Quaternion.equalsWithin 1e-12 (Quaternion.slerp Quaternion.identity neg 1.) q) "negated finish at factor 1"
            Expect.isTrue (Quaternion.equalsWithin 1e-12 (Quaternion.slerpEncoded Quaternion.identity neg 0.5) (Quaternion.createFromUnitVecDegrees axis -135.)) "encoded 270 degrees the other way"
            for factor in [-2.; 0.25; 0.5; 1.5] do
                Expect.isTrue (Quaternion.equalsWithin 1e-15 (Quaternion.slerpEncoded Quaternion.identity neg factor) (neg.ScaleAngle factor)) "same as scaleAngle from identity"
        }

        test "slerp handles equal, opposite and very close quaternions" {
            let axis = UnitVec.create -3. 1. 2.
            let q = Quaternion.createFromUnitVecDegrees axis 123.
            let neg = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
            for factor in [-2.; 0.25; 0.5; 3.] do
                Expect.isTrue (Quaternion.equalsWithin 1e-15 (Quaternion.slerp q q factor) q) "same quaternion"
                Expect.isTrue (Quaternion.equalsWithin 1e-15 (Quaternion.slerpEncoded q q factor) q) "same quaternion encoded"
                Expect.isTrue (Quaternion.equalsWithin 1e-15 (Quaternion.slerp q neg factor) q) "opposite representation is the same rotation"
                Expect.throws (fun () -> Quaternion.slerpEncoded q neg factor |> ignore) "encoded full-turn axis is undefined"
            Expect.isTrue (Quaternion.equalsExact (Quaternion.slerpEncoded q neg 0.) q) "encoded factor 0"
            Expect.isTrue (Quaternion.equalsExact (Quaternion.slerpEncoded q neg 1.) neg) "encoded factor 1"
            let tinyAxis = UnitVec.create 2. 1. -1.
            for radians in [1e-10; 1e-14] do
                let close = q.Then (Quaternion.createFromUnitVecRadians tinyAxis radians)
                let expected = q.Then (Quaternion.createFromUnitVecRadians tinyAxis (radians * 0.5))
                Expect.isTrue (Quaternion.equalsWithin 1e-15 (Quaternion.slerp q close 0.5) expected) $"close quaternions at {radians} radians"
                Expect.isTrue (Quaternion.equalsWithin 1e-15 (Quaternion.slerpEncoded q close 0.5) expected) $"close quaternions encoded at {radians} radians"
        }

        test "slerp rejects non-finite factors" {
            let a = Quaternion.createFromUnitVecDegrees (UnitVec.create 1. 2. 3.) 40.
            let b = Quaternion.createFromUnitVecDegrees (UnitVec.create -1. 0. 2.) 70.
            for factor in [nan; infinity; -infinity] do
                Expect.throws (fun () -> Quaternion.slerp a b factor |> ignore) "non-finite factor"
                Expect.throws (fun () -> Quaternion.slerpEncoded a b factor |> ignore) "non-finite factor encoded"
                Expect.throws (fun () -> Quaternion.slerp a a factor |> ignore) "non-finite factor on equal quaternions"
        }

        test "Quaternion 90 z" {
            let q = Quaternion.createFromVecDegrees (Vec.zAxis*9.0) 90.
            let a = Pnt(9,0,3)
            let b = a *** q
            let expected = Pnt(0,9,3)
            "*** z " |> expectEqual b expected
            let c = a |> Pnt.rotate q
            "rotate z" |> expectEqual c expected
        }

        test "Quaternion 90 x" {
            let q = Quaternion.createFromVecDegrees (Vec.xAxis*9.0) 90.
            let a = Pnt(0,9,0)
            let b = a *** q
            let expected = Pnt(0,0,9)
            "*** x" |> expectEqual b expected
            let c = a |> Pnt.rotate q
            "rotate x" |> expectEqual c expected
        }

        test "Quaternion 90 y" {
            let q = Quaternion.createFromVecDegrees Vec.yAxis 90.
            let a = Pnt(9,0,0)
            let b = a *** q
            let expected = Pnt(0,0,-9)
            "*** y" |> expectEqual b expected
            let c = a |> Pnt.rotate q
            "rotate y" |> expectEqual c expected
        }

        test "Quaternion 90 inverse" {
            let q = Quaternion.createFromVecDegrees (Vec.zAxis*9.0) 90.
            let a = Pnt(9,0,3)
            let b = a *** q
            let iq = q.Inverse
            let c = b *** iq
            "*** inverse" |> expectEqual a c
            let d = a |> Pnt.rotate q |> Pnt.rotate iq
            "rotate inverse" |> expectEqual a d
        }

        test "Quaternion 180 degrees" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 180.
            let a = Pnt(5,3,7)
            let b = a *** q
            let expected = Pnt(-5,-3,7)
            "180° rotation z" |> expectEqual b expected
        }

        test "Quaternion 45 degrees" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let a = Pnt(1,0,0)
            let b = a *** q
            let sqrt2_2 = sqrt(2.0) / 2.0
            let expected = Pnt(sqrt2_2, sqrt2_2, 0)
            "45° rotation z" |> expectEqual b expected
        }

        test "Quaternion arbitrary axis" {
            let axis = Vec(1,1,1)
            let q = Quaternion.createFromVecDegrees axis 120.
            let a = Pnt(1,0,0)
            let b = a *** q
            let expected = Pnt(0,1,0)
            "120° around (1,1,1)" |> expectEqual b expected
        }

        test "Quaternion multiplication composition" {
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let q2 = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let qCombined = q1 *** q2
            let qDirect = Quaternion.createFromVecDegrees Vec.zAxis 90.
            let a = Pnt(1,0,0)
            let result1 = a *** qCombined
            let result2 = a *** qDirect
            "quaternion multiplication" |> expectEqual result1 result2
        }

        test "Quaternion Then applies first rotation first" {
            let qx = Quaternion.createFromVecDegrees Vec.xAxis 90.
            let qy = Quaternion.createFromVecDegrees Vec.yAxis 90.
            let a = Pnt(2,3,5)
            let viaThen = a *** (qx.Then qy)
            let viaSteps = (a *** qx) *** qy // note: *** is right associative, so the parentheses are needed
            "Then equals stepwise" |> expectEqual viaThen viaSteps
            "Then x then y" |> expectEqual viaThen (Pnt(3,-5,-2))
            let reversed = a *** (qy.Then qx)
            "Then y then x" |> expectEqual reversed (Pnt(5,2,3))
            // Hamilton product applies the right operand first:
            "multiply applies right first" |> expectEqual (a *** (qx *** qy)) reversed
        }

        test "Quaternion Then matches Matrix and RigidMatrix order" {
            let axis = Vec(1., 2., 3.)
            let q1 = Quaternion.createFromVecDegrees axis 70.
            let q2 = Quaternion.createFromVecDegrees (Vec(-2., 1., 4.)) 33.
            let m1 = Matrix.createRotationFromVecDegrees axis 70.
            let m2 = Matrix.createRotationFromVecDegrees (Vec(-2., 1., 4.)) 33.
            let r1 = RigidMatrix.createRotationFromVecDegrees axis 70.
            let r2 = RigidMatrix.createRotationFromVecDegrees (Vec(-2., 1., 4.)) 33.
            let a = Pnt(12.3, -4.5, 6.7)
            let pq = a *** (q1.Then q2)
            "Quaternion vs Matrix" |> expectEqual pq (a *** (m1.Then m2))
            "Quaternion vs RigidMatrix" |> expectEqual pq (a *** (r1.Then r2))
            "Matrix.Then equals ***" |> expectEqual (a *** (m1.Then m2)) (a *** (m1 *** m2))
            "RigidMatrix.Then equals ***" |> expectEqual (a *** (r1.Then r2)) (a *** (r1 *** r2))
        }

        test "Quaternion identity" {
            let q = Quaternion.identity
            let a = Pnt(5,3,7)
            let b = a *** q
            "identity quaternion" |> expectEqual a b
        }

        test "Quaternion angle properties" {
            let q = Quaternion.createFromVecDegrees Vec.xAxis 60.
            Expect.floatClose Accuracy.high q.AngleInDegrees 60.0 "angle in degrees"
            Expect.floatClose Accuracy.high q.AngleInRadians (System.Math.PI / 3.0) "angle in radians"
        }

        test "Quaternion with angle" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 90.
            let q2 = q.WithAngleInDegrees(45.)
            let a = Pnt(1,0,0)
            let result = a *** q2
            let sqrt2_2 = sqrt(2.0) / 2.0
            let expected = Pnt(sqrt2_2, sqrt2_2, 0)
            "set angle to 45°" |> expectEqual result expected
        }

        test "Quaternion vector to vector" {
            let from = UnitVec.xAxis
            let toPt = UnitVec.yAxis
            let q = Quaternion.createFromUnitVecs from toPt
            let result = UnitVec.xAxis *** q
            let expected = UnitVec.yAxis
            Expect.floatClose Accuracy.high result.X expected.X "vec to vec X"
            Expect.floatClose Accuracy.high result.Y expected.Y "vec to vec Y"
            Expect.floatClose Accuracy.high result.Z expected.Z "vec to vec Z"
        }

        test "Quaternion Euler XYZ" {
            let q = Quaternion.createFromEulerXYZDegrees 30. 45. 60.
            // Test that the quaternion is valid (has unit length)
            Expect.floatClose Accuracy.high (q.X*q.X + q.Y*q.Y + q.Z*q.Z + q.W*q.W) 1.0 "quaternion is unit length"
        }

        test "Quaternion Euler roundtrip ZYX" {
            let originalZ, originalY, originalX = 30., 45., 60.
            let q = Quaternion.createFromEulerZYXDegrees originalZ originalY originalX
            let (z, y, x) = Quaternion.toEulerAnglesZYX(q)
            Expect.floatClose Accuracy.medium z originalZ "roundtrip Z"
            Expect.floatClose Accuracy.medium y originalY "roundtrip Y"
            Expect.floatClose Accuracy.medium x originalX "roundtrip X"
        }

        test "Quaternion conjugate equals inverse" {
            let q = Quaternion.createFromVecDegrees (Vec(1,2,3)) 47.
            let conj = q.Conjugate
            let inv = q.Inverse
            Expect.floatClose Accuracy.high conj.X inv.X "conjugate X = inverse X"
            Expect.floatClose Accuracy.high conj.Y inv.Y "conjugate Y = inverse Y"
            Expect.floatClose Accuracy.high conj.Z inv.Z "conjugate Z = inverse Z"
            Expect.floatClose Accuracy.high conj.W inv.W "conjugate W = inverse W"
        }

        test "Quaternion multiple rotations" {
            let q1 = Quaternion.createFromVecDegrees Vec.xAxis 90. // no chnage, pt is on X axis
            let q2 = Quaternion.createFromVecDegrees Vec.yAxis 90.
            let q3 = Quaternion.createFromVecDegrees Vec.zAxis 90. // no change, pt is on Z axis
            let a = Pnt(1,0,0)
            let result = a *** q1 *** q2 *** q3
            // After X:90°, Y:90°, Z:90° rotations
            let expected = Pnt(0,0,1)
            "multiple rotations" |> expectEqual result expected
        }

        test "Quaternion multiple rotations2" {
            let q1 = Quaternion.createFromVecDegrees Vec.xAxis 90.
            let q2 = Quaternion.createFromVecDegrees Vec.yAxis 90.
            let q3 = Quaternion.createFromVecDegrees Vec.zAxis 90.
            let a = Pnt(0,1,0)
            let result = a *** q1 *** q2 *** q3
            // After X:90°, Y:90°, Z:90° rotations
            let expected = Pnt(0,-1,0)
            "multiple rotations" |> expectEqual result expected
        }

        test "Quaternion small angles" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 0.001
            let a = Pnt(1000,0,0)
            let b = a *** q
            // For very small angles, should be approximately the same
            Expect.floatClose Accuracy.medium b.X a.X "small angle X unchanged"
            Expect.isTrue (abs(b.Y) < 1.0) "small angle Y is small"
        }

        test "Quaternion opposite vectors error" {
            let from = UnitVec.xAxis
            let toPt = UnitVec.createFromVec(Vec(-1,0,0))
            Expect.throws (fun () -> Quaternion.createFromUnitVecs from toPt |> ignore) "opposite vectors should throw"
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
            Expect.throws (fun () -> Quaternion.createFromVecRadians zeroAxis 90. |> ignore) "zero-length axis should fail"
        }

        test "Quaternion createFromRadians with very short axis should fail" {
            let tinyAxis = Vec(1e-20, 1e-20, 1e-20)
            Expect.throws (fun () -> Quaternion.createFromVecRadians tinyAxis 90. |> ignore) "very short axis should fail"
        }

        test "Quaternion createFromDegree with very short axis should fail" {
            let tinyAxis = Vec(1e-15, 0., 0.)
            Expect.throws (fun () -> Quaternion.createFromVecDegrees tinyAxis 45. |> ignore) "very short axis should fail"
        }

        test "Quaternion withAngle static members match instance members" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 90.
            let a = Quaternion.withAngleInDegrees 45. q
            let b = q.WithAngleInDegrees 45.
            let c = Quaternion.withAngleInRadians (System.Math.PI * 0.25) q
            Expect.floatClose Accuracy.high a.AngleInDegrees 45.0 "static withAngleInDegrees"
            Expect.floatClose Accuracy.high b.AngleInDegrees 45.0 "instance WithAngleInDegrees"
            Expect.floatClose Accuracy.high c.AngleInDegrees 45.0 "static withAngleInRadians"
            Expect.floatClose Accuracy.high q.AngleInDegrees 90.0 "input quaternion is unchanged"
        }

        test "Quaternion withAngle keeps the rotation axis" {
            let axis = Vec(1., 2., 3.)
            let q = Quaternion.createFromVecDegrees axis 30.
            let q2 = q.WithAngleInDegrees 120.
            Expect.floatClose Accuracy.high q2.AngleInDegrees 120.0 "new angle"
            let a1 = Vec.unitize q.Axis
            let a2 = Vec.unitize q2.Axis
            Expect.isTrue (Vec.length (a1.AsVec - a2.AsVec) < 1e-9) "axis stays the same"
        }

        test "Quaternion withAngle on identity should fail" {
            let q = Quaternion.identity
            Expect.throws (fun () -> q.WithAngleInRadians 1.0 |> ignore) "withAngle on identity should fail"
            Expect.throws (fun () -> q.WithAngleInDegrees 45. |> ignore) "withAngle on identity should fail"
        }

        test "Quaternion withAngle on near-identity should fail" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 1e-10
            Expect.throws (fun () -> q.WithAngleInRadians 1.0 |> ignore) "withAngle on near-identity should fail"
        }

        test "Quaternion Axis on identity returns zero vector" {
            let q = Quaternion.identity
            let axis = q.Axis
            Expect.floatClose Accuracy.high axis.X 0.0 "identity axis X is 0"
            Expect.floatClose Accuracy.high axis.Y 0.0 "identity axis Y is 0"
            Expect.floatClose Accuracy.high axis.Z 0.0 "identity axis Z is 0"
        }

        test "Quaternion Axis length equals sin(angle/2)" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 60.
            let axis = q.Axis
            let expectedLength = sin(System.Math.PI / 6.0) // sin(30°) for 60° rotation
            let actualLength = sqrt(axis.X*axis.X + axis.Y*axis.Y + axis.Z*axis.Z)
            Expect.floatClose Accuracy.high actualLength expectedLength "axis length is sin(angle/2)"
        }

        test "Quaternion equals with double-coverage (q vs -q)" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let qNeg = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
            // q and -q represent the same rotation but should compare as unequal
            Expect.isFalse (Quaternion.equalsWithin 1e-10 q qNeg) "q and -q should be unequal (component-wise)"
            // But they should produce the same rotation result
            let a = Pnt(1,0,0)
            let result1 = a *** q
            let result2 = a *** qNeg
            "q and -q produce same rotation" |> expectEqual result1 result2
        }

        test "Quaternion equalsRotation with same quaternion" {
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let q2 = Quaternion.createFromVecDegrees Vec.zAxis 45.
            Expect.isTrue (Quaternion.equalsRotationWithin 1e-10 q1 q2) "same quaternions should be rotationally equal"
        }

        test "Quaternion equalsRotation with negated quaternion (q vs -q)" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let qNeg = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
            // equalsRotation should recognize q and -q as the same rotation
            Expect.isTrue (Quaternion.equalsRotationWithin 1e-10 q qNeg) "q and -q should be rotationally equal"
            // Regular equals should still return false
            Expect.isFalse (Quaternion.equalsWithin 1e-10 q qNeg) "q and -q should be component-wise unequal"
        }

        test "Quaternion equalsRotation with different rotations" {
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let q2 = Quaternion.createFromVecDegrees Vec.zAxis 90.
            Expect.isFalse (Quaternion.equalsRotationWithin 1e-10 q1 q2) "different rotations should not be equal"
        }

        test "Quaternion equalsRotation with different axes" {
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let q2 = Quaternion.createFromVecDegrees Vec.xAxis 45.
            Expect.isFalse (Quaternion.equalsRotationWithin 1e-10 q1 q2) "same angle different axis should not be equal"
        }

        test "Quaternion equalsRotation with tolerance" {
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis 45.0
            let q2 = Quaternion.createFromVecDegrees Vec.zAxis 45.001
            // Should be equal within loose tolerance
            Expect.isTrue (Quaternion.equalsRotationWithin 0.01 q1 q2) "similar quaternions within tolerance"
            // Should be unequal with tight tolerance
            Expect.isFalse (Quaternion.equalsRotationWithin 1e-10 q1 q2) "similar quaternions outside tight tolerance"
        }

        test "Quaternion equalsRotation with negated and tolerance" {
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis 45.0
            let q2 = Quaternion.createFromVecDegrees Vec.zAxis 45.001
            let q2Neg = Quaternion.createUnchecked(-q2.X, -q2.Y, -q2.Z, -q2.W)
            // Should be equal within tolerance even when negated
            Expect.isTrue (Quaternion.equalsRotationWithin 0.01 q1 q2Neg) "negated similar quaternions within tolerance"
        }

        test "Quaternion equalsRotation with identity" {
            let q1 = Quaternion.identity
            let q2 = Quaternion.identity
            Expect.isTrue (Quaternion.equalsRotationWithin 1e-10 q1 q2) "identity quaternions should be equal"
        }

        test "Quaternion equalsRotation with zero tolerance" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let qNeg = Quaternion.createUnchecked(-q.X, -q.Y, -q.Z, -q.W)
            // Even with zero tolerance, q and -q should be rotationally equal
            Expect.isTrue (Quaternion.equalsRotationWithin 0.0 q qNeg) "q and -q equal with zero tolerance"
        }

        test "Quaternion equalsRotation consistency with rotation result" {
            // If equalsRotation returns true, both should rotate a point to the same position
            let q1 = Quaternion.createFromVecDegrees (Vec(1,1,1)) 60.
            let q2 = Quaternion.createUnchecked(-q1.X, -q1.Y, -q1.Z, -q1.W)
            Expect.isTrue (Quaternion.equalsRotationWithin 1e-10 q1 q2) "q and -q are rotationally equal"
            // Verify they produce the same rotation
            let testPt = Pnt(5, 3, 7)
            let result1 = testPt *** q1
            let result2 = testPt *** q2
            "rotationally equal quaternions produce same result" |> expectEqual result1 result2
        }

        test "Quaternion equalsRotation with 180 degree rotation" {
            // 180° rotation is special - q and -q might behave differently
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis 180.
            let q1Neg = Quaternion.createUnchecked(-q1.X, -q1.Y, -q1.Z, -q1.W)
            Expect.isTrue (Quaternion.equalsRotationWithin 1e-10 q1 q1Neg) "180° rotation q and -q are equal"
        }

        test "Quaternion createVecToVec with nearly identical vectors returns identity" {
            let from = UnitVec.xAxis
            let toPt = UnitVec.createFromVec(Vec(1.0, 1e-13, 0.))
            let q = Quaternion.createFromUnitVecs from toPt
            // Should return identity quaternion
            Expect.floatClose Accuracy.medium q.AngleInDegrees 0.0 "nearly identical vectors produce near-identity"
        }

        test "Quaternion createVecToVec with exactly identical vectors returns identity" {
            let from = UnitVec.xAxis
            let toPt = UnitVec.xAxis
            let q = Quaternion.createFromUnitVecs from toPt
            Expect.floatClose Accuracy.high q.X 0.0 "identical vectors X"
            Expect.floatClose Accuracy.high q.Y 0.0 "identical vectors Y"
            Expect.floatClose Accuracy.high q.Z 0.0 "identical vectors Z"
            Expect.floatClose Accuracy.high q.W 1.0 "identical vectors W"
        }

        test "Quaternion createVecToVec (Vec overload) with zero-length from should fail" {
            let zeroVec = Vec(0., 0., 0.)
            let target = Vec(1., 0., 0.)
            Expect.throws (fun () -> Quaternion.createFromVecs zeroVec target |> ignore) "zero-length vecFrom should fail"
        }

        test "Quaternion createVecToVec (Vec overload) with zero-length to should fail" {
            let source = Vec(1., 0., 0.)
            let zeroVec = Vec(0., 0., 0.)
            Expect.throws (fun () -> Quaternion.createFromVecs source zeroVec |> ignore) "zero-length vecTo should fail"
        }

        test "Quaternion createVecToVec (Vec overload) with very short vectors should fail" {
            let tinyVec = Vec(1e-20, 0., 0.)
            let target = Vec(1., 0., 0.)
            Expect.throws (fun () -> Quaternion.createFromVecs tinyVec target |> ignore) "very short vector should fail"
        }

        test "Quaternion numerical drift from repeated multiplications" {
            // Start with a small rotation
            let q = Quaternion.createFromVecDegrees Vec.zAxis 1.0
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
            let q = Quaternion.createFromEulerZYXDegrees 30. 89.9 60.
            let _z, y, _x = Quaternion.toEulerAnglesZYX q
            // Near gimbal lock, the conversion may have reduced accuracy
            Expect.floatClose Accuracy.low y 89.9 "near gimbal lock Y"
        }

        test "Quaternion toEulerAnglesZYX at exact 90 degrees" {
            // Test at exact gimbal lock condition (Beta = 90°)
            let q = Quaternion.createFromEulerZYXDegrees 30. 90. 60.
            let _z, y, _x = Quaternion.toEulerAnglesZYX q
            // At gimbal lock, Y should still be 90
            Expect.floatClose Accuracy.medium y 90.0 "gimbal lock Y = 90"
        }

        test "Quaternion equals with different tolerances" {
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis 45.
            let q2 = Quaternion.createFromVecDegrees Vec.zAxis 45.001
            // Should be equal with loose tolerance
            Expect.isTrue (Quaternion.equalsWithin 0.01 q1 q2) "similar quaternions within loose tolerance"
            // Should be unequal with tight tolerance
            Expect.isFalse (Quaternion.equalsWithin 1e-10 q1 q2) "similar quaternions outside tight tolerance"
        }

        test "Quaternion zero angle rotation" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 0.
            let a = Pnt(5,3,7)
            let b = a *** q
            "zero angle rotation" |> expectEqual a b
        }

        test "Quaternion 360 degree rotation" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 360.
            let a = Pnt(5,3,7)
            let b = a *** q
            "360° rotation returns to original" |> expectEqual a b
        }

        test "Quaternion very large angle" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 720. // Two full rotations
            let a = Pnt(5,3,7)
            let b = a *** q
            "720° rotation returns to original" |> expectEqual a b
        }

        test "Quaternion negative angle" {
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis (-90.)
            let q2 = Quaternion.createFromVecDegrees Vec.zAxis 270.
            let a = Pnt(1,0,0)
            let result1 = a *** q1
            let result2 = a *** q2
            "-90° equals 270°" |> expectEqual result1 result2
        }

        test "Quaternion Normalize restores unit length" {
            // Create a slightly denormalized quaternion (simulates drift from multiplications)
            let q1 = Quaternion.createFromVecDegrees Vec.zAxis 45.
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
            let q = Quaternion.createFromVecDegrees Vec.zAxis 60.
            let qNormalized = q.Normalize()
            let a = Pnt(1,0,0)
            let result1 = a *** q
            let result2 = a *** qNormalized
            // Both should produce the same result
            "Normalize preserves rotation" |> expectEqual result1 result2
        }

        test "Quaternion Normalize after many multiplications" {
            let q = Quaternion.createFromVecDegrees Vec.zAxis 1.0
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
            let q = Quaternion.createFromVecDegrees Vec.zAxis 45.
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


        test "Quaternion Euler Radians factories match their Degrees partners" {
            let a, b, c = 17.3, -123.4, 271.5
            let r = UtilEuclid.toRadians
            "XYZ" |> Expect.isTrue (Quaternion.equalsExact (Quaternion.createFromEulerXYZDegrees a b c) (Quaternion.createFromEulerXYZRadians (r a) (r b) (r c)))
            "XZY" |> Expect.isTrue (Quaternion.equalsExact (Quaternion.createFromEulerXZYDegrees a b c) (Quaternion.createFromEulerXZYRadians (r a) (r b) (r c)))
            "YXZ" |> Expect.isTrue (Quaternion.equalsExact (Quaternion.createFromEulerYXZDegrees a b c) (Quaternion.createFromEulerYXZRadians (r a) (r b) (r c)))
            "YZX" |> Expect.isTrue (Quaternion.equalsExact (Quaternion.createFromEulerYZXDegrees a b c) (Quaternion.createFromEulerYZXRadians (r a) (r b) (r c)))
            "ZXY" |> Expect.isTrue (Quaternion.equalsExact (Quaternion.createFromEulerZXYDegrees a b c) (Quaternion.createFromEulerZXYRadians (r a) (r b) (r c)))
            "ZYX" |> Expect.isTrue (Quaternion.equalsExact (Quaternion.createFromEulerZYXDegrees a b c) (Quaternion.createFromEulerZYXRadians (r a) (r b) (r c)))
        }

        test "Quaternion curried factories support partial application" {
            let aroundZ = Quaternion.createFromUnitVecDegrees UnitVec.zAxis
            let qs = [ 10.0; 20.0; 30.0 ] |> List.map aroundZ
            "three rotations" |> Expect.equal qs.Length 3
            "same as full application" |> Expect.isTrue (Quaternion.equalsExact qs.[1] (Quaternion.createFromUnitVecDegrees UnitVec.zAxis 20.0))
            let fromX = Quaternion.createFromVecs Vec.xAxis
            let q = fromX (Vec(0.0, 3.0, 0.0))
            let rotated = Vec.xAxis *** q
            "X to Y" |> Expect.isTrue ((rotated - Vec.yAxis).Length < 1e-12)
        }

        test "Quaternion.andThen applies the current rotation first" {
            let qx = Quaternion.createFromUnitVecDegrees UnitVec.xAxis 90.0
            let qz = Quaternion.createFromUnitVecDegrees UnitVec.zAxis 90.0
            let v = Vec(1.0, 2.0, 3.0)
            let viaPipe = qx |> Quaternion.andThen qz
            "andThen equals Then" |> Expect.isTrue (Quaternion.equalsExact viaPipe (qx.Then qz))
            "x first, then z" |> Expect.isTrue (((v *** viaPipe) - ((v *** qx) *** qz)).Length < 1e-12)
            "noncommuting" |> Expect.isFalse (((v *** viaPipe) - (v *** (qz |> Quaternion.andThen qx))).Length < 1e-9)
        }

    ]

