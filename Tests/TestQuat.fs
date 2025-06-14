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

    test "Quaternion 90 inverse" {
        let q = Quaternion.createFromDegree(Vec.Zaxis*9.0, 90.)
        let a = Pnt(9,0,3)
        let b = a *** q
        let iq = q.Inverse
        let c = b *** iq
        "*** inverse" |> expectEqual a c
        let d = a |> Pnt.rotateByQuaternion q |> Pnt.rotateByQuaternion iq
        "rotateByQuaternion inverse" |> expectEqual a c
    }

  ]

