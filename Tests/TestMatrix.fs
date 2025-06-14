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



    test "RigidMatrix translate " {
        let m = RigidMatrix.createTranslation (Vec(2,3,4))
        let a = Accuracy.veryHigh
        "Determinant is one " |> Expect.floatClose Accuracy.veryHigh m.Determinant 1.0

        let m = RigidMatrix.createFromQuaternion (Quaternion.createFromDegree(Vec(2,5,6), 16.))
        let a = Accuracy.veryHigh
        "Determinant is one " |> Expect.floatClose Accuracy.veryHigh m.Determinant 1.0
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

    test "RigidMatrix inverse transform ***" {
            let m = RigidMatrix.createTranslation (Vec(-2,-3,4))
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




  ]