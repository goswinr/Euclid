module TestVectors

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let tol = Accuracy.veryHigh
let tolMed = Accuracy.medium

let tests =
    testList "Basic Vector and Point Types" [

        testList "Pt (2D Point)" [
            test "create 2D point" {
                let pt = Pt(1.0, 2.0)
                "X is 1.0" |> Expect.floatClose tol pt.X 1.0
                "Y is 2.0" |> Expect.floatClose tol pt.Y 2.0
            }

            test "distance between points" {
                let pt1 = Pt(0.0, 0.0)
                let pt2 = Pt(3.0, 4.0)
                let dist = Pt.distance pt1 pt2
                "distance is 5.0" |> Expect.floatClose tol dist 5.0
            }

            test "distanceSq between points" {
                let pt1 = Pt(0.0, 0.0)
                let pt2 = Pt(3.0, 4.0)
                let distSq = Pt.distanceSq pt1 pt2
                "distanceSq is 25.0" |> Expect.floatClose tol distSq 25.0
            }

            test "point addition with vector" {
                let pt = Pt(1.0, 2.0)
                let vc = Vc(3.0, 4.0)
                let result = pt + vc
                "result X is 4.0" |> Expect.floatClose tol result.X 4.0
                "result Y is 6.0" |> Expect.floatClose tol result.Y 6.0
            }

            test "point subtraction yields vector" {
                let pt1 = Pt(5.0, 7.0)
                let pt2 = Pt(2.0, 3.0)
                let vc = pt1 - pt2
                "vector X is 3.0" |> Expect.floatClose tol vc.X 3.0
                "vector Y is 4.0" |> Expect.floatClose tol vc.Y 4.0
            }
        ]

        testList "Vc (2D Vector)" [
            test "create 2D vector" {
                let vc = Vc(3.0, 4.0)
                "X is 3.0" |> Expect.floatClose tol vc.X 3.0
                "Y is 4.0" |> Expect.floatClose tol vc.Y 4.0
            }

            test "vector length" {
                let vc = Vc(3.0, 4.0)
                let len = vc.Length
                "length is 5.0" |> Expect.floatClose tol len 5.0
            }

            test "vector lengthSq" {
                let vc = Vc(3.0, 4.0)
                let lenSq = vc.LengthSq
                "lengthSq is 25.0" |> Expect.floatClose tol lenSq 25.0
            }

            test "vector addition" {
                let vc1 = Vc(1.0, 2.0)
                let vc2 = Vc(3.0, 4.0)
                let result = vc1 + vc2
                "result X is 4.0" |> Expect.floatClose tol result.X 4.0
                "result Y is 6.0" |> Expect.floatClose tol result.Y 6.0
            }

            test "vector subtraction" {
                let vc1 = Vc(5.0, 7.0)
                let vc2 = Vc(2.0, 3.0)
                let result = vc1 - vc2
                "result X is 3.0" |> Expect.floatClose tol result.X 3.0
                "result Y is 4.0" |> Expect.floatClose tol result.Y 4.0
            }

            test "vector scalar multiplication" {
                let vc = Vc(2.0, 3.0)
                let result = vc * 2.0
                "result X is 4.0" |> Expect.floatClose tol result.X 4.0
                "result Y is 6.0" |> Expect.floatClose tol result.Y 6.0
            }

            test "vector scalar division" {
                let vc = Vc(6.0, 8.0)
                let result = vc / 2.0
                "result X is 3.0" |> Expect.floatClose tol result.X 3.0
                "result Y is 4.0" |> Expect.floatClose tol result.Y 4.0
            }
        ]

        testList "UnitVc (2D Unit Vector)" [
            test "create unit vector from components" {
                let uvc = UnitVc.create(3.0, 4.0)
                "X and Y are normalized" |> Expect.isTrue (abs(uvc.X * uvc.X + uvc.Y * uvc.Y - 1.0) < 1e-9)
            }

            test "unit vector X and Y components" {
                let uvc = UnitVc.create(3.0, 4.0)
                "X is 0.6" |> Expect.floatClose tolMed uvc.X 0.6
                "Y is 0.8" |> Expect.floatClose tolMed uvc.Y 0.8
            }

            test "create unit vector from zero throws" {
                let f() = UnitVc.create(0.0, 0.0) |> ignore
                "throws on zero vector" |> Expect.throws f
            }

            test "create unit vector from very small throws" {
                let f() = UnitVc.create(1e-15, 0.0) |> ignore
                "throws on very small vector" |> Expect.throws f
            }

            test "unit vector from normalized Vc" {
                let vc = Vc(3.0, 4.0)
                let uvc = UnitVc.create(vc.X, vc.Y)
                let direct = vc / vc.Length
                "X matches direct normalization" |> Expect.floatClose tol uvc.X direct.X
                "Y matches direct normalization" |> Expect.floatClose tol uvc.Y direct.Y
            }
        ]

        testList "Pnt (3D Point)" [
            test "create 3D point" {
                let pnt = Pnt(1.0, 2.0, 3.0)
                "X is 1.0" |> Expect.floatClose tol pnt.X 1.0
                "Y is 2.0" |> Expect.floatClose tol pnt.Y 2.0
                "Z is 3.0" |> Expect.floatClose tol pnt.Z 3.0
            }

            test "distance between 3D points" {
                let pnt1 = Pnt(0.0, 0.0, 0.0)
                let pnt2 = Pnt(1.0, 2.0, 2.0)
                let dist = Pnt.distance pnt1 pnt2
                "distance is 3.0" |> Expect.floatClose tol dist 3.0
            }

            test "distanceSq between 3D points" {
                let pnt1 = Pnt(0.0, 0.0, 0.0)
                let pnt2 = Pnt(1.0, 2.0, 2.0)
                let distSq = Pnt.distanceSq pnt1 pnt2
                "distanceSq is 9.0" |> Expect.floatClose tol distSq 9.0
            }

            test "point addition with 3D vector" {
                let pnt = Pnt(1.0, 2.0, 3.0)
                let vec = Vec(4.0, 5.0, 6.0)
                let result = pnt + vec
                "result X is 5.0" |> Expect.floatClose tol result.X 5.0
                "result Y is 7.0" |> Expect.floatClose tol result.Y 7.0
                "result Z is 9.0" |> Expect.floatClose tol result.Z 9.0
            }

            test "point subtraction yields 3D vector" {
                let pnt1 = Pnt(5.0, 7.0, 9.0)
                let pnt2 = Pnt(2.0, 3.0, 4.0)
                let vec = pnt1 - pnt2
                "vector X is 3.0" |> Expect.floatClose tol vec.X 3.0
                "vector Y is 4.0" |> Expect.floatClose tol vec.Y 4.0
                "vector Z is 5.0" |> Expect.floatClose tol vec.Z 5.0
            }
        ]

        testList "Vec (3D Vector)" [
            test "create 3D vector" {
                let vec = Vec(3.0, 4.0, 0.0)
                "X is 3.0" |> Expect.floatClose tol vec.X 3.0
                "Y is 4.0" |> Expect.floatClose tol vec.Y 4.0
                "Z is 0.0" |> Expect.floatClose tol vec.Z 0.0
            }

            test "3D vector length" {
                let vec = Vec(2.0, 3.0, 6.0)
                let len = vec.Length
                "length is 7.0" |> Expect.floatClose tol len 7.0
            }

            test "3D vector lengthSq" {
                let vec = Vec(2.0, 3.0, 6.0)
                let lenSq = vec.LengthSq
                "lengthSq is 49.0" |> Expect.floatClose tol lenSq 49.0
            }

            test "3D vector addition" {
                let vec1 = Vec(1.0, 2.0, 3.0)
                let vec2 = Vec(4.0, 5.0, 6.0)
                let result = vec1 + vec2
                "result X is 5.0" |> Expect.floatClose tol result.X 5.0
                "result Y is 7.0" |> Expect.floatClose tol result.Y 7.0
                "result Z is 9.0" |> Expect.floatClose tol result.Z 9.0
            }

            test "3D vector subtraction" {
                let vec1 = Vec(5.0, 7.0, 9.0)
                let vec2 = Vec(2.0, 3.0, 4.0)
                let result = vec1 - vec2
                "result X is 3.0" |> Expect.floatClose tol result.X 3.0
                "result Y is 4.0" |> Expect.floatClose tol result.Y 4.0
                "result Z is 5.0" |> Expect.floatClose tol result.Z 5.0
            }

            test "3D vector scalar multiplication" {
                let vec = Vec(2.0, 3.0, 4.0)
                let result = vec * 2.0
                "result X is 4.0" |> Expect.floatClose tol result.X 4.0
                "result Y is 6.0" |> Expect.floatClose tol result.Y 6.0
                "result Z is 8.0" |> Expect.floatClose tol result.Z 8.0
            }

            test "3D vector scalar division" {
                let vec = Vec(6.0, 8.0, 10.0)
                let result = vec / 2.0
                "result X is 3.0" |> Expect.floatClose tol result.X 3.0
                "result Y is 4.0" |> Expect.floatClose tol result.Y 4.0
                "result Z is 5.0" |> Expect.floatClose tol result.Z 5.0
            }

            test "3D vector cross product" {
                let vec1 = Vec(1.0, 0.0, 0.0)
                let vec2 = Vec(0.0, 1.0, 0.0)
                let cross = Vec.cross(vec1, vec2)
                "cross X is 0.0" |> Expect.floatClose tol cross.X 0.0
                "cross Y is 0.0" |> Expect.floatClose tol cross.Y 0.0
                "cross Z is 1.0" |> Expect.floatClose tol cross.Z 1.0
            }

            test "3D vector dot product" {
                let vec1 = Vec(1.0, 2.0, 3.0)
                let vec2 = Vec(4.0, 5.0, 6.0)
                let dot = Vec.dot(vec1, vec2)
                "dot is 32.0" |> Expect.floatClose tol dot 32.0
            }
        ]

        testList "UnitVec (3D Unit Vector)" [
            test "create 3D unit vector from components" {
                let uvec = UnitVec.create(3.0, 4.0, 0.0)
                "X Y Z are normalized" |> Expect.isTrue (abs(uvec.X * uvec.X + uvec.Y * uvec.Y + uvec.Z * uvec.Z - 1.0) < 1e-9)
            }

            test "3D unit vector X Y Z components" {
                let uvec = UnitVec.create(2.0, 3.0, 6.0)
                let len = sqrt(4.0 + 9.0 + 36.0)
                "X is 2/7" |> Expect.floatClose tolMed uvec.X (2.0/len)
                "Y is 3/7" |> Expect.floatClose tolMed uvec.Y (3.0/len)
                "Z is 6/7" |> Expect.floatClose tolMed uvec.Z (6.0/len)
            }

            test "create 3D unit vector from zero throws" {
                let f() = UnitVec.create(0.0, 0.0, 0.0) |> ignore
                "throws on zero vector" |> Expect.throws f
            }

            test "create 3D unit vector from very small throws" {
                let f() = UnitVec.create(1e-15, 0.0, 0.0) |> ignore
                "throws on very small vector" |> Expect.throws f
            }

            test "3D unit vector from normalized Vec" {
                let vec = Vec(3.0, 4.0, 0.0)
                let uvec = UnitVec.create(vec.X, vec.Y, vec.Z)
                let direct = vec / vec.Length
                "X matches direct normalization" |> Expect.floatClose tol uvec.X direct.X
                "Y matches direct normalization" |> Expect.floatClose tol uvec.Y direct.Y
                "Z matches direct normalization" |> Expect.floatClose tol uvec.Z direct.Z
            }

            test "3D unit vector dot product" {
                let uvec1 = UnitVec.create(1.0, 0.0, 0.0)
                let uvec2 = UnitVec.create(1.0, 1.0, 0.0)
                let dot = UnitVec.dot(uvec1, uvec2)
                "dot is 1/sqrt(2)" |> Expect.floatClose tolMed dot (1.0 / sqrt 2.0)
            }

            test "3D unit vector cross product" {
                let uvec1 = UnitVec.create(1.0, 0.0, 0.0)
                let uvec2 = UnitVec.create(0.0, 1.0, 0.0)
                let cross = UnitVec.cross(uvec1, uvec2)
                "cross X is 0.0" |> Expect.floatClose tol cross.X 0.0
                "cross Y is 0.0" |> Expect.floatClose tol cross.Y 0.0
                "cross Z is 1.0" |> Expect.floatClose tol cross.Z 1.0
            }
        ]

        testList "Edge Cases" [
            test "2D zero vector has zero length" {
                let vc = Vc(0.0, 0.0)
                "length is 0.0" |> Expect.floatClose tol vc.Length 0.0
            }

            test "3D zero vector has zero length" {
                let vec = Vec(0.0, 0.0, 0.0)
                "length is 0.0" |> Expect.floatClose tol vec.Length 0.0
            }

            test "2D very small vector has small length" {
                let vc = Vc(1e-10, 1e-10)
                "length is very small" |> Expect.isTrue (vc.Length < 1e-9)
            }

            test "3D very small vector has small length" {
                let vec = Vec(1e-10, 1e-10, 1e-10)
                "length is very small" |> Expect.isTrue (vec.Length < 1e-9)
            }

            test "2D point equality" {
                let pt1 = Pt(1.0, 2.0)
                let pt2 = Pt(1.0, 2.0)
                let dist = Pt.distance pt1 pt2
                "identical points have zero distance" |> Expect.floatClose tol dist 0.0
            }

            test "3D point equality" {
                let pnt1 = Pnt(1.0, 2.0, 3.0)
                let pnt2 = Pnt(1.0, 2.0, 3.0)
                let dist = Pnt.distance pnt1 pnt2
                "identical points have zero distance" |> Expect.floatClose tol dist 0.0
            }

            test "2D negative vector" {
                let vc = Vc(3.0, 4.0)
                let neg = vc * -1.0
                "negative X is -3.0" |> Expect.floatClose tol neg.X -3.0
                "negative Y is -4.0" |> Expect.floatClose tol neg.Y -4.0
            }

            test "3D negative vector" {
                let vec = Vec(3.0, 4.0, 5.0)
                let neg = vec * -1.0
                "negative X is -3.0" |> Expect.floatClose tol neg.X -3.0
                "negative Y is -4.0" |> Expect.floatClose tol neg.Y -4.0
                "negative Z is -5.0" |> Expect.floatClose tol neg.Z -5.0
            }
        ]
    ]
