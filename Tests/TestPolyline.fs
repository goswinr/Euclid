module TestPolyline

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pt.distance a b < 1e-9

let pl = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.); Pt(0.,0.)]
let plo = Polyline2D.create [Pt(0.,0.); Pt(10.,0.); Pt(10.,10.); Pt(0.,10.)]


let tests =
  testList "Polyline2D offsetting" [

    test "5 points" {
        let o = Polyline2D.offset (pl, 2.)
        "pt 0 ok" |> Expect.isTrue (eq o.Points.[0] (Pt(2,2)))
        "pt 1 ok" |> Expect.isTrue (eq o.Points.[1] (Pt(8,2)))
        "pt 2 ok" |> Expect.isTrue (eq o.Points.[2] (Pt(8,8)))
        "pt 3 ok" |> Expect.isTrue (eq o.Points.[3] (Pt(2,8)))
        "pt 4 ok" |> Expect.isTrue (eq o.Points.[4] (Pt(2,2)))
    }

    test "4 points open" {
        let o = Polyline2D.offset (plo, 2.)
        "pt 0 ok" |> Expect.isTrue (eq o.Points.[0] (Pt(0,2)))
        "pt 1 ok" |> Expect.isTrue (eq o.Points.[1] (Pt(8,2)))
        "pt 2 ok" |> Expect.isTrue (eq o.Points.[2] (Pt(8,8)))
        "pt 3 ok" |> Expect.isTrue (eq o.Points.[3] (Pt(0,8)))
    }

    test "4 points looped" {
        let o = Polyline2D.offset (plo, 2., loop=true)
        "pt 0 ok" |> Expect.isTrue (eq o.Points.[0] (Pt(2,2)))
        "pt 1 ok" |> Expect.isTrue (eq o.Points.[1] (Pt(8,2)))
        "pt 2 ok" |> Expect.isTrue (eq o.Points.[2] (Pt(8,8)))
        "pt 3 ok" |> Expect.isTrue (eq o.Points.[3] (Pt(2,8)))
    }


    test "4 points looped referenceOrient = 1" {
        let o = Polyline2D.offset (plo, 2., loop=true, referenceOrient = 1.)
        "pt 0 ok" |> Expect.isTrue (eq o.Points.[0] (Pt(2,2)))
        "pt 1 ok" |> Expect.isTrue (eq o.Points.[1] (Pt(8,2)))
        "pt 2 ok" |> Expect.isTrue (eq o.Points.[2] (Pt(8,8)))
        "pt 3 ok" |> Expect.isTrue (eq o.Points.[3] (Pt(2,8)))
    }

    test "4 points looped referenceOrient = -1" {
        // bad orient, so offset is in the other direction
        let o = Polyline2D.offset (plo, -2., loop=true, referenceOrient = -1.)
        "pt 0 ok" |> Expect.isTrue (eq o.Points.[0] (Pt(2,2)))
        "pt 1 ok" |> Expect.isTrue (eq o.Points.[1] (Pt(8,2)))
        "pt 2 ok" |> Expect.isTrue (eq o.Points.[2] (Pt(8,8)))
        "pt 3 ok" |> Expect.isTrue (eq o.Points.[3] (Pt(2,8)))
    }

    test "wrong dist count " {
        Expect.throws (fun () -> Polyline2D.offset (plo, [| 0.;0.|]) |> ignore ) " just two distances"
        Expect.throws (fun () -> Polyline2D.offset (plo, [| 0.;0.;0.;0.|]) |> ignore ) " four but 3 wanted distances"
    }

    test "4 points looped, 4 params" {
        let o = Polyline2D.offset (plo,  [| 4.;2.;2; 2 |] , loop=true)
        "pt 0 ok" |> Expect.isTrue (eq o.Points.[0] (Pt(2,4)))
        "pt 1 ok" |> Expect.isTrue (eq o.Points.[1] (Pt(8,4)))
        "pt 2 ok" |> Expect.isTrue (eq o.Points.[2] (Pt(8,8)))
        "pt 3 ok" |> Expect.isTrue (eq o.Points.[3] (Pt(2,8)))
    }

    test "4 points open, 3 params" {
        let o = Polyline2D.offset (plo, [| 4.;2.;2 |] )
        "pt 0 ok" |> Expect.isTrue (eq o.Points.[0] (Pt(0,4)))
        "pt 1 ok" |> Expect.isTrue (eq o.Points.[1] (Pt(8,4)))
        "pt 2 ok" |> Expect.isTrue (eq o.Points.[2] (Pt(8,8)))
        "pt 3 ok" |> Expect.isTrue (eq o.Points.[3] (Pt(0,8)))
    }

    test "5 points, 4 params" {
        let o = Polyline2D.offset (pl, [| 4.;2.;2; 2 |], loop=true)
        "pt 0 ok" |> Expect.isTrue (eq o.Points.[0] (Pt(2,4)))
        "pt 1 ok" |> Expect.isTrue (eq o.Points.[1] (Pt(8,4)))
        "pt 2 ok" |> Expect.isTrue (eq o.Points.[2] (Pt(8,8)))
        "pt 3 ok" |> Expect.isTrue (eq o.Points.[3] (Pt(2,8)))
        "pt 4 ok" |> Expect.isTrue (eq o.Points.[4] (Pt(2,4)))
    }

    test "5 points outwards" {
        let o = Polyline2D.offset (pl, -2.)
        "pt 0 ok" |> Expect.isTrue (eq o.Points.[0] (Pt(-2,-2)))
        "pt 1 ok" |> Expect.isTrue (eq o.Points.[1] (Pt(12,-2)))
        "pt 2 ok" |> Expect.isTrue (eq o.Points.[2] (Pt(12,12)))
        "pt 3 ok" |> Expect.isTrue (eq o.Points.[3] (Pt(-2,12)))
        "pt 4 ok" |> Expect.isTrue (eq o.Points.[4] (Pt(-2,-2)))
    }

  ]
