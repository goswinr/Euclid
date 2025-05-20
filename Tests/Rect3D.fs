module Rect3D

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pnt.distance a b < 1e-9
let inline equ a (b:float) = abs(a-b) < 1e-9


let o = Pnt.Origin
let x = Vec.Xaxis
let y = Vec.Yaxis
let rect = Rect3D.createFromVectors(o,x,y)

let tests =
  testList "samples" [

    test "Rect3D.grid" {
      let grid = Rect3D.grid (rect, 2, 2)
      "2-Rect3D.grid: array outer length" |> Expect.equal grid.Length 2
      "2-Rect3D.grid: array inner length" |> Expect.equal grid[0].Length 2
      "2-Corner1,1" |> Expect.isTrue (eq grid.[1].[1] (Pnt(1,1,0)))
      "2-Corner0,0" |> Expect.isTrue (eq grid.[0].[0] (Pnt(0,0,0)))

      let grid = Rect3D.grid (rect, 3, 3)
      "Rect3D.grid: array outer length" |> Expect.equal grid.Length 3
      "Rect3D.grid: array inner length" |> Expect.equal grid[0].Length 3
      "3-Corner0,0" |> Expect.isTrue (eq grid.[0].[0] (Pnt(0,0,0)))
      "3-Corner1,1" |> Expect.isTrue (eq grid.[1].[1] (Pnt(0.5,0.5,0)))
      "3-Corner2,2" |> Expect.isTrue (eq grid.[2].[2] (Pnt(1,1,0)))
      "3-Corner0,0" |> Expect.isTrue (eq grid.[0].[2] (Pnt(0,1,0)))

    }

    test "Rect3D.SizeX" {
        let o = Pnt(1,2,3)
        let x = Pnt(5,7,8)
        let y = Pnt(-8,5,4)
        let l = Line3D(o,x)
        let d = l.DistanceToPntInfinite y
        let r = Rect3D.createFrom3Points (o,x,y)
        "Rect3D.SizeX" |> Expect.isTrue (equ r.SizeX l.Length)
        "Rect3D.SizeY" |> Expect.isTrue (equ r.SizeY d)
    }











  ]