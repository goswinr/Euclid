module TestRect2D

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pt.distance a b < 1e-9

let o = Pt.Origin
let x = Vc.Xaxis
let y = Vc.Yaxis
let rect = Rect2D.createFromVectors(o,x,y)

let tests =
  testList "Rect2D" [

    test "Rect2D.grid" {
      let grid = Rect2D.grid (rect, 2, 2)
      "2-Rect2D.grid: array outer length" |> Expect.equal grid.Length 2
      "2-Rect2D.grid: array inner length" |> Expect.equal grid[0].Length 2
      "2-Corner1,1" |> Expect.isTrue (eq grid.[1].[1] (Pt(1,1)))
      "2-Corner0,0" |> Expect.isTrue (eq grid.[0].[0] (Pt(0,0)))

      let grid = Rect2D.grid (rect, 3, 3)
      "Rect2D.grid: array outer length" |> Expect.equal grid.Length 3
      "Rect2D.grid: array inner length" |> Expect.equal grid[0].Length 3
      "3-Corner0,0" |> Expect.isTrue (eq grid.[0].[0] (Pt(0,0)))
      "3-Corner1,1" |> Expect.isTrue (eq grid.[1].[1] (Pt(0.5,0.5)))
      "3-Corner2,2" |> Expect.isTrue (eq grid.[2].[2] (Pt(1,1)))
      "3-Corner0,0" |> Expect.isTrue (eq grid.[0].[2] (Pt(0,1)))


      let grid = Rect2D.gridMaxLength (rect, 0.4, 0.4)
      "Rect2D.gridMaxLength: 0.4 -> 4 points" |> Expect.equal grid.Length 4
      let grid = Rect2D.gridMaxLength (rect, 0.5, 0.5)
      "Rect2D.gridMaxLength: 0.5 -> 3 points" |> Expect.equal grid.Length 3
      let grid = Rect2D.gridMaxLength (rect, 0.6, 0.6)
      "Rect2D.gridMaxLength: 0.6 -> 3 points" |> Expect.equal grid.Length 3

      let grid = Rect2D.gridMinLength (rect, 0.4, 0.4)
      "Rect2D.gridMinLength: 1/0.4 -> 3 points" |> Expect.equal grid.Length 3
      let grid = Rect2D.gridMinLength (rect, 0.5, 0.5)
      "Rect2D.gridMinLength: 1/0.5 -> 3 points" |> Expect.equal grid.Length 3
      let grid = Rect2D.gridMinLength (rect, 0.6, 0.6)
      "Rect2D.gridMinLength: 1/0.6 -> 2 points" |> Expect.equal grid.Length 2

      let grid = Rect2D.subDivideMaxLength (rect, 0.4, 0.4, 0., 0.)
      "Rect2D.subDivideMaxLength: 0.4 -> 3 rects" |> Expect.equal grid.Length 3
      let grid = Rect2D.subDivideMaxLength (rect, 0.5, 0.5, 0., 0.)
      "Rect2D.subDivideMaxLength: 0.5 -> 2 rects" |> Expect.equal grid.Length 2
      let grid = Rect2D.subDivideMaxLength (rect, 0.6, 0.6, 0., 0.)
      "Rect2D.subDivideMaxLength: 0.6 -> 2 rects" |> Expect.equal grid.Length 2

      let grid = Rect2D.subDivideMinLength (rect, 0.4, 0.4, 0., 0.)
      "Rect2D.subDivideMinLength: 1/0.4 -> 2 rects" |> Expect.equal grid.Length 2
      let grid = Rect2D.subDivideMinLength (rect, 0.5, 0.5, 0., 0.)
      "Rect2D.subDivideMinLength: 1/0.5 -> 2 rects" |> Expect.equal grid.Length 2
      let grid = Rect2D.subDivideMinLength (rect, 0.6, 0.6, 0., 0.)
      "Rect2D.subDivideMinLength: 1/0.6 -> 1 rects" |> Expect.equal grid.Length 1


    }
  ]
