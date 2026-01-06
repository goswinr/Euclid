module TestRect3D

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
    testList "Rect3D" [

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
            let d = l.DistanceRayPoint y
            let r = Rect3D.createFrom3Points (o,x,y)
            "Rect3D.SizeX" |> Expect.isTrue (equ r.SizeX l.Length)
            "Rect3D.SizeY" |> Expect.isTrue (equ r.SizeY d)
        }

        test "Rect3D.fitToPoints - all positive projections" {
            // Reference rectangle at origin with unit axes
            let refRect = Rect3D.createFromVectors(Pnt.Origin, Vec.Xaxis, Vec.Yaxis)
            // Points all in positive quadrant
            let pts = [| Pnt(1., 1., 0.); Pnt(2., 1., 0.); Pnt(2., 2., 0.); Pnt(1., 2., 0.) |]
            let fitted = Rect3D.fitToPoints pts refRect
            "fitToPoints positive - origin X" |> Expect.isTrue (equ fitted.Origin.X 1.0)
            "fitToPoints positive - origin Y" |> Expect.isTrue (equ fitted.Origin.Y 1.0)
            "fitToPoints positive - SizeX" |> Expect.isTrue (equ fitted.SizeX 1.0)
            "fitToPoints positive - SizeY" |> Expect.isTrue (equ fitted.SizeY 1.0)
        }

        test "Rect3D.fitToPoints - all negative projections" {
            // Reference rectangle at origin with unit axes
            let refRect = Rect3D.createFromVectors(Pnt.Origin, Vec.Xaxis, Vec.Yaxis)
            // Points all in negative quadrant
            let pts = [| Pnt(-2., -2., 0.); Pnt(-1., -2., 0.); Pnt(-1., -1., 0.); Pnt(-2., -1., 0.) |]
            let fitted = Rect3D.fitToPoints pts refRect
            "fitToPoints negative - origin X" |> Expect.isTrue (equ fitted.Origin.X -2.0)
            "fitToPoints negative - origin Y" |> Expect.isTrue (equ fitted.Origin.Y -2.0)
            "fitToPoints negative - SizeX" |> Expect.isTrue (equ fitted.SizeX 1.0)
            "fitToPoints negative - SizeY" |> Expect.isTrue (equ fitted.SizeY 1.0)
        }

        test "Rect3D.fitToPoints - mixed projections" {
            // Reference rectangle at origin with unit axes
            let refRect = Rect3D.createFromVectors(Pnt.Origin, Vec.Xaxis, Vec.Yaxis)
            // Points spanning across origin
            let pts = [| Pnt(-1., -1., 0.); Pnt(2., -1., 0.); Pnt(2., 1., 0.); Pnt(-1., 1., 0.) |]
            let fitted = Rect3D.fitToPoints pts refRect
            "fitToPoints mixed - origin X" |> Expect.isTrue (equ fitted.Origin.X -1.0)
            "fitToPoints mixed - origin Y" |> Expect.isTrue (equ fitted.Origin.Y -1.0)
            "fitToPoints mixed - SizeX" |> Expect.isTrue (equ fitted.SizeX 3.0)
            "fitToPoints mixed - SizeY" |> Expect.isTrue (equ fitted.SizeY 2.0)
        }

        test "Rect3D.fitToPoints - offset reference rectangle" {
            // Reference rectangle offset from origin
            let refRect = Rect3D.createFromVectors(Pnt(10., 10., 0.), Vec.Xaxis, Vec.Yaxis)
            // Points relative to offset
            let pts = [| Pnt(11., 11., 0.); Pnt(13., 11., 0.); Pnt(13., 12., 0.); Pnt(11., 12., 0.) |]
            let fitted = Rect3D.fitToPoints pts refRect
            "fitToPoints offset - origin X" |> Expect.isTrue (equ fitted.Origin.X 11.0)
            "fitToPoints offset - origin Y" |> Expect.isTrue (equ fitted.Origin.Y 11.0)
            "fitToPoints offset - SizeX" |> Expect.isTrue (equ fitted.SizeX 2.0)
            "fitToPoints offset - SizeY" |> Expect.isTrue (equ fitted.SizeY 1.0)
        }


    ]