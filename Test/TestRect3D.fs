module TestRect3D

open Euclid

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pnt.dist a b < 1e-9
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

        test "Rect3D point iterators" {
            let r = Rect3D.createFromVectors(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 20., 2.))
            let p0 = Pnt(1., 2., 3.)
            let p1 = Pnt(11., 2., 3.)
            let p2 = Pnt(11., 22., 5.)
            let p3 = Pnt(1., 22., 5.)
            let collect iterator =
                let pts = ResizeArray<Pnt>()
                iterator (fun x y z -> pts.Add(Pnt(x, y, z))) r
                pts.ToArray()
            let expectPoints label (actual:Pnt[]) (expected:Pnt[]) =
                label + " length" |> Expect.equal actual.Length expected.Length
                for i = 0 to actual.Length-1 do
                    label + string i |> Expect.isTrue (eq actual.[i] expected.[i])

            expectPoints "iterPointsCCW" (collect Rect3D.iterPointsCCW) [| p0; p1; p2; p3 |]
            expectPoints "iterPointsLoopedCCW" (collect Rect3D.iterPointsLoopedCCW) [| p0; p1; p2; p3; p0 |]
            expectPoints "iterPointsCW" (collect Rect3D.iterPointsCW) [| p0; p3; p2; p1 |]
            expectPoints "iterPointsLoopedCW" (collect Rect3D.iterPointsLoopedCW) [| p0; p3; p2; p1; p0 |]
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

        test "Rect3D.createFromVectors accepts large rect that is perpendicular within tolerance" {
            // dot product of perpendicular axes scales with the square of the size,
            // so an absolute tolerance falsely rejects large valid rectangles.
            let r = Rect3D.createFromVectors(o, Vec(1e5, 0., 0.), Vec(1e-5, 1e5, 0.))
            "SizeX" |> Expect.isTrue (equ r.SizeX 1e5)
            "SizeY" |> Expect.isTrue (equ r.SizeY 1e5)
        }

        test "Rect3D.xAxisUnit / yAxisUnit static members" {
            let r = Rect3D.createFromVectors(o, Vec(2., 0., 0.), Vec(0., 3., 0.))
            "xAxisUnit X" |> Expect.isTrue (equ (Rect3D.xAxisUnit r).X 1.0)
            "yAxisUnit Y" |> Expect.isTrue (equ (Rect3D.yAxisUnit r).Y 1.0)
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

        testList "Transformation Methods" [
            test "Move instance method" {
                let r = Rect3D.createFromVectors(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.))
                let moved = r.Move(Vec(5., 3., 2.))
                "Move - origin" |> Expect.isTrue (eq moved.Origin (Pnt(5., 3., 2.)))
            }

            test "MoveX instance method" {
                let r = Rect3D.createFromVectors(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.))
                let moved = r.MoveX(5.)
                "MoveX - origin" |> Expect.isTrue (eq moved.Origin (Pnt(5., 0., 0.)))
            }

            test "MoveY instance method" {
                let r = Rect3D.createFromVectors(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.))
                let moved = r.MoveY(3.)
                "MoveY - origin" |> Expect.isTrue (eq moved.Origin (Pnt(0., 3., 0.)))
            }

            test "MoveZ instance method" {
                let r = Rect3D.createFromVectors(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.))
                let moved = r.MoveZ(2.)
                "MoveZ - origin" |> Expect.isTrue (eq moved.Origin (Pnt(0., 0., 2.)))
            }

            test "move static method" {
                let r = Rect3D.createFromVectors(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.))
                let moved = Rect3D.move (Vec(5., 3., 2.)) r
                "move - origin" |> Expect.isTrue (eq moved.Origin (Pnt(5., 3., 2.)))
            }

            test "moveX, moveY, moveZ static methods" {
                let r = Rect3D.createFromVectors(Pnt(0., 0., 0.), Vec(10., 0., 0.), Vec(0., 5., 0.))
                let movedX = Rect3D.moveX 5. r
                let movedY = Rect3D.moveY 3. r
                let movedZ = Rect3D.moveZ 2. r
                "moveX" |> Expect.isTrue (eq movedX.Origin (Pnt(5., 0., 0.)))
                "moveY" |> Expect.isTrue (eq movedY.Origin (Pnt(0., 3., 0.)))
                "moveZ" |> Expect.isTrue (eq movedZ.Origin (Pnt(0., 0., 2.)))
            }


            test "Rotate with identity quaternion" {
                let r = Rect3D.createFromVectors(Pnt(1., 2., 3.), Vec(10., 0., 0.), Vec(0., 5., 0.))
                let rotated = r.Rotate(Quaternion.identity)
                "Rotate identity - origin" |> Expect.isTrue (eq rotated.Origin r.Origin)
            }

            test "Rotate 90 degrees around Z axis" {
                let r = Rect3D.createFromVectors(Pnt(1., 0., 0.), Vec(1., 0., 0.), Vec(0., 1., 0.))
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = r.Rotate(q)
                "Rotate 90 - origin" |> Expect.isTrue (eq rotated.Origin (Pnt(0., 1., 0.)))
            }

            test "rotate static method" {
                let r = Rect3D.createFromVectors(Pnt(1., 0., 0.), Vec(1., 0., 0.), Vec(0., 1., 0.))
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = Rect3D.rotate q r
                "rotate static - origin" |> Expect.isTrue (eq rotated.Origin (Pnt(0., 1., 0.)))
            }

            test "RotateWithCenter keeps center fixed" {
                let r = Rect3D.createFromVectors(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.))
                let center = r.Center
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = r.RotateWithCenter(center, q)
                "RotateWithCenter - center" |> Expect.isTrue (eq rotated.Center center)
            }

            test "rotateWithCenter static method" {
                let r = Rect3D.createFromVectors(Pnt(0., 0., 0.), Vec(2., 0., 0.), Vec(0., 2., 0.))
                let center = r.Center
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = Rect3D.rotateWithCenter center q r
                "rotateWithCenter static - center" |> Expect.isTrue (eq rotated.Center center)
            }
        ]

        testList "flip / offsetZ" [
            test "Rect3D.flip" {
                let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))
                let f = Rect3D.flip r
                "flip origin"    |> Expect.isTrue (eq f.Origin (Pnt(4.,2.,0.)))
                "flip farcorner" |> Expect.isTrue (eq f.FarCorner (Pnt(0.,0.,0.)))
                "flip sizeX"     |> Expect.isTrue (equ f.SizeX 2.0)
                "flip sizeY"     |> Expect.isTrue (equ f.SizeY 4.0)
                "flip area"      |> Expect.isTrue (equ f.Area r.Area)
                "flip normal Z"  |> Expect.isTrue (equ f.NormalUnit.Z -1.0)
            }

            test "Rect3D.offsetZ along normal" {
                let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))
                let up = Rect3D.offsetZ 5.0 r
                "offsetZ origin"  |> Expect.isTrue (eq up.Origin (Pnt(0.,0.,5.)))
                "offsetZ sizeX"   |> Expect.isTrue (equ up.SizeX 4.0)
                "offsetZ sizeY"   |> Expect.isTrue (equ up.SizeY 2.0)
                let down = Rect3D.offsetZ -3.0 r
                "offsetZ neg origin" |> Expect.isTrue (eq down.Origin (Pnt(0.,0.,-3.)))
            }
        ]

        testList "Rect3D offsets" [
            test "Rect3D.offset shrinks and keeps center" {
                let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))
                let o = Rect3D.offset 0.5 r
                "offset origin" |> Expect.isTrue (eq o.Origin (Pnt(0.5,0.5,0.)))
                "offset sizeX"  |> Expect.isTrue (equ o.SizeX 3.0)
                "offset sizeY"  |> Expect.isTrue (equ o.SizeY 1.0)
                "offset center" |> Expect.isTrue (eq o.Center r.Center)
            }

            test "Rect3D.offsetVar per-edge distances" {
                // dist array is [Edge01; Edge12; Edge23; Edge30]
                let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))
                let o = Rect3D.offsetVar [|0.2; 0.5; 0.4; 0.1|] r
                "offsetVar origin" |> Expect.isTrue (eq o.Origin (Pnt(0.1,0.2,0.)))
                "offsetVar sizeX"  |> Expect.isTrue (equ o.SizeX 3.4) // 4 - 0.1 - 0.5
                "offsetVar sizeY"  |> Expect.isTrue (equ o.SizeY 1.4) // 2 - 0.2 - 0.4
            }

            test "Rect3D.offsetCorner at corner 0 and 2" {
                let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))
                let c0 = Rect3D.offsetCorner(r, 0, 1.0, 0.5, 1.5, 0.5)
                "corner0 origin" |> Expect.isTrue (eq c0.Origin (Pnt(1.0,0.5,0.)))
                "corner0 sizeX"  |> Expect.isTrue (equ c0.SizeX 1.5)
                "corner0 sizeY"  |> Expect.isTrue (equ c0.SizeY 0.5)
                let c2 = Rect3D.offsetCorner(r, 2, 1.0, 0.5, 1.5, 0.5)
                "corner2 origin" |> Expect.isTrue (eq c2.Origin (Pnt(1.5,1.0,0.)))
                "corner2 sizeX"  |> Expect.isTrue (equ c2.SizeX 1.5)
                "corner2 sizeY"  |> Expect.isTrue (equ c2.SizeY 0.5)
            }

            test "Rect3D.offsetEdge at edge 0 and 2" {
                let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))
                let e0 = Rect3D.offsetEdge(r, 0, 0.3, 0.5, 1.0, 1.0)
                "edge0 origin" |> Expect.isTrue (eq e0.Origin (Pnt(1.0,0.3,0.)))
                "edge0 sizeX"  |> Expect.isTrue (equ e0.SizeX 2.0) // 4 - 1 - 1
                "edge0 sizeY"  |> Expect.isTrue (equ e0.SizeY 0.5) // width
                let e2 = Rect3D.offsetEdge(r, 2, 0.3, 0.5, 1.0, 1.0)
                "edge2 origin" |> Expect.isTrue (eq e2.Origin (Pnt(1.0,1.2,0.)))
                "edge2 sizeX"  |> Expect.isTrue (equ e2.SizeX 2.0)
                "edge2 sizeY"  |> Expect.isTrue (equ e2.SizeY 0.5)
            }
        ]

        testList "Rect3D.subDivide" [
            test "2x1 without gap" {
                let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))
                let g = Rect3D.subDivide(r, 2, 1, 0., 0.)
                "outer length"  |> Expect.equal g.Length 2
                "inner length"  |> Expect.equal g.[0].Length 1
                "sub00 origin"  |> Expect.isTrue (eq g.[0].[0].Origin (Pnt(0.,0.,0.)))
                "sub00 sizeX"   |> Expect.isTrue (equ g.[0].[0].SizeX 2.0)
                "sub00 sizeY"   |> Expect.isTrue (equ g.[0].[0].SizeY 2.0)
                "sub10 origin"  |> Expect.isTrue (eq g.[1].[0].Origin (Pnt(2.,0.,0.)))
            }

            test "2x1 with gap" {
                let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))
                let g = Rect3D.subDivide(r, 2, 1, 0.4, 0.)
                "sub00 sizeX"  |> Expect.isTrue (equ g.[0].[0].SizeX 1.8) // (4 - 0.4)/2
                "sub10 origin" |> Expect.isTrue (eq g.[1].[0].Origin (Pnt(2.2,0.,0.)))
                "sub10 sizeX"  |> Expect.isTrue (equ g.[1].[0].SizeX 1.8)
            }

            test "too small returns empty" {
                let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))
                let g = Rect3D.subDivide(r, 10, 10, 5., 5.)
                "empty outer" |> Expect.equal g.Length 0
            }
        ]

        testList "Rect3D intersections" [
            // A 4 x 2 rectangle in the XY plane, normal pointing +Z
            let r = Rect3D.createFromVectors(Pnt(0.,0.,0.), Vec(4.,0.,0.), Vec(0.,2.,0.))

            test "intersectRayParameters - hits inside the rectangle" {
                let ln = Line3D(Pnt(1.,0.5,5.), Pnt(1.,0.5,-5.))
                match Rect3D.intersectRayParameters ln r with
                | Some (t,tx,ty) ->
                    "ray t"  |> Expect.isTrue (equ t 0.5)
                    "ray tx" |> Expect.isTrue (equ tx 0.25)
                    "ray ty" |> Expect.isTrue (equ ty 0.25)
                | None ->
                    "intersectRayParameters should hit" |> Expect.isTrue false
            }

            test "intersectRayParameters - returns params even outside bounds" {
                let ln = Line3D(Pnt(10.,0.5,5.), Pnt(10.,0.5,-5.))
                match Rect3D.intersectRayParameters ln r with
                | Some (t,tx,ty) ->
                    "ray t"  |> Expect.isTrue (equ t 0.5)
                    "ray tx" |> Expect.isTrue (equ tx 2.5) // outside 0..1 on purpose
                    "ray ty" |> Expect.isTrue (equ ty 0.25)
                | None ->
                    "intersectRayParameters should still resolve" |> Expect.isTrue false
            }

            test "intersectRayParameters - parallel returns None" {
                let ln = Line3D(Pnt(0.,0.,1.), Pnt(4.,0.,1.))
                "parallel ray" |> Expect.isTrue (Option.isNone (Rect3D.intersectRayParameters ln r))
            }

            test "intersectRayParameter - returns line parameter" {
                let ln = Line3D(Pnt(1.,0.5,5.), Pnt(1.,0.5,-5.))
                match Rect3D.intersectRayParameter ln r with
                | Some t -> "ray param t" |> Expect.isTrue (equ t 0.5)
                | None -> "intersectRayParameter should hit" |> Expect.isTrue false
                // beyond the segment, the infinite-ray parameter is still returned
                let ln2 = Line3D(Pnt(1.,0.5,10.), Pnt(1.,0.5,5.))
                match Rect3D.intersectRayParameter ln2 r with
                | Some t -> "ray param t2" |> Expect.isTrue (equ t 2.0)
                | None -> "intersectRayParameter should resolve" |> Expect.isTrue false
                "parallel ray param" |> Expect.isTrue (Option.isNone (Rect3D.intersectRayParameter (Line3D(Pnt(0.,0.,1.), Pnt(4.,0.,1.))) r))
            }

            test "intersectLineParameters - inside the segment and rectangle" {
                let ln = Line3D(Pnt(1.,0.5,5.), Pnt(1.,0.5,-5.))
                match Rect3D.intersectLineParameters ln r with
                | Some (t,tx,ty) ->
                    "line t"  |> Expect.isTrue (equ t 0.5)
                    "line tx" |> Expect.isTrue (equ tx 0.25)
                    "line ty" |> Expect.isTrue (equ ty 0.25)
                | None ->
                    "intersectLineParameters should hit" |> Expect.isTrue false
            }

            test "intersectLineParameters - outside rectangle bounds returns None" {
                let ln = Line3D(Pnt(10.,0.5,5.), Pnt(10.,0.5,-5.))
                "outside rect" |> Expect.isTrue (Option.isNone (Rect3D.intersectLineParameters ln r))
            }

            test "intersectLineParameters - beyond segment returns None" {
                let ln = Line3D(Pnt(1.,0.5,10.), Pnt(1.,0.5,5.)) // plane crossed at t = 2.0
                "beyond segment" |> Expect.isTrue (Option.isNone (Rect3D.intersectLineParameters ln r))
            }

            test "intersectLine - returns intersection point inside" {
                let ln = Line3D(Pnt(1.,0.5,5.), Pnt(1.,0.5,-5.))
                match Rect3D.intersectLine ln r with
                | Some p -> "line point" |> Expect.isTrue (eq p (Pnt(1.,0.5,0.)))
                | None -> "intersectLine should hit" |> Expect.isTrue false
            }

            test "intersectLine - None when outside, beyond, or parallel" {
                let outside = Line3D(Pnt(10.,0.5,5.), Pnt(10.,0.5,-5.))
                let beyond  = Line3D(Pnt(1.,0.5,10.), Pnt(1.,0.5,5.))
                let paral   = Line3D(Pnt(0.,0.,1.), Pnt(4.,0.,1.))
                "outside"  |> Expect.isTrue (Option.isNone (Rect3D.intersectLine outside r))
                "beyond"   |> Expect.isTrue (Option.isNone (Rect3D.intersectLine beyond r))
                "parallel" |> Expect.isTrue (Option.isNone (Rect3D.intersectLine paral r))
            }
        ]

    ]