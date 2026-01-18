module TestPolyline3D

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPnt a b = Pnt.distance a b < 1e-9

let tol = Accuracy.veryHigh

let tests =
    testList "Polyline3D" [

        test "create empty polyline" {
            let pl = Polyline3D()
            "empty polyline has 0 points" |> Expect.equal pl.PointCount 0
            "empty polyline has 0 segments" |> Expect.equal pl.SegmentCount 0
        }

        test "create polyline with capacity" {
            let pl = Polyline3D(10)
            "polyline with capacity has 0 points" |> Expect.equal pl.PointCount 0
        }

        test "create polyline from points" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            "polyline has correct point count" |> Expect.equal pl.PointCount 3
            "polyline has correct segment count" |> Expect.equal pl.SegmentCount 2
        }

        test "polyline length calculation" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let expectedLength = 2.0 // 1.0 + 1.0
            "polyline length is correct" |> Expect.floatClose tol pl.Length expectedLength
        }

        test "polyline length with 3D diagonal" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,1,1)])
            let pl = Polyline3D(pts)
            let expectedLength = sqrt 3.0
            "polyline length is sqrt(3)" |> Expect.floatClose tol pl.Length expectedLength
        }

        test "empty polyline has zero length" {
            let pl = Polyline3D()
            "empty polyline has zero length" |> Expect.floatClose tol pl.Length 0.0
        }

        test "single point polyline has zero length" {
            let pts = ResizeArray([Pnt(0,0,0)])
            let pl = Polyline3D(pts)
            "single point polyline has zero length" |> Expect.floatClose tol pl.Length 0.0
        }

        test "FirstPoint and LastPoint accessors" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            "FirstPoint is correct" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0,0,0)))
            "LastPoint is correct" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(1,1,0)))
        }

        test "Start and End accessors" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            "Start is correct" |> Expect.isTrue (eqPnt pl.Start (Pnt(0,0,0)))
            "End is correct" |> Expect.isTrue (eqPnt pl.End (Pnt(1,1,0)))
        }

        test "SecondPoint and SecondLastPoint accessors" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0,1,0)])
            let pl = Polyline3D(pts)
            "SecondPoint is correct" |> Expect.isTrue (eqPnt pl.SecondPoint (Pnt(1,0,0)))
            "SecondLastPoint is correct" |> Expect.isTrue (eqPnt pl.SecondLastPoint (Pnt(1,1,0)))
        }

        test "GetSegment returns correct segment" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let seg0 = pl.GetSegment(0)
            let seg1 = pl.GetSegment(1)
            "segment 0 from is correct" |> Expect.isTrue (eqPnt seg0.From (Pnt(0,0,0)))
            "segment 0 to is correct" |> Expect.isTrue (eqPnt seg0.To (Pnt(1,0,0)))
            "segment 1 from is correct" |> Expect.isTrue (eqPnt seg1.From (Pnt(1,0,0)))
            "segment 1 to is correct" |> Expect.isTrue (eqPnt seg1.To (Pnt(1,1,0)))
        }

        test "FirstSegment and LastSegment" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let first = pl.FirstSegment
            let last = pl.LastSegment
            "FirstSegment from is correct" |> Expect.isTrue (eqPnt first.From (Pnt(0,0,0)))
            "FirstSegment to is correct" |> Expect.isTrue (eqPnt first.To (Pnt(1,0,0)))
            "LastSegment from is correct" |> Expect.isTrue (eqPnt last.From (Pnt(1,0,0)))
            "LastSegment to is correct" |> Expect.isTrue (eqPnt last.To (Pnt(1,1,0)))
        }

        test "Segments returns all segments" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let segs = pl.Segments
            "segments count is correct" |> Expect.equal segs.Count 2
        }

        test "IsClosed returns true for closed polyline" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0,0,0)])
            let pl = Polyline3D(pts)
            "polyline is closed" |> Expect.isTrue pl.IsClosed
        }

        test "IsClosed returns false for open polyline" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            "polyline is not closed" |> Expect.isFalse pl.IsClosed
        }

        test "IsAlmostClosed returns true within tolerance" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0.001,0,0)])
            let pl = Polyline3D(pts)
            "polyline is almost closed with tolerance 0.01" |> Expect.isTrue (pl.IsAlmostClosed(0.01))
        }

        test "IsAlmostClosed returns false outside tolerance" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0.1,0,0)])
            let pl = Polyline3D(pts)
            "polyline is not almost closed with tolerance 0.01" |> Expect.isFalse (pl.IsAlmostClosed(0.01))
        }

        test "BoundingBox is correct" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(2,3,4); Pnt(1,1,1)])
            let pl = Polyline3D(pts)
            let bbox = pl.BoundingBox
            "bounding box MinX is 0" |> Expect.floatClose tol bbox.MinX 0.0
            "bounding box MinY is 0" |> Expect.floatClose tol bbox.MinY 0.0
            "bounding box MinZ is 0" |> Expect.floatClose tol bbox.MinZ 0.0
            "bounding box MaxX is 2" |> Expect.floatClose tol bbox.MaxX 2.0
            "bounding box MaxY is 3" |> Expect.floatClose tol bbox.MaxY 3.0
            "bounding box MaxZ is 4" |> Expect.floatClose tol bbox.MaxZ 4.0
        }

        test "Duplicate creates independent copy" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
            let pl = Polyline3D(pts)
            let dup = pl.Duplicate()
            pts.[0] <- Pnt(99,99,99)
            "original changed" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(99,99,99)))
            "duplicate not changed" |> Expect.isTrue (eqPnt dup.FirstPoint (Pnt(0,0,0)))
        }

        test "Clone creates independent copy" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
            let pl = Polyline3D(pts)
            let clone = pl.Clone()
            pts.[0] <- Pnt(99,99,99)
            "original changed" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(99,99,99)))
            "clone not changed" |> Expect.isTrue (eqPnt clone.FirstPoint (Pnt(0,0,0)))
        }

        test "ReverseInPlace reverses the polyline" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            pl.ReverseInPlace()
            "first point after reverse" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(1,1,0)))
            "last point after reverse" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(0,0,0)))
        }

        test "Reverse returns new reversed polyline" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let rev = pl.Reverse()
            "original first point unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0,0,0)))
            "reversed first point" |> Expect.isTrue (eqPnt rev.FirstPoint (Pnt(1,1,0)))
            "reversed last point" |> Expect.isTrue (eqPnt rev.LastPoint (Pnt(0,0,0)))
        }

        test "CloseIfOpen closes almost closed polyline" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0.001,0,0)])
            let pl = Polyline3D(pts)
            pl.CloseIfOpen(0.01)
            "polyline is now closed" |> Expect.isTrue pl.IsClosed
            "last point equals first" |> Expect.isTrue (eqPnt pl.FirstPoint pl.LastPoint)
        }

        test "CloseIfOpen adds point if too far" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let origCount = pl.PointCount
            pl.CloseIfOpen(0.01)
            "point count increased" |> Expect.equal pl.PointCount (origCount + 1)
            "polyline is now closed" |> Expect.isTrue pl.IsClosed
        }

        test "SetVertex updates vertex" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            pl.SetVertex 1 (Pnt(2,2,2))
            "vertex updated" |> Expect.isTrue (eqPnt pl.Points.[1] (Pnt(2,2,2)))
        }

        test "SetVertex on closed polyline updates both first and last" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0,0,0)])
            let pl = Polyline3D(pts)
            pl.SetVertex 0 (Pnt(5,5,5))
            "first point updated" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(5,5,5)))
            "last point also updated" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(5,5,5)))
        }

        test "SetVertex with last index on closed polyline updates both" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0,0,0)])
            let pl = Polyline3D(pts)
            pl.SetVertex (pl.LastPointIndex) (Pnt(5,5,5))
            "last point updated" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(5,5,5)))
            "first point also updated" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(5,5,5)))
        }

        test "ToString works for empty polyline" {
            let pl = Polyline3D()
            let s = pl.ToString()
            "ToString contains 'empty'" |> Expect.stringContains s "empty"
        }

        test "ToString works for non-empty polyline" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
            let pl = Polyline3D(pts)
            let s = pl.ToString()
            "ToString contains 'Polyline3D'" |> Expect.stringContains s "Polyline3D"
            "ToString contains 'length'" |> Expect.stringContains s "length"
        }

        test "AsString for open polyline" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
            let pl = Polyline3D(pts)
            let s = pl.AsString
            "AsString contains 'open'" |> Expect.stringContains s "open"
        }

        test "AsString for closed polyline" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0,0,0)])
            let pl = Polyline3D(pts)
            let s = pl.AsString
            "AsString contains 'closed'" |> Expect.stringContains s "closed"
        }

        test "GetSegment out of range throws" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
            let pl = Polyline3D(pts)
            let f() = pl.GetSegment(5) |> ignore
            "GetSegment out of range throws" |> Expect.throws f
        }

        test "FirstPoint on empty polyline throws" {
            let pl = Polyline3D()
            let f() = pl.FirstPoint |> ignore
            "FirstPoint on empty polyline throws" |> Expect.throws f
        }

        test "SecondPoint on single point polyline throws" {
            let pts = ResizeArray([Pnt(0,0,0)])
            let pl = Polyline3D(pts)
            let f() = pl.SecondPoint |> ignore
            "SecondPoint on single point polyline throws" |> Expect.throws f
        }


        test "LastSegment on single point polyline throws" {
            let pts = ResizeArray([Pnt(0,0,0)])
            let pl = Polyline3D(pts)
            let f() = pl.LastSegment |> ignore
            "LastSegment on single point polyline throws" |> Expect.throws f
        }

        test "AsFSharpCode generates valid code" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
            let pl = Polyline3D(pts)
            let code = pl.AsFSharpCode
            "AsFSharpCode contains 'Polyline3D.create'" |> Expect.stringContains code "Polyline3D.create"
        }

        test "polyline with collinear points" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(2,0,0); Pnt(3,0,0)])
            let pl = Polyline3D(pts)
            let expectedLength = 3.0
            "length of collinear points" |> Expect.floatClose tol pl.Length expectedLength
        }

        test "polyline in 3D space" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,1,1); Pnt(2,2,2)])
            let pl = Polyline3D(pts)
            let expectedLength = 2.0 * sqrt 3.0
            "length in 3D space" |> Expect.floatClose tol pl.Length expectedLength
        }

        test "closed cube edge polyline" {
            // Four edges forming a square, closed
            let pts = ResizeArray([
                Pnt(0,0,0)
                Pnt(1,0,0)
                Pnt(1,1,0)
                Pnt(0,1,0)
                Pnt(0,0,0)
            ])
            let pl = Polyline3D(pts)
            "is closed" |> Expect.isTrue pl.IsClosed
            "length is 4" |> Expect.floatClose tol pl.Length 4.0
        }

        test "LastPointIndex and LastSegmentIndex" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            "LastPointIndex is 2" |> Expect.equal pl.LastPointIndex 2
            "LastSegmentIndex is 1" |> Expect.equal pl.LastSegmentIndex 1
        }

        testList "Transformation Methods" [
            test "Move instance method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
                let pl = Polyline3D(pts)
                let moved = pl.Move(Vec(5., 3., 2.))
                "Move - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(5., 3., 2.)))
                "Move - last point" |> Expect.isTrue (eqPnt moved.LastPoint (Pnt(6., 4., 2.)))
            }

            test "MoveX instance method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = pl.MoveX(5.)
                "MoveX - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(5., 0., 0.)))
            }

            test "MoveY instance method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = pl.MoveY(3.)
                "MoveY - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(0., 3., 0.)))
            }

            test "MoveZ instance method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = pl.MoveZ(2.)
                "MoveZ - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(0., 0., 2.)))
            }

            test "move static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = Polyline3D.move (Vec(5., 3., 2.)) pl
                "move static - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(5., 3., 2.)))
            }

            test "translate static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = Polyline3D.translate (Vec(5., 3., 2.)) pl
                "translate static - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(5., 3., 2.)))
            }

            test "moveX, moveY, moveZ static methods" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let movedX = Polyline3D.moveX 5. pl
                let movedY = Polyline3D.moveY 3. pl
                let movedZ = Polyline3D.moveZ 2. pl
                "moveX static" |> Expect.isTrue (eqPnt movedX.FirstPoint (Pnt(5., 0., 0.)))
                "moveY static" |> Expect.isTrue (eqPnt movedY.FirstPoint (Pnt(0., 3., 0.)))
                "moveZ static" |> Expect.isTrue (eqPnt movedZ.FirstPoint (Pnt(0., 0., 2.)))
            }

            test "Transform with identity matrix" {
                let pts = ResizeArray([Pnt(1,2,3); Pnt(4,5,6)])
                let pl = Polyline3D(pts)
                let transformed = pl.Transform(Matrix.identity)
                "Transform identity - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint pl.FirstPoint)
            }

            test "Transform with translation matrix" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                let transformed = pl.Transform(m)
                "Transform translation - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint (Pnt(5., 3., 2.)))
            }

            test "transform static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                let transformed = Polyline3D.transform m pl
                "transform static - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint (Pnt(5., 3., 2.)))
            }

            test "TransformRigid instance method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                let transformed = pl.TransformRigid(m)
                "TransformRigid - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint (Pnt(5., 3., 2.)))
            }

            test "transformRigid static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                let transformed = Polyline3D.transformRigid m pl
                "transformRigid static - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint (Pnt(5., 3., 2.)))
            }

            test "Rotate with identity quaternion" {
                let pts = ResizeArray([Pnt(1,2,3); Pnt(4,5,6)])
                let pl = Polyline3D(pts)
                let rotated = pl.Rotate(Quaternion.identity)
                "Rotate identity - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint pl.FirstPoint)
            }

            test "Rotate 90 degrees around Z axis" {
                let pts = ResizeArray([Pnt(1,0,0); Pnt(2,0,0)])
                let pl = Polyline3D(pts)
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = pl.Rotate(q)
                "Rotate 90 - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint (Pnt(0., 1., 0.)))
            }

            test "rotateByQuaternion static method" {
                let pts = ResizeArray([Pnt(1,0,0); Pnt(2,0,0)])
                let pl = Polyline3D(pts)
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = Polyline3D.rotateByQuaternion q pl
                "rotateByQuaternion - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint (Pnt(0., 1., 0.)))
            }

            test "RotateWithCenter keeps center fixed" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(2,0,0); Pnt(2,2,0); Pnt(0,2,0)])
                let pl = Polyline3D(pts)
                let center = pl.Center
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = pl.RotateWithCenter(center, q)
                "RotateWithCenter - center" |> Expect.isTrue (eqPnt rotated.Center center)
            }

            test "rotateWithCenterByQuaternion static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(2,0,0); Pnt(2,2,0); Pnt(0,2,0)])
                let pl = Polyline3D(pts)
                let center = pl.Center
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = Polyline3D.rotateWithCenterByQuaternion center q pl
                "rotateWithCenterByQuaternion - center" |> Expect.isTrue (eqPnt rotated.Center center)
            }

            test "Scale instance method" {
                let pts = ResizeArray([Pnt(1,2,3); Pnt(2,4,6)])
                let pl = Polyline3D(pts)
                let scaled = pl.Scale(2.)
                "Scale - first point" |> Expect.isTrue (eqPnt scaled.FirstPoint (Pnt(2., 4., 6.)))
                "Scale - last point" |> Expect.isTrue (eqPnt scaled.LastPoint (Pnt(4., 8., 12.)))
            }

            test "scale static method" {
                let pts = ResizeArray([Pnt(1,2,3); Pnt(2,4,6)])
                let pl = Polyline3D(pts)
                let scaled = Polyline3D.scale 2. pl
                "scale static - first point" |> Expect.isTrue (eqPnt scaled.FirstPoint (Pnt(2., 4., 6.)))
            }

            test "rotate2D static method" {
                let pts = ResizeArray([Pnt(1,0,0); Pnt(2,0,0)])
                let pl = Polyline3D(pts)
                let r = Rotation2D.createFromDegrees 90.
                let rotated = Polyline3D.rotate2D r pl
                "rotate2D - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint (Pnt(0., 1., 0.)))
            }
        ]
    ]
