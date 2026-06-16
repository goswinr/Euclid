module TestPolyline3D

open Euclid

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPnt a b = Pnt.dist a b < 1e-9

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
            dup.SetPnt (0, Pnt(99,99,99))
            "duplicate changed" |> Expect.isTrue (eqPnt dup.FirstPoint (Pnt(99,99,99)))
            "original not changed" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0,0,0)))
        }

        test "Clone creates independent copy" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
            let pl = Polyline3D(pts)
            let clone = pl.Clone()
            clone.SetPnt (0, Pnt(99,99,99))
            "clone changed" |> Expect.isTrue (eqPnt clone.FirstPoint (Pnt(99,99,99)))
            "original not changed" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0,0,0)))
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
            pl.CloseInPlace(0.01)
            "polyline is now closed" |> Expect.isTrue pl.IsClosed
            "last point equals first" |> Expect.isTrue (eqPnt pl.FirstPoint pl.LastPoint)
        }

        test "CloseIfOpen adds point if too far" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let origCount = pl.PointCount
            pl.CloseInPlace(0.01)
            "point count increased" |> Expect.equal pl.PointCount (origCount + 1)
            "polyline is now closed" |> Expect.isTrue pl.IsClosed
        }

        test "SetPoint updates point" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            pl.SetPnt (1, Pnt(2,2,2))
            "point updated" |> Expect.isTrue (eqPnt pl.AsPoints.[1] (Pnt(2,2,2)))
        }

        test "SetPointXYZClosed on closed polyline updates both first and last" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0,0,0)])
            let pl = Polyline3D(pts)
            pl.SetPointXYZClosed (0, 5., 5., 5.)
            "first point updated" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(5,5,5)))
            "last point also updated" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(5,5,5)))
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

        test "SignedAreaIn2D matches 2D signed area" {
            let pl = Polyline3D.createFromPts [Pnt(0.,0.,7.); Pnt(10.,0.,7.); Pnt(10.,10.,7.); Pnt(0.,10.,7.); Pnt(0.,0.,7.)]
            "projected area is not doubled" |> Expect.floatClose tol pl.SignedAreaIn2D 100.0
        }

        test "static setPointXYZ is curried like setPointXY" {
            let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]
            Polyline3D.setPointXYZ 2. 3. 4. 1 pl
            "point updated" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(2.,3.,4.)))
        }

        test "CloseInPlace with zero tolerance snaps already matching ends" {
            let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(0.,0.,0.)]
            pl.CloseInPlace(0.0)
            "no duplicate closure point added" |> Expect.equal pl.PointCount 3
            "still closed" |> Expect.isTrue pl.IsClosed
        }

        test "createFrom member helpers" {
            let fromXYZ = Polyline3D.createFromXYZMembers [Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
            "XYZ first" |> Expect.isTrue (eqPnt fromXYZ.FirstPoint (Pnt(1.,2.,3.)))
            "XYZ second" |> Expect.isTrue (eqPnt fromXYZ.LastPoint (Pnt(4.,5.,6.)))

            let fromXY = Polyline3D.createFrom2DXYMembers [Pt(1.,2.); Pt(3.,4.)]
            "XY first uses zero z" |> Expect.isTrue (eqPnt fromXY.FirstPoint (Pnt(1.,2.,0.)))
            "XY second uses zero z" |> Expect.isTrue (eqPnt fromXY.LastPoint (Pnt(3.,4.,0.)))

            let fromxyz = Polyline3D.createFromxyzMembers [{| x = 7.; y = 8.; z = 9. |}]
            "lowercase xyz" |> Expect.isTrue (eqPnt fromxyz.FirstPoint (Pnt(7.,8.,9.)))
        }

        test "removeDuplicatePoints uses distance tolerance and preserves closure" {
            let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(0.009,0.009,0.009); Pnt(1.,0.,0.); Pnt(0.,0.,0.)]
            let cleaned = Polyline3D.removeDuplicatePoints 0.01 pl
            "diagonal point is outside Euclidean tolerance and kept" |> Expect.equal cleaned.PointCount 4
            "closure preserved" |> Expect.isTrue cleaned.IsClosed
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

            test "rotate static method" {
                let pts = ResizeArray([Pnt(1,0,0); Pnt(2,0,0)])
                let pl = Polyline3D(pts)
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = Polyline3D.rotate q pl
                "rotate - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint (Pnt(0., 1., 0.)))
            }

            test "RotateWithCenter keeps center fixed" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(2,0,0); Pnt(2,2,0); Pnt(0,2,0)])
                let pl = Polyline3D(pts)
                let center = pl.Center
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = pl.RotateWithCenter(center, q)
                "RotateWithCenter - center" |> Expect.isTrue (eqPnt rotated.Center center)
            }

            test "rotateWithCenter static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(2,0,0); Pnt(2,2,0); Pnt(0,2,0)])
                let pl = Polyline3D(pts)
                let center = pl.Center
                let q = Quaternion.createFromDegree(UnitVec.Zaxis, 90.)
                let rotated = Polyline3D.rotateWithCenter center q pl
                "rotateWithCenter - center" |> Expect.isTrue (eqPnt rotated.Center center)
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
                let rotated = Polyline3D.rotateOnZ r pl
                "rotate2D - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint (Pnt(0., 1., 0.)))
            }
        ]

        testList "Edge case regressions" [
            test "SegmentVectorsXYZ on empty polyline returns empty list" {
                let pl = Polyline3D()
                Expect.equal pl.SegmentVectorsXYZ.Count 0 "empty polyline has no segment vectors"
            }
            test "SegmentVectorsXYZ on single point returns empty list" {
                let pl = Polyline3D.createFromPts [Pnt(1.,1.,1.)]
                Expect.equal pl.SegmentVectorsXYZ.Count 0 "single point has no segment vectors"
            }
            test "close fails with less than 3 points" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]
                Expect.throws (fun () -> Polyline3D.close 1e-6 pl |> ignore) "two point close should fail"
            }
            test "close with 0.0 tolerance on exactly closed polyline adds no point" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,0.,0.)]
                let closed = Polyline3D.close 0.0 pl
                Expect.equal closed.PointCount pl.PointCount "no extra point added"
                Expect.isTrue closed.IsClosed "still closed"
            }
            test "IsAlmostClosed with 0.0 tolerance on exactly closed polyline" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,0.,0.)]
                Expect.isTrue (pl.IsAlmostClosed 0.0) "exactly closed is almost closed with 0.0 tolerance"
                let plOpen = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                Expect.isFalse (plOpen.IsAlmostClosed 0.0) "open is not almost closed with 0.0 tolerance"
            }
            test "removeDuplicatePoints keeps first point when all points collapse" {
                let pl = Polyline3D.createFromPts [Pnt(1.,1.,1.); Pnt(1.0000001,1.,1.)]
                let result = Polyline3D.removeDuplicatePoints 1e-6 pl
                Expect.equal result.PointCount 1 "collapses to one point"
                "kept point is the first point" |> Expect.isTrue (eqPnt (result.GetPt 0) (Pnt(1.,1.,1.)))
            }
            test "removeDuplicateAndColinearPoints removes colinear point" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(2.,0.,0.); Pnt(2.,2.,0.)]
                let result = Polyline3D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-6 pl
                Expect.equal result.PointCount 3 "middle colinear point removed"
            }
            test "removeDuplicateAndColinearPoints fails when all points are duplicates" {
                let pl = Polyline3D.createFromPts [Pnt(1.,1.,1.); Pnt(1.,1.,1.); Pnt(1.,1.,1.)]
                Expect.throws (fun () -> Polyline3D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-6 pl |> ignore) "all duplicates should fail with a clear error"
            }
            test "removeDuplicateAndColinearPoints square" {
                let pl = Polyline3D.createFromPts [ Pnt(2.,0.,0.); Pnt(4.,0.,0.); Pnt(4.,2.,0.); Pnt(4.,4.,0.); Pnt(2.,4.,0.); Pnt(0.,4.,0.); Pnt(0.,2.,0.); Pnt(0.,0.,0.); Pnt(2.,0.,0.)]
                let simplified = Polyline3D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.equal simplified.PointCount 5 "colinear edge midpoints removed, corners kept, stays closed"
                Expect.isTrue simplified.IsClosed "remains closed"
            }
            test "removeDuplicateAndColinearPoints open line with repeats" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(2.,0.,0.); Pnt(3.,0.,0.); Pnt(3.,0.,0.)]
                let simplified = Polyline3D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.equal simplified.PointCount 2 "duplicate end removed; interior colinear points removed"
                "start" |> Expect.isTrue (eqPnt (simplified.GetPt 0) (Pnt(0.,0.,0.)))
                "end" |> Expect.isTrue (eqPnt (simplified.GetPt (simplified.PointCount - 1)) (Pnt(3.,0.,0.)))
            }
            test "removeDuplicateAndColinearPoints colinear start/end segments closed polygon" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(2.,0.,0.); Pnt(2.,2.,0.); Pnt(2.,4.,0.); Pnt(1.,4.,0.); Pnt(0.,4.,0.); Pnt(0.,2.,0.); Pnt(0.,0.,0.)]
                let simplified = Polyline3D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.isTrue simplified.IsClosed "closed retained"
                Expect.equal simplified.PointCount 5 "colinear removal at this tolerance"
            }
            test "removeDuplicateAndColinearPoints colinear in 3D space" {
                // colinear points along a diagonal in 3D
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,1.,1.); Pnt(2.,2.,2.); Pnt(3.,3.,0.)]
                let simplified = Polyline3D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.equal simplified.PointCount 3 "middle colinear point on 3D diagonal removed"
            }
            test "removeDuplicateAndColinearPoints almost colinear tiny angle retained" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(20.,0.2,0.); Pnt(30.,0.,0.)]
                let simplified = Polyline3D.removeDuplicateAndColinearPoints Cosine.``0.01`` 1e-9 pl
                Expect.equal simplified.PointCount 4 "almost straight kept"
            }
            test "removeDuplicateAndColinearPoints single point returns same" {
                let pl = Polyline3D.createFromPts [Pnt(1.,2.,3.)]
                let result = Polyline3D.removeDuplicateAndColinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.equal result.PointCount 1 "single point unchanged"
            }
            test "removeDuplicateAndColinearPoints tolerance too tight throws" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(2.,0.,0.)]
                let tooTight = UtilEuclid.withMeasure 0.9999999999 // cosine of ~0.0 degrees, above Cosine.``0.01``
                Expect.throws
                    (fun () -> Polyline3D.removeDuplicateAndColinearPoints tooTight 1e-9 pl |> ignore)
                    "tolerance tighter than 0.01 degrees should throw"
            }
        ]

        testList "Coverage for untested members" [
            test "equals on clones and on different counts" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                Expect.isTrue (Polyline3D.equalsTol  1e-9 pl (pl.Clone())) "clones are equal"
                let shorter = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]
                Expect.isFalse (Polyline3D.equalsTol  1e-9 pl shorter) "different counts are not equal"
            }
            test "equals uses  distance  per-axis" {
                let a = Polyline3D.createFromPts [Pnt(0.,0.,0.);Pnt(0.,0.,0.)]
                let b = Polyline3D.createFromPts [Pnt(0.,0.,0.);Pnt(0.008, 0.008, 0.008)] // each axis within 0.001, Euclidean distance ~0.001386
                Expect.isFalse (Polyline3D.equalsTol  0.007 a b) "outside Euclidean tolerance"
                Expect.isTrue (Polyline3D.equalsTol   0.009 a b) "inside Euclidean tolerance"
            }
            test "EvaluateAt start, mid-segment and out of range" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                "at 0.0 is start" |> Expect.isTrue (eqPnt (pl.EvaluateAt 0.0) (Pnt(0.,0.,0.)))
                "at 1.5 is middle of second segment" |> Expect.isTrue (eqPnt (pl.EvaluateAt 1.5) (Pnt(1.,0.5,0.)))
                Expect.throws (fun () -> pl.EvaluateAt -1.0 |> ignore) "negative parameter throws"
                Expect.throws (fun () -> pl.EvaluateAt 5.0 |> ignore) "parameter beyond end throws"
            }
            test "ClosestPoint, ClosestParameter and DistanceTo" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                let testPt = Pnt(0.5, -1., 0.)
                "closest point on first segment" |> Expect.isTrue (eqPnt (pl.ClosestPoint testPt) (Pnt(0.5, 0., 0.)))
                "closest parameter is 0.5" |> Expect.floatClose tol (pl.ClosestParameter testPt) 0.5
                "distance is 1.0" |> Expect.floatClose tol (pl.DistanceTo testPt) 1.0
                "closest point index is 1" |> Expect.equal (pl.ClosestPointIndex (Pnt(1.1, 0.1, 0.))) 1
            }
            test "subPolyline forward and reversed" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                let sub = Polyline3D.subPolyline 0.5 1.5 pl
                Expect.equal sub.PointCount 3 "sub polyline has 3 points"
                "sub starts at mid first segment" |> Expect.isTrue (eqPnt sub.FirstPoint (Pnt(0.5, 0., 0.)))
                "sub ends at mid second segment" |> Expect.isTrue (eqPnt sub.LastPoint (Pnt(1., 0.5, 0.)))
                let rev = Polyline3D.subPolyline 1.5 0.5 pl
                "reversed sub starts at mid second segment" |> Expect.isTrue (eqPnt rev.FirstPoint (Pnt(1., 0.5, 0.)))
            }
            test "AverageNormal of counterclockwise square points up" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,1.,0.)]
                let n = pl.AverageNormal
                Expect.isTrue (n.Z > 0.0) "normal points up for CCW square in XY plane"
                Expect.floatClose tol n.X 0.0 "normal X is zero"
                Expect.floatClose tol n.Y 0.0 "normal Y is zero"
            }
            test "Center is average of points" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(2.,0.,0.); Pnt(2.,2.,2.)]
                "center is average" |> Expect.isTrue (eqPnt pl.Center (Pnt(4./3., 2./3., 2./3.)))
                Expect.throws (fun () -> Polyline3D().Center |> ignore) "center of empty polyline throws"
            }
            test "ToString distinguishes open and closed" {
                let plOpen3 = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                let plClosed3 = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,0.,0.)]
                Expect.stringContains (plOpen3.ToString()) "open" "open polyline string says open"
                Expect.stringContains (plClosed3.ToString()) "closed" "closed polyline string says closed"
                Expect.stringContains (Polyline3D().ToString()) "empty" "empty polyline string says empty"
            }
            test "SignedAreaIn2D on empty polyline throws" {
                Expect.throws (fun () -> Polyline3D().SignedAreaIn2D |> ignore) "empty polyline has no signed area"
            }
            test "static addXYZ is curried" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.)]
                Polyline3D.addXYZ 1. 2. 3. pl
                Expect.equal pl.PointCount 2 "point added"
                "added point" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(1.,2.,3.)))
            }
            test "tryFind point helpers" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(5.,1.,2.); Pnt(10.,1.,2.); Pnt(5.,3.,4.)]

                match Polyline3D.tryFind (fun x _ _ -> x = 5.0) pl with
                | Some pt -> "first matching point" |> Expect.isTrue (eqPnt pt (Pnt(5.,1.,2.)))
                | None -> "first matching point" |> Expect.isTrue false

                match Polyline3D.tryFindLast (fun x _ _ -> x = 5.0) pl with
                | Some pt -> "last matching point" |> Expect.isTrue (eqPnt pt (Pnt(5.,3.,4.)))
                | None -> "last matching point" |> Expect.isTrue false

                "first matching flat index" |> Expect.equal (Polyline3D.tryFindIndex (fun x _ _ -> x = 5.0) pl) (Some 3)
                "last matching flat index" |> Expect.equal (Polyline3D.tryFindLastIndex (fun x _ _ -> x = 5.0) pl) (Some 9)
                let noPointMatch =
                    match Polyline3D.tryFind (fun _ _ z -> z = 99.0) pl with
                    | None -> true
                    | Some _ -> false
                "no point match" |> Expect.isTrue noPointMatch
                "no index match on empty polyline" |> Expect.equal (Polyline3D.tryFindIndex (fun _ _ _ -> true) (Polyline3D())) None
            }
            test "iter visits every point" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Pnt>()
                pl |> Polyline3D.iter (fun x y z -> acc.Add(Pnt(x,y,z)))
                "iter count" |> Expect.equal acc.Count 3
                "iter first" |> Expect.isTrue (eqPnt acc.[0] (Pnt(0.,0.,0.)))
                "iter last" |> Expect.isTrue (eqPnt acc.[2] (Pnt(4.,5.,6.)))
            }
            test "iteri passes the flat coordinate index" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let idxs = ResizeArray<int>()
                pl |> Polyline3D.iteri (fun i _ _ _ -> idxs.Add i)
                "iteri flat indices (step 3)" |> Expect.equal (List.ofSeq idxs) [0; 3; 6]
            }
            test "iterPnt visits every point" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Pnt>()
                pl |> Polyline3D.iterPnt (fun pt -> acc.Add pt)
                "iterPnt count" |> Expect.equal acc.Count 3
                "iterPnt first" |> Expect.isTrue (eqPnt acc.[0] (Pnt(0.,0.,0.)))
                "iterPnt last" |> Expect.isTrue (eqPnt acc.[2] (Pnt(4.,5.,6.)))
            }
            test "iteriPnt passes the point position" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let idxs = ResizeArray<int>()
                let acc = ResizeArray<Pnt>()
                pl |> Polyline3D.iteriPnt (fun i pt -> idxs.Add i; acc.Add pt)
                "iteriPnt point positions" |> Expect.equal (List.ofSeq idxs) [0; 1; 2]
                "iteriPnt second point" |> Expect.isTrue (eqPnt acc.[1] (Pnt(1.,2.,3.)))
            }
            test "iterSkipLast skips the last point" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Pnt>()
                pl |> Polyline3D.iterSkipLast (fun x y z -> acc.Add(Pnt(x,y,z)))
                "iterSkipLast count" |> Expect.equal acc.Count 2
                "iterSkipLast first" |> Expect.isTrue (eqPnt acc.[0] (Pnt(0.,0.,0.)))
                "iterSkipLast last" |> Expect.isTrue (eqPnt acc.[1] (Pnt(1.,2.,3.)))
            }
            test "iterPtSkipLast skips the last point" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Pnt>()
                pl |> Polyline3D.iterPntSkipLast (fun pt -> acc.Add pt)
                "iterPtSkipLast count" |> Expect.equal acc.Count 2
                "iterPtSkipLast first" |> Expect.isTrue (eqPnt acc.[0] (Pnt(0.,0.,0.)))
                "iterPtSkipLast last" |> Expect.isTrue (eqPnt acc.[1] (Pnt(1.,2.,3.)))
            }
            test "iterSegments visits consecutive point pairs" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Pnt*Pnt>()
                pl |> Polyline3D.iterSegments (fun x1 y1 z1 x2 y2 z2 -> acc.Add(Pnt(x1,y1,z1), Pnt(x2,y2,z2)))
                "iterSegments count" |> Expect.equal acc.Count 2
                "first segment start" |> Expect.isTrue (eqPnt (fst acc.[0]) (Pnt(0.,0.,0.)))
                "first segment end"   |> Expect.isTrue (eqPnt (snd acc.[0]) (Pnt(1.,2.,3.)))
                "second segment end"  |> Expect.isTrue (eqPnt (snd acc.[1]) (Pnt(4.,5.,6.)))
                // fewer than two points yields no segments
                let single = Polyline3D.createFromPts [Pnt(0.,0.,0.)]
                let mutable called = false
                single |> Polyline3D.iterSegments (fun _ _ _ _ _ _ -> called <- true)
                "no segment on single point" |> Expect.isFalse called
            }
            test "iterLineSegments yields Line3D segments" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Line3D>()
                pl |> Polyline3D.iterLineSegments (fun ln -> acc.Add ln)
                "iterLineSegments count" |> Expect.equal acc.Count 2
                "first line from" |> Expect.isTrue (eqPnt acc.[0].From (Pnt(0.,0.,0.)))
                "first line to"   |> Expect.isTrue (eqPnt acc.[0].To (Pnt(1.,2.,3.)))
                "second line to"  |> Expect.isTrue (eqPnt acc.[1].To (Pnt(4.,5.,6.)))
            }
            test "offset' takes inPlane distance first then perpendicular" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(0.,0.,0.)]
                let viaTupled = Polyline3D.offset(pl, 1.0, 0.5)
                let viaCurried = Polyline3D.offset' 1.0 0.5 pl
                Expect.isTrue (Polyline3D.equalsTol  1e-9 viaTupled viaCurried) "offset' argument order matches offset"
            }
        ]

        testList "AreaIn2D and orientation helpers" [
            test "AreaIn2D is the absolute value of SignedAreaIn2D" {
                let ccw = Polyline3D.createFromPts [Pnt(0.,0.,7.); Pnt(10.,0.,7.); Pnt(10.,10.,7.); Pnt(0.,10.,7.); Pnt(0.,0.,7.)]
                let cw  = Polyline3D.createFromPts [Pnt(0.,0.,7.); Pnt(0.,10.,7.); Pnt(10.,10.,7.); Pnt(10.,0.,7.); Pnt(0.,0.,7.)]
                "CCW area" |> Expect.floatClose tol ccw.AreaIn2D 100.0
                "CW area is still positive" |> Expect.floatClose tol cw.AreaIn2D 100.0
                "static areaIn2D matches" |> Expect.floatClose tol (Polyline3D.areaIn2D cw) 100.0
                "signed area is negative for CW" |> Expect.isTrue (cw.SignedAreaIn2D < 0.0)
            }
            test "ensureCounterClockwiseIn2D returns the same instance when already CCW" {
                let ccw = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(0.,0.,0.)]
                let r = Polyline3D.ensureCounterClockwiseIn2D ccw
                Expect.isTrue (System.Object.ReferenceEquals(r, ccw)) "already CCW returns same instance"
                Expect.isTrue r.IsCounterClockwiseIn2D "result is CCW"
            }
            test "ensureClockwiseIn2D reverses a CCW polyline into a new instance" {
                let ccw = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(0.,0.,0.)]
                let r = Polyline3D.ensureClockwiseIn2D ccw
                Expect.isFalse (System.Object.ReferenceEquals(r, ccw)) "CCW input is reversed to a new instance"
                Expect.isTrue r.IsClockwiseIn2D "result is CW"
                Expect.isTrue ccw.IsCounterClockwiseIn2D "input is not mutated"
            }
            test "ensureClockwiseInPlaceIn2D mutates in place and returns the same instance" {
                let ccw = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(0.,0.,0.)]
                let r = Polyline3D.ensureClockwiseInPlaceIn2D ccw
                Expect.isTrue (System.Object.ReferenceEquals(r, ccw)) "returns same instance"
                Expect.isTrue ccw.IsClockwiseIn2D "original is now CW"
            }
            test "ensureCounterClockwiseInPlaceIn2D mutates a CW polyline in place" {
                let cw = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(0.,10.,0.); Pnt(10.,10.,0.); Pnt(10.,0.,0.); Pnt(0.,0.,0.)]
                let r = Polyline3D.ensureCounterClockwiseInPlaceIn2D cw
                Expect.isTrue (System.Object.ReferenceEquals(r, cw)) "returns same instance"
                Expect.isTrue cw.IsCounterClockwiseIn2D "original is now CCW"
            }
        ]

        testList "removeUTurns and removeUTurnsDeeply" [
            test "simple open U-turn point removed (axis aligned)" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(5.,0.,0.); Pnt(5.,5.,0.); Pnt(5.,1.,0.); Pnt(9.,1.,0.)]
                let simplified = Polyline3D.removeUTurns Cosine.``179.99`` pl
                Expect.equal simplified.PointCount 4 "should remove the U-turn point"
                "removed the U-turn vertex (5,5,0)" |> Expect.isTrue (eqPnt (simplified.GetPt 2) (Pnt(5.,1.,0.)))
            }
            test "U-turn in a non-axis-aligned (XZ) plane is detected" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(2.,0.,0.); Pnt(2.,0.,2.); Pnt(2.,0.,0.5); Pnt(5.,0.,0.5)]
                let simplified = Polyline3D.removeUTurns Cosine.``179.99`` pl
                Expect.equal simplified.PointCount 4 "should remove the 3D U-turn point"
                "removed the U-turn vertex (2,0,2)" |> Expect.isTrue (eqPnt (simplified.GetPt 2) (Pnt(2.,0.,0.5)))
            }
            test "no U-turns returns an equal polyline" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,1.,0.)]
                let r = Polyline3D.removeUTurns Cosine.``179.0`` pl
                Expect.isTrue (Polyline3D.equalsTol  1e-9 r pl) "unchanged when no U-turns"
            }
            test "U-turn at seam keeps closed polyline closed" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(5.,0.001,0.); Pnt(0.,0.,0.)]
                Expect.isTrue pl.IsClosed "input should be closed"
                let simplified = Polyline3D.removeUTurns Cosine.``179.9`` pl
                Expect.isTrue simplified.IsClosed "closed polyline must stay closed after removing seam U-turn"
            }
            test "removeUTurnsDeeply on a simple U matches a single pass" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(5.,0.,0.); Pnt(5.,5.,0.); Pnt(5.,1.,0.); Pnt(9.,1.,0.)]
                let deep = Polyline3D.removeUTurnsDeeply Cosine.``179.99`` pl
                Expect.equal deep.PointCount 4 "single U is fully removed"
            }
            test "removeUTurnsDeeply U-turn at seam keeps closed polyline closed" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(5.,0.001,0.); Pnt(0.,0.,0.)]
                let simplified = Polyline3D.removeUTurnsDeeply Cosine.``179.9`` pl
                Expect.isTrue simplified.IsClosed "closed polyline must stay closed"
            }
        ]

        testList "removeDuplicatePointsFaithfully" [
            test "coplanar re-intersection matches the 2D result" {
                // same configuration as the Polyline2D test, embedded in the z=0 plane
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(0.993,0.,0.); Pnt(1.0,0.002,0.); Pnt(1.,1.,0.)]
                let cleaned = Polyline3D.removeDuplicatePointsFaithfully 0.01 pl
                Expect.equal cleaned.PointCount 3 "one point removed as duplicate within tolerance"
                "edges re-intersected, not moved to the average" |> Expect.isTrue (eqPnt (cleaned.GetPt 1) (Pnt(1.0,0.0,0.)))
                "start preserved" |> Expect.isTrue (eqPnt cleaned.Start pl.Start)
                "end preserved"   |> Expect.isTrue (eqPnt cleaned.End pl.End)
            }
            test "re-intersection works in a tilted (non-axis-aligned) plane" {
                // the same shape mapped by (x,y,0) -> (x,y,x); the two edges stay coplanar so the
                // closest approach is the true intersection. tolerance 0.02 keeps the lengthened cluster within tolerance.
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(0.993,0.,0.993); Pnt(1.0,0.002,1.0); Pnt(1.,1.,1.)]
                let cleaned = Polyline3D.removeDuplicatePointsFaithfully 0.02 pl
                Expect.equal cleaned.PointCount 3 "one point removed"
                "intersection computed in the tilted plane" |> Expect.isTrue (eqPnt (cleaned.GetPt 1) (Pnt(1.0,0.0,1.0)))
                "start preserved" |> Expect.isTrue (eqPnt cleaned.Start pl.Start)
                "end preserved"   |> Expect.isTrue (eqPnt cleaned.End pl.End)
            }
            test "parallel segments fall back to the cluster midpoint" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.005,0.,0.); Pnt(2.,0.,0.)]
                let cleaned = Polyline3D.removeDuplicatePointsFaithfully 0.01 pl
                Expect.equal cleaned.PointCount 3 "one point removed"
                "no intersection possible, so the midpoint of the too-close cluster is used" |> Expect.isTrue (eqPnt (cleaned.GetPt 1) (Pnt(1.0025,0.,0.)))
            }
            test "no change when no points are within tolerance" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                let cleaned = Polyline3D.removeDuplicatePointsFaithfully 0.01 pl
                Expect.equal cleaned.PointCount 3 "nothing removed"
            }
            test "too short polyline returned unchanged" {
                let pl = Polyline3D.createFromPts [Pnt(0.,0.,0.)]
                let cleaned = Polyline3D.removeDuplicatePointsFaithfully 0.01 pl
                Expect.equal cleaned.PointCount 1 "single point returned as is"
            }
        ]
    ]
