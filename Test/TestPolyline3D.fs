module TestPolyline3D

open Euclid

open EuclidTestSupport

let inline eqPnt a b = Pnt.dist a b < 1e-9

let inline eqVec a b = Vec.length (a - b) < 1e-9

let tol = Accuracy.veryHigh

/// A closed, non-planar quadrilateral that is rotated and away from the origin, for the MovePnt tests.
let movePntQuad () =
    Polyline3D.createFromPnts [Pnt(12.5, 7.25, 3.5); Pnt(17.75, 9.5, 4.25); Pnt(15.25, 14.75, 6.0); Pnt(10.5, 12.25, 5.25); Pnt(12.5, 7.25, 3.5)]

/// The vector the MovePnt tests move by.
let movePntVec = Vec(0.3, -1.7, 2.9)

/// Checks that two points are bit-identical, not just close.
let expectSamePnt (a:Pnt) (b:Pnt) (msg:string) =
    Expect.isTrue (a.X = b.X && a.Y = b.Y && a.Z = b.Z) $"{msg}: {a.AsString} is not exactly {b.AsString}"

let tests =
    testList "Polyline3D" [

        test "create empty polyline" {
            let pl = Polyline3D([])
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
            let pl = Polyline3D([])
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

        test "Reverse reverses the polyline in place" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let xyzs = pl.XYZs
            pl.Reverse()
            "same coordinate list after reverse" |> Expect.isTrue (obj.ReferenceEquals(xyzs, pl.XYZs))
            "first point after reverse" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(1,1,0)))
            "last point after reverse" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(0,0,0)))
        }

        test "Reverse on a Duplicate keeps the original" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let rev = pl.Duplicate()
            rev.Reverse()
            "original first point unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0,0,0)))
            "reversed first point" |> Expect.isTrue (eqPnt rev.FirstPoint (Pnt(1,1,0)))
            "reversed last point" |> Expect.isTrue (eqPnt rev.LastPoint (Pnt(0,0,0)))
        }

        test "Close closes almost closed polyline" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0.001,0,0)])
            let pl = Polyline3D(pts)
            pl.Close(0.01)
            "polyline is now closed" |> Expect.isTrue pl.IsClosed
            "last point equals first" |> Expect.isTrue (eqPnt pl.FirstPoint pl.LastPoint)
        }

        test "Close adds point if too far" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            let origCount = pl.PointCount
            pl.Close(0.01)
            "point count increased" |> Expect.equal pl.PointCount (origCount + 1)
            "polyline is now closed" |> Expect.isTrue pl.IsClosed
        }

        test "SetPoint updates point" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0)])
            let pl = Polyline3D(pts)
            pl.SetPnt (1, Pnt(2,2,2))
            "point updated" |> Expect.isTrue (eqPnt pl.AsPoints.[1] (Pnt(2,2,2)))
        }

        test "SetPntXYZClosed on closed polyline updates both first and last" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0); Pnt(1,1,0); Pnt(0,0,0)])
            let pl = Polyline3D(pts)
            pl.SetPntXYZClosed (0, 5., 5., 5.)
            "first point updated" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(5,5,5)))
            "last point also updated" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(5,5,5)))
        }

        test "point getters and setters throw on a pointIndex out of range" {
            let pl = Polyline3D.createFromPnts [Pnt(3.,4.,5.); Pnt(6.,4.,5.); Pnt(6.,8.,5.)]
            for i in [ -1; pl.PointCount ] do
                Expect.throws (fun () -> pl.GetX i |> ignore) $"GetX {i}"
                Expect.throws (fun () -> pl.GetY i |> ignore) $"GetY {i}"
                Expect.throws (fun () -> pl.GetZ i |> ignore) $"GetZ {i}"
                Expect.throws (fun () -> pl.GetPnt i |> ignore) $"GetPnt {i}"
                Expect.throws (fun () -> pl.SetPnt (i, Pnt(0.,0.,0.))) $"SetPnt {i}"
                Expect.throws (fun () -> pl.SetPntXYZ (i, 0., 0., 0.)) $"SetPntXYZ {i}"
                Expect.throws (fun () -> pl.SetPntXYZClosed (i, 0., 0., 0.)) $"SetPntXYZClosed {i}"
            "point count unchanged" |> Expect.equal pl.PointCount 3
        }

        test "MovePnt moves only the point at the index" {
            let pl = movePntQuad ()
            let orig = pl.AsPoints
            pl.MovePnt (2, movePntVec)
            "moved point" |> Expect.isTrue (eqPnt (pl.GetPnt 2) (orig.[2] + movePntVec))
            for i in [0; 1; 3; 4] do
                expectSamePnt (pl.GetPnt i) orig.[i] $"point {i} unchanged"
        }

        test "MovePntXYZ gives the same coordinates as MovePnt" {
            let a = movePntQuad ()
            let b = movePntQuad ()
            a.MovePnt (1, movePntVec)
            b.MovePntXYZ (1, movePntVec.X, movePntVec.Y, movePntVec.Z)
            "same coordinates" |> Expect.isTrue (Seq.forall2 (=) a.XYZs b.XYZs)
        }

        test "static movePnt and movePntXYZ take the point index first" {
            let pl = movePntQuad ()
            let orig = pl.AsPoints
            Polyline3D.movePnt 3 movePntVec pl
            "movePnt moved point 3" |> Expect.isTrue (eqPnt (pl.GetPnt 3) (orig.[3] + movePntVec))
            Polyline3D.movePntXYZ 1 0.5 (-2.0) 1.25 pl
            "movePntXYZ moved point 1" |> Expect.isTrue (eqPnt (pl.GetPnt 1) (Pnt(18.25, 7.5, 5.5)))
        }

        test "MovePnt on the first point opens a closed polyline" {
            let pl = movePntQuad ()
            pl.MovePnt (0, movePntVec)
            "polyline is open" |> Expect.isFalse pl.IsClosed
            expectSamePnt pl.LastPoint (Pnt(12.5, 7.25, 3.5)) "last point unchanged"
        }

        test "MovePntClosed on the first point moves the last point too" {
            let pl = movePntQuad ()
            let orig = pl.AsPoints
            pl.MovePntClosed (0, movePntVec)
            "polyline still closed" |> Expect.isTrue pl.IsClosed
            "first point moved" |> Expect.isTrue (eqPnt pl.FirstPoint (orig.[0] + movePntVec))
            for i in 1 .. 3 do
                expectSamePnt (pl.GetPnt i) orig.[i] $"point {i} unchanged"
        }

        test "MovePntClosed on the last point moves the first point too" {
            let pl = movePntQuad ()
            let orig = pl.AsPoints
            pl.MovePntClosed (4, movePntVec)
            "polyline still closed" |> Expect.isTrue pl.IsClosed
            "last point moved" |> Expect.isTrue (eqPnt pl.LastPoint (orig.[4] + movePntVec))
            for i in 1 .. 3 do
                expectSamePnt (pl.GetPnt i) orig.[i] $"point {i} unchanged"
        }

        test "MovePntXYZClosed keeps a closed polyline closed" {
            let pl = movePntQuad ()
            pl.MovePntXYZClosed (0, movePntVec.X, movePntVec.Y, movePntVec.Z)
            pl.MovePntXYZClosed (4, -0.7, 0.9, -1.1)
            "polyline still closed" |> Expect.isTrue pl.IsClosed
            "first point moved twice" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(12.5 + 0.3 - 0.7, 7.25 - 1.7 + 0.9, 3.5 + 2.9 - 1.1)))
        }

        test "static movePntClosed and movePntXYZClosed keep a closed polyline closed" {
            let pl = movePntQuad ()
            Polyline3D.movePntClosed 4 movePntVec pl
            Polyline3D.movePntXYZClosed 0 0.5 (-2.0) 1.25 pl
            "polyline still closed" |> Expect.isTrue pl.IsClosed
        }

        test "MovePntClosed by a tiny vector keeps the ends bit-identical" {
            let pl = movePntQuad ()
            pl.MovePntClosed (0, Vec(1e-13, -3e-14, 2e-14))
            "first point moved" |> Expect.isFalse (pl.FirstPoint.X = 12.5)
            "polyline still closed" |> Expect.isTrue pl.IsClosed
        }

        test "MovePntClosed on an open polyline moves only the point" {
            let pl = Polyline3D.createFromPnts [Pnt(12.5, 7.25, 3.5); Pnt(17.75, 9.5, 4.25); Pnt(15.25, 14.75, 6.0); Pnt(10.5, 12.25, 5.25)]
            pl.MovePntClosed (0, movePntVec)
            "first point moved" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(12.5, 7.25, 3.5) + movePntVec))
            expectSamePnt pl.LastPoint (Pnt(10.5, 12.25, 5.25)) "last point unchanged"
        }

        test "moving by a zero vector leaves the polyline unchanged" {
            let pl = movePntQuad ()
            let orig = movePntQuad ()
            pl.MovePnt (1, Vec(0., 0., 0.))
            pl.MovePntClosed (0, Vec(0., 0., 0.))
            pl.MovePntXYZ (2, 0., 0., 0.)
            pl.MovePntXYZClosed (4, 0., 0., 0.)
            "same coordinates" |> Expect.isTrue (Seq.forall2 (=) pl.XYZs orig.XYZs)
        }

        test "moving the point of a single point polyline" {
            let pl = Polyline3D.createFromPnts [Pnt(-3.5, 8.25, -1.75)]
            pl.MovePnt (0, movePntVec)
            pl.MovePntClosed (0, movePntVec)
            "point moved twice" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(-3.5, 8.25, -1.75) + movePntVec + movePntVec))
            "still one point" |> Expect.equal pl.PointCount 1
        }

        test "MovePnt throws on a pointIndex out of range" {
            let pl = movePntQuad ()
            for i in [ -1; pl.PointCount ] do
                Expect.throws (fun () -> pl.MovePnt (i, movePntVec)) $"MovePnt {i}"
                Expect.throws (fun () -> pl.MovePntXYZ (i, 1., 1., 1.)) $"MovePntXYZ {i}"
                Expect.throws (fun () -> pl.MovePntClosed (i, movePntVec)) $"MovePntClosed {i}"
                Expect.throws (fun () -> pl.MovePntXYZClosed (i, 1., 1., 1.)) $"MovePntXYZClosed {i}"
            "same coordinates" |> Expect.isTrue (Seq.forall2 (=) pl.XYZs (movePntQuad ()).XYZs)
            let empty = Polyline3D.createEmpty 4
            Expect.throws (fun () -> empty.MovePnt (0, movePntVec)) "empty polyline"
        }



        test "ToString works for empty polyline" {
            let pl = Polyline3D.createEmpty(10)
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
            let pl = Polyline3D.createFromPnts [Pnt(0.,0.,7.); Pnt(10.,0.,7.); Pnt(10.,10.,7.); Pnt(0.,10.,7.); Pnt(0.,0.,7.)]
            "projected area is not doubled" |> Expect.floatClose tol pl.SignedAreaIn2D 100.0
        }

        test "static setPntXYZ takes the point index before coordinates" {
            let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]
            Polyline3D.setPntXYZ 1 2. 3. 4. pl
            "point updated" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(2.,3.,4.)))
        }

        test "static setPntXYZClosed takes the index first and the polyline last" {
            let pl = Polyline3D.createFromPnts [Pnt(10.,20.,30.); Pnt(15.,20.,30.); Pnt(15.,26.,31.); Pnt(10.,20.,30.)]
            pl |> Polyline3D.setPntXYZClosed 0 107. (-33.) 12.
            "first point set" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(107.,-33.,12.)))
            "last point set too" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(107.,-33.,12.)))
            "middle point unchanged" |> Expect.isTrue (eqPnt (pl.GetPnt 1) (Pnt(15.,20.,30.)))
        }

        test "static point queries take the point first and the polyline last" {
            // rotated and moved off the origin, so a swapped coordinate shows up
            let m = RigidMatrix.createRotationAxis(Vec(1., 2., 3.), 40.) *** RigidMatrix.createTranslation(100., -40., 25.)
            let place (p:Pnt) = p *** m
            let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.)] |> Polyline3D.transformRigid m
            let q = place (Pnt(5.,-1.,2.)) // closest to the middle of the first segment
            "closestParameter" |> Expect.floatClose tol (pl |> Polyline3D.closestParameter q) 0.5
            "closestParameterXYZ" |> Expect.floatClose tol (pl |> Polyline3D.closestParameterXYZ q.X q.Y q.Z) 0.5
            "closestPoint" |> Expect.isTrue (eqPnt (pl |> Polyline3D.closestPoint q) (place (Pnt(5.,0.,0.))))
            "closestPointIndex" |> Expect.equal (pl |> Polyline3D.closestPointIndex (place (Pnt(9.,0.5,1.)))) 1
            "distanceTo" |> Expect.floatClose tol (pl |> Polyline3D.distanceTo q) (sqrt 5.)
            "distanceToXYZ" |> Expect.floatClose tol (pl |> Polyline3D.distanceToXYZ q.X q.Y q.Z) (sqrt 5.)
        }

        test "Close with zero tolerance snaps already matching ends" {
            let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(0.,0.,0.)]
            pl.Close(0.0)
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
            let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(0.009,0.009,0.009); Pnt(1.,0.,0.); Pnt(0.,0.,0.)]
            let cleaned = Polyline3D.removeDuplicatePoints 0.01 pl
            "diagonal point is outside Euclidean tolerance and kept" |> Expect.equal cleaned.PointCount 4
            "closure preserved" |> Expect.isTrue cleaned.IsClosed
        }

        test "removeDuplicatePointsAndReport returns an empty list and an unchanged copy" {
            let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(2.,0.,0.)]
            let cleaned, duplicates = Polyline3D.removeDuplicatePointsAndReport 0.01 pl
            Expect.equal duplicates.Count 0 "no duplicate points"
            Expect.isTrue (Polyline3D.equals cleaned pl) "copy has the same points"
            Expect.isFalse (obj.ReferenceEquals(cleaned.XYZs, pl.XYZs)) "copy has its own coordinate list"
        }

        test "removeDuplicatePointsAndReport lists identical consecutive duplicates once" {
            let duplicate = Pnt(0.005,0.,0.)
            let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); duplicate; duplicate; Pnt(1.,0.,0.)]
            let duplicates = pl.RemoveDuplicatePointsAndReport 0.01
            Expect.equal pl.PointCount 2 "duplicate points removed in place"
            Expect.equal duplicates.Count 1 "each removed point listed once"
            Expect.isTrue (eqPnt duplicates.[0] duplicate) "removed point returned"
        }

        test "removeDuplicatePointsAndReport reports matching duplicates from separate runs" {
            let duplicate = Pnt(0.005,0.,0.)
            let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); duplicate; Pnt(1.,0.,0.); Pnt(0.,0.,0.); duplicate; Pnt(2.,0.,0.)]
            let cleaned, duplicates = Polyline3D.removeDuplicatePointsAndReport 0.01 pl
            Expect.equal cleaned.PointCount 4 "only consecutive duplicate points removed"
            Expect.equal pl.PointCount 6 "input should be unchanged"
            Expect.equal duplicates.Count 2 "same duplicate reported for each separate run"
            Expect.isTrue (eqPnt duplicates.[0] duplicate) "first occurrence returned"
            Expect.isTrue (eqPnt duplicates.[1] duplicate) "later occurrence returned"
        }

        test "GetSegment out of range throws" {
            let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
            let pl = Polyline3D(pts)
            let f() = pl.GetSegment(5) |> ignore
            "GetSegment out of range throws" |> Expect.throws f
        }

        test "FirstPoint on empty polyline throws" {
            let pl = Polyline3D.createEmpty(10)
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
                let moved = pl.Duplicate()
                moved.Move(Vec(5., 3., 2.))
                "Move - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(5., 3., 2.)))
                "Move - last point" |> Expect.isTrue (eqPnt moved.LastPoint (Pnt(6., 4., 2.)))
            }

            test "MoveX instance method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = pl.Duplicate()
                moved.MoveX(5.)
                "MoveX - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(5., 0., 0.)))
            }

            test "MoveY instance method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = pl.Duplicate()
                moved.MoveY(3.)
                "MoveY - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(0., 3., 0.)))
            }

            test "MoveZ instance method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = pl.Duplicate()
                moved.MoveZ(2.)
                "MoveZ - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(0., 0., 2.)))
            }

            test "move static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = Polyline3D.move (Vec(5., 3., 2.)) pl
                "move static - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(5., 3., 2.)))
                "move static - input unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0., 0., 0.)))
            }

            test "translate static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let moved = Polyline3D.translate (Vec(5., 3., 2.)) pl
                "translate static - first point" |> Expect.isTrue (eqPnt moved.FirstPoint (Pnt(5., 3., 2.)))
                "translate static - input unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0., 0., 0.)))
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
                "input unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0., 0., 0.)))
            }

            test "Transform with identity matrix" {
                let pts = ResizeArray([Pnt(1,2,3); Pnt(4,5,6)])
                let pl = Polyline3D(pts)
                let transformed = pl.Duplicate()
                transformed.Transform(Matrix.identity)
                "Transform identity - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint pl.FirstPoint)
            }

            test "Transform with translation matrix" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                let transformed = pl.Duplicate()
                transformed.Transform(m)
                "Transform translation - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint (Pnt(5., 3., 2.)))
            }

            test "transform static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let m = Matrix.createTranslation(Vec(5., 3., 2.))
                let transformed = Polyline3D.transform m pl
                "transform static - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint (Pnt(5., 3., 2.)))
                "transform static - input unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0., 0., 0.)))
            }

            test "TransformRigid instance method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                let transformed = pl.Duplicate()
                transformed.TransformRigid(m)
                "TransformRigid - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint (Pnt(5., 3., 2.)))
            }

            test "transformRigid static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(1,0,0)])
                let pl = Polyline3D(pts)
                let m = RigidMatrix.createTranslation(Vec(5., 3., 2.))
                let transformed = Polyline3D.transformRigid m pl
                "transformRigid static - first point" |> Expect.isTrue (eqPnt transformed.FirstPoint (Pnt(5., 3., 2.)))
                "transformRigid static - input unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0., 0., 0.)))
            }

            test "Rotate with identity quaternion" {
                let pts = ResizeArray([Pnt(1,2,3); Pnt(4,5,6)])
                let pl = Polyline3D(pts)
                let rotated = pl.Duplicate()
                rotated.Rotate(Quaternion.identity)
                "Rotate identity - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint pl.FirstPoint)
            }

            test "Rotate 90 degrees around Z axis" {
                let pts = ResizeArray([Pnt(1,0,0); Pnt(2,0,0)])
                let pl = Polyline3D(pts)
                let q = Quaternion.createFromDegrees(UnitVec.Zaxis, 90.)
                let rotated = pl.Duplicate()
                rotated.Rotate(q)
                "Rotate 90 - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint (Pnt(0., 1., 0.)))
            }

            test "rotate static method" {
                let pts = ResizeArray([Pnt(1,0,0); Pnt(2,0,0)])
                let pl = Polyline3D(pts)
                let q = Quaternion.createFromDegrees(UnitVec.Zaxis, 90.)
                let rotated = Polyline3D.rotate q pl
                "rotate - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint (Pnt(0., 1., 0.)))
                "rotate - input unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(1., 0., 0.)))
            }

            test "RotateAround keeps center fixed" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(2,0,0); Pnt(2,2,0); Pnt(0,2,0)])
                let pl = Polyline3D(pts)
                let center = pl.Center
                let q = Quaternion.createFromDegrees(UnitVec.Zaxis, 90.)
                let rotated = pl.Duplicate()
                rotated.RotateAround center q
                "RotateAround - center" |> Expect.isTrue (eqPnt rotated.Center center)
            }

            test "rotateAround static method" {
                let pts = ResizeArray([Pnt(0,0,0); Pnt(2,0,0); Pnt(2,2,0); Pnt(0,2,0)])
                let pl = Polyline3D(pts)
                let center = pl.Center
                let q = Quaternion.createFromDegrees(UnitVec.Zaxis, 90.)
                let rotated = Polyline3D.rotateAround center q pl
                "rotateAround - center" |> Expect.isTrue (eqPnt rotated.Center center)
                "rotateAround - input unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(0., 0., 0.)))
            }

            test "Scale instance method" {
                let pts = ResizeArray([Pnt(1,2,3); Pnt(2,4,6)])
                let pl = Polyline3D(pts)
                let scaled = pl.Duplicate()
                scaled.Scale(2.)
                "Scale - first point" |> Expect.isTrue (eqPnt scaled.FirstPoint (Pnt(2., 4., 6.)))
                "Scale - last point" |> Expect.isTrue (eqPnt scaled.LastPoint (Pnt(4., 8., 12.)))
            }

            test "scale static method" {
                let pts = ResizeArray([Pnt(1,2,3); Pnt(2,4,6)])
                let pl = Polyline3D(pts)
                let scaled = Polyline3D.scale 2. pl
                "scale static - first point" |> Expect.isTrue (eqPnt scaled.FirstPoint (Pnt(2., 4., 6.)))
                "scale static - input unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(1., 2., 3.)))
            }

            test "rotateOnZ static method" {
                let pts = ResizeArray([Pnt(1,0,0); Pnt(2,0,0)])
                let pl = Polyline3D(pts)
                let r = Rotation2D.createFromDegrees 90.
                let rotated = Polyline3D.rotateOnZ r pl
                "rotateOnZ - first point" |> Expect.isTrue (eqPnt rotated.FirstPoint (Pnt(0., 1., 0.)))
                "rotateOnZ - input unchanged" |> Expect.isTrue (eqPnt pl.FirstPoint (Pnt(1., 0., 0.)))
            }
        ]

        testList "Edge case regressions" [
            test "SegmentVectorsXYZ on empty polyline returns empty list" {
                let pl = Polyline3D.createEmpty(10)
                Expect.equal pl.SegmentVectorsXYZ.Count 0 "empty polyline has no segment vectors"
            }
            test "SegmentVectorsXYZ on single point returns empty list" {
                let pl = Polyline3D.createFromPnts [Pnt(1.,1.,1.)]
                Expect.equal pl.SegmentVectorsXYZ.Count 0 "single point has no segment vectors"
            }
            test "close fails with less than 3 points" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]
                Expect.throws (fun () -> Polyline3D.close 1e-6 pl |> ignore) "two point close should fail"
            }
            test "close with 0.0 tolerance on exactly closed polyline adds no point" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,0.,0.)]
                let closed = Polyline3D.close 0.0 pl
                Expect.equal closed.PointCount pl.PointCount "no extra point added"
                Expect.isTrue closed.IsClosed "still closed"
            }
            test "IsAlmostClosed with 0.0 tolerance on exactly closed polyline" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,0.,0.)]
                Expect.isTrue (pl.IsAlmostClosed 0.0) "exactly closed is almost closed with 0.0 tolerance"
                let plOpen = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                Expect.isFalse (plOpen.IsAlmostClosed 0.0) "open is not almost closed with 0.0 tolerance"
            }
            test "removeDuplicatePoints keeps first point when all points collapse" {
                let pl = Polyline3D.createFromPnts [Pnt(1.,1.,1.); Pnt(1.0000001,1.,1.)]
                let result = Polyline3D.removeDuplicatePoints 1e-6 pl
                Expect.equal result.PointCount 1 "collapses to one point"
                "kept point is the first point" |> Expect.isTrue (eqPnt (result.GetPnt 0) (Pnt(1.,1.,1.)))
                Expect.equal pl.PointCount 2 "input should be unchanged"
            }
            test "removeDuplicateAndCollinearPoints removes collinear point" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(2.,0.,0.); Pnt(2.,2.,0.)]
                let result = Polyline3D.removeDuplicateAndCollinearPoints Cosine.``0.1`` 1e-6 pl
                Expect.equal result.PointCount 3 "middle collinear point removed"
                Expect.equal pl.PointCount 4 "input should be unchanged"
            }
            test "removeDuplicateAndCollinearPoints fails when all points are duplicates" {
                let pl = Polyline3D.createFromPnts [Pnt(1.,1.,1.); Pnt(1.,1.,1.); Pnt(1.,1.,1.)]
                Expect.throws (fun () -> Polyline3D.removeDuplicateAndCollinearPoints Cosine.``0.1`` 1e-6 pl |> ignore) "all duplicates should fail with a clear error"
            }
            test "removeDuplicateAndCollinearPoints square" {
                let pl = Polyline3D.createFromPnts [ Pnt(2.,0.,0.); Pnt(4.,0.,0.); Pnt(4.,2.,0.); Pnt(4.,4.,0.); Pnt(2.,4.,0.); Pnt(0.,4.,0.); Pnt(0.,2.,0.); Pnt(0.,0.,0.); Pnt(2.,0.,0.)]
                let simplified = Polyline3D.removeDuplicateAndCollinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.equal simplified.PointCount 5 "collinear edge midpoints removed, corners kept, stays closed"
                Expect.isTrue simplified.IsClosed "remains closed"
            }
            test "removeDuplicateAndCollinearPoints open line with repeats" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(2.,0.,0.); Pnt(3.,0.,0.); Pnt(3.,0.,0.)]
                let simplified = pl.Duplicate()
                simplified.RemoveDuplicateAndCollinearPoints Cosine.``0.1`` 1e-9
                Expect.equal simplified.PointCount 2 "duplicate end removed; interior collinear points removed"
                "start" |> Expect.isTrue (eqPnt (simplified.GetPnt 0) (Pnt(0.,0.,0.)))
                "end" |> Expect.isTrue (eqPnt (simplified.GetPnt (simplified.PointCount - 1)) (Pnt(3.,0.,0.)))
            }
            test "removeDuplicateAndCollinearPoints collinear start/end segments closed polygon" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(2.,0.,0.); Pnt(2.,2.,0.); Pnt(2.,4.,0.); Pnt(1.,4.,0.); Pnt(0.,4.,0.); Pnt(0.,2.,0.); Pnt(0.,0.,0.)]
                let simplified = Polyline3D.removeDuplicateAndCollinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.isTrue simplified.IsClosed "closed retained"
                Expect.equal simplified.PointCount 5 "collinear removal at this tolerance"
            }
            test "removeDuplicateAndCollinearPoints collinear in 3D space" {
                // collinear points along a diagonal in 3D
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,1.,1.); Pnt(2.,2.,2.); Pnt(3.,3.,0.)]
                let simplified = Polyline3D.removeDuplicateAndCollinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.equal simplified.PointCount 3 "middle collinear point on 3D diagonal removed"
            }
            test "removeDuplicateAndCollinearPoints almost collinear tiny angle retained" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(20.,0.2,0.); Pnt(30.,0.,0.)]
                let simplified = Polyline3D.removeDuplicateAndCollinearPoints Cosine.``0.01`` 1e-9 pl
                Expect.equal simplified.PointCount 4 "almost straight kept"
            }
            test "removeDuplicateAndCollinearPoints single point returns same" {
                let pl = Polyline3D.createFromPnts [Pnt(1.,2.,3.)]
                let result = Polyline3D.removeDuplicateAndCollinearPoints Cosine.``0.1`` 1e-9 pl
                Expect.equal result.PointCount 1 "single point unchanged"
            }
            test "removeDuplicateAndCollinearPoints tolerance too tight throws" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(2.,0.,0.)]
                let tooTight = UtilEuclid.withMeasure 0.9999999999 // cosine of ~0.0 degrees, above Cosine.``0.01``
                Expect.throws
                    (fun () -> Polyline3D.removeDuplicateAndCollinearPoints tooTight 1e-9 pl |> ignore)
                    "tolerance tighter than 0.01 degrees should throw"
            }
        ]

        testList "Coverage for untested members" [
            test "equals on clones and on different counts" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                Expect.isTrue (Polyline3D.equalsTol  1e-9 pl (pl.Clone())) "clones are equal"
                let shorter = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]
                Expect.isFalse (Polyline3D.equalsTol  1e-9 pl shorter) "different counts are not equal"
            }
            test "equals uses  distance  per-axis" {
                let a = Polyline3D.createFromPnts [Pnt(0.,0.,0.);Pnt(0.,0.,0.)]
                let b = Polyline3D.createFromPnts [Pnt(0.,0.,0.);Pnt(0.008, 0.008, 0.008)] // each axis within 0.001, Euclidean distance ~0.001386
                Expect.isFalse (Polyline3D.equalsTol  0.007 a b) "outside Euclidean tolerance"
                Expect.isTrue (Polyline3D.equalsTol   0.009 a b) "inside Euclidean tolerance"
            }
            test "EvaluateAt start, mid-segment and out of range" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                "at 0.0 is start" |> Expect.isTrue (eqPnt (pl.EvaluateAt 0.0) (Pnt(0.,0.,0.)))
                "at 1.5 is middle of second segment" |> Expect.isTrue (eqPnt (pl.EvaluateAt 1.5) (Pnt(1.,0.5,0.)))
                Expect.throws (fun () -> pl.EvaluateAt -1.0 |> ignore) "negative parameter throws"
                Expect.throws (fun () -> pl.EvaluateAt 5.0 |> ignore) "parameter beyond end throws"
            }
            test "ClosestPoint, ClosestParameter and DistanceTo" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                let testPt = Pnt(0.5, -1., 0.)
                "closest point on first segment" |> Expect.isTrue (eqPnt (pl.ClosestPoint testPt) (Pnt(0.5, 0., 0.)))
                "closest parameter is 0.5" |> Expect.floatClose tol (pl.ClosestParameter testPt) 0.5
                "distance is 1.0" |> Expect.floatClose tol (pl.DistanceTo testPt) 1.0
                "closest point index is 1" |> Expect.equal (pl.ClosestPointIndex (Pnt(1.1, 0.1, 0.))) 1
            }
            test "subPolyline forward and reversed" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                let sub = Polyline3D.subPolyline 0.5 1.5 pl
                Expect.equal sub.PointCount 3 "sub polyline has 3 points"
                "sub starts at mid first segment" |> Expect.isTrue (eqPnt sub.FirstPoint (Pnt(0.5, 0., 0.)))
                "sub ends at mid second segment" |> Expect.isTrue (eqPnt sub.LastPoint (Pnt(1., 0.5, 0.)))
                let rev = Polyline3D.subPolyline 1.5 0.5 pl
                "reversed sub starts at mid second segment" |> Expect.isTrue (eqPnt rev.FirstPoint (Pnt(1., 0.5, 0.)))
            }
            test "AverageNormal of counterclockwise square points up" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,1.,0.)]
                let n = pl.AverageNormal
                Expect.isTrue (n.Z > 0.0) "normal points up for CCW square in XY plane"
                Expect.floatClose tol n.X 0.0 "normal X is zero"
                Expect.floatClose tol n.Y 0.0 "normal Y is zero"
            }

            test "ToString distinguishes open and closed" {
                let plOpen3 = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                let plClosed3 = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,0.,0.)]
                Expect.stringContains (plOpen3.ToString()) "open" "open polyline string says open"
                Expect.stringContains (plClosed3.ToString()) "closed" "closed polyline string says closed"
                Expect.stringContains (Polyline3D.createEmpty(10).ToString()) "empty" "empty polyline string says empty"
            }

            test "static addXYZ is curried" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.)]
                Polyline3D.addXYZ 1. 2. 3. pl
                Expect.equal pl.PointCount 2 "point added"
                "added point" |> Expect.isTrue (eqPnt pl.LastPoint (Pnt(1.,2.,3.)))
            }
            test "tryFind point helpers" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(5.,1.,2.); Pnt(10.,1.,2.); Pnt(5.,3.,4.)]

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
                "no index match on empty polyline" |> Expect.equal (Polyline3D.tryFindIndex (fun _ _ _ -> true) (Polyline3D([]))) None
            }
            test "iter visits every point" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Pnt>()
                pl |> Polyline3D.iter (fun x y z -> acc.Add(Pnt(x,y,z)))
                "iter count" |> Expect.equal acc.Count 3
                "iter first" |> Expect.isTrue (eqPnt acc.[0] (Pnt(0.,0.,0.)))
                "iter last" |> Expect.isTrue (eqPnt acc.[2] (Pnt(4.,5.,6.)))
            }
            test "iteri passes the flat coordinate index" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let idxs = ResizeArray<int>()
                pl |> Polyline3D.iteri (fun i _ _ _ -> idxs.Add i)
                "iteri flat indices (step 3)" |> Expect.equal (List.ofSeq idxs) [0; 3; 6]
            }
            test "iterPnt visits every point" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Pnt>()
                pl |> Polyline3D.iterPnt (fun pt -> acc.Add pt)
                "iterPnt count" |> Expect.equal acc.Count 3
                "iterPnt first" |> Expect.isTrue (eqPnt acc.[0] (Pnt(0.,0.,0.)))
                "iterPnt last" |> Expect.isTrue (eqPnt acc.[2] (Pnt(4.,5.,6.)))
            }
            test "iteriPnt passes the point position" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let idxs = ResizeArray<int>()
                let acc = ResizeArray<Pnt>()
                pl |> Polyline3D.iteriPnt (fun i pt -> idxs.Add i; acc.Add pt)
                "iteriPnt point positions" |> Expect.equal (List.ofSeq idxs) [0; 1; 2]
                "iteriPnt second point" |> Expect.isTrue (eqPnt acc.[1] (Pnt(1.,2.,3.)))
            }
            // test "iterSkipLast skips the last point" {
            //     let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
            //     let acc = ResizeArray<Pnt>()
            //     pl |> Polyline3D.iterSkipLast (fun x y z -> acc.Add(Pnt(x,y,z)))
            //     "iterSkipLast count" |> Expect.equal acc.Count 2
            //     "iterSkipLast first" |> Expect.isTrue (eqPnt acc.[0] (Pnt(0.,0.,0.)))
            //     "iterSkipLast last" |> Expect.isTrue (eqPnt acc.[1] (Pnt(1.,2.,3.)))
            // }
            // test "iterPtSkipLast skips the last point" {
            //     let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
            //     let acc = ResizeArray<Pnt>()
            //     pl |> Polyline3D.iterPntSkipLast (fun pt -> acc.Add pt)
            //     "iterPtSkipLast count" |> Expect.equal acc.Count 2
            //     "iterPtSkipLast first" |> Expect.isTrue (eqPnt acc.[0] (Pnt(0.,0.,0.)))
            //     "iterPtSkipLast last" |> Expect.isTrue (eqPnt acc.[1] (Pnt(1.,2.,3.)))
            // }
            test "iterSegments visits consecutive point pairs" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Pnt*Pnt>()
                pl |> Polyline3D.iterSegments (fun x1 y1 z1 x2 y2 z2 -> acc.Add(Pnt(x1,y1,z1), Pnt(x2,y2,z2)))
                "iterSegments count" |> Expect.equal acc.Count 2
                "first segment start" |> Expect.isTrue (eqPnt (fst acc.[0]) (Pnt(0.,0.,0.)))
                "first segment end"   |> Expect.isTrue (eqPnt (snd acc.[0]) (Pnt(1.,2.,3.)))
                "second segment end"  |> Expect.isTrue (eqPnt (snd acc.[1]) (Pnt(4.,5.,6.)))
                // fewer than two points yields no segments
                let single = Polyline3D.createFromPnts [Pnt(0.,0.,0.)]
                let mutable called = false
                single |> Polyline3D.iterSegments (fun _ _ _ _ _ _ -> called <- true)
                "no segment on single point" |> Expect.isFalse called
            }
            test "iterLineSegments yields Line3D segments" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,2.,3.); Pnt(4.,5.,6.)]
                let acc = ResizeArray<Line3D>()
                pl |> Polyline3D.iterLineSegments (fun ln -> acc.Add ln)
                "iterLineSegments count" |> Expect.equal acc.Count 2
                "first line from" |> Expect.isTrue (eqPnt acc.[0].From (Pnt(0.,0.,0.)))
                "first line to"   |> Expect.isTrue (eqPnt acc.[0].To (Pnt(1.,2.,3.)))
                "second line to"  |> Expect.isTrue (eqPnt acc.[1].To (Pnt(4.,5.,6.)))
            }
            test "offset' takes inPlane distance first then perpendicular" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(0.,0.,0.)]
                let viaTupled = Polyline3D.offset(pl, 1.0, 0.5)
                let viaCurried = Polyline3D.offset' 1.0 0.5 pl
                Expect.isTrue (Polyline3D.equalsTol  1e-9 viaTupled viaCurried) "offset' argument order matches offset"
            }
        ]

        testList "AreaIn2D and orientation helpers" [
            test "AreaIn2D is the absolute value of SignedAreaIn2D" {
                let ccw = Polyline3D.createFromPnts [Pnt(0.,0.,7.); Pnt(10.,0.,7.); Pnt(10.,10.,7.); Pnt(0.,10.,7.); Pnt(0.,0.,7.)]
                let cw  = Polyline3D.createFromPnts [Pnt(0.,0.,7.); Pnt(0.,10.,7.); Pnt(10.,10.,7.); Pnt(10.,0.,7.); Pnt(0.,0.,7.)]
                "CCW area" |> Expect.floatClose tol ccw.AreaIn2D 100.0
                "CW area is still positive" |> Expect.floatClose tol cw.AreaIn2D 100.0
                "static areaIn2D matches" |> Expect.floatClose tol (Polyline3D.areaIn2D cw) 100.0
                "signed area is negative for CW" |> Expect.isTrue (cw.SignedAreaIn2D < 0.0)
            }
            test "ensureCounterClockwiseIn2D keeps a CCW polyline unchanged" {
                let ccw = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(0.,0.,0.)]
                let res = Polyline3D.ensureCounterClockwiseIn2D ccw
                Expect.isTrue res.IsCounterClockwiseIn2D "still CCW"
                Expect.isTrue (eqPnt res.SecondPoint (Pnt(10.,0.,0.))) "point order unchanged"
                Expect.isFalse (obj.ReferenceEquals(res, ccw)) "returns a new Polyline3D even if unchanged"
            }
            test "ensureClockwiseIn2D returns a reversed copy of a CCW polyline" {
                let ccw = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(0.,0.,0.)]
                let cw = Polyline3D.ensureClockwiseIn2D ccw
                Expect.isTrue cw.IsClockwiseIn2D "result is CW"
                Expect.isTrue ccw.IsCounterClockwiseIn2D "input is still CCW"
            }
            test "EnsureClockwiseIn2D reverses a CCW polyline in place" {
                let ccw = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(0.,0.,0.)]
                ccw.EnsureClockwiseIn2D()
                Expect.isTrue ccw.IsClockwiseIn2D "original is now CW"
            }
            test "EnsureCounterClockwiseIn2D reverses a CW polyline in place" {
                let cw = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(0.,10.,0.); Pnt(10.,10.,0.); Pnt(10.,0.,0.); Pnt(0.,0.,0.)]
                cw.EnsureCounterClockwiseIn2D()
                Expect.isTrue cw.IsCounterClockwiseIn2D "original is now CCW"
            }
        ]

        testList "removeUTurns and removeUTurnsDeeply" [
            test "simple open U-turn point removed (axis aligned)" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(5.,0.,0.); Pnt(5.,5.,0.); Pnt(5.,1.,0.); Pnt(9.,1.,0.)]
                let simplified = Polyline3D.removeUTurns Cosine.``179.99`` pl
                Expect.equal simplified.PointCount 4 "should remove the U-turn point"
                "removed the U-turn vertex (5,5,0)" |> Expect.isTrue (eqPnt (simplified.GetPnt 2) (Pnt(5.,1.,0.)))
                Expect.equal pl.PointCount 5 "input should be unchanged"
            }
            test "U-turn in a non-axis-aligned (XZ) plane is detected" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(2.,0.,0.); Pnt(2.,0.,2.); Pnt(2.,0.,0.5); Pnt(5.,0.,0.5)]
                let simplified = pl.Duplicate()
                simplified.RemoveUTurns Cosine.``179.99``
                Expect.equal simplified.PointCount 4 "should remove the 3D U-turn point"
                "removed the U-turn vertex (2,0,2)" |> Expect.isTrue (eqPnt (simplified.GetPnt 2) (Pnt(2.,0.,0.5)))
            }
            test "no U-turns returns an equal polyline" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,1.,0.)]
                let r = Polyline3D.removeUTurns Cosine.``179.0`` pl
                Expect.isTrue (Polyline3D.equalsTol  1e-9 r pl) "unchanged when no U-turns"
                Expect.isFalse (obj.ReferenceEquals(r.XYZs, pl.XYZs)) "returns a copy even without U-turns"
            }
            test "U-turn at seam keeps closed polyline closed" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(5.,0.001,0.); Pnt(0.,0.,0.)]
                Expect.isTrue pl.IsClosed "input should be closed"
                let simplified = Polyline3D.removeUTurns Cosine.``179.9`` pl
                Expect.isTrue simplified.IsClosed "closed polyline must stay closed after removing seam U-turn"
            }
            test "removeUTurnsDeeply on a simple U matches a single pass" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(5.,0.,0.); Pnt(5.,5.,0.); Pnt(5.,1.,0.); Pnt(9.,1.,0.)]
                let deep = Polyline3D.removeUTurnsDeeply Cosine.``179.99`` pl
                Expect.equal deep.PointCount 4 "single U is fully removed"
                Expect.equal pl.PointCount 5 "input should be unchanged"
            }
            test "removeUTurnsDeeply U-turn at seam keeps closed polyline closed" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(10.,0.,0.); Pnt(10.,10.,0.); Pnt(0.,10.,0.); Pnt(5.,0.001,0.); Pnt(0.,0.,0.)]
                let simplified = pl.Duplicate()
                simplified.RemoveUTurnsDeeply Cosine.``179.9``
                Expect.isTrue simplified.IsClosed "closed polyline must stay closed"
            }
        ]

        testList "removeDuplicatePointsFaithfully" [
            test "coplanar re-intersection matches the 2D result" {
                // same configuration as the Polyline2D test, embedded in the z=0 plane
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(0.993,0.,0.); Pnt(1.0,0.002,0.); Pnt(1.,1.,0.)]
                let cleaned = Polyline3D.removeDuplicatePointsFaithfully 0.01 pl
                Expect.equal cleaned.PointCount 3 "one point removed as duplicate within tolerance"
                Expect.equal pl.PointCount 4 "input should be unchanged"
                "edges re-intersected, not moved to the average" |> Expect.isTrue (eqPnt (cleaned.GetPnt 1) (Pnt(1.0,0.0,0.)))
                "start preserved" |> Expect.isTrue (eqPnt cleaned.Start pl.Start)
                "end preserved"   |> Expect.isTrue (eqPnt cleaned.End pl.End)
            }
            test "re-intersection works in a tilted (non-axis-aligned) plane" {
                // the same shape mapped by (x,y,0) -> (x,y,x); the two edges stay coplanar so the
                // closest approach is the true intersection. tolerance 0.02 keeps the lengthened cluster within tolerance.
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(0.993,0.,0.993); Pnt(1.0,0.002,1.0); Pnt(1.,1.,1.)]
                let cleaned = pl.Duplicate()
                cleaned.RemoveDuplicatePointsFaithfully 0.02
                Expect.equal cleaned.PointCount 3 "one point removed"
                "intersection computed in the tilted plane" |> Expect.isTrue (eqPnt (cleaned.GetPnt 1) (Pnt(1.0,0.0,1.0)))
                "start preserved" |> Expect.isTrue (eqPnt cleaned.Start pl.Start)
                "end preserved"   |> Expect.isTrue (eqPnt cleaned.End pl.End)
            }
            test "parallel segments fall back to the cluster midpoint" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.005,0.,0.); Pnt(2.,0.,0.)]
                let cleaned = Polyline3D.removeDuplicatePointsFaithfully 0.01 pl
                Expect.equal cleaned.PointCount 3 "one point removed"
                "no intersection possible, so the midpoint of the too-close cluster is used" |> Expect.isTrue (eqPnt (cleaned.GetPnt 1) (Pnt(1.0025,0.,0.)))
            }
            test "no change when no points are within tolerance" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.)]
                let cleaned = Polyline3D.removeDuplicatePointsFaithfully 0.01 pl
                Expect.equal cleaned.PointCount 3 "nothing removed"
            }
            test "too short polyline returned unchanged" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.)]
                let cleaned = Polyline3D.removeDuplicatePointsFaithfully 0.01 pl
                Expect.equal cleaned.PointCount 1 "single point returned as is"
            }
        ]

        testList "TryPlane and IsPlanar" [
            test "counterclockwise square in XY plane" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,1.,0.); Pnt(0.,0.,0.)]
                "square is planar" |> Expect.isTrue (pl.IsPlanar())
                match pl.TryPlane() with
                | Some pln ->
                    "normal points up for a CCW square in the XY plane" |> Expect.floatClose tol pln.Normal.Z 1.0
                    "origin is the average of all points" |> Expect.isTrue (eqPnt pln.Origin pl.Center)
                    "origin is on the XY plane" |> Expect.floatClose tol pln.Origin.Z 0.0
                | None ->
                    "square has a plane" |> Expect.isTrue false
            }

            test "clockwise square has a flipped normal" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(0.,1.,0.); Pnt(1.,1.,0.); Pnt(1.,0.,0.)]
                match pl.TryPlane() with
                | Some pln -> "normal points down for a CW square in the XY plane" |> Expect.floatClose tol pln.Normal.Z -1.0
                | None -> "square has a plane" |> Expect.isTrue false
            }

            test "triangle in a tilted plane" {
                let a = Pnt(1.,2.,3.)
                let b = Pnt(4.,2.,3.)
                let c = Pnt(1.,5.,7.)
                let pl = Polyline3D.createFromPnts [a; b; c]
                let expected = (NPlane.createFrom3Points a b c).Normal
                match pl.TryPlane() with
                | Some pln ->
                    "normal matches the plane through the three points" |> Expect.floatClose tol (UnitVec.dot(pln.Normal, expected)) 1.0
                    "all points are on the plane" |> Expect.isTrue (pl.AsPoints |> Seq.forall (fun p -> abs (NPlane.distanceToPntSigned p pln) < 1e-9))
                | None -> "triangle has a plane" |> Expect.isTrue false
            }

            test "non planar polyline returns None" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,1.); Pnt(0.,1.,0.)]
                "warped quad is not planar" |> Expect.isFalse (pl.IsPlanar())
                "warped quad has no plane" |> Expect.isTrue (pl.TryPlane() |> Option.isNone)
            }

            test "tolerance decides on an almost planar polyline" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.001); Pnt(0.,1.,0.)]
                "not planar within the default tolerance" |> Expect.isFalse (pl.IsPlanar())
                "planar within a 0.01 tolerance" |> Expect.isTrue (pl.IsPlanar(0.01))
                "static isPlanar is curried" |> Expect.isTrue (Polyline3D.isPlanar 0.01 pl)
                "static tryPlane is curried" |> Expect.isTrue (Polyline3D.tryPlane 0.01 pl |> Option.isSome)
                "static tryPlane with a tight tolerance" |> Expect.isTrue (Polyline3D.tryPlane 1e-6 pl |> Option.isNone)
            }

            test "less than three points has no plane" {
                "empty polyline" |> Expect.isFalse ((Polyline3D([])).IsPlanar())
                "one point" |> Expect.isFalse ((Polyline3D.createFromPnts [Pnt(1.,2.,3.)]).IsPlanar())
                "two points" |> Expect.isFalse ((Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]).IsPlanar())
                "two points have no plane" |> Expect.isTrue ((Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]).TryPlane() |> Option.isNone)
            }

            test "collinear points have no unique plane" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,1.,1.); Pnt(2.,2.,2.); Pnt(3.,3.,3.)]
                "collinear points are not planar" |> Expect.isFalse (pl.IsPlanar())
                "collinear points have no plane" |> Expect.isTrue (pl.TryPlane() |> Option.isNone)
            }

            test "almost collinear points still define a plane" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.001,0.); Pnt(2.,0.,0.)]
                "the tiny offset is bigger than the tolerance" |> Expect.isTrue (pl.IsPlanar())
                "within a 0.01 tolerance the points are on one line" |> Expect.isFalse (pl.IsPlanar(0.01))
            }

            test "duplicate points have no unique plane" {
                let pl = Polyline3D.createFromPnts [Pnt(5.,5.,5.); Pnt(5.,5.,5.); Pnt(5.,5.,5.); Pnt(5.,5.,5.)]
                "coincident points are not planar" |> Expect.isFalse (pl.IsPlanar())
                "coincident points have no plane" |> Expect.isTrue (pl.TryPlane() |> Option.isNone)
            }

            test "self intersecting planar polyline still finds the plane" {
                // a bow tie, the summed cross products of Newell's method cancel out to zero here
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,4.); Pnt(1.,0.,4.); Pnt(0.,1.,4.); Pnt(1.,1.,4.); Pnt(0.,0.,4.)]
                "bow tie is planar" |> Expect.isTrue (pl.IsPlanar())
                match pl.TryPlane() with
                | Some pln ->
                    "normal is vertical" |> Expect.floatClose tol (abs pln.Normal.Z) 1.0
                    "plane is at z=4" |> Expect.floatClose tol pln.Origin.Z 4.0
                | None -> "bow tie has a plane" |> Expect.isTrue false
            }

            test "very small polyline is still planar" {
                // the summed normal of Newell's method is shorter than 1e-6 here
                let s = 1e-4
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(s,0.,0.); Pnt(s,s,0.); Pnt(0.,s,0.)]
                "tiny square is planar" |> Expect.isTrue (pl.IsPlanar(1e-12))
                match pl.TryPlane(1e-12) with
                | Some pln -> "normal points up for a CCW tiny square" |> Expect.floatClose tol pln.Normal.Z 1.0
                | None -> "tiny square has a plane" |> Expect.isTrue false
            }

            test "AverageNormal fails when no normal can be found" {
                let collinear = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,1.,1.); Pnt(2.,2.,2.)]
                "collinear points have no average normal" |> Expect.throws (fun () -> collinear.AverageNormal |> ignore)
                "empty polyline" |> Expect.throws (fun () -> (Polyline3D([])).AverageNormal |> ignore)
                "two points" |> Expect.throws (fun () -> (Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]).AverageNormal |> ignore)
                "coincident points" |> Expect.throws (fun () -> (Polyline3D.createFromPnts [Pnt(5.,5.,5.); Pnt(5.,5.,5.); Pnt(5.,5.,5.)]).AverageNormal |> ignore)
                let bowTie = Polyline3D.createFromPnts [Pnt(0.,0.,4.); Pnt(1.,0.,4.); Pnt(0.,1.,4.); Pnt(1.,1.,4.)]
                "the cross products cancel out on a bow tie" |> Expect.throws (fun () -> bowTie.AverageNormal |> ignore)
                "but AveragePlane still finds the plane" |> Expect.floatClose tol (abs (bowTie.AveragePlane()).Normal.Z) 1.0
                "static averageNormal fails too" |> Expect.throws (fun () -> Polyline3D.averageNormal collinear |> ignore)
            }

            test "TryAverageNormal returns the same vector as AverageNormal" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,0.); Pnt(0.,1.,0.)]
                match pl.TryAverageNormal with
                | Some n ->
                    "same vector as AverageNormal" |> Expect.isTrue (eqVec n pl.AverageNormal)
                    "not unitized, its length is twice the area" |> Expect.floatClose tol n.Length 2.0
                | None -> "square has an average normal" |> Expect.isTrue false
                "static tryAverageNormal" |> Expect.isTrue (Polyline3D.tryAverageNormal pl |> Option.isSome)
            }

            test "TryAverageNormal returns None on degenerate input" {
                let collinear = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,1.,1.); Pnt(2.,2.,2.)]
                "collinear points" |> Expect.isTrue (collinear.TryAverageNormal |> Option.isNone)
                "empty polyline" |> Expect.isTrue ((Polyline3D([])).TryAverageNormal |> Option.isNone)
                "two points" |> Expect.isTrue ((Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]).TryAverageNormal |> Option.isNone)
                let bowTie = Polyline3D.createFromPnts [Pnt(0.,0.,4.); Pnt(1.,0.,4.); Pnt(0.,1.,4.); Pnt(1.,1.,4.)]
                "the cross products cancel out on a bow tie" |> Expect.isTrue (bowTie.TryAverageNormal |> Option.isNone)
                "but TryAveragePlane still finds the plane" |> Expect.isTrue (bowTie.TryAveragePlane() |> Option.isSome)
            }

            test "AveragePlane works on non planar points" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.); Pnt(1.,1.,1.); Pnt(0.,1.,0.)]
                "the warped quad is not planar" |> Expect.isFalse (pl.IsPlanar())
                "so TryPlane finds nothing" |> Expect.isTrue (pl.TryPlane() |> Option.isNone)
                "but there is an average plane" |> Expect.isTrue (pl.TryAveragePlane() |> Option.isSome)
                let pln = pl.AveragePlane()
                "origin is the average of all points" |> Expect.isTrue (eqPnt pln.Origin pl.Center)
                "normal points up" |> Expect.isTrue (pln.Normal.Z > 0.0)
                "normal is the unitized AverageNormal" |> Expect.floatClose tol (UnitVec.dot(pln.Normal, pl.AverageNormal.Unitized)) 1.0
                "static averagePlane is curried" |> Expect.isTrue (eqPnt (Polyline3D.averagePlane 1e-6 pl).Origin pln.Origin)
                "static tryAveragePlane is curried" |> Expect.isTrue (Polyline3D.tryAveragePlane 1e-6 pl |> Option.isSome)
            }

            test "AveragePlane of a planar polyline matches TryPlane" {
                let pl = Polyline3D.createFromPnts [Pnt(1.,2.,3.); Pnt(4.,2.,3.); Pnt(4.,5.,7.); Pnt(1.,5.,7.)]
                match pl.TryPlane(), pl.TryAveragePlane() with
                | Some strict, Some avg ->
                    "same origin" |> Expect.isTrue (eqPnt strict.Origin avg.Origin)
                    "same normal" |> Expect.floatClose tol (UnitVec.dot(strict.Normal, avg.Normal)) 1.0
                | _ -> "planar polyline has both planes" |> Expect.isTrue false
            }

            test "AveragePlane fails on degenerate input" {
                let collinear = Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,1.,1.); Pnt(2.,2.,2.)]
                "collinear points have no average plane" |> Expect.isTrue (collinear.TryAveragePlane() |> Option.isNone)
                "AveragePlane throws on collinear points" |> Expect.throws (fun () -> collinear.AveragePlane() |> ignore)
                "AveragePlane throws on too few points" |> Expect.throws (fun () -> (Polyline3D.createFromPnts [Pnt(0.,0.,0.); Pnt(1.,0.,0.)]).AveragePlane() |> ignore)
                "AveragePlane throws on coincident points" |> Expect.throws (fun () -> (Polyline3D.createFromPnts [Pnt(5.,5.,5.); Pnt(5.,5.,5.); Pnt(5.,5.,5.)]).AveragePlane() |> ignore)
            }

            test "polyline in a plane parallel to the world XZ plane" {
                let pl = Polyline3D.createFromPnts [Pnt(0.,7.,0.); Pnt(1.,7.,0.); Pnt(1.,7.,1.); Pnt(0.,7.,1.)]
                match pl.TryPlane() with
                | Some pln ->
                    "normal is along Y" |> Expect.floatClose tol (abs pln.Normal.Y) 1.0
                    "origin y is 7" |> Expect.floatClose tol pln.Origin.Y 7.0
                | None -> "vertical square has a plane" |> Expect.isTrue false
            }
        ]

        testList "Operations mutate in place" [
            test "transformations keep the same coordinate list" {
                let pts = [Pnt(3.,2.,1.); Pnt(9.,4.,2.); Pnt(7.,8.,-1.); Pnt(3.,2.,1.)]
                let pl = Polyline3D.createFromPnts pts
                let xyzs = pl.XYZs
                let q = Quaternion.createFromDegrees(Vec(1.,2.,3.), 40.)
                let m = Matrix.createRotationAxisCenter(Vec(-1.,1.,2.), Pnt(4.,6.,1.), 25.)
                pl.Move(Vec(-2.,5.,3.))
                pl.RotateAround (Pnt(4.,6.,1.)) q
                pl.Transform m
                pl.ScaleOn (Pnt(1.,-1.,2.)) 1.5
                Expect.isTrue (obj.ReferenceEquals(xyzs, pl.XYZs)) "same coordinate list"
                let expected =
                    Polyline3D.createFromPnts pts
                    |> Polyline3D.translate (Vec(-2.,5.,3.))
                    |> Polyline3D.rotateAround (Pnt(4.,6.,1.)) q
                    |> Polyline3D.transform m
                    |> Polyline3D.scaleOn (Pnt(1.,-1.,2.)) 1.5
                Expect.isTrue (Polyline3D.equalsTol 1e-12 pl expected) "instance and static calls give the same result"
                Expect.isFalse (eqPnt pl.Start (Pnt(3.,2.,1.))) "first point moved"
            }
            test "RemoveDuplicatePoints shrinks the same coordinate list" {
                let pl = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(3.001,2.,1.); Pnt(9.,4.,2.); Pnt(9.,4.,2.); Pnt(7.,8.,-1.)]
                let xyzs = pl.XYZs
                pl.RemoveDuplicatePoints 0.01
                Expect.isTrue (obj.ReferenceEquals(xyzs, pl.XYZs)) "same coordinate list"
                Expect.equal pl.PointCount 3 "two duplicates removed"
                Expect.isTrue (eqPnt pl.End (Pnt(7.,8.,-1.))) "end point kept"
            }
            test "RemoveDuplicatePointsFaithfully and RemoveDuplicateAndCollinearPoints shrink the same coordinate list" {
                let pl = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(9.,4.,2.); Pnt(9.004,4.002,2.001); Pnt(7.,8.,-1.)]
                let xyzs = pl.XYZs
                pl.RemoveDuplicatePointsFaithfully 0.01
                Expect.isTrue (obj.ReferenceEquals(xyzs, pl.XYZs)) "same coordinate list after RemoveDuplicatePointsFaithfully"
                Expect.equal pl.PointCount 3 "one duplicate removed"
                let line = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(5.,3.,2.); Pnt(7.,4.,3.); Pnt(7.,4.,3.); Pnt(9.,5.,4.)]
                let lineXyzs = line.XYZs
                line.RemoveDuplicateAndCollinearPoints Cosine.``0.1`` 1e-9
                Expect.isTrue (obj.ReferenceEquals(lineXyzs, line.XYZs)) "same coordinate list after RemoveDuplicateAndCollinearPoints"
                Expect.equal line.PointCount 2 "collinear and duplicate points removed"
            }
            test "RemoveUTurns without U-turns keeps the polyline unchanged" {
                let pl = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(9.,4.,2.); Pnt(7.,8.,-1.); Pnt(3.,2.,1.)]
                let xyzs = pl.XYZs
                pl.RemoveUTurns Cosine.``170.0``
                Expect.isTrue (obj.ReferenceEquals(xyzs, pl.XYZs)) "same coordinate list"
                Expect.equal pl.PointCount 4 "no point removed"
            }
            test "RemoveUTurnsDeeply leaves the polyline unchanged when a later pass fails" {
                // the first pass removes the spike, the second pass leaves too few points and fails
                let pl = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(13.,2.5,1.2); Pnt(3.2,3.,1.); Pnt(3.,2.,1.)]
                let once = Polyline3D.removeUTurns Cosine.``170.0`` pl
                Expect.equal once.PointCount 3 "a single pass removes the spike"
                Expect.throws (fun () -> pl.RemoveUTurnsDeeply Cosine.``170.0``) "repeated passes fail"
                Expect.equal pl.PointCount 4 "point count unchanged"
                Expect.isTrue (eqPnt (pl.GetPnt 1) (Pnt(13.,2.5,1.2))) "spike still there"
            }
            test "RemoveDuplicateAndCollinearPoints leaves the polyline unchanged when it fails" {
                let pl = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(3.,2.,1.); Pnt(3.,2.,1.)]
                Expect.throws (fun () -> pl.RemoveDuplicateAndCollinearPoints Cosine.``0.1`` 1e-6) "all points are duplicates"
                Expect.equal pl.PointCount 3 "point count unchanged"
            }
        ]

        testList "Static functions return a new Polyline3D" [

            test "all static transformations and clean up functions leave the input unchanged" {
                // a closed polygon off the origin, tilted out of the XY plane, with one duplicate point
                let withDuplicate () = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(9.,4.,2.); Pnt(9.,4.,2.); Pnt(7.,8.,-1.); Pnt(5.,6.,0.5); Pnt(3.,2.,1.)]
                let withoutDuplicate () = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(9.,4.,2.); Pnt(7.,8.,-1.); Pnt(5.,6.,0.5); Pnt(3.,2.,1.)] // U-turn removal fails on duplicates
                let check (name:string) (original:unit -> Polyline3D) (op:Polyline3D -> Polyline3D) =
                    let pl = original ()
                    let xyzs = pl.XYZs
                    let res = op pl
                    Expect.isFalse (obj.ReferenceEquals(res, pl)) $"{name} should return a new Polyline3D"
                    Expect.isFalse (obj.ReferenceEquals(res.XYZs, pl.XYZs)) $"{name} should return its own coordinate list"
                    Expect.isTrue (obj.ReferenceEquals(xyzs, pl.XYZs)) $"{name} should keep the coordinate list of the input"
                    Expect.isTrue (Polyline3D.equals pl (original ())) $"{name} should not change the input"
                let expectNewAndInputUnchanged name op = check name withDuplicate op
                let q = Quaternion.createFromDegrees(Vec(1.,2.,3.), 40.)
                let r = Rotation2D.createFromDegrees 30.
                let cen = Pnt(4.,6.,1.)
                let m = Matrix.createRotationAxisCenter(Vec(-1.,1.,2.), cen, 25.)
                expectNewAndInputUnchanged "scale" (Polyline3D.scale 1.5)
                expectNewAndInputUnchanged "scaleOn" (Polyline3D.scaleOn cen 1.5)
                expectNewAndInputUnchanged "move" (Polyline3D.move (Vec(-2.,5.,3.)))
                expectNewAndInputUnchanged "translate" (Polyline3D.translate (Vec(-2.,5.,3.)))
                expectNewAndInputUnchanged "moveX" (Polyline3D.moveX 3.)
                expectNewAndInputUnchanged "moveY" (Polyline3D.moveY 3.)
                expectNewAndInputUnchanged "moveZ" (Polyline3D.moveZ 3.)
                expectNewAndInputUnchanged "transform" (Polyline3D.transform m)
                expectNewAndInputUnchanged "transformRigid" (Polyline3D.transformRigid (RigidMatrix.createTranslation(Vec(-2.,5.,3.))))
                expectNewAndInputUnchanged "rotate" (Polyline3D.rotate q)
                expectNewAndInputUnchanged "rotateAround" (Polyline3D.rotateAround cen q)
                expectNewAndInputUnchanged "rotateOnZ" (Polyline3D.rotateOnZ r)
                expectNewAndInputUnchanged "rotateOnZWithCenter" (Polyline3D.rotateOnZWithCenter cen r)
                expectNewAndInputUnchanged "reverse" Polyline3D.reverse
                expectNewAndInputUnchanged "close" (Polyline3D.close 1e-6)
                expectNewAndInputUnchanged "ensureClockwiseIn2D" Polyline3D.ensureClockwiseIn2D
                expectNewAndInputUnchanged "ensureCounterClockwiseIn2D" Polyline3D.ensureCounterClockwiseIn2D
                check "removeUTurns" withoutDuplicate (Polyline3D.removeUTurns Cosine.``170.0``)
                check "removeUTurnsDeeply" withoutDuplicate (Polyline3D.removeUTurnsDeeply Cosine.``170.0``)
                expectNewAndInputUnchanged "removeDuplicatePoints" (Polyline3D.removeDuplicatePoints 0.01)
                expectNewAndInputUnchanged "removeDuplicatePointsAndReport" (Polyline3D.removeDuplicatePointsAndReport 0.01 >> fst)
                expectNewAndInputUnchanged "removeDuplicatePointsFaithfully" (Polyline3D.removeDuplicatePointsFaithfully 0.01)
                expectNewAndInputUnchanged "removeDuplicateAndCollinearPoints" (Polyline3D.removeDuplicateAndCollinearPoints Cosine.``0.1`` 1e-6)
            }

            test "static functions give the same result as their instance members" {
                let original () = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(9.,4.,2.); Pnt(9.,4.,2.); Pnt(7.,8.,-1.); Pnt(5.,6.,0.5); Pnt(3.,2.,1.)]
                let q = Quaternion.createFromDegrees(Vec(1.,2.,3.), 40.)
                let cen = Pnt(4.,6.,1.)
                let check (name:string) (op:Polyline3D -> Polyline3D) (inPlace:Polyline3D -> unit) =
                    let expected = original ()
                    inPlace expected
                    Expect.isTrue (Polyline3D.equals (op (original ())) expected) $"{name} should match its instance member"
                check "scaleOn" (Polyline3D.scaleOn cen 1.5) (fun p -> p.ScaleOn cen 1.5)
                check "rotateAround" (Polyline3D.rotateAround cen q) (fun p -> p.RotateAround cen q)
                check "reverse" Polyline3D.reverse (fun p -> p.Reverse())
                check "close" (Polyline3D.close 1e-6) (fun p -> p.Close(1e-6))
                check "ensureClockwiseIn2D" Polyline3D.ensureClockwiseIn2D (fun p -> p.EnsureClockwiseIn2D())
                check "removeDuplicatePoints" (Polyline3D.removeDuplicatePoints 0.01) (fun p -> p.RemoveDuplicatePoints 0.01)
                check "removeDuplicateAndCollinearPoints" (Polyline3D.removeDuplicateAndCollinearPoints Cosine.``0.1`` 1e-6) (fun p -> p.RemoveDuplicateAndCollinearPoints Cosine.``0.1`` 1e-6)
            }

            test "removeDuplicatePointsAndReport returns the cleaned copy and the removed points" {
                let pl = Polyline3D.createFromPnts [Pnt(3.,2.,1.); Pnt(9.,4.,2.); Pnt(9.,4.,2.); Pnt(7.,8.,-1.); Pnt(5.,6.,0.5); Pnt(3.,2.,1.)]
                let cleaned, removed = Polyline3D.removeDuplicatePointsAndReport 0.01 pl
                Expect.equal cleaned.PointCount 5 "one duplicate removed from the copy"
                Expect.equal removed.Count 1 "one removed point reported"
                Expect.isTrue (eqPnt removed.[0] (Pnt(9.,4.,2.))) "removed point"
                Expect.equal pl.PointCount 6 "input keeps all points"
            }
        ]
    ]
