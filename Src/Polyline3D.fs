namespace Euclid

open System
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open System.Collections.Generic

// # nowarn "52" // copying of structs


[<Obsolete("Not Obsolete, but internal, but needs to be public for inlining")>]
module Polyline3DErr =
    let failToSmall (name:string) (minCount:int) =
         EuclidException.Raise $"Euclid.Polyline3D.{name} failed on Polyline3D with less than {minCount} points "

#nowarn "44" //for obsolete warning
open Polyline3DErr

/// A mutable 3D Polyline.
/// If the last point is the same as the first point, the Polyline3D is closed.
// [<Struct>]
[<NoEquality; NoComparison>] // because its made up from floats
[<DataContract>] // for using DataMember on fields
type Polyline3D (points: ResizeArray<Pnt>) =

    /// Gets the internal list of all Points of the Polyline3D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline3D.
    [<DataMember>]
    member _.Points = points

        /// Create a new empty Polyline3D
    new () = Polyline3D(ResizeArray<Pnt>())

    /// Create a new empty Polyline3D with predefined capacity for the internal list of points.
    new (capacity:int) = Polyline3D(ResizeArray<Pnt>(capacity))

    /// Nicely formatted string representation of the Box including its size.
    override p.ToString() =
        if points.Count = 0 then
            "An empty Euclid.Polyline3D."
        else
            $"Euclid.Polyline3D with length {p.Length}, from {p.Points.Count} points"

    /// Creates a copy of the Polyline3D
    /// Same as polyline.Clone()
    member p.Duplicate(): Polyline3D =
        Polyline3D.createDirectlyUnsafe(points.GetRange(0, points.Count))

    /// Creates a copy of the Polyline3D.
    /// Same as polyline.Duplicate()
    member p.Clone(): Polyline3D =
        Polyline3D.createDirectlyUnsafe(points.GetRange(0, points.Count))

    /// Gets or sets first point of the Polyline3D
    /// This is the point at index 0.
    /// Same as Polyline3D.FirstPoint
    member p.Start
        with get() =
            if points.Count < 1 then failToSmall "Start.get" 1
            points.[0]
        and set(v) =
            if points.Count < 1 then failToSmall "Start.set" 1
            points.[0] <- v

    /// Gets or sets last or end point of the Polyline3D
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.LastPoint
    member p.End
        with get() =
            if points.Count < 1 then failToSmall "End.get" 1
            points.[points.Count - 1]
        and set(v) =
            if points.Count < 1 then failToSmall "End.set" 1
            points.[points.Count - 1] <- v

    /// Gets or sets the last point of the Polyline3D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.End
    member p.LastPoint
        with get() =
            if points.Count < 1 then failToSmall "LastPoint.get" 1
            points.[points.Count - 1]
        and set(v) =
            if points.Count < 1 then failToSmall "LastPoint.set" 1
            points.[points.Count - 1] <- v

    /// Gets or sets the second last point of the Polyline3D.
    member p.SecondLastPoint
        with get() =
            if points.Count < 2 then failToSmall "SecondLastPoint.get" 2
            points.[points.Count - 2]
        and set(v) =
            if points.Count < 2 then failToSmall "SecondLastPoint.set" 2
            points.[points.Count - 2] <- v

    /// Gets or sets the second point of the Polyline3D.
    /// This is the point at index 1.
    member p.SecondPoint
        with get() =
            if points.Count < 2 then failToSmall "SecondPoint.get" 2
            points.[1]
        and set(v) =
            if points.Count < 2 then failToSmall "SecondPoint.set" 2
            points.[1] <- v

    /// Gets or sets the first point of the Polyline3D.
    /// This is the point at index 0.
    /// Same as Polyline3D.Start
    member p.FirstPoint
        with get() =
            if points.Count < 1 then failToSmall "FirstPoint.get" 1
            points.[0]
        and set(v) =
            if points.Count < 1 then failToSmall "FirstPoint.set" 1
            points.[0] <- v

    /// Gets the count of points in the Polyline3D
    member p.PointCount =
        points.Count

    /// Gets the count of segments in the Polyline3D
    /// This is poly.Points.Count - 1
    member p.SegmentCount =
        max 0 (points.Count - 1 )

    /// Gets the index of the last point in the Polyline3D.
    /// points.Count - 1
    member p.LastPointIndex =
        points.Count - 1

    /// Gets the index of the last segment in the Polyline3D.
    /// This is poly.Points.Count - 2
    member p.LastSegmentIndex =
        points.Count - 2

    /// Gets the length of the Polyline3D
    /// Returns 0.0 if there are less than 2 points.
    member p.Length =
        let mutable l = 0.0
        if points.Count > 1 then
            let mutable prev = points.[0]
            for i = 1 to points.Count-1 do
                let t = points.[i]
                l <- l + Pnt.distance prev t
                prev <- t
        l

    /// Gets the segment at index i of the Polyline3D.
    member p.GetSegment(i:int) =
        if i < 0 || i > points.Count - 2 then
            EuclidException.Raisef "Euclid.Polyline3D.GetSegment: index %d is out of range for Polyline3D with %d points." i points.Count
        Line3D(points.[i], points.[i+1])

    /// Gets the last segment of the Polyline3D.
    member p.LastSegment =
        if points.Count < 2 then failToSmall ".LastSegment" 2
        let i = points.Count - 1
        Line3D(points.[i-1], points.[i])

    /// Gets the first segment of the Polyline3D.
    member p.FirstSegment =
        if points.Count < 2 then failToSmall "FirstSegment" 2
        Line3D(points.[0], points.[1])

    /// Returns all segments of the Polyline3D as a list of Line3D.
    member p.Segments =
        let lns = ResizeArray()
        let pts = points
        if pts.Count < 2 then
            lns
        else
            let mutable a = pts.[0]
            for i = 1 to points.LastIndex do
                let b = pts.[i]
                lns.Add(Line3D(a, b))
                a <- b
            lns

    /// Gets the a bounding box of the Polyline3D
    member p.BoundingBox =
        BBox.createFromIList points

    /// Tests if Polyline3D start and end points are exactly the same.
    /// Fails if the Polyline3D has less than 3 points.
    member p.IsClosed =
        if points.Count < 3 then failToSmall "IsClosed" 3
        let v = p.Start  - p.End
        v.IsZero

    /// Tests if Polyline3D is closed within given tolerance.
    /// Fails if the Polyline3D has less than 3 points.
    member p.IsAlmostClosed(tolerance) =
        if points.Count < 3 then failToSmall "IsAlmostClosed" 3
        let v = p.Start  - p.End
        v.LengthSq < tolerance*tolerance

    /// Reverse order of the Polyline3D in place.
    member p.ReverseInPlace() =
        points.Reverse()

    /// Returns new Polyline3D in reversed Order.
    member p.Reverse () =
        let n = p.Duplicate()
        n.Points.Reverse()
        n

    /// Close the Polyline3D if it is not already closed.
    /// If the ends are closer than the tolerance. The last point is set to equal the first point.
    /// Else the start point is added to the end of the Polyline3D.
    member p.CloseIfOpen(toleranceForAddingPoint) =
        if points.Count < 3 then failToSmall "CloseIfOpen" 3
        let v = p.Start  - p.End
        if v.LengthSq < toleranceForAddingPoint*toleranceForAddingPoint then
            points.Last <- p.Start
        else
            points.Add p.Start



    /// Calculates the signed area of the Polyline3D when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is CCW.
    member p.SignedAreaIn2D =
        //https://helloacm.com/sign-area-of-irregular-polygon/
        let mutable area = 0.0
        let mutable t = points.Last // calculate from last to first too
        for i=0 to points.Count-1 do
            let n = points.[i]
            let a = t.X - n.X
            let b = n.Y + t.Y
            area <- area + a*b
            t <- n
        area

    /// Test if Polyline3D is CounterClockwise when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is CCW.
    member p.IsCounterClockwiseIn2D =
        let  area = p.SignedAreaIn2D
        if   abs(area) < UtilEuclid.zeroLengthTolerance then EuclidException.Raisef "Euclid.Polyline3D.IsCounterClockwiseIn2D: Polyline3D the area is zero: %O" p
        else area > 0.0


    /// Test if Polyline3D is Clockwise when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is CCW.
    member p.IsClockwiseIn2D =
        let area = p.SignedAreaIn2D
        if   abs(area) < UtilEuclid.zeroLengthTolerance then EuclidException.Raisef "Euclid.Polyline3D.IsClockwiseIn2D: Polyline3D the area is zero: %O" p
        else area < 0.0


    /// Returns the point at a given parameter on the Polyline3D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    /// If the parameter is within 1e-6 of an integer value, the integer value is used as parameter.
    member pl.EvaluateAt(t:float) =
        let i = int t       // integer part of the parameter
        let p = t - float i // fractional part of the parameter
        if   i < -1 then
            EuclidException.Raisef "Euclid.Polyline3D.EvaluateAt: Parameter %f is less than 0.0" t

        elif i >= pl.Points.Count then
            EuclidException.Raisef "Euclid.Polyline3D.EvaluateAt: Parameter %f is more than point count(%d) - 1." t pl.Points.Count

        // handle the case where the value is just below 0.0:
        elif i = -1 then
            if p > 0.99999 then pl.Points.First
            else EuclidException.Raisef "Euclid.Polyline3D.EvaluateAt: Parameter %f is less than 0.0" t

        // handle the case where the value is just above point count - 1:
        elif i = pl.Points.Count - 1  then
            if p < 0.00001 then pl.Points.Last
            else EuclidException.Raisef "Euclid.Polyline3D.EvaluateAt: Parameter %f is more than point count(%d) - 1." t pl.Points.Count

        // return point if point is almost matching and integer
        elif p < 0.000001 then
            pl.Points.[i]
        elif p > 0.999999 then
            pl.Points.[i+1]
        else
            let t = pl.Points.[i]
            let v = pl.Points.[i+1] - t
            t + v * p

    /// Returns the Unitized Tangent at a given parameter on the Polyline3D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.TangentAt(t:float) =
        let i = int t
        let p = t - float i
        if   i < -1 then
            EuclidException.Raisef "Euclid.Polyline3D.TangentAt: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then
            EuclidException.Raisef "Euclid.Polyline3D.TangentAt: Parameter %f is more than point count(%d)." t pl.Points.Count
        elif i = -1 then
            if p > 0.9999 then UnitVec.create(pl.Points.First, pl.Points.Second)
            else EuclidException.Raisef "Euclid.Polyline3D.TangentAt: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then
            if   p > 1e-4 then  EuclidException.Raisef "Euclid.Polyline3D.TangentAt: Parameter %f is more than point count(%d)." t pl.Points.Count
            else UnitVec.create(pl.Points.SecondLast, pl.Points.Last)
        // return point  if point is almost matching
        else
            UnitVec.create(pl.Points.[i], pl.Points.[i+1])



    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.ClosestParameter(pt:Pnt) =
        // for very large polylines, this is could be optimized by using search R-tree
        let ps = pl.Points
        if points.Count < 2 then failToSmall "ClosestParameter" 2
        // vectors of the segments
        let vs = Array.zeroCreate (points.Count-1)
        for i = 0 to vs.Length-1 do
            let ti = ps[i]
            let ne = ps[i+1]
            vs[i] <- ne-ti

        // closest parameters  of the segments
        let ts = Array.zeroCreate (points.Count-1)
        for i = 0 to ts.Length-1 do
            let p = ps[i]
            let v = vs[i]
            // finding ClosestParameter on line segment and clamp to 0.0 to 1.0
            let len = v.LengthSq
            ts[i] <- if len < 1e-9 then 0.0 else -((p-pt) *** v) / len |> UtilEuclid.clampBetweenZeroAndOne //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html

        // square distances per segment
        let ds = Array.zeroCreate (points.Count-1)
        for i = 0 to ds.Length-1 do
            let p = ps[i]
            let v = vs[i]
            let t = ts[i]
            ds[i] <- Pnt.distanceSq pt (p + v*t)

        let i = Arr.minIndex ds
        let t = ts.[i]
        float i + t

    /// Returns the point on the Polyline3D that is the closest point to the given point.
    member pl.ClosestPoint(pt:Pnt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t

    /// Returns the distance of the test point to the closest point on the Polyline3D.
    member pl.DistanceTo(pt:Pnt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t
        |> Pnt.distance pt


    /// Returns the average center of all points of the Polyline3D.
    member pl.Center =
        if points.Count = 0 then failToSmall "Center" 1
        let mutable x = 0.0
        let mutable y = 0.0
        let mutable z = 0.0
        for i = 0 to points.LastIndex do
            let p = points.[i]
            x <- x + p.X
            y <- y + p.Y
            z <- z + p.Z
        Pnt(x / float points.Count, y / float points.Count, z / float points.Count)

    /// Scales the 3D polyline by a given factor.
    /// Scale center is World Origin 0,0
    member p.Scale (factor:float) : Polyline3D =
        let ps = points  |> ResizeArr.map (fun pt -> pt * factor)
        Polyline3D.createDirectlyUnsafe ps


    /// Scales the 2D polyline by a given factor on a given center point
    member p.ScaleOn (cen:Pnt) (factor:float) : Polyline3D =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        points
        |> ResizeArr.map (fun pt ->
            Pnt(cx + (pt.X - cx) * factor,
                cy + (pt.Y - cy) * factor,
                cz + (pt.Z - cz) * factor)
            )
        |> Polyline3D.createDirectlyUnsafe


    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Gets the internal list of all Points of the Polyline3D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline3D.
    static member pointsUnsafeInternal (p:Polyline3D) =
        p.Points

    /// Gets first point of the Polyline3D
    static member start (p:Polyline3D) =
        let points = p.Points
        if points.Count < 1 then  failToSmall "start" 1
        points.[0]

    /// Gets last or end point of the Polyline3D
    static member ende (p:Polyline3D) =
        let points = p.Points
        if points.Count < 1 then failToSmall "ende" 1
        points.[points.Count - 1]

    /// Reverse order of the Polyline3D in place.
    static member reverseInPlace (p:Polyline3D) =
        p.ReverseInPlace()

    /// Returns new Polyline3D in reversed Order.
    static member reverse (p:Polyline3D) =
        p.Reverse()

    /// Returns the point at a given parameter on the Polyline3D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at point count.
    static member evaluateAt (t:float) (pl:Polyline3D) =
        pl.EvaluateAt t


    /// Apply a mapping function to each point in the 3D Polyline. Returns new Polyline3D.
    static member map (mapping:Pnt->Pnt) (pl:Polyline3D) =
        pl.Points |> ResizeArr.map mapping |> Polyline3D.createDirectlyUnsafe

    /// Move a Polyline3D by a vector. (same as Polyline3D.move)
    static member translate (v:Vec) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.addVec v)

    /// Move a Polyline3D by a vector. (same as Polyline3D.translate)
    static member move (v:Vec) (pl:Polyline3D) = Polyline3D.translate v pl

    /// Returns a Polyline3D moved by a given distance in X direction.
    static member moveX (distance:float) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.moveX distance)

    /// Returns a Polyline3D moved by a given distance in Y direction.
    static member moveY (distance:float) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.moveY distance)

    /// Returns a Polyline3D moved by a given distance in Z direction.
    static member moveZ (distance:float) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.moveZ distance)


    /// Scales the Polyline3D by a given factor.
    /// Scale center is World Origin 0,0,0
    /// Returns a new Polyline3D.
    static member scale (factor:float) (pl:Polyline3D) : Polyline3D =
        pl |> Polyline3D.map (fun pt -> pt * factor)


    /// Applies a 4x4 transformation matrix.
    static member transform (m:Matrix) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.transform m)

    /// Rotation a Polyline3D around Z-Axis.
    static member rotate (r:Rotation2D) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.rotateZBy r)

    /// Rotation a Polyline3D round given Center point an a local Z-axis.
    static member rotateWithCenter (cen:Pnt) (r:Rotation2D) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.rotateZwithCenterBy cen r)

    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at point count.
    static member closestParameter (pl:Polyline3D) (pt:Pnt) =
        pl.ClosestParameter pt

    /// Returns the point on the Polyline3D that is the closest point to the given point.
    static member closestPoint (pl:Polyline3D) (pt:Pnt) =
        pl.ClosestPoint pt

    /// Returns the distance of the test point to the closest point on the Polyline3D.
    static member distanceTo (pl:Polyline3D) (pt:Pnt) =
        pl.DistanceTo pt

    /// Create a new Polyline3D by copying over all points.
    static member create(points: seq<Pnt>) =
        Polyline3D(ResizeArray(points))

    /// Create a new Polyline3D by using the provided ResizeArray directly.
    /// All later changes to the ResizeArray will be reflected in the Polyline3D.
    static member createDirectlyUnsafe (points: ResizeArray<Pnt>) =
        Polyline3D(points)

    /// Create a new empty Polyline3D without any points.
    /// But predefined capacity.
    static member createEmpty (capacity:int) =
        Polyline3D(ResizeArray(capacity))

    /// Returns new Polyline3D from point at Parameter a to point at Parameter b.
    /// if 'a' is bigger 'b' then the new Polyline3D is in opposite direction.
    /// If a parameter is within 1e-4 of an integer value, the integer value is used as parameter.
    static member subPolyline a b (pl:Polyline3D) : Polyline3D =
        let rev = a>b
        let u, v = if rev then b, a else a, b
        let np = Polyline3D.createEmpty (int(v-u)+2)
        let nps = np.Points
        let ps  = pl.Points
        // first point
        let ui = int u
        let uf = u - float ui
        if uf < 0.9999 then
            nps.Add(pl.EvaluateAt u)
        // inner points
        for i = int u + 1 to int v do
            if i >= 0 && i < ps.Count then
                nps.Add(ps[i])
        // last point
        let vi = int v
        let vf = v - float vi
        if vf > 1e-4 then
            nps.Add(pl.EvaluateAt v)
        // reverse if necessary
        if rev then
            np.ReverseInPlace()
        np

    [<Obsolete("Renamed to Polyline3D.subPolyline")>]
    static member segment a b (pl:Polyline3D) :Polyline3D =
        Polyline3D.subPolyline a b pl


    /// Returns a new closed Polyline3D.
    /// If the first and last point are within 1e-6 of each other, the last point is set equal to the first point.
    /// Otherwise one point is added.
    static member close (pl:Polyline3D) =
        if pl.Points.Count < 2 then failToSmall "close" 2
        let points = pl.Points
        let np = Polyline3D.createEmpty (points.Count + 1)
        np.Points.AddRange(points.GetRange(0, points.Count))
        if Pnt.distanceSq points.First points.Last < 1e-12 then
            np.Points.[np.Points.Count-1] <- np.Points.First // set last point equal to first
        else
            np.Points.Add np.Points.First
        np

    /// Closes the Polyline3D in place by adding a point.
    /// If the first and last point are within 1e-6 of each other, the last point is set equal to the first point instead.
    static member closeInPlace (pl:Polyline3D) =
        if pl.Points.Count < 2 then failToSmall "closeInPlace" 2
        let points = pl.Points
        if Pnt.distanceSq points.First points.Last < 1e-12 then
            points.[points.Count-1] <- points.First
        else
            points.Add points.First


    /// Tests if two Polyline3D have the same number of points and points are equal within a given tolerance.
    static member equals tol (a:Polyline3D) (b:Polyline3D) =
        let k = a.PointCount
        if k <> b.PointCount then
            false
        else
            let mutable i = 0
            let mutable same = true
            let aPts = a.Points
            let bPts = b.Points
            while i < k && same do
                if Pnt.equals tol aPts.[i] bPts.[i] then
                    i <- i + 1
                else
                    same <- false
            same

    //--------------------------------------------------------------------------------
    //------------------------Offset------------------------------------
    //--------------------------------------------------------------------------------

    /// Returns the index of an outer corner and.
    /// a normal vector corresponding to an Counter-Clockwise view on the loop.
    /// This is used to calculate the RefNormal vector for the offset function.
    /// The input vectors are the vectors for each segment of the polyline.
    /// From first and second point up to last and first point.
    static member findOuterCornerAndRefNormal(pts:ResizeArray<Pnt>, vs:Vec[])=
        if pts.Count <> vs.Length then EuclidException.Raisef "Euclid.Polyline3D.findOuterCornerAndRefNormal pts (%d) and vs(%d) must have the same length." pts.Count vs.Length
        let us = Array.zeroCreate vs.Length
        // mark very short segments with 0, 0, 0:
        for i=0 to vs.Length-1 do
            let v = vs.[i]
            let l = v.Length
            if l > 1e-6 then
                let f = 1. / l
                us.[i] <- UnitVec.createUnchecked(v.X*f, v.Y*f, v.Z*f)
            //else  us.[i] <- Vec.Zero //happens anyway by zeroCreate

        let ref = Points.normalOfPoints(pts)// the orientation of this may wrong.
        let mutable posAngSum = 0.0
        let mutable negAngSum = 0.0
        let mutable posIdx = -1
        let mutable negIdx = -1
        let isNotZero  (v:UnitVec) = v.X<>0. || v.Y<>0. || v.Z<>0.

        // get angle sums
        let prevUIdx = us|> Array.findIndexBack isNotZero
        let mutable prevU = us.[prevUIdx]
        let len = us.Length
        let deg2 = Math.PI / 90. // 2 degree
        let rec loop(i) =
            if i < len then
                let thisU = us.[i]
                if isNotZero thisU then
                    let ang = UnitVec.anglePi thisU prevU
                    if ang < deg2 then // parallel point, loop on
                        loop(i+1)
                    else
                        let c = UnitVec.cross(prevU, thisU)
                        if c *** ref>0.0 then
                            posAngSum <- posAngSum + ang
                            if posIdx= -1 then posIdx <- i
                        else
                            negAngSum <- negAngSum + ang
                            if negIdx= -1 then negIdx <- i
                        prevU <- thisU
                        loop(i+1)
                else // Duplicate point, loop on
                    loop(i+1)
        loop(0)

        if posAngSum > negAngSum then
            posIdx, ref
        else
            negIdx, -ref

    /// The inner core routine of Points.offset. This function considers input a closed polyline.
    /// Start point and end point may not be equal, all arrays must be of the same length.
    /// 'referenceNormal' to be in Z Axis for Counter-Clockwise loops in 2D or if it is Vec.Zero it wil be calculated add hoc.
    static member offsetCore(pts:ResizeArray<Pnt>, offD:IList<float>, normD:ResizeArray<float>, referenceNormal:Vec, fixColinearLooped, allowObliqueOffsetOnColinearSegments) : ResizeArray<Pnt>  =
        if notNull offD  && pts.Count <> offD.Count  then EuclidException.Raisef "Euclid.Polyline3D.offsetCore pts(%d) and offD(%d) must have the same length." pts.Count offD.Count
        if notNull normD && pts.Count <> normD.Count then EuclidException.Raisef "Euclid.Polyline3D.offsetCore pts(%d) and normD(%d) must have the same length." pts.Count normD.Count
        let lenTolSq = 1e-12 // square length tolerance
        let lenPts   = pts.Count
        let lastIdx  = lenPts - 1

        // (1) collect array of vectors going from this to next
        let vs = Array.zeroCreate lenPts
        let mutable this = pts.[0]
        for i=1 to pts.Count-1 do
            let next = pts.[i]
            vs.[i-1] <- next-this
            this <- next
        vs.[lastIdx] <- pts.[0]-pts[lastIdx]

        let refNorm :Vec =
            if referenceNormal.IsZero then Polyline3D.findOuterCornerAndRefNormal(pts, vs) |> snd
            else referenceNormal

        // (2) main loop
        let colinear: bool[] = Array.zeroCreate lenPts
        let res = pts.GetRange(0, lenPts) // copy
        //(2.1) find last valid vector
        let prevVIdx =
            vs
            |> Array.tryFindIndexBack (fun v -> v.LengthSq > lenTolSq)
            |> Option.defaultWith (fun _ -> EuclidException.Raisef "Euclid.Polyline3D.offsetCore: invalid Polyline3D, all points are in the same location" )
        for i=prevVIdx+1 to lastIdx do // set last points a colinear if prevVIdx is not the index of the last point
            colinear.[i] <- true

        let mutable prevV = vs.[prevVIdx]
        let mutable prevOff = if notNull offD then offD.[prevVIdx] else 0.0

        //(2.2) looping


        for i = 0 to lastIdx do
            let nextV = vs.[i]
            let thisOff = if notNull offD then  offD.[i] else 0.0
            let normDist   = if notNull normD then normD.[i] else 0.0

            let doOff   = abs(thisOff)  > 1e-9 && abs(prevOff) > 1e-9
            let doNorm  = abs(normDist) > 1e-9

            if not doOff && not doNorm then
                // offset distance is zero for previous and this segment,
                // just copy the point
                res.[i] <- pts.[i]
                prevOff <- thisOff
                prevV   <- nextV
            else
                let ax = prevV.X
                let ay = prevV.Y
                let az = prevV.Z
                let bx = nextV.X
                let by = nextV.Y
                let bz = nextV.Z
                let a = ax*ax + ay*ay + az*az // square length of prevV
                let c = bx*bx + by*by + bz*bz // square length of nextV
                if c < lenTolSq then
                    colinear.[i] <- true
                    prevOff <- thisOff
                    prevV   <- nextV

                //elif a < lenTolSq then
                //    failwithf "too short segment not recognized. This should already be checked!"
                else
                    let b = ax*bx + ay*by + az*bz // dot product of both lines
                    let ac = a*c // square of square length, never negative
                    let bb = b*b // square of square dot product, never negative
                    let discriminant = ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
                    let div = ac+bb // never negative
                    let rel = discriminant/div
                    if rel < float RelAngleDiscriminant.``0.25`` then //parallel
                        colinear.[i] <- true
                        prevOff <- thisOff
                        prevV   <- nextV
                    else
                        let n = Vec.cross(prevV, nextV) |> Vec.matchOrientation refNorm

                        // in corner offset
                        if doOff then
                            let thisPt = pts.[i]
                            let offP = thisPt + (Vec.cross(n, prevV)  |> Vec.withLength prevOff)  // not always the same as lastIdx !
                            let offN = thisPt + (Vec.cross(n, nextV)  |> Vec.withLength thisOff)
                            let vx = offN.X - offP.X
                            let vy = offN.Y - offP.Y
                            let vz = offN.Z - offP.Z
                            let e = bx*vx + by*vy + bz*vz
                            let d = ax*vx + ay*vy + az*vz
                            let t = (c * d - b * e) / discriminant
                            res.[i] <- offP + t * prevV

                        // add normal offset:
                        if doNorm then
                            res.[i] <- res.[i] + n.Unitized * normDist

                        prevOff <- thisOff
                        prevV <- nextV


        // (3.5) correct colinear points by nearest neighbors that are ok in a loop
        if fixColinearLooped then
            let rec searchBack i =
                let ii = saveIdx (i) colinear.Length
                if not colinear.[ii] then ii
                elif i < -colinear.Length then EuclidException.Raisef "Euclid.Polyline3D.offsetCore : all %d points for offset are colinear within 0.25 degree or identical. " pts.Count
                else searchBack (i - 1)

            let rec  searchForward i =
                let ii = saveIdx (i) colinear.Length
                if not colinear.[ii] then ii // no need to check for endless loop here because check is done in searchBack that is called first
                else searchForward (i + 1)

            for i = 0 to colinear.Length-1 do
                if colinear.[i]  then
                    let pi = searchBack    (i - 1)
                    let ni = searchForward (i + 1)
                    if pi = ni then //  does this ever happen ? it is either all colinear (caught in searchBack) or at least three points are not colinear ??
                        EuclidException.Raisef "Euclid.Polyline3D.offsetCore : all %d points for offset are colinear within 0.25 degree or identical. " pts.Count
                    let ln = Line3D(res.[pi], res.[ni])
                    res.[i] <- ln.ClosestPointInfinite(pts.[i])
                    // TODO add safety check? could be omitted. offset is then averaged out.
                    if not allowObliqueOffsetOnColinearSegments && abs (offD.[pi] - offD.[ni]) > 1e-9 then
                        EuclidException.Raise $"Euclid.Polyline3D.offsetCore: can't offset collinear segment at index {i} with different offset distances from index {pi} and {ni}{Format.nl} these distances are not the same: {offD.[pi]} and {offD.[ni]}"
        else
            let rec searchBack i =
                if i<0 then -1
                elif not colinear.[i] then i
                else searchBack (i - 1)

            let rec  searchForward i =
                if i = colinear.Length  then -1
                elif not colinear.[i] then i
                else searchForward (i + 1)

            for i = 0 to colinear.Length-1 do
                if colinear.[i]  then
                    let pi = searchBack    (i-1)
                    let ni = searchForward (i+1)
                    if pi = ni then //  does this ever happen ? it is either all colinear (caught in searchBack) or at least three points are not colinear ??
                        EuclidException.Raisef "Euclid.Polyline3D.offsetCore : all %d points for offset are colinear within 0.25 degree or identical. " pts.Count
                    elif pi = -1 then
                        if ni = -1 then
                            EuclidException.Raisef "Euclid.Polyline3D.offsetCore : all %d points for offset are colinear within 0.25 degree or identical. " pts.Count
                        else // colinear start, get frame
                            match Points.offsetInCornerEx (pts[ni-1], pts[ni], pts[ni+1],  offD.[ni-1],  offD.[ni], refNorm) with
                            |ValueNone -> EuclidException.Raisef "Euclid.Polyline3D.offsetCore :offsetInCornerEx-1 failed unexpectedly."
                            |ValueSome (_, n, prevShift, _) -> res.[i] <- pts.[i] + prevShift + if isNull normD then Vec.Zero else n * normD.[i]
                    elif ni = -1 then  // colinear end, get frame
                        match Points.offsetInCornerEx (pts[pi-1], pts[pi], pts[pi+1],  offD.[pi-1],  offD.[pi], refNorm) with
                        |ValueNone -> EuclidException.Raisef "Euclid.Polyline3D.offsetCore :offsetInCornerEx-1 failed unexpectedly."
                        |ValueSome (_, n, _, nextShift) -> res.[i] <- pts.[i] + nextShift + if isNull normD then Vec.Zero else n * normD.[i]
                    else
                        let ln = Line3D(res.[pi], res.[ni])
                        res.[i] <- ln.ClosestPointInfinite(pts.[i])
                        // TODO add safety check? could be omitted. offset is then averaged out.
                        if not allowObliqueOffsetOnColinearSegments && abs (offD.[pi] - offD.[ni]) > 1e-9 then
                            EuclidException.Raise $"Euclid.Polyline3D.offsetCore: can't offset collinear segment at index {i} with different offset distances from index {pi} and {ni}{Format.nl} these distances are not the same: {offD.[pi]} and {offD.[ni]}"
        res


    /// <summary>Offsets a Polyline in 3D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// Does not fail on colinear or duplicate points.</summary>
    /// <param name="polyLine"> A 3D Polyline. </param>
    /// <param name="offsetDistances">The parallel offset distances for each segment of the polyline.
    ///    A positive distance offsets inwards in corners, a negative offset outwards.
    ///    For open and closed polylines this list of distances must have one item less than number of points in the polyline.
    ///    Except if the polyline is open and the loop parameter is set to true. Then points and distances list shall have the same count.
    ///    A empty list for no offset or singleton for constant offset is allowed too.
    /// </param>
    /// <param name="normalDistances">Optional. Normal distances defined as a perpendicular offset at each corner.
    ///    Unlike offsetDistances this list of distances must have the same item count as number of points in the polyline.
    ///    Except if the input polyline is already closed. Then distances list shall have one less. (independent of 'loop' parameter)
    ///    A empty list for no normal offset or singleton for constant offset is allowed too.
    /// </param>
    /// <param name="loop">Optional. Consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="refNormal">Optional. An approximate orientation Normal to help find the correct offset side,
    ///    if given it doesn't need to be calculated anymore. This speeds up the algorithm.
    ///    To be in Z Axis orientation for Counter-Clockwise loops in 2D.
    /// </param>
    /// <param name="obliqueOffsets">Optional. When two adjacent segments are colinear but have different offset distances there is no solution with a parallel offset.
    ///    By default an exception is raised. Set this to true to create and averaged oblique offset instead of failing.
    /// </param>
    /// <returns>A list of points that has the same length as the input list.</returns>
    static member offset(   polyLine:Polyline3D,
                            offsetDistances: float seq,
                            [<OPT;DEF(null:seq<float>)>] normalDistances: float seq,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(Vec())>] refNormal:Vec,
                            [<OPT;DEF(false)>] obliqueOffsets:bool
                            ) : Polyline3D =
        let points = polyLine.Points
        // (1) Fail if polyline has less than one point
        if points.Count < 2 then
            EuclidException.Raisef "Euclid.Polyline3D.offset needs at least two points but %O given." polyLine

        let getWithLength len (xs:seq<float>) : Result<ResizeArray<float>, int> =
            if isNull xs then Ok null
            else
                let ds = ResizeArray(xs)
                if   ds.Count = 0               then Ok  null
                elif ds.Count = 1 && ds.[0]=0.0 then Ok  null
                elif ds.Count = 1               then Ok  (ResizeArr.create len ds.[0])
                elif ds.Count = len             then Ok  ds
                else Error(ds.Count)

        // (2) check if last and first point are the same and if so remove last point
        if Pnt.distanceSq points.[0] points.[points.Count-1] < 1e-12 then // sqrt of 1e-6, auto detect closed polyline points  then
            let pts = points.GetRange(0, points.Count-1) // remove last point
            let offD =
                match getWithLength pts.Count offsetDistances with
                |Error k  -> EuclidException.Raise $"Euclid.Polyline3D.offset: offsetDistances has {k} items but should have 0, 1 or {pts.Count} for {points.Count} given points {Format.nl}in closed Polyline3D with identical start and end:{Format.nl}{polyLine}"
                |Ok    ds -> ds

            let normD =
                match getWithLength pts.Count normalDistances with
                |Error k  -> EuclidException.Raise $"Euclid.Polyline3D.offset: normalDistances has {k} items but should have 0, 1 or {pts.Count} for {points.Count} given points {Format.nl}in closed Polyline3D with identical start and end:{Format.nl}{polyLine}"
                |Ok    ds -> ds

            let res = Polyline3D.offsetCore(pts, offD, normD, refNormal, fixColinearLooped=true, allowObliqueOffsetOnColinearSegments=obliqueOffsets)
            res.Add(res.[0])     // set last equal first
            Polyline3D.createDirectlyUnsafe res

        // (3) check if open but looping desired
        elif loop then
            let offD =
                match getWithLength points.Count offsetDistances with
                |Error k  -> EuclidException.Raise $"Euclid.Polyline3D.offset: offsetDistances has {k} items but should have 0, 1 or {points.Count} for {points.Count} given points {Format.nl}in open Polyline3D with start and end apart and loop set to '{loop}' :{Format.nl}{polyLine}"
                |Ok    ds -> ds

            let normD =
                match getWithLength points.Count normalDistances with
                |Error k  -> EuclidException.Raise $"Euclid.Polyline3D.offset: normalDistances has {k} items but should have 0, 1 or {points.Count} for {points.Count} given points {Format.nl}in closed Polyline3D with start and end apart and loop set to '{loop}' :{Format.nl}{polyLine}"
                |Ok    ds -> ds

            Polyline3D.offsetCore(points, offD, normD, refNormal, fixColinearLooped=true, allowObliqueOffsetOnColinearSegments=obliqueOffsets)
            |> Polyline3D.createDirectlyUnsafe

        // (4)  open, no looping desired
        else
            let offD =
                match getWithLength (points.Count-1) offsetDistances with
                |Error k  -> EuclidException.Raise $"Euclid.Polyline3D.offset: offsetDistances has {k} items but should have 0, 1 or {points.Count} for {points.Count} given points {Format.nl}in open Polyline3D with start and end apart and loop set to '{loop}' :{Format.nl}{polyLine}"
                |Ok    ds ->
                    if notNull ds then ds.Add ds.[0] // make same length as points
                    ds

            let normD =
                match getWithLength points.Count normalDistances with
                |Error k  -> EuclidException.Raise $"Euclid.Polyline3D.offset: normalDistances has {k} items but should have 0, 1 or {points.Count} for {points.Count} given points {Format.nl}in closed Polyline3D with start and end apart and loop set to '{loop}' :{Format.nl}{polyLine}"
                |Ok    ds -> ds

            let res = Polyline3D.offsetCore(points, offD, normD, refNormal, fixColinearLooped=false, allowObliqueOffsetOnColinearSegments=obliqueOffsets)

            // (4.1) fix ends if not looped
            // TODO if the normal offset is not constant, then the end points will not be exactly correct
            if not loop then
                let firstLn = Line3D(points.[0], points.[1])
                let firstV = res.[0]  - (firstLn.ClosestPointInfinite res.[0] )
                res.[0]  <- points.[0] + firstV
                let lastIdx = res.Count - 1
                let lastLn = Line3D(points.[lastIdx-1], points.[lastIdx])
                let lastV = res.[lastIdx]  - (lastLn.ClosestPointInfinite res.[lastIdx] )
                res.[lastIdx]  <- points.[lastIdx] + lastV

            Polyline3D.createDirectlyUnsafe res

    /// <summary>Offsets a Polyline in 3D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// Does not fail on colinear or duplicate points.</summary>
    /// <param name="polyLine"> A 3D Polyline. </param>
    /// <param name="offsetDistance">The offset distance for all segments of the polyline. A positive distance offsets inwards in corners, a negative offset outwards.</param>
    /// <param name="normalDistance">Optional, The normal distance defined as a perpendicular offset for all corners. </param>
    /// <param name="loop">Consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="refNormal">Optional. An approximate orientation Normal to help find the correct offset side,
    ///    if given it doesn't need to be calculated anymore. This speeds up the algorithm.
    ///    To be in Z Axis orientation for Counter-Clockwise loops in 2D.
    /// </param>
    /// <returns>A list of points that has the same length as the input list.</returns>
    static member offset(   polyLine:Polyline3D,
                            offsetDistance: float,
                            [<OPT;DEF(0.0)>]normalDistance: float,
                            [<OPT;DEF(false)>]loop:bool,
                            [<OPT;DEF(Vec())>] refNormal:Vec) : Polyline3D =
        if normalDistance = 0.0 then Polyline3D.offset(polyLine, [offsetDistance], []              , loop, refNormal, false)
        else                         Polyline3D.offset(polyLine, [offsetDistance], [normalDistance], loop, refNormal, false)


    // the above two methods do not fail anymore with fable
    // see https://github.com/fable-compiler/Fable/issues/3326


