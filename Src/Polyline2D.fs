namespace Euclid

open System
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open System.Collections.Generic // forIList



[<Obsolete("Not Obsolete, but internal, but needs to be public for inlining")>]
module Polyline2DErr =
    let failToSmall (name:string) (minCount:int) =
         EuclidException.Raise $"Euclid.Polyline2D.{name} failed on Polyline2D with less than {minCount} points "

#nowarn "44" //for obsolete warning
open Polyline2DErr

/// A mutable 2D Polyline.
/// If the last point is the same as the first point, the Polyline2D is considered closed.
// [<Struct>]
[<NoEquality; NoComparison>] // because its made up from floats
[<DataContract>] // for using DataMember on fields
type Polyline2D (points: ResizeArray<Pt>) =

    /// Gets the internal list of all Points of the Polyline2D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline2D.
    [<DataMember>]
    member _.Points =  points

    /// Create a new empty Polyline2D
    new () = Polyline2D(ResizeArray<Pt>())

    /// Create a new empty Polyline2D with predefined capacity for the internal list of points.
    new (capacity:int) = Polyline2D(ResizeArray<Pt>(capacity))

    /// Nicely formatted string representation of the Box including its size.
    override p.ToString() =
        if points.Count = 0 then
            "An empty Euclid.Polyline2D."
        else
            $"Euclid.Polyline2D with length {p.Length}, from {points.Count} points"

    /// Creates a copy of the Polyline2D
    /// Same as polyline.Clone()
    member p.Duplicate(): Polyline2D =
        Polyline2D.createDirectlyUnsafe(points.GetRange(0, points.Count))

    /// Creates a copy of the Polyline2D.
    /// Same as polyline.Duplicate()
    member p.Clone(): Polyline2D =
        Polyline2D.createDirectlyUnsafe(points.GetRange(0, points.Count))

    /// Gets or sets first point of the Polyline2D
    /// This is the point at index 0.
    /// Same as Polyline2D.FirstPoint
    member p.Start
        with get() =
            if points.Count < 1 then failToSmall "Start.get" 1
            points.[0]
        and set(v) =
            if points.Count < 1 then failToSmall "Start.set" 1
            points.[0] <- v

    /// Gets or sets last or end point of the Polyline2D
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline2D.LastPoint
    member p.End
        with get() =
            if points.Count < 1 then failToSmall "End.get" 1
            points.[points.Count - 1]
        and set(v) =
            if points.Count < 1 then failToSmall "End.set" 1
            points.[points.Count - 1] <- v

    /// Gets or sets the last point of the Polyline2D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline2D.End
    member p.LastPoint
        with get() =
            if points.Count < 1 then failToSmall "LastPoint.get" 1
            points.[points.Count - 1]
        and set(v) =
            if points.Count < 1 then failToSmall "LastPoint.set" 1
            points.[points.Count - 1] <- v

    /// Gets or sets the second last point of the Polyline2D.
    member p.SecondLastPoint
        with get() =
            if points.Count < 2 then failToSmall "SecondLastPoint.get" 2
            points.[points.Count - 2]
        and set(v) =
            if points.Count < 2 then failToSmall "SecondLastPoint.set" 2
            points.[points.Count - 2] <- v

    /// Gets or sets the second point of the Polyline2D.
    /// This is the point at index 1.
    member p.SecondPoint
        with get() =
            if points.Count < 2 then failToSmall "SecondPoint.get" 2
            points.[1]
        and set(v) =
            if points.Count < 2 then failToSmall "SecondPoint.set" 2
            points.[1] <- v

    /// Gets or sets the first point of the Polyline2D.
    /// This is the point at index 0.
    /// Same as Polyline2D.Start
    member p.FirstPoint
        with get() =
            if points.Count < 1 then failToSmall "FirstPoint.get" 1
            points.[0]
        and set(v) =
            if points.Count < 1 then failToSmall "FirstPoint.set" 1
            points.[0] <- v


    /// Gets the count of points in the Polyline2D
    member p.PointCount =
        points.Count

    /// Gets the count of segments in the Polyline2D
    /// This is poly.Points.Count - 1
    member p.SegmentCount =
        max 0 (points.Count - 1 )

    /// Gets the index of the last point in the Polyline2D.
    /// points.Count - 1
    member p.LastPointIndex =
        points.Count - 1

    /// Gets the index of the last segment in the Polyline2D.
    /// This is poly.Points.Count - 2
    member p.LastSegmentIndex =
        points.Count - 2

    /// Gets the length of the Polyline2D
    /// Returns 0.0 if there are less than 2 points.
    member p.Length =
        let mutable l = 0.0
        if points.Count > 1 then
            let mutable prev = points.[0]
            for i = 1 to points.Count-1 do
                let t = points.[i]
                l <- l + Pt.distance prev t
                prev <- t
        l

    /// Gets the segment at index i of the Polyline2D.
    member p.GetSegment(i:int) =
        if i < 0 || i > points.Count - 2 then
            EuclidException.Raisef "Euclid.Polyline2D.GetSegment: index %d is out of range for Polyline2D with %d points." i points.Count
        Line2D(points.[i], points.[i+1])

    /// Gets the segment at index i of the Polyline2D.
    member p.LastSegment =
        if points.Count < 2 then failToSmall "LastSegment" 2
        let i = points.Count - 1
        Line2D(points.[i-1], points.[i])

    /// Gets the first segment of the Polyline2D.
    member p.FirstSegment =
        if points.Count < 2 then failToSmall "FirstSegment" 2
        Line2D(points.[0], points.[1])

    /// Returns all segments of the Polyline2D as a list of Line2D.
    member p.Segments =
        let lns = ResizeArray(p.SegmentCount)
        let pts = points
        if pts.Count < 2 then
            lns
        else
            let mutable a = pts.[0]
            for i = 1 to points.LastIndex do
                let b = pts.[i]
                lns.Add(Line2D(a, b))
                a <- b
            lns

    /// Gets bounding rectangle of the Polyline2D
    member p.BoundingRectangle =
        BRect.createFromIList points

    /// Tests if Polyline2D start and end points are exactly the same.
    /// Fails if the Polyline3D has less than 3 points.
    member p.IsClosed =
        if points.Count < 3 then failToSmall "IsClosed" 3
        let v = p.Start  - p.End
        v.IsZero

    /// Tests if Polyline2D is closed within given tolerance.
    /// Fails if the Polyline3D has less than 3 points.
    member p.IsAlmostClosed(tolerance) =
        if points.Count < 3 then  failToSmall "IsAlmostClosed" 3
        let v = p.Start  - p.End
        v.LengthSq < tolerance*tolerance

    /// Reverse order of the Polyline2D in place.
    member p.ReverseInPlace() =
        points.Reverse()

    /// Returns new Polyline2D in reversed Order.
    member p.Reverse () =
        let n = p.Duplicate()
        n.Points.Reverse()
        n

    /// Close the Polyline2D if it is not already closed.
    /// If the ends are closer than the tolerance. The last point is set to equal the first point.
    /// Else the start point is added to the end of the Polyline2D.
    member p.CloseIfOpen(toleranceForAddingPoint) =
        if points.Count < 3 then  failToSmall "CloseIfOpen" 3
        let v = p.Start  - p.End
        if v.LengthSq < toleranceForAddingPoint*toleranceForAddingPoint then
            points.Last <- p.Start
        else
            points.Add p.Start

    /// The signed area of the Polyline2D .
    /// If it is positive the Polyline2D is Counter Clockwise.
    /// Polyline does not need to be exactly closed. But then result might be wrong. Or without meaning.
    /// For self intersecting Polylines the result is also invalid.
    member p.SignedArea =
        //https://helloacm.com/sign-area-of-irregular-polygon/

        let mutable area = 0.0
        let mutable t = points.Last // calculate from last to first too
        for i=0 to points.Count-1 do
            let n = points.[i]
            let a = t.X - n.X
            let b = n.Y + t.Y
            area <- area + (a * b)
            t <- n
        area * 0.5

    /// The area of the Polyline2D.
    /// Fails if Polyline is not exactly closed.
    /// For self intersecting Polylines the result is invalid.
    member p.Area =
        if not p.IsClosed then EuclidException.Raisef "Euclid.Polyline2D.Area failed on Polyline2D that is not exactly closed %O" p
        abs(p.SignedArea)

    /// Test if Polyline2D is CounterClockwise.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated.
    /// If it is positive the Polyline2D is Counter Clockwise.
    member p.IsCounterClockwise =
        let  area = p.SignedArea
        if   abs(area) < UtilEuclid.zeroLengthTolerance then EuclidException.Raisef "Euclid.Polyline2D.IsCounterClockwise: Polyline2D the area is zero: %O" p
        else area > 0.0


    /// Test if Polyline2D is Clockwise.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated.
    /// If it is negative the Polyline2D is Clockwise.
    member p.IsClockwise =
        let  area = p.SignedArea
        if   abs(area) < UtilEuclid.zeroLengthTolerance then EuclidException.Raisef "Euclid.Polyline2D.IsClockwise: Polyline2D the area is zero: %O" p
        else area < 0.0


    /// Returns the point at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    /// If the parameter is within 1e-6 of an integer value, the integer value is used as parameter.
    member pl.EvaluateAt(t:float) =
        let i = int t       // integer part of the parameter
        let p = t - float i // fractional part of the parameter
        if   i < -1 then
            EuclidException.Raisef "Euclid.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t

        elif i >= pl.Points.Count then
            EuclidException.Raisef "Euclid.Polyline2D.EvaluateAt: Parameter %f is more than point count(%d) - 1." t pl.Points.Count

        // handle the case where the value is just below 0.0:
        elif i = -1 then
            if p > 0.99999 then pl.Points.First
            else EuclidException.Raisef "Euclid.Polyline2D.EvaluateAt: Parameter %f is less than 0.0" t

        // handle the case where the value is just above point count - 1:
        elif i = pl.Points.Count - 1  then
            if p < 0.00001 then pl.Points.Last
            else EuclidException.Raisef "Euclid.Polyline2D.EvaluateAt: Parameter %f is more than point count(%d) - 1." t pl.Points.Count

        // return point if point is almost matching and integer
        elif p < 0.000001 then
            pl.Points.[i]
        elif p > 0.999999 then
            pl.Points.[i+1]
        else
            let t = pl.Points.[i]
            let v = pl.Points.[i+1] - t
            t + v * p

    /// Returns the Unitized Tangent at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.TangentAt(t:float) =
        let i = int t
        let p = t - float i
        if  i < -1 then
            EuclidException.Raisef "Euclid.Polyline2D.TangentAt: Parameter %f is less than 0.0" t
        elif i > pl.Points.Count then
            EuclidException.Raisef "Euclid.Polyline2D.TangentAt: Parameter %f is more than point count(%d)." t pl.Points.Count
        elif i = -1 then
            if p > 0.9999 then UnitVc.create(pl.Points.First, pl.Points.Second)
            else EuclidException.Raisef "Euclid.Polyline2D.TangentAt: Parameter %f is less than 0.0" t
        elif i = pl.Points.Count then
            if   p > 1e-4 then  EuclidException.Raisef "Euclid.Polyline2D.TangentAt: Parameter %f is more than point count(%d)." t pl.Points.Count
            else UnitVc.create(pl.Points.SecondLast, pl.Points.Last)
        // return point  if point is almost matching
        else
            UnitVc.create(pl.Points.[i], pl.Points.[i+1])


    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.ClosestParameter(pt:Pt) =
        // for very large polylines, this is could be optimized by using search R-tree
        let points = pl.Points
        if points.Count < 2 then  failToSmall "ClosestParameter" 2
        // vectors of the segments
        let vs = Array.zeroCreate (points.Count-1)
        for i = 0 to vs.Length-1 do
            let ti = points[i]
            let ne = points[i+1]
            vs[i] <- ne-ti

        // closest parameters  of the segments
        let ts = Array.zeroCreate (points.Count-1)
        for i = 0 to ts.Length-1 do
            let p = points[i]
            let v = vs[i]
            // finding ClosestParameter on line segment and clamp to 0.0 to 1.0
            let len = v.LengthSq
            ts[i] <- if len < 1e-9 then 0.0 else -((p-pt) *** v) / len |> UtilEuclid.clampBetweenZeroAndOne //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html

        // square distances per segment
        let ds = Array.zeroCreate (points.Count-1)
        for i = 0 to ds.Length-1 do
            let p = points[i]
            let v = vs[i]
            let t = ts[i]
            ds[i] <- Pt.distanceSq pt (p + v*t)

        let i = Arr.minIndex ds
        let t = ts.[i]
        float i + t

    /// Returns the point on the Polyline2D that is the closest point to the given point.
    member pl.ClosestPoint(pt:Pt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    member pl.DistanceTo(pt:Pt) =
        let t = pl.ClosestParameter pt
        pl.EvaluateAt t
        |> Pt.distance pt


    /// If it is 0 then point is outside of the Loop
    /// The first and last point of the Polyline2D need to be identical for correct results
    member pl.WindingNumber (point:Pt) : int =
        // from https://github.com/FreyaHolmer/Mathfs/blob/master/Runtime/Geometric%20Shapes/Polygon.cs#L92
        // https://x.com/FreyaHolmer/status/1232826293902888960
        // or use ? https://github.com/blenderfan/AdvancedGamedevTutorials/blob/main/AdvancedGamedev-WindingNumbers/Polygon2D.cs
        // https://www.youtube.com/watch?v=E51LrZQuuPE
        let px = point.X
        let py = point.Y
        let isLeft (aa:Pt) (bb:Pt)  =
            let ax = px - aa.X // cross product unrolled to avoid allocation of temp Vc
            let ay = py - aa.Y
            let bx = bb.X - aa.X
            let by = bb.Y - aa.Y
            let det = ax * by - ay * bx
            if   det >  1e-12 then  1
            elif det < -1e-12 then -1
            else 0

        let mutable winding = 0
        let pts = pl.Points
        if pts.Count > 0 then
            let mutable this = pts.[0]
            for i = 1 to pts.LastIndex do
                let next = pts.[i]
                if this.Y <= py then
                    if next.Y > py && isLeft this next  > 0 then
                        winding <- winding - 1
                else
                    if next.Y <= py && isLeft this next  < 0 then
                        winding <- winding + 1
                this <- next

        winding

    /// The first and last point of the Polyline2D need to be identical for correct results
    /// Uses the WindingNumber to determine if the point is inside the Polyline2D.
    member pl.Contains (point:Pt) : bool =
        pl.WindingNumber point <> 0

    /// Returns the average center of all points of the Polyline2D.
    member p.Center =
        if points.Count = 0 then failToSmall "Center" 1
        let mutable x = 0.0
        let mutable y = 0.0
        for i = 0 to points.LastIndex do
            let p = points.[i]
            x <- x + p.X
            y <- y + p.Y
        Pt(x / float points.Count, y / float points.Count)


    /// Scales the 2D polyline by a given factor.
    /// Scale center is World Origin 0,0
    member p.Scale (factor:float) : Polyline2D =
        points
        |> ResizeArr.map (fun pt -> pt * factor)
        |> Polyline2D.createDirectlyUnsafe


    /// Scales the 2D polyline by a given factor on a given center point
    member p.ScaleOn (cen:Pt) (factor:float) : Polyline2D =
        let cx = cen.X
        let cy = cen.Y
        points
        |> ResizeArr.map (fun pt ->
            Pt( cx + (pt.X - cx) * factor,
                cy + (pt.Y - cy) * factor)
            )
        |> Polyline2D.createDirectlyUnsafe


    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Gets the internal list of all Points of the Polyline2D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline2D.
    static member pointsUnsafeInternal (p:Polyline2D) =
        p.Points

    /// Gets first point of the Polyline2D
    static member start (p:Polyline2D) =
        let points = p.Points
        if points.Count < 1 then failToSmall "start" 1
        points.[0]

    /// Gets last or end point of the Polyline2D
    static member ende (p:Polyline2D) =
        let points = p.Points
        if points.Count < 1 then failToSmall "ende" 1
        points.[ points.Count - 1 ]

    /// Reverse order of the Polyline2D in place.
    static member reverseInPlace (p:Polyline2D) =
        p.ReverseInPlace()

    /// Returns new Polyline2D in reversed Order.
    static member reverse (p:Polyline2D) =
        p.Reverse()

    /// Returns the point at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at point count.
    static member evaluateAt (t:float) (pl:Polyline2D) =
        pl.EvaluateAt t

    /// Apply a mapping function to each point in the 2D Polyline2D. Returns new Polyline2D.
    static member map (mapping:Pt->Pt) (pl:Polyline2D) =
        pl.Points |> ResizeArr.map mapping |> Polyline2D.createDirectlyUnsafe

    /// Move a Polyline2D by a vector. (same as Polyline2D.move)
    static member translate (v:Vc) (pl:Polyline2D) =
        pl |> Polyline2D.map (Pt.addVc v)

    /// Move a Polyline2D by a vector. (same as Polyline2D.translate)
    static member move (v:Vc) (pl:Polyline2D) =
        Polyline2D.translate v pl

    /// Returns a Polyline2D moved by a given distance in X direction.
    static member moveX (distance:float) (pl:Polyline2D) =
        pl |> Polyline2D.map (Pt.moveX distance)

    /// Returns a Polyline2D moved by a given distance in Y direction.
    static member moveY (distance:float) (pl:Polyline2D) =
        pl |> Polyline2D.map (Pt.moveY distance)


    /// Scales the Polyline2D by a given factor.
    /// Scale center is World Origin 0,0
    /// Returns a new Polyline2D.
    static member scale (factor:float) (pl:Polyline2D) : Polyline2D =
        pl |> Polyline2D.map (fun pt -> pt * factor)


    /// Rotation a Polyline2D around Z-Axis.
    static member rotate (r:Rotation2D) (pl:Polyline2D) =
        pl |> Polyline2D.map (Pt.rotateBy r)

    /// Rotation a Polyline2D round given Center point an a local Z-axis.
    static member rotateWithCenter (cen:Pt) (r:Rotation2D) (pl:Polyline2D) =
        pl |> Polyline2D.map (Pt.rotateWithCenterBy cen r)

    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at point count.
    static member closestParameter (pl:Polyline2D) (pt:Pt) =
        pl.ClosestParameter pt

    /// Returns the point on the Polyline2D that is the closest point to the given point.
    static member closestPoint (pl:Polyline2D) (pt:Pt) =
        pl.ClosestPoint pt

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    static member distanceTo (pl:Polyline2D) (pt:Pt) =
        pl.DistanceTo pt

    /// Create a new Polyline2D by copying over all points.
    static member create(points: seq<Pt>) =
        Polyline2D(ResizeArray(points))

    /// Create a new Polyline2D by using the provided ResizeArray directly.
    /// All later changes to the ResizeArray will be reflected in the Polyline2D.
    static member createDirectlyUnsafe (points: ResizeArray<Pt>) =
        Polyline2D(points)

    /// Create a new empty Polyline2D without any points.
    /// But predefined capacity.
    static member createEmpty (capacity:int) =
        Polyline2D(ResizeArray(capacity))


    /// Returns new Polyline2D from point at Parameter a to point at Parameter b.
    /// if 'a' is bigger 'b' then the new Polyline2D is in opposite direction.
    /// If a parameter is within 1e-4 of an integer value, the integer value is used as parameter.
    static member subPolyline a b (pl:Polyline2D) :Polyline2D =
        let rev = a>b
        let u, v = if rev then b, a else a, b
        let np = Polyline2D.createEmpty (int(v-u)+2)
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


    [<Obsolete("Renamed to Polyline2D.subPolyline")>]
    static member segment a b (pl:Polyline2D) :Polyline2D =
        Polyline2D.subPolyline a b pl

    /// Returns a new closed Polyline2D.
    /// If the first and last point are within 1e-6 of each other, the last point is set equal to the first point.
    /// Otherwise one point is added.
    static member close (pl:Polyline2D) =
        if pl.Points.Count < 2 then failToSmall "close" 2
        let ps = pl.Points
        let np = Polyline2D.createEmpty (ps.Count + 1)
        np.Points.AddRange(ps.GetRange(0, ps.Count))
        if Pt.distanceSq ps.First ps.Last < 1e-12 then
            np.Points.[np.Points.Count-1] <- np.Points.First // set last point equal to first
        else
            np.Points.Add np.Points.First
        np


    /// Closes the Polyline2D in place by adding a point.
    /// If the first and last point are within 1e-6 of each other, the last point is set equal to the first point instead.
    static member closeInPlace (pl:Polyline2D) =
        if pl.Points.Count < 2 then failToSmall "closeInPlace" 2
        let points = pl.Points
        if Pt.distanceSq points.First points.Last < 1e-12 then
            points.[points.Count-1] <- points.First
        else
            points.Add points.First

    /// Tests if two Polyline2D have the same number of points and points are equal within a given tolerance.
    static member equals tol (a:Polyline2D) (b:Polyline2D) =
        let k = a.PointCount
        if k <> b.PointCount then
            false
        else
            let mutable i = 0
            let mutable same = true
            let aPts = a.Points
            let bPts = b.Points
            while i < k && same do
                if Pt.equals tol aPts.[i] bPts.[i] then
                    i <- i + 1
                else
                    same <- false
            same

    //--------------------------------------------------------------------------------
    //------------------------Offset------------------------------------
    //--------------------------------------------------------------------------------

    /// Returns the index of an outer corner and
    /// a Cross Product as normal vector corresponding to an Counter-Clockwise view on the loop.
    /// In 2D the Cross Product is the signed area of the parallelogram spanned by the two vectors. So just a scalar. Not a vector
    /// This is used to calculate the RefNormal vector for the offset function.
    /// The input vectors are the vectors for each segment of the polyline.
    /// From first and second point up to last and first point.
    /// The endIndexChange is to skip the last point if first and last are the same. ( then use -1)
    static member internal findOuterCornerAndRefNormal(pts:ResizeArray<Pt>, vs:Vc[], endIndexChange) : int*float =
        if pts.Count + endIndexChange <> vs.Length then
            EuclidException.Raisef "Euclid.Polyline2D.findOuterCornerAndRefNormal pts (%d) and vs(%d) must have the same length." pts.Count vs.Length
        let us = Array.zeroCreate vs.Length
        // mark very short segments with 0, 0, 0:
        for i=0 to vs.Length-1 do
            let v = vs.[i]
            let l = v.Length
            if l > 1e-6 then
                let f = 1. / l
                us.[i] <- UnitVc.createUnchecked(v.X*f, v.Y*f)
            //else  us.[i] <- Vc.Zero //happens anyway by zeroCreate

        let ref = Points.getSignedArea(pts)
        let mutable posAngSum = 0.0
        let mutable negAngSum = 0.0
        let mutable posIdx = -1
        let mutable negIdx = -1
        let isNotZero (v:UnitVc) = v.X<>0. || v.Y<>0.

        // get angle sums
        let prevUIdx = us|> Array.findIndexBack isNotZero
        let mutable prevU = us.[prevUIdx]
        let len = us.Length
        let deg2 = Math.PI / 90. // 2 degree
        let rec loop(i) =
            if i < len then
                let thisU = us.[i]
                if isNotZero thisU then
                    let ang = UnitVc.anglePi thisU prevU
                    if ang < deg2 then // parallel point, loop on
                        loop(i+1)
                    else
                        let c = UnitVc.cross(prevU, thisU)
                        if c*ref>0.0 then
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
    /// The offset distances are given for each segment of the polyline. offDi:IList must have the same length as the polyline or be a singleton
    /// Start point and end point may not be equal, all arrays must be of the same length..
    /// The 'referenceOrient' corresponds to the Cross Product of two segment of the polyline at an outside corner.
    /// If 'referenceOrient' is given as 0.0 then the algorithm tries to detect if the loop is clockwise or counterclockwise
    /// it also tries to find an outer corner on non convex polylines.
    /// If 'referenceOrient' is bigger than 0.0 a counter-clock-wise loop is assumed and the detection part is skipped.
    /// If 'referenceOrient' is smaller than 0.0 a clock-wise loop is assumed and the detection part is skipped.
    /// The endIndexChange is to skip the last point if first and last are the same. ( then use -1)
    static member internal offsetCore(  pts:ResizeArray<Pt>,
                                        offDi:IList<float>,
                                        referenceOrient:float,
                                        endIndexChange:int ) : ResizeArray<Pt> =
        let offCount = offDi.Count
        if offCount <> 1 // case: just one offset distance for all segments
            &&  offCount <> pts.Count + endIndexChange //case: first and last points are the same.
            &&  offCount <> pts.Count - 1 //case: open polyline and, no looping desired:
            then
                EuclidException.Raisef "Euclid.Polyline2D.offsetCore pts(%d) and offD(%d) must have the same length or offD must be of length 1." pts.Count offDi.Count

        let lenTolSq = 1e-12 //local squared length tolerance
        let lenPts = pts.Count + endIndexChange // The endIndexChange is to skip the last point if first and last are the same. ( then value is -1 )
        let lastIdx = lenPts - 1

        // Gets the offset distance a given segment.
        // this function to allow a single distance to be used everywhere
        let inline getOffDist i =
            if offCount = 1 then offDi[0]
            elif offCount = i then offDi[offDi.Count-1] //0.0  //case: open polyline and, no looping desired:
            else offDi[i]

        // (1) precomputed array of vectors going from this to next
        // no checking for zero length is needed here, will be done later
        let vs = Array.zeroCreate lenPts
        let mutable this = pts.[0]
        for i=1 to lastIdx do
            let next = pts.[i]
            vs.[i-1] <- next-this // fills index 0 to lastIdx-1
            this <- next
        vs.[lastIdx] <- pts.[0]-pts[lastIdx] // fills last item

        let refNorm :float =
            if referenceOrient = 0.0 then Polyline2D.findOuterCornerAndRefNormal(pts, vs, endIndexChange) |> snd
            else referenceOrient

        // (2) main loop
        // create error and result arrays
        let colinear: bool[] = Array.zeroCreate lenPts
        let res = pts.GetRange(0, lenPts) // copy the input points

        //(2.1) find last valid vector
        let prevVIdx =
            let mutable li = vs.Length - 1
            let mutable v = vs.[li]
            while v.LengthSq < lenTolSq do
                li <- li - 1
                if li = -1 then
                    EuclidException.Raisef "Euclid.Polyline2D.offsetCore: invalid Polyline2D, all %d points are in the same location" pts.Count
                v <- vs.[li]
            li

        // set last points a colinear if prevVIdx is not the index of the last point
        for i=prevVIdx + 1 to lastIdx do
            colinear.[i] <- true

        let mutable prevV = vs.[prevVIdx]
        let mutable prevOff = getOffDist(prevVIdx)

        //(2.2) looping
        let inline rot90Len (v:Vc) (sqLen:float) (off:float) = // rotate 90 degrees counter-clockwise and sets length to offset length
            let x = -v.Y // rotate 90 degrees counter-clockwise
            let y = v.X  // rotate 90 degrees counter-clockwise
            let f = off / sqrt sqLen
            Vc(x*f, y*f)

        for i=0 to lastIdx do
            let thisOff = getOffDist(i)
            let nextV = vs.[i]
            if abs(thisOff) < 1e-9 && abs(prevOff) < 1e-9 then
                // offset distance is zero for previous and this segment,
                // just copy the point
                res.[i] <- pts.[i]
            else
                let ax = prevV.X
                let ay = prevV.Y
                let bx = nextV.X
                let by = nextV.Y
                let prevLenSq = ax*ax + ay*ay // square length of prevV
                let nextLenSq = bx*bx + by*by // square length of nextV
                if nextLenSq < lenTolSq then
                    colinear.[i] <- true
                //elif a < lenTolSq then failwithf "too short segment not recognized. This should already be checked!"
                else
                    let b = ax*bx + ay*by  // dot product of both lines
                    let ac = prevLenSq*nextLenSq // square of square length, never negative
                    let bb = b*b // square of square dot product, never negative
                    let discriminant = ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
                    let div = ac + bb // never negative
                    let rel = discriminant / div
                    if rel < float RelAngleDiscriminant.``0.25`` then // parallel or identical points, use previous offset
                        colinear.[i] <- true
                    else
                        /// Check with the orientation with the reference normal
                        /// In 2D the Cross Product vector is the signed area of the parallelogram spanned by the two vectors.
                        /// So just a scalar. Not actually a vector
                        let n = Vc.cross(prevV, nextV) |> matchSign refNorm
                        let thisPt = pts.[i]
                        let prevShift = rot90Len prevV prevLenSq (if n>0. then prevOff else -prevOff)
                        let nextShift = rot90Len nextV nextLenSq (if n>0. then thisOff else -thisOff)
                        let offP = thisPt + prevShift
                        let offN = thisPt + nextShift
                        let vx = offN.X - offP.X
                        let vy = offN.Y - offP.Y
                        let e = bx*vx + by*vy
                        let d = ax*vx + ay*vy
                        let t = (nextLenSq * d - b * e) / discriminant
                        res.[i] <- offP + t * prevV

            prevOff <- thisOff
            prevV <- nextV

        // (3.5) correct colinear and identical points by evaluation the closest valid segment.
        for i = 0 to colinear.Length - 1 do
            if colinear.[i]  then
                // search backwards for first non-colinear point:
                let len = colinear.Length
                let mutable prevIdx = saveIdx (i-1) len
                while colinear.[prevIdx] && prevIdx<>i do // search backwards for first non-colinear point
                    prevIdx <- saveIdx (prevIdx-1) len

                if prevIdx = i then //  same index reached from searchBack : all points for offset are colinear :
                    let v = vs.[prevVIdx]
                    for ii = 0 to colinear.Length - 1 do
                        colinear.[ii] <- false // reset to not loop on
                        let offDist = getOffDist ii
                        let shift = v.Rotate90CCW |> Vc.withLength (if refNorm > 0.0 then offDist else -offDist)
                        res.[ii] <- pts.[ii] + shift
                else
                    // search forward for first non-colinear point:
                    let mutable nextIdx =  saveIdx (i+1) len
                    while colinear.[nextIdx] && nextIdx<>i do // search forward for first non-colinear point
                        nextIdx <- saveIdx (nextIdx+1) colinear.Length

                    let rln = Line2D(res.[prevIdx], res.[nextIdx])
                    let pln = Line2D(pts.[prevIdx], pts.[nextIdx])
                    // iterate over colinear points between the valid ones:
                    for ii = prevIdx+1 to nextIdx-1 do
                        colinear.[ii] <- false // reset to not loop on
                        let p = pts.[ii]
                        let t = pln.ClosestParameter p // parameter on the line segment
                        res.[ii] <- rln.EvaluateAt t // evaluate the equal parameter on offset segment
                        // TODO add safety check? could be omitted. offset is then averaged out.
                        // if not allowObliqueOffsetOnColinearSegments && abs (getOffDist(prevIdx) - getOffDist(nextIdx)) > 1e-9 then
                        //     EuclidException.Raisef "Euclid.Polyline2D.offsetCore: can't offset collinear segment at index %d with different offset distances from index %d and %d{nl}these distances are not the same: %f and %f" i prevIdx nextIdx (getOffDist prevIdx) (getOffDist nextIdx)

        res


    /// <summary>Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// Does not fail on colinear or duplicate points.</summary>
    /// <param name="polyLine"> A 2D Polyline. </param>
    /// <param name="offsetDistances">The parallel offset distances for each segment of the polyline.
    /// A positive distance offsets inwards in corners, a negative offset outwards.
    /// For open and closed polylines this list of distances must have one item less than number of points in the polyline.
    /// Except if the polyline is open and the loop parameter is set to true. Then points and distances list shall have the same count.
    /// A singleton for constant offset is allowed too.
    /// </param>
    /// <param name="loop">bool, Optional. Consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="referenceOrient">float, Optional.
    /// This is to control on which side of the polyline the offset is created.
    /// By default the offset is created to the inside of the polygon / polyline.
    /// The algorithm tries to detect if the loop is clockwise or counterclockwise. But this cannot always be detected.
    /// For example on self intersecting polylines.
    /// If 'referenceOrient' is bigger than 0.0 a counterclockwise loop is assumed and the detection part is skipped.
    /// If 'referenceOrient' is smaller than 0.0 a clockwise loop is assumed and the detection part is skipped.
    /// The 'referenceOrient' corresponds to the Cross Product of two segment of the polyline an an convex corner.
    /// If 'referenceOrient' is 0.0 or omitted then the algorithm tries to detect if the loop is clockwise or counterclockwise.
    /// It also tries to find a convex corner on a convex and concave polyline.
    /// If the given referenceOrient does not correspond to the looping order the offset will be to the other side.</param>
    /// <returns>A new 2D polyline with the same amount of points.</returns>
    static member offset(   polyLine:Polyline2D,
                            offsetDistances: float IList,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(0.0)>]   referenceOrient:float
                            ) : Polyline2D =
        let points = polyLine.Points
        // (1) Fail if polyline has less than one point
        if points.Count < 2 then
            EuclidException.Raisef "Euclid.Polyline2D.offset needs at least two points but %O given." polyLine

        let checkDistanceCount len  =
            if isNull offsetDistances then Error "offsetDistances is null"
            elif offsetDistances.Count = len then Ok()
            elif offsetDistances.Count = 1   then Ok()
            else Error $"offsetDistances has {offsetDistances.Count} items"


        // (2) check if last and first point are the same and if so skip last point via endIndexChange = -1
        if Pt.distanceSq polyLine.Start polyLine.End < 1e-12 then // sqrt of 1e-6, auto detect closed polyline points  then
            match checkDistanceCount (points.Count - 1) with
            |Error k  -> EuclidException.Raise $"Euclid.Polyline2D.offset: {k} but should have 1 or {(points.Count-1)} for {points.Count} given points. {Format.nl}In closed Polyline2D with identical start and end points:{Format.nl}{polyLine}"
            |Ok _ -> ()

            // use endIndexChange = -1 to skip the last point, it will then be re added at the end.
            let res = Polyline2D.offsetCore(points, offsetDistances, referenceOrient, endIndexChange = -1)
            res.Add(res.[0])  // set last equal first, it was skipped with 'endIndexChange = -1'
            Polyline2D.createDirectlyUnsafe res

        // (3) check if open but looping desired
        elif loop then
            match checkDistanceCount points.Count with
            |Error k  -> EuclidException.Raise $"Euclid.Polyline2D.offset: {k} but should have 1 or {points.Count} for {points.Count} given points. {Format.nl}In open Polyline2D with start and end apart and loop set to TRUE :{Format.nl}{polyLine}"
            |Ok _ -> ()
            Polyline2D.offsetCore(points, offsetDistances, referenceOrient, endIndexChange = 0)
            |> Polyline2D.createDirectlyUnsafe

        // (4) open polyline and, no looping desired: (one distance value will be added, in getOffDist in offsetCore, )
        else
            match checkDistanceCount (points.Count-1) with
            |Error k  -> EuclidException.Raise $"Euclid.Polyline2D.offset: {k} but should have 1 or {points.Count} for {points.Count} given points. {Format.nl}In open Polyline2D with start and end apart and loop set to FALSE :{Format.nl}{polyLine}"
            |Ok _ -> ()

            let res = Polyline2D.offsetCore(points, offsetDistances, referenceOrient, endIndexChange = 0)
            // (4.1) fix ends if not looped
            if not loop then
                let firstLn = Line2D(points.[0], points.[1])
                let firstV = res.[0]  - firstLn.ClosestPointInfinite res.[0]
                res.[0]  <- points.[0] + firstV
                let lastIdx = res.Count - 1
                let lastLn = Line2D(points.[lastIdx-1], points.[lastIdx])
                let lastV = res.[lastIdx]  - lastLn.ClosestPointInfinite res.[lastIdx]
                res.[lastIdx]  <- points.[lastIdx] + lastV

            Polyline2D.createDirectlyUnsafe res

    /// <summary>Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// Does not fail on colinear or duplicate points.</summary>
    /// <param name="polyLine"> A 2D Polyline. </param>
    /// <param name="offsetDistance">The offset distance for all segments of the polyline.
    /// A positive distance offsets inwards in corners, a negative offset outwards.</param>
    /// <param name="loop">Consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="referenceOrient">float, Optional.
    /// This is to control on which side of the polyline the offset is created.
    /// By default the offset is created to the inside of the polygon / polyline.
    /// The algorithm tries to detect if the loop is clockwise or counterclockwise. But this cannot always be detected.
    /// For example on self intersecting polylines.
    /// If 'referenceOrient' is bigger than 0.0 a counterclockwise loop is assumed and the detection part is skipped.
    /// If 'referenceOrient' is smaller than 0.0 a clockwise loop is assumed and the detection part is skipped.
    /// The 'referenceOrient' corresponds to the Cross Product of two segment of the polyline an an convex corner.
    /// If 'referenceOrient' is 0.0 or omitted then the algorithm tries to detect if the loop is clockwise or counterclockwise.
    /// It also tries to find a convex corner on a convex and concave polyline.
    /// If the given referenceOrient does not correspond to the looping order the offset will be to the other side.</param>
    /// <returns>A new 2D polyline with the same amount of points.</returns>
    static member offset(   polyLine:Polyline2D,
                            offsetDistance: float,
                            [<OPT;DEF(false)>]loop:bool,
                            [<OPT;DEF(0.0)>] referenceOrient:float) : Polyline2D =
        Polyline2D.offset(polyLine, [|offsetDistance|], loop, referenceOrient)


(*


    /// <summary>Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// Does not fail on colinear or duplicate points.</summary>
    /// <param name="polyLine"> A 2D Polyline. </param>
    /// <param name="offsetDistances">The parallel offset distances for each segment of the polyline.
    /// A positive distance offsets inwards in corners, a negative offset outwards.
    /// For open and closed polylines this list of distances must have one item less than number of points in the polyline.
    /// Except if the polyline is open and the loop parameter is set to true. Then points and distances list shall have the same count.
    /// A singleton for constant offset is allowed too.
    /// </param>
    /// <param name="loop">bool, Optional. Consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="referenceOrient">float, Optional.
    /// This is to control on which side of the polyline the offset is created.
    /// By default the offset is created to the inside of the polygon / polyline.
    /// The algorithm tries to detect if the loop is clockwise or counterclockwise. But this cannot always be detected.
    /// For example on self intersecting polylines.
    /// If 'referenceOrient' is bigger than 0.0 a counterclockwise loop is assumed and the detection part is skipped.
    /// If 'referenceOrient' is smaller than 0.0 a clockwise loop is assumed and the detection part is skipped.
    /// The 'referenceOrient' corresponds to the Cross Product of two segment of the polyline an an convex corner.
    /// If 'referenceOrient' is 0.0 or omitted then the algorithm tries to detect if the loop is clockwise or counterclockwise.
    /// It also tries to find a convex corner on a convex and concave polyline.
    /// If the given referenceOrient does not correspond to the looping order the offset will be to the other side.</param>
    /// <param name="obliqueOffsets">bool, Optional. When two adjacent segments are colinear but have different offset distances
    /// there is no solution with a parallel offset.
    /// By default an exception is raised. Set this to true to create and averaged oblique offset instead of failing.</param>
    /// <returns>A new 2D polyline with the same amount of points.</returns>
    static member offset(   polyLine:Polyline2D,
                            offsetDistances: float IList,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(0.0)>]   referenceOrient:float,
                            [<OPT;DEF(false)>] obliqueOffsets:bool
                            ) : Polyline2D =
        let points = polyLine.Points
        // (1) Fail if polyline has less than one point
        if points.Count < 2 then
            EuclidException.Raisef "Euclid.Polyline2D.offset needs at least two points but %O given." polyLine

        let checkDistanceCount len  =
            if isNull offsetDistances then Error "offsetDistances is null"
            elif offsetDistances.Count = len then Ok()
            elif offsetDistances.Count = 1   then Ok()
            else Error $"offsetDistances has {offsetDistances.Count} items"


        // (2) check if last and first point are the same and if so remove last point (one point needs to be removed from list)
        if Pt.distanceSq polyLine.Start polyLine.End < 1e-12 then // sqrt of 1e-6, auto detect closed polyline points  then
            match checkDistanceCount (points.Count - 1) with
            |Error k  -> EuclidException.Raise $"Euclid.Polyline2D.offset: {k} but should have 1 or {(points.Count-1)} for {points.Count} given points. {Format.nl}In closed Polyline2D with identical start and end points:{Format.nl}{polyLine}"
            |Ok _ -> ()

            // use endIndexChange = -1 to skip the last point, wil then be re added at the end.
            let res = Polyline2D.offsetCore(points, offsetDistances, referenceOrient, fixColinearLooped=true, allowObliqueOffsetOnColinearSegments=obliqueOffsets, endIndexChange = -1)
            res.Add(res.[0])  // set last equal first, it was skipped with 'endIndexChange = -1'
            Polyline2D.createDirectlyUnsafe res

        // (3) check if open but looping desired
        elif loop then
            match checkDistanceCount points.Count with
            |Error k  -> EuclidException.Raise $"Euclid.Polyline2D.offset: {k} but should have 1 or {points.Count} for {points.Count} given points. {Format.nl}In open Polyline2D with start and end apart and loop set to TRUE :{Format.nl}{polyLine}"
            |Ok _ -> ()
            Polyline2D.offsetCore(points, offsetDistances, referenceOrient, fixColinearLooped=true, allowObliqueOffsetOnColinearSegments=obliqueOffsets, endIndexChange = 0)
            |> Polyline2D.createDirectlyUnsafe

        // (4) open polyline and, no looping desired: (one distance value will be added, in getOffDist in offsetCore, )
        else
            match checkDistanceCount (points.Count-1) with
            |Error k  -> EuclidException.Raise $"Euclid.Polyline2D.offset: {k} but should have 1 or {points.Count} for {points.Count} given points. {Format.nl}In open Polyline2D with start and end apart and loop set to FALSE :{Format.nl}{polyLine}"
            |Ok _ -> ()
            let res = Polyline2D.offsetCore(points, offsetDistances, referenceOrient, fixColinearLooped=false, allowObliqueOffsetOnColinearSegments=obliqueOffsets, endIndexChange = 0)

            // (4.1) fix ends if not looped
            if not loop then
                let firstLn = Line2D(points.[0], points.[1])
                let firstV = res.[0]  - firstLn.ClosestPointInfinite res.[0]
                res.[0]  <- points.[0] + firstV
                let lastIdx = res.Count - 1
                let lastLn = Line2D(points.[lastIdx-1], points.[lastIdx])
                let lastV = res.[lastIdx]  - lastLn.ClosestPointInfinite res.[lastIdx]
                res.[lastIdx]  <- points.[lastIdx] + lastV

            Polyline2D.createDirectlyUnsafe res
*)






        // // (3.5) correct colinear points by nearest neighbors that are ok in a loop
        // if fixColinearLooped then
        //     let rec searchBack i =
        //         let ii = saveIdx i colinear.Length
        //         if not colinear.[ii] then ii
        //         elif i < -colinear.Length then
        //             EuclidException.Raise $"Euclid.Polyline2D.offsetCore : all {pts.Count} points for offset are identical or colinear within 0.25 degrees."
        //         else searchBack (i - 1)

        //     let rec  searchForward i =
        //         let ii = saveIdx i colinear.Length
        //         if not colinear.[ii] then ii // no need to check for endless loop here because check is done in searchBack that is called first
        //         else searchForward (i + 1)

        //     for i = 0 to colinear.Length - 1 do
        //         if colinear.[i]  then
        //             let pi = searchBack    (i - 1)
        //             let ni = searchForward (i + 1)
        //             if pi = ni then //  does this ever happen ? it is either all colinear (caught in searchBack) or at least three points are not colinear ??
        //                 EuclidException.Raise $"Euclid.Polyline2D.offsetCore : all {pts.Count} points for offset are identical or colinear within 0.25 degrees."
        //             let ln = Line2D(res.[pi], res.[ni])
        //             res.[i] <- ln.ClosestPointInfinite(pts.[i]) //TODO evaluate at the same domain instead of closes point , to keep points in the corner together.
        //             // TODO add safety check? could be omitted. offset is then averaged out.
        //             if not allowObliqueOffsetOnColinearSegments && abs (getOffDist(pi) - getOffDist(ni)) > 1e-9 then
        //                 EuclidException.Raise $"Euclid.Polyline2D.offsetCore: can't offset collinear segment at index {i} with different offset distances from index {pi} and {ni}{Format.nl}these distances are not the same: {getOffDist(pi)} and {getOffDist(ni)}"

        // else // this els was not there before, bug ?
        //     let rec searchBack i =
        //         if i<0 then -1
        //         elif not colinear.[i] then i
        //         else searchBack (i - 1)

        //     let rec  searchForward i =
        //         if i = colinear.Length  then -1
        //         elif not colinear.[i] then i
        //         else searchForward (i + 1)

        //     for i = 0 to colinear.Length - 1 do
        //         if colinear.[i]  then
        //             let pi = searchBack    (i-1)
        //             let ni = searchForward (i+1)
        //             if pi = ni then //  same index reached from searchBack and searchForward, all points for offset are identical or colinear :
        //                 if offCount = 1 then
        //                     let v = vs.[prevVIdx]
        //                     let offDist = getOffDist(prevVIdx)
        //                     let shift = v.Rotate90CCW |> Vc.withLength (if refNorm > 0.0 then offDist else -offDist)
        //                     for i = 0 to lastIdx do
        //                         res.[i] <- pts.[i] + shift
        //                 else
        //                     EuclidException.Raisef "Euclid.Polyline2D.offsetCore: all %d points for %d different offset distances are identical or colinear within 0.25 degrees." offCount pts.Count


        //             elif pi = -1 then
        //                 // colinear start, get frame
        //                 match Points.offsetInCornerEx2D (pts[ni-1], pts[ni], pts[ni+1],  getOffDist(ni-1),  getOffDist(ni), refNorm) with
        //                 |ValueNone -> EuclidException.Raisef "Euclid.Polyline2D.offsetCore :offsetInCornerEx-1 failed unexpectedly."
        //                 |ValueSome (_, prevShift, _) -> res.[i] <- pts.[i] + prevShift

        //             elif ni = -1 then  // colinear end, get frame
        //                 match Points.offsetInCornerEx2D (pts[pi-1], pts[pi], pts[pi+1],  getOffDist(pi-1),  getOffDist(pi), refNorm) with
        //                 |ValueNone -> EuclidException.Raisef "Euclid.Polyline2D.offsetCore :offsetInCornerEx-1 failed unexpectedly."
        //                 |ValueSome (_, _, nextShift) -> res.[i] <- pts.[i] + nextShift
        //             else
        //                 let ln = Line2D(res.[pi], res.[ni])
        //                 res.[i] <- ln.ClosestPointInfinite(pts.[i])
        //                 // TODO add safety check? could be omitted. offset is then averaged out.
        //                 if not allowObliqueOffsetOnColinearSegments && abs (getOffDist(pi) - getOffDist(ni)) > 1e-9 then
        //                     EuclidException.Raise $"Euclid.Polyline2D.offsetCore: can't offset collinear segment at index {i} with different offset distances from index {pi} and {ni}{Format.nl}these distances are not the same: {getOffDist(pi)} and {getOffDist(ni)}"

