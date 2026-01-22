namespace Euclid

open System
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open System.Collections.Generic
open EuclidErrors
open Euclid.EuclidCollectionUtilities



/// A mutable 3D Polyline.
/// If the last point is the same as the first point, the Polyline3D is closed.
/// The Default constructor uses the provided ResizeArray of points directly,
/// so changes to the list will be reflected in the Polyline3D.
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

    /// Nicely formatted string representation of the Polyline3D including its length.
    override p.ToString() =
        if points.Count = 0 then
            "An empty Euclid.Polyline3D."
        else
            $"Euclid.Polyline3D with length {p.Length}, from {p.Points.Count} points"


    /// Format Polyline3D into string including its length.
    member p.AsString : string =
        if points.Count = 0 then
            "empty Polyline3D."
        elif p.IsClosed then
            $"closed Polyline3D with length {p.Length}, from {points.Count} points"
        else
            $"open Polyline3D with length {p.Length}, from {points.Count} points"


    /// Format this 3D polyline into an F# code string that can be used to recreate the point.
    member p.AsFSharpCode : string =
        let ptsAsCode =
            points
            |> ResizeArr.map _.AsFSharpCode
            |> String.concat "; "
        $"Polyline3D.create [| {ptsAsCode} |]"

    /// Creates a copy of the Polyline3D
    /// Same as polyline.Clone()
    member p.Duplicate(): Polyline3D =
        Polyline3D.createDirectlyUnsafe(points.GetRange(0, points.Count))

    /// Creates a copy of the Polyline3D.
    /// Same as polyline.Duplicate()
    member p.Clone(): Polyline3D =
        Polyline3D.createDirectlyUnsafe(points.GetRange(0, points.Count))

    /// Sets the vertex at given index to the given point.
    /// On a closed Polyline3D, setting the first or last point will set both to the same point.
    member p.SetVertex idx (pt:Pnt) =
        if idx < 0 || idx >= points.Count then
            fail $"Polyline3D.SetVertex: index {idx} is out of range for Polyline3D with {points.Count} points."
        if idx = 0 && p.IsClosed then
            points.[points.LastIndex] <- pt
        elif idx = points.LastIndex && p.IsClosed then
            points.[0] <- pt
        points.[idx] <- pt //do last, otherwise IsClosed check fails


    /// Gets or sets first point of the Polyline3D
    /// This is the point at index 0.
    /// Same as Polyline3D.FirstPoint
    member p.Start
        with get() =
            if points.Count < 1 then failTooFewPoly3D "Start.get" 1 points.Count
            points.[0]
        and set(pt) =
            if points.Count < 1 then failTooFewPoly3D "Start.set" 1 points.Count
            points.[0] <- pt

    /// Gets or sets last or end point of the Polyline3D
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.LastPoint
    member p.End
        with get() =
            if points.Count < 1 then failTooFewPoly3D "End.get" 1 points.Count
            points.[points.Count - 1]
        and set(pt) =
            if points.Count < 1 then failTooFewPoly3D "End.set" 1 points.Count
            points.[points.Count - 1] <- pt

    /// Gets or sets the last point of the Polyline3D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.End
    member p.LastPoint
        with get() =
            if points.Count < 1 then failTooFewPoly3D "LastPoint.get" 1 points.Count
            points.[points.Count - 1]
        and set(pt) =
            if points.Count < 1 then failTooFewPoly3D "LastPoint.set" 1 points.Count
            points.[points.Count - 1] <- pt

    /// Gets or sets the second last point of the Polyline3D.
    member p.SecondLastPoint
        with get() =
            if points.Count < 2 then failTooFewPoly3D "SecondLastPoint.get" 2 points.Count
            points.[points.Count - 2]
        and set(pt) =
            if points.Count < 2 then failTooFewPoly3D "SecondLastPoint.set" 2 points.Count
            points.[points.Count - 2] <- pt

    /// Gets or sets the second point of the Polyline3D.
    /// This is the point at index 1.
    member p.SecondPoint
        with get() =
            if points.Count < 2 then failTooFewPoly3D "SecondPoint.get" 2 points.Count
            points.[1]
        and set(pt) =
            if points.Count < 2 then failTooFewPoly3D "SecondPoint.set" 2 points.Count
            points.[1] <- pt

    /// Gets or sets the first point of the Polyline3D.
    /// This is the point at index 0.
    /// Same as Polyline3D.Start
    member p.FirstPoint
        with get() =
            if points.Count < 1 then failTooFewPoly3D "FirstPoint.get" 1 points.Count
            points.[0]
        and set(pt) =
            if points.Count < 1 then failTooFewPoly3D "FirstPoint.set" 1 points.Count
            points.[0] <- pt

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
    member p.Length : float =
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
            fail $"Polyline3D.GetSegment: index {i} is out of range for Polyline3D with {points.Count} points."
        Line3D(points.[i], points.[i+1])

    /// Gets the last segment of the Polyline3D.
    member p.LastSegment =
        if points.Count < 2 then failTooFewPoly3D "LastSegment" 2 points.Count
        let i = points.Count - 1
        Line3D(points.[i-1], points.[i])

    /// Gets the first segment of the Polyline3D.
    member p.FirstSegment =
        if points.Count < 2 then failTooFewPoly3D "FirstSegment" 2 points.Count
        Line3D(points.[0], points.[1])

    /// Returns all segments of the Polyline3D as a list of Line3D.
    member p.Segments =
        let lns = ResizeArray(p.SegmentCount)
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

    /// Returns the line vectors of all segments of the Polyline3D as a list of Vec.
    member p.SegmentVectors : ResizeArray<Vec> =
        let vs = ResizeArray(p.SegmentCount)
        let pts = points
        if pts.Count < 2 then
            vs
        else
            let mutable a = pts.[0]
            for i = 1 to points.LastIndex do
                let b = pts.[i]
                vs.Add(b-a)
                a <- b
            vs

    /// Gets the a bounding box of the Polyline3D
    member p.BoundingBox =
        BBox.createFromIList points

    /// Tests if Polyline3D start and end points are exactly the same.
    /// Returns False if the Polyline3D has less than 3 points.
    member p.IsClosed =
        points.Count > 2
        &&
        (p.Start  - p.End).IsZero


    /// Tests if Polyline3D is closed within given tolerance.
    /// Returns False if the Polyline3D has less than 3 points.
    member p.IsAlmostClosed tolerance =
        points.Count > 2
        &&
        Pnt.distanceSq p.Start p.End < tolerance*tolerance

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
    member p.CloseInPlace(toleranceForAddingPoint) =
        if points.Count < 3 then failTooFewPoly3D "CloseInPlace" 3 points.Count
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
        if   abs(area) < UtilEuclid.zeroLengthTolerance then
            fail $"Polyline3D.IsCounterClockwiseIn2D: Polyline3D the area is zero: {p}"
        area > 0.0


    /// Test if Polyline3D is Clockwise when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is CCW.
    member p.IsClockwiseIn2D =
        let area = p.SignedAreaIn2D
        if   abs(area) < UtilEuclid.zeroLengthTolerance then
            fail $"Polyline3D.IsClockwiseIn2D: Polyline3D the area is zero: {p}"
        area < 0.0


    /// Returns the point at a given parameter on the Polyline3D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    /// If the parameter is within 1e-6 of an integer value, the integer value is used as parameter.
    member pl.EvaluateAt(t:float) : Pnt =
        let i = int t       // integer part of the parameter
        let p = t - float i // fractional part of the parameter
        let count = pl.Points.Count
        let countF = float count

        // values next to  the start of the polyline:
        if t < 1e-6 then
            if t < -1e-6 then
                fail $"Polyline3D.EvaluateAt: Parameter {t} is less than 0.0"
            pl.Points.First

        // values next to  the end of the polyline:
        elif t > (countF - 1e-6) then
            if t > (countF + 1e-6) then
                fail $"Polyline3D.EvaluateAt: Parameter {t} is more than point count {pl.Points.Count}."
            pl.Points.Last

        // return point if point is almost matching and integer
        elif p < 1e-6 then
            pl.Points.[i]
        elif p > 1.0 - 1e-6 then
            pl.Points.[i+1]
        else
            let t = pl.Points.[i]
            let v = pl.Points.[i+1] - t
            t + v * p

    [<Obsolete("This was semantically unclear, what happens at vertex? Use GetSegment(i).UnitTangent instead")>]
    member pl.TangentAt(_t:float) =
        fail "Polyline3D.TangentAt is obsolete, use GetSegment(i).UnitTangent instead." |> unbox // unbox to make type checker happy



    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.ClosestParameter(p:Pnt) =
        let pts = pl.Points
        if pts.IsEmpty then  fail "Polyline3D.ClosestParameter failed on empty Polyline3D"
        let mutable a = pts[0]
        let mutable minT = 0.0
        let mutable seg = 0
        let mutable minDistSq = Pnt.distanceSq a p // this handles the case of a single point Polyline3D
        for i = 1 to pts.Count-1 do
            let b = pts.[i]
            let dx = b.X - a.X
            let dy = b.Y - a.Y
            let dz = b.Z - a.Z
            if dx <> 0.0 || dy <> 0.0 || dz <> 0.0 then
                let t = ((p.X - a.X) * dx + (p.Y - a.Y) * dy + (p.Z - a.Z) * dz) / (dx * dx + dy * dy + dz * dz)
                let t' = max 0.0 (min 1.0 t)
                let projX = a.X + dx * t'
                let projY = a.Y + dy * t'
                let projZ = a.Z + dz * t'
                let dpx = p.X - projX
                let dpy = p.Y - projY
                let dpz = p.Z - projZ
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minT <- t'
                    seg <- i - 1
            else
                let dpx = p.X - a.X
                let dpy = p.Y - a.Y
                let dpz = p.Z - a.Z
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minT <- 0.0
                    seg <- i - 1
            a <- b
        float seg + minT

    /// Returns the point on the Polyline3D that is the closest point to the given point.
    member pl.ClosestPoint(p:Pnt) =
        let pts = pl.Points
        if pts.IsEmpty then  fail "Polyline3D.ClosestPoint failed on empty Polyline3D"
        let mutable a = pts[0]
        let mutable minPt = a // this handles the case of a single point Polyline3D
        let mutable minDistSq = Pnt.distanceSq a p // this handles the case of a single point Polyline3D
        for i = 1 to pts.LastIndex do
            let b = pts.[i]
            let dx = b.X - a.X
            let dy = b.Y - a.Y
            let dz = b.Z - a.Z
            if dx <> 0.0 || dy <> 0.0 || dz <> 0.0 then // zero distance between points
                let t = ((p.X - a.X) * dx + (p.Y - a.Y) * dy + (p.Z - a.Z) * dz) / (dx * dx + dy * dy + dz * dz)
                let t' = max 0.0 (min 1.0 t)
                let projX = a.X + dx * t'
                let projY = a.Y + dy * t'
                let projZ = a.Z + dz * t'
                let dpx = p.X - projX
                let dpy = p.Y - projY
                let dpz = p.Z - projZ
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minPt <- Pnt(projX, projY, projZ)
            else
                let dpx = p.X - a.X
                let dpy = p.Y - a.Y
                let dpz = p.Z - a.Z
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minPt <- a
            a <- b
        minPt


    /// Returns the index into the Polylines point list of the vertex that is closest to the given point.
    member pl.ClosestVertex(p:Pnt) : int =
        let pts = pl.Points
        if pts.IsEmpty then  fail "Polyline3D.ClosestVertex failed on empty Polyline3D"
        let mutable minIndex = 0
        let mutable minDistSq = Pnt.distanceSq pts.[0] p
        for i = 1 to pts.LastIndex do
            let dSq = Pnt.distanceSq pts.[i] p
            if dSq < minDistSq then
                minDistSq <- dSq
                minIndex <- i
        minIndex

    /// Returns the distance of the test point to the closest point on the Polyline3D.
    member pl.DistanceTo(p:Pnt) =
        let pts = pl.Points
        if pts.IsEmpty then  fail "Polyline3D.DistanceTo failed on empty Polyline3D"
        let mutable a = pts[0]
        let mutable minDistSq = Pnt.distanceSq a p // this handles the case of a single point Polyline3D
        for i = 1 to pts.LastIndex do
            let b = pts.[i]
            let dx = b.X - a.X
            let dy = b.Y - a.Y
            let dz = b.Z - a.Z
            if dx <> 0.0 || dy <> 0.0 || dz <> 0.0 then // zero distance between points
                let t = ((p.X - a.X) * dx + (p.Y - a.Y) * dy + (p.Z - a.Z) * dz) / (dx * dx + dy * dy + dz * dz)
                let t' = max 0.0 (min 1.0 t)
                let projX = a.X + dx * t'
                let projY = a.Y + dy * t'
                let projZ = a.Z + dz * t'
                let dpx = p.X - projX
                let dpy = p.Y - projY
                let dpz = p.Z - projZ
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
            else
                let dpx = p.X - a.X
                let dpy = p.Y - a.Y
                let dpz = p.Z - a.Z
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
            a <- b
        sqrt minDistSq


    /// Returns the average center of all points of the Polyline3D.
    member pl.Center =
        if points.Count = 0 then failTooFewPoly3D "Center" 1 pl.PointCount
        let mutable x = 0.0
        let mutable y = 0.0
        let mutable z = 0.0
        for i = 0 to points.LastIndex do
            let p = points.[i]
            x <- x + p.X
            y <- y + p.Y
            z <- z + p.Z
        Pnt(x / float points.Count, y / float points.Count, z / float points.Count)


    /// Returns the average normal vector of the Polyline3D.
    /// It is calculated by summing up the cross products of all segments around the center point.
    /// Does not check for bad input, may be zero length if points are colinear.
    member pl.AverageNormal =
        let c = pl.Center
        let mutable normal = Vec.Zero
        let pts = pl.Points
        let mutable a = pts.[pts.LastIndex] - c
        for i = 0 to pts.LastIndex do
            let b = pts.[i] - c
            normal <- normal + Vec.cross(a, b)
            a <- b
        normal

    /// Scales the 3D polyline by a given factor.
    /// Scale center is World Origin 0,0,0
    member p.Scale (factor:float) : Polyline3D =
        points
        |> ResizeArr.map (fun pt -> pt * factor)
        |> Polyline3D


    /// Scales the 3D polyline by a given factor on a given center point
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
        |> Polyline3D

    /// Returns a Polyline3D moved by a vector.
    member p.Move (v:Vec) : Polyline3D =
        points
        |> ResizeArr.map (fun pt -> pt + v)
        |> Polyline3D

    /// Returns a Polyline3D moved by a given distance in X direction.
    member p.MoveX (distance:float) : Polyline3D =
        points
        |> ResizeArr.map (fun pt -> Pnt(pt.X + distance, pt.Y, pt.Z))
        |> Polyline3D

    /// Returns a Polyline3D moved by a given distance in Y direction.
    member p.MoveY (distance:float) : Polyline3D =
        points
        |> ResizeArr.map (fun pt -> Pnt(pt.X, pt.Y + distance, pt.Z))
        |> Polyline3D

    /// Returns a Polyline3D moved by a given distance in Z direction.
    member p.MoveZ (distance:float) : Polyline3D =
        points
        |> ResizeArr.map (fun pt -> Pnt(pt.X, pt.Y, pt.Z + distance))
        |> Polyline3D

    /// Applies or multiplies a 4x4 transformation matrix to the Polyline3D.
    member p.Transform (m:Matrix) : Polyline3D =
        points
        |> ResizeArr.map (fun pt -> pt *** m)
        |> Polyline3D

    /// Multiplies (or applies) a RigidMatrix to the Polyline3D.
    member p.TransformRigid (m:RigidMatrix) : Polyline3D =
        points
        |> ResizeArr.map (fun pt -> Pnt.transformRigid m pt)
        |> Polyline3D

    /// Multiplies (or applies) a Quaternion to the Polyline3D.
    /// The polyline is rotated around the World Origin.
    member p.Rotate (q:Quaternion) : Polyline3D =
        points
        |> ResizeArr.map (fun pt -> pt *** q)
        |> Polyline3D

    /// Multiplies (or applies) a Quaternion to the Polyline3D around a given center point.
    member p.RotateWithCenter (cen:Pnt, q:Quaternion) : Polyline3D =
        points
        |> ResizeArr.map (fun pt -> Pnt.rotateWithCenterByQuat cen q pt)
        |> Polyline3D



    // --------------------------------------------------------------------
    //            █████               █████     ███
    //           ░░███               ░░███     ░░░
    //    █████  ███████    ██████   ███████   ████   ██████
    //   ███░░  ░░░███░    ░░░░░███ ░░░███░   ░░███  ███░░███
    //  ░░█████   ░███      ███████   ░███     ░███ ░███ ░░░
    //   ░░░░███  ░███ ███ ███░░███   ░███ ███ ░███ ░███  ███
    //   ██████   ░░█████ ░░████████  ░░█████  █████░░██████
    //  ░░░░░░     ░░░░░   ░░░░░░░░    ░░░░░  ░░░░░  ░░░░░░
    //
    //                                             █████
    //                                            ░░███
    //    █████████████    ██████  █████████████   ░███████   ██████  ████████   █████
    //   ░░███░░███░░███  ███░░███░░███░░███░░███  ░███░░███ ███░░███░░███░░███ ███░░
    //    ░███ ░███ ░███ ░███████  ░███ ░███ ░███  ░███ ░███░███████  ░███ ░░░ ░░█████
    //    ░███ ░███ ░███ ░███░░░   ░███ ░███ ░███  ░███ ░███░███░░░   ░███      ░░░░███
    //    █████░███ █████░░██████  █████░███ █████ ████████ ░░██████  █████     ██████
    //   ░░░░░ ░░░ ░░░░░  ░░░░░░  ░░░░░ ░░░ ░░░░░ ░░░░░░░░   ░░░░░░  ░░░░░     ░░░░░░


    /// Gets the internal list of all Points of the Polyline3D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline3D.
    static member pointsUnsafeInternal (p:Polyline3D) =
        p.Points

    /// Gets first point of the Polyline3D
    static member start (pl:Polyline3D) =
        let points = pl.Points
        if points.Count < 1 then  failTooFewPoly3D "start" 1 pl.PointCount
        points.[0]

    /// Gets last or end point of the Polyline3D
    static member ende (pl:Polyline3D) =
        let points = pl.Points
        if points.Count < 1 then failTooFewPoly3D "ende" 1 pl.PointCount
        points.[points.Count - 1]

    /// Gets the length of the Polyline3D.
    /// The sum of the lengths of all segments.
    static member inline length (p:Polyline3D) =
        p.Length

    /// Gets the number of points in the Polyline3D.
    static member inline pointCount (p:Polyline3D) =
        p.Points.Count

    /// Gets the number of segments in the Polyline3D.
    static member inline segmentCount (p:Polyline3D) =
        p.SegmentCount

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
    static member translate (v:Vec) (pl:Polyline3D)  : Polyline3D =
        pl |> Polyline3D.map (Pnt.addVec v)

    /// Move a Polyline3D by a vector. (same as Polyline3D.translate)
    static member move (v:Vec) (pl:Polyline3D)  : Polyline3D = Polyline3D.translate v pl

    /// Returns a Polyline3D moved by a given distance in X direction.
    static member moveX (distance:float) (pl:Polyline3D)  : Polyline3D =
        pl |> Polyline3D.map (Pnt.moveX distance)

    /// Returns a Polyline3D moved by a given distance in Y direction.
    static member moveY (distance:float) (pl:Polyline3D)  : Polyline3D =
        pl |> Polyline3D.map (Pnt.moveY distance)

    /// Returns a Polyline3D moved by a given distance in Z direction.
    static member moveZ (distance:float) (pl:Polyline3D)  : Polyline3D =
        pl |> Polyline3D.map (Pnt.moveZ distance)


    /// Scales the Polyline3D by a given factor.
    /// Scale center is World Origin 0,0,0
    /// Returns a new Polyline3D.
    static member scale (factor:float) (pl:Polyline3D) : Polyline3D =
        pl |> Polyline3D.map (fun pt -> pt * factor)


    /// Applies a 4x4 transformation matrix.
    static member transform (m:Matrix) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.transform m)

    /// Multiplies (or applies) a RigidMatrix to the Polyline3D.
    static member transformRigid (m:RigidMatrix) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.transformRigid m)

    /// Rotation a Polyline3D around Z-Axis.
    static member rotate2D (r:Rotation2D) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.rotateZBy r)

    /// Rotation a Polyline3D around Z-Axis.
    [<Obsolete("Renamed to rotate2D to avoid confusion with Quaternion rotation")>]
    static member rotate (r:Rotation2D) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.rotateZBy r)

    /// Rotation a Polyline3D round given Center point an a local Z-axis.
    static member rotate2DWithCenter (cen:Pnt) (r:Rotation2D) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.rotateZwithCenterBy cen r)

    /// Rotation a Polyline3D round given Center point an a local Z-axis.
    [<Obsolete("Renamed to rotate2DWithCenter to avoid confusion with Quaternion rotation")>]
    static member rotateWithCenter (cen:Pnt) (r:Rotation2D) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.rotateZwithCenterBy cen r)

    /// Multiplies (or applies) a Quaternion to the Polyline3D.
    /// The polyline is rotated around the World Origin.
    static member rotateByQuaternion (q:Quaternion) (pl:Polyline3D) =
        pl |> Polyline3D.map (fun pt -> pt *** q)

    /// Multiplies (or applies) a Quaternion to the Polyline3D around a given center point.
    static member rotateWithCenterByQuaternion (cen:Pnt) (q:Quaternion) (pl:Polyline3D) =
        pl |> Polyline3D.map (Pnt.rotateWithCenterByQuat cen q)

    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at point count.
    static member inline closestParameter (pl:Polyline3D) (pt:Pnt) =
        pl.ClosestParameter pt

    /// Returns the point on the Polyline3D that is the closest point to the given point.
    static member inline closestPoint (pl:Polyline3D) (pt:Pnt) =
        pl.ClosestPoint pt

    /// Returns the index into the Polyline3D's point list of the vertex that is closest to the given point.
    static member inline closestVertex (pl:Polyline3D) (pt:Pnt) =
        pl.ClosestVertex pt

    /// Returns the distance of the test point to the closest point on the Polyline3D.
    static member inline distanceTo (pl:Polyline3D) (pt:Pnt) =
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
        if pl.Points.Count < 2 then failTooFewPoly3D "close" 2 pl.PointCount
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
        if pl.Points.Count < 2 then failTooFewPoly3D "closeInPlace" 2 pl.PointCount
        let points = pl.Points
        if Pnt.distanceSq points.First points.Last < 1e-12 then
            points.[points.Count-1] <- points.First
        else
            points.Add points.First



    /// Tests if two Polyline3D have the same number of points and points are equal within a given tolerance.
    static member equals (tol:float) (a:Polyline3D) (b:Polyline3D)  : bool =
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


    /// Removes consecutive duplicate points from the Polyline3D within a given tolerance.
    /// This algorithm allows the last and first point to be identical if the Polyline3D is closed.
    static member removeDuplicatePoints (distanceTolerance:float) (pl:Polyline3D) =
        let pts = pl.Points
        if pts.Count < 2 then
            pl
        else
            let nps = ResizeArray(pts.Count)
            let mutable last = pts.[0]
            nps.Add last
            for i = 1 to pts.LastIndex do
                let p = pts.[i]
                if not (Pnt.equals distanceTolerance last p) then
                    nps.Add p
                    last <- p
            Polyline3D.createDirectlyUnsafe nps

    /// Removes consecutive duplicate points and colinear points from the Polyline3D within given tolerances.
    /// This algorithm allows the last and first point to be identical if the Polyline3D is closed.
    /// Colinear points are removed when the angle between segments is smaller than the cosine threshold (e.g. cosine of 0.5 degrees ).
    /// If the Polyline3D is closed and starts and ends with colinear segments, the first point is replaced with the last non-colinear point.
    /// So the joint of the loop is now moved to the last non-colinear point.
    /// So that there are no colinear segments even between start and end.
    static member removeColinearAndDuplicatePoints (angleTolerance:float<Cosine.cosine>) (distanceTolerance:float) (pl:Polyline3D) =
        if angleTolerance < Cosine.``45.0`` then
            fail $"Polyline3D.removeColinearAndDuplicatePoints: angleTolerance must be at least Cosine.``45.0`` ( that is 0.707) but was {angleTolerance} (= {acos (float angleTolerance)} degrees)."
        if angleTolerance > Cosine.``0.01`` then
            fail $"Polyline3D.removeColinearAndDuplicatePoints: angleTolerance must be at most Cosine.``0.01`` ( that is 0.999999984) but was {angleTolerance} (= {acos (float angleTolerance)} degrees)."

        let pts = pl.Points
        if pts.Count < 2 then // single point or empty polyline
            pl
        else
            let nps = ResizeArray(pts.Count)

            let lastIdx = pts.LastIndex
            let mutable prev = pts.[0]
            nps.Add prev // add first  point

            // find first non-duplicate point:
            let mutable i = 1
            let mutable this = pts.[i]
            let mutable len = Pnt.distance prev this
            while len < distanceTolerance && i < lastIdx do
                i <- i + 1
                this  <- pts.[i]
                len   <- Pnt.distance prev this

            let firstVec = UnitVec.create(prev, this)
            let mutable vPrev = firstVec

            // main loop:
            for idx = i + 1 to lastIdx do
                let next = pts.[idx]
                let vx = next.X - this.X
                let vy = next.Y - this.Y
                let vz = next.Z - this.Z
                let len = vx * vx + vy * vy + vz * vz |> sqrt
                let f  = 1.0 / len
                if len > distanceTolerance then
                    let vNext = UnitVec.createUnchecked(vx * f, vy * f, vz * f)
                    let cos = UnitVec.dot (vPrev, vNext)
                    if withMeasure cos < angleTolerance then
                        // not colinear , keep this point
                        nps.Add this
                        prev <- this
                        vPrev <- vNext // advance previous vector only when point kept
                    this <- next // always advance this point

            // handle last segment to first point
            if pl.IsAlmostClosed distanceTolerance then
                // if closed now check if last and first segment are colinear
                let cos = UnitVec.dot (vPrev, firstVec)
                if withMeasure cos < angleTolerance then
                    // not colinear , keep the original end point
                    nps.Add pts.Last
                else
                    // colinear , replace first point with last non-colinear point
                    nps.[0] <- nps.Last
            else
                // open polyline , just add last point if not duplicate
                if Pnt.notEquals distanceTolerance this prev then
                    nps.Add this

            Polyline3D.createDirectlyUnsafe nps




    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Takes a reference normal for orienting the perpendicular offset and determining inside/outside.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// This function raises an Exception on duplicate points and 180 degree U-turns.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistance">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline (for counterclockwise polylines when refNormal points up).
    /// A negative distance offsets to the outside.</param>
    /// <param name="perpendicularOffsetDistance">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the same orientation of refNormal. A negative distance offsets in the opposite direction.</param>
    /// <param name="refNormal">A unit vector defining an approximate normal direction for orienting the perpendicular offset and determining inside/outside.</param>
    /// <param name="loop">Optional, defaults to false.
    /// Set to true to treat an open polyline as a closed loop, even if first and last points are not at the same location.
    /// When loop=true on an open polyline, both distance lists must contain the same number of items as the polyline has points.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetWithRef(polyLine:Polyline3D,
                                inPlaneOffsetDistance: float,
                                perpendicularOffsetDistance: float,
                                refNormal: UnitVec,
                                [<OPT;DEF(false)>] loop:bool
                                ) : Polyline3D =
        let pts = polyLine.Points
        if pts.Count < 2 then
            fail $"Polyline3D.offsetWithRef: Polyline3D must have at least 2 points but has {pts.Count} points."
        let isOpen = Pnt.distanceSq pts.First pts.Last > Offset3D.sqOpenTolerance
        // check if looping desired and curve is open
        if loop && isOpen then
            let closedPts = ResizeArr.closeLoop pts
            let uvs  = Offset3D.getSegmentUnitVectors closedPts
            let dirs = Offset3D.getOffsetDirections(uvs, refNormal, false, Cosine.``2.5``, Cosine.``179.0``)
            let res  = Offset3D.offsetConstantWithDirections(closedPts, dirs, inPlaneOffsetDistance, perpendicularOffsetDistance)
            res.Pop() |> ignore // remove last point to open the polyline again
            Polyline3D.createDirectlyUnsafe res
        else
            let uvs = Offset3D.getSegmentUnitVectors pts
            let dirs = Offset3D.getOffsetDirections(uvs, refNormal, isOpen, Cosine.``2.5``, Cosine.``179.0``)
            Offset3D.offsetConstantWithDirections(pts, dirs, inPlaneOffsetDistance, perpendicularOffsetDistance)
            |> Polyline3D.createDirectlyUnsafe

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// The reference normal is computed from the average normal of the polyline.
    /// This function raises an Exception on duplicate points, 180 degree U-turns, or colinear points.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistance">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="perpendicularOffsetDistance">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counterclockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="loop">Optional, defaults to false.
    /// Set to true to treat an open polyline as a closed loop, even if first and last points are not at the same location.
    /// When loop=true on an open polyline, both distance lists must contain the same number of items as the polyline has points.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offset(   polyLine:Polyline3D,
                            inPlaneOffsetDistance: float,
                            perpendicularOffsetDistance: float,
                            [<OPT;DEF(false)>] loop:bool
                            ) : Polyline3D =
        let pts = polyLine.Points
        if pts.Count < 2 then
            fail $"Polyline3D.offset: Polyline3D must have at least 2 points but has {pts.Count} points."
        let refNormal = polyLine.AverageNormal
        if refNormal.LengthSq < 1e-8 then
            fail $"Polyline3D.offset: Cannot compute average normal of Polyline3D, the {pts.Count} points are colinear or too close together: "
        Polyline3D.offsetWithRef(polyLine,inPlaneOffsetDistance, perpendicularOffsetDistance, refNormal.Unitized, loop)


    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Takes a reference normal for orienting the perpendicular offset and determining inside/outside.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// This function raises an Exception on duplicate points and 180 degree U-turns.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistance">The offset distances in the local plane defined by two segments. One distance per segment.
    /// A positive distance offsets to the inside of the polyline (for counterclockwise polylines when refNormal points up).
    /// A negative distance offsets to the outside.</param>
    /// <param name="perpendicularOffsetDistances">The offset distances perpendicular to the local plane. One distance per segment.
    /// A positive distance offsets in the same orientation of refNormal. A negative distance offsets in the opposite direction.</param>
    /// <param name="refNormal">A unit vector defining an approximate normal direction for orienting the perpendicular offset and determining inside/outside.</param>
    /// <param name="loop">Optional, defaults to false.
    /// Set to true to treat an open polyline as a closed loop, even if first and last points are not at the same location.</param>
    /// <param name="varDistParallelBehaviour">Optional, defaults to Fail.
    /// The behavior to use when colinear segments with different offset distances are found.</param>
    /// <param name="considerColinearBelow">Optional, defaults to 2.5 degrees in cosine measure.
    /// The cosine of the angle below which segments are considered colinear.</param>
    /// <param name="failAtUTurnAbove">Optional, defaults to 175 degrees in cosine measure.
    /// The cosine of the angle above which a U-turn is considered to fail.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetVarWithRef(
                            polyLine:Polyline3D,
                            inPlaneOffsetDistance: Collections.Generic.IList<float>,
                            perpendicularOffsetDistances: Collections.Generic.IList<float>,
                            refNormal: UnitVec,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(Offset3D.VarDistParallel.Fail)>] varDistParallelBehaviour: Offset3D.VarDistParallel, // fail by default
                            [<OPT;DEF(Cosine.``2.5``)>] considerColinearBelow: float<Cosine.cosine>,
                            [<OPT;DEF(Cosine.``175.0``)>] failAtUTurnAbove :float<Cosine.cosine>
                            ) : Polyline3D =
        let pts = polyLine.Points
        if pts.Count < 2 then
            fail $"Polyline3D.offsetVarWithRef: Polyline3D must have at least 2 points but has {pts.Count} points."
        let isOpen = Pnt.distanceSq pts.First pts.Last > Offset3D.sqOpenTolerance
        // check if looping desired and curve is open
        if loop && isOpen then
            if inPlaneOffsetDistance.Count <> pts.Count then
                fail ($"Polyline3D.offsetVarWithRef: For open Polyline3D with loop=true the inPlaneOffsetDistance list must have the same number of items as the polyline has points.\n" +
                      $"But polyline has {pts.Count} points and inPlaneOffsetDistance has {inPlaneOffsetDistance.Count} items.")
            if perpendicularOffsetDistances.Count <> pts.Count then
                fail ($"Polyline3D.offsetVarWithRef: For open Polyline3D with loop=true the perpendicularOffsetDistances list must have the same number of items as the polyline has points.\n" +
                      $"But polyline has {pts.Count} points and perpendicularOffsetDistances has {perpendicularOffsetDistances.Count} items.")
            let closedPts = ResizeArr.closeLoop pts
            let uvs  = Offset3D.getSegmentUnitVectors closedPts
            let dirs = Offset3D.getOffsetDirections(uvs, refNormal, false, considerColinearBelow, failAtUTurnAbove)
            let res  = Offset3D.offsetVariableWithDirections(closedPts,uvs, dirs, inPlaneOffsetDistance, perpendicularOffsetDistances, true, varDistParallelBehaviour)
            res.Pop() |> ignore // remove last point to open the polyline again
            Polyline3D.createDirectlyUnsafe res
        else
            let uvs = Offset3D.getSegmentUnitVectors pts
            let dirs = Offset3D.getOffsetDirections(uvs, refNormal, isOpen, considerColinearBelow, failAtUTurnAbove)
            Offset3D.offsetVariableWithDirections(pts,uvs, dirs, inPlaneOffsetDistance, perpendicularOffsetDistances, isOpen, varDistParallelBehaviour)
            |> Polyline3D.createDirectlyUnsafe


    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// The reference normal is computed from the average normal of the polyline.
    /// This function raises an Exception on duplicate points, 180 degree U-turns, or colinear points.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistance">The offset distances in the local plane defined by two segments. One distance per segment.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="perpendicularOffsetDistances">The offset distances perpendicular to the local plane. One distance per segment.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counterclockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="loop">Optional, defaults to false.
    /// Set to true to treat an open polyline as a closed loop, even if first and last points are not at the same location.</param>
    /// <param name="varDistParallelBehaviour">Optional, defaults to Fail.
    /// The behavior to use when colinear segments with different offset distances are found.</param>
    /// <param name="considerColinearBelow">Optional, defaults to 2.5 degrees in cosine measure.
    /// The cosine of the angle below which segments are considered colinear.</param>
    /// <param name="failAtUTurnAbove">Optional, defaults to 175 degrees in cosine measure.
    /// The cosine of the angle above which a U-turn is considered to fail.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetVar(polyLine:Polyline3D,
                            inPlaneOffsetDistance: ResizeArray<float>,
                            perpendicularOffsetDistances: ResizeArray<float>,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(Offset3D.VarDistParallel.Fail)>] varDistParallelBehaviour: Offset3D.VarDistParallel, // fail by default
                            [<OPT;DEF(Cosine.``2.5``)>] considerColinearBelow: float<Cosine.cosine>,
                            [<OPT;DEF(Cosine.``175.0``)>] failAtUTurnAbove :float<Cosine.cosine>
                            ) : Polyline3D =
        let refNormal = polyLine.AverageNormal
        if refNormal.LengthSq < 1e-8 then
            fail $"Polyline3D.offsetVar: Cannot compute average normal of Polyline3D, the {polyLine.Points.Count} points are colinear or too close together: "
        Polyline3D.offsetVarWithRef(polyLine,inPlaneOffsetDistance, perpendicularOffsetDistances, refNormal.Unitized, loop, varDistParallelBehaviour, considerColinearBelow, failAtUTurnAbove)



    [<Obsolete("Use polyline3D.CloseInPlace instead.")>]
    member p.CloseIfOpen(t) = p.CloseInPlace(t)