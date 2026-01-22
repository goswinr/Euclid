namespace Euclid

open System
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors
open Euclid.EuclidCollectionUtilities


/// A mutable 2D Polyline.
/// If the last point is the same as the first point, the Polyline2D is considered closed.
/// The Default constructor uses the provided ResizeArray of points directly,
/// so changes to the list will be reflected in the Polyline2D.
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

    /// Nicely formatted string representation of the Polyline2D including its length.
    override p.ToString() =
        if points.Count = 0 then
            "empty Euclid.Polyline2D."
        elif p.IsClosed then
            $"closed Euclid.Polyline2D with length {p.Length}, from {points.Count} points"
        else
            $"open Euclid.Polyline2D with length {p.Length}, from {points.Count} points"

    /// Format Polyline2D into string including its length.
    member p.AsString : string =
        if points.Count = 0 then
            "empty Polyline2D."
        elif p.IsClosed then
            $"closed Polyline2D with length {p.Length}, from {points.Count} points"
        else
            $"open Polyline2D with length {p.Length}, from {points.Count} points"


    /// Format a 2D polyline into an F# code string that can be used to recreate the point.
    member p.AsFSharpCode : string =
        let ptsAsCode =
            points
            |> ResizeArr.map _.AsFSharpCode
            |> String.concat "; "
        $"Polyline2D.create [| {ptsAsCode} |]"



    /// Creates a copy of the Polyline2D
    /// Same as polyline.Clone()
    member p.Duplicate(): Polyline2D =
        Polyline2D(points.GetRange(0, points.Count))

    /// Creates a copy of the Polyline2D.
    /// Same as polyline.Duplicate()
    member p.Clone(): Polyline2D =
        Polyline2D(points.GetRange(0, points.Count))

    /// Sets the vertex at given index to the given point.
    /// On a closed Polyline2D, setting the first or last point will set both to the same point.
    member p.SetVertex idx (pt:Pt) =
        if idx < 0 || idx >= points.Count then
            fail $"Polyline2D.SetVertex: index {idx} is out of range for Polyline2D with {points.Count} points."
        if idx = 0 && p.IsClosed then
            points.[points.LastIndex] <- pt
        elif idx = points.LastIndex && p.IsClosed then
            points.[0] <- pt
        points.[idx] <- pt //do last, otherwise IsClosed check fails

    /// Gets or sets first point of the Polyline2D
    /// This is the point at index 0.
    /// Same as Polyline2D.FirstPoint
    member p.Start
        with get() =
            if points.Count < 1 then failTooFewPoly2D "Start.get" 1 points.Count
            points.[0]
        and set(v) =
            if points.Count < 1 then failTooFewPoly2D "Start.set" 1 points.Count
            points.[0] <- v

    /// Gets or sets last or end point of the Polyline2D
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline2D.LastPoint
    member p.End
        with get() =
            if points.Count < 1 then failTooFewPoly2D "End.get" 1 points.Count
            points.[points.Count - 1]
        and set(v) =
            if points.Count < 1 then failTooFewPoly2D "End.set" 1 points.Count
            points.[points.Count - 1] <- v

    /// Gets or sets the last point of the Polyline2D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline2D.End
    member p.LastPoint
        with get() =
            if points.Count < 1 then failTooFewPoly2D "LastPoint.get" 1 points.Count
            points.[points.Count - 1]
        and set(v) =
            if points.Count < 1 then failTooFewPoly2D "LastPoint.set" 1 points.Count
            points.[points.Count - 1] <- v

    /// Gets or sets the second last point of the Polyline2D.
    member p.SecondLastPoint
        with get() =
            if points.Count < 2 then failTooFewPoly2D "SecondLastPoint.get" 2 p.PointCount
            points.[points.Count - 2]
        and set(v) =
            if points.Count < 2 then failTooFewPoly2D "SecondLastPoint.set" 2 p.PointCount
            points.[points.Count - 2] <- v

    /// Gets or sets the second point of the Polyline2D.
    /// This is the point at index 1.
    member p.SecondPoint
        with get() =
            if points.Count < 2 then failTooFewPoly2D "SecondPoint.get" 2 p.PointCount
            points.[1]
        and set(v) =
            if points.Count < 2 then failTooFewPoly2D "SecondPoint.set" 2 p.PointCount
            points.[1] <- v

    /// Gets or sets the first point of the Polyline2D.
    /// This is the point at index 0.
    /// Same as Polyline2D.Start
    member p.FirstPoint
        with get() =
            if points.Count < 1 then failTooFewPoly2D "FirstPoint.get" 1 p.PointCount
            points.[0]
        and set(v) =
            if points.Count < 1 then failTooFewPoly2D "FirstPoint.set" 1 p.PointCount
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
    member p.Length : float =
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
            fail $"Polyline2D.GetSegment: index {i} is out of range for Polyline2D with {points.Count} points."
        Line2D(points.[i], points.[i+1])

    /// Gets the segment at index i of the Polyline2D.
    member p.LastSegment =
        if points.Count < 2 then failTooFewPoly2D "LastSegment" 2 p.PointCount
        let i = points.Count - 1
        Line2D(points.[i-1], points.[i])

    /// Gets the first segment of the Polyline2D.
    member p.FirstSegment =
        if points.Count < 2 then failTooFewPoly2D "FirstSegment" 2 p.PointCount
        Line2D(points.[0], points.[1])

    /// Returns all segments of the Polyline2D as a list of Line2D.
    member p.Segments : ResizeArray<Line2D> =
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

    /// Returns the line vectors of all segments of the Polyline2D as a list of Vc.
    member p.SegmentVectors : ResizeArray<Vc> =
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


    /// Gets bounding rectangle of the Polyline2D
    member p.BoundingRectangle =
        BRect.createFromIList points

    /// Tests if Polyline2D start and end points are exactly the same.
    /// Returns False if the Polyline2D has less than 3 points.
    member p.IsClosed =
        points.Count > 2
        &&
        (p.Start  - p.End).IsZero


    /// Tests if Polyline2D is closed within given tolerance.
    /// Returns False if the Polyline2D has less than 3 points.
    member p.IsAlmostClosed tolerance =
        points.Count > 2
        &&
        Pt.distanceSq p.Start p.End < tolerance*tolerance

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
    member p.CloseInPlace(toleranceForAddingPoint) =
        if points.Count < 3 then failTooFewPoly2D "CloseInPlace" 3 p.PointCount
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
            area <- area + (t.X-n.X) * (n.Y + t.Y)
            t <- n
        area * 0.5

    /// The area of the Polyline2D.
    /// Fails if Polyline is not exactly closed.
    /// For self intersecting Polylines the result is invalid.
    member p.Area : float =
        if not p.IsClosed then fail $"Polyline2D.Area failed on Polyline2D that is not exactly closed {p}"
        abs(p.SignedArea)

    /// Test if Polyline2D is CounterClockwise.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated.
    /// If it is positive the Polyline2D is Counter Clockwise.
    member p.IsCounterClockwise =
        let  area = p.SignedArea
        if abs(area) < UtilEuclid.zeroLengthTolerance then
            fail $"Polyline2D.IsCounterClockwise: Polyline2D the area is zero: {p}"
        area > 0.0


    /// Test if Polyline2D is Clockwise.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated.
    /// If it is negative the Polyline2D is Clockwise.
    member p.IsClockwise =
        let area = p.SignedArea
        if abs(area) < UtilEuclid.zeroLengthTolerance then
            fail $"Polyline2D.IsClockwise: Polyline2D the area is zero: {p}"
        area < 0.0


    /// Returns the point at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    /// If the parameter is within 1e-6 of an integer value, the integer value is used as parameter.
    member pl.EvaluateAt(t:float) : Pt =
        let i = int t       // integer part of the parameter
        let p = t - float i // fractional part of the parameter
        let count = pl.Points.Count
        let countF = float count

        // values next to  the start of the polyline:
        if t < 1e-6 then
            if t < -1e-6 then
                fail $"Polyline2D.EvaluateAt: Parameter {t} is less than 0.0"
            pl.Points.First

        // values next to  the end of the polyline:
        elif t > (countF - 1e-6) then
            if t > (countF + 1e-6) then
                fail $"Polyline2D.EvaluateAt: Parameter {t} is more than point count {pl.Points.Count}."
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
        fail "Polyline2D.TangentAt is obsolete, use GetSegment(i).UnitTangent instead." |> unbox // unbox to make type checker happy


    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter form 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.ClosestParameter(p:Pt) =
        let pts = pl.Points
        if pts.IsEmpty then  fail "Polyline2D.ClosestParameter failed on empty Polyline2D"
        let mutable a = pts[0]
        let mutable minT = 0.0
        let mutable seg = 0
        let mutable minDistSq = Pt.distanceSq a p // this handles the case of a single point Polyline2D
        for i = 1 to pts.LastIndex do // 1 because last point is same as first
            let b = pts.[i]
            let dx = b.X - a.X
            let dy = b.Y - a.Y
            if dx <> 0.0 || dy <> 0.0 then // zero distance between points
                let t = ((p.X - a.X) * dx + (p.Y - a.Y) * dy) / (dx * dx + dy * dy)
                let t' = max 0.0 (min 1.0 t)
                let projX = a.X + dx * t'
                let projY = a.Y + dy * t'
                let dpx = p.X - projX
                let dpy = p.Y - projY
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minT <- t'
                    seg <- i - 1 // -1 because i starts at 1
            else
                let dpx = p.X - a.X
                let dpy = p.Y - a.Y
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minT <- 0.0
                    seg <- i - 1
            a <- b
        float seg + minT


    /// Returns the point on the Polyline2D that is the closest point to the given point.
    /// This might be a point on a segment or a vertex point.
    member pl.ClosestPoint(p:Pt) =
        let pts = pl.Points
        if pts.IsEmpty then  fail "Polyline2D.ClosestPoint failed on empty Polyline2D"
        let mutable a = pts[0]
        let mutable minPt = a // this handles the case of a single point Polyline2D
        let mutable minDistSq = Pt.distanceSq a p // this handles the case of a single point Polyline2D
        for i = 1 to pts.LastIndex do // 1 because last point is same as first
            let b = pts.[i]
            let dx = b.X - a.X
            let dy = b.Y - a.Y
            if dx <> 0.0 || dy <> 0.0 then // zero distance between points
                let t = ((p.X - a.X) * dx + (p.Y - a.Y) * dy) / (dx * dx + dy * dy)
                let t' = max 0.0 (min 1.0 t)
                let projX = a.X + dx * t'
                let projY = a.Y + dy * t'
                let dpx = p.X - projX
                let dpy = p.Y - projY
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minPt <- Pt(projX, projY)
            else
                let dpx = p.X - a.X
                let dpy = p.Y - a.Y
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minPt <- a
            a <- b
        minPt

    /// Returns the index into the Polylines point list of the vertex that is closest to the given point.
    member pl.ClosestVertex(p:Pt) : int =
        let pts = pl.Points
        if pts.IsEmpty then  fail "Polyline2D.ClosestVertex failed on empty Polyline2D"
        let mutable minIdx = 0
        let mutable minDistSq = Pt.distanceSq pts[0] p
        for i = 1 to pts.LastIndex do
            let dSq = Pt.distanceSq pts.[i] p
            if dSq < minDistSq then
                minDistSq <- dSq
                minIdx <- i
        minIdx

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    member pl.DistanceTo(p:Pt) =
        let pts = pl.Points
        if pts.IsEmpty then  fail "Polyline2D.DistanceTo failed on empty Polyline2D"
        let mutable a = pts[0]
        let mutable minDistSq = Pt.distanceSq a p // this handles the case of a single point Polyline2D
        for i = 1 to pts.LastIndex do // 1 because last point is same as first
            let b = pts.[i]
            let dx = b.X - a.X
            let dy = b.Y - a.Y
            if dx <> 0.0 || dy <> 0.0 then // zero distance between points
                let t = ((p.X - a.X) * dx + (p.Y - a.Y) * dy) / (dx * dx + dy * dy)
                let t' = max 0.0 (min 1.0 t)
                let projX = a.X + dx * t'
                let projY = a.Y + dy * t'
                let dpx = p.X - projX
                let dpy = p.Y - projY
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
            else
                let dpx = p.X - a.X
                let dpy = p.Y - a.Y
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
            a <- b
        sqrt minDistSq



    /// <summary>Count how many times the polygon winds around the point.
    /// If the result is 0 then the point is outside of the Polyline2D.
    /// A non-zero value indicates the point is inside.</summary>
    /// <param name="point">The point to test.</param>
    /// <returns>The winding number. Zero means outside, non-zero means inside.</returns>
    /// <remarks>The first and last point of the Polyline2D need to be identical for correct results.
    /// Boundary cases: Points exactly on edges or vertices may return inconsistent results
    /// due to floating-point precision. For points on horizontal edges, behavior depends on
    /// the edge direction. This method Handles self-intersecting polygons more intuitively than the pl.Contains method.</remarks>
    member pl.WindingNumber (point:Pt) : int =
        // from https://github.com/FreyaHolmer/Mathfs/blob/master/Runtime/Geometric%20Shapes/Polygon.cs#L92
        // https://x.com/FreyaHolmer/status/1232826293902888960
        // or use ? https://github.com/blenderfan/AdvancedGamedevTutorials/blob/main/AdvancedGamedev-WindingNumbers/Polygon2D.cs
        // https://www.youtube.com/watch?v=E51LrZQuuPE
        let px = point.X
        let py = point.Y
        let inline isLeft (aa:Pt) (bb:Pt)  =
            let ax = px - aa.X // cross product unrolled to avoid allocation of temp Vc
            let ay = py - aa.Y
            let bx = bb.X - aa.X
            let by = bb.Y - aa.Y
            let det = ax * by - ay * bx
            // if   det >  1e-12 then  1
            // elif det < -1e-12 then -1
            if   det > 0 then  1
            elif det < 0 then -1
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

    /// <summary>Tests if a point is inside the closed Polyline2D using the ray casting algorithm.</summary>
    /// <param name="pt">The point to test.</param>
    /// <returns>TRUE if the point is inside, FALSE otherwise.</returns>
    /// <remarks>The first and last point of the Polyline2D need to be identical for correct results.
    /// Self-intersecting polygons give "alternating" inside/outside regions
    /// Uses ray casting: runs an infinite horizontal ray (increasing x, fixed y) from the test point
    /// and counts edge crossings. Each crossing toggles inside/outside state (Jordan curve theorem).
    /// Always returns FALSE if the Polyline2D has less than 3 points.
    /// Boundary cases: Points exactly on edges or vertices have implementation-specific behavior.
    /// Horizontal edges are handled by the strict inequality convention (pi.Y > y) != (pj.Y > y).
    /// Points on left/bottom edges tend to be considered inside, right/top edges outside.
    /// The result may differ from checking the pl.WindingNumber 0 for boundary points.</remarks>
    member p.Contains (pt: Pt)  =
        let pts = p.Points
        if pts.Count < 3 then
            false
        else
            // taken from Polylabel algorithm
            // also see https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html
            let mutable inside = false
            let mutable pi = pts.[0]
            let y = pt.Y
            let x = pt.X
            for i = 1 to pts.LastIndex do
                let pj = pts.[i]
                if  (pi.Y > y) <> (pj.Y > y)
                &&  x < (pj.X - pi.X) * (y - pi.Y) / (pj.Y - pi.Y) + pi.X
                    then
                        inside <- not inside
                pi <- pj
            inside


    /// Calculates the shortest distance from the test point to the polyline with `DistanceTo`,
    /// then signs that value by testing containment via the ray-casting based `Contains` helper.
    /// Returns a positive distance for points that lie inside the polyline boundary and negative otherwise.
    /// For reliable results the polyline should be closed and have identical first and last vertices.
    member pl.SignedDistanceTo (point: Pt)  :float =
        let distance = pl.DistanceTo point
        if pl.Contains point then distance else -distance


    /// Returns the average center of all points of the Polyline2D.
    member p.Center =
        if points.Count = 0 then failTooFewPoly2D "Center" 1 p.PointCount
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
        |> Polyline2D


    /// Scales the 2D polyline by a given factor on a given center point.
    member p.ScaleOn (cen:Pt) (factor:float) : Polyline2D =
        let cx = cen.X
        let cy = cen.Y
        points
        |> ResizeArr.map (fun pt ->
            Pt( cx + (pt.X - cx) * factor,
                cy + (pt.Y - cy) * factor)
            )
        |> Polyline2D

    /// Finds a point inside a closed Polyline2D that is the farthest away from the edges of the Polyline2D.
    /// Uses the Polylabel algorithm from Mapbox. It is a highly optimized algorithm specifically designed to find the
    /// pole of inaccessibility for polygons. The point within the polygon that is farthest from the edges,
    /// often used for optimal label placement.
    /// Adaptive Precision: Can trade accuracy for speed based on your needs.
    /// Returns the best point and its distance to the polygon edge. Supplying an open polyline is allowed,
    /// but the computed "inside" still assumes the points describe a closed boundary (first and last vertices should match).
    member pl.FindLablePoint (precision: float)  =
        // see https://github.com/mapbox/polylabel
        // Polylabel uses a clever grid-based approach with iterative refinement
        // Initial Grid: Creates a grid covering the polygon's bounding box
        // Distance Calculation: For each grid cell, calculates the distance from the cell center to the nearest polygon edge
        // Priority Queue: Uses a priority queue to explore the most promising cells first (those with highest potential distance)
        // Iterative Refinement: Subdivides promising cells into smaller cells and continues the search
        // Convergence: Stops when the precision threshold is reached
        //
        // Bounding Box Pruning: Quickly eliminates cells that can't possibly contain the optimal point
        // Distance Upper Bounds: Each cell maintains an upper bound on the possible distance, allowing early termination of unpromising branches
        if pl.PointCount < 1 then
            fail $"Polyline2D.FindLablePoint must have at least 1 point but has {pl.PointCount} points."

        let inline createCellPt (point:Pt) h (polygon:Polyline2D) : Polylabel.Cell  =
            let distance =  polygon.SignedDistanceTo point
            let maxDistance = distance + h * 1.4142135623730951 // = sqrt 2.0
            { X = point.X; Y = point.Y; H = h; Distance = distance; MaxDistance = maxDistance }

        let inline createCell x y h (polygon:Polyline2D) : Polylabel.Cell =
            createCellPt (Pt(x, y)) h polygon

        let bRect = pl.BoundingRectangle
        let minX = bRect.MinX
        let minY = bRect.MinY
        let maxX = bRect.MaxX
        let maxY = bRect.MaxY
        let width = maxX - minX
        let height = maxY - minY
        if width < precision || height < precision then
            // Degenerate polygon (almost or just a line or point)
            let c = pl.Center
            let dist = pl.SignedDistanceTo c
            c, dist
        else
            let cellSize = min width height
            let halfCell = cellSize * 0.5

            let heap = Polylabel.CellHeap()

            // Seed initial grid cells
            let mutable y = minY
            while y < maxY do
                let mutable x = minX
                while x < maxX do
                    let cell = createCell (x + halfCell) (y + halfCell) halfCell pl
                    // Keep all initial cells (filtering here can prematurely prune)
                    heap.Add cell
                    x <- x + cellSize
                y <- y + cellSize

            // Initial best: polygon centroid
            let mutable bestCell = createCellPt pl.Center 0.0 pl

            // Also try bbox center
            let bboxCell = createCellPt bRect.Center 0.0 pl
            if bboxCell.Distance > bestCell.Distance then
                bestCell <- bboxCell

            // Main loop
            while heap.Count > 0 do
                let cell = heap.Pop()

                // Prune if this cell cannot improve current best beyond precision
                if cell.MaxDistance - bestCell.Distance <= precision then
                    ()
                else
                    // If sufficiently small, treat center as candidate
                    if cell.H <= precision then
                        if cell.Distance > bestCell.Distance then
                            bestCell <- cell
                    else
                        // Subdivide
                        let inline enqueueIfBetter (c: Polylabel.Cell) =
                            // Only enqueue if it could still beat current best
                            if c.MaxDistance - bestCell.Distance > precision then
                                heap.Add c
                            if c.Distance > bestCell.Distance then
                                bestCell <- c

                        let h = cell.H * 0.5
                        pl |> createCell (cell.X - h) (cell.Y - h) h |> enqueueIfBetter
                        pl |> createCell (cell.X + h) (cell.Y - h) h |> enqueueIfBetter
                        pl |> createCell (cell.X - h) (cell.Y + h) h |> enqueueIfBetter
                        pl |> createCell (cell.X + h) (cell.Y + h) h |> enqueueIfBetter


            Pt(bestCell.X, bestCell.Y), bestCell.Distance




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


    /// Finds a point inside a closed Polyline2D that is the farthest away from the edges of the Polyline2D.
    /// Uses the Polylabel algorithm from Mapbox. It is a highly optimized algorithm specifically designed to find the
    /// pole of inaccessibility for polygons. The point within the polygon that is farthest from the edges,
    /// often used for optimal label placement.
    /// Adaptive Precision: Can trade accuracy for speed based on your needs.
    /// Returns the best point and its distance to the polygon edge. Supplying an open polyline is allowed,
    /// but the computed "inside" still assumes the points describe a closed boundary (first and last vertices should match).
    static member findLablePoint (precision: float) (pl: Polyline2D)  =
        pl.FindLablePoint precision

    /// Gets the internal list of all Points of the Polyline2D.
    /// This is not a copy, so changes to the list will be reflected in the Polyline2D.
    static member pointsUnsafeInternal (p:Polyline2D) =
        p.Points

    /// Gets first point of the Polyline2D
    static member start (p:Polyline2D) =
        let points = p.Points
        if points.Count < 1 then failTooFewPoly2D "start" 1 p.PointCount
        points.[0]

    /// Gets last or end point of the Polyline2D
    static member ende (p:Polyline2D) =
        let points = p.Points
        if points.Count < 1 then failTooFewPoly2D "ende" 1 p.PointCount
        points.[ points.Count - 1 ]

    /// Gets the length of the Polyline2D.
    /// The sum of the lengths of all segments.
    static member inline length (p:Polyline2D) =
        p.Length

    /// Gets the number of points in the Polyline2D.
    static member inline pointCount (p:Polyline2D) =
        p.Points.Count

    /// Gets the number of segments in the Polyline2D.
    static member inline segmentCount (p:Polyline2D) =
        p.SegmentCount


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
        pl.Points |> ResizeArr.map mapping |> Polyline2D

    /// Move a Polyline2D by a vector. (same as Polyline2D.move)
    static member translate (v:Vc) (pl:Polyline2D)  : Polyline2D =
        pl |> Polyline2D.map (Pt.addVc v)

    /// Move a Polyline2D by a vector. (same as Polyline2D.translate)
    static member move (v:Vc) (pl:Polyline2D)  : Polyline2D =
        Polyline2D.translate v pl

    /// Returns a Polyline2D moved by a given distance in X direction.
    static member moveX (distance:float) (pl:Polyline2D)  : Polyline2D =
        pl |> Polyline2D.map (Pt.moveX distance)

    /// Returns a Polyline2D moved by a given distance in Y direction.
    static member moveY (distance:float) (pl:Polyline2D)  : Polyline2D =
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
    static member inline closestParameter (pl:Polyline2D) (pt:Pt) =
        pl.ClosestParameter pt

    /// Returns the point on the Polyline2D that is the closest point to the given point.
    static member inline closestPoint (pl:Polyline2D) (pt:Pt) =
        pl.ClosestPoint pt

    /// Returns the index into the Polylines point list of the vertex that is closest to the given point.
    static member inline closestVertex (pl:Polyline2D) (pt:Pt) : int =
        pl.ClosestVertex pt

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    static member inline distanceTo (pl:Polyline2D) (pt:Pt) =
        pl.DistanceTo pt

    /// Create a new Polyline2D by copying over all points.
    /// This will allocate a new ResizeArray and copy all points.
    static member inline create(points: seq<Pt>) =
        Polyline2D(ResizeArray(points))

    /// Create a new Polyline2D by using the provided ResizeArray directly.
    /// Unsafe because all later changes to the ResizeArray will be reflected in the Polyline2D.
    static member createDirectlyUnsafe (points: ResizeArray<Pt>) =
        Polyline2D(points)

    /// Create a new empty Polyline2D without any points.
    /// But predefined capacity.
    static member inline createEmpty (capacity:int) =
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



    /// Returns a new closed Polyline2D.
    /// If the first and last point are within 1e-6 of each other, the last point is set equal to the first point.
    /// Otherwise one point is added.
    static member close (pl:Polyline2D) =
        if pl.Points.Count < 2 then failTooFewPoly2D "close" 2 pl.PointCount
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
        if pl.Points.Count < 2 then failTooFewPoly2D "closeInPlace" 2 pl.PointCount
        let points = pl.Points
        if Pt.distanceSq points.First points.Last < 1e-12 then
            points.[points.Count-1] <- points.First
        else
            points.Add points.First

    /// Tests if two Polyline2D have the same number of points and points are equal within a given tolerance.
    static member equals tol (a:Polyline2D) (b:Polyline2D)  : bool =
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

    /// Removes consecutive duplicate points from the Polyline2D within a given tolerance.
    /// This algorithm allows the last and first point to be identical if the Polyline2D is closed.
    static member removeDuplicatePoints (distanceTolerance:float) (pl:Polyline2D) =
        let pts = pl.Points
        if pts.Count < 2 then // single point or empty polyline
            pl
        else
            let nps = ResizeArray(pts.Count)
            let mutable prev = pts.[0]
            nps.Add prev
            for i = 1 to pts.LastIndex do
                let p = pts.[i]
                if not (Pt.equals distanceTolerance prev p) then
                    nps.Add p
                    prev <- p
            Polyline2D.createDirectlyUnsafe nps

    /// Removes consecutive duplicate points and colinear points from the Polyline2D within given tolerances.
    /// This algorithm allows the last and first point to be identical if the Polyline2D is closed.
    /// Colinear points are removed when the angle between segments is smaller than the cosine threshold (e.g. cosine of 0.5 degrees ).
    /// If the Polyline2D is closed and starts and ends with colinear segments, the first point is replaced with the last non-colinear point.
    /// So the joint of the loop is now moved to the last non-colinear point.
    /// So that there are no colinear segments even between start and end.
    static member removeColinearAndDuplicatePoints (angleTolerance:float<Cosine.cosine>) (distanceTolerance:float) (pl:Polyline2D) =
        if angleTolerance < Cosine.``45.0`` then
            fail $"Polyline2D.removeColinearAndDuplicatePoints: angleTolerance must be at least Cosine.``45.0`` ( that is 0.707) but was {angleTolerance} (= {acos (float angleTolerance)} degrees)."
        if angleTolerance > Cosine.``0.01`` then
            fail $"Polyline2D.removeColinearAndDuplicatePoints: angleTolerance must be at most Cosine.``0.01`` ( that is 0.999999984) but was {angleTolerance} (= {acos (float angleTolerance)} degrees)."

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
            let mutable len = Pt.distance prev this
            while len < distanceTolerance && i < lastIdx do
                i <- i + 1
                this  <- pts.[i]
                len   <- Pt.distance prev this

            let firstVec = UnitVc.create(prev, this)
            let mutable vPrev = firstVec

            // main loop:
            for idx = i + 1 to lastIdx do
                let next = pts.[idx]
                let vx = next.X - this.X
                let vy = next.Y - this.Y
                let len = vx * vx + vy * vy |> sqrt
                if len > distanceTolerance then
                    let vNext = UnitVc.createUnchecked(vx / len, vy / len)
                    let cos = UnitVc.dot (vPrev, vNext)
                    if withMeasure cos < angleTolerance then
                        // not colinear , keep this point
                        nps.Add this
                        prev <- this
                        vPrev <- vNext // advance previous vector only when point kept
                    this <- next // always advance this point

            // handle last segment to first point
            if pl.IsAlmostClosed distanceTolerance then
                // if closed now check if last and first segment are colinear
                let cos = UnitVc.dot (vPrev, firstVec)
                if withMeasure cos < angleTolerance then
                    // not colinear , keep the original end point
                    nps.Add pts.Last
                else
                    // colinear , replace first point with last non-colinear point
                    nps.[0] <- nps.Last
            else
                // open polyline , just add last point if not duplicate
                if Pt.notEquals distanceTolerance this prev then
                    nps.Add this

            Polyline2D.createDirectlyUnsafe nps



    /// <summary> Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// By default this function raises an Exception on duplicate points, 180 degree U-Turns.
    /// But this can be configured with optional parameters.</summary>
    /// <param name="polyLine"> A 2D Polyline, open or closed. </param>
    /// <param name="constantOffsetDistance">The offset distance for all segments of the polyline.
    /// A positive distance offset to the inside of the polyline. A negative distance offset to the outside of the polyline.</param>
    /// <param name="loop">bool, Optional (false).
    /// Set to true to explicitly consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="checkOrientation">bool, Optional(true). By default the algorithm always checks if the polyline is clockwise or counter clockwise.
    /// So that positive offset distances are always towards the inside of the polyline.
    /// Set this parameter to false if you are sure that the input polyline is counter clockwise or if you want to skip this check.</param>
    /// <param name="uTurnBehavior"> Optional. Default value: <code>Offset2D.UTurnBehavior.Fail</code>.
    /// What to do at a 180 degree U-turn? Fail, Chamfer with two points, Use179 or Skip the point.</param>
    /// <param name="useUTurnBehaviorAbove"> Optional. Default value: <code>Cosine.``175.0``</code>.
    /// The angle between normals after which, instead of a normal miter, the joint is chamfered by adding an extra point.</param>
    /// <returns>A new 2D polyline.</returns>
    static member offset(   polyLine:Polyline2D,
                            constantOffsetDistance: float,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(true)>] checkOrientation:bool,
                            [<OPT;DEF(Offset2D.UTurn.Fail)>] uTurnBehavior: Offset2D.UTurn,
                            [<OPT;DEF(Cosine.``175.0``)>] useUTurnBehaviorAbove: float<Cosine.cosine>
                            ) : Polyline2D =
        let pts = polyLine.Points
        if pts.Count < 2 then
            fail $"Polyline2D.offset: Polyline2D must have at least 2 points but has {pts.Count} points. {polyLine}"

        let constantOffsetDistance =
            if checkOrientation && polyLine.SignedArea < 0.0 then
                constantOffsetDistance * -1.0
            else
                constantOffsetDistance

        // check if looping desired and curve is open
        if loop && Pt.distanceSq pts.First pts.Last > Offset2D.sqOpenTolerance then
            let closedPts = ResizeArr.closeLoop pts
            let normals = Offset2D.makeOffsetDirections closedPts
            let res  = Offset2D.offsetWithDirections(closedPts, normals, constantOffsetDistance,  uTurnBehavior, useUTurnBehaviorAbove)
            res.Pop() |> ignore // remove last point to open the polyline again
            Polyline2D.createDirectlyUnsafe res
        else
            let normals = Offset2D.makeOffsetDirections pts
            Offset2D.offsetWithDirections(pts , normals, constantOffsetDistance, uTurnBehavior, useUTurnBehaviorAbove)
            |> Polyline2D.createDirectlyUnsafe



    /// <summary> Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// By default this function raises an Exception on duplicate points, 180 degree U-Turns, and variable distances at colinear segments.
    /// But this can be configured with optional parameters.</summary>
    /// <param name="polyLine"> A 2D Polyline, open or closed.</param>
    /// <param name="multipleOffsetDistances">The parallel offset distances for each segment of the polyline.
    /// A positive distance offsets inwards in corners, a negative offset outwards.
    /// For open and closed polylines this list of distances must have one item less than number of points in the polyline.
    /// Except if the polyline is open and the loop parameter is set to true. Then points and distances list shall have the same count.</param>
    /// <param name="loop">bool, Optional (false).
    /// Set to true to explicitly consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="checkOrientation">bool, Optional(true). By default the algorithm always checks if the polyline is clockwise or counter clockwise.
    /// So that positive offset distances are always towards the inside of the polyline.
    /// Set this parameter to false if you are sure that the input polyline is counter clockwise or if you want to skip this check.</param>
    /// <param name="varDistParallelBehavior"> Optional. Default value: <code>Offset2D.VarDistParallelBehavior.Fail</code>.
    ///  What to do with colinear segments below 'useVarDistParallelBehaviorBelow' degrees when offset distances are different too.</param>
    /// <param name="uTurnBehavior"> Optional. Default value: <code>Offset2D.UTurnBehavior.Fail</code>.
    /// What to do at a 180 degree U-turn? Fail, Chamfer with two points, Use179 or Skip the point.</param>
    /// <param name="useVarDistParallelBehaviorBelow"> Optional. Default value: <code>Cosine.``5.0``</code>.
    /// The angle between normals below which points are considered colinear and VarDistParallelBehavior is applied if distances are not the same. </param>
    /// <param name="useUTurnBehaviorAbove"> Optional. Default value: <code>Cosine.``175.0``</code>.
    /// The angle between normals after which, instead of a normal miter, the joint is chamfered by adding an extra point.</param>
    /// <returns>A new 2D polyline.</returns>
    static member offsetVar(polyLine:Polyline2D,
                            multipleOffsetDistances: Collections.Generic.IList<float>,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(true)>] checkOrientation:bool,
                            [<OPT;DEF(Offset2D.VarDistParallel.Fail)>] varDistParallelBehavior: Offset2D.VarDistParallel,
                            [<OPT;DEF(Offset2D.UTurn.Fail)>] uTurnBehavior: Offset2D.UTurn,
                            [<OPT;DEF(Cosine.``5.0``)>] useVarDistParallelBehaviorBelow: float<Cosine.cosine>,
                            [<OPT;DEF(Cosine.``175.0``)>] useUTurnBehaviorAbove: float<Cosine.cosine>
                            ) : Polyline2D =
        let pts = polyLine.Points
        if pts.Count < 2 then
            fail $"Polyline2D.offset: Polyline2D must have at least 2 points but has {pts.Count} points. {polyLine}"

        let distances : Collections.Generic.IList<float> =
            if checkOrientation && polyLine.SignedArea < 0.0 then
                // reverse all distances if loop is clockwise
                let ds = ResizeArray(multipleOffsetDistances.Count)
                for i=0 to multipleOffsetDistances.Count - 1 do
                    ds.Add(multipleOffsetDistances[i] * -1.0)
                ds
            else
                multipleOffsetDistances

        // check if looping desired and polyline is open
        if loop && Pt.distanceSq pts.First pts.Last > Offset2D.sqOpenTolerance then
            if distances.Count <> pts.Count then
                fail ($"Polyline2D.offset: For open Polyline2D with loop=true the multipleOffsetDistances must have the same number of items as the polyline has points.\n" +
                      $"But polyline has {pts.Count} points and multipleOffsetDistances has {distances.Count} items.")
            let closedPts = ResizeArr.closeLoop pts
            let normals = Offset2D.makeOffsetDirections closedPts
            let res  = Offset2D.offsetVariableWithDirections(closedPts, normals, distances, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove)
            res.Pop() |> ignore // remove last point to open the polyline again
            Polyline2D res
        else
            let normals = Offset2D.makeOffsetDirections pts
            Offset2D.offsetVariableWithDirections(pts , normals, distances, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove)
            |> Polyline2D

    /// <summary> Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// By default this function raises an Exception on duplicate points, 180 degree U-Turns, and variable distances at colinear segments.
    /// But this can be configured with optional parameters.</summary>
    /// <param name="polyLine"> A 2D Polyline, open or closed. </param>
    /// <param name="multipleOffsetDistances">The parallel offset distances for each segment of the polyline.
    /// A positive distance offsets inwards in corners, a negative offset outwards.
    /// For open and closed polylines this list of distances must have one item less than number of points in the polyline.
    /// Except if the polyline is open and the loop parameter is set to true. Then points and distances list shall have the same count.
    /// </param>
    /// <param name="loop">bool, Optional (false).
    /// Set to true to explicitly consider last point and first point to be from a closed loop, even if they are not at the same location.</param>
    /// <param name="checkOrientation">bool, Optional(true). By default the algorithm always checks if the polyline is clockwise or counter clockwise.
    /// So that positive offset distances are always towards the inside of the polyline.
    /// Set this parameter to false if you are sure that the input polyline is counter clockwise or if you want to skip this check.</param>
    /// <param name="varDistParallelBehavior"> Optional. Default value: <code>Offset2D.VarDistParallelBehavior.Fail</code>.
    ///  What to do with colinear segments below 'useVarDistParallelBehaviorBelow' degrees when offset distances are different too.</param>
    /// <param name="uTurnBehavior"> Optional. Default value: <code>Offset2D.UTurnBehavior.Fail</code>.
    /// What to do at a 180 degree U-turn? Fail, Chamfer with two points, Use179 or Skip the point.</param>
    /// <param name="useVarDistParallelBehaviorBelow"> Optional. Default value: <code>Cosine.``5.0``</code>.
    /// The angle between normals below which points are considered colinear and VarDistParallelBehavior is applied if distances are not the same. </param>
    /// <param name="useUTurnBehaviorAbove"> Optional. Default value: <code>Cosine.``175.0``</code>.
    /// The angle between normals after which, instead of a normal miter, the joint is chamfered by adding an extra point.</param>
    /// <returns>A new 2D polyline.</returns>
    static member offsetVar(polyLine:Polyline2D,
                            multipleOffsetDistances: float [],
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(true)>] checkOrientation:bool,
                            [<OPT;DEF(Offset2D.VarDistParallel.Fail)>] varDistParallelBehavior: Offset2D.VarDistParallel,
                            [<OPT;DEF(Offset2D.UTurn.Fail)>] uTurnBehavior: Offset2D.UTurn,
                            [<OPT;DEF(Cosine.``5.0``)>] useVarDistParallelBehaviorBelow: float<Cosine.cosine>,
                            [<OPT;DEF(Cosine.``175.0``)>] useUTurnBehaviorAbove: float<Cosine.cosine>
                            ) : Polyline2D =
        Polyline2D.offsetVar(polyLine, ResizeArray multipleOffsetDistances, loop, checkOrientation, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove)



    /// Tries to find a self intersection in the Polyline2D.
    /// Also returns Some if segments are just touching.
    /// If found returns the intersection point and the indices of the two segments that intersect.
    /// If no intersection is found returns None.
    /// This is an O(n^2) algorithm and should only be used for small Polylines.
    static member tryFindSelfIntersection (pl:Polyline2D) : Option<Pt * int * int> =
        let pts = pl.Points
        let segmentVs = pl.SegmentVectors
        let segLastIdx = segmentVs.LastIndex
        let brs = ResizeArray(segmentVs.Count)
        let mutable pp = pts.[0]
        for i = 1 to pts.LastIndex do
            let p = pts.[i]
            let br = BRect.create(pp,p)
            brs.Add br
            pp <- p

        // O(n^2) check of all segments against each other
        let rec checkSegs i j =
            if i > segLastIdx then
                None // exit loop

            elif j > segLastIdx then
                checkSegs (i + 1) (i + 3) // +3 to skip adjacent segments

            // first do a quick bounding rectangle overlap test because most lines do not intersect
            elif not <|  BRect.doOverlap brs.[i] brs.[j] then
                checkSegs i (j + 1) // move on to next segment

            else
                match XLine2D.tryIntersect(pts.[i], pts.[j], segmentVs.[i], segmentVs.[j]) with
                | Some pt ->
                    Some (pt, i, j)
                | None    ->
                    checkSegs i (j + 1)

        if pl.IsClosed then
            checkSegs 0 2
        else
            // if the polyline is open check first and last segment.
            match XLine2D.tryIntersect(pts.First, pts.SecondLast, segmentVs.First, segmentVs.Last) with
            | Some pt ->
                Some (pt, 0, segmentVs.LastIndex)
            | None    ->
                checkSegs 0 2 // start with segment 0 and segment 2 to avoid checking adjacent segments




    /// <summary>Count how many times the polygon winds around the point.
    /// If the result is 0 then the point is outside of the Polyline2D.
    /// A non-zero value indicates the point is inside.</summary>
    /// <param name="point">The point to test.</param>
    /// <param name="pl">The closed Polyline2D.</param>
    /// <returns>The winding number. Zero means outside, non-zero means inside.</returns>
    /// <remarks>The first and last point of the Polyline2D need to be identical for correct results.
    /// Boundary cases: Points exactly on edges or vertices may return inconsistent results
    /// due to floating-point precision. For points on horizontal edges, behavior depends on
    /// the edge direction. This method Handles self-intersecting polygons more intuitively than the pl.Contains method.</remarks>
    static member inline windingNumber (point:Pt) (pl:Polyline2D) : int =
        pl.WindingNumber point


    /// <summary>Tests if a point is inside the closed Polyline2D using the ray casting algorithm.</summary>
    /// <param name="pt">The point to test.</param>
    /// <param name="pl">The closed Polyline2D.</param>
    /// <returns>TRUE if the point is inside, FALSE otherwise.</returns>
    /// <remarks>The first and last point of the Polyline2D need to be identical for correct results.
    /// Self-intersecting polygons give "alternating" inside/outside regions
    /// Uses ray casting: runs an infinite horizontal ray (increasing x, fixed y) from the test point
    /// and counts edge crossings. Each crossing toggles inside/outside state (Jordan curve theorem).
    /// Always returns FALSE if the Polyline2D has less than 3 points.
    /// Boundary cases: Points exactly on edges or vertices have implementation-specific behavior.
    /// Horizontal edges are handled by the strict inequality convention (pi.Y > y) != (pj.Y > y).
    /// Points on left/bottom edges tend to be considered inside, right/top edges outside.
    /// The result may differ from checking the pl.WindingNumber 0 for boundary points.</remarks>
    static member inline contains (pt:Pt) (pl:Polyline2D) =
        pl.Contains pt



    [<Obsolete("Use polyline2D.CloseInPlace instead.")>]
    member p.CloseIfOpen(t) = p.CloseInPlace(t)


    [<Obsolete("Renamed to Polyline2D.subPolyline")>]
    static member segment a b (pl:Polyline2D) :Polyline2D =
        Polyline2D.subPolyline a b pl

[<Obsolete("Use Euclid.Loop has been removed from Euclid in v0.20.0. use Polyline2D instead.",true)>]
type Loop private  () =

    [<Obsolete("Use Euclid.Loop has been removed from Euclid in v0.20.0. use Polyline2D instead.",true)>]
    static member create () =
        failwithf "Euclid.Loop has been removed from Euclid in v0.20.0. use Polyline2D instead."