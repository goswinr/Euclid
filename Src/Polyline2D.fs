namespace Euclid

open System
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors
open Euclid.EuclidCollectionUtilities


module R = ResizeArr

module private Polyline2DUtil =

    let inline countPts (xys: ResizeArray<float>) = R.len xys / 2

    let failPointIndex methodName idx (xys: ResizeArray<float>) =
        fail $"Polyline2D.{methodName}: index {idx} is out of range for Polyline2D with {countPts xys} points."

    let inline getPt i (xys: ResizeArray<float>) = Pt(xys.[i * 2], xys.[i * 2 + 1])

    let inline setCoordXY i x y (xys: ResizeArray<float>) =
        xys.[i * 2    ] <- x
        xys.[i * 2 + 1] <- y

    let inline appendXY x y (xys: ResizeArray<float>) =
        xys.Add x
        xys.Add y

    let inline copy (xys: ResizeArray<float>) : ResizeArray<float> =
        xys.GetRange(0, xys.Count)


open Polyline2DUtil

/// A class holding a list of 2D points representing a mutable 2D Polyline.
/// If the last point is the same as the first point, the Polyline2D is considered closed.
/// The source-of-truth storage is an interleaved float buffer: x0, y0, x1, y1, ...
// [<Struct>] // if it was a struct the Polyline2D() constructor would set _XYs to null, not empty.
[<NoEquality; NoComparison>] // because its made up from floats
[<DataContract>] // for using DataMember on fields
type Polyline2D private (xys: ResizeArray<float>) =

    // /// Create a new empty Polyline2D
    // new () = Polyline2D(ResizeArray<float>()) // do not allow this, maybe it wil be a struct in the future, and then this constructor would set _XYZs to null, not empty.

    /// Create a new empty Polyline2D with predefined point count capacity.
    new (capacity:int) =
        Polyline2D(ResizeArray<float>(capacity * 2))


    /// Gets the X and Y interleaved ResizeArray of the Polyline2D: x0, y0, x1, y1, ...
    /// This is the live internal buffer, so changes to the list will be reflected in the Polyline2D.
    [<DataMember>]
    member _.XYs : ResizeArray<float> =
        xys

    /// Gets the X and Y interleaved ResizeArray of the Polyline2D: x0, y0, x1, y1, ...
    /// This is the live internal buffer, so changes to the list will be reflected in the Polyline2D.
    static member inline getXYs (p:Polyline2D) : ResizeArray<float> =
        p.XYs

    /// Converts the float buffer of the Polyline2D into a list of Points. Use .XYs to access the live internal buffer.
    member _.AsPoints : ResizeArray<Pt> =
        let pts = ResizeArray<Pt>(countPts xys)
        let len = R.len xys
        let mutable i = 0
        while i < len do
            pts.Add(Pt(xys.[i], xys.[i + 1]))
            i <- i + 2
        pts

    /// Converts the float buffer of the Polyline2D into a list of Points. Use .XYs to access the live internal buffer.
    static member inline asPoints (p : Polyline2D) : ResizeArray<Pt> =
        p.AsPoints

    // #endregion
    // #region Get /Set

    /// Gets the x coordinate of the point at the given position.
    /// (does xys.[position * 2] internally)
    member _.GetX (position:int) : float =
        #if DEBUG || CHECK_EUCLID
        let len = xys.Count
        if position < 0 || position > len / 2 - 1 then
            failPointIndex "GetX" position xys
        #endif
        xys.[position * 2]

    /// Gets the x coordinate of the point at the given index.
    /// (does xys.[position * 2] internally)
    static member inline getX (position:int) (p:Polyline2D) : float =
        p.GetX position

    /// Gets the y coordinate of the point at the given position.
    /// (does xys.[position * 2 + 1] internally)
    member _.GetY (position:int) : float =
        #if DEBUG || CHECK_EUCLID
        let len = xys.Count
        if position < 0 || position > len / 2 - 1 then
            failPointIndex "GetY" position xys
        #endif
        xys.[position * 2 + 1]

    /// Gets the y coordinate of the point at the given index.
    /// (does xys.[position * 2 + 1] internally)
    static member inline getY (position:int) (p:Polyline2D) : float =
        p.GetY position

    /// Gets the point at the given position.
    /// (does Pt(xys.[position * 2], xys.[position * 2 + 1]) internally)
    member p.GetPt (position:int) : Pt =
        #if DEBUG || CHECK_EUCLID
        let len = xys.Count
        if position < 0 || position > len / 2 - 1 then
            failPointIndex "GetPt" position xys
        #endif
        getPt position xys

    /// Gets the point at the given position.
    /// (does Pt(xys.[position * 2], xys.[position * 2 + 1]) internally)
    static member inline getPt (position:int) (p:Polyline2D) : Pt =
        p.GetPt position

    /// Sets the point at given position to the given point.
    /// ( sets xys.[position * 2] and xys.[position * 2 + 1] internally)
    member p.SetPt (position:int, pt:Pt) : unit =
        #if DEBUG || CHECK_EUCLID
        let len = xys.Count
        if position < 0 || position > len / 2 - 1 then
            failPointIndex "SetPt" position xys
        #endif
        setCoordXY position pt.X pt.Y xys

    /// Sets the point at given position to the given point.
    /// ( sets xys.[position * 2] and xys.[position * 2 + 1] internally)
    static member inline setPt (position:int) (pt:Pt) (p:Polyline2D) : unit =
        p.SetPt (position, pt)

    /// Sets the x and y coordinates of the point at the given position.
    /// On a closed Polyline2D, setting the first or last point will set both to the same point.
    /// Raises an error if the position is out of range.
    /// (sets xys.[position * 2] and xys.[position * 2 + 1] internally)
    member p.SetPointXYClosed (position:int, x:float, y:float) : unit =
        #if DEBUG || CHECK_EUCLID
        let len = xys.Count
        if position < 0 || position > len / 2 - 1 then
            failPointIndex "SetPointXYClosed" position xys
        #endif
        let wasClosed = p.IsClosed
        if wasClosed && position = 0 then
            setCoordXY (p.PointCount-1) x y xys
        elif wasClosed && position = p.PointCount-1 then
            setCoordXY 0 x y xys
        setCoordXY position x y xys

    /// Sets the x and y coordinates of the point at the given index.
    /// On a closed Polyline2D, setting the first or last point will set both to the same point.
    /// Raises an error if the index is out of range.
    /// (sets xys.[position * 2] and xys.[position * 2 + 1] internally)
    static member inline setPointXYClosed (x:float) (y:float) (position:int) (p:Polyline2D) : unit =
        p.SetPointXYClosed (position, x, y)

    /// Sets the x and y coordinates of the point at the given index.
    /// NOTE: setting the first or last point on a closed Polyline2D might open it.
    /// (sets xys.[position * 2] and xys.[position * 2 + 1] internally )
    member p.SetPointXY (position:int, x:float, y:float) : unit =
        #if DEBUG || CHECK_EUCLID
        let len = xys.Count
        if position < 0 || position > len / 2 - 1 then
            failPointIndex "SetPointXY" position xys
        #endif
        setCoordXY position x y xys

    /// Sets the x and y coordinates of the point at the given index.
    /// NOTE: setting the first or last point on a closed Polyline2D might open it.
    /// (sets xys.[position * 2] and xys.[position * 2 + 1] internally )
    static member inline setPointXY (x:float) ( y:float) (position:int) (p:Polyline2D) : unit =
        p.SetPointXY (position, x, y)

    /// Adds a point from x and y coordinates.
    member _.AddXY( x:float, y:float ) : unit =
        appendXY x y xys

    /// Adds a point from x and y coordinates.
    static member inline addXY (x:float) (y:float) (p:Polyline2D) : unit =
        p.AddXY( x, y )

    /// Adds a point to the end of the Polyline2D.
    member p.AddPoint (pt:Pt) : unit =
        appendXY pt.X pt.Y xys

    /// Adds a point to the end of the Polyline2D.
    static member inline addPoint (pt:Pt) (p:Polyline2D) : unit =
        p.AddPoint pt

    /// Gets or sets first point of the Polyline2D
    /// This is the point at index 0.
    /// Same as Polyline2D.FirstPoint
    member p.Start
        with get() : Pt =
            if p.PointCount < 1 then failTooFewPoly2D "Start.get" 1 p.PointCount
            Pt(xys.[0], xys.[1])
        and set(v:Pt) : unit=
            if p.PointCount < 1 then failTooFewPoly2D "Start.set" 1 p.PointCount
            xys.[0] <- v.X
            xys.[1] <- v.Y

    /// Gets first point of the Polyline2D
    static member inline start (p:Polyline2D) : Pt =
        p.Start

    /// Gets or sets last or end point of the Polyline2D
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline2D.LastPoint
    member p.End
        with get() : Pt =
            if p.PointCount < 1 then failTooFewPoly2D "End.get" 1 p.PointCount
            Pt(xys.SecondLast, xys.Last)
        and set(v:Pt) : unit =
            if p.PointCount < 1 then failTooFewPoly2D "End.set" 1 p.PointCount
            xys.SecondLast <- v.X
            xys.Last       <- v.Y

    /// Gets the last or end point of the Polyline2D
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline2D.LastPoint
    static member inline end' (p:Polyline2D) : Pt =
        p.End

    /// Gets or sets the last point of the Polyline2D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline2D.End
    member p.LastPoint
        with get() : Pt=
            if p.PointCount < 1 then failTooFewPoly2D "LastPoint.get" 1 p.PointCount
            Pt(xys.SecondLast, xys.Last)
        and set(v:Pt) : unit =
            if p.PointCount < 1 then failTooFewPoly2D "LastPoint.set" 1 p.PointCount
            xys.SecondLast <- v.X
            xys.Last       <- v.Y

    /// Gets the last point of the Polyline2D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline2D.End
    static member inline lastPoint (p:Polyline2D) : Pt =
        p.LastPoint

    /// Gets or sets the second last point of the Polyline2D.
    member p.SecondLastPoint
        with get() : Pt =
            if p.PointCount < 2 then failTooFewPoly2D "SecondLastPoint.get" 2 p.PointCount
            Pt(xys.[xys.Count - 4], xys.[xys.Count - 3])
        and set(v:Pt) : unit =
            if p.PointCount < 2 then failTooFewPoly2D "SecondLastPoint.set" 2 p.PointCount
            xys.[xys.Count - 4] <- v.X
            xys.[xys.Count - 3] <- v.Y

    /// Gets the second last point of the Polyline2D.
    static member inline secondLastPoint (p:Polyline2D) : Pt =
        p.SecondLastPoint

    /// Gets or sets the second point of the Polyline2D.
    /// This is the point at index 1.
    member p.SecondPoint
        with get() : Pt =
            if p.PointCount < 2 then failTooFewPoly2D "SecondPoint.get" 2 p.PointCount
            Pt(xys.[2], xys.[3])
        and set(v:Pt) : unit =
            if p.PointCount < 2 then failTooFewPoly2D "SecondPoint.set" 2 p.PointCount
            xys.[2] <- v.X
            xys.[3] <- v.Y

    /// Gets the second point of the Polyline2D.
    /// This is the point at index 1.
    static member inline secondPoint (p:Polyline2D) : Pt =
        p.SecondPoint

    /// Gets or sets the first point of the Polyline2D.
    /// This is the point at index 0.
    /// Same as Polyline2D.Start
    member p.FirstPoint
        with get() : Pt =
            if p.PointCount < 1 then failTooFewPoly2D "FirstPoint.get" 1 p.PointCount
            Pt(xys.[0], xys.[1])
        and set(v:Pt) : unit =
            if p.PointCount < 1 then failTooFewPoly2D "FirstPoint.set" 1 p.PointCount
            xys.[0] <- v.X
            xys.[1] <- v.Y

    /// Gets the first point of the Polyline2D.
    /// This is the point at index 0.
    /// Same as Polyline2D.Start
    static member inline firstPoint (p:Polyline2D) : Pt =
        p.FirstPoint


    // #endregion
    // #region ToString

    /// Nicely formatted string representation of the Polyline2D including its length.
    override p.ToString() : string =
        let pc = p.PointCount
        if pc = 0 then
            "empty Euclid.Polyline2D."
        elif p.IsClosed then
            $"closed Euclid.Polyline2D with length {p.Length}, from {pc} points"
        else
            $"open Euclid.Polyline2D with length {p.Length}, from {pc} points"

    /// Format Polyline2D into string including its length.
    member p.AsString : string =
        let pc = p.PointCount
        if pc = 0 then
            "empty Polyline2D."
        elif p.IsClosed then
            $"closed Polyline2D with length {p.Length}, from {pc} points"
        else
            $"open Polyline2D with length {p.Length}, from {pc} points"

    /// Format Polyline2D into string including its length.
    static member inline asString (p : Polyline2D) : string =
        if p.PointCount = 0 then
            "empty Polyline2D."
        elif p.IsClosed then
            $"closed Polyline2D with length {p.Length}, from {p.PointCount} points"
        else
            $"open Polyline2D with length {p.Length}, from {p.PointCount} points"

    /// Format a 2D polyline into an F# code string that can be used to recreate the polyline.
    member p.AsFSharpCode : string =
        let ptsAsCode =
            p.AsPoints
            |> R.map _.AsFSharpCode
            |> String.concat "; "
        $"Polyline2D.createFromPts [| {ptsAsCode} |]"

    /// Format a 2D polyline into an F# code string that can be used to recreate the polyline.
    static member inline asFSharpCode (p : Polyline2D) : string =
        p.AsFSharpCode

    /// Creates a copy of the Polyline2D
    /// Same as polyline.Clone()
    member p.Duplicate(): Polyline2D =
        Polyline2D(copy xys)

    /// Creates a copy of the Polyline2D
    /// Same as polyline.Clone()
    static member inline duplicate (p : Polyline2D) : Polyline2D =
        p.Duplicate()

    /// Creates a copy of the Polyline2D.
    /// Same as polyline.Duplicate()
    member p.Clone(): Polyline2D =
        Polyline2D(copy xys)

    /// Creates a copy of the Polyline2D.
    /// Same as polyline.Duplicate()
    static member inline clone (p : Polyline2D) : Polyline2D =
        p.Clone()

    /// Gets the count of points in the Polyline2D
    member p.PointCount : int =
        countPts xys

    /// Gets the number of points in the Polyline2D.
    static member inline pointCount (p:Polyline2D) : int =
        p.PointCount

    /// Gets the count of segments in the Polyline2D
    /// This is poly.Points.Count - 1
    member p.SegmentCount : int =
        max 0 (p.PointCount - 1 )

    /// Gets the number of segments in the Polyline2D.
    static member inline segmentCount (p:Polyline2D) : int =
        p.SegmentCount

    /// Gets the length of the Polyline2D
    /// Returns 0.0 if there are less than 2 points.
    member p.Length : float =
        let mutable l = 0.0
        let len =  R.len xys
        if len >= 4 then
            let mutable px = xys.[0]
            let mutable py = xys.[1]
            let mutable i = 2
            while i < len do
                let x = xys.[i]
                let y = xys.[i + 1]
                let dx = x - px
                let dy = y - py
                l <- l + sqrt (dx * dx + dy * dy)
                px <- x
                py <- y
                i <- i + 2
        l

    /// Gets the length of the Polyline2D.
    /// The sum of the lengths of all segments.
    static member inline length (p:Polyline2D) : float =
        p.Length

    /// Gets the segment at index i of the Polyline2D.
    member p.GetSegment(i:int) : Line2D =
        if i < 0 || i > p.PointCount - 2 then
            fail $"Polyline2D.GetSegment: index {i} is out of range for Polyline2D with {p.PointCount} points."
        let j = i * 2
        Line2D(xys.[j], xys.[j + 1], xys.[j + 2], xys.[j + 3])

    /// Gets the segment at index i of the Polyline2D.
    static member inline getSegment (i:int) (p:Polyline2D) : Line2D =
        p.GetSegment i

    /// Gets the last segment of the Polyline2D.
    member p.LastSegment : Line2D =
        if p.PointCount < 2 then failTooFewPoly2D "LastSegment" 2 p.PointCount
        let i = xys.Count - 4
        Line2D(xys.[i], xys.[i + 1], xys.[i + 2], xys.[i + 3])

    /// Gets the last segment of the Polyline2D.
    static member inline lastSegment (p:Polyline2D) : Line2D =
        p.LastSegment

    /// Gets the first segment of the Polyline2D.
    member p.FirstSegment : Line2D =
        if p.PointCount < 2 then failTooFewPoly2D "FirstSegment" 2 p.PointCount
        Line2D(xys.[0], xys.[1], xys.[2], xys.[3])

    /// Gets the first segment of the Polyline2D.
    static member inline firstSegment (p:Polyline2D) : Line2D =
        p.FirstSegment

    /// Returns all segments of the Polyline2D as a list of Line2D.
    member p.Segments : ResizeArray<Line2D> =
        let lns = ResizeArray(p.SegmentCount)
        if p.PointCount < 2 then
            lns
        else
            let mutable i = 0
            while i < xys.Count - 2 do
                lns.Add(Line2D(xys.[i], xys.[i + 1], xys.[i + 2], xys.[i + 3]))
                i <- i + 2
            lns

    /// Returns all segments of the Polyline2D as a list of Line2D.
    static member inline segments (p:Polyline2D) : ResizeArray<Line2D> =
        p.Segments

    /// Returns the line vectors of all segments of the Polyline2D as a list of Vc.
    /// The length of the list is one less than the point count.
    member p.SegmentVectors : ResizeArray<Vc> =
        let vs = ResizeArray(p.SegmentCount)
        let len = xys.Count
        if len < 4 then
            vs
        else
            let mutable ax = xys.[0]
            let mutable ay = xys.[1]
            let mutable i = 2
            while i < len do
                let bx = xys.[i]
                let by = xys.[i + 1]
                vs.Add(Vc(bx - ax, by - ay))
                ax <- bx
                ay <- by
                i <- i + 2
            vs

    /// Returns the line vectors of all segments of the Polyline2D as a list of Vc.
    static member inline segmentVectors (p:Polyline2D) : ResizeArray<Vc> =
        p.SegmentVectors

    /// Returns the line vectors of all segments of the Polyline2D as a flat list of x and y components.
    /// The length of the list is 2 less than the xys count, so one less vector than points in the polyline.
    member p.SegmentVectorsXY : ResizeArray<float> =
        let xys = p.XYs
        let len = xys.Count
        let vs = ResizeArray(max 0 (len - 2)) // max 0 to avoid negative capacity on empty polyline
        if len < 4 then
            vs
        else
            let mutable ax = xys.[0]
            let mutable ay = xys.[1]
            let mutable i = 2
            while i < len do
                let bx = xys.[i]
                let by = xys.[i + 1]
                vs.Add (bx - ax)
                vs.Add (by - ay)
                ax <- bx
                ay <- by
                i <- i + 2
            vs

    /// Returns the line vectors of all segments of the Polyline2D as a flat list of x and y components.
    /// The length of the list is 2 less than the xys count, so one less vector than points in the polyline.
    static member inline segmentVectorsXY (p:Polyline2D) : ResizeArray<float> =
        p.SegmentVectorsXY

    /// Gets bounding rectangle of the Polyline2D
    member p.BoundingRectangle : BRect =
        if p.PointCount = 0 then failEmptySeq "Polyline2D.BoundingRectangle" "Polyline2D"
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        let mutable i = 0
        let len = xys.Count
        while i < len do
            let x = xys.[i]
            let y = xys.[i + 1]
            minX <- min minX x
            minY <- min minY y
            maxX <- max maxX x
            maxY <- max maxY y
            i <- i + 2
        BRect.createUnchecked(minX, minY, maxX, maxY)

    /// Gets bounding rectangle of the Polyline2D
    static member inline boundingRectangle (p:Polyline2D) : BRect =
        p.BoundingRectangle

    /// Tests if Polyline2D start and end points are exactly the same.
    /// Returns False if the Polyline2D has less than 3 points.
    member p.IsClosed : bool =
        p.PointCount > 2
        && xys.[0] = xys.SecondLast
        && xys.[1] = xys.Last

    /// Tests if Polyline2D start and end points are exactly the same.
    /// Returns False if the Polyline2D has less than 3 points.
    static member inline isClosed (p:Polyline2D) : bool =
        p.IsClosed

    /// Tests if Polyline2D is closed within given tolerance.
    /// Returns False if the Polyline2D has less than 3 points.
    member p.IsAlmostClosed tolerance : bool =
        if p.PointCount <= 2 then
            false
        else
            let dx = xys[0] - xys.SecondLast
            let dy = xys[1] - xys.Last
            dx * dx + dy * dy <= tolerance*tolerance // <= needed so it works with 0.0

    /// Tests if Polyline2D is closed within given tolerance.
    /// Returns False if the Polyline2D has less than 3 points.
    static member inline isAlmostClosed tolerance (p:Polyline2D) : bool =
        p.IsAlmostClosed tolerance

    /// Returns new Polyline2D in reversed Order.
    member p.Reverse () : Polyline2D =
        let rev = ResizeArray<float>(xys.Count)
        let mutable i = xys.Count - 2
        while i >= 0 do
            rev.Add xys.[i]
            rev.Add xys.[i + 1]
            i <- i - 2
        Polyline2D.createDirectly rev

    /// Returns new Polyline2D in reversed Order.
    static member reverse (p:Polyline2D) : Polyline2D =
        p.Reverse()

    /// Reverse order of the Polyline2D in place.
    member p.ReverseInPlace() : unit =
        let mutable left = 0
        let mutable right = xys.Count - 2
        while left < right do
            let lx = xys.[left]
            let ly = xys.[left + 1]
            xys.[left]     <- xys.[right]
            xys.[left + 1] <- xys.[right + 1]
            xys.[right]     <- lx
            xys.[right + 1] <- ly
            left <- left + 2
            right <- right - 2

    /// <summary>Reverse order of the Polyline2D in place.</summary>
    /// <param name="p">The Polyline2D to reverse.</param>
    /// <returns>A reference to the same Polyline2D as the input</returns>
    static member reverseInPlace (p:Polyline2D) : Polyline2D =
        p.ReverseInPlace()
        p

    /// <summary>Close the Polyline2D if it is not already closed.
    /// If the ends are closer than the tolerance, the last point is set equal to the first point.
    /// Otherwise the start point is added to the end of the Polyline2D.</summary>
    /// <param name="toleranceForAddingPoint">Optional. 1e-6 by default
    /// The tolerance used to decide whether to snap the last point to the first point.</param>
    /// <returns>Unit.</returns>
    member p.CloseInPlace([<OPT;DEF(1e-6)>]toleranceForAddingPoint:float) : unit =
        if p.PointCount < 3 then failTooFewPoly2D "CloseInPlace" 3 p.PointCount
        let sx = xys.[0]
        let sy = xys.[1]
        let ex = xys.SecondLast
        let ey = xys.Last
        let dx = sx - ex
        let dy = sy - ey
        if dx * dx + dy * dy <= toleranceForAddingPoint*toleranceForAddingPoint then // <= needed so it works with 0.0
            xys.SecondLast <- sx
            xys.Last       <- sy
        else
            xys.Add sx
            xys.Add sy

    /// <summary>Close the Polyline2D in place using the given tolerance.
    /// If the ends are closer than the tolerance, the last point is set equal to the first point.
    /// Otherwise the start point is added to the end of the Polyline2D.</summary>
    /// <param name="toleranceForAddingPoint">The tolerance used to decide whether to snap the last point to the first point.</param>
    /// <param name="pl">The Polyline2D to close.</param>
    /// <returns>A reference to the same Polyline2D as the input</returns>
    static member closeInPlace (toleranceForAddingPoint:float) (pl:Polyline2D) : Polyline2D =
        pl.CloseInPlace toleranceForAddingPoint
        pl

    /// Calculates the signed area of the Polyline2D .
    /// If it is positive the Polyline2D is Counter Clockwise.
    /// Polyline does not need to be exactly closed.
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self intersecting Polylines the result is invalid.
    /// Raises an error on an empty Polyline2D.
    member p.SignedArea : float =
        //https://helloacm.com/sign-area-of-irregular-polygon/
        if p.PointCount = 0 then failTooFewPoly2D "SignedArea" 1 p.PointCount
        let mutable area = 0.0
        let mutable tx = xys.SecondLast // calculate from last to first too
        let mutable ty = xys.Last
        let mutable i = 0
        let len = xys.Count
        while i < len do
            let nx = xys.[i]
            let ny = xys.[i + 1]
            area <- area + (tx - nx) * (ny + ty)
            tx <- nx
            ty <- ny
            i <- i + 2
        area * 0.5

    /// Calculates the signed area of the Polyline2D .
    /// If it is positive the Polyline2D is Counter Clockwise.
    /// Polyline does not need to be exactly closed.
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self intersecting Polylines the result is invalid.
    static member inline signedArea (p:Polyline2D) : float =
        p.SignedArea

    /// Calculates the area of the Polyline2D .
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self intersecting Polylines the result is invalid.
    member p.Area : float =
        abs p.SignedArea

    /// Calculates the area of the Polyline2D .
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self intersecting Polylines the result is invalid.
    static member inline area (p:Polyline2D) : float =
        abs p.SignedArea

    /// Test if Polyline2D is CounterClockwise.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated.
    /// If it is positive the Polyline2D is Counter Clockwise.
    member p.IsCounterClockwise : bool =
        let  area = p.SignedArea
        if abs(area) < UtilEuclid.zeroLengthTolerance then
            fail $"Polyline2D.IsCounterClockwise: Polyline2D the area is zero: {p}"
        area > 0.0

    /// Test if Polyline2D is CounterClockwise.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated.
    /// If it is positive the Polyline2D is Counter Clockwise.
    static member inline isCounterClockwise (p:Polyline2D) : bool =
        p.IsCounterClockwise

    /// Test if Polyline2D is Clockwise.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated.
    /// If it is negative the Polyline2D is Clockwise.
    member p.IsClockwise : bool =
        let area = p.SignedArea
        if abs(area) < UtilEuclid.zeroLengthTolerance then
            fail $"Polyline2D.IsClockwise: Polyline2D the area is zero: {p}"
        area < 0.0

    /// Test if Polyline2D is Clockwise.
    /// The Polyline2D does not need to be actually closed.
    /// The signed area of the Polyline2D is calculated.
    /// If it is negative the Polyline2D is Clockwise.
    static member inline isClockwise (p:Polyline2D) : bool =
        p.IsClockwise

    /// Returns the point at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    /// If the parameter is within 1e-6 of an integer value, the integer value is used as parameter.
    member pl.EvaluateAt(t:float) : Pt =
        let i = int t       // integer part of the parameter
        let p = t - float i // fractional part of the parameter
        let count = pl.PointCount
        let lastParam = float (count - 1)

        // values next to  the start of the polyline:
        if t < 1e-6 then
            if t < -1e-6 then
                fail $"Polyline2D.EvaluateAt: Parameter {t} is less than 0.0"
            pl.Start

        // values next to  the end of the polyline:
        elif t > (lastParam - 1e-6) then
            if t > (lastParam + 1e-6) then
                fail $"Polyline2D.EvaluateAt: Parameter {t} is more than last point index {lastParam}."
            pl.End

        // return point if point is almost matching and integer
        elif p < 1e-6 then
            getPt i xys
        elif p > 1.0 - 1e-6 then
            getPt (i+1) xys
        else
            let xyIdx = i * 2
            let x = xys.[xyIdx]
            let y = xys.[xyIdx + 1]
            Pt(x + (xys.[xyIdx + 2] - x) * p,
               y + (xys.[xyIdx + 3] - y) * p)

    /// Returns the point at a given parameter on the Polyline2D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    static member evaluateAt (t:float) (pl:Polyline2D) : Pt =
        pl.EvaluateAt t

    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.ClosestParameterXY(x:float, y:float) : float =
        if pl.PointCount = 0 then  fail "Polyline2D.ClosestParameter failed on empty Polyline2D"
        let px = x
        let py = y
        let mutable ax = xys.[0]
        let mutable ay = xys.[1]
        let mutable minT = 0.0
        let mutable seg = 0
        let mutable minDistSq =
            let dx = px - ax
            let dy = py - ay
            dx * dx + dy * dy // this handles the case of a single point Polyline2D
        let mutable i = 2
        let mutable segmentIndex = 0
        let len = xys.Count
        while i < len do
            let bx = xys.[i]
            let by = xys.[i + 1]
            let dx = bx - ax
            let dy = by - ay
            if dx <> 0.0 || dy <> 0.0 then // exclude duplicate points in polyline, zero distance between points on this segment
                let t = ((px - ax) * dx + (py - ay) * dy) / (dx * dx + dy * dy)
                let t' = max 0.0 (min 1.0 t)
                let projX = ax + dx * t'
                let projY = ay + dy * t'
                let dpx = px - projX
                let dpy = py - projY
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minT <- t'
                    seg <- segmentIndex
            else
                let dpx = px - ax
                let dpy = py - ay
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minT <- 0.0
                    seg <- segmentIndex
            ax <- bx
            ay <- by
            segmentIndex <- segmentIndex + 1
            i <- i + 2
        float seg + minT

    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.ClosestParameter(p:Pt) : float =
        pl.ClosestParameterXY(p.X, p.Y)

    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    static member inline closestParameterXY (pl:Polyline2D) (x:float) (y:float) : float =
        pl.ClosestParameterXY (x, y)

    /// Returns the parameter on the Polyline2D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline2D starts at 0.0 and ends at points.Count - 1.0 .
    static member inline closestParameter (pl:Polyline2D) (pt:Pt) : float =
        pl.ClosestParameter pt

    /// Returns the point on the Polyline2D that is the closest point to the given point.
    /// This might be a point on a segment or a vertex of the Polyline2D.
    member pl.ClosestPoint(p:Pt) : Pt =
        if pl.PointCount = 0 then  fail "Polyline2D.ClosestPoint failed on empty Polyline2D"
        let px = p.X
        let py = p.Y
        let mutable ax = xys.[0]
        let mutable ay = xys.[1]
        let mutable minPt = Pt(ax, ay) // this handles the case of a single point Polyline2D
        let mutable minDistSq =
            let dx = px - ax
            let dy = py - ay
            dx * dx + dy * dy // this handles the case of a single point Polyline2D
        let mutable i = 2
        let len = xys.Count
        while i < len do
            let bx = xys.[i]
            let by = xys.[i + 1]
            let dx = bx - ax
            let dy = by - ay
            if dx <> 0.0 || dy <> 0.0 then // zero distance between points
                let t = ((px - ax) * dx + (py - ay) * dy) / (dx * dx + dy * dy)
                let t' = max 0.0 (min 1.0 t)
                let projX = ax + dx * t'
                let projY = ay + dy * t'
                let dpx = px - projX
                let dpy = py - projY
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minPt <- Pt(projX, projY)
            else
                let dpx = px - ax
                let dpy = py - ay
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minPt <- Pt(ax, ay)
            ax <- bx
            ay <- by
            i <- i + 2
        minPt

    /// Returns the point on the Polyline2D that is the closest point to the given point.
    static member inline closestPoint (pl:Polyline2D) (pt:Pt) : Pt =
        pl.ClosestPoint pt

    /// Returns the index into the Polylines point list of the point that is closest to the given point.
    member pl.ClosestPointIndex(p:Pt) : int =
        if pl.PointCount = 0 then  fail "Polyline2D.ClosestPointIndex failed on empty Polyline2D"
        let px = p.X
        let py = p.Y
        let mutable minIdx = 0
        let mutable minDistSq =
            let dx = px - xys.[0]
            let dy = py - xys.[1]
            dx * dx + dy * dy
        let mutable i = 2
        let mutable idx = 1
        let len = xys.Count
        while i < len do
            let dx = px - xys.[i]
            let dy = py - xys.[i + 1]
            let dSq = dx * dx + dy * dy
            if dSq < minDistSq then
                minDistSq <- dSq
                minIdx <- idx
            idx <- idx + 1
            i <- i + 2
        minIdx

    /// Returns the index into the Polylines point list of the point that is closest to the given point.
    static member inline closestPointIndex (pl:Polyline2D) (pt:Pt) : int =
        pl.ClosestPointIndex pt

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    member pl.DistanceToXY(x:float, y:float) : float =
        if pl.PointCount = 0 then  fail "Polyline2D.DistanceTo failed on empty Polyline2D"
        let px = x
        let py = y
        let mutable ax = xys.[0]
        let mutable ay = xys.[1]
        let mutable minDistSq =
            let dx = px - ax
            let dy = py - ay
            dx * dx + dy * dy // this handles the case of a single point Polyline2D
        let mutable i = 2
        let len = xys.Count
        while i < len do
            let bx = xys.[i]
            let by = xys.[i + 1]
            let dx = bx - ax
            let dy = by - ay
            if dx <> 0.0 || dy <> 0.0 then // zero distance between points
                let t = ((px - ax) * dx + (py - ay) * dy) / (dx * dx + dy * dy)
                let t' = max 0.0 (min 1.0 t)
                let projX = ax + dx * t'
                let projY = ay + dy * t'
                let dpx = px - projX
                let dpy = py - projY
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
            else
                let dpx = px - ax
                let dpy = py - ay
                let distSq = dpx * dpx + dpy * dpy
                if distSq < minDistSq then
                    minDistSq <- distSq
            ax <- bx
            ay <- by
            i <- i + 2
        sqrt minDistSq

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    member pl.DistanceTo(p:Pt) : float =
        pl.DistanceToXY(p.X, p.Y)

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    static member inline distanceToXY (pl:Polyline2D) (x:float) (y:float) : float =
        pl.DistanceToXY (x, y)

    /// Returns the distance of the test point to the closest point on the Polyline2D.
    static member inline distanceTo (pl:Polyline2D) (pt:Pt) : float =
        pl.DistanceTo pt

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
        let inline isLeft ax ay bx by  =
            let axp = px - ax // cross product unrolled to avoid allocation of temp Vc
            let ayp = py - ay
            let bdx = bx - ax
            let bdy = by - ay
            let det = axp * bdy - ayp * bdx
            // if   det >  1e-12 then  1
            // elif det < -1e-12 then -1
            if   det > 0 then  1
            elif det < 0 then -1
            else 0

        let mutable winding = 0
        let len = xys.Count
        if len > 0 then
            let mutable thisX = xys.[0]
            let mutable thisY = xys.[1]
            let mutable i = 2
            while i < len do
                let nextX = xys.[i]
                let nextY = xys.[i + 1]
                if thisY <= py then
                    if nextY > py && isLeft thisX thisY nextX nextY  > 0 then
                        winding <- winding - 1
                else
                    if nextY <= py && isLeft thisX thisY nextX nextY  < 0 then
                        winding <- winding + 1
                thisX <- nextX
                thisY <- nextY
                i <- i + 2

        winding

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
    /// <param name="x">The X coordinate of the point to test.</param>
    /// <param name="y">The Y coordinate of the point to test.</param>
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
    member p.ContainsXY (x: float, y: float) : bool =
        if p.PointCount < 3 then
            false
        else
            // taken from Polylabel algorithm
            // also see https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html
            let mutable inside = false
            let mutable pix = xys.[0]
            let mutable piy = xys.[1]
            let mutable i = 2
            let len = xys.Count
            while i < len do
                let pjx = xys.[i]
                let pjy = xys.[i + 1]
                if  (piy > y) <> (pjy > y)
                &&  x < (pjx - pix) * (y - piy) / (pjy - piy) + pix
                    then
                        inside <- not inside
                pix <- pjx
                piy <- pjy
                i <- i + 2
            inside

    /// <summary>Tests if a point is inside the closed Polyline2D using the ray casting algorithm.</summary>
    /// <param name="pt">The point to test.</param>
    /// <returns>TRUE if the point is inside, FALSE otherwise.</returns>
    member p.Contains (pt: Pt) : bool =
        p.ContainsXY (pt.X, pt.Y)

    /// <summary>Tests if a point is inside the closed Polyline2D using the ray casting algorithm.</summary>
    /// <param name="x">The X coordinate of the point to test.</param>
    /// <param name="y">The Y coordinate of the point to test.</param>
    /// <param name="pl">The closed Polyline2D.</param>
    /// <returns>TRUE if the point is inside, FALSE otherwise.</returns>
    static member inline containsXY (x:float) (y:float) (pl:Polyline2D) : bool =
        pl.ContainsXY (x, y)

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
    static member inline contains (pt:Pt) (pl:Polyline2D) : bool =
        pl.Contains pt

    /// Calculates the shortest distance from the test point to the polyline with `DistanceTo`,
    /// then signs that value by testing containment via the ray-casting based `Contains` helper.
    /// Returns a positive distance for points that lie inside the polyline boundary and negative otherwise.
    /// For reliable results the polyline should be closed and have identical first and last vertices.
    member pl.SignedDistanceToXY (x:float, y:float) :float =
        let distance = pl.DistanceToXY (x, y)
        if pl.ContainsXY (x, y) then distance else -distance

    /// Calculates the shortest distance from the test point to the polyline with `DistanceTo`,
    /// then signs that value by testing containment via the ray-casting based `Contains` helper.
    /// Returns a positive distance for points that lie inside the polyline boundary and negative otherwise.
    /// For reliable results the polyline should be closed and have identical first and last vertices.
    member pl.SignedDistanceTo (point: Pt)  :float =
        pl.SignedDistanceToXY (point.X, point.Y)

    /// Calculates the shortest distance from the test point to the polyline with `DistanceTo`,
    /// then signs that value by testing containment via the ray-casting based `Contains` helper.
    /// Returns a positive distance for points that lie inside the polyline boundary and negative otherwise.
    /// For reliable results the polyline should be closed and have identical first and last vertices.
    static member inline signedDistanceToXY (x:float) (y:float) (pl:Polyline2D) : float =
        pl.SignedDistanceToXY (x, y)

    /// Calculates the shortest distance from the test point to the polyline with `DistanceTo`,
    /// then signs that value by testing containment via the ray-casting based `Contains` helper.
    /// Returns a positive distance for points that lie inside the polyline boundary and negative otherwise.
    /// For reliable results the polyline should be closed and have identical first and last vertices.
    static member inline signedDistanceTo (point: Pt) (pl:Polyline2D) : float =
        pl.SignedDistanceTo point

    /// Returns the average center of all points of the Polyline2D.
    member p.Center : Pt =
        if p.PointCount = 0 then failTooFewPoly2D "Center" 1 p.PointCount
        let mutable x = 0.0
        let mutable y = 0.0
        let mutable i = 0
        let len = xys.Count
        while i < len do
            x <- x + xys.[i]
            y <- y + xys.[i + 1]
            i <- i + 2
        Pt(x / float p.PointCount, y / float p.PointCount)

    /// Returns the average center of all points of the Polyline2D.
    static member inline center (p:Polyline2D) : Pt =
        p.Center

    /// Scales the 2D polyline by a given factor.
    /// Scale center is World Origin 0,0
    member p.Scale (factor:float) : Polyline2D =
        let cs = ResizeArray<float>(xys.Count)
        let mutable i = 0
        let len = xys.Count
        while i < len do
            cs.Add(xys.[i] * factor)
            cs.Add(xys.[i + 1] * factor)
            i <- i + 2
        Polyline2D(cs)

    /// Scales the Polyline2D by a given factor.
    /// Scale center is World Origin 0,0
    /// Returns a new Polyline2D.
    static member scale (factor:float) (pl:Polyline2D) : Polyline2D =
        pl.Scale factor

    /// Scales the 2D polyline by a given factor on a given center point.
    member p.ScaleOn (cen:Pt) (factor:float) : Polyline2D =
        let cx = cen.X
        let cy = cen.Y
        let cs = ResizeArray<float>(xys.Count)
        let mutable i = 0
        let len = xys.Count
        while i < len do
            cs.Add(cx + (xys.[i    ] - cx) * factor)
            cs.Add(cy + (xys.[i + 1] - cy) * factor)
            i <- i + 2
        Polyline2D(cs)

    /// Scales the 2D polyline by a given factor on a given center point.
    static member inline scaleOn (cen:Pt) (factor:float) (pl:Polyline2D) : Polyline2D =
        pl.ScaleOn cen factor

    /// Returns a Polyline2D moved by a vector. Same as Polyline2D.move and Polyline2D.translate.
    member p.Move (v:Vc) : Polyline2D =
        Polyline2D.translate v p

    /// Move a Polyline2D by a vector. (same as Polyline2D.translate)
    static member move (v:Vc) (pl:Polyline2D)  : Polyline2D =
        Polyline2D.translate v pl

    /// Returns a Polyline2D moved by a given distance in X direction.
    member p.MoveX (distance:float) : Polyline2D =
        Polyline2D.moveX distance p

    /// Returns a Polyline2D moved by a given distance in X direction.
    static member moveX (distance:float) (pl:Polyline2D)  : Polyline2D =
        let cs = ResizeArray<float>(pl.XYs.Count)
        let mutable i = 0
        let xys = pl.XYs
        let len = xys.Count
        while i < len do
            cs.Add(xys.[i] + distance)
            cs.Add(xys.[i + 1])
            i <- i + 2
        Polyline2D(cs)

    /// Returns a Polyline2D moved by a given distance in Y direction.
    member p.MoveY (distance:float) : Polyline2D =
        Polyline2D.moveY distance p

    /// Returns a Polyline2D moved by a given distance in Y direction.
    static member moveY (distance:float) (pl:Polyline2D)  : Polyline2D =
        let cs = ResizeArray<float>(pl.XYs.Count)
        let mutable i = 0
        let xys = pl.XYs
        let len = xys.Count
        while i < len do
            cs.Add(xys.[i])
            cs.Add(xys.[i + 1] + distance)
            i <- i + 2
        Polyline2D(cs)

    /// Rotation a Polyline2D around the World-Origin by a Rotation2D.
    member p.Rotate (r:Rotation2D) : Polyline2D =
        Polyline2D.rotate r p

    /// Rotation a Polyline2D around Z-Axis.
    static member rotate (r:Rotation2D) (pl:Polyline2D) : Polyline2D =
        let cs = ResizeArray<float>(pl.XYs.Count)
        let mutable i = 0
        let xys = pl.XYs
        let len = xys.Count
        let sin = r.Sin
        let cos = r.Cos
        while i < len do
            let x = xys.[i]
            let y = xys.[i + 1]
            cs.Add (cos * x - sin * y)
            cs.Add (sin * x + cos * y)
            i <- i + 2
        Polyline2D(cs)

    /// Rotation a Polyline2D around a given center point by a Rotation2D.
    member p.RotateWithCenter (cen:Pt, r:Rotation2D) : Polyline2D =
        Polyline2D.rotateWithCenter cen r p


    // #endregion
    // #region LablePoint

    /// Rotation a Polyline2D round given center point an a local Z-axis.
    static member rotateWithCenter (cen:Pt) (r:Rotation2D) (pl:Polyline2D) : Polyline2D =
        let cs = ResizeArray<float>(pl.XYs.Count)
        let mutable i = 0
        let xys = pl.XYs
        let len = xys.Count
        let sin = r.Sin
        let cos = r.Cos
        let cx = cen.X
        let cy = cen.Y
        while i < len do
            let x = xys.[i] - cx
            let y = xys.[i + 1] - cy
            cs.Add (cx + cos * x - sin * y)
            cs.Add (cy + sin * x + cos * y)
            i <- i + 2
        Polyline2D(cs)


    // #endregion
    // #region Map and Iter

    /// Finds a point inside a closed Polyline2D that is the farthest away from the edges of the Polyline2D.
    /// Uses the Polylabel algorithm from Mapbox. It is a highly optimized algorithm specifically designed to find the
    /// pole of inaccessibility for polygons. The point within the polygon that is farthest from the edges,
    /// often used for optimal label placement.
    /// Adaptive Precision: Can trade accuracy for speed based on your needs.
    /// The precision must be a positive number, otherwise an error is raised.
    /// Returns the best point and its distance to the polygon edge. Supplying an open polyline is allowed,
    /// but the computed "inside" still assumes the points describe a closed boundary (first and last vertices should match).
    member pl.FindLabelPoint (precision: float) : Pt * float =
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
            fail $"Polyline2D.FindLabelPoint must have at least 1 point but has {pl.PointCount} points."
        if not (precision > 0.0) then // 'not >' instead of '<=' so it also catches NaN
            fail $"Polyline2D.FindLabelPoint: precision must be a positive number but is {precision}."

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

    /// Finds a point inside a closed Polyline2D that is the farthest away from the edges of the Polyline2D.
    /// Uses the Polylabel algorithm from Mapbox. It is a highly optimized algorithm specifically designed to find the
    /// pole of inaccessibility for polygons. The point within the polygon that is farthest from the edges,
    /// often used for optimal label placement.
    /// Adaptive Precision: Can trade accuracy for speed based on your needs.
    /// The precision must be a positive number, otherwise an error is raised.
    /// Returns the best point and its distance to the polygon edge. Supplying an open polyline is allowed,
    /// but the computed "inside" still assumes the points describe a closed boundary (first and last vertices should match).
    static member findLabelPoint (precision: float) (pl: Polyline2D) : Pt * float =
        pl.FindLabelPoint precision


    // #endregion
    // #region Static members

    /// Tests if Polyline2D is Clockwise.
    /// Returns the same instance if the Polyline2D is already Clockwise,
    /// otherwise returns a new reversed Polyline2D.
    static member inline ensureClockwise (pl:Polyline2D) : Polyline2D =
        if pl.IsClockwise then pl else pl.Reverse()

    /// Tests if Polyline2D is Counter Clockwise.
    /// Returns the same instance if the Polyline2D is already Counter Clockwise,
    /// otherwise returns a new reversed Polyline2D.
    static member inline ensureCounterClockwise (pl:Polyline2D) : Polyline2D =
        if pl.IsCounterClockwise then pl else pl.Reverse()

    /// Tests if Polyline2D is Clockwise.
    /// If not reverse the Polyline2D in place.
    /// Always returns the same instance.
    static member inline ensureClockwiseInPlace (pl:Polyline2D) : Polyline2D =
        if pl.IsCounterClockwise then pl.ReverseInPlace()
        pl

    /// Tests if Polyline2D is Counter Clockwise.
    /// If not reverse the Polyline2D in place.
    /// Always returns the same instance.
    static member inline ensureCounterClockwiseInPlace (pl:Polyline2D) : Polyline2D =
        if pl.IsClockwise then pl.ReverseInPlace()
        pl

    /// Move a Polyline2D by a vector. (same as Polyline2D.move)
    static member translate (v:Vc) (pl:Polyline2D)  : Polyline2D =
        let cs = ResizeArray<float>(pl.XYs.Count)
        let mutable i = 0
        let xys = pl.XYs
        let len = xys.Count
        while i < len do
            cs.Add(xys.[i]     + v.X)
            cs.Add(xys.[i + 1] + v.Y)
            i <- i + 2
        Polyline2D(cs)

    /// <summary>Apply a mapping function to each point in the Polyline2D. Returns new Polyline2D.</summary>
    /// <param name="mapping">A function that takes a point and returns a new point.</param>
    /// <param name="pl">The Polyline2D to map over.</param>
    /// <returns>A new Polyline2D with the mapped points.</returns>
    static member mapPt (mapping:Pt-> Pt) (pl:Polyline2D) : Polyline2D =
        let cs = ResizeArray<float>(pl.XYs.Count)
        let mutable i = 0
        let xys = pl.XYs
        let len = xys.Count
        while i < len do
            let pt = mapping (Pt(xys.[i], xys.[i + 1]))
            cs.Add pt.X
            cs.Add pt.Y
            i <- i + 2
        Polyline2D(cs)

    /// <summary>Apply a mapping function to each point in the Polyline2D with point position (not float index). Returns new Polyline2D.</summary>
    /// <param name="mapping">A function that takes the the position of the point (index/2) and the point itself, and returns a new point.
    /// </param>
    /// <param name="pl">The Polyline2D to map over.</param>
    /// <returns>A new Polyline2D with the mapped points.</returns>
    static member mapiPt (mapping:int -> Pt -> Pt) (pl:Polyline2D) : Polyline2D =
        let cs = ResizeArray<float>(pl.XYs.Count)
        let mutable i = 0
        let xys = pl.XYs
        let len = xys.Count
        while i < len do
            let pt = mapping (i/2) (Pt(xys.[i], xys.[i + 1]))
            cs.Add pt.X
            cs.Add pt.Y
            i <- i + 2
        Polyline2D(cs)

    /// <summary>Apply a mapping function to each point in the Polyline2D. Returns new Polyline2D.</summary>
    /// <param name="mapping">A function that takes the X and Y coordinates of a point and returns a new point.</param>
    /// <param name="pl">The Polyline2D to map over.</param>
    /// <returns>A new Polyline2D with the mapped points.</returns>
    static member map (mapping:float -> float -> Pt) (pl:Polyline2D) : Polyline2D =
        let cs = ResizeArray<float>(pl.XYs.Count)
        let mutable i = 0
        let xys = pl.XYs
        let len = xys.Count
        while i < len do
            let pt = mapping xys.[i] xys.[i + 1]
            cs.Add pt.X
            cs.Add pt.Y
            i <- i + 2
        Polyline2D(cs)

    /// <summary>Iterate over each point in the Polyline2D and perform an action.</summary>
    /// <param name="action">A function that takes a point.</param>
    /// <param name="pl">The Polyline2D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member iterPt (action:Pt -> unit) (pl:Polyline2D) : unit =
        let xys = pl.XYs
        let len = xys.Count
        let mutable i = 0
        while i < len do
            action (Pt(xys.[i], xys.[i + 1]))
            i <- i + 2

    /// <summary>Iterate over each point in the Polyline2D with index and perform an action.</summary>
    /// <param name="action">A function that takes the the position of the point (index/2) and the point itself.</param>
    /// <param name="pl">The Polyline2D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member iteriPt (action:int -> Pt -> unit) (pl:Polyline2D) : unit =
        let xys = pl.XYs
        let len = xys.Count
        let mutable i = 0
        while i < len do
            action (i/2) (Pt(xys.[i], xys.[i + 1]))
            i <- i + 2

    /// <summary>Apply a mapping function to each point in the Polyline2D with index. Returns new Polyline2D.</summary>
    /// <param name="mapping">A function that takes the index of the X coordinate (in the flat coordinate array) and the X and Y coordinates of a point and returns a new point.</param>
    /// <param name="pl">The Polyline2D to map over.</param>
    /// <returns>A new Polyline2D with the mapped points.</returns>
    static member mapi (mapping:int -> float -> float -> Pt) (pl:Polyline2D) : Polyline2D =
        let cs = ResizeArray<float>(pl.XYs.Count)
        let mutable i = 0
        let xys = pl.XYs
        let len = xys.Count
        while i < len do
            let pt = mapping i xys.[i] xys.[i + 1]
            cs.Add pt.X
            cs.Add pt.Y
            i <- i + 2
        Polyline2D(cs)

    /// <summary>Iterate over each point in the Polyline2D.</summary>
    /// <param name="action">A function that takes the X and Y coordinates of a point.</param>
    /// <param name="pl">The Polyline2D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member inline iter (action:float -> float -> unit) (pl:Polyline2D) : unit =
        let xys = pl.XYs
        let len = xys.Count
        let mutable i = 0
        while i < len do
            action xys.[i] xys.[i + 1]
            i <- i + 2

    /// <summary>Iterate over each point in the Polyline2D except the last point.</summary>
    /// <param name="action">A function that takes the X and Y coordinates of a point.</param>
    /// <param name="pl">The Polyline2D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member inline iterSkipLast (action:float -> float -> unit) (pl:Polyline2D) : unit =
        let xys = pl.XYs
        let len = xys.Count - 2
        let mutable i = 0
        while i < len do
            action xys.[i] xys.[i + 1]
            i <- i + 2

    /// <summary>Iterate over each point in the Polyline2D except the last point.</summary>
    /// <param name="action">A function that takes a point.</param>
    /// <param name="pl">The Polyline2D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member inline iterPtSkipLast (action:Pt-> unit) (pl:Polyline2D) : unit =
        let xys = pl.XYs
        let len = xys.Count - 2
        let mutable i = 0
        while i < len do
            action (Pt(xys.[i], xys.[i + 1]))
            i <- i + 2

    /// <summary>Iterate over each point in the Polyline2D with index.</summary>
    /// <param name="action">A function that takes the index of the X coordinate (in the flat coordinate array) and the X and Y coordinates of a point.</param>
    /// <param name="pl">The Polyline2D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member iteri (action:int -> float -> float -> unit) (pl:Polyline2D) : unit =
        let xys = pl.XYs
        let len = xys.Count
        let mutable i = 0
        while i < len do
            action i xys.[i] xys.[i + 1]
            i <- i + 2

    /// <summary>Find the first point in the Polyline2D that satisfies a given condition. Returns Some(point) if found, otherwise None.</summary>
    /// <param name="condition">A function that takes the X and Y coordinates of a point and returns a boolean indicating whether the condition is satisfied.</param>
    /// <param name="pl">The Polyline2D to search through.</param>
    /// <returns>Some(Pt) if a point satisfying the condition is found, otherwise None.</returns>
    static member inline tryFind (condition: float -> float -> bool) (pl:Polyline2D) : Pt option =
        let mutable result = None
        let xys = pl.XYs
        let len = xys.Count
        let mutable i = 0
        while i < len do
            let x = xys.[i]
            let y = xys.[i + 1]
            if condition x y then
                result <- Some (Pt(x, y))
                i <- len // exit loop
            else
                i <- i + 2
        result

    /// <summary>Find the last point in the Polyline2D that satisfies a given condition. Returns Some(point) if found, otherwise None.</summary>
    /// <param name="condition">A function that takes the X and Y coordinates of a point and returns a boolean indicating whether the condition is satisfied.</param>
    /// <param name="pl">The Polyline2D to search through.</param>
    /// <returns>Some(Pt) if a point satisfying the condition is found, otherwise None.</returns>
    static member inline tryFindLast (condition: float -> float -> bool) (pl:Polyline2D) : Pt option =
        let mutable result = None
        let xys = pl.XYs
        let mutable i = xys.Count - 2
        while i >= 0 do
            let x = xys.[i]
            let y = xys.[i + 1]
            if condition x y then
                result <- Some (Pt(x, y))
                i <- -2 // exit loop
            else
                i <- i - 2
        result

    /// <summary>Find the index of the first point in the Polyline2D that satisfies a given condition. Returns Some(index) if found, otherwise None.</summary>
    /// <param name="condition">A function that takes the X and Y coordinates of a point and returns a boolean indicating whether the condition is satisfied.</param>
    /// <param name="pl">The Polyline2D to search through.</param>
    /// <returns>Some(index of x) the index of the x value in the polylines flat array if found, otherwise None.</returns>
    static member inline tryFindIndex (condition: float -> float -> bool) (pl:Polyline2D) : int option =
        let mutable result = None
        let xys = pl.XYs
        let len = xys.Count
        let mutable i = 0
        while i < len do
            let x = xys.[i]
            let y = xys.[i + 1]
            if condition x y then
                result <- Some i
                i <- len // exit loop
            else
                i <- i + 2
        result

    /// <summary>Find the index of the last point in the Polyline2D that satisfies a given condition. Returns Some(index) if found, otherwise None.</summary>
    /// <param name="condition">A function that takes the X and Y coordinates of a point and returns a boolean indicating whether the condition is satisfied.</param>
    /// <param name="pl">The Polyline2D to search through.</param>
    /// <returns>Some(index of x) the index of the x value in the polylines flat array if found, otherwise None.</returns>
    static member inline tryFindLastIndex (condition: float -> float -> bool) (pl:Polyline2D) : int option =
        let mutable result = None
        let xys = pl.XYs
        let mutable i = xys.Count - 2
        while i >= 0 do
            let x = xys.[i]
            let y = xys.[i + 1]
            if condition x y then
                result <- Some i
                i <- -2 // exit loop
            else
                i <- i - 2
        result

    /// <summary>Iterate over each segment in the Polyline2D.</summary>
    /// <param name="action">A function that takes startX, startY, endX, and endY coordinates of a segment.</param>
    /// <param name="pl">The Polyline2D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member inline iterSegments (action: float -> float -> float -> float -> unit) (pl:Polyline2D) : unit =
        let xys = pl.XYs
        let len = xys.Count
        if len < 4 then
            () // not enough points for a segment
        else
            let mutable x = xys.[0]
            let mutable y = xys.[1]
            let mutable i = 2
            while i < len do
                let nextX = xys.[i]
                let nextY = xys.[i + 1]
                action x y nextX nextY
                x <- nextX
                y <- nextY
                i <- i + 2

    /// <summary>Iterate over each segment in the Polyline2D.</summary>
    /// <param name="action">A function that takes a Line2D representing the segment.</param>
    /// <param name="pl">The Polyline2D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member inline iterLineSegments (action: Line2D -> unit) (pl:Polyline2D) : unit =
        pl |> Polyline2D.iterSegments (fun x1 y1 x2 y2 -> action (Line2D(x1, y1, x2, y2)))


    /// <summary>Tests if two Polyline2D have the same point count and if their corresponding points are equal within a given tolerance.</summary>
    /// <param name="tol">The tolerance value for comparing the coordinates.</param>
    /// <param name="pl1">The first Polyline2D instance.</param>
    /// <param name="pl2">The second Polyline2D instance.</param>
    static member equalsTol (tol:float) (pl1:Polyline2D) (pl2:Polyline2D) : bool =
        if Object.ReferenceEquals(pl1, pl2) then
            true
        elif pl1.XYs.Count <> pl2.XYs.Count then
            false
        else
            let xys1 = pl1.XYs
            let xys2 = pl2.XYs
            let len = xys1.Count
            let mutable i = 0
            let mutable equal = true
            while i < len do
                if abs (xys1.[i] - xys2.[i]) > tol then
                    equal <- false
                    i <- len // exit loop
                else
                    i <- i + 1
            equal

    /// <summary>Tests if two Polyline2D have the same point count and if their corresponding points are exactly equal.</summary>
    /// <param name="pl1">The first Polyline2D instance.</param>
    /// <param name="pl2">The second Polyline2D instance.</param>
    /// <returns>True if the two Polyline2D instances are equal, otherwise false.</returns>
    static member equals (pl1:Polyline2D) (pl2:Polyline2D) : bool =
        if Object.ReferenceEquals(pl1, pl2) then
            true
        elif pl1.XYs.Count <> pl2.XYs.Count then
            false
        else
            let xys1 = pl1.XYs
            let xys2 = pl2.XYs
            let len = xys1.Count
            let mutable i = 0
            let mutable equal = true
            while i < len do
                if xys1.[i] <> xys2.[i] then
                    equal <- false
                    i <- len // exit loop
                else
                    i <- i + 1
            equal


    // #endregion
    // #region Create

    /// Creates a Polyline2D from a list of objects with X and Y members (uppercase).
    static member inline createFromXYMembers (xyObjs: seq< ^T >) : Polyline2D =
        let coordinates = ResizeArray<float>()
        for pt in xyObjs do
            let x = float (^T : (member X : _) pt)
            let y = float (^T : (member Y : _) pt)
            coordinates.Add x
            coordinates.Add y
        Polyline2D.createDirectly coordinates

    /// Creates a Polyline2D from a list of objects with x and y members (lowercase).
    static member inline createFromxyMembers (xyObjs: seq< ^T >) : Polyline2D =
        let coordinates = ResizeArray<float>()
        for pt in xyObjs do
            let x = float (^T : (member x : _) pt)
            let y = float (^T : (member y : _) pt)
            coordinates.Add x
            coordinates.Add y
        Polyline2D.createDirectly coordinates

    /// Create a new Polyline2D by copying over all points.
    /// This will allocate a new ResizeArray and copy all points.
    static member createFromPts(points: seq<Pt>) : Polyline2D =
        if isNull points then
            failNull "Polyline2D.createFromPts" "points"
        let xys = ResizeArray<float>()
        for pt in points do
            xys.Add pt.X
            xys.Add pt.Y
        Polyline2D(xys)

    /// Create a new Polyline2D by using the provided X and Y interleaved ResizeArray directly.
    /// Unsafe because later changes to the ResizeArray will be reflected in the Polyline2D.
    static member createDirectly (xys: ResizeArray<float>) : Polyline2D =
        if isNull xys then failNull "Polyline2D.createDirectly" "coordinates"
        if xys.Count % 2 <> 0 then
            fail $"Polyline2D.createDirectly: coordinate buffer must contain an even number of floats, but has {xys.Count} values."
        Polyline2D(xys)

    /// Create a new empty Polyline2D without any points.
    /// But predefined capacity.
    static member inline createEmpty (capacity:int) : Polyline2D =
        Polyline2D(capacity)

    /// Creates a counter-clockwise closed Polyline2D starting at the Origin of the Rect2D
    static member createFromRectCCW (r:Rect2D)  : Polyline2D =
        Polyline2D r.PointsXYLoopedCCW

    /// Creates a clockwise closed Polyline2D starting at the Origin of the Rect2D
    static member createFromRectCW (r:Rect2D)  : Polyline2D =
        Polyline2D r.PointsXYLoopedCW

    /// Creates a counter-clockwise Polyline2D starting at the MinX and MinY for the bounding Rectangle
    static member createFromBRectCCW (r:BRect)  : Polyline2D =
        let cs = ResizeArray<float>(10)
        cs.Add r.MinX; cs.Add r.MinY
        cs.Add r.MaxX; cs.Add r.MinY
        cs.Add r.MaxX; cs.Add r.MaxY
        cs.Add r.MinX; cs.Add r.MaxY
        cs.Add r.MinX; cs.Add r.MinY
        Polyline2D cs

    /// Creates a clockwise Polyline2D starting at the MinX and MinY for the bounding Rectangle
    static member createFromBRectCW (r:BRect)  : Polyline2D =
        let cs = ResizeArray<float>(10)
        cs.Add r.MinX; cs.Add r.MinY
        cs.Add r.MinX; cs.Add r.MaxY
        cs.Add r.MaxX; cs.Add r.MaxY
        cs.Add r.MaxX; cs.Add r.MinY
        cs.Add r.MinX; cs.Add r.MinY
        Polyline2D cs

    /// Returns new Polyline2D from point at Parameter a to point at Parameter b.
    /// If 'a' is bigger than 'b' then the new Polyline2D is in opposite direction.
    /// If the start parameter is less than 1e-4 below the next integer it snaps up to that vertex.
    /// If the end parameter is less than 1e-4 above the previous integer it snaps down to that vertex.
    /// Raises an error if a parameter is outside the domain of the Polyline2D (0.0 to points.Count - 1.0).
    static member subPolyline a b (pl:Polyline2D) :Polyline2D =
        let rev = a>b
        let u, v = if rev then b, a else a, b
        let np = Polyline2D.createEmpty (int(v-u)+2)
        // first point
        let ui = int u
        let uf = u - float ui
        if uf < 0.9999 then
            let p = pl.EvaluateAt u
            np.AddXY (p.X, p.Y)
        // inner points
        for i = int u + 1 to int v do
            if i >= 0 && i < pl.PointCount then
                np.AddXY (pl.GetX i, pl.GetY i)
        // last point
        let vi = int v
        let vf = v - float vi
        if vf > 1e-4 then
            let p = pl.EvaluateAt v
            np.AddXY (p.X, p.Y)
        // reverse if necessary
        if rev then
            np.ReverseInPlace()
        np

    /// Returns a new closed Polyline2D.
    /// If the first and last point are within the tolerance of each other, the last point is set equal to the first point.
    /// Otherwise one point is added.
    /// Raises an error if the Polyline2D has fewer than 3 points, same as CloseInPlace.
    static member close (toleranceForAddingPoint:float) (pl:Polyline2D) : Polyline2D =
        if pl.PointCount < 3 then failTooFewPoly2D "close" 3 pl.PointCount
        let np = pl.Duplicate()
        let sx = np.GetX 0
        let sy = np.GetY 0
        let lastIdx = np.PointCount - 1
        let ex = np.GetX lastIdx
        let ey = np.GetY lastIdx
        let dx = sx - ex
        let dy = sy - ey
        if dx * dx + dy * dy <= toleranceForAddingPoint * toleranceForAddingPoint then // <= needed so it works with 0.0
            np.SetPointXY (lastIdx, sx, sy) // set last point equal to first
        else
            np.AddXY (sx, sy)
        np


    // #endregion
    // #region Clean up

    /// <summary>Removes simple sharp U-Turns from a Polyline</summary>
    /// <param name="minCos"> The angle between segments so that they are considered a U-turn.
    /// For example, for 179.9 degrees use 'Cosine.``179.9``'. </param>
    /// <param name="polyLine"> A 2D Polyline, open or closed. </param>
    /// <remarks>The Offset2D module also has a removeUTurns function that takes precomputed unit vectors and is therefore more efficient.
    /// For nested U-turns and collinear points in U-turns segments use 'removeUTurnsDeeply'. That function calls repeatedly until no more U-turns are present.</remarks>
    /// <returns>If no U-turns are present, the List of points of the original Polyline2D is reused and a new Polyline2D is created with that list.
    /// If U-turns are present, a new ResizeArray of points is returned with simple U-turns removed.</returns>
    static member removeUTurns ( minCos:float<Cosine.cosine>) (polyLine:Polyline2D): Polyline2D =
        let vs  = Offset2D.makeUnitTangents polyLine.XYs
        let pts = Offset2D.removeUTurns(polyLine.XYs,vs,minCos)
        Polyline2D pts

    /// <summary>Removes all sharp U-Turns from a Polyline recursively until no more U-turns are present.
    /// This function calls 'removeUTurns' repeatedly until no more U-turns are present.
    /// Use this function when you have nested U-turns and collinear points in U-turns segments.</summary>
    /// <param name="minCos"> The angle between segments so that they are considered a U-turn.
    /// For example, for 179.9 degrees use 'Cosine.``179.9``'. </param>
    /// <param name="polyLine"> A 2D Polyline, open or closed. </param>
    /// <returns>If no U-turns are present, the List of points of the original Polyline2D is reused and a new Polyline2D is created with that list.
    /// If U-turns are present, a new ResizeArray of points is returned with all U-turns removed.</returns>
    static member removeUTurnsDeeply ( minCos:float<Cosine.cosine>) (polyLine:Polyline2D): Polyline2D =
        let mutable input = polyLine
        let mutable output = Polyline2D.removeUTurns minCos polyLine
        while output.PointCount <> input.PointCount do
            input  <- output
            output <- Polyline2D.removeUTurns minCos input
        output

    /// <summary>Removes consecutive duplicate points from the Polyline2D within a given tolerance.</summary>
    /// <param name="distanceTolerance"> The distance within which points are considered duplicates. </param>
    /// <param name="pl"> A 2D Polyline, open or closed. </param>
    /// <remarks>From a cluster of points that are closer than the distanceTolerance, only the first point is kept.
    /// Use 'Polyline2D.removeDuplicatePointsFaithfully' if you want to keep the edges in their position by re-intersecting segments.
    /// The position of start and end point is NOT changed. Use Polyline2D.close to ensure start and end point are identical.</remarks>
    static member removeDuplicatePoints (distanceTolerance:float) (pl:Polyline2D) : Polyline2D =
        let xys = pl.XYs
        if xys.Count < 4 then // single point or empty polyline
            pl
        else
            let toSq = distanceTolerance * distanceTolerance
            let nps = ResizeArray<float>(xys.Count)
            let mutable prevX = xys.[0]
            let mutable prevY = xys.[1]
            nps.Add prevX
            nps.Add prevY
            let mutable i = 2
            while i < xys.Count do
                let px = xys.[i]
                let py = xys.[i + 1]
                let dx = abs (px - prevX)
                let dy = abs (py - prevY)
                if dx * dx + dy * dy > toSq then
                    nps.Add px
                    nps.Add py
                    prevX <- px
                    prevY <- py
                i <- i + 2
            if nps.Count >= 4 then // if only one point is left keep the start point, don't overwrite it with the end point
                nps.[nps.Count - 2] <- xys.[xys.Count - 2] // ensure last point is not moved by the algorithm, it might be off by distanceTolerance
                nps.[nps.Count - 1] <- xys.[xys.Count - 1]
            Polyline2D.createDirectly nps

    /// <summary>Removes consecutive duplicate points from the Polyline2D within a given tolerance.</summary>
    /// <param name="distanceTolerance"> The distance within which points are considered duplicates. </param>
    /// <param name="pl"> A 2D Polyline, open or closed. </param>
    /// <remarks>This algorithm ensures to keep edges in their position by re-intersects segments if points are closer than the distanceTolerance but not identical.
    /// The position of start and end point is NOT changed. Use Polyline2D.close to ensure start and end point are identical.</remarks>
    static member removeDuplicatePointsFaithfully (distanceTolerance:float) (pl:Polyline2D) : Polyline2D =
        let xys = pl.XYs
        if xys.Count < 4 then // single point or empty polyline
            pl
        else
            let tolSq = distanceTolerance * distanceTolerance
            let nps = ResizeArray<float>(xys.Count)
            let mutable prevX = xys.[0]
            let mutable prevY = xys.[1]
            let mutable prevWasBad = false
            nps.Add prevX
            nps.Add prevY
            let mutable i = 2
            while i < xys.Count do
                let thisX = xys.[i]
                let thisY = xys.[i + 1]
                let dx = thisX - prevX
                let dy = thisY - prevY
                let dSq = dx * dx + dy * dy
                // the distance is to small:
                if dSq < tolSq then
                    if dSq > 1e-24 then
                        // if the points are not identical, we will try to keep line fidelity by re-intersecting segments.
                        prevWasBad <- true

                // the distance is OK:
                else
                    // (a) fix the last point if it was bad and not the first point
                    if prevWasBad && nps.Count >= 4 then // if nps has less than 2 points we keep the start point exactly. like at the end see below.
                        // else, if nps has at least 2 points, re-intersect the edge from lastBad to this with the edge from prev back to the previous ok point
                        let lastBadX = xys.[i - 2] // prev is the last point of a cluster of too close points, so we use this as the last point of the edge.
                        let lastBadY = xys.[i - 1]
                        let vLastX = thisX - lastBadX // Vc.create(lastBadPt, this)
                        let vLastY = thisY - lastBadY
                        let prevOkX = nps.[nps.Count - 4] // the point in nps before prev
                        let prevOkY = nps.[nps.Count - 3]
                        let vFirstX = prevOkX - prevX // Vc.create(prev, prevOkPt), prev is the first bad point
                        let vFirstY = prevOkY - prevY
                        match XLineXY.tryParameterA(lastBadX, lastBadY, prevX, prevY, vLastX, vLastY, vFirstX, vFirstY) with
                        |ValueSome t ->
                            // the expected parameter on A is somewhere round 0.0, so use -0.4 to 0.4 as range,
                            let x = lastBadX + t * vLastX // t is the parameter on line A (lastBad, vLast)
                            let y = lastBadY + t * vLastY
                            // the intersection point must stay within the distanceTolerance of the cluster it replaces,
                            // measured against prev (last OK point) and lastBad (start of the following OK segment).
                            let dPrevX = x - prevX
                            let dPrevY = y - prevY
                            let dLastX = x - lastBadX
                            let dLastY = y - lastBadY
                            if t > -0.4 && t < 0.4
                               && dPrevX * dPrevX + dPrevY * dPrevY < tolSq
                               && dLastX * dLastX + dLastY * dLastY < tolSq then
                                nps.[nps.Count - 2] <- x // replace prev that was already set
                                nps.[nps.Count - 1] <- y
                            else
                                // intersection is too far away or segments are near parallel, line fidelity is not kept, use the midpoint
                                nps.[nps.Count - 2] <- (lastBadX + prevX) * 0.5
                                nps.[nps.Count - 1] <- (lastBadY + prevY) * 0.5
                        |ValueNone -> // segments are parallel, no intersection possible
                            nps.[nps.Count - 2] <- (lastBadX + prevX) * 0.5 // in this case line fidelity is not kept, use the midpoint
                            nps.[nps.Count - 1] <- (lastBadY + prevY) * 0.5

                    // (b) just use this point, no last points to fix
                    nps.Add thisX
                    nps.Add thisY
                    prevX <- thisX
                    prevY <- thisY
                    prevWasBad <- false

                i <- i + 2

            // last point :
            if prevWasBad then
                // TODO it could be debated what to do if several close by points are at a the end
                // in this case we are maintaining the last point exactly. like we do with the first point.
                // this might lead to less fidelity in the last segment though.
                // an alternative with higher line fidelity would be to pull the last point onto the last good segment and use this as endpoint.
                // then the same would have to be done at the start.
                nps.[nps.Count - 2] <- xys.[xys.Count - 2]
                nps.[nps.Count - 1] <- xys.[xys.Count - 1]

            Polyline2D.createDirectly nps

    /// Removes consecutive duplicate points and collinear points from the Polyline2D within given tolerances.
    /// This algorithm allows the last and first point to be identical if the Polyline2D is closed.
    /// Collinear points are removed when the angle between segments is smaller than the cosine threshold (e.g. cosine of 0.5 degrees ).
    /// If the Polyline2D is closed and starts and ends with collinear segments, the first point is replaced with the last non-collinear point.
    /// So the joint of the loop is now moved to the last non-collinear point.
    /// So that there are no collinear segments even between start and end.
    /// Raises an error if all points are within the distanceTolerance of the first point.
    static member removeDuplicateAndCollinearPoints (angleTolerance:float<Cosine.cosine>) (distanceTolerance:float) (pl:Polyline2D) : Polyline2D =
        if angleTolerance < Cosine.``45.0`` then
            fail $"Polyline2D.removeDuplicateAndCollinearPoints: angleTolerance must be at least Cosine.``45.0`` ( that is 0.707) but was {angleTolerance} (= {acos (float angleTolerance) |> toDegrees} degrees)."
        if angleTolerance > Cosine.``0.01`` then
            fail $"Polyline2D.removeDuplicateAndCollinearPoints: angleTolerance must be at most Cosine.``0.01`` ( that is 0.999999984) but was {angleTolerance} (= {acos (float angleTolerance) |> toDegrees} degrees)."

        let xys = pl.XYs
        if xys.Count < 4 then // single point or empty polyline
            pl
        else
            let distTol = max distanceTolerance 1e-6 // vectors need to be longer than zero, otherwise unitizing would fail
            let nps = ResizeArray<float>(xys.Count)

            let lastIdx = xys.Count
            let mutable prevX = xys.[0]
            let mutable prevY = xys.[1]
            nps.Add prevX // add first  point
            nps.Add prevY

            // (1)find first non-duplicate point:
            let mutable thisX = xys.[2]
            let mutable thisY = xys.[3]
            let mutable len = sqrt ((thisX - prevX) * (thisX - prevX) + (thisY - prevY) * (thisY - prevY))
            let mutable i = 4
            while len < distTol && i < lastIdx do
                thisX <- xys.[i]
                thisY <- xys.[i + 1]
                len   <- sqrt ((thisX - prevX) * (thisX - prevX) + (thisY - prevY) * (thisY - prevY))
                i <- i + 2

            if len < distTol then
                fail $"Polyline2D.removeDuplicateAndCollinearPoints: all {xys.Count / 2} points are within the distanceTolerance {distTol} of the first point {Pt(prevX, prevY)}."

            // first unit vector from prev to this
            let firstVecX = (thisX - prevX) / len
            let firstVecY = (thisY - prevY) / len
            let mutable vPrevX = firstVecX
            let mutable vPrevY = firstVecY

            // (2) main loop:
            while i < lastIdx do
                let nextX = xys.[i]
                let nextY = xys.[i + 1]
                let vx = nextX - thisX
                let vy = nextY - thisY
                let len = vx * vx + vy * vy |> sqrt
                if len > distTol then
                    // not duplicate, now check if collinear
                    let vNextX = vx / len
                    let vNextY = vy / len
                    let cos : float<Cosine.cosine> = LanguagePrimitives.FloatWithMeasure (vPrevX * vNextX + vPrevY * vNextY)
                    if cos < angleTolerance then
                        // not collinear , keep this point
                        nps.Add thisX
                        nps.Add thisY
                        prevX <- thisX
                        prevY <- thisY
                        vPrevX <- vNextX // advance previous vector only when point kept
                        vPrevY <- vNextY
                    thisX <- nextX // always advance this point
                    thisY <- nextY
                i <- i + 2

            // (3) handle last segment to first point
            if pl.IsAlmostClosed distTol then
                // closed polyline, now check if last and first segment are collinear
                let cos : float<Cosine.cosine> = LanguagePrimitives.FloatWithMeasure (vPrevX * firstVecX + vPrevY * firstVecY)
                if cos < angleTolerance then
                    // not collinear , keep the original end point
                    nps.Add xys.[xys.Count - 2]
                    nps.Add xys.[xys.Count - 1]
                else
                    // collinear , replace first point with last non-collinear point
                    nps.[0] <- nps.[nps.Count - 2]
                    nps.[1] <- nps.[nps.Count - 1]
            else
                if abs (thisX - prevX) > distTol || abs (thisY - prevY) > distTol then
                    // open polyline , just add last point if not duplicate
                    nps.Add thisX
                    nps.Add thisY
                else
                    // ensure last point is not changed, it might be off by distanceTolerance
                    nps.[nps.Count - 2] <- xys.[xys.Count - 2]
                    nps.[nps.Count - 1] <- xys.[xys.Count - 1]

            Polyline2D.createDirectly nps



    // #endregion
    // #region Offset

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
    /// Set this parameter to FALSE if you want to skip the orientation check.
    /// Clockwise polylines will offset to the outside, counter-clockwise polylines to the inside.</param>
    /// <param name="uTurnBehavior"> Optional. Default value: `Offset2D.UTurn.Fail`.
    /// What to do at a 180 degree U-turn? Fail, Chamfer with two points, Use179 or Skip the point.</param>
    /// <param name="useUTurnBehaviorAbove"> Optional. Default value: `Cosine.``175.0`` `.
    /// The angle between normals after which, instead of a normal miter, the joint is chamfered by adding an extra point.</param>
    /// <returns>A new 2D polyline.</returns>
    static member offset(   polyLine:Polyline2D,
                            constantOffsetDistance: float,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(true)>] checkOrientation:bool,
                            [<OPT;DEF(Offset2D.UTurn.Fail)>] uTurnBehavior: Offset2D.UTurn,
                            [<OPT;DEF(Cosine.``175.0``)>] useUTurnBehaviorAbove: float<Cosine.cosine>
                            ) : Polyline2D =
        let xys = polyLine.XYs
        let ptCount = polyLine.PointCount
        if ptCount < 2 then
            fail $"Polyline2D.offset: Polyline2D must have at least 2 points but has {ptCount} points. {polyLine}"

        let constantOffsetDistance =
            if checkOrientation && polyLine.SignedArea < 0.0 then
                constantOffsetDistance * -1.0
            else
                constantOffsetDistance

        // check if looping desired and curve is open
        let dx = xys.[0] - xys.[xys.Count - 2]
        let dy = xys.[1] - xys.[xys.Count - 1]
        if loop && dx * dx + dy * dy > Offset2D.sqOpenTolerance then
            let closedXYs = ResizeArray<float>(xys.Count + 2)
            closedXYs.AddRange xys
            closedXYs.Add xys.[0]
            closedXYs.Add xys.[1]
            let normals = Offset2D.makeOffsetDirections closedXYs
            let res  = Offset2D.offsetWithDirections(closedXYs, normals, constantOffsetDistance,  uTurnBehavior, useUTurnBehaviorAbove)
            res.RemoveAt(res.Count - 1) // remove last point to open the polyline again
            res.RemoveAt(res.Count - 1)
            Polyline2D.createDirectly res
        else
            let normals = Offset2D.makeOffsetDirections xys
            Offset2D.offsetWithDirections(xys, normals, constantOffsetDistance, uTurnBehavior, useUTurnBehaviorAbove)
            |> Polyline2D.createDirectly

    /// Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// Raises an Exception on duplicate points and 180 degree U-Turns.
    /// Positive offset distances are always towards the inside of the polyline, negative offset distances to the outside.
    static member inline offset' (offsetDistance: float) (pl:Polyline2D) : Polyline2D =
        Polyline2D.offset( pl, offsetDistance)

    /// <summary> Offsets a Polyline in 2D space by finding the local offset in each corner.
    /// Auto detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// By default this function raises an Exception on duplicate points, 180 degree U-Turns, and variable distances at collinear segments.
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
    /// Set this parameter to FALSE if you want to skip the orientation check.
    /// Clockwise polylines will offset to the outside, counter-clockwise polylines to the inside.</param>
    /// <param name="varDistParallelBehavior"> Optional. Default value: `Offset2D.VarDistParallel.Fail`.
    ///  What to do with collinear segments below 'useVarDistParallelBehaviorBelow' degrees when offset distances are different too.</param>
    /// <param name="uTurnBehavior"> Optional. Default value: `Offset2D.UTurn.Fail`.
    /// What to do at a 180 degree U-turn? Fail, Chamfer with two points, Use179 or Skip the point.</param>
    /// <param name="useVarDistParallelBehaviorBelow"> Optional. Default value: `Cosine.``5.0`` `.
    /// The angle between normals below which points are considered collinear and VarDistParallelBehavior is applied if distances are not the same. </param>
    /// <param name="useUTurnBehaviorAbove"> Optional. Default value: `Cosine.``175.0`` `.
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
        let xys = polyLine.XYs
        let ptCount = polyLine.PointCount
        if ptCount < 2 then
            fail $"Polyline2D.offsetVar: Polyline2D must have at least 2 points but has {ptCount} points. {polyLine}"

        let distances :  Collections.Generic.IList<float> =
            if checkOrientation && polyLine.SignedArea < 0.0 then
                // reverse all distances if loop is clockwise
                let ds = ResizeArray(multipleOffsetDistances.Count)
                for i=0 to multipleOffsetDistances.Count - 1 do
                    ds.Add(multipleOffsetDistances[i] * -1.0)
                ds
            else
                multipleOffsetDistances

        // check if looping desired and polyline is open
        let dx = xys.[0] - xys.[xys.Count - 2]
        let dy = xys.[1] - xys.[xys.Count - 1]
        if loop && dx * dx + dy * dy > Offset2D.sqOpenTolerance then
            if distances.Count <> ptCount then
                fail ($"Polyline2D.offsetVar: For open Polyline2D with loop=true the multipleOffsetDistances must have the same number of items as the polyline has points.\n" +
                      $"But polyline has {ptCount} points and multipleOffsetDistances has {distances.Count} items.")
            let closedXYs = ResizeArray<float>(xys.Count + 2)
            closedXYs.AddRange xys
            closedXYs.Add xys.[0]
            closedXYs.Add xys.[1]
            let normals = Offset2D.makeOffsetDirections closedXYs
            let res  = Offset2D.offsetVariableWithDirections(closedXYs, normals, distances, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove)
            res.RemoveAt(res.Count - 1) // remove last point to open the polyline again
            res.RemoveAt(res.Count - 1)
            Polyline2D.createDirectly res
        else
            let normals = Offset2D.makeOffsetDirections xys
            Offset2D.offsetVariableWithDirections(xys, normals, distances, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove)
            |> Polyline2D.createDirectly

    // #endregion
    // #region Self Intersection

    /// Tries to find a self intersection in Polyline2D.
    /// This function is optimized for polylines with fewer than 50 points.
    /// A closed Polyline2D, is not reported as a self intersection.
    /// Also returns Some if two non-adjacent segments are just touching.
    /// If found returns the first intersection point and the indices of the two segments that intersect.
    /// If no intersection is found returns None.
    /// Returns None for Polylines with fewer than 3 points (less than two segments).
    /// This is an iterative O(n^2) algorithm
    /// For larger polylines with more than 50 points, tryFindSelfIntersectionWithBRect is faster on .NET, but never on Fable JS.
    static member tryFindSelfIntersection(pl:Polyline2D) : Option<Pt * int * int> =
        // see Test\BenchmarkPolyline2DSelfIntersection.fsx for performance tests and comparison of this algorithm with tryFindSelfIntersectionBig.
        let xys = pl.XYs
        let len = xys.Count
        if len < 6 then
            None // a polyline with fewer than two segments cannot intersect itself
        else
            let segmentVs = pl.SegmentVectorsXY
            let segLastIdx = segmentVs.LastIndex
            let isClosed = pl.IsAlmostClosed 1e-7
            let mutable result = None

            let firstRowEnd = if isClosed then segLastIdx - 3 else segLastIdx
            let mutable j = 4 // +2 to skip the adjacent segment
            let mutable ptix = xys.[0]
            let mutable ptiy = xys.[1]
            let mutable sVix = segmentVs.[0]
            let mutable sViy = segmentVs.[1]
            while result.IsNone && j <= firstRowEnd do
                let mutable ptjx = xys.[j]
                let mutable ptjy = xys.[j + 1]
                let mutable sVjx = segmentVs.[j]
                let mutable sVjy = segmentVs.[j + 1]
                match XLineXY.tryIntersect(ptix, ptiy, ptjx, ptjy, sVix, sViy, sVjx, sVjy) with
                | ValueSome pt -> result <- Some (pt, 0, j / 2)
                | ValueNone    -> ()
                j <- j + 2

            // The remaining segments from index 1 onward have no closure special case.
            let mutable i = 2
            while result.IsNone && i <= segLastIdx do
                let mutable j = i + 4 // +2 to skip the adjacent segment
                ptix <- xys.[i]
                ptiy <- xys.[i + 1]
                sVix <- segmentVs.[i]
                sViy <- segmentVs.[i + 1]
                while result.IsNone && j <= segLastIdx do
                    let mutable ptjx = xys.[j]
                    let mutable ptjy = xys.[j + 1]
                    let mutable sVjx = segmentVs.[j]
                    let mutable sVjy = segmentVs.[j + 1]
                    match XLineXY.tryIntersect(ptix, ptiy, ptjx, ptjy, sVix, sViy, sVjx, sVjy) with
                    | ValueSome pt -> result <- Some (pt, i / 2, j / 2)
                    | ValueNone    -> ()
                    j <- j + 2
                i <- i + 2
            result

    /// Tries to find a self intersection in Polyline2D.
    /// This function is the same as tryFindSelfIntersection but with an initial broad phase using bounding rectangles to quickly exclude non-intersecting segments.
    /// It runs faster than tryFindSelfIntersection for polylines with more than 50 points, but only on .NET, not on Fable JS.
    /// A closed Polyline2D, is not reported as a self intersection.
    /// Also returns Some if two non-adjacent segments are just touching.
    /// If found returns the first intersection point and the indices of the two segments that intersect.
    /// If no intersection is found returns None.
    /// Returns None for Polylines with fewer than 3 points (less than two segments).
    /// This is an iterative O(n^2) algorithm
    /// This function is optimized for polylines with more than 50 points.
    static member tryFindSelfIntersectionWithBRect (pl:Polyline2D) : Option<Pt * int * int> =
        // see Test\BenchmarkPolyline2DSelfIntersection.fsx for performance tests and comparison of this algorithm with tryFindSelfIntersectionSmall.
        let xys = pl.XYs
        let len = xys.Count
        if len < 6 then
            None // a polyline with fewer than two segments cannot intersect itself
        else

            // brs is indexed per segment (0,1,2,..), while i and j below are flat (x,y)
            // coordinate indices into xys/segmentVs that step by 2, so brs is accessed as brs.[i/2].
            let brs = ResizeArray(len - 2) // one vector less than points
            let mutable px = xys.[0]
            let mutable py = xys.[1]
            let mutable i = 2
            while i < len do
                let tx = xys.[i]
                let ty = xys.[i + 1]
                brs.Add (BRect.createXY(px, py, tx, ty))
                px <- tx
                py <- ty
                i <- i + 2

            let segmentVs = pl.SegmentVectorsXY
            let segLastIdx = segmentVs.LastIndex
            let isClosed = pl.IsAlmostClosed 1e-7

            let mutable result = None

            // Segment 0 is handled first because its range depends on whether the Polyline2D is closed:
            // on a closed Polyline2D the first and last segment share the closure vertex and are therefore
            // adjacent, so on a closed Polyline2D segment 0 is not checked against the last segment.
            // Indices are flat (x,y) coordinate indices stepping by 2, so excluding the last segment
            // means dropping a full segment stride: segLastIdx is odd (the last segment's y-component),
            // segLastIdx-1 is the last segment's start and segLastIdx-2 is the previous segment's start.
            let firstRowEnd = if isClosed then segLastIdx - 2 else segLastIdx
            let mutable j = 4 // +2 to skip the adjacent segment
            let mutable bri = brs.[0]
            let mutable ptix = xys.[0]
            let mutable ptiy = xys.[1]
            let mutable sVix = segmentVs.[0]
            let mutable sViy = segmentVs.[1]
            while result.IsNone && j < firstRowEnd do
                if BRect.isOverlapping bri brs.[j / 2] then
                    let mutable ptjx = xys.[j]
                    let mutable ptjy = xys.[j + 1]
                    let mutable sVjx = segmentVs.[j]
                    let mutable sVjy = segmentVs.[j + 1]
                    match XLineXY.tryIntersect(ptix, ptiy, ptjx, ptjy, sVix, sViy, sVjx, sVjy) with
                    | ValueSome pt -> result <- Some (pt, 0, j / 2)
                    | ValueNone    -> ()
                j <- j + 2

            // The remaining segments from index 1 onward have no closure special case.
            let mutable i = 2
            while result.IsNone && i < segLastIdx do
                let mutable j = i + 4 // +2 to skip the adjacent segment
                bri <- brs.[i / 2]
                ptix <- xys.[i]
                ptiy <- xys.[i + 1]
                sVix <- segmentVs.[i]
                sViy <- segmentVs.[i + 1]
                while result.IsNone && j <= segLastIdx do
                    if BRect.isOverlapping bri brs.[j / 2] then
                        let mutable ptjx = xys.[j]
                        let mutable ptjy = xys.[j + 1]
                        let mutable sVjx = segmentVs.[j]
                        let mutable sVjy = segmentVs.[j + 1]
                        match XLineXY.tryIntersect(ptix, ptiy, ptjx, ptjy, sVix, sViy, sVjx, sVjy) with
                        | ValueSome pt -> result <- Some (pt, i / 2, j / 2)
                        | ValueNone    -> ()
                    j <- j + 2
                i <- i + 2
            result



    #if BENCHMARKS

    /// exists only for performance comparison with tryFindSelfIntersectionSmall, not used in production code.
    /// allocating 4 Vc or Pt objects for XLine2D.tryIntersect is 600% slower in js!

    static member tryFindSelfIntersectionWithPtObjects (pl:Polyline2D) : Option<Pt * int * int> =
        // see Test\BenchmarkPolyline2DSelfIntersection.js for performance tests and comparison of this algorithm with tryFindSelfIntersectionBig.
        let xys = pl.XYs
        let len = xys.Count
        if len < 6 then
            None // a polyline with fewer than two segments cannot intersect itself
        else
            let segmentVs = pl.SegmentVectorsXY
            let segLastIdx = segmentVs.LastIndex
            let isClosed = pl.IsAlmostClosed 1e-7
            let mutable result = None

            let firstRowEnd = if isClosed then segLastIdx - 3 else segLastIdx
            let mutable j = 4 // +2 to skip the adjacent segment
            let mutable ptix = xys.[0]
            let mutable ptiy = xys.[1]
            let mutable sVix = segmentVs.[0]
            let mutable sViy = segmentVs.[1]
            while result.IsNone && j <= firstRowEnd do
                let mutable ptjx = xys.[j]
                let mutable ptjy = xys.[j + 1]
                let mutable sVjx = segmentVs.[j]
                let mutable sVjy = segmentVs.[j + 1]
                let pa = Pt(ptix, ptiy)
                let va = Vc(sVix, sViy)
                let pb = Pt(ptjx, ptjy)
                let vb = Vc(sVjx, sVjy)
                match XLine2D.tryIntersect(pa, pb, va, vb) with
                | Some pt -> result <- Some (pt, 0, j / 2)
                | None    -> ()
                j <- j + 2

            // The remaining segments from index 1 onward have no closure special case.
            let mutable i = 2
            while result.IsNone && i <= segLastIdx do
                let mutable j = i + 4 // +2 to skip the adjacent segment
                ptix <- xys.[i]
                ptiy <- xys.[i + 1]
                sVix <- segmentVs.[i]
                sViy <- segmentVs.[i + 1]
                while result.IsNone && j <= segLastIdx do
                    let mutable ptjx = xys.[j]
                    let mutable ptjy = xys.[j + 1]
                    let mutable sVjx = segmentVs.[j]
                    let mutable sVjy = segmentVs.[j + 1]
                    let pa = Pt(ptix, ptiy)
                    let va = Vc(sVix, sViy)
                    let pb = Pt(ptjx, ptjy)
                    let vb = Vc(sVjx, sVjy)
                    match XLine2D.tryIntersect(pa, pb, va, vb) with
                    | Some pt -> result <- Some (pt, i / 2, j / 2)
                    | None    -> ()
                    j <- j + 2
                i <- i + 2
            result

    #endif


    // #endregion
    // #region Obsolete

    /// Obsolete misspelling, use Polyline2D.FindLabelPoint instead.
    [<Obsolete("Use Polyline2D.FindLabelPoint instead.")>]
    member pl.FindLablePoint (precision: float) : Pt * float =
        pl.FindLabelPoint precision

    /// Obsolete misspelling, use Polyline2D.findLabelPoint instead.
    [<Obsolete("Use Polyline2D.findLabelPoint instead.")>]
    static member findLablePoint (precision: float) (pl: Polyline2D) : Pt * float =
        pl.FindLabelPoint precision

    [<Obsolete("This is no longer the internal structure of Polyline2D, use .XYs instead. Or .AsPoints to get points.")>]
    member p.Points : ResizeArray<Pt> =
        p.AsPoints

    [<Obsolete("This was semantically unclear, what happens at point? Use GetSegment(i).UnitTangent instead")>]
    member pl.TangentAt(_t:float) : 'a =
        fail "Polyline2D.TangentAt is obsolete, use GetSegment(i).UnitTangent instead." |> unbox // unbox to make type checker happy

    [<Obsolete("Use polyline2D.CloseInPlace instead.")>]
    member p.CloseIfOpen(t) : unit =
        p.CloseInPlace(t)

    [<Obsolete("Renamed to Polyline2D.subPolyline")>]
    static member segment a b (pl:Polyline2D) :Polyline2D =
        Polyline2D.subPolyline a b pl

    [<Obsolete("Since the internal structure of Polyline2D has changed this is not anymore a direct creation but a copy to a flat array.")>]
    static member createDirectlyUnsafe (coordinates: ResizeArray<Pt>) : Polyline2D =
        Polyline2D.createFromPts(coordinates)

    [<Obsolete("Since the internal structure of Polyline2D has changed to a flat array using this for looping doesn't make sense any more.")>]
    member p.LastPointIndex : int =
        p.PointCount - 1

    [<Obsolete("Since the internal structure of Polyline2D has changed to a flat array using this for looping doesn't make sense any more.")>]
    member p.LastSegmentIndex : int =
        p.PointCount - 2

    [<Obsolete("Use Euclid.Polyline2D.createFromPts instead.")>]
    static member create (pts: seq<Pt>) : Polyline2D =
        Polyline2D.createFromPts pts

[<Obsolete("Use Euclid.Loop has been removed from Euclid in 0.20.0. use Polyline2D instead.",true)>]
type Loop private  () =

    [<Obsolete("Use Euclid.Loop has been removed from Euclid in 0.20.0. use Polyline2D instead.",true)>]
    static member create () : unit =
        failwithf "Euclid.Loop has been removed from Euclid in 0.20.0. use Polyline2D instead."

