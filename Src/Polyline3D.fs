namespace Euclid

open System
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open System.Collections.Generic
open EuclidErrors
open Euclid.EuclidCollectionUtilities


module private Polyline3DUtil =

    let inline countPts (xyzs: ResizeArray<float>) = ResizeArr.len xyzs / 3

    let failPointIndex methodName idx (xyzs: ResizeArray<float>) =
        fail $"Polyline3D.{methodName}: index {idx} is out of range for Polyline3D with {countPts xyzs} points."


    let inline getPt i (xyzs: ResizeArray<float>) = Pnt(xyzs.[i * 3], xyzs.[i * 3 + 1], xyzs.[i * 3 + 2])

    let inline setCoordXYZ i x y z (xyzs: ResizeArray<float>) =
        xyzs.[i * 3    ] <- x
        xyzs.[i * 3 + 1] <- y
        xyzs.[i * 3 + 2] <- z


    let inline appendXYZ x y z (xyzs: ResizeArray<float>) =
        xyzs.Add x
        xyzs.Add y
        xyzs.Add z

    let inline copy (xyzs: ResizeArray<float>) : ResizeArray<float> =
        xyzs.GetRange(0, xyzs.Count)


open Polyline3DUtil

/// A class holding a list of 3D points representing a mutable 3D Polyline.
/// If the last point is the same as the first point, the Polyline3D is closed.
/// The source-of-truth storage is an interleaved float buffer: x0, y0, z0, x1, y1, z1, ...
// [<Struct>]
[<NoEquality; NoComparison>] // because its made up from floats
[<DataContract>] // for using DataMember on fields
type Polyline3D private (xyzs: ResizeArray<float>) =

    /// Create a new empty Polyline3D
    new () = Polyline3D(ResizeArray<float>())

    /// Create a new empty Polyline3D with predefined capacity for the internal list of points.
    new (capacity:int) = Polyline3D(ResizeArray<float>(capacity * 3))

    /// Create a new Polyline3D by copying the provided sequence of points into a flat array.
    new (points: seq<Pnt>) =
        if isNull points then
            failNull "Polyline3D" "points"
        let xyzs = ResizeArray<float>()
        for pt in points do
            xyzs.Add pt.X
            xyzs.Add pt.Y
            xyzs.Add pt.Z
        Polyline3D(xyzs)

    /// Gets the X, Y and Z interleaved ResizeArray of the Polyline3D: x0, y0, z0, x1, y1, z1, ...
    /// This is the live internal buffer, so changes to the list will be reflected in the Polyline3D.
    [<DataMember>]
    member _.XYZs : ResizeArray<float> =
        xyzs

    /// Gets the X, Y and Z interleaved ResizeArray of the Polyline3D: x0, y0, z0, x1, y1, z1, ...
    /// This is the live internal buffer, so changes to the list will be reflected in the Polyline3D.
    static member inline getXYZs (p:Polyline3D) : ResizeArray<float> =
        p.XYZs

    /// Converts the float buffer of the Polyline3D into a list of Points. Use .XYZs to access the live internal buffer.
    member _.AsPoints : ResizeArray<Pnt> =
        let pts = ResizeArray<Pnt>(countPts xyzs)
        let len = ResizeArr.len xyzs
        let mutable i = 0
        while i < len do
            pts.Add(Pnt(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2]))
            i <- i + 3
        pts

    /// Converts the float buffer of the Polyline3D into a list of Points. Use .XYZs to access the live internal buffer.
    static member inline asPoints (p : Polyline3D) : ResizeArray<Pnt> =
        p.AsPoints


    // #endregion
    // #region Get /Set

    /// Gets the x coordinate of the point at the given position.
    /// (does xyzs.[position * 3] internally)
    member _.GetX (position:int) : float =
        #if DEBUG || CHECK_EUCLID
        let len = xyzs.Count
        if position < 0 || position > len / 3 - 1 then
            failPointIndex "GetX" position xyzs
        #endif
        xyzs.[position * 3]

    /// Gets the x coordinate of the point at the given index.
    /// (does xyzs.[position * 3] internally)
    static member inline getX (position:int) (p:Polyline3D) : float =
        p.GetX position

    /// Gets the y coordinate of the point at the given position.
    /// (does xyzs.[position * 3 + 1] internally)
    member _.GetY (position:int) : float =
        #if DEBUG || CHECK_EUCLID
        let len = xyzs.Count
        if position < 0 || position > len / 3 - 1 then
            failPointIndex "GetY" position xyzs
        #endif
        xyzs.[position * 3 + 1]

    /// Gets the y coordinate of the point at the given index.
    /// (does xyzs.[position * 3 + 1] internally)
    static member inline getY(position:int) (p:Polyline3D) : float =
        p.GetY position


    /// Gets the z coordinate of the point at the given position.
    /// (does xyzs.[position * 3 + 2] internally)
    member _.GetZ (position:int) : float =
        #if DEBUG || CHECK_EUCLID
        let len = xyzs.Count
        if position < 0 || position > len / 3 - 1 then
            failPointIndex "GetZ" position xyzs
        #endif
        xyzs.[position * 3 + 2]

    /// Gets the z coordinate of the point at the given index.
    /// (does xyzs.[position * 3 + 2] internally)
    static member inline getZ (position:int) (p:Polyline3D) : float =
        p.GetZ position


    /// Gets the point at the given position.
    /// (does Pnt(xyzs.[position * 3], xyzs.[position * 3 + 1], xyzs.[position * 3 + 2]) internally)
    member p.GetPt (position:int) : Pnt =
        #if DEBUG || CHECK_EUCLID
        let len = xyzs.Count
        if position < 0 || position > len / 3 - 1 then
            failPointIndex "GetPt" position xyzs
        #endif
        getPt position xyzs

    /// Gets the point at the given position.
    /// (does Pnt(xyzs.[position * 3], xyzs.[position * 3 + 1], xyzs.[position * 3 + 2]) internally)
    static member inline getPt (position:int) (p:Polyline3D) : Pnt =
        p.GetPt position


    /// Sets the point at given position to the given point.
    /// ( sets xyzs.[position * 3] and xyzs.[position * 3 + 1] and xyzs.[position * 3 + 2] internally)
    member p.SetPnt (position:int,pt:Pnt) : unit =
        #if DEBUG || CHECK_EUCLID
        let len = xyzs.Count
        if position < 0 || position > len / 3 - 1 then
            failPointIndex "SetPnt" position xyzs
        #endif
        p.SetPointXYZ (position, pt.X, pt.Y, pt.Z)

    /// Sets the point at given position to the given point.
    /// ( sets xyzs.[position * 3] and xyzs.[position * 3 + 1] and xyzs.[position * 3 + 2] internally)
    static member inline setPnt (position:int) (pt:Pnt) (p:Polyline3D) : unit =
        p.SetPnt (position, pt)

    /// Sets the x, y, and z coordinates of the point at the given position.
    /// On a closed Polyline3D, setting the first or last point will set both to the same point.
    /// Raises an error if the position is out of range.
    /// (sets xyzs.[position * 3] and xyzs.[position * 3 + 1] and xyzs.[position * 3 + 2]  internally)
    member p.SetPointXYZClosed (position:int, x:float, y:float, z:float): unit =
        #if DEBUG || CHECK_EUCLID
        let len = xyzs.Count
        if position < 0 || position > len / 3 - 1 then
            failPointIndex "SetPointXYZClosed" position xyzs
        #endif
        let wasClosed = p.IsClosed
        if wasClosed && position = 0 then
            setCoordXYZ (p.PointCount-1) x y z xyzs
        elif wasClosed && position = p.PointCount-1 then
            setCoordXYZ 0 x y z xyzs
        setCoordXYZ position x y z xyzs

    /// Sets the x, y, and z coordinates of the point at the given index.
    /// On a closed Polyline3D, setting the first or last point will set both to the same point.
    /// Raises an error if the index is out of range.
    /// (sets xyzs.[position * 3], xyzs.[position * 3 + 1], and xyzs.[position * 3 + 2] internally)
    static member inline setPointXYZClosed x y z (position:int) (p:Polyline3D) : unit =
        p.SetPointXYZClosed (position, x, y, z)

    /// Sets the x, y, and z coordinates of the point at the given index.
    /// NOTE: setting the first or last point on a closed Polyline3D might open it.
    /// (sets xyzs.[position * 3], xyzs.[position * 3 + 1], and xyzs.[position * 3 + 2] internally )
    member p.SetPointXYZ (position:int, x:float, y:float, z:float) : unit =
        #if DEBUG || CHECK_EUCLID
        let len = xyzs.Count
        if position < 0 || position > len / 3 - 1 then
            failPointIndex "SetPointXYZ" position xyzs
        #endif
        setCoordXYZ position x y z xyzs

    /// Sets the x, y, and z coordinates of the point at the given index.
    /// NOTE: setting the first or last point on a closed Polyline3D might open it.
    /// (sets xyzs.[position * 3], xyzs.[position * 3 + 1], and xyzs.[position * 3 + 2] internally )
    static member inline setPointXYZ x y z (position:int) (p:Polyline3D) : unit =
        p.SetPointXYZ (position, x, y, z)

    /// Adds a point from x, y, and z coordinates.
    member _.AddXYZ (x:float, y:float, z:float) : unit =
        appendXYZ x y z xyzs

    /// Adds a point from x, y, and z coordinates.
    static member inline addXYZ (x:float) (y:float) (z:float) (p:Polyline3D) : unit =
        p.AddXYZ (x, y, z)

    /// Adds a point to the end of the Polyline3D.
    member p.AddPoint (pt:Pnt) : unit =
        p.AddXYZ (pt.X, pt.Y, pt.Z)

    /// Adds a point to the end of the Polyline3D.
    static member inline addPoint (pt:Pnt) (p:Polyline3D) : unit =
        p.AddPoint pt

    // #endregion
    // #region ToString


    /// Nicely formatted string representation of the Polyline3D including its length.
    override p.ToString() : string =
        let pc = p.PointCount
        if pc = 0 then
            "empty Euclid.Polyline3D."
        elif p.IsClosed then
            $"closed Euclid.Polyline3D with length {p.Length}, from {pc} points"
        else
            $"open Euclid.Polyline3D with length {p.Length}, from {pc} points"

    /// Format Polyline3D into string including its length.
    member p.AsString : string =
        let pc = p.PointCount
        if pc = 0 then
            "empty Polyline3D."
        elif p.IsClosed then
            $"closed Polyline3D with length {p.Length}, from {pc} points"
        else
            $"open Polyline3D with length {p.Length}, from {pc} points"

    /// Format Polyline3D into string including its length.
    static member inline asString (pl:Polyline3D) : string =
        pl.AsString

    /// Format this 3D polyline into an F# code string that can be used to recreate the polyline.
    member p.AsFSharpCode : string =
        let ptsAsCode =
            p.AsPoints
            |> ResizeArr.map _.AsFSharpCode
            |> String.concat "; "
        $"Polyline3D.create [| {ptsAsCode} |]"

    /// Format this 3D polyline into an F# code string that can be used to recreate the polyline.
    static member inline asFSharpCode (pl:Polyline3D) : string =
        pl.AsFSharpCode




    /// Creates a copy of the Polyline3D
    /// Same as polyline.Clone()
    member p.Duplicate(): Polyline3D =
        Polyline3D(copy xyzs)

    /// Creates a copy of the Polyline3D
    /// Same as polyline.Clone()
    static member inline duplicate (pl:Polyline3D) : Polyline3D =
        pl.Duplicate()

    /// Creates a copy of the Polyline3D.
    /// Same as polyline.Duplicate()
    member p.Clone(): Polyline3D =
        Polyline3D(copy xyzs)

    /// Creates a copy of the Polyline3D.
    /// Same as polyline.Duplicate()
    static member inline clone (pl:Polyline3D) : Polyline3D =
        pl.Clone()

    /// Gets or sets first point of the Polyline3D
    /// This is the point at index 0.
    /// Same as Polyline3D.FirstPoint
    member p.Start
        with get() : Pnt =
            if p.PointCount < 1 then failTooFewPoly3D "Start.get" 1 p.PointCount
            getPt 0 xyzs
        and set(pt:Pnt) : unit =
            if p.PointCount < 1 then failTooFewPoly3D "Start.set" 1 p.PointCount
            xyzs.[0] <- pt.X
            xyzs.[1] <- pt.Y
            xyzs.[2] <- pt.Z

    /// Gets first point of the Polyline3D
    static member inline start (pl:Polyline3D) : Pnt =
        pl.Start

    /// Gets or sets last or end point of the Polyline3D
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.LastPoint
    member p.End
        with get() : Pnt =
            if p.PointCount < 1 then failTooFewPoly3D "End.get" 1 p.PointCount
            let c = xyzs.Count
            Pnt(xyzs.[c - 3], xyzs.[c - 2], xyzs.[c - 1])
        and set(pt:Pnt) : unit =
            if p.PointCount < 1 then failTooFewPoly3D "End.set" 1 p.PointCount
            let c = xyzs.Count
            xyzs.[c - 3] <- pt.X
            xyzs.[c - 2] <- pt.Y
            xyzs.[c - 1] <- pt.Z

    /// Gets the last or end point of the Polyline3D
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.LastPoint
    static member inline end' (pl:Polyline3D) : Pnt =
        pl.End

    /// Gets or sets the last point of the Polyline3D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.End
    member p.LastPoint
        with get() : Pnt =
            if p.PointCount < 1 then failTooFewPoly3D "LastPoint.get" 1 p.PointCount
            let c = xyzs.Count
            Pnt(xyzs.[c - 3], xyzs.[c - 2], xyzs.[c - 1])
        and set(pt:Pnt) : unit =
            if p.PointCount < 1 then failTooFewPoly3D "LastPoint.set" 1 p.PointCount
            let c = xyzs.Count
            xyzs.[c - 3] <- pt.X
            xyzs.[c - 2] <- pt.Y
            xyzs.[c - 1] <- pt.Z

    /// Gets the last point of the Polyline3D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.End
    static member inline lastPoint (pl:Polyline3D) : Pnt =
        pl.LastPoint

    /// Gets or sets the second last point of the Polyline3D.
    member p.SecondLastPoint
        with get() : Pnt =
            if p.PointCount < 2 then failTooFewPoly3D "SecondLastPoint.get" 2 p.PointCount
            let c = xyzs.Count
            Pnt(xyzs.[c - 6], xyzs.[c - 5], xyzs.[c - 4])
        and set(pt:Pnt) : unit =
            if p.PointCount < 2 then failTooFewPoly3D "SecondLastPoint.set" 2 p.PointCount
            let c = xyzs.Count
            xyzs.[c - 6] <- pt.X
            xyzs.[c - 5] <- pt.Y
            xyzs.[c - 4] <- pt.Z

    /// Gets the second last point of the Polyline3D.
    static member inline secondLastPoint (pl:Polyline3D) : Pnt =
        pl.SecondLastPoint

    /// Gets or sets the second point of the Polyline3D.
    /// This is the point at index 1.
    member p.SecondPoint
        with get() : Pnt =
            if p.PointCount < 2 then failTooFewPoly3D "SecondPoint.get" 2 p.PointCount
            Pnt(xyzs.[3], xyzs.[4], xyzs.[5])
        and set(pt:Pnt) : unit =
            if p.PointCount < 2 then failTooFewPoly3D "SecondPoint.set" 2 p.PointCount
            xyzs.[3] <- pt.X
            xyzs.[4] <- pt.Y
            xyzs.[5] <- pt.Z

    /// Gets the second point of the Polyline3D.
    /// This is the point at index 1.
    static member inline secondPoint (pl:Polyline3D) : Pnt =
        pl.SecondPoint

    /// Gets or sets the first point of the Polyline3D.
    /// This is the point at index 0.
    /// Same as Polyline3D.Start
    member p.FirstPoint
        with get() : Pnt =
            if p.PointCount < 1 then failTooFewPoly3D "FirstPoint.get" 1 p.PointCount
            Pnt(xyzs.[0], xyzs.[1], xyzs.[2])
        and set(pt:Pnt) : unit =
            if p.PointCount < 1 then failTooFewPoly3D "FirstPoint.set" 1 p.PointCount
            xyzs.[0] <- pt.X
            xyzs.[1] <- pt.Y
            xyzs.[2] <- pt.Z

    /// Gets the first point of the Polyline3D.
    /// This is the point at index 0.
    /// Same as Polyline3D.Start
    static member inline firstPoint (pl:Polyline3D) : Pnt =
        pl.FirstPoint

    /// Gets the count of points in the Polyline3D
    member p.PointCount : int =
        countPts xyzs

    /// Gets the number of points in the Polyline3D.
    static member inline pointCount (p:Polyline3D) : int =
        p.PointCount

    /// Gets the count of segments in the Polyline3D
    /// This is poly.Points.Count - 1
    member p.SegmentCount : int =
        max 0 (p.PointCount - 1 )

    /// Gets the number of segments in the Polyline3D.
    static member inline segmentCount (p:Polyline3D) : int =
        p.SegmentCount

    /// Gets the length of the Polyline3D
    /// Returns 0.0 if there are less than 2 points.
    member p.Length : float =
        let mutable l = 0.0
        let len = xyzs.Count
        if len >= 6 then
            let mutable px = xyzs.[0]
            let mutable py = xyzs.[1]
            let mutable pz = xyzs.[2]
            let mutable i = 3
            while i < len do
                let x = xyzs.[i]
                let y = xyzs.[i + 1]
                let z = xyzs.[i + 2]
                let dx = x - px
                let dy = y - py
                let dz = z - pz
                l <- l + sqrt (dx * dx + dy * dy + dz * dz)
                px <- x
                py <- y
                pz <- z
                i <- i + 3
        l

    /// Gets the length of the Polyline3D.
    /// The sum of the lengths of all segments.
    static member inline length (p:Polyline3D) : float =
        p.Length

    /// Gets the segment at index i of the Polyline3D.
    member p.GetSegment(i:int) : Line3D =
        if i < 0 || i > p.PointCount - 2 then
            fail $"Polyline3D.GetSegment: index {i} is out of range for Polyline3D with {p.PointCount} points."
        Line3D(getPt i xyzs, getPt (i+1) xyzs)

    /// Gets the segment at index i of the Polyline3D.
    static member inline getSegment (i:int) (pl:Polyline3D) : Line3D =
        pl.GetSegment i

    /// Gets the last segment of the Polyline3D.
    member p.LastSegment : Line3D =
        if p.PointCount < 2 then failTooFewPoly3D "LastSegment" 2 p.PointCount
        let i = p.PointCount - 1
        Line3D(getPt (i-1) xyzs, getPt i xyzs)

    /// Gets the last segment of the Polyline3D.
    static member inline lastSegment (pl:Polyline3D) : Line3D =
        pl.LastSegment

    /// Gets the first segment of the Polyline3D.
    member p.FirstSegment : Line3D =
        if p.PointCount < 2 then failTooFewPoly3D "FirstSegment" 2 p.PointCount
        Line3D(getPt 0 xyzs, getPt 1 xyzs)

    /// Gets the first segment of the Polyline3D.
    static member inline firstSegment (pl:Polyline3D) : Line3D =
        pl.FirstSegment

    /// Returns all segments of the Polyline3D as a list of Line3D.
    member p.Segments : ResizeArray<Line3D> =
        let lns = ResizeArray(p.SegmentCount)
        if p.PointCount < 2 then
            lns
        else
            let mutable a = getPt 0 xyzs
            for i = 1 to p.PointCount - 1 do
                let b = getPt i xyzs
                lns.Add(Line3D(a, b))
                a <- b
            lns

    /// Returns all segments of the Polyline3D as a list of Line3D.
    static member inline segments (pl:Polyline3D) : ResizeArray<Line3D> =
        pl.Segments

    /// Returns the line vectors of all segments of the Polyline3D as a list of Vec.
    /// The length of the list is one less than the point count.
    member p.SegmentVectors : ResizeArray<Vec> =
        let vs = ResizeArray(p.SegmentCount)
        let len = xyzs.Count
        if len < 6 then
            vs
        else
            let mutable ax = xyzs.[0]
            let mutable ay = xyzs.[1]
            let mutable az = xyzs.[2]
            let mutable i = 3
            while i < len do
                let bx = xyzs.[i]
                let by = xyzs.[i + 1]
                let bz = xyzs.[i + 2]
                vs.Add(Vec(bx - ax, by - ay, bz - az))
                ax <- bx
                ay <- by
                az <- bz
                i <- i + 3
            vs

    /// Returns the line vectors of all segments of the Polyline3D as a list of Vec.
    /// The length of the list is one less than the point count.
    static member inline segmentVectors (pl:Polyline3D) : ResizeArray<Vec> =
        pl.SegmentVectors


    /// Returns the line vectors of all segments of the Polyline3D as a flat list of x, y, and z components.
    /// The length of the list is 3 less than the xyzs count, so one less vector than points in the polyline.
    member p.SegmentVectorsXYZ : ResizeArray<float> =
        let xyzs = p.XYZs
        let len = xyzs.Count
        let vs = ResizeArray(max 0 (len - 3)) // max 0 to avoid negative capacity on empty polyline
        if len < 6 then
            vs
        else
            let mutable ax = xyzs.[0]
            let mutable ay = xyzs.[1]
            let mutable az = xyzs.[2]
            let mutable i = 3
            while i < len do
                let bx = xyzs.[i]
                let by = xyzs.[i + 1]
                let bz = xyzs.[i + 2]
                vs.Add (bx - ax)
                vs.Add (by - ay)
                vs.Add (bz - az)
                ax <- bx
                ay <- by
                az <- bz
                i <- i + 3
            vs

    /// Returns the line vectors of all segments of the Polyline3D as a flat list of x, y, and z components.
    /// The length of the list is 3 less than the xyzs count, so one less vector than points in the polyline.
    static member inline segmentVectorsXYZ (p:Polyline3D) : ResizeArray<float> =
        p.SegmentVectorsXYZ

    /// Gets the bounding box of the Polyline3D.
    member p.BoundingBox : BBox =
        if p.PointCount = 0 then failEmptySeq "Polyline3D.BoundingBox" "Polyline3D"
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable minZ = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        let mutable maxZ = Double.MinValue
        let mutable i = 0
        let len = xyzs.Count
        while i < len do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            minX <- min minX x
            minY <- min minY y
            minZ <- min minZ z
            maxX <- max maxX x
            maxY <- max maxY y
            maxZ <- max maxZ z
            i <- i + 3
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)

    /// Gets the bounding box of the Polyline3D.
    static member inline boundingBox (pl:Polyline3D) : BBox =
        pl.BoundingBox

    /// Tests if Polyline3D start and end points are exactly the same.
    /// Returns False if the Polyline3D has less than 3 points.
    member p.IsClosed : bool =
        p.PointCount > 2
        && (let c = xyzs.Count
            xyzs.[0] = xyzs.[c - 3]
            && xyzs.[1] = xyzs.[c - 2]
            && xyzs.[2] = xyzs.[c - 1])

    /// Tests if Polyline3D start and end points are exactly the same.
    /// Returns False if the Polyline3D has less than 3 points.
    static member inline isClosed (pl:Polyline3D) : bool =
        pl.IsClosed

    /// Tests if Polyline3D is closed within given tolerance.
    /// Returns False if the Polyline3D has less than 3 points.
    member p.IsAlmostClosed tolerance : bool =
        if p.PointCount <= 2 then
            false
        else
            let c = xyzs.Count
            let dx = xyzs.[0] - xyzs.[c - 3]
            let dy = xyzs.[1] - xyzs.[c - 2]
            let dz = xyzs.[2] - xyzs.[c - 1]
            dx * dx + dy * dy + dz * dz <= tolerance*tolerance // <= needed so it works with 0.0

    /// Tests if Polyline3D is closed within given tolerance.
    /// Returns False if the Polyline3D has less than 3 points.
    static member inline isAlmostClosed tolerance (pl:Polyline3D) : bool =
        pl.IsAlmostClosed tolerance

    /// Returns new Polyline3D in reversed Order.
    member p.Reverse () : Polyline3D =
        let rev = ResizeArray<float>(xyzs.Count)
        let mutable i = xyzs.Count - 3
        while i >= 0 do
            rev.Add xyzs.[i]
            rev.Add xyzs.[i + 1]
            rev.Add xyzs.[i + 2]
            i <- i - 3
        Polyline3D rev

    /// Returns new Polyline3D in reversed Order.
    static member reverse (p:Polyline3D) : Polyline3D =
        p.Reverse()

    /// Reverse order of the Polyline3D in place.
    member p.ReverseInPlace() : unit =
        let mutable left = 0
        let mutable right = xyzs.Count - 3
        while left < right do
            let lx = xyzs.[left]
            let ly = xyzs.[left + 1]
            let lz = xyzs.[left + 2]
            xyzs.[left]     <- xyzs.[right]
            xyzs.[left + 1] <- xyzs.[right + 1]
            xyzs.[left + 2] <- xyzs.[right + 2]
            xyzs.[right]     <- lx
            xyzs.[right + 1] <- ly
            xyzs.[right + 2] <- lz
            left <- left + 3
            right <- right - 3

    /// <summary>Reverse order of the Polyline3D in place.</summary>
    /// <param name="p">The Polyline3D to reverse.</param>
    /// <returns>A reference to the same Polyline3D as the input</returns>
    static member reverseInPlace (p:Polyline3D) : Polyline3D =
        p.ReverseInPlace()
        p

    /// Close the Polyline3D if it is not already closed.
    /// If the ends are closer than the tolerance, the last point is set equal to the first point.
    /// Otherwise the start point is added to the end of the Polyline3D.
    /// The default tolerance is 1e-6
    member p.CloseInPlace([<OPT; DEF(1e-6)>]toleranceForAddingPoint:float) : unit =
        if p.PointCount < 3 then failTooFewPoly3D "CloseInPlace" 3 p.PointCount
        let c = xyzs.Count
        let sx = xyzs.[0]
        let sy = xyzs.[1]
        let sz = xyzs.[2]
        let ex = xyzs.[c - 3]
        let ey = xyzs.[c - 2]
        let ez = xyzs.[c - 1]
        let dx = sx - ex
        let dy = sy - ey
        let dz = sz - ez
        if dx * dx + dy * dy + dz * dz <= toleranceForAddingPoint*toleranceForAddingPoint then // <= needed so it works with 0.0
            xyzs.[c - 3] <- sx
            xyzs.[c - 2] <- sy
            xyzs.[c - 1] <- sz
        else
            xyzs.Add sx
            xyzs.Add sy
            xyzs.Add sz

    /// <summary>Closes the Polyline3D in place by adding a point.
    /// If the first and last point are within the given tolerance of each other,
    /// the last point is set equal to the first point instead.</summary>
    /// <param name="toleranceForAddingPoint">The tolerance used to decide whether to snap the last point to the first point.</param>
    /// <param name="pl">The Polyline3D to close.</param>
    /// <returns>A reference to the same Polyline3D as the input</returns>
    static member closeInPlace (toleranceForAddingPoint:float)  (pl:Polyline3D) : Polyline3D =
        pl.CloseInPlace toleranceForAddingPoint
        pl

    /// Calculates the signed area of the Polyline3D when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is CCW.
    /// Raises an error on an empty Polyline3D.
    member p.SignedAreaIn2D : float =
        //https://helloacm.com/sign-area-of-irregular-polygon/
        if p.PointCount = 0 then failTooFewPoly3D "SignedAreaIn2D" 1 p.PointCount
        let mutable area = 0.0
        let c = xyzs.Count
        let mutable tx = xyzs.[c - 3] // calculate from last to first too
        let mutable ty = xyzs.[c - 2]
        let mutable i = 0
        while i < c do
            let nx = xyzs.[i]
            let ny = xyzs.[i + 1]
            let a = tx - nx
            let b = ny + ty
            area <- area + a*b
            tx <- nx
            ty <- ny
            i <- i + 3
        area * 0.5

    /// Calculates the signed area of the Polyline3D when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is CCW.
    static member inline signedAreaIn2D (pl:Polyline3D) : float =
        pl.SignedAreaIn2D

    /// Calculates the area of the Polyline3D when projected in 2D.
    /// Z values are ignored.
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self intersecting Polylines the result is invalid.
    member p.AreaIn2D : float =
        abs p.SignedAreaIn2D

    /// Calculates the area of the Polyline3D when projected in 2D.
    /// Z values are ignored.
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self intersecting Polylines the result is invalid.
    static member inline areaIn2D (pl:Polyline3D) : float =
        abs pl.SignedAreaIn2D

    /// Test if Polyline3D is CounterClockwise when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is CCW.
    member p.IsCounterClockwiseIn2D : bool =
        let  area = p.SignedAreaIn2D
        if   abs(area) < UtilEuclid.zeroLengthTolerance then
            fail $"Polyline3D.IsCounterClockwiseIn2D: Polyline3D the area is zero: {p}"
        area > 0.0

    /// Test if Polyline3D is CounterClockwise when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is CCW.
    static member inline isCounterClockwiseIn2D (pl:Polyline3D) : bool =
        pl.IsCounterClockwiseIn2D

    /// Test if Polyline3D is Clockwise when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is negative the Polyline3D is Clockwise.
    member p.IsClockwiseIn2D : bool =
        let area = p.SignedAreaIn2D
        if   abs(area) < UtilEuclid.zeroLengthTolerance then
            fail $"Polyline3D.IsClockwiseIn2D: Polyline3D the area is zero: {p}"
        area < 0.0

    /// Test if Polyline3D is Clockwise when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is negative the Polyline3D is Clockwise.
    static member inline isClockwiseIn2D (pl:Polyline3D) : bool =
        pl.IsClockwiseIn2D

    /// Returns the point at a given parameter on the Polyline3D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    /// If the parameter is within 1e-6 of an integer value, the integer value is used as parameter.
    member pl.EvaluateAt(t:float) : Pnt =
        let i = int t       // integer part of the parameter
        let p = t - float i // fractional part of the parameter
        let count = pl.PointCount
        let lastParam = float (count - 1)

        // values next to  the start of the polyline:
        if t < 1e-6 then
            if t < -1e-6 then
                fail $"Polyline3D.EvaluateAt: Parameter {t} is less than 0.0"
            pl.Start

        // values next to  the end of the polyline:
        elif t > (lastParam - 1e-6) then
            if t > (lastParam + 1e-6) then
                fail $"Polyline3D.EvaluateAt: Parameter {t} is more than last point index {lastParam}."
            pl.End

        // return point if point is almost matching and integer
        elif p < 1e-6 then
            getPt i xyzs
        elif p > 1.0 - 1e-6 then
            getPt (i+1) xyzs
        else
            let xyzIdx = i * 3
            let x = xyzs.[xyzIdx]
            let y = xyzs.[xyzIdx + 1]
            let z = xyzs.[xyzIdx + 2]
            Pnt(x + (xyzs.[xyzIdx + 3] - x) * p,
                y + (xyzs.[xyzIdx + 4] - y) * p,
                z + (xyzs.[xyzIdx + 5] - z) * p)

    /// Returns the point at a given parameter on the Polyline3D.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    static member evaluateAt (t:float) (pl:Polyline3D) : Pnt =
        pl.EvaluateAt t

    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.ClosestParameter(p:Pnt) : float =
        if pl.PointCount = 0 then  fail "Polyline3D.ClosestParameter failed on empty Polyline3D"
        let px = p.X
        let py = p.Y
        let pz = p.Z
        let mutable ax = xyzs.[0]
        let mutable ay = xyzs.[1]
        let mutable az = xyzs.[2]
        let mutable minT = 0.0
        let mutable seg = 0
        let mutable minDistSq =
            let dx = px - ax
            let dy = py - ay
            let dz = pz - az
            dx * dx + dy * dy + dz * dz // this handles the case of a single point Polyline3D
        let mutable i = 3
        let mutable segmentIndex = 0
        let len = xyzs.Count
        while i < len do
            let bx = xyzs.[i]
            let by = xyzs.[i + 1]
            let bz = xyzs.[i + 2]
            let dx = bx - ax
            let dy = by - ay
            let dz = bz - az
            if dx <> 0.0 || dy <> 0.0 || dz <> 0.0 then
                let t = ((px - ax) * dx + (py - ay) * dy + (pz - az) * dz) / (dx * dx + dy * dy + dz * dz)
                let t' = max 0.0 (min 1.0 t)
                let projX = ax + dx * t'
                let projY = ay + dy * t'
                let projZ = az + dz * t'
                let dpx = px - projX
                let dpy = py - projY
                let dpz = pz - projZ
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minT <- t'
                    seg <- segmentIndex
            else
                let dpx = px - ax
                let dpy = py - ay
                let dpz = pz - az
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minT <- 0.0
                    seg <- segmentIndex
            ax <- bx
            ay <- by
            az <- bz
            segmentIndex <- segmentIndex + 1
            i <- i + 3
        float seg + minT

    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    static member inline closestParameter (pl:Polyline3D) (pt:Pnt) : float =
        pl.ClosestParameter pt

    /// Returns the point on the Polyline3D that is the closest point to the given point.
    member pl.ClosestPoint(p:Pnt) : Pnt =
        if pl.PointCount = 0 then  fail "Polyline3D.ClosestPoint failed on empty Polyline3D"
        let px = p.X
        let py = p.Y
        let pz = p.Z
        let mutable ax = xyzs.[0]
        let mutable ay = xyzs.[1]
        let mutable az = xyzs.[2]
        let mutable minPt = Pnt(ax, ay, az) // this handles the case of a single point Polyline3D
        let mutable minDistSq =
            let dx = px - ax
            let dy = py - ay
            let dz = pz - az
            dx * dx + dy * dy + dz * dz // this handles the case of a single point Polyline3D
        let mutable i = 3
        let len = xyzs.Count
        while i < len do
            let bx = xyzs.[i]
            let by = xyzs.[i + 1]
            let bz = xyzs.[i + 2]
            let dx = bx - ax
            let dy = by - ay
            let dz = bz - az
            if dx <> 0.0 || dy <> 0.0 || dz <> 0.0 then // zero distance between points
                let t = ((px - ax) * dx + (py - ay) * dy + (pz - az) * dz) / (dx * dx + dy * dy + dz * dz)
                let t' = max 0.0 (min 1.0 t)
                let projX = ax + dx * t'
                let projY = ay + dy * t'
                let projZ = az + dz * t'
                let dpx = px - projX
                let dpy = py - projY
                let dpz = pz - projZ
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minPt <- Pnt(projX, projY, projZ)
            else
                let dpx = px - ax
                let dpy = py - ay
                let dpz = pz - az
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minPt <- Pnt(ax, ay, az)
            ax <- bx
            ay <- by
            az <- bz
            i <- i + 3
        minPt

    /// Returns the point on the Polyline3D that is the closest point to the given point.
    static member inline closestPoint (pl:Polyline3D) (pt:Pnt) : Pnt =
        pl.ClosestPoint pt

    /// Returns the index into the Polylines point list of the point that is closest to the given point.
    member pl.ClosestPointIndex(p:Pnt) : int =
        if pl.PointCount = 0 then  fail "Polyline3D.ClosestPointIndex failed on empty Polyline3D"
        let px = p.X
        let py = p.Y
        let pz = p.Z
        let mutable minIdx = 0
        let mutable minDistSq =
            let dx = px - xyzs.[0]
            let dy = py - xyzs.[1]
            let dz = pz - xyzs.[2]
            dx * dx + dy * dy + dz * dz
        let mutable i = 3
        let mutable idx = 1
        let len = xyzs.Count
        while i < len do
            let dx = px - xyzs.[i]
            let dy = py - xyzs.[i + 1]
            let dz = pz - xyzs.[i + 2]
            let dSq = dx * dx + dy * dy + dz * dz
            if dSq < minDistSq then
                minDistSq <- dSq
                minIdx <- idx
            idx <- idx + 1
            i <- i + 3
        minIdx

    /// Returns the index into the Polyline3D's point list of the point that is closest to the given point.
    static member inline closestPointIndex (pl:Polyline3D) (pt:Pnt) : int =
        pl.ClosestPointIndex pt

    /// Returns the distance of the test point to the closest point on the Polyline3D.
    member pl.DistanceTo(p:Pnt) : float =
        if pl.PointCount = 0 then  fail "Polyline3D.DistanceTo failed on empty Polyline3D"
        let px = p.X
        let py = p.Y
        let pz = p.Z
        let mutable ax = xyzs.[0]
        let mutable ay = xyzs.[1]
        let mutable az = xyzs.[2]
        let mutable minDistSq =
            let dx = px - ax
            let dy = py - ay
            let dz = pz - az
            dx * dx + dy * dy + dz * dz // this handles the case of a single point Polyline3D
        let mutable i = 3
        let len = xyzs.Count
        while i < len do
            let bx = xyzs.[i]
            let by = xyzs.[i + 1]
            let bz = xyzs.[i + 2]
            let dx = bx - ax
            let dy = by - ay
            let dz = bz - az
            if dx <> 0.0 || dy <> 0.0 || dz <> 0.0 then // zero distance between points
                let t = ((px - ax) * dx + (py - ay) * dy + (pz - az) * dz) / (dx * dx + dy * dy + dz * dz)
                let t' = max 0.0 (min 1.0 t)
                let projX = ax + dx * t'
                let projY = ay + dy * t'
                let projZ = az + dz * t'
                let dpx = px - projX
                let dpy = py - projY
                let dpz = pz - projZ
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
            else
                let dpx = px - ax
                let dpy = py - ay
                let dpz = pz - az
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
            ax <- bx
            ay <- by
            az <- bz
            i <- i + 3
        sqrt minDistSq

    /// Returns the distance of the test point to the closest point on the Polyline3D.
    static member inline distanceTo (pl:Polyline3D) (pt:Pnt) : float =
        pl.DistanceTo pt

    /// Returns the average center of all points of the Polyline3D.
    member pl.Center : Pnt =
        if pl.PointCount = 0 then failTooFewPoly3D "Center" 1 pl.PointCount
        let mutable x = 0.0
        let mutable y = 0.0
        let mutable z = 0.0
        let mutable i = 0
        let len = xyzs.Count
        while i < len do
            x <- x + xyzs.[i]
            y <- y + xyzs.[i + 1]
            z <- z + xyzs.[i + 2]
            i <- i + 3
        Pnt(x / float pl.PointCount, y / float pl.PointCount, z / float pl.PointCount)

    /// Returns the average center of all points of the Polyline3D.
    static member inline center (pl:Polyline3D) : Pnt =
        pl.Center

    /// Returns the average normal vector of the Polyline3D.
    /// It is calculated by summing up the cross products of all segments around the center point.
    /// Does not check for bad input, may be zero length if points are colinear.
    member pl.AverageNormal : Vec =
        let c = pl.Center // fails on empty Polyline3D
        let mutable normal = Vec.Zero
        let n = pl.PointCount
        let mutable a = (getPt (n-1) xyzs) - c
        for i = 0 to n-1 do
            let b = (getPt i xyzs) - c
            normal <- normal + Vec.cross(a, b)
            a <- b
        normal

    /// Returns the average normal vector of the Polyline3D.
    /// It is calculated by summing up the cross products of all segments around the center point.
    /// Does not check for bad input, may be zero length if points are colinear.
    static member inline averageNormal (pl:Polyline3D) : Vec =
        pl.AverageNormal

    /// Scales the 3D polyline by a given factor.
    /// Scale center is World Origin 0,0,0
    member p.Scale (factor:float) : Polyline3D =
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            cs.Add(xyzs.[i]     * factor)
            cs.Add(xyzs.[i + 1] * factor)
            cs.Add(xyzs.[i + 2] * factor)
            i <- i + 3
        Polyline3D cs

    /// Scales the Polyline3D by a given factor.
    /// Scale center is World Origin 0,0,0
    /// Returns a new Polyline3D.
    static member scale (factor:float) (pl:Polyline3D) : Polyline3D =
        pl.Scale factor

    /// Scales the 3D polyline by a given factor on a given center point
    member p.ScaleOn (cen:Pnt) (factor:float) : Polyline3D =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        let mutable i = 0
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        while i < len do
            cs.Add(cx + (xyzs.[i    ] - cx) * factor)
            cs.Add(cy + (xyzs.[i + 1] - cy) * factor)
            cs.Add(cz + (xyzs.[i + 2] - cz) * factor)
            i <- i + 3
        Polyline3D cs

    /// Scales the 3D polyline by a given factor on a given center point
    static member inline scaleOn (cen:Pnt) (factor:float) (pl:Polyline3D) : Polyline3D =
        pl.ScaleOn cen factor

    /// Returns a Polyline3D moved by a vector.
    member p.Move (v:Vec) : Polyline3D =
        let vx = v.X
        let vy = v.Y
        let vz = v.Z
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            cs.Add(xyzs.[i]     + vx)
            cs.Add(xyzs.[i + 1] + vy)
            cs.Add(xyzs.[i + 2] + vz)
            i <- i + 3
        Polyline3D cs

    /// Move a Polyline3D by a vector. (same as Polyline3D.translate)
    static member move (v:Vec) (pl:Polyline3D)  : Polyline3D =
        pl.Move v

    /// Returns a Polyline3D moved by a given distance in X direction.
    member p.MoveX (distance:float) : Polyline3D =
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            cs.Add(xyzs.[i] + distance)
            cs.Add(xyzs.[i + 1])
            cs.Add(xyzs.[i + 2])
            i <- i + 3
        Polyline3D cs

    /// Returns a Polyline3D moved by a given distance in X direction.
    static member moveX (distance:float) (pl:Polyline3D)  : Polyline3D =
        pl.MoveX distance

    /// Returns a Polyline3D moved by a given distance in Y direction.
    member p.MoveY (distance:float) : Polyline3D =
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            cs.Add(xyzs.[i])
            cs.Add(xyzs.[i + 1] + distance)
            cs.Add(xyzs.[i + 2])
            i <- i + 3
        Polyline3D cs

    /// Returns a Polyline3D moved by a given distance in Y direction.
    static member moveY (distance:float) (pl:Polyline3D)  : Polyline3D =
        pl.MoveY distance

    /// Returns a Polyline3D moved by a given distance in Z direction.
    member p.MoveZ (distance:float) : Polyline3D =
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            cs.Add(xyzs.[i])
            cs.Add(xyzs.[i + 1])
            cs.Add(xyzs.[i + 2] + distance)
            i <- i + 3
        Polyline3D cs

    /// Returns a Polyline3D moved by a given distance in Z direction.
    static member moveZ (distance:float) (pl:Polyline3D)  : Polyline3D =
        pl.MoveZ distance

    /// Applies or multiplies a 4x4 transformation matrix to the Polyline3D.
    member p.Transform (m:Matrix) : Polyline3D =
        let xyzs = p.XYZs
        let len = xyzs.Count
        let r = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            let x' = m.M11*x + m.M21*y + m.M31*z + m.X41 // * w (= 1.0)
            let y' = m.M12*x + m.M22*y + m.M32*z + m.Y42 // * w
            let z' = m.M13*x + m.M23*y + m.M33*z + m.Z43 // * w
            let w' = m.M14*x + m.M24*y + m.M34*z + m.M44 // * w
            let sc = 1.0 / w'
            r.Add(x' * sc)
            r.Add(y' * sc)
            r.Add(z' * sc)
            i <- i + 3
        Polyline3D r


    /// Applies a 4x4 transformation matrix.
    static member transform (m:Matrix) (pl:Polyline3D) : Polyline3D =
        pl.Transform m

    /// Multiplies (or applies) a RigidMatrix to the Polyline3D.
    member p.TransformRigid (m:RigidMatrix) : Polyline3D =
        let xyzs = p.XYZs
        let len = xyzs.Count
        let r = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            r.Add( m.M11*x + m.M21*y + m.M31*z + m.X41 )
            r.Add( m.M12*x + m.M22*y + m.M32*z + m.Y42 )
            r.Add( m.M13*x + m.M23*y + m.M33*z + m.Z43 )
            i <- i + 3
        Polyline3D r

    /// Multiplies (or applies) a RigidMatrix to the Polyline3D.
    static member transformRigid (m:RigidMatrix) (pl:Polyline3D) : Polyline3D =
        pl.TransformRigid m


    // #endregion
    // #region Rotate

    /// Multiplies (or applies) a Quaternion to the Polyline3D.
    /// The polyline is rotated around the World Origin.
    member p.Rotate (q:Quaternion) : Polyline3D =
        let xyzs = p.XYZs
        let len = xyzs.Count
        let r = ResizeArray<float>(len)
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let mutable i = 0
        while i < len do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            let tx = 2.0 * ( qy * z - qz * y)
            let ty = 2.0 * ( qz * x - qx * z)
            let tz = 2.0 * ( qx * y - qy * x)
            // v + q.w * t + cross( q.xyz, t);
            r.Add( x + qw * tx + qy * tz - qz * ty)
            r.Add( y + qw * ty + qz * tx - qx * tz)
            r.Add( z + qw * tz + qx * ty - qy * tx)
            i <- i + 3
        Polyline3D r

    /// Multiplies (or applies) a Quaternion to the Polyline3D.
    /// The polyline is rotated around the World Origin.
    static member inline rotate (q:Quaternion) (pl:Polyline3D) : Polyline3D =
        pl.Rotate q

    /// Multiplies (or applies) a Quaternion to the Polyline3D to rotate around a given center point.
    member p.RotateWithCenter (cen:Pnt, q:Quaternion) : Polyline3D =
        let xyzs = p.XYZs
        let len = xyzs.Count
        let r = ResizeArray<float>(len)
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let mutable i = 0
        while i < len do
            let x = xyzs.[i    ] - cen.X
            let y = xyzs.[i + 1] - cen.Y
            let z = xyzs.[i + 2] - cen.Z
            let tx = 2.0 * ( qy * z - qz * y)
            let ty = 2.0 * ( qz * x - qx * z)
            let tz = 2.0 * ( qx * y - qy * x)
            // v + q.w * t + cross( q.xyz, t);
            r.Add( x + qw * tx + qy * tz - qz * ty + cen.X)
            r.Add( y + qw * ty + qz * tx - qx * tz + cen.Y)
            r.Add( z + qw * tz + qx * ty - qy * tx + cen.Z)
            i <- i + 3
        Polyline3D r

    /// Multiplies (or applies) a Quaternion to the Polyline3D to rotate around a given center point.
    static member rotateWithCenter (cen:Pnt) (q:Quaternion) (pl:Polyline3D) : Polyline3D =
        pl.RotateWithCenter (cen, q)

    /// Rotation a Polyline3D around Z-Axis.
    member p.RotateOnZ (r:Rotation2D) : Polyline3D =
        let xyzs = p.XYZs
        let len = xyzs.Count
        let n = ResizeArray<float>(len)
        let sin = r.Sin
        let cos = r.Cos
        let mutable i = 0
        while i < len do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            n.Add (cos * x - sin * y)
            n.Add (sin * x + cos * y)
            n.Add (z)
            i <- i + 3
        Polyline3D n

    /// Rotation a Polyline3D around Z-Axis.
    static member rotateOnZ (r:Rotation2D) (pl:Polyline3D) : Polyline3D =
        pl.RotateOnZ r

    /// Rotation a Polyline3D round given center point an a local Z-axis.
    member p.RotateOnZWithCenter (cen:Pnt) (r:Rotation2D) : Polyline3D =
        let xyzs = p.XYZs
        let len = xyzs.Count
        let n = ResizeArray<float>(len)
        let sin = r.Sin
        let cos = r.Cos
        let cx = cen.X
        let cy = cen.Y
        let mutable i = 0
        while i < len do
            let x = xyzs.[i    ] - cx
            let y = xyzs.[i + 1] - cy
            let z = xyzs.[i + 2]
            n.Add (cos * x - sin * y + cx)
            n.Add (sin * x + cos * y + cy)
            n.Add (z)
            i <- i + 3
        Polyline3D n

    /// Rotation a Polyline3D round given center point an a local Z-axis.
    static member rotateOnZWithCenter (cen:Pnt) (r:Rotation2D) (pl:Polyline3D) : Polyline3D =
        pl.RotateOnZWithCenter cen r

    // #endregion
    // #region Static members


    /// Tests if the Polyline3D is Clockwise when projected in 2D (Z values are ignored).
    /// Returns the same instance if the Polyline3D is already Clockwise,
    /// otherwise returns a new reversed Polyline3D.
    static member inline ensureClockwiseIn2D (pl:Polyline3D) : Polyline3D =
        if pl.IsClockwiseIn2D then pl else pl.Reverse()

    /// Tests if the Polyline3D is Counter Clockwise when projected in 2D (Z values are ignored).
    /// Returns the same instance if the Polyline3D is already Counter Clockwise,
    /// otherwise returns a new reversed Polyline3D.
    static member inline ensureCounterClockwiseIn2D (pl:Polyline3D) : Polyline3D =
        if pl.IsCounterClockwiseIn2D then pl else pl.Reverse()

    /// Tests if the Polyline3D is Clockwise when projected in 2D (Z values are ignored).
    /// If not reverse the Polyline3D in place.
    /// Always returns the same instance.
    static member inline ensureClockwiseInPlaceIn2D (pl:Polyline3D) : Polyline3D =
        if pl.IsCounterClockwiseIn2D then pl.ReverseInPlace()
        pl

    /// Tests if the Polyline3D is Counter Clockwise when projected in 2D (Z values are ignored).
    /// If not reverse the Polyline3D in place.
    /// Always returns the same instance.
    static member inline ensureCounterClockwiseInPlaceIn2D (pl:Polyline3D) : Polyline3D =
        if pl.IsClockwiseIn2D then pl.ReverseInPlace()
        pl


    /// <summary>Apply a mapping function to each point in the 3D Polyline. Returns new Polyline3D.</summary>
    /// <param name="mapping">A function that takes a point and returns a new point.</param>
    /// <param name="pl">The Polyline3D to map over.</param>
    /// <returns>A new Polyline3D with the mapped points.</returns>
    static member mapPt (mapping:Pnt-> Pnt) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            let pt = mapping (Pnt(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2]))
            cs.Add pt.X
            cs.Add pt.Y
            cs.Add pt.Z
            i <- i + 3
        Polyline3D cs


    /// <summary>Apply a mapping function to each point in the 3D Polyline3D with point position (not float index). Returns new Polyline3D.</summary>
    /// <param name="mapping">A function that takes the position ( = array index/3) of the point and the point itself, and returns a new point.
    /// </param>
    /// <param name="pl">The Polyline3D to map over.</param>
    /// <returns>A new Polyline3D with the mapped points.</returns>
    static member mapiPt (mapping:int -> Pnt -> Pnt) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            let pt = mapping (i/3) (Pnt(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2]))
            cs.Add pt.X
            cs.Add pt.Y
            cs.Add pt.Z
            i <- i + 3
        Polyline3D(cs)

    /// <summary>Apply a mapping function to each point in the 3D Polyline. Returns new Polyline3D.</summary>
    /// <param name="mapping">A function that takes the X, Y and Z coordinates of a point and returns a new point.</param>
    /// <param name="pl">The Polyline3D to map over.</param>
    /// <returns>A new Polyline3D with the mapped points.</returns>
    static member map (mapping:float -> float -> float -> Pnt) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            let pt = mapping xyzs.[i] xyzs.[i + 1] xyzs.[i + 2]
            cs.Add pt.X
            cs.Add pt.Y
            cs.Add pt.Z
            i <- i + 3
        Polyline3D cs

    /// <summary>Apply a mapping function to each point in the 3D Polyline with index. Returns new Polyline3D.</summary>
    /// <param name="mapping">A function that takes the point-index and the X, Y and Z coordinates of a point and returns a new point.</param>
    /// <param name="pl">The Polyline3D to map over.</param>
    /// <returns>A new Polyline3D with the mapped points.</returns>
    static member mapi (mapping:int -> float -> float -> float -> Pnt) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        let mutable idx = 0
        while i < len do
            let pt = mapping idx xyzs.[i] xyzs.[i + 1] xyzs.[i + 2]
            cs.Add pt.X
            cs.Add pt.Y
            cs.Add pt.Z
            i <- i + 3
            idx <- idx + 1
        Polyline3D cs

    /// <summary>Iterate over each point in the 3D Polyline and perform an action.</summary>
    /// <param name="action">A function that takes the X, Y and Z coordinates of a point and performs an action (returns unit).</param>
    /// <param name="pl">The Polyline3D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member iter (action:float -> float -> float -> unit) (pl:Polyline3D) : unit =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            action xyzs.[i] xyzs.[i + 1] xyzs.[i + 2]
            i <- i + 3

    /// <summary>Iterate over each point in the 3D Polyline with index and perform an action.</summary>
    /// <param name="action">A function that takes the point-index and the X, Y and Z coordinates of a point and performs an action (returns unit).</param>
    /// <param name="pl">The Polyline3D to iterate over.</param>
    /// <returns>Unit.</returns>
    static member iteri (action:int -> float -> float -> float -> unit) (pl:Polyline3D) : unit =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let mutable i = 0
        let mutable idx = 0
        while i < len do
            action idx xyzs.[i] xyzs.[i + 1] xyzs.[i + 2]
            i <- i + 3
            idx <- idx + 1

    /// Move a Polyline3D by a vector. (same as Polyline3D.move)
    static member translate (v:Vec) (pl:Polyline3D)  : Polyline3D =
        pl.Move v


    // #endregion
    // #region Create

    /// Creates a Polyline3D from a list of objects with X, Y, and Z members (uppercase).
    static member inline createFromXYZMembers (xyzObjs: seq< ^T >) : Polyline3D =
        let coordinates = ResizeArray<float>()
        for pt in xyzObjs do
            let x = float (^T : (member X : _) pt)
            let y = float (^T : (member Y : _) pt)
            let z = float (^T : (member Z : _) pt)
            coordinates.Add x
            coordinates.Add y
            coordinates.Add z
        Polyline3D.createDirectly coordinates

    /// Creates a Polyline3D from a list of objects with x, y, and z members (lowercase).
    static member inline createFromxyzMembers (xyzObjs: seq< ^T >) : Polyline3D =
        let coordinates = ResizeArray<float>()
        for pt in xyzObjs do
            let x = float (^T : (member x : _) pt)
            let y = float (^T : (member y : _) pt)
            let z = float (^T : (member z : _) pt)
            coordinates.Add x
            coordinates.Add y
            coordinates.Add z
        Polyline3D.createDirectly coordinates

    /// Creates a Polyline3D from a list of objects with X and Y members (uppercase), using 0.0 for Z.
    static member inline createFrom2DXYMembers (xyObjs: seq< ^T >) : Polyline3D =
        let coordinates = ResizeArray<float>()
        for pt in xyObjs do
            let x = float (^T : (member X : _) pt)
            let y = float (^T : (member Y : _) pt)
            coordinates.Add x
            coordinates.Add y
            coordinates.Add 0.0
        Polyline3D.createDirectly coordinates

    /// Creates a Polyline3D from a list of objects with x and y members (lowercase), using 0.0 for z.
    static member inline createFrom2DxyMembers (xyObjs: seq< ^T >) : Polyline3D =
        let coordinates = ResizeArray<float>()
        for pt in xyObjs do
            let x = float (^T : (member x : _) pt)
            let y = float (^T : (member y : _) pt)
            coordinates.Add x
            coordinates.Add y
            coordinates.Add 0.0
        Polyline3D.createDirectly coordinates

    /// Create a new Polyline3D by copying over all points.
    static member create(points: seq<Pnt>) : Polyline3D =
        Polyline3D(points)

    /// Create a new Polyline3D by using the provided X, Y and Z interleaved ResizeArray directly.
    /// Unsafe because later changes to the ResizeArray will be reflected in the Polyline3D.
    static member createDirectly (xyzs: ResizeArray<float>) : Polyline3D =
        if isNull xyzs then failNull "Polyline3D.createDirectly" "coordinates"
        if xyzs.Count % 3 <> 0 then
            fail $"Polyline3D.createDirectly: coordinate buffer must contain a multiple of three floats, but has {xyzs.Count} values."
        Polyline3D(xyzs)

    /// Create a new empty Polyline3D without any points.
    /// But predefined capacity.
    static member inline createEmpty (capacity:int) : Polyline3D =
        Polyline3D(capacity)

    /// Creates a Polyline3D starting at the Origin,
    /// going to x, then x+y, then y and back to origin.
    static member createFromRect3D (r:Rect3D)  : Polyline3D =
        Polyline3D r.PointsLooped

    /// Returns new Polyline3D from point at Parameter a to point at Parameter b.
    /// If 'a' is bigger than 'b' then the new Polyline3D is in opposite direction.
    /// If the start parameter is less than 1e-4 below the next integer it snaps up to that vertex.
    /// If the end parameter is less than 1e-4 above the previous integer it snaps down to that vertex.
    /// Raises an error if a parameter is outside the domain of the Polyline3D (0.0 to points.Count - 1.0).
    static member subPolyline a b (pl:Polyline3D) : Polyline3D =
        let rev = a>b
        let u, v = if rev then b, a else a, b
        let np = Polyline3D.createEmpty (int(v-u)+2)
        // first point
        let ui = int u
        let uf = u - float ui
        if uf < 0.9999 then
            let p = pl.EvaluateAt u
            np.AddXYZ (p.X, p.Y, p.Z)
        // inner points
        for i = int u + 1 to int v do
            if i >= 0 && i < pl.PointCount then
                np.AddXYZ (pl.GetX i, pl.GetY i, pl.GetZ i)
        // last point
        let vi = int v
        let vf = v - float vi
        if vf > 1e-4 then
            let p = pl.EvaluateAt v
            np.AddXYZ (p.X, p.Y, p.Z)
        // reverse if necessary
        if rev then
            np.ReverseInPlace()
        np

    /// Returns a new closed Polyline3D.
    /// If the first and last point are within the tolerance of each other, the last point is set equal to the first point.
    /// Otherwise one point is added.
    /// Raises an error if the Polyline3D has fewer than 3 points, same as CloseInPlace.
    static member close (toleranceForAddingPoint:float) (pl:Polyline3D) : Polyline3D =
        if pl.PointCount < 3 then failTooFewPoly3D "close" 3 pl.PointCount
        let np = pl.Duplicate()
        let sx = np.GetX 0
        let sy = np.GetY 0
        let sz = np.GetZ 0
        let lastIdx = np.PointCount - 1
        let ex = np.GetX lastIdx
        let ey = np.GetY lastIdx
        let ez = np.GetZ lastIdx
        let dx = sx - ex
        let dy = sy - ey
        let dz = sz - ez
        if dx * dx + dy * dy + dz * dz <= toleranceForAddingPoint * toleranceForAddingPoint then // <= needed so it works with 0.0
            np.SetPointXYZ (lastIdx, sx, sy, sz) // set last point equal to first
        else
            np.AddXYZ (sx, sy, sz)
        np

    /// Tests if two Polyline3D have the same number of points and points are equal within a given tolerance.
    static member equals (tol:float) (a:Polyline3D) (b:Polyline3D)  : bool =
        let k = a.PointCount
        if k <> b.PointCount then
            false
        else
            let mutable i = 0
            let mutable same = true
            while i < k && same do
                let dx = a.GetX i - b.GetX i
                let dy = a.GetY i - b.GetY i
                let dz = a.GetZ i - b.GetZ i
                if dx * dx + dy * dy + dz * dz <= tol * tol then // Euclidean distance, same as Polyline2D.equals
                    i <- i + 1
                else
                    same <- false
            same

    /// Removes consecutive duplicate points from the Polyline3D within a given tolerance.
    /// This algorithm allows the last and first point to be identical if the Polyline3D is closed.
    static member removeDuplicatePoints (distanceTolerance:float) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        if xyzs.Count < 6 then // single point or empty polyline
            pl
        else
            let len = xyzs.Count
            let toSq = distanceTolerance * distanceTolerance
            let nps = ResizeArray<float>(len)
            let mutable lastX = xyzs.[0]
            let mutable lastY = xyzs.[1]
            let mutable lastZ = xyzs.[2]
            nps.Add lastX
            nps.Add lastY
            nps.Add lastZ
            let mutable i = 3
            while i < len do
                let x = xyzs.[i]
                let y = xyzs.[i + 1]
                let z = xyzs.[i + 2]
                let dx = x - lastX
                let dy = y - lastY
                let dz = z - lastZ
                if dx * dx + dy * dy + dz * dz > toSq then
                    nps.Add x
                    nps.Add y
                    nps.Add z
                    lastX <- x
                    lastY <- y
                    lastZ <- z
                i <- i + 3
            if nps.Count >= 6 then // if only one point is left keep the start point, don't overwrite it with the end point
                nps.[nps.Count - 3] <- xyzs.[xyzs.Count - 3] // ensure last point is not moved by the algorithm, it might be off by distanceTolerance
                nps.[nps.Count - 2] <- xyzs.[xyzs.Count - 2]
                nps.[nps.Count - 1] <- xyzs.[xyzs.Count - 1]
            Polyline3D.createDirectly nps

    /// <summary>Removes consecutive duplicate points from the Polyline3D within a given tolerance.</summary>
    /// <param name="distanceTolerance"> The distance within which points are considered duplicates. </param>
    /// <param name="pl"> A 3D Polyline, open or closed. </param>
    /// <remarks>This algorithm ensures to keep edges in their position by re-intersecting segments at their closest approach if points are closer than the distanceTolerance but not identical.
    /// The position of start and end point is NOT changed. Use Polyline3D.close to ensure start and end point are identical.</remarks>
    static member removeDuplicatePointsFaithfully (distanceTolerance:float) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        if xyzs.Count < 6 then // single point or empty polyline
            pl
        else
            let tolSq = distanceTolerance * distanceTolerance
            let nps = ResizeArray<float>(xyzs.Count)
            let mutable prevX = xyzs.[0]
            let mutable prevY = xyzs.[1]
            let mutable prevZ = xyzs.[2]
            let mutable prevWasBad = false
            nps.Add prevX
            nps.Add prevY
            nps.Add prevZ
            let mutable i = 3
            while i < xyzs.Count do
                let thisX = xyzs.[i]
                let thisY = xyzs.[i + 1]
                let thisZ = xyzs.[i + 2]
                let dx = thisX - prevX
                let dy = thisY - prevY
                let dz = thisZ - prevZ
                let dSq = dx * dx + dy * dy + dz * dz
                // the distance is too small:
                if dSq < tolSq then
                    if dSq > 1e-24 then
                        // if the points are not identical, we will try to keep line fidelity by re-intersecting segments.
                        prevWasBad <- true

                // the distance is OK:
                else
                    // (a) fix the last point if it was bad and not the first point
                    if prevWasBad && nps.Count >= 6 then // if nps has less than 2 points we keep the start point exactly. like at the end see below.
                        // else, if nps has at least 2 points, re-intersect the edge from lastBad to this with the edge from prev back to the previous ok point
                        let lastBadX = xyzs.[i - 3] // prev is the last point of a cluster of too close points, so we use this as the last point of the edge.
                        let lastBadY = xyzs.[i - 2]
                        let lastBadZ = xyzs.[i - 1]
                        let vLastX = thisX - lastBadX // Vec.create(lastBadPt, this)
                        let vLastY = thisY - lastBadY
                        let vLastZ = thisZ - lastBadZ
                        let prevOkX = nps.[nps.Count - 6] // the point in nps before prev
                        let prevOkY = nps.[nps.Count - 5]
                        let prevOkZ = nps.[nps.Count - 4]
                        let vFirstX = prevOkX - prevX // Vec.create(prev, prevOkPt), prev is the first bad point
                        let vFirstY = prevOkY - prevY
                        let vFirstZ = prevOkZ - prevZ
                        match XLine3D.tryClosestParameterRayA(lastBadX, lastBadY, lastBadZ, prevX, prevY, prevZ, vLastX, vLastY, vLastZ, vFirstX, vFirstY, vFirstZ) with
                        |Some t ->
                            // the expected parameter on A is somewhere round 0.0, so use -0.4 to 0.4 as range,
                            let x = lastBadX + t * vLastX // t is the parameter on line A (lastBad, vLast)
                            let y = lastBadY + t * vLastY
                            let z = lastBadZ + t * vLastZ
                            // the closest-approach point must stay within the distanceTolerance of the cluster it replaces,
                            // measured against prev (last OK point) and lastBad (start of the following OK segment).
                            let dPrevX = x - prevX
                            let dPrevY = y - prevY
                            let dPrevZ = z - prevZ
                            let dLastX = x - lastBadX
                            let dLastY = y - lastBadY
                            let dLastZ = z - lastBadZ
                            if t > -0.4 && t < 0.4
                               && dPrevX * dPrevX + dPrevY * dPrevY + dPrevZ * dPrevZ < tolSq
                               && dLastX * dLastX + dLastY * dLastY + dLastZ * dLastZ < tolSq then
                                nps.[nps.Count - 3] <- x // replace prev that was already set
                                nps.[nps.Count - 2] <- y
                                nps.[nps.Count - 1] <- z
                            else
                                // closest approach is too far away or segments are near parallel, line fidelity is not kept, use the midpoint
                                nps.[nps.Count - 3] <- (lastBadX + prevX) * 0.5
                                nps.[nps.Count - 2] <- (lastBadY + prevY) * 0.5
                                nps.[nps.Count - 1] <- (lastBadZ + prevZ) * 0.5
                        |None -> // segments are parallel, no closest approach parameter
                            nps.[nps.Count - 3] <- (lastBadX + prevX) * 0.5 // in this case line fidelity is not kept, use the midpoint
                            nps.[nps.Count - 2] <- (lastBadY + prevY) * 0.5
                            nps.[nps.Count - 1] <- (lastBadZ + prevZ) * 0.5

                    // (b) just use this point, no last points to fix
                    nps.Add thisX
                    nps.Add thisY
                    nps.Add thisZ
                    prevX <- thisX
                    prevY <- thisY
                    prevZ <- thisZ
                    prevWasBad <- false

                i <- i + 3

            // last point :
            if prevWasBad then
                // in this case we are maintaining the last point exactly. like we do with the first point.
                nps.[nps.Count - 3] <- xyzs.[xyzs.Count - 3]
                nps.[nps.Count - 2] <- xyzs.[xyzs.Count - 2]
                nps.[nps.Count - 1] <- xyzs.[xyzs.Count - 1]

            Polyline3D.createDirectly nps

    /// Removes consecutive duplicate points and colinear points from the Polyline3D within given tolerances.
    /// This algorithm allows the last and first point to be identical if the Polyline3D is closed.
    /// Colinear points are removed when the angle between segments is smaller than the cosine threshold (e.g. cosine of 0.5 degrees ).
    /// If the Polyline3D is closed and starts and ends with colinear segments, the first point is replaced with the last non-colinear point.
    /// So the joint of the loop is now moved to the last non-colinear point.
    /// So that there are no colinear segments even between start and end.
    /// Raises an error if all points are within the distanceTolerance of the first point.
    static member removeDuplicateAndColinearPoints (angleTolerance:float<Cosine.cosine>) (distanceTolerance:float) (pl:Polyline3D) : Polyline3D =
        if angleTolerance < Cosine.``45.0`` then
            fail $"Polyline3D.removeDuplicateAndColinearPoints: angleTolerance must be at least Cosine.``45.0`` ( that is 0.707) but was {angleTolerance} (= {acos (float angleTolerance) |> toDegrees} degrees)."
        if angleTolerance > Cosine.``0.01`` then
            fail $"Polyline3D.removeDuplicateAndColinearPoints: angleTolerance must be at most Cosine.``0.01`` ( that is 0.999999984) but was {angleTolerance} (= {acos (float angleTolerance) |> toDegrees} degrees)."

        let pts = pl.AsPoints
        if pts.Count < 2 then // single point or empty polyline
            pl
        else
            let distTol = max distanceTolerance 1e-6 // vectors need to be longer than zero, otherwise unitizing would fail
            let nps = ResizeArray<Pnt>(pts.Count)

            let lastIdx = pts.LastIndex
            let mutable prev = pts.[0]
            nps.Add prev // add first  point

            // find first non-duplicate point:
            let mutable i = 1
            let mutable this = pts.[i]
            let mutable len = Pnt.dist prev this
            while len < distTol && i < lastIdx do
                i <- i + 1
                this  <- pts.[i]
                len   <- Pnt.dist prev this

            if len < distTol then
                fail $"Polyline3D.removeDuplicateAndColinearPoints: all {pts.Count} points are within the distanceTolerance {distTol} of the first point {prev}."

            let firstVec = UnitVec.create(prev, this)
            let mutable vPrev = firstVec

            // main loop:
            for idx = i + 1 to lastIdx do
                let next = pts.[idx]
                let vx = next.X - this.X
                let vy = next.Y - this.Y
                let vz = next.Z - this.Z
                let len = vx * vx + vy * vy + vz * vz |> sqrt
                if len > distTol then
                    let f  = 1.0 / len
                    let vNext = UnitVec.createUnchecked(vx * f, vy * f, vz * f)
                    let cos = UnitVec.dot (vPrev, vNext)
                    if withMeasure cos < angleTolerance then
                        // not colinear , keep this point
                        nps.Add this
                        prev <- this
                        vPrev <- vNext // advance previous vector only when point kept
                    this <- next // always advance this point

            // handle last segment to first point
            if pl.IsAlmostClosed distTol then
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
                if Pnt.notEquals distTol this prev then
                    nps.Add this
                else
                    nps.Last <- pts.Last // ensure last point is not changed, it might be off by distanceTolerance

            Polyline3D nps

    /// <summary>Removes simple sharp U-Turns from a Polyline</summary>
    /// <param name="minCos"> The angle between segments so that they are considered a U-turn.
    /// For example, for 179.9 degrees use 'Cosine.``179.9``'. </param>
    /// <param name="polyLine"> A 3D Polyline, open or closed. </param>
    /// <remarks>The Offset3D module also has a removeUTurns function that takes precomputed unit vectors and is therefore more efficient.
    /// For nested U-turns and colinear points in U-turns segments use 'removeUTurnsDeeply'. That function calls repeatedly until no more U-turns are present.</remarks>
    /// <returns>If no U-turns are present, the List of points of the original Polyline3D is reused and a new Polyline3D is created with that list.
    /// If U-turns are present, a new ResizeArray of points is returned with simple U-turns removed.</returns>
    static member removeUTurns (minCos:float<Cosine.cosine>) (polyLine:Polyline3D) : Polyline3D =
        let uvs = Offset3D.getSegmentUnitVectors polyLine.XYZs
        let pts = Offset3D.removeUTurns(polyLine.XYZs, uvs, minCos)
        Polyline3D pts

    /// <summary>Removes all sharp U-Turns from a Polyline recursively until no more U-turns are present.
    /// This function calls 'removeUTurns' repeatedly until no more U-turns are present.
    /// Use this function when you have nested U-turns and colinear points in U-turns segments.</summary>
    /// <param name="minCos"> The angle between segments so that they are considered a U-turn.
    /// For example, for 179.9 degrees use 'Cosine.``179.9``'. </param>
    /// <param name="polyLine"> A 3D Polyline, open or closed. </param>
    /// <returns>If no U-turns are present, the List of points of the original Polyline3D is reused and a new Polyline3D is created with that list.
    /// If U-turns are present, a new ResizeArray of points is returned with all U-turns removed.</returns>
    static member removeUTurnsDeeply (minCos:float<Cosine.cosine>) (polyLine:Polyline3D) : Polyline3D =
        let mutable input = polyLine
        let mutable output = Polyline3D.removeUTurns minCos polyLine
        while output.PointCount <> input.PointCount do
            input  <- output
            output <- Polyline3D.removeUTurns minCos input
        output

    // #endregion
    // #region Offset

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
        let xyzs = polyLine.XYZs
        let ptCount = polyLine.PointCount
        if ptCount < 2 then
            fail $"Polyline3D.offsetWithRef: Polyline3D must have at least 2 points but has {ptCount} points."
        // check if looping desired and curve is open
        let dx = xyzs.[0] - xyzs.[xyzs.Count - 3]
        let dy = xyzs.[1] - xyzs.[xyzs.Count - 2]
        let dz = xyzs.[2] - xyzs.[xyzs.Count - 1]
        if loop && dx * dx + dy * dy + dz * dz > Offset3D.sqOpenTolerance then
            let closedXYZs = ResizeArray<float>(xyzs.Count + 3)
            closedXYZs.AddRange xyzs
            closedXYZs.Add xyzs.[0]
            closedXYZs.Add xyzs.[1]
            closedXYZs.Add xyzs.[2]
            let uvs  = Offset3D.getSegmentUnitVectors closedXYZs
            let dirs = Offset3D.getOffsetDirections(uvs, refNormal, false, Cosine.``2.5``, Cosine.``179.0``)
            let res  = Offset3D.offsetConstantWithDirections(closedXYZs, dirs, inPlaneOffsetDistance, perpendicularOffsetDistance)
            ResizeArr.popOff res// remove last point to open the polyline again
            ResizeArr.popOff res
            ResizeArr.popOff res
            Polyline3D.createDirectly res
        else
            let uvs = Offset3D.getSegmentUnitVectors xyzs
            let isOpen = dx * dx + dy * dy + dz * dz > Offset3D.sqOpenTolerance
            let dirs = Offset3D.getOffsetDirections(uvs, refNormal, isOpen, Cosine.``2.5``, Cosine.``179.0``)
            Offset3D.offsetConstantWithDirections(xyzs, dirs, inPlaneOffsetDistance, perpendicularOffsetDistance)
            |> Polyline3D.createDirectly

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
        if polyLine.PointCount < 2 then
            fail $"Polyline3D.offset: Polyline3D must have at least 2 points but has {polyLine.PointCount} points."
        let refNormal = polyLine.AverageNormal
        if refNormal.LengthSq < 1e-8 then
            fail $"Polyline3D.offset: Cannot compute average normal of Polyline3D, the {polyLine.PointCount} points are colinear or too close together: {polyLine}"
        Polyline3D.offsetWithRef(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistance, refNormal.Unitized, loop)

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// The reference normal is computed from the average normal of the polyline.
    /// This function raises an Exception on duplicate points, 180 degree U-turns, or colinear points.</summary>
    /// <param name="inPlaneOffsetDistance">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="perpendicularOffsetDistance">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counterclockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offset' (inPlaneOffsetDistance:float) (perpendicularOffsetDistance:float) (polyLine:Polyline3D): Polyline3D =
        Polyline3D.offset(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistance)

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Takes a reference normal for orienting the perpendicular offset and determining inside/outside.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// This function raises an Exception on duplicate points and 180 degree U-turns.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistances">The offset distances in the local plane defined by two segments. One distance per segment.
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
                            inPlaneOffsetDistances: Collections.Generic.IList<float>,
                            perpendicularOffsetDistances: Collections.Generic.IList<float>,
                            refNormal: UnitVec,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(Offset3D.VarDistParallel.Fail)>] varDistParallelBehaviour: Offset3D.VarDistParallel, // fail by default
                            [<OPT;DEF(Cosine.``2.5``)>] considerColinearBelow: float<Cosine.cosine>,
                            [<OPT;DEF(Cosine.``175.0``)>] failAtUTurnAbove :float<Cosine.cosine>
                            ) : Polyline3D =
        let xyzs = polyLine.XYZs
        let ptCount = polyLine.PointCount
        if ptCount < 2 then
            fail $"Polyline3D.offsetVarWithRef: Polyline3D must have at least 2 points but has {ptCount} points."
        // check if looping desired and curve is open
        let dx = xyzs.[0] - xyzs.[xyzs.Count - 3]
        let dy = xyzs.[1] - xyzs.[xyzs.Count - 2]
        let dz = xyzs.[2] - xyzs.[xyzs.Count - 1]
        let isOpen = dx * dx + dy * dy + dz * dz > Offset3D.sqOpenTolerance
        if loop && isOpen then
            if inPlaneOffsetDistances.Count <> ptCount then
                fail ($"Polyline3D.offsetVarWithRef: For open Polyline3D with loop=true the inPlaneOffsetDistances list must have the same number of items as the polyline has points.\n" +
                      $"But polyline has {ptCount} points and inPlaneOffsetDistances has {inPlaneOffsetDistances.Count} items.")
            if perpendicularOffsetDistances.Count <> ptCount then
                fail ($"Polyline3D.offsetVarWithRef: For open Polyline3D with loop=true the perpendicularOffsetDistances list must have the same number of items as the polyline has points.\n" +
                      $"But polyline has {ptCount} points and perpendicularOffsetDistances has {perpendicularOffsetDistances.Count} items.")
            let closedXYZs = ResizeArray<float>(xyzs.Count + 3)
            closedXYZs.AddRange xyzs
            closedXYZs.Add xyzs.[0]
            closedXYZs.Add xyzs.[1]
            closedXYZs.Add xyzs.[2]
            let uvs  = Offset3D.getSegmentUnitVectors closedXYZs
            let dirs = Offset3D.getOffsetDirections(uvs, refNormal, false, considerColinearBelow, failAtUTurnAbove)
            let res  = Offset3D.offsetVariableWithDirections(closedXYZs, uvs, dirs, inPlaneOffsetDistances, perpendicularOffsetDistances, true, varDistParallelBehaviour)
            res.RemoveAt(res.Count - 1) // remove last point to open the polyline again
            res.RemoveAt(res.Count - 1)
            res.RemoveAt(res.Count - 1)
            Polyline3D.createDirectly res
        else
            let uvs = Offset3D.getSegmentUnitVectors xyzs
            let dirs = Offset3D.getOffsetDirections(uvs, refNormal, isOpen, considerColinearBelow, failAtUTurnAbove)
            Offset3D.offsetVariableWithDirections(xyzs, uvs, dirs, inPlaneOffsetDistances, perpendicularOffsetDistances, isOpen, varDistParallelBehaviour)
            |> Polyline3D.createDirectly

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// The reference normal is computed from the average normal of the polyline.
    /// This function raises an Exception on duplicate points, 180 degree U-turns, or colinear points.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistances">The offset distances in the local plane defined by two segments. One distance per segment.
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
                            inPlaneOffsetDistances: ResizeArray<float>,
                            perpendicularOffsetDistances: ResizeArray<float>,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(Offset3D.VarDistParallel.Fail)>] varDistParallelBehaviour: Offset3D.VarDistParallel, // fail by default
                            [<OPT;DEF(Cosine.``2.5``)>] considerColinearBelow: float<Cosine.cosine>,
                            [<OPT;DEF(Cosine.``175.0``)>] failAtUTurnAbove :float<Cosine.cosine>
                            ) : Polyline3D =
        if polyLine.PointCount < 2 then
            fail $"Polyline3D.offsetVar: Polyline3D must have at least 2 points but has {polyLine.PointCount} points."
        let refNormal = polyLine.AverageNormal
        if refNormal.LengthSq < 1e-8 then
            fail $"Polyline3D.offsetVar: Cannot compute average normal of Polyline3D, the {polyLine.PointCount} points are colinear or too close together: {polyLine}"
        Polyline3D.offsetVarWithRef(polyLine, inPlaneOffsetDistances, perpendicularOffsetDistances, refNormal.Unitized, loop, varDistParallelBehaviour, considerColinearBelow, failAtUTurnAbove)





    // #endregion
    // #region Obsolete

    [<Obsolete("This is no longer the internal structure of Polyline3D, use .XYZs instead. Or .AsPoints to get points.")>]
    member p.Points : ResizeArray<Pnt> =
        p.AsPoints

    [<Obsolete("This was semantically unclear, what happens at vertex? Use GetSegment(i).UnitTangent instead")>]
    member pl.TangentAt(_t:float) : 'a =
        fail "Polyline3D.TangentAt is obsolete, use GetSegment(i).UnitTangent instead." |> unbox // unbox to make type checker happy

    [<Obsolete("Renamed to Polyline3D.subPolyline")>]
    static member segment a b (pl:Polyline3D) :Polyline3D =
        Polyline3D.subPolyline a b pl

    [<Obsolete("Use polyline3D.CloseInPlace instead.")>]
    member p.CloseIfOpen(t) : unit =
        p.CloseInPlace(t)

    [<Obsolete("Use polyline3D.CloseInPlace instead.")>]
    static member inline closeIfOpen (t) (pl:Polyline3D) : unit =
        pl.CloseInPlace(t)

    [<Obsolete("Use Polyline3D.rotate instead.")>]
    static member rotateByQuaternion (q:Quaternion) (pl:Polyline3D) : Polyline3D =
        Polyline3D.rotate q pl

    [<Obsolete("Use Polyline3D.rotateWithCenter instead.")>]
    static member rotateWithCenterByQuaternion (cen:Pnt) (q:Quaternion) (pl:Polyline3D) : Polyline3D =
        Polyline3D.rotateWithCenter cen q pl

    [<Obsolete("Use Polyline3D.rotateOnZ instead.")>]
    static member rotate2D (r:Rotation2D) (pl:Polyline3D) : Polyline3D =
        Polyline3D.rotateOnZ r pl

    [<Obsolete("Since the internal structure of Polyline3D has changed this is not anymore a direct creation but a copy to a flat array.")>]
    static member createDirectlyUnsafe (points: ResizeArray<Pnt>) : Polyline3D =
        Polyline3D(points)

    [<Obsolete("Since the internal structure of Polyline3D has changed to a flat array using this for looping doesn't make sense any more.")>]
    member p.LastPointIndex : int =
        p.PointCount - 1

    [<Obsolete("Since the internal structure of Polyline3D has changed to a flat array using this for looping doesn't make sense any more.")>]
    member p.LastSegmentIndex : int =
        p.PointCount - 2



