namespace Euclid

open System
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open System.Collections.Generic
open EuclidErrors
open Euclid.EuclidCollectionUtilities
#if !FABLE_COMPILER
open System.Text.Json.Serialization
#endif


module private Polyline3DUtil =

    let inline countPts (xyzs: ResizeArray<float>) = ResizeArr.len xyzs / 3

    let failPointIndex methodName idx (xyzs: ResizeArray<float>) =
        fail $"Polyline3D.{methodName}: pointIndex {idx} is out of range for Polyline3D with {countPts xyzs} points."


    let inline getPt i (xyzs: ResizeArray<float>) = Pnt(xyzs.[i * 3], xyzs.[i * 3 + 1], xyzs.[i * 3 + 2])

    let inline setCoordXYZ i x y z (xyzs: ResizeArray<float>) =
        xyzs.[i * 3    ] <- x
        xyzs.[i * 3 + 1] <- y
        xyzs.[i * 3 + 2] <- z

    let inline moveCoordXYZ i x y z (xyzs: ResizeArray<float>) =
        xyzs.[i * 3    ] <- xyzs.[i * 3    ] + x
        xyzs.[i * 3 + 1] <- xyzs.[i * 3 + 1] + y
        xyzs.[i * 3 + 2] <- xyzs.[i * 3 + 2] + z


    let inline appendXYZ x y z (xyzs: ResizeArray<float>) =
        xyzs.Add x
        xyzs.Add y
        xyzs.Add z

    let inline copy (xyzs: ResizeArray<float>) : ResizeArray<float> =
        xyzs.GetRange(0, xyzs.Count)

    /// Sums up the cross products of all points around their center: Newell's method.
    /// The result is not unitized, its length is twice the area of the polygon projected onto that plane.
    /// Does not check for bad input, the result may be zero length, e.g. if all points are in one line.
    /// There must be at least one point.
    let inline averageNormalOf (xyzs: ResizeArray<float>) : Vec =
        Offset3D.averageNormal xyzs

    /// Tries to find the average plane of the points.
    /// If 'checkPlanarity' is TRUE it also verifies that every point is within the tolerance of that plane.
    /// The 'tolerance' is the maximum distance any point may have from the plane, and from one point or one line.
    /// The origin of the returned plane is the average of all points.
    /// Returns ValueNone if there are less than 3 points,
    /// if all points are within the tolerance of one point or of one line,
    /// or, when checking planarity, if any point is further away from the plane than the tolerance.
    let tryPlaneOf (checkPlanarity:bool) (tolerance:float) (xyzs: ResizeArray<float>) : NPlane voption =
        let ptCount = countPts xyzs
        if ptCount < 3 then
            ValueNone
        else
            let len = xyzs.Count
            // the center of all points is the origin of the plane
            let mutable ox = 0.0
            let mutable oy = 0.0
            let mutable oz = 0.0
            let mutable i = 0
            while i < len do
                ox <- ox + xyzs.[i    ]
                oy <- oy + xyzs.[i + 1]
                oz <- oz + xyzs.[i + 2]
                i <- i + 3
            let count = float ptCount // divide, just like Polyline3D.Center does, so the origin is the very same point
            ox <- ox / count
            oy <- oy / count
            oz <- oz / count

            // Newell's method: sum up the cross products of all points around the center.
            // This averages out the noise of near planar points.
            // At the same time find the point that is furthest from the center, it spans the main axis.
            let mutable nx = 0.0
            let mutable ny = 0.0
            let mutable nz = 0.0
            let mutable ux = 0.0 // the main axis
            let mutable uy = 0.0
            let mutable uz = 0.0
            let mutable uLenSq = 0.0
            let mutable ax = xyzs.[len - 3] - ox // the previous point, starting at the last one to close the loop
            let mutable ay = xyzs.[len - 2] - oy
            let mutable az = xyzs.[len - 1] - oz
            i <- 0
            while i < len do
                let bx = xyzs.[i    ] - ox
                let by = xyzs.[i + 1] - oy
                let bz = xyzs.[i + 2] - oz
                nx <- nx + ay * bz - az * by
                ny <- ny + az * bx - ax * bz
                nz <- nz + ax * by - ay * bx
                let lenSq = XYZ.lengthSq bx by bz
                if lenSq > uLenSq then
                    uLenSq <- lenSq
                    ux <- bx
                    uy <- by
                    uz <- bz
                ax <- bx
                ay <- by
                az <- bz
                i <- i + 3

            if uLenSq <= tolerance * tolerance then
                ValueNone // all points are within the tolerance of the center point, so there is no unique plane
            else
                // find the point that is furthest away from the main axis.
                // The length of this cross product is the distance from the axis times the length of the axis.
                let mutable px = 0.0
                let mutable py = 0.0
                let mutable pz = 0.0
                let mutable pLenSq = 0.0
                i <- 0
                while i < len do
                    let vx = xyzs.[i    ] - ox
                    let vy = xyzs.[i + 1] - oy
                    let vz = xyzs.[i + 2] - oz
                    let wx = uy * vz - uz * vy
                    let wy = uz * vx - ux * vz
                    let wz = ux * vy - uy * vx
                    let lenSq = XYZ.lengthSq wx wy wz
                    if lenSq > pLenSq then
                        pLenSq <- lenSq
                        px <- wx
                        py <- wy
                        pz <- wz
                    i <- i + 3

                if pLenSq <= tolerance * tolerance * uLenSq then
                    ValueNone // all points are within the tolerance of one line, so there is no unique plane
                else
                    // Newell's normal is preferred, it uses all points. But it cancels out to zero on
                    // self intersecting or very small shapes. Then the normal of the widest triangle is used.
                    let mutable mx = nx
                    let mutable my = ny
                    let mutable mz = nz
                    if isTooSmallSq (XYZ.lengthSq nx ny nz) then
                        if XYZ.dot px py pz nx ny nz < 0.0 then // keep the orientation of the point order if there is any
                            mx <- -px
                            my <- -py
                            mz <- -pz
                        else
                            mx <- px
                            my <- py
                            mz <- pz
                    let f = 1.0 / XYZ.length mx my mz
                    let normX = mx * f
                    let normY = my * f
                    let normZ = mz * f
                    // check that all points are within the tolerance of this plane
                    let mutable isPlanar = true
                    let mutable k = if checkPlanarity then 0 else len
                    while isPlanar && k < len do
                        let d = (xyzs.[k] - ox) * normX + (xyzs.[k + 1] - oy) * normY + (xyzs.[k + 2] - oz) * normZ
                        if abs d > tolerance then isPlanar <- false
                        k <- k + 3
                    if isPlanar then
                        ValueSome (NPlane.createUnchecked(ox, oy, oz, normX, normY, normZ))
                    else
                        ValueNone


open Polyline3DUtil

/// A class holding a list of 3D points representing a mutable 3D Polyline.
/// If the last point is the same as the first point, the Polyline3D is closed.
/// The source-of-truth storage is an interleaved float buffer: x0, y0, z0, x1, y1, z1, ...
/// The instance members for transformations, like Move, Scale or Rotate, and for clean up, like Reverse, Close or RemoveDuplicatePoints,
/// mutate the Polyline3D in place and return unit.
/// Their static counterparts, like Polyline3D.move, Polyline3D.reverse or Polyline3D.removeDuplicatePoints,
/// return a new Polyline3D and leave the input unchanged.
/// The indexed point setters and movers, setPoint, setPointPreservingClosure, setXYZ, setXYZPreservingClosure, movePoint, moveXYZ, movePointPreservingClosure and moveXYZPreservingClosure,
/// and the append functions addPoint and addXYZ mutate in place and return unit, as instance and as static members.
/// Taking a point index alone does not mean mutation: getPoint, getX, getY, getZ and getLine only read the Polyline3D.
/// XYZs and xyzs expose the live coordinate buffer, changes to it change the Polyline3D.
// [<Struct>]
[<NoEquality; NoComparison>] // because its made up from floats
[<DataContract>] // for using DataMember on fields, for Newtonsoft.Json
#if !FABLE_COMPILER
[<JsonConverter(typeof<Polyline3DJsonConverter>)>]
#endif
type Polyline3D private (xyzs: ResizeArray<float>) =

    // /// Create a new empty Polyline3D
    // new () = Polyline3D(ResizeArray<float>()) // do not allow this, maybe it wil be a struct in the future, and then this constructor would set _XYZs to null, not empty Array.

    /// Create a new empty Polyline3D with predefined point count capacity.
    new (capacity:int) =
        Polyline3D(ResizeArray<float>(capacity * 3))


    /// Gets the X, Y and Z interleaved ResizeArray of the Polyline3D: x0, y0, z0, x1, y1, z1, ...
    /// This is the live internal buffer, so changes to the list will be reflected in the Polyline3D.
    [<DataMember>]
    member _.XYZs : ResizeArray<float> =
        xyzs

    /// Gets the X, Y and Z interleaved ResizeArray of the Polyline3D: x0, y0, z0, x1, y1, z1, ...
    /// This is the live internal buffer, so changes to the list will be reflected in the Polyline3D.
    static member inline xyzs (p:Polyline3D) : ResizeArray<float> =
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

    /// Gets the x coordinate of the 3D point at the given pointIndex in the list of points.
    /// (this gets XYZs.[pointIndex * 3] from the internal flat list of x, y, and z coordinates )
    member _.GetX (pointIndex:int) : float =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "GetX" pointIndex xyzs
        xyzs.[pointIndex * 3]

    /// Gets the x coordinate of the 3D point at the given pointIndex in the list of points.
    /// (this gets XYZs.[pointIndex * 3] from the internal flat list of x, y, and z coordinates )
    static member inline getX (pointIndex:int) (p:Polyline3D) : float =
        p.GetX pointIndex

    /// Gets the y coordinate of the 3D point at the given pointIndex in the list of points.
    /// (this gets XYZs.[pointIndex * 3 + 1] from the internal flat list of x, y, and z coordinates )
    member _.GetY (pointIndex:int) : float =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "GetY" pointIndex xyzs
        xyzs.[pointIndex * 3 + 1]

    /// Gets the y coordinate of the 3D point at the given pointIndex in the list of points.
    /// (this gets XYZs.[pointIndex * 3 + 1] from the internal flat list of x, y, and z coordinates )
    static member inline getY(pointIndex:int) (p:Polyline3D) : float =
        p.GetY pointIndex


    /// Gets the z coordinate of the 3D point at the given pointIndex in the list of points.
    /// (this gets XYZs.[pointIndex * 3 + 2] from the internal flat list of x, y, and z coordinates )
    member _.GetZ (pointIndex:int) : float =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "GetZ" pointIndex xyzs
        xyzs.[pointIndex * 3 + 2]

    /// Gets the z coordinate of the 3D point at the given pointIndex in the list of points.
    /// (this gets XYZs.[pointIndex * 3 + 2] from the internal flat list of x, y, and z coordinates )
    static member inline getZ (pointIndex:int) (p:Polyline3D) : float =
        p.GetZ pointIndex


    /// Gets the 3D point at the given pointIndex in the list of points.
    /// (this gets Pnt(XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], XYZs.[pointIndex * 3 + 2]) from the internal flat list of x, y, and z coordinates )
    member p.GetPoint (pointIndex:int) : Pnt =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "GetPoint" pointIndex xyzs
        getPt pointIndex xyzs

    /// Gets the 3D point at the given pointIndex in the list of points.
    /// (this gets Pnt(XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], XYZs.[pointIndex * 3 + 2]) from the internal flat list of x, y, and z coordinates )
    static member inline getPoint (pointIndex:int) (p:Polyline3D) : Pnt =
        p.GetPoint pointIndex


    /// Sets the 3D point at the given pointIndex in the list of points.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )
    member _.SetPoint (pointIndex:int,pnt:Pnt) : unit =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "SetPoint" pointIndex xyzs
        setCoordXYZ pointIndex pnt.X pnt.Y pnt.Z xyzs

    /// Sets the 3D point at the given pointIndex in the list of points.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )
    static member inline setPoint (pointIndex:int) (pnt:Pnt) (p:Polyline3D) : unit =
        p.SetPoint (pointIndex, pnt)

    /// Sets the 3D point at the given pointIndex in the list of points.
    /// On a closed Polyline3D, setting the first or last point will set both to the same point.
    /// Raises an error if the pointIndex is out of range.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )
    member p.SetPointPreservingClosure (pointIndex:int, pnt:Pnt) : unit =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "SetPointPreservingClosure" pointIndex xyzs
        let wasClosed = p.IsClosed
        if wasClosed && pointIndex = 0 then
            setCoordXYZ (p.PointCount-1) pnt.X pnt.Y pnt.Z xyzs
        elif wasClosed && pointIndex = p.PointCount-1 then
            setCoordXYZ 0 pnt.X pnt.Y pnt.Z xyzs
        setCoordXYZ pointIndex pnt.X pnt.Y pnt.Z xyzs

    /// Sets the 3D point at the given pointIndex in the list of points.
    /// On a closed Polyline3D, setting the first or last point will set both to the same point.
    /// Raises an error if the pointIndex is out of range.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )
    static member inline setPointPreservingClosure (pointIndex:int) (pnt:Pnt) (p:Polyline3D) : unit =
        p.SetPointPreservingClosure (pointIndex, pnt)

    /// Sets the x, y, and z coordinates of the 3D point at the given pointIndex in the list of points.
    /// On a closed Polyline3D, setting the first or last point will set both to the same point.
    /// Raises an error if the pointIndex is out of range.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )
    member p.SetXYZPreservingClosure (pointIndex:int, x:float, y:float, z:float): unit =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "SetXYZPreservingClosure" pointIndex xyzs
        let wasClosed = p.IsClosed
        if wasClosed && pointIndex = 0 then
            setCoordXYZ (p.PointCount-1) x y z xyzs
        elif wasClosed && pointIndex = p.PointCount-1 then
            setCoordXYZ 0 x y z xyzs
        setCoordXYZ pointIndex x y z xyzs

    /// Sets the x, y, and z coordinates of the 3D point at the given pointIndex in the list of points.
    /// On a closed Polyline3D, setting the first or last point will set both to the same point.
    /// Raises an error if the pointIndex is out of range.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )
    static member inline setXYZPreservingClosure (pointIndex:int) x y z (p:Polyline3D) : unit =
        p.SetXYZPreservingClosure (pointIndex, x, y, z)

    /// Sets the x, y, and z coordinates of the 3D point at the given pointIndex in the list of points.
    /// NOTE: setting the first or last point on a closed Polyline3D might open it.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )
    member p.SetXYZ (pointIndex:int, x:float, y:float, z:float) : unit =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "SetXYZ" pointIndex xyzs
        setCoordXYZ pointIndex x y z xyzs

    /// Sets the x, y, and z coordinates of the 3D point at the given pointIndex in the list of points.
    /// NOTE: setting the first or last point on a closed Polyline3D might open it.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )
    static member inline setXYZ (pointIndex:int) x y z (p:Polyline3D) : unit =
        p.SetXYZ (pointIndex, x, y, z)

    /// <summary>Moves the 3D point at the given pointIndex in place by a vector.
    /// NOTE: moving the first or last point on a closed Polyline3D opens it. Use MovePointPreservingClosure to keep it closed.
    /// (this adds v.X, v.Y and v.Z to XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1] and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )</summary>
    /// <param name="pointIndex">The index of the point to move. Must be between 0 and PointCount - 1.</param>
    /// <param name="v">The vector to move the point by.</param>
    member _.MovePoint (pointIndex:int, v:Vec) : unit =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "MovePoint" pointIndex xyzs
        moveCoordXYZ pointIndex v.X v.Y v.Z xyzs

    /// <summary>Moves the 3D point at the given pointIndex in place by a vector.
    /// NOTE: moving the first or last point on a closed Polyline3D opens it. Use Polyline3D.movePointPreservingClosure to keep it closed.</summary>
    /// <param name="pointIndex">The index of the point to move. Must be between 0 and PointCount - 1.</param>
    /// <param name="v">The vector to move the point by.</param>
    /// <param name="p">The Polyline3D to change.</param>
    static member inline movePoint (pointIndex:int) (v:Vec) (p:Polyline3D) : unit =
        p.MovePoint (pointIndex, v)

    /// <summary>Moves the 3D point at the given pointIndex in place by the given x, y, and z distances.
    /// NOTE: moving the first or last point on a closed Polyline3D opens it. Use MoveXYZPreservingClosure to keep it closed.
    /// (this adds x, y and z to XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1] and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )</summary>
    /// <param name="pointIndex">The index of the point to move. Must be between 0 and PointCount - 1.</param>
    /// <param name="x">The distance to move the point in X direction.</param>
    /// <param name="y">The distance to move the point in Y direction.</param>
    /// <param name="z">The distance to move the point in Z direction.</param>
    member _.MoveXYZ (pointIndex:int, x:float, y:float, z:float) : unit =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "MoveXYZ" pointIndex xyzs
        moveCoordXYZ pointIndex x y z xyzs

    /// <summary>Moves the 3D point at the given pointIndex in place by the given x, y, and z distances.
    /// NOTE: moving the first or last point on a closed Polyline3D opens it. Use Polyline3D.moveXYZPreservingClosure to keep it closed.</summary>
    /// <param name="pointIndex">The index of the point to move. Must be between 0 and PointCount - 1.</param>
    /// <param name="x">The distance to move the point in X direction.</param>
    /// <param name="y">The distance to move the point in Y direction.</param>
    /// <param name="z">The distance to move the point in Z direction.</param>
    /// <param name="p">The Polyline3D to change.</param>
    static member inline moveXYZ (pointIndex:int) (x:float) (y:float) (z:float) (p:Polyline3D) : unit =
        p.MoveXYZ (pointIndex, x, y, z)

    /// <summary>Moves the 3D point at the given pointIndex in place by a vector.
    /// On a closed Polyline3D, moving the first or last point moves both, so it stays closed.
    /// (this adds v.X, v.Y and v.Z to XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1] and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )</summary>
    /// <param name="pointIndex">The index of the point to move. Must be between 0 and PointCount - 1.</param>
    /// <param name="v">The vector to move the point by.</param>
    member p.MovePointPreservingClosure (pointIndex:int, v:Vec) : unit =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "MovePointPreservingClosure" pointIndex xyzs
        let wasClosed = p.IsClosed
        moveCoordXYZ pointIndex v.X v.Y v.Z xyzs
        if wasClosed then
            let last = p.PointCount - 1
            if pointIndex = 0 then
                setCoordXYZ last xyzs.[0] xyzs.[1] xyzs.[2] xyzs
            elif pointIndex = last then
                setCoordXYZ 0 xyzs.[last * 3] xyzs.[last * 3 + 1] xyzs.[last * 3 + 2] xyzs

    /// <summary>Moves the 3D point at the given pointIndex in place by a vector.
    /// On a closed Polyline3D, moving the first or last point moves both, so it stays closed.</summary>
    /// <param name="pointIndex">The index of the point to move. Must be between 0 and PointCount - 1.</param>
    /// <param name="v">The vector to move the point by.</param>
    /// <param name="p">The Polyline3D to change.</param>
    static member inline movePointPreservingClosure (pointIndex:int) (v:Vec) (p:Polyline3D) : unit =
        p.MovePointPreservingClosure (pointIndex, v)

    /// <summary>Moves the 3D point at the given pointIndex in place by the given x, y, and z distances.
    /// On a closed Polyline3D, moving the first or last point moves both, so it stays closed.
    /// (this adds x, y and z to XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1] and XYZs.[pointIndex * 3 + 2] in the internal flat list of x, y, and z coordinates )</summary>
    /// <param name="pointIndex">The index of the point to move. Must be between 0 and PointCount - 1.</param>
    /// <param name="x">The distance to move the point in X direction.</param>
    /// <param name="y">The distance to move the point in Y direction.</param>
    /// <param name="z">The distance to move the point in Z direction.</param>
    member p.MoveXYZPreservingClosure (pointIndex:int, x:float, y:float, z:float) : unit =
        let len = xyzs.Count
        if pointIndex < 0 || pointIndex > len / 3 - 1 then
            failPointIndex "MoveXYZPreservingClosure" pointIndex xyzs
        let wasClosed = p.IsClosed
        moveCoordXYZ pointIndex x y z xyzs
        if wasClosed then
            let last = p.PointCount - 1
            if pointIndex = 0 then
                setCoordXYZ last xyzs.[0] xyzs.[1] xyzs.[2] xyzs
            elif pointIndex = last then
                setCoordXYZ 0 xyzs.[last * 3] xyzs.[last * 3 + 1] xyzs.[last * 3 + 2] xyzs

    /// <summary>Moves the 3D point at the given pointIndex in place by the given x, y, and z distances.
    /// On a closed Polyline3D, moving the first or last point moves both, so it stays closed.</summary>
    /// <param name="pointIndex">The index of the point to move. Must be between 0 and PointCount - 1.</param>
    /// <param name="x">The distance to move the point in X direction.</param>
    /// <param name="y">The distance to move the point in Y direction.</param>
    /// <param name="z">The distance to move the point in Z direction.</param>
    /// <param name="p">The Polyline3D to change.</param>
    static member inline moveXYZPreservingClosure (pointIndex:int) (x:float) (y:float) (z:float) (p:Polyline3D) : unit =
        p.MoveXYZPreservingClosure (pointIndex, x, y, z)

    /// Adds a point from x, y, and z coordinates.
    member _.AddXYZ (x:float, y:float, z:float) : unit =
        appendXYZ x y z xyzs

    /// Adds a point from x, y, and z coordinates.
    static member inline addXYZ (x:float) (y:float) (z:float) (p:Polyline3D) : unit =
        p.AddXYZ (x, y, z)

    /// Adds a point to the end of the Polyline3D.
    member p.AddPoint (pnt:Pnt) : unit =
        p.AddXYZ (pnt.X, pnt.Y, pnt.Z)

    /// Adds a point to the end of the Polyline3D.
    static member inline addPoint (pnt:Pnt) (p:Polyline3D) : unit =
        p.AddPoint pnt

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

    /// Format a 3D polyline into an F# code string that can be used to recreate the polyline.
    member p.AsFSharpCode : string =
        let ptsAsCode =
            p.AsPoints
            |> ResizeArr.map _.AsFSharpCode
            |> String.concat "; "
        $"Polyline3D.create [| {ptsAsCode} |]"

    /// Format a 3D polyline into an F# code string that can be used to recreate the polyline.
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

    /// Gets or sets the first point of the Polyline3D.
    /// This is the point at index 0.
    /// Same as Polyline3D.FirstPoint
    /// Setting it mutates the Polyline3D in place and moves only this one point, so a closed Polyline3D gets opened. Use SetXYZPreservingClosure or MovePointPreservingClosure to keep it closed.
    member p.Start
        with get() : Pnt =
            if p.PointCount < 1 then failTooFewPoly3D "Start.get" 1 p.PointCount
            getPt 0 xyzs
        and set(pnt:Pnt) : unit =
            if p.PointCount < 1 then failTooFewPoly3D "Start.set" 1 p.PointCount
            xyzs.[0] <- pnt.X
            xyzs.[1] <- pnt.Y
            xyzs.[2] <- pnt.Z

    /// Gets the first point of the Polyline3D.
    /// This is the point at index 0.
    /// Same as Polyline3D.FirstPoint
    static member inline start (pl:Polyline3D) : Pnt =
        pl.Start

    /// Gets or sets the last or end point of the Polyline3D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.LastPoint
    /// Setting it mutates the Polyline3D in place and moves only this one point, so a closed Polyline3D gets opened. Use SetXYZPreservingClosure or MovePointPreservingClosure to keep it closed.
    member p.End
        with get() : Pnt =
            if p.PointCount < 1 then failTooFewPoly3D "End.get" 1 p.PointCount
            let c = xyzs.Count
            Pnt(xyzs.[c - 3], xyzs.[c - 2], xyzs.[c - 1])
        and set(pnt:Pnt) : unit =
            if p.PointCount < 1 then failTooFewPoly3D "End.set" 1 p.PointCount
            let c = xyzs.Count
            xyzs.[c - 3] <- pnt.X
            xyzs.[c - 2] <- pnt.Y
            xyzs.[c - 1] <- pnt.Z

    /// Gets the last or end point of the Polyline3D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.LastPoint
    static member inline end' (pl:Polyline3D) : Pnt =
        pl.End

    /// Gets or sets the last point of the Polyline3D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.End
    /// Setting it mutates the Polyline3D in place and moves only this one point, so a closed Polyline3D gets opened. Use SetXYZPreservingClosure or MovePointPreservingClosure to keep it closed.
    member p.LastPoint
        with get() : Pnt =
            if p.PointCount < 1 then failTooFewPoly3D "LastPoint.get" 1 p.PointCount
            let c = xyzs.Count
            Pnt(xyzs.[c - 3], xyzs.[c - 2], xyzs.[c - 1])
        and set(pnt:Pnt) : unit =
            if p.PointCount < 1 then failTooFewPoly3D "LastPoint.set" 1 p.PointCount
            let c = xyzs.Count
            xyzs.[c - 3] <- pnt.X
            xyzs.[c - 2] <- pnt.Y
            xyzs.[c - 1] <- pnt.Z

    /// Gets the last point of the Polyline3D.
    /// This is the point at index Points.Count - 1.
    /// Same as Polyline3D.End
    static member inline lastPoint (pl:Polyline3D) : Pnt =
        pl.LastPoint

    /// Gets or sets the second last point of the Polyline3D.
    /// Setting it mutates the Polyline3D in place.
    member p.SecondLastPoint
        with get() : Pnt =
            if p.PointCount < 2 then failTooFewPoly3D "SecondLastPoint.get" 2 p.PointCount
            let c = xyzs.Count
            Pnt(xyzs.[c - 6], xyzs.[c - 5], xyzs.[c - 4])
        and set(pnt:Pnt) : unit =
            if p.PointCount < 2 then failTooFewPoly3D "SecondLastPoint.set" 2 p.PointCount
            let c = xyzs.Count
            xyzs.[c - 6] <- pnt.X
            xyzs.[c - 5] <- pnt.Y
            xyzs.[c - 4] <- pnt.Z

    /// Gets the second last point of the Polyline3D.
    static member inline secondLastPoint (pl:Polyline3D) : Pnt =
        pl.SecondLastPoint

    /// Gets or sets the second point of the Polyline3D.
    /// This is the point at index 1.
    /// Setting it mutates the Polyline3D in place.
    member p.SecondPoint
        with get() : Pnt =
            if p.PointCount < 2 then failTooFewPoly3D "SecondPoint.get" 2 p.PointCount
            Pnt(xyzs.[3], xyzs.[4], xyzs.[5])
        and set(pnt:Pnt) : unit =
            if p.PointCount < 2 then failTooFewPoly3D "SecondPoint.set" 2 p.PointCount
            xyzs.[3] <- pnt.X
            xyzs.[4] <- pnt.Y
            xyzs.[5] <- pnt.Z

    /// Gets the second point of the Polyline3D.
    /// This is the point at index 1.
    static member inline secondPoint (pl:Polyline3D) : Pnt =
        pl.SecondPoint

    /// Gets or sets the first point of the Polyline3D.
    /// This is the point at index 0.
    /// Same as Polyline3D.Start
    /// Setting it mutates the Polyline3D in place and moves only this one point, so a closed Polyline3D gets opened. Use SetXYZPreservingClosure or MovePointPreservingClosure to keep it closed.
    member p.FirstPoint
        with get() : Pnt =
            if p.PointCount < 1 then failTooFewPoly3D "FirstPoint.get" 1 p.PointCount
            Pnt(xyzs.[0], xyzs.[1], xyzs.[2])
        and set(pnt:Pnt) : unit =
            if p.PointCount < 1 then failTooFewPoly3D "FirstPoint.set" 1 p.PointCount
            xyzs.[0] <- pnt.X
            xyzs.[1] <- pnt.Y
            xyzs.[2] <- pnt.Z

    /// Gets the first point of the Polyline3D.
    /// This is the point at index 0.
    /// Same as Polyline3D.Start
    static member inline firstPoint (pl:Polyline3D) : Pnt =
        pl.FirstPoint

    /// Gets the number of points in the Polyline3D.
    member p.PointCount : int =
        countPts xyzs

    /// Gets the number of points in the Polyline3D.
    static member inline pointCount (p:Polyline3D) : int =
        p.PointCount

    /// Gets the number of segments in the Polyline3D.
    /// This is the point count minus one, but never negative.
    member p.SegmentCount : int =
        max 0 (p.PointCount - 1 )

    /// Gets the number of segments in the Polyline3D.
    /// This is the point count minus one, but never negative.
    static member inline segmentCount (p:Polyline3D) : int =
        p.SegmentCount

    /// Gets the length of the Polyline3D.
    /// The sum of the lengths of all segments.
    /// Returns 0.0 if there are fewer than 2 points.
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
    /// Returns 0.0 if there are fewer than 2 points.
    static member inline length (p:Polyline3D) : float =
        p.Length

    /// Gets the segment at index i of the Polyline3D.
    member p.GetSegment(i:int) : Line3D =
        if i < 0 || i > p.PointCount - 2 then
            fail $"Polyline3D.GetSegment: index {i} is out of range for Polyline3D with {p.PointCount} points."
        let j = i * 3
        Line3D(xyzs.[j], xyzs.[j + 1], xyzs.[j + 2], xyzs.[j + 3], xyzs.[j + 4], xyzs.[j + 5])

    /// Gets the segment at index i of the Polyline3D.
    static member inline getSegment (i:int) (pl:Polyline3D) : Line3D =
        pl.GetSegment i

    /// Gets the last segment of the Polyline3D.
    member p.LastSegment : Line3D =
        if p.PointCount < 2 then failTooFewPoly3D "LastSegment" 2 p.PointCount
        let i = xyzs.Count - 6
        Line3D(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2], xyzs.[i + 3], xyzs.[i + 4], xyzs.[i + 5])

    /// Gets the last segment of the Polyline3D.
    static member inline lastSegment (pl:Polyline3D) : Line3D =
        pl.LastSegment

    /// Gets the first segment of the Polyline3D.
    member p.FirstSegment : Line3D =
        if p.PointCount < 2 then failTooFewPoly3D "FirstSegment" 2 p.PointCount
        Line3D(xyzs.[0], xyzs.[1], xyzs.[2], xyzs.[3], xyzs.[4], xyzs.[5])

    /// Gets the first segment of the Polyline3D.
    static member inline firstSegment (pl:Polyline3D) : Line3D =
        pl.FirstSegment

    /// Returns all segments of the Polyline3D as a list of Line3D.
    member p.Segments : ResizeArray<Line3D> =
        let lns = ResizeArray(p.SegmentCount)
        if p.PointCount < 2 then
            lns
        else
            let mutable i = 0
            while i < xyzs.Count - 3 do
                lns.Add(Line3D(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2], xyzs.[i + 3], xyzs.[i + 4], xyzs.[i + 5]))
                i <- i + 3
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
    /// Returns FALSE if the Polyline3D has fewer than 3 points.
    member p.IsClosed : bool =
        p.PointCount > 2
        && (let c = xyzs.Count
            xyzs.[0] = xyzs.[c - 3]
            && xyzs.[1] = xyzs.[c - 2]
            && xyzs.[2] = xyzs.[c - 1])

    /// Tests if Polyline3D start and end points are exactly the same.
    /// Returns FALSE if the Polyline3D has fewer than 3 points.
    static member inline isClosed (pl:Polyline3D) : bool =
        pl.IsClosed

    /// Tests if Polyline3D is closed within given tolerance.
    /// Returns FALSE if the Polyline3D has fewer than 3 points.
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
    /// Returns FALSE if the Polyline3D has fewer than 3 points.
    static member inline isAlmostClosed tolerance (pl:Polyline3D) : bool =
        pl.IsAlmostClosed tolerance

    /// Reverses the order of the Polyline3D in place.
    member p.Reverse() : unit =
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

    /// Returns a new Polyline3D with the order of the points reversed.
    /// The input Polyline3D is not changed. Use p.Reverse() to reverse in place.
    static member reverse (p:Polyline3D) : Polyline3D =
        let res = p.Duplicate()
        res.Reverse()
        res

    /// <summary>Closes the Polyline3D in place if it is not already closed.
    /// If the ends are closer than the tolerance, the last point is set equal to the first point.
    /// Otherwise the start point is added to the end of the Polyline3D.
    /// Raises an error if the Polyline3D has fewer than 3 points.</summary>
    /// <param name="toleranceForAddingPoint">Optional. 1e-6 by default
    /// The tolerance used to decide whether to snap the last point to the first point.</param>
    member p.Close([<OPT; DEF(1e-6)>]toleranceForAddingPoint:float) : unit =
        if p.PointCount < 3 then failTooFewPoly3D "Close" 3 p.PointCount
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

    /// <summary>Returns a new closed Polyline3D. The input Polyline3D is not changed.
    /// If the ends are closer than the tolerance, the last point is set equal to the first point.
    /// Otherwise the start point is added to the end of the Polyline3D.
    /// Raises an error if the Polyline3D has fewer than 3 points.</summary>
    /// <param name="toleranceForAddingPoint">The tolerance used to decide whether to snap the last point to the first point.</param>
    /// <param name="pl">The Polyline3D to close.</param>
    /// <returns>A new closed Polyline3D. Use p.Close to close in place.</returns>
    static member close (toleranceForAddingPoint:float) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.Close toleranceForAddingPoint
        res

    /// Calculates the signed area of the Polyline3D when projected in 2D.
    /// Z values are ignored.
    /// If it is positive the Polyline3D is counter-clockwise.
    /// Polyline does not need to be exactly closed.
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self-intersecting Polylines the result is invalid.
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
    /// If it is positive the Polyline3D is counter-clockwise.
    /// Polyline does not need to be exactly closed.
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self-intersecting Polylines the result is invalid.
    /// Raises an error on an empty Polyline3D.
    static member inline signedAreaIn2D (pl:Polyline3D) : float =
        pl.SignedAreaIn2D

    /// Calculates the area of the Polyline3D when projected in 2D.
    /// Z values are ignored.
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self-intersecting Polylines the result is invalid.
    member p.AreaIn2D : float =
        abs p.SignedAreaIn2D

    /// Calculates the area of the Polyline3D when projected in 2D.
    /// Z values are ignored.
    /// The segment from the last point to the first point is included in the area calculation.
    /// For self-intersecting Polylines the result is invalid.
    static member inline areaIn2D (pl:Polyline3D) : float =
        abs pl.SignedAreaIn2D

    /// Test if Polyline3D is counter-clockwise when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is counter-clockwise.
    member p.IsCounterClockwiseIn2D : bool =
        let  area = p.SignedAreaIn2D
        if   abs(area) < UtilEuclid.zeroLengthTolerance then
            fail $"Polyline3D.IsCounterClockwiseIn2D: Polyline3D the area is zero: {p}"
        area > 0.0

    /// Test if Polyline3D is counter-clockwise when projected in 2D.
    /// Z values are ignored.
    /// The Polyline3D does not need to be actually closed.
    /// The signed area of the Polyline3D is calculated.
    /// If it is positive the Polyline3D is counter-clockwise.
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
    /// If the parameter is within 1e-6 of an integer value, the integer value is used as parameter.
    static member evaluateAt (t:float) (pl:Polyline3D) : Pnt =
        pl.EvaluateAt t

    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    member pl.ClosestParameterXYZ(x:float, y:float, z:float) : float =
        if pl.PointCount = 0 then  fail "Polyline3D.ClosestParameter failed on empty Polyline3D"
        let px = x
        let py = y
        let pz = z
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
    member pl.ClosestParameter(p:Pnt) : float =
        pl.ClosestParameterXYZ(p.X, p.Y, p.Z)

    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    static member inline closestParameterXYZ (x:float) (y:float) (z:float) (pl:Polyline3D) : float =
        pl.ClosestParameterXYZ (x, y, z)

    /// Returns the parameter on the Polyline3D that is the closest point to the given point.
    /// The integer part of the parameter is the index of the segment that the point is on.
    /// The fractional part of the parameter is the parameter from 0.0 to 1.0 on the segment.
    /// The domain Polyline3D starts at 0.0 and ends at points.Count - 1.0 .
    static member inline closestParameter (pnt:Pnt) (pl:Polyline3D) : float =
        pl.ClosestParameter pnt

    /// Returns the point on the Polyline3D that is the closest point to the given point.
    /// This might be a point on a segment or a vertex of the Polyline3D.
    member pl.ClosestPoint(p:Pnt) : Pnt =
        if pl.PointCount = 0 then  fail "Polyline3D.ClosestPoint failed on empty Polyline3D"
        let px = p.X
        let py = p.Y
        let pz = p.Z
        let mutable ax = xyzs.[0]
        let mutable ay = xyzs.[1]
        let mutable az = xyzs.[2]
        // the closest point found so far, starting with the first point, this handles the case of a single point Polyline3D
        let mutable minX = ax
        let mutable minY = ay
        let mutable minZ = az
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
                    minX <- projX
                    minY <- projY
                    minZ <- projZ
            else
                let dpx = px - ax
                let dpy = py - ay
                let dpz = pz - az
                let distSq = dpx * dpx + dpy * dpy + dpz * dpz
                if distSq < minDistSq then
                    minDistSq <- distSq
                    minX <- ax
                    minY <- ay
                    minZ <- az
            ax <- bx
            ay <- by
            az <- bz
            i <- i + 3
        Pnt(minX, minY, minZ)

    /// Returns the point on the Polyline3D that is the closest point to the given point.
    /// This might be a point on a segment or a vertex of the Polyline3D.
    static member inline closestPoint (pnt:Pnt) (pl:Polyline3D) : Pnt =
        pl.ClosestPoint pnt

    /// Returns the index into the Polyline3D's point list of the point that is closest to the given point.
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
    static member inline closestPointIndex (pnt:Pnt) (pl:Polyline3D) : int =
        pl.ClosestPointIndex pnt

    /// Returns the distance of the test point to the closest point on the Polyline3D.
    member pl.DistanceToXYZ(x:float, y:float, z:float) : float =
        if pl.PointCount = 0 then  fail "Polyline3D.DistanceTo failed on empty Polyline3D"
        let px = x
        let py = y
        let pz = z
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
    member pl.DistanceTo(p:Pnt) : float =
        pl.DistanceToXYZ(p.X, p.Y, p.Z)

    /// Returns the distance of the test point to the closest point on the Polyline3D.
    static member inline distanceToXYZ (x:float) (y:float) (z:float) (pl:Polyline3D) : float =
        pl.DistanceToXYZ (x, y, z)

    /// Returns the distance of the test point to the closest point on the Polyline3D.
    static member inline distanceTo (pnt:Pnt) (pl:Polyline3D) : float =
        pl.DistanceTo pnt

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
    /// The returned vector is not unitized, its length is twice the area of the polygon projected onto that plane.
    /// Fails if the summed normal is shorter than 1e-6, because then no normal can be found. This happens if:
    /// the Polyline3D has less than 3 points,
    /// or all points are in one line,
    /// or the cross products cancel each other out on a self intersecting shape,
    /// or the Polyline3D is very small.
    /// Use TryAverageNormal to get ValueNone instead of an exception.
    /// For the last two cases Polyline3D.AveragePlane still finds a plane.
    member pl.AverageNormal : Vec =
        if pl.PointCount < 3 then failTooFewPoly3D "AverageNormal" 3 pl.PointCount
        let n = averageNormalOf xyzs
        if isTooSmallSq n.LengthSq then
            fail $"Polyline3D.AverageNormal: no normal can be found. The points are in one line, or the cross products cancel each other out, or the Polyline3D is very small: {pl.AsString}"
        n

    /// Returns the average normal vector of the Polyline3D.
    /// It is calculated by summing up the cross products of all segments around the center point.
    /// The returned vector is not unitized, its length is twice the area of the polygon projected onto that plane.
    /// Fails if the summed normal is shorter than 1e-6, because then no normal can be found. This happens if:
    /// the Polyline3D has less than 3 points,
    /// or all points are in one line,
    /// or the cross products cancel each other out on a self intersecting shape,
    /// or the Polyline3D is very small.
    /// Use tryAverageNormal to get ValueNone instead of an exception.
    /// For the last two cases Polyline3D.averagePlane still finds a plane.
    static member inline averageNormal (pl:Polyline3D) : Vec =
        pl.AverageNormal

    /// Returns the average normal vector of the Polyline3D, or ValueNone if it is degenerate.
    /// It is calculated by summing up the cross products of all segments around the center point,
    /// so it is the same vector as Polyline3D.AverageNormal, but bad input returns ValueNone instead of raising an exception.
    /// The returned vector is not unitized, its length is twice the area of the polygon projected onto that plane.
    /// Returns ValueNone if the summed normal is shorter than 1e-6. This happens if:
    /// the Polyline3D has less than 3 points,
    /// or all points are in one line,
    /// or the cross products cancel each other out on a self intersecting shape,
    /// or the Polyline3D is very small.
    /// For these last two cases Polyline3D.TryAveragePlane still finds a plane.
    member pl.TryAverageNormal : Vec voption =
        if pl.PointCount < 3 then
            ValueNone
        else
            let n = averageNormalOf xyzs
            if isTooSmallSq n.LengthSq then ValueNone
            else ValueSome n

    /// Returns the average normal vector of the Polyline3D, or ValueNone if it is degenerate.
    /// It is calculated by summing up the cross products of all segments around the center point,
    /// so it is the same vector as Polyline3D.averageNormal, but bad input returns ValueNone instead of raising an exception.
    /// The returned vector is not unitized, its length is twice the area of the polygon projected onto that plane.
    /// Returns ValueNone if the summed normal is shorter than 1e-6. This happens if:
    /// the Polyline3D has less than 3 points,
    /// or all points are in one line,
    /// or the cross products cancel each other out on a self intersecting shape,
    /// or the Polyline3D is very small.
    /// For these last two cases Polyline3D.tryAveragePlane still finds a plane.
    static member inline tryAverageNormal (pl:Polyline3D) : Vec voption =
        pl.TryAverageNormal

    /// Returns the average plane of the Polyline3D, even if the points are not planar.
    /// As opposed to Polyline3D.TryPlane this does not check if all points are on that plane.
    /// The origin of the returned NPlane is the average of all points, see Polyline3D.Center.
    /// The normal is the unitized Polyline3D.AverageNormal, so it follows the order of the points.
    /// If they are counter-clockwise in the World X-Y plane, then the normal points in World Z direction.
    /// On self intersecting shapes, where these cross products cancel each other out, the orientation is arbitrary.
    /// The optional 'tolerance' is the maximum distance for considering all points to be on one point or on one line,
    /// it defaults to 1e-6.
    /// Fails if the Polyline3D has less than 3 points,
    /// or if all points are within the tolerance of one point or of one line, because then there is no unique plane.
    member p.AveragePlane([<OPT;DEF(1e-6)>] tolerance:float) : NPlane =
        match tryPlaneOf false tolerance xyzs with
        | ValueSome pln -> pln
        | ValueNone ->
            if p.PointCount < 3 then failTooFewPoly3D "AveragePlane" 3 p.PointCount
            else fail $"Polyline3D.AveragePlane: all points are within the tolerance of {tolerance} of one point or of one line, so there is no unique plane: {p.AsString}"

    /// Returns the average plane of the Polyline3D, even if the points are not planar.
    /// As opposed to Polyline3D.tryPlane this does not check if all points are on that plane.
    /// The origin of the returned NPlane is the average of all points, see Polyline3D.center.
    /// The normal is the unitized Polyline3D.averageNormal, so it follows the order of the points.
    /// If they are counter-clockwise in the World X-Y plane, then the normal points in World Z direction.
    /// On self intersecting shapes, where these cross products cancel each other out, the orientation is arbitrary.
    /// The 'tolerance' is the maximum distance for considering all points to be on one point or on one line.
    /// Use 1e-6 as a default.
    /// Fails if the Polyline3D has less than 3 points,
    /// or if all points are within the tolerance of one point or of one line, because then there is no unique plane.
    static member inline averagePlane (tolerance:float) (pl:Polyline3D) : NPlane =
        pl.AveragePlane(tolerance)

    /// Tries to find the average plane of the Polyline3D, even if the points are not planar.
    /// As opposed to Polyline3D.TryPlane this does not check if all points are on that plane.
    /// The origin of the returned NPlane is the average of all points, see Polyline3D.Center.
    /// The normal is the unitized Polyline3D.AverageNormal, so it follows the order of the points.
    /// If they are counter-clockwise in the World X-Y plane, then the normal points in World Z direction.
    /// On self intersecting shapes, where these cross products cancel each other out, the orientation is arbitrary.
    /// The optional 'tolerance' is the maximum distance for considering all points to be on one point or on one line,
    /// it defaults to 1e-6.
    /// Returns ValueNone if the Polyline3D has less than 3 points,
    /// or if all points are within the tolerance of one point or of one line, because then there is no unique plane.
    member _.TryAveragePlane([<OPT;DEF(1e-6)>] tolerance:float) : NPlane voption =
        tryPlaneOf false tolerance xyzs

    /// Tries to find the average plane of the Polyline3D, even if the points are not planar.
    /// As opposed to Polyline3D.tryPlane this does not check if all points are on that plane.
    /// The origin of the returned NPlane is the average of all points, see Polyline3D.center.
    /// The normal is the unitized Polyline3D.averageNormal, so it follows the order of the points.
    /// If they are counter-clockwise in the World X-Y plane, then the normal points in World Z direction.
    /// On self intersecting shapes, where these cross products cancel each other out, the orientation is arbitrary.
    /// The 'tolerance' is the maximum distance for considering all points to be on one point or on one line.
    /// Use 1e-6 as a default.
    /// Returns ValueNone if the Polyline3D has less than 3 points,
    /// or if all points are within the tolerance of one point or of one line, because then there is no unique plane.
    static member inline tryAveragePlane (tolerance:float) (pl:Polyline3D) : NPlane voption =
        pl.TryAveragePlane(tolerance)

    /// Tries to find the plane that all points of the Polyline3D lie in.
    /// The optional 'tolerance' is the maximum distance any point may have from the plane, it defaults to 1e-6.
    /// The origin of the returned NPlane is the average of all points, see Polyline3D.Center.
    /// The orientation of the normal follows the order of the points.
    /// If they are counter-clockwise in the World X-Y plane, then the normal points in World Z direction.
    /// On self intersecting shapes, where these cross products cancel each other out, the orientation is arbitrary.
    /// Returns ValueNone if:
    /// the Polyline3D has less than 3 points,
    /// or all points are within the tolerance of one point or of one line, so that there is no unique plane,
    /// or any point is further away from the plane than the tolerance.
    /// Use Polyline3D.TryAveragePlane to get the average plane of non planar points too.
    member _.TryPlane([<OPT;DEF(1e-6)>] tolerance:float) : NPlane voption =
        tryPlaneOf true tolerance xyzs

    /// Tries to find the plane that all points of the Polyline3D lie in.
    /// The 'tolerance' is the maximum distance any point may have from the plane. Use 1e-6 as a default.
    /// The origin of the returned NPlane is the average of all points, see Polyline3D.Center.
    /// The orientation of the normal follows the order of the points.
    /// If they are counter-clockwise in the World X-Y plane, then the normal points in World Z direction.
    /// On self intersecting shapes, where these cross products cancel each other out, the orientation is arbitrary.
    /// Returns ValueNone if:
    /// the Polyline3D has less than 3 points,
    /// or all points are within the tolerance of one point or of one line, so that there is no unique plane,
    /// or any point is further away from the plane than the tolerance.
    /// Use Polyline3D.tryAveragePlane to get the average plane of non planar points too.
    static member inline tryPlane (tolerance:float) (pl:Polyline3D) : NPlane voption =
        pl.TryPlane(tolerance)

    /// Tests if all points of the Polyline3D lie in one plane.
    /// The optional 'tolerance' is the maximum distance any point may have from that plane, it defaults to 1e-6.
    /// Returns FALSE if the Polyline3D has less than 3 points,
    /// or if all points are within the tolerance of one point or of one line,
    /// because then there is no unique plane.
    member _.IsPlanar([<OPT;DEF(1e-6)>] tolerance:float) : bool =
        match tryPlaneOf true tolerance xyzs with
        | ValueSome _ -> true
        | ValueNone -> false

    /// Tests if all points of the Polyline3D lie in one plane.
    /// The 'tolerance' is the maximum distance any point may have from that plane. Use 1e-6 as a default.
    /// Returns FALSE if the Polyline3D has less than 3 points,
    /// or if all points are within the tolerance of one point or of one line,
    /// because then there is no unique plane.
    static member inline isPlanar (tolerance:float) (pl:Polyline3D) : bool =
        pl.IsPlanar(tolerance)

    /// Scales the Polyline3D in place by a given factor.
    /// Scale center is World Origin 0,0,0
    member p.Scale (factor:float) : unit =
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            xyzs.[i] <- xyzs.[i] * factor
            i <- i + 1

    /// Returns a new Polyline3D scaled by a given factor. The input Polyline3D is not changed.
    /// Scale center is World Origin 0,0,0
    static member scale (factor:float) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.Scale factor
        res

    /// <summary>Scales the Polyline3D in place by a given factor around a given center point.</summary>
    /// <param name="cen">The center point of the scaling.</param>
    /// <param name="factor">The scale factor.</param>
    member p.ScaleOn (cen:Pnt) (factor:float) : unit =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        let mutable i = 0
        let len = xyzs.Count
        while i < len do
            xyzs.[i    ] <- cx + (xyzs.[i    ] - cx) * factor
            xyzs.[i + 1] <- cy + (xyzs.[i + 1] - cy) * factor
            xyzs.[i + 2] <- cz + (xyzs.[i + 2] - cz) * factor
            i <- i + 3

    /// <summary>Returns a new Polyline3D scaled by a given factor around a given center point.</summary>
    /// <param name="cen">The center point of the scaling.</param>
    /// <param name="factor">The scale factor.</param>
    /// <param name="pl">The Polyline3D to scale. It is not changed.</param>
    /// <returns>A new Polyline3D. Use p.ScaleOn to scale in place.</returns>
    static member inline scaleOn (cen:Pnt) (factor:float) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.ScaleOn cen factor
        res

    /// Moves the Polyline3D in place by a vector.
    member p.Move (v:Vec) : unit =
        let vx = v.X
        let vy = v.Y
        let vz = v.Z
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            xyzs.[i    ] <- xyzs.[i    ] + vx
            xyzs.[i + 1] <- xyzs.[i + 1] + vy
            xyzs.[i + 2] <- xyzs.[i + 2] + vz
            i <- i + 3

    /// Returns a new Polyline3D moved by a vector. The input Polyline3D is not changed.
    static member move (v:Vec) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.Move v
        res

    /// Moves the Polyline3D in place by a given distance in X direction.
    member p.MoveX (distance:float) : unit =
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            xyzs.[i] <- xyzs.[i] + distance
            i <- i + 3

    /// Returns a new Polyline3D moved by a given distance in X direction. The input Polyline3D is not changed.
    static member moveX (distance:float) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.MoveX distance
        res

    /// Moves the Polyline3D in place by a given distance in Y direction.
    member p.MoveY (distance:float) : unit =
        let len = xyzs.Count
        let mutable i = 1
        while i < len do
            xyzs.[i] <- xyzs.[i] + distance
            i <- i + 3

    /// Returns a new Polyline3D moved by a given distance in Y direction. The input Polyline3D is not changed.
    static member moveY (distance:float) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.MoveY distance
        res

    /// Moves the Polyline3D in place by a given distance in Z direction.
    member p.MoveZ (distance:float) : unit =
        let len = xyzs.Count
        let mutable i = 2
        while i < len do
            xyzs.[i] <- xyzs.[i] + distance
            i <- i + 3

    /// Returns a new Polyline3D moved by a given distance in Z direction. The input Polyline3D is not changed.
    static member moveZ (distance:float) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.MoveZ distance
        res

    /// Applies or multiplies a 4x4 transformation matrix to the Polyline3D in place.
    member p.Transform (m:Matrix) : unit =
        let len = xyzs.Count
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
            xyzs.[i    ] <- x' * sc
            xyzs.[i + 1] <- y' * sc
            xyzs.[i + 2] <- z' * sc
            i <- i + 3


    /// Returns a new Polyline3D transformed by a 4x4 transformation matrix. The input Polyline3D is not changed.
    static member transform (m:Matrix) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.Transform m
        res

    /// Multiplies (or applies) a RigidMatrix to the Polyline3D in place.
    member p.TransformRigid (m:RigidMatrix) : unit =
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            xyzs.[i    ] <- m.M11*x + m.M21*y + m.M31*z + m.X41
            xyzs.[i + 1] <- m.M12*x + m.M22*y + m.M32*z + m.Y42
            xyzs.[i + 2] <- m.M13*x + m.M23*y + m.M33*z + m.Z43
            i <- i + 3

    /// Returns a new Polyline3D transformed by a RigidMatrix. The input Polyline3D is not changed.
    static member transformRigid (m:RigidMatrix) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.TransformRigid m
        res


    // #endregion
    // #region Rotate

    /// Multiplies (or applies) a Quaternion to the Polyline3D in place.
    /// The polyline is rotated around the World Origin.
    member p.Rotate (q:Quaternion) : unit =
        let len = xyzs.Count
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
            xyzs.[i    ] <- x + qw * tx + qy * tz - qz * ty
            xyzs.[i + 1] <- y + qw * ty + qz * tx - qx * tz
            xyzs.[i + 2] <- z + qw * tz + qx * ty - qy * tx
            i <- i + 3

    /// Returns a new Polyline3D rotated by a Quaternion. The input Polyline3D is not changed.
    /// The polyline is rotated around the World Origin.
    static member inline rotate (q:Quaternion) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.Rotate q
        res

    /// <summary>Multiplies (or applies) a Quaternion to the Polyline3D in place, to rotate around a given center point.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="q">The rotation to apply.</param>
    member p.RotateAround (cen:Pnt) (q:Quaternion) : unit =
        let len = xyzs.Count
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let mutable i = 0
        while i < len do
            let x = xyzs.[i    ] - cx
            let y = xyzs.[i + 1] - cy
            let z = xyzs.[i + 2] - cz
            let tx = 2.0 * ( qy * z - qz * y)
            let ty = 2.0 * ( qz * x - qx * z)
            let tz = 2.0 * ( qx * y - qy * x)
            // v + q.w * t + cross( q.xyz, t);
            xyzs.[i    ] <- x + qw * tx + qy * tz - qz * ty + cx
            xyzs.[i + 1] <- y + qw * ty + qz * tx - qx * tz + cy
            xyzs.[i + 2] <- z + qw * tz + qx * ty - qy * tx + cz
            i <- i + 3

    /// <summary>Returns a new Polyline3D rotated by a Quaternion around a given center point.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="q">The rotation to apply.</param>
    /// <param name="pl">The Polyline3D to rotate. It is not changed.</param>
    /// <returns>A new Polyline3D. Use p.RotateAround to rotate in place.</returns>
    static member rotateAround (cen:Pnt) (q:Quaternion) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.RotateAround cen q
        res

    /// Rotates the Polyline3D in place around the Z-axis.
    member p.RotateOnZ (r:Rotation2D) : unit =
        let len = xyzs.Count
        let sin = r.Sin
        let cos = r.Cos
        let mutable i = 0
        while i < len do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            xyzs.[i    ] <- cos * x - sin * y
            xyzs.[i + 1] <- sin * x + cos * y
            i <- i + 3

    /// Returns a new Polyline3D rotated around the Z-axis. The input Polyline3D is not changed.
    static member rotateOnZ (r:Rotation2D) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.RotateOnZ r
        res

    /// Rotates the Polyline3D in place around the local Z-axis through the given center point.
    member p.RotateOnZAround (cen:Pnt) (r:Rotation2D) : unit =
        let len = xyzs.Count
        let sin = r.Sin
        let cos = r.Cos
        let cx = cen.X
        let cy = cen.Y
        let mutable i = 0
        while i < len do
            let x = xyzs.[i    ] - cx
            let y = xyzs.[i + 1] - cy
            xyzs.[i    ] <- cos * x - sin * y + cx
            xyzs.[i + 1] <- sin * x + cos * y + cy
            i <- i + 3

    /// Returns a new Polyline3D rotated around the local Z-axis through the given center point. The input Polyline3D is not changed.
    static member rotateOnZAround (cen:Pnt) (r:Rotation2D) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.RotateOnZAround cen r
        res

    // #endregion
    // #region Static members


    /// Tests if the Polyline3D is Clockwise when projected in 2D (Z values are ignored).
    /// If not, reverses the Polyline3D in place.
    /// Raises an error if the area of the Polyline3D in 2D is zero.
    member pl.EnsureClockwiseIn2D() : unit =
        if pl.IsCounterClockwiseIn2D then pl.Reverse()

    /// Returns a new Polyline3D that is Clockwise when projected in 2D (Z values are ignored).
    /// It is reversed if the input is counter-clockwise. The input Polyline3D is not changed.
    /// Raises an error if the area of the Polyline3D in 2D is zero.
    static member inline ensureClockwiseIn2D (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.EnsureClockwiseIn2D()
        res

    /// Tests if the Polyline3D is counter-clockwise when projected in 2D (Z values are ignored).
    /// If not, reverses the Polyline3D in place.
    /// Raises an error if the area of the Polyline3D in 2D is zero.
    member pl.EnsureCounterClockwiseIn2D() : unit =
        if pl.IsClockwiseIn2D then pl.Reverse()

    /// Returns a new Polyline3D that is counter-clockwise when projected in 2D (Z values are ignored).
    /// It is reversed if the input is clockwise. The input Polyline3D is not changed.
    /// Raises an error if the area of the Polyline3D in 2D is zero.
    static member inline ensureCounterClockwiseIn2D (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.EnsureCounterClockwiseIn2D()
        res


    // #endregion
    // #region Map and Iter

    /// <summary>Apply a mapping function to each point in the Polyline3D. Returns new Polyline3D.</summary>
    /// <param name="mapping">A function that takes a point and returns a new point.</param>
    /// <param name="pl">The Polyline3D to map over.</param>
    /// <returns>A new Polyline3D with the mapped points.</returns>
    static member mapPnt (mapping:Pnt-> Pnt) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            let pnt = mapping (Pnt(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2]))
            cs.Add pnt.X
            cs.Add pnt.Y
            cs.Add pnt.Z
            i <- i + 3
        Polyline3D cs


    /// <summary>Apply a mapping function to each point in the Polyline3D with the pointIndex (not the index into the flat coordinate array). Returns new Polyline3D.</summary>
    /// <param name="mapping">A function that takes the pointIndex of the point ( = array index/3) and the point itself, and returns a new point.
    /// </param>
    /// <param name="pl">The Polyline3D to map over.</param>
    /// <returns>A new Polyline3D with the mapped points.</returns>
    static member mapiPnt (mapping:int -> Pnt -> Pnt) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            let pnt = mapping (i/3) (Pnt(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2]))
            cs.Add pnt.X
            cs.Add pnt.Y
            cs.Add pnt.Z
            i <- i + 3
        Polyline3D(cs)

    /// <summary>Apply a mapping function to each point in the Polyline3D. Returns new Polyline3D.</summary>
    /// <param name="mapping">A function that takes the X, Y and Z coordinates of a point and returns a new point.</param>
    /// <param name="pl">The Polyline3D to map over.</param>
    /// <returns>A new Polyline3D with the mapped points.</returns>
    static member map (mapping:float -> float -> float -> Pnt) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            let pnt = mapping xyzs.[i] xyzs.[i + 1] xyzs.[i + 2]
            cs.Add pnt.X
            cs.Add pnt.Y
            cs.Add pnt.Z
            i <- i + 3
        Polyline3D cs

    /// <summary>Apply a mapping function to each point in the Polyline3D with index. Returns new Polyline3D.</summary>
    /// <param name="mapping">A function that takes the index of the X coordinate (in the flat coordinate array) and the X, Y and Z coordinates of a point and returns a new point.</param>
    /// <param name="pl">The Polyline3D to map over.</param>
    /// <returns>A new Polyline3D with the mapped points.</returns>
    static member mapi (mapping:int -> float -> float -> float -> Pnt) (pl:Polyline3D) : Polyline3D =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let cs = ResizeArray<float>(len)
        let mutable i = 0
        while i < len do
            let pnt = mapping i xyzs.[i] xyzs.[i + 1] xyzs.[i + 2]
            cs.Add pnt.X
            cs.Add pnt.Y
            cs.Add pnt.Z
            i <- i + 3
        Polyline3D cs

    /// <summary>Iterate over each point in the Polyline3D.</summary>
    /// <param name="action">A function that takes the X, Y and Z coordinates of a point.</param>
    /// <param name="pl">The Polyline3D to iterate over.</param>
    static member inline iter (action:float -> float -> float -> unit) (pl:Polyline3D) : unit =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            action xyzs.[i] xyzs.[i + 1] xyzs.[i + 2]
            i <- i + 3

    /// <summary>Iterate over each point in the Polyline3D.</summary>
    /// <param name="action">A function that takes a point.</param>
    /// <param name="pl">The Polyline3D to iterate over.</param>
    static member iterPnt (action:Pnt -> unit) (pl:Polyline3D) : unit =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            action (Pnt(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2]))
            i <- i + 3

    // /// <summary>Iterate over each point in the Polyline3D except the last point.</summary>
    // /// <param name="action">A function that takes the X, Y and Z coordinates of a point.</param>
    // /// <param name="pl">The Polyline3D to iterate over.</param>
    // static member inline iterSkipLast (action:float -> float -> float -> unit) (pl:Polyline3D) : unit =
    //     let xyzs = pl.XYZs
    //     let len = xyzs.Count - 3 // skip last point
    //     let mutable i = 0
    //     while i < len do
    //         action xyzs.[i] xyzs.[i + 1] xyzs.[i + 2]
    //         i <- i + 3

    // /// <summary>Iterate over each point in the Polyline3D except the last point.</summary>
    // /// <param name="action">A function that takes a point.</param>
    // /// <param name="pl">The Polyline3D to iterate over.</param>
    // static member inline iterPntSkipLast (action:Pnt -> unit) (pl:Polyline3D) : unit =
    //     let xyzs = pl.XYZs
    //     let len = xyzs.Count - 3 // skip last point
    //     let mutable i = 0
    //     while i < len do
    //         action (Pnt(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2]))
    //         i <- i + 3


    /// <summary>Iterate over each point in the Polyline3D with index.</summary>
    /// <param name="action">A function that takes the index of the X coordinate (in the flat coordinate array) and the X, Y and Z coordinates of a point.</param>
    /// <param name="pl">The Polyline3D to iterate over.</param>
    static member iteri (action:int -> float -> float -> float -> unit) (pl:Polyline3D) : unit =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            action i xyzs.[i] xyzs.[i + 1] xyzs.[i + 2]
            i <- i + 3

    /// <summary>Iterate over each point in the Polyline3D with index.</summary>
    /// <param name="action">A function that takes the pointIndex of the point ( = array index/3) and the point itself.</param>
    /// <param name="pl">The Polyline3D to iterate over.</param>
    static member iteriPnt (action:int -> Pnt -> unit) (pl:Polyline3D) : unit =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            action (i/3) (Pnt(xyzs.[i], xyzs.[i + 1], xyzs.[i + 2]))
            i <- i + 3

    /// <summary>Find the first point in the Polyline3D that satisfies a given condition. Returns ValueSome(point) if found, otherwise ValueNone.</summary>
    /// <param name="condition">A function that takes the X, Y and Z coordinates of a point and returns a boolean indicating whether the condition is satisfied.</param>
    /// <param name="pl">The Polyline3D to search through.</param>
    /// <returns>ValueSome(Pnt) if a point satisfying the condition is found, otherwise ValueNone.</returns>
    static member inline tryFind (condition: float -> float -> float -> bool) (pl:Polyline3D) : Pnt voption =
        let mutable result = ValueNone
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            if condition x y z then
                result <- ValueSome (Pnt(x, y, z))
                i <- len // exit loop
            else
                i <- i + 3
        result

    /// <summary>Find the last point in the Polyline3D that satisfies a given condition. Returns ValueSome(point) if found, otherwise ValueNone.</summary>
    /// <param name="condition">A function that takes the X, Y and Z coordinates of a point and returns a boolean indicating whether the condition is satisfied.</param>
    /// <param name="pl">The Polyline3D to search through.</param>
    /// <returns>ValueSome(Pnt) if a point satisfying the condition is found, otherwise ValueNone.</returns>
    static member inline tryFindLast (condition: float -> float -> float -> bool) (pl:Polyline3D) : Pnt voption =
        let mutable result = ValueNone
        let xyzs = pl.XYZs
        let mutable i = xyzs.Count - 3
        while i >= 0 do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            if condition x y z then
                result <- ValueSome (Pnt(x, y, z))
                i <- -3 // exit loop
            else
                i <- i - 3
        result

    /// <summary>Find the index of the first point in the Polyline3D that satisfies a given condition. Returns ValueSome(index) if found, otherwise ValueNone.</summary>
    /// <param name="condition">A function that takes the X, Y and Z coordinates of a point and returns a boolean indicating whether the condition is satisfied.</param>
    /// <param name="pl">The Polyline3D to search through.</param>
    /// <returns>ValueSome(index of x) the index of the x value in the polyline's flat array if found, otherwise ValueNone.</returns>
    static member inline tryFindIndex (condition: float -> float -> float -> bool) (pl:Polyline3D) : int voption =
        let mutable result = ValueNone
        let xyzs = pl.XYZs
        let len = xyzs.Count
        let mutable i = 0
        while i < len do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            if condition x y z then
                result <- ValueSome i
                i <- len // exit loop
            else
                i <- i + 3
        result

    /// <summary>Find the index of the last point in the Polyline3D that satisfies a given condition. Returns ValueSome(index) if found, otherwise ValueNone.</summary>
    /// <param name="condition">A function that takes the X, Y and Z coordinates of a point and returns a boolean indicating whether the condition is satisfied.</param>
    /// <param name="pl">The Polyline3D to search through.</param>
    /// <returns>ValueSome(index of x) the index of the x value in the polyline's flat array if found, otherwise ValueNone.</returns>
    static member inline tryFindLastIndex (condition: float -> float -> float -> bool) (pl:Polyline3D) : int voption =
        let mutable result = ValueNone
        let xyzs = pl.XYZs
        let mutable i = xyzs.Count - 3
        while i >= 0 do
            let x = xyzs.[i]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            if condition x y z then
                result <- ValueSome i
                i <- -3 // exit loop
            else
                i <- i - 3
        result

    /// <summary>Iterate over each segment in the Polyline3D.</summary>
    /// <param name="action">A function that takes startX, startY, startZ, endX, endY, and endZ coordinates of a segment.</param>
    /// <param name="pl">The Polyline3D to iterate over.</param>
    static member inline iterSegments (action: float -> float -> float -> float -> float -> float -> unit) (pl:Polyline3D) : unit =
        let xyzs = pl.XYZs
        let len = xyzs.Count
        if len < 6 then
            () // not enough points for a segment
        else
            let mutable x = xyzs.[0]
            let mutable y = xyzs.[1]
            let mutable z = xyzs.[2]
            let mutable i = 3
            while i < len do
                let nextX = xyzs.[i]
                let nextY = xyzs.[i + 1]
                let nextZ = xyzs.[i + 2]
                action x y z nextX nextY nextZ
                x <- nextX
                y <- nextY
                z <- nextZ
                i <- i + 3

    /// <summary>Iterate over each segment in the Polyline3D.</summary>
    /// <param name="action">A function that takes a Line3D representing the segment.</param>
    /// <param name="pl">The Polyline3D to iterate over.</param>
    static member inline iterLineSegments (action: Line3D -> unit) (pl:Polyline3D) : unit =
        pl |> Polyline3D.iterSegments (fun x1 y1 z1 x2 y2 z2 -> action (Line3D(x1, y1, z1, x2, y2, z2)))



    // #endregion
    // #region Create

    /// Creates a Polyline3D from a list of objects with X, Y, and Z members (uppercase).
    static member inline createFromMembersXYZ (xyzObjs: seq< ^T >) : Polyline3D =
        let coordinates = ResizeArray<float>()
        for pt in xyzObjs do
            let x = float (^T : (member X : _) pt)
            let y = float (^T : (member Y : _) pt)
            let z = float (^T : (member Z : _) pt)
            coordinates.Add x
            coordinates.Add y
            coordinates.Add z
        Polyline3D.createFromXYZs coordinates

    /// Creates a Polyline3D from a list of objects with x, y, and z members (lowercase).
    static member inline createFromMembersxyz (xyzObjs: seq< ^T >) : Polyline3D =
        let coordinates = ResizeArray<float>()
        for pt in xyzObjs do
            let x = float (^T : (member x : _) pt)
            let y = float (^T : (member y : _) pt)
            let z = float (^T : (member z : _) pt)
            coordinates.Add x
            coordinates.Add y
            coordinates.Add z
        Polyline3D.createFromXYZs coordinates

    /// Creates a Polyline3D from a list of objects with X and Y members (uppercase), using 0.0 for Z.
    static member inline createFromMembersXY (xyObjs: seq< ^T >) : Polyline3D =
        let coordinates = ResizeArray<float>()
        for pt in xyObjs do
            let x = float (^T : (member X : _) pt)
            let y = float (^T : (member Y : _) pt)
            coordinates.Add x
            coordinates.Add y
            coordinates.Add 0.0
        Polyline3D.createFromXYZs coordinates

    /// Creates a Polyline3D from a list of objects with x and y members (lowercase), using 0.0 for z.
    static member inline createFromMembersxy (xyObjs: seq< ^T >) : Polyline3D =
        let coordinates = ResizeArray<float>()
        for pt in xyObjs do
            let x = float (^T : (member x : _) pt)
            let y = float (^T : (member y : _) pt)
            coordinates.Add x
            coordinates.Add y
            coordinates.Add 0.0
        Polyline3D.createFromXYZs coordinates

    /// Create a new Polyline3D by copying over all points.
    /// This will allocate a new ResizeArray and copy all points.
    static member createFromPnts(points: seq<Pnt>) : Polyline3D =
        if isNull points then
            failNull "Polyline3D" "points"
        let xyzs = ResizeArray<float>()
        for pnt in points do
            xyzs.Add pnt.X
            xyzs.Add pnt.Y
            xyzs.Add pnt.Z
        Polyline3D(xyzs)

    /// Create a new Polyline3D by using the provided X, Y and Z interleaved ResizeArray directly.
    /// The buffer is shared, not copied: later changes to the ResizeArray will be reflected in the Polyline3D, and vice versa.
    static member createFromXYZs (xyzs: ResizeArray<float>) : Polyline3D =
        if isNull xyzs then failNull "Polyline3D.createFromXYZs" "coordinates"
        if xyzs.Count % 3 <> 0 then
            fail $"Polyline3D.createFromXYZs: coordinate buffer must contain a multiple of three floats, but has {xyzs.Count} values."
        Polyline3D(xyzs)

    /// Create a new empty Polyline3D without any points.
    /// But predefined capacity.
    static member inline createEmpty (capacity:int) : Polyline3D =
        Polyline3D(capacity)

    /// Creates a closed Polyline3D starting at the Origin,
    /// going to x, then x+y, then y and back to origin.
    static member createFromRect3D (r:Rect3D)  : Polyline3D =
        Polyline3D.createFromXYZs r.PointsXYZLooped



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
            np.Reverse()
        np

    /// <summary>Tests if two Polyline3D have the same point count and if their corresponding points are equal within a given tolerance.
    /// The tolerance applies to the X, Y and Z coordinates of each point separately, not to the Euclidean distance between the points.
    /// The same instance is always equal to itself. Otherwise a NaN coordinate is never equal.
    /// Use Polyline3D.equalsExact to check for an exact match.</summary>
    /// <param name="tol">The tolerance value for comparing each coordinate.</param>
    /// <param name="pl1">The first Polyline3D instance.</param>
    /// <param name="pl2">The second Polyline3D instance.</param>
    /// <returns>TRUE if the two Polyline3D instances are equal within the tolerance, otherwise FALSE.</returns>
    static member equalsWithin (tol:float) (pl1:Polyline3D) (pl2:Polyline3D) : bool =
        if Object.ReferenceEquals(pl1, pl2) then
            true
        elif pl1.XYZs.Count <> pl2.XYZs.Count then
            false
        else
            let xyzs1 = pl1.XYZs
            let xyzs2 = pl2.XYZs
            let len = xyzs1.Count
            let mutable i = 0
            let mutable equal = true
            while i < len do
                if not (abs (xyzs1.[i] - xyzs2.[i]) <= tol) then // 'not' to catch NaN too
                    equal <- false
                    i <- len // exit loop
                else
                    i <- i + 1
            equal

    /// <summary>Tests if two Polyline3D have the same point count and if their corresponding points are exactly equal.
    /// The same instance is always equal to itself. Otherwise a NaN coordinate is never equal.</summary>
    /// <param name="pl1">The first Polyline3D instance.</param>
    /// <param name="pl2">The second Polyline3D instance.</param>
    /// <returns>TRUE if the two Polyline3D instances are equal, otherwise FALSE.</returns>
    static member equalsExact (pl1:Polyline3D) (pl2:Polyline3D) : bool =
        if Object.ReferenceEquals(pl1, pl2) then
            true
        elif pl1.XYZs.Count <> pl2.XYZs.Count then
            false
        else
            let xyzs1 = pl1.XYZs
            let xyzs2 = pl2.XYZs
            let len = xyzs1.Count
            let mutable i = 0
            let mutable equal = true
            while i < len do
                if xyzs1.[i] <> xyzs2.[i] then
                    equal <- false
                    i <- len // exit loop
                else
                    i <- i + 1
            equal

    /// <summary>Removes consecutive duplicate points from the Polyline3D in place, within a given tolerance.</summary>
    /// <param name="distanceTolerance"> The distance within which points are considered duplicates. </param>
    /// <remarks>From a cluster of points that are closer than the distanceTolerance, only the first point is kept.
    /// Use 'RemoveDuplicatePointsFaithfully' if you want to keep the edges in their position by re-intersecting segments.
    /// The position of start and end point is NOT changed. Use Close to ensure start and end point are identical.</remarks>
    member pl.RemoveDuplicatePoints (distanceTolerance:float) : unit =
        let xyzs = pl.XYZs
        if xyzs.Count > 3 then // skip a single point or empty polyline
            let len = xyzs.Count
            let toSq = distanceTolerance * distanceTolerance
            let mutable lastX = xyzs.[0]
            let mutable lastY = xyzs.[1]
            let mutable lastZ = xyzs.[2]
            let mutable w = 3 // write index for the kept points, it never gets ahead of the read index i
            let mutable i = 3
            while i < len do
                let x = xyzs.[i]
                let y = xyzs.[i + 1]
                let z = xyzs.[i + 2]
                let dx = x - lastX
                let dy = y - lastY
                let dz = z - lastZ
                if dx * dx + dy * dy + dz * dz > toSq then
                    xyzs.[w    ] <- x
                    xyzs.[w + 1] <- y
                    xyzs.[w + 2] <- z
                    w <- w + 3
                    lastX <- x
                    lastY <- y
                    lastZ <- z
                i <- i + 3
            if w >= 6 then // if only one point is left keep the start point, don't overwrite it with the end point
                xyzs.[w - 3] <- xyzs.[len - 3] // ensure last point is not moved by the algorithm, it might be off by distanceTolerance
                xyzs.[w - 2] <- xyzs.[len - 2]
                xyzs.[w - 1] <- xyzs.[len - 1]
            if w < len then
                xyzs.RemoveRange(w, len - w)

    /// <summary>Returns a new Polyline3D with consecutive duplicate points removed, within a given tolerance.
    /// The input Polyline3D is not changed.</summary>
    /// <param name="distanceTolerance"> The distance within which points are considered duplicates. </param>
    /// <param name="pl"> A 3D Polyline, open or closed. </param>
    /// <returns>A new Polyline3D. Use p.RemoveDuplicatePoints to remove the points in place.</returns>
    /// <remarks>From a cluster of points that are closer than the distanceTolerance, only the first point is kept.
    /// Use 'Polyline3D.removeDuplicatePointsFaithfully' if you want to keep the edges in their position by re-intersecting segments.
    /// The position of start and end point is NOT changed. Use Polyline3D.close to ensure start and end point are identical.</remarks>
    static member removeDuplicatePoints (distanceTolerance:float) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.RemoveDuplicatePoints distanceTolerance
        res

    /// <summary>Removes consecutive duplicate points from the Polyline3D in place, within a given tolerance, and returns them.
    /// Same as 'RemoveDuplicatePoints', but also reports the points that were removed.</summary>
    /// <param name="distanceTolerance">The distance within which points are considered duplicates.</param>
    /// <returns>The removed duplicate points, with consecutive identical entries listed once per run.
    /// An empty list if no duplicates were found and the Polyline3D was not changed.</returns>
    member pl.RemoveDuplicatePointsAndReport (distanceTolerance:float) : ResizeArray<Pnt> =
        let xyzs = pl.XYZs
        let duplicatePoints = ResizeArray<Pnt>()
        if xyzs.Count > 3 then // skip a single point or empty polyline
            let len = xyzs.Count
            let toleranceSq = distanceTolerance * distanceTolerance
            let mutable lastX = xyzs.[0]
            let mutable lastY = xyzs.[1]
            let mutable lastZ = xyzs.[2]
            let mutable w = 3 // write index for the kept points, it never gets ahead of the read index i
            let mutable i = 3
            let mutable previousPointWasSkipped = false
            while i < len do
                let x = xyzs.[i]
                let y = xyzs.[i + 1]
                let z = xyzs.[i + 2]
                let dx = x - lastX
                let dy = y - lastY
                let dz = z - lastZ
                if dx * dx + dy * dy + dz * dz > toleranceSq then
                    xyzs.[w    ] <- x
                    xyzs.[w + 1] <- y
                    xyzs.[w + 2] <- z
                    w <- w + 3
                    lastX <- x
                    lastY <- y
                    lastZ <- z
                    previousPointWasSkipped <- false
                else
                    if not previousPointWasSkipped then
                        duplicatePoints.Add(Pnt(x, y, z))
                    else
                        let previousDuplicate = duplicatePoints.[duplicatePoints.Count - 1]
                        if previousDuplicate.X <> x || previousDuplicate.Y <> y || previousDuplicate.Z <> z then
                            duplicatePoints.Add(Pnt(x, y, z))
                    previousPointWasSkipped <- true
                i <- i + 3
            if w < len then // duplicates were found
                if w >= 6 then
                    xyzs.[w - 3] <- xyzs.[len - 3]
                    xyzs.[w - 2] <- xyzs.[len - 2]
                    xyzs.[w - 1] <- xyzs.[len - 1]
                xyzs.RemoveRange(w, len - w)
        duplicatePoints

    /// <summary>Returns a new Polyline3D with consecutive duplicate points removed, within a given tolerance, and the removed points.
    /// Same as 'Polyline3D.removeDuplicatePoints', but also reports the points that were removed.
    /// The input Polyline3D is not changed.</summary>
    /// <param name="distanceTolerance">The distance within which points are considered duplicates.</param>
    /// <param name="pl">A 3D Polyline, open or closed.</param>
    /// <returns>A tuple of the new Polyline3D and the removed duplicate points, with consecutive identical entries listed once per run.
    /// The list of removed points is empty if no duplicates were found.
    /// Use p.RemoveDuplicatePointsAndReport to remove the points in place.</returns>
    static member removeDuplicatePointsAndReport (distanceTolerance:float) (pl:Polyline3D) : Polyline3D * ResizeArray<Pnt> =
        let res = pl.Duplicate()
        let duplicatePoints = res.RemoveDuplicatePointsAndReport distanceTolerance
        res, duplicatePoints

    /// <summary>Removes consecutive duplicate points from the Polyline3D in place, within a given tolerance.</summary>
    /// <param name="distanceTolerance"> The distance within which points are considered duplicates. </param>
    /// <remarks>This algorithm keeps edges in their position by re-intersecting segments at their closest approach if points are closer than the distanceTolerance but not identical.
    /// The position of start and end point is NOT changed. Use Close to ensure start and end point are identical.</remarks>
    member pl.RemoveDuplicatePointsFaithfully (distanceTolerance:float) : unit =
        let xyzs = pl.XYZs
        if xyzs.Count >= 6 then // skip a single point or empty polyline
            let len = xyzs.Count
            let tolSq = distanceTolerance * distanceTolerance
            let mutable prevX = xyzs.[0]
            let mutable prevY = xyzs.[1]
            let mutable prevZ = xyzs.[2]
            let mutable prevWasBad = false
            let mutable w = 3 // write index for the kept points, it never gets ahead of the read index i
            let mutable i = 3
            while i < len do
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
                    if prevWasBad && w >= 6 then // If fewer than 2 points are kept, keep the start point exactly, as at the end below.
                        // else, if at least 2 points are kept, re-intersect the edge from lastBad to this with the edge from prev back to the previous ok point
                        // lastBad is not overwritten yet: a bad point was skipped, so the write index is at least one point behind it.
                        let lastBadX = xyzs.[i - 3] // prev is the last point of a cluster of too close points, so we use this as the last point of the edge.
                        let lastBadY = xyzs.[i - 2]
                        let lastBadZ = xyzs.[i - 1]
                        let vLastX = thisX - lastBadX // Vec.createFromTo lastBadPt this
                        let vLastY = thisY - lastBadY
                        let vLastZ = thisZ - lastBadZ
                        let prevOkX = xyzs.[w - 6] // the kept point before prev
                        let prevOkY = xyzs.[w - 5]
                        let prevOkZ = xyzs.[w - 4]
                        let vFirstX = prevOkX - prevX // Vec.createFromTo prev prevOkPt, prev is the first bad point
                        let vFirstY = prevOkY - prevY
                        let vFirstZ = prevOkZ - prevZ
                        match XLineXYZ.tryClosestParameterRayA(lastBadX, lastBadY, lastBadZ, prevX, prevY, prevZ, vLastX, vLastY, vLastZ, vFirstX, vFirstY, vFirstZ) with
                        |ValueSome t ->
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
                                xyzs.[w - 3] <- x // replace prev that was already set
                                xyzs.[w - 2] <- y
                                xyzs.[w - 1] <- z
                            else
                                // closest approach is too far away or segments are near parallel, line fidelity is not kept, use the midpoint
                                xyzs.[w - 3] <- (lastBadX + prevX) * 0.5
                                xyzs.[w - 2] <- (lastBadY + prevY) * 0.5
                                xyzs.[w - 1] <- (lastBadZ + prevZ) * 0.5
                        |ValueNone -> // segments are parallel, no closest approach parameter
                            xyzs.[w - 3] <- (lastBadX + prevX) * 0.5 // in this case line fidelity is not kept, use the midpoint
                            xyzs.[w - 2] <- (lastBadY + prevY) * 0.5
                            xyzs.[w - 1] <- (lastBadZ + prevZ) * 0.5

                    // (b) just use this point, no last points to fix
                    xyzs.[w    ] <- thisX
                    xyzs.[w + 1] <- thisY
                    xyzs.[w + 2] <- thisZ
                    w <- w + 3
                    prevX <- thisX
                    prevY <- thisY
                    prevZ <- thisZ
                    prevWasBad <- false

                i <- i + 3

            // last point :
            if prevWasBad then
                // in this case we are maintaining the last point exactly. like we do with the first point.
                xyzs.[w - 3] <- xyzs.[len - 3]
                xyzs.[w - 2] <- xyzs.[len - 2]
                xyzs.[w - 1] <- xyzs.[len - 1]

            if w < len then
                xyzs.RemoveRange(w, len - w)

    /// <summary>Returns a new Polyline3D with consecutive duplicate points removed, within a given tolerance.
    /// The input Polyline3D is not changed.</summary>
    /// <param name="distanceTolerance"> The distance within which points are considered duplicates. </param>
    /// <param name="pl"> A 3D Polyline, open or closed. </param>
    /// <returns>A new Polyline3D. Use p.RemoveDuplicatePointsFaithfully to remove the points in place.</returns>
    /// <remarks>This algorithm keeps edges in their position by re-intersecting segments at their closest approach if points are closer than the distanceTolerance but not identical.
    /// The position of start and end point is NOT changed. Use Polyline3D.close to ensure start and end point are identical.</remarks>
    static member removeDuplicatePointsFaithfully (distanceTolerance:float) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.RemoveDuplicatePointsFaithfully distanceTolerance
        res

    /// Removes consecutive duplicate points and collinear points from the Polyline3D in place, within given tolerances.
    /// This algorithm allows the last and first point to be identical if the Polyline3D is closed.
    /// Collinear points are removed when the angle between segments is smaller than the cosine threshold (e.g. cosine of 0.5 degrees ).
    /// If the Polyline3D is closed and starts and ends with collinear segments, the first point is replaced with the last non-collinear point.
    /// So the joint of the loop is now moved to the last non-collinear point.
    /// So that there are no collinear segments even between start and end.
    /// Raises an error if all points are within the distanceTolerance of the first point. In that case the Polyline3D is not changed.
    member pl.RemoveDuplicateAndCollinearPoints (angleTolerance:float<Cosine.cosine>) (distanceTolerance:float) : unit =
        if angleTolerance < Cosine.``45.0`` then
            fail $"Polyline3D.removeDuplicateAndCollinearPoints: angleTolerance must be at least Cosine.``45.0`` ( that is 0.707) but was {angleTolerance} (= {acos (float angleTolerance) |> toDegrees} degrees)."
        if angleTolerance > Cosine.``0.01`` then
            fail $"Polyline3D.removeDuplicateAndCollinearPoints: angleTolerance must be at most Cosine.``0.01`` ( that is 0.999999984) but was {angleTolerance} (= {acos (float angleTolerance) |> toDegrees} degrees)."

        let xyzs = pl.XYZs
        if xyzs.Count >= 6 then // skip a single point or empty polyline
            let distTol = max distanceTolerance 1e-6 // vectors need to be longer than zero, otherwise unitizing would fail

            let lastIdx = xyzs.Count
            let mutable prevX = xyzs.[0] // the first point stays in place
            let mutable prevY = xyzs.[1]
            let mutable prevZ = xyzs.[2]
            let mutable w = 3 // write index for the kept points, it stays at least one point behind the read index i

            // (1) find first non-duplicate point:
            let mutable thisX = xyzs.[3]
            let mutable thisY = xyzs.[4]
            let mutable thisZ = xyzs.[5]
            let mutable len = sqrt ((thisX - prevX) * (thisX - prevX) + (thisY - prevY) * (thisY - prevY) + (thisZ - prevZ) * (thisZ - prevZ))
            let mutable i = 6
            while len < distTol && i < lastIdx do
                thisX <- xyzs.[i]
                thisY <- xyzs.[i + 1]
                thisZ <- xyzs.[i + 2]
                len   <- sqrt ((thisX - prevX) * (thisX - prevX) + (thisY - prevY) * (thisY - prevY) + (thisZ - prevZ) * (thisZ - prevZ))
                i <- i + 3

            if len < distTol then
                fail $"Polyline3D.removeDuplicateAndCollinearPoints: all {xyzs.Count / 3} points are within the distanceTolerance {distTol} of the first point {Pnt(prevX, prevY, prevZ)}."

            // first unit vector from prev to this
            let firstVecX = (thisX - prevX) / len
            let firstVecY = (thisY - prevY) / len
            let firstVecZ = (thisZ - prevZ) / len
            let mutable vPrevX = firstVecX
            let mutable vPrevY = firstVecY
            let mutable vPrevZ = firstVecZ

            // (2) main loop:
            while i < lastIdx do
                let nextX = xyzs.[i]
                let nextY = xyzs.[i + 1]
                let nextZ = xyzs.[i + 2]
                let vx = nextX - thisX
                let vy = nextY - thisY
                let vz = nextZ - thisZ
                let len = vx * vx + vy * vy + vz * vz |> sqrt
                if len > distTol then
                    // not duplicate, now check if collinear
                    let vNextX = vx / len
                    let vNextY = vy / len
                    let vNextZ = vz / len
                    let cos : float<Cosine.cosine> = LanguagePrimitives.FloatWithMeasure (vPrevX * vNextX + vPrevY * vNextY + vPrevZ * vNextZ)
                    if cos < angleTolerance then
                        // not collinear , keep this point
                        xyzs.[w    ] <- thisX
                        xyzs.[w + 1] <- thisY
                        xyzs.[w + 2] <- thisZ
                        w <- w + 3
                        prevX <- thisX
                        prevY <- thisY
                        prevZ <- thisZ
                        vPrevX <- vNextX // advance previous vector only when point kept
                        vPrevY <- vNextY
                        vPrevZ <- vNextZ
                    thisX <- nextX // always advance this point
                    thisY <- nextY
                    thisZ <- nextZ
                i <- i + 3

            // (3) handle last segment to first point
            if pl.IsAlmostClosed distTol then // the first and the last point are not overwritten yet
                // closed polyline, now check if last and first segment are collinear
                let cos : float<Cosine.cosine> = LanguagePrimitives.FloatWithMeasure (vPrevX * firstVecX + vPrevY * firstVecY + vPrevZ * firstVecZ)
                if cos < angleTolerance then
                    // not collinear , keep the original end point
                    xyzs.[w    ] <- xyzs.[lastIdx - 3]
                    xyzs.[w + 1] <- xyzs.[lastIdx - 2]
                    xyzs.[w + 2] <- xyzs.[lastIdx - 1]
                    w <- w + 3
                else
                    // collinear , replace first point with last non-collinear point
                    xyzs.[0] <- xyzs.[w - 3]
                    xyzs.[1] <- xyzs.[w - 2]
                    xyzs.[2] <- xyzs.[w - 1]
            else
                if abs (thisX - prevX) > distTol || abs (thisY - prevY) > distTol || abs (thisZ - prevZ) > distTol then
                    // open polyline , just add last point if not duplicate
                    xyzs.[w    ] <- thisX
                    xyzs.[w + 1] <- thisY
                    xyzs.[w + 2] <- thisZ
                    w <- w + 3
                else
                    // ensure last point is not changed, it might be off by distanceTolerance
                    xyzs.[w - 3] <- xyzs.[lastIdx - 3]
                    xyzs.[w - 2] <- xyzs.[lastIdx - 2]
                    xyzs.[w - 1] <- xyzs.[lastIdx - 1]

            if w < lastIdx then
                xyzs.RemoveRange(w, lastIdx - w)

    /// Returns a new Polyline3D with consecutive duplicate points and collinear points removed, within given tolerances.
    /// The input Polyline3D is not changed. Use p.RemoveDuplicateAndCollinearPoints to remove the points in place.
    /// This algorithm allows the last and first point to be identical if the Polyline3D is closed.
    /// Collinear points are removed when the angle between segments is smaller than the cosine threshold (e.g. cosine of 0.5 degrees ).
    /// If the Polyline3D is closed and starts and ends with collinear segments, the first point is replaced with the last non-collinear point.
    /// So the joint of the loop is now moved to the last non-collinear point.
    /// So that there are no collinear segments even between start and end.
    /// Raises an error if all points are within the distanceTolerance of the first point.
    static member removeDuplicateAndCollinearPoints (angleTolerance:float<Cosine.cosine>) (distanceTolerance:float) (pl:Polyline3D) : Polyline3D =
        let res = pl.Duplicate()
        res.RemoveDuplicateAndCollinearPoints angleTolerance distanceTolerance
        res

    /// <summary>Removes simple sharp U-Turns from the Polyline3D in place.</summary>
    /// <param name="minCos"> The angle between segments so that they are considered a U-turn.
    /// For example, for 179.9 degrees use 'Cosine.``179.9``'. </param>
    /// <remarks>The Offset3D module also has a removeUTurns function that takes precomputed unit vectors and is therefore more efficient.
    /// For nested U-turns and collinear points in U-turns segments use 'RemoveUTurnsDeeply'. That function calls repeatedly until no more U-turns are present.</remarks>
    member pl.RemoveUTurns (minCos:float<Cosine.cosine>) : unit =
        let xyzs = pl.XYZs
        let uvs = Offset3D.getSegmentUnitVectors xyzs
        let res = Offset3D.removeUTurns(xyzs, uvs, minCos)
        if not (Object.ReferenceEquals(res, xyzs)) then // Offset3D.removeUTurns returns the input list if there are no U-turns
            ResizeArr.clear xyzs
            xyzs.AddRange res

    /// <summary>Returns a new Polyline3D with simple sharp U-Turns removed. The input Polyline3D is not changed.</summary>
    /// <param name="minCos"> The angle between segments so that they are considered a U-turn.
    /// For example, for 179.9 degrees use 'Cosine.``179.9``'. </param>
    /// <param name="polyLine"> A 3D Polyline, open or closed. </param>
    /// <returns>A new Polyline3D. Use p.RemoveUTurns to remove the U-turns in place.</returns>
    /// <remarks>For nested U-turns and collinear points in U-turns segments use 'removeUTurnsDeeply'.</remarks>
    static member removeUTurns (minCos:float<Cosine.cosine>) (polyLine:Polyline3D) : Polyline3D =
        let res = polyLine.Duplicate()
        res.RemoveUTurns minCos
        res

    /// <summary>Removes all sharp U-Turns from the Polyline3D in place, recursively until no more U-turns are present.
    /// This function calls 'RemoveUTurns' repeatedly until no more U-turns are present.
    /// Use this function when you have nested U-turns and collinear points in U-turns segments.</summary>
    /// <param name="minCos"> The angle between segments so that they are considered a U-turn.
    /// For example, for 179.9 degrees use 'Cosine.``179.9``'. </param>
    /// <remarks>If an error is raised in any of the repeated calls, the Polyline3D is not changed.</remarks>
    member pl.RemoveUTurnsDeeply (minCos:float<Cosine.cosine>) : unit =
        let xyzs = pl.XYZs
        let mutable input = xyzs
        let mutable output = Offset3D.removeUTurns(input, Offset3D.getSegmentUnitVectors input, minCos)
        while output.Count <> input.Count do
            input  <- output
            output <- Offset3D.removeUTurns(input, Offset3D.getSegmentUnitVectors input, minCos)
        if not (Object.ReferenceEquals(output, xyzs)) then // only write back once all passes succeeded
            ResizeArr.clear xyzs
            xyzs.AddRange output

    /// <summary>Returns a new Polyline3D with all sharp U-Turns removed, recursively until no more U-turns are present.
    /// The input Polyline3D is not changed.
    /// Use this function when you have nested U-turns and collinear points in U-turns segments.</summary>
    /// <param name="minCos"> The angle between segments so that they are considered a U-turn.
    /// For example, for 179.9 degrees use 'Cosine.``179.9``'. </param>
    /// <param name="polyLine"> A 3D Polyline, open or closed. </param>
    /// <returns>A new Polyline3D. Use p.RemoveUTurnsDeeply to remove the U-turns in place.</returns>
    static member removeUTurnsDeeply (minCos:float<Cosine.cosine>) (polyLine:Polyline3D) : Polyline3D =
        let res = polyLine.Duplicate()
        res.RemoveUTurnsDeeply minCos
        res

    // #endregion
    // #region Offset

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Takes a reference normal for orienting the perpendicular offset and determining inside/outside.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// This function raises an Exception on duplicate points and on U-turns sharper than 175 degrees.
    /// Uses the default settings of Polyline3D.offsetByNormalCustom. Use Polyline3D.offsetByNormalCustom to set them.</summary>
    /// <param name="refNormal">A unit vector defining an approximate normal direction for orienting the perpendicular offset and determining inside/outside.
    /// Only if the polyline has no corner, like a straight one, it defines the offset plane.
    /// Fails if it is within half a degree of perpendicular to the plane of every corner, because then it cannot tell the sides apart.</param>
    /// <param name="inPlaneOffsetDistance">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline (for counter-clockwise polylines when refNormal points up).
    /// A negative distance offsets to the outside.</param>
    /// <param name="perpendicularOffsetDistance">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the same orientation of refNormal. A negative distance offsets in the opposite direction.</param>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetByNormal (refNormal:UnitVec) (inPlaneOffsetDistance:float) (perpendicularOffsetDistance:float) (polyLine:Polyline3D) : Polyline3D =
        Polyline3D.offsetByNormalCustom(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistance, refNormal)

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Takes a reference normal for orienting the perpendicular offset and determining inside/outside.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// This function raises an Exception on duplicate points and on U-turns sharper than failAtUTurnAbove.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistance">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline (for counter-clockwise polylines when refNormal points up).
    /// A negative distance offsets to the outside.</param>
    /// <param name="perpendicularOffsetDistance">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the same orientation of refNormal. A negative distance offsets in the opposite direction.</param>
    /// <param name="refNormal">A unit vector defining an approximate normal direction for orienting the perpendicular offset and determining inside/outside.
    /// Only if the polyline has no corner, like a straight one, it defines the offset plane.
    /// Fails if it is within half a degree of perpendicular to the plane of every corner, because then it cannot tell the sides apart.</param>
    /// <param name="loop">Optional, defaults to FALSE.
    /// Set to TRUE to treat an open polyline as a closed loop, even if first and last points are not at the same location.</param>
    /// <param name="considerCollinearBelow">Optional, defaults to 2.5 degrees in cosine measure.
    /// The cosine of the angle below which segments are considered collinear,
    /// unless the polyline turned by more than this angle since the previous corner, so that an arc keeps its corners.
    /// A collinear point takes the plane interpolated between the neighboring corners.</param>
    /// <param name="failAtUTurnAbove">Optional, defaults to 175 degrees in cosine measure.
    /// The cosine of the angle above which a U-turn is considered to fail.
    /// There is no U-turn behavior like Offset2D.UTurn, on purpose: two almost opposite segments span no reliable plane in 3D, so a chamfer or threshold point has no defined direction. Remove U-turns first, for example with Polyline3D.removeUTurns.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetByNormalCustom(polyLine:Polyline3D,
                                inPlaneOffsetDistance: float,
                                perpendicularOffsetDistance: float,
                                refNormal: UnitVec,
                                [<OPT;DEF(false)>] loop:bool,
                                [<OPT;DEF(Cosine.``2.5``)>] considerCollinearBelow: float<Cosine.cosine>,
                                [<OPT;DEF(Cosine.``175.0``)>] failAtUTurnAbove :float<Cosine.cosine>
                                ) : Polyline3D =
        let xyzs = polyLine.XYZs
        let ptCount = polyLine.PointCount
        if ptCount < 2 then
            fail $"Polyline3D.offsetByNormal: Polyline3D must have at least 2 points but has {ptCount} points."
        // check if looping desired and curve is open
        let dx = xyzs.[0] - xyzs.[xyzs.Count - 3]
        let dy = xyzs.[1] - xyzs.[xyzs.Count - 2]
        let dz = xyzs.[2] - xyzs.[xyzs.Count - 1]
        let isOpen = dx * dx + dy * dy + dz * dz > Offset3D.openToleranceSq
        if loop && isOpen then
            let closedXYZs = ResizeArray<float>(xyzs.Count + 3)
            closedXYZs.AddRange xyzs
            closedXYZs.Add xyzs.[0]
            closedXYZs.Add xyzs.[1]
            closedXYZs.Add xyzs.[2]
            let res = Offset3D.offsetByNormalCustom(closedXYZs, inPlaneOffsetDistance, perpendicularOffsetDistance, refNormal, considerCollinearBelow, failAtUTurnAbove)
            ResizeArr.popOff res// remove last point to open the polyline again
            ResizeArr.popOff res
            ResizeArr.popOff res
            Polyline3D.createFromXYZs res
        else
            Offset3D.offsetByNormalCustom(xyzs, inPlaneOffsetDistance, perpendicularOffsetDistance, refNormal, considerCollinearBelow, failAtUTurnAbove)
            |> Polyline3D.createFromXYZs

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// The reference normal is computed from the average normal of the polyline.
    /// This function raises an Exception on duplicate points, on U-turns sharper than failAtUTurnAbove, and if the average normal cannot be computed, for example if all points are collinear.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistance">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="perpendicularOffsetDistance">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="loop">Optional, defaults to FALSE.
    /// Set to TRUE to treat an open polyline as a closed loop, even if first and last points are not at the same location.</param>
    /// <param name="considerCollinearBelow">Optional, defaults to 2.5 degrees in cosine measure.
    /// The cosine of the angle below which segments are considered collinear,
    /// unless the polyline turned by more than this angle since the previous corner, so that an arc keeps its corners.
    /// A collinear point takes the plane interpolated between the neighboring corners.</param>
    /// <param name="failAtUTurnAbove">Optional, defaults to 175 degrees in cosine measure.
    /// The cosine of the angle above which a U-turn is considered to fail.
    /// There is no U-turn behavior like Offset2D.UTurn, on purpose: two almost opposite segments span no reliable plane in 3D, so a chamfer or threshold point has no defined direction. Remove U-turns first, for example with Polyline3D.removeUTurns.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetCustom(polyLine:Polyline3D,
                            inPlaneOffsetDistance: float,
                            perpendicularOffsetDistance: float,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(Cosine.``2.5``)>] considerCollinearBelow: float<Cosine.cosine>,
                            [<OPT;DEF(Cosine.``175.0``)>] failAtUTurnAbove :float<Cosine.cosine>
                            ) : Polyline3D =
        if polyLine.PointCount < 2 then
            fail $"Polyline3D.offset: Polyline3D must have at least 2 points but has {polyLine.PointCount} points."
        let refNormal = averageNormalOf polyLine.XYZs // not Polyline3D.AverageNormal, a degenerate normal is reported below
        if isTooSmallSq refNormal.LengthSq then // like Polyline3D.AverageNormal
            fail $"Polyline3D.offset: Cannot compute the average normal, the polygon through the {polyLine.PointCount} points has no area, for example because they are collinear. Use offsetByNormal with an explicit reference normal: {polyLine}"
        Polyline3D.offsetByNormalCustom(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistance, refNormal.Unitized, loop, considerCollinearBelow, failAtUTurnAbove)

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// The reference normal is computed from the average normal of the polyline.
    /// This function raises an Exception on duplicate points, on U-turns sharper than 175 degrees, and if the average normal cannot be computed, for example if all points are collinear.</summary>
    /// <param name="inPlaneOffsetDistance">The offset distance in the local plane defined by two segments.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="perpendicularOffsetDistance">The offset distance perpendicular to the local plane.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offset (inPlaneOffsetDistance:float) (perpendicularOffsetDistance:float) (polyLine:Polyline3D): Polyline3D =
        Polyline3D.offsetCustom(polyLine, inPlaneOffsetDistance, perpendicularOffsetDistance)

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner, with one in-plane distance per segment and one perpendicular distance per point.
    /// Takes a reference normal for orienting the perpendicular offset and determining inside/outside.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// This function raises an Exception on duplicate points, on U-turns sharper than 175 degrees, and at collinear points where the two segments have different in-plane distances.
    /// Uses the default settings of Polyline3D.offsetVariableByNormalCustom. Use Polyline3D.offsetVariableByNormalCustom to set them.</summary>
    /// <param name="refNormal">A unit vector defining an approximate normal direction for orienting the perpendicular offset and determining inside/outside.
    /// Only if the polyline has no corner, like a straight one, it defines the offset plane.
    /// Fails if it is within half a degree of perpendicular to the plane of every corner, because then it cannot tell the sides apart.</param>
    /// <param name="inPlaneOffsetDistances">The offset distances in the local plane defined by two segments. One distance per segment.</param>
    /// <param name="perpendicularOffsetDistances">The offset distances perpendicular to the local plane. One distance per point.
    /// For a closed polyline one less, because its last point is its first point.</param>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetVariableByNormal (refNormal:UnitVec) (inPlaneOffsetDistances: Collections.Generic.IList<float>) (perpendicularOffsetDistances: Collections.Generic.IList<float>) (polyLine:Polyline3D) : Polyline3D =
        Polyline3D.offsetVariableByNormalCustom(polyLine, inPlaneOffsetDistances, perpendicularOffsetDistances, refNormal)

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Takes a reference normal for orienting the perpendicular offset and determining inside/outside.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// This function raises an Exception on duplicate points, on U-turns sharper than failAtUTurnAbove, and at collinear points where the two segments have different in-plane distances, unless varDistParallelBehavior says otherwise.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistances">The offset distances in the local plane defined by two segments. One distance per segment.
    /// A positive distance offsets to the inside of the polyline (for counter-clockwise polylines when refNormal points up).
    /// A negative distance offsets to the outside.</param>
    /// <param name="perpendicularOffsetDistances">The offset distances perpendicular to the local plane. One distance per point.
    /// For a closed polyline one less, because its last point is its first point. With loop = TRUE one per point of the open polyline.
    /// A positive distance offsets in the same orientation of refNormal. A negative distance offsets in the opposite direction.</param>
    /// <param name="refNormal">A unit vector defining an approximate normal direction for orienting the perpendicular offset and determining inside/outside.
    /// Only if the polyline has no corner, like a straight one, it defines the offset plane.
    /// Fails if it is within half a degree of perpendicular to the plane of every corner, because then it cannot tell the sides apart.</param>
    /// <param name="loop">Optional, defaults to FALSE.
    /// Set to TRUE to treat an open polyline as a closed loop, even if first and last points are not at the same location.</param>
    /// <param name="varDistParallelBehavior">Optional, defaults to Fail.
    /// The behavior to use when collinear segments with different offset distances are found.</param>
    /// <param name="considerCollinearBelow">Optional, defaults to 2.5 degrees in cosine measure.
    /// The cosine of the angle below which segments are considered collinear,
    /// unless the polyline turned by more than this angle since the previous corner, or for different distances, since the last point that was offset exactly.</param>
    /// <param name="failAtUTurnAbove">Optional, defaults to 175 degrees in cosine measure.
    /// The cosine of the angle above which a U-turn is considered to fail.
    /// There is no U-turn behavior like Offset2D.UTurn, on purpose: two almost opposite segments span no reliable plane in 3D, so a chamfer or threshold point has no defined direction. Remove U-turns first, for example with Polyline3D.removeUTurns.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetVariableByNormalCustom(
                            polyLine:Polyline3D,
                            inPlaneOffsetDistances: Collections.Generic.IList<float>,
                            perpendicularOffsetDistances: Collections.Generic.IList<float>,
                            refNormal: UnitVec,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(Offset3D.VarDistParallel.Fail)>] varDistParallelBehavior: Offset3D.VarDistParallel, // fail by default
                            [<OPT;DEF(Cosine.``2.5``)>] considerCollinearBelow: float<Cosine.cosine>,
                            [<OPT;DEF(Cosine.``175.0``)>] failAtUTurnAbove :float<Cosine.cosine>
                            ) : Polyline3D =
        let xyzs = polyLine.XYZs
        let ptCount = polyLine.PointCount
        if ptCount < 2 then
            fail $"Polyline3D.offsetVariableByNormal: Polyline3D must have at least 2 points but has {ptCount} points."
        // check if looping desired and curve is open
        let dx = xyzs.[0] - xyzs.[xyzs.Count - 3]
        let dy = xyzs.[1] - xyzs.[xyzs.Count - 2]
        let dz = xyzs.[2] - xyzs.[xyzs.Count - 1]
        let isOpen = dx * dx + dy * dy + dz * dz > Offset3D.openToleranceSq
        if loop && isOpen then
            if inPlaneOffsetDistances.Count <> ptCount then
                fail ($"Polyline3D.offsetVariableByNormal: For open Polyline3D with loop=true the inPlaneOffsetDistances list must have the same number of items as the polyline has points.\n" +
                      $"But polyline has {ptCount} points and inPlaneOffsetDistances has {inPlaneOffsetDistances.Count} items.")
            if perpendicularOffsetDistances.Count <> ptCount then
                fail ($"Polyline3D.offsetVariableByNormal: For open Polyline3D with loop=true the perpendicularOffsetDistances list must have the same number of items as the polyline has points.\n" +
                      $"But polyline has {ptCount} points and perpendicularOffsetDistances has {perpendicularOffsetDistances.Count} items.")
            let closedXYZs = ResizeArray<float>(xyzs.Count + 3)
            closedXYZs.AddRange xyzs
            closedXYZs.Add xyzs.[0]
            closedXYZs.Add xyzs.[1]
            closedXYZs.Add xyzs.[2]
            let res = Offset3D.offsetVariableByNormalCustom(closedXYZs, inPlaneOffsetDistances, perpendicularOffsetDistances, refNormal, varDistParallelBehavior, considerCollinearBelow, failAtUTurnAbove)
            ResizeArr.popOff res // remove last point to open the polyline again
            ResizeArr.popOff res
            ResizeArr.popOff res
            Polyline3D.createFromXYZs res
        else
            Offset3D.offsetVariableByNormalCustom(xyzs, inPlaneOffsetDistances, perpendicularOffsetDistances, refNormal, varDistParallelBehavior, considerCollinearBelow, failAtUTurnAbove)
            |> Polyline3D.createFromXYZs

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner, with one in-plane distance per segment and one perpendicular distance per point.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// The reference normal is computed from the average normal of the polyline.
    /// This function raises an Exception on duplicate points, on U-turns sharper than 175 degrees, at collinear points where the two segments have different in-plane distances, and if the average normal cannot be computed, for example if all points are collinear.
    /// Uses the default settings of Polyline3D.offsetVariableCustom. Use Polyline3D.offsetVariableCustom to set them.</summary>
    /// <param name="inPlaneOffsetDistances">The offset distances in the local plane defined by two segments. One distance per segment.</param>
    /// <param name="perpendicularOffsetDistances">The offset distances perpendicular to the local plane. One distance per point.
    /// For a closed polyline one less, because its last point is its first point.</param>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetVariable (inPlaneOffsetDistances: Collections.Generic.IList<float>) (perpendicularOffsetDistances: Collections.Generic.IList<float>) (polyLine:Polyline3D) : Polyline3D =
        Polyline3D.offsetVariableCustom(polyLine, inPlaneOffsetDistances, perpendicularOffsetDistances)

    /// <summary> Offsets a Polyline in 3D space by finding the local plane in each corner.
    /// Auto-detects if given points are from a closed Polyline (first point = last point) and loops them.
    /// The reference normal is computed from the average normal of the polyline.
    /// This function raises an Exception on duplicate points, on U-turns sharper than failAtUTurnAbove, at collinear points where the two segments have different in-plane distances unless varDistParallelBehavior says otherwise, and if the average normal cannot be computed, for example if all points are collinear.</summary>
    /// <param name="polyLine"> A 3D Polyline, open or closed.</param>
    /// <param name="inPlaneOffsetDistances">The offset distances in the local plane defined by two segments. One distance per segment.
    /// A positive distance offsets to the inside of the polyline.
    /// A negative distance offsets to the outside.
    /// No matter how the polyline is oriented.</param>
    /// <param name="perpendicularOffsetDistances">The offset distances perpendicular to the local plane. One distance per point.
    /// For a closed polyline one less, because its last point is its first point. With loop = TRUE one per point of the open polyline.
    /// A positive distance offsets in the direction of the computed normal.
    /// For a counter-clockwise polyline in xy-plane this is Upwards.
    /// A negative distance offsets in the opposite direction.</param>
    /// <param name="loop">Optional, defaults to FALSE.
    /// Set to TRUE to treat an open polyline as a closed loop, even if first and last points are not at the same location.</param>
    /// <param name="varDistParallelBehavior">Optional, defaults to Fail.
    /// The behavior to use when collinear segments with different offset distances are found.</param>
    /// <param name="considerCollinearBelow">Optional, defaults to 2.5 degrees in cosine measure.
    /// The cosine of the angle below which segments are considered collinear,
    /// unless the polyline turned by more than this angle since the previous corner, or for different distances, since the last point that was offset exactly.</param>
    /// <param name="failAtUTurnAbove">Optional, defaults to 175 degrees in cosine measure.
    /// The cosine of the angle above which a U-turn is considered to fail.
    /// There is no U-turn behavior like Offset2D.UTurn, on purpose: two almost opposite segments span no reliable plane in 3D, so a chamfer or threshold point has no defined direction. Remove U-turns first, for example with Polyline3D.removeUTurns.</param>
    /// <returns>A new 3D polyline.</returns>
    static member offsetVariableCustom(polyLine:Polyline3D,
                            inPlaneOffsetDistances: Collections.Generic.IList<float>,
                            perpendicularOffsetDistances: Collections.Generic.IList<float>,
                            [<OPT;DEF(false)>] loop:bool,
                            [<OPT;DEF(Offset3D.VarDistParallel.Fail)>] varDistParallelBehavior: Offset3D.VarDistParallel, // fail by default
                            [<OPT;DEF(Cosine.``2.5``)>] considerCollinearBelow: float<Cosine.cosine>,
                            [<OPT;DEF(Cosine.``175.0``)>] failAtUTurnAbove :float<Cosine.cosine>
                            ) : Polyline3D =
        if polyLine.PointCount < 2 then
            fail $"Polyline3D.offsetVariable: Polyline3D must have at least 2 points but has {polyLine.PointCount} points."
        let refNormal = averageNormalOf polyLine.XYZs // not Polyline3D.AverageNormal, a degenerate normal is reported below
        if isTooSmallSq refNormal.LengthSq then // like Polyline3D.AverageNormal
            fail $"Polyline3D.offsetVariable: Cannot compute the average normal, the polygon through the {polyLine.PointCount} points has no area, for example because they are collinear. Use offsetVariableByNormal with an explicit reference normal: {polyLine}"
        Polyline3D.offsetVariableByNormalCustom(polyLine, inPlaneOffsetDistances, perpendicularOffsetDistances, refNormal.Unitized, loop, varDistParallelBehavior, considerCollinearBelow, failAtUTurnAbove)


    // #endregion

    /// <summary>Searches non-adjacent segments in index order for an intersection in O(n²) time.
    /// Excludes the closure joint when endpoints are within 1e-7, parallel overlaps (angle below 0.25 degrees),
    /// and segments shorter than 1e-6. Uses the XLine3D endpoint parameter tolerance of 1e-6.
    /// The closest separation must be less than 1e-6. Use Polyline3D.tryFindSelfIntersectionCustom to set this tolerance.</summary>
    /// <param name="pl">The polyline to search.</param>
    /// <returns>ValueSome with a point on the first segment and both segment indices, or ValueNone.</returns>
    static member tryFindSelfIntersection (pl:Polyline3D) : (Pnt * int * int) voption =
        Polyline3D.tryFindSelfIntersectionCustom(pl)

    /// <summary>Searches non-adjacent segments in index order for an intersection in O(n²) time.
    /// Excludes the closure joint when endpoints are within 1e-7, parallel overlaps (angle below 0.25 degrees),
    /// and segments shorter than 1e-6. Uses the XLine3D endpoint parameter tolerance of 1e-6.
    /// The closest separation must be strictly less than maxSkewDistance; zero therefore finds no intersections.
    /// The optional parameter can be given by name. Use Polyline3D.tryFindSelfIntersection for the default.</summary>
    /// <param name="pl">The polyline to search.</param>
    /// <param name="maxSkewDistance">Finite, nonnegative separation tolerance; defaults to 1e-6.</param>
    /// <returns>ValueSome with a point on the first segment and both segment indices, or ValueNone.</returns>
    static member tryFindSelfIntersectionCustom(pl:Polyline3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float) : (Pnt * int * int) voption =
        if maxSkewDistance < 0.0 || isNanInfinity maxSkewDistance then
            fail $"Polyline3D.tryFindSelfIntersectionCustom: maxSkewDistance must be finite and nonnegative, got {maxSkewDistance}."
        let xyzs = pl.XYZs
        let segmentCount = pl.PointCount - 1
        let mutable result = ValueNone
        if segmentCount > 1 then
            let isClosed = pl.IsAlmostClosed 1e-7
            let mutable i = 0
            while result.IsNone && i < segmentCount - 2 do
                let a = i * 3
                let ax = xyzs.[a+3] - xyzs.[a]
                let ay = xyzs.[a+4] - xyzs.[a+1]
                let az = xyzs.[a+5] - xyzs.[a+2]
                let last = if i = 0 && isClosed then segmentCount - 2 else segmentCount - 1
                let mutable j = i + 2
                while result.IsNone && j <= last do
                    let b = j * 3
                    match XLineXYZ.tryIntersect(
                        xyzs.[a], xyzs.[a+1], xyzs.[a+2], xyzs.[b], xyzs.[b+1], xyzs.[b+2],
                        ax, ay, az, xyzs.[b+3] - xyzs.[b], xyzs.[b+4] - xyzs.[b+1], xyzs.[b+5] - xyzs.[b+2],
                        maxSkewDistance, Tangent.``0.25``, 1e-6) with
                    | ValueSome p -> result <- ValueSome(p, i, j)
                    | ValueNone -> ()
                    j <- j + 1
                i <- i + 1
        result

    /// <summary>Finds all intersections between non-adjacent segments in O(n²) time,
    /// with the same rules as Polyline3D.tryFindSelfIntersection:
    /// Excludes the closure joint when endpoints are within 1e-7, parallel overlaps (angle below 0.25 degrees),
    /// and segments shorter than 1e-6. Uses the XLine3D endpoint parameter tolerance of 1e-6.
    /// The closest separation must be less than 1e-6. Use Polyline3D.findAllSelfIntersectionsCustom to set this tolerance.</summary>
    /// <param name="pl">The polyline to search.</param>
    /// <returns>A new list of points on the first segment with the indices of the two intersecting segments,
    /// sorted by the first and then by the second segment index. The first index is always smaller than the second.
    /// The list is empty if no intersection is found.</returns>
    static member findAllSelfIntersections (pl:Polyline3D) : ResizeArray<Pnt * int * int> =
        Polyline3D.findAllSelfIntersectionsCustom(pl)

    /// <summary>Finds all intersections between non-adjacent segments in O(n²) time,
    /// with the same rules as Polyline3D.tryFindSelfIntersectionCustom:
    /// Excludes the closure joint when endpoints are within 1e-7, parallel overlaps (angle below 0.25 degrees),
    /// and segments shorter than 1e-6. Uses the XLine3D endpoint parameter tolerance of 1e-6.
    /// The closest separation must be strictly less than maxSkewDistance; zero therefore finds no intersections.
    /// The optional parameter can be given by name. Use Polyline3D.findAllSelfIntersections for the default.</summary>
    /// <param name="pl">The polyline to search.</param>
    /// <param name="maxSkewDistance">Finite, nonnegative separation tolerance; defaults to 1e-6.</param>
    /// <returns>A new list of points on the first segment with the indices of the two intersecting segments,
    /// sorted by the first and then by the second segment index. The first index is always smaller than the second.
    /// The list is empty if no intersection is found.</returns>
    static member findAllSelfIntersectionsCustom(pl:Polyline3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float) : ResizeArray<Pnt * int * int> =
        if maxSkewDistance < 0.0 || isNanInfinity maxSkewDistance then
            fail $"Polyline3D.findAllSelfIntersectionsCustom: maxSkewDistance must be finite and nonnegative, got {maxSkewDistance}."
        let xyzs = pl.XYZs
        let segmentCount = pl.PointCount - 1
        let result = ResizeArray<Pnt * int * int>()
        if segmentCount > 1 then
            let isClosed = pl.IsAlmostClosed 1e-7
            let mutable i = 0
            while i < segmentCount - 2 do
                let a = i * 3
                let ax = xyzs.[a+3] - xyzs.[a]
                let ay = xyzs.[a+4] - xyzs.[a+1]
                let az = xyzs.[a+5] - xyzs.[a+2]
                let last = if i = 0 && isClosed then segmentCount - 2 else segmentCount - 1
                let mutable j = i + 2
                while j <= last do
                    let b = j * 3
                    match XLineXYZ.tryIntersect(
                        xyzs.[a], xyzs.[a+1], xyzs.[a+2], xyzs.[b], xyzs.[b+1], xyzs.[b+2],
                        ax, ay, az, xyzs.[b+3] - xyzs.[b], xyzs.[b+4] - xyzs.[b+1], xyzs.[b+5] - xyzs.[b+2],
                        maxSkewDistance, Tangent.``0.25``, 1e-6) with
                    | ValueSome p -> result.Add((p, i, j))
                    | ValueNone -> ()
                    j <- j + 1
                i <- i + 1
        result


#if !FABLE_COMPILER
/// Serializes a Polyline3D as its interleaved XYZ coordinate buffer with System.Text.Json.
and internal Polyline3DJsonConverter() =
    inherit JsonConverter<Polyline3D>()

    override _.Write(writer, polyline, options) =
        writer.WriteStartObject()
        JsonConvert.writeFloatResizeArray writer options "XYZs" polyline.XYZs
        writer.WriteEndObject()

    override _.Read(reader, _, options) =
        JsonConvert.readStartObject &reader "Euclid.Polyline3D"
        let xyzs = JsonConvert.readFloatResizeArray &reader options "Euclid.Polyline3D" "XYZs"
        JsonConvert.readEndObject &reader "Euclid.Polyline3D"
        Polyline3D.createFromXYZs xyzs
#endif

