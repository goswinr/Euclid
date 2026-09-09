namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid
open EuclidErrors
#if !FABLE_COMPILER
open System.Text.Json.Serialization
#endif


module private FreeBoxUtil =

    /// The number of floats in the flat coordinate buffer of a FreeBox: 8 points times 3 coordinates.
    [<Literal>]
    let CoordinateCount = 24

    let failPointIndex (methodName:string) (idx:int) =
        fail $"FreeBox.{methodName}: point index {idx} is out of range. It must be between 0 and 7."

    let inline getPt (i:int) (xyzs:float[]) : Pnt =
        Pnt(xyzs.[i * 3], xyzs.[i * 3 + 1], xyzs.[i * 3 + 2])

    let inline setCoordXYZ (i:int) (x:float) (y:float) (z:float) (xyzs:float[]) : unit =
        xyzs.[i * 3    ] <- x
        xyzs.[i * 3 + 1] <- y
        xyzs.[i * 3 + 2] <- z

    /// Moves the point at index 'i' by the vector, in place, in the given coordinate array.
    let inline movePtBy (i:int) (v:Vec) (xyzs:float[]) : unit =
        xyzs.[i * 3    ] <- xyzs.[i * 3    ] + v.X
        xyzs.[i * 3 + 1] <- xyzs.[i * 3 + 1] + v.Y
        xyzs.[i * 3 + 2] <- xyzs.[i * 3 + 2] + v.Z

    let inline edge (a:int) (b:int) (xyzs:float[]) : Line3D =
        Line3D(xyzs.[a * 3], xyzs.[a * 3 + 1], xyzs.[a * 3 + 2],
               xyzs.[b * 3], xyzs.[b * 3 + 1], xyzs.[b * 3 + 2])


open FreeBoxUtil


/// <summary>
/// A class containing a flat array of 24 floats representing an arbitrary 3D box-like topology.
/// The 8 corner points are stored interleaved as x0, y0, z0, x1, y1, z1, ... x7, y7, z7.
/// The points can be in arbitrary position in space.
/// <code>
///       7               6
///       +---------------+
///      /|              /|
///     / |             / |
/// 4  /  |          5 /  |
///   +---------------+   |
///   |   |           |   |
///   |   +-----------|---+
///   |  / 3          |  / 2
///   | /             | /
///   |/              |/
///   +---------------+
///   0               1
/// </code>
/// </summary>
[<NoEquality; NoComparison>] // because its made up from floats
[<DataContract>] // for using DataMember on fields, for Newtonsoft.Json
#if !FABLE_COMPILER
[<JsonConverter(typeof<FreeBoxJsonConverter>)>]
#endif
type FreeBox private (xyzs:float[]) =

    /// <summary>Gets the X, Y and Z interleaved array of 24 floats of the box:
    /// x0, y0, z0, x1, y1, z1, ... x7, y7, z7.
    /// This is the live internal buffer, so changes to the array will be reflected in the FreeBox.</summary>
    [<DataMember>]
    member _.XYZs : float[] =
        xyzs

    /// <summary>Gets the X, Y and Z interleaved array of 24 floats of the box:
    /// x0, y0, z0, x1, y1, z1, ... x7, y7, z7.
    /// This is the live internal buffer, so changes to the array will be reflected in the FreeBox.</summary>
    static member inline getXYZs (b:FreeBox) : float[] =
        b.XYZs

    /// Converts the flat float buffer of the FreeBox into an array of the 8 corner points.
    /// Use .XYZs to access the live internal buffer.
    member _.AsPoints : Pnt array =
        [| getPt 0 xyzs; getPt 1 xyzs; getPt 2 xyzs; getPt 3 xyzs
           getPt 4 xyzs; getPt 5 xyzs; getPt 6 xyzs; getPt 7 xyzs |]

    /// Converts the flat float buffer of the FreeBox into an array of the 8 corner points.
    /// Use .XYZs to access the live internal buffer.
    static member inline asPoints (b:FreeBox) : Pnt array =
        b.AsPoints

    /// <summary>Gets or sets the point 0 of the box.
    /// (this gets or sets XYZs.[0], XYZs.[1] and XYZs.[2] of the internal flat array of x, y, and z coordinates)<code>
    ///       7               6
    ///       +---------------+
    ///      /|              /|
    ///     / |             / |
    /// 4  /  |          5 /  |
    ///   +---------------+   |
    ///   |   |           |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/
    ///   +---------------+
    ///   0               1
    /// </code></summary>
    member _.Pt0
        with get() : Pnt = getPt 0 xyzs
        and  set (p:Pnt) = setCoordXYZ 0 p.X p.Y p.Z xyzs

    /// Point 0 of the box.
    static member inline pt0 (b:FreeBox) : Pnt = b.Pt0

    /// Gets or sets the X coordinate of the point 0 of the box.
    /// (this gets or sets XYZs.[0] of the internal flat array of x, y, and z coordinates)
    member _.Pt0X
        with get() : float = xyzs.[0]
        and  set (v:float) = xyzs.[0] <- v

    /// The X coordinate of the point 0 of the box. (this is XYZs.[0])
    static member inline pt0X (b:FreeBox) : float = b.Pt0X

    /// Gets or sets the Y coordinate of the point 0 of the box.
    /// (this gets or sets XYZs.[1] of the internal flat array of x, y, and z coordinates)
    member _.Pt0Y
        with get() : float = xyzs.[1]
        and  set (v:float) = xyzs.[1] <- v

    /// The Y coordinate of the point 0 of the box. (this is XYZs.[1])
    static member inline pt0Y (b:FreeBox) : float = b.Pt0Y

    /// Gets or sets the Z coordinate of the point 0 of the box.
    /// (this gets or sets XYZs.[2] of the internal flat array of x, y, and z coordinates)
    member _.Pt0Z
        with get() : float = xyzs.[2]
        and  set (v:float) = xyzs.[2] <- v

    /// The Z coordinate of the point 0 of the box. (this is XYZs.[2])
    static member inline pt0Z (b:FreeBox) : float = b.Pt0Z

    /// <summary>Gets or sets the point 1 of the box.
    /// (this gets or sets XYZs.[3], XYZs.[4] and XYZs.[5] of the internal flat array of x, y, and z coordinates)<code>
    ///       7               6
    ///       +---------------+
    ///      /|              /|
    ///     / |             / |
    /// 4  /  |          5 /  |
    ///   +---------------+   |
    ///   |   |           |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/
    ///   +---------------+
    ///   0               1
    /// </code></summary>
    member _.Pt1
        with get() : Pnt = getPt 1 xyzs
        and  set (p:Pnt) = setCoordXYZ 1 p.X p.Y p.Z xyzs

    /// Point 1 of the box.
    static member inline pt1 (b:FreeBox) : Pnt = b.Pt1

    /// Gets or sets the X coordinate of the point 1 of the box.
    /// (this gets or sets XYZs.[3] of the internal flat array of x, y, and z coordinates)
    member _.Pt1X
        with get() : float = xyzs.[3]
        and  set (v:float) = xyzs.[3] <- v

    /// The X coordinate of the point 1 of the box. (this is XYZs.[3])
    static member inline pt1X (b:FreeBox) : float = b.Pt1X

    /// Gets or sets the Y coordinate of the point 1 of the box.
    /// (this gets or sets XYZs.[4] of the internal flat array of x, y, and z coordinates)
    member _.Pt1Y
        with get() : float = xyzs.[4]
        and  set (v:float) = xyzs.[4] <- v

    /// The Y coordinate of the point 1 of the box. (this is XYZs.[4])
    static member inline pt1Y (b:FreeBox) : float = b.Pt1Y

    /// Gets or sets the Z coordinate of the point 1 of the box.
    /// (this gets or sets XYZs.[5] of the internal flat array of x, y, and z coordinates)
    member _.Pt1Z
        with get() : float = xyzs.[5]
        and  set (v:float) = xyzs.[5] <- v

    /// The Z coordinate of the point 1 of the box. (this is XYZs.[5])
    static member inline pt1Z (b:FreeBox) : float = b.Pt1Z

    /// <summary>Gets or sets the point 2 of the box.
    /// (this gets or sets XYZs.[6], XYZs.[7] and XYZs.[8] of the internal flat array of x, y, and z coordinates)<code>
    ///       7               6
    ///       +---------------+
    ///      /|              /|
    ///     / |             / |
    /// 4  /  |          5 /  |
    ///   +---------------+   |
    ///   |   |           |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/
    ///   +---------------+
    ///   0               1
    /// </code></summary>
    member _.Pt2
        with get() : Pnt = getPt 2 xyzs
        and  set (p:Pnt) = setCoordXYZ 2 p.X p.Y p.Z xyzs

    /// Point 2 of the box.
    static member inline pt2 (b:FreeBox) : Pnt = b.Pt2

    /// Gets or sets the X coordinate of the point 2 of the box.
    /// (this gets or sets XYZs.[6] of the internal flat array of x, y, and z coordinates)
    member _.Pt2X
        with get() : float = xyzs.[6]
        and  set (v:float) = xyzs.[6] <- v

    /// The X coordinate of the point 2 of the box. (this is XYZs.[6])
    static member inline pt2X (b:FreeBox) : float = b.Pt2X

    /// Gets or sets the Y coordinate of the point 2 of the box.
    /// (this gets or sets XYZs.[7] of the internal flat array of x, y, and z coordinates)
    member _.Pt2Y
        with get() : float = xyzs.[7]
        and  set (v:float) = xyzs.[7] <- v

    /// The Y coordinate of the point 2 of the box. (this is XYZs.[7])
    static member inline pt2Y (b:FreeBox) : float = b.Pt2Y

    /// Gets or sets the Z coordinate of the point 2 of the box.
    /// (this gets or sets XYZs.[8] of the internal flat array of x, y, and z coordinates)
    member _.Pt2Z
        with get() : float = xyzs.[8]
        and  set (v:float) = xyzs.[8] <- v

    /// The Z coordinate of the point 2 of the box. (this is XYZs.[8])
    static member inline pt2Z (b:FreeBox) : float = b.Pt2Z

    /// <summary>Gets or sets the point 3 of the box.
    /// (this gets or sets XYZs.[9], XYZs.[10] and XYZs.[11] of the internal flat array of x, y, and z coordinates)<code>
    ///       7               6
    ///       +---------------+
    ///      /|              /|
    ///     / |             / |
    /// 4  /  |          5 /  |
    ///   +---------------+   |
    ///   |   |           |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/
    ///   +---------------+
    ///   0               1
    /// </code></summary>
    member _.Pt3
        with get() : Pnt = getPt 3 xyzs
        and  set (p:Pnt) = setCoordXYZ 3 p.X p.Y p.Z xyzs

    /// Point 3 of the box.
    static member inline pt3 (b:FreeBox) : Pnt = b.Pt3

    /// Gets or sets the X coordinate of the point 3 of the box.
    /// (this gets or sets XYZs.[9] of the internal flat array of x, y, and z coordinates)
    member _.Pt3X
        with get() : float = xyzs.[9]
        and  set (v:float) = xyzs.[9] <- v

    /// The X coordinate of the point 3 of the box. (this is XYZs.[9])
    static member inline pt3X (b:FreeBox) : float = b.Pt3X

    /// Gets or sets the Y coordinate of the point 3 of the box.
    /// (this gets or sets XYZs.[10] of the internal flat array of x, y, and z coordinates)
    member _.Pt3Y
        with get() : float = xyzs.[10]
        and  set (v:float) = xyzs.[10] <- v

    /// The Y coordinate of the point 3 of the box. (this is XYZs.[10])
    static member inline pt3Y (b:FreeBox) : float = b.Pt3Y

    /// Gets or sets the Z coordinate of the point 3 of the box.
    /// (this gets or sets XYZs.[11] of the internal flat array of x, y, and z coordinates)
    member _.Pt3Z
        with get() : float = xyzs.[11]
        and  set (v:float) = xyzs.[11] <- v

    /// The Z coordinate of the point 3 of the box. (this is XYZs.[11])
    static member inline pt3Z (b:FreeBox) : float = b.Pt3Z

    /// <summary>Gets or sets the point 4 of the box.
    /// (this gets or sets XYZs.[12], XYZs.[13] and XYZs.[14] of the internal flat array of x, y, and z coordinates)<code>
    ///       7               6
    ///       +---------------+
    ///      /|              /|
    ///     / |             / |
    /// 4  /  |          5 /  |
    ///   +---------------+   |
    ///   |   |           |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/
    ///   +---------------+
    ///   0               1
    /// </code></summary>
    member _.Pt4
        with get() : Pnt = getPt 4 xyzs
        and  set (p:Pnt) = setCoordXYZ 4 p.X p.Y p.Z xyzs

    /// Point 4 of the box.
    static member inline pt4 (b:FreeBox) : Pnt = b.Pt4

    /// Gets or sets the X coordinate of the point 4 of the box.
    /// (this gets or sets XYZs.[12] of the internal flat array of x, y, and z coordinates)
    member _.Pt4X
        with get() : float = xyzs.[12]
        and  set (v:float) = xyzs.[12] <- v

    /// The X coordinate of the point 4 of the box. (this is XYZs.[12])
    static member inline pt4X (b:FreeBox) : float = b.Pt4X

    /// Gets or sets the Y coordinate of the point 4 of the box.
    /// (this gets or sets XYZs.[13] of the internal flat array of x, y, and z coordinates)
    member _.Pt4Y
        with get() : float = xyzs.[13]
        and  set (v:float) = xyzs.[13] <- v

    /// The Y coordinate of the point 4 of the box. (this is XYZs.[13])
    static member inline pt4Y (b:FreeBox) : float = b.Pt4Y

    /// Gets or sets the Z coordinate of the point 4 of the box.
    /// (this gets or sets XYZs.[14] of the internal flat array of x, y, and z coordinates)
    member _.Pt4Z
        with get() : float = xyzs.[14]
        and  set (v:float) = xyzs.[14] <- v

    /// The Z coordinate of the point 4 of the box. (this is XYZs.[14])
    static member inline pt4Z (b:FreeBox) : float = b.Pt4Z

    /// <summary>Gets or sets the point 5 of the box.
    /// (this gets or sets XYZs.[15], XYZs.[16] and XYZs.[17] of the internal flat array of x, y, and z coordinates)<code>
    ///       7               6
    ///       +---------------+
    ///      /|              /|
    ///     / |             / |
    /// 4  /  |          5 /  |
    ///   +---------------+   |
    ///   |   |           |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/
    ///   +---------------+
    ///   0               1
    /// </code></summary>
    member _.Pt5
        with get() : Pnt = getPt 5 xyzs
        and  set (p:Pnt) = setCoordXYZ 5 p.X p.Y p.Z xyzs

    /// Point 5 of the box.
    static member inline pt5 (b:FreeBox) : Pnt = b.Pt5

    /// Gets or sets the X coordinate of the point 5 of the box.
    /// (this gets or sets XYZs.[15] of the internal flat array of x, y, and z coordinates)
    member _.Pt5X
        with get() : float = xyzs.[15]
        and  set (v:float) = xyzs.[15] <- v

    /// The X coordinate of the point 5 of the box. (this is XYZs.[15])
    static member inline pt5X (b:FreeBox) : float = b.Pt5X

    /// Gets or sets the Y coordinate of the point 5 of the box.
    /// (this gets or sets XYZs.[16] of the internal flat array of x, y, and z coordinates)
    member _.Pt5Y
        with get() : float = xyzs.[16]
        and  set (v:float) = xyzs.[16] <- v

    /// The Y coordinate of the point 5 of the box. (this is XYZs.[16])
    static member inline pt5Y (b:FreeBox) : float = b.Pt5Y

    /// Gets or sets the Z coordinate of the point 5 of the box.
    /// (this gets or sets XYZs.[17] of the internal flat array of x, y, and z coordinates)
    member _.Pt5Z
        with get() : float = xyzs.[17]
        and  set (v:float) = xyzs.[17] <- v

    /// The Z coordinate of the point 5 of the box. (this is XYZs.[17])
    static member inline pt5Z (b:FreeBox) : float = b.Pt5Z

    /// <summary>Gets or sets the point 6 of the box.
    /// (this gets or sets XYZs.[18], XYZs.[19] and XYZs.[20] of the internal flat array of x, y, and z coordinates)<code>
    ///       7               6
    ///       +---------------+
    ///      /|              /|
    ///     / |             / |
    /// 4  /  |          5 /  |
    ///   +---------------+   |
    ///   |   |           |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/
    ///   +---------------+
    ///   0               1
    /// </code></summary>
    member _.Pt6
        with get() : Pnt = getPt 6 xyzs
        and  set (p:Pnt) = setCoordXYZ 6 p.X p.Y p.Z xyzs

    /// Point 6 of the box.
    static member inline pt6 (b:FreeBox) : Pnt = b.Pt6

    /// Gets or sets the X coordinate of the point 6 of the box.
    /// (this gets or sets XYZs.[18] of the internal flat array of x, y, and z coordinates)
    member _.Pt6X
        with get() : float = xyzs.[18]
        and  set (v:float) = xyzs.[18] <- v

    /// The X coordinate of the point 6 of the box. (this is XYZs.[18])
    static member inline pt6X (b:FreeBox) : float = b.Pt6X

    /// Gets or sets the Y coordinate of the point 6 of the box.
    /// (this gets or sets XYZs.[19] of the internal flat array of x, y, and z coordinates)
    member _.Pt6Y
        with get() : float = xyzs.[19]
        and  set (v:float) = xyzs.[19] <- v

    /// The Y coordinate of the point 6 of the box. (this is XYZs.[19])
    static member inline pt6Y (b:FreeBox) : float = b.Pt6Y

    /// Gets or sets the Z coordinate of the point 6 of the box.
    /// (this gets or sets XYZs.[20] of the internal flat array of x, y, and z coordinates)
    member _.Pt6Z
        with get() : float = xyzs.[20]
        and  set (v:float) = xyzs.[20] <- v

    /// The Z coordinate of the point 6 of the box. (this is XYZs.[20])
    static member inline pt6Z (b:FreeBox) : float = b.Pt6Z

    /// <summary>Gets or sets the point 7 of the box.
    /// (this gets or sets XYZs.[21], XYZs.[22] and XYZs.[23] of the internal flat array of x, y, and z coordinates)<code>
    ///       7               6
    ///       +---------------+
    ///      /|              /|
    ///     / |             / |
    /// 4  /  |          5 /  |
    ///   +---------------+   |
    ///   |   |           |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/
    ///   +---------------+
    ///   0               1
    /// </code></summary>
    member _.Pt7
        with get() : Pnt = getPt 7 xyzs
        and  set (p:Pnt) = setCoordXYZ 7 p.X p.Y p.Z xyzs

    /// Point 7 of the box.
    static member inline pt7 (b:FreeBox) : Pnt = b.Pt7

    /// Gets or sets the X coordinate of the point 7 of the box.
    /// (this gets or sets XYZs.[21] of the internal flat array of x, y, and z coordinates)
    member _.Pt7X
        with get() : float = xyzs.[21]
        and  set (v:float) = xyzs.[21] <- v

    /// The X coordinate of the point 7 of the box. (this is XYZs.[21])
    static member inline pt7X (b:FreeBox) : float = b.Pt7X

    /// Gets or sets the Y coordinate of the point 7 of the box.
    /// (this gets or sets XYZs.[22] of the internal flat array of x, y, and z coordinates)
    member _.Pt7Y
        with get() : float = xyzs.[22]
        and  set (v:float) = xyzs.[22] <- v

    /// The Y coordinate of the point 7 of the box. (this is XYZs.[22])
    static member inline pt7Y (b:FreeBox) : float = b.Pt7Y

    /// Gets or sets the Z coordinate of the point 7 of the box.
    /// (this gets or sets XYZs.[23] of the internal flat array of x, y, and z coordinates)
    member _.Pt7Z
        with get() : float = xyzs.[23]
        and  set (v:float) = xyzs.[23] <- v

    /// The Z coordinate of the point 7 of the box. (this is XYZs.[23])
    static member inline pt7Z (b:FreeBox) : float = b.Pt7Z


    #nowarn "52" // The value has been copied to ensure the original is not mutated by this operation

    /// Nicely formatted string representation of the box including Pt0 and Pt7.
    override b.ToString() : string =
        $"Euclid.FreeBox pt0 at {b.Pt0.AsString}, Pt7 at {b.Pt7.AsString}"

    /// Nicely formatted string representation of the box including Pt0.
    member b.AsString : string =
        $"FreeBox at {b.Pt0.AsString}"

    #warnon "52" // reenable warning

    /// Nicely formatted string representation of the box including Pt0.
    static member inline asString (b:FreeBox) : string = b.AsString

    /// Format FreeBox into an F# code string that can be used to recreate the box.
    member b.AsFSharpCode : string =
        let ps = b.AsPoints
        $"FreeBox.createFromEightPoints([| {ps[0].AsFSharpCode}; {ps[1].AsFSharpCode}; {ps[2].AsFSharpCode}; {ps[3].AsFSharpCode}; {ps[4].AsFSharpCode}; {ps[5].AsFSharpCode}; {ps[6].AsFSharpCode}; {ps[7].AsFSharpCode} |])"

    /// Format FreeBox into an F# code string that can be used to recreate the box.
    static member inline asFSharpCode (b:FreeBox) : string = b.AsFSharpCode


    /// Get the x coordinate of the point at the given point index. The index must be between 0 and 7.
    /// (this gets XYZs.[pointIndex * 3] from the internal flat array of x, y, and z coordinates)
    member _.GetX (pointIndex:int) : float =
        if pointIndex < 0 || pointIndex > 7 then
            failPointIndex "GetX" pointIndex
        xyzs.[pointIndex * 3]

    /// Get the x coordinate of the point at the given point index. The index must be between 0 and 7.
    /// (this gets XYZs.[pointIndex * 3] from the internal flat array of x, y, and z coordinates)
    static member inline getX (pointIndex:int) (b:FreeBox) : float =
        b.GetX pointIndex

    /// Get the y coordinate of the point at the given point index. The index must be between 0 and 7.
    /// (this gets XYZs.[pointIndex * 3 + 1] from the internal flat array of x, y, and z coordinates)
    member _.GetY (pointIndex:int) : float =
        if pointIndex < 0 || pointIndex > 7 then
            failPointIndex "GetY" pointIndex
        xyzs.[pointIndex * 3 + 1]

    /// Get the y coordinate of the point at the given point index. The index must be between 0 and 7.
    /// (this gets XYZs.[pointIndex * 3 + 1] from the internal flat array of x, y, and z coordinates)
    static member inline getY (pointIndex:int) (b:FreeBox) : float =
        b.GetY pointIndex

    /// Get the z coordinate of the point at the given point index. The index must be between 0 and 7.
    /// (this gets XYZs.[pointIndex * 3 + 2] from the internal flat array of x, y, and z coordinates)
    member _.GetZ (pointIndex:int) : float =
        if pointIndex < 0 || pointIndex > 7 then
            failPointIndex "GetZ" pointIndex
        xyzs.[pointIndex * 3 + 2]

    /// Get the z coordinate of the point at the given point index. The index must be between 0 and 7.
    /// (this gets XYZs.[pointIndex * 3 + 2] from the internal flat array of x, y, and z coordinates)
    static member inline getZ (pointIndex:int) (b:FreeBox) : float =
        b.GetZ pointIndex

    /// Get the point at the given point index. The index must be between 0 and 7.
    /// (this gets Pnt(XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], XYZs.[pointIndex * 3 + 2]) from the internal flat array of x, y, and z coordinates)
    member _.GetPt (pointIndex:int) : Pnt =
        if pointIndex < 0 || pointIndex > 7 then
            failPointIndex "GetPt" pointIndex
        getPt pointIndex xyzs

    /// Get the point at the given point index. The index must be between 0 and 7.
    /// (this gets Pnt(XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1], XYZs.[pointIndex * 3 + 2]) from the internal flat array of x, y, and z coordinates)
    static member inline getPt (pointIndex:int) (b:FreeBox) : Pnt =
        b.GetPt(pointIndex)

    /// Sets the point at the given point index. The index must be between 0 and 7.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1] and XYZs.[pointIndex * 3 + 2] of the internal flat array of x, y, and z coordinates)
    member _.SetPt (pointIndex:int) (p:Pnt) : unit =
        if pointIndex < 0 || pointIndex > 7 then
            failPointIndex "SetPt" pointIndex
        setCoordXYZ pointIndex p.X p.Y p.Z xyzs

    /// Sets the point at the given point index. The index must be between 0 and 7.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1] and XYZs.[pointIndex * 3 + 2] of the internal flat array of x, y, and z coordinates)
    static member inline setPt (pointIndex:int) (p:Pnt) (b:FreeBox) : unit =
        b.SetPt pointIndex p

    /// Sets the x, y, and z coordinates of the point at the given point index. The index must be between 0 and 7.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1] and XYZs.[pointIndex * 3 + 2] of the internal flat array of x, y, and z coordinates)
    member _.SetPtXYZ (pointIndex:int, x:float, y:float, z:float) : unit =
        if pointIndex < 0 || pointIndex > 7 then
            failPointIndex "SetPtXYZ" pointIndex
        setCoordXYZ pointIndex x y z xyzs

    /// Sets the x, y, and z coordinates of the point at the given point index. The index must be between 0 and 7.
    /// (this sets XYZs.[pointIndex * 3], XYZs.[pointIndex * 3 + 1] and XYZs.[pointIndex * 3 + 2] of the internal flat array of x, y, and z coordinates)
    static member inline setPtXYZ (pointIndex:int) (x:float) (y:float) (z:float) (b:FreeBox) : unit =
        b.SetPtXYZ(pointIndex, x, y, z)

    /// Creates a copy of the FreeBox with its own flat array of 24 floats.
    member _.Duplicate() : FreeBox =
        FreeBox(Array.copy xyzs)

    /// Creates a copy of the FreeBox with its own flat array of 24 floats.
    static member inline duplicate (b:FreeBox) : FreeBox =
        b.Duplicate()

    /// Scales the FreeBox by a given factor on the world origin (0, 0, 0).
    member _.Scale (factor:float) : FreeBox =
        let r = Array.zeroCreate<float> CoordinateCount
        for i = 0 to CoordinateCount - 1 do
            r.[i] <- xyzs.[i] * factor
        FreeBox r

    /// Scales the FreeBox by a given factor.
    /// Scale center is World Origin 0,0,0
    static member inline scale (factor:float) (b:FreeBox) : FreeBox =
        b.Scale(factor)

    /// Scales the FreeBox by a given factor on a given center point.
    member _.ScaleOn (cen:Pnt, factor:float) : FreeBox =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        let r = Array.zeroCreate<float> CoordinateCount
        let mutable i = 0
        while i < CoordinateCount do
            r.[i    ] <- cx + (xyzs.[i    ] - cx) * factor
            r.[i + 1] <- cy + (xyzs.[i + 1] - cy) * factor
            r.[i + 2] <- cz + (xyzs.[i + 2] - cz) * factor
            i <- i + 3
        FreeBox r

    /// Scales the FreeBox by a given factor on a given center point.
    static member inline scaleOn (cen:Pnt)  (factor:float) (b:FreeBox) : FreeBox =
        b.ScaleOn(cen, factor)

    /// Returns a FreeBox moved by a vector. Same as FreeBox.move and FreeBox.translate.
    member _.Move (v:Vec) : FreeBox =
        let vx = v.X
        let vy = v.Y
        let vz = v.Z
        let r = Array.zeroCreate<float> CoordinateCount
        let mutable i = 0
        while i < CoordinateCount do
            r.[i    ] <- xyzs.[i    ] + vx
            r.[i + 1] <- xyzs.[i + 1] + vy
            r.[i + 2] <- xyzs.[i + 2] + vz
            i <- i + 3
        FreeBox r

    /// Move a FreeBox by a vector. Same as FreeBox.translate.
    static member inline move (v:Vec) (b:FreeBox) : FreeBox =
        b.Move(v)

    /// Returns a FreeBox moved by a given distance in world X direction.
    member _.MoveX (distance:float) : FreeBox =
        let r = Array.copy xyzs
        let mutable i = 0
        while i < CoordinateCount do
            r.[i] <- r.[i] + distance
            i <- i + 3
        FreeBox r

    /// Returns the FreeBox moved by a given distance in world X direction.
    static member inline moveX (distance:float) (b:FreeBox) : FreeBox =
        b.MoveX(distance)

    /// Returns a FreeBox moved by a given distance in Y direction.
    member _.MoveY (distance:float) : FreeBox =
        let r = Array.copy xyzs
        let mutable i = 1
        while i < CoordinateCount do
            r.[i] <- r.[i] + distance
            i <- i + 3
        FreeBox r

    /// Returns the FreeBox moved by a given distance in world Y direction.
    static member inline moveY (distance:float) (b:FreeBox) : FreeBox =
        b.MoveY(distance)

    /// Returns a FreeBox moved by a given distance in world Z direction.
    member _.MoveZ (distance:float) : FreeBox =
        let r = Array.copy xyzs
        let mutable i = 2
        while i < CoordinateCount do
            r.[i] <- r.[i] + distance
            i <- i + 3
        FreeBox r

    /// Returns the FreeBox moved by a given distance in world Z direction.
    static member inline moveZ (distance:float) (b:FreeBox) : FreeBox =
        b.MoveZ(distance)

    /// Moves the point at the given point index by a vector. The index must be between 0 and 7.
    /// NOTE: unlike Move, MoveX, MoveY and MoveZ, that return a new FreeBox, this mutates this FreeBox in place.
    /// All other points stay where they are.
    member _.MovePt (pointIndex:int, v:Vec) : unit =
        if pointIndex < 0 || pointIndex > 7 then
            failPointIndex "MovePt" pointIndex
        movePtBy pointIndex v xyzs

    /// Moves the point at the given point index by a vector. The index must be between 0 and 7.
    /// NOTE: unlike FreeBox.move, moveX, moveY and moveZ, that return a new FreeBox, this mutates the given FreeBox in place.
    /// All other points stay where they are.
    static member inline movePt (pointIndex:int) (v:Vec) (b:FreeBox) : unit =
        b.MovePt(pointIndex, v)

    /// Moves the point 0 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike Move, MoveX, MoveY and MoveZ, that return a new FreeBox, this mutates this FreeBox in place.
    member _.MovePt0 (v:Vec) : unit =
        movePtBy 0 v xyzs

    /// Moves the point 0 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike FreeBox.move, moveX, moveY and moveZ, that return a new FreeBox, this mutates the given FreeBox in place.
    static member inline movePt0 (v:Vec) (b:FreeBox) : unit =
        b.MovePt0(v)

    /// Moves the point 1 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike Move, MoveX, MoveY and MoveZ, that return a new FreeBox, this mutates this FreeBox in place.
    member _.MovePt1 (v:Vec) : unit =
        movePtBy 1 v xyzs

    /// Moves the point 1 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike FreeBox.move, moveX, moveY and moveZ, that return a new FreeBox, this mutates the given FreeBox in place.
    static member inline movePt1 (v:Vec) (b:FreeBox) : unit =
        b.MovePt1(v)

    /// Moves the point 2 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike Move, MoveX, MoveY and MoveZ, that return a new FreeBox, this mutates this FreeBox in place.
    member _.MovePt2 (v:Vec) : unit =
        movePtBy 2 v xyzs

    /// Moves the point 2 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike FreeBox.move, moveX, moveY and moveZ, that return a new FreeBox, this mutates the given FreeBox in place.
    static member inline movePt2 (v:Vec) (b:FreeBox) : unit =
        b.MovePt2(v)

    /// Moves the point 3 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike Move, MoveX, MoveY and MoveZ, that return a new FreeBox, this mutates this FreeBox in place.
    member _.MovePt3 (v:Vec) : unit =
        movePtBy 3 v xyzs

    /// Moves the point 3 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike FreeBox.move, moveX, moveY and moveZ, that return a new FreeBox, this mutates the given FreeBox in place.
    static member inline movePt3 (v:Vec) (b:FreeBox) : unit =
        b.MovePt3(v)

    /// Moves the point 4 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike Move, MoveX, MoveY and MoveZ, that return a new FreeBox, this mutates this FreeBox in place.
    member _.MovePt4 (v:Vec) : unit =
        movePtBy 4 v xyzs

    /// Moves the point 4 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike FreeBox.move, moveX, moveY and moveZ, that return a new FreeBox, this mutates the given FreeBox in place.
    static member inline movePt4 (v:Vec) (b:FreeBox) : unit =
        b.MovePt4(v)

    /// Moves the point 5 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike Move, MoveX, MoveY and MoveZ, that return a new FreeBox, this mutates this FreeBox in place.
    member _.MovePt5 (v:Vec) : unit =
        movePtBy 5 v xyzs

    /// Moves the point 5 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike FreeBox.move, moveX, moveY and moveZ, that return a new FreeBox, this mutates the given FreeBox in place.
    static member inline movePt5 (v:Vec) (b:FreeBox) : unit =
        b.MovePt5(v)

    /// Moves the point 6 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike Move, MoveX, MoveY and MoveZ, that return a new FreeBox, this mutates this FreeBox in place.
    member _.MovePt6 (v:Vec) : unit =
        movePtBy 6 v xyzs

    /// Moves the point 6 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike FreeBox.move, moveX, moveY and moveZ, that return a new FreeBox, this mutates the given FreeBox in place.
    static member inline movePt6 (v:Vec) (b:FreeBox) : unit =
        b.MovePt6(v)

    /// Moves the point 7 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike Move, MoveX, MoveY and MoveZ, that return a new FreeBox, this mutates this FreeBox in place.
    member _.MovePt7 (v:Vec) : unit =
        movePtBy 7 v xyzs

    /// Moves the point 7 of the box by a vector. All other points stay where they are.
    /// NOTE: unlike FreeBox.move, moveX, moveY and moveZ, that return a new FreeBox, this mutates the given FreeBox in place.
    static member inline movePt7 (v:Vec) (b:FreeBox) : unit =
        b.MovePt7(v)

    /// Applies or multiplies a 4x4 transformation matrix to the FreeBox.
    member _.Transform (m:Matrix) : FreeBox =
        let r = Array.zeroCreate<float> CoordinateCount
        let mutable i = 0
        while i < CoordinateCount do
            let x = xyzs.[i    ]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            let x' = m.M11*x + m.M21*y + m.M31*z + m.X41 // * w (= 1.0)
            let y' = m.M12*x + m.M22*y + m.M32*z + m.Y42 // * w
            let z' = m.M13*x + m.M23*y + m.M33*z + m.Z43 // * w
            let w' = m.M14*x + m.M24*y + m.M34*z + m.M44 // * w
            let sc = 1.0 / w'
            r.[i    ] <- x' * sc
            r.[i + 1] <- y' * sc
            r.[i + 2] <- z' * sc
            i <- i + 3
        FreeBox r

    /// Applies or multiplies a 4x4 transformation matrix to the FreeBox.
    static member inline transform (m:Matrix) (b:FreeBox) : FreeBox =
        b.Transform(m)

    /// Multiplies (or applies) a RigidMatrix to the FreeBox.
    member _.TransformRigid (m:RigidMatrix) : FreeBox =
        let r = Array.zeroCreate<float> CoordinateCount
        let mutable i = 0
        while i < CoordinateCount do
            let x = xyzs.[i    ]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            r.[i    ] <- m.M11*x + m.M21*y + m.M31*z + m.X41
            r.[i + 1] <- m.M12*x + m.M22*y + m.M32*z + m.Y42
            r.[i + 2] <- m.M13*x + m.M23*y + m.M33*z + m.Z43
            i <- i + 3
        FreeBox r

    /// Multiplies (or applies) a RigidMatrix to the FreeBox.
    static member inline transformRigid (m:RigidMatrix) (b:FreeBox) : FreeBox =
        b.TransformRigid(m)

    /// Multiplies (or applies) a Quaternion to the FreeBox.
    /// The box is rotated around the World Origin.
    member _.Rotate (q:Quaternion) : FreeBox =
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let r = Array.zeroCreate<float> CoordinateCount
        let mutable i = 0
        while i < CoordinateCount do
            let x = xyzs.[i    ]
            let y = xyzs.[i + 1]
            let z = xyzs.[i + 2]
            let tx = 2.0 * (qy * z - qz * y)
            let ty = 2.0 * (qz * x - qx * z)
            let tz = 2.0 * (qx * y - qy * x)
            // v + q.w * t + cross(q.xyz, t)
            r.[i    ] <- x + qw * tx + qy * tz - qz * ty
            r.[i + 1] <- y + qw * ty + qz * tx - qx * tz
            r.[i + 2] <- z + qw * tz + qx * ty - qy * tx
            i <- i + 3
        FreeBox r

    /// Multiplies (or applies) a Quaternion to the FreeBox.
    /// The box is rotated around the World Origin.
    static member inline rotate (q:Quaternion) (b:FreeBox) : FreeBox =
        b.Rotate(q)

    /// Multiplies (or applies) a Quaternion to the FreeBox around a given center point.
    member _.RotateWithCenter (cen:Pnt, q:Quaternion) : FreeBox =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let r = Array.zeroCreate<float> CoordinateCount
        let mutable i = 0
        while i < CoordinateCount do
            let x = xyzs.[i    ] - cx
            let y = xyzs.[i + 1] - cy
            let z = xyzs.[i + 2] - cz
            let tx = 2.0 * (qy * z - qz * y)
            let ty = 2.0 * (qz * x - qx * z)
            let tz = 2.0 * (qx * y - qy * x)
            // v + q.w * t + cross(q.xyz, t)
            r.[i    ] <- x + qw * tx + qy * tz - qz * ty + cx
            r.[i + 1] <- y + qw * ty + qz * tx - qx * tz + cy
            r.[i + 2] <- z + qw * tz + qx * ty - qy * tx + cz
            i <- i + 3
        FreeBox r

    /// Multiplies (or applies) a Quaternion to the FreeBox around a given center point.
    static member inline rotateWithCenter (cen:Pnt) (q:Quaternion) (b:FreeBox) : FreeBox =
        b.RotateWithCenter(cen, q)

    /// <summary>Creates a FreeBox from a flat array of 24 floats:
    /// x0, y0, z0, x1, y1, z1, ... x7, y7, z7.
    /// The array is used directly as the internal buffer, it is not copied.</summary>
    static member createDirectly (xyzs:float[]) : FreeBox =
        if isNull xyzs then
            failNull "FreeBox.createDirectly" "xyzs"
        if xyzs.Length <> CoordinateCount then
            fail $"FreeBox.createDirectly: the coordinate array must have {CoordinateCount} floats, but it has {xyzs.Length}."
        FreeBox xyzs

    /// Creates a FreeBox from an array of 8 points. The points can be in arbitrary position in space.
    /// The points are copied into a flat array of 24 floats.
    static member createFromEightPoints (pts:Pnt[]) : FreeBox =
        if isNull pts then
            failNull "FreeBox.createFromEightPoints" "pts"
        if pts.Length <> 8 then
            fail $"FreeBox.createFromEightPoints must be initialized with 8 points, but got {pts.Length}."
        let xyzs = Array.zeroCreate<float> CoordinateCount
        for i = 0 to 7 do
            setCoordXYZ i pts.[i].X pts.[i].Y pts.[i].Z xyzs
        FreeBox xyzs

    /// Creates a FreeBox from a Box. The 8 points of the FreeBox are the 8 corners of the Box.
    static member createFromBox (box:Box) : FreeBox =
        let xyzs = Array.zeroCreate<float> CoordinateCount
        let p0 = box.Pt0
        let p1 = box.Pt1
        let p2 = box.Pt2
        let p3 = box.Pt3
        let p4 = box.Pt4
        let p5 = box.Pt5
        let p6 = box.Pt6
        let p7 = box.Pt7
        setCoordXYZ 0 p0.X p0.Y p0.Z xyzs
        setCoordXYZ 1 p1.X p1.Y p1.Z xyzs
        setCoordXYZ 2 p2.X p2.Y p2.Z xyzs
        setCoordXYZ 3 p3.X p3.Y p3.Z xyzs
        setCoordXYZ 4 p4.X p4.Y p4.Z xyzs
        setCoordXYZ 5 p5.X p5.Y p5.Z xyzs
        setCoordXYZ 6 p6.X p6.Y p6.Z xyzs
        setCoordXYZ 7 p7.X p7.Y p7.Z xyzs
        FreeBox xyzs

    /// Creates a FreeBox from four 2D points and a zMin and zMax value.
    static member createFromFour2DPoints (zMin:float) (zMax:float) (pts:Pt[]) : FreeBox =
        if isNull pts then
            failNull "FreeBox.createFromFour2DPoints" "pts"
        if pts.Length <> 4 then
            fail $"FreeBox.createFromFour2DPoints must be initialized with 4 points, but got {pts.Length}."
        let xyzs = Array.zeroCreate<float> CoordinateCount
        for i = 0 to 3 do
            let p = pts.[i]
            setCoordXYZ  i      p.X p.Y zMin xyzs
            setCoordXYZ (i + 4) p.X p.Y zMax xyzs
        FreeBox xyzs

    /// Creates a FreeBox from four 2D points in counter-clockwise order and a zMin and zMax value.
    static member createFromFour2DPointsArgs (a:Pt, b:Pt, c:Pt, d:Pt, zMin:float, zMax:float) : FreeBox =
        //       7               6
        //       +---------------+
        //      /|              /|
        //     / |             / |
        // 4  /  |          5 /  |
        //   +---------------+   |
        //   |   |           |   |
        //   |   +-----------|---+
        //   |  / 3          |  / 2
        //   | /             | /
        //   |/              |/
        //   +---------------+
        //   0               1
        let xyzs = Array.zeroCreate<float> CoordinateCount
        setCoordXYZ 0 a.X a.Y zMin xyzs
        setCoordXYZ 1 b.X b.Y zMin xyzs
        setCoordXYZ 2 c.X c.Y zMin xyzs
        setCoordXYZ 3 d.X d.Y zMin xyzs
        setCoordXYZ 4 a.X a.Y zMax xyzs
        setCoordXYZ 5 b.X b.Y zMax xyzs
        setCoordXYZ 6 c.X c.Y zMax xyzs
        setCoordXYZ 7 d.X d.Y zMax xyzs
        FreeBox xyzs

    /// Translate a FreeBox by a vector. Same as FreeBox.move.
    static member inline translate (v:Vec) (b:FreeBox) : FreeBox =
        b.Move(v)


    /// <summary>Returns the 12 box edges.
    /// Pairs in this order:
    /// 0-1, 1-2, 3-2, 0-3, 0-4, 1-5, 2-6, 3-7, 4-5, 5-6, 7-6, 4-7
    /// <code>
    ///       7               6
    ///       +---------------+
    ///      /|              /|
    ///     / |             / |
    /// 4  /  |          5 /  |
    ///   +---------------+   |
    ///   |   |           |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/
    ///   +---------------+
    ///   0               1
    /// </code>
    /// </summary>
    member b.Edges :Line3D[] =
        [| b.Edge01; b.Edge12; b.Edge32; b.Edge03; // bottom face
           b.Edge04; b.Edge15; b.Edge26; b.Edge37; // vertical edges
           b.Edge45; b.Edge56; b.Edge76; b.Edge47 |] // top face

    /// Returns the 12 box edges.
    static member inline edges (b:FreeBox) : Line3D[] =
        b.Edges



    /// Returns the edge between point 0 and point 1.
    member _.Edge01 :Line3D =
        edge 0 1 xyzs

    /// Returns the edge between point 0 and point 1.
    static member inline edge01 (b:FreeBox) :Line3D =
        b.Edge01

    /// Returns the edge between point 1 and point 2.
    member _.Edge12 :Line3D =
        edge 1 2 xyzs

    /// Returns the edge between point 1 and point 2.
    static member inline edge12 (b:FreeBox) :Line3D =
        b.Edge12

    /// Returns the edge between point 3 and point 2.
    member _.Edge32 :Line3D =
        edge 3 2 xyzs

    /// Returns the edge between point 3 and point 2.
    static member inline edge32 (b:FreeBox) :Line3D =
        b.Edge32

    /// Returns the edge between point 0 and point 3.
    member _.Edge03 :Line3D =
        edge 0 3 xyzs

    /// Returns the edge between point 0 and point 3.
    static member inline edge03 (b:FreeBox) :Line3D =
        b.Edge03

    /// Returns the edge between point 0 and point 4.
    member _.Edge04 :Line3D =
        edge 0 4 xyzs

    /// Returns the edge between point 0 and point 4.
    static member inline edge04 (b:FreeBox) :Line3D =
        b.Edge04

    /// Returns the edge between point 1 and point 5.
    member _.Edge15 :Line3D =
        edge 1 5 xyzs

    /// Returns the edge between point 1 and point 5.
    static member inline edge15 (b:FreeBox) :Line3D =
        b.Edge15

    /// Returns the edge between point 2 and point 6.
    member _.Edge26 :Line3D =
        edge 2 6 xyzs

    /// Returns the edge between point 2 and point 6.
    static member inline edge26 (b:FreeBox) :Line3D =
        b.Edge26

    /// Returns the edge between point 3 and point 7.
    member _.Edge37 :Line3D =
        edge 3 7 xyzs

    /// Returns the edge between point 3 and point 7.
    static member inline edge37 (b:FreeBox) :Line3D =
        b.Edge37

    /// Returns the edge between point 4 and point 5.
    member _.Edge45 :Line3D =
        edge 4 5 xyzs

    /// Returns the edge between point 4 and point 5.
    static member inline edge45 (b:FreeBox) :Line3D =
        b.Edge45

    /// Returns the edge between point 5 and point 6.
    member _.Edge56 :Line3D =
        edge 5 6 xyzs

    /// Returns the edge between point 5 and point 6.
    static member inline edge56 (b:FreeBox) :Line3D =
        b.Edge56

    /// Returns the edge between point 7 and point 6.
    member _.Edge76 :Line3D =
        edge 7 6 xyzs

    /// Returns the edge between point 7 and point 6.
    static member inline edge76 (b:FreeBox) :Line3D =
        b.Edge76

    /// Returns the edge between point 4 and point 7.
    member _.Edge47 :Line3D =
        edge 4 7 xyzs

    /// Returns the edge between point 4 and point 7.
    static member inline edge47 (b:FreeBox) :Line3D =
        b.Edge47

    /// Returns the edge between point 1 and point 0. This is the reverse of Edge01.
    member _.Edge10 :Line3D =
        edge 1 0 xyzs

    /// Returns the edge between point 1 and point 0. This is the reverse of Edge01.
    static member inline edge10 (b:FreeBox) :Line3D =
        b.Edge10

    /// Returns the edge between point 2 and point 1. This is the reverse of Edge12.
    member _.Edge21 :Line3D =
        edge 2 1 xyzs

    /// Returns the edge between point 2 and point 1. This is the reverse of Edge12.
    static member inline edge21 (b:FreeBox) :Line3D =
        b.Edge21

    /// Returns the edge between point 2 and point 3. This is the reverse of Edge32.
    member _.Edge23 :Line3D =
        edge 2 3 xyzs

    /// Returns the edge between point 2 and point 3. This is the reverse of Edge32.
    static member inline edge23 (b:FreeBox) :Line3D =
        b.Edge23

    /// Returns the edge between point 3 and point 0. This is the reverse of Edge03.
    member _.Edge30 :Line3D =
        edge 3 0 xyzs

    /// Returns the edge between point 3 and point 0. This is the reverse of Edge03.
    static member inline edge30 (b:FreeBox) :Line3D =
        b.Edge30

    /// Returns the edge between point 4 and point 0. This is the reverse of Edge04.
    member _.Edge40 :Line3D =
        edge 4 0 xyzs

    /// Returns the edge between point 4 and point 0. This is the reverse of Edge04.
    static member inline edge40 (b:FreeBox) :Line3D =
        b.Edge40

    /// Returns the edge between point 5 and point 1. This is the reverse of Edge15.
    member _.Edge51 :Line3D =
        edge 5 1 xyzs

    /// Returns the edge between point 5 and point 1. This is the reverse of Edge15.
    static member inline edge51 (b:FreeBox) :Line3D =
        b.Edge51

    /// Returns the edge between point 6 and point 2. This is the reverse of Edge26.
    member _.Edge62 :Line3D =
        edge 6 2 xyzs

    /// Returns the edge between point 6 and point 2. This is the reverse of Edge26.
    static member inline edge62 (b:FreeBox) :Line3D =
        b.Edge62

    /// Returns the edge between point 7 and point 3. This is the reverse of Edge37.
    member _.Edge73 :Line3D =
        edge 7 3 xyzs

    /// Returns the edge between point 7 and point 3. This is the reverse of Edge37.
    static member inline edge73 (b:FreeBox) :Line3D =
        b.Edge73

    /// Returns the edge between point 5 and point 4. This is the reverse of Edge45.
    member _.Edge54 :Line3D =
        edge 5 4 xyzs

    /// Returns the edge between point 5 and point 4. This is the reverse of Edge45.
    static member inline edge54 (b:FreeBox) :Line3D =
        b.Edge54

    /// Returns the edge between point 6 and point 5. This is the reverse of Edge56.
    member _.Edge65 :Line3D =
        edge 6 5 xyzs

    /// Returns the edge between point 6 and point 5. This is the reverse of Edge56.
    static member inline edge65 (b:FreeBox) :Line3D =
        b.Edge65

    /// Returns the edge between point 6 and point 7. This is the reverse of Edge76.
    member _.Edge67 :Line3D =
        edge 6 7 xyzs

    /// Returns the edge between point 6 and point 7. This is the reverse of Edge76.
    static member inline edge67 (b:FreeBox) :Line3D =
        b.Edge67

    /// Returns the edge between point 7 and point 4. This is the reverse of Edge47.
    member _.Edge74 :Line3D =
        edge 7 4 xyzs

    /// Returns the edge between point 7 and point 4. This is the reverse of Edge47.
    static member inline edge74 (b:FreeBox) :Line3D =
        b.Edge74



    // these members don't make sense for a FreeBox:

    // /// The first point of the Box array.
    // member b.Origin : Pnt =
    //     getPt 0 xyzs

    // /// The first point of the Box array.
    // static member inline origin (b:FreeBox) : Pnt = b.Origin

    // /// The vector from Pt0 to Pt1 defining the X axis direction and length.
    // member b.Xaxis : Vec =
    //     b.Pt1 - b.Pt0


    // /// The vector from Pt0 to Pt1 defining the X axis direction and length.
    // static member inline xaxis (b:FreeBox) : Vec = b.Xaxis

    // /// The vector from Pt0 to Pt3 defining the Y axis direction and length.
    // member b.Yaxis : Vec =
    //     b.Pt3 - b.Pt0

    // /// The vector from Pt0 to Pt3 defining the Y axis direction and length.
    // static member inline yaxis (b:FreeBox) : Vec = b.Yaxis

    // /// The vector from Pt0 to Pt4 defining the Z axis direction and length.
    // member b.Zaxis : Vec =
    //     b.Pt4 - b.Pt0

    // /// The vector from Pt0 to Pt4 defining the Z axis direction and length.
    // static member inline zaxis (b:FreeBox) : Vec = b.Zaxis

    // /// The length of the Box from Pt0 to Pt1 in the X direction.
    // member b.SizeX : float =
    //     b.Xaxis.Length

    // /// The length of the Box from Pt0 to Pt1 in the X direction.
    // static member inline sizeX (b:FreeBox) : float = b.SizeX

    // /// The length of the Box from Pt0 to Pt3 in the Y direction.
    // member b.SizeY : float =
    //     b.Yaxis.Length

    // /// The length of the Box from Pt0 to Pt3 in the Y direction.
    // static member inline sizeY (b:FreeBox) : float = b.SizeY

    // /// The length of the Box from Pt0 to Pt4 in the Z direction.
    // member b.SizeZ : float =
    //     b.Zaxis.Length

    // /// The length of the Box from Pt0 to Pt4 in the Z direction.
    // static member inline sizeZ (b:FreeBox) : float = b.SizeZ


    // #region Obsolete

    /// The 8 points that make up the box.
    [<Obsolete("Since the internal structure of FreeBox has changed to a flat array of 24 floats, this is not the live internal buffer any more, but a copy. Use FreeBox.AsPoints instead, or FreeBox.XYZs to get the live buffer.")>]
    member b.Points : Pnt array =
        b.AsPoints

    /// The 8 points that make up the box.
    [<Obsolete("Since the internal structure of FreeBox has changed to a flat array of 24 floats, this is not the live internal buffer any more, but a copy. Use FreeBox.asPoints instead, or FreeBox.getXYZs to get the live buffer.")>]
    static member points (b:FreeBox) : Pnt array =
        b.AsPoints

    // #endregion

#if !FABLE_COMPILER
/// Serializes a FreeBox as its interleaved XYZ coordinate array with System.Text.Json.
and FreeBoxJsonConverter() =
    inherit JsonConverter<FreeBox>()

    override _.Write(writer, box, options) =
        JsonConvert.writeProperty writer options "XYZs" box.XYZs

    override _.Read(reader, _, options) =
        let xyzs = JsonConvert.readProperty<float[]> &reader options "Euclid.FreeBox" "XYZs"
        FreeBox.createDirectly xyzs
#endif
