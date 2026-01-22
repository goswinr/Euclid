namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar

open System.Collections.Generic

#nowarn "52" // The value has been copied to ensure the original is not mutated by this operation


/// An arbitrary 3D Box.
/// Described by 8 3D points.
/// The points can be in arbitrary position in space.
///
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
[<NoEquality; NoComparison>] // because its made up from floats
[<DataContract>] // for using DataMember on fields
type FreeBox private (pts:Pnt[]) =

    member b.Pt0 = pts.[0]
    member b.Pt1 = pts.[1]
    member b.Pt2 = pts.[2]
    member b.Pt3 = pts.[3]
    member b.Pt4 = pts.[4]
    member b.Pt5 = pts.[5]
    member b.Pt6 = pts.[6]
    member b.Pt7 = pts.[7]

    /// The 8 points that make up the box.
    [<DataMember>]
    member b.Points = pts


    /// Nicely formatted string representation of the Box including its size.
    override b.ToString() =
        let sizeX = Format.float b.SizeX
        let sizeY = Format.float b.SizeY
        let sizeZ = Format.float b.SizeZ
        let origin = b.Origin.AsString
        let xAxis  = b.Xaxis.AsString
        let yAxis  = b.Yaxis.AsString
        let zAxis  = b.Zaxis.AsString
        $"Euclid.Box %s{sizeX} x %s{sizeY} x %s{sizeZ} (Origin:%s{origin}| X-ax:%s{xAxis}|Y-ax:%s{yAxis}|Z-ax:%s{zAxis})"


    /// Format Box into string with nice floating point number formatting of X, Y and Z size only.
    /// But without type name as in v.ToString()
    member b.AsString : string =
        let sizeX = Format.float b.SizeX
        let sizeY = Format.float b.SizeY
        let sizeZ = Format.float b.SizeZ
        $"%s{sizeX} x %s{sizeY} x %s{sizeZ}"

    /// Format FreeBox into an F# code string that can be used to recreate the box.
    member b.AsFSharpCode : string =
        let ps = b.Points
        $"FreeBox.createFromEightPoints([| {ps[0].AsFSharpCode}; {ps[1].AsFSharpCode}; {ps[2].AsFSharpCode}; {ps[3].AsFSharpCode}; {ps[4].AsFSharpCode}; {ps[5].AsFSharpCode}; {ps[6].AsFSharpCode}; {ps[7].AsFSharpCode} |])"

    /// The first point of the Box array.
    member b.Origin : Pnt =
        pts.[0]

    /// The vector from Pt0 to Pt1 defining the X axis direction and length.
    member b.Xaxis : Vec =
        pts.[1] - pts.[0]

    /// The vector from Pt0 to Pt3 defining the Y axis direction and length.
    member b.Yaxis : Vec =
        pts.[3] - pts.[0]

    /// The vector from Pt0 to Pt4 defining the Z axis direction and length.
    member b.Zaxis : Vec =
        pts.[4] - pts.[0]

    /// The length of the Box from Pt0 to Pt1 in the X direction.
    member b.SizeX : float =
        b.Xaxis.Length

    /// The length of the Box from Pt0 to Pt3 in the Y direction.
    member b.SizeY : float =
        b.Yaxis.Length

    /// The length of the Box from Pt0 to Pt4 in the Z direction.
    member b.SizeZ : float =
        b.Zaxis.Length

    member b.GetPt (i:int) =
        if i < 0 || i > 7 then
            failwithf "FreeBox.Pt index must be between 0 and 7, got %d" i
        pts.[i]

    /// Scales the 3D rectangle by a given factor on world origin (0,0,0)
    member b.Scale (factor:float) : FreeBox =
        b.Points
        |> Array.map (fun p -> p * factor)
        |> FreeBox


    /// Scales the 3D rectangle by a given factor on a given center point
    member b.ScaleOn (cen:Pnt) (factor:float) : FreeBox =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        b.Points
        |> Array.map (fun p ->
                Pnt(
                    cx + (p.X - cx) * factor,
                    cy + (p.Y - cy) * factor,
                    cz + (p.Z - cz) * factor
                )
            )
        |> FreeBox

    static member createFromEightPoints (pts:Pnt[]) =
        if pts.Length <> 8 then
            failwithf "FreeBox.createFrom8 must be initialized with 8 points, got %d" pts.Length
        FreeBox pts

    static member createFromBox (box:Box) =
        FreeBox [|
            box.Pt0; box.Pt1; box.Pt2; box.Pt3;
            box.Pt4; box.Pt5; box.Pt6; box.Pt7
        |]

    /// Creates a FreeBox from four 2D points and a zMin and zMax value.
    static member createFromFour2DPoints zMin zMax (pts:Pt[]) =
        if pts.Length <> 4 then
            failwithf "FreeBox.createFrom4 must be initialized with 4 points, got %d" pts.Length
        FreeBox [|
            pts[0].WithZ zMin// 0
            pts[1].WithZ zMin// 1
            pts[2].WithZ zMin// 2
            pts[3].WithZ zMin// 3
            pts[0].WithZ zMax// 4
            pts[1].WithZ zMax// 5
            pts[2].WithZ zMax// 6
            pts[3].WithZ zMax// 7
            |]

    /// Creates a FreeBox from four 2D points and a zMin and zMax value.
    static member createFromFour2DPointsArgs ( a:Pt, b:Pt, c:Pt, d:Pt, zMin, zMax) =
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
        FreeBox [|
            a.WithZ zMin // 0
            b.WithZ zMin // 1
            c.WithZ zMin // 2
            d.WithZ zMin // 3
            a.WithZ zMax // 4
            b.WithZ zMax // 5
            c.WithZ zMax // 6
            d.WithZ zMax // 7
            |]


