namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar

open System.Collections.Generic


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

    member this.Pt0 = pts.[0]
    member this.Pt1 = pts.[1]
    member this.Pt2 = pts.[2]
    member this.Pt3 = pts.[3]
    member this.Pt4 = pts.[4]
    member this.Pt5 = pts.[5]
    member this.Pt6 = pts.[6]
    member this.Pt7 = pts.[7]

    /// The 8 points that make up the box.
    [<DataMember>]
    member this.Points = pts

    member this.GetPt (i:int) =
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
        let p0 = pts[0].WithZ zMin
        let p1 = pts[1].WithZ zMin
        let p2 = pts[2].WithZ zMin
        let p3 = pts[3].WithZ zMin
        let p4 = pts[0].WithZ zMax
        let p5 = pts[1].WithZ zMax
        let p6 = pts[2].WithZ zMax
        let p7 = pts[3].WithZ zMax
        FreeBox [|
            p0; p1; p2; p3;
            p4; p5; p6; p7
        |]

    /// Creates a FreeBox from four 2D points and a zMin and zMax value.
    static member createFromFour2DPointsArgs ( a:Pt, b:Pt, c:Pt, d:Pt, zMin, zMax) =
        let p0 = a.WithZ zMin
        let p1 = b.WithZ zMin
        let p2 = c.WithZ zMin
        let p3 = d.WithZ zMin
        let p4 = a.WithZ zMax
        let p5 = b.WithZ zMax
        let p6 = c.WithZ zMax
        let p7 = d.WithZ zMax
        FreeBox [|
            p0; p1; p2; p3;
            p4; p5; p6; p7
        |]


