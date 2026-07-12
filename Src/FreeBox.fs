namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid
open EuclidErrors



/// <summary>
/// A class containing an array of 8 points representing an arbitrary 3D box-like topology.
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
[<DataContract>] // for using DataMember on fields
type FreeBox private (pts:Pnt[]) =

    /// The 8 points that make up the box.
    [<DataMember>]
    member b.Points : Pnt array =
        pts

    /// The 8 points that make up the box.
    static member inline points (b:FreeBox) : Pnt array =
        b.Points

    /// <summary>Gets or sets the point 0 of the box array.<code>
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
    member b.Pt0
        with get() : Pnt = pts.[0]
        and  set (p:Pnt) = pts.[0] <- p

    /// Point 0 of the box array. pts.[0]
    static member inline pt0 (b:FreeBox) : Pnt = b.Pt0

    /// Gets or sets the point 1 of the box array.<code>
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
    member b.Pt1
        with get() : Pnt = pts.[1]
        and  set (p:Pnt) = pts.[1] <- p

    /// Point 1 of the box array.
    static member inline pt1 (b:FreeBox) : Pnt = b.Pt1

    /// <summary>Gets or sets the point 2 of the box array.<code>
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
    member b.Pt2
        with get() : Pnt = pts.[2]
        and  set (p:Pnt) = pts.[2] <- p

    /// The third point of the box array. pts.[2]
    static member inline pt2 (b:FreeBox) : Pnt = b.Pt2

    /// <summary>Gets or sets the point 3 of the box array.<code>
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
    member b.Pt3
        with get() : Pnt = pts.[3]
        and  set (p:Pnt) = pts.[3] <- p

    /// The fourth point of the box array. pts.[3]
    static member inline pt3 (b:FreeBox) : Pnt = b.Pt3

    /// <summary>Gets or sets the point 4 of the box array.<code>
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
    member b.Pt4
        with get() : Pnt = pts.[4]
        and  set (p:Pnt) = pts.[4] <- p

    /// The fifth point of the box array. pts.[4]
    static member inline pt4 (b:FreeBox) : Pnt = b.Pt4

    /// <summary>Gets or sets the point 5 of the box array.<code>
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
    member b.Pt5
        with get() : Pnt = pts.[5]
        and  set (p:Pnt) = pts.[5] <- p

    /// The sixth point of the box array. pts.[5]
    static member inline pt5 (b:FreeBox) : Pnt = b.Pt5

    /// <summary>Gets or sets the point 6 of the box array.<code>
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
    member b.Pt6
        with get() : Pnt = pts.[6]
        and  set (p:Pnt) = pts.[6] <- p

    /// The seventh point of the box array. pts.[6]
    static member inline pt6 (b:FreeBox) : Pnt = b.Pt6

    /// <summary>Gets or sets the point 7 of the box array.<code>
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
    member b.Pt7
        with get() : Pnt = pts.[7]
        and  set (p:Pnt) = pts.[7] <- p

    /// The eighth point of the box array. pts.[7]
    static member inline pt7 (b:FreeBox) : Pnt = b.Pt7


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
        let ps = b.Points
        $"FreeBox.createFromEightPoints([| {ps[0].AsFSharpCode}; {ps[1].AsFSharpCode}; {ps[2].AsFSharpCode}; {ps[3].AsFSharpCode}; {ps[4].AsFSharpCode}; {ps[5].AsFSharpCode}; {ps[6].AsFSharpCode}; {ps[7].AsFSharpCode} |])"

    /// Format FreeBox into an F# code string that can be used to recreate the box.
    static member inline asFSharpCode (b:FreeBox) : string = b.AsFSharpCode


    /// Get the point at index in the box array. Index must be between 0 and 7.
    member b.GetPt (i:int) : Pnt =
        if i < 0 || i > 7 then
            fail $"FreeBox.GetPt invalid index {i}"
        pts.[i]

    /// Get the point at index in the box array. Index must be between 0 and 7.
    static member inline getPt (i:int) (b:FreeBox) : Pnt =
        b.GetPt(i)

    /// Sets the point at the specified index. The index must be between 0 and 7.
    member b.SetPt (i:int) (p:Pnt) : unit =
        if i < 0 || i > 7 then
            fail $"FreeBox.SetPt invalid index {i}"
        pts.[i] <- p

    /// Sets the point at the specified index. The index must be between 0 and 7.
    static member inline setPt (i:int) (p:Pnt) (b:FreeBox) : unit =
        b.SetPt i p

    /// Scales the FreeBox by a given factor on the world origin (0, 0, 0).
    member b.Scale (factor:float) : FreeBox =
        b.Points
        |> Array.map (fun p -> p * factor)
        |> FreeBox

    /// Scales the FreeBox by a given factor.
    /// Scale center is World Origin 0,0,0
    static member inline scale (factor:float) (b:FreeBox) : FreeBox =
        b.Scale(factor)

    /// Scales the FreeBox by a given factor on a given center point.
    member b.ScaleOn (cen:Pnt, factor:float) : FreeBox =
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

    /// Scales the FreeBox by a given factor on a given center point.
    static member inline scaleOn (cen:Pnt)  (factor:float) (b:FreeBox) : FreeBox =
        b.ScaleOn(cen, factor)

    /// Returns a FreeBox moved by a vector. Same as FreeBox.move and FreeBox.translate.
    member b.Move (v:Vec) : FreeBox =
        b.Points
        |> Array.map (fun p -> p + v)
        |> FreeBox

    /// Move a FreeBox by a vector. Same as FreeBox.translate.
    static member inline move (v:Vec) (b:FreeBox) : FreeBox =
        b.Move(v)

    /// Returns a FreeBox moved by a given distance in world X direction.
    member b.MoveX (distance:float) : FreeBox =
        b.Points
        |> Array.map (fun p -> Pnt(p.X + distance, p.Y, p.Z))
        |> FreeBox

    /// Returns the FreeBox moved by a given distance in world X direction.
    static member inline moveX (distance:float) (b:FreeBox) : FreeBox =
        b.MoveX(distance)

    /// Returns a FreeBox moved by a given distance in Y direction.
    member b.MoveY (distance:float) : FreeBox =
        b.Points
        |> Array.map (fun p -> Pnt(p.X, p.Y + distance, p.Z))
        |> FreeBox

    /// Returns the FreeBox moved by a given distance in world Y direction.
    static member inline moveY (distance:float) (b:FreeBox) : FreeBox =
        b.MoveY(distance)

    /// Returns a FreeBox moved by a given distance in world Z direction.
    member b.MoveZ (distance:float) : FreeBox =
        b.Points
        |> Array.map (fun p -> Pnt(p.X, p.Y, p.Z + distance))
        |> FreeBox

    /// Returns the FreeBox moved by a given distance in world Z direction.
    static member inline moveZ (distance:float) (b:FreeBox) : FreeBox =
        b.MoveZ(distance)

    /// Applies or multiplies a 4x4 transformation matrix to the FreeBox.
    member b.Transform (m:Matrix) : FreeBox =
        b.Points
        |> Array.map (fun p -> p *** m)
        |> FreeBox

    /// Applies or multiplies a 4x4 transformation matrix to the FreeBox.
    static member inline transform (m:Matrix) (b:FreeBox) : FreeBox =
        b.Transform(m)

    /// Multiplies (or applies) a RigidMatrix to the FreeBox.
    member b.TransformRigid (m:RigidMatrix) : FreeBox =
        b.Points
        |> Array.map (fun p -> Pnt.transformRigid m p)
        |> FreeBox

    /// Multiplies (or applies) a RigidMatrix to the FreeBox.
    static member inline transformRigid (m:RigidMatrix) (b:FreeBox) : FreeBox =
        b.TransformRigid(m)

    /// Multiplies (or applies) a Quaternion to the FreeBox.
    /// The box is rotated around the World Origin.
    member b.Rotate (q:Quaternion) : FreeBox =
        b.Points
        |> Array.map (fun p -> p *** q)
        |> FreeBox

    /// Multiplies (or applies) a Quaternion to the FreeBox.
    /// The box is rotated around the World Origin.
    static member inline rotate (q:Quaternion) (b:FreeBox) : FreeBox =
        b.Rotate(q)

    /// Multiplies (or applies) a Quaternion to the FreeBox around a given center point.
    member b.RotateWithCenter (cen:Pnt, q:Quaternion) : FreeBox =
        b.Points
        |> Array.map (fun p -> Pnt.rotateWithCenter cen q p)
        |> FreeBox

    /// Multiplies (or applies) a Quaternion to the FreeBox around a given center point.
    static member inline rotateWithCenter (cen:Pnt) (q:Quaternion) (b:FreeBox) : FreeBox =
        b.RotateWithCenter(cen, q)

    /// Creates a FreeBox from an array of 8 points. The points can be in arbitrary position in space.
    static member createFromEightPoints (pts:Pnt[]) : FreeBox =
        if pts.Length <> 8 then
            failwithf "FreeBox.createFrom8 must be initialized with 8 points, got %d" pts.Length
        new FreeBox( pts )

    /// Creates a FreeBox from a Box. The 8 points of the FreeBox are the 8 corners of the Box.
    static member createFromBox (box:Box) : FreeBox =
        FreeBox [|
            box.Pt0; box.Pt1; box.Pt2; box.Pt3;
            box.Pt4; box.Pt5; box.Pt6; box.Pt7
        |]

    /// Creates a FreeBox from four 2D points and a zMin and zMax value.
    static member createFromFour2DPoints zMin zMax (pts:Pt[]) : FreeBox =
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

    /// Creates a FreeBox from four 2D points in counter-clockwise order and a zMin and zMax value.
    static member createFromFour2DPointsArgs ( a:Pt, b:Pt, c:Pt, d:Pt, zMin, zMax) : FreeBox =
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
    member b.Edge01 :Line3D =
        Line3D(pts.[0].X, pts.[0].Y, pts.[0].Z, pts.[1].X, pts.[1].Y, pts.[1].Z)

    /// Returns the edge between point 0 and point 1.
    static member inline edge01 (b:FreeBox) :Line3D =
        b.Edge01

    /// Returns the edge between point 1 and point 2.
    member b.Edge12 :Line3D =
        Line3D(pts.[1].X, pts.[1].Y, pts.[1].Z, pts.[2].X, pts.[2].Y, pts.[2].Z)

    /// Returns the edge between point 1 and point 2.
    static member inline edge12 (b:FreeBox) :Line3D =
        b.Edge12

    /// Returns the edge between point 3 and point 2.
    member b.Edge32 :Line3D =
        Line3D(pts.[3].X, pts.[3].Y, pts.[3].Z, pts.[2].X, pts.[2].Y, pts.[2].Z)

    /// Returns the edge between point 3 and point 2.
    static member inline edge32 (b:FreeBox) :Line3D =
        b.Edge32

    /// Returns the edge between point 0 and point 3.
    member b.Edge03 :Line3D =
        Line3D(pts.[0].X, pts.[0].Y, pts.[0].Z, pts.[3].X, pts.[3].Y, pts.[3].Z)

    /// Returns the edge between point 0 and point 3.
    static member inline edge03 (b:FreeBox) :Line3D =
        b.Edge03

    /// Returns the edge between point 0 and point 4.
    member b.Edge04 :Line3D =
        Line3D(pts.[0].X, pts.[0].Y, pts.[0].Z, pts.[4].X, pts.[4].Y, pts.[4].Z)

    /// Returns the edge between point 0 and point 4.
    static member inline edge04 (b:FreeBox) :Line3D =
        b.Edge04

    /// Returns the edge between point 1 and point 5.
    member b.Edge15 :Line3D =
        Line3D(pts.[1].X, pts.[1].Y, pts.[1].Z, pts.[5].X, pts.[5].Y, pts.[5].Z)

    /// Returns the edge between point 1 and point 5.
    static member inline edge15 (b:FreeBox) :Line3D =
        b.Edge15

    /// Returns the edge between point 2 and point 6.
    member b.Edge26 :Line3D =
        Line3D(pts.[2].X, pts.[2].Y, pts.[2].Z, pts.[6].X, pts.[6].Y, pts.[6].Z)

    /// Returns the edge between point 2 and point 6.
    static member inline edge26 (b:FreeBox) :Line3D =
        b.Edge26

    /// Returns the edge between point 3 and point 7.
    member b.Edge37 :Line3D =
        Line3D(pts.[3].X, pts.[3].Y, pts.[3].Z, pts.[7].X, pts.[7].Y, pts.[7].Z)

    /// Returns the edge between point 3 and point 7.
    static member inline edge37 (b:FreeBox) :Line3D =
        b.Edge37

    /// Returns the edge between point 4 and point 5.
    member b.Edge45 :Line3D =
        Line3D(pts.[4].X, pts.[4].Y, pts.[4].Z, pts.[5].X, pts.[5].Y, pts.[5].Z)

    /// Returns the edge between point 4 and point 5.
    static member inline edge45 (b:FreeBox) :Line3D =
        b.Edge45

    /// Returns the edge between point 5 and point 6.
    member b.Edge56 :Line3D =
        Line3D(pts.[5].X, pts.[5].Y, pts.[5].Z, pts.[6].X, pts.[6].Y, pts.[6].Z)

    /// Returns the edge between point 5 and point 6.
    static member inline edge56 (b:FreeBox) :Line3D =
        b.Edge56

    /// Returns the edge between point 7 and point 6.
    member b.Edge76 :Line3D =
        Line3D(pts.[7].X, pts.[7].Y, pts.[7].Z, pts.[6].X, pts.[6].Y, pts.[6].Z)

    /// Returns the edge between point 7 and point 6.
    static member inline edge76 (b:FreeBox) :Line3D =
        b.Edge76

    /// Returns the edge between point 4 and point 7.
    member b.Edge47 :Line3D =
        Line3D(pts.[4].X, pts.[4].Y, pts.[4].Z, pts.[7].X, pts.[7].Y, pts.[7].Z)

    /// Returns the edge between point 4 and point 7.
    static member inline edge47 (b:FreeBox) :Line3D =
        b.Edge47



    // these members don't make sense for a FreeBox:

    // /// The first point of the Box array.
    // member b.Origin : Pnt =
    //     pts.[0]

    // /// The first point of the Box array.
    // static member inline origin (b:FreeBox) : Pnt = b.Origin

    // /// The vector from Pt0 to Pt1 defining the X axis direction and length.
    // member b.Xaxis : Vec =
    //     pts.[1] - pts.[0]


    // /// The vector from Pt0 to Pt1 defining the X axis direction and length.
    // static member inline xaxis (b:FreeBox) : Vec = b.Xaxis

    // /// The vector from Pt0 to Pt3 defining the Y axis direction and length.
    // member b.Yaxis : Vec =
    //     pts.[3] - pts.[0]

    // /// The vector from Pt0 to Pt3 defining the Y axis direction and length.
    // static member inline yaxis (b:FreeBox) : Vec = b.Yaxis

    // /// The vector from Pt0 to Pt4 defining the Z axis direction and length.
    // member b.Zaxis : Vec =
    //     pts.[4] - pts.[0]

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

    [<Obsolete("Use .Edge01 instead.")>]
    member b.Edge0 :Line3D = b.Edge01

    [<Obsolete("Use .Edge12 instead.")>]
    member b.Edge1 :Line3D = b.Edge12

    [<Obsolete("Use .Edge32 instead.")>]
    member b.Edge2 :Line3D = b.Edge32

    [<Obsolete("Use .Edge03 instead.")>]
    member b.Edge3 :Line3D = b.Edge03

    [<Obsolete("Use .Edge45 instead.")>]
    member b.Edge4 :Line3D = b.Edge45

    [<Obsolete("Use .Edge56 instead.")>]
    member b.Edge5 :Line3D = b.Edge56

    [<Obsolete("Use .Edge76 instead.")>]
    member b.Edge6 :Line3D = b.Edge76

    [<Obsolete("Use .Edge47 instead.")>]
    member b.Edge7 :Line3D = b.Edge47

    [<Obsolete("Use .Edge04 instead.")>]
    member b.Edge8 :Line3D = b.Edge04

    [<Obsolete("Use .Edge15 instead.")>]
    member b.Edge9 :Line3D = b.Edge15

    [<Obsolete("Use .Edge26 instead.")>]
    member b.Edge10 :Line3D = b.Edge26

    [<Obsolete("Use .Edge37 instead.")>]
    member b.Edge11 :Line3D = b.Edge37

    // #endregion
