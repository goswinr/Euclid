namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar

open System.Collections.Generic

/// An immutable 3D Box with any rotation in 3D space.
/// Described by an Origin and three Edge vectors.
/// Similar to PPlane, however the three vectors are not unitized.
/// This implementation guarantees the box to be always valid.
/// That means the Min X, Y and Z axes cannot be flipped individually.
/// However the length of one of these axes might still be zero.
///
///   local        local
///   Z-Axis       Y-Axis
///   ^           /
///   |   7      /        6
///   |   +---------------+
///   |  /|    /         /|
///   | / |   /         / |
/// 4 |/  |  /       5 /  |
///   +---------------+   |
///   |   |/          |   |
///   |   +-----------|---+
///   |  / 3          |  / 2
///   | /             | /
///   |/              |/     local
///   +---------------+----> X-Axis
///   0               1
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Box =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar
    /// The Origin Corner of the Box.
    [<DataMember>] val Origin: Pnt

    /// The Edge vector representing the X-axis of the Box.
    [<DataMember>] val Xaxis: Vec

    /// The Edge vector representing the Y-axis of the Box.
    [<DataMember>] val Yaxis: Vec

    /// The Edge vector representing the Z-axis of the Box.
    [<DataMember>] val Zaxis: Vec

    /// Unchecked Internal Constructor Only.
    /// Create a Parametrized Plane with X, Y and Z Direction.
    internal new (origin, axisX, axisY, axisZ) = {Origin=origin; Xaxis=axisX; Yaxis=axisY; Zaxis=axisZ}

    /// The size in X direction, same as member box.SizeX.
    [<Obsolete("use SizeX")>]
    member inline b.Width = b.Xaxis.Length

    /// The size in X direction.
    member inline b.SizeX = b.Xaxis.Length

    /// The size in X direction squared.
    member inline b.SizeXSq = b.Xaxis.LengthSq

    /// The size in Y direction, same as member box.SizeY.
    [<Obsolete("use SizeY")>]
    member inline b.Depth = b.Yaxis.Length

    /// The size in Y direction.
    member inline b.SizeY = b.Yaxis.Length

    /// The size in Y direction squared.
    member inline b.SizeYSq = b.Yaxis.LengthSq

    /// The size in Z direction, same as member box.SizeZ.
    [<Obsolete("use SizeZ")>]
    member inline b.Height3D = b.Zaxis.Length

    /// The size in Z direction.
    member inline b.SizeZ = b.Zaxis.Length

    /// The size in Z direction squared.
    member inline b.SizeZSq = b.Zaxis.LengthSq


    /// Nicely formatted string representation of the Box including its size.
    override b.ToString() =
        sprintf "Euclid.Box %s x %s x %s (Origin:%s| X-ax:%s| Y-ax:%s| Z-ax:%s)"
            (Format.float b.SizeX) (Format.float b.SizeY) (Format.float b.SizeZ)
            b.Origin.AsString b.Xaxis.AsString b.Yaxis.AsString b.Zaxis.AsString


    /// Format Box into string with nice floating point number formatting of X, Y and Z size only.
    /// But without type name as in v.ToString()
    member b.AsString = sprintf "%s x %s x %s" (Format.float b.SizeX)  (Format.float b.SizeY) (Format.float b.SizeZ)

    /// Returns point 0 of the box, same box.Origin.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt0 = b.Origin

    /// Returns point 1 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt1 = b.Origin + b.Xaxis

    /// Returns point 2 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt2 = b.Origin + b.Xaxis + b.Yaxis

    /// Returns point 3 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt3 = b.Origin + b.Yaxis

    /// Returns point 4 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt4 = b.Origin + b.Zaxis

    /// Returns point 5 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt5 = b.Origin + b.Xaxis + b.Zaxis

    /// Returns point 6 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt6 = b.Origin + b.Xaxis + b.Yaxis + b.Zaxis

    /// Returns point 7 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt7 = b.Origin + b.Yaxis + b.Zaxis

    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit =
        let len = r.Xaxis.Length
        if isTooTiny len then EuclidException.Raisef "Euclid.Box.XaxisUnit: rect Xaxis is too small for unitizing: %s" r.AsString
        r.Xaxis*(1./len)

    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit =
        let len = r.Yaxis.Length
        if isTooTiny len then EuclidException.Raisef "Euclid.Box.XaxisUnit: rect Yaxis is too small for unitizing: %s" r.AsString
        r.Yaxis*(1./len)

    /// Creates a unitized version of the local Z-Axis.
    member inline r.ZaxisUnit =
        let len = r.Zaxis.Length
        if isTooTiny len then EuclidException.Raisef "Euclid.Box.XaxisUnit: rect Zaxis is too small for unitizing: %s" r.AsString
        r.Zaxis*(1./len)

    /// The corner diagonally opposite of corner from Origin.
    member inline b.FarCorner = b.Origin + b.Xaxis + b.Yaxis + b.Zaxis

    /// The diagonal vector of the Box.
    member inline b.Diagonal = b.Xaxis + b.Yaxis + b.Zaxis

    /// The center of the Box.
    member inline b.Center = b.Origin + b.Xaxis*0.5 + b.Yaxis*0.5 + b.Zaxis*0.5


    /// Returns the bottom corners of the Box in Counter-Clockwise order, starting at Origin.
    /// Then the top corners staring above Origin. Returns an array of 8 Points.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.Points :Pnt[] =
        let p0 = b.Origin
        let p1 = p0 + b.Xaxis
        let p4 = p0 + b.Zaxis
        let p5 = p4 + b.Xaxis
        [|
            p0
            p1
            p1 + b.Yaxis
            p0 + b.Yaxis
            p4
            p5
            p5 + b.Yaxis
            p4 + b.Yaxis
        |]

    /// Returns the top face of the Box in Counter-Clockwise order, looking from above.
    /// Returns Origin at point 4, X-Axis to point 5, Y-Axis to point 7.
    /// The normal of the Rect3D points away from the Box.
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.TopFace :Rect3D = Rect3D(b.Origin + b.Zaxis, b.Xaxis, b.Yaxis)


    /// Returns the bottom face of the Box in Counter-Clockwise order, looking from above.
    /// Returns Origin at point 0, X-Axis to point 1, Y-Axis to point 3.
    /// The normal of the Rect3D points into the Box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.BottomFace = Rect3D(b.Origin, b.Xaxis, b.Yaxis)



    /// Returns the front face of the Box in Counter-Clockwise order, looking from front.
    /// Returns Origin at point 0, X-Axis to point 1, Y-Axis to point 4.
    /// The normal of the Rect3D points away from the Box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.FrontFace = Rect3D(b.Origin, b.Xaxis, b.Zaxis)

    /// Returns the back face of the Box in Counter-Clockwise order, looking from front.
    /// Returns Origin at point 3, X-Axis to point 2, Y-Axis to point 7.
    /// The normal of the Rect3D points into the Box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.BackFace = Rect3D(b.Origin + b.Yaxis, b.Xaxis, b.Zaxis)

    /// Returns the right face of the Box in Counter-Clockwise order, looking from right.
    /// Returns Origin at point 1, X-Axis to point 2, Y-Axis to point 5.
    /// The normal of the Rect3D points away from the Box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.RightFace = Rect3D(b.Origin + b.Xaxis, b.Yaxis, b.Zaxis)

    /// Returns the left face of the Box in Counter-Clockwise order, looking from right.
    /// Returns Origin at point 0, X-Axis to point 3, Y-Axis to point 4.
    /// The normal of the Rect3D points into the Box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.LeftFace = Rect3D(b.Origin, b.Yaxis, b.Zaxis)

    /// Evaluate a X, Y and Z parameter of the Box.
    ///  0.0, 0.0, 0.0 returns the Origin.
    ///  1.0, 1.0, 1.0 returns the FarCorner.
    member inline b.EvaluateAt (xParameter:float, yParameter:float, zParameter:float) =
        b.Origin + b.Xaxis * xParameter + b.Yaxis * yParameter + b.Zaxis * zParameter


    /// Calculates the volume of the Box.
    member inline b.Volume =
        b.Xaxis.Length*b.Yaxis.Length*b.Zaxis.Length


     /// Calculates the squared volume of the Box.
    /// by using the squared lengths of the X , Y and Z axis.
    /// This is a bit faster than calculating the area and good enough for relative comparisons or sorting by size.
    member inline r.AreaSq =
        r.Xaxis.LengthSq * r.Yaxis.LengthSq * r.Zaxis.LengthSq

    /// Gets the Plane that this box is based on.
    member inline b.Plane =
        let x = b.Xaxis.Unitized
        let y = b.Yaxis.Unitized
        let z = b.Zaxis.Unitized
        PPlane.createUnchecked (b.Origin, x, y, z)


    /// Check for point containment in the Box.
    /// By doing 6 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    member b.Contains(p:Pnt) =
        let v = p - b.Origin
        v *** b.Xaxis >= 0.
        &&
        v *** b.Yaxis >= 0.
        &&
        v *** b.Zaxis >= 0.
        &&
        (p - b.Pt3) *** b.Yaxis <= 0.
        &&
        (p - b.Pt1) *** b.Xaxis <= 0.
        &&
        (p - b.Pt4) *** b.Zaxis <= 0.


    /// Gets the axis aligned 3D Bounding Box of the Box.
    member r.BBox =
        let y = r.Yaxis
        let z = r.Zaxis
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        let p2 = p1 + y
        let p3 = p0 + y
        let p4 = p0 + z
        let p5 = p1 + z
        let p6 = p2 + z
        let p7 = p3 + z
        let minX = min (min (min (min (min (min (min p0.X p1.X) p2.X) p3.X) p4.X) p5.X) p6.X) p7.X
        let minY = min (min (min (min (min (min (min p0.Y p1.Y) p2.Y) p3.Y) p4.Y) p5.Y) p6.Y) p7.Y
        let minZ = min (min (min (min (min (min (min p0.Z p1.Z) p2.Z) p3.Z) p4.Z) p5.Z) p6.Z) p7.Z
        let maxX = max (max (max (max (max (max (max p0.X p1.X) p2.X) p3.X) p4.X) p5.X) p6.X) p7.X
        let maxY = max (max (max (max (max (max (max p0.Y p1.Y) p2.Y) p3.Y) p4.Y) p5.Y) p6.Y) p7.Y
        let maxZ = max (max (max (max (max (max (max p0.Z p1.Z) p2.Z) p3.Z) p4.Z) p5.Z) p6.Z) p7.Z
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)


    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Check for point containment in the Box.
    /// By doing 6 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    static member inline contains (p:Pnt) (b:Box) = b.Contains p

    /// Checks if two 3D-boxes are equal within tolerance.
    /// Does not recognize congruent boxes with different rotation as equal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:Box) (b:Box) =
        abs (a.Origin.X - b.Origin.X) <= tol &&
        abs (a.Origin.Y - b.Origin.Y) <= tol &&
        abs (a.Origin.Z - b.Origin.Z) <= tol &&
        abs (a.Xaxis.X  - b.Xaxis.X ) <= tol &&
        abs (a.Xaxis.Y  - b.Xaxis.Y ) <= tol &&
        abs (a.Xaxis.Z  - b.Xaxis.Z ) <= tol &&
        abs (a.Yaxis.X  - b.Yaxis.X ) <= tol &&
        abs (a.Yaxis.Y  - b.Yaxis.Y ) <= tol &&
        abs (a.Yaxis.Z  - b.Yaxis.Z ) <= tol &&
        abs (a.Zaxis.X  - b.Zaxis.X ) <= tol &&
        abs (a.Zaxis.Y  - b.Zaxis.Y ) <= tol &&
        abs (a.Zaxis.Z  - b.Zaxis.Z ) <= tol


    /// Check if two 3D-boxes are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two 3D-boxes are not exactly equal.
    static member notEquals (tol:float) (a:Box) (b:Box) =
        abs (a.Origin.X - b.Origin.X) > tol ||
        abs (a.Origin.Y - b.Origin.Y) > tol ||
        abs (a.Origin.Z - b.Origin.Z) > tol ||
        abs (a.Xaxis.X  - b.Xaxis.X ) > tol ||
        abs (a.Xaxis.Y  - b.Xaxis.Y ) > tol ||
        abs (a.Xaxis.Z  - b.Xaxis.Z ) > tol ||
        abs (a.Yaxis.X  - b.Yaxis.X ) > tol ||
        abs (a.Yaxis.Y  - b.Yaxis.Y ) > tol ||
        abs (a.Yaxis.Z  - b.Yaxis.Z ) > tol ||
        abs (a.Zaxis.X  - b.Zaxis.X ) > tol ||
        abs (a.Zaxis.Y  - b.Zaxis.Y ) > tol ||
        abs (a.Zaxis.Z  - b.Zaxis.Z ) > tol


    /// Returns Box expanded by distance on all six sides.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (b:Box) =
        let siX = b.SizeX
        let siY = b.SizeY
        let hei = b.SizeZ
        let d = dist * -2.0
        if siX<=d || siY<=d || hei<=d then
            EuclidException.Raisef "Euclid.Box.expand: Box %s is too small to expand by negative distance %s"  b.AsString (Format.float dist)
        let x = b.Xaxis * (dist / siX)
        let y = b.Yaxis * (dist / siY)
        let z = b.Zaxis * (dist / hei)
        Box(b.Origin-x-y-z, b.Xaxis+x*2., b.Yaxis+y*2., b.Zaxis+z*2.)

    /// Returns Box expanded by respective distances on all six sides.
    /// Does check for overflow if distance is negative and fails.
    /// distX, distY and distZ are for X, Y and Z-axis respectively.
    static member expandXYZ distX distY distZ (b:Box) =
        let siX = b.SizeX
        let siY = b.SizeY
        let hei = b.SizeZ
        if siX <= distX * -2.0 then EuclidException.Raisef "Euclid.Box.expandXYZ: Box %s is too small to expand by negative distance distX %s"  b.AsString (Format.float distX)
        if siY <= distY * -2.0 then EuclidException.Raisef "Euclid.Box.expandXYZ: Box %s is too small to expand by negative distance distY %s"  b.AsString (Format.float distY)
        if hei <= distZ * -2.0 then EuclidException.Raisef "Euclid.Box.expandXYZ: Box %s is too small to expand by negative distance distZ %s"  b.AsString (Format.float distZ)
        let x = b.Xaxis * (distX / b.SizeX)
        let y = b.Yaxis * (distY / b.SizeY )
        let z = b.Zaxis * (distZ / b.SizeZ)
        Box(b.Origin-x-y-z, b.Xaxis+x*2., b.Yaxis+y*2., b.Zaxis+z*2.)

    /// Creates a 3D box from PPlane and x, y and Z size.
    static member createFromPlane (pl:PPlane, x, y, z) =
        Box(pl.Origin, pl.Xaxis*x, pl.Yaxis*y, pl.Zaxis*z)

    /// Creates a 3D box from a 3D a bounding box.
    static member createFromBoundingBox (b:BBox) =
        Box(b.MinPnt, Vec.Xaxis*b.SizeX, Vec.Yaxis*b.SizeY, Vec.Zaxis*b.SizeZ)

    /// Creates a 3D box from a 2D rectangle and Z lower and upper position.
    static member createFromRect2D (r:Rect2D, zLow, zHigh) =
        Box(r.Origin.WithZ zLow,
            r.Xaxis.AsVec,
            r.Yaxis.AsVec,
            Vec.Zaxis*(zHigh-zLow))

     /// Creates a 3D box from a 3D rectangle and Z lower and upper position.
    static member createFromRect3D (r:Rect3D, zLow, zHigh) =
        let z = Vec.cross(r.Xaxis, r.Yaxis)
        Box(r.Origin  + z.WithLength(zLow),
            r.Xaxis,
            r.Yaxis,
            z.WithLength(zHigh-zLow)
        )

    /// Finds the oriented bounding box in 3D of a set of points.
    /// The orientation of the X-axis is defined by the dirX vector.
    /// The orientation of the Y-axis is defined by the dirY vector.
    static member createFromPlaneAndPoints (pl:PPlane)  (pts:IList<Pnt>) : Box =
        if pts.Count < 2 then EuclidException.Raisef $"Euclid.Box.createFromPlaneAndPoints: cannot create a Box from {pts.Count} points"
        let o = pl.Origin
        let x = pl.Xaxis
        let y = pl.Yaxis
        let z = pl.Zaxis
        let mutable minX = 0.0
        let mutable minY = 0.0
        let mutable maxX = 0.0
        let mutable maxY = 0.0
        let mutable minZ = 0.0
        let mutable maxZ = 0.0
        for i = 1 to pts.Count-1 do
            let v = pts.[i] - o
            let dotX = v *** x
            minX <- min minX dotX
            maxX <- max maxX dotX
            let dotY = v *** y
            minY <- min minY dotY
            maxY <- max maxY dotY
            let dotZ = v *** z
            minZ <- min minZ dotZ
            maxZ <- max maxZ dotZ
        let bo = pl.EvaluateAt(minX, minY, minZ)
        let sizeX = maxX - minX
        let sizeY = maxY - minY
        let sizeZ = maxZ - minZ
        Box(bo, x*sizeX, y*sizeY, z*sizeZ)

    /// Finds the oriented bounding box in 3D of a set of points.
    /// The orientation of the X-axis is defined by the dirX vector.
    /// The orientation of the Y-axis is defined by the dirY vector.
    static member createFromDirsAndPoints (dirX:Vec)  (dirY:Vec)  (pts:IList<Pnt>) : Box =
        if isTooSmallSq dirX.LengthSq then EuclidException.Raisef $"Euclid.Box.createFromDirsAndPoints: dirX too short: {dirX.AsString}"
        if isTooSmallSq dirX.LengthSq then EuclidException.Raisef $"Euclid.Box.createFromDirsAndPoints: dirY too short: {dirY.AsString}"
        if pts.Count < 2 then EuclidException.Raisef $"Euclid.Box.createFromDirsAndPoints: cannot create a Box from {pts.Count} points"
        let pl = PPlane.createOriginXaxisYaxis(pts[0],dirX,dirY)
        Box.createFromPlaneAndPoints pl pts

    /// Creates a 3D box moved by a vector.
    static member move (v:Vec) (b:Box) =
        Box(b.Origin + v, b.Xaxis, b.Yaxis, b.Zaxis)

    /// Creates a 3D box translated along the local X-axis of the Box.
    static member translateX (distX:float) (b:Box) =
        let x = b.Xaxis
        let len = x.Length
        if isTooTiny len then EuclidException.Raisef "Euclid.Box.translateX: box.Xaxis is zero length in Box: %s" b.AsString
        Box(b.Origin + x*(distX/len), x, b.Yaxis, b.Zaxis)

    /// Creates a 3D box translated along the local Y-axis of the Box.
    static member translateY (distY:float) (b:Box) =
        let y = b.Yaxis
        let len = y.Length
        if isTooTiny len then EuclidException.Raisef "Euclid.Box.translateY: box.Yaxis is zero length in Box: %s" b.AsString
        Box(b.Origin + y*(distY/len), b.Xaxis, y, b.Zaxis)

    /// Creates a 3D box translated along the local Z-axis of the Box.
    static member translateZ (distZ:float) (b:Box) =
        let z = b.Zaxis
        let len = z.Length
        if isTooTiny len then EuclidException.Raisef "Euclid.Box.translateZ: box.Zaxis is zero length in Box: %s" b.AsString
        Box(b.Origin + z*(distZ/len), b.Xaxis, b.Yaxis, z)

    /// Transform the Box by the given RigidMatrix.
    /// The returned Box is guaranteed to have still orthogonal vectors.
    static member transform (m:RigidMatrix) (b:Box) =
        let o = Pnt.transformRigid m b.Origin
        let x = Vec.transformRigid m b.Xaxis
        let y = Vec.transformRigid m b.Yaxis
        let z = Vec.transformRigid m b.Zaxis
        Box(o, x, y, z)




