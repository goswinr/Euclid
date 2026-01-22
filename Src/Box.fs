namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open UtilEuclid
open EuclidErrors
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

    /// Unsafe internal constructor, public only for inlining.
    [<Obsolete("This is not Obsolete, but an unsafe internal constructor. the input is not verified, so it might create invalid geometry. It is exposed as a public member so that it can be inlined.") >]
    new (origin, axisX, axisY, axisZ) =
        {Origin=origin; Xaxis=axisX; Yaxis=axisY; Zaxis=axisZ}

    /// Unsafe constructor that creates a Box from origin and three axis vectors.
    /// It does NOT verify the orientation of vectors.
    static member inline createUnchecked (origin, xAxis, yAxis, zAxis) : Box =
        #nowarn "44"
        Box(origin, xAxis, yAxis, zAxis)
        #warnon "44" // re-enable warning for obsolete usage

    [<Obsolete("use SizeX")>]
    member inline b.Width =
        b.Xaxis.Length
    /// The size in X direction.
    member inline b.SizeX : float =
        b.Xaxis.Length

    /// The size in X direction squared.
    member inline b.SizeXSq =
        b.Xaxis.LengthSq


    [<Obsolete("use SizeY")>]
    member inline b.Depth =
        b.Yaxis.Length
    /// The size in Y direction.
    member inline b.SizeY : float =
        b.Yaxis.Length

    /// The size in Y direction squared.
    member inline b.SizeYSq =
        b.Yaxis.LengthSq


    [<Obsolete("use SizeZ")>]
    member inline b.Height3D =
        b.Zaxis.Length
    /// The size in Z direction.
    member inline b.SizeZ : float =
        b.Zaxis.Length

    /// The size in Z direction squared.
    member inline b.SizeZSq =
        b.Zaxis.LengthSq


    /// Nicely formatted string representation of the Box including its size.
    override b.ToString() =
        let sizeX = Format.float b.SizeX
        let sizeY = Format.float b.SizeY
        let sizeZ = Format.float b.SizeZ
        let origin = b.Origin.AsString
        let xAxis = b.Xaxis.AsString
        let yAxis = b.Yaxis.AsString
        let zAxis = b.Zaxis.AsString
        $"Euclid.Box %s{sizeX} x %s{sizeY} x %s{sizeZ} (Origin:%s{origin}| X-ax:%s{xAxis}|Y-ax:%s{yAxis}|Z-ax:%s{zAxis})"


    /// Format Box into string with nice floating point number formatting of X, Y and Z size only.
    /// But without type name as in v.ToString()
    member b.AsString : string =
        let sizeX = Format.float b.SizeX
        let sizeY = Format.float b.SizeY
        let sizeZ = Format.float b.SizeZ
        $"%s{sizeX} x %s{sizeY} x %s{sizeZ}"

    /// Format Box into an F# code string that can be used to recreate the box.
    member b.AsFSharpCode : string =
        $"Box.createUnchecked({b.Origin.AsFSharpCode}, {b.Xaxis.AsFSharpCode}, {b.Yaxis.AsFSharpCode}, {b.Zaxis.AsFSharpCode})"


    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit : UnitVec =
        let a = r.Xaxis
        let x = a.X
        let y = a.Y
        let z = a.Z
        let sqLen = x*x + y*y + z*z
        if isTooTinySq sqLen then
            failTooSmall "Box.XaxisUnit Xaxis" r
        let f = 1.0 / sqrt sqLen
        UnitVec.createUnchecked(x*f, y*f, z*f)


    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit =
        let a = r.Yaxis
        let x = a.X
        let y = a.Y
        let z = a.Z
        let sqLen = x*x + y*y + z*z
        if isTooTinySq sqLen then
            failTooSmall "Box.YaxisUnit Yaxis" r
        let f = 1.0 / sqrt sqLen
        UnitVec.createUnchecked(x*f, y*f, z*f)

    /// Creates a unitized version of the local Z-Axis.
    member inline r.ZaxisUnit =
        let a = r.Zaxis
        let x = a.X
        let y = a.Y
        let z = a.Z
        let sqLen = x*x + y*y + z*z
        if isTooTinySq sqLen then
            failTooSmall "Box.ZaxisUnit Zaxis" r
        let f = 1.0 / sqrt sqLen
        UnitVec.createUnchecked(x*f, y*f, z*f)


    /// The corner diagonally opposite of corner from Origin.
    member inline b.FarCorner =
        b.Origin + b.Xaxis + b.Yaxis + b.Zaxis

    /// The diagonal vector of the Box.
    member inline b.Diagonal =
        b.Xaxis + b.Yaxis + b.Zaxis

    /// The center of the Box.
    member inline b.Center : Pnt =
        b.Origin + b.Xaxis*0.5 + b.Yaxis*0.5 + b.Zaxis*0.5

    /// Evaluate a X, Y and Z parameter of the Box.
    ///  0.0, 0.0, 0.0 returns the Origin.
    ///  1.0, 1.0, 1.0 returns the FarCorner.
    member inline b.EvaluateAt (xParameter:float, yParameter:float, zParameter:float) =
        b.Origin + b.Xaxis * xParameter + b.Yaxis * yParameter + b.Zaxis * zParameter


    /// Calculates the volume of the Box.
    member inline b.Volume : float =
        b.Xaxis.Length*b.Yaxis.Length*b.Zaxis.Length


     /// Calculates the squared volume of the Box.
    /// by using the squared lengths of the X , Y and Z axis.
    /// This is a bit faster than calculating the volume and good enough for relative comparisons or sorting by size.
    /// r.Xaxis.LengthSq * r.Yaxis.LengthSq * r.Zaxis.LengthSq
    [<Obsolete("this ia actually the volume, also this does not scale proportionally, use .Volume")>]
    member inline r.AreaSq =
        r.Xaxis.LengthSq * r.Yaxis.LengthSq * r.Zaxis.LengthSq

    /// Returns the longest edge of the Box.
    member inline b.LongestEdge : float =
        let x = b.Xaxis.LengthSq
        let y = b.Yaxis.LengthSq
        let z = b.Zaxis.LengthSq
        sqrt (max (max x y) z)

    /// Returns the shortest edge of the Box.
    member inline b.ShortestEdge : float =
        let x = b.Xaxis.LengthSq
        let y = b.Yaxis.LengthSq
        let z = b.Zaxis.LengthSq
        sqrt (min (min x y) z)


    /// Returns the square length of longest edge of the Box.
    member inline b.LongestEdgeSq =
        let x = b.Xaxis.LengthSq
        let y = b.Yaxis.LengthSq
        let z = b.Zaxis.LengthSq
        max (max x y) z

    /// Returns the square length of shortest edge of the Box.
    member inline b.ShortestEdgeSq =
        let x = b.Xaxis.LengthSq
        let y = b.Yaxis.LengthSq
        let z = b.Zaxis.LengthSq
        min (min x y) z

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsPoint.
    member inline b.IsZero : bool =
        isTooTinySq b.Xaxis.LengthSq &&
        isTooTinySq b.Yaxis.LengthSq &&
        isTooTinySq b.Zaxis.LengthSq

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsZero.
    member inline b.IsPoint : bool =
        isTooTinySq b.Xaxis.LengthSq &&
        isTooTinySq b.Yaxis.LengthSq &&
        isTooTinySq b.Zaxis.LengthSq


    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, 2 or 3.
    member inline b.CountZeroSides : int =
        countTooTinySqOrNaN    b.Xaxis.LengthSq
        +  countTooTinySqOrNaN b.Yaxis.LengthSq
        +  countTooTinySqOrNaN b.Zaxis.LengthSq


    /// Tests if two of the X, Y and Z axis is smaller than the zeroLength tolerance.
    member inline b.IsLine : bool =
        b.CountZeroSides = 2

    /// Tests if one of the X, Y and Z axis is smaller than the zeroLength tolerance.
    member inline b.IsFlat : bool =
        b.CountZeroSides = 1

    /// Tests if no sides of the X, Y and Z axis is smaller than the zeroLength tolerance.
    /// Same as .HasVolume
    member inline b.IsValid : bool =
        b.CountZeroSides = 0

    /// Tests if none of the X, Y and Z axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasVolume : bool =
        b.CountZeroSides = 0


    /// Gets the Plane that this box is based on.
    member inline b.Plane =
        let x = b.Xaxis.Unitized
        let y = b.Yaxis.Unitized
        let z = b.Zaxis.Unitized
        PPlane.createUnchecked (b.Origin, x, y, z)

    /// Scales the Box by a given factor.
    /// Scale center is World Origin 0,0
    member inline b.Scale (factor:float) : Box =
        Box.createUnchecked(
            b.Origin * factor,
            b.Xaxis * factor,
            b.Yaxis * factor,
            b.Zaxis * factor
        )

    // Scales the Box by a given factor on a given center point
    member inline l.ScaleOn (cen:Pnt) (factor:float) : Box =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        let o = l.Origin
        Box.createUnchecked(
            Pnt(cx + (o.X - cx) * factor,
                cy + (o.Y - cy) * factor,
                cz + (o.Z - cz) * factor),
            l.Xaxis * factor,
            l.Yaxis * factor,
            l.Zaxis * factor
        )

    /// Returns a 3D box moved by a vector.
    member inline b.Move (v:Vec) =
        Box.createUnchecked(b.Origin + v, b.Xaxis, b.Yaxis, b.Zaxis)

    /// Returns a 3D box moved by a given distance in X direction.
    member inline b.MoveX (distance:float) =
        Box.createUnchecked(Pnt(b.Origin.X + distance, b.Origin.Y, b.Origin.Z), b.Xaxis, b.Yaxis, b.Zaxis)

    /// Returns a 3D box moved by a given distance in Y direction.
    member inline b.MoveY (distance:float) =
        Box.createUnchecked(Pnt(b.Origin.X, b.Origin.Y + distance, b.Origin.Z), b.Xaxis, b.Yaxis, b.Zaxis)

    /// Returns a 3D box moved by a given distance in Z direction.
    member inline b.MoveZ (distance:float) =
        Box.createUnchecked(Pnt(b.Origin.X, b.Origin.Y, b.Origin.Z + distance), b.Xaxis, b.Yaxis, b.Zaxis)

    /// Applies or multiplies a 4x4 transformation matrix to a 3D box.
    /// The returned box may not have orthogonal axis vectors anymore for non-IsRigid matrices.
    member inline b.Transform (m:Matrix) =
        let o = b.Origin *** m
        let x = Vec.transform m b.Xaxis
        let y = Vec.transform m b.Yaxis
        let z = Vec.transform m b.Zaxis
        Box.createUnchecked(o, x, y, z)

    /// Multiplies (or applies) a RigidMatrix to a 3D box.
    /// The returned Box is guaranteed to have still orthogonal vectors.
    member inline b.TransformRigid (m:RigidMatrix) =
        let o = b.Origin *** m
        let x = Vec.transformRigid m b.Xaxis
        let y = Vec.transformRigid m b.Yaxis
        let z = Vec.transformRigid m b.Zaxis
        Box.createUnchecked(o, x, y, z)

    /// Multiplies (or applies) a Quaternion to a 3D box.
    /// The box is rotated around the World Origin.
    member inline b.Rotate (q:Quaternion) =
        let o = b.Origin *** q
        let x = b.Xaxis *** q
        let y = b.Yaxis *** q
        let z = b.Zaxis *** q
        Box.createUnchecked(o, x, y, z)

    /// Multiplies (or applies) a Quaternion to a 3D box around a given center point.
    member inline b.RotateWithCenter (cen:Pnt, q:Quaternion) =
        let o = Pnt.rotateWithCenterByQuat cen q b.Origin
        let x = b.Xaxis *** q
        let y = b.Yaxis *** q
        let z = b.Zaxis *** q
        Box.createUnchecked(o, x, y, z)


    /// Check for point containment in the Box.
    /// By doing 6 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    member b.Contains(p:Pnt) =
        let x = b.Xaxis
        let y = b.Yaxis
        let z = b.Zaxis
        let p0 = b.Origin
        let v = p - p0
        let p1 = p0 + x
        let p3 = p0 + y
        let p4 = p0 + z
        v *** x >= 0.
        &&
        v *** y >= 0.
        &&
        v *** z >= 0.
        &&
        (p - p3) *** y <= 0.
        &&
        (p - p1) *** x <= 0.
        &&
        (p - p4) *** z <= 0.


    /// Gets the axis aligned 3D Bounding Box of the Box.
    member b.BBox =
        let p0 = b.Origin
        let p1 = p0 + b.Xaxis
        let y = b.Yaxis
        let p2 = p1 + y
        let p3 = p0 + y
        let z = b.Zaxis
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


    /// Returns the Area of the biggest face of the box.
    /// This is the biggest of the three faces X*Y, X*Z and Y*Z.
    member b.AreaOfBiggestFace =
        let x = b.Xaxis.Length
        let y = b.Yaxis.Length
        let z = b.Zaxis.Length
        max (x * y) (max (x * z) (y * z))

    /// Returns the Area of the smallest face of the box.
    /// This is the smallest of the three faces X*Y, X*Z and Y*Z.
    member b.AreaOfSmallestFace =
        let x = b.Xaxis.Length
        let y = b.Yaxis.Length
        let z = b.Zaxis.Length
        min (x * y) (min (x * z) (y * z))


    //            █████               █████     ███                                                       █████
    //           ░░███               ░░███     ░░░                                                       ░░███
    //    █████  ███████    ██████   ███████   ████   ██████     █████████████    ██████  █████████████   ░███████   ██████  ████████   █████
    //   ███░░  ░░░███░    ░░░░░███ ░░░███░   ░░███  ███░░███   ░░███░░███░░███  ███░░███░░███░░███░░███  ░███░░███ ███░░███░░███░░███ ███░░
    //  ░░█████   ░███      ███████   ░███     ░███ ░███ ░░░     ░███ ░███ ░███ ░███████  ░███ ░███ ░███  ░███ ░███░███████  ░███ ░░░ ░░█████
    //   ░░░░███  ░███ ███ ███░░███   ░███ ███ ░███ ░███  ███    ░███ ░███ ░███ ░███░░░   ░███ ░███ ░███  ░███ ░███░███░░░   ░███      ░░░░███
    //   ██████   ░░█████ ░░████████  ░░█████  █████░░██████     █████░███ █████░░██████  █████░███ █████ ████████ ░░██████  █████     ██████
    //  ░░░░░░     ░░░░░   ░░░░░░░░    ░░░░░  ░░░░░  ░░░░░░     ░░░░░ ░░░ ░░░░░  ░░░░░░  ░░░░░ ░░░ ░░░░░ ░░░░░░░░   ░░░░░░  ░░░░░     ░░░░░░


    /// Check for point containment in the Box.
    /// By doing 6 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    static member inline contains (p:Pnt) (b:Box)  : bool = b.Contains p

    /// Checks if two 3D-boxes are equal within tolerance.
    /// Does not recognize congruent boxes with different rotation as equal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:Box) (b:Box)  : bool =
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
    static member notEquals (tol:float) (a:Box) (b:Box)  : bool =
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
    static member expand dist (b:Box)  : Box =
        let siX = b.SizeX
        let siY = b.SizeY
        let hei = b.SizeZ
        let d = dist * -2.0
        if siX<=d || siY<=d || hei<=d then
            fail $"Box.expand: Box {b.AsString} is too small to expand by negative (=shrink) distance {dist}"
        let x = b.Xaxis * (dist / siX)
        let y = b.Yaxis * (dist / siY)
        let z = b.Zaxis * (dist / hei)
        Box.createUnchecked(b.Origin-x-y-z, b.Xaxis+x*2., b.Yaxis+y*2., b.Zaxis+z*2.)

    /// Returns Box expanded by respective distances on all six sides.
    /// Does check for overflow if distance is negative and fails.
    /// distX, distY and distZ are for X, Y and Z-axis respectively.
    static member expandXYZ distX distY distZ (b:Box) =
        let siX = b.SizeX
        let siY = b.SizeY
        let hei = b.SizeZ
        if siX <= distX * -2.0 then fail $"Box.expandXYZ: Box {b.AsString} is too small to expand by negative (=shrink) distance distX {distX}"
        if siY <= distY * -2.0 then fail $"Box.expandXYZ: Box {b.AsString} is too small to expand by negative (=shrink) distance distY {distY}"
        if hei <= distZ * -2.0 then fail $"Box.expandXYZ: Box {b.AsString} is too small to expand by negative (=shrink) distance distZ {distZ}"
        let x = b.Xaxis * (distX / b.SizeX)
        let y = b.Yaxis * (distY / b.SizeY )
        let z = b.Zaxis * (distZ / b.SizeZ)
        Box.createUnchecked(b.Origin-x-y-z, b.Xaxis+x*2., b.Yaxis+y*2., b.Zaxis+z*2.)


    /// Returns the 3D box expanded by a relative factor on all six sides.
    /// Values between 0.0 and 1.0 shrink the box.
    /// Values larger than 1.0 expand the box.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRel factor (b:Box)  : Box =
        if factor < 0.0 then
            fail $"Box.expandRel: a negative factor {factor} is not allowed for expanding the 3D box {b.AsString}"
        let x = b.Xaxis * factor
        let y = b.Yaxis * factor
        let z = b.Zaxis * factor
        Box.createUnchecked(b.Center - x*0.5 - y*0.5 - z*0.5, x, y, z)

    /// Returns the 3D box expanded by a relative factor on all six sides, separately for X, Y, Z.
    /// Values between 0.0 and 1.0 shrink the box.
    /// Values larger than 1.0 expand the box.
    /// Does check for underflow if any factor is negative and raises EuclidException.
    static member expandRelXYZ factorX factorY factorZ (b:Box)  : Box =
        if factorX < 0.0 then
            fail $"Box.expandRelXYZ: a negative factorX {factorX} is not allowed for expanding the 3D box {b.AsString}"
        if factorY < 0.0 then
            fail $"Box.expandRelXYZ: a negative factorY {factorY} is not allowed for expanding the 3D box {b.AsString}"
        if factorZ < 0.0 then
            fail $"Box.expandRelXYZ: a negative factorZ {factorZ} is not allowed for expanding the 3D box {b.AsString}"
        let x = b.Xaxis * factorX
        let y = b.Yaxis * factorY
        let z = b.Zaxis * factorZ
        Box.createUnchecked(b.Center - x*0.5 - y*0.5 - z*0.5, x, y, z)



    /// Creates a 3D box from PPlane and x, y and Z size.
    static member inline createFromPlane x y z (pl:PPlane) =
        Box.createUnchecked(pl.Origin, pl.Xaxis*x, pl.Yaxis*y, pl.Zaxis*z)

    /// Creates a 3D box from a 3D a bounding box.
    static member inline createFromBoundingBox (b:BBox) =
        Box.createUnchecked(b.MinPnt, Vec.Xaxis*b.SizeX, Vec.Yaxis*b.SizeY, Vec.Zaxis*b.SizeZ)

    /// Creates a 3D box from a 2D rectangle and Z lower and upper position.
    static member createFromRect2D zLow zHigh (r:Rect2D) =
        Box.createUnchecked(r.Origin.WithZ zLow,
            r.Xaxis.AsVec,
            r.Yaxis.AsVec,
            Vec.Zaxis*(zHigh-zLow)
        )

     /// Creates a 3D box from a 3D rectangle and Z lower and upper position.
    static member inline createFromRect3D zLow zHigh (r:Rect3D) =
        let z = Vec.cross(r.Xaxis, r.Yaxis)
        Box.createUnchecked(r.Origin  + z.WithLength(zLow),
            r.Xaxis,
            r.Yaxis,
            z.WithLength(zHigh-zLow)
        )

    /// Finds the oriented bounding box in 3D of a set of points.
    /// The orientation of the X-axis is defined by the dirX vector.
    /// The orientation of the Y-axis is defined by the dirY vector.
    static member createFromPlaneAndPoints (pl:PPlane)  (pts:IList<Pnt>) : Box =
        if pts.Count < 2 then fail $"Box.createFromPlaneAndPoints: cannot create a Box from just {pts.Count} points"
        let o = pl.Origin
        let x = pl.Xaxis
        let y = pl.Yaxis
        let z = pl.Zaxis
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        let mutable minZ = Double.MaxValue
        let mutable maxZ = Double.MinValue
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
        Box.createUnchecked(bo, x*sizeX, y*sizeY, z*sizeZ)

    /// Finds the oriented bounding box in 3D of a set of points.
    /// The orientation of the X-axis is defined by the dirX vector.
    /// The orientation of the Y-axis is defined by the dirY vector.
    static member createFromDirsAndPoints (dirX:Vec)  (dirY:Vec)  (pts:IList<Pnt>) : Box =
        if isTooSmallSq dirX.LengthSq then failTooSmall "Euclid.Box.createFromDirsAndPoints: dirX too short" dirX
        if isTooSmallSq dirY.LengthSq then failTooSmall "Euclid.Box.createFromDirsAndPoints: dirY too short" dirY
        if pts.Count < 2              then fail $"Box.createFromDirsAndPoints: cannot create a Box from just {pts.Count} points"
        let pl = PPlane.createOriginXaxisYaxis(pts[0],dirX,dirY)
        Box.createFromPlaneAndPoints pl pts

    /// Creates a 3D box moved by a vector.
    static member inline move (v:Vec) (b:Box)  : Box =
        Box.createUnchecked(b.Origin + v, b.Xaxis, b.Yaxis, b.Zaxis)

    /// Creates a 3D box translated along the local X-axis of the Box.
    static member translateLocalX (distX:float) (b:Box) =
        let x = b.Xaxis
        let len = x.Length
        if isTooTiny len then failTooSmall "Box.translateLocalX Xaxis " b
        Box.createUnchecked(b.Origin + x*(distX/len), x, b.Yaxis, b.Zaxis)

    /// Creates a 3D box translated along the local Y-axis of the Box.
    static member translateLocalY (distY:float) (b:Box) =
        let y = b.Yaxis
        let len = y.Length
        if isTooTiny len then failTooSmall "Box.translateLocalY Yaxis" b
        Box.createUnchecked(b.Origin + y*(distY/len), b.Xaxis, y, b.Zaxis)

    /// Creates a 3D box translated along the local Z-axis of the Box.
    static member translateLocalZ (distZ:float) (b:Box) =
        let z = b.Zaxis
        let len = z.Length
        if isTooTiny len then failTooSmall "Box.translateLocalZ Zaxis" b
        Box.createUnchecked(b.Origin + z*(distZ/len), b.Xaxis, b.Yaxis, z)

    /// Scales the 3D box by a given factor.
    /// Scale center is World Origin 0,0,0
    static member inline scale (factor:float) (b:Box) : Box =
        Box.createUnchecked(
            b.Origin * factor,
            b.Xaxis * factor,
            b.Yaxis * factor,
            b.Zaxis * factor
        )

    /// Move a 3D box by a vector. Same as Box.translate.
    static member inline translate (v:Vec) (b:Box) =
        Box.createUnchecked(b.Origin + v, b.Xaxis, b.Yaxis, b.Zaxis)

    /// Returns the 3D box moved by a given distance in X direction.
    static member inline moveX (distance:float) (b:Box) =
        Box.createUnchecked(Pnt(b.Origin.X + distance, b.Origin.Y, b.Origin.Z), b.Xaxis, b.Yaxis, b.Zaxis)

    /// Returns the 3D box moved by a given distance in Y direction.
    static member inline moveY (distance:float) (b:Box) =
        Box.createUnchecked(Pnt(b.Origin.X, b.Origin.Y + distance, b.Origin.Z), b.Xaxis, b.Yaxis, b.Zaxis)

    /// Returns the 3D box moved by a given distance in Z direction.
    static member inline moveZ (distance:float) (b:Box) =
        Box.createUnchecked(Pnt(b.Origin.X, b.Origin.Y, b.Origin.Z + distance), b.Xaxis, b.Yaxis, b.Zaxis)

    /// Applies or multiplies a 4x4 transformation matrix to a 3D box.
    /// The returned box may not have orthogonal axis vectors anymore for non-IsRigid matrices.
    static member inline transform (m:Matrix) (b:Box) =
        let o = b.Origin *** m
        let x = Vec.transform m b.Xaxis
        let y = Vec.transform m b.Yaxis
        let z = Vec.transform m b.Zaxis
        Box.createUnchecked(o, x, y, z)

    /// Transforms the Box by the given RigidMatrix.
    /// The returned Box is guaranteed to have still orthogonal vectors.
    static member inline transformRigid (m:RigidMatrix) (b:Box) =
        let o = Pnt.transformRigid m b.Origin
        let x = Vec.transformRigid m b.Xaxis
        let y = Vec.transformRigid m b.Yaxis
        let z = Vec.transformRigid m b.Zaxis
        Box.createUnchecked(o, x, y, z)

    /// Multiplies (or applies) a Quaternion to a 3D box.
    /// The box IsRotated around the World Origin.
    static member inline rotate (q:Quaternion) (b:Box) =
        let o = b.Origin *** q
        let x = b.Xaxis *** q
        let y = b.Yaxis *** q
        let z = b.Zaxis *** q
        Box.createUnchecked(o, x, y, z)

    /// Multiplies (or applies) a Quaternion to a 3D box around a given center point.
    static member inline rotateWithCenter (cen:Pnt) (q:Quaternion) (b:Box) =
        let o = Pnt.rotateWithCenterByQuat cen q b.Origin
        let x = b.Xaxis *** q
        let y = b.Yaxis *** q
        let z = b.Zaxis *** q
        Box.createUnchecked(o, x, y, z)


    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // ------------------------Intersection---------------------------------------------------------------------------------------------------------------------------------------
    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


    /// <summary>Intersects an infinite ray (Line3D extended infinitely in both directions) with this Box.
    /// Uses the slab intersection method in the box's local coordinate system.
    /// Returns None if the ray does not intersect the box or if the ray direction is too short.
    /// Returns Some with entry and exit parameters on the ray if it intersects.
    /// A parameter of 0.0 corresponds to the ray's From point, 1.0 to its To point.</summary>
    /// <param name="ray">The ray (Line3D) to intersect with the box.</param>
    /// <returns>None if no intersection, Some(tEntry, tExit) with the entry and exit parameters on the ray.</returns>
    member b.IntersectRay(ray:Line3D) : Option<float*float> =
        // Transform ray to box's local coordinate system
        let rayDir = ray.Direction
        let rayDirLenSq = rayDir.LengthSq
        if isTooSmallSq rayDirLenSq then
            None // Ray direction too short
        else
            let rayOrigin = ray.From - b.Origin

            // Get box axes and their squared lengths
            let xAxis = b.Xaxis
            let yAxis = b.Yaxis
            let zAxis = b.Zaxis
            let xLenSq = xAxis.LengthSq
            let yLenSq = yAxis.LengthSq
            let zLenSq = zAxis.LengthSq

            // Initialize tMin and tMax
            let mutable tMin = Double.MinValue
            let mutable tMax = Double.MaxValue

            // Process X-axis slab
            if isTooTinySq xLenSq then
                // Box is flat in X direction, check if ray origin is within slab
                let originDotX = rayOrigin *** xAxis
                if originDotX < 0.0 || originDotX > xLenSq then
                    tMin <- Double.MaxValue // Force no intersection
            else
                let dirDotX = rayDir *** xAxis
                let originDotX = rayOrigin *** xAxis
                if abs dirDotX < 1e-18 then
                    // Ray is parallel to X slab
                    if originDotX < 0.0 || originDotX > xLenSq then
                        tMin <- Double.MaxValue // Force no intersection
                else
                    let t1 = -originDotX / dirDotX
                    let t2 = (xLenSq - originDotX) / dirDotX
                    if t1 < t2 then
                        tMin <- max tMin t1
                        tMax <- min tMax t2
                    else
                        tMin <- max tMin t2
                        tMax <- min tMax t1

            // Process Y-axis slab
            if tMin <= tMax then
                if isTooTinySq yLenSq then
                    let originDotY = rayOrigin *** yAxis
                    if originDotY < 0.0 || originDotY > yLenSq then
                        tMin <- Double.MaxValue
                else
                    let dirDotY = rayDir *** yAxis
                    let originDotY = rayOrigin *** yAxis
                    if abs dirDotY < 1e-18 then
                        if originDotY < 0.0 || originDotY > yLenSq then
                            tMin <- Double.MaxValue
                    else
                        let t1 = -originDotY / dirDotY
                        let t2 = (yLenSq - originDotY) / dirDotY
                        if t1 < t2 then
                            tMin <- max tMin t1
                            tMax <- min tMax t2
                        else
                            tMin <- max tMin t2
                            tMax <- min tMax t1

            // Process Z-axis slab
            if tMin <= tMax then
                if isTooTinySq zLenSq then
                    let originDotZ = rayOrigin *** zAxis
                    if originDotZ < 0.0 || originDotZ > zLenSq then
                        tMin <- Double.MaxValue
                else
                    let dirDotZ = rayDir *** zAxis
                    let originDotZ = rayOrigin *** zAxis
                    if abs dirDotZ < 1e-18 then
                        if originDotZ < 0.0 || originDotZ > zLenSq then
                            tMin <- Double.MaxValue
                    else
                        let t1 = -originDotZ / dirDotZ
                        let t2 = (zLenSq - originDotZ) / dirDotZ
                        if t1 < t2 then
                            tMin <- max tMin t1
                            tMax <- min tMax t2
                        else
                            tMin <- max tMin t2
                            tMax <- min tMax t1

            if tMin <= tMax then
                Some(tMin, tMax)
            else
                None


    /// <summary>Intersects an infinite ray (Line3D extended infinitely in both directions) with the Box.
    /// Uses the slab intersection method in the box's local coordinate system.
    /// Returns None if the ray does not intersect the box or if the ray direction is too short.
    /// Returns Some with entry and exit parameters on the ray if it intersects.
    /// A parameter of 0.0 corresponds to the ray's From point, 1.0 to its To point.</summary>
    /// <param name="ray">The ray (Line3D) to intersect with the box.</param>
    /// <param name="box">The box to intersect with.</param>
    /// <returns>None if no intersection, Some(tEntry, tExit) with the entry and exit parameters on the ray.</returns>
    static member inline intersectRay (ray:Line3D) (box:Box) : Option<float*float> =
        box.IntersectRay(ray)




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



    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // ------------------------Faces----------------------------------------------------------------------------------------------------------------------------------------------
    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


    /// Returns 6 face of the Box in
    /// The normal of the Rect3Ds are oriented with the X-Axis, Y-Axis or Z-Axis.
    /// The order of the Rect3D is: BottomFace, FrontFace, RightFace, BackFace, LeftFace, TopFace.
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
    member b.Faces:Rect3D[] =

        [|
        b.BottomFace
        b.FrontFace
        b.RightFace
        b.BackFace
        b.LeftFace
        b.TopFace
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
    member b.TopFace :Rect3D = Rect3D.createUnchecked(b.Origin + b.Zaxis, b.Xaxis, b.Yaxis)


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
    member b.BottomFace = Rect3D.createUnchecked(b.Origin, b.Xaxis, b.Yaxis)



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
    member b.FrontFace = Rect3D.createUnchecked(b.Origin, b.Xaxis, b.Zaxis)

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
    member b.BackFace = Rect3D.createUnchecked(b.Origin + b.Yaxis, b.Xaxis, b.Zaxis)

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
    member b.RightFace = Rect3D.createUnchecked(b.Origin + b.Xaxis, b.Yaxis, b.Zaxis)

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
    member b.LeftFace = Rect3D.createUnchecked(b.Origin, b.Yaxis, b.Zaxis)



    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // ------------------------Edges----------------------------------------------------------------------------------------------------------------------------------------------
    // ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    /// Returns the 12 box edges.
    /// The returned line is parallel to and oriented with the X-Axis, Y-Axis or Z-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member b.Edges :Line3D[] =
        let p0 = b.Origin
        let p1 = p0 + b.Xaxis
        let p4 = p0 + b.Zaxis
        let p5 = p4 + b.Xaxis
        let y = b.Yaxis
        let p2 = p1 + y
        let p3 = p0 + y
        let p6 = p5 + y
        let p7 = p4 + y

        [|
        Line3D(p0, p1) // E0
        Line3D(p1, p2) // E1
        Line3D(p3,p2) // E2
        Line3D(p0,p3) // E3
        Line3D(p4,p5) // E4
        Line3D(p5,p6) // E5
        Line3D(p7,p6) // E6
        Line3D(p4,p7) // E7
        Line3D(p0,p4) // E8
        Line3D(p1,p5) // E9
        Line3D(p2,p6) // E10
        Line3D(p3,p7) // E11
        |]



    /// Returns edge 1 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the X-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge0 =
        let st = b.Origin
        Line3D(st, st + b.Xaxis)

    /// Returns edge 1 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the Y-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge1 =
        let st = b.Origin + b.Xaxis
        Line3D(st, st + b.Yaxis)

    /// Returns edge 2 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the X-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge2 =
        let st = b.Origin + b.Yaxis
        Line3D(st, st + b.Xaxis)

    /// Returns edge 3 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the Y-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge3 =
        let st = b.Origin
        Line3D(st, st + b.Yaxis)

    /// Returns edge 4 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the X-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge4 =
        let st = b.Origin + b.Zaxis
        Line3D(st, st + b.Xaxis)

    /// Returns edge 5 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the Y-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge5 =
        let st = b.Origin + b.Zaxis + b.Xaxis
        Line3D(st, st + b.Yaxis)

    /// Returns edge 6 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the X-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge6 =
        let st = b.Origin + b.Zaxis + b.Yaxis
        Line3D(st, st + b.Xaxis)

    /// Returns edge 7 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the Y-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge7 =
        let st = b.Origin + b.Zaxis
        Line3D(st, st + b.Yaxis)

    /// Returns edge 8 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the Z-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge8 =
        let st = b.Origin
        Line3D(st, st + b.Zaxis)

    /// Returns edge 9 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the Z-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge9 =
        let st = b.Origin + b.Xaxis
        Line3D(st, st + b.Zaxis)

    /// Returns edge 10 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the Z-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge10 =
        let st = b.Origin + b.Xaxis + b.Yaxis
        Line3D(st, st + b.Zaxis)

    /// Returns edge 11 (of the 12 box edges.)
    /// The returned line is parallel to and oriented with the Z-Axis.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |          /
    ///   |   +--------E6-----+
    ///   |  /|    /         /|
    ///   |E7 E11 /         E5|
    ///   |/  |  /         /  |
    ///   +--------E4-----+   E10
    ///   |   |/          E9  |
    ///   E8  +-----E2----|---+
    ///   |  /            |  /
    ///   | E3            | E1
    ///   |/              |/       local
    ///   +------E0 ------+------> X-Axis
    ///
    member inline b.Edge11 =
        let st = b.Origin + b.Yaxis
        Line3D(st, st + b.Zaxis)
