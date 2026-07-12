namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open UtilEuclid
open EuclidErrors
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open System.Collections.Generic


/// <summary>A struct of 9 floats  representing an immutable 3D Box with any rotation in 3D space.
/// Described by z, y and z of the Origin and x, y and z for each of the three Edge vectors.
/// Similar to PPlane, however the three vectors are not unitized.
/// This implementation guarantees the box to be always valid.
/// That means the Min X, Y and Z axes cannot be flipped individually.
/// However the length of one of these axes might still be zero.
/// <code>
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
///

/// </code>
/// </summary>
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Box =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The X coordinate of the Origin Corner of the Box.
    [<DataMember>] val public OriginX: float

    /// The Y coordinate of the Origin Corner of the Box.
    [<DataMember>] val public OriginY: float

    /// The Z coordinate of the Origin Corner of the Box.
    [<DataMember>] val public OriginZ: float

    /// The X component of the edge vector representing the X-axis of the Box.
    [<DataMember>] val public XaxisX: float

    /// The Y component of the edge vector representing the X-axis of the Box.
    [<DataMember>] val public XaxisY: float

    /// The Z component of the edge vector representing the X-axis of the Box.
    [<DataMember>] val public XaxisZ: float

    /// The X component of the edge vector representing the Y-axis of the Box.
    [<DataMember>] val public YaxisX: float

    /// The Y component of the edge vector representing the Y-axis of the Box.
    [<DataMember>] val public YaxisY: float

    /// The Z component of the edge vector representing the Y-axis of the Box.
    [<DataMember>] val public YaxisZ: float

    /// The X component of the edge vector representing the Z-axis of the Box.
    [<DataMember>] val public ZaxisX: float

    /// The Y component of the edge vector representing the Z-axis of the Box.
    [<DataMember>] val public ZaxisY: float

    /// The Z component of the edge vector representing the Z-axis of the Box.
    [<DataMember>] val public ZaxisZ: float

    /// Unsafe internal constructor, public only for inlining.
    /// Creates a Box from origin coordinates and X, Y and Z axis vector components.
    [<Obsolete("This is not Obsolete, but an unsafe internal constructor. the input is not verified, so it might create invalid geometry. It is exposed as a public member so that it can be inlined.") >]
    new (originX:float, originY:float, originZ:float,
         xAxisX:float, xAxisY:float, xAxisZ:float,
         yAxisX:float, yAxisY:float, yAxisZ:float,
         zAxisX:float, zAxisY:float, zAxisZ:float) =
           {OriginX=originX; OriginY=originY; OriginZ=originZ
            XaxisX=xAxisX; XaxisY=xAxisY; XaxisZ=xAxisZ
            YaxisX=yAxisX; YaxisY=yAxisY; YaxisZ=yAxisZ
            ZaxisX=zAxisX; ZaxisY=zAxisY; ZaxisZ=zAxisZ}

    /// Create a Box from origin coordinates and X, Y and Z axis vector components.
    /// It does NOT verify the orientation of vectors.
    static member inline createUnchecked (originX:float, originY:float, originZ:float,
                                             xAxisX:float, xAxisY:float, xAxisZ:float,
                                             yAxisX:float, yAxisY:float, yAxisZ:float,
                                             zAxisX:float, zAxisY:float, zAxisZ:float) : Box =
        #nowarn "44"
        Box(originX, originY, originZ, xAxisX, xAxisY, xAxisZ, yAxisX, yAxisY, yAxisZ, zAxisX, zAxisY, zAxisZ)
        #warnon "44" // re-enable warning for obsolete usage

    /// Unsafe constructor that creates a Box from origin point and three axis vectors.
    /// It does NOT verify the orientation of vectors.
    static member inline createUncheckedVec (origin:Pnt, xAxis:Vec, yAxis:Vec, zAxis:Vec) : Box =
        Box.createUnchecked(origin.X, origin.Y, origin.Z, xAxis.X, xAxis.Y, xAxis.Z, yAxis.X, yAxis.Y, yAxis.Z, zAxis.X, zAxis.Y, zAxis.Z)

    /// Creates a 3D Point from b.OriginX, b.OriginY and b.OriginZ
    member b.Origin : Pnt =
        Pnt(b.OriginX, b.OriginY, b.OriginZ)

    /// Creates a 3D Point from b.OriginX, b.OriginY and b.OriginZ
    static member inline origin (b:Box) : Pnt =
        b.Origin

    /// Creates a 3D Vector from b.XaxisX, b.XaxisY and b.XaxisZ
    member b.Xaxis : Vec =
        Vec(b.XaxisX, b.XaxisY, b.XaxisZ)

    /// Creates a 3D Vector from b.YaxisX, b.YaxisY and b.YaxisZ
    member b.Yaxis : Vec =
        Vec(b.YaxisX, b.YaxisY, b.YaxisZ)

    /// Creates a 3D Vector from b.ZaxisX, b.ZaxisY and b.ZaxisZ
    member b.Zaxis : Vec =
        Vec(b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Creates a 3D Vector from b.XaxisX, b.XaxisY and b.XaxisZ
    static member inline xAxis (b:Box) : Vec =
        b.Xaxis

    /// Creates a 3D Vector from b.YaxisX, b.YaxisY and b.YaxisZ
    static member inline yAxis (b:Box) : Vec =
        b.Yaxis

    /// Creates a 3D Vector from b.ZaxisX, b.ZaxisY and b.ZaxisZ
    static member inline zAxis (b:Box) : Vec =
        b.Zaxis

    /// The size in X direction.
    member inline b.SizeX : float =
        XYZ.length b.XaxisX b.XaxisY b.XaxisZ

    /// The size in X direction.
    static member inline sizeX (b:Box) : float =
        b.SizeX

    /// The size in X direction squared.
    member inline b.SizeXSq : float =
        XYZ.sqLength b.XaxisX b.XaxisY b.XaxisZ

    /// The size in X direction squared.
    static member inline sizeXSq (b:Box) : float =
        b.SizeXSq

    /// The size in Y direction.
    member inline b.SizeY : float =
        XYZ.length b.YaxisX b.YaxisY b.YaxisZ

    /// The size in Y direction.
    static member inline sizeY (b:Box) : float =
        b.SizeY

    /// The size in Y direction squared.
    member inline b.SizeYSq : float =
        XYZ.sqLength b.YaxisX b.YaxisY b.YaxisZ

    /// The size in Y direction squared.
    static member inline sizeYSq (b:Box) : float =
        b.SizeYSq

    /// The size in Z direction.
    member inline b.SizeZ : float =
        XYZ.length b.ZaxisX b.ZaxisY b.ZaxisZ

    /// The size in Z direction.
    static member inline sizeZ (b:Box) : float =
        b.SizeZ

    /// The size in Z direction squared.
    member inline b.SizeZSq : float =
        XYZ.sqLength b.ZaxisX b.ZaxisY b.ZaxisZ

    /// The size in Z direction squared.
    static member inline sizeZSq (b:Box) : float =
        b.SizeZSq

    /// Nicely formatted string representation of the Box including its size.
    override b.ToString() : string =
        let sizeX = Format.float b.SizeX
        let sizeY = Format.float b.SizeY
        let sizeZ = Format.float b.SizeZ
        let origin = $"X={Format.float b.OriginX}|Y={Format.float b.OriginY}|Z={Format.float b.OriginZ}"
        let xAxis = $"X={Format.float b.XaxisX}|Y={Format.float b.XaxisY}|Z={Format.float b.XaxisZ}"
        let yAxis = $"X={Format.float b.YaxisX}|Y={Format.float b.YaxisY}|Z={Format.float b.YaxisZ}"
        let zAxis = $"X={Format.float b.ZaxisX}|Y={Format.float b.ZaxisY}|Z={Format.float b.ZaxisZ}"
        $"Euclid.Box %s{sizeX} x %s{sizeY} x %s{sizeZ} (Origin:%s{origin}| X-ax:%s{xAxis}|Y-ax:%s{yAxis}|Z-ax:%s{zAxis})"


    /// Format Box into string with nice floating point number formatting of X, Y and Z size only.
    /// But without type name as in v.ToString()
    member b.AsString : string =
        let sizeX = Format.float b.SizeX
        let sizeY = Format.float b.SizeY
        let sizeZ = Format.float b.SizeZ
        $"%s{sizeX} x %s{sizeY} x %s{sizeZ}"

    /// Format box into string with nice floating point number formatting of X, Y and Z size only.
    static member inline asString (b:Box) : string =
        b.AsString

    /// Format Box into an F# code string that can be used to recreate the box.
    member b.AsFSharpCode : string =
        $"Box.createUnchecked({b.OriginX}, {b.OriginY}, {b.OriginZ}, {b.XaxisX}, {b.XaxisY}, {b.XaxisZ}, {b.YaxisX}, {b.YaxisY}, {b.YaxisZ}, {b.ZaxisX}, {b.ZaxisY}, {b.ZaxisZ})"

    /// Format box into an F# code string that can be used to recreate it.
    static member inline asFSharpCode (b:Box) : string =
        b.AsFSharpCode

    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit : UnitVec =
        let x = r.XaxisX
        let y = r.XaxisY
        let z = r.XaxisZ
        let sqLen = x*x + y*y + z*z
        if isTooTinySq sqLen then
            failTooSmall "Box.XaxisUnit Xaxis" r
        let f = 1.0 / sqrt sqLen
        UnitVec.createUnchecked(x*f, y*f, z*f)

    /// Creates a unitized version of the local X-axis.
    static member inline xAxisUnit (b:Box) : UnitVec =
        b.XaxisUnit

    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit : UnitVec =
        let x = r.YaxisX
        let y = r.YaxisY
        let z = r.YaxisZ
        let sqLen = x*x + y*y + z*z
        if isTooTinySq sqLen then
            failTooSmall "Box.YaxisUnit Yaxis" r
        let f = 1.0 / sqrt sqLen
        UnitVec.createUnchecked(x*f, y*f, z*f)

    /// Creates a unitized version of the local Y-axis.
    static member inline yAxisUnit (b:Box) : UnitVec =
        b.YaxisUnit

    /// Creates a unitized version of the local Z-Axis.
    member inline r.ZaxisUnit : UnitVec =
        let x = r.ZaxisX
        let y = r.ZaxisY
        let z = r.ZaxisZ
        let sqLen = x*x + y*y + z*z
        if isTooTinySq sqLen then
            failTooSmall "Box.ZaxisUnit Zaxis" r
        let f = 1.0 / sqrt sqLen
        UnitVec.createUnchecked(x*f, y*f, z*f)

    /// Creates a unitized version of the local Z-axis.
    static member inline zAxisUnit (b:Box) : UnitVec =
        b.ZaxisUnit

    /// The corner diagonally opposite of corner from Origin.
    member inline b.FarCorner : Pnt =
        Pnt(b.OriginX + b.XaxisX + b.YaxisX + b.ZaxisX,
            b.OriginY + b.XaxisY + b.YaxisY + b.ZaxisY,
            b.OriginZ + b.XaxisZ + b.YaxisZ + b.ZaxisZ)

    /// The corner diagonally opposite of corner from Origin.
    static member inline farCorner (b:Box) : Pnt =
        b.FarCorner

    /// The diagonal vector of the Box.
    member inline b.Diagonal : Vec =
        Vec(b.XaxisX + b.YaxisX + b.ZaxisX,
            b.XaxisY + b.YaxisY + b.ZaxisY,
            b.XaxisZ + b.YaxisZ + b.ZaxisZ)

    /// The diagonal vector of the box.
    static member inline diagonal (b:Box) : Vec =
        b.Diagonal

    /// The center of the Box.
    member inline b.Center : Pnt =
        Pnt(b.OriginX + (b.XaxisX + b.YaxisX + b.ZaxisX)*0.5,
            b.OriginY + (b.XaxisY + b.YaxisY + b.ZaxisY)*0.5,
            b.OriginZ + (b.XaxisZ + b.YaxisZ + b.ZaxisZ)*0.5)

    /// The center of the box.
    static member inline center (b:Box) : Pnt =
        b.Center

    /// Evaluate a X, Y and Z parameter of the Box.
    ///  0.0, 0.0, 0.0 returns the Origin.
    ///  1.0, 1.0, 1.0 returns the FarCorner.
    member inline b.EvaluateAt (xParameter:float, yParameter:float, zParameter:float) : Pnt =
        Pnt(b.OriginX + b.XaxisX*xParameter + b.YaxisX*yParameter + b.ZaxisX*zParameter,
            b.OriginY + b.XaxisY*xParameter + b.YaxisY*yParameter + b.ZaxisY*zParameter,
            b.OriginZ + b.XaxisZ*xParameter + b.YaxisZ*yParameter + b.ZaxisZ*zParameter)

    /// Evaluate a X, Y and Z parameter of the box.
    static member inline evaluateAt xParameter yParameter zParameter (b:Box) : Pnt =
        b.EvaluateAt(xParameter, yParameter, zParameter)

    /// Calculates the volume of the Box.
    member inline b.Volume : float =
        b.SizeX * b.SizeY * b.SizeZ

    /// Calculates the volume of the box.
    static member inline volume (b:Box) : float =
        b.Volume

    /// Returns the longest edge of the Box.
    member inline b.LongestEdge : float =
        let x = b.SizeXSq
        let y = b.SizeYSq
        let z = b.SizeZSq
        sqrt (max (max x y) z)

    /// Returns the longest edge of the box.
    static member inline longestEdge (b:Box) : float =
        b.LongestEdge

    /// Returns the shortest edge of the Box.
    member inline b.ShortestEdge : float =
        let x = b.SizeXSq
        let y = b.SizeYSq
        let z = b.SizeZSq
        sqrt (min (min x y) z)

    /// Returns the shortest edge of the box.
    static member inline shortestEdge (b:Box) : float =
        b.ShortestEdge

    /// Returns the square length of longest edge of the Box.
    member inline b.LongestEdgeSq : float =
        let x = b.SizeXSq
        let y = b.SizeYSq
        let z = b.SizeZSq
        max (max x y) z

    /// Returns the square length of longest edge of the box.
    static member inline longestEdgeSq (b:Box) : float =
        b.LongestEdgeSq

    /// Returns the square length of shortest edge of the Box.
    member inline b.ShortestEdgeSq : float =
        let x = b.SizeXSq
        let y = b.SizeYSq
        let z = b.SizeZSq
        min (min x y) z

    /// Returns the square length of shortest edge of the box.
    static member inline shortestEdgeSq (b:Box) : float =
        b.ShortestEdgeSq

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as box.IsPoint.
    member inline b.IsZero : bool =
        isTooTinySq b.SizeXSq &&
        isTooTinySq b.SizeYSq &&
        isTooTinySq b.SizeZSq

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as isPoint.
    static member inline isZero (b:Box) : bool =
        b.IsZero

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as box.IsZero.
    member inline b.IsPoint : bool =
        isTooTinySq b.SizeXSq &&
        isTooTinySq b.SizeYSq &&
        isTooTinySq b.SizeZSq

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as isZero.
    static member inline isPoint (b:Box) : bool =
        b.IsPoint

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, 2 or 3.
    member inline b.CountZeroSides : int =
        countTooTinySqOrNaN b.SizeXSq
        +  countTooTinySqOrNaN b.SizeYSq
        +  countTooTinySqOrNaN b.SizeZSq

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    static member inline countZeroSides (b:Box) : int =
        b.CountZeroSides

    /// Tests if two of the X, Y and Z axis is smaller than the zeroLength tolerance.
    member inline b.IsLine : bool =
        b.CountZeroSides = 2

    /// Tests if two of the X, Y and Z axis is smaller than the zeroLength tolerance.
    static member inline isLine (b:Box) : bool =
        b.IsLine

    /// Tests if one of the X, Y and Z axis is smaller than the zeroLength tolerance.
    member inline b.IsFlat : bool =
        b.CountZeroSides = 1

    /// Tests if one of the X, Y and Z axis is smaller than the zeroLength tolerance.
    static member inline isFlat (b:Box) : bool =
        b.IsFlat

    /// Tests if no sides of the X, Y and Z axis is smaller than the zeroLength tolerance.
    /// Same as .HasVolume
    member inline b.IsValid : bool =
        b.CountZeroSides = 0

    /// Tests if no sides of the X, Y and Z axis is smaller than the zeroLength tolerance.
    static member inline isValid (b:Box) : bool =
        b.IsValid

    /// Tests if none of the X, Y and Z axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasVolume : bool =
        b.CountZeroSides = 0

    /// Tests if none of the X, Y and Z axis is smaller than the zeroLength tolerance.
    static member inline hasVolume (b:Box) : bool =
        b.HasVolume

    /// Gets the Plane that this box is based on.
    member inline b.Plane : PPlane =
        let lenX = b.SizeX
        let lenY = b.SizeY
        let lenZ = b.SizeZ
        if isTooTiny lenX || isTooTiny lenY || isTooTiny lenZ then
            failTooSmall "Box.Plane: box Xaxis, Yaxis or Zaxis" b
        let ux = b.XaxisX / lenX
        let uy = b.XaxisY / lenX
        let uz = b.XaxisZ / lenX
        let vx = b.YaxisX / lenY
        let vy = b.YaxisY / lenY
        let vz = b.YaxisZ / lenY
        let nx = b.ZaxisX / lenZ
        let ny = b.ZaxisY / lenZ
        let nz = b.ZaxisZ / lenZ
        PPlane.createUnchecked(b.OriginX, b.OriginY, b.OriginZ, ux, uy, uz, vx, vy, vz, nx, ny, nz)

    /// Gets the plane that this box is based on.
    static member inline plane (b:Box) : PPlane =
        b.Plane

    /// Scales the Box by a given factor.
    /// Scale center is World Origin 0,0
    member inline b.Scale (factor:float) : Box =
        Box.createUnchecked(
            b.OriginX * factor, b.OriginY * factor, b.OriginZ * factor,
            b.XaxisX * factor, b.XaxisY * factor, b.XaxisZ * factor,
            b.YaxisX * factor, b.YaxisY * factor, b.YaxisZ * factor,
            b.ZaxisX * factor, b.ZaxisY * factor, b.ZaxisZ * factor)

    /// Scales the 3D box by a given factor.
    /// Scale center is World Origin 0,0,0
    static member inline scale (factor:float) (b:Box) : Box =
        b.Scale factor

    /// Scales the Box by a given factor on a given center point
    member inline l.ScaleOn (cen:Pnt) (factor:float) : Box =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        Box.createUnchecked(
            cx + (l.OriginX - cx) * factor,
            cy + (l.OriginY - cy) * factor,
            cz + (l.OriginZ - cz) * factor,
            l.XaxisX * factor, l.XaxisY * factor, l.XaxisZ * factor,
            l.YaxisX * factor, l.YaxisY * factor, l.YaxisZ * factor,
            l.ZaxisX * factor, l.ZaxisY * factor, l.ZaxisZ * factor)

    /// Scales the box by a given factor around a given center point.
    static member inline scaleOn (cen:Pnt) (factor:float) (b:Box) : Box =
        b.ScaleOn cen factor

    /// Returns a 3D box moved by a vector. Same as Box.translate.
    member inline b.Move (v:Vec) : Box =
        Box.createUnchecked(b.OriginX + v.X, b.OriginY + v.Y, b.OriginZ + v.Z, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Creates a 3D box moved by a vector. Same as Box.translate.
    static member inline move (v:Vec) (b:Box)  : Box =
        Box.createUnchecked(b.OriginX + v.X, b.OriginY + v.Y, b.OriginZ + v.Z, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns a 3D box moved by a given distance in X direction.
    member inline b.MoveX (distance:float) : Box =
        Box.createUnchecked(b.OriginX + distance, b.OriginY, b.OriginZ, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns the 3D box moved by a given distance in X direction.
    static member inline moveX (distance:float) (b:Box) : Box =
        Box.createUnchecked(b.OriginX + distance, b.OriginY, b.OriginZ, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns a 3D box moved by a given distance in Y direction.
    member inline b.MoveY (distance:float) : Box =
        Box.createUnchecked(b.OriginX, b.OriginY + distance, b.OriginZ, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns the 3D box moved by a given distance in Y direction.
    static member inline moveY (distance:float) (b:Box) : Box =
        Box.createUnchecked(b.OriginX, b.OriginY + distance, b.OriginZ, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns a 3D box moved by a given distance in Z direction.
    member inline b.MoveZ (distance:float) : Box =
        Box.createUnchecked(b.OriginX, b.OriginY, b.OriginZ + distance, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns the 3D box moved by a given distance in Z direction.
    static member inline moveZ (distance:float) (b:Box) : Box =
        Box.createUnchecked(b.OriginX, b.OriginY, b.OriginZ + distance, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Multiplies (or applies) a RigidMatrix to a 3D box.
    /// The returned Box is guaranteed to have still orthogonal vectors.
    member inline b.TransformRigid (m:RigidMatrix) : Box =
        let mutable x = b.OriginX
        let mutable y = b.OriginY
        let mutable z = b.OriginZ
        let ox = m.M11*x + m.M21*y + m.M31*z + m.X41
        let oy = m.M12*x + m.M22*y + m.M32*z + m.Y42
        let oz = m.M13*x + m.M23*y + m.M33*z + m.Z43
        x <- b.XaxisX
        y <- b.XaxisY
        z <- b.XaxisZ
        let xx = m.M11*x + m.M21*y + m.M31*z
        let xy = m.M12*x + m.M22*y + m.M32*z
        let xz = m.M13*x + m.M23*y + m.M33*z
        x <- b.YaxisX
        y <- b.YaxisY
        z <- b.YaxisZ
        let yx = m.M11*x + m.M21*y + m.M31*z
        let yy = m.M12*x + m.M22*y + m.M32*z
        let yz = m.M13*x + m.M23*y + m.M33*z
        x <- b.ZaxisX
        y <- b.ZaxisY
        z <- b.ZaxisZ
        let zx = m.M11*x + m.M21*y + m.M31*z
        let zy = m.M12*x + m.M22*y + m.M32*z
        let zz = m.M13*x + m.M23*y + m.M33*z
        Box.createUnchecked(ox, oy, oz, xx, xy, xz, yx, yy, yz, zx, zy, zz)

    /// Transforms the Box by the given RigidMatrix.
    /// The returned Box is guaranteed to have still orthogonal vectors.
    static member inline transformRigid (m:RigidMatrix) (b:Box) : Box =
        b.TransformRigid m

    /// Multiplies (or applies) a Quaternion to a 3D box.
    /// The box is rotated around the World Origin.
    member inline b.Rotate (q:Quaternion) : Box =
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let mutable x = b.OriginX
        let mutable y = b.OriginY
        let mutable z = b.OriginZ
        let mutable tx = 2.0 * ( qy * z - qz * y)
        let mutable ty = 2.0 * ( qz * x - qx * z)
        let mutable tz = 2.0 * ( qx * y - qy * x)
        let ox = x + qw * tx + qy * tz - qz * ty
        let oy = y + qw * ty + qz * tx - qx * tz
        let oz = z + qw * tz + qx * ty - qy * tx
        x <- b.XaxisX
        y <- b.XaxisY
        z <- b.XaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let xx = x + qw * tx + qy * tz - qz * ty
        let xy = y + qw * ty + qz * tx - qx * tz
        let xz = z + qw * tz + qx * ty - qy * tx
        x <- b.YaxisX
        y <- b.YaxisY
        z <- b.YaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let yx = x + qw * tx + qy * tz - qz * ty
        let yy = y + qw * ty + qz * tx - qx * tz
        let yz = z + qw * tz + qx * ty - qy * tx
        x <- b.ZaxisX
        y <- b.ZaxisY
        z <- b.ZaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let zx = x + qw * tx + qy * tz - qz * ty
        let zy = y + qw * ty + qz * tx - qx * tz
        let zz = z + qw * tz + qx * ty - qy * tx
        Box.createUnchecked(ox, oy, oz, xx, xy, xz, yx, yy, yz, zx, zy, zz)

    /// Multiplies (or applies) a Quaternion to a 3D box.
    /// The box IsRotated around the World Origin.
    static member inline rotate (q:Quaternion) (b:Box) : Box =
        b.Rotate q

    /// Multiplies (or applies) a Quaternion to a 3D box around a given center point.
    member inline b.RotateWithCenter (cen:Pnt, q:Quaternion) : Box =
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let mutable x = b.OriginX - cen.X
        let mutable y = b.OriginY - cen.Y
        let mutable z = b.OriginZ - cen.Z
        let mutable tx = 2.0 * ( qy * z - qz * y)
        let mutable ty = 2.0 * ( qz * x - qx * z)
        let mutable tz = 2.0 * ( qx * y - qy * x)
        let ox = x + qw * tx + qy * tz - qz * ty + cen.X
        let oy = y + qw * ty + qz * tx - qx * tz + cen.Y
        let oz = z + qw * tz + qx * ty - qy * tx + cen.Z
        x <- b.XaxisX
        y <- b.XaxisY
        z <- b.XaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let xx = x + qw * tx + qy * tz - qz * ty
        let xy = y + qw * ty + qz * tx - qx * tz
        let xz = z + qw * tz + qx * ty - qy * tx
        x <- b.YaxisX
        y <- b.YaxisY
        z <- b.YaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let yx = x + qw * tx + qy * tz - qz * ty
        let yy = y + qw * ty + qz * tx - qx * tz
        let yz = z + qw * tz + qx * ty - qy * tx
        x <- b.ZaxisX
        y <- b.ZaxisY
        z <- b.ZaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let zx = x + qw * tx + qy * tz - qz * ty
        let zy = y + qw * ty + qz * tx - qx * tz
        let zz = z + qw * tz + qx * ty - qy * tx
        Box.createUnchecked(ox, oy, oz, xx, xy, xz, yx, yy, yz, zx, zy, zz)

    /// Multiplies (or applies) a Quaternion to a 3D box around a given center point.
    static member inline rotateWithCenter (cen:Pnt) (q:Quaternion) (b:Box) : Box =
        b.RotateWithCenter(cen, q)

    /// Check for point containment in the Box.
    /// By doing 6 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    member b.ContainsXYZ(x:float, y:float, z:float) : bool =
        let vx = x - b.OriginX
        let vy = y - b.OriginY
        let vz = z - b.OriginZ
        let dotX = vx*b.XaxisX + vy*b.XaxisY + vz*b.XaxisZ
        let dotY = vx*b.YaxisX + vy*b.YaxisY + vz*b.YaxisZ
        let dotZ = vx*b.ZaxisX + vy*b.ZaxisY + vz*b.ZaxisZ
        let xLenSq = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ
        let yLenSq = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ
        let zLenSq = b.ZaxisX*b.ZaxisX + b.ZaxisY*b.ZaxisY + b.ZaxisZ*b.ZaxisZ
        dotX >= 0.
        &&
        dotY >= 0.
        &&
        dotZ >= 0.
        &&
        dotY - yLenSq <= 0.
        &&
        dotX - xLenSq <= 0.
        &&
        dotZ - zLenSq <= 0.

    /// Check for point containment in the Box.
    /// By doing 6 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    member b.Contains(p:Pnt) : bool =
        b.ContainsXYZ(p.X, p.Y, p.Z)

    /// Check for point containment in the Box.
    /// By doing 6 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    static member inline containsXYZ (x:float) (y:float) (z:float) (b:Box)  : bool =
        b.ContainsXYZ (x, y, z)

    /// Check for point containment in the Box.
    /// By doing 6 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    static member inline contains (p:Pnt) (b:Box)  : bool =
        b.Contains p



    /// Returns the Area of the biggest face of the box.
    /// This is the biggest of the three faces X*Y, X*Z and Y*Z.
    member b.AreaOfBiggestFace : float =
        let x = sqrt(b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ)
        let y = sqrt(b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ)
        let z = sqrt(b.ZaxisX*b.ZaxisX + b.ZaxisY*b.ZaxisY + b.ZaxisZ*b.ZaxisZ)
        max (x * y) (max (x * z) (y * z))

    /// Returns the area of the biggest face of the box.
    static member inline areaOfBiggestFace (b:Box) : float =
        b.AreaOfBiggestFace

    /// Returns the Area of the smallest face of the box.
    /// This is the smallest of the three faces X*Y, X*Z and Y*Z.
    member b.AreaOfSmallestFace : float =
        let x = sqrt(b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ)
        let y = sqrt(b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ)
        let z = sqrt(b.ZaxisX*b.ZaxisX + b.ZaxisY*b.ZaxisY + b.ZaxisZ*b.ZaxisZ)
        min (x * y) (min (x * z) (y * z))

    /// Returns the area of the smallest face of the box.
    static member inline areaOfSmallestFace (b:Box) : float =
        b.AreaOfSmallestFace

    // #endregion
    // #region Static members

    /// Checks if two 3D-boxes are equal within tolerance.
    /// Does not recognize congruent boxes with different rotation as equal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:Box) (b:Box)  : bool =
        abs (a.OriginX - b.OriginX) <= tol &&
        abs (a.OriginY - b.OriginY) <= tol &&
        abs (a.OriginZ - b.OriginZ) <= tol &&
        abs (a.XaxisX  - b.XaxisX ) <= tol &&
        abs (a.XaxisY  - b.XaxisY ) <= tol &&
        abs (a.XaxisZ  - b.XaxisZ ) <= tol &&
        abs (a.YaxisX  - b.YaxisX ) <= tol &&
        abs (a.YaxisY  - b.YaxisY ) <= tol &&
        abs (a.YaxisZ  - b.YaxisZ ) <= tol &&
        abs (a.ZaxisX  - b.ZaxisX ) <= tol &&
        abs (a.ZaxisY  - b.ZaxisY ) <= tol &&
        abs (a.ZaxisZ  - b.ZaxisZ ) <= tol

    /// Check if two 3D-boxes are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two 3D-boxes are not exactly equal.
    static member notEquals (tol:float) (a:Box) (b:Box)  : bool =
        abs (a.OriginX - b.OriginX) > tol ||
        abs (a.OriginY - b.OriginY) > tol ||
        abs (a.OriginZ - b.OriginZ) > tol ||
        abs (a.XaxisX  - b.XaxisX ) > tol ||
        abs (a.XaxisY  - b.XaxisY ) > tol ||
        abs (a.XaxisZ  - b.XaxisZ ) > tol ||
        abs (a.YaxisX  - b.YaxisX ) > tol ||
        abs (a.YaxisY  - b.YaxisY ) > tol ||
        abs (a.YaxisZ  - b.YaxisZ ) > tol ||
        abs (a.ZaxisX  - b.ZaxisX ) > tol ||
        abs (a.ZaxisY  - b.ZaxisY ) > tol ||
        abs (a.ZaxisZ  - b.ZaxisZ ) > tol

    /// Returns Box expanded by distance on all six sides.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (b:Box)  : Box =
        let siX = b.SizeX
        let siY = b.SizeY
        let hei = b.SizeZ
        let d = dist * -2.0
        if siX<=d || siY<=d || hei<=d then
            fail $"Box.expand: Box {b.AsString} is too small to expand by negative (=shrink) distance {dist}"
        let fx = dist / siX
        let fy = dist / siY
        let fz = dist / hei
        let xX = b.XaxisX * fx
        let xY = b.XaxisY * fx
        let xZ = b.XaxisZ * fx
        let yX = b.YaxisX * fy
        let yY = b.YaxisY * fy
        let yZ = b.YaxisZ * fy
        let zX = b.ZaxisX * fz
        let zY = b.ZaxisY * fz
        let zZ = b.ZaxisZ * fz
        Box.createUnchecked(
            b.OriginX - xX - yX - zX,
            b.OriginY - xY - yY - zY,
            b.OriginZ - xZ - yZ - zZ,
            b.XaxisX + xX*2., b.XaxisY + xY*2., b.XaxisZ + xZ*2.,
            b.YaxisX + yX*2., b.YaxisY + yY*2., b.YaxisZ + yZ*2.,
            b.ZaxisX + zX*2., b.ZaxisY + zY*2., b.ZaxisZ + zZ*2.)

    /// Returns Box expanded by respective distances on all six sides.
    /// Does check for overflow if distance is negative and fails.
    /// distX, distY and distZ are for X, Y and Z-axis respectively.
    static member expandXYZ distX distY distZ (b:Box) : Box =
        let siX = b.SizeX
        let siY = b.SizeY
        let hei = b.SizeZ
        if siX <= distX * -2.0 then fail $"Box.expandXYZ: Box {b.AsString} is too small to expand by negative (=shrink) distance distX {distX}"
        if siY <= distY * -2.0 then fail $"Box.expandXYZ: Box {b.AsString} is too small to expand by negative (=shrink) distance distY {distY}"
        if hei <= distZ * -2.0 then fail $"Box.expandXYZ: Box {b.AsString} is too small to expand by negative (=shrink) distance distZ {distZ}"
        let fx = distX / siX
        let fy = distY / siY
        let fz = distZ / hei
        let xX = b.XaxisX * fx
        let xY = b.XaxisY * fx
        let xZ = b.XaxisZ * fx
        let yX = b.YaxisX * fy
        let yY = b.YaxisY * fy
        let yZ = b.YaxisZ * fy
        let zX = b.ZaxisX * fz
        let zY = b.ZaxisY * fz
        let zZ = b.ZaxisZ * fz
        Box.createUnchecked(
            b.OriginX - xX - yX - zX,
            b.OriginY - xY - yY - zY,
            b.OriginZ - xZ - yZ - zZ,
            b.XaxisX + xX*2., b.XaxisY + xY*2., b.XaxisZ + xZ*2.,
            b.YaxisX + yX*2., b.YaxisY + yY*2., b.YaxisZ + yZ*2.,
            b.ZaxisX + zX*2., b.ZaxisY + zY*2., b.ZaxisZ + zZ*2.)

    /// Returns the 3D box expanded by a relative factor on all six sides.
    /// Values between 0.0 and 1.0 shrink the box.
    /// Values larger than 1.0 expand the box.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRel factor (b:Box)  : Box =
        if factor < 0.0 then
            fail $"Box.expandRel: a negative factor {factor} is not allowed for expanding the 3D box {b.AsString}"
        let xX = b.XaxisX * factor
        let xY = b.XaxisY * factor
        let xZ = b.XaxisZ * factor
        let yX = b.YaxisX * factor
        let yY = b.YaxisY * factor
        let yZ = b.YaxisZ * factor
        let zX = b.ZaxisX * factor
        let zY = b.ZaxisY * factor
        let zZ = b.ZaxisZ * factor
        let cx = b.OriginX + (b.XaxisX + b.YaxisX + b.ZaxisX)*0.5
        let cy = b.OriginY + (b.XaxisY + b.YaxisY + b.ZaxisY)*0.5
        let cz = b.OriginZ + (b.XaxisZ + b.YaxisZ + b.ZaxisZ)*0.5
        Box.createUnchecked(
            cx - xX*0.5 - yX*0.5 - zX*0.5,
            cy - xY*0.5 - yY*0.5 - zY*0.5,
            cz - xZ*0.5 - yZ*0.5 - zZ*0.5,
            xX, xY, xZ, yX, yY, yZ, zX, zY, zZ)

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
        let xX = b.XaxisX * factorX
        let xY = b.XaxisY * factorX
        let xZ = b.XaxisZ * factorX
        let yX = b.YaxisX * factorY
        let yY = b.YaxisY * factorY
        let yZ = b.YaxisZ * factorY
        let zX = b.ZaxisX * factorZ
        let zY = b.ZaxisY * factorZ
        let zZ = b.ZaxisZ * factorZ
        let cx = b.OriginX + (b.XaxisX + b.YaxisX + b.ZaxisX)*0.5
        let cy = b.OriginY + (b.XaxisY + b.YaxisY + b.ZaxisY)*0.5
        let cz = b.OriginZ + (b.XaxisZ + b.YaxisZ + b.ZaxisZ)*0.5
        Box.createUnchecked(
            cx - xX*0.5 - yX*0.5 - zX*0.5,
            cy - xY*0.5 - yY*0.5 - zY*0.5,
            cz - xZ*0.5 - yZ*0.5 - zZ*0.5,
            xX, xY, xZ, yX, yY, yZ, zX, zY, zZ)

    /// Creates a 3D box from PPlane and x, y and Z size.
    static member inline createFromPlane x y z (pl:PPlane) : Box =
        Box.createUncheckedVec(pl.Origin, pl.Xaxis*x, pl.Yaxis*y, pl.Zaxis*z)

    /// Creates a 3D box from a 3D a bounds (minX, minY, minZ) and (maxX, maxY, maxZ).
    static member inline createFromBounds(minX:float, minY:float, minZ:float, maxX:float, maxY:float, maxZ:float) : Box =
        Box.createUnchecked(minX, minY, minZ, maxX - minX, 0.0, 0.0, 0.0, maxY - minY, 0.0, 0.0, 0.0, maxZ - minZ)

    /// Creates a 3D box from a 2D rectangle and Z lower and upper position.
    static member createFromRect2D (zLow:float) (zHigh:float) (r:Rect2D) : Box =
        if zHigh < zLow then
            fail $"Box.createFromRect2D: zHigh {zHigh} cannot be smaller than zLow {zLow}"
        Box.createUnchecked(
            r.OriginX, r.OriginY, zLow,
            r.XaxisX, r.XaxisY, 0.0,
            r.YaxisX, r.YaxisY, 0.0,
            0.0, 0.0, zHigh - zLow)

     /// Creates a 3D box from a 3D rectangle and a Z lower and upper offset.
     /// Both zLow and zHigh offsets are applied along the normal of the rectangle in the same direction from the Origin.
    static member inline createFromRect3D (zLow:float) (zHigh:float) (r:Rect3D) : Box =
        if zHigh < zLow then
            fail $"Box.createFromRect3D: zHigh {zHigh} cannot be smaller than zLow {zLow}"
        // get z axis from cross product of x and y axis of the rectangle
        let nx = r.XaxisY * r.YaxisZ - r.XaxisZ * r.YaxisY
        let ny = r.XaxisZ * r.YaxisX - r.XaxisX * r.YaxisZ
        let nz = r.XaxisX * r.YaxisY - r.XaxisY * r.YaxisX
        let len = XYZ.length nx ny nz
        // get the scacling factor for the zLow
        let fLow = if isTooTiny len then 0.0 else zLow / len
        let ox = r.OriginX + nx * fLow
        let oy = r.OriginY + ny * fLow
        let oz = r.OriginZ + nz * fLow
        // get the scacling factor for the zHigh
        let fHigh = if isTooTiny len then 0.0 else (zHigh - zLow) / len
        let zx = nx * fHigh
        let zy = ny * fHigh
        let zz = nz * fHigh
        Box.createUnchecked(
            ox, oy, oz,
            r.XaxisX, r.XaxisY, r.XaxisZ,
            r.YaxisX, r.YaxisY, r.YaxisZ,
            zx, zy, zz)

    /// Finds the oriented bounding box in 3D of a set of points.
    /// The orientation of the X-axis is defined by the dirX vector.
    /// The orientation of the Y-axis is defined by the dirY vector.
    static member createFromPlaneAndPoints (pl:PPlane) (pts:IList<Pnt>) : Box =
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
        let bo = pl.EvaluateAtXYZ(minX, minY, minZ)
        let sizeX = maxX - minX
        let sizeY = maxY - minY
        let sizeZ = maxZ - minZ
        Box.createUncheckedVec(bo, x*sizeX, y*sizeY, z*sizeZ)

    /// Finds the oriented bounding box in 3D of a set of points.
    /// The orientation of the X-axis is defined by the dirX vector.
    /// The orientation of the Y-axis is defined by the dirY vector.
    static member createFromDirsAndPoints (dirX:Vec)  (dirY:Vec)  (pts:IList<Pnt>) : Box =
        if isTooSmallSq dirX.LengthSq then failTooSmall "Euclid.Box.createFromDirsAndPoints: dirX too short" dirX
        if isTooSmallSq dirY.LengthSq then failTooSmall "Euclid.Box.createFromDirsAndPoints: dirY too short" dirY
        if pts.Count < 2              then fail $"Box.createFromDirsAndPoints: cannot create a Box from just {pts.Count} points"
        let pl = PPlane.createOriginXaxisYaxis(pts[0],dirX,dirY)
        Box.createFromPlaneAndPoints pl pts

    /// Creates a 3D box translated along the local X-axis of the Box.
    static member translateLocalX (distX:float) (b:Box) : Box =
        let len = XYZ.length b.XaxisX b.XaxisY b.XaxisZ
        if isTooTiny len then failTooSmall "Box.translateLocalX Xaxis " b
        let f = distX / len
        Box.createUnchecked(b.OriginX + b.XaxisX*f, b.OriginY + b.XaxisY*f, b.OriginZ + b.XaxisZ*f, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Creates a 3D box translated along the local Y-axis of the Box.
    static member translateLocalY (distY:float) (b:Box) : Box =
        let len = XYZ.length b.YaxisX b.YaxisY b.YaxisZ
        if isTooTiny len then failTooSmall "Box.translateLocalY Yaxis" b
        let f = distY / len
        Box.createUnchecked(b.OriginX + b.YaxisX*f, b.OriginY + b.YaxisY*f, b.OriginZ + b.YaxisZ*f, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Creates a 3D box translated along the local Z-axis of the Box.
    static member translateLocalZ (distZ:float) (b:Box) : Box =
        let len = XYZ.length b.ZaxisX b.ZaxisY b.ZaxisZ
        if isTooTiny len then failTooSmall "Box.translateLocalZ Zaxis" b
        let f = distZ / len
        Box.createUnchecked(b.OriginX + b.ZaxisX*f, b.OriginY + b.ZaxisY*f, b.OriginZ + b.ZaxisZ*f, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Move a 3D box by a vector. Same as Box.translate.
    static member inline translate (v:Vec) (b:Box) : Box =
        Box.createUnchecked(b.OriginX + v.X, b.OriginY + v.Y, b.OriginZ + v.Z, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// <summary>Intersects an infinite ray (Line3D extended infinitely in both directions) with this Box.
    /// Uses the slab intersection method in the box's local coordinate system.
    /// Returns None if the ray does not intersect the box or if the ray direction is too short.
    /// Returns Some with entry and exit parameters on the ray if it intersects.
    /// A parameter of 0.0 corresponds to the ray's From point, 1.0 to its To point.</summary>
    /// <param name="ray">The ray (Line3D) to intersect with the box.</param>
    /// <returns>None if no intersection, Some(tEntry, tExit) with the entry and exit parameters on the ray.</returns>
    member b.IntersectRay(ray:Line3D) : voption<float*float> =
        // Transform ray to box's local coordinate system
        let rayDirX = ray.VectorX
        let rayDirY = ray.VectorY
        let rayDirZ = ray.VectorZ
        let rayDirLenSq = rayDirX*rayDirX + rayDirY*rayDirY + rayDirZ*rayDirZ
        if isTooSmallSq rayDirLenSq then
            ValueNone // Ray direction too short
        else
            let rayOriginX = ray.FromX - b.OriginX
            let rayOriginY = ray.FromY - b.OriginY
            let rayOriginZ = ray.FromZ - b.OriginZ

            // Get box axes squared lengths
            let xLenSq = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ
            let yLenSq = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ
            let zLenSq = b.ZaxisX*b.ZaxisX + b.ZaxisY*b.ZaxisY + b.ZaxisZ*b.ZaxisZ

            // Initialize tMin and tMax
            let mutable tMin = Double.MinValue
            let mutable tMax = Double.MaxValue

            // Process X-axis slab
            if isTooTinySq xLenSq then
                // Box is flat in X direction, check if ray origin is within slab
                let originDotX = rayOriginX*b.XaxisX + rayOriginY*b.XaxisY + rayOriginZ*b.XaxisZ
                if originDotX < 0.0 || originDotX > xLenSq then
                    tMin <- Double.MaxValue // Force no intersection
            else
                let dirDotX = rayDirX*b.XaxisX + rayDirY*b.XaxisY + rayDirZ*b.XaxisZ
                let originDotX = rayOriginX*b.XaxisX + rayOriginY*b.XaxisY + rayOriginZ*b.XaxisZ
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
                    let originDotY = rayOriginX*b.YaxisX + rayOriginY*b.YaxisY + rayOriginZ*b.YaxisZ
                    if originDotY < 0.0 || originDotY > yLenSq then
                        tMin <- Double.MaxValue
                else
                    let dirDotY = rayDirX*b.YaxisX + rayDirY*b.YaxisY + rayDirZ*b.YaxisZ
                    let originDotY = rayOriginX*b.YaxisX + rayOriginY*b.YaxisY + rayOriginZ*b.YaxisZ
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
                    let originDotZ = rayOriginX*b.ZaxisX + rayOriginY*b.ZaxisY + rayOriginZ*b.ZaxisZ
                    if originDotZ < 0.0 || originDotZ > zLenSq then
                        tMin <- Double.MaxValue
                else
                    let dirDotZ = rayDirX*b.ZaxisX + rayDirY*b.ZaxisY + rayDirZ*b.ZaxisZ
                    let originDotZ = rayOriginX*b.ZaxisX + rayOriginY*b.ZaxisY + rayOriginZ*b.ZaxisZ
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
                ValueSome(tMin, tMax)
            else
                ValueNone

    /// <summary>Intersects an infinite ray (Line3D extended infinitely in both directions) with the Box.
    /// Uses the slab intersection method in the box's local coordinate system.
    /// Returns None if the ray does not intersect the box or if the ray direction is too short.
    /// Returns Some with entry and exit parameters on the ray if it intersects.
    /// A parameter of 0.0 corresponds to the ray's From point, 1.0 to its To point.</summary>
    /// <param name="ray">The ray (Line3D) to intersect with the box.</param>
    /// <param name="box">The box to intersect with.</param>
    /// <returns>None if no intersection, Some(tEntry, tExit) with the entry and exit parameters on the ray.</returns>
    static member inline intersectRay (ray:Line3D) (box:Box) : voption<float*float> =
        box.IntersectRay(ray)


    // #endregion
    // #region Points

    /// <summary>Returns the bottom corners of the Box in Counter-Clockwise order, starting at Origin.
    /// Then the top corners staring above Origin. Returns an array of 8 Points.
    /// <code>
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
    /// </code>
    /// </summary>
    member b.Points :Pnt[] =
        let p0x = b.OriginX
        let p0y = b.OriginY
        let p0z = b.OriginZ
        let p1x = p0x + b.XaxisX
        let p1y = p0y + b.XaxisY
        let p1z = p0z + b.XaxisZ
        let p4x = p0x + b.ZaxisX
        let p4y = p0y + b.ZaxisY
        let p4z = p0z + b.ZaxisZ
        let p5x = p4x + b.XaxisX
        let p5y = p4y + b.XaxisY
        let p5z = p4z + b.XaxisZ
        [|
            Pnt(p0x, p0y, p0z)
            Pnt(p1x, p1y, p1z)
            Pnt(p1x + b.YaxisX, p1y + b.YaxisY, p1z + b.YaxisZ)
            Pnt(p0x + b.YaxisX, p0y + b.YaxisY, p0z + b.YaxisZ)
            Pnt(p4x, p4y, p4z)
            Pnt(p5x, p5y, p5z)
            Pnt(p5x + b.YaxisX, p5y + b.YaxisY, p5z + b.YaxisZ)
            Pnt(p4x + b.YaxisX, p4y + b.YaxisY, p4z + b.YaxisZ)
        |]

    /// Returns the corners of the box in the documented order.
    static member inline points (b:Box) : Pnt[] =
        b.Points

    /// <summary>Returns point 0 of the box, same box.Origin.
    /// <code>
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
    /// </code>
    /// </summary>
    member inline b.Pt0 : Pnt =
        Pnt(b.OriginX, b.OriginY, b.OriginZ)

    /// Returns point 0 of the box, same as Origin.
    static member inline pt0 (b:Box) : Pnt =
        b.Pt0

    /// <summary>Returns point 1 of the box.
    /// <code>
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
    /// </code>
    /// </summary>
    member inline b.Pt1 : Pnt =
        Pnt(b.OriginX + b.XaxisX, b.OriginY + b.XaxisY, b.OriginZ + b.XaxisZ)

    /// Returns point 1 of the box.
    static member inline pt1 (b:Box) : Pnt =
        b.Pt1

    /// <summary>Returns point 2 of the box.
    /// <code>
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
    /// </code>
    /// </summary>
    member inline b.Pt2 : Pnt =
        Pnt(b.OriginX + b.XaxisX + b.YaxisX, b.OriginY + b.XaxisY + b.YaxisY, b.OriginZ + b.XaxisZ + b.YaxisZ)

    /// Returns point 2 of the box.
    static member inline pt2 (b:Box) : Pnt =
        b.Pt2

    /// <summary>Returns point 3 of the box.
    /// <code>
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
    /// </code>
    /// </summary>
    member inline b.Pt3 : Pnt =
        Pnt(b.OriginX + b.YaxisX, b.OriginY + b.YaxisY, b.OriginZ + b.YaxisZ)

    /// Returns point 3 of the box.
    static member inline pt3 (b:Box) : Pnt =
        b.Pt3

    /// <summary>Returns point 4 of the box.
    /// <code>
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
    /// </code>
    /// </summary>
    member inline b.Pt4 : Pnt =
        Pnt(b.OriginX + b.ZaxisX, b.OriginY + b.ZaxisY, b.OriginZ + b.ZaxisZ)

    /// Returns point 4 of the box.
    static member inline pt4 (b:Box) : Pnt =
        b.Pt4

    /// <summary>Returns point 5 of the box.
    /// <code>
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
    /// </code>
    /// </summary>
    member inline b.Pt5 : Pnt =
        Pnt(b.OriginX + b.XaxisX + b.ZaxisX, b.OriginY + b.XaxisY + b.ZaxisY, b.OriginZ + b.XaxisZ + b.ZaxisZ)

    /// Returns point 5 of the box.
    static member inline pt5 (b:Box) : Pnt =
        b.Pt5

    /// <summary>Returns point 6 of the box.
    /// <code>
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
    /// </code>
    /// </summary>
    member inline b.Pt6 : Pnt =
        Pnt(b.OriginX + b.XaxisX + b.YaxisX + b.ZaxisX, b.OriginY + b.XaxisY + b.YaxisY + b.ZaxisY, b.OriginZ + b.XaxisZ + b.YaxisZ + b.ZaxisZ)

    /// Returns point 6 of the box.
    static member inline pt6 (b:Box) : Pnt =
        b.Pt6

    /// <summary>Returns point 7 of the box.
    /// <code>
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
    /// </code>
    /// </summary>
    member inline b.Pt7 : Pnt =
        Pnt(b.OriginX + b.YaxisX + b.ZaxisX, b.OriginY + b.YaxisY + b.ZaxisY, b.OriginZ + b.YaxisZ + b.ZaxisZ)

    /// Returns point 7 of the box.
    static member inline pt7 (b:Box) : Pnt =
        b.Pt7

    /// <summary>Returns the point of the Box at the specified index.
    /// The order of the points is: Pt0, Pt1, Pt2, Pt3, Pt4, Pt5, Pt6, Pt7.
    /// <code>
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
    /// </code>
    /// </summary>
    member b.GetPoint (pointIndex:int) : Pnt =
        match pointIndex with
        | 0 -> b.Pt0
        | 1 -> b.Pt1
        | 2 -> b.Pt2
        | 3 -> b.Pt3
        | 4 -> b.Pt4
        | 5 -> b.Pt5
        | 6 -> b.Pt6
        | 7 -> b.Pt7
        | _ -> fail $"Box.GetPoint: pointIndex {pointIndex} is out of range. Valid range is 0 to 7."

    /// Returns the point of the Box at the specified index.
    static member inline getPoint (pointIndex:int) (b:Box) : Pnt =
        b.GetPoint pointIndex


    // #endregion
    // #region Faces



    /// <summary>Returns the top face of the Box in Counter-Clockwise order, looking from above.
    /// Returns Origin at point 4, X-Axis to point 5, Y-Axis to point 7.
    /// The normal of the Rect3D points away from the Box.
    /// <code>
    ///            local      F3(back)
    ///            Z-Axis     F7(top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- F2(right)
    ///            |   |           |   |
    ///  (left)F4--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/     local
    ///            +---------------+----> X-Axis
    ///            0       |       1
    ///                    |
    ///                    F0(bottom)
    ///                    F1(front)
    /// </code>
    /// </summary>
    member b.TopFace :Rect3D =
        Rect3D.createUnchecked(b.OriginX + b.ZaxisX, b.OriginY + b.ZaxisY, b.OriginZ + b.ZaxisZ, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ)

    /// Returns the top face of the box.
    static member inline topFace (b:Box) : Rect3D =
        b.TopFace

    /// <summary>Returns the bottom face of the Box in Counter-Clockwise order, looking from above.
    /// Returns Origin at point 0, X-Axis to point 1, Y-Axis to point 3.
    /// The normal of the Rect3D points into the Box.
    /// <code>
    ///            local      F3(back)
    ///            Z-Axis     F7(top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- F2(right)
    ///            |   |           |   |
    ///  (left)F4--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/     local
    ///            +---------------+----> X-Axis
    ///            0       |       1
    ///                    |
    ///                    F0(bottom)
    ///                    F1(front)
    /// </code>
    /// </summary>
    member b.BottomFace : Rect3D =
        Rect3D.createUnchecked(b.OriginX, b.OriginY, b.OriginZ, b.XaxisX, b.XaxisY, b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ)

    /// Returns the bottom face of the box.
    static member inline bottomFace (b:Box) : Rect3D =
        b.BottomFace

    /// <summary>Returns the front face of the Box in Counter-Clockwise order, looking from front.
    /// Returns Origin at point 0, X-Axis to point 1, Y-Axis to point 4.
    /// The normal of the Rect3D points away from the Box.
    /// <code>
    ///            local      F3(back)
    ///            Z-Axis     F7(top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- F2(right)
    ///            |   |           |   |
    ///  (left)F4--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/     local
    ///            +---------------+----> X-Axis
    ///            0       |       1
    ///                    |
    ///                    F0(bottom)
    ///                    F1(front)
    /// </code>
    /// </summary>
    member b.FrontFace : Rect3D =
        Rect3D.createUnchecked(b.OriginX, b.OriginY, b.OriginZ, b.XaxisX, b.XaxisY, b.XaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns the front face of the box.
    static member inline frontFace (b:Box) : Rect3D =
        b.FrontFace

    /// <summary>Returns the back face of the Box in Counter-Clockwise order, looking from front.
    /// Returns Origin at point 3, X-Axis to point 2, Y-Axis to point 7.
    /// The normal of the Rect3D points into the Box.
    /// <code>
    ///            local      F3(back)
    ///            Z-Axis     F7(top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- F2(right)
    ///            |   |           |   |
    ///  (left)F4--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/     local
    ///            +---------------+----> X-Axis
    ///            0       |       1
    ///                    |
    ///                    F0(bottom)
    ///                    F1(front)
    /// </code>
    /// </summary>
    member b.BackFace : Rect3D =
        Rect3D.createUnchecked(b.OriginX + b.YaxisX, b.OriginY + b.YaxisY, b.OriginZ + b.YaxisZ, b.XaxisX, b.XaxisY, b.XaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns the back face of the box.
    static member inline backFace (b:Box) : Rect3D =
        b.BackFace

    /// <summary>Returns the right face of the Box in Counter-Clockwise order, looking from right.
    /// Returns Origin at point 1, X-Axis to point 2, Y-Axis to point 5.
    /// The normal of the Rect3D points away from the Box.
    /// <code>
    ///            local      F3(back)
    ///            Z-Axis     F7(top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- F2(right)
    ///            |   |           |   |
    ///  (left)F4--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/     local
    ///            +---------------+----> X-Axis
    ///            0       |       1
    ///                    |
    ///                    F0(bottom)
    ///                    F1(front)
    /// </code>
    /// </summary>
    member b.RightFace : Rect3D =
        Rect3D.createUnchecked(b.OriginX + b.XaxisX, b.OriginY + b.XaxisY, b.OriginZ + b.XaxisZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns the right face of the box.
    static member inline rightFace (b:Box) : Rect3D =
        b.RightFace

    /// <summary>Returns the left face of the Box in Counter-Clockwise order, looking from right.
    /// Returns Origin at point 0, X-Axis to point 3, Y-Axis to point 4.
    /// The normal of the Rect3D points into the Box.
    /// <code>
    ///            local      F3(back)
    ///            Z-Axis     F7(top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- F2(right)
    ///            |   |           |   |
    ///  (left)F4--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/     local
    ///            +---------------+----> X-Axis
    ///            0       |       1
    ///                    |
    ///                    F0(bottom)
    ///                    F1(front)
    /// </code>
    /// </summary>
    member b.LeftFace : Rect3D =
        Rect3D.createUnchecked(b.OriginX, b.OriginY, b.OriginZ, b.YaxisX, b.YaxisY, b.YaxisZ, b.ZaxisX, b.ZaxisY, b.ZaxisZ)

    /// Returns the left face of the box.
    static member inline leftFace (b:Box) : Rect3D =
        b.LeftFace


    /// <summary>Returns 6 face of the Box in
    /// The normal of the Rect3Ds are oriented with the X-Axis, Y-Axis or Z-Axis.
    /// The order of the Rect3D is: BottomFace, FrontFace, RightFace, BackFace, LeftFace, TopFace.
    /// <code>
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
    /// </code>
    /// </summary>
    member b.Faces : Rect3D[] =
        [|
        b.BottomFace
        b.FrontFace
        b.RightFace
        b.BackFace
        b.LeftFace
        b.TopFace
        |]

    /// Returns the six faces of the box in documented order.
    static member inline faces (b:Box) : Rect3D[] =
        b.Faces

    /// <summary>Returns the face of the Box at the specified index.
    /// The order of the faces is: BottomFace, FrontFace, RightFace, BackFace, LeftFace, TopFace.
    /// <code>
    ///            local      F3(back)
    ///            Z-Axis     F7(top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- F2(right)
    ///            |   |           |   |
    ///  (left)F4--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/     local
    ///            +---------------+----> X-Axis
    ///            0       |       1
    ///                    |
    ///                    F0(bottom)
    ///                    F1(front)
    /// </code>
    /// </summary>
    /// <param name="faceIndex">The index of the face to retrieve. Valid range is 0 to 5.</param>
    /// <returns>The Rect3D representing the specified face of the Box.</returns>
    member b.GetFace (faceIndex:int) : Rect3D =
        match faceIndex with
        | 0 -> b.BottomFace
        | 1 -> b.FrontFace
        | 2 -> b.RightFace
        | 3 -> b.BackFace
        | 4 -> b.LeftFace
        | 5 -> b.TopFace
        | _ -> fail $"Box.GetFace: faceIndex {faceIndex} is out of range. Valid range is 0 to 5."

    /// Returns the face of the Box at the specified index.
    static member inline getFace (faceIndex:int) (b:Box) : Rect3D =
        b.GetFace faceIndex

    // #endregion
    // #region Edges


    /// <summary>Returns the X-aligned edge from point 0 to 1.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge01 : Line3D =
        Line3D(b.OriginX, b.OriginY, b.OriginZ,
               b.OriginX + b.XaxisX, b.OriginY + b.XaxisY, b.OriginZ + b.XaxisZ)

    /// <summary>Returns the X-aligned edge from point 0 to 1.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge01 (b:Box) : Line3D =
        b.Edge01

    /// <summary>Returns the Y-aligned edge from point 1 to 2.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge12 : Line3D =
        Line3D(b.OriginX + b.XaxisX, b.OriginY + b.XaxisY, b.OriginZ + b.XaxisZ,
               b.OriginX + b.XaxisX + b.YaxisX, b.OriginY + b.XaxisY + b.YaxisY, b.OriginZ + b.XaxisZ + b.YaxisZ)

    /// <summary>Returns the Y-aligned edge from point 1 to 2.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge12 (b:Box) : Line3D =
        b.Edge12

    /// <summary>Returns the X-aligned edge from point 3 to 2.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge32 : Line3D =
        Line3D(b.OriginX + b.YaxisX, b.OriginY + b.YaxisY, b.OriginZ + b.YaxisZ,
               b.OriginX + b.XaxisX + b.YaxisX, b.OriginY + b.XaxisY + b.YaxisY, b.OriginZ + b.XaxisZ + b.YaxisZ)

    /// <summary>Returns the X-aligned edge from point 3 to 2.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge32 (b:Box) : Line3D =
        b.Edge32

    /// <summary>Returns the Y-aligned edge from point 0 to 3.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge03 : Line3D =
        Line3D(b.OriginX, b.OriginY, b.OriginZ,
               b.OriginX + b.YaxisX, b.OriginY + b.YaxisY, b.OriginZ + b.YaxisZ)

    /// <summary>Returns the Y-aligned edge from point 0 to 3.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge03 (b:Box) : Line3D =
        b.Edge03

    /// <summary>Returns the X-aligned edge from point 4 to 5.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge45 : Line3D =
        Line3D(b.OriginX + b.ZaxisX, b.OriginY + b.ZaxisY, b.OriginZ + b.ZaxisZ,
               b.OriginX + b.XaxisX + b.ZaxisX, b.OriginY + b.XaxisY + b.ZaxisY, b.OriginZ + b.XaxisZ + b.ZaxisZ)

    /// <summary>Returns the X-aligned edge from point 4 to 5.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge45 (b:Box) : Line3D =
        b.Edge45

    /// <summary>Returns the Y-aligned edge from point 5 to 6.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge56 : Line3D =
        Line3D(b.OriginX + b.XaxisX + b.ZaxisX, b.OriginY + b.XaxisY + b.ZaxisY, b.OriginZ + b.XaxisZ + b.ZaxisZ,
               b.OriginX + b.XaxisX + b.YaxisX + b.ZaxisX, b.OriginY + b.XaxisY + b.YaxisY + b.ZaxisY, b.OriginZ + b.XaxisZ + b.YaxisZ + b.ZaxisZ)

    /// <summary>Returns the Y-aligned edge from point 5 to 6.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge56 (b:Box) : Line3D =
        b.Edge56

    /// <summary>Returns the X-aligned edge from point 7 to 6.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge76 : Line3D =
        Line3D(b.OriginX + b.YaxisX + b.ZaxisX, b.OriginY + b.YaxisY + b.ZaxisY, b.OriginZ + b.YaxisZ + b.ZaxisZ,
               b.OriginX + b.XaxisX + b.YaxisX + b.ZaxisX, b.OriginY + b.XaxisY + b.YaxisY + b.ZaxisY, b.OriginZ + b.XaxisZ + b.YaxisZ + b.ZaxisZ)

    /// <summary>Returns the X-aligned edge from point 7 to 6.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge76 (b:Box) : Line3D =
        b.Edge76

    /// <summary>Returns the Y-aligned edge from point 4 to 7.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge47 : Line3D =
        Line3D(b.OriginX + b.ZaxisX, b.OriginY + b.ZaxisY, b.OriginZ + b.ZaxisZ,
               b.OriginX + b.YaxisX + b.ZaxisX, b.OriginY + b.YaxisY + b.ZaxisY, b.OriginZ + b.YaxisZ + b.ZaxisZ)

    /// <summary>Returns the Y-aligned edge from point 4 to 7.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge47 (b:Box) : Line3D =
        b.Edge47

    /// <summary>Returns the Z-aligned edge from point 0 to 4.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge04 : Line3D =
        Line3D(b.OriginX, b.OriginY, b.OriginZ,
               b.OriginX + b.ZaxisX, b.OriginY + b.ZaxisY, b.OriginZ + b.ZaxisZ)

    /// <summary>Returns the Z-aligned edge from point 0 to 4.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge04 (b:Box) : Line3D =
        b.Edge04

    /// <summary>Returns the Z-aligned edge from point 1 to 5.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge15 : Line3D =
        Line3D(b.OriginX + b.XaxisX, b.OriginY + b.XaxisY, b.OriginZ + b.XaxisZ,
               b.OriginX + b.XaxisX + b.ZaxisX, b.OriginY + b.XaxisY + b.ZaxisY, b.OriginZ + b.XaxisZ + b.ZaxisZ)

    /// <summary>Returns the Z-aligned edge from point 1 to 5.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge15 (b:Box) : Line3D =
        b.Edge15

    /// <summary>Returns the Z-aligned edge from point 2 to 6.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge26 : Line3D =
        Line3D(b.OriginX + b.XaxisX + b.YaxisX, b.OriginY + b.XaxisY + b.YaxisY, b.OriginZ + b.XaxisZ + b.YaxisZ,
               b.OriginX + b.XaxisX + b.YaxisX + b.ZaxisX, b.OriginY + b.XaxisY + b.YaxisY + b.ZaxisY, b.OriginZ + b.XaxisZ + b.YaxisZ + b.ZaxisZ)

    /// <summary>Returns the Z-aligned edge from point 2 to 6.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge26 (b:Box) : Line3D =
        b.Edge26

    /// <summary>Returns the Z-aligned edge from point 3 to 7.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    member inline b.Edge37 : Line3D =
        Line3D(b.OriginX + b.YaxisX, b.OriginY + b.YaxisY, b.OriginZ + b.YaxisZ,
               b.OriginX + b.YaxisX + b.ZaxisX, b.OriginY + b.YaxisY + b.ZaxisY, b.OriginZ + b.YaxisZ + b.ZaxisZ)

    /// <summary>Returns the Z-aligned edge from point 3 to 7.
    /// <code>
    ///   7------6
    ///  /|     /|
    /// 4------5 |
    /// | |    | |
    /// | 3----|-2
    /// |/     |/
    /// 0------1
    /// </code>
    /// </summary>
    static member inline edge37 (b:Box) : Line3D =
        b.Edge37


    /// <summary>Returns the edge of the box at the specified index.
    /// The order of the edges is: 0-1, 1-2, 3-2, 0-3, 0-4, 1-5, 2-6, 3-7, 4-5, 5-6, 7-6, 4-7
    /// <code>
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
    /// </code>
    /// </summary>
    member b.GetEdge (edgeIndex:int) : Line3D =
        match edgeIndex with
        | 0  -> b.Edge01
        | 1  -> b.Edge12
        | 2  -> b.Edge32
        | 3  -> b.Edge03
        | 4  -> b.Edge04
        | 5  -> b.Edge15
        | 6  -> b.Edge26
        | 7  -> b.Edge37
        | 8  -> b.Edge45
        | 9  -> b.Edge56
        | 10 -> b.Edge76
        | 11 -> b.Edge47
        | _ -> fail $"Box.GetEdge: edgeIndex {edgeIndex} is out of range. Valid range is 0 to 11."

    /// Returns the edge of the box at the specified index.
    static member inline getEdge (edgeIndex:int) (b:Box): Line3D =
        b.GetEdge(edgeIndex)


    /// <summary>Returns the 12 edges of this box as an array of 12 Lines.
    /// Pairs in this order:
    /// 0-1, 1-2, 3-2, 0-3, 0-4, 1-5, 2-6, 3-7, 4-5, 5-6, 7-6, 4-7
    /// <code>
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
    /// </code>
    /// </summary>
    member b.Edges : Line3D[] = // this function needs to be defined after the EdgeXX members, because they are inlined.
        [| b.Edge01; b.Edge12; b.Edge32; b.Edge03; // bottom face
           b.Edge04; b.Edge15; b.Edge26; b.Edge37; // vertical edges
           b.Edge45; b.Edge56; b.Edge76; b.Edge47 |] // top face

    /// Returns all 12 edges of the box.
    static member inline edges (b:Box) : Line3D[] =
        b.Edges

    // #endregion
    // #region Obsolete


    [<Obsolete("Use .Edge01 instead.")>]
    member inline b.Edge0 : Line3D = b.Edge01

    [<Obsolete("Use .Edge12 instead.")>]
    member inline b.Edge1 : Line3D = b.Edge12

    [<Obsolete("Use .Edge32 instead.")>]
    member inline b.Edge2 : Line3D = b.Edge32

    [<Obsolete("Use .Edge03 instead.")>]
    member inline b.Edge3 : Line3D = b.Edge03

    [<Obsolete("Use .Edge45 instead.")>]
    member inline b.Edge4 : Line3D = b.Edge45

    [<Obsolete("Use .Edge56 instead.")>]
    member inline b.Edge5 : Line3D = b.Edge56

    [<Obsolete("Use .Edge76 instead.")>]
    member inline b.Edge6 : Line3D = b.Edge76

    [<Obsolete("Use .Edge47 instead.")>]
    member inline b.Edge7 : Line3D = b.Edge47

    [<Obsolete("Use .Edge04 instead.")>]
    member inline b.Edge8 : Line3D = b.Edge04

    [<Obsolete("Use .Edge15 instead.")>]
    member inline b.Edge9 : Line3D = b.Edge15

    [<Obsolete("Use .Edge26 instead.")>]
    member inline b.Edge10 : Line3D = b.Edge26

    [<Obsolete("Use .Edge37 instead.")>]
    member inline b.Edge11 : Line3D = b.Edge37

    [<Obsolete("This is actually the volume, also this does not scale proportionally, use .Volume")>]
    member inline r.AreaSq : float =
        (r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY + r.XaxisZ*r.XaxisZ)
        * (r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY + r.YaxisZ*r.YaxisZ)
        * (r.ZaxisX*r.ZaxisX + r.ZaxisY*r.ZaxisY + r.ZaxisZ*r.ZaxisZ)

    [<Obsolete("use SizeX")>]
    member inline b.Width : float =
        sqrt(b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ)

    [<Obsolete("use SizeZ")>]
    member inline b.Height3D : float =
        sqrt(b.ZaxisX*b.ZaxisX + b.ZaxisY*b.ZaxisY + b.ZaxisZ*b.ZaxisZ)

    [<Obsolete("use SizeY")>]
    member inline b.Depth : float =
        sqrt(b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ)


    #nowarn "44" // Obsolete member

    [<Obsolete("If the matrix is projecting this transformation will not return a valid Box. Use RigidMatrix or with the Points of this box instead.")>]
    member inline b.Transform (m:Matrix) : Box =
        // Origin is transformed as a position (with translation and perspective division).
        let ox' = m.M11*b.OriginX + m.M21*b.OriginY + m.M31*b.OriginZ + m.X41
        let oy' = m.M12*b.OriginX + m.M22*b.OriginY + m.M32*b.OriginZ + m.Y42
        let oz' = m.M13*b.OriginX + m.M23*b.OriginY + m.M33*b.OriginZ + m.Z43
        let ow' = m.M14*b.OriginX + m.M24*b.OriginY + m.M34*b.OriginZ + m.M44
        let sc = 1.0 / ow'
        let ox = ox' * sc
        let oy = oy' * sc
        let oz = oz' * sc
        // axis vectors are transformed as directions (translation ignored).
        let xx = m.M11*b.XaxisX + m.M21*b.XaxisY + m.M31*b.XaxisZ
        let xy = m.M12*b.XaxisX + m.M22*b.XaxisY + m.M32*b.XaxisZ
        let xz = m.M13*b.XaxisX + m.M23*b.XaxisY + m.M33*b.XaxisZ
        let yx = m.M11*b.YaxisX + m.M21*b.YaxisY + m.M31*b.YaxisZ
        let yy = m.M12*b.YaxisX + m.M22*b.YaxisY + m.M32*b.YaxisZ
        let yz = m.M13*b.YaxisX + m.M23*b.YaxisY + m.M33*b.YaxisZ
        let zx = m.M11*b.ZaxisX + m.M21*b.ZaxisY + m.M31*b.ZaxisZ
        let zy = m.M12*b.ZaxisX + m.M22*b.ZaxisY + m.M32*b.ZaxisZ
        let zz = m.M13*b.ZaxisX + m.M23*b.ZaxisY + m.M33*b.ZaxisZ
        Box.createUnchecked(ox, oy, oz, xx, xy, xz, yx, yy, yz, zx, zy, zz)

    [<Obsolete("If the matrix is projecting this transformation will not return a valid Box. Use RigidMatrix or with the Points of this box instead.")>]
    static member inline transform (m:Matrix) (b:Box) : Box =
        b.Transform m


    /// returns minX, minY, minZ, maxX, maxY, maxZ
    [<Obsolete("Use BBox.createFromBox instead.")>]
    member b.BBox : float * float * float * float * float * float =
        // Each of the 8 corners is Origin plus any subset of the three axis
        // components, so min/max decompose per coordinate: sum the negative
        // parts for the min, the positive parts for the max.
        let minX = b.OriginX + min 0. b.XaxisX + min 0. b.YaxisX + min 0. b.ZaxisX
        let minY = b.OriginY + min 0. b.XaxisY + min 0. b.YaxisY + min 0. b.ZaxisY
        let minZ = b.OriginZ + min 0. b.XaxisZ + min 0. b.YaxisZ + min 0. b.ZaxisZ
        let maxX = b.OriginX + max 0. b.XaxisX + max 0. b.YaxisX + max 0. b.ZaxisX
        let maxY = b.OriginY + max 0. b.XaxisY + max 0. b.YaxisY + max 0. b.ZaxisY
        let maxZ = b.OriginZ + max 0. b.XaxisZ + max 0. b.YaxisZ + max 0. b.ZaxisZ
        (minX, minY, minZ, maxX, maxY, maxZ)

    /// minX, minY, minZ, maxX, maxY, maxZ
    [<Obsolete("Use BBox.createFromBox instead.")>]
    static member inline bbox (b:Box) : float * float * float * float * float * float =
        b.BBox

    [<Obsolete("Use bBox.AsBox instead.")>]
    static member inline createFromBoundingBox (minX, minY, minZ, maxX, maxY, maxZ) : Box =
        Box.createUnchecked(minX, minY, minZ, maxX - minX, 0.0, 0.0, 0.0, maxY - minY, 0.0, 0.0, 0.0, maxZ - minZ)
