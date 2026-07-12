namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open UtilEuclid
open EuclidErrors
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open System.Collections.Generic


/// <summary>
/// A struct containing a 3D Origin point and two 3D Edge vectors,
/// representing an immutable planar 3D-rectangle with any rotation in 3D space.
/// Similar to PPlane, however the two vectors are not unitized.
/// This implementation guarantees the 3D-rectangle to be always valid.
/// That means the  X and Y axes are always perpendicular to each other.
/// However the length of one of these axes might still be zero.
/// <code>
///   local
///   Y-Axis
///   ^
///   |
///   |             2
/// 3 +------------+
///   |            |
///   |            |
///   |            |
///   |            |
///   |            |       local
///   +------------+-----> X-Axis
///  0-Origin       1
/// </code>
/// </summary>
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Rect3D =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The X coordinate of the Origin Corner of the 3D-rectangle.
    [<DataMember>] val public OriginX: float

    /// The Y coordinate of the Origin Corner of the 3D-rectangle.
    [<DataMember>] val public OriginY: float

    /// The Z coordinate of the Origin Corner of the 3D-rectangle.
    [<DataMember>] val public OriginZ: float

    /// The X component of the edge vector representing the X-axis of the 3D-rectangle.
    [<DataMember>] val public XaxisX: float

    /// The Y component of the edge vector representing the X-axis of the 3D-rectangle.
    [<DataMember>] val public XaxisY: float

    /// The Z component of the edge vector representing the X-axis of the 3D-rectangle.
    [<DataMember>] val public XaxisZ: float

    /// The X component of the edge vector representing the Y-axis of the 3D-rectangle.
    [<DataMember>] val public YaxisX: float

    /// The Y component of the edge vector representing the Y-axis of the 3D-rectangle.
    [<DataMember>] val public YaxisY: float

    /// The Z component of the edge vector representing the Y-axis of the 3D-rectangle.
    [<DataMember>] val public YaxisZ: float

    /// Unchecked Internal Constructor Only.
    /// Creates a 3D rectangle from origin coordinates and X and Y axis vector components.
    [<Obsolete("Unsafe internal constructor, doesn't check the input (unless compiled in DEBUG mode), but must be public for inlining. So marked Obsolete instead.") >]
    new (originX:float, originY:float, originZ:float, axisXX:float, axisXY:float, axisXZ:float, axisYX:float, axisYY:float, axisYZ:float) =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
            let lenX = sqrt(axisXX*axisXX + axisXY*axisXY + axisXZ*axisXZ)
            let lenY = sqrt(axisYX*axisYX + axisYY*axisYY + axisYZ*axisYZ)
            if isTooSmall (lenX) then  failTooSmall2 "Rect3D() axisX" (Vec(axisXX, axisXY, axisXZ)) (Vec(axisYX, axisYY, axisYZ))
            if isTooSmall (lenY) then  failTooSmall2 "Rect3D() axisY" (Vec(axisYX, axisYY, axisYZ)) (Vec(axisXX, axisXY, axisXZ))
            // scale the dot-product tolerance by the axis lengths so the check is a relative angle tolerance (independent of the rectangle size):
            if abs (axisXX*axisYX + axisXY*axisYY + axisXZ*axisYZ) > lenX*lenY * 1e-9 then fail2 $"Rect3D(): X-axis and Y-axis are not perpendicular" (Vec(axisXX, axisXY, axisXZ)) (Vec(axisYX, axisYY, axisYZ))
        #endif
            {OriginX=originX; OriginY=originY; OriginZ=originZ; XaxisX=axisXX; XaxisY=axisXY; XaxisZ=axisXZ; YaxisX=axisYX; YaxisY=axisYY; YaxisZ=axisYZ}

    /// Create a 3D-rectangle from origin coordinates and X and Y axis vector components.
    /// Does not check for perpendicularity.
    static member inline createUnchecked (originX:float, originY:float, originZ:float, xAxisX:float, xAxisY:float, xAxisZ:float, yAxisX:float, yAxisY:float, yAxisZ:float) : Rect3D =
        #nowarn "44"
        Rect3D(originX, originY, originZ, xAxisX, xAxisY, xAxisZ, yAxisX, yAxisY, yAxisZ)
        #warnon "44" // re-enable warning for obsolete usage

    /// Create a 3D-rectangle from the origin point and X-edge and Y edge.
    /// Does not check for perpendicularity.
    static member inline createUncheckedVec (origin:Pnt, x:Vec, y:Vec) : Rect3D =
        Rect3D.createUnchecked(origin.X, origin.Y, origin.Z, x.X, x.Y, x.Z, y.X, y.Y, y.Z)

    /// Creates a 3D Point from r.OriginX, r.OriginY and r.OriginZ
    member r.Origin : Pnt =
        Pnt(r.OriginX, r.OriginY, r.OriginZ)

    /// Creates a 3D Point from r.OriginX, r.OriginY and r.OriginZ
    static member inline origin (r:Rect3D) : Pnt =
        r.Origin

    /// Creates a 3D Vector from r.XaxisX, r.XaxisY and r.XaxisZ
    member r.Yaxis : Vec =
        Vec(r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Creates a 3D Vector from r.XaxisX, r.XaxisY and r.XaxisZ
    member r.Xaxis : Vec =
        Vec(r.XaxisX, r.XaxisY, r.XaxisZ)

    /// Creates a 3D Vector from r.YaxisX, r.YaxisY and r.YaxisZ
    static member inline xAxis (r:Rect3D) : Vec =
        r.Xaxis

    /// Creates a 3D Vector from r.YaxisX, r.YaxisY and r.YaxisZ
    static member inline yAxis (r:Rect3D) : Vec =
        r.Yaxis

    /// The size in X direction
    member inline r.SizeX : float =
        sqrt(r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY + r.XaxisZ*r.XaxisZ)

    /// The size in X direction.
    static member inline sizeX (r:Rect3D) : float =
        r.SizeX

    /// The size in Y direction
    member inline r.SizeY : float =
        sqrt(r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY + r.YaxisZ*r.YaxisZ)

    /// The size in Y direction.
    static member inline sizeY (r:Rect3D) : float =
        r.SizeY

    /// The squared size in X direction
    member inline r.SizeXSq : float =
        r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY + r.XaxisZ*r.XaxisZ

    /// The squared size in X direction.
    static member inline sizeXSq (r:Rect3D) : float =
        r.SizeXSq

    /// The squared size in Y direction
    member inline r.SizeYSq : float =
        r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY + r.YaxisZ*r.YaxisZ

    /// The squared size in Y direction.
    static member inline sizeYSq (r:Rect3D) : float =
        r.SizeYSq

    /// Nicely formatted string representation of the 3D-rectangle including its size.
    override r.ToString() : string =
        let sizeX = Format.float r.SizeX
        let sizeY = Format.float r.SizeY
        let origin = $"X={Format.float r.OriginX}|Y={Format.float r.OriginY}|Z={Format.float r.OriginZ}"
        let xAxis = $"X={Format.float r.XaxisX}|Y={Format.float r.XaxisY}|Z={Format.float r.XaxisZ}"
        let yAxis = $"X={Format.float r.YaxisX}|Y={Format.float r.YaxisY}|Z={Format.float r.YaxisZ}"
        $"Euclid.Rect3D {sizeX} x {sizeY} (Origin:{origin}| X-ax:{xAxis}|Y-ax:{yAxis})"


    /// Format the 3D-rectangle into string with nice floating point number formatting of X and Y size only.
    /// But without type name as in v.ToString()
    member r.AsString : string =
        let sizeX = Format.float r.SizeX
        let sizeY = Format.float r.SizeY
        $"%s{sizeX} x %s{sizeY}"

    /// Format the 3D-rectangle into string with nice floating point number formatting of X and Y size only.
    static member inline asString (r:Rect3D) : string =
        r.AsString

    /// Format Rect3D into an F# code string that can be used to recreate the rectangle.
    member r.AsFSharpCode : string =
        $"Rect3D.createUnchecked({r.OriginX}, {r.OriginY}, {r.OriginZ}, {r.XaxisX}, {r.XaxisY}, {r.XaxisZ}, {r.YaxisX}, {r.YaxisY}, {r.YaxisZ})"

    /// Format Rect3D into an F# code string that can be used to recreate the rectangle.
    static member inline asFSharpCode (r:Rect3D) : string =
        r.AsFSharpCode

    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit : UnitVec =
        let x = r.XaxisX
        let y = r.XaxisY
        let z = r.XaxisZ
        let sqLen = x*x + y*y + z*z
        if isTooTinySq sqLen then
            failTooSmall "Rect3D.XaxisUnit: box Xaxis" r
        let f = 1.0 / sqrt sqLen
        UnitVec.createUnchecked(x*f, y*f, z*f)

    /// Creates a unitized version of the local X-Axis.
    static member inline xAxisUnit (r:Rect3D) : UnitVec =
        r.XaxisUnit

    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit : UnitVec =
        let x = r.YaxisX
        let y = r.YaxisY
        let z = r.YaxisZ
        let sqLen = x*x + y*y + z*z
        if isTooTinySq sqLen then
            failTooSmall "Rect3D.YaxisUnit: box Yaxis" r
        let f = 1.0 / sqrt sqLen
        UnitVec.createUnchecked(x*f, y*f, z*f)

    /// Creates a unitized version of the local Y-Axis.
    static member inline yAxisUnit (r:Rect3D) : UnitVec =
        r.YaxisUnit

    /// Returns the Normal
    /// Resulting from the Cross Product of r.Xaxis with r.Yaxis.
    member inline r.Normal : Vec =
        Vec(r.XaxisY*r.YaxisZ - r.XaxisZ*r.YaxisY,
            r.XaxisZ*r.YaxisX - r.XaxisX*r.YaxisZ,
            r.XaxisX*r.YaxisY - r.XaxisY*r.YaxisX)

    /// Returns the Normal resulting from the Cross Product of Xaxis with Yaxis.
    static member inline normal (r:Rect3D) : Vec =
        r.Normal

    /// Returns the unitized Normal.
    /// Resulting from the Cross Product of r.Xaxis with r.Yaxis.
    member r.NormalUnit : UnitVec =
        // cross product:
        let x = r.XaxisY * r.YaxisZ - r.XaxisZ * r.YaxisY
        let y = r.XaxisZ * r.YaxisX - r.XaxisX * r.YaxisZ
        let z = r.XaxisX * r.YaxisY - r.XaxisY * r.YaxisX
        let len = sqrt (x*x + y*y + z*z)
        if isTooTiny len then
            failTooSmall "Rect3D.NormalUnit: rect" r
        let f = 1.0 / len
        UnitVec.createUnchecked(x*f, y*f, z*f)

    /// Returns the unitized Normal resulting from the Cross Product of Xaxis with Yaxis.
    static member inline normalUnit (r:Rect3D) : UnitVec =
        r.NormalUnit

    /// Returns the diagonal vector of the 3D-rectangle.
    /// From Origin to FarCorner.
    member inline r.Diagonal : Vec =
        Vec(r.XaxisX + r.YaxisX, r.XaxisY + r.YaxisY, r.XaxisZ + r.YaxisZ)

    /// Returns the diagonal vector of the 3D-rectangle.
    static member inline diagonal (r:Rect3D) : Vec =
        r.Diagonal

    /// Returns the center of the 3D-rectangle.
    member inline r.Center : Pnt =
        Pnt(r.OriginX + (r.XaxisX + r.YaxisX)*0.5,
            r.OriginY + (r.XaxisY + r.YaxisY)*0.5,
            r.OriginZ + (r.XaxisZ + r.YaxisZ)*0.5)

    /// Returns the center of the 3D-rectangle.
    static member inline center (r:Rect3D) : Pnt =
        r.Center

    /// Evaluate a X and Y parameter of the 3D-rectangle.
    ///  0.0, 0.0 returns the Origin.
    ///  1.0, 1.0 returns the FarCorner.
    member inline r.EvaluateAt (xParameter:float, yParameter:float) : Pnt =
        Pnt(r.OriginX + r.XaxisX*xParameter + r.YaxisX*yParameter,
            r.OriginY + r.XaxisY*xParameter + r.YaxisY*yParameter,
            r.OriginZ + r.XaxisZ*xParameter + r.YaxisZ*yParameter)

    /// Evaluate a X and Y parameter of the 3D-rectangle.
    static member inline evaluateAt (xParameter:float) (yParameter:float) (r:Rect3D) : Pnt =
        r.EvaluateAt(xParameter, yParameter)

    /// Evaluate a point at X and Y distance on the respective axes of the 3D-rectangle.
    member inline r.EvaluateDist (xDistance:float, yDistance:float) : Pnt =
        let lx = sqrt(r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY + r.XaxisZ*r.XaxisZ)
        let ly = sqrt(r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY + r.YaxisZ*r.YaxisZ)
        if isTooTiny (lx) || isTooTiny (ly) then
            failTooSmall "Rect3D.EvaluateDist: rect Xaxis or Yaxis" r
        let xf = xDistance/lx
        let yf = yDistance/ly
        Pnt(r.OriginX + r.XaxisX*xf + r.YaxisX*yf,
            r.OriginY + r.XaxisY*xf + r.YaxisY*yf,
            r.OriginZ + r.XaxisZ*xf + r.YaxisZ*yf)

    /// Evaluate a point at X and Y distance on the respective axes of the 3D-rectangle.
    static member inline evaluateDist (xDistance:float) (yDistance:float) (r:Rect3D) : Pnt =
        r.EvaluateDist(xDistance, yDistance)

    /// Returns the X and Y parameters of the closest point on the 3D-rectangle to a given point.
    /// If the parameters are outside the range of 0.0 to 1.0, the closest point is outside the rectangle.
    member r.ClosestParameters (p:Pnt) : float*float =
        let vx = p.X - r.OriginX
        let vy = p.Y - r.OriginY
        let vz = p.Z - r.OriginZ
        let xLenSq = r.SizeXSq
        let yLenSq = r.SizeYSq
        if isTooTiny (xLenSq) || isTooTiny (yLenSq) then
            failTooSmall "Rect3D.ClosestParameters: rect Xaxis or Yaxis" r
        let xParam = (vx*r.XaxisX + vy*r.XaxisY + vz*r.XaxisZ) / xLenSq
        let yParam = (vx*r.YaxisX + vy*r.YaxisY + vz*r.YaxisZ) / yLenSq
        xParam, yParam

    /// Returns the X and Y parameters of the closest point on the 3D-rectangle to a given point.
    /// If the parameters are outside the range of 0.0 to 1.0, the closest point is outside the rectangle.
    static member inline closestParameters (p:Pnt) (r:Rect3D) : float*float =
        r.ClosestParameters(p)

    /// Returns the closest point on the 3D-rectangle to a given point.
    /// If the closest parameter is outside 0.0 to 1.0, the closest point will still be clamped onto the edge of the rectangle.
    member r.ClosestPoint (p:Pnt) : Pnt =
        let xParam, yParam = r.ClosestParameters(p)
        r.EvaluateAt(UtilEuclid.clamp01 xParam, UtilEuclid.clamp01 yParam)

    /// Returns the closest point on the 3D-rectangle to a given point.
    /// If the closest parameter is outside 0.0 to 1.0, the closest point will still be clamped onto the edge of the rectangle.
    static member inline closestPoint (p:Pnt) (r:Rect3D) : Pnt =
        r.ClosestPoint(p)

    /// Calculates the area of the 3D-rectangle.
    member inline r.Area : float =
        sqrt(r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY + r.XaxisZ*r.XaxisZ) * sqrt(r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY + r.YaxisZ*r.YaxisZ)

    /// Calculates the area of the 3D-rectangle.
    static member inline area (r:Rect3D) : float =
        r.Area

    /// Scales the 3D rectangle by a given factor.
    /// Scale center is World Origin 0,0
    member inline r.Scale (factor:float) : Rect3D =
        Rect3D.createUnchecked(
                r.OriginX * factor,
                r.OriginY * factor,
                r.OriginZ * factor,
                r.XaxisX * factor,
                r.XaxisY * factor,
                r.XaxisZ * factor,
                r.YaxisX * factor,
                r.YaxisY * factor,
                r.YaxisZ * factor)

    /// Scales the 3D rectangle by a given factor.
    /// Scale center is World Origin 0,0,0
    static member inline scale (factor:float) (r:Rect3D) : Rect3D =
        Rect3D.createUnchecked(
                r.OriginX * factor,
                r.OriginY * factor,
                r.OriginZ * factor,
                r.XaxisX * factor,
                r.XaxisY * factor,
                r.XaxisZ * factor,
                r.YaxisX * factor,
                r.YaxisY * factor,
                r.YaxisZ * factor)

    /// Scales the 3D rectangle by a given factor on a given center point
    member inline l.ScaleOn (cen:Pnt) (factor:float) : Rect3D =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        Rect3D.createUnchecked(
            cx + (l.OriginX - cx) * factor,
            cy + (l.OriginY - cy) * factor,
            cz + (l.OriginZ - cz) * factor,
            l.XaxisX * factor,
            l.XaxisY * factor,
            l.XaxisZ * factor,
            l.YaxisX * factor,
            l.YaxisY * factor,
            l.YaxisZ * factor
        )

    /// Scales the 3D rectangle by a given factor on a given center point.
    static member inline scaleOn (cen:Pnt) (factor:float) (r:Rect3D) : Rect3D =
        r.ScaleOn cen factor

    /// Returns a 3D rectangle moved by a vector. Same as Rect3D.move and Rect3D.translate.
    member inline r.Move (v:Vec) : Rect3D =
        Rect3D.createUnchecked(r.OriginX + v.X, r.OriginY + v.Y, r.OriginZ + v.Z, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Move the 3D-rectangle by a vector.(same as Rect3D.translate)
    static member move (v:Vec) (r:Rect3D)  : Rect3D =
        Rect3D.createUnchecked(r.OriginX + v.X, r.OriginY + v.Y, r.OriginZ + v.Z, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Returns a 3D rectangle moved by a given distance in X direction.
    member inline r.MoveX (distance:float) : Rect3D =
        Rect3D.createUnchecked(r.OriginX + distance, r.OriginY, r.OriginZ, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Returns a 3D rectangle moved by a given distance in X direction.
    static member inline moveX (distance:float) (r:Rect3D) : Rect3D =
        Rect3D.createUnchecked(r.OriginX + distance, r.OriginY, r.OriginZ, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Returns a 3D rectangle moved by a given distance in Y direction.
    member inline r.MoveY (distance:float) : Rect3D =
        Rect3D.createUnchecked(r.OriginX, r.OriginY + distance, r.OriginZ, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Returns a 3D rectangle moved by a given distance in Y direction.
    static member inline moveY (distance:float) (r:Rect3D) : Rect3D =
        Rect3D.createUnchecked(r.OriginX, r.OriginY + distance, r.OriginZ, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Returns a 3D rectangle moved by a given distance in Z direction.
    member inline r.MoveZ (distance:float) : Rect3D =
        Rect3D.createUnchecked(r.OriginX, r.OriginY, r.OriginZ + distance, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Returns a 3D rectangle moved by a given distance in Z direction.
    static member inline moveZ (distance:float) (r:Rect3D) : Rect3D =
        Rect3D.createUnchecked(r.OriginX, r.OriginY, r.OriginZ + distance, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Multiplies (or applies) a RigidMatrix to a 3D rectangle.
    /// The returned 3D rectangle is guaranteed to have still orthogonal vectors.
    member inline r.TransformRigid (m:RigidMatrix) : Rect3D =
        let mutable x = r.OriginX
        let mutable y = r.OriginY
        let mutable z = r.OriginZ
        let ox = m.M11*x + m.M21*y + m.M31*z + m.X41
        let oy = m.M12*x + m.M22*y + m.M32*z + m.Y42
        let oz = m.M13*x + m.M23*y + m.M33*z + m.Z43
        x <- r.XaxisX
        y <- r.XaxisY
        z <- r.XaxisZ
        let xx = m.M11*x + m.M21*y + m.M31*z
        let xy = m.M12*x + m.M22*y + m.M32*z
        let xz = m.M13*x + m.M23*y + m.M33*z
        x <- r.YaxisX
        y <- r.YaxisY
        z <- r.YaxisZ
        let yx = m.M11*x + m.M21*y + m.M31*z
        let yy = m.M12*x + m.M22*y + m.M32*z
        let yz = m.M13*x + m.M23*y + m.M33*z
        Rect3D.createUnchecked(ox, oy, oz, xx, xy, xz, yx, yy, yz)

    /// Transforms the 3D-rectangle by the given RigidMatrix.
    /// The returned 3D-rectangle is guaranteed to have orthogonal vectors.
    static member inline transformRigid  (m:RigidMatrix) (r:Rect3D) : Rect3D =
        r.TransformRigid m

    /// Multiplies (or applies) a Quaternion to a 3D rectangle.
    /// The rectangle is rotated around the World Origin.
    member inline r.Rotate (q:Quaternion) : Rect3D =
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let mutable x = r.OriginX
        let mutable y = r.OriginY
        let mutable z = r.OriginZ
        let mutable tx = 2.0 * ( qy * z - qz * y)
        let mutable ty = 2.0 * ( qz * x - qx * z)
        let mutable tz = 2.0 * ( qx * y - qy * x)
        let ox = x + qw * tx + qy * tz - qz * ty
        let oy = y + qw * ty + qz * tx - qx * tz
        let oz = z + qw * tz + qx * ty - qy * tx
        x <- r.XaxisX
        y <- r.XaxisY
        z <- r.XaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let xx =  x + qw * tx + qy * tz - qz * ty
        let xy =  y + qw * ty + qz * tx - qx * tz
        let xz =  z + qw * tz + qx * ty - qy * tx
        x <- r.YaxisX
        y <- r.YaxisY
        z <- r.YaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let yx =  x + qw * tx + qy * tz - qz * ty
        let yy =  y + qw * ty + qz * tx - qx * tz
        let yz =  z + qw * tz + qx * ty - qy * tx
        Rect3D.createUnchecked(ox, oy, oz, xx, xy, xz, yx, yy, yz)

    /// Multiplies (or applies) a Quaternion to a 3D rectangle.
    /// The rectangle is rotated around the World Origin.
    static member inline rotate (q:Quaternion) (r:Rect3D) : Rect3D =
        r.Rotate q

    /// Multiplies (or applies) a Quaternion to a 3D rectangle around a given center point.
    member inline r.RotateWithCenter (cen:Pnt, q:Quaternion) : Rect3D =
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let mutable x = r.OriginX - cen.X
        let mutable y = r.OriginY - cen.Y
        let mutable z = r.OriginZ - cen.Z
        let mutable tx = 2.0 * ( qy * z - qz * y)
        let mutable ty = 2.0 * ( qz * x - qx * z)
        let mutable tz = 2.0 * ( qx * y - qy * x)
        let ox = x + qw * tx + qy * tz - qz * ty + cen.X
        let oy = y + qw * ty + qz * tx - qx * tz + cen.Y
        let oz = z + qw * tz + qx * ty - qy * tx + cen.Z
        x <- r.XaxisX
        y <- r.XaxisY
        z <- r.XaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let xx =  x + qw * tx + qy * tz - qz * ty
        let xy =  y + qw * ty + qz * tx - qx * tz
        let xz =  z + qw * tz + qx * ty - qy * tx
        x <- r.YaxisX
        y <- r.YaxisY
        z <- r.YaxisZ
        tx <- 2.0 * ( qy * z - qz * y)
        ty <- 2.0 * ( qz * x - qx * z)
        tz <- 2.0 * ( qx * y - qy * x)
        let yx =  x + qw * tx + qy * tz - qz * ty
        let yy =  y + qw * ty + qz * tx - qx * tz
        let yz =  z + qw * tz + qx * ty - qy * tx
        Rect3D.createUnchecked(ox, oy, oz, xx, xy, xz, yx, yy, yz)

    /// Multiplies (or applies) a Quaternion to a 3D rectangle around a given center point.
    static member inline rotateWithCenter (cen:Pnt) (q:Quaternion) (r:Rect3D) : Rect3D =
        r.RotateWithCenter( cen, q)

    /// Returns the longest edge of the Rect3D.
    member inline b.LongestEdge : float =
        let x = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ
        let y = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ
        sqrt  (max x y)

    /// Returns the longest edge of the Rect3D.
    static member inline longestEdge (r:Rect3D) : float =
        r.LongestEdge

    /// Returns the shortest edge of the Rect3D.
    member inline b.ShortestEdge : float =
        let x = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ
        let y = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ
        sqrt  (min x y)

    /// Returns the shortest edge of the Rect3D.
    static member inline shortestEdge (r:Rect3D) : float =
        r.ShortestEdge

    /// Returns the square length of longest edge of the Rect3D.
    member inline b.LongestEdgeSq : float =
        let x = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ
        let y = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ
        max x y

    /// Returns the square length of longest edge of the Rect3D.
    static member inline longestEdgeSq (r:Rect3D) : float =
        r.LongestEdgeSq

    /// Returns the square length of shortest edge of the Rect3D.
    member inline b.ShortestEdgeSq : float =
        let x = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ
        let y = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ
        min x y

    /// Returns the square length of shortest edge of the Rect3D.
    static member inline shortestEdgeSq (r:Rect3D) : float =
        r.ShortestEdgeSq

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsPoint.
    member inline b.IsZero : bool =
        isTooTinySq (b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ) &&
        isTooTinySq (b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ)

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as isPoint.
    static member inline isZero (r:Rect3D) : bool =
        r.IsZero

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsZero.
    member inline b.IsPoint : bool =
        isTooTinySq (b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ) &&
        isTooTinySq (b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ)

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as isZero.
    static member inline isPoint (r:Rect3D) : bool =
        r.IsPoint

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, 2 .
    member inline b.CountZeroSides : int =
        countTooTinySqOrNaN    (b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY + b.XaxisZ*b.XaxisZ)
        +  countTooTinySqOrNaN (b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY + b.YaxisZ*b.YaxisZ)

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    static member inline countZeroSides (r:Rect3D) : int =
        r.CountZeroSides

    /// Tests if two of the X and Y axis is smaller than the zeroLength tolerance.
    member inline b.IsLine : bool =
        b.CountZeroSides = 1

    /// Tests if two of the X and Y axis is smaller than the zeroLength tolerance.
    static member inline isLine (r:Rect3D) : bool =
        r.IsLine

    /// Tests if no sides of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .HasArea
    member inline b.IsValid : bool =
        b.CountZeroSides = 0

    /// Tests if no sides of the X and Y axis is smaller than the zeroLength tolerance.
    static member inline isValid (r:Rect3D) : bool =
        r.IsValid

    /// Tests if none of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasArea : bool =
        b.CountZeroSides = 0

    /// Tests if none of the X and Y axis is smaller than the zeroLength tolerance.
    static member inline hasArea (r:Rect3D) : bool =
        r.HasArea

    /// Gets the Plane that this 3D-rectangle is based on.
    member inline r.Plane : PPlane =
        let lenX = sqrt(r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY + r.XaxisZ*r.XaxisZ)
        let lenY = sqrt(r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY + r.YaxisZ*r.YaxisZ)
        if isTooTiny lenX || isTooTiny lenY then
            failTooSmall "Rect3D.Plane: rect Xaxis or Yaxis" r
        let ux = r.XaxisX / lenX
        let uy = r.XaxisY / lenX
        let uz = r.XaxisZ / lenX
        let vx = r.YaxisX / lenY
        let vy = r.YaxisY / lenY
        let vz = r.YaxisZ / lenY
        let nx = uy * vz - uz * vy
        let ny = uz * vx - ux * vz
        let nz = ux * vy - uy * vx
        PPlane.createUnchecked(r.OriginX, r.OriginY, r.OriginZ, ux, uy, uz, vx, vy, vz, nx, ny, nz)

    /// Gets the Plane that this 3D-rectangle is based on.
    static member inline plane (r:Rect3D) : PPlane =
        r.Plane


    /// <summary>Returns the corner diagonally opposite of corner from Origin (point 2).
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.FarCorner : Pnt =
        Pnt(r.OriginX + r.XaxisX + r.YaxisX,
            r.OriginY + r.XaxisY + r.YaxisY,
            r.OriginZ + r.XaxisZ + r.YaxisZ)

    /// Returns the corner diagonally opposite of the origin corner.
    static member inline farCorner (r:Rect3D) : Pnt =
        r.FarCorner

    /// <summary>Returns the corner at end of X-axis (point 1).
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.XCorner : Pnt =
        Pnt(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY, r.OriginZ + r.XaxisZ)

    /// Returns the corner at end of X-axis.
    static member inline xCorner (r:Rect3D) : Pnt =
        r.XCorner

    /// <summary>Returns the corner at end of Y-axis (point 3).
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.YCorner : Pnt =
        Pnt(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginZ + r.YaxisZ)

    /// Returns the corner at end of Y-axis.
    static member inline yCorner (r:Rect3D) : Pnt =
        r.YCorner

    /// <summary>Returns point 0 of the 3D-rectangle. Same as member rect.Origin.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Pt0 : Pnt =
        Pnt(r.OriginX, r.OriginY, r.OriginZ)

    /// Returns point 0 of the 3D-rectangle.
    static member inline pt0 (r:Rect3D) : Pnt =
        r.Pt0

    /// <summary>Returns point 1 of the 3D-rectangle.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Pt1 : Pnt =
        Pnt(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY, r.OriginZ + r.XaxisZ)

    /// Returns point 1 of the 3D-rectangle.
    static member inline pt1 (r:Rect3D) : Pnt =
        r.Pt1

    /// <summary>Returns point 2 of the 3D-rectangle. Same as rect.FarCorner.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Pt2 : Pnt =
        Pnt(r.OriginX + r.XaxisX + r.YaxisX,
            r.OriginY + r.XaxisY + r.YaxisY,
            r.OriginZ + r.XaxisZ + r.YaxisZ)

    /// Returns point 2 of the 3D-rectangle.
    static member inline pt2 (r:Rect3D) : Pnt =
        r.Pt2

    /// <summary>Returns point 3 of the 3D-rectangle.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Pt3 : Pnt =
        Pnt(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginZ + r.YaxisZ)

    /// Returns point 3 of the 3D-rectangle.
    static member inline pt3 (r:Rect3D) : Pnt =
        r.Pt3

    /// <summary>Returns a 3D line from point 0 to 1 of the 3D-rectangle.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge01 : Line3D =
        Line3D(r.OriginX, r.OriginY, r.OriginZ, r.OriginX + r.XaxisX, r.OriginY + r.XaxisY, r.OriginZ + r.XaxisZ)

    /// Returns a 3D line from point 0 to 1 of the rectangle.
    static member inline edge01 (r:Rect3D) : Line3D =
        r.Edge01

    /// <summary>Returns a 3D line from point 1 to 2 of the 3D-rectangle.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge12 : Line3D =
        let sx = r.OriginX + r.XaxisX
        let sy = r.OriginY + r.XaxisY
        let sz = r.OriginZ + r.XaxisZ
        Line3D(sx, sy, sz, sx + r.YaxisX, sy + r.YaxisY, sz + r.YaxisZ)

    /// Returns a 3D line from point 1 to 2 of the rectangle.
    static member inline edge12 (r:Rect3D) : Line3D =
        r.Edge12

    /// <summary>Returns a 3D line from point 2 to 3 of the 3D-rectangle.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge23 : Line3D =
        let p3x = r.OriginX + r.YaxisX
        let p3y = r.OriginY + r.YaxisY
        let p3z = r.OriginZ + r.YaxisZ
        Line3D(p3x + r.XaxisX, p3y + r.XaxisY, p3z + r.XaxisZ, p3x, p3y, p3z)

    /// Returns a 3D line from point 2 to 3 of the rectangle.
    static member inline edge23 (r:Rect3D) : Line3D =
        r.Edge23

    /// <summary>Returns a 3D line from point 3 to 0 of the 3D-rectangle.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge30 : Line3D =
        Line3D(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginZ + r.YaxisZ, r.OriginX, r.OriginY, r.OriginZ)

    /// Returns a 3D line from point 3 to 0 of the rectangle.
    static member inline edge30 (r:Rect3D) : Line3D =
        r.Edge30

    /// <summary>Returns the local X side as the 3D line from point 0 to 1 of the 3D-rectangle.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.EdgeX : Line3D =
        r.Edge01

    /// Returns the local X side as a 3D line from point 0 to 1.
    static member inline edgeX (r:Rect3D) : Line3D =
        r.EdgeX

    /// <summary>Returns the local Y side as 3D line from point 0 to 3 of the 3D-rectangle.
    /// This is the reverse of Edge30.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.EdgeY : Line3D =
        Line3D(r.OriginX, r.OriginY, r.OriginZ, r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginZ + r.YaxisZ)

    /// Returns the local Y side as a 3D line from point 0 to 3.
    static member inline edgeY (r:Rect3D) : Line3D =
        r.EdgeY

    /// <summary>Returns the diagonal 3D line from point 0 to 2 of the 3D-rectangle.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.DiagonalLine : Line3D =
        Line3D(r.OriginX, r.OriginY, r.OriginZ, r.OriginX + r.YaxisX + r.XaxisX, r.OriginY + r.YaxisY + r.XaxisY, r.OriginZ + r.YaxisZ + r.XaxisZ)

    /// Returns the diagonal 3D line from point 0 to 2.
    static member inline diagonalLine (r:Rect3D) : Line3D =
        r.DiagonalLine

    /// <summary>Returns the Rectangle flipped. Or rotated 180 around its diagonal from point 1 to 3.
    /// The normal of the rectangle gets flipped.
    /// Origin will be at point 2, X-axis points down to to point 1, Y-axis points left to point 3.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.Flipped : Rect3D =
        Rect3D.createUnchecked(
            r.OriginX + r.XaxisX + r.YaxisX,
            r.OriginY + r.XaxisY + r.YaxisY,
            r.OriginZ + r.XaxisZ + r.YaxisZ,
            -r.YaxisX, -r.YaxisY, -r.YaxisZ,
            -r.XaxisX, -r.XaxisY, -r.XaxisZ)

    /// Returns the Rectangle flipped.
    static member inline flipped (r:Rect3D) : Rect3D =
        r.Flipped

    /// <summary>Returns the same rectangle with a new orientation rotated by 90 degrees clockwise around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 3, X-axis to to point 0, Y-axis to point 2.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.RotateOrientation90CW : Rect3D =
        Rect3D.createUnchecked(
            r.OriginX + r.YaxisX,
            r.OriginY + r.YaxisY,
            r.OriginZ + r.YaxisZ,
            -r.YaxisX, -r.YaxisY, -r.YaxisZ,
            r.XaxisX, r.XaxisY, r.XaxisZ)

    /// Returns the same rectangle with orientation rotated by 90 degrees clockwise around its center.
    static member inline rotateOrientation90CW (r:Rect3D) : Rect3D =
        r.RotateOrientation90CW

    /// <summary>Returns the Rectangle rotated 180 degrees around its center.
    /// Returns the same rectangle with a new orientation rotated by 180 degrees around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 2, X-axis to to point 3, Y-axis to point 1.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.RotateOrientation180 : Rect3D =
        Rect3D.createUnchecked(
            r.OriginX + r.YaxisX + r.XaxisX,
            r.OriginY + r.YaxisY + r.XaxisY,
            r.OriginZ + r.YaxisZ + r.XaxisZ,
            -r.XaxisX, -r.XaxisY, -r.XaxisZ,
            -r.YaxisX, -r.YaxisY, -r.YaxisZ)

    /// Returns the same rectangle with orientation rotated by 180 degrees around its center.
    static member inline rotateOrientation180 (r:Rect3D) : Rect3D =
        r.RotateOrientation180

    /// <summary>Returns the same rectangle with a new orientation rotated by 90 degrees counter clockwise around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 1, X-axis to to point 2, Y-axis to point 0.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.RotateOrientation90CCW : Rect3D =
        Rect3D.createUnchecked(
            r.OriginX + r.XaxisX,
            r.OriginY + r.XaxisY,
            r.OriginZ + r.XaxisZ,
            r.YaxisX, r.YaxisY, r.YaxisZ,
            -r.XaxisX, -r.XaxisY, -r.XaxisZ)

    /// Returns the same rectangle with orientation rotated by 90 degrees counter clockwise around its center.
    static member inline rotateOrientation90CCW (r:Rect3D) : Rect3D =
        r.RotateOrientation90CCW

    /// <summary>Returns the 4 corners of the 3D-rectangle in Counter-Clockwise order, starting at Origin.
    /// Returns an array of 4 Points: point 0 then 1, 2 and 3.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.Points :Pnt[] =
        let p0 = Pnt(r.OriginX, r.OriginY, r.OriginZ)
        let p1 = Pnt(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY, r.OriginZ + r.XaxisZ)
        let p2 = Pnt(r.OriginX + r.XaxisX + r.YaxisX, r.OriginY + r.XaxisY + r.YaxisY, r.OriginZ + r.XaxisZ + r.YaxisZ)
        let p3 = Pnt(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginZ + r.YaxisZ)
        [| p0 ; p1 ; p2; p3|]

    /// Returns the 4 corners of the 3D-rectangle in counter-clockwise order, starting at origin.
    static member inline points (r:Rect3D) : Pnt[] =
        r.Points

    /// <summary>Returns the 4 corners of the 3D-rectangle als closed loop in Counter-Clockwise order, starting at Origin.
    /// First and last point are the same.
    /// Returns an array of 5 Points: point 0 then 1, 2, 3 and again 0.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.PointsLooped :Pnt[] =
        let p0 = Pnt(r.OriginX, r.OriginY, r.OriginZ)
        let p1 = Pnt(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY, r.OriginZ + r.XaxisZ)
        let p2 = Pnt(r.OriginX + r.XaxisX + r.YaxisX, r.OriginY + r.XaxisY + r.YaxisY, r.OriginZ + r.XaxisZ + r.YaxisZ)
        let p3 = Pnt(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginZ + r.YaxisZ)
        [| p0 ; p1 ; p2; p3; p0|]

    /// Returns the 4 corners of the 3D-rectangle as closed loop in counter-clockwise order, starting at origin.
    static member inline pointsLooped (r:Rect3D) : Pnt[] =
        r.PointsLooped

    /// <summary> Returns the 4 corners of the 3D Rectangle as an open loop of 12 floats, starting at Origin.
    /// Returns a ResizeArray of 12 floats: x, y, and z of point 0, point 1, point 2 and point 3.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.PointsXY :ResizeArray<float> =
        let xys = ResizeArray<float>(12)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginZ)
        xys.Add(r.OriginX + r.XaxisX)
        xys.Add(r.OriginY + r.XaxisY)
        xys.Add(r.OriginZ + r.XaxisZ)
        xys.Add(r.OriginX + r.XaxisX + r.YaxisX)
        xys.Add(r.OriginY + r.XaxisY + r.YaxisY)
        xys.Add(r.OriginZ + r.XaxisZ + r.YaxisZ)
        xys.Add(r.OriginX + r.YaxisX)
        xys.Add(r.OriginY + r.YaxisY)
        xys.Add(r.OriginZ + r.YaxisZ)
        xys

    /// Returns the 4 corners of the 3D Rectangle as open loop, starting at Origin.
    /// Returns a ResizeArray of 12 floats: x, y, and z of point 0, point 1, point 2 and point 3.
    static member inline pointsXY (r:Rect3D) : ResizeArray<float> =
        r.PointsXY

    /// <summary> Returns the 5 corners of the 3D Rectangle as closed loop of 15 floats, starting at Origin.
    /// Returns a ResizeArray of 15 floats: x, y, and z of point 0, point 1, point 2, point 3 and point 0 again.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.PointsLoopedXY :ResizeArray<float> =
        let xys = ResizeArray<float>(15)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginZ)
        xys.Add(r.OriginX + r.XaxisX)
        xys.Add(r.OriginY + r.XaxisY)
        xys.Add(r.OriginZ + r.XaxisZ)
        xys.Add(r.OriginX + r.XaxisX + r.YaxisX)
        xys.Add(r.OriginY + r.XaxisY + r.YaxisY)
        xys.Add(r.OriginZ + r.XaxisZ + r.YaxisZ)
        xys.Add(r.OriginX + r.YaxisX)
        xys.Add(r.OriginY + r.YaxisY)
        xys.Add(r.OriginZ + r.YaxisZ)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginZ)
        xys

    /// <summary>Iterates the 4 corners of the 3D Rectangle in Counter-Clockwise order, starting at Origin.</summary>
    /// <param name="action">The action to call 4 times. Once for each corner, with x, y and z as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsCCW (action: float -> float -> float -> unit) (r:Rect3D) : unit =
        action r.OriginX r.OriginY r.OriginZ
        action (r.OriginX + r.XaxisX) (r.OriginY + r.XaxisY) (r.OriginZ + r.XaxisZ)
        action (r.OriginX + r.XaxisX + r.YaxisX) (r.OriginY + r.XaxisY + r.YaxisY) (r.OriginZ + r.XaxisZ + r.YaxisZ)
        action (r.OriginX + r.YaxisX) (r.OriginY + r.YaxisY) (r.OriginZ + r.YaxisZ)

    /// <summary>Iterates the 4 corners of the 3D Rectangle as closed loop in Counter-Clockwise order, starting and ending at Origin.</summary>
    /// <param name="action">The action to call 5 times, with x, y and z as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsLoopedCCW (action: float -> float -> float -> unit) (r:Rect3D) : unit =
        action r.OriginX r.OriginY r.OriginZ
        action (r.OriginX + r.XaxisX) (r.OriginY + r.XaxisY) (r.OriginZ + r.XaxisZ)
        action (r.OriginX + r.XaxisX + r.YaxisX) (r.OriginY + r.XaxisY + r.YaxisY) (r.OriginZ + r.XaxisZ + r.YaxisZ)
        action (r.OriginX + r.YaxisX) (r.OriginY + r.YaxisY) (r.OriginZ + r.YaxisZ)
        action r.OriginX r.OriginY r.OriginZ

    /// <summary>Iterates the 4 corners of the 3D Rectangle in Clockwise order, starting at Origin.</summary>
    /// <param name="action">The action to call 4 times. Once for each corner, with x, y and z as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsCW (action: float -> float -> float -> unit) (r:Rect3D) : unit =
        action r.OriginX r.OriginY r.OriginZ
        action (r.OriginX + r.YaxisX) (r.OriginY + r.YaxisY) (r.OriginZ + r.YaxisZ)
        action (r.OriginX + r.XaxisX + r.YaxisX) (r.OriginY + r.XaxisY + r.YaxisY) (r.OriginZ + r.XaxisZ + r.YaxisZ)
        action (r.OriginX + r.XaxisX) (r.OriginY + r.XaxisY) (r.OriginZ + r.XaxisZ)

    /// <summary>Iterates the 4 corners of the 3D Rectangle as closed loop in Clockwise order, starting and ending at Origin.</summary>
    /// <param name="action">The action to call 5 times, with x, y and z as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsLoopedCW (action: float -> float -> float -> unit) (r:Rect3D) : unit =
        action r.OriginX r.OriginY r.OriginZ
        action (r.OriginX + r.YaxisX) (r.OriginY + r.YaxisY) (r.OriginZ + r.YaxisZ)
        action (r.OriginX + r.XaxisX + r.YaxisX) (r.OriginY + r.XaxisY + r.YaxisY) (r.OriginZ + r.XaxisZ + r.YaxisZ)
        action (r.OriginX + r.XaxisX) (r.OriginY + r.XaxisY) (r.OriginZ + r.XaxisZ)
        action r.OriginX r.OriginY r.OriginZ

    /// <summary>Returns the 4 Edges of the 3D-rectangle in Counter-Clockwise order, starting at Origin.
    /// Returns an array of 4 Lines: from point
    /// 0 to 1,
    /// 1 to 2,
    /// 2 to 3,
    /// 3 to 0.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.Edges :Line3D[] =
        let p0x = r.OriginX
        let p0y = r.OriginY
        let p0z = r.OriginZ
        let p1x = p0x + r.XaxisX
        let p1y = p0y + r.XaxisY
        let p1z = p0z + r.XaxisZ
        let p2x = p1x + r.YaxisX
        let p2y = p1y + r.YaxisY
        let p2z = p1z + r.YaxisZ
        let p3x = p0x + r.YaxisX
        let p3y = p0y + r.YaxisY
        let p3z = p0z + r.YaxisZ
        [| Line3D(p0x, p0y, p0z, p1x, p1y, p1z); Line3D(p1x, p1y, p1z, p2x, p2y, p2z); Line3D(p2x, p2y, p2z, p3x, p3y, p3z); Line3D(p3x, p3y, p3z, p0x, p0y, p0z)|]

    /// Returns the 4 edges of the 3D-rectangle in counter-clockwise order, starting at origin.
    static member inline edges (r:Rect3D) : Line3D[] =
        r.Edges

    /// <summary>Returns one of the 4 Edges as 3D Line:
    /// Edge 0: from point  0 to 1
    /// Edge 1: from point  1 to 2
    /// Edge 2: from point  2 to 3
    /// Edge 3: from point  3 to 0
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    member r.GetEdge i : Line3D =
        match i with
        | 0 -> Line3D(r.OriginX, r.OriginY, r.OriginZ, r.OriginX + r.XaxisX, r.OriginY + r.XaxisY, r.OriginZ + r.XaxisZ)
        | 1 -> Line3D(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY, r.OriginZ + r.XaxisZ, r.OriginX + r.XaxisX + r.YaxisX, r.OriginY + r.XaxisY + r.YaxisY, r.OriginZ + r.XaxisZ + r.YaxisZ)
        | 2 -> Line3D(r.OriginX + r.XaxisX + r.YaxisX, r.OriginY + r.XaxisY + r.YaxisY, r.OriginZ + r.XaxisZ + r.YaxisZ, r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginZ + r.YaxisZ)
        | 3 -> Line3D(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginZ + r.YaxisZ, r.OriginX, r.OriginY, r.OriginZ)
        | _ -> fail $"Rect3D.GetEdge: index {i} out of range 0..3" |> unbox // unbox to make type checker happy

    /// Returns one of the 4 edges as a 3D line.
    static member inline getEdge (i:int) (r:Rect3D) : Line3D =
        r.GetEdge i


    // #endregion
    // #region create


    /// Create a 3D-rectangle from the origin point, an x-edge and an y-edge.
    /// Fails if x and y are not perpendicular.
    /// Fails on vectors shorter than 1e-6.
    static member createFromVectors(origin:Pnt, x:Vec, y:Vec) : Rect3D =
        let xLenSq = x.X*x.X + x.Y*x.Y + x.Z*x.Z
        let yLenSq = y.X*y.X + y.Y*y.Y + y.Z*y.Z
        if isTooSmallSq xLenSq  then failTooSmall2 "Rect3D.createFromVectors x" x y
        if isTooSmallSq yLenSq  then failTooSmall2 "Rect3D.createFromVectors y" y x
        // scale the dot-product tolerance by the axis lengths so the check is a relative angle tolerance (independent of the rectangle size):
        if abs (x.X*y.X + x.Y*y.Y + x.Z*y.Z) > sqrt(xLenSq*yLenSq) * 1e-9 then fail2 $"Rect3D.createFromVectors: X-axis and Y-axis are not perpendicular" x y
        Rect3D.createUnchecked(origin.X, origin.Y, origin.Z, x.X, x.Y, x.Z, y.X, y.Y, y.Z)

    /// Give PPlane and sizes.
    /// The Rect3D's Origin will be at the plane's Origin.
    /// Fails on negative sizes.
    static member createFromPlane (pl:PPlane, sizeX:float, sizeY:float) : Rect3D =
        if isNegative sizeX then fail $"Rect3D.createFromPlane sizeX is negative: {sizeX}, sizeY is: {sizeY}, plane: {pl.AsString}"
        if isNegative sizeY then fail $"Rect3D.createFromPlane sizeY is negative: {sizeY}, sizeX is: {sizeX}, plane: {pl.AsString}"
        Rect3D.createUncheckedVec(pl.Origin, pl.Xaxis*sizeX, pl.Yaxis*sizeY)

    /// Give PPlane and sizes.
    /// The Rect3D's center will be at the plane's Origin.
    /// Fails on negative sizes.
    static member createCenteredFromPlane (pl:PPlane, sizeX:float, sizeY:float) : Rect3D =
        if isNegative sizeX then fail $"Rect3D.createCenteredFromPlane sizeX is negative: {sizeX}, sizeY is: {sizeY}, plane: {pl.AsString}"
        if isNegative sizeY then fail $"Rect3D.createCenteredFromPlane sizeY is negative: {sizeY}, sizeX is: {sizeX}, plane: {pl.AsString}"
        let x = pl.Xaxis*sizeX
        let y = pl.Yaxis*sizeY
        Rect3D.createUncheckedVec(pl.Origin- x*0.5 - y*0.5, x, y)

    /// Creates a 3D-rectangle from the given bounds in 2D. The rectangle will be in the XY-plane with Z=0.
    static member createFromBounds2D (minX, minY, maxX, maxY)  : Rect3D =
        Rect3D.createUnchecked(minX, minY, 0.0, maxX - minX, 0.0, 0.0, 0.0, maxY - minY, 0.0)

    /// <summary>Creates a 3D-rectangle from three points. Fails if points are too close to each other or all collinear.
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y axis.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    static member createFrom3Points (origin:Pnt, xPt:Pnt, yPt:Pnt) : Rect3D =
        let x = xPt - origin
        if isTooSmallSq x.LengthSq  then fail $"Rect3D.createFrom3Points: X-Point {xPt.AsString} too close to origin: {origin.AsString}."
        let y = yPt - origin
        if isTooSmallSq y.LengthSq  then fail $"Rect3D.createFrom3Points: Y-Point {yPt.AsString} too close to origin: {origin.AsString}."
        let z = Vec.cross(x,y)
        if isTooSmallSq z.LengthSq  then fail $"Rect3D.createFrom3Points: Y-Point {yPt.AsString} is too close to Xaxis."
        let yu = Vec.cross(z, x).Unitized
        let yr = yu * (yu *** y) // get the y point projected on the y axis
        Rect3D.createUncheckedVec(origin, x, yr)

    /// <summary>Tries to create a 3D-rectangle from three points. Returns None if points are too close to each other or all collinear.
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y axis.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    static member tryCreateFrom3Points (origin:Pnt, xPt:Pnt, yPt:Pnt) : Rect3D option =
        let x = xPt - origin
        if isTooSmallSq x.LengthSq  then None
        else
            let y = yPt - origin
            if isTooSmallSq y.LengthSq  then None
            else
                let z = Vec.cross(x,y)
                if isTooSmallSq z.LengthSq  then None
                else
                    let yu = Vec.cross(z, x).Unitized
                    let yr = yu * (yu *** y) // get the y point projected on the y axis
                    Some <| Rect3D.createUncheckedVec(origin, x, yr)

    /// Creates a new 3D rectangle( = oriented bounding rectangle ) to contain the projections of all given points.
    /// But not the corners of the reference rectangle.
    /// Keeps the same plane and the same X- and Y-axis orientation as the input rectangle.
    /// For a 3D oriented bounding box use the Box.createFromPlaneAndPoints function.
    static member fitToPoints (pts:IList<Pnt>) (refRect:Rect3D) : Rect3D =
        let xLen = XYZ.length refRect.XaxisX refRect.XaxisY refRect.XaxisZ
        if isTooTiny xLen then
            failTooSmall "Rect3D.fitToPoints: Xaxis" (Vec(refRect.XaxisX, refRect.XaxisY, refRect.XaxisZ))
        let yLen = XYZ.length refRect.YaxisX refRect.YaxisY refRect.YaxisZ
        if isTooTiny yLen then
            failTooSmall "Rect3D.fitToPoints: Yaxis" (Vec(refRect.YaxisX, refRect.YaxisY, refRect.YaxisZ))
        let xX = refRect.XaxisX / xLen
        let xY = refRect.XaxisY / xLen
        let xZ = refRect.XaxisZ / xLen
        let yX = refRect.YaxisX / yLen
        let yY = refRect.YaxisY / yLen
        let yZ = refRect.YaxisZ / yLen
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        for i = 0 to pts.Count-1 do
            let vX = pts.[i].X - refRect.OriginX
            let vY = pts.[i].Y - refRect.OriginY
            let vZ = pts.[i].Z - refRect.OriginZ
            let dotX = vX*xX + vY*xY + vZ*xZ
            minX <- min minX dotX
            maxX <- max maxX dotX
            let dotY = vX*yX + vY*yY + vZ*yZ
            minY <- min minY dotY
            maxY <- max maxY dotY
        let sizeX = maxX - minX
        let sizeY = maxY - minY
        Rect3D.createUnchecked(
            refRect.OriginX + xX*minX + yX*minY,
            refRect.OriginY + xY*minX + yY*minY,
            refRect.OriginZ + xZ*minX + yZ*minY,
            xX*sizeX, xY*sizeX, xZ*sizeX,
            yX*sizeY, yY*sizeY, yZ*sizeY)


    // #endregion
    // #region Static members





    // TODO find correct implementation

    // Returns the line of intersection between two planes.
    // Returns None if they are parallel or coincident.
    // static member intersect (a:Rect3D) (b:Rect3D) : Line3D option=
    //     let an = a.NormalUnit
    //     let bn = b.NormalUnit
    //     let v = UnitVec.cross (an, bn)
    //     if isTooSmallSq v.LengthSq then
    //         None
    //     else
    //         let pa = Vec.cross(v, an)
    //         let nenner = pa *** bn
    //         let ao = a.Origin
    //         let t = ((b.Origin - ao) *** bn) / nenner
    //         let xpt = ao + pa * t
    //         let l = Line3D( xpt.X    , xpt.Y    , xpt.Z,
    //                         xpt.X+v.X, xpt.Y+v.Y, xpt.Z+v.Z)
    //         Some <| l

    /// Checks if two 3D-rectangles are equal within tolerance.
    /// Does not recognize congruent rectangles with different rotation as equal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:Rect3D) (b:Rect3D)  : bool =
        abs (a.OriginX - b.OriginX) <= tol &&
        abs (a.OriginY - b.OriginY) <= tol &&
        abs (a.OriginZ - b.OriginZ) <= tol &&
        abs (a.XaxisX  - b.XaxisX ) <= tol &&
        abs (a.XaxisY  - b.XaxisY ) <= tol &&
        abs (a.XaxisZ  - b.XaxisZ ) <= tol &&
        abs (a.YaxisX  - b.YaxisX ) <= tol &&
        abs (a.YaxisY  - b.YaxisY ) <= tol &&
        abs (a.YaxisZ  - b.YaxisZ ) <= tol

    /// Check if two 3D-rectangles are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two rectangles are not exactly equal.
    static member notEquals (tol:float) (a:Rect3D) (b:Rect3D)  : bool =
        abs (a.OriginX - b.OriginX) > tol ||
        abs (a.OriginY - b.OriginY) > tol ||
        abs (a.OriginZ - b.OriginZ) > tol ||
        abs (a.XaxisX  - b.XaxisX ) > tol ||
        abs (a.XaxisY  - b.XaxisY ) > tol ||
        abs (a.XaxisZ  - b.XaxisZ ) > tol ||
        abs (a.YaxisX  - b.YaxisX ) > tol ||
        abs (a.YaxisY  - b.YaxisY ) > tol ||
        abs (a.YaxisZ  - b.YaxisZ ) > tol

    /// Returns the 3D-rectangle expanded by distance on all four sides.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (r:Rect3D)  : Rect3D =
        let siX = r.SizeX
        let siY = r.SizeY
        let d = dist * -2.0
        if siX<=d || siY<=d  then
            fail $"Rect3D.expand: the 3D-rectangle {r.AsString} is too small to expand by negative distance {Format.float dist}"
        let xf = dist / siX
        let yf = dist / siY
        let xX = r.XaxisX * xf
        let xY = r.XaxisY * xf
        let xZ = r.XaxisZ * xf
        let yX = r.YaxisX * yf
        let yY = r.YaxisY * yf
        let yZ = r.YaxisZ * yf
        Rect3D.createUnchecked(
            r.OriginX - xX - yX,
            r.OriginY - xY - yY,
            r.OriginZ - xZ - yZ,
            r.XaxisX + xX * 2.,
            r.XaxisY + xY * 2.,
            r.XaxisZ + xZ * 2.,
            r.YaxisX + yX * 2.,
            r.YaxisY + yY * 2.,
            r.YaxisZ + yZ * 2.)

    /// Returns the 3D-rectangle expanded by respective distances on all four sides.
    /// Does check for overflow if distance is negative and fails.
    /// distX, distY are for the local X and Y-axis respectively.
    static member expandXY distX distY (r:Rect3D) : Rect3D =
        let siX = r.SizeX
        let siY = r.SizeY
        if siX <= distX * -2.0 then fail $"Rect3D.expandXY: the 3D-rectangle {r.AsString} is too small to expand by negative distance distX {Format.float distX}"
        if siY <= distY * -2.0 then fail $"Rect3D.expandXY: the 3D-rectangle {r.AsString} is too small to expand by negative distance distY {Format.float distY}"
        let xf = distX / siX
        let yf = distY / siY
        let xX = r.XaxisX * xf
        let xY = r.XaxisY * xf
        let xZ = r.XaxisZ * xf
        let yX = r.YaxisX * yf
        let yY = r.YaxisY * yf
        let yZ = r.YaxisZ * yf
        Rect3D.createUnchecked(
            r.OriginX - xX - yX,
            r.OriginY - xY - yY,
            r.OriginZ - xZ - yZ,
            r.XaxisX + xX * 2.,
            r.XaxisY + xY * 2.,
            r.XaxisZ + xZ * 2.,
            r.YaxisX + yX * 2.,
            r.YaxisY + yY * 2.,
            r.YaxisZ + yZ * 2.)

    /// Returns the 3D-rectangle expanded by a relative factor on all four sides.
    /// Values between 0.0 and 1.0 shrink the rectangle.
    /// Values larger than 1.0 expand the rectangle.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRel factor (r:Rect3D)  : Rect3D =
        if factor < 0.0  then
            fail $"Rect3D.expandRel: a negative factor {factor} is not allowed for expanding the 3D-rectangle {r.AsString}"
        let xX = r.XaxisX * factor
        let xY = r.XaxisY * factor
        let xZ = r.XaxisZ * factor
        let yX = r.YaxisX * factor
        let yY = r.YaxisY * factor
        let yZ = r.YaxisZ * factor
        Rect3D.createUnchecked(
            r.OriginX + (r.XaxisX - xX)*0.5 + (r.YaxisX - yX)*0.5,
            r.OriginY + (r.XaxisY - xY)*0.5 + (r.YaxisY - yY)*0.5,
            r.OriginZ + (r.XaxisZ - xZ)*0.5 + (r.YaxisZ - yZ)*0.5,
            xX, xY, xZ, yX, yY, yZ)

    /// Returns the 3D-rectangle expanded by a relative factor on all four sides.
    /// Values between 0.0 and 1.0 shrink the rectangle.
    /// Values larger than 1.0 expand the rectangle.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRelXY factorX factorY (r:Rect3D)  : Rect3D =
        if factorX < 0.0  then
            fail $"Rect3D.expandRelXY: a negative factor {factorX} is not allowed for expanding the 3D-rectangle {r.AsString}"
        if factorY < 0.0  then
            fail $"Rect3D.expandRelXY: a negative factor {factorY} is not allowed for expanding the 3D-rectangle {r.AsString}"
        let xX = r.XaxisX * factorX
        let xY = r.XaxisY * factorX
        let xZ = r.XaxisZ * factorX
        let yX = r.YaxisX * factorY
        let yY = r.YaxisY * factorY
        let yZ = r.YaxisZ * factorY
        Rect3D.createUnchecked(
            r.OriginX + (r.XaxisX - xX)*0.5 + (r.YaxisX - yX)*0.5,
            r.OriginY + (r.XaxisY - xY)*0.5 + (r.YaxisY - yY)*0.5,
            r.OriginZ + (r.XaxisZ - xZ)*0.5 + (r.YaxisZ - yZ)*0.5,
            xX, xY, xZ, yX, yY, yZ)


    /// <summary>Returns the Rectangle flipped. Or rotated 180 around its diagonal from point 1 to 3.
    /// Origin will be at point 2, X-axis points down to to point 1, Y-axis points left to point 3.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code>
    /// </summary>
    static member inline flip (r:Rect3D) : Rect3D =
        Rect3D.createUnchecked(
            r.OriginX + r.XaxisX + r.YaxisX,
            r.OriginY + r.XaxisY + r.YaxisY,
            r.OriginZ + r.XaxisZ + r.YaxisZ,
            -r.YaxisX, -r.YaxisY, -r.YaxisZ,
            -r.XaxisX, -r.XaxisY, -r.XaxisZ)

    /// Translate along the local X-axis of the 3D-rectangle.
    static member translateLocalX (distX:float) (r:Rect3D) : Rect3D =
        let len = XYZ.length r.XaxisX r.XaxisY r.XaxisZ
        if isTooTiny len then failTooSmall "Rect3D.translateLocalX: Xaxis" r
        let f = distX / len
        Rect3D.createUnchecked(r.OriginX + r.XaxisX*f, r.OriginY + r.XaxisY*f, r.OriginZ + r.XaxisZ*f, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Translate along the local Y-axis of the 3D-rectangle.
    static member translateLocalY (distY:float) (r:Rect3D) : Rect3D =
        let len = XYZ.length r.YaxisX r.YaxisY r.YaxisZ
        if isTooTiny len then failTooSmall "Rect3D.translateLocalY: Yaxis" r
        let f = distY / len
        Rect3D.createUnchecked(r.OriginX + r.YaxisX*f, r.OriginY + r.YaxisY*f, r.OriginZ + r.YaxisZ*f, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Translate by a 3D vector.(same as Rect3D.move)
    static member translate (v:Vec) (r:Rect3D)  : Rect3D =
        Rect3D.createUnchecked(r.OriginX + v.X, r.OriginY + v.Y, r.OriginZ + v.Z, r.XaxisX, r.XaxisY, r.XaxisZ, r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Offset or Translate along the local Z-axis.
    /// The local Z-axis is calculated from Cross Product of X and Y-axis of the 3D-rectangle.
    static member offsetZ (offsetDistance :float) (r:Rect3D) : Rect3D =
        let zX = r.XaxisY*r.YaxisZ - r.XaxisZ*r.YaxisY
        let zY = r.XaxisZ*r.YaxisX - r.XaxisX*r.YaxisZ
        let zZ = r.XaxisX*r.YaxisY - r.XaxisY*r.YaxisX
        let len = XYZ.length zX zY zZ
        if isTooTiny len then failTooSmall "Rect3D.offsetZ: rect" r
        let f = offsetDistance/len
        Rect3D.createUnchecked(
            r.OriginX + zX*f, r.OriginY + zY*f, r.OriginZ + zZ*f,
            r.XaxisX, r.XaxisY, r.XaxisZ,
            r.YaxisX, r.YaxisY, r.YaxisZ)

    /// Offset a Rect3D like a Polyline inwards by a given distance.
    /// Negative distances will offset outwards.
    /// Fails if the distance is larger than half the size of the rectangle.
    static member offset dist (rect:Rect3D) : Rect3D =
        let xl = rect.SizeX
        let yl = rect.SizeY
        if xl < dist*2.0 ||yl < dist*2.0 then
            fail $"Rect3D.offset: the 3D-rectangle {rect.AsString} is too small to offset by distance {Format.float dist}"
        let xf = dist / xl
        let yf = dist / yl
        let xX = rect.XaxisX * xf
        let xY = rect.XaxisY * xf
        let xZ = rect.XaxisZ * xf
        let yX = rect.YaxisX * yf
        let yY = rect.YaxisY * yf
        let yZ = rect.YaxisZ * yf
        Rect3D.createUnchecked(
            rect.OriginX + xX + yX,
            rect.OriginY + xY + yY,
            rect.OriginZ + xZ + yZ,
            rect.XaxisX - xX*2.0,
            rect.XaxisY - xY*2.0,
            rect.XaxisZ - xZ*2.0,
            rect.YaxisX - yX*2.0,
            rect.YaxisY - yY*2.0,
            rect.YaxisZ - yZ*2.0)

    /// <summary>Offset a Rect3D like a Polyline inwards by four distances.
    /// The distance array is for Edge01, Edge12, Edge23, and Edge30 respectively.
    /// Fails if the distance is larger than half the size of the rectangle.
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></summary>
    static member offsetVar (dist:float[]) (rect:Rect3D) : Rect3D =
        if dist.Length <> 4 then fail $"Rect3D.offsetVar: the distance array must have 4 elements, but has {dist.Length}"
        let xl = rect.SizeX
        let yl = rect.SizeY
        if xl < dist.[1]+dist.[3] ||yl < dist.[0]+dist.[2] then
            fail $"Rect3D.offsetVar: the 3D-rectangle {rect.AsString} is too small to offset by distances [|{Format.float dist.[0]};{Format.float dist.[1]};{Format.float dist.[2]};{Format.float dist.[3]}|]"
        let x0f = dist.[3] / xl
        let x1f = dist.[1] / xl
        let y0f = dist.[0] / yl
        let y1f = dist.[2] / yl
        let x0X = rect.XaxisX * x0f
        let x0Y = rect.XaxisY * x0f
        let x0Z = rect.XaxisZ * x0f
        let x1X = rect.XaxisX * x1f
        let x1Y = rect.XaxisY * x1f
        let x1Z = rect.XaxisZ * x1f
        let y0X = rect.YaxisX * y0f
        let y0Y = rect.YaxisY * y0f
        let y0Z = rect.YaxisZ * y0f
        let y1X = rect.YaxisX * y1f
        let y1Y = rect.YaxisY * y1f
        let y1Z = rect.YaxisZ * y1f
        Rect3D.createUnchecked(
            rect.OriginX + x0X + y0X,
            rect.OriginY + x0Y + y0Y,
            rect.OriginZ + x0Z + y0Z,
            rect.XaxisX - x0X - x1X,
            rect.XaxisY - x0Y - x1Y,
            rect.XaxisZ - x0Z - x1Z,
            rect.YaxisX - y0X - y1X,
            rect.YaxisY - y0Y - y1Y,
            rect.YaxisZ - y0Z - y1Z)

    ///<summary>Offsets a local Rect3D at one of the four corners.</summary>
    ///<param name="rect">The 3D-rectangle</param>
    ///<param name="corner">The Index of the corner to Offset
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///  0-Origin       1
    /// </code></param>
    ///<param name="xOffset">The local offset distances in x direction. (Applies to the y side.) Positive values offset to the inside of the rectangle, negative values will offset outwards.</param>
    ///<param name="yOffset">The local offset distances in y direction. (Applies to the x side.) Positive values offset to the inside of the rectangle, negative values will offset outwards.</param>
    ///<param name="xWidth">The width (or size in x direction) that will be added to the current offset.</param>
    ///<param name="yHeight">The height (or size in y direction) that will be added to the current offset.</param>
    ///<returns>A new 3D-rectangle. It will always have the same x and y axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetCorner (rect:Rect3D, corner:int, xOffset:float, yOffset:float, xWidth:float, yHeight:float) : Rect3D =
        let xl = rect.SizeX
        let yl = rect.SizeY
        if isTooTiny xl then failTooSmall "Rect3D.offsetCorner: Xaxis" rect
        if isTooTiny yl then failTooSmall "Rect3D.offsetCorner: Yaxis" rect
        let xvf = xWidth/xl
        let yvf = yHeight/yl
        let xvX = rect.XaxisX * xvf
        let xvY = rect.XaxisY * xvf
        let xvZ = rect.XaxisZ * xvf
        let yvX = rect.YaxisX * yvf
        let yvY = rect.YaxisY * yvf
        let yvZ = rect.YaxisZ * yvf
        let inline create xDist yDist =
            let xf = xDist / xl
            let yf = yDist / yl
            Rect3D.createUnchecked(
                rect.OriginX + rect.XaxisX*xf + rect.YaxisX*yf,
                rect.OriginY + rect.XaxisY*xf + rect.YaxisY*yf,
                rect.OriginZ + rect.XaxisZ*xf + rect.YaxisZ*yf,
                xvX, xvY, xvZ,
                yvX, yvY, yvZ)
        match corner with
        | 0 ->
            create xOffset yOffset
        | 1 ->
            create (xl-xOffset-xWidth) yOffset
        | 2 ->
            create (xl-xOffset-xWidth) (yl-yOffset-yHeight)
        | 3 ->
            create xOffset (yl-yOffset-yHeight)
        | _ ->
            fail $"Rect3D.offsetCorner: corner {corner} out of range 0..3" |> unbox // unbox to make type checker happy

    ///<summary>Offsets a local Rect3D at one of the four edges.</summary>
    ///<param name="rect">The 3D-rectangle</param>
    ///<param name="edgeIdx">The Index of the edge to offset
    /// <code>
    ///   local
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |      2
    ///   +------------+
    ///   |            |
    ///   |            |
    ///  3|            |1
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis
    ///         0
    /// </code></param>
    ///<param name="offEdge">The local offset distances parallel to the edge.</param>
    ///<param name="width">The width of the new rectangle. This is like the second offset to be applied to the first offset of offEdge</param>
    ///<param name="offStart">The local offset distances perpendicular to the edge at the start.</param>
    ///<param name="offEnd">The local offset distances perpendicular to the edge at the end.</param>
    ///<returns>A new 3D-rectangle. It will always have the same x and y axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetEdge (rect:Rect3D, edgeIdx:int, offEdge:float, width:float, offStart:float, offEnd:float) : Rect3D =
        let lx = rect.SizeX
        let ly = rect.SizeY

        let inline checkX d : float = if d > 1e-6 then d else failRect3DOffsetEdge(offStart, offEnd, lx, edgeIdx, d)  //|> unbox // unbox to make type checker happy
        let inline checkY d : float = if d > 1e-6 then d else failRect3DOffsetEdge(offStart, offEnd, ly, edgeIdx, d)  //|> unbox // unbox to make type checker happy

        let inline create xx yy xDist yDist =
            let xxx = xx / lx
            let yyy = yy / ly
            let xxDist = xDist / lx
            let yyDist = yDist / ly
            Rect3D.createUnchecked(
                rect.OriginX + rect.XaxisX * xxx + rect.YaxisX * yyy,
                rect.OriginY + rect.XaxisY * xxx + rect.YaxisY * yyy,
                rect.OriginZ + rect.XaxisZ * xxx + rect.YaxisZ * yyy,
                rect.XaxisX * xxDist,
                rect.XaxisY * xxDist,
                rect.XaxisZ * xxDist,
                rect.YaxisX * yyDist,
                rect.YaxisY * yyDist,
                rect.YaxisZ * yyDist)

        if isTooTiny (lx) || isTooTiny (ly) then
            failTooSmall "Rect3D.offsetEdge: Xaxis or Yaxis" rect
        if width > 1e-6 then
            match edgeIdx with
            | 0 ->
                let x = lx-offStart-offEnd |> checkX
                create offStart offEdge x width

            | 1 ->
                let y = ly-offStart-offEnd |> checkY
                create (lx-offEdge-width) offStart width y

            | 2 ->
                let x = lx-offStart-offEnd |> checkX
                create offEnd (ly-offEdge-width) x width

            | 3 ->
                let y = ly-offStart-offEnd |> checkY
                create offEdge offEnd width y

            | _ ->
                fail $"Rect3D.offsetEdge: edgeIdx {edgeIdx} out of range 0..3" |> unbox // unbox to make type checker happy

        elif width < -1e-6 then // the rect origin needs to be at the other corner
            match edgeIdx with
            | 0 ->
                let x = lx-offStart-offEnd |> checkX
                create offStart (offEdge+width) x (-width)

            | 1 ->
                let y = ly-offStart-offEnd |> checkY
                create (lx-offEdge) offStart (-width) y

            | 2 ->
                let x = lx-offStart-offEnd |> checkX
                create offEnd (ly-offEdge) x (-width)

            | 3 ->
                let y = ly-offStart-offEnd |> checkY
                create (offEdge+width) offEnd (-width) y

            | _ ->
                fail $"Rect3D.offsetEdge: edgeIdx {edgeIdx} out of range 0..3" |> unbox // unbox to make type checker happy
        else
            fail $"Rect3D.offsetEdge: width {width} must be more than 1e-6" |> unbox // unbox to make type checker happy

    /// Divides a 3D-rectangle into a grid of sub-rectangles. The sub-rectangles are returned as an array of arrays.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D-rectangle.
    /// The returned array has xCount elements, each element is an array of yCount sub-rectangles.
    static member subDivide (rect:Rect3D, xCount:int, yCount:int, xGap:float, yGap:float) : Rect3D array array =
        if xCount <= 0 ||yCount <= 0 then
            fail $"Rect3D.subDivide: xCount {xCount} and yCount {yCount} must be 1 or more"
        let xl = rect.SizeX
        let yl = rect.SizeY
        let lx1 = (xl - xGap * float (xCount-1) )/ float xCount
        let ly1 = (yl - yGap * float (yCount-1) )/ float yCount
        if isTooTiny (lx1) || isTooTiny (ly1) then
            [||]
        else
            let vxf = lx1/xl
            let vyf = ly1/yl
            let vxX = rect.XaxisX * vxf
            let vxY = rect.XaxisY * vxf
            let vxZ = rect.XaxisZ * vxf
            let vyX = rect.YaxisX * vyf
            let vyY = rect.YaxisY * vyf
            let vyZ = rect.YaxisZ * vyf
            let rss = Array.zeroCreate xCount
            for ix = 0 to xCount-1 do
                let rs = Array.zeroCreate yCount
                for iy = 0 to yCount-1 do
                    let xf = (xGap * float ix + lx1 * float ix) / xl
                    let yf = (yGap * float iy + ly1 * float iy) / yl
                    rs.[iy] <- Rect3D.createUnchecked(
                                rect.OriginX + rect.XaxisX*xf + rect.YaxisX*yf,
                                rect.OriginY + rect.XaxisY*xf + rect.YaxisY*yf,
                                rect.OriginZ + rect.XaxisZ*xf + rect.YaxisZ*yf,
                                vxX, vxY, vxZ,
                                vyX, vyY, vyZ)
                rss.[ix] <- rs
            rss

    /// Divides a a 3D-rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D-rectangle.
    /// It will create as many sub-rectangles as possible, respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.9999 to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member subDivideMinLength (rect:Rect3D, xMinLen:float, yMinLen:float, xGap:float, yGap:float) : Rect3D array array =
        let xLen = rect.SizeX
        let yLen = rect.SizeY
        if xLen < xMinLen then
            fail $"Rect3D.subDivideMinLength: xMinLen {xMinLen} is bigger than rect X-axis length {xLen} for {rect.AsString}"
        if yLen < yMinLen then
            fail $"Rect3D.subDivideMinLength: yMinLen {yMinLen} is bigger than rect Y-axis length {yLen} for {rect.AsString}"
        let xCount = int (xLen / (xMinLen*0.9999))
        let yCount = int (yLen / (yMinLen*0.9999))
        Rect3D.subDivide (rect, xCount, yCount, xGap, yGap)

    /// Divides a a 3D-rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D-rectangle.
    /// It will create as few segments as possible respecting the maximum segment length.
    /// The input maxSegmentLength is multiplied by factor 1.00001 to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member subDivideMaxLength (rect:Rect3D, xMaxLen:float, yMaxLen:float, xGap:float, yGap:float) : Rect3D array array =
        let xLen = rect.SizeX
        let yLen = rect.SizeY
        let xCount = 1 + int (xLen / (xMaxLen*1.00001))
        let yCount = 1 + int (yLen / (yMaxLen*1.00001))
        Rect3D.subDivide (rect, xCount, yCount, xGap, yGap)

    /// Divides a 3D-rectangle into a grid of points. The points are returned as an array of arrays.
    /// A xCount and yCount of 2 will only return the 4 corners of the rectangle.
    static member grid (rect:Rect3D, xCount:int, yCount:int) : Pnt[][]=
        if xCount <= 1 ||yCount <= 1 then
            fail $"Rect3D.grid: xCount {xCount} and yCount {yCount} must be 2 or more"
        let xl = rect.SizeX
        let yl = rect.SizeY
        let lx1 = xl / float (xCount-1)
        let ly1 = yl / float (yCount-1)
        let rss = Array.zeroCreate xCount
        for ix = 0 to xCount-1 do
            let rs = Array.zeroCreate yCount
            for iy = 0 to yCount-1 do
                let xf = lx1 * float ix / xl
                let yf = ly1 * float iy / yl
                rs.[iy] <- Pnt(
                            rect.OriginX + rect.XaxisX*xf + rect.YaxisX*yf,
                            rect.OriginY + rect.XaxisY*xf + rect.YaxisY*yf,
                            rect.OriginZ + rect.XaxisZ*xf + rect.YaxisZ*yf)
            rss.[ix] <- rs
        rss

    /// Divides a a 3D-rectangle into a grid of points.
    /// It will create as many points as possible respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member gridMinLength (rect:Rect3D, xMinLen:float, yMinLen:float) : Pnt array array =
        let xLen = rect.SizeX
        let yLen = rect.SizeY
        if xLen < xMinLen  then fail $"Rect3D.gridMinLength: xMinLen {xMinLen} is bigger than rect X-axis length {xLen} for {rect.AsString}"
        if yLen < yMinLen  then fail $"Rect3D.gridMinLength: yMinLen {yMinLen} is bigger than rect Y-axis length {yLen} for {rect.AsString}"
        let xCount = 1 + int (xLen / (xMinLen*0.9999))
        let yCount = 1 + int (yLen / (yMinLen*0.9999))
        Rect3D.grid (rect, xCount, yCount)

    /// Divides a a 3D-rectangle into a grid of points.
    /// It will create as few as points as possible respecting the maximum segment length.
    /// The input maxSegmentLength is multiplied by factor 1.00001 to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member gridMaxLength (rect:Rect3D, xMaxLen:float, yMaxLen:float) : Pnt array array =
        let xLen = rect.SizeX
        let yLen = rect.SizeY
        let xCount = 2 + int (xLen / (xMaxLen*1.00001))
        let yCount = 2 + int (yLen / (yMaxLen*1.00001))
        Rect3D.grid (rect, xCount, yCount)


    /// Returns the line parameter.
    /// The parameter is the intersection point of the ray with the infinitely extended Rect3D.
    /// The intersection point lies on the line segment itself only if the parameter is within the range 0.0 to 1.0.
    /// Returns ValueNone if they are parallel or coincident.
    static member intersectRay (ln:Line3D) (r:Rect3D) : voption<float> =
        // (1) get the unit normal of the rectangle
        let mutable nx = r.XaxisY * r.YaxisZ - r.XaxisZ * r.YaxisY
        let mutable ny = r.XaxisZ * r.YaxisX - r.XaxisX * r.YaxisZ
        let mutable nz = r.XaxisX * r.YaxisY - r.XaxisY * r.YaxisX
        let len = sqrt (nx*nx + ny*ny + nz*nz)
        if isTooTiny len then
            failTooSmall "Rect3D.intersectRayParameters: rect" r
        let f = 1.0 / len
        nx <- nx * f
        ny <- ny * f
        nz <- nz * f
        // (2) get the tangent vector of the line
        let vx = ln.VectorX
        let vy = ln.VectorY
        let vz = ln.VectorZ
        // (3) dot product of tangent and normal
        let nenner = vx * nx + vy * ny + vz * nz
        if isTooSmall (abs nenner) then
            ValueNone
        else
            let ox = r.OriginX
            let oy = r.OriginY
            let oz = r.OriginZ
            // (4) get the vector from the line start to the rectangle origin
            let lnOrX = ox - ln.FromX
            let lnOrY = oy - ln.FromY
            let lnOrZ = oz - ln.FromZ
            // (5) get the line parameter of the intersection point
            let t = (lnOrX * nx + lnOrY * ny + lnOrZ * nz) / nenner
            ValueSome t


    /// Returns intersection point of a Line3D with Rect3D.
    /// Returns None if the intersection point is outside of their bounds.
    /// Use Rect3D.intersectRay to get the parameters of the intersection point.
    /// Returns None if they are parallel or coincident or the line has zero length.
    /// If the intersection parameters on the line and the rectangle are within 1e-6 of the bounds 0.0 or 1.0,
    /// it will still be considered as inside the rectangle and line.
    static member intersectLine (ln:Line3D) (r:Rect3D) : voption<Pnt> =
        match Rect3D.intersectRay ln r with
        | ValueNone -> ValueNone
        | ValueSome t ->
            if UtilEuclid.isBetweenZeroAndOneTolerantIncl t then
                // (6) get the intersection point
                let xptX = ln.FromX + ln.VectorX * t
                let xptY = ln.FromY + ln.VectorY * t
                let xptZ = ln.FromZ + ln.VectorZ * t
                // (7) get the vector from the rectangle origin to the intersection point
                let vecInPlaneX = xptX - r.OriginX
                let vecInPlaneY = xptY - r.OriginY
                let vecInPlaneZ = xptZ - r.OriginZ
                // (8) get the X and Y parameters of the intersection point
                let tx = (r.XaxisX*vecInPlaneX + r.XaxisY*vecInPlaneY + r.XaxisZ*vecInPlaneZ) / (r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY + r.XaxisZ*r.XaxisZ)
                if UtilEuclid.isBetweenZeroAndOneTolerantIncl tx then
                    let ty = (r.YaxisX*vecInPlaneX + r.YaxisY*vecInPlaneY + r.YaxisZ*vecInPlaneZ) / (r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY + r.YaxisZ*r.YaxisZ)
                    if UtilEuclid.isBetweenZeroAndOneTolerantIncl ty then
                        ValueSome <| Pnt(xptX, xptY, xptZ)
                    else
                        ValueNone
                else
                    ValueNone
            else
                ValueNone

    // #endregion
    // #region Obsolete

    [<Obsolete("Renamed to Rect3D.xAxisUnit for naming consistency with Rect2D.")>]
    static member xaxisUnit (r:Rect3D) : UnitVec =
        r.XaxisUnit

    [<Obsolete("Renamed to Rect3D.yAxisUnit for naming consistency with Rect2D.")>]
    static member yaxisUnit (r:Rect3D) : UnitVec =
        r.YaxisUnit

    [<Obsolete("This does not scale proportionally to the actual area, use just .Area for sorting by area")>]
    member inline r.AreaSq : float =
        (r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY + r.XaxisZ*r.XaxisZ) * (r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY + r.YaxisZ*r.YaxisZ)


    /// The size in Y direction, same as member rect.SizeY.
    [<Obsolete("use SizeY")>]
    member  r.Height2D : float =
        r.SizeY

    /// The size in X direction, same as member rect.SizeX.
    [<Obsolete("use SizeX")>]
    member  r.Width : float =
        r.SizeX



    /// return minX, minY, minZ, maxX, maxY, maxZ
    [<Obsolete("Use BBox.createFromRect3D instead.")>]
    member r.BBox : float * float * float * float * float * float =
        let p0x = r.OriginX
        let p0y = r.OriginY
        let p0z = r.OriginZ
        let p1x = p0x + r.XaxisX
        let p1y = p0y + r.XaxisY
        let p1z = p0z + r.XaxisZ
        let p2x = p1x + r.YaxisX
        let p2y = p1y + r.YaxisY
        let p2z = p1z + r.YaxisZ
        let p3x = p0x + r.YaxisX
        let p3y = p0y + r.YaxisY
        let p3z = p0z + r.YaxisZ
        let minX = min (min (min p0x p1x) p2x) p3x
        let minY = min (min (min p0y p1y) p2y) p3y
        let minZ = min (min (min p0z p1z) p2z) p3z
        let maxX = max (max (max p0x p1x) p2x) p3x
        let maxY = max (max (max p0y p1y) p2y) p3y
        let maxZ = max (max (max p0z p1z) p2z) p3z
        (minX, minY, minZ, maxX, maxY, maxZ)

    /// return minX, minY, maxX, maxY
    [<Obsolete("Use BRect.createFromRect3D instead.")>]
    member r.BRect : float * float * float * float  =
        let p0x = r.OriginX
        let p0y = r.OriginY
        let p0z = r.OriginZ
        let p1x = p0x + r.XaxisX
        let p1y = p0y + r.XaxisY
        let p1z = p0z + r.XaxisZ
        let p2x = p1x + r.YaxisX
        let p2y = p1y + r.YaxisY
        let p2z = p1z + r.YaxisZ
        let p3x = p0x + r.YaxisX
        let p3y = p0y + r.YaxisY
        let p3z = p0z + r.YaxisZ
        let minX = min (min (min p0x p1x) p2x) p3x
        let minY = min (min (min p0y p1y) p2y) p3y
        let minZ = min (min (min p0z p1z) p2z) p3z
        let maxX = max (max (max p0x p1x) p2x) p3x
        (minX, minY, minZ, maxX)

    [<Obsolete("Use Rect3D.createFromBounds2D instead.")>]
    static member createFromBRect (minX, minY, maxX, maxY)  : Rect3D =
        Rect3D.createUnchecked(minX, minY, 0.0, maxX - minX, 0.0, 0.0, 0.0, maxY - minY, 0.0)

    #nowarn "44" // Obsolete member

    /// return minX, minY, minZ, maxX, maxY, maxZ
    [<Obsolete("Use BBox.createFromRect3D instead.")>]
    static member inline bBox (r:Rect3D) : float * float * float * float * float * float =
        r.BBox
