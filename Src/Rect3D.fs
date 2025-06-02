namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open System.Collections.Generic

/// An immutable planar 3D-rectangle with any rotation in 3D space.
/// Described by an Origin and two Edge vectors.
/// Similar to PPlane, however the two vectors are not unitized.
/// This implementation guarantees the 3D-rectangle to be always valid.
/// That means the  X and Y axes are always perpendicular to each other.
/// However the length of one of these axes might still be zero.
///
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
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Rect3D =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The Origin Corner of the 3D-rectangle.
    [<DataMember>] val Origin: Pnt

    /// The edge vector representing the X-axis of the 3D-rectangle.
    /// Also called Length.
    [<DataMember>] val Xaxis: Vec

    /// The edge vector representing the Y-axis of the 3D-rectangle.
    [<DataMember>] val Yaxis: Vec

    /// Unchecked Internal Constructor Only.
    /// Creates a 3D rectangle with X, Y Direction.
    internal new (origin:Pnt, axisX:Vec, axisY:Vec) =
        #if DEBUG
        let lenX = axisX.Length
        let lenY = axisY.Length
        if isTooSmall (lenX) then  EuclidException.Raisef "Euclid.Rect3D(): X-axis is too short: %O" axisX
        if isTooSmall (lenY) then  EuclidException.Raisef "Euclid.Rect3D(): Y-axis is too short: %O" axisY
        //just using zeroLengthTolerance 1e-12 seems too strict for dot product check:
        if abs (axisX *** axisY) > (lenX+lenY) * 1e-9 then EuclidException.Raise $"Euclid.Rect3D(): X-axis and Y-axis are not perpendicular{Format.nl}(dot={(abs(axisX***axisY))}) > 1e-9 {Format.nl}{axisX} and {Format.nl}{axisY}"
        #endif
        {Origin=origin; Xaxis=axisX; Yaxis=axisY}

    /// The size in X direction, same as member rect.SizeX.
    [<Obsolete("use SizeX")>]
    member inline r.Width = r.Xaxis.Length

    /// The size in X direction
    member inline r.SizeX = r.Xaxis.Length

    /// The squared size in X direction
    member inline r.SizeXSq = r.Xaxis.LengthSq

    /// The size in Y direction, same as member rect.SizeY.
    [<Obsolete("use SizeY")>]
    member inline r.Height2D = r.Yaxis.Length

    /// The size in Y direction
    member inline r.SizeY = r.Yaxis.Length

    /// The squared size in Y direction
    member inline r.SizeYSq = r.Yaxis.LengthSq

    /// Nicely formatted string representation of the 3D-rectangle including its size.
    override r.ToString() =
        sprintf "Euclid.Rect3D %s x %s (Origin:%s| X-ax:%s| Y-ax:%s)"
           (Format.float r.SizeX) (Format.float r.SizeY)
            r.Origin.AsString r.Xaxis.AsString r.Yaxis.AsString


    /// Format the 3D-rectangle into string with nice floating point number formatting of X and Y size only.
    /// But without type name as in v.ToString()
    member r.AsString = sprintf "%s x %s" (Format.float r.SizeX) (Format.float r.SizeY)


    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit : UnitVec =
        let v = r.Xaxis
        let x = v.X
        let y = v.Y
        let z = v.Z
        let len = sqrt (x*x + y*y + z*z)
        if isTooTiny len then EuclidException.Raisef "Euclid.Rect3D.XaxisUnit: rect Xaxis is too small for unitizing: %s" r.AsString
        UnitVec.createUnchecked (x/len, y/len, z/len)

    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit : UnitVec =
        let v = r.Yaxis
        let x = v.X
        let y = v.Y
        let z = v.Z
        let len = sqrt (x*x + y*y + z*z)
        if isTooTiny len then EuclidException.Raisef "Euclid.Rect3D.YaxisUnit: rect Yaxis is too small for unitizing: %s" r.AsString
        UnitVec.createUnchecked (x/len, y/len, z/len)

    /// Returns the Normal
    /// Resulting from the Cross Product of r.Xaxis with r.Yaxis.
    member inline r.Normal = Vec.cross(r.Xaxis, r.Yaxis)

    /// Returns the unitized Normal.
    /// Resulting from the Cross Product of r.Xaxis with r.Yaxis.
    member r.NormalUnit : UnitVec =
        let a = r.Xaxis
        let b = r.Yaxis
        // cross product:
        let x = a.Y * b.Z - a.Z * b.Y
        let y = a.Z * b.X - a.X * b.Z
        let z = a.X * b.Y - a.Y * b.X
        let len = sqrt (x*x + y*y + z*z)
        if isTooTiny len then EuclidException.Raisef "Euclid.Rect3D.NormalUnit: rect is too small for finding a normal vector: %s" r.AsString
        UnitVec.createUnchecked (x/len, y/len, z/len)

    /// Returns the diagonal vector of the 3D-rectangle.
    /// From Origin to FarCorner.
    member inline r.Diagonal = r.Xaxis + r.Yaxis

    /// Returns the center of the 3D-rectangle.
    member inline r.Center = r.Origin + r.Xaxis*0.5 + r.Yaxis*0.5


    /// Evaluate a X and Y parameter of the 3D-rectangle.
    ///  0.0, 0.0 returns the Origin.
    ///  1.0, 1.0 returns the FarCorner.
    member inline r.EvaluateAt (xParameter:float, yParameter:float) =
        r.Origin + r.Xaxis * xParameter + r.Yaxis * yParameter

    /// Evaluate a point at X and Y distance on the respective axes of the 2D Rectangle.
    member inline r.EvaluateDist (xDistance:float, yDistance:float) =
        let lx = r.Xaxis.Length
        let ly = r.Yaxis.Length
        if isTooTiny (lx) || isTooTiny (ly) then
            EuclidException.Raisef "Euclid.Rect3D.EvaluateDist: rect Xaxis or Yaxis is too small for evaluating distance: %s" r.AsString
        r.Origin + r.Xaxis * (xDistance/lx) + r.Yaxis * (yDistance/ly)

    /// Calculates the area of the 3D-rectangle.
    member inline r.Area =
        r.Xaxis.Length*r.Yaxis.Length

    /// Calculates the squared area of the 3D-rectangle.
    /// by using the squared lengths of the X and Y axis.
    /// This is a bit faster than calculating the area and good enough for relative comparisons or sorting by size.
    /// r.Xaxis.LengthSq * r.Yaxis.LengthSq
    [<Obsolete("this does not scale proportionally, use .Area")>]
    member inline r.AreaSq =
        r.Xaxis.LengthSq * r.Yaxis.LengthSq


    /// Returns the longest edge of the Rect3D.
    member inline b.LongestEdge =
        let x = b.Xaxis.LengthSq
        let y = b.Yaxis.LengthSq
        sqrt  (max x y)

    /// Returns the shortest edge of the Rect3D.
    member inline b.ShortestEdge =
        let x = b.Xaxis.LengthSq
        let y = b.Yaxis.LengthSq
        sqrt  (min x y)

    /// Returns the square length of longest edge of the Rect3D.
    member inline b.LongestEdgeSq =
        let x = b.Xaxis.LengthSq
        let y = b.Yaxis.LengthSq
        max x y

    /// Returns the square length of shortest edge of the Rect3D.
    member inline b.ShortestEdgeSq =
        let x = b.Xaxis.LengthSq
        let y = b.Yaxis.LengthSq
        min x y

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsPoint.
    member inline b.IsZero =
        isTooTinySq b.Xaxis.LengthSq &&
        isTooTinySq b.Yaxis.LengthSq

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsZero.
    member inline b.IsPoint =
        isTooTinySq b.Xaxis.LengthSq &&
        isTooTinySq b.Yaxis.LengthSq

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, 2 .
    member inline b.CountZeroSides =
        countTooTinySq    b.Xaxis.LengthSq
        +  countTooTinySq b.Yaxis.LengthSq

    /// Tests if two of the X and Y axis is smaller than the zeroLength tolerance.
    member inline b.IsLine =
        b.CountZeroSides = 1

    /// Tests if no sides of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .HasVolume
    member inline b.IsValid =
        b.CountZeroSides = 0

    /// Tests if none of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasVolume =
        b.CountZeroSides = 0

    /// Gets the Plane that this 3D-rectangle is based on.
    member inline r.Plane =
        let x = r.Xaxis.Unitized
        let y = r.Yaxis.Unitized
        let z = UnitVec.cross(x, y).Unitized
        PPlane.createUnchecked (r.Origin, x, y, z)


    /// Gets the axis aligned 2D Bounding Rectangle of the 3D-rectangle.
    /// The z-coordinate components are ignored.
    member r.BRect =
        let x = r.Xaxis
        let y = r.Yaxis
        let p0 = r.Origin
        let p1 = p0 + x
        let p2 = p1 + y
        let p3 = p0 + y
        let minX = min (min (min p0.X p1.X) p2.X) p3.X
        let minY = min (min (min p0.Y p1.Y) p2.Y) p3.Y
        let maxX = max (max (max p0.X p1.X) p2.X) p3.X
        let maxY = max (max (max p0.Y p1.Y) p2.Y) p3.Y
        BRect.createUnchecked(minX, minY, maxX, maxY)

    /// Gets the axis aligned 3D Bounding Box of the 3D-rectangle.
    member r.BBox =
        let x = r.Xaxis
        let y = r.Yaxis
        let p0 = r.Origin
        let p1 = p0 + x
        let p2 = p1 + y
        let p3 = p0 + y
        let minX = min (min (min p0.X p1.X) p2.X) p3.X
        let minY = min (min (min p0.Y p1.Y) p2.Y) p3.Y
        let minZ = min (min (min p0.Z p1.Z) p2.Z) p3.Z
        let maxX = max (max (max p0.X p1.X) p2.X) p3.X
        let maxY = max (max (max p0.Y p1.Y) p2.Y) p3.Y
        let maxZ = max (max (max p0.Z p1.Z) p2.Z) p3.Z
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)


    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Checks if two 3D-rectangles are equal within tolerance.
    /// Does not recognize congruent rectangles with different rotation as equal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:Rect3D) (b:Rect3D) =
        abs (a.Origin.X - b.Origin.X) <= tol &&
        abs (a.Origin.Y - b.Origin.Y) <= tol &&
        abs (a.Origin.Z - b.Origin.Z) <= tol &&
        abs (a.Xaxis.X  - b.Xaxis.X ) <= tol &&
        abs (a.Xaxis.Y  - b.Xaxis.Y ) <= tol &&
        abs (a.Xaxis.Z  - b.Xaxis.Z ) <= tol &&
        abs (a.Yaxis.X  - b.Yaxis.X ) <= tol &&
        abs (a.Yaxis.Y  - b.Yaxis.Y ) <= tol &&
        abs (a.Yaxis.Z  - b.Yaxis.Z ) <= tol

    /// Check if two 3D-rectangles are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two rectangles are not exactly equal.
    static member notEquals (tol:float) (a:Rect3D) (b:Rect3D) =
        abs (a.Origin.X - b.Origin.X) > tol ||
        abs (a.Origin.Y - b.Origin.Y) > tol ||
        abs (a.Origin.Z - b.Origin.Z) > tol ||
        abs (a.Xaxis.X  - b.Xaxis.X ) > tol ||
        abs (a.Xaxis.Y  - b.Xaxis.Y ) > tol ||
        abs (a.Xaxis.Z  - b.Xaxis.Z ) > tol ||
        abs (a.Yaxis.X  - b.Yaxis.X ) > tol ||
        abs (a.Yaxis.Y  - b.Yaxis.Y ) > tol ||
        abs (a.Yaxis.Z  - b.Yaxis.Z ) > tol


    /// Returns the 3D-rectangle expanded by distance on all four sides.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (r:Rect3D) =
        let siX = r.SizeX
        let siY = r.SizeY
        let d = dist * -2.0
        if siX<=d || siY<=d  then
            EuclidException.Raisef "Euclid.Rect3D.expand: the 3D-rectangle %s is too small to expand by negative distance %s"  r.AsString (Format.float dist)
        let x = r.Xaxis * (dist / siX)
        let y = r.Yaxis * (dist / siY)
        Rect3D(r.Origin-x-y, r.Xaxis+x*2., r.Yaxis+y*2.)

    /// Returns the 3D-rectangle expanded by respective distances on all four sides.
    /// Does check for overflow if distance is negative and fails.
    /// distX, distY are for the local X and Y-axis respectively.
    static member expandXY distX distY (r:Rect3D) =
        let siX = r.SizeX
        let siY = r.SizeY
        if siX <= distX * -2.0 then EuclidException.Raisef "Euclid.Rect3D.expandXY: the 3D-rectangle %s is too small to expand by negative distance distX %s"  r.AsString (Format.float distX)
        if siY <= distY * -2.0 then EuclidException.Raisef "Euclid.Rect3D.expandXY: the 3D-rectangle %s is too small to expand by negative distance distY %s"  r.AsString (Format.float distY)
        let x = r.Xaxis * (distX / r.SizeX)
        let y = r.Yaxis * (distY / r.SizeY)
        Rect3D(r.Origin-x-y, r.Xaxis+x*2., r.Yaxis+y*2.)



    /// Returns the 3D-rectangle expanded by a relative factor on all four sides.
    /// Values between 0.0 and 1.0 shrink the rectangle.
    /// Values larger than 1.0 expand the rectangle.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRel factor (r:Rect3D) =
        if factor < 0.0  then
            EuclidException.Raise $"Euclid.Rect3D.expandRel: a negative factor {factor} is not allowed for expanding the 3D-rectangle {r.AsString}"
        let x = r.Xaxis * factor
        let y = r.Yaxis * factor
        Rect3D(r.Center - x*0.5 - y*0.5, x, y)


    /// Returns the 3D-rectangle expanded by a relative factor on all four sides.
    /// Values between 0.0 and 1.0 shrink the rectangle.
    /// Values larger than 1.0 expand the rectangle.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRelXY factorX factorY (r:Rect3D) =
        if factorX < 0.0  then
            EuclidException.Raise $"Euclid.Rect3D.expandRelXY: a negative factor {factorX} is not allowed for expanding the 3D-rectangle {r.AsString}"
        if factorY < 0.0  then
            EuclidException.Raise $"Euclid.Rect3D.expandRelXY: a negative factor {factorY} is not allowed for expanding the 3D-rectangle {r.AsString}"
        let x = r.Xaxis * factorX
        let y = r.Yaxis * factorY
        Rect3D(r.Center - x*0.5 - y*0.5, x, y)



    /// Create a 3D-rectangle from the origin point, an x-edge and an y-edge.
    /// Fails if x and y are not perpendicularity.
    /// Fails on vectors shorter than 1e-9.
    static member createFromVectors(origin, x:Vec, y:Vec) =
        if isTooSmallSq(x.LengthSq)then EuclidException.Raise $"Euclid.Rect3D.createFromVectors(origin, x:Vec, y:Vec): X-axis is too short:{Format.nl}{y}"
        if isTooSmallSq(y.LengthSq) then EuclidException.Raise $"Euclid.Rect3D.createFromVectors(origin, x:Vec, y:Vec): Y-axis is too short:{Format.nl}{y}"
        //zeroLengthTolerance seems too strict for dot product:
        if abs (x *** y) > 1e-10 then EuclidException.Raise $"Euclid.Rect3D.createFromVectors(origin, x:Vec, y:Vec): X-axis and Y-axis are not perpendicular (dot={(abs(x***y))}): {Format.nl}{x} and{Format.nl}{y}"
        Rect3D(origin, x, y)

    /// Give PPlane and sizes.
    /// The Rect3D's Origin will be at the plane's Origin.
    /// Fails on negative sizes.
    static member createFromPlane (pl:PPlane, sizeX:float, sizeY:float) =
        if isNegative(sizeX) then EuclidException.Raisef "Euclid.Rect3D.createFromPlane sizeX is negative: %g, sizeY is: %g, plane: %O"  sizeX sizeY  pl
        if isNegative(sizeY) then EuclidException.Raisef "Euclid.Rect3D.createFromPlane sizeY is negative: %g, sizeX is: %g, plane: %O"  sizeY sizeX  pl
        Rect3D(pl.Origin, pl.Xaxis*sizeX, pl.Yaxis*sizeY)

    /// Give PPlane and sizes.
    /// The Rect3D's Center will be at the plane's Origin.
    /// Fails on negative sizes.
    static member createCenteredFromPlane (pl:PPlane, sizeX:float, sizeY:float) =
        if isNegative(sizeX) then EuclidException.Raisef "Euclid.Rect3D.createCenteredFromPlane sizeX is negative: %g, sizeY is: %g, plane: %O"  sizeX sizeY  pl
        if isNegative(sizeY) then EuclidException.Raisef "Euclid.Rect3D.createCenteredFromPlane sizeY is negative: %g, sizeX is: %g, plane: %O"  sizeY sizeX  pl
        let x = pl.Xaxis*sizeX
        let y = pl.Yaxis*sizeY
        Rect3D(pl.Origin- x*0.5 - y*0.5, x, y)

    /// Give 2D Bounding Rect.
    static member createFromBRect (b:BRect) =
        Rect3D(b.MinPt.AsPnt, Vec.Xaxis*b.SizeX, Vec.Yaxis*b.SizeY)


    /// Create a 3D-rectangle from the origin point and X-edge and Y edge.
    /// Does not check for perpendicularity.
    static member createUnchecked (origin, x:Vec, y:Vec) =
        Rect3D(origin, x, y)

    /// Creates a 3D-rectangle from three points. Fails if points are too close to each other or all colinear.
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y axis.
    ///
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
    static member createFrom3Points (origin:Pnt, xPt:Pnt, yPt:Pnt) =
        let x = xPt - origin
        if isTooSmallSq x.LengthSq  then EuclidException.Raisef "Euclid.Rect3D.createFrom3Points: X-Point %s too close to origin: %s." origin.AsString x.AsString
        let y = yPt - origin
        if isTooSmallSq y.LengthSq  then EuclidException.Raisef "Euclid.Rect3D.createFrom3Points: Y-Point %s too close to origin: %s." origin.AsString y.AsString
        let z = Vec.cross(x,y)
        if isTooSmallSq z.LengthSq  then EuclidException.Raisef "Euclid.Rect3D.createFrom3Points: Y-Point %s is too close to Xaxis." y.AsString
        let yu = Vec.cross(z, x).Unitized
        let yr = yu * (yu *** y) // get the y point projected on the y axis
        Rect3D(origin, x, yr)



    /// Tries to create a 3D-rectangle from three points. Returns None if points are too close to each other or all colinear..
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y axis.
    ///
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
    static member tryCreateFrom3Points (origin:Pnt, xPt:Pnt, yPt:Pnt) =
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
                    Some <| Rect3D(origin, x, yr)


    /// Returns the projected oriented bounding box of the points.
    /// Adjusts the 3D-rectangle to contain the projections of all given points.
    /// Keeps the same plane and the same X- and Y-axis as the input rectangle.
    /// For a 3D oriented bounding box use the Box.createFromPlaneAndPoints function.
    static member fitToPoints (pts:IList<Pnt>) (refRect:Rect3D) : Rect3D =
        let o = refRect.Origin
        let x = refRect.Xaxis.Unitized
        let y = refRect.Yaxis.Unitized
        let mutable minX = 0.0
        let mutable minY = 0.0
        let mutable maxX = 0.0
        let mutable maxY = 0.0
        for i = 1 to pts.Count-1 do
            let v = pts.[i] - o
            let dotX = v *** x
            minX <- min minX dotX
            maxX <- max maxX dotX
            let dotY = v *** y
            minY <- min minY dotY
            maxY <- max maxY dotY
        let bo =  o + x * minX + y * minY
        let sizeX = maxX - minX
        let sizeY = maxY - minY
        Rect3D.createUnchecked(bo, x * sizeX, y * sizeY)


    /// Returns the Rectangle flipped. Or rotated 180 around its diagonal from point 1 to 3.
    /// Origin will be at point 2, X-axis points down to to point 1, Y-axis points left to point 3.
    ///
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
    static member flip (r:Rect3D) = Rect3D(r.Origin + r.Xaxis + r.Yaxis, -r.Yaxis, -r.Xaxis)

    /// Translate along the local X-axis of the 3D-rectangle.
    static member translateLocalX (distX:float) (r:Rect3D) =
        let x = r.Xaxis
        let len = x.Length
        if isTooTiny len then EuclidException.Raisef "Euclid.Rect3D.translateLocalX: rect.Xaxis is zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + x*(distX/len), x, r.Yaxis)

    /// Translate along the local Y-axis of the 3D-rectangle.
    static member translateLocalY (distY:float) (r:Rect3D) =
        let y = r.Yaxis
        let len = y.Length
        if isTooTiny len then EuclidException.Raisef "Euclid.Rect3D.translateLocalY: rect.Yaxis is zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + y*(distY/len), r.Xaxis, y)

    /// Translate by a 3D vector.(same as Rect3D.move)
    static member translate (v:Vec) (r:Rect3D) =
        Rect3D(r.Origin + v, r.Xaxis, r.Yaxis)

    /// Move the 3D-rectangle by a vector.(same as Rect3D.translate)
    static member move (v:Vec) (r:Rect3D) =
        Rect3D(r.Origin + v, r.Xaxis, r.Yaxis)

    /// Transform the 3D-rectangle by the given RigidMatrix.
    /// The returned 3D-rectangle is guaranteed to have orthogonal vectors.
    static member transform (m:RigidMatrix) (r:Rect3D) =
        let o = Pnt.transformRigid m r.Origin
        let x = Vec.transformRigid m r.Xaxis
        let y = Vec.transformRigid m r.Yaxis
        Rect3D(o, x, y)


    /// Offset or Translate along the local Z-axis.
    /// The local Z-axis is calculated from Cross Product of X and Y-axis of the 3D-rectangle.
    static member offsetZ (offsetDistance :float) (r:Rect3D) =
        let z = Vec.cross(r.Xaxis, r.Yaxis)
        let len = z.Length
        if isTooTiny len then EuclidException.Raisef "Euclid.Rect3D.offsetZ: rect is too small for offsetting zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + z*(offsetDistance/len), r.Xaxis, r.Yaxis)

    /// Offset a Rect3D like a Polyline inwards by a given distance.
    /// Negative distances will offset outwards.
    /// Fails if the distance is larger than half the size of the rectangle.
    static member offset dist (rect:Rect3D) =
        let xl = rect.Xaxis.Length
        let yl = rect.Yaxis.Length
        if xl < dist*2.0 || yl < dist*2.0 then
            EuclidException.Raisef "Euclid.Rect3D.offset: the 2D Rectangle %s is too small to offset by distance %s"  rect.AsString (Format.float dist)
        let x = rect.Xaxis * (dist / xl)
        let y = rect.Yaxis * (dist / yl)
        Rect3D(rect.Origin+x+y, rect.Xaxis - x*2.0, rect.Yaxis - y*2.0)


    /// Offset a Rect3D like a Polyline inwards by four distances.
    /// The distance array is for Edge01, Edge12, Edge23, and Edge30 respectively.
    /// Fails if the distance is larger than half the size of the rectangle.
    ///
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
    static member offsetVar (dist:float[]) (rect:Rect3D) =
        if dist.Length <> 4 then EuclidException.Raisef "Euclid.Rect3D.offsetVar: the distance array must have 4 elements, but has %i" dist.Length
        let xl = rect.Xaxis.Length
        let yl = rect.Yaxis.Length
        if xl < dist.[1]+dist.[3] || yl < dist.[0]+dist.[2] then
            EuclidException.Raisef "Euclid.Rect3D.offsetVar: the 2D Rectangle %s is too small to offset by distances [|%s;%s;%s;%s|]"  rect.AsString (Format.float dist.[0]) (Format.float dist.[1]) (Format.float dist.[2]) (Format.float dist.[3])
        let x0 = rect.Xaxis * (dist.[3] / xl)
        let x1 = rect.Xaxis * (dist.[1] / xl)
        let y0 = rect.Yaxis * (dist.[0] / yl)
        let y1 = rect.Yaxis * (dist.[2] / yl)
        Rect3D(rect.Origin+x0+y0, rect.Xaxis - x0 - x1, rect.Yaxis - y0 - y1)


    ///<summary>Offsets a local Rect3D at one of the four corners.</summary>
    ///<param name="rect">The 3D-rectangle</param>
    ///<param name="corner">The Index of the corner to Offset
    ///
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
    ///  0-Origin       1 </param>
    ///<param name="xOffset">The local offset distances in x direction. (Applies to the y side.) Positive values offset to the inside of the rectangle, negative values will offset outwards.</param>
    ///<param name="yOffset">The local offset distances in y direction. (Applies to the x side.) Positive values offset to the inside of the rectangle, negative values will offset outwards.</param>
    ///<param name="xWidth">The width (or size in x direction) that will be added to the current offset.</param>
    ///<param name="yHeight">The height (or size in y direction) that will be added to the current offset.</param>
    ///<returns>A new 3D-rectangle. It will always have the same x and y axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetCorner (rect:Rect3D, corner:int, xOffset:float, yOffset:float, xWidth:float, yHeight:float) =
        let xa = rect.Xaxis
        let ya = rect.Yaxis
        let xl = xa.Length
        let yl = ya.Length
        if isTooTiny (xl) || isTooTiny (yl) then
            EuclidException.Raisef "Euclid.Rect3D.offsetCorner: the 3D-rectangle %s is too small to offsetCorner"  rect.AsString
        let xv = xa * (xWidth/xl)
        let yv = ya * (yHeight/yl)
        match corner with
        | 0 ->
            let x = xa * xOffset / xl
            let y = ya * yOffset / yl
            Rect3D(rect.Origin + x + y, xv, yv)
        | 1 ->
            let x = xa * (xl-xOffset-xWidth) / xl
            let y = ya * yOffset             / yl
            Rect3D(rect.Origin + x + y, xv, yv)
        | 2 ->
            let x = xa * (xl-xOffset-xWidth)  / xl
            let y = ya * (yl-yOffset-yHeight) / yl
            Rect3D(rect.Origin + x + y, xv, yv)
        | 3 ->
            let x = xa *xOffset              / xl
            let y = ya *(yl-yOffset-yHeight) / yl
            Rect3D(rect.Origin + x + y, xv, yv)
        | _ ->
            EuclidException.Raisef "Euclid.Rect3D.offsetCorner: corner %i out of range 0..3" corner
    static member private failOffsetEdge(offStart, offEnd, len, edgeIdx, d) =
        EuclidException.Raise $"Euclid.Rect3D.offsetEdge: the 3D-rectangle is too small to offsetEdge by {d} at edgeIdx {edgeIdx}. offStart: {offStart}, offEnd: {offEnd}, Length: {len}"

    ///<summary>Offsets a local Rect3D at one of the four edges.</summary>
    ///<param name="rect">The 3D-rectangle</param>
    ///<param name="edgeIdx">The Index of the edge to offset
    ///
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
    ///         0</param>
    ///<param name="offEdge">The local offset distances parallel to the edge.</param>
    ///<param name="width">The width of the new rectangle. This is like the second offset to be applied to the first offset of offEdge</param>
    ///<param name="offStart">The local offset distances perpendicular to the edge at the start.</param>
    ///<param name="offEnd">The local offset distances perpendicular to the edge at the end.</param>
    ///<returns>A new 3D-rectangle. It will always have the same x and y axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetEdge (rect:Rect3D, edgeIdx:int, offEdge:float, width:float, offStart:float, offEnd:float) =
        let x = rect.Xaxis
        let y = rect.Yaxis
        let lx = x.Length
        let ly = y.Length

        let inline xLen d = x * (d / lx)

        let inline yLen d = y * (d / ly)

        let inline checkX d = if d < 1e-6 then Rect3D.failOffsetEdge(offStart, offEnd, lx, edgeIdx, d) else d
        let inline checkY d = if d < 1e-6 then Rect3D.failOffsetEdge(offStart, offEnd, ly, edgeIdx, d) else d

        let inline orig xx yy =
            rect.Origin + xLen xx + yLen yy

        if isTooTiny (lx) || isTooTiny (ly) then
            EuclidException.Raisef "Euclid.Rect3D.offsetEdge: the 2D Rectangle %s is too small to offset"  rect.AsString
        if width > 1e-6 then
            match edgeIdx with
            | 0 ->
                let x = lx-offStart-offEnd |> checkX
                Rect3D( orig offStart offEdge , xLen x , yLen width  )

            | 1 ->
                let y = ly-offStart-offEnd |> checkY
                Rect3D( orig (lx-offEdge-width) offStart , xLen width, yLen y )

            | 2 ->
                let x = lx-offStart-offEnd |> checkX
                Rect3D( orig offEnd (ly-offEdge-width), xLen x, yLen width   )

            | 3 ->
                let y = ly-offStart-offEnd |> checkY
                Rect3D( orig offEdge offEnd, xLen width, yLen y )

            | _ ->
                EuclidException.Raisef "Euclid.Rect3D.offsetEdge: edgeIdx %i out of range 0..3" edgeIdx

        elif width < -1e-6 then // the rect origin needs to be at the other corner
            match edgeIdx with
            | 0 ->
                let x = lx-offStart-offEnd |> checkX
                Rect3D( orig offStart (offEdge+width) , xLen x , yLen -width  )

            | 1 ->
                let y = ly-offStart-offEnd |> checkY
                Rect3D( orig (lx-offEdge) offStart , xLen -width, yLen y )

            | 2 ->
                let x = lx-offStart-offEnd |> checkX
                Rect3D( orig offEnd (ly-offEdge) , xLen x, yLen -width   )

            | 3 ->
                let y = ly-offStart-offEnd |> checkY
                Rect3D( orig (offEdge+width) offEnd, xLen -width, yLen y )

            | _ ->
                EuclidException.Raisef "Euclid.Rect3D.offsetEdge: edgeIdx %i out of range 0..3" edgeIdx
        else
            EuclidException.Raisef "Euclid.Rect3D.offsetEdge: width %g must be more than 1e-6" width


    /// Divides a 3D-rectangle into a grid of sub-rectangles. The sub-rectangles are returned as an array of arrays.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D-rectangle.
    /// The returned array has xCount elements, each element is an array of yCount sub-rectangles.
    static member subDivide (rect:Rect3D, xCount:int, yCount:int, xGap:float, yGap:float)=
        if xCount <= 0 || yCount <= 0 then
            EuclidException.Raisef "Euclid.Rect3D.subDivide: xCount %d and yCount %d must be 1 or more" xCount yCount
        let xa = rect.Xaxis
        let ya = rect.Yaxis
        let xl = xa.Length
        let yl = ya.Length
        let lx1 = (xl - xGap * float (xCount-1) )/ float xCount
        let ly1 = (yl - yGap * float (yCount-1) )/ float yCount
        if isTooTiny (lx1) || isTooTiny (ly1) then
            [||]
        else
            let o = rect.Origin
            let vx = xa * (lx1/xl)
            let vy = ya * (ly1/yl)
            let rss = Array.zeroCreate xCount
            for ix = 0 to xCount-1 do
                let rs = Array.zeroCreate yCount
                for iy = 0 to yCount-1 do
                    let x = xa * (xGap * float ix / xl + lx1 * float ix / xl)
                    let y = ya * (yGap * float iy / yl + ly1 * float iy / yl)
                    rs.[iy] <- Rect3D(o + x + y, vx, vy)
                rss.[ix] <- rs
            rss


    /// Divides a a 3D-rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D-rectangle.
    /// It will create as many sub-rectangles as possible, respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.99999 to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member subDivideMinLength (rect:Rect3D, xMinLen:float, yMinLen:float, xGap:float, yGap:float) =
        let xLen = rect.Xaxis.Length
        let yLen = rect.Yaxis.Length
        if xLen < xMinLen  then EuclidException.Raisef "Euclid.Rect3D.subDivideMinLength: xMinLen %g is bigger than rect X-axis length %g for %O"  xMinLen xLen rect
        if yLen < yMinLen  then EuclidException.Raisef "Euclid.Rect3D.subDivideMinLength: yMinLen %g is bigger than rect Y-axis length %g for %O"  yMinLen yLen rect
        let xCount = int (xLen / (xMinLen*0.9999))
        let yCount = int (yLen / (yMinLen*0.9999))
        Rect3D.subDivide (rect, xCount, yCount, xGap, yGap)


    /// Divides a a 3D-rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D-rectangle.
    /// It will create as few segments as possible respecting the maximum segment length.
    /// The input maxSegmentLength is multiplied by factor 1.00001 to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member subDivideMaxLength (rect:Rect3D, xMaxLen:float, yMaxLen:float, xGap:float, yGap:float) =
        let xLen = rect.Xaxis.Length
        let yLen = rect.Yaxis.Length
        let xCount = 1 + int (xLen / (xMaxLen*1.00001))
        let yCount = 1 + int (yLen / (yMaxLen*1.00001))
        Rect3D.subDivide (rect, xCount, yCount, xGap, yGap)


    /// Divides a 3D-rectangle into a grid of points. The points are returned as an array of arrays.
    /// A xCount and yCount of 2 will only return the 4 corners of the rectangle.
    static member grid (rect:Rect3D, xCount:int, yCount:int) : Pnt[][]=
        if xCount <= 1 || yCount <= 1 then
            EuclidException.Raisef "Euclid.Rect3D.grid: xCount %d and yCount %d must be 2 or more" xCount yCount
        let xa = rect.Xaxis
        let ya = rect.Yaxis
        let xl = xa.Length
        let yl = ya.Length
        let lx1 = xl / float (xCount-1)
        let ly1 = yl / float (yCount-1)
        let o = rect.Origin
        let rss = Array.zeroCreate xCount
        for ix = 0 to xCount-1 do
            let rs = Array.zeroCreate yCount
            for iy = 0 to yCount-1 do
                let x = xa * (lx1 * float ix / xl)
                let y = ya * (ly1 * float iy / yl)
                rs.[iy] <- o + x + y
            rss.[ix] <- rs
        rss

    /// Divides a a 3D-rectangle into a grid of points.
    /// It will create as many points as possible respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member gridMinLength (rect:Rect3D, xMinLen:float, yMinLen:float) =
        let xLen = rect.Xaxis.Length
        let yLen = rect.Yaxis.Length
        if xLen < xMinLen  then EuclidException.Raisef "Euclid.Rect3D.gridMinLength: xMinLen %g is bigger than rect X-axis length %g for %O"  xMinLen xLen rect
        if yLen < yMinLen  then EuclidException.Raisef "Euclid.Rect3D.gridMinLength: yMinLen %g is bigger than rect Y-axis length %g for %O"  yMinLen yLen rect
        let xCount = 1 + int (xLen / (xMinLen*0.9999))
        let yCount = 1 + int (yLen / (yMinLen*0.9999))
        Rect3D.grid (rect, xCount, yCount)


    /// Divides a a 3D-rectangle into a grid of points.
    /// It will create as few as points as possible respecting the maximum segment length.
    /// The input maxSegmentLength is multiplied by factor 0.0001 of to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member gridMaxLength (rect:Rect3D, xMaxLen:float, yMaxLen:float) =
        let xLen = rect.Xaxis.Length
        let yLen = rect.Yaxis.Length
        let xCount = 2 + int (xLen / (xMaxLen*1.00001))
        let yCount = 2 + int (yLen / (yMaxLen*1.00001))
        Rect3D.grid (rect, xCount, yCount)



    /// Returns the line parameter and the X and Y parameters on the Rect3D as tuple (pLn, pPlX, pPlY).
    /// The parameters is the intersection point of the infinite Line3D with the infinite Rect3D.
    /// So if any of the parameters is outside of the range 0.0 to 1.0 the intersection point is actually outside of the rectangle.
    /// Returns None if they are parallel or coincident.
    static member intersectLineParametersInfinite  (ln:Line3D) (pl:Rect3D) : option<float*float*float> =
        let z = pl.NormalUnit
        let v = ln.Tangent
        let nenner = v *** z
        if isTooSmall (abs nenner) then
            // EuclidException.Raise "Euclid.Rect3D.intersectLineParameters: Line and Rect3D are parallel or line has zero length: %O, %O" ln pl
            None
        else
            let t = ((pl.Origin - ln.From) *** z) / nenner
            let xpt = ln.From + v * t
            let vecInPlane = xpt - pl.Origin
            Some   (t,
                    pl.Xaxis *** vecInPlane / pl.Xaxis.LengthSq ,
                    pl.Yaxis *** vecInPlane / pl.Yaxis.LengthSq )

    /// Returns the line parameter.
    /// The parameter is the intersection point of the infinite Line3D with the infinite Rect3D.
    /// The line is outside of the rectangle if the range is 0.0 to 1.0 .
    /// Returns None if they are parallel or coincident.
    static member intersectLineParameterInfinite  (ln:Line3D) (pl:Rect3D) : option<float> =
        let z = pl.NormalUnit
        let v = ln.Tangent
        let nenner = v *** z
        if isTooSmall (abs nenner) then
            // EuclidException.Raise "Euclid.Rect3D.intersectLineParameters: Line and Rect3D are parallel or line has zero length: %O, %O" ln pl
            None
        else
            let t = ((pl.Origin - ln.From) *** z) / nenner
            Some t

    /// Returns the line parameter and the X and Y parameters on the Rect3D as tuple (pLn, pPlX, pPlY).
    /// These parameters ar all in the range 0.0 to 1.0.
    /// Returns None if the intersection point is outside of their bounds.
    /// Use Rect3D.intersectLineParameters to get the parameters of the intersection point.
    /// Returns None if they are parallel or coincident.
    static member intersectLineParameters (ln:Line3D) (pl:Rect3D) : option<float*float*float> =
        let z = pl.NormalUnit
        let v = ln.Tangent
        let nenner = v *** z
        if isTooSmall (abs nenner) then
            None //Line and Rect3D are parallel or line has zero length
        else
            let t = ((pl.Origin - ln.From) *** z) / nenner
            if t < 0.0 || t > 1.0 then
                None
            else
                let xpt = ln.From + v * t
                let vecInPlane = xpt - pl.Origin
                let tx = pl.Xaxis *** vecInPlane / pl.Xaxis.LengthSq
                if tx < 0.0 || tx > 1.0 then
                    None
                else
                    let ty = pl.Yaxis *** vecInPlane / pl.Yaxis.LengthSq
                    if ty < 0.0 || ty > 1.0 then
                        None
                    else
                        Some (t, tx, ty)

    /// Returns intersection point of a Line3D with Rect3D.
    /// Returns None if the intersection point is outside of their bounds.
    /// Use Rect3D.intersectLineParameters to get the parameters of the intersection point.
    /// Returns None if they are parallel or coincident.
    static member intersectLine (ln:Line3D) (pl:Rect3D) : Pnt option =
        let z = pl.NormalUnit
        let v = ln.Tangent
        let nenner = v *** z
        if isTooSmall (abs nenner) then
            None //Line and Rect3D are parallel or line has zero length
        else
            let t = ((pl.Origin - ln.From) *** z) / nenner
            if t < 0.0 || t > 1.0 then
                None
            else
                let xpt = ln.From + v * t
                let vecInPlane = xpt - pl.Origin
                let tx = pl.Xaxis *** vecInPlane / pl.Xaxis.LengthSq
                if tx < 0.0 || tx > 1.0 then
                    None
                else
                    let ty = pl.Yaxis *** vecInPlane / pl.Yaxis.LengthSq
                    if ty < 0.0 || ty > 1.0 then
                        None
                    else
                        Some <| xpt


    /// Scales the 3D rectangle by a given factor.
    /// Scale center is World Origin 0,0,0
    static member inline scale (factor:float) (r:Rect3D) : Rect3D =
        Rect3D.createUnchecked(r.Origin * factor, r.Xaxis * factor, r.Yaxis * factor)

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





    /// Returns the corner diagonally opposite of corner from Origin (point 2).
    ///
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
    member inline r.FarCorner =
        r.Origin + r.Xaxis + r.Yaxis

    /// Returns the corner at end of X-axis (point 1).
    ///
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
    member inline r.XCorner =
        r.Origin + r.Xaxis


    /// Returns the corner at end of Y-axis (point 3).
    ///
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
    member inline r.YCorner =
        r.Origin + r.Yaxis

    /// Returns point 0 of the 3D-rectangle. Same as member rect.Origin.
    ///
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
    member inline r.Pt0 =
        r.Origin


    /// Returns point 1 of the 3D-rectangle.
    ///
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
    member inline r.Pt1 =
        r.Origin + r.Xaxis


    /// Returns point 2 of the 3D-rectangle. Same as rect.FarCorner.
    ///
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
    member inline r.Pt2 =
        r.Origin + r.Xaxis + r.Yaxis

    /// Returns point 3 of the 3D-rectangle.
    ///
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
    member inline r.Pt3 =
        r.Origin  + r.Yaxis

    /// Returns a 3D line from point 0 to 1 of the 2D rectangle.
    ///
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
    member inline r.Edge01 =
        Line3D (r.Origin, r.Origin + r.Xaxis)

    /// Returns a 3D line from point 1 to 2 of the 2D rectangle.
    ///
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
    member inline r.Edge12 =
        let s = r.Origin + r.Xaxis
        Line3D (s, s + r.Yaxis)



    /// Returns a 3D line from point 2 to 3 of the 2D rectangle.
    ///
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
    member inline r.Edge23 =
        let p3 = r.Origin + r.Yaxis
        Line3D (p3 + r.Xaxis, p3)


    /// Returns a 3D line from point 3 to 0 of the 2D rectangle.
    ///
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
    member inline r.Edge30 =
        Line3D (r.Origin + r.Yaxis, r.Origin)

    /// Returns the local X side as the 2D line from point 0 to 1 of the 2D rectangle.
    ///
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
    member inline r.EdgeX =
        r.Edge01

    /// Returns the local Y side as 3D line from point 0 to 3 of the 2D rectangle.
    /// This is the reverse of Edge30.
    ///
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
    member inline r.EdgeY =
        Line3D (r.Origin, r.Origin + r.Yaxis)

    /// Returns the diagonal 3D line from point 0 to 2 of the 2D rectangle.
    ///
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
    member inline r.DiagonalLine =
        Line3D (r.Origin, r.Origin + r.Yaxis + r.Xaxis)



    /// Returns the Rectangle flipped. Or rotated 180 around its diagonal from point 1 to 3.
    /// The normal of the rectangle gets flipped.
    /// Origin will be at point 2, X-axis points down to to point 1, Y-axis points left to point 3.
    ///
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
    member r.Flipped =
        Rect3D(r.Origin + r.Xaxis + r.Yaxis, -r.Yaxis, -r.Xaxis)

    /// Returns the same rectangle with a new orientation rotated by 90 degrees clockwise around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 3, X-axis to to point 0, Y-axis to point 2.
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
    member r.RotateOrientation90CW =
        Rect3D(r.Origin + r.Yaxis, -r.Yaxis, r.Xaxis)


    /// Returns the Rectangle rotated 180 degrees around its center.
    /// Returns the same rectangle with a new orientation rotated by 180 degrees around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 2, X-axis to to point 3, Y-axis to point 1.
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
    member r.RotateOrientation180 =
        Rect3D(r.Origin + r.Yaxis + r.Xaxis, -r.Xaxis, -r.Yaxis)

    /// Returns the same rectangle with a new orientation rotated by 90 degrees counter clockwise around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 1, X-axis to to point 2, Y-axis to point 0.
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
    member r.RotateOrientation90CCW =
        Rect3D(r.Origin + r.Xaxis, r.Yaxis, -r.Xaxis)


    /// Returns the 4 corners of the 3D-rectangle in Counter-Clockwise order, starting at Origin.
    /// Returns an array of 4 Points: point 0 then 1, 2 and 3.
    ///
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
    member r.Points :Pnt[] =
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        [| p0  ; p1 ; p1 + r.Yaxis; p0 + r.Yaxis|]

    /// Returns the 4 corners of the 3D-rectangle als closed loop in Counter-Clockwise order, starting at Origin.
    /// First and last point are the same.
    /// Returns an array of 5 Points: point 0 then 1, 2, 3 and again 0.
    ///
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
    member r.PointsLooped :Pnt[] =
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        [| p0  ; p1 ; p1 + r.Yaxis; p0 + r.Yaxis; p0|]

    /// Returns the 4 Edges of the 3D-rectangle in Counter-Clockwise order, starting at Origin.
    /// Returns an array of 4 Lines: from point
    /// 0 to 1,
    /// 1 to 2,
    /// 2 to 3,
    /// 3 to 0.
    ///
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
    member r.Edges :Line3D[] =
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        let p2 = p1 + r.Yaxis
        let p3 = p0 + r.Yaxis
        [| Line3D(p0, p1); Line3D(p1, p2); Line3D(p2, p3); Line3D(p3, p0)|]

    /// Returns one of the 4 Edges as 3D Line:
    /// Edge 0: from point  0 to 1
    /// Edge 1: from point  1 to 2
    /// Edge 2: from point  2 to 3
    /// Edge 3: from point  3 to 0
    ///
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
    member r.GetEdge i =
        match i with
        | 0 -> Line3D(r.Origin, r.Origin + r.Xaxis)
        | 1 -> Line3D(r.Origin + r.Xaxis, r.Origin + r.Xaxis + r.Yaxis)
        | 2 -> Line3D(r.Origin + r.Xaxis + r.Yaxis, r.Origin + r.Yaxis)
        | 3 -> Line3D(r.Origin + r.Yaxis, r.Origin)
        | _ -> EuclidException.Raisef "Euclid.Rect3D.GetEdge: index %i out of range 0..3" i
