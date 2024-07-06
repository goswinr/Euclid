namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar


/// An immutable planar 3D Rectangle with any rotation in 3D space.
/// Described by an Origin and two Edge vectors.
/// Similar to PPlane, however the two vectors are not unitized.
/// The X and Y axes are also called Width, Height2D.
/// This implementation guarantees the 3D Rectangle to be always valid.
/// That means the  X and Y axes are always perpendicular to each other.
/// However the length of one of these axes might still be zero.
///
///   local
///   Y-Axis (Height2D)
///   ^
///   |
///   |             2
/// 3 +------------+
///   |            |
///   |            |
///   |            |
///   |            |
///   |            |       local
///   +------------+-----> X-Axis (Width)
///  0-Origin       1
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Rect3D =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The Origin Corner of the 3D Rectangle.
    [<DataMember>] val Origin: Pnt

    /// The edge vector representing the X-axis of the 3D Rectangle.
    /// Also called Length.
    [<DataMember>] val Xaxis: Vec

    /// The edge vector representing the Y-axis of the 3D Rectangle.
    /// Also called Height2D.
    [<DataMember>] val Yaxis: Vec

    /// Unchecked Internal Constructor Only.
    /// Create a Parametrized Plane with X, Y and Z Direction.
    internal new (origin:Pnt, axisX:Vec, axisY:Vec) =
        #if DEBUG
        let lenX = axisX.Length
        let lenY = axisY.Length
        if isTooSmall (lenX) then  EuclidException.Raise "Euclid.Rect3D(): X-axis is too short: %O" axisX
        if isTooSmall (lenY) then  EuclidException.Raise "Euclid.Rect3D(): Y-axis is too short: %O" axisY
        //just using zeroLengthTolerance 1e-12 seems too strict for dot product check:
        if abs (axisX *** axisY) > (lenX+lenY)*1e-9 then EuclidException.Raise "Euclid.Rect3D(): X-axis and Y-axis are not perpendicular\r\n(dot=%g) > 1e-10 \r\n%O and \r\n%O" (abs (axisX *** axisY)) axisX axisY
        #endif
        {Origin=origin; Xaxis=axisX; Yaxis=axisY}

    /// The size in X direction, same as member rect.SizeX.
    member inline r.Width = r.Xaxis.Length

    /// The size in X direction, same as member rect.Width.
    member inline r.SizeX = r.Xaxis.Length

    /// The size in Y direction, same as member rect.SizeY.
    member inline r.Height2D = r.Yaxis.Length

    /// The size in Y direction, same as member rect.Height2D.
    member inline r.SizeY = r.Yaxis.Length

    /// Nicely formatted string representation of the 3D Rectangle including its size.
    override r.ToString() =
        sprintf "Euclid.Rect3D %s x %s (Origin:%s| X-ax:%s| Y-ax:%s)"
           (Format.float r.SizeX) (Format.float r.SizeY)
            r.Origin.AsString r.Xaxis.AsString r.Yaxis.AsString


    /// Format the 3D Rectangle into string with nice floating point number formatting of X, Y and Z size only.
    /// But without type name as in v.ToString()
    member r.AsString = sprintf "%s x %s" (Format.float r.SizeX) (Format.float r.SizeY)

    /// Returns the corner diagonally opposite of corner from Origin (point 2).
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.FarCorner = r.Origin + r.Xaxis + r.Yaxis

    /// Returns the corner at end of X-axis (point 1).
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.XCorner = r.Origin + r.Xaxis


    /// Returns the corner at end of Y-axis (point 3).
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.YCorner = r.Origin + r.Yaxis

    /// Returns point 0 of the 3D rectangle. Same as member rect.Origin.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.Pt0 = r.Origin


    /// Returns point 1 of the 3D rectangle.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.Pt1 = r.Origin + r.Xaxis


    /// Returns point 2 of the 3D rectangle. Same as rect.FarCorner.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.Pt2 = r.Origin + r.Xaxis + r.Yaxis

    /// Returns point 3 of the 3D rectangle.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.Pt3 = r.Origin  + r.Yaxis

    /// Returns a 3D line from point 0 to 1 of the 2D rectangle.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.Edge01 = Line3D (r.Origin, r.Origin + r.Xaxis)

    /// Returns a 3D line from point 1 to 2 of the 2D rectangle.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.Edge12 =
        let s = r.Origin + r.Xaxis
        Line3D (s, s + r.Yaxis)



    /// Returns a 3D line from point 2 to 3 of the 2D rectangle.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.Edge23 =
        let p3 = r.Origin + r.Yaxis
        Line3D (p3 + r.Xaxis, p3)


    /// Returns a 3D line from point 3 to 0 of the 2D rectangle.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.Edge30 = Line3D (r.Origin + r.Yaxis, r.Origin)

    /// Returns the local X side as the 2D line from point 0 to 1 of the 2D rectangle.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.EdgeX = r.Edge01

    /// Returns the local Y side as 3D line from point 0 to 3 of the 2D rectangle.
    /// This is the reverse of Edge30.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.EdgeY = Line3D (r.Origin, r.Origin + r.Yaxis)

    /// Returns the diagonal 3D line from point 0 to 2 of the 2D rectangle.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member inline r.DiagonalLine = Line3D (r.Origin, r.Origin + r.Yaxis + r.Xaxis)

    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit =
        let len = r.Xaxis.Length
        if isTooTiny len then EuclidException.Raise "Euclid.Rect3D.XaxisUnit: rect Xaxis is too small for unitizing: %s" r.AsString
        r.Xaxis*(1./len)

    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit =
        let len = r.Yaxis.Length
        if isTooTiny len then EuclidException.Raise "Euclid.Rect3D.XaxisUnit: rect Yaxis is too small for unitizing: %s" r.AsString
        r.Yaxis*(1./len)

    /// Returns the Normal
    /// Resulting from the cross product of r.Xaxis with r.Yaxis.
    member inline r.Normal = Vec.cross(r.Xaxis, r.Yaxis)

    /// Returns the unitized Normal.
    /// Resulting from the cross product of r.Xaxis with r.Yaxis.
    member r.NormalUnit =
        let z = Vec.cross(r.Xaxis, r.Yaxis)
        let len = z.Length
        if isTooTiny len then EuclidException.Raise "Euclid.Rect3D.NormalUnit: rect is too small for finding a normal vector: %s" r.AsString
        z*(1./len)

    /// Returns the diagonal vector of the 3D Rectangle.
    /// From Origin to FarCorner.
    member inline r.Diagonal = r.Xaxis + r.Yaxis

    /// Returns the center of the 3D Rectangle.
    member inline r.Center = r.Origin + r.Xaxis*0.5 + r.Yaxis*0.5

    /// Returns the Rectangle flipped. Or rotated 180 around its diagonal from point 1 to 3.
    /// The normal of the rectangle gets flipped.
    /// Origin will be at point 2, X-axis points down to to point 1, Y-axis points left to point 3.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member r.Flipped = Rect3D(r.Origin + r.Xaxis + r.Yaxis, -r.Yaxis, -r.Xaxis)

    /// Returns the same rectangle with a new orientation rotated by 90 degrees clockwise around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 3, X-axis to to point 0, Y-axis to point 2.
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member r.RotateOrientation90CW = Rect3D(r.Origin + r.Yaxis, -r.Yaxis, r.Xaxis)


    /// Returns the Rectangle rotated 180 degrees around its center.
    /// Returns the same rectangle with a new orientation rotated by 180 degrees around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 2, X-axis to to point 3, Y-axis to point 1.
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member r.RotateOrientation180 = Rect3D(r.Origin + r.Yaxis + r.Xaxis, -r.Xaxis, -r.Yaxis)

    /// Returns the same rectangle with a new orientation rotated by 90 degrees counter clockwise around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 1, X-axis to to point 2, Y-axis to point 0.
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member r.RotateOrientation90CCW = Rect3D(r.Origin + r.Xaxis, r.Yaxis, -r.Xaxis)


    /// Returns the 4 corners of the 3D Rectangle in Counter-Clockwise order, starting at Origin.
    /// Returns an array of 4 Points: point 0 then 1, 2 and 3.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member r.Points :Pnt[] =
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        [| p0  ; p1 ; p1 + r.Yaxis; p0 + r.Yaxis|]

    /// Returns the 4 corners of the 3D Rectangle als closed loop in Counter-Clockwise order, starting at Origin.
    /// First and last point are the same.
    /// Returns an array of 5 Points: point 0 then 1, 2, 3 and again 0.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member r.PointsLooped :Pnt[] =
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        [| p0  ; p1 ; p1 + r.Yaxis; p0 + r.Yaxis; p0|]

    /// Returns the 4 Edges of the 3D Rectangle in Counter-Clockwise order, starting at Origin.
    /// Returns an array of 4 Lines: from point 0 to 1, 1 to 2 to 3 and 3 to 0.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
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
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    member r.GetEdge i =
        match i with
        | 0 -> Line3D(r.Origin, r.Origin + r.Xaxis)
        | 1 -> Line3D(r.Origin + r.Xaxis, r.Origin + r.Xaxis + r.Yaxis)
        | 2 -> Line3D(r.Origin + r.Xaxis + r.Yaxis, r.Origin + r.Yaxis)
        | 3 -> Line3D(r.Origin + r.Yaxis, r.Origin)
        | _ -> EuclidException.Raise "Euclid.Rect3D.GetEdge: index %i out of range 0..3" i


    /// Evaluate a X and Y parameter of the 3D Rectangle.
    ///  0.0, 0.0 returns the Origin.
    ///  1.0, 1.0 returns the FarCorner.
    member inline r.EvaluateAt (xParameter:float, yParameter:float) =
        r.Origin + r.Xaxis * xParameter + r.Yaxis * yParameter

    /// Evaluate a point at X and Y distance on the respective axes of the 2D Rectangle.
    member inline r.EvaluateDist (xDistance:float, yDistance:float) =
        let lx = r.Xaxis.Length
        let ly = r.Yaxis.Length
        if isTooTiny (lx) || isTooTiny (ly) then
            EuclidException.Raise "Euclid.Rect3D.EvaluateDist: rect Xaxis or Yaxis is too small for evaluating distance: %s" r.AsString
        r.Origin + r.Xaxis * (xDistance/lx) + r.Yaxis * (yDistance/ly)

    /// Calculates the volume of the 3D Rectangle.
    member inline r.Area =
        r.Xaxis.Length*r.Yaxis.Length

    /// Gets the Plane that this 3D rectangle is based on.
    member inline r.Plane =
        let x = r.Xaxis.Unitized
        let y = r.Yaxis.Unitized
        let z = UnitVec.cross(x, y).Unitized
        PPlane.createUnchecked (r.Origin, x, y, z)

    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Checks if two 3D Rectangles are equal within tolerance.
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


    /// Returns the 3D Rectangle expanded by distance on all four sides.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (r:Rect3D) =
        let siX = r.SizeX
        let siY = r.SizeY
        let d = dist * -2.0
        if siX<=d || siY<=d  then
            EuclidException.Raise "Euclid.Rect3D.expand: the 3D Rectangle %s is too small to expand by negative distance %s"  r.AsString (Format.float dist)
        let x = r.Xaxis * (dist / siX)
        let y = r.Yaxis * (dist / siY)
        Rect3D(r.Origin-x-y, r.Xaxis+x*2., r.Yaxis+y*2.)

    /// Returns the 3D Rectangle expanded by respective distances on all four sides.
    /// Does check for overflow if distance is negative and fails.
    /// distLen, distWidth are for the local  X and Y-axis respectively.
    static member expandXY distLen distWidth (r:Rect3D) =
        let siX = r.SizeX
        let siY = r.SizeY
        if siX <= distLen   * -2.0 then EuclidException.Raise "Euclid.Rect3D.expandXY: the 3D Rectangle %s is too small to expand by negative distance distLen %s"  r.AsString (Format.float distLen)
        if siY <= distWidth * -2.0 then EuclidException.Raise "Euclid.Rect3D.expandXY: the 3D Rectangle %s is too small to expand by negative distance distWidth %s"  r.AsString (Format.float distWidth)
        let x = r.Xaxis * (distLen   / r.SizeX)
        let y = r.Yaxis * (distWidth / r.SizeY)
        Rect3D(r.Origin-x-y, r.Xaxis+x*2., r.Yaxis+y*2.)

    /// Create a 3D Rectangle from the origin point, an x-edge and an y-edge.
    /// Fails if x and y are not perpendicularity.
    /// Fails on vectors shorter than 1e-9.
    static member createFromVectors(origin, x:Vec, y:Vec) =
        if isTooSmallSq(x.LengthSq)then EuclidException.Raise "Euclid.Rect3D.createFromVectors(origin, x:Vec, y:Vec): X-axis is too short:\r\n%O" y
        if isTooSmallSq(y.LengthSq) then EuclidException.Raise "Euclid.Rect3D.createFromVectors(origin, x:Vec, y:Vec): Y-axis is too short:\r\n%O" y
        //zeroLengthTolerance seems too strict for dot product:
        if abs (x *** y) > 1e-10 then EuclidException.Raise "Euclid.Rect3D.createFromVectors(origin, x:Vec, y:Vec): X-axis and Y-axis are not perpendicular (dot=%g): \r\n%O and\r\n%O" (abs (x *** y)) x y
        Rect3D(origin, x, y)

    /// Give PPlane and sizes.
    /// The Rect3D's Origin will be at the plane's Origin.
    /// Fails on negative sizes.
    static member createFromPlane (pl:PPlane, sizeX:float, sizeY:float) =
        if isNegative(sizeX) then EuclidException.Raise "Euclid.Rect3D.createFromPlane sizeX is negative: %g, sizeY is: %g, plane: %O"  sizeX sizeY  pl
        if isNegative(sizeY) then EuclidException.Raise "Euclid.Rect3D.createFromPlane sizeY is negative: %g, sizeX is: %g, plane: %O"  sizeY sizeX  pl
        Rect3D(pl.Origin, pl.Xaxis*sizeX, pl.Yaxis*sizeY)

    /// Give PPlane and sizes.
    /// The Rect3D's Center will be at the plane's Origin.
    /// Fails on negative sizes.
    static member createCenteredFromPlane (pl:PPlane, sizeX:float, sizeY:float) =
        if isNegative(sizeX) then EuclidException.Raise "Euclid.Rect3D.createCenteredFromPlane sizeX is negative: %g, sizeY is: %g, plane: %O"  sizeX sizeY  pl
        if isNegative(sizeY) then EuclidException.Raise "Euclid.Rect3D.createCenteredFromPlane sizeY is negative: %g, sizeX is: %g, plane: %O"  sizeY sizeX  pl
        let x = pl.Xaxis*sizeX
        let y = pl.Yaxis*sizeY
        Rect3D(pl.Origin- x*0.5 - y*0.5, x, y)

    /// Give 2D Bounding Rect.
    static member createFromBRect (b:BRect) =
        Rect3D(b.MinPt.AsPnt, Vec.Xaxis*b.SizeX, Vec.Yaxis*b.SizeY)


    /// Create a 3D Rectangle from the origin point and X-edge and Y edge.
    /// Does not check for perpendicularity.
    static member createUnchecked (origin, x:Vec, y:Vec) =
        Rect3D(origin, x, y)

    /// Creates a 3D rectangle from three points. Fails if points are too close to each other or all colinear.
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y axis.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    static member createFrom3Points (origin:Pnt, xPt:Pnt, yPt:Pnt) =
        let x = xPt - origin
        if isTooSmallSq x.LengthSq  then EuclidException.Raise "Euclid.Rect3D.createThreePoints: X-Point %s too close to origin: %s." origin.AsString x.AsString
        let y = yPt - origin
        if isTooSmallSq y.LengthSq  then EuclidException.Raise "Euclid.Rect3D.createThreePoints: Y-Point %s too close to origin: %s." origin.AsString y.AsString
        let z = Vec.cross(x,y)
        if isTooSmallSq z.LengthSq  then EuclidException.Raise "Euclid.Rect3D.createThreePoints: Y-Point %s is too close to Xaxis." y.AsString
        let yu = Vec.cross(z, x).Unitized
        let yr = yu * (yu *** y) // get the y point projected on the y axis
        Rect3D(origin, x, yr)

    /// Tries to create a 3D rectangle from three points. Returns None if points are too close to each other or all colinear..
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y axis.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
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


    /// Returns the Rectangle flipped. Or rotated 180 around its diagonal from point 1 to 3.
    /// Origin will be at point 2, X-axis points down to to point 1, Y-axis points left to point 3.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    static member flip (r:Rect3D) = Rect3D(r.Origin + r.Xaxis + r.Yaxis, -r.Yaxis, -r.Xaxis)

    /// Translate along the local X-axis of the 3D Rectangle.
    static member translateX (distX:float) (r:Rect3D) =
        let x = r.Xaxis
        let len = x.Length
        if isTooTiny len then EuclidException.Raise "Euclid.Rect3D.translateX: rect.Xaxis is zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + x*(distX/len), x, r.Yaxis)

    /// Translate along the local Y-axis of the 3D Rectangle.
    static member translateY (distY:float) (r:Rect3D) =
        let y = r.Yaxis
        let len = y.Length
        if isTooTiny len then EuclidException.Raise "Euclid.Rect3D.translateY: rect.Yaxis is zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + y*(distY/len), r.Xaxis, y)

    /// Translate by a 3D vector.(same as Rect3D.move)
    static member translate (v:Vec) (r:Rect3D) =
        Rect3D(r.Origin + v, r.Xaxis, r.Yaxis)

    /// Move the 3D Rectangle by a vector.(same as Rect3D.translate)
    static member move (v:Vec) (r:Rect3D) =
        Rect3D(r.Origin + v, r.Xaxis, r.Yaxis)

    /// Transform the 3D Rectangle by the given RigidMatrix.
    /// The returned 3D Rectangle is guaranteed to have orthogonal vectors.
    static member transform (m:RigidMatrix) (r:Rect3D) =
        let o = Pnt.transformRigid m r.Origin
        let x = Vec.transformRigid m r.Xaxis
        let y = Vec.transformRigid m r.Yaxis
        Rect3D(o, x, y)


    /// Offset or Translate along the local Z-axis.
    /// The local Z-axis is calculated from cross product of X and Y-axis of the 3D Rectangle.
    static member offsetZ (offsetDistance :float) (r:Rect3D) =
        let z = Vec.cross(r.Xaxis, r.Yaxis)
        let len = z.Length
        if isTooTiny len then EuclidException.Raise "Euclid.Rect3D.offsetZ: rect is too small for offsetting zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + z*(offsetDistance/len), r.Xaxis, r.Yaxis)

    /// Offset a Rect3D like a Polyline inwards by a given distance.
    /// Fails if the distance is larger than half the size of the rectangle.
    static member offset dist (rect:Rect3D) =
        let xl = rect.Xaxis.Length
        let yl = rect.Yaxis.Length
        if xl < dist*2.0 || yl < dist*2.0 then
            EuclidException.Raise "Euclid.Rect3D.offset: the 2D Rectangle %s is too small to offset by distance %s"  rect.AsString (Format.float dist)
        let x = rect.Xaxis * (dist / xl)
        let y = rect.Yaxis * (dist / yl)
        Rect3D(rect.Origin+x+y, rect.Xaxis - x*2.0, rect.Yaxis - y*2.0)


    /// Offset a Rect3D like a Polyline inwards by four distances.
    /// The distance array is for Edge01, Edge12, Edge23 and Edge30 respectively.
    /// Fails if the distance is larger than half the size of the rectangle.
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1
    static member offsetVar (dist:float[]) (rect:Rect3D) =
        if dist.Length <> 4 then EuclidException.Raise "Euclid.Rect3D.offsetVar: the distance array must have 4 elements, but has %i" dist.Length
        let xl = rect.Xaxis.Length
        let yl = rect.Yaxis.Length
        if xl < dist.[1]+dist.[3] || yl < dist.[0]+dist.[2] then
            EuclidException.Raise "Euclid.Rect3D.offsetVar: the 2D Rectangle %s is too small to offset by distances [|%s;%s;%s;%s|]"  rect.AsString (Format.float dist.[0]) (Format.float dist.[1]) (Format.float dist.[2]) (Format.float dist.[3])
        let x0 = rect.Xaxis * (dist.[3] / xl)
        let x1 = rect.Xaxis * (dist.[1] / xl)
        let y0 = rect.Yaxis * (dist.[0] / yl)
        let y1 = rect.Yaxis * (dist.[2] / yl)
        Rect3D(rect.Origin+x0+y0, rect.Xaxis - x0 - x1, rect.Yaxis - y0 - y1)


    ///<summary>Offsets a local Rect3D at one of the four corners.</summary>
    ///<param name="rect">The 3D Rectangle</param>
    ///<param name="corner">The Index of the corner to Offset
    ///
    ///   local
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///  0-Origin       1 </param>
    ///<param name="xOffset">The local offset distances in x direction. (Applies to the y side.) Positive values offset to the inside of the rectangle, negative values will offset outwards.</param>
    ///<param name="yOffset">The local offset distances in y direction. (Applies to the x side.) Positive values offset to the inside of the rectangle, negative values will offset outwards.</param>
    ///<param name="xWidth">The the width (or size in x direction) that will be added to the current offset.</param>
    ///<param name="yHeight">The the height (or size in y direction) that will be added to the current offset.</param>
    ///<returns>A new 3D Rectangle. It will always have the same x and y axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetCorner (rect:Rect3D, corner:int, xOffset:float, yOffset:float, xWidth:float, yHeight:float) =
        let xa = rect.Xaxis
        let ya = rect.Yaxis
        let xl = xa.Length
        let yl = ya.Length
        if isTooTiny (xl) || isTooTiny (yl) then
            EuclidException.Raise "Euclid.Rect3D.offsetCorner: the 3D Rectangle %s is too small to offsetCorner"  rect.AsString
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
            EuclidException.Raise "Euclid.Rect3D.offsetCorner: corner %i out of range 0..3" corner

    ///<summary>Offsets a local Rect3D at one of the four corners.</summary>
    ///<param name="rect">The 3D Rectangle</param>
    ///<param name="edgeIdx">The Index of the edge to offset
    ///
    ///   local
    ///   Y-Axis (Height3D)
    ///   ^
    ///   |
    ///   |      2
    ///   +------------+
    ///   |            |
    ///   |            |
    ///  3|            |1
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-Axis (Width)
    ///         0</param>
    ///<param name="offEdge">The local offset distances parallel to the edge.</param>
    ///<param name="width">The width. this is like the next offset to be applied to offEdge</param>
    ///<param name="offStart">The local offset distances perpendicular to the edge at the start.</param>
    ///<param name="offEnd">The local offset distances perpendicular to the edge at the end.</param>
    ///<returns>A new 3D Rectangle. It will always have the same x and y axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetEdge (rect:Rect3D, edgeIdx:int, offEdge:float, width:float, offStart:float, offEnd:float) =
        let x = rect.Xaxis
        let y = rect.Yaxis
        let lx = x.Length
        let ly = y.Length
        let inline xLen l = x * (l / lx)
        let inline yLen l = y * (l / ly)
        let inline orig xx yy = rect.Origin + xLen xx + yLen yy
        if isTooTiny (lx) || isTooTiny (ly) then
            EuclidException.Raise "Euclid.Rect3D.offsetEdge: the 3D Rectangle %s is too small to offsetCorner"  rect.AsString
        match edgeIdx with
        | 0 ->  Rect3D( orig offStart offEdge
                      , xLen (lx-offStart-offEnd)
                      , yLen width  )

        | 1 -> Rect3D( orig (lx-offEdge-width) offStart
                      , xLen width
                      , yLen (ly-offStart-offEnd)  )

        | 2 -> Rect3D( orig offEnd (ly-offEdge-width)
                      , xLen (lx-offStart-offEnd)
                      , yLen width   )

        | 3 -> Rect3D( orig offEdge offEnd
                      , xLen width
                      , yLen (ly-offStart-offEnd)  )
        | _ ->
            EuclidException.Raise "Euclid.Rect3D.offsetCorner: corner %i out of range 0..3" edgeIdx



    /// Divides a 3D Rectangle into a grid of sub-rectangles. The sub-rectangles are returned as an array of arrays.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D Rectangle.
    /// The returned array has xCount elements, each element is an array of yCount sub-rectangles.
    static member subDivide (rect:Rect3D, xCount:int, yCount:int, xGap:float, yGap:float)=
        if xCount <= 0 || yCount <= 0 then
            EuclidException.Raise "Euclid.Rect3D.subDivide: xCount %d and yCount %d must be 1 or more" xCount yCount
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


    /// Divides a a 3D Rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D Rectangle.
    /// It will create as many sub-rectangles as possible respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.99999 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member subDivideMinLength (rect:Rect3D, xMinLen:float, yMinLen:float, xGap:float, yGap:float) =
        let xLen = rect.Xaxis.Length
        let yLen = rect.Yaxis.Length
        if xLen < xMinLen  then EuclidException.Raise "Euclid.Rect3D.subDivideMinLength: xMinLen %g is bigger than rect X-axis length %g for %O"  xMinLen xLen rect
        if yLen < yMinLen  then EuclidException.Raise "Euclid.Rect3D.subDivideMinLength: yMinLen %g is bigger than rect Y-axis length %g for %O"  yMinLen yLen rect
        let xCount = int (xLen / (xMinLen*0.9999))
        let yCount = int (yLen / (yMinLen*0.9999))
        Rect3D.subDivide (rect, xCount, yCount, xGap, yGap)


    /// Divides a a 3D Rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D Rectangle.
    /// It will create as few as segments as possible respecting the maximum segment length.
    /// The input maxSegmentLength is multiplied by factor 1.00001 of to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member subDivideMaxLength (rect:Rect3D, xMaxLen:float, yMaxLen:float, xGap:float, yGap:float) =
        let xLen = rect.Xaxis.Length
        let yLen = rect.Yaxis.Length
        let xCount = 1 + int (xLen / (xMaxLen*1.00001))
        let yCount = 1 + int (yLen / (yMaxLen*1.00001))
        Rect3D.subDivide (rect, xCount, yCount, xGap, yGap)


    /// Divides a 3D Rectangle into a grid of points. The points are returned as an array of arrays.
    /// A xCount and yCount of 2 will only return the 4 corners of the rectangle.
    static member grid (rect:Rect3D, xCount:int, yCount:int) : Pnt[][]=
        if xCount <= 1 || yCount <= 1 then
            EuclidException.Raise "Euclid.Rect3D.grid: xCount %d and yCount %d must be 2 or more" xCount yCount
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

    /// Divides a a 3D Rectangle into a grid of points.
    /// It will create as many points as possible respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member gridMinLength (rect:Rect3D, xMinLen:float, yMinLen:float) =
        let xLen = rect.Xaxis.Length
        let yLen = rect.Yaxis.Length
        if xLen < xMinLen  then EuclidException.Raise "Euclid.Rect3D.gridMinLength: xMinLen %g is bigger than rect X-axis length %g for %O"  xMinLen xLen rect
        if yLen < yMinLen  then EuclidException.Raise "Euclid.Rect3D.gridMinLength: yMinLen %g is bigger than rect Y-axis length %g for %O"  yMinLen yLen rect
        let xCount = 1 + int (xLen / (xMinLen*0.9999))
        let yCount = 1 + int (yLen / (yMinLen*0.9999))
        Rect3D.grid (rect, xCount, yCount)


    /// Divides a a 3D Rectangle into a grid of points.
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