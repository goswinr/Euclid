namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Util
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
    internal new (origin, axisX, axisY) ={Origin=origin; Xaxis=axisX; Yaxis=axisY}

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

    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit =
        let len = r.Xaxis.Length
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect3D.XaxisUnit: rect Xaxis is too small for unitizing: %s" r.AsString
        r.Xaxis*(1./len)

    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit =
        let len = r.Yaxis.Length
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect3D.XaxisUnit: rect Yaxis is too small for unitizing: %s" r.AsString
        r.Yaxis*(1./len)

    /// Returns the Normal; resulting from the cross product of r.Xaxis with r.Yaxis.
    member inline r.Normal = Vec.cross(r.Xaxis, r.Yaxis)

    /// Returns the unitized Normal; resulting from the cross product of r.Xaxis with r.Yaxis.
    member r.NormalUnit =
        let z = Vec.cross(r.Xaxis, r.Yaxis)
        let len = z.Length
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect3D.NormalUnit: rect is too small for finding a normal vector: %s" r.AsString
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

    /// Returns the Rectangle rotated 90 degrees clockwise around its center.
    /// The normal of the rectangle stays the same.
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
    member r.RotatedCW90 = Rect3D(r.Origin + r.Yaxis, -r.Yaxis, r.Xaxis)


    /// Returns the Rectangle rotated 180 degrees around its center.
    /// The normal of the rectangle stays the same.
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
    member r.Rotated180 = Rect3D(r.Origin + r.Yaxis + r.Xaxis, -r.Xaxis, -r.Yaxis)

    /// Returns the Rectangle rotated 90 degrees Counter-Clockwise around its center.
    /// The normal of the rectangle stays the same.
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
    member r.RotatedCCW90 = Rect3D(r.Origin + r.Xaxis, r.Yaxis, -r.Xaxis)


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


    /// Evaluate a X, Y and Z parameter of the the 3D Rectangle.
    ///  0.0, 0.0, 0.0 returns the Origin.
    ///  1.0, 1.0, 1.0 returns the FarCorner.
    member inline r.EvaluateAt (xParameter:float, yParameter:float) =
        r.Origin + r.Xaxis * xParameter + r.Yaxis * yParameter


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
    static member equals tol (a:Rect3D) (b:Rect3D) =
        let tt = tol*tol
        Pnt.distanceSq a.Origin b.Origin < tt &&
        Vec.differenceSq a.Xaxis b.Xaxis < tt &&
        Vec.differenceSq a.Yaxis b.Yaxis < tt


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
        let y = r.Yaxis * (distWidth / r.SizeY )
        Rect3D(r.Origin-x-y, r.Xaxis+x*2., r.Yaxis+y*2.)

    /// Give PPlane and sizes.
    /// The Rect3D's Origin will be at the plane's Origin.
    static member createFromPlane (pl:PPlane, sizeX:float, sizeY:float ) =
        if sizeX < 0. then EuclidException.Raise "Euclid.Rect3D.createFromPlane sizeX is negative: %g, sizeY is: %g, plane: %O"  sizeX sizeY  pl
        if sizeY < 0. then EuclidException.Raise "Euclid.Rect3D.createFromPlane sizeY is negative: %g, sizeX is: %g, plane: %O"  sizeY sizeX  pl
        Rect3D(pl.Origin, pl.Xaxis*sizeX, pl.Yaxis*sizeY)

    /// Give PPlane and sizes.
    /// The Rect3D's Center will be at the plane's Origin.
    static member createCenteredFromPlane (pl:PPlane, sizeX:float, sizeY:float ) =
        if sizeX < 0. then EuclidException.Raise "Euclid.Rect3D.createCenteredFromPlane sizeX is negative: %g, sizeY is: %g, plane: %O"  sizeX sizeY  pl
        if sizeY < 0. then EuclidException.Raise "Euclid.Rect3D.createCenteredFromPlane sizeY is negative: %g, sizeX is: %g, plane: %O"  sizeY sizeX  pl
        let x = pl.Xaxis*sizeX
        let y = pl.Yaxis*sizeY
        Rect3D(pl.Origin- x*0.5 - y*0.5, x, y)


    /// Give 2D Bounding Rect.
    static member createFromBRect (b:BRect) =
        Rect3D(b.MinPt.AsPnt, Vec.Xaxis*b.SizeX, Vec.Yaxis*b.SizeY)


    /// Move the 3D Rectangle by a vector.
    static member move (v:Vec) (r:Rect3D) =
        Rect3D(r.Origin + v, r.Xaxis, r.Yaxis)

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
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect3D.translateX: rect.Xaxis is zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + x*(distX/len), x, r.Yaxis)

    /// Translate along the local Y-axis of the 3D Rectangle.
    static member translateY (distY:float) (r:Rect3D) =
        let y = r.Yaxis
        let len = y.Length
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect3D.translateY: rect.Yaxis is zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + y*(distY/len), r.Xaxis, y)

    /// Translate by a 3D vector.
    static member translate (v:Vec) (r:Rect3D) =
        Rect3D(r.Origin + v, r.Xaxis, r.Yaxis)

    /// Offset or Translate along the local Z-axis.
    /// The local Z-axis is calculated from cross product of X and Y-axis of the 3D Rectangle.
    static member offset (offsetDistance :float) (r:Rect3D) =
        let z = Vec.cross(r.Xaxis, r.Yaxis)
        let len = z.Length
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect3D.offset: rect is too small for offsetting zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + z*(offsetDistance/len), r.Xaxis, r.Yaxis)

    /// Transform the 3D Rectangle by the given RigidMatrix.
    /// The returned 3D Rectangle is guaranteed to have orthogonal vectors.
    static member transform (m:RigidMatrix) (r:Rect3D) =
        let o = Pnt.transformRigid m r.Origin
        let x = Vec.transformRigid m r.Xaxis
        let y = Vec.transformRigid m r.Yaxis
        Rect3D(o, x, y)

