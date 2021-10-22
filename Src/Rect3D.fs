namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Util

/// A immutable planar 3D Rectangle with any rotation in 3D space.
/// Described by an Origin and two Edge vectors.
/// Similar to PPlane, however the two vectors are not unitized.
/// The X and Y axes are also called Length, Width.
/// This implementation guarantees the 3D Rectangle to be always valid.
/// That means the Min X and Y axes cannot be flipped individually.
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
type Rect3D =

    /// The Origin Corner of the 3D Rectangle.
    val Origin: Pnt

    /// The Edge vector representing the X-axis of the 3D Rectangle.
    /// Also called Length.
    val Xaxis: Vec

    /// The Edge vector representing the Y-axis of the 3D Rectangle.
    /// Also called Width.
    val Yaxis: Vec

    /// Unchecked Internal Constructor Only.
    /// Create a Parametrized Plane with X, Y and Z Direction.
    internal new (origin,axisX,axisY)  =  {Origin=origin; Xaxis=axisX; Yaxis=axisY}

    /// The size in X direction, same as member rect.SizeX.
    member inline r.Length = r.Xaxis.Length

    /// The size in X direction, same as member rect.Length.
    member inline r.SizeX = r.Xaxis.Length

    /// The size in Y direction, same as member rect.SizeY.
    member inline r.Width  = r.Yaxis.Length

    /// The size in Y direction, same as member rect.Width.
    member inline r.SizeY  = r.Yaxis.Length

    /// Nicely formatted string representation of the 3D Rectangle including its size.
    override r.ToString() =
        sprintf "FsEx.Geo.Rect3D %s x %s  (Origin:%s| X-ax:%s| Y-ax:%s)"
            (Format.float r.Length)  (Format.float r.Width)
            r.Origin.AsString r.Xaxis.AsString r.Yaxis.AsString


    /// Format the 3D Rectangle into string with nice floating point number formatting of X,Y and Z size only
    /// But without type name as in v.ToString()
    member r.AsString = sprintf "%s x %s" (Format.float r.Length)  (Format.float r.Width)

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
    member inline r.FarCorner = r.Origin + r.Xaxis + r.Yaxis

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
    member inline r.XCorner = r.Origin + r.Xaxis


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
    member inline r.YCorner = r.Origin + r.Yaxis

    /// Returns Point 0 of the 3D rectangle. Same as member rect.Origin.
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
    member inline r.Pt0 = r.Origin


    /// Returns Point 1 of the 3D rectangle.
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
    member inline r.Pt1 = r.Origin + r.Xaxis 


    /// Returns Point 2 of the 3D rectangle. Same as rect.FarCorner.
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
    member inline r.Pt2 = r.Origin + r.Xaxis + r.Yaxis

    /// Returns Point 3 of the 3D rectangle.
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
    member inline r.Pt3 = r.Origin  + r.Yaxis

    /// Creates a unitized version of the local X-Axis
    member inline r.XaxisUnit =
        let len = r.Xaxis.Length
        if len = zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Rect3D.XaxisUnit: rect XAxis is too small for unitizing: %s" r.AsString
        r.Xaxis*(1./len)

    /// Creates a unitized version of the local Y-Axis
    member inline r.YaxisUnit =
        let len = r.Yaxis.Length
        if len = zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Rect3D.XaxisUnit: rect YAxis is too small for unitizing: %s" r.AsString
        r.Yaxis*(1./len)

    /// Returns the Normal; resulting from the cross product of r.Xaxis with r.Yaxis
    member inline r.Normal = Vec.cross(r.Xaxis, r.Yaxis)

    /// Returns the unitized Normal; resulting from the cross product of r.Xaxis with r.Yaxis
    member r.NormalUnit =
        let z = Vec.cross(r.Xaxis,r.Yaxis)
        let len = z.Length
        if len = zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Rect3D.NormalUnit: rect is too small for finding a normal vector: %s" r.AsString
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
    member r.Flipped = Rect3D(r.Origin + r.Xaxis + r.Yaxis, -r.Yaxis, -r.Xaxis)

    /// Returns the Rectangle rotated 90 degrees clockwise around its center.
    /// The normal of the rectangle stays the same.
    /// Origin will be at point 3, X-axis  to to point 0, Y-axis  to point 2.    
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
    member r.RotatedCW90 = Rect3D(r.Origin + r.Yaxis , -r.Yaxis, r.Xaxis)


    /// Returns the Rectangle rotated 180 degrees around its center.
    /// The normal of the rectangle stays the same.
    /// Origin will be at point 2, X-axis  to to point 3, Y-axis  to point 1.    
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
    member r.Rotated180 = Rect3D(r.Origin + r.Yaxis + r.Xaxis , -r.Xaxis, -r.Yaxis)

    /// Returns the Rectangle rotated 90 degrees counter clockwise around its center.
    /// The normal of the rectangle stays the same.
    /// Origin will be at point 1, X-axis  to to point 2, Y-axis  to point 0.    
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
    member r.RotatedCCW90 = Rect3D(r.Origin + r.Xaxis , r.Yaxis, -r.Xaxis)
    

    /// Returns the 4 corners of the 3D Rectangle in counter clockwise order, starting at Origin.
    /// Returns an array of 4 Points: point 0 then 1, 2 and  3.
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
    member r.Corners :Pnt[] =
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        [| p0  ; p1 ; p1 + r.Yaxis; p0 + r.Yaxis|]

    /// Returns the 4 corners of the 3D Rectangle als closed loop in counter clockwise order, starting at Origin.
    /// First and last point are the same.
    /// Returns an array of 5 Points: point 0 then 1, 2, 3 and  again 0.
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
    member r.CornersClosed :Pnt[] =
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        [| p0  ; p1 ; p1 + r.Yaxis; p0 + r.Yaxis; p0|]        
    

    /// Evaluate a X,Y and Z parameter of the  the 3D Rectangle.
    ///  0.0, 0.0, 0.0 returns the Origin.
    ///  1.0, 1.0, 1.0 returns the FarCorner.
    member inline r.EvaluateAt (xParameter:float,yParameter:float) =
        r.Origin + r.Xaxis * xParameter + r.Yaxis * yParameter


    /// Calculates the volume of the 3D Rectangle.
    member inline r.Area  =
        r.Xaxis.Length*r.Yaxis.Length

    /// Gets the Plane that this 3D rectangle is based on.
    member inline r.PPlane  =
        PPlane.fromOriginXaxisAndYaxis r.Origin r.Xaxis r.Yaxis

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


    /// Returns the 3D Rectangle expanded by distance on all six sides.
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expand dist (r:Rect3D) =
        let len = r.Length
        let wid = r.Width
        let d = dist * -2.0
        if len<=d || wid<=d  then
            FsExGeoException.Raise "FsEx.Geo.Rect3D.expand: the 3D Rectangle %s is too small to expand by negative distance %s"  r.AsString (Format.float dist)
        let x = r.Xaxis * (dist / len)
        let y = r.Yaxis * (dist / wid)
        Rect3D(r.Origin-x-y, r.Xaxis+x*2., r.Yaxis+y*2.)

    /// Returns the 3D Rectangle expanded by respective distances on all six sides
    /// Does check for overflow if distance is negative and fails.
    /// distLen, distWid  are for x, y axis respectively.
    static member expandXYZ distLen distWid  (r:Rect3D) =
        let len = r.Length
        let wid = r.Width
        if len <= distLen * -2.0 then FsExGeoException.Raise "FsEx.Geo.Rect3D.expandXYZ: the 3D Rectangle %s is too small to expand by negative distance distLen %s"  r.AsString (Format.float distLen)
        if wid <= distWid * -2.0 then FsExGeoException.Raise "FsEx.Geo.Rect3D.expandXYZ: the 3D Rectangle %s is too small to expand by negative distance distWid %s"  r.AsString (Format.float distWid)
        let x = r.Xaxis * (distLen / r.Length)
        let y = r.Yaxis * (distWid / r.Width )
        Rect3D(r.Origin-x-y, r.Xaxis+x*2., r.Yaxis+y*2.)

    /// Give PPlane and sizes
    static member createFromPlane (pl:PPlane,x,y) =
        Rect3D(pl.Origin, pl.Xaxis*x, pl.Yaxis*y)

    /// Give 2D Bounding Rect
    static member createFromBoundingRect (b:BRect) =
        Rect3D(b.MinPt.AsPnt, Vec.XAxis*b.Length, Vec.YAxis*b.Width)

    /// Move the 3D Rectangle by a vector.
    static member move (v:Vec) (r:Rect3D) =
        Rect3D(r.Origin + v, r.Xaxis, r.Yaxis)

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

    /// Translate along the local X-axis of the 3D Rectangle
    static member translateX (distX:float) (r:Rect3D) =
        let x = r.Xaxis
        let len = x.Length
        if len = zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Rect3D.translateX: rect.Xaxis is zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + x*(distX/len), x, r.Yaxis)

    /// Translate along the local Y-axis of the 3D Rectangle
    static member translateY (distY:float) (r:Rect3D) =
        let y = r.Yaxis
        let len = y.Length
        if len = zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Rect3D.translateY: rect.Yaxis is zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + y*(distY/len), r.Xaxis, y)

    /// Offset or Translate along the local Z-axis.
    /// The local Z-axis is calculated from cross product of X and Y axis of the 3D Rectangle.
    static member offset (offsetDistance :float) (r:Rect3D) =
        let z = Vec.cross(r.Xaxis,r.Yaxis)
        let len = z.Length
        if len = zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Rect3D.offset: rect is to small for offsetting zero length in Rect3D: %s" r.AsString
        Rect3D(r.Origin + z*(offsetDistance/len), r.Xaxis, r.Yaxis)

    /// Transform the 3D Rectangle by the given matrix.
    /// If the transformation includes a shear or projection
    /// that would make the edges non perpendicular it fails with an FsExGeoException.
    /// The returned 3D Rectangle is guaranteed to have orthogonal vectors.
    static member transform (m:Matrix) (r:Rect3D) =
        let o  = Pnt.transform m r.Origin
        let x = Vec.transform m r.Xaxis
        let y = Vec.transform m r.Yaxis
        if abs(x*y) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Rect3D.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        Rect3D(o,x,y)

