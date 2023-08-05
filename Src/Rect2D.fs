namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Util

open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar


/// An immutable 2D Rectangle with any rotation in 2D space.
/// Described by an Origin and two Edge vectors.
/// This implementation guarantees the 2D Rectangle to be always valid.
/// That means the X and Y axes are always perpendicular to each other.
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
type Rect2D =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The Origin Corner of the 2D Rectangle.
    [<DataMember>] val Origin: Pt

    /// The Edge vector representing the X-axis of the 2D Rectangle.
    /// Also called Length.
    [<DataMember>] val Xaxis: Vc

    /// The Edge vector representing the Y-axis of the 2D Rectangle.
    /// Also called Width.
    [<DataMember>] val Yaxis: Vc

    /// Unchecked Internal Constructor Only.
    /// Create a Parametrized Plane with X, Y and Z Direction.
    internal new (origin, axisX, axisY) = {Origin=origin; Xaxis=axisX; Yaxis=axisY}

    /// The size in X direction, same as member rect.SizeX.
    member inline r.Length = r.Xaxis.Length

    /// The size in X direction, same as member rect.Length.
    member inline r.SizeX = r.Xaxis.Length

    /// The size in Y direction, same as member rect.SizeY.
    member inline r.Width = r.Yaxis.Length

    /// The size in Y direction, same as member rect.Width.
    member inline r.SizeY = r.Yaxis.Length

    /// Nicely formatted string representation of the 2D Rectangle including its size.
    override r.ToString() =
        sprintf "Euclid.Rect2D %s x %s (Origin:%s| X-ax:%s| Y-ax:%s)"
            (Format.float r.Length) (Format.float r.Width)
            r.Origin.AsString r.Xaxis.AsString r.Yaxis.AsString


    /// Format the 2D Rectangle into string with nice floating point number formatting of X, Y and Z size only.
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

    /// Returns point 0 of the 2D rectangle. Same as member rect.Origin.
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


    /// Returns point 1 of the 2D rectangle.
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


    /// Returns point 2 of the 2D rectangle. Same as rect.FarCorner.
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

    /// Returns point 3 of the 2D rectangle.
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
    member inline r.EdgeX = Line2D (r.Origin, r.Origin + r.Xaxis)


    /// Returns the local X aligned far side as the 2D line from point 3 to 2 of the 2D rectangle.
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
    member inline r.EdgeXFar = 
        let s = r.Origin + r.Yaxis
        Line2D (s, s + r.Xaxis)
    

    /// Returns the local Y side as 2D line from point 0 to 3 of the 2D rectangle.
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
    member inline r.EdgeY = Line2D (r.Origin, r.Origin + r.Yaxis) 

    /// Returns the local Y aligned far side as 2D line from point 1 to 2 of the 2D rectangle.
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
    member inline r.EdgeYFar = 
        let s = r.Origin + r.Xaxis
        Line2D (s, s + r.Yaxis)


    /// Returns the diagonal 2D line from point 0 to 2 of the 2D rectangle.
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
    member inline r.DiagonalLine = Line2D (r.Origin, r.Origin + r.Yaxis + r.Xaxis) 
    

    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit =
        let len = r.Xaxis.Length
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect2D.XaxisUnit: rect Xaxis is too small for unitizing: %s" r.AsString
        r.Xaxis*(1./len)

    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit =
        let len = r.Yaxis.Length
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect2D.XaxisUnit: rect Yaxis is too small for unitizing: %s" r.AsString
        r.Yaxis*(1./len)
    

    /// Returns the diagonal vector of the 2D Rectangle.
    /// From Origin to FarCorner.
    member inline r.Diagonal = r.Xaxis + r.Yaxis

    /// Returns the center of the 2D Rectangle.
    member inline r.Center = r.Origin + r.Xaxis*0.5 + r.Yaxis*0.5


    /// Returns the Rectangle rotated 90 degrees clockwise around its center.
    /// The normal of the rectangle stays the same.
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
    member r.RotatedCW90 = Rect2D(r.Origin + r.Yaxis, -r.Yaxis, r.Xaxis)


    /// Returns the Rectangle rotated 180 degrees around its center.
    /// The normal of the rectangle stays the same.
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
    member r.Rotated180 = Rect2D(r.Origin + r.Yaxis + r.Xaxis, -r.Xaxis, -r.Yaxis)

    /// Returns the Rectangle rotated 90 degrees Counter-Clockwise around its center.
    /// The normal of the rectangle stays the same.
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
    member r.RotatedCCW90 = Rect2D(r.Origin + r.Xaxis, r.Yaxis, -r.Xaxis)


    /// Returns the 4 corners of the 2D Rectangle in Counter-Clockwise order, starting at Origin.
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
    member r.Corners :Pt[] =
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        [| p0  ; p1 ; p1 + r.Yaxis; p0 + r.Yaxis|]

    /// Returns the 4 corners of the 2D Rectangle als closed loop in Counter-Clockwise order, starting at Origin.
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
    member r.CornersLooped :Pt[] =
        let p0 = r.Origin
        let p1 = p0 + r.Xaxis
        [| p0  ; p1 ; p1 + r.Yaxis; p0 + r.Yaxis; p0|]


    /// Evaluate a X, Y and Z parameter of the the 2D Rectangle.
    ///  0.0, 0.0, 0.0 returns the Origin.
    ///  1.0, 1.0, 1.0 returns the FarCorner.
    member inline r.EvaluateAt (xParameter:float, yParameter:float) =
        r.Origin + r.Xaxis * xParameter + r.Yaxis * yParameter


    /// Calculates the volume of the 2D Rectangle.
    member inline r.Area =
        r.Xaxis.Length*r.Yaxis.Length


    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Checks if two 2D Rectangles are equal within tolerance.
    /// Does not recognize congruent rectangles with different rotation as equal.
    static member equals tol (a:Rect2D) (b:Rect2D) =
        let tt = tol*tol
        Pt.distanceSq a.Origin b.Origin < tt &&
        Vc.differenceSq a.Xaxis b.Xaxis < tt &&
        Vc.differenceSq a.Yaxis b.Yaxis < tt


    /// Returns the 2D Rectangle expanded by distance on all six sides.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (r:Rect2D) =
        let len = r.Length
        let wid = r.Width
        let d = dist * -2.0
        if len<=d || wid<=d  then
            EuclidException.Raise "Euclid.Rect2D.expand: the 2D Rectangle %s is too small to expand by negative distance %s"  r.AsString (Format.float dist)
        let x = r.Xaxis * (dist / len)
        let y = r.Yaxis * (dist / wid)
        Rect2D(r.Origin-x-y, r.Xaxis+x*2., r.Yaxis+y*2.)

    /// Returns the 2D Rectangle expanded by respective distances on all six sides.
    /// Does check for overflow if distance is negative and fails.
    /// distLen, distWid are for x, Y-axis respectively.
    static member expandXY distLen distWid (r:Rect2D) =
        let len = r.Length
        let wid = r.Width
        if len <= distLen * -2.0 then EuclidException.Raise "Euclid.Rect2D.expandXY: the 2D Rectangle %s is too small to expand by negative distance distLen %s"  r.AsString (Format.float distLen)
        if wid <= distWid * -2.0 then EuclidException.Raise "Euclid.Rect2D.expandXY: the 2D Rectangle %s is too small to expand by negative distance distWid %s"  r.AsString (Format.float distWid)
        let x = r.Xaxis * (distLen / r.Length)
        let y = r.Yaxis * (distWid / r.Width )
        Rect2D(r.Origin-x-y, r.Xaxis+x*2., r.Yaxis+y*2.)

    /// Create a 2D Rectangle from the origin point and X-edge and Y edge.
    /// Checks for counter-clockwise order of x and y.
    /// Checks for perpendicularity.
    /// Fails on vectors shorter than 1e-9.
    static member create(origin, x:Vc, y:Vc) =
        if Vc.cross(x, y) = 0.0 then 
            let yr = x.Rotate90CCW
            if yr*y < 0.0 then
                    EuclidException.Raise "Euclid.Rect2D.create(origin, x:Vc, y:Vc): Vc 'y' has the wrong orientation : %s. 'x':%s " y.AsString x.AsString 
            Rect2D(origin, x, y)
        else            
            let lx = x.Length
            if lx < 1e-9 then EuclidException.Raise "Euclid.Rect2D.create(origin, x:Vc, y:Vc): Vc 'x' is too short: %s. 'z':%s " x.AsString y.AsString
            let ly = y.Length
            if ly < 1e-9 then EuclidException.Raise "Euclid.Rect2D.create(origin, x:Vc, y:Vc): Vc 'y' is too short: %s. 'x':%s " y.AsString x.AsString            
            let xu = x * (1.0 / lx)
            let yu = y * (1.0 / ly)
            let d = xu*yu
            if float -Cosine.``0.5`` < d && d  < float Cosine.``0.5`` then //x.IsPerpendicularTo( y, Cosine.``0.05``)
                let yr = x.Rotate90CCW * (ly/lx)
                if yr*y < 0.0 then
                    EuclidException.Raise "Euclid.Rect2D.create(origin, x:Vc, y:Vc): Vc 'y' has the wrong orientation : %s. 'x':%s " y.AsString x.AsString   
                Rect2D(origin, x, yr)
            else
                EuclidException.Raise "Euclid.Rect2D.create(origin, x:Vc, y:Vc): the X-axis %s and Y-axis %s are not perpendicular"  x.AsString y.AsString

    /// Creates a Bounding Rectangle from a origin point, the X vector and Y size.
    static member create (origin:Pt, x:Vc, sizeY ) =
        if sizeY < 0. then EuclidException.Raise "Euclid.Rect2D.create(origin:Pt, x:Vc, sizeY ) sizeY is negative: %g, x is: %O, origin: %O"  sizeY  x.AsString  origin.AsString
        let y = x.Rotate90CCW * sizeY
        Rect2D(origin, x, y)

    /// Creates a Bounding Rectangle from a origin point, the X direction, the total X and Y size.
    static member create (origin:Pt, directionX:UnitVc, sizeX, sizeY ) =
        if sizeX < 0. then EuclidException.Raise "Euclid.Rect2D.create(origin:Pt, directionX:UnitVc, sizeX, sizeY ) sizeX is negative: %g, sizeY is: %g, origin: %O"  sizeX sizeY  origin.AsString
        if sizeY < 0. then EuclidException.Raise "Euclid.Rect2D.create(origin:Pt, directionX:UnitVc, sizeX, sizeY ) sizeY is negative: %g, sizeX is: %g, origin: %O"  sizeY sizeX  origin.AsString
        let x = directionX * sizeX
        let y = directionX.Rotate90CCW * sizeY
        Rect2D(origin, x, y)
    

    /// Create a 2D Rectangle from the origin point and X-edge and Y edge.
    /// Does not check for counter-clockwise order of x and y.
    /// Does not check for perpendicularity.
    static member createUnchecked (origin, x:Vc, y:Vc) =
        Rect2D(origin, x, y)
            
    /// Create a 2D Rectangle from a Line and a  right and left Offset.
    /// The left offset is in the direction of the future Y-axis.
    static member createFromLine(line:Line2D, offRight, offLeft) =
        if -offRight >= offLeft then EuclidException.Raise "Euclid.Rect2D.createFromLine: flipped Rect2D : minus offRight %g must be smaller than offLeft %g .  " offRight  offLeft 
        let x = line.Vector
        let len = x.Length
        if len < 1e-9 then EuclidException.Raise "Euclid.Rect2D.createFromLine: Line too short: %s.  " line.AsString
        let y = x.Rotate90CCW
        let o = line.From - y * (offRight / len)
        let y = y * ((offLeft + offRight) / len)
        Rect2D(o, x, y)   

    /// Give 2D Bounding Rect.
    static member createFromBRect (b:BRect) =
        Rect2D(b.MinPt, Vc.Xaxis*b.Length, Vc.Yaxis*b.Width)

    /// Creates a Bounding Rectangle from a center point, the X direction, the total X and Y size.
    static member createFromCenter (center:Pt, directionX:UnitVc, sizeX, sizeY ) =
        if sizeX < 0. then EuclidException.Raise "Euclid.Rect2D.createFromCenter(center:Pt, directionX:UnitVc, sizeX, sizeY ) sizeX is negative: %g, sizeY is: %g, center: %O"  sizeX sizeY  center.AsString
        if sizeY < 0. then EuclidException.Raise "Euclid.Rect2D.createFromCenter(center:Pt, directionX:UnitVc, sizeX, sizeY ) sizeY is negative: %g, sizeX is: %g, center: %O"  sizeY sizeX  center.AsString
        let x = directionX * sizeX
        let y = directionX.Rotate90CCW * sizeY
        Rect2D(center - x * 0.5 - y * 0.5, x, y)

    /// Creates a Bounding Rectangle from a center point, the X vector and Y size.
    static member createFromCenter (center:Pt, x:Vc, sizeY ) =
        if sizeY < 0. then EuclidException.Raise "Euclid.Rect2D.createFromCenter(center:Pt, x:Vc, sizeY ) sizeY is negative: %g, x is: %O, center: %O"  sizeY  x.AsString  center.AsString
        let y = x.Rotate90CCW * sizeY
        Rect2D(center - x * 0.5 - y * 0.5, x, y)
    
    /// Creates a Bounding Rectangle from three points, the Origin, the point in X Axis direction
    /// and a point for the length in Y Axis direction. If Y-point is not perpendicular to X-point, 
    /// the Y-point will be projected on the perpendicular Y axis.
    /// Fails if the point Y is on right side of the X-axis.
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
    static member createThreePoints (origin:Pt, x:Pt, y:Pt ) =
        let xv = x - origin
        if xv.LengthSq < 1e-9 then EuclidException.Raise "Euclid.Rect2D.createThreePoints: X-Point %s too close to origin: %s." origin.AsString x.AsString 
        let yv = y - origin
        if yv.LengthSq < 1e-9 then EuclidException.Raise "Euclid.Rect2D.createThreePoints: Y-Point %s too close to origin: %s." origin.AsString y.AsString
        let y0 = xv.Rotate90CCW
        if y0 * yv < 0. then EuclidException.Raise "Euclid.Rect2D.createThreePoints: Y-Point %s is on right side but should be on the left of X-Point %s." origin.AsString y.AsString
        let yu = y0.Unitized
        let y = yu * (yv * yu) // get the y point projected on the y axis
        Rect2D(origin, xv, y)

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
    static member flip (r:Rect2D) = Rect2D(r.Origin + r.Xaxis + r.Yaxis, -r.Yaxis, -r.Xaxis)

    /// Translate along the local X-axis of the 2D Rectangle.
    static member translateX (distX:float) (r:Rect2D) =
        let x = r.Xaxis
        let len = x.Length
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect2D.translateX: rect.Xaxis is zero length in Rect2D: %s" r.AsString
        Rect2D(r.Origin + x*(distX/len), x, r.Yaxis)

    /// Translate along the local Y-axis of the 2D Rectangle.
    static member translateY (distY:float) (r:Rect2D) =
        let y = r.Yaxis
        let len = y.Length
        if len = zeroLengthTolerance then EuclidException.Raise "Euclid.Rect2D.translateY: rect.Yaxis is zero length in Rect2D: %s" r.AsString
        Rect2D(r.Origin + y*(distY/len), r.Xaxis, y)

    /// Translate by a 2D vector.(Same as Rect2D.move)
    static member translate (v:Vc) (r:Rect2D) =
        Rect2D(r.Origin + v, r.Xaxis, r.Yaxis)    

    /// Translate by a 2D vector.(Same as Rect2D.translate)
    static member move (v:Vc) (r:Rect2D) =
        Rect2D(r.Origin + v, r.Xaxis, r.Yaxis)  

    /// Rotation of a Rect2D.
    static member rotate (rot:Rotation2D) (rect:Rect2D) = 
        Rect2D(Pt.rotateBy rot rect.Origin, Vc.rotateBy rot rect.Xaxis, Vc.rotateBy rot rect.Yaxis)

    /// Rotation of a Rect2D. around a given Center.
    static member rotateOn (cen:Pt) (rot:Rotation2D) (rect:Rect2D) = 
        Rect2D(Pt.rotateWithCenterBy cen rot rect.Origin, Vc.rotateBy rot rect.Xaxis, Vc.rotateBy rot rect.Yaxis)
        
