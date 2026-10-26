namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid
open System.Collections.Generic
open EuclidErrors
#if !FABLE_COMPILER
open System.Text.Json.Serialization
#endif


/// <summary>
/// A struct containing a 2D Origin point and two 2D Edge vectors,
/// representing an immutable 2D rectangle with any rotation in 2D space.
/// This implementation guarantees the 2D rectangle to be always valid.
/// That means the X and Y axes are always perpendicular to each other.
/// However the length of one of these axes might still be zero.
/// <code>
///   local
///   Y-axis
///   ^
///   |
///   |             2
/// 3 +------------+
///   |            |
///   |            |
///   |            |
///   |            |
///   |            |       local
///   +------------+-----> X-axis
///  0-Origin       1
/// </code>
/// </summary>
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields, for Newtonsoft.Json
#if !FABLE_COMPILER
[<JsonConverter(typeof<Rect2DJsonConverter>)>]
#endif
type Rect2D =

    

    /// The X coordinate of the Origin Corner of the 2D Rectangle.
    [<DataMember>] val public OriginX: float

    /// The Y coordinate of the Origin Corner of the 2D Rectangle.
    [<DataMember>] val public OriginY: float

    /// The X component of the Edge vector representing the X-axis of the 2D Rectangle.
    [<DataMember>] val public XAxisX: float

    /// The Y component of the Edge vector representing the X-axis of the 2D Rectangle.
    [<DataMember>] val public XAxisY: float

    /// The X component of the Edge vector representing the Y-axis of the 2D Rectangle.
    [<DataMember>] val public YAxisX: float

    /// The Y component of the Edge vector representing the Y-axis of the 2D Rectangle.
    [<DataMember>] val public YAxisY: float

    /// Unchecked Internal Constructor Only.
    /// Create a 2D rectangle from origin coordinates and X- and Y-axis vector components.
    [<Obsolete("Unsafe internal constructor, doesn't check the input (unless compiled in DEBUG mode), but must be public for inlining. So marked Obsolete instead.") >]
    new (originX:float, originY:float, axisXX:float, axisXY:float, axisYX:float, axisYY:float) =
        #if DEBUG || CHECKED_EUCLID // CHECKED_EUCLID so checks can still be enabled when using with Fable release mode
            let lenX = sqrt(axisXX*axisXX + axisXY*axisXY)
            let lenY = sqrt(axisYX*axisYX + axisYY*axisYY)
            if isTooSmall lenX then  failTooSmall2 "Rect2D() axisX" (Vc(axisXX, axisXY)) (Vc(axisYX, axisYY))
            if isTooSmall lenY then  failTooSmall2 "Rect2D() axisY" (Vc(axisYX, axisYY)) (Vc(axisXX, axisXY))
            // scale the dot-product tolerance by the axis lengths so the check is a relative angle tolerance (independent of the rectangle size):
            if abs (axisXX*axisYX + axisXY*axisYY) > lenX*lenY * 1e-9 then fail2 $"Rect2D(): X-axis and Y-axis are not perpendicular" (Vc(axisXX, axisXY)) (Vc(axisYX, axisYY))
            if isNegative(axisXX*axisYY - axisXY*axisYX) then fail2 $"Rect2D(): X-axis and Y-axis are not counter-clockwise" (Vc(axisXX, axisXY)) (Vc(axisYX, axisYY))
        #endif
            {OriginX=originX; OriginY=originY; XAxisX=axisXX; XAxisY=axisXY; YAxisX=axisYX; YAxisY=axisYY}


    /// Create a 2D rectangle from the origin point and X-edge and Y edge.
    /// Does not check for counter-clockwise order of x and y.
    /// Does not check for perpendicularity.
    static member inline createUnchecked (originX:float, originY:float, xaxisX:float, xaxisY:float, yaxisX:float, yaxisY:float) : Rect2D =
        #nowarn "44"
        Rect2D(originX, originY, xaxisX, xaxisY, yaxisX, yaxisY)
        #warnon "44" // re-enable warning for obsolete usage

    /// Create a 2D rectangle from the origin point and X-edge and Y edge.
    /// Does not check for counter-clockwise order of x and y.
    /// Does not check for perpendicularity.
    static member inline createFromVcsUnchecked (origin:Pt, x:Vc, y:Vc) : Rect2D =
        Rect2D.createUnchecked(origin.X, origin.Y, x.X, x.Y, y.X, y.Y)

    /// Creates a 2D Point from r.OriginX and r.OriginY
    member r.Origin : Pt =
        Pt(r.OriginX, r.OriginY)

    /// Creates a 2D Point from r.OriginX and r.OriginY
    static member inline origin (r:Rect2D) : Pt =
        r.Origin

    /// Creates a 2D Vector from r.YAxisX and r.YAxisY
    member r.YAxis : Vc =
        Vc(r.YAxisX, r.YAxisY)

    /// Creates a 2D Vector from r.XAxisX and r.XAxisY
    member r.XAxis : Vc =
        Vc(r.XAxisX, r.XAxisY)

    /// Creates a 2D Vector from r.XAxisX and r.XAxisY
    static member inline xAxis (r:Rect2D) : Vc =
        r.XAxis

    /// Creates a 2D Vector from r.YAxisX and r.YAxisY
    static member inline yAxis (r:Rect2D) : Vc =
        r.YAxis

    /// The size in X direction
    member inline r.SizeX : float =
        sqrt(r.XAxisX*r.XAxisX + r.XAxisY * r.XAxisY)

    /// Returns the size in X direction.
    static member inline sizeX (r:Rect2D) : float =
        r.SizeX

   /// The squared size in X direction
    member inline r.SizeXSq : float =
        r.XAxisX*r.XAxisX + r.XAxisY*r.XAxisY

    /// Returns the squared size in X direction.
    static member inline sizeXSq (r:Rect2D) : float =
        r.SizeXSq

    /// The size in Y direction
    member inline r.SizeY : float =
        sqrt(r.YAxisX*r.YAxisX + r.YAxisY*r.YAxisY)

    /// The squared size in Y direction
    member inline r.SizeYSq : float =
        r.YAxisX*r.YAxisX + r.YAxisY*r.YAxisY

    /// Returns the size in Y direction.
    static member inline sizeY (r:Rect2D) : float =
        r.SizeY

    /// Returns the squared size in Y direction.
    static member inline sizeYSq (r:Rect2D) : float =
        r.SizeYSq

    /// Nicely formatted string representation of the 2D rectangle including its size.
    override r.ToString() : string =
        let sizeX = Format.float r.SizeX
        let sizeY = Format.float r.SizeY
        let origin = $"X={Format.float r.OriginX}|Y={Format.float r.OriginY}"
        let xAxis = $"X={Format.float r.XAxisX}|Y={Format.float r.XAxisY}"
        let yAxis = $"X={Format.float r.YAxisX}|Y={Format.float r.YAxisY}"
        $"Euclid.Rect2D {sizeX} x {sizeY} (Origin:{origin}| X-ax:{xAxis}|Y-ax:{yAxis})"


    /// Format the 2D rectangle into string with nice floating point number formatting of X and Y size only.
    /// But without type name as in v.ToString()
    member r.AsString : string =
        let sizeX = Format.float r.SizeX
        let sizeY = Format.float r.SizeY
        $"%s{sizeX} x %s{sizeY}"

    /// Returns a concise formatted string representation of the 2D rectangle.
    static member inline asString (r:Rect2D) : string =
        r.AsString

    /// Format Rect2D into an F# code string that can be used to recreate the rectangle.
    member r.AsFSharpCode : string =
        let f = Format.floatAsFSharpCode
        $"Rect2D.createUnchecked({f r.OriginX}, {f r.OriginY}, {f r.XAxisX}, {f r.XAxisY}, {f r.YAxisX}, {f r.YAxisY})"

    /// Returns an F# code string that recreates the 2D rectangle.
    static member inline asFSharpCode (r:Rect2D) : string =
        r.AsFSharpCode

    /// Creates a unitized version of the local X-axis.
    member inline r.XAxisUnit : UnitVc =
        let x = r.XAxisX
        let y = r.XAxisY
        let sqLen = x*x + y*y
        if isTooTinySq sqLen then
            failTooSmall "Rect2D.XaxisUnit" r
        let f = 1.0 / sqrt sqLen
        UnitVc.createUnchecked(x*f, y*f)

    /// Returns the unitized local X-axis of the 2D rectangle.
    static member inline xAxisUnit (r:Rect2D) : UnitVc =
        r.XAxisUnit

    /// Creates a unitized version of the local Y-axis.
    member inline r.YAxisUnit : UnitVc =
        let x = r.YAxisX
        let y = r.YAxisY
        let sqLen = x*x + y*y
        if isTooTinySq sqLen then
            failTooSmall "Rect2D.YaxisUnit" r
        let f = 1.0 / sqrt sqLen
        UnitVc.createUnchecked(x*f, y*f)

    /// Returns the unitized local Y-axis of the 2D rectangle.
    static member inline yAxisUnit (r:Rect2D) : UnitVc =
        r.YAxisUnit

    /// Returns the diagonal vector of the 2D Rectangle.
    /// From Origin to FarCorner.
    member inline r.Diagonal : Vc =
        Vc(r.XAxisX + r.YAxisX, r.XAxisY + r.YAxisY)

    /// Returns the diagonal vector of the 2D rectangle.
    static member inline diagonal (r:Rect2D) : Vc =
        r.Diagonal

    /// Returns the center of the 2D Rectangle.
    member inline r.Center : Pt =
        Pt(r.OriginX + (r.XAxisX + r.YAxisX)*0.5,
           r.OriginY + (r.XAxisY + r.YAxisY)*0.5)

    /// Returns the center point of the 2D rectangle.
    static member inline center (r:Rect2D) : Pt =
        r.Center

    /// Evaluate an X and Y parameter of the 2D rectangle.
    ///  0.0, 0.0 returns the Origin.
    ///  1.0, 1.0 returns the FarCorner.
    member inline r.EvaluateAt (xParameter:float, yParameter:float) : Pt =
        Pt(r.OriginX + r.XAxisX*xParameter + r.YAxisX*yParameter,
           r.OriginY + r.XAxisY*xParameter + r.YAxisY*yParameter)

    /// Evaluates X and Y parameters on the 2D rectangle and returns a point.
    static member inline evaluateAt (xParameter:float) (yParameter:float) (r:Rect2D) : Pt =
        r.EvaluateAt(xParameter, yParameter)

    /// Evaluate a point at X and Y distance on the respective axes of the 2D Rectangle.
    member inline r.EvaluateDistance (xDistance:float, yDistance:float) : Pt =
        let lx = sqrt(r.XAxisX*r.XAxisX + r.XAxisY*r.XAxisY)
        let ly = sqrt(r.YAxisX*r.YAxisX + r.YAxisY*r.YAxisY)
        if isTooTiny (lx) then failTooSmall "Rect2D.EvaluateDistance Xaxis" r
        if isTooTiny (ly) then failTooSmall "Rect2D.EvaluateDistance Yaxis" r
        let xf = xDistance/lx
        let yf = yDistance/ly
        Pt(r.OriginX + r.XAxisX*xf + r.YAxisX*yf,
           r.OriginY + r.XAxisY*xf + r.YAxisY*yf)

    /// Evaluates a point by X and Y distances along local axes of the 2D rectangle.
    static member inline evaluateDistance (xDistance:float) (yDistance:float) (r:Rect2D) : Pt =
        r.EvaluateDistance(xDistance, yDistance)

    /// <summary>Returns the X and Y parameters of the closest point on the 2D rectangle to a given point.
    /// If the parameters are outside the range of 0.0 to 1.0, the given point is outside the rectangle.
    /// Fails if the X- or Y-axis of the rectangle is shorter than 1e-6.</summary>
    /// <param name="p">The point to find the closest parameters for.</param>
    /// <returns>A tuple of the X and Y parameters, not clamped to the range 0.0 to 1.0.</returns>
    member r.ClosestParameters (p:Pt) : float*float =
        let vx = p.X - r.OriginX
        let vy = p.Y - r.OriginY
        let xLenSq = r.SizeXSq
        let yLenSq = r.SizeYSq
        if isTooSmallSq xLenSq || isTooSmallSq yLenSq then
            failTooSmall "Rect2D.ClosestParameters: rect Xaxis or Yaxis" r
        let xParam = (vx*r.XAxisX + vy*r.XAxisY) / xLenSq
        let yParam = (vx*r.YAxisX + vy*r.YAxisY) / yLenSq
        xParam, yParam

    /// <summary>Returns the X and Y parameters of the closest point on the 2D rectangle to a given point.
    /// If the parameters are outside the range of 0.0 to 1.0, the given point is outside the rectangle.
    /// Fails if the X- or Y-axis of the rectangle is shorter than 1e-6.</summary>
    /// <param name="p">The point to find the closest parameters for.</param>
    /// <param name="r">The 2D rectangle.</param>
    /// <returns>A tuple of the X and Y parameters, not clamped to the range 0.0 to 1.0.</returns>
    static member inline closestParameters (p:Pt) (r:Rect2D) : float*float =
        r.ClosestParameters(p)

    /// <summary>Returns the closest point on the 2D rectangle to a given point.
    /// For a point inside the rectangle, this is the point itself, up to floating-point rounding.
    /// A point outside the rectangle is clamped onto its edge.
    /// Fails if the X- or Y-axis of the rectangle is shorter than 1e-6.</summary>
    /// <param name="p">The point to find the closest point for.</param>
    /// <returns>The closest point on or inside the 2D rectangle.</returns>
    member r.ClosestPoint (p:Pt) : Pt =
        let vx = p.X - r.OriginX
        let vy = p.Y - r.OriginY
        let xLenSq = r.SizeXSq
        let yLenSq = r.SizeYSq
        if isTooSmallSq xLenSq || isTooSmallSq yLenSq then
            failTooSmall "Rect2D.ClosestPoint: rect Xaxis or Yaxis" r
        let xParam = (vx*r.XAxisX + vy*r.XAxisY) / xLenSq
        let yParam = (vx*r.YAxisX + vy*r.YAxisY) / yLenSq
        r.EvaluateAt(UtilEuclid.clamp01 xParam, UtilEuclid.clamp01 yParam)

    /// <summary>Returns the closest point on the 2D rectangle to a given point.
    /// For a point inside the rectangle, this is the point itself, up to floating-point rounding.
    /// A point outside the rectangle is clamped onto its edge.
    /// Fails if the X- or Y-axis of the rectangle is shorter than 1e-6.</summary>
    /// <param name="p">The point to find the closest point for.</param>
    /// <param name="r">The 2D rectangle.</param>
    /// <returns>The closest point on or inside the 2D rectangle.</returns>
    static member inline closestPoint (p:Pt) (r:Rect2D) : Pt =
        r.ClosestPoint(p)

    /// Calculates the area of the 2D Rectangle.
    member inline r.Area : float =
        sqrt(r.XAxisX*r.XAxisX + r.XAxisY*r.XAxisY) * sqrt(r.YAxisX*r.YAxisX + r.YAxisY*r.YAxisY)

    /// Returns the area of the 2D rectangle.
    static member inline area (r:Rect2D) : float =
        r.Area

    /// Scales the 2D rectangle by a given factor.
    /// Scale center is World Origin 0,0
    member inline r.Scale (factor:float) : Rect2D =
        Rect2D.createUnchecked(
                r.OriginX * factor,
                r.OriginY * factor,
                r.XAxisX * factor,
                r.XAxisY * factor,
                r.YAxisX * factor,
                r.YAxisY * factor)

    /// Scales the 2D rectangle by a given factor.
    /// Scale center is World Origin 0,0
    static member inline scale (factor:float) (r:Rect2D) : Rect2D =
        Rect2D.createUnchecked(
                r.OriginX * factor,
                r.OriginY * factor,
                r.XAxisX * factor,
                r.XAxisY * factor,
                r.YAxisX * factor,
                r.YAxisY * factor)

    /// <summary>Scales the 2D rectangle by a given factor around a given center point.</summary>
    /// <param name="cen">The center point of the scaling.</param>
    /// <param name="factor">The scale factor.</param>
    /// <returns>The scaled 2D rectangle.</returns>
    member inline l.ScaleOn (cen:Pt) (factor:float) : Rect2D =
        let cx = cen.X
        let cy = cen.Y
        Rect2D.createUnchecked(
            cx + (l.OriginX - cx) * factor,
            cy + (l.OriginY - cy) * factor,
            l.XAxisX * factor,
            l.XAxisY * factor,
            l.YAxisX * factor,
            l.YAxisY * factor
        )

    /// <summary>Scales a 2D rectangle by a given factor around a given center point.</summary>
    /// <param name="cen">The center point of the scaling.</param>
    /// <param name="factor">The scale factor.</param>
    /// <param name="r">The 2D rectangle to scale.</param>
    /// <returns>The scaled 2D rectangle.</returns>
    static member inline scaleOn (cen:Pt) (factor:float) (r:Rect2D) : Rect2D =
        r.ScaleOn cen factor

    /// Returns the longest edge of the Rect2D.
    member inline b.LongestEdge : float =
        let x = b.XAxisX*b.XAxisX + b.XAxisY*b.XAxisY
        let y = b.YAxisX*b.YAxisX + b.YAxisY*b.YAxisY
        sqrt  (max x y)

    /// Returns the longest edge length of the 2D rectangle.
    static member inline longestEdge (r:Rect2D) : float =
        r.LongestEdge

    /// Returns the shortest edge of the Rect2D.
    member inline b.ShortestEdge : float =
        let x = b.XAxisX*b.XAxisX + b.XAxisY*b.XAxisY
        let y = b.YAxisX*b.YAxisX + b.YAxisY*b.YAxisY
        sqrt  (min x y)

    /// Returns the shortest edge length of the 2D rectangle.
    static member inline shortestEdge (r:Rect2D) : float =
        r.ShortestEdge

    /// Returns the square length of longest edge of the Rect2D.
    member inline b.LongestEdgeSq : float =
        let x = b.XAxisX*b.XAxisX + b.XAxisY*b.XAxisY
        let y = b.YAxisX*b.YAxisX + b.YAxisY*b.YAxisY
        max x y

    /// Returns the squared length of the longest edge of the 2D rectangle.
    static member inline longestEdgeSq (r:Rect2D) : float =
        r.LongestEdgeSq

    /// Returns the square length of shortest edge of the Rect2D.
    member inline b.ShortestEdgeSq : float =
        let x = b.XAxisX*b.XAxisX + b.XAxisY*b.XAxisY
        let y = b.YAxisX*b.YAxisX + b.YAxisY*b.YAxisY
        min x y

    /// Returns the squared length of the shortest edge of the 2D rectangle.
    static member inline shortestEdgeSq (r:Rect2D) : float =
        r.ShortestEdgeSq

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// Same as b.IsPoint.
    member inline b.IsZero : bool =
        isTooTinySq (b.XAxisX*b.XAxisX + b.XAxisY*b.XAxisY) &&
        isTooTinySq (b.YAxisX*b.YAxisX + b.YAxisY*b.YAxisY)

    /// Returns TRUE if all sides are below zero-length tolerance.
    /// Same as Rect2D.isPoint.
    static member inline isZero (r:Rect2D) : bool =
        r.IsZero

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// Same as b.IsZero.
    member inline b.IsPoint : bool =
        isTooTinySq (b.XAxisX*b.XAxisX + b.XAxisY*b.XAxisY) &&
        isTooTinySq (b.YAxisX*b.YAxisX + b.YAxisY*b.YAxisY)

    /// Returns TRUE if the 2D rectangle degenerates to a point.
    /// Same as Rect2D.isZero.
    static member inline isPoint (r:Rect2D) : bool =
        r.IsPoint

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1 or 2.
    member inline b.CountZeroSides : int =
        countTooTinySqOrNaN    (b.XAxisX*b.XAxisX + b.XAxisY*b.XAxisY)
        +  countTooTinySqOrNaN (b.YAxisX*b.YAxisX + b.YAxisY*b.YAxisY)

    /// Returns the number of sides below zero-length tolerance.
    static member inline countZeroSides (r:Rect2D) : int =
        r.CountZeroSides

    /// Tests if one of the X and Y axes is smaller than the zeroLength tolerance.
    member inline b.IsLine : bool =
        b.CountZeroSides = 1

    /// Returns TRUE if exactly one side is below zero-length tolerance.
    static member inline isLine (r:Rect2D) : bool =
        r.IsLine

    /// Tests if neither the X nor Y-axis is smaller than the zeroLength tolerance.
    /// Same as .HasArea
    member inline b.IsValid : bool =
        b.CountZeroSides = 0

    /// Returns TRUE if both sides are above zero-length tolerance.
    static member inline isValid (r:Rect2D) : bool =
        r.IsValid

    /// Tests if neither the X nor Y-axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasArea : bool =
        b.CountZeroSides = 0

    /// Returns TRUE if the 2D rectangle has area.
    static member inline hasArea (r:Rect2D) : bool =
        r.HasArea

    /// Check for point containment in the 2D Rectangle.
    /// By doing 4 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the rectangle is considered inside.
    member r.ContainsXY(x:float, y:float) : bool =
        let vx = x - r.OriginX
        let vy = y - r.OriginY
        vx*r.XAxisX + vy*r.XAxisY >= 0.
        &&
        vx*r.YAxisX + vy*r.YAxisY >= 0.
        &&
        (x - r.OriginX - r.YAxisX)*r.YAxisX + (y - r.OriginY - r.YAxisY)*r.YAxisY <= 0.
        &&
        (x - r.OriginX - r.XAxisX)*r.XAxisX + (y - r.OriginY - r.XAxisY)*r.XAxisY <= 0.

    /// Check for point containment in the 2D Rectangle.
    /// By doing 4 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the rectangle is considered inside.
    member r.Contains(p:Pt) : bool =
        r.ContainsXY(p.X, p.Y)

    /// Check for point containment in the 2D Rectangle.
    /// By doing 4 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the rectangle is considered inside.
    static member inline containsXY (x:float) (y:float) (r:Rect2D)  : bool =
        r.ContainsXY (x, y)

    /// Check for point containment in the 2D Rectangle.
    /// By doing 4 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the rectangle is considered inside.
    static member inline contains (p:Pt) (r:Rect2D)  : bool =
        r.Contains p




    //#endregion
    //#region Create

    /// Create a 2D rectangle from an x-edge, an y-edge and the origin point.
    /// Fails if x and y are not in counter-clockwise order.
    /// Fails if x and y are not perpendicular.
    /// Fails on vectors shorter than 1e-6.
    static member createFromVcs (x:Vc) (y:Vc) (origin:Pt) : Rect2D =
        let xLenSq = x.X*x.X + x.Y*x.Y
        let yLenSq = y.X*y.X + y.Y*y.Y
        if isTooSmallSq xLenSq  then failTooSmall2 "Rect2D.createFromVcs x" x y
        if isTooSmallSq yLenSq  then failTooSmall2 "Rect2D.createFromVcs y" y x
        // scale the dot-product tolerance by the axis lengths so the check is a relative angle tolerance (independent of the rectangle size):
        if abs (x.X*y.X + x.Y*y.Y) > sqrt(xLenSq*yLenSq) * 1e-9 then fail2 $"Rect2D.createFromVcs: X-axis and Y-axis are not perpendicular" x y
        if isNegative(x.X*y.Y - x.Y*y.X)then fail2 $"Rect2D.createFromVcs: X-axis and Y-axis are not counter-clockwise" x y
        Rect2D.createUnchecked(origin.X, origin.Y, x.X, x.Y, y.X, y.Y)

    /// Creates a 2D rectangle from the Y size, the X vector and an origin point.
    /// The length of the X vector defines the X size, sizeY defines the absolute Y size.
    /// Fails on negative Y size.
    static member createFromVc (sizeY:float) (x:Vc) (origin:Pt) : Rect2D =
        if isNegative sizeY then fail1 $"Rect2D.createFromVc(): sizeY cannot be negative" sizeY
        if x.IsTinySq 1e-12 then failTooSmall "Rect2D.createFromVc: x vector is too small" x
        let f = sizeY / sqrt(x.X*x.X + x.Y*x.Y) // x Vc need to be unitized
        let yX = -x.Y * f // x and y swaped to rotate 90 degree
        let yY =  x.X * f
        Rect2D.createUnchecked(origin.X, origin.Y, x.X, x.Y, yX, yY)

    /// Creates a 2D rectangle from the size in X and Y direction, the X direction as unit-vector and an origin point.
    /// Fails on negative sizes.
    static member createFromUnitVc (sizeX:float) (sizeY:float) (directionX:UnitVc) (origin:Pt) : Rect2D =
        if isNegative sizeX then fail2 $"Rect2D.createFromUnitVc(): sizeX cannot be negative" sizeX sizeY
        if isNegative sizeY then fail2 $"Rect2D.createFromUnitVc(): sizeY cannot be negative" sizeY sizeX
        Rect2D.createUnchecked(origin.X, origin.Y, directionX.X*sizeX, directionX.Y*sizeX, -directionX.Y*sizeY, directionX.X*sizeY)

    /// Create a 2D rectangle from a right and left offset and a 2D line.
    /// The left offset is in the direction of the future Y-axis.
    static member createFromLine (offRight:float) (offLeft:float) (line:Line2D) : Rect2D =
        if -offRight >= offLeft then
            fail $"Rect2D.createFromLine: flipped Rect2D : minus offRight {offRight} must be smaller than offLeft {offLeft}."
        let xX = line.VectorX
        let xY = line.VectorY
        let len = sqrt(xX*xX + xY*xY)
        if isTooSmall len then failTooSmall "Rect2D.createFromLine" line
        let rightFac = offRight / len
        let yFac = (offLeft + offRight) / len
        Rect2D.createUnchecked(
            line.FromX + xY*rightFac,
            line.FromY - xX*rightFac,
            xX,
            xY,
            -xY*yFac,
            xX*yFac)

    /// Create a 2D rectangle from an axis-aligned 2D Bounds
    /// given as minX, minY, maxX, maxY.
    static member createFromBounds (minX:float, minY:float, maxX:float, maxY:float)  : Rect2D =
        Rect2D.createUnchecked(minX, minY, maxX - minX, 0.0, 0.0, maxY - minY)

    /// Creates a 2D rectangle from the X and the Y size, the X direction and a center point.
    /// Fails on negative sizes.
    static member createCenteredFromUnitVc (sizeX:float) (sizeY:float) (directionX:UnitVc) (center:Pt) : Rect2D =
        if isNegative sizeX then fail $"Rect2D.createCenteredFromUnitVc(..) sizeX cannot be negative: {sizeX}, sizeY is: {sizeY}, center: {center.AsString}"
        if isNegative sizeY then fail $"Rect2D.createCenteredFromUnitVc(..) sizeY cannot be negative: {sizeY}, sizeX is: {sizeX}, center: {center.AsString}"
        let xX = directionX.X * sizeX
        let xY = directionX.Y * sizeX
        let yX = -directionX.Y * sizeY
        let yY = directionX.X * sizeY
        Rect2D.createUnchecked(center.X - (xX + yX)*0.5, center.Y - (xY + yY)*0.5, xX, xY, yX, yY)

    /// Creates a 2D rectangle from the Y size, the X vector and a center point.
    /// The length of the X vector defines the X size, sizeY defines the absolute Y size.
    /// Fails on negative Y size.
    static member createCenteredFromVc (sizeY:float) (x:Vc) (center:Pt) : Rect2D =
        if isNegative sizeY then fail $"Rect2D.createCenteredFromVc(..) sizeY cannot be negative: {sizeY}, x is: {x.AsString}, center: {center.AsString}"
        if x.IsTinySq 1e-12  then failTooSmall "Rect2D.createCenteredFromVc: x vector is too small" x
        let f = sizeY / sqrt(x.X*x.X + x.Y*x.Y) // x Vc need to be unitized
        let yX = -x.Y * f // x and y swapped to rotate 90 degree
        let yY =  x.X * f
        Rect2D.createUnchecked(center.X - (x.X + yX)*0.5, center.Y - (x.Y + yY)*0.5, x.X, x.Y, yX, yY)


    // #endregion
    // #region static members

    /// Checks if two 2D rectangles are equal within tolerance.
    /// Does not recognize congruent rectangles with different rotation as equal.
    /// The tolerance applies to each coordinate of the origin and of the axis vectors separately, not to the Euclidean distance between them.
    /// Use Rect2D.equalsExact to check for an exact match.
    static member equalsWithin (tol:float) (a:Rect2D) (b:Rect2D)  : bool =
        abs (a.OriginX - b.OriginX) <= tol && //TODO raise an exception if the tolerance is negative ?
        abs (a.OriginY - b.OriginY) <= tol &&
        abs (a.XAxisX -  b.XAxisX ) <= tol &&
        abs (a.XAxisY -  b.XAxisY ) <= tol &&
        abs (a.YAxisX -  b.YAxisX ) <= tol &&
        abs (a.YAxisY -  b.YAxisY ) <= tol

    /// Checks if two 2D rectangles are exactly equal.
    /// Compares each field with the = operator, so a NaN value is never equal.
    static member inline equalsExact (a:Rect2D) (b:Rect2D) : bool =
        a.OriginX = b.OriginX &&
        a.OriginY = b.OriginY &&
        a.XAxisX = b.XAxisX &&
        a.XAxisY = b.XAxisY &&
        a.YAxisX = b.YAxisX &&
        a.YAxisY = b.YAxisY

    /// Check if two 2D rectangles are not equal within a given tolerance.
    /// The tolerance applies to each coordinate of the origin and of the axis vectors separately, not to the Euclidean distance between them.
    static member notEqualsWithin (tol:float) (a:Rect2D) (b:Rect2D)  : bool =
        abs (a.OriginX - b.OriginX) > tol || //TODO raise an exception if the tolerance is negative ?
        abs (a.OriginY - b.OriginY) > tol ||
        abs (a.XAxisX -  b.XAxisX ) > tol ||
        abs (a.XAxisY -  b.XAxisY ) > tol ||
        abs (a.YAxisX -  b.YAxisX ) > tol ||
        abs (a.YAxisY -  b.YAxisY ) > tol

    /// Returns the 2D rectangle expanded by distance on all four sides.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (r:Rect2D)  : Rect2D =
        let siX = r.SizeX
        let siY = r.SizeY
        let d = dist * -2.0
        if siX <= d || siY <= d  then
            fail $"Rect2D.expand: the 2D Rectangle {r.AsString} is too small to expand by negative distance {dist}"
        let xf = dist / siX
        let yf = dist / siY
        let xX = r.XAxisX * xf
        let xY = r.XAxisY * xf
        let yX = r.YAxisX * yf
        let yY = r.YAxisY * yf
        Rect2D.createUnchecked(
            r.OriginX - xX - yX,
            r.OriginY - xY - yY,
            r.XAxisX + xX * 2.,
            r.XAxisY + xY * 2.,
            r.YAxisX + yX * 2.,
            r.YAxisY + yY * 2.)

    /// Returns the 2D rectangle expanded by respective distances on all four sides.
    /// Does check for overflow if distance is negative and fails.
    /// distX, distY are for the local X- and Y-axis respectively.
    static member expandXY distX distY (r:Rect2D) : Rect2D =
        let siX = r.SizeX
        let siY = r.SizeY
        if siX <= distX * -2.0 then fail $"Rect2D.expandXY: the 2D Rectangle {r.AsString} is too small to expand by negative distance distX {distX}"
        if siY <= distY * -2.0 then fail $"Rect2D.expandXY: the 2D Rectangle {r.AsString} is too small to expand by negative distance distY {distY}"
        let xf = distX / siX
        let yf = distY / siY
        let xX = r.XAxisX * xf
        let xY = r.XAxisY * xf
        let yX = r.YAxisX * yf
        let yY = r.YAxisY * yf
        Rect2D.createUnchecked(
            r.OriginX - xX - yX,
            r.OriginY - xY - yY,
            r.XAxisX + xX * 2.,
            r.XAxisY + xY * 2.,
            r.YAxisX + yX * 2.,
            r.YAxisY + yY * 2.)

    /// Returns the 2D-rectangle expanded by a relative factor on all four sides.
    /// Values between 0.0 and 1.0 shrink the rectangle.
    /// Values larger than 1.0 expand the rectangle.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRel factor (r:Rect2D)  : Rect2D =
        if factor < 0.0  then
            fail $"Rect2D.expandRel: a negative factor {factor} is not allowed for expanding the 2D-rectangle {r.AsString}"
        let xX = r.XAxisX * factor
        let xY = r.XAxisY * factor
        let yX = r.YAxisX * factor
        let yY = r.YAxisY * factor
        Rect2D.createUnchecked(
            r.OriginX + (r.XAxisX - xX)*0.5 + (r.YAxisX - yX)*0.5,
            r.OriginY + (r.XAxisY - xY)*0.5 + (r.YAxisY - yY)*0.5,
            xX,
            xY,
            yX,
            yY)

    /// Returns the 2D-rectangle expanded by a relative factor on all four sides.
    /// Values between 0.0 and 1.0 shrink the rectangle.
    /// Values larger than 1.0 expand the rectangle.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRelXY factorX factorY (r:Rect2D)  : Rect2D =
        if factorX < 0.0  then
            fail $"Rect2D.expandRelXY: a negative factor {factorX} is not allowed for expanding the 2D-rectangle {r.AsString}"
        if factorY < 0.0  then
            fail $"Rect2D.expandRelXY: a negative factor {factorY} is not allowed for expanding the 2D-rectangle {r.AsString}"
        let xX = r.XAxisX * factorX
        let xY = r.XAxisY * factorX
        let yX = r.YAxisX * factorY
        let yY = r.YAxisY * factorY
        Rect2D.createUnchecked(
            r.OriginX + (r.XAxisX - xX)*0.5 + (r.YAxisX - yX)*0.5,
            r.OriginY + (r.XAxisY - xY)*0.5 + (r.YAxisY - yY)*0.5,
            xX,
            xY,
            yX,
            yY)

    /// <summary>Creates a 2D rectangle from three points. Fails if points are too close to each other or all collinear.
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y-axis.
    /// If the y-point is on the left side of the X-axis the origin will be at point 0, X at point 1.
    /// If the y-point is on the right side of the X-axis, the X-axis will be reversed. the origin will be at point x, and the end of the X-axis at the origin.
    /// E.G if called with points (origin=3,x=2,y=0) the origin will be at 2, X at 3, and y at 1.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    static member createFrom3Pts (origin:Pt) (xPt:Pt) (yPt:Pt) : Rect2D =
        let xX = xPt.X - origin.X
        let xY = xPt.Y - origin.Y
        let xLenSq = xX*xX + xY*xY
        if isTooSmallSq xLenSq  then
            fail $"Rect2D.createFrom3Pts: X-Point {xPt.AsString} too close to origin: X=%s{Format.float xX}|Y=%s{Format.float xY}."
        let yX = yPt.X - origin.X
        let yY = yPt.Y - origin.Y
        let yLenSq = yX*yX + yY*yY
        if isTooSmallSq yLenSq  then
            fail $"Rect2D.createFrom3Pts: Y-Point {yPt.AsString} too close to origin: X=%s{Format.float yX}|Y=%s{Format.float yY}."
        let xLen = sqrt xLenSq
        let yuX = -xY / xLen
        let yuY =  xX / xLen
        //if y0 * yv < 0. then EuclidException.Raise "Euclid.Rect2D.createFrom3Pts: Y-Point %s is on right side but should be on the left of X-Point %s." origin.AsString y.AsString
        let dot = yuX*yX + yuY*yY
        if isTooSmall (abs dot) then
            fail $"Rect2D.createFrom3Pts: Y-Point X=%s{Format.float yX}|Y=%s{Format.float yY} is too close to Xaxis."
        let yrX = yuX * dot // get the y point projected on the y axis
        let yrY = yuY * dot
        if dot > 0. then
            Rect2D.createUnchecked(origin.X, origin.Y, xX, xY, yrX, yrY) //point is on left side of X-axis
        else
           //Rect2D.createUnchecked(origin + y, xv, -y) //alternative valid result: If the point Y is on the right side of the X-axis the origin will be at point 3, X at point 2.
           Rect2D.createUnchecked(xPt.X, xPt.Y, -xX, -xY, yrX, yrY) //the origin will be at point x, and the end of the x-Axis at the origin.

    /// <summary>Tries to create a 2D rectangle from three points. Returns ValueNone if points are too close to each other or all collinear.
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y-axis.
    /// If the y-point is on the left side of the X-axis the origin will be at point 0, X at point 1.
    /// If the y-point is on the right side of the X-axis, the X-axis will be reversed. the origin will be at point x, and the end of the X-axis at the origin.
    /// E.G if called with points (origin=3,x=2,y=0) the origin will be at 2, X at 3, and y at 1.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    static member tryCreateFrom3Pts (origin:Pt) (xPt:Pt) (yPt:Pt) : Rect2D voption =
        let xX = xPt.X - origin.X
        let xY = xPt.Y - origin.Y
        let xLenSq = xX*xX + xY*xY
        if isTooSmallSq xLenSq  then ValueNone
        else
            let yX = yPt.X - origin.X
            let yY = yPt.Y - origin.Y
            if isTooSmallSq (yX*yX + yY*yY)  then ValueNone
            else
                let xLen = sqrt xLenSq
                let yuX = -xY / xLen
                let yuY =  xX / xLen
                //if y0 * yv < 0. then EuclidException.Raise "Euclid.Rect2D.createThreePoints: Y-Point %s is on right side but should be on the left of X-Point %s." origin.AsString y.AsString
                let dot = yuX*yX + yuY*yY
                if isTooSmall (abs dot) then ValueNone
                else
                    let yrX = yuX * dot // get the y point projected on the y axis
                    let yrY = yuY * dot
                    if dot > 0. then
                        ValueSome <| Rect2D.createUnchecked(origin.X, origin.Y, xX, xY, yrX, yrY) //point is on left side of X-axis
                    else
                        //Rect2D.createUnchecked(origin + y, xv, -y) //alternative valid result: If the point Y is on the right side of the X-axis the origin will be at point 3, X at point 2.
                        ValueSome <| Rect2D.createUnchecked(xPt.X, xPt.Y, -xX, -xY, yrX, yrY) //the origin will be at point x, and the end of the x-Axis at the origin.

    /// Finds the oriented bounding rectangle of a set of points.
    /// The orientation of the X-axis is defined by the direction vector.
    static member createFromVcAndPts (dirX:Vc) (pts:IList<Pt>) :Rect2D =
        let dirXLenSq = dirX.X*dirX.X + dirX.Y*dirX.Y
        if isTooSmallSq dirXLenSq then failTooSmall "Rect2D.createFromVcAndPts dirX" dirX
        if pts.Count < 2 then fail $"Rect2D.createFromVcAndPts: cannot create a 2D rectangle from {pts.Count} points"
        let p0 = pts.[0]
        let yDirX = -dirX.Y
        let yDirY = dirX.X
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        for i = 0 to pts.Count-1 do
            let p = pts.[i]
            let relX = p.X - p0.X
            let relY = p.Y - p0.Y
            let px = (dirX.X*relX + dirX.Y*relY) / dirXLenSq
            if px < minX then
                minX <- px
            if px > maxX then
                maxX <- px
            let py = (yDirX*relX + yDirY*relY) / dirXLenSq
            if py < minY then
                minY <- py
            if py > maxY then
                maxY <- py
        let xLen = maxX - minX
        let yLen = maxY - minY
        Rect2D.createUnchecked(
            p0.X + dirX.X*minX + yDirX*minY,
            p0.Y + dirX.Y*minX + yDirY*minY,
            dirX.X*xLen,
            dirX.Y*xLen,
            yDirX*yLen,
            yDirY*yLen)

    /// Creates a new 2D rectangle( = oriented bounding rectangle ) to contain the projections of all given points.
    /// But not the corners of the reference rectangle.
    /// Keeps the same X- and Y-axis orientation as the input rectangle.
    static member fitToPoints (pts:IList<Pt>) (refRect:Rect2D) : Rect2D =
        let xLen = sqrt(refRect.XAxisX*refRect.XAxisX + refRect.XAxisY*refRect.XAxisY)
        if isTooTiny xLen then failTooSmall "Rect2D.fitToPoints: Xaxis" (Vc(refRect.XAxisX, refRect.XAxisY))
        let yLen = sqrt(refRect.YAxisX*refRect.YAxisX + refRect.YAxisY*refRect.YAxisY)
        if isTooTiny yLen then failTooSmall "Rect2D.fitToPoints: Yaxis" (Vc(refRect.YAxisX, refRect.YAxisY))
        let xX = refRect.XAxisX / xLen
        let xY = refRect.XAxisY / xLen
        let yX = refRect.YAxisX / yLen
        let yY = refRect.YAxisY / yLen
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        for i = 0 to pts.Count-1 do
            let vX = pts.[i].X - refRect.OriginX
            let vY = pts.[i].Y - refRect.OriginY
            let dotX = vX*xX + vY*xY
            minX <- min minX dotX
            maxX <- max maxX dotX
            let dotY = vX*yX + vY*yY
            minY <- min minY dotY
            maxY <- max maxY dotY
        let sizeX = maxX - minX
        let sizeY = maxY - minY
        Rect2D.createUnchecked(
            refRect.OriginX + xX*minX + yX*minY,
            refRect.OriginY + xY*minX + yY*minY,
            xX*sizeX,
            xY*sizeX,
            yX*sizeY,
            yY*sizeY)

    /// <summary>Creates a new 2D rectangle( = oriented bounding rectangle ) to contain the projections of all given points.
    /// But not the corners of the reference rectangle.
    /// Keeps the same X- and Y-axis orientation as the input rectangle.</summary>
    /// <param name="xys">The points given as a flat list of coordinates [x0, y0, x1, y1, ...].</param>
    /// <param name="refRect">The reference rectangle to use for orientation.</param>
    static member fitToPointsXY (xys:IList<float>) (refRect:Rect2D) : Rect2D =
        if xys.Count % 2 <> 0 then fail $"Rect2D.fitToPointsXY: the list of coordinates must have an even number of elements, but has {xys.Count} elements."
        let xLen = sqrt(refRect.XAxisX*refRect.XAxisX + refRect.XAxisY*refRect.XAxisY)
        if isTooTiny xLen then failTooSmall "Rect2D.fitToPointsXY: Xaxis" (Vc(refRect.XAxisX, refRect.XAxisY))
        let yLen = sqrt(refRect.YAxisX*refRect.YAxisX + refRect.YAxisY*refRect.YAxisY)
        if isTooTiny yLen then failTooSmall "Rect2D.fitToPointsXY: Yaxis" (Vc(refRect.YAxisX, refRect.YAxisY))
        let xX = refRect.XAxisX / xLen
        let xY = refRect.XAxisY / xLen
        let yX = refRect.YAxisX / yLen
        let yY = refRect.YAxisY / yLen
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        let mutable i = 0
        let cnt = xys.Count
        while i < cnt do
            let vX = xys.[i] - refRect.OriginX
            let vY = xys.[i+1] - refRect.OriginY
            let dotX = vX*xX + vY*xY
            minX <- min minX dotX
            maxX <- max maxX dotX
            let dotY = vX*yX + vY*yY
            minY <- min minY dotY
            maxY <- max maxY dotY
            i <- i + 2
        let sizeX = maxX - minX
        let sizeY = maxY - minY
        Rect2D.createUnchecked(
            refRect.OriginX + xX*minX + yX*minY,
            refRect.OriginY + xY*minX + yY*minY,
            xX*sizeX,
            xY*sizeX,
            yX*sizeY,
            yY*sizeY)

    /// Translate along the local X-axis of the 2D Rectangle.
    static member moveAlongLocalX (distX:float) (r:Rect2D) : Rect2D =
        let len = sqrt(r.XAxisX*r.XAxisX + r.XAxisY*r.XAxisY)
        if isTooTiny len then failTooSmall $"Rect2D.moveAlongLocalX: Xaxis" r
        let f = distX / len
        Rect2D.createUnchecked(r.OriginX + r.XAxisX*f, r.OriginY + r.XAxisY*f, r.XAxisX, r.XAxisY, r.YAxisX, r.YAxisY)

    /// Translate along the local Y-axis of the 2D Rectangle.
    static member moveAlongLocalY (distY:float) (r:Rect2D) : Rect2D =
        let len = sqrt(r.YAxisX*r.YAxisX + r.YAxisY*r.YAxisY)
        if isTooTiny len then failTooSmall $"Rect2D.moveAlongLocalY: Yaxis" r
        let f = distY / len
        Rect2D.createUnchecked(r.OriginX + r.YAxisX*f, r.OriginY + r.YAxisY*f, r.XAxisX, r.XAxisY, r.YAxisX, r.YAxisY)

    /// Translate by a 2D vector in world space. (Same as Rect2D.Move)
    static member move (v:Vc) (r:Rect2D)  : Rect2D =
        Rect2D.createUnchecked(r.OriginX + v.X, r.OriginY + v.Y, r.XAxisX, r.XAxisY, r.YAxisX, r.YAxisY)

    /// Returns a 2D rectangle moved by a vector in world space. Same as Rect2D.move.
    member inline r.Move (v:Vc) : Rect2D =
        Rect2D.createUnchecked(r.OriginX + v.X, r.OriginY + v.Y, r.XAxisX, r.XAxisY, r.YAxisX, r.YAxisY)

    /// Returns a 2D rectangle moved by a given distance along the world X-axis.
    /// This is not the rectangle's own (possibly rotated) local X-axis, use Rect2D.moveAlongLocalX for that.
    member inline r.MoveX (distance:float) : Rect2D =
        Rect2D.createUnchecked(r.OriginX + distance, r.OriginY, r.XAxisX, r.XAxisY, r.YAxisX, r.YAxisY)

    /// Returns a 2D rectangle moved by a given distance along the world X-axis.
    /// This is not the rectangle's own (possibly rotated) local X-axis, use Rect2D.moveAlongLocalX for that.
    static member inline moveX (distance:float) (r:Rect2D) : Rect2D =
        Rect2D.createUnchecked(r.OriginX + distance, r.OriginY, r.XAxisX, r.XAxisY, r.YAxisX, r.YAxisY)

    /// Returns a 2D rectangle moved by a given distance along the world Y-axis.
    /// This is not the rectangle's own (possibly rotated) local Y-axis, use Rect2D.moveAlongLocalY for that.
    member inline r.MoveY (distance:float) : Rect2D =
        Rect2D.createUnchecked(r.OriginX, r.OriginY + distance, r.XAxisX, r.XAxisY, r.YAxisX, r.YAxisY)

    /// Returns a 2D rectangle moved by a given distance along the world Y-axis.
    /// This is not the rectangle's own (possibly rotated) local Y-axis, use Rect2D.moveAlongLocalY for that.
    static member inline moveY (distance:float) (r:Rect2D) : Rect2D =
        Rect2D.createUnchecked(r.OriginX, r.OriginY + distance, r.XAxisX, r.XAxisY, r.YAxisX, r.YAxisY)

    /// Rotation of a Rect2D.
    static member rotate (rot:Rotation2D) (rect:Rect2D) : Rect2D =
        Rect2D.createUnchecked(
            rot.Cos*rect.OriginX - rot.Sin*rect.OriginY,
            rot.Sin*rect.OriginX + rot.Cos*rect.OriginY,
            rot.Cos*rect.XAxisX - rot.Sin*rect.XAxisY,
            rot.Sin*rect.XAxisX + rot.Cos*rect.XAxisY,
            rot.Cos*rect.YAxisX - rot.Sin*rect.YAxisY,
            rot.Sin*rect.YAxisX + rot.Cos*rect.YAxisY)

    /// <summary>Rotates a 2D rectangle counter-clockwise around a given center point by a Rotation2D.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="rot">The rotation to apply.</param>
    /// <param name="rect">The 2D rectangle to rotate.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    static member rotateAround (cen:Pt) (rot:Rotation2D) (rect:Rect2D) : Rect2D =
        let ox = rect.OriginX - cen.X
        let oy = rect.OriginY - cen.Y
        Rect2D.createUnchecked(
            rot.Cos*ox - rot.Sin*oy + cen.X,
            rot.Sin*ox + rot.Cos*oy + cen.Y,
            rot.Cos*rect.XAxisX - rot.Sin*rect.XAxisY,
            rot.Sin*rect.XAxisX + rot.Cos*rect.XAxisY,
            rot.Cos*rect.YAxisX - rot.Sin*rect.YAxisY,
            rot.Sin*rect.YAxisX + rot.Cos*rect.YAxisY)

    /// Rotates the 2D rectangle around the World Origin. Same as Rect2D.rotate.
    member inline r.Rotate (rot:Rotation2D) : Rect2D =
        Rect2D.rotate rot r

    /// <summary>Rotates the 2D rectangle counter-clockwise around the World Origin by an angle in degrees.</summary>
    /// <param name="angDegree">The rotation angle in degrees.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    /// <remarks>For better performance precompute the Rotation2D struct and rotate with this.Rotate(rotation2D).</remarks>
    member inline r.RotateDegrees (angDegree:float) : Rect2D =
        Rect2D.rotate (Rotation2D.createFromDegrees angDegree) r

    /// <summary>Rotates a 2D rectangle counter-clockwise around the World Origin by an angle in degrees.</summary>
    /// <param name="angDegree">The rotation angle in degrees.</param>
    /// <param name="r">The 2D rectangle to rotate.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    /// <remarks>For better performance precompute the Rotation2D struct and rotate with Rect2D.rotate.</remarks>
    static member inline rotateDegrees (angDegree:float) (r:Rect2D) : Rect2D =
        Rect2D.rotate (Rotation2D.createFromDegrees angDegree) r

    /// <summary>Rotates the 2D rectangle counter-clockwise around the World Origin by an angle in radians.</summary>
    /// <param name="angRadians">The rotation angle in radians.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    /// <remarks>For better performance precompute the Rotation2D struct and rotate with this.Rotate(rotation2D).</remarks>
    member inline r.RotateRadians (angRadians:float) : Rect2D =
        Rect2D.rotate (Rotation2D.createFromRadians angRadians) r

    /// <summary>Rotates a 2D rectangle counter-clockwise around the World Origin by an angle in radians.</summary>
    /// <param name="angRadians">The rotation angle in radians.</param>
    /// <param name="r">The 2D rectangle to rotate.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    /// <remarks>For better performance precompute the Rotation2D struct and rotate with Rect2D.rotate.</remarks>
    static member inline rotateRadians (angRadians:float) (r:Rect2D) : Rect2D =
        Rect2D.rotate (Rotation2D.createFromRadians angRadians) r

    /// <summary>Rotates the 2D rectangle counter-clockwise around a given center point by a Rotation2D. Same as Rect2D.rotateAround.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="rot">The rotation to apply.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    member inline r.RotateAround (cen:Pt) (rot:Rotation2D) : Rect2D =
        Rect2D.rotateAround cen rot r

    /// <summary>Rotates the 2D rectangle counter-clockwise around a given center point by an angle in degrees.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="angDegree">The rotation angle in degrees.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    member inline r.RotateDegreesAround (cen:Pt) (angDegree:float) : Rect2D =
        Rect2D.rotateAround cen (Rotation2D.createFromDegrees angDegree) r

    /// <summary>Rotates a 2D rectangle counter-clockwise around a given center point by an angle in degrees.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="angDegree">The rotation angle in degrees.</param>
    /// <param name="r">The 2D rectangle to rotate.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    static member inline rotateDegreesAround (cen:Pt) (angDegree:float) (r:Rect2D) : Rect2D =
        Rect2D.rotateAround cen (Rotation2D.createFromDegrees angDegree) r

    /// <summary>Rotates the 2D rectangle counter-clockwise around a given center point by an angle in radians.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="angRadians">The rotation angle in radians.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    member inline r.RotateRadiansAround (cen:Pt) (angRadians:float) : Rect2D =
        Rect2D.rotateAround cen (Rotation2D.createFromRadians angRadians) r

    /// <summary>Rotates a 2D rectangle counter-clockwise around a given center point by an angle in radians.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="angRadians">The rotation angle in radians.</param>
    /// <param name="r">The 2D rectangle to rotate.</param>
    /// <returns>The rotated 2D rectangle.</returns>
    static member inline rotateRadiansAround (cen:Pt) (angRadians:float) (r:Rect2D) : Rect2D =
        Rect2D.rotateAround cen (Rotation2D.createFromRadians angRadians) r


    /// <summary>Intersects an infinite ray (Line2D extended infinitely in both directions) with a 2D rectangle.
    /// Uses the slab intersection method in the rectangle's local coordinate system.
    /// Returns ValueNone if the ray does not intersect the rectangle or if the ray direction is too short.
    /// Returns ValueSome with the entry and exit parameters on the ray if it intersects.
    /// A parameter of 0.0 corresponds to the ray's From point, 1.0 to its To point.</summary>
    /// <param name="ray">The 2D line considered infinite, to intersect with the rectangle.</param>
    /// <param name="rect">The 2D rectangle to intersect with the line.</param>
    /// <returns>A ValueSome with a tuple of (tEntry, tExit) with the entry and exit parameters on the ray.
    /// or ValueNone if
    ///  - The ray does not intersect the rectangle
    ///  - The ray length is shorter than 1e-6
    /// </returns>
    static member tryIntersectRay (ray:Line2D) (rect:Rect2D) : voption<float * float> =
        // Transform ray to the rectangle's local coordinate system
        let rayDirX = ray.VectorX
        let rayDirY = ray.VectorY
        let rayDirLenSq = rayDirX*rayDirX + rayDirY*rayDirY
        if isTooSmallSq rayDirLenSq then
            ValueNone // Ray direction too short
        else
            let rayOriginX = ray.FromX - rect.OriginX
            let rayOriginY = ray.FromY - rect.OriginY

            // Get rectangle axes squared lengths
            let xLenSq = rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY
            let yLenSq = rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY

            // Initialize tMin and tMax
            let mutable tMin = Double.MinValue
            let mutable tMax = Double.MaxValue

            // Process X-axis slab
            if isTooTinySq xLenSq then
                // Rectangle is flat in X direction, check if ray origin is within slab
                let originDotX = rayOriginX*rect.XAxisX + rayOriginY*rect.XAxisY
                if originDotX < 0.0 || originDotX > xLenSq then
                    tMin <- Double.MaxValue // Force ValueNone return
            else
                let dirDotX = rayDirX*rect.XAxisX + rayDirY*rect.XAxisY
                let originDotX = rayOriginX*rect.XAxisX + rayOriginY*rect.XAxisY
                if abs dirDotX < 1e-12 then
                    // Ray is parallel to X slab
                    if originDotX < 0.0 || originDotX > xLenSq then
                        tMin <- Double.MaxValue // Force ValueNone return
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
                    let originDotY = rayOriginX*rect.YAxisX + rayOriginY*rect.YAxisY
                    if originDotY < 0.0 || originDotY > yLenSq then
                        tMin <- Double.MaxValue
                else
                    let dirDotY = rayDirX*rect.YAxisX + rayDirY*rect.YAxisY
                    let originDotY = rayOriginX*rect.YAxisX + rayOriginY*rect.YAxisY
                    if abs dirDotY < 1e-12 then
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

            if tMin <= tMax then
                ValueSome(tMin, tMax)
            else
                ValueNone

    /// <summary>Intersects an infinite ray (Line2D extended infinitely in both directions) with a 2D rectangle.
    /// Same as Rect2D.tryIntersectRay, but fails if the ray does not intersect the rectangle or if the ray direction is too short.
    /// A parameter of 0.0 corresponds to the ray's From point, 1.0 to its To point.</summary>
    /// <param name="ray">The 2D line considered infinite, to intersect with the rectangle.</param>
    /// <param name="rect">The 2D rectangle to intersect with the line.</param>
    /// <returns>The entry and exit parameters on the ray (tEntry, tExit).</returns>
    static member intersectRay (ray:Line2D) (rect:Rect2D) : float * float =
        match Rect2D.tryIntersectRay ray rect with
        | ValueSome ts -> ts
        | ValueNone -> fail $"Rect2D.intersectRay: the ray does not intersect the rectangle or is too short:{nl}{ray}{nl}{rect}"

    /// Intersects or clips a 2D line with a 2D rectangle.
    /// Returns the clipped line segment of the intersection if it exists.
    /// Same as Rect2D.tryClipLine.
    static member tryIntersectLine (line:Line2D) (rect:Rect2D) : voption<Line2D> =
        match Rect2D.tryIntersectRay line rect with
        | ValueNone -> ValueNone
        | ValueSome(tEntry, tExit) ->
            let p = UtilEuclid.clamp01 tEntry
            let t = UtilEuclid.clamp01 tExit
            if p <> t then // if the line is outside the box p and t get both clamped to 0.0 or 1.0
                ValueSome(line.SubLine(p,t))
            else
                ValueNone

    /// Intersects or clips a 2D line with a 2D rectangle.
    /// Returns the clipped line segment of the intersection.
    /// Fails if the line does not intersect the rectangle.
    /// Use Rect2D.tryIntersectLine to get ValueNone instead.
    /// Same as Rect2D.clipLine.
    static member intersectLine (line:Line2D) (rect:Rect2D) : Line2D =
        match Rect2D.tryIntersectLine line rect with
        | ValueSome ln -> ln
        | ValueNone -> fail $"Rect2D.intersectLine: the line does not intersect the rectangle:{nl}{line}{nl}{rect}"

    /// Intersects or clips a 2D line with a 2D rectangle.
    /// Returns the clipped line segment of the intersection if it exists.
    /// Same as Rect2D.tryIntersectLine.
    static member inline tryClipLine (line:Line2D) (rect:Rect2D) : voption<Line2D> =
        Rect2D.tryIntersectLine line rect

    /// Intersects or clips a 2D line with a 2D rectangle.
    /// Returns the clipped line segment of the intersection.
    /// Fails if the line does not intersect the rectangle.
    /// Use Rect2D.tryClipLine to get ValueNone instead.
    /// Same as Rect2D.intersectLine.
    static member inline clipLine (line:Line2D) (rect:Rect2D) : Line2D =
        Rect2D.intersectLine line rect

    // #endregion
    // #region offset

    /// Offset a Rect2D inwards by a given distance.
    /// A negative distance will offset outwards.
    /// Fails if the distance is larger than half the size of the rectangle.
    static member offset dist (rect:Rect2D) : Rect2D =
        let xl = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let yl = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)
        if xl < dist*2.0 ||yl < dist*2.0 then
            fail $"Rect2D.offset: the 2D Rectangle {rect.AsString} is too small to offset by distance {Format.float dist}"
        let xf = dist / xl
        let yf = dist / yl
        let xX = rect.XAxisX * xf
        let xY = rect.XAxisY * xf
        let yX = rect.YAxisX * yf
        let yY = rect.YAxisY * yf
        Rect2D.createUnchecked(
            rect.OriginX + xX + yX,
            rect.OriginY + xY + yY,
            rect.XAxisX - xX*2.0,
            rect.XAxisY - xY*2.0,
            rect.YAxisX - yX*2.0,
            rect.YAxisY - yY*2.0)

    /// <summary>Offset a Rect2D inwards by four distances.
    /// Negative distances will offset outwards.
    /// The distance array is for Edge01, Edge12, Edge23, and Edge30 respectively.
    /// Fails if the distance is larger than half the size of the rectangle.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    static member offsetVariable (dist:float[]) (rect:Rect2D) : Rect2D =
        if dist.Length <> 4 then fail $"Rect2D.offsetVariable: the distance array must have 4 elements, but has {dist.Length}"
        let xl = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let yl = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)
        if xl < dist.[1]+dist.[3] ||yl < dist.[0]+dist.[2] then
            fail $"Rect2D.offsetVariable: the 2D Rectangle {rect.AsString} is too small to offset by distances [|{Format.float dist.[0]};{Format.float dist.[1]};{Format.float dist.[2]};{Format.float dist.[3]}|]"
        let x0f = dist.[3] / xl
        let x1f = dist.[1] / xl
        let y0f = dist.[0] / yl
        let y1f = dist.[2] / yl
        let x0X = rect.XAxisX * x0f
        let x0Y = rect.XAxisY * x0f
        let x1X = rect.XAxisX * x1f
        let x1Y = rect.XAxisY * x1f
        let y0X = rect.YAxisX * y0f
        let y0Y = rect.YAxisY * y0f
        let y1X = rect.YAxisX * y1f
        let y1Y = rect.YAxisY * y1f
        Rect2D.createUnchecked(
            rect.OriginX + x0X + y0X,
            rect.OriginY + x0Y + y0Y,
            rect.XAxisX - x0X - x1X,
            rect.XAxisY - x0Y - x1Y,
            rect.YAxisX - y0X - y1X,
            rect.YAxisY - y0Y - y1Y)

    ///<summary>Offsets a local Rect2D at one of the four corners.</summary>
    ///<param name="rect">The 2D rectangle</param>
    ///<param name="corner">The Index of the corner to offset
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></param>
    ///<param name="xOffset">The local offset distances in x direction. (Applies to the y side.) Positive values offset to the inside of the rectangle, negative values will offset outwards.</param>
    ///<param name="yOffset">The local offset distances in y direction. (Applies to the x side.) Positive values offset to the inside of the rectangle, negative values will offset outwards.</param>
    ///<param name="xWidth">The width (or size in x direction) that will be added to the current offset.</param>
    ///<param name="yHeight">The height (or size in y direction) that will be added to the current offset.</param>
    ///<returns>A new 2D Rectangle. It will always have the same X- and Y-axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetCorner (rect:Rect2D, corner:int, xOffset:float, yOffset:float, xWidth:float, yHeight:float) : Rect2D =
        let xl = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let yl = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)
        if isTooTiny xl then failTooSmall "Rect2D.offsetCorner: Xaxis" rect
        if isTooTiny yl then failTooSmall "Rect2D.offsetCorner: Yaxis" rect
        let xvf = xWidth/xl
        let yvf = yHeight/yl
        let xvX = rect.XAxisX * xvf
        let xvY = rect.XAxisY * xvf
        let yvX = rect.YAxisX * yvf
        let yvY = rect.YAxisY * yvf
        let inline create xDist yDist =
            let xf = xDist / xl
            let yf = yDist / yl
            Rect2D.createUnchecked(
                rect.OriginX + rect.XAxisX*xf + rect.YAxisX*yf,
                rect.OriginY + rect.XAxisY*xf + rect.YAxisY*yf,
                xvX,
                xvY,
                yvX,
                yvY)
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
            fail $"Rect2D.offsetCorner: corner {corner} out of range 0..3" |> unbox // unbox to make type checker happy

    ///<summary>Offsets a local Rect2D at one of the four edges.</summary>
    ///<param name="rect">The 2D rectangle</param>
    ///<param name="edgeIdx">The Index of the edge to offset
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |      2
    ///   +------------+
    ///   |            |
    ///   |            |
    ///  3|            |1
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///         0
    /// </code></param>
    ///<param name="offEdge">The local offset distances parallel to the edge.</param>
    ///<param name="width">The width of the new rectangle. This is like the second offset to be applied to the first offset of offEdge</param>
    ///<param name="offStart">The local offset distances perpendicular to the edge at the start.</param>
    ///<param name="offEnd">The local offset distances perpendicular to the edge at the end.</param>
    ///<returns>A new 2D Rectangle. It will always have the same X- and Y-axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetEdge (rect:Rect2D, edgeIdx:int, offEdge:float, width:float, offStart:float, offEnd:float) : Rect2D =
        let lx = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let ly = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)

        let inline xLenX d = rect.XAxisX * (d / lx)
        let inline xLenY d = rect.XAxisY * (d / lx)
        let inline yLenX d = rect.YAxisX * (d / ly)
        let inline yLenY d = rect.YAxisY * (d / ly)

        let inline checkX d = if d > 1e-6 then d else failRect2DOffsetEdge(offStart, offEnd, lx, edgeIdx, d) |> unbox
        let inline checkY d = if d > 1e-6 then d else failRect2DOffsetEdge(offStart, offEnd, ly, edgeIdx, d) |> unbox

        let inline create xx yy xDist yDist =
            Rect2D.createUnchecked(
                rect.OriginX + xLenX xx + yLenX yy,
                rect.OriginY + xLenY xx + yLenY yy,
                xLenX xDist,
                xLenY xDist,
                yLenX yDist,
                yLenY yDist)

        if isTooTiny lx then failTooSmall "Rect2D.offsetEdge: Xaxis" rect
        if isTooTiny ly then failTooSmall "Rect2D.offsetEdge: Yaxis" rect

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
                fail $"Rect2D.offsetEdge: edgeIdx {edgeIdx} out of range 0..3" |> unbox // unbox to make type checker happy

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
                fail $"Rect2D.offsetEdge: edgeIdx {edgeIdx} out of range 0..3" |> unbox // unbox to make type checker happy
        else
            fail $"Rect2D.offsetEdge: width {width} must be more than 1e-6" |> unbox // unbox to make type checker happy

    // #endregion
    // #region subDivide

    /// Divides a 2D rectangle into a grid of sub-rectangles. The sub-rectangles are returned as an array of arrays.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 2D Rectangle.
    /// The returned array has xCount elements, each element is an array of yCount sub-rectangles.
    static member subDivide (xCount:int) (yCount:int) (xGap:float) (yGap:float) (rect:Rect2D) : Rect2D array array =
        if xCount <= 0 ||yCount <= 0 then
            fail $"Rect2D.subDivide: xCount {xCount} and yCount {yCount} must be 1 or more"
        let xl = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let yl = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)
        let lx1 = (xl - xGap * float (xCount-1) )/ float xCount
        let ly1 = (yl - yGap * float (yCount-1) )/ float yCount
        if isTooTiny (lx1) || isTooTiny (ly1) then
            [||]
        else
            let vxF = lx1/xl
            let vyF = ly1/yl
            let vxX = rect.XAxisX * vxF
            let vxY = rect.XAxisY * vxF
            let vyX = rect.YAxisX * vyF
            let vyY = rect.YAxisY * vyF
            let rss = Array.zeroCreate xCount
            for ix = 0 to xCount-1 do
                let rs = Array.zeroCreate yCount
                let xF = xGap * float ix / xl + lx1 * float ix / xl
                let xX = rect.XAxisX * xF
                let xY = rect.XAxisY * xF
                for iy = 0 to yCount-1 do
                    let yF = yGap * float iy / yl + ly1 * float iy / yl
                    rs.[iy] <-
                        Rect2D.createUnchecked(
                            rect.OriginX + xX + rect.YAxisX*yF,
                            rect.OriginY + xY + rect.YAxisY*yF,
                            vxX,
                            vxY,
                            vyX,
                            vyY)
                rss.[ix] <- rs
            rss

    /// Divides a 2D rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 2D Rectangle.
    /// It will create as many sub-rectangles as possible respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the X-axis. The sub-array is divided along the Y-axis.
    static member subDivideMinLength (rect:Rect2D, xMinLen:float, yMinLen:float, xGap:float, yGap:float) : Rect2D array array =
        let xLen = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let yLen = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)
        if xLen < xMinLen  then fail $"Rect2D.subDivideMinLength: xMinLen {xMinLen} is bigger than rect X-axis length {xLen} for {rect}"
        if yLen < yMinLen  then fail $"Rect2D.subDivideMinLength: yMinLen {yMinLen} is bigger than rect Y-axis length {yLen} for {rect}"
        let xCount = int (xLen / (xMinLen*0.9999))
        let yCount = int (yLen / (yMinLen*0.9999))
        Rect2D.subDivide xCount yCount xGap yGap rect

    /// Divides a 2D rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 2D Rectangle.
    /// It will create as few as segments as possible respecting the maximum segment length.
    /// The input maxSegmentLength is multiplied by factor 1.00001 of to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    /// The returned array is divided along the X-axis. The sub-array is divided along the Y-axis.
    static member subDivideMaxLength (rect:Rect2D, xMaxLen:float, yMaxLen:float, xGap:float, yGap:float) : Rect2D array array =
        let xLen = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let yLen = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)
        let xCount = 1 + int (xLen / (xMaxLen*1.00001))
        let yCount = 1 + int (yLen / (yMaxLen*1.00001))
        Rect2D.subDivide xCount yCount xGap yGap rect

    /// Divides a 2D rectangle into a grid of points. The points are returned as an array of arrays.
    /// A xCount and yCount of 2 will only return just the 4 corners of the rectangle.
    /// A xCount and yCount of 3 will return 9 points, including the 4 corners, the 4 mid points on the edges and the center.
    static member grid (xCount:int) (yCount:int) (rect:Rect2D) : Pt[][]=
        if xCount <= 1 ||yCount <= 1 then
            fail $"Rect2D.grid: xCount {xCount} and yCount {yCount} must be 2 or more"
        let xl = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let yl = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)
        let lx1 = xl / float (xCount-1)
        let ly1 = yl / float (yCount-1)
        let rss = Array.zeroCreate xCount
        for ix = 0 to xCount-1 do
            let rs = Array.zeroCreate yCount
            let xF = lx1 * float ix / xl
            let xX = rect.XAxisX * xF
            let xY = rect.XAxisY * xF
            for iy = 0 to yCount-1 do
                let yF = ly1 * float iy / yl
                rs.[iy] <- Pt(rect.OriginX + xX + rect.YAxisX*yF, rect.OriginY + xY + rect.YAxisY*yF)
            rss.[ix] <- rs
        rss

    /// Divides a 2D rectangle into a grid of points.
    /// It will create as many points as possible respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the X-axis. The sub-array is divided along the Y-axis.
    static member gridMinLength (xMinLen:float) (yMinLen:float) (rect:Rect2D) : Pt array array =
        let xLen = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let yLen = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)
        if xLen < xMinLen  then fail $"Rect2D.gridMinLength: xMinLen {xMinLen} is bigger than rect X-axis length {xLen} for {rect}"
        if yLen < yMinLen  then fail $"Rect2D.gridMinLength: yMinLen {yMinLen} is bigger than rect Y-axis length {yLen} for {rect}"
        let xCount = 1 + int (xLen / (xMinLen*0.9999))
        let yCount = 1 + int (yLen / (yMinLen*0.9999))
        Rect2D.grid xCount yCount rect

    /// Divides a 2D rectangle into a grid of points.
    /// It will create as few as points as possible respecting the maximum segment length.
    /// The input maxSegmentLength is multiplied by factor 1.00001 to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    /// The returned array is divided along the X-axis. The sub-array is divided along the Y-axis.
    static member gridMaxLength (xMaxLen:float) (yMaxLen:float) (rect:Rect2D) : Pt array array =
        let xLen = sqrt(rect.XAxisX*rect.XAxisX + rect.XAxisY*rect.XAxisY)
        let yLen = sqrt(rect.YAxisX*rect.YAxisX + rect.YAxisY*rect.YAxisY)
        let xCount = 2 + int (xLen / (xMaxLen*1.00001))
        let yCount = 2 + int (yLen / (yMaxLen*1.00001))
        Rect2D.grid xCount yCount rect

    // #endregion
    // #region Points

    /// <summary>Returns the corner diagonally opposite of corner from Origin (point 2).
    /// r.Origin + r.XAxis + r.YAxis
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.FarCorner : Pt =
        Pt(r.OriginX + r.XAxisX + r.YAxisX,
           r.OriginY + r.XAxisY + r.YAxisY)

    /// Returns the far corner (point 2) of the 2D rectangle.
    static member inline farCorner (r:Rect2D) : Pt =
        r.FarCorner

    /// <summary>Returns the corner at end of X-axis (point 1).
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.XCorner : Pt =
        Pt(r.OriginX + r.XAxisX, r.OriginY + r.XAxisY)

    /// Returns the X corner (point 1) of the 2D rectangle.
    static member inline xCorner (r:Rect2D) : Pt =
        r.XCorner

    /// <summary>Returns the corner at end of Y-axis (point 3).
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.YCorner : Pt =
        Pt(r.OriginX + r.YAxisX, r.OriginY + r.YAxisY)

    /// Returns the Y corner (point 3) of the 2D rectangle.
    static member inline yCorner (r:Rect2D) : Pt =
        r.YCorner

    /// <summary>Returns point 0 of the 2D rectangle. Same as member rect.Origin.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Pt0 : Pt =
        Pt(r.OriginX, r.OriginY)

    /// Returns point 0 of the 2D rectangle.
    static member inline pt0 (r:Rect2D) : Pt =
        r.Pt0

    /// <summary>Returns point 1 of the 2D rectangle.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Pt1 : Pt =
        Pt(r.OriginX + r.XAxisX, r.OriginY + r.XAxisY)

    /// Returns point 1 of the 2D rectangle.
    static member inline pt1 (r:Rect2D) : Pt =
        r.Pt1

    /// <summary>Returns point 2 of the 2D rectangle. Same as rect.FarCorner.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Pt2 : Pt =
        Pt(r.OriginX + r.XAxisX + r.YAxisX,
           r.OriginY + r.XAxisY + r.YAxisY)

    /// Returns point 2 of the 2D rectangle.
    static member inline pt2 (r:Rect2D) : Pt =
        r.Pt2

    /// <summary>Returns point 3 of the 2D rectangle.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Pt3 : Pt =
        Pt(r.OriginX + r.YAxisX, r.OriginY + r.YAxisY)

    /// Returns point 3 of the 2D rectangle.
    static member inline pt3 (r:Rect2D) : Pt =
        r.Pt3



    /// <summary>Returns the 4 corners of the 2D rectangle in counter-clockwise order, starting at Origin.
    /// Returns an array of 4 Points: point 0 then 1, 2 and 3.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.Points :Pt[] =
        let p0 = Pt(r.OriginX, r.OriginY)
        let p1 = Pt(r.OriginX + r.XAxisX, r.OriginY + r.XAxisY)
        let p2 = Pt(r.OriginX + r.XAxisX + r.YAxisX, r.OriginY + r.XAxisY + r.YAxisY)
        let p3 = Pt(r.OriginX + r.YAxisX, r.OriginY + r.YAxisY)
        [| p0 ; p1 ; p2; p3|]

    /// Returns the 4 corner points of the 2D rectangle.
    static member inline points (r:Rect2D) : Pt[] =
        r.Points

    /// <summary>Returns the 4 corners of the 2D rectangle as closed loop in counter-clockwise order, starting at Origin.
    /// First and last point are the same.
    /// Returns an array of 5 Points: point 0 then 1, 2, 3 and again 0.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.PointsLooped :Pt[] =
        let p0 = Pt(r.OriginX, r.OriginY)
        let p1 = Pt(r.OriginX + r.XAxisX, r.OriginY + r.XAxisY)
        let p2 = Pt(r.OriginX + r.XAxisX + r.YAxisX, r.OriginY + r.XAxisY + r.YAxisY)
        let p3 = Pt(r.OriginX + r.YAxisX, r.OriginY + r.YAxisY)
        [| p0 ; p1 ; p2; p3; p0|]

    /// Returns the 4 corner points as a closed loop.
    static member inline pointsLooped (r:Rect2D) : Pt[] =
        r.PointsLooped

    /// <summary> Returns the 4 corners of the 2D rectangle as open loop in counter-clockwise order, starting at Origin.
    /// Returns a ResizeArray of 8 floats: x and y of point 0, x and y of point 1, x and y of point 2, x and y of point 3.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.PointsXY_CCW :ResizeArray<float> =
        let xys = ResizeArray<float>(8)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginX + r.XAxisX)
        xys.Add(r.OriginY + r.XAxisY)
        xys.Add(r.OriginX + r.XAxisX + r.YAxisX)
        xys.Add(r.OriginY + r.XAxisY + r.YAxisY)
        xys.Add(r.OriginX + r.YAxisX)
        xys.Add(r.OriginY + r.YAxisY)
        xys

    /// Returns the 4 corner points of the 2D rectangle as an open loop in counter-clockwise order, starting at Origin.
    /// Returns a ResizeArray of 8 floats: x and y of point 0, x and y of point 1, x and y of point 2, x and y of point 3.
    static member inline pointsXY_CCW (r:Rect2D) : ResizeArray<float> =
        r.PointsXY_CCW

    /// <summary> Returns the 4 corners of the 2D rectangle as closed loop in counter-clockwise order, starting at Origin.
    /// Returns a ResizeArray of 10 floats: x and y of point 0, x and y of point 1, x and y of point 2, x and y of point 3 and again x and y of point 0.
    /// <code>
    ///  local
    ///  Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.PointsXYLooped_CCW : ResizeArray<float> =
        let xys = ResizeArray<float>(10)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginX + r.XAxisX)
        xys.Add(r.OriginY + r.XAxisY)
        xys.Add(r.OriginX + r.XAxisX + r.YAxisX)
        xys.Add(r.OriginY + r.XAxisY + r.YAxisY)
        xys.Add(r.OriginX + r.YAxisX)
        xys.Add(r.OriginY + r.YAxisY)
        xys.Add(r.OriginX) // loop back to first point
        xys.Add(r.OriginY)
        xys

    /// Returns the 4 corner points as a closed loop in counter-clockwise order, starting at Origin.
    /// Returns a ResizeArray of 10 floats: x and y of point 0, x and y of point 1, x and y of point 2, x and y of point 3 and again x and y of point 0.
    static member inline pointsXYLooped_CCW (r:Rect2D) : ResizeArray<float> =
        r.PointsXYLooped_CCW

    // clockwise versions of the above:

    /// <summary> Returns the 4 corners of the 2D rectangle as open loop in Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 8 floats: x and y of point 0, x and y of point 3, x and y of point 2, x and y of point 1.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.PointsXY_CW :ResizeArray<float> =
        let xys = ResizeArray<float>(8)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginX + r.YAxisX)
        xys.Add(r.OriginY + r.YAxisY)
        xys.Add(r.OriginX + r.XAxisX + r.YAxisX)
        xys.Add(r.OriginY + r.XAxisY + r.YAxisY)
        xys.Add(r.OriginX + r.XAxisX)
        xys.Add(r.OriginY + r.XAxisY)
        xys

    /// Returns the 4 corner points of the 2D rectangle as open loop in Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 8 floats: x and y of point 0, x and y of point 3, x and y of point 2, x and y of point 1.
    static member inline pointsXY_CW (r:Rect2D) : ResizeArray<float> =
        r.PointsXY_CW

    /// <summary> Returns the 4 corners of the 2D rectangle as closed loop in Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 10 floats: x and y of point 0, x and y of point 3, x and y of point 2, x and y of point 1 and again x and y of point 0.
    /// <code>
    ///  local
    ///  Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.PointsXYLooped_CW : ResizeArray<float> =
        let xys = ResizeArray<float>(10)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginX + r.YAxisX)
        xys.Add(r.OriginY + r.YAxisY)
        xys.Add(r.OriginX + r.XAxisX + r.YAxisX)
        xys.Add(r.OriginY + r.XAxisY + r.YAxisY)
        xys.Add(r.OriginX + r.XAxisX)
        xys.Add(r.OriginY + r.XAxisY)
        xys.Add(r.OriginX) // loop back to first point
        xys.Add(r.OriginY)
        xys

    /// Returns the 4 corner points as a closed loop in Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 10 floats: x and y of point 0, x and y of point 3, x and y of point 2, x and y of point 1 and again x and y of point 0.
    static member inline pointsXYLooped_CW (r:Rect2D) : ResizeArray<float> =
        r.PointsXYLooped_CW

    /// <summary>Iterates the 4 corners of the 2D rectangle in counter-clockwise order, starting at Origin.</summary>
    /// <param name="action">The action to call 4 times . Once for each corner, with x and y as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsCCW (action: float -> float -> unit) (r:Rect2D) : unit =
        action r.OriginX r.OriginY
        action (r.OriginX + r.XAxisX) (r.OriginY + r.XAxisY)
        action (r.OriginX + r.XAxisX + r.YAxisX) (r.OriginY + r.XAxisY + r.YAxisY)
        action (r.OriginX + r.YAxisX) (r.OriginY + r.YAxisY)

    /// <summary>Iterates the 4 corners of the 2D rectangle as closed loop in counter-clockwise order, starting and ending at Origin.</summary>
    /// <param name="action">The action to call 5 times. With x and y as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsLoopedCCW (action: float -> float -> unit) (r:Rect2D) : unit =
        action r.OriginX r.OriginY
        action (r.OriginX + r.XAxisX) (r.OriginY + r.XAxisY)
        action (r.OriginX + r.XAxisX + r.YAxisX) (r.OriginY + r.XAxisY + r.YAxisY)
        action (r.OriginX + r.YAxisX) (r.OriginY + r.YAxisY)
        action r.OriginX r.OriginY

    /// <summary>Iterates the 4 corners of the 2D rectangle in Clockwise order, starting at Origin.</summary>
    /// <param name="action">The action to call 4 times . Once for each corner, with x and y as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsCW (action: float -> float -> unit) (r:Rect2D) : unit =
        action r.OriginX r.OriginY
        action (r.OriginX + r.YAxisX) (r.OriginY + r.YAxisY)
        action (r.OriginX + r.XAxisX + r.YAxisX) (r.OriginY + r.XAxisY + r.YAxisY)
        action (r.OriginX + r.XAxisX) (r.OriginY + r.XAxisY)

    /// <summary>Iterates the 4 corners of the 2D rectangle as closed loop in Clockwise order, starting and ending at Origin.</summary>
    /// <param name="action">The action to call 5 times. With x and y as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsLoopedCW (action: float -> float -> unit) (r:Rect2D) : unit =
        action r.OriginX r.OriginY
        action (r.OriginX + r.YAxisX) (r.OriginY + r.YAxisY)
        action (r.OriginX + r.XAxisX + r.YAxisX) (r.OriginY + r.XAxisY + r.YAxisY)
        action (r.OriginX + r.XAxisX) (r.OriginY + r.XAxisY)
        action r.OriginX r.OriginY

    // #endregion
    // #region Edges

    /// <summary>Returns a 2D line from point 0 to 1 of the 2D rectangle.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge01 : Line2D =
        Line2D(r.OriginX, r.OriginY, r.OriginX + r.XAxisX, r.OriginY + r.XAxisY)

    /// Returns edge 0-1 of the 2D rectangle.
    static member inline edge01 (r:Rect2D) : Line2D =
        r.Edge01

    /// <summary>Returns a 2D line from point 1 to 2 of the 2D rectangle.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge12 : Line2D =
        let sx = r.OriginX + r.XAxisX
        let sy = r.OriginY + r.XAxisY
        Line2D(sx, sy, sx + r.YAxisX, sy + r.YAxisY)

    /// Returns edge 1-2 of the 2D rectangle.
    static member inline edge12 (r:Rect2D) : Line2D =
        r.Edge12

    /// <summary>Returns a 2D line from point 2 to 3 of the 2D rectangle.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge23 : Line2D =
        let p3x = r.OriginX + r.YAxisX
        let p3y = r.OriginY + r.YAxisY
        Line2D(p3x + r.XAxisX, p3y + r.XAxisY, p3x, p3y)

    /// Returns edge 2-3 of the 2D rectangle.
    static member inline edge23 (r:Rect2D) : Line2D =
        r.Edge23

    /// <summary>Returns a 2D line from point 3 to 0 of the 2D rectangle.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge30 : Line2D =
        Line2D(r.OriginX + r.YAxisX, r.OriginY + r.YAxisY, r.OriginX, r.OriginY)

    /// Returns edge 3-0 of the 2D rectangle.
    static member inline edge30 (r:Rect2D) : Line2D =
        r.Edge30

    /// <summary>Returns a 2D line from point 1 to 0 of the 2D rectangle. This is the reverse of Edge01.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge10 : Line2D =
        Line2D(r.OriginX + r.XAxisX, r.OriginY + r.XAxisY, r.OriginX, r.OriginY)

    /// Returns edge 1-0 of the 2D rectangle. This is the reverse of Edge01.
    static member inline edge10 (r:Rect2D) : Line2D =
        r.Edge10

    /// <summary>Returns a 2D line from point 2 to 1 of the 2D rectangle. This is the reverse of Edge12.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge21 : Line2D =
        let sx = r.OriginX + r.XAxisX
        let sy = r.OriginY + r.XAxisY
        Line2D(sx + r.YAxisX, sy + r.YAxisY, sx, sy)

    /// Returns edge 2-1 of the 2D rectangle. This is the reverse of Edge12.
    static member inline edge21 (r:Rect2D) : Line2D =
        r.Edge21

    /// <summary>Returns a 2D line from point 3 to 2 of the 2D rectangle. This is the reverse of Edge23.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge32 : Line2D =
        let p3x = r.OriginX + r.YAxisX
        let p3y = r.OriginY + r.YAxisY
        Line2D(p3x, p3y, p3x + r.XAxisX, p3y + r.XAxisY)

    /// Returns edge 3-2 of the 2D rectangle. This is the reverse of Edge23.
    static member inline edge32 (r:Rect2D) : Line2D =
        r.Edge32

    /// <summary>Returns a 2D line from point 0 to 3 of the 2D rectangle. This is the reverse of Edge30.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.Edge03 : Line2D =
        Line2D(r.OriginX, r.OriginY, r.OriginX + r.YAxisX, r.OriginY + r.YAxisY)

    /// Returns edge 0-3 of the 2D rectangle. This is the reverse of Edge30.
    static member inline edge03 (r:Rect2D) : Line2D =
        r.Edge03

    /// <summary>Returns the local X side as the 2D line from point 0 to 1 of the 2D rectangle.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.EdgeX : Line2D =
        r.Edge01

    /// Returns the local X edge of the 2D rectangle.
    static member inline edgeX (r:Rect2D) : Line2D =
        r.EdgeX

    /// <summary>Returns the local Y side as 2D line from point 0 to 3 of the 2D rectangle.
    /// This is the reverse of Edge30.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.EdgeY : Line2D =
        Line2D(r.OriginX, r.OriginY, r.OriginX + r.YAxisX, r.OriginY + r.YAxisY)

    /// Returns the local Y edge of the 2D rectangle.
    static member inline edgeY (r:Rect2D) : Line2D =
        r.EdgeY

    /// <summary>Returns the diagonal 2D line from point 0 to 2 of the 2D rectangle.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member inline r.DiagonalLine : Line2D =
        Line2D(r.OriginX, r.OriginY, r.OriginX + r.YAxisX + r.XAxisX, r.OriginY + r.YAxisY + r.XAxisY)

    /// Returns the diagonal line from point 0 to point 2 of the 2D rectangle.
    static member inline diagonalLine (r:Rect2D) : Line2D =
        r.DiagonalLine

    /// <summary>Returns the same rectangle with a new orientation rotated by 90 degrees clockwise around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 3, X-axis to point 0, Y-axis to point 2.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.RotateOrientation90CW : Rect2D =
        Rect2D.createUnchecked(r.OriginX + r.YAxisX, r.OriginY + r.YAxisY, -r.YAxisX, -r.YAxisY, r.XAxisX, r.XAxisY)

    /// Returns the same rectangle with orientation rotated 90 degrees clockwise around center.
    static member inline rotateOrientation90CW (r:Rect2D) : Rect2D =
        r.RotateOrientation90CW

    /// <summary>Returns the rectangle rotated 180 degrees around its center.
    /// Returns the same rectangle with a new orientation rotated by 180 degrees around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 2, X-axis to point 3, Y-axis to point 1.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.RotateOrientation180 : Rect2D =
        Rect2D.createUnchecked(r.OriginX + r.YAxisX + r.XAxisX, r.OriginY + r.YAxisY + r.XAxisY, -r.XAxisX, -r.XAxisY, -r.YAxisX, -r.YAxisY)

    /// Returns the same rectangle with orientation rotated 180 degrees around center.
    static member inline rotateOrientation180 (r:Rect2D) : Rect2D =
        r.RotateOrientation180

    /// <summary>Returns the same rectangle with a new orientation rotated by 90 degrees counter-clockwise around its center.
    /// This only changes the internal representation of the rectangle, the appearance is not changed.
    /// Origin will be at point 1, X-axis to point 2, Y-axis to point 0.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.RotateOrientation90CCW : Rect2D =
        Rect2D.createUnchecked(r.OriginX + r.XAxisX, r.OriginY + r.XAxisY, r.YAxisX, r.YAxisY, -r.XAxisX, -r.XAxisY)

    /// Returns the same rectangle with orientation rotated 90 degrees counter-clockwise around center.
    static member inline rotateOrientation90CCW (r:Rect2D) : Rect2D =
        r.RotateOrientation90CCW


    /// <summary>Returns the 4 Edges of the 2D rectangle in counter-clockwise order, starting at Origin.
    /// Returns an array of 4 Lines: from point 0 to 1, 1 to 2 to 3 and 3 to 0.
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.Edges :Line2D[] =
        let p0x = r.OriginX
        let p0y = r.OriginY
        let p1x = p0x + r.XAxisX
        let p1y = p0y + r.XAxisY
        let p2x = p1x + r.YAxisX
        let p2y = p1y + r.YAxisY
        let p3x = p0x + r.YAxisX
        let p3y = p0y + r.YAxisY
        [| Line2D(p0x, p0y, p1x, p1y); Line2D(p1x, p1y, p2x, p2y); Line2D(p2x, p2y, p3x, p3y); Line2D(p3x, p3y, p0x, p0y)|]

    /// Returns the 4 edges of the 2D rectangle.
    static member inline edges (r:Rect2D) : Line2D[] =
        r.Edges

    /// <summary>Returns one of the 4 Edges as 2D Line:
    /// Edge 0: from point  0 to 1
    /// Edge 1: from point  1 to 2
    /// Edge 2: from point  2 to 3
    /// Edge 3: from point  3 to 0
    /// <code>
    ///   local
    ///   Y-axis
    ///   ^
    ///   |
    ///   |             2
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |       local
    ///   +------------+-----> X-axis
    ///  0-Origin       1
    /// </code></summary>
    member r.GetEdge i : Line2D =
        match i with
        | 0 -> Line2D(r.OriginX, r.OriginY, r.OriginX + r.XAxisX, r.OriginY + r.XAxisY)
        | 1 -> Line2D(r.OriginX + r.XAxisX, r.OriginY + r.XAxisY, r.OriginX + r.XAxisX + r.YAxisX, r.OriginY + r.XAxisY + r.YAxisY)
        | 2 -> Line2D(r.OriginX + r.XAxisX + r.YAxisX, r.OriginY + r.XAxisY + r.YAxisY, r.OriginX + r.YAxisX, r.OriginY + r.YAxisY)
        | 3 -> Line2D(r.OriginX + r.YAxisX, r.OriginY + r.YAxisY, r.OriginX, r.OriginY)
        | _ -> fail $"Rect2D.GetEdge: index {i} out of range 0..3" |> unbox // unbox to make type checker happy

    /// Returns one edge of the 2D rectangle by index 0..3.
    static member inline getEdge (i:int) (r:Rect2D) : Line2D =
        r.GetEdge i


    // #endregion

    #nowarn "44"

#if !FABLE_COMPILER
/// Serializes a Rect2D as its origin and axis components with System.Text.Json.
and internal Rect2DJsonConverter() =
    inherit JsonConverter<Rect2D>()

    override _.Write(writer, rectangle, options) =
        writer.WriteStartObject()
        JsonConvert.writeFloat writer options "OriginX" rectangle.OriginX
        JsonConvert.writeFloat writer options "OriginY" rectangle.OriginY
        JsonConvert.writeFloat writer options "XAxisX" rectangle.XAxisX
        JsonConvert.writeFloat writer options "XAxisY" rectangle.XAxisY
        JsonConvert.writeFloat writer options "YAxisX" rectangle.YAxisX
        JsonConvert.writeFloat writer options "YAxisY" rectangle.YAxisY
        writer.WriteEndObject()

    override _.Read(reader, _, options) =
        JsonConvert.readStartObject &reader "Euclid.Rect2D"
        let originX = JsonConvert.readFloat &reader options "Euclid.Rect2D" "OriginX"
        let originY = JsonConvert.readFloat &reader options "Euclid.Rect2D" "OriginY"
        let xAxisX = JsonConvert.readFloat &reader options "Euclid.Rect2D" "XAxisX"
        let xAxisY = JsonConvert.readFloat &reader options "Euclid.Rect2D" "XAxisY"
        let yAxisX = JsonConvert.readFloat &reader options "Euclid.Rect2D" "YAxisX"
        let yAxisY = JsonConvert.readFloat &reader options "Euclid.Rect2D" "YAxisY"
        JsonConvert.readEndObject &reader "Euclid.Rect2D"
        Rect2D.createFromVcs (Vc(xAxisX, xAxisY)) (Vc(yAxisX, yAxisY)) (Pt(originX, originY))
#endif
