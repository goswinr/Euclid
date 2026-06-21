namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid
open System.Collections.Generic
open EuclidErrors


/// <summary>
/// A struct containing a 2D Origin point and two 2D Edge vectors,
/// representing an immutable 2D Rectangle with any rotation in 2D space.
/// This implementation guarantees the 2D Rectangle to be always valid.
/// That means the X and Y axes are always perpendicular to each other.
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
type Rect2D =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The X coordinate of the Origin Corner of the 2D Rectangle.
    [<DataMember>] val public OriginX: float

    /// The Y coordinate of the Origin Corner of the 2D Rectangle.
    [<DataMember>] val public OriginY: float

    /// The X component of the Edge vector representing the X-axis of the 2D Rectangle.
    [<DataMember>] val public XaxisX: float

    /// The Y component of the Edge vector representing the X-axis of the 2D Rectangle.
    [<DataMember>] val public XaxisY: float

    /// The X component of the Edge vector representing the Y-axis of the 2D Rectangle.
    [<DataMember>] val public YaxisX: float

    /// The Y component of the Edge vector representing the Y-axis of the 2D Rectangle.
    [<DataMember>] val public YaxisY: float

    /// Unchecked Internal Constructor Only.
    /// Create a 2D Rectangle from origin coordinates and X and Y axis vector components.
    [<Obsolete("Unsafe internal constructor, doesn't check the input (unless compiled in DEBUG mode), but must be public for inlining. So marked Obsolete instead.") >]
    new (originX:float, originY:float, axisXX:float, axisXY:float, axisYX:float, axisYY:float) =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
            let lenX = sqrt(axisXX*axisXX + axisXY*axisXY)
            let lenY = sqrt(axisYX*axisYX + axisYY*axisYY)
            if isTooSmall lenX then  failTooSmall2 "Rect2D() axisX" (Vc(axisXX, axisXY)) (Vc(axisYX, axisYY))
            if isTooSmall lenY then  failTooSmall2 "Rect2D() axisY" (Vc(axisYX, axisYY)) (Vc(axisXX, axisXY))
            // scale the dot-product tolerance by the axis lengths so the check is a relative angle tolerance (independent of the rectangle size):
            if abs (axisXX*axisYX + axisXY*axisYY) > lenX*lenY * 1e-9 then fail2 $"Rect2D(): X-axis and Y-axis are not perpendicular" (Vc(axisXX, axisXY)) (Vc(axisYX, axisYY))
            if isNegative(axisXX*axisYY - axisXY*axisYX) then fail2 $"Rect2D(): X-axis and Y-axis are not counter-clockwise" (Vc(axisXX, axisXY)) (Vc(axisYX, axisYY))
        #endif
            {OriginX=originX; OriginY=originY; XaxisX=axisXX; XaxisY=axisXY; YaxisX=axisYX; YaxisY=axisYY}


    /// Create a 2D Rectangle from the origin point and X-edge and Y edge.
    /// Does not check for counter-clockwise order of x and y.
    /// Does not check for perpendicularity.
    static member inline createUnchecked (originX:float, originY:float, xaxisX:float, xaxisY:float, yaxisX:float, yaxisY:float) : Rect2D =
        #nowarn "44"
        Rect2D(originX, originY, xaxisX, xaxisY, yaxisX, yaxisY)
        #warnon "44" // re-enable warning for obsolete usage

    /// Create a 2D Rectangle from the origin point and X-edge and Y edge.
    /// Does not check for counter-clockwise order of x and y.
    /// Does not check for perpendicularity.
    static member inline createUncheckedVec (origin:Pt, x:Vc, y:Vc) : Rect2D =
        Rect2D.createUnchecked(origin.X, origin.Y, x.X, x.Y, y.X, y.Y)

    /// Creates a 2D Point from r.OriginX and r.OriginY
    member r.Origin : Pt =
        Pt(r.OriginX, r.OriginY)

    /// Creates a 2D Point from r.OriginX and r.OriginY
    static member inline origin (r:Rect2D) : Pt =
        r.Origin

    /// Creates a 2D Vector from r.YaxisX and r.YaxisY
    member r.Yaxis : Vc =
        Vc(r.YaxisX, r.YaxisY)

    /// Creates a 2D Vector from r.XaxisX and r.XaxisY
    member r.Xaxis : Vc =
        Vc(r.XaxisX, r.XaxisY)

    /// Creates a 2D Vector from r.XaxisX and r.XaxisY
    static member inline xAxis (r:Rect2D) : Vc =
        r.Xaxis

    /// Creates a 2D Vector from r.YaxisX and r.YaxisY
    static member inline yAxis (r:Rect2D) : Vc =
        r.Yaxis

    /// The size in X direction
    member inline r.SizeX : float =
        sqrt(r.XaxisX*r.XaxisX + r.XaxisY * r.XaxisY)

    /// Returns the size in X direction.
    static member inline sizeX (r:Rect2D) : float =
        r.SizeX

   /// The squared size in X direction
    member inline r.SizeXSq : float =
        r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY

    /// Returns the squared size in X direction.
    static member inline sizeXSq (r:Rect2D) : float =
        r.SizeXSq

    /// The size in Y direction
    member inline r.SizeY : float =
        sqrt(r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY)

    /// The squared size in Y direction
    member inline r.SizeYSq : float =
        r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY

    /// Returns the size in Y direction.
    static member inline sizeY (r:Rect2D) : float =
        r.SizeY

    /// Returns the squared size in Y direction.
    static member inline sizeYSq (r:Rect2D) : float =
        r.SizeYSq

    /// Nicely formatted string representation of the 2D Rectangle including its size.
    override r.ToString() : string =
        let sizeX = Format.float r.SizeX
        let sizeY = Format.float r.SizeY
        let origin = $"X={Format.float r.OriginX}|Y={Format.float r.OriginY}"
        let xAxis = $"X={Format.float r.XaxisX}|Y={Format.float r.XaxisY}"
        let yAxis = $"X={Format.float r.YaxisX}|Y={Format.float r.YaxisY}"
        $"Euclid.Rect2D {sizeX} x {sizeY} (Origin:{origin}| X-ax:{xAxis}|Y-ax:{yAxis})"

    /// Returns a nicely formatted string representation of the 2D rectangle.
    static member inline toString (r:Rect2D) : string =
        r.ToString()

    /// Format the 2D Rectangle into string with nice floating point number formatting of X and Y size only.
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
        $"Rect2D.createUnchecked({r.OriginX}, {r.OriginY}, {r.XaxisX}, {r.XaxisY}, {r.YaxisX}, {r.YaxisY})"

    /// Returns an F# code string that recreates the 2D rectangle.
    static member inline asFSharpCode (r:Rect2D) : string =
        r.AsFSharpCode

    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit : UnitVc =
        let x = r.XaxisX
        let y = r.XaxisY
        let sqLen = x*x + y*y
        if isTooTinySq sqLen then
            failTooSmall "Rect2D.XaxisUnit" r
        let f = 1.0 / sqrt sqLen
        UnitVc.createUnchecked(x*f, y*f)

    /// Returns the unitized local X-axis of the 2D rectangle.
    static member inline xAxisUnit (r:Rect2D) : UnitVc =
        r.XaxisUnit

    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit : UnitVc =
        let x = r.YaxisX
        let y = r.YaxisY
        let sqLen = x*x + y*y
        if isTooTinySq sqLen then
            failTooSmall "Rect2D.YaxisUnit" r
        let f = 1.0 / sqrt sqLen
        UnitVc.createUnchecked(x*f, y*f)

    /// Returns the unitized local Y-axis of the 2D rectangle.
    static member inline yAxisUnit (r:Rect2D) : UnitVc =
        r.YaxisUnit

    /// Returns the diagonal vector of the 2D Rectangle.
    /// From Origin to FarCorner.
    member inline r.Diagonal : Vc =
        Vc(r.XaxisX + r.YaxisX, r.XaxisY + r.YaxisY)

    /// Returns the diagonal vector of the 2D rectangle.
    static member inline diagonal (r:Rect2D) : Vc =
        r.Diagonal

    /// Returns the center of the 2D Rectangle.
    member inline r.Center : Pt =
        Pt(r.OriginX + (r.XaxisX + r.YaxisX)*0.5,
           r.OriginY + (r.XaxisY + r.YaxisY)*0.5)

    /// Returns the center point of the 2D rectangle.
    static member inline center (r:Rect2D) : Pt =
        r.Center

    /// Evaluate a X and Y parameter of the 2D Rectangle.
    ///  0.0, 0.0 returns the Origin.
    ///  1.0, 1.0 returns the FarCorner.
    member inline r.EvaluateAt (xParameter:float, yParameter:float) : Pt =
        Pt(r.OriginX + r.XaxisX*xParameter + r.YaxisX*yParameter,
           r.OriginY + r.XaxisY*xParameter + r.YaxisY*yParameter)

    /// Evaluates X and Y parameters on the 2D rectangle and returns a point.
    static member inline evaluateAt (xParameter:float) (yParameter:float) (r:Rect2D) : Pt =
        r.EvaluateAt(xParameter, yParameter)

    /// Evaluate a point at X and Y distance on the respective axes of the 2D Rectangle.
    member inline r.EvaluateDist (xDistance:float, yDistance:float) : Pt =
        let lx = sqrt(r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY)
        let ly = sqrt(r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY)
        if isTooTiny (lx) then failTooSmall "Rect2D.EvaluateDist Xaxis" r
        if isTooTiny (ly) then failTooSmall "Rect2D.EvaluateDist Yaxis" r
        let xf = xDistance/lx
        let yf = yDistance/ly
        Pt(r.OriginX + r.XaxisX*xf + r.YaxisX*yf,
           r.OriginY + r.XaxisY*xf + r.YaxisY*yf)

    /// Evaluates a point by X and Y distances along local axes of the 2D rectangle.
    static member inline evaluateDist (xDistance:float) (yDistance:float) (r:Rect2D) : Pt =
        r.EvaluateDist(xDistance, yDistance)

    /// Calculates the area of the 2D Rectangle.
    member inline r.Area : float =
        sqrt(r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY) * sqrt(r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY)

    /// Returns the area of the 2D rectangle.
    static member inline area (r:Rect2D) : float =
        r.Area

    /// Scales the 2D rectangle by a given factor.
    /// Scale center is World Origin 0,0
    member inline r.Scale (factor:float) : Rect2D =
        Rect2D.createUnchecked(
                r.OriginX * factor,
                r.OriginY * factor,
                r.XaxisX * factor,
                r.XaxisY * factor,
                r.YaxisX * factor,
                r.YaxisY * factor)

    /// Scales the 2D rectangle by a given factor.
    /// Scale center is World Origin 0,0
    static member inline scale (factor:float) (r:Rect2D) : Rect2D =
        Rect2D.createUnchecked(
                r.OriginX * factor,
                r.OriginY * factor,
                r.XaxisX * factor,
                r.XaxisY * factor,
                r.YaxisX * factor,
                r.YaxisY * factor)

    /// Scales the 2D rectangle by a given factor on a given center point
    member inline l.ScaleOn (cen:Pt) (factor:float) : Rect2D =
        let cx = cen.X
        let cy = cen.Y
        Rect2D.createUnchecked(
            cx + (l.OriginX - cx) * factor,
            cy + (l.OriginY - cy) * factor,
            l.XaxisX * factor,
            l.XaxisY * factor,
            l.YaxisX * factor,
            l.YaxisY * factor
        )

    /// Scales the 2D rectangle around a given center point.
    static member inline scaleOn (cen:Pt) (factor:float) (r:Rect2D) : Rect2D =
        r.ScaleOn cen factor

    /// Returns the longest edge of the Rect2D.
    member inline b.LongestEdge : float =
        let x = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY
        let y = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY
        sqrt  (max x y)

    /// Returns the longest edge length of the 2D rectangle.
    static member inline longestEdge (r:Rect2D) : float =
        r.LongestEdge

    /// Returns the shortest edge of the Rect2D.
    member inline b.ShortestEdge : float =
        let x = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY
        let y = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY
        sqrt  (min x y)

    /// Returns the shortest edge length of the 2D rectangle.
    static member inline shortestEdge (r:Rect2D) : float =
        r.ShortestEdge

    /// Returns the square length of longest edge of the Rect2D.
    member inline b.LongestEdgeSq : float =
        let x = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY
        let y = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY
        max x y

    /// Returns the squared length of the longest edge of the 2D rectangle.
    static member inline longestEdgeSq (r:Rect2D) : float =
        r.LongestEdgeSq

    /// Returns the square length of shortest edge of the Rect2D.
    member inline b.ShortestEdgeSq : float =
        let x = b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY
        let y = b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY
        min x y

    /// Returns the squared length of the shortest edge of the 2D rectangle.
    static member inline shortestEdgeSq (r:Rect2D) : float =
        r.ShortestEdgeSq

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsPoint.
    member inline b.IsZero : bool =
        isTooTinySq (b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY) &&
        isTooTinySq (b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY)

    /// Returns TRUE if all sides are below zero-length tolerance.
    static member inline isZero (r:Rect2D) : bool =
        r.IsZero

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsZero.
    member inline b.IsPoint : bool =
        isTooTinySq (b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY) &&
        isTooTinySq (b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY)

    /// Returns TRUE if the 2D rectangle degenerates to a point.
    static member inline isPoint (r:Rect2D) : bool =
        r.IsPoint

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1 or 2.
    member inline b.CountZeroSides : int =
        countTooTinySqOrNaN    (b.XaxisX*b.XaxisX + b.XaxisY*b.XaxisY)
        +  countTooTinySqOrNaN (b.YaxisX*b.YaxisX + b.YaxisY*b.YaxisY)

    /// Returns the number of sides below zero-length tolerance.
    static member inline countZeroSides (r:Rect2D) : int =
        r.CountZeroSides

    /// Tests if two of the X and Y axis is smaller than the zeroLength tolerance.
    member inline b.IsLine : bool =
        b.CountZeroSides = 1

    /// Returns TRUE if exactly one side is below zero-length tolerance.
    static member inline isLine (r:Rect2D) : bool =
        r.IsLine

    /// Tests if no sides of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .HasArea
    member inline b.IsValid : bool =
        b.CountZeroSides = 0

    /// Returns TRUE if both sides are above zero-length tolerance.
    static member inline isValid (r:Rect2D) : bool =
        r.IsValid

    /// Tests if none of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasArea : bool =
        b.CountZeroSides = 0

    /// Returns TRUE if the 2D rectangle has area.
    static member inline hasArea (r:Rect2D) : bool =
        r.HasArea

    /// Check for point containment in the 2D Rectangle.
    /// By doing 4 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    member r.ContainsXY(x:float, y:float) : bool =
        let vx = x - r.OriginX
        let vy = y - r.OriginY
        vx*r.XaxisX + vy*r.XaxisY >= 0.
        &&
        vx*r.YaxisX + vy*r.YaxisY >= 0.
        &&
        (x - r.OriginX - r.YaxisX)*r.YaxisX + (y - r.OriginY - r.YaxisY)*r.YaxisY <= 0.
        &&
        (x - r.OriginX - r.XaxisX)*r.XaxisX + (y - r.OriginY - r.XaxisY)*r.XaxisY <= 0.

    /// Check for point containment in the 2D Rectangle.
    /// By doing 4 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    member r.Contains(p:Pt) : bool =
        r.ContainsXY(p.X, p.Y)

    /// Check for point containment in the 2D Rectangle.
    /// By doing 4 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    static member inline containsXY (x:float) (y:float) (r:Rect2D)  : bool =
        r.ContainsXY (x, y)

    /// Check for point containment in the 2D Rectangle.
    /// By doing 4 dot products with the sides of the rectangle.
    /// A point exactly on the edge of the Box is considered inside.
    static member inline contains (p:Pt) (r:Rect2D)  : bool =
        r.Contains p

    /// Get the axis aligned 2D Bounding Rectangle of the 2D Rectangle.
    member r.BRect : BRect =
        // Each of the 4 corners is Origin plus any subset of the two axis
        // components, so min/max decompose per coordinate.
        let minX = r.OriginX + min 0. r.XaxisX + min 0. r.YaxisX
        let minY = r.OriginY + min 0. r.XaxisY + min 0. r.YaxisY
        let maxX = r.OriginX + max 0. r.XaxisX + max 0. r.YaxisX
        let maxY = r.OriginY + max 0. r.XaxisY + max 0. r.YaxisY
        BRect.createUnchecked(minX, minY, maxX, maxY)

    /// Returns the axis-aligned bounding rectangle of the 2D rectangle.
    static member inline bRect (r:Rect2D) : BRect =
        r.BRect


    //#endregion
    //#region Create

    /// Create a 2D Rectangle from the origin point, an x-edge and an y-edge.
    /// Fails if x and y are not in counter-clockwise order.
    /// Fails if x and y are not perpendicular.
    /// Fails on vectors shorter than 1e-6.
    static member createFromVectors(origin:Pt, x:Vc, y:Vc) : Rect2D =
        let xLenSq = x.X*x.X + x.Y*x.Y
        let yLenSq = y.X*y.X + y.Y*y.Y
        if isTooSmallSq xLenSq  then failTooSmall2 "Rect2D.createFromVectors x" x y
        if isTooSmallSq yLenSq  then failTooSmall2 "Rect2D.createFromVectors y" y x
        // scale the dot-product tolerance by the axis lengths so the check is a relative angle tolerance (independent of the rectangle size):
        if abs (x.X*y.X + x.Y*y.Y) > sqrt(xLenSq*yLenSq) * 1e-9 then fail2 $"Rect2D.createFromVectors: X-axis and Y-axis are not perpendicular" x y
        if isNegative(x.X*y.Y - x.Y*y.X)then fail2 $"Rect2D.createFromVectors: X-axis and Y-axis are not counter-clockwise" x y
        Rect2D.createUnchecked(origin.X, origin.Y, x.X, x.Y, y.X, y.Y)

    /// Creates a 2D rectangle from a origin point, the X vector and Y size.
    /// The length of the X vector defines the X size, sizeY defines the absolute Y size.
    /// Fails on negative Y size.
    static member createFromXVectorAndWidth (origin:Pt, x:Vc, sizeY) : Rect2D =
        if isNegative sizeY then fail1 $"Rect2D.createFromXVectorAndWidth(): sizeY cannot be negative" sizeY
        if x.IsTinySq 1e-12 then failTooSmall "Rect2D.createFromXVectorAndWidth: x vector is too small" x
        let f = sizeY / sqrt(x.X*x.X + x.Y*x.Y) // x Vc need to be unitized
        let yX = -x.Y * f // x and y swaped to rotate 90 degree
        let yY =  x.X * f
        Rect2D.createUnchecked(origin.X, origin.Y, x.X, x.Y, yX, yY)

    /// Creates a 2D rectangle from an origin point, the X direction as unit-vector, the size in  X and Y direction.
    /// Fails on negative sizes.
    static member createFromDirectionAndSizes (origin:Pt, directionX:UnitVc, sizeX, sizeY) : Rect2D =
        if isNegative sizeX then fail2 $"Rect2D.createFromDirectionAndSizes(): sizeX cannot be negative" sizeX sizeY
        if isNegative sizeY then fail2 $"Rect2D.createFromDirectionAndSizes(): sizeY cannot be negative" sizeY sizeX
        Rect2D.createUnchecked(origin.X, origin.Y, directionX.X*sizeX, directionX.Y*sizeX, -directionX.Y*sizeY, directionX.X*sizeY)

    /// Create a 2D Rectangle from a 2D line and a  right and left offset.
    /// The left offset is in the direction of the future Y-axis.
    static member createFromLine(line:Line2D, offRight, offLeft) : Rect2D =
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

    /// Create a 2D Rectangle from an axis-aligned 2D Bounding Rectangle.
    static member createFromBRect (b:BRect)  : Rect2D =
        Rect2D.createUnchecked(b.MinX, b.MinY, b.SizeX, 0.0, 0.0, b.SizeY)

    /// Creates a 2D rectangle from a center point, the X direction, the X and the Y size.
    /// Fails on negative sizes.
    static member createFromCenterAndDirection (center:Pt, directionX:UnitVc, sizeX, sizeY) : Rect2D =
        if isNegative sizeX then fail $"Rect2D.createFromCenterAndDirection(..) sizeX cannot be negative: {sizeX}, sizeY is: {sizeY}, center: {center.AsString}"
        if isNegative sizeY then fail $"Rect2D.createFromCenterAndDirection(..) sizeY cannot be negative: {sizeY}, sizeX is: {sizeX}, center: {center.AsString}"
        let xX = directionX.X * sizeX
        let xY = directionX.Y * sizeX
        let yX = -directionX.Y * sizeY
        let yY = directionX.X * sizeY
        Rect2D.createUnchecked(center.X - (xX + yX)*0.5, center.Y - (xY + yY)*0.5, xX, xY, yX, yY)

    /// Creates a 2D rectangle from a center point, the X vector and the Y size.
    /// The length of the X vector defines the X size, sizeY defines the absolute Y size.
    /// Fails on negative Y size.
    static member createFromCenterAndVector (center:Pt, x:Vc, sizeY) : Rect2D =
        if isNegative sizeY then fail $"Rect2D.createFromCenterAndVector(..) sizeY cannot be negative: {sizeY}, x is: {x.AsString}, center: {center.AsString}"
        if x.IsTinySq 1e-12  then failTooSmall "Rect2D.createFromCenterAndVector: x vector is too small" x
        let f = sizeY / sqrt(x.X*x.X + x.Y*x.Y) // x Vc need to be unitized
        let yX = -x.Y * f // x and y swapped to rotate 90 degree
        let yY =  x.X * f
        Rect2D.createUnchecked(center.X - (x.X + yX)*0.5, center.Y - (x.Y + yY)*0.5, x.X, x.Y, yX, yY)

    /// Checks if two 2D Rectangles are equal within tolerance.
    /// Does not recognize congruent rectangles with different rotation as equal.
    /// Use a tolerance of 0.0 to check for an exact match of exactly equal rectangles.
    static member equals (tol:float) (a:Rect2D) (b:Rect2D)  : bool =
        abs (a.OriginX - b.OriginX) <= tol && //TODO raise an exception if the tolerance is negative ?
        abs (a.OriginY - b.OriginY) <= tol &&
        abs (a.XaxisX -  b.XaxisX ) <= tol &&
        abs (a.XaxisY -  b.XaxisY ) <= tol &&
        abs (a.YaxisX -  b.YaxisX ) <= tol &&
        abs (a.YaxisY -  b.YaxisY ) <= tol

    /// Check if two 2D Rectangles are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two rectangles are not exactly equal.
    static member notEquals (tol:float) (a:Rect2D) (b:Rect2D)  : bool =
        abs (a.OriginX - b.OriginX) > tol || //TODO raise an exception if the tolerance is negative ?
        abs (a.OriginY - b.OriginY) > tol ||
        abs (a.XaxisX -  b.XaxisX ) > tol ||
        abs (a.XaxisY -  b.XaxisY ) > tol ||
        abs (a.YaxisX -  b.YaxisX ) > tol ||
        abs (a.YaxisY -  b.YaxisY ) > tol

    /// Returns the 2D Rectangle expanded by distance on all four sides.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (r:Rect2D)  : Rect2D =
        let siX = r.SizeX
        let siY = r.SizeY
        let d = dist * -2.0
        if siX <= d || siY <= d  then
            fail $"Rect2D.expand: the 2D Rectangle {r.AsString} is too small to expand by negative distance {dist}"
        let xf = dist / siX
        let yf = dist / siY
        let xX = r.XaxisX * xf
        let xY = r.XaxisY * xf
        let yX = r.YaxisX * yf
        let yY = r.YaxisY * yf
        Rect2D.createUnchecked(
            r.OriginX - xX - yX,
            r.OriginY - xY - yY,
            r.XaxisX + xX * 2.,
            r.XaxisY + xY * 2.,
            r.YaxisX + yX * 2.,
            r.YaxisY + yY * 2.)

    /// Returns the 2D Rectangle expanded by respective distances on all four sides.
    /// Does check for overflow if distance is negative and fails.
    /// distX, distY are for the local X and Y-axis respectively.
    static member expandXY distX distY (r:Rect2D) : Rect2D =
        let siX = r.SizeX
        let siY = r.SizeY
        if siX <= distX * -2.0 then fail $"Rect2D.expandXY: the 2D Rectangle {r.AsString} is too small to expand by negative distance distX {distX}"
        if siY <= distY * -2.0 then fail $"Rect2D.expandXY: the 2D Rectangle {r.AsString} is too small to expand by negative distance distY {distY}"
        let xf = distX / siX
        let yf = distY / siY
        let xX = r.XaxisX * xf
        let xY = r.XaxisY * xf
        let yX = r.YaxisX * yf
        let yY = r.YaxisY * yf
        Rect2D.createUnchecked(
            r.OriginX - xX - yX,
            r.OriginY - xY - yY,
            r.XaxisX + xX * 2.,
            r.XaxisY + xY * 2.,
            r.YaxisX + yX * 2.,
            r.YaxisY + yY * 2.)

    /// Returns the 2D-rectangle expanded by a relative factor on all four sides.
    /// Values between 0.0 and 1.0 shrink the rectangle.
    /// Values larger than 1.0 expand the rectangle.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRel factor (r:Rect2D)  : Rect2D =
        if factor < 0.0  then
            fail $"Rect2D.expandRel: a negative factor {factor} is not allowed for expanding the 2D-rectangle {r.AsString}"
        let xX = r.XaxisX * factor
        let xY = r.XaxisY * factor
        let yX = r.YaxisX * factor
        let yY = r.YaxisY * factor
        Rect2D.createUnchecked(
            r.OriginX + (r.XaxisX - xX)*0.5 + (r.YaxisX - yX)*0.5,
            r.OriginY + (r.XaxisY - xY)*0.5 + (r.YaxisY - yY)*0.5,
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
        let xX = r.XaxisX * factorX
        let xY = r.XaxisY * factorX
        let yX = r.YaxisX * factorY
        let yY = r.YaxisY * factorY
        Rect2D.createUnchecked(
            r.OriginX + (r.XaxisX - xX)*0.5 + (r.YaxisX - yX)*0.5,
            r.OriginY + (r.XaxisY - xY)*0.5 + (r.YaxisY - yY)*0.5,
            xX,
            xY,
            yX,
            yY)

    /// <summary>Creates a 2D rectangle from three points. Fails if points are too close to each other or all colinear.
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y axis.
    /// If the y-point is on the left side of the X-axis the origin will be at point 0, X at point 1.
    /// If the y-point is on the right side of the X-axis, the X-axis will be reversed. the origin will be at point x, and the end of the x-Axis at the origin.
    /// E.G if called with points (origin=3,x=2,y=0) the origin will be at 2, X at 3, and y at 1.
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
    static member createFrom3Points (origin:Pt, xPt:Pt, yPt:Pt) : Rect2D =
        let xX = xPt.X - origin.X
        let xY = xPt.Y - origin.Y
        let xLenSq = xX*xX + xY*xY
        if isTooSmallSq xLenSq  then
            fail $"Rect2D.createFrom3Points: X-Point {xPt.AsString} too close to origin: X=%s{Format.float xX}|Y=%s{Format.float xY}."
        let yX = yPt.X - origin.X
        let yY = yPt.Y - origin.Y
        let yLenSq = yX*yX + yY*yY
        if isTooSmallSq yLenSq  then
            fail $"Rect2D.createFrom3Points: Y-Point {yPt.AsString} too close to origin: X=%s{Format.float yX}|Y=%s{Format.float yY}."
        let xLen = sqrt xLenSq
        let yuX = -xY / xLen
        let yuY =  xX / xLen
        //if y0 * yv < 0. then EuclidException.Raise "Euclid.Rect2D.createFrom3Points: Y-Point %s is on right side but should be on the left of X-Point %s." origin.AsString y.AsString
        let dot = yuX*yX + yuY*yY
        if isTooSmall (abs dot) then
            fail $"Rect2D.createFrom3Points: Y-Point X=%s{Format.float yX}|Y=%s{Format.float yY} is too close to Xaxis."
        let yrX = yuX * dot // get the y point projected on the y axis
        let yrY = yuY * dot
        if dot > 0. then
            Rect2D.createUnchecked(origin.X, origin.Y, xX, xY, yrX, yrY) //point is on left side of X-axis
        else
           //Rect2D.createUnchecked(origin + y, xv, -y) //alternative valid result: If the point Y is on the right side of the X-axis the origin will be at point 3, X at point 2.
           Rect2D.createUnchecked(xPt.X, xPt.Y, -xX, -xY, yrX, yrY) //the origin will be at point x, and the end of the x-Axis at the origin.

    /// <summary>Tries to create a 2D rectangle from three points. Returns None if points are too close to each other or all colinear.
    /// The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
    /// Origin and x-point define the X-axis orientation of the Rectangle.
    /// The y-point only defines the length and side of the Y axis.
    /// If the y-point is on the left side of the X-axis the origin will be at point 0, X at point 1.
    /// If the y-point is on the right side of the X-axis, the X-axis will be reversed. the origin will be at point x, and the end of the x-Axis at the origin.
    /// E.G if called with points (origin=3,x=2,y=0) the origin will be at 2, X at 3, and y at 1.
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
    static member tryCreateFrom3Points (origin:Pt, xPt:Pt, yPt:Pt) :Rect2D option=
        let xX = xPt.X - origin.X
        let xY = xPt.Y - origin.Y
        let xLenSq = xX*xX + xY*xY
        if isTooSmallSq xLenSq  then None
        else
            let yX = yPt.X - origin.X
            let yY = yPt.Y - origin.Y
            if isTooSmallSq (yX*yX + yY*yY)  then None
            else
                let xLen = sqrt xLenSq
                let yuX = -xY / xLen
                let yuY =  xX / xLen
                //if y0 * yv < 0. then EuclidException.Raise "Euclid.Rect2D.createThreePoints: Y-Point %s is on right side but should be on the left of X-Point %s." origin.AsString y.AsString
                let dot = yuX*yX + yuY*yY
                if isTooSmall (abs dot) then None
                else
                    let yrX = yuX * dot // get the y point projected on the y axis
                    let yrY = yuY * dot
                    if dot > 0. then
                        Some <| Rect2D.createUnchecked(origin.X, origin.Y, xX, xY, yrX, yrY) //point is on left side of X-axis
                    else
                        //Rect2D.createUnchecked(origin + y, xv, -y) //alternative valid result: If the point Y is on the right side of the X-axis the origin will be at point 3, X at point 2.
                        Some <| Rect2D.createUnchecked(xPt.X, xPt.Y, -xX, -xY, yrX, yrY) //the origin will be at point x, and the end of the x-Axis at the origin.

    /// Finds the oriented bounding rectangle of a set of points.
    /// The orientation of the X Axis is defined by the direction vector.
    static member createFromDirAndPoints (dirX:Vc) (pts:IList<Pt>) :Rect2D =
        let dirXLenSq = dirX.X*dirX.X + dirX.Y*dirX.Y
        if isTooSmallSq dirXLenSq then failTooSmall "Rect2D.createFromDirAndPoints dirX" dirX
        if pts.Count < 2 then fail $"Rect2D.createFromDirAndPoints: cannot create a 2D rectangle from {pts.Count} points"
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
        let xLen = sqrt(refRect.XaxisX*refRect.XaxisX + refRect.XaxisY*refRect.XaxisY)
        if isTooTiny xLen then failTooSmall "Rect2D.fitToPoints: Xaxis" (Vc(refRect.XaxisX, refRect.XaxisY))
        let yLen = sqrt(refRect.YaxisX*refRect.YaxisX + refRect.YaxisY*refRect.YaxisY)
        if isTooTiny yLen then failTooSmall "Rect2D.fitToPoints: Yaxis" (Vc(refRect.YaxisX, refRect.YaxisY))
        let xX = refRect.XaxisX / xLen
        let xY = refRect.XaxisY / xLen
        let yX = refRect.YaxisX / yLen
        let yY = refRect.YaxisY / yLen
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
        let xLen = sqrt(refRect.XaxisX*refRect.XaxisX + refRect.XaxisY*refRect.XaxisY)
        if isTooTiny xLen then failTooSmall "Rect2D.fitToPointsXY: Xaxis" (Vc(refRect.XaxisX, refRect.XaxisY))
        let yLen = sqrt(refRect.YaxisX*refRect.YaxisX + refRect.YaxisY*refRect.YaxisY)
        if isTooTiny yLen then failTooSmall "Rect2D.fitToPointsXY: Yaxis" (Vc(refRect.YaxisX, refRect.YaxisY))
        let xX = refRect.XaxisX / xLen
        let xY = refRect.XaxisY / xLen
        let yX = refRect.YaxisX / yLen
        let yY = refRect.YaxisY / yLen
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
    static member translateLocalX (distX:float) (r:Rect2D) : Rect2D =
        let len = sqrt(r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY)
        if isTooTiny len then failTooSmall $"Rect2D.translateLocalX: Xaxis" r
        let f = distX / len
        Rect2D.createUnchecked(r.OriginX + r.XaxisX*f, r.OriginY + r.XaxisY*f, r.XaxisX, r.XaxisY, r.YaxisX, r.YaxisY)

    /// Translate along the local Y-axis of the 2D Rectangle.
    static member translateLocalY (distY:float) (r:Rect2D) : Rect2D =
        let len = sqrt(r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY)
        if isTooTiny len then failTooSmall $"Rect2D.translateLocalY: Yaxis" r
        let f = distY / len
        Rect2D.createUnchecked(r.OriginX + r.YaxisX*f, r.OriginY + r.YaxisY*f, r.XaxisX, r.XaxisY, r.YaxisX, r.YaxisY)

    /// Translate by a 2D vector.(Same as Rect2D.move)
    static member translate (v:Vc) (r:Rect2D)  : Rect2D =
        Rect2D.createUnchecked(r.OriginX + v.X, r.OriginY + v.Y, r.XaxisX, r.XaxisY, r.YaxisX, r.YaxisY)

    /// Translate by a 2D vector.(Same as Rect2D.translate)
    static member move (v:Vc) (r:Rect2D)  : Rect2D =
        Rect2D.createUnchecked(r.OriginX + v.X, r.OriginY + v.Y, r.XaxisX, r.XaxisY, r.YaxisX, r.YaxisY)

    /// Rotation of a Rect2D.
    static member rotate (rot:Rotation2D) (rect:Rect2D) : Rect2D =
        Rect2D.createUnchecked(
            rot.Cos*rect.OriginX - rot.Sin*rect.OriginY,
            rot.Sin*rect.OriginX + rot.Cos*rect.OriginY,
            rot.Cos*rect.XaxisX - rot.Sin*rect.XaxisY,
            rot.Sin*rect.XaxisX + rot.Cos*rect.XaxisY,
            rot.Cos*rect.YaxisX - rot.Sin*rect.YaxisY,
            rot.Sin*rect.YaxisX + rot.Cos*rect.YaxisY)

    /// Rotation of a Rect2D. around a given center.
    static member rotateWithCenter (cen:Pt) (rot:Rotation2D) (rect:Rect2D) : Rect2D =
        let ox = rect.OriginX - cen.X
        let oy = rect.OriginY - cen.Y
        Rect2D.createUnchecked(
            rot.Cos*ox - rot.Sin*oy + cen.X,
            rot.Sin*ox + rot.Cos*oy + cen.Y,
            rot.Cos*rect.XaxisX - rot.Sin*rect.XaxisY,
            rot.Sin*rect.XaxisX + rot.Cos*rect.XaxisY,
            rot.Cos*rect.YaxisX - rot.Sin*rect.YaxisY,
            rot.Sin*rect.YaxisX + rot.Cos*rect.YaxisY)

    /// Offset a Rect2D inwards by a given distance.
    /// A negative distance will offset outwards.
    /// Fails if the distance is larger than half the size of the rectangle.
    static member offset dist (rect:Rect2D) : Rect2D =
        let xl = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let yl = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)
        if xl < dist*2.0 ||yl < dist*2.0 then
            fail $"Rect2D.offset: the 2D Rectangle {rect.AsString} is too small to offset by distance {Format.float dist}"
        let xf = dist / xl
        let yf = dist / yl
        let xX = rect.XaxisX * xf
        let xY = rect.XaxisY * xf
        let yX = rect.YaxisX * yf
        let yY = rect.YaxisY * yf
        Rect2D.createUnchecked(
            rect.OriginX + xX + yX,
            rect.OriginY + xY + yY,
            rect.XaxisX - xX*2.0,
            rect.XaxisY - xY*2.0,
            rect.YaxisX - yX*2.0,
            rect.YaxisY - yY*2.0)

    /// <summary>Offset a Rect2D inwards by four distances.
    /// Negative distances will offset outwards.
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
    static member offsetVar (dist:float[]) (rect:Rect2D) : Rect2D =
        if dist.Length <> 4 then fail $"Rect2D.offsetVar: the distance array must have 4 elements, but has {dist.Length}"
        let xl = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let yl = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)
        if xl < dist.[1]+dist.[3] ||yl < dist.[0]+dist.[2] then
            fail $"Rect2D.offsetVar: the 2D Rectangle {rect.AsString} is too small to offset by distances [|{Format.float dist.[0]};{Format.float dist.[1]};{Format.float dist.[2]};{Format.float dist.[3]}|]"
        let x0f = dist.[3] / xl
        let x1f = dist.[1] / xl
        let y0f = dist.[0] / yl
        let y1f = dist.[2] / yl
        let x0X = rect.XaxisX * x0f
        let x0Y = rect.XaxisY * x0f
        let x1X = rect.XaxisX * x1f
        let x1Y = rect.XaxisY * x1f
        let y0X = rect.YaxisX * y0f
        let y0Y = rect.YaxisY * y0f
        let y1X = rect.YaxisX * y1f
        let y1Y = rect.YaxisY * y1f
        Rect2D.createUnchecked(
            rect.OriginX + x0X + y0X,
            rect.OriginY + x0Y + y0Y,
            rect.XaxisX - x0X - x1X,
            rect.XaxisY - x0Y - x1Y,
            rect.YaxisX - y0X - y1X,
            rect.YaxisY - y0Y - y1Y)

    ///<summary>Offsets a local Rect2D at one of the four corners.</summary>
    ///<param name="rect">The 2D Rectangle</param>
    ///<param name="corner">The Index of the corner to offset
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
    ///<returns>A new 2D Rectangle. It will always have the same x and y axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetCorner (rect:Rect2D, corner:int, xOffset:float, yOffset:float, xWidth:float, yHeight:float) : Rect2D =
        let xl = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let yl = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)
        if isTooTiny xl then failTooSmall "Rect2D.offsetCorner: Xaxis" rect
        if isTooTiny yl then failTooSmall "Rect2D.offsetCorner: Yaxis" rect
        let xvf = xWidth/xl
        let yvf = yHeight/yl
        let xvX = rect.XaxisX * xvf
        let xvY = rect.XaxisY * xvf
        let yvX = rect.YaxisX * yvf
        let yvY = rect.YaxisY * yvf
        let inline create xDist yDist =
            let xf = xDist / xl
            let yf = yDist / yl
            Rect2D.createUnchecked(
                rect.OriginX + rect.XaxisX*xf + rect.YaxisX*yf,
                rect.OriginY + rect.XaxisY*xf + rect.YaxisY*yf,
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
    ///<param name="rect">The 2D Rectangle</param>
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
    ///<returns>A new 2D Rectangle. It will always have the same x and y axis orientation as the input rectangle. Independent of negative or positive offsets</returns>
    static member offsetEdge (rect:Rect2D, edgeIdx:int, offEdge:float, width:float, offStart:float, offEnd:float) : Rect2D =
        let lx = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let ly = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)

        let inline xLenX d = rect.XaxisX * (d / lx)
        let inline xLenY d = rect.XaxisY * (d / lx)
        let inline yLenX d = rect.YaxisX * (d / ly)
        let inline yLenY d = rect.YaxisY * (d / ly)

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

    /// Divides a 2D Rectangle into a grid of sub-rectangles. The sub-rectangles are returned as an array of arrays.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 2D Rectangle.
    /// The returned array has xCount elements, each element is an array of yCount sub-rectangles.
    static member subDivide (rect:Rect2D, xCount:int, yCount:int, xGap:float, yGap:float) : Rect2D array array =
        if xCount <= 0 ||yCount <= 0 then
            fail $"Rect2D.subDivide: xCount {xCount} and yCount {yCount} must be 1 or more"
        let xl = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let yl = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)
        let lx1 = (xl - xGap * float (xCount-1) )/ float xCount
        let ly1 = (yl - yGap * float (yCount-1) )/ float yCount
        if isTooTiny (lx1) || isTooTiny (ly1) then
            [||]
        else
            let vxF = lx1/xl
            let vyF = ly1/yl
            let vxX = rect.XaxisX * vxF
            let vxY = rect.XaxisY * vxF
            let vyX = rect.YaxisX * vyF
            let vyY = rect.YaxisY * vyF
            let rss = Array.zeroCreate xCount
            for ix = 0 to xCount-1 do
                let rs = Array.zeroCreate yCount
                let xF = xGap * float ix / xl + lx1 * float ix / xl
                let xX = rect.XaxisX * xF
                let xY = rect.XaxisY * xF
                for iy = 0 to yCount-1 do
                    let yF = yGap * float iy / yl + ly1 * float iy / yl
                    rs.[iy] <-
                        Rect2D.createUnchecked(
                            rect.OriginX + xX + rect.YaxisX*yF,
                            rect.OriginY + xY + rect.YaxisY*yF,
                            vxX,
                            vxY,
                            vyX,
                            vyY)
                rss.[ix] <- rs
            rss

    /// Divides a a 2D Rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 2D Rectangle.
    /// It will create as many sub-rectangles as possible respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member subDivideMinLength (rect:Rect2D, xMinLen:float, yMinLen:float, xGap:float, yGap:float) : Rect2D array array =
        let xLen = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let yLen = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)
        if xLen < xMinLen  then fail $"Rect2D.subDivideMinLength: xMinLen {xMinLen} is bigger than rect X-axis length {xLen} for {rect}"
        if yLen < yMinLen  then fail $"Rect2D.subDivideMinLength: yMinLen {yMinLen} is bigger than rect Y-axis length {yLen} for {rect}"
        let xCount = int (xLen / (xMinLen*0.9999))
        let yCount = int (yLen / (yMinLen*0.9999))
        Rect2D.subDivide (rect, xCount, yCount, xGap, yGap)

    /// Divides a a 2D Rectangle into a grid of sub-rectangles.
    /// The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 2D Rectangle.
    /// It will create as few as segments as possible respecting the maximum segment length.
    /// The input maxSegmentLength is multiplied by factor 1.00001 of to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member subDivideMaxLength (rect:Rect2D, xMaxLen:float, yMaxLen:float, xGap:float, yGap:float) : Rect2D array array =
        let xLen = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let yLen = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)
        let xCount = 1 + int (xLen / (xMaxLen*1.00001))
        let yCount = 1 + int (yLen / (yMaxLen*1.00001))
        Rect2D.subDivide (rect, xCount, yCount, xGap, yGap)

    /// Divides a 2D Rectangle into a grid of points. The points are returned as an array of arrays.
    /// A xCount and yCount of 2 will only return just the 4 corners of the rectangle.
    /// A xCount and yCount of 3 will return 9 points, including the 4 corners, the 4 mid points on the edges and the center.
    static member grid (rect:Rect2D, xCount:int, yCount:int) : Pt[][]=
        if xCount <= 1 ||yCount <= 1 then
            fail $"Rect2D.grid: xCount {xCount} and yCount {yCount} must be 2 or more"
        let xl = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let yl = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)
        let lx1 = xl / float (xCount-1)
        let ly1 = yl / float (yCount-1)
        let rss = Array.zeroCreate xCount
        for ix = 0 to xCount-1 do
            let rs = Array.zeroCreate yCount
            let xF = lx1 * float ix / xl
            let xX = rect.XaxisX * xF
            let xY = rect.XaxisY * xF
            for iy = 0 to yCount-1 do
                let yF = ly1 * float iy / yl
                rs.[iy] <- Pt(rect.OriginX + xX + rect.YaxisX*yF, rect.OriginY + xY + rect.YaxisY*yF)
            rss.[ix] <- rs
        rss

    /// Divides a a 2D Rectangle into a grid of points.
    /// It will create as many points as possible respecting the minimum side length for x and y.
    /// The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member gridMinLength (rect:Rect2D, xMinLen:float, yMinLen:float) : Pt array array =
        let xLen = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let yLen = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)
        if xLen < xMinLen  then fail $"Rect2D.gridMinLength: xMinLen {xMinLen} is bigger than rect X-axis length {xLen} for {rect}"
        if yLen < yMinLen  then fail $"Rect2D.gridMinLength: yMinLen {yMinLen} is bigger than rect Y-axis length {yLen} for {rect}"
        let xCount = 1 + int (xLen / (xMinLen*0.9999))
        let yCount = 1 + int (yLen / (yMinLen*0.9999))
        Rect2D.grid (rect, xCount, yCount)

    /// Divides a a 2D Rectangle into a grid of points.
    /// It will create as few as points as possible respecting the maximum segment length.
    /// The input maxSegmentLength is multiplied by factor 1.00001 to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    /// The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
    static member gridMaxLength (rect:Rect2D, xMaxLen:float, yMaxLen:float) : Pt array array =
        let xLen = sqrt(rect.XaxisX*rect.XaxisX + rect.XaxisY*rect.XaxisY)
        let yLen = sqrt(rect.YaxisX*rect.YaxisX + rect.YaxisY*rect.YaxisY)
        let xCount = 2 + int (xLen / (xMaxLen*1.00001))
        let yCount = 2 + int (yLen / (yMaxLen*1.00001))
        Rect2D.grid (rect, xCount, yCount)

    /// <summary>Returns the corner diagonally opposite of corner from Origin (point 2).
    /// r.Origin + r.Xaxis + r.Yaxis
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
    member inline r.FarCorner : Pt =
        Pt(r.OriginX + r.XaxisX + r.YaxisX,
           r.OriginY + r.XaxisY + r.YaxisY)

    /// Returns the far corner (point 2) of the 2D rectangle.
    static member inline farCorner (r:Rect2D) : Pt =
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
    member inline r.XCorner : Pt =
        Pt(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY)

    /// Returns the X corner (point 1) of the 2D rectangle.
    static member inline xCorner (r:Rect2D) : Pt =
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
    member inline r.YCorner : Pt =
        Pt(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY)

    /// Returns the Y corner (point 3) of the 2D rectangle.
    static member inline yCorner (r:Rect2D) : Pt =
        r.YCorner

    /// <summary>Returns point 0 of the 2D rectangle. Same as member rect.Origin.
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
    member inline r.Pt0 : Pt =
        Pt(r.OriginX, r.OriginY)

    /// Returns point 0 of the 2D rectangle.
    static member inline pt0 (r:Rect2D) : Pt =
        r.Pt0

    /// <summary>Returns point 1 of the 2D rectangle.
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
    member inline r.Pt1 : Pt =
        Pt(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY)

    /// Returns point 1 of the 2D rectangle.
    static member inline pt1 (r:Rect2D) : Pt =
        r.Pt1

    /// <summary>Returns point 2 of the 2D rectangle. Same as rect.FarCorner.
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
    member inline r.Pt2 : Pt =
        Pt(r.OriginX + r.XaxisX + r.YaxisX,
           r.OriginY + r.XaxisY + r.YaxisY)

    /// Returns point 2 of the 2D rectangle.
    static member inline pt2 (r:Rect2D) : Pt =
        r.Pt2

    /// <summary>Returns point 3 of the 2D rectangle.
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
    member inline r.Pt3 : Pt =
        Pt(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY)

    /// Returns point 3 of the 2D rectangle.
    static member inline pt3 (r:Rect2D) : Pt =
        r.Pt3

    /// <summary>Returns a 2D line from point 0 to 1 of the 2D rectangle.
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
    member inline r.Edge01 : Line2D =
        Line2D(r.OriginX, r.OriginY, r.OriginX + r.XaxisX, r.OriginY + r.XaxisY)

    /// Returns edge 0-1 of the 2D rectangle.
    static member inline edge01 (r:Rect2D) : Line2D =
        r.Edge01

    /// <summary>Returns a 2D line from point 1 to 2 of the 2D rectangle.
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
    member inline r.Edge12 : Line2D =
        let sx = r.OriginX + r.XaxisX
        let sy = r.OriginY + r.XaxisY
        Line2D(sx, sy, sx + r.YaxisX, sy + r.YaxisY)

    /// Returns edge 1-2 of the 2D rectangle.
    static member inline edge12 (r:Rect2D) : Line2D =
        r.Edge12

    /// <summary>Returns a 2D line from point 2 to 3 of the 2D rectangle.
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
    member inline r.Edge23 : Line2D =
        let p3x = r.OriginX + r.YaxisX
        let p3y = r.OriginY + r.YaxisY
        Line2D(p3x + r.XaxisX, p3y + r.XaxisY, p3x, p3y)

    /// Returns edge 2-3 of the 2D rectangle.
    static member inline edge23 (r:Rect2D) : Line2D =
        r.Edge23

    /// <summary>Returns a 2D line from point 3 to 0 of the 2D rectangle.
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
    member inline r.Edge30 : Line2D =
        Line2D(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginX, r.OriginY)

    /// Returns edge 3-0 of the 2D rectangle.
    static member inline edge30 (r:Rect2D) : Line2D =
        r.Edge30

    /// <summary>Returns the local X side as the 2D line from point 0 to 1 of the 2D rectangle.
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
    member inline r.EdgeX : Line2D =
        r.Edge01

    /// Returns the local X edge of the 2D rectangle.
    static member inline edgeX (r:Rect2D) : Line2D =
        r.EdgeX

    /// <summary>Returns the local Y side as 2D line from point 0 to 3 of the 2D rectangle.
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
    member inline r.EdgeY : Line2D =
        Line2D(r.OriginX, r.OriginY, r.OriginX + r.YaxisX, r.OriginY + r.YaxisY)

    /// Returns the local Y edge of the 2D rectangle.
    static member inline edgeY (r:Rect2D) : Line2D =
        r.EdgeY

    /// <summary>Returns the diagonal 2D line from point 0 to 2 of the 2D rectangle.
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
    member inline r.DiagonalLine : Line2D =
        Line2D(r.OriginX, r.OriginY, r.OriginX + r.YaxisX + r.XaxisX, r.OriginY + r.YaxisY + r.XaxisY)

    /// Returns the diagonal line from point 0 to point 2 of the 2D rectangle.
    static member inline diagonalLine (r:Rect2D) : Line2D =
        r.DiagonalLine

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
    member r.RotateOrientation90CW : Rect2D =
        Rect2D.createUnchecked(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, -r.YaxisX, -r.YaxisY, r.XaxisX, r.XaxisY)

    /// Returns the same rectangle with orientation rotated 90 degrees clockwise around center.
    static member inline rotateOrientation90CW (r:Rect2D) : Rect2D =
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
    member r.RotateOrientation180 : Rect2D =
        Rect2D.createUnchecked(r.OriginX + r.YaxisX + r.XaxisX, r.OriginY + r.YaxisY + r.XaxisY, -r.XaxisX, -r.XaxisY, -r.YaxisX, -r.YaxisY)

    /// Returns the same rectangle with orientation rotated 180 degrees around center.
    static member inline rotateOrientation180 (r:Rect2D) : Rect2D =
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
    member r.RotateOrientation90CCW : Rect2D =
        Rect2D.createUnchecked(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY, r.YaxisX, r.YaxisY, -r.XaxisX, -r.XaxisY)

    /// Returns the same rectangle with orientation rotated 90 degrees counter-clockwise around center.
    static member inline rotateOrientation90CCW (r:Rect2D) : Rect2D =
        r.RotateOrientation90CCW


    // #endregion
    // #region as array of points

    /// <summary>Returns the 4 corners of the 2D Rectangle in Counter-Clockwise order, starting at Origin.
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
    member r.Points :Pt[] =
        let p0 = Pt(r.OriginX, r.OriginY)
        let p1 = Pt(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY)
        let p2 = Pt(r.OriginX + r.XaxisX + r.YaxisX, r.OriginY + r.XaxisY + r.YaxisY)
        let p3 = Pt(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY)
        [| p0 ; p1 ; p2; p3|]

    /// Returns the 4 corner points of the 2D rectangle.
    static member inline points (r:Rect2D) : Pt[] =
        r.Points

    /// <summary>Returns the 4 corners of the 2D Rectangle as closed loop in Counter-Clockwise order, starting at Origin.
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
    member r.PointsLooped :Pt[] =
        let p0 = Pt(r.OriginX, r.OriginY)
        let p1 = Pt(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY)
        let p2 = Pt(r.OriginX + r.XaxisX + r.YaxisX, r.OriginY + r.XaxisY + r.YaxisY)
        let p3 = Pt(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY)
        [| p0 ; p1 ; p2; p3; p0|]

    /// Returns the 4 corner points as a closed loop.
    static member inline pointsLooped (r:Rect2D) : Pt[] =
        r.PointsLooped

    /// <summary> Returns the 4 corners of the 2D Rectangle as open loop in Counter-Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 8 floats: x and y of point 0, x and y of point 1, x and y of point 2, x and y of point 3.
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
    member r.PointsXY_CCW :ResizeArray<float> =
        let xys = ResizeArray<float>(8)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginX + r.XaxisX)
        xys.Add(r.OriginY + r.XaxisY)
        xys.Add(r.OriginX + r.XaxisX + r.YaxisX)
        xys.Add(r.OriginY + r.XaxisY + r.YaxisY)
        xys.Add(r.OriginX + r.YaxisX)
        xys.Add(r.OriginY + r.YaxisY)
        xys

    /// Returns the 4 corner points of the 2D rectangl as open loop in Counter-Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 8 floats: x and y of point 0, x and y of point 1, x and y of point 2, x and y of point 3.
    static member inline pointsXY_CCW (r:Rect2D) : ResizeArray<float> =
        r.PointsXY_CCW

    /// <summary> Returns the 4 corners of the 2D Rectangle as closed loop in Counter-Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 10 floats: x and y of point 0, x and y of point 1, x and y of point 2, x and y of point 3 and again x and y of point 0.
    /// <code>
    ///  local
    ///  Y-Axis
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
    member r.PointsXYLoopedCCW : ResizeArray<float> =
        let xys = ResizeArray<float>(10)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginX + r.XaxisX)
        xys.Add(r.OriginY + r.XaxisY)
        xys.Add(r.OriginX + r.XaxisX + r.YaxisX)
        xys.Add(r.OriginY + r.XaxisY + r.YaxisY)
        xys.Add(r.OriginX + r.YaxisX)
        xys.Add(r.OriginY + r.YaxisY)
        xys.Add(r.OriginX) // loop back to first point
        xys.Add(r.OriginY)
        xys

    /// Returns the 4 corner points as a closed loop in Counter-Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 10 floats: x and y of point 0, x and y of point 1, x and y of point 2, x and y of point 3 and again x and y of point 0.
    static member inline pointsXYLoopedCCW (r:Rect2D) : ResizeArray<float> =
        r.PointsXYLoopedCCW

    // clockwise versions of the above:

    /// <summary> Returns the 4 corners of the 2D Rectangle as open loop in Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 8 floats: x and y of point 0, x and y of point 3, x and y of point 2, x and y of point 1.
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
    member r.PointsXY_CW :ResizeArray<float> =
        let xys = ResizeArray<float>(8)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginX + r.YaxisX)
        xys.Add(r.OriginY + r.YaxisY)
        xys.Add(r.OriginX + r.XaxisX + r.YaxisX)
        xys.Add(r.OriginY + r.XaxisY + r.YaxisY)
        xys.Add(r.OriginX + r.XaxisX)
        xys.Add(r.OriginY + r.XaxisY)
        xys

    /// Returns the 4 corner points of the 2D rectangle as open loop in Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 8 floats: x and y of point 0, x and y of point 3, x and y of point 2, x and y of point 1.
    static member inline pointsXY_CW (r:Rect2D) : ResizeArray<float> =
        r.PointsXY_CW

    /// <summary> Returns the 4 corners of the 2D Rectangle as closed loop in Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 10 floats: x and y of point 0, x and y of point 3, x and y of point 2, x and y of point 1 and again x and y of point 0.
    /// <code>
    ///  local
    ///  Y-Axis
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
    member r.PointsXYLoopedCW : ResizeArray<float> =
        let xys = ResizeArray<float>(10)
        xys.Add(r.OriginX)
        xys.Add(r.OriginY)
        xys.Add(r.OriginX + r.YaxisX)
        xys.Add(r.OriginY + r.YaxisY)
        xys.Add(r.OriginX + r.XaxisX + r.YaxisX)
        xys.Add(r.OriginY + r.XaxisY + r.YaxisY)
        xys.Add(r.OriginX + r.XaxisX)
        xys.Add(r.OriginY + r.XaxisY)
        xys.Add(r.OriginX) // loop back to first point
        xys.Add(r.OriginY)
        xys

    /// Returns the 4 corner points as a closed loop in Clockwise order, starting at Origin.
    /// Returns a ResizeArray of 10 floats: x and y of point 0, x and y of point 3, x and y of point 2, x and y of point 1 and again x and y of point 0.
    static member inline pointsXYLoopedCW (r:Rect2D) : ResizeArray<float> =
        r.PointsXYLoopedCW

    /// <summary>Iterates the 4 corners of the 2D Rectangle in Counter-Clockwise order, starting at Origin.</summary>
    /// <param name="action">The action to call 4 times . Once for each corner, with x and y as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsCCW (action: float -> float -> unit) (r:Rect2D) : unit =
        action r.OriginX r.OriginY
        action (r.OriginX + r.XaxisX) (r.OriginY + r.XaxisY)
        action (r.OriginX + r.XaxisX + r.YaxisX) (r.OriginY + r.XaxisY + r.YaxisY)
        action (r.OriginX + r.YaxisX) (r.OriginY + r.YaxisY)

    /// <summary>Iterates the 4 corners of the 2D Rectangle as closed loop in Counter-Clockwise order, starting and ending at Origin.</summary>
    /// <param name="action">The action to call 5 times. With x and y as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsLoopedCCW (action: float -> float -> unit) (r:Rect2D) : unit =
        action r.OriginX r.OriginY
        action (r.OriginX + r.XaxisX) (r.OriginY + r.XaxisY)
        action (r.OriginX + r.XaxisX + r.YaxisX) (r.OriginY + r.XaxisY + r.YaxisY)
        action (r.OriginX + r.YaxisX) (r.OriginY + r.YaxisY)
        action r.OriginX r.OriginY

    /// <summary>Iterates the 4 corners of the 2D Rectangle in Clockwise order, starting at Origin.</summary>
    /// <param name="action">The action to call 4 times . Once for each corner, with x and y as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsCW (action: float -> float -> unit) (r:Rect2D) : unit =
        action r.OriginX r.OriginY
        action (r.OriginX + r.YaxisX) (r.OriginY + r.YaxisY)
        action (r.OriginX + r.XaxisX + r.YaxisX) (r.OriginY + r.XaxisY + r.YaxisY)
        action (r.OriginX + r.XaxisX) (r.OriginY + r.XaxisY)

    /// <summary>Iterates the 4 corners of the 2D Rectangle as closed loop in Clockwise order, starting and ending at Origin.</summary>
    /// <param name="action">The action to call 5 times. With x and y as parameters.</param>
    /// <param name="r">The rectangle to iterate the corners of.</param>
    static member iterPointsLoopedCW (action: float -> float -> unit) (r:Rect2D) : unit =
        action r.OriginX r.OriginY
        action (r.OriginX + r.YaxisX) (r.OriginY + r.YaxisY)
        action (r.OriginX + r.XaxisX + r.YaxisX) (r.OriginY + r.XaxisY + r.YaxisY)
        action (r.OriginX + r.XaxisX) (r.OriginY + r.XaxisY)
        action r.OriginX r.OriginY

    /// <summary>Returns the 4 Edges of the 2D Rectangle in Counter-Clockwise order, starting at Origin.
    /// Returns an array of 4 Lines: from point 0 to 1, 1 to 2 to 3 and 3 to 0.
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
    member r.Edges :Line2D[] =
        let p0x = r.OriginX
        let p0y = r.OriginY
        let p1x = p0x + r.XaxisX
        let p1y = p0y + r.XaxisY
        let p2x = p1x + r.YaxisX
        let p2y = p1y + r.YaxisY
        let p3x = p0x + r.YaxisX
        let p3y = p0y + r.YaxisY
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
    member r.GetEdge i : Line2D =
        match i with
        | 0 -> Line2D(r.OriginX, r.OriginY, r.OriginX + r.XaxisX, r.OriginY + r.XaxisY)
        | 1 -> Line2D(r.OriginX + r.XaxisX, r.OriginY + r.XaxisY, r.OriginX + r.XaxisX + r.YaxisX, r.OriginY + r.XaxisY + r.YaxisY)
        | 2 -> Line2D(r.OriginX + r.XaxisX + r.YaxisX, r.OriginY + r.XaxisY + r.YaxisY, r.OriginX + r.YaxisX, r.OriginY + r.YaxisY)
        | 3 -> Line2D(r.OriginX + r.YaxisX, r.OriginY + r.YaxisY, r.OriginX, r.OriginY)
        | _ -> fail $"Rect2D.GetEdge: index {i} out of range 0..3" |> unbox // unbox to make type checker happy

    /// Returns one edge of the 2D rectangle by index 0..3.
    static member inline getEdge (i:int) (r:Rect2D) : Line2D =
        r.GetEdge i

    // #endregion
    // #region Line Intersection

    /// Internal Liang–Barsky clip of a 2D line against this 2D Rectangle.
    /// The rectangle is enlarged by 'tol' on each side so that lines grazing an edge or corner still count as touching.
    /// If 'asRay' is TRUE the line is treated as infinite in both directions, otherwise as the finite segment From-To.
    /// Returns the two endpoints (x0,y0,x1,y1) of the portion inside the rectangle, or ValueNone if it is fully outside.
    /// Fails if the rectangle has a zero length axis or the line is too short.
    static member private clipLine2D (r:Rect2D, line:Line2D, asRay:bool, tol:float) : ValueOption<struct(float*float*float*float)> =
        let xlenSq = r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY
        let ylenSq = r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY
        if isTooSmallSq xlenSq then failTooSmall "Rect2D line intersection: Xaxis" r
        if isTooSmallSq ylenSq then failTooSmall "Rect2D line intersection: Yaxis" r
        let xlen = sqrt xlenSq
        let ylen = sqrt ylenSq
        let uxx = r.XaxisX / xlen // unit X-axis of the rectangle
        let uxy = r.XaxisY / xlen
        let uyx = r.YaxisX / ylen // unit Y-axis of the rectangle
        let uyy = r.YaxisY / ylen
        let vx = line.ToX - line.FromX
        let vy = line.ToY - line.FromY
        let lenSq = vx*vx + vy*vy
        if isTooSmallSq lenSq then failTooSmall "Rect2D line intersection: line is too short" line
        let len = sqrt lenSq
        let ndx = vx / len // unit direction of the line
        let ndy = vy / len
        let ox = line.FromX - r.OriginX
        let oy = line.FromY - r.OriginY
        let p0u = ox*uxx + oy*uxy   // local X distance of the From point
        let p0v = ox*uyx + oy*uyy   // local Y distance of the From point
        let du  = ndx*uxx + ndy*uxy // local X component of the unit direction
        let dv  = ndx*uyx + ndy*uyy // local Y component of the unit direction
        // Liang–Barsky: walk the line parameter t (a distance because the direction is unitized)
        // from tE (entering) to tL (leaving). Each box boundary is an inequality  p*t <= q .
        // 'tol' enlarges the box by that distance on all four sides.
        let mutable tE = if asRay then -infinity else 0.0
        let mutable tL = if asRay then  infinity else len
        let mutable ok = true
        let mutable p = -du                  // boundary  local-X >= -tol
        let mutable q = p0u + tol
        if   abs p <= 1e-12 then (if q < 0.0 then ok <- false)
        elif p < 0.0        then (let t = q/p in if t > tL then ok <- false elif t > tE then tE <- t)
        else                     (let t = q/p in if t < tE then ok <- false elif t < tL then tL <- t)
        p <- du                              // boundary  local-X <= xlen + tol
        q <- xlen + tol - p0u
        if   abs p <= 1e-12 then (if q < 0.0 then ok <- false)
        elif p < 0.0        then (let t = q/p in if t > tL then ok <- false elif t > tE then tE <- t)
        else                     (let t = q/p in if t < tE then ok <- false elif t < tL then tL <- t)
        p <- -dv                             // boundary  local-Y >= -tol
        q <- p0v + tol
        if   abs p <= 1e-12 then (if q < 0.0 then ok <- false)
        elif p < 0.0        then (let t = q/p in if t > tL then ok <- false elif t > tE then tE <- t)
        else                     (let t = q/p in if t < tE then ok <- false elif t < tL then tL <- t)
        p <- dv                              // boundary  local-Y <= ylen + tol
        q <- ylen + tol - p0v
        if   abs p <= 1e-12 then (if q < 0.0 then ok <- false)
        elif p < 0.0        then (let t = q/p in if t > tL then ok <- false elif t > tE then tE <- t)
        else                     (let t = q/p in if t < tE then ok <- false elif t < tL then tL <- t)
        if ok && tE <= tL then
            ValueSome (struct(line.FromX + tE*ndx, line.FromY + tE*ndy, line.FromX + tL*ndx, line.FromY + tL*ndy))
        else
            ValueNone

    /// <summary>Tests if a finite 2D line (the segment From-To) touches or crosses this 2D Rectangle.
    /// The rectangle is enlarged by the tolerance, so a line grazing an edge or corner still counts.</summary>
    /// <param name="line">The finite 2D line to test against this rectangle.</param>
    /// <param name="tol">Optional tolerance, default 1e-6. The rectangle is enlarged by this distance on each of its four sides.</param>
    /// <returns>TRUE if any part of the line segment lies inside the (tolerance enlarged) rectangle. Otherwise FALSE.</returns>
    member r.Intersects(line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : bool =
        match Rect2D.clipLine2D(r, line, false, tol) with
        | ValueSome _ -> true
        | ValueNone   -> false

    /// <summary>Returns the points where a finite 2D line (the segment From-To) enters and leaves this 2D Rectangle.
    /// The rectangle is enlarged by the tolerance, so a line grazing an edge or corner still counts.
    /// The returned points are the endpoints of the portion of the line inside the rectangle, so a segment
    /// endpoint that lies inside the rectangle is returned as such.</summary>
    /// <param name="line">The finite 2D line to intersect with this rectangle.</param>
    /// <param name="tol">Optional tolerance, default 1e-6. The rectangle is enlarged by this distance on each of its four sides.</param>
    /// <returns>An array of 0, 1 or 2 points: empty if the line is fully outside, one point if it just grazes a corner
    /// (within the tolerance), otherwise the two points bounding the inside portion.</returns>
    member r.IntersectionPoints(line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : Pt[] =
        match Rect2D.clipLine2D(r, line, false, tol) with
        | ValueNone -> [| |]
        | ValueSome (struct(x0,y0,x1,y1)) ->
            let dx = x1-x0
            let dy = y1-y0
            if dx*dx + dy*dy <= tol*tol then [| Pt((x0+x1)*0.5, (y0+y1)*0.5) |] // the line only grazes a corner
            else [| Pt(x0,y0); Pt(x1,y1) |]

    /// <summary>Returns the portion of a finite 2D line (the segment From-To) that lies inside this 2D Rectangle.
    /// The rectangle is enlarged by the tolerance, so a line grazing an edge or corner still counts.</summary>
    /// <param name="line">The finite 2D line to clip to this rectangle.</param>
    /// <param name="tol">Optional tolerance, default 1e-6. The rectangle is enlarged by this distance on each of its four sides.</param>
    /// <returns>ValueSome with the clipped 2D line if any part lies inside the rectangle (this may be a zero length line
    /// if the line only grazes a corner). ValueNone if the line is fully outside.</returns>
    member r.IntersectionLine(line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : Line2D voption =
        match Rect2D.clipLine2D(r, line, false, tol) with
        | ValueNone -> ValueNone
        | ValueSome (struct(x0,y0,x1,y1)) -> ValueSome (Line2D(x0,y0,x1,y1))

    /// <summary>Tests if the infinite 2D line through the given finite line touches or crosses this 2D Rectangle.
    /// The line is extended infinitely in both directions. The rectangle is enlarged by the tolerance, so a line
    /// grazing an edge or corner still counts.</summary>
    /// <param name="line">A finite 2D line defining the infinite line by its start point and direction.</param>
    /// <param name="tol">Optional tolerance, default 1e-6. The rectangle is enlarged by this distance on each of its four sides.</param>
    /// <returns>TRUE if the infinite line passes through the (tolerance enlarged) rectangle. Otherwise FALSE.</returns>
    member r.RayIntersects(line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : bool =
        match Rect2D.clipLine2D(r, line, true, tol) with
        | ValueSome _ -> true
        | ValueNone   -> false

    /// <summary>Returns the points where the infinite 2D line through the given finite line enters and leaves this 2D Rectangle.
    /// The line is extended infinitely in both directions. The rectangle is enlarged by the tolerance, so a line
    /// grazing an edge or corner still counts.</summary>
    /// <param name="line">A finite 2D line defining the infinite line by its start point and direction.</param>
    /// <param name="tol">Optional tolerance, default 1e-6. The rectangle is enlarged by this distance on each of its four sides.</param>
    /// <returns>An array of 0, 1 or 2 points: empty if the line misses the rectangle, one point if it just grazes a corner
    /// (within the tolerance), otherwise the two boundary points.</returns>
    member r.RayIntersectionPoints(line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : Pt[] =
        match Rect2D.clipLine2D(r, line, true, tol) with
        | ValueNone -> [| |]
        | ValueSome (struct(x0,y0,x1,y1)) ->
            let dx = x1-x0
            let dy = y1-y0
            if dx*dx + dy*dy <= tol*tol then [| Pt((x0+x1)*0.5, (y0+y1)*0.5) |] // the line only grazes a corner
            else [| Pt(x0,y0); Pt(x1,y1) |]

    /// <summary>Returns the chord of the infinite 2D line through the given finite line inside this 2D Rectangle.
    /// The line is extended infinitely in both directions. The rectangle is enlarged by the tolerance, so a line
    /// grazing an edge or corner still counts.</summary>
    /// <param name="line">A finite 2D line defining the infinite line by its start point and direction.</param>
    /// <param name="tol">Optional tolerance, default 1e-6. The rectangle is enlarged by this distance on each of its four sides.</param>
    /// <returns>ValueSome with the chord as a 2D line if the infinite line passes through the rectangle (this may be a
    /// zero length line if it only grazes a corner). ValueNone if the line misses the rectangle.</returns>
    member r.RayIntersectionLine(line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : Line2D voption =
        match Rect2D.clipLine2D(r, line, true, tol) with
        | ValueNone -> ValueNone
        | ValueSome (struct(x0,y0,x1,y1)) -> ValueSome (Line2D(x0,y0,x1,y1))

    /// Tests if a finite 2D line (the segment From-To) touches or crosses the 2D Rectangle.
    /// The rectangle is enlarged by the tolerance (default 1e-6), so a line grazing an edge or corner still counts.
    static member intersects (rect:Rect2D, line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : bool =
        rect.Intersects(line, tol)

    /// Returns the 0, 1 or 2 points where a finite 2D line (the segment From-To) enters and leaves the 2D Rectangle.
    /// The rectangle is enlarged by the tolerance (default 1e-6), so a line grazing an edge or corner still counts.
    static member intersectionPoints (rect:Rect2D, line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : Pt[] =
        rect.IntersectionPoints(line, tol)

    /// Returns the portion of a finite 2D line (the segment From-To) that lies inside the 2D Rectangle.
    /// The rectangle is enlarged by the tolerance (default 1e-6), so a line grazing an edge or corner still counts.
    static member intersectionLine (rect:Rect2D, line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : Line2D voption =
        rect.IntersectionLine(line, tol)

    /// Tests if the infinite 2D line through the given finite line touches or crosses the 2D Rectangle.
    /// The rectangle is enlarged by the tolerance (default 1e-6), so a line grazing an edge or corner still counts.
    static member rayIntersects (rect:Rect2D, line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : bool =
        rect.RayIntersects(line, tol)

    /// Returns the 0, 1 or 2 points where the infinite 2D line through the given finite line enters and leaves the 2D Rectangle.
    /// The rectangle is enlarged by the tolerance (default 1e-6), so a line grazing an edge or corner still counts.
    static member rayIntersectionPoints (rect:Rect2D, line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : Pt[] =
        rect.RayIntersectionPoints(line, tol)

    /// Returns the chord of the infinite 2D line through the given finite line inside the 2D Rectangle.
    /// The rectangle is enlarged by the tolerance (default 1e-6), so a line grazing an edge or corner still counts.
    static member rayIntersectionLine (rect:Rect2D, line:Line2D, [<OPT;DEF(1e-6)>] tol:float) : Line2D voption =
        rect.RayIntersectionLine(line, tol)

    // #endregion
    // #region Obsolete

    [<Obsolete("use SizeX")>]
    member inline r.Width : float =
        sqrt(r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY)

    [<Obsolete("use SizeY")>]
    member inline r.Height2D : float =
        sqrt(r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY)

    [<Obsolete("This does not scale proportionally to the actual area, use just .Area for sorting by area")>]
    member inline r.AreaSq : float =
        (r.XaxisX*r.XaxisX + r.XaxisY*r.XaxisY) * (r.YaxisX*r.YaxisX + r.YaxisY*r.YaxisY)

