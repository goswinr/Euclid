namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid
open EuclidErrors


/// <summary>A struct of 4 floats representing an immutable 2D bounding rectangle.
/// Sometimes also called 2D a bounding box.
/// This implementation guarantees the rectangle to be always valid.
/// That means the Min X and Y values are always smaller or equal than the respective Max values.
/// <code>
///   Y-Axis
///   ^
///   |
///   |             2 max X,Y
/// 3 +------------+
///   |            |
///   |            |
///   |            |
///   |            |
///   |            |       local
///   +------------+-----> X-Axis
///  0-min X,Y      1
/// </code>
/// </summary>
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type BRect =
    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The fields holding the minimum X value of this bounding rectangle.
    [<DataMember>]
    val MinX : float
    /// The fields holding the minimum Y value of this bounding rectangle.
    [<DataMember>]
    val MinY : float
    /// The fields holding the maximum X value of this bounding rectangle.
    [<DataMember>]
    val MaxX : float
    /// The fields holding the maximum Y value of this bounding rectangle.
    [<DataMember>]
    val MaxY : float


    /// Unsafe internal constructor, public only for inlining.
    [<Obsolete("This is not Obsolete, but an unsafe internal constructor. the input is not verified, so it might create invalid geometry. It is exposed as a public member so that it can be inlined.") >]
    new (minX, minY, maxX, maxY) =
        {MinX = minX
         MinY = minY
         MaxX = maxX
         MaxY = maxY}


    /// Does not verify the order of min and max values.
    /// Creates a new bounding rectangle.
    static member inline createUnchecked (minX, minY, maxX, maxY) : BRect =
        #nowarn "44"
        BRect(minX, minY, maxX, maxY)
        #warnon "44" // re-enable warning for obsolete usage

    /// Nicely formatted string representation of the bounding rectangle, including its size.
    override r.ToString() : string =
        let sizeX = Format.float (r.MaxX - r.MinX)
        let sizeY = Format.float (r.MaxY - r.MinY)
        let atX = Format.float r.MinX
        let atY = Format.float r.MinY
        $"Euclid.BRect: sizeX=%s{sizeX}| sizeY=%s{sizeY}| at X=%s{atX}|Y=%s{atY}"

    /// Nicely formatted string representation of the bounding rectangle, including its size.
    static member inline toString (r:BRect) : string =
        r.ToString()

    /// Format bounding rectangle into string with nice floating point number formatting of size and position.
    /// But without full type name as in rect.ToString()
    member r.AsString : string =
        let sizeX = Format.float (r.MaxX - r.MinX)
        let sizeY = Format.float (r.MaxY - r.MinY)
        let atX = Format.float r.MinX
        let atY = Format.float r.MinY
        $"sizeX=%s{sizeX}| sizeY=%s{sizeY}| at X=%s{atX}|Y=%s{atY}"

    /// Format bounding rectangle into string with nice floating point number formatting of size and position.
    /// But without full type name as in rect.ToString()
    static member inline asString (r:BRect) : string =
        r.AsString

    /// Format bounding rectangle into an F# code string that can be used to recreate the rectangle.
    member r.AsFSharpCode : string =
        $"BRect.createUnchecked({r.MinX}, {r.MinY}, {r.MaxX}, {r.MaxY})"

    /// Format bounding rectangle into an F# code string that can be used to recreate the rectangle.
    static member inline asFSharpCode (r:BRect) : string =
        r.AsFSharpCode

    /// The point where X and Y are the minimum values.
    member inline r.MinPt : Pt =
        Pt(r.MinX, r.MinY)

    /// The point where X and Y are the minimum values.
    static member inline minPt (r:BRect) : Pt =
        r.MinPt

    /// The point where X and Y are the maximum values.
    member inline r.MaxPt : Pt =
        Pt(r.MaxX, r.MaxY)

    /// The point where X and Y are the maximum values.
    static member inline maxPt (r:BRect) : Pt =
        r.MaxPt

    /// The size in X direction.
    member inline r.SizeX : float =
        r.MaxX - r.MinX

    /// The size in X direction.
    static member inline sizeX (r:BRect) : float =
        r.SizeX

    /// The size in Y direction.
    member inline r.SizeY : float =
        r.MaxY - r.MinY

    /// The size in Y direction.
    static member inline sizeY (r:BRect) : float =
        r.SizeY

    /// The diagonal 2D vector of the bounding rect. From MinPt to MaxPt.
    member inline r.Diagonal : Vc =
        Vc(r.MaxX - r.MinX, r.MaxY - r.MinY)

    /// The diagonal 2D vector of the bounding rect. From MinPt to MaxPt.
    static member inline diagonal (r:BRect) : Vc =
        r.Diagonal

    /// The center of the bounding rect.
    member inline r.Center : Pt =
        Pt( (r.MaxX + r.MinX) * 0.5,
            (r.MaxY + r.MinY) * 0.5)

    /// The center of the bounding rect.
    static member inline center (r:BRect) : Pt =
        r.Center

    /// Returns a bounding rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.Expand(dist) : BRect =
        let n = BRect.createUnchecked(r.MinX-dist, r.MinY-dist, r.MaxX+dist, r.MaxY+dist)
        if dist<0. &&  (n.MinX > n.MaxX || n.MinY > n.MaxY) then
            fail $"BRect.Expand(dist): Negative distance {dist} causes an underflow, on {r.AsString}"
        n

    /// Returns bounding rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member inline expand (dist:float) (r:BRect) : BRect =
        r.Expand dist

    /// Returns a bounding rectangle expanded by a distance for X and Y-axis each.
    /// Raises EuclidException if the resulting rectangle would be invalid (Min > Max).
    member inline r.ExpandXY(xDist, yDist) : BRect =
        let n = BRect.createUnchecked(r.MinX-xDist, r.MinY-yDist, r.MaxX+xDist, r.MaxY+yDist)
        if n.MinX > n.MaxX ||  n.MinY > n.MaxY then
            fail $"BRect.Expand(x, y): Distance(s) X: {xDist} and Y: {yDist} cause an underflow, on {r.AsString}"
        n

    /// Returns a bounding rectangle expanded by a distance for X and Y-axis each.
    static member inline expandXY (xDist:float) (yDist:float) (r:BRect) : BRect =
        r.ExpandXY(xDist, yDist)

    /// Returns a bounding rectangle expanded by a distance for X and Y-axis each.
    /// If expansion is negative it shrinks the Rectangle. It also prevents the rectangle from collapsing past its center.
    /// When the negative expansion is bigger than the size, Min and Max values will be both at the center point.
    member b.ExpandSafeXY(xDist:float, yDist:float) : BRect =
        let mutable minXCh = b.MinX - xDist
        let mutable maxXCh = b.MaxX + xDist
        if minXCh > maxXCh then  // Overflow! Set both to the same mid point
            let mid = b.MinX + (b.MaxX-b.MinX) * 0.5
            minXCh <- mid
            maxXCh <- mid
        // expand Y:
        let mutable minYCh = b.MinY - yDist
        let mutable maxYCh = b.MaxY + yDist
        if minYCh > maxYCh then  // Overflow! Set both to the same mid point
            let mid = b.MinY + (b.MaxY-b.MinY) * 0.5
            minYCh <- mid
            maxYCh <- mid
        BRect.createUnchecked(minXCh, minYCh, maxXCh, maxYCh)

    /// Returns a bounding rectangle expanded by a distance for X and Y-axis each.
    /// If expansion is negative it shrinks the Rectangle. It also prevents the rectangle from collapsing past its center.
    /// When the negative expansion is bigger than the size, Min and Max values will be both at the center point.
    static member inline expandSafeXY (xDist:float) (yDist:float) (r:BRect) : BRect =
        r.ExpandSafeXY(xDist, yDist)

    /// Returns a bounding rectangle expanded by a distance.
    /// If expansion is negative it shrinks the Rectangle. It also prevents the rectangle from collapsing past its center.
    /// When the negative expansion is bigger than the size, Min and Max values will be both at the center point.
    member inline b.ExpandSafe(dist:float) : BRect =
        b.ExpandSafeXY(dist, dist)

    /// Returns a bounding rectangle expanded by a distance.
    /// If expansion is negative it shrinks the Rectangle. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    static member inline expandSafe (dist:float) (r:BRect) : BRect =
        r.ExpandSafeXY(dist, dist)

    /// Returns a bounding rectangle expanded only in X direction by different distances for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.ExpandXaxis(startDist, endDist) : BRect =
        let n = BRect.createUnchecked(r.MinX-startDist, r.MinY, r.MaxX+endDist, r.MaxY)
        if n.MinX > n.MaxX then
            fail $"BRect.ExpandXaxis: Negative distances for start({startDist}) and end ({endDist}) cause an underflow, on {r.AsString}"
        n

    /// Returns bounding rectangle expanded only in X direction by different distances for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member inline expandXaxis startDist endDist (r:BRect) : BRect =
        r.ExpandXaxis(startDist, endDist)

    /// Returns a bounding rectangle expanded only in Y direction by different distances for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.ExpandYaxis(startDist, endDist) : BRect =
        let n = BRect.createUnchecked(r.MinX, r.MinY-startDist, r.MaxX, r.MaxY+endDist)
        if n.MinY > n.MaxY then
            fail $"BRect.ExpandYaxis: Negative distances for start({startDist}) and end({endDist}) cause an underflow, on {r.AsString}"
        n

    /// Returns bounding rectangle expanded only in Y direction by different distances for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member inline expandYaxis startDist endDist (r:BRect) : BRect =
        r.ExpandYaxis(startDist, endDist)

    /// Returns TRUE if the two 2D bounding rectangles do overlap or touch exactly.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    member inline b.IsOverlapping (a:BRect) : bool =
        a.MinX <= b.MaxX &&
        b.MinX <= a.MaxX &&
        a.MinY <= b.MaxY &&
        b.MinY <= a.MaxY

    /// Returns TRUE if the two 2D bounding rectangles do overlap or touch exactly.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    static member inline isOverlapping (a:BRect) (b:BRect) : bool =
        b.IsOverlapping a

    /// Returns TRUE if the two 2D bounding rectangles do overlap or are apart less than the specified tolerance.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    /// A negative tolerance shrinks the effective boundaries instead of expanding them. The math works out symmetrically:
    member inline b.IsOverlappingOrClose (a:BRect, tolerance:float) : bool =
        a.MinX - tolerance <= b.MaxX &&
        b.MinX - tolerance <= a.MaxX &&
        a.MinY - tolerance <= b.MaxY &&
        b.MinY - tolerance <= a.MaxY

    /// Returns TRUE if the two 2D bounding rectangles do overlap or are apart less than the specified tolerance.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    /// A negative tolerance shrinks the effective boundaries instead of expanding them. The math works out symmetrically:
    static member inline isOverlappingOrClose (tolerance:float) (a:BRect) (b:BRect) : bool =
        b.IsOverlappingOrClose(a, tolerance)

    /// Returns TRUE if the two 2D bounding rectangles are just touching within the tolerance.
    /// The default tolerance is 1e-6.But can also be specified as a parameter.
    /// Returns FALSE if one box is completely inside the other.
    /// Returns FALSE if one box is completely surrounding the other.
    member inline this.IsTouching (other:BRect, [<OPT;DEF(1e-6)>] tolerance:float) : bool =
        // Signed face-gaps: positive = faces are apart, negative = faces penetrate
        let g1 = this.MinX - other.MaxX
        let g2 = other.MinX - this.MaxX
        let g3 = this.MinY - other.MaxY
        let g4 = other.MinY - this.MaxY
        // Per-axis separation: >0 = apart, <0 = overlap
        let gx = max g1 g2
        let gy = max g3 g4
        if gx > tolerance || gy > tolerance  then
            false  // fully apart on at least one axis
        elif gx < 0.0 && gy < 0.0  then
            false // share area
        else
            true

    /// Returns TRUE if the two 2D bounding rectangles are just touching within a given specified tolerance.
    /// Returns FALSE if one box is completely inside the other.
    /// Returns FALSE if one box is completely surrounding the other.
    static member inline isTouching (tolerance:float) (other:BRect) (this:BRect) : bool =
        this.IsTouching(other, tolerance)

    /// Returns a value from -1 to 6 indicating the side on which the two 2D
    /// are just touching within the tolerance.
    /// The default tolerance is 1e-6.But can also be specified as a parameter.
    ///   -1 : not touching but overlapping / one inside the other
    ///    0 : not touching, apart on at least one axis by more than tolerance
    ///    1 : other is in front  of this (this.MinX touches other.MaxX)
    ///    2 : other is behind    this  (this.MaxX touches other.MinX)
    ///    3 : other is to the left  of this (this.MinY touches other.MaxY)
    ///    4 : other is to the right of this (this.MaxY touches other.MinY)
    ///    5 : this.MinZ touches other.MaxZ
    ///    6 : this.MaxZ touches other.MinZ
    member inline this.TouchingSide (other:BRect, [<OPT;DEF(1e-6)>] tolerance:float) : int =
        // Signed face-gaps: positive = faces are apart, negative = faces penetrate
        let g1 = this.MinX - other.MaxX
        let g2 = other.MinX - this.MaxX
        let g3 = this.MinY - other.MaxY
        let g4 = other.MinY - this.MaxY
        // Per-axis separation: >0 = apart, <0 = overlap
        let gx = max g1 g2
        let gy = max g3 g4
        if gx > tolerance || gy > tolerance then
            0  // fully apart on at least one axis
        elif gx < 0.0 && gy < 0.0  then
            -1 // share area
        else
            // At least one axis has a face-gap in [0, tolerance]. Pick the tightest.
            let mutable side = 0
            let mutable best = System.Double.PositiveInfinity
            if g1 >= 0.0 && g1 < best then side <- 1; best <- g1
            if g2 >= 0.0 && g2 < best then side <- 2; best <- g2
            if g3 >= 0.0 && g3 < best then side <- 3; best <- g3
            if g4 >= 0.0 && g4 < best then side <- 4; best <- g4
            side

    /// Returns a value from -1 to 6 indicating the side on which the two 2D
    /// are just touching within a given specified tolerance.
    ///   -1 : not touching but overlapping / one inside the other
    ///    0 : not touching, apart on at least one axis by more than tolerance
    ///    1 : other is in front  of this (this.MinX touches other.MaxX)
    ///    2 : other is behind    this  (this.MaxX touches other.MinX)
    ///    3 : other is to the left  of this (this.MinY touches other.MaxY)
    ///    4 : other is to the right of this (this.MaxY touches other.MinY)
    ///    5 : this.MinZ touches other.MaxZ
    ///    6 : this.MaxZ touches other.MinZ
    static member inline touchingSide (other:BRect) (tolerance:float) (this:BRect) : int =
        this.TouchingSide(other, tolerance)


    // /// Returns TRUE if the two 2D bounding rectangles overlap by at least the specified threshold on every axis.
    // /// (See note: when one box is smaller than the threshold on some axis but fully inside the other,
    // ///  this returns FALSE.)
    // member inline b.IsOverlappingMoreThan (a:Brect, threshold:float) : bool =
    //     (min a.MaxX b.MaxX) - (max a.MinX b.MinX) >= threshold &&
    //     (min a.MaxY b.MaxY) - (max a.MinY b.MinY) >= threshold &&
    //     (min a.MaxZ b.MaxZ) - (max a.MinZ b.MinZ) >= threshold

    // /// Returns TRUE if the two 2D bounding rectangles overlap by at least the specified threshold on every axis.
    // /// Also returns TRUE if one box is completely inside the other (even if smaller than the threshold).
    // /// Also returns TRUE if one box is completely surrounding the other.
    // member inline b.IsOverlappingMoreThan (a:Brect, threshold:float) : bool =
    //     (   (min a.MaxX b.MaxX) - (max a.MinX b.MinX) >= threshold &&
    //         (min a.MaxY b.MaxY) - (max a.MinY b.MinY) >= threshold &&
    //         (min a.MaxZ b.MaxZ) - (max a.MinZ b.MinZ) >= threshold    )
    //     ||
    //     (   a.MinX >= b.MinX && a.MaxX <= b.MaxX &&
    //         a.MinY >= b.MinY && a.MaxY <= b.MaxY &&
    //         a.MinZ >= b.MinZ && a.MaxZ <= b.MaxZ    )
    //     ||
    //     (   b.MinX >= a.MinX && b.MaxX <= a.MaxX &&
    //         b.MinY >= a.MinY && b.MaxY <= a.MaxY &&
    //         b.MinZ >= a.MinZ && b.MaxZ <= a.MaxZ    )

    /// Returns TRUE if the point is inside or exactly on this bounding rectangle.
    member inline r.ContainsXY (x:float, y:float) : bool =
        x >= r.MinX &&
        x <= r.MaxX &&
        y >= r.MinY &&
        y <= r.MaxY

    /// Returns TRUE if the point is inside or on this bounding rectangle.
    static member inline containsXY (x:float) (y:float) (rect:BRect) : bool =
        rect.ContainsXY (x, y)

    /// Returns TRUE if the point is inside or exactly on this bounding rectangle.
    member inline r.ContainsPt (p:Pt) : bool =
        r.ContainsXY (p.X, p.Y)

    /// Returns TRUE if the point is inside or on this bounding rectangle.
    static member inline containsPt (pt:Pt) (rect:BRect) : bool =
        rect.ContainsPt pt

    /// Returns TRUE if the Rectangle is inside or exactly on the other bounding rectangle.
    member inline r.Contains (o:BRect) : bool =
        r.ContainsXY (o.MinX, o.MinY) && r.ContainsXY (o.MaxX, o.MaxY)

    /// Returns TRUE if this bounding rectangle is inside or exactly on the other bounding rectangle.
    /// Argument order matters!
    static member inline contains (rectInside:BRect) (surroundingRect:BRect) : bool =
        surroundingRect.Contains rectInside

    /// Evaluate a X and Y parameter of this bounding rectangle.
    ///  0.0, 0.0 returns the MinPt.
    ///  1.0, 1.0 returns the MaxPt.
    member inline b.EvaluateAt (xParameter, yParameter) : Pt =
        Pt (b.MinX + (b.MaxX-b.MinX) * xParameter,
            b.MinY + (b.MaxY-b.MinY) * yParameter)

    /// Evaluate a X and Y parameter of this bounding rectangle.
    /// 0.0, 0.0 returns the MinPt.
    /// 1.0, 1.0 returns the MaxPt.
    static member inline evaluateAt xParameter yParameter (r:BRect) : Pt =
        r.EvaluateAt(xParameter, yParameter)

    /// Returns the area of this bounding rectangle.
    member inline r.Area : float =
        r.SizeX * r.SizeY

    /// Returns the area of this bounding rectangle.
    static member inline area (r:BRect) : float =
        r.SizeX * r.SizeY

    /// Returns the longest edge of the Box.
    member inline b.LongestEdge : float =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        max x y

    /// Returns the longest edge of the Box.
    static member inline longestEdge (r:BRect) : float =
        r.LongestEdge

    /// Returns the shortest edge of the Box.
    member inline b.ShortestEdge : float =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        min x y

    /// Returns the shortest edge of the Box.
    static member inline shortestEdge (r:BRect) : float =
        r.ShortestEdge

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsPoint.
    member inline b.IsZero : bool =
        isTooTiny (b.MaxX - b.MinX) &&
        isTooTiny (b.MaxY - b.MinY)

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsPoint.
    static member inline isZero (r:BRect) : bool =
        r.IsZero

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsZero.
    member inline b.IsPoint : bool =
        b.IsZero

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsZero.
    static member inline isPoint (r:BRect) : bool =
        r.IsPoint

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, or 2.
    member inline b.CountZeroSides : int =
        countTooTinyOrNaN    (b.MaxX - b.MinX)
        +  countTooTinyOrNaN (b.MaxY - b.MinY)

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, or 2.
    static member inline countZeroSides (r:BRect) : int =
        r.CountZeroSides

    /// Tests if one of the X or Y axis is smaller than the zeroLength tolerance.
    member inline b.IsLine : bool =
        b.CountZeroSides = 1

    /// Tests if one of the X or Y axis is smaller than the zeroLength tolerance.
    static member inline isLine (r:BRect) : bool =
        r.IsLine

    /// Tests if no sides of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .HasArea
    member inline b.IsValid : bool =
        b.CountZeroSides = 0

    /// Tests if no sides of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .HasArea.
    static member inline isValid (r:BRect) : bool =
        r.IsValid

    /// Tests if none of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasArea : bool =
        b.CountZeroSides = 0

    /// Tests if none of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid.
    static member inline hasArea (r:BRect) : bool =
        r.HasArea

    /// Returns a bounding rectangle that contains both input Rectangles.
    member inline r.Union (b:BRect) : BRect =
        BRect.createUnchecked(min b.MinX r.MinX, min b.MinY r.MinY, max b.MaxX r.MaxX, max b.MaxY r.MaxY)

    /// Returns a bounding rectangle that contains the input Rectangles and the point.
    member inline r.UnionPt (p:Pt) : BRect =
        BRect.createUnchecked(min r.MinX p.X, min r.MinY p.Y, max r.MaxX p.X, max r.MaxY p.Y)

    /// Returns a bounding rectangle that contains both input Rectangles.
    static member inline union (a:BRect) (b:BRect) : BRect =
        a.Union b

    /// Returns a bounding rectangle that contains the input Rectangles and the point.
    static member inline unionPt (p:Pt) (r:BRect) : BRect =
        r.UnionPt p

    /// Returns the intersection of two bounding rectangles.
    /// The returned Rectangle is the area that is inside both input Rectangles.
    /// Returns ValueNone if the two Rectangles do not overlap.
    /// Just touching Rectangles will return ValueSome with zero area collapsed BRect.
    member inline b.Intersection (a:BRect) : BRect voption =
        let minX = max a.MinX b.MinX
        let minY = max a.MinY b.MinY
        let maxX = min a.MaxX b.MaxX
        let maxY = min a.MaxY b.MaxY
        if minX <= maxX && minY <= maxY then
            ValueSome (BRect.createUnchecked(minX, minY, maxX, maxY))
        else
            ValueNone

    /// Returns the intersection of two bounding rectangles.
    /// The returned Rectangle is the area that is inside both input Rectangles.
    /// Returns ValueNone if the two Rectangles do not overlap.
    /// Just touching Rectangles will return ValueSome with zero area collapsed BRect.
    static member inline intersection (a:BRect) (b:BRect) : BRect voption =
        a.Intersection b

    /// Scales the 2D bounding rectangle by a given factor.
    /// Scale center is World Origin 0,0.
    /// A factor of 0.0 will collapse the rectangle to a point at the origin.
    /// Negative factors would flip the rectangle, breaking the Min/Max invariant, so this is raising an  EuclidException.
    static member inline scale (factor:float) (r:BRect) : BRect =
        if factor < 0.0 then
            fail $"BRect.scale: Negative factor {factor} is not allowed, would flip the rectangle on {r.AsString}"
        BRect.createUnchecked(  r.MinX * factor,
                r.MinY * factor,
                r.MaxX * factor,
                r.MaxY * factor)

    /// <summary>Returns the point (0) or minX, minY.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2 = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y      1
    /// </code>
    /// </summary>
    member r.Pt0 : Pt =
        Pt(r.MinX, r.MinY)

    /// Returns the point (0) or minX, minY.
    static member inline pt0 (r:BRect) : Pt =
        r.Pt0

    /// <summary>Returns the point (1) or maxX, minY.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2 = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y      1
    /// </code>
    /// </summary>
    member r.Pt1 : Pt =
        Pt(r.MaxX, r.MinY)

    /// Returns the point (1) or maxX, minY.
    static member inline pt1 (r:BRect) : Pt =
        r.Pt1

    /// <summary>Returns the point (2) or maxX, maxY.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2 = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y      1
    /// </code>
    /// </summary>
    member r.Pt2 : Pt =
        Pt(r.MaxX, r.MaxY)

    /// Returns the point (2) or maxX, maxY.
    static member inline pt2 (r:BRect) : Pt =
        r.Pt2

    /// <summary>Returns the point (3) or minX, maxY.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2 = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y      1
    /// </code>
    /// </summary>
    member r.Pt3 : Pt =
        Pt(r.MinX, r.MaxY)

    /// Returns the point (3) or minX, maxY.
    static member inline pt3 (r:BRect) : Pt =
        r.Pt3

    /// <summary>Returns the corners of this bounding rectangle in Counter-Clockwise order, starting at MinPt.
    /// Returns an array of 4 Points.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y    1
    /// </code>
    /// </summary>
    member r.Points : Pt[] =
        [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY) |]

    /// Returns the corners of this bounding rectangle in Counter-Clockwise order, starting at MinPt.
    /// Returns an array of 4 Points.
    static member inline points (r:BRect) : Pt[] =
        r.Points

    /// <summary>Returns a Counter-Clockwise array of 5 Points, starting at MinPt.
    /// Last and first point are the same.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y    1
    /// </code>
    /// </summary>
    member r.PointsLooped : Pt[] =
        [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY); Pt(r.MinX, r.MinY)|]

    /// Returns a Counter-Clockwise array of 5 Points, starting at MinPt.
    /// Last and first point are the same.
    static member inline pointsLooped (r:BRect) : Pt[] =
        r.PointsLooped

    /// <summary>The bottom edge. The line from point 0 to 1.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y    1
    /// </code>
    /// </summary>
    member r.Edge01 : Line2D =
        Line2D(r.MinX,r.MinY,r.MaxX,r.MinY)

    /// The bottom edge. The line from point 0 to 1.
    static member inline edge01 (r:BRect) : Line2D =
        r.Edge01

    /// <summary>The right edge. The line from point 1 to 2.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y    1
    /// </code>
    /// </summary>
    member r.Edge12 : Line2D =
        Line2D(r.MaxX,r.MinY,r.MaxX,r.MaxY)

    /// The right edge. The line from point 1 to 2.
    static member inline edge12 (r:BRect) : Line2D =
        r.Edge12

    /// <summary>The top edge. The line from point 2 to 3.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y    1
    /// </code>
    /// </summary>
    member r.Edge23 : Line2D =
        Line2D(r.MaxX,r.MaxY,r.MinX,r.MaxY)

    /// The top edge. The line from point 2 to 3.
    static member inline edge23 (r:BRect) : Line2D =
        r.Edge23

    /// <summary>The left edge. The line from point 3 to 0.
    /// <code>
    ///   Y-Axis
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis
    ///  0 = min X,Y    1
    /// </code>
    /// </summary>
    member r.Edge30 : Line2D =
        Line2D(r.MinX,r.MaxY,r.MinX,r.MinY)

    /// The left edge. The line from point 3 to 0.
    static member inline edge30 (r:BRect) : Line2D =
        r.Edge30


    // #endregion
    // #region Static members

    /// Finds min and max values for x and y.
    static member inline create (a:Pt, b:Pt) : BRect =
        BRect.createUnchecked(
            min a.X b.X,
            min a.Y b.Y,
            max a.X b.X,
            max a.Y b.Y)

    /// <summary>Finds min and max values points A and B given as separate X and Y values.</summary>
    /// <param name="ptAx">X value of point A</param>
    /// <param name="ptAy">Y value of point A</param>
    /// <param name="ptBx">X value of point B</param>
    /// <param name="ptBy">Y value of point B</param>
    /// <returns>A bounding rectangle that contains both points A and B.</returns>
    static member inline createXY (ptAx:float, ptAy:float, ptBx:float, ptBy:float) : BRect =
        BRect.createUnchecked(
            min ptAx ptBx,
            min ptAy ptBy,
            max ptAx ptBx,
            max ptAy ptBy)

    /// Finds min and max values for x and y.
    /// Fails if the sequence is null or empty.
    static member inline createFromSeq (ps:seq<Pt> ) : BRect =
        if isNull ps      then failNull "BRect.createFromSeq" "seq<Pt>"
        if Seq.isEmpty ps then failEmptySeq "BRect.createFromSeq" "seq<Pt>"
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        for p in ps do
            minX <- min minX p.X
            minY <- min minY p.Y
            maxX <- max maxX p.X
            maxY <- max maxY p.Y
        BRect.createUnchecked(minX, minY, maxX, maxY)

    /// Finds min and max values for x and y.
    /// Fails if the IList is null or empty.
    static member inline createFromIList (ps:Collections.Generic.IList<Pt> ) : BRect =
        if isNull ps    then failNull "BRect.createFromIList" "IList<Pt>"
        if ps.Count = 0 then failEmptySeq "BRect.createFromIList" "IList<Pt>"
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        for i = 0 to ps.Count-1 do
            let p = ps.[i]
            minX <- min minX p.X
            minY <- min minY p.Y
            maxX <- max maxX p.X
            maxY <- max maxY p.Y
        BRect.createUnchecked(minX, minY, maxX, maxY)

    /// Creates a bounding rectangle from a center point and the total X and Y size.
    /// Fails if sizeX or sizeY is negative.
    static member inline createFromCenter (center:Pt, sizeX, sizeY) : BRect =
        if isNegative sizeX then fail $"BRect.createFromCenter sizeX is negative: {sizeX}, sizeY is: {sizeY}, center: {center.AsString}"
        if isNegative sizeY then fail $"BRect.createFromCenter sizeY is negative: {sizeY}, sizeX is: {sizeX}, center: {center.AsString}"
        let minX = center.X - sizeX*0.5
        let minY = center.Y - sizeY*0.5
        let maxX = center.X + sizeX*0.5
        let maxY = center.Y + sizeY*0.5
        BRect.createUnchecked(minX, minY, maxX, maxY)

    /// Creates a bounding rectangle of a line.
    static member inline createFromLine (l:Line2D) : BRect =
        let minX = min l.FromX l.ToX
        let maxX = max l.FromX l.ToX
        let minY = min l.FromY l.ToY
        let maxY = max l.FromY l.ToY
        BRect.createUnchecked(minX, minY, maxX, maxY)

    /// Returns the 2D bounding rectangle expanded by a relative factor on all four sides.
    /// Values between 0.0 and 1.0 shrink the rectangle.
    /// Values larger than 1.0 expand the rectangle.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRel factor (r:BRect) : BRect =
        if factor < 0.0 then
            fail $"BRect.expandRel: a negative factor {factor} is not allowed for expanding the 2D bounding rectangle {r.AsString}"
        let center = r.Center
        let sizeX = r.SizeX * factor
        let sizeY = r.SizeY * factor
        BRect.createFromCenter(center, sizeX, sizeY)

    /// Returns the 2D bounding rectangle expanded by a relative factor on all four sides, separately for X and Y.
    /// Values between 0.0 and 1.0 shrink the rectangle.
    /// Values larger than 1.0 expand the rectangle.
    /// Does check for underflow if any factor is negative and raises EuclidException.
    static member expandRelXY factorX factorY (r:BRect) : BRect =
        if factorX < 0.0 then
            fail $"BRect.expandRelXY: a negative factorX {factorX} is not allowed for expanding the 2D bounding rectangle {r.AsString}"
        if factorY < 0.0 then
            fail $"BRect.expandRelXY: a negative factorY {factorY} is not allowed for expanding the 2D bounding rectangle {r.AsString}"
        let center = r.Center
        let sizeX = r.SizeX * factorX
        let sizeY = r.SizeY * factorY
        BRect.createFromCenter(center, sizeX, sizeY)

    /// Checks if two 2D bounding rectangles are equal within tolerance.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member inline equals (tol:float) (a:BRect) (b:BRect) : bool =
        abs(a.MinX-b.MinX) <= tol &&
        abs(a.MinY-b.MinY) <= tol &&
        abs(a.MaxX-b.MaxX) <= tol &&
        abs(a.MaxY-b.MaxY) <= tol

    /// Check if two 2D bounding rectangles  are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two 2D bounding rectangles  are not exactly equal.
    static member inline notEquals (tol:float) (a:BRect) (b:BRect) : bool =
        abs(a.MinX-b.MinX) > tol ||
        abs(a.MinY-b.MinY) > tol ||
        abs(a.MaxX-b.MaxX) > tol ||
        abs(a.MaxY-b.MaxY) > tol

    /// Returns a new 2D-bounding-rectangle moved by a vector.
    /// This is the same as translate.
    static member inline move (v:Vc) (r:BRect) : BRect =
        BRect.createUnchecked(r.MinX+v.X, r.MinY+v.Y, r.MaxX+v.X, r.MaxY+v.Y)

    /// Returns a new 2D-bounding-rectangle moved by a vector.
    /// This is the same as move.
    static member inline translate (v:Vc) (r:BRect) : BRect =
        BRect.createUnchecked(r.MinX+v.X, r.MinY+v.Y, r.MaxX+v.X, r.MaxY+v.Y)

    /// Returns a new 2D-bounding-rectangle moved in X-axis direction.
    static member inline moveX (translation:float) (r:BRect) : BRect =
        BRect.createUnchecked(r.MinX+translation, r.MinY, r.MaxX+translation, r.MaxY)

    /// Returns a new 2D-bounding-rectangle moved in Y-axis direction.
    static member inline moveY (translation:float) (r:BRect) : BRect =
        BRect.createUnchecked(r.MinX, r.MinY+translation, r.MaxX, r.MaxY+translation)



    // #endregion
    // #region Obsolete

    [<Obsolete("use .isOverlapping instead")>]
    static member inline doOverlap(a:BRect) (b:BRect) : bool =
        b.IsOverlapping(a)

    [<Obsolete("use .isOverlappingOrClose instead")>]
    static member inline doOverlapMoreThan tol (a:BRect) (b:BRect)  : bool =
        b.IsOverlappingOrClose(a, -tol)

    [<Obsolete("use SizeX")>]
    member inline r.Width : float =
        r.MaxX - r.MinX

    [<Obsolete("use SizeY")>]
    member inline r.Height2D : float =
        r.MaxY - r.MinY

    [<Obsolete("typo, use ExpandSafe instead")>]
    member inline b.ExpandSave(xDist, yDist) : BRect =
        b.ExpandSafeXY(xDist, yDist)

    [<Obsolete("typo, use ExpandSafe instead")>]
    member inline b.ExpandSave(dist) : BRect =
        b.ExpandSafeXY(dist, dist)

    [<Obsolete("Use ExpandXY instead.")>]
    member inline r.Expand(xDist:float, yDist:float) : BRect =
        r.ExpandXY(xDist, yDist)

    [<Obsolete("Use ExpandSafeXY instead.")>]
    member inline b.ExpandSafe(xDist:float, yDist:float) : BRect =
        b.ExpandSafeXY(xDist, yDist)

    [<Obsolete("Use IsOverlapping instead.")>]
    member inline b.OverlapsWith (a:BRect) : bool =
        b.IsOverlapping(a)

    [<Obsolete("Use IsOverlappingOrClose instead.")>]
    member inline b.OverlapsWith (a:BRect, tolerance:float) : bool =
        b.IsOverlappingOrClose(a, tolerance)

