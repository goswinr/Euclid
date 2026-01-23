namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid
open EuclidErrors


/// <summary>An immutable 2D bounding rectangle.
/// Sometimes also called 2D a bounding box.
/// This implementation guarantees the rectangle to be always valid.
/// That means the Min X and Y values are always smaller or equal than the respective Max values.
/// <code>
///   Y-Axis (Height2D)
///   ^
///   |
///   |             2 max X,Y
/// 3 +------------+
///   |            |
///   |            |
///   |            |
///   |            |
///   |            |       local
///   +------------+-----> X-Axis (Width)
///  0-min X,Y      1
/// </code>
/// </summary>
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type BRect =
    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar
    [<DataMember>] val MinX : float
    [<DataMember>] val MinY : float
    [<DataMember>] val MaxX : float
    [<DataMember>] val MaxY : float


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
    override r.ToString() =
        let sizeX = Format.float (r.MaxX - r.MinX)
        let sizeY = Format.float (r.MaxY - r.MinY)
        let atX = Format.float r.MinX
        let atY = Format.float r.MinY
        $"Euclid.BRect: sizeX=%s{sizeX}| sizeY=%s{sizeY}| at X=%s{atX}|Y=%s{atY}"

    /// Format bounding rectangle into string with nice floating point number formatting of size and position.
    /// But without full type name as in rect.ToString()
    member r.AsString : string =
        let sizeX = Format.float (r.MaxX - r.MinX)
        let sizeY = Format.float (r.MaxY - r.MinY)
        let atX = Format.float r.MinX
        let atY = Format.float r.MinY
        $"sizeX=%s{sizeX}| sizeY=%s{sizeY}| at X=%s{atX}|Y=%s{atY}"

    /// Format bounding rectangle into an F# code string that can be used to recreate the rectangle.
    member r.AsFSharpCode : string =
        $"BRect.createUnchecked({r.MinX}, {r.MinY}, {r.MaxX}, {r.MaxY})"

    /// The point where X and Y are the minimum values.
    member inline r.MinPt : Pt =
        Pt(r.MinX, r.MinY)

    /// The point where X and Y are the maximum values.
    member inline r.MaxPt : Pt =
        Pt(r.MaxX, r.MaxY)

    /// The size in X direction.
    member inline r.SizeX : float =
        r.MaxX - r.MinX

    /// The size in Y direction.
    member inline r.SizeY : float =
        r.MaxY - r.MinY

    /// The diagonal 2D vector of the bounding rect. From MinPt to MaxPt.
    member inline r.Diagonal : Vc =
        Vc(r.MaxX - r.MinX, r.MaxY - r.MinY)

    /// The center of the bounding rect.
    member inline r.Center : Pt =
        Pt( (r.MaxX + r.MinX) * 0.5,
            (r.MaxY + r.MinY) * 0.5)


    /// Returns a bounding rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.Expand(dist) : BRect =
        let n = BRect.createUnchecked(r.MinX-dist, r.MinY-dist, r.MaxX+dist, r.MaxY+dist)
        if dist<0. &&  (n.MinX > n.MaxX || n.MinY > n.MaxY) then
            fail $"BRect.Expand(dist): Negative distance {dist} causes an underflow, on {r.AsString}"
        n


    /// Returns a bounding rectangle expanded by a distance for X and Y-axis each.
    /// Raises EuclidException if the resulting rectangle would be invalid (Min > Max).
    member inline r.Expand(xDist, yDist) : BRect =
        let n = BRect.createUnchecked(r.MinX-xDist, r.MinY-yDist, r.MaxX+xDist, r.MaxY+yDist)
        if n.MinX > n.MaxX ||  n.MinY > n.MaxY then
            fail $"BRect.Expand(x, y): Distance(s) X: {xDist} and Y: {yDist} cause an underflow, on {r.AsString}"
        n

    /// Returns a bounding rectangle expanded by a distance for X and Y-axis each.
    /// If expansion is negative it shrinks the Rectangle. It also prevents the rectangle from collapsing past its center.
    /// When the negative expansion is bigger than the size, Min and Max values will be both at the center point.
    member inline b.ExpandSafe(xDist, yDist) : BRect =
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

    /// Returns a bounding rectangle expanded by a distance.
    /// If expansion is negative it shrinks the Rectangle. It also prevents the rectangle from collapsing past its center.
    /// When the negative expansion is bigger than the size, Min and Max values will be both at the center point.
    member inline b.ExpandSafe(dist) : BRect =
        b.ExpandSafe(dist, dist)

    /// Returns a bounding rectangle expanded only in X direction by different distances for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.ExpandXaxis(startDist, endDist) : BRect =
        let n = BRect.createUnchecked(r.MinX-startDist, r.MinY, r.MaxX+endDist, r.MaxY)
        if n.MinX > n.MaxX then
            fail $"BRect.ExpandXaxis: Negative distances for start({startDist}) and end ({endDist}) cause an underflow, on {r.AsString}"
        n

    /// Returns a bounding rectangle expanded only in Y direction by different distances for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.ExpandYaxis(startDist, endDist) : BRect =
        let n = BRect.createUnchecked(r.MinX, r.MinY-startDist, r.MaxX, r.MaxY+endDist)
        if n.MinY > n.MaxY then
            fail $"BRect.ExpandYaxis: Negative distances for start({startDist}) and end({endDist}) cause an underflow, on {r.AsString}"
        n


    /// Returns TRUE if the two bounding rectangles do overlap or touch.
    /// Also returns TRUE if one bounding rect is completely inside the other.
    /// Also returns TRUE if one bounding rect is completely surrounding the other.
    member inline r.OverlapsWith (a:BRect) : bool =
        not (  r.MinX > a.MaxX
            || a.MinX > r.MaxX
            || a.MinY > r.MaxY
            || r.MinY > a.MaxY)

    /// <summary>
    /// Determines whether two bounding rectangles overlap more than a given tolerance distance.
    /// </summary>
    /// <param name="a">The bounding rectangle to test for overlap with.</param>
    /// <param name="tol">The tolerance distance. Use a negative tolerance to count touching if they are apart by abs(tolerance). Default is 1e-6.</param>
    /// <returns>
    /// Returns TRUE if:
    /// - The rectangles overlap by more than the tolerance distance
    /// - One rectangle is completely inside the other
    /// - One rectangle completely surrounds the other
    /// Returns FALSE if:
    /// - The rectangles are just touching (within tolerance)
    /// - The rectangles are separated
    /// </returns>
    /// Returns TRUE if the two bounding rectangles do overlap more than a given tolerance distance.
    /// Use a negative tolerance to count touching if they are apart by abs(tolerance)
    /// Returns false if the two bounding rectangles are just touching or apart.
    /// Also returns TRUE if one bounding rect is completely inside the other.
    /// Also returns TRUE if one bounding rect is completely surrounding the other.
    member inline r.OverlapsWith (a:BRect, [<OPT;DEF(1e-6)>]tol:float) : bool =
        not (  r.MinX > a.MaxX - tol
            || a.MinX > r.MaxX - tol
            || a.MinY > r.MaxY - tol
            || r.MinY > a.MaxY - tol
            )

    /// Returns TRUE if the point is inside or exactly on this bounding rectangle.
    member inline r.Contains (p:Pt) : bool =
        p.X >= r.MinX &&
        p.X <= r.MaxX &&
        p.Y >= r.MinY &&
        p.Y <= r.MaxY

    /// Returns TRUE if the Rectangle is inside or exactly on the other bounding rectangle.
    member inline r.Contains (o:BRect) : bool =
        r.Contains(o.MinPt) && r.Contains(o.MaxPt)

    /// <summary>Test if 2D bounding rectangles are only touching each other from the Outside within a given tolerance.</summary>
    /// <param name="a">Other 2D bounding rectangle to test against.</param>
    /// <param name="tol">Optional. A tolerance for touching test. Default is 1e-6.</param>
    /// <returns>TRUE if the two 2D bounding rectangles are touching each other within the given tolerance.
    /// FALSE if the two 2D bounding rectangles are overlapping or intersecting.</returns>
    member b.IsTouching (a:BRect, [<OPT;DEF(1e-6)>]tol:float) : bool =
        let xOverlap = not (b.MinX > a.MaxX + tol || a.MinX > b.MaxX + tol)
        let yOverlap = not (a.MinY > b.MaxY + tol || b.MinY > a.MaxY + tol)
        let xTouch   = abs(b.MinX - a.MaxX)  <= tol || abs(a.MinX - b.MaxX) <= tol
        let yTouch   = abs(a.MinY - b.MaxY)  <= tol || abs(b.MinY - a.MaxY) <= tol
        (xOverlap && yTouch) || (xTouch && yOverlap)

    /// Evaluate a X and Y parameter of this bounding rectangle.
    ///  0.0, 0.0 returns the MinPt.
    ///  1.0, 1.0 returns the MaxPt.
    member inline b.EvaluateAt (xParameter, yParameter) : Pt =
        Pt (b.MinX + (b.MaxX-b.MinX) * xParameter,
            b.MinY + (b.MaxY-b.MinY) * yParameter)

    /// Returns the area of this bounding rectangle.
    member inline r.Area : float =
        r.SizeX * r.SizeY


    /// Returns the longest edge of the Box.
    member inline b.LongestEdge : float =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        max x y

    /// Returns the shortest edge of the Box.
    member inline b.ShortestEdge : float =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        min x y


    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsPoint.
    member inline b.IsZero : bool =
        isTooTiny (b.MaxX - b.MinX) &&
        isTooTiny (b.MaxY - b.MinY)

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsZero.
    member inline b.IsPoint : bool =
        b.IsZero


    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, or 2.
    member inline b.CountZeroSides : int =
        countTooTinyOrNaN    (b.MaxX - b.MinX)
        +  countTooTinyOrNaN (b.MaxY - b.MinY)

    /// Tests if one of the X or Y axis is smaller than the zeroLength tolerance.
    member inline b.IsLine : bool =
        b.CountZeroSides = 1

    /// Tests if no sides of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .HasArea
    member inline b.IsValid : bool =
        b.CountZeroSides = 0

    /// Tests if none of the X and Y axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasArea : bool =
        b.CountZeroSides = 0


    /// Returns a bounding rectangle that contains both input Rectangles.
    member inline r.Union  (b:BRect) : BRect =
        BRect.createUnchecked(min b.MinX r.MinX, min b.MinY r.MinY, max b.MaxX r.MaxX, max b.MaxY r.MaxY)

    /// Returns a bounding rectangle that contains the input Rectangles and the point.
    member inline r.Union (p:Pt) : BRect =
        BRect.createUnchecked(min r.MinX p.X, min r.MinY p.Y, max r.MaxX p.X, max r.MaxY p.Y)


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

    // -------------------------------------------------------------------------------------
    //            █████               █████     ███
    //           ░░███               ░░███     ░░░
    //    █████  ███████    ██████   ███████   ████   ██████
    //   ███░░  ░░░███░    ░░░░░███ ░░░███░   ░░███  ███░░███
    //  ░░█████   ░███      ███████   ░███     ░███ ░███ ░░░
    //   ░░░░███  ░███ ███ ███░░███   ░███ ███ ░███ ░███  ███
    //   ██████   ░░█████ ░░████████  ░░█████  █████░░██████
    //  ░░░░░░     ░░░░░   ░░░░░░░░    ░░░░░  ░░░░░  ░░░░░░
    //
    //                                             █████
    //                                            ░░███
    //    █████████████    ██████  █████████████   ░███████   ██████  ████████   █████
    //   ░░███░░███░░███  ███░░███░░███░░███░░███  ░███░░███ ███░░███░░███░░███ ███░░
    //    ░███ ░███ ░███ ░███████  ░███ ░███ ░███  ░███ ░███░███████  ░███ ░░░ ░░█████
    //    ░███ ░███ ░███ ░███░░░   ░███ ░███ ░███  ░███ ░███░███░░░   ░███      ░░░░███
    //    █████░███ █████░░██████  █████░███ █████ ████████ ░░██████  █████     ██████
    //   ░░░░░ ░░░ ░░░░░  ░░░░░░  ░░░░░ ░░░ ░░░░░ ░░░░░░░░   ░░░░░░  ░░░░░     ░░░░░░
    // -------------------------------------------------------------------------------------


    /// Finds min and max values for x and y.
    static member inline create (a:Pt, b:Pt) : BRect =
        // sort min and max values (not using allocating tuples for swapping)
        let mutable minX = a.X
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X
        let mutable minY = a.Y
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        BRect.createUnchecked(minX, minY, maxX, maxY)

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

    /// Returns bounding rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member inline expand dist (r:BRect) : BRect =
        r.Expand dist

    /// Returns a bounding rectangle expanded by a distance for X and Y-axis each.
    /// If expansion is negative it shrinks the Rectangle. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    static member inline expandSafe dist (r:BRect) : BRect =
        r.ExpandSafe dist

    /// Returns bounding rectangle expanded only in X direction by different distances for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member inline expandXaxis startDist endDist (r:BRect) : BRect =
        r.ExpandXaxis(startDist, endDist)

    /// Returns bounding rectangle expanded only in Y direction by different distances for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member inline expandYaxis startDist endDist (r:BRect) : BRect =
        r.ExpandYaxis(startDist, endDist)

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


    /// Returns TRUE if the two bounding rectangles do overlap or touch exactly.
    /// Also returns TRUE if one bounding rect is completely inside the other.
    /// Also returns TRUE if one bounding rect is completely surrounding the other.
    static member inline doOverlap(a:BRect) (b:BRect) : bool =
        b.OverlapsWith a

    /// Returns TRUE if the two bounding rectangles do overlap more than a given tolerance distance.
    /// Use a negative tolerance to count touching if they are apart by abs(tolerance)
    /// Returns false if the two bounding rectangles are just touching or apart.
    /// Also returns TRUE if one bounding rect is completely inside the other.
    /// Also returns TRUE if one bounding rect is completely surrounding the other.
    static member inline doOverlapMoreThan tol (a:BRect) (b:BRect) : bool =
        b.OverlapsWith(a, tol)

    /// Returns TRUE if this bounding rectangle is inside or exactly on the other bounding rectangle.
    /// Argument order matters!
    static member inline contains (rectInside:BRect) (surroundingRect:BRect) : bool =
        surroundingRect.Contains rectInside

    /// Returns TRUE if the point is inside or on this bounding rectangle.
    static member inline containsPt (pt:Pt) (rect:BRect) : bool =
        rect.Contains pt


    /// Returns a bounding rectangle that contains both input Rectangles.
    static member inline union (a:BRect) (b:BRect) : BRect =
        BRect.createUnchecked(min b.MinX a.MinX, min b.MinY a.MinY, max b.MaxX a.MaxX, max b.MaxY a.MaxY)

    /// Returns a bounding rectangle that contains the input Rectangles and the point.
    static member inline unionPt (p:Pt) (r:BRect) : BRect =
        BRect.createUnchecked(min r.MinX p.X, min r.MinY p.Y, max r.MaxX p.X, max r.MaxY p.Y)

    /// Returns the intersection of two bounding rectangles.
    /// The returned Rectangle is the area that is inside both input Rectangles.
    /// Returns ValueNone if the two Rectangles do not overlap.
    /// Just touching Rectangles will return ValueSome with zero area collapsed BRect.
    static member inline intersection (a:BRect) (b:BRect) : BRect voption =
        a.Intersection b



    /// Returns the area of this bounding rectangle.
    static member inline area (r:BRect) : float =
        r.SizeX * r.SizeY


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
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2 = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y      1
    /// </code>
    /// </summary>
    member r.Pt0 : Pt = Pt(r.MinX, r.MinY)

    /// <summary>Returns the point (1) or maxX, minY.
    /// <code>
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2 = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y      1
    /// </code>
    /// </summary>
    member r.Pt1 : Pt = Pt(r.MaxX, r.MinY)

    /// <summary>Returns the point (2) or maxX, maxY.
    /// <code>
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2 = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y      1
    /// </code>
    /// </summary>
    member r.Pt2 : Pt = Pt(r.MaxX, r.MaxY)

    /// <summary>Returns the point (3) or minX, maxY.
    /// <code>
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2 = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y      1
    /// </code>
    /// </summary>
    member r.Pt3 : Pt = Pt(r.MinX, r.MaxY)


    /// <summary>Returns the corners of this bounding rectangle in Counter-Clockwise order, starting at MinPt.
    /// Returns an array of 4 Points.
    /// <code>
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y    1
    /// </code>
    /// </summary>
    member r.Points : Pt[] =
        [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY) |]


    /// Returns a Counter-Clockwise array of 5 Points, starting at MinPt.
    /// Last and first point are the same.
    ///
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y    1
    member r.PointsLooped : Pt[] =
        [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY); Pt(r.MinX, r.MinY)|]

    /// The bottom edge. The line from point 0 to 1
    ///
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y    1
    member r.Edge01 : Line2D = Line2D(r.MinX,r.MinY,r.MaxX,r.MinY)

    /// The right edge. The line from point 1 to 2
    ///
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y    1
    member r.Edge12 : Line2D = Line2D(r.MaxX,r.MinY,r.MaxX,r.MaxY)

    /// The top edge. The line from point 2 to 3
    ///
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y    1
    member r.Edge23 : Line2D = Line2D(r.MaxX,r.MaxY,r.MinX,r.MaxY)

    /// The left edge. The line from point 3 to 0
    ///
    ///   Y-Axis (Height2D)
    ///   ^
    ///   |
    ///   |             2  = max X,Y
    /// 3 +------------+
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   +------------+-----> X-Axis (Width)
    ///  0 = min X,Y    1
    member r.Edge30 : Line2D = Line2D(r.MinX,r.MaxY,r.MinX,r.MinY)







    [<Obsolete("use SizeX")>]
    member inline r.Width : float =
        r.MaxX - r.MinX

    [<Obsolete("use SizeY")>]
    member inline r.Height2D : float =
        r.MaxY - r.MinY

    [<Obsolete("typo, use ExpandSafe instead")>]
    member inline b.ExpandSave(xDist, yDist) : BRect =
        b.ExpandSafe(xDist, yDist)

    [<Obsolete("typo, use ExpandSafe instead")>]
    member inline b.ExpandSave(dist) : BRect =
        b.ExpandSafe(dist, dist)