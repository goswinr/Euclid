namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid

#nowarn "44" // for hidden constructors via Obsolete Attribute

/// An immutable 2D bounding rectangle.
/// Sometimes also called 2D a bounding box.
/// This implementation guarantees the rectangle to be always valid.
/// That means the Min X and Y values are always smaller or equal than the respective Max values.
///
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
    [<Obsolete("Unsafe internal constructor, but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >]
    new (minX, minY, maxX, maxY) =
        {MinX = minX
         MinY = minY
         MaxX = maxX
         MaxY = maxY}

    /// Nicely formatted string representation of the bounding rectangle, including its size.
    override r.ToString() =
        sprintf "Euclid.BRect: sizeX=%s| sizeY=%s| at X=%s| Y=%s"
            (Format.float (r.MaxX - r.MinX)) (Format.float (r.MaxY - r.MinY)) (Format.float r.MinX) (Format.float r.MinY)

    /// Format bounding rectangle into string with nice floating point number formatting of size and position.
    /// But without full type name as in rect.ToString()
    member r.AsString =
        sprintf "sizeX=%s| sizeY=%s| at X=%s| Y=%s"
            (Format.float (r.MaxX - r.MinX)) (Format.float (r.MaxY - r.MinY)) (Format.float r.MinX) (Format.float r.MinY)

    /// The point where X, Y and Z are the minimum values.
    member inline r.MinPt = Pt(r.MinX, r.MinY)

    /// The point where X, Y and Z are the maximum values.
    member inline r.MaxPt = Pt(r.MaxX, r.MaxY)

    /// The size in X direction, same as member rect.SizeX.
    member inline r.Width = r.MaxX - r.MinX
    /// The size in X direction, same as member rect.Width.
    member inline r.SizeX = r.MaxX - r.MinX

    /// The size in Y direction, same as member rect.SizeY.
    member inline r.Height2D = r.MaxY - r.MinY
    /// The size in Y direction, same as member rect.Height2D.
    member inline r.SizeY = r.MaxY - r.MinY

    /// The diagonal 2D vector of the bounding rect. From MinPt to MaxPt.
    member inline r.Diagonal = Vc(r.MaxX - r.MinX, r.MaxY - r.MinY)

    /// The center of the bounding rect.
    member inline r.Center =
        Pt( (r.MaxX + r.MinX) * 0.5,
            (r.MaxY + r.MinY) * 0.5)

    /// Returns the point (0) or minX, minY.
    ///
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
    member r.Pt0 = Pt(r.MinX, r.MinY)

    /// Returns the point (1) or maxX, minY.
    ///
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
    member r.Pt1 = Pt(r.MaxX, r.MinY)

    /// Returns the point (2) or maxX, maxY.
    ///
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
    member r.Pt2 = Pt(r.MaxX, r.MaxY)

    /// Returns the point (3) or minX, maxY.
    ///
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
    member r.Pt3 = Pt(r.MinX, r.MaxY)


    /// Returns the corners of this bounding rectangle in Counter-Clockwise order, starting at MinPt.
    /// Returns an array of 4 Points.
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
    member r.Points =
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
    member r.PointsLooped =
        [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY); Pt(r.MinX, r.MinY)|]

    /// The bottom Edge. The line from point 0 to 1
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
    member r.Edge01 = Line2D(r.MinX,r.MinY,r.MaxX,r.MinY)

    /// The right Edge. The line from point 1 to 2
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
    member r.Edge12 = Line2D(r.MaxX,r.MinY,r.MaxX,r.MaxY)

    /// The top Edge. The line from point 2 to 3
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
    member r.Edge23 = Line2D(r.MaxX,r.MaxY,r.MinX,r.MaxY)

    /// The left Edge. The line from point 3 to 0
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
    member r.Edge30 = Line2D(r.MinX,r.MaxY,r.MinX,r.MinY)

    /// Returns a bounding rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.Expand(dist) : BRect =
        let n = BRect(r.MinX-dist, r.MinY-dist, r.MaxX+dist, r.MaxY+dist)
        if dist<0. &&  (n.MinX > n.MaxX || n.MinY > n.MaxX) then
            EuclidException.Raise "Euclid.BRect.Expand(dist): Negative distance %g causes an underflow, on %s" dist r.AsString
        n


    /// Returns a bounding rectangle expanded by a distance for X and Y-axis each.
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.Expand(xDist, yDist) : BRect =
        let n = BRect(r.MinX-xDist, r.MinY-yDist, r.MaxX+xDist, r.MaxY+yDist)
        if n.MinX > n.MaxX ||  n.MinY > n.MaxX then
            EuclidException.Raise "Euclid.BRect.Expand(x, y): Negative distance(s) X: %g and Y: %g cause an underflow, on %s" xDist yDist r.AsString
        n


    /// Returns a bounding rectangle expanded by a distance for X and Y-axis each.
    /// If expansion is negative it shrinks the Rectangle. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    member inline b.ExpandSave(xDist, yDist) : BRect =
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
        BRect(minXCh, minYCh, maxXCh, maxYCh)

    /// Returns a bounding rectangle expanded by a distance.
    /// If expansion is negative it shrinks the Rectangle. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    member inline b.ExpandSave(dist) : BRect =
        b.ExpandSave(dist, dist)

    /// Returns a bounding rectangle expanded only in X direction by different distance for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.ExpandXaxis(startDist, endDist) : BRect =
        let n = BRect(r.MinX-startDist, r.MinY, r.MaxX+endDist, r.MaxY)
        if n.MinX > n.MaxX then
            EuclidException.Raise "Euclid.BRect.ExpandXaxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist r.AsString
        n

    /// Returns a bounding rectangle expanded only in Y direction by different distance for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline r.ExpandYaxis(startDist, endDist) : BRect =
        let n = BRect(r.MinX, r.MinY-startDist, r.MaxX, r.MaxY+endDist)
        if n.MinY > n.MaxY then
            EuclidException.Raise "Euclid.BRect.ExpandYaxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist r.AsString
        n

    /// Returns true if the two bounding rectangles do overlap or touch.
    member inline r.OverlapsWith (a:BRect) =
        not (  r.MinX > a.MaxX
            || a.MinX > r.MaxX
            || a.MinY > r.MaxY
            || r.MinY > a.MaxY)

    /// Returns true if the two bounding rectangles do overlap more than a given tolerance distance.
    /// Use a negative tolerance to count touching if they are apart by abs(tolerance)
    /// Returns false if the two bounding rectangles are just touching or apart.
    member inline r.OverlapsWith (a:BRect, tol) =
        not (  r.MinX > a.MaxX - tol
            || a.MinX > r.MaxX - tol
            || a.MinY > r.MaxY - tol
            || r.MinY > a.MaxY - tol
            )

    /// Returns true if the point is inside or exactly on this bounding rectangle.
    member inline r.Contains (p:Pt) =
        p.X >= r.MinX &&
        p.X <= r.MaxX &&
        p.Y >= r.MinY &&
        p.Y <= r.MaxY

    /// Returns true if the Rectangle is inside or exactly on the other bounding rectangle.
    member inline r.Contains (o:BRect) =
        r.Contains(o.MinPt) && r.Contains(o.MaxPt)

    /// Test if bounding rectangles are only touching each other from the Outside within a given tolerance.
    member b.IsTouching (a:BRect, tol) =
        let xOverlap = not (b.MinX > a.MaxX + tol || a.MinX > b.MaxX + tol)
        let yOverlap = not (a.MinY > b.MaxY + tol || b.MinY > a.MaxY + tol)
        let xTouch   = abs(b.MinX - a.MaxX)  < tol || abs(a.MinX - b.MaxX) < tol
        let yTouch   = abs(a.MinY - b.MaxY)  < tol || abs(b.MinY - a.MaxY) < tol
        (xOverlap && yTouch) || (xTouch && yOverlap)

    /// Evaluate a X and Y parameter of this bounding rectangle.
    ///  0.0, 0.0, 0.0 returns the MinPt.
    ///  1.0, 1.0, 1.0 returns the MaxPt.
    member inline b.EvaluateAt (xParameter, yParameter) =
        Pt (b.MinX + (b.MaxX-b.MinX) * xParameter,
            b.MinY + (b.MaxY-b.MinY) * yParameter)

    /// Returns the area of this bounding rectangle.
    member inline r.Area =
        r.SizeX*r.SizeY

    /// Returns a bounding rectangle that contains both input Rectangles.
    member inline r.Union  (b:BRect) =
        BRect (min b.MinX r.MinX, min b.MinY r.MinY, max b.MaxX r.MaxX, max b.MaxY r.MaxY)

    /// Returns a bounding rectangle that contains the input Rectangles and the point.
    member inline r.Union (p:Pt) =
        BRect (min r.MinX p.X, min r.MinY p.Y, max r.MaxX p.X, max r.MaxY p.Y)


    /// Returns the intersection of two bounding rectangles.
    /// The returned Rectangle is the volume inside both input Rectangle.
    /// Returns ValueNone if the two Rectangles do not overlap.
    member inline b.Intersection (a:BRect) =
        let mutable minX = max a.MinX b.MinX
        let mutable minY = max a.MinY b.MinY
        let mutable maxX = min a.MaxX b.MaxX
        let mutable maxY = min a.MaxY b.MaxY
        if minX <= maxX && minY <= maxY then
            ValueSome (BRect(minX, minY, maxX, maxY))
        else
            ValueNone

    //-------------------------------------------------------------------
    //------------------------static members---------------------------
    //-------------------------------------------------------------------

    /// Checks if two 2D bounding rectangle are equal within tolerance.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:BRect) (b:BRect) =
        abs(a.MinX-b.MinX) <= tol &&
        abs(a.MinY-b.MinY) <= tol &&
        abs(a.MaxX-b.MaxX) <= tol &&
        abs(a.MaxY-b.MaxY) <= tol

    /// Returns bounding rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (r:BRect) =
        r.Expand dist

    /// Returns bounding rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandSave dist (r:BRect) =
        r.Expand dist

    /// Returns bounding rectangle expanded only in X direction by different distance for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandXaxis startDist endDist (r:BRect) =
        r.ExpandXaxis(startDist, endDist)

    /// Returns bounding rectangle expanded only in Y direction by different distance for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandYaxis startDist endDist (r:BRect) =
        r.ExpandYaxis(startDist, endDist)

    /// Move this bounding rectangle by a vector.
    static member move (v:Vc) (r:BRect) =
        BRect(r.MinX+v.X, r.MinY+v.Y, r.MaxX+v.X, r.MaxY+v.Y)

    /// Returns true if the two bounding rectangles do overlap or touch exactly.
    static member inline doOverlap(a:BRect) (b:BRect) =
        b.OverlapsWith(a)

    /// Returns true if the two bounding rectangles do overlap more than a given tolerance distance.
    /// Use a negative tolerance to count touching if they are apart by abs(tolerance)
    /// Returns false if the two bounding rectangles are just touching or apart.
    static member inline doOverlapMoreThan tol (a:BRect) (b:BRect) =
        b.OverlapsWith(a, tol)

    /// Returns true if this bounding rectangle is inside or exactly on the other bounding rectangle.
    /// Argument order matters!
    static member inline contains (rectInside:BRect) (surroundingRect:BRect) =
        surroundingRect.Contains(rectInside)

    /// Returns true if the point is inside or on this bounding rectangle.
    static member inline containsPt (pt:Pt) (rect:BRect) =
        rect.Contains(pt)


    /// Returns a bounding rectangle that contains both input Rectangles.
    static member inline union (a:BRect) (b:BRect) =
        BRect (min b.MinX a.MinX, min b.MinY a.MinY, max b.MaxX a.MaxX, max b.MaxY a.MaxY)

    /// Returns a bounding rectangle that contains the input Rectangles and the point.
    static member inline unionPt (p:Pt) (r:BRect) =
        BRect (min r.MinX p.X, min r.MinY p.Y, max r.MaxX p.X, max r.MaxY p.Y)

    /// Returns the intersection of two bounding rectangles.
    /// The returned Rectangle is the volume inside both input Rectangle.
    /// Returns ValueNone if the two Rectangles do not overlap.
    static member inline intersection (a:BRect) (b:BRect) =
        let mutable minX = max a.MinX b.MinX
        let mutable minY = max a.MinY b.MinY
        let mutable maxX = min a.MaxX b.MaxX
        let mutable maxY = min a.MaxY b.MaxY
        if minX <= maxX && minY <= maxY then
            ValueSome (BRect(minX, minY, maxX, maxY))
        else
            ValueNone

    /// Finds min and max values for x and y.
    static member inline create (a:Pt, b:Pt) =
        // sort min and max values (not using allocating tuples for swapping)
        let mutable minX = a.X
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X
        let mutable minY = a.Y
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        BRect(minX, minY, maxX, maxY)

    /// Finds min and max values for x and y.
    static member inline createFromSeq (ps:seq<Pt> ) =
        if Seq.isEmpty ps then raise <| EuclidException("Euclid.BRect.createFromSeq: seq<Pt> is empty.")
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        for p in ps do
            minX <- min minX p.X
            minY <- min minY p.Y
            maxX <- max maxX p.X
            maxY <- max maxY p.Y
        BRect(minX, minY, maxX, maxY)

    /// Finds min and max values for x and y.
    static member inline createFromIList (ps:Collections.Generic.IList<Pt> ) =
        if ps.Count = 0 then raise <| EuclidException("Euclid.BRect.createFromIList: IList<Pt> is empty.")
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
        BRect(minX, minY, maxX, maxY)

    /// Creates a bounding rectangle from a center point and the total X and Y size.
    static member inline createFromCenter (center:Pt, sizeX, sizeY) =
        if isNegative(sizeX) then EuclidException.Raise "Euclid.BRect.createFromCenter sizeX is negative: %g, sizeY is: %g, center: %O"  sizeX sizeY  center.AsString
        if isNegative(sizeY) then EuclidException.Raise "Euclid.BRect.createFromCenter sizeY is negative: %g, sizeX is: %g, center: %O"  sizeY sizeX  center.AsString
        let minX = center.X - sizeX*0.5
        let minY = center.Y - sizeY*0.5
        let maxX = center.X + sizeX*0.5
        let maxY = center.Y + sizeY*0.5
        BRect(minX, minY, maxX, maxY)


    /// Does not verify the order of min and max values.
    static member inline createUnchecked (minX, minY, maxX, maxY) =
        BRect(minX, minY, maxX, maxY)

    /// Returns the area of this bounding rectangle.
    static member inline area  (r:BRect) =
        r.SizeX * r.SizeY

    static member createFromLine (l:Line2D) =
        let minX = min l.FromX l.ToX
        let maxX = max l.FromX l.ToX
        let minY = min l.FromY l.ToY
        let maxY = max l.FromY l.ToY
        BRect(minX, minY, maxX, maxY)
