namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike


#nowarn "44" // for hidden constructors via Obsolete Attribute

/// An immutable 2D Bounding Rectangle.
/// Sometimes also called 2D Bounding Box.
/// This implementation guarantees the rectangle to be always valid.
/// That means the Min X and Y values are always smaller or equal than the respective Max values
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
type BRect =
    val MinX : float
    val MinY : float
    val MaxX : float
    val MaxY : float

    /// Unsafe internal constructor,  public only for inlining.
    [<Obsolete("Unsafe internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >]
    new (minX,minY,maxX,maxY) =
        {MinX = minX
         MinY = minY
         MaxX = maxX
         MaxY = maxY}

    /// Nicely formatted string representation of the bounding Rectangle, including its size.
    override r.ToString() =
        sprintf "FsEx.Geo.BRect: length(x)=%s| width(y)=%s| at X=%s| Y=%s"
            (Format.float (r.MaxX - r.MinX)) (Format.float (r.MaxY - r.MinY)) (Format.float r.MinX) (Format.float r.MinY)

    /// Format Bounding Rectangle into string with nice floating point number formatting of size and position
    /// But without full type name as in rect.ToString()
    member r.AsString =
        sprintf "length=%s| width=%s| at X=%s| Y=%s"
            (Format.float (r.MaxX - r.MinX)) (Format.float (r.MaxY - r.MinY)) (Format.float r.MinX) (Format.float r.MinY)

    /// The Point where X, Y and Z are the minimum values.
    member inline r.MinPt = Pt(r.MinX,r.MinY)

    /// The Point where X, Y and Z are the maximum values.
    member inline r.MaxPt = Pt(r.MaxX,r.MaxY)

    /// The size in X direction, same as member rect.SizeX.
    member inline r.Length = r.MaxX - r.MinX
    /// The size in X direction, same as member rect.Length.
    member inline r.SizeX = r.MaxX - r.MinX

    /// The size in Y direction, same as member rect.SizeY.
    member inline r.Width  = r.MaxY - r.MinY
    /// The size in Y direction, same as member rect.Width.
    member inline r.SizeY  = r.MaxY - r.MinY

    /// The diagonal 3D vector of the bounding rect. From MinPt to MaxPt.
    member inline r.Diagonal = Vc(r.MaxX - r.MinX, r.MaxY - r.MinY)

    /// The center of the bounding rect.
    member inline r.Center = Pt( (r.MaxX + r.MinX)*0.5, (r.MaxY + r.MinY)*0.5)

    /// Returns the corners of the Bounding Rectangle  in counter clockwise order, starting at MinPt
    ///  Returns an array of 4 Points. Same as member rect.Polyline.
    ///
    /// 3 +------------+ 2
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    ///   |            |
    /// 0 +------------+ 1
    ///
    member r.Corners = [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY) |]

    /// Returns a counter clockwise array of 4 Points, starting at MinPt
    /// Last and first point are NOT the same Same as member rect.Corners.
    member r.Polyline = [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY) |]

    /// Returns a counter clockwise array of 5 Points, starting at MinPt
    /// Last and first point are the same
    member r.PolylineClosed = [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY); Pt(r.MinX, r.MinY)|]


    /// Returns Bounding Rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    member inline r.Expand(dist) : BRect =
        let n = BRect(r.MinX-dist, r.MinY-dist, r.MaxX+dist, r.MaxY+dist)
        if dist<0. &&  (n.MinX > n.MaxX || n.MinY > n.MaxX) then
            FsExGeoException.Raise "BRect.Expand(dist): Negative distance %g causes an underflow, on %s" dist r.AsString
        n


    /// Returns Bounding Rectangle expanded by a distance for X and Y Axis each.
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    member inline r.Expand(xDist,yDist) : BRect =
        let n = BRect(r.MinX-xDist, r.MinY-yDist, r.MaxX+xDist, r.MaxY+yDist)
        if n.MinX > n.MaxX ||  n.MinY > n.MaxX then
            FsExGeoException.Raise "BRect.Expand(x,y): Negative distance(s) X: %g and Y: %g cause an underflow, on %s" xDist yDist r.AsString
        n


    /// Returns Bounding Rectangle expanded by a distance for X and Y Axis each.
    /// If expansion is negative it shrinks the Rectangle. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    member inline b.ExpandSave(xDist,yDist) : BRect =
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
        BRect(minXCh, minYCh,  maxXCh, maxYCh)

    /// Returns Bounding Rectangle expanded by a distance.
    /// If expansion is negative it shrinks the Rectangle. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    member inline b.ExpandSave(dist) : BRect =
        b.ExpandSave(dist,dist)

    /// Returns Bounding Rectangle expanded only in X direction by different distance for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    member inline r.ExpandXAxis(startDist, endDist) : BRect =
        let n = BRect(r.MinX-startDist, r.MinY, r.MaxX+endDist, r.MaxY)
        if n.MinX > n.MaxX  then
            FsExGeoException.Raise "BRect.ExpandXAxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist r.AsString
        n

    /// Returns Bounding Rectangle expanded  only in Y direction by different distance for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    member inline r.ExpandYAxis(startDist, endDist) : BRect =
        let n = BRect(r.MinX, r.MinY-startDist, r.MaxX, r.MaxY+endDist)
        if n.MinY > n.MaxY  then
            FsExGeoException.Raise "BRect.ExpandYAxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist r.AsString
        n

    /// Returns true if the two Bounding Rectangles do overlap or touch.
    member inline r.OverlapsWith (a:BRect) =
        not (  r.MinX > a.MaxX
            || a.MinX > r.MaxX
            || a.MinY > r.MaxY
            || r.MinY > a.MaxY )

    /// Returns true if the two Bounding Rectangles do overlap more than a given tolerance distance.
    /// Returns false if the two Bounding Rectangles are just touching.
    member inline r.OverlapsWith (a:BRect,tol) =
        not (  r.MinX > a.MaxX - tol
            || a.MinX > r.MaxX - tol
            || a.MinY > r.MaxY - tol
            || r.MinY > a.MaxY - tol
            )

    /// Returns true if the point is inside or exactly on the bounding Rectangle.
    member inline r.Contains (p:Pt) =
        p.X >= r.MinX &&
        p.X <= r.MaxX &&
        p.Y >= r.MinY &&
        p.Y <= r.MaxY

    /// Returns true if the Rectangle is inside or exactly on the other bounding Rectangle.
    member inline r.Contains (o:BRect) =
        r.Contains(o.MinPt) && r.Contains(o.MaxPt)

    /// Test if Bounding Rectangles are only touching each other from the Outside within a given tolerance
    member b.IsTouching (a:BRect, tol) =
        let xOverlap =  not ( b.MinX > a.MaxX + tol || a.MinX > b.MaxX + tol)
        let yOverlap =  not ( a.MinY > b.MaxY + tol || b.MinY > a.MaxY + tol)
        let xTouch   =  abs(b.MinX - a.MaxX)  < tol || abs(a.MinX - b.MaxX) < tol
        let yTouch   =  abs(a.MinY - b.MaxY)  < tol || abs(b.MinY - a.MaxY) < tol
        (xOverlap && yTouch ) || (xTouch && yOverlap )

    /// Evaluate a X and Y parameter of the Bounding Rectangle.
    ///  0.0, 0.0, 0.0 returns the MinPt.
    ///  1.0, 1.0, 1.0 returns the MaxPt.
    member inline b.EvaluateAt (xParameter,yParameter) =
        Pt (b.MinX + (b.MaxX-b.MinX) * xParameter,
            b.MinY + (b.MaxY-b.MinY) * yParameter)

    /// Returns the area of the Bounding Rectangle.
    member inline r.Area  =
        r.SizeX*r.SizeY

    //-------------------------------------------------------------------
    //------------------------static members---------------------------
    //-------------------------------------------------------------------
    
    /// Checks if two 3D Bounding Boxes are equal within tolerance.
    static member equals tol (a:BRect) (b:BRect) =        
        abs(a.MinX-b.MinX)<tol &&
        abs(a.MinY-b.MinY)<tol &&
        abs(a.MaxX-b.MaxX)<tol &&
        abs(a.MaxY-b.MaxY)<tol 

    /// Returns Bounding Rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expand dist (r:BRect) =
        r.Expand dist

    /// Returns Bounding Rectangle expanded by distance.
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expandSave dist (r:BRect) =
        r.Expand dist

    /// Returns Bounding Rectangle expanded  only in X direction by different distance for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expandXAxis startDist endDist (r:BRect) =
        r.ExpandXAxis(startDist, endDist)


    /// Returns Bounding Rectangle expanded  only in Y direction by different distance for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expandYAxis startDist endDist (r:BRect) =
        r.ExpandYAxis(startDist, endDist)

    /// Move the Bounding Rectangle by a vector.
    static member move (v:Vc) (r:BRect) =
        BRect(r.MinX+v.X, r.MinY+v.Y, r.MaxX+v.X, r.MaxY+v.Y)

    /// Returns true if the two Bounding Rectangles do overlap or touch exactly
    static member inline doOverlap(a:BRect) (b:BRect) =
        b.OverlapsWith(a)

    /// Returns true if the two Bounding Rectangles do overlap more than a given tolerance distance.
    /// Returns false if the two Bounding Rectangles are just touching.
    static member inline doOverlapMoreThan tol (a:BRect) (b:BRect) =
        b.OverlapsWith(a,tol)

    /// Returns true if the Bounding Rectangle is inside or exactly on the other bounding Rectangle.
    /// Argument order matters!
    static member inline contains (rectInside:BRect) (surroundingRect:BRect) =
        surroundingRect.Contains(rectInside)

    /// Returns true if the point is inside or on  the bounding Rectangle
    static member inline containsPt (pt:Pt) (rect:BRect) =
        rect.Contains(pt)


    /// Returns a bounding Rectangle that contains both input Rectangles
    static member inline union (a:BRect) (b:BRect) =
        BRect (min b.MinX a.MinX ,min b.MinY a.MinY,max b.MaxX a.MaxX ,max b.MaxY a.MaxY)

    /// Returns a bounding Rectangle that contains the input Rectangles and the point
    static member inline unionPt (p:Pt) (r:BRect) =
        BRect (min r.MinX p.X ,min r.MinY p.Y, max r.MaxX p.X ,max r.MaxY p.Y)


    /// Finds min and max values for x and y.
    static member inline create (a:Pt , b:Pt ) =
        // sort min and max values ( not using allocating tuples for swapping)
        let mutable minX = a.X
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X
        let mutable minY = a.Y
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        BRect(minX,minY,maxX,maxY)

    /// Finds min and max values for x and y.
    static member inline create (ps:seq<Pt> ) =
        if Seq.isEmpty ps then raise <| FsExGeoException("BRect.create(seq<Pt>) input is empty seq")
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        for p in ps do
            minX <- min minX p.X
            minY <- min minY p.Y
            maxX <- max maxX p.X
            maxY <- max maxY p.Y
        BRect(minX,minY,maxX,maxY)

    /// Does not verify the order of min and max values
    static member inline createUnchecked (minX,minY,maxX,maxY) =
        BRect(minX,minY,maxX,maxY)

    /// Returns the area of the Bounding Rectangle.
    static member inline area  (r:BRect) =
        r.SizeX*r.SizeY
