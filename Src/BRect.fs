namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike


#nowarn "44" // for hidden constructors via Obsolete Attribute  
    
/// A 2D Bounding Rectangle.
/// Sometimes also called 2D Bounding Box.
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
    
    override r.ToString() =  sprintf "FsEx.Geo.BRect: length(x)=%s , width(y)=%s (at X=%s  Y=%s)" (Format.float (r.MaxX - r.MinX)) (Format.float (r.MaxY - r.MinY)) (Format.float r.MinX) (Format.float r.MinY)
    
    member inline r.MinPt = Pt(r.MinX,r.MinY)

    member inline r.MaxPt = Pt(r.MaxX,r.MaxY)
    
    /// the size in Y direction 
    member inline r.Width  = r.MaxY - r.MinY

    /// the size in X direction 
    member inline r.Length = r.MaxX - r.MinX

    member inline r.Diagonal = Vc(r.MaxX - r.MinX, r.MaxY - r.MinY)

    member inline r.Center = Pt( (r.MaxX + r.MinX)*0.5, (r.MaxY + r.MinY)*0.5 )

    /// Returns a counter clockwise array of 4 Points, starting at MinPt 
    /// Last and first Point are NOT the same
    member r.Polyline = [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY) |]

    /// Returns a counter clockwise array of 5 Points, starting at MinPt
    /// Last and first Point are the same
    member r.PolylineClosed = [| Pt(r.MinX, r.MinY); Pt(r.MaxX, r.MinY);  Pt(r.MaxX, r.MaxY); Pt(r.MinX, r.MaxY); Pt(r.MinX, r.MinY)|]

    /// Checks that min X and Y are smaller than max X and Y.
    /// This might happen if the Rectangle is expanded by a negative value bigger than the BRect.
    member inline r.IsValid =   r.MinX <= r.MaxX && r.MinY <= r.MaxX

    /// Checks if min X or Y are bigger than max X or Y.
    /// This might happen if the Rectangle is expanded by a negative value bigger than the BRect.
    member inline r.IsNotValid =   r.MinX > r.MaxX || r.MinY > r.MaxX


    /// Returns Bounding Rectangle expanded by distance
    /// Does not check overflow if distance is negative.
    member inline r.Expand(d) = 
        BRect(r.MinX-d, r.MinY-d, r.MaxX+d, r.MaxY+d)

      
    /// Returns true if the two bounding Rectangles do overlap or touch
    member inline r.OverlapsWith (a:BRect) =
        not (  r.MinX > a.MaxX
            || a.MinX > r.MaxX
            || a.MinY > r.MaxY 
            || r.MinY > a.MaxY )
    
    /// Returns true if the point is inside or excatly on the bounding Rectangle
    member inline r.Contains (p:Pt) =
        p.X >= r.MinX &&
        p.X <= r.MaxX &&
        p.Y >= r.MinY &&
        p.Y <= r.MaxY 

    /// Returns true if the Rectangle is inside or excatly on the other bounding Rectangle
    member inline r.Contains (o:BRect) =
        r.Contains(o.MinPt) && r.Contains(o.MaxPt)     


    /// Returns true if the two bounding Rectangles do overlap or touch excatly
    static member inline doOverlap(a:BRect) (r:BRect) =
        not (  r.MinX > a.MaxX
            || a.MinX > r.MaxX
            || a.MinY > r.MaxY 
            || r.MinY > a.MaxY )
    
    /// Returns true if the point is inside or on  the bounding Rectangle
    static member inline contains (p:Pt) (r:BRect) =
        p.X >= r.MinX &&
        p.X <= r.MaxX &&
        p.Y >= r.MinY &&
        p.Y <= r.MaxY 
    
    /// Returns a bounding Rectangle that contains both input Rectangles
    static member inline union (a:BRect) (b:BRect) =
        BRect (min b.MinX a.MinX ,min b.MinY a.MinY,max b.MaxX a.MaxX ,max b.MaxY a.MaxY)
    
    /// Returns a bounding Rectangle that contains the input Rectangles and the point
    static member inline unionPt (p:Pt) (r:BRect) =
        BRect (min r.MinX p.X ,min r.MinY p.Y, max r.MaxX p.X ,max r.MaxY p.Y)
        

    /// Finds min and max values for x and y.
    /// Adds the Expansion value is used to shrink lowwer bound and increase upper bound.
    /// Total size is bigger by expansion times two.
    /// If expansion is negative it shrinks the Rectangle. It also makes sure that there is no overflow 
    /// when the negative expansion is bigger than the size.
    static member create (a:Pt , b:Pt,  expansion ) = 
        // sort min and max values ( not useing allocating tuples for swaping) 
        let mutable minX = a.X  
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X 
        let mutable minY = a.Y  
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        // expand X:
        // Moves both 'mi' and 'ma' by 'expansion' amount away from each other.
        // The interval increase by expansion.
        // If expansion is negative it shrinks the interval. It also makes sure that there is no overflow
        // when 'mi' and 'ma' are closer than the negative expansion.
        // The mid point between  'mi' and 'ma' will be returned in that case.
        let mutable minXCh = minX - expansion 
        let mutable maxXCh = maxX + expansion 
        if minXCh > maxXCh then  // Overflow! Set both to the same mid point
            let mid = minX + (maxX-minX) * 0.5
            minXCh <- mid
            maxXCh <- mid
        // expand Y:
        let mutable minYCh = minY - expansion 
        let mutable maxYCh = maxY + expansion 
        if minYCh > maxYCh then  // Overflow! Set both to the same mid point
            let mid = minY + (maxY-minY) * 0.5
            minYCh <- mid
            maxYCh <- mid

        BRect(minXCh, minYCh,maxXCh,maxYCh)
    
    /// Finds min and max values for x and y.
    static member inline create (a:Pt , b:Pt ) =
        // sort min and max values ( not useing allocating tuples for swaping) 
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
