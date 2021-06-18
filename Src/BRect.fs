namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike


#nowarn "44" // for hidden constructors via Obsolete Attribute  
    
/// A 2D Bounding box
[<Struct; NoEquality; NoComparison>] 
[<IsReadOnly>]
//[<IsByRefLike>]
type BRect =
    val MinX : float
    val MinY : float
    val MaxX : float
    val MaxY : float
    
    /// Unsave internal constructor,  public only for inlining.
    [<Obsolete("Unsave internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (minX,minY,maxX,maxY) =
        {MinX = minX
         MinY = minY
         MaxX = maxX
         MaxY = maxY}    
    
    override b.ToString() = $"FsEx.Geo.BRect: width=%s{Format.float (b.MaxX - b.MinX)} , height=%s{Format.float (b.MaxY - b.MinY)} (at X=%s{Format.float b.MinX}  Y=%s{Format.float b.MinY})"
    
    member inline b.MinPt = Pt(b.MinX,b.MinY)

    member inline b.MaxPt = Pt(b.MaxX,b.MaxY)
    
    member inline b.Height = b.MaxY - b.MinY

    member inline b.Width  = b.MaxX - b.MinX

    member inline b.Diagonal = Vc(b.MaxX - b.MinX, b.MaxY - b.MinY)

    member inline b.Center = Pt( (b.MaxX + b.MinX)*0.5, (b.MaxY + b.MinY)*0.5 )

    /// Checks that min X and Y are smaller than max X and Y.
    /// This might happen if the box is expanded by a negative value bigger than the BRect.
    member inline b.IsValid =   b.MinX <= b.MaxX && b.MinY <= b.MaxX

    /// Checks if min X or Y are bigger than max X or Y.
    /// This might happen if the box is expanded by a negative value bigger than the BRect.
    member inline b.IsNotValid =   b.MinX > b.MaxX || b.MinY > b.MaxX


    /// Returns Bounding box expanded by distance
    /// Does not check overflow if distance is negative.
    member inline b.Expand(d) = 
        BRect(b.MinX-d, b.MinY-d, b.MaxX+d, b.MaxY+d)

    /// As counterclockwise closed loop (last Pt = first Pt)
    /// Starting at bbox.Min
    member b.AsPolyLine = 
        let a = Array.zeroCreate 5
        a.[0] <- Pt(b.MinX, b.MinY)
        a.[1] <- Pt(b.MaxX, b.MinY)
        a.[2] <- Pt(b.MaxX, b.MaxY)
        a.[3] <- Pt(b.MinX, b.MaxY)
        a.[4] <- Pt(b.MinX, b.MinY)
        a    
    
    /// Returns true if the two bounding boxes do overlap or touch
    member inline b.OverlapsWith (a:BRect) =
        not (  b.MinX > a.MaxX
            || a.MinX > b.MaxX
            || a.MinY > b.MaxY 
            || b.MinY > a.MaxY )
    
    /// Returns true if the point is inside or on the bounding box
    member inline b.Contains (p:Pt) =
        p.X >= b.MinX &&
        p.X <= b.MaxX &&
        p.Y >= b.MinY &&
        p.Y <= b.MaxY 

    /// Returns true if the Box  is inside or on the other bounding box
    member inline b.Contains (o:BRect) =
        b.Contains(o.MinPt) && b.Contains(o.MaxPt)     


    /// Returns true if the two bounding boxes do overlap or touch
    static member inline doOverlap(a:BRect) (b:BRect) =
        not (  b.MinX > a.MaxX
            || a.MinX > b.MaxX
            || a.MinY > b.MaxY 
            || b.MinY > a.MaxY )
    
    /// Returns true if the point is inside or on  the bounding box
    static member inline contains (p:Pt) (b:BRect) =
        p.X >= b.MinX &&
        p.X <= b.MaxX &&
        p.Y >= b.MinY &&
        p.Y <= b.MaxY 
    
    /// Returns a bounding box that contains both input boxes
    static member inline union (a:BRect) (b:BRect) =
        BRect (min b.MinX a.MinX ,min b.MinY a.MinY,max b.MaxX a.MaxX ,max b.MaxY a.MaxY)
    
    /// Returns a bounding box that contains the input boxes and the point
    static member inline unionPt (p:Pt) (b:BRect) =
        BRect (min b.MinX p.X ,min b.MinY p.Y, max b.MaxX p.X ,max b.MaxY p.Y)
        

    /// Finds min and max values for x and y.
    /// Adds the Expansion value is used to shrink lowwer bound and increase upper bound.
    /// Total size is bigger by expansion times two.
    /// If expansion is negative it shrinks the Box. It also makes sure that there is no overflow 
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