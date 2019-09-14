namespace FsEx.Geo
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
    
/// A 2D Bounding box
[<Struct; NoEquality; NoComparison>] 
[<IsReadOnly>]
//[<IsByRefLike>]
type BBox =
    val MinX : float
    val MinY : float
    val MaxX : float
    val MaxY : float
    
    /// Adds the Expansion value is used to shrink lowwer bound and increase upper bound.
    /// Total size is bigger by expansion times two.
    /// If expansion is negative it shrinks the Box. It also makes sure that there is no overflow 
    /// when the negative expansion is bigger than the size.
    new (a:Pt , b:Pt,  expansion ) = 
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

        {MinX = minXCh
         MinY = minYCh
         MaxX = maxXCh
         MaxY = maxYCh}

    new (a:Pt , b:Pt ) =
        // sort min and max values ( not useing allocating tuples for swaping) 
        let mutable minX = a.X  
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X 
        let mutable minY = a.Y  
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        {MinX = minX
         MinY = minY
         MaxX = maxX
         MaxY = maxY}
    
    override b.ToString() = $"FsEx.Geo.BBox: width=%s{Format.float (b.MaxX - b.MinX)} , height=%s{Format.float (b.MaxY - b.MinY)} (at X=%s{Format.float b.MinX}  Y=%s{Format.float b.MinY})"

    member inline b.Height = b.MaxY - b.MinY

    member inline b.Width  = b.MaxX - b.MinX

    member inline b.Diagonal = Vc(b.MaxX - b.MinX, b.MaxY - b.MinY)

    member inline b.Center = Pt( (b.MaxX + b.MinX)*0.5, (b.MaxY + b.MinY)*0.5 )

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
    member inline b.OverlapsWith (a:BBox) =
        not (  b.MinX > a.MaxX
            || b.MaxX < a.MinX
            || b.MaxY < a.MinY
            || b.MinY > a.MaxY )

    /// Returns true if the two bounding boxes do overlap or touch
    static member inline doOverlap(a:BBox) (b:BBox) =
        not (  b.MinX > a.MaxX
            || b.MaxX < a.MinX
            || b.MaxY < a.MinY
            || b.MinY > a.MaxY )
