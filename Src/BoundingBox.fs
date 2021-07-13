namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike


#nowarn "44" // for hidden constructors via Obsolete Attribute  
    
/// A 3D Bounding Boxangle.
/// Sometimes also called 2D Bounding Box.
[<Struct; NoEquality; NoComparison>] 
[<IsReadOnly>]
//[<IsByRefLike>]
type BBox =
    val MinX : float
    val MinY : float
    val MinZ : float
    val MaxX : float
    val MaxY : float
    val MaxZ : float
    
    /// Unsafe internal constructor,  public only for inlining.
    [<Obsolete("Unsafe internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (minX,minY,minZ,maxX,maxY,maxZ) =
        {MinX = minX
         MinY = minY
         MinZ = minZ
         MaxX = maxX
         MaxY = maxY
         MaxZ = maxZ
         }    
    
    override b.ToString() =  
        sprintf "FsEx.Geo.BBox: length(x)= %s width(y)=%s , height(z)=%s (at X=%s Y=%s Z=%s)" 
            (Format.float (b.MaxX - b.MinX)) (Format.float (b.MaxY - b.MinY)) (Format.float (b.MaxZ - b.MinZ)) 
            (Format.float b.MinX) (Format.float b.MinY) (Format.float b.MinZ)
    
    member inline b.MinPnt = Pnt(b.MinX,b.MinY,b.MinZ)

    member inline b.MaxPnt = Pnt(b.MaxX,b.MaxY,b.MinZ)    

    /// The size in X direction 
    member inline b.Length = b.MaxX - b.MinX

    /// The size in Y direction 
    member inline b.Width  = b.MaxY - b.MinY

    /// The size in Z direction 
    member inline b.Height  = b.MaxZ - b.MinZ

    member inline b.Diagonal = Vec(b.MaxX - b.MinX, b.MaxY - b.MinY, b.MaxZ - b.MinZ)

    member inline b.Center = Pnt( (b.MaxX + b.MinX)*0.5, (b.MaxY + b.MinY)*0.5, (b.MaxZ + b.MinZ)*0.5  )

    /// Returns the bottom corners of the Box as in counter clockwise order, starting at MinPt
    /// Then the top corners staring above  MinPt. Array of 8 Points    
    member b.Corners = [| Pnt(b.MinX, b.MinY, b.MinZ); Pnt(b.MaxX, b.MinY, b.MinZ);  Pnt(b.MaxX, b.MaxY, b.MinZ); Pnt(b.MinX, b.MaxY, b.MinZ)
                          Pnt(b.MinX, b.MinY, b.MaxZ); Pnt(b.MaxX, b.MinY, b.MaxZ);  Pnt(b.MaxX, b.MaxY, b.MaxZ); Pnt(b.MinX, b.MaxY, b.MaxZ)|]

    /// Returns the bottom of the Box as a counter clockwise array of 4 Points, starting at MinPt 
    /// Last and first Point are NOT the same
    member b.PolylineBottom = [| Pnt(b.MinX, b.MinY,b.MinZ); Pnt(b.MaxX, b.MinY,b.MinZ);  Pnt(b.MaxX, b.MaxY,b.MinZ); Pnt(b.MinX, b.MaxY,b.MinZ) |]

    /// Returns the bottom of the Box as a counter clockwise array of 5 Points, starting at MinPt
    /// Last and first Point are the same
    member b.PolylineBottomClosed = [| Pnt(b.MinX, b.MinY,b.MinZ); Pnt(b.MaxX, b.MinY,b.MinZ);  Pnt(b.MaxX, b.MaxY,b.MinZ); Pnt(b.MinX, b.MaxY,b.MinZ); Pnt(b.MinX, b.MinY,b.MinZ)|]

    /// Checks that min X and Y are smaller than max X and Y.
    /// This might happen if the Boxangle is expanded by a negative value bigger than the BBox.
    member inline b.IsValid =   b.MinX <= b.MaxX && b.MinY <= b.MaxX && b.MinZ <= b.MaxZ

    /// Checks if min X or Y are bigger than max X or Y.
    /// This might happen if the Boxangle is expanded by a negative value bigger than the BBox.
    member inline b.IsNotValid =   b.MinX > b.MaxX || b.MinY > b.MaxX || b.MinZ > b.MaxZ


    /// Returns Bounding Boxangle expanded by distance
    /// Does not check overflow if distance is negative.
    member inline b.Expand(d) = 
        BBox(b.MinX-d, b.MinY-d, b.MaxX+d, b.MaxY+d, b.MaxZ+d, b.MaxZ+d)


    /// Returns true if the two bounding Boxangles do overlap or touch
    member inline b.OverlapsWith (a:BBox) =
        not (  b.MinX > a.MaxX
            || a.MinX > b.MaxX
            || a.MinY > b.MaxY 
            || b.MinY > a.MaxY 
            || a.MinZ > b.MaxZ 
            || b.MinZ > a.MaxZ            
            )
    
    /// Returns true if the point is inside or excatly on the bounding Boxangle
    member inline b.Contains (p:Pnt) =
        p.X >= b.MinX &&
        p.X <= b.MaxX &&
        p.Y >= b.MinY &&
        p.Y <= b.MaxY &&
        p.Z >= b.MinZ &&
        p.Z <= b.MaxZ 

    /// Returns true if the Boxangle is inside or excatly on the other bounding Boxangle
    member inline b.Contains (o:BBox) =
        b.Contains(o.MinPnt) && b.Contains(o.MaxPnt)     

    
    //-------------------------------------------------------------------
    //------------------------static members---------------------------
    //-------------------------------------------------------------------

    /// Returns Bounding Boxangle expanded by distance
    /// Does not check overflow if distance is negative.
    static member expand dist (b:BBox) = 
        BBox(b.MinX-dist, b.MinY-dist, b.MaxX+dist, b.MaxY+dist, b.MaxZ+dist, b.MaxZ+dist)


    /// Returns true if the two bounding Boxangles do overlap or touch excatly
    static member inline doOverlap(a:BBox) (b:BBox) =
        not (  b.MinX > a.MaxX
            || a.MinX > b.MaxX
            || a.MinY > b.MaxY 
            || b.MinY > a.MaxY
            || a.MinZ > b.MaxZ 
            || b.MinZ > a.MaxZ             
            )
    
    /// Returns true if the point is inside or on  the bounding Boxangle
    static member inline contains (p:Pnt) (b:BBox) =
        p.X >= b.MinX &&
        p.X <= b.MaxX &&
        p.Y >= b.MinY &&
        p.Y <= b.MaxY &&
        p.Z >= b.MinZ &&
        p.Z <= b.MaxZ 
    
    /// Returns a bounding Boxangle that contains both input Boxangles
    static member inline union (a:BBox) (b:BBox) =
        BBox (min b.MinX a.MinX ,min b.MinY a.MinY,min b.MinZ a.MinZ,
              max b.MaxX a.MaxX ,max b.MaxY a.MaxY,max b.MaxZ a.MaxZ)
    
    /// Returns a bounding Boxangle that contains the input Boxangles and the point
    static member inline unionPt (p:Pnt) (b:BBox) =
        BBox (min b.MinX p.X ,min b.MinY p.Y,min b.MinZ p.Z, 
              max b.MaxX p.X ,max b.MaxY p.Y,max b.MaxZ p.Z)
        

    /// Finds min and max values for x ,y and z.
    /// Adds the Expansion value is used to shrink lowwer bound and increase upper bound.
    /// Total size is bigger by expansion times two.
    /// If expansion is negative it shrinks the Boxangle. It also makes sure that there is no overflow 
    /// when the negative expansion is bigger than the size.
    static member create (a:Pnt , b:Pnt,  expansion ) = 
        // sort min and max values ( not useing allocating tuples for swaping) 
        let mutable minX = a.X  
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X 
        let mutable minY = a.Y  
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        let mutable minZ = a.Z  
        let maxZ = if b.Z > minZ then b.Z else minZ <- b.Z ;  a.Z

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
        // expand Z:
        let mutable minZCh = minZ - expansion 
        let mutable maxZCh = maxZ + expansion 
        if minZCh > maxZCh then  // Overflow! Set both to the same mid point
            let mid = minZ + (maxZ-minZ) * 0.5
            minZCh <- mid
            maxZCh <- mid
        BBox(minXCh, minYCh, minZCh,
             maxXCh, maxYCh, maxZCh)
    
    /// Finds min and max values for x and y.
    static member inline create (a:Pnt , b:Pnt ) =
        // sort min and max values ( not useing allocating tuples for swaping) 
        let mutable minX = a.X  
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X 
        let mutable minY = a.Y  
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        let mutable minZ = a.Z  
        let maxZ = if b.Z > minZ then b.Z else minZ <- b.Z ;  a.Z
        BBox(minX,minY,minZ,maxX,maxY,maxZ)


    /// Finds min and max values for x and y.
    static member inline create (ps:seq<Pnt> ) =
        if Seq.isEmpty ps then raise <| FsExGeoException("BBox.create(seq<Pt>) input is empty seq")
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable minZ = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        let mutable maxZ = Double.MinValue
        for p in ps do
            minX <- min minX p.X 
            minY <- min minY p.Y
            minZ <- min minZ p.Z
            maxX <- max maxX p.X 
            maxY <- max maxY p.Y
            maxZ <- max maxZ p.Z
        BBox(minX,minY,minZ,maxX,maxY,maxZ)
    
    /// Does not verify the order of min and max values
    static member inline createUnchecked (minX,minY,minZ,maxX,maxY,maxZ) = 
        BBox(minX,minY,minZ,maxX,maxY,maxZ)
