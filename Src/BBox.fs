namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike


#nowarn "44" // for hidden constructors via Obsolete Attribute


/// An immutable 3D Bounding Box.
/// This implementation guarantees the box to be always valid.
/// That means the Min X, Y and Z values are always smaller or equal than the respective Max values
///         
///   Z-Axis       Y-Axis
///   ^           /
///   |   7      /        6 MaxPt
///   |   +---------------+
///   |  /|    /         /|
///   | / |   /         / |
/// 4 |/  |  /       5 /  |
///   +---------------+   |
///   |   |/          |   |
///   |   +-----------|---+
///   |  / 3          |  / 2
///   | /             | /
///   |/              |/     
///   +---------------+----> X-Axis
///   0 MinPt         1
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

    /// Nicely formatted string representation of the BoundingBox, including its size.
    override b.ToString() =
        sprintf "FsEx.Geo.BBox: length(x)= %s width(y)=%s , height(z)=%s (at X=%s | Y=%s | Z=%s)"
            (Format.float (b.MaxX - b.MinX)) (Format.float (b.MaxY - b.MinY)) (Format.float (b.MaxZ - b.MinZ))
            (Format.float b.MinX) (Format.float b.MinY) (Format.float b.MinZ)

    /// Format BoundingBox into string with nice floating point number formatting of size and position
    /// But without full type name as in bbox.ToString()
    member b.AsString =
        sprintf "len= %s wid=%s , hei=%s (at X=%s | Y=%s | Z=%s)"
            (Format.float (b.MaxX - b.MinX)) (Format.float (b.MaxY - b.MinY)) (Format.float (b.MaxZ - b.MinZ))
            (Format.float b.MinX) (Format.float b.MinY) (Format.float b.MinZ)

    /// The Point where X, Y and Z are the minimum values.
    member inline b.MinPnt = Pnt(b.MinX,b.MinY,b.MinZ)

    /// The Point where X, Y and Z are the maximum values.
    member inline b.MaxPnt = Pnt(b.MaxX,b.MaxY,b.MinZ)

    /// The size in X direction, same as member box.SizeX.
    member inline b.Length = b.MaxX - b.MinX
    /// The size in X direction, same as member box.Length.
    member inline b.SizeX = b.MaxX - b.MinX

    /// The size in Y direction, same as member box.SizeY.
    member inline b.Width  = b.MaxY - b.MinY
    /// The size in Y direction, same as member box.Width.
    member inline b.SizeY  = b.MaxY - b.MinY

    /// The size in Z direction, same as member box.SizeZ.
    member inline b.Height  = b.MaxZ - b.MinZ
    /// The size in Z direction, same as member box.Height.
    member inline b.SizeZ  = b.MaxZ - b.MinZ

    /// The diagonal 3D vector of the bounding box. From MinPnt to MaxPnt.
    member inline b.Diagonal = Vec(b.MaxX - b.MinX, b.MaxY - b.MinY, b.MaxZ - b.MinZ)

    /// The center of the bounding box.
    member inline b.Center = Pnt( (b.MaxX + b.MinX)*0.5, (b.MaxY + b.MinY)*0.5, (b.MaxZ + b.MinZ)*0.5  )

    /// Returns Point 0 of the bounding box, same as member box.MinPnt.
    ///        
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member inline b.Pt0 = Pnt(b.MinX,b.MinY,b.MinZ)

    /// Returns Point 1 of the bounding box.
    ///        
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member inline b.Pt1 = Pnt(b.MaxX,b.MinY,b.MinZ)

    /// Returns Point 2 of the bounding box.
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member inline b.Pt2 = Pnt(b.MaxX,b.MaxY,b.MinZ)

    /// Returns Point 3 of the bounding box.
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member inline b.Pt3 = Pnt(b.MinX,b.MaxY,b.MinZ)
    
    /// Returns Point 4 of the bounding box.
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member inline b.Pt4 = Pnt(b.MinX,b.MinY,b.MaxZ)
    
    /// Returns Point 5 of the bounding box.
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    
    member inline b.Pt5 = Pnt(b.MaxX,b.MinY,b.MaxZ)
    /// Returns Point 6 of the bounding box.
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member inline b.Pt6 = Pnt(b.MaxX,b.MaxY,b.MaxZ)

    /// Returns Point 7 of the bounding box.
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member inline b.Pt7 = Pnt(b.MinX,b.MaxY,b.MaxZ)

    /// Returns the bottom corners of the Bounding Box in counter clockwise order, starting at MinPt
    /// Then the top corners staring above  MinPt. Returns an array of 8 Points.
    ///        
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member b.Corners = [|b.Pt0; b.Pt1; b.Pt2; b.Pt3; b.Pt4; b.Pt5; b.Pt6; b.Pt7|]

    /// Returns the bottom of the Box as a counter clockwise array of 4 Points.
    /// Starting at MinPt.  Point 0, 1, 2 and 3.
    /// Last and first point are NOT the same.
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member b.BottomPoints = [|b.Pt0; b.Pt1; b.Pt2; b.Pt3|]
        

    /// Returns the bottom of the Box as a counter clockwise array of 5 Points, starting at MinPt
    /// Starting at MinPt.  Point 0, 1, 2, 3 and again 0.
    /// Last and first point are the same
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member b.BottomPointsClosed = [|b.Pt0; b.Pt1; b.Pt2; b.Pt3; b.Pt0|]

    /// Returns the bottom of the Box as a counter clockwise array of 4 Points.
    /// Staring at Point 4 then 5, 6 and 7.
    /// Last and first point are NOT the same.
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member b.TopPoints = [|b.Pt4; b.Pt5; b.Pt6; b.Pt7|]        

    /// Returns the bottom of the Box as a counter clockwise array of 5 Points.
    /// Starting Point 4 then 5, 6 ,7 and again 4.
    /// Last and first point are the same
    ///         
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6 MaxPt
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     
    ///   +---------------+----> X-Axis
    ///   0 MinPt         1
    member b.TopPointsClosed = [|b.Pt4; b.Pt5; b.Pt6; b.Pt7; b.Pt4|] 


    /// Returns Bounding Box expanded by distance.
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    member inline b.Expand(dist) : BBox =
        let n = BBox(   b.MinX-dist, b.MinY-dist, b.MinZ-dist,
                        b.MaxX+dist, b.MaxY+dist, b.MaxZ+dist)
        if dist<0. &&  (n.MinX > n.MaxX ||  n.MinY > n.MaxX ||  n.MinZ > n.MaxZ) then
            FsExGeoException.Raise "BBox.Expand(dist): Negative distance %g cause an underflow, on %s" dist b.AsString
        n

    /// Returns Bounding Box expanded by a distance for X , Y and Z Axis each.
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    member inline b.Expand(xDist,yDist,zDist) : BBox =
        let n = BBox(   b.MinX-xDist, b.MinY-yDist, b.MinZ-zDist,
                        b.MaxX+xDist, b.MaxY+yDist, b.MaxZ+zDist)
        if n.MinX > n.MaxX ||  n.MinY > n.MaxX ||  n.MinZ > n.MaxZ then
            FsExGeoException.Raise "BBox.Expand(x,y,z): Negative distance(s) X %g Y: %g and Z:%g cause an underflow, on %s" xDist yDist zDist b.AsString
        n

    /// Returns Bounding Box expanded by a distance for X , Y and Z Axis each.
    /// If expansion is negative it shrinks the Box. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    member inline b.ExpandSave(xDist,yDist,zDist) : BBox =
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
        // expand Z:
        let mutable minZCh = b.MinZ - zDist
        let mutable maxZCh = b.MaxZ + zDist
        if minZCh > maxZCh then  // Overflow! Set both to the same mid point
            let mid = b.MinZ + (b.MaxZ-b.MinZ) * 0.5
            minZCh <- mid
            maxZCh <- mid
        BBox(minXCh, minYCh, minZCh, maxXCh, maxYCh, maxZCh)

    /// Returns Bounding Box expanded by a distance.
    /// If expansion is negative it shrinks the Box. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    member inline b.ExpandSave(dist) : BBox =
        b.ExpandSave(dist,dist,dist)

    /// Returns Bounding Box expanded only in X direction by different distance for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    member inline b.ExpandXaxis(startDist, endDist) : BBox =
        let n = BBox(b.MinX-startDist, b.MinY, b.MinZ, b.MaxX+endDist, b.MaxY, b.MaxZ)
        if n.MinX > n.MaxX  then
            FsExGeoException.Raise "BBox.ExpandXaxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist b.AsString
        n

    /// Returns Bounding Box expanded  only in Y direction by different distance for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    member inline b.ExpandYaxis(startDist, endDist) : BBox =
        let n = BBox(b.MinX, b.MinY-startDist, b.MinZ, b.MaxX, b.MaxY+endDist, b.MaxZ)
        if n.MinY > n.MaxY  then
            FsExGeoException.Raise "BBox.ExpandYaxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist b.AsString
        n

    /// Returns Bounding Box expanded  only in Z direction by different distance for start(minZ) and end (maxZ).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    member inline b.ExpandZaxis(startDist, endDist) : BBox =
        let n = BBox(b.MinX, b.MinY, b.MinZ-startDist, b.MaxX, b.MaxY, b.MaxZ+endDist)
        if n.MinZ > n.MaxZ  then
            FsExGeoException.Raise "BBox.ExpandYaxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist b.AsString
        n

    /// Returns true if the two Bounding Boxes do overlap or touch.
    member inline b.OverlapsWith (a:BBox) =
        not (  b.MinX > a.MaxX
            || a.MinX > b.MaxX
            || a.MinY > b.MaxY
            || b.MinY > a.MaxY
            || a.MinZ > b.MaxZ
            || b.MinZ > a.MaxZ
            )

    /// Returns true if the two Bounding Boxes do overlap more than a given tolerance distance.
    /// Returns false if the two Bounding Boxes are just touching.
    member inline b.OverlapsWith (a:BBox, tol) =
        not (  b.MinX > a.MaxX - tol
            || a.MinX > b.MaxX - tol
            || a.MinY > b.MaxY - tol
            || b.MinY > a.MaxY - tol
            || a.MinZ > b.MaxZ - tol
            || b.MinZ > a.MaxZ - tol
            )

    /// Returns true if the point is inside or exactly on the bounding Box.
    member inline b.Contains (p:Pnt) =
        p.X >= b.MinX &&
        p.X <= b.MaxX &&
        p.Y >= b.MinY &&
        p.Y <= b.MaxY &&
        p.Z >= b.MinZ &&
        p.Z <= b.MaxZ

    /// Returns true if the Bounding Box is inside or exactly on the other bounding Box.
    member inline b.Contains (o:BBox) =
        b.Contains(o.MinPnt) && b.Contains(o.MaxPnt)

    /// Test if Bounding Boxes are only touching each other from the Outside within a given tolerance
    member b.IsTouching (a:BBox, tol) =
        let xOverlap =  not ( b.MinX > a.MaxX + tol || a.MinX > b.MaxX + tol)
        let yOverlap =  not ( a.MinY > b.MaxY + tol || b.MinY > a.MaxY + tol)
        let zOverlap =  not ( a.MinZ > b.MaxZ + tol || b.MinZ > a.MaxZ + tol)
        let xTouch   =  abs(b.MinX - a.MaxX)  < tol || abs(a.MinX - b.MaxX) < tol
        let yTouch   =  abs(a.MinY - b.MaxY)  < tol || abs(b.MinY - a.MaxY) < tol
        let zTouch   =  abs(a.MinZ - b.MaxZ)  < tol || abs(b.MinZ - a.MaxZ) < tol
        (xOverlap && yOverlap && zTouch  ) ||
        (xOverlap && yTouch   && zOverlap) ||
        (xTouch   && yOverlap && zOverlap)


    /// Evaluate a X,Y and Z parameter of the Bounding Box.
    ///  0.0, 0.0, 0.0 returns the MinPnt.
    ///  1.0, 1.0, 1.0 returns the MaxPnt.
    member inline b.EvaluateAt (xParameter,yParameter,zParameter) =
        Pnt(b.MinX + (b.MaxX-b.MinX) * xParameter,
            b.MinY + (b.MaxY-b.MinY) * yParameter,
            b.MinZ + (b.MaxZ-b.MinZ) * zParameter)

    /// Returns the volume of the Bounding Box.
    member inline b.Volume  =
        b.SizeX*b.SizeY*b.SizeZ

    /// Returns the 2D part of this Bounding Box as a Bounding Rectangle.
    member inline b.asRect =
        BRect.createUnchecked(b.MinX,b.MinY,b.MaxX,b.MaxY)

    
    /// Returns a Bounding Box that contains both input Bounding Box.
    member inline b.Union (a:BBox)  =
        BBox   (min b.MinX a.MinX ,min b.MinY a.MinY,min b.MinZ a.MinZ,
                max b.MaxX a.MaxX ,max b.MaxY a.MaxY,max b.MaxZ a.MaxZ)

    /// Returns a bounding Bounding Box that contains the input Bounding Box and the point.
    member inline b.Union (p:Pnt)  =
        BBox   (min b.MinX p.X ,min b.MinY p.Y,min b.MinZ p.Z,
                max b.MaxX p.X ,max b.MaxY p.Y,max b.MaxZ p.Z)

    /// Returns the intersection of two Bounding Boxes.
    /// The returned Box is the volume inside both input boxes.
    /// Returns ValueNone if the two boxes do not overlap.
    member inline b.Intersection (a:BBox)  =
        let mutable minX = max a.MinX b.MinX
        let mutable minY = max a.MinY b.MinY
        let mutable minZ = max a.MinZ b.MinZ
        let mutable maxX = min a.MaxX b.MaxX
        let mutable maxY = min a.MaxY b.MaxY
        let mutable maxZ = min a.MaxZ b.MaxZ
        if minX <= maxX && minY <= maxY && minZ <= maxZ then
            ValueSome (BBox(minX,minY,minZ,maxX,maxY,maxZ))
        else
            ValueNone

    //-------------------------------------------------------------------
    //------------------------static members---------------------------
    //-------------------------------------------------------------------

    /// Checks if two 3D Bounding Boxes are equal within tolerance.
    static member equals tol (a:BBox) (b:BBox) =        
        abs(a.MinX-b.MinX)<tol &&
        abs(a.MinY-b.MinY)<tol &&
        abs(a.MinZ-b.MinZ)<tol &&
        abs(a.MaxX-b.MaxX)<tol &&
        abs(a.MaxY-b.MaxY)<tol &&
        abs(a.MaxZ-b.MaxZ)<tol 
        

    /// Returns Bounding Box expanded by distance.
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expand dist (b:BBox) =
        b.Expand dist

    /// Returns Bounding Box expanded by distance.
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expandSave dist (b:BBox) =
        b.Expand dist

    /// Returns Bounding Rectangle expanded  only in X direction by different distance for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expandXaxis startDist endDist (b:BBox) =
        b.ExpandXaxis(startDist, endDist)


    /// Returns Bounding Rectangle expanded  only in Y direction by different distance for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expandYaxis startDist endDist (b:BBox) =
        b.ExpandYaxis(startDist, endDist)

    /// Returns Bounding Rectangle expanded  only in Z direction by different distance for start(minZ) and end (maxZ).
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expandZaxis startDist endDist (b:BBox) =
        b.ExpandZaxis(startDist, endDist)

    /// Move the Bounding Box by a vector.
    static member move (v:Vec) (b:BBox) =
        BBox(b.MinX+v.X, b.MinY+v.Y, b.MinZ+v.Z, b.MaxX+v.X, b.MaxY+v.Y, b.MaxZ+v.Z)

    /// Returns true if the two Bounding Boxes do overlap or touch exactly.
    static member inline doOverlap(a:BBox) (b:BBox) =
        b.OverlapsWith(a)

    /// Returns true if the two Bounding Boxes do overlap more than a given tolerance distance.
    /// Returns false if the two Bounding Boxes are just touching.
    static member inline doOverlapMoreThan tol (a:BBox) (b:BBox) =
        b.OverlapsWith(a,tol)

    /// Returns true if the Bounding Box is inside or exactly on the other bounding Box.
    /// Argument order matters!
    static member inline contains (boxInside:BBox) (surroundingBox:BBox) =
        surroundingBox.Contains(boxInside)

    /// Returns true if the point is inside or on the Bounding Box.
    static member inline containsPnt (pt:Pnt) (box:BBox) =
        box.Contains(pt)

    /// Returns a Bounding Box that contains both input Bounding Box.
    static member inline union (a:BBox) (b:BBox) =
        BBox   (min b.MinX a.MinX ,min b.MinY a.MinY,min b.MinZ a.MinZ,
                max b.MaxX a.MaxX ,max b.MaxY a.MaxY,max b.MaxZ a.MaxZ)

    /// Returns a bounding Bounding Box that contains the input Bounding Box and the point.
    static member inline unionPt (p:Pnt) (b:BBox) =
        BBox   (min b.MinX p.X ,min b.MinY p.Y,min b.MinZ p.Z,
                max b.MaxX p.X ,max b.MaxY p.Y,max b.MaxZ p.Z)

    /// Returns the intersection of two Bounding Boxes.
    /// The returned Box is the volume inside both input boxes.
    /// Returns ValueNone if the two boxes do not overlap.
    static member inline intersection (a:BBox) (b:BBox) = 
        a.Intersection(b)


    /// Finds min and max values for x, y and z.
    static member inline create (a:Pnt , b:Pnt ) =
        // sort min and max values ( not using allocating tuples for swapping).
        let mutable minX = a.X
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X
        let mutable minY = a.Y
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        let mutable minZ = a.Z
        let maxZ = if b.Z > minZ then b.Z else minZ <- b.Z ;  a.Z
        BBox(minX,minY,minZ,maxX,maxY,maxZ)


    /// Finds min and max values for x, y and z.
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

    /// Does not verify the order of min and max values.
    static member inline createUnchecked (minX,minY,minZ,maxX,maxY,maxZ) =
        BBox(minX,minY,minZ,maxX,maxY,maxZ)

    /// Returns the volume of the Bounding Box.
    static member inline volume (b:BBox) =
        b.SizeX*b.SizeY*b.SizeZ

    /// Returns the 2D part of this Bounding Box as a Bounding Rectangle.
    static member inline toRect (b:BBox) =
        BRect.createUnchecked(b.MinX,b.MinY,b.MaxX,b.MaxY)
