namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid


#nowarn "44" // for hidden constructors via Obsolete Attribute


/// An immutable 3D-bounding-box.
/// This implementation guarantees the box to be always valid.
/// That means the Min X, Y and Z values are always smaller or equal than the respective Max values.
/// The X, Y and Z axes are also called Width, Depth and Height3D.
///
///   Z-Axis       Y-Axis (Depth)
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
///   +---------------+----> X-Axis (Width)
///   0 MinPt         1
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type BBox =
    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar
    [<DataMember>] val MinX : float
    [<DataMember>] val MinY : float
    [<DataMember>] val MinZ : float
    [<DataMember>] val MaxX : float
    [<DataMember>] val MaxY : float
    [<DataMember>] val MaxZ : float

    /// Unsafe internal constructor, public only for inlining.
    [<Obsolete("Unsafe internal constructor, but must be public for inlining.") >]
    new (minX, minY, minZ, maxX, maxY, maxZ) =
        {MinX = minX
         MinY = minY
         MinZ = minZ
         MaxX = maxX
         MaxY = maxY
         MaxZ = maxZ
         }

    /// Nicely formatted string representation of the BoundingBox, including its size.
    override b.ToString() =
        sprintf "Euclid.BBox; Size: x=%s | y=%s | z=%s (at X=%s | Y=%s | Z=%s)"
            (Format.float (b.MaxX - b.MinX)) (Format.float (b.MaxY - b.MinY)) (Format.float (b.MaxZ - b.MinZ))
            (Format.float b.MinX) (Format.float b.MinY) (Format.float b.MinZ)

    /// Format BoundingBox into string with nice floating point number formatting of size and position.
    /// But without full type name as in bbox.ToString()
    member b.AsString =
        sprintf "Size: x=%s | y=%s | z=%s (at X=%s | Y=%s | Z=%s)"
            (Format.float (b.MaxX - b.MinX)) (Format.float (b.MaxY - b.MinY)) (Format.float (b.MaxZ - b.MinZ))
            (Format.float b.MinX) (Format.float b.MinY) (Format.float b.MinZ)

    /// The point where X, Y and Z are the minimum values.
    member inline b.MinPnt = Pnt(b.MinX, b.MinY, b.MinZ)

    /// The point where X, Y and Z are the maximum values.
    member inline b.MaxPnt = Pnt(b.MaxX, b.MaxY, b.MinZ)

    /// The size in X direction, same as member box.SizeX.
    member inline b.Width = b.MaxX - b.MinX
    /// The size in X direction, same as member box.Width.
    member inline b.SizeX = b.MaxX - b.MinX

    /// The size in Y direction, same as member box.SizeY.
    member inline b.Depth = b.MaxY - b.MinY
    /// The size in Y direction, same as member box.Depth.
    member inline b.SizeY = b.MaxY - b.MinY

    /// The size in Z direction, same as member box.SizeZ.
    member inline b.Height3D = b.MaxZ - b.MinZ
    /// The size in Z direction, same as member box.Height3D.
    member inline b.SizeZ = b.MaxZ - b.MinZ

    /// The diagonal 3D vector of this 3D-bounding-box. From MinPnt to MaxPnt.
    member inline b.Diagonal = Vec(b.MaxX - b.MinX, b.MaxY - b.MinY, b.MaxZ - b.MinZ)

    /// The center of this 3D-bounding-box.
    member inline b.Center = Pnt( (b.MaxX + b.MinX)*0.5, (b.MaxY + b.MinY)*0.5, (b.MaxZ + b.MinZ)*0.5)



    /// Returns a 3D-bounding-box expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.Expand(dist) : BBox =
        let n = BBox(   b.MinX-dist, b.MinY-dist, b.MinZ-dist,
                        b.MaxX+dist, b.MaxY+dist, b.MaxZ+dist)
        if dist<0. &&  (n.MinX > n.MaxX ||  n.MinY > n.MaxX ||  n.MinZ > n.MaxZ) then
            EuclidException.Raisef "Euclid.BBox.Expand(dist): Negative distance %g cause an underflow, on %s" dist b.AsString
        n

    /// Returns a 3D-bounding-box expanded by a distance for X, Y and Z-axis each.
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.Expand(xDist, yDist, zDist) : BBox =
        let n = BBox(   b.MinX-xDist, b.MinY-yDist, b.MinZ-zDist,
                        b.MaxX+xDist, b.MaxY+yDist, b.MaxZ+zDist)
        if n.MinX > n.MaxX ||  n.MinY > n.MaxX ||  n.MinZ > n.MaxZ then
            EuclidException.Raisef "Euclid.BBox.Expand(x, y, z): Negative distance(s) X %g Y: %g and Z:%g cause an underflow, on %s" xDist yDist zDist b.AsString
        n

    /// Returns a 3D-bounding-box expanded by a distance for X, Y and Z-axis each.
    /// If expansion is negative it shrinks the Box. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    member inline b.ExpandSave(xDist, yDist, zDist) : BBox =
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

    /// Returns a 3D-bounding-box expanded by a distance.
    /// If expansion is negative it shrinks the Box. It also makes sure that there is no underflow.
    /// When the negative expansion is bigger than the size, Min and Max values will be both in the middle from where they were before.
    member inline b.ExpandSave(dist) : BBox =
        b.ExpandSave(dist, dist, dist)

    /// Returns a 3D-bounding-box expanded only in X direction by different distance for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.ExpandXaxis(startDist, endDist) : BBox =
        let n = BBox(b.MinX-startDist, b.MinY, b.MinZ, b.MaxX+endDist, b.MaxY, b.MaxZ)
        if n.MinX > n.MaxX then
            EuclidException.Raisef "Euclid.BBox.ExpandXaxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist b.AsString
        n

    /// Returns a 3D-bounding-box expanded only in Y direction by different distance for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.ExpandYaxis(startDist, endDist) : BBox =
        let n = BBox(b.MinX, b.MinY-startDist, b.MinZ, b.MaxX, b.MaxY+endDist, b.MaxZ)
        if n.MinY > n.MaxY then
            EuclidException.Raisef "Euclid.BBox.ExpandYaxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist b.AsString
        n

    /// Returns a 3D-bounding-box expanded only in Z direction by different distance for start(minZ) and end (maxZ).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.ExpandZaxis(startDist, endDist) : BBox =
        let n = BBox(b.MinX, b.MinY, b.MinZ-startDist, b.MaxX, b.MaxY, b.MaxZ+endDist)
        if n.MinZ > n.MaxZ then
            EuclidException.Raisef "Euclid.BBox.ExpandYaxis: Negative distances for start(%g) and end (%g) cause an underflow, on %s" startDist endDist b.AsString
        n

    /// Returns true if the two 3D-bounding-boxes do overlap or touch.
    /// Also returns true if one box is completely inside the other.
    /// Also returns true if one box is completely surrounding the other.
    member inline b.OverlapsWith (a:BBox) =
        not (  b.MinX > a.MaxX
            || a.MinX > b.MaxX
            || a.MinY > b.MaxY
            || b.MinY > a.MaxY
            || a.MinZ > b.MaxZ
            || b.MinZ > a.MaxZ
            )

    /// Returns true if the two 3D-bounding-boxes do overlap more than a given tolerance distance.
    /// Returns false if the two 3D-bounding-boxes are just touching.
    /// Also returns true if one box is completely inside the other.
    /// Also returns true if one box is completely surrounding the other.
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

    /// Returns true if this 3D-bounding-box is inside or exactly on the other bounding Box.
    member inline b.Contains (o:BBox) =
        b.Contains(o.MinPnt) && b.Contains(o.MaxPnt)

    /// Test if 3D-bounding-boxes are only touching each other from the Outside within a given tolerance.
    member b.IsTouching (a:BBox, tol) =
        let xOverlap = not (b.MinX > a.MaxX + tol || a.MinX > b.MaxX + tol)
        let yOverlap = not (a.MinY > b.MaxY + tol || b.MinY > a.MaxY + tol)
        let zOverlap = not (a.MinZ > b.MaxZ + tol || b.MinZ > a.MaxZ + tol)
        let xTouch   = abs(b.MinX - a.MaxX)  < tol || abs(a.MinX - b.MaxX) < tol
        let yTouch   = abs(a.MinY - b.MaxY)  < tol || abs(b.MinY - a.MaxY) < tol
        let zTouch   = abs(a.MinZ - b.MaxZ)  < tol || abs(b.MinZ - a.MaxZ) < tol
        xOverlap && yOverlap && zTouch   ||
        xOverlap && yTouch   && zOverlap ||
        xTouch   && yOverlap && zOverlap


    /// Evaluate a X, Y and Z parameter of this 3D-bounding-box.
    /// 0.0, 0.0, 0.0 returns the MinPnt.
    /// 1.0, 1.0, 1.0 returns the MaxPnt.
    member inline b.EvaluateAt (xParameter, yParameter, zParameter) =
        Pnt(b.MinX + (b.MaxX-b.MinX) * xParameter,
            b.MinY + (b.MaxY-b.MinY) * yParameter,
            b.MinZ + (b.MaxZ-b.MinZ) * zParameter)

    /// Returns the volume of this 3D-bounding-box.
    member inline b.Volume =
        b.SizeX * b.SizeY * b.SizeZ

    /// Returns the longest edge of the Box.
    member inline b.LongestEdge =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        let z = b.MaxZ - b.MinZ
        max (max x y) z

    /// Returns the shortest edge of the Box.
    member inline b.ShortestEdge =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        let z = b.MaxZ - b.MinZ
        min (min x y) z


    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsPoint.
    member inline b.IsZero =
        isTooTiny (b.MaxX - b.MinX) &&
        isTooTiny (b.MaxY - b.MinY) &&
        isTooTiny (b.MaxZ - b.MinZ)

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsZero.
    member inline b.IsPoint =
        b.IsZero


    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, 2 or 3.
    member inline b.CountZeroSides =
        countTooTiny    (b.MaxX - b.MinX)
        +  countTooTiny (b.MaxY - b.MinY)
        +  countTooTiny (b.MaxZ - b.MinZ)


    /// Tests if two of the X, Y and Z axis is smaller than the zeroLength tolerance.
    member inline b.IsLine =
        b.CountZeroSides = 2

    /// Tests if one of the X, Y and Z axis is smaller than the zeroLength tolerance.
    member inline b.IsFlat =
        b.CountZeroSides = 1

    /// Tests if no sides of the X, Y and Z axis is smaller than the zeroLength tolerance.
    /// Same as .HasVolume
    member inline b.IsValid =
        b.CountZeroSides = 0

    /// Tests if none of the X, Y and Z axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasVolume =
        b.CountZeroSides = 0


    /// Returns the 2D part of this 3D-bounding-box as a bounding rectangle (BRect).
    member inline b.asBRect =
        BRect.createUnchecked(b.MinX, b.MinY, b.MaxX, b.MaxY)

    /// Returns the 2D part of this 3D-bounding-box as a bounding rectangle (BRect).
    [<Obsolete("Use .asBRect instead")>]
    member inline b.asRect =
        b.asBRect


    /// Returns a 3D-bounding-box that contains both input 3D-bounding-box.
    member inline b.Union (a:BBox) =
        BBox   (min b.MinX a.MinX, min b.MinY a.MinY, min b.MinZ a.MinZ,
                max b.MaxX a.MaxX, max b.MaxY a.MaxY, max b.MaxZ a.MaxZ)

    /// Returns a bounding 3D-bounding-box that contains the input 3D-bounding-box and the point.
    member inline b.Union (p:Pnt) =
        BBox   (min b.MinX p.X, min b.MinY p.Y, min b.MinZ p.Z,
                max b.MaxX p.X, max b.MaxY p.Y, max b.MaxZ p.Z)

    /// Returns the intersection of two 3D-bounding-boxes.
    /// The returned Box is the volume inside both input boxes.
    /// Returns ValueNone if the two boxes do not overlap.
    member inline b.Intersection (a:BBox) =
        let mutable minX = max a.MinX b.MinX
        let mutable minY = max a.MinY b.MinY
        let mutable minZ = max a.MinZ b.MinZ
        let mutable maxX = min a.MaxX b.MaxX
        let mutable maxY = min a.MaxY b.MaxY
        let mutable maxZ = min a.MaxZ b.MaxZ
        if minX <= maxX && minY <= maxY && minZ <= maxZ then
            ValueSome (BBox(minX, minY, minZ, maxX, maxY, maxZ))
        else
            ValueNone


    //-------------------------------------------------------------------
    //------------------------static members---------------------------
    //-------------------------------------------------------------------



    /// Returns the volume of the a 3D-bounding-box.
    static member inline volume (b:BBox) =
        b.SizeX * b.SizeY * b.SizeZ

    /// Returns the 2D part of a 3D-bounding-box as a bounding rectangle.
    static member inline toBRect (b:BBox) =
        BRect.createUnchecked(b.MinX, b.MinY, b.MaxX, b.MaxY)



    /// Checks if two 3D-bounding-boxes are equal within tolerance.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:BBox) (b:BBox) =
        abs(a.MinX-b.MinX) <= tol &&
        abs(a.MinY-b.MinY) <= tol &&
        abs(a.MinZ-b.MinZ) <= tol &&
        abs(a.MaxX-b.MaxX) <= tol &&
        abs(a.MaxY-b.MaxY) <= tol &&
        abs(a.MaxZ-b.MaxZ) <= tol


    /// Check if two 3D-bounding-boxes are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two 3D-bounding-boxes are not exactly equal.
    static member notEquals (tol:float) (a:BBox) (b:BBox) =
        abs(a.MinX-b.MinX) > tol ||
        abs(a.MinY-b.MinY) > tol ||
        abs(a.MinZ-b.MinZ) > tol ||
        abs(a.MaxX-b.MaxX) > tol ||
        abs(a.MaxY-b.MaxY) > tol ||
        abs(a.MaxZ-b.MaxZ) > tol

    /// Returns a 3D-bounding-box expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (b:BBox) =
        b.Expand dist

    /// Returns a 3D-bounding-box expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandSave dist (b:BBox) =
        b.Expand dist

    /// Returns a 3D-bounding-box expanded only in X direction by different distance for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandXaxis startDist endDist (b:BBox) =
        b.ExpandXaxis(startDist, endDist)


    /// Returns a 3D-bounding-box expanded only in Y direction by different distance for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandYaxis startDist endDist (b:BBox) =
        b.ExpandYaxis(startDist, endDist)

    /// Returns a 3D-bounding-box expanded only in Z direction by different distance for start(minZ) and end (maxZ).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandZaxis startDist endDist (b:BBox) =
        b.ExpandZaxis(startDist, endDist)

    /// Returns a 3D-bounding-box moved by a vector.
    static member move (v:Vec) (b:BBox) =
        BBox(b.MinX+v.X, b.MinY+v.Y, b.MinZ+v.Z, b.MaxX+v.X, b.MaxY+v.Y, b.MaxZ+v.Z)


    /// Returns a new 3D-bounding-box moved in X-axis direction.
    static member moveX (translation:float) (b:BBox) =
        BBox(b.MinX+translation, b.MinY, b.MinZ, b.MaxX+translation, b.MaxY, b.MaxZ)

    /// Returns a new 3D-bounding-box moved in Y-axis direction.
    static member moveY (translation:float) (b:BBox) =
        BBox(b.MinX, b.MinY+translation, b.MinZ, b.MaxX, b.MaxY+translation, b.MaxZ)

    /// Returns a new 3D-bounding-box moved in Z-axis direction.
    static member moveZ (translation:float) (b:BBox) =
        BBox(b.MinX, b.MinY, b.MinZ+translation, b.MaxX, b.MaxY, b.MaxZ+translation)


    /// Returns true if the two a 3D-bounding-boxes do overlap or touch exactly.
    /// Also returns true if one box is completely inside the other.
    /// Also returns true if one box is completely surrounding the other.
    static member inline doOverlap(a:BBox) (b:BBox) =
        b.OverlapsWith(a)

    /// Returns true if the two a 3D-bounding-boxes do overlap more than a given tolerance distance.
    /// Returns false if the two a 3D-bounding-boxes are just touching.
    /// Also returns true if one box is completely inside the other.
    /// Also returns true if one box is completely surrounding the other.
    static member inline doOverlapMoreThan tol (a:BBox) (b:BBox) =
        b.OverlapsWith(a, tol)

    /// Returns true if the a 3D-bounding-box is inside or exactly on the other bounding Box.
    /// Argument order matters!
    static member inline contains (boxInside:BBox) (surroundingBox:BBox) =
        surroundingBox.Contains(boxInside)

    /// Returns true if the point is inside or on the a 3D-bounding-box.
    static member inline containsPnt (pt:Pnt) (box:BBox) =
        box.Contains(pt)

    /// Returns a 3D-bounding-box that contains both input a 3D-bounding-box.
    static member inline union (a:BBox) (b:BBox) =
        BBox   (min b.MinX a.MinX, min b.MinY a.MinY, min b.MinZ a.MinZ,
                max b.MaxX a.MaxX, max b.MaxY a.MaxY, max b.MaxZ a.MaxZ)

    /// Returns a bounding a 3D-bounding-box that contains the input a 3D-bounding-box and the point.
    static member inline unionPt (p:Pnt) (b:BBox) =
        BBox   (min b.MinX p.X, min b.MinY p.Y, min b.MinZ p.Z,
                max b.MaxX p.X, max b.MaxY p.Y, max b.MaxZ p.Z)

    /// Returns the intersection of two a 3D-bounding-boxes.
    /// The returned Box is the volume inside both input boxes.
    /// Returns ValueNone if the two boxes do not overlap.
    static member inline intersection (a:BBox) (b:BBox) =
        a.Intersection(b)


    /// Finds min and max values for x, y and z.
    static member inline create (a:Pnt, b:Pnt) =
        // sort min and max values (not using allocating tuples for swapping).
        let mutable minX = a.X
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X
        let mutable minY = a.Y
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        let mutable minZ = a.Z
        let maxZ = if b.Z > minZ then b.Z else minZ <- b.Z ;  a.Z
        BBox(minX, minY, minZ, maxX, maxY, maxZ)


    /// Finds min and max values for x, y and z.
    /// Creates a 3D-bounding-box from the points.
    static member inline createFromSeq (ps:seq<Pnt> ) =
        if Seq.isEmpty ps then raise <| EuclidException("Euclid.BBox.createFromSeq: seq<Pt> is empty.")
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
        BBox(minX, minY, minZ, maxX, maxY, maxZ)

    /// Finds min and max values for x, y and z.
    /// Creates a 3D-bounding-box from the points.
    static member inline createFromIList (ps:Collections.Generic.IList<Pnt> ) =
        if Seq.isEmpty ps then raise <| EuclidException("Euclid.BBox.createFromIList: IList<Pt> is empty.")
        let mutable minX = Double.MaxValue
        let mutable minY = Double.MaxValue
        let mutable minZ = Double.MaxValue
        let mutable maxX = Double.MinValue
        let mutable maxY = Double.MinValue
        let mutable maxZ = Double.MinValue
        for i=0 to ps.Count-1 do
            let p = ps.[i]
            minX <- min minX p.X
            minY <- min minY p.Y
            minZ <- min minZ p.Z
            maxX <- max maxX p.X
            maxY <- max maxY p.Y
            maxZ <- max maxZ p.Z
        BBox(minX, minY, minZ, maxX, maxY, maxZ)


    /// Creates a 3D-bounding-box from a center point and the total X, Y and Z size.
    static member inline createFromCenter (center:Pnt, sizeX, sizeY, sizeZ) =
        if isNegative(sizeX) then EuclidException.Raisef "Euclid.BBox.createFromCenter sizeX is negative: %g, sizeY is: %g, sizeZ is: %g, center: %O"  sizeX sizeY sizeZ center.AsString
        if isNegative(sizeY) then EuclidException.Raisef "Euclid.BBox.createFromCenter sizeY is negative: %g, sizeX is: %g, sizeZ is: %g, center: %O"  sizeY sizeX sizeZ center.AsString
        if isNegative(sizeZ) then EuclidException.Raisef "Euclid.BBox.createFromCenter sizeZ is negative: %g, sizeX is: %g, sizeY is: %g, center: %O"  sizeZ sizeX sizeY center.AsString
        let minX = center.X - sizeX*0.5
        let minY = center.Y - sizeY*0.5
        let maxX = center.X + sizeX*0.5
        let maxY = center.Y + sizeY*0.5
        let minZ = center.Z - sizeZ*0.5
        let maxZ = center.Z + sizeZ*0.5
        BBox(minX, minY, minZ, maxX, maxY, maxZ)

    /// Does not verify the order of min and max values.
    static member inline createUnchecked (minX, minY, minZ, maxX, maxY, maxZ) =
        BBox(minX, minY, minZ, maxX, maxY, maxZ)

    static member inline createFromLine (l:Line3D) =
        let minX = min l.FromX l.ToX
        let maxX = max l.FromX l.ToX
        let minY = min l.FromY l.ToY
        let maxY = max l.FromY l.ToY
        let minZ = min l.FromZ l.ToZ
        let maxZ = max l.FromZ l.ToZ
        BBox(minX, minY, minZ, maxX, maxY, maxZ)

    static member inline createFromBRect minZ maxZ (r : BRect) =
        if minZ>maxZ then raise <| EuclidException $"Euclid.BBox.createFromBRect: minZ > maxZ: {minZ} > {maxZ}"
        BBox(r.MinX, r.MinY, minZ, r.MaxX, r.MaxY, maxZ)


    /// Returns point 0 of this 3D-bounding-box, same as member box.MinPnt.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member inline b.Pt0 = Pnt(b.MinX, b.MinY, b.MinZ)

    /// Returns point 1 of this 3D-bounding-box.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member inline b.Pt1 = Pnt(b.MaxX, b.MinY, b.MinZ)

    /// Returns point 2 of this 3D-bounding-box.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member inline b.Pt2 = Pnt(b.MaxX, b.MaxY, b.MinZ)

    /// Returns point 3 of this 3D-bounding-box.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member inline b.Pt3 = Pnt(b.MinX, b.MaxY, b.MinZ)

    /// Returns point 4 of this 3D-bounding-box.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member inline b.Pt4 = Pnt(b.MinX, b.MinY, b.MaxZ)

    /// Returns point 5 of this 3D-bounding-box.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member inline b.Pt5 = Pnt(b.MaxX, b.MinY, b.MaxZ)

    /// Returns point 6 of this 3D-bounding-box.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member inline b.Pt6 = Pnt(b.MaxX, b.MaxY, b.MaxZ)

    /// Returns point 7 of this 3D-bounding-box.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member inline b.Pt7 = Pnt(b.MinX, b.MaxY, b.MaxZ)




    /// Returns the bottom corners of this 3D-bounding-box in Counter-Clockwise order, starting at MinPt.
    /// Then the top corners staring above MinPt. Returns an array of 8 Points.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member b.Points = [|b.Pt0; b.Pt1; b.Pt2; b.Pt3; b.Pt4; b.Pt5; b.Pt6; b.Pt7|]

    /// Returns the bottom of the Box as a Counter-Clockwise array of 4 Points.
    /// Starting at MinPt. Point 0, 1, 2 and 3.
    /// Last and first point are NOT the same.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member b.BottomPoints = [|b.Pt0; b.Pt1; b.Pt2; b.Pt3|]


    /// Returns the bottom of the Box as a Counter-Clockwise array of 5 Points, starting at MinPt.
    /// Starting at MinPt. Point 0, 1, 2, 3 and again 0.
    /// Last and first point are the same.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member b.BottomPointsLooped = [|b.Pt0; b.Pt1; b.Pt2; b.Pt3; b.Pt0|]

    /// Returns the bottom of the Box as a Counter-Clockwise array of 4 Points.
    /// Staring at point 4 then 5, 6 and 7.
    /// Last and first point are NOT the same.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member b.TopPoints = [|b.Pt4; b.Pt5; b.Pt6; b.Pt7|]

    /// Returns the bottom of the Box as a Counter-Clockwise array of 5 Points.
    /// Starting point 4 then 5, 6, 7 and again 4.
    /// Last and first point are the same.
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member b.TopPointsLooped = [|b.Pt4; b.Pt5; b.Pt6; b.Pt7; b.Pt4|]


    /// Returns the X aligned Edge from point 0 to 1.
    member inline b.Edge01 = Line3D(b.Pt0, b.Pt1)

    /// Returns the Y aligned Edge from point 1 to 2.
    member inline b.Edge12 = Line3D(b.Pt1, b.Pt2)

    /// Returns the X aligned Edge from point 3 to 2.
    member inline b.Edge32 = Line3D(b.Pt3, b.Pt2)

    /// Returns the Y aligned Edge from point 0 to 3.
    member inline b.Edge03 = Line3D(b.Pt0, b.Pt3)

    /// Returns the Z aligned Edge from point 0 to 4.
    member inline b.Edge04 = Line3D(b.Pt0, b.Pt4)

    /// Returns the Z aligned Edge from point 1 to 5.
    member inline b.Edge15 = Line3D(b.Pt1, b.Pt5)

    /// Returns the Z aligned Edge from point 2 to 6.
    member inline b.Edge26 = Line3D(b.Pt2, b.Pt6)

    /// Returns the Z aligned Edge from point 3 to 7.
    member inline b.Edge37 = Line3D(b.Pt3, b.Pt7)

    /// Returns the X aligned Edge from point 4 to 5.
    member inline b.Edge45 = Line3D(b.Pt4, b.Pt5)

    /// Returns the Y aligned Edge from point 5 to 6.
    member inline b.Edge56 = Line3D(b.Pt5, b.Pt6)

    /// Returns the X aligned Edge from point 7 to 6.
    member inline b.Edge76= Line3D(b.Pt7, b.Pt6)

    /// Returns the Y aligned Edge from point 4 to 7.
    member inline b.Edge47 = Line3D(b.Pt4, b.Pt7)

    /// Returns the 12 Edges of this 3D-bounding-box as an array of 12 Lines.
    /// Pair is this order:
    /// 0-1, 1-2, 3-2, 0-3, 0-4, 1-5, 2-6, 3-7, 4-5, 5-6, 7-6, 4-7
    ///
    ///   Z-Axis       Y-Axis (Depth)
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
    ///   +---------------+----> X-Axis (Width)
    ///   0 MinPt         1
    member b.Edges =
        [| b.Edge01; b.Edge12; b.Edge32; b.Edge03; // bottom face
           b.Edge04; b.Edge15; b.Edge26; b.Edge37; // vertical edges
           b.Edge45; b.Edge56; b.Edge76; b.Edge47 |] // top face


    /// Scales the 3D bounding box by a given factor.
    /// Scale center is World Origin 0,0,0
    static member inline scale (factor:float) (b:BBox) : BBox =
        BBox(
            b.MinX * factor,
            b.MinY * factor,
            b.MinZ * factor,
            b.MaxX * factor,
            b.MaxY * factor,
            b.MaxZ * factor
        )