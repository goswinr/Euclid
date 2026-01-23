namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid
open EuclidErrors




/// <summary>
/// An immutable 3D bounding box.
/// This implementation guarantees the box to always be valid.
/// That means the Min X, Y, and Z values are always smaller or equal to the respective Max values.
/// The X, Y, and Z axes are also called Width, Depth, and Height3D.
/// <code>
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
/// </code>
/// </summary>
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
    [<Obsolete("This is not Obsolete, but an unsafe internal constructor. the input is not verified, so it might create invalid geometry. It is exposed as a public member so that it can be inlined.") >]
    new (minX, minY, minZ, maxX, maxY, maxZ) =
        {MinX = minX
         MinY = minY
         MinZ = minZ
         MaxX = maxX
         MaxY = maxY
         MaxZ = maxZ
         }

    /// Creates a 3D bounding box from six coordinate values.
    /// Does not verify that min values are less than or equal to max values.
    static member inline createUnchecked (minX, minY, minZ, maxX, maxY, maxZ) : BBox =
        #nowarn "44"
        BBox(minX, minY, minZ, maxX, maxY, maxZ)
        #warnon "44" // re-enable warning for obsolete usage


    /// Nicely formatted string representation of the bounding box, including its size.
    override b.ToString() =
        let sizeX = Format.float (b.MaxX - b.MinX)
        let sizeY = Format.float (b.MaxY - b.MinY)
        let sizeZ = Format.float (b.MaxZ - b.MinZ)
        let minX = Format.float b.MinX
        let minY = Format.float b.MinY
        let minZ = Format.float b.MinZ
        $"Euclid.BBox: Size: x=%s{sizeX}|y=%s{sizeY}|z=%s{sizeZ} (at X=%s{minX}|Y=%s{minY}|Z=%s{minZ})"

    /// Format bounding box into string with nice floating point number formatting of size and position.
    /// But without full type name as in bbox.ToString()
    member b.AsString : string =
        let sizeX = Format.float (b.MaxX - b.MinX)
        let sizeY = Format.float (b.MaxY - b.MinY)
        let sizeZ = Format.float (b.MaxZ - b.MinZ)
        let minX = Format.float b.MinX
        let minY = Format.float b.MinY
        let minZ = Format.float b.MinZ
        $"Size: x=%s{sizeX}|y=%s{sizeY}|z=%s{sizeZ} (at X=%s{minX}|Y=%s{minY}|Z=%s{minZ})"

    /// Format bounding box into an F# code string that can be used to recreate the bounding box.
    member b.AsFSharpCode : string =
        $"BBox.createUnchecked({b.MinX}, {b.MinY}, {b.MinZ}, {b.MaxX}, {b.MaxY}, {b.MaxZ})"

    /// The point where X, Y, and Z are the minimum values.
    member inline b.MinPnt : Pnt =
        Pnt(b.MinX, b.MinY, b.MinZ)

    /// The point where X, Y, and Z are the maximum values.
    member inline b.MaxPnt : Pnt =
        Pnt(b.MaxX, b.MaxY, b.MaxZ)


    /// The size in X direction, often also called Width.
    member inline b.SizeX : float =
        b.MaxX - b.MinX

    /// The size in Y direction.
    member inline b.SizeY : float =
        b.MaxY - b.MinY

    /// The size in Z direction, also called Height.
    member inline b.SizeZ : float =
        b.MaxZ - b.MinZ

    /// The diagonal 3D vector of this 3D bounding box. From MinPnt to MaxPnt.
    member inline b.Diagonal : Vec =
        Vec(b.MaxX - b.MinX, b.MaxY - b.MinY, b.MaxZ - b.MinZ)

    /// The center of this 3D bounding box.
    member inline b.Center : Pnt =
        Pnt( (b.MaxX + b.MinX)*0.5, (b.MaxY + b.MinY)*0.5, (b.MaxZ + b.MinZ)*0.5)



    /// Returns a 3D bounding box expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.Expand(dist) : BBox =
        let n = BBox.createUnchecked(   b.MinX-dist, b.MinY-dist, b.MinZ-dist,
                                        b.MaxX+dist, b.MaxY+dist, b.MaxZ+dist)
        if dist<0. &&  (n.MinX > n.MaxX ||  n.MinY > n.MaxY ||  n.MinZ > n.MaxZ) then
            fail $"BBox.Expand(dist): Negative distance {dist} causes an underflow, on {b.AsString}"
        n

    /// Returns a 3D bounding box expanded by a distance for X, Y, and Z-axis each.
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.Expand(xDist, yDist, zDist) : BBox =
        let n = BBox.createUnchecked(   b.MinX-xDist, b.MinY-yDist, b.MinZ-zDist,
                                        b.MaxX+xDist, b.MaxY+yDist, b.MaxZ+zDist)
        if n.MinX > n.MaxX ||  n.MinY > n.MaxY ||  n.MinZ > n.MaxZ then
            fail $"BBox.Expand(x, y, z): Negative distance(s) X: {xDist} Y: {yDist} and Z: {zDist} cause an underflow, on {b.AsString}"
        n

    /// Returns a 3D bounding box expanded by a distance for X, Y, and Z-axis each.
    /// If expansion is negative it shrinks the Box without causing underflow.
    /// When the negative expansion is bigger than the size on any axis, both Min and Max values
    /// on that axis will be set to the midpoint of their original values (the box collapses to a plane or line on that axis).
    member inline b.ExpandSafe(xDist, yDist, zDist) : BBox =
        let mutable minXCh = b.MinX - xDist
        let mutable maxXCh = b.MaxX + xDist
        if minXCh > maxXCh then  // Underflow! Set both to the same mid point
            let mid = b.MinX + (b.MaxX-b.MinX) * 0.5
            minXCh <- mid
            maxXCh <- mid
        // expand Y:
        let mutable minYCh = b.MinY - yDist
        let mutable maxYCh = b.MaxY + yDist
        if minYCh > maxYCh then  // Underflow! Set both to the same mid point
            let mid = b.MinY + (b.MaxY-b.MinY) * 0.5
            minYCh <- mid
            maxYCh <- mid
        // expand Z:
        let mutable minZCh = b.MinZ - zDist
        let mutable maxZCh = b.MaxZ + zDist
        if minZCh > maxZCh then  // Underflow! Set both to the same mid point
            let mid = b.MinZ + (b.MaxZ-b.MinZ) * 0.5
            minZCh <- mid
            maxZCh <- mid
        BBox.createUnchecked(minXCh, minYCh, minZCh, maxXCh, maxYCh, maxZCh)

    /// Returns a 3D bounding box expanded by a distance.
    /// If expansion is negative it shrinks the Box without causing underflow.
    /// When the negative expansion is bigger than the size on any axis, both Min and Max values
    /// on that axis will be set to the midpoint of their original values (the box collapses to a plane or line on that axis).
    member inline b.ExpandSafe(dist) : BBox =
        b.ExpandSafe(dist, dist, dist)


    /// Returns a 3D bounding box expanded only in X direction by different distances for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.ExpandXaxis(startDist, endDist) : BBox =
        let n = BBox.createUnchecked(b.MinX-startDist, b.MinY, b.MinZ, b.MaxX+endDist, b.MaxY, b.MaxZ)
        if n.MinX > n.MaxX then
            fail $"BBox.ExpandXaxis: Negative distances for start({startDist}) and end ({endDist}) cause an underflow, on {b.AsString}"
        n

    /// Returns a 3D bounding box expanded only in Y direction by different distances for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.ExpandYaxis(startDist, endDist) : BBox =
        let n = BBox.createUnchecked(b.MinX, b.MinY-startDist, b.MinZ, b.MaxX, b.MaxY+endDist, b.MaxZ)
        if n.MinY > n.MaxY then
            fail $"BBox.ExpandYaxis: Negative distances for start({startDist}) and end({endDist}) cause an underflow, on {b.AsString}"
        n

    /// Returns a 3D bounding box expanded only in Z direction by different distances for start(minZ) and end (maxZ).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.ExpandZaxis(startDist, endDist) : BBox =
        let n = BBox.createUnchecked(b.MinX, b.MinY, b.MinZ-startDist, b.MaxX, b.MaxY, b.MaxZ+endDist)
        if n.MinZ > n.MaxZ then
            fail $"BBox.ExpandZaxis: Negative distances for start({startDist}) and end({endDist}) cause an underflow, on {b.AsString}"
        n

    /// Returns TRUE if the two 3D bounding boxes do overlap or touch.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    member inline b.OverlapsWith (a:BBox) : bool =
        not (  b.MinX > a.MaxX
            || a.MinX > b.MaxX
            || a.MinY > b.MaxY
            || b.MinY > a.MaxY
            || a.MinZ > b.MaxZ
            || b.MinZ > a.MaxZ
            )

    /// <summary>
    /// Determines whether this bounding box overlaps with another bounding box within a specified tolerance.
    /// </summary>
    /// <param name="a">The bounding box to test for overlap with.</param>
    /// <param name="tol">The tolerance distance used to shrink both boxes before testing overlap. Default is 1e-6.
    /// The tolerance effectively shrinks both boxes by the given amount before testing overlap.</param>
    /// <returns>
    /// Returns TRUE if:
    /// - The boxes overlap with a gap smaller than the tolerance distance
    /// - One box completely surrounds the other
    /// Returns FALSE if:
    /// - The boxes are separated by more than the tolerance distance
    /// - The boxes are just touching (within tolerance)
    /// </returns>
    member inline b.OverlapsWith (a:BBox, [<OPT;DEF(1e-6)>]tol:float) : bool =
        not (  b.MinX > a.MaxX - tol
            || a.MinX > b.MaxX - tol
            || a.MinY > b.MaxY - tol
            || b.MinY > a.MaxY - tol
            || a.MinZ > b.MaxZ - tol
            || b.MinZ > a.MaxZ - tol
            )

    /// Returns TRUE if the point is inside or exactly on the bounding Box.
    member inline b.Contains (p:Pnt) : bool =
        p.X >= b.MinX &&
        p.X <= b.MaxX &&
        p.Y >= b.MinY &&
        p.Y <= b.MaxY &&
        p.Z >= b.MinZ &&
        p.Z <= b.MaxZ

    /// Returns TRUE if this 3D bounding box is inside or exactly on the other bounding Box.
    member inline b.Contains (o:BBox) : bool =
        b.Contains(o.MinPnt) && b.Contains(o.MaxPnt)

    /// <summary>Test if 3D bounding boxes are only touching each other from the Outside within a given tolerance.</summary>
    /// <param name="a">Other 3D bounding box to test against.</param>
    /// <param name="tol">Optional. A tolerance for touching test. Default is 1e-6.</param>
    /// <returns>TRUE if the two 3D bounding boxes are touching each other within the given tolerance.
    /// FALSE if the two 3D bounding boxes are overlapping or intersecting </returns>
    member b.IsTouching (a:BBox, [<OPT;DEF(1e-6)>]tol:float) : bool =
        let xOverlap = not (b.MinX > a.MaxX + tol || a.MinX > b.MaxX + tol)
        let yOverlap = not (a.MinY > b.MaxY + tol || b.MinY > a.MaxY + tol)
        let zOverlap = not (a.MinZ > b.MaxZ + tol || b.MinZ > a.MaxZ + tol)
        let xTouch   = abs(b.MinX - a.MaxX)  <= tol || abs(a.MinX - b.MaxX) <= tol
        let yTouch   = abs(a.MinY - b.MaxY)  <= tol || abs(b.MinY - a.MaxY) <= tol
        let zTouch   = abs(a.MinZ - b.MaxZ)  <= tol || abs(b.MinZ - a.MaxZ) <= tol
        xOverlap && yOverlap && zTouch   ||
        xOverlap && yTouch   && zOverlap ||
        xTouch   && yOverlap && zOverlap


    /// Evaluate a X, Y and Z parameter of this 3D bounding box.
    /// 0.0, 0.0, 0.0 returns the MinPnt.
    /// 1.0, 1.0, 1.0 returns the MaxPnt.
    member inline b.EvaluateAt (xParameter, yParameter, zParameter) : Pnt =
        Pnt(b.MinX + (b.MaxX-b.MinX) * xParameter,
            b.MinY + (b.MaxY-b.MinY) * yParameter,
            b.MinZ + (b.MaxZ-b.MinZ) * zParameter)

    /// Returns the volume of this 3D bounding box.
    member inline b.Volume : float =
        b.SizeX * b.SizeY * b.SizeZ

    /// Returns the longest edge of the Box.
    member inline b.LongestEdge : float =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        let z = b.MaxZ - b.MinZ
        max (max x y) z

    /// Returns the shortest edge of the Box.
    member inline b.ShortestEdge : float =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        let z = b.MaxZ - b.MinZ
        min (min x y) z


    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsPoint.
    member inline b.IsZero : bool =
        isTooTiny (b.MaxX - b.MinX) &&
        isTooTiny (b.MaxY - b.MinY) &&
        isTooTiny (b.MaxZ - b.MinZ)


    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, 2 or 3.
    member inline b.CountZeroSides : int =
        countTooTinyOrNaN    (b.MaxX - b.MinX)
        +  countTooTinyOrNaN (b.MaxY - b.MinY)
        +  countTooTinyOrNaN (b.MaxZ - b.MinZ)


    /// Tests if two of the X, Y and Z axis is smaller than the zeroLength tolerance.
    member inline b.IsLine : bool =
        b.CountZeroSides = 2

    /// Tests if one of the X, Y and Z axis is smaller than the zeroLength tolerance.
    member inline b.IsFlat : bool =
        b.CountZeroSides = 1

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// This is the same as IsZero.
    member inline b.IsPoint : bool =
        b.IsZero

    /// Tests if no sides of the X, Y and Z axis is smaller than the zeroLength tolerance.
    /// Same as .HasVolume
    member inline b.IsValid : bool =
        b.CountZeroSides = 0

    /// Tests if none of the X, Y and Z axis is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasVolume : bool =
        b.CountZeroSides = 0


    /// Returns the 2D part of this 3D bounding box as a bounding rectangle (BRect).
    member inline b.asBRect : BRect =
        BRect.createUnchecked(b.MinX, b.MinY, b.MaxX, b.MaxY)


    /// Returns a 3D bounding box that contains both input 3D bounding boxes.
    member inline b.Union (a:BBox) : BBox =
        BBox.createUnchecked   (min b.MinX a.MinX, min b.MinY a.MinY, min b.MinZ a.MinZ,
                                max b.MaxX a.MaxX, max b.MaxY a.MaxY, max b.MaxZ a.MaxZ)

    /// Returns a bounding 3D bounding box that contains the input 3D bounding box and the point.
    member inline b.Union (p:Pnt) : BBox =
        BBox.createUnchecked   (min b.MinX p.X, min b.MinY p.Y, min b.MinZ p.Z,
                                max b.MaxX p.X, max b.MaxY p.Y, max b.MaxZ p.Z)


    /// Returns the intersection of two 3D bounding boxes.
    /// The returned BBox is the volume that is inside both input 3D bounding boxes.
    /// Returns ValueNone if the two 3D bounding boxes do not overlap.
    /// Just touching 3D bounding boxes will return ValueSome with a zero volume collapsed BBox.
    member inline b.Intersection (a:BBox) : BBox voption =
        let minX = max a.MinX b.MinX
        let minY = max a.MinY b.MinY
        let minZ = max a.MinZ b.MinZ
        let maxX = min a.MaxX b.MaxX
        let maxY = min a.MaxY b.MaxY
        let maxZ = min a.MaxZ b.MaxZ
        if minX <= maxX && minY <= maxY && minZ <= maxZ then
            ValueSome (BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ))
        else
            ValueNone



    /// Returns point 0 of this 3D bounding box, same as member box.MinPnt.
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
    member inline b.Pt0 : Pnt = Pnt(b.MinX, b.MinY, b.MinZ)

    /// Returns point 1 of this 3D bounding box.
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
    member inline b.Pt1 : Pnt = Pnt(b.MaxX, b.MinY, b.MinZ)

    /// Returns point 2 of this 3D bounding box.
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
    member inline b.Pt2 : Pnt = Pnt(b.MaxX, b.MaxY, b.MinZ)

    /// Returns point 3 of this 3D bounding box.
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
    member inline b.Pt3 : Pnt = Pnt(b.MinX, b.MaxY, b.MinZ)

    /// Returns point 4 of this 3D bounding box.
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
    member inline b.Pt4 : Pnt = Pnt(b.MinX, b.MinY, b.MaxZ)

    /// Returns point 5 of this 3D bounding box.
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
    member inline b.Pt5 : Pnt = Pnt(b.MaxX, b.MinY, b.MaxZ)

    /// Returns point 6 of this 3D bounding box.
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
    member inline b.Pt6 : Pnt = Pnt(b.MaxX, b.MaxY, b.MaxZ)

    /// Returns point 7 of this 3D bounding box.
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
    member inline b.Pt7 : Pnt = Pnt(b.MinX, b.MaxY, b.MaxZ)




    /// Returns the bottom corners of this 3D bounding box in Counter-Clockwise order, starting at MinPt.
    /// Then the top corners starting above MinPt. Returns an array of 8 Points.
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
    member b.Points : Pnt[] = [|b.Pt0; b.Pt1; b.Pt2; b.Pt3; b.Pt4; b.Pt5; b.Pt6; b.Pt7|]

    /// Returns the bottom of the box as a Counter-Clockwise array of 4 Points.
    /// Starting at MinPt. Points 0, 1, 2, and 3.
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
    member b.BottomPoints : Pnt[] = [|b.Pt0; b.Pt1; b.Pt2; b.Pt3|]


    /// Returns the bottom of the box as a Counter-Clockwise array of 5 Points, starting at MinPt.
    /// Points 0, 1, 2, 3, and again 0.
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
    member b.BottomPointsLooped : Pnt[] = [|b.Pt0; b.Pt1; b.Pt2; b.Pt3; b.Pt0|]

    /// Returns the top of the box as a Counter-Clockwise array of 4 Points.
    /// Starting at point 4 then 5, 6, and 7.
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
    member b.TopPoints : Pnt[] = [|b.Pt4; b.Pt5; b.Pt6; b.Pt7|]

    /// Returns the top of the box as a Counter-Clockwise array of 5 Points.
    /// Points 4, 5, 6, 7, and again 4.
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
    member b.TopPointsLooped : Pnt[] = [|b.Pt4; b.Pt5; b.Pt6; b.Pt7; b.Pt4|]


    /// Returns the X-aligned edge from point 0 to 1.
    member inline b.Edge01 : Line3D = Line3D(b.Pt0, b.Pt1)

    /// Returns the Y-aligned edge from point 1 to 2.
    member inline b.Edge12 : Line3D = Line3D(b.Pt1, b.Pt2)

    /// Returns the X-aligned edge from point 3 to 2.
    member inline b.Edge32 : Line3D = Line3D(b.Pt3, b.Pt2)

    /// Returns the Y-aligned edge from point 0 to 3.
    member inline b.Edge03 : Line3D = Line3D(b.Pt0, b.Pt3)

    /// Returns the Z-aligned edge from point 0 to 4.
    member inline b.Edge04 : Line3D = Line3D(b.Pt0, b.Pt4)

    /// Returns the Z-aligned edge from point 1 to 5.
    member inline b.Edge15 : Line3D = Line3D(b.Pt1, b.Pt5)

    /// Returns the Z-aligned edge from point 2 to 6.
    member inline b.Edge26 : Line3D = Line3D(b.Pt2, b.Pt6)

    /// Returns the Z-aligned edge from point 3 to 7.
    member inline b.Edge37 : Line3D = Line3D(b.Pt3, b.Pt7)

    /// Returns the X-aligned edge from point 4 to 5.
    member inline b.Edge45 : Line3D = Line3D(b.Pt4, b.Pt5)

    /// Returns the Y-aligned edge from point 5 to 6.
    member inline b.Edge56 : Line3D = Line3D(b.Pt5, b.Pt6)

    /// Returns the X-aligned edge from point 7 to 6.
    member inline b.Edge76 : Line3D = Line3D(b.Pt7, b.Pt6)

    /// Returns the Y-aligned edge from point 4 to 7.
    member inline b.Edge47 : Line3D = Line3D(b.Pt4, b.Pt7)


    /// Returns the 12 edges of this 3D bounding box as an array of 12 Lines.
    /// Pairs in this order:
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
    member b.Edges : Line3D[] = // this function needs to be defined after the EdgeXX members, because they are inlined.
        [| b.Edge01; b.Edge12; b.Edge32; b.Edge03; // bottom face
           b.Edge04; b.Edge15; b.Edge26; b.Edge37; // vertical edges
           b.Edge45; b.Edge56; b.Edge76; b.Edge47 |] // top face


    // ----------------------------------------------------------------------------------
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
    // --------------------------------------------------------------------------------


    /// Creates a 3D bounding box from two points.
    /// Automatically determines min and max values for each axis,
    /// so the points can be provided in any order.
    static member inline create (a:Pnt, b:Pnt) : BBox =
        // sort min and max values (not using allocating tuples for swapping).
        let mutable minX = a.X
        let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X
        let mutable minY = a.Y
        let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
        let mutable minZ = a.Z
        let maxZ = if b.Z > minZ then b.Z else minZ <- b.Z ;  a.Z
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)





    /// Finds min and max values for x, y and z.
    /// Creates a 3D bounding box from the points.
    static member inline createFromSeq (ps:seq<Pnt> )  : BBox =
        if isNull ps      then failNull "BBox.createFromSeq" "seq<Pnt>"
        if Seq.isEmpty ps then failEmptySeq "BBox.createFromSeq" "seq<Pnt>"
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
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)

    /// Finds min and max values for x, y and z.
    /// Creates a 3D bounding box from the points.
    static member inline createFromIList (ps:Collections.Generic.IList<Pnt> ) =
        if isNull ps    then failNull "BBox.createFromIList" "IList<Pnt>"
        if ps.Count = 0 then failEmptySeq "BBox.createFromIList" "IList<Pnt>"
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
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)


    /// Creates a 3D bounding box from a center point and the total X, Y and Z size.
    static member inline createFromCenter (center:Pnt, sizeX, sizeY, sizeZ)  : BBox =
        if isNegative sizeX then fail $"BBox.createFromCenter sizeX is negative: {sizeX}, sizeY is: {sizeY}, sizeZ is: {sizeZ}, center: {center.AsString}"
        if isNegative sizeY then fail $"BBox.createFromCenter sizeY is negative: {sizeY}, sizeX is: {sizeX}, sizeZ is: {sizeZ}, center: {center.AsString}"
        if isNegative sizeZ then fail $"BBox.createFromCenter sizeZ is negative: {sizeZ}, sizeX is: {sizeX}, sizeY is: {sizeY}, center: {center.AsString}"
        let minX = center.X - sizeX*0.5
        let minY = center.Y - sizeY*0.5
        let maxX = center.X + sizeX*0.5
        let maxY = center.Y + sizeY*0.5
        let minZ = center.Z - sizeZ*0.5
        let maxZ = center.Z + sizeZ*0.5
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)


    /// Creates a 3D bounding box from a 3D line.
    /// The bounding box will be the minimal box containing the line's start and end points.
    static member inline createFromLine (l:Line3D)  : BBox =
        let minX = min l.FromX l.ToX
        let maxX = max l.FromX l.ToX
        let minY = min l.FromY l.ToY
        let maxY = max l.FromY l.ToY
        let minZ = min l.FromZ l.ToZ
        let maxZ = max l.FromZ l.ToZ
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)

    /// Creates a 3D bounding box from a 2D bounding rectangle (BRect) and Z-axis bounds.
    /// The BRect defines the X and Y extents, while minZ and maxZ define the Z extent.
    static member inline createFromBRect minZ maxZ (r : BRect)  : BBox =
        if minZ>maxZ then fail $"BBox.createFromBRect: minZ > maxZ: {minZ} > {maxZ}"
        BBox.createUnchecked(r.MinX, r.MinY, minZ, r.MaxX, r.MaxY, maxZ)


    /// Returns the volume of a 3D bounding box.
    static member inline volume (b:BBox)  : float =
        b.SizeX * b.SizeY * b.SizeZ

    /// Returns the 2D part of a 3D bounding box as a bounding rectangle (BRect).
    /// Projects the BBox onto the XY plane, discarding Z information.
    static member inline toBRect (b:BBox)  : BRect =
        BRect.createUnchecked(b.MinX, b.MinY, b.MaxX, b.MaxY)


    /// Checks if two 3D bounding boxes are equal within tolerance.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:BBox) (b:BBox)  : bool =
        abs(a.MinX-b.MinX) <= tol &&
        abs(a.MinY-b.MinY) <= tol &&
        abs(a.MinZ-b.MinZ) <= tol &&
        abs(a.MaxX-b.MaxX) <= tol &&
        abs(a.MaxY-b.MaxY) <= tol &&
        abs(a.MaxZ-b.MaxZ) <= tol


    /// Check if two 3D bounding boxes are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two 3D bounding boxes are not exactly equal.
    static member notEquals (tol:float) (a:BBox) (b:BBox)  : bool =
        abs(a.MinX-b.MinX) > tol ||
        abs(a.MinY-b.MinY) > tol ||
        abs(a.MinZ-b.MinZ) > tol ||
        abs(a.MaxX-b.MaxX) > tol ||
        abs(a.MaxY-b.MaxY) > tol ||
        abs(a.MaxZ-b.MaxZ) > tol

    /// Returns a 3D bounding box expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (b:BBox)  : BBox =
        b.Expand dist

    /// Returns a 3D bounding box expanded by distance.
    /// If expansion is negative it shrinks the Box without causing underflow.
    /// When the negative expansion is bigger than the size on any axis, both Min and Max values
    /// on that axis will be set to the midpoint of their original values (the box collapses to a plane or line on that axis).
    static member expandSafe dist (b:BBox)  : BBox =
        b.ExpandSafe dist

    /// Returns a 3D bounding box expanded only in X direction by different distances for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandXaxis startDist endDist (b:BBox)  : BBox =
        b.ExpandXaxis(startDist, endDist)

    /// Returns a 3D bounding box expanded only in Y direction by different distances for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandYaxis startDist endDist (b:BBox)  : BBox =
        b.ExpandYaxis(startDist, endDist)

    /// Returns a 3D bounding box expanded only in Z direction by different distances for start(minZ) and end (maxZ).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandZaxis startDist endDist (b:BBox)  : BBox =
        b.ExpandZaxis(startDist, endDist)

    /// Returns the 3D bounding box expanded by a relative factor on all six sides.
    /// Values between 0.0 and 1.0 shrink the box.
    /// Values larger than 1.0 expand the box.
    /// Does check for underflow if factor is negative and raises EuclidException.
    static member expandRel factor (b:BBox)  : BBox =
        if factor < 0.0 then
            fail $"BBox.expandRel: a negative factor {factor} is not allowed for expanding the 3D bounding box {b.AsString}"
        let center = b.Center
        let sizeX = b.SizeX * factor
        let sizeY = b.SizeY * factor
        let sizeZ = b.SizeZ * factor
        BBox.createFromCenter(center, sizeX, sizeY, sizeZ)

    /// Returns the 3D bounding box expanded by a relative factor on all six sides, separately for X, Y, Z.
    /// Values between 0.0 and 1.0 shrink the box.
    /// Values larger than 1.0 expand the box.
    /// Does check for underflow if any factor is negative and raises EuclidException.
    static member expandRelXYZ factorX factorY factorZ (b:BBox)  : BBox =
        if factorX < 0.0 then
            fail $"BBox.expandRelXYZ: a negative factorX {factorX} is not allowed for expanding the 3D bounding box {b.AsString}"
        if factorY < 0.0 then
            fail $"BBox.expandRelXYZ: a negative factorY {factorY} is not allowed for expanding the 3D bounding box {b.AsString}"
        if factorZ < 0.0 then
            fail $"BBox.expandRelXYZ: a negative factorZ {factorZ} is not allowed for expanding the 3D bounding box {b.AsString}"
        let center = b.Center
        let sizeX = b.SizeX * factorX
        let sizeY = b.SizeY * factorY
        let sizeZ = b.SizeZ * factorZ
        BBox.createFromCenter(center, sizeX, sizeY, sizeZ)

    /// Returns a 3D bounding box moved by a vector.
    static member move (v:Vec) (b:BBox)  : BBox =
        BBox.createUnchecked(b.MinX+v.X, b.MinY+v.Y, b.MinZ+v.Z, b.MaxX+v.X, b.MaxY+v.Y, b.MaxZ+v.Z)

    /// Returns a 3D bounding box moved by a vector.
    /// This is an alias for the 'move' function.
    static member translate (v:Vec) (b:BBox)  : BBox =
        BBox.createUnchecked(b.MinX+v.X, b.MinY+v.Y, b.MinZ+v.Z, b.MaxX+v.X, b.MaxY+v.Y, b.MaxZ+v.Z)

    /// Returns a new 3D bounding box moved in X-axis direction.
    static member moveX (translation:float) (b:BBox)  : BBox =
        BBox.createUnchecked(b.MinX+translation, b.MinY, b.MinZ, b.MaxX+translation, b.MaxY, b.MaxZ)

    /// Returns a new 3D bounding box moved in Y-axis direction.
    static member moveY (translation:float) (b:BBox)  : BBox =
        BBox.createUnchecked(b.MinX, b.MinY+translation, b.MinZ, b.MaxX, b.MaxY+translation, b.MaxZ)

    /// Returns a new 3D bounding box moved in Z-axis direction.
    static member moveZ (translation:float) (b:BBox)  : BBox =
        BBox.createUnchecked(b.MinX, b.MinY, b.MinZ+translation, b.MaxX, b.MaxY, b.MaxZ+translation)


    /// Returns TRUE if the two a 3D bounding boxes do overlap or touch exactly.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    static member inline doOverlap(a:BBox) (b:BBox) =
        b.OverlapsWith(a)

    /// Returns TRUE if the two a 3D bounding boxes do overlap more than a given tolerance distance.
    /// Returns false if the two a 3D bounding boxes are just touching.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    static member inline doOverlapMoreThan tol (a:BBox) (b:BBox)  : bool =
        b.OverlapsWith(a, tol)

    /// Returns TRUE if the a 3D bounding box is inside or exactly on the other bounding Box.
    /// Argument order matters!
    static member inline contains (boxInside:BBox) (surroundingBox:BBox)  : bool =
        surroundingBox.Contains(boxInside)

    /// Returns TRUE if the point is inside or on the a 3D bounding box.
    static member inline containsPnt (pt:Pnt) (box:BBox)  : bool =
        box.Contains(pt)

    /// Returns a 3D bounding box that contains both input a 3D bounding boxes.
    static member inline union (a:BBox) (b:BBox)  : BBox =
        BBox.createUnchecked   (min b.MinX a.MinX, min b.MinY a.MinY, min b.MinZ a.MinZ,
                                max b.MaxX a.MaxX, max b.MaxY a.MaxY, max b.MaxZ a.MaxZ)

    /// Returns a bounding a 3D bounding box that contains the input a 3D bounding box and the point.
    static member inline unionPt (p:Pnt) (b:BBox) =
        BBox.createUnchecked   (min b.MinX p.X, min b.MinY p.Y, min b.MinZ p.Z,
                                max b.MaxX p.X, max b.MaxY p.Y, max b.MaxZ p.Z)

    /// Returns the intersection of two 3D bounding boxes.
    /// The returned BBox is the volume that is inside both input 3D bounding boxes.
    /// Returns ValueNone if the two 3D bounding boxes do not overlap.
    /// Just touching 3D bounding boxes will return ValueSome with a zero volume collapsed BBox.
    static member inline intersection (a:BBox) (b:BBox) =
        a.Intersection(b)

    /// Scales the 3D bounding box by a given factor.
    /// Scale center is World Origin 0,0,0.
    /// A negative factor will flip the box orientation, creating an invalid BBox.
    /// Use only positive factors.
    static member inline scale (factor:float) (b:BBox) : BBox =
        if factor < 0.0 then
            fail $"BBox.scale: a negative factor {factor} is not allowed for scaling the 3D bounding box {b.AsString}"
        BBox.createUnchecked(
            b.MinX * factor,
            b.MinY * factor,
            b.MinZ * factor,
            b.MaxX * factor,
            b.MaxY * factor,
            b.MaxZ * factor
        )






    [<Obsolete("use SizeX")>]
    member inline b.Width =
        b.MaxX - b.MinX

    [<Obsolete("use SizeY")>]
    member inline b.Depth =
        b.MaxY - b.MinY

    [<Obsolete("use SizeZ")>]
    member inline b.Height3D =
        b.MaxZ - b.MinZ

    [<Obsolete("typo, use ExpandSafe instead")>]
    member inline b.ExpandSave(xDist, yDist, zDist) : BBox =
        b.ExpandSafe(xDist, yDist, zDist)

    [<Obsolete("typo, use ExpandSafe instead")>]
    member inline b.ExpandSave(dist) : BBox =
        b.ExpandSafe(dist, dist, dist)

    /// Returns the 2D part of this 3D bounding box as a bounding rectangle (BRect).
    [<Obsolete("Use .asBRect instead")>]
    member inline b.asRect =
        b.asBRect

    [<Obsolete("typo, use expandSafe instead")>]
    static member expandSave dist (b:BBox) =
        b.ExpandSafe dist


