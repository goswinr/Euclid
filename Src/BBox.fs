namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open UtilEuclid
open EuclidErrors


/// <summary>
/// A struct of 6 floats representing an immutable 3D bounding box.
/// This implementation guarantees the box to always be valid.
/// That means the Min X, Y, and Z values are always smaller or equal to the respective Max values.
/// The X, Y, and Z axes are also called Width, Depth, and Height3D.
/// <code>
///   Z-axis       Y-axis
///   ^           /
///   |   7      /        6 MaxPnt
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
///   +---------------+----> X-axis
///   0 MinPnt        1
///
/// A BBox centered at the origin the points has these signs of X, Y and Z coordinates:
/// Pt0 (-,-,-)
/// Pt1 (+,-,-)
/// Pt2 (+,-,+)
/// Pt3 (-,-,+)
/// Pt4 (-,+,-)
/// Pt5 (+,+,-)
/// Pt6 (+,+,+)
/// Pt7 (-,+,+)
/// </code>
/// </summary>
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type BBox =
    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The fields holding the minimum X value of this 3D bounding box.
    [<DataMember>]
    val MinX : float

    /// The fields holding the minimum Y value of this 3D bounding box.
    [<DataMember>]
    val MinY : float

    /// The fields holding the minimum Z value of this 3D bounding box.
    [<DataMember>]
    val MinZ : float
    /// The fields holding the maximum X value of this 3D bounding box.
    [<DataMember>]
    val MaxX : float

    /// The fields holding the maximum Y value of this 3D bounding box.
    [<DataMember>]
    val MaxY : float

    /// The fields holding the maximum Z value of this 3D bounding box.
    [<DataMember>]
    val MaxZ : float

    /// Unsafe internal constructor, public only for inlining.
    [<Obsolete("This is not Obsolete, but an unsafe internal constructor. the input is not verified, so it might create invalid geometry. It is exposed as a public member so that it can be inlined.") >]
    new (minX, minY, minZ, maxX, maxY, maxZ) = {
        MinX = minX
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
    override b.ToString() : string =
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

    /// Returns a concise formatted string representation of the 3D bounding box.
    static member inline asString (b:BBox) : string =
        b.AsString

    /// Format bounding box into an F# code string that can be used to recreate the bounding box.
    member b.AsFSharpCode : string =
        $"BBox.createUnchecked({b.MinX}, {b.MinY}, {b.MinZ}, {b.MaxX}, {b.MaxY}, {b.MaxZ})"

    /// Returns an F# code string that recreates the 3D bounding box.
    static member inline asFSharpCode (b:BBox) : string =
        b.AsFSharpCode

    /// The point where X, Y, and Z are the minimum values.
    member inline b.MinPnt : Pnt =
        Pnt(b.MinX, b.MinY, b.MinZ)

    /// Returns the minimum corner point of the 3D bounding box.
    static member inline minPnt (b:BBox) : Pnt =
        b.MinPnt

    /// The point where X, Y, and Z are the maximum values.
    member inline b.MaxPnt : Pnt =
        Pnt(b.MaxX, b.MaxY, b.MaxZ)

    /// Returns the maximum corner point of the 3D bounding box.
    static member inline maxPnt (b:BBox) : Pnt =
        b.MaxPnt

    /// The size in X direction, often also called Width.
    member inline b.SizeX : float =
        b.MaxX - b.MinX

    /// Returns the X-size (width) of the 3D bounding box.
    static member inline sizeX (b:BBox) : float =
        b.SizeX

    /// The size in Y direction.
    member inline b.SizeY : float =
        b.MaxY - b.MinY

    /// Returns the Y-size (depth) of the 3D bounding box.
    static member inline sizeY (b:BBox) : float =
        b.SizeY

    /// The size in Z direction, also called Height.
    member inline b.SizeZ : float =
        b.MaxZ - b.MinZ

    /// Returns the Z-size (height) of the 3D bounding box.
    static member inline sizeZ (b:BBox) : float =
        b.SizeZ

    /// The diagonal 3D vector of this 3D bounding box. From MinPnt to MaxPnt.
    member inline b.Diagonal : Vec =
        Vec(b.MaxX - b.MinX, b.MaxY - b.MinY, b.MaxZ - b.MinZ)

    /// Returns the diagonal vector from MinPnt to MaxPnt.
    static member inline diagonal (b:BBox) : Vec =
        b.Diagonal

    /// The center of this 3D bounding box.
    member inline b.Center : Pnt =
        Pnt( (b.MaxX + b.MinX)*0.5, (b.MaxY + b.MinY)*0.5, (b.MaxZ + b.MinZ)*0.5)

    /// Returns the center point of the 3D bounding box.
    static member inline center (b:BBox) : Pnt =
        b.Center

    /// Returns the volume of this 3D bounding box.
    member inline b.Volume : float =
        b.SizeX * b.SizeY * b.SizeZ

    /// Returns the volume of a 3D bounding box.
    static member inline volume (b:BBox)  : float =
        b.SizeX * b.SizeY * b.SizeZ

    /// Returns a 3D box representation of this 3D bounding box.
    member inline b.AsBox : Box =
        Box.createUnchecked(b.MinX, b.MinY, b.MinZ, b.SizeX, 0.0, 0.0, 0.0, b.SizeY, 0.0, 0.0, 0.0, b.SizeZ)

    /// Returns a 3D box representation of the given 3D bounding box.
    static member inline asBox (b:BBox) : Box =
        b.AsBox


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

    /// Returns a 3D bounding box expanded by distance.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (b:BBox)  : BBox =
        b.Expand dist

    /// Returns a 3D bounding box expanded by separate distances in X, Y and Z.
    static member inline expandXYZ xDist yDist zDist (b:BBox) : BBox =
        b.Expand(xDist, yDist, zDist)

    /// Returns a 3D bounding box expanded by a distance for X, Y, and Z-axis each.
    /// If expansion is negative it shrinks the box without causing underflow.
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
    /// If expansion is negative it shrinks the box without causing underflow.
    /// When the negative expansion is bigger than the size on any axis, both Min and Max values
    /// on that axis will be set to the midpoint of their original values (the box collapses to a plane or line on that axis).
    member inline b.ExpandSafe(dist) : BBox =
        b.ExpandSafe(dist, dist, dist)

    /// Returns a 3D bounding box expanded by distance.
    /// If expansion is negative it shrinks the box without causing underflow.
    /// When the negative expansion is bigger than the size on any axis, both Min and Max values
    /// on that axis will be set to the midpoint of their original values (the box collapses to a plane or line on that axis).
    static member expandSafe dist (b:BBox)  : BBox =
        b.ExpandSafe dist

    /// Returns a 3D bounding box safely expanded by separate distances in X, Y and Z.
    static member inline expandSafeXYZ xDist yDist zDist (b:BBox) : BBox =
        b.ExpandSafe(xDist, yDist, zDist)

    /// Returns a 3D bounding box expanded only in X direction by different distances for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.ExpandXaxis(startDist, endDist) : BBox =
        let n = BBox.createUnchecked(b.MinX-startDist, b.MinY, b.MinZ, b.MaxX+endDist, b.MaxY, b.MaxZ)
        if n.MinX > n.MaxX then
            fail $"BBox.ExpandXaxis: Negative distances for start({startDist}) and end ({endDist}) cause an underflow, on {b.AsString}"
        n

    /// Returns a 3D bounding box expanded only in X direction by different distances for start(minX) and end (maxX).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandXaxis startDist endDist (b:BBox)  : BBox =
        b.ExpandXaxis(startDist, endDist)

    /// Returns a 3D bounding box expanded only in Y direction by different distances for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.ExpandYaxis(startDist, endDist) : BBox =
        let n = BBox.createUnchecked(b.MinX, b.MinY-startDist, b.MinZ, b.MaxX, b.MaxY+endDist, b.MaxZ)
        if n.MinY > n.MaxY then
            fail $"BBox.ExpandYaxis: Negative distances for start({startDist}) and end({endDist}) cause an underflow, on {b.AsString}"
        n

    /// Returns a 3D bounding box expanded only in Y direction by different distances for start(minY) and end (maxY).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandYaxis startDist endDist (b:BBox)  : BBox =
        b.ExpandYaxis(startDist, endDist)

    /// Returns a 3D bounding box expanded only in Z direction by different distances for start(minZ) and end (maxZ).
    /// Does check for underflow if distance is negative and raises EuclidException.
    member inline b.ExpandZaxis(startDist, endDist) : BBox =
        let n = BBox.createUnchecked(b.MinX, b.MinY, b.MinZ-startDist, b.MaxX, b.MaxY, b.MaxZ+endDist)
        if n.MinZ > n.MaxZ then
            fail $"BBox.ExpandZaxis: Negative distances for start({startDist}) and end({endDist}) cause an underflow, on {b.AsString}"
        n

    /// Returns a 3D bounding box expanded only in Z direction by different distances for start(minZ) and end (maxZ).
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expandZaxis startDist endDist (b:BBox)  : BBox =
        b.ExpandZaxis(startDist, endDist)

    /// Returns the longest edge of the Box.
    member inline b.LongestEdge : float =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        let z = b.MaxZ - b.MinZ
        max (max x y) z

    /// Returns the longest edge length of the 3D bounding box.
    static member inline longestEdge (b:BBox) : float =
        b.LongestEdge

    /// Returns the shortest edge of the Box.
    member inline b.ShortestEdge : float =
        let x = b.MaxX - b.MinX
        let y = b.MaxY - b.MinY
        let z = b.MaxZ - b.MinZ
        min (min x y) z

    /// Returns the shortest edge length of the 3D bounding box.
    static member inline shortestEdge (b:BBox) : float =
        b.ShortestEdge

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// Same as b.IsPoint.
    member inline b.IsZero : bool =
        isTooTiny (b.MaxX - b.MinX) &&
        isTooTiny (b.MaxY - b.MinY) &&
        isTooTiny (b.MaxZ - b.MinZ)

    /// Returns TRUE if all box dimensions are below zero-length tolerance.
    /// Same as BBox.isPoint.
    static member inline isZero (b:BBox) : bool =
        b.IsZero

    /// Counts the amount of sides that are smaller than the zeroLength tolerance.
    /// This is 0, 1, 2 or 3.
    member inline b.CountZeroSides : int =
        countTooTinyOrNaN    (b.MaxX - b.MinX)
        +  countTooTinyOrNaN (b.MaxY - b.MinY)
        +  countTooTinyOrNaN (b.MaxZ - b.MinZ)

    /// Returns the amount of dimensions that are below zero-length tolerance.
    static member inline countZeroSides (b:BBox) : int =
        b.CountZeroSides

    /// Tests if two of the X, Y, and Z axes are smaller than the zeroLength tolerance.
    member inline b.IsLine : bool =
        b.CountZeroSides = 2

    /// Returns TRUE if exactly two dimensions are below zero-length tolerance.
    static member inline isLine (b:BBox) : bool =
        b.IsLine

    /// Tests if one of the X, Y, and Z axes is smaller than the zeroLength tolerance.
    member inline b.IsFlat : bool =
        b.CountZeroSides = 1

    /// Returns TRUE if exactly one dimension is below zero-length tolerance.
    static member inline isFlat (b:BBox) : bool =
        b.IsFlat

    /// Tests if all sides are smaller than the zeroLength tolerance.
    /// Same as b.IsZero.
    member inline b.IsPoint : bool =
        b.IsZero

    /// Returns TRUE if all dimensions are below zero-length tolerance.
    /// Same as BBox.isZero.
    static member inline isPoint (b:BBox) : bool =
        b.IsPoint

    /// Tests if none of the X, Y, and Z axes are smaller than the zeroLength tolerance.
    /// Same as .HasVolume
    member inline b.IsValid : bool =
        b.CountZeroSides = 0

    /// Returns TRUE if no dimension is below zero-length tolerance.
    static member inline isValid (b:BBox) : bool =
        b.IsValid

    /// Tests if none of the X, Y, and Z axes is smaller than the zeroLength tolerance.
    /// Same as .IsValid
    member inline b.HasVolume : bool =
        b.CountZeroSides = 0

    /// Returns TRUE if the 3D bounding box has volume.
    static member inline hasVolume (b:BBox) : bool =
        b.HasVolume

    /// Returns the 2D part of this 3D bounding box as a bounding rectangle (BRect).
    member inline b.AsBRect : BRect =
        BRect.createUnchecked(b.MinX, b.MinY, b.MaxX, b.MaxY)

    /// Returns the 2D part of a 3D bounding box as a bounding rectangle (BRect).
    /// Projects the BBox onto the XY plane, discarding Z information.
    static member inline asBRect (b:BBox)  : BRect =
        BRect.createUnchecked(b.MinX, b.MinY, b.MaxX, b.MaxY)

    /// Returns TRUE if the two 3D bounding boxes do overlap or touch exactly.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    /// Use .IsOverlappingOrClose if you want to specify a tolerance for near misses.
    member inline b.IsOverlapping (a:BBox) : bool =
        a.MinX <= b.MaxX &&
        b.MinX <= a.MaxX &&
        a.MinY <= b.MaxY &&
        b.MinY <= a.MaxY &&
        a.MinZ <= b.MaxZ &&
        b.MinZ <= a.MaxZ

    /// Returns TRUE if the two 3D bounding boxes do overlap or touch exactly.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    /// Use BBox.isOverlappingOrClose if you want to specify a tolerance for near misses.
    static member inline isOverlapping (a:BBox) (b:BBox) : bool =
        b.IsOverlapping(a)

    /// Returns TRUE if the two 3D bounding boxes do overlap or are apart less than the specified tolerance.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    /// A negative tolerance shrinks the effective boundaries instead of expanding them.
    member inline b.IsOverlappingOrClose (a:BBox, tolerance:float) : bool =
        a.MinX - tolerance <= b.MaxX &&
        b.MinX - tolerance <= a.MaxX &&
        a.MinY - tolerance <= b.MaxY &&
        b.MinY - tolerance <= a.MaxY &&
        a.MinZ - tolerance <= b.MaxZ &&
        b.MinZ - tolerance <= a.MaxZ

    /// Returns TRUE if the two 3D bounding boxes do overlap or are apart less than the specified tolerance.
    /// Also returns TRUE if one box is completely inside the other.
    /// Also returns TRUE if one box is completely surrounding the other.
    /// A negative tolerance shrinks the effective boundaries instead of expanding them. The math works out symmetrically:
    static member inline isOverlappingOrClose (tolerance:float) (a:BBox) (b:BBox) : bool =
        b.IsOverlappingOrClose(a, tolerance)

    /// Returns TRUE if the two 3D bounding boxes are just touching.
    /// The default tolerance is 1e-6.But can also be specified as a parameter.
    /// Returns FALSE if one box is completely inside the other.
    /// Returns FALSE if one box is completely surrounding the other.
    member inline this.IsTouching (other:BBox, [<OPT;DEF(1e-6)>] tolerance:float) : bool =
        // Signed face-gaps: positive = faces are apart, negative = faces penetrate
        let g1 = this.MinX - other.MaxX
        let g2 = other.MinX - this.MaxX
        let g3 = this.MinY - other.MaxY
        let g4 = other.MinY - this.MaxY
        let g5 = this.MinZ - other.MaxZ
        let g6 = other.MinZ - this.MaxZ
        // Per-axis separation: >0 = apart, <0 = overlap
        let gx = max g1 g2
        let gy = max g3 g4
        let gz = max g5 g6
        if gx > tolerance || gy > tolerance || gz > tolerance then
            false  // fully apart on at least one axis
        elif gx < 0.0 && gy < 0.0 && gz < 0.0 then
            false // share volume
        else
            true

    /// Returns TRUE if the two 3D bounding boxes are just touching within a given specified tolerance.
    /// Returns FALSE if one box is completely inside the other.
    /// Returns FALSE if one box is completely surrounding the other.
    static member inline isTouching (tolerance:float) (other:BBox) (thisBox:BBox) : bool =
        thisBox.IsTouching(other, tolerance)

    /// Returns a value from -1 to 6 indicating the side on which the two 3D bounding boxes
    /// are just touching within the tolerance.
    /// The default tolerance is 1e-6.But can also be specified as a parameter.
    ///   -1 : not touching but overlapping / one inside the other
    ///    0 : not touching, apart on at least one axis by more than tolerance
    ///    1 : other is in front  of this (this.MinX touches other.MaxX)
    ///    2 : other is behind    this  (this.MaxX touches other.MinX)
    ///    3 : other is to the left  of this (this.MinY touches other.MaxY)
    ///    4 : other is to the right of this (this.MaxY touches other.MinY)
    ///    5 : this.MinZ touches other.MaxZ
    ///    6 : this.MaxZ touches other.MinZ
    member inline this.TouchingSide (other:BBox, [<OPT;DEF(1e-6)>] tolerance:float) : int =
        // Signed face-gaps: positive = faces are apart, negative = faces penetrate
        let g1 = this.MinX - other.MaxX
        let g2 = other.MinX - this.MaxX
        let g3 = this.MinY - other.MaxY
        let g4 = other.MinY - this.MaxY
        let g5 = this.MinZ - other.MaxZ
        let g6 = other.MinZ - this.MaxZ
        // Per-axis separation: >0 = apart, <0 = overlap
        let gx = max g1 g2
        let gy = max g3 g4
        let gz = max g5 g6
        if gx > tolerance || gy > tolerance || gz > tolerance then
            0  // fully apart on at least one axis
        elif gx < 0.0 && gy < 0.0 && gz < 0.0 then
            -1 // share volume
        else
            // At least one axis has a face-gap in [0, tolerance]. Pick the tightest.
            let mutable side = 0
            let mutable best = System.Double.PositiveInfinity
            if g1 >= 0.0 && g1 < best then side <- 1; best <- g1
            if g2 >= 0.0 && g2 < best then side <- 2; best <- g2
            if g3 >= 0.0 && g3 < best then side <- 3; best <- g3
            if g4 >= 0.0 && g4 < best then side <- 4; best <- g4
            if g5 >= 0.0 && g5 < best then side <- 5; best <- g5
            if g6 >= 0.0 && g6 < best then side <- 6; best <- g6
            side

    /// Returns a value from -1 to 6 indicating the side on which the two 3D bounding boxes
    /// are just touching within a given specified tolerance.
    ///   -1 : not touching but overlapping / one inside the other
    ///    0 : not touching, apart on at least one axis by more than tolerance
    ///    1 : other is in front  of this (this.MinX touches other.MaxX)
    ///    2 : other is behind    this  (this.MaxX touches other.MinX)
    ///    3 : other is to the left  of this (this.MinY touches other.MaxY)
    ///    4 : other is to the right of this (this.MaxY touches other.MinY)
    ///    5 : this.MinZ touches other.MaxZ
    ///    6 : this.MaxZ touches other.MinZ
    static member inline touchingSide tolerance (other:BBox) (thisBox:BBox) : int =
        thisBox.TouchingSide(other, tolerance)

    /// Returns TRUE if the point is inside or exactly on the bounding Box.
    member inline b.ContainsXYZ (x:float, y:float, z:float) : bool =
        x >= b.MinX &&
        x <= b.MaxX &&
        y >= b.MinY &&
        y <= b.MaxY &&
        z >= b.MinZ &&
        z <= b.MaxZ

    /// Returns TRUE if the point is inside or on a 3D bounding box.
    static member inline containsXYZ (x:float) (y:float) (z:float) (box:BBox)  : bool =
        box.ContainsXYZ (x, y, z)

    /// Returns TRUE if the point is inside or exactly on the bounding Box.
    member inline b.ContainsPnt (p:Pnt) : bool =
        b.ContainsXYZ (p.X, p.Y, p.Z)

    /// Returns TRUE if the point is inside or on a 3D bounding box.
    static member inline containsPnt (pt:Pnt) (box:BBox)  : bool =
        box.ContainsPnt(pt)

    /// Returns TRUE if the other 3D bounding box is inside or exactly on this bounding box.
    member inline b.Contains (o:BBox) : bool =
        b.ContainsXYZ(o.MinX, o.MinY, o.MinZ) &&
        b.ContainsXYZ(o.MaxX, o.MaxY, o.MaxZ)

    /// Returns TRUE if a 3D bounding box is inside or exactly on the other bounding Box.
    /// Argument order matters!
    static member inline contains (boxInside:BBox) (surroundingBox:BBox)  : bool =
        surroundingBox.Contains(boxInside)

    /// Evaluate an X, Y and Z parameter of this 3D bounding box.
    /// 0.0, 0.0, 0.0 returns the MinPnt.
    /// 1.0, 1.0, 1.0 returns the MaxPnt.
    member inline b.EvaluateAt (xParameter, yParameter, zParameter) : Pnt =
        Pnt(b.MinX + (b.MaxX-b.MinX) * xParameter,
            b.MinY + (b.MaxY-b.MinY) * yParameter,
            b.MinZ + (b.MaxZ-b.MinZ) * zParameter)

    /// Evaluates X, Y and Z parameters on the 3D bounding box and returns a point.
    static member inline evaluateAt xParameter yParameter zParameter (b:BBox) : Pnt =
        b.EvaluateAt(xParameter, yParameter, zParameter)

    /// Returns a 3D bounding box that contains both input 3D bounding boxes.
    member inline b.Union (a:BBox) : BBox =
        BBox.createUnchecked   (min b.MinX a.MinX, min b.MinY a.MinY, min b.MinZ a.MinZ,
                                max b.MaxX a.MaxX, max b.MaxY a.MaxY, max b.MaxZ a.MaxZ)

    /// Returns a 3D bounding box that contains the input 3D bounding box and the point.
    member inline b.UnionPnt (p:Pnt) : BBox =
        BBox.createUnchecked   (min b.MinX p.X, min b.MinY p.Y, min b.MinZ p.Z,
                                max b.MaxX p.X, max b.MaxY p.Y, max b.MaxZ p.Z)

    /// Returns a 3D bounding box that contains both input 3D bounding boxes.
    static member inline union (a:BBox) (b:BBox)  : BBox =
        BBox.createUnchecked   (min b.MinX a.MinX, min b.MinY a.MinY, min b.MinZ a.MinZ,
                                max b.MaxX a.MaxX, max b.MaxY a.MaxY, max b.MaxZ a.MaxZ)

    /// Returns a 3D bounding box that contains the input 3D bounding box and the point.
    static member inline unionPnt (p:Pnt) (b:BBox) : BBox =
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

    /// Returns the intersection of two 3D bounding boxes.
    /// The returned BBox is the volume that is inside both input 3D bounding boxes.
    /// Returns ValueNone if the two 3D bounding boxes do not overlap.
    /// Just touching 3D bounding boxes will return ValueSome with a zero volume collapsed BBox.
    static member inline intersection (a:BBox) (b:BBox) : BBox voption =
        a.Intersection(b)

    /// <summary>Intersects an infinite ray (a Line3D extended infinitely in both directions) with this 3D bounding box.
    /// Uses the slab intersection method.
    /// Returns ValueNone if the ray does not intersect the bounding box or if the ray direction is too short.
    /// Returns ValueSome with the entry and exit parameters on the ray if it intersects.
    /// A parameter of 0.0 corresponds to the ray's From point, and 1.0 to its To point.
    /// Contact with the boundary counts as an intersection: a ray exactly on a face intersects if its other coordinates pass through the box.
    /// A ray parallel to a slab axis and outside that axis returns ValueNone, even if it is outside by only a tiny amount such as 1e-16.
    /// A ray that touches only one box corner returns ValueSome(t, t), where t is the corner parameter.</summary>
    /// <param name="ray">The ray to intersect with the bounding box.</param>
    /// <returns>ValueNone if there is no intersection; otherwise, ValueSome(tEntry, tExit).</returns>
    member b.IntersectRay (ray:Line3D) : voption<float*float> =
        let dirX = ray.VectorX
        let dirY = ray.VectorY
        let dirZ = ray.VectorZ
        if isTooSmallSq (dirX*dirX + dirY*dirY + dirZ*dirZ) then
            ValueNone
        else
            let mutable tMin = Double.MinValue
            let mutable tMax = Double.MaxValue

            if abs dirX < 1e-12 then
                if ray.FromX < b.MinX || ray.FromX > b.MaxX then
                    tMin <- Double.MaxValue
            else
                let t1 = (b.MinX - ray.FromX) / dirX
                let t2 = (b.MaxX - ray.FromX) / dirX
                tMin <- max tMin (min t1 t2)
                tMax <- min tMax (max t1 t2)

            if tMin <= tMax then
                if abs dirY < 1e-12 then
                    if ray.FromY < b.MinY || ray.FromY > b.MaxY then
                        tMin <- Double.MaxValue
                else
                    let t1 = (b.MinY - ray.FromY) / dirY
                    let t2 = (b.MaxY - ray.FromY) / dirY
                    tMin <- max tMin (min t1 t2)
                    tMax <- min tMax (max t1 t2)

            if tMin <= tMax then
                if abs dirZ < 1e-12 then
                    if ray.FromZ < b.MinZ || ray.FromZ > b.MaxZ then
                        tMin <- Double.MaxValue
                else
                    let t1 = (b.MinZ - ray.FromZ) / dirZ
                    let t2 = (b.MaxZ - ray.FromZ) / dirZ
                    tMin <- max tMin (min t1 t2)
                    tMax <- min tMax (max t1 t2)

            if tMin <= tMax then
                ValueSome(tMin, tMax)
            else
                ValueNone

    /// Same as b.IntersectRay(ray).
    static member inline intersectRay (ray:Line3D) (b:BBox) : voption<float*float> =
        b.IntersectRay ray


    // #endregion
    // #region Points

    /// <summary>Returns point 0 of this 3D bounding box, same as member box.MinPnt.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member inline b.Pt0 : Pnt =
        Pnt(b.MinX, b.MinY, b.MinZ)

    /// Returns corner point 0 of the 3D bounding box.
    static member inline pt0 (b:BBox) : Pnt =
        b.Pt0

    /// <summary>Returns point 1 of this 3D bounding box.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member inline b.Pt1 : Pnt =
        Pnt(b.MaxX, b.MinY, b.MinZ)

    /// Returns corner point 1 of the 3D bounding box.
    static member inline pt1 (b:BBox) : Pnt =
        b.Pt1

    /// <summary>Returns point 2 of this 3D bounding box.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member inline b.Pt2 : Pnt =
        Pnt(b.MaxX, b.MaxY, b.MinZ)

    /// Returns corner point 2 of the 3D bounding box.
    static member inline pt2 (b:BBox) : Pnt =
        b.Pt2

    /// <summary>Returns point 3 of this 3D bounding box.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member inline b.Pt3 : Pnt =
        Pnt(b.MinX, b.MaxY, b.MinZ)

    /// Returns corner point 3 of the 3D bounding box.
    static member inline pt3 (b:BBox) : Pnt =
        b.Pt3

    /// <summary>Returns point 4 of this 3D bounding box.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member inline b.Pt4 : Pnt =
        Pnt(b.MinX, b.MinY, b.MaxZ)

    /// Returns corner point 4 of the 3D bounding box.
    static member inline pt4 (b:BBox) : Pnt =
        b.Pt4

    /// <summary>Returns point 5 of this 3D bounding box.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member inline b.Pt5 : Pnt =
        Pnt(b.MaxX, b.MinY, b.MaxZ)

    /// Returns corner point 5 of the 3D bounding box.
    static member inline pt5 (b:BBox) : Pnt =
        b.Pt5

    /// <summary>Returns point 6 of this 3D bounding box.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member inline b.Pt6 : Pnt =
        Pnt(b.MaxX, b.MaxY, b.MaxZ)

    /// Returns corner point 6 of the 3D bounding box.
    static member inline pt6 (b:BBox) : Pnt =
        b.Pt6

    /// <summary>Returns point 7 of this 3D bounding box.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member inline b.Pt7 : Pnt =
        Pnt(b.MinX, b.MaxY, b.MaxZ)

    /// Returns corner point 7 of the 3D bounding box.
    static member inline pt7 (b:BBox) : Pnt =
        b.Pt7

    /// <summary>Returns the bottom corners of this 3D bounding box in counter-clockwise order, starting at MinPnt.
    /// Then the top corners starting above MinPnt. Returns an array of 8 Points.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member b.Points : Pnt[] =
        [|b.Pt0; b.Pt1; b.Pt2; b.Pt3; b.Pt4; b.Pt5; b.Pt6; b.Pt7|]

    /// Returns all 8 corner points of the 3D bounding box.
    static member inline points (b:BBox) : Pnt[] =
        b.Points


    /// <summary>Returns the point of the box at the specified index.
    /// The order of the points is: Pt0, Pt1, Pt2, Pt3, Pt4, Pt5, Pt6, Pt7.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member b.GetPoint (pointIndex:int) : Pnt =
        match pointIndex with
        | 0 -> b.Pt0
        | 1 -> b.Pt1
        | 2 -> b.Pt2
        | 3 -> b.Pt3
        | 4 -> b.Pt4
        | 5 -> b.Pt5
        | 6 -> b.Pt6
        | 7 -> b.Pt7
        | _ -> fail $"Box.GetPoint: pointIndex {pointIndex} is out of range. Valid range is 0 to 7."

    /// Returns the point of the box at the specified index.
    static member inline getPoint (pointIndex:int) (b:BBox) : Pnt =
        b.GetPoint pointIndex


    /// <summary>Returns the bottom of the box as a counter-clockwise array of 4 Points.
    /// Starting at MinPnt. Points 0, 1, 2, and 3.
    /// Last and first point are NOT the same.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member b.BottomPoints : Pnt[] =
        [|b.Pt0; b.Pt1; b.Pt2; b.Pt3|]

    /// Returns the 4 bottom face points of the 3D bounding box.
    static member inline bottomPoints (b:BBox) : Pnt[] =
        b.BottomPoints

    /// <summary>Returns the bottom of the box as a counter-clockwise array of 5 Points, starting at MinPnt.
    /// Points 0, 1, 2, 3, and again 0.
    /// Last and first point are the same.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member b.BottomPointsLooped : Pnt[] =
        [|b.Pt0; b.Pt1; b.Pt2; b.Pt3; b.Pt0|]

    /// Returns the looped 5-point bottom face polyline of the 3D bounding box.
    static member inline bottomPointsLooped (b:BBox) : Pnt[] =
        b.BottomPointsLooped

    /// <summary>Returns the top of the box as a counter-clockwise array of 4 Points.
    /// Starting at point 4 then 5, 6, and 7.
    /// Last and first point are NOT the same.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member b.TopPoints : Pnt[] =
        [|b.Pt4; b.Pt5; b.Pt6; b.Pt7|]

    /// Returns the 4 top face points of the 3D bounding box.
    static member inline topPoints (b:BBox) : Pnt[] =
        b.TopPoints

    /// <summary>Returns the top of the box as a counter-clockwise array of 5 Points.
    /// Points 4, 5, 6, 7, and again 4.
    /// Last and first point are the same.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member b.TopPointsLooped : Pnt[] =
        [|b.Pt4; b.Pt5; b.Pt6; b.Pt7; b.Pt4|]

    /// Returns the looped 5-point top face polyline of the 3D bounding box.
    static member inline topPointsLooped (b:BBox) : Pnt[] =
        b.TopPointsLooped

    // #endregion
    // #region Edges

    /// Returns the X-aligned edge from point 0 to 1.
    member inline b.Edge01 : Line3D =
        Line3D(b.MinX, b.MinY, b.MinZ, b.MaxX, b.MinY, b.MinZ)

    /// Returns the X-aligned edge from point 0 to 1.
    static member inline edge01 (b:BBox) : Line3D =
        b.Edge01

    /// Returns the Y-aligned edge from point 1 to 2.
    member inline b.Edge12 : Line3D =
        Line3D(b.MaxX, b.MinY, b.MinZ, b.MaxX, b.MaxY, b.MinZ)

    /// Returns the Y-aligned edge from point 1 to 2.
    static member inline edge12 (b:BBox) : Line3D =
        b.Edge12

    /// Returns the X-aligned edge from point 3 to 2.
    member inline b.Edge32 : Line3D =
        Line3D(b.MinX, b.MaxY, b.MinZ, b.MaxX, b.MaxY, b.MinZ)

    /// Returns the X-aligned edge from point 3 to 2.
    static member inline edge32 (b:BBox) : Line3D =
        b.Edge32

    /// Returns the Y-aligned edge from point 0 to 3.
    member inline b.Edge03 : Line3D =
        Line3D(b.MinX, b.MinY, b.MinZ, b.MinX, b.MaxY, b.MinZ)

    /// Returns the Y-aligned edge from point 0 to 3.
    static member inline edge03 (b:BBox) : Line3D =
        b.Edge03

    /// Returns the Z-aligned edge from point 0 to 4.
    member inline b.Edge04 : Line3D =
        Line3D(b.MinX, b.MinY, b.MinZ, b.MinX, b.MinY, b.MaxZ)

    /// Returns the Z-aligned edge from point 0 to 4.
    static member inline edge04 (b:BBox) : Line3D =
        b.Edge04

    /// Returns the Z-aligned edge from point 1 to 5.
    member inline b.Edge15 : Line3D =
        Line3D(b.MaxX, b.MinY, b.MinZ, b.MaxX, b.MinY, b.MaxZ)

    /// Returns the Z-aligned edge from point 1 to 5.
    static member inline edge15 (b:BBox) : Line3D =
        b.Edge15

    /// Returns the Z-aligned edge from point 2 to 6.
    member inline b.Edge26 : Line3D =
        Line3D(b.MaxX, b.MaxY, b.MinZ, b.MaxX, b.MaxY, b.MaxZ)

    /// Returns the Z-aligned edge from point 2 to 6.
    static member inline edge26 (b:BBox) : Line3D =
        b.Edge26

    /// Returns the Z-aligned edge from point 3 to 7.
    member inline b.Edge37 : Line3D =
        Line3D(b.MinX, b.MaxY, b.MinZ, b.MinX, b.MaxY, b.MaxZ)

    /// Returns the Z-aligned edge from point 3 to 7.
    static member inline edge37 (b:BBox) : Line3D =
        b.Edge37

    /// Returns the X-aligned edge from point 4 to 5.
    member inline b.Edge45 : Line3D =
        Line3D(b.MinX, b.MinY, b.MaxZ, b.MaxX, b.MinY, b.MaxZ)

    /// Returns the X-aligned edge from point 4 to 5.
    static member inline edge45 (b:BBox) : Line3D =
        b.Edge45

    /// Returns the Y-aligned edge from point 5 to 6.
    member inline b.Edge56 : Line3D =
        Line3D(b.MaxX, b.MinY, b.MaxZ, b.MaxX, b.MaxY, b.MaxZ)

    /// Returns the Y-aligned edge from point 5 to 6.
    static member inline edge56 (b:BBox) : Line3D =
        b.Edge56

    /// Returns the X-aligned edge from point 7 to 6.
    member inline b.Edge76 : Line3D =
        Line3D(b.MinX, b.MaxY, b.MaxZ, b.MaxX, b.MaxY, b.MaxZ)

    /// Returns the X-aligned edge from point 7 to 6.
    static member inline edge76 (b:BBox) : Line3D =
        b.Edge76

    /// Returns the Y-aligned edge from point 4 to 7.
    member inline b.Edge47 : Line3D =
        Line3D(b.MinX, b.MinY, b.MaxZ, b.MinX, b.MaxY, b.MaxZ)

    /// Returns the Y-aligned edge from point 4 to 7.
    static member inline edge47 (b:BBox) : Line3D =
        b.Edge47

    /// <summary>Returns the 12 edges of this 3D bounding box as an array of 12 Lines.
    /// Pairs in this order:
    /// 0-1, 1-2, 3-2, 0-3, 0-4, 1-5, 2-6, 3-7, 4-5, 5-6, 7-6, 4-7
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member b.Edges : Line3D[] = // this function needs to be defined after the EdgeXX members, because they are inlined.
        [| b.Edge01; b.Edge12; b.Edge32; b.Edge03; // bottom face
           b.Edge04; b.Edge15; b.Edge26; b.Edge37; // vertical edges
           b.Edge45; b.Edge56; b.Edge76; b.Edge47 |] // top face

    /// Returns all 12 edges of the 3D bounding box.
    static member inline edges (b:BBox) : Line3D[] =
        b.Edges


    /// <summary>Returns the edge of this 3D bounding box at the specified index. Valid indices are 0 through 11.
    /// Edges use this order:
    /// 0-1, 1-2, 3-2, 0-3, 0-4, 1-5, 2-6, 3-7, 4-5, 5-6, 7-6, 4-7.
    /// <code>
    ///   Z-axis       Y-axis
    ///   ^           /
    ///   |   7      /        6 MaxPnt
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
    ///   +---------------+----> X-axis
    ///   0 MinPnt         1
    /// </code>
    /// </summary>
    member b.GetEdge (edgeIndex:int) : Line3D =
        match edgeIndex with
        | 0  -> b.Edge01
        | 1  -> b.Edge12
        | 2  -> b.Edge32
        | 3  -> b.Edge03
        | 4  -> b.Edge04
        | 5  -> b.Edge15
        | 6  -> b.Edge26
        | 7  -> b.Edge37
        | 8  -> b.Edge45
        | 9  -> b.Edge56
        | 10 -> b.Edge76
        | 11 -> b.Edge47
        | _ -> fail $"BBox.GetEdge: edgeIndex {edgeIndex} is out of range. Valid range is 0 to 11."

    /// Returns the edge of the BBox at the specified index.
    static member inline getEdge (edgeIndex:int) (b:BBox): Line3D =
        b.GetEdge(edgeIndex)


    // #endregion
    // #region Faces



    /// <summary>Returns the top face of the bounding box , looking from above.
    /// Returns Origin at point 4, X-axis to point 5, Y-axis to point 7.
    /// The normal of the Rect3D points up, away from the Box.
    /// <code>
    ///                       (back)
    ///            Z-axis     (top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- (right)
    ///            |   |           |   |
    ///    (left)--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/
    ///            +---------------+----> X-axis
    ///            0       |       1
    ///                    |
    ///                    (bottom)
    ///                    (front)
    /// </code>
    /// </summary>
    member b.TopFace :Rect3D =
        Rect3D.createUnchecked(b.MinX, b.MinY, b.MaxZ, b.SizeX, 0.0, 0.0, 0.0, b.SizeY, 0.0)

    /// Returns the top face of the bounding box , looking from above.
    /// Returns Origin at point 4, X-axis to point 5, Y-axis to point 7.
    /// The normal of the Rect3D points up, away from the Box.
    static member inline topFace (b:BBox) : Rect3D =
        b.TopFace

    /// <summary>Returns the bottom face of the bounding box , looking from above.
    /// Returns Origin at point 0, X-axis to point 1, Y-axis to point 3.
    /// The normal of the Rect3D points into the bounding box.
    /// <code>
    ///                       (back)
    ///            Z-axis     (top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- (right)
    ///            |   |           |   |
    ///    (left)--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/
    ///            +---------------+----> X-axis
    ///            0       |       1
    ///                    |
    ///                    (bottom)
    ///                    (front)
    /// </code>
    /// </summary>
    member b.BottomFace : Rect3D =
        Rect3D.createUnchecked(b.MinX, b.MinY, b.MinZ,  b.SizeX, 0.0, 0.0,   0.0, b.SizeY, 0.0)

    /// Returns the bottom face of the bounding box , looking from above.
    /// Returns Origin at point 0, X-axis to point 1, Y-axis to point 3.
    /// The normal of the Rect3D points into the bounding box.
    static member inline bottomFace (b:BBox) : Rect3D =
        b.BottomFace

    /// <summary>Returns the front face of the bounding box , looking from front.
    /// Returns Origin at point 0, X-axis to point 1, Y-axis to point 4.
    /// The normal of the Rect3D points away from the bounding box.
    /// <code>
    ///                       (back)
    ///            Z-axis     (top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- (right)
    ///            |   |           |   |
    ///    (left)--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/
    ///            +---------------+----> X-axis
    ///            0       |       1
    ///                    |
    ///                    (bottom)
    ///                    (front)
    /// </code>
    /// </summary>
    member b.FrontFace : Rect3D =
        Rect3D.createUnchecked(b.MinX, b.MinY, b.MinZ,  b.SizeX, 0.0, 0.0,  0.0, 0.0, b.SizeZ)

    /// Returns the front face of the bounding box , looking from front.
    /// Returns Origin at point 0, X-axis to point 1, Y-axis to point 4.
    /// The normal of the Rect3D points away from the bounding box.
    static member inline frontFace (b:BBox) : Rect3D =
        b.FrontFace

    /// <summary>Returns the back face of the bounding box , looking from front.
    /// Returns Origin at point 3, X-axis to point 2, Y-axis to point 7.
    /// The normal of the Rect3D points into the bounding box.
    /// <code>
    ///                       (back)
    ///            Z-axis     (top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- (right)
    ///            |   |           |   |
    ///    (left)--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/
    ///            +---------------+----> X-axis
    ///            0       |       1
    ///                    |
    ///                    (bottom)
    ///                    (front)
    /// </code>
    /// </summary>
    member b.BackFace : Rect3D =
        Rect3D.createUnchecked(b.MinX, b.MaxY, b.MinZ,  b.SizeX, 0.0, 0.0,  0.0, 0.0, b.SizeZ)

    /// Returns the back face of the bounding box , looking from front.
    /// Returns Origin at point 3, X-axis to point 2, Y-axis to point 7.
    /// The normal of the Rect3D points into the bounding box.
    static member inline backFace (b:BBox) : Rect3D =
        b.BackFace

    /// <summary>Returns the right face of the bounding box , looking from right.
    /// Returns Origin at point 1, X-axis to point 2, Y-axis to point 5.
    /// The normal of the Rect3D points away from the bounding box.
    /// <code>
    ///                       (back)
    ///            Z-axis     (top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- (right)
    ///            |   |           |   |
    ///    (left)--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/
    ///            +---------------+----> X-axis
    ///            0       |       1
    ///                    |
    ///                    (bottom)
    ///                    (front)
    /// </code>
    /// </summary>
    member b.RightFace : Rect3D =
        Rect3D.createUnchecked(b.MaxX, b.MinY, b.MinZ,  0.0, b.SizeY, 0.0,  0.0, 0.0, b.SizeZ)

    /// Returns the right face of the bounding box , looking from right.
    /// Returns Origin at point 1, X-axis to point 2, Y-axis to point 5.
    /// The normal of the Rect3D points away from the bounding box.
    static member inline rightFace (b:BBox) : Rect3D =
        b.RightFace

    /// <summary>Returns the left face of the bounding box , looking from right.
    /// Returns Origin at point 0, X-axis to point 3, Y-axis to point 4.
    /// The normal of the Rect3D points into the bounding box.
    /// <code>
    ///                       (back)
    ///            Z-axis     (top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- (right)
    ///            |   |           |   |
    ///    (left)--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/
    ///            +---------------+----> X-axis
    ///            0       |       1
    ///                    |
    ///                    (bottom)
    ///                    (front)
    /// </code>
    /// </summary>
    member b.LeftFace : Rect3D =
        Rect3D.createUnchecked(b.MinX, b.MinY, b.MinZ,  0.0, b.SizeY, 0.0,  0.0, 0.0, b.SizeZ)

    /// Returns the left face of the bounding box , looking from right.
    /// Returns Origin at point 0, X-axis to point 3, Y-axis to point 4.
    /// The normal of the Rect3D points into the bounding box.
    static member inline leftFace (b:BBox) : Rect3D =
        b.LeftFace


    /// <summary>Returns the six faces of the bounding box.
    /// The normals of the Rect3Ds are oriented with the X-axis, Y-axis, or Z-axis.
    /// The order of the Rect3D is: BottomFace, FrontFace, RightFace, BackFace, LeftFace, TopFace.
    /// <code>
    ///                       3 (back)
    ///            Z-axis     5 (top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- 2 (right)
    ///            |   |           |   |
    ///  4 (left)--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/
    ///            +---------------+----> X-axis
    ///            0       |       1
    ///                    |
    ///                   0 (bottom)
    ///                   1 (front)
    /// </code>
    /// </summary>
    member b.Faces : Rect3D[] =
        [|
        b.BottomFace
        b.FrontFace
        b.RightFace
        b.BackFace
        b.LeftFace
        b.TopFace
        |]

    /// Returns the six faces of the bounding box in documented order.
    static member inline faces (b:BBox) : Rect3D[] =
        b.Faces

    /// <summary>Returns the face of the box at the specified index.
    /// The order of the faces is: BottomFace, FrontFace, RightFace, BackFace, LeftFace, TopFace.
    /// <code>
    ///                       3 (back)
    ///            Z-axis     5 (top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- 2 (right)
    ///            |   |           |   |
    ///  4 (left)--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/
    ///            +---------------+----> X-axis
    ///            0       |       1
    ///                    |
    ///                   0 (bottom)
    ///                   1 (front)
    /// </code>
    /// </summary>
    /// <param name="faceIndex">The index of the face to retrieve. Valid range is 0 to 5.</param>
    /// <returns>The Rect3D representing the specified face of the Box.</returns>
    member b.GetFace (faceIndex:int) : Rect3D =
        match faceIndex with
        | 0 -> b.BottomFace
        | 1 -> b.FrontFace
        | 2 -> b.RightFace
        | 3 -> b.BackFace
        | 4 -> b.LeftFace
        | 5 -> b.TopFace
        | _ -> fail $"Box.GetFace: faceIndex {faceIndex} is out of range. Valid range is 0 to 5."

    /// <summary>Returns the face of the box at the specified index.
    /// The order of the faces is: BottomFace, FrontFace, RightFace, BackFace, LeftFace, TopFace.
    /// <code>
    ///                       3 (back)
    ///            Z-axis     5 (top)
    ///            ^          |
    ///            |   7      |        6
    ///            |   +---------------+
    ///            |  /|      |       /|
    ///            | / |             / |
    ///          4 |/  |          5 /  |
    ///            +---------------+  -|-- 2 (right)
    ///            |   |           |   |
    ///  4 (left)--|-  +-----------|---+
    ///            |  / 3          |  / 2
    ///            | /             | /
    ///            |/      |       |/
    ///            +---------------+----> X-axis
    ///            0       |       1
    ///                    |
    ///                   0 (bottom)
    ///                   1 (front)
    /// </code>
    /// </summary>
    static member inline getFace (faceIndex:int) (b:BBox) : Rect3D =
        b.GetFace faceIndex


    // #endregion
    // #region  create

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
    static member createFromSeq (ps:seq<Pnt> )  : BBox =
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
    static member createFromIList (ps:Collections.Generic.IList<Pnt> ) : BBox =
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
    static member createFromCenter (center:Pnt, sizeX, sizeY, sizeZ)  : BBox =
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
    static member createFromBRect minZ maxZ (r : BRect)  : BBox =
        if minZ>maxZ then
            fail $"BBox.createFromBRect: minZ > maxZ: {minZ} > {maxZ}"
        BBox.createUnchecked(r.MinX, r.MinY, minZ, r.MaxX, r.MaxY, maxZ)


    /// Creates a 3D bounding box from a Rect3D.
    static member createFromRect3D (r:Rect3D)  : BBox =
        let minX = r.OriginX + min 0. r.XaxisX + min 0. r.YaxisX
        let minY = r.OriginY + min 0. r.XaxisY + min 0. r.YaxisY
        let minZ = r.OriginZ + min 0. r.XaxisZ + min 0. r.YaxisZ
        let maxX = r.OriginX + max 0. r.XaxisX + max 0. r.YaxisX
        let maxY = r.OriginY + max 0. r.XaxisY + max 0. r.YaxisY
        let maxZ = r.OriginZ + max 0. r.XaxisZ + max 0. r.YaxisZ
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)


    /// Gets the world axis aligned 3D BoundingBox of the Box.
    static member createFromBox (b:Box) : BBox =
        // Each of the 8 corners is Origin plus any subset of the three axis
        // components, so min/max decompose per coordinate: sum the negative
        // parts for the min, the positive parts for the max.
        let minX = b.OriginX + min 0. b.XaxisX + min 0. b.YaxisX + min 0. b.ZaxisX
        let minY = b.OriginY + min 0. b.XaxisY + min 0. b.YaxisY + min 0. b.ZaxisY
        let minZ = b.OriginZ + min 0. b.XaxisZ + min 0. b.YaxisZ + min 0. b.ZaxisZ
        let maxX = b.OriginX + max 0. b.XaxisX + max 0. b.YaxisX + max 0. b.ZaxisX
        let maxY = b.OriginY + max 0. b.XaxisY + max 0. b.YaxisY + max 0. b.ZaxisY
        let maxZ = b.OriginZ + max 0. b.XaxisZ + max 0. b.YaxisZ + max 0. b.ZaxisZ
        BBox.createUnchecked(minX, minY, minZ, maxX, maxY, maxZ)


    // #endregion
    // #region Static members

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

    /// Returns a 3D bounding box moved by a vector. Same as BBox.translate.
    static member move (v:Vec) (b:BBox)  : BBox =
        BBox.createUnchecked(b.MinX+v.X, b.MinY+v.Y, b.MinZ+v.Z, b.MaxX+v.X, b.MaxY+v.Y, b.MaxZ+v.Z)

    /// Returns a 3D bounding box moved by a vector. Same as BBox.move.
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

    // #endregion
    // #region Obsolete

    [<Obsolete("use .isOverlapping instead")>]
    static member doOverlap(a:BBox) (b:BBox) : bool =
        b.IsOverlapping(a)

    [<Obsolete("use .isOverlappingOrClose instead")>]
    static member doOverlapMoreThan tol (a:BBox) (b:BBox)  : bool =
        b.IsOverlappingOrClose(a, -tol)

    [<Obsolete("use .isOverlapping instead")>]
    static member overlapsWith (a:BBox) (b:BBox) : bool =
        b.IsOverlapping(a)

    [<Obsolete("use .IsOverlapping instead")>]
    member  b.OverlapsWith (a:BBox): bool =
        b.IsOverlapping(a)

    [<Obsolete("use .IsOverlapping instead")>]
    member  b.OverlapsWith (a:BBox, tol:float): bool =
        b.IsOverlappingOrClose(a, tol)

    [<Obsolete("use .isOverlappingOrClose instead")>]
    static member  overlapsWithTol tol (a:BBox) (b:BBox) : bool =
        b.IsOverlappingOrClose(a, -tol)

    [<Obsolete("use SizeX")>]
    member b.Width : float =
        b.MaxX - b.MinX

    [<Obsolete("use SizeY")>]
    member b.Depth : float =
        b.MaxY - b.MinY

    [<Obsolete("use SizeZ")>]
    member b.Height3D : float =
        b.MaxZ - b.MinZ

    [<Obsolete("typo, use ExpandSafe instead")>]
    member b.ExpandSave(xDist, yDist, zDist) : BBox =
        b.ExpandSafe(xDist, yDist, zDist)

    [<Obsolete("typo, use ExpandSafe instead")>]
    member b.ExpandSave(dist) : BBox =
        b.ExpandSafe(dist, dist, dist)


    // [<Obsolete("Use .AsBRect instead")>]
    // member b.asBRect : BRect =
    //     b.AsBRect

    [<Obsolete("Use .asBRect instead")>]
    static member toBRect (b:BBox) : BRect =
        BBox.asBRect b

    [<Obsolete("typo, use expandSafe instead")>]
    static member expandSave dist (b:BBox) : BBox =
        b.ExpandSafe dist

    [<Obsolete("use .UnionPnt instead")>]
    member inline b.Union (p:Pnt) : BBox =
        b.UnionPnt p

    [<Obsolete("use unionPnt instead")>]
    static member inline unionPt (p:Pnt) (b:BBox) : BBox =
        b.UnionPnt p


    // /// Returns TRUE if the two 3D bounding boxes overlap by at least the specified threshold on every axis.
    // /// (See note: when one box is smaller than the threshold on some axis but fully inside the other,
    // ///  this returns FALSE.)
    // member inline b.IsOverlappingMoreThan (a:BBox, threshold:float) : bool =
    //     (min a.MaxX b.MaxX) - (max a.MinX b.MinX) >= threshold &&
    //     (min a.MaxY b.MaxY) - (max a.MinY b.MinY) >= threshold &&
    //     (min a.MaxZ b.MaxZ) - (max a.MinZ b.MinZ) >= threshold

    // /// Returns TRUE if the two 3D bounding boxes overlap by at least the specified threshold on every axis.
    // /// Also returns TRUE if one box is completely inside the other (even if smaller than the threshold).
    // /// Also returns TRUE if one box is completely surrounding the other.
    // member inline b.IsOverlappingMoreThan (a:BBox, threshold:float) : bool =
    //     (   (min a.MaxX b.MaxX) - (max a.MinX b.MinX) >= threshold &&
    //         (min a.MaxY b.MaxY) - (max a.MinY b.MinY) >= threshold &&
    //         (min a.MaxZ b.MaxZ) - (max a.MinZ b.MinZ) >= threshold    )
    //     ||
    //     (   a.MinX >= b.MinX && a.MaxX <= b.MaxX &&
    //         a.MinY >= b.MinY && a.MaxY <= b.MaxY &&
    //         a.MinZ >= b.MinZ && a.MaxZ <= b.MaxZ    )
    //     ||
    //     (   b.MinX >= a.MinX && b.MaxX <= a.MaxX &&
    //         b.MinY >= a.MinY && b.MaxY <= a.MaxY &&
    //         b.MinZ >= a.MinZ && b.MaxZ <= a.MaxZ    )

