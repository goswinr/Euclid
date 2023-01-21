namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Util
open AutoOpenPt
open AutoOpenUnitVec
open AutoOpenUnitVec
open AutoOpenUnitVc

/// An immutable 3D Box with any rotation in 3D space.
/// Described by an Origin and three Edge vectors.
/// Similar to PPlane, however the three vectors are not unitized.
/// The X, Y and Z axes are also called Length, Width and Height.
/// This implementation guarantees the box to be always valid.
/// That means the Min X, Y and Z axes cannot be flipped individually.
/// However the length of one of these axes might still be zero.
///
///   local        local
///   Z-Axis       Y-Axis
///   ^           /
///   |   7      /        6
///   |   +---------------+
///   |  /|    /         /|
///   | / |   /         / |
/// 4 |/  |  /       5 /  |
///   +---------------+   |
///   |   |/          |   |
///   |   +-----------|---+
///   |  / 3          |  / 2
///   | /             | /
///   |/              |/     local
///   +---------------+----> X-Axis
///   0               1
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Box =

    /// The Origin Corner of the Box.
    val Origin: Pnt

    /// The Edge vector representing the X-axis of the Box.
    /// Also called Length.
    val Xaxis: Vec

    /// The Edge vector representing the Y-axis of the Box.
    /// Also called Width.
    val Yaxis: Vec

    /// The Edge vector representing the Z-axis of the Box.
    /// Also called Height.
    val Zaxis: Vec

    /// Unchecked Internal Constructor Only.
    /// Create a Parametrized Plane with X, Y and Z Direction.
    internal new (origin,axisX,axisY,axisZ)  =  {Origin=origin; Xaxis=axisX; Yaxis=axisY; Zaxis=axisZ}

    /// The size in X direction, same as member box.SizeX.
    member inline b.Length = b.Xaxis.Length

    /// The size in X direction, same as member box.Length.
    member inline b.SizeX = b.Xaxis.Length

    /// The size in Y direction, same as member box.SizeY.
    member inline b.Width  = b.Yaxis.Length

    /// The size in Y direction, same as member box.Width.
    member inline b.SizeY  = b.Yaxis.Length

    /// The size in Z direction, same as member box.SizeZ.
    member inline b.Height  = b.Zaxis.Length

    /// The size in Z direction, same as member box.Height.
    member inline b.SizeZ  = b.Zaxis.Length


    /// Returns point 0 of the box, same box.Origin.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt0  = b.Origin

    /// Returns point 1 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt1  = b.Origin + b.Xaxis

    /// Returns point 2 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt2  = b.Origin + b.Xaxis + b.Yaxis

    /// Returns point 3 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt3  = b.Origin + b.Yaxis

    /// Returns point 4 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt4  = b.Origin + b.Zaxis

    /// Returns point 5 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt5  = b.Origin + b.Xaxis + b.Zaxis

    /// Returns point 6 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt6  = b.Origin + b.Xaxis + b.Yaxis + b.Zaxis

    /// Returns point 7 of the box.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member inline b.Pt7  = b.Origin + b.Yaxis + b.Zaxis

    /// Creates a unitized version of the local X-Axis.
    member inline r.XaxisUnit =
        let len = r.Xaxis.Length
        if len = zeroLengthTol then EuclidException.Raise "Euclid.Box.XaxisUnit: rect Xaxis is too small for unitizing: %s" r.AsString
        r.Xaxis*(1./len)

    /// Creates a unitized version of the local Y-Axis.
    member inline r.YaxisUnit =
        let len = r.Yaxis.Length
        if len = zeroLengthTol then EuclidException.Raise "Euclid.Box.XaxisUnit: rect Yaxis is too small for unitizing: %s" r.AsString
        r.Yaxis*(1./len)

    /// Creates a unitized version of the local Z-Axis.
    member inline r.ZaxisUnit =
        let len = r.Zaxis.Length
        if len = zeroLengthTol then EuclidException.Raise "Euclid.Box.XaxisUnit: rect Zaxis is too small for unitizing: %s" r.AsString
        r.Zaxis*(1./len)

    /// The corner diagonally opposite of corner from Origin.
    member inline b.FarCorner = b.Origin + b.Xaxis + b.Yaxis + b.Zaxis

    /// The diagonal vector of the Box.
    member inline b.Diagonal = b.Xaxis + b.Yaxis + b.Zaxis

    /// The center of the Box.
    member inline b.Center = b.Origin + b.Xaxis*0.5 + b.Yaxis*0.5 + b.Zaxis*0.5

    /// Nicely formatted string representation of the Box including its size.
    override b.ToString() =
        sprintf "Euclid.Box %s x %s x %s (Origin:%s| X-ax:%s| Y-ax:%s| Z-ax:%s)"
            (Format.float b.Length)  (Format.float b.Width) (Format.float b.Height)
            b.Origin.AsString b.Xaxis.AsString b.Yaxis.AsString b.Zaxis.AsString


    /// Format Box into string with nice floating point number formatting of X, Y and Z size only.
    /// But without type name as in v.ToString()
    member b.AsString = sprintf "%s x %s x %s" (Format.float b.Length)  (Format.float b.Width) (Format.float b.Height)

    /// Returns the bottom corners of the Box in counter clockwise order, starting at Origin.
    /// Then the top corners staring above Origin. Returns an array of 8 Points.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.Corners :Pnt[] =
        let p0 = b.Origin
        let p1 = p0 + b.Xaxis
        let p4 = p0 + b.Zaxis
        let p5 = p4 + b.Xaxis
        [|
            p0
            p1
            p1 + b.Yaxis
            p0 + b.Yaxis
            p4
            p5
            p5 + b.Yaxis
            p4 + b.Yaxis
        |]

    /// Returns the top face of the Box in counter clockwise order, looking from above.
    /// Returns Origin at point 4, X-Axis at point 5, Y-Axis at point 7.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.TopFace :Rect3D = Rect3D(b.Origin + b.Zaxis , b.Xaxis, b.Yaxis)


    /// Returns the bottom face of the Box in counter clockwise order, looking from above.
    /// Returns Origin at point 0, X-Axis at point 1, Y-Axis at point 3.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.BottomFace  =  Rect3D(b.Origin , b.Xaxis, b.Yaxis)



    /// Returns the front face of the Box in counter clockwise order, looking from front.
    /// Returns Origin at point 0, X-Axis at point 1, Y-Axis at point 4.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.FrontFace  =  Rect3D(b.Origin , b.Xaxis, b.Zaxis)

    /// Returns the back face of the Box in counter clockwise order, looking from front.
    /// Returns Origin at point 3, X-Axis at point 2, Y-Axis at point 7.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.BackFace  =  Rect3D(b.Origin + b.Yaxis, b.Xaxis, b.Zaxis)

    /// Returns the right face of the Box in counter clockwise order, looking from right.
    /// Returns Origin at point 1, X-Axis at point 2, Y-Axis at point 5.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.RightFace  =  Rect3D(b.Origin + b.Xaxis, b.Yaxis, b.Zaxis)

    /// Returns the right face of the Box in counter clockwise order, looking from right.
    /// Returns Origin at point 1, X-Axis at point 2, Y-Axis at point 5.
    ///
    ///   local        local
    ///   Z-Axis       Y-Axis
    ///   ^           /
    ///   |   7      /        6
    ///   |   +---------------+
    ///   |  /|    /         /|
    ///   | / |   /         / |
    /// 4 |/  |  /       5 /  |
    ///   +---------------+   |
    ///   |   |/          |   |
    ///   |   +-----------|---+
    ///   |  / 3          |  / 2
    ///   | /             | /
    ///   |/              |/     local
    ///   +---------------+----> X-Axis
    ///   0               1
    member b.LeftFace  =  Rect3D(b.Origin + b.Xaxis, b.Yaxis, b.Zaxis)

    /// Evaluate a X, Y and Z parameter of the Box.
    ///  0.0, 0.0, 0.0 returns the Origin.
    ///  1.0, 1.0, 1.0 returns the FarCorner.
    member inline b.EvaluateAt (xParameter:float,yParameter:float,zParameter:float) =
        b.Origin + b.Xaxis * xParameter + b.Yaxis * yParameter + b.Zaxis * zParameter


    /// Calculates the volume of the Box.
    member inline b.Volume  =
        b.Xaxis.Length*b.Yaxis.Length*b.Zaxis.Length

    /// Gets the Plane that this box is based on.
    member inline b.Plane  =
        let x = b.Xaxis.Unitized
        let y = b.Yaxis.Unitized
        let z = b.Zaxis.Unitized
        PPlane.createUnchecked (b.Origin, x,y,z)

    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------

    /// Checks if two 3D Boxes are equal within tolerance.
    /// Does not recognize congruent boxes with different rotation as equal.
    static member equals tol (a:Box) (b:Box) =
        let tt = tol*tol
        Pnt.distanceSq a.Origin b.Origin < tt &&
        Vec.differenceSq a.Xaxis b.Xaxis < tt &&
        Vec.differenceSq a.Yaxis b.Yaxis < tt &&
        Vec.differenceSq a.Zaxis b.Zaxis < tt

    /// Returns Box expanded by distance on all six sides.
    /// Does check for underflow if distance is negative and raises EuclidException.
    static member expand dist (b:Box) =
        let len = b.Length
        let wid = b.Width
        let hei = b.Height
        let d = dist * -2.0
        if len<=d || wid<=d || hei<=d then
            EuclidException.Raise "Euclid.Box.expand: Box %s is too small to expand by negative distance %s"  b.AsString (Format.float dist)
        let x = b.Xaxis * (dist / len)
        let y = b.Yaxis * (dist / wid)
        let z = b.Zaxis * (dist / hei)
        Box(b.Origin-x-y-z, b.Xaxis+x*2., b.Yaxis+y*2., b.Zaxis+z*2.)

    /// Returns Box expanded by respective distances on all six sides.
    /// Does check for overflow if distance is negative and fails.
    /// distLen, distWid and distHei are for X, Y and Z-axis respectively.
    static member expandXYZ distLen distWid distHei (b:Box) =
        let len = b.Length
        let wid = b.Width
        let hei = b.Height
        if len <= distLen * -2.0 then EuclidException.Raise "Euclid.Box.expandXYZ: Box %s is too small to expand by negative distance distLen %s"  b.AsString (Format.float distLen)
        if wid <= distWid * -2.0 then EuclidException.Raise "Euclid.Box.expandXYZ: Box %s is too small to expand by negative distance distWid %s"  b.AsString (Format.float distWid)
        if hei <= distHei * -2.0 then EuclidException.Raise "Euclid.Box.expandXYZ: Box %s is too small to expand by negative distance distHei %s"  b.AsString (Format.float distHei)
        let x = b.Xaxis * (distLen / b.Length)
        let y = b.Yaxis * (distWid / b.Width )
        let z = b.Zaxis * (distHei / b.Height)
        Box(b.Origin-x-y-z, b.Xaxis+x*2., b.Yaxis+y*2., b.Zaxis+z*2.)

    /// Give PPlane and x, y and Z size.
    static member createFromPlane (pl:PPlane, x, y, z) =
        Box(pl.Origin, pl.Xaxis*x, pl.Yaxis*y, pl.Zaxis*z)

    /// Give 3D Bounding Box.
    static member createFromBoundingBox (b:BBox) =
        Box(b.MinPnt, Vec.Xaxis*b.Length, Vec.Yaxis*b.Width, Vec.Zaxis*b.Height)

    /// Give 2D Rectangle and Z lower and upper position.
    static member createFromRect2D (r:Rect2D, zLow, zHigh) =
        Box(r.Origin.WithZ zLow, 
            r.Xaxis.AsVec,
            r.Yaxis.AsVec, 
            Vec.Zaxis*(zHigh-zLow))

     /// Give 2D Rectangle and Z lower and upper position.
    static member createFromRect3D (r:Rect3D, zLow, zHigh) =
        let z = Vec.cross(r.Xaxis,r.Yaxis)
        Box(r.Origin  + z.WithLength(zLow), 
            r.Xaxis,
            r.Yaxis, 
            z.WithLength(zHigh-zLow)
            ) 

    /// Move the Box by a vector.
    static member move (v:Vec) (b:Box) =
        Box(b.Origin + v, b.Xaxis, b.Yaxis, b.Zaxis)

    /// Translate along the local X-axis of the Box.
    static member translateX (distX:float) (b:Box) =
        let x = b.Xaxis
        let len = x.Length
        if len = zeroLengthTol then EuclidException.Raise "Euclid.Box.translateX: box.Xaxis is zero length in Box: %s" b.AsString
        Box(b.Origin + x*(distX/len), x, b.Yaxis, b.Zaxis)

    /// Translate along the local Y-axis of the Box.
    static member translateY (distY:float) (b:Box) =
        let y = b.Yaxis
        let len = y.Length
        if len = zeroLengthTol then EuclidException.Raise "Euclid.Box.translateY: box.Yaxis is zero length in Box: %s" b.AsString
        Box(b.Origin + y*(distY/len), b.Xaxis, y, b.Zaxis)

    /// Translate along the local Z-axis of the Box.
    static member translateZ (distZ:float) (b:Box) =
        let z = b.Zaxis
        let len = z.Length
        if len = zeroLengthTol then EuclidException.Raise "Euclid.Box.translateZ: box.Zaxis is zero length in Box: %s" b.AsString
        Box(b.Origin + z*(distZ/len), b.Xaxis, b.Yaxis, z)

    /// Transform the Box by the given OrthoMatrix.
    /// The returned Box is guaranteed to have orthogonal vectors.
    static member transform (m:OrthoMatrix) (b:Box) =
        let o = Pnt.transformOrtho m b.Origin
        let x = Vec.rotateOrtho m b.Xaxis
        let y = Vec.rotateOrtho m b.Yaxis
        let z = Vec.rotateOrtho m b.Zaxis
        Box(o,x,y,z)

