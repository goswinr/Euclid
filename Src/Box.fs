namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Util

/// A immutable 3D Box with any rotation in 3D space
/// Described by an Origin and three Edge vectors. 
/// Similar to PPlane, however the three vectors are not unitized.
/// The X, Y and Z axes are also called Length, Width and Height.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Box =  
    
    /// The Origin Corner of the Box.
    val Origin: Pnt

    /// The Edge vector representing the X-axis of the Box.
    /// Also called Length
    val Xax: Vec 

    /// The Edge vector representing the Y-axis of the Box.
    /// Also called Width
    val Yax: Vec 

    /// The Edge vector representing the Z-axis of the Box.
    /// also called Height
    val Zax: Vec 
    
    /// Create a Parametrized Plane with X, Y and Z Direction.
    internal new (origin,axisX,axisY,axisZ)  =  {Origin=origin; Xax=axisX; Yax=axisY; Zax=axisZ}    

    /// The size in X direction. 
    member inline b.Length = b.Xax.Length

    /// The size in Y direction.
    member inline b.Width  = b.Yax.Length

    /// The size in Z direction. 
    member inline b.Height  = b.Zax.Length
    
    /// The corner diagonally opposite of corner from Origin.
    member inline b.FarCorner = b.Origin + b.Xax + b.Yax + b.Zax

    /// The diagonal vector of the Box.
    member inline b.Diagonal = b.Xax + b.Yax + b.Zax
    
    /// The center of the Box.
    member inline b.Center = b.Origin + b.Xax*0.5 + b.Yax*0.5 + b.Zax*0.5

    /// Evaluates a point of the Box.
    /// Considering the box start 0.0 and its opposite corners 1.0
    member b.EvaluateAt (x:float, y:float, z:float) = b.Origin + b.Xax*x + b.Yax*y + b.Zax*z
    
    /// Nicely formatted string representation of the Box including its size.
    override b.ToString() = 
        sprintf "FsEx.Geo.Box %s x %s x %s (Origin:%s| X-ax:%s| Y-ax:%s| Z-ax:%s)" 
            (Format.float b.Length)  (Format.float b.Width) (Format.float b.Height) 
            b.Origin.AsShortString b.Xax.AsShortString b.Yax.AsShortString b.Zax.AsShortString
    
    
    /// Format Box into string with nice floating point number formatting of X,Y and Z size only
    /// But without type name as in v.ToString()
    member b.AsShortString = sprintf "%s x %s x %s" (Format.float b.Length)  (Format.float b.Width) (Format.float b.Height) 
    
    /// Returns Box expanded by distance on all six sides.
    /// Does check for overflow if distance is negative and fails.
    static member expand dist (b:Box) = 
        let len = b.Length 
        let wid = b.Width
        let hei = b.Height
        let d = dist * -2.0
        if len<=d || wid<=d || hei<=d then
            FsExGeoException.Raise "FsEx.Geo.Box.expand: Box %s is too small to expand by negative distance %s"  b.AsShortString (Format.float dist) 
        let x = b.Xax * (dist / len)
        let y = b.Yax * (dist / wid)
        let z = b.Zax * (dist / hei)        
        Box(b.Origin-x-y-z, b.Xax+x*2., b.Yax+y*2., b.Zax+z*2.)

    /// Returns Box expanded by respective distances on all six sides
    /// Does check for overflow if distance is negative and fails.
    /// distLen, distWid and  distHei are for x, y and Z axis respectively.
    static member expandXYZ distLen distWid distHei (b:Box) = 
        let len = b.Length 
        let wid = b.Width
        let hei = b.Height
        if len<=distLen * -2.0 then FsExGeoException.Raise "FsEx.Geo.Box.expandXYZ: Box %s is too small to expand by negative distance distLen %s"  b.AsShortString (Format.float distLen) 
        if wid<=distWid * -2.0 then FsExGeoException.Raise "FsEx.Geo.Box.expandXYZ: Box %s is too small to expand by negative distance distWid %s"  b.AsShortString (Format.float distWid) 
        if hei<=distHei * -2.0 then FsExGeoException.Raise "FsEx.Geo.Box.expandXYZ: Box %s is too small to expand by negative distance distHei %s"  b.AsShortString (Format.float distHei)         
        let x = b.Xax * (distLen / b.Length)
        let y = b.Yax * (distWid / b.Width )
        let z = b.Zax * (distHei / b.Height)        
        Box(b.Origin-x-y-z, b.Xax+x*2., b.Yax+y*2., b.Zax+z*2.)

    /// Give PPlane and size
    static member createFromPlane (pl:PPlane,x,y,z) = 
        Box(pl.Origin, pl.Xax*x, pl.Yax*y, pl.Zax*z)

    /// Give PPlane and size
    static member createFromBoundingBox (b:BBox) = 
        Box(b.MinPnt, Vec.XAxis*b.Length, Vec.YAxis*b.Width, Vec.ZAxis*b.Height)

    static member translate (v:Vec) (pl:Box) = Box(pl.Origin + v, pl.Xax, pl.Yax, pl.Zax) 
    
    /// Translate along the local X-axis of the Box
    static member translateX (d:float) (pl:Box) = Box(pl.Origin + pl.Xax*d, pl.Xax, pl.Yax, pl.Zax) 
    
    /// Translate along the local Y-axis of the Box
    static member translateY (d:float) (pl:Box) = Box(pl.Origin + pl.Yax*d, pl.Xax, pl.Yax, pl.Zax)     
    
    /// Translate along the local Z-axis of the Box
    static member translateZ (d:float) (pl:Box) = Box(pl.Origin + pl.Zax*d, pl.Xax, pl.Yax, pl.Zax)

    /// Transform the Box by the given matrix.
    /// If the transformation includes a shear it fails.
    /// The returned Box is guaranteed to have orthogonal vectors.
    static member transform (m:Matrix) (b:Box) = 
        let o  = Pnt.transform m b.Origin
        let x = Vec.transformBy3x3PartOnly m b.Xax
        let y = Vec.transformBy3x3PartOnly m b.Yax
        let z = Vec.transformBy3x3PartOnly m b.Zax
        if abs(x*y) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Box.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        if abs(x*z) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Box.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        if abs(y*z) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Box.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        Box(o,x,y,z) 
    
