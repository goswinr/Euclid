namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Util

/// A 3D Box with any rotation in 3D space
/// Described by an Origin and three Edge vectors. 
/// Similar to PPlane, however these vectors are not unitized.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Box =  
    val Origin: Pnt
    val Xax: Vec 
    val Yax: Vec 
    val Zax: Vec 
    
    /// Create a Parametrized Plane with X, Y and Z Direction
    internal new (origin,axisX,axisY,axisZ)  =  {Origin=origin; Xax=axisX; Yax=axisY; Zax=axisZ}    

    /// The size in X direction 
    member inline b.Length = b.Xax.Length

    /// The size in Y direction 
    member inline b.Width  = b.Yax.Length

    /// The size in Z direction 
    member inline b.Height  = b.Zax.Length
    
    member inline b.FarCorner = b.Origin + b.Xax + b.Yax + b.Zax

    member inline b.Diagonal = b.Xax + b.Yax + b.Zax
    
    member inline b.Center = b.Origin + b.Xax*0.5 + b.Yax*0.5 + b.Zax*0.5

    /// Evaluates a point of the Box,
    /// considering the box start 0.0 and its opposite corners 1.0
    member b.EvaluateAt (x:float, y:float, z:float) = b.Origin + b.Xax*x + b.Yax*y + b.Zax*z
    
    override b.ToString() = 
        sprintf "FsEx.Geo.Box %s x %s x %s (Origin=%O; Xax=%O; Yax=%O; Yax=%O;)" 
            (Format.float b.Length)  (Format.float b.Width) (Format.float b.Height) 
            b.Origin b.Xax b.Yax b.Zax
    
    /// Returns Box expanded by distance
    /// Does not check overflow if distance is negative.
    static member expand dist (b:Box) = 
        let x = b.Xax * (dist / b.Length)
        let y = b.Yax * (dist / b.Width )
        let z = b.Zax * (dist / b.Height)
        let o = Pnt()
        Box(b.Origin-x-y-z, b.Xax+x, b.Yax+y, b.Zax+z)

    /// Returns Box expanded by distance
    /// Does not check overflow if distance is negative.
    static member expandXYZ distX distY distZ (b:Box) = 
        let x = b.Xax * (distX / b.Length)
        let y = b.Yax * (distY / b.Width )
        let z = b.Zax * (distZ / b.Height)
        let o = Pnt()
        Box(b.Origin-x-y-z, b.Xax+x, b.Yax+y, b.Zax+z)

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

    
    /// If the transformation includes a shear the only X-axis can be kept.
    /// The plane will be defined with the direction of Y-axis ( which might not be perpendicular after shearing ).
    /// The returned Box has orthogonal unit vectors
    static member transform (m:Matrix) (b:Box) = 
        let o  = Pnt.transform m b.Origin
        let x = Vec.transform m b.Xax
        let y = Vec.transform m b.Yax
        let z = Vec.transform m b.Zax
        if abs(x*y) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Box.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        if abs(x*z) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Box.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        if abs(y*z) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Box.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        Box(o,x,y,z) 
    
