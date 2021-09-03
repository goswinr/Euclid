namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Util

(*
   local     7                                          6                       
   Z-Axis    +-----------------------------------------+                
   ^        /|                                        /|                
   |       / |                                       / |                
   |      /  |                                      /  |                
   |     /   |                                     /   |                
   |    /    |                                    /    |                
   |   /     |                                   /     |                
   |  /      |                                  /      |                
   | /       |                                 /       |                
   |/        |                              5 /        |                
 4 +-----------------------------------------+         |                
   |         |      local                    |         |                
   |         |      Y-Axis                   |         |                
   |         |     /                         |         |                
   |         |    /                          |         | 
   |         |   /                           |         |                
   |         |  /                            |         |                
   |         | /                             |         |                
   |       3 |/                              |         |                
   |         +-------------------------------|---------+ 2              
   |        /                                |        /                 
   |       /                                 |       /                  
   |      /                                  |      /                   
   |     /                                   |     /                    
   |    /                                    |    /                     
   |   /                                     |   /                      
   |  /                                      |  /                       
   | /                                       | /                        
   |/                                        |/                   local 
   +-----------------------------------------+------------------> X-Axis
  0                                           1  

*)


/// A immutable 3D Box with any rotation in 3D space
/// Described by an Origin and three Edge vectors. 
/// Similar to PPlane, however the three vectors are not unitized.
/// The X, Y and Z axes are also called Length, Width and Height.
/// 
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Box =  
    
    /// The Origin Corner of the Box.
    val Origin: Pnt

    /// The Edge vector representing the X-axis of the Box.
    /// Also called Length
    val Xaxis: Vec 

    /// The Edge vector representing the Y-axis of the Box.
    /// Also called Width
    val Yaxis: Vec 

    /// The Edge vector representing the Z-axis of the Box.
    /// also called Height
    val Zaxis: Vec 
    
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

    /// Creates a unitized version of the local X-Axis
    member b.XaxisUnit = b.Xaxis.Unitized
    
    /// Creates a unitized version of the local Y-Axis
    member b.YaxisUnit = b.Yaxis.Unitized

    /// Creates a unitized version of the local Z-Axis
    member b.ZaxisUnit = b.Zaxis.Unitized

    /// The corner diagonally opposite of corner from Origin.
    member inline b.FarCorner = b.Origin + b.Xaxis + b.Yaxis + b.Zaxis

    /// The diagonal vector of the Box.
    member inline b.Diagonal = b.Xaxis + b.Yaxis + b.Zaxis
    
    /// The center of the Box.
    member inline b.Center = b.Origin + b.Xaxis*0.5 + b.Yaxis*0.5 + b.Zaxis*0.5

    /// Evaluates a point of the Box.
    /// Considering the box start 0.0 and its opposite corners 1.0
    member b.EvaluateAt (x:float, y:float, z:float) = b.Origin + b.Xaxis*x + b.Yaxis*y + b.Zaxis*z
    
    /// Nicely formatted string representation of the Box including its size.
    override b.ToString() = 
        sprintf "FsEx.Geo.Box %s x %s x %s (Origin:%s| X-ax:%s| Y-ax:%s| Z-ax:%s)" 
            (Format.float b.Length)  (Format.float b.Width) (Format.float b.Height) 
            b.Origin.AsString b.Xaxis.AsString b.Yaxis.AsString b.Zaxis.AsString
    
    
    /// Format Box into string with nice floating point number formatting of X,Y and Z size only
    /// But without type name as in v.ToString()
    member b.AsString = sprintf "%s x %s x %s" (Format.float b.Length)  (Format.float b.Width) (Format.float b.Height) 
    
    /// Returns the bottom corners of the Box in counter clockwise order, starting at Origin
    /// Then the top corners staring above Origin. Returns an array of 8 Points 
    /// 
    ///      7              6                                                                 
    ///       +------------+                                                                 
    ///      /|           /|                                                                 
    ///     / |          / |                                                                 
    ///  4 /  |       5 /  |                                                                 
    ///   +------------+   |                                                                 
    ///   |   |        |   |                                                                 
    ///   |   +--------|---+                                                                 
    ///   |  / 3       |  / 2                                                                
    ///   | /          | /                                                                   
    ///   |/           |/                                                                    
    ///   +------------+                                                                     
    ///   0             1 
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
    
    //member b.Faces = [b.Origin, b.Origin + b.Xaxis, b.Origin + b.Yaxis, b.Origin + b.Zaxis, b.Origin + b.Xaxis + b.Yaxis, b.Origin + b.Xaxis + b.Zaxis, b.Origin + b.Yaxis + b.Zaxis, b.Origin + b.Xaxis + b.Yaxis + b.Zaxis]
    
    /// Evaluate a X,Y and Z parameter of the  Box.
    ///  0.0, 0.0, 0.0 returns the Origin.
    ///  1.0, 1.0, 1.0 returns the FarCorner.
    member inline b.Evaluate (xParameter,yParameter,zParameter) =
        b.Origin + b.Xaxis * xParameter + b.Yaxis * yParameter + b.Zaxis * zParameter


     /// Calculates the volume of the Box.
    member inline b.Volume  =  
        b.Xaxis.Length*b.Yaxis.Length*b.Zaxis.Length 

    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------


    /// Returns Box expanded by distance on all six sides.    
    /// Does check for underflow if distance is negative and raises FsExGeoException.
    static member expand dist (b:Box) = 
        let len = b.Length 
        let wid = b.Width
        let hei = b.Height
        let d = dist * -2.0
        if len<=d || wid<=d || hei<=d then
            FsExGeoException.Raise "FsEx.Geo.Box.expand: Box %s is too small to expand by negative distance %s"  b.AsString (Format.float dist) 
        let x = b.Xaxis * (dist / len)
        let y = b.Yaxis * (dist / wid)
        let z = b.Zaxis * (dist / hei)        
        Box(b.Origin-x-y-z, b.Xaxis+x*2., b.Yaxis+y*2., b.Zaxis+z*2.)

    /// Returns Box expanded by respective distances on all six sides
    /// Does check for overflow if distance is negative and fails.
    /// distLen, distWid and  distHei are for x, y and Z axis respectively.
    static member expandXYZ distLen distWid distHei (b:Box) = 
        let len = b.Length 
        let wid = b.Width
        let hei = b.Height
        if len <= distLen * -2.0 then FsExGeoException.Raise "FsEx.Geo.Box.expandXYZ: Box %s is too small to expand by negative distance distLen %s"  b.AsString (Format.float distLen) 
        if wid <= distWid * -2.0 then FsExGeoException.Raise "FsEx.Geo.Box.expandXYZ: Box %s is too small to expand by negative distance distWid %s"  b.AsString (Format.float distWid) 
        if hei <= distHei * -2.0 then FsExGeoException.Raise "FsEx.Geo.Box.expandXYZ: Box %s is too small to expand by negative distance distHei %s"  b.AsString (Format.float distHei)         
        let x = b.Xaxis * (distLen / b.Length)
        let y = b.Yaxis * (distWid / b.Width )
        let z = b.Zaxis * (distHei / b.Height)        
        Box(b.Origin-x-y-z, b.Xaxis+x*2., b.Yaxis+y*2., b.Zaxis+z*2.)

    /// Give PPlane and size
    static member createFromPlane (pl:PPlane,x,y,z) = 
        Box(pl.Origin, pl.Xaxis*x, pl.Yaxis*y, pl.Zaxis*z)

    /// Give PPlane and size
    static member createFromBoundingBox (b:BBox) = 
        Box(b.MinPnt, Vec.XAxis*b.Length, Vec.YAxis*b.Width, Vec.ZAxis*b.Height)

    /// Move the  Box by a vector.
    static member translate (v:Vec) (pl:Box) = Box(pl.Origin + v, pl.Xaxis, pl.Yaxis, pl.Zaxis) 
    
    /// Translate along the local X-axis of the Box
    static member translateX (distX:float) (b:Box) = 
        let x = b.Xaxis
        let len = x.Length
        if len = zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Box.translateX: box.Xaxis is zero length in Box: %s" b.AsString
        Box(b.Origin + x*(distX/len), x, b.Yaxis, b.Zaxis) 
    
    /// Translate along the local Y-axis of the Box
    static member translateY (distY:float) (b:Box) = 
        let y = b.Yaxis
        let len = y.Length
        if len = zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Box.translateY: box.Yaxis is zero length in Box: %s" b.AsString
        Box(b.Origin + y*(distY/len), b.Xaxis, y, b.Zaxis)     
    
    /// Translate along the local Z-axis of the Box
    static member translateZ (distZ:float) (b:Box) = 
        let z = b.Zaxis
        let len = z.Length
        if len = zeroLengthTol then FsExGeoException.Raise "FsEx.Geo.Box.translateZ: box.Zaxis is zero length in Box: %s" b.AsString
        Box(b.Origin + z*(distZ/len), b.Xaxis, b.Yaxis, z)

    /// Transform the Box by the given matrix.
    /// If the transformation includes a shear it fails.
    /// The returned Box is guaranteed to have orthogonal vectors.
    static member transform (m:Matrix) (b:Box) = 
        let o  = Pnt.transform m b.Origin
        let x = Vec.transformBy3x3PartOnly m b.Xaxis
        let y = Vec.transformBy3x3PartOnly m b.Yaxis
        let z = Vec.transformBy3x3PartOnly m b.Zaxis
        if abs(x*y) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Box.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        if abs(x*z) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Box.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        if abs(y*z) > zeroLengthTol then  FsExGeoException.Raise "FsEx.Geo.Box.transform failed because the edges are not perpendicular to each other anymore after transforming with:\r\n%O" m
        Box(o,x,y,z) 
    
