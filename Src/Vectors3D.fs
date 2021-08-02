namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open FsEx.Geo.Util    
    
/// A 3D Vector with any length. Made up from 3 floats: X, Y, and Z.
/// (Unit vectors with length 1.0 are called 'UnitVec')
/// (2D Vectors are called 'Vc') 
[<Struct; NoEquality; NoComparison>] 
[<IsReadOnly>]
//[<IsByRefLike>]
type Vec = 
    val X : float
    val Y : float 
    val Z : float
    
    /// Create a new 3D Vector with any length. Made up from 3 floats: X, Y, and Z.
    new (x,y,z) =
        #if DEBUG
        if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then FsExGeoException.Raise "FsEx.Geo.Vec Constructor failed for x:%g , y:%g, z:%g"  x y z
        #endif
        {X=x; Y=y; Z=z}
        
    /// Format 3D vector into string including type name and nice floating point number formatting.
    override p.ToString() = sprintf "FsEx.Geo.Vec(X=%s, Y=%s, Z=%s)" (Format.float p.X) (Format.float p.Y) (Format.float p.Z)
    
    /// Negate or inverse a 3D vectors. Returns a new 3D vector.
    static member inline (~- ) (v:Vec)          = Vec( -v.X , -v.Y , -v.Z)
    
    /// Subtract one 3D vectors from another. Returns a new 3D vector.
    static member inline ( - ) (a:Vec, b:Vec)   = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)            

    /// Add two 3D vectors together. Returns a new 3D vector.
    static member inline ( + ) (a:Vec, b:Vec)   = Vec (a.X + b.X , a.Y + b.Y , a.Z + b.Z)  

    /// Multiplies a 3D vector with a scalar, also called scaling a vector. Returns a new 3D vector.
    static member inline ( * ) (a:Vec  , f:float) = Vec (a.X * f , a.Y * f , a.Z * f) 

    /// Multiplies a scalar with a 3D vector, also called scaling a vector. Returns a new 3D vector. 
    static member inline ( * ) (f:float, a:Vec  ) = Vec (a.X * f , a.Y * f , a.Z * f) 
    
    /// Dot product, or scalar product of two 3D vectors. Returns a float.
    static member inline ( * ) (a:Vec  , b:Vec  ) = a.X * b.X + a.Y * b.Y + a.Z * b.Z             

    /// Divides a 3D vector by a scalar, also be called dividing/scaling a vector. Returns a new 3D vector.
    static member inline ( / )  (v:Vec, f:float) = 
        #if DEBUG
        if abs f < Util.zeroLengthTol then  FsExGeoDivByZeroException.Raise "%g is too small for dividing %O using / operator. Tolerance:%g" f v zeroLengthTol
        #endif
        v * (1./f) // or Vec (v.X / f , v.Y / f , , v.Z / f) ? 
    
    
#nowarn "44" // for hidden constructors via Obsolete Attribute    
        
/// A 3D Vector guaranteed to be unitized. (2D Unit Vectors are called 'UnitVc') 
/// This type shall only be created by unitizing a  3D vector (Vec)
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
type UnitVec =  
    val X : float
    val Y : float 
    val Z : float    
    
    /// Unsafe internal constructor,  public only for inlining.
    [<Obsolete("Unsafe internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (x,y,z) = 
        #if DEBUG
        let l = x*x + y*y + z*z // TODO : with this test all  operations are 2.5 times slower  
        if 0.999999999 > l || l > 1.000000001 then  FsExGeoException.Raise "FsEx.Geo.UnitVc Constructor failed for x:%g, y:%g, z:%g. The length needs to be 1.0."  x y z
        #endif
        {X=x; Y=y; Z=z}
        
    override p.ToString() = sprintf "FsEx.Geo.UnitVec(X=%s, Y=%s, Z=%s)" (Format.float p.X)(Format.float p.Y)(Format.float p.Z)
    
    static member inline ( ~- ) (v:UnitVec)            = UnitVec ( -v.X , -v.Y , -v.Z)        
    static member inline ( - )  (a:UnitVec, b:UnitVec) = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)            
    
    static member inline ( + )  (a:UnitVec, b:UnitVec) = Vec (a.X + b.X , a.Y + b.Y , a.Z + b.Z)  

    static member inline ( * )  (a:UnitVec, f:float  ) = Vec (a.X * f , a.Y * f , a.Z * f) // scale Vector
    static member inline ( * )  (f:float,   a:UnitVec) = Vec (a.X * f , a.Y * f , a.Z * f) // scale Vector
    static member inline ( * )  (a:UnitVec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z // dot product
    static member inline ( * )  (a:Vec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z // dot product for projected length
    static member inline ( * )  (a:UnitVec, b:Vec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z // dot product for projected length
    
    static member inline ( / )  (v:Vec, f:float) = 
        #if DEBUG
        if abs f < Util.zeroLengthTol then FsExGeoDivByZeroException.Raise "%g is too small for dividing %O using / operator. Tolerance:%g" f v zeroLengthTol
        #endif
        v * (1./f) // or Vec (v.X / f , v.Y / f , , v.Z / f) ?
    
    /// Requires correct input of unitized values
    static member inline createUnchecked(x,y,z)  = UnitVec(x,y,z)

    /// Does the unitizing too.
    static member inline create (x:float, y:float, z:float) = 
        // this member cant be an extension method because it is used with SRTP. 
        // see error FS1114: The value 'FsEx.Geo.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x*x  + y*y + z*z)                       
        if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "UnitVec.create: x:%g, y:%g and z:%g are too small for creating a Unit vector, Tolerance:%g" x y z zeroLengthTol 
        let li = 1. / l
        UnitVec.createUnchecked( li*x , li*y , li*z ) 

/// A 3D Point (2D Points are called 'Pt') 
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Pnt = 
    val X : float
    val Y : float 
    val Z : float
    new (x,y,z) = 
        #if DEBUG // with this test all Pnt operations are 2.5 times slower:
        if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then FsExGeoException.Raise "FsEx.Geo.Pnt Constructor failed for x:%g, y:%g, z:%g"  x y z  
        #endif
        {X=x; Y=y; Z=z} 
    
    override p.ToString() = sprintf "FsEx.Geo.Pnt(X=%s, Y=%s, Z=%s)" (Format.float p.X)(Format.float p.Y)(Format.float p.Z)

    static member inline ( - ) (a:Pnt, b:Pnt)     = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)
    static member inline ( - ) (p:Pnt, v:UnitVec) = Pnt (p.X - v.X , p.Y - v.Y , p.Z - v.Z)     
    static member inline ( - ) (p:Pnt, v:Vec)     = Pnt (p.X - v.X , p.Y - v.Y , p.Z - v.Z)            

    static member inline ( + ) (a:Pnt, b:Pnt)     = Pnt (a.X + b.X , a.Y + b.Y , a.Z + b.Z) // required for Seq.average and midPt
    static member inline ( + ) (a:Pnt, v:Vec)     = Pnt (a.X + v.X , a.Y + v.Y , a.Z + v.Z)  
    static member inline ( + ) (p:Pnt, v:UnitVec) = Pnt (p.X + v.X , p.Y + v.Y , p.Z + v.Z)  

    static member inline ( * )  (a:Pnt, f:float) = Pnt (a.X * f , a.Y * f , a.Z * f) 
    static member inline ( * )  (f:float, a:Pnt) = Pnt (a.X * f , a.Y * f , a.Z * f) 

    static member inline ( / )  (p:Pt, f:float) = 
        #if DEBUG
        if abs f < Util.zeroLengthTol then  FsExGeoDivByZeroException.Raise "%g is too small for dividing %O using / operator. Tolerance:%g" f p zeroLengthTol 
        #endif
        p * (1./f) // or Pnt (v.X / f , v.Y / f , , v.Z / f) ?


