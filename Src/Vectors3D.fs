namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open FsEx.Geo.Util    
    
/// A 3D Vector (2D Vectors are called 'Vc') 
[<Struct; NoEquality; NoComparison>] 
[<IsReadOnly>]
//[<IsByRefLike>]
type Vec = 
    val X : float
    val Y : float 
    val Z : float
        
    new (x,y,z) =
        #if DEBUG
        if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then FsExGeoException.Raise $"FsEx.Geo.Vec Constructor failed for x:%g{x} , y:%g{y}, z:%g{z}"    
        #endif
        {X=x; Y=y; Z=z}
        
    override p.ToString() = $"FsEx.Geo.Vec(X=%s{Format.float p.X}, Y=%s{Format.float p.Y}, Z=%s{Format.float p.Z})" 
    
    static member inline (~- ) (v:Vec)          = Vec( -v.X , -v.Y , -v.Z)
    static member inline ( - ) (a:Vec, b:Vec)   = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)            

    static member inline ( + ) (a:Vec, b:Vec)   = Vec (a.X + b.X , a.Y + b.Y , a.Z + b.Z)  

    static member inline ( * ) (a:Vec  , f:float) = Vec (a.X * f , a.Y * f , a.Z * f) // scale Vec
    static member inline ( * ) (f:float, a:Vec  ) = Vec (a.X * f , a.Y * f , a.Z * f) // scale Vec
    static member inline ( * ) (a:Vec  , b:Vec  ) = a.X * b.X + a.Y * b.Y + a.Z * b.Z // dot product              

    static member inline ( / )  (v:Vec, f:float) = 
        #if DEBUG
        if abs f < Util.zeroLenghtTol then  FsExGeoDivByZeroException.Raise $"%g{f} is too small for dividing %O{v} using / operator. Tolerance:%g{Util.zeroLenghtTol}" 
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
    
    /// Unsave internal constructor,  public only for inlining.
    [<Obsolete("Unsave internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (x,y,z) = 
        #if DEBUG
        let l = x*x + y*y + z*z // TODO : with this test all  operations are 2.5 times slower  
        if 0.999999999 > l || l > 1.000000001 then  FsExGeoException.Raise $"FsEx.Geo.UnitVc Constructor failed for x:%g{x}, y:%g{y}, z:%g{z}. The length needs to be 1.0."   
        #endif
        {X=x; Y=y; Z=z}
        
    override p.ToString() = $"FsEx.Geo.UnitVec(X=%s{Format.float p.X}, Y=%s{Format.float p.Y}, Z=%s{Format.float p.Z})" 
    
    static member inline ( ~- ) (v:UnitVec)            = UnitVec ( -v.X , -v.Y , -v.Z)        
    static member inline ( - )  (a:UnitVec, b:UnitVec) = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)            
    
    static member inline ( + )  (a:UnitVec, b:UnitVec) = Vec (a.X + b.X , a.Y + b.Y , a.Z + b.Z)  

    static member inline ( * )  (a:UnitVec, f:float  ) = Vec (a.X * f , a.Y * f , a.Z * f) // scale Vector
    static member inline ( * )  (f:float,   a:UnitVec) = Vec (a.X * f , a.Y * f , a.Z * f) // scale Vector
    static member inline ( * )  (a:UnitVec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z // dot product
    
    static member inline ( / )  (v:Vec, f:float) = 
        #if DEBUG
        if abs f < Util.zeroLenghtTol then FsExGeoDivByZeroException.Raise $"%g{f} is too small for dividing %O{v} using / operator. Tolerance:%g{Util.zeroLenghtTol}" 
        #endif
        v * (1./f) // or Vec (v.X / f , v.Y / f , , v.Z / f) ?
    
    /// Requires correct input of unitized values
    static member inline createUnchecked(x,y,z)  = UnitVec(x,y,z)

    /// Does the unitizing too.
    static member inline create (x:float, y:float, z:float) = 
        // this member cant be an extension method because it is used with SRTP. 
        // see error FS1114: The value 'FsEx.Geo.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x*x  + y*y + z*z)                       
        if l < zeroLenghtTol then FsExGeoDivByZeroException.Raise $"UnitVec.create: x:%g{x}, y:%g{y} and z:%g{z} are too small for creating a Unit vector, Tolerance:%g{zeroLenghtTol}" 
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
        if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then FsExGeoException.Raise $"FsEx.Geo.Pnt Constructor failed for x:%g{x} , y:%g{y}, z:%g{z}"    
        #endif
        {X=x; Y=y; Z=z} 
    
    override p.ToString() = $"FsEx.Geo.Pnt(X=%s{Format.float p.X}, Y=%s{Format.float p.Y}, Z=%s{Format.float p.Z})" 

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
        if abs f < Util.zeroLenghtTol then  FsExGeoDivByZeroException.Raise $"%g{f} is too small for dividing %O{p} using / operator. Tolerance:%g{Util.zeroLenghtTol}" 
        #endif
        p * (1./f) // or Pnt (v.X / f , v.Y / f , , v.Z / f) ?


