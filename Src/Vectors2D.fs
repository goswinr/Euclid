namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]  see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike    

    
/// A 2D Vector (3D Vectors are called 'Vec') 
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Vc =
    val X : float
    val Y : float
    
    new (x,y) =         
        #if DEBUG // TODO : with this test all  operations are 2.5 times slower 
        if Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y  then FsExGeoException.Raise $"FsEx.Geo.Vc Constructor failed for x:%g{x} , y:%g{y}"  
        #endif
        {X=x; Y=y}  
    
    override v.ToString() = $"FsEx.Geo.Vc(X=%s{Format.float v.X}, Y=%s{Format.float v.Y})" 
    
    static member inline (~- )  (v:Vc)       = Vc ( -v.X , -v.Y )
    static member inline ( - )  (a:Vc, b:Vc) = Vc (a.X - b.X , a.Y - b.Y )
    
    static member inline ( + )  (a:Vc, b:Vc) = Vc (a.X + b.X , a.Y + b.Y )
    

    static member inline ( * )  (v:Vc   , f:float) = Vc (v.X * f , v.Y * f ) // scale Vector
    static member inline ( * )  (f:float, v:Vc  )  = Vc (v.X * f , v.Y * f ) // scale Vector
    static member inline ( * )  (v:Vc   , b:Vc  )  = v.X * b.X + v.Y * b.Y   // dot product

    static member inline ( / )  (v:Vc, f:float) = 
        #if DEBUG
        if abs f < Util.zeroLenghtTol then FsExGeoDivByZeroException.Raise $"%g{f} is too small for dividing %O{v} using / operator. Tolerance:%g{Util.zeroLenghtTol}"  
        #endif
        Vc (v.X / f , v.Y / f ) 


#nowarn "44" // for internal inline constructors 

/// A 2D Vector guaranteed to be unitized (3D Unit Vectors are called 'UnitVec') 
/// This type shall only be created by unitizing a  2D vector (Vc)
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type UnitVc =
    val X : float
    val Y : float

    /// Unsave internal constructor, doesn't check or unitize the input,  public only for inlining.
    [<Obsolete("Unsave internal constructor, doesn't check or unitize the input, but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (x,y) = 
        #if DEBUG
        let l = x*x + y*y // TODO : with this test all  operations are 2.5 times slower  
        if 0.999999999 > l || l > 1.000000001 then  FsExGeoException.Raise $"FsEx.Geo.UnitVc Constructor failed for x:%g{x} and y:%g{y}. The length needs to be 1.0."   
        #endif
        {X=x; Y=y}
        
    override v.ToString() =  $"FsEx.Geo.UnitVc(X=%s{Format.float v.X}, Y=%s{Format.float v.Y})" 
        
    static member inline (~- )  (v:UnitVc) = UnitVc( -v.X , -v.Y )
    static member inline ( - )  (a:UnitVc, b:UnitVc) = Vc (a.X - b.X , a.Y - b.Y )

    static member inline ( + )  (a:UnitVc, b:UnitVc) = Vc (a.X + b.X , a.Y + b.Y )
    static member inline ( + )  (a:Vc,     b:UnitVc) = Vc (a.X + b.X , a.Y + b.Y )
    static member inline ( + )  (a:UnitVc, b:Vc)     = Vc (a.X + b.X , a.Y + b.Y )

    static member inline ( * )  (a:UnitVc  , f:float   ) = Vc (a.X * f , a.Y * f )
    static member inline ( * )  (f:float   , a:UnitVc  ) = Vc (a.X * f , a.Y * f )
    static member inline ( * )  (a:UnitVc  , b:UnitVc  ) = a.X * b.X+ a.Y * b.Y  // dot product
    static member inline ( * )  (a:UnitVc  , b:Vc      ) = a.X * b.X+ a.Y * b.Y  // dot product
    static member inline ( * )  (a:Vc      , b:UnitVc  ) = a.X * b.X+ a.Y * b.Y  // dot product

    static member inline ( / )  (v:UnitVc, f:float) = 
        #if DEBUG
        if abs f < Util.zeroLenghtTol then  FsExGeoDivByZeroException.Raise $"%g{f} is too small for dividing %O{v} using / operator. Tolerance:%g{Util.zeroLenghtTol}" 
        #endif
        Vc (v.X / f , v.Y / f ) 
    
    /// Assumes correct input of unitized values     
    static member inline createUnchecked(x,y)  = UnitVc(x,y)

/// A 2D Point (3D Points are called 'Pnt') 
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Pt =
    val X : float
    val Y : float

    new (x,y) =         
        #if DEBUG // TODO : with this test all  operations are 2.5 times slower    
        if Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y  then FsExGeoException.Raise $"FsEx.Geo.Pt Constructor failed for x:%g{x} , y:%g{y}"  
        #endif
        {X=x; Y=y}

    override p.ToString() = $"FsEx.Geo.Pt(X=%s{Format.float p.X}, Y=%s{Format.float p.Y})" 

    static member inline ( - )  (a:Pt, b:Pt)     = Vc (a.X - b.X , a.Y - b.Y )
    static member inline ( - )  (a:Pt, b:Vc)     = Pt (a.X - b.X , a.Y - b.Y )
    static member inline ( - )  (a:Pt, b:UnitVc) = Pt (a.X - b.X , a.Y - b.Y )

    //static member inline ( + )  (v:UnitVc, p:Pt)     = Pt (p.X + v.X , p.Y + v.Y )
    //static member inline ( + )  (v:Vc,     p:Pt)     = Pt (p.X + v.X , p.Y + v.Y )
    static member inline ( + )  (p:Pt,     v:Vc)     = Pt (p.X + v.X , p.Y + v.Y )
    static member inline ( + )  (p:Pt,     v:UnitVc) = Pt (p.X + v.X , p.Y + v.Y )
    static member inline ( + )  (a:Pt,     b:Pt)     = Pt (a.X + b.X , a.Y + b.Y )

    static member inline ( * )  (a:Pt  , f:float) = Pt (a.X * f , a.Y * f ) // scale Vector
    static member inline ( * )  (f:float, a:Pt  ) = Pt (a.X * f , a.Y * f ) // scale Vector   

    static member inline ( / )  (p:Pt, f:float) = 
        if abs f > Util.zeroLenghtTol then  Pt (p.X / f , p.Y / f ) 
        else FsExGeoDivByZeroException.Raise $"%g{f} is too small for dividing %O{p} using '/' operator. Tolerance:%g{Util.zeroLenghtTol}" 
