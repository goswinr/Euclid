namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]  see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike    
open FsEx.Geo.Util  
    
/// A 2D Vector with any length. Made up from 2 floats: X and Y.
/// (2D Unit vectors with length 1.0 are called 'UnitVc')
/// (3D Vectors are called 'Vec') 
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>] // not used, see notes at end of file  
type Vc =
    val X : float
    val Y : float
    
    new (x,y) =         
        #if DEBUG // TODO : with this test all  operations are 2.5 times slower 
        if Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y  then FsExGeoException.Raise "FsEx.Geo.Vc Constructor failed for x:%g , y:%g"  x y
        #endif
        {X=x; Y=y}  
    
    override v.ToString() = sprintf "FsEx.Geo.Vc(X=%s, Y=%s)" (Format.float v.X)(Format.float v.Y)
    
    static member inline (~- )  (v:Vc)       = Vc ( -v.X , -v.Y )
    static member inline ( - )  (a:Vc, b:Vc) = Vc (a.X - b.X , a.Y - b.Y )
    
    static member inline ( + )  (a:Vc, b:Vc) = Vc (a.X + b.X , a.Y + b.Y )
    

    static member inline ( * )  (v:Vc   , f:float) = Vc (v.X * f , v.Y * f ) // scale Vector
    static member inline ( * )  (f:float, v:Vc  )  = Vc (v.X * f , v.Y * f ) // scale Vector
    static member inline ( * )  (v:Vc   , b:Vc  )  = v.X * b.X + v.Y * b.Y   // dot product

    static member inline ( / )  (v:Vc, f:float) = 
        #if DEBUG
        if abs f < Util.zeroLengthTol then FsExGeoDivByZeroException.Raise "%g is too small for dividing %O using / operator. Tolerance:%g"  f v zeroLengthTol
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

    /// Unsafe internal constructor, doesn't check or unitize the input,  public only for inlining.
    [<Obsolete("Unsafe internal constructor, doesn't check or unitize the input, but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (x,y) = 
        #if DEBUG
        let l = x*x + y*y // TODO : with this test all  operations are 2.5 times slower  
        if 0.999999999 > l || l > 1.000000001 then  FsExGeoException.Raise "FsEx.Geo.UnitVc Constructor failed for x:%g and y:%g. The length needs to be 1.0." x y 
        #endif
        {X=x; Y=y}
        
    override v.ToString() =  sprintf "FsEx.Geo.UnitVc(X=%s, Y=%s)" (Format.float v.X)(Format.float v.Y)
        
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
        if abs f < Util.zeroLengthTol then  FsExGeoDivByZeroException.Raise "%g is too small for dividing %O using / operator. Tolerance:%g" f v zeroLengthTol
        #endif
        Vc (v.X / f , v.Y / f ) 
    
    /// Requires correct input of unitized values     
    static member inline createUnchecked(x,y)  = UnitVc(x,y) // needs #nowarn "44" // for internal inline constructors 

    /// Does the unitizing too.
    static member inline create (x:float, y:float) = 
        // this member cant be an extension method because it is used with SRTP. 
        // see error FS1114: The value 'FsEx.Geo.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x * x  + y * y)                     
        if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "UnitVc.create: x:%g and z:%g are too small for creating a Unit vector. Tolerance:%g" x y zeroLengthTol        
        UnitVc( x/l , y/l )  

/// A 2D Point (3D Points are called 'Pnt') 
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Pt =
    val X : float
    val Y : float

    new (x,y) =         
        #if DEBUG // TODO : with this test all  operations are 2.5 times slower    
        if Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y  then FsExGeoException.Raise "FsEx.Geo.Pt Constructor failed for x:%g , y:%g"  x y 
        #endif
        {X=x; Y=y}

    override p.ToString() = sprintf "FsEx.Geo.Pt(X=%s, Y=%s)" (Format.float p.X) (Format.float p.Y)

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
        if abs f > Util.zeroLengthTol then  Pt (p.X / f , p.Y / f ) 
        else FsExGeoDivByZeroException.Raise "%g is too small for dividing %O using '/' operator. Tolerance:%g" f p zeroLengthTol


(*
from: 
https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
        
[<IsByRefLike>] is another attribute. 
We are talking a lot about passing value types using memory location addresses instead of doing deep copies. 
Marking struct using this attribute is basically saying "I always want to pass this value by reference". 
This of course comes with severe limitations: it cannot be boxed (moved to managed heap) and 
for this reason it can never be captured by closures, implement interfaces or be used as field in classes or other non-by-ref structs.
        
In terms of F# this basically means that this kind of structs are used mostly for code that
is executed right away within the function body, with no computation expressions or other indirections. 
This usually qualifies them to hot paths in our code, where CPU intensive work is expected and allocations 
are not welcome, like:
        
for .. in loops - in fact many moderns .NET structures have special variants of GetEnumerator 
that doesn't allocate any memory and is implemented as by-ref struct. 
F# also understands that pattern - in fact you can define custom 
GetEnumerator(): MyEnumerator method for your collection, with MyEnumerator - which can even be a ref struct - having two methods: 
        
Current: 'item and 
MoveNext: unit -> bool, and F# will automatically understand how to use it in loops. 
You can see an example implementation of it here - 
https://github.com/Horusiath/fsharp.core.extensions/blob/62b102e84325e89b0a6c4065b973936c11adee55/src/FSharp.Core.Extensions/Vec.fs#L147
it's a part of implementation of persistent vector data type, 
similar to FSharpX persistent vector, but it's 4.5 times faster and not allocating anything on heap when executed in loops.
*)
