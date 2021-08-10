namespace FsEx.Geo

// Design notes:
// The structs types in this file only have the constructors , ToString override and operators define in this file. 
// For structs that need a checked and unchecked constructor ( like unit vectors) the main 'new' constructor is marked obsolete. 
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. see files in folder members.
// This design however makes extension members unaccessible from see C#. To fix this all types and all members could be put into one file.
// the types would have to be marked as recursive. This file would be very large and probably have bad editor performance. 

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]  see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike    
open FsEx.Geo.Util  

/// A immutable 2D vector with any length. Made up from 2 floats: X and Y.
/// ( 2D Unit vectors with length 1.0 are called 'UnitVc' )
/// ( 3D vectors are called 'Vec' ) 
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>] // not used, see notes at end of file  
type Vc =
    /// Gets the X part of this 2D vector
    val X : float
    
    /// Gets the Y part of this 2D vector
    val Y : float
    
    /// Create a new 2D vector with any length. Made up from 2 floats: X and Y.
    new (x,y) =         
        #if DEBUG // TODO : with this test all  operations are 2.5 times slower 
        if Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y  then FsExGeoException.Raise "FsEx.Geo.Vc Constructor failed for x:%g , y:%g"  x y
        #endif
        {X=x; Y=y}  
    
    /// Format 2D vector into string including type name and nice floating point number formatting of X,Y and length.
    override v.ToString() = sprintf "FsEx.Geo.Vc(X=%s, Y=%s) Length: %s" (Format.float v.X) (Format.float v.Y) (Format.float (sqrt (v.X*v.X + v.Y*v.Y)))
    
    /// Format 2D vector into string with nice floating point number formatting of X and Y
    /// But without full type name or length as in v.ToString()
    member v.AsShortString = sprintf "X=%s, Y=%s" (Format.float v.X) (Format.float v.Y) 
    

    /// Negate or inverse a 2D vectors. Returns a new 2D vector.
    static member inline (~- )  (v:Vc)       = Vc ( -v.X , -v.Y )
    
    /// Subtract one 2D vectors from another. Returns a new 2D vector.
    static member inline ( - )  (a:Vc, b:Vc) = Vc (a.X - b.X , a.Y - b.Y )
        
    /// Add two 2D vectors together. Returns a new 2D vector.
    static member inline ( + )  (a:Vc, b:Vc) = Vc (a.X + b.X , a.Y + b.Y )
        
    /// Multiplies a 2D vector with a scalar, also called scaling a vector. Returns a new 2D vector.
    static member inline ( * )  (v:Vc   , f:float) = Vc (v.X * f , v.Y * f ) 
    
    /// Multiplies a scalar with a 2D vector, also called scaling a vector. Returns a new 2D vector. 
    static member inline ( * )  (f:float, v:Vc  )  = Vc (v.X * f , v.Y * f ) 
    
    /// Dot product, or scalar product of two 2D vectors. Returns a float.
    static member inline ( * )  (v:Vc   , b:Vc  )  = v.X * b.X + v.Y * b.Y   

    /// Divides a 2D vector by a scalar, also be called dividing/scaling a vector. Returns a new 2D vector.
    static member inline ( / )  (v:Vc, f:float) = 
        //#if DEBUG
        if abs f < Util.zeroLengthTol then FsExGeoDivByZeroException.Raise "%g is too small for dividing %O using / operator. Tolerance:%g"  f v zeroLengthTol
        //#endif
        Vc (v.X / f , v.Y / f ) 


#nowarn "44" // for internal inline constructors 

/// A immutable 2D vector guaranteed to be unitized (3D Unit Vectors are called 'UnitVec') 
/// Use UnitVc.create or UnitVc.createUnchecked to created instances.
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]// not used, see notes at end of file  
type UnitVc =
    
    /// Gets the X part of this 2D unit vector
    val X : float
    
    /// Gets the Y part of this 2D unit vector
    val Y : float

    /// Unsafe internal constructor, doesn't check or unitize the input,  public only for inlining.
    [<Obsolete("Unsafe internal constructor, doesn't check or unitize the input, but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (x,y) = 
        #if DEBUG
        let l = x*x + y*y // TODO : with this test all  operations are 2.5 times slower  
        if Util.isNotOne l then  FsExGeoException.Raise "FsEx.Geo.UnitVc Constructor failed for x:%g and y:%g. The length needs to be 1.0." x y 
        #endif
        {X=x; Y=y}
        
    /// Format 2D unit vector into string including type name and nice floating point number formatting.
    override v.ToString() =  sprintf "FsEx.Geo.UnitVc(X=%s, Y=%s)" (Format.float v.X)(Format.float v.Y)        
    
    /// Format 2D unit vector into string with nice floating point number formatting of X and Y
    /// But without full type name as in v.ToString()
    member v.AsShortString = sprintf "X=%s, Y=%s" (Format.float v.X) (Format.float v.Y) 

    /// Negate or inverse a 2D unit vectors. Returns a new 2D unit vector.
    static member inline (~- )  (v:UnitVc) = UnitVc( -v.X , -v.Y )
    
    /// Subtract one 2D unit vectors from another. Returns a new (non-unitized) 2D vector.
    static member inline ( - )  (a:UnitVc, b:UnitVc) = Vc (a.X - b.X , a.Y - b.Y )
    
    /// Subtract a 2D unit vectors from a 2D vector". Returns a new (non-unitized) 2D vector.
    static member inline ( - )  (a:Vc, b:UnitVc) = Vc (a.X - b.X , a.Y - b.Y )
    
    /// Subtract a 2D vectors from a 2D unit vector". Returns a new (non-unitized) 2D vector.
    static member inline ( - )  (a:UnitVc, b:Vc) = Vc (a.X - b.X , a.Y - b.Y )
        
    /// Add two 2D unit vectors together. 
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + )  (a:UnitVc, b:UnitVc) = Vc (a.X + b.X , a.Y + b.Y )
    
    /// Add a 2D unit vectors and a 2D vector together. 
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + )  (a:Vc,     b:UnitVc) = Vc (a.X + b.X , a.Y + b.Y )
    
    /// Add a 2D vectors and a 2D unit vector together. 
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + )  (a:UnitVc, b:Vc)     = Vc (a.X + b.X , a.Y + b.Y )
        
    /// Multiplies a 2D unit vector with a scalar, also called scaling a vector. 
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( * )  (a:UnitVc  , f:float   ) = Vc (a.X * f , a.Y * f )
    
    /// Multiplies a scalar with a 2D unit vector, also called scaling a vector. 
    /// Returns a new (non-unitized) 2D vector. 
    static member inline ( * )  (f:float   , a:UnitVc  ) = Vc (a.X * f , a.Y * f )
    
    /// Dot product, or scalar product of two 2D unit vectors. 
    /// Returns a float. This float is the Cosine of the angle between the two vectors.
    static member inline ( * )  (a:UnitVc  , b:UnitVc  ) = a.X * b.X+ a.Y * b.Y  
    
    /// Dot product, or scalar product of a 2D unit vector with a 2D vector  
    /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit vector
    static member inline ( * )  (a:UnitVc  , b:Vc      ) = a.X * b.X+ a.Y * b.Y  
    
    /// Dot product, or scalar product of a 2D vector with a 2D unit vector  
    /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit vector
    static member inline ( * )  (a:Vc      , b:UnitVc  ) = a.X * b.X+ a.Y * b.Y  
        
    /// Divides a 2D unit vector by a scalar, also be called dividing/scaling a vector. Returns a new (non-unitized) 2D vector.
    static member inline ( / )  (v:UnitVc, f:float) = 
        //#if DEBUG
        if abs f < Util.zeroLengthTol then  FsExGeoDivByZeroException.Raise "%g is too small for dividing %O using / operator. Tolerance:%g" f v zeroLengthTol
        //#endif
        Vc (v.X / f , v.Y / f ) 
    
    /// For use as a faster constructor
    /// Requires correct input of unitized values   
    static member inline createUnchecked(x,y)  = UnitVc(x,y) // needs #nowarn "44" // for internal inline constructors 

    /// Create 2D unit vector. Does the unitizing too.
    static member inline create (x:float, y:float) = 
        // this member cant be an extension method because it is used with SRTP. 
        // see error FS1114: The value 'FsEx.Geo.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x * x  + y * y)                     
        if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "UnitVc.create: x:%g and z:%g are too small for creating a Unit vector. Tolerance:%g" x y zeroLengthTol        
        UnitVc( x/l , y/l )  

/// A immutable 2D Point. Made up from 2 floats: X and Y.
/// ( 3D Points are called 'Pnt' ) 
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Pt =
    
    /// Gets the X part of this 2D point
    val X : float

    /// Gets the Z part of this 2D point
    val Y : float

    /// Create a new 2D Point. Made up from 3 floats: X, Y, and Z.
    new (x,y) =         
        #if DEBUG // TODO : with this test all  operations are 2.5 times slower    
        if Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y  then FsExGeoException.Raise "FsEx.Geo.Pt Constructor failed for x:%g , y:%g"  x y 
        #endif
        {X=x; Y=y}

    /// Format 2D point into string including type name and nice floating point number formatting.
    override p.ToString() = sprintf "FsEx.Geo.Pt(X=%s, Y=%s)" (Format.float p.X) (Format.float p.Y)
    
    /// Format 2D point into string with nice floating point number formatting of X and Y
    /// But without full type name as in p.ToString()
    member p.AsShortString = sprintf "X=%s, Y=%s" (Format.float p.X) (Format.float p.Y) 

    /// Subtract one 2D point from another. 
    /// 'a-b' returns a new 2D vector from b to a. 
    static member inline ( - )  (a:Pt, b:Pt)     = Vc (a.X - b.X , a.Y - b.Y )
    
    /// Subtract a unit vector from a 2D point. Returns a new 2D point.
    static member inline ( - )  (a:Pt, b:Vc)     = Pt (a.X - b.X , a.Y - b.Y )
    
    /// Subtract a vector from a 2D point. Returns a new 2D point.
    static member inline ( - )  (a:Pt, b:UnitVc) = Pt (a.X - b.X , a.Y - b.Y )

    //static member inline ( + )  (v:UnitVc, p:Pt)     = Pt (p.X + v.X , p.Y + v.Y )
    //static member inline ( + )  (v:Vc,     p:Pt)     = Pt (p.X + v.X , p.Y + v.Y )
    
    
    /// Add two 2D points together. Returns a new 2D point.
    static member inline ( + )  (p:Pt, v:Vc) = Pt (p.X + v.X , p.Y + v.Y )
    
    /// Add a vector to a 2D point. 
    /// Returns a new 2D point.
    static member inline ( + )  (p:Pt,v:UnitVc) = Pt (p.X + v.X , p.Y + v.Y )
    
    /// Add a unit vector to a 2D point. 
    /// Returns a new 2D point.
    static member inline ( + )  (a:Pt,b:Pt) = Pt (a.X + b.X , a.Y + b.Y )
    
    /// Multiplies a 2D point with a scalar, also called scaling a point. 
    /// Returns a new 2D point.
    static member inline ( * )  (a:Pt, f:float) = Pt (a.X * f , a.Y * f ) 
    
    /// Multiplies a scalar with a 2D point, also called scaling a point. 
    /// Returns a new 2D point. 
    static member inline ( * )  (f:float, a:Pt  ) = Pt (a.X * f , a.Y * f )   

    
    /// Divides a 2D point by a scalar, also be called dividing/scaling a point. Returns a new 2D point.
    static member inline ( / )  (p:Pt, f:float) = 
        //#if DEBUG
        if abs f < Util.zeroLengthTol then  FsExGeoDivByZeroException.Raise "%g is too small for dividing %O using / operator. Tolerance:%g" f p zeroLengthTol
        //#endif
        Pt (p.X / f , p.Y / f ) 
        

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
