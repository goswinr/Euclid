namespace FsEx.Geo

// Design notes:
// The structs types in this file only have the constructors , the ToString override and operators define in this file. 
// For structs that need a checked and unchecked constructor ( like unit vectors) the main 'new' constructor is marked obsolete. 
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. see files in folder 'members'.
// This design however makes extension members unaccessible from see C#. To fix this all types and all members could be put into one file.
// The types would have to be marked as recursive. This file would be very large and probably have bad editor performance. 

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open FsEx.Geo.Util  


/// An immutable 3D vector of any length. Made up from 3 floats: X, Y, and Z.
/// ( 3D Unit vectors of length 1.0 are called 'UnitVec' )
/// ( 2D vectors are called 'Vc' ) 
[<Struct; NoEquality; NoComparison>] 
[<IsReadOnly>]
//[<IsByRefLike>] // not used, see notes at end of file  
type Vec = 
    /// Gets the X part of this 3D vector
    val X : float

    /// Gets the Y part of this 3D vector
    val Y : float 

    /// Gets the Z part of this 3D vector
    val Z : float
    
    /// Create a new 3D vector with any length. Made up from 3 floats: X, Y, and Z.
    new (x,y,z) =
        #if DEBUG
        if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then 
            FsExGeoException.Raise "FsEx.Geo.Vec Constructor failed for x:%g , y:%g, z:%g"  x y z
        #endif
        {X=x; Y=y; Z=z}
        
    /// Format 3D vector into string including type name and nice floating point number formatting of X,Y,Z and length.
    override v.ToString() = sprintf "FsEx.Geo.Vec: X=%s| Y=%s| Z=%s| Length: %s" (Format.float v.X) (Format.float v.Y) (Format.float v.Z) (Format.float (sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z)))
    
    /// Format 3D vector into string with nice floating point number formatting of X,Y and Z
    /// But without full type name or length as in v.ToString()
    member v.AsString = sprintf "X=%s| Y=%s| Z=%s" (Format.float v.X) (Format.float v.Y)  (Format.float v.Z) 

    /// Returns the length of the 3D vector 
    member inline v.Length = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z) 

    /// Returns the squared length of the 3D vector 
    /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
    member inline v.LengthSq = v.X*v.X + v.Y*v.Y + v.Z*v.Z
    
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
        //#if DEBUG
        if abs f < zeroLengthTol then  FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec divide operator: %g is too small for dividing %O using / operator. Tolerance:%g" f v zeroLengthTol
        // #endif
        //v * (1./f) // maybe faster but worse precision 
        Vec (v.X / f , v.Y / f ,  v.Z / f) 
    
    /// Dot product, or scalar product of two 3D vectors. 
    /// Returns a float. 
    static member inline dot  (a:Vec, b:Vec)   = a.X * b.X + a.Y * b.Y + a.Z * b.Z
    
    /// Cross product, of two 3D vectors. 
    /// The resulting vector is perpendicular to both input vectors.
    /// Its length is the area of the parallelogram spanned by the input vectors.
    /// Its direction follows th right-hand rule.
    /// A x B = |A| * |B| * sin(angle)
    static member inline cross (a:Vec, b:Vec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) 



    
#nowarn "44" // for hidden constructors via Obsolete Attribute    
        
/// An immutable 3D vector guaranteed to be always unitized. ( 2D Unit vectors are called 'UnitVc' ) 
/// Use UnitVec.create or UnitVec.createUnchecked to created instances.
/// Note: Never use the struct default constructor UnitVec() as it will create an invalid zero length vector. 
/// Use UnitVec.create or UnitVec.createUnchecked instead.
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>] // not used, see notes at end of file  
type UnitVec =  
    /// Gets the X part of this 3D  unit-vector
    val X : float

    /// Gets the Y part of this 3D unit-vector
    val Y : float 

    /// Gets the Z part of this 3D unit-vector
    val Z : float 

    /// Unsafe internal constructor, doesn't check or unitize the input, public only for inlining.
    [<Obsolete("Unsafe internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (x,y,z) = 
        #if DEBUG
        if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then 
            FsExGeoException.Raise "FsEx.Geo.UnitVec Constructor failed for x:%g, y:%g, z:%g"  x y z 
        let lenSq = x*x + y*y + z*z // TODO : with this test all  operations are 2.5 times slower  
        if Util.isNotOne lenSq then  
            FsExGeoException.Raise "FsEx.Geo.UnitVec Constructor failed for x:%g, y:%g, z:%g. The length needs to be 1.0."  x y z
        #endif
        {X=x; Y=y; Z=z}
        
    /// Format 3D unit-vector into string including type name and nice floating point number formatting.
    override p.ToString() = sprintf "FsEx.Geo.UnitVec: X=%s| Y=%s| Z=%s" (Format.float p.X)(Format.float p.Y)(Format.float p.Z)
    
    /// Format 3D unit-vector into string with nice floating point number formatting of X,Y and Z
    /// But without full type name as in v.ToString()
    member v.AsString = sprintf "X=%s| Y=%s| Z=%s" (Format.float v.X) (Format.float v.Y) (Format.float v.Z) 
    
    /// Negate or inverse a 3D unit vectors. Returns a new 3D unit-vector.
    static member inline ( ~- ) (v:UnitVec) = UnitVec ( -v.X , -v.Y , -v.Z)   
    
    /// Subtract one 3D unit vectors from another. Returns a new (non-unitized) 3D vector.
    static member inline ( - )  (a:UnitVec, b:UnitVec) = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)    
    
    /// Subtract a 3D vectors from a 3D unit-vector. Returns a new (non-unitized) 3D vector.
    static member inline ( - )  (a:UnitVec, b:Vec) = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)    
    
    /// Subtract a 3D unit vectors from a 3D vector. Returns a new (non-unitized) 3D vector.
    static member inline ( - )  (a:Vec, b:UnitVec) = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)    
    
    /// Add two 3D unit vectors together. Returns a new (non-unitized) 3D vector.
    static member inline ( + )  (a:UnitVec, b:UnitVec) = Vec (a.X + b.X , a.Y + b.Y , a.Z + b.Z)
    
    /// Add a 3D vectors and a 3D unit-vector together. Returns a new (non-unitized) 3D vector.
    static member inline ( + )  (a:Vec, b:UnitVec) = Vec (a.X + b.X , a.Y + b.Y , a.Z + b.Z)
    
    /// Add a 3D unit vectors and a 3D vector together. Returns a new (non-unitized) 3D vector.
    static member inline ( + )  (a:UnitVec, b:Vec) = Vec (a.X + b.X , a.Y + b.Y , a.Z + b.Z)
    
    /// Multiplies a 3D unit-vector with a scalar, also called scaling a vector. Returns a new (non-unitized) 3D vector.
    static member inline ( * )  (a:UnitVec, f:float  ) = Vec (a.X * f , a.Y * f , a.Z * f)

    /// Multiplies a scalar with a 3D unit-vector, also called scaling a vector. Returns a new (non-unitized) 3D vector. 
    static member inline ( * )  (f:float,   a:UnitVec) = Vec (a.X * f , a.Y * f , a.Z * f)

    /// Dot product, or scalar product of two 3D unit vectors. 
    /// Returns a float. This float is the Cosine of the angle between the two 3D vectors.
    static member inline ( * )  (a:UnitVec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z 
    
    /// Dot product, or scalar product of a 3D unit vectors with a 3D vector  
    /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector
    static member inline ( * )  (a:UnitVec, b:Vec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z 
    
    /// Dot product, or scalar product of a 3D unit vectors with a 3D vector  
    /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector
    static member inline ( * )  (a:Vec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z 

    /// Divides a 3D unit-vector by a scalar, also be called dividing/scaling a vector. Returns a new (non-unitized) 3D vector.
    static member inline ( / )  (v:UnitVec, f:float) = 
        //#if DEBUG
        if abs f < Util.zeroLengthTol then 
            FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVec divide Operator: %g is too small for dividing %O using / operator. Tolerance:%g" f v zeroLengthTol
        //#endif
        //v * (1./f) // maybe faster but worse precision
        Vec (v.X / f , v.Y / f , v.Z / f)     

    /// Dot product, or scalar product of two 3D unit vectors. 
    /// Returns a float. This float of unit vectors is the Cosine of the angle between the two vectors.
    static member inline dot  (a:UnitVec, b:UnitVec)   = a.X * b.X + a.Y * b.Y + a.Z * b.Z

    /// Cross product, of two 3D vectors. 
    /// The resulting vector is perpendicular to both input vectors.
    /// Its length is the area of the parallelogram spanned by the input vectors.
    /// Its direction follows th right-hand rule.
    /// A x B = |A| * |B| * sin(angle)
    static member inline cross (a:UnitVec, b:UnitVec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X )  
    
    /// For use as a faster internal constructor.
    /// Requires correct input of unitized values.
    static member inline createUnchecked(x,y,z)  = UnitVec(x,y,z)
    
    /// Create 3D unit-vector. Does the unitizing too.
    static member inline create (x:float, y:float, z:float) = 
        // this member cant be an extension method because it is used with SRTP. 
        // see error FS1114: The value 'FsEx.Geo.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x*x  + y*y + z*z)                       
        if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVec.create: x:%g, y:%g and z:%g are too small for creating a unit-vector, Tolerance:%g" x y z zeroLengthTol 
        let li = 1. / l
        UnitVec.createUnchecked( li*x , li*y , li*z )
    

/// An immutable 3D point. Made up from 3 floats: X, Y, and Z.
/// ( 2D Points are called 'Pt' ) 
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Pnt = 
    
    /// Gets the X part of this 3D point
    val X : float

    /// Gets the Y part of this 3D point
    val Y : float 

    /// Gets the Z part of this 3D point
    val Z : float

    /// Create a new 3D point. Made up from 3 floats: X, Y, and Z.
    new (x,y,z) = 
        #if DEBUG // with this test all Pnt operations are 2.5 times slower:
        if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then FsExGeoException.Raise "FsEx.Geo.Pnt Constructor failed for x:%g, y:%g, z:%g"  x y z  
        #endif
        {X=x; Y=y; Z=z} 
    
    /// Format 3D point into string including type name and nice floating point number formatting.
    override p.ToString() = sprintf "FsEx.Geo.Pnt: X=%s| Y=%s| Z=%s" (Format.float p.X)(Format.float p.Y)(Format.float p.Z)
    
    
    /// Format 3D point into string with nice floating point number formatting of X,Y and Z
    /// But without full type name as in pt.ToString()
    member p.AsString = sprintf "X=%s| Y=%s| Z=%s" (Format.float p.X) (Format.float p.Y) (Format.float p.Z) 

    /// Subtract one 3D point from another. 
    /// 'a-b' returns a new 3D vector from b to a. 
    static member inline ( - ) (a:Pnt, b:Pnt)     = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)

    /// Subtract a unit-vector from a 3D point. Returns a new 3D point.
    static member inline ( - ) (p:Pnt, v:UnitVec) = Pnt (p.X - v.X , p.Y - v.Y , p.Z - v.Z)  
    
    /// Subtract a vector from a 3D point. Returns a new 3D point.
    static member inline ( - ) (p:Pnt, v:Vec)     = Pnt (p.X - v.X , p.Y - v.Y , p.Z - v.Z)

    /// Add two 3D points together. Returns a new 3D point.
    static member inline ( + ) (a:Pnt, b:Pnt)     = Pnt (a.X + b.X , a.Y + b.Y , a.Z + b.Z) // required for Seq.average and Pnt.midPt
    
    /// Add a vector to a 3D point. Returns a new 3D point.
    static member inline ( + ) (p:Pnt, v:Vec)     = Pnt (p.X + v.X , p.Y + v.Y , p.Z + v.Z)  
    
    /// Add a unit-vector to a 3D point. Returns a new 3D point.
    static member inline ( + ) (p:Pnt, v:UnitVec) = Pnt (p.X + v.X , p.Y + v.Y , p.Z + v.Z)  

    /// Multiplies a 3D point with a scalar, also called scaling a point. Returns a new 3D point.
    static member inline ( * )  (a:Pnt, f:float) = Pnt (a.X * f , a.Y * f , a.Z * f) 

    /// Multiplies a scalar with a 3D point, also called scaling a point. Returns a new 3D point. 
    static member inline ( * )  (f:float, a:Pnt) = Pnt (a.X * f , a.Y * f , a.Z * f)

    /// Divides a 3D point by a scalar, also be called dividing/scaling a point. Returns a new 3D point.
    static member inline ( / )  (p:Pnt, f:float) = 
        //#if DEBUG
        if abs f < Util.zeroLengthTol then  
            FsExGeoDivByZeroException.Raise "FsEx.Geo.Pnt divide operator: %g is too small for dividing %O using / operator. Tolerance:%g" f p zeroLengthTol 
        //#endif
        //p * (1./f) // maybe faster but worse precision
        Pnt (p.X / f , p.Y / f ,  p.Z / f) 


(*
from: 
https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike

netstandard2.0 does not support [<IsByRefLike>] nor  [<IsReadOnly>]

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
