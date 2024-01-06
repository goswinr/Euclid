namespace Euclid

// Design notes:
// The structs types in this file only have the constructors, ToString override and operators define in this file.
// For structs that need a checked and unchecked constructor (like unit vectors) the main 'new' constructor is marked obsolete.
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. see files in folder members.
// This design however makes extension members unaccessible from see C#. To fix this all types and all members could be put into one file.
// the types would have to be marked as recursive. This file would be very large and probably have bad editor performance.

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>]  see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.Util
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar

#nowarn "44" // for internal inline constructors and hidden obsolete members for error cases


/// An immutable 2D vector guaranteed to be unitized (3D Unit vectors are called 'UnitVec')
/// Use UnitVc.create or UnitVc.createUnchecked to created instances.
/// Note: Never use the default constructor UnitVc() as it will create an invalid zero length vector.
/// Use UnitVc.create or UnitVc.createUnchecked instead.
[<Struct;NoEquality;NoComparison>]// because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]// not used, see notes at end of file
[<DataContract>] // for using DataMember on fields  
type UnitVc =

    /// Gets the X part of this 2D unit-vector.
    [<DataMember>] val X : float

    /// Gets the Y part of this 2D unit-vector.
    [<DataMember>] val Y : float

    /// Unsafe internal constructor, doesn't check or unitize the input, public only for inlining.
    [<Obsolete("Unsafe internal constructor, doesn't check or unitize the input (unless compiled in DEBUG mode), but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >]
    new (x, y) =
        #if DEBUG
        if Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y  then
            EuclidException.Raise "Euclid.UnitVc Constructor failed for x:%g, y:%g"  x y
        let lenSq = x*x + y*y // TODO : with this test all  operations are 2.5 times slower
        if Util.isNotOne lenSq then
            EuclidException.Raise "Euclid.UnitVc Constructor failed for x:%g and y:%g. The length needs to be 1.0." x y
        #endif
        {X=x; Y=y}

    /// Format 2D unit-vector into string including type name and nice floating point number formatting.
    override v.ToString() = sprintf "Euclid.UnitVc: X=%s|Y=%s" (Format.float v.X)(Format.float v.Y)

    /// Format 2D unit-vector into string with nice floating point number formatting of X and Y
    /// But without full type name as in v.ToString()
    member v.AsString = sprintf "X=%s| Y=%s" (Format.float v.X) (Format.float v.Y)

    /// Negate or inverse a 2D unit vectors. Returns a new 2D unit-vector.
    static member inline ( ~- ) (v:UnitVc) = UnitVc( -v.X, -v.Y)

    /// Subtract one 2D unit vectors from another. Returns a new (non-unitized) 2D vector.
    static member inline ( - ) (a:UnitVc, b:UnitVc) = Vc (a.X - b.X, a.Y - b.Y)

    /// Subtract a 2D unit vectors from a 2D vector". Returns a new (non-unitized) 2D vector.
    static member inline ( - ) (a:Vc, b:UnitVc) = Vc (a.X - b.X, a.Y - b.Y)

    /// Subtract a 2D vectors from a 2D unit-vector". Returns a new (non-unitized) 2D vector.
    static member inline ( - ) (a:UnitVc, b:Vc) = Vc (a.X - b.X, a.Y - b.Y)

    /// Add two 2D unit vectors together.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + ) (a:UnitVc, b:UnitVc) = Vc (a.X + b.X, a.Y + b.Y)

    /// Add a 2D unit vectors and a 2D vector together.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + ) (a:Vc, b:UnitVc) = Vc (a.X + b.X, a.Y + b.Y)

    /// Add a 2D vectors and a 2D unit-vector together.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + ) (a:UnitVc, b:Vc) = Vc (a.X + b.X, a.Y + b.Y)

    /// Multiplies a 2D unit-vector with a scalar, also called scaling a vector.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( * ) (a:UnitVc, f:float) = Vc (a.X * f, a.Y * f)

    /// Multiplies a scalar with a 2D unit-vector, also called scaling a vector.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( * ) (f:float, a:UnitVc) = Vc (a.X * f, a.Y * f)

    /// Dot product, or scalar product of two 2D unit vectors.
    /// Returns a float. This float is the Cosine of the angle between the two 2D vectors.
    static member inline ( *** ) (a:UnitVc, b:UnitVc) = a.X * b.X+ a.Y * b.Y

    /// Dot product, or scalar product of a 2D unit-vector with a 2D vector.
    /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:UnitVc, b:Vc) = a.X * b.X+ a.Y * b.Y

    /// Dot product, or scalar product of a 2D vector with a 2D unit-vector.
    /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:Vc, b:UnitVc) = a.X * b.X+ a.Y * b.Y

    /// 2D cross product.
    /// Its Just a scalar equal to the area of the parallelogram spanned by the input vectors.
    /// For unit vectors this is the same as the sine of the angle between the two vectors. (while the dot product is the cosine)
    static member inline cross (a:UnitVc, b:UnitVc) = a.X*b.Y - a.Y*b.X
    
    /// A separate function to compose the error message that does not get inlined.
    [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
    member v.FailedDivide(f) = EuclidDivByZeroException.Raise "Euclid.UnitVc: divide operator: %g is too small for dividing %O using the '/' operator. Tolerance:%g"  f v zeroLengthTolerance
        
    /// Divides a 2D unit-vector by a scalar, also be called dividing/scaling a vector. Returns a new (non-unitized) 2D vector.
    static member inline ( / ) (v:UnitVc, f:float) = 
        if abs f < zeroLengthTolerance then v.FailedDivide(f) // don't compose error msg directly here to keep inlined code small.        
        Vc (v.X / f, v.Y / f)    

    /// For use as a faster constructor.
    /// Requires correct input of unitized values.
    static member inline createUnchecked(x, y) = UnitVc(x, y) // needs #nowarn "44" // for internal inline constructors
    
    /// For use as a faster constructor.
    /// Requires correct input of unitized values.
    static member inline createUnchecked(v:Vc) = UnitVc(v.X, v.Y) // needs #nowarn "44" // for internal inline constructors

    /// A separate function to compose the error message that does not get inlined.
    [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
    static member failedCreate (x:float, y:float) = EuclidDivByZeroException.Raise "Euclid.UnitVc.create: x:%g and y:%g are too small for creating a unit-vector. Tolerance:%g" x y zeroLengthTolerance
        
    /// Create 2D unit-vector. Does the unitizing too.
    static member inline create (x:float, y:float) =
        // this member cant be an extension method because it is used with SRTP in UnitV.createFromMembersXY
        // see error FS1114: The value 'Euclid.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x * x  + y * y)
        if l < zeroLengthTolerance then UnitVc.failedCreate(x, y) // don't compose error msg directly here to keep inlined code small.
        UnitVc(x/l, y/l)

        
    // These members cannot be implemented since Array.sum and Array.average of UnitVc would return a 'Vc' and not a 'UnitVc'
    // static member Zero = UnitVc (0., 0.)  // needed by 'Array.sum'
    // static member inline DivideByInt (v:UnitVc, i:int) = v / float i  // needed by  'Array.average'
