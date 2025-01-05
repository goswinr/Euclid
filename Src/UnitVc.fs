namespace Euclid

// Design notes: see file Pt.fs for more details

open System
open System.Runtime.CompilerServices
open Euclid.UtilEuclid
open System.Runtime.Serialization

#nowarn "44" // for internal inline constructors and hidden obsolete members for error cases

/// <summary>UnitVc is an immutable 2D unit-vector. it is guaranteed to be unitized.
/// Never use the struct default constructor UnitVc()!
/// It will create an invalid zero length vector.
/// Use UnitVc.create or UnitVc.createUnchecked instead.</summary>
/// <remarks>3D unit-vectors are called 'UnitVec'</remarks>
[<Struct;DataContract;NoEquality;NoComparison;IsReadOnly>] //[<IsByRefLike>]
type UnitVc =

    /// <summary>Gets the X part of this 2D unit-vector.</summary>
    [<DataMember>] val X : float

    /// <summary>Gets the X part of this 2D unit-vector.</summary>
    [<DataMember>] val Y : float

    /// Unsafe internal constructor, doesn't check or unitize the input, public only for inlining.
    [<Obsolete("Unsafe internal constructor, doesn't check or unitize the input (unless compiled in DEBUG mode), but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >]
    new (x, y) =
        #if DEBUG // TODO : with this test all  operations are 2.5 times slower
        if Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y  then EuclidException.Raisef "Euclid.Vc Constructor failed for x:%g, y:%g"  x y
        #endif
        {X=x; Y=y}

    /// Format 2D unit-vector into string including type name and nice floating point number formatting.
    override v.ToString() =
        sprintf "Euclid.UnitVc: X=%s|Y=%s" (Format.float v.X)(Format.float v.Y)

    /// Format 2D unit-vector into string with nice floating point number formatting of X and Y
    /// But without full type name as in v.ToString()
    member v.AsString =
        sprintf "X=%s| Y=%s" (Format.float v.X) (Format.float v.Y)

    /// Negate or inverse a 2D unit-vectors. Returns a new 2D unit-vector.
    static member inline ( ~- ) (v:UnitVc) =
        UnitVc( -v.X, -v.Y)

    /// Subtract one 2D unit-vectors from another. Returns a new (non-unitized) 2D vector.
    static member inline ( - ) (a:UnitVc, b:UnitVc) =
        Vc (a.X - b.X, a.Y - b.Y)

    /// Subtract a 2D unit-vectors from a 2D vector". Returns a new (non-unitized) 2D vector.
    static member inline ( - ) (a:Vc, b:UnitVc) =
        Vc (a.X - b.X, a.Y - b.Y)

    /// Subtract a 2D vectors from a 2D unit-vector". Returns a new (non-unitized) 2D vector.
    static member inline ( - ) (a:UnitVc, b:Vc) =
        Vc (a.X - b.X, a.Y - b.Y)

    /// Add two 2D unit-vectors together.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + ) (a:UnitVc, b:UnitVc) =
        Vc (a.X + b.X, a.Y + b.Y)

    /// Add a 2D unit-vectors and a 2D vector together.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + ) (a:Vc, b:UnitVc) =
        Vc (a.X + b.X, a.Y + b.Y)

    /// Add a 2D vectors and a 2D unit-vector together.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + ) (a:UnitVc, b:Vc) =
        Vc (a.X + b.X, a.Y + b.Y)

    /// Multiplies a 2D unit-vector with a scalar, also called scaling a vector.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( * ) (a:UnitVc, f:float) =
        Vc (a.X * f, a.Y * f)

    /// Multiplies a scalar with a 2D unit-vector, also called scaling a vector.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( * ) (f:float, a:UnitVc) =
        Vc (a.X * f, a.Y * f)

    /// Dot product, or scalar product of two 2D unit-vectors.
    /// Returns a float. This float is the Cosine of the angle between the two 2D vectors.
    static member inline ( *** ) (a:UnitVc, b:UnitVc) =
        a.X * b.X+ a.Y * b.Y

    /// Dot product, or scalar product of a 2D unit-vector with a 2D vector.
    /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:UnitVc, b:Vc) =
        a.X * b.X+ a.Y * b.Y

    /// Dot product, or scalar product of a 2D vector with a 2D unit-vector.
    /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:Vc, b:UnitVc) =
        a.X * b.X+ a.Y * b.Y

    /// Dot product, or scalar product of two 2D unit-vectors.
    /// Returns a float.
    /// This float is the Cosine of the angle between the two 2D vectors.
    static member inline dot (a:UnitVc, b:UnitVc) =
        a.X * b.X+ a.Y * b.Y


    /// The 2D cross product.
    /// It is just a scalar equal to the signed square area of the parallelogram spanned by the input vectors.
    /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
    /// For unit-vectors this is the same as the sine of the angle between the two vectors.
    /// (while the dot product is the cosine)
    static member inline cross (a:UnitVc, b:UnitVc) =
        a.X*b.Y - a.Y*b.X

    /// A separate function to compose the error message that does not get inlined.
    [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
    member v.FailedDivide(f) = EuclidDivByZeroException.Raisef "Euclid.UnitVc: divide operator: %g is too small for dividing %O using the '/' operator. Tolerance:%g"  f v zeroLengthTolerance

    /// Divides a 2D unit-vector by a scalar, also be called dividing/scaling a vector. Returns a new (non-unitized) 2D vector.
    static member inline ( / ) (v:UnitVc, f:float) =
        if isTooTiny (abs f) then v.FailedDivide(f) // don't compose error msg directly here to keep inlined code small. // https://github.com/dotnet/runtime/issues/24626#issuecomment-356736809
        Vc (v.X / f, v.Y / f)

    /// For use as a faster constructor.
    /// Requires correct input of unitized values.
    static member inline createUnchecked(x, y) =
        UnitVc(x, y) // needs #nowarn "44" // for internal inline constructors

    /// For use as a faster constructor.
    /// Requires correct input of unitized values.
    static member inline createUnchecked(v:Vc) =
        UnitVc(v.X, v.Y) // needs #nowarn "44" // for internal inline constructors

    /// A separate function to compose the error message that does not get inlined.
    [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
    static member failedCreate (x:float, y:float) = EuclidDivByZeroException.Raisef "Euclid.UnitVc.create: x:%g and y:%g are too small for creating a unit-vector. Tolerance:%g" x y zeroLengthTolerance

    /// Create 2D unit-vector. Does the unitizing too.
    static member inline create (x:float, y:float) =
        // this member cant be an extension method because it is used with SRTP in UnitV.createFromMembersXY
        // see error FS1114: The value 'Euclid.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x * x  + y * y)
        if isTooTiny l then UnitVc.failedCreate(x, y) // don't compose error msg directly here to keep inlined code small. // https://github.com/dotnet/runtime/issues/24626#issuecomment-356736809
        UnitVc(x/l, y/l)


    // These members cannot be implemented since Array.sum and Array.average of UnitVc would return a 'Vc' and not a 'UnitVc'
    // static member Zero = UnitVc (0., 0.)  // needed by 'Array.sum'
    // static member inline DivideByInt (v:UnitVc, i:int) = v / float i  // needed by  'Array.average'
