namespace Euclid

// Design notes: see file Pt.fs for more details

open System
open System.Runtime.CompilerServices
open Euclid.UtilEuclid
open System.Runtime.Serialization
open EuclidErrors

/// <summary>UnitVc is an immutable 2D unit-vector. It is guaranteed to be unitized.
/// Never use the struct default constructor UnitVc()!
/// It will create an invalid zero length vector.
/// Use UnitVc.create or UnitVc.createUnchecked instead.</summary>
/// <remarks>3D unit-vectors are called 'UnitVec'</remarks>
[<Struct;DataContract;NoEquality;NoComparison;IsReadOnly>] //[<IsByRefLike>]
type UnitVc =

    /// Gets the X part of this 2D unit-vector.
    [<DataMember>] val X : float

    /// Gets the Y part of this 2D unit-vector.
    [<DataMember>] val Y : float

    /// Unsafe internal constructor, doesn't check or unitize the input, public only for inlining.
    [<Obsolete("Unsafe internal constructor, doesn't check or unitize the input (unless compiled in DEBUG mode), but must be public for inlining. So marked Obsolete instead.") >]
    new (x, y) =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode // TODO : with this test all  operations are 2.5 times slower
            if Double.IsNaN x || Double.IsNaN y || Double.IsInfinity x || Double.IsInfinity y  then
                failNaN2 "UnitVc()" x y
            let lenSq = x*x + y*y
            if UtilEuclid.isNotOne lenSq then
                failNotOne2 "UnitVc()" x y
        #endif
            {X=x; Y=y}

    /// For use as a faster constructor.
    /// Requires correct input of unitized values.
    static member inline createUnchecked(x, y) =
        #nowarn "44"
        UnitVc(x, y)
        #warnon "44" // re-enable warning for obsolete usage

    /// Format 2D unit-vector into string including type name and nice floating point number formatting.
    override v.ToString() =
        let x = Format.float v.X
        let y = Format.float v.Y
        $"Euclid.UnitVc: X=%s{x}|Y=%s{y}"

    /// Format 2D unit-vector into string with nice floating point number formatting of X and Y
    /// But without full type name as in v.ToString()
    member v.AsString =
        let x = Format.float v.X
        let y = Format.float v.Y
        $"X=%s{x}|Y=%s{y}"

    /// Format 2D unit-vector into an F# code string that can be used to recreate the unit-vector.
    member v.AsFSharpCode =
        $"UnitVc.create({v.X}, {v.Y})"

    /// Negate or inverse a 2D unit-vector. Returns a new 2D unit-vector.
    static member inline ( ~- ) (v:UnitVc) =
        UnitVc.createUnchecked( -v.X, -v.Y)

    /// Subtract one 2D unit-vector from another. Returns a new (non-unitized) 2D vector.
    static member inline ( - ) (a:UnitVc, b:UnitVc) =
        Vc (a.X - b.X, a.Y - b.Y)

    /// Subtract a 2D unit-vector from a 2D vector. Returns a new (non-unitized) 2D vector.
    static member inline ( - ) (a:Vc, b:UnitVc) =
        Vc (a.X - b.X, a.Y - b.Y)

    /// Subtract a 2D vector from a 2D unit-vector. Returns a new (non-unitized) 2D vector.
    static member inline ( - ) (a:UnitVc, b:Vc) =
        Vc (a.X - b.X, a.Y - b.Y)

    /// Add two 2D unit-vectors together.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + ) (a:UnitVc, b:UnitVc) =
        Vc (a.X + b.X, a.Y + b.Y)

    /// Add a 2D vector and a 2D unit-vector together.
    /// Returns a new (non-unitized) 2D vector.
    static member inline ( + ) (a:Vc, b:UnitVc) =
        Vc (a.X + b.X, a.Y + b.Y)

    /// Add a 2D unit-vector and a 2D vector together.
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
        a.X * b.X + a.Y * b.Y

    /// Dot product, or scalar product of a 2D unit-vector with a 2D vector.
    /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:UnitVc, b:Vc) =
        a.X * b.X + a.Y * b.Y

    /// Dot product, or scalar product of a 2D vector with a 2D unit-vector.
    /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:Vc, b:UnitVc) =
        a.X * b.X + a.Y * b.Y

    /// Dot product, or scalar product of two 2D unit-vectors.
    /// Returns a float.
    /// This float is the Cosine of the angle between the two 2D vectors.
    static member inline dot (a:UnitVc, b:UnitVc) =
        a.X * b.X + a.Y * b.Y


    /// The 2D Cross Product.
    /// It is also known as the Determinant, or the sine of the angle between the two vectors.
    /// In 2D it is just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
    /// If the rotation from 'a' to 'b' is Counter-Clockwise the result is positive.
    /// For unit-vectors this is the same as the sine of the angle between the two vectors.
    /// (while the dot product is the cosine)
    static member inline cross (a:UnitVc, b:UnitVc) =
        a.X*b.Y - a.Y*b.X


    /// Divides a 2D unit-vector by a scalar, also called dividing/scaling a vector. Returns a new (non-unitized) 2D vector.
    static member inline ( / ) (v:UnitVc, f:float) =
        if isTooTiny (abs f) then failDivide "'/' operator" f v // don't compose error msg directly here to keep inlined code small. // https://github.com/dotnet/runtime/issues/24626#issuecomment-356736809
        Vc (v.X / f, v.Y / f)


    // /// For use as a faster constructor.
    // /// Requires correct input of unitized values.
    // static member inline createUnchecked(v:Vc) =
    //     UnitVc.createUnchecked(v.X, v.Y)

    /// Create 2D unit-vector. Does the unitizing too.
    static member inline create (x:float, y:float) =
        // this member cant be an extension method because it is used with SRTP in UnitV.createFromMembersXY
        // see error FS1114: The value 'Euclid.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x * x  + y * y)
        if isTooTiny l then failUnit2 "UnitVc.create" x y
        UnitVc.createUnchecked(x/l, y/l)


    // These members cannot be implemented since Array.sum and Array.average of UnitVc would return a 'Vc' and not a 'UnitVc'
    // static member Zero = UnitVc (0., 0.)  // needed by 'Array.sum'
    // static member inline DivideByInt (v:UnitVc, i:int) = v / float i  // needed by  'Array.average'
