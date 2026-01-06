namespace Euclid

// Design notes: see file Pt.fs for more details
open System
open System.Runtime.CompilerServices
open Euclid.UtilEuclid
open System.Runtime.Serialization
open EuclidErrors

/// <summary>Vc is an immutable 2D vector with any length. Made up from 2 floats: X and Y.</summary>
/// <remarks>2D unit-vectors with length 1.0 are called 'UnitVc'.
/// 3D vectors are called 'Vec'.</remarks>
[<Struct;DataContract;NoEquality;NoComparison;IsReadOnly>] //[<IsByRefLike>]
type Vc =

    /// <summary>Gets the X part of this 2D vector.</summary>
    [<DataMember>] val X : float

    /// <summary>Gets the Y part of this 2D vector.</summary>
    [<DataMember>] val Y : float

    /// Create a new 2D vector with any length. Made up from 2 floats: X and Y.
    new (x, y) =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode // TODO : with this test all  operations are 2.5 times slower
            if isNanInfinity x || isNanInfinity y then failNaN2 "Vc()" x y
        #endif
            {X=x; Y=y}


    /// Format 2D vector into string including type name and nice floating point number formatting of X, Y and length.
    override v.ToString() =
        let x = Format.float v.X
        let y = Format.float v.Y
        let length = Format.float (sqrt (v.X*v.X + v.Y*v.Y))
        $"Euclid.Vc: X=%s{x}|Y=%s{y}|length: %s{length}"

    /// Format 2D vector into string with nice floating point number formatting of X and Y
    /// But without full type name or length as in v.ToString()
    member v.AsString =
        let x = Format.float v.X
        let y = Format.float v.Y
        $"X=%s{x}|Y=%s{y}"

    /// Format 2D vector into an F# code string that can be used to recreate the vector.
    member v.AsFSharpCode =
        $"Vc({v.X}, {v.Y})"

    /// Negate or inverse a 2D vector. Returns a new 2D vector.
    static member inline ( ~- ) (v:Vc) =
        Vc ( -v.X, -v.Y)

    /// Subtract one 2D vector from another. Returns a new 2D vector.
    static member inline ( - ) (a:Vc, b:Vc) =
        Vc (a.X - b.X, a.Y - b.Y)

    /// Add two 2D vectors together. Returns a new 2D vector.
    static member inline ( + ) (a:Vc, b:Vc) =
        Vc (a.X + b.X, a.Y + b.Y)

    /// Multiplies a 2D vector with a scalar, also called scaling a vector. Returns a new 2D vector.
    static member inline ( * ) (v:Vc, f:float) =
        Vc (v.X * f, v.Y * f)

    /// Multiplies a scalar with a 2D vector, also called scaling a vector. Returns a new 2D vector.
    static member inline ( * ) (f:float, v:Vc) =
        Vc (v.X * f, v.Y * f)

    /// Dot product, or scalar product of two 2D vectors. Returns a float.
    static member inline ( *** ) (a:Vc, b:Vc) =
        a.X * b.X + a.Y * b.Y

    /// Divides a 2D vector by a scalar. Returns a new 2D vector.
    /// Throws EuclidDivByZeroException if the divisor is too small (|f| < 1e-12).
    static member inline ( / ) (v:Vc, f:float) =
        if isTooTiny (abs f) then failDivide "'/' operator" f v // don't compose error msg directly here to keep inlined code small. // https://github.com/dotnet/runtime/issues/24626#issuecomment-356736809
        Vc (v.X / f, v.Y / f)

    /// Returns the length of the 2D vector.
    member inline v.Length =
        let x = v.X
        let y = v.Y
        sqrt (x*x + y*y)

    /// Returns the squared length of the 2D vector.
    /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
    member inline v.LengthSq =
        let x = v.X
        let y = v.Y
        x*x + y*y

    //-----------------------------------------------------------------------------------------------------
    // These static members can't be extension methods to be useful for Array.sum and Array.average :
    //-----------------------------------------------------------------------------------------------------

    /// Returns a zero length vector: Vc(0, 0)
    static member inline Zero =
        Vc(0, 0)  // this member is needed by Seq.sum, so that it doesn't fail on empty seq.

    /// Divides the vector by an integer.
    /// (This member is needed by Array.average and similar functions)
    /// Throws EuclidDivByZeroException if i is 0.
    static member DivideByInt (v:Vc, i:int) = // needed by 'Array.average'
        if i = 0 then failDivide "Vc.DivideByInt by zero" 0.0 v
        let d = float i
        Vc(v.X/d, v.Y/d)

