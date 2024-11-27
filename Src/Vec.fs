namespace Euclid

// Design notes:
// The structs types in this file only have the constructors, the ToString override and operators define in this file.
// For structs that need a checked and unchecked constructor (like unit-vectors) the main 'new' constructor is marked obsolete.
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. see files in folder 'members'.
// This design however makes extension members unaccessible from see C#. To fix this all types and all members could be put into one file.
// The types would have to be marked as recursive. This file would be very large and probably have bad editor performance.

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar

#nowarn "44" // to skip Obsolete warnings (members just needs to be public for inlining, but should be hidden)



/// An immutable 3D vector of any length. Made up from 3 floats: X, Y, and Z.
/// A 3D vector represents a direction or an offset in space, but not a location.
/// A 4x4 transformation matrix applied to a vector will only rotate and scale the vector but not translate it.
/// (3D unit-vectors of length 1.0 are called 'UnitVec' )
/// (2D vectors are called 'Vc' )
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>] // not used, see notes at end of file
[<DataContract>] // for using DataMember on fields
type Vec =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// Gets the X part of this 3D vector.
    [<DataMember>] val X : float

    /// Gets the Y part of this 3D vector.
    [<DataMember>] val Y : float

    /// Gets the Z part of this 3D vector.
    [<DataMember>] val Z : float

    /// Create a new 3D vector with any length. Made up from 3 floats: X, Y, and Z.
    new (x, y, z) =
        #if DEBUG
        if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then
            EuclidException.Raisef "Euclid.Vec Constructor failed for x:%g, y:%g, z:%g"  x y z
        #endif
        {X=x; Y=y; Z=z}

    /// Format 3D vector into string including type name and nice floating point number formatting of X, Y, Z and length.
    override v.ToString() =
        sprintf "Euclid.Vec: X=%s|Y=%s|Z=%s|length: %s" (Format.float v.X) (Format.float v.Y) (Format.float v.Z) (Format.float (sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z)))

    /// Format 3D vector into string with nice floating point number formatting of X, Y and Z.
    /// But without full type name or length as in v.ToString()
    member v.AsString =
        sprintf "X=%s|Y=%s|Z=%s" (Format.float v.X) (Format.float v.Y) (Format.float v.Z)

    /// Returns the length of the 3D vector.
    member inline v.Length =
        sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z)

    /// Returns the squared length of the 3D vector.
    /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
    member inline v.LengthSq =
        v.X*v.X + v.Y*v.Y + v.Z*v.Z

    /// Negate or inverse a 3D vectors. Returns a new 3D vector.
    static member inline ( ~- ) (v:Vec) =
        Vec( -v.X, -v.Y, -v.Z)

    /// Subtract one 3D vectors from another. Returns a new 3D vector.
    static member inline ( - ) (a:Vec, b:Vec) =
        Vec (a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    /// Add two 3D vectors together. Returns a new 3D vector.
    static member inline ( + ) (a:Vec, b:Vec) =
        Vec (a.X + b.X, a.Y + b.Y, a.Z + b.Z)

    /// Multiplies a 3D vector with a scalar, also called scaling a vector. Returns a new 3D vector.
    static member inline ( * ) (a:Vec, f:float) =
        Vec (a.X * f, a.Y * f, a.Z * f)

    /// Multiplies a scalar with a 3D vector, also called scaling a vector. Returns a new 3D vector.
    static member inline ( * ) (f:float, a:Vec) =
        Vec (a.X * f, a.Y * f, a.Z * f)

    /// Dot product, or scalar product of two 3D vectors. Returns a float.
    static member inline ( *** ) (a:Vec, b:Vec) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z

    /// A separate function to compose the error message that does not get inlined.
    [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
    member v.FailedDivide(f) = EuclidDivByZeroException.Raisef "Euclid.Vec: divide operator: %g is too small for dividing %O using the '/' operator. Tolerance:%g"  f v zeroLengthTolerance

    /// Divides a 3D vector by a scalar, also be called dividing/scaling a vector. Returns a new 3D vector.
    static member inline ( / ) (v:Vec, f:float) =
        if isTooTiny (abs f) then v.FailedDivide(f) // don't compose error msg directly here to keep inlined code small.// https://github.com/dotnet/runtime/issues/24626#issuecomment-356736809
        Vec (v.X / f, v.Y / f, v.Z / f)


    /// Dot product, or scalar product of two 3D vectors.
    /// Returns a float.
    static member inline dot (a:Vec, b:Vec) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z

    /// Cross product, of two 3D vectors.
    /// The resulting vector is perpendicular to both input vectors.
    /// Its length is the area of the parallelogram spanned by the input vectors.
    /// Its direction follows th right-hand rule.
    /// A x B = |A| * |B| * sin(angle)
    static member inline cross (a:Vec, b:Vec) =
        Vec (a.Y * b.Z - a.Z * b.Y,
             a.Z * b.X - a.X * b.Z,
             a.X * b.Y - a.Y * b.X)

    //-----------------------------------------------------------------------------------------------------
    // These static members can't be extension methods to be useful for Array.sum and Array.average :
    //-----------------------------------------------------------------------------------------------------

    /// Divides the vector by an integer.
    /// (This member is needed by Array.average and similar functions)
    static member DivideByInt (v:Vec, i:int) = // needed by 'Array.average'
        if i = 0 then EuclidDivByZeroException.Raisef "Euclid.Vec.DivideByInt is zero %O" v
        let d = float i
        Vec(v.X/d, v.Y/d, v.Z/d)

    /// Returns a zero length vector: Vec(0, 0, 0)
    static member inline Zero =
        Vec(0, 0, 0)  // this member is needed by Seq.sum, so that it doesn't fail on empty seq.

