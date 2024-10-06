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


/// An immutable 3D vector guaranteed to be always unitized.
/// A 3D vector represents a direction or an offset in space, but not a location.
/// A 4x4 transformation matrix applied to a vector will only rotate and scale the vector but not translate it.
/// (2D unit-vectors are called 'UnitVc' )
/// Use UnitVec.create or UnitVec.createUnchecked to created instances.
/// Note: Never use the struct default constructor UnitVec() as it will create an invalid zero length vector.
/// Use UnitVec.create or UnitVec.createUnchecked instead.
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>] // not used, see notes at end of file
[<DataContract>] // for using DataMember on fields
type UnitVec =

    /// Gets the X part of this 3D unit-vector.
    [<DataMember>] val X : float

    /// Gets the Y part of this 3D unit-vector.
    [<DataMember>] val Y : float

    /// Gets the Z part of this 3D unit-vector.
    [<DataMember>] val Z : float

    /// Unsafe internal constructor, doesn't check or unitize the input, public only for inlining.
    [<Obsolete("Unsafe internal constructor, but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >]
    new (x, y, z) =
        #if DEBUG //  with these tests all operations are 2.5 times slower
        if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then
            EuclidException.Raise "Euclid.UnitVec Constructor failed for x:%g, y:%g, z:%g"  x y z
        let lenSq = x*x + y*y + z*z
        if UtilEuclid.isNotOne lenSq then
            EuclidException.Raise "Euclid.UnitVec Constructor failed for x:%g, y:%g, z:%g. The length needs to be 1.0."  x y z
        #endif
        {X=x; Y=y; Z=z}

    /// Format 3D unit-vector into string including type name and nice floating point number formatting.
    override p.ToString() =
        sprintf "Euclid.UnitVec: X=%s|Y=%s|Z=%s" (Format.float p.X)(Format.float p.Y)(Format.float p.Z)

    /// Format 3D unit-vector into string with nice floating point number formatting of X, Y and Z
    /// But without full type name as in v.ToString()
    member v.AsString =
        sprintf "X=%s| Y=%s| Z=%s" (Format.float v.X) (Format.float v.Y) (Format.float v.Z)

    /// Negate or inverse a 3D unit-vectors. Returns a new 3D unit-vector.
    static member inline ( ~- ) (v:UnitVec) =
        UnitVec ( -v.X, -v.Y, -v.Z)

    /// Subtract one 3D unit-vectors from another. Returns a new (non-unitized) 3D vector.
    static member inline ( - ) (a:UnitVec, b:UnitVec) =
        Vec (a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    /// Subtract a 3D vectors from a 3D unit-vector. Returns a new (non-unitized) 3D vector.
    static member inline ( - ) (a:UnitVec, b:Vec) =
        Vec (a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    /// Subtract a 3D unit-vectors from a 3D vector. Returns a new (non-unitized) 3D vector.
    static member inline ( - ) (a:Vec, b:UnitVec) =
        Vec (a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    /// Add two 3D unit-vectors together. Returns a new (non-unitized) 3D vector.
    static member inline ( + ) (a:UnitVec, b:UnitVec) =
        Vec (a.X + b.X, a.Y + b.Y, a.Z + b.Z)

    /// Add a 3D vectors and a 3D unit-vector together. Returns a new (non-unitized) 3D vector.
    static member inline ( + ) (a:Vec, b:UnitVec) =
        Vec (a.X + b.X, a.Y + b.Y, a.Z + b.Z)

    /// Add a 3D unit-vectors and a 3D vector together. Returns a new (non-unitized) 3D vector.
    static member inline ( + ) (a:UnitVec, b:Vec) =
        Vec (a.X + b.X, a.Y + b.Y, a.Z + b.Z)

    /// Multiplies a 3D unit-vector with a scalar, also called scaling a vector. Returns a new (non-unitized) 3D vector.
    static member inline ( * ) (a:UnitVec, f:float) =
        Vec (a.X * f, a.Y * f, a.Z * f)

    /// Multiplies a scalar with a 3D unit-vector, also called scaling a vector. Returns a new (non-unitized) 3D vector.
    static member inline ( * ) (f:float, a:UnitVec) =
        Vec (a.X * f, a.Y * f, a.Z * f)

    /// Dot product, or scalar product of two 3D unit-vectors.
    /// Returns a float. This float is the Cosine of the angle between the two 3D vectors.
    static member inline ( *** ) (a:UnitVec, b:UnitVec) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z

    /// Dot product, or scalar product of a 3D unit-vectors with a 3D vector.
    /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:UnitVec, b:Vec) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z

    /// Dot product, or scalar product of a 3D unit-vectors with a 3D vector.
    /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:Vec, b:UnitVec) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z

    // A separate function to compose the error message that does not get inlined.
    [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
    member v.FailedDivide(f) = EuclidDivByZeroException.Raise "Euclid.UnitVec: divide operator: %g is too small for dividing %O using the '/' operator. Tolerance:%g"  f v zeroLengthTolerance
    /// Divides a 3D unit-vector by a scalar, also be called dividing/scaling a vector. Returns a new (non-unitized) 3D vector.
    static member inline ( / ) (v:UnitVec, f:float) =
        if isTooTiny (abs f) then v.FailedDivide(f) // don't compose error msg directly here to keep inlined code small. // https://github.com/dotnet/runtime/issues/24626#issuecomment-356736809
        Vec (v.X / f, v.Y / f, v.Z / f)

    /// Dot product, or scalar product of two 3D unit-vectors.
    /// Returns a float. This float of unit-vectors is the Cosine of the angle between the two vectors.
    static member inline dot (a:UnitVec, b:UnitVec) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z

    /// Cross product, of two 3D vectors.
    /// The resulting vector is perpendicular to both input vectors.
    /// Its length is the area of the parallelogram spanned by the input vectors.
    /// Its direction follows th right-hand rule.
    /// A x B = |A| * |B| * sin(angle)
    static member inline cross (a:UnitVec, b:UnitVec) =
        Vec (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)

    /// For use as a faster internal constructor.
    /// Requires correct input of unitized values.
    static member inline createUnchecked(x, y, z) =
        UnitVec(x, y, z)

    /// For use as a faster internal constructor.
    /// Requires correct input of unitized values.
    static member inline createUnchecked(v:Vec) =
        UnitVec(v.X, v.Y, v.Z)

    // A separate function to compose the error message that does not get inlined.    [<Obsolete("Not actually obsolete but just hidden. (Needs to be public for inlining of the functions using it.)")>]
    static member failedCreate (x:float, y:float, z:float) = EuclidDivByZeroException.Raise "Euclid.UnitVec.create: x:%g, y:%g and z:%g are too small for creating a unit-vector, Tolerance:%g" x y z zeroLengthTolerance
    /// Create 3D unit-vector. Does the unitizing too.
    static member inline create (x:float, y:float, z:float) =
        // this member cant be an extension method because it is used with SRTP.
        // see error FS1114: The value 'Euclid.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x*x  + y*y + z*z)
        if isTooTiny l then UnitVec.failedCreate(x, y, z) // don't compose error msg directly here to keep inlined code small. // https://github.com/dotnet/runtime/issues/24626#issuecomment-356736809
        let li = 1. / l
        UnitVec.createUnchecked(li*x, li*y, li*z)

    // These members cannot be implemented since
    // Array.sum and Array.average of UnitVec would return a 'Vec' and not a 'UnitVec'
    // static member Zero = UnitVec (0, 0, 0)  // needed by 'Array.sum'
    // static member inline DivideByInt (v:UnitVec, i:int) = v / float i  // needed by  'Array.average'
