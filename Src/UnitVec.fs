namespace Euclid

// Design notes:
// The structs types in this file only have the constructors, the ToString override and operators defined in this file.
// For structs that need a checked and unchecked constructor (like unit-vectors) the main 'new' constructor is marked obsolete.
// A 'create' and 'createUnchecked' static member is provided instead.
// All other members are implemented as extension members. See files in folder 'members'.
// This design however makes extension members inaccessible from C#. To fix this all types and all members could be put into one file.
// The types would have to be marked as recursive. This file would be very large and probably have bad editor performance.

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors


/// An immutable 3D vector guaranteed to be always unitized.
/// A 3D vector represents a direction or translation in space, but not a location.
/// A 4x4 transformation matrix applied to a vector will only rotate and scale the vector but not translate it.
/// (2D unit-vectors are called 'UnitVc' )
/// Use UnitVec.create or UnitVec.createUnchecked to create instances.
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
    [<Obsolete("This is not Obsolete, but an unsafe internal constructor. the input is not verified, so it might create invalid geometry. It is exposed as a public member so that it can be inlined. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >]
    new (x, y, z) =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode //  with these tests all operations are 2.5 times slower
            if isNanInfinity x || isNanInfinity y || isNanInfinity z then
                failNaN3 "UnitVec()" x y z
            let lenSq = x*x + y*y + z*z
            if UtilEuclid.isNotOne lenSq then
                failNotOne3 "UnitVec()" x y z
        #endif
            {X=x; Y=y; Z=z}

    /// For use as a faster internal constructor.
    /// Requires correct input of unitized values.
    static member inline createUnchecked(x, y, z) : UnitVec =
        #nowarn "44"
        UnitVec(x, y, z)
        #warnon "44" // re-enable warning for obsolete usage


    /// Format 3D unit-vector into string including type name and nice floating point number formatting.
    override p.ToString() =
        $"Euclid.UnitVec: X={Format.float p.X}|Y={Format.float p.Y}|Z={Format.float p.Z}"

    /// Format 3D unit-vector into string with nice floating point number formatting of X, Y and Z
    /// But without full type name as in v.ToString()
    member v.AsString : string =
        $"X={Format.float v.X}|Y={Format.float v.Y}|Z={Format.float v.Z}"

    /// Format 3D unit-vector into an F# code string that can be used to recreate the unit-vector.
    member v.AsFSharpCode : string =
        $"UnitVec.create({v.X}, {v.Y}, {v.Z})"

    /// Negate or inverse a 3D unit-vector. Returns a new 3D unit-vector.
    static member inline ( ~- ) (v:UnitVec) =
        UnitVec.createUnchecked ( -v.X, -v.Y, -v.Z)

    /// Subtract one 3D unit-vector from another. Returns a new (non-unitized) 3D vector.
    static member inline ( - ) (a:UnitVec, b:UnitVec) =
        Vec (a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    /// Subtract a 3D vector from a 3D unit-vector. Returns a new (non-unitized) 3D vector.
    static member inline ( - ) (a:UnitVec, b:Vec) =
        Vec (a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    /// Subtract a 3D unit-vector from a 3D vector. Returns a new (non-unitized) 3D vector.
    static member inline ( - ) (a:Vec, b:UnitVec) =
        Vec (a.X - b.X, a.Y - b.Y, a.Z - b.Z)

    /// Add two 3D unit-vectors together. Returns a new (non-unitized) 3D vector.
    static member inline ( + ) (a:UnitVec, b:UnitVec) =
        Vec (a.X + b.X, a.Y + b.Y, a.Z + b.Z)

    /// Add a 3D vector and a 3D unit-vector together. Returns a new (non-unitized) 3D vector.
    static member inline ( + ) (a:Vec, b:UnitVec) =
        Vec (a.X + b.X, a.Y + b.Y, a.Z + b.Z)

    /// Add a 3D unit-vector and a 3D vector together. Returns a new (non-unitized) 3D vector.
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

    /// Dot product, or scalar product of a 3D unit-vector with a 3D vector.
    /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:UnitVec, b:Vec) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z

    /// Dot product, or scalar product of a 3D vector with a 3D unit-vector.
    /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit-vector.
    static member inline ( *** ) (a:Vec, b:UnitVec) =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z

   /// Divides a 3D unit-vector by a scalar, also called dividing/scaling a vector. Returns a new (non-unitized) 3D vector.
    static member inline ( / ) (v:UnitVec, f:float) =
        if isTooTiny (abs f) then failDivide "'/' operator" f v // don't compose error msg directly here to keep inlined code small. // https://github.com/dotnet/runtime/issues/24626#issuecomment-356736809
        Vec (v.X / f, v.Y / f, v.Z / f)

    /// Dot product, or scalar product of two 3D unit-vectors.
    /// Returns a float.
    /// This float for unit-vectors is the Cosine of the angle between the two vectors.
    static member inline dot (a:UnitVec, b:UnitVec) : float =
        a.X * b.X + a.Y * b.Y + a.Z * b.Z


    /// The Cross Product.
    /// It is also known as the Determinant, Wedge Product or Outer Product.
    /// The resulting vector is perpendicular to both input vectors.
    /// The length of this resulting vector is the area of the parallelogram spanned by the input vectors.
    /// Its direction follows the right-hand rule.
    /// A x B = |A| * |B| * sin(angle)
    static member inline cross (a:UnitVec, b:UnitVec) : Vec =
        Vec (a.Y * b.Z - a.Z * b.Y, a.Z * b.X - a.X * b.Z, a.X * b.Y - a.Y * b.X)


    // /// For use as a faster internal constructor.
    // /// Requires correct input of unitized values.
    // static member inline createUnchecked(v:Vec) =
    //     UnitVec(v.X, v.Y, v.Z)

    /// Create 3D unit-vector. Does the unitizing too.
    static member inline create (x:float, y:float, z:float) : UnitVec =
        // this member cant be an extension method because it is used with SRTP.
        // see error FS1114: The value 'Euclid.AutoOpenUnitVc.create' was marked inline but was not bound in the optimization environment
        let l = sqrt(x*x  + y*y + z*z)
        if isTooTiny l then failUnit3 "UnitVec.create" x y z
        let li = 1. / l
        UnitVec.createUnchecked(li*x, li*y, li*z)

    // These members cannot be implemented since
    // Array.sum and Array.average of UnitVec would return a 'Vec' and not a 'UnitVec'
    // static member Zero = UnitVec (0, 0, 0)  // needed by 'Array.sum'
    // static member inline DivideByInt (v:UnitVec, i:int) = v / float i  // needed by  'Array.average'
