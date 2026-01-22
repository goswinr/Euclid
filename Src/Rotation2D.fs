namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors

/// 2D Counter Clockwise rotation. It can be applied in World X, Y or Z plane.
/// Internally stored just as a Sine and Cosine value.
/// For arbitrary rotations use Quaternions or 4x4 Matrix.
/// However this module has much better performance than the more general Matrix4x4 or a Quaternion.
/// Note: Never use the struct default constructor Rotation2D() as it will create an invalid zero Rotation2D.
/// Use Rotation2D.createFromRadians, Rotation2D.createFromDegrees, or Rotation2D.createUnchecked instead.
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Rotation2D =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The Sine component of this rotation.
    /// The range of this field is -1.0 to +1.0
    [<DataMember>] val Sin : float

    /// The Cosine component of this rotation.
    /// The range of this field is -1.0 to +1.0
    [<DataMember>] val Cos : float

    /// Unsafe internal constructor, public only for inlining.
    [<Obsolete("This is not Obsolete, but an unsafe internal constructor. the input is not verified, so it might create invalid geometry. It is exposed as a public member so that it can be inlined. So marked Obsolete instead.") >]
    new (sin, cos) =
        #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
            let sum = sin*sin + cos*cos in
            if 0.99999 > sum || sum > 1.00001  then failRot sin cos
        #endif
            {Sin = sin; Cos = cos}


    /// Construct 2D Rotation from sine and corresponding cosine directly.
    /// Input is unchecked and not validated.
    static member inline createUnchecked (sine, cosine) : Rotation2D =
        #nowarn "44"
        Rotation2D (sine, cosine)
        #warnon "44" // re-enable warning for obsolete usage

    /// Format rotation into string showing angle in Degrees as nicely formatted floating point number.
    override r.ToString() =
        let deg = atan2 r.Sin r.Cos |> toDegrees |> Format.float
        $"Euclid.Rotation2D of %s{deg}° Degrees."

    /// Format rotation into string showing angle in Degrees as nicely formatted floating point number.
    /// But without type name as in r.ToString()
    member r.AsString : string =
        let deg = atan2 r.Sin r.Cos |> toDegrees |> Format.float
        $"%s{deg}° Degrees."

    /// Format Rotation2D into an F# code string that can be used to recreate the rotation.
    member r.AsFSharpCode : string =
        // $"Rotation2D.createFromDegrees({r.Sin |> asinSafe |> toDegrees})"
        $"Rotation2D.createUnchecked({r.Sin}, {r.Cos})"

    /// Returns the angle represented by this 2D Rotation in Radians.
    /// The returned angle is in the range [-π, π] (-180° to +180°).
    member inline r.InRadians =
        atan2 r.Sin r.Cos

    /// Returns the angle represented by this 2D Rotation in Degrees.
    /// The returned angle is in the range [-180°, +180°].
    member inline r.InDegrees =
        r.InRadians |> toDegrees

    /// Returns the 2D Rotation in the opposite direction.
    member inline r.Inverse =
        Rotation2D.createUnchecked (-r.Sin, r.Cos)

    /// Create a new 2D Rotation that adds a 2D Rotation to the existing one.
    member inline r.Add(ro:Rotation2D) =
        //use 2x2 matrix multiplication logic for better performance:
        Rotation2D.createUnchecked ( r.Sin*ro.Cos + r.Cos*ro.Sin
                                   , r.Cos*ro.Cos - r.Sin*ro.Sin)


    /// Returns half the 2D Rotation in the same direction.
    member inline r.Half =
        let halfCos = sqrt ((1.0 + r.Cos) / 2.0)
        let halfSin = sqrt ((1.0 - r.Cos) / 2.0)
        let halfSinSigned = if r.Sin < 0.0 then -halfSin else halfSin
        Rotation2D.createUnchecked (halfSinSigned, halfCos)


    // ----------------------------------------------------------------------------------
    //            █████               █████     ███
    //           ░░███               ░░███     ░░░
    //    █████  ███████    ██████   ███████   ████   ██████
    //   ███░░  ░░░███░    ░░░░░███ ░░░███░   ░░███  ███░░███
    //  ░░█████   ░███      ███████   ░███     ░███ ░███ ░░░
    //   ░░░░███  ░███ ███ ███░░███   ░███ ███ ░███ ░███  ███
    //   ██████   ░░█████ ░░████████  ░░█████  █████░░██████
    //  ░░░░░░     ░░░░░   ░░░░░░░░    ░░░░░  ░░░░░  ░░░░░░
    //
    //                                             █████
    //                                            ░░███
    //    █████████████    ██████  █████████████   ░███████   ██████  ████████   █████
    //   ░░███░░███░░███  ███░░███░░███░░███░░███  ░███░░███ ███░░███░░███░░███ ███░░
    //    ░███ ░███ ░███ ░███████  ░███ ░███ ░███  ░███ ░███░███████  ░███ ░░░ ░░█████
    //    ░███ ░███ ░███ ░███░░░   ░███ ░███ ░███  ░███ ░███░███░░░   ░███      ░░░░███
    //    █████░███ █████░░██████  █████░███ █████ ████████ ░░██████  █████     ██████
    //   ░░░░░ ░░░ ░░░░░  ░░░░░░  ░░░░░ ░░░ ░░░░░ ░░░░░░░░   ░░░░░░  ░░░░░     ░░░░░░
    // --------------------------------------------------------------------------------

    /// Checks if two 2D Rotations are equal within tolerance.
    /// By comparing the fields Sin and Cos each with the given tolerance.
    /// The range of these fields is -1.0 to +1.0
    /// Use a tolerance of 0.0 to check for an exact match.
    static member inline equals (tol:float) (a:Rotation2D) (b:Rotation2D)  : bool =
        abs(a.Sin-b.Sin) <= tol &&
        abs(a.Cos-b.Cos) <= tol

    /// Construct a counter clockwise 2D Rotation from angle in Radians.
    /// Use negative radians for clockwise rotation.
    static member inline createFromRadians rad =
        Rotation2D.createUnchecked (sin rad, cos rad)

    /// Construct a counter clockwise 2D Rotation from angle in Degrees.
    /// Use negative degrees for clockwise rotation.
    static member inline createFromDegrees deg =
        let rad = toRadians deg
        Rotation2D.createUnchecked (sin rad, cos rad)


    /// Construct a counter clockwise 2D Rotation from angle given in its cosine value.
    /// The input must be in the range [-1.0, +1.0].
    /// Note: Only angles in the range [0°, 180°] can be created since acos returns values in [0, π].
    static member inline createFromCosine cos =
        #if DEBUG || CHECK_EUCLID
        if cos < -1.0 || cos > 1.0 then fail $"Rotation2D.createFromCosine: input cosine {cos} is out of range [-1.0, +1.0]."
        #endif
        let sin = sqrt (1.0 - cos*cos)
        Rotation2D.createUnchecked (sin, cos)

    /// Construct a counter clockwise 2D Rotation from angle given in its sine value.
    /// The input must be in the range [-1.0, +1.0].
    /// Note: Only angles in the range [-90°, +90°] can be created since asin returns values in [-π/2, π/2].
    static member inline createFromSine sin =
        #if DEBUG || CHECK_EUCLID
        if sin < -1.0 || sin > 1.0 then fail $"Rotation2D.createFromSine: input sine {sin} is out of range [-1.0, +1.0]."
        #endif
        let cos = sqrt (1.0 - sin*sin)
        Rotation2D.createUnchecked (sin, cos)


    /// Construct a 2D Rotation from two unit vectors.
    /// The rotation represents the counter-clockwise angle from vector 'a' to vector 'b'.
    /// Uses the dot product for cosine and cross product for sine.
    static member createFromVectors (a:UnitVc,b:UnitVc) =
        let dot = a *** b
        let cross = UnitVc.cross(a, b)
        Rotation2D.createUnchecked (cross, dot)

    /// Construct a 2D Rotation from two 2D vectors.
    /// The rotation represents the counter-clockwise angle from vector 'a' to vector 'b'.
    /// Normalizes both vectors internally and uses dot product for cosine and cross product for sine.
    /// Fails if either vector has near-zero length.
    static member createFromVectors (a:Vc,b:Vc) =
        let la = a.Length
        let lb = b.Length
        if isTooTiny la || isTooTiny lb then
            failUnit2 "Rotation2D.createFromVectors" la lb
        let ax = a.X/la
        let ay = a.Y/la
        let bx = b.X/lb
        let by = b.Y/lb
        let dot = ax*bx + ay*by
        let cross = ax*by - ay*bx
        Rotation2D.createUnchecked (cross, dot)



    // These members need to be down here for inlining to work correctly:


    /// Create a new 2D Rotation that adds a counter clockwise angle in Degrees to the existing one.
    member inline r.AddDegrees(deg:float) =
        r.Add(Rotation2D.createFromDegrees deg)

    /// Create a new 2D Rotation that adds a counter clockwise angle in Radians to the existing one.
    member inline r.AddRadians(rad:float) =
        r.Add(Rotation2D.createFromRadians rad)