namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar

#nowarn "44" // for hidden constructors via Obsolete Attribute

/// 2D Counter Clockwise rotation. It can be applied in World X, Y or Z plane.
/// Internally stored just as a Sine and Cosine value.
/// For arbitrary rotations use Quaternions or 4x4 Matrix.
/// However this module has much better performance than the more general Matrix4x4 or a Quaternion.
/// Note: Never use the struct default constructor Rotation2D() as it will create an invalid zero Rotation2D.
/// Use Rotation2D.create or Rotation2D.createUnchecked instead.
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Rotation2D =

    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar

    /// The Sine component of this rotation.
    /// The range of these field is -1.0 to +1.0
    [<DataMember>] val Sin : float

    /// The Cosine component of this rotation.
    /// The range of these field is -1.0 to +1.0
    [<DataMember>] val Cos : float

    /// Unsafe internal constructor, public only for inlining.
    [<Obsolete("This is not Obsolete, but an unsafe internal constructor. the input is not verified, so it might create invalid geometry. It is exposed as a public member so that it can be inlined. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >]
    new (sin, cos) =
        #if DEBUG
        let sum = sin*sin + cos*cos in
        if 0.99999 > sum || sum > 1.00001  then  EuclidException.Raisef "Euclid.Rotate Constructor failed for sin:%g and cos:%g.  Because sin*sin + cos*cos needs to be 1.0." sin cos
        #endif
        {Sin = sin; Cos = cos}

    /// Format rotation into string showing angle in Degrees as nicely formatted floating point number.
    override r.ToString() =
        let deg = r.Sin  |> asinSafe |> toDegrees |> Format.float
        sprintf "Euclid.Rotation2D of %s° Degrees." deg

    /// Returns the angle represented by this 2D Rotation in Radians.
    member inline r.InRadians =
        r.Sin  |>  asinSafe

    /// Returns the angle represented by this 2D Rotation in Degrees.
    member inline r.InDegrees =
        r.InRadians|> toDegrees

    /// Returns the 2D Rotation in the opposite direction.
    member inline r.Inverse = Rotation2D (-r.Sin, r.Cos)

    /// Create a new 2D Rotation that adds 2D Rotation to the existing one.
    member inline r.Add(ro:Rotation2D) =
        //use 2x2 matrix multiplication logic for better performance:
        Rotation2D  ( r.Sin*ro.Cos + r.Cos*ro.Sin
                    , r.Cos*ro.Cos - r.Sin*ro.Sin)

    /// Create a new 2D Rotation that adds and angle in Degrees to the existing one.
    member inline r.AddDegrees(deg:float) =
        r.Add(Rotation2D.createFromDegrees deg)

    /// Create a new 2D Rotation that adds and angle in Radians to the existing one.
    member inline r.AddRadians(rad:float) =
        r.Add(Rotation2D.createFromRadians rad)

    //----------------------------------------------------------------------------------------------
    //--------------------------  Static Members  --------------------------------------------------
    //----------------------------------------------------------------------------------------------

    /// Checks if two 2D Rotations are equal within tolerance.
    /// By comparing the fields Sin and Cos each with the given tolerance.
    /// The range of these field is -1.0 to +1.0
    /// Use a tolerance of 0.0 to check for an exact match.
    static member equals (tol:float) (a:Rotation2D) (b:Rotation2D) =
        abs(a.Sin-b.Sin) <= tol &&
        abs(a.Cos-b.Cos) <= tol

    ///Construct 2D Rotation from angle in Radians
    static member createFromRadians rad =
        Rotation2D (sin rad, cos rad)

    /// Construct 2D Rotation from angle in Degree.
    static member createFromDegrees deg =
        let rad = toRadians deg
        Rotation2D (sin rad, cos rad)

    /// Construct 2D Rotation from sine and corresponding cosine directly.
    /// Input is unchecked and not validated.
    static member createUnchecked (sine, cosine) =
        Rotation2D (sine, cosine)

    static member createFromVectors (a:UnitVc,b:UnitVc) =
        let dot = a *** b
        let cross = UnitVc.cross(a, b)
        Rotation2D (cross, dot)

    static member createFromVectors (a:Vc,b:Vc) =
        let la = a.Length
        let lb = b.Length
        if isTooTiny (la) || isTooTiny (lb) then
            EuclidException.Raisef "Euclid.Rotation2D.createFromVectors failed because one of the input vectors is too shorter than zeroLengthTolerance: a: %s, b: %s" a.AsString b.AsString
        let ax = a.X/la
        let ay = a.Y/la
        let bx = b.X/lb
        let by = b.Y/lb
        let dot = ax*bx + ay*by
        let cross = ax*by - ay*bx
        Rotation2D (cross, dot)

