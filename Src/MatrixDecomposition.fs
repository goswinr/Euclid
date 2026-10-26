namespace Euclid

open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors
#if !FABLE_COMPILER
open System.Text.Json.Serialization
#endif

// The decomposition follows the 'unmatrix' method of Spencer W. Thomas, Graphics Gems II, 1991,
// adapted to column vectors and to the Euclid application order.
// It is also used to interpolate CSS transforms.


/// <summary>A struct containing 13 floats, representing an immutable affine 4x4 Matrix decomposed into
/// translation, rotation, scale and shear.
/// Use Matrix.decompose to create one, and d.ToMatrix to recompose it.
/// The components are applied to a point in this order: first Scale, then Shear, then Rotation and last Translation.
/// So the recomposed matrix is the same as:
/// <code>
/// Matrix.createScale d.ScaleX d.ScaleY d.ScaleZ
/// *** Matrix.createShear(0, 0, d.ShearYX, 0, d.ShearZX, d.ShearZY)
/// *** Matrix.createFromQuaternion d.Rotation
/// *** Matrix.createTranslation d.Translation
/// </code>
/// The shear is upper triangular, so the X-axis is never sheared, the Y-axis is only sheared towards the X-axis,
/// and the Z-axis is sheared towards the X- and Y-axis.
/// A mirroring matrix has a negative ScaleX.
/// Interpolating two decompositions component by component gives smooth animations between two matrices.
/// Note: Never use the struct default constructor MatrixDecomposition() as it will create an invalid zero rotation.
/// Use MatrixDecomposition.create instead.</summary>
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
[<DataContract>] // for using DataMember on fields, for Newtonsoft.Json
#if !FABLE_COMPILER
[<JsonConverter(typeof<MatrixDecompositionJsonConverter>)>]
#endif
type MatrixDecomposition =

    /// The X component of the translation.
    [<DataMember>] val TranslationX : float
    /// The Y component of the translation.
    [<DataMember>] val TranslationY : float
    /// The Z component of the translation.
    [<DataMember>] val TranslationZ : float
    /// The X component of the rotation quaternion.
    [<DataMember>] val RotationX : float
    /// The Y component of the rotation quaternion.
    [<DataMember>] val RotationY : float
    /// The Z component of the rotation quaternion.
    [<DataMember>] val RotationZ : float
    /// The W component of the rotation quaternion.
    [<DataMember>] val RotationW : float
    /// The scale factor along the X-axis. It is negative for mirroring matrices.
    [<DataMember>] val ScaleX : float
    /// The scale factor along the Y-axis.
    [<DataMember>] val ScaleY : float
    /// The scale factor along the Z-axis.
    [<DataMember>] val ScaleZ : float
    /// The amount to shear the X component when moving along the Y-axis.
    /// Same as the yx argument of Matrix.createShear.
    [<DataMember>] val ShearYX : float
    /// The amount to shear the X component when moving along the Z-axis.
    /// Same as the zx argument of Matrix.createShear.
    [<DataMember>] val ShearZX : float
    /// The amount to shear the Y component when moving along the Z-axis.
    /// Same as the zy argument of Matrix.createShear.
    [<DataMember>] val ShearZY : float

    /// Unsafe internal constructor. The rotation components must form a unit quaternion.
    internal new(   translationX, translationY, translationZ,
                    rotationX, rotationY, rotationZ, rotationW,
                    scaleX, scaleY, scaleZ,
                    shearYX, shearZX, shearZY) = {
                        TranslationX=translationX; TranslationY=translationY; TranslationZ=translationZ
                        RotationX=rotationX; RotationY=rotationY; RotationZ=rotationZ; RotationW=rotationW
                        ScaleX=scaleX; ScaleY=scaleY; ScaleZ=scaleZ
                        ShearYX=shearYX; ShearZX=shearZX; ShearZY=shearZY }

    /// <summary>Returns the translation part as a vector.</summary>
    member d.Translation : Vec =
        Vec(d.TranslationX, d.TranslationY, d.TranslationZ)

    /// <summary>Returns the rotation part as a unit quaternion.</summary>
    member d.Rotation : Quaternion =
        Quaternion.createUnchecked(d.RotationX, d.RotationY, d.RotationZ, d.RotationW)

    /// <summary>Returns the three scale factors as a vector.</summary>
    member d.Scale : Vec =
        Vec(d.ScaleX, d.ScaleY, d.ScaleZ)

    /// <summary>Nicely formats the decomposition, showing the rotation axis and angle.</summary>
    member d.AsString : string =
        let f = Format.float
        let t = d.Translation
        let r = d.Rotation
        $"Translation: {t.AsString}, Rotation: {r.AsString}, Scale: X={f d.ScaleX}|Y={f d.ScaleY}|Z={f d.ScaleZ}, Shear: YX={f d.ShearYX}|ZX={f d.ShearZX}|ZY={f d.ShearZY}"

    /// <summary>Nicely formats the decomposition, showing the rotation axis and angle.</summary>
    static member inline asString (d:MatrixDecomposition) : string =
        d.AsString

    /// <summary>Formats the decomposition into an F# code string that can be used to recreate it.</summary>
    member d.AsFSharpCode : string =
        let f = Format.floatAsFSharpCode
        $"MatrixDecomposition.create (Vec({f d.TranslationX}, {f d.TranslationY}, {f d.TranslationZ})) (Quaternion.create({f d.RotationX}, {f d.RotationY}, {f d.RotationZ}, {f d.RotationW})) (Vec({f d.ScaleX}, {f d.ScaleY}, {f d.ScaleZ})) {f d.ShearYX} {f d.ShearZX} {f d.ShearZY}"

    /// <summary>Formats the decomposition into an F# code string that can be used to recreate it.</summary>
    static member inline asFSharpCode (d:MatrixDecomposition) : string =
        d.AsFSharpCode

    /// <summary>Nicely formats the decomposition, showing the rotation axis and angle.</summary>
    override d.ToString() : string =
        $"Euclid.MatrixDecomposition({d.AsString})"

    /// <summary>Recomposes the 4x4 Matrix:
    /// first scale, then shear, then rotate and last translate.</summary>
    member d.ToMatrix : Matrix =
        // the rotation matrix, as in Matrix.createFromQuaternion
        let x = d.RotationX
        let y = d.RotationY
        let z = d.RotationZ
        let w = d.RotationW
        let x2 = x + x
        let y2 = y + y
        let z2 = z + z
        let xx = x * x2
        let xy = x * y2
        let xz = x * z2
        let yy = y * y2
        let yz = y * z2
        let zz = z * z2
        let wx = w * x2
        let wy = w * y2
        let wz = w * z2
        // r12 is the element in row 1 and column 2
        let r11 = 1. - (yy + zz)
        let r12 = xy - wz
        let r13 = xz + wy
        let r21 = xy + wz
        let r22 = 1. - (xx + zz)
        let r23 = yz - wx
        let r31 = xz - wy
        let r32 = yz + wx
        let r33 = 1. - (xx + yy)
        // the rotation matrix multiplied with the upper triangular shear and scale matrix:
        // column 1: sx * r1
        // column 2: sy * (yx * r1 + r2)
        // column 3: sz * (zx * r1 + zy * r2 + r3)
        let sx = d.ScaleX
        let sy = d.ScaleY
        let sz = d.ScaleZ
        let yx = d.ShearYX
        let zx = d.ShearZX
        let zy = d.ShearZY
        Matrix(
            sx*r11, sy*(yx*r11 + r12), sz*(zx*r11 + zy*r12 + r13), d.TranslationX,
            sx*r21, sy*(yx*r21 + r22), sz*(zx*r21 + zy*r22 + r23), d.TranslationY,
            sx*r31, sy*(yx*r31 + r32), sz*(zx*r31 + zy*r32 + r33), d.TranslationZ,
            0     , 0                , 0                         , 1             )

    /// <summary>Recomposes the 4x4 Matrix:
    /// first scale, then shear, then rotate and last translate.</summary>
    /// <param name="d">The decomposition to recompose.</param>
    static member inline toMatrix (d:MatrixDecomposition) : Matrix =
        d.ToMatrix

    /// <summary>Interpolates from this decomposition to finish.
    /// The translation, scale and shear are interpolated linearly.
    /// The rotation is interpolated with Quaternion.slerp along the shortest rotation, at a constant angular speed.
    /// Factor 0 returns this decomposition and factor 1 returns finish, but finish's rotation might have all components negated.
    /// The factor is not clamped: negative factors and factors above one extrapolate.
    /// If the scale of one axis changes its sign, the interpolated matrix collapses on the way, when this scale passes zero.
    /// Fails if the factor is not finite.</summary>
    /// <param name="finish">The decomposition at factor 1.</param>
    /// <param name="factor">The finite interpolation factor.</param>
    /// <returns>A new interpolated MatrixDecomposition.</returns>
    member d.Interpolate (finish:MatrixDecomposition, factor:float) : MatrixDecomposition =
        if isNanInfinity factor then
            fail $"MatrixDecomposition.Interpolate failed. The factor must be finite: {factor}, for start {d.AsString} and finish {finish.AsString}."
        let inline lerp (a:float) (b:float) = a + factor * (b - a)
        let r = d.Rotation
        let q = r.Slerp(finish.Rotation, factor)
        MatrixDecomposition(
            lerp d.TranslationX finish.TranslationX,
            lerp d.TranslationY finish.TranslationY,
            lerp d.TranslationZ finish.TranslationZ,
            q.X, q.Y, q.Z, q.W,
            lerp d.ScaleX finish.ScaleX,
            lerp d.ScaleY finish.ScaleY,
            lerp d.ScaleZ finish.ScaleZ,
            lerp d.ShearYX finish.ShearYX,
            lerp d.ShearZX finish.ShearZX,
            lerp d.ShearZY finish.ShearZY)

    /// <summary>Interpolates from start to finish.
    /// The translation, scale and shear are interpolated linearly.
    /// The rotation is interpolated with Quaternion.slerp along the shortest rotation, at a constant angular speed.
    /// Factor 0 returns start and factor 1 returns finish, but finish's rotation might have all components negated.
    /// The factor is not clamped: negative factors and factors above one extrapolate.
    /// If the scale of one axis changes its sign, the interpolated matrix collapses on the way, when this scale passes zero.
    /// Fails if the factor is not finite.</summary>
    /// <param name="start">The decomposition at factor 0.</param>
    /// <param name="finish">The decomposition at factor 1.</param>
    /// <param name="factor">The finite interpolation factor.</param>
    /// <returns>A new interpolated MatrixDecomposition.</returns>
    static member inline interpolate (start:MatrixDecomposition) (finish:MatrixDecomposition) (factor:float) : MatrixDecomposition =
        start.Interpolate(finish, factor)

    /// <summary>Checks if two decompositions are equal within tolerance.
    /// By comparing the 13 fields from TranslationX to ShearZY each with the given tolerance,
    /// not by a Euclidean distance.
    /// A rotation quaternion with every component negated represents the same rotation but will be considered unequal.
    /// Use MatrixDecomposition.equalsExact to check for an exact match.</summary>
    /// <param name="tol">The tolerance for comparing each field.</param>
    /// <param name="a">The first decomposition.</param>
    /// <param name="b">The second decomposition.</param>
    static member equalsWithin (tol:float) (a:MatrixDecomposition) (b:MatrixDecomposition) : bool =
        abs(a.TranslationX-b.TranslationX) <= tol &&
        abs(a.TranslationY-b.TranslationY) <= tol &&
        abs(a.TranslationZ-b.TranslationZ) <= tol &&
        abs(a.RotationX-b.RotationX) <= tol &&
        abs(a.RotationY-b.RotationY) <= tol &&
        abs(a.RotationZ-b.RotationZ) <= tol &&
        abs(a.RotationW-b.RotationW) <= tol &&
        abs(a.ScaleX-b.ScaleX) <= tol &&
        abs(a.ScaleY-b.ScaleY) <= tol &&
        abs(a.ScaleZ-b.ScaleZ) <= tol &&
        abs(a.ShearYX-b.ShearYX) <= tol &&
        abs(a.ShearZX-b.ShearZX) <= tol &&
        abs(a.ShearZY-b.ShearZY) <= tol

    /// <summary>Checks if two decompositions are exactly equal.
    /// Compares each field with the = operator, so a NaN value is never equal.</summary>
    /// <param name="a">The first decomposition.</param>
    /// <param name="b">The second decomposition.</param>
    static member equalsExact (a:MatrixDecomposition) (b:MatrixDecomposition) : bool =
        a.TranslationX = b.TranslationX &&
        a.TranslationY = b.TranslationY &&
        a.TranslationZ = b.TranslationZ &&
        a.RotationX = b.RotationX &&
        a.RotationY = b.RotationY &&
        a.RotationZ = b.RotationZ &&
        a.RotationW = b.RotationW &&
        a.ScaleX = b.ScaleX &&
        a.ScaleY = b.ScaleY &&
        a.ScaleZ = b.ScaleZ &&
        a.ShearYX = b.ShearYX &&
        a.ShearZX = b.ShearZX &&
        a.ShearZY = b.ShearZY

    /// <summary>Returns the decomposition of the identity matrix:
    /// no translation, no rotation, a scale of one and no shear.</summary>
    static member identity : MatrixDecomposition =
        MatrixDecomposition(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0)

    /// <summary>Creates a decomposition from its components.
    /// When recomposed, they are applied to a point in this order: first scale, then shear, then rotate and last translate.</summary>
    /// <param name="translation">The translation, applied last.</param>
    /// <param name="rotation">The rotation, applied after the shear.</param>
    /// <param name="scale">The scale factors along the X-, Y- and Z-axis, applied first.</param>
    /// <param name="shearYX">The amount to shear the X component when moving along the Y-axis, applied after the scale.</param>
    /// <param name="shearZX">The amount to shear the X component when moving along the Z-axis, applied after the scale.</param>
    /// <param name="shearZY">The amount to shear the Y component when moving along the Z-axis, applied after the scale.</param>
    /// <returns>A new MatrixDecomposition.</returns>
    static member create (translation:Vec) (rotation:Quaternion) (scale:Vec) (shearYX:float) (shearZX:float) (shearZY:float) : MatrixDecomposition =
        MatrixDecomposition(
            translation.X, translation.Y, translation.Z,
            rotation.X, rotation.Y, rotation.Z, rotation.W,
            scale.X, scale.Y, scale.Z,
            shearYX, shearZX, shearZY)


#if !FABLE_COMPILER
/// Serializes a MatrixDecomposition as its 13 fields with System.Text.Json.
and internal MatrixDecompositionJsonConverter() =
    inherit JsonConverter<MatrixDecomposition>()

    override _.Write(writer, d, options) =
        writer.WriteStartObject()
        JsonConvert.writeFloat writer options "TranslationX" d.TranslationX
        JsonConvert.writeFloat writer options "TranslationY" d.TranslationY
        JsonConvert.writeFloat writer options "TranslationZ" d.TranslationZ
        JsonConvert.writeFloat writer options "RotationX" d.RotationX
        JsonConvert.writeFloat writer options "RotationY" d.RotationY
        JsonConvert.writeFloat writer options "RotationZ" d.RotationZ
        JsonConvert.writeFloat writer options "RotationW" d.RotationW
        JsonConvert.writeFloat writer options "ScaleX" d.ScaleX
        JsonConvert.writeFloat writer options "ScaleY" d.ScaleY
        JsonConvert.writeFloat writer options "ScaleZ" d.ScaleZ
        JsonConvert.writeFloat writer options "ShearYX" d.ShearYX
        JsonConvert.writeFloat writer options "ShearZX" d.ShearZX
        JsonConvert.writeFloat writer options "ShearZY" d.ShearZY
        writer.WriteEndObject()

    override _.Read(reader, _, options) =
        JsonConvert.readStartObject &reader "Euclid.MatrixDecomposition"
        let tx = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "TranslationX"
        let ty = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "TranslationY"
        let tz = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "TranslationZ"
        let rx = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "RotationX"
        let ry = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "RotationY"
        let rz = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "RotationZ"
        let rw = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "RotationW"
        let sx = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "ScaleX"
        let sy = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "ScaleY"
        let sz = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "ScaleZ"
        let yx = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "ShearYX"
        let zx = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "ShearZX"
        let zy = JsonConvert.readFloat &reader options "Euclid.MatrixDecomposition" "ShearZY"
        JsonConvert.readEndObject &reader "Euclid.MatrixDecomposition"
        let q = Quaternion.create(rx, ry, rz, rw)
        MatrixDecomposition(tx, ty, tz, q.X, q.Y, q.Z, q.W, sx, sy, sz, yx, zx, zy)
#endif
