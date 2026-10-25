namespace Euclid

open Euclid.UtilEuclid

/// When Euclid is opened, this module will be auto-opened.
/// It only contains extension members for the type Quaternion.
[<AutoOpen>]
module AutoOpenQuaternion =

  type Quaternion with

    /// <summary>Scales the encoded rotation angle in [0, 360] degrees, preserving the encoded axis.
    /// Negative factors reverse the rotation; factors above one extend it. The factor is not clamped.
    /// Unlike Half, this does not choose the shortest rotation.
    /// Fails for (0, 0, 0, -1), whose full-turn axis is undefined, or if the factor or scaled half angle is not finite.</summary>
    /// <param name="factor">The finite multiplier applied to the encoded angle.</param>
    /// <returns>A new unit quaternion; ordinary identity remains identity.</returns>
    member q.ScaleAngle (factor:float) : Quaternion =
        if isNanInfinity factor then
            EuclidErrors.fail $"Quaternion.ScaleAngle failed. The factor must be finite: {factor}."
        // Scale before taking the length to retain even very small encoded axes.
        let axisScale = max (abs q.X) (max (abs q.Y) (abs q.Z))
        if axisScale = 0.0 then
            if q.W < 0.0 then
                EuclidErrors.fail $"Quaternion.ScaleAngle failed. The encoded full-turn axis is undefined for quaternion {q.AsString} and factor {factor}."
            Quaternion.identity
        else
            let x = q.X / axisScale
            let y = q.Y / axisScale
            let z = q.Z / axisScale
            let length = sqrt(x*x + y*y + z*z) // At least one component has magnitude one.
            let halfAngle = atan2 (axisScale * length) q.W
            let scaledHalfAngle = halfAngle * factor
            if isNanInfinity scaledHalfAngle then
                EuclidErrors.fail $"Quaternion.ScaleAngle failed. The scaled half angle is not finite for factor {factor} and quaternion {q.AsString}."
            let scale = sin scaledHalfAngle / length
            Quaternion.createUnchecked(x * scale, y * scale, z * scale, cos scaledHalfAngle)

    /// <summary>Scales the encoded angle, preserving its axis without choosing the shortest rotation.
    /// The factor is not clamped. Fails for (0, 0, 0, -1), whose axis is undefined,
    /// or if the factor or scaled half angle is not finite.</summary>
    /// <param name="factor">The finite angle multiplier; negative values and values above one are supported.</param>
    /// <param name="rotation">The unit quaternion whose encoded angle is scaled.</param>
    /// <returns>A new unit quaternion; ordinary identity remains identity.</returns>
    static member inline scaleAngle (factor:float) (rotation:Quaternion) : Quaternion =
        rotation.ScaleAngle factor

    /// <summary>Returns half the shortest rotation.
    /// Preserves the encoded axis for rotations between -180 and 180 degrees, inclusive.
    /// Reverses the axis only when the encoded rotation exceeds 180 degrees in magnitude (W is negative).</summary>
    /// <returns>A unit quaternion whose composition with itself represents this rotation.</returns>
    member q.Half : Quaternion =
        let sign = if q.W < 0.0 then -1.0 else 1.0
        Quaternion.create(sign*q.X, sign*q.Y, sign*q.Z, 1.0 + abs q.W)

    /// <summary>Returns half the shortest rotation.
    /// Preserves the encoded axis for rotations between -180 and 180 degrees, inclusive.
    /// Reverses the axis only when the encoded rotation exceeds 180 degrees in magnitude (W is negative).</summary>
    /// <param name="rotation">The unit quaternion to halve.</param>
    /// <returns>The half rotation; both identity representations return identity.</returns>
    static member inline half (rotation:Quaternion) : Quaternion = rotation.Half

    /// If the matrix is affine and has no shear or scaling, it returns the rotation defined in its 3x3 part.
    static member tryCreateFromMatrix(matrix:Matrix) : voption<Quaternion> =
        match RigidMatrix.tryCreateFromMatrix(matrix) with
        |ValueNone -> ValueNone
        |ValueSome m ->
            // http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm
            // assumes the upper 3x3 of m is a pure rotation matrix (i.e. unscaled)
            let m11 = m.M11
            let m12 = m.M21
            let m13 = m.M31
            let m21 = m.M12
            let m22 = m.M22
            let m23 = m.M32
            let m31 = m.M13
            let m32 = m.M23
            let m33 = m.M33
            let trace = m11 + m22 + m33
            if  trace > 0  then
                let s = 0.5 / sqrt(trace + 1.0)
                ValueSome <| Quaternion.createUnchecked(
                    (m32 - m23) * s,
                    (m13 - m31) * s,
                    (m21 - m12) * s,
                    0.25 / s       )

            elif  m11 > m22 && m11 > m33  then
                let s = 2.0 * sqrt(1.0 + m11 - m22 - m33)
                ValueSome <| Quaternion.createUnchecked(
                    0.25 * s        ,
                    (m12 + m21) / s ,
                    (m13 + m31) / s ,
                    (m32 - m23) / s )

            elif  m22 > m33  then
                let s = 2.0 * sqrt(1.0 + m22 - m11 - m33)
                ValueSome <| Quaternion.createUnchecked(
                    (m12 + m21) / s ,
                    0.25 * s        ,
                    (m23 + m32) / s ,
                    (m13 - m31) / s )
            else
                let s = 2.0 * sqrt(1.0 + m33 - m11 - m22)
                ValueSome <| Quaternion.createUnchecked(
                    (m13 + m31) / s,
                    (m23 + m32) / s,
                    0.25 * s       ,
                    (m21 - m12) / s )

    /// Returns the rotation defined in a RigidMatrix's 3x3 part.
    static member createFromRigidMatrix(orthoMatrix:RigidMatrix) : Quaternion =
        // http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm
        // assumes the upper 3x3 of m is a pure rotation matrix (i.e, unscaled)
        let m11 = orthoMatrix.M11
        let m12 = orthoMatrix.M21
        let m13 = orthoMatrix.M31
        let m21 = orthoMatrix.M12
        let m22 = orthoMatrix.M22
        let m23 = orthoMatrix.M32
        let m31 = orthoMatrix.M13
        let m32 = orthoMatrix.M23
        let m33 = orthoMatrix.M33
        let trace = m11 + m22 + m33
        if  trace > 0  then
            let s = 0.5 / sqrt(trace + 1.0)
            Quaternion.createUnchecked(
                (m32 - m23) * s,
                (m13 - m31) * s,
                (m21 - m12) * s,
                0.25 / s       )

        elif  m11 > m22 && m11 > m33  then
            let s = 2.0 * sqrt(1.0 + m11 - m22 - m33)
            Quaternion.createUnchecked(
                0.25 * s          ,
                (m12 + m21) / s ,
                (m13 + m31) / s ,
                (m32 - m23) / s)

        elif  m22 > m33  then
            let s = 2.0 * sqrt(1.0 + m22 - m11 - m33)
            Quaternion.createUnchecked(
                (m12 + m21) / s ,
                0.25 * s          ,
                (m23 + m32) / s ,
                (m13 - m31) / s)
        else
            let s = 2.0 * sqrt(1.0 + m33 - m11 - m22)
            Quaternion.createUnchecked(
                (m13 + m31) / s,
                (m23 + m32) / s,
                0.25 * s         ,
                (m21 - m12) / s)

