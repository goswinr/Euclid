namespace Euclid

open Euclid.UtilEuclid
open System.ComponentModel

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type RigidMatrix.
[<AutoOpen; EditorBrowsable(EditorBrowsableState.Never)>]
module AutoOpenRigidMatrix =

  type RigidMatrix with

    /// <summary>Decomposes a RigidMatrix into its rotation and translation.
    /// The rotation is applied first and the translation second.</summary>
    /// <param name="m">The matrix to decompose.</param>
    /// <returns>A tuple of the rotation quaternion and the translation vector.
    /// RigidMatrix.createFromQuaternionAndTranslation recomposes the matrix.</returns>
    static member decompose (m:RigidMatrix) : Quaternion * Vec =
        Quaternion.createFromRigidMatrix m, m.Translation

    /// <summary>Decomposes this RigidMatrix into its rotation and translation.
    /// The rotation is applied first and the translation second.</summary>
    /// <returns>A tuple of the rotation quaternion and the translation vector.
    /// RigidMatrix.createFromQuaternionAndTranslation recomposes the matrix.</returns>
    member m.Decompose() : Quaternion * Vec =
        RigidMatrix.decompose m

    /// <summary>Creates a RigidMatrix that first rotates and then translates.</summary>
    /// <param name="rotation">The rotation, applied first.</param>
    /// <param name="translation">The translation, applied second.</param>
    /// <returns>A new RigidMatrix.</returns>
    static member createFromQuaternionAndTranslation (rotation:Quaternion) (translation:Vec) : RigidMatrix =
        let r = RigidMatrix.createFromQuaternion rotation
        RigidMatrix(
            r.M11, r.M21, r.M31, translation.X,
            r.M12, r.M22, r.M32, translation.Y,
            r.M13, r.M23, r.M33, translation.Z)

    /// <summary>Interpolates from start to finish for animations.
    /// The translation is interpolated linearly, so the origin moves on a straight line.
    /// The rotation is interpolated with Quaternion.slerp along the shortest rotation, at a constant angular speed.
    /// Unlike an interpolation of the 12 matrix elements, the result is always a valid RigidMatrix.
    /// Factor 0 returns start and factor 1 returns finish.
    /// The factor is not clamped: negative factors and factors above one extrapolate.
    /// Fails if the factor is not finite.</summary>
    /// <param name="start">The matrix at factor 0.</param>
    /// <param name="finish">The matrix at factor 1.</param>
    /// <param name="factor">The finite interpolation factor.</param>
    /// <returns>A new interpolated RigidMatrix.</returns>
    static member interpolate (start:RigidMatrix) (finish:RigidMatrix) (factor:float) : RigidMatrix =
        if isNanInfinity factor then
            EuclidErrors.fail $"RigidMatrix.interpolate failed. The factor must be finite: {factor}."
        if factor = 0.0 then
            start
        elif factor = 1.0 then
            finish
        else
            let qa = Quaternion.createFromRigidMatrix start
            let qb = Quaternion.createFromRigidMatrix finish
            let r = RigidMatrix.createFromQuaternion (qa.Slerp(qb, factor))
            RigidMatrix(
                r.M11, r.M21, r.M31, start.X41 + factor * (finish.X41 - start.X41),
                r.M12, r.M22, r.M32, start.Y42 + factor * (finish.Y42 - start.Y42),
                r.M13, r.M23, r.M33, start.Z43 + factor * (finish.Z43 - start.Z43))

    /// <summary>Interpolates from this matrix to finish for animations.
    /// The translation is interpolated linearly, so the origin moves on a straight line.
    /// The rotation is interpolated with Quaternion.slerp along the shortest rotation, at a constant angular speed.
    /// Factor 0 returns this matrix and factor 1 returns finish.
    /// The factor is not clamped: negative factors and factors above one extrapolate.
    /// Fails if the factor is not finite.</summary>
    /// <param name="finish">The matrix at factor 1.</param>
    /// <param name="factor">The finite interpolation factor.</param>
    /// <returns>A new interpolated RigidMatrix.</returns>
    member m.Interpolate (finish:RigidMatrix, factor:float) : RigidMatrix =
        RigidMatrix.interpolate m finish factor
