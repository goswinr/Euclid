namespace Euclid

open Euclid.UtilEuclid
open System.ComponentModel

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Matrix.
[<AutoOpen; EditorBrowsable(EditorBrowsableState.Never)>]
module AutoOpenMatrix =

  type Matrix with

    /// Create 4x4 matrix from 3x3 rigid matrix. The last column is set to (0,0,0,1).
    static member createFromRigidMatrix (m:RigidMatrix) : Matrix =
        Matrix( m.M11,  m.M21,  m.M31,  m.X41,
                m.M12,  m.M22,  m.M32,  m.Y42,
                m.M13,  m.M23,  m.M33,  m.Z43,
                0    ,  0    ,  0    ,  1    )

    /// <summary>Tries to decompose an affine Matrix into translation, rotation, scale and shear.
    /// Applied to a point in this order, they give the same transformation as the matrix:
    /// first scale, then shear, then rotate and last translate.
    /// The shear is upper triangular, so the X-axis is never sheared.
    /// For a mirroring matrix the scale along the X-axis is negative.
    /// Returns ValueNone if the matrix is projecting,
    /// or if it is singular, that is if a scale factor is less than 1e-12.</summary>
    /// <param name="m">The matrix to decompose.</param>
    /// <returns>ValueSome decomposition, or ValueNone for a projecting or singular matrix.</returns>
    static member tryDecompose (m:Matrix) : MatrixDecomposition voption =
        if m.IsProjecting then
            ValueNone
        else
            // Gram-Schmidt orthonormalization of the three column vectors c1, c2 and c3 into u1, u2 and u3:
            let c1x = m.M11
            let c1y = m.M12
            let c1z = m.M13
            let sx = sqrt(c1x*c1x + c1y*c1y + c1z*c1z)
            if isTooTiny sx then
                ValueNone
            else
                let u1x = c1x / sx
                let u1y = c1y / sx
                let u1z = c1z / sx
                // remove the part of c2 along u1:
                let d12 = u1x*m.M21 + u1y*m.M22 + u1z*m.M23
                let c2x = m.M21 - d12*u1x
                let c2y = m.M22 - d12*u1y
                let c2z = m.M23 - d12*u1z
                let sy = sqrt(c2x*c2x + c2y*c2y + c2z*c2z)
                if isTooTiny sy then
                    ValueNone
                else
                    let u2x = c2x / sy
                    let u2y = c2y / sy
                    let u2z = c2z / sy
                    // remove the part of c3 along u1, then the part along u2:
                    let d13 = u1x*m.M31 + u1y*m.M32 + u1z*m.M33
                    let e3x = m.M31 - d13*u1x
                    let e3y = m.M32 - d13*u1y
                    let e3z = m.M33 - d13*u1z
                    let d23 = u2x*e3x + u2y*e3y + u2z*e3z
                    let c3x = e3x - d23*u2x
                    let c3y = e3y - d23*u2y
                    let c3z = e3z - d23*u2z
                    let sz = sqrt(c3x*c3x + c3y*c3y + c3z*c3z)
                    if isTooTiny sz then
                        ValueNone
                    else
                        let u3x = c3x / sz
                        let u3y = c3y / sz
                        let u3z = c3z / sz
                        // A left-handed basis means a mirroring matrix. Then flip u1, and with it the X scale and the shears towards X.
                        let det = (u1y*u2z - u1z*u2y)*u3x + (u1z*u2x - u1x*u2z)*u3y + (u1x*u2y - u1y*u2x)*u3z
                        let sign = if det < 0.0 then -1.0 else 1.0
                        let rot =
                            RigidMatrix(
                                sign*u1x, u2x, u3x, 0,
                                sign*u1y, u2y, u3y, 0,
                                sign*u1z, u2z, u3z, 0)
                        let q = Quaternion.createFromRigidMatrix rot
                        ValueSome (MatrixDecomposition(
                                        m.X41, m.Y42, m.Z43,
                                        q.X, q.Y, q.Z, q.W,
                                        sign*sx, sy, sz,
                                        sign*d12/sy, sign*d13/sz, d23/sz))

    /// <summary>Decomposes an affine Matrix into translation, rotation, scale and shear.
    /// Applied to a point in this order, they give the same transformation as the matrix:
    /// first scale, then shear, then rotate and last translate.
    /// The shear is upper triangular, so the X-axis is never sheared.
    /// For a mirroring matrix the scale along the X-axis is negative.
    /// Fails if the matrix is projecting,
    /// or if it is singular, that is if a scale factor is less than 1e-12.</summary>
    /// <param name="m">The matrix to decompose.</param>
    /// <returns>The decomposition. d.ToMatrix recomposes the matrix.</returns>
    static member decompose (m:Matrix) : MatrixDecomposition =
        match Matrix.tryDecompose m with
        | ValueSome d -> d
        | ValueNone -> EuclidErrors.fail $"Matrix.decompose failed. The matrix is projecting or singular:{Format.nl}{m.AsString}"

    /// <summary>Tries to decompose this affine Matrix into translation, rotation, scale and shear.
    /// Applied to a point in this order, they give the same transformation as the matrix:
    /// first scale, then shear, then rotate and last translate.
    /// Returns ValueNone if the matrix is projecting,
    /// or if it is singular, that is if a scale factor is less than 1e-12.</summary>
    /// <returns>ValueSome decomposition, or ValueNone for a projecting or singular matrix.</returns>
    member m.TryDecompose() : MatrixDecomposition voption =
        Matrix.tryDecompose m

    /// <summary>Decomposes this affine Matrix into translation, rotation, scale and shear.
    /// Applied to a point in this order, they give the same transformation as the matrix:
    /// first scale, then shear, then rotate and last translate.
    /// Fails if the matrix is projecting,
    /// or if it is singular, that is if a scale factor is less than 1e-12.</summary>
    /// <returns>The decomposition. d.ToMatrix recomposes the matrix.</returns>
    member m.Decompose() : MatrixDecomposition =
        Matrix.decompose m

    /// <summary>Recomposes a Matrix from its translation, rotation, scale and shear.
    /// Same as d.ToMatrix.</summary>
    /// <param name="d">The decomposition to recompose.</param>
    /// <returns>A new affine Matrix.</returns>
    static member createFromDecomposition (d:MatrixDecomposition) : Matrix =
        d.ToMatrix

    /// <summary>Interpolates from start to finish for animations,
    /// by decomposing both into translation, rotation, scale and shear and interpolating these.
    /// The translation, scale and shear are interpolated linearly.
    /// The rotation is interpolated with Quaternion.slerp along the shortest rotation, at a constant angular speed.
    /// Unlike an interpolation of the 16 matrix elements, this keeps a rotation rigid and does not shrink it on the way.
    /// Factor 0 returns start and factor 1 returns finish.
    /// The factor is not clamped: negative factors and factors above one extrapolate.
    /// If only one of the matrices is mirroring, the interpolated matrix collapses on the way, when its X scale passes zero.
    /// Fails if the factor is not finite, or if start or finish is projecting or singular.</summary>
    /// <param name="start">The matrix at factor 0.</param>
    /// <param name="finish">The matrix at factor 1.</param>
    /// <param name="factor">The finite interpolation factor.</param>
    /// <returns>A new interpolated affine Matrix.</returns>
    static member interpolate (start:Matrix) (finish:Matrix) (factor:float) : Matrix =
        if isNanInfinity factor then
            EuclidErrors.fail $"Matrix.interpolate failed. The factor must be finite: {factor}."
        match Matrix.tryDecompose start with
        | ValueNone -> EuclidErrors.fail $"Matrix.interpolate failed. The start matrix is projecting or singular:{Format.nl}{start.AsString}"
        | ValueSome a ->
            match Matrix.tryDecompose finish with
            | ValueNone -> EuclidErrors.fail $"Matrix.interpolate failed. The finish matrix is projecting or singular:{Format.nl}{finish.AsString}"
            | ValueSome b ->
                if factor = 0.0 then
                    start
                elif factor = 1.0 then
                    finish
                else
                    let d = a.Interpolate(b, factor)
                    d.ToMatrix

    /// <summary>Interpolates from this matrix to finish for animations,
    /// by decomposing both into translation, rotation, scale and shear and interpolating these.
    /// The translation, scale and shear are interpolated linearly.
    /// The rotation is interpolated with Quaternion.slerp along the shortest rotation, at a constant angular speed.
    /// Factor 0 returns this matrix and factor 1 returns finish.
    /// The factor is not clamped: negative factors and factors above one extrapolate.
    /// If only one of the matrices is mirroring, the interpolated matrix collapses on the way, when its X scale passes zero.
    /// Fails if the factor is not finite, or if this matrix or finish is projecting or singular.</summary>
    /// <param name="finish">The matrix at factor 1.</param>
    /// <param name="factor">The finite interpolation factor.</param>
    /// <returns>A new interpolated affine Matrix.</returns>
    member m.Interpolate (finish:Matrix, factor:float) : Matrix =
        Matrix.interpolate m finish factor

