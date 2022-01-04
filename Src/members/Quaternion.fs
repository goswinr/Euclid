namespace FsEx.Geo

open FsEx.Geo.Util

/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type Quaternion.
[<AutoOpen>]
module AutoOpenQuaternion =

  type Quaternion with

    /// If the matrix is affine and has no shear or scaling it retuns the rotation defined in its 3x3 part.
    static member tryCreateFromMatrix( matrix:Matrix) : option<Quaternion> =
        match OrthoMatrix.tryCreateFromMatrix(matrix) with
        |None -> None
        |Some m ->
            // http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm
            // assumes the upper 3x3 of m is a pure rotation matrix (i.e, unscaled)
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
                let s = 0.5 / sqrt( trace + 1.0 )
                Some <| Quaternion.createDirectlyUnchecked(
                    ( m32 - m23 ) * s,
                    ( m13 - m31 ) * s,
                    ( m21 - m12 ) * s,
                    0.25 / s         )

            elif  m11 > m22 && m11 > m33  then
                let s = 2.0 * sqrt( 1.0 + m11 - m22 - m33 )
                Some <| Quaternion.createDirectlyUnchecked(
                    0.25 * s          ,
                    ( m12 + m21 ) / s ,
                    ( m13 + m31 ) / s ,
                    ( m32 - m23 ) / s )

            elif  m22 > m33  then
                let s = 2.0 * sqrt( 1.0 + m22 - m11 - m33 )
                Some <| Quaternion.createDirectlyUnchecked(
                    ( m12 + m21 ) / s ,
                    0.25 * s          ,
                    ( m23 + m32 ) / s ,
                    ( m13 - m31 ) / s )
            else
                let s = 2.0 * sqrt( 1.0 + m33 - m11 - m22 )
                Some <| Quaternion.createDirectlyUnchecked(
                    ( m13 + m31 ) / s,
                    ( m23 + m32 ) / s,
                    0.25 * s         ,
                    ( m21 - m12 ) / s)

    /// Returns the rotation defined in and Orthomatrix's 3x3 part.
    static member createFromOrthoMatrix( orthoMatrix:OrthoMatrix) : Quaternion =
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
            let s = 0.5 / sqrt( trace + 1.0 )
            Quaternion.createDirectlyUnchecked(
                ( m32 - m23 ) * s,
                ( m13 - m31 ) * s,
                ( m21 - m12 ) * s,
                0.25 / s         )

        elif  m11 > m22 && m11 > m33  then
            let s = 2.0 * sqrt( 1.0 + m11 - m22 - m33 )
            Quaternion.createDirectlyUnchecked(
                0.25 * s          ,
                ( m12 + m21 ) / s ,
                ( m13 + m31 ) / s ,
                ( m32 - m23 ) / s )

        elif  m22 > m33  then
            let s = 2.0 * sqrt( 1.0 + m22 - m11 - m33 )
            Quaternion.createDirectlyUnchecked(
                ( m12 + m21 ) / s ,
                0.25 * s          ,
                ( m23 + m32 ) / s ,
                ( m13 - m31 ) / s )
        else
            let s = 2.0 * sqrt( 1.0 + m33 - m11 - m22 )
            Quaternion.createDirectlyUnchecked(
                ( m13 + m31 ) / s,
                ( m23 + m32 ) / s,
                0.25 * s         ,
                ( m21 - m12 ) / s)
