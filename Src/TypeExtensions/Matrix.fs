namespace Euclid

open Euclid.UtilEuclid
open EuclidErrors

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Matrix.
[<AutoOpen>]
module AutoOpenMatrix =

  type Matrix with

    static member createFromRigidMatrix (m:RigidMatrix) : Matrix =
        Matrix( m.M11,  m.M21,  m.M31,  m.X41,
                m.M12,  m.M22,  m.M32,  m.Y42,
                m.M13,  m.M23,  m.M33,  m.Z43,
                0    ,  0    ,  0    ,  1    )


    /// <summary>
    /// Creates a look-at matrix that orients a coordinate system at position looking towards target,
    /// using the world Z-axis as the up direction.
    /// </summary>
    /// <param name="position">The origin of the coordinate system.</param>
    /// <param name="target">The target point to look at.</param>
    static member createLookAt(position:Pnt, target:Pnt) :Matrix =
        let forwardVec = target - position
        if isTooTinySq forwardVec.LengthSq then
            fail $"Matrix.createLookAt failed because position and target are too close: position {position} target {target}"

        let forward = forwardVec.Unitized
        let up = UnitVec.createUnchecked(0, 0, 1)
        let rightVec = up.Cross(forward)
        if isTooTinySq rightVec.LengthSq then
            fail $"Matrix.createLookAt failed because direction is parallel to the Z-up vector: position {position} target {target}"

        let right = rightVec.Unitized
        let trueUp = forward.Cross(right)

        Matrix(
            right.X,   trueUp.X,   forward.X, position.X,
            right.Y,   trueUp.Y,   forward.Y, position.Y,
            right.Z,   trueUp.Z,   forward.Z, position.Z,
            0,         0,          0,         1)
