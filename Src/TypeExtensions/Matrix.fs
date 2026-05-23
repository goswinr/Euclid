namespace Euclid

open Euclid.UtilEuclid

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Matrix.
[<AutoOpen>]
module AutoOpenMatrix =

  type Matrix with

    /// Create 4x4 matrix from 3x3 rigid matrix. The last column is set to (0,0,0,1).
    static member createFromRigidMatrix (m:RigidMatrix) : Matrix =
        Matrix( m.M11,  m.M21,  m.M31,  m.X41,
                m.M12,  m.M22,  m.M32,  m.Y42,
                m.M13,  m.M23,  m.M33,  m.Z43,
                0    ,  0    ,  0    ,  1    )

