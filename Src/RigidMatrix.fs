namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.UtilEuclid
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar
open EuclidErrors

// why 3x4 ?
// https://x.com/SebAaltonen/status/1761304184379347349
// @SebAaltonen:
// Yeah. 4x3 matrices are hard to beat. We use them too.
// Still some engines there using 4x4 matrices everywhere for simplicity.
// Not performance or memory optimal at all.


/// <summary>A struct containing 12 floats, representing an immutable 4x3 rigid matrix.
/// For only rotation and translation in 3D space.
/// This matrix guarantees to NOT scale, shear, flip, mirror, reflect or project.
/// Angles are preserved. Lengths are preserved. Area is preserved. Volume is preserved.
/// A rigid matrix is a matrix whose 3x3 columns and rows are orthogonal unit-vectors.
/// The matrix is represented in the following column-vector syntax form:
/// <code>
/// M11 M21 M31 X41
/// M12 M22 M32 Y42
/// M13 M23 M33 Z43
/// </code>
/// Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.
/// The Determinant of this matrix is always 1.0.
/// Note: Never use the struct default constructor RigidMatrix() as it will create an invalid zero RigidMatrix.
/// Use RigidMatrix.create instead.</summary>
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type RigidMatrix =
    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar
    [<DataMember>] val M11 :float ; [<DataMember>] val M21 :float ; [<DataMember>] val M31 :float; [<DataMember>] val X41:float
    [<DataMember>] val M12 :float ; [<DataMember>] val M22 :float ; [<DataMember>] val M32 :float; [<DataMember>] val Y42:float
    [<DataMember>] val M13 :float ; [<DataMember>] val M23 :float ; [<DataMember>] val M33 :float; [<DataMember>] val Z43:float

    /// <summary>Creates an immutable 4x3 Transformation Matrix. For only rotation and translation in 3D space.
    /// This Constructor takes arguments in row-major order.
    /// The matrix is represented in the following column-vector syntax form:
    /// <code>
    /// M11 M21 M31 X41
    /// M12 M22 M32 Y42
    /// M13 M23 M33 Z43
    /// </code>
    /// Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.</summary>
    internal new(   m11, m21, m31, x41,
                    m12, m22, m32, y42,
                    m13, m23, m33, z43) = {
                        M11=m11 ; M21=m21 ; M31=m31 ; X41=x41 ;
                        M12=m12 ; M22=m22 ; M32=m32 ; Y42=y42 ;
                        M13=m13 ; M23=m23 ; M33=m33 ; Z43=z43 }

    /// <summary>Returns the 12 elements column-major order:
    /// <code>[| M11 M12 M13 M21 M22 M23 M31 M32 M33 X41 Y42 Z43 |]</code>
    /// Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.</summary>
    member m.ToArrayByColumns =
        [| m.M11; m.M12; m.M13; m.M21; m.M22; m.M23; m.M31; m.M32; m.M33; m.X41; m.Y42; m.Z43 |]

    /// <summary>Returns the 12 elements in row-major order:
    /// <code>[| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 |]</code>
    /// Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.</summary>
    member m.ToArrayByRows =
        [| m.M11; m.M21; m.M31; m.X41; m.M12; m.M22; m.M32; m.Y42; m.M13; m.M23; m.M33; m.Z43 |]

    /// <summary>Nicely formats the Matrix to a Grid of 4x3.</summary>
    member m.AsString : string =
        let ts = m.ToArrayByRows |> Array.map (sprintf "%0.3f")
        let most = ts |> Array.maxBy (fun s -> s.Length)
        $"4x3 Column-Vector Rigid Transformation Matrix:{Format.nl}" + (
        ts
        |> Array.map (fun x -> String(' ', most.Length-x.Length) + x)
        |> Array.chunkBySize 4
        |> Array.map (fun es -> " " + String.concat " | " es)
        |> String.concat Environment.NewLine
        )

    /// <summary>Format RigidMatrix into an F# code string that can be used to recreate the matrix.</summary>
    member m.AsFSharpCode : string =
        $"RigidMatrix.create({m.M11}, {m.M21}, {m.M31}, {m.X41}, {m.M12}, {m.M22}, {m.M32}, {m.Y42}, {m.M13}, {m.M23}, {m.M33}, {m.Z43})"

    /// <summary>Nicely formats the Matrix to a Grid of 4x3 including field names.
    /// Using the following column-vector syntax form:
    /// <code>
    /// M11 M21 M31 X41
    /// M12 M22 M32 Y42
    /// M13 M23 M33 Z43
    /// </code>
    /// Where X41, Y42 and Z43 refer to the translation part of the matrix.</summary>
    override m.ToString()=
       let names =[| "M11"; "M21"; "M31"; "X41"; "M12"; "M22"; "M32"; "Y42"; "M13"; "M23"; "M33"; "Z43"|]
       let ts =  m.ToArrayByRows |> Array.map ( sprintf "%0.3f" )
       let most = ts |> Array.maxBy (fun s -> s.Length)
       $"Column-Vector Rigid Transformation Matrix:{Format.nl}" + (
       (names, ts)
       ||> Array.map2 (fun n v -> n + ": " + String(' ', most.Length-v.Length) + v)
       |> Array.chunkBySize 4
       |> Array.map (fun es -> " " + String.concat " | " es)
       |> String.concat Environment.NewLine
       )

    /// <summary>Returns the first column vector. M11, M12 and M13.</summary>
    member m.ColumnVector1 = Vec(m.M11, m.M12, m.M13)

    /// <summary>Returns the second column vector. M21, M22 and M23.</summary>
    member m.ColumnVector2 = Vec(m.M21, m.M22, m.M23)

    /// <summary>Returns the third column vector. M31, M32 and M33.</summary>
    member m.ColumnVector3 = Vec(m.M31, m.M32, m.M33)

    /// <summary>Returns the translation or fourth column vector. X41, Y42 and Z43.</summary>
    member m.Translation = Vec(m.X41, m.Y42, m.Z43)

    /// <summary>Converts the 3x4 RigidMatrix to a general 4x4 Matrix.</summary>
    member m.Matrix =
        // converts the RigidMatrix to a Matrix
        Matrix(
            m.M11, m.M21, m.M31, m.X41,
            m.M12, m.M22, m.M32, m.Y42,
            m.M13, m.M23, m.M33, m.Z43,
            0.0  , 0.0  , 0.0  , 1.0  )


    /// <summary>The Determinant of a Rigid Matrix is always 1.0.
    /// The Determinant describes the volume that a unit cube will have after the matrix was applied.
    /// This method only exists for testing.</summary>
    [<Obsolete("The Determinant of a Rigid Matrix is always 1.0.")>]
    member m.Determinant :float =
        let m = m.Matrix
        m.Determinant

    /// <summary>Inverts the RigidMatrix.
    /// Rigid matrices always have determinant 1.0 so they can always be inverted.</summary>
    member m.Inverse =
        // simplified from Matrix.Inverse in Matrix.fs:
        let n11 = m.M11
        let n21 = m.M21
        let n31 = m.M31
        let x41 = m.X41
        let n12 = m.M12
        let n22 = m.M22
        let n32 = m.M32
        let y42 = m.Y42
        let n13 = m.M13
        let n23 = m.M23
        let n33 = m.M33
        let z43 = m.Z43

        let n33y42 = n33*y42
        let n32z43 = n32*z43
        let n13n22 = n13*n22
        let n12n23 = n12*n23
        let n31z43 = n31*z43
        let n31y42 = n31*y42
        let n13n21 = n13*n21
        let n12n21 = n12*n21
        let n33x41 = n33*x41
        let n11n23 = n11*n23
        let n11n22 = n11*n22
        let n32x41 = n32*x41

        RigidMatrix
              (  n22*n33 - n23*n32                                                             // M11
              ,  n23*n31 - n21*n33                                                             // M21
              ,  n21*n32 - n22*n31                                                             // M31
              ,  n23*n32x41 - n22*n33x41 - n23*n31y42 + n21*n33y42 + n22*n31z43 - n21*n32z43   // X41
              ,  n13*n32 - n12*n33                                                             // M12
              ,  n11*n33 - n13*n31                                                             // M22
              ,  n12*n31 - n11*n32                                                             // M32
              ,  n12*n33x41 - n13*n32x41 + n13*n31y42 - n11*n33y42 - n12*n31z43 + n11*n32z43   // Y42
              ,  n12n23 - n13n22                                                               // M13
              ,  n13n21 - n11n23                                                               // M23
              ,  n11n22 - n12n21                                                               // M33
              ,  n13n22*x41 - n12n23*x41 - n13n21*y42 + n11n23*y42 + n12n21*z43 - n11n22*z43   // Z43
              )

    /// <summary>Checks if the Matrix is an Identity Matrix in the form of:
    /// <code>
    /// 1  0  0  0
    /// 0  1  0  0
    /// 0  0  1  0
    /// </code>
    /// Using an approximate tolerance of 1e-6.</summary>
    member m.IsIdentity =
        isOne  m.M11 && isZero m.M21 && isZero m.M31 && isZero m.X41 &&
        isZero m.M12 && isOne  m.M22 && isZero m.M32 && isZero m.Y42 &&
        isZero m.M13 && isZero m.M23 && isOne  m.M33 && isZero m.Z43


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
    // ------------------------------------------------------------------------------------




    /// <summary>Checks if two Matrices are equal within tolerance.
    /// By comparing the fields M11 to Z43 each with the given tolerance.
    /// Use a tolerance of 0.0 to check for an exact match.</summary>
    /// <param name="tol">The tolerance for comparing each matrix element.</param>
    /// <param name="a">The first matrix.</param>
    /// <param name="b">The second matrix.</param>
    static member equals (tol:float) (a:RigidMatrix) (b:RigidMatrix)  : bool =
        abs(a.M11-b.M11) <= tol &&
        abs(a.M12-b.M12) <= tol &&
        abs(a.M13-b.M13) <= tol &&
        abs(a.M21-b.M21) <= tol &&
        abs(a.M22-b.M22) <= tol &&
        abs(a.M23-b.M23) <= tol &&
        abs(a.M31-b.M31) <= tol &&
        abs(a.M32-b.M32) <= tol &&
        abs(a.M33-b.M33) <= tol &&
        abs(a.X41-b.X41) <= tol &&
        abs(a.Y42-b.Y42) <= tol &&
        abs(a.Z43-b.Z43) <= tol



    /// <summary>Multiplies matrixA with matrixB.
    /// The resulting transformation will first do matrixA and then matrixB.</summary>
    /// <param name="matrixA">The first matrix (applied first).</param>
    /// <param name="matrixB">The second matrix (applied second).</param>
    static member multiply (matrixA:RigidMatrix, matrixB:RigidMatrix) =
        let a11 = matrixA.M11
        let a12 = matrixA.M12
        let a13 = matrixA.M13
        let a21 = matrixA.M21
        let a22 = matrixA.M22
        let a23 = matrixA.M23
        let a31 = matrixA.M31
        let a32 = matrixA.M32
        let a33 = matrixA.M33
        let a41 = matrixA.X41
        let a42 = matrixA.Y42
        let a43 = matrixA.Z43

        let b11 = matrixB.M11
        let b12 = matrixB.M12
        let b13 = matrixB.M13
        let b21 = matrixB.M21
        let b22 = matrixB.M22
        let b23 = matrixB.M23
        let b31 = matrixB.M31
        let b32 = matrixB.M32
        let b33 = matrixB.M33
        RigidMatrix(
             a11*b11 + a12*b21 + a13*b31       , // M11
             a21*b11 + a22*b21 + a23*b31       , // M21
             a31*b11 + a32*b21 + a33*b31       , // M31
             a41*b11 + a42*b21 + a43*b31 + matrixB.X41 , // X41
             a11*b12 + a12*b22 + a13*b32       , // M12
             a21*b12 + a22*b22 + a23*b32       , // M22
             a31*b12 + a32*b22 + a33*b32       , // M32
             a41*b12 + a42*b22 + a43*b32 + matrixB.Y42 , // Y42
             a11*b13 + a12*b23 + a13*b33       , // M13
             a21*b13 + a22*b23 + a23*b33       , // M23
             a31*b13 + a32*b23 + a33*b33       , // M33
             a41*b13 + a42*b23 + a43*b33 + matrixB.Z43)  // Z43


    /// <summary>Multiplies matrixA with matrixB.
    /// The resulting transformation will first do matrixA and then matrixB.</summary>
    /// <param name="matrixA">The first matrix (applied first).</param>
    /// <param name="matrixB">The second matrix (applied second).</param>
    static member inline ( *** ) (matrixA:RigidMatrix, matrixB:RigidMatrix) = RigidMatrix.multiply(matrixA, matrixB)

    /// <summary>The Determinant of a Rigid Matrix is always 1.0.
    /// The Determinant describes the volume that a unit cube will have after the matrix was applied.</summary>
    /// <param name="m">The matrix.</param>
    [<Obsolete("The Determinant of a Rigid Matrix is always 1.0.")>]
    static member inline determinant (m:RigidMatrix) =
        let m = m.Matrix
        m.Determinant

    /// <summary>Inverts the RigidMatrix.
    /// A RigidMatrix can always be inverted. (as opposed to a general Matrix)</summary>
    /// <param name="m">The matrix to invert.</param>
    static member inline inverse (m:RigidMatrix) = m.Inverse


    /// <summary>Returns the Identity RigidMatrix:
    /// <code>
    /// 1  0  0  0
    /// 0  1  0  0
    /// 0  0  1  0
    /// </code></summary>
    static member identity =
        RigidMatrix(
            1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0)

    /// <summary>Creates a translation RigidMatrix.
    /// The resulting matrix will be:
    /// <code>
    /// 1  0  0  x
    /// 0  1  0  y
    /// 0  0  1  z
    /// </code></summary>
    /// <param name="x">The amount to translate in the X-axis.</param>
    /// <param name="y">The amount to translate in the Y-axis.</param>
    /// <param name="z">The amount to translate in the Z-axis.</param>
    static member createTranslation(x, y, z) =
        RigidMatrix(
            1, 0, 0, x,
            0, 1, 0, y,
            0, 0, 1, z)

    /// <summary>Creates a translation RigidMatrix.
    /// The resulting RigidMatrix will be:
    /// <code>
    /// 1  0  0  v.X
    /// 0  1  0  v.Y
    /// 0  0  1  v.Z
    /// </code></summary>
    /// <param name="v">The vector by which to translate.</param>
    static member createTranslation(v:Vec) =
        RigidMatrix(
            1, 0, 0, v.X,
            0, 1, 0, v.Y,
            0, 0, 1, v.Z)

    /// <summary>Creates a translation RigidMatrix.
    /// The resulting RigidMatrix will be:
    /// <code>
    /// 1  0  0  x
    /// 0  1  0  0
    /// 0  0  1  0
    /// </code></summary>
    /// <param name="x">The amount by which to translate in X-axis.</param>
    static member createTranslationX(x) =
        RigidMatrix(
            1, 0, 0, x,
            0, 1, 0, 0,
            0, 0, 1, 0)

    /// <summary>Creates a translation RigidMatrix.
    /// The resulting RigidMatrix will be:
    /// <code>
    /// 1  0  0  0
    /// 0  1  0  y
    /// 0  0  1  0
    /// </code></summary>
    /// <param name="y">The amount by which to translate in Y-axis.</param>
    static member createTranslationY(y) =
        RigidMatrix(
            1, 0, 0, 0,
            0, 1, 0, y,
            0, 0, 1, 0)


    /// <summary>Creates a translation RigidMatrix.
    /// The resulting RigidMatrix will be:
    /// <code>
    /// 1  0  0  0
    /// 0  1  0  0
    /// 0  0  1  z
    /// </code></summary>
    /// <param name="z">The amount by which to translate in Z-axis.</param>
    static member createTranslationZ(z) =
        RigidMatrix(
            1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, z)


    /// <summary>Creates a rotation around the X-axis RigidMatrix
    /// by angle in Degrees (not Radians).
    /// A positive rotation will be from Y towards Z-axis,
    /// so counter-clockwise when the X-axis vector is pointing towards the observer. (right-hand rule)
    /// The resulting RigidMatrix will be:
    /// <code>
    /// 1 0      0        0
    /// 0 cos(θ) -sin(θ)  0
    /// 0 sin(θ) cos(θ)   0
    /// </code></summary>
    /// <param name="angleDegrees">Rotation angle in Degrees.</param>
    static member createRotationX(angleDegrees) =
        let angle = UtilEuclid.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        RigidMatrix(
            1, 0,  0, 0,
            0, c, -s, 0,
            0, s,  c, 0)

    /// <summary>Creates a rotation around the Y-axis RigidMatrix
    /// by angle in Degrees (not Radians).
    /// A positive rotation will be from Z towards X-axis,
    /// so counter-clockwise when the Y-axis vector is pointing towards the observer.( right-hand rule)
    /// The resulting RigidMatrix will be:
    /// <code>
    /// cos(θ)  0 sin(θ) 0
    /// 0       1 0      0
    /// -sin(θ) 0 cos(θ) 0
    /// </code></summary>
    /// <param name="angleDegrees">Rotation angle in Degrees.</param>
    static member createRotationY(angleDegrees) =
        let angle = UtilEuclid.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        RigidMatrix(
            c  ,  0,  s,  0,
            0  ,  1,  0,  0,
            -s ,  0,  c,  0)

    /// <summary>Creates a rotation around the Z-axis RigidMatrix
    /// by angle in Degrees (not Radians).
    /// A positive rotation will be from X toward Y-axis,
    /// so counter-clockwise when the Z-axis vector is pointing towards the observer. (right-hand rule)
    /// The resulting RigidMatrix will be:
    /// <code>
    /// cos(θ) -sin(θ) 0 0
    /// sin(θ) cos(θ)  0 0
    /// 0      0       1 0
    /// </code></summary>
    /// <param name="angleDegrees">Rotation angle in Degrees.</param>
    static member createRotationZ(angleDegrees) =
        let angle = UtilEuclid.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        RigidMatrix(
            c, -s, 0, 0,
            s,  c, 0, 0,
            0,  0, 1, 0)

    /// <summary>Creates a rotation around an Axis RigidMatrix.
    /// A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).</summary>
    /// <param name="axis">Rotation axis, as unit-vector.</param>
    /// <param name="angleDegrees">Rotation angle in Degrees.</param>
    static member createRotationAxis(axis:UnitVec, angleDegrees:float) =
        // Based on http://www.gamedev.net/reference/articles/article1199.asp
        let angle = UtilEuclid.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        let t = 1.0 - c
        let x = axis.X
        let y = axis.Y
        let z = axis.Z
        let tx = t * x
        let ty = t * y
        RigidMatrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0)

    /// <summary>Creates a rotation around an Axis RigidMatrix.
    /// A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).</summary>
    /// <param name="axis">Rotation axis, a vector of any length but 0.0.</param>
    /// <param name="angleDegrees">Rotation angle in Degrees.</param>
    static member createRotationAxis(axis:Vec, angleDegrees:float) =
        // first unitize
        let len = sqrt (axis.X*axis.X + axis.Y*axis.Y + axis.Z*axis.Z)
        if isTooTiny(len) then
            fail $"RigidMatrix.createRotationAxis failed on too short axis: {axis} and rotation: {angleDegrees}° Degrees."
        let sc = 1. / len
        let x = axis.X * sc
        let y = axis.Y * sc
        let z = axis.Z * sc
        // Based on http://www.gamedev.net/reference/articles/article1199.asp
        let angle = UtilEuclid.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        let t = 1.0 - c
        let tx = t * x
        let ty = t * y
        RigidMatrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0)


    /// <summary>Creates a rotation matrix around an Axis at a given center point.
    /// A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).</summary>
    /// <param name="axis">Rotation axis, a vector of any length but 0.0.</param>
    /// <param name="cen">The center point for the rotation.</param>
    /// <param name="angleDegrees">Rotation angle in Degrees.</param>
    static member createRotationAxisCenter(axis:Vec, cen:Pnt, angleDegrees:float) =
        RigidMatrix.createTranslation(-cen.X, -cen.Y, -cen.Z)
        *** RigidMatrix.createRotationAxis(axis, angleDegrees)
        *** RigidMatrix.createTranslation(cen.X, cen.Y, cen.Z)

    /// <summary>Creates a rotation matrix around an Axis at a given center point.
    /// A positive angle rotates counter-clockwise when the axis vector is pointing towards the observer (right-hand rule).</summary>
    /// <param name="axis">Rotation axis, a unit-vector.</param>
    /// <param name="cen">The center point for the rotation.</param>
    /// <param name="angleDegrees">Rotation angle in Degrees.</param>
    static member createRotationAxisCenter(axis:UnitVec, cen:Pnt, angleDegrees:float) =
        RigidMatrix.createTranslation(-cen.X, -cen.Y, -cen.Z)
        *** RigidMatrix.createRotationAxis(axis, angleDegrees)
        *** RigidMatrix.createTranslation(cen.X, cen.Y, cen.Z)


    /// <summary>Creates a rotation from one unit-vectors direction to another unit-vectors direction.
    /// If the tips of the two unitized vectors have a distance less than 1e-12 the identity matrix is returned.
    /// If the tips of the two vectors are almost exactly opposite,
    /// that is if tips of the two vectors are less than 1e-12 apart when summed,
    /// there is no valid unique 180 degree rotation that can be found, so an exception is raised.</summary>
    /// <param name="vecFrom">The source unit-vector direction.</param>
    /// <param name="vecTo">The target unit-vector direction.</param>
    static member createVecToVec(vecFrom:UnitVec, vecTo:UnitVec) =
        let vt = vecFrom - vecTo
        if isTooTinySq vt.LengthSq then // the vectors are almost the same
            RigidMatrix.identity
        else
            let v = vecFrom + vecTo
            if isTooTinySq v.LengthSq then // the vectors are almost exactly opposite
                fail $"RigidMatrix.createVecToVec failed to find a rotation axis for (almost) colinear unit-vectors in opposite directions: {vecFrom} and {vecTo}"
            let axis0 = UnitVec.cross(vecFrom, vecTo)
            let len = axis0.Length
            let axis = axis0 / len
            let x = axis.X
            let y = axis.Y
            let z = axis.Z
            let c = vecFrom *** vecTo  // dot to find cos
            let s = len // cross product magnitude is the sine (for unit vectors)
            let t = 1.0 - c
            let tx = t * x
            let ty = t * y
            RigidMatrix(
                tx * x + c    , tx * y - s * z , tx * z + s * y , 0 ,
                tx * y + s * z, ty * y + c     , ty * z - s * x , 0 ,
                tx * z - s * y, ty * z + s * x , t  * z * z + c , 0 )

    /// <summary>Creates a rotation from one vectors direction to another vectors direction.
    /// If the tips of the two unitized vectors have a distance less than 1e-12 the identity matrix is returned.
    /// If the tips of the two vectors are almost exactly opposite,
    /// that is if tips of the two vectors are less than 1e-12 apart when summed,
    /// there is no valid unique 180 degree rotation that can be found, so an exception is raised.
    /// Fails if either vector is too short (length less than 1e-6).</summary>
    /// <param name="vecFrom">The source vector direction.</param>
    /// <param name="vecTo">The target vector direction.</param>
    static member createVecToVec(vecFrom:Vec, vecTo:Vec) =
        let fu =
            let x = vecFrom.X
            let y = vecFrom.Y
            let z = vecFrom.Z
            let length = sqrt(x*x + y*y + z*z)
            if isTooSmall length then
                fail $"RigidMatrix.createVecToVec failed. The vector is too short: vecFrom: {vecFrom}"
            let sc = 1.0 / length // inverse for unitizing vector:
            UnitVec.createUnchecked(x*sc, y*sc, z*sc)
        let tu =
            let x = vecTo.X
            let y = vecTo.Y
            let z = vecTo.Z
            let length = sqrt(x*x + y*y + z*z)
            if isTooSmall length then
                fail $"RigidMatrix.createVecToVec failed. The vector is too short: vecTo: {vecTo}"
            let sc = 1.0 / length // inverse for unitizing vector:
            UnitVec.createUnchecked(x*sc, y*sc, z*sc)

        let vt = fu - tu
        if isTooTinySq vt.LengthSq  then // the vectors are almost the same
            RigidMatrix.identity
        else
            let v = fu + tu
            if isTooTinySq v.LengthSq then // the vectors are almost exactly opposite
                fail $"RigidMatrix.createVecToVec failed to find a rotation axis for (almost) colinear vectors in opposite directions: {vecFrom} and {vecTo}"
            let axis0 = UnitVec.cross(fu, tu)
            let len = axis0.Length
            let axis = axis0 / len
            let x = axis.X
            let y = axis.Y
            let z = axis.Z
            let c = fu *** tu  // dot to find cos
            let s = len // cross product magnitude is the sine (for unit vectors)
            let t = 1.0 - c
            let tx = t * x
            let ty = t * y
            RigidMatrix(
                tx * x + c    , tx * y - s * z , tx * z + s * y , 0 ,
                tx * y + s * z, ty * y + c     , ty * z - s * x , 0 ,
                tx * z - s * y, ty * z + s * x , t  * z * z + c , 0)



    /// <summary>Creates a RigidMatrix to transform from World plane or Coordinate System to given Plane.
    /// Also called Change of Basis.</summary>
    /// <param name="p">The target plane.</param>
    static member createToPlane(p:PPlane) =
        RigidMatrix(
            p.Xaxis.X , p.Yaxis.X , p.Zaxis.X ,  p.Origin.X ,
            p.Xaxis.Y , p.Yaxis.Y , p.Zaxis.Y ,  p.Origin.Y ,
            p.Xaxis.Z , p.Yaxis.Z , p.Zaxis.Z ,  p.Origin.Z)


    /// <summary>Creates a RigidMatrix to transform from one Plane or Coordinate System to another Plane.</summary>
    /// <param name="fromPlane">The source plane.</param>
    /// <param name="toPlane">The target plane.</param>
    static member createPlaneToPlane(fromPlane:PPlane, toPlane:PPlane) =
        let f = fromPlane |> RigidMatrix.createToPlane |> RigidMatrix.inverse
        let t = toPlane   |> RigidMatrix.createToPlane
        f *** t

    /// <summary>Tries to create a 3x4 RigidMatrix from a general 4x4 matrix.
    /// Returns None if the input matrix does scale, shear, flip, mirror, reflect or project.
    /// However, translation is allowed.</summary>
    /// <param name="m">The general 4x4 matrix to convert.</param>
    static member tryCreateFromMatrix (m:Matrix) =
        if m.IsProjecting then // checks the last row is 0,0,0,1
            None
        else
            let x = Vec(m.M11, m.M12, m.M13)
            let y = Vec(m.M21, m.M22, m.M23)
            let z = Vec(m.M31, m.M32, m.M33)
            let inline sqLen (v:Vec) = v.X*v.X + v.Y*v.Y + v.Z*v.Z // defined here again, because Vec extension members are not in scope here
            if
                UtilEuclid.isOne (sqLen x) && // exclude scaling
                UtilEuclid.isOne (sqLen y) &&
                UtilEuclid.isOne (sqLen z) &&
                UtilEuclid.isZero (x *** y) && // orthogonal if dot product of row or column vectors is zero
                UtilEuclid.isZero (x *** z) &&
                UtilEuclid.isZero (y *** z) &&
                UtilEuclid.isOne  (Vec.cross (x, y) *** z) then // check it's not reflecting (would be -1)
                    Some (RigidMatrix(  m.M11, m.M21, m.M31, m.X41,
                                        m.M12, m.M22, m.M32, m.Y42,
                                        m.M13, m.M23, m.M33, m.Z43)
                        )
            else
                None

    /// <summary>Create immutable a 4x3 Transformation Matrix.
    /// Checks the input values to ensure they form a valid RigidMatrix.
    /// This Constructor takes arguments in row-major order:
    /// <code>M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43</code>
    /// Where X41, Y42 and Z43 refer to the translation part of the RigidMatrix.</summary>
    static member create( m11, m21, m31, x41,
                          m12, m22, m32, y42,
                          m13, m23, m33, z43) =
        let z = Vec(m31, m32, m33)
        let y = Vec(m21, m22, m23)
        let x = Vec(m11, m12, m13)
        let inline sqLen (v:Vec) = v.X*v.X + v.Y*v.Y + v.Z*v.Z  // defined here again, because Vec extension members are not in scope here
        if
            UtilEuclid.isOne  (sqLen x) && // exclude scaling
            UtilEuclid.isOne  (sqLen y) &&
            UtilEuclid.isOne  (sqLen z) &&
            UtilEuclid.isZero (x *** y) && // orthogonal if dot product of row or column vectors is zero
            UtilEuclid.isZero (x *** z) &&
            UtilEuclid.isZero (y *** z) &&
            UtilEuclid.isOne  (Vec.cross (x, y) *** z) then // check it's not reflecting (would be -1)
                RigidMatrix (   m11, m21, m31, x41,
                                m12, m22, m32, y42,
                                m13, m23, m33, z43)
        else
            fail $"RigidMatrix.create failed. The input values do scale, shear, flip, mirror, reflect or project: {m11}, {m21}, {m31}, {x41}, {m12}, {m22}, {m32}, {y42}, {m13}, {m23}, {m33}, {z43}"
            |> unbox // to make the type checker happy


    /// <summary>Tries to create a 3x4 RigidMatrix from a general 4x4 matrix.
    /// Fails if the input matrix does scale, shear, flip, mirror, reflect or project.
    /// However, translation is allowed.</summary>
    /// <param name="m">The general 4x4 matrix to convert.</param>
    static member createFromMatrix (m:Matrix) =
        match RigidMatrix.tryCreateFromMatrix m with
        | Some m ->
            m
        | None ->
            fail $"RigidMatrix.createFromMatrix failed. The input matrix does scale, shear, flip, mirror, reflect or project: {m}"
            |> unbox // to make the type checker happy


    /// <summary>Converts the 3x4 RigidMatrix to a general 4x4 Matrix.</summary>
    /// <param name="m">The RigidMatrix to convert.</param>
    static member toMatrix (m:RigidMatrix) =
        Matrix(
            m.M11, m.M12, m.M13, 0.0,
            m.M21, m.M22, m.M23, 0.0,
            m.M31, m.M32, m.M33, 0.0,
            m.X41, m.Y42, m.Z43, 1.0)

    /// <summary>Create a RigidMatrix from a Quaternion.</summary>
    /// <param name="quaternion">The quaternion representing the rotation.</param>
    static member createFromQuaternion(quaternion:Quaternion) =
        let x = quaternion.X
        let y = quaternion.Y
        let z = quaternion.Z
        let w = quaternion.W
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
        // Create a 4x4 Transformation Matrix.
        // This Constructor takes arguments in row-major order,
        RigidMatrix ( 1. - (yy + zz)
                    , xy - wz
                    , xz + wy
                    , 0
                    , xy + wz
                    , 1. - (xx + zz)
                    , yz - wx
                    , 0
                    , xz - wy
                    , yz + wx
                    , 1. - (xx + yy)
                    , 0 )


    /// <summary>Removes the translation part by setting X41, Y42 and Z43 to 0.0.</summary>
    /// <param name="m">The matrix to modify.</param>
    static member removeTranslation (m:RigidMatrix) =
        RigidMatrix(m.M11, m.M21, m.M31, 0.0,
                    m.M12, m.M22, m.M32, 0.0,
                    m.M13, m.M23, m.M33, 0.0)

    /// <summary>Add a vector translation to an existing RigidMatrix.</summary>
    /// <param name="v">The translation vector to add.</param>
    /// <param name="m">The matrix to modify.</param>
    static member addTranslation (v:Vec) (m:RigidMatrix) =
        RigidMatrix(m.M11, m.M21, m.M31, m.X41 + v.X,
                    m.M12, m.M22, m.M32, m.Y42 + v.Y,
                    m.M13, m.M23, m.M33, m.Z43 + v.Z)



    /// <summary>Add a X, Y and Z translation to an existing RigidMatrix.</summary>
    /// <param name="x">The X translation to add.</param>
    /// <param name="y">The Y translation to add.</param>
    /// <param name="z">The Z translation to add.</param>
    /// <param name="m">The matrix to modify.</param>
    static member addTranslationXYZ x y z  (m:RigidMatrix) =
        RigidMatrix(m.M11, m.M21, m.M31, m.X41 + x,
                    m.M12, m.M22, m.M32, m.Y42 + y,
                    m.M13, m.M23, m.M33, m.Z43 + z)


    // ----------------------------------------------
    // operators for matrix multiplication:
    // ----------------------------------------------

    /// <summary>Multiplies (or applies) a RigidMatrix to a 3D vector.
    /// Since a 3D vector represents a direction or translation in space, but not a location,
    /// all translations are ignored. (Homogeneous Vector)</summary>
    /// <param name="v">The 3D vector to transform.</param>
    /// <param name="m">The transformation matrix.</param>
    static member inline ( *** ) (v:Vec, m:RigidMatrix) =
        let x = v.X
        let y = v.Y
        let z = v.Z
        Vec(  m.M11*x + m.M21*y + m.M31*z // + m.X41
            , m.M12*x + m.M22*y + m.M32*z // + m.Y42
            , m.M13*x + m.M23*y + m.M33*z // + m.Z43
            )

    /// <summary>Multiplies (or applies) a RigidMatrix to a 3D unit-vector.
    /// Since a 3D vector represents a direction or translation in space, but not a location,
    /// all translations are ignored. (Homogeneous Vector)</summary>
    /// <param name="v">The 3D unit-vector to transform.</param>
    /// <param name="m">The transformation matrix.</param>
    static member inline ( *** ) (v:UnitVec, m:RigidMatrix) =
        let x = v.X
        let y = v.Y
        let z = v.Z
        UnitVec.createUnchecked(
              m.M11*x + m.M21*y + m.M31*z // + m.X41
            , m.M12*x + m.M22*y + m.M32*z // + m.Y42
            , m.M13*x + m.M23*y + m.M33*z // + m.Z43
            )

    /// <summary>Multiplies (or applies) a RigidMatrix to a 3D point.</summary>
    /// <param name="v">The 3D point to transform.</param>
    /// <param name="m">The transformation matrix.</param>
    static member inline ( *** ) (v:Pnt, m:RigidMatrix) =
        let x = v.X
        let y = v.Y
        let z = v.Z
        Pnt(  m.M11*x + m.M21*y + m.M31*z + m.X41
            , m.M12*x + m.M22*y + m.M32*z + m.Y42
            , m.M13*x + m.M23*y + m.M33*z + m.Z43
            )

