namespace Euclid

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Euclid.Util
open System.Runtime.Serialization // for serialization of struct fields only but not properties via  [<DataMember>] attribute. with Newtonsoft.Json or similar


/// An immutable 4x4 Transformation Matrix.
/// The matrix is represented in the following column-vector syntax form:
/// M11 M21 M31 X41
/// M12 M22 M32 Y42
/// M13 M23 M33 Z43
/// M14 M24 M34 M44
/// Where X41, Y42 and Z43 refer to the translation part of the matrix.
/// Note: Never use the struct default constructor Matrix() as it will create an invalid zero Matrix.
/// Use Matrix.create or Matrix.createUnchecked instead.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
[<DataContract>] // for using DataMember on fields
type Matrix =
    //[<DataMember>] //to serialize this struct field (but not properties) with Newtonsoft.Json and similar
    [<DataMember>] val M11 :float ; [<DataMember>] val M21 :float ; [<DataMember>] val M31 :float; [<DataMember>] val X41:float
    [<DataMember>] val M12 :float ; [<DataMember>] val M22 :float ; [<DataMember>] val M32 :float; [<DataMember>] val Y42:float
    [<DataMember>] val M13 :float ; [<DataMember>] val M23 :float ; [<DataMember>] val M33 :float; [<DataMember>] val Z43:float
    [<DataMember>] val M14 :float ; [<DataMember>] val M24 :float ; [<DataMember>] val M34 :float; [<DataMember>] val M44:float

    // this implementation is based on https://github.com/mrdoob/three.js/blob/dev/src/math/Matrix4.js

    /// Create a 4x4 Transformation Matrix.
    /// This Constructor takes arguments in row-major order,
    /// The matrix is represented in the following column-vector syntax form:
    /// M11 M21 M31 X41
    /// M12 M22 M32 Y42
    /// M13 M23 M33 Z43
    /// M14 M24 M34 M44
    /// Where X41, Y42 and Z43 refer to the translation part of the matrix.
    new (   m11,  m21,  m31,  x41,
            m12,  m22,  m32,  y42,
            m13,  m23,  m33,  z43,
            m14,  m24,  m34,  m44) = {
                M11=m11 ; M21=m21 ; M31=m31 ; X41=x41 ;
                M12=m12 ; M22=m22 ; M32=m32 ; Y42=y42 ;
                M13=m13 ; M23=m23 ; M33=m33 ; Z43=z43 ;
                M14=m14 ; M24=m24 ; M34=m34 ; M44=m44 }

    /// Returns the 16 elements column-major order:
    /// [| M11 M12 M13 M14 M21 M22 M23 M24 M31 M32 M33 M34 X41 Y42 Z43 M44 |]
    /// Where X41, Y42 and Z43 refer to the translation part of the matrix.
    member m.ToArrayByColumns =
        [| m.M11; m.M12; m.M13; m.M14; m.M21; m.M22; m.M23; m.M24; m.M31; m.M32; m.M33; m.M34; m.X41; m.Y42; m.Z43; m.M44 |]

    /// Returns the 16 elements in row-major order:
    /// [| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 M14 M24 M34 M44 |]
    /// Where X41, Y42 and Z43 refer to the translation part of the matrix.
    member m.ToArrayByRows =
        [| m.M11; m.M21; m.M31; m.X41; m.M12; m.M22; m.M32; m.Y42; m.M13; m.M23; m.M33; m.Z43; m.M14; m.M24; m.M34; m.M44 |]

    /// Nicely formats the Matrix to a Grid of 4x4.
    override m.ToString()=
        let ts   = m.ToArrayByRows |> Array.map ( fun x -> x.ToString("0.###"))
        let most = ts |> Array.maxBy ( fun s -> s.Length)
        "4x4 Column-Vector Transformation Matrix:\r\n" + (
        ts
        |> Array.map ( fun x -> String(' ', most.Length-x.Length) + x )
        |> Array.chunkBySize 4
        |> Array.map (fun es -> " " + String.concat " | " es)
        |> String.concat Environment.NewLine
        )

    //Nicely formats the Matrix to a Grid of 4x4 including field names.
    //override m.ToString()=
    //    let names =[| "M11"; "M21"; "M31"; "X41"; "M12"; "M22"; "M32"; "Y42"; "M13"; "M23"; "M33"; "Z43"; "M14"; "M24"; "M34"; "M44"|]
    //    let ts   = (names, m.ByRows)  ||> Array.map2 ( fun n v -> v.ToString("0.###"))
    //    let most = ts |> Array.maxBy ( fun s -> s.Length)
    //    "Colum-Vector Transformation Matrix:\r\n" + (
    //    (names, ts)
    //    ||> Array.map2 ( fun n v ->n + ": " + String(' ', most.Length-v.Length) + v )
    //    |> Array.chunkBySize 4
    //    |> Array.map (fun es -> " " + String.concat " | " es)
    //    |> String.concat Environment.NewLine
    //    )


    /// Returns the first column vector. M11, M12 and M13
    member m.ColumnVector1 = Vec(m.M11, m.M12, m.M13)

    /// Returns the second column vector. M21, M22 and M23
    member m.ColumnVector2 = Vec(m.M21, m.M22, m.M23)

    /// Returns the third column vector. M31, M32 and M33
    member m.ColumnVector3 = Vec(m.M31, m.M32, m.M33)

    /// Returns the translation or forth column vector. X41, Y42 and Z43
    member m.Translation = Vec(m.X41, m.Y42, m.Z43)


    /// The determinant of the Matrix.
    /// The Determinant describes the signed volume that a unit cube will have after the matrix was applied.
    member m.Determinant =
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
        let n14 = m.M14
        let n24 = m.M24
        let n34 = m.M34
        let n44 = m.M44

        let t11 = n23 * n34 * y42 - n24 * n33 * y42 + n24 * n32 * z43 - n22 * n34 * z43 - n23 * n32 * n44 + n22 * n33 * n44
        let t12 = n14 * n33 * y42 - n13 * n34 * y42 - n14 * n32 * z43 + n12 * n34 * z43 + n13 * n32 * n44 - n12 * n33 * n44
        let t13 = n13 * n24 * y42 - n14 * n23 * y42 + n14 * n22 * z43 - n12 * n24 * z43 - n13 * n22 * n44 + n12 * n23 * n44
        let t14 = n14 * n23 * n32 - n13 * n24 * n32 - n14 * n22 * n33 + n12 * n24 * n33 + n13 * n22 * n34 - n12 * n23 * n34
        n11 * t11 + n21 * t12 + n31 * t13 + x41 * t14



    /// Inverts the matrix.
    /// If the determinant is zero the Matrix cannot be inverted.
    /// An Exception is raised.
    member m.Inverse =
        // based on http://www.euclideanspace.com/maths/algebra/matrix/functions/inverse/fourD/index.htm

        //Speed Optimization TODO
        //The below  program is valid for a general 4×4 matrix which will work in all circumstances but when the matrix is being used to
        //represent a combined rotation and translation (as described on this page) then the matrix carries a lot of redundant information.
        //So if we want to speed up the code on this page then, for this case only, we can take advantage of this redundant information.
        //to invert a pure rotation then we just take the transpose of the 3x3 part of the matrix.
        //to invert a pure translation the we just negate the translation

        // this order would actually need to be transposed when comparing to three.js, but its transposed in the output too, so its correct.
        // The internal array in three.js is column major.
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
        let n14 = m.M14 // x translation in three.js
        let n24 = m.M24 // y translation in three.js
        let n34 = m.M34 // z translation in three.js
        let n44 = m.M44

        let t11 = n23 * n34 * y42 - n24 * n33 * y42 + n24 * n32 * z43 - n22 * n34 * z43 - n23 * n32 * n44 + n22 * n33 * n44
        let t12 = n14 * n33 * y42 - n13 * n34 * y42 - n14 * n32 * z43 + n12 * n34 * z43 + n13 * n32 * n44 - n12 * n33 * n44
        let t13 = n13 * n24 * y42 - n14 * n23 * y42 + n14 * n22 * z43 - n12 * n24 * z43 - n13 * n22 * n44 + n12 * n23 * n44
        let t14 = n14 * n23 * n32 - n13 * n24 * n32 - n14 * n22 * n33 + n12 * n24 * n33 + n13 * n22 * n34 - n12 * n23 * n34
        let det = n11 * t11 + n21 * t12 + n31 * t13 + x41 * t14

        if abs det < 1e-24 then EuclidException.Raise "Euclid.Matrix has a zero or almost zero Determinant. It is smaller than 1e-24. It cannot be inverted:\r\n%O" m // TODO or return all zero matrix like threeJS ?

        let detInv = 1. / det

        // this order would actually need to be transposed when comparing to three.js, but since input is transposed it is correct.
        // TODO optimize: common sub expressions could be precalculated
        Matrix  (   t11 * detInv                                                                                                         // M11
                , ( n24 * n33 * x41 - n23 * n34 * x41 - n24 * n31 * z43 + n21 * n34 * z43 + n23 * n31 * n44 - n21 * n33 * n44 ) * detInv // M21
                , ( n22 * n34 * x41 - n24 * n32 * x41 + n24 * n31 * y42 - n21 * n34 * y42 - n22 * n31 * n44 + n21 * n32 * n44 ) * detInv // M31
                , ( n23 * n32 * x41 - n22 * n33 * x41 - n23 * n31 * y42 + n21 * n33 * y42 + n22 * n31 * z43 - n21 * n32 * z43 ) * detInv // X41
                ,   t12 * detInv                                                                                                         // M12
                , ( n13 * n34 * x41 - n14 * n33 * x41 + n14 * n31 * z43 - n11 * n34 * z43 - n13 * n31 * n44 + n11 * n33 * n44 ) * detInv // M22
                , ( n14 * n32 * x41 - n12 * n34 * x41 - n14 * n31 * y42 + n11 * n34 * y42 + n12 * n31 * n44 - n11 * n32 * n44 ) * detInv // M32
                , ( n12 * n33 * x41 - n13 * n32 * x41 + n13 * n31 * y42 - n11 * n33 * y42 - n12 * n31 * z43 + n11 * n32 * z43 ) * detInv // Y42
                ,   t13 * detInv                                                                                                         // M13
                , ( n14 * n23 * x41 - n13 * n24 * x41 - n14 * n21 * z43 + n11 * n24 * z43 + n13 * n21 * n44 - n11 * n23 * n44 ) * detInv // M23
                , ( n12 * n24 * x41 - n14 * n22 * x41 + n14 * n21 * y42 - n11 * n24 * y42 - n12 * n21 * n44 + n11 * n22 * n44 ) * detInv // M33
                , ( n13 * n22 * x41 - n12 * n23 * x41 - n13 * n21 * y42 + n11 * n23 * y42 + n12 * n21 * z43 - n11 * n22 * z43 ) * detInv // Z43
                ,   t14 * detInv                                                                                                         // M14
                , ( n13 * n24 * n31 - n14 * n23 * n31 + n14 * n21 * n33 - n11 * n24 * n33 - n13 * n21 * n34 + n11 * n23 * n34 ) * detInv // M24
                , ( n14 * n22 * n31 - n12 * n24 * n31 - n14 * n21 * n32 + n11 * n24 * n32 + n12 * n21 * n34 - n11 * n22 * n34 ) * detInv // M34
                , ( n12 * n23 * n31 - n13 * n22 * n31 + n13 * n21 * n32 - n11 * n23 * n32 - n12 * n21 * n33 + n11 * n22 * n33 ) * detInv // M44
                )

    /// Checks if the Matrix is an Identity Matrix in the form of:
    /// 1  0  0  0
    /// 0  1  0  0
    /// 0  0  1  0
    /// 0  0  0  1
    /// Using an approximate tolerance of 1e-6.
    member m.IsIdentity =
        isOne  m.M11 && isZero m.M21 && isZero m.M31 && isZero m.X41 &&
        isZero m.M12 && isOne  m.M22 && isZero m.M32 && isZero m.Y42 &&
        isZero m.M13 && isZero m.M23 && isOne  m.M33 && isZero m.Z43 &&
        isZero m.M14 && isZero m.M24 && isZero m.M34 && isOne  m.M44

    /// Checks if the Matrix is an affine transformation.
    /// That means it does not do any projection.
    /// The fields m.M14, m.M24 and m.M34 must be 0.0 and m.M44 must be 1.0 or very close to it.
    /// Using an approximate tolerance of 1e-6.
    member m.IsAffine  =
        isZero m.M14 && isZero m.M24 && isZero m.M34 && isOne m.M44

    /// Checks if the Matrix is an affine transformation.
    /// That means it does not do any projection.
    /// The fields m.M14, m.M24 and m.M34 must be 0.0 and m.M44 must be 1.0 or very close to it.
    /// Using an approximate tolerance of 1e-6.
    member m.IsProjecting  =
        isNotZero m.M14 || isNotZero m.M24 || isNotZero m.M34 || isNotOne m.M44

    /// Returns if matrix is orthogonal.
    /// It might also be mirroring or scaling, but not shearing or projecting.
    /// It must be affine and the dot products of the three column vectors must be zero.
    member m.IsOrthogonal =
        m.IsAffine &&
        (
        let x = Vec(m.M11, m.M12, m.M13)
        let y = Vec(m.M21, m.M22, m.M23)
        let z = Vec(m.M31, m.M32, m.M33)
        Util.isZero (x * y) &&
        Util.isZero (x * z) &&
        Util.isZero (y * z)
        )

    /// Returns if matrix is mirroring or reflection.
    /// It might also be rotating, translating or scaling, but not projecting.
    /// It must be affine and the determinate of the 3x3 part must be negative.
    /// Same as m.IsReflecting.
    member m.IsMirroring =
        m.IsAffine &&
        (
        let x = Vec(m.M11, m.M12, m.M13)
        let y = Vec(m.M21, m.M22, m.M23)
        let z = Vec(m.M31, m.M32, m.M33)
         // defined here again, because Vec extension members are not in scope here
        let inline cross (a:Vec) (b:Vec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) // could be inlined here for performance
        ( cross x y) * z  < 0.0
        )

    /// Returns if matrix is mirroring or reflection.
    /// It might also be rotating, translating or scaling, but not projecting.
    /// It must be affine and the determinate of the 3x3 part must be negative.
    /// Same as m.IsMirroring.
    member m.IsReflecting = 
        m.IsMirroring

    /// Returns if matrix is scaling.
    /// It might also be rotating, translating or reflecting, but not projecting.
    /// It must be affine and the 3 column vector must have a length other than one.
    member m.IsScaling =
        m.IsAffine &&
        (
        let inline sqLen    (v:Vec) = v.X*v.X + v.Y*v.Y + v.Z*v.Z // defined here again, because Vec extension members are not in scope here
        Util.isNotOne (sqLen (Vec(m.M11, m.M12, m.M13))) || // exclude scaling
        Util.isNotOne (sqLen (Vec(m.M21, m.M22, m.M23))) ||
        Util.isNotOne (sqLen (Vec(m.M31, m.M32, m.M33)))
        )

    /// Returns true if matrix is translating.
    /// It might also be rotating, scaling and reflecting, but not projecting.
    member m.IsTranslating = m.IsAffine && (isNotZero m.X41 || isNotZero m.Y42|| isNotZero m.Z43 )

    /// Returns true if matrix is only translating.
    /// It might not be rotating, scaling, reflecting, or projecting.
    member m.IsOnlyTranslating =
        isOne  m.M11 && isZero m.M21 && isZero m.M31 &&
        isZero m.M12 && isOne  m.M22 && isZero m.M32 &&
        isZero m.M13 && isZero m.M23 && isOne  m.M33 &&
        isZero m.M14 && isZero m.M24 && isZero m.M34 && isOne  m.M44


    //----------------------------------------------------------------------------------------------
    //--------------------------  Static Members  --------------------------------------------------
    //----------------------------------------------------------------------------------------------
    
        /// Returns the 16 elements column-major order:
    /// [| M11 M12 M13 M14 M21 M22 M23 M24 M31 M32 M33 M34 X41 Y42 Z43 M44 |]
    /// Where X41, Y42 and Z43 refer to the translation part of the matrix.
    static member inline toArrayByColumns (m:Matrix) = 
        m.ToArrayByColumns
    
    /// Returns the 16 elements in row-major order:
    /// [| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 M14 M24 M34 M44 |]
    /// Where X41, Y42 and Z43 refer to the translation part of the matrix.
    static member inline toArrayByRows (m:Matrix) = 
        m.ToArrayByRows


    /// Checks if two Matrices are equal within tolerance.
    /// By comparing the fields M11 to M44 each with the given tolerance.
    static member equals tol (a:Matrix) (b:Matrix) =
        abs(a.M11-b.M11) < tol &&
        abs(a.M12-b.M12) < tol &&
        abs(a.M13-b.M13) < tol &&
        abs(a.M14-b.M14) < tol &&
        abs(a.M21-b.M21) < tol &&
        abs(a.M22-b.M22) < tol &&
        abs(a.M23-b.M23) < tol &&
        abs(a.M24-b.M24) < tol &&
        abs(a.M31-b.M31) < tol &&
        abs(a.M32-b.M32) < tol &&
        abs(a.M33-b.M33) < tol &&
        abs(a.M34-b.M34) < tol &&
        abs(a.X41-b.X41) < tol &&
        abs(a.Y42-b.Y42) < tol &&
        abs(a.Z43-b.Z43) < tol &&
        abs(a.M44-b.M44) < tol



    /// Multiplies matrixA with matrixB.
    /// The resulting transformation will first do matrixA and then matrixB.
    static member multiply (matrixA:Matrix,  matrixB:Matrix) =
        let a11 = matrixA.M11
        let a12 = matrixA.M12
        let a13 = matrixA.M13
        let a14 = matrixA.M14
        let a21 = matrixA.M21
        let a22 = matrixA.M22
        let a23 = matrixA.M23
        let a24 = matrixA.M24
        let a31 = matrixA.M31
        let a32 = matrixA.M32
        let a33 = matrixA.M33
        let a34 = matrixA.M34
        let a41 = matrixA.X41
        let a42 = matrixA.Y42
        let a43 = matrixA.Z43
        let a44 = matrixA.M44

        let b11 = matrixB.M11
        let b12 = matrixB.M12
        let b13 = matrixB.M13
        let b14 = matrixB.M14
        let b21 = matrixB.M21
        let b22 = matrixB.M22
        let b23 = matrixB.M23
        let b24 = matrixB.M24
        let b31 = matrixB.M31
        let b32 = matrixB.M32
        let b33 = matrixB.M33
        let b34 = matrixB.M34
        let b41 = matrixB.X41
        let b42 = matrixB.Y42
        let b43 = matrixB.Z43
        let b44 = matrixB.M44
        Matrix(
             a11 * b11 + a12 * b21 + a13 * b31 + a14 * b41 , // M11
             a21 * b11 + a22 * b21 + a23 * b31 + a24 * b41 , // M21
             a31 * b11 + a32 * b21 + a33 * b31 + a34 * b41 , // M31
             a41 * b11 + a42 * b21 + a43 * b31 + a44 * b41 , // X41
             a11 * b12 + a12 * b22 + a13 * b32 + a14 * b42 , // M12
             a21 * b12 + a22 * b22 + a23 * b32 + a24 * b42 , // M22
             a31 * b12 + a32 * b22 + a33 * b32 + a34 * b42 , // M32
             a41 * b12 + a42 * b22 + a43 * b32 + a44 * b42 , // Y42
             a11 * b13 + a12 * b23 + a13 * b33 + a14 * b43 , // M13
             a21 * b13 + a22 * b23 + a23 * b33 + a24 * b43 , // M23
             a31 * b13 + a32 * b23 + a33 * b33 + a34 * b43 , // M33
             a41 * b13 + a42 * b23 + a43 * b33 + a44 * b43 , // Z43
             a11 * b14 + a12 * b24 + a13 * b34 + a14 * b44 , // M14
             a21 * b14 + a22 * b24 + a23 * b34 + a24 * b44 , // M24
             a31 * b14 + a32 * b24 + a33 * b34 + a34 * b44 , // M34
             a41 * b14 + a42 * b24 + a43 * b34 + a44 * b44 ) // M44


    /// Multiplies matrixA with matrixB.
    /// The resulting transformation will first do matrixA and then matrixB.
    static member inline ( * ) (matrixA:Matrix,  matrixB:Matrix) = 
        Matrix.multiply(matrixA, matrixB)

    /// If the Determinant of the Matrix.
    /// The Determinant describes the volume that a unit cube will have have the matrix was applied.
    static member inline determinant (m:Matrix) = 
        m.Determinant

    /// Inverses the matrix.
    /// If the Determinant is zero the Matrix cannot be inverted.
    /// An Exception is raised.
    static member inline inverse (m:Matrix) = 
        m.Inverse

    /// Transposes this matrix.
    static member transpose(m:Matrix) =
        Matrix  ( m.M11, m.M12, m.M13, m.M14
                , m.M21, m.M22, m.M23, m.M24
                , m.M31, m.M32, m.M33, m.M34
                , m.X41, m.Y42, m.Z43, m.M44 )

    /// Returns the Identity matrix:
    /// 1  0  0  0
    /// 0  1  0  0
    /// 0  0  1  0
    /// 0  0  0  1
    static member identity =
        Matrix(
            1, 0, 0, 0 ,
            0, 1, 0, 0 ,
            0, 0, 1, 0 ,
            0, 0, 0, 1 )

    /// Sets this matrix as a translation transform:
    /// x - the amount to translate in the X-axis.
    /// y - the amount to translate in the Y-axis.
    /// z - the amount to translate in the Z-axis.
    /// The resulting matrix will be:
    /// 1  0  0  x
    /// 0  1  0  y
    /// 0  0  1  z
    /// 0  0  0  1
    static member createTranslation( x, y, z ) =
        Matrix(
            1, 0, 0, x ,
            0, 1, 0, y ,
            0, 0, 1, z ,
            0, 0, 0, 1 )

    /// Sets this matrix as a translation transform:
    /// x - the amount to translate in the X-axis.
    /// y - the amount to translate in the Y-axis.
    /// z - the amount to translate in the Z-axis.
    /// The resulting matrix will be:
    /// 1  0  0  x
    /// 0  1  0  y
    /// 0  0  1  z
    /// 0  0  0  1
    static member createTranslation(v:Vec ) =
        Matrix(
            1, 0, 0, v.X ,
            0, 1, 0, v.Y ,
            0, 0, 1, v.Z ,
            0, 0, 0, 1 )



    /// Creates a rotation transformation matrix around the X-axis
    /// by angle in Degrees (not Radians).
    /// angleDegrees — Rotation angle in Degrees.
    /// A positive rotation will be from Y towards Z-axis,  so counter-clockwise looking onto Y-X Plane.
    /// The resulting matrix will be:
    /// 1 0      0        0
    /// 0 cos(θ) -sin(θ)  0
    /// 0 sin(θ) cos(θ)   0
    /// 0 0      0        1
    static member createRotationX( angleDegrees ) =
        let angle = Util.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        Matrix(
            1, 0,  0, 0,
            0, c, -s, 0,
            0, s,  c, 0,
            0, 0,  0, 1 )

    /// Creates a rotation transformation matrix around the Y-axis
    /// by angle in Degrees (not Radians).
    /// angleDegrees — Rotation angle in Degrees.
    /// A positive rotation will be from Z towards X-axis,  so counter-clockwise looking onto Z-X Plane.
    /// The resulting matrix will be:
    /// cos(θ)  0 sin(θ) 0
    /// 0       1 0      0
    /// -sin(θ) 0 cos(θ) 0
    /// 0       0 0      1
    static member createRotationY( angleDegrees ) =
        let angle = Util.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        Matrix(
            c  ,  0,  s,  0,
            0  ,  1,  0,  0,
            -s ,  0,  c,  0,
            0  ,  0,  0,  1 )

    /// Creates a rotation transformation matrix around the Z-axis
    /// by angle in Degrees (not Radians).
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be from X toward Y-axis,  so counter-clockwise looking onto X-Y plane.
    /// The resulting matrix will be:
    /// cos(θ) -sin(θ) 0 0
    /// sin(θ) cos(θ)  0 0
    /// 0      0       1 0
    /// 0      0       0 1
    static member createRotationZ( angleDegrees ) =
        let angle = Util.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        Matrix(
            c, -s, 0, 0,
            s,  c, 0, 0,
            0,  0, 1, 0,
            0,  0, 0, 1 )


    /// Creates a rotation around an Axis transformation matrix.
    /// axis — Rotation axis, as unit-vector.
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector.
    static member createRotationAxis( axis:UnitVec, angleDegrees:float ) =
        // Based on http://www.gamedev.net/reference/articles/article1199.asp
        let angle = Util.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        let t = 1.0 - c
        let x = axis.X
        let y = axis.Y
        let z = axis.Z
        let tx = t * x
        let ty = t * y
        Matrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0,
            0             , 0              , 0              , 1 )

    /// Creates a rotation around an Axis transformation matrix.
    /// axis — Rotation axis, a vector of any length but 0.0 .
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector.
    static member createRotationAxis( axis:Vec, angleDegrees:float ) =
        // first unitize
        let len = sqrt (axis.X*axis.X + axis.Y*axis.Y + axis.Z*axis.Z)
        if len <  zeroLengthTol then
            EuclidException.Raise "Euclid.Matrix.createRotationAxis failed on too short axis: %O and rotation: %g° Degrees." axis angleDegrees
        let sc = 1. / len
        let x = axis.X * sc
        let y = axis.Y * sc
        let z = axis.Z * sc
        // Based on http://www.gamedev.net/reference/articles/article1199.asp
        let angle = Util.toRadians angleDegrees
        let c = cos angle
        let s = sin angle
        let t = 1.0 - c
        let tx = t * x
        let ty = t * y
        Matrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0,
            0             , 0              , 0              , 1 )


    /// Creates a rotation matrix around an Axis at a given center point.
    /// axis — Rotation axis, a vector of any length but 0.0
    /// cen — The center point for the rotation.
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector.
    static member createRotationAxisCenter( axis:Vec, cen:Pnt, angleDegrees:float ) =
        Matrix.createTranslation(-cen.X, -cen.Y, -cen.Z)
        * Matrix.createRotationAxis(axis, angleDegrees)
        * Matrix.createTranslation(cen.X, cen.Y, cen.Z)

    /// Creates a rotation matrix around an Axis at a given center point.
    /// axis — Rotation axis, a unit-vector.
    /// cen — The center point for the rotation.
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector.
    static member createRotationAxisCenter( axis:UnitVec, cen:Pnt, angleDegrees:float ) =
        Matrix.createTranslation(-cen.X, -cen.Y, -cen.Z)
        * Matrix.createRotationAxis(axis, angleDegrees)
        * Matrix.createTranslation(cen.X, cen.Y, cen.Z)


    /// Creates a scale transformation matrix:
    /// x - the amount to scale in the X-axis.
    /// y - the amount to scale in the Y-axis.
    /// z - the amount to scale in the Z-axis.
    /// The resulting matrix will be:
    /// x, 0, 0, 0,
    /// 0, y, 0, 0,
    /// 0, 0, z, 0,
    /// 0, 0, 0, 1
    static member createScale( x, y, z ) =
        Matrix(
            x, 0, 0, 0,
            0, y, 0, 0,
            0, 0, z, 0,
            0, 0, 0, 1 )

    /// Creates a rotation from one vectors direction to another vectors direction.
    /// Fails on colinear vectors.
    static member createVecToVec( fromVec:UnitVec, toVec:UnitVec ) =
        let c = fromVec*toVec  // dot to find cos
        let s = sqrt(1. - c*c ) // Pythagoras to find sine
        let t = 1.0 - c
        let axis0 = UnitVec.cross(fromVec, toVec)
        let len = axis0.Length
        if len <  zeroLengthTol then
            EuclidException.Raise "Euclid.Matrix.createVecToVec failed to find rotation axis on colinear vectors: %O and %O" fromVec toVec
        let axis = axis0 / len
        let x = axis.X
        let y = axis.Y
        let z = axis.Z
        let tx = t * x
        let ty = t * y
        Matrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0,
            0             , 0              , 0              , 1 )

    /// Creates a rotation from one vectors direction to another vectors direction.
    /// Ignores the vectors length. Fails on colinear vectors.
    static member createVecToVec(vecFrom:Vec, vecTo:Vec ) =
        let fu =
            let x = vecFrom.X
            let y = vecFrom.Y
            let z = vecFrom.Z
            let length = sqrt(x*x + y*y + z*z)
            if length <  zeroLengthTol then
                EuclidException.Raise "Euclid.Matrix.createVecToVec failed. too short vector vecFrom: %O" vecFrom
            let sc =  1. / length // inverse for unitizing vector:
            UnitVec.createUnchecked(x*sc, y*sc, z*sc)
        let tu =
            let x = vecTo.X
            let y = vecTo.Y
            let z = vecTo.Z
            let length = sqrt(x*x + y*y + z*z)
            if length <  zeroLengthTol then
                EuclidException.Raise "Euclid.Matrix.createVecToVec failed. too short vector vecTo: %O" vecTo
            let sc =  1. / length // inverse for unitizing vector:
            UnitVec.createUnchecked(x*sc, y*sc, z*sc)
        let c =  fu * tu  // dot to find cosine
        let s = sqrt(1. - c*c) // Pythagoras to find sine
        let t = 1.0 - c
        let axis = Vec.cross(vecFrom, vecTo)
        let len = axis.Length
        if len <  Util.zeroLengthTol then
            EuclidException.Raise "Euclid.Matrix.createVecToVec failed to find rotation axis on colinear or zero length vectors: %O and %O" vecFrom vecTo
        let sc = 1. / len
        let x = axis.X * sc
        let y = axis.Y * sc
        let z = axis.Z * sc
        let tx = t * x
        let ty = t * y
        Matrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0,
            0             , 0              , 0              , 1 )



    /// Creates a shear transformation matrix:
    /// xy - the amount to shear a X unit-vector a long Y-axis.
    /// xz - the amount to shear a X unit-vector a long Z-axis.
    /// yx - the amount to shear a Y unit-vector a long X-axis.
    /// yz - the amount to shear a Y unit-vector a long Z-axis.
    /// zx - the amount to shear a Z unit-vector a long X-axis.
    /// zy - the amount to shear a Z unit-vector a long Y-axis.
    /// The resulting matrix will be:
    /// 1,   yx,  zx,  0,
    /// xy,   1,  zy,  0,
    /// xz,  yz,   1,  0,
    /// 0,    0,   0,  1
    static member createShear( xy, xz, yx, yz, zx, zy ) =
        Matrix(
            1,  yx, zx, 0 ,
            xy, 1 , zy, 0 ,
            xz, yz,  1, 0 ,
            0 , 0 ,  0, 1 )

    /// Creates a Matrix to transform from World plane or Coordinate System to given Plane.
    /// Also called Change of Basis.
    static member createToPlane(p:PPlane) =
        Matrix(
            p.Xaxis.X , p.Yaxis.X , p.Zaxis.X ,  p.Origin.X ,
            p.Xaxis.Y , p.Yaxis.Y , p.Zaxis.Y ,  p.Origin.Y ,
            p.Xaxis.Z , p.Yaxis.Z , p.Zaxis.Z ,  p.Origin.Z ,
            0       ,       0 ,        0,            1 )


    /// Creates a Matrix to transform from one Plane or Coordinate System to another Plane.
    static member createPlaneToPlane(fromPlane:PPlane,  toPlane:PPlane) =
        let f = fromPlane |> Matrix.createToPlane |> Matrix.inverse
        let t = toPlane   |> Matrix.createToPlane
        f*t

    /// Creates a Matrix to mirror on a Plane.
    static member createMirror (p:PPlane) =
        let toPlane   =  Matrix.createToPlane p
        let fromPlane =  toPlane.Inverse
        let zFlip     =  Matrix.createScale(1,1,-1)
        fromPlane*zFlip*toPlane

    /// Create Matrix from Quaternion.
    static member createFromQuaternion( quaternion:Quaternion) =
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
        // the sequence is reordered here, when compared to Three js
        Matrix  (
                 ( 1. - ( yy + zz ) )
                ,( xy - wz )
                ,( xz + wy )
                ,0
                ,( xy + wz )
                ,( 1. - ( xx + zz ) )
                ,0
                ,( yz - wx )
                ,( xz - wy )
                ,( yz + wx )
                ,( 1. - ( xx + yy ) )
                ,0
                ,0
                ,0
                ,0
                ,1 )



    /// Creates a matrix from array of 16 elements in Column Major order:
    /// [| M11 M12 M13 M14 M21 M22 M23 M24 M31 M32 M33 M34 X41 Y42 Z43 M44 |]
    /// Where X41, Y42 and Z43 refer to the translation part of the matrix.
    static member createFromColumMajorArray (xs:float[]) =
        if xs.Length <> 16 then
            EuclidException.Raise "Euclid.Matrix.createFromColumMajorArray expects an array of 16 items but got %d " xs.Length
        else
            Matrix (
                xs[0],  xs[4],  xs[ 8],  xs[12] ,
                xs[1],  xs[5],  xs[ 9],  xs[13] ,
                xs[2],  xs[6],  xs[10],  xs[14] ,
                xs[3],  xs[7],  xs[11],  xs[15] )


    /// Creates a matrix from array of 16 elements in Row Major order:
    /// [| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 M14 M24 M34 M44 |]
    /// Where X41, Y42 and Z43 refer to the translation part of the matrix.
    static member createFromRowMajorArray (xs:float[]) =
        if xs.Length <> 16 then
            EuclidException.Raise "Euclid.Matrix.createFromRowMajorArray expects an array of 16 items but got %d " xs.Length
        else
            Matrix (
                xs[ 0],  xs[ 1],  xs[ 2],  xs[ 3] ,
                xs[ 4],  xs[ 5],  xs[ 6],  xs[ 7] ,
                xs[ 8],  xs[ 9],  xs[10],  xs[11] ,
                xs[12],  xs[13],  xs[14],  xs[15] )


    /// Add a vector translation to an existing matrix.
    static member addTranslation (v:Vec) (m:Matrix) =
        Matrix( m.M11,  m.M21,  m.M31,  m.X41 + v.X,
                m.M12,  m.M22,  m.M32,  m.Y42 + v.Y,
                m.M13,  m.M23,  m.M33,  m.Z43 + v.Z,
                m.M14,  m.M24,  m.M34,  m.M44)

    /// Add a X, Y and Z translation to an existing matrix.
    static member addTranslationXYZ x y z  (m:Matrix) =
        Matrix( m.M11,  m.M21,  m.M31,  m.X41 + x,
                m.M12,  m.M22,  m.M32,  m.Y42 + y,
                m.M13,  m.M23,  m.M33,  m.Z43 + z,
                m.M14,  m.M24,  m.M34,  m.M44)

    // ----------------------------------------------
    // operators for matrix multiplication:
    // ----------------------------------------------

    /// Multiplies (or applies) a Matrix to a 3D vector (with an implicit 1 in the 4th dimension,
    /// so that it also works correctly for projections.)
    static member inline  ( * ) (v:Vec, m:Matrix) =
        // from applyMatrix4( m ) in  https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
        let x = v.X
        let y = v.Y
        let z = v.Z
        //let w = 1.0
        let x' = m.M11*x + m.M21*y + m.M31*z + m.X41 // * w
        let y' = m.M12*x + m.M22*y + m.M32*z + m.Y42 // * w
        let z' = m.M13*x + m.M23*y + m.M33*z + m.Z43 // * w
        let w' = m.M14*x + m.M24*y + m.M34*z + m.M44 // * w
        let sc = 1.0 / w'
        Vec(x' * sc, y'* sc, z'* sc)

    /// Multiplies a Matrix with a 3D vector (with an implicit 1 in the 4th dimension,
    /// So that it also works correctly for projections.)
    /// The resulting vector is not unitized.
    static member inline  ( * ) (v:UnitVec, m:Matrix) =
        // from applyMatrix4( m ) in  https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
        let x = v.X
        let y = v.Y
        let z = v.Z
        //let w = 1.0
        let x' = m.M11*x + m.M21*y + m.M31*z + m.X41 // * w
        let y' = m.M12*x + m.M22*y + m.M32*z + m.Y42 // * w
        let z' = m.M13*x + m.M23*y + m.M33*z + m.Z43 // * w
        let w' = m.M14*x + m.M24*y + m.M34*z + m.M44 // * w
        let sc = 1.0 / w'
        Vec(x' * sc, y'* sc, z'* sc)

    /// Multiplies (or applies) a Matrix to a 3D point (with an implicit 1 in the 4th dimension,
    /// so that it also works correctly for projections.)
    static member inline  ( * ) (p:Pnt, m:Matrix) =
        // from applyMatrix4( m ) in  https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
        let x = p.X
        let y = p.Y
        let z = p.Z
        //let w = 1.0
        let x' = m.M11*x + m.M21*y + m.M31*z + m.X41 // * w
        let y' = m.M12*x + m.M22*y + m.M32*z + m.Y42 // * w
        let z' = m.M13*x + m.M23*y + m.M33*z + m.Z43 // * w
        let w' = m.M14*x + m.M24*y + m.M34*z + m.M44 // * w
        let sc = 1.0 / w'
        Pnt(x' * sc, y'* sc, z'* sc)
