namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open FsEx.Geo.Util

/// A 4x4 Transformation Matrix.
/// The matrix is represented in the following colum-vector syntax form:
/// M11 M21 M31 X41 
/// M12 M22 M32 Y42 
/// M13 M23 M33 Z43 
/// M14 M24 M34 M44
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type Matrix = 
    val M11 :float ; val M21 :float ; val M31 :float; val X41:float
    val M12 :float ; val M22 :float ; val M32 :float; val Y42:float
    val M13 :float ; val M23 :float ; val M33 :float; val Z43:float
    val M14 :float ; val M24 :float ; val M34 :float; val M44:float
    
    //this iplementation is based on https://github.com/mrdoob/three.js/blob/dev/src/math/Matrix4.js    
    
    /// Create a 4x4 Transformation Matrix.
    /// This Constructor takes arguments in row-major order, 
    /// The matrix is represented in the following colum-vector syntax form: 
    /// M11 M21 M31 X41
    /// M12 M22 M32 Y42
    /// M13 M23 M33 Z43
    /// M14 M24 M34 M44
    new ( m11,  m21,  m31,  x41, 
          m12,  m22,  m32,  y42, 
          m13,  m23,  m33,  z43, 
          m14,  m24,  m34,  m44) = {
                M11=m11 ; M21=m21 ; M31=m31 ; X41=x41 ;
                M12=m12 ; M22=m22 ; M32=m32 ; Y42=y42 ;
                M13=m13 ; M23=m23 ; M33=m33 ; Z43=z43 ;
                M14=m14 ; M24=m24 ; M34=m34 ; M44=m44 }
    
    /// Returns the 16 elements colum-major order:
    /// [| M11 M12 M13 M14 M21 M22 M23 M24 M31 M32 M33 M34 X41 Y42 Z43 M44 |] 
    member m.ByColumns = [|  
        m.M11
        m.M12
        m.M13
        m.M14
        m.M21
        m.M22
        m.M23
        m.M24
        m.M31
        m.M32
        m.M33
        m.M34
        m.X41
        m.Y42
        m.Z43
        m.M44 
        |]
    
    /// Returns the 16 elements in row-major order: 
    /// [| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 M14 M24 M34 M44 |] 
    member m.ByRows = [|  
        m.M11
        m.M21
        m.M31
        m.X41 
        m.M12
        m.M22
        m.M32
        m.Y42 
        m.M13
        m.M23
        m.M33
        m.Z43 
        m.M14
        m.M24
        m.M34
        m.M44 
        |]        

    /// If the determinant of the Matrix. 
    /// The Determinant descirbes the volume that a unit cube will have have the matrix was applied
    member m.Determinant = 
        let n11 = m.M11
        let n21 = m.M21
        let n31 = m.M31
        let n41 = m.X41
        let n12 = m.M12
        let n22 = m.M22
        let n32 = m.M32
        let n42 = m.Y42
        let n13 = m.M13
        let n23 = m.M23
        let n33 = m.M33
        let n43 = m.Z43
        let n14 = m.M14
        let n24 = m.M24
        let n34 = m.M34
        let n44 = m.M44

        let t11 = n23 * n34 * n42 - n24 * n33 * n42 + n24 * n32 * n43 - n22 * n34 * n43 - n23 * n32 * n44 + n22 * n33 * n44
        let t12 = n14 * n33 * n42 - n13 * n34 * n42 - n14 * n32 * n43 + n12 * n34 * n43 + n13 * n32 * n44 - n12 * n33 * n44
        let t13 = n13 * n24 * n42 - n14 * n23 * n42 + n14 * n22 * n43 - n12 * n24 * n43 - n13 * n22 * n44 + n12 * n23 * n44
        let t14 = n14 * n23 * n32 - n13 * n24 * n32 - n14 * n22 * n33 + n12 * n24 * n33 + n13 * n22 * n34 - n12 * n23 * n34
        n11 * t11 + n21 * t12 + n31 * t13 + n41 * t14

    /// Inverts the matrix. 
    /// If the determinant is zero the Matrix cannot be inverted. 
    /// An Exception is raised.
    member m.Inverse = 
        // based on http://www.euclideanspace.com/maths/algebra/matrix/functions/inverse/fourD/index.htm        
        let n11 = m.M11
        let n21 = m.M21
        let n31 = m.M31
        let n41 = m.X41
        let n12 = m.M12
        let n22 = m.M22
        let n32 = m.M32
        let n42 = m.Y42
        let n13 = m.M13
        let n23 = m.M23
        let n33 = m.M33
        let n43 = m.Z43
        let n14 = m.M14
        let n24 = m.M24
        let n34 = m.M34
        let n44 = m.M44

        let t11 = n23 * n34 * n42 - n24 * n33 * n42 + n24 * n32 * n43 - n22 * n34 * n43 - n23 * n32 * n44 + n22 * n33 * n44
        let t12 = n14 * n33 * n42 - n13 * n34 * n42 - n14 * n32 * n43 + n12 * n34 * n43 + n13 * n32 * n44 - n12 * n33 * n44
        let t13 = n13 * n24 * n42 - n14 * n23 * n42 + n14 * n22 * n43 - n12 * n24 * n43 - n13 * n22 * n44 + n12 * n23 * n44
        let t14 = n14 * n23 * n32 - n13 * n24 * n32 - n14 * n22 * n33 + n12 * n24 * n33 + n13 * n22 * n34 - n12 * n23 * n34
        let det = n11 * t11 + n21 * t12 + n31 * t13 + n41 * t14

        if abs det < 1e-12 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Matrix has an almost zero determinant. It is smaller than 1e-12. It cannot be inverted:\r\n%O" m // TODO or return all zero matrix like threeJS ?

        let detInv = 1. / det
        
        Matrix( t11 * detInv                                                                                                          // M11
              ,( n24 * n33 * n41 - n23 * n34 * n41 - n24 * n31 * n43 + n21 * n34 * n43 + n23 * n31 * n44 - n21 * n33 * n44 ) * detInv // M21
              ,( n22 * n34 * n41 - n24 * n32 * n41 + n24 * n31 * n42 - n21 * n34 * n42 - n22 * n31 * n44 + n21 * n32 * n44 ) * detInv // M31
              ,( n23 * n32 * n41 - n22 * n33 * n41 - n23 * n31 * n42 + n21 * n33 * n42 + n22 * n31 * n43 - n21 * n32 * n43 ) * detInv // X41 
              , t12 * detInv                                                                                                          // M12
              ,( n13 * n34 * n41 - n14 * n33 * n41 + n14 * n31 * n43 - n11 * n34 * n43 - n13 * n31 * n44 + n11 * n33 * n44 ) * detInv // M22
              ,( n14 * n32 * n41 - n12 * n34 * n41 - n14 * n31 * n42 + n11 * n34 * n42 + n12 * n31 * n44 - n11 * n32 * n44 ) * detInv // M32
              ,( n12 * n33 * n41 - n13 * n32 * n41 + n13 * n31 * n42 - n11 * n33 * n42 - n12 * n31 * n43 + n11 * n32 * n43 ) * detInv // Y42 
              , t13 * detInv                                                                                                          // M13
              ,( n14 * n23 * n41 - n13 * n24 * n41 - n14 * n21 * n43 + n11 * n24 * n43 + n13 * n21 * n44 - n11 * n23 * n44 ) * detInv // M23
              ,( n12 * n24 * n41 - n14 * n22 * n41 + n14 * n21 * n42 - n11 * n24 * n42 - n12 * n21 * n44 + n11 * n22 * n44 ) * detInv // M33
              ,( n13 * n22 * n41 - n12 * n23 * n41 - n13 * n21 * n42 + n11 * n23 * n42 + n12 * n21 * n43 - n11 * n22 * n43 ) * detInv // Z43 
              , t14 * detInv                                                                                                          // M14
              ,( n13 * n24 * n31 - n14 * n23 * n31 + n14 * n21 * n33 - n11 * n24 * n33 - n13 * n21 * n34 + n11 * n23 * n34 ) * detInv // M24
              ,( n14 * n22 * n31 - n12 * n24 * n31 - n14 * n21 * n32 + n11 * n24 * n32 + n12 * n21 * n34 - n11 * n22 * n34 ) * detInv // M34
              ,( n12 * n23 * n31 - n13 * n22 * n31 + n13 * n21 * n32 - n11 * n23 * n32 - n12 * n21 * n33 + n11 * n22 * n33 ) * detInv // M44
              )

    /// Checks if the Matrix is an Identity Matrix in the form of:
    /// 1  0  0  0
    /// 0  1  0  0
    /// 0  0  1  0
    /// 0  0  0  1
    /// Using an approximate tolerance of aporox 1e-7.
    member m.IsIdentity =               
        isOne  m.M11 && isZero m.M21 && isZero m.M31 && isZero m.X41 &&
        isZero m.M12 && isOne  m.M22 && isZero m.M32 && isZero m.Y42 &&
        isZero m.M13 && isZero m.M23 && isOne  m.M33 && isZero m.Z43 &&
        isZero m.M14 && isZero m.M24 && isZero m.M34 && isOne  m.M44
    
    /// Checks if the Matrix is an affine transformation.
    /// That means it does not do any projection.
    /// The fields m.M14, m.M24 and m.M34 must be 0.0 and m.M44 must be 1.0 or very close to it.
    /// Using an approximate tolerance of 1e-7.
    member m.IsAffine  =        
        isZero m.M14 && isZero m.M24 && isZero m.M34 && isOne m.M44

    /// Nicely formats the Matrix to a Grid of 4x4.
    override m.ToString()= 
        let ts   = m.ByRows |> Array.map ( fun x -> x.ToString("0.###")) 
        let most = ts |> Array.maxBy ( fun s -> s.Length)
        "Colum-Vector Transformation Matrix:\r\n" + (
        ts   
        |> Array.map ( fun x -> String(' ', most.Length-x.Length) + x ) 
        |> Array.chunkBySize 4
        |> Array.map (fun es -> " " + String.concat " | " es) 
        |> String.concat Environment.NewLine
        )  

    //Nicely formats the Matrix to a Grid of 4x4 incuding field names.
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


    /// Multiplies matrixA with matrixB
    /// The resulting transformation will firts do matrixA and then matrixB
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
    
    
    /// Multiplies matrixA with matrixB
    /// The resulting transformation will first do matrixA and then matrixB
    static member inline ( * ) (matrixA:Matrix,  matrixB:Matrix) = Matrix.multiply(matrixA, matrixB)
    
    /// If the determinant of the Matrix. 
    /// The Determinant descirbes the volume that a unit cube will have have the matrix was applied
    static member inline determinant (m:Matrix) = m.Determinant
    
    /// Inverses the matrix. 
    /// If the determinant is zero the Matrix cannot be inverted. 
    /// An Exception is raised.
    static member inline inverse (m:Matrix) = m.Inverse
    
    /// Transposes this matrix.
    static member transpose(m:Matrix) =
        Matrix( m.M11, m.M12, m.M13, m.M14
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
    /// x - the amount to translate in the X axis.
    /// y - the amount to translate in the Y axis.
    /// z - the amount to translate in the Z axis. 
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
    /// x - the amount to translate in the X axis.
    /// y - the amount to translate in the Y axis.
    /// z - the amount to translate in the Z axis. 
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


    
    /// Creates a rotation transformation matrix around the X axis 
    /// by angle in degrees (not Radians).  
    /// angleDegrees — Rotation angle in Degrees.
    /// A positive rotation will be  from Y towards Z axis,  so counter-clockwise looking onto Y-X Plane
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
    
    /// Creates a rotation transformation matrix around the Y axis 
    /// by angle in degrees (not Radians). 
    /// angleDegrees — Rotation angle in Degrees.
    /// A positive  rotation will be  from Z towards X axis,  so counter-clockwise looking onto Z-X Plane
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
    
    /// Creates a rotation transformation matrix around the Z axis 
    /// by angle in degrees (not Radians). 
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive  rotation will be  from X toward Y axis,  so counter-clockwise looking onto X-Y Plane
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
    /// axis — Rotation axis, as unit vector.
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector
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
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector
    static member createRotationAxis( axis:Vec, angleDegrees:float ) =
        // first unitize
        let len = sqrt (axis.X*axis.X + axis.Y*axis.Y + axis.Z*axis.Z) 
        if len <  zeroLengthTol then 
            FsExGeoException.Raise "FsEx.Geo.Matrix.createRotationAxis failed on too short axis: %O and rotation: %g° degrees" axis angleDegrees
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


    /// Creates a rotation matrix around an Axis at a given center Point.
    /// axis — Rotation axis, a vector of any length but 0.0 
    /// cen — The center point for the rotation
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector
    static member createRotationAxisCenter( axis:Vec, cen:Pnt, angleDegrees:float ) =
        Matrix.createTranslation(-cen.X, -cen.Y, -cen.Z)
        * Matrix.createRotationAxis(axis, angleDegrees)
        * Matrix.createTranslation(cen.X, cen.Y, cen.Z)

    /// Creates a rotation matrix around an Axis at a given center Point.
    /// axis — Rotation axis, a Unit vector 
    /// cen — The center point for the rotation
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector
    static member createRotationAxisCenter( axis:UnitVec, cen:Pnt, angleDegrees:float ) =
        Matrix.createTranslation(-cen.X, -cen.Y, -cen.Z)
        * Matrix.createRotationAxis(axis, angleDegrees)
        * Matrix.createTranslation(cen.X, cen.Y, cen.Z)


    /// Creates a scale transformation matrix:
    /// x - the amount to scale in the X axis.
    /// y - the amount to scale in the Y axis.
    /// z - the amount to scale in the Z axis.
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


    /// Creates a shear transformation matrix:
    /// xy - the amount to shear X by Y.
    /// xz - the amount to shear X by Z.
    /// yx - the amount to shear Y by X.
    /// yz - the amount to shear Y by Z.
    /// zx - the amount to shear Z by X.
    /// zy - the amount to shear Z by Y.
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
    
    /// Creates a Matrix to transform from World Plane or Coordinate System to given Plane
    /// Also called Change of Basis
    static member createToPlane(p:PPlane) =
        Matrix(
            p.Xax.X , p.Yax.X , p.Zax.X ,  p.Origin.X , 
            p.Xax.Y , p.Yax.Y , p.Zax.Y ,  p.Origin.Y , 
            p.Xax.Z , p.Yax.Z , p.Zax.Z ,  p.Origin.Z , 
            0       ,       0 ,        0,            1 )
            
            
    /// Creates a Matrix to transform from one Plane or Coordinate System to another Plane 
    static member createPlaneToPlane(fromPlane:PPlane,  toPlane:PPlane) =
        let f = fromPlane |> Matrix.createToPlane |> Matrix.inverse
        let t = toPlane   |> Matrix.createToPlane 
        f*t
    
    /// Creates a Matrix to mirror on a Plane  
    static member createMirror (p:PPlane) = 
        let toPlane   =  Matrix.createToPlane p
        let fromPlane =  toPlane.Inverse
        let zFlip     =  Matrix.createScale(1,1,-1)  
        fromPlane*zFlip*toPlane
    
    /// Create Matrix from Quaternion
    static member createfromQuaternion( quaternion:Quaternion) =
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
        Matrix (( 1. - ( yy + zz ) ) 
                ,( xy + wz ) 
                ,( xz - wy ) 
                ,0
                ,( xy - wz ) 
                ,( 1. - ( xx + zz ) ) 
                ,( yz + wx ) 
                ,0                
                ,( xz + wy ) 
                ,( yz - wx ) 
                ,( 1. - ( xx + yy ) ) 
                ,0 
                ,0 ,0 ,0 ,1 )

    /// Compose a center, a quaternion and scale to one Matrix
    static member compose( position:Pnt, quaternion:Quaternion, scale:Vec ) =
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
        let sx = scale.X
        let sy = scale.Y
        let sz = scale.Z
        Matrix (( 1. - ( yy + zz ) ) * sx
                ,( xy + wz ) * sx
                ,( xz - wy ) * sx
                ,0
                ,( xy - wz ) * sy
                ,( 1. - ( xx + zz ) ) * sy
                ,( yz + wx ) * sy
                ,0                
                ,( xz + wy ) * sz
                ,( yz - wx ) * sz
                ,( 1. - ( xx + yy ) ) * sz
                ,0
                ,position.X
                ,position.Y
                ,position.Z
                ,1
                )




    /// Creates a matrix from array of 16 elements in Column Major order:
    /// [| M11 M12 M13 M14 M21 M22 M23 M24 M31 M32 M33 M34 X41 Y42 Z43 M44 |] 
    static member createFromColumMajorArray (xs:float[]) =
        if xs.Length <> 16 then 
            FsExGeoException.Raise "FsEx.Geo.Matrix.createFromColumMajorArray expects an array of 16 items but got %d " xs.Length
        else
            Matrix ( 
                xs[0],  xs[4],  xs[ 8],  xs[12] , 
                xs[1],  xs[5],  xs[ 9],  xs[13] , 
                xs[2],  xs[6],  xs[10],  xs[14] , 
                xs[3],  xs[7],  xs[11],  xs[15] )


    /// Creates a matrix from array of 16 elements in Row Major order:
    /// [| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 M14 M24 M34 M44 |] 
    static member createFromRowMajorArray (xs:float[]) =
        if xs.Length <> 16 then 
            FsExGeoException.Raise "FsEx.Geo.Matrix.createFromRowMajorArray expects an array of 16 items but got %d " xs.Length
        else
            Matrix ( 
                xs[ 0],  xs[ 1],  xs[ 2],  xs[ 3] , 
                xs[ 4],  xs[ 5],  xs[ 6],  xs[ 7] , 
                xs[ 8],  xs[ 9],  xs[10],  xs[11] , 
                xs[12],  xs[13],  xs[14],  xs[15] )
 
   