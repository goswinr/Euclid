namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open FsEx.Geo.Util

/// An immutable 4x3 Orthogonal matrix. For only Rotation and Translation in 3D space.
/// This matrix guarantees to NOT scale, shear, flip, mirror, reflect or project.
/// Angles are preserved. Lengths are preserved. Area is preserved. Volume is preserved
/// An orthogonal matrix is a matrix whose 3x3 columns and rows are orthogonal unit vectors.
/// The matrix is represented in the following column-vector syntax form:
/// M11 M21 M31 X41 
/// M12 M22 M32 Y42 
/// M13 M23 M33 Z43  
/// Where X41, Y42 and Z43 refer to the translation part of the OrthoMatrix.
/// The Determinant of this matrix is always 1.0.
/// Note: Never use the struct default constructor OrthoMatrix() as it will create an invalid zero OrthoMatrix. 
/// Use OrthoMatrix.create or OrthoMatrix.createUnchecked instead.
[<Struct; NoEquality; NoComparison>] // because its made up from floats
[<IsReadOnly>]
//[<IsByRefLike>]
type OrthoMatrix = 
    val M11 :float ; val M21 :float ; val M31 :float; val X41:float
    val M12 :float ; val M22 :float ; val M32 :float; val Y42:float
    val M13 :float ; val M23 :float ; val M33 :float; val Z43:float        
        
    /// Create immutable a 4x3 Transformation Matrix. For only Rotation and Translation in 3D space.
    /// This Constructor takes arguments in row-major order, 
    /// The matrix is represented in the following column-vector syntax form: 
    /// M11 M21 M31 X41 
    /// M12 M22 M32 Y42 
    /// M13 M23 M33 Z43  
        /// Where X41, Y42 and Z43 refer to the translation part of the OrthoMatrix.
        internal new(   m11,  m21,  m31,  x41, 
                        m12,  m22,  m32,  y42, 
                        m13,  m23,  m33,  z43) = {
                            M11=m11 ; M21=m21 ; M31=m31 ; X41=x41 ;
                            M12=m12 ; M22=m22 ; M32=m32 ; Y42=y42 ;
                            M13=m13 ; M23=m23 ; M33=m33 ; Z43=z43 }
                            
    /// Returns the 12 elements column-major order:
    /// [| M11 M12 M13 M21 M22 M23 M31 M32 M33 X41 Y42 Z43 |] 
    /// Where X41, Y42 and Z43 refer to the translation part of the OrthoMatrix.
    member m.ByColumns = 
        [| m.M11; m.M12; m.M13; m.M21; m.M22; m.M23; m.M31; m.M32; m.M33; m.X41; m.Y42; m.Z43 |] 

    /// Returns the 12 elements in row-major order: 
    /// [| M11 M21 M31 X41 M12 M22 M32 Y42 M13 M23 M33 Z43 |] 
    /// Where X41, Y42 and Z43 refer to the translation part of the OrthoMatrix.
    member m.ByRows = 
        [| m.M11; m.M21; m.M31; m.X41; m.M12; m.M22; m.M32; m.Y42; m.M13; m.M23; m.M33; m.Z43 |] 

    /// Nicely formats the Matrix to a Grid of 4x3.
    override m.ToString()= 
        let ts   = m.ByRows |> Array.map ( fun x -> x.ToString("0.###")) 
        let most = ts |> Array.maxBy ( fun s -> s.Length)
        "4x3 Colum-Vector Orthogonal Transformation Matrix:\r\n" + (
        ts   
        |> Array.map ( fun x -> String(' ', most.Length-x.Length) + x ) 
        |> Array.chunkBySize 4
        |> Array.map (fun es -> " " + String.concat " | " es) 
        |> String.concat Environment.NewLine
        )  

    //Nicely formats the Matrix to a Grid of 4x3 including field names.
    //override m.ToString()= 
    //    let names =[| "M11"; "M21"; "M31"; "X41"; "M12"; "M22"; "M32"; "Y42"; "M13"; "M23"; "M33"; "Z43"; "M14"; "M24"; "M34"; "M44"|]
    //    let ts   = (names, m.ByRows)  ||> Array.map2 ( fun n v -> v.ToString("0.###")) 
    //    let most = ts |> Array.maxBy ( fun s -> s.Length)
    //    "Colum-Vector Orthogonal Transformation Matrix:\r\n" + (
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


    /// The Determinant of an Ortho Matrix  is always 1.0
    /// The Determinant describes the volume that a unit cube will have after the matrix was applied
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
        let t11 =  - n23 * n32  + n22 * n33 
        let t12 =    n13 * n32  - n12 * n33 
        let t13 =  - n13 * n22  + n12 * n23 
        n11 * t11 + n21 * t12 + n31 * t13 

    /// Inverts the OrthoMatrix. 
    /// OrthoMatrices have always determinant 1.0 so the can always be inverted.
    member m.Inverse =         
        // this order would actually need to be transposed when comparing to three.js, but its transposed in the output too, so its correct.
        // The internal array in three.js is column major.         
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
        
        OrthoMatrix
              ( -n23*n32  + n22*n33                                                                 // M11
              , n23*n31 - n21*n33                                                                   // M21
              , -n22*n31 + n21*n32                                                                  // M31
              , n23*n32*n41 - n22*n33*n41 - n23*n31*n42 + n21*n33*n42 + n22*n31*n43 - n21*n32*n43   // X41 
              , n13*n32  - n12*n33                                                                  // M12
              , -n13*n31 + n11*n33                                                                  // M22
              , n12*n31 - n11*n32                                                                   // M32
              , n12*n33*n41 - n13*n32*n41 + n13*n31*n42 - n11*n33*n42 - n12*n31*n43 + n11*n32*n43   // Y42 
              , -n13*n22  + n12*n23                                                                 // M13
              , n13*n21 - n11*n23                                                                   // M23
              , -n12*n21 + n11*n22                                                                  // M33
              , n13*n22*n41 - n12*n23*n41 - n13*n21*n42 + n11*n23*n42 + n12*n21*n43 - n11*n22*n43   // Z43 
              )

    /// Checks if the Matrix is an Identity Matrix in the form of:
    /// 1  0  0  0
    /// 0  1  0  0
    /// 0  0  1  0
    /// Using an approximate tolerance of approx 1e-7.
    member m.IsIdentity =               
        isOne  m.M11 && isZero m.M21 && isZero m.M31 && isZero m.X41 &&
        isZero m.M12 && isOne  m.M22 && isZero m.M32 && isZero m.Y42 &&
        isZero m.M13 && isZero m.M23 && isOne  m.M33 && isZero m.Z43 

    //----------------------------------------------------------------------------------------------
    //--------------------------  Static Members  --------------------------------------------------
    //----------------------------------------------------------------------------------------------


    /// Checks if two Matrices are equal within tolerance.
    /// By comparing the fields M11 to M44 each with the given tolerance.
    static member equals tol (a:OrthoMatrix) (b:OrthoMatrix) =        
        abs(a.M11-b.M11) < tol &&
        abs(a.M12-b.M12) < tol &&
        abs(a.M13-b.M13) < tol &&
        abs(a.M21-b.M21) < tol &&
        abs(a.M22-b.M22) < tol &&
        abs(a.M23-b.M23) < tol &&
        abs(a.M31-b.M31) < tol &&
        abs(a.M32-b.M32) < tol &&
        abs(a.M33-b.M33) < tol &&
        abs(a.X41-b.X41) < tol &&
        abs(a.Y42-b.Y42) < tol &&
        abs(a.Z43-b.Z43) < tol 



    /// Multiplies matrixA with matrixB
    /// The resulting transformation will first do matrixA and then matrixB
    static member multiply (matrixA:OrthoMatrix,  matrixB:OrthoMatrix) =  
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
        let b41 = matrixB.X41
        let b42 = matrixB.Y42
        let b43 = matrixB.Z43
        OrthoMatrix( 
             a11*b11 + a12*b21 + a13*b31 ,       // M11
             a21*b11 + a22*b21 + a23*b31 ,       // M21
             a31*b11 + a32*b21 + a33*b31 ,       // M31
             a41*b11 + a42*b21 + a43*b31 + b41 , // X41
             a11*b12 + a12*b22 + a13*b32 ,       // M12
             a21*b12 + a22*b22 + a23*b32 ,       // M22
             a31*b12 + a32*b22 + a33*b32 ,       // M32
             a41*b12 + a42*b22 + a43*b32 + b42 , // Y42
             a11*b13 + a12*b23 + a13*b33 ,       // M13
             a21*b13 + a22*b23 + a23*b33 ,       // M23
             a31*b13 + a32*b23 + a33*b33 ,       // M33
             a41*b13 + a42*b23 + a43*b33 + b43 ) // Z43    
    
    
    /// Multiplies matrixA with matrixB
    /// The resulting transformation will first do matrixA and then matrixB
    static member inline ( * ) (matrixA:OrthoMatrix,  matrixB:OrthoMatrix) = OrthoMatrix.multiply(matrixA, matrixB)
    
    /// If the determinant of the OrthoMatrix. 
    /// The Determinant describes the volume that a unit cube will have have the matrix was applied
    static member inline determinant (m:OrthoMatrix) = m.Determinant
    
    /// Inverses the OrthoMatrix. 
    /// If the determinant is zero the Matrix cannot be inverted. 
    /// An Exception is raised.
    static member inline inverse (m:OrthoMatrix) = m.Inverse
    

    /// Returns the Identity matrix:
    /// 1  0  0  0
    /// 0  1  0  0
    /// 0  0  1  0
    /// 0  0  0  1
    static member identity =
        OrthoMatrix(
            1, 0, 0, 0 ,
            0, 1, 0, 0 ,
            0, 0, 1, 0 )  
    
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
        OrthoMatrix(
            1, 0, 0, x ,
            0, 1, 0, y ,
            0, 0, 1, z )    
            
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
        OrthoMatrix(
            1, 0, 0, v.X ,
            0, 1, 0, v.Y ,
            0, 0, 1, v.Z  )    


    
    /// Creates a rotation transformation matrix around the X-axis 
    /// by angle in Degrees (not Radians).  
    /// angleDegrees — Rotation angle in Degrees.
    /// A positive rotation will be  from Y towards Z-axis,  so counter-clockwise looking onto Y-X Plane
    /// The resulting matrix will be:
    /// 1 0      0        0
    /// 0 cos(θ) -sin(θ)  0
    /// 0 sin(θ) cos(θ)   0
    /// 0 0      0        1
    static member createRotationX( angleDegrees ) =
        let angle = Util.toRadians angleDegrees
        let c = cos angle  
        let s = sin angle 
        OrthoMatrix(
            1, 0,  0, 0,
            0, c, -s, 0,
            0, s,  c, 0 ) 
    
    /// Creates a rotation transformation matrix around the Y-axis 
    /// by angle in Degrees (not Radians). 
    /// angleDegrees — Rotation angle in Degrees.
    /// A positive  rotation will be  from Z towards X-axis,  so counter-clockwise looking onto Z-X Plane
    /// The resulting matrix will be:
    /// cos(θ)  0 sin(θ) 0
    /// 0       1 0      0
    /// -sin(θ) 0 cos(θ) 0
    /// 0       0 0      1
    static member createRotationY( angleDegrees ) =
        let angle = Util.toRadians angleDegrees
        let c = cos angle  
        let s = sin angle 
        OrthoMatrix(
            c  ,  0,  s,  0,
            0  ,  1,  0,  0,
            -s ,  0,  c,  0 )
    
    /// Creates a rotation transformation matrix around the Z-axis 
    /// by angle in Degrees (not Radians). 
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive  rotation will be  from X toward Y-axis,  so counter-clockwise looking onto  X-Y plane
    /// The resulting matrix will be:
    /// cos(θ) -sin(θ) 0 0
    /// sin(θ) cos(θ)  0 0
    /// 0      0       1 0
    /// 0      0       0 1
    static member createRotationZ( angleDegrees ) =
        let angle = Util.toRadians angleDegrees
        let c = cos angle  
        let s = sin angle 
        OrthoMatrix(
            c, -s, 0, 0,
            s,  c, 0, 0,
            0,  0, 1, 0 )

    /// Creates a rotation around an Axis transformation OrthoMatrix.
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
        OrthoMatrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0 )

    /// Creates a rotation around an Axis transformation OrthoMatrix.
    /// axis — Rotation axis, a vector of any length but 0.0 .
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector
    static member createRotationAxis( axis:Vec, angleDegrees:float ) =
        // first unitize
        let len = sqrt (axis.X*axis.X + axis.Y*axis.Y + axis.Z*axis.Z) 
        if len <  zeroLengthTol then 
            FsExGeoException.Raise "FsEx.Geo.OrthoMatrix.createRotationAxis failed on too short axis: %O and rotation: %g° Degrees" axis angleDegrees
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
        OrthoMatrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0 )


    /// Creates a rotation matrix around an Axis at a given center Point.
    /// axis — Rotation axis, a vector of any length but 0.0 
    /// cen — The center point for the rotation
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector
    static member createRotationAxisCenter( axis:Vec, cen:Pnt, angleDegrees:float ) =
        OrthoMatrix.createTranslation(-cen.X, -cen.Y, -cen.Z)
        * OrthoMatrix.createRotationAxis(axis, angleDegrees)
        * OrthoMatrix.createTranslation(cen.X, cen.Y, cen.Z)

    /// Creates a rotation matrix around an Axis at a given center Point.
    /// axis — Rotation axis, a Unit vector 
    /// cen — The center point for the rotation
    /// angleDegrees — Rotation angle in Degrees.
    /// Returns a positive rotation will be so clockwise looking in the direction of the axis vector
    static member createRotationAxisCenter( axis:UnitVec, cen:Pnt, angleDegrees:float ) =
        OrthoMatrix.createTranslation(-cen.X, -cen.Y, -cen.Z)
        * OrthoMatrix.createRotationAxis(axis, angleDegrees)
        * OrthoMatrix.createTranslation(cen.X, cen.Y, cen.Z)

        /// Creates a rotation from one vectors direction to another vectors direction.
    /// Fails on colinear vectors.
    static member createVecToVec( fromVec:UnitVec, toVec:UnitVec ) =
        let c = fromVec*toVec  // dot to find cos
        let s = sqrt(1. - c*c ) // Pythagoras to find sine
        let t = 1.0 - c        
        let axis0 = UnitVec.cross(fromVec, toVec) 
        let len = axis0.Length
        if len <  zeroLengthTol then 
            FsExGeoException.Raise "FsEx.Geo.Matrix.createVecToVec failed to find rotation axis on colinear vectors: %O and %O" fromVec toVec
        let axis = axis0 / len
        let x = axis.X
        let y = axis.Y
        let z = axis.Z
        let tx = t * x
        let ty = t * y
        OrthoMatrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0 )   
            
    /// Creates a rotation from one vectors direction to another vectors direction.
    /// Ignores the vectors length. Fails on colinear vectors.
    static member createVecToVec(vecFrom:Vec, vecTo:Vec ) =
        let fu = 
            let x = vecFrom.X
            let y = vecFrom.Y
            let z = vecFrom.Z
            let length = sqrt(x*x + y*y + z*z) 
            if length <  zeroLengthTol then 
                FsExGeoException.Raise "FsEx.Geo.Matrix.createVecToVec failed. too short vector vecFrom: %O" vecFrom 
            let sc =  1. / length // inverse for unitizing vector:
            UnitVec.createUnchecked(x*sc, y*sc, z*sc)
        let tu = 
            let x = vecTo.X
            let y = vecTo.Y
            let z = vecTo.Z
            let length = sqrt(x*x + y*y + z*z) 
            if length <  zeroLengthTol then
                FsExGeoException.Raise "FsEx.Geo.Matrix.createVecToVec failed. too short vector vecTo: %O" vecTo 
            let sc =  1. / length // inverse for unitizing vector:
            UnitVec.createUnchecked(x*sc, y*sc, z*sc)        
        let c =  fu * tu  // dot to find cosine
        let s = sqrt(1. - c*c) // Pythagoras to fins sine
        let t = 1.0 - c        
        let axis = Vec.cross(vecFrom, vecTo) 
        let len = axis.Length
        if len <  Util.zeroLengthTol then 
            FsExGeoException.Raise "FsEx.Geo.Matrix.createVecToVec failed to find rotation axis on colinear or zero length vectors: %O and %O" vecFrom vecTo
        let sc = 1. / len
        let x = axis.X * sc
        let y = axis.Y * sc
        let z = axis.Z * sc
        let tx = t * x
        let ty = t * y
        OrthoMatrix(
            tx * x + c    , tx * y - s * z , tx * z + s * y , 0,
            tx * y + s * z, ty * y + c     , ty * z - s * x , 0,
            tx * z - s * y, ty * z + s * x , t  * z * z + c , 0 )     

    /// Creates a Matrix to transform from World plane or Coordinate System to given Plane
    /// Also called Change of Basis
    static member createToPlane(p:PPlane) =
        OrthoMatrix(
            p.Xaxis.X , p.Yaxis.X , p.Zaxis.X ,  p.Origin.X , 
            p.Xaxis.Y , p.Yaxis.Y , p.Zaxis.Y ,  p.Origin.Y , 
            p.Xaxis.Z , p.Yaxis.Z , p.Zaxis.Z ,  p.Origin.Z )
            
            
    /// Creates a Matrix to transform from one Plane or Coordinate System to another Plane 
    static member createPlaneToPlane(fromPlane:PPlane,  toPlane:PPlane) =
        let f = fromPlane |> OrthoMatrix.createToPlane |> OrthoMatrix.inverse
        let t = toPlane   |> OrthoMatrix.createToPlane 
        f*t
    
    /// Create a 3x4 OrthoMatrix from a general 4x4 Matrix.
    /// Returns None if the input Matrix does scale, shear, flip, mirror, reflect or project.
    /// However Translation is allowed.
    static member tryCreateFromMatrix (m:Matrix) =
        if m.IsProjecting then
            None
        else            
            let x = Vec(m.M11, m.M12, m.M13)
            let y = Vec(m.M21, m.M22, m.M23)
            let z = Vec(m.M31, m.M32, m.M33)
            let inline sqLen    (v:Vec) = v.X*v.X + v.Y*v.Y + v.Z*v.Z // defined here again, because Vec extension members are not in scope here
            let inline cross (a:Vec) (b:Vec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X )
            if  Util.isOne (sqLen x) && // exclude scaling
                Util.isOne (sqLen y) && 
                Util.isOne (sqLen z) &&
                Util.isZero (x * y)    && // orthogonal if dot product of row or column vectors is be zero
                Util.isZero (x * z)    && 
                Util.isZero (y * z)    &&
                Util.isOne  (( cross x y) * z ) then // check reflecting ( would be -1)
                    Some (OrthoMatrix(  m.M11,  m.M21,  m.M31,  m.X41, 
                                        m.M12,  m.M22,  m.M32,  m.Y42, 
                                        m.M13,  m.M23,  m.M33,  m.Z43)
                        )
            else
                None           

    /// Create Matrix from Quaternion
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
        OrthoMatrix (( 1. - ( yy + zz ) )                      
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
                    ,0)    


    /// Add a vector translation to an existing matrix
    static member addTranslation (v:Vec) (m:OrthoMatrix) =
        OrthoMatrix(m.M11,  m.M21,  m.M31,  m.X41 + v.X, 
                    m.M12,  m.M22,  m.M32,  m.Y42 + v.Y, 
                    m.M13,  m.M23,  m.M33,  m.Z43 + v.Z) 
    
    /// Add a X, Y and Z translation to an existing matrix
    static member addTranslationXYZ x y z  (m:OrthoMatrix) =
        OrthoMatrix(m.M11,  m.M21,  m.M31,  m.X41 + x, 
                    m.M12,  m.M22,  m.M32,  m.Y42 + y, 
                    m.M13,  m.M23,  m.M33,  m.Z43 + z) 


    // ----------------------------------------------
    // operators for matrix multiplication:
    // ----------------------------------------------
    
    /// Multiplies (or applies) an OrthoMatrix to a 3D point. 
    static member inline  ( * ) (v:Vec, m:OrthoMatrix) = 
        let x = v.X
        let y = v.Y
        let z = v.Z 
        Vec(  m.M11*x + m.M21*y + m.M31*z + m.X41 
            , m.M12*x + m.M22*y + m.M32*z + m.Y42 
            , m.M13*x + m.M23*y + m.M33*z + m.Z43 
            )
    
    /// Multiplies (or applies) an OrthoMatrix to a 3D Vector . 
    /// The resulting vector is not unitized if Matrix is translating too.
    static member inline  ( * ) (v:UnitVec, m:OrthoMatrix) = 
        let x = v.X
        let y = v.Y
        let z = v.Z 
        Vec(  m.M11*x + m.M21*y + m.M31*z + m.X41 
            , m.M12*x + m.M22*y + m.M32*z + m.Y42 
            , m.M13*x + m.M23*y + m.M33*z + m.Z43 
            ) 
    
    /// Multiplies (or applies) an OrthoMatrix to a 3D vector. 
    static member inline  ( * ) (v:Pnt, m:OrthoMatrix) = 
        let x = v.X
        let y = v.Y
        let z = v.Z 
        Pnt(  m.M11*x + m.M21*y + m.M31*z + m.X41 
            , m.M12*x + m.M22*y + m.M32*z + m.Y42 
            , m.M13*x + m.M23*y + m.M33*z + m.Z43 
            )                     

