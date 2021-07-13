namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open FsEx.Geo.Util    

#nowarn "44" // for hidden constructors via Obsolete Attribute

/// Quaternion, for arbitrary 3D rotations.
/// This implementation guarantees the Quaternion to be always unitized.
[<Struct; NoEquality; NoComparison>] 
[<IsReadOnly>]
//[<IsByRefLike>]
type Quaternion = 
    //  https://github.com/mrdoob/three.js/blob/dev/src/math/Quaternion.js
     
    val X:float
    val Y:float
    val Z:float  
    val W:float
     
    /// Unsafe internal constructor,  public only for inlining.
    [<Obsolete("Unsafe internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
    new (x,y,z,w) = 
        #if DEBUG
        let l = x*x  + y*y + z*z + w*w 
        if 0.99999 > l || l > 1.00001 then  
            FsExGeoException.Raise "FsEx.Geo.Quaternion Constructor failed for x:%g, y:%g, z:%g, w:%g. The length needs to be 1.0." x y z w
        #endif
        {X=x; Y=y; Z=z; W=w}
    
    override q.ToString() =         
        sprintf "FsEx.Geo.Quaternion(X=%s, Y=%s, Z=%s, W=%s, angle: %s°)" 
                (Format.float q.X) (Format.float q.Y) (Format.float q.Z) (Format.float q.W) (Format.float q.AngleInDegrees)
     

    static member multiply (l:Quaternion, r:Quaternion)  =  
        Quaternion(   
            l.W * r.X + l.X * r.W + l.Y * r.Z - l.Z * r.Y ,
            l.W * r.Y + l.Y * r.W + l.Z * r.X - l.X * r.Z ,
            l.W * r.Z + l.Z * r.W + l.X * r.Y - l.Y * r.X ,
            l.W * r.W - l.X * r.X - l.Y * r.Y - l.Z * r.Z ) 

    static member inline ( * ) (l:Quaternion, r:Quaternion)  =  
        Quaternion.multiply(l,r)
                 
    member q.Conjugate = Quaternion (-q.X, -q.Y, -q.Z, q.W)  
     
    /// Should always be 1.0
    member q.Magnitude = sqrt (q.X*q.X + q.Y*q.Y + q.Z*q.Z + q.W*q.W) 
     
    /// Returns Angle in Radians 
    member q.AngleInRadians =  
        q.W |> acosSafe |> ( * ) 2.0 

    /// Returns Angle in Degree 
    member q.AngleInDegrees =  
        q.AngleInRadians |>  toDegrees
    
    /// This constructor does unitizing too
    static member create (x,y,z,w)  =  
        let l = sqrt(x*x  + y*y + z*z + w*w )
        if abs l < zeroLengthTol then  
            FsExGeoException.Raise "FsEx.Geo.Quaternion create failed for x:%g, y:%g, z:%g, w:%g. The length needs to be bigge than zero" x y z w
        let sc = 1./l
        Quaternion(x*sc,y*sc,z*sc,w*sc)

    /// The created Rotation is Clockwise looking in the direction of the vector.
    /// The vector may be of any length but zero.
    static member createFromRadians (axis:Vec, angleInRadians)  =        
        let length = sqrt(axis.X*axis.X + axis.Y*axis.Y + axis.Z*axis.Z) 
        if length <  zeroLengthTol then // TODO or return identity Quaternion ?
            FsExGeoException.Raise "FsEx.Geo.Quaternion.createFromRadians failed too short axis: %O and rotation: %g° degrees" axis (toDegrees angleInRadians)
        let angHalf = angleInRadians * 0.5
        let sa = sin angHalf
        let sc =  1. / length // inverse for unitizing vector:
        Quaternion ( axis.X * sc * sa, axis.Y * sc * sa, axis.Z * sc * sa, cos angHalf )

    /// The created Rotation is Clockwise looking in the direction of the Unit Vector.
    static member createFromRadians (axis:UnitVec, angleInRadians)  =         
        let angHalf = angleInRadians * 0.5
        let sa = sin angHalf         
        Quaternion ( axis.X  * sa, axis.Y  * sa, axis.Z  * sa, cos angHalf )
     
    /// The created Rotation is Clockwise looking in the direction of the Vector (of any length but zero).
    static member createFromDegree (axis:Vec, angleInDegrees) = 
        Quaternion.createFromRadians (axis,  toRadians angleInDegrees) 
     
    /// The created Rotation is Clockwise looking in the direction of the Unit Vector.
    static member createFromDegree (axis:UnitVec, angleInDegrees) = 
        Quaternion.createFromRadians (axis,  toRadians angleInDegrees) 

    /// Creates a rotation from one vector to another (both of non zero length)  
    static member createFromVectors( vFrom:UnitVec, vTo:UnitVec ) =
        let mutable r = vFrom * vTo  + 1.0
        if  r < zeroLengthTol  then // vFrom and vTo point in opposite directions            
            r <- 0.0
            if  abs vFrom.X  > abs vFrom.Z   then   Quaternion.create ( -vFrom.Y  , vFrom.X   , 0       , r)
            else                                    Quaternion.create (0          ,- vFrom.Z  ,vFrom.Y  , r)
        else 
            // crossVectors( vFrom, vTo ); // inlined to avoid cyclic dependency on Vector3
            Quaternion.create ( vFrom.Y * vTo.Z - vFrom.Z * vTo.Y
                              , vFrom.Z * vTo.X - vFrom.X * vTo.Z
                              , vFrom.X * vTo.Y - vFrom.Y * vTo.X
                              , r
                              )  
    /// Creates a rotation from one unit vector to another 
    static member createFromVectors( vFrom:Vec, vTo:Vec ) =
        let vFrom = 
            let length = sqrt(vFrom.X*vFrom.X + vFrom.Y*vFrom.Y + vFrom.Z*vFrom.Z) 
            if length <  zeroLengthTol then // TODO or return identity Quaternion ?
                FsExGeoException.Raise "FsEx.Geo.Quaternion.createFromVectors failed too short vector vFrom: %O" vFrom 
            let sc =  1. / length // inverse for unitizing vector:
            UnitVec.createUnchecked(vFrom.X*sc, vFrom.Y*sc, vFrom.Z*sc)
        let vTo = 
            let length = sqrt(vTo.X*vTo.X + vTo.Y*vTo.Y + vTo.Z*vTo.Z) 
            if length <  zeroLengthTol then // TODO or return identity Quaternion ?
                FsExGeoException.Raise "FsEx.Geo.Quaternion.createFromVectors failed too short vector vTo: %O" vTo 
            let sc =  1. / length // inverse for unitizing vector:
            UnitVec.createUnchecked(vTo.X*sc, vTo.Y*sc, vTo.Z*sc)
        let mutable r = vFrom * vTo  + 1.0
        if  r < zeroLengthTol  then // vFrom and vTo point in opposite directions            
            r <- 0.0
            if  abs vFrom.X  > abs vFrom.Z   then   Quaternion.create ( -vFrom.Y  , vFrom.X   , 0       , r)
            else                                    Quaternion.create (0          ,- vFrom.Z  ,vFrom.Y  , r)
        else 
            // crossProduct:
            Quaternion.create ( vFrom.Y * vTo.Z - vFrom.Z * vTo.Y
                              , vFrom.Z * vTo.X - vFrom.X * vTo.Z
                              , vFrom.X * vTo.Y - vFrom.Y * vTo.X
                              , r
                              )  


    /// Angles are given in degrees, 
    /// The order in which to apply rotations is X-Y-Z, 
    /// which means that the object will first be rotated around its X axis, 
    /// then its Y axis 
    /// and finally its Z axis. 
    /// This  uses intrinsic Tait-Bryan angles. 
    /// This means that rotations are performed with respect to the local coordinate system. 
    /// That is, for order X-Y-Z, the rotation is first around the local-X axis (which is the same as the world-X axis), 
    /// then around local-Y (which may now be different from the world Y-axis), 
    /// then local-Z (which may be different from the world Z-axis)
    static member createFromEulerXYZ(degreesX,degreesY,degreesZ) =
        let c1 = cos(toRadians degreesX * 0.5 )
        let c2 = cos(toRadians degreesY * 0.5 )
        let c3 = cos(toRadians degreesZ * 0.5 )
        let s1 = sin(toRadians degreesX * 0.5 )
        let s2 = sin(toRadians degreesY * 0.5 )
        let s3 = sin(toRadians degreesZ * 0.5 ) 
        Quaternion(  s1 * c2 * c3 + c1 * s2 * s3
                  ,  c1 * s2 * c3 - s1 * c2 * s3
                  ,  c1 * c2 * s3 + s1 * s2 * c3
                  ,  c1 * c2 * c3 - s1 * s2 * s3) 
                  
    /// Angles are given in degrees, 
    /// The order in which to apply rotations is Y-X-Z, 
    /// which means that the object will first be rotated around its Y axis, 
    /// then its X axis 
    /// and finally its Z axis. 
    /// This  uses intrinsic Tait-Bryan angles. 
    /// This means that rotations are performed with respect to the local coordinate system. 
    /// That is, for order Y-X-Z, the rotation is first around the local-Y axis (which is the same as the world-Y axis), 
    /// then around local-X (which may now be different from the world X-axis), 
    /// then local-Z (which may be different from the world Z-axis)
    static member createFromEulerYXZ(degreesY,degreesX,degreesZ) =
        let c1 = cos(toRadians degreesX * 0.5 )
        let c2 = cos(toRadians degreesY * 0.5 )
        let c3 = cos(toRadians degreesZ * 0.5 )
        let s1 = sin(toRadians degreesX * 0.5 )
        let s2 = sin(toRadians degreesY * 0.5 )
        let s3 = sin(toRadians degreesZ * 0.5 ) 
        Quaternion(  s1 * c2 * c3 + c1 * s2 * s3
                  ,  c1 * s2 * c3 - s1 * c2 * s3
                  ,  c1 * c2 * s3 - s1 * s2 * c3
                  ,  c1 * c2 * c3 + s1 * s2 * s3)  
  
                
    /// Angles are given in degrees, 
    /// The order in which to apply rotations is Z-X-Y, 
    /// which means that the object will first be rotated around its Z axis, 
    /// then its X axis 
    /// and finally its Y axis. 
    /// This  uses intrinsic Tait-Bryan angles. 
    /// This means that rotations are performed with respect to the local coordinate system. 
    /// That is, for order Z-X-Y, the rotation is first around the local-Z axis (which is the same as the world-Z axis), 
    /// then around local-X (which may now be different from the world X-axis), 
    /// then local-Y (which may be different from the world Y-axis)
    static member createFromEulerZXY(degreesZ, degreesX, degreesY) =
        let c1 = cos(toRadians degreesX * 0.5 )
        let c2 = cos(toRadians degreesY * 0.5 )
        let c3 = cos(toRadians degreesZ * 0.5 )
        let s1 = sin(toRadians degreesX * 0.5 )
        let s2 = sin(toRadians degreesY * 0.5 )
        let s3 = sin(toRadians degreesZ * 0.5 ) 
        Quaternion(  s1 * c2 * c3 - c1 * s2 * s3
                  ,  c1 * s2 * c3 + s1 * c2 * s3
                  ,  c1 * c2 * s3 + s1 * s2 * c3
                  ,  c1 * c2 * c3 - s1 * s2 * s3)             
                
                
    /// Angles are given in degrees, 
    /// The order in which to apply rotations is Z-Y-X, 
    /// which means that the object will first be rotated around its Z axis, 
    /// then its Y axis 
    /// and finally its X axis. 
    /// This  uses intrinsic Tait-Bryan angles. 
    /// This means that rotations are performed with respect to the local coordinate system. 
    /// That is, for order Z-Y-X, the rotation is first around the local-Z axis (which is the same as the world-Z axis), 
    /// then around local-Y (which may now be different from the world Y-axis), 
    /// then local-X (which may be different from the world X-axis)
    static member createFromEulerZYX(degreesZ, degreesY, degreesX) =
        let c1 = cos(toRadians degreesX * 0.5 )
        let c2 = cos(toRadians degreesY * 0.5 )
        let c3 = cos(toRadians degreesZ * 0.5 )
        let s1 = sin(toRadians degreesX * 0.5 )
        let s2 = sin(toRadians degreesY * 0.5 )
        let s3 = sin(toRadians degreesZ * 0.5 ) 
        Quaternion(  s1 * c2 * c3 - c1 * s2 * s3
                  ,  c1 * s2 * c3 + s1 * c2 * s3
                  ,  c1 * c2 * s3 - s1 * s2 * c3
                  ,  c1 * c2 * c3 + s1 * s2 * s3)             
                
    /// Angles are given in degrees, 
    /// The order in which to apply rotations is Y-Z-X, 
    /// which means that the object will first be rotated around its Y axis, 
    /// then its Z axis 
    /// and finally its X axis. 
    /// This  uses intrinsic Tait-Bryan angles. 
    /// This means that rotations are performed with respect to the local coordinate system. 
    /// That is, for order Y-Z-X, the rotation is first around the local-Y axis (which is the same as the world-Y axis), 
    /// then around local-Z (which may now be different from the world Z-axis), 
    /// then local-X (which may be different from the world X-axis)
    static member createFromEulerYZX(degreesY, degreesZ, degreesX) =
        let c1 = cos(toRadians degreesX * 0.5 )
        let c2 = cos(toRadians degreesY * 0.5 )
        let c3 = cos(toRadians degreesZ * 0.5 )
        let s1 = sin(toRadians degreesX * 0.5 )
        let s2 = sin(toRadians degreesY * 0.5 )
        let s3 = sin(toRadians degreesZ * 0.5 ) 
        Quaternion(  s1 * c2 * c3 + c1 * s2 * s3
                  ,  c1 * s2 * c3 + s1 * c2 * s3
                  ,  c1 * c2 * s3 - s1 * s2 * c3
                  ,  c1 * c2 * c3 - s1 * s2 * s3)             
                
    /// Angles are given in degrees, 
    /// The order in which to apply rotations is X-Z-Y, 
    /// which means that the object will first be rotated around its X axis, 
    /// then its Z axis 
    /// and finally its Y axis. 
    /// This  uses intrinsic Tait-Bryan angles. 
    /// This means that rotations are performed with respect to the local coordinate system. 
    /// That is, for order X-Z-Y, the rotation is first around the local-X axis (which is the same as the world-X axis), 
    /// then around local-Z (which may now be different from the world Z-axis), 
    /// then local-Y (which may be different from the world Y-axis)
    static member createFromEulerXZY(degreesX, degreesZ, degreesY) =
        let c1 = cos(toRadians degreesX * 0.5 )
        let c2 = cos(toRadians degreesY * 0.5 )
        let c3 = cos(toRadians degreesZ * 0.5 )
        let s1 = sin(toRadians degreesX * 0.5 )
        let s2 = sin(toRadians degreesY * 0.5 )
        let s3 = sin(toRadians degreesZ * 0.5 ) 
        Quaternion(  s1 * c2 * c3 - c1 * s2 * s3
                  ,  c1 * s2 * c3 - s1 * c2 * s3
                  ,  c1 * c2 * s3 + s1 * s2 * c3
                  ,  c1 * c2 * c3 + s1 * s2 * s3)             



    /// The quaternion expresses a relationship between two coordinate frames, A and B say. 
    /// Returns the EulerAngles in degrees: Alpha, Beta , Gamma.
    /// This relationship, if expressed using Euler angles, is as follows:
    /// 1) Rotate Frame A about its Z axis by angle Gamma;
    /// 2) Rotate the resulting frame about its (new) Y axis by angle Beta;
    /// 3) Rotate the resulting frame about its (new) X axis by angle Alpha, to arrive at frame B.
    /// Returns the angels in degrees as triple. For rotating first round the axis Z then local Y and finally local X.
    /// see Quaternion.createFromEulerZYX(z,y,x)
    static member toEulerAnglesZYX(q:Quaternion) : float*float*float = 
        // from https://github.com/mathnet/mathnet-spatial/blob/master/src/Spatial/Euclidean/Quaternion.cs#L491
        toDegrees <| Math.Atan2( 2.0 * (q.W*q.Z + q.X*q.Y),  q.W*q.W + q.X*q.X - q.Y*q.Y - q.Z*q.Z)
        ,
        toDegrees <|  asinSafe (2.0 * (q.W*q.Y - q.X*q.Z))
        ,
        toDegrees  <| Math.Atan2(2.0 * (q.W*q.X + q.Y*q.Z),  q.W*q.W + q.Z*q.Z - q.X*q.X - q.Y*q.Y)
     

       