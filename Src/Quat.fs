namespace FsEx.Geo

open System
open System.Runtime.CompilerServices // for [<IsByRefLike; IsReadOnly>] see https://learn.microsoft.com/en-us/dotnet/api/system.type.isbyreflike
open Util    

#nowarn "44" // for hidden constructors via Obsolete Attribute

 /// Quaternion, for arbitrary 3D rotations.
 /// This implementation guarantees the Quat to be always unitized.
 [<Struct; NoEquality; NoComparison>]
 [<IsReadOnly>]
//[<IsByRefLike>]
 type Quat = 
     // https://www.youtube.com/watch?v=zjMuIxRvygQ
     // https://github.com/mcneel/rhinocommon/blob/master/dotnet/opennurbs/opennurbs_quaternion.cs 
     // http://www.codeproject.com/Articles/36868/Quaternion-Mathematics-and-3D-Library-with-C-and-G
     // http://physicsforgames.blogspot.co.at/2010/02/quaternions.html
     // http://www.ogre3d.org/tikiwiki/Quaternion+and+Rotation+Primer    
     // https://referencesource.microsoft.com/0PresentationCore/Core/CSharp/System/Windows/Media3D/Quaternion.cs
     // https://github.com/mrdoob/three.js/blob/dev/src/math/Quaternion.js
     
     val X:float
     val Y:float
     val Z:float  
     val W:float
     
     /// Unsave internal constructor,  public only for inlining.
     [<Obsolete("Unsave internal constructor,  but must be public for inlining. So marked Obsolete instead. Use #nowarn \"44\" to hide warning.") >] 
     new (x,y,z,w) = 
         #if DEBUG
         let l = x*x  + y*y + z*z + w*w 
         if 0.999999999 > l || l > 1.000000001 then  FsExGeoException.Raise $"FsEx.Geo.Quat Constructor failed for x:%g{x}, y:%g{y}, z:%g{z}, w:%g{w}. The length needs to be 1.0."   
         #endif
         {X=x; Y=y; Z=z; W=w}
    
     override q.ToString() = 
        let deg = 
            let mutable w = q.W
            if w < 0.0 then w <-0.0 // clamp,  to avoid error in acos
            if w > 1.0 then w <-1.0
            (Math.Acos w) * 2.0 |>  toDegrees |> Format.float
        $"FsEx.Geo.Quat(X=%s{Format.float q.X}, Y=%s{Format.float q.Y}, Z=%s{Format.float q.Z},W=%s{Format.float q.W}, angle: %s{deg}°)"          
     
     static member inline ( * ) (l:Quat, r:Quat)  =  
         Quat(   l.W * r.X + l.X * r.W + l.Y * r.Z - l.Z * r.Y ,
                 l.W * r.Y + l.Y * r.W + l.Z * r.X - l.X * r.Z ,
                 l.W * r.Z + l.Z * r.W + l.X * r.Y - l.Y * r.X ,
                 l.W * r.W - l.X * r.X - l.Y * r.Y - l.Z * r.Z ) 
                 
     member inline q.Conjugate = Quat (-q.X, -q.Y, -q.Z, q.W)  
     
     /// Schould allways be 1.0
     member inline q.Magnitude = sqrt (q.X*q.X + q.Y*q.Y + q.Z*q.Z + q.W*q.W) 
     
     /// Returns Angle in Degree 
     member q.AngleInRadians =  
         // better alternative : https://referencesource.microsoft.com/0PresentationCore/Core/CSharp/System/Windows/Media3D/Quaternion.cs,185 ?
         let mutable w = q.W
         if w < 0.0 then w <-0.0 // clamp,  to avoid error in acos
         if w > 1.0 then w <-1.0
         (acos w) * 2.0 

     /// Returns Angle in Degree 
     member inline q.AngleInDegrees =  
         q.AngleInRadians |>  toDegrees
     
     /// Transform a Point round Pnt.Zero
     member q.Rotate(pt:Pnt)  =  
         // https://gamedev.stackexchange.com/a/28418
         let twoX = q.X + q.X
         let twoY = q.Y + q.Y
         let twoZ = q.Z + q.Z 
         let WXX = q.W * twoX
         let WYY = q.W * twoY
         let WZZ = q.W * twoZ
         let XXX = q.X * twoX
         let XYY = q.X * twoY
         let XZZ = q.X * twoZ
         let YYY = q.Y * twoY
         let YZZ = q.Y * twoZ
         let ZZZ = q.Z * twoZ
         // TODO this is actually a 3x3 Matrix: https://github.com/mrdoob/three.js/blob/dev/src/math/Matrix4.js
         let xx = 1.0 - YYY - ZZZ
         let xy = XYY - WZZ
         let xz = XZZ + WYY
         let yx = XYY + WZZ
         let yy = 1.0 - XXX - ZZZ
         let yz = YZZ - WXX
         let zx = XZZ - WYY
         let zy = YZZ + WXX
         let zz = 1.0 - XXX - YYY 
         Pnt ( pt.X * xx + pt.Y * xy + pt.Z * xz
             , pt.X * yx + pt.Y * yy + pt.Z * yz    
             , pt.X * zx + pt.Y * zy + pt.Z * zz 
             ) 
     /// Transform a Vector        
     member q.Rotate(v:Vec)  =  
         // https://gamedev.stackexchange.com/a/28418
         let twoX = q.X + q.X
         let twoY = q.Y + q.Y
         let twoZ = q.Z + q.Z 
         let WXX = q.W * twoX
         let WYY = q.W * twoY
         let WZZ = q.W * twoZ
         let XXX = q.X * twoX
         let XYY = q.X * twoY
         let XZZ = q.X * twoZ
         let YYY = q.Y * twoY
         let YZZ = q.Y * twoZ
         let ZZZ = q.Z * twoZ
         // TODO this is actually a 3x3 Matrix: https://github.com/mrdoob/three.js/blob/dev/src/math/Matrix4.js
         let xx = 1.0 - YYY - ZZZ
         let xy = XYY - WZZ
         let xz = XZZ + WYY
         let yx = XYY + WZZ
         let yy = 1.0 - XXX - ZZZ
         let yz = YZZ - WXX
         let zx = XZZ - WYY
         let zy = YZZ + WXX
         let zz = 1.0 - XXX - YYY 
         Vec ( v.X * xx + v.Y * xy + v.Z * xz
             , v.X * yx + v.Y * yy + v.Z * yz    
             , v.X * zx + v.Y * zy + v.Z * zz 
             )         
     /// Transform a UnitVector         
     member q.Rotate(v:UnitVec)  =  
         // https://gamedev.stackexchange.com/a/28418
         let twoX = q.X + q.X
         let twoY = q.Y + q.Y
         let twoZ = q.Z + q.Z 
         let WXX = q.W * twoX
         let WYY = q.W * twoY
         let WZZ = q.W * twoZ
         let XXX = q.X * twoX
         let XYY = q.X * twoY
         let XZZ = q.X * twoZ
         let YYY = q.Y * twoY
         let YZZ = q.Y * twoZ
         let ZZZ = q.Z * twoZ
         // TODO this is actually a 3x3 Matrix: https://github.com/mrdoob/three.js/blob/dev/src/math/Matrix4.js
         let xx = 1.0 - YYY - ZZZ
         let xy = XYY - WZZ
         let xz = XZZ + WYY
         let yx = XYY + WZZ
         let yy = 1.0 - XXX - ZZZ
         let yz = YZZ - WXX
         let zx = XZZ - WYY
         let zy = YZZ + WXX
         let zz = 1.0 - XXX - YYY 
         UnitVec ( v.X * xx + v.Y * xy + v.Z * xz
             , v.X * yx + v.Y * yy + v.Z * yz    
             , v.X * zx + v.Y * zy + v.Z * zz 
             ) 
     
     /// Transform a Point round a center point 
     member q.RotateWithCenter(cen:Pnt, pt:Pnt) :Pnt =  
         let p = pt-cen 
         let r = q.Rotate(p) 
         cen + r 
     
     /// The quaternion expresses a relationship between two coordinate frames, A and B say. 
     /// Returns the EulerAngles in degrees: Alpha, Beta , Gamma.
     /// This relationship, if expressed using Euler angles, is as follows:
     /// 1) Rotate frame A about its z axis by angle gamma;
     /// 2) Rotate the resulting frame about its (new) y axis by angle beta;
     /// 3) Rotate the resulting frame about its (new) x axis by angle alpha, to arrive at frame B.
     static member toEulerAngles(q:Quat)= 
         // from https://github.com/mathnet/mathnet-spatial/blob/8f08be97b4b6d2ff676ee51dd91f88f7818bad3a/src/Spatial/Euclidean/Quaternion.cs#L499
         toDegrees (Math.Atan2(2.0 * ((q.W * q.X) + (q.Y * q.Z)), (q.W * q.W) + (q.Z * q.Z) - (q.X * q.X) - (q.Y * q.Y))),
         toDegrees (Math.Asin(2.0 *  ((q.W * q.Y) - (q.X * q.Z)))),
         toDegrees (Math.Atan2(2.0 * ((q.W * q.Z) + (q.X * q.Y)), (q.W * q.W) + (q.X * q.X) - (q.Y * q.Y) - (q.Z * q.Z)))

     /// The created Rotation is Clockwise looking in the direction of the Vector 
     static member createFromRadians (axis:Vec, angleInRadians)  =
         // from https://referencesource.microsoft.com/0PresentationCore/Core/CSharp/System/Windows/Media3D/Quaternion.cs,91 
         let mutable li = sqrt(axis.X*axis.X + axis.Y*axis.Y + axis.Z*axis.Z) 
         if li <  zeroLenghtTol then FsExGeoException.Raise $"FsEx.Geo.Quat.createFromRadians failed too short axis %O{axis} and rotation:%g{toDegrees angleInRadians}°" // or return identity Quat ?
         let angHalf = angleInRadians * 0.5
         let sa = sin angHalf
         li <- 1. / li // inverse for unitizing vector:
         Quat ( axis.X * li * sa, axis.Y * li * sa, axis.Z * li * sa, cos angHalf )
     
     /// The created Rotation is Clockwise looking in the direction of the Vector
     static member inline createFromDegree (axis : Vec, angleInDegrees) = 
         Quat.createFromRadians (axis,  toRadians angleInDegrees) 




       
