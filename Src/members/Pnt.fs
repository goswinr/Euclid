namespace FsEx.Geo


/// Members and operators for 3D Points, Vectors and Rotations
[<AutoOpen>]
module AutoOpenPnt = 
    open Util

    type Pnt with   
    
        member inline pt.IsOrigin = pt.X = 0.0 && pt.Y = 0.0 && pt.Z= 0.0
        member inline pt.IsAlmostOrigin tol = abs pt.X < tol && abs pt.Y < tol  
       
        //member inline pt.IsInValid =  Double.IsNaN pt.X || Double.IsNaN pt.Y || Double.IsNaN pt.Z || Double.IsInfinity pt.X || Double.IsInfinity pt.Y || Double.IsInfinity pt.Z
        
        member inline pt.WithX x = Pnt (x ,pt.Y, pt.Z) // returns new Vector with new x coordinate, y and z the same as before
        member inline pt.WithY y = Pnt (pt.X, y, pt.Z)
        member inline pt.WithZ z = Pnt (pt.X ,pt.Y, z)
    
        member inline pt.DistFromOrigin = sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z) 
        member inline pt.DistFromOriginSquare = pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z
        member inline pt.DistFromOriginInXY =  sqrt (pt.X*pt.X + pt.Y*pt.Y)
        member inline pt.DistFromOriginSquareInXY = pt.X*pt.X + pt.Y*pt.Y
        member inline pt.WithDistFromOrigin (l:float) = 
            let d = pt.DistFromOrigin 
            if d < zeroLengthTol then FsExGeoException.Raise "pnt.WithDistFromOrigin  %O is too small to be scaled." pt
            pt * (l/d) 
        
        /// Returns the Diamond Angle from this point to another point.
        /// The diamond angle is always positive and in the range of 0.0 to 4.0 ( for 360 degrees) 
        /// 0.0 = XAxis,  going Counter clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        member inline p.DirDiamondTo(o:Pnt) =
            // https://stackoverflow.com/a/14675998/969070            
            let x = o.X-p.X
            let y = o.Y-p.Y
            if abs x < 1e-16 && abs y < 1e-16 then FsExGeoException.Raise "FsEx.Geo.Pnt.DirDiamondTo Failed for too short Distance between %O and %O." p o
            if y >= 0.0 then 
                if x >= 0.0 then   
                    y/(x+y) 
                else             
                    1.0 - x/(-x+y)
            else
                if x < 0.0 then   
                    2.0 - y/(-x-y) 
                else 
                    3.0 + x/(x-y)  
        
        member inline p.AsVec       = Vec(p.X,p.Y,p.Z)
        member inline p.AsPt        = Pt(p.X,p.Y)

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Same as Pnt.Origin
        static member Zero   = Pnt ( 0. , 0. , 0.)  // needed by 'Array.sum' 
        
        /// Same as Pnt.Zero
        static member Origin = Pnt ( 0. , 0. , 0.) 
        
        /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofXYZ pt  = 
            let x = ( ^T : (member X : _) pt)
            let y = ( ^T : (member Y : _) pt)
            let z = ( ^T : (member Z : _) pt)
            try Pnt(float x, float y, float z) 
            with e -> FsExGeoDivByZeroException.Raise "Pnt.ofXYZ: %A could not be converted to a FsEx.Geo.Pnt:\r\n%A" pt e

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxyz pt  = 
            let x = ( ^T : (member x : _) pt)
            let y = ( ^T : (member y : _) pt)
            let z = ( ^T : (member z : _) pt)
            try Pnt(float x, float y, float z) 
            with e -> FsExGeoDivByZeroException.Raise "Pnt.ofxyz: %A could not be converted to a FsEx.Geo.Pnt:\r\n%A" pt e
        
        static member inline ofPt       (p:Pt)      = Pnt (p.X, p.Y, 0.0) 
        static member inline ofVec      (v:Vec)     = Pnt (v.X, v.Y, v.Z)  
        static member inline ofUnitVec  (v:UnitVec) = Pnt (v.X, v.Y, v.Z)                  
        static member inline create     (x:float, y:float, z:float) =  Pnt( x , y , z ) 

        /// Returns a 3D point from z level and 2D point.
        static member inline ofPtAt  (z:float)  (p:Pt)  = Pnt (p.X, p.Y, z) 

        /// Project point to World XY Plane.
        /// Use make2D to convert to 2D point instance
        static member inline projectToXYPlane (pt:Pnt) = Pnt(pt.X,pt.Y, 0.0)
        
        /// Project point to World XY Plane.
        /// Use projectToXYPlane to keep to 3D point instance
        static member inline make2D (pt:Pnt) = Pt(pt.X,pt.Y)
    
        //static member inline DivideByInt (pt:Pnt, i:int) = if i<>0 then pt / float i else failwithf "DivideByInt 0 %O " pt  // needed by  'Array.average'

        /// Sets the X value and returns new Pnt
        static member inline setX x (pt:Pnt) =  Pnt(x, pt.Y, pt.Z)
       
        /// Sets the Y value and returns new Pnt
        static member inline setY y (pt:Pnt) =  Pnt(pt.X, y, pt.Z)
       
        /// Sets the Z value and returns new Pnt
        static member inline setZ z (pt:Pnt) =  Pnt(pt.X, pt.Y, z)
       
        /// Gets the X value of  Pnt
        static member inline getX (pt:Pnt)  =  pt.X
       
        /// Gets the Y value of  Pnt
        static member inline getY (pt:Pnt) =  pt.Y
       
        /// Gets the Z value of  Pnt
        static member inline getZ (pt:Pnt) =  pt.Z

        static member inline add        (a:Pnt) (b:Pnt) = a + b
        static member inline addVec     (v:Vec) (a:Pnt)  = a + v
   
        static member inline midPt      (a:Pnt)   (b:Pnt)         = (a+b) * 0.5
        static member inline scale      (f:float) (pt:Pnt) = pt*f
        
        static member inline shiftX     (x:float) (pt:Pnt) = Pnt (pt.X+x, pt.Y,   pt.Z)
        static member inline shiftY     (y:float) (pt:Pnt) = Pnt (pt.X,   pt.Y+y, pt.Z)
        static member inline shiftZ     (z:float) (pt:Pnt) = Pnt (pt.X,   pt.Y,   pt.Z+z)    
        
        /// Returns the distance between two points
        static member inline distance (a:Pnt) (b:Pnt) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
       
        /// Returns the horizontal distance between two points(ignoring their Z Value)
        static member inline distanceXY (a:Pnt) (b:Pnt) = let x = a.X-b.X in let y=a.Y-b.Y in sqrt(x*x + y*y)
       
        /// Returns the squared distance between two points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        static member inline distanceSq (a:Pnt) (b:Pnt) = let v = a-b in  v.X*v.X + v.Y*v.Y + v.Z*v.Z

        static member inline distFromOrigin (pt:Pnt) = pt.DistFromOrigin
        static member inline setDistFromOrigin f (pt:Pnt) = pt.WithDistFromOrigin f
        static member inline distFromOriginSquare (pt:Pnt) = pt.DistFromOriginSquare
    
        /// Returns angle between three Points in Radians. Range 0.0 to Pi  
        static member inline anglePiPts (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt)  =   
            Vec.anglePI (ptPrev-ptThis) (ptNext-ptThis)

        /// Returns angle between three Points in Degrees. Range 0.0 to 180 
        static member inline angle180Pts (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt)  =   
            Pnt.anglePiPts (ptPrev, ptThis, ptNext) |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction from ptThis. 
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized 
        /// ptPrev * ptThis * ptNext ->   bisector Vector 
        static member inline bisector (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) =  
            (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized   
    
        /// For three Points describing a plane return a normal.
        /// If the returned vector has length zero then the points are in one Line.
        static member normalOf3Pts (a:Pnt, b:Pnt, c:Pnt) =  Vec.cross (a-b, c-b)    
          
       
        /// Returns a point that is at a given distance from a point in the direction of another point.
        static member inline distPt (fromPt:Pnt, dirPt:Pnt,distance:float) : Pnt  = 
            let v = dirPt - fromPt
            let sc = distance/v.Length
            fromPt + v*sc       
       
        /// Returns a Point by evaluation a line between two point with a normalized parameter.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint
        static member inline divPt(fromPt:Pnt, toPt:Pnt,rel:float) : Pnt  = 
            let v = toPt - fromPt
            fromPt + v*rel
       
        /// Returns a point that is at a given Z level,
        /// going from a point in the direction of another point.
        static member inline extendToZLevel (fromPt:Pnt, toPt:Pnt,z:float) = 
            let v = toPt - fromPt
            if fromPt.Z < toPt.Z && z < fromPt.Z  then FsExGeoException.Raise "Pnt.extendToZLevel cannot be reached for fromPt:%O toPt:%O z:%g" fromPt toPt z
            if fromPt.Z > toPt.Z && z > fromPt.Z  then FsExGeoException.Raise "Pnt.extendToZLevel cannot be reached for fromPt:%O toPt:%O z:%g" fromPt toPt z
            let dot = abs ( v * Vec.ZAxis)
            if dot < 0.0001 then  FsExGeoException.Raise "Pnt.extendToZLevel cannot be reached for fromPt:%O toPt:%O because they are both at the same level. target z:%g " fromPt toPt z
            let diffZ = abs (fromPt.Z - z)
            let fac = diffZ / dot
            fromPt + v * fac       
       
             
        /// Applies a translation vector
        static member inline translate (shift:Vec) (pt:Pnt ) = 
            pt + shift      
        
       
        /// Snap to point if within snapDistance
        static member snapIfClose (snapDistance) (snapTo:Pnt) (pt:Pnt) = 
            if (snapTo-pt).Length < snapDistance then snapTo else pt
       
        /// Every line has a normal vector in XY Plane.
        /// If line is vertical then XAxis is returned
        /// Rotated counter clockwise in top view.
        /// result is unitized
        /// see also : Vec.perpendicularVecInXY
        static member normalOfTwoPointsInXY(fromPt:Pnt, toPt:Pnt) = 
            let x = toPt.Y - fromPt.Y
            let y = fromPt.X - toPt.X  // this is the same as: Vec.cross v Vec.ZAxis
            let len = sqrt(x*x + y*y)
            if len < zeroLengthTol then Vec.XAxis
            else Vec(x/len, y/len, 0.0)
       

        /// Offsets two points by two given distances.
        /// The fist distance (distHor) is applied in in XY Plane
        /// The second distance (distNormal) is applied perpendicular to the line (made by the two points) and perpendicular to the horizontal offset direction.
        /// this is in World.Z direction if both points are at the same Z level.
        /// If points are closer than than 1e-6 units the World.XAxis is used as first direction and World.ZAxis as second direction.
        static member offsetTwoPt(    fromPt:Pnt,
                            toPt:Pnt,
                            distHor:float,
                            distNormal:float) : Pnt*Pnt= 
            let v = toPt - fromPt
            let normHor = 
                Vec.cross(v, Vec.ZAxis)
                |> Vec.unitizeOrDefault UnitVec.XAxis
       
            let normFree = 
                Vec.cross(v, normHor)
                |> Vec.unitizeOrDefault UnitVec.ZAxis
       
            let shift = distHor * normHor + distNormal * normFree
            fromPt +  shift, toPt + shift             
                  
    
        /// Multiplies the Matrix with a Point (with an implicit 1 in the 4th dimension), 
        /// So that it also works correctly for projections
        /// See also Pnt.transformSimple for better performance
        static member transform (m:Matrix) (p:Pnt) = 
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
       
        /// Partially Multiplies the Matrix with a Point. 
        /// Use this only for affine transformations that do NOT include a projection 
        /// and if you need maximum performance
        /// The fields m.M14, m.M24 and m.M34 must be 0.0 and m.M44 must be 1.0
        /// Otherwise use Pnt.transform
        static member transformSimple (m:Matrix) (p:Pnt) = 
            let x = p.X
            let y = p.Y
            let z = p.Z 
            Pnt(  m.M11*x + m.M21*y + m.M31*z + m.X41 
                , m.M12*x + m.M22*y + m.M32*z + m.Y42 
                , m.M13*x + m.M23*y + m.M33*z + m.Z43 
                ) 
           
        // Multiplies the Matrix with a Point (with an implicit 1 in the 4th dimension)
        //static member inline ( * ) (matrix:Matrix, pt:Pnt) = //TODO in main declaration ,  not extension
        //    Pnt.transform matrix pt

        // Rotate 2D and 3D: 

        /// Rotate the 3D Point around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member rotateXBy (r:Rotation2D) (p:Pnt) = Pnt (p.X,  r.Cos*p.Y - r.Sin*p.Z, r.Sin*p.Y + r.Cos*p.Z)
        
        /// Rotate the 3D Point around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member rotateYBy (r:Rotation2D) (p:Pnt) = Pnt ( r.Sin*p.Z + r.Cos*p.X,  p.Y, r.Cos*p.Z - r.Sin*p.X) 
        
        /// Rotate the 3D Point around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member rotateZBy (r:Rotation2D) (p:Pnt) = Pnt (r.Cos*p.X - r.Sin*p.Y, r.Sin*p.X + r.Cos*p.Y,  p.Z)
        
        /// Rotate the 3D Point around a center 3D Point and a X aligned axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member rotateXonCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) =  
            let x = pt.X - cen.X 
            let y = pt.Y - cen.Y 
            let z = pt.Z - cen.Z
            Pnt (x                 + cen.X,  
                 r.Cos*y - r.Sin*z + cen.Y, 
                 r.Sin*y + r.Cos*z + cen.Z)         

        /// Rotate the 3D Point around a center Point and a Y aligned axis, from Z to X Axis, Counter Clockwise looking from back.
        static member rotateYonCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) =  
            let x = pt.X - cen.X 
            let y = pt.Y - cen.Y 
            let z = pt.Z - cen.Z
            Pnt ( r.Sin*z + r.Cos*x + cen.X, 
                  y                 + cen.Y, 
                  r.Cos*z - r.Sin*x + cen.Z) 
        
        /// Rotate the 3D Point around a center Point and a Z aligned axis, from X to Y Axis, Counter Clockwise looking from top.
        static member rotateZonCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) =  
            let x = pt.X - cen.X  
            let y = pt.Y - cen.Y 
            let z = pt.Z - cen.Z
            Pnt (r.Cos*x - r.Sin*y + cen.X, 
                 r.Sin*x + r.Cos*y + cen.Y, 
                 z                 + cen.Z)
        
        /// Rotate the 3D Point in Degrees around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (pt:Pnt) = 
            Pnt.rotateXBy (Rotation2D.createFromDegrees angDegree) pt
    
        /// Rotate the 3D Point in Degrees around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (pt:Pnt) = 
            Pnt.rotateYBy  (Rotation2D.createFromDegrees angDegree) pt 
    
        /// Rotate the 3D Point in Degrees around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (pt:Pnt) = 
            Pnt.rotateZBy  (Rotation2D.createFromDegrees angDegree) pt 
    
        /// Rotate the 3D Point in Degrees around center Point and a X aligned axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member inline rotateXonCenter (cen:Pnt) (angDegree) (pt:Pnt) = 
            Pnt.rotateXonCenterBy cen (Rotation2D.createFromDegrees angDegree) pt  

        /// Rotate the 3D Point in Degrees around center Point and a Y aligned axis, from Z to X Axis, Counter Clockwise looking from back.
        static member inline rotateYonCenter (cen:Pnt) (angDegree) (pt:Pnt) = 
            Pnt.rotateYonCenterBy cen (Rotation2D.createFromDegrees angDegree) pt 
    
        /// Rotate the 3D Point in Degrees around center Point and a Z aligned axis, from X to Y Axis, Counter Clockwise looking from top.
        static member inline rotateZonCenter (cen:Pnt) (angDegree) (pt:Pnt) = 
            Pnt.rotateZonCenterBy cen (Rotation2D.createFromDegrees angDegree) pt 

        /// Rotate by Quaternion around Origin
        static member inline rotateByQuat  (q:Quaternion) (pt:Pnt) =
            // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
            let x = pt.X
            let y = pt.Y
            let z = pt.Z
            let qx = q.X
            let qy = q.Y
            let qz = q.Z
            let qw = q.W
            // calculate quat * vector
            let ix =  qw * x + qy * z - qz * y
            let iy =  qw * y + qz * x - qx * z
            let iz =  qw * z + qx * y - qy * x
            let iw = -qx * x - qy * y - qz * z
            // calculate result * inverse quat
            Pnt( ix * qw + iw * - qx + iy * - qz - iz * - qy
               , iy * qw + iw * - qy + iz * - qx - ix * - qz
               , iz * qw + iw * - qz + ix * - qy - iy * - qx
               )
  
        /// Rotate by Quaternion around given Center Point 
        static member inline rotateOnCenterByQuat (cen:Pnt) (q:Quaternion) (pt:Pnt) =
            // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
            let x = pt.X-cen.X
            let y = pt.Y-cen.Y
            let z = pt.Z-cen.Z
            let qx = q.X
            let qy = q.Y
            let qz = q.Z
            let qw = q.W
            // calculate quat * vector
            let ix =  qw * x + qy * z - qz * y
            let iy =  qw * y + qz * x - qx * z
            let iz =  qw * z + qx * y - qy * x
            let iw = -qx * x - qy * y - qz * z
            // calculate result * inverse quat
            Pnt(  ix * qw + iw * - qx + iy * - qz - iz * - qy  + cen.X
                , iy * qw + iw * - qy + iz * - qx - ix * - qz  + cen.Y
                , iz * qw + iw * - qz + ix * - qy - iy * - qx  + cen.Z
                )



