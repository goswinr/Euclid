namespace FsEx.Geo

open System

/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type Pnt
[<AutoOpen>]
module AutoOpenPnt = 
    open Util

    type Pnt with   
        
        /// Returns a boolean indicating wether X , Y and Z are exactly 0.0.
        member inline pt.IsOrigin = pt.X = 0.0 && pt.Y = 0.0 && pt.Z= 0.0
        
        /// Returns a boolean indicating wether the absolute value of X, Y and Z is each less than the given tolerance.
        member inline pt.IsAlmostOrigin tol = abs pt.X < tol && abs pt.Y < tol  
        
        /// Returns new 3D point with new X coordinate, Y and Z stay the same.
        member inline pt.WithX x = Pnt (x ,pt.Y, pt.Z)         
        
        /// Returns a new 3D vector with new y coordinate, X and Z  stay the same.
        member inline pt.WithY y = Pnt (pt.X, y, pt.Z)
        
        /// Returns a new 3D vector with new z coordinate, X and Y  stay the same.
        member inline pt.WithZ z = Pnt (pt.X ,pt.Y, z)

        /// Returns the distance between two points
        member inline p.DistanceTo (b:Pnt) = let v = p-b in sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z )
        
        /// Returns the squared distance between two points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        member inline p.DistanceToSquare (b:Pnt) = let v = p-b in  v.X*v.X + v.Y*v.Y + v.Z*v.Z
        
        /// Returns the distance from Origin (0,0,0)
        member inline pt.DistanceFromOrigin = sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z) 
        
        /// Returns the squared distance from Origin (0,0,0)
        member inline pt.DistanceFromOriginSquare = pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z
        
        /// Returns the projected distance from Origin (0,0,0). Ignoring the Z component.
        member inline pt.DistanceInXYFromOrigin =  sqrt (pt.X*pt.X + pt.Y*pt.Y)
        
        /// Returns the projected square distance from Origin (0,0,0). Ignoring the Z component.
        member inline pt.DistanceInXYFromOriginSquare = pt.X*pt.X + pt.Y*pt.Y
        
        /// Returns new 3D point with given Distance from Origin by scaling it up or down.
        member inline pt.WithDistanceFromOrigin (l:float) = 
            let d = pt.DistanceFromOrigin 
            if d < zeroLengthTol then FsExGeoException.Raise "pnt.WithDistFromOrigin  %O is too small to be scaled." pt
            pt * (l/d) 
        
        /// Returns the Diamond Angle from this point to another point projected in X-Y plane.
        /// The diamond angle is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees) 
        /// 0.0 = XAxis,  going Counter clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        member inline p.DirectionDiamondInXYTo(o:Pnt) =
            // https://stackoverflow.com/a/14675998/969070            
            let x = o.X-p.X
            let y = o.Y-p.Y
            //#if DEBUG 
            if abs(x) < zeroLengthTol && abs(y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Pnt.DirectionDiamondInXYTo failed for too short Distance between %O and %O." p o
            //#endif 
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
        
        /// Returns the Angle in Radians from this point to another point projected in X-Y plane.
        /// 0.0 = XAxis,  going Counter clockwise till two Pi.
        member inline p.Angle2PiInXYTo(o:Pnt) =
            // https://stackoverflow.com/a/14675998/969070            
            let x = o.X-p.X
            let y = o.Y-p.Y
            //#if DEBUG 
            if abs(x) < zeroLengthTol && abs(y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Pnt.Angle2PiInXYTo failed for too short Distance between %O and %O." p o
            //#endif  
            let a = Math.Atan2(y, x) 
            if a < 0. then  a + Util.twoPi
            else            a
        
        /// Returns the Angle in Degrees from this point to another point projected in X-Y plane.
        /// 0.0 = XAxis,  going Counter clockwise till 360.
        member inline p.Angle360InXYTo(o:Pnt) =
            p.Angle2PiInXYTo o |> toDegrees

        /// Returns the 3D point as 3D vector. 
        member inline p.AsVec       = Vec(p.X,p.Y,p.Z)

        /// Returns the 3D point as 2D point. 
        member inline p.AsPt        = Pt(p.X,p.Y)

        /// Get closest point on finit line to test point. 
        member inline testPt.ClosestPointOnLine(fromPt:Pnt, toPt:Pnt) = 
            let dir = testPt - fromPt
            let v   = toPt   - fromPt
            let lenSq = v.LengthSq
            let lenSq =  v.LengthSq
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pt.closestPointOnLine: Line is too short for fromPt %O to  %O and  %O" fromPt toPt testPt
            let dot = Vec.dot (v,  dir) / lenSq
            if   dot <= 0.0 then  fromPt 
            elif dot >= 1.0 then  toPt
            else                 fromPt+dot*v
        
        /// Get closest point on finit line to test point. 
        member inline testPt.ClosestPointOnLine(fromPt:Pnt, uv:UnitVec, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vec.dot (uv,  dir) 
            if   dot <= 0.0 then  fromPt 
            elif dot >= len then (fromPt+len*uv)  
            else                 fromPt+dot*uv 
        
        /// Squared Distance between point and finite line segment defined by start point, direction and length.   
        member inline testPt.DistanceToLineSquare(fromPt:Pnt, uv:UnitVec, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vec.dot (uv,  dir) 
            if   dot <= 0.0 then testPt.DistanceToSquare  fromPt 
            elif dot >= len then testPt.DistanceToSquare (fromPt+len*uv)  
            else                 testPt.DistanceToSquare (fromPt+dot*uv) 
                
        /// Squared Distance between point and finite line segment defined by start point , end point,  direction and length 
        /// The last two parameters  help speed up calculations.
        member inline testPt.DistanceToLineSquare(fromPt:Pnt, toPt:Pnt,  uv:UnitVec, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vec.dot (uv,  dir) 
            if   dot <= 0.0 then testPt.DistanceToSquare fromPt 
            elif dot >= len then testPt.DistanceToSquare toPt 
            else                 testPt.DistanceToSquare (fromPt+dot*uv) 
                
        /// Distance between point and finite line segment defined by start point, direction and length.
        member inline testPt.DistanceToLine(fromPt:Pnt, uv:UnitVec, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vec.dot (uv,  dir) 
            if   dot <= 0.0 then testPt.DistanceToSquare fromPt 
            elif dot >= len then testPt.DistanceToSquare (fromPt+len*uv)  
            else                 testPt.DistanceToSquare (fromPt+dot*uv) 
                
        /// Distance between point and finite line segment  defined by start and end.
        member inline testPt.DistanceToLine(fromPt:Pnt, toPt:Pnt) =  
            let dir = testPt - fromPt 
            let v   = toPt   - fromPt
            let lenSq =  v.LengthSq 
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pnt.DistanceToLine: Line is too short for fromPt %O to  %O and  %O" fromPt toPt testPt
            let dot = Vec.dot (v,  dir) / v.LengthSq 
            if   dot <= 0.0 then testPt.DistanceTo   fromPt 
            elif dot >= 1.0 then testPt.DistanceTo   toPt  
            else                 testPt.DistanceTo   (fromPt + v * dot) 

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Same as Pnt.Origin
        static member Zero   = Pnt ( 0. , 0. , 0.)  // needed by 'Array.sum' 
        
        /// Same as Pnt.Zero
        static member Origin = Pnt ( 0. , 0. , 0.) 
        
        /// Divides the 3D point by an integer.
        /// (This member is needed by Array.average and similar functions)
        static member inline DivideByInt (pt:Pnt, i:int) = 
            if i<>0 then  let d = float i in  Pnt(pt.X/d, pt.Y/d, pt.Z/d)
            else FsExGeoDivByZeroException.Raise "FsEx.Geo.Pnt.DivideByInt 0 %O " pt  // needed by  'Array.average'

        /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofXYZ pt  = 
            let x = ( ^T : (member X : _) pt)
            let y = ( ^T : (member Y : _) pt)
            let z = ( ^T : (member Z : _) pt)
            try Pnt(float x, float y, float z) 
            with e -> FsExGeoException.Raise "FsEx.Geo.Pnt.ofXYZ: %A could not be converted to a FsEx.Geo.Pnt:\r\n%A" pt e

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxyz pt  = 
            let x = ( ^T : (member x : _) pt)
            let y = ( ^T : (member y : _) pt)
            let z = ( ^T : (member z : _) pt)
            try Pnt(float x, float y, float z) 
            with e -> FsExGeoException.Raise "FsEx.Geo.Pnt.ofxyz: %A could not be converted to a FsEx.Geo.Pnt:\r\n%A" pt e
        
        /// Create 3D point from 2D point. Using 0.0 for Z
        static member inline ofPt (p:Pt)  = Pnt (p.X, p.Y, 0.0) 
        
        /// Create 3D point from 3D vector.
        static member inline ofVec (v:Vec)  = Pnt (v.X, v.Y, v.Z)  
        
        /// Create 3D point from 3D unit vector.
        static member inline ofUnitVec  (v:UnitVec) = Pnt (v.X, v.Y, v.Z)                  
        
        /// Create 3D point from X, Y and Z components.
        static member inline create (x:float, y:float, z:float) =  Pnt( x , y , z ) 

        /// Returns a 3D point from Z level and 2D point.
        static member inline ofPtAt (z:float)  (p:Pt)  = Pnt (p.X, p.Y, z) 

        /// Project point to World X-Y plane.
        /// Use make2D to convert to 2D point instance
        static member inline projectToXYPlane (pt:Pnt) = Pnt(pt.X,pt.Y, 0.0)
                
        /// Sets the X value and return new 3D point.
        static member inline setX x (pt:Pnt) =  Pnt(x, pt.Y, pt.Z)
        
        /// Sets the Y value and return new 3D point.
        static member inline setY y (pt:Pnt) =  Pnt(pt.X, y, pt.Z)
        
        /// Sets the Z value and return new 3D point.
        static member inline setZ z (pt:Pnt) =  Pnt(pt.X, pt.Y, z)
        
        /// Gets the X value of 3D point.
        static member inline getX (pt:Pnt)  =  pt.X
        
        /// Gets the Y value of 3D point.
        static member inline getY (pt:Pnt) =  pt.Y
        
        /// Gets the Z value of 3D point.
        static member inline getZ (pt:Pnt) =  pt.Z

        /// Adds two 3D points and return new 3D point.
        static member inline add (a:Pnt) (b:Pnt) = a + b
        
        /// Add a 3D point to a 3D vector and return new 3D point.
        static member inline addVec (v:Vec) (a:Pnt)  = a + v
        
        /// Return the midpoint of two 3D points.
        static member inline midPt (a:Pnt)   (b:Pnt)         = (a+b) * 0.5
        
        /// Scale a 3D point by a scalar and return new 3D point.
        static member inline scale (f:float) (pt:Pnt) = pt*f
        
        /// Add float to X component of a 3D point and return new 3D point.
        static member inline shiftX (x:float) (pt:Pnt) = Pnt (pt.X+x, pt.Y,   pt.Z)
        
        /// Add float to Y component of a 3D point and return new 3D point.
        static member inline shiftY (y:float) (pt:Pnt) = Pnt (pt.X,   pt.Y+y, pt.Z)
        
        /// Add float to Z component of a 3D point and return new 3D point.
        static member inline shiftZ (z:float) (pt:Pnt) = Pnt (pt.X,   pt.Y,   pt.Z+z)    
        
        /// Returns the distance between two 3D points
        static member inline distance (a:Pnt) (b:Pnt) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
        
        /// Returns the horizontal distance between two 3D points(ignoring their Z Value)
        static member inline distanceXY (a:Pnt) (b:Pnt) = let x = a.X-b.X in let y=a.Y-b.Y in sqrt(x*x + y*y)
        
        /// Returns the squared distance between two 3D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        static member inline distanceSq (a:Pnt) (b:Pnt) = let v = a-b in  v.X*v.X + v.Y*v.Y + v.Z*v.Z

        /// Returns the distance from World Origin. 
        static member inline distanceFromOrigin (pt:Pnt) = pt.DistanceFromOrigin
        
        /// Returns the square distance from World Origin. 
        static member inline distanceFromOriginSquare (pt:Pnt) = pt.DistanceFromOriginSquare
        
        /// Returns a new 3D point at a given distance from World Origin by scaling the input.
        static member inline setDistanceFromOrigin f (pt:Pnt) = pt.WithDistanceFromOrigin f
    
        /// Returns angle between three 3D Points in Radians. Range 0.0 to Pi  
        static member inline anglePiPts (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt)  =   
            Vec.anglePi (ptPrev-ptThis) (ptNext-ptThis)

        /// Returns angle between three 3D Points in Degrees. Range 0.0 to 180 
        static member inline angle180Pts (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt)  =   
            Pnt.anglePiPts (ptPrev, ptThis, ptNext) |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction from ptThis. 
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized 
        /// ptPrev * ptThis * ptNext ->   bisector vector 
        static member inline bisector (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) =  
            (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized   
    
        /// For three Points describing a plane return a normal.
        /// If the returned vector has length zero then the points are in one Line.
        static member normalOf3Pts (a:Pnt, b:Pnt, c:Pnt) =  Vec.cross (a-b, c-b)    
    
        
        /// Returns a point that is at a given distance from a 3D point in the direction of another point.
        static member inline distPt (fromPt:Pnt, dirPt:Pnt, distance:float) : Pnt  = 
            let v = dirPt - fromPt
            let sc = distance/v.Length
            fromPt + v*sc       
        
        /// Returns a point by evaluation a line between two point with a normalized parameter.
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
        
        /// Every line has a normal vector in X-Y plane.
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
        /// The fist distance (distHor) is applied in in X-Y plane
        /// The second distance (distNormal) is applied perpendicular to the line (made by the two points) and perpendicular to the horizontal offset direction.
        /// this is in World.Z direction if both points are at the same Z level.
        /// If points are closer than than 1e-6 units the World.XAxis is used as first direction and World Z-axis as second direction.
        static member offsetTwoPt(  fromPt:Pnt,
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
        
    
        /// Multiplies the Matrix with a point (with an implicit 1 in the 4th dimension), 
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
        
        // Partially Multiplies the Matrix with a Point. 
        // Use this only for affine transformations that do NOT include a projection 
        // and if you need maximum performance
        // The fields m.M14, m.M24 and m.M34 must be 0.0 and m.M44 must be 1.0
        // Otherwise use Pnt.transform
        //static member transformSimple (m:Matrix) (p:Pnt) = 
        //    let x = p.X
        //    let y = p.Y
        //    let z = p.Z 
        //    Pnt(  m.M11*x + m.M21*y + m.M31*z + m.X41 
        //        , m.M12*x + m.M22*y + m.M32*z + m.Y42 
        //        , m.M13*x + m.M23*y + m.M33*z + m.Z43 
        //        ) 
            
        // Multiplies the Matrix with a point (with an implicit 1 in the 4th dimension)
        //static member inline ( * ) (matrix:Matrix, pt:Pnt) = Pnt.transform matrix pt //TODO in main declaration ,  not extension
           

        // Rotate 2D and 3D: 

        /// Rotate the 3D point around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateXBy (r:Rotation2D) (p:Pnt) = Pnt (p.X,  r.Cos*p.Y - r.Sin*p.Z, r.Sin*p.Y + r.Cos*p.Z)
        
        /// Rotate the 3D point around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateYBy (r:Rotation2D) (p:Pnt) = Pnt ( r.Sin*p.Z + r.Cos*p.X,  p.Y, r.Cos*p.Z - r.Sin*p.X) 
        
        /// Rotate the 3D point around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateZBy (r:Rotation2D) (p:Pnt) = Pnt (r.Cos*p.X - r.Sin*p.Y, r.Sin*p.X + r.Cos*p.Y,  p.Z)
        
        /// Rotate the 3D point around a center 3D point and a X aligned axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateXonCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) =  
            let x = pt.X - cen.X 
            let y = pt.Y - cen.Y 
            let z = pt.Z - cen.Z
            Pnt (x                 + cen.X,  
                 r.Cos*y - r.Sin*z + cen.Y, 
                 r.Sin*y + r.Cos*z + cen.Z)         

        /// Rotate the 3D point around a center point and a Y aligned axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateYonCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) =  
            let x = pt.X - cen.X 
            let y = pt.Y - cen.Y 
            let z = pt.Z - cen.Z
            Pnt ( r.Sin*z + r.Cos*x + cen.X, 
                  y                 + cen.Y, 
                  r.Cos*z - r.Sin*x + cen.Z) 
        
        /// Rotate the 3D point around a center point and a Z aligned axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateZonCenterBy (cen:Pnt) (r:Rotation2D) (pt:Pnt) =  
            let x = pt.X - cen.X  
            let y = pt.Y - cen.Y 
            let z = pt.Z - cen.Z
            Pnt (r.Cos*x - r.Sin*y + cen.X, 
                 r.Sin*x + r.Cos*y + cen.Y, 
                 z                 + cen.Z)
        
        /// Rotate the 3D point in Degrees around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (pt:Pnt) = 
            Pnt.rotateXBy (Rotation2D.createFromDegrees angDegree) pt
    
        /// Rotate the 3D point in Degrees around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (pt:Pnt) = 
            Pnt.rotateYBy  (Rotation2D.createFromDegrees angDegree) pt 
    
        /// Rotate the 3D point in Degrees around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (pt:Pnt) = 
            Pnt.rotateZBy  (Rotation2D.createFromDegrees angDegree) pt 
    
        /// Rotate the 3D point in Degrees around center point and a X aligned axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateXonCenter (cen:Pnt) (angDegree) (pt:Pnt) = 
            Pnt.rotateXonCenterBy cen (Rotation2D.createFromDegrees angDegree) pt  

        /// Rotate the 3D point in Degrees around center point and a Y aligned axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateYonCenter (cen:Pnt) (angDegree) (pt:Pnt) = 
            Pnt.rotateYonCenterBy cen (Rotation2D.createFromDegrees angDegree) pt 
    
        /// Rotate the 3D point in Degrees around center point and a Z aligned axis, from X to Y-axis, Counter Clockwise looking from top.
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
    
        /// Rotate by Quaternion around given Center point 
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

        /// Returns angle in Degrees at mid point (thisPt).
        static member angleInCorner(prevPt:Pnt, thisPt:Pnt, nextPt:Pnt) = 
            let a = prevPt-thisPt
            let b = nextPt-thisPt
            Vec.angle180 a b

        /// 'fromPt' point  and uv unit vector describe an endless line. 
        /// testPt gets projected on to this line.
        /// Returns the parameter (or scaling for unit vector) on this line of the projection
        static member inline projectedParameter (fromPt:Pnt, uv:UnitVec, testPt:Pnt) = 
            let dir = testPt-fromPt
            Vec.dot (dir,  uv) 
        
        /// 'fromPt' point  and 'v' vector describe an endless line.
        /// testPt gets projected on to this line.
        /// Returns the parameter (or scaling for vector) on this line of the projection
        static member inline projectedParameter (fromPt:Pnt, v:Vec, testPt:Pnt) = 
            let dir = testPt-fromPt
            let lenSq =  v.LengthSq
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Pnt.projectedParameter:  %O is too short for fromPt %O and  %O" v fromPt testPt
            Vec.dot (v,  dir) / v.LengthSq 
        
        /// 'fromPt' point  and 'toPt' point describe an endless line. 
        /// testPt gets projected on to this line.
        /// Returns the parameter (or scaling for vector) on this line of the projection
        static member inline projectedParameter (fromPt:Pnt, toPt:Pnt, testPt:Pnt) = 
            let dir = testPt - fromPt 
            let v   = toPt   - fromPt
            let lenSq =  v.LengthSq 
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "FsEx.Geo.Pnt.projectedParameter: Line is too short for fromPt %O to  %O and  %O" fromPt toPt testPt
            Vec.dot (v,  dir) / v.LengthSq
