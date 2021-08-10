namespace FsEx.Geo

open System


/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type UnitVec
[<AutoOpen>]
module AutoOpenUnitVec = 
    open Util

    /// Returns distance between the tips of two vectors
    let inline internal vecDist3(ax:float,ay:float,az:float,bx:float,by:float,bz:float) =
        let x = bx-ax
        let y = by-ay
        let z = bz-az
        sqrt(x*x+y*y+z*z)


    type UnitVec with 

        /// Returns the length of the 3D vector projected into world XY plane.
        member inline v.LengthInXY =  sqrt (v.X*v.X + v.Y*v.Y)

        /// Returns the squared length of the 3D vector projected into world XY plane.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSqInXY = v.X*v.X + v.Y*v.Y

        /// Returns  a new 3D Vector with new X coordinate, Y and Z  stay the same.
        member inline v.WithX x = Vec (x ,v.Y, v.Z) 
        
        /// Returns a new 3D Vector with new y coordinate, X and Z  stay the same.
        member inline v.WithY y = Vec (v.X, y, v.Z)
        
        /// Returns a new 3D Vector with new z coordinate, X and Y  stay the same.
        member inline v.WithZ z = Vec (v.X ,v.Y, z)

        /// Tests if dot product is bigger than 0.0.
        /// That means the angle between the two vectors is less than 90 degrees
        member inline v.MatchesOrientation (vv:UnitVec) = v*vv > 0. // direction match
        
        /// The diamond angle is always positive and in the range of 0.0 to 4.0 ( for 360 degrees) 
        /// 0.0 = XAxis,  going Counter clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        member inline v.DirDiamondInXY =
            // https://stackoverflow.com/a/14675998/969070            
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then FsExGeoDivByZeroException.Raise "UnitVec.DirDiamondInXY: input vector is vertical or zero length:%O" v
            #endif
            if v.Y >= 0.0 then 
                if v.X >= 0.0 then   
                    v.Y/(v.X+v.Y) 
                else             
                    1.0 - v.X/(-v.X+v.Y)
            else
                if v.X < 0.0 then   
                    2.0 - v.Y/(-v.X-v.Y) 
                else 
                    3.0 + v.X/(v.X-v.Y) 
        
        /// Returns the Angle in Radians from XAxis,  
        /// Going Counter clockwise till two Pi. Ignoring Z component.
        member inline v.Angle2PiInXY =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then FsExGeoDivByZeroException.Raise "UnitVec.Angle2PIInXY: input vector is vertical or zero length:%O" v
            #endif
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Util.twoPi
            else  
                a        
        
        /// Returns the Angle in Degrees from XAxis.  
        /// Going Counter clockwise till 360. Ignoring Z component.
        member inline v.Angle360InXY =
            v.Angle2PiInXY |> toDegrees
        
        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.
        /// Or Vec.Zero if input is vertical.
        /// Just does Vec(-v.Y, v.X, 0.0) 
        member inline v.PerpendicularInXY = Vec(-v.Y, v.X, 0) 

        /// Convert 3D unit vector to 3D Vector. 
        member inline v.AsVec = Vec(v.X, v.Y, v.Z)
        
        /// Convert 3D unit vector to 3D Point. 
        member inline v.AsPnt  = Pnt(v.X, v.Y, v.Z)

        /// Convert 3D unit vector to 2D Vector, discarding the Z value. 
        member inline v.AsVc  = Vc(v.X, v.Y)      

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Returns the world X-axis with length one: UnitVec(1,0,0)
        static member inline XAxis  = UnitVec.createUnchecked (1.0 , 0.0, 0.0)
        
        /// Returns the world Y-axis with length one: UnitVec(0.1,0)
        static member inline YAxis  = UnitVec.createUnchecked (0.0 , 1.0, 0.0)
        
        /// Returns the world Z-axis with length one: UnitVec(0,0,1)
        static member inline ZAxis  = UnitVec.createUnchecked (0.0 , 0.0, 1.0)
        
        // These members cannot be implemented since 
        // Array.sum and Array.average of UnitVec would return a 'Vec' and not a 'UnitVec' 
        // static member Zero = UnitVec ( 0 , 0, 0)  // needed by 'Array.sum' 
        // static member inline DivideByInt (v:UnitVec, i:int) = v / float i  // needed by  'Array.average' 

        /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float. 
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofXYZ vec  = 
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            let z = ( ^T : (member Z : _) vec)
            try UnitVec.create(float x, float y, float z) 
            with e -> FsExGeoDivByZeroException.Raise "UnitVec.ofXYZ: %A could not be converted to a FsEx.Geo.UnitVec:\r\n%A" vec e

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float. 
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxyz vec  = 
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            let z = ( ^T : (member z : _) vec)
            try UnitVec.create(float x, float y, float z) 
            with e -> FsExGeoDivByZeroException.Raise "UnitVec.ofxyz: %A could not be converted to a FsEx.Geo.UnitVec:\r\n%A" vec e

        /// Create 3D unit vector from 3D point. Does the unitizing too.
        static member inline ofPnt  (pt:Pnt) =  
            let l = sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z) 
            if l <  zeroLengthTol then FsExGeoDivByZeroException.Raise "UnitVec.ofPnt failed on too short %O" pt
            let li = 1. / l
            UnitVec.createUnchecked( li*pt.X , li*pt.Y , li*pt.Z ) 
        
        /// Create 3D unit vector from 3D vector. Does the unitizing too.
        static member inline ofVec (v:Vec) = 
            let l = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z) 
            if l <  zeroLengthTol then FsExGeoDivByZeroException.Raise "UnitVec.ofVec failed on too short %O" v
            let li = 1. / l
            UnitVec.createUnchecked( li*v.X , li*v.Y , li*v.Z )       
        
        /// Convert 3D unit vector to 2D point by ignoring Z value. 
        static member inline asPt(v:UnitVec)  = Pt( v.X, v.Y)

        /// Convert 3D unit vector to 2D Vector by ignoring Z value. 
        static member inline asVc(v:UnitVec) = Vc(v.X, v.Y)
        
        /// Convert 3D unit vector to 2D Unit Vector by ignoring Z value and unitizing again.
        static member inline asUnitVc(v:UnitVec) = UnitVc.create(v.X, v.Y)
        
        /// Convert 3D unit vector to 3D point. 
        static member inline asPnt(v:UnitVec) = Pnt(v.X, v.Y, v.Z) 
        
        /// Convert 3D unit vector to 3D vector. 
        static member inline asVec(v:UnitVec) = Vec(v.X, v.Y, v.Z) 


        /// Cross product, of two 3D unit vectors. 
        /// The resulting vector is perpendicular to both input vectors.
        /// Its length is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows th right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:UnitVec, b:UnitVec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X )       
        
        /// Cross product, of a 3D unit vectors an a 3D vector. 
        /// The resulting vector is perpendicular to both input vectors.
        /// Its length is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows th right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:UnitVec, b:Vec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) 
        
        /// Cross product, of a 3D vector and a 3D unit vectors. 
        /// The resulting vector is perpendicular to both input vectors.
        /// Its length is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows th right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:Vec, b:UnitVec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) 

        /// Dot product, or scalar product of two 3D unit vectors. 
        /// Returns a float. This float is the cosine of the angle between the two vectors.
        static member inline dot  (a:UnitVec, b:UnitVec)   = a.X * b.X + a.Y * b.Y + a.Z * b.Z
        
        /// Dot product, or scalar product of a 3D unit vector with a 3D vector  
        /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit vector
        static member inline dot  (a:UnitVec, b:Vec ) = a.X * b.X + a.Y * b.Y + a.Z * b.Z
        
        /// Dot product, or scalar product of a 3D vector with a 3D unit vector  
        /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit vector
        static member inline dot  (a:Vec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Gets the X part of this 3D unit vector
        static member inline getX  (v:UnitVec) = v.X
        
        /// Gets the Y part of this 3D unit vector
        static member inline getY (v:UnitVec) = v.Y
        
        /// Gets the Z part of this 3D unit vector
        static member inline getZ  (v:UnitVec) = v.Z
        
        /// Returns new 3D Vector with new X value, Y  and Z stay the same.
        static member inline setX  x (v:UnitVec) = v.WithX x
        
        /// Returns new 3D Vector with new Y value, X  and Z stay the same.
        static member inline setY  y (v:UnitVec) = v.WithY y
        
        /// Returns new 3D Vector with new z value, X  and Y stay the same.
        static member inline setZ z (v:UnitVec) = v.WithZ z
        
        /// Add two 3D unit vectors together. Returns a new (non-unitized) 3D vector.
        static member inline add      (a:UnitVec) (b:UnitVec) = b + a  
        
        /// Tests if dot product is bigger than 0.0.
        /// That means the angle between the two vectors is less than 90 degrees.
        static member inline dirMatch (a:UnitVec) (b:UnitVec) = b.MatchesOrientation a
        
        /// Multiplies a 3D unit vector with a scalar, also called scaling a vector. 
        /// Same as UnitVec.setLength. Returns a new (non-unitized) 3D vector.
        static member inline scale    (f:float) (v:UnitVec) = Vec (v.X * f , v.Y * f , v.Z * f)    

        /// Multiplies a 3D unit vector with a scalar, also called scaling a vector. 
        /// Same as UnitVec.scale. Returns a new (non-unitized) 3D vector.
        static member inline setLength(f:float) (v:UnitVec) = Vec (v.X * f , v.Y * f , v.Z * f) 
        
        /// Add to the X part of this 3D unit vectors together. Returns a new (non-unitized) 3D vector.
        static member inline shiftX x (v:UnitVec) = Vec (v.X+x, v.Y,   v.Z)
        
        /// Add to the Y part of this 3D unit vectors together. Returns a new (non-unitized) 3D vector.
        static member inline shiftY y (v:UnitVec) = Vec (v.X,   v.Y+y, v.Z)
        
        /// Add to the Z part of this 3D unit vectors together. Returns a new (non-unitized) 3D vector.
        static member inline shiftZ z (v:UnitVec) = Vec (v.X,   v.Y,   v.Z+z)
            
        /// Project vector to World XY Plane.
        /// Use Vc.ofUnitVec to convert to 2D vector instance
        static member inline projectToXYPlane (v:UnitVec) = Vec(v.X,v.Y, 0.0)
                    

        /// Negate or inverse a 3D unit vectors. Returns a new 3D unit vector. 
        /// Same as UnitVec.flip
        static member inline reverse  (v:UnitVec) = -v   
        
        /// Negate or inverse a 3D unit vectors. Returns a new 3D unit vector. 
        /// Same as UnitVec.reverse
        static member inline flip  (v:UnitVec) = -v

        /// Flips the vector if Z part is smaller than 0.0
        static member inline flipToPointUp (v:UnitVec) = if v.Z < 0.0 then -v else v 


        /// Returns three vectors Determinant
        /// This is also the signed volume of the Parallelepipeds define by these three vectors.
        /// Also called scalar triple product, mixed product, box product, or in german: Spatprodukt.
        /// It is defined as the dot product of one of the vectors with the cross product of the other two.
        static member inline determinant (u:UnitVec, v:UnitVec, w:UnitVec) = u.X*v.Y*w.Z + v.X*w.Y*u.Z + w.X*u.Y*v.Z - w.X*v.Y*u.Z - v.X*u.Y*w.Z - u.X*w.Y*v.Z 

        /// Returns angle between two UnitVectors in Radians.
        /// Takes vector orientation into account. 
        /// Range 0.0 to PI( = 0 to 180 degree)
        static member inline anglePI (a:UnitVec) (b:UnitVec) = 
            // The "straight forward" method of acos(u.v) has large precision
            // issues when the dot product is near +/-1.  This is due to the
            // steep slope of the acos function as we approach +/- 1.  Slight
            // precision errors in the dot product calculation cause large
            // variation in the output value.
            // To avoid this we use an alternative method which finds the
            // angle bisector by (u-v)/2
            // Because u and v and unit vectors, (u-v)/2 forms a right angle
            // with the angle bisector.  The hypotenuse is 1, therefore
            // 2*asin(|u-v|/2) gives us the angle between u and v.
            // The largest possible value of |u-v| occurs with perpendicular
            // vectors and is sqrt(2)/2 which is well away from extreme slope
            // at +/-1. (See Windows OS Bug 01706299 for details) (form WPF reference source code)
            let dot = a * b
            if -0.98 < dot && dot < 0.98 then // threshold for switching 0.98 ? 
                acos dot
            else
                if dot < 0. then Math.PI - 2.0 * asin(vecDist3(-a.X,-a.Y,-a.Z,b.X,b.Y,b.Z) * 0.5) 
                else                       2.0 * asin(vecDist3( a.X, a.Y, a.Z,b.X,b.Y,b.Z) * 0.5)            

        /// Returns positive angle between two UnitVectors in Radians. 
        /// Ignores orientation. 
        /// Range 0.0 to PI/2 ( = 0 to 90 degree)
        static member inline angleHalfPI (a:UnitVec) (b:UnitVec) = 
            let dot =  a * b
            let dotAbs = abs dot
            if dotAbs < 0.98 then  
                acos dotAbs 
            else
                if dot < 0. then 2.0 * asin(vecDist3(-a.X,-a.Y,-a.Z,b.X,b.Y,b.Z) * 0.5)
                else             2.0 * asin(vecDist3( a.X, a.Y, a.Z,b.X,b.Y,b.Z) * 0.5) 
        
        /// Returns positive angle from Vector 'a' to Vector 'b' projected in XY Plane.
        /// In Radians
        /// Considering counter clockwise rotation round the World ZAxis
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline angle2PiInXY (a:UnitVec, b:UnitVec)   =
            let r = b.Angle2PIInXY  - a.Angle2PIInXY            
            if r >= 0. then  r
            else r + Util.twoPi 

        /// Returns positive angle of two Vector projected in XY Plane in Degrees
        /// Considering positive rotation round the World ZAxis
        /// Range:  0 to 360 degrees
        static member inline angle360InXY (a:UnitVec, b:UnitVec)   = 
            UnitVec.angle2PiInXY (a, b) |> toDegrees
        
        /// Returns positive angle between two UnitVectors in Degrees. 
        /// Takes vector orientation into account.
        /// Range 0 to 180 degrees.
        static member inline angle180 (a:UnitVec) (b:UnitVec) = 
            UnitVec.anglePI a b |>  toDegrees 

        /// Returns positive angle between two UnitVectors in Degrees,
        /// Ignores vector orientation.
        /// Range: 0 to 90 degrees.
        static member inline angle90 (a:UnitVec) (b:UnitVec) = 
            UnitVec.angleHalfPI a b |>  toDegrees 
        

        /// Ensure vector has a positive dot product with given orientation vector
        static member inline matchOrientation (orientationToMatch:UnitVec) (v:UnitVec) = 
            if orientationToMatch * v < 0.0 then -v else v

        /// Check if vector has a positive dot product with given orientation vector
        static member inline doesOrientationMatch (orientationToCheck:UnitVec) (v:UnitVec) = 
            orientationToCheck * v > 0.0   

        // Rotate2D: 

        /// Rotate the 3D UnitVector around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member rotateXBy (r:Rotation2D) (v:UnitVec) = UnitVec.createUnchecked (v.X,  r.Cos*v.Y - r.Sin*v.Z, r.Sin*v.Y + r.Cos*v.Z)
        
        /// Rotate the 3D UnitVector around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member rotateYBy (r:Rotation2D) (v:UnitVec) = UnitVec.createUnchecked ( r.Sin*v.Z + r.Cos*v.X,  v.Y, r.Cos*v.Z - r.Sin*v.X) 
        
        /// Rotate the 3D UnitVector around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member rotateZBy (r:Rotation2D) (v:UnitVec) = UnitVec.createUnchecked (r.Cos*v.X - r.Sin*v.Y, r.Sin*v.X + r.Cos*v.Y,  v.Z)
                
        /// Rotate the 3D UnitVector in Degrees around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (v:UnitVec) = 
            UnitVec.rotateXBy (Rotation2D.createFromDegrees angDegree) v
    
        /// Rotate the 3D UnitVector in Degrees around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (v:UnitVec) = 
            UnitVec.rotateYBy  (Rotation2D.createFromDegrees angDegree) v 
    
        /// Rotate the 3D UnitVector in Degrees around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (v:UnitVec) = 
            UnitVec.rotateZBy  (Rotation2D.createFromDegrees angDegree) v 

        /// Rotate by Quaternion
        static member inline rotateByQuaternion  (q:Quaternion) (v:UnitVec) =
            // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
            let x = v.X
            let y = v.Y
            let z = v.Z
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
            UnitVec.createUnchecked( ix * qw + iw * - qx + iy * - qz - iz * - qy
                                   , iy * qw + iw * - qy + iz * - qx - ix * - qz
                                   , iz * qw + iw * - qz + ix * - qy - iy * - qx
                                   )
        
        
        
        /// Vector length projected into X Y Plane
        /// sqrt( v.X * v.X  + v.Y * v.Y)
        static member inline lengthInXY(v:UnitVec) = sqrt(v.X * v.X  + v.Y * v.Y)

        /// Checks if a vector is vertical by doing:
        /// abs(v.X) + abs(v.Y) < zeroLengthTol
        static member inline isVertical (v:UnitVec) =             
            abs(v.X) + abs(v.Y) < zeroLengthTol

        /// Checks if a vector is horizontal  by doing:
        /// abs(v.Z) < zeroLengthTol
        static member inline isHorizontal (v:UnitVec) =             
            abs(v.Z) < zeroLengthTol     

        /// Returns positive or negative slope of a vector in Radians
        /// in relation to XY Plane
        static member inline slopeRadians (v:UnitVec) = 
            v.Y |> acosSafe          

        /// Returns positive or negative slope of a vector in Degrees
        /// in relation to XY Plane
        static member inline slopeDegrees (v:UnitVec) = 
            UnitVec.slopeRadians v |> toDegrees

        /// Returns positive or negative slope of a vector in Percent
        /// in relation to XY Plane
        /// 100% = 45 degrees
        static member inline slopePercent (v:UnitVec) = 
            if abs(v.Z) < zeroLengthTol then FsExGeoDivByZeroException.Raise "UnitVec.slopePercent: Can't get Slope from vertical vector %O" v
            let l = UnitVec.lengthInXY v
            100.0 * (v.Z/l)

        /// Reverse vector if Z part is smaller than 0.0
        static member inline orientUp (v:UnitVec) = 
            if v.Z < 0.0 then -v else v

        /// Reverse vector if Z part is bigger than 0.0
        static member inline orientDown (v:UnitVec) = 
            if v.Z < 0.0 then v else -v
     

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.        
        /// Just does Vec(-v.Y, v.X, 0.0) 
        /// On vertical input vector resulting vector if of zero length. 
        static member inline perpendicularInXY (v:UnitVec) :Vec = 
            Vec(-v.Y, v.X, 0.0) 

        /// Returns a vector that is perpendicular to the given vector and in the same vertical Plane.        
        /// Projected into the XY Plane input and output vectors are parallel and of same orientation.
        /// Not of same length, not unitized.
        /// On vertical input vector resulting vector if of zero length. 
        static member inline perpendicularInVerticalPlane (v:UnitVec) :Vec = 
            let hor = Vec(v.Y, -v.X, 0.0)
            let r = UnitVec.cross (v, hor)            
            if v.Z < 0.0 then -r else r


        /// Multiplies the Matrix with a Vector (with an implicit 1 in the 4th dimension), 
        /// So that it also works correctly for projections
        /// See also Pnt.transformSimple for better performance
        static member transform (m:Matrix) (p:UnitVec) = 
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
            Vec(x' * sc, y'* sc, z'* sc)     
        
        // Partially Multiplies the Matrix with a Vector. 
        // Use this only for affine transformations that do NOT include a projection 
        // and if you need maximum performance.
        // The fields m.M14, m.M24 and m.M34 must be 0.0 and m.M44 must be 1.0
        // Otherwise use Vec.transform
        //static member transformSimple (m:Matrix) (p:Vec) = 
        //    let x = p.X
        //    let y = p.Y
        //    let z = p.Z 
        //    Vec(  m.M11*x + m.M21*y + m.M31*z + m.X41 
        //        , m.M12*x + m.M22*y + m.M32*z + m.Y42 
        //        , m.M13*x + m.M23*y + m.M33*z + m.Z43 
        //        )

