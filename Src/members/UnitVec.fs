namespace FsEx.Geo

open System


/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type UnitVec
[<AutoOpen>]
module AutoOpenUnitVec = 
    open Util

    /// Returns distance between the tips of two 3D unit vectors
    let inline private vecDist3(ax:float,ay:float,az:float,bx:float,by:float,bz:float) =
        let x = bx-ax
        let y = by-ay
        let z = bz-az
        sqrt(x*x+y*y+z*z)


    type UnitVec with 
        
        /// Returns the length of the 3D vector projected into World X-Y plane.
        member inline v.LengthInXY = sqrt (v.X*v.X + v.Y*v.Y)

        /// Returns the squared length of the 3D vector projected into World X-Y plane.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSqInXY = v.X*v.X + v.Y*v.Y

        /// Returns  a new 3D vector with new X coordinate, Y and Z  stay the same.
        member inline v.WithX x = Vec (x ,v.Y, v.Z) 
        
        /// Returns a new 3D vector with new y coordinate, X and Z  stay the same.
        member inline v.WithY y = Vec (v.X, y, v.Z)
        
        /// Returns a new 3D vector with new z coordinate, X and Y  stay the same.
        member inline v.WithZ z = Vec (v.X ,v.Y, z)
        
        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.
        /// Or Vec.Zero if input is vertical.
        /// Just does Vec(-v.Y, v.X, 0.0) 
        member inline v.PerpendicularInXY = Vec(-v.Y, v.X, 0) 

        /// 90 Degree rotation counter clockwise around Z-axis.
        member inline v.RotateOnZ90CCW = UnitVec.createUnchecked( -v.Y,   v.X ,   v.Z)

        /// 90 Degree rotation clockwise around Z-axis.
        member inline v.RotateOnZ90CW  = UnitVec.createUnchecked(  v.Y,  -v.X,   v.Z  )  

        /// The diamond angle is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees) 
        /// 0.0 = Xaxis,  going Counter clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is useful for radial sorting.
        /// For X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionDiamondInXY =
            // https://stackoverflow.com/a/14675998/969070            
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVec.DirectionDiamondInXY: input vector is vertical or zero length:%O" v
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
        

        /// Returns the Angle in Radians from Xaxis,  
        /// Going Counter clockwise till two Pi.         
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction2PiInXY =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVec.Direction2PiInXY: input vector is zero length: %O" v
            #endif
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Util.twoPi
            else  
                a
        
        /// Returns the Angle in Radians from Xaxis in World  X-Y plane, 
        /// Ignores orientation.
        /// Range 0.0 to Pi. 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionPiInXY =
            // https://stackoverflow.com/a/14675998/969070            
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.UnitVec.DirectionPiInXY: input vector is zero length: %O" v
            #endif
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Math.PI
            else  
                a
        
        /// Returns the Angle in Degrees from Xaxis in World  X-Y plane.  
        /// Going Counter clockwise till 360. 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction360InXY =
            v.Direction2PiInXY |> toDegrees
        
        /// Returns the Angle in Radians from Xaxis, 
        /// Ignores orientation.
        /// Range 0.0 to 180. 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction180InXY =
            v.DirectionPiInXY |> toDegrees
        
        /// Returns positive angle for rotating counter clockwise from this vector to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 Degrees) 
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions. 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.AngleDiamondInXYTo (b:UnitVec)   =              
            let r = b.DirectionDiamondInXY - v.DirectionDiamondInXY          
            if r >= 0. then  r
            else r + 4.0   

        /// Convert 3D unit vector to 3D Vector. 
        member inline v.AsVec = Vec(v.X, v.Y, v.Z)
        
        /// Convert 3D unit vector to 3D Point. 
        member inline v.AsPnt  = Pnt(v.X, v.Y, v.Z)

        /// Convert 3D unit vector to 2D Vector, discarding the Z value. 
        member inline v.AsVc  = Vc(v.X, v.Y)      

        
        /// Checks if the angle between the two 3D unit vectors is less than 180 degrees.
        /// Calculates the dot product of two 3D unit vectors. 
        /// Then checks if it is positive.
        member inline v.MatchesOrientation180  (other:UnitVec) = 
            v * other > 0.0  

        /// Checks if the angle between the two 3D unit vectors is less than 90 degrees.   
        /// Calculates the dot product of the two 3D unit vectors unitized. 
        /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
        member inline v.MatchesOrientation90  (other:UnitVec) = 
            v * other > 0.707107

        /// Checks if two 3D unit vectors are parallel.
        /// Ignores the line orientation.
        /// The default angle tolerance is 0.25 degrees.  
        /// This tolerance can be customized by an optional minium cosine value.
        /// See FsEx.Geo.Cosine module.
        member inline a.IsParallelTo( b:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine ) = 
            abs(b*a) > minCosine // 0.999990480720734 = cosine of 0.25 degrees:            
            
            
        /// Checks if two 3D unit vectors are parallel.
        /// Takes the line orientation into account too.
        /// The default angle tolerance is 0.25 degrees.  
        /// This tolerance can be customized by an optional minium cosine value.
        /// See FsEx.Geo.Cosine module.  
        member inline a.IsParallelAndOrientedTo  (b:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine ) = 
            b*a >  minCosine // 0.999990480720734 = cosine of 0.25 degrees:    
            
        
        /// Checks if two 3D unit vectors are perpendicular to each other.
        /// The default angle tolerance is 89.75 to  90.25 degrees.   
        /// This tolerance can be customized by an optional minium cosine value.
        /// The default cosine is 0.0043633 ( = 89.75 deg )
        /// See FsEx.Geo.Cosine module. 
        member inline a.IsPerpendicularTo (b:UnitVec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine ) =             
            let d = b*a            
            -maxCosine < d && d  < maxCosine // = cosine of 98.75 and 90.25 degrees 



        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //---------------------------------------------------------------------------------------------- 

        /// Create 3D unit vector from start and endpoint. Does the unitizing too.
        static member inline create (fromPnt:Pnt, toPnt:Pnt) = 
            let x = toPnt.X - fromPnt.X
            let y = toPnt.Y - fromPnt.Y
            let z = toPnt.Z - fromPnt.Z
            UnitVec.create( x , y , z )

        /// Returns the World X-axis with length one: UnitVec(1,0,0)
        static member inline Xaxis  = UnitVec.createUnchecked (1.0 , 0.0, 0.0)
        
        /// Returns the World Y-axis with length one: UnitVec(0.1,0)
        static member inline Yaxis  = UnitVec.createUnchecked (0.0 , 1.0, 0.0)
        
        /// Returns the World Z-axis with length one: UnitVec(0,0,1)
        static member inline Zaxis  = UnitVec.createUnchecked (0.0 , 0.0, 1.0)
        
        /// Returns the distance between the tips of two 3D unit vectors.
        static member inline difference (a:UnitVec) (b:UnitVec) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
                
        /// Returns the squared distance between the tips of two 3D unit vectors.
        /// This operation is slightly faster than Vec.difference  and sufficient for many algorithms like finding closest points.
        static member inline differenceSq (a:UnitVec) (b:UnitVec) = let v = a-b in  v.X*v.X + v.Y*v.Y + v.Z*v.Z

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
            with e -> FsExGeoException.Raise "UnitVec.ofXYZ: %A could not be converted to a FsEx.Geo.UnitVec:\r\n%A" vec e

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float. 
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxyz vec  = 
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            let z = ( ^T : (member z : _) vec)
            try UnitVec.create(float x, float y, float z) 
            with e -> FsExGeoException.Raise "UnitVec.ofxyz: %A could not be converted to a FsEx.Geo.UnitVec:\r\n%A" vec e

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

        /// Convert 3D unit vector to 2D vector by ignoring Z value. 
        static member inline asVc(v:UnitVec) = Vc(v.X, v.Y)
        
        /// Convert 3D unit vector to 2D Unit vector by ignoring Z value and unitizing again.
        static member inline asUnitVc(v:UnitVec) = UnitVc.create(v.X, v.Y)
        
        /// Convert 3D unit vector to 3D point. 
        static member inline asPnt(v:UnitVec) = Pnt(v.X, v.Y, v.Z) 
        
        /// Convert 3D unit vector to 3D vector. 
        static member inline asVec(v:UnitVec) = Vec(v.X, v.Y, v.Z) 
        
        //static member inline cross (a:UnitVec, b:UnitVec)  moved to Vec type declaration
        
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

        //static member inline dot  (a:UnitVec, b:UnitVec)  //moved to Vec type declaration
        
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
        
        /// Returns new 3D vector with new X value, Y  and Z stay the same.
        static member inline setX  x (v:UnitVec) = v.WithX x
        
        /// Returns new 3D vector with new Y value, X  and Z stay the same.
        static member inline setY  y (v:UnitVec) = v.WithY y
        
        /// Returns new 3D vector with new z value, X  and Y stay the same.
        static member inline setZ z (v:UnitVec) = v.WithZ z
        
        /// Add two 3D unit vectors together. Returns a new (non-unitized) 3D vector.
        static member inline add      (a:UnitVec) (b:UnitVec) = b + a  
        
        /// Multiplies a 3D unit vector with a scalar, also called scaling a vector. 
        /// Same as UnitVec.setLength. Returns a new (non-unitized) 3D vector.
        static member inline scale    (f:float) (v:UnitVec) = Vec (v.X * f , v.Y * f , v.Z * f)    

        /// Multiplies a 3D unit vector with a scalar, also called scaling a vector. 
        /// Same as UnitVec.scale. Returns a new (non-unitized) 3D vector.
        static member inline setLength(f:float) (v:UnitVec) = Vec (v.X * f , v.Y * f , v.Z * f) 
        
        /// Add to the X part of this 3D unit vectors together. Returns a new (non-unitized) 3D vector.
        static member inline moveX x (v:UnitVec) = Vec (v.X+x, v.Y,   v.Z)
        
        /// Add to the Y part of this 3D unit vectors together. Returns a new (non-unitized) 3D vector.
        static member inline moveY y (v:UnitVec) = Vec (v.X,   v.Y+y, v.Z)
        
        /// Add to the Z part of this 3D unit vectors together. Returns a new (non-unitized) 3D vector.
        static member inline moveZ z (v:UnitVec) = Vec (v.X,   v.Y,   v.Z+z)
            
        /// Project vector to World X-Y plane.
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

        /// Returns angle between two 3D unit vectors in Radians.
        /// Takes vector orientation into account. 
        /// Range 0.0 to Pi( = 0 to 180 Degree)
        static member inline anglePi (a:UnitVec) (b:UnitVec) = 
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

        /// Returns positive angle between two 3D unit vectors in Radians. 
        /// Ignores orientation. 
        /// Range 0.0 to Pi/2 ( = 0 to 90 Degree)
        static member inline angleHalfPi (a:UnitVec) (b:UnitVec) = 
            let dot =  a * b
            let dotAbs = abs dot
            if dotAbs < 0.98 then  
                acos dotAbs 
            else
                if dot < 0. then 2.0 * asin(vecDist3(-a.X,-a.Y,-a.Z,b.X,b.Y,b.Z) * 0.5)
                else             2.0 * asin(vecDist3( a.X, a.Y, a.Z,b.X,b.Y,b.Z) * 0.5) 
        
        /// Returns positive angle from vector 'a' to vector 'b' projected in X-Y plane.
        /// In Radians
        /// Considering counter clockwise rotation round the World Z-axis
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline angle2PiInXY (a:UnitVec, b:UnitVec)   =
            let r = b.Direction2PiInXY  - a.Direction2PiInXY            
            if r >= 0. then  r
            else r + Util.twoPi 

        /// Returns positive angle between two 3D unit vectors in Degrees,
        /// Ignores vector orientation.
        /// Range: 0 to 90 Degrees.
        static member inline angle90 (a:UnitVec) (b:UnitVec) = 
            UnitVec.angleHalfPi a b |>  toDegrees 
        
        /// Returns positive angle between two 3D unit vectors in Degrees. 
        /// Takes vector orientation into account.
        /// Range 0 to 180 Degrees.
        static member inline angle180 (a:UnitVec) (b:UnitVec) = 
            UnitVec.anglePi a b |>  toDegrees 

        /// Returns positive angle of two 3D unit vector projected in X-Y plane in Degrees
        /// Considering positive rotation round the World Z-axis
        /// Range:  0 to 360 Degrees
        static member inline angle360InXY (a:UnitVec, b:UnitVec)   = 
            UnitVec.angle2PiInXY (a, b) |> toDegrees

        
        /// Returns positive angle for rotating counter clockwise from vector 'a' to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 Degrees) 
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline angleDiamondInXY (a:UnitVec , b:UnitVec)   = a.AngleDiamondInXYTo(b)             
        
        /// The diamond angle.
        /// Returns positive angle of 3D unit vector in World  X-Y plane.
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees) 
        /// 0.0 = Xaxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline directionDiamondInXY(v:UnitVec) = v.DirectionDiamondInXY
        
        /// Returns positive angle of 3D unit vector in World  X-Y plane. Counter clockwise from X-axis.
        /// In Radians
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees) 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction2PiInXY (v:UnitVec)   = v.Direction2PiInXY

        /// Returns positive angle of 3D unit vector in World  X-Y plane. Counter clockwise from X-axis.
        /// In Degree
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees) 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction360InXY (v:UnitVec)  = v.Direction360InXY        

    
        /// Ensure vector has a positive dot product with given orientation vector.
        static member inline matchOrientation (orientationToMatch:UnitVec) (v:UnitVec) = 
            if orientationToMatch * v < 0.0 then -v else v        

        /// Checks if the angle between the two 3D unit vectors is less than 180 degrees.
        /// Calculates the dot product of two 3D unit vectors. 
        /// Then checks if it is positive.
        static member inline matchesOrientation180 (other:UnitVec) (v:UnitVec) = v.MatchesOrientation180 other             

        /// Checks if the angle between the two 3D unit vectors is less than 90 degrees.   
        /// Calculates the dot product of the two 3D unit vectors unitized. 
        static member inline matchesOrientation90 (other:UnitVec) (v:UnitVec) = v.MatchesOrientation90 other           
            
        /// Checks if Angle between two vectors is Below 0.25 Degree.
        /// Ignores vector orientation.
        /// Same as isAngleBelowQuatreDegree
        static member inline isParallelTo (other:UnitVec) (v:UnitVec) =   v.IsParallelTo other

        
        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        static member inline isParallelAndOrientedTo (other:UnitVec) (v:UnitVec) = v.IsParallelAndOrientedTo other
        
        /// Checks if Angle between two vectors is between 98.75 and 90.25 Degree.
        /// Ignores vector orientation.
        static member inline isPerpendicularTo (other:UnitVec) (v:UnitVec) =  v.IsPerpendicularTo other
        

        // Rotate2D: 

        /// 90 Degree rotation counter clockwise around Z-axis.
        static member inline rotateOnZ90CCW(v:UnitVec) = UnitVec.createUnchecked( -v.Y,   v.X ,   v.Z)

        /// 90 Degree rotation clockwise around Z-axis.
        static member inline rotateOnZ90CW(v:UnitVec)  = UnitVec.createUnchecked(  v.Y,  -v.X,   v.Z  )  

        /// Rotate the 3D UnitVector around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateXBy (r:Rotation2D) (v:UnitVec) =
            UnitVec.createUnchecked (v.X,  r.Cos*v.Y - r.Sin*v.Z, r.Sin*v.Y + r.Cos*v.Z)
        
        /// Rotate the 3D UnitVector around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateYBy (r:Rotation2D) (v:UnitVec) = 
            UnitVec.createUnchecked ( r.Sin*v.Z + r.Cos*v.X,  v.Y, r.Cos*v.Z - r.Sin*v.X) 
        
        /// Rotate the 3D UnitVector around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateZBy (r:Rotation2D) (v:UnitVec) = 
            UnitVec.createUnchecked (r.Cos*v.X - r.Sin*v.Y, r.Sin*v.X + r.Cos*v.Y,  v.Z)
                
        /// Rotate the 3D UnitVector in Degrees around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (v:UnitVec) = 
            UnitVec.rotateXBy (Rotation2D.createFromDegrees angDegree) v
    
        /// Rotate the 3D UnitVector in Degrees around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (v:UnitVec) = 
            UnitVec.rotateYBy  (Rotation2D.createFromDegrees angDegree) v 
    
        /// Rotate the 3D UnitVector in Degrees around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (v:UnitVec) = 
            UnitVec.rotateZBy  (Rotation2D.createFromDegrees angDegree) v 

        /// Rotate by Quaternion
        static member inline rotateByQuaternion  (q:Quaternion) (v:UnitVec) =
            v*q  // operator * is defined in Quaternion.fs       
        
        
        /// vector length projected into X Y Plane
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
        /// in relation to X-Y plane
        static member inline slopeRadians (v:UnitVec) = 
            v.Y |> acosSafe          

        /// Returns positive or negative slope of a vector in Degrees
        /// in relation to X-Y plane
        static member inline slopeDegrees (v:UnitVec) = 
            UnitVec.slopeRadians v |> toDegrees

        /// Returns positive or negative slope of a vector in Percent
        /// in relation to X-Y plane
        /// 100% = 45 Degrees
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
        /// Projected into the X-Y plane input and output vectors are parallel and of same orientation.
        /// Not of same length, not unitized.
        /// On vertical input vector resulting vector if of zero length. 
        static member inline perpendicularInVerticalPlane (v:UnitVec) :Vec = 
            let hor = Vec(v.Y, -v.X, 0.0)
            let r = UnitVec.cross (v, hor)            
            if v.Z < 0.0 then -r else r


        /// Multiplies a Matrix with a 3D vector (with an implicit 1 in the 4th dimension, 
        /// So that it also works correctly for projections.)
        /// The resulting vector is not unitized.
        static member transform (m:Matrix) (v:UnitVec) = 
            v*m // operator * is defined in Matrix.fs
        
        /// Multiplies (or applies) an OrthoMatrix to a 3D Vector . 
        /// The resulting vector is not unitized if Matrix is translating too.
        static member transformOrtho (m:OrthoMatrix) (v:UnitVec) = 
            v*m // operator * is defined in OrthoMatrix.fs
        
        /// Multiplies (or applies) onl the 3x3 rotation part of an OrthoMatrix to a 3D Unit Vector . 
        /// The resulting vector is unitized too.
        static member rotateOrtho (m:OrthoMatrix) (v:UnitVec) = 
            let x = v.X
            let y = v.Y
            let z = v.Z 
            UnitVec.createUnchecked (
                      m.M11*x + m.M21*y + m.M31*z 
                    , m.M12*x + m.M22*y + m.M32*z 
                    , m.M13*x + m.M23*y + m.M33*z 
                    )
        
        /// Checks if Angle between two vectors is Below one Degree.
        /// Ignores vector orientation.
        /// USe Vec.isParallelTo for custom tolerance
        static member isAngleBelow1Degree(a:UnitVec, b:UnitVec) = 
            abs(b*a) > Cosine.``1.0``

            
        /// Checks if Angle between two vectors is Below 0.25 Degree.
        /// Ignores vector orientation.
        /// USe Vec.isParallelTo for custom tolerance
        static member isAngleBelowQuatreDegree(a:UnitVec, b:UnitVec) = 
            abs(b*a) > Cosine.``0.25``


        /// Checks if Angle between two vectors is Below 5 Degrees.
        /// Ignores vector orientation.
        /// USe Vec.isParallelTo for custom tolerance
        static member isAngleBelow5Degree(a:UnitVec, b:UnitVec) = 
            abs(b*a) > Cosine.``5.0``
