namespace FsEx.Geo

open System


/// Members and operators for 3D Points, Vectors and Rotations
[<AutoOpen>]
module AutoOpenUnitVec = 
    open Util

    /// Returns distance between the tips of two vectoers
    let inline internal vecDist3(ax:float,ay:float,az:float,bx:float,by:float,bz:float) =
         let x = bx-ax
         let y = by-ay
         let z = bz-az
         sqrt(x*x+y*y+z*z)


    type UnitVec with 
               
        member inline v.LengthInXY =  sqrt (v.X*v.X + v.Y*v.Y)

        member inline v.LengthSqInXY = v.X*v.X + v.Y*v.Y

        member inline v.WithX x = Vec (x ,v.Y, v.Z) // returns new Vector with new x coordinate, y and z the same as before
        member inline v.WithY y = Vec (v.X, y, v.Z)
        member inline v.WithZ z = Vec (v.X ,v.Y, z)

        /// Tests if dot product is bigger than 0.0
        member inline v.MatchesOrientation (vv:UnitVec) = v*vv > 0. // direction match
        
 
        /// The diamond angle is always positive and in the range of 0.0 to 4.0 ( for 360 degrees) 
        /// 0.0 = XAxis,  going Counter clockwise. Ignoring Z component.
        /// This is the fastest angle computation since it does not use Math.Cos or Math.Sin.
        /// It is usefull for radial sorting.
        member inline v.DirDiamondInXY =
            // https://stackoverflow.com/a/14675998/969070            
            #if DEBUG 
            if abs(v.X) < zeroLenghtTol && abs(v.Y) < zeroLenghtTol then FsExGeoDivByZeroException.Raise "UnitVec.DirDiamondInXY: input vector is vertical or zero length:%O" v
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
        member inline v.Angle2PIInXY =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG 
            if abs(v.X) < zeroLenghtTol && abs(v.Y) < zeroLenghtTol then FsExGeoDivByZeroException.Raise "UnitVec.Angle2PIInXY: input vector is vertical or zero length:%O" v
            #endif
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Util.twoPi
            else  
                a        
         
        /// Returns the Angle in Degrees from XAxis.  
        /// Going Counter clockwise till 360.
        member inline v.Angle360InXY =
            v.Angle2PIInXY |> toDegrees
        
        member inline v.AsVec        = Vec(v.X, v.Y, v.Z)
        member inline v.AsPnt        = Pnt(v.X, v.Y, v.Z)
        member inline v.AsVc         = Vc(v.X, v.Y)        

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        static member inline XAxis  = UnitVec.createUnchecked (1.0 , 0.0, 0.0)        
        static member inline YAxis  = UnitVec.createUnchecked (0.0 , 1.0, 0.0)
        static member inline ZAxis  = UnitVec.createUnchecked (0.0 , 0.0, 1.0)


        /// cross product // A x B = |A|*|B|*sin(angle), direction follow right-hand rule
        static member inline cross (a:UnitVec, b:UnitVec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X )       
        /// cross product // A x B = |A|*|B|*sin(angle), direction follow right-hand rule
        static member inline cross (a:UnitVec, b:Vec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) 
        /// cross product // A x B = |A|*|B|*sin(angle), direction follow right-hand rule
        static member inline cross (a:Vec, b:UnitVec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) 

        /// dot product, or scalar product
        static member inline dot  (a:UnitVec, b:UnitVec  )     = a.X * b.X + a.Y * b.Y + a.Z * b.Z
        /// dot product, or scalar product
        static member inline dot  (a:Vec, b:UnitVec  ) = a.X * b.X + a.Y * b.Y + a.Z * b.Z
        /// dot product, or scalar product
        static member inline dot  (a:UnitVec, b:Vec  ) = a.X * b.X + a.Y * b.Y + a.Z * b.Z


        static member inline getX     (v:UnitVec) = v.X
        static member inline getY     (v:UnitVec) = v.Y
        static member inline getZ     (v:UnitVec) = v.Z
        static member inline setX     x (v:UnitVec) = v.WithX x
        static member inline setY     y (v:UnitVec) = v.WithY y
        static member inline setZ     z (v:UnitVec) = v.WithZ z
        static member inline add      (a:UnitVec) (b:UnitVec) = b + a  
        static member inline dirMatch (a:UnitVec) (b:UnitVec) = b.MatchesOrientation a
        static member inline scale    (f:float) (v:UnitVec) = Vec (v.X * f , v.Y * f , v.Z * f)        
        static member inline shiftX     x (v:UnitVec) = Vec (v.X+x, v.Y,   v.Z)
        static member inline shiftY     y (v:UnitVec) = Vec (v.X,   v.Y+y, v.Z)
        static member inline shiftZ     z (v:UnitVec) = Vec (v.X,   v.Y,   v.Z+z)
        static member inline setLength  f (v:UnitVec) = Vec (v.X * f , v.Y * f , v.Z * f) 
           
        /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float. 
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Staticaly Resolved Type Parmeters at compile time.
        static member inline ofXYZ vec  = 
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            let z = ( ^T : (member Z : _) vec)
            try UnitVec.create(float x, float y, float z) 
            with e -> FsExGeoDivByZeroException.Raise "UnitVec.ofXYZ: %A could not be converted to a FsEx.Geo.UnitVec:\r\n%A" vec e

        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float. 
        /// Does the unitizing too.
        /// Internally this is not using reflection at runtime but F# Staticaly Resolved Type Parmeters at compile time.
        static member inline ofxyz vec  = 
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            let z = ( ^T : (member z : _) vec)
            try UnitVec.create(float x, float y, float z) 
            with e -> FsExGeoDivByZeroException.Raise "UnitVec.ofxyz: %A could not be converted to a FsEx.Geo.UnitVec:\r\n%A" vec e

        /// Does the unitizing too.
        static member inline ofPnt  (pt:Pnt) =  
            let l = sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z) 
            if l <  zeroLenghtTol then FsExGeoDivByZeroException.Raise "UnitVec.ofPnt failed on too short %O" pt
            let li = 1. / l
            UnitVec.createUnchecked( li*pt.X , li*pt.Y , li*pt.Z ) 
        
        /// Does the unitizing too.
        static member inline ofVec (v:Vec) = 
            let l = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z) 
            if l <  zeroLenghtTol then FsExGeoDivByZeroException.Raise "UnitVec.ofVec failed on too short %O" v
            let li = 1. / l
            UnitVec.createUnchecked( li*v.X , li*v.Y , li*v.Z )       
        
        
        /// Project vector to World XY Plane.
        /// Use make2D to convert to 2D vector instance
        static member inline projectToXYPlane (v:UnitVec) = Vec(v.X,v.Y, 0.0)
        
        /// Project vector to World XY Plane.
        /// Use projectToXYPlane to keep to 3D vector instance
        static member inline make2D (v:UnitVec) = Vc(v.X,v.Y)
        
        /// Same as reverse
        static member inline flip  (v:Vec) = -v

        /// Same as flip
        static member inline reverse  (v:Vec) = -v   

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
            // at +/-1. (See Windows OS Bug 01706299 for details) (form WPF refrence scource code)
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
        /// Considering positve rotation round the World ZAxis
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
        
        // Rotate2D: 

        /// Rotate the 3D UnitVector around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member rotateXBy (r:Rotation2D) (v:UnitVec) = UnitVec.createUnchecked (v.X,  r.cos*v.Y - r.sin*v.Z, r.sin*v.Y + r.cos*v.Z)
        
        /// Rotate the 3D UnitVector around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member rotateYBy (r:Rotation2D) (v:UnitVec) = UnitVec.createUnchecked ( r.sin*v.Z + r.cos*v.X,  v.Y, r.cos*v.Z - r.sin*v.X) 
        
        /// Rotate the 3D UnitVector around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member rotateZBy (r:Rotation2D) (v:UnitVec) = UnitVec.createUnchecked (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y,  v.Z)
        
        
        /// Rotate the 3D UnitVector in Degrees around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (v:UnitVec) = 
            UnitVec.rotateXBy (Rotation2D.createFromDegrees angDegree) v
    
        /// Rotate the 3D UnitVector in Degrees around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (v:UnitVec) = 
            UnitVec.rotateYBy  (Rotation2D.createFromDegrees angDegree) v 
    
        /// Rotate the 3D UnitVector in Degrees around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (v:UnitVec) = 
            UnitVec.rotateZBy  (Rotation2D.createFromDegrees angDegree) v 
        
        /// Flips the vector if Z part is smaller than 0.0
        static member inline flipToPointUp (v:UnitVec) = if v.Z < 0.0 then -v else v 

        
        /// Vector length projected into X Y Plane
        /// sqrt( v.X * v.X  + v.Y * v.Y)
        static member inline lengthInXY(v:UnitVec) = sqrt(v.X * v.X  + v.Y * v.Y)

        /// Checks if a vector is vertical by doing:
        /// abs(v.X) + abs(v.Y) < zeroLenghtTol
        static member inline isVertical (v:UnitVec) =             
            abs(v.X) + abs(v.Y) < zeroLenghtTol

        /// Checks if a vector is horizontal  by doing:
        /// abs(v.Z) < zeroLenghtTol
        static member inline isHorizontal (v:UnitVec) =             
            abs(v.Z) < zeroLenghtTol     

        /// Returns positive or negative slope of a vector in Radians
        /// in relation to XY Plane
        static member inline slopeRad (v:UnitVec) = 
            v.Y |> clamp11 |> acos           

        /// Returns positive or negative slope of a vector in Degrees
        /// in relation to XY Plane
        static member inline slopeDeg (v:UnitVec) = 
            UnitVec.slopeRad v |> toDegrees

        /// Returns positive or negative slope of a vector in Percent
        /// in relation to XY Plane
        /// 100% = 45 degrees
        static member inline slopePercent (v:UnitVec) = 
            if abs(v.Z) < zeroLenghtTol then FsExGeoDivByZeroException.Raise "UnitVec.slopePercent: Can't get Slope from vertical vector %O" v
            let l = UnitVec.lengthInXY v
            100.0 * (v.Z/l)

        /// Reverse vector if Z part is smaller than 0.0
        static member inline orientUp (v:UnitVec) = 
            if v.Z < 0.0 then -v else v

        /// Reverse vector if Z part is bigger than 0.0
        static member inline orientDown (v:UnitVec) = 
            if v.Z < 0.0 then v else -v

        /// Ensure vector has a positive dot product with given orientation vector
        static member inline matchOrientation (orientationToMatch:UnitVec) (v:UnitVec) = 
            if orientationToMatch * v < 0.0 then -v else v

        /// Check if vector has a positive dot product with given orientation vector
        static member inline doesOrientationMatch (orientationToCkeck:UnitVec) (v:UnitVec) = 
            orientationToCkeck * v > 0.0        

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.        
        /// Just does Vec(-v.Y, v.X, 0.0) 
        /// On vertical input vector resulting vector if of zero length. 
        static member inline perpendicularInXY (v:UnitVec) :Vec = 
            Vec(-v.Y, v.X, 0.0) 


        /// Returns a vector that is perpendicular to the given vector and in the same vertical Plane.        
        /// Projected into the XY Plane input and output vectors are parallell and of same orientation.
        /// Not of same length, not unitized.
        /// On vertical input vector resulting vector if of zero length. 
        static member inline perpendicularInVerticalPlane (v:UnitVec) :Vec = 
            let hor = Vec(v.Y, -v.X, 0.0)
            let r = UnitVec.cross (v, hor)            
            if v.Z < 0.0 then -r else r


        //[<Obsolete("Unsave Member") >]
        //static member Zero = Vec ( 0. , 0. , 0.)  // needed by 'Array.sum' 
        //[<Obsolete("Unsave Member") >]
        //static member inline DivideByInt (v:UnitVec, i:int) = if i<>0 then v / float i else failwithf "DivideByInt 0 %O " v // needed by  'Array.average'  


