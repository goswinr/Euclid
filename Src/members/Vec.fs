namespace FsEx.Geo
open System

/// Members and operators for 3D Points, Vectors and Rotations
[<AutoOpen>]
module AutoOpenVec = 
    open Util

    type Vec with         

        member inline v.IsZero = v.X = 0.0 && v.Y = 0.0 && v.Z= 0.0
        member inline v.IsTiny tol = abs v.X < tol && abs v.Y < tol && abs v.Z < tol
        //member inline v.IsInValid =  Double.IsNaN v.X || Double.IsNaN v.Y || Double.IsNaN v.Z || Double.IsInfinity v.X || Double.IsInfinity v.Y || Double.IsInfinity v.Z

        member inline v.Length = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z) 
        /// Length Squared
        member inline v.LengthSq = v.X*v.X + v.Y*v.Y + v.Z*v.Z
        member inline v.LengthInXY =  sqrt (v.X*v.X + v.Y*v.Y)
        member inline v.LengthSqInXY = v.X*v.X + v.Y*v.Y

        member inline v.WithX x = Vec (x ,v.Y, v.Z) // returns new Vector with new x coordinate, y and z the same as before
        member inline v.WithY y = Vec (v.X, y, v.Z)
        member inline v.WithZ z = Vec (v.X ,v.Y, z) 
    
        member inline v.Half = Vec (v.X*0.5 ,v.Y*0.5, v.Z*0.5)
    
        /// Test for positive Dot product
        member inline v.MatchesOrientation (vv:Vec) = v*vv > 0. // direction match
    
        member inline v.WithLength (desiredLength:float) =  
            let l = sqrt(v.X*v.X+v.Y*v.Y+v.Z*v.Z) 
            if l < zeroLenghtTol then FsExGeoDivByZeroException.Raise "Vec.WithLength %g : %O is too small for unitizing, Tolerance:%g" desiredLength v zeroLenghtTol
            v*(desiredLength / l)            
            
        member inline v.Unitized =  
            let l = sqrt(v.X*v.X+v.Y*v.Y+v.Z*v.Z) 
            if l < zeroLenghtTol then FsExGeoDivByZeroException.Raise "%O is too small for unitizing, Tolerance:%g" v zeroLenghtTol
            let li=1./l in 
            UnitVec.createUnchecked( li*v.X , li*v.Y ,li*v.Z )             
    
        member inline v.UnitizedUnchecked =  
            let li = 1. / sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z) 
            UnitVec.createUnchecked( li*v.X , li*v.Y ,li*v.Z ) 
    
        member inline v.IsUnit   = 
            let l = v.Length 
            0.999999999 < l && 
            1.000000001 > l
        

        /// The diamond angle is always positive and in the range of 0.0 to 4.0 ( for 360 degrees) 
        /// 0.0 = XAxis,  going Counter clockwise. Ignoring Z component.
        member inline v.DirDiamondInXY =
            // https://stackoverflow.com/a/14675998/969070            
            #if DEBUG 
            if abs(v.X) < zeroLenghtTol && abs(v.Y) < zeroLenghtTol then 
                FsExGeoDivByZeroException.Raise "Vec.DirDiamondInXY: input vector is vertical or zero length:%O" v
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
            if abs(v.X) < zeroLenghtTol && abs(v.Y) < zeroLenghtTol then 
                FsExGeoDivByZeroException.Raise "Vec.Angle2PIInXY: input vector is vertical or zero length:%O" v
            #endif
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Util.twoPi
            else  
                a
        
        /// Returns the Angle in Degrees from XAxis.  
        /// Going Counter clockwise till 360.  Ignoring Z component.
        member inline v.Angle360InXY =
            v.Angle2PiInXY |> toDegrees        
        
        member inline v.AsPnt       = Pnt(v.X, v.Y, v.Z)
        member inline v.AsVc        = Vc(v.X, v.Y)

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.
        /// Or Vec.Zero if input is vertical.
        /// just does Vec(-v.Y, v.X, 0.0) 
        member inline v.PerpendicularInXY() = Vec(-v.Y, v.X, 0.0) 
        
        
        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------
        
        
        static member Zero = Vec ( 0. , 0. , 0.)  // needed by 'Array.sum' 
    
        static member inline XAxis  = Vec (1.0 , 0.0, 0.0)
        static member inline YAxis  = Vec (0.0 , 1.0, 0.0)
        static member inline ZAxis  = Vec (0.0 , 0.0, 1.0)         

        /// cross product // A x B = |A|*|B|*sin(angle), direction follow right-hand rule
        static member inline cross (a:Vec, b:Vec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X )       
        /// cross product // A x B = |A|*|B|*sin(angle), direction follow right-hand rule
        static member inline cross (a:UnitVec, b:Vec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) 
        /// cross product // A x B = |A|*|B|*sin(angle), direction follow right-hand rule
        static member inline cross (a:Vec, b:UnitVec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) 

        /// dot product, or scalar product
        static member inline dot  (a:Vec, b:Vec  )     = a.X * b.X + a.Y * b.Y + a.Z * b.Z
        /// dot product, or scalar product
        static member inline dot  (a:Vec, b:UnitVec  ) = a.X * b.X + a.Y * b.Y + a.Z * b.Z
        /// dot product, or scalar product
        static member inline dot  (a:UnitVec, b:Vec  ) = a.X * b.X + a.Y * b.Y + a.Z * b.Z

    
        static member inline getX     (v:Vec) = v.X
        static member inline getY     (v:Vec) = v.Y
        static member inline getZ     (v:Vec) = v.Z
        static member inline setX     x (v:Vec) = v.WithX x
        static member inline setY     y (v:Vec) = v.WithY y
        static member inline setZ     z (v:Vec) = v.WithZ z
        static member inline add      (a:Vec) (b:Vec) = b + a
        static member inline lengthSq (v:Vec) = v.LengthSq
        static member inline dirMatch (a:Vec) (b:Vec) = b.MatchesOrientation a
        static member inline scale    (f:float) (v:Vec) = v*f
        static member inline length       (v:Vec) = v.Length
        static member inline shiftX     x (v:Vec) = Vec (v.X+x, v.Y,   v.Z)
        static member inline shiftY     y (v:Vec) = Vec (v.X,   v.Y+y, v.Z)
        static member inline shiftZ     z (v:Vec) = Vec (v.X,   v.Y,   v.Z+z)    
        static member inline isTiny   tol (v:Vec) = v.IsTiny tol
        static member inline setLength  f (v:Vec) = v.WithLength f    

        /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Staticaly Resolved Type Parmeters at compile time.
        static member inline ofXYZ vec  = 
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            let z = ( ^T : (member Z : _) vec)
            try Vec(float x, float y, float z) 
            with e -> FsExGeoDivByZeroException.Raise "Vec.ofXYZ: %A could not be converted to a FsEx.Geo.Vec:\r\n%A" vec e
        
        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Staticaly Resolved Type Parmeters at compile time.
        static member inline ofxyz vec  = 
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            let z = ( ^T : (member z : _) vec)
            try Vec(float x, float y, float z) 
            with e -> FsExGeoDivByZeroException.Raise "Vec.ofxyz: %A could not be converted to a FsEx.Geo.Vec:\r\n%A" vec e   
            
        //static member inline ofPnt   (pt:Pnt) = Vec( pt.X , pt.Y , pt.Z )         
        //static member inline ofUnitVec (v:Vec) = Vec( v.X , v.Y , v.Z ) 
        //static member inline create (x:float, y:float, z:float) =  Vec( x , y , z )
        static member inline create (start:Pnt,ende:Pnt) = ende-start  
        
        /// Returns a 3D vector from z value and 2D vector.
        static member inline ofVcWithZ  (z:float)  (v:Vc)  = Vec (v.X, v.Y, z) 

        /// Project vector to World XY Plane.
        /// Use make2D to convert to 2D vector instance
        static member inline projectToXYPlane (v:Vec) = Vec(v.X,v.Y, 0.0)
        
        /// Project vector to World XY Plane.
        /// Use projectToXYPlane to keep to 3D vector instance
        static member inline make2D (v:Vec) = Vc(v.X,v.Y)

        /// Same as reverse
        static member inline flip  (v:Vec) = -v

        /// Same as flip
        static member inline reverse  (v:Vec) = -v   

        /// Returns vector unitized, fails on zero length vectors
        static member inline unitize (v:Vec) =  v.Unitized
    
        /// Returns vector unitized or Vec(NaN,NaN,NaN) on zero length vectors
        static member inline unitizeUnChecked (v:Vec) = v.UnitizedUnchecked

        /// Returns three vectors Determinant
        /// This is also the signed volume of the Parallelepipeds define by these three vectors.
        /// Also called scalar triple product, mixed product, box product, or in german: Spatprodukt.
        /// It is defined as the dot product of one of the vectors with the cross product of the other two.
        static member inline determinant (u:Vec, v:Vec, w:Vec) = u.X*v.Y*w.Z + v.X*w.Y*u.Z + w.X*u.Y*v.Z - w.X*v.Y*u.Z - v.X*u.Y*w.Z - u.X*w.Y*v.Z 
    
        /// Returns positive angle between two vectors in Radians.
        /// Takes vector orientation into account,
        /// Range 0.0 to PI( = 0 to 180 degree).
        static member inline anglePI (a:Vec) (b:Vec) = 
            UnitVec.anglePI a.Unitized b.Unitized 

        /// Returns positive angle between two vectors in Degrees.
        /// Takes vector orientation into account,
        /// Range 0 to 180 degrees.
        static member inline angle180 (a:Vec) (b:Vec) = 
            UnitVec.angle180 a.Unitized b.Unitized 

        /// Returns positive angle between two vectors in Radians.
        /// Ignores vector orientation,
        /// Range: 0.0 to PI/2 ( = 0 to 90 degrees) 
        static member inline angleHalfPI (a:Vec) (b:Vec) = 
            UnitVec.angleHalfPI a.Unitized b.Unitized 

        /// Returns positive angle between two vectors in Degrees.
        /// Ignores vector orientation,
        /// Range: 0 to 90 degrees.
        static member inline angle90 (a:Vec) (b:Vec) = 
            UnitVec.angle90 a.Unitized b.Unitized     
        

        /// Returns positive angle from Vector 'a' to Vector 'b' projected in XY Plane.
        /// In Radians
        /// Considering counter clockwise rotation round the World ZAxis
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline angle2PiInXY (a:Vec, b:Vec)   =
            let r = b.Angle2PiInXY  - a.Angle2PiInXY            
            if r >= 0. then  r
            else r + Util.twoPi 

        /// Returns positive angle of two Vector projected in XY Plane in Degrees
        /// Considering positve rotation round the World ZAxis
        /// Range:  0 to 360 degrees
        static member inline angle360InXY (a:Vec, b:Vec)   = 
            Vec.angle2PiInXY (a, b) |> toDegrees
        
        /// Returns a (not unitized) bisector vector in the middle direction. 
        /// Code : a.Unitized + b.Unitized
        static member inline bisector (a:Vec) (b:Vec) = a.Unitized + b.Unitized 


        /// Flips the vector if Z part is smaller than 0.0
        static member inline flipToPointUp (v:Vec) = if v.Z < 0.0 then -v else v  

        // Rotate2D: 

        /// Rotate the 3D Vector around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member rotateXBy (r:Rotation2D) (v:Vec) = Vec (v.X,  r.cos*v.Y - r.sin*v.Z, r.sin*v.Y + r.cos*v.Z)
        
        /// Rotate the 3D Vector around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member rotateYBy (r:Rotation2D) (v:Vec) = Vec ( r.sin*v.Z + r.cos*v.X,  v.Y, r.cos*v.Z - r.sin*v.X) 
        
        /// Rotate the 3D Vector around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member rotateZBy (r:Rotation2D) (v:Vec) = Vec (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y,  v.Z)
        
        
        /// Rotate the 3D Vector in Degrees around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (v:Vec) = 
            Vec.rotateXBy (Rotation2D.createFromDegrees angDegree) v
    
        /// Rotate the 3D Vector in Degrees around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (v:Vec) = 
            Vec.rotateYBy  (Rotation2D.createFromDegrees angDegree) v 
    
        /// Rotate the 3D Vector in Degrees around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (v:Vec) = 
            Vec.rotateZBy  (Rotation2D.createFromDegrees angDegree) v  



        /// Vector length projected into X Y Plane
        /// sqrt( v.X * v.X  + v.Y * v.Y)
        static member inline lengthInXY(v:Vec) = sqrt(v.X * v.X  + v.Y * v.Y)

        /// Checks if a vector is vertical by doing:
        /// abs(v.X) + abs(v.Y) < zeroLenghtTol
        /// fails on tiny (shorter than zeroLenghtTol) vectors
        static member inline isVertical (v:Vec) =             
            if v.IsTiny(zeroLenghtTol) then FsExGeoDivByZeroException.Raise "Vec Cannot not check very tiny vector for verticality %O" v
            abs(v.X) + abs(v.Y) < zeroLenghtTol

        /// Checks if a vector is horizontal  by doing:
        /// abs(v.Z) < zeroLenghtTol
        /// Fails on tiny (shorter than zeroLenghtTol) vectors
        static member inline isHorizontal (v:Vec) =            
             if v.IsTiny(zeroLenghtTol) then FsExGeoDivByZeroException.Raise "Vec Cannot not check very tiny vector for horizontality %O" v
             abs(v.Z) < zeroLenghtTol     

        /// Unitize vector, if input vector is shorter than 1e-6 alternative vector is returned (without beeing unitized).
        static member inline unitizeWithAlternative (unitVectorAlt:Vec) (v:Vec) = 
            let l = v.LengthSq
            if l < 1e-12  then  //sqrt (1e-06)
                unitVectorAlt 
            else
                let f = 1.0 / sqrt(l)
                Vec(v.X*f , v.Y*f , v.Z*f)    

        /// Returns positive or negative slope of a vector in Radians
        /// in relation to XY Plane
        static member inline slopeRad (v:Vec) = 
            let f = Vec(v.X, v.Y, 0.0)
            if v.Z >= 0.0 then  Vec.angleHalfPI v f
            else              -(Vec.angleHalfPI v f)

        /// Returns positive or negative slope of a vector in Degrees
        /// in relation to XY Plane
        static member inline slopeDeg (v:Vec) = 
            let f = Vec(v.X, v.Y, 0.0)
            if v.Z >= 0.0 then  Vec.angle90 v f
            else              -(Vec.angle90 v f)

        /// Returns positive or negative slope of a vector in Percent
        /// in relation to XY Plane
        /// 100% = 45 degrees
        static member inline slopePercent (v:Vec) = 
            if abs(v.Z) < zeroLenghtTol then FsExGeoDivByZeroException.Raise "UnitVec.slopePercent: Can't get Slope from vertical vector %O" v
            let f = Vec(v.X, v.Y, 0.0)
            100.0 * (v.Z/f.Length)


        /// Reverse vector if Z part is smaller than 0.0
        static member inline orientUp (v:Vec) = 
            if v.Z < 0.0 then -v else v

        /// Reverse vector if Z part is bigger than 0.0
        static member inline orientDown (v:Vec) = 
            if v.Z < 0.0 then v else -v

        /// Ensure vector has a positive dot product with given orientation vector
        static member inline matchOrientation (orientationToMatch:Vec) (v:Vec) = 
            if orientationToMatch * v < 0.0 then -v else v
        

        /// Check if vector has a positive dot product with given orientation vector
        static member inline doesOrientationMatch (orientationToCkeck:Vec) (v:Vec) = 
            orientationToCkeck * v > 0.0        

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.        
        /// Just does Vec(-v.Y, v.X, 0.0) 
        /// On vertical input vector resulting vector if of zero length. 
        static member inline perpendicularInXY (v:Vec) = 
            Vec(-v.Y, v.X, 0.0) 


        /// Returns a vector that is perpendicular to the given vector and in the same vertical Plane.        
        /// Projected into the XY Plane input and output vectors are parallell and of same orientation.
        /// Not of same length, not unitized.
        /// On vertical input vector resulting vector if of zero length. 
        static member inline perpendicularInVerticalPlane (v:Vec) = 
            let hor = Vec(v.Y, -v.X, 0.0)
            let r = Vec.cross (v, hor)            
            if v.Z < 0.0 then -r else r


        //[<Obsolete("Unsave Member") >]
        //static member inline DivideByInt (v:UnitVec, i:int) = if i<>0 then v / float i else failwithf "DivideByInt 0 %O " v // needed by  'Array.average'

