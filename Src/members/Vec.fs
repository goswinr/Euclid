namespace FsEx.Geo
open System

/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type Vec
[<AutoOpen>]
module AutoOpenVec = 
    open Util

    type Vec with         
        
        /// Returns a boolean indicating wether X,Y and Z are exactly 0.0.
        member inline v.IsZero =  v.X = 0.0 && v.Y = 0.0 && v.Z= 0.0 
        
        /// Returns a boolean indicating wether the absolute value of X,Y and Z is each less than the given tolerance.
        member inline v.IsTiny tol = abs v.X < tol && abs v.Y < tol && abs v.Z < tol

        /// Returns the length of the 3D vector 
        member inline v.Length = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z) 

        /// Returns the squared length of the 3D vector 
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSq = v.X*v.X + v.Y*v.Y + v.Z*v.Z

        /// Returns the length of the 3D vector projected into World X-Y plane.
        member inline v.LengthInXY =  sqrt (v.X*v.X + v.Y*v.Y)

        /// Returns the squared length of the 3D vector projected into World X-Y plane.
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSqInXY = v.X*v.X + v.Y*v.Y

        /// Returns  a new 3D vector with new X coordinate, Y and Z  stay the same.
        member inline v.WithX x = Vec (x ,v.Y, v.Z) 

        /// Returns a new 3D vector with new y coordinate, X and Z  stay the same.
        member inline v.WithY y = Vec (v.X, y, v.Z)

        /// Returns a new 3D vector with new z coordinate, X and Y  stay the same.
        member inline v.WithZ z = Vec (v.X ,v.Y, z) 
        
        /// Returns a new 3D vector with half the length.
        member inline v.Half = Vec (v.X*0.5 ,v.Y*0.5, v.Z*0.5)
    
        /// Returns a new 3D vector scaled to the desired length.
        member inline v.WithLength (desiredLength:float) =  
            let l = sqrt(v.X*v.X+v.Y*v.Y+v.Z*v.Z) 
            if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "Vec.WithLength %g : %O is too small for unitizing, Tolerance:%g" desiredLength v zeroLengthTol
            v*(desiredLength / l)            
        
        /// Returns the 3D vector unitized.
        /// Fails with FsExGeoDivByZeroException if the length of the vector is 
        /// too small (1-e16) to unitize.    
        member inline v.Unitized =  
            let l = sqrt(v.X*v.X+v.Y*v.Y+v.Z*v.Z) 
            if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "%O is too small for unitizing, Tolerance:%g" v zeroLengthTol
            let li=1./l in 
            UnitVec.createUnchecked( li*v.X , li*v.Y ,li*v.Z )             

        // Returns the 3D vector unitized.
        // If the length of the vector is 0.0 an invalid unit vector is returned.
        // UnitVec(0,0,0)
        //member inline v.UnitizedUnchecked =  
        //    let li = 1. / sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z) 
        //    UnitVec.createUnchecked( li*v.X , li*v.Y ,li*v.Z ) 

        /// Test if the 3D vector is a unit vector. 
        /// Test if the vectors square length is within 6 float steps of 1.0
        /// So between 0.99999964 and 1.000000715.
        member inline v.IsUnit   = 
            Util.isOne v.LengthSq        

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.
        /// Or Vec.Zero if input is vertical.
        /// Just does Vec(-v.Y, v.X, 0.0) 
        member inline v.PerpendicularInXY = Vec(-v.Y, v.X, 0) 

        /// 90 Degree rotation counter clockwise around Z-axis.
        member inline v.RotateOnZ90CCW = Vec( -v.Y,   v.X ,   v.Z)

        /// 90 Degree rotation clockwise around Z-axis.
        member inline v.RotateOnZ90CW  = Vec(  v.Y,  -v.X,   v.Z  )  
        
        

        /// The diamond angle.
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions. 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionDiamondInXY =
            // https://stackoverflow.com/a/14675998/969070            
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.DirectionDiamondInXY: input vector is vertical or zero length:%O" v
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
        /// Going Counter clockwise till two Pi.         
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction2PiInXY =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.Direction2PiInXY: input vector is zero length: %O" v
            #endif
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Util.twoPi
            else  
                a

        /// Returns the Angle in Radians from XAxis, 
        /// Ignores orientation.
        /// Range 0.0 to Pi. 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.DirectionPiInXY =
            // https://stackoverflow.com/a/14675998/969070            
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.DirectionPiInXY: input vector is zero length: %O" v
            #endif
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Math.PI
            else  
                a
        
        /// Returns the Angle in Degrees from XAxis.  
        /// Going Counter clockwise till 360. 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        member inline v.Direction360InXY =
            v.Direction2PiInXY |> toDegrees
        
        /// Returns the Angle in Radians from XAxis, 
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
        member inline v.AngleDiamondInXYTo (b:Vec)   =              
            let r = b.DirectionDiamondInXY - v.DirectionDiamondInXY          
            if r >= 0. then  r
            else r + 4.0   
        
        /// Convert 3D vector to 3D Point.
        member inline v.AsPnt = Pnt(v.X, v.Y, v.Z)

        /// Convert 3D vector to 2D Vector, discarding the Z value. 
        member inline v.AsVc  = Vc(v.X, v.Y)        
        

        /// Checks if the angle between the two 3D vectors is less than 180 degrees.
        /// Calculates the dot product of two 3D vectors. 
        /// Then checks if it is positive.
        member inline v.MatchesOrientation180  (other:Vec) = 
            v * other > 0.0  

        /// Checks if the angle between the two 3D vectors is less than 90 degrees.   
        /// Calculates the dot product of the two 3D vectors unitized. 
        /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
        member inline v.MatchesOrientation90  (other:Vec) = 
            v.Unitized * other.Unitized > 0.707107
            
        /// Checks if two 3D vectors are parallel. Ignoring orientation
        /// Calculates the cross product of the two 3D vectors. (It's length is the volume of the parallelepiped)
        /// And checks if it is smaller than 1e-9.
        /// (NOTE: for very long 3D vectors a higher tolerance might be needed)
        member inline v.IsParallelTo  (other:Vec) =                     
            let x =  v.Y * other.Z - v.Z * other.Y   
            let y =  v.Z * other.X - v.X * other.Z   
            let z =  v.X * other.Y - v.Y * other.X 
            (x*x + y*y + z*z) < 3.162278e-05 // sqrt of 1e-9

        /// Checks if two 3D vectors are parallel and orientated the same way.
        /// Calculates the cross product of the two 3D vectors. (It's length is the volume of the parallelepiped)
        /// And checks if it is smaller than 1e-9.
        /// Then calculates the dot product and checks if it is positive.
        /// (NOTE: for very long 3D vectors a higher tolerance might be needed)
        member inline v.IsParallelAndOrientedTo  (other:Vec) =
            let x =  v.Y * other.Z - v.Z * other.Y   
            let y =  v.Z * other.X - v.X * other.Z   
            let z =  v.X * other.Y - v.Y * other.X 
            (x*x + y*y + z*z) < 3.162278e-05 // sqrt of 1e-9
            && 
            v.X*other.X + v.Y*other.Y + v.Z*other.Z > 0.0 
            
        /// Checks if two 3D vectors are perpendicular. 
        /// Calculates the dot product and checks if it is smaller than 1e-9.
        /// (NOTE: for very long 3D vectors a higher tolerance might be needed)
        member inline v.IsPerpendicularTo (other:Vec) =     
            abs(v.X*other.X + v.Y*other.Y + v.Z*other.Z) < 1e-9 

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------
        
        /// Returns the World X-axis with length one: Vec(1,0,0)
        static member inline XAxis  = Vec(1,0,0)

        /// Returns the World Y-axis with length one: Vec(0,1,0)
        static member inline YAxis  = Vec(0,1,0)

        /// Returns the World Z-axis with length one: Vec(0,0,1)
        static member inline ZAxis  = Vec(0,0,1)        

        /// Returns a zero length vector: Vec(0,0,0)
        static member inline Zero   = Vec(0,0,0)  // this member is needed by Seq.sum, so that it doesn't fail on empty seq.  
        
        /// Returns the distance between the tips of two 3D vectors.
        static member inline difference (a:Vec) (b:Vec) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
                
        /// Returns the squared distance between the tips of two 3D vectors.
        /// This operation is slightly faster than Vec.difference  and sufficient for many algorithms like finding closest points.
        static member inline differenceSq (a:Vec) (b:Vec) = let v = a-b in  v.X*v.X + v.Y*v.Y + v.Z*v.Z

        /// Divides the vector by an integer.
        /// (This member is needed by Array.average and similar functions)
        static member inline DivideByInt (v:Vec, i:int) = // needed by 'Array.average'
            if i<>0 then v / float i 
            else FsExGeoDivByZeroException.Raise "FsEx.Geo.Vec.DivideByInt is zero %O " v 
        
                /// Accepts any type that has a X, Y and Z (UPPERCASE) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofXYZ vec  = 
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            let z = ( ^T : (member Z : _) vec)
            try Vec(float x, float y, float z) 
            with e -> FsExGeoException.Raise "FsEx.Geo.Vec.ofXYZ: %A could not be converted to a FsEx.Geo.Vec:\r\n%A" vec e
        
        /// Accepts any type that has a x, y and z (lowercase) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxyz vec  = 
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            let z = ( ^T : (member z : _) vec)
            try Vec(float x, float y, float z) 
            with e -> FsExGeoException.Raise "FsEx.Geo.Vec.ofxyz: %A could not be converted to a FsEx.Geo.Vec:\r\n%A" vec e   

        /// Create 3D vector from 3D point. 
        static member inline ofPnt  (pt:Pnt) =  Vec( pt.X , pt.Y , pt.Z ) 
        
        /// Create 3D vector from 3D unit vector.
        static member inline ofUnitVec (v:UnitVec) =  Vec(v.X, v.Y, v.Z)       
        
        /// Convert 3D vector to 2D point by ignoring Z value. 
        static member inline asPt(v:Vec)  = Pt( v.X, v.Y)

        /// Convert 3D vector to 2D vector by ignoring Z value. 
        static member inline asVc(v:Vec) = Vc(v.X, v.Y)  
        
        /// Convert 3D vector to 3D point. 
        static member inline asPnt(v:Vec) = Pnt(v.X, v.Y, v.Z) 
        

        /// Cross product, of two 3D vectors. 
        /// The resulting vector is perpendicular to both input vectors.
        /// Its length is the area of the parallelogram spanned by the input vectors.
        /// Its direction follows th right-hand rule.
        /// A x B = |A| * |B| * sin(angle)
        static member inline cross (a:Vec, b:Vec)  = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X )       
        
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

        /// Dot product, or scalar product of two 3D vectors. 
        /// Returns a float. 
        static member inline dot  (a:Vec, b:Vec)   = a.X * b.X + a.Y * b.Y + a.Z * b.Z
        
        /// Dot product, or scalar product of a 3D unit vector with a 3D vector  
        /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit vector
        static member inline dot  (a:UnitVec, b:Vec ) = a.X * b.X + a.Y * b.Y + a.Z * b.Z
        
        /// Dot product, or scalar product of a 3D vector with a 3D unit vector  
        /// Returns a float. This float is the projected length of the 3D vector on the direction of the unit vector
        static member inline dot  (a:Vec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z

        /// Gets the X part of this 3D vector
        static member inline getX  (v:Vec) = v.X
        
        /// Gets the Y part of this 3D vector
        static member inline getY (v:Vec) = v.Y
        
        /// Gets the Z part of this 3D vector
        static member inline getZ  (v:Vec) = v.Z
        
        /// Returns new 3D vector with new X value, Y  and Z stay the same.
        static member inline setX  x (v:Vec) = v.WithX x
        
        /// Returns new 3D vector with new Y value, X  and Z stay the same.
        static member inline setY  y (v:Vec) = v.WithY y
        
        /// Returns new 3D vector with new z value, X  and Y stay the same.
        static member inline setZ z (v:Vec) = v.WithZ z
        
        /// Add two 3D vectors together. Returns a new 3D vector.
        static member inline add      (a:Vec) (b:Vec) = b + a  
        
        /// Multiplies a 3D vector with a scalar, also called scaling a vector. 
        /// Same as Vec.setLength. Returns a new 3D vector.
        static member inline scale    (f:float) (v:Vec) = Vec (v.X * f , v.Y * f , v.Z * f)    

        /// Multiplies a 3D vector with a scalar, also called scaling a vector. 
        /// Same as Vec.scale. Returns a new 3D vector.
        static member inline setLength(f:float) (v:Vec) = Vec (v.X * f , v.Y * f , v.Z * f) 
        
        /// Add to the X part of this 3D vectors together. Returns a new 3D vector.
        static member inline moveX x (v:Vec) = Vec (v.X+x, v.Y,   v.Z)
        
        /// Add to the Y part of this 3D vectors together. Returns a new 3D vector.
        static member inline moveY y (v:Vec) = Vec (v.X,   v.Y+y, v.Z)
        
        /// Add to the Z part of this 3D vectors together. Returns a new 3D vector.
        static member inline moveZ z (v:Vec) = Vec (v.X,   v.Y,   v.Z+z)
        
        /// Returns a boolean indicating wether the absolute value of X,Y and Z is each less than the given tolerance.
        static member inline isTiny   tol (v:Vec) = v.IsTiny tol

        /// Returns the length of the 3D vector 
        static member inline length       (v:Vec) = v.Length

        /// Returns the squared length of the 3D vector 
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        static member inline lengthSq (v:Vec) = v.LengthSq
        
        /// Returns a new 3D vector from X,Y and Z parts.
        static member inline create (x:float, y:float, z:float) =  Vec( x , y , z )
        
        /// Returns a new 3D vector from start and end point.
        static member inline create (start:Pnt,ende:Pnt) = ende-start  
        
        /// Returns a 3D vector from z value and 2D vector.
        static member inline ofVcWithZ  (z:float)  (v:Vc)  = Vec (v.X, v.Y, z) 

        /// Project vector to World X-Y plane.
        /// Use Vc.ofVec to convert to 2D vector instance
        static member inline projectToXYPlane (v:Vec) = Vec(v.X,v.Y, 0.0)

        /// Negate or inverse a 3D vectors. Returns a new 3D vector. 
        /// Same as Vec.flip
        static member inline reverse  (v:Vec) = -v   
        
        /// Negate or inverse a 3D vectors. Returns a new 3D vector. 
        /// Same as Vec.reverse
        static member inline flip  (v:Vec) = -v

        /// Flips the vector if Z part is smaller than 0.0
        static member inline flipToPointUp (v:Vec) = if v.Z < 0.0 then -v else v  
        
        /// Returns 3D vector unitized, fails on zero length vectors
        static member inline unitize (v:Vec) =  v.Unitized    
        
        /// Unitize 3D vector, if input vector is shorter than 1e-6 the default Unit vector is returned.
        static member inline unitizeOrDefault (defaultUnitVector:UnitVec) (v:Vec) = 
            let l = v.LengthSq
            if l < 1e-12  then  // = sqrt (1e-06)
                defaultUnitVector 
            else
                let f = 1.0 / sqrt(l)
                UnitVec.createUnchecked(v.X*f , v.Y*f , v.Z*f)  

        /// Returns three vector's Determinant
        /// This is also the signed volume of the Parallelepipeds define by these three vectors.
        /// Also called scalar triple product, mixed product, Box product, or in German: Spatprodukt.
        /// It is defined as the dot product of one of the vectors with the cross product of the other two.
        static member inline determinant (u:Vec, v:Vec, w:Vec) = u.X*v.Y*w.Z + v.X*w.Y*u.Z + w.X*u.Y*v.Z - w.X*v.Y*u.Z - v.X*u.Y*w.Z - u.X*w.Y*v.Z 
    
        /// Returns positive angle between two 3D vectors in Radians.
        /// Takes vector orientation into account,
        /// Range 0.0 to Pi( = 0 to 180 Degree).
        static member inline anglePi (a:Vec) (b:Vec) = 
            UnitVec.anglePi a.Unitized b.Unitized 

        /// Returns positive angle between two 3D vectors in Degrees.
        /// Takes vector orientation into account,
        /// Range 0 to 180 Degrees.
        static member inline angle180 (a:Vec) (b:Vec) = 
            UnitVec.angle180 a.Unitized b.Unitized 

        /// Returns positive angle between two 3D vectors in Radians.
        /// Ignores vector orientation,
        /// Range: 0.0 to Pi/2 ( = 0 to 90 Degrees) 
        static member inline angleHalfPi (a:Vec) (b:Vec) = 
            UnitVec.angleHalfPi a.Unitized b.Unitized 

        /// Returns positive angle between two 3D vectors in Degrees.
        /// Ignores vector orientation,
        /// Range: 0 to 90 Degrees.
        static member inline angle90 (a:Vec) (b:Vec) = 
            UnitVec.angle90 a.Unitized b.Unitized 

        /// Returns positive angle from vector 'a' to vector 'b' projected in  X-Y plane.
        /// In Radians
        /// Considering counter clockwise rotation round the World ZAxis
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline angle2PiInXY (a:Vec, b:Vec)   =
            let r = b.Direction2PiInXY  - a.Direction2PiInXY            
            if r >= 0. then  r
            else r + Util.twoPi 

        /// Returns positive angle of two 3D vector projected in  X-Y plane. 
        /// In Degrees
        /// Considering positive rotation round the World Z-axis
        /// Range: 0 to 360 Degrees
        static member inline angle360InXY (a:Vec, b:Vec)   = 
            Vec.angle2PiInXY (a, b) |> toDegrees
        
        /// Returns positive angle for rotating counter clockwise from vector 'a' to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 Degrees) 
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline angleDiamondInXY (a:Vec , b:Vec)   = a.AngleDiamondInXYTo(b) 

        /// The diamond angle.
        /// Returns positive angle of 3D vector in World  X-Y plane.
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline directionDiamondInXY(v:Vec) = v.DirectionDiamondInXY
        
        /// Returns positive angle of 3D vector in World  X-Y plane. Counter clockwise from X-axis.
        /// In Radians
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees) 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction2PiInXY (v:Vec)   = v.Direction2PiInXY

        /// Returns positive angle of 3D vector in World  X-Y plane. Counter clockwise from X-axis.
        /// In Degree
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees) 
        /// For World X-Y plane. Considers only the X and Y components of the vector.
        static member inline direction360InXY (v:Vec)  = v.Direction360InXY

        /// Returns a (not unitized) bisector vector in the middle direction. 
        /// Code : a.Unitized + b.Unitized
        static member inline bisector (a:Vec) (b:Vec) = a.Unitized + b.Unitized 

        
        /// Ensure vector has a positive dot product with given orientation vector.
        static member inline matchOrientation (orientationToMatch:Vec) (v:Vec) = 
            if orientationToMatch * v < 0.0 then -v else v
        

        /// Checks if the angle between the two 3D vectors is less than 180 degrees.
        /// Calculates the dot product of two 3D vectors. 
        /// Then checks if it is positive.
        static member inline matchesOrientation180 (other:Vec) (v:Vec) = v.MatchesOrientation180 other             

        /// Checks if the angle between the two 3D vectors is less than 90 degrees.   
        /// Calculates the dot product of the two 3D vectors unitized. 
        /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
        static member inline matchesOrientation90 (other:Vec) (v:Vec) = v.MatchesOrientation90 other           
            
        /// Checks if two 3D vectors are parallel. Ignoring orientation
        /// Calculates the cross product of the two 3D vectors. (It's length is the volume of the parallelepiped)
        /// And checks if it is smaller than 1e-9.
        /// (NOTE: for very long 3D vectors a higher tolerance might be needed)
        static member inline isParallelTo (other:Vec) (v:Vec) =   v.IsParallelTo other

        /// Checks if two 3D vectors are parallel and orientated the same way.
        /// Calculates the cross product of the two 3D vectors. (It's length is the volume of the parallelepiped)
        /// And checks if it is smaller than 1e-9.
        /// Then calculates the dot product and checks if it is positive.
        /// (NOTE: for very long 3D vectors a higher tolerance might be needed)
        static member inline isParallelAndOrientedTo (other:Vec) (v:Vec) = v.IsParallelAndOrientedTo other
        
        /// Checks if two 3D vectors are perpendicular. 
        /// Calculates the dot product and checks if it is smaller than 1e-9.
        /// (NOTE: for very long 3D vectors a higher tolerance might be needed)
        static member inline isPerpendicularTo (other:Vec) (v:Vec) =  v.IsPerpendicularTo other
        

        // Rotate2D: 

        /// 90 Degree rotation counter clockwise around Z-axis.
        static member inline rotateOnZ90CCW(v:Vec) = Vec( -v.Y,   v.X ,   v.Z)

        /// 90 Degree rotation clockwise around Z-axis.
        static member inline rotateOnZ90CW(v:Vec)  = Vec(  v.Y,  -v.X,   v.Z  )  

        /// Rotate the 3D vector around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member rotateXBy (r:Rotation2D) (v:Vec) = Vec (v.X,  r.Cos*v.Y - r.Sin*v.Z, r.Sin*v.Y + r.Cos*v.Z)
        
        /// Rotate the 3D vector around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member rotateYBy (r:Rotation2D) (v:Vec) = Vec ( r.Sin*v.Z + r.Cos*v.X,  v.Y, r.Cos*v.Z - r.Sin*v.X) 
        
        /// Rotate the 3D vector around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member rotateZBy (r:Rotation2D) (v:Vec) = Vec (r.Cos*v.X - r.Sin*v.Y, r.Sin*v.X + r.Cos*v.Y,  v.Z)
        
        
        /// Rotate the 3D vector in Degrees around X-axis, from Y to Z-axis, Counter Clockwise looking from right.
        static member inline rotateX (angDegree) (v:Vec) = 
            Vec.rotateXBy (Rotation2D.createFromDegrees angDegree) v
    
        /// Rotate the 3D vector in Degrees around Y-axis, from Z to X-axis, Counter Clockwise looking from back.
        static member inline rotateY (angDegree) (v:Vec) = 
            Vec.rotateYBy  (Rotation2D.createFromDegrees angDegree) v 
    
        /// Rotate the 3D vector in Degrees around Z-axis, from X to Y-axis, Counter Clockwise looking from top.
        static member inline rotateZ (angDegree) (v:Vec) = 
            Vec.rotateZBy  (Rotation2D.createFromDegrees angDegree) v  

        /// Rotate by Quaternion
        static member inline rotateByQuaternion  (q:Quaternion) (v:Vec) =
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
            Vec( ix * qw + iw * - qx + iy * - qz - iz * - qy
               , iy * qw + iw * - qy + iz * - qx - ix * - qz
               , iz * qw + iw * - qz + ix * - qy - iy * - qx
               )


        /// vector length projected into X Y Plane
        /// sqrt( v.X * v.X  + v.Y * v.Y)
        static member inline lengthInXY(v:Vec) = sqrt(v.X * v.X  + v.Y * v.Y)

        /// Checks if a vector is vertical by doing:
        /// abs(v.X) + abs(v.Y) < zeroLengthTol
        /// fails on tiny (shorter than zeroLengthTol) vectors
        static member inline isVertical (v:Vec) =             
            if v.IsTiny(zeroLengthTol) then FsExGeoDivByZeroException.Raise "Vec Cannot not check very tiny vector for verticality %O" v
            abs(v.X) + abs(v.Y) < zeroLengthTol

        /// Checks if a vector is horizontal  by doing:
        /// abs(v.Z) < zeroLengthTol
        /// Fails on tiny (shorter than zeroLengthTol) vectors
        static member inline isHorizontal (v:Vec) =            
            if v.IsTiny(zeroLengthTol) then FsExGeoDivByZeroException.Raise "Vec Cannot not check very tiny vector for horizontality %O" v
            abs(v.Z) < zeroLengthTol  

        /// Returns positive or negative slope of a vector in Radians.
        /// In relation to X-Y plane.
        static member inline slopeRadians (v:Vec) = 
            let f = Vec(v.X, v.Y, 0.0)
            if v.Z >= 0.0 then  Vec.angleHalfPi v f
            else              -(Vec.angleHalfPi v f)

        /// Returns positive or negative slope of a vector in Degrees.
        /// In relation to X-Y plane.
        static member inline slopeDegrees (v:Vec) = 
            let f = Vec(v.X, v.Y, 0.0)
            if v.Z >= 0.0 then  Vec.angle90 v f
            else              -(Vec.angle90 v f)

        /// Returns positive or negative slope of a vector in Percent.
        /// In relation to X-Y plane.
        /// 100% = 45 Degrees
        static member inline slopePercent (v:Vec) = 
            if abs(v.Z) < zeroLengthTol then FsExGeoDivByZeroException.Raise "UnitVec.slopePercent: Can't get Slope from vertical vector %O" v
            let f = Vec(v.X, v.Y, 0.0)
            100.0 * (v.Z/f.Length)


        /// Reverse vector if Z part is smaller than 0.0
        static member inline orientUp (v:Vec) = 
            if v.Z < 0.0 then -v else v

        /// Reverse vector if Z part is bigger than 0.0
        static member inline orientDown (v:Vec) = 
            if v.Z < 0.0 then v else -v

        

        /// Returns a perpendicular horizontal vector. Rotated counterclockwise.        
        /// Just does Vec(-v.Y, v.X, 0.0) 
        /// On vertical input vector resulting vector if of zero length. 
        static member inline perpendicularInXY (v:Vec) = 
            Vec(-v.Y, v.X, 0.0) 


        /// Returns a vector that is perpendicular to the given vector and in the same vertical Plane.        
        /// Projected into the X-Y plane input and output vectors are parallel and of same orientation.
        /// Not of same length, not unitized.
        /// On vertical input vector resulting vector if of zero length. 
        static member inline perpendicularInVerticalPlane (v:Vec) = 
            let hor = Vec(v.Y, -v.X, 0.0)
            let r = Vec.cross (v, hor)            
            if v.Z < 0.0 then -r else r

        /// Multiplies a Matrix with a 3D vector (with an implicit 1 in the 4th dimension, 
        /// So that it also works correctly for projections.)
        static member transform (m:Matrix) (v:Vec) = 
            // from applyMatrix4( m ) in  https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js 
            let x = v.X
            let y = v.Y
            let z = v.Z
            //let w = 1.0           
            let x' = m.M11*x + m.M21*y + m.M31*z + m.X41 // * w
            let y' = m.M12*x + m.M22*y + m.M32*z + m.Y42 // * w
            let z' = m.M13*x + m.M23*y + m.M33*z + m.Z43 // * w
            let w' = m.M14*x + m.M24*y + m.M34*z + m.M44 // * w 
            let sc = 1.0 / w'           
            Vec(x' * sc, y'* sc, z'* sc)     
        
        /// Multiplies (or applies) an OrthoMatrix to a 3D Vector . 
        static member transformOrtho (m:OrthoMatrix) (v:Vec) = 
            let x = v.X
            let y = v.Y
            let z = v.Z 
            Vec(  m.M11*x + m.M21*y + m.M31*z + m.X41 
                , m.M12*x + m.M22*y + m.M32*z + m.Y42 
                , m.M13*x + m.M23*y + m.M33*z + m.Z43 
                )
        
        /// Multiplies (or applies) only the 3x3 rotation part of an OrthoMatrix to a 3D Unit Vector . 
        /// The resulting vector has the same length as the input.
        static member rotateOrtho (m:OrthoMatrix) (v:Vec) = 
            let x = v.X
            let y = v.Y
            let z = v.Z 
            Vec ( m.M11*x + m.M21*y + m.M31*z 
                , m.M12*x + m.M22*y + m.M32*z 
                , m.M13*x + m.M23*y + m.M33*z 
                )


