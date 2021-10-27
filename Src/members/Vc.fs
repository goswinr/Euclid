namespace FsEx.Geo
open System

/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type Vc
[<AutoOpen>]
module AutoOpenVc = 
    open Util

    type Vc with

        /// Returns a boolean indicating wether X and Y are exactly 0.0.
        member inline v.IsZero = v.X = 0.0 && v.Y = 0.0         
        
        /// Returns a boolean indicating if any of  X and Y is not exactly 0.0.
        member inline v.IsNotZero =  v.X <> 0.0 || v.Y <> 0.0   

        /// Returns a boolean indicating wether the absolute value of X and Y is each less than the given tolerance.
        member inline v.IsTiny tol = abs v.X < tol && abs v.Y < tol          
    
        /// Returns the length of the 2D vector 
        member inline v.Length = sqrt (v.X*v.X + v.Y*v.Y )
        
        /// Returns the squared length of the 2D vector 
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSq = v.X*v.X + v.Y*v.Y         

        /// Returns new 2D vector with new X coordinate, Y stays the same.
        member inline v.WithX x = Vc (x ,v.Y) 

        /// Returns new 2D vector with new Y coordinate, X stays the same.
        member inline v.WithY y = Vc (v.X, y)

        /// Returns new 3D vector with Z coordinate, X and Y stay the same.
        /// If you want Z to be 0.0 you can use v.AsVec too.
        member inline v.WithZ z = Vec (v.X ,v.Y, z)
        
        /// Returns a new 2D vector with half the length.
        member inline v.Half = Vc (v.X*0.5 ,v.Y*0.5)
    
        /// Returns a new 2D vector scaled to the desired length.
        /// Same as Vc.setLength
        member inline v.WithLength (desiredLength:float) =  
            let l = v.Length
            if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "Vc.WithLength %g : %O is too small for unitizing, Tolerance:%g" desiredLength v zeroLengthTol
            v*(desiredLength / l)  

        /// Returns the 2D vector unitized.
        /// Fails with FsExGeoDivByZeroException if the length of the vector is 
        /// too small (1-e16) to unitize. 
        member inline v.Unitized  = 
            let l = sqrt(v.X * v.X  + v.Y * v.Y)
            // #if DEBUG add here too? TODO ?
            if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "%O is too small for unitizing, Tolerance:%g" v zeroLengthTol
            UnitVc.createUnchecked( v.X/l , v.Y/l)       
        
        /// Test if the 2D vector is a unit vector. 
        /// Tests if square length is within 6 float steps of 1.0
        /// So between 0.99999964 and 1.000000715.
        member inline v.IsUnit   = 
            Util.isOne v.LengthSq

        /// 2D cross product. 
        /// Its Just a scalar equal to the area of the parallelogram spanned by the input vectors.
        member inline a.Cross (b:Vc)     = a.X*b.Y - a.Y*b.X

        /// 2D cross product. 
        /// Its Just a scalar equal to the area of the parallelogram spanned by the input vectors.
        member inline a.Cross (b:UnitVc) = a.X*b.Y - a.Y*b.X

        /// 90 Degree rotation counter clockwise
        member inline v.Rotate90CCW = Vc( -v.Y,   v.X )

        /// 90 Degree rotation clockwise
        member inline v.Rotate90CW  = Vc( v.Y,  -v.X )

        /// The diamond angle.
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions
        member inline v.DirectionDiamond =
            // https://stackoverflow.com/a/14675998/969070 
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vc.DirectionDiamond: input vector is zero length: %O" v
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
        member inline v.Direction2Pi =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG 
            if abs(v.X) < zeroLengthTol && abs(v.Y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Vc.Direction2Pi: input vector is zero length: %O" v
            #endif
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Util.twoPi
            else  
                a
        
        /// Returns the Angle in Radians from XAxis, 
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        member inline v.DirectionPi =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG // TODO : with this test all  operations are 2.5 times slower 
            if v.IsTiny 1e-16  then FsExGeoException.Raise "FsEx.Geo.Vc.DirectionPi Failed for tiny vector %O." v
            #endif
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Math.PI
            else  
                a

        /// Returns the Angle in Degrees from XAxis.  
        /// Going Counter clockwise till 360.
        member inline v.Direction360 =
            v.Direction2Pi |> toDegrees
        
        /// Returns the Angle in Radians from XAxis, 
        /// Ignores orientation.
        /// Range 0.0 to 180.
        member inline v.Direction180 =
            v.DirectionPi |> toDegrees

        /// Returns positive angle for rotating counter clockwise from this vector to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 Degrees) 
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions
        member inline v.AngleDiamondTo (b:Vc)   =              
            let r = b.DirectionDiamond - v.DirectionDiamond          
            if r >= 0. then  r
            else r + 4.0 

        /// Convert 2D vector to 2D Point
        member inline v.AsPt         = Pt( v.X, v.Y)
        
        /// Convert 2D vector to 3D vector using 0.0 as Z value
        /// If you want a different Z value use the member v.WithZ(z)
        member inline v.AsVec        = Vec(v.X, v.Y, 0.0)
        
        /// Convert 2D vector to 3D point using 0.0 as Z value. 
        member inline v.AsPnt        = Pnt(v.X, v.Y, 0.0)

        /// Checks if the angle between the two 2D vectors is less than 180 degrees.
        /// Calculates the dot product of two 2D vectors. 
        /// Then checks if it is positive.
        member inline v.MatchesOrientation180  (other:Vc) = 
            v * other > 0.0  

        /// Checks if the angle between the two 2D vectors is less than 90 degrees.   
        /// Calculates the dot product of the two 2D vectors unitized. 
        /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
        member inline v.MatchesOrientation90  (other:Vc) = 
            v.Unitized * other.Unitized > 0.707107
            
        /// Checks if two 2D vectors are parallel. Ignoring orientation
        /// Calculates the cross product of the two 2D vectors. (= the area of the parallelogram)
        /// And checks if it is smaller than 1e-9
        /// (NOTE: for very long 2D vectors a higher tolerance might be needed)
        member inline v.IsParallelTo  (other:Vc) =                     
            // 2D cross product. 
            // Its Just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
            abs(v.X*other.Y - v.Y*other.X) < 1e-9 

        /// Checks if two 2D vectors are parallel and orientated the same way.
        /// Calculates the cross product of the two 2D vectors. (= the area of the parallelogram)
        /// And checks if it is smaller than 1e-9
        /// Then calculates the dot product and checks if it is positive.
        /// (NOTE: for very long 2D vectors a higher tolerance might be needed)
        member inline v.IsParallelAndOrientedTo  (other:Vc) =
            abs(v.X*other.Y - v.Y*other.X) < 1e-9  
            && 
            v.X*other.X + v.Y*other.Y > 0.0 
            
        /// Checks if two 2D vectors are perpendicular. 
        /// Calculates the dot product and checks if it is smaller than 1e-9.
        /// (NOTE: for very long 2D vectors a higher tolerance might be needed)
        member inline v.IsPerpendicularTo (other:Vc) =     
            abs(v.X*other.X + v.Y*other.Y) < 1e-9 


        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Returns the World X-axis with length one: Vc(1,0)
        static member inline XAxis  = Vc(1,0)

        /// Returns the World Y-axis with length one: Vc(0,1)
        static member inline YAxis  = Vc(0,1)
        
        /// Returns a zero length vector: Vec(0,0)
        static member inline Zero   = Vc(0,0)  // this member is needed by Seq.sum, so that it doesn't fail on empty seq.  
        
        /// Returns the distance between the tips of two 2D vectors.
        static member inline difference (a:Vc) (b:Vc) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y )
                
        /// Returns the squared distance between the tips of two 2D vectors.
        /// This operation is slightly faster than Vec.difference  and sufficient for many algorithms like finding closest points.
        static member inline differenceSq (a:Vc) (b:Vc) = let v = a-b in  v.X*v.X + v.Y*v.Y 

        /// Divides the vector by an integer.
        /// (This member is needed by Array.average and similar functions)
        static member inline DivideByInt (v:Vc, i:int) = // needed by 'Array.average'
            if i<>0 then v / float i 
            else FsExGeoDivByZeroException.Raise "FsEx.geo.Vc.DivideByInt is zero %O " v 
        
        /// Accepts any type that has a X and Y (UPPERCASE) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofXY vec  = 
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            try Vc(float x, float y) 
            with e -> FsExGeoException.Raise "Vc.ofXY: %A could not be converted to a FsEx.Geo.Vc:\r\n%A" vec e

        /// Accepts any type that has a x and y (lowercase) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxy vec  = 
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            try Vc(float x, float y) 
            with e -> FsExGeoException.Raise "Vc.ofxy: %A could not be converted to a FsEx.Geo.Vc:\r\n%A" vec e


         /// Create 2D vector from 2D point. 
        static member inline ofPt  (pt:Pnt) =  Vc( pt.X , pt.Y ) 
        
        /// Create 2D vector from 2D unit vector.
        static member inline ofUnitVc (v:UnitVc) =  Vc(v.X, v.Y)       
        
        /// Convert 2D vector to 2D point. 
        static member inline asPt(v:Vc)  = Pt( v.X, v.Y)
        
        /// Convert 2D vector to 3D point. Using 0.0 as Z value.
        static member inline asPnt(v:Vc) = Pnt(v.X, v.Y, 0.0) 

        /// Convert 2D vector to 3D vector. Using 0.0 as Z value.
        static member inline asVec(v:Vc) = Vec(v.X, v.Y, 0.0)        
        
        /// 2D Cross product, of two 2D vectors.
        /// Its Just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
        static member inline cross (a:Vc, b:Vc)  = a.X*b.Y - a.Y*b.X        
        
        /// 2D Cross product, of a 2D unit vectors an a 2D vector. 
        /// Its Just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
        static member inline cross (a:UnitVc, b:Vc)  = a.X*b.Y - a.Y*b.X
        
        /// 2D Cross product, of a 2D vectors an a 2D unit vector. 
        /// Its Just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
        static member inline cross (a:Vc, b:UnitVc)  = a.X*b.Y - a.Y*b.X

        /// Dot product, or scalar product of two 2D vectors. 
        /// Returns a float. 
        static member inline dot  (a:Vc, b:Vc  ) = a.X * b.X + a.Y * b.Y 
        
        /// Dot product, or scalar product of a 2D unit vector with a 2D vector  
        /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit vector
        static member inline dot  (a:Vc, b:UnitVc  ) = a.X * b.X + a.Y * b.Y 
        
        /// Dot product, or scalar product of a 2D vector with a 2D unit vector  
        /// Returns a float. This float is the projected length of the 2D vector on the direction of the unit vector
        static member inline dot  (a:UnitVc, b:Vc  ) = a.X * b.X + a.Y * b.Y 

        /// Gets the X part of this 2D vector
        static member inline getX  (v:Vc) = v.X
        
        /// Gets the Y part of this 2D vector
        static member inline getY (v:Vc) = v.Y        
        
        /// Returns new 2D vector with new X value,  Y stays the same.
        static member inline setX  x (v:Vc) = v.WithX x
        
        /// Returns new 2D vector with new Y value, X  stays the same.
        static member inline setY  y (v:Vc) = v.WithY y        
                
        /// Add two 2D vectors together. Returns a new 2D vector.
        static member inline add (a:Vc) (b:Vc) = b + a   
        
        /// Multiplies a 2D vector with a scalar, also called scaling a vector. 
        /// Same as Vc.setLength. Returns a new 2D vector.
        static member inline scale  (f:float) (v:Vc) = Vc (v.X * f , v.Y * f )    

        /// Returns a new 3D vector scaled to the desired length.
        /// Same as vc.WithLength. Returns a new 3D vector.
        static member inline setLength(desiredLength:float) (v:Vc) = 
            let l = v.Length
            if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "Vc.setLength %g : %O is too small for unitizing, Tolerance:%g" desiredLength v zeroLengthTol
            v * (desiredLength / l)  
        
        /// Add to the X part of this 2D vectors together. Returns a new 2D vector.
        static member inline moveX x (v:Vc) = Vc (v.X+x, v.Y)
        
        /// Add to the Y part of this 2D vectors together. Returns a new 2D vector.
        static member inline moveY y (v:Vc) = Vc (v.X,   v.Y+y)
        
        /// Returns a boolean indicating wether the absolute value of X and Y is each less than the given tolerance.
        static member inline isTiny tol (v:Vc) = v.IsTiny tol

        /// Returns the length of the 2D vector 
        static member inline length  (v:Vc) = v.Length

        /// Returns the squared length of the 2D vector 
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        static member inline lengthSq (v:Vc) = v.LengthSq
        
        /// Returns a new 2D vector from X and Y parts.
        static member inline create (x:float, y:float) =  Vc( x , y  )
        
        /// Returns a new 2D vector from start and end point.
        static member inline create (start:Pt,ende:Pt) = ende-start  

        /// Negate or inverse a 2D vectors. Returns a new 2D vector. 
        /// Same as Vec.flip
        static member inline reverse  (v:Vc) = -v   
        
        /// Negate or inverse a 2D vectors. Returns a new 2D vector. 
        /// Same as Vec.reverse
        static member inline flip  (v:Vc) = -v

        /// Returns 2D vector unitized, fails on zero length vectors
        static member inline unitize (v:Vc) =  v.Unitized
        
        /// Unitize 2D vector, if input vector is shorter than 1e-6 the default Unit vector is returned.
        static member inline unitizeOrDefault (defaultUnitVector:UnitVc) (v:Vc) = 
            let l = v.LengthSq
            if l < 1e-12  then  // = sqrt (1e-06)
                defaultUnitVector 
            else
                let f = 1.0 / sqrt(l)
                UnitVc.createUnchecked(v.X*f , v.Y*f ) 

        /// Returns angle between two 2D unit vectors in Radians.
        /// Takes vector orientation into account.
        /// Range 0.0 to Pi( = 0 to 180 Degree)
        static member inline anglePi (a:Vc) (b:Vc) = 
            UnitVc.anglePi a.Unitized b.Unitized             
            //    let mutable r = b.Direction2Pi  - a.Direction2Pi     // this does perform slightly worse than using unitizing and acos for anglePi       
            //    if r < 0.      then  r <- r + Util.twoPi 
            //    if r > Math.PI then  r <- Util.twoPi - r 
            //    r

        /// Returns positive angle between two 2D unit vectors in Radians. Ignores orientation. 
        /// Range 0.0 to Pi/2 ( = 0 to 90 Degree)
        static member inline angleHalfPi (a:Vc) (b:Vc) = 
            UnitVc.angleHalfPi a.Unitized b.Unitized             
            //    let mutable r = b.Direction2Pi  - a.Direction2Pi   // this does perform slightly worse than using unitizing and acos for anglePi         
            //    if r < 0.      then  r <- r + Util.twoPi 
            //    if r > Math.PI then  r <- Util.twoPi - r 
            //    if r > halfPi  then  r <- Math.PI - r 
            //    r
        
        
        /// Returns positive angle for rotating  counter clockwise from vector 'a' to vector 'b' .
        /// In Radians
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline angle2Pi (a:Vc , b:Vc)   =              
            let r = b.Direction2Pi  - a.Direction2Pi             
            if r >= 0. then  r
            else r + Util.twoPi 

        /// Returns positive angle between two 2D unit vectors in Degrees,
        /// Ignores vector orientation.
        /// Range: 0 to 90 Degrees.
        static member inline angle90 (a:Vc) (b:Vc) = 
            Vc.angleHalfPi a b |>  toDegrees

        /// Returns positive angle between two 2D unit vectors in Degrees. 
        /// Takes vector orientation into account.
        /// Range 0 to 180 Degrees.
        static member inline angle180 (a:Vc) (b:Vc) = 
            Vc.anglePi a b |>  toDegrees 
            
        
        /// Returns positive angle for rotating  counter clockwise from vector 'a' to vector 'b' .
        /// In Degree
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline angle360 (a:Vc, b:Vc)  = 
            Vc.angle2Pi (a,b) |> toDegrees
        
        /// Returns positive angle for rotating counter clockwise from vector 'a' to vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 Degrees) 
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions
        static member inline angleDiamond (a:Vc , b:Vc)   =              
            let r = b.DirectionDiamond - a.DirectionDiamond          
            if r >= 0. then  r
            else r + 4.0 

        /// The diamond angle.
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions
        static member inline directionDiamond(a:Vc) =
            // https://stackoverflow.com/a/14675998/969070           
            if a.Y >= 0.0 then 
                if a.X >= 0.0 then   
                    a.Y/(a.X+a.Y) 
                else             
                    1.0 - a.X/(-a.X+a.Y)
            else
                if a.X < 0.0 then   
                    2.0 - a.Y/(-a.X-a.Y) 
                else 
                    3.0 + a.X/(a.X-a.Y) 

        /// Returns positive angle of vector. Counter clockwise from X-axis.
        /// In Radians
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline direction2Pi (v:Vc)   =              
            v.Direction2Pi

        /// Returns positive angle of vector. Counter clockwise from X-axis.
        /// In Degree
        /// Range: 0.0 to 2 Pi ( = 0 to 360 Degrees)
        static member inline direction360 (v:Vc)  = 
            v.Direction360    

        /// Returns a (not unitized) bisector vector in the middle direction. 
        /// Code : a.Unitized + b.Unitized
        static member inline bisector (a:Vc) (b:Vc) = a.Unitized + b.Unitized 

        /// Ensure vector has a positive dot product with given orientation vector
        static member inline matchOrientation (orientationToMatch:Vc) (v:Vc) = 
            if orientationToMatch * v < 0.0 then -v else v  

        /// Checks if the angle between the two 2D vectors is less than 180 degrees.
        /// Calculates the dot product of two 2D vectors. 
        /// Then checks if it is positive.
        static member inline matchesOrientation180  (v:Vc) (other:Vc) = 
            v * other > 0.0  

        /// Checks if the angle between the two 2D vectors is less than 90 degrees.   
        /// Calculates the dot product of the two 2D vectors unitized. 
        /// Then checks if it is bigger than 0.707107 (cosine of  90 degrees).
        static member inline matchesOrientation90  (v:Vc)  (other:Vc) = 
            v.Unitized * other.Unitized > 0.707107
            
        /// Checks if two 2D vectors are parallel. Ignoring orientation
        /// Calculates the cross product of the two 2D vectors. (= the area of the parallelogram)
        /// And checks if it is smaller than 1e-9
        /// (NOTE: for very long 2D vectors a higher tolerance might be needed)
        static member inline isParallelTo  (v:Vc)  (other:Vc) =                     
            // 2D cross product. 
            // Its Just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
            abs(v.X*other.Y - v.Y*other.X) < 1e-9 

        /// Checks if two 2D vectors are parallel and orientated the same way.
        /// Calculates the cross product of the two 2D vectors. (= the area of the parallelogram)
        /// And checks if it is smaller than 1e-9
        /// Then calculates the dot product and checks if it is positive.
        /// (NOTE: for very long 2D vectors a higher tolerance might be needed)
        static member inline isParallelAndOrientedTo  (v:Vc)  (other:Vc) =
            abs(v.X*other.Y - v.Y*other.X) < 1e-9  
            && 
            v.X*other.X + v.Y*other.Y > 0.0 
            
        /// Checks if two 2D vectors are perpendicular. 
        /// Calculates the dot product and checks if it is smaller than 1e-9.
        /// (NOTE: for very long 2D vectors a higher tolerance might be needed)
        static member inline isPerpendicularTo (v:Vc)  (other:Vc) =     
            abs(v.X*other.X + v.Y*other.Y) < 1e-9     


        /// Rotate the a 2D vector Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateBy (r:Rotation2D) (v:Vc) = 
            Vc(r.Cos*v.X - r.Sin*v.Y, 
               r.Sin*v.X + r.Cos*v.Y) 

        /// Rotate the 2D vector in Degrees. Counter Clockwise.
        /// For better Performance precomputed the Rotate2D struct and use its member to rotate. see Vc.rotateBy
        static member inline rotate (angDegree) (vec:Vc) = 
            Vc.rotateBy (Rotation2D.createFromDegrees angDegree) vec  

        
        /// 90 Degree rotation counter clockwise
        static member inline rotate90CCW (v:UnitVc) = UnitVc.createUnchecked( -v.Y,   v.X  )

        /// 90 Degree rotation clockwise
        static member inline rotate90CW (v:UnitVc) = UnitVc.createUnchecked(  v.Y,  -v.X  )  

            
        /// Checks if Angle between two vectors is Below one Degree
        /// Ignores vector orientation
        /// Fails on zero length vectors, tolerance 1e-12
        static member inline isAngleBelow1Degree(a:Vc, b:Vc) = 
            let sa = a.LengthSq
            if sa < 1e-6 then
                FsExGeoException.Raise "FsEx.Geo.Vc Duplicate points: isAngleBelow1Degree: prevPt - thisPt: %s.LengthSq < 1e-6; nextPt - thisPt:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if sb < 1e-6 then
                FsExGeoException.Raise "FsEx.Geo.Vc Duplicate points: isAngleBelow1Degree: nextPt - thisPt: %s.LengthSq < 1e-6; prevPt - thisPt:%s " b.AsString a.AsString
            let lena = sqrt sa
            let lenb = sqrt sb
            if lena < 1e-5 then
                FsExGeoException.Raise "FsEx.Geo.Vc Duplicate points: isAngleBelow1Degree: prevPt - thisPt: %s < 1e-5: %f; nextPt - thisPt:%s " a.AsString 1e-5 b.AsString
            if lenb < 1e-5 then
                FsExGeoException.Raise "FsEx.Geo.Vc Duplicate points: isAngleBelow1Degree: nextPt - thisPt: %s < 1e-5: %f; prevPt - thisPt:%s " b.AsString 1e-5 a.AsString
            let au = a * (1.0 / lena)
            let bu = b * (1.0 / lenb)
            abs(bu*au) > 0.999847695156391 // = cosine of 1 degree (2 degrees would be =  0.999390827019096)

            

        /// Checks if Angle between two vectors is Below 0.25 Degrees
        /// Ignores vector orientation
        /// Fails on zero length vectors, tolerance 1e-12
        static member inline isAngleBelowQuatreDegree(a:Vc, b:Vc) =          
            let sa = a.LengthSq
            if sa < 1e-6 then
                FsExGeoException.Raise "FsEx.Geo.Vc Duplicate points: isAngleBelowQuatreDegree: prevPt - thisPt: %s.LengthSq < 1e-6; nextPt - thisPt:%s " a.AsString b.AsString
            let sb = b.LengthSq
            if sb < 1e-6 then
                FsExGeoException.Raise "FsEx.Geo.Vc Duplicate points: isAngleBelowQuatreDegree: nextPt - thisPt: %s.LengthSq < 1e-6; prevPt - thisPt:%s " b.AsString a.AsString
            let lena = sqrt sa
            let lenb = sqrt sb
            if lena < 1e-5 then
                FsExGeoException.Raise "FsEx.Geo.Vc Duplicate points: isAngleBelowQuatreDegree: prevPt - thisPt: %s < 1e-5: %f; nextPt - thisPt:%s " a.AsString 1e-5 b.AsString
            if lenb < 1e-5 then
                FsExGeoException.Raise "FsEx.Geo.Vc Duplicate points: isAngleBelowQuatreDegree: nextPt - thisPt: %s < 1e-5: %f; prevPt - thisPt:%s " b.AsString 1e-5 a.AsString
            let au = a * (1.0 / lena)
            let bu = b * (1.0 / lenb)
            abs(bu*au) > 0.999990480720734 // = cosine of 0.25 degree: printfn "%.18f" (cos( 0.25 * (System.Math.PI / 180.))) // for fsi: printfn "%.18f" (cos( 0.25 * (System.Math.PI / 180.)))



