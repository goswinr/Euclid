namespace FsEx.Geo

open System

/// Members and operators for 2D Points, Vectors and Rotations
[<AutoOpen>]
module AutoOpenUnitVc = 
    open Util

    /// Returns distance between the tips of two vectoers
    let inline internal vecDist2(ax:float,ay:float,bx:float,by:float) =
        let x = bx-ax
        let y = by-ay
        sqrt(x*x+y*y)

    type UnitVc with 

        member inline v.WithX x = Vc (x ,v.Y) // returns new Vector with new x coordinate, y and z the same as before
        member inline v.WithY y = Vc (v.X, y)
        member inline v.WithZ z = Vec (v.X, v.Y, z)        

        /// Tests if dot product is bigger than 0.0
        member inline v.MatchesOrientation (vv:UnitVc) = v*vv > 0. // direction match

         /// 2D cross product. Its Just a scalar
        member inline a.Cross (b:Vc)     = a.X*b.Y - a.Y*b.X

        /// 2D cross product. Its Just a scalar
        member inline a.Cross (b:UnitVc) = a.X*b.Y - a.Y*b.X

        /// 90 degree rotation counter clockwise
        member inline v.RotatedCCW = UnitVc.createUnchecked( -v.Y,   v.X  )

        /// 90 degree rotation clockwise
        member inline v.RotatedCW  = UnitVc.createUnchecked(  v.Y,  -v.X  )  

        /// The diamond angle.
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve cosine or atan functions
        member inline v.DirectionDiamond =
            // https://stackoverflow.com/a/14675998/969070            
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
        member inline v.Direction2PI =
            // https://stackoverflow.com/a/14675998/969070
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Util.twoPi
            else  
                a

        /// Returns the Angle in Radians from XAxis, 
        /// Ignores orientation.
        /// Range 0.0 to Pi.
        member inline v.DirectionPI =
            // https://stackoverflow.com/a/14675998/969070            
            let a = Math.Atan2(v.Y, v.X) 
            if a < 0. then  
                a + Math.PI
            else  
                a
        
        /// Returns the Angle in Degrees from XAxis.  
        /// Going Counter clockwise till 360.
        member inline v.Direction360 =
            v.Direction2PI |> toDegrees
        
        /// Returns the Angle in Radians from XAxis, 
        /// Ignores orientation.
        /// Range 0.0 to 180.
        member inline v.Direction180 =
            v.DirectionPI |> toDegrees
        
        member inline v.AsPt         = Pt( v.X, v.Y)
        member inline v.AsVec        = Vec(v.X, v.Y, 0.0)
        member inline v.AsUnitVec    = UnitVec.createUnchecked(v.X, v.Y, 0.0)
        member inline v.AsPnt        = Pnt(v.X, v.Y, 0.0)
        member inline v.AsVecWithZ z = Vec(v.X, v.Y, z)
        member inline v.AsPntWithZ z = Pnt(v.X, v.Y, z)
     
        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        static member inline ofPt  (pt:Pt) =  
            let l = sqrt (pt.X*pt.X + pt.Y*pt.Y ) 
            if l <  zeroLenghtTol then FsExGeoDivByZeroException.Raise $"UnitVc.ofPt failed on too short %O{pt}"            
            UnitVc.createUnchecked( pt.X / l , pt.Y / l ) 
        
        static member inline ofVec  (v:Vc) = 
            let l = sqrt (v.X*v.X + v.Y*v.Y ) 
            if l <  zeroLenghtTol then FsExGeoDivByZeroException.Raise $"UnitVc.ofVc failed on too short %O{v}" 
            UnitVc.createUnchecked( v.X / l , v.Y / l ) 
        
        /// Does the unitizing
        static member inline create (x:float, y:float) = 
            let l = sqrt(x * x  + y * y)                     
            if l < zeroLenghtTol then FsExGeoDivByZeroException.Raise $"UnitVc.create: x:%g{x} and z:%g{y} are too small for creating a Unit vector, Tolerance:%g{zeroLenghtTol}"            
            UnitVc.createUnchecked( x/l , y/l )  
        
        static member inline make3D (v:UnitVc) = UnitVec.createUnchecked(v.X,v.Y, 0.0)

        static member inline XAxis  = UnitVc.createUnchecked (1.0 , 0.0)
        static member inline YAxis  = UnitVc.createUnchecked (0.0 , 1.0)

        /// 2D cross product. Its Just a scalar
        static member inline cross (a:UnitVc, b:UnitVc)      = a.X*b.Y - a.Y*b.X        
        /// 2D cross product. Its Just a scalar
        static member inline cross (a:UnitVc, b:Vc)  = a.X*b.Y - a.Y*b.X
        /// 2D cross product. Its Just a scalar
        static member inline cross (a:Vc, b:UnitVc)  = a.X*b.Y - a.Y*b.X
        
        /// dot product, or scalar product
        static member inline dot  (a:UnitVc, b:UnitVc  ) = a.X * b.X + a.Y * b.Y 
        /// dot product, or scalar product
        static member inline dot  (a:Vc, b:UnitVc  ) = a.X * b.X + a.Y * b.Y 
        /// dot product, or scalar product
        static member inline dot  (a:UnitVc, b:Vc  ) = a.X * b.X + a.Y * b.Y 
        
        static member inline getX     (v:UnitVc) = v.X
        static member inline getY     (v:UnitVc) = v.Y        
        static member inline setX     x (v:UnitVc) = v.WithX x
        static member inline setY     y (v:UnitVc) = v.WithY y
        static member inline setZ     z (v:UnitVc) = v.WithZ z
        static member inline add      (a:UnitVc) (b:UnitVc) = b + a  
        static member inline dirMatch (a:UnitVc) (b:UnitVc) = b.MatchesOrientation a
        static member inline scale    (f:float) (v:UnitVc) = Vc (v.X * f , v.Y * f )        
        static member inline shiftX     x (v:UnitVc) = Vc (v.X+x, v.Y)
        static member inline shiftY     y (v:UnitVc) = Vc (v.X,   v.Y+y)        
        static member inline setLength  f (v:UnitVc) = Vc (v.X * f , v.Y * f ) 

        /// Returns angle between two UnitVectors in Radians.
        /// Takes vector orientation into account.
        /// Ignores order of input vectors. anglePI(a,b) = anglePI(b,a)
        /// Range 0.0 to PI( = 0 to 180 degree)
        static member inline anglePI (a:UnitVc) (b:UnitVc) = 
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
                if dot < 0. then Math.PI - 2.0 * asin(vecDist2(-a.X,-a.Y,b.X,b.Y) * 0.5) 
                else                       2.0 * asin(vecDist2( a.X, a.Y,b.X,b.Y) * 0.5) 
            //let mutable r = b.Direction2PI  - a.Direction2PI  // this alternative calculation is about 10x slower for unit vectors            
            //if r < 0.      then  r <- r + Util.twoPi 
            //if r > Math.PI then  r <- Util.twoPi - r 
            //r


        /// Returns positive angle between two UnitVectors in Radians. 
        /// Ignores orientation. 
        /// Ignores order of input vectors. angleHalfPI(a,b) = angleHalfPI(b,a) = angleHalfPI(-b,a) = angleHalfPI(-b,-a)
        /// Range 0.0 to PI/2 ( = 0 to 90 degree)
        static member inline angleHalfPI (a:UnitVc) (b:UnitVc) = 
            let dot =  a * b
            let dotAbs = abs dot
            if dotAbs < 0.98 then  
                acos dotAbs 
            else
                if dot < 0. then 2.0 * asin(vecDist2(-a.X,-a.Y,b.X,b.Y) * 0.5)
                else             2.0 * asin(vecDist2( a.X, a.Y,b.X,b.Y) * 0.5)         
            //let mutable r = b.Direction2PI  - a.Direction2PI // this alternative calculation is about 10x slower for unit vectors              
            //if r < 0.      then  r <- r + Util.twoPi 
            //if r > Math.PI then  r <- Util.twoPi - r 
            //if r > halfPi  then  r <- Math.PI - r 
            //r


        /// Returns positive angle for rotating counter clockwise from Vector 'a' to Vector 'b' .
        /// In Radians.
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline angle2PI (a:UnitVc , b:UnitVc)   =              
            let r = b.Direction2PI  - a.Direction2PI            
            if r >= 0. then  r
            else r + Util.twoPi 

        /// Returns positive angle between two UnitVectors in Degrees,
        /// Ignores vector orientation.
        /// Ignores order of input vectors. angle90(a,b) = angle90(b,a) = angle90(-b,a) = angle90(-b,-a)
        /// Range: 0 to 90 degrees.
        static member inline angle90 (a:UnitVc) (b:UnitVc) = 
            UnitVc.angleHalfPI a b |>  toDegrees        
            

        /// Returns positive angle between two UnitVectors in Degrees. 
        /// Takes vector orientation into account. 
        /// Ignores order of input vectors. angle180(a,b) = angle180(b,a)
        /// Range 0 to 180 degrees.
        static member inline angle180 (a:UnitVc) (b:UnitVc) = 
            UnitVc.anglePI a b |>  toDegrees         


        /// Returns positive angle for rotating counter clockwise from Vector 'a' to Vector 'b' .
        /// In Degree.
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline angle360 (a:UnitVc, b:UnitVc)  = 
            UnitVc.angle2PI (a,b) |> toDegrees


        /// The diamond angle.
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve cosine or atan functions
        static member inline directionDiamond(a:UnitVc) =
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
        

        /// Returns positive angle of unit vector. Counter clockwise from X Axis.
        /// In Radians
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline direction2PI (v:UnitVc)   =              
            v.Direction2PI

        /// Returns positive angle of unit vector. Counter clockwise from X Axis.
        /// In Degree
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline direction360 (v:UnitVc)  = 
            v.Direction360
        

        /// Rotate the UnitVector in Degrees. Counter Clockwise.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate.
        static member inline rotate (angDegree) (vec:UnitVc) = 
            (Rotate.createFromDegrees angDegree).Rotate vec  

         

        //[<Obsolete("Unsave Member") >]
        //static member Zero = UnitVc ( 0. , 0.)  // needed by 'Array.sum' 
        //[<Obsolete("Unsave Member") >]
        //static member inline DivideByInt (v:UnitVc, i:int) = if i<>0 then v / float i else failwithf "DivideByInt 0 %O " v // needed by  'Array.average'  
        
        
        
        
        

