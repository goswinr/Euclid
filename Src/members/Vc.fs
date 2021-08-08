namespace FsEx.Geo
open System

/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type Vc
[<AutoOpen>]
module AutoOpenVc = 
    open Util

    type Vc with

        /// Returns a boolean indicating wether X andY  are exactly 0.0.
        member inline v.IsZero = v.X = 0.0 && v.Y = 0.0 
        
        /// Returns a boolean indicating wether the absolute value of X and Y is each less than the given tolerance.
        member inline v.IsTiny tol = abs v.X < tol && abs v.Y < tol  
        
        //member inline v.IsInValid =  Double.IsNaN v.X || Double.IsNaN v.Y || Double.IsNaN v.Z || Double.IsInfinity v.X || Double.IsInfinity v.Y || Double.IsInfinity v.Z
        
        /// Returns the length of the 2D vector 
        member inline v.Length = sqrt (v.X*v.X + v.Y*v.Y )
        
        /// Returns the squared length of the 2D vector 
        /// The square length is faster to calculate and often good enough for use cases such as sorting vectors by length.
        member inline v.LengthSq = v.X*v.X + v.Y*v.Y         

        /// Returns new 2D Vector with new X coordinate, Y stays the same.
        member inline v.WithX x = Vc (x ,v.Y) 

        /// Returns new 2D Vector with new Y coordinate, X stays the same.
        member inline v.WithY y = Vc (v.X, y)

        /// Returns new 3D Vector with Z coordinate, X and Y stay the same.
        /// If you want Z to be 0.0 you can use v.AsVec too.
        member inline v.WithZ z = Vec (v.X ,v.Y, z)
        
        /// Returns a new 2D Vector with half the length.
        member inline v.Half = Vc (v.X*0.5 ,v.Y*0.5)
    
        /// Tests if dot product is bigger than 0.0.
        /// That means the angle between the two vectors is less than 90 degrees.
        member inline v.MatchesOrientation (vv:Vc) = v*vv > 0. 

        member inline v.WithLength (desiredLength:float) =  
            let l = sqrt(v.X*v.X+v.Y*v.Y) 
            if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "Vc.WithLength %g : %O is too small for unitizing, Tolerance:%g" desiredLength v zeroLengthTol
            v*(desiredLength / l)  

        member inline v.Unitized  = 
            let l = sqrt(v.X * v.X  + v.Y * v.Y)
            // #if DEBUG add here too? TODO ?
            if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "%O is too small for unitizing, Tolerance:%g" v zeroLengthTol
            UnitVc.createUnchecked( v.X/l , v.Y/l)          
        
        member inline v.UnitizedUnchecked =  
            let l = sqrt(v.X*v.X + v.Y*v.Y) 
            UnitVc.createUnchecked( v.X/l , v.Y/l)   
        
        /// Test if the 2D vector is a unit vector. 
        /// Tests if square length is within 6 float steps of 1.0
        /// So between 0.99999964 and 1.000000715.
        member inline v.IsUnit   = 
            Util.isOne v.LengthSq

        /// 2D cross product. Its Just a scalar
        member inline a.Cross (b:Vc)     = a.X*b.Y - a.Y*b.X

        /// 2D cross product. Its Just a scalar
        member inline a.Cross (b:UnitVc) = a.X*b.Y - a.Y*b.X

        /// 90 degree rotation counter clockwise
        member inline v.RotatedCCW = Vc( -v.Y,   v.X )

        /// 90 degree rotation clockwise
        member inline v.RotatedCW  = Vc( v.Y,  -v.X )

        /// The diamond angle.
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve cosine or atan functions
        member inline v.DirectionDiamond =
            // https://stackoverflow.com/a/14675998/969070       
            #if DEBUG // TODO : with this test all  operations are 2.5 times slower 
            if v.IsTiny 1e-16  then FsExGeoException.Raise "FsEx.Geo.Vc.DirectionDiamond Failed for tiny Vector %O." v
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
        member inline v.Direction2PI =
            // https://stackoverflow.com/a/14675998/969070
            #if DEBUG // TODO : with this test all  operations are 2.5 times slower 
            if v.IsTiny 1e-16  then FsExGeoException.Raise "FsEx.Geo.Vc.Direction2PI Failed for tiny Vector %O." v
            #endif
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
            #if DEBUG // TODO : with this test all  operations are 2.5 times slower 
            if v.IsTiny 1e-16  then FsExGeoException.Raise "FsEx.Geo.Vc.DirectionPI Failed for tiny Vector %O." v
            #endif
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

        /// Returns positive angle for rotating counter clockwise from this Vector to Vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 degrees) 
        /// It is the fastest angle calculation since it does not involve cosine or atan functions
        member inline v.AngleDiamondTo (b:Vc)   =              
            let r = b.DirectionDiamond - v.DirectionDiamond          
            if r >= 0. then  r
            else r + 4.0 

        member inline v.AsPt         = Pt( v.X, v.Y)
        member inline v.AsVec        = Vec(v.X, v.Y, 0.0)
        member inline v.AsPnt        = Pnt(v.X, v.Y, 0.0)
        member inline v.AsVecWithZ z = Vec(v.X, v.Y, z)
        member inline v.AsPntWithZ z = Pnt(v.X, v.Y, z)

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Vc with X=0.0 and Y=0.0
        static member Zero = Vc(0.0 , 0.0)  // needed by 'Array.sum'
        
        static member inline XAxis  = Vc (1.0 , 0.0)
        static member inline YAxis  = Vc (0.0 , 1.0)   


        /// Accepts any type that has a X and Y (UPPERCASE) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofXY vec  = 
            let x = ( ^T : (member X : _) vec)
            let y = ( ^T : (member Y : _) vec)
            try Vc(float x, float y) 
            with e -> FsExGeoDivByZeroException.Raise "Vc.ofXY: %A could not be converted to a FsEx.Geo.Vc:\r\n%A" vec e

        /// Accepts any type that has a x and y (lowercase) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxy vec  = 
            let x = ( ^T : (member x : _) vec)
            let y = ( ^T : (member y : _) vec)
            try Vc(float x, float y) 
            with e -> FsExGeoDivByZeroException.Raise "Vc.ofxy: %A could not be converted to a FsEx.Geo.Vc:\r\n%A" vec e


        static member inline create   (start:Pt,ende:Pt) = ende-start  
        
        
        /// 2D cross product. Its Just a scalar
        static member inline cross (a:Vc, b:Vc)  = a.X*b.Y - a.Y*b.X        
        /// 2D cross product. Its Just a scalar
        static member inline cross (a:UnitVc, b:Vc)  = a.X*b.Y - a.Y*b.X
        /// 2D cross product. Its Just a scalar
        static member inline cross (a:Vc, b:UnitVc)  = a.X*b.Y - a.Y*b.X

        /// dot product, or scalar product
        static member inline dot  (a:Vc, b:Vc  ) = a.X * b.X + a.Y * b.Y 
        /// dot product, or scalar product
        static member inline dot  (a:Vc, b:UnitVc  ) = a.X * b.X + a.Y * b.Y 
        /// dot product, or scalar product
        static member inline dot  (a:UnitVc, b:Vc  ) = a.X * b.X + a.Y * b.Y 

        static member inline getX     (v:Vc) = v.X
        static member inline getY     (v:Vc) = v.Y
        static member inline setX     x (v:Vc) = v.WithX x
        static member inline setY     y (v:Vc) = v.WithY y
        static member inline add      (a:Vc) (b:Vc) = b + a
        static member inline sqLength (v:Vc) = v.LengthSq
        static member inline dirMatch (a:Vc) (b:Vc) = b.MatchesOrientation a
        static member inline scale    (f:float) (v:Vc) = v*f
        static member inline length       (v:Vc) = v.Length
        static member inline shiftX     x (v:Vc) = Vc (v.X+x, v.Y)
        static member inline shiftY     y (v:Vc) = Vc (v.X,   v.Y+y)    
        static member inline isTiny   tol (v:Vc) = v.IsTiny tol
        static member inline setLength  f (v:Vc) = v.WithLength f    

        /// Returns vector reversed
        static member inline reverse (v:Vc) = -v

        /// Returns vector unitized, fails on zero length vectors
        static member inline unitize (v:Vc) =  
            let l = sqrt((v.X * v.X)  + (v.Y * v.Y))            
            if l < zeroLengthTol then FsExGeoDivByZeroException.Raise "%O is too small for unitizing, Tolerance:%g" v zeroLengthTol
            UnitVc.createUnchecked( v.X/l , v.Y/l)
    
        /// Returns vector unitized or Vc(NaN,NaN,NaN) on zero length vectors
        static member inline unitizeUnChecked (v:Vc) = 
            v.UnitizedUnchecked
       
        /// Returns angle between two UnitVectors in Radians.
        /// Takes vector orientation into account.
        /// Range 0.0 to PI( = 0 to 180 degree)
        static member inline anglePI (a:Vc) (b:Vc) = 
            UnitVc.anglePI a.Unitized b.Unitized             
            //    let mutable r = b.Direction2PI  - a.Direction2PI     // this does perform slightly worse than using unitizing and acos for anglePI       
            //    if r < 0.      then  r <- r + Util.twoPi 
            //    if r > Math.PI then  r <- Util.twoPi - r 
            //    r

        /// Returns positive angle between two UnitVectors in Radians. Ignores orientation. 
        /// Range 0.0 to PI/2 ( = 0 to 90 degree)
        static member inline angleHalfPI (a:Vc) (b:Vc) = 
            UnitVc.angleHalfPI a.Unitized b.Unitized             
            //    let mutable r = b.Direction2PI  - a.Direction2PI   // this does perform slightly worse than using unitizing and acos for anglePI         
            //    if r < 0.      then  r <- r + Util.twoPi 
            //    if r > Math.PI then  r <- Util.twoPi - r 
            //    if r > halfPi  then  r <- Math.PI - r 
            //    r
        
        
        /// Returns positive angle for rotating  counter clockwise from Vector 'a' to Vector 'b' .
        /// In Radians
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline angle2PI (a:Vc , b:Vc)   =              
            let r = b.Direction2PI  - a.Direction2PI             
            if r >= 0. then  r
            else r + Util.twoPi 

        /// Returns positive angle between two UnitVectors in Degrees,
        /// Ignores vector orientation.
        /// Range: 0 to 90 degrees.
        static member inline angle90 (a:Vc) (b:Vc) = 
            Vc.angleHalfPI a b |>  toDegrees

        /// Returns positive angle between two UnitVectors in Degrees. 
        /// Takes vector orientation into account.
        /// Range 0 to 180 degrees.
        static member inline angle180 (a:Vc) (b:Vc) = 
            Vc.anglePI a b |>  toDegrees 
           
        
        /// Returns positive angle for rotating  counter clockwise from Vector 'a' to Vector 'b' .
        /// In Degree
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline angle360 (a:Vc, b:Vc)  = 
            Vc.angle2PI (a,b) |> toDegrees
        
        /// Returns positive angle for rotating counter clockwise from Vector 'a' to Vector 'b' .
        /// In Diamond Angle. Using only proportion of X to Y components.
        /// Range of 0.0 to 4.0 ( for 360 degrees) 
        /// It is the fastest angle calculation since it does not involve cosine or atan functions
        static member inline angleDiamond (a:Vc , b:Vc)   =              
            let r = b.DirectionDiamond - a.DirectionDiamond          
            if r >= 0. then  r
            else r + 4.0 

        /// The diamond angle.
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve cosine or atan functions
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

        /// Returns positive angle of vector. Counter clockwise from X Axis.
        /// In Radians
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline direction2PI (v:Vc)   =              
            v.Direction2PI

        /// Returns positive angle of vector. Counter clockwise from X Axis.
        /// In Degree
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline direction360 (v:Vc)  = 
            v.Direction360        

        /// Rotate the a 2D Vector Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateBy (r:Rotation2D) (v:Vc) = 
            Vc(r.Cos*v.X - r.Sin*v.Y, 
               r.Sin*v.X + r.Cos*v.Y) 

        /// Rotate the 2D Vector in Degrees. Counter Clockwise.
        /// For better Performance precomputed the Rotate2D struct and use its member to rotate. see Vc.rotateBy
        static member inline rotate (angDegree) (vec:Vc) = 
            Vc.rotateBy (Rotation2D.createFromDegrees angDegree) vec  

        
        //static member inline ( + )  (a:Vc, b:Vc) = Pnt (a.X + b.X , a.Y + b.Y , a.Z + b.Z) // required for Seq.average
        //static member inline DivideByInt (v:Vc, i:int) = if i<>0 then v / float i else failwithf "DivideByInt 0 %O " pt  // needed by  'Array.average'
        
        
        
        
        
        
        
        
        
        

