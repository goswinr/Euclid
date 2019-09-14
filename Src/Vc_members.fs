namespace FsEx.Geo
open System

/// Members and operators for 2D Points, Vectors and Rotations
[<AutoOpen>]
module AutoOpenVc = 
    open Util

    type Vc with

        member inline v.IsZero = v.X = 0.0 && v.Y = 0.0 
        member inline v.IsTiny tol = abs v.X < tol && abs v.Y < tol  
        //member inline v.IsInValid =  Double.IsNaN v.X || Double.IsNaN v.Y || Double.IsNaN v.Z || Double.IsInfinity v.X || Double.IsInfinity v.Y || Double.IsInfinity v.Z

        member inline v.Length = sqrt (v.X*v.X + v.Y*v.Y )
        /// Length Squared
        member inline v.LengthSq = v.X*v.X + v.Y*v.Y         

        member inline v.WithX x = Vc (x ,v.Y) // returns new Vector with new x coordinate, y and z the same as before
        member inline v.WithY y = Vc (v.X, y)
        member inline v.WithZ z = Vec (v.X ,v.Y, z)
    
        member inline v.Half = Vc (v.X*0.5 ,v.Y*0.5)
    
        /// Test for positive Dot product
        member inline v.MatchesOrientation (vv:Vc) = v*vv > 0. // direction match

        member inline v.WithLength (desiredLength:float) =  
            let l = sqrt(v.X*v.X+v.Y*v.Y) 
            if l < zeroLenghtTol then FsExGeoDivByZeroException.Raise $"Vc.WithLength %g{desiredLength} : %O{v} is too small for unitizing, Tolerance:%g{zeroLenghtTol}"  
            v*(desiredLength / l)  

        member inline v.Unitized  = 
            let l = sqrt(v.X * v.X  + v.Y * v.Y)
            // #if DEBUG add here too? TODO ?
            if l < zeroLenghtTol then FsExGeoDivByZeroException.Raise $"%O{v} is too small for unitizing, Tolerance:%g{zeroLenghtTol}"  
            UnitVc.createUnchecked( v.X/l , v.Y/l)          
        
        member inline v.UnitizedUnchecked =  
            let l = sqrt(v.X*v.X + v.Y*v.Y) 
            UnitVc.createUnchecked( v.X/l , v.Y/l)   
        
        member inline v.IsUnit   = 
            let l = v.Length 
            0.999999999 < l && 
            1.000000001 > l

        /// 2D cross product. Its Just a scalar
        member inline a.Cross (b:Vc)     = a.X*b.Y - a.Y*b.X

        /// 2D cross product. Its Just a scalar
        member inline a.Cross (b:UnitVc) = a.X*b.Y - a.Y*b.X

        /// 90 degree rotation counter clockwise
        member inline v.RotatedCCW = Vc( -v.Y,   v.X )

        /// 90 degree rotation clockwise
        member inline v.RotatedCW  = Vc( v.Y,  -v.X )


        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Vc with X=0.0 and Y=0.0
        static member Zero = Vc(0.0 , 0.0)  // needed by 'Array.sum'
        
        static member inline XAxis  = Vc (1.0 , 0.0)
        static member inline YAxis  = Vc (0.0 , 1.0)   

        //static member inline ofPt     (v:Pt)     = Vc (v.X, v.Y) 
        //static member inline ofUnitVc (v:UnitVc) = Vc (v.X, v.Y)
        //static member inline create (x:float, y:float) =  Vec( x , y )
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
            // #if DEBUG add here too? TODO ?
            if l < zeroLenghtTol then FsExGeoDivByZeroException.Raise $"%O{v} is too small for unitizing, Tolerance:%g{zeroLenghtTol}"  
            UnitVc.createUnchecked( v.X/l , v.Y/l)
    
        /// Returns vector unitized or Vc(NaN,NaN,NaN) on zero length vectors
        static member inline unitizeUnChecked (v:Vc) = v.UnitizedUnchecked
       
        /// Returns angle between two UnitVectors in Radians.
        /// Takes vector orientation into account.
        /// Range 0.0 to PI( = 0 to 180 degree)
        static member inline anglePi (a:Vc) (b:Vc) = 
            UnitVc.anglePi a.Unitized b.Unitized

        /// Returns positive angle between two UnitVectors in Radians. Ignores orientation. 
        /// Range 0.0 to PI/2 ( = 0 to 90 degree)
        static member inline angleHalfPi (a:Vc) (b:Vc) = 
            UnitVc.angleHalfPi a.Unitized b.Unitized
        

        /// Returns positive angle between two UnitVectors in Degrees. 
        /// Takes vector orientation into account.
        /// Range 0 to 180 degrees.
        static member inline angle180 (a:Vc) (b:Vc) = 
            UnitVc.anglePi a.Unitized b.Unitized |>  toDegrees 


        /// Returns positive angle between two UnitVectors in Degrees,
        /// Ignores vector orientation.
        /// Range: 0 to 90 degrees.
        static member inline angle90 (a:Vc) (b:Vc) = 
            UnitVc.angleHalfPi a.Unitized b.Unitized |>  toDegrees 
        

     
        
        //static member inline ( + )  (a:Vc, b:Vc) = Pnt (a.X + b.X , a.Y + b.Y , a.Z + b.Z) // required for Seq.average
        //static member inline DivideByInt (v:Vc, i:int) = if i<>0 then v / float i else failwithf "DivideByInt 0 %O " pt  // needed by  'Array.average'
        
        
        
        
        
        
        
        
        
        

