#if COMPILED
namespace Geometry3D
#endif
open System

(* module Units = 
        
    /// Degree (of Angle) 
    [<Measure>] type deg 
    
    /// Radians 
    [<Measure>] type rad 
    
    /// Converts Angels from Degrees to Radians
    let inline toRadians (degrees:float<deg>)  : float<rad> =  0.0174532925199433<rad/deg> * degrees 

    /// Converts Angels from Radians to Degrees
    let inline toDegrees (radians: float<rad>) :float<deg>  = 57.2957795130823<deg/rad> * radians 
    
    *) 

#nowarn "44" // for hidden constructors via Obsolete Attribute

/// 3D Points, Vectors and Rotations
module Vectors =  
    
    /// Converts Angels from Degrees to Radians
    let inline toRadians (degrees:float)  : float =  0.0174532925199433 * degrees 

    /// Converts Angels from Radians to Degrees
    let inline toDegrees (radians: float) :float  = 57.2957795130823 * radians 
    
    /// Tolerance for zero length: 1e-12 
    let [<Literal>] zeroLenghtTol = 1e-12
    
    /// A 3D Point (2D Points are called 'Pt') 
    [<Struct; NoEquality; NoComparison>]
    type Pnt = 
        val X : float
        val Y : float 
        val Z : float
        new (x,y,z) = 
            //if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then 
            //    failwithf "Pnt Constructor failed for x:%g , y:%g, z:%g" x y z  // with this test all Pnt operations are 2.5 times slower
            {X=x; Y=y; Z=z} 
        
        override pt.ToString() =  
            //sprintf "Pnt(X=%g; Y=%g; Z=%g)" pt.X pt.Y pt.Z
            sprintf "Pnt(X: %.04f, Y: %.04f, Z: %.04f)" pt.X pt.Y pt.Z 
    
    /// A 3D Vector (2D Vectors are called 'Vc') 
    [<Struct; NoEquality; NoComparison>] 
    type Vec = 
        val X : float
        val Y : float 
        val Z : float
        
        new (x,y,z) = 
            //if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then 
            //    failwithf "Vec Constructor failed for x:%g , y:%g, z:%g" x y z  // with this test all Vec operations are 2.5 times slower
            {X=x; Y=y; Z=z} 
        
        override v.ToString() =  
            //sprintf "Vec(X=%g; Y=%g; Z=%g)" v.X v.Y v.Z
            sprintf "Vec(X: %.04f, Y: %.04f, Z: %.04f)" v.X v.Y v.Z    
        
        member inline v.Length = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z) 
        
    /// A 3D Vector guaranteed to be unitized (2D Unit Vectors are called 'UnitVc') 
    [<Struct; NoEquality; NoComparison>]
    type UnitVec =  
        val X : float
        val Y : float 
        val Z : float
        
        /// Unsave internal constructor,  public only for inlining.
        [<Obsolete("Unsave internal constructor,  but must be public for inlining. So marked Obsolete instead") >]
        new (x,y,z) =  
            //if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z || Double.IsInfinity x || Double.IsInfinity y || Double.IsInfinity z then 
            //    failwithf "UnitVec Constructor failed for x:%g , y:%g, z:%g" x y z  // with this test all Vec operations are 2.5 times slower
            {X=x; Y=y; Z=z} 
        
        override v.ToString() =  
            //sprintf "UnitVec(X=%g; Y=%g; Z=%g)" v.X v.Y v.Z
            sprintf "UnitVec(X: %.04f, Y: %.04f, Z: %.04f)" v.X v.Y v.Z
    
    /// 2D Rotations in X,  Y or Z plane.
    [<Struct; NoEquality; NoComparison>]
    type Rotate2D = 
        val sin : float
        val cos : float
        /// Unsave internal constructor,  public only for inlining.
        //[<Obsolete("Unsave internal constructor,  public only for inlining.") >]
        new (sin, cos) = {sin = sin; cos = cos} 
        
        override r.ToString() =  
            let valid = let sum = r.sin*r.sin + r.cos*r.cos in 0.99999999 < sum && sum < 1.00000001
            if valid then sprintf "Rotate2D( %g Degrees)"        (r.sin|>Math.Asin|> toDegrees)  
            else sprintf " an invalid Rotate2D(sin %g; cos %g) " (r.sin|>Math.Asin|> toDegrees) (r.cos|>Math.Acos|> toDegrees) 
        
        ///Construct Matrix from angle in Radians 
        static member createFromRadians alpha =  
            Rotate2D (sin alpha, cos alpha)
        
        /// Construct Matrix from angle in Degree 
        static member createFromDegrees alpha = 
            let rad =   toRadians alpha
            Rotate2D (sin rad, cos rad) 
        
        member inline r.IsValid =  // check required precision
            let v = r.sin*r.sin + r.cos*r.cos
            0.99999999 < v && v < 1.00000001 
            
        member inline r.InDegree = r.sin |> Math.Asin |>  toDegrees
        
        member inline r.InRadian = r.sin |> Math.Asin 
        
         /// Rotate the Point around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        member inline r.Rotate (x:float, y:float) = struct (r.cos*x - r.sin*y, r.sin*x + r.cos*y) 
        
        /// Rotate the Vector around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        member inline r.RotateOnX (v:Vec) = Vec (v.X,  r.cos*v.Y - r.sin*v.Z, r.sin*v.Y + r.cos*v.Z)
        
        /// Rotate the Vector around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        member inline r.RotateOnY (v:Vec) = Vec ( r.sin*v.Z + r.cos*v.X,  v.Y, r.cos*v.Z - r.sin*v.X) 
        
        /// Rotate the Vector around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        member inline r.RotateOnZ (v:Vec) = Vec (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y,  v.Z)
        
        /// Rotate the UnitVector around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        member inline r.RotateOnX (v:UnitVec) = UnitVec (v.X,  r.cos*v.Y - r.sin*v.Z, r.sin*v.Y + r.cos*v.Z)
        
        /// Rotate the UnitVector around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        member inline r.RotateOnY (v:UnitVec) = UnitVec ( r.sin*v.Z + r.cos*v.X,  v.Y, r.cos*v.Z - r.sin*v.X) 
        
        /// Rotate the UnitVector around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        member inline r.RotateOnZ (v:UnitVec) = UnitVec (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y,  v.Z) 
        
        /// Rotate the Point around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        member inline r.RotateOnX (v:Pnt) = Pnt (v.X,  r.cos*v.Y - r.sin*v.Z, r.sin*v.Y + r.cos*v.Z)
        
        /// Rotate the Point around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        member inline r.RotateOnY (v:Pnt) = Pnt ( r.sin*v.Z + r.cos*v.X,  v.Y, r.cos*v.Z - r.sin*v.X) 
        
        /// Rotate the Point around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        member inline r.RotateOnZ (v:Pnt) = Pnt (r.cos*v.X - r.sin*v.Y, r.sin*v.X + r.cos*v.Y,  v.Z)
        
        /// Rotate the Point around center Point and a X aligned axis, from Y to Z Axis, Counter Clockwise looking from right.
        member inline r.RotateOnXwithCenter (cen:Pnt,  pt:Pnt) =  
            let x = pt.X - cen.X 
            let y = pt.Y - cen.Y 
            let z = pt.Z - cen.Z
            Pnt (x + cen.X,  r.cos*y - r.sin*z + cen.Y, r.sin*y + r.cos*z + cen.Y) 
    
        /// Rotate the Point around center Point and a Y aligned axis, from Z to X Axis, Counter Clockwise looking from back.
        member inline r.RotateOnYwithCenter  (cen:Pnt, pt:Pnt) =  
            let x = pt.X - cen.X 
            let y = pt.Y - cen.Y 
            let z = pt.Z - cen.Z
            Pnt ( r.sin*z + r.cos*x + cen.X, y + cen.X, r.cos*z - r.sin*x + cen.X) 
        
        /// Rotate the Point around center Point and a Z aligned axis, from X to Y Axis, Counter Clockwise looking from top.
        member inline r.RotateOnZwithCenter  (cen:Pnt, pt:Pnt) =  
            let x = pt.X - cen.X  
            let y = pt.Y - cen.Y 
            let z = pt.Z - cen.Z
            Pnt (r.cos*x - r.sin*y + cen.X, r.sin*x + r.cos*y + cen.X, z + cen.X)
    
    type UnitVec with 
    
        //new (v:Vec) =    // use Vec.Unitize instead ?
        //    let l = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z) 
        //    if l<  zeroLenghtTol then failwithf "Cannot construct UnitVec from %O" v
        //    let li=1./l
        //    { X = li*v.X ; Y = li*v.Y ; Z = li*v.Z } 
    
        static member Zero = Vec ( 0. , 0. , 0.)  // needed by 'Array.sum'  type vec or Unit vector  ( Monad ?)   
        static member inline ( ~- ) (v:UnitVec) = UnitVec ( -v.X , -v.Y , -v.Z)        
        static member inline ( * )  (a:UnitVec, f:float) = Vec (a.X * f , a.Y * f , a.Z * f) // scale Vector
        static member inline ( * )  (f:float,   a:UnitVec) = a * f // scale Vector (a*f = f*a)
        static member inline ( / )  (v:UnitVec, f:float) = if abs f >  zeroLenghtTol then v * (1./f) else failwithf "%f too small for dividing %O, tol: %g" f v  zeroLenghtTol
        static member inline ( - )  (a:UnitVec, b:UnitVec) = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)            
        static member inline ( + )  (a:UnitVec, b:UnitVec) = Vec (a.X + b.X , a.Y + b.Y , a.Z + b.Z)  
        static member inline ( * )  (a:UnitVec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z // dot product
        static member inline dot    (a:UnitVec, b:UnitVec) = a.X * b.X + a.Y * b.Y + a.Z * b.Z // dot product
        static member inline cross  (a:UnitVec, b:UnitVec) = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) // cross product // A x B = |A|*|B|*sin(angle), direction follow right-hand rule
        
        static member inline XAxis  = UnitVec (1.0 , 0.0, 0.0)
        static member inline YAxis  = UnitVec (0.0 , 1.0, 0.0)
        static member inline ZAxis  = UnitVec (0.0 , 0.0, 1.0)
        
        static member inline ofPnt  (pt:Pnt) =  
            let l = sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z) 
            if l <  zeroLenghtTol then failwithf "UnitVec.ofPnt failed on too short %O" pt
            let li = 1. / l
            UnitVec( li*pt.X , li*pt.Y , li*pt.Z ) 
        static member inline ofVec  (v:Vec) = 
            let l = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z) 
            if l <  zeroLenghtTol then failwithf "UnitVec.ofVec failed on too short %O" v
            let li = 1. / l
            UnitVec( li*v.X , li*v.Y , li*v.Z ) 
        
        member inline v.IsZero     =  v.X = 0.0 && v.Y = 0.0 && v.Z= 0.0
        member inline v.IsTiny tol =  abs v.X < tol && abs v.Y < tol && abs v.Z < tol
        member inline v.IsInValid  =  Double.IsNaN v.X || Double.IsNaN v.Y || Double.IsNaN v.Z || Double.IsInfinity v.X || Double.IsInfinity v.Y || Double.IsInfinity v.Z
        member inline v.LengthInXY =  sqrt (v.X*v.X + v.Y*v.Y)
        member inline v.LengthSqInXY = v.X*v.X + v.Y*v.Y
        member inline v.WithX x = Vec (x ,v.Y, v.Z) // returns new Vector with new x coordinate, y and z the same as before
        member inline v.WithY y = Vec (v.X, y, v.Z)
        member inline v.WithZ z = Vec (v.X ,v.Y, z)
        member inline v.DirMatch (vv:UnitVec) = v*vv > 0. // direction match
        
        //static member inline DivideByInt (v:UnitVec, i:int) = if i<>0 then v / float i else failwithf "DivideByInt 0 %O " v // needed by  'Array.average'  
        
        /// Returns angle between two UnitVectors in Radians.
        /// Takes vector orientation into account. 
        /// Range 0.0 to PI( = 0 to 180 degree)
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
            // at +/-1. (See Windows OS Bug #1706299 for details) (form WPF refrence scource code)
            let dot = a * b
            if -0.98 < dot && dot < 0.98 then // threshold for switching 0.98 ? 
                acos dot
            else
                if dot < 0. then Math.PI - 2.0 * asin((-a - b).Length * 0.5)
                else                       2.0 * asin(( a - b).Length * 0.5)            

        /// Returns positive angle between two UnitVectors in Radians. 
        /// Ignores orientation. 
        /// Range 0.0 to PI/2 ( = 0 to 90 degree)
        static member inline angleHalfPi (a:UnitVec) (b:UnitVec) = 
            let dot =  a * b
            let dotAbs = abs dot
            if dotAbs < 0.98 then  
                acos dotAbs 
            else
                if dot < 0. then 2.0 * asin((-a - b).Length * 0.5)
                else             2.0 * asin(( a - b).Length * 0.5) 
            
        /// Returns positive angle between two UnitVectors in Degrees. 
        /// Takes vector orientation into account.
        /// Range 0 to 180 degrees.
        static member inline angle180 (a:UnitVec) (b:UnitVec) = 
            UnitVec.anglePi a b |>  toDegrees 
    
    
        /// Returns positive angle between two UnitVectors in Degrees,
        /// Ignores vector orientation.
        /// Range: 0 to 90 degrees.
        static member inline angle90 (a:UnitVec) (b:UnitVec) = 
            UnitVec.angleHalfPi a b |>  toDegrees 
        
        (*
        /// Returns positive angle from Vector 'a' to Vector 'b' projected in XY Plane.
        /// In Radians
        /// Considering counter clockwise rotation round the World ZAxis
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        static member inline angle2PiProjectedInXYPlane (a:UnitVec) (b:UnitVec)   = 
            if abs(a.X) < zeroLenghtTol && abs(a.Y) < zeroLenghtTol then failwithf "Vec.angleTwoPiProjectedInXYPlane: input vector a is vertical or zero length:%O" a
            if abs(b.X) < zeroLenghtTol && abs(b.Y) < zeroLenghtTol then failwithf "Vec.angleTwoPiProjectedInXYPlane: input vector b is vertical or zero length:%O" b
            let va = Vec(a.X, a.Y, 0.0)  //TODO used 2D Vc
            let vb = Vec(b.X, b.Y, 0.0)  
            let ang = UnitVec.anglePi va vb 
            if (UnitVec.cross va vb).Z >= 0.0 then ang
            else                           Math.PI * 2. - ang
    
        /// Returns positive angle of two Vector projected in XY Plane in Degrees
        /// Considering positve rotation round the World ZAxis
        /// Range:  0 to 360 degrees
        /// input vector does not need to be unitized
        static member inline angle360ProjectedInXYPlane (a:Vec) (b:Vec)   = angleTwoPiProjectedInXYPlane a b |> toDegrees
        *) 
        
        /// Rotate the UnitVector in Degrees around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate.
        static member inline rotateOnX (angDegree) (vec:UnitVec) = (Rotate2D.createFromDegrees angDegree).RotateOnX vec
        
        /// Rotate the UnitVector in Degrees around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate.
        static member inline rotateOnY (angDegree) (vec:UnitVec) = (Rotate2D.createFromDegrees angDegree).RotateOnY vec 
        
        /// Rotate the UnitVector in Degrees around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate.
        static member inline rotateOnZ (angDegree) (vec:UnitVec) = (Rotate2D.createFromDegrees angDegree).RotateOnZ vec     
    
    type Vec with 
        static member Zero = Vec ( 0. , 0. , 0.)  // needed by 'Array.sum'        
        static member inline (~- ) (v:Vec)          = Vec( -v.X , -v.Y , -v.Z)
        static member inline ( * ) (a:Vec, f:float) = Vec (a.X * f , a.Y * f , a.Z * f) // scale Vec
        static member inline ( * ) (f:float, a:Vec) = a * f // scale Vector (a*f = f*a)
        static member inline ( / ) (v:Vec, f:float) = if abs f >  zeroLenghtTol then v * (1./f) else failwithf "Divide Vec via '/' : %f too small for dividing %O, tol: %g" f v zeroLenghtTol
        static member inline ( - ) (a:Vec, b:Vec)   = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)            
        static member inline ( + ) (a:Vec, b:Vec)   = Vec (a.X + b.X , a.Y + b.Y , a.Z + b.Z)  
        static member inline ( * ) (a:Vec, b:Vec)   = a.X * b.X + a.Y * b.Y + a.Z * b.Z // dot product
        
        static member inline XAxis  = Vec (1.0 , 0.0, 0.0)
        static member inline YAxis  = Vec (0.0 , 1.0, 0.0)
        static member inline ZAxis  = Vec (0.0 , 0.0, 1.0)
        static member inline ofPnt     (v:Pnt)     = Vec (v.X, v.Y, v.Z) 
        static member inline ofUnitVec (v:UnitVec) = Vec (v.X, v.Y, v.Z) 
        
        member inline v.IsZero = v.X = 0.0 && v.Y = 0.0 && v.Z= 0.0
        member inline v.IsTiny tol = abs v.X < tol && abs v.Y < tol && abs v.Z < tol
        member inline v.IsInValid =  Double.IsNaN v.X || Double.IsNaN v.Y || Double.IsNaN v.Z || Double.IsInfinity v.X || Double.IsInfinity v.Y || Double.IsInfinity v.Z
        //member inline v.Length = sqrt (v.X*v.X + v.Y*v.Y + v.Z*v.Z) // needs to be defined earlier
        member inline v.LengthSq = v.X*v.X + v.Y*v.Y + v.Z*v.Z
        member inline v.LengthInXY =  sqrt (v.X*v.X + v.Y*v.Y)
        member inline v.LengthSqInXY = v.X*v.X + v.Y*v.Y
        member inline v.WithX x = Vec (x ,v.Y, v.Z) // returns new Vector with new x coordinate, y and z the same as before
        member inline v.WithY y = Vec (v.X, y, v.Z)
        member inline v.WithZ z = Vec (v.X ,v.Y, z) 
        
        member inline v.Half = Vec (v.X*0.5 ,v.Y*0.5, v.Z*0.5)
        
        /// Test for positive Dot product
        member inline v.DirMatch (vv:Vec) = v*vv > 0. // direction match
        
        member inline v.WithLength (desiredLength:float) =  
            let len = v.Length 
            if len >  zeroLenghtTol then v*(desiredLength/len)
            else failwithf "Vec.WithLength %O too short for ScaleToLength, tol: %g" v  zeroLenghtTol
        
        member inline v.Unitized =  
            let l = sqrt(v.X*v.X+v.Y*v.Y+v.Z*v.Z) 
            if l >  zeroLenghtTol then let li=1./l in UnitVec( li*v.X , li*v.Y ,li*v.Z ) 
            else failwithf "%O is too small for unitizing, tol:  zeroLenghtTol" v 
        
        member inline v.UnitizedUnchecked =  
            let li = 1./sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z) 
            UnitVec( li*v.X , li*v.Y ,li*v.Z ) 
        
        member inline v.IsUnit   = 
            let l = v.Length 
            0.999999999 < l && 
            1.000000001 > l
        
        ///Projects vector in XY plane and returns perpendicular vector ( rotated counterclockwise)
        member inline v.PerpendicularVecInXY() = Vec(-v.Y, v.X, 0.0) 
        
        //static member inline ( + )  (a:Vec, b:Vec) = Pnt (a.X + b.X , a.Y + b.Y , a.Z + b.Z) // required for Seq.average
        //static member inline DivideByInt (v:Vec, i:int) = if i<>0 then v / float i else failwithf "DivideByInt 0 %O " pt  // needed by  'Array.average'
    
        /// cross product // A x B = |A|*|B|*sin(angle), direction follow right-hand rule
        static member inline cross    (a:Vec,b:Vec) = Vec (a.Y * b.Z - a.Z * b.Y ,  a.Z * b.X - a.X * b.Z ,  a.X * b.Y - a.Y * b.X ) 
        static member inline dot      (a:Vec,b:Vec) = a * b
        
        static member inline getX     (v:Vec) = v.X
        static member inline getY     (v:Vec) = v.Y
        static member inline getZ     (v:Vec) = v.Z
        static member inline setX       x (v:Vec) = v.WithX x
        static member inline setY       y (v:Vec) = v.WithY y
        static member inline setZ       z (v:Vec) = v.WithZ z
        static member inline add      (a:Vec) (b:Vec) = b + a   
        static member inline create   (a:Vec) (b:Vec) = b - a   
        static member inline sqLength (v:Vec) = v.LengthSq
        static member inline dirMatch (a:Vec) (b:Vec) = b.DirMatch a
        static member inline scale    (f:float) (v:Vec) = v*f
        static member inline length       (v:Vec) = v.Length
        static member inline shiftX     x (v:Vec) = Vec (v.X+x, v.Y,   v.Z)
        static member inline shiftY     y (v:Vec) = Vec (v.X,   v.Y+y, v.Z)
        static member inline shiftZ     z (v:Vec) = Vec (v.X,   v.Y,   v.Z+z)    
        static member inline isTiny   tol (v:Vec) = v.IsTiny tol
        static member inline setLength  f (v:Vec) = v.WithLength f    
    
        /// Returns vector reversed
        static member inline reverse (v:Vec) = -v
    
        /// Returns vector unitized, fails on zero length vectors
        static member inline unitize (v:Vec) =  v.Unitized
        
        /// Returns vector unitized or Vec(NaN,NaN,NaN) on zero length vectors
        static member inline unitizeUnChecked (v:Vec) = v.UnitizedUnchecked
    
        /// Returns vector Determinatnt
        static member inline det (u:Vec) (v:Vec) (w:Vec) = u.X*v.Y*w.Z + v.X*w.Y*u.Z + w.X*u.Y*v.Z - w.X*v.Y*u.Z - v.X*u.Y*w.Z - u.X*w.Y*v.Z 
        
        /// Returns positive angle between two vectors in Radians.
        /// Takes vector orientation into account,
        /// Range 0.0 to PI( = 0 to 180 degree).
        static member inline anglePi (a:Vec) (b:Vec) = 
            UnitVec.anglePi a.Unitized b.Unitized 
    
        /// Returns positive angle between two vectors in Degrees.
        /// Takes vector orientation into account,
        /// Range 0 to 180 degrees.
        static member inline angle180 (a:Vec) (b:Vec) = 
            UnitVec.angle180 a.Unitized b.Unitized 
    
        /// Returns positive angle between two vectors in Radians.
        /// Ignores vector orientation,
        /// Range: 0.0 to PI/2 ( = 0 to 90 degrees) 
        static member inline angleHalfPi (a:Vec) (b:Vec) = 
            UnitVec.angleHalfPi a.Unitized b.Unitized 
    
        /// Returns positive angle between two vectors in Degrees.
        /// Ignores vector orientation,
        /// Range: 0 to 90 degrees.
        static member inline angle90 (a:Vec) (b:Vec) = 
            UnitVec.angle90 a.Unitized b.Unitized 
        
        /// Returns a (not unitized) bisector vector in the middle direction. 
        /// Code : a.Unitized + b.Unitized
        static member inline bisector (a:Vec) (b:Vec) = a.Unitized + b.Unitized 

    
        /// Flips the vector if Z part is smaller than 0.0
        static member inline flipToPointUp (v:Vec) = if v.Z < 0.0 then -v else v 
        
        /// Rotate the Vector in Degrees around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate.
        static member inline rotateOnX (angDegree) (vec:Vec) = (Rotate2D.createFromDegrees angDegree).RotateOnX vec
        
        /// Rotate the Vector in Degrees around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate.
        static member inline rotateOnY (angDegree) (vec:Vec) = (Rotate2D.createFromDegrees angDegree).RotateOnY vec 
        
        /// Rotate the Vector in Degrees around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate.
        static member inline rotateOnZ (angDegree) (vec:Vec) = (Rotate2D.createFromDegrees angDegree).RotateOnZ vec 
    
    type Pnt with 
        
        static member Zero = Pnt ( 0. , 0. , 0.)  // needed by 'Array.sum'        
        static member inline ( * )  (a:Pnt, f:float) = Pnt (a.X * f , a.Y * f , a.Z * f) 
        static member inline ( * )  (f:float, a:Pnt) = Pnt (a.X * f , a.Y * f , a.Z * f) 
        static member inline ( / )  (pt:Pnt, f:float) =  if abs f >  zeroLenghtTol then pt * (1./f) else failwithf "Divide Pnt via '/' : %f too small for dividing %O, tol: %g" f pt  zeroLenghtTol
        static member inline ( - )  (a:Pnt, b:Pnt) = Vec (a.X - b.X , a.Y - b.Y , a.Z - b.Z)
        
        static member inline ( - )  (a:Pnt, v:Vec)     = Pnt (a.X - v.X , a.Y - v.Y , a.Z - v.Z)            
        static member inline ( - )  (a:Pnt, v:UnitVec) = Pnt (a.X - v.X , a.Y - v.Y , a.Z - v.Z)            
        static member inline ( + )  (a:Pnt, v:Vec)     = Pnt (a.X + v.X , a.Y + v.Y , a.Z + v.Z)  
        static member inline ( + )  (a:Pnt, v:UnitVec) = Pnt (a.X + v.X , a.Y + v.Y , a.Z + v.Z)  
        
        static member inline ( + )  (a:Pnt, b:Pnt) = Pnt (a.X + b.X , a.Y + b.Y , a.Z + b.Z) // required for Seq.average and midPt
        
        static member inline ofVec      (v:Vec)     = Pnt (v.X, v.Y, v.Z)  
        static member inline ofUnitVec  (v:UnitVec) = Pnt (v.X, v.Y, v.Z) 
        
        member inline pt.IsZero = pt.X = 0.0 && pt.Y = 0.0 && pt.Z= 0.0
        member inline v.IsInValid =  Double.IsNaN v.X || Double.IsNaN v.Y || Double.IsNaN v.Z || Double.IsInfinity v.X || Double.IsInfinity v.Y || Double.IsInfinity v.Z
        member inline pt.WithX x = Pnt (x ,pt.Y, pt.Z) // returns new Vector with new x coordinate, y and z the same as before
        member inline pt.WithY y = Pnt (pt.X, y, pt.Z)
        member inline pt.WithZ z = Pnt (pt.X ,pt.Y, z)
        
        //member inline pt.DistFromOrigin = sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z) 
        //member inline pt.DistFromOriginSquare = pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z
        //member inline pt.DistFromOriginInXY =  sqrt (pt.X*pt.X + pt.Y*pt.Y)
        //member inline pt.DistFromOriginSquareInXY = pt.X*pt.X + pt.Y*pt.Y
        //member inline pt.WithDistFromOrigin (l:float) = let d=pt.DistFromOrigin in (if d >  zeroLenghtTol then pt*(l/d) else failwithf "WithDistFromOrigin Pt=0,0,0 %O" pt)
        
        //static member inline DivideByInt (pt:Pnt, i:int) = if i<>0 then pt / float i else failwithf "DivideByInt 0 %O " pt  // needed by  'Array.average'
    
        static member inline getX       (pt:Pnt) = pt.X
        static member inline getY       (pt:Pnt) = pt.Y
        static member inline getZ       (pt:Pnt) = pt.Z
        static member inline add        (v:Vec) (a:Pnt) = a + v
        static member inline addToPtn   (a:Pnt) (v:Vec) = a + v
        static member inline distance   (a:Pnt) (b:Pnt) = (b-a).Length
        static member inline distanceSq (a:Pnt) (b:Pnt) = (b-a).LengthSq
        static member inline divPt      (a:Pnt,b:Pnt, f:float)  = a + (b-a)*f
        static member inline distPt     (a:Pnt,b:Pnt, d:float)  = a + (b-a).Unitized*d 
        static member inline midPt      (a:Pnt) (b:Pnt)         = (a+b) * 0.5
        static member inline scale      (f:float) (pt:Pnt) = pt*f
        static member inline setX       (x:float) (pt:Pnt) = pt.WithX x
        static member inline setY       (y:float) (pt:Pnt) = pt.WithY y
        static member inline setZ       (z:float) (pt:Pnt) = pt.WithZ z
        static member inline shiftX     (x:float) (pt:Pnt) = Pnt (pt.X+x, pt.Y,   pt.Z)
        static member inline shiftY     (y:float) (pt:Pnt) = Pnt (pt.X,   pt.Y+y, pt.Z)
        static member inline shiftZ     (z:float) (pt:Pnt) = Pnt (pt.X,   pt.Y,   pt.Z+z)    
        
        //static member inline distFromOrigin (pt:Pnt) = pt.DistFromOrigin
        //static member inline setDistFromOrigin f (pt:Pnt) = pt.WithDistFromOrigin f
        //static member inline distFromOriginSquare (pt:Pnt) = pt.DistFromOriginSquare
        
        /// Returns angle between three Points in Radians 
        static member inline angleFrom3Pts (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt)  =   
            Vec.anglePi (ptPrev-ptThis) (ptNext-ptThis)
    
        /// Returns a (not unitized) bisector vector in the middle direction from ptThis. 
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized 
        /// ptPrev * ptThis * ptNext ->   bisector Vector 
        static member inline bisector (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) =  
            (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized   
        
        /// For three Points decribing a plane return a normal.
        /// If the returned vector has length zero then the points are in one Line.
        static member normalOf3Pts (a:Pnt, b:Pnt, c:Pnt) =  Vec.cross (a-b, c-b) 
        
        /// Rotate the Point in Degrees around X axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member inline rotateOnX (angDegree) (pt:Pnt) = (Rotate2D.createFromDegrees angDegree).RotateOnX pt
        
        /// Rotate the Point in Degrees around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member inline rotateOnY (angDegree) (pt:Pnt) = (Rotate2D.createFromDegrees angDegree).RotateOnY pt 
        
        /// Rotate the Point in Degrees around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member inline rotateOnZ (angDegree) (pt:Pnt) = (Rotate2D.createFromDegrees angDegree).RotateOnZ pt 
        
        /// Rotate the Point in Degrees around center Point and a X aligned axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member inline rotateOnXwithCenter (cen:Pnt) (angDegree) (pt:Pnt) = (Rotate2D.createFromDegrees angDegree).RotateOnXwithCenter(cen,pt) 
    
        /// Rotate the Point in Degrees around center Point and a Y aligned axis, from Z to X Axis, Counter Clockwise looking from back.
        static member inline rotateOnYwithCenter (cen:Pnt) (angDegree) (pt:Pnt) = (Rotate2D.createFromDegrees angDegree).RotateOnYwithCenter(cen,pt) 
        
        /// Rotate the Point in Degrees around center Point and a Z aligned axis, from X to Y Axis, Counter Clockwise looking from top.
        static member inline rotateOnZwithCenter (cen:Pnt) (angDegree) (pt:Pnt) = (Rotate2D.createFromDegrees angDegree).RotateOnZwithCenter(cen,pt) 
        
        (*
        /// The 3 points describe a corner, return point has the sames distance to both lines (ptPrev, ptThis) and (ptThis, ptNext) of this corner: 
        /// distance -> (ptPrev * ptThis * ptNext) -> Point with same distance to both lines
        static member inline ptInBisector distance (ptPrev:Pnt, ptThis:Pnt, ptNext:Pnt) =
            let vp = (ptPrev-ptThis)
            let vn = (ptNext-ptThis)
            let alpha = Vec.angle vp vn
            let sinus = sin alpha
            if abs sinus <  zeroLenghtTol then failwithf " ptInBisector: 3Points are in Line %O, %O, %O" ptPrev ptThis ptNext
            ptThis + (vp+vn) * (distance / sinus) // summe der einheitsvectoren mal sin alpha
        
        
        /// Move a point by amount along vector in orientation towards DirPt:
        /// along vector -> by amount -> in direction of DirPt -> Pt to move
        static member distPtVecInDirOfPt (vec:Vec)  (amount:float)  (dirPt:Pnt) (pt:Pnt) =
            let vec = if Vec.dirMatch (dirPt - pt)  vec  then vec.Unitized else -vec.Unitized  // orientation check
            pt + vec*amount
        
         
        /// Retrurns the unitized mean normal of a sequence of points, 
        /// Points must describe a convex polygon for stable results.
        static member normalOfSeq (vecs: Pnt seq) = 
            vecs  
            |> Seq.prevThisNextLoop  // compare performance to Array ?? compare using struct tuples ?? or hand rolled loop
            |> Seq.map (fun (p,t,n) -> (n-t) /* (p-t) )
            |> Seq.sum 
            |> Vec.unitize
        
        /// Retrurns the unitized mean normal of a sequence of points, 
        /// Points canbe in any order.
        static member normalOfSeqUnOrdered (pts: Pnt seq) = 
            let cen = Seq.average pts
            let vs = 
                pts  
                |> Seq.thisNextLoop
                |> Seq.map (fun (p,n) -> (n-cen) /* (p-cen) )
                |> Array.ofSeq
            let av = Array.average vs
            let avc = if av.IsTiny 0.00001 then Vec(0.,0.,1.) else av
            vs
            |> Array.map( fun v -> if Vec.dirMatch v avc then v else -v)
            |> Array.sum
            |> Vec.unitize
        *)
       
        //static member ofIntTup precMult z (t:IntTup) = Pnt( ((float t.a)+0.5)/precMult , ((float t.b)+0.5)/precMult , z )
    
    /// Quaternion, for arbitrary 3D rotations.
    /// This implementation guarantees the Quat to be always unitized.
    [<Struct; NoEquality; NoComparison>]
    type Quat = 
        // https://www.youtube.com/watch?v=zjMuIxRvygQ
        // https://github.com/mcneel/rhinocommon/blob/master/dotnet/opennurbs/opennurbs_quaternion.cs 
        // http://www.codeproject.com/Articles/36868/Quaternion-Mathematics-and-3D-Library-with-C-and-G
        // http://physicsforgames.blogspot.co.at/2010/02/quaternions.html
        // http://www.ogre3d.org/tikiwiki/Quaternion+and+Rotation+Primer    
        // https://referencesource.microsoft.com/#PresentationCore/Core/CSharp/System/Windows/Media3D/Quaternion.cs
        // https://github.com/mrdoob/three.js/blob/dev/src/math/Quaternion.js
        
        val X:float
        val Y:float
        val Z:float  
        val W:float
        
        /// Unsave Internal constructor,  public only for inlining.
        [<Obsolete("Unsave internal constructor,  but must be public for inlining. So marked Obsolete instead") >]
        new (x, y, z, w) =  
            //let sum = x*x + y*y + z*z + w*w
            //if 0.99999999 > sum ||sum > 1.00000001 then failwithf "Quat constructors are not unitized: %g %g %g %g" x y z w 
            {X = x; Y = y; Z = z; W = w}   
       
        override q.ToString() =  
            //sprintf "Quat(X=%g, Y=%g, Z=%g, W=%g, rotation in degree = %g)" q.X q.Y q.Z q.W q.AngleInDegrees
            sprintf   "Quat(X=%.02f, Y=%.02f, Z=%.02f, W=%.02f, rotation in degree = %.02f)" q.X q.Y q.Z q.W q.AngleInDegrees
        
        static member inline ( * ) (l:Quat, r:Quat)  =  
            Quat(   l.W * r.X + l.X * r.W + l.Y * r.Z - l.Z * r.Y ,
                    l.W * r.Y + l.Y * r.W + l.Z * r.X - l.X * r.Z ,
                    l.W * r.Z + l.Z * r.W + l.X * r.Y - l.Y * r.X ,
                    l.W * r.W - l.X * r.X - l.Y * r.Y - l.Z * r.Z ) 
                    
        member inline q.Conjugate = Quat (-q.X, -q.Y, -q.Z, q.W)  
        
        /// Schould allways be 1.0
        member inline q.Magnitude = sqrt (q.X*q.X + q.Y*q.Y + q.Z*q.Z + q.W*q.W) 
        
        /// Returns Angle in Degree 
        member q.AngleInDegrees =  
            // alternative : https://referencesource.microsoft.com/#PresentationCore/Core/CSharp/System/Windows/Media3D/Quaternion.cs,185
            let mutable w = q.W
            if w < 0.0 then w<-0.0 // clamp,  to avoid error in acos
            if w > 1.0 then w<-1.0
            (acos w |>  toDegrees) * 2.0 
        
        /// Transform a Point round Pnt.Zero
        member q.Rotate(pt:Pnt)  =  
            // https://gamedev.stackexchange.com/a/28418
            let twoX = q.X + q.X
            let twoY = q.Y + q.Y
            let twoZ = q.Z + q.Z 
            let WXX = q.W * twoX
            let WYY = q.W * twoY
            let WZZ = q.W * twoZ
            let XXX = q.X * twoX
            let XYY = q.X * twoY
            let XZZ = q.X * twoZ
            let YYY = q.Y * twoY
            let YZZ = q.Y * twoZ
            let ZZZ = q.Z * twoZ
            // TODO this is actually a 3x3 Matrix: https://github.com/mrdoob/three.js/blob/dev/src/math/Matrix4.js
            let xx = 1.0 - YYY - ZZZ
            let xy = XYY - WZZ
            let xz = XZZ + WYY
            let yx = XYY + WZZ
            let yy = 1.0 - XXX - ZZZ
            let yz = YZZ - WXX
            let zx = XZZ - WYY
            let zy = YZZ + WXX
            let zz = 1.0 - XXX - YYY 
            Pnt ( pt.X * xx + pt.Y * xy + pt.Z * xz
                , pt.X * yx + pt.Y * yy + pt.Z * yz    
                , pt.X * zx + pt.Y * zy + pt.Z * zz 
                ) 
        /// Transform a Vector        
        member q.Rotate(v:Vec)  =  
            // https://gamedev.stackexchange.com/a/28418
            let twoX = q.X + q.X
            let twoY = q.Y + q.Y
            let twoZ = q.Z + q.Z 
            let WXX = q.W * twoX
            let WYY = q.W * twoY
            let WZZ = q.W * twoZ
            let XXX = q.X * twoX
            let XYY = q.X * twoY
            let XZZ = q.X * twoZ
            let YYY = q.Y * twoY
            let YZZ = q.Y * twoZ
            let ZZZ = q.Z * twoZ
            // TODO this is actually a 3x3 Matrix: https://github.com/mrdoob/three.js/blob/dev/src/math/Matrix4.js
            let xx = 1.0 - YYY - ZZZ
            let xy = XYY - WZZ
            let xz = XZZ + WYY
            let yx = XYY + WZZ
            let yy = 1.0 - XXX - ZZZ
            let yz = YZZ - WXX
            let zx = XZZ - WYY
            let zy = YZZ + WXX
            let zz = 1.0 - XXX - YYY 
            Vec ( v.X * xx + v.Y * xy + v.Z * xz
                , v.X * yx + v.Y * yy + v.Z * yz    
                , v.X * zx + v.Y * zy + v.Z * zz 
                )         
        /// Transform a UnitVector         
        member q.Rotate(v:UnitVec)  =  
            // https://gamedev.stackexchange.com/a/28418
            let twoX = q.X + q.X
            let twoY = q.Y + q.Y
            let twoZ = q.Z + q.Z 
            let WXX = q.W * twoX
            let WYY = q.W * twoY
            let WZZ = q.W * twoZ
            let XXX = q.X * twoX
            let XYY = q.X * twoY
            let XZZ = q.X * twoZ
            let YYY = q.Y * twoY
            let YZZ = q.Y * twoZ
            let ZZZ = q.Z * twoZ
            // TODO this is actually a 3x3 Matrix: https://github.com/mrdoob/three.js/blob/dev/src/math/Matrix4.js
            let xx = 1.0 - YYY - ZZZ
            let xy = XYY - WZZ
            let xz = XZZ + WYY
            let yx = XYY + WZZ
            let yy = 1.0 - XXX - ZZZ
            let yz = YZZ - WXX
            let zx = XZZ - WYY
            let zy = YZZ + WXX
            let zz = 1.0 - XXX - YYY 
            UnitVec ( v.X * xx + v.Y * xy + v.Z * xz
                , v.X * yx + v.Y * yy + v.Z * yz    
                , v.X * zx + v.Y * zy + v.Z * zz 
                ) 
        
        /// Transform a Point round a center point 
        member q.RotateWithCenter(cen:Pnt, pt:Pnt) :Pnt =  
            let p = pt-cen 
            let r = q.Rotate(p) 
            cen + r 
        
        /// The created Rotation is Counter Clockwise looking in the direction of the Vector
        static member inline createFromRadians (axis:Vec, angleInRadians)  =
            // from https://referencesource.microsoft.com/#PresentationCore/Core/CSharp/System/Windows/Media3D/Quaternion.cs,91 
            let mutable li = axis.Length
            if li <  zeroLenghtTol then failwithf "*** Cannot create Quat to rotate %g radians around zero length vector %O" angleInRadians axis // or return identity Quat ?
            let angHalf = angleInRadians * 0.5
            let sa = sin angHalf
            li <- 1. / li // inverse 
            //unitizing vector:
            Quat ( axis.X * li * sa, axis.Y * li * sa, axis.Z * li * sa, cos angHalf )
        
        /// The created Rotation is Clockwise looking in the direction of the Vector ( Right Hand Rule) 
        static member inline createFromDegree (axis : Vec, angleInDegrees) = 
            Quat.createFromRadians (axis,  toRadians angleInDegrees) 
    
    (* type Vec with  
         // from Rhino.Scripting.Extra:
         
        static member inline SqrtEpsilon  =  1e-9
    
        /// Gets the X value of  Vec
        static member inline getX (v:Vec)  =  v.X
    
        /// Gets the Y value of  Vec
        static member inline getY (v:Vec) =  v.Y
    
        /// Gets the Z value of  Vec
        static member inline getZ (v:Vec) =  v.Z
    
        /// Sets the X value and returns new Vec
        static member inline setX x (v:Vec) =  Vec(x, v.Y, v.Z)
    
        /// Sets the Y value and returns new Vec
        static member inline setY y (v:Vec) =  Vec(v.X, y, v.Z)
    
        /// Sets the Z value and returns new Vec
        static member inline setZ z (v:Vec) =  Vec(v.X, v.Y, z)
    
        /// Scales the vector
        static member inline scale (sc:float) (v:Vec) = v * sc
    
        /// Same as reverse
        static member inline flip  (v:Vec) = -v
    
        /// Same as flip
        static member inline reverse  (v:Vec) = -v
    
        /// Dot product
        static member inline dot (a:Vec) (b:Vec) = a * b
    
        /// Vector length
        static member inline length (v:Vec) = v.Length
    
        /// Vector length projected into X Y Plane
        /// sqrt( v.X * v.X  + v.Y * v.Y)
        static member inline lengthInXY(v:Vec) = sqrt(v.X * v.X  + v.Y * v.Y)
    
        /// Checks if a vector is vertical by doing:
        /// abs(v.X) + abs(v.Y) < Vec.SqrtEpsilon
        /// fails on tiny (shorter than Vec.SqrtEpsilon) vectors
        static member inline isVertical (v:Vec) = 
            if v.IsTiny(Vec.SqrtEpsilon) then failwithf "Vec Cannot not check very tiny vector for verticality %O" v
            abs(v.X) + abs(v.Y) < Vec.SqrtEpsilon
    
        /// Checks if a vector is horizontal  by doing:
        /// abs(v.Z) < Vec.SqrtEpsilon
        /// fails on tiny (shorter than Vec.SqrtEpsilon) vectors
        static member inline isHorizontal (v:Vec) = 
            if v.IsTiny(Vec.SqrtEpsilon) then failwithf "Vec Cannot not check very tiny vector for horizontality %O" v
            abs(v.Z) < Vec.SqrtEpsilon
    
        /// Cross product
        /// A x B = |A|*|B|*sin(angle), direction follow right-hand rule
        static member inline cross (a:Vec) (b:Vec) = 
            Vec (  a.Y * b.Z - a.Z * b.Y ,
                        a.Z * b.X - a.X * b.Z ,
                        a.X * b.Y - a.Y * b.X )
    
        /// Unitizes the vector
        /// fails if length is les than 1e-9 units
        static member inline unitize (v:Vec) = 
            let len = sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z) // see v.Unitized() type extension too
            if len > 1e-9 then v * (1./len)
            else failwithf "Vec Vec.unitize: %O is too small for unitizing, tol: 1e-9" v
    
        /// Unitize vector, if input vector is shorter than 1e-6 alternative vector is returned (without beeing unitized).
        static member inline unitizeWithAlternative (unitVectorAlt:Vec) (v:Vec) = 
            let l = v.SquareLength
            if l < RhinoMath.ZeroTolerance  then  //sqrt RhinoMath.ZeroTolerance = 1e-06
                unitVectorAlt //|> Vec.unitize
            else
                let f = 1.0 / sqrt(l)
                Vec(v.X*f , v.Y*f , v.Z*f)
    
        // Applies a transformation matrix
        //let transform (xForm:Transform) (vec:Vec ) = 
        //    let v = Vec(vec)
        //    v.Transform(xForm)
        //    v
    
    
        /// Project vector to World XY Plane
        /// Fails if resulting vector is of almost zero length (Vec.SqrtEpsilon)
        /// Returns Vec(v.X, v.Y, 0.0)
        static member inline projectToXYPlane (v:Vec) = 
            let r = Vec(v.X, v.Y, 0.0)
            if r.IsTiny(Vec.SqrtEpsilon) then failwithf "Vec.projectToXYPlane: Cannot projectToXYPlane for vertical vector %O" v
            r
    
        /// Project vector to Plane
        /// Fails if resulting vector is of almost zero length (Vec.SqrtEpsilon)
        static member inline projectToPlane (pl:Geometry.Plane) (v:Vec) = 
            let pt = pl.Origin + v
            let clpt = pl.ClosestPoint(pt)
            let r = clpt-pl.Origin
            if r.IsTiny(Vec.SqrtEpsilon) then failwithf "Vec.projectToPlane: Cannot projectToPlane for perpendicular vector %O to given plane %O" v pl
            r
    
        // Project point onto a finite line in directin of v
        // Fails if line is missed by tolerance 1e-6
        //and draws debug objects on layer 'Error-projectToLine'
        //let projectToLine (ln:Line) (v:Vec) (pt:Point3d) = 
        //    let h = Line(pt,v)
        //    let ok,tln,th = Intersect.Intersection.LineLine(ln,h)
        //    if not ok then failwithf "Vec.projectToLine: project in direction failed. (are they paralell?)"
        //    let a = ln.PointAt(tln)
        //    let b = h.PointAt(th)
        //    if (a-b).SquareLength > RhinoMath.ZeroTolerance then
        //        //Scripting.Doc.Objects.AddLine ln   |> Scripting.setLayer "Error-projectToLine"
        //        //Scripting.Doc.Objects.AddLine h    |> Scripting.setLayer "Error-projectToLineDirection"
        //        //Scripting.Doc.Objects.AddPoint pt  |> Scripting.setLayer "Error-projectToLineFrom"
        //        failwithf "Vec.projectToLine: missed Line by: %g " (a-b).Length
        //    a
    
    
        /// Input vectors MUST BE UNITIZED
        /// Returns positive angle between two vectors in Radians , takes vector orientation into account,
        /// Range 0.0 to PI( = 0 to 180 degree)
        static member inline anglePiFast (a:Vec) (b:Vec) = // INPUT MUST BE UNITIZES
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
            // at +/-1. (See Windows OS Bug #1706299 for details) (form WPF refrence scource code)
            //INPUT MUST BE UNITIZED
            let dot = a * b
            if -0.98 < dot && dot < 0.98 then acos dot // TODO test and compare this to real Acossafe function !!! also to find thershold for switching !!  0.98 ??
            else
                if dot < 0. then System.Math.PI - 2.0 * asin((-a - b).Length * 0.5)
                else                              2.0 * asin(( a - b).Length * 0.5)
    
        /// Returns positive angle between two vectors in Radians , takes vector orientation into account,
        /// Range 0.0 to PI( = 0 to 180 degree)
        /// Unitizes the input vectors
        static member inline anglePi (a:Vec) (b:Vec) = 
            anglePiFast a.Unitized b.Unitized
    
    
        /// Returns positive angle between two vectors in Degrees , takes vector orientation into account,
        /// Range 0 to 180 degrees
        /// Unitizes the input vectors
        static member inline angle180 (a:Vec) (b:Vec) = 
            anglePiFast a.Unitized b.Unitized |>  toDegrees
    
    
        /// Returns positive angle between two vectors in Radians,
        /// Ignores vector orientation,
        /// Range: 0.0 to PI/2 ( = 0 to 90 degrees)
        /// Unitizes the input vectors
        static member inline angleHalfPi (a:Vec) (b:Vec) = 
            let ang = anglePiFast a.Unitized b.Unitized
            if ang > System.Math.PI*0.5 then System.Math.PI - ang else ang
    
        /// Returns positive angle between two vectors in Degrees,
        /// Ignores vector orientation,
        /// Range: 0 to 90 degrees
        /// Unitizes the input vectors
        static member inline angle90 (a:Vec) (b:Vec) = 
            angleHalfPi a b |>  toDegrees
    
        //--------------------angels 360:---------------------------
    
        /// Returns positive angle between two Vectors in Radians projected to a Plane
        /// Considering positve rotation round a Planes ZAxis
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        /// Unitizes the input vectors
        static member inline angleTwoPiProjected (pl:Geometry.Plane) (a:Vec) (b:Vec)  = 
            let x = projectToPlane pl a
            let y = projectToPlane pl b
            let ang = anglePi x y
            if dot (cross x y) pl.ZAxis >= 0.0 then ang
            else                                    Math.PI * 2. - ang
    
        /// Returns positive angle between two Vectors in Degrees projected to a Plane
        /// Considering positve rotation round a Planes ZAxis
        /// Range:  0 to 360 degrees
        /// Unitizes the input vectors
        static member inline angle360Projected (pl:Geometry.Plane) (a:Vec) (b:Vec)  =  angleTwoPiProjected pl a b|> toDegrees
    
        /// Returns positive angle of two Vector projected in XY Plane in Radians
        /// Considering positve rotation round the World ZAxis
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        /// input vector does not need to be unitized
        static member inline angleTwoPiProjectedInXYPlane (a:Vec) (b:Vec)   = 
            if abs(a.X)<Vec.SqrtEpsilon && abs(a.Y)<Vec.SqrtEpsilon then failwithf "Vec.angleTwoPiProjectedInXYPlane: input vector a is vertical or zero length:%A" a
            if abs(b.X)<Vec.SqrtEpsilon && abs(b.Y)<Vec.SqrtEpsilon then failwithf "Vec.angleTwoPiProjectedInXYPlane: input vector b is vertical or zero length:%A" b
            let va = Vec(a.X, a.Y, 0.0)  // project to xy plane
            let vb = Vec(b.X, b.Y, 0.0)  // project to xy plane
            let ang = anglePi va vb //TODO could be optimized with 2D math
            if (cross va vb).Z >= 0.0 then ang
            else                           Math.PI * 2. - ang
    
        /// Returns positive angle of two Vector projected in XY Plane in Degrees
        /// Considering positve rotation round the World ZAxis
        /// Range:  0 to 360 degrees
        /// input vector does not need to be unitized
        static member inline angle360ProjectedInXYPlane (a:Vec) (b:Vec)   = angleTwoPiProjectedInXYPlane a b |> toDegrees
    
    
        /// Returns positive angle of Vector to XAxis projected in XY Plane in Radians
        /// Considering positve rotation round the World ZAxis
        /// Range: 0.0 to 2 PI ( = 0 to 360 degrees)
        /// input vector does not need to be unitized
        static member inline angleTwoPiProjectedToXAxis(vec:Vec)   = 
            if abs(vec.X)<Vec.SqrtEpsilon && abs(vec.Y)<Vec.SqrtEpsilon then failwithf "Vec.angleTwoPiProjectedToXAxis: input vector is vertical or zero length:%A" vec
            let v = Vec(vec.X, vec.Y, 0.0) |> unitize // project to xy plane
            let ang = anglePiFast Vec.XAxis v //TODO could be optimized with 2D math
            if (cross Vec.XAxis v).Z >= 0.0 then  ang
            else                                       Math.PI * 2. - ang
    
        /// Returns positive angle of Vector to XAxis projected in XY Plane in Degrees
        /// Considering positve rotation round the World ZAxis
        /// Range: 0 to 360 degrees
        /// input vector does not need to be unitized
        static member inline angle360ProjectedToXAxis(v:Vec)   =  angleTwoPiProjectedToXAxis v |> toDegrees
    
    
    
         //--------------------end angels 360:---------------------------
    
        /// Returns positive or negative  slope of a vector in Radians
        /// in relation to XY Plane
        static member inline slopeRad (v:Vec) = 
            let f = Vec(v.X, v.Y, 0.0)
            if v.Z >= 0.0 then  angleHalfPi v f
            else              -(angleHalfPi v f)
    
        /// Returns positive or negative slope of a vector in Degrees
        /// in relation to XY Plane
        static member inline slopeDeg (v:Vec) = 
            let f = Vec(v.X, v.Y, 0.0)
            if v.Z >= 0.0 then  angle90 v f
            else              -(angle90 v f)
    
        /// Returns positive or negative slope of a vector in Percent
        /// in relation to XY Plane
        /// 100% = 45 degrees
        static member inline slopePercent (v:Vec) = 
            if abs(v.Z) < Vec.SqrtEpsilon then failwithf "Vec.slopePercent: Can't get Slope from vertical vector %O" v
            let f = Vec(v.X, v.Y, 0.0)
            100.0 * (v.Z/f.Length)
    
    
        /// Set vector to a given Length
        /// Fails on tiny vectors (v.SquareLength < Vec.SqrtEpsilon)
        static member inline setLength (len:float) (v:Vec) = 
            let l  = v.SquareLength
            if l < Vec.SqrtEpsilon then failwithf "Vec.setLength: Cant set length of tiny vector %O" v
            let f = len / sqrt(l) in Vec(v.X*f, v.Y*f, v.Z*f)
    
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
        static member inline isSameOrientation (orientationToCkeck:Vec) (v:Vec) = 
            orientationToCkeck * v > 0.0
    
        /// Returns a horizontal vector that is perpendicular to the given vector.
        /// just: Vec(v.Y, -v.X, 0.0)
        /// Not of same length, NOT unitized
        /// Rotated counter clockwise in top view.
        /// Fails on vertical input vector where resulting vector would be of almost zero length (Vec.SqrtEpsilon)
        static member inline perpendicularVecInXY (v:Vec) = 
            // test:
            //let rnd  =  Random()
            //let next()   =  -1.0  +  2.0 * rnd.NextDouble()
            //for i = 0 to 20 do
            //    let v  =  Vec(next()   , next()  , next()  ) |> Vec.unitize
            //    let o  =  Point3d.Origin
            //    let e  =  o + v
            //    Scripting.AddLine(o, e)  |> ignore
            //    let p  =  Vec.perpendicularVecInXY(e-o) |> Vec.scale 0.1
            //    let b  =  Pnt.normalOfTwoPointsInXY(o, e) |> Vec.scale 0.2
            //    let g  =  Vec.cross (e-o) Vec.ZAxis|> Vec.scale 0.2
            //    //Scripting.AddLine(e, e + p)  |> ignore
            //    //Scripting.AddLine(e, e + b)  |> ignore
            //    Scripting.AddLine(e, e + g)  |> ignore
            let x = v.Y
            let y = -v.X  // this is the same as: Vec.cross v Vec.ZAxis
            let len = sqrt(x*x + y*y)
            if len < Vec.SqrtEpsilon then
                failwithf "Vec.perpendicularVecInXY: Cannot find perpendicularVecInXY for vertical vector %O" v
            else
                Vec(x, y, 0.0)
    
    
        /// Returns a vector that is perpendicular to the given vector an in the same vertical Plane .
        /// Projected into the XY Plane input and output vectors are parallell and of same orientation.
        /// Not of same length, not unitized
        /// Fails on vertical input vector where resulting vector would be of almost zero length (Vec.SqrtEpsilon)
        static member inline perpendicularVecInVerticalPlane (v:Vec) = 
            let hor = Vec(v.Y, -v.X, 0.0)
            let r = cross v hor
            if r.IsTiny(Vec.SqrtEpsilon) then failwithf "Vec.perpendicularVecInVerticalPlane: Cannot find perpendicularVecInVerticalPlane for vertical vector %O" v
            if v.Z < 0.0 then -r else r
    
    
    
        /// Checks if Angle between two vectors is Below one Degree
        /// Ignores vector orientation
        /// Fails on zero length vectors, tolerance 0.00012
        static member  isAngleBelow1Degree(a:Vec, b:Vec) = //(prevPt:Point3d, thisPt:Point3d, nextPt:Point3d) = 
            //let a = prevPt - thisPt
            //let b = nextPt - thisPt
            let sa = a.SquareLength
            if sa < Vec.SqrtEpsilon then
                failwithf "Vec Duplicate points: isAngleBelow1Degree: prevPt - thisPt: %s.SquareLength < Vec.SqrtEpsilon; nextPt - thisPt:%s " a.ToNiceString b.ToNiceString
            let sb = b.SquareLength
            if sb < Vec.SqrtEpsilon then
                failwithf "Vec Duplicate points: isAngleBelow1Degree: nextPt - thisPt: %s.SquareLength < Vec.SqrtEpsilon; prevPt - thisPt:%s " b.ToNiceString a.ToNiceString
            let lena = sqrt sa
            let lenb = sqrt sb
            if lena < Scripting.Doc.ModelAbsoluteTolerance then
                failwithf "Vec Duplicate points: isAngleBelow1Degree: prevPt - thisPt: %s < Scripting.Doc.ModelAbsoluteTolerance: %f; nextPt - thisPt:%s " a.ToNiceString Scripting.Doc.ModelAbsoluteTolerance b.ToNiceString
            if lenb < Scripting.Doc.ModelAbsoluteTolerance then
                failwithf "Vec Duplicate points: isAngleBelow1Degree: nextPt - thisPt: %s < Scripting.Doc.ModelAbsoluteTolerance: %f; prevPt - thisPt:%s " b.ToNiceString Scripting.Doc.ModelAbsoluteTolerance a.ToNiceString
            let au = a * (1.0 / lena)
            let bu = b * (1.0 / lenb)
            abs(bu*au) > 0.999847695156391 // = cosine of 1 degree (2 degrees would be =  0.999390827019096)
    
            // for fsi: printfn "%.18f" (cos( 0.25 * (System.Math.PI / 180.)))
    
        /// Checks if Angle between two vectors is Below 0.25 Degrees
        /// Ignores vector orientation
        /// Fails on zero length vectors, tolerance 0.00012
        static member  isAngleBelowQuaterDegree(a:Vec, b:Vec) = //(prevPt:Point3d, thisPt:Point3d, nextPt:Point3d) = 
            //let a = prevPt - thisPt
            //let b = nextPt - thisPt
            let sa = a.SquareLength
            if sa < Vec.SqrtEpsilon then
                failwithf "Vec Duplicate points: isAngleBelowQuaterDegree: prevPt - thisPt: %s.SquareLength < Vec.SqrtEpsilon; nextPt - thisPt:%s " a.ToNiceString b.ToNiceString
            let sb = b.SquareLength
            if sb < Vec.SqrtEpsilon then
                failwithf "Vec Duplicate points: isAngleBelowQuaterDegree: nextPt - thisPt: %s.SquareLength < Vec.SqrtEpsilon; prevPt - thisPt:%s " b.ToNiceString a.ToNiceString
            let lena = sqrt sa
            let lenb = sqrt sb
            if lena < Scripting.Doc.ModelAbsoluteTolerance then
                failwithf "Vec Duplicate points: isAngleBelowQuaterDegree: prevPt - thisPt: %s < Scripting.Doc.ModelAbsoluteTolerance: %f; nextPt - thisPt:%s " a.ToNiceString Scripting.Doc.ModelAbsoluteTolerance b.ToNiceString
            if lenb < Scripting.Doc.ModelAbsoluteTolerance then
                failwithf "Vec Duplicate points: isAngleBelowQuaterDegree: nextPt - thisPt: %s < Scripting.Doc.ModelAbsoluteTolerance: %f; prevPt - thisPt:%s " b.ToNiceString Scripting.Doc.ModelAbsoluteTolerance a.ToNiceString
            let au = a * (1.0 / lena)
            let bu = b * (1.0 / lenb)
            abs(bu*au) > 0.999990480720734 // = cosine of 0.25 degree: printfn "%.18f" (cos( 0.25 * (System.Math.PI / 180.)))
    
        *) 
 
    
#if INTERACTIVE 
module Test =  
    open Vectors
    
    let quat() =
        let v = Vec(0.0 , 0.0 , 9.0 )
        let q = Quat.createFromDegree(v, 90.0) 
        let pt = Pnt(5.0 , 0.0 , 0.0 )
        let npt = q.Rotate(pt) 
        printfn $"q  {pt} -> Tran:{q.Rotate(pt)}"
        
    let ang() =
        let a = Vec(0.0 , 0.0 , 1.0 )
        for x in [0. .. 0.005 .. 1.0] do
            let b = Vec(0.0 , x, -1.0 )
            let an = Vec.angle180 a b 
            let ani = Vec.angle90 a b 
            printfn $"Angle {an} , {ani} {an+ani}  "    
        
        
        
        
        
Test.ang() 
#endif     
        
        
        
        
        
        
        
        
        
        
    
    
    
