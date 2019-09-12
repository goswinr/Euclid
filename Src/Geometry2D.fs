#if COMPILED
namespace Geometry2D
#endif
open System

/// 2D Points, Vectors and Rotations
module Vectors = 
    
    /// Tolerance for zero length: 1e-12
    [<Literal>]
    let zeroLenghtTol = 1e-12 
    
    /// Converts Angels from Degrees to Radians
    let inline toRadians degrees =  0.0174532925199433 * degrees 

    /// Converts Angels from Radians to Degrees
    let inline toDegrees radians = 57.2957795130823 * radians 
    
    /// A 2D Point (3D Points are called 'Pnt') 
    [<Struct;NoEquality;NoComparison>]// because its made up from floats
    type Pt =
        val X : float
        val Y : float
        new (x,y) = {X=x; Y=y}
        override p.ToString() =  
            //sprintf "Pt(X=%g, Y=%g)" p.X p.Y
            sprintf "Pt(X: %.04f, Y: %.04f)" p.X p.Y
    
    /// A 2D Vector (3D Vectors are called 'Vec') 
    [<Struct;NoEquality;NoComparison>]// because its made up from floats
    type Vc =
        val X : float
        val Y : float
        new (x,y) = {X=x; Y=y}
        new (start:Pt, ende:Pt) =  {X=ende.X - start.X ; Y= ende.Y - start.Y }
        member inline v.Length    =  sqrt((v.X * v.X)  + (v.Y * v.Y))
        override v.ToString() =  
            //sprintf "Vc(X=%g, Y=%g)" v.X v.Y
            sprintf "Vc(X: %.04f, Y: %.04f)" v.X v.Y

    /// A 2D Vector guaranteed to be unitized (3D Unit Vectors are called 'UnitVec') 
    [<Struct;NoEquality;NoComparison>]// because its made up from floats
    type UnitVc =
        val X : float
        val Y : float

        /// Unsave internal constructor,  public only for inlining.
        //[<Obsolete("Unsave internal constructor,  public only for inlining.") >] //TODO
        new (x,y) = {X=x; Y=y}
        
        override v.ToString() =  
            //sprintf "UnitVc(X=%g, Y=%g)" v.X v.Y
            sprintf "UnitVc(X: %.04f, Y: %.04f)" v.X v.Y 
        
        //new (v:Vc) = // use v.Unitized instead
        //    let l = sqrt((v.X * v.X)  + (v.Y * v.Y))
        //    if l < zeroLenghtTol then failwithf "Vc.Unitize failed on very small vector %O" v
        //    {X= v.X/l ; Y= v.Y/l} 

         /// 2D cross product. Its Just a scalar
        member inline a.Cross (b:Vc)     = a.X*b.Y - a.Y*b.X
        member inline a.Cross (b:UnitVc) = a.X*b.Y - a.Y*b.X

        /// 90 degree rotation counter clockwise
        member inline v.RotatedCCW = UnitVc( -v.Y,   v.X  )

        /// 90 degree rotation clockwise
        member inline v.RotatedCW  = UnitVc(  v.Y,  -v.X  )

        static member inline (~- ) (v:UnitVc) = UnitVc( -v.X , -v.Y )

        static member inline ( * )  (a:UnitVc  , f:float    ) = Vc (a.X * f , a.Y * f )
        static member inline ( * )  (f:float    , a:UnitVc  ) = Vc (a.X * f , a.Y * f )
        static member inline ( * )  (a:UnitVc  , b:UnitVc  ) = a.X * b.X+ a.Y * b.Y  // dot product
        static member inline ( * )  (a:UnitVc  , b:Vc      ) = a.X * b.X+ a.Y * b.Y  // dot product
        static member inline ( * )  (a:Vc      , b:UnitVc  ) = a.X * b.X+ a.Y * b.Y  // dot product

        static member inline ( / )  (v:UnitVc, f:float) = if abs f > zeroLenghtTol then v * (1./f) else failwithf "%f too small for dividing %A, tol:%g" f v zeroLenghtTol

        static member inline ( - )  (a:UnitVc, b:UnitVc) = Vc (a.X - b.X , a.Y - b.Y )

        static member inline ( + )  (a:UnitVc, b:UnitVc) = Vc (a.X + b.X , a.Y + b.Y )
        static member inline ( + )  (a:Vc,     b:UnitVc) = Vc (a.X + b.X , a.Y + b.Y )
        static member inline ( + )  (a:UnitVc, b:Vc)     = Vc (a.X + b.X , a.Y + b.Y )
        
        static member inline XAxis  = UnitVc (1.0 , 0.0)
        static member inline YAxis  = UnitVc (0.0 , 1.0)
        
        /// Returns angle between two UnitVectors in Radians.
        /// Takes vector orientation into account.
        /// Range 0.0 to PI( = 0 to 180 degree)
        static member inline anglePi (a:UnitVc) (b:UnitVc) = 
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

        /// Returns positive angle between two UnitVectors in Radians. Ignores orientation. 
        /// Range 0.0 to PI/2 ( = 0 to 90 degree)
        static member inline angleHalfPi (a:UnitVc) (b:UnitVc) = 
            let dot =  a * b
            let dotAbs = abs dot
            if dotAbs < 0.98 then  
                acos dotAbs 
            else
                if dot < 0. then 2.0 * asin((-a - b).Length * 0.5)
                else             2.0 * asin(( a - b).Length * 0.5) 

    type Vc with

        static member Zero = Vc(0.0 , 0.0)  // needed by 'Array.sum'

        static member inline (~- ) (v:Vc) = Vc ( -v.X , -v.Y )

        static member inline ( * )  (a:Vc  , f:float) = Vc (a.X * f , a.Y * f ) // scale Vector
        static member inline ( * )  (f:float, a:Vc  ) = Vc (a.X * f , a.Y * f ) // scale Vector
        static member inline ( * )  (a:Vc  , b:Vc  ) = a.X*b.X + a.Y*b.Y  // dot product

        static member inline ( / )  (v:Vc, f:float) = if abs f > zeroLenghtTol then v * (1./f) else failwithf "%f too small for dividing %A, tol:%g" f v zeroLenghtTol

        static member inline ( - )  (a:Vc, b:Vc) = Vc (a.X - b.X , a.Y - b.Y )

        static member inline ( + )  (a:Vc, b:Vc) = Vc (a.X + b.X , a.Y + b.Y )
        
        static member inline XAxis  = Vc (1.0 , 0.0)
        static member inline YAxis  = Vc (0.0 , 1.0)
        
        static member inline unitize (v:Vc) =  
            let l = sqrt((v.X * v.X)  + (v.Y * v.Y))
            if l < zeroLenghtTol then failwithf "Vc.unitize failed on very small vector %O, to: %g" v zeroLenghtTol 
            UnitVc( v.X/l , v.Y/l)
        
        member inline v.Unitized  = 
            let l = sqrt((v.X * v.X)  + (v.Y * v.Y))
            if l < zeroLenghtTol then failwithf "Vc.Unitized failed on very small vector %O, to: %g" v zeroLenghtTol 
            UnitVc( v.X/l , v.Y/l)     
            
        /// 2D cross product. Its Just a scalar
        member inline a.Cross (b:Vc)     = a.X*b.Y - a.Y*b.X
        member inline a.Cross (b:UnitVc) = a.X*b.Y - a.Y*b.X

        /// 90 degree rotation counter clockwise
        member inline v.RotatedCCW = Vc( -v.Y,   v.X  )

        /// 90 degree rotation clockwise
        member inline v.RotatedCW  = Vc(  v.Y,  -v.X  )

        // Square Length
        member inline v.LengthSq  =       (v.X * v.X)  + (v.Y * v.Y)

        // member inline v.Length    =  sqrt((v.X * v.X)  + (v.Y * v.Y)) defined above

    type Pt with

        static member Zero = Vc(0.0 , 0.0)  // needed by 'Array.sum'

        static member inline ( * )  (a:Pt  , f:float) = Pt (a.X * f , a.Y * f ) // scale Vector
        static member inline ( * )  (f:float, a:Pt  ) = Pt (a.X * f , a.Y * f ) // scale Vector

        static member inline ( - )  (a:Pt, b:Pt) = Vc (a.X - b.X , a.Y - b.Y )
        static member inline ( - )  (a:Pt, b:Vc) = Pt (a.X - b.X , a.Y - b.Y )

        static member inline ( + )  (v:Vc, p:Pt) = Pt (p.X + v.X , p.Y + v.Y )
        static member inline ( + )  (p:Pt, v:Vc) = Pt (p.X + v.X , p.Y + v.Y )
        static member inline ( + )  (a:Pt, b:Pt) = Pt (a.X + b.X , a.Y + b.Y )

        static member inline distSq (a:Pt) (b:Pt) =
            let x = a.X - b.X
            let y = a.Y - b.Y
            x*x + y*y

        static member inline dist (a:Pt) (b:Pt)  =
            let x = a.X - b.X
            let y = a.Y - b.Y
            sqrt(x*x + y*y)
    
    /// Represents a Rotation in 2D. Stores the sine and cosine of an angle.        
    [<Struct; NoEquality; NoComparison>]
    type Rot = 
        val sin : float
        val cos : float
        
        /// Unsave internal constructor,  public only for inlining.
        //[<Obsolete("Unsave internal constructor,  public only for inlining.") >]
        new (sin, cos) = {sin = sin; cos = cos} 
        
        override r.ToString() =  
            let valid = let v = r.sin*r.sin + r.cos*r.cos in 0.999999999 < v && v < 1.000000001 
            if valid then sprintf "Rot( %g Degrees)"        (r.sin|>asin|>toDegrees)  
            else sprintf " an invalid Rot(sin: %g, cos: %g) " (r.sin|>asin|>toDegrees) (r.cos|>acos|>toDegrees)
                 
        
        ///Construct Rot from angle in Radians 
        static member fromRadians alpha =  
            Rot (sin alpha, cos alpha)
        
        /// Construct Rot from angle in Degrees 
        static member fromDegrees alpha = 
            let rad =  toRadians alpha
            Rot (sin rad, cos rad) 
        
        /// checks the stored sine and cosine correspond to the sdame angle
        member inline r.IsValid =  
            let v = r.sin*r.sin + r.cos*r.cos
            0.999999999 < v && v < 1.000000001 
            
        member inline r.InDegree = r.sin |> Math.Asin |> toDegrees
        
        member inline r.InRadian = r.sin |> Math.Asin 
        
        /// Rotate the Vector around 0,0 .  Counter Clockwise
        member inline r.Rotate (v:Vc) = Vc (r.cos*v.X - r.sin*v.Y,  r.sin*v.X + r.cos*v.Y)
        
        /// Rotate the UnitVector around 0,0 .  Counter Clockwise
        member inline r.Rotate (v:UnitVc) = UnitVc (r.cos*v.X - r.sin*v.Y,  r.sin*v.X + r.cos*v.Y)
        
        /// Rotate the UnitVector around 0,0 .  Counter Clockwise
        member inline r.Rotate (p:Pt) = Pt (r.cos*p.X - r.sin*p.Y,  r.sin*p.X + r.cos*p.Y)
        
        /// Rotate the Point around center Point and a X aligned axis, from Y to Z Axis, Counter Clockwise looking from right.
        member inline r.RotateWithCenter (cen:Pt,  pt:Pt) =  
            let v = pt-cen
            Pt (r.cos*v.X - r.sin*v.Y + cen.X,  r.sin*v.X + r.cos*v.Y+ cen.Y)
        
        /// Angle given in degrees    
        static member inline witCen  (cen:Pt)  angDegree (p:Pt)  = (Rot.fromDegrees angDegree).RotateWithCenter(cen,p)    
        
        /// Angle given in degrees    
        static member inline pt     angDegree (p:Pt)  = (Rot.fromDegrees angDegree).Rotate p    
        /// Angle given in degrees
        static member inline vc     angDegree (v:Vc)  = (Rot.fromDegrees angDegree).Rotate v    
        /// Angle given in degrees
        static member inline unitVc angDegree (v:Vc)  = (Rot.fromDegrees angDegree).Rotate v    
            

/// Set these functions to enable debugging
module Debug =  
    open Vectors
    
    let mutable addDot = fun (msg:string) (pt:Pt) -> () 
    
    let mutable addPt  = fun (pt:Pt) -> () 
    
    let mutable addLine = fun (a:Pt,b:Pt) -> () 
    
    let mutable addPolyLine = fun (ps:Pt seq) -> () 

module BBoxes = 
    open Vectors

    (*
    // Moves both 'mi' and 'ma' by 'expansion' amount away from each other.
    // The interval increase by expansion.
    // If expansion is negative it shrinks the interval. It also makes sure that there is no overflow
    // when 'mi' and 'ma' are closer than the negative expansion.
    // The mid point between  'mi' and 'ma' will be returned in that case.
    let inline expandOrShrink(mi, ma, expansion) =
        let mit = mi - expansion * 0.5
        let mat = ma + expansion * 0.5
        if mit <= mat then // All good,  No overflow
            mit, mat
        else // Overflow! Set both to the same mid point
            let mid = mi + (ma-mi) * 0.5
            mid, mid
    *) 

    [<Struct;NoEquality;NoComparison>]
    type BBox =
        val MinX : float
        val MinY : float
        val MaxX : float
        val MaxY : float
        
        /// Adds the Expansion value is used to shrink lowwer bound and increase upper bound.
        /// Total size is bigger by expansion times two.
        /// If expansion is negative it shrinks the Box. It also makes sure that there is no overflow 
        /// when the negative expansion is bigger than the size.
        new (a:Pt , b:Pt,  expansion ) = 
            // sort min and max values ( not useing allocating tuples for swaping) 
            let mutable minX = a.X  
            let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X 
            let mutable minY = a.Y  
            let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
            // expand X:
            // Moves both 'mi' and 'ma' by 'expansion' amount away from each other.
            // The interval increase by expansion.
            // If expansion is negative it shrinks the interval. It also makes sure that there is no overflow
            // when 'mi' and 'ma' are closer than the negative expansion.
            // The mid point between  'mi' and 'ma' will be returned in that case.
            let mutable minXCh = minX - expansion 
            let mutable maxXCh = maxX + expansion 
            if minXCh > maxXCh then  // Overflow! Set both to the same mid point
                let mid = minX + (maxX-minX) * 0.5
                minXCh <- mid
                maxXCh <- mid
            // expand Y:
            let mutable minYCh = minY - expansion 
            let mutable maxYCh = maxY + expansion 
            if minYCh > maxYCh then  // Overflow! Set both to the same mid point
                let mid = minY + (maxY-minY) * 0.5
                minYCh <- mid
                maxYCh <- mid

            {MinX = minXCh
             MinY = minYCh
             MaxX = maxXCh
             MaxY = maxYCh}

        new (a:Pt , b:Pt ) =
            // sort min and max values ( not useing allocating tuples for swaping) 
            let mutable minX = a.X  
            let maxX = if b.X > minX then b.X else minX <- b.X ;  a.X 
            let mutable minY = a.Y  
            let maxY = if b.Y > minY then b.Y else minY <- b.Y ;  a.Y
            {MinX = minX
             MinY = minY
             MaxX = maxX
             MaxY = maxY}


        member inline b.Height = b.MaxY - b.MinY

        member inline b.Width  = b.MaxX - b.MinX

        member inline b.Diagonal = Vc(b.MaxX - b.MinX, b.MaxY - b.MinY)

        member inline b.Center = Pt( (b.MaxX + b.MinX)*0.5, (b.MaxY + b.MinY)*0.5 )

        /// As counterclockwise closed loop (last Pt = first Pt)
        /// Starting at bbox.Min
        member b.AsPolyLine = [|
                Pt(b.MinX, b.MinY)
                Pt(b.MaxX, b.MinY)
                Pt(b.MaxX, b.MaxY)
                Pt(b.MinX, b.MaxY)
                Pt(b.MinX, b.MinY)
                |]

        override b.ToString() = sprintf "BBox width %g height %g (at X=%g, Y=%g)" b.Width b.Height b.MinX b.MinY

    /// Returns true if the two bounding boxes do overlap
    let overlap(a:BBox) (b:BBox) =
        not (  b.MinX > a.MaxX
            || b.MaxX < a.MinX
            || b.MaxY < a.MinY
            || b.MinY > a.MaxY )

    //open Rhino
    //type rs = Rhino.Scripting

    //let draw( aStart:Pt , aEnd:Pt , bStart:Pt , bEnd :Pt, tol )  =
    //    let minXa,maxXa = minMax aStart.X  aEnd.X
    //    let minYa,maxYa = minMax aStart.Y  aEnd.Y
    //    let minXb,maxXb = minMax bStart.X  bEnd.X
    //    let minYb,maxYb = minMax bStart.Y  bEnd.Y
    //    let minXaCh,maxXaCh = expandOrShrink(minXa, maxXa,tol)
    //    let minYaCh,maxYaCh = expandOrShrink(minYa, maxYa,tol)
    //    let minXbCh,maxXbCh = expandOrShrink(minXb, maxXb,tol)
    //    let minYbCh,maxYbCh = expandOrShrink(minYb, maxYb,tol)
    //    try rs.AddLine2D(minXaCh, minYaCh, minXaCh, maxYaCh) |> rs.setLayer $"X::Box a" with _ -> eprintfn "cant draw zero Length Line"
    //    try rs.AddLine2D(minXaCh, maxYaCh, maxXaCh, maxYaCh) |> rs.setLayer $"X::Box a" with _ -> eprintfn "cant draw zero Length Line"
    //    try rs.AddLine2D(maxXaCh, maxYaCh, maxXaCh, minYaCh) |> rs.setLayer $"X::Box a" with _ -> eprintfn "cant draw zero Length Line"
    //    try rs.AddLine2D(maxXaCh, minYaCh, minXaCh, minYaCh) |> rs.setLayer $"X::Box a" with _ -> eprintfn "cant draw zero Length Line"
    //    try rs.AddLine2D(minXbCh, minYbCh, minXbCh, maxYbCh) |> rs.setLayer $"X::Box b" with _ -> eprintfn "cant draw zero Length Line"
    //    try rs.AddLine2D(minXbCh, maxYbCh, maxXbCh, maxYbCh) |> rs.setLayer $"X::Box b" with _ -> eprintfn "cant draw zero Length Line"
    //    try rs.AddLine2D(maxXbCh, maxYbCh, maxXbCh, minYbCh) |> rs.setLayer $"X::Box b" with _ -> eprintfn "cant draw zero Length Line"
    //    try rs.AddLine2D(maxXbCh, minYbCh, minXbCh, minYbCh) |> rs.setLayer $"X::Box b" with _ -> eprintfn "cant draw zero Length Line"

module Intersect = 
    open Vectors
    open BBoxes

    //[<Struct>]
    //type IntersectionParamter =
    //    |NoParamColinear
    //    |Param of float

    ///// Retuns the parameter on vector 'va' where 'va' and 'vb' intersect as endless rays.
    ///// If they start from points 'a' and 'b' respectivley.
    //let getParameter (a:Pt, va:Vc, b:Pt,  vb:Vc) =
    //    // https://www.youtube.com/watch?v=c065KoXooSw
    //    let nom = va.Cross vb
    //    if -zeroLenghtTol < nom && nom < zeroLenghtTol  then NoParamColinear
    //    else Param (((b-a).Cross(vb)) / nom)


    ///// Retuns the parameter on vector 'va' where 'va' and 'vb' intersect as endless rays.
    ///// If they start from points 'a' and 'b' respectivley.
    //let getParameterU (a:Pt, va:UnitVc , b :Pt ,  vb:UnitVc) =
    //    let nom = va.Cross vb
    //    if -zeroLenghtTol < nom && nom < zeroLenghtTol  then NoParamColinear
    //    else Param (((b-a).Cross(vb)) / nom)


    ///// Retuns the parameter on vector 'va' where 'va' and 'vb' intersect intersect as endless rays.
    ///// If they start from points 'a' and 'b' respectivley.
    ///// This might be infinity too. Does not check for colinear points !
    //let getParameterUnChecked (a:Pt, va:UnitVc, b:Pt,  vb:UnitVc) =
    //    ((b-a).Cross(vb)) / va.Cross vb
        
        
    /// Retuns the parameter on vector 'va' where 'va' and 'vb' intersect intersect as endless rays.
    /// If they start from points 'a' and 'b' respectivley.
    /// Pass in  va.Cross vb  is precomputed  and inverted  
    let inline private getXPara (a:Pt, vaXvbInverse:float, b:Pt,  vb:UnitVc) =
        // https://www.youtube.com/watch?v=c065KoXooSw
        ((b-a).Cross(vb)) * vaXvbInverse // va.Cross vb  is precomputed  and inverted 


    let inline private isParamStillBelowZeroAfterOffsets(ap:Pt, au:UnitVc, aXbInverse:float, bp:Pt, bu:UnitVc, snapThreshold:float) =
        let n = au.RotatedCCW * snapThreshold
        // TODO would it be enough to only compute one of these two? depending on the sign of aXbInverse ?
        getXPara(ap + n, aXbInverse,  bp, bu) < 0.0-snapThreshold
        &&
        getXPara(ap - n, aXbInverse,  bp, bu) < 0.0-snapThreshold

    let inline private isParamStillMoreThanLengthAfterOffsets(ap:Pt, au:UnitVc , aXbInverse:float, al:float, bp:Pt, bu:UnitVc, snapThreshold:float) =
        let n = au.RotatedCCW * snapThreshold
        getXPara(ap + n, aXbInverse,  bp, bu) > al+snapThreshold
        &&
        getXPara(ap - n, aXbInverse,  bp, bu) > al+snapThreshold


    type LineLineRelation =  
        // this DU could be also encoded via  Float NaN and infinity to avoid an extra object allocation
        |NoIntersection
        |Colinear // within threshold,  might still not  overlap,  needs to be checked via BBox
        |Parallel // more than threshold apart
        |BfromRight of struct ( float * float) // parameters for unit vector,  might be out of bound by snapThreshold
        |BfromLeft  of struct ( float * float) // parameters for unit vector,  might be out of bound by snapThreshold
    
    // TODO inline functions?

    let getRelation (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) :LineLineRelation=
        let aXb = au.Cross bu //precomputed  cross product 
        
        if abs(aXb) > zeroLenghtTol then  // not paralell
            let aXbInverse = 1./aXb // invert only once,  then pass on as inverted value
            let ta = getXPara (ap, aXbInverse, bp, bu)
        
            // parameter on first is below zero, so probably false unless closer than snapThreshold and almost colinear
            if ta < -snapThreshold && isParamStillBelowZeroAfterOffsets (ap, au, aXbInverse, bp, bu, snapThreshold) then   
                NoIntersection // no need to even check parameter on second segment

            // parameter on first segment is  beyond length ,  so probaly false unless closer than snapThreshold and colinear
            elif ta > al+snapThreshold && isParamStillMoreThanLengthAfterOffsets(ap, au, aXbInverse, al, bp, bu, snapThreshold) then   
                NoIntersection // no need to even check parameter on second segment

            // now checking if parameter on second lin is inside too:
            else
                // this might still be a very shallow intersection that counts as pralell
                let bXaInverse = -aXbInverse
                let tb = getXPara (bp, bXaInverse, ap, au)

                // parameter on second segment is  below zero ,  so probaly false unless closer than snapThreshold and colinear
                if tb < -snapThreshold && isParamStillBelowZeroAfterOffsets (bp, bu, bXaInverse, ap, au, snapThreshold) then  
                    NoIntersection

                // parameter on second segment is  beyond length ,  so probaly false unless closer than snapThreshold and colinear
                elif tb > bl + snapThreshold && isParamStillMoreThanLengthAfterOffsets (bp, bu, bXaInverse, bl, ap, au, snapThreshold) then  
                    NoIntersection

                else  
                    if aXb > 0.0 then BfromRight (ta, tb)
                    else              BfromLeft  (ta, tb)

        else // Colinear
            // probaly no itersection  unless closer than snapThreshold
            let perp = au.RotatedCCW // unit v
            let vab = ap-bp
            let dot = perp * vab // project vab onto unit vector
            if abs dot < snapThreshold then
                Colinear // paralle distance is less than snapThreshold distance,  TODO but actual overlap needs to be confirmed via BBox
            else
                Parallel // paralle distance is more than snapThreshold distance,

    let doIntersect (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) :bool =
        match getRelation(ap, au, al,  bp, bu, bl, snapThreshold)   with
        |NoIntersection -> false
        |Parallel       -> false
        |Colinear       -> true// TODO but actual overlap needs to be confirmed via BBox
        |BfromLeft _    -> true
        |BfromRight _   -> true
    
    
    type IntersectionPoint =
        |NoPoint
        |FromLeft  of Pt
        |FromRight of Pt
    
    /// Does also clamp point to actually be on line even if it is not quite in case of PreStart or PostEnd
    let getIntersectionPoint (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) : IntersectionPoint =
        match getRelation(ap, au, al,  bp, bu, bl, snapThreshold)   with
        |NoIntersection
        |Colinear
        |Parallel            -> NoPoint
        |BfromLeft  (ta, _ ) -> FromLeft  (ap + au * (max 0.0 (min al ta))) // clamp point to actually be on line even if it is not quite in case of PreStart or PostEnd
        |BfromRight (ta, _ ) -> FromRight (ap + au * (max 0.0 (min al ta))) 
    
    
    /// Start and End means the intesection point is within the threshold distance of start or end point respectively
    /// PreStart and PostEnd means intesection point is beyond the threshold distance,  but an offest by threshold distance would intersect within this 
    type IntersectionZone =  
        PreStart | Start | Middle | End | PostEnd

    let getZone(t:float, len,  snapThreshold) =
        if    t < -snapThreshold     then PreStart
        elif  t <  snapThreshold     then Start
        elif  t <  len-snapThreshold then Middle
        elif  t <  len+snapThreshold then End
        else                              PostEnd


#if INTERACTIVE
#r "nuget: FsEx, 0.8.0"
#endif

module Loops =
    open FsEx
    open Vectors
    open BBoxes
    open FsEx.ExtensionsArray
    
    /// The sign is negative if the loop is clockwise
    let getSignedArea(ps:Rarr<Pt>) =
        //https://helloacm.com/sign-area-of-irregular-polygon/
        let mutable area = 0.0
        let mutable t = ps.[0]
        for i=1 to ps.LastIndex do
            let n = ps.[i]
            let a = t.X - n.X
            let b = n.Y + t.Y
            area <- area + a*b
            t <- n
        area * 0.5

    /// A counter-clockwise,  closed series of points.
    /// Checks for too short segments
    /// Closes loop if not closed yet.
    /// Makes it Counterclockwise.
    /// Also check for self intersection.
    /// Does NOT remove colinear points.
    type Loop(points:Rarr<Pt>, minSegmentLength:float, snapThreshold:float) =
        
        let pts = 
            if points.Count<3 then failwithf $"There need to be at least 3 points in the list for the Loop constructor:\r\n{points.ToNiceString}" 
            
            let ps= Rarr<Pt>(points.Count+1)
            // check gap sizes
            let minLenSq = minSegmentLength * minSegmentLength
            ps.Add points.[0]
            for i = 1 to points.LastIndex do
                let pt = points.[i]
                if Pt.distSq ps.Last pt > minLenSq then
                    ps.Add pt
                else
                    // set last to average
                    ps.Last <- (ps.Last + pt) *0.5 
                    Debug.addDot $"short segm:{i-1}" pt
                    eprintfn $"Loop constructor: Segment {i-1} shorter than {snapThreshold} was skiped, it was just {Pt.dist ps.Last pt} long."
            // close
            if Pt.distSq ps.Last ps.First > minLenSq then
                ps.Add ps.First // add first to close loop
            else
                ps.Last <- ps.First // make sure they are exactly the same
            ps

        let segCount = pts.Count-1
        
        let segLastIdx = pts.Count-2

        // get Area also Reverse to make it counter clockwise if needed
        let area =
            let sa = getSignedArea pts
            if sa < 0. then  pts.Reverse() ;  -sa
            else sa 
        
        let mutable xmin, ymin = Double.MaxValue, Double.MaxValue // for overall bounding box
        let mutable xmax, ymax = Double.MinValue, Double.MinValue
        
        // loop again to precalculate vectors ,  unit vectors,   BBoxes,  and lengths
        let vcts, unitVcts, bboxes , lens=
            let vs   = Array.zeroCreate (segCount)
            let uvs  = Array.zeroCreate (segCount)
            let bs   = Array.zeroCreate (segCount)
            let lens = Array.zeroCreate (segCount)
            let mutable t = pts.[0]
            for ii = 1 to pts.LastIndex do // start at +1,  last = first
                let n = pts.[ii]
                let v = Vc(t, n)
                let l = v.Length
                let i = ii-1 // becaus loop starts at 1
                vs.[  i] <- v
                uvs.[ i] <- UnitVc( v.X/l , v.Y/l) // no check for div by zero needed,  since minSpacing is already checked
                bs.[  i] <- BBox(t, n, snapThreshold)
                lens.[i] <- l
                // overall bounding box:
                xmin <- min xmin t.X
                ymin <- min ymin t.Y
                xmax <- max xmax t.X
                ymax <- max ymax t.Y
                // swap:
                t <- n
            vs, uvs,  bs , lens

        do
            // Test for 180 U-turns
            // angle 160 degrees, dot product of unit vectors: -0.93969
            // angle 170 degrees, dot product of unit vectors: -0.984808
            // angle 178 degrees, dot product of unit vectors: -0.999391
            // Check ther is no U-Turn between 170 and 180 degrees
            let mutable t = unitVcts.[0]
            for ii=1 to segLastIdx do
                let n = unitVcts.[ii]
                if t*n < -0.984808 then   
                    Debug.addDot $"+170° ?" pts.[ii]
                    failwithf "Lines for Loop make a kink between 170 and 180 degrees."
                t <- n

            // check for self intersection, 
            if vcts.Length > 3 then // a triangle is cover by angle checks above
                // checking second last and last
                if Intersect.doIntersect( pts.[segLastIdx]  , unitVcts.[segLastIdx]  , lens.[segLastIdx]   , pts.[1], unitVcts.[1], lens.[1] , snapThreshold ) then   
                    Debug.addDot $"self X last with second" pts.[segLastIdx]
                    Debug.addPolyLine pts
                    failwithf "Loop has self intersection last with second"
                if Intersect.doIntersect( pts.[segLastIdx-1], unitVcts.[segLastIdx-1], lens.[segLastIdx-1] , pts.[0], unitVcts.[0], lens.[0] , snapThreshold ) then   
                    Debug.addDot $"self X second last with first" pts.[segLastIdx-1]
                    Debug.addPolyLine pts
                    failwithf "Loop has self intersection second last with first"
                // check each segment with all other segemnts
                for i = 1 to segLastIdx do // start at second ,  (last and first do intersect)
                    // TODO quadratic O !  replace with sweep line algorithm ?
                    let ap = pts.[i]
                    let au = unitVcts.[i]
                    let al = lens.[i]
                    for j = i+2 to segLastIdx do
                        let bp = pts.[j]
                        let bu = unitVcts.[j]
                        let bl = lens.[j]
                        if Intersect.doIntersect(ap, au, al,  bp, bu, bl, snapThreshold ) then 
                            Debug.addDot $"self X:{i}+{j}" bp
                            Debug.addPolyLine pts
                            failwith $"Loop of {points.Count} Points has self intersection."

        /// Without sign,  since loop is guaranteed to be Counter Clockwise
        member _.Area = area

        /// This list is one item shorter than Points
        member _.BBoxes = bboxes

        /// This list is one item shorter than Points
        member _.Vectors = vcts

        /// This list is one item shorter than Points
        member _.UnitVectors = unitVcts

        /// This list is one item shorter than Points
        member _.Lengths = lens

        /// This list is one item Longer than Vectors , BBoxes or Lengths
        /// Last Point equals first Point
        member _.Points = pts
        
        /// One more than Vector count 
        member val PointCount = pts.Count
        
        /// One less than Points count 
        member val VecCount = vcts.Length  
        
        /// The overall Bounding Box,  including snapThreshold
        member val BoundingBox = BBox(Pt(xmin, ymin) , Pt(xmax, ymax) , snapThreshold) 
        
        member _.MinSegmentLength = minSegmentLength
        
        member _.SnapThreshold = snapThreshold

    
    open Intersect
    
    [<RequireQualifiedAccess>]
    type ContinueOn = A | B 
    
    type Location = { 
        aIdx:int
        bIdx:int
        at:float // parameter on unitvector of A segment
        bt:float // parameter on unitvector of B segment
        dir: ContinueOn 
        }
    
    // now develeped in Geeometry2D test:
    
    //let booleanIntersection (loopA:Loop, loopB:Loop) :Rarr<Loop> = 
    //    if loopA.SnapThreshold <> loopB.SnapThreshold then eprintfn $"Loop.union: loopA.SnapThreshold {loopA.SnapThreshold} <> loopB.SnapThreshold {loopB.SnapThreshold}"
    //    if loopA.MinSegmentLength <> loopB.MinSegmentLength then eprintfn $"Loop.union: loopA.MinSegmentLength {loopA.MinSegmentLength} <> loopB.MinSegmentLength {loopB.MinSegmentLength}"
        
    //    if not <| BBoxes.overlap loopA.BoundingBox loopB.BoundingBox then  
    //        Rarr.empty 
    //    else 
    //        let aLen  = loopA.VecCount
    //        let bLen  = loopB.VecCount
    //        let aBox  = loopA.BBoxes
    //        let bBox  = loopB.BBoxes
    //        let aPts  = loopA.Points
    //        let bPts  = loopB.Points
    //        let aUnit = loopA.UnitVectors       
    //        let bUnit = loopB.UnitVectors       
    //        let aLens = loopA.Lengths       
    //        let bLens = loopB.Lengths         
            
    //        let locs = Rarr<Location>() 
            
    //        /// first collect just the intersection points
    //        for ai = 0 to aLen-1 do  
    //            for bi = 0 to bLen-1 do // TODO quadratic!  replace with sweep line algorithm 
    //                if BBoxes.overlap aBox.[ai] bBox.[bi] then 
    //                    match getRelation(aPts.[ai], aUnit.[ai], aLens.[ai], bPts.[bi], bUnit.[bi], bLens.[bi], loopA.SnapThreshold) with 
    //                    |NoIntersection ->  ()
    //                    |Parallel       ->  ()  // more than threshold apart 
    //                    |Colinear ->  failwith "implementation for Colinear overlap missing" //TODO
                            
    //                    // parameters for unit vector,  might be out of bound by snapThreshold
    //                    |BfromRight (at, bt)  ->  locs.Add {aIdx = ai;  bIdx = bi;  at=at; bt=bt ; dir = ContinueOn.B} 
    //                    |BfromLeft  (at, bt)  ->  locs.Add {aIdx = ai;  bIdx = bi;  at=at; bt=bt ; dir = ContinueOn.A} // swaping a and b would yield boolean union !
            
    //        /// second loop over points
    //        if locs.IsEmpty then 
    //            Rarr.empty 
    //        elif locs.Count % 2 = 1 then  
    //            failwith "implementation for odd location count missing"
    //        else 
    //            let resPts = Rarr<Pt>() 
                
    //            let inline addPt(l:Location) =  
    //                resPts.Add (aPts.[l.aIdx] +  aUnit.[l.aIdx] * l.at) // TODO add clamping ?
                
    //            let inline addPts(si, ei, fromPts:Rarr<Pt>) =  
    //                if si<=ei then  
    //                    for i=si to ei do resPts.Add (fromPts.[i]) 
    //                else 
    //                    for i=si to fromPts.Count-2 do resPts.Add (fromPts.[i]) // -2 because start and end are the same
    //                    for i=0  to ei              do resPts.Add (fromPts.[i]) 
                
                
    //            locs |> Rarr.sortInPlaceBy(fun l -> l.aIdx, l.at) 
    //            for t, n in Seq.thisNext locs do  
    //                addPt t
    //                match t.dir with
    //                |ContinueOn.A ->  
    //                    Debug.addDot $"onA: {resPts.LastIndex}" resPts.Last
    //                    addPts(t.aIdx+1, n.aIdx, aPts) ; 
    //                |ContinueOn.B ->  
    //                    Debug.addDot $"onB: {resPts.LastIndex}" resPts.Last
    //                    addPts(t.bIdx+1, n.bIdx, bPts) ; 
                
    //            Rarr.singelton <| Loop(resPts,loopA.MinSegmentLength, loopA.SnapThreshold )  
                            
    
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
