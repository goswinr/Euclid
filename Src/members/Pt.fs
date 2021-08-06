namespace FsEx.Geo

open System



/// Members and operators for 2D Points, Vectors and Rotations
[<AutoOpen>]
module AutoOpenPt = 
    open Util
    
    type Pt with      
        
        member inline pt.IsOrigin = pt.X = 0.0 && pt.Y = 0.0 
        member inline v.IsAlomstOrigin tol = abs v.X < tol && abs v.Y < tol 
        
        //member inline v.IsInValid =  Double.IsNaN v.X || Double.IsNaN v.Y || Double.IsNaN v.Z || Double.IsInfinity v.X || Double.IsInfinity v.Y || Double.IsInfinity v.Z
        
        member inline pt.WithX x = Pt (x ,pt.Y) // returns new Vector with new x coordinate, y and z the same as before
        member inline pt.WithY y = Pt (pt.X, y)
        member inline pt.WithZ z = Pnt (pt.X ,pt.Y, z)
        
        /// Returns the distance between two points
        member inline p.DistanceTo (b:Pt) = let v = p-b in sqrt(v.X*v.X + v.Y*v.Y )
       
        /// Returns the squared distance bewteen two points.
        /// This operation is slighty faster than the distance function, and sufficient for many algorithms like finding closest points.
        member inline p.DistanceSqTo (b:Pt) = let v = p-b in  v.X*v.X + v.Y*v.Y 
        
        /// Returns the distance fron Origin (0,0)
        member inline pt.DistanceFromOrigin = sqrt (pt.X*pt.X + pt.Y*pt.Y ) 
        
        /// Returns the squared distance fron Origin (0,0)
        member inline pt.DistanceSqFromOriginSquare = pt.X*pt.X + pt.Y*pt.Y 
        
        member inline pt.WithDistFromOrigin (l:float) = 
            let d = pt.DistanceFromOrigin 
            if d < zeroLengthTol then FsExGeoException.Raise "pnt.WithDistFromOrigin  %O is too small to be scaled" pt
            pt * (l/d) 
        
        /// Returns the Diamond Angle from this point to another point.        
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve cosine or atan functions
        member inline p.DirDiamondTo(o:Pt) =
            // https://stackoverflow.com/a/14675998/969070            
            let x = o.X-p.X
            let y = o.Y-p.Y
            if abs x < 1e-16 && abs y < 1e-16 then FsExGeoException.Raise "FsEx.Geo.Pt.DirDiamondTo Failed for too short Distance between %O and %O." p o
            if y >= 0.0 then 
                if x >= 0.0 then   
                    y/(x+y) 
                else             
                    1.0 - x/(-x+y)
            else
                if x < 0.0 then   
                    2.0 - y/(-x-y) 
                else 
                    3.0 + x/(x-y) 

        /// Returns the Angle in Radians from this point to another point.
        /// 0.0 = XAxis,  going Counter clockwise till two Pi.
        member inline p.Angle2PiTo(o:Pt) =
            // https://stackoverflow.com/a/14675998/969070            
            let x = o.X-p.X
            let y = o.Y-p.Y
            let a = Math.Atan2(y, x) 
            if a < 0. then  a + Util.twoPi
            else            a
        
        /// Returns the Angle in Degrees from this point to another point.
        /// 0.0 = XAxis,  going Counter clockwise till 360.
        member inline p.Angle360To(o:Pt) =
            p.Angle2PiTo o |> toDegrees

        member inline p.AsVc         = Vc( p.X, p.Y)
        member inline p.AsVec        = Vec(p.X, p.Y, 0.0)
        member inline p.AsPnt        = Pnt(p.X, p.Y, 0.0)
        member inline p.AsVecWithZ z = Vec(p.X, p.Y, z)
        member inline p.AsPntWithZ z = Pnt(p.X, p.Y, z)

        /// Get closest point on finit line to test point. 
        member inline testPt.ClosestPointOnLine(fromPt:Pt, toPt:Pt) = 
            let dir = testPt - fromPt
            let v   = toPt   - fromPt
            let lenSq = v.LengthSq
            let lenSq =  v.LengthSq
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pt.closetPointOnLine: Line is too short for fromPt %O to  %O and  %O" fromPt toPt testPt
            let dot = Vc.dot (v,  dir) / lenSq
            if   dot < 0.0 then  fromPt 
            elif dot > 1.0 then  toPt
            else                 fromPt+dot*v
        
        /// Get closest point on finit line to test point. 
        member inline testPt.ClosestPointOnLine(fromPt:Pt, uv:UnitVc, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vc.dot (uv,  dir) 
            if   dot < 0.0 then  fromPt 
            elif dot > len then (fromPt+len*uv)  
            else                 fromPt+dot*uv 
        
        /// Squared Distance between point and finite line segment.   
        member inline testPt.DistanceSqToLine(fromPt:Pt, uv:UnitVc, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vc.dot (uv,  dir) 
            if   dot < 0.0 then testPt.DistanceSqTo  fromPt 
            elif dot > len then testPt.DistanceSqTo (fromPt+len*uv)  
            else 
                let actual = uv.RotatedCCW * dir 
                actual*actual
                
        /// Squared Distance between point and finite line segment  defined by start , end,  direction and length 
        /// The last two paramters  help speed up calculations.
        member inline testPt.DistanceSqToLine(fromPt:Pt, toPt:Pt,  uv:UnitVc, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vc.dot (uv,  dir) 
            if   dot < 0.0 then testPt.DistanceSqTo fromPt 
            elif dot > len then testPt.DistanceSqTo toPt 
            else 
                let actual = uv.RotatedCCW * dir 
                actual*actual 
                
        /// Distance between point and finite line segment  defined by start ,  direction and length.
        member inline testPt.DistanceToLine(fromPt:Pt, uv:UnitVc, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vc.dot (uv,  dir) 
            if   dot < 0.0 then testPt.DistanceSqTo  fromPt 
            elif dot > len then testPt.DistanceSqTo  (fromPt+len*uv)  
            else                abs (uv.RotatedCCW * dir) 
                
        /// Distance between point and finite line segment  defined by start and end.
        member inline testPt.DistanceToLine(fromPt:Pt, toPt:Pt) =  
            let dir = testPt - fromPt 
            let v   = toPt   - fromPt
            let lenSq =  v.LengthSq 
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pt.DistanceToLine: Line is too short for fromPt %O to  %O and  %O" fromPt toPt testPt
            let dot = Vc.dot (v,  dir) / v.LengthSq 
            if   dot < 0.0 then testPt.DistanceTo   fromPt 
            elif dot > 1.0 then testPt.DistanceTo   toPt  
            else                testPt.DistanceTo   (fromPt + v * dot) 


        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        
         
        /// Same as Pt.Origin 
        static member Zero   = Pt ( 0. , 0. )  // needed by 'Array.sum' 
        
        /// Same as Pt.Zero
        static member Origin = Pt ( 0. , 0. ) 
        
        /// Accepts any type that has a X and Y (UPPERCASE) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Staticaly Resolved Type Parmeters at compile time.
        static member inline ofXY pt  = 
            let x = ( ^T : (member X: _) pt)
            let y = ( ^T : (member Y: _) pt)
            try Pt(float x, float y) 
            with e -> FsExGeoDivByZeroException.Raise "Pt.ofXY: %A could not be converted to a FsEx.Geo.Pt:\r\n%A" pt e

        /// Accepts any type that has a x and y (lowercase) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Staticaly Resolved Type Parmeters at compile time.
        static member inline ofxy pt  = 
            let x = ( ^T : (member x: _) pt)
            let y = ( ^T : (member y: _) pt)
            try Pt(float x, float y) 
            with e -> FsExGeoDivByZeroException.Raise "Pt.ofxy: %A could not be converted to a FsEx.Geo.Pt:\r\n%A" pt e

        static member inline ofPnt      (p:Pnt)     = Pt (p.X, p.Y)    
        static member inline ofVec      (v:Vc)      = Pt (v.X, v.Y)  
        static member inline ofUnitVec  (v:UnitVc)  = Pt (v.X, v.Y)

        static member inline create    (x:float, y:float) =  Pt( x , y ) 

        //static member inline DivideByInt (pt:Pt, i:int) = if i<>0 then pt / float i else failwithf "DivideByInt 0 %O " pt  // needed by  'Array.average'

        /// Sets the X value and returns new Pt
        static member inline setX x (pt:Pt) =  Pt(x, pt.Y)
       
        /// Sets the Y value and returns new Pt
        static member inline setY y (pt:Pt) =  Pt(pt.X, y)   
       
        /// Gets the X value of  Pt
        static member inline getX (pt:Pt)  =  pt.X
       
        /// Gets the Y value of  Pt
        static member inline getY (pt:Pt) =  pt.Y
       
        static member inline add        (a:Pt) (b:Pt) = a + b
        static member inline addVc      (v:Vc) (a:Pt) = a + v

        static member inline midPt      (a:Pt) (b:Pt)         = (a+b) * 0.5
        static member inline scale      (f:float) (pt:Pt) = pt*f
        
        static member inline shiftX     (x:float) (pt:Pt) = Pt (pt.X+x, pt.Y)
        static member inline shiftY     (y:float) (pt:Pt) = Pt (pt.X,   pt.Y+y)
          
        
        /// Returns the distance between two points
        static member inline distance (a:Pt) (b:Pt) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y )
       
        /// Returns the squared distance bewteen two points.
        /// This operation is slighty faster than the distance function, and sufficient for many algorithms like finding closest points.
        static member inline distanceSq (a:Pt) (b:Pt) = let v = a-b in  v.X*v.X + v.Y*v.Y 

        static member inline distFromOrigin (pt:Pt) = pt.DistanceFromOrigin
        static member inline setDistFromOrigin f (pt:Pt) = pt.WithDistFromOrigin f
        static member inline distFromOriginSquare (pt:Pt) = pt.DistanceSqFromOriginSquare
    
        /// Returns angle between three Points in Radians. Range 0.0 to Pi  
        static member inline anglePiPts (ptPrev:Pt, ptThis:Pt, ptNext:Pt)  =   
            Vc.anglePI (ptPrev-ptThis) (ptNext-ptThis)

        /// Returns angle between three Points in Degrees. Range 0.0 to 180 
        static member inline angle180Pts (ptPrev:Pt, ptThis:Pt, ptNext:Pt)  =   
            Pt.anglePiPts (ptPrev, ptThis, ptNext) |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction from ptThis. 
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized 
        /// ptPrev * ptThis * ptNext ->   bisector Vector 
        static member inline bisector (ptPrev:Pt, ptThis:Pt, ptNext:Pt) =  
            (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized   
        
        /// Rotate the a 2D Point Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateBy (r:Rotation2D) (p:Pt) = 
            Pt(r.Cos*p.X - r.Sin*p.Y, 
               r.Sin*p.X + r.Cos*p.Y) 

        /// Rotate the 2D Point in Degrees. Counter Clockwise.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate. see Vc.rotateBy
        static member inline rotate (angDegree) (vec:Pt) = Pt.rotateBy (Rotation2D.createFromDegrees angDegree) vec 
        
        /// Rotate the 2D Point around a center 2D Point. Counter Clockwise.
        /// By a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateWithCenterBy (cen:Pt) (r:Rotation2D) (pt:Pt) = 
            let x = pt.X - cen.X  
            let y = pt.Y - cen.Y         
            Pt (r.Cos*x - r.Sin*y + cen.X, 
                r.Sin*x + r.Cos*y + cen.Y) 
         
        /// Rotate 2D Point around a center point counter clockwise. Angle given in degrees.    
        static member inline ptWitCen  (cen:Pt)  angDegree (pt:Pt)  = 
            Pt.rotateWithCenterBy cen (Rotation2D.createFromDegrees angDegree) pt
       
        /// Returns a point that is at a given distance from a point in the direction of another point.
        static member inline distPt (fromPt:Pt, dirPt:Pt, distance:float) : Pt  = 
            let v = dirPt - fromPt
            let sc = distance/v.Length
            fromPt + v*sc
       
       
        /// Returns a Point by evaluation a line between two point with a normalized patrameter.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint
        static member inline divPt(fromPt:Pt, toPt:Pt,rel:float) : Pt  = 
            let v = toPt - fromPt
            fromPt + v*rel     
             
        /// Applies a translation vector
        static member inline translate (shift:Vc) (pt:Pt ) = 
            pt + shift
       
            
        /// Snap to point if within snapDistance
        static member snapIfClose (snapDistance) (snapTo:Pt) (pt:Pt) = 
            if (snapTo-pt).Length < snapDistance then snapTo else pt
            
              
        /// Returns angle in degree at midd point
        static member angelInCorner(prevPt:Pt, thisPt:Pt, nextPt:Pt) = 
            let a = prevPt-thisPt
            let b = nextPt-thisPt
            Vc.angle180 a b

        /// 'fromPt' Pt  and UnitVc decribe an endless line. 
        /// testPt gets projected on to this line.
        /// Returns the paramter (or scaling for unitvector) on this line of the projection
        static member inline projectedParameter (fromPt:Pt, uv:UnitVc, testPt:Pt) = 
           let dir = testPt-fromPt
           Vc.dot (dir,  uv) 
        
        /// 'fromPt' Pt  and Vc decribe an endless line.
        /// testPt gets projected on to this line.
        /// Returns the paramter (or scaling for vector) on this line of the projection
        static member inline projectedParameter (fromPt:Pt, v:Vc, testPt:Pt) = 
           let dir = testPt-fromPt
           let lenSq =  v.LengthSq
           if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pt.projectedParameter:  %O is too short for fromPt %O and  %O" v fromPt testPt
           Vc.dot (v,  dir) / v.LengthSq 
        
        /// 'fromPt' Pt  and Pt decribe an endless line. 
        /// testPt gets projected on to this line.
        /// Returns the paramter (or scaling for vector) on this line of the projection
        static member inline projectedParameter (fromPt:Pt, toPt:Pt, testPt:Pt) = 
            let dir = testPt - fromPt 
            let v   = toPt   - fromPt
            let lenSq =  v.LengthSq 
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pt.projectedParameter: Line is too short for fromPt %O to  %O and  %O" fromPt toPt testPt
            Vc.dot (v,  dir) / v.LengthSq
        
        




       
       

    
    
    
    
    




            

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

