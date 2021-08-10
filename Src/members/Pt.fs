namespace FsEx.Geo

open System



/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type Pt
[<AutoOpen>]
module AutoOpenPt = 
    open Util
    
    type Pt with      
        
        /// Returns a boolean indicating wether X and Y are exactly 0.0.
        member inline pt.IsOrigin = pt.X = 0.0 && pt.Y = 0.0 
        
        /// Returns a boolean indicating wether the absolute value of X and Y is each less than the given tolerance.
        member inline pt.IsAlmostOrigin tol = abs pt.X < tol && abs pt.Y < tol 
        
        /// Returns new 2D point with new X coordinate, Y stays the same.
        member inline pt.WithX x = Pt (x ,pt.Y) 

        /// Returns new 2D point with new Y coordinate, X stays the same.
        member inline pt.WithY y = Pt (pt.X, y)

        /// Returns new 3D point with Z coordinate, X and Y stay the same.
        /// If you want Z to be 0.0 you can use pt.AsPnt too.
        member inline pt.WithZ z = Pnt (pt.X ,pt.Y, z)
        
        /// Returns the distance between two points
        member inline p.DistanceTo (b:Pt) = let v = p-b in sqrt(v.X*v.X + v.Y*v.Y )
        
        /// Returns the squared distance between two points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        member inline p.DistanceToSquare (b:Pt) = let v = p-b in  v.X*v.X + v.Y*v.Y 
        
        /// Returns the distance from Origin (0,0)
        member inline pt.DistanceFromOrigin = sqrt (pt.X*pt.X + pt.Y*pt.Y ) 
        
        /// Returns the squared distance from Origin (0,0)
        member inline pt.DistanceFromOriginSquare = pt.X*pt.X + pt.Y*pt.Y 
        
        /// Returns new 2D point with given Distance from Origin by scaling it up or down.
        member inline pt.WithDistanceFromOrigin (l:float) = 
            let d = pt.DistanceFromOrigin 
            if d < zeroLengthTol then FsExGeoException.Raise "pnt.WithDistFromOrigin  %O is too small to be scaled" pt
            pt * (l/d) 
        
        /// Returns the Diamond Angle from this point to another point.        
        /// Calculates the proportion of X to Y component. 
        /// It is always positive and in the range of 0.0 to 4.0 ( for 360 Degrees) 
        /// 0.0 = XAxis,  going Counter clockwise.
        /// It is the fastest angle calculation since it does not involve Cosine or ArcTangent functions
        member inline p.DirectionDiamondTo(o:Pt) =
            // https://stackoverflow.com/a/14675998/969070            
            let x = o.X-p.X
            let y = o.Y-p.Y
            //#if DEBUG 
            if abs(x) < zeroLengthTol && abs(y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Pt.DirectionDiamondTo failed for too short Distance between %O and %O." p o
            //#endif            
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
            //#if DEBUG 
            if abs(x) < zeroLengthTol && abs(y) < zeroLengthTol then // TODO : with this test all  operations are 2.5 times slower 
                FsExGeoDivByZeroException.Raise "FsEx.Geo.Pt.Angle2PiTo failed for too short Distance between %O and %O." p o
            //#endif
            let a = Math.Atan2(y, x) 
            if a < 0. then  a + Util.twoPi
            else            a
        
        /// Returns the Angle in Degrees from this point to another point.
        /// 0.0 = XAxis,  going Counter clockwise till 360.
        member inline p.Angle360To(o:Pt) =
            p.Angle2PiTo o |> toDegrees

        /// Returns the 2D point as 2D vector.
        member inline p.AsVc         = Vc( p.X, p.Y)

        /// Returns the 2D point as 3D vector. Using 0.0 for Z
        member inline p.AsVec        = Vec(p.X, p.Y, 0.0)

        /// Returns the 2D point as 3D point. Using 0.0 for Z
        member inline p.AsPnt        = Pnt(p.X, p.Y, 0.0)        

        /// Get closest point on finit line to test point. 
        member inline testPt.ClosestPointOnLine(fromPt:Pt, toPt:Pt) = 
            let dir = testPt - fromPt
            let v   = toPt   - fromPt
            let lenSq = v.LengthSq
            let lenSq =  v.LengthSq
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pt.closetPointOnLine: Line is too short for fromPt %O to  %O and  %O" fromPt toPt testPt
            let dot = Vc.dot (v,  dir) / lenSq
            if   dot <= 0.0 then  fromPt 
            elif dot >= 1.0 then  toPt
            else                 fromPt+dot*v
        
        /// Get closest point on finit line to test point. 
        member inline testPt.ClosestPointOnLine(fromPt:Pt, uv:UnitVc, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vc.dot (uv,  dir) 
            if   dot <= 0.0 then  fromPt 
            elif dot >= len then (fromPt+len*uv)  
            else                 fromPt+dot*uv 
        
        /// Squared Distance between point and finite line segment.   
        member inline testPt.DistanceToLineSquare(fromPt:Pt, uv:UnitVc, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vc.dot (uv,  dir) 
            if   dot <= 0.0 then testPt.DistanceToSquare  fromPt 
            elif dot >= len then testPt.DistanceToSquare (fromPt+len*uv)  
            else 
                let actual = uv.Rotate90CCW * dir 
                actual*actual
                
        /// Squared Distance between point and finite line segment  defined by start , end,  direction and length 
        /// The last two parameters  help speed up calculations.
        member inline testPt.DistanceToLineSquare(fromPt:Pt, toPt:Pt,  uv:UnitVc, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vc.dot (uv,  dir) 
            if   dot <= 0.0 then testPt.DistanceToSquare fromPt 
            elif dot >= len then testPt.DistanceToSquare toPt 
            else 
                let actual = uv.Rotate90CCW * dir 
                actual*actual 
                
        /// Distance between point and finite line segment  defined by start ,  direction and length.
        member inline testPt.DistanceToLine(fromPt:Pt, uv:UnitVc, len:float) = 
            let dir = testPt-fromPt 
            let dot = Vc.dot (uv,  dir) 
            if   dot <= 0.0 then testPt.DistanceToSquare  fromPt 
            elif dot >= len then testPt.DistanceToSquare  (fromPt+len*uv)  
            else                abs (uv.Rotate90CCW * dir) 
                
        /// Distance between point and finite line segment  defined by start and end.
        member inline testPt.DistanceToLine(fromPt:Pt, toPt:Pt) =  
            let dir = testPt - fromPt 
            let v   = toPt   - fromPt
            let lenSq =  v.LengthSq 
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pt.DistanceToLine: Line is too short for fromPt %O to  %O and  %O" fromPt toPt testPt
            let dot = Vc.dot (v,  dir) / v.LengthSq 
            if   dot <= 0.0 then testPt.DistanceTo   fromPt 
            elif dot >= 1.0 then testPt.DistanceTo   toPt  
            else                testPt.DistanceTo   (fromPt + v * dot) 


        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        
        
        /// Same as Pt.Origin 
        static member Zero   = Pt ( 0. , 0. )  // needed by 'Array.sum' 
        
        /// Same as Pt.Zero
        static member Origin = Pt ( 0. , 0. ) 

         /// Divides the 3D point by an integer.
        /// (This member is needed by Array.average and similar functions)
        static member inline DivideByInt (pt:Pt, i:int) = 
            if i<>0 then  let d = float i in  Pt(pt.X/d, pt.Y/d)
            else FsExGeoDivByZeroException.Raise "FsEx.Geo.Pt.DivideByInt 0 %O " pt  // needed by  'Array.average'
        
        /// Accepts any type that has a X and Y (UPPERCASE) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofXY pt  = 
            let x = ( ^T : (member X: _) pt)
            let y = ( ^T : (member Y: _) pt)
            try Pt(float x, float y) 
            with e -> FsExGeoDivByZeroException.Raise "Pt.ofXY: %A could not be converted to a FsEx.Geo.Pt:\r\n%A" pt e

        /// Accepts any type that has a x and y (lowercase) member that can be converted to a float. 
        /// Internally this is not using reflection at runtime but F# Statically Resolved Type Parameters at compile time.
        static member inline ofxy pt  = 
            let x = ( ^T : (member x: _) pt)
            let y = ( ^T : (member y: _) pt)
            try Pt(float x, float y) 
            with e -> FsExGeoDivByZeroException.Raise "Pt.ofxy: %A could not be converted to a FsEx.Geo.Pt:\r\n%A" pt e

        /// Create 2D point from 3D point. Ignoring Z component
        static member inline ofPnt      (p:Pnt)     = Pt (p.X, p.Y)    
        
        /// Create 2D point from 2D vector. 
        static member inline ofVc       (v:Vc)      = Pt (v.X, v.Y)  

        /// Create 2D point from 2D unit vector. 
        static member inline ofUnitVc  (v:UnitVc)  = Pt (v.X, v.Y)

        /// Create 3D point from X and Y  components.
        static member inline create  (x:float, y:float) =  Pt( x , y ) 

        /// Sets the X value and return new 2D point.
        static member inline setX x (pt:Pt) =  Pt(x, pt.Y)
        
        /// Sets the Y value and return new 2D point.
        static member inline setY y (pt:Pt) =  Pt(pt.X, y)   
        
        /// Gets the X value of 2D point.
        static member inline getX (pt:Pt)  =  pt.X
        
        /// Gets the Y value of 2D point.
        static member inline getY (pt:Pt) =  pt.Y

        /// Adds two 2D points. Returns a new 2D point.
        static member inline add    (a:Pt) (b:Pt) = a + b
        
        /// Add a 2D point to a 2D vector. Returns a new 2D point.
        static member inline addVc  (v:Vc) (a:Pt) = a + v

        /// Returns the midpoint of two 2D points.
        static member inline midPt  (a:Pt) (b:Pt)         = (a+b) * 0.5

        /// Scales a 2D point by a factor. Returns a new 2D point.
        static member inline scale  (f:float) (pt:Pt) = pt*f
        
        /// Add a float to X component of a 2D point. Returns a new 2D point.
        static member inline shiftX (x:float) (pt:Pt) = Pt (pt.X+x, pt.Y)

        /// Add a float to Y component of a 2D point. Returns a new 2D point.
        static member inline shiftY (y:float) (pt:Pt) = Pt (pt.X,   pt.Y+y)    
        
        /// Returns the distance between two 2D points.
        static member inline distance (a:Pt) (b:Pt) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y )
        
        /// Returns the squared distance between two 2D points.
        /// This operation is slightly faster than the distance function, and sufficient for many algorithms like finding closest points.
        static member inline distanceSq (a:Pt) (b:Pt) = let v = a-b in  v.X*v.X + v.Y*v.Y 
        
        /// Returns the distance from World Origin. 
        static member inline distanceFromOrigin (pt:Pt) = pt.DistanceFromOrigin
        
        /// Returns the square distance from World Origin. 
        static member inline distanceFromOriginSquare (pt:Pt) = pt.DistanceFromOriginSquare

        /// Returns a new 2D point at a given distance from World Origin by scaling the input.
        static member inline setDistanceFromOrigin f (pt:Pt) = pt.WithDistanceFromOrigin f
    
        /// Returns angle between three 2D Points in Radians. Range 0.0 to Pi  
        static member inline anglePiPts (ptPrev:Pt, ptThis:Pt, ptNext:Pt)  =   
            Vc.anglePi (ptPrev-ptThis) (ptNext-ptThis)

        /// Returns angle between three 2D Points in Degrees. Range 0.0 to 180 
        static member inline angle180Pts (ptPrev:Pt, ptThis:Pt, ptNext:Pt)  =   
            Pt.anglePiPts (ptPrev, ptThis, ptNext) |> toDegrees

        /// Returns a (not unitized) bisector vector in the middle direction from ptThis. 
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized 
        /// ptPrev * ptThis * ptNext ->   bisector vector 
        static member inline bisector (ptPrev:Pt, ptThis:Pt, ptNext:Pt) =  
            (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized   
        
        /// Rotate the a 2D point Counter Clockwise by a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateBy (r:Rotation2D) (p:Pt) = 
            Pt( r.Cos*p.X - r.Sin*p.Y, 
                r.Sin*p.X + r.Cos*p.Y) 

        /// Rotate the 2D point in Degrees. Counter Clockwise.
        /// For better Performance precompute the Rotate2D struct and use its member to rotate. see Vc.rotateBy
        static member inline rotate (angDegree) (vec:Pt) = 
            Pt.rotateBy (Rotation2D.createFromDegrees angDegree) vec 
        
        /// Rotate the 2D point around a center 2D Point. Counter Clockwise.
        /// By a 2D Rotation (that has cos and sin precomputed)
        static member inline rotateWithCenterBy (cen:Pt) (r:Rotation2D) (pt:Pt) = 
            let x = pt.X - cen.X  
            let y = pt.Y - cen.Y         
            Pt (r.Cos*x - r.Sin*y + cen.X, 
                r.Sin*x + r.Cos*y + cen.Y) 
        
        /// Rotate 2D point around a center point counter clockwise. Angle given in Degrees.    
        static member inline rotateWithCenter (cen:Pt)  angDegree (pt:Pt)  =             
            Pt.rotateWithCenterBy cen (Rotation2D.createFromDegrees angDegree) pt
        
        /// Returns a point that is at a given distance from a point in the direction of another point.
        static member inline distPt (fromPt:Pt, dirPt:Pt, distance:float) : Pt  = 
            let v = dirPt - fromPt
            let sc = distance/v.Length
            fromPt + v*sc        
        
        /// Returns a point by evaluation a line between two point with a normalized parameter.
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
            
        
        /// Returns angle in Degrees at mid point (thisPt).
        static member angleInCorner(prevPt:Pt, thisPt:Pt, nextPt:Pt) = 
            let a = prevPt-thisPt
            let b = nextPt-thisPt
            Vc.angle180 a b

        /// 'fromPt' Pt  and UnitVc describe an endless line. 
        /// testPt gets projected on to this line.
        /// Returns the parameter (or scaling for unit vector) on this line of the projection
        static member inline projectedParameter (fromPt:Pt, uv:UnitVc, testPt:Pt) = 
            let dir = testPt-fromPt
            Vc.dot (dir,  uv) 
        
        /// 'fromPt' Pt  and Vc describe an endless line.
        /// testPt gets projected on to this line.
        /// Returns the parameter (or scaling for vector) on this line of the projection
        static member inline projectedParameter (fromPt:Pt, v:Vc, testPt:Pt) = 
            let dir = testPt-fromPt
            let lenSq =  v.LengthSq
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pt.projectedParameter:  %O is too short for fromPt %O and  %O" v fromPt testPt
            Vc.dot (v,  dir) / v.LengthSq 
        
        /// 'fromPt' Pt  and Pt describe an endless line. 
        /// testPt gets projected on to this line.
        /// Returns the parameter (or scaling for vector) on this line of the projection
        static member inline projectedParameter (fromPt:Pt, toPt:Pt, testPt:Pt) = 
            let dir = testPt - fromPt 
            let v   = toPt   - fromPt
            let lenSq =  v.LengthSq 
            if lenSq < 1e-6 then FsExGeoDivByZeroException.Raise "Pt.projectedParameter: Line is too short for fromPt %O to  %O and  %O" fromPt toPt testPt
            Vc.dot (v,  dir) / v.LengthSq
        
        

