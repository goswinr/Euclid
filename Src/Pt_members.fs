namespace FsEx.Geo

open System



/// Members and operators for 2D Points, Vectors and Rotations
[<AutoOpen>]
module AutoOpenPt = 
    open Util
    
    type Pt with  
    
        member inline pt.IsZero = pt.X = 0.0 && pt.Y = 0.0 
        //member inline v.IsInValid =  Double.IsNaN v.X || Double.IsNaN v.Y || Double.IsNaN v.Z || Double.IsInfinity v.X || Double.IsInfinity v.Y || Double.IsInfinity v.Z
        
        member inline pt.WithX x = Pt (x ,pt.Y) // returns new Vector with new x coordinate, y and z the same as before
        member inline pt.WithY y = Pt (pt.X, y)
        member inline pt.WithZ z = Pnt (pt.X ,pt.Y, z)
    
        member inline pt.DistFromOrigin = sqrt (pt.X*pt.X + pt.Y*pt.Y ) 
        member inline pt.DistFromOriginSquare = pt.X*pt.X + pt.Y*pt.Y 
        
        member inline pt.WithDistFromOrigin (l:float) = 
            let d = pt.DistFromOrigin 
            if d < zeroLenghtTol then FsExGeoException.Raise $"pnt.WithDistFromOrigin  %O{pt} is too small to be scaled" 
            pt * (l/d) 
        
        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------


        static member Zero   = Pt ( 0. , 0. )  // needed by 'Array.sum' 
        static member Origin = Pt ( 0. , 0. ) 
    
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
       
        static member inline add        (v:Vc) (a:Pt) = a + v
        static member inline addToPtn   (a:Pt) (v:Vc) = a + v

        static member inline midPt      (a:Pt) (b:Pt)         = (a+b) * 0.5
        static member inline scale      (f:float) (pt:Pt) = pt*f
        
        static member inline shiftX     (x:float) (pt:Pt) = Pt (pt.X+x, pt.Y)
        static member inline shiftY     (y:float) (pt:Pt) = Pt (pt.X,   pt.Y+y)
          
        
        /// Returns the distance between two points
        static member inline distance (a:Pt) (b:Pt) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y )
       
        /// Returns the squared distance bewteen two points.
        /// This operation is slighty faster than the distance function, and sufficient for many algorithms like finding closest points.
        static member inline distanceSq (a:Pt) (b:Pt) = let v = a-b in  v.X*v.X + v.Y*v.Y 

        static member inline distFromOrigin (pt:Pt) = pt.DistFromOrigin
        static member inline setDistFromOrigin f (pt:Pt) = pt.WithDistFromOrigin f
        static member inline distFromOriginSquare (pt:Pt) = pt.DistFromOriginSquare
    
        /// Returns angle between three Points in Radians 
        static member inline angleFrom3Pts (ptPrev:Pt, ptThis:Pt, ptNext:Pt)  =   
            Vc.anglePi (ptPrev-ptThis) (ptNext-ptThis)

        /// Returns a (not unitized) bisector vector in the middle direction from ptThis. 
        /// Code : (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized 
        /// ptPrev * ptThis * ptNext ->   bisector Vector 
        static member inline bisector (ptPrev:Pt, ptThis:Pt, ptNext:Pt) =  
            (ptPrev-ptThis).Unitized  + (ptNext-ptThis).Unitized   
        
        /// Rotate the 2D Point Counter Clockwise.
        static member inline rotate (angDegree) (pt:Pt) = (Rotate.createFromDegrees angDegree).Rotate pt
    
        /// Rotate the 2D Point around a center 2D Point. Counter Clockwise.
        static member inline rotateWithCenter (cen:Pt) (angDegree) (pt:Pt) = (Rotate.createFromDegrees angDegree).RotateWithCenter(cen,pt)              
       
        /// returns a point that is at a given distance from a point in the direction of another point.
        static member inline distPt (fromPt:Pt) ( dirPt:Pt) ( distance:float) : Pt  = 
            let v = dirPt - fromPt
            let sc = distance/v.Length
            fromPt + v*sc
       
       
        /// returns a Point by evaluation a line between two point with a normalized patrameter.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint
        static member inline divPt(fromPt:Pt)( toPt:Pt)(rel:float) : Pt  = 
            let v = toPt - fromPt
            fromPt + v*rel     
             
        /// Applies a translation vector
        static member inline translate (shift:Vc) (pt:Pt ) = 
            pt + shift
       
            
        /// Snap to point if within snapDistance
        static member snapIfClose (snapDistance) (snapTo:Pt) (pt:Pt) = 
            if (snapTo-pt).Length < snapDistance then snapTo else pt
            
              
        /// returns angle in degree at midd point
        static member angelInCorner(prevPt:Pt, thisPt:Pt, nextPt:Pt) = 
            let a = prevPt-thisPt
            let b = nextPt-thisPt
            Vc.angle180 a b
       
       

    
    
    
    
    




            

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        

