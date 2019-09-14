namespace FsEx.Geo
open System



/// Members and operators for 3D Points, Vectors and Rotations
[<AutoOpen>]
module AutoOpenPnt = 
    open Util

    type Pnt with   
    
        member inline pt.IsZero = pt.X = 0.0 && pt.Y = 0.0 && pt.Z= 0.0
        //member inline v.IsInValid =  Double.IsNaN v.X || Double.IsNaN v.Y || Double.IsNaN v.Z || Double.IsInfinity v.X || Double.IsInfinity v.Y || Double.IsInfinity v.Z
        member inline pt.WithX x = Pnt (x ,pt.Y, pt.Z) // returns new Vector with new x coordinate, y and z the same as before
        member inline pt.WithY y = Pnt (pt.X, y, pt.Z)
        member inline pt.WithZ z = Pnt (pt.X ,pt.Y, z)
    
        member inline pt.DistFromOrigin = sqrt (pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z) 
        member inline pt.DistFromOriginSquare = pt.X*pt.X + pt.Y*pt.Y + pt.Z*pt.Z
        member inline pt.DistFromOriginInXY =  sqrt (pt.X*pt.X + pt.Y*pt.Y)
        member inline pt.DistFromOriginSquareInXY = pt.X*pt.X + pt.Y*pt.Y
        member inline pt.WithDistFromOrigin (l:float) = 
            let d = pt.DistFromOrigin 
            if d < zeroLenghtTol then FsExGeoException.Raise $"pnt.WithDistFromOrigin  %O{pt} is too small to be scaled" 
            pt * (l/d) 

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        static member Zero   = Pnt ( 0. , 0. , 0.)  // needed by 'Array.sum' 
        static member Origin = Pnt ( 0. , 0. , 0.) 
    
        
        static member inline ofPt       (p:Pt)      = Pnt (p.X, p.Y, 0.0) 
        static member inline ofVec      (v:Vec)     = Pnt (v.X, v.Y, v.Z)  
        static member inline ofUnitVec  (v:UnitVec) = Pnt (v.X, v.Y, v.Z)                  
        static member inline create     (x:float, y:float, z:float) =  Pnt( x , y , z ) 

        /// Returns a 3D point from z level and 2D point.
        static member inline ofPtAt  (z:float)  (p:Pt)  = Pnt (p.X, p.Y, z) 

        /// Project point to World XY Plane.
        /// Use make2D to convert to 2D point instance
        static member inline projectToXYPlane (v:Pnt) = Pnt(v.X,v.Y, 0.0)
        
        /// Project point to World XY Plane.
        /// Use projectToXYPlane to keep to 3D point instance
        static member inline make2D (v:Pnt) = Pt(v.X,v.Y)
    
        //static member inline DivideByInt (pt:Pnt, i:int) = if i<>0 then pt / float i else failwithf "DivideByInt 0 %O " pt  // needed by  'Array.average'

        /// Sets the X value and returns new Pnt
        static member inline setX x (pt:Pnt) =  Pnt(x, pt.Y, pt.Z)
       
        /// Sets the Y value and returns new Pnt
        static member inline setY y (pt:Pnt) =  Pnt(pt.X, y, pt.Z)
       
        /// Sets the Z value and returns new Pnt
        static member inline setZ z (pt:Pnt) =  Pnt(pt.X, pt.Y, z)
       
        /// Gets the X value of  Pnt
        static member inline getX (pt:Pnt)  =  pt.X
       
        /// Gets the Y value of  Pnt
        static member inline getY (pt:Pnt) =  pt.Y
       
        /// Gets the Z value of  Pnt
        static member inline getZ (pt:Pnt) =  pt.Z

        static member inline add        (v:Vec) (a:Pnt) = a + v
        static member inline addToPtn   (a:Pnt) (v:Vec) = a + v
   
        static member inline midPt      (a:Pnt) (b:Pnt)         = (a+b) * 0.5
        static member inline scale      (f:float) (pt:Pnt) = pt*f
        
        static member inline shiftX     (x:float) (pt:Pnt) = Pnt (pt.X+x, pt.Y,   pt.Z)
        static member inline shiftY     (y:float) (pt:Pnt) = Pnt (pt.X,   pt.Y+y, pt.Z)
        static member inline shiftZ     (z:float) (pt:Pnt) = Pnt (pt.X,   pt.Y,   pt.Z+z)    
        
        /// Returns the distance between two points
        static member inline distance (a:Pnt) (b:Pnt) = let v = a-b in sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
       
        /// Returns the horizontal distance between two points(ignoring their Z Value)
        static member inline distanceXY (a:Pnt) (b:Pnt) = let x = a.X-b.X in let y=a.Y-b.Y in sqrt(x*x + y*y)
       
        /// Returns the squared distance bewteen two points.
        /// This operation is slighty faster than the distance function, and sufficient for many algorithms like finding closest points.
        static member inline distanceSq (a:Pnt) (b:Pnt) = let v = a-b in  v.X*v.X + v.Y*v.Y + v.Z*v.Z

        static member inline distFromOrigin (pt:Pnt) = pt.DistFromOrigin
        static member inline setDistFromOrigin f (pt:Pnt) = pt.WithDistFromOrigin f
        static member inline distFromOriginSquare (pt:Pnt) = pt.DistFromOriginSquare
    
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
        static member inline rotateOnX (angDegree) (pt:Pnt) = (Rotate.createFromDegrees angDegree).RotateOnX pt
    
        /// Rotate the Point in Degrees around Y axis, from Z to X Axis, Counter Clockwise looking from back.
        static member inline rotateOnY (angDegree) (pt:Pnt) = (Rotate.createFromDegrees angDegree).RotateOnY pt 
    
        /// Rotate the Point in Degrees around Z axis, from X to Y Axis, Counter Clockwise looking from top.
        static member inline rotateOnZ (angDegree) (pt:Pnt) = (Rotate.createFromDegrees angDegree).RotateOnZ pt 
    
        /// Rotate the Point in Degrees around center Point and a X aligned axis, from Y to Z Axis, Counter Clockwise looking from right.
        static member inline rotateOnXwithCenter (cen:Pnt) (angDegree) (pt:Pnt) = (Rotate.createFromDegrees angDegree).RotateOnXwithCenter(cen,pt) 

        /// Rotate the Point in Degrees around center Point and a Y aligned axis, from Z to X Axis, Counter Clockwise looking from back.
        static member inline rotateOnYwithCenter (cen:Pnt) (angDegree) (pt:Pnt) = (Rotate.createFromDegrees angDegree).RotateOnYwithCenter(cen,pt) 
    
        /// Rotate the Point in Degrees around center Point and a Z aligned axis, from X to Y Axis, Counter Clockwise looking from top.
        static member inline rotateOnZwithCenter (cen:Pnt) (angDegree) (pt:Pnt) = (Rotate.createFromDegrees angDegree).RotateOnZwithCenter(cen,pt) 
             
       
        /// returns a point that is at a given distance from a point in the direction of another point.
        static member inline distPt (fromPt:Pnt) ( dirPt:Pnt) ( distance:float) : Pnt  = 
            let v = dirPt - fromPt
            let sc = distance/v.Length
            fromPt + v*sc
       
       
        /// returns a Point by evaluation a line between two point with a normalized patrameter.
        /// e.g. rel=0.5 will return the middle point, rel=1.0 the endPoint
        static member inline divPt(fromPt:Pnt)( toPt:Pnt)(rel:float) : Pnt  = 
            let v = toPt - fromPt
            fromPt + v*rel
       
        /// returns a point that is at a given Z level,
        /// going from a point in the direction of another point.
        static member inline extendToZLevel (fromPt:Pnt)( toPt:Pnt) (z:float) = 
            let v = toPt - fromPt
            if fromPt.Z < toPt.Z && z < fromPt.Z  then FsExGeoException.Raise $"Pnt.extendToZLevel cannot be reached for fromPt:%O{fromPt} toPt:%O{toPt} z:%g{z}" 
            if fromPt.Z > toPt.Z && z > fromPt.Z  then FsExGeoException.Raise $"Pnt.extendToZLevel cannot be reached for fromPt:%O{fromPt} toPt:%O{toPt} z:%g{z}" 
            let dot = abs ( v * Vec.ZAxis)
            if dot < 0.0001 then  FsExGeoException.Raise $"Pnt.extendToZLevel cannot be reached for fromPt:%O{fromPt} toPt:%O{toPt} because they are boyh at the same level. target z:%g{z} "
            let diffZ = abs (fromPt.Z - z)
            let fac = diffZ / dot
            fromPt + v * fac       
       
             
        /// Applies a translation vector
        static member inline translate (shift:Vec) (pt:Pnt ) = 
            pt + shift      
        
       
        /// Snap to point if within snapDistance
        static member snapIfClose (snapDistance) (snapTo:Pnt) (pt:Pnt) = 
            if (snapTo-pt).Length < snapDistance then snapTo else pt
       
        /// Every line has a normal vector in XY Plane.
        /// If line is vertical then XAxis is returned
        /// Rotated counter clockwise in top view.
        /// result is unitized
        /// see also : Vec.perpendicularVecInXY
        static member normalOfTwoPointsInXY(fromPt:Pnt, toPt:Pnt) = 
            let x = toPt.Y - fromPt.Y
            let y = fromPt.X - toPt.X  // this is the same as: Vec.cross v Vec.ZAxis
            let len = sqrt(x*x + y*y)
            if len < zeroLenghtTol then Vec.XAxis
            else Vec(x/len, y/len, 0.0)
       
        /// Offsets two points by two given distances.
        /// The fist distance (distHor) is applied in in XY Plane
        /// The second distance (distNormal) is applied perpendicular to the line (made by the two points) and perpendicular to the horizontal offset direction.
        /// this is in Wolrd.Z direction if both points are at the same Z level.
        /// If points are closer than than 1e-6 units the World.XAxis is used as first direction and World.ZAxis as second direction.
        static member offsetTwoPt(    fromPt:Pnt,
                            toPt:Pnt,
                            distHor:float,
                            distNormal:float) : Pnt*Pnt= 
            let v = toPt - fromPt
            let normHor = 
                Vec.cross(v, Vec.ZAxis)
                |> Vec.unitizeWithAlternative Vec.XAxis
       
            let normFree = 
                Vec.cross(v, normHor)
                |> Vec.unitizeWithAlternative Vec.ZAxis
       
            let shift = distHor * normHor + distNormal * normFree
            fromPt +  shift, toPt + shift
             
              
        /// returns angle in degree at midd point
        static member angelInCorner(prevPt:Pnt, thisPt:Pnt, nextPt:Pnt) = 
            let a = prevPt-thisPt
            let b = nextPt-thisPt
            Vec.angle180 a b
       
       

    
    
    
    
    



