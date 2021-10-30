namespace FsEx.Geo

open FsEx.Geo.Util

/// When FsEx.Geo is opened this module will be auto-opened. 
/// It only contains extension members for type PPlane
[<AutoOpen>]
module AutoOpenPPlane =     
    

    // pre allocate , used often 
    let private worldXY = PPlane(Pnt.Origin, UnitVec.Xaxis, UnitVec.Yaxis, UnitVec.Zaxis)
    
    type PPlane with  

        /// Evaluate at 3D parameter
        member p.EvaluateAt (px:float, py:float, pz:float) = p.Origin + p.Xaxis*px + p.Yaxis*py + p.Zaxis*pz
        
        /// Evaluate at 2D parameter ( Z parameter = 0.0)
        member p.EvaluateAtXY (px:float, py:float) = p.Origin + p.Xaxis*px + p.Yaxis*py 

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Checks if two 3D Parametrized Planes are equal within tolerance.
        static member equals tol (a:PPlane) (b:PPlane) =
            let tt = tol*tol
            Pnt.distanceSq a.Origin b.Origin < tt &&
            UnitVec.differenceSq a.Xaxis b.Xaxis < tt &&
            UnitVec.differenceSq a.Yaxis b.Yaxis < tt &&
            UnitVec.differenceSq a.Zaxis b.Zaxis < tt

        /// Returns the World Coordinate System Plane at World Origin
        /// X-axis = World X-axis 
        /// Y-axis = World Y-axis 
        /// Z-axis = World Z-axis 
        /// same as PPlane.WorldTop
        static member WorldXY = worldXY

        /// Returns the World Coordinate System Plane at World Origin
        /// X-axis = World X-axis 
        /// Y-axis = World Y-axis 
        /// Z-axis = World Z-axis 
        /// same as PPlane.WorldXY
        static member WorldTop = worldXY  

        /// Returns the Coordinate System Plane of a Front view
        /// X-axis = World X-axis 
        /// Y-axis = World Z-axis 
        /// Z-axis = minus World Y-axis 
        static member WorldFront = 
            PPlane(Pnt.Origin, UnitVec.Xaxis, UnitVec.Zaxis, -UnitVec.Yaxis)

        /// Returns the Coordinate System Plane of a Right view
        /// X-axis = World Y-axis 
        /// Y-axis = World Z-axis 
        /// Z-axis = World X-axis 
        static member WorldRight = 
            PPlane(Pnt.Origin, UnitVec.Yaxis, UnitVec.Zaxis, UnitVec.Xaxis)

        /// Returns the Coordinate System Plane of a Left view
        /// X-axis = minus World Y-axis 
        /// Y-axis = World Z-axis 
        /// Z-axis = minus World X-axis 
        static member WorldLeft = 
            PPlane(Pnt.Origin, -UnitVec.Yaxis, UnitVec.Zaxis, -UnitVec.Xaxis)

        /// Returns the Coordinate System Plane of a Back view
        /// X-axis = minus World X-axis 
        /// Y-axis = World Z-axis 
        /// Z-axis = World Y-axis 
        static member WorldBack = 
            PPlane(Pnt.Origin, -UnitVec.Xaxis, UnitVec.Zaxis, UnitVec.Yaxis)  
            
        /// Returns the Coordinate System Plane of a Bottom view
        /// X-axis = World X-axis 
        /// Y-axis = minus World Y-axis 
        /// Z-axis = minus World Z-axis 
        static member WorldBottom = 
            PPlane(Pnt.Origin, UnitVec.Xaxis, -UnitVec.Yaxis, -UnitVec.Zaxis)   

        /// Creates a Parametrized Plane from a Point and Vector representing the X Axis.
        /// The resulting PPlane will have the X-Axis in direction of X vector. 
        /// The X and Y vectors will define the plane and the side that Z will be on.
        /// The given Y vector does not need to be perpendicular to the X vector , just not parallel.
        static member createOriginXaxisYaxis (origin:Pnt) (x:Vec) (y:Vec) = 
            let z = Vec.cross (x , y)
            if z.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane.createOriginXaxisYaxis from origin:%O, x:%O and y:%O" origin x y
            let y = Vec.cross (z , x)
            PPlane(origin, x.Unitized, y.Unitized, z.Unitized)

        /// Creates a Parametrized Plane from a Point and Vector representing the Y Axis.
        /// The X Axis will be found by taking the cross product of the Y Axis and the Wold Z Axis.
        /// This will fail if the given Y Axis is parallel to the World Z Axis.
        static member createOriginYaxis (origin:Pnt) (y:Vec) = 
            let x = Vec.cross (y , Vec.Zaxis)
            if x.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane.createOriginYaxis from %O and %O" origin  y
            let z = Vec.cross (x , y)
            PPlane(origin, x.Unitized, y.Unitized, z.Unitized)

        /// Creates a Parametrized Plane from a Point and Vector representing the X Axis.
        /// The Y Axis will be found by taking the cross product of the Wold Z Axis and the X Axis.
        /// This will fail if the given X Axis is parallel to the World Z Axis.
        static member createOriginXaxis (origin:Pnt) (x:Vec) = 
            let y = Vec.cross (Vec.Zaxis , x)
            if y.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane.createOriginXaxis from origin:%O and x:%O " origin x 
            let z = Vec.cross (x , y)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first point , X-axis to second Point, checks for collinear points.    
        static member createThreePoints (origin:Pnt) (b:Pnt) (c:Pnt) = 
            let x = b-origin
            let yt = c-origin
            let z = Vec.cross (x , yt)
            if z.IsTiny 1e-6 then FsExGeoDivByZeroException.Raise "Cannot construct FsEx.Geo.PPlane.createThreePoints from %O, %O and %O, are they colinear?" origin b c
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first point.
        /// The resulting PPlane will have the X-Axis in direction of the second point. 
        /// The Y Axis will be found by taking the cross product of the Wold Z Axis and the x Axis.
        /// This will fail if the given X Axis is parallel to the World Z Axis.
        static member createTwoPts (a:Pnt) (b:Pnt) = 
            PPlane.createOriginXaxis a (b-a)        

        /// WorldXY rotated 180 degrees round Z Axis.
        static member WorldMinusXMinusY= 
            PPlane(Pnt.Origin, -UnitVec.Xaxis, -UnitVec.Yaxis, UnitVec.Zaxis)

        /// WorldXY rotated 90 degrees round Z Axis counter clockwise from top.
        static member WorldYMinusX= 
            PPlane(Pnt.Origin, UnitVec.Yaxis, -UnitVec.Xaxis,UnitVec.Zaxis)

        /// WorldXY rotated 270 degrees round Z Axis counter clockwise from top.
        static member WorldMinusYX= 
            PPlane(Pnt.Origin, -UnitVec.Yaxis, UnitVec.Xaxis,UnitVec.Zaxis)

        /// WorldXY rotated 180 degrees round X Axis, Z points down now.
        static member WorldXMinusY= 
            PPlane(Pnt.Origin, UnitVec.Xaxis, -UnitVec.Yaxis, -UnitVec.Zaxis)

        /// Return a new plane with given Origin
        static member setOrig (pt:Pnt) (pl:PPlane) =   
            PPlane(pt, pl.Xaxis, pl.Yaxis, pl.Zaxis)
            
        /// Return a new plane with given Origin X value changed.
        static member  setOrigX (x:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.setX x, pl.Xaxis, pl.Yaxis, pl.Zaxis)    
        
        /// Return a new plane with given Origin Y value changed.
        static member  setOrigY (y:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.setY y, pl.Xaxis, pl.Yaxis, pl.Zaxis)        
        
        /// Return a new plane with given Origin Z value changed.
        static member  setOrigZ (z:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.setZ z, pl.Xaxis, pl.Yaxis, pl.Zaxis)               

        /// Return a new plane with Origin translated by Vec.
        static member  translateBy (v:Vec) (pl:PPlane) =   
            PPlane(pl.Origin + v , pl.Xaxis, pl.Yaxis, pl.Zaxis)   
        
        /// Return a new plane with Origin translated in World X direction.
        static member  translateByWorldX (x:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.moveX x, pl.Xaxis, pl.Yaxis, pl.Zaxis)       
        
        /// Return a new plane with Origin translated in World Y direction.
        static member  translateByWorldY (y:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.moveY y, pl.Xaxis, pl.Yaxis, pl.Zaxis)       
        
        /// Return a new plane with Origin translated in World Z direction.
        static member  translateByWorldZ (z:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.moveZ z, pl.Xaxis, pl.Yaxis, pl.Zaxis)  
        
        /// Rotate about Z axis by angle in degree.  
        /// Counter clockwise in top view (for WorldXY Plane).
        static member  rotateZ (angDegree:float) (pl:PPlane) =   
            let q = Quaternion.createFromDegree (pl.Zaxis, angDegree)
            PPlane(pl.Origin , pl.Xaxis*q, pl.Yaxis*q, pl.Zaxis) 

        /// Move Plane along the local X-axis by the given distance
        static member translateX (d:float) (pl:PPlane) = 
            PPlane(pl.Origin + pl.Xaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 

        /// Move Plane along the local Y-axis by the given distance
        static member translateY (d:float) (pl:PPlane) = 
            PPlane(pl.Origin + pl.Yaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 

        /// Move Plane along the local Z-axis by the given distance
        /// Same as PPlane.offset
        static member translateZ (d:float) (pl:PPlane) = 
            PPlane(pl.Origin + pl.Zaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 
        
        /// Move Plane along the local Z-axis by the given distance
        /// Same as PPlane.translateZ
        static member offset (d:float) (pl:PPlane) = 
            PPlane(pl.Origin + pl.Zaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 

        /// Rotate the Plane 180 degrees on its Y axis.
        /// Called flip because Z axis points in the opposite direction.
        static member flipOnY (pl:PPlane) = 
            PPlane(pl.Origin , -pl.Xaxis, pl.Yaxis, -pl.Zaxis) 
        
        /// Rotate the Plane 180 degrees on its X axis.
        /// Called flip because Z axis points in the opposite direction.
        static member flipOnX (pl:PPlane) = 
            PPlane(pl.Origin , pl.Xaxis, -pl.Yaxis, -pl.Zaxis) 
        
        /// Rotate the Plane 180 degrees on its Z axis.
        static member rotateOnZ180 (pl:PPlane) = 
            PPlane(pl.Origin , -pl.Xaxis, -pl.Yaxis, pl.Zaxis) 

        /// Transforms the plane by the given OrthoMatrix.
        /// The returned PPlane has orthogonal unit vectors
        static member transform (m:OrthoMatrix) (pl:PPlane) = 
            let o = Pnt.transformOrtho m pl.Origin
            let x = UnitVec.rotateOrtho m pl.Xaxis
            let y = UnitVec.rotateOrtho m pl.Yaxis
            let z = UnitVec.rotateOrtho m pl.Zaxis             
            PPlane.createUnchecked (o, x, y, z)
                
        /// Rotate Plane 180 Degrees around Z-axis if the Y-axis orientation does not match World Y (pl.Yax.Y < 0.0)
        /// To ensure that Y is always positive. For example for showing Text 
        static member rotateZ180IfYNegative (pl:PPlane) = 
            if pl.Yaxis.Y < 0.0 then PPlane.rotateOnZ180 pl else pl


