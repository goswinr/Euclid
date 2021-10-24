namespace FsEx.Geo

open FsEx.Geo.Util

/// When FsEx.Geo is opened this module will be auto-opened.
/// It only contains extension members for type PPlane
[<AutoOpen>]
module AutoOpenPPlane =     
    
    //TODO finish docstrings

    // pre allocate , used often 
    let private worldXY = PPlane(Pnt.Origin,UnitVec.XAxis,UnitVec.YAxis,UnitVec.ZAxis)
    
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
        static member WorldTop = worldXY

        /// The resulting PPlane wil have the X-Axis in direction of X. 
        /// x and y will define the plane and the side that Z will be on.
        /// The given y does not need to be perpendicular to x, just not parallel.
        static member createOriginXaxisYaxis (origin:Pnt) (x:Vec) (y:Vec) = 
            let z = Vec.cross (x , y)
            if z.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane.createOriginXaxisYaxis from origin:%O, x:%O and y:%O" origin x y
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized, y.Unitized, z.Unitized)
    
        static member createOriginYaxis (origin:Pnt) (y:Vec) = 
            let x = Vec.cross (y , Vec.ZAxis)
            if x.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane.createOriginYaxis from %O and %O" origin  y
            let z = Vec.cross (x , y)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)

        static member createOriginXaxis (origin:Pnt) (x:Vec) = 
            let y = Vec.cross (Vec.ZAxis , x)
            if y.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane.createOriginXaxis from origin:%O and x:%O " origin x 
            let z = Vec.cross (x , y)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first point , X-axis to second Point, checks for collinear points    
        static member createThreePoints (origin:Pnt) (b:Pnt) (c:Pnt) = 
            let x = b-origin
            let yt = c-origin
            let z = Vec.cross (x , yt)
            if z.IsTiny 1e-6 then FsExGeoDivByZeroException.Raise "Cannot construct FsEx.Geo.PPlane.createThreePoints from %O, %O and %O, are they colinear?" origin b c
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first point , X-axis to second Point,
        static member createTwoPts (a:Pnt) (b:Pnt) = PPlane.createOriginXaxis a (b-a)

        (*

        /// Return a new plane with given Origin
        let inline setOrig (pt:Point3d) (pl:Plane) =   
            let mutable p = pl.Clone()
            p.Origin <- pt
            p 
        
        /// Return a new plane with given Origin X value changed.
        let inline setOrigX (x:float) (pl:Plane) =   
            let mutable p = pl.Clone()
            p.OriginX <- x
            p      
        
        /// Return a new plane with given Origin Y value changed.
        let inline setOrigY (y:float) (pl:Plane) =   
            let mutable p = pl.Clone()
            p.OriginY <- y
            p      
        
        /// Return a new plane with given Origin Z value changed.
        let inline setOrigZ (z:float) (pl:Plane) =   
            let mutable p = pl.Clone()
            p.OriginZ <- z
            p           

        /// Return a new plane with Origin translated by Vector3d.
        let inline translateBy (v:Vector3d) (pl:Plane) =   
            let mutable p = pl.Clone()
            p.Origin <- p.Origin + v
            p 
        
        /// Return a new plane with Origin translated in World X direction.
        let inline translateByWorldX (x:float) (pl:Plane) =   
            let mutable p = pl.Clone()
            p.OriginX <- p.OriginX + x
            p      
        
        /// Return a new plane with Origin translated in World Y direction.
        let inline translateByWorldY (y:float) (pl:Plane) =   
            let mutable p = pl.Clone()
            p.OriginY <- p.OriginY + y
            p      
        
        /// Return a new plane with Origin translated in World Z direction.
        let inline translateByWorldZ (z:float) (pl:Plane) =   
            let mutable p = pl.Clone()
            p.OriginZ <- p.OriginZ + z
            p

        
        /// Rotate about Z axis by angle in degree.  
        /// Counter clockwise in top view (for WorldXY Plane).
        let inline rotateZ (angDegree:float) (pl:Plane) =   
            let mutable p = pl.Clone()
            if not <| p.Rotate(UtilMath.toRadians angDegree, Vector3d.ZAxis) then 
                RhinoScriptingException.Raise "Rhino.Scripting.Extension.RhPlane.rotateZ by %s for %s" angDegree.ToNiceString pl.ToNiceString
            p
        *)



        /// Move Plane along the local X-axis by the given distance
        static member translateX (d:float) (pl:PPlane) = PPlane(pl.Origin + pl.Xaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 

        /// Move Plane along the local Y-axis by the given distance
        static member translateY (d:float) (pl:PPlane) = PPlane(pl.Origin + pl.Yaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 

        /// Move Plane along the local Z-axis by the given distance
        /// Same as PPlane.offset
        static member translateZ (d:float) (pl:PPlane) = PPlane(pl.Origin + pl.Zaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 
        
        /// Move Plane along the local Z-axis by the given distance
        /// Same as PPlane.translateZ
        static member offset (d:float) (pl:PPlane) = PPlane(pl.Origin + pl.Zaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 

        static member flipOnY (pl:PPlane) = PPlane(pl.Origin , -pl.Xaxis, pl.Yaxis, -pl.Zaxis) 
    
        static member flipOnX (pl:PPlane) = PPlane(pl.Origin , pl.Xaxis, -pl.Yaxis, -pl.Zaxis) 
    
        static member rotateOnZ180 (pl:PPlane) = PPlane(pl.Origin , -pl.Xaxis, -pl.Yaxis, pl.Zaxis) 

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
