namespace FsEx.Geo

open FsEx.Geo.Util

/// When FsEx.Geo is opened this module will be auto-opened. 
/// It only contains extension members for type PPlane
[<AutoOpen>]
module AutoOpenPPlane =     
    

    // pre allocate , used often 
    let private worldXY = PPlane(Pnt.Origin, UnitVec.Xaxis, UnitVec.Yaxis, UnitVec.Zaxis)
    
    type PPlane with  

        /// Returns signed distance of point to plane, also indicating on which side it is.
        member inline pl.DistanceToPt pt = pl.Zaxis*(pt-pl.Origin) 

        /// Returns the closest point on the plane from a test point.
        member inline pl.ClosestPoint pt = pt - pl.Zaxis*(pl.DistanceToPt pt) 

        /// Returns the X, Y and Z parameters of a point with regards to the plane.
        member inline pl.PointParameters pt = 
            let v = pt-pl.Origin
            pl.Xaxis*v, pl.Yaxis*v, pl.Zaxis*v

        /// First finds the closet point on plane from a test point.
        /// Then returns a new plane with Origin at this point and the same Axes vectors
        member inline pl.PlaneAtClPt pt = 
            let o = pl.ClosestPoint pt
            PPlane.setOrigin o pl

        /// Returns the Angle to another Plane in Degree, ignoring the plane's orientation. 
        /// So between 0 to 90 degrees
        member inline this.AngleToPlane (pl:PPlane) = UnitVec.angle90 this.Zaxis pl.Zaxis 

        /// Returns the Angle to 3D vector in Degree, ignoring the plane's orientation.
        /// So between 0 to 90 degrees
        member inline pl.AngleToVec (v:Vec) = UnitVec.angle90 v.Unitized pl.Zaxis 

        /// Returns the Angle to 3D unit-vector in Degree, ignoring the plane's orientation. 
        /// So between 0 to 90 degrees
        member inline pl.AngleToVec (v:UnitVec) = UnitVec.angle90 v pl.Zaxis 

        /// Evaluate at 3D parameter
        member inline p.EvaluateAt (px:float, py:float, pz:float) = p.Origin + p.Xaxis*px + p.Yaxis*py + p.Zaxis*pz
        
        /// Evaluate at 2D parameter ( Z parameter = 0.0)
        member inline p.EvaluateAtXY (px:float, py:float) = p.Origin + p.Xaxis*px + p.Yaxis*py 

        /// Checks if two PPlanes are coincident within the distance tolerance. 1e-6 by default.
        /// This means that their Z-axes are parallel within the angle tolerance
        /// and the distance of second origin to the first plane is less than the distance tolerance.
        /// The default angle tolerance is 0.25 degrees.  
        /// This tolerance can be customized by an optional minium cosine value.
        /// See FsEx.Geo.Cosine module.
        member inline pl.IsCoincidentTo (other:PPlane, 
                                        [<OPT;DEF(1e-6)>] distanceTolerance:float, 
                                        [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine>) =          
            pl.Zaxis.IsParallelTo(other.Zaxis,minCosine)
            && 
            pl.DistanceToPt other.Origin < distanceTolerance


        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Checks if two 3D Parametrized Planes are equal within tolerance.
        static member inline equals tol (a:PPlane) (b:PPlane) =
            let tt = tol*tol
            Pnt.distanceSq a.Origin b.Origin < tt &&
            UnitVec.differenceSq a.Xaxis b.Xaxis < tt &&
            UnitVec.differenceSq a.Yaxis b.Yaxis < tt &&
            UnitVec.differenceSq a.Zaxis b.Zaxis < tt
      

        /// Checks if two 3D Parametrized Planes are coincident.
        /// This means that the Z-axes are parallel within 0.25 degrees
        /// and the distance of  second origin to the first plane is less than 1e-6 units tolerance.
        static member inline areCoincident tol (a:PPlane) (b:PPlane) = a.IsCoincidentTo (b)

        /// Returns the World Coordinate System Plane at World Origin
        /// X-axis = World X-axis 
        /// Y-axis = World Y-axis 
        /// Z-axis = World Z-axis 
        /// same as PPlane.WorldTop
        static member (*inline*) WorldXY = worldXY

        /// Returns the World Coordinate System Plane at World Origin
        /// X-axis = World X-axis 
        /// Y-axis = World Y-axis 
        /// Z-axis = World Z-axis 
        /// same as PPlane.WorldXY
        static member (*inline*) WorldTop = worldXY  

        /// Returns the Coordinate System Plane of a Front view
        /// X-axis = World X-axis 
        /// Y-axis = World Z-axis 
        /// Z-axis = minus World Y-axis 
        static member (*inline*) WorldFront = 
            PPlane(Pnt.Origin, UnitVec.Xaxis, UnitVec.Zaxis, -UnitVec.Yaxis)

        /// Returns the Coordinate System Plane of a Right view
        /// X-axis = World Y-axis 
        /// Y-axis = World Z-axis 
        /// Z-axis = World X-axis 
        static member (*inline*) WorldRight = 
            PPlane(Pnt.Origin, UnitVec.Yaxis, UnitVec.Zaxis, UnitVec.Xaxis)

        /// Returns the Coordinate System Plane of a Left view
        /// X-axis = minus World Y-axis 
        /// Y-axis = World Z-axis 
        /// Z-axis = minus World X-axis 
        static member (*inline*) WorldLeft = 
            PPlane(Pnt.Origin, -UnitVec.Yaxis, UnitVec.Zaxis, -UnitVec.Xaxis)

        /// Returns the Coordinate System Plane of a Back view
        /// X-axis = minus World X-axis 
        /// Y-axis = World Z-axis 
        /// Z-axis = World Y-axis 
        static member (*inline*) WorldBack = 
            PPlane(Pnt.Origin, -UnitVec.Xaxis, UnitVec.Zaxis, UnitVec.Yaxis)  
            
        /// Returns the Coordinate System Plane of a Bottom view
        /// X-axis = World X-axis 
        /// Y-axis = minus World Y-axis 
        /// Z-axis = minus World Z-axis 
        static member (*inline*) WorldBottom = 
            PPlane(Pnt.Origin, UnitVec.Xaxis, -UnitVec.Yaxis, -UnitVec.Zaxis)   

        /// Creates a Parametrized Plane from a point and vector representing the X-axis.
        /// The resulting PPlane will have the X-Axis in direction of X vector. 
        /// The X and Y vectors will define the plane and the side that Z will be on.
        /// The given Y vector does not need to be perpendicular to the X vector , just not parallel.
        static member createOriginXaxisYaxis (origin:Pnt,x:Vec,y:Vec) = 
            let z = Vec.cross (x , y)
            if z.LengthSq < 1e-12 then 
                FsExGeoException.Raise "FsEx.Geo.PPlane.createOriginXaxisYaxis failed for origin:%O, x:%O and y:%O" origin x y
            let y = Vec.cross (z , x)
            PPlane(origin, x.Unitized, y.Unitized, z.Unitized)

        /// Creates a Parametrized Plane from a point and a unit-vector representing the X-axis.
        /// The resulting PPlane will have the X-Axis in direction of X vector. 
        /// The X and Y vectors will define the plane and the side that Z will be on.
        /// The given Y vector does not need to be perpendicular to the X vector , just not parallel.
        static member createOriginXaxisYaxis (origin:Pnt,x:UnitVec,y:UnitVec) = 
            let z = UnitVec.cross (x , y) 
            if z.LengthSq < 1e-12 then 
                FsExGeoException.Raise "FsEx.Geo.PPlane.createOriginXaxisYaxis failed for origin:%O, x:%O and y:%O" origin x y
            let y = Vec.cross (z , x)
            PPlane(origin, x, y.Unitized, z.Unitized)            

        /// Creates a Parametrized Plane from a point and vector representing the Z-axis.
        /// The X-axis will be found by taking the cross product of the World Y-axis and the given Z-axis.
        /// If this fails the cross product of the World X-axis and the given Z-axis will be used.
        static member createOriginZaxis (origin:Pnt,z:Vec) = 
            let x = Vec.cross (Vec.Yaxis , z)
            if x.LengthSq < 1e-12 then 
                let x = Vec.cross (Vec.Xaxis , z)
                let y = Vec.cross (z, x)
                PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
            else 
                let y = Vec.cross (z, x)
                PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
        
        /// Creates a Parametrized Plane from a point and unit-vector representing the Z-axis.
        /// The X-axis will be found by taking the cross product of the World Y-axis and the given Z-axis.
        /// If this fails the cross product of the World X-axis and the given Z-axis will be used.
        static member createOriginZaxis (origin:Pnt,z:UnitVec) = 
            let x = Vec.cross (Vec.Yaxis , z)
            if x.LengthSq < 1e-12 then 
                let x = Vec.cross (Vec.Xaxis , z)
                let y = Vec.cross (z, x)
                PPlane(origin,x.Unitized,y.Unitized,z)
            else 
                let y = Vec.cross (z, x)
                PPlane(origin,x.Unitized,y.Unitized,z)

        /// Creates a Parametrized Plane from a point and vector representing the Y-axis.
        /// The Z-axis will be found by taking the cross product of the World X-axis with the given Y-axis.
        /// If this fails the cross product of the World Y-axis and the given Y-axis will be used.
        static member createOriginYaxis (origin:Pnt,y:Vec) = 
            let z = Vec.cross (Vec.Xaxis , y)
            if z.LengthSq < 1e-12 then 
                let z = Vec.cross (Vec.Yaxis , y)
                let x = Vec.cross (y, z)
                PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
            else 
                let x = Vec.cross (y, z)
                PPlane(origin,x.Unitized,y.Unitized,z.Unitized)

        /// Creates a Parametrized Plane from a point and unit-vector representing the Y-axis.
        /// The Z-axis will be found by taking the cross product of the World X-axis with the given Y-axis.
        /// If this fails the cross product of the World Y-axis and the given Y-axis will be used.
        static member createOriginYaxis (origin:Pnt,y:UnitVec) = 
            let z = Vec.cross (Vec.Xaxis , y)
            if z.LengthSq < 1e-12 then 
                let z = Vec.cross (Vec.Yaxis , y)
                let x = Vec.cross (y, z)
                PPlane(origin,x.Unitized,y,z.Unitized)
            else 
                let x = Vec.cross (y, z)
                PPlane(origin,x.Unitized,y,z.Unitized)

                

        /// Creates a Parametrized Plane from a point and vector representing the X-axis.
        /// The Z-axis will be found by taking the cross product of the given X-axis and the World Y-axis.
        /// If this fails the cross product of the given X-axis and the World X-axis will be used.
        static member createOriginXaxis (origin:Pnt,x:Vec) = 
            let z = Vec.cross (x , Vec.Yaxis)
            if z.LengthSq < 1e-12 then 
                let z = Vec.cross (x , Vec.Xaxis)
                let y = Vec.cross (z, x)
                PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
            else 
                let y = Vec.cross (z, x)
                PPlane(origin,x.Unitized,y.Unitized,z.Unitized)

        /// Creates a Parametrized Plane from a point and unit-vector representing the X-axis.
        /// The Z-axis will be found by taking the cross product of the given X-axis and the World Y-axis.
        /// If this fails the cross product of the given X-axis and the World X-axis will be used.
        static member createOriginXaxis (origin:Pnt,x:UnitVec) = 
            let z = Vec.cross (x , Vec.Yaxis)
            if z.LengthSq < 1e-12 then 
                let z = Vec.cross (x , Vec.Xaxis)
                let y = Vec.cross (z, x)
                PPlane(origin,x,y.Unitized,z.Unitized)
            else 
                let y = Vec.cross (z, x)
                PPlane(origin,x,y.Unitized,z.Unitized)
                
    
        /// Builds Plane at first point , X-axis to second point, checks for collinear points.    
        static member createThreePoints (origin:Pnt) (b:Pnt) (c:Pnt) = 
            let x = b-origin
            let yt = c-origin
            let z = Vec.cross (x , yt)
            if z.LengthSq < 1e-12 then 
                FsExGeoException.Raise "FsEx.Geo.PPlane.createThreePoints failed for %O, %O and %O, are they colinear?" origin b c
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first point.
        /// The resulting PPlane will have the X-Axis in direction of the second point. 
        /// The Y-axis will be found by taking the cross product of the World Z-axis and the X-axis.
        /// If this fails the cross product of the World Y-axis and the given X-axis will be used.
        static member inline createTwoPts (a:Pnt) (b:Pnt) = 
            PPlane.createOriginXaxis (a, b-a)        

        /// WorldXY rotated 180 degrees round Z-axis.
        static member (*inline*) WorldMinusXMinusY= 
            PPlane(Pnt.Origin, -UnitVec.Xaxis, -UnitVec.Yaxis, UnitVec.Zaxis)

        /// WorldXY rotated 90 degrees round Z-axis counter clockwise from top.
        static member (*inline*) WorldYMinusX= 
            PPlane(Pnt.Origin, UnitVec.Yaxis, -UnitVec.Xaxis,UnitVec.Zaxis)

        /// WorldXY rotated 270 degrees round Z-axis counter clockwise from top.
        static member (*inline*) WorldMinusYX= 
            PPlane(Pnt.Origin, -UnitVec.Yaxis, UnitVec.Xaxis,UnitVec.Zaxis)

        /// WorldXY rotated 180 degrees round X-axis, Z points down now.
        static member (*inline*) WorldXMinusY= 
            PPlane(Pnt.Origin, UnitVec.Xaxis, -UnitVec.Yaxis, -UnitVec.Zaxis)

        /// Return a new plane with given Origin
        static member (*inline*) setOrigin (pt:Pnt) (pl:PPlane) =   
            PPlane(pt, pl.Xaxis, pl.Yaxis, pl.Zaxis)
            
        /// Return a new plane with given Origin X value changed.
        static member (*inline*) setOriginX (x:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.setX x, pl.Xaxis, pl.Yaxis, pl.Zaxis)    
        
        /// Return a new plane with given Origin Y value changed.
        static member (*inline*) setOriginY (y:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.setY y, pl.Xaxis, pl.Yaxis, pl.Zaxis)        
        
        /// Return a new plane with given Origin Z value changed.
        static member (*inline*) setOriginZ (z:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.setZ z, pl.Xaxis, pl.Yaxis, pl.Zaxis)               

        /// Return a new plane with Origin translated by Vec.
        static member (*inline*) translateBy (v:Vec) (pl:PPlane) =   
            PPlane(pl.Origin + v , pl.Xaxis, pl.Yaxis, pl.Zaxis)   
        
        /// Return a new plane with Origin translated in World X direction.
        static member (*inline*) translateByWorldX (x:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.moveX x, pl.Xaxis, pl.Yaxis, pl.Zaxis)       
        
        /// Return a new plane with Origin translated in World Y direction.
        static member (*inline*) translateByWorldY (y:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.moveY y, pl.Xaxis, pl.Yaxis, pl.Zaxis)       
        
        /// Return a new plane with Origin translated in World Z direction.
        static member (*inline*) translateByWorldZ (z:float) (pl:PPlane) =   
            PPlane(pl.Origin |> Pnt.moveZ z, pl.Xaxis, pl.Yaxis, pl.Zaxis)  
        
        /// Rotate about Z-axis by angle in degree.  
        /// Counter clockwise in top view (for WorldXY Plane).
        static member (*inline*) rotateZ (angDegree:float) (pl:PPlane) =   
            let q = Quaternion.createFromDegree (pl.Zaxis, angDegree)
            PPlane(pl.Origin , pl.Xaxis*q, pl.Yaxis*q, pl.Zaxis) 

        /// Move Plane along the local X-axis by the given distance
        static member (*inline*) translateX (d:float) (pl:PPlane) = 
            PPlane(pl.Origin + pl.Xaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 

        /// Move Plane along the local Y-axis by the given distance
        static member (*inline*) translateY (d:float) (pl:PPlane) = 
            PPlane(pl.Origin + pl.Yaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 

        /// Move Plane along the local Z-axis by the given distance
        /// Same as PPlane.offset
        static member (*inline*) translateZ (d:float) (pl:PPlane) = 
            PPlane(pl.Origin + pl.Zaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 
        
        /// Move Plane along the local Z-axis by the given distance
        /// Same as PPlane.translateZ
        static member (*inline*) offset (d:float) (pl:PPlane) = 
            PPlane(pl.Origin + pl.Zaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis) 

        /// Rotate the Plane 180 degrees on its Y-axis.
        /// Called flip because Z-axis points in the opposite direction.
        static member (*inline*) flipOnY (pl:PPlane) = 
            PPlane(pl.Origin , -pl.Xaxis, pl.Yaxis, -pl.Zaxis) 
        
        /// Rotate the Plane 180 degrees on its X-axis.
        /// Called flip because Z-axis points in the opposite direction.
        static member (*inline*) flipOnX (pl:PPlane) = 
            PPlane(pl.Origin , pl.Xaxis, -pl.Yaxis, -pl.Zaxis) 
        
        /// Rotate the Plane 180 degrees on its Z-axis.
        static member (*inline*) rotateOnZ180 (pl:PPlane) = 
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
        static member (*inline*) rotateZ180IfYNegative (pl:PPlane) = 
            if pl.Yaxis.Y < 0.0 then PPlane.rotateOnZ180 pl else pl


