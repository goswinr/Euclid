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
        static member fromOriginXaxisAndYaxis (origin:Pnt) (x:Vec) (y:Vec) = 
            let z = Vec.cross (x , y)
            if z.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane.fromOriginXaxisAndYaxis from origin:%O, x:%O and y:%O" origin x y
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized, y.Unitized, z.Unitized)
    
        static member fromOriginAndYaxis (origin:Pnt) (y:Vec) = 
            let x = Vec.cross (y , Vec.ZAxis)
            if x.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane.fromOriginAndYaxis from %O and %O" origin  y
            let z = Vec.cross (x , y)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)

        static member fromOriginAndXaxis (origin:Pnt) (x:Vec) = 
            let y = Vec.cross (Vec.ZAxis , x)
            if y.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane.fromOriginAndXaxis from origin:%O and x:%O " origin x 
            let z = Vec.cross (x , y)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first point , X-axis to second Point, checks for collinear points    
        static member fromThreePoints (origin:Pnt) (b:Pnt) (c:Pnt) = 
            let x = b-origin
            let yt = c-origin
            let z = Vec.cross (x , yt)
            if z.IsTiny 1e-6 then FsExGeoDivByZeroException.Raise "Cannot construct FsEx.Geo.PPlane.fromThreePoints from %O, %O and %O, are they colinear?" origin b c
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first point , X-axis to second Point,
        static member fromTwoPts (a:Pnt) (b:Pnt) = PPlane.fromOriginAndXaxis a (b-a)

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

        /// Transforms the plane by the given matrix
        /// If the transformation includes a shear the only X-axis can be kept.
        /// The plane will be defined with the direction of Y-axis ( which might not be perpendicular after shearing ).
        /// The returned PPlane has orthogonal unit vectors
        static member transform (m:Matrix) (pl:PPlane) = 
            let o = Pnt.transform m pl.Origin
            let x = UnitVec.transformBy3x3PartOnly m pl.Xaxis
            let y = UnitVec.transformBy3x3PartOnly m pl.Yaxis
            let z = UnitVec.transformBy3x3PartOnly m pl.Zaxis 
            // test if any shear happened:           
            if abs(x*y) < zeroLengthTol && abs(x*z) < zeroLengthTol && abs(y*z) < zeroLengthTol then 
                PPlane(o,x.Unitized,y.Unitized,z.Unitized) // unitize to ignore any scaling
            else 
                PPlane.fromOriginXaxisAndYaxis o x y
                
        /// Rotate Plane 180 Degrees around Z-axis if the Y-axis orientation does not match World Y (pl.Yax.Y < 0.0)
        /// To ensure that Y is always positive. For example for showing Text 
        static member rotateZ180IfYNegative (pl:PPlane) = 
            if pl.Yaxis.Y < 0.0 then PPlane.rotateOnZ180 pl else pl
