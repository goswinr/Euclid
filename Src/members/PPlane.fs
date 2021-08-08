namespace FsEx.Geo

open FsEx.Geo.Util

/// Static and Instance Members for PPlane
[<AutoOpen>]
module AutoOpenPPlane =     
    
    // pre allocate , used often 
    let private worldXY = PPlane(Pnt.Origin,UnitVec.XAxis,UnitVec.YAxis,UnitVec.ZAxis)
    
    type PPlane with  

        /// Evaluate at 3D parameter
        member p.At (px:float, py:float, pz:float) = p.Origin + p.Xax*px + p.Yax*py + p.Zax*pz
        
        /// Evaluate at 2D parameter ( Z = 0.0)
        member p.AtXY (px:float, py:float) = p.Origin + p.Xax*px + p.Yax*py 

        /// Returns the World Coordinate System Plane at World Origin
        /// X axis = world X axis 
        /// Y axis = world Y axis 
        /// Z axis = world Z axis 
        static member WorldTop = worldXY

        /// The resulting PPlane wil have the X-Axis in direction of X. 
        /// x and y will define the plane and the side that Z will be on.
        /// The given y does not need to be perpendicular to x, just not parallel.
        static member fromPtXY (origin:Pnt) (x:Vec) (y:Vec) = 
            let z = Vec.cross (x , y)
            if z.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane fromPtXY from origin:%O, x:%O and y:%O" origin x y
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized, y.Unitized, z.Unitized)
    
        static member fromPtY (origin:Pnt) (y:Vec) = 
            let x = Vec.cross (y , Vec.ZAxis)
            if x.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane fromPtY from %O and %O" origin  y
            let z = Vec.cross (x , y)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)

        static member fromPtX (origin:Pnt) (x:Vec) = 
            let y = Vec.cross (Vec.ZAxis , x)
            if y.IsTiny 1e-6 then FsExGeoException.Raise "Cannot construct FsEx.Geo.PPlane fromPtX from origin:%O and x:%O " origin x 
            let z = Vec.cross (x , y)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first Point , X axis to second Point, checks for collinear points    
        static member from3Pts (origin:Pnt) (b:Pnt) (c:Pnt) = 
            let x = b-origin
            let yt = c-origin
            let z = Vec.cross (x , yt)
            if z.IsTiny 1e-6 then FsExGeoDivByZeroException.Raise "Cannot construct FsEx.Geo.PPlane by3Pt from %O, %O and %O, are they colinear?" origin b c
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first Point , X axis to second Point,
        static member from2Pts (a:Pnt) (b:Pnt) = PPlane.fromPtX a (b-a)

        static member translateX (d:float) (pl:PPlane) = PPlane(pl.Origin + pl.Xax*d, pl.Xax, pl.Yax, pl.Zax) 
    
        static member translateY (d:float) (pl:PPlane) = PPlane(pl.Origin + pl.Yax*d, pl.Xax, pl.Yax, pl.Zax) 
    
        /// Same as PPlane.offset
        static member translateZ (d:float) (pl:PPlane) = PPlane(pl.Origin + pl.Zax*d, pl.Xax, pl.Yax, pl.Zax) 
    
        /// Same as PPlane.translateZ
        static member offset (d:float) (pl:PPlane) = PPlane(pl.Origin + pl.Zax*d, pl.Xax, pl.Yax, pl.Zax) 

        static member flipOnY (pl:PPlane) = PPlane(pl.Origin , -pl.Xax, pl.Yax, -pl.Zax) 
    
        static member flipOnX (pl:PPlane) = PPlane(pl.Origin , pl.Xax, -pl.Yax, -pl.Zax) 
    
        static member rotateOnZ180 (pl:PPlane) = PPlane(pl.Origin , -pl.Xax, -pl.Yax, pl.Zax) 

    
        /// If the transformation includes a shear the only X axis can be kept.
        /// The plane will be defined with the direction of Y Axis ( which might not be perpendicular after shearing ).
        /// The returned PPlane has orthogonal unit vectors
        static member transform (m:Matrix) (pl:PPlane) = 
            let o  = Pnt.transform m pl.Origin
            let px = Pnt.transform m (pl.Origin+pl.Xax)
            let py = Pnt.transform m (pl.Origin+pl.Yax)
            PPlane.fromPtXY  o (px-o) (py-o)
    
        /// Rotate Plane 180 degrees around Z-axis if the Y Axis orientation does not match World Y (pl.Yax.Y < 0.0)
        /// To ensure that Y is always positive. For example for showing Text 
        static member rotateZ180IfYNegative (pl:PPlane) = 
            if pl.Yax.Y < 0.0 then PPlane.rotateOnZ180 pl else pl
