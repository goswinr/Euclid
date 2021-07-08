namespace FsEx.Geo

open FsEx.Geo.Util

/// Members and operators for 3D Points, Vectors and Rotations
[<AutoOpen>]
module AutoOpenPPPlane =     

    type PPlane with  
    
        member p.At (px:float, py:float, pz:float) = p.Origin + p.Xax*px + p.Yax*py + p.Zax*pz

        member p.AtXY (px:float, py:float) = p.Origin + p.Xax*px + p.Yax*py 

        /// The resulting PPlane wil have the X-Axis in direction of X. 
        /// x and y will define the plane and the side that Z will be on.
        /// The given y does not need to be perpendicular to x, just not parallel.
        static member fromPtAndXYvec (origin:Pnt) (x:Vec) (y:Vec) = 
            let z = Vec.cross (x , y)
            if z.IsTiny 1e-6 then failwithf "Cannot construct PPlane byPtAndXYvec from %O, %O and %O" origin x y
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized, y.Unitized, z.Unitized)
    
        static member fromPtAndYvec (origin:Pnt) (y:Vec) = 
            let x = Vec.cross (y , Vec.ZAxis)
            if x.IsTiny 1e-6 then failwithf "Cannot construct PPlane byPtAndXYvec from %O, %O and %O" origin x y
            let z = Vec.cross (x , y)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)

        static member fromPtAndXvec (origin:Pnt) (x:Vec) = 
            let y = Vec.cross (Vec.ZAxis , x)
            if y.IsTiny 1e-6 then failwithf "Cannot construct PPlane byPtAndXYvec from a%O, x%O and %O, are a and x vertical?" origin x y
            let z = Vec.cross (x , y)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first Point , X axis to second Point, checks for coliniear points    
        static member from3Pts (origin:Pnt) (b:Pnt) (c:Pnt) = 
            let x = b-origin
            let yt = c-origin
            let z = Vec.cross (x , yt)
            if z.IsTiny 1e-6 then failwithf "Cannot construct PPlane by3Pt from %O, %O and %O, are they colinear?" origin b c
            let y = Vec.cross (z , x)
            PPlane(origin,x.Unitized,y.Unitized,z.Unitized)
    
        /// Builds Plane at first Point , X axis to second Point,
        static member from2Pts (a:Pnt) (b:Pnt) = PPlane.fromPtAndXvec a (b-a)

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
        /// The returned PPlane has orthogonal Unitvectors
        static member transform (m:Matrix) (pl:PPlane) = 
            let o  = Pnt.transform m pl.Origin
            let px = Pnt.transform m (pl.Origin+pl.Xax)
            let py = Pnt.transform m (pl.Origin+pl.Yax)
            PPlane.fromPtAndXYvec  o (px-o) (py-o)
    
        /// To ensure that Y is always positive. For example for showing Text 
        static member rotateZ180ifYdown (pl:PPlane) = if pl.Yax.Y < 0.0 then PPlane.rotateOnZ180 pl else pl
