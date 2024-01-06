namespace Euclid

open Euclid.Util

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type PPlane.
[<AutoOpen>]
module AutoOpenPPlane2 =

    // This file is only for extension members that depend on Line3D !!
    // This file is only for extension members that depend on Line3D !!
    // This file is only for extension members that depend on Line3D !!

    type PPlane with


        /// Returns the angle to a Line3D in Degree, ignoring the normal's orientation.
        /// So between 0 to 90 degrees.
        member inline pl.AngleToLine (ln:Line3D) = UnitVec.angle90 ln.Tangent.Unitized pl.Zaxis

        member inline pl.AsPlane = Plane.create(pl.Origin, pl.Zaxis)

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------


        /// Returns the line of intersection between two planes.
        /// Fails if they are parallel.
        static member intersect  (a:PPlane) (b:PPlane) =
            let bn = b.Zaxis
            let an = a.Zaxis
            let ao = a.Origin
            let v = UnitVec.cross (an, bn)
            if v.LengthSq < 1e-18 then
                EuclidException.Raise "Euclid.PPlane.intersect: Planes are parallel or coincident: %O, %O" a b
            let pa = Vec.cross(v, an)
            let nenner = pa *** bn
            let t = ((b.Origin - ao) *** bn) / nenner
            let xpt = ao + pa * t
            Line3D.createFromPntAndVec (xpt, v)

        /// Returns the parameter on the line.
        /// The parameter is the intersection point of the infinite Line3D with the PPlane.
        /// Fails if they are parallel or coincident.
        static member intersectLineParameter  (ln:Line3D) (pl:PPlane) =
            let z = pl.Zaxis
            let nenner = ln.Tangent *** z
            if abs nenner < 1e-9 then
                EuclidException.Raise "Euclid.PPlane.intersectLineParameter: Line and Plane are parallel or line has zero length: %O, %O" ln pl
            ((pl.Origin - ln.From) *** z) / nenner


        /// Returns the line parameter and the X and Y parameters on the Plane.
        /// The parameters is the intersection point of the infinite Line3D with the PPlane.
        /// Fails if they are parallel or coincident.
        static member intersectLineParameters  (ln:Line3D) (pl:PPlane) =
            let z = pl.Zaxis
            let v = ln.Tangent
            let nenner = v *** z
            if abs nenner < 1e-9 then
                EuclidException.Raise "Euclid.PPlane.intersectLineParameters: Line and Plane are parallel or line has zero length: %O, %O" ln pl
            let t = ((pl.Origin - ln.From) *** z) / nenner
            let xpt = ln.From + v * t
            let v = xpt-pl.Origin
            t, pl.Xaxis *** v, pl.Yaxis *** v

        /// Returns intersection point of infinite Line3D with Plane.
        /// Fails if they are parallel.
        static member intersectLine (ln:Line3D) (pl:PPlane) =
            ln.EvaluateAt <| PPlane.intersectLineParameter  ln pl

        /// Checks if a finite Line3D intersects with Plane in one point.
        /// Returns false for parallel and coincident lines.
        static member inline doLinePlaneIntersect (ln:Line3D) (pl:PPlane) =
            let nenner = ln.Tangent *** pl.Zaxis
            let t = ((pl.Origin - ln.From) *** pl.Zaxis) / nenner // if nenner is 0.0 then 't' is Infinity
            0. <= t && t <= 1.
