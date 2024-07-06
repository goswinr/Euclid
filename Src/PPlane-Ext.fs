namespace Euclid

open Euclid.UtilEuclid

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type PPlane.
[<AutoOpen>]
module AutoOpenPPlane2 =

    // This file is only for extension members that depend on Line3D !!

    type PPlane with


        /// Returns the angle to a Line3D in Degree, ignoring the normal's orientation.
        /// So between 0 to 90 degrees.
        member inline pl.Angle90ToLine (ln:Line3D) = UnitVec.angle90 ln.UnitTangent pl.Zaxis

        member inline pl.AsPlane = Plane.create(pl.Origin, pl.Zaxis)

        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------



        /// Returns the line of intersection between two planes.
        /// Returns None if they are parallel or coincident.
        static member intersect (a:PPlane) (b:PPlane) : Line3D option=
            let bn = b.Zaxis
            let an = a.Zaxis
            let v = UnitVec.cross (an, bn)
            if isTooSmallSq v.LengthSq then
                // EuclidException.Raise "Euclid.PPlane.intersect: Planes are parallel or coincident: %O, %O" a b
                None
            else
                let pa = Vec.cross(v, an)
                let nenner = pa *** bn
                let ao = a.Origin
                let t = ((b.Origin - ao) *** bn) / nenner
                let xpt = ao + pa * t
                Some <| Line3D.createFromPntAndVec (xpt, v)

        /// Returns the parameter on the line.
        /// The parameter is the intersection point of the infinite Line3D with the PPlane.
        /// Returns None if they are parallel or coincident.
        static member intersectLineParameter  (ln:Line3D) (pl:PPlane) : float option =
            let z = pl.Zaxis
            let nenner = ln.Tangent *** z
            if isTooSmall (abs nenner) then
                // EuclidException.Raise "Euclid.PPlane.intersectLineParameter: Line and Plane are parallel or line has zero length: %O, %O" ln pl
                None
            else
                Some <| ((pl.Origin - ln.From) *** z) / nenner


        /// Returns the line parameter and the X and Y parameters on the Plane. as tuple (pLn, pPlX, pPlY).
        /// The parameters is the intersection point of the infinite Line3D with the PPlane.
        /// Returns None if they are parallel or coincident.
        static member intersectLineParameters  (ln:Line3D) (pl:PPlane) : option<float*float*float> =
            let z = pl.Zaxis
            let v = ln.Tangent
            let nenner = v *** z
            if isTooSmall (abs nenner) then
                // EuclidException.Raise "Euclid.PPlane.intersectLineParameters: Line and Plane are parallel or line has zero length: %O, %O" ln pl
                None
            else
                let t = ((pl.Origin - ln.From) *** z) / nenner
                let xpt = ln.From + v * t
                let v = xpt-pl.Origin
                Some <| (t, pl.Xaxis *** v, pl.Yaxis *** v)

        /// Returns intersection point of infinite Line3D with Plane.
        /// Returns None if they are parallel.
        static member intersectLine (ln:Line3D) (pl:PPlane) : Pnt option =
            match PPlane.intersectLineParameter ln pl with
            | Some t -> Some (ln.From + ln.Tangent * t)
            | None -> None

        /// Checks if a finite Line3D intersects with Plane in one point.
        /// Returns false for NaN values or (almost) parallel or coincident lines.
        static member inline doLinePlaneIntersect (ln:Line3D) (pl:PPlane) =
            let nenner = ln.Tangent *** pl.Zaxis
            if isTooSmall (abs nenner) then
                false
            else
                let t = ((pl.Origin - ln.From) *** pl.Zaxis) / nenner // if nenner is 0.0 then 't' is Infinity
                0. <= t && t <= 1.
