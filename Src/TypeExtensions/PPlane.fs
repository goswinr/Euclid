namespace Euclid

open Euclid.UtilEuclid

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type PPlane.
[<AutoOpen>]
module AutoOpenPPlane =

    // pre allocate, used often
    let private worldXY = PPlane.createUnchecked(Pnt.Origin, UnitVec.Xaxis, UnitVec.Yaxis, UnitVec.Zaxis)

    type PPlane with

        /// Returns signed distance of point to plane, also indicating on which side it is.
        member inline pl.DistanceToPt pt = pl.Zaxis *** (pt-pl.Origin)

        /// Returns the closest point on the plane from a test point.
        member inline pl.ClosestPoint pt = pt - pl.Zaxis*(pl.DistanceToPt pt)

        /// Returns the X, Y and Z parameters of a point with regards to the plane.
        member inline pl.PointParameters pt =
            let v = pt-pl.Origin
            pl.Xaxis *** v, pl.Yaxis *** v, pl.Zaxis *** v

        /// First finds the closet point on plane from a test point.
        /// Then returns a new plane with Origin at this point and the same Axes vectors.
        member inline pl.PlaneAtClPt pt =
            let o = pl.ClosestPoint pt
            PPlane.createUnchecked(o, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Returns the angle to another Plane in Degree, ignoring the plane's orientation.
        /// So between 0 to 90 degrees.
        member inline this.Angle90ToPlane (pl:PPlane) = UnitVec.angle90 this.Zaxis pl.Zaxis

        /// Returns the angle to 3D vector in Degree, ignoring the plane's orientation.
        /// So between 0 to 90 degrees.
        member inline pl.Angle90ToVec (v:Vec) = UnitVec.angle90 v.Unitized pl.Zaxis

        /// Returns the angle to 3D unit-vector in Degree, ignoring the plane's orientation.
        /// So between 0 to 90 degrees.
        member inline pl.Angle90ToVec (v:UnitVec) = UnitVec.angle90 v pl.Zaxis

        /// Evaluate at 3D parameter.
        member inline p.EvaluateAt (px:float, py:float, pz:float) = p.Origin + p.Xaxis*px + p.Yaxis*py + p.Zaxis*pz

        /// Evaluate at 2D parameter (Z parameter = 0.0)
        member inline p.EvaluateAtXY (px:float, py:float) = p.Origin + p.Xaxis*px + p.Yaxis*py

        /// Checks if two PPlanes are coincident within the distance tolerance. 1e-6 by default.
        /// This means that their Z-axes are parallel within the angle tolerance
        /// and the distance of second origin to the first plane is less than the distance tolerance.
        /// The default angle tolerance is 0.25 degrees.
        /// This tolerance can be customized by an optional minium cosine value.
        /// See Euclid.Cosine module.
        member inline pl.IsCoincidentTo (other:PPlane,
                                        [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                        [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine>) =
            pl.Zaxis.IsParallelTo(other.Zaxis, minCosine)
            &&
            pl.DistanceToPt other.Origin < distanceTolerance


        //----------------------------------------------------------------------------------------------
        //--------------------------  Static Members  --------------------------------------------------
        //----------------------------------------------------------------------------------------------

        /// Checks if two Parametrized Planes are equal within tolerance distance
        /// For the tips of its units vectors and its origin.
        /// Use a tolerance of 0.0 to check for an exact match.
        static member inline equals (tol:float) (a:PPlane) (b:PPlane) =
            abs (a.Origin.X - b.Origin.X) <= tol &&
            abs (a.Origin.Y - b.Origin.Y) <= tol &&
            abs (a.Origin.Z - b.Origin.Z) <= tol &&
            abs (a.Xaxis.X - b.Xaxis.X) <= tol &&
            abs (a.Xaxis.Y - b.Xaxis.Y) <= tol &&
            abs (a.Xaxis.Z - b.Xaxis.Z) <= tol &&
            abs (a.Yaxis.X - b.Yaxis.X) <= tol &&
            abs (a.Yaxis.Y - b.Yaxis.Y) <= tol &&
            abs (a.Yaxis.Z - b.Yaxis.Z) <= tol //&&
            //abs (a.Zaxis.X - b.Zaxis.X) <= tol &&
            //abs (a.Zaxis.Y - b.Zaxis.Y) <= tol &&
            //abs (a.Zaxis.Z - b.Zaxis.Z) <= tol



        /// Checks if two 3D Parametrized Planes are coincident within the distance tolerance..
        /// This means that the Z-axes are parallel within 0.25 degrees
        /// and the distance of second origin to the first plane is less than the tolerance.
        static member inline areCoincident tol (a:PPlane) (b:PPlane) = a.IsCoincidentTo (b,tol)

        /// Returns the World Coordinate System Plane at World Origin.
        /// X-axis = World X-axis
        /// Y-axis = World Y-axis
        /// Z-axis = World Z-axis
        /// same as PPlane.WorldTop
        static member WorldXY = worldXY

        /// Returns the World Coordinate System Plane at World Origin.
        /// X-axis = World X-axis
        /// Y-axis = World Y-axis
        /// Z-axis = World Z-axis
        /// same as PPlane.WorldXY
        static member WorldTop = worldXY

        /// Returns the Coordinate System Plane of a Front view.
        /// X-axis = World X-axis
        /// Y-axis = World Z-axis
        /// Z-axis = minus World Y-axis
        static member inline WorldFront =
            PPlane.createUnchecked(Pnt.Origin, UnitVec.Xaxis, UnitVec.Zaxis, -UnitVec.Yaxis)

        /// Returns the Coordinate System Plane of a Right view.
        /// X-axis = World Y-axis
        /// Y-axis = World Z-axis
        /// Z-axis = World X-axis
        static member inline WorldRight =
            PPlane.createUnchecked(Pnt.Origin, UnitVec.Yaxis, UnitVec.Zaxis, UnitVec.Xaxis)

        /// Returns the Coordinate System Plane of a Left view.
        /// X-axis = minus World Y-axis
        /// Y-axis = World Z-axis
        /// Z-axis = minus World X-axis
        static member inline WorldLeft =
            PPlane.createUnchecked(Pnt.Origin, -UnitVec.Yaxis, UnitVec.Zaxis, -UnitVec.Xaxis)

        /// Returns the Coordinate System Plane of a Back view.
        /// X-axis = minus World X-axis
        /// Y-axis = World Z-axis
        /// Z-axis = World Y-axis
        static member inline WorldBack =
            PPlane.createUnchecked(Pnt.Origin, -UnitVec.Xaxis, UnitVec.Zaxis, UnitVec.Yaxis)

        /// Returns the Coordinate System Plane of a Bottom view.
        /// X-axis = World X-axis
        /// Y-axis = minus World Y-axis
        /// Z-axis = minus World Z-axis
        static member inline WorldBottom =
            PPlane.createUnchecked(Pnt.Origin, UnitVec.Xaxis, -UnitVec.Yaxis, -UnitVec.Zaxis)

        /// WorldXY rotated 180 degrees round Z-axis.
        static member inline WorldMinusXMinusY=
            PPlane.createUnchecked(Pnt.Origin, -UnitVec.Xaxis, -UnitVec.Yaxis, UnitVec.Zaxis)

        /// WorldXY rotated 90 degrees round Z-axis Counter-Clockwise from top.
        static member inline WorldYMinusX=
            PPlane.createUnchecked(Pnt.Origin, UnitVec.Yaxis, -UnitVec.Xaxis, UnitVec.Zaxis)

        /// WorldXY rotated 270 degrees round Z-axis Counter-Clockwise from top.
        static member inline WorldMinusYX=
            PPlane.createUnchecked(Pnt.Origin, -UnitVec.Yaxis, UnitVec.Xaxis, UnitVec.Zaxis)

        /// WorldXY rotated 180 degrees round X-axis, Z points down now.
        static member inline WorldXMinusY=
            PPlane.createUnchecked(Pnt.Origin, UnitVec.Xaxis, -UnitVec.Yaxis, -UnitVec.Zaxis)

        /// Builds Plane at first point, X-axis to second point,
        /// Y-axis to third point or at lest in plane with third point.
        /// Fails if points are closer than 1e-5.
        static member createThreePoints (origin:Pnt) (xPt:Pnt) (yPt:Pnt) =
            let x = xPt-origin
            let y = yPt-origin
            let lx = x.Length
            let ly = y.Length
            if isTooSmall (lx) then  EuclidException.Raise "Euclid.PPlane.createThreePoints the distance between origin %s and xPt %s is too small" origin.AsString xPt.AsString
            if isTooSmall (ly) then  EuclidException.Raise "Euclid.PPlane.createThreePoints the distance between origin %s and yPt %s is too small" origin.AsString yPt.AsString
            let xf = 1./lx
            let yf = 1./ly
            let xu = UnitVec.createUnchecked(x.X*xf, x.Y*xf, x.Z*xf)
            let yu = UnitVec.createUnchecked(y.X*yf, y.Y*yf, y.Z*yf)
            if xu.IsParallelTo(yu, Cosine.``1.0``) then EuclidException.Raise "Euclid.PPlane.createThreePoints failed. The points are colinear by less than 1.0 degree, origin %s and xPt %s and yPt %s" origin.AsString xPt.AsString yPt.AsString
            let z = UnitVec.cross (xu, yu)
            let y' = Vec.cross (z, x)
            PPlane.createUnchecked(origin, xu, y'.Unitized, z.Unitized)

        /// Creates a Parametrized Plane from a point and a unit-vector representing the X-axis.
        /// The resulting PPlane will have the X-Axis in direction of X vector.
        /// The X and Y vectors will define the plane and the side that Z will be on.
        /// The given Y vector does not need to be perpendicular to the X vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-5.
        static member createOriginXaxisYaxis (origin:Pnt, xAxis:UnitVec, yAxis:UnitVec) =
            if xAxis.IsParallelTo(yAxis, Cosine.``1.0``) then
                EuclidException.Raise "Euclid.PPlane.createOriginXaxisYaxis failed. The vectors are colinear by less than 1.0 degrees, origin %s and xAxis%s and yAxis %s" origin.AsString xAxis.AsString yAxis.AsString
            let z = UnitVec.cross (xAxis, yAxis)
            let y = Vec.cross (z, xAxis)
            PPlane.createUnchecked(origin, xAxis, y.Unitized, z.Unitized)

        /// Creates a Parametrized Plane from a point and vector representing the X-axis.
        /// The resulting PPlane will have the X-Axis in direction of X vector.
        /// The X and Y vectors will define the plane and the side that Z will be on.
        /// The given Y vector does not need to be perpendicular to the X vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-5.
        static member createOriginXaxisYaxis (origin:Pnt, xAxis:Vec, yAxis:Vec) =
            let lx = xAxis.Length
            let ly = yAxis.Length
            if isTooSmall (lx) then  EuclidException.Raise "Euclid.PPlane.createOriginXaxisYaxis the X-axis is too small. origin %s X-Axis %s" origin.AsString xAxis.AsString
            if isTooSmall (ly) then  EuclidException.Raise "Euclid.PPlane.createOriginXaxisYaxis the Y-axis is too small. origin %s Y-Axis %s" origin.AsString yAxis.AsString
            let xf = 1./lx
            let yf = 1./ly
            let xu = UnitVec.createUnchecked(xAxis.X*xf, xAxis.Y*xf, xAxis.Z*xf)
            let yu = UnitVec.createUnchecked(yAxis.X*yf, yAxis.Y*yf, yAxis.Z*yf)
            PPlane.createOriginXaxisYaxis (origin, xu, yu)

        /// Creates a Parametrized Plane from a point and unit-vector representing the normal (or Z-axis).
        /// The X-axis will be found by taking the cross product of the World Z-axis and the given normal (or Z-axis).
        /// This will make the X-axis horizontal.
        /// If this fails because they are coincident, the cross product of the World Y-axis and the given normal (or Z-axis) will be used.
        static member createOriginNormal (origin:Pnt, normal:UnitVec) =
            if normal.IsParallelTo(UnitVec.Zaxis, Cosine.``0.5``) then
                let y = Vec.cross (normal,Vec.Xaxis)
                let x = Vec.cross (y, normal)
                PPlane.createUnchecked(origin, x.Unitized, y.Unitized, normal)
            else
                let x = Vec.cross (Vec.Zaxis, normal)
                let y = Vec.cross (normal, x)
                PPlane.createUnchecked(origin, x.Unitized, y.Unitized, normal)

        /// Creates a Parametrized Plane from a point and vector representing the normal (or Z-axis).
        /// The X-axis will be found by taking the cross product of the World Z-axis and the given normal (or Z-axis).
        /// This will make the X-axis horizontal.
        /// If this fails because they are coincident, the cross product of the World Y-axis and the given normal (or Z-axis) will be used.
        /// Fails if the vectors are shorter than 1e-5.
        static member createOriginNormal (origin:Pnt, normal:Vec) =
            let len = normal.Length
            if isTooSmall (len) then  EuclidException.Raise "Euclid.PPlane.createOriginNormal the Z-axis is too small. origin %s Z-Axis %s" origin.AsString normal.AsString
            let f = 1./len
            PPlane.createOriginNormal (origin, UnitVec.createUnchecked(normal.X*f, normal.Y*f, normal.Z*f))

        /// Creates a Parametrized Plane from a point and unit-vector representing the Z-axis.
        /// The given X vector does not need to be perpendicular to the normal vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-5 or normal and xAxis are parallel.
        static member createOriginNormalXaxis (origin:Pnt, normal:UnitVec, xAxis:UnitVec) =
            if normal.IsParallelTo(xAxis, Cosine.``1.0``) then
                EuclidException.Raise "Euclid.PPlane.createOriginNormalXaxis failed. The vectors are colinear by less than 1.0 degrees, origin %s and normal %s and normal %s" origin.AsString normal.AsString xAxis.AsString
            let y = UnitVec.cross (normal, xAxis)
            let x = Vec.cross (y, normal)
            PPlane.createUnchecked(origin, x.Unitized, y.Unitized, normal)

        /// Creates a Parametrized Plane from a point and unit-vector representing the Z-axis.
        /// The given X vector does not need to be perpendicular to the normal vector, just not parallel.
        /// Fails if the vectors are shorter than 1e-5 or normal and X are parallel.
        static member createOriginNormalXaxis (origin:Pnt, normal:Vec, xAxis:Vec) =
            let lx = xAxis.Length
            let ln = normal.Length
            if isTooSmall (lx) then  EuclidException.Raise "Euclid.PPlane.createOriginNormalXaxis the X-axis is too small. origin %s X-Axis %s" origin.AsString xAxis.AsString
            if isTooSmall (ln) then  EuclidException.Raise "Euclid.PPlane.createOriginNormalXaxis the normal is too small. origin %s Normal %s" origin.AsString normal.AsString
            let xf = 1./lx
            let nf = 1./ln
            let xu = UnitVec.createUnchecked(xAxis.X *xf,  xAxis.Y*xf,  xAxis.Z*xf)
            let nu = UnitVec.createUnchecked(normal.X*nf, normal.Y*nf, normal.Z*nf)
            PPlane.createOriginNormalXaxis (origin, nu, xu)


        /// Returns a new plane with given Origin.
        static member inline setOrigin (pt:Pnt) (pl:PPlane) =
            PPlane.createUnchecked(pt, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Returns a new plane with given Origin X value changed.
        static member inline setOriginX (x:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin |> Pnt.withX x, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Returns a new plane with given Origin Y value changed.
        static member inline setOriginY (y:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin |> Pnt.withY y, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Returns a new plane with given Origin Z value changed.
        static member inline setOriginZ (z:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin |> Pnt.withZ z, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Returns a new plane with Origin translated by Vec.
        static member inline translateBy (v:Vec) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin + v, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Returns a new plane with Origin translated in World X direction.
        static member inline translateByWorldX (x:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin |> Pnt.moveX x, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Returns a new plane with Origin translated in World Y direction.
        static member inline translateByWorldY (y:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin |> Pnt.moveY y, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Returns a new plane with Origin translated in World Z direction.
        static member inline translateByWorldZ (z:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin |> Pnt.moveZ z, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Rotate about Z-axis of the Plane by angle in degree.
        /// Counter-Clockwise in top view (for WorldXY Plane).
        static member inline rotateZ (angDegree:float) (pl:PPlane) =
            let m = RigidMatrix.createRotationAxisCenter (pl.Zaxis, pl.Origin, angDegree)
            let x = UnitVec.transformRigid m pl.Xaxis
            let y = UnitVec.transformRigid m pl.Yaxis
            PPlane.createUnchecked(pl.Origin, x, y, pl.Zaxis)

        /// Move Plane along the local X-axis by the given distance.
        static member inline translateX (d:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin + pl.Xaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Move Plane along the local Y-axis by the given distance.
        static member inline translateY (d:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin + pl.Yaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Move Plane along the local Z-axis by the given distance.
        /// Same as PPlane.offset.
        static member inline translateZ (d:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin + pl.Zaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Move Plane along the local Z-axis by the given distance.
        /// Same as PPlane.translateZ.
        static member inline offset (d:float) (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin + pl.Zaxis*d, pl.Xaxis, pl.Yaxis, pl.Zaxis)

        /// Rotate the Plane 180 degrees on its Y-axis.
        /// Called flip because Z-axis points in the opposite direction.
        static member inline flipOnY (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin, -pl.Xaxis, pl.Yaxis, -pl.Zaxis)

        /// Rotate the Plane 180 degrees on its X-axis.
        /// Called flip because Z-axis points in the opposite direction.
        static member inline flipOnX (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin, pl.Xaxis, -pl.Yaxis, -pl.Zaxis)

        /// Rotate the Plane 180 degrees on its Z-axis.
        static member inline rotateOnZ180 (pl:PPlane) =
            PPlane.createUnchecked(pl.Origin, -pl.Xaxis, -pl.Yaxis, pl.Zaxis)

        /// Transforms the plane by the given RigidMatrix.
        /// The returned PPlane has orthogonal unit-vectors.
        static member transform (m:RigidMatrix) (pl:PPlane) =
            let o = Pnt.transformRigid m pl.Origin
            let x = UnitVec.transformRigid m pl.Xaxis
            let y = UnitVec.transformRigid m pl.Yaxis
            let z = UnitVec.transformRigid m pl.Zaxis
            PPlane.createUnchecked (o, x, y, z)

        /// Rotate Plane 180 Degrees around Z-axis if the Y-axis orientation does not match World Y (pl.Yax.Y < 0.0)
        /// To ensure that Y is always positive. For example for showing Text.
        static member inline rotateZ180IfYNegative (pl:PPlane) =
            if pl.Yaxis.Y < 0.0 then PPlane.rotateOnZ180 pl else pl


