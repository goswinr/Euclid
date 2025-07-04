namespace Euclid


/// The result of a line-cone intersection test.
/// This is the return type of the function Intersection.lineCone.
[<RequireQualifiedAccess>]
type XLineCone =
    | NoIntersection
    | Tangential
    | Touching of float
    | Intersecting of float*float


module Intersect =
    open UtilEuclid

    /// A Discriminated Union with the result cases
    /// from the first step of a 2D line-line intersection test.
    type LineLineRelation =
        // TODO: this DU could also be encoded via Float NaN and infinity to avoid an extra object allocation (using ref out parameters?)
        |NoIntersection
        |Colinear // within threshold, might still not overlap, needs to be checked via BRect
        |Parallel // more than threshold apart
        |BfromRight of struct (float * float) // parameters for unit-vector, might be out of bounds by snapThreshold
        |BfromLeft  of struct (float * float) // parameters for unit-vector, might be out of bounds by snapThreshold

    /// Returns the parameter on vector 'va' where 'va' and 'vb' intersect as endless rays.
    /// If they start from points 'a' and 'b' respectively.
    /// Pass in va.Cross vb as precomputed and inverted.
    let inline private getXPara (a:Pt, vaXvbInverse:float, b:Pt, vb:UnitVc) :float =
        // find intersection using 2D Cross Product:
        // https://www.youtube.com/watch?v=c065KoXooSw and https://gist.github.com/EgoMoose/f3d22a503e0598c12d1e3926dc72fb19
        ((b-a).Cross vb) * vaXvbInverse // va.Cross vb is precomputed and inverted

    let inline private isParamStillBelowZeroAfterOffsets(ap:Pt, au:UnitVc, aXbInverse:float, bp:Pt, bu:UnitVc, snapThreshold:float) :bool =
        let n = au.Rotate90CCW * snapThreshold
        // TODO would it be enough to only compute one of these two? depending on the sign of aXbInverse ?
        getXPara(ap + n, aXbInverse, bp, bu) <  -snapThreshold //with threshold subtracted the  range factor is 1 to 1.4 . without 0.7 to 1 of threshold
        &&
        getXPara(ap - n, aXbInverse, bp, bu) <  -snapThreshold

    let inline private isParamStillMoreThanLengthAfterOffsets(ap:Pt, au:UnitVc, aXbInverse:float, al:float, bp:Pt, bu:UnitVc, snapThreshold:float) :bool =
        let n = au.Rotate90CCW * snapThreshold
        getXPara(ap + n, aXbInverse, bp, bu) > al + snapThreshold //with threshold added the range factor is 1 to 1.4 . without 0.7 to 1 of threshold
        &&
        getXPara(ap - n, aXbInverse, bp, bu) > al + snapThreshold



    /// A call to this should be preceded by BRect.doOverlap to exit quickly if apart.
    /// For line A and line B, give for each:
    /// Start point, unitized direction, line length.
    /// And finally a tolerance: Curve A will be extended on both ends and offset to both sides.
    /// These offsets will also be checked with curve B that is also extended by this amount.
    let internal getRelation (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) : LineLineRelation =
        let aXb = au.Cross bu // precomputed Cross Product

        if abs(aXb) > zeroLengthTolerance then  // not parallel
            let aXbInverse = 1./aXb // invert only once, then pass it on as inverted value
            let ta = getXPara (ap, aXbInverse, bp, bu)

            // parameter on first is below zero, so probably no intersection  unless closer than snapThreshold and almost colinear
            if ta < -snapThreshold && isParamStillBelowZeroAfterOffsets (ap, au, aXbInverse, bp, bu, snapThreshold) then
                NoIntersection // no need to even check parameter on second segment

            // parameter on first segment is  beyond length, so probably no intersection  unless closer than snapThreshold and colinear
            elif ta > al+snapThreshold && isParamStillMoreThanLengthAfterOffsets(ap, au, aXbInverse, al, bp, bu, snapThreshold) then
                NoIntersection // no need to even check parameter on second segment

            // now checking if parameter on second line is inside too:
            else
                // this might still be a very shallow intersection that counts as parallel
                let bXaInverse = -aXbInverse
                let tb = getXPara (bp, bXaInverse, ap, au)

                // parameter on second segment is  below zero, so probably no intersection  unless closer than snapThreshold and colinear
                if tb < -snapThreshold && isParamStillBelowZeroAfterOffsets (bp, bu, bXaInverse, ap, au, snapThreshold) then
                    NoIntersection

                // parameter on second segment is  beyond length, so probably false unless closer than snapThreshold and colinear
                elif tb > bl + snapThreshold && isParamStillMoreThanLengthAfterOffsets (bp, bu, bXaInverse, bl, ap, au, snapThreshold) then
                    NoIntersection
                else
                    if aXb > 0.0 then BfromRight (ta, tb) // TODO might still be almost colinear. was an intersection very far outside bounding Rectangles.
                    else              BfromLeft  (ta, tb) // TODO could to be almost colinear too, check offset  !!

        else // Colinear
            // probably no intersection  unless closer than snapThreshold
            let perp = au.Rotate90CCW // unit v
            let vab = ap-bp
            let dot = perp *** vab // project vab onto unit-vector
            if abs dot < snapThreshold then
                Colinear // parallel distance is less than snapThreshold distance, TODO but actual overlap needs to be confirmed via BRect
            else
                Parallel // parallel distance is more than snapThreshold distance,

    /// This function includes an initial call to BRect.doOverlap.
    let inline internal doIntersectOrOverlapColinear (ap:Pt, au:UnitVc, al:float, abb:BRect, bp:Pt, bu:UnitVc, bl:float, bbb:BRect, snapThreshold:float) : bool =
        BRect.doOverlap abb bbb
        &&
        match getRelation(ap, au, al, bp, bu, bl, snapThreshold)   with
        |NoIntersection -> false
        |Parallel       -> false
        |BfromLeft _    -> true
        |BfromRight _   -> true
        |Colinear       -> true


    /// Returns the intersection point or midpoint between two 2D lines.
    /// (Used mainly for drawing debug notes at this point)
    let internal getXPointOrMid (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) : Pt =
        match getRelation(ap, au, al, bp, bu, bl, snapThreshold)   with
        |NoIntersection
        |Colinear
        |Parallel            -> (ap + ap + bp + bp + au*al + bu*bl) * 0.25
        |BfromLeft  (ta, _) ->  ap + au * ta // clamp point to actually be on line even if it is not quite in case of PreStart or PostEnd
        |BfromRight (ta, _) ->  ap + au * ta



    /// Calculates the intersection of a finite line with a triangle.
    /// Returns Some(Pnt) or None if no intersection was found,
    /// or if the input line has near zero length,
    /// or if the input triangle has near zero area.
    /// This algorithm still returns an intersection even if line and triangle are almost parallel.
    /// Since it is using the triple product, it is hard to find an appropriate tolerance for
    /// considering lines and triangles parallel based on the volume of the tetrahedron between them.
    let lineTriangle(line:Line3D, p1 :Pnt, p2:Pnt, p3:Pnt) : Pnt option =
        // https://stackoverflow.com/questions/42740765/intersection-between-line-and-triangle-in-3d
        let inline tetrahedronVolumeSigned(a:Pnt, b:Pnt, c:Pnt, d:Pnt) : float=
            // computes the signed Volume of a Tetrahedron
            //((Vec.cross(b-a, c-a)) * (d-a)) / 6.0 // the actual volume of a Tetrahedron
            Vec.cross(b-a, c-a) *** (d-a) // divide by 6.0 is not needed, because we only need the sign of the result

        let inline sign (x:float) =
            if   x = 0 then 0
            elif x > 0 then 1
            else           -1

        let q1 = line.From
        let q2 = line.To
        let s1 = sign (tetrahedronVolumeSigned(q1, p1, p2, p3))
        let s2 = sign (tetrahedronVolumeSigned(q2, p1, p2, p3))
        // if line and triangle are exactly in the same plane s1 and s2 are both 0
        // It is hard to say at which very small volume it should be considers flat.
        // TODO add a tolerance parameter for tangential triangles and lines
        if s1 = s2 then
            None
        else
            let s3 = sign (tetrahedronVolumeSigned(q1, q2, p1, p2))
            let s4 = sign (tetrahedronVolumeSigned(q1, q2, p2, p3))
            let s5 = sign (tetrahedronVolumeSigned(q1, q2, p3, p1))
            if s3 = s4 && s4 = s5 then
                let n = Vec.cross(p2-p1, p3-p1)
                let v = q2-q1
                let div = v *** n
                if isTooTinySq(abs(div)) then
                    None
                else
                    let t = ((p1-q1) *** n) / div
                    // this extra check should not be needed,
                    // but probably helps to deal with potential numerical precision issues:
                    if isBetweenZeroAndOne t then
                        Some (q1 + v * t)
                    else
                        None
            else None

    /// Intersects an infinite line with an infinite double cone that has its axis on the Z-axis.
    /// coneRadius -> coneBaseZ -> coneTipZ -> (ln:Line3D) -> XConeLine
    /// Returns the parameter(s) on the line.
    let lineCone (ln:Line3D, coneRadius, coneBaseZ, coneTipZ) : XLineCone =
        let h = coneBaseZ-coneTipZ
        if isTooTiny( abs h )then EuclidException.Raisef "Euclid.Intersection.lineCone: cone has zero height: coneRadius: %g, coneBaseZ: %g, coneTipZ: %g" coneRadius coneBaseZ coneTipZ
        let lam = coneRadius / h
        let lam = lam * lam
        let v = ln.Tangent
        let f2 = lam*v.Z*v.Z - v.X*v.X - v.Y*v.Y
        if isTooTiny(abs f2) then
            XLineCone.Tangential
        else
            let f1 = 2.*lam*ln.FromZ*v.Z - 2.*lam*v.Z*coneTipZ - 2.*v.Y*ln.FromY - 2.*ln.FromX*v.X
            let f0 = lam*ln.FromZ*ln.FromZ + lam*coneTipZ*coneTipZ - 2.*ln.FromZ*coneTipZ*lam - ln.FromY*ln.FromY - ln.FromX*ln.FromX
            let part = f1**2. - 4.* f2 * f0
            if part < 0.0 then
                XLineCone.NoIntersection
            else
                let sqrtPart = sqrt(part)
                let div = 1. / (2. * f2)
                let u = (-f1 + sqrtPart) * div
                let v = (-f1 - sqrtPart) * div
                if isTooTiny(abs(u-v)) then
                    XLineCone.Touching ((u+v)*0.5)
                else
                    XLineCone.Intersecting (u, v)

