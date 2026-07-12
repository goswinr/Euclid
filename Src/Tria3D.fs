namespace Euclid

open System
open UtilEuclid
open EuclidErrors

/// A type containing only static member functions for operations on 3D Triangles.
type Tria3D =

    /// Returns the double area of a triangle.
    /// This is the fastest way to get a comparison or sorting value for the areas of triangles.
    /// This is just the length of the cross product vector.
    static member inline areaDouble (a:Pnt, b:Pnt, c:Pnt) :float =
        let v = b - a
        let w = c - a
        Vec.cross (v, w) |> Vec.length

    /// Returns the area of a triangle described by 3 points.
    static member inline area (a:Pnt, b:Pnt, c:Pnt) : float =
        Tria3D.areaDouble(a, b, c) *  0.5


    /// Checks if three points are in one line.
    /// This is a very fast check, but it is hard to find an appropriate tolerance. (Default is 0.001)
    /// This tolerance is the area of the parallelogram described by two vectors created from the 3 points.
    /// So it also returns TRUE if the points are equal or very close to each other.
    /// Returns FALSE for NaN input values.
    /// Use Points.areInLine if you need better control over the actual tolerance distance.
    static member inline isLinearFast (a:Pnt, b:Pnt, c:Pnt, [<OPT;DEF(0.001)>] maxAreaParallelogram:float) : bool =
        let doubleArea = Tria3D.areaDouble(a, b, c)
        doubleArea < maxAreaParallelogram


    /// Checks if three points are in one line.
    /// By finding the biggest angle in the triangle.
    /// And then measuring the distance from this point to the line defined by the other two points.
    static member isLinear (a:Pnt, b:Pnt, c:Pnt, [<OPT;DEF(1e-6)>] distanceTolerance:float) : bool =
        let inline distanceSqLineToPntInfinite(lnFrom:Pnt, lnTo:Pnt, p:Pnt,  lnSqLen:float) =
            // rewritten from Line3D.distanceLineToPntInfinite
            // http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
            let x = lnFrom.X - lnTo.X
            let y = lnFrom.Y - lnTo.Y
            let z = lnFrom.Z - lnTo.Z
            let u = lnFrom.X - p.X
            let v = lnFrom.Y - p.Y
            let w = lnFrom.Z - p.Z
            let dot = x*u + y*v + z*w
            let t = dot/lnSqLen
            let x' = lnFrom.X - x * t
            let y' = lnFrom.Y - y * t
            let z' = lnFrom.Z - z * t
            let u' = x' - p.X
            let v' = y' - p.Y
            let w' = z' - p.Z
            u'*u' + v'*v' + w'*w'

        let ab = b-a
        let bc = c-b
        let ca = a-c
        let abLenSq = ab.LengthSq
        let bcLenSq = bc.LengthSq
        let caLenSq = ca.LengthSq
        let distSq = distanceTolerance*distanceTolerance
        if abLenSq < distSq || bcLenSq < distSq || caLenSq < distSq then // two or more points are too close to each other
            true
        else
            let dotA = ab *** ca
            let dotB = bc *** ab
            let dotC = ca *** bc
            // the corner with the biggest dot product is the one with the biggest angle,
            // so the one with the closest distance to the opposite line in this triangle
            if dotA > dotB && dotA > dotC then
                distanceSqLineToPntInfinite(b, c, a, bcLenSq) < distSq
            elif dotB > dotA && dotB > dotC then
                distanceSqLineToPntInfinite(c, a, b, caLenSq) < distSq
            else
                distanceSqLineToPntInfinite(a, b, c, abLenSq) < distSq


    /// Returns the offset point based on the previous and next normals, the distance and the precomputed cosine (= dot product of nPrev * nNext).
    /// Returns Infinity if the cosine is -1.0 (normals in opposite directions at a 180-degree U-turn).
    static member inline private offsetPntCore (pt:Pnt) (dist:float) (nPrev:UnitVec) (nNext:UnitVec) (cosine:float) : Pnt =
        // based on https://www.angusj.com/clipper2/Docs/Trigonometry.htm
        pt + (nPrev + nNext) * (dist / (1.0 + cosine)) // offset point


    /// Returns the capped offset point for a near-180-degree U-turn corner, in the triangle plane around 'perp'.
    /// The exact miter would shoot towards infinity, so the corner is capped at the 179-degree
    /// threshold. The result is scaled by 'dist' and lies on the same side as 'offsetPntCore'.
    /// This is the 3D analog of Tria2D.offsetPtUTurn ('perp' need not be unit length).
    static member inline private offsetPntUTurn (pt:Pnt) (dist:float) (nPrev:UnitVec) (nNext:UnitVec) (perp:Vec) : Pnt =
        let cosHalf = sqrt ((1.0 + float Cosine.``179.0``) / 2.0) // cosine of half the threshold angle = cosine of 89.5 degrees
        let f = 0.5 * dist / cosHalf // hypotenuse = adjacent / cos(angle); *0.5 because the summed direction length is almost 2.0
        let dirPrev = Vec.cross (perp, nPrev) |> Vec.withLength f // 90-degree rotation of nPrev in the triangle plane
        let dirNext = Vec.cross (nNext, perp) |> Vec.withLength f // 90-degree rotation of nNext in the triangle plane
        pt + dirPrev + dirNext


    /// Returns the offset point based on the previous and next normals, the distance and the precomputed cosine (= dot product of nPrev * nNext).
    /// Checks for 180 degrees U-turns and caps them at 179 degrees.
    static member inline private offsetPntCoreSafe (pt:Pnt) (dist:float) (nPrev:UnitVec) (nNext:UnitVec) (perp:Vec) (cosine:float) : Pnt =
        if withMeasure cosine < Cosine.``179.0`` then // check for 180 degrees U-turns, (0.0 degrees are handled fine)
            Tria3D.offsetPntUTurn pt dist nPrev nNext perp
        else
            Tria3D.offsetPntCore pt dist nPrev nNext cosine


    /// Offsets one point by a given distance.
    /// If the points 'prev', 'this' and 'next' are in counter-clockwise order, the offset is inwards.
    /// Otherwise it is outwards.
    /// A negative offset distance inverts the direction.
    static member offsetPnt(pntToOffset:Pnt, prev:Pnt, next:Pnt, dist:float) : Pnt =
        let vPrev = pntToOffset - prev
        let vNext = next - pntToOffset
        let perp = Vec.cross (vPrev, vNext)
        let lenSq = perp.LengthSq
        if isTooTinySq lenSq then
            failCollinear $"Tria3D.offsetPnt" pntToOffset prev next

        let nPrev = Vec.cross(perp, vPrev) |> Vec.unitize//Unchecked
        let nNext = Vec.cross(perp, vNext) |> Vec.unitize//Unchecked
        let cosine = nPrev *** nNext
        if withMeasure cosine < Cosine.``179.0`` then // check for 180 degrees U-turns, (0.0 degrees are handled fine)
            Tria3D.offsetPntUTurn pntToOffset dist nPrev nNext perp
        else
            Tria3D.offsetPntCore pntToOffset dist nPrev nNext cosine


    /// Offsets all points by a given distance.
    /// The offset is inwards for positive distance and outwards for negative distance.
    static member offset(a:Pnt, b:Pnt, c:Pnt, dist:float) : Pnt*Pnt*Pnt =
        let vAB = b-a
        let vBC = c-b
        let vCA = a-c
        let perp = Vec.cross(vAB, vBC)
        if isTooSmallSq perp.LengthSq  then
            failCollinear $"Tria3D.offset" a b c
        let na = Vec.cross(perp,vAB) |> Vec.unitize
        let nb = Vec.cross(perp,vBC) |> Vec.unitize
        let nc = Vec.cross(perp,vCA) |> Vec.unitize
        let cosineA = na *** nc
        let cosineB = na *** nb
        let cosineC = nb *** nc
        (Tria3D.offsetPntCoreSafe a dist na nc perp cosineA,
         Tria3D.offsetPntCoreSafe b dist na nb perp cosineB,
         Tria3D.offsetPntCoreSafe c dist nb nc perp cosineC)


    /// Finds the offset point based on
    /// the previous and next unit normals (= offset direction),
    /// and their offset distances.
    static member offsetVar (ptToOffset:Pnt, prev:Pnt, next:Pnt, distPrev:float, distNext:float)  : ValueOption<Pnt> =
        let vPrev = ptToOffset - prev
        let vNext = next - ptToOffset
        let perp = Vec.cross (vPrev, vNext)
        let lenPerpSq = perp.LengthSq
        if isTooTinySq lenPerpSq then
            // test if lines are long enough but just parallel
            //if isTooSmallSq vPrev.LengthSq || isTooSmallSq vNext.LengthSq then
            // EuclidException.Raisef "Tria3D.offsetVar: Points are too close or collinear: %O, %O, %O" ptToOffset prev next
            ValueNone
        else
            // shadow the original values:
            let vPrevU = vPrev |> Vec.unitize
            let vNextU = vNext |> Vec.unitize
            let perpU  = perp * (1.0 / sqrt lenPerpSq) // normalize the perp vector
            let nPrevU = Vec.cross(perpU, vPrevU) // they are now unit vectors too
            let nNextU = Vec.cross(perpU, vNextU) // they are now unit vectors too
            let ax = vPrevU.X
            let ay = vPrevU.Y
            let az = vPrevU.Z
            let bx = vNextU.X
            let by = vNextU.Y
            let bz = vNextU.Z
            // let a = ax*ax + ay*ay + az*az // square length of A // always 1.0
            // let c = bx*bx + by*by + bz*bz // square length of B // always 1.0
            let lnAFrom = ptToOffset + nPrevU * distPrev
            let lnBFrom = ptToOffset + nNextU * distNext

            // do line-line intersection:
            let b = ax*bx + ay*by + az*bz // dot product of both lines
            let vx = lnBFrom.X - lnAFrom.X
            let vy = lnBFrom.Y - lnAFrom.Y
            let vz = lnBFrom.Z - lnAFrom.Z
            //let ac = a * c // square of square length, never negative// always 1.0
            let bb = b * b // never negative
            let discriminant = 1.0 - bb //ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
            let e = bx*vx + by*vy + bz*vz
            let d = ax*vx + ay*vy + az*vz
            let tPrev = (b * e - d) / discriminant
            let pt = lnAFrom + nPrevU * tPrev // the offset point
            ValueSome pt


    /// Finds the intersection parameter on the infinite Line3D with a 3D triangle.
    /// Returns ValueNone if
    ///  - any of the direction vectors are too small,
    ///  - the line is parallel to the triangle plane,
    ///  - the intersection point is outside the triangle.
    /// lnFromX, lnFromY, lnFromZ are the start point of the line
    /// dirX, dirY, dirZ are the direction vector of the line (not the end point)
    /// ptAx, ptAy, ptAz is the triangle points
    /// e1x, e1y, e1z is the triangle edge1 vector (B-A)
    /// e2x, e2y, e2z is the triangle edge2 vector (C-A)
    static member intersectTriaVectorsWithRay(  lnFromX:float, lnFromY:float, lnFromZ:float,
                                    dirX:float, dirY:float, dirZ:float,
                                    ptAx:float, ptAy:float, ptAz:float,
                                    e1x:float, e1y:float, e1z:float,
                                    e2x:float, e2y:float, e2z:float) : voption<float> =
        // Möller–Trumbore implementation, two-sided (no backface culling) and allocation-free:
        // pVec = cross product dir × edge2
        let px = dirY*e2z - dirZ*e2y
        let py = dirZ*e2x - dirX*e2z
        let pz = dirX*e2y - dirY*e2x
        // det = edge1 · pVec  (== -dir · triangleNormal); ~0 means line parallel to triangle plane
        let det = e1x*px + e1y*py + e1z*pz


        // a degenerate (zero-area) triangle produces different results depending on which corner was duplicated,
        // because the only degeneracy guard was the absolute test abs det > 1e-12:

        // // Degeneracy guard: reject zero-area triangles (e.g. a coincident corner, so one edge is zero
        // // or both edges are equal) up front. The absolute `abs det > 1e-12` test alone is unreliable and
        // // inconsistent for these cases: when an edge is exactly zero det comes out exactly 0 (rejected),
        // // but when the two edges are equal det is formed by cancellation and rounds to garbage ~1e-9 that
        // // sails past 1e-12 (wrongly accepted). Testing the triangle normal (= edge1 × edge2 = 2·area)
        // // relative to the edge lengths is scale-independent and is exactly 0 in both degenerate cases.
        // let nx = e1y*e2z - e1z*e2y   // triangle normal = edge1 × edge2
        // let ny = e1z*e2x - e1x*e2z
        // let nz = e1x*e2y - e1y*e2x
        // let nLenSq = nx*nx + ny*ny + nz*nz                 // (2 × area)²
        // let e1Sq = e1x*e1x + e1y*e1y + e1z*e1z
        // let e2Sq = e2x*e2x + e2y*e2y + e2z*e2z
        // // nLenSq / (e1Sq*e2Sq) = sin²(angle between edges); below 1e-12 the triangle is effectively a sliver/point
        // if nLenSq > 1e-12 * e1Sq * e2Sq && abs det > 1e-12 then // also catches zero length lines and rays parallel to the plane


        if abs det > 1e-12 then // this catches zero length lines or edges
            let invDet = 1.0 / det // det is not 0.0
            // tVec = lnFrom - A
            let tx = lnFromX - ptAx
            let ty = lnFromY - ptAy
            let tz = lnFromZ - ptAz
            // u = (tVec · pVec) * invDet
            let u = (tx*px + ty*py + tz*pz) * invDet
            if isBetweenZeroAndOneTolerantIncl u then // first barycentric bounds check — it tests whether the intersection point falls outside the triangle along the first edge direction
                // qVec = cross product tVec × edge1
                let qx = ty*e1z - tz*e1y
                let qy = tz*e1x - tx*e1z
                let qz = tx*e1y - ty*e1x
                // v = (dir · qVec) * invDet
                let v = (dirX*qx + dirY*qy + dirZ*qz) * invDet
                if v > -1e-6 && u + v <``1.0 + 1e-6`` then
                    // t = (edge2 · qVec) * invDet
                    let t = (e2x*qx + e2y*qy + e2z*qz) * invDet
                    ValueSome t
                else
                    ValueNone
            else
                ValueNone
        else
            ValueNone

    /// Calculates the intersection parameter on the infinite Ray / Line3D with a 3D triangle.
    /// Returns:
    /// ValueSome with the parameter on the line.
    /// ValueNone if the ray/line
    ///  - is parallel to the triangle plane,
    ///  - lies in the triangle's plane,
    ///  - passes outside the triangle,
    ///  - has zero length.
    ///  - a triangle edge has zero length.
    /// Note: A degenerated linear triangle that still intersects with the line may sometimes return Some or None depending on the order of input points.
    static member inline intersectRayXYZ(   lnFromX:float, lnFromY:float, lnFromZ:float,
                                             lnToX:float,   lnToY:float,   lnToZ:float,
                                             ptAx:float, ptAy:float, ptAz:float,
                                             ptBx:float, ptBy:float, ptBz:float,
                                             ptCx:float, ptCy:float, ptCz:float) : voption<float> =
        // Note a degenerated linear triangle that still intersects with the line
        // may sometimes return Some or None depending on the order of input points.
        // see .\Test\TestInRhino\xRayTria.fsx
        let dirX = lnToX - lnFromX
        let dirY = lnToY - lnFromY
        let dirZ = lnToZ - lnFromZ
        // edge1 = B - A,  edge2 = C - A
        let e1x = ptBx - ptAx
        let e1y = ptBy - ptAy
        let e1z = ptBz - ptAz
        let e2x = ptCx - ptAx
        let e2y = ptCy - ptAy
        let e2z = ptCz - ptAz
        Tria3D.intersectTriaVectorsWithRay(lnFromX, lnFromY, lnFromZ, dirX, dirY, dirZ, ptAx, ptAy, ptAz, e1x, e1y, e1z, e2x, e2y, e2z)

    /// Calculates the intersection parameter on the infinite Ray / Line3D with a 3D triangle.
    /// Returns:
    /// ValueSome with the parameter on the line.
    /// ValueNone if the ray /line
    ///  - is parallel to the triangle plane,
    ///  - lies in the triangle's plane,
    ///  - passes outside the triangle,
    ///  - has zero length.
    ///  - a triangle edge has zero length.
    static member inline intersectRay(line:Line3D, p1:Pnt, p2:Pnt, p3:Pnt) : voption<float> =
        Tria3D.intersectRayXYZ(line.FromX, line.FromY, line.FromZ, line.ToX, line.ToY, line.ToZ, p1.X, p1.Y, p1.Z, p2.X, p2.Y, p2.Z, p3.X, p3.Y, p3.Z)


    /// Calculates the intersection point on the finite Line3D with a 3D triangle.
    /// Returns:
    /// ValueSome with the intersection point.
    /// ValueNone if the finite line
    ///  - does not intersect the triangle,
    ///  - has zero length.
    ///  - a triangle edge has zero length.
    static member inline intersectLine(line:Line3D, p1:Pnt, p2:Pnt, p3:Pnt) : Pnt voption =
        match Tria3D.intersectRay(line, p1, p2, p3) with
        | ValueSome t when isBetweenZeroAndOneTolerantIncl t -> ValueSome (line.EvaluateAt t)
        | _ -> ValueNone
