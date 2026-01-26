namespace Euclid

open System
open UtilEuclid
open EuclidErrors

/// Provides operations on 2D Triangles.
type  Tria3D =

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
    /// So it also returns true if the points are equal or very close to each other.
    /// Returns false for NaN input values.
    /// Use Points.areInLine if you need better control over the actual tolerance distance.
    static member inline isLinearFast (a:Pnt, b:Pnt, c:Pnt, [<OPT;DEF(0.001)>] maxAreaParallelogram:float) =
        let doubleArea = Tria3D.areaDouble(a, b, c)
        doubleArea < maxAreaParallelogram


    /// Checks if three points are in one line.
    /// By finding the biggest angle in the triangle.
    /// And then measuring the distance from this point to the line defined by the other two points.
    static member isLinear (a:Pnt, b:Pnt, c:Pnt, [<OPT;DEF(1e-6)>] distanceTolerance:float) =
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
        // previously:
        // Fails if any of the points are closer than the given distance tolerance to each other.
        // if not(distSq < abLenSq) then EuclidExcePntion.Raisef "Points.areInLine failed on very short line %O to %O " a b
        // if not(distSq < bcLenSq) then EuclidExcePntion.Raisef "Points.areInLine failed on very short line %O to %O " b c
        // if not(distSq < caLenSq) then EuclidExcePntion.Raisef "Points.areInLine failed on very short line %O to %O " c a
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
    static member inline private offsetPntCore (pt:Pnt) (dist:float) (nPrev:UnitVec) (nNext:UnitVec) (cosine:float) =
        // based on https://www.angusj.com/clipper2/Docs/Trigonometry.htm
        pt + (nPrev + nNext) * (dist / (1.0 + cosine)) // offset point


    /// Returns the offset point based on the previous and next normals, the distance and the precomputed cosine (= dot product of nPrev * nNext).
    /// Checks for 180 degrees U-turns. sets it to 179 degrees
    static member inline private offsetPntCoreSafe (pt:Pnt) (dist:float) (nPrev:UnitVec) (nNext:UnitVec) (perp:Vec) (cosine:float) =
        if withMeasure cosine < Cosine.``179.0`` then // check for 180 degrees U-turns, (0.0 degrees are handled fine)
            let dirPrev = Vec.cross (nPrev, perp) |> Vec.withLength 28.645 //tangent function of 89 degrees * 0.5 (90+89=179 degrees)
            let dirNext = Vec.cross (perp, nNext) |> Vec.withLength 28.645 //tangent function of 89 degrees * 0.5 (90+89=179 degrees)
            pt + dirPrev + dirNext
        else
            Tria3D.offsetPntCore pt dist nPrev nNext cosine


    /// Offsets one point by a given distance.
    /// If the points 'prev', 'this' and 'next' are in counter-clockwise order, the offset is inwards.
    /// Otherwise it is outwards.
    /// A negative offset distance inverts the direction.
    static member offsetPnt(pntToOffset:Pnt, prev:Pnt, next:Pnt, dist:float) =
        let vPrev = pntToOffset - prev
        let vNext = next - pntToOffset
        let perp = Vec.cross (vPrev, vNext)
        let lenSq = perp.LengthSq
        if isTooTinySq lenSq then
            failColinear $"Tria3D.offsetPnt" pntToOffset prev next

        let nPrev = Vec.cross(perp, vPrev) |> Vec.unitize//Unchecked
        let nNext = Vec.cross(perp, vNext) |> Vec.unitize//Unchecked
        let mutable cosine = nPrev *** nNext
        if withMeasure cosine < Cosine.``179.0`` then // check for 180 degrees U-turns, (0.0 degrees are handled fine)
            let dirPrev = vPrev |> Vec.withLength  28.645 //tangent function of 89 degrees * 0.5 (90+89=179 degrees)
            let dirNext = vNext |> Vec.withLength -28.645 //inverted here !
            pntToOffset + dirPrev + dirNext
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
            failColinear $"Tria3D.offset" a b c
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


    /// Calculates the intersection of a finite line with a triangle.
    /// Returns Some(Pnt) or None if no intersection was found,
    /// or if the input line has near zero length,
    /// or if the input triangle has near zero area.
    /// This algorithm still returns an intersection even if line and triangle are almost parallel.
    /// Since it is using the triple product, it is hard to find an appropriate tolerance for
    /// considering lines and triangles parallel based on the volume of the tetrahedron between them.
    static member intersectLine(line:Line3D, p1 :Pnt, p2:Pnt, p3:Pnt) : Pnt option =
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
                if isTooTinySq(abs div) then
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



[<Obsolete("Use the module Euclid.Line3D or Euclid.XLine3D instead.")>]
module Intersect =

    [<Obsolete("Use Tria3D.intersectLine instead.")>]
    let lineTriangle(line:Line3D, p1 :Pnt, p2:Pnt, p3:Pnt) : Pnt option =
       Tria3D.intersectLine(line, p1, p2, p3)

    [<Obsolete("Use Line3D.intersectCone or XLine3D.intersectCone for a more detailed result instead.")>]
    let lineCone (ln:Line3D, coneRadius, coneBaseZ, coneTipZ) : option<float*float> =
        Line3D.intersectCone(ln, coneRadius, coneBaseZ, coneTipZ)