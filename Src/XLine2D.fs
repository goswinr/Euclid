
namespace Euclid

open System
open UtilEuclid


/// A module containing the result types for 2D Line-Line-Intersections
/// and 2D Line-Line relationship queries.
module XLine2D =


    /// Describes the possible intersection parameters of two rays (rays are 2D lines extended infinitely in both directions).
    /// Returns parameters on both lines if they intersect.
    [<RequireQualifiedAccess>]
    type XRayParam =

        /// The rays (2d-lines extended infinitely) are intersecting.
        /// Contains the parameters on the first and second ray.
        /// These parameters can range from -infinity to +infinity.
        | Intersect of twoParams : float*float

        /// The lines are parallel or even coincident, within the given tolerance.
        | Parallel

        /// Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth


    /// Describes the possible intersection point of two rays (rays are 2D lines extended infinitely in both directions).
    /// Returns the intersection point if they intersect.
    [<NoEquality; NoComparison>]
    [<RequireQualifiedAccess>]
    type XRay =

        /// The rays (2d-lines extended infinitely) are intersecting in one well defined point.
        | Intersect of xPt : Pt

        /// The lines are parallel or even coincident, within the given tolerance.
        | Parallel

        /// Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth



    /// Describes the intersection parameters of two 2D lines.
    /// Returns the parameters in the range 0.0 to 1.0 on both lines if they intersect.
    [<NoEquality; NoComparison>]
    [<RequireQualifiedAccess>]
    type XParam =

        /// The finite lines are intersecting in one well defined point.
        /// Contains both parameters on both lines at the intersection point.
        /// These parameters are in the range 0.0 to 1.0
        | Intersect of twoParams : float*float

        /// The finite 2D lines do not touch each other.
        /// Their intersection point is outside of their finite definition.
        /// However they are not parallel. XParam.Parallel is used when they are parallel.
        | Apart

        /// The lines are parallel or even coincident, within the given tolerance.
        | Parallel

        /// Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth

    /// Describes the possible relationships of two finite 2D lines.
    /// Returns the intersection point if they intersect.
    [<NoEquality; NoComparison>]
    [<RequireQualifiedAccess>]
    type XPt =

        /// The finite lines are intersecting in one well defined point.
        | Intersect of xPt : Pt

        /// The finite 2D lines do not touch each other.
        /// Their intersection point is outside of their finite definition.
        /// However they are not parallel. XPt.Parallel is used when they are parallel.
        | Apart

        /// The lines are parallel or even coincident, within the given tolerance.
        | Parallel

        /// Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth



    /// Describes the possible cases of the closest parameters between finite 2D lines.
    [<RequireQualifiedAccess>]
    type ClParams =

        /// The lines intersect. Contains the parameters at the intersection point for both lines.
        /// These parameters are in the range 0.0 to 1.0
        | Intersect of paramA:float * paramB:float

        /// The lines are parallel or coincident. Contains the parameters at the middle of any overlapping segment.
        /// These parameters are in the range 0.0 to 1.0
        | Parallel of paramA:float * paramB:float

        /// The lines are apart. Contains the parameters and squared distance at their closest points.
        /// These parameters are in the range 0.0 to 1.0
        | Apart of paramA:float * paramB:float * squareDist:float

        /// Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth


    /// Describes the possible cases of the closest points between finite 2D lines.
    [<NoEquality; NoComparison>]
    [<RequireQualifiedAccess>]
    type ClPts =

        /// The lines intersect. Contains the intersection point.
        | Intersect of pt:Pt

        /// The lines are parallel or coincident. Contains the points at the middle of any overlapping segment on both lines.
        | Parallel of ptA:Pt * ptB:Pt

        /// The lines are apart. Contains the closest points on both lines and the squared distance between them.
        | Apart of ptA:Pt * ptB:Pt * squareDist:float

        /// Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth

    /// Describes the possible cases of two finite 2D lines touching at their ends.
    /// Does not check if they are parallel or intersecting.
    [<RequireQualifiedAccess>]
    type XEnds =

        /// Not touching.
        | NotTouching

        /// Touching at Start of A and Start of B.
        | StartA_StartB

        /// Touching at End of A and End of B.
        | EndA_EndB

        /// Touching at End of A and Start of B.
        | EndA_StartB

        /// Touching at Start of A and End of B.
        | StartA_EndB

        /// Touching at both Start and End. Lines are identical and in same orientation.
        | Identical

        /// Touching at both at the other Start or End. Lines are identical but in opposite orientation.
        | IdenticalFlipped



    /// squares a float value
    let inline internal sq (x:float) : float = x * x


    /// Returns the parameter of the closest point on a ray.
    /// May be NaN or infinity if the line vector is zero length or very short.
    let inline internal clParamRayPt(pAx:float, pAy:float, vAx:float, vAy:float, x:float, y:float) : float =
        // project point B onto line A and check distance:
        // vector from A to B
        let u = x - pAx
        let v = y - pAy
        // dot product of AB with A's vector:
        let dotV = vAx*u + vAy*v
        let lenSq = vAx*vAx + vAy*vAy
        // parameter t of the closest point on line A to point B
        dotV / lenSq



    /// Returns the parameter of the closest point on an finite line.
    /// The result is clamped to the range 0.0 to  1.0.
    /// If the line vector is zero length 0.0 is returned.
    let inline internal clParamLnPt (ln:Line2D, x:float, y:float) : float =
        clParamRayPt(ln.FromX, ln.FromY, ln.VectorX , ln.VectorY, x, y) |> clampBetweenZeroAndOne

    /// Returns the squared distance of a point to a ray.
    /// May be NaN or infinity if the line vector is zero length or very short.
    let inline internal sqRayPtDist(pAx:float, pAy:float, vAx:float, vAy:float, x:float, y:float) : float =
        let nx = -vAy // normal vector to line A
        let ny = vAx
        let lenSq = nx*nx + ny*ny
        // vector from A to B
        let u = x - pAx
        let v = y - pAy
        // dot product of AB with normal vector of A:
        let dotN = nx*u + ny*v
        // squared distance from point B to line A:
        (dotN * dotN) / lenSq

        // alternative implementation computing the closest point first:
        // like used in sqDistLnPt
        // // parameter t of the closest point on line A to point B
        // let t = clParamRayPt(pAx, pAy, vAx, vAy, x, y)
        // let clPtX = pAx + vAx * t
        // let clPtY = pAy + vAy * t
        // // vector from closest point to B
        // let vx = clPtX - x
        // let vy = clPtY - y
        // vx*vx + vy*vy


    /// Returns the closest point to a finite line.
    let inline internal clPtLn(pAx:float, pAy:float, vAx:float, vAy:float, x:float, y:float) : Pt =
        // parameter t of the closest point on line A to point B
        let t = clParamRayPt(pAx, pAy, vAx, vAy, x, y)
        if t > -1e-6 then // to handle NaN too
            if t < ``1.0 + 1e-6`` then
                Pt(pAx + vAx * t, pAy + vAy * t)
            else
                Pt(pAx + vAx , pAy + vAy )
        else // case x <= 0.0 or x = NaN
            Pt(pAx, pAy)


    /// Returns the closest point to a finite line.
    let inline internal clPtLn'(ln:Line2D, x:float, y:float) : Pt =
        clPtLn(ln.FromX, ln.FromY, ln.VectorX        , ln.VectorY        , x, y)


    /// Returns the squared distance of a point to a finite line.
    let inline internal sqDistLnPt(pAx:float, pAy:float, vAx:float, vAy:float, x:float, y:float) : float =
        // parameter t of the closest point on line A to point B
        let t = clParamRayPt(pAx, pAy, vAx, vAy, x, y)
        if t > -1e-6 then // to handle NaN too
            if t < ``1.0 + 1e-6`` then
                let clPtX = pAx + vAx * t
                let clPtY = pAy + vAy * t
                let vx = clPtX - x
                let vy = clPtY - y
                vx*vx + vy*vy
            else
                let clPtX = pAx + vAx
                let clPtY = pAy + vAy
                let vx = clPtX - x
                let vy = clPtY - y
                vx*vx + vy*vy
        else // case x <= 0.0 or x = NaN
            let vX = pAx - x
            let vY = pAy - y
            vX*vX + vY*vY


    /// Returns the squared distance of a point to a finite line.
    let inline internal sqDistLnPt'(ln:Line2D, x:float, y:float) : float =
        sqDistLnPt(ln.FromX, ln.FromY, ln.VectorX, ln.VectorY , x, y)

    /// Returns the squared distance between the start points of two finite lines.
    let inline internal sqDistLnFromLnFrom(a:Line2D, b:Line2D) : float =
        let dx = a.FromX - b.FromX
        let dy = a.FromY - b.FromY
        dx*dx + dy*dy

    /// Given the start and end points of two finite lines A and B.
    /// Returns the parameters of the closest end points between the two lines.
    let inline internal projectEndsBackAndForth (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): float*float*float*float =
        let uAs = clParamRayPt (pAx, pAy, vAx, vAy, pBx, pBy) |> clampBetweenZeroAndOne // parameter on line A of the projection of start point of line B
        let x = pAx + uAs * vAx // X coordinate of the projection of start point of line B on line A
        let y = pAy + uAs * vAy // Y coordinate of the projection of start point of line B on line A
        let uBs = clParamRayPt (pBx, pBy, vBx, vBy, x, y) |> clampBetweenZeroAndOne // parameter on line B of the projection of start point of line A

        // project the end point from line B onto line A and back to line B
        let eBx = pBx + vBx // end point of line B
        let eBy = pBy + vBy // end point of line B
        let uAe = clParamRayPt (pAx, pAy, vAx, vAy, eBx, eBy) |> clampBetweenZeroAndOne
        let xE = pAx + uAe * vAx // X coordinate of the projection of end point of line B on line A
        let yE = pAy + uAe * vAy // Y coordinate of the projection of end point of line B on line A
        let uBe = clParamRayPt (pBx, pBy, vBx, vBy, xE, yE) |> clampBetweenZeroAndOne
        uAs, uBs, uAe, uBe


    /// Only for apart Lines, not for parallel.
    /// Returns the distance of the closest points between two finite lines A and B that are not intersecting nor parallel
    let inline internal sqDistance (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): float =
        let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy)
        // get point params at these parameters:
        let clAsX = pAx + uAs * vAx
        let clAsY = pAy + uAs * vAy
        let clBsX = pBx + uBs * vBx
        let clBsY = pBy + uBs * vBy
        let distSqStart = sq (clAsX - clBsX) + sq (clAsY - clBsY)
        let clAeX = pAx + uAe * vAx
        let clAeY = pAy + uAe * vAy
        let clBeX = pBx + uBe * vBx
        let clBeY = pBy + uBe * vBy
        let distSqEnd = sq (clAeX - clBeX) + sq (clAeY - clBeY)
        min distSqStart distSqEnd

    /// Only for apart Lines, not for parallel.
    /// Returns the parameters and the square distance of the closest points between two finite lines A and B that are not intersecting nor parallel
    let inline internal closestParams (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): float*float*float =
        let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy)
        // get point params at these parameters:
        let clAsX = pAx + uAs * vAx
        let clAsY = pAy + uAs * vAy
        let clBsX = pBx + uBs * vBx
        let clBsY = pBy + uBs * vBy
        let distSqStart = sq (clAsX - clBsX) + sq (clAsY - clBsY)
        let clAeX = pAx + uAe * vAx
        let clAeY = pAy + uAe * vAy
        let clBeX = pBx + uBe * vBx
        let clBeY = pBy + uBe * vBy
        let distSqEnd = sq (clAeX - clBeX) + sq (clAeY - clBeY)
        if distSqStart < distSqEnd then
            uAs, uBs, distSqStart
        else
            uAe, uBe, distSqEnd

    /// Only for apart Lines, not for parallel.
    /// Returns the closest points and the square distance between two finite lines A and B that are not intersecting nor parallel.
    let inline internal closestPts (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): Pt*Pt*float =
        let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy)
        // get point params at these parameters:
        let clAsX = pAx + uAs * vAx
        let clAsY = pAy + uAs * vAy
        let clBsX = pBx + uBs * vBx
        let clBsY = pBy + uBs * vBy
        let distSqStart = sq (clAsX - clBsX) + sq (clAsY - clBsY)
        let clAeX = pAx + uAe * vAx
        let clAeY = pAy + uAe * vAy
        let clBeX = pBx + uBe * vBx
        let clBeY = pBy + uBe * vBy
        let distSqEnd = sq (clAeX - clBeX) + sq (clAeY - clBeY)
        if distSqStart < distSqEnd then
            Pt(clAsX, clAsY), Pt(clBsX, clBsY), distSqStart
        else
            Pt(clAeX, clAeY), Pt(clBeX, clBeY), distSqEnd

    /// Checks if the line vectors is shorter than the given tolerance.
    /// It uses the manhattan distance for the check.
    /// So the cutoff is at 71% to 100% of the tooShortTolerance length.
    let inline internal isTooShort( x:float, y:float, tooShortTolerance:float) : bool =
        (abs x + abs y) < tooShortTolerance


    /// Returns the tangent between two lines.
    /// Compare it against precalculated tangent values in UtilEuclid.Tangent module.
    /// This is the cross-product divided by the dot-product of the two line vectors.
    /// Returns NaN if one of the line vectors is zero length.
    /// Returns negative or positive infinity if the lines are perpendicular. (Dot product is zero).
    let inline internal tangent (vAx:float, vAy:float, vBx:float, vBy:float): float<Tangent.tangent> =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dot = vAx * vBx + vAy * vBy // Dot product of vectors
        det / dot // tangent of the angle between the two vectors, may be negative too
        |> LanguagePrimitives.FloatWithMeasure<Tangent.tangent>

open XLine2D

/// Provides static methods for computing 2D line intersections.
/// Some functions return Discriminated Unions from the XLine2D module.
[<RequireQualifiedAccess>]
type XLine2D =

    (*
    In parallel cases the calculation may return both Nan or Infinity values for the parameter:

    0.0 / 0.0                         =  nan
    Double.Epsilon/0.0                =  infinity
    -Double.Epsilon/0.0               =  -infinity
    1.0 / Double.Epsilon              =  infinity
    1.0 / -Double.Epsilon             =  -infinity
    0.0 / Double.Epsilon              =  0.0
    0.0 / -Double.Epsilon             =  -0.0
    Double.Epsilon/Double.Epsilon     =  1.0
    -Double.Epsilon/Double.Epsilon    =  -1.0

    nan >  0.0: false
    nan <  0.0: false
    nan <= 0.0: false
    nan >= 0.0: false
    nan =  0.0: false
    nan <> 0.0: true
    *)


    /// <summary> Returns the intersection parameter on the first ray (A) where ray A and ray B intersect.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <returns> The parameter at which the two rays intersect on line A,
    ///  This is NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  This is positive or negative Infinity for almost parallel lines.</returns>
    static member inline parameterA (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): float =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx // difference in start points
        let dy = pBy - pAy // difference in start points
        (dx * vBy - dy * vBx) / det //  embrace float math, this can be NaN or Infinity

    /// <summary> Returns the intersection parameter on the first ray (A) where ray A and ray B intersect.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The parameter on line A at the intersection with line B.
    ///  NaN if both points coincide and vectors are exactly parallel (0.0/0.0).
    ///  Positive/Negative Infinity for almost parallel lines.</returns>
    static member inline parameterA (pA:Pt, pB:Pt, vA:Vc, vB:Vc): float =
        XLine2D.parameterA (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Returns the intersection parameter on the first ray (A) where ray A and ray B intersect.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The parameter on lineA at the intersection with lineB.
    ///  NaN if both start points coincide and vectors are exactly parallel (0.0/0.0).
    ///  Positive/Negative Infinity for almost parallel lines.</returns>
    static member inline parameterA (lineA: Line2D, lineB: Line2D): float =
        XLine2D.parameterA ( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )



    /// <summary> Returns the intersection parameters on both rays A and B.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their intersection.
    ///  These are NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  These are positive or negative Infinity for almost parallel lines.</returns>
    static member inline parameters (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): float*float =  // or us struct tuples
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det // embrace float math, this can be NaN or Infinity
        let u = (dx * vAy - dy * vAx) / det // embrace float math, this can be NaN or Infinity
        t,u

    /// <summary> Returns the intersection parameters on both rays A and B.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their intersection.
    ///  These are NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  These are positive or negative Infinity for almost parallel lines.</returns>
    static member inline parameters (pA:Pt, pB:Pt, vA:Vc, vB:Vc): float*float =
        XLine2D.parameters (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Returns the intersection parameters on both rays.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their intersection.
    ///  These are NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  These are positive or negative Infinity for almost parallel lines.</returns>
    static member inline parameters(lineA: Line2D, lineB: Line2D): float*float =
        XLine2D.parameters( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )

    /// <summary> Checks if two 2D-lines intersect within the parameter range 0.0 to 1.0 (with 1e-6 tolerance) on both lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <returns> True if both lines have an intersection parameter in the range 0.0 to 1.0 (with 1e-6 tolerance).
    ///  False if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    /// <remarks> This does not check for an explicit angle tolerance, so overlapping almost parallel lines may return true.</remarks>
    static member inline doIntersect(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): bool =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det // this can be NaN or Infinity
        if t > -1e-6  && t < ``1.0 + 1e-6`` then // handles NaN correctly
            let u = (dx * vAy - dy * vAx) / det
            if u > -1e-6 && u < ``1.0 + 1e-6`` then
                true
            else
                false
        else
            false

    /// <summary> Checks if two 2D-lines intersect within the parameter range 0.0 to 1.0 (with 1e-6 tolerance) on both lines.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <returns> True if both lines have an intersection parameter in the range 0.0 to 1.0 (with 1e-6 tolerance).
    ///  False if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    /// <remarks> This does not check for an explicit angle tolerance, so overlapping almost parallel lines may return true.</remarks>
    static member inline doIntersect (pA:Pt, pB:Pt, vA:Vc, vB:Vc): bool =
        XLine2D.doIntersect (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Checks if two 2D-lines intersect within the parameter range 0.0 to 1.0 (with 1e-6 tolerance) on both lines.</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <returns> True if both lines have an intersection parameter in the range 0.0 to 1.0 (with 1e-6 tolerance).
    ///  False if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    /// <remarks> This does not check for an explicit angle tolerance, so overlapping almost parallel lines may return true.</remarks>
    static member inline doIntersect (lineA: Line2D, lineB: Line2D): bool =
        XLine2D.doIntersect( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )



    /// <summary> Checks if two 2D-lines both have intersection parameters within the given ranges for each line</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive.</param>
    /// <param name="minParameterB"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterB"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> True if both lines have an intersection parameter in the range (inclusive min and max value).
    ///  False if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    static member inline isWithinRanges (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float
                                       , minParameterA, maxParameterA, minParameterB, maxParameterB): bool =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det
        if t >= minParameterA && t <= maxParameterA then // handles NaN correctly
            let u = (dx * vAy - dy * vAx) / det
            u >= minParameterB && u <= maxParameterB
        else
            false

    /// <summary> Checks if two 2D-lines both have intersection parameters within the given ranges for each line</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive.</param>
    /// <param name="minParameterB"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterB"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> True if both lines have an intersection parameter in the range (inclusive min and max value).
    ///  False if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    static member inline isWithinRanges (pA:Pt, pB:Pt, vA:Vc, vB:Vc, minParameterA, maxParameterA, minParameterB, maxParameterB): bool =
        XLine2D.isWithinRanges (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, minParameterA, maxParameterA, minParameterB, maxParameterB)

    /// <summary> Checks if two 2D-lines both have intersection parameters within the given ranges for each line</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive.</param>
    /// <param name="minParameterB"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterB"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> True if both lines have an intersection parameter in the range (inclusive min and max value).
    ///  False if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    static member inline isWithinRanges (lineA: Line2D, lineB: Line2D, minParameterA, maxParameterA, minParameterB, maxParameterB ): bool =
        XLine2D.isWithinRanges( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, minParameterA, maxParameterA, minParameterB, maxParameterB )




    /// <summary> Checks if 2D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="tolerance" > Is an optional squared distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    /// <returns> TRUE if the lines are coincident and overlap or touch.
    ///  FALSE if the lines are not coincident or do not overlap nor touch.</returns>
    static member doOverlap(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, [<OPT;DEF(1e-6)>] tolerance:float ) : bool =
        // parameter t of the closest point on line A to point B
        let t = clParamRayPt(pAx, pAy, vAx, vAy, pBx, pBy)
        let clPtX = pAx + vAx * t
        let clPtY = pAy + vAy * t
        // vector from closest point to B
        let vx = clPtX - pBx
        let vy = clPtY - pBy
        let dist = vx*vx + vy*vy
        let sqTolerance = tolerance * tolerance
        if dist > sqTolerance then
            false
        else
            let bex = pBx + vBx
            let bey = pBy + vBy
            let u = clParamRayPt(pAx, pAy, vAx, vAy, bex, bey)
            let cluPtx = pAx + vAx * u
            let cluPty = pAy + vAy * u
            // vector from closest point to B end point
            let vx2 = cluPtx - bex
            let vy2 = cluPty - bey
            let dist2 = vx2*vx2 + vy2*vy2
            if dist2 > sqTolerance then
                false
            else
                // check if the A closest point is in the range of line A
                if isBetweenZeroAndOneTolerantIncl t then
                    true
                else
                    // check if the B end point is also in the range of line A
                    if isBetweenZeroAndOneTolerantIncl u then
                        true
                    // otherwise check if B covers the A line completely
                    elif t < 0.0 && u > 1.0 then
                        true
                    else
                        u < 0.0 && t > 1.0

    /// <summary> Checks if 2D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="pA"> The start point on the first line.</param>
    /// <param name="pB"> The start point on the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="tolerance" > Is an optional squared distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    static member inline doOverlap(pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(1e-6)>] tolerance:float ) : bool =
        XLine2D.doOverlap(pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tolerance)

    /// <summary> Checks if 2D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <param name="tolerance" > Is an optional squared distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    static member inline  doOverlap(lineA:Line2D, lineB:Line2D, [<OPT;DEF(1e-6)>] tolerance:float ) : bool =
        XLine2D.doOverlap( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tolerance )



    // tryOverlap : Line2D option
    // is covered by tryProjectOntoLineParam



    //
    //      ███████               █████     ███
    //    ███░░░░░███            ░░███     ░░░
    //   ███     ░░███ ████████  ███████   ████   ██████  ████████    █████
    //  ░███      ░███░░███░░███░░░███░   ░░███  ███░░███░░███░░███  ███░░
    //  ░███      ░███ ░███ ░███  ░███     ░███ ░███ ░███ ░███ ░███ ░░█████
    //  ░░███     ███  ░███ ░███  ░███ ███ ░███ ░███ ░███ ░███ ░███  ░░░░███
    //   ░░░███████░   ░███████   ░░█████  █████░░██████  ████ █████ ██████
    //     ░░░░░░░     ░███░░░     ░░░░░  ░░░░░  ░░░░░░  ░░░░ ░░░░░ ░░░░░░
    //                 ░███
    //                 █████
    //                ░░░░░


    /// <summary> Returns the intersection parameter on the first ray (A) where ray A and ray B intersect.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <returns> The parameter at which the two rays intersect on line A if it is smaller than 1e12 in absolute value,
    /// or None if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryParameterA (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): float option =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx // difference in start points
        let dy = pBy - pAy // difference in start points
        let t = (dx * vBy - dy * vBx) / det //  embrace float math, this can be NaN or Infinity
        if t > -1e12 && t < 1e12 then
            Some t
        else
            None

    /// <summary> Returns the intersection parameter on the first ray (A) where ray A and ray B intersect.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The parameter on line A at the intersection with line B if it is smaller than 1e12 in absolute value,
    /// or None if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryParameterA (pA:Pt, pB:Pt, vA:Vc, vB:Vc): float option =
        XLine2D.tryParameterA (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Returns the intersection parameter on the first ray (A) where ray A and ray B intersect.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The parameter on lineA at the intersection with lineB if it is smaller than 1e12 in absolute value,
    /// or None if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryParameterA (lineA:Line2D, lineB:Line2D): float option =
        XLine2D.tryParameterA (lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )



    /// <summary> Returns the intersection point of two rays if it is within the given parameter range for line A.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> A point if lineA has an intersection parameter in the range (inclusive min and max value).
    ///  None if the lines are parallel, coincident or the intersection parameter is outside of the range.
    /// </returns>
    static member inline tryIntersectInRangeA (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, minParameterA, maxParameterA): Pt option =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det //  embrace float math, this can be NaN or Infinity
        if t >= minParameterA && t <= maxParameterA then // handles NaN correctly
            Some <| Pt(pAx + t * vAx, pAy + t * vAy)
        else
            None

    /// <summary> Returns the intersection point of two rays if it is within the given parameter range for line A.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> A point if lineA has an intersection parameter in the range (inclusive min and max value).
    /// None if the lines are parallel, coincident or the intersection parameter is outside of the range.
    /// </returns>
    static member inline tryIntersectInRangeA (pA:Pt, pB:Pt, vA:Vc, vB:Vc, minParameterA, maxParameterA): Pt option =
        XLine2D.tryIntersectInRangeA (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, minParameterA, maxParameterA)

    /// <summary> Returns the intersection point of two rays if it is within the given parameter range for line A.</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> A point if lineA has an intersection parameter in the range (inclusive min and max value).
    /// None if the lines are parallel, coincident or the intersection parameter is outside of the range.
    /// </returns>
    static member inline tryIntersectInRangeA (lineA: Line2D, lineB: Line2D, minParameterA, maxParameterA): Pt option =
        XLine2D.tryIntersectInRangeA( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, minParameterA, maxParameterA )



    /// <summary> Returns the intersection point of two rays if it is within the given parameter ranges for line A and line B.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive.</param>
    /// <param name="minParameterB"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterB"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> A point if both lines have an intersection parameter in the range (inclusive min and max value).
    ///  None if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    static member inline tryIntersectInRanges (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, minParameterA, maxParameterA, minParameterB, maxParameterB): Pt option =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det //  embrace float math, this can be NaN or Infinity
        if t >= minParameterA && t <= maxParameterA then // handles NaN correctly
            let u = (dx * vAy - dy * vAx) / det
            if u >= minParameterB && u <= maxParameterB then
                Some <| Pt(pAx + t * vAx, pAy + t * vAy)
            else
                None
        else
            None

    /// <summary> Returns the intersection point of two rays if it is within the given parameter ranges for line A and line B.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive</param>
    /// <param name="minParameterB"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterB"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> A point if both lines have an intersection parameter in the range (inclusive
    /// min and max value).     None if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    static member inline tryIntersectInRanges (pA:Pt, pB:Pt, vA:Vc, vB:Vc, minParameterA, maxParameterA, minParameterB, maxParameterB): Pt option =
        XLine2D.tryIntersectInRanges (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, minParameterA, maxParameterA, minParameterB, maxParameterB)

    /// <summary> Returns the intersection point of two rays if it is within the given parameter ranges for line A and line B.</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive.</param>
    /// <param name="minParameterB"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterB"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> A point if both lines have an intersection parameter in the range (inclusive min and max value).
    /// None if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    static member inline tryIntersectInRanges (lineA: Line2D, lineB: Line2D, minParameterA, maxParameterA, minParameterB, maxParameterB): Pt option =
        XLine2D.tryIntersectInRanges( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, minParameterA, maxParameterA, minParameterB, maxParameterB )



    /// <summary> Returns the intersection point of two finite line segments if they intersect.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <returns> The intersection point if both parameters are within 0.0 to 1.0;
    /// None otherwise or if parallel/coincident.</returns>
    /// <remarks> This method uses a small tolerance of 1e-6 to account for numerical inaccuracies when checking the 0..1 range.</remarks>
    static member inline tryIntersect (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): Pt option =
        XLine2D.tryIntersectInRanges (pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, -1e-6, ``1.0 + 1e-6``, -1e-6, ``1.0 + 1e-6``)

    /// <summary> Returns the intersection point of two finite line segments if they intersect.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> Vector from pA to the other end of line A (its length defines the domain 0..1).</param>
    /// <param name="vB"> Vector from pB to the other end of line B (its length defines the domain 0..1).</param>
    /// <returns> The intersection point if both parameters are within 0.0 to 1.0;
    /// None otherwise or if parallel/coincident.</returns>
    /// <remarks> This method uses a small tolerance of 1e-6 to account for numerical inaccuracies when checking the 0..1 range.</remarks>
    static member inline tryIntersect (pA:Pt, pB:Pt, vA:Vc, vB:Vc): Pt option =
        XLine2D.tryIntersect (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Returns the intersection point of two finite line segments if they intersect.</summary>
    /// <param name="lineA"> First finite line segment.</param>
    /// <param name="lineB"> Second finite line segment.</param>
    /// <returns> The intersection point if both parameters are within 0.0 to 1.0;
    /// None otherwise or if parallel/coincident.</returns>
    /// <remarks> This method uses a small tolerance of 1e-6 to account for numerical inaccuracies when checking the 0..1 range.</remarks>
    static member inline tryIntersect (lineA: Line2D, lineB: Line2D): Pt option =
        XLine2D.tryIntersect( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )


    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <returns> The point at which the two rays intersect or None.</returns>
    /// <remarks> If the lines are parallel or coincident, or if the parameter on Line A is bigger than 1e12 in absolute value,
    /// None is returned.</remarks>
    static member inline tryIntersectRay (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): Pt option =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det //  this can be NaN or Infinity
        if t > -1e12 && t < 1e12 then // checks for NaN correctly
            Some <| Pt(pAx + t * vAx, pAy + t * vAy)
        else
            None


    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The point at which the two rays intersect or None.</returns>
    /// <remarks> If the lines are parallel or coincident, or if the parameter on Line A is bigger than 1e12 in absolute value,
    /// None is returned.</remarks>
    static member inline tryIntersectRay (pA:Pt, pB:Pt, vA:Vc, vB:Vc): Pt option =
        XLine2D.tryIntersectRay (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The point at which the two rays intersect or None.</returns>
    /// <remarks> If the lines are parallel or coincident, or if the parameter on Line A is bigger than 1e12 in absolute value,
    /// None is returned.</remarks>
    static member inline tryIntersectRay (lineA: Line2D, lineB: Line2D): Pt option =
        XLine2D.tryIntersectRay (lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )






    //      ██████████    ███                              ███                   ███                        █████                 █████
    //     ░░███░░░░███  ░░░                              ░░░                   ░░░                        ░░███                 ░░███
    //      ░███   ░░███ ████   █████   ██████  ████████  ████  █████████████   ████  ████████    ██████   ███████    ██████   ███████
    //      ░███    ░███░░███  ███░░   ███░░███░░███░░███░░███ ░░███░░███░░███ ░░███ ░░███░░███  ░░░░░███ ░░░███░    ███░░███ ███░░███
    //      ░███    ░███ ░███ ░░█████ ░███ ░░░  ░███ ░░░  ░███  ░███ ░███ ░███  ░███  ░███ ░███   ███████   ░███    ░███████ ░███ ░███
    //      ░███    ███  ░███  ░░░░███░███  ███ ░███      ░███  ░███ ░███ ░███  ░███  ░███ ░███  ███░░███   ░███ ███░███░░░  ░███ ░███
    //      ██████████   █████ ██████ ░░██████  █████     █████ █████░███ █████ █████ ████ █████░░████████  ░░█████ ░░██████ ░░████████
    //     ░░░░░░░░░░   ░░░░░ ░░░░░░   ░░░░░░  ░░░░░     ░░░░░ ░░░░░ ░░░ ░░░░░ ░░░░░ ░░░░ ░░░░░  ░░░░░░░░    ░░░░░   ░░░░░░   ░░░░░░░░
    //
    //              █████  █████             ███
    //             ░░███  ░░███             ░░░
    //              ░███   ░███  ████████   ████   ██████  ████████    █████
    //              ░███   ░███ ░░███░░███ ░░███  ███░░███░░███░░███  ███░░
    //              ░███   ░███  ░███ ░███  ░███ ░███ ░███ ░███ ░███ ░░█████
    //              ░███   ░███  ░███ ░███  ░███ ░███ ░███ ░███ ░███  ░░░░███
    //              ░░████████   ████ █████ █████░░██████  ████ █████ ██████
    //               ░░░░░░░░   ░░░░ ░░░░░ ░░░░░  ░░░░░░  ░░░░ ░░░░░ ░░░░░░     DOS Rebel font by ASCIIDecorator vscode plugin



    /// <summary> Tries to get intersection parameters of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An XRayParam discriminated union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    static member getRayIntersectionParam(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float,
                                [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                ) : XRayParam =
        if isTooShort(vAx, vAy, tooShortTolerance) then
            if isTooShort(vBx, vBy, tooShortTolerance) then
                XRayParam.TooShortBoth
            else
                XRayParam.TooShortA
        elif isTooShort(vBx, vBy, tooShortTolerance) then
            XRayParam.TooShortB
        else
            let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
            let dot = vAx * vBx + vAy * vBy // Dot product of vectors
            let tan = det / dot // tangent of the angle between the two vectors, may be negative too
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let dx = pBx - pAx
                let dy = pBy - pAy
                let t = (dx * vBy - dy * vBx) / det
                let u = (dx * vAy - dy * vAx) / det
                XRayParam.Intersect ((t,u))
            else
                XRayParam.Parallel

    /// <summary> Tries to get intersection parameters of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An XRayParam discriminated union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    static member inline getRayIntersectionParam(pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRayParam =
        XLine2D.getRayIntersectionParam (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

    /// <summary> Tries to get intersection parameters of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XRayParam discriminated union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    static member inline getRayIntersectionParam(lineA: Line2D, lineB: Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRayParam =
        XLine2D.getRayIntersectionParam( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance )



    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XRay Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB</returns>
    static member getRayIntersection(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float,
                                [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                ) : XRay =
        if isTooShort(vAx, vAy, tooShortTolerance) then
            if isTooShort(vBx, vBy, tooShortTolerance) then
                XRay.TooShortBoth
            else
                XRay.TooShortA
        elif isTooShort(vBx, vBy, tooShortTolerance) then
            XRay.TooShortB
        else
            let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
            let dot = vAx * vBx + vAy * vBy // Dot product of vectors
            let tan = det / dot // tangent of the angle between the two vectors, may be negative too
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let t = (dx * vBy - dy * vBx) / det
                let x = pAx +  t * vAx
                let y = pAy +  t * vAy
                XRay.Intersect (Pt(x,y))
            else
                XRay.Parallel

    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XRay Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getRayIntersection(pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRay =
        XLine2D.getRayIntersection(pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XRay Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getRayIntersection(lineA: Line2D, lineB: Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRay =
        XLine2D.getRayIntersection( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance )



    /// <summary> Tries to get intersection parameters of two finite 2D-lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XParam Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member getIntersectionParam(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float,
                                [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                ) : XParam =
        if isTooShort(vAx, vAy, tooShortTolerance) then
            if isTooShort(vBx, vBy, tooShortTolerance) then
                XParam.TooShortBoth
            else
                XParam.TooShortA
        elif isTooShort(vBx, vBy, tooShortTolerance) then
            XParam.TooShortB
        else
            let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
            let dot = vAx * vBx + vAy * vBy // Dot product of vectors
            let tan = det / dot // tangent of the angle between the two vectors, may be negative too
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let t = (dx * vBy - dy * vBx) / det // embrace float math, this can be NaN or Infinity
                let u = (dx * vAy - dy * vAx) / det // embrace float math, this can be NaN or Infinity
                if t > -1e-6 && t < ``1.0 + 1e-6`` && u > -1e-6 && u < ``1.0 + 1e-6`` then
                    XParam.Intersect (t,u)
                else
                    XParam.Apart
            else
                XParam.Parallel

    /// <summary> Tries to get intersection parameters of two finite 2D-lines.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XParam Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersectionParam(pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XParam =
        XLine2D.getIntersectionParam (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

    /// <summary> Tries to get intersection parameters of two finite 2D-lines.</summary>
    /// <param name="lnA"> First line.</param>
    /// <param name="lnB"> Second line.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XParam Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersectionParam(lnA: Line2D, lnB: Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XParam =
        XLine2D.getIntersectionParam( lnA.FromX, lnA.FromY, lnB.FromX, lnB.FromY, lnA.VectorX        , lnA.VectorY        , lnB.VectorX        , lnB.VectorY        , tangent, tooShortTolerance )



    /// <summary> Tries to get intersection point of two finite 2D lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XPt Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member getIntersection(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float,
                                [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                ) : XPt =
        if isTooShort(vAx, vAy, tooShortTolerance) then
            if isTooShort(vBx, vBy, tooShortTolerance) then
                XPt.TooShortBoth
            else
                XPt.TooShortA
        elif isTooShort(vBx, vBy, tooShortTolerance) then
            XPt.TooShortB
        else
            let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
            let dot = vAx * vBx + vAy * vBy // Dot product of vectors
            let tan = det / dot // tangent of the angle between the two vectors, may be negative too
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let t = (dx * vBy - dy * vBx) / det // embrace float math, this can be NaN or Infinity
                let u = (dx * vAy - dy * vAx) / det // embrace float math, this can be NaN or Infinity
                if t > -1e-6 && t < ``1.0 + 1e-6`` && u > -1e-6 && u < ``1.0 + 1e-6`` then
                    let x = pAx +  t * vAx
                    let y = pAy +  t * vAy
                    XPt.Intersect (Pt(x,y))
                else
                    XPt.Apart
            else
                XPt.Parallel

    /// <summary> Tries to get intersection point of two finite 2D lines.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XPt Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersection(pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,[<OPT;DEF(1e-6)>] tooShortTolerance:float) : XPt =
        XLine2D.getIntersection(pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

    /// <summary> Tries to get intersection point of two finite 2D lines.</summary>
    /// <param name="lineA"> First finite line segment.</param>
    /// <param name="lineB"> Second finite line segment.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XPt Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersection(lineA: Line2D, lineB: Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XPt =
        XLine2D.getIntersection( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance )




    /// <summary> Gets the parameters on the finite lines where the lines are closest to each other.
    /// For parallel or coincident lines, this returns the midpoint of the interval spanned by the projected endpoints
    /// (which coincides with the middle of any overlapping segment when one exists).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An ClParams Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Apart of parameterA:float * parameterB:float * squareDistance:float
    /// | Parallel of float*float
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    static member getClosestParameters (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float,
                                        [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                        [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                        ): ClParams =
        if isTooShort(vAx, vAy, tooShortTolerance) then
            if isTooShort(vBx, vBy, tooShortTolerance) then
                ClParams.TooShortBoth
            else
                ClParams.TooShortA
        elif isTooShort(vBx, vBy, tooShortTolerance) then
            ClParams.TooShortB
        else
            let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
            let dot = vAx * vBx + vAy * vBy // Dot product of vectors
            let tan = det / dot // tangent of the angle between the two vectors, may be negative too
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let t = (dx * vBy - dy * vBx) / det // embrace float math, this can be NaN or Infinity
                let u = (dx * vAy - dy * vAx) / det // embrace float math, this can be NaN or Infinity
                if t >= -1e-6 && t <= ``1.0 + 1e-6`` && u >= -1e-6 && u <= ``1.0 + 1e-6`` then
                    ClParams.Intersect (t,u)
                else
                    ClParams.Apart <| closestParams(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy)
            else
                let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy)
                ClParams.Parallel (
                    (uAs + uAe) * 0.5,
                    (uBs + uBe) * 0.5 )

    /// <summary> Gets the parameters on the finite lines where the lines are closest to each other.
    /// For parallel or coincident lines, this returns the midpoint of the interval spanned by the projected endpoints
    /// (which coincides with the middle of any overlapping segment when one exists).</summary>
    /// <param name="pA"> The start point on the first line.</param>
    /// <param name="pB"> The start point on the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An ClParams Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Apart of parameterA:float * parameterB:float * squareDistance:float
    /// | Parallel of float*float
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    static member inline getClosestParameters (pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float): ClParams =
        XLine2D.getClosestParameters (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

    /// <summary> Gets the parameters on the finite lines where the lines are closest to each other.
    /// For parallel or coincident lines, this returns the midpoint of the interval spanned by the projected endpoints
    /// (which coincides with the middle of any overlapping segment when one exists).</summary>
    /// <param name="lineA"> The first finite line segment.</param>
    /// <param name="lineB"> The second finite line segment.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An ClParams Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Apart of parameterA:float * parameterB:float * squareDistance:float
    /// | Parallel of float*float
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    static member inline getClosestParameters (lineA:Line2D, lineB:Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float): ClParams =
        XLine2D.getClosestParameters ( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance)



    /// <summary> Gets the point or points on the finite lines where the lines are closest to each other.
    /// For parallel or coincident lines, this returns the midpoint of the interval spanned by the projected endpoints
    /// (matching the middle of the overlapping segment when one exists).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An ClPts Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Apart of Pt * Pt * squareDistance:float
    /// | Parallel of Pt * Pt
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth
    /// </returns>
    /// <remarks> This method uses a small tolerance of 1e-6 to account for numerical inaccuracies when checking the 0..1 range.</remarks>
    static member getClosestPoints (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float,
            [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
            [<OPT;DEF(1e-6)>] tooShortTolerance:float
            ): ClPts =
        if isTooShort(vAx, vAy, tooShortTolerance) then
            if isTooShort(vBx, vBy, tooShortTolerance) then
                ClPts.TooShortBoth
            else
                ClPts.TooShortA
        elif isTooShort(vBx, vBy, tooShortTolerance) then
            ClPts.TooShortB
        else
            let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
            let dot = vAx * vBx + vAy * vBy // Dot product of vectors
            let tan = det / dot // tangent of the angle between the two vectors, may be negative too
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let t = (dx * vBy - dy * vBx) / det // embrace float math, this can be NaN or Infinity
                let u = (dx * vAy - dy * vAx) / det // embrace float math, this can be NaN or Infinity
                if t > -1e-6 && t < ``1.0 + 1e-6`` && u > -1e-6 && u < ``1.0 + 1e-6`` then
                    ClPts.Intersect <| Pt(pAx + t * vAx, pAy + t * vAy)
                else
                    ClPts.Apart <| closestPts(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy)
            else
                let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy)
                let tA = (uAs + uAe) * 0.5
                let tB = (uBs + uBe) * 0.5
                let a = Pt(pAx + tA * vAx, pAy + tA * vAy)
                let b = Pt(pBx + tB * vBx, pBy + tB * vBy)
                ClPts.Parallel (a, b)

    /// <summary> Gets the point or points on the finite lines where the lines are closest to each other.
    /// For parallel or coincident lines, this returns the midpoint of the interval spanned by the projected endpoints
    /// (matching the middle of the overlapping segment when one exists).</summary>
    /// <param name="pA"> The start point on the first line.</param>
    /// <param name="pB"> The start point on the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An ClPts Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Apart of Pt * Pt * squareDistance:float
    /// | Parallel of Pt * Pt
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    /// <remarks> This method uses a small tolerance of 1e-6 to account for numerical inaccuracies when checking the 0..1 range.</remarks>
    static member inline getClosestPoints (pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : ClPts =
        XLine2D.getClosestPoints (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

    /// <summary> Gets the point or points on the finite lines where the lines are closest to each other.
    /// For parallel or coincident lines, this returns the midpoint of the interval spanned by the projected endpoints
    /// (matching the middle of the overlapping segment when one exists).</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An ClPts Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Apart of Pt * Pt * squareDistance:float
    /// | Parallel of Pt * Pt
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    /// <remarks> This method uses a small tolerance of 1e-6 to account for numerical inaccuracies when checking the 0..1 range.</remarks>
    static member inline getClosestPoints (lineA: Line2D, lineB: Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : ClPts =
        XLine2D.getClosestPoints ( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance)


    /// <summary> Gets the square distance between two finite lines. Works on Parallel lines too.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <returns> The squared distance between the two lines.</returns>
    static member getSqDistance(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float ): float =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx // difference in start points
        let dy = pBy - pAy // difference in start points
        let t = (dx * vBy - dy * vBx) / det // embrace float math, this can be NaN or Infinity
        let u = (dx * vAy - dy * vAx) / det // embrace float math, this can be NaN or Infinity
        if t >= 0.0 && t <= 1.0 && u >= 0.0 && u <= 1.0 then // handles NaN and Infinity correctly too
            0.0
        else
            // checking for parallel lines would not help here .
            // we need to call projectEndsBackAndForth anyway to find the closest points on the finite lines
            sqDistance(pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy)

    /// <summary> Gets the square distance between two finite lines. Works on Parallel lines too.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <returns> The squared distance between the two lines.</returns>
    static member inline getSqDistance(pA:Pt, pB:Pt, vA:Vc, vB:Vc ): float =
        XLine2D.getSqDistance (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Gets the square distance between two finite lines. Works on Parallel lines too.</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <returns> The squared distance between the two lines.</returns>
    static member inline getSqDistance(lineA: Line2D, lineB: Line2D ): float =
        XLine2D.getSqDistance (lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )




    /// <summary>Checks if the two finite 2D lines are touching each other at exactly one of their end points
    /// within the given tolerance.</summary>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <param name="tolerance"> Is an optional distance tolerance. 1e-6 by default.</param>
    /// <returns>A Discriminated Union LineEndsTouching that describes the possible cases of two finite 2D lines touching at their ends:
    /// | NotTouching
    /// | StartA_StartB
    /// | EndA_EndB
    /// | EndA_StartB
    /// | StartA_EndB
    /// | Identical
    /// | IdenticalFlipped
    /// </returns>
    static member getEndsTouching (a:Line2D, b:Line2D, [<OPT;DEF(1e-6)>] tolerance:float) : XEnds =
        let sqTolerance = tolerance * tolerance
        if sq(a.ToX-b.FromX) + sq(a.ToY-b.FromY) < sqTolerance then
            if sq(a.FromX-b.ToX) + sq(a.FromY-b.ToY) < sqTolerance then
                XEnds.IdenticalFlipped
            else
                XEnds.EndA_StartB
        elif sq(a.FromX-b.ToX) + sq(a.FromY-b.ToY) < sqTolerance then
            XEnds.StartA_EndB
        elif sq(a.FromX-b.FromX) + sq(a.FromY-b.FromY) < sqTolerance then
            if sq(a.ToX-b.ToX) + sq(a.ToY-b.ToY) < sqTolerance then
                XEnds.Identical
            else
                XEnds.StartA_StartB
        elif sq(a.ToX-b.ToX) + sq(a.ToY-b.ToY) < sqTolerance then
            XEnds.EndA_EndB
        else
            XEnds.NotTouching
