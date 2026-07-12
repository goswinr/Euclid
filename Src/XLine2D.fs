
namespace Euclid

open System
open UtilEuclid


// #region XLine2D types

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

        /// The lines are parallel or coincident.
        /// Overlapping parallel lines can have many parameters of closest distance.
        /// Contains the parameters at the midpoint of the overlapping segment.
        /// These parameters are in the range 0.0 to 1.0
        | Parallel of paramA:float * paramB:float

        /// The lines are apart. Contains the parameters at their closest points.
        /// These parameters are in the range 0.0 to 1.0
        | Apart of paramA:float * paramB:float

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

        /// The lines are parallel or coincident.
        /// Overlapping parallel lines can have many closest points.
        /// Contains the points at the midpoint of the overlapping segment.
        | Parallel of ptA:Pt * ptB:Pt

        /// The lines are apart. Contains the closest points on both lines.
        | Apart of ptA:Pt * ptB:Pt

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


// #endregion
// #region XLineXY module

module XLineXY =

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
    /// The result is clamped to the range 0.0 to 1.0.
    /// If the line vector is zero length, 0.0 is returned.
    let inline internal clParamLnPt (ln:Line2D, x:float, y:float) : float =
        clParamRayPt(ln.FromX, ln.FromY, ln.VectorX , ln.VectorY, x, y) |> clamp01NaN

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
        // https://www.youtube.com/watch?v=PMltMdi1Wzg
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
    let inline parameterA (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): float =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx // difference in start points
        let dy = pBy - pAy // difference in start points
        (dx * vBy - dy * vBx) / det //  embrace float math, this can be NaN or Infinity




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
    let inline parameters (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): float*float =  // or us struct tuples
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det // embrace float math, this can be NaN or Infinity
        let u = (dx * vAy - dy * vAx) / det // embrace float math, this can be NaN or Infinity
        t,u

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
    let inline doIntersect(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): bool =
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
    let inline isWithinRanges (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, minParameterA, maxParameterA, minParameterB, maxParameterB): bool =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det
        if t >= minParameterA && t <= maxParameterA then // handles NaN correctly
            let u = (dx * vAy - dy * vAx) / det
            u >= minParameterB && u <= maxParameterB
        else
            false



    /// <summary> Checks if 2D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="tolerance" > Is a distance tolerance (compared against the squared distance internally).
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    /// <returns> TRUE if the lines are coincident and overlap or touch.
    ///  FALSE if the lines are not coincident or do not overlap nor touch.</returns>
    let doOverlap(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, tolerance:float ) : bool =
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

    /// <summary> Intersects two infinite 2D lines. Fails if they are parallel or zero length.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <returns> The intersection point of the two rays.</returns>
    let inline intersectRays (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): Pt =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        if abs det < 1e-18 then
            EuclidErrors.fail "XLineXY.intersectRays: lines are parallel or of zero length."
        let dx = pBx - pAx // difference in start points
        let dy = pBy - pAy // difference in start points
        let t = (dx * vBy - dy * vBx) / det
        Pt(pAx + t * vAx, pAy + t * vAy)


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
    let sqDistance(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float ): float =
        // Ericson's parametric segment–segment routine (Real-Time Collision Detection, §5.1.9), specialized to 2D.
        let inline distToAStart tb = // takes parameter on vec B
            // closest point on B is cB = pB + tb·vB, distance to pA
            let dx = pAx - pBx - tb*vBx
            let dy = pAy - pBy - tb*vBy
            dx*dx + dy*dy

        let inline distToBStart ta = // takes parameter on vec A
            let dx = pAx + ta*vAx - pBx
            let dy = pAy + ta*vAy - pBy
            dx*dx + dy*dy

        let inline distToBEnd ta = // takes parameter on vec A
            // closest point on B is its end cB = pB + vB, distance to A(ta)
            let dx = pAx + ta*vAx - pBx - vBx
            let dy = pAy + ta*vAy - pBy - vBy
            dx*dx + dy*dy

        let inline distOnLine ta tb =
            // closest points: cA = pA + ta·vA, cB = pB + tb·vB
            let dx = pAx + ta*vAx - pBx - tb*vBx
            let dy = pAy + ta*vAy - pBy - tb*vBy
            dx*dx + dy*dy

        // r = pA - pB  (vector from start of B to start of A)
        let rx = pAx - pBx
        let ry = pAy - pBy
        let a = vAx*vAx + vAy*vAy   // squared length of A,  vA·vA   (>= 0)
        let e = vBx*vBx + vBy*vBy   // squared length of B,  vB·vB   (>= 0)
        let f = vBx*rx + vBy*ry     // vB·r
        // Tolerance for treating a segment as a degenerate point.
        // It is compared against *squared* lengths, so this ~= (1e-6)^2.
        let tolSq = 1e-12

        if a < tolSq && e < tolSq then
            rx*rx + ry*ry // both segments are points: distance between them
        elif a <= tolSq then
            // A is a point: closest point on B to pA, t = f/e
            let tb = max 0.0 (min 1.0 (f / e))
            distToAStart tb
        else
            let c = vAx*rx + vAy*ry             // vA·r
            if e < tolSq then
                // B is a point: closest point on A to pB, s = -c/a
                let ta = max 0.0 (min 1.0 (-c / a))
                distToBStart ta
            else
                let mutable ta = 0.0
                let mutable tb = 0.0
                let b = vAx*vBx + vAy*vBy // vA·vB
                let denom = a*e - b*b     // = 0 if parallel
                if denom <> 0.0 then
                    // The denom <> 0.0 exact-zero test is deliberately not a tolerance check.
                    // For merely near-parallel segments, dividing by a tiny denom can throw s way out of range,
                    // but the subsequent clamp-and-recompute steps pull it back to the correct endpoint — so the exact test is actually the robust choice here.
                    ta <- max 0.0 (min 1.0 ((b*f - c*e) / denom))

                // else parallel: keep s = 0, the t-clamp below recovers the correct distance
                tb <- (b*ta + f) / e  // closest point on line B to A(s)
                if tb <= 0.0 then
                    let ta = max 0.0 (min 1.0 (-c / a))
                    distToBStart ta
                elif tb >= 1.0 then
                    let ta = max 0.0 (min 1.0 ((b - c) / a))
                    distToBEnd ta
                else
                    distOnLine ta tb

    /// <summary> Gets the parameters s and t on two finite lines where they are closest to each other.
    /// Works on parallel lines too.
    /// </summary>
    /// <returns> A struct tuple (s, t) of the two clamped parameters. 0.0,0.0 for zero-length lines.</returns>
    /// <remarks> For overlapping parallels there are many valid closest pairs. This algorithm returns the parameter closest to 0.0 on segment A.
    /// The more detailed function `getClosestParameters` returns the midpoint of the overlapping segment.</remarks>
    let closestParameters(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float) : struct(float*float) =
        let diffX = pAx - pBx
        let diffY = pAy - pBy
        let sqLenA = vAx*vAx + vAy*vAy   // vA·vA  (>= 0)
        let sqLenB = vBx*vBx + vBy*vBy   // vB·vB  (>= 0)
        let dotB = vBx*diffX + vBy*diffY     // vB·r
        let sqTol = 1e-12             // compared against squared lengths
        let mutable ta = 0.0
        let mutable tb = 0.0
        if sqLenA <= sqTol && sqLenB <= sqTol then
            ()                                      // both points: s = t = 0
        elif sqLenA <= sqTol then
            tb <- max 0.0 (min 1.0 (dotB / sqLenB))          // A is a point
        else
            let dotA = vAx*diffX + vAy*diffY                 // vA·r
            if sqLenB <= sqTol then
                ta <- max 0.0 (min 1.0 (-dotA / sqLenA))     // B is a point
            else
                let b = vAx*vBx + vAy*vBy            // vA·vB
                let denom = sqLenA*sqLenB - b*b     //  = 0 iff parallel
                if denom <> 0.0 then
                    // The denom <> 0.0 exact-zero test is deliberately not a tolerance check.
                    // For merely near-parallel segments, dividing by a tiny denom can throw s way out of range,
                    // but the subsequent clamp-and-recompute steps pull it back to the correct endpoint — so the exact test is actually the robust choice here.
                    ta <- max 0.0 (min 1.0 ((b*dotB - dotA*sqLenB) / denom))
                tb <- (b*ta + dotB) / sqLenB
                if tb < 0.0 then
                    tb <- 0.0
                    ta <- max 0.0 (min 1.0 (-dotA / sqLenA))
                elif tb > 1.0 then
                    tb <- 1.0
                    ta <- max 0.0 (min 1.0 ((b - dotA) / sqLenA))
        struct(ta, tb)



    // #endregion
    // #region Options


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
    let inline tryParameterA (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): float voption =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx // difference in start points
        let dy = pBy - pAy // difference in start points
        let t = (dx * vBy - dy * vBx) / det //  embrace float math, this can be NaN or Infinity
        if t > -1e12 && t < 1e12 then
            ValueSome t
        else
            ValueNone


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
    let inline tryIntersectInRangeA (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, minParameterA, maxParameterA): Pt voption =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det //  embrace float math, this can be NaN or Infinity
        if t >= minParameterA && t <= maxParameterA then // handles NaN correctly
            ValueSome <| Pt(pAx + t * vAx, pAy + t * vAy)
        else
            ValueNone


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
    let inline tryIntersectInRanges (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, minParameterA, maxParameterA, minParameterB, maxParameterB): Pt voption =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant), this is 0.0 if vectors are parallel
        let dx = pBx - pAx
        let dy = pBy - pAy
        let t = (dx * vBy - dy * vBx) / det //  embrace float math, this can be NaN or Infinity
        if t >= minParameterA && t <= maxParameterA then // handles NaN correctly
            let u = (dx * vAy - dy * vAx) / det
            if u >= minParameterB && u <= maxParameterB then
                ValueSome <| Pt(pAx + t * vAx, pAy + t * vAy)
            else
                ValueNone
        else
            ValueNone


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
    let inline tryIntersect (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float): Pt voption =
        tryIntersectInRanges (pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, -1e-6, ``1.0 + 1e-6``, -1e-6, ``1.0 + 1e-6``)



    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="tangent" > The tangent value of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <returns> The point at which the two rays intersect or None.</returns>
    /// <remarks> If the lines are parallel or coincident (within the tangent tolerance) or too short, None is returned.</remarks>
    let inline tryIntersectRay (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, tangent:float<Tangent.tangent>): Pt voption =
        let det = vAx * vBy - vAy * vBx // Cross product of vectors (determinant, or signed area of parallelogram between them), this is 0.0 if vectors are parallel or very short
        let dot = vAx * vBx + vAy * vBy // Dot product of vectors, may be 0.0 for perpendicular vectors
        let absDet = abs det
        // (1) division-free tangent test to return None on parallel lines: |det| / |dot| > tangent  <=>  |det| > tangent · |dot|. If both lines have zero length, det and dot are 0.0, so this is FALSE, handled too.
        // (2) length check on det , (the area of the parallelogram) , so if both perpendicular lines ar shorter than 0.00001 (1e-5)
        // None is returned. because  1e-5 * 1e-5 = 1e-10
        // If one line is longer but the other one even shorter, None is still returned as long as the area of the parallelogram is smaller than 1e-10
        if !^ absDet > tangent * abs dot && absDet > 1e-10 then
            let dx = pBx - pAx
            let dy = pBy - pAy
            let t = (dx * vBy - dy * vBx) / det //  this can't be NaN or Infinity anymore because of the tangent check
            ValueSome <| Pt(pAx + t * vAx, pAy + t * vAy)
        else
            ValueNone



    open XLine2D

    // #endregion
    // #region DUs

    /// <summary> Tries to get intersection parameters of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An XRayParam discriminated union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    let getRayIntersectionParam(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, tangent:float<Tangent.tangent>,tooShortTolerance:float) : XRayParam =
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
            // division-free parallel test: |det| / |dot| > tangent  <=>  |det| > tangent · |dot| (handles NaN/zero-length too)
            if !^ (abs det) > tangent * abs dot then
                let dx = pBx - pAx
                let dy = pBy - pAy
                let t = (dx * vBy - dy * vBx) / det
                let u = (dx * vAy - dy * vAx) / det
                XRayParam.Intersect ((t,u))
            else
                XRayParam.Parallel

    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XRay Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB</returns>
    let getRayIntersection(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, tangent:float<Tangent.tangent>, tooShortTolerance:float ) : XRay =
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
            // division-free parallel test: |det| / |dot| > tangent  <=>  |det| > tangent · |dot| (handles NaN/zero-length too)
            if !^ (abs det) > tangent * abs dot then
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let t = (dx * vBy - dy * vBx) / det
                let x = pAx +  t * vAx
                let y = pAy +  t * vAy
                XRay.Intersect (Pt(x,y))
            else
                XRay.Parallel



    /// <summary> Tries to get intersection parameters of two finite 2D-lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XParam Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    let getIntersectionParam(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, tangent:float<Tangent.tangent>,tooShortTolerance:float) : XParam =
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
            // division-free parallel test: |det| / |dot| > tangent  <=>  |det| > tangent · |dot| (handles NaN/zero-length too)
            if !^ (abs det) > tangent * abs dot then
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


    /// <summary> Tries to get intersection point of two finite 2D lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XPt Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    let getIntersection(pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, tangent:float<Tangent.tangent>, tooShortTolerance:float ) : XPt =
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
            // division-free parallel test: |det| / |dot| > tangent  <=>  |det| > tangent · |dot| (handles NaN/zero-length too)
            if !^ (abs det) > tangent * abs dot then
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


    /// <summary> Gets the parameters on the finite lines where the lines are closest to each other.
    /// For parallel or overlapping lines, there are many solutions, The parameters in the middle of the overlap are returned.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="tangent"> This is a tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An ClParams Discriminated Union with the following cases:
    /// | Intersect of parameterA:float * parameterB:float
    /// | Apart of parameterA:float * parameterB:float
    /// | Parallel of parameterA:float * parameterB:float
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    /// <remarks>Alternatively you can use `closestParameters` for a simpler interface that returns a tuple of the closest parameters.</remarks>
    let getClosestParameters (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, tangent:float<Tangent.tangent>,tooShortTolerance:float): ClParams =
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
            // division-free parallel test: |det| / |dot| > tangent  <=>  |det| > tangent · |dot| (handles NaN/zero-length too)
            if !^ (abs det) > tangent * abs dot then
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let t = (dx * vBy - dy * vBx) / det // embrace float math, this can be NaN or Infinity
                let u = (dx * vAy - dy * vAx) / det // embrace float math, this can be NaN or Infinity
                if t > -1e-6 && t < ``1.0 + 1e-6`` && u > -1e-6 && u < ``1.0 + 1e-6`` then
                    ClParams.Intersect (t, u)
                else
                    // Skew lines, but the infinite-line crossing lies outside at least one segment.
                    // Closest pair sits at a clamped endpoint -> clamp-and-recompute (Ericson §5.1.9).
                    let sqLenA = vAx * vAx + vAy * vAy
                    let sqLenB = vBx * vBx + vBy * vBy
                    let c = -(vAx * dx + vAy * dy)  // vA·(pA-pB) ; dx,dy are pB-pA, hence the minus
                    let f = -(vBx * dx + vBy * dy)  // vB·(pA-pB)
                    let denom = det * det           // = a*e - b² (Lagrange identity in 2D), > 0 here
                    let mutable sa = clamp01 ((dot * f - c * sqLenB) / denom)
                    let mutable sb = (dot * sa + f) / sqLenB
                    if sb < 0.0 then
                        sb <- 0.0;
                        sa <- clamp01 (-c / sqLenA)
                    elif sb > 1.0 then
                        sb <- 1.0;
                        sa <- clamp01 ((dot - c) / sqLenA)
                    ClParams.Apart (sa, sb)
            else
                // Parallel within tolerance: minimum-distance solutions span an interval on A.
                // Return the midpoint of the overlapping segment.
                let sqLenA = vAx * vAx + vAy * vAy
                let sqLenB = vBx * vBx + vBy * vBy
                let wx = pBx - pAx
                let wy = pBy - pAy
                let sB0 = (wx * vAx + wy * vAy) / sqLenA          // pB      projected onto line A
                let sB1 = sB0 + dot / sqLenA                      // pB + vB projected onto line A (vB·vA = dot, already computed)
                let lo = max 0.0 (min sB0 sB1)               // overlap interval on A, intersected with [0,1]
                let hi = min 1.0 (max sB0 sB1)
                let sa = clamp01 ((lo + hi) * 0.5)           // midpoint of overlap; clamp covers the no-overlap case
                let cx = pAx + sa * vAx - pBx                // perpendicular foot of A(sa) ...
                let cy = pAy + sa * vAy - pBy
                let sb = clamp01 ((cx * vBx + cy * vBy) / sqLenB) // ... onto line B
                ClParams.Parallel (sa, sb)


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
    /// <param name="tangent"> This is a tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > The length tolerance.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An ClPts Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Apart of Pt * Pt
    /// | Parallel of Pt * Pt
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth
    /// </returns>
    /// <remarks> This method uses a small tolerance of 1e-6 to account for numerical inaccuracies when checking the 0..1 range.</remarks>
    let getClosestPoints (pAx:float, pAy:float, pBx:float, pBy:float, vAx:float, vAy:float, vBx:float, vBy:float, tangent:float<Tangent.tangent>, tooShortTolerance:float ): ClPts =
        match getClosestParameters (pAx, pAy, pBx, pBy, vAx, vAy, vBx, vBy, tangent, tooShortTolerance) with
        | ClParams.TooShortA -> ClPts.TooShortA
        | ClParams.TooShortB -> ClPts.TooShortB
        | ClParams.TooShortBoth -> ClPts.TooShortBoth
        | ClParams.Intersect (sa, _) ->
            let x = pAx + sa * vAx
            let y = pAy + sa * vAy
            ClPts.Intersect (Pt(x,y))
        | ClParams.Apart (sa, sb) ->
            let ax = pAx + sa * vAx
            let ay = pAy + sa * vAy
            let bx = pBx + sb * vBx
            let by = pBy + sb * vBy
            ClPts.Apart (Pt(ax,ay), Pt(bx,by))
        | ClParams.Parallel (sa, sb) ->
            let ax = pAx + sa * vAx
            let ay = pAy + sa * vAy
            let bx = pBx + sb * vBx
            let by = pBy + sb * vBy
            ClPts.Parallel (Pt(ax,ay), Pt(bx,by))



// #endregion
// #region XLine2D

open XLine2D
open UtilEuclid

/// A type containing only static member functions for computing 2D line intersections.
/// Some functions return Discriminated Unions from the XLine2D module.
[<RequireQualifiedAccess>]
type XLine2D =


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
        XLineXY.parameterA (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Returns the intersection parameter on the first ray (A) where ray A and ray B intersect.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The parameter on lineA at the intersection with lineB.
    ///  NaN if both start points coincide and vectors are exactly parallel (0.0/0.0).
    ///  Positive/Negative Infinity for almost parallel lines.</returns>
    static member inline parameterA (lineA: Line2D, lineB: Line2D): float =
        XLineXY.parameterA ( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )


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
        XLineXY.parameters (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Returns the intersection parameters on both rays.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their intersection.
    ///  These are NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  These are positive or negative Infinity for almost parallel lines.</returns>
    static member inline parameters(lineA: Line2D, lineB: Line2D): float*float =
        XLineXY.parameters( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )



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
        XLineXY.doIntersect (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Checks if two 2D-lines intersect within the parameter range 0.0 to 1.0 (with 1e-6 tolerance) on both lines.</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <returns> True if both lines have an intersection parameter in the range 0.0 to 1.0 (with 1e-6 tolerance).
    ///  False if the lines are parallel, coincident or at least one intersection parameter is outside of the range.
    /// </returns>
    /// <remarks> This does not check for an explicit angle tolerance, so overlapping almost parallel lines may return true.</remarks>
    static member inline doIntersect (lineA: Line2D, lineB: Line2D): bool =
        XLineXY.doIntersect( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )


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
        XLineXY.isWithinRanges (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, minParameterA, maxParameterA, minParameterB, maxParameterB)

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
        XLineXY.isWithinRanges( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, minParameterA, maxParameterA, minParameterB, maxParameterB )


    /// <summary> Checks if 2D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="pA"> The start point on the first line.</param>
    /// <param name="pB"> The start point on the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="tolerance" > Is an optional distance tolerance (compared against the squared distance internally). 1e-6 by default.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    static member inline doOverlap(pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(1e-6)>] tolerance:float ) : bool =
        XLineXY.doOverlap(pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tolerance)

    /// <summary> Checks if 2D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <param name="tolerance" > Is an optional distance tolerance (compared against the squared distance internally). 1e-6 by default.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    static member inline doOverlap(lineA:Line2D, lineB:Line2D, [<OPT;DEF(1e-6)>] tolerance:float ) : bool =
        XLineXY.doOverlap( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tolerance )



    /// <summary> Gets the square distance between two finite lines. Works on Parallel lines too.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <returns> The squared distance between the two lines.</returns>
    static member inline getSqDistance(pA:Pt, pB:Pt, vA:Vc, vB:Vc ): float =
        XLineXY.sqDistance (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Gets the square distance between two finite lines. Works on Parallel lines too.</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <returns> The squared distance between the two lines.</returns>
    static member inline getSqDistance(lineA: Line2D, lineB: Line2D ): float =
        XLineXY.sqDistance (lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )

    /// <summary> Intersects two infinite 2D lines. Fails if they are parallel or zero length.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The intersection point of the two rays.</returns>
    static member inline intersectRays (pA:Pt, pB:Pt, vA:Vc, vB:Vc): Pt =
        XLineXY.intersectRays (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Intersects two infinite 2D lines. Fails if they are parallel or zero length.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The intersection point of the two rays.</returns>
    static member inline intersectRays (lineA: Line2D, lineB: Line2D): Pt =
        XLineXY.intersectRays (lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )

    /// <summary> Gets the parameters on the finite lines where the lines are closest to each other.
    /// For parallel or overlapping lines, there are many solutions, The parameter pair closest to 0.0 on line A is returned.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their closest points.</returns>
    static member closestParameters (pA:Pt, pB:Pt, vA:Vc, vB:Vc): struct(float*float) =
        XLineXY.closestParameters (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Gets the parameters on the finite lines where the lines are closest to each other.
    /// For parallel or overlapping lines, there are many solutions, The parameter pair closest to 0.0 on line A is returned.</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their closest points.</returns>
    static member closestParameters (lineA: Line2D, lineB: Line2D): struct(float*float) =
        XLineXY.closestParameters (lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )



    // #endregion
    // #region Options


    /// <summary> Returns the intersection parameter on the first ray (A) where ray A and ray B intersect.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The parameter on line A at the intersection with line B if it is smaller than 1e12 in absolute value,
    /// or None if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryParameterA (pA:Pt, pB:Pt, vA:Vc, vB:Vc): float voption =
        XLineXY.tryParameterA (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Returns the intersection parameter on the first ray (A) where ray A and ray B intersect.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The parameter on lineA at the intersection with lineB if it is smaller than 1e12 in absolute value,
    /// or None if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryParameterA (lineA:Line2D, lineB:Line2D): float voption =
        XLineXY.tryParameterA (lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )


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
    static member inline tryIntersectInRangeA (pA:Pt, pB:Pt, vA:Vc, vB:Vc, minParameterA, maxParameterA): Pt voption =
        XLineXY.tryIntersectInRangeA (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, minParameterA, maxParameterA)

    /// <summary> Returns the intersection point of two rays if it is within the given parameter range for line A.</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <param name="minParameterA"> The minimum parameter value of the range, inclusive.</param>
    /// <param name="maxParameterA"> The maximum parameter value of the range, inclusive.</param>
    /// <returns> A point if lineA has an intersection parameter in the range (inclusive min and max value).
    /// None if the lines are parallel, coincident or the intersection parameter is outside of the range.
    /// </returns>
    static member inline tryIntersectInRangeA (lineA: Line2D, lineB: Line2D, minParameterA, maxParameterA): Pt voption =
        XLineXY.tryIntersectInRangeA( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, minParameterA, maxParameterA )


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
    static member inline tryIntersectInRanges (pA:Pt, pB:Pt, vA:Vc, vB:Vc, minParameterA, maxParameterA, minParameterB, maxParameterB): Pt voption =
        XLineXY.tryIntersectInRanges (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, minParameterA, maxParameterA, minParameterB, maxParameterB)

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
    static member inline tryIntersectInRanges (lineA: Line2D, lineB: Line2D, minParameterA, maxParameterA, minParameterB, maxParameterB): Pt voption =
        XLineXY.tryIntersectInRanges( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, minParameterA, maxParameterA, minParameterB, maxParameterB )


    /// <summary> Returns the intersection point of two finite line segments if they intersect.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> Vector from pA to the other end of line A (its length defines the domain 0..1).</param>
    /// <param name="vB"> Vector from pB to the other end of line B (its length defines the domain 0..1).</param>
    /// <returns> The intersection point if both parameters are within 0.0 to 1.0;
    /// None otherwise or if parallel/coincident.</returns>
    /// <remarks> This method uses a small tolerance of 1e-6 to account for numerical inaccuracies when checking the 0..1 range.</remarks>
    static member inline tryIntersect (pA:Pt, pB:Pt, vA:Vc, vB:Vc): Pt voption =
        XLineXY.tryIntersect (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y)

    /// <summary> Returns the intersection point of two finite line segments if they intersect.</summary>
    /// <param name="lineA"> First finite line segment.</param>
    /// <param name="lineB"> Second finite line segment.</param>
    /// <returns> The intersection point if both parameters are within 0.0 to 1.0;
    /// None otherwise or if parallel/coincident.</returns>
    /// <remarks> This method uses a small tolerance of 1e-6 to account for numerical inaccuracies when checking the 0..1 range.</remarks>
    static member inline tryIntersect (lineA: Line2D, lineB: Line2D): Pt voption =
        XLineXY.tryIntersect( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY )


    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <returns> The point at which the two rays intersect or None.</returns>
    /// <remarks> If the lines are parallel or coincident (within the tangent tolerance) or too short, None is returned.</remarks>
    static member inline tryIntersectRay (pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>): Pt voption =
        XLineXY.tryIntersectRay (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent)

    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <returns> The point at which the two rays intersect or None.</returns>
    /// <remarks> If the lines are parallel or coincident (within the tangent tolerance), or too short, None is returned.</remarks>
    static member inline tryIntersectRay (lineA: Line2D, lineB: Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>): Pt voption =
        XLineXY.tryIntersectRay (lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent)



    // #endregion
    // #region Discriminated Unions



    /// <summary> Tries to get intersection parameters of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then a 'TooShort' union case is returned.</param>
    /// <returns> An XRayParam discriminated union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    static member inline getRayIntersectionParam(pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRayParam =
        XLineXY.getRayIntersectionParam (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

    /// <summary> Tries to get intersection parameters of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XRayParam discriminated union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortA
    /// | TooShortB
    /// | TooShortBoth </returns>
    static member inline getRayIntersectionParam(lineA: Line2D, lineB: Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRayParam =
        XLineXY.getRayIntersectionParam( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance )



    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XRay Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getRayIntersection(pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRay =
        XLineXY.getRayIntersection(pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XRay Discriminated Union with the following cases:
    /// | Intersect of Pt
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getRayIntersection(lineA: Line2D, lineB: Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRay =
        XLineXY.getRayIntersection( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance )




    /// <summary> Tries to get intersection parameters of two finite 2D-lines.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XParam Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersectionParam(pA:Pt, pB:Pt, vA:Vc, vB:Vc, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XParam =
        XLineXY.getIntersectionParam (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

    /// <summary> Tries to get intersection parameters of two finite 2D-lines.</summary>
    /// <param name="lnA"> First line.</param>
    /// <param name="lnB"> Second line.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XParam Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersectionParam(lnA: Line2D, lnB: Line2D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XParam =
        XLineXY.getIntersectionParam( lnA.FromX, lnA.FromY, lnB.FromX, lnB.FromY, lnA.VectorX        , lnA.VectorY        , lnB.VectorX        , lnB.VectorY        , tangent, tooShortTolerance )



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
        XLineXY.getIntersection(pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

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
        XLineXY.getIntersection( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance )



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
        XLineXY.getClosestParameters (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

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
        XLineXY.getClosestParameters ( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance)




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
        XLineXY.getClosestPoints (pA.X, pA.Y, pB.X, pB.Y, vA.X, vA.Y, vB.X, vB.Y, tangent, tooShortTolerance)

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
        XLineXY.getClosestPoints ( lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, lineA.VectorX, lineA.VectorY, lineB.VectorX, lineB.VectorY, tangent, tooShortTolerance)


    // #endregion
    // #region XEndsTouching


    /// <summary>Checks if the two finite 2D lines are touching each other at exactly one of their end points
    /// within the given tolerance.</summary>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <param name="tolerance"> Is an optional distance tolerance. 1e-6 by default.</param>
    /// <returns>A Discriminated Union Line2D.XEnds that describes the possible cases of two finite 2D lines touching at their ends:
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

