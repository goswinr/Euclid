
namespace Euclid

open System
open UtilEuclid
open EuclidErrors

// #region types

/// A module containing the result types for 3D Line-Line-Intersections,
/// 3D Line-Line relationship queries,
/// and Line-Cone intersections.
module XLine3D =



    /// Result of intersecting an infinite line with one nappe of an infinite cone
    /// (only the part of the cone on the base side of the tip, not the mirrored double cone).
    /// All parameters are in the parametrization of the line: t=0 at ray.From, t=1 at ray.To,
    /// values outside 0..1 (including negative ones) lie on the infinite extension.
    [<RequireQualifiedAccess>]
    type XCone =
        /// The line misses the cone entirely.
        /// (This includes lines that would only hit the mirrored upper nappe of the double cone.)
        | NoIntersection

        /// The line crosses the cone surface at two distinct points, both on the lower nappe.
        /// Contains both parameters on the line, sorted ascending.
        | Intersecting of float * float

        /// The line crosses the cone surface transversally at exactly one point of the lower nappe.
        /// This happens when the second root of the quadratic lies on the mirrored upper nappe,
        /// or when the line is parallel to a generator line of the cone (the quadratic degenerates to linear).
        /// Unlike 'Touching' this is a TRUE crossing from outside to inside.
        | IntersectingOne of float

        /// The line touches the cone surface tangentially at exactly one point of the lower nappe.
        /// (Double root of the quadratic, touch point is not the tip.)
        | Touching of float

        /// The line passes through the tip (apex) point only and is not on the cone surface.
        | ThroughTip of float

        /// The line lies on the cone surface: it is a generator line through the tip.
        /// Since only the lower nappe counts as the cone, just the half-line starting
        /// at the tip lies on the cone:
        /// tipParam is the parameter of the tip on the line.
        /// If onConeTowardsPositiveT is TRUE the on-cone half is  t >= tipParam,  otherwise  t <= tipParam.
        | LineOnCone of tipParam:float * onConeTowardsPositiveT:bool


    /// Describes the possible relationships of two rays (rays are 3D lines extended infinitely in both directions).
    /// Returns parameters on both lines if they intersect or are skew.
    [<RequireQualifiedAccess>]
    type XRayParam =

        /// The rays are intersecting (truly coplanar) or skew.
        /// They have each one point where they are touching each other (or are closest to each other in the skew case).
        /// Contains the parameters on the first and second ray.
        /// In the skew case, these parameters indicate the closest approach points.
        | SkewOrX of twoParams : float*float

        /// The lines are parallel, within the given tolerance.
        /// They have infinity many or no points in common.
        | Parallel

        /// Only Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Only Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth


    /// Describes the possible relationships of two rays (rays are 3D lines extended infinitely in both directions).
    /// Returns the intersection or closest point if they intersect or are skew.
    [<NoEquality; NoComparison>]
    [<RequireQualifiedAccess>]
    type XRay =

        /// The rays (3d-lines extended infinitely) are intersecting in one well defined point (coplanar case).
        | Intersect of xPt : Pnt

        /// The rays are skew (not parallel, not intersecting).
        /// Contains the closest points on both rays and the squared distance between them.
        | Skew of closestPtA : Pnt *  closestPtB : Pnt * squareDist : float

        /// The lines are parallel, within the given tolerance.
        /// They have infinity many or no points in common.
        | Parallel

        /// Only Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Only Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth



    /// Describes the possible relationships of two finite 3D lines.
    /// Returns parameters on both lines if they intersect or are skew.
    [<NoEquality; NoComparison>]
    [<RequireQualifiedAccess>]
    type XParam =

        /// The finite lines are intersecting in one well defined point (coplanar case).
        | Intersect of twoParams : float*float

        /// The finite 3D lines are skew (not parallel, not coplanar).
        /// Their closest approach is within the finite line segments.
        | Skew of paramA:float * paramB:float * squareDist:float

        /// The finite 3D lines do not touch each other and their closest approach is outside their finite definition.
        /// However they are not parallel. XParam.Parallel is used when they are parallel.
        | Apart

        /// The lines are parallel, within the given tolerance.
        /// They have infinity many or no points in common.
        | Parallel

        /// Only Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Only Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth

    /// Describes the possible relationships of two finite 3D lines.
    /// Returns the intersection or closest point if they intersect or are skew.
    [<NoEquality; NoComparison>]
    [<RequireQualifiedAccess>]
    type XPnt =

        /// The finite lines are intersecting in one well defined point (coplanar case).
        | Intersect of xPnt : Pnt

        /// The finite 3D lines are skew (not parallel, not coplanar).
        /// Contains the closest points on both lines and the squared distance between them.
        | Skew of closestPntA : Pnt * closestPntB : Pnt * squareDist : float

        /// The finite 3D lines do not touch each other.
        /// Their intersection point (or closest approach) is outside of their finite definition.
        /// However they are not parallel. XPt.Parallel is used when they are parallel.
        | Apart

        /// The lines are parallel, within the given tolerance.
        /// They have infinity many or no points in common.
        | Parallel

        /// Only Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Only Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth




    /// Describes the possible cases of the closest parameters between finite 3D lines.
    [<RequireQualifiedAccess>]
    type ClParams =

        /// The lines intersect. Contains the parameters at the intersection point for both lines.
        /// These parameters are in the range 0.0 to 1.0
        | Intersect of paramA:float * paramB:float

        /// The lines are skew (not parallel, not coplanar). Contains the parameters and squared distance at their closest approach.
        /// These parameters are in the range 0.0 to 1.0
        | Skew of paramA:float * paramB:float * squareDist:float

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


    /// Describes the possible cases of the closest points between finite 3D lines.
    [<NoEquality; NoComparison>]
    [<RequireQualifiedAccess>]
    type ClPts =

        /// The lines intersect. Contains the intersection point.
        | Intersect of pt:Pnt

        /// The lines are skew (not parallel, not coplanar). Contains the closest points on both lines and the squared distance between them.
        | Skew of ptA:Pnt * ptB:Pnt * squareDist:float

        /// The lines are parallel or coincident. Contains the points at the middle of any overlapping segment on both lines.
        | Parallel of ptA:Pnt * ptB:Pnt

        /// The lines are apart. Contains the closest points on both lines and the squared distance between them.
        | Apart of ptA:Pnt * ptB:Pnt * squareDist:float

        /// Line A is shorter than the given minimum length tolerance.
        | TooShortA

        /// Line B is shorter than the given minimum length tolerance.
        | TooShortB

        /// Both Lines are shorter than the given minimum length tolerance.
        | TooShortBoth


    /// Describes the possible cases of two finite 3D lines touching at their ends.
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

        /// Both endpoints touch the opposite endpoints; the lines are identical but oppositely oriented.
        | IdenticalFlipped


// #endregion
// #region XLineXYZ

module XLineXYZ =

    open XLine3D

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



    /// squares a float value
    let inline internal sq (x:float) : float = x * x


    /// Returns the parameter of the closest point on an infinite ray.
    /// May be NaN or infinity if the line vector is zero length
    let inline internal clParamRayPt(pAx:float, pAy:float, pAz:float, vAx:float, vAy:float, vAz:float, x:float, y:float, z:float) : float =
        // project point onto line A
        // vector from A to point
        let u = x - pAx
        let v = y - pAy
        let w = z - pAz
        // dot product of vector with A's direction:
        let dotV = vAx*u + vAy*v + vAz*w
        let lenSq = vAx*vAx + vAy*vAy + vAz*vAz
        // parameter t of the closest point on line A to point
        dotV / lenSq

    /// Returns the parameter of the closest point on a finite line.
    /// May be NaN or infinity if the line vector is zero length
    let inline internal clParamLnPt(ln:Line3D, x:float, y:float, z:float) : float =
        clParamRayPt(ln.FromX, ln.FromY, ln.FromZ, ln.VectorX        , ln.VectorY        , ln.VectorZ        , x, y, z) |> clamp01

    /// Returns the squared distance of a point to an infinite ray.
    /// May be NaN or infinity if the line vector is zero length
    let inline internal sqRayPtDist(pAx:float, pAy:float, pAz:float, vAx:float, vAy:float, vAz:float, x:float, y:float, z:float) : float =
        // parameter t of the closest point on line A to point
        let t = clParamRayPt(pAx, pAy, pAz, vAx, vAy, vAz, x, y, z)
        let clPtX = pAx + vAx * t
        let clPtY = pAy + vAy * t
        let clPtZ = pAz + vAz * t
        // vector from closest point to point
        let vx = clPtX - x
        let vy = clPtY - y
        let vz = clPtZ - z
        vx*vx + vy*vy + vz*vz


    /// Returns the closest point to a finite line.
    let inline internal clPtLn(pAx:float, pAy:float, pAz:float, vAx:float, vAy:float, vAz:float, x:float, y:float, z:float) : Pnt =
        // parameter t of the closest point on line A to point
        let t = clParamRayPt(pAx, pAy, pAz, vAx, vAy, vAz, x, y, z)
        if t > -1e-6 then // to handle NaN too
            if t < ``1.0 + 1e-6`` then
                let clPtX = pAx + vAx * t
                let clPtY = pAy + vAy * t
                let clPtZ = pAz + vAz * t
                Pnt(clPtX, clPtY, clPtZ)
            else
                Pnt(pAx + vAx , pAy + vAy , pAz + vAz )
        else // case x <= 0.0 or x = NaN
            Pnt(pAx, pAy, pAz)


    /// Returns the closest point to a finite line.
    let inline internal clPtLn'(ln:Line3D, x:float, y:float, z:float) : Pnt =
        clPtLn(ln.FromX, ln.FromY, ln.FromZ, ln.VectorX        , ln.VectorY        , ln.VectorZ        , x, y, z)


    /// Returns the squared distance of a point to a finite line.
    let inline internal sqDistLnPt(pAx:float, pAy:float, pAz:float, vAx:float, vAy:float, vAz:float, x:float, y:float, z:float) : float =
        // parameter t of the closest point on line A to point
        let t = clParamRayPt(pAx, pAy, pAz, vAx, vAy, vAz, x, y, z) |> clamp01
        if t > -1e-6 then // to handle NaN too
            if t < ``1.0 + 1e-6`` then
                let clPtX = pAx + vAx * t
                let clPtY = pAy + vAy * t
                let clPtZ = pAz + vAz * t
                // vector from closest point to point
                let vx = clPtX - x
                let vy = clPtY - y
                let vz = clPtZ - z
                vx*vx + vy*vy + vz*vz
            else
                // vector from end point to point
                let vx = (pAx + vAx) - x
                let vy = (pAy + vAy) - y
                let vz = (pAz + vAz) - z
                vx*vx + vy*vy + vz*vz
        else // case x <= 0.0 or x = NaN
            // vector from start point to point
            let vx = pAx - x
            let vy = pAy - y
            let vz = pAz - z
            vx*vx + vy*vy + vz*vz

    /// Returns the squared distance of a point to a finite line.
    let inline internal sqDistLnPt'(ln:Line3D, x:float, y:float, z:float) : float =
        sqDistLnPt(ln.FromX, ln.FromY, ln.FromZ, ln.VectorX        , ln.VectorY        , ln.VectorZ        , x, y, z)

    /// Returns the squared distance between the start points of two finite lines.
    let inline internal sqDistLnFromLnFrom(a:Line3D, b:Line3D) : float =
        let dx = a.FromX - b.FromX
        let dy = a.FromY - b.FromY
        let dz = a.FromZ - b.FromZ
        dx*dx + dy*dy + dz*dz


    /// Checks if the line vector is shorter than the given tolerance.
    /// It uses the manhattan distance for the check.
    /// So the cutoff is at 58% to 100% of the tooShortTolerance length.
    let inline internal isTooShort(x:float, y:float, z:float, tooShortTolerance:float) : bool =
        (abs x + abs y + abs z) < tooShortTolerance


    /// returns the tangent between two lines.
    /// This is the magnitude of the cross-product divided by the dot-product of the two line vectors.
    /// Returns NaN if one of the line vectors is zero length.
    let inline internal tangent (vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): float<Tangent.tangent> =
        // Cross product of vectors vA × vB
        let crossX = vAy * vBz - vAz * vBy
        let crossY = vAz * vBx - vAx * vBz
        let crossZ = vAx * vBy - vAy * vBx
        let crossMag = sqrt(crossX * crossX + crossY * crossY + crossZ * crossZ)
        let dot = vAx * vBx + vAy * vBy + vAz * vBz // Dot product of vectors
        crossMag / dot // tangent of the angle between the two vectors, always positive
        |> LanguagePrimitives.FloatWithMeasure<Tangent.tangent>


    /// <summary> Returns the closest approach parameter on the first ray (A) where ray A and ray B come closest.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vAz"> The Z component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="vBz"> The Z component of the vector of the second ray.</param>
    /// <returns> The parameter at which the two rays intersect (or come closest) on line A.
    ///  This is NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  This is positive or negative Infinity for almost parallel lines.</returns>
    let inline parameterA (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                     vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): float =
        // Cross product of vectors vA × vB
        let crossX = vAy * vBz - vAz * vBy
        let crossY = vAz * vBx - vAx * vBz
        let crossZ = vAx * vBy - vAy * vBx
        // Magnitude squared of cross product (if zero, lines are parallel)
        let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ
        // Difference in start points
        let dx = pBx - pAx
        let dy = pBy - pAy
        let dz = pBz - pAz
        // Cross product of difference vector with vB: (pB - pA) × vB
        let numerX = dy * vBz - dz * vBy
        let numerY = dz * vBx - dx * vBz
        let numerZ = dx * vBy - dy * vBx
        // Dot product of (pB - pA) × vB with vA × vB
        let numerator = numerX * crossX + numerY * crossY + numerZ * crossZ
        numerator / crossMagSq





    /// <summary> Returns the closest approach parameters on both rays A and B.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vAz"> The Z component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="vBz"> The Z component of the vector of the second ray.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their closest approach.
    ///  These are NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  These are positive or negative Infinity for almost parallel lines.</returns>
    let inline parameters (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                           vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): float*float =
        // Cross product of vectors vA × vB
        let crossX = vAy * vBz - vAz * vBy
        let crossY = vAz * vBx - vAx * vBz
        let crossZ = vAx * vBy - vAy * vBx
        // Magnitude squared of cross product
        let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ
        // Difference in start points
        let dx = pBx - pAx
        let dy = pBy - pAy
        let dz = pBz - pAz
        // For parameter t on line A: ((pB - pA) × vB) · (vA × vB)
        let numerX_t = dy * vBz - dz * vBy
        let numerY_t = dz * vBx - dx * vBz
        let numerZ_t = dx * vBy - dy * vBx
        let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
        let t = numerator_t / crossMagSq
        // For parameter u on line B: ((pB - pA) × vA) · (vA × vB)
        let numerX_u = dy * vAz - dz * vAy
        let numerY_u = dz * vAx - dx * vAz
        let numerZ_u = dx * vAy - dy * vAx
        let numerator_u = numerX_u * crossX + numerY_u * crossY + numerZ_u * crossZ
        let u = numerator_u / crossMagSq
        t, u







    /// <summary> Checks if 3D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vAz"> The Z component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="vBz"> The Z component of the vector of the second line.</param>
    /// <param name="tolerance" > Is a distance tolerance.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    /// <returns> TRUE if the lines are coincident and overlap or touch.
    ///  FALSE if the lines are not coincident or do not overlap nor touch.</returns>
    let doOverlap(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                  vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                  tolerance:float ) : bool =
        // parameter t of the closest point on line A to point B
        let t = clParamRayPt(pAx, pAy, pAz, vAx, vAy, vAz, pBx, pBy, pBz)
        let clPtX = pAx + vAx * t
        let clPtY = pAy + vAy * t
        let clPtZ = pAz + vAz * t
        // vector from closest point to B
        let vx = clPtX - pBx
        let vy = clPtY - pBy
        let vz = clPtZ - pBz
        let dist = vx*vx + vy*vy + vz*vz
        let sqTolerance = tolerance * tolerance
        if dist > sqTolerance then
            false
        else
            let bex = pBx + vBx
            let bey = pBy + vBy
            let bez = pBz + vBz
            let u = clParamRayPt(pAx, pAy, pAz, vAx, vAy, vAz, bex, bey, bez)
            let cluPtx = pAx + vAx * u
            let cluPty = pAy + vAy * u
            let cluPtz = pAz + vAz * u
            // vector from closest point to B end point
            let vx2 = cluPtx - bex
            let vy2 = cluPty - bey
            let vz2 = cluPtz - bez
            let dist2 = vx2*vx2 + vy2*vy2 + vz2*vz2
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




    /// <summary> Checks if two 3D-rays actually intersect within a given maximum skew distance.
    /// Returns FALSE if rays are parallel or even identical, or if input lines are of zero length.
    /// Use XLine3D.getRayIntersection to check for those cases too.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vAz"> The Z component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="vBz"> The Z component of the vector of the second ray.</param>
    /// <param name="maxSkewDistance" > Is a maximum distance tolerance.
    /// Used for checking if the distance between the two lines at their closest approach is less than this value.</param>
    /// <returns> TRUE if both rays intersect within the given maximum skew distance.
    ///  FALSE if the rays are parallel, coincident or the closest approach distance is larger than the given tolerance.</returns>
    let inline doRaysIntersect (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                maxSkewDistance:float): bool =
        let tA = parameterA(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)
        if tA > -1e12 && tA < 1e12  then
            let clPtAx =  pAx + tA * vAx
            let clPtAy =  pAy + tA * vAy
            let clPtAz =  pAz + tA * vAz
            let d = sqRayPtDist(pBx, pBy, pBz, vBx, vBy, vBz, clPtAx, clPtAy, clPtAz)
            d < maxSkewDistance * maxSkewDistance
        else
            false




    /// <summary> Checks if two non zero finite 3D lines intersect within a given tolerance.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vAz"> The Z component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="vBz"> The Z component of the vector of the second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, FALSE is returned.</param>
    /// <returns> TRUE if the lines intersect within the given tolerance, FALSE if the segments are skew, parallel, or too short.</returns>
    let doIntersect(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                    vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                    maxSkewDistance:float,
                    tangent:float<Tangent.tangent>,
                    tooShortTolerance:float
                    ) : bool =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            // TODO or return true if a zero length line is exactly on the another one ?
            false
        elif isTooShort(vBx, vBy, vBz, tooShortTolerance) then
            false
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ // squared magnitude of vA × vB
            let dot = vAx * vBx + vAy * vBy + vAz * vBz
            // parallel test without sqrt or division: |vA×vB| / |dot| > tangent  <=>  crossMagSq > tangent² · dot²
            if !^ crossMagSq > tangent * tangent * dot * dot then // handles the NaN/zero-length case correctly too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy
                let dz = pBz - pAz
                let dvA = dx * vAx + dy * vAy + dz * vAz // (pB-pA)·vA
                let dvB = dx * vBx + dy * vBy + dz * vBz // (pB-pA)·vB
                let sqLenB = vBx * vBx + vBy * vBy + vBz * vBz // |vB|²
                let t = (dvA * sqLenB - dvB * dot) / crossMagSq // numerator via Lagrange identity = (d×vB)·(vA×vB)
                if t > -1e-6 && t < 1.0 + 1e-6 then
                    let sqLenA = vAx * vAx + vAy * vAy + vAz * vAz // |vA|²
                    let u = (dvA * dot - dvB * sqLenA) / crossMagSq // numerator via Lagrange identity = (d×vA)·(vA×vB)
                    if u > -1e-6 && u < 1.0 + 1e-6 then
                        let ptAx = pAx + t * vAx
                        let ptAy = pAy + t * vAy
                        let ptAz = pAz + t * vAz
                        let ptBx = pBx + u * vBx
                        let ptBy = pBy + u * vBy
                        let ptBz = pBz + u * vBz
                        let dx2 = ptAx - ptBx
                        let dy2 = ptAy - ptBy
                        let dz2 = ptAz - ptBz
                        let sqDist = dx2*dx2 + dy2*dy2 + dz2*dz2
                        if sqDist < maxSkewDistance*maxSkewDistance then
                            true
                        else
                            false
                    else
                        false
                else
                    false
            else
                false





    /// <summary> Gets the squared distance between two finite lines. Works on parallel and skew lines too.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vAz"> The Z component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="vBz"> The Z component of the vector of the second line.</param>
    /// <returns> the squared distance between the two lines </returns>
    let sqDistance(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float ): float =
        // Ericson's parametric segment–segment routine (Real-Time Collision Detection, §5.1.9).
        let inline distToAStart tb = // takes parameter on vec B
            // closest point on B is cB = pB + tb·vB, distance to pA
            let dx = pAx - pBx - tb*vBx
            let dy = pAy - pBy - tb*vBy
            let dz = pAz - pBz - tb*vBz
            dx*dx + dy*dy + dz*dz

        let inline distToBStart ta = // takes parameter on vec A
            let dx = pAx + ta*vAx - pBx
            let dy = pAy + ta*vAy - pBy
            let dz = pAz + ta*vAz - pBz
            dx*dx + dy*dy + dz*dz

        let inline distToBEnd ta = // takes parameter on vec A
            // closest point on B is its end cB = pB + vB, distance to A(ta)
            let dx = pAx + ta*vAx - pBx - vBx
            let dy = pAy + ta*vAy - pBy - vBy
            let dz = pAz + ta*vAz - pBz - vBz
            dx*dx + dy*dy + dz*dz

        let inline distOnLine ta tb =
            // closest points: cA = pA + ta·vA, cB = pB + tb·vB
            let dx = pAx + ta*vAx - pBx - tb*vBx
            let dy = pAy + ta*vAy - pBy - tb*vBy
            let dz = pAz + ta*vAz - pBz - tb*vBz
            dx*dx + dy*dy + dz*dz

        // r = pA - pB  (vector from start of B to start of A)
        let rx = pAx - pBx
        let ry = pAy - pBy
        let rz = pAz - pBz
        let a = vAx*vAx + vAy*vAy + vAz*vAz   // squared length of A,  vA·vA   (>= 0)
        let e = vBx*vBx + vBy*vBy + vBz*vBz   // squared length of B,  vB·vB   (>= 0)
        let f = vBx*rx + vBy*ry + vBz*rz      // vB·r
        // Tolerance for treating a segment as a degenerate point.
        // It is compared against *squared* lengths, so this ~= (1e-6)^2.
        let tolSq = 1e-12

        if a < tolSq && e < tolSq then
            rx*rx + ry*ry + rz*rz // both segments are points: distance between them
        elif a <= tolSq then
            // A is a point: closest point on B to pA, t = f/e
            let tb = max 0.0 (min 1.0 (f / e))
            distToAStart tb
        else
            let c = vAx*rx + vAy*ry + vAz*rz    // vA·r
            if e < tolSq then
                // B is a point: closest point on A to pB, s = -c/a
                let ta = max 0.0 (min 1.0 (-c / a))
                distToBStart ta
            else
                let mutable ta = 0.0
                let mutable tb = 0.0
                let b = vAx*vBx + vAy*vBy + vAz*vBz // vA·vB
                let denom = a*e - b*b     // = |vA×vB|² (Lagrange identity), 0 if parallel
                if denom <> 0.0 then
                    // The denom <> 0.0 exact-zero test is deliberately not a tolerance check.
                    // For merely near-parallel segments, dividing by a tiny denom can throw ta way out of range,
                    // but the subsequent clamp-and-recompute steps pull it back to the correct endpoint — so the exact test is actually the robust choice here.
                    ta <- max 0.0 (min 1.0 ((b*f - c*e) / denom))

                // else parallel: keep ta = 0, the tb-clamp below recovers the correct distance
                tb <- (b*ta + f) / e  // closest point on line B to A(ta)
                if tb <= 0.0 then
                    let ta = max 0.0 (min 1.0 (-c / a))
                    distToBStart ta
                elif tb >= 1.0 then
                    let ta = max 0.0 (min 1.0 ((b - c) / a))
                    distToBEnd ta
                else
                    distOnLine ta tb



// #endregion
// #region Option


    /// <summary> Tries to get an actual intersection point of two finite 3D lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vAz"> The Z component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="vBz"> The Z component of the vector of the second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, ValueNone is returned.</param>
    /// <returns> An intersection point on line A, or ValueNone if the segments are skew, parallel, or too short.</returns>
    let tryIntersect(  pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                       vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                       maxSkewDistance:float,
                       tangent:float<Tangent.tangent>,
                       tooShortTolerance:float
                       ) : Pnt voption =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            ValueNone
        elif isTooShort(vBx, vBy, vBz, tooShortTolerance) then
            ValueNone
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ // squared magnitude of vA × vB
            let dot = vAx * vBx + vAy * vBy + vAz * vBz
            // parallel test without sqrt or division: |vA×vB| / |dot| > tangent  <=>  crossMagSq > tangent² · dot²
            if !^ crossMagSq > tangent * tangent * dot * dot then // handles the NaN/zero-length case correctly too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy
                let dz = pBz - pAz
                let dvA = dx * vAx + dy * vAy + dz * vAz // (pB-pA)·vA
                let dvB = dx * vBx + dy * vBy + dz * vBz // (pB-pA)·vB
                let sqLenB = vBx * vBx + vBy * vBy + vBz * vBz // |vB|²
                let t = (dvA * sqLenB - dvB * dot) / crossMagSq // numerator via Lagrange identity = (d×vB)·(vA×vB)
                if t > -1e-6 && t < 1.0 + 1e-6 then
                    let sqLenA = vAx * vAx + vAy * vAy + vAz * vAz // |vA|²
                    let u = (dvA * dot - dvB * sqLenA) / crossMagSq // numerator via Lagrange identity = (d×vA)·(vA×vB)
                    if u > -1e-6 && u < 1.0 + 1e-6 then
                        let ptAx = pAx + t * vAx
                        let ptAy = pAy + t * vAy
                        let ptAz = pAz + t * vAz
                        let ptBx = pBx + u * vBx
                        let ptBy = pBy + u * vBy
                        let ptBz = pBz + u * vBz
                        let dx2 = ptAx - ptBx
                        let dy2 = ptAy - ptBy
                        let dz2 = ptAz - ptBz
                        let sqDist = dx2*dx2 + dy2*dy2 + dz2*dz2
                        if sqDist < maxSkewDistance*maxSkewDistance then
                            ValueSome <| Pnt(ptAx, ptAy, ptAz)
                        else
                            ValueNone
                    else
                        ValueNone
                else
                    ValueNone
            else
                ValueNone

    /// <summary> Returns the closest approach parameter on the first ray (A) where ray A and ray B come closest.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vAz"> The Z component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="vBz"> The Z component of the vector of the second ray.</param>
    /// <returns> The parameter at which the two rays come closest on line A if it is smaller than 1e12 in absolute value,
    /// or ValueNone if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    let inline tryClosestParameterRayA (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                        vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): float voption =
        // Cross product of vectors vA × vB
        let crossX = vAy * vBz - vAz * vBy
        let crossY = vAz * vBx - vAx * vBz
        let crossZ = vAx * vBy - vAy * vBx
        // Magnitude squared of cross product (if zero, lines are parallel)
        let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ
        // Difference in start points
        let dx = pBx - pAx
        let dy = pBy - pAy
        let dz = pBz - pAz
        // Cross product of difference vector with vB: (pB - pA) × vB
        let numerX = dy * vBz - dz * vBy
        let numerY = dz * vBx - dx * vBz
        let numerZ = dx * vBy - dy * vBx
        // Dot product of (pB - pA) × vB with vA × vB
        let numerator = numerX * crossX + numerY * crossY + numerZ * crossZ
        let t = numerator / crossMagSq //  this can be NaN or Infinity
        if t > -1e12 && t < 1e12 then
            ValueSome t
        else
            ValueNone





    /// <summary> Tries to get closest approach point of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vAz"> The Z component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="vBz"> The Z component of the vector of the second ray.</param>
    /// <returns> The point at which the two rays come closest (on Line A) if the parameter on Line A is smaller than 1e12 in absolute value,
    /// or ValueNone if the parameter is bigger than 1e12 ( = the lines are far away or parallel) or if they are exactly parallel or coincident.</returns>
    let inline tryClosestPntRayA (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                  vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): Pnt voption =
        // Cross product of vectors vA × vB
        let crossX = vAy * vBz - vAz * vBy
        let crossY = vAz * vBx - vAx * vBz
        let crossZ = vAx * vBy - vAy * vBx
        // Magnitude squared of cross product (if zero, lines are parallel)
        let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ
        // Difference in start points
        let dx = pBx - pAx
        let dy = pBy - pAy
        let dz = pBz - pAz
        // Cross product of difference vector with vB: (pB - pA) × vB
        let numerX = dy * vBz - dz * vBy
        let numerY = dz * vBx - dx * vBz
        let numerZ = dx * vBy - dy * vBx
        // Dot product of (pB - pA) × vB with vA × vB
        let numerator = numerX * crossX + numerY * crossY + numerZ * crossZ
        let t = numerator / crossMagSq //  this can be NaN or Infinity
        if t > -1e12 && t < 1e12 then // checks for NaN correctly
            ValueSome <| Pnt(pAx + t * vAx, pAy + t * vAy, pAz + t * vAz)
        else
            ValueNone




    /// <summary> Tries to get an actual intersection point of two rays (3D lines treated as infinite).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vAz"> The Z component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="vBz"> The Z component of the vector of the second ray.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, ValueNone is returned.</param>
    /// <returns> An intersection point on ray A, or ValueNone if the rays are skew, parallel, or too short.</returns>
    let tryIntersectRay(  pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                    vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                    maxSkewDistance:float,
                                    tangent:float<Tangent.tangent>,
                                    tooShortTolerance:float
                                    ) : Pnt voption =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            ValueNone
        elif isTooShort(vBx, vBy, vBz, tooShortTolerance) then
            ValueNone
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ // squared magnitude of vA × vB
            let dot = vAx * vBx + vAy * vBy + vAz * vBz
            // parallel test without sqrt or division: |vA×vB| / |dot| > tangent  <=>  crossMagSq > tangent² · dot²
            if !^ crossMagSq > tangent * tangent * dot * dot then // handles the NaN/zero-length case correctly too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy
                let dz = pBz - pAz
                let dvA = dx * vAx + dy * vAy + dz * vAz // (pB-pA)·vA
                let dvB = dx * vBx + dy * vBy + dz * vBz // (pB-pA)·vB
                let sqLenB = vBx * vBx + vBy * vBy + vBz * vBz // |vB|²
                let t = (dvA * sqLenB - dvB * dot) / crossMagSq // numerator via Lagrange identity = (d×vB)·(vA×vB)
                let ptAx = pAx + t * vAx
                let ptAy = pAy + t * vAy
                let ptAz = pAz + t * vAz
                let d = sqRayPtDist(pBx, pBy, pBz, vBx, vBy, vBz, ptAx, ptAy, ptAz)
                if d < maxSkewDistance*maxSkewDistance then
                    ValueSome <|  Pnt(ptAx, ptAy, ptAz)
                else // skew lines
                    ValueNone
            else
                ValueNone









// #endregion
// #region Discriminated Union



    /// <summary> Tries to get closest approach parameters of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vAz"> The Z component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="vBz"> The Z component of the vector of the second ray.</param>
    ///<param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XRayParam Discriminated Union with the following cases:
    /// | SkewOrX of float*float  (parameters on both lines at closest approach)
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    let getRayClosestParam(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                          vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                          tangent:float<Tangent.tangent>,
                                          tooShortTolerance:float
                                          ) : XRayParam =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            if isTooShort(vBx, vBy, vBz, tooShortTolerance) then
                XRayParam.TooShortBoth
            else
                XRayParam.TooShortA
        elif isTooShort(vBx, vBy, vBz, tooShortTolerance) then
            XRayParam.TooShortB
        else
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ // squared magnitude of vA × vB
            let dot = vAx * vBx + vAy * vBy + vAz * vBz
            // parallel test without sqrt or division: |vA×vB| / |dot| > tangent  <=>  crossMagSq > tangent² · dot²
            if !^ crossMagSq > tangent * tangent * dot * dot then
                let dx = pBx - pAx
                let dy = pBy - pAy
                let dz = pBz - pAz
                let dvA = dx * vAx + dy * vAy + dz * vAz // (pB-pA)·vA
                let dvB = dx * vBx + dy * vBy + dz * vBz // (pB-pA)·vB
                let sqLenA = vAx * vAx + vAy * vAy + vAz * vAz // |vA|²
                let sqLenB = vBx * vBx + vBy * vBy + vBz * vBz // |vB|²
                // t and u numerators via the Lagrange identity, equal to (d×vB)·(vA×vB) and (d×vA)·(vA×vB)
                let t = (dvA * sqLenB - dvB * dot) / crossMagSq
                let u = (dvA * dot - dvB * sqLenA) / crossMagSq
                XRayParam.SkewOrX ((t,u))
            else
                XRayParam.Parallel





    /// <summary> Tries to get closest approach point of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first ray.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first ray.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first ray.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second ray.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second ray.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second ray.</param>
    /// <param name="vAx"> The X component of the vector of the first ray.</param>
    /// <param name="vAy"> The Y component of the vector of the first ray.</param>
    /// <param name="vAz"> The Z component of the vector of the first ray.</param>
    /// <param name="vBx"> The X component of the vector of the second ray.</param>
    /// <param name="vBy"> The Y component of the vector of the second ray.</param>
    /// <param name="vBz"> The Z component of the vector of the second ray.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XRay Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB</returns>
    let getRayIntersection(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                     vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                     maxSkewDistance:float,
                                     tangent:float<Tangent.tangent>,
                                     tooShortTolerance:float
                                     ) : XRay =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            if isTooShort(vBx, vBy, vBz, tooShortTolerance) then
                XRay.TooShortBoth
            else
                XRay.TooShortA
        elif
            isTooShort(vBx, vBy, vBz, tooShortTolerance) then
                XRay.TooShortB
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ // squared magnitude of vA × vB
            let dot = vAx * vBx + vAy * vBy + vAz * vBz
            // parallel test without sqrt or division: |vA×vB| / |dot| > tangent  <=>  crossMagSq > tangent² · dot²
            if !^ crossMagSq > tangent * tangent * dot * dot then // handles the NaN/zero-length case correctly too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy
                let dz = pBz - pAz
                let dvA = dx * vAx + dy * vAy + dz * vAz // (pB-pA)·vA
                let dvB = dx * vBx + dy * vBy + dz * vBz // (pB-pA)·vB
                let sqLenA = vAx * vAx + vAy * vAy + vAz * vAz // |vA|²
                let sqLenB = vBx * vBx + vBy * vBy + vBz * vBz // |vB|²
                // t and u numerators via the Lagrange identity, equal to (d×vB)·(vA×vB) and (d×vA)·(vA×vB)
                let t = (dvA * sqLenB - dvB * dot) / crossMagSq
                let u = (dvA * dot - dvB * sqLenA) / crossMagSq
                let ptAx = pAx + t * vAx
                let ptAy = pAy + t * vAy
                let ptAz = pAz + t * vAz
                let ptBx = pBx + u * vBx
                let ptBy = pBy + u * vBy
                let ptBz = pBz + u * vBz
                let dx2 = ptAx - ptBx
                let dy2 = ptAy - ptBy
                let dz2 = ptAz - ptBz
                let sqDist = dx2*dx2 + dy2*dy2 + dz2*dz2
                if sqDist < maxSkewDistance*maxSkewDistance then // approximately intersecting (coplanar within tolerance)
                    let ptA = Pnt(ptAx, ptAy, ptAz)
                    XRay.Intersect ptA
                else // skew lines
                    let ptA = Pnt(ptAx, ptAy, ptAz)
                    let ptB = Pnt(ptBx, ptBy, ptBz)
                    XRay.Skew (ptA, ptB, sqDist)
            else
                XRay.Parallel





    /// <summary> Tries to get intersection or closest approach parameters of two finite 3D-lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vAz"> The Z component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="vBz"> The Z component of the vector of the second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    ///<param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XParam Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Skew of float*float*float
    /// | Apart
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    let getIntersectionParam(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                       vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                       maxSkewDistance:float,
                                       tangent:float<Tangent.tangent>,
                                       tooShortTolerance:float
                                       ) : XParam =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            if isTooShort(vBx, vBy, vBz, tooShortTolerance) then
                XParam.TooShortBoth
            else
                XParam.TooShortA
        elif isTooShort(vBx, vBy, vBz, tooShortTolerance) then
            XParam.TooShortB
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ // squared magnitude of vA × vB
            let dot = vAx * vBx + vAy * vBy + vAz * vBz // Dot product of vectors
            // parallel test without sqrt or division: |vA×vB| / |dot| > tangent  <=>  crossMagSq > tangent² · dot²
            if !^ crossMagSq > tangent * tangent * dot * dot then // handles the NaN/zero-length case correctly too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy
                let dz = pBz - pAz
                let dvA = dx * vAx + dy * vAy + dz * vAz // (pB-pA)·vA
                let dvB = dx * vBx + dy * vBy + dz * vBz // (pB-pA)·vB
                let sqLenA = vAx * vAx + vAy * vAy + vAz * vAz // |vA|²
                let sqLenB = vBx * vBx + vBy * vBy + vBz * vBz // |vB|²
                // t and u numerators via the Lagrange identity, equal to (d×vB)·(vA×vB) and (d×vA)·(vA×vB)
                let t = (dvA * sqLenB - dvB * dot) / crossMagSq // embrace float math, this can be NaN or Infinity
                let u = (dvA * dot - dvB * sqLenA) / crossMagSq // embrace float math, this can be NaN or Infinity
                if t > -1e-6 && t < ``1.0 + 1e-6`` && u > -1e-6 && u < ``1.0 + 1e-6`` then //0 to 1 range check with tolerance, handles NaN too
                    // Check if lines actually intersect (coplanar) or are skew
                    let ptAx = pAx + t * vAx
                    let ptAy = pAy + t * vAy
                    let ptAz = pAz + t * vAz
                    let ptBx = pBx + u * vBx
                    let ptBy = pBy + u * vBy
                    let ptBz = pBz + u * vBz
                    let dx2 = ptAx - ptBx
                    let dy2 = ptAy - ptBy
                    let dz2 = ptAz - ptBz
                    let sqDist = dx2*dx2 + dy2*dy2 + dz2*dz2
                    if sqDist < maxSkewDistance*maxSkewDistance then // approximately intersecting (coplanar within tolerance)
                        XParam.Intersect (t,u)
                    else // skew lines
                        XParam.Skew (t, u, sqDist)
                else
                    XParam.Apart
            else
                XParam.Parallel





    /// <summary> Tries to get intersection or closest approach point of two finite 3D lines.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vAz"> The Z component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="vBz"> The Z component of the vector of the second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    ///<param name="tangent" > Is the tangent of the maximum allowed angle between the two line vectors.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XPt Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Apart
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    let getIntersection(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                  vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                  maxSkewDistance:float,
                                  tangent:float<Tangent.tangent>,
                                  tooShortTolerance:float
                                  ) : XPnt =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            if isTooShort(vBx, vBy, vBz, tooShortTolerance) then
                XPnt.TooShortBoth
            else
                XPnt.TooShortA
        elif isTooShort(vBx, vBy, vBz, tooShortTolerance) then
            XPnt.TooShortB
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ // squared magnitude of vA × vB
            let dot = vAx * vBx + vAy * vBy + vAz * vBz // Dot product of vectors
            // parallel test without sqrt or division: |vA×vB| / |dot| > tangent  <=>  crossMagSq > tangent² · dot²
            if !^ crossMagSq > tangent * tangent * dot * dot then // handles the NaN/zero-length case correctly too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy
                let dz = pBz - pAz
                let dvA = dx * vAx + dy * vAy + dz * vAz // (pB-pA)·vA
                let dvB = dx * vBx + dy * vBy + dz * vBz // (pB-pA)·vB
                let sqLenA = vAx * vAx + vAy * vAy + vAz * vAz // |vA|²
                let sqLenB = vBx * vBx + vBy * vBy + vBz * vBz // |vB|²
                // t and u numerators via the Lagrange identity, equal to (d×vB)·(vA×vB) and (d×vA)·(vA×vB)
                let t = (dvA * sqLenB - dvB * dot) / crossMagSq // embrace float math, this can be NaN or Infinity
                let u = (dvA * dot - dvB * sqLenA) / crossMagSq // embrace float math, this can be NaN or Infinity
                if t > -1e-6 && t < ``1.0 + 1e-6`` && u > -1e-6 && u < ``1.0 + 1e-6`` then
                    let ptAx = pAx + t * vAx
                    let ptAy = pAy + t * vAy
                    let ptAz = pAz + t * vAz
                    let ptBx = pBx + u * vBx
                    let ptBy = pBy + u * vBy
                    let ptBz = pBz + u * vBz
                    let dx2 = ptAx - ptBx
                    let dy2 = ptAy - ptBy
                    let dz2 = ptAz - ptBz
                    let sqDist = dx2*dx2 + dy2*dy2 + dz2*dz2
                    if sqDist < maxSkewDistance*maxSkewDistance then // approximately intersecting (coplanar within tolerance)
                        XPnt.Intersect (Pnt(ptAx, ptAy, ptAz))
                    else // skew lines
                        XPnt.Skew (Pnt(ptAx, ptAy, ptAz), Pnt(ptBx, ptBy, ptBz), sqDist)
                else
                    XPnt.Apart
            else
                XPnt.Parallel





    /// <summary> Gets the parameters on the finite lines where the lines are closest to each other.
    /// For parallel lines, this will return the parameter at the middle point of any overlapping segment.
    /// For skew lines, this returns the closest approach parameters.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vAz"> The Z component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="vBz"> The Z component of the vector of the second line.</param>
    /// <param name="tangent"> This is a tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> A ClParams Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Skew of float*float*float
    /// | Parallel of float*float
    /// | Apart of float*float*float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    let getClosestParameters(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                       vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                       tangent:float<Tangent.tangent>,
                                       tooShortTolerance:float
                                       ): ClParams =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            if isTooShort(vBx, vBy, vBz, tooShortTolerance) then
                ClParams.TooShortBoth
            else
                ClParams.TooShortA
        elif isTooShort(vBx, vBy, vBz, tooShortTolerance) then
            ClParams.TooShortB
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMagSq = crossX * crossX + crossY * crossY + crossZ * crossZ // squared magnitude of vA × vB
            let dot = vAx * vBx + vAy * vBy + vAz * vBz // Dot product of vectors
            // parallel test without sqrt or division: |vA×vB| / |dot| > tangent  <=>  crossMagSq > tangent² · dot²
            if !^ crossMagSq > tangent * tangent * dot * dot then // handles the NaN/zero-length case correctly too
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy
                let dz = pBz - pAz
                let dvA = dx * vAx + dy * vAy + dz * vAz // (pB-pA)·vA
                let dvB = dx * vBx + dy * vBy + dz * vBz // (pB-pA)·vB
                let sqLenA = vAx * vAx + vAy * vAy + vAz * vAz // |vA|²
                let sqLenB = vBx * vBx + vBy * vBy + vBz * vBz // |vB|²
                // t and u numerators via the Lagrange identity, equal to (d×vB)·(vA×vB) and (d×vA)·(vA×vB)
                let t = (dvA * sqLenB - dvB * dot) / crossMagSq // embrace float math, this can be NaN or Infinity
                let u = (dvA * dot - dvB * sqLenA) / crossMagSq // embrace float math, this can be NaN or Infinity
                if t >= -1e-6 && t <= ``1.0 + 1e-6`` && u >= -1e-6 && u <= ``1.0 + 1e-6`` then
                    let ptAx = pAx + t * vAx
                    let ptAy = pAy + t * vAy
                    let ptAz = pAz + t * vAz
                    let ptBx = pBx + u * vBx
                    let ptBy = pBy + u * vBy
                    let ptBz = pBz + u * vBz
                    let dx2 = ptAx - ptBx
                    let dy2 = ptAy - ptBy
                    let dz2 = ptAz - ptBz
                    let sqDist = dx2*dx2 + dy2*dy2 + dz2*dz2
                    if sqDist < 1e-12 then // approximately intersecting (coplanar within tolerance)
                        ClParams.Intersect (t,u)
                    else // skew lines
                        ClParams.Skew (t, u, sqDist)
                else
                    // The closest approach of the infinite lines lies outside at least one segment.
                    // Closest pair sits at a clamped endpoint -> clamp-and-recompute (Ericson §5.1.9).
                    // t is already the unclamped closest-approach parameter on A: (b*f - c*e)/denom with denom = sqLenA*sqLenB - dot² = crossMagSq (Lagrange identity).
                    let mutable sa = clamp01 t
                    let mutable sb = (dot * sa - dvB) / sqLenB // closest point on line B to A(sa); dvB = vB·(pB-pA), hence the minus
                    if sb < 0.0 then
                        sb <- 0.0
                        sa <- clamp01 (dvA / sqLenA)
                    elif sb > 1.0 then
                        sb <- 1.0
                        sa <- clamp01 ((dvA + dot) / sqLenA)
                    let ax = pAx + sa * vAx
                    let ay = pAy + sa * vAy
                    let az = pAz + sa * vAz
                    let bx = pBx + sb * vBx
                    let by = pBy + sb * vBy
                    let bz = pBz + sb * vBz
                    let dx2 = ax - bx
                    let dy2 = ay - by
                    let dz2 = az - bz
                    ClParams.Apart (sa, sb, dx2*dx2 + dy2*dy2 + dz2*dz2)
            else
                // Parallel within tolerance: minimum-distance solutions span an interval on A.
                // Return the midpoint of the overlapping segment.
                let sqLenA = vAx * vAx + vAy * vAy + vAz * vAz
                let sqLenB = vBx * vBx + vBy * vBy + vBz * vBz
                let wx = pBx - pAx
                let wy = pBy - pAy
                let wz = pBz - pAz
                let sB0 = (wx * vAx + wy * vAy + wz * vAz) / sqLenA // pB      projected onto line A
                let sB1 = sB0 + dot / sqLenA                        // pB + vB projected onto line A (vB·vA = dot, already computed)
                let lo = max 0.0 (min sB0 sB1)               // overlap interval on A, intersected with [0,1]
                let hi = min 1.0 (max sB0 sB1)
                let sa = clamp01 ((lo + hi) * 0.5)           // midpoint of overlap; clamp covers the no-overlap case
                let cx = pAx + sa * vAx - pBx                // perpendicular foot of A(sa) ...
                let cy = pAy + sa * vAy - pBy
                let cz = pAz + sa * vAz - pBz
                let sb = clamp01 ((cx * vBx + cy * vBy + cz * vBz) / sqLenB) // ... onto line B
                ClParams.Parallel (sa, sb)






    /// <summary> Gets the point or points on the finite lines where the lines are closest to each other.
    /// For parallel lines, this will return the middle point of any overlapping segment.
    /// For skew lines, this returns the two closest approach points.</summary>
    /// <param name="pAx"> The X coordinate of the start point on the first line.</param>
    /// <param name="pAy"> The Y coordinate of the start point on the first line.</param>
    /// <param name="pAz"> The Z coordinate of the start point on the first line.</param>
    /// <param name="pBx"> The X coordinate of the start point on the second line.</param>
    /// <param name="pBy"> The Y coordinate of the start point on the second line.</param>
    /// <param name="pBz"> The Z coordinate of the start point on the second line.</param>
    /// <param name="vAx"> The X component of the vector of the first line.</param>
    /// <param name="vAy"> The Y component of the vector of the first line.</param>
    /// <param name="vAz"> The Z component of the vector of the first line.</param>
    /// <param name="vBx"> The X component of the vector of the second line.</param>
    /// <param name="vBy"> The Y component of the vector of the second line.</param>
    /// <param name="vBz"> The Z component of the vector of the second line.</param>
    /// <param name="tangent"> This is a tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is a length tolerance.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> A ClPts Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Parallel of Pnt * Pnt
    /// | Apart of Pnt * Pnt * float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    let getClosestPoints(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                   vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                   tangent:float<Tangent.tangent>,
                                   tooShortTolerance:float
                                   ): ClPts =
        match getClosestParameters (pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz, tangent, tooShortTolerance) with
        | ClParams.TooShortA -> ClPts.TooShortA
        | ClParams.TooShortB -> ClPts.TooShortB
        | ClParams.TooShortBoth -> ClPts.TooShortBoth
        | ClParams.Intersect (sa, _) ->
            ClPts.Intersect (Pnt(pAx + sa * vAx, pAy + sa * vAy, pAz + sa * vAz))
        | ClParams.Skew (sa, sb, sqDist) ->
            ClPts.Skew (Pnt(pAx + sa * vAx, pAy + sa * vAy, pAz + sa * vAz),
                        Pnt(pBx + sb * vBx, pBy + sb * vBy, pBz + sb * vBz),
                        sqDist)
        | ClParams.Apart (sa, sb, sqDist) ->
            ClPts.Apart (Pnt(pAx + sa * vAx, pAy + sa * vAy, pAz + sa * vAz),
                         Pnt(pBx + sb * vBx, pBy + sb * vBy, pBz + sb * vBz),
                         sqDist)
        | ClParams.Parallel (sa, sb) ->
            ClPts.Parallel (Pnt(pAx + sa * vAx, pAy + sa * vAy, pAz + sa * vAz),
                            Pnt(pBx + sb * vBx, pBy + sb * vBy, pBz + sb * vBz))






// #endregion
// #region XLine3D

open XLine3D
open XLineXYZ

/// A type containing only static member functions for computing 3D line intersections and closest approaches.
/// Some functions return Discriminated Unions from the XLine3D module.
[<RequireQualifiedAccess>]
type XLine3D =


    /// <summary> Returns the closest approach parameter on the first ray (A) where ray A and ray B come closest.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The parameter on line A at the closest approach with line B.
    ///  NaN if both points coincide and vectors are exactly parallel (0.0/0.0).
    ///  Positive/Negative Infinity for almost parallel lines.</returns>
    static member inline parameterA (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec): float =
        XLineXYZ.parameterA (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Returns the closest approach parameter on the first ray (A) where ray A and ray B come closest.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The parameter on lineA at the closest approach with lineB.
    ///  NaN if both start points coincide and vectors are exactly parallel (0.0/0.0).
    ///  Positive/Negative Infinity for almost parallel lines.</returns>
    static member inline parameterA (lineA: Line3D, lineB: Line3D): float =
        XLineXYZ.parameterA (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                            lineA.VectorX, lineA.VectorY, lineA.VectorZ,
                            lineB.VectorX, lineB.VectorY, lineB.VectorZ)


    /// <summary> Returns the closest approach parameters on both rays A and B.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their closest approach.
    ///  These are NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  These are positive or negative Infinity for almost parallel lines.</returns>
    static member inline parameters (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec): float*float =
        XLineXYZ.parameters (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Returns the closest approach parameters on both rays.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their closest approach.
    ///  These are NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  These are positive or negative Infinity for almost parallel lines.</returns>
    static member inline parameters(lineA: Line3D, lineB: Line3D): float*float =
        XLineXYZ.parameters(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                           lineA.VectorX, lineA.VectorY, lineA.VectorZ,
                           lineB.VectorX, lineB.VectorY, lineB.VectorZ)

    /// <summary> Checks if two 3D-lines intersect within the parameter range 0.0 to 1.0 (with 1e-6 tolerance) on both lines.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="maxSkewDistance" > Is an optional maximum distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between the two lines at their closest approach is less than this value.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    /// If one or both lines are shorter than this, FALSE is returned.</param>
    /// <returns> TRUE if both finite lines intersect within the given maximum skew distance.
    /// FALSE if the lines are parallel, coincident, too short, skew beyond the given tolerance,
    /// or at least one intersection parameter is outside of the range.</returns>
    static member inline doIntersect (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float): bool =
        XLineXYZ.doIntersect (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, maxSkewDistance, tangent, tooShortTolerance)

    /// <summary> Checks if two 3D-lines intersect within the parameter range 0.0 to 1.0 (with 1e-6 tolerance) on both lines.</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <param name="maxSkewDistance" > Is an optional maximum distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between the two lines at their closest approach is less than this value.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    /// If one or both lines are shorter than this, FALSE is returned.</param>
    /// <returns> TRUE if both finite lines intersect within the given maximum skew distance.
    /// FALSE if the lines are parallel, coincident, too short, skew beyond the given tolerance,
    /// or at least one intersection parameter is outside of the range.</returns>
    static member inline doIntersect (lineA: Line3D, lineB: Line3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float): bool =
        XLineXYZ.doIntersect (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                              lineA.VectorX, lineA.VectorY, lineA.VectorZ,
                              lineB.VectorX, lineB.VectorY, lineB.VectorZ,
                              maxSkewDistance, tangent, tooShortTolerance)




    /// <summary> Checks if 3D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="pA"> The start point on the first line.</param>
    /// <param name="pB"> The start point on the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="tolerance" > Is an optional distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    /// <returns> TRUE if the lines are coincident and overlap or touch.
    ///  FALSE if the lines are not coincident or do not overlap nor touch.</returns>
    static member inline doOverlap(pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(1e-6)>] tolerance:float ) : bool =
        XLineXYZ.doOverlap(pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, tolerance)

    /// <summary> Checks if 3D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <param name="tolerance" > Is an optional distance tolerance (compared against the squared distance internally). 1e-6 by default.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    /// <returns> TRUE if the lines are coincident and overlap or touch.
    ///  FALSE if the lines are not coincident or do not overlap nor touch.</returns>
    static member inline doOverlap(lineA:Line3D, lineB:Line3D, [<OPT;DEF(1e-6)>] tolerance:float ) : bool =
        XLineXYZ.doOverlap(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                            lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, tolerance)



    /// <summary> Checks if two 3D-rays actually intersect within a given maximum skew distance.
    /// Returns FALSE if rays are parallel or even identical, or if input lines are of zero length.
    /// Use XLine3D.getRayIntersection to check for those cases too.</summary>
    /// <param name="pA"> The start point on the first ray.</param>
    /// <param name="pB"> The start point on the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <param name="maxSkewDistance" > Is an optional maximum distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between the two lines at their closest approach is less than this value.</param>
    /// <returns> TRUE if both rays intersect within the given maximum skew distance.
    /// FALSE if the rays are parallel, coincident or the closest approach distance is larger than the given tolerance.</returns>
    static member inline doRaysIntersect (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(1e-6)>] maxSkewDistance:float): bool =
        XLineXYZ.doRaysIntersect (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, maxSkewDistance)

    /// <summary> Checks if two 3D-rays actually intersect within a given maximum skew distance.
    /// Returns FALSE if rays are parallel or even identical, or if input lines are of zero length.
    /// Use XLine3D.getRayIntersection to check for those cases too.</summary>
    /// <param name="lineA"> The first ray.</param>
    /// <param name="lineB"> The second ray.</param>
    /// <param name="maxSkewDistance" > Is an optional maximum distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between the two lines at their closest approach is less than this value.</param>
    /// <returns> TRUE if both rays intersect within the given maximum skew distance.
    /// FALSE if the rays are parallel, coincident or the closest approach distance is larger than the given tolerance.</returns>
    static member inline doRaysIntersect (lineA: Line3D, lineB: Line3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float): bool =
        XLineXYZ.doRaysIntersect (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                                  lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance)



    // #endregion
    // #region Option


    /// <summary> Returns the closest approach parameter on the first ray (A) where ray A and ray B come closest.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The parameter on line A at the closest approach with line B if it is smaller than 1e12 in absolute value,
    /// or ValueNone if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryClosestParameterRayA (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec): float voption =
        XLineXYZ.tryClosestParameterRayA (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Returns the closest approach parameter on the first ray (A) where ray A and ray B come closest.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The parameter on lineA at the closest approach with lineB if it is smaller than 1e12 in absolute value,
    /// or ValueNone if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryClosestParameterRayA (lineA:Line3D, lineB:Line3D): float voption =
        XLineXYZ.tryClosestParameterRayA (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                                          lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ)



    /// <summary> Tries to get closest approach point of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The point at which the two rays come closest (on Line A) if the parameter on Line A is smaller than 1e12 in absolute value,
    /// or ValueNone if the parameter is bigger than 1e12 ( = the lines are far away or parallel) or if they are exactly parallel or coincident.</returns>
    static member inline tryClosestPntRayA (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec): Pnt voption =
        XLineXYZ.tryClosestPntRayA (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Tries to get closest approach point of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <returns> The point at which the two rays come closest (on Line A, first Line) if the parameter on Line A is smaller than 1e12 in absolute value,
    /// or ValueNone if the parameter is bigger than 1e12 ( = the lines are far away or parallel) or if they are exactly parallel or coincident.</returns>
    static member inline tryClosestPntRayA (lineA: Line3D, lineB: Line3D): Pnt voption =
        XLineXYZ.tryClosestPntRayA (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                                    lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ)



    /// <summary> Tries to get an actual intersection point of two rays (3D lines treated as infinite).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, ValueNone is returned.</param>
    /// <returns> An intersection point on ray A, or ValueNone if the rays are skew, parallel, or too short.</returns>
    static member inline tryIntersectRay(  pA:Pnt, pB:Pnt, vA:Vec, vB:Vec,
                                            [<OPT;DEF(1e-6)>] maxSkewDistance:float,
                                            [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                            [<OPT;DEF(1e-6)>] tooShortTolerance:float) : Pnt voption =
        XLineXYZ.tryIntersectRay( pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z,maxSkewDistance, tangent, tooShortTolerance)

    /// <summary> Tries to get an actual intersection point of two rays (3D lines treated as infinite).</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, ValueNone is returned.</param>
    /// <returns> An intersection point on ray A, or ValueNone if the rays are skew, parallel, or too short.</returns>
    static member inline tryIntersectRay(lineA:Line3D, lineB:Line3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float,
                                        [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                        [<OPT;DEF(1e-6)>] tooShortTolerance:float) : Pnt voption =
        XLineXYZ.tryIntersectRay(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                                lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)



    /// <summary> Tries to get an actual intersection point of two finite 3D lines.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, ValueNone is returned.</param>
    /// <returns> An intersection point on line A, or ValueNone if the segments are skew, parallel, or too short.</returns>
    static member inline tryIntersect( pA:Pnt, pB:Pnt, vA:Vec, vB:Vec,
                                        [<OPT;DEF(1e-6)>] maxSkewDistance:float,
                                        [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                        [<OPT;DEF(1e-6)>] tooShortTolerance:float) : Pnt voption =
        XLineXYZ.tryIntersect( pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z,maxSkewDistance, tangent, tooShortTolerance)


    /// <summary> Tries to get an actual intersection point of two finite 3D lines.</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, ValueNone is returned.</param>
    /// <returns> An intersection point on line A, or ValueNone if the segments are skew, parallel, or too short.</returns>
    static member inline tryIntersect(lineA:Line3D, lineB:Line3D,
                                      [<OPT;DEF(1e-6)>] maxSkewDistance:float,
                                      [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                      [<OPT;DEF(1e-6)>] tooShortTolerance:float) : Pnt voption =
        XLineXYZ.tryIntersect(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                              lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)



    // #endregion
    // #region Discriminated Unions


    /// <summary> Tries to get closest approach parameters of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XRayParam Discriminated Union with the following cases:
    /// | SkewOrX of float*float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getRayClosestParam(pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRayParam =
        XLineXYZ.getRayClosestParam (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, tangent, tooShortTolerance)

    /// <summary> Tries to get closest approach parameters of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XRayParam Discriminated Union with the following cases:
    /// | SkewOrX of float*float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getRayClosestParam(lineA: Line3D, lineB: Line3D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRayParam =
        XLineXYZ.getRayClosestParam(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, tangent, tooShortTolerance)



    /// <summary> Classifies the relationship between two rays (treated as infinite lines) and returns their intersection or closest approach.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XRay Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getRayIntersection(pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(1e-6)>] maxSkewDistance:float,[<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRay =
        XLineXYZ.getRayIntersection(pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, maxSkewDistance, tangent, tooShortTolerance)

    /// <summary> Classifies the relationship between two rays (treated as infinite lines) and returns their intersection or closest approach.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XRay Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getRayIntersection(lineA: Line3D, lineB: Line3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float,[<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XRay =
        XLineXYZ.getRayIntersection(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)



    /// <summary> Tries to get intersection or closest approach parameters of two finite 3D-lines.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting. Default is 1e-6.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XParam Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Skew of float*float*float
    /// | Apart
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersectionParam(pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XParam =
        XLineXYZ.getIntersectionParam (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, maxSkewDistance, tangent, tooShortTolerance)

    /// <summary> Tries to get intersection or closest approach parameters of two finite 3D-lines.</summary>
    /// <param name="lnA"> First line.</param>
    /// <param name="lnB"> Second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting. Default is 1e-6.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XParam Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Skew of float*float*float
    /// | Apart
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersectionParam(lnA: Line3D, lnB: Line3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XParam =
        XLineXYZ.getIntersectionParam(lnA.FromX, lnA.FromY, lnA.FromZ, lnB.FromX, lnB.FromY, lnB.FromZ, lnA.VectorX, lnA.VectorY, lnA.VectorZ, lnB.VectorX, lnB.VectorY, lnB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)



    /// <summary> Tries to get intersection or closest approach point of two finite 3D lines.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting. Default is 1e-6.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XPt Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Apart
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersection(pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,[<OPT;DEF(1e-6)>] tooShortTolerance:float) : XPnt =
        XLineXYZ.getIntersection(pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, maxSkewDistance, tangent, tooShortTolerance)

    /// <summary> Tries to get intersection or closest approach point of two finite 3D lines.</summary>
    /// <param name="lineA"> First finite line segment.</param>
    /// <param name="lineB"> Second finite line segment.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting. Default is 1e-6.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XPt Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Apart
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getIntersection(lineA: Line3D, lineB: Line3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float ) : XPnt =
        XLineXYZ.getIntersection(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)


    /// <summary> Gets the parameters on the finite lines where the lines are closest to each other.
    /// For parallel lines, this will return the parameter at the middle point of any overlapping segment.
    /// For skew lines, this returns the closest approach parameters.</summary>
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
    /// <returns> A ClParams Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Skew of float*float*float
    /// | Parallel of float*float
    /// | Apart of float*float*float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getClosestParameters (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float): ClParams =
        XLineXYZ.getClosestParameters (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, tangent, tooShortTolerance)

    /// <summary> Gets the parameters on the finite lines where the lines are closest to each other.
    /// For parallel lines, this will return the parameter at the middle point of any overlapping segment.
    /// For skew lines, this returns the closest approach parameters.</summary>
    /// <param name="lineA"> The first finite line segment.</param>
    /// <param name="lineB"> The second finite line segment.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> A ClParams Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Skew of float*float*float
    /// | Parallel of float*float
    /// | Apart of float*float*float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getClosestParameters (lineA:Line3D, lineB:Line3D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float): ClParams =
        XLineXYZ.getClosestParameters (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, tangent, tooShortTolerance)


    /// <summary> Gets the point or points on the finite lines where the lines are closest to each other.
    /// For parallel lines, this will return the middle point of any overlapping segment.
    /// For skew lines, this returns the two closest approach points.</summary>
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
    /// <returns> A ClPts Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Parallel of Pnt * Pnt
    /// | Apart of Pnt * Pnt * float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getClosestPoints (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : ClPts =
        XLineXYZ.getClosestPoints (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, tangent, tooShortTolerance)

    /// <summary> Gets the point or points on the finite lines where the lines are closest to each other.
    /// For parallel lines, this will return the middle point of any overlapping segment.
    /// For skew lines, this returns the two closest approach points.</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> A ClPts Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Parallel of Pnt * Pnt
    /// | Apart of Pnt * Pnt * float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getClosestPoints (lineA: Line3D, lineB: Line3D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : ClPts =
        XLineXYZ.getClosestPoints (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, tangent, tooShortTolerance)


    /// <summary> Gets the squared distance between two finite lines. Works on parallel and skew lines too.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <returns> the squared distance between the two lines </returns>
    static member inline getSqDistance(pA:Pnt, pB:Pnt, vA:Vec, vB:Vec ): float =
        XLineXYZ.sqDistance (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Gets the squared distance between two finite lines. Works on parallel and skew lines too.</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <returns> the squared distance between the two lines </returns>
    static member inline getSqDistance(lineA: Line3D, lineB: Line3D ): float =
        XLineXYZ.sqDistance (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ)



    /// <summary>Checks if the two finite 3D lines are touching each other at exactly one of their end points
    /// within the given tolerance.
    /// This will return a separate case (5 or 6) if the lines are touching on both points.</summary>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <param name="tolerance"> Is an optional distance tolerance. 1e-6 by default.</param>
    /// <returns>A Discriminated Union XLine3D.XEnds that describes the possible cases of two finite 3D lines touching at their ends:
    /// | NotTouching
    /// | StartA_StartB
    /// | EndA_EndB
    /// | EndA_StartB
    /// | StartA_EndB
    /// | Identical
    /// | IdenticalFlipped
    /// </returns>
    static member getEndsTouching (a:Line3D, b:Line3D, [<OPT;DEF(1e-6)>] tolerance:float) : XLine3D.XEnds =
        let sqTolerance = tolerance * tolerance
        if sq(a.ToX-b.FromX) + sq(a.ToY-b.FromY) + sq(a.ToZ-b.FromZ) < sqTolerance then
            if sq(a.FromX-b.ToX) + sq(a.FromY-b.ToY) + sq(a.FromZ-b.ToZ) < sqTolerance then
                XEnds.IdenticalFlipped
            else
                XEnds.EndA_StartB
        elif sq(a.FromX-b.ToX) + sq(a.FromY-b.ToY) + sq(a.FromZ-b.ToZ) < sqTolerance then
            XEnds.StartA_EndB
        elif sq(a.FromX-b.FromX) + sq(a.FromY-b.FromY) + sq(a.FromZ-b.FromZ) < sqTolerance then
            if sq(a.ToX-b.ToX) + sq(a.ToY-b.ToY) + sq(a.ToZ-b.ToZ) < sqTolerance then
                XEnds.Identical
            else
                XEnds.StartA_StartB
        elif sq(a.ToX-b.ToX) + sq(a.ToY-b.ToY) + sq(a.ToZ-b.ToZ) < sqTolerance then
            XEnds.EndA_EndB
        else
            XEnds.NotTouching


    /// <summary>Intersects an infinite ray / line with the base-side nappe of a cone whose axis is the Z-axis.</summary>
    /// <param name="ray">The Line3D to intersect. It is considered infinite in both directions.</param>
    /// <param name="coneRadius">The radius of the cone at the base. Parallel to the XY plane. Must be positive.</param>
    /// <param name="coneBaseZ">The Z coordinate of the cone base.</param>
    /// <param name="coneTipZ">The Z coordinate of the cone tip. Must differ from coneBaseZ.
    /// Only the nappe on the base side of the tip is considered part of the cone.</param>
    /// <returns>An XCone discriminated union, see its documentation for all cases.</returns>
    static member intersectCone (ray:Line3D, coneRadius:float, coneBaseZ:float, coneTipZ:float) : XCone =

        // Relative tolerance used for all coefficient comparisons.
        // Each coefficient is compared against the sum of the absolute values of the
        // terms it was computed from. This makes the check scale-invariant.
        let relTol = 1e-9

        // Looser relative tolerance for snapping points to the tip / tip plane.
        // Near a double root the computed t carries an error of roughly sqrt(machine epsilon),
        // so a tighter value than ~1e-6 would misclassify true through-tip lines as Touching.
        let tipTol = 1e-6

        let height = coneBaseZ - coneTipZ
        if abs height < 1e-12 then
            fail "Line3D.intersectCone: coneBaseZ and coneTipZ are equal (%g), the cone is degenerate." coneBaseZ
            // in Euclid use: EuclidException.Raisef "..."
        if coneRadius < 1e-12 then
            fail "Line3D.intersectCone: coneRadius %g is zero or negative, the cone is degenerate." coneRadius

        // Slope of the cone flank: radius change per unit of Z. Only k² is used in the quadratic.
        let k  = coneRadius / height
        let k2 = k * k
        // The lower (valid) nappe is where sign(z - coneTipZ) = sign(height):
        let signH = if height > 0.0 then 1.0 else -1.0

        // Ray origin, shifted so that the cone tip sits at the world origin:
        let ox = ray.FromX
        let oy = ray.FromY
        let oz = ray.FromZ - coneTipZ
        // Ray direction. The returned parameters are in units of this vector:
        let dx = ray.ToX - ray.FromX
        let dy = ray.ToY - ray.FromY
        let dz = ray.ToZ - ray.FromZ

        if dx*dx + dy*dy + dz*dz < 1e-24 then
            fail "Line3D.intersectCone: ray is too short: %O" ray

        // Implicit double cone equation with tip at the origin:   x² + y² - k²·z² = 0
        // Substituting the line  P(t) = O + t·D  yields the quadratic  a·t² + b·t + c = 0 :
        let a = dx*dx + dy*dy - k2*dz*dz
        let b = 2.0 * (ox*dx + oy*dy - k2*oz*dz)
        let c = ox*ox + oy*oy - k2*oz*oz

        // Magnitude scales for the relative tolerance checks (sums of absolute values of the same terms).
        // If a scale is exactly 0.0 then the corresponding coefficient is exactly 0.0 too,
        // so 'abs x <= relTol * scale' is still correct (0 <= 0).
        let aScale = dx*dx + dy*dy + k2*dz*dz
        let bScale = 2.0 * (abs(ox*dx) + abs(oy*dy) + k2*(abs(oz*dz)))
        let cScale = ox*ox + oy*oy + k2*oz*oz

        // Classifies a root of the quadratic by which nappe its point lies on:
        //  +1 : on the lower (valid) nappe
        //   0 : at the tip (within tolerance)
        //  -1 : on the mirrored upper nappe -> not part of the cone
        // Because every root lies on the cone surface, the Z coordinate alone decides:
        // on the surface |z| ~ 0 implies x,y ~ 0, i.e. the point is the apex.
        let nappeOf (t:float) =
            let pz     = oz + t*dz
            let zScale = abs oz + abs (t*dz)
            if   abs pz <= tipTol * zScale then 0
            elif pz * signH > 0.0          then 1
            else                                -1

        if abs a <= relTol * aScale then
            // Degenerate quadratic: the ray direction is parallel to a generator line of the cone.
            if abs b <= relTol * bScale then
                if abs c <= relTol * cScale then
                    // All coefficients vanish: the line lies on the (double) cone, it is a generator.
                    // A generator is never horizontal, so dz <> 0 and the tip is at pz = 0:
                    let tTip = -oz / dz
                    // for t > tTip the Z offset from the tip is (t-tTip)*dz, valid if its sign matches signH:
                    XCone.LineOnCone (tTip, dz * signH > 0.0)
                else
                    XCone.NoIntersection // parallel to a generator but offset from the surface
            else
                // Linear equation: exactly one crossing with the double cone.
                // (It cannot be at the tip: through-tip + parallel-to-generator would be a generator,
                // which is caught above. So nappeOf decides between the two nappes.)
                let t = -c / b
                if nappeOf t >= 0 then XCone.IntersectingOne t
                else                   XCone.NoIntersection
        else
            let disc      = b*b - 4.0*a*c
            let discScale = b*b + abs (4.0*a*c)
            if disc > relTol * discScale then
                // Two distinct roots on the double cone. Computed the numerically stable way
                // to avoid catastrophic cancellation when b² >> 4ac:
                let sqrtDisc = sqrt disc
                let q  = -0.5 * (b + (if b >= 0.0 then sqrtDisc else -sqrtDisc))
                let r1 = q / a
                let r2 = c / q
                let t1, t2 = if r1 <= r2 then r1, r2 else r2, r1
                match nappeOf t1 >= 0, nappeOf t2 >= 0 with
                | true , true  -> XCone.Intersecting (t1, t2)
                | true , false -> XCone.IntersectingOne t1
                | false, true  -> XCone.IntersectingOne t2
                | false, false -> XCone.NoIntersection
            elif disc < -relTol * discScale then
                XCone.NoIntersection
            else
                // Double root: the line is tangent to the cone, or it passes through the tip.
                let t  = -0.5 * b / a
                let px = ox + t*dx
                let py = oy + t*dy
                let pz = oz + t*dz
                // Distance of the touch point from the tip, compared against the
                // magnitude of the coordinates involved in computing it:
                let distToTip = sqrt (px*px + py*py + pz*pz)
                let geoScale  = sqrt cScale + abs t * sqrt aScale
                if   distToTip <= tipTol * geoScale then XCone.ThroughTip t // the tip belongs to the cone
                elif pz * signH > 0.0               then XCone.Touching   t // tangent point on the valid nappe
                else                                     XCone.NoIntersection // tangent to the mirrored nappe only


    // /// <summary>Intersects a ray with an infinite double cone that has its axis on the Z-axis.</summary>
    // /// <param name="ray">The Line3D to intersect. It is considered as infinite ray.</param>
    // /// <param name="coneRadius">The radius of the cone at the base. Parallel to the XY plane.</param>
    // /// <param name="coneBaseZ">The Z coordinate of the cone base.</param>
    // /// <param name="coneTipZ">The Z coordinate of the cone tip.</param>
    // /// <returns>An XLine3D.XCone discriminated union representing the intersection result with parameter(s) on the line:
    // /// | NoIntersection: The line does not intersect the cone.
    // /// | Tangential: The line lies on the cone surface as a generator line through the tip.
    // /// | Touching of float: The line touches the cone at one point with a tolerance of 1e-6, contains the parameter on the line.
    // /// | ParallelInside of float: The line is parallel to a generator line of the cone but still pierces it at one point, contains the parameter on the line.
    // /// | Intersecting of float*float: The line intersects the cone at two points, contains both parameters on the line.
    // /// </returns>
    // static member intersectCone (ray:Line3D, coneRadius, coneBaseZ, coneTipZ) : XLine3D.XCone =
    //     let h = coneBaseZ-coneTipZ
    //     if isTooTiny( abs h )then
    //         fail $"XLine3D.intersectCone: cone has zero height: coneRadius: {coneRadius}, coneBaseZ: {coneBaseZ}, coneTipZ: {coneTipZ}"
    //     let lam = coneRadius / h
    //     let lam = lam * lam
    //     let vx = ray.VectorX
    //     let vy = ray.VectorY
    //     let vz = ray.VectorZ
    //     let f2 = lam*vz*vz - vx*vx - vy*vy
    //     let f1 = 2.*lam*ray.FromZ*vz - 2.*lam*vz*coneTipZ - 2.*vy*ray.FromY - 2.*ray.FromX*vx
    //     let f0 = lam*ray.FromZ*ray.FromZ + lam*coneTipZ*coneTipZ - 2.*ray.FromZ*coneTipZ*lam - ray.FromY*ray.FromY - ray.FromX*ray.FromX
    //     if isTooTiny(abs f2) then
    //         // The ray is parallel to a generator line of the cone, so the quadratic degenerates to the linear equation f1*t + f0 = 0.
    //         if isTooTiny(abs f1) then
    //             if isTooTiny(abs f0) then
    //                 XLine3D.XCone.Tangential // The ray lies on the cone surface as a generator line through the tip.
    //             else
    //                 XLine3D.XCone.NoIntersection // Parallel to a generator but offset, the ray never meets the cone.
    //         else
    //             XLine3D.XCone.ParallelInside (-f0 / f1) // Parallel to a generator but pierces the cone at one point.
    //     else
    //         let part = f1**2. - 4.* f2 * f0
    //         if part < 0.0 then
    //             XLine3D.XCone.NoIntersection
    //         else
    //             let sqrtPart = sqrt(part)
    //             let div = 1. / (2. * f2)
    //             let u = (-f1 + sqrtPart) * div
    //             let v = (-f1 - sqrtPart) * div
    //             if isTooTiny(abs(u-v)) then
    //                 XLine3D.XCone.Touching ((u+v)*0.5)
    //             else
    //                 XLine3D.XCone.Intersecting (u, v)
