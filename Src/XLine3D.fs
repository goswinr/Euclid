
namespace Euclid

open System
open UtilEuclid
open EuclidErrors


/// A module containing the result types for 3D Line-Line-Intersections
/// , 3D Line-Line relationship queries,
/// and Line-Cone intersections.
[<AutoOpen>]
module XLine3D =


    /// The result of a line-cone intersection test.
    /// This is the return type of the function Intersection.lineCone.
    [<RequireQualifiedAccess>]
    type XCone =
        | NoIntersection
        | Tangential
        | Touching of float
        | Intersecting of float*float


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

        /// Touching at both at the other Start or End. Lines are identical but in opposite orientation.
        | IdenticalFlipped



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
        clParamRayPt(ln.FromX, ln.FromY, ln.FromZ, ln.VectorX        , ln.VectorY        , ln.VectorZ        , x, y, z) |> clampBetweenZeroAndOne

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
        let t = clParamRayPt(pAx, pAy, pAz, vAx, vAy, vAz, x, y, z) |> clampBetweenZeroAndOne
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

    /// Given the start and end points of two finite lines A and B.
    /// Returns the parameters of the closest end points between the two lines.
    let inline internal projectEndsBackAndForth (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                                  vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): float*float*float*float =
        let uAs = clParamRayPt (pAx, pAy, pAz, vAx, vAy, vAz, pBx, pBy, pBz) |> clampBetweenZeroAndOne // parameter on line A of the projection of start point of line B
        let x = pAx + uAs * vAx // X coordinate of the projection of start point of line B on line A
        let y = pAy + uAs * vAy // Y coordinate of the projection of start point of line B on line A
        let z = pAz + uAs * vAz // Z coordinate of the projection of start point of line B on line A
        let uBs = clParamRayPt (pBx, pBy, pBz, vBx, vBy, vBz, x, y, z) |> clampBetweenZeroAndOne // parameter on line B of the projection back
        // project the end point from line B onto line A and back to line B
        let eBx = pBx + vBx // end point of line B
        let eBy = pBy + vBy // end point of line B
        let eBz = pBz + vBz // end point of line B
        let uAe = clParamRayPt (pAx, pAy, pAz, vAx, vAy, vAz, eBx, eBy, eBz) |> clampBetweenZeroAndOne
        let xE = pAx + uAe * vAx // X coordinate of the projection of end point of line B on line A
        let yE = pAy + uAe * vAy // Y coordinate of the projection of end point of line B on line A
        let zE = pAz + uAe * vAz // Z coordinate of the projection of end point of line B on line A
        let uBe = clParamRayPt (pBx, pBy, pBz, vBx, vBy, vBz, xE, yE, zE) |> clampBetweenZeroAndOne
        uAs, uBs, uAe, uBe


    /// Only for apart Lines (or skew lines), not for parallel.
    /// Returns the squared distance of the closest points between two finite lines A and B that are not intersecting nor parallel.
    let inline internal sqDistance (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                     vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): float =
        let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)
        // get point params at these parameters:
        let clAsX = pAx + uAs * vAx
        let clAsY = pAy + uAs * vAy
        let clAsZ = pAz + uAs * vAz
        let clBsX = pBx + uBs * vBx
        let clBsY = pBy + uBs * vBy
        let clBsZ = pBz + uBs * vBz
        let distSqStart = sq (clAsX - clBsX) + sq (clAsY - clBsY) + sq (clAsZ - clBsZ)
        let clAeX = pAx + uAe * vAx
        let clAeY = pAy + uAe * vAy
        let clAeZ = pAz + uAe * vAz
        let clBeX = pBx + uBe * vBx
        let clBeY = pBy + uBe * vBy
        let clBeZ = pBz + uBe * vBz
        let distSqEnd = sq (clAeX - clBeX) + sq (clAeY - clBeY) + sq (clAeZ - clBeZ)
        min distSqStart distSqEnd

    /// Only for apart Lines (or skew lines), not for parallel.
    /// Returns the parameters and the square distance of the closest points between two finite lines A and B that are not intersecting nor parallel.
    let inline internal closestParams (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                        vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): float*float*float =
        let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)
        // get point params at these parameters:
        let clAsX = pAx + uAs * vAx
        let clAsY = pAy + uAs * vAy
        let clAsZ = pAz + uAs * vAz
        let clBsX = pBx + uBs * vBx
        let clBsY = pBy + uBs * vBy
        let clBsZ = pBz + uBs * vBz
        let distSqStart = sq (clAsX - clBsX) + sq (clAsY - clBsY) + sq (clAsZ - clBsZ)
        let clAeX = pAx + uAe * vAx
        let clAeY = pAy + uAe * vAy
        let clAeZ = pAz + uAe * vAz
        let clBeX = pBx + uBe * vBx
        let clBeY = pBy + uBe * vBy
        let clBeZ = pBz + uBe * vBz
        let distSqEnd = sq (clAeX - clBeX) + sq (clAeY - clBeY) + sq (clAeZ - clBeZ)
        if distSqStart < distSqEnd then
            uAs, uBs, distSqStart
        else
            uAe, uBe, distSqEnd

    /// Only for apart Lines (or skew lines), not for parallel.
    /// Returns the closest points and the square distance between two finite lines A and B that are not intersecting nor parallel.
    let inline internal closestPts (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                     vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): Pnt*Pnt*float =
        let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)
        // get point params at these parameters:
        let clAsX = pAx + uAs * vAx
        let clAsY = pAy + uAs * vAy
        let clAsZ = pAz + uAs * vAz
        let clBsX = pBx + uBs * vBx
        let clBsY = pBy + uBs * vBy
        let clBsZ = pBz + uBs * vBz
        let distSqStart = sq (clAsX - clBsX) + sq (clAsY - clBsY) + sq (clAsZ - clBsZ)
        let clAeX = pAx + uAe * vAx
        let clAeY = pAy + uAe * vAy
        let clAeZ = pAz + uAe * vAz
        let clBeX = pBx + uBe * vBx
        let clBeY = pBy + uBe * vBy
        let clBeZ = pBz + uBe * vBz
        let distSqEnd = sq (clAeX - clBeX) + sq (clAeY - clBeY) + sq (clAeZ - clBeZ)
        if distSqStart < distSqEnd then
            Pnt(clAsX, clAsY, clAsZ), Pnt(clBsX, clBsY, clBsZ), distSqStart
        else
            Pnt(clAeX, clAeY, clAeZ), Pnt(clBeX, clBeY, clBeZ), distSqEnd

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

open XLine3D

/// Provides static methods for computing 3D line intersections and closest approaches.
/// Some functions return Discriminated Unions from the XLine3D module.
[<RequireQualifiedAccess>]
type XLine3D =

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
    static member inline parameterA (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
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
        XLine3D.parameterA (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Returns the closest approach parameter on the first ray (A) where ray A and ray B come closest.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The parameter on lineA at the closest approach with lineB.
    ///  NaN if both start points coincide and vectors are exactly parallel (0.0/0.0).
    ///  Positive/Negative Infinity for almost parallel lines.</returns>
    static member inline parameterA (lineA: Line3D, lineB: Line3D): float =
        XLine3D.parameterA (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                            lineA.VectorX, lineA.VectorY, lineA.VectorZ,
                            lineB.VectorX, lineB.VectorY, lineB.VectorZ)



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
    static member inline parameters (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
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
        XLine3D.parameters (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Returns the closest approach parameters on both rays.
    /// Returns NaN or Infinity for zero length or parallel lines.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> A (t,u) tuple where t is the parameter on line A and u on line B at their closest approach.
    ///  These are NaN if A and B points are identical and the vectors are exactly parallel (zero divided by zero).
    ///  These are positive or negative Infinity for almost parallel lines.</returns>
    static member inline parameters(lineA: Line3D, lineB: Line3D): float*float =
        XLine3D.parameters(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,
                           lineA.VectorX, lineA.VectorY, lineA.VectorZ,
                           lineB.VectorX, lineB.VectorY, lineB.VectorZ)





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
    /// <param name="tolerance" > Is an optional distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    /// <returns> TRUE if the lines are coincident and overlap or touch.
    ///  FALSE if the lines are not coincident or do not overlap nor touch.</returns>
    static member doOverlap(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                                 vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                                 [<OPT;DEF(1e-6)>] tolerance:float ) : bool =
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
        XLine3D.doOverlap(pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, tolerance)

    /// <summary> Checks if 3D lines are not only parallel and coincident but are also overlapping or at least touching at their ends.</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <param name="tolerance" > Is an optional squared distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between parallel lines is less than this value.</param>
    /// <returns> TRUE if the lines are coincident and overlap or touch.
    ///  FALSE if the lines are not coincident or do not overlap nor touch.</returns>
    static member inline doOverlap(lineA:Line3D, lineB:Line3D, [<OPT;DEF(1e-6)>] tolerance:float ) : bool =
        XLine3D.doOverlap(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ,lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, tolerance)


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
    /// <param name="maxSkewDistance" > Is an optional maximum distance tolerance. 1e-6 by default.
    /// Used for checking if the distance between the two lines at their closest approach is less than this value.</param>
    /// <returns> TRUE if both rays intersect within the given maximum skew distance.
    ///  FALSE if the rays are parallel, coincident or the closest approach distance is larger than the given tolerance.</returns>
    static member inline doRaysIntersect (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                         vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,[<OPT;DEF(1e-6)>] maxSkewDistance:float): bool =
        let tA = XLine3D.parameterA(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)
        if tA > -1e12 && tA < 1e12  then
            let clPtAx =  pAx + tA * vAx
            let clPtAy =  pAy + tA * vAy
            let clPtAz =  pAz + tA * vAz
            let d = sqRayPtDist(pBx, pBy, pBz, vBx, vBy, vBz, clPtAx, clPtAy, clPtAz)
            d < maxSkewDistance * maxSkewDistance
        else
            false

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
        XLine3D.doRaysIntersect (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, maxSkewDistance)

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
        XLine3D.doRaysIntersect (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance)




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
    /// or None if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryClosestParameterRayA (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                                  vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): float option =
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
            Some t
        else
            None

    /// <summary> Returns the closest approach parameter on the first ray (A) where ray A and ray B come closest.</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The parameter on line A at the closest approach with line B if it is smaller than 1e12 in absolute value,
    /// or None if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryClosestParameterRayA (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec): float option =
        XLine3D.tryClosestParameterRayA (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Returns the closest approach parameter on the first ray (A) where ray A and ray B come closest.</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <returns> The parameter on lineA at the closest approach with lineB if it is smaller than 1e12 in absolute value,
    /// or None if the parameter is bigger than 1e12 or if they are parallel or coincident.</returns>
    static member inline tryClosestParameterRayA (lineA:Line3D, lineB:Line3D): float option =
        XLine3D.tryClosestParameterRayA (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ)



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
    /// or None if the parameter is bigger than 1e12 ( = the lines are far away or parallel) or if they are exactly parallel or coincident.</returns>
    static member inline tryClosestPntRayA (pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                          vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float): Pnt option =
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
            Some <| Pnt(pAx + t * vAx, pAy + t * vAy, pAz + t * vAz)
        else
            None

    /// <summary> Tries to get closest approach point of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="pA"> The start point of the first ray.</param>
    /// <param name="pB"> The start point of the second ray.</param>
    /// <param name="vA"> The direction vector of the first ray.</param>
    /// <param name="vB"> The direction vector of the second ray.</param>
    /// <returns> The point at which the two rays come closest (on Line A) if the parameter on Line A is smaller than 1e12 in absolute value,
    /// or None if the parameter is bigger than 1e12 ( = the lines are far away or parallel) or if they are exactly parallel or coincident.</returns>
    static member inline tryClosestPntRayA (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec): Pnt option =
        XLine3D.tryClosestPntRayA (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Tries to get closest approach point of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <returns> The point at which the two rays come closest (on Line A, first Line) if the parameter on Line A is smaller than 1e12 in absolute value,
    /// or None if the parameter is bigger than 1e12 ( = the lines are far away or parallel) or if they are exactly parallel or coincident.</returns>
    static member inline tryClosestPntRayA (lineA: Line3D, lineB: Line3D): Pnt option =
        XLine3D.tryClosestPntRayA (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ)


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
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, None is returned.</param>
    /// <returns> An intersection point on ray A, or None if the rays are skew, parallel, or too short.</returns>
    static member tryIntersectRay(  pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                    vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                    [<OPT;DEF(1e-6)>] maxSkewDistance:float,
                                    [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                    [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                    ) : Pnt option =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            None
        elif isTooShort(vBx, vBy, vBz, tooShortTolerance) then
            None
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMag = sqrt(crossX * crossX + crossY * crossY + crossZ * crossZ)
            let dot = vAx * vBx + vAy * vBy + vAz * vBz
            let tan = crossMag / dot
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let crossMagSq = crossMag * crossMag
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let dz = pBz - pAz // difference in start points
                let numerX_t = dy * vBz - dz * vBy
                let numerY_t = dz * vBx - dx * vBz
                let numerZ_t = dx * vBy - dy * vBx
                let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
                let t = numerator_t / crossMagSq
                let ptAx = pAx + t * vAx
                let ptAy = pAy + t * vAy
                let ptAz = pAz + t * vAz
                let d = sqRayPtDist(pBx, pBy, pBz, vBx, vBy, vBz, ptAx, ptAy, ptAz)
                if d < maxSkewDistance*maxSkewDistance then
                    Some <|  Pnt(ptAx, ptAy, ptAz)
                else // skew lines
                    None
            else
                None



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
    ///  If one or both lines are shorter than this, None is returned.</param>
    /// <returns> An intersection point on ray A, or None if the rays are skew, parallel, or too short.</returns>
    static member inline tryIntersectRay(  pA:Pnt, pB:Pnt, vA:Vec, vB:Vec,[<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : Pnt option =
        XLine3D.tryIntersectRay( pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z,maxSkewDistance, tangent, tooShortTolerance)

    /// <summary> Tries to get an actual intersection point of two rays (3D lines treated as infinite).</summary>
    /// <param name="lineA"> First ray.</param>
    /// <param name="lineB"> Second ray.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, None is returned.</param>
    /// <returns> An intersection point on ray A, or None if the rays are skew, parallel, or too short.</returns>
    static member inline tryIntersectRay(lineA:Line3D, lineB:Line3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : Pnt option =
        XLine3D.tryIntersectRay(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)


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
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, None is returned.</param>
    /// <returns> An intersection point on line A, or None if the segments are skew, parallel, or too short.</returns>
    static member tryIntersect(  pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                [<OPT;DEF(1e-6)>] maxSkewDistance:float,
                                [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                ) : Pnt option =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            None
        elif isTooShort(vBx, vBy, vBz, tooShortTolerance) then
            None
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMag = sqrt(crossX * crossX + crossY * crossY + crossZ * crossZ)
            let dot = vAx * vBx + vAy * vBy + vAz * vBz
            let tan = crossMag / dot
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let crossMagSq = crossMag * crossMag
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let dz = pBz - pAz // difference in start points
                let numerX_t = dy * vBz - dz * vBy
                let numerY_t = dz * vBx - dx * vBz
                let numerZ_t = dx * vBy - dy * vBx
                let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
                let t = numerator_t / crossMagSq
                if t > -1e-6 && t < 1.0 + 1e-6 then
                    let numerX_u = dy * vAz - dz * vAy
                    let numerY_u = dz * vAx - dx * vAz
                    let numerZ_u = dx * vAy - dy * vAx
                    let numerator_u = numerX_u * crossX + numerY_u * crossY + numerZ_u * crossZ
                    let u = numerator_u / crossMagSq
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
                            Some <|  Pnt(ptAx, ptAy, ptAz)
                        else
                            None
                    else
                        None
                else
                    None
            else
                None


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
    ///  If one or both lines are shorter than this, None is returned.</param>
    /// <returns> An intersection point on line A, or None if the segments are skew, parallel, or too short.</returns>
    static member inline tryIntersect(  pA:Pnt, pB:Pnt, vA:Vec, vB:Vec,[<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : Pnt option =
        XLine3D.tryIntersect( pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z,maxSkewDistance, tangent, tooShortTolerance)


    /// <summary> Tries to get an actual intersection point of two finite 3D lines.</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting.</param>
    /// <param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, None is returned.</param>
    /// <returns> An intersection point on line A, or None if the segments are skew, parallel, or too short.</returns>
    static member inline tryIntersect(lineA:Line3D, lineB:Line3D, [<OPT;DEF(1e-6)>] maxSkewDistance:float, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : Pnt option =
        XLine3D.tryIntersect(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)





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
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An XRayParam Discriminated Union with the following cases:
    /// | SkewOrX of float*float  (parameters on both lines at closest approach)
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member getRayClosestParam(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                          vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                          [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                          [<OPT;DEF(1e-6)>] tooShortTolerance:float
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
            let crossMag = sqrt(crossX * crossX + crossY * crossY + crossZ * crossZ)
            let dot = vAx * vBx + vAy * vBy + vAz * vBz
            let tan = crossMag / dot
            if abs !^ tan > tangent then
                let crossMagSq = crossMag * crossMag
                let dx = pBx - pAx
                let dy = pBy - pAy
                let dz = pBz - pAz
                let numerX_t = dy * vBz - dz * vBy
                let numerY_t = dz * vBx - dx * vBz
                let numerZ_t = dx * vBy - dy * vBx
                let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
                let t = numerator_t / crossMagSq
                let numerX_u = dy * vAz - dz * vAy
                let numerY_u = dz * vAx - dx * vAz
                let numerZ_u = dx * vAy - dy * vAx
                let numerator_u = numerX_u * crossX + numerY_u * crossY + numerZ_u * crossZ
                let u = numerator_u / crossMagSq
                XRayParam.SkewOrX ((t,u))
            else
                XRayParam.Parallel

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
        XLine3D.getRayClosestParam (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, tangent, tooShortTolerance)

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
        XLine3D.getRayClosestParam(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, tangent, tooShortTolerance)



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
    /// | TooShortB</returns>
    static member getRayIntersection(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                     vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                     [<OPT;DEF(1e-6)>] maxSkewDistance:float,
                                     [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                     [<OPT;DEF(1e-6)>] tooShortTolerance:float
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
            let crossMag = sqrt(crossX * crossX + crossY * crossY + crossZ * crossZ)
            let dot = vAx * vBx + vAy * vBy + vAz * vBz
            let tan = crossMag / dot
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let crossMagSq = crossMag * crossMag
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let dz = pBz - pAz // difference in start points
                let numerX_t = dy * vBz - dz * vBy
                let numerY_t = dz * vBx - dx * vBz
                let numerZ_t = dx * vBy - dy * vBx
                let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
                let t = numerator_t / crossMagSq
                let numerX_u = dy * vAz - dz * vAy
                let numerY_u = dz * vAx - dx * vAz
                let numerZ_u = dx * vAy - dy * vAx
                let numerator_u = numerX_u * crossX + numerY_u * crossY + numerZ_u * crossZ
                let u = numerator_u / crossMagSq
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
        XLine3D.getRayIntersection(pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, maxSkewDistance, tangent, tooShortTolerance)

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
        XLine3D.getRayIntersection(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)



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
    static member getIntersectionParam(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                       vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                       [<OPT;DEF(1e-6)>] maxSkewDistance:float,
                                       [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                       [<OPT;DEF(1e-6)>] tooShortTolerance:float
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
            let crossMag = sqrt(crossX * crossX + crossY * crossY + crossZ * crossZ)
            let dot = vAx * vBx + vAy * vBy + vAz * vBz // Dot product of vectors
            let tan = crossMag / dot // tangent of the angle between the two vectors
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let crossMagSq = crossMag * crossMag
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let dz = pBz - pAz // difference in start points
                let numerX_t = dy * vBz - dz * vBy
                let numerY_t = dz * vBx - dx * vBz
                let numerZ_t = dx * vBy - dy * vBx
                let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
                let t = numerator_t / crossMagSq // embrace float math, this can be NaN or Infinity
                let numerX_u = dy * vAz - dz * vAy
                let numerY_u = dz * vAx - dx * vAz
                let numerZ_u = dx * vAy - dy * vAx
                let numerator_u = numerX_u * crossX + numerY_u * crossY + numerZ_u * crossZ
                let u = numerator_u / crossMagSq // embrace float math, this can be NaN or Infinity
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
        XLine3D.getIntersectionParam (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, maxSkewDistance, tangent, tooShortTolerance)

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
        XLine3D.getIntersectionParam(lnA.FromX, lnA.FromY, lnA.FromZ, lnB.FromX, lnB.FromY, lnB.FromZ, lnA.VectorX, lnA.VectorY, lnA.VectorZ, lnB.VectorX, lnB.VectorY, lnB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)



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
    /// <param name="maxSkewDistance"> The maximum allowed distance at closest approach to still consider the lines intersecting. Default is 1e-6.</param>
    ///<param name="tangent" > Is an optional tangent of the maximum allowed angle between the two line vectors.
    ///  The default value is '0.00436' this corresponds to approx 0.25 degree. Below this angle the lines are considered parallel.
    ///  Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An XPt Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Apart
    /// | Parallel
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member getIntersection(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                  vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                  [<OPT;DEF(1e-6)>] maxSkewDistance:float,
                                  [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                  [<OPT;DEF(1e-6)>] tooShortTolerance:float
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
            let crossMag = sqrt(crossX * crossX + crossY * crossY + crossZ * crossZ)
            let dot = vAx * vBx + vAy * vBy + vAz * vBz // Dot product of vectors
            let tan = crossMag / dot // tangent of the angle between the two vectors
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let crossMagSq = crossMag * crossMag
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let dz = pBz - pAz // difference in start points
                let numerX_t = dy * vBz - dz * vBy
                let numerY_t = dz * vBx - dx * vBz
                let numerZ_t = dx * vBy - dy * vBx
                let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
                let t = numerator_t / crossMagSq // embrace float math, this can be NaN or Infinity
                let numerX_u = dy * vAz - dz * vAy
                let numerY_u = dz * vAx - dx * vAz
                let numerZ_u = dx * vAy - dy * vAx
                let numerator_u = numerX_u * crossX + numerY_u * crossY + numerZ_u * crossZ
                let u = numerator_u / crossMagSq // embrace float math, this can be NaN or Infinity
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
        XLine3D.getIntersection(pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, maxSkewDistance, tangent, tooShortTolerance)

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
        XLine3D.getIntersection(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, maxSkewDistance, tangent, tooShortTolerance)



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
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An ClParams Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Skew of float*float*float
    /// | Parallel of float*float
    /// | Apart of float*float*float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member getClosestParameters(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                       vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                       [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                       [<OPT;DEF(1e-6)>] tooShortTolerance:float
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
            let crossMag = sqrt(crossX * crossX + crossY * crossY + crossZ * crossZ)
            let dot = vAx * vBx + vAy * vBy + vAz * vBz // Dot product of vectors
            let tan = crossMag / dot // tangent of the angle between the two vectors
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let crossMagSq = crossMag * crossMag
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let dz = pBz - pAz // difference in start points
                let numerX_t = dy * vBz - dz * vBy
                let numerY_t = dz * vBx - dx * vBz
                let numerZ_t = dx * vBy - dy * vBx
                let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
                let t = numerator_t / crossMagSq // embrace float math, this can be NaN or Infinity
                let numerX_u = dy * vAz - dz * vAy
                let numerY_u = dz * vAx - dx * vAz
                let numerZ_u = dx * vAy - dy * vAx
                let numerator_u = numerX_u * crossX + numerY_u * crossY + numerZ_u * crossZ
                let u = numerator_u / crossMagSq // embrace float math, this can be NaN or Infinity
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
                    ClParams.Apart <| closestParams(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)
            else
                let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)
                ClParams.Parallel (
                    (uAs + uAe) * 0.5,
                    (uBs + uBe) * 0.5 )

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
    /// <returns> An ClParams Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Skew of float*float*float
    /// | Parallel of float*float
    /// | Apart of float*float*float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getClosestParameters (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float): ClParams =
        XLine3D.getClosestParameters (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, tangent, tooShortTolerance)

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
    /// <returns> An ClParams Discriminated Union with the following cases:
    /// | Intersect of float*float
    /// | Skew of float*float*float
    /// | Parallel of float*float
    /// | Apart of float*float*float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getClosestParameters (lineA:Line3D, lineB:Line3D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float): ClParams =
        XLine3D.getClosestParameters (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, tangent, tooShortTolerance)



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
    /// <param name="tangent"> This is an optional tolerance for considering lines parallel.
    /// This is a precomputed tangent of the maximum allowed angle between the two line vectors.
    /// The default value is '0.00436' this corresponds to approx 0.25 degree. Below this the middle point of any overlapping segment is returned.
    /// Use the module Euclid.UtilEuclid.Tangent to set another tolerance here.</param>
    /// <param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    /// <returns> An ClPts Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Parallel of Pnt * Pnt
    /// | Apart of Pnt * Pnt * float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member getClosestPoints(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                   vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float,
                                   [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>,
                                   [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                   ): ClPts =
        if isTooShort(vAx, vAy, vAz, tooShortTolerance) then
            if isTooShort(vBx, vBy, vBz, tooShortTolerance) then
                ClPts.TooShortBoth
            else
                ClPts.TooShortA
        elif
            isTooShort(vBx, vBy, vBz, tooShortTolerance) then
                ClPts.TooShortB
        else
            // Cross product of vectors vA × vB
            let crossX = vAy * vBz - vAz * vBy
            let crossY = vAz * vBx - vAx * vBz
            let crossZ = vAx * vBy - vAy * vBx
            let crossMag = sqrt(crossX * crossX + crossY * crossY + crossZ * crossZ)
            let dot = vAx * vBx + vAy * vBy + vAz * vBz // Dot product of vectors
            let tan = crossMag / dot // tangent of the angle between the two vectors
            if abs !^ tan > tangent then // abs check needed, handles the NaN case correctly of zero length lines too
                let crossMagSq = crossMag * crossMag
                let dx = pBx - pAx // difference in start points
                let dy = pBy - pAy // difference in start points
                let dz = pBz - pAz // difference in start points
                let numerX_t = dy * vBz - dz * vBy
                let numerY_t = dz * vBx - dx * vBz
                let numerZ_t = dx * vBy - dy * vBx
                let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
                let t = numerator_t / crossMagSq // embrace float math, this can be NaN or Infinity
                let numerX_u = dy * vAz - dz * vAy
                let numerY_u = dz * vAx - dx * vAz
                let numerZ_u = dx * vAy - dy * vAx
                let numerator_u = numerX_u * crossX + numerY_u * crossY + numerZ_u * crossZ
                let u = numerator_u / crossMagSq // embrace float math, this can be NaN or Infinity
                if t >= 0.0 && t <= 1.0 && u >= 0.0 && u <= 1.0 then
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
                        ClPts.Intersect <| Pnt(ptAx, ptAy, ptAz)
                    else // skew lines
                        ClPts.Skew (Pnt(ptAx, ptAy, ptAz), Pnt(ptBx, ptBy, ptBz), sqDist)
                else
                    ClPts.Apart <| closestPts(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)
            else
                let uAs, uBs, uAe, uBe = projectEndsBackAndForth (pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)
                let tA = (uAs + uAe) * 0.5
                let tB = (uBs + uBe) * 0.5
                let a = Pnt(pAx + tA * vAx, pAy + tA * vAy, pAz + tA * vAz)
                let b = Pnt(pBx + tB * vBx, pBy + tB * vBy, pBz + tB * vBz)
                ClPts.Parallel (a, b)

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
    /// <returns> An ClPts Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Parallel of Pnt * Pnt
    /// | Apart of Pnt * Pnt * float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getClosestPoints (pA:Pnt, pB:Pnt, vA:Vec, vB:Vec, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : ClPts =
        XLine3D.getClosestPoints (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z, tangent, tooShortTolerance)

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
    /// <returns> An ClPts Discriminated Union with the following cases:
    /// | Intersect of Pnt
    /// | Skew of Pnt * Pnt * float
    /// | Parallel of Pnt * Pnt
    /// | Apart of Pnt * Pnt * float
    /// | TooShortBoth
    /// | TooShortA
    /// | TooShortB </returns>
    static member inline getClosestPoints (lineA: Line3D, lineB: Line3D, [<OPT;DEF(Tangent.``0.25``)>] tangent:float<Tangent.tangent>, [<OPT;DEF(1e-6)>] tooShortTolerance:float) : ClPts =
        XLine3D.getClosestPoints (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ, tangent, tooShortTolerance)



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
    static member getSqDistance(pAx:float, pAy:float, pAz:float, pBx:float, pBy:float, pBz:float,
                                vAx:float, vAy:float, vAz:float, vBx:float, vBy:float, vBz:float ): float =
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
        let numerX_t = dy * vBz - dz * vBy
        let numerY_t = dz * vBx - dx * vBz
        let numerZ_t = dx * vBy - dy * vBx
        // Dot product of (pB - pA) × vB with vA × vB
        let numerator_t = numerX_t * crossX + numerY_t * crossY + numerZ_t * crossZ
        let t = numerator_t / crossMagSq // embrace float math, this can be NaN or Infinity
        let numerX_u = dy * vAz - dz * vAy
        let numerY_u = dz * vAx - dx * vAz
        let numerZ_u = dx * vAy - dy * vAx
        let numerator_u = numerX_u * crossX + numerY_u * crossY + numerZ_u * crossZ
        let u = numerator_u / crossMagSq // embrace float math, this can be NaN or Infinity
        if t >= 0.0 && t <= 1.0 && u >= 0.0 && u <= 1.0 then // handles NaN and Infinity correctly too
            let ptAx = pAx + t * vAx
            let ptAy = pAy + t * vAy
            let ptAz = pAz + t * vAz
            let ptBx = pBx + u * vBx
            let ptBy = pBy + u * vBy
            let ptBz = pBz + u * vBz
            let dx2 = ptAx - ptBx
            let dy2 = ptAy - ptBy
            let dz2 = ptAz - ptBz
            dx2*dx2 + dy2*dy2 + dz2*dz2
        else
            // checking for parallel lines would not help here.
            // we need to call projectEndsBackAndForth anyway to find the closest points on the finite lines
            sqDistance(pAx, pAy, pAz, pBx, pBy, pBz, vAx, vAy, vAz, vBx, vBy, vBz)

    /// <summary> Gets the squared distance between two finite lines. Works on parallel and skew lines too.</summary>
    /// <param name="pA"> The start point of the first line.</param>
    /// <param name="pB"> The start point of the second line.</param>
    /// <param name="vA"> The direction vector of the first line.</param>
    /// <param name="vB"> The direction vector of the second line.</param>
    /// <returns> the squared distance between the two lines </returns>
    static member inline getSqDistance(pA:Pnt, pB:Pnt, vA:Vec, vB:Vec ): float =
        XLine3D.getSqDistance (pA.X, pA.Y, pA.Z, pB.X, pB.Y, pB.Z, vA.X, vA.Y, vA.Z, vB.X, vB.Y, vB.Z)

    /// <summary> Gets the squared distance between two finite lines. Works on parallel and skew lines too.</summary>
    /// <param name="lineA"> The first line.</param>
    /// <param name="lineB"> The second line.</param>
    /// <returns> the squared distance between the two lines </returns>
    static member inline getSqDistance(lineA: Line3D, lineB: Line3D ): float =
        XLine3D.getSqDistance (lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, lineA.VectorX, lineA.VectorY, lineA.VectorZ, lineB.VectorX, lineB.VectorY, lineB.VectorZ)



    // TODO add getEndsTouching overload for float and Pt Input ?


    /// <summary>Checks if the two finite 3D lines are touching each other at exactly one of their end points
    /// within the given tolerance.
    /// This will return a separate case (5 or 6) if the lines are touching on both points.</summary>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <param name="tolerance"> Is an optional distance tolerance. 1e-6 by default.</param>
    /// <returns>A Discriminated Union XEnds that describes the possible cases of two finite 3D lines touching at their ends:
    /// | NotTouching
    /// | StartA_StartB
    /// | EndA_EndB
    /// | EndA_StartB
    /// | StartA_EndB
    /// | Identical
    /// | IdenticalFlipped
    /// </returns>
    static member getEndsTouching (a:Line3D, b:Line3D, [<OPT;DEF(1e-6)>] tolerance:float) : XEnds =
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



    /// <summary>Intersects a ray with an infinite double cone that has its axis on the Z-axis.</summary>
    /// <param name="ray">The Line3D to intersect. It is considered as infinite ray.</param>
    /// <param name="coneRadius">The radius of the cone at the base. Parallel to the XY plane.</param>
    /// <param name="coneBaseZ">The Z coordinate of the cone base.</param>
    /// <param name="coneTipZ">The Z coordinate of the cone tip.</param>
    /// <returns>An XLine3D.XCone discriminated union representing the intersection result with parameter(s) on the line:
    /// | NoIntersection: The line does not intersect the cone.
    /// | Tangential: The line is tangent to the cone surface.
    /// | Touching of float: The line touches the cone at one point with a tolerance of 1e-6, contains the parameter on the line.
    /// | Intersecting of float*float: The line intersects the cone at two points, contains both parameters on the line.
    /// </returns>
    static member intersectCone (ray:Line3D, coneRadius, coneBaseZ, coneTipZ) : XLine3D.XCone =
        let h = coneBaseZ-coneTipZ
        if isTooTiny( abs h )then
            fail $"XLine3D.intersectCone: cone has zero height: coneRadius: {coneRadius}, coneBaseZ: {coneBaseZ}, coneTipZ: {coneTipZ}"
        let lam = coneRadius / h
        let lam = lam * lam
        let v = ray.Tangent
        let f2 = lam*v.Z*v.Z - v.X*v.X - v.Y*v.Y
        if isTooTiny(abs f2) then
            XLine3D.XCone.Tangential
        else
            let f1 = 2.*lam*ray.FromZ*v.Z - 2.*lam*v.Z*coneTipZ - 2.*v.Y*ray.FromY - 2.*ray.FromX*v.X
            let f0 = lam*ray.FromZ*ray.FromZ + lam*coneTipZ*coneTipZ - 2.*ray.FromZ*coneTipZ*lam - ray.FromY*ray.FromY - ray.FromX*ray.FromX
            let part = f1**2. - 4.* f2 * f0
            if part < 0.0 then
                XLine3D.XCone.NoIntersection
            else
                let sqrtPart = sqrt(part)
                let div = 1. / (2. * f2)
                let u = (-f1 + sqrtPart) * div
                let v = (-f1 - sqrtPart) * div
                if isTooTiny(abs(u-v)) then
                    XLine3D.XCone.Touching ((u+v)*0.5)
                else
                    XLine3D.XCone.Intersecting (u, v)
