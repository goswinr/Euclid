namespace Euclid

open System
open Euclid.UtilEuclid
open UtilEuclid
open EuclidErrors
open System.ComponentModel

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Line2D.
[<AutoOpen; EditorBrowsable(EditorBrowsableState.Never)>]
module AutoOpenLine2D =

  type Line2D with

    /// Checks if 2D line is parallel to the world X-axis. Ignoring orientation.
    /// The absolute deviation tolerance along the Y-axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsXAligned : bool =
        let x = abs ln.VectorX
        let y = abs ln.VectorY
        if isTooSmall (x+y) then
            failTooSmall "Line2D.IsXAligned" ln
        y < axisAlignmentTolerance

    /// Checks if 2D line is parallel to the world X-axis. Ignoring orientation.
    /// The absolute deviation tolerance along the Y-axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    static member inline isXAligned (l:Line2D) : bool =
        l.IsXAligned

    /// Checks if 2D line is parallel to the world Y-axis. Ignoring orientation.
    /// The absolute deviation tolerance along the X-axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsYAligned : bool =
        let x = abs ln.VectorX
        let y = abs ln.VectorY
        if isTooSmall (x+y) then
            failTooSmall "Line2D.IsYAligned" ln
        x < axisAlignmentTolerance

    /// Checks if 2D line is parallel to the world Y-axis. Ignoring orientation.
    /// The absolute deviation tolerance along the X-axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    static member inline isYAligned (l:Line2D) : bool =
        l.IsYAligned

    /// Check if the line has the same starting and ending point.
    member inline ln.IsZeroLength : bool =
        ln.ToX = ln.FromX &&
        ln.ToY = ln.FromY

    /// Check if the line has same starting and ending point.
    static member inline isZeroLength (l:Line2D) : bool =
        l.IsZeroLength

    /// Returns TRUE if line is shorter than tolerance.
    /// Or has NaN components.
    member inline ln.IsTiny tol : bool =
        not (ln.Length > tol) // use not(..) to catch NaN too

    /// Check if line is shorter than tolerance.
    /// Or contains a NaN value
    static member inline isTiny tol (l:Line2D) : bool =
        not (l.Length > tol)

    /// Returns TRUE if the line's square length is shorter than squared tolerance.
    /// Or has NaN components.
    member inline ln.IsTinySq tol : bool =
        not (ln.LengthSq > tol)

    /// Check if line is shorter than squared tolerance.
    /// Or contains a NaN value
    static member inline isTinySq tol (l:Line2D) : bool =
        not (l.LengthSq > tol)

    /// Evaluate line at a given parameter (parameters 0.0 to 1.0 are on the line).
    member inline ln.EvaluateAt (p:float) : Pt =
        Pt  (ln.FromX + ln.VectorX*p,
             ln.FromY + ln.VectorY*p)

    /// Evaluate line at a given parameter ( parameters 0.0 to 1.0 are on the line)
    static member inline evaluateAt t (ln:Line2D) : Pt =
        ln.EvaluateAt t

    /// Evaluate line at given parameters (parameters 0.0 to 1.0 are on the line).
    /// Returns a new line from evaluated points.
    /// Same as ln.SubLine(start, endParameter).
    member inline ln.Segment(start:float, endParameter:float) : Line2D =
        let fromX = ln.FromX
        let fromY = ln.FromY
        let x = ln.ToX-fromX
        let y = ln.ToY-fromY
        Line2D( fromX + x * start,
                fromY + y * start,
                fromX + x * endParameter ,
                fromY + y * endParameter )

    /// Returns new Line2D from point at Parameter a to point at Parameter b.
    /// Same as Line2D.subLine
    static member inline segment start endParameter (ln:Line2D) : Line2D =
        ln.Segment (start, endParameter)

    /// Evaluate line at given parameters (parameters 0.0 to 1.0 are on the line).
    /// Returns a new line from evaluated points.
    /// Same as ln.Segment(start, endParameter).
    member inline ln.SubLine (start:float, endParameter:float) : Line2D =
        ln.Segment (start, endParameter)

    /// Returns new Line2D from point at Parameter a to point at Parameter b.
    /// Same as Line2D.segment
    static member inline subLine start endParameter (ln:Line2D) : Line2D =
        ln.Segment (start, endParameter)

    /// Returns the length of the line segment from the start point to the given parameter.
    /// This length is negative if the parameter is negative.
    member inline ln.LengthTillParameter (p:float) : float =
        let x = ln.VectorX * p
        let y = ln.VectorY * p
        let l = sqrt(x*x + y*y)
        if p> 0.0 then l else -l

    /// Returns the length of the line segment from the start point to the given parameter.
    /// This length is negative if the parameter is negative.
    static member inline lengthTillParameter p (ln:Line2D) : float =
        ln.LengthTillParameter p

    /// Returns the length of the line segment from the given parameter till the line End.
    /// This length is negative if the parameter is bigger than 1.0.
    member inline ln.LengthFromParameter (t:float) : float =
        let p = 1.0-t
        let x = ln.VectorX * p
        let y = ln.VectorY * p
        let l = sqrt(x*x + y*y)
        if p> 0.0 then l else -l

    /// Returns the length of the line segment from the given parameter till the line End.
    /// This length is negative if the parameter is bigger than 1.0.
    static member inline lengthFromParameter t (ln:Line2D) : float =
        ln.LengthFromParameter t

    /// Returns the midpoint of the line,
    member inline ln.Mid : Pt =
        let x = (ln.ToX + ln.FromX)*0.5
        let y = (ln.ToY + ln.FromY)*0.5
        Pt(x, y)

    /// Get point at center of line.
    static member inline mid (ln:Line2D) : Pt =
        ln.Mid

    /// Returns the Line2D reversed.
    member inline ln.Reversed : Line2D =
        Line2D(ln.ToX, ln.ToY, ln.FromX, ln.FromY)

    /// Returns the Line2D reversed.
    static member inline reversed (ln:Line2D) : Line2D =
        ln.Reversed

    /// Extend 2D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.Extend (distAtStart:float, distAtEnd:float) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.Extend" ln
        Line2D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l)

    /// Extend 2D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extend (distAtStart:float) (distAtEnd:float) (ln:Line2D) : Line2D =
        ln.Extend(distAtStart, distAtEnd)

    /// Extend 2D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendStart (distAtStart:float) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendStart" ln
        Line2D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.ToX,
                ln.ToY)

    /// Extend 2D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendStart (distAtStart:float) (ln:Line2D) : Line2D =
        ln.ExtendStart distAtStart

    /// Extend 2D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendEnd (distAtEnd:float) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendEnd" ln
        Line2D( ln.FromX,
                ln.FromY,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l)

    /// Extend 2D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendEnd (distAtEnd:float) (ln:Line2D) : Line2D =
        ln.ExtendEnd(distAtEnd)

    /// Extend 2D line by relative amount at start and end.
    /// A relative amount of 0.5 extends the line by half its length on each side.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendRelative (relAtStart:float, relAtEnd:float) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendRelative" ln
        Line2D( ln.FromX - x*relAtStart,
                ln.FromY - y*relAtStart,
                ln.ToX   + x*relAtEnd,
                ln.ToY   + y*relAtEnd)

    /// Extend 2D line by relative amount at start and end.
    /// A relative amount of 0.5 extends the line by half its length on each side.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendRelative (relAtStart:float) (relAtEnd:float) (ln:Line2D) : Line2D =
        ln.ExtendRelative(relAtStart, relAtEnd)

    /// Extend 2D line by relative amount at start.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendStartRelative (relAtStart:float) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendStartRelative" ln
        Line2D( ln.FromX - x*relAtStart,
                ln.FromY - y*relAtStart,
                ln.ToX,
                ln.ToY)

    /// Extend 2D line by relative amount at start.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendStartRelative (relAtStart:float) (ln:Line2D) : Line2D =
        ln.ExtendStartRelative(relAtStart)

    /// Extend 2D line by relative amount at end.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendEndRelative (relAtEnd:float) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendEndRelative" ln
        Line2D( ln.FromX,
                ln.FromY,
                ln.ToX   + x*relAtEnd,
                ln.ToY   + y*relAtEnd)

    /// Extend 2D line by relative amount at end.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendEndRelative (relAtEnd:float) (ln:Line2D) : Line2D =
        ln.ExtendEndRelative(relAtEnd)

    /// Shrink 2D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.Shrink (distAtStart:float, distAtEnd:float) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.Shrink" ln
        Line2D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l)

    /// Shrink 2D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrink (distAtStart:float) (distAtEnd:float) (ln:Line2D) : Line2D =
        ln.Shrink(distAtStart, distAtEnd)

    /// Shrink 2D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ShrinkStart (distAtStart:float) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ShrinkStart" ln
        Line2D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.ToX,
                ln.ToY)

    /// Shrink 2D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrinkStart (distAtStart:float) (ln:Line2D) : Line2D =
        ln.ShrinkStart(distAtStart)

    /// Shrink 2D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ShrinkEnd (distAtEnd:float) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ShrinkEnd" ln
        Line2D( ln.FromX,
                ln.FromY,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l)

    /// Shrink 2D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrinkEnd (distAtEnd:float) (ln:Line2D) : Line2D =
        ln.ShrinkEnd(distAtEnd)

    /// Returns a Line2D moved by a vector. Same as Line2D.move.
    member inline ln.Move (v:Vc) : Line2D =
        Line2D( ln.FromX+v.X,
                ln.FromY+v.Y,
                ln.ToX+v.X,
                ln.ToY+v.Y)

    /// Move a Line2D by a vector.
    static member inline move (v:Vc) (ln:Line2D)  : Line2D =
        ln.Move(v)

    /// Returns a Line2D moved by a given distance in X direction.
    member inline ln.MoveX (distance:float) : Line2D =
        Line2D( ln.FromX+distance,
                ln.FromY,
                ln.ToX+distance,
                ln.ToY)

    /// Returns a Line2D moved by a given distance in X direction.
    static member inline moveX (distance:float) (ln:Line2D)  : Line2D =
        ln.MoveX(distance)

    /// Returns a Line2D moved by a given distance in Y direction.
    member inline ln.MoveY (distance:float) : Line2D =
        Line2D( ln.FromX,
                ln.FromY+distance,
                ln.ToX,
                ln.ToY+distance)

    /// Returns a Line2D moved by a given distance in Y direction.
    static member inline moveY (distance:float) (ln:Line2D)  : Line2D =
        ln.MoveY(distance)

    /// Assumes the Line2D to be an infinite ray!
    /// Returns the parameter at which a point is closest to the ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Fails on curves shorter than 1e-6 units. (ln.ClosestParameter does not)
    member ln.RayClosestParameterXY (x:float, y:float) : float =
        // http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        // https://www.youtube.com/watch?v=PMltMdi1Wzg
        let vx = ln.FromX - ln.ToX
        let vy = ln.FromY - ln.ToY
        let lenSq = vx*vx + vy*vy
        if isTooSmallSq(lenSq) then // the parameter is infinite so we have to fail
            failTooSmall2 "Line2D.RayClosestParameter" ln (Pt(x, y))
        let u = ln.FromX - x
        let v = ln.FromY - y
        let dot = vx*u + vy*v
        dot / lenSq

    /// Assumes the Line2D to be an infinite ray!
    /// Returns the parameter at which a point is closest to the ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Fails on curves shorter than 1e-6 units. (ln.ClosestParameter does not)
    member ln.RayClosestParameter (pt:Pt) : float =
        ln.RayClosestParameterXY (pt.X, pt.Y)

    /// Assumes Line2D to be an infinite ray!
    /// Returns the parameter at which a point is closest to the infinite ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    static member inline rayClosestParameterXY (x:float) (y:float) (ln:Line2D) : float =
        ln.RayClosestParameterXY (x, y)

    /// Assumes Line2D to be an infinite ray!
    /// Returns the parameter at which a point is closest to the infinite ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    static member inline rayClosestParameter (p:Pt) (ln:Line2D) : float =
        ln.RayClosestParameter p

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    /// Does not fail on very short curves.
    member inline ln.ClosestParameterXY (x:float, y:float) : float =
        // http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        // https://www.youtube.com/watch?v=PMltMdi1Wzg
        let vx = ln.FromX - ln.ToX
        let vy = ln.FromY - ln.ToY
        let u = ln.FromX - x
        let v = ln.FromY - y
        let dot = vx*u + vy*v
        let lenSq = vx*vx + vy*vy
        if isTooSmallSq (lenSq) then // if the parameter is infinite we can still return 0.0 or 1.0 since that is our range
            if dot < 0.0 then 0.0 else 1.0
        else
            dot / lenSq |> clamp01

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    /// Does not fail on very short curves.
    member inline ln.ClosestParameter (p:Pt) : float =
        ln.ClosestParameterXY (p.X, p.Y)

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameterXY (x:float) (y:float) (ln:Line2D) : float =
        ln.ClosestParameterXY (x, y)

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameter (p:Pt) (ln:Line2D) : float =
        ln.ClosestParameter p

    /// Assumes Line2D to be an infinite ray!
    /// Returns closest point on ray.
    /// Fails on curves shorter than 1e-6 units. (ln.ClosestPoint does not.)
    member ln.RayClosestPoint (pt:Pt) : Pt =
        let fromX = ln.FromX
        let fromY = ln.FromY
        let x = fromX - ln.ToX
        let y = fromY - ln.ToY
        let lenSq = x*x + y*y
        if isTooSmallSq(lenSq) then // the parameter is infinite so we have to fail
            failTooSmall2 "Line2D.RayClosestPoint" ln pt
        let u = fromX - pt.X
        let v = fromY - pt.Y
        let dot = x*u + y*v
        let t = dot/lenSq
        let x' = fromX - x*t
        let y' = fromY - y*t
        Pt(x', y')

    /// Assumes Line2D to be an infinite ray!
    /// Returns closest point on ray.
    static member inline rayClosestPoint (p:Pt) (ln:Line2D) : Pt =
        ln.RayClosestPoint p

    /// Returns closest point on (finite) line.
    /// Does not fail on very short curves.
    member ln.ClosestPoint (p:Pt) : Pt =
        ln.EvaluateAt(ln.ClosestParameter(p))

    /// Returns closest point on (finite) line.
    static member inline closestPoint (p:Pt) (ln:Line2D) : Pt =
        ln.ClosestPoint p

    /// <summary> Finds the closest points between two finite 2D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> For parallel and overlapping lines the points returned are in the center of their overlap.
    /// If the lines intersect the returned points are exactly the same.
    /// This method uses a default angle tolerance of 0.25 degrees for parallel detection
    /// and a length tolerance of 1e-6 for detecting lines that are too short.
    /// For more control over these tolerances, use XLine2D.classifyClosestPoints directly.</remarks>
    /// <returns> A tuple of two Pt, the first is the closest point on lnA, the second on lnB.</returns>
    member lnA.ClosestPoints (lnB:Line2D) : Pt * Pt =
        match XLine2D.classifyClosestPoints lnA lnB with
        | XLine2D.ClPts.Apart (a,b) -> a,b
        | XLine2D.ClPts.Parallel (a,b) -> a,b
        | XLine2D.ClPts.Intersect a -> a,a
        | XLine2D.ClPts.TooShortBoth -> lnA.From , lnB.From // TODO or use midpoint, or use end point ? they are all kind off the same
        | XLine2D.ClPts.TooShortA    -> lnA.From , XLineXY.clPtLn' (lnB, lnA.FromX, lnA.FromY)
        | XLine2D.ClPts.TooShortB    -> XLineXY.clPtLn' (lnA, lnB.FromX, lnB.FromY), lnB.From

    ///<summary> Finds the closest points between two finite 2D Lines, also works on parallel and overlapping lines.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<remarks> For parallel and overlapping lines the points returned are in the center of their overlap.
    /// If the lines intersect the returned points are exactly the same</remarks>
    static member closestPoints (lnA:Line2D) (lnB:Line2D) : Pt * Pt =
        lnA.ClosestPoints lnB

    /// <summary> Finds the parameters of closest points between two finite 2D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> For parallel and overlapping lines the parameters returned are in the center of their overlap.
    /// If the lines intersect the returned parameters are exactly the same.
    /// This method uses a default angle tolerance of 0.25 degrees for parallel detection
    /// and a length tolerance of 1e-6 for detecting lines that are too short.
    /// For more control over these tolerances, use XLine2D.classifyClosestParameters directly.</remarks>
    /// <returns> A tuple of two floats, the first is the parameter on lnA, the second on lnB.</returns>
    member lnA.ClosestParameters (lnB:Line2D) : float * float =
        match XLine2D.classifyClosestParameters lnA lnB with
        | XLine2D.ClParams.Apart (a,b) -> a,b
        | XLine2D.ClParams.Parallel (a,b) -> a,b
        | XLine2D.ClParams.Intersect (a,b) -> a,b
        | XLine2D.ClParams.TooShortBoth -> 0.0  , 0.0 // TODO or use midpoint, or use end point ? they are all kind off the same
        | XLine2D.ClParams.TooShortA    -> 0.0  , XLineXY.clParamLnPt (lnB, lnA.FromX, lnA.FromY)
        | XLine2D.ClParams.TooShortB    -> XLineXY.clParamLnPt (lnA, lnB.FromX, lnB.FromY), 0.0

    ///<summary> Finds the parameters of closest points between two finite 2D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> For parallel and overlapping lines the parameters returned are in the center of their overlap.
    ///  If the lines intersect the returned parameters are exactly the same</remarks>
    /// <returns> A tuple of two floats, the first is the parameter on lnA, the second on lnB</returns>
    static member closestParameters (lnA:Line2D) (lnB:Line2D) : float * float =
        lnA.ClosestParameters lnB

    /// Assumes Line2D to be an infinite ray!
    /// Returns square distance from point to ray.
    /// Fails on curves shorter than 1e-6 units. (ln.DistanceToPtSq does not.)
    member ln.DistanceRayXYSq(x:float, y:float) : float =
        let vAx = ln.VectorX
        let vAy = ln.VectorY
        // normal vector:
        let nx = -vAy
        let ny = vAx
        let lenSq = nx*nx + ny*ny
        if isTooSmallSq(lenSq) then
            failTooSmall2 "Line2D.DistanceRayPointSq" ln (Pt(x, y))
        let u = ln.FromX - x
        let v = ln.FromY - y
        let dot = nx*u + ny*v
        (dot*dot) / lenSq

    /// Assumes Line2D to be an infinite ray!
    /// Returns square distance from point to ray.
    /// Fails on curves shorter than 1e-6 units. (ln.DistanceToPtSq does not.)
    member ln.DistanceRayPointSq(p:Pt) : float =
        ln.DistanceRayXYSq(p.X, p.Y)

    /// Assumes Line2D to be an infinite ray!
    /// Returns the square distance from point to ray.
    static member inline distanceRayXYSq(x:float) (y:float) (ln:Line2D) : float =
        ln.DistanceRayXYSq (x, y)

    /// Assumes Line2D to be an infinite ray!
    /// Returns the square distance from point to ray.
    static member inline distanceRayPointSq(p:Pt) (ln:Line2D) : float =
        ln.DistanceRayPointSq p

    /// Assumes Line2D to be an infinite ray!
    /// Returns distance from point to ray.
    /// Fails on curves shorter than 1e-6 units. (ln.DistanceToPt does not.)
    member inline ln.DistanceRayXY(x:float, y:float) : float =
        ln.DistanceRayXYSq(x, y) |> sqrt

    /// Assumes Line2D to be an infinite ray!
    /// Returns distance from point to ray.
    /// Fails on curves shorter than 1e-6 units. (ln.DistanceToPt does not.)
    member inline ln.DistanceRayPoint(p:Pt) : float =
        ln.DistanceRayXY(p.X, p.Y)

    /// Assumes Line2D to be an infinite ray!
    /// Returns distance from point to ray.
    static member inline distanceRayXY(x:float) (y:float) (ln:Line2D) : float =
        ln.DistanceRayXY (x, y)

    /// Assumes Line2D to be an infinite ray!
    /// Returns distance from point to ray.
    static member inline distanceRayPoint(p:Pt) (ln:Line2D) : float =
        ln.DistanceRayPoint p

    /// Returns square distance from point to finite line.
    member ln.DistanceToXYSq(x:float, y:float) : float =
        XLineXY.sqDistLnPt'(ln, x, y)

    /// Returns square distance from point to finite line.
    member ln.DistanceToPtSq(p:Pt) : float =
        ln.DistanceToXYSq(p.X, p.Y)

    /// Returns the square distance from point to finite line.
    static member inline distanceToXYSq(x:float) (y:float) (ln:Line2D) : float =
        ln.DistanceToXYSq (x, y)

    /// Returns the square distance from point to finite line.
    static member inline distanceToPtSq(p:Pt) (ln:Line2D) : float =
        ln.DistanceToPtSq p

    /// Returns distance from point to (finite) line.
    member inline ln.DistanceToXY(x:float, y:float) : float =
        XLineXY.sqDistLnPt'(ln, x, y) |> sqrt

    /// Returns distance from point to (finite) line.
    member inline ln.DistanceToPt(p:Pt) : float =
        ln.DistanceToXY(p.X, p.Y)

    /// Returns distance from point to (finite) line.
    static member inline distanceToXY(x:float) (y:float) (ln:Line2D) : float =
        ln.DistanceToXY (x, y)

    /// Returns distance from point to (finite) line.
    static member inline distanceToPt(p:Pt) (ln:Line2D) : float =
        ln.DistanceToPt p

    /// Checks if the dot product between the two 2D lines is positive .
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation (otherLn:Line2D) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let bx = otherLn.VectorX
        let by = otherLn.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.MatchesOrientation this" ln otherLn
        if isTooTinySq(bx*bx + by*by) then failTooSmall2 "Line2D.MatchesOrientation other" otherLn ln
        ax*bx + ay*by > 1e-12

    /// Checks if the dot product between two 2D lines is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientation (l:Line2D) (ln:Line2D) : bool =
        l.MatchesOrientation ln

    /// Checks if the dot product between a 2D line and a vector is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation (v:Vc) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        if isTooTinySq(v.LengthSq)   then failTooSmall2 "Line2D.MatchesOrientation this" v ln
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.MatchesOrientation other" ln v
        ax*v.X + ay*v.Y > 1e-12

    /// Checks if the dot product between a 2D line and a unit-vector is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation (v:UnitVc) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.MatchesOrientation other" ln v
        ax*v.X + ay*v.Y > 1e-12

    /// Checks if the angle between the two 2D lines is less than 45 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation45 (l:Line2D) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let bx = l.VectorX
        let by = l.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.MatchesOrientation45 this" ln l
        if isTooTinySq(bx*bx + by*by) then failTooSmall2 "Line2D.MatchesOrientation45 other" l ln
        let tan = XLineXY.tangent (ax, ay, bx, by)
        tan < Tangent.``45.0``

    /// Checks if the angle between the two 2D lines is less than 45 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientation45 (l:Line2D) (ln:Line2D) : bool =
        l.MatchesOrientation45 ln

    /// Checks if two 2D lines are parallel.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:Line2D, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let bx = other.VectorX
        let by = other.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.IsParallelTo this" ln other
        if isTooTinySq(bx*bx + by*by) then failTooSmall2 "Line2D.IsParallelTo other" other ln
        let tan = XLineXY.tangent (ax, ay, bx, by)
        abs tan < minTangent

    /// Checks if two 2D lines are parallel. Ignoring orientation.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelTo (minTangent:float<Tangent.tangent>) (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsParallelTo(lnB, minTangent)

    /// Checks if a 2D line is parallel to a 2D vector.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:Vc, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.IsParallelTo" ln other
        if isTooTinySq(other.LengthSq) then failTooSmall2 "Line2D.IsParallelTo" other ln
        let tan = XLineXY.tangent (ax, ay, other.X, other.Y)
        abs tan < minTangent

    /// Checks if a 2D line is parallel to a 2D unit-vector.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:UnitVc, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.IsParallelTo" ln other
        let tan = XLineXY.tangent (ax, ay, other.X, other.Y)
        abs tan < minTangent

    /// Checks if two 2D lines are parallel.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:Line2D, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let bx = other.VectorX
        let by = other.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.IsParallelAndOrientedTo this" ln other
        if isTooTinySq(bx*bx + by*by) then failTooSmall2 "Line2D.IsParallelAndOrientedTo other" other ln
        let tan = XLineXY.tangent (ax, ay, bx, by)
        tan < minTangent

    /// Checks if two 2D lines are parallel and orientated the same way.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelAndOrientedTo (minTangent:float<Tangent.tangent>) (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsParallelAndOrientedTo(lnB, minTangent)

    /// Checks if a 2D line is parallel to a 2D vector.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines  or vectors shorter than 1e-12.
    member inline ln.IsParallelAndOrientedTo (other:Vc, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.IsParallelAndOrientedTo" ln other
        if isTooTinySq(other.LengthSq) then failTooSmall2 "Line2D.IsParallelAndOrientedTo" other ln
        let tan = XLineXY.tangent (ax, ay, other.X, other.Y)
        tan < minTangent

    /// Checks if a 2D line is parallel to a 2D unit-vector.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:UnitVc, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.IsParallelAndOrientedTo" ln other
        let tan = XLineXY.tangent (ax, ay, other.X, other.Y)
        tan < minTangent

    /// Checks if two 2D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:Line2D, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let bx = other.VectorX
        let by = other.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.IsPerpendicularTo this" ln other
        if isTooTinySq(bx*bx + by*by) then failTooSmall2 "Line2D.IsPerpendicularTo other" other ln
        let tan = XLineXY.tangent (ax, ay, bx, by)
        abs tan > maxTangent

    /// Checks if two 2D lines are perpendicular to each other.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75`` is about 229.2.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isPerpendicularTo (maxTangent:float<Tangent.tangent>) (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsPerpendicularTo(lnB, maxTangent)

    /// Checks if a 2D line is perpendicular to a 2D vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:Vc, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.IsPerpendicularTo this" ln other
        if isTooTinySq(other.LengthSq) then failTooSmall2 "Line2D.IsPerpendicularTo other" other ln
        let tan = XLineXY.tangent (ax, ay, other.X, other.Y)
        abs tan > maxTangent

    /// Checks if a 2D line is perpendicular to a 2D unit-vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:UnitVc, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        if isTooTinySq(ax*ax + ay*ay) then failTooSmall2 "Line2D.IsPerpendicularTo this" ln other
        let tan = XLineXY.tangent (ax, ay, other.X, other.Y)
        abs tan > maxTangent

    /// Checks if two 2D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as ln.IsPerpendicularTo.
    member inline ln.IsNormalTo (other:Line2D, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if two 2D lines are perpendicular to each other.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75`` is about 229.2.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as Line2D.isPerpendicularTo.
    static member inline isNormalTo (maxTangent:float<Tangent.tangent>) (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsNormalTo(lnB, maxTangent)

    /// Checks if a 2D line is perpendicular to a 2D vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as ln.IsPerpendicularTo.
    member inline ln.IsNormalTo (other:Vc, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if a 2D line is perpendicular to a 2D unit-vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as ln.IsPerpendicularTo.
    member inline ln.IsNormalTo (other:UnitVc, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if two 2D lines are coincident on the same ray within the distance tolerance. 1e-6 by default.
    /// This means that lines are parallel within the angle tolerance
    /// and the distance of second start to the first line is less than the distance tolerance.
    /// Also returns FALSE on zero Length lines.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    member inline ln.IsCoincidentTo (other:Line2D,
                                    [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                    [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent>) : bool =
        let vtX = ln.VectorX
        let vtY = ln.VectorY
        let voX = other.VectorX
        let voY = other.VectorY
        if isTooTinySq(vtX*vtX + vtY*vtY) then failTooSmall2 "Line2D.IsCoincidentTo this" ln other
        if isTooTinySq(voX*voX + voY*voY) then failTooSmall2 "Line2D.IsCoincidentTo other" other ln
        let dot = vtX*voX + vtY*voY
        let det = vtX * voY - vtY * voX
        // division-free parallel test: |det| / |dot| < minTangent  <=>  |det| < minTangent * |dot| (handles NaN/zero too)
        if !^ (abs det) < minTangent * abs dot then
            // they are parallel, now check distance:
            let x = other.FromX - ln.FromX
            let y = other.FromY - ln.FromY
            let dotN =  x*vtY - y*vtX // looks like a cross product, but is the dot with the normal
            let lenSq = vtX*vtX + vtY*vtY
            let distSq = (dotN * dotN) / lenSq
            distSq < distanceTolerance * distanceTolerance
        else
            // they are not parallel within tolerance
            false

    /// Checks if two 2D lines are coincident on the same ray within the distance tolerance.
    /// This means that lines are parallel within the angle tolerance.
    /// Also returns FALSE on zero length lines.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    static member inline isCoincidentTo distanceTolerance (minTangent:float<Tangent.tangent>) (a:Line2D) (b:Line2D) : bool =
        a.IsCoincidentTo(b, distanceTolerance, minTangent)

    /// A fast version of IsParallelTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are parallel within a given tolerance for their cross product.
    /// Lines are parallel if cross product is less than parallelogramAreaTolerance.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// ATTENTION this method returns TRUE on zero length or very small lines.
    /// It returns FALSE on almost coincident lines that are very long.
    /// Pick a bigger parallelogramAreaTolerance for longer lines.
    member inline ln.IsParallelToFast( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float ) : bool =
        // Get direction vectors
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d2x = other.VectorX
        let d2y = other.VectorY
        // Cross product in 2D (scalar value)
        let cross = d1x * d2y - d1y * d2x
        // Lines are parallel if cross product is near zero
        abs cross < parallelogramAreaTolerance

    /// A fast version of IsParallelTo that uses cross-product to determine parallelism.
    static member inline isParallelToFast parallelogramAreaTolerance (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsParallelToFast(lnB, parallelogramAreaTolerance)

    /// A fast version of IsCoincidentTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are coincident within a given tolerance for their cross product.
    /// Lines are coincident if they are parallel (cross product less than parallelogramAreaTolerance)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// ATTENTION this method returns TRUE on zero length or very small lines.
    /// It returns FALSE on almost coincident lines that are very long.
    /// Pick a bigger parallelogramAreaTolerance for longer lines.
    member inline ln.IsCoincidentToFast ( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float) : bool =
        // First check if parallel
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d2x = other.VectorX
        let d2y = other.VectorY
        let cross = d1x * d2y - d1y * d2x
        if abs cross < parallelogramAreaTolerance then
            // Then check vector between line starts is also parallel to the lines
            let px = ln.FromX - other.FromX
            let py = ln.FromY - other.FromY
            let cross = px * d2y - py * d2x
            abs cross < parallelogramAreaTolerance
        else
            false

    /// A fast version of IsCoincidentTo that uses cross-product to determine parallelism.
    static member inline isCoincidentToFast parallelogramAreaTolerance (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsCoincidentToFast(lnB, parallelogramAreaTolerance)

    /// A fast version of IsParallelToAndOrientedTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are parallel within a given tolerance for their cross product
    /// and oriented in the same direction by having a positive dot-product.
    /// Lines are parallel if cross product is less than given parallelogramAreaTolerance.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// Lines are oriented if dot product is positive.
    /// minDotProduct must be POSITIVE! just above zero. 1e-6 by default.
    /// The minDotProduct argument helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsParallelAndOrientedToFast( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float , [<OPT;DEF(1e-6)>]minDotProduct :float) : bool =
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d2x = other.VectorX
        let d2y = other.VectorY
        let cross = d1x * d2y - d1y * d2x
        // Lines are parallel if cross product is near zero
        if abs cross < parallelogramAreaTolerance then
            let dot = d1x * d2x + d1y * d2y
            dot > minDotProduct // instead of checking bigger than zero we use minDotProduct to avoid true for  tiny lines
        else
            false

    /// A fast version of IsParallelAndOrientedTo that uses cross-product and dot-product checks.
    static member inline isParallelAndOrientedToFast parallelogramAreaTolerance minDotProduct (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsParallelAndOrientedToFast(lnB, parallelogramAreaTolerance, minDotProduct)

    /// A fast version of IsParallelToAndOpposingTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are parallel within a given tolerance for their cross product
    /// and opposing by having a negative dot-product.
    /// Lines are parallel if cross product is less than given parallelogramAreaTolerance.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// Lines are opposing if dot product is negative.
    /// maxDotProduct must be NEGATIVE! just below zero, -1e-6 by default !
    /// The maxDotProduct helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsParallelAndOpposingToFast( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float , [<OPT;DEF(-1e-6)>]maxDotProduct :float) : bool =
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d2x = other.VectorX
        let d2y = other.VectorY
        let cross = d1x * d2y - d1y * d2x
        // Lines are parallel if cross product is near zero
        if abs cross < parallelogramAreaTolerance then
            let dot = d1x * d2x + d1y * d2y // this is expected to be negative
            dot < maxDotProduct // instead of checking bigger than zero we use maxDotProduct to avoid true for  tiny lines
        else
            false

    /// A fast version of IsParallelAndOpposingTo that uses cross-product and dot-product checks.
    static member inline isParallelAndOpposingToFast parallelogramAreaTolerance maxDotProduct (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsParallelAndOpposingToFast(lnB, parallelogramAreaTolerance, maxDotProduct)

    /// A fast version of IsCoincidentAndOrientedTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are coincident on the same ray within given tolerances.
    /// Lines are coincident if they are parallel (cross product less than parallelogramAreaTolerance)
    /// and oriented in the same direction if the dot-product is positive (bigger than minDotProduct)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// Lines are oriented if dot product is positive.
    /// minDotProduct must be POSITIVE! just above zero. 1e-6 by default.
    /// The minDotProduct argument helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsCoincidentAndOrientedToFast ( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float, [<OPT;DEF(1e-6)>]minDotProduct :float) : bool =
        // First check if parallel and oriented
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d2x = other.VectorX
        let d2y = other.VectorY
        let cross = d1x * d2y - d1y * d2x
        if abs cross < parallelogramAreaTolerance then
            let dot = d1x * d2x + d1y * d2y
            if dot > minDotProduct then
                // Then check vector between line starts is also parallel to the lines
                let px = ln.FromX - other.FromX
                let py = ln.FromY - other.FromY
                let cross = px * d2y - py * d2x
                abs cross < parallelogramAreaTolerance
            else
                false
        else
            false

    /// A fast version of IsCoincidentAndOrientedTo that uses cross-product and dot-product checks.
    static member inline isCoincidentAndOrientedToFast parallelogramAreaTolerance minDotProduct (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsCoincidentAndOrientedToFast(lnB, parallelogramAreaTolerance, minDotProduct)

    /// A fast version of IsCoincidentAndOpposingTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are coincident on opposing rays within given tolerances.
    /// Lines are coincident if they are parallel (cross product less than parallelogramAreaTolerance)
    /// and opposing if the dot-product is negative (smaller than maxDotProduct)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// Lines are opposing if dot product is negative.
    /// maxDotProduct must be NEGATIVE! just below zero, -1e-6 by default !
    /// The maxDotProduct helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsCoincidentAndOpposingToFast( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float, [<OPT;DEF(-1e-6)>]maxDotProduct :float) : bool =
        // First check if parallel and opposing
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d2x = other.VectorX
        let d2y = other.VectorY
        let cross = d1x * d2y - d1y * d2x
        if abs cross < parallelogramAreaTolerance then
            let dot = d1x * d2x + d1y * d2y
            if dot < maxDotProduct then
                // Then check vector between line starts is also parallel to the lines
                let px = ln.FromX - other.FromX
                let py = ln.FromY - other.FromY
                let cross = px * d2y - py * d2x
                abs cross < parallelogramAreaTolerance
            else
                false
        else
            false

    /// A fast version of IsCoincidentAndOpposingTo that uses cross-product and dot-product checks.
    static member inline isCoincidentAndOpposingToFast parallelogramAreaTolerance maxDotProduct (lnA:Line2D) (lnB:Line2D) : bool =
        lnA.IsCoincidentAndOpposingToFast(lnB, parallelogramAreaTolerance, maxDotProduct)

    /// Looking in the direction of the line.
    /// Check if a given point is on the right side of the ray.
    /// Also returns FALSE if the point is on the line.
    member inline ln.IsXYOnRight(x:float, y:float) : bool =
        ln.VectorY * (x - ln.FromX) - ln.VectorX * (y - ln.FromY) > 0.0

    /// Looking in the direction of the line.
    /// Check if a given point is on the right side of the ray.
    /// Also returns FALSE if the point is on the line.
    member inline ln.IsPointOnRight(pt:Pt) : bool =
        ln.IsXYOnRight(pt.X, pt.Y)

    /// Checks if a point is on the right side of the line ray.
    /// Also returns FALSE if the point is on the line.
    static member inline isXYOnRight (x:float) (y:float) (ln:Line2D) : bool =
        ln.IsXYOnRight (x, y)

    /// Checks if a point is on the right side of the line ray.
    /// Also returns FALSE if the point is on the line.
    static member inline isPointOnRight (pt:Pt) (ln:Line2D) : bool =
        ln.IsPointOnRight pt

    /// Looking in the direction of the line.
    /// Check if a given point is on the left side of the infinite ray.
    /// Also returns FALSE if the point is on the line.
    member inline ln.IsXYOnLeft(x:float, y:float) : bool =
        ln.VectorX * (y - ln.FromY) - ln.VectorY * (x - ln.FromX) > 0.0

    /// Looking in the direction of the line.
    /// Check if a given point is on the left side of the infinite ray.
    /// Also returns FALSE if the point is on the line.
    member inline ln.IsPointOnLeft(pt:Pt) : bool =
        ln.IsXYOnLeft(pt.X, pt.Y)

    /// Checks if a point is on the left side of the line ray.
    /// Also returns FALSE if the point is on the line.
    static member inline isXYOnLeft (x:float) (y:float) (ln:Line2D) : bool =
        ln.IsXYOnLeft (x, y)

    /// Checks if a point is on the left side of the line ray.
    /// Also returns FALSE if the point is on the line.
    static member inline isPointOnLeft (pt:Pt) (ln:Line2D) : bool =
        ln.IsPointOnLeft pt

    /// Scales the 2D line by a given factor.
    /// Scale center is World Origin 0,0
    member inline l.Scale (factor:float) : Line2D =
        Line2D(
            l.FromX * factor,
            l.FromY * factor,
            l.ToX   * factor,
            l.ToY   * factor)

    /// Scale the 2D line by a given factor.
    /// Scale center is World Origin 0,0
    static member inline scale (factor:float) (ln:Line2D) : Line2D =
        Line2D(ln.FromX * factor,
               ln.FromY * factor,
               ln.ToX * factor,
               ln.ToY * factor)

    /// <summary>Scales the 2D line by a given factor around a given center point.</summary>
    /// <param name="cen">The center point of the scaling.</param>
    /// <param name="factor">The scale factor.</param>
    /// <returns>The scaled 2D line.</returns>
    member inline l.ScaleOn (cen:Pt) (factor:float) : Line2D =
        let cx = cen.X
        let cy = cen.Y
        Line2D(
            cx + (l.FromX - cx) * factor,
            cy + (l.FromY - cy) * factor,
            cx + (l.ToX   - cx) * factor,
            cy + (l.ToY   - cy) * factor)

    /// <summary>Scales a 2D line by a given factor around a given center point.</summary>
    /// <param name="cen">The center point of the scaling.</param>
    /// <param name="factor">The scale factor.</param>
    /// <param name="ln">The 2D line to scale.</param>
    /// <returns>The scaled 2D line.</returns>
    static member inline scaleOn (cen:Pt) (factor:float) (ln:Line2D) : Line2D =
        ln.ScaleOn cen factor

    /// Rotate a Line2D by a Rotation2D.
    /// The resulting line has the same length as the input.
    member inline ln.Rotate (r:Rotation2D) : Line2D =
        let fx = ln.FromX
        let fy = ln.FromY
        let tx = ln.ToX
        let ty = ln.ToY
        let c = r.Cos
        let s = r.Sin
        Line2D( c*fx - s*fy,
                s*fx + c*fy,
                c*tx - s*ty,
                s*tx + c*ty)

    /// Rotation a Line2D.
    static member inline rotate (r:Rotation2D) (ln:Line2D) : Line2D =
        let fx = ln.FromX
        let fy = ln.FromY
        let tx = ln.ToX
        let ty = ln.ToY
        let c = r.Cos
        let s = r.Sin
        Line2D( c*fx - s*fy,
                s*fx + c*fy,
                c*tx - s*ty,
                s*tx + c*ty)

    /// <summary>Rotate the Line2D counter-clockwise around the World Origin by an angle in degrees.</summary>
    /// <param name="angDegree">The rotation angle in degrees.</param>
    /// <returns>The rotated 2D line.</returns>
    /// <remarks>For better performance precompute the Rotation2D struct and rotate with this.Rotate(rotation2D).</remarks>
    member inline ln.RotateDegrees (angDegree:float) : Line2D =
        Line2D.rotate (Rotation2D.createFromDegrees angDegree) ln

    /// <summary>Rotate a Line2D counter-clockwise around the World Origin by an angle in degrees.</summary>
    /// <param name="angDegree">The rotation angle in degrees.</param>
    /// <param name="ln">The 2D line to rotate.</param>
    /// <returns>The rotated 2D line.</returns>
    /// <remarks>For better performance precompute the Rotation2D struct and rotate with Line2D.rotate.</remarks>
    static member inline rotateDegrees (angDegree:float) (ln:Line2D) : Line2D =
        Line2D.rotate (Rotation2D.createFromDegrees angDegree) ln

    /// <summary>Rotate the Line2D counter-clockwise around the World Origin by an angle in radians.</summary>
    /// <param name="angRadians">The rotation angle in radians.</param>
    /// <returns>The rotated 2D line.</returns>
    /// <remarks>For better performance precompute the Rotation2D struct and rotate with this.Rotate(rotation2D).</remarks>
    member inline ln.RotateRadians (angRadians:float) : Line2D =
        Line2D.rotate (Rotation2D.createFromRadians angRadians) ln

    /// <summary>Rotate a Line2D counter-clockwise around the World Origin by an angle in radians.</summary>
    /// <param name="angRadians">The rotation angle in radians.</param>
    /// <param name="ln">The 2D line to rotate.</param>
    /// <returns>The rotated 2D line.</returns>
    /// <remarks>For better performance precompute the Rotation2D struct and rotate with Line2D.rotate.</remarks>
    static member inline rotateRadians (angRadians:float) (ln:Line2D) : Line2D =
        Line2D.rotate (Rotation2D.createFromRadians angRadians) ln

    /// <summary>Rotate the Line2D counter-clockwise around a given center point by a Rotation2D.
    /// The resulting line has the same length as the input.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="r">The rotation to apply.</param>
    /// <returns>The rotated 2D line.</returns>
    member inline ln.RotateAround (cen:Pt) (r:Rotation2D) : Line2D =
        let cx = cen.X
        let cy = cen.Y
        let fx = ln.FromX - cx
        let fy = ln.FromY - cy
        let tx = ln.ToX - cx
        let ty = ln.ToY - cy
        let c = r.Cos
        let s = r.Sin
        Line2D( c*fx - s*fy + cx,
                s*fx + c*fy + cy,
                c*tx - s*ty + cx,
                s*tx + c*ty + cy)

    /// <summary>Rotate the Line2D counter-clockwise around a given center point by an angle in degrees.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="angDegree">The rotation angle in degrees.</param>
    /// <returns>The rotated 2D line.</returns>
    member inline ln.RotateDegreesAround (cen:Pt) (angDegree:float) : Line2D =
        ln.RotateAround cen (Rotation2D.createFromDegrees angDegree)

    /// <summary>Rotate a Line2D counter-clockwise around a given center point by an angle in degrees.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="angDegree">The rotation angle in degrees.</param>
    /// <param name="ln">The 2D line to rotate.</param>
    /// <returns>The rotated 2D line.</returns>
    static member inline rotateDegreesAround (cen:Pt) (angDegree:float) (ln:Line2D) : Line2D =
        ln.RotateAround cen (Rotation2D.createFromDegrees angDegree)

    /// <summary>Rotate the Line2D counter-clockwise around a given center point by an angle in radians.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="angRadians">The rotation angle in radians.</param>
    /// <returns>The rotated 2D line.</returns>
    member inline ln.RotateRadiansAround (cen:Pt) (angRadians:float) : Line2D =
        ln.RotateAround cen (Rotation2D.createFromRadians angRadians)

    /// <summary>Rotate a Line2D counter-clockwise around a given center point by an angle in radians.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="angRadians">The rotation angle in radians.</param>
    /// <param name="ln">The 2D line to rotate.</param>
    /// <returns>The rotated 2D line.</returns>
    static member inline rotateRadiansAround (cen:Pt) (angRadians:float) (ln:Line2D) : Line2D =
        ln.RotateAround cen (Rotation2D.createFromRadians angRadians)

    /// <summary>Rotate a Line2D counter-clockwise around a given center point by a Rotation2D.
    /// The resulting line has the same length as the input.</summary>
    /// <param name="cen">The center point of the rotation.</param>
    /// <param name="r">The rotation to apply.</param>
    /// <param name="ln">The 2D line to rotate.</param>
    /// <returns>The rotated 2D line.</returns>
    static member inline rotateAround (cen:Pt) (r:Rotation2D) (ln:Line2D) : Line2D =
        let cx = cen.X
        let cy = cen.Y
        let fx = ln.FromX - cx
        let fy = ln.FromY - cy
        let tx = ln.ToX - cx
        let ty = ln.ToY - cy
        let c = r.Cos
        let s = r.Sin
        Line2D( c*fx - s*fy + cx,
                s*fx + c*fy + cy,
                c*tx - s*ty + cx,
                s*tx + c*ty + cy)


    // #endregion
    // #region Static members

    /// Checks if two 2D lines are equal within tolerance.
    /// Identical Lines in opposite directions are not considered equal.
    /// The tolerance applies to each coordinate of the start and end points separately, not to the Euclidean distance between them.
    /// Use Line2D.equalsExact to check for an exact match.
    static member inline equalsWithin (tol:float) (a:Line2D) (b:Line2D)  : bool =
        abs (a.FromX - b.FromX) <= tol &&
        abs (a.FromY - b.FromY) <= tol &&
        abs (a.ToX   - b.ToX  ) <= tol &&
        abs (a.ToY   - b.ToY  ) <= tol

    /// Checks if two 2D lines are exactly equal.
    /// Compares each field with the = operator, so a NaN value is never equal.
    static member inline equalsExact (a:Line2D) (b:Line2D) : bool =
        a.FromX = b.FromX &&
        a.FromY = b.FromY &&
        a.ToX = b.ToX &&
        a.ToY = b.ToY

    /// Check if two 2D lines are not equal within a given tolerance.
    /// The tolerance applies to each coordinate of the start and end points separately, not to the Euclidean distance between them.
    static member notEqualsWithin (tol:float) (a:Line2D) (b:Line2D)  : bool =
        abs (a.FromX - b.FromX) > tol ||
        abs (a.FromY - b.FromY) > tol ||
        abs (a.ToX   - b.ToX  ) > tol ||
        abs (a.ToY   - b.ToY  ) > tol

    /// Returns the Cross Product or Determinant between the vectors of the Two lines.
    /// In 2D it is just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
    /// If the result is zero the two lines are parallel (or one line is zero-length).
    /// This is tupled, like Vc.cross, because the order of the arguments matters.
    static member inline cross (a:Line2D, b:Line2D) : float =
        let ax = a.VectorX
        let ay = a.VectorY
        let bx = b.VectorX
        let by = b.VectorY
        ax*by - ay*bx

    /// Creates a 3D line from a 2D line with Z coordinates set to the specified level.
    static member inline toLine3D (zLevel:float) (ln:Line2D) : Line3D =
        Line3D(ln.FromX, ln.FromY, zLevel, ln.ToX, ln.ToY, zLevel)

    /// Creates a 2D line from a 3D line by projecting onto the XY plane.
    /// The Z coordinate is discarded.
    static member inline createFromLine3D (ln:Line3D) : Line2D =
        Line2D(ln.FromX, ln.FromY, ln.ToX, ln.ToY)

    /// Creates a 2D line starting at World Origin and going to along the given vector.
    static member inline createFromVc (v:Vc) : Line2D =
        Line2D(0., 0., v.X, v.Y)

    /// Creates a 2D line from a start and an end point.
    /// Same as the Line2D(start, finish) constructor. Zero length lines are allowed.
    static member inline create (start:Pt) (finish:Pt) : Line2D =
        Line2D(start.X, start.Y, finish.X, finish.Y)

    /// Creates a 2D line starting at given point and going to along the given vector.
    static member inline createFromPtAndVc (p:Pt) (v:Vc) : Line2D =
        Line2D(p.X, p.Y, p.X+v.X, p.Y+v.Y)

    /// Returns the Start point of the line. Same as Line2D.from.
    static member inline start (l:Line2D) : Pt =
        l.From

    /// Returns the Start point of the line. Same as Line2D.start.
    static member inline from (l:Line2D) : Pt =
        l.From

    /// Returns the Start point's X coordinate of the line.
    static member inline fromX (l:Line2D) : float =
        l.FromX

    /// Returns the Start point's Y coordinate of the line.
    static member inline fromY (l:Line2D) : float =
        l.FromY

    /// Returns the End point of the line. Same as Line2D.to'
    static member inline end' (l:Line2D) : Pt =
        l.To

    /// Returns the End point of the line. Same as Line2D.end'.
    static member inline to' (l:Line2D) : Pt =
        l.To

    /// Returns the End point's X coordinate of the line.
    static member inline toX (l:Line2D) : float =
        l.ToX

    /// Returns the End point's Y coordinate of the line.
    static member inline toY (l:Line2D) : float =
        l.ToY

    /// The X component of the line Direction/Vector.
    static member inline vectorX (l:Line2D) : float =
        l.VectorX

    /// The Y component of the line Direction/Vector.
    static member inline vectorY (l:Line2D) : float =
        l.VectorY

    /// Format 2D line into string from X and Y for start and end points.
    /// Using nice floating point number formatting.
    /// But without full type name as in ln.ToString()
    static member inline asString (l:Line2D) : string =
        l.AsString

    /// Format 2D line into an F# code string that can be used to recreate the line.
    static member inline asFSharpCode (l:Line2D) : string =
        l.AsFSharpCode

    /// Returns a new 2D line with a new start point, the end point stays the same.
    /// Same as Line2D.withFrom.
    static member inline withStart (pt:Pt) (ln:Line2D) : Line2D =
        Line2D( pt.X, pt.Y, ln.ToX, ln.ToY)

    /// Returns a new 2D line with a new start point, the end point stays the same.
    /// Same as Line2D.withStart.
    static member inline withFrom (pt:Pt) (ln:Line2D) : Line2D =
        Line2D( pt.X, pt.Y, ln.ToX, ln.ToY)

    /// Returns a new 2D line with a new end point, the start point stays the same.
    /// Same as Line2D.withTo.
    static member inline withEnd (pt:Pt) (ln:Line2D) : Line2D =
        Line2D( ln.FromX, ln.FromY, pt.X, pt.Y)

    /// Returns a new 2D line with a new end point, the start point stays the same.
    /// Same as Line2D.withEnd.
    static member inline withTo (pt:Pt) (ln:Line2D) : Line2D =
        Line2D( ln.FromX, ln.FromY, pt.X, pt.Y)

    /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Direction : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line2D.
    member inline ln.Tangent : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Returns a unit-vector of the line Direction.
    member inline ln.UnitTangent : UnitVc =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x * x  + y * y)
        if UtilEuclid.isTooTiny l then
            EuclidErrors.failUnit2 "Line2D.UnitTangent" x y
        UnitVc.createUnchecked (x/l, y/l)

    /// Same as Line2D.vector or Line2D.tangent.
    /// The returned vector has the same length as the Line2D.
    static member inline direction (ln:Line2D) : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as Line2D.tangent or Line2D.direction.
    /// The returned vector has the same length as the Line2D.
    static member inline vector (ln:Line2D) : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as Line2D.vector or Line2D.direction.
    /// The returned vector has the same length as the Line2D.
    static member inline tangent (ln:Line2D) : Vc =
        Vc(ln.VectorX, ln.VectorY)

    /// Returns a unit-vector of the line Direction.
    static member inline unitTangent (ln:Line2D) : UnitVc =
        ln.UnitTangent

    /// Returns the length of the line.
    static member inline length (l:Line2D) : float =
        l.Length

    /// Returns the square length of the line.
    static member inline lengthSq (l:Line2D) : float =
        l.LengthSq

    /// Reverse or flip the Line2D.
    static member inline reverse (ln:Line2D) : Line2D =
        ln.Reversed

    /// Ensure 2D line has a positive dot product with given orientation line.
    static member inline matchOrientation (orientationToMatch:Line2D) (lineToFlip:Line2D) : Line2D =
        if orientationToMatch.VectorX * lineToFlip.VectorX + orientationToMatch.VectorY * lineToFlip.VectorY < 0.0 then lineToFlip.Reversed else lineToFlip

    /// Ensure 2D line has a positive dot product with given orientation 2D vector.
    static member inline matchVcOrientation (orientationToMatch:Vc) (lineToFlip:Line2D) : Line2D =
        if orientationToMatch.X * lineToFlip.VectorX + orientationToMatch.Y * lineToFlip.VectorY < 0.0 then lineToFlip.Reversed else lineToFlip

    /// Ensure 2D line has a positive dot product with given orientation 2D vector.
    static member inline matchUnitVcOrientation (orientationToMatch:UnitVc) (lineToFlip:Line2D) : Line2D =
        if orientationToMatch.X * lineToFlip.VectorX + orientationToMatch.Y * lineToFlip.VectorY < 0.0 then lineToFlip.Reversed else lineToFlip

    /// Checks if the dot product between a 2D line and a vector is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientationVc (v:Vc) (ln:Line2D) : bool =
        ln.MatchesOrientation v

    /// Checks if the dot product between a 2D line and a unit-vector is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientationUnitVc (v:UnitVc) (ln:Line2D) : bool =
        ln.MatchesOrientation v

    /// Checks if a 2D line is parallel to a 2D vector. Ignores orientation.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelToVc (minTangent:float<Tangent.tangent>) (other:Vc) (ln:Line2D) : bool =
        ln.IsParallelTo(other, minTangent)

    /// Checks if a 2D line is parallel to a 2D unit-vector. Ignores orientation.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelToUnitVc (minTangent:float<Tangent.tangent>) (other:UnitVc) (ln:Line2D) : bool =
        ln.IsParallelTo(other, minTangent)

    /// Checks if two 2D lines are Not parallel. Ignoring orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isNotParallelTo (lnA:Line2D) (lnB:Line2D) : bool =
        not <| lnA.IsParallelTo(lnB)

    /// Checks if a 2D line is parallel to a 2D vector and oriented the same way.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelAndOrientedToVc (minTangent:float<Tangent.tangent>) (other:Vc) (ln:Line2D) : bool =
        ln.IsParallelAndOrientedTo(other, minTangent)

    /// Checks if a 2D line is parallel to a 2D unit-vector and oriented the same way.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelAndOrientedToUnitVc (minTangent:float<Tangent.tangent>) (other:UnitVc) (ln:Line2D) : bool =
        ln.IsParallelAndOrientedTo(other, minTangent)

    /// Checks if a 2D line is perpendicular to a 2D vector.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75`` is about 0.0044.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isPerpendicularToVc (maxTangent:float<Tangent.tangent>) (other:Vc) (ln:Line2D) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if a 2D line is perpendicular to a 2D unit-vector.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75`` is about 0.0044.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isPerpendicularToUnitVc (maxTangent:float<Tangent.tangent>) (other:UnitVc) (ln:Line2D) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if a 2D line is perpendicular to a 2D vector.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75`` is about 229.2.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as Line2D.isPerpendicularToVc.
    static member inline isNormalToVc (maxTangent:float<Tangent.tangent>) (other:Vc) (ln:Line2D) : bool =
        ln.IsNormalTo(other, maxTangent)

    /// Checks if a 2D line is perpendicular to a 2D unit-vector.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75`` is about 229.2.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as Line2D.isPerpendicularToUnitVc.
    static member inline isNormalToUnitVc (maxTangent:float<Tangent.tangent>) (other:UnitVc) (ln:Line2D) : bool =
        ln.IsNormalTo(other, maxTangent)

    /// A faster Check if two 2D lines are parallel. Ignoring orientation.
    /// The angle tolerance is 0.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return FALSE on zero-length lines.
    static member inline isParallelTo' (lnA:Line2D) (lnB:Line2D) : bool =
        let vtX = lnA.VectorX
        let vtY = lnA.VectorY
        let voX = lnB.VectorX
        let voY = lnB.VectorY
        let dot = vtX*voX + vtY*voY
        let det = vtX * voY - vtY * voX
        // division-free parallel test: |det| / |dot| < tan(0.25 deg)  <=>  |det| < tan(0.25 deg) * |dot|
        !^ (abs det) < Tangent.``0.25`` * abs dot

    /// A faster Check if two 2D lines are not parallel. Ignoring orientation.
    /// The angle tolerance is 0.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return FALSE on zero-length lines.
    static member inline isNotParallelTo' (lnA:Line2D) (lnB:Line2D) : bool =
        let vtX = lnA.VectorX
        let vtY = lnA.VectorY
        let voX = lnB.VectorX
        let voY = lnB.VectorY
        let dot = vtX*voX + vtY*voY
        let det = vtX * voY - vtY * voX
        // division-free parallel test: |det| / |dot| > tan(0.25 deg)  <=>  |det| > tan(0.25 deg) * |dot|
        !^ (abs det) > Tangent.``0.25`` * abs dot

    /// A faster Check if two 2D lines are perpendicular to each other.
    /// The angle tolerance is 89.75 to 90.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return FALSE on zero-length lines.
    /// Same as Line2D.isNormalTo'
    static member inline isPerpendicularTo' (lnA:Line2D) (lnB:Line2D) : bool =
        let vtX = lnA.VectorX
        let vtY = lnA.VectorY
        let voX = lnB.VectorX
        let voY = lnB.VectorY
        let dot = vtX*voX + vtY*voY
        let det = vtX * voY - vtY * voX
        // division-free perpendicularity test: |det| / |dot| > tan(89.75 deg)  <=>  |det| > tan(89.75 deg) * |dot|
        !^ (abs det) > Tangent.``89.75`` * abs dot

    /// A faster Check if two 2D lines are perpendicular to each other.
    /// The angle tolerance is 89.75 to 90.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return FALSE on zero-length lines.
    /// Same as Line2D.isPerpendicularTo'
    static member inline isNormalTo' (lnA:Line2D) (lnB:Line2D) : bool =
        Line2D.isPerpendicularTo' lnA lnB

    /// Assumes Line2D to be an infinite ray!
    /// Returns the parameter at which a point is closest to the infinite ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Get distance from start of line to point projected onto line, may be negative.
    static member inline lengthToPtOnLine (pt:Pt) (ln:Line2D) : float =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = XY.length x y
        if isTooTiny l then failTooSmall "Line2D.lengthToPtOnLine" ln
        let dx = pt.X - ln.FromX
        let dy = pt.Y - ln.FromY
        let t = XY.dot x y dx dy
        t / l

    /// Finds point at given distance from line start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline pointAtDistance dist (ln:Line2D) : Pt =
        let x = ln.VectorX
        let y = ln.VectorY
        let len = sqrt(x*x + y*y)
        if isTooTiny len then failTooSmall "Line2D.pointAtDistance" ln
        Pt(ln.FromX + x*dist/len,
            ln.FromY + y*dist/len)

    /// Finds point at given distance from line end, going towards start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline pointAtDistanceFromEnd dist (ln:Line2D) : Pt =
        let x = ln.VectorX
        let y = ln.VectorY
        let len = sqrt(x*x + y*y)
        if isTooTiny len then failTooSmall "Line2D.pointAtDistanceFromEnd" ln
        Pt(ln.ToX - x*dist/len,
            ln.ToY - y*dist/len)

    /// Returns new Line2D with given length, going out from start in direction of end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline withLengthFromStart len (ln:Line2D) : Line2D =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.withLengthFromStart" ln
        Line2D( ln.FromX,
                ln.FromY,
                ln.FromX + x*len/l,
                ln.FromY + y*len/l)

    /// Returns new Line2D ending at current LineEnd with given length coming from direction of start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline withLengthToEnd len (ln:Line2D) : Line2D =
        let x = ln.FromX-ln.ToX
        let y = ln.FromY-ln.ToY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.withLengthToEnd" ln
        Line2D( ln.ToX + x*len/l,
                ln.ToY + y*len/l,
                ln.ToX,
                ln.ToY)

    /// Returns new Line2D with given length. Fixed in the midpoint.
    /// Missing length is added to or subtracted from both the end and start of the line.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline withLengthFromMid len (ln:Line2D) : Line2D =
        let x = ln.FromX-ln.ToX
        let y = ln.FromY-ln.ToY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.withLengthFromMid" ln
        let f = (len/l + 1.0) * 0.5
        Line2D( ln.ToX   + x*f,
                ln.ToY   + y*f,
                ln.FromX - x*f,
                ln.FromY - y*f)

    /// Offset line in XY-Plane to left side in line direction.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// If amount is 0.0 no offset is computed and the input line is returned.
    static member offset (distance:float) (ln:Line2D) : Line2D =
        if distance = 0.0 then
            ln
        else
            let x = ln.VectorX
            let y = ln.VectorY
            let lenXY = sqrt (x*x + y*y)
            if isTooTiny (lenXY ) then
                failTooSmall "Line2D.offset" ln
            let ox = -y*distance/lenXY // unitized, horizontal, perpendicular  vector
            let oy =  x*distance/lenXY  // unitized, horizontal, perpendicular  vector
            Line2D( ln.FromX+ox,
                    ln.FromY+oy,
                    ln.ToX+ox,
                    ln.ToY+oy)

    /// Divides a 2D line into given amount of segments.
    /// Returns an array of 2D points of length: segment count + 1.
    /// Includes start and endpoint of line.
    static member divide (segments:int) (ln:Line2D) : Pt[] =
        if segments < 1 then
            fail $"Line2D.divide: segments < 1: {segments}"
        if segments = 1 then
            [|ln.From;  ln.To|]
        else
            let x = ln.VectorX
            let y = ln.VectorY
            let sx = ln.FromX
            let sy = ln.FromY
            let kk = float segments
            let r = Array.zeroCreate (segments+1)
            r.[0] <- ln.From
            for i = 1 to segments-1 do
                let t = float i / kk
                r.[i] <- Pt(sx + x*t, sy + y*t)
            r.[segments] <- ln.To
            r

    /// Divides a 2D line into as many as segments as possible respecting the minimum segment length.
    /// Returned Array includes start and endpoint of line.
    /// The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    static member divideMinLength (minSegmentLength:float) (ln:Line2D) : Pt[] =
        let len = ln.Length
        if len < 1e-6 then
            fail $"Line2D.divideMinLength: line length {len} is too small."
        if len < minSegmentLength then
            fail $"Line2D.divideMinLength: line length {len} is smaller than minSegmentLength {minSegmentLength}"
        let k = int (len / (minSegmentLength*1.000001))
        Line2D.divide k ln

    /// Divides a 2D line into as few as segments as possible respecting the maximum segment length.
    /// Returned Array includes start and endpoint of line.
    /// The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical  errors.
    /// That means in an edge case there are fewer segments returned, not more.
    static member divideMaxLength (maxSegmentLength:float) (ln:Line2D) : Pt[] =
        let len = ln.Length
        if len < 1e-6 then
            fail $"Line2D.divideMaxLength: line length {len} is too small."
        if maxSegmentLength < 1e-6 then
            fail $"Line2D.divideMaxLength: maxSegmentLength must be greater than 0.0, was {maxSegmentLength}"
        let k = int (len / maxSegmentLength*0.999999) + 1
        Line2D.divide k ln

    /// Divides a 2D line into given amount of segments.
    /// Includes a gap between the segments. But not at the start or end.
    /// Returns an array of 2D Lines.
    /// Returns an empty array if the length of the line is less than gap-size x segment-count-minus-1.
    static member split (gap:float) (segments:int) (ln:Line2D) : Line2D[] =
        if segments <= 0  then
            fail $"Line2D.split: invalid segments: {segments}"
        let vx = ln.VectorX
        let vy = ln.VectorY
        let len = sqrt(vx*vx + vy*vy)
        let lenMinusGaps = len - gap * float (segments-1)
        let segLen = lenMinusGaps / float segments
        if isTooTiny (segLen) then
            [||]
        else
            let lns = Array.zeroCreate segments
            let x = ln.FromX
            let y = ln.FromY
            for i = 0 to segments-1 do
                let g = float i
                let s = float (i+1)
                let sf = (g*segLen + g*gap)/len
                let ef = (s*segLen + g*gap)/len
                let xs = x + vx*sf
                let ys = y + vy*sf
                let xe = x + vx*ef
                let ye = y + vy*ef
                lns.[i] <- Line2D(xs,ys,xe,ye)
            // correct last point to avoid numerical errors
            lns.[segments-1] <- Line2D.withEnd ln.To lns.[segments-1]
            lns

    /// Divides a 2D line into as many as segments as possible respecting the minimum segment length and the gap.
    /// Includes a gap between the segments. But not at the start or end.
    /// Returns an array of 2D lines
    /// The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    static member splitMinLength (gap:float) (minSegmentLength:float) (ln:Line2D) : Line2D[] =
        let len = ln.Length
        if len < minSegmentLength then
            fail $"Line2D.splitMinLength: line length {len} is smaller than minSegmentLength {minSegmentLength}"
        if minSegmentLength < 1e-6 then
            fail $"Line2D.splitMinLength: minSegmentLength must be greater than 0.0, was {minSegmentLength}"
        let k = int ((len+gap) / ((minSegmentLength+gap)*1.000001))
        Line2D.split gap k ln

    /// Divides a 2D line into as few as segments as possible respecting the maximum segment length and the gap.
    /// Includes a gap between the segments. But not at the start or end.
    /// Returns an array of 2D lines
    /// The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    static member splitMaxLength (gap:float) (maxSegmentLength:float) (ln:Line2D)  : Line2D[] =
        let len = ln.Length
        if maxSegmentLength < 1e-6 then
            fail $"Line2D.splitMaxLength: maxSegmentLength must be greater than 0.0, was {maxSegmentLength}"
        if len < 1e-6 then
            fail $"Line2D.splitMaxLength: line length {len} is too small."
        let k = int ((len+gap) / (maxSegmentLength+gap)*0.999999) + 1
        Line2D.split gap k ln

    /// Divides a 2D line into segments of given length.
    /// Includes start and end point.
    /// If the line length is smaller than the given distance just the start and end point is returned.
    /// Adds end point only if there is a remainder bigger than 0.1% of the segment length.
    static member divideEvery dist (l:Line2D) : ResizeArray<Pt> =
        let len = l.Length
        let div = len / dist
        let floor = Math.Floor div
        if floor = 0.0 then
            let pts = ResizeArray<Pt>(2)
            pts.Add l.From
            pts.Add l.To
            pts
        else
            let step = dist / len
            let count = int floor
            let pts = ResizeArray<Pt>(count + 2)
            pts.Add l.From
            for i = 1 to count do
                pts.Add <| l.EvaluateAt (step * float i)
            if div - floor > 0.001 then
                pts.Add l.To // add end point only if there is a remainder bigger than 0.1%
            pts

    /// Divides a 2D line into segments of given length.
    /// Excludes start and end point.
    /// If the line length is smaller than the given distance an empty array is returned.
    /// Adds last div point before end only if there is a remainder bigger than 0.1% of the segment length.
    static member divideInsideEvery dist (l:Line2D) : ResizeArray<Pt> =
        let len = l.Length
        let div = len / dist
        let floor = Math.Floor div
        if floor = 0.0 then
            ResizeArray<Pt>()
        else
            let step = dist / len
            let count = int floor
            let pts = ResizeArray<Pt>(count)
            for i = 1 to count do
                if i < count || div - floor > 0.001 then
                    pts.Add <| l.EvaluateAt (step * float i)
            pts

    /// Project a line onto another line considered infinite in both directions.
    /// Returns the start and end parameters of the projected line on the target line.
    static member projectOntoRayParameters (rayToProjectOnto:Line2D) (lineToProject:Line2D) : float*float =
        let osx = rayToProjectOnto.FromX
        let osy = rayToProjectOnto.FromY
        let ovx = rayToProjectOnto.ToX - osx
        let ovy = rayToProjectOnto.ToY - osy
        let lenSq = ovx*ovx + ovy*ovy
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line2D.projectOntoRayParameters" rayToProjectOnto
        // start parameter
        let u = lineToProject.FromX - osx
        let v = lineToProject.FromY - osy
        let dot = ovx*u + ovy*v
        let s = dot / lenSq
        // end parameter
        let u = lineToProject.ToX - osx
        let v = lineToProject.ToY - osy
        let dot = ovx*u + ovy*v
        let e =  dot / lenSq
        s,e

    /// Project a line onto another line considered infinite in both directions.
    static member projectOntoRay (rayToProjectOnto:Line2D) (lineToProject:Line2D) : Line2D =
        let osx = rayToProjectOnto.FromX
        let osy = rayToProjectOnto.FromY
        let ovx = rayToProjectOnto.ToX - osx
        let ovy = rayToProjectOnto.ToY - osy
        let lenSq = ovx*ovx + ovy*ovy
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line2D.projectOntoRay" rayToProjectOnto
        // start parameter
        let u = lineToProject.FromX - osx
        let v = lineToProject.FromY - osy
        let dot = ovx*u + ovy*v
        let s = dot / lenSq
        // end parameter
        let u = lineToProject.ToX - osx
        let v = lineToProject.ToY - osy
        let dot = ovx*u + ovy*v
        let e =  dot / lenSq
        Line2D( osx + ovx * s,
                osy + ovy * s,
                osx + ovx * e,
                osy + ovy * e)

    /// Tries to project a line onto another line considered finite.
    /// Returns ValueNone if there is no overlap.
    /// Returns ValueSome (startParam, endParam) if there is an overlap.
    /// The parameters are between 0.0 and 1.0 on the target line
    /// The first parameter is from the start of the line to project.
    /// The second parameter is from the end of the line to project.
    /// Therefore, if the first parameter is greater than the second, the lines have opposite orientations.
    static member tryProjectOntoLineParameters (onToLine:Line2D) (lineToProject:Line2D) : voption<float*float> =
        let osx = onToLine.FromX
        let osy = onToLine.FromY
        let ovx = onToLine.ToX - osx
        let ovy = onToLine.ToY - osy
        let lenSq = ovx*ovx + ovy*ovy
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line2D.tryProjectOntoLineParameters" onToLine
        // start parameter
        let u = lineToProject.FromX - osx
        let v = lineToProject.FromY - osy
        let dot = ovx*u + ovy*v
        let bStartOnA = dot / lenSq
        // end parameter
        let u = lineToProject.ToX - osx
        let v = lineToProject.ToY - osy
        let dot = ovx*u + ovy*v
        let bEndOnA =  dot / lenSq
        if bStartOnA < -1e-6 && bEndOnA < -1e-6 then
            ValueNone
        elif bStartOnA > ``1.0 + 1e-6`` && bEndOnA > ``1.0 + 1e-6`` then
            ValueNone
        else
            ValueSome (clamp01 bStartOnA, clamp01 bEndOnA)

    /// Tries to project a line onto another line considered finite.
    /// Returns ValueSome Line2D if there is an overlap.
    /// Returns ValueNone if there is no overlap.
    /// Keeps the orientation of the line to project.
    static member tryProjectOntoLine (onToLine:Line2D) (lineToProject:Line2D) : voption<Line2D> =
        let osx = onToLine.FromX
        let osy = onToLine.FromY
        let ovx = onToLine.ToX - osx
        let ovy = onToLine.ToY - osy
        let lenSq = ovx*ovx + ovy*ovy
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line2D.tryProjectOntoLine" onToLine
        // start parameter
        let u = lineToProject.FromX - osx
        let v = lineToProject.FromY - osy
        let dot = ovx*u + ovy*v
        let bStartOnA = dot / lenSq
        // end parameter
        let u = lineToProject.ToX - osx
        let v = lineToProject.ToY - osy
        let dot = ovx*u + ovy*v
        let bEndOnA =  dot / lenSq
        if bStartOnA < -1e-6 && bEndOnA < -1e-6 then
            ValueNone
        elif bStartOnA > ``1.0 + 1e-6`` && bEndOnA > ``1.0 + 1e-6`` then
            ValueNone
        else
            let st = clamp01 bStartOnA
            let en = clamp01 bEndOnA
            ValueSome <| Line2D( osx + ovx * st,
                            osy + ovy * st,
                            osx + ovx * en,
                            osy + ovy * en)


    // #endregion
    // #region Intersection


    /// <summary> Intersects two infinite 2D lines. Fails if they are parallel.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> Use the XLine2D module for more specialized intersection calculations.
    /// Like getting the kind of Parallel relation between lines, only getting the parameter of one line,
    /// or setting a tolerance for when lines are parallel or coincident.</remarks>
    static member intersectRays (lnA:Line2D) (lnB:Line2D) : Pt =
        XLine2D.intersectRays lnA lnB

    /// <summary> A fast test tests if two finite 2D lines truly intersect.
    /// Does not use a default tolerance for parallel or coincident lines. Just checks within line range and not NaN.
    /// Returns FALSE on zero length lines or if parallel lines are touching or overlapping each other.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> Use the XLine2D module for more specialized intersection calculations.
    /// Like getting the kind of Parallel relation between lines, only getting the parameter of one line,
    /// or setting a tolerance for when lines are parallel or coincident.</remarks>
    /// <remarks> This does not check for an explicit angle tolerance, so overlapping almost parallel lines may return TRUE.</remarks>
    static member doIntersect (lnA:Line2D) (lnB:Line2D) : bool =
        XLine2D.doIntersect lnA lnB

    /// <summary> Tests if two finite 2D lines intersect or touch.
    /// Also returns TRUE if parallel lines are touching or overlapping each other.
    /// Also returns TRUE if a zero length lines are at the same location.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> This method uses an angle of 0.25 degrees to classify Lines as parallel.
    /// In which case it also checks if they overlap or touch.
    /// Use the Euclid's XLine2D module for more specialized intersection calculations.
    /// Like getting the kind of Parallel relation between lines, only getting the parameter of one line,
    /// or setting a tolerance for when lines are parallel or coincident.</remarks>
    static member doIntersectOrOverlap (lnA:Line2D) (lnB:Line2D) : bool =
        match XLine2D.classifyIntersectionParameters lnA lnB with
        | XLine2D.XParam.Intersect _        -> true
        | XLine2D.XParam.Parallel           -> XLine2D.doOverlap lnA lnB
        | XLine2D.XParam.Apart              -> false
        | XLine2D.XParam.TooShortBoth       -> XLineXY.sqDistLnFromLnFrom (lnA, lnB) < 1e-12
        | XLine2D.XParam.TooShortA          -> XLineXY.sqDistLnPt' (lnB, lnA.FromX, lnA.FromY) < 1e-12
        | XLine2D.XParam.TooShortB          -> XLineXY.sqDistLnPt' (lnA, lnB.FromX, lnB.FromY) < 1e-12

    /// <summary> A fast intersection of two finite 2D lines.
    /// Does not use a default tolerance for parallel or coincident lines.
    /// Just checks if both parameters are within line range and not NaN.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <returns> A Pt if the lines intersect truly,
    /// ValueNone if apart, too short, or if parallel lines are touching or overlapping each other.</returns>
    /// <remarks> Use the XLine2D module for more specialized intersection calculations.
    /// Like getting the kind of Parallel relation between lines, only getting the parameter of one line,
    /// or setting a tolerance for when lines are parallel or coincident.</remarks>
    static member tryIntersect(lnA:Line2D) (lnB:Line2D) : Pt voption =
        XLine2D.tryIntersectInRanges(-1e-6, ``1.0 + 1e-6``, -1e-6, ``1.0 + 1e-6``, lnA, lnB)

    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <returns>The point at which the two rays intersect, or ValueNone.</returns>
    /// <remarks> If the lines are parallel or coincident, or if the parameter on Line A is bigger than 1e12 in absolute value,
    /// ValueNone is returned.</remarks>
    static member tryIntersectRays(lineA:Line2D) (lineB:Line2D) : Pt voption =
        XLine2D.tryIntersectRays lineA lineB

    /// <summary> Intersects two finite 2D Lines.
    /// Also returns a point if parallel lines are touching or overlapping each other.
    /// Also returns a point if zero-length lines are at the same location within 1e-6 distance.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> This method uses an angle of 0.25 degrees to classify Lines as parallel.
    /// In which case it also checks if they overlap or touch.
    /// Parallel lines are treated as coincident when their perpendicular distance is below 1e-6
    /// (1e-12 squared), the same tolerance as Line2D.tryGetOverlap.
    /// Use the Euclid's XLine2D module for more specialized intersection calculations.
    /// Like getting the kind of Parallel relation between lines, only getting the parameter of one line,
    /// or setting a tolerance for when lines are parallel or coincident.</remarks>
    /// <returns> A voption of Pt
    /// If the lines are coincident and overlapping this point is in the center of their overlap.</returns>
    static member tryIntersectOrOverlap(lnA:Line2D) (lnB:Line2D) : Pt voption =
        match XLine2D.classifyIntersection lnA lnB with
        |XLine2D.XPt.Intersect p -> ValueSome p
        |XLine2D.XPt.Apart -> ValueNone
        |XLine2D.XPt.Parallel ->
            let sqDist = XLineXY.sqRayPtDist(lnA.FromX, lnA.FromY, lnA.VectorX, lnA.VectorY, lnB.FromX, lnB.FromY)
            if sqDist < 1e-12 then // squared distance: coincident within 1e-6
                match lnB |> Line2D.tryProjectOntoLineParameters lnA with
                | ValueSome (s,e) -> ValueSome <| lnA.EvaluateAt((s+e)*0.5)
                | ValueNone -> ValueNone
            else
                ValueNone
        | XLine2D.XPt.TooShortBoth -> if XLineXY.sqDistLnFromLnFrom (lnA, lnB) < 1e-12           then ValueSome lnA.From else ValueNone
        | XLine2D.XPt.TooShortA    -> if XLineXY.sqDistLnPt' (lnB, lnA.FromX, lnA.FromY) < 1e-12 then ValueSome lnA.From else ValueNone
        | XLine2D.XPt.TooShortB    -> if XLineXY.sqDistLnPt' (lnA, lnB.FromX, lnB.FromY) < 1e-12 then ValueSome lnB.From else ValueNone

    /// <summary> Checks if lines are parallel, coincident and overlapping.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <returns> A voption of a tuple of two floats.
    /// Returns the tuple containing the start and end parameter of the overlap on lnA if the lines are parallel and overlapping.
    /// Returns ValueNone if the lines are not parallel, not overlapping, or too short.</returns>
    /// <remarks> If the first parameter in the overlap is smaller than the second the lines are oriented in the same direction.
    /// If the first parameter is greater than the second the lines are oriented in the opposite direction.
    /// Parallel lines are treated as coincident when their perpendicular distance is below 1e-6
    /// (1e-12 squared), the same tolerance as Line2D.tryIntersectOrOverlap.</remarks>
    static member tryGetOverlap (lnA:Line2D) (lnB:Line2D) : voption<float*float> =
        let vax = lnA.VectorX
        let vay = lnA.VectorY
        let vbx = lnB.VectorX
        let vby = lnB.VectorY
        if XLineXY.isTooShort(vax, vay, 1e-6) || XLineXY.isTooShort(vbx, vby, 1e-6) then
            ValueNone
        else
            let tan = XLineXY.tangent(vax, vay, vbx, vby)
            if abs tan < Tangent.``0.25`` then
                let sqDist = XLineXY.sqRayPtDist(lnA.FromX, lnA.FromY, lnA.VectorX, lnA.VectorY, lnB.FromX, lnB.FromY)
                if sqDist < 1e-12 then // squared distance: coincident within 1e-6
                    Line2D.tryProjectOntoLineParameters lnA lnB
                else
                    ValueNone
            else
                ValueNone

    /// <summary> Computes the squared distance between two finite 2D lines.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <returns> The squared distance between the two lines. No matter if they are zero length or parallel</returns>
    static member distanceToLineSq (lnA:Line2D) (lnB:Line2D) : float =
        XLine2D.distanceSq lnA lnB

    /// <summary> Computes the distance between two finite 2D lines.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <returns> The distance between the two lines. No matter if they are zero length or parallel</returns>
    static member distanceToLine (lnA:Line2D) (lnB:Line2D) : float =
        XLine2D.distanceSq lnA lnB |> sqrt

    /// <summary>Checks if the two finite 2D lines are touching each other at any of end points
    /// within the given tolerance.
    /// This will also return TRUE if the lines are touching on both points.</summary>
    /// <param name="squareTolerance"> The squared tolerance for the distance between the end points.</param>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <remarks> Use <see cref="XLine2D.classifyEndsTouching"/> to get more detailed information about which ends are touching.</remarks>
    static member isTouchingEndOf squareTolerance (a:Line2D) (b:Line2D)  : bool =
        (
            let x = a.ToX-b.FromX
            let y = a.ToY-b.FromY
            x*x + y*y < squareTolerance
        ) || (
            let x = a.FromX-b.ToX
            let y = a.FromY-b.ToY
            x*x + y*y < squareTolerance
        ) || (
            let x = a.FromX-b.FromX
            let y = a.FromY-b.FromY
            x*x + y*y < squareTolerance
        ) || (
            let x = a.ToX-b.ToX
            let y = a.ToY-b.ToY
            x*x + y*y < squareTolerance
        )

    // #endregion
