namespace Euclid

open System
open Euclid.UtilEuclid
open UtilEuclid
open EuclidErrors

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Line2D.
[<AutoOpen>]
module AutoOpenLine2D =

  type Line2D with

    /// Checks if 2D line is parallel to the world X axis. Ignoring orientation.
    /// The absolute deviation tolerance along Y axis is 1e-9.
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsXAligned =
        let x = abs ln.VectorX
        let y = abs ln.VectorY
        if isTooSmall (x+y) then
            failTooSmall "Line2D.IsXAligned" ln
        y < 1e-9

    /// Checks if 2D line is parallel to the world Y axis. Ignoring orientation.
    /// The absolute deviation tolerance along X axis is 1e-9.
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsYAligned =
        let x = abs ln.VectorX
        let y = abs ln.VectorY
        if isTooSmall (x+y) then
            failTooSmall "Line2D.IsYAligned" ln
        x < 1e-9

    /// Check if the line has the same starting and ending point.
    member inline ln.IsZeroLength =
        ln.ToX = ln.FromX &&
        ln.ToY = ln.FromY

    /// Returns TRUE if line is shorter than tolerance.
    /// Or has NaN components.
    member inline ln.IsTiny tol =
        not (ln.Length > tol) // use not(..) to catch NaN too

    /// Returns TRUE if the line's square length is shorter than squared tolerance.
    /// Or has NaN components.
    member inline ln.IsTinySq tol =
        not (ln.LengthSq > tol)

    /// Evaluate line at a given parameter (parameters 0.0 to 1.0 are on the line).
    member inline ln.EvaluateAt (p:float) =
        Pt  (ln.FromX + ln.VectorX*p,
             ln.FromY + ln.VectorY*p)

    /// Evaluate line at given parameters (parameters 0.0 to 1.0 are on the line).
    /// Returns a new line from evaluated points.
    /// Same as ln.Segment(start,ende).
    member inline ln.SubLine (start:float, ende:float) =
        let fromX = ln.FromX
        let fromY = ln.FromY
        let x = ln.ToX-fromX
        let y = ln.ToY-fromY
        Line2D( fromX + x * start,
                fromY + y * start,
                fromX + x * ende ,
                fromY + y * ende )

    /// Evaluate line at given parameters (parameters 0.0 to 1.0 are on the line).
    /// Returns a new line from evaluated points.
    /// Same as ln.SubLine(start,ende).
    member inline ln.Segment(start:float, ende:float) =
        let fromX = ln.FromX
        let fromY = ln.FromY
        let x = ln.ToX-fromX
        let y = ln.ToY-fromY
        Line2D( fromX + x * start,
                fromY + y * start,
                fromX + x * ende ,
                fromY + y * ende )

    /// Returns the length of the line segment from the start point to the given parameter.
    /// This length is negative if the parameter is negative.
    member inline ln.LengthTillParam (p:float) =
        let x = ln.VectorX * p
        let y = ln.VectorY * p
        let l = sqrt(x*x + y*y)
        if p> 0.0 then l else -l

    /// Returns the length of the line segment from the given parameter till the line End.
    /// This length is negative if the parameter is bigger than 1.0.
    member inline ln.LengthFromParam (t:float) =
        let p = 1.0-t
        let x = ln.VectorX * p
        let y = ln.VectorY * p
        let l = sqrt(x*x + y*y)
        if p> 0.0 then l else -l


    /// Returns the midpoint of the line,
    member inline ln.Mid =
        let x = (ln.ToX + ln.FromX)*0.5
        let y = (ln.ToY + ln.FromY)*0.5
        Pt(x, y)

    /// Returns the Line2D reversed.
    member inline ln.Reversed =
        Line2D(ln.ToX, ln.ToY, ln.FromX, ln.FromY)


    /// Extend 2D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.Extend (distAtStart:float, distAtEnd:float) =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.Extend" ln
        Line2D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l)

    /// Extend 2D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendStart (distAtStart:float) =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendStart" ln
        Line2D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.ToX,
                ln.ToY)

    /// Extend 2D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendEnd (distAtEnd:float) =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendEnd" ln
        Line2D( ln.FromX,
                ln.FromY,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l)

    /// Extend 2D line by relative amount at start and end.
    /// A relative amount of 0.5 extends the line by half its length on each side.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendRel (relAtStart:float, relAtEnd:float) =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendRel" ln
        Line2D( ln.FromX - x*relAtStart,
                ln.FromY - y*relAtStart,
                ln.ToX   + x*relAtEnd,
                ln.ToY   + y*relAtEnd)

    /// Extend 2D line by relative amount at start.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendStartRel (relAtStart:float) =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendStartRel" ln
        Line2D( ln.FromX - x*relAtStart,
                ln.FromY - y*relAtStart,
                ln.ToX,
                ln.ToY)

    /// Extend 2D line by relative amount at end.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendEndRel (relAtEnd:float) =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ExtendEndRel" ln
        Line2D( ln.FromX,
                ln.FromY,
                ln.ToX   + x*relAtEnd,
                ln.ToY   + y*relAtEnd)

    /// Shrink 2D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.Shrink (distAtStart:float, distAtEnd:float) =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.Shrink" ln
        Line2D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l)

    /// Shrink 2D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ShrinkStart (distAtStart:float) =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ShrinkStart" ln
        Line2D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.ToX,
                ln.ToY)

    /// Shrink 2D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ShrinkEnd (distAtEnd:float) =
        let x = ln.VectorX
        let y = ln.VectorY
        let l = sqrt(x*x + y*y)
        if isTooTiny l then failTooSmall "Line2D.ShrinkEnd" ln
        Line2D( ln.FromX,
                ln.FromY,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l)


    /// Returns a Line2D moved by a vector.
    member inline ln.Move (v:Vc) =
        Line2D( ln.FromX+v.X,
                ln.FromY+v.Y,
                ln.ToX+v.X,
                ln.ToY+v.Y)

    /// Returns a Line2D moved by a given distance in X direction.
    member inline ln.MoveX (distance:float) =
        Line2D( ln.FromX+distance,
                ln.FromY,
                ln.ToX+distance,
                ln.ToY)

    /// Returns a Line2D moved by a given distance in Y direction.
    member inline ln.MoveY (distance:float) =
        Line2D( ln.FromX,
                ln.FromY+distance,
                ln.ToX,
                ln.ToY+distance)


    /// Assumes the Line2D to be an infinite ray!
    /// Returns the parameter at which a point is closest to the ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Fails on curves shorter than 1e-9 units. (ln.ClosestParameter does not)
    member ln.RayClosestParameter (pt:Pt) =
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let lenSq = x*x + y*y
        if isTooSmallSq(lenSq) then // the parameter is infinite so we have to fail
            failTooSmall2 "Line2D.RayClosestParameter" ln pt
        let u = ln.FromX-pt.X
        let v = ln.FromY-pt.Y
        let dot = x*u + y*v
        dot / lenSq

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    /// Does not fail on very short curves.
    member inline ln.ClosestParameter (p:Pt) =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let dot = x*u + y*v
        let lenSq = x*x + y*y
        if isTooSmallSq (lenSq) then // if the parameter is infinite we can still return 0.0 or 1.0 since that is our range
            if dot < 0.0 then 0.0 else 1.0
        else
            dot / lenSq |> clampBetweenZeroAndOne



    /// Assumes Line2D to be an infinite ray!
    /// Returns closest point on ray.
    /// Fails on curves shorter than 1e-9 units. (ln.ClosestPoint does not.)
    member ln.RayClosestPoint (pt:Pt) =
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

    /// Returns closest point on (finite) line.
    /// Does not fail on very short curves.
    member ln.ClosestPoint (p:Pt) =
        ln.EvaluateAt(ln.ClosestParameter(p))


    /// <summary> Finds the closest points between two finite 2D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> For parallel and overlapping lines the points returned are in the center of their overlap.
    /// If the lines intersect the returned points are exactly the same</remarks>
    member lnA.ClosestPoints (lnB:Line2D) : Pt * Pt =
        match XLine2D.getClosestPoints(lnA, lnB) with
        | XLine2D.ClPts.Apart (a,b, _) -> a,b
        | XLine2D.ClPts.Parallel (a,b) -> a,b
        | XLine2D.ClPts.Intersect a -> a,a
        | XLine2D.ClPts.TooShortBoth -> lnA.From , lnB.From // TODO or use midpoint, or use end point ? they are all kind off the same
        | XLine2D.ClPts.TooShortA    -> lnA.From , XLine2D.clPtLn' (lnB, lnA.FromX, lnA.FromY)
        | XLine2D.ClPts.TooShortB    -> XLine2D.clPtLn' (lnA, lnB.FromX, lnB.FromY), lnB.From


    /// <summary> Finds the parameters of closest points between two finite 2D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> For parallel and overlapping lines the parameters returned are in the center of their overlap.
    ///  If the lines intersect the returned parameters are exactly the same</remarks>
    /// <returns> A tuple of two floats, the first is the parameter on lnA, the second on lnB</returns>
    member lnA.ClosestParameters (lnB:Line2D) : float * float =
        match XLine2D.getClosestParameters(lnA, lnB) with
        | XLine2D.ClParams.Apart (a,b, _) -> a,b
        | XLine2D.ClParams.Parallel (a,b) -> a,b
        | XLine2D.ClParams.Intersect (a,b) -> a,b
        | XLine2D.ClParams.TooShortBoth -> 0.0  , 0.0 // TODO or use midpoint, or use end point ? they are all kind off the same
        | XLine2D.ClParams.TooShortA    -> 0.0  , XLine2D.clParamLnPt (lnB, lnA.FromX, lnA.FromY)
        | XLine2D.ClParams.TooShortB    -> XLine2D.clParamLnPt (lnA, lnB.FromX, lnB.FromY), 0.0

    /// Assumes Line2D to be an infinite ray!
    /// Returns square distance from point to ray.
    /// Fails on curves shorter than 1e-9 units. (ln.DistanceSqFromPoint does not.)
    member ln.SqDistanceRayPoint(p:Pt) =
        let vAx = ln.VectorX
        let vAy = ln.VectorY
        // normal vector:
        let nx = -vAy
        let ny = vAx
        let lenSq = nx*nx + ny*ny
        if isTooSmallSq(lenSq) then
            failTooSmall2 "Line2D.SqDistanceRayPoint" ln p
        let u = ln.FromX - p.X
        let v = ln.FromY - p.Y
        let dot = nx*u + ny*v
        (dot*dot) / lenSq

    /// Assumes Line2D to be an infinite ray!
    /// Returns distance from point to ray.
    /// Fails on curves shorter than 1e-9 units. (ln.DistanceToPt does not.)
    member inline ln.DistanceRayPoint(p:Pt) =
        ln.SqDistanceRayPoint(p) |> sqrt

    /// Returns square distance from point to finite line.
    member ln.SqDistanceFromPoint(p:Pt) =
        XLine2D.sqDistLnPt'(ln, p.X, p.Y)

    /// Returns distance from point to (finite) line.
    member inline ln.DistanceToPt(p:Pt) =
        XLine2D.sqDistLnPt'(ln, p.X, p.Y) |> sqrt




    /// Checks if the dot product between the two 2D lines is positive .
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation (otherLn:Line2D) =
        let vLn = ln.Vector
        let vOt = otherLn.Vector
        if isTooTinySq(vLn.LengthSq) then failTooSmall2 "Line2D.MatchesOrientation this" ln otherLn
        if isTooTinySq(vOt.LengthSq) then failTooSmall2 "Line2D.MatchesOrientation other" otherLn ln
        vLn *** vOt > 1e-12


    /// Checks if the dot product between the a 2D lines and a vector is positive .
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation (v:Vc) =
        let vLn = ln.Vector
        if isTooTinySq(v.LengthSq)   then failTooSmall2 "Line2D.MatchesOrientation this" v ln
        if isTooTinySq(vLn.LengthSq) then failTooSmall2 "Line2D.MatchesOrientation other" ln v
        vLn *** v > 1e-12

    /// Checks if the dot product between the a 2D lines and a unit-vector is positive .
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation (v:UnitVc) =
        let vLn = ln.Vector
        if isTooTinySq(vLn.LengthSq) then failTooSmall2 "Line2D.MatchesOrientation other" ln v
        vLn *** v > 1e-12



    /// Checks if the angle between the two 2D lines is less than 45 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation45 (l:Line2D) =
        let vLn = ln.Vector
        let vOt = l.Vector
        if isTooTinySq(vLn.LengthSq) then failTooSmall2 "Line2D.MatchesOrientation45 this" vLn vOt
        if isTooTinySq(vOt.LengthSq) then failTooSmall2 "Line2D.MatchesOrientation45 other" vOt vLn
        let tan = XLine2D.tangent (vLn.X, vLn.Y, vOt.X, vOt.Y)
        tan < Tangent.``45.0``



    /// Checks if two 2D lines are parallel.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:Line2D, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) =
        let a = ln.Vector
        let b = other.Vector
        if isTooTinySq(a.LengthSq) then failTooSmall2 "Line2D.IsParallelTo this" a b
        if isTooTinySq(b.LengthSq) then failTooSmall2 "Line2D.IsParallelTo other" b a
        let tan = XLine2D.tangent (a.X, a.Y, b.X, b.Y)
        abs tan < minTangent


    /// Checks if a 2D lines is parallel to a 2D vector.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:Vc, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) =
        let a = ln.Vector
        if isTooTinySq(a.LengthSq) then failTooSmall2 "Line2D.IsParallelTo" a other
        if isTooTinySq(other.LengthSq) then failTooSmall2 "Line2D.IsParallelTo" other a
        let tan = XLine2D.tangent (a.X, a.Y, other.X, other.Y)
        abs tan < minTangent

    /// Checks if a 2D lines is parallel to a 2D unit-vector.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:UnitVc, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) =
        let a = ln.Vector
        if isTooTinySq(a.LengthSq) then failTooSmall2 "Line2D.IsParallelTo" a other
        let tan = XLine2D.tangent (a.X, a.Y, other.X, other.Y)
        abs tan < minTangent


    /// Checks if two 2D lines are parallel.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:Line2D, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) =
        let a = ln.Vector
        let b = other.Vector
        if isTooTinySq(a.LengthSq) then failTooSmall2 "Line2D.IsParallelAndOrientedTo this" a b
        if isTooTinySq(b.LengthSq) then failTooSmall2 "Line2D.IsParallelAndOrientedTo other" b a
        let tan = XLine2D.tangent (a.X, a.Y, b.X, b.Y)
        tan < minTangent

    /// Checks if a 2D lines is parallel to a 2D vector.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines  or vectors shorter than 1e-12.
    member inline ln.IsParallelAndOrientedTo (other:Vc, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) =
        let a = ln.Vector
        if isTooTinySq(a.LengthSq) then failTooSmall2 "Line2D.IsParallelAndOrientedTo" a other
        if isTooTinySq(other.LengthSq) then failTooSmall2 "Line2D.IsParallelAndOrientedTo" other a
        let tan = XLine2D.tangent (a.X, a.Y, other.X, other.Y)
        tan < minTangent


    /// Checks if a 2D lines is parallel to a 2D unit-vector.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:UnitVc, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) =
        let a = ln.Vector
        if isTooTinySq(a.LengthSq) then failTooSmall2 "Line2D.IsParallelAndOrientedTo" a other
        let tan = XLine2D.tangent (a.X, a.Y, other.X, other.Y)
        tan < minTangent


    /// Checks if two 2D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:Line2D, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) =
        let a = ln.Vector
        let b = other.Vector
        if isTooTinySq(a.LengthSq) then failTooSmall2 "Line2D.IsPerpendicularTo this" a b
        if isTooTinySq(b.LengthSq) then failTooSmall2 "Line2D.IsPerpendicularTo other" b a
        let tan = XLine2D.tangent (a.X, a.Y, b.X, b.Y)
        abs tan > maxTangent



    /// Checks if a 2D lines is perpendicular to a 2D vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:Vc, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) =
        let a = ln.Vector
        if isTooTinySq(a.LengthSq) then failTooSmall2 "Line2D.IsPerpendicularTo this" a other
        if isTooTinySq(other.LengthSq) then failTooSmall2 "Line2D.IsPerpendicularTo other" other a
        let tan = XLine2D.tangent (a.X, a.Y, other.X, other.Y)
        abs tan > maxTangent

    /// Checks if a 2D lines is perpendicular to a 2D unit-vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:UnitVc, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) =
        let a = ln.Vector
        if isTooTinySq(a.LengthSq) then failTooSmall2 "Line2D.IsPerpendicularTo this" a other
        let tan = XLine2D.tangent (a.X, a.Y, other.X, other.Y)
        abs tan > maxTangent


    /// Checks if two 2D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Alias for ln.IsPerpendicularTo.
    member inline ln.IsNormalTo (other:Line2D, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if a 2D lines is perpendicular to a 2D vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Alias for ln.IsPerpendicularTo.
    member inline ln.IsNormalTo (other:Vc, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if a 2D lines is perpendicular to a 2D unit-vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Alias for ln.IsPerpendicularTo.
    member inline ln.IsNormalTo (other:UnitVc, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) =
        ln.IsPerpendicularTo(other, maxTangent)


    /// Checks if two 2D lines are coincident on the same ray within the distance tolerance. 1e-6 by default.
    /// This means that lines are parallel within the angle tolerance
    /// and the distance of second start to the first line is less than the distance tolerance.
    /// Also returns false on zero Length lines.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    member inline ln.IsCoincidentTo (other:Line2D,
                                    [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                    [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent>) =
        let vtX = ln.VectorX
        let vtY = ln.VectorY
        let voX = other.VectorX
        let voY = other.VectorY
        if isTooTinySq(vtX*vtX + vtY*vtY) then failTooSmall2 "Line2D.IsCoincidentTo this" ln other
        if isTooTinySq(voX*voX + voY*voY) then failTooSmall2 "Line2D.IsCoincidentTo other" other ln
        let dot = vtX*voX + vtY*voY
        let det = vtX * voY - vtY * voX
        let tan = det / dot |> abs
        if !^ tan < minTangent then // handles NaN
            // they are parallel, now check distance:
            let x = other.FromX - ln.FromX
            let y = other.FromY - ln.FromY
            let dotN =  x*vtY - y*vtX // looks like a cross product, but is the dot with the normal
            let lenSq = vtX*vtX + vtY*vtY
            let dist = (dotN * dotN) / lenSq
            dist < distanceTolerance
        else
            // they are not parallel within tolerance
            false



    /// A fast version of IsParallelTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are parallel within a given tolerance for their cross product.
    /// Lines are parallel if cross product is less than parallelogramAreaTolerance.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// ATTENTION this method returns TRUE on zero length or very small lines.
    /// It returns FALSE on almost coincident lines that are very long.
    /// Pick a bigger parallelogramAreaTolerance for longer lines.
    member inline ln.IsParallelToFast( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float ) =
        // Get direction vectors
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d2x = other.VectorX
        let d2y = other.VectorY
        // Cross product in 2D (scalar value)
        let cross = d1x * d2y - d1y * d2x
        // Lines are parallel if cross product is near zero
        abs cross < parallelogramAreaTolerance


    /// A fast version of IsCoincidentTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are coincident within a given tolerance for their cross product.
    /// Lines are coincident if they are parallel (cross product less than parallelogramAreaTolerance)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// ATTENTION this method returns TRUE on zero length or very small lines.
    /// It returns FALSE on almost coincident lines that are very long.
    /// Pick a bigger parallelogramAreaTolerance for longer lines.
    member inline ln.IsCoincidentToFast ( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float) =
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


    /// A fast version of IsParallelToAndOrientedTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are parallel within a given tolerance for their cross product
    /// and oriented in the same direction by having a positive dot-product.
    /// Lines are parallel if cross product is less than given parallelogramAreaTolerance.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// Lines are oriented if dot product is positive.
    /// minDotProduct must be POSITIVE! just above zero. 1e-6 by default.
    /// The minDotProduct argument helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsParallelAndOrientedToFast( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float , [<OPT;DEF(1e-6)>]minDotProduct :float) =
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

    /// A fast version of IsParallelToAndOpposingTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are parallel within a given tolerance for their cross product
    /// and opposing by having a negative dot-product.
    /// Lines are parallel if cross product is less than given parallelogramAreaTolerance.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// Lines are opposing if dot product is negative.
    /// maxDotProduct must be NEGATIVE! just below zero, -1e-6 by default !
    /// The maxDotProduct helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsParallelAndOpposingToFast( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float , [<OPT;DEF(-1e-6)>]maxDotProduct :float) =
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

    /// A fast version of IsCoincidentAndOrientedTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are coincident on the same ray within given tolerances.
    /// Lines are coincident if they are parallel (cross product less than parallelogramAreaTolerance)
    /// and oriented in the same direction if the dot-product is positive (bigger than minDotProduct)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// Lines are oriented if dot product is positive.
    /// minDotProduct must be POSITIVE! just above zero. 1e-6 by default.
    /// The minDotProduct argument helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsCoincidentAndOrientedToFast ( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float, [<OPT;DEF(1e-6)>]minDotProduct :float) =
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

    /// A fast version of IsCoincidentAndOpposingTo that uses cross-product to determine parallelism.
    /// Checks if two 2D lines are coincident on opposing rays within given tolerances.
    /// Lines are coincident if they are parallel (cross product less than parallelogramAreaTolerance)
    /// and opposing if the dot-product is negative (smaller than maxDotProduct)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product corresponds to the signed area of the parallelogram spanned by the two direction vectors.
    /// Lines are opposing if dot product is negative.
    /// maxDotProduct must be NEGATIVE! just below zero, -1e-6 by default !
    /// The maxDotProduct helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsCoincidentAndOpposingToFast( other:Line2D, [<OPT;DEF(1e-6)>]parallelogramAreaTolerance:float, [<OPT;DEF(-1e-6)>]maxDotProduct :float) =
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



    /// Looking in the direction of the line.
    /// Check if a given point is on the right side of the ray.
    /// Also returns false if the point is on the line.
    member inline ln.IsPointOnRight(pt:Pt) =
        let lv = ln.Vector.Rotate90CW
        let pv = pt - ln.From
        lv *** pv > 0.0

    /// Looking in the direction of the line.
    /// Check if a given point is on the left side of the infinite ray.
    /// Also returns false if the point is on the line.
    member inline ln.IsPointOnLeft(pt:Pt) =
        let lv = ln.Vector.Rotate90CCW
        let pv = pt - ln.From
        lv *** pv > 0.0


    /// Scales the 2D line by a given factor.
    /// Scale center is World Origin 0,0
    member inline l.Scale (factor:float) : Line2D =
        Line2D(
            l.FromX * factor,
            l.FromY * factor,
            l.ToX   * factor,
            l.ToY   * factor)

    /// Scales the 2D line by a given factor on a given center point
    member inline l.ScaleOn (cen:Pt) (factor:float) : Line2D =
        let cx = cen.X
        let cy = cen.Y
        Line2D(
            cx + (l.FromX - cx) * factor,
            cy + (l.FromY - cy) * factor,
            cx + (l.ToX   - cx) * factor,
            cy + (l.ToY   - cy) * factor)

    /// Rotate a Line2D by a Rotation2D.
    /// The resulting line has the same length as the input.
    member inline ln.Rotate (r:Rotation2D) =
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

    /// Rotate a Line2D by a Rotation2D around a given center point.
    /// The resulting line has the same length as the input.
    member inline ln.RotateWithCenter (cen:Pt, r:Rotation2D) =
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



    // ----------------------------------------------------------------------------------
    //            █████               █████     ███
    //           ░░███               ░░███     ░░░
    //    █████  ███████    ██████   ███████   ████   ██████
    //   ███░░  ░░░███░    ░░░░░███ ░░░███░   ░░███  ███░░███
    //  ░░█████   ░███      ███████   ░███     ░███ ░███ ░░░
    //   ░░░░███  ░███ ███ ███░░███   ░███ ███ ░███ ░███  ███
    //   ██████   ░░█████ ░░████████  ░░█████  █████░░██████
    //  ░░░░░░     ░░░░░   ░░░░░░░░    ░░░░░  ░░░░░  ░░░░░░
    //
    //                                             █████
    //                                            ░░███
    //    █████████████    ██████  █████████████   ░███████   ██████  ████████   █████
    //   ░░███░░███░░███  ███░░███░░███░░███░░███  ░███░░███ ███░░███░░███░░███ ███░░
    //    ░███ ░███ ░███ ░███████  ░███ ░███ ░███  ░███ ░███░███████  ░███ ░░░ ░░█████
    //    ░███ ░███ ░███ ░███░░░   ░███ ░███ ░███  ░███ ░███░███░░░   ░███      ░░░░███
    //    █████░███ █████░░██████  █████░███ █████ ████████ ░░██████  █████     ██████
    //   ░░░░░ ░░░ ░░░░░  ░░░░░░  ░░░░░ ░░░ ░░░░░ ░░░░░░░░   ░░░░░░  ░░░░░     ░░░░░░
    // --------------------------------------------------------------------------------



    /// Checks if two 2D lines are equal within tolerance.
    /// Identical Lines in opposite directions are not considered equal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member inline equals (tol:float) (a:Line2D) (b:Line2D)  : bool =
        abs (a.FromX - b.FromX) <= tol &&
        abs (a.FromY - b.FromY) <= tol &&
        abs (a.ToX   - b.ToX  ) <= tol &&
        abs (a.ToY   - b.ToY  ) <= tol


    /// Check if two 2D lines are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two 2D-lines are not exactly equal.
    static member notEquals (tol:float) (a:Line2D) (b:Line2D)  : bool =
        abs (a.FromX - b.FromX) > tol ||
        abs (a.FromY - b.FromY) > tol ||
        abs (a.ToX   - b.ToX  ) > tol ||
        abs (a.ToY   - b.ToY  ) > tol


    /// Returns the Cross Product or Determinant between the vectors of the Two lines.
    /// In 2D it is just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
    /// If the result is zero the two lines are parallel (or one line is zero-length).
    /// Same as Line2D.det
    static member inline cross (a:Line2D) (b:Line2D)  : float =
        let ax = a.VectorX
        let ay = a.VectorY
        let bx = b.VectorX
        let by = b.VectorY
        ax*by - ay*bx


    /// Returns the Cross Product or Determinant between the vectors of the Two lines.
    /// In 2D it is just a scalar equal to the signed area of the parallelogram spanned by the input vectors.
    /// If the result is zero the two lines are parallel (or one line is zero-length).
    /// Same as Line2D.cross.
    static member inline det (a:Line2D) (b:Line2D) =
        let ax = a.VectorX
        let ay = a.VectorY
        let bx = b.VectorX
        let by = b.VectorY
        ax*by - ay*bx


    /// Creates a 3D line from a 2D line with Z coordinates set to the specified level.
    /// The default zLevel is 0.0 if not provided.
    static member inline toLine3D ([<OPT;DEF(0.0)>]zLevel:float) (ln:Line2D) =
        Line3D(ln.FromX, ln.FromY, zLevel, ln.ToX, ln.ToY, zLevel)

    /// Creates a 2D line from a 3D line by projecting onto the XY plane.
    /// The Z coordinate is discarded.
    static member inline createFromLine3D (ln:Line3D) =
        Line2D(ln.FromX, ln.FromY, ln.ToX, ln.ToY)


    /// Creates a 2D line starting at World Origin and going to along the given vector.
    static member inline createFromVec (v:Vc) =
        Line2D(0., 0., v.X, v.Y)

    /// Creates a 2D line starting at given point and going to along the given vector.
    static member inline createFromPtAndVc (p:Pt, v:Vc) =
        Line2D(p.X, p.Y, p.X+v.X, p.Y+v.Y)

    /// Returns the Start point of the line. Same as Line2D.from.
    static member inline start (l:Line2D) =
        l.From

    /// Returns the Start point of the line. Same as Line2D.start.
    static member inline from (l:Line2D) =
        l.From

    /// Returns the Start point's X coordinate of the line.
    static member inline fromX (l:Line2D) =
        l.FromX

    /// Returns the Start point's Y coordinate of the line.
    static member inline fromY (l:Line2D) =
        l.FromY

    /// Returns the End point of the line. Same as Line2D.to'
    static member inline ende (l:Line2D) =
        l.To

    /// Returns the End point of the line. Same as Line2D.ende.
    static member inline to' (l:Line2D) =
        l.To

    /// Returns the End point's X coordinate of the line.
    static member inline toX (l:Line2D) =
        l.ToX

    /// Returns the End point's Y coordinate of the line.
    static member inline toY (l:Line2D) =
        l.ToY

    /// Set Line2D start point, returns a new line.
    static member inline setStart (pt:Pt) (ln:Line2D) =
        Line2D( pt.X, pt.Y, ln.ToX, ln.ToY)

    /// Set Line2D end point, returns a new line.
    static member inline setEnd (pt:Pt) (ln:Line2D) =
        Line2D( ln.FromX, ln.FromY, pt.X, pt.Y)

    /// Same as Line2D.vector or Line2D.tangent.
    /// The returned vector has the same length as the Line2D.
    static member inline direction (ln:Line2D) =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as Line2D.tangent or Line2D.direction.
    /// The returned vector has the same length as the Line2D.
    static member inline vector (ln:Line2D) =
        Vc(ln.VectorX, ln.VectorY)

    /// Same as Line2D.vector or Line2D.direction.
    /// The returned vector has the same length as the Line2D.
    static member inline tangent (ln:Line2D) =
        Vc(ln.VectorX, ln.VectorY)

    /// Returns a unit-vector of the line Direction.
    static member inline unitTangent (ln:Line2D) =
        ln.UnitTangent

    /// Returns the length of the line.
    static member inline length (l:Line2D) =
        l.Length

    /// Returns the square length of the line.
    static member inline lengthSq (l:Line2D) =
        l.LengthSq

    /// Check if the line has same starting and ending point.
    static member inline isZeroLength (l:Line2D) =
        l.IsZeroLength

    /// Check if line is shorter than tolerance.
    /// Or contains a NaN value
    static member inline isTiny tol (l:Line2D) =
        not (l.Length > tol)

    /// Check if line is shorter than squared tolerance.
    /// Or contains a NaN value
    static member inline isTinySq tol (l:Line2D) =
        not (l.LengthSq > tol)

    /// Checks if 2D line is parallel to the world X axis. Ignoring orientation.
    /// The absolute deviation tolerance along Y axis is 1e-9.
    /// Fails on lines shorter than 1e-6.
    static member inline isXAligned (l:Line2D) =
        l.IsXAligned

    /// Checks if 2D line is parallel to the world Y axis. Ignoring orientation.
    /// The absolute deviation tolerance along X axis is 1e-9.
    /// Fails on lines shorter than 1e-6.
    static member inline isYAligned (l:Line2D) =
        l.IsYAligned

    /// Evaluate line at a given parameter ( parameters 0.0 to 1.0 are on the line)
    static member inline evaluateAt t (ln:Line2D) =
        ln.EvaluateAt t

    /// Returns new Line2D from point at Parameter a to point at Parameter b.
    /// Same as Line2D.segment
    static member inline subLine start ende (ln:Line2D) =
        ln.Segment (start, ende)

    /// Returns new Line2D from point at Parameter a to point at Parameter b.
    /// Same as Line2D.subLine
    static member inline segment start ende (ln:Line2D) =
        ln.Segment (start, ende)

    /// Get point at center of line.
    static member inline mid (ln:Line2D) =
        ln.Mid

    /// Reverse or flip the Line2D (same as Line2D.flip)
    static member inline reverse (ln:Line2D) =
        ln.Reversed

    /// Reverse or flip the Line2D (same as Line2D.reverse)
    static member inline flip (ln:Line2D) =
        ln.Reversed



    /// Move a Line2D by a vector. (same as Line2D.move)
    static member inline translate (v:Vc) (ln:Line2D)  : Line2D =
        ln.Move(v)

    /// Returns a Line2D moved by a given distance in X direction.
    static member inline moveX (distance:float) (ln:Line2D)  : Line2D =
        ln.MoveX(distance)

    /// Returns a Line2D moved by a given distance in Y direction.
    static member inline moveY (distance:float) (ln:Line2D)  : Line2D =
        ln.MoveY(distance)

    /// Move a Line2D by a vector. (same as Line2D.translate)
    static member inline move (v:Vc) (ln:Line2D)  : Line2D =
        ln.Move(v)

    /// Rotation a Line2D.
    static member inline rotate (r:Rotation2D) (ln:Line2D) =
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

    /// Rotation a Line2D around a given Center.
    static member inline rotateWithCenter (cen:Pt) (r:Rotation2D) (ln:Line2D) =
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

    /// Ensure 2D line has a positive dot product with given orientation line.
    static member inline matchOrientation (orientationToMatch:Line2D) (lineToFlip:Line2D) =
        if orientationToMatch.Vector *** lineToFlip.Vector < 0.0 then lineToFlip.Reversed else lineToFlip

    /// Ensure 2D line has a positive dot product with given orientation 2D vector.
    static member inline matchVcOrientation (orientationToMatch:Vc) (lineToFlip:Line2D) =
        if orientationToMatch *** lineToFlip.Vector < 0.0 then lineToFlip.Reversed else lineToFlip

    /// Ensure 2D line has a positive dot product with given orientation 2D vector.
    static member inline matchUnitVcOrientation (orientationToMatch:UnitVc) (lineToFlip:Line2D) =
        if orientationToMatch *** lineToFlip.Vector < 0.0 then lineToFlip.Reversed else lineToFlip


    /// Checks if the dot product between the a 2D lines and a unit-vector is positive .
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientation (l:Line2D) (ln:Line2D) =
        l.MatchesOrientation ln

    /// Checks if the angle between the two 2D lines is less than 45 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientation45 (l:Line2D) (ln:Line2D) =
        l.MatchesOrientation45 ln

    /// Checks if two 2D lines are coincident on the same ray within tolerance.
    /// This means that lines are parallel within 0.25 degrees
    /// and the distance of second start point to the first line is less than 1e-6.
    static member inline isCoincidentTo (a:Line2D) (b:Line2D) =
        a.IsCoincidentTo(b)

    /// Checks if two 2D lines are parallel. Ignoring orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelTo (lnA:Line2D) (lnB:Line2D) =
        lnA.IsParallelTo(lnB)

    /// Checks if two 2D lines are Not parallel. Ignoring orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isNotParallelTo (lnA:Line2D) (lnB:Line2D) =
        not <| lnA.IsParallelTo(lnB)

    /// Checks if two 2D lines are parallel and orientated the same way.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelAndOrientedTo (lnA:Line2D) (lnB:Line2D) =
        lnA.IsParallelAndOrientedTo(lnB)

    /// Checks if two 2D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as Line2D.isNormalTo
    static member inline isPerpendicularTo (lnA:Line2D) (lnB:Line2D) =
        lnA.IsPerpendicularTo(lnB)

    /// Checks if two 2D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as Line2D.isNormalTo
    static member inline isNormalTo (lnA:Line2D) (lnB:Line2D) =
        lnA.IsPerpendicularTo(lnB)





    /// A faster Check if two 2D lines are parallel. Ignoring orientation.
    /// The angle tolerance is 0.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return false on zero-length lines.
    static member inline isParallelTo' (lnA:Line2D) (lnB:Line2D) : bool =
        let vtX = lnA.VectorX
        let vtY = lnA.VectorY
        let voX = lnB.VectorX
        let voY = lnB.VectorY
        let dot = vtX*voX + vtY*voY
        let det = vtX * voY - vtY * voX
        let tan = det / dot |> abs
        !^ tan < Tangent.``0.25``

    /// A faster Check if two 2D lines are not parallel. Ignoring orientation.
    /// The angle tolerance is 0.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return false on zero-length lines.
    static member inline isNotParallelTo' (lnA:Line2D) (lnB:Line2D) : bool =
        let vtX = lnA.VectorX
        let vtY = lnA.VectorY
        let voX = lnB.VectorX
        let voY = lnB.VectorY
        let dot = vtX*voX + vtY*voY
        let det = vtX * voY - vtY * voX
        let tan = det / dot |> abs
        !^ tan > Tangent.``0.25``

    /// A faster Check if two 2D lines are perpendicular to each other.
    /// The angle tolerance is 89.75 to 90.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return false on zero-length lines.
    /// Same as Line2D.isNormalTo'
    static member inline isPerpendicularTo' (lnA:Line2D) (lnB:Line2D) : bool =
        let vtX = lnA.VectorX
        let vtY = lnA.VectorY
        let voX = lnB.VectorX
        let voY = lnB.VectorY
        let dot = vtX*voX + vtY*voY
        let det = vtX * voY - vtY * voX
        let tan = det / dot |> abs
        !^ tan > Tangent.``89.75``

    /// A faster Check if two 2D lines are perpendicular to each other.
    /// The angle tolerance is 89.75 to 90.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return false on zero-length lines.
    /// Same as Line2D.isPerpendicularTo'
    static member inline isNormalTo' (lnA:Line2D) (lnB:Line2D) : bool =
        Line2D.isPerpendicularTo' lnA lnB






    /// Assumes Line2D to be an infinite ray!
    /// Returns the parameter at which a point is closest to the infinite ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    static member inline rayClosestParameter (p:Pt) (ln:Line2D) =
        ln.RayClosestParameter p

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameter (p:Pt) (ln:Line2D) =
        ln.ClosestParameter p

    /// Assumes Line2D to be an infinite ray!
    /// Returns closest point on ray.
    static member inline rayClosestPoint (p:Pt) (ln:Line2D) =
        ln.RayClosestPoint p

    /// Returns closest point on (finite) line.
    static member inline closestPoint (p:Pt) (ln:Line2D) =
        ln.ClosestPoint p

    /// Assumes Line2D to be an infinite ray!
    /// Returns the square distance from point to ray.
    static member inline sqDistanceRayPoint(p:Pt) (ln:Line2D) =
        ln.SqDistanceRayPoint p

    /// Assumes Line2D to be an infinite ray!
    /// Returns distance from point to ray.
    static member inline distanceRayPoint(p:Pt) (ln:Line2D) =
        ln.DistanceRayPoint p

    /// Returns the square distance from point to finite line.
    static member inline sqDistanceFromPoint(p:Pt) (ln:Line2D) =
        ln.SqDistanceFromPoint p

    /// Returns distance from point to (finite) line.
    static member inline distanceToPt(p:Pt) (ln:Line2D) =
        ln.DistanceToPt p

    /// Get distance from start of line to point projected onto line, may be negative.
    static member inline lengthToPtOnLine (line:Line2D) pt =
        // TODO can be optimized by inlining floats.
        line.UnitTangent *** (pt-line.From)

    /// Extend 2D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extend (distAtStart:float) (distAtEnd:float) (ln:Line2D) =
        ln.Extend(distAtStart, distAtEnd)

    /// Extend 2D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendStart (distAtStart:float) (ln:Line2D) =
        ln.ExtendStart(distAtStart)

    /// Extend 2D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendEnd (distAtEnd:float) (ln:Line2D) =
        ln.ExtendEnd(distAtEnd)


    /// Extend 2D line by relative amount at start and end.
    /// A relative amount of 0.5 extends the line by half its length on each side.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendRel (relAtStart:float) (relAtEnd:float) (ln:Line2D) =
        ln.ExtendRel(relAtStart, relAtEnd)

    /// Extend 2D line by relative amount at start.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendStartRel (relAtStart:float) (ln:Line2D) =
        ln.ExtendStartRel(relAtStart)

    /// Extend 2D line by relative amount at end.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendEndRel (relAtEnd:float) (ln:Line2D) =
        ln.ExtendEndRel(relAtEnd)

    /// Shrink 2D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrink (distAtStart:float) (distAtEnd:float) (ln:Line2D) =
        ln.Shrink(distAtStart, distAtEnd)

    /// Shrink 2D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrinkStart (distAtStart:float) (ln:Line2D) =
        ln.ShrinkStart(distAtStart)

    /// Shrink 2D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrinkEnd (distAtEnd:float) (ln:Line2D) =
        ln.ShrinkEnd(distAtEnd)



    /// Finds point at given distance from line start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline pointAtDistance dist (ln:Line2D) =
        let x = ln.VectorX
        let y = ln.VectorY
        let len = sqrt(x*x + y*y)
        if isTooTiny len then failTooSmall "Line2D.pointAtDistance" ln
        Pt(ln.FromX + x*dist/len,
            ln.FromY + y*dist/len)

    /// Returns new Line2D with given length, going out from start in direction of end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline withLengthFromStart len (ln:Line2D) =
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
    static member inline withLengthToEnd len (ln:Line2D) =
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
    static member inline withLengthFromMid len (ln:Line2D) =
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
    static member offset (distance:float) (ln:Line2D) =
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
        let v = ln.Vector
        let len = v.Length
        let lenMinusGaps = len - gap * float (segments-1)
        let segLen = lenMinusGaps / float segments
        if isTooTiny (segLen) then
            [||]
        else
            let lns = Array.zeroCreate segments
            let vx = v.X
            let vy = v.Y
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
            lns.[segments-1] <- Line2D.setEnd ln.To lns.[segments-1]
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
    static member divideEvery dist (l:Line2D) =
        let len = l.Length
        let div = len / dist
        let floor = Math.Floor div
        if floor = 0.0 then
            let pts = ResizeArray<Pt>(2)
            pts.Add l.From
            pts.Add l.To
            pts
        else
            let step = 1.0 / floor
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
    static member divideInsideEvery dist (l:Line2D) =
        let len = l.Length
        let div = len / dist
        let floor = Math.Floor div
        if floor = 0.0 then
            ResizeArray<Pt>()
        else
            let step = 1.0 / floor
            let count = int floor
            let pts = ResizeArray<Pt>(count)
            for i = 1 to count - 1 do
                pts.Add <| l.EvaluateAt (step * float i)
            if div - floor > 0.001 then
                pts.Add <| l.EvaluateAt (step * floor) // add last div point only if there is a remainder bigger than 0.1%
            pts


    /// Scale the 2D line by a given factor.
    /// Scale center is World Origin 0,0
    static member inline scale (factor:float) (ln:Line2D) : Line2D =
        Line2D(ln.FromX * factor,
               ln.FromY * factor,
               ln.ToX * factor,
               ln.ToY * factor)


    /// <summary>Checks if the two finite 2D lines are touching each other at one of their end points
    /// within the default tolerance of 1e-6. use XLine2D.areEndsTouching to set a custom tolerance</summary>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <returns>A Discriminated Union LineEndsTouching that describes the possible cases of two finite 2D lines touching at their ends:
    /// | NotTouching
    /// | StartA_StartB
    /// | EndA_EndB
    /// | EndA_StartB
    /// | StartA_EndB
    /// | Identical
    /// | IdenticalFlipped
    /// </returns>
    static member isTouchingEndsOf (a:Line2D) (b:Line2D) : XLine2D.XEnds =
        XLine2D.getEndsTouching(a,b)


    /// Project a line onto another line considered infinite in both directions.
    /// Returns the start and end parameters of the projected line on the target line.
    static member projectOntoRayParam (rayToProjectOnto:Line2D) (lineToProject:Line2D) : float*float =
        let osx = rayToProjectOnto.FromX
        let osy = rayToProjectOnto.FromY
        let ovx = rayToProjectOnto.ToX - osx
        let ovy = rayToProjectOnto.ToY - osy
        let lenSq = ovx*ovx + ovy*ovy
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line2D.projectOntoRayParam" rayToProjectOnto
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
    /// Returns None if there is no overlap.
    /// Returns Some (startParam, endParam) if there is an overlap.
    /// The parameters are between 0.0 and 1.0 on the target line
    /// The first parameter is from the start of the line to project.
    /// The second parameter is from the end of the line to project.
    /// So the if the first parameter is bigger than the second, the lines are oriented in opposite direction.
    static member tryProjectOntoLineParam (onToLine:Line2D) (lineToProject:Line2D) : option<float*float> =
        let osx = onToLine.FromX
        let osy = onToLine.FromY
        let ovx = onToLine.ToX - osx
        let ovy = onToLine.ToY - osy
        let lenSq = ovx*ovx + ovy*ovy
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line2D.tryProjectOntoLineParam" onToLine
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
            None
        elif bStartOnA > ``1.0 + 1e-6`` && bEndOnA > ``1.0 + 1e-6`` then
            None
        else
            Some (clampBetweenZeroAndOne bStartOnA, clampBetweenZeroAndOne bEndOnA)

    /// Tries to a line onto another line considered finite.
    /// Returns Some Line2D if there is an overlap.
    /// Returns None if there is no overlap.
    /// Keeps the orientation of the line to project.
    static member tryProjectOntoLine (onToLine:Line2D) (lineToProject:Line2D) : option<Line2D> =
        let osx = onToLine.FromX
        let osy = onToLine.FromY
        let ovx = onToLine.ToX - osx
        let ovy = onToLine.ToY - osy
        let lenSq = ovx*ovx + ovy*ovy
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line2D.projectOntoLineParam" onToLine
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
            None
        elif bStartOnA > ``1.0 + 1e-6`` && bEndOnA > ``1.0 + 1e-6`` then
            None
        else
            let st = clampBetweenZeroAndOne bStartOnA
            let en = clampBetweenZeroAndOne bEndOnA
            Some <| Line2D( osx + ovx * st,
                            osy + ovy * st,
                            osx + ovx * en,
                            osy + ovy * en)




    //   █████        ███                       ████████  ██████████
    //  ░░███        ░░░                       ███░░░░███░░███░░░░███
    //   ░███        ████  ████████    ██████ ░░░    ░███ ░███   ░░███
    //   ░███       ░░███ ░░███░░███  ███░░███   ███████  ░███    ░███
    //   ░███        ░███  ░███ ░███ ░███████   ███░░░░   ░███    ░███
    //   ░███      █ ░███  ░███ ░███ ░███░░░   ███      █ ░███    ███
    //   ███████████ █████ ████ █████░░██████ ░██████████ ██████████
    //  ░░░░░░░░░░░ ░░░░░ ░░░░ ░░░░░  ░░░░░░  ░░░░░░░░░░ ░░░░░░░░░░
    //
    //
    //
    //   █████             █████                                                  █████     ███
    //  ░░███             ░░███                                                  ░░███     ░░░
    //   ░███  ████████   ███████    ██████  ████████   █████   ██████   ██████  ███████   ████   ██████  ████████    █████
    //   ░███ ░░███░░███ ░░░███░    ███░░███░░███░░███ ███░░   ███░░███ ███░░███░░░███░   ░░███  ███░░███░░███░░███  ███░░
    //   ░███  ░███ ░███   ░███    ░███████  ░███ ░░░ ░░█████ ░███████ ░███ ░░░   ░███     ░███ ░███ ░███ ░███ ░███ ░░█████
    //   ░███  ░███ ░███   ░███ ███░███░░░   ░███      ░░░░███░███░░░  ░███  ███  ░███ ███ ░███ ░███ ░███ ░███ ░███  ░░░░███
    //   █████ ████ █████  ░░█████ ░░██████  █████     ██████ ░░██████ ░░██████   ░░█████  █████░░██████  ████ █████ ██████
    //  ░░░░░ ░░░░ ░░░░░    ░░░░░   ░░░░░░  ░░░░░     ░░░░░░   ░░░░░░   ░░░░░░     ░░░░░  ░░░░░  ░░░░░░  ░░░░ ░░░░░ ░░░░░░
    //                                                                                                                            DOS Rebel font by ASCIIDecorator vscode plugin



    /// <summary> A fast test tests if two finite 2D lines truly intersect.
    /// Does not use a default tolerance for parallel or coincident lines. Just checks within line range and not NaN.
    /// Returns false on zero length lines or if parallel lines are touching or overlapping each other.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> Use the XLine2D module for more specialized intersection calculations.
    /// Like getting the kind of Parallel relation between lines, only getting the parameter of one line,
    /// or setting a tolerance for when lines are parallel or coincident.</remarks>
    /// <remarks> This does not check for an explicit angle tolerance, so overlapping almost parallel lines may return true.</remarks>
    static member doIntersect (lnA:Line2D) (lnB:Line2D) : bool =
        XLine2D.doIntersect(lnA, lnB)


    /// <summary> Tests if two finite 2D lines intersect or touch.
    /// Also returns true if parallel lines are touching or overlapping each other.
    /// Also returns true if a zero length lines are at the same location.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> This method uses an angle of 0.25 degrees to classify Lines as parallel.
    /// In wich case it also checks if they overlap or touch.
    /// Use the Euclid's XLine2D module for more specialized intersection calculations.
    /// Like getting the kind of Parallel relation between lines, only getting the parameter of one line,
    /// or setting a tolerance for when lines are parallel or coincident.</remarks>
    static member doIntersectOrOverlap (lnA:Line2D) (lnB:Line2D) : bool =
        match XLine2D.getIntersectionParam(lnA, lnB) with
        | XLine2D.XParam.Intersect _        -> true
        | XLine2D.XParam.Parallel           -> XLine2D.doOverlap(lnA, lnB)
        | XLine2D.XParam.Apart              -> false
        | XLine2D.XParam.TooShortBoth       -> XLine2D.sqDistLnFromLnFrom (lnA, lnB) < 1e-12
        | XLine2D.XParam.TooShortA          -> XLine2D.sqDistLnPt' (lnB, lnA.FromX, lnA.FromY) < 1e-12
        | XLine2D.XParam.TooShortB          -> XLine2D.sqDistLnPt' (lnA, lnB.FromX, lnB.FromY) < 1e-12


    /// <summary> A fast intersection of two finite 2D lines.
    /// Does not use a default tolerance for parallel or coincident lines.
    /// Just checks if both parameters are within line range and not NaN.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <returns> A Pt if the lines intersect truly,
    /// None if apart, lines too short or if parallel lines are touching or overlapping each other.</returns>
    /// <remarks> Use the XLine2D module for more specialized intersection calculations.
    /// Like getting the kind of Parallel relation between lines, only getting the parameter of one line,
    /// or setting a tolerance for when lines are parallel or coincident.</remarks>
    static member tryIntersect(lnA:Line2D) (lnB:Line2D) : Pt option =
        XLine2D.tryIntersectInRanges(lnA, lnB, -1e-6, ``1.0 + 1e-6``, -1e-6, ``1.0 + 1e-6``)


    /// <summary> Tries to get intersection point of two rays (rays are 2D lines extended infinitely).</summary>
    /// <param name="lineA"> First line.</param>
    /// <param name="lineB"> Second line.</param>
    /// <returns> The point at which the two rays intersect or None.</returns>
    /// <remarks> If the lines are parallel or coincident, or if the parameter on Line A is bigger than 1e12 in absolute value,
    /// None is returned.</remarks>
    static member tryIntersectRay(lineA:Line2D) (lineB:Line2D) : Pt option =
        XLine2D.tryIntersectRay(lineA, lineB)



    /// <summary> Intersects two finite 2D Lines.
    /// Also returns a point if parallel lines are touching or overlapping each other.
    /// Also returns a point a zero length lines are at the same location within 1e-6 distance.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> This method uses an angle of 0.25 degrees to classify Lines as parallel.
    /// In wich case it also checks if they overlap or touch.
    /// Use the Euclid's XLine2D module for more specialized intersection calculations.
    /// Like getting the kind of Parallel relation between lines, only getting the parameter of one line,
    /// or setting a tolerance for when lines are parallel or coincident.</remarks>
    /// <returns> An option of Pt
    /// If the lines are coincident and overlapping this point is in the center of their overlap.</returns>
    static member tryIntersectOrOverlap(lnA:Line2D) (lnB:Line2D) : Pt option =
        match XLine2D.getIntersection(lnA, lnB) with
        |XLine2D.XPt.Intersect p -> Some p
        |XLine2D.XPt.Apart -> None
        |XLine2D.XPt.Parallel ->
            let sqDist = XLine2D.sqRayPtDist(lnA.FromX, lnA.FromY, lnA.VectorX, lnA.VectorY, lnB.FromX, lnB.FromY)
            if sqDist < 1e-12 then
                match lnB |> Line2D.tryProjectOntoLineParam lnA  with
                | Some (s,e) -> Some <| lnA.EvaluateAt((s+e)*0.5)
                | None -> None
            else
                None
        | XLine2D.XPt.TooShortBoth -> if XLine2D.sqDistLnFromLnFrom (lnA, lnB) < 1e-12           then Some lnA.From else None
        | XLine2D.XPt.TooShortA    -> if XLine2D.sqDistLnPt' (lnB, lnA.FromX, lnA.FromY) < 1e-12 then Some lnA.From else None
        | XLine2D.XPt.TooShortB    -> if XLine2D.sqDistLnPt' (lnA, lnB.FromX, lnB.FromY) < 1e-12 then Some lnB.From else None



    ///<summary> Finds the closest points between two finite 2D Lines, also works on parallel and overlapping lines.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<remarks> For parallel and overlapping lines the points returned are in the center of their overlap.
    /// If the lines intersect the returned points are exactly the same</remarks>
    static member closestPoints (lnA:Line2D) (lnB:Line2D) : Pt * Pt =
        lnA.ClosestPoints lnB


    ///<summary> Finds the parameters of closest points between two finite 2D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <remarks> For parallel and overlapping lines the parameters returned are in the center of their overlap.
    ///  If the lines intersect the returned parameters are exactly the same</remarks>
    /// <returns> A tuple of two floats, the first is the parameter on lnA, the second on lnB</returns>
    static member closestParameters (lnA:Line2D) (lnB:Line2D) : float * float =
        lnA.ClosestParameters lnB


    /// <summary> Checks if lines are parallel, coincident and overlapping.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <returns> An option of a tuple of two floats.
    /// Returns the tuple containing the start and end parameter of the overlap on lnA if the lines are parallel and overlapping.
    /// Returns None if the lines are not parallel, not overlapping, our just touching at ends,or too short.</returns>
    /// <remarks> If the first parameter in the overlap is smaller than the second the lines are oriented in the same direction.
    /// If the first parameter is greater than the second the lines are oriented in the opposite direction.</remarks>
    static member tryGetOverlap (lnA:Line2D) (lnB:Line2D) : option<float*float> =
        let va = lnA.Vector
        let vb = lnB.Vector
        if XLine2D.isTooShort(va.X, va.Y, 1e-6) || XLine2D.isTooShort(vb.X, vb.Y, 1e-6) then
            None
        else
            let tan = XLine2D.tangent(va.X, va.Y, vb.X, vb.Y)
            if abs tan < Tangent.``0.25`` then
                let sqDist = XLine2D.sqRayPtDist(lnA.FromX, lnA.FromY, lnA.VectorX, lnA.VectorY, lnB.FromX, lnB.FromY)
                if sqDist < 1e-9 then
                    Line2D.tryProjectOntoLineParam lnA lnB
                else
                    None
            else
                None

    /// <summary> Computes the squared distance between two finite 2D lines.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <returns> The squared distance between the two lines. No matter if they are zero length or parallel</returns>
    static member sqDistanceToLine (lnA:Line2D) (lnB:Line2D) : float =
        XLine2D.getSqDistance(lnA, lnB)


    /// <summary> Computes the distance between two finite 2D lines.</summary>
    /// <param name="lnA"> The first line.</param>
    /// <param name="lnB"> The second line.</param>
    /// <returns> The distance between the two lines. No matter if they are zero length or parallel</returns>
    static member distanceToLine (lnA:Line2D) (lnB:Line2D) : float =
        XLine2D.getSqDistance(lnA, lnB) |> sqrt


    /// <summary>Checks if the two finite 2D lines are touching each other at any of end points
    /// within the given tolerance.
    /// This will also return TRUE if the lines are touching on both points.</summary>
    /// <param name="squareTolerance"> The squared tolerance for the distance between the end points.</param>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <remarks> Use XLine2D.getEndsTouching to get more detailed information about which ends are touching.</remarks>
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


    // Instance members marked as Obsolete that have direct replacements:

    // Note: The old IsCoincidentTo(other, distanceTolerance, minCosine) signature was changed
    // to use Tangent tolerance instead of Cosine tolerance in v0.30
    // The new signature is: IsCoincidentTo(other, distanceTolerance, minTangent)
    // This change makes the angle tolerance more intuitive for near-parallel line detection



    [<Obsolete("Use ln.MatchesOrientation instead")>]
    member inline ln.MatchesOrientation180 (otherLn:Line2D) =  ln.MatchesOrientation(otherLn)

    [<Obsolete("Use ln.MatchesOrientation instead")>]
    member inline ln.MatchesOrientation180 (v:Vc) = ln.MatchesOrientation(v)

    [<Obsolete("Use ln.MatchesOrientation instead")>]
    member inline ln.MatchesOrientation180 (v:UnitVc) =  ln.MatchesOrientation(v)

    [<Obsolete("Use ln.MatchesOrientation45 instead")>]
    member inline ln.MatchesOrientation90 (l:Line2D) = ln.MatchesOrientation45(l)



    [<Obsolete("Use this.RayClosestParameter instead. Obsolete since 0.20.0")>]
    member ln.ClosestParameterInfinite (p:Pt) =
        ln.RayClosestParameter(p)

    [<Obsolete("Use this.RayClosestPoint instead. Obsolete since 0.20.0")>]
    member ln.ClosestPointInfinite (p:Pt) =
        ln.RayClosestPoint(p)

    [<Obsolete("Use this.SqDistanceRayPoint instead. Obsolete since 0.20.0")>]
    member ln.DistanceSqFromPointInfinite(p:Pt) =
        ln.SqDistanceRayPoint(p)

    [<Obsolete("Use this.DistanceRayPoint instead. Obsolete since 0.20.0")>]
    member inline ln.DistanceToPtInfinite(p:Pt) =
        ln.DistanceRayPoint(p)

    [<Obsolete("Use this.SqDistanceFromPoint instead. Obsolete since 0.20.0")>]
    member ln.DistanceSqFromPoint(p:Pt) =
        XLine2D.sqDistLnPt'(ln, p.X, p.Y)

    // Static members marked as Obsolete that have direct replacements:



    [<Obsolete("Use matchesOrientation instead")>]
    static member inline matchesOrientation180 (l:Line2D) (ln:Line2D) = l.MatchesOrientation ln

    [<Obsolete("Use matchesOrientation45 instead")>]
    static member inline matchesOrientation90 (l:Line2D) (ln:Line2D) = l.MatchesOrientation45 ln



    [<Obsolete("Use Line2D.isCoincidentTo instead. Obsolete since 0.20.0")>]
    static member inline areCoincident (a:Line2D) (b:Line2D) =
        a.IsCoincidentTo(b)

    [<Obsolete("Use Line2D.isParallelTo instead. Obsolete since 0.20.0")>]
    static member inline areParallel (l:Line2D) (ln:Line2D) =
        l.IsParallelTo ln

    [<Obsolete("Use Line2D.isParallelAndOrientedTo instead. Obsolete since 0.20.0")>]
    static member inline areParallelAndMatchOrientation (l:Line2D) (ln:Line2D) =
        l.IsParallelAndOrientedTo ln

    [<Obsolete("Use Line2D.isPerpendicularTo instead. Obsolete since 0.20.0")>]
    static member inline arePerpendicular(l:Line2D) (ln:Line2D) =
        l.IsPerpendicularTo ln

    [<Obsolete("Use Line2D.rayClosestParameter instead. Obsolete since 0.20.0")>]
    static member inline closestParameterInfinite (p:Pt) (ln:Line2D) =
        ln.RayClosestParameter p

    [<Obsolete("Use Line2D.rayClosestPoint instead. Obsolete since 0.20.0")>]
    static member inline closestPointInfinite (p:Pt) (ln:Line2D) =
        ln.RayClosestPoint p

    [<Obsolete("Use Line2D.sqDistanceRayPoint instead. Obsolete since 0.20.0")>]
    static member inline distanceSqFromPointInfinite(p:Pt) (ln:Line2D) =
        ln.SqDistanceRayPoint p

    [<Obsolete("Use Line2D.distanceRayPoint instead. Obsolete since 0.20.0")>]
    static member inline distanceToPtInfinite(p:Pt) (ln:Line2D) =
        ln.DistanceRayPoint p

    [<Obsolete("Use Line2D.sqDistanceFromPoint instead. Obsolete since 0.20.0")>]
    static member inline distanceSqFromPoint(p:Pt) (ln:Line2D) =
        ln.SqDistanceFromPoint p

    [<Obsolete("Use Line2D.distanceToLine instead. Obsolete since 0.20.0")>]
    static member distanceBetweenLines(lnA:Line2D, lnB:Line2D) =
        Line2D.distanceToLine lnA lnB

    [<Obsolete("Use Line2D.projectOntoRay instead. Obsolete since 0.20.0")>]
    static member projectOn(onToLine:Line2D) (lineToProject:Line2D) =
        Line2D.projectOntoRay onToLine lineToProject



    // Obsolete members that don't have a direct replacement:

    [<Obsolete("Use XLine2D.getIntersectionParam instead. Obsolete since 0.20.0",error=true)>]
    static member intersectionParamInfinite(_lnA:Line2D, _lnB:Line2D)  =
        failObsoleteV30 "Line2D.intersectionParamInfinite" "XLine2D.getIntersectionParam"

    [<Obsolete("Use XLine2D.getIntersection instead. Obsolete since 0.20.0",error=true)>]
    static member intersectionInfinite(_lnA:Line2D, _lnB:Line2D) =
        failObsoleteV30 "Line2D.intersectionInfinite" "XLine2D.getIntersection"

    [<Obsolete("Use XLine2D.tryIntersectRay instead. Obsolete since 0.20.0",error=true)>]
    static member intersectionPointInfinite(_lnA:Line2D, _lnB:Line2D) =
        failObsoleteV30 "Line2D.intersectionPointInfinite" "XLine2D.tryIntersectRay"

    [<Obsolete("Use XLine2D.getClosestParameters instead. Obsolete since 0.20.0",error=true)>]
    static member intersectionParam(_lnA:Line2D, _lnB:Line2D) =
        failObsoleteV30 "Line2D.intersectionParam" "XLine2D.getClosestParameters"

    [<Obsolete("Use XLine2D.getClosestPoints instead. Obsolete since 0.20.0",error=true)>]
    static member intersection(_lnA:Line2D, _lnB:Line2D) =
        failObsoleteV30 "Line2D.intersection" "XLine2D.getClosestPoints"

    [<Obsolete("Use Line2D.tryIntersect or Line2D.tryIntersectOrOverlap instead. Obsolete since 0.20.0",error=true)>]
    static member intersectionPoint(_lnA:Line2D, _lnB:Line2D) =
        failObsoleteV30 "Line2D.intersectionPoint" "Line2D.tryIntersect or Line2D.tryIntersectOrOverlap"





    [<Obsolete("Use isTouchingEndOf or  XLine2D.getEndsTouching instead. Obsolete since 0.20.0",error=true)>]
    static member areTouchingAny(tol:float)( a:Line2D)( b:Line2D) =
        XLine2D.getEndsTouching(a,b,tol)

    [<Obsolete("Use isTouchingEndOf or XLine2D.getEndsTouching instead. Obsolete since 0.20.0",error=true)>]
    static member areTouchingEither(tol:float)( a:Line2D)( b:Line2D) =
        XLine2D.getEndsTouching(a,b,tol)

