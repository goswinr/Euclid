namespace Euclid

namespace Euclid

open Euclid.UtilEuclid
open Euclid.LineIntersectionTypes
open UtilEuclid

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Line3D.
[<AutoOpen>]
module AutoOpenLine3D =

  type Line3D with


    /// Checks if line is parallel to the world X axis. Ignoring orientation.
    /// The absolute deviation tolerance along Y and Z axis is 1e-9.
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsXAligned =
        let x = abs (ln.ToX-ln.FromX)
        let y = abs (ln.ToY-ln.FromY)
        let z = abs (ln.ToZ-ln.FromZ)
        if isTooSmall (x+y+z) then EuclidException.Raisef "Euclid.Line3D.IsXAligned cannot not check very short line. (tolerance 1e-6) %O" ln
        else y < 1e-9 && z < 1e-9

    /// Checks if 3D line is parallel to the world Y axis. Ignoring orientation.
    /// The absolute deviation tolerance along X and Z axis is 1e-9.
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsYAligned =
        let x = abs (ln.ToX-ln.FromX)
        let y = abs (ln.ToY-ln.FromY)
        let z = abs (ln.ToZ-ln.FromZ)
        if isTooSmall (x+y+z) then EuclidException.Raisef "Euclid.Line3D.IsYAligned cannot not check very short line. (tolerance 1e-6) %O" ln
        else x < 1e-9 && z < 1e-9

    /// Checks if 3D line is parallel to the world Z axis. Ignoring orientation.
    /// The absolute deviation tolerance along X and Y axis is 1e-9.
    /// Fails on lines shorter than 1e-6.
    /// Same as ln.IsVertical
    member inline ln.IsZAligned =
        let x = abs (ln.ToX-ln.FromX)
        let y = abs (ln.ToY-ln.FromY)
        let z = abs (ln.ToZ-ln.FromZ)
        if isTooSmall (x+y+z) then EuclidException.Raisef "Euclid.Line3D.IsZAligned cannot not check very short line. (tolerance 1e-6) %O" ln
        else x < 1e-9 && y < 1e-9

    /// Checks if 3D line is parallel to the world Z axis. Ignoring orientation.
    /// The absolute deviation tolerance along X and Y axis is 1e-9.
    /// Fails on lines shorter than 1e-6.
    /// Same as ln.IsZAligned
    member inline ln.IsVertical =
        let x = abs (ln.ToX-ln.FromX)
        let y = abs (ln.ToY-ln.FromY)
        let z = abs (ln.ToZ-ln.FromZ)
        if isTooSmall (x+y+z) then EuclidException.Raisef "Euclid.Line3D.IsVertical cannot not check very short line. (tolerance 1e-6) %O" ln
        else x < 1e-9 && y < 1e-9

    /// Checks if 3D line is horizontal.
    /// The absolute deviation tolerance along Z axis is 1e-9.
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsHorizontal =
        let x = abs (ln.ToX-ln.FromX)
        let y = abs (ln.ToY-ln.FromY)
        let z = abs (ln.ToZ-ln.FromZ)
        if isTooSmall (x+y+z) then EuclidException.Raisef "Euclid.Line3D.IsHorizontal cannot not check very short line. (tolerance 1e-6) %O" ln
        else z < 1e-9

    /// Check if the 3D line has exactly the same starting and ending point.
    member inline ln.IsZeroLength =
        ln.ToX = ln.FromX &&
        ln.ToY = ln.FromY &&
        ln.ToZ = ln.FromZ

    /// Check if 3D line is shorter than tolerance.
    ///  Or contains a NaN value
    member inline ln.IsTiny tol =
        ln.Length < tol

    /// Check if 3D line is shorter than the squared tolerance.
    ///  Or contains a NaN value
    member inline ln.IsTinySq tol =
        not (ln.LengthSq > tol)

    /// Evaluate 3D line at a given parameter.
    /// Parameters 0.0 to 1.0 are on the line.
    member inline ln.EvaluateAt (p:float) =
        Pnt(ln.FromX + (ln.ToX-ln.FromX)*p,
            ln.FromY + (ln.ToY-ln.FromY)*p,
            ln.FromZ + (ln.ToZ-ln.FromZ)*p)

    /// Evaluate line at a given parameters (parameters 0.0 to 1.0 are on the line),
    /// Return a new line from evaluated points.
    member inline ln.SubLine (start:float, ende:float) =
        let fromX = ln.FromX
        let fromY = ln.FromY
        let fromZ = ln.FromZ
        let x = ln.ToX - fromX
        let y = ln.ToY - fromY
        let z = ln.ToZ - fromZ
        Line3D( fromX + x * start,
                fromY + y * start,
                fromZ + z * start,
                fromX + x * ende ,
                fromY + y * ende ,
                fromZ + z * ende )

    /// Returns the length of the line segment from the start point to the given parameter.
    /// This length is negative if the parameter is negative.
    member inline ln.LengthTillParam (p:float) =
        let x = (ln.ToX - ln.FromX)*p
        let y = (ln.ToY - ln.FromY)*p
        let z = (ln.ToZ - ln.FromZ)*p
        let l = sqrt(x*x + y*y + z*z)
        if p> 0.0 then l else -l

    /// Returns the length of the line segment from the given parameter till the line End.
    /// This length is negative if the parameter is bigger than 1.0.
    member inline ln.LengthFromParam (t:float) =
        let p = 1.0-t
        let x = (ln.ToX - ln.FromX)*p
        let y = (ln.ToY - ln.FromY)*p
        let z = (ln.ToZ - ln.FromZ)*p
        let l = sqrt(x*x + y*y + z*z)
        if p> 0.0 then l else -l

    /// Returns the midpoint of the 3D line,
    member inline ln.Mid =
        Pnt((ln.ToX + ln.FromX)*0.5,
            (ln.ToY + ln.FromY)*0.5,
            (ln.ToZ + ln.FromZ)*0.5)

    /// Returns the 3D line reversed.
    member inline ln.Reversed =
        Line3D(ln.ToX, ln.ToY, ln.ToZ, ln.FromX, ln.FromY, ln.FromZ)


    /// Returns a Line3D from point at Parameter a to point at Parameter b.
    member inline ln.Segment(a, b) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        Line3D( ln.FromX + x*a,
                ln.FromY + y*a,
                ln.FromZ + z*a,
                ln.FromX + x*b,
                ln.FromY + y*b,
                ln.FromZ + z*b)

    /// Extend 3D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.Extend (distAtStart:float, distAtEnd:float) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.Extend %O to short for finding point at a distance." ln
        Line3D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.FromZ - z*distAtStart/l,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l,
                ln.ToZ   + z*distAtEnd/l)

    /// Extend 3D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendStart (distAtStart:float) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.ExtendStart %O to short for finding point at a distance." ln
        Line3D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.FromZ - z*distAtStart/l,
                ln.ToX,
                ln.ToY,
                ln.ToZ)

    /// Extend 3D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendEnd (distAtEnd:float) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.ExtendEnd %O to short for finding point at a distance." ln
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l,
                ln.ToZ   + z*distAtEnd/l)

    /// Extend 3D line by relative amount at start and end.
    /// A relative amount of 0.5 extends the line by half its length on each side.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendRel (relAtStart:float, relAtEnd:float) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.ExtendRel %O to short for finding point at a distance." ln
        Line3D( ln.FromX - x*relAtStart,
                ln.FromY - y*relAtStart,
                ln.FromZ - z*relAtStart,
                ln.ToX   + x*relAtEnd,
                ln.ToY   + y*relAtEnd,
                ln.ToZ   + z*relAtEnd)

    /// Extend 3D line by relative amount at start.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendStartRel (relAtStart:float) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.ExtendStartRel %O to short for finding point at a distance." ln
        Line3D( ln.FromX - x*relAtStart,
                ln.FromY - y*relAtStart,
                ln.FromZ - z*relAtStart,
                ln.ToX,
                ln.ToY,
                ln.ToZ)

    /// Extend 3D line by relative amount at end.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendEndRel (relAtEnd:float) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.ExtendEndRel %O to short for finding point at a distance." ln
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ,
                ln.ToX   + x*relAtEnd,
                ln.ToY   + y*relAtEnd,
                ln.ToZ   + z*relAtEnd)

    /// Shrink 3D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.Shrink (distAtStart:float, distAtEnd:float) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.Shrink %O to short for finding point at a distance." ln
        Line3D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.FromZ + z*distAtStart/l,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l,
                ln.ToZ   - z*distAtEnd/l)

    /// Shrink 3D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ShrinkStart (distAtStart:float) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.ShrinkStart %O to short for finding point at a distance." ln
        Line3D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.FromZ + z*distAtStart/l,
                ln.ToX,
                ln.ToY,
                ln.ToZ)

    /// Shrink 3D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ShrinkEnd (distAtEnd:float) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.ShrinkEnd %O to short for finding point at a distance." ln
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l,
                ln.ToZ   - z*distAtEnd/l)

    /// Returns a Line3D moved by a vector.
    member inline ln.Move (v:Vec) =
        Line3D( ln.FromX + v.X,
                ln.FromY + v.Y,
                ln.FromZ + v.Z,
                ln.ToX   + v.X,
                ln.ToY   + v.Y,
                ln.ToZ   + v.Z)

    /// Returns a Line3D moved by a given distance in X direction.
    member inline ln.MoveX (distance:float) =
        Line3D( ln.FromX + distance,
                ln.FromY,
                ln.FromZ,
                ln.ToX  + distance,
                ln.ToY,
                ln.ToZ)
    /// Returns a Line3D moved by a given distance in Y direction.
    member inline ln.MoveY (distance:float) =
        Line3D( ln.FromX,
                ln.FromY + distance,
                ln.FromZ,
                ln.ToX,
                ln.ToY + distance,
                ln.ToZ)

    /// Returns a Line3D moved by a given distance in Z direction.
    member inline ln.MoveZ (distance:float) =
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ + distance,
                ln.ToX,
                ln.ToY,
                ln.ToZ + distance)

    /// Assumes Line3D to be infinite.
    /// Returns the parameter at which a point is closest to the infinite line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Fails on curves shorter than 1e-9 units. (ln.ClosestParameter does not)
    member inline ln.ClosestParameterInfinite (p:Pnt) =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let lenSq = x*x + y*y + z*z
        if isTooSmallSq(lenSq) then
            EuclidException.Raisef "Euclid.Line3D.ClosestParameterInfinite failed on very short line %O for point %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let w = ln.FromZ-p.Z
        let dot = x*u + y*v + z*w
        dot / lenSq

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    /// Does not fails on very short curves.
    member inline ln.ClosestParameter (p:Pnt) =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let w = ln.FromZ-p.Z
        let dot = x*u + y*v + z*w
        let lenSq = x*x + y*y + z*z
        if isTooSmallSq(lenSq) then
            if dot < 0.0 then 0.0 else 1.0
        else
            dot / lenSq |> UtilEuclid.clampBetweenZeroAndOne



    /// Assumes Line3D to be infinite.
    /// Returns closest point on infinite line.
    /// Fails on curves shorter than 1e-9 units. (ln.ClosestPoint does not.)
    member inline ln.ClosestPointInfinite (p:Pnt) =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let lenSq = x*x + y*y + z*z
        if isTooSmallSq(lenSq) then
            EuclidException.Raisef "Euclid.Line3D.ClosestPointInfinite failed on very short line %O for point %O" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let w = ln.FromZ-p.Z
        let dot = x*u + y*v + z*w
        let t = dot/lenSq
        let x' = ln.FromX - x*t
        let y' = ln.FromY - y*t
        let z' = ln.FromZ - z*t
        Pnt(x', y', z')

    /// Returns closest point on (finite) line.
    /// Does not fails on very short curves.
    member inline ln.ClosestPoint (p:Pnt) =
        ln.EvaluateAt(ln.ClosestParameter(p))

    /// Assumes Line3D to be infinite.
    /// Returns square distance from point to infinite line.
    /// Fails on curves shorter than 1e-6 units. (ln.DistanceSqToPnt does not.)
    member ln.DistanceSqToPntInfinite(p:Pnt) =
        let lnFromX = ln.FromX
        let lnFromY = ln.FromY
        let lnFromZ = ln.FromZ
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = lnFromX - ln.ToX
        let y = lnFromY - ln.ToY
        let z = lnFromZ - ln.ToZ
        let lenSq = x*x + y*y + z*z
        if isTooSmallSq lenSq  then // corresponds to a line Length of 1e-6
            EuclidException.Raisef "Euclid.Line3D.DistanceSqToPntInfiniteSq failed on very short line %O for point %O" ln p
        let u = lnFromX - p.X
        let v = lnFromY - p.Y
        let w = lnFromZ - p.Z
        let dot = x*u + y*v + z*w
        let t = dot/lenSq
        let x' = lnFromX - x*t
        let y' = lnFromY - y*t
        let z' = lnFromZ - z*t
        let u' = x' - p.X
        let v' = y' - p.Y
        let w' = z' - p.Z
        u'*u' + v'*v' + w'*w'

    /// Assumes Line3D to be infinite.
    /// Returns distance from point to infinite line.
    /// Fails on curves shorter than 1e-9 units. (ln.DistanceToPnt does not.)
    member inline ln.DistanceToPntInfinite(p:Pnt) =
        ln.DistanceSqToPntInfinite(p) |> sqrt

    /// Returns square distance from point to finite line.
    member inline ln.DistanceSqToPnt(p:Pnt) =
        p
        |> ln.ClosestParameter
        |> ln.EvaluateAt
        |> Pnt.distanceSq p

    /// Returns distance from point to (finite) line.
    member inline ln.DistanceToPnt(p:Pnt) =
        ln.DistanceSqToPnt(p) |> sqrt

    /// Checks if the angle between the two 3D lines is less than 180 degrees.
    /// Calculates the dot product of two 3D lines.
    /// Then checks if it is bigger than 1e-12.
    member inline ln.MatchesOrientation180 (otherLn:Line3D) =
        let dot = (otherLn.ToX-otherLn.FromX)*(ln.ToX-ln.FromX) + (otherLn.ToY-otherLn.FromY)*(ln.ToY-ln.FromY) + (otherLn.ToZ-otherLn.FromZ)*(ln.ToZ-ln.FromZ)
        dot > 1e-12

    /// Checks if the angle between the a 3D line and a 3D vector is less than 180 degrees.
    /// Calculates the dot product of both.
    /// Then checks if it is bigger than 1e-12.
    member inline ln.MatchesOrientation180 (v:Vec) =
        if isTooTinySq(v.LengthSq) then EuclidException.Raisef "Euclid.Line3D.MatchesOrientation180: Vec 'v' is too short: %s. 'ln':%s " v.AsString ln.AsString
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY) + v.Z*(ln.ToZ-ln.FromZ)
        dot > 1e-12

    /// Checks if the angle between the a 3D line and a 3D unit-vector is less than 180 degrees.
    /// Calculates the dot product of both.
    /// Then checks if it is bigger than 1e-12.
    member inline ln.MatchesOrientation180 (v:UnitVec) =
        let dot = v.X*(ln.ToX-ln.FromX) + v.Y*(ln.ToY-ln.FromY) + v.Z*(ln.ToZ-ln.FromZ)
        dot > 1e-12

    /// Checks if the angle between the two 3D lines is less than 90 degrees.
    /// Calculates the dot product of the unit-vectors of the two 3D lines.
    /// Then checks if it is bigger than 0.707107 (cosine of 90 degrees).
    member inline ln.MatchesOrientation90 (otherLn:Line3D) =
        let dot = ln.UnitTangent *** otherLn.UnitTangent
        dot > 0.707107

    /// Checks if two 3D lines are parallel.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See Euclid.Cosine module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:Line3D, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Line3D.IsParallelTo: Line3D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Line3D.IsParallelTo: Line3D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
        let au = a * (1.0 / sqrt sa)
        let bu = b * (1.0 / sqrt sb)
        abs(bu *** au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

    /// Checks if a 3D lines is parallel to a 3D vector.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See Euclid.Cosine module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let b = other
        let sa = a.LengthSq
        if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Line2D.IsParallelTo: Line2D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Line2D.IsParallelTo: Vec 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
        let au = a * (1.0 / sqrt sa)
        let bu = b * (1.0 / sqrt sb)
        abs(bu *** au) > float minCosine

    /// Checks if a 3D lines is parallel to a 3D unit-vector.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See Euclid.Cosine module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let sa = a.LengthSq
        if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Line3D.IsParallelTo: Line3D 'ln' is too short: %s. 'other':%s " a.AsString other.AsString
        let au = a * (1.0 / sqrt sa)
        abs(other *** au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:


    /// Checks if two 3D lines are parallel.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See Euclid.Cosine module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:Line3D, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Line3D.IsParallelAndOrientedTo: Line3D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Line3D.IsParallelAndOrientedTo: Line3D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
        let au = a * (1.0 / sqrt sa)
        let bu = b * (1.0 / sqrt sb)
        bu *** au > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:

    /// Checks if a 3D lines is parallel to a 3D vector.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See Euclid.Cosine module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:Vec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let b = other
        let sa = a.LengthSq
        if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Line2D.IsParallelAndOrientedTo: Line2D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Line2D.IsParallelAndOrientedTo: Vec 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
        let au = a * (1.0 / sqrt sa)
        let bu = b * (1.0 / sqrt sb)
        bu *** au > float minCosine

    /// Checks if a 3D lines is parallel to a 3D unit-vector.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See Euclid.Cosine module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:UnitVec, [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let sa = a.LengthSq
        if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Line3D.IsParallelAndOrientedTo: Line3D 'ln' is too short: %s. 'other':%s " a.AsString other.AsString
        let au = a * (1.0 / sqrt sa)
        other *** au > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:


    /// Checks if two 3D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// The default cosine is 0.0043633 ( = 89.75 deg)
    /// See Euclid.Cosine module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:Line3D, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Line3D.IsPerpendicularTo: Line3D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Line3D.IsPerpendicularTo: Line3D 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
        let au = a * (1.0 / sqrt sa)
        let bu = b * (1.0 / sqrt sb)
        let d = bu *** au
        float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees


    /// Checks if a 3D lines is perpendicular to a 3D vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// The default cosine is 0.0043633 ( = 89.75 deg)
    /// See Euclid.Cosine module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:Vec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let b = other
        let sa = a.LengthSq
        if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Line2D.IsPerpendicularTo: Line2D 'ln' is too short: %s. 'other':%s " a.AsString b.AsString
        let sb = b.LengthSq
        if isTooTinySq(sb) then EuclidException.Raisef "Euclid.Line2D.IsPerpendicularTo: Vec 'other' is too short: %s. 'ln':%s " b.AsString a.AsString
        let au = a * (1.0 / sqrt sa)
        let bu = b * (1.0 / sqrt sb)
        let d = bu *** au
        float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees

    /// Checks if a 3D lines is perpendicular to a 3D unit-vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// The default cosine is 0.0043633 ( = 89.75 deg)
    /// See Euclid.Cosine module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:UnitVec, [<OPT;DEF(Cosine.``89.75``)>] maxCosine:float<Cosine.cosine> ) =
        let a = ln.Vector
        let sa = a.LengthSq
        if isTooTinySq(sa) then EuclidException.Raisef "Euclid.Line3D.IsPerpendicularTo: Line3D 'ln' is too short: %s. 'other':%s " a.AsString other.AsString
        let au = a * (1.0 / sqrt sa)
        let d = other *** au
        float -maxCosine < d && d  < float maxCosine // = cosine of 98.75 and 90.25 degrees


    /// Checks if two 3D lines are coincident within the distance tolerance. 1e-6 by default.
    /// This means that lines are parallel within the angle tolerance.
    /// and the distance of second start to the first line is less than the distance tolerance.
    /// Also returns false on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// The default angle tolerance is 0.25 degrees.
    /// This tolerance can be customized by an optional minium cosine value.
    /// See Euclid.Cosine module.
    member inline ln.IsCoincidentTo (other:Line3D,
                                    [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                    [<OPT;DEF(Cosine.``0.25``)>] minCosine:float<Cosine.cosine>) =
        let a = ln.Vector
        let b = other.Vector
        let sa = a.LengthSq
        if isTooTinySq(sa) then
            false
        else
            let sb = b.LengthSq
            if isTooTinySq(sb) then
                false
            else
                let au = a * (1.0 / sqrt sa)
                let bu = b * (1.0 / sqrt sb)
                abs(bu *** au) > float minCosine // 0.999990480720734 = cosine of 0.25 degrees:
                &&
                let pX = other.FromX
                let pY = other.FromY
                let pZ = other.FromZ
                //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
                let x = a.X
                let y = a.Y
                let z = a.Z
                let u = ln.FromX - pX
                let v = ln.FromY - pY
                let w = ln.FromZ - pZ
                let dot = x*u + y*v + z*w
                let t = dot/sa
                let x' = ln.FromX - x*t
                let y' = ln.FromY - y*t
                let z' = ln.FromZ - z*t
                let u' = x' - pX
                let v' = y' - pY
                let w' = z' - pZ
                u'*u' + v'*v' + w'*w' < distanceTolerance * distanceTolerance

    /// Applies or multiplies a 4x4 transformation matrix to a 3D line.
    member inline l.Transform (m:Matrix) = Line3D(l.From *** m, l.To *** m)

    /// Multiplies (or applies) a RigidMatrix to a 3D line .
    member inline l.TransformRigid (m:RigidMatrix) = Line3D(l.From *** m, l.To *** m)




    //-------------------------------------------------------------------
    //------------------------static members-----------------------------
    //-------------------------------------------------------------------


    /// Checks if two 3D Lines are equal within tolerance.
    /// Identical Lines in opposite directions are not considered equal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member inline equals (tol:float) (a:Line3D) (b:Line3D) =
        abs (a.FromX - b.FromX) <= tol &&
        abs (a.FromY - b.FromY) <= tol &&
        abs (a.FromZ - b.FromZ) <= tol &&
        abs (a.ToX   - b.ToX  ) <= tol &&
        abs (a.ToY   - b.ToY  ) <= tol &&
        abs (a.ToZ   - b.ToZ  ) <= tol

    /// Check if two 3D Lines are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two rectangles are not exactly equal.
    static member notEquals (tol:float) (a:Line3D) (b:Line3D) =
        abs (a.FromX - b.FromX) > tol ||
        abs (a.FromY - b.FromY) > tol ||
        abs (a.FromZ - b.FromZ) > tol ||
        abs (a.ToX   - b.ToX  ) > tol ||
        abs (a.ToY   - b.ToY  ) > tol ||
        abs (a.ToZ   - b.ToZ  ) > tol

    /// Checks if two 3D lines are coincident within tolerance.
    /// This means that lines are parallel within 0.25 degrees.
    /// and the distance of second start point to the first line is less than 1e-6.
    static member inline areCoincident (a:Line3D) (b:Line3D) =
        a.IsCoincidentTo(b)

    /// Creates a 2D line from 3D line. Ignoring Z value.
    static member inline toLine2D (ln:Line3D) =
        Line2D(ln.FromX, ln.FromY, ln.ToX, ln.ToY)

    /// Creates a 3D line from 2D line. Setting Z to 0.0
    static member inline createFromLine2D (ln:Line2D) =
        Line3D(ln.FromX, ln.FromY, 0., ln.ToX, ln.ToY, 0.)

    /// Creates a 3D line from 2D line. Setting Z to given value.
    static member inline createFromLine2DwithZ (zLevel) (ln:Line2D) =
        Line3D(ln.FromX, ln.FromY, zLevel, ln.ToX, ln.ToY, zLevel)

    /// Creates a line starting at World Origin and going to along the given vector.
    static member inline createFromVec (v:Vec) =
        Line3D(0., 0., 0., v.X, v.Y, v.Z)

    /// Creates a line starting at given point and going to along the given vector.
    static member inline createFromPntAndVec (p:Pnt, v:Vec) =
        Line3D(p.X, p.Y, p.Z, p.X+v.X, p.Y+v.Y, p.Z+v.Z)

    /// Returns the Start point of the line. Same as Line3D.from.
    static member inline start (l:Line3D) =
        l.From

    /// Returns the Start point of the line. Same as Line3D.start.
    static member inline from (l:Line3D) =
        l.From

    /// Returns the Start point's X coordinate of the line.
    static member inline fromX (l:Line3D) =
        l.FromX

    /// Returns the Start point's Y coordinate of the line.
    static member inline fromY (l:Line3D) =
        l.FromY

    /// Returns the Start point's Z coordinate of the line.
    static member inline fromZ (l:Line3D) =
        l.FromZ

    /// Returns the End point of the line. Same as Line3D.to'
    static member inline ende (l:Line3D) =
        l.To

    /// Returns the End point of the line. Same as Line3D.ende.
    static member inline to' (l:Line3D) =
        l.To

    /// Returns the End point's X coordinate of the line.
    static member inline toX (l:Line3D) =
        l.ToX

    /// Returns the End point's Y coordinate of the line.
    static member inline toY (l:Line3D) =
        l.ToY

    /// Returns the End point's Z coordinate of the line.
    static member inline toZ (l:Line3D) =
        l.ToZ

    /// Set Line3D start point, returns a new line.
    static member inline setStart (pt:Pnt) (ln:Line3D) =
        Line3D(pt.X, pt.Y, pt.Z, ln.ToX, ln.ToY, ln.ToZ)

    /// Set Line3D end point, returns a new line.
    static member inline setEnd (pt:Pnt) (ln:Line3D) =
        Line3D(ln.FromX, ln.FromY, ln.FromZ, pt.X, pt.Y, pt.Z)

    /// Same as Line3D.vector or Line3D.tangent.
    /// The returned vector has the same length as the Line3D.
    static member inline direction (ln:Line3D) =
        Vec(ln.ToX-ln.FromX, ln.ToY-ln.FromY, ln.ToZ-ln.FromZ)

    /// Same as Line3D.tangent or Line3D.direction.
    /// The returned vector has the same length as the Line3D.
    static member inline vector (ln:Line3D) =
        Vec(ln.ToX-ln.FromX, ln.ToY-ln.FromY, ln.ToZ-ln.FromZ)

    /// Same as Line3D.vector or Line3D.direction.
    /// The returned vector has the same length as the Line3D.
    static member inline tangent (ln:Line3D) =
        Vec(ln.ToX-ln.FromX, ln.ToY-ln.FromY, ln.ToZ-ln.FromZ)

    /// Returns a unit-vector of the line Direction.
    static member inline unitTangent (ln:Line3D) =
        ln.UnitTangent

    /// Returns the length of the line.
    static member inline length (l:Line3D) =
        l.Length

    /// Returns the square length of the line.
    static member inline lengthSq (l:Line3D) =
        l.LengthSq

    /// Check if the line has same starting and ending point.
    static member inline isZeroLength (l:Line3D) =
        l.IsZeroLength

    /// Check if line is shorter than tolerance.
    /// Also checks if any component is a NaN.
    static member inline isTiny tol (l:Line3D) =
        l.Length < tol

    /// Check if the lines square length is shorter than squared tolerance.
    /// Also checks if any component is a NaN.
    static member inline isTinySq tol (l:Line3D) =
        not (l.LengthSq > tol)

    /// Checks if 3D line is parallel to the world X axis. Ignoring orientation.
    /// Tolerance is 1e-6.
    /// Fails on lines shorter than 1e-6.
    static member inline isXAligned (l:Line3D) =
        l.IsXAligned

    /// Checks if 3D line is parallel to the world Y axis. Ignoring orientation.
    /// Tolerance is 1e-6.
    /// Fails on lines shorter than 1e-6.
    static member inline isYAligned (l:Line3D) =
        l.IsYAligned

    /// Checks if 3D line is parallel to the world Z axis. Ignoring orientation.
    /// Tolerance is 1e-6.
    /// Fails on lines shorter than 1e-6.
    /// Same as ln.IsVertical
    static member inline isZAligned (l:Line3D) =
        l.IsZAligned

    /// Checks if 3D line is parallel to the world Z axis. Ignoring orientation.
    /// Tolerance is 1e-6.
    /// Fails on lines shorter than 1e-6.
    /// Same as ln.IsZAligned
    static member inline isVertical (l:Line3D) =
        l.IsVertical

    /// Checks if 3D line is horizontal (Z component is almost zero).
    /// Tolerance is 1e-6.
    /// Fails on lines shorter than 1e-6.
    static member inline isHorizontal (l:Line3D) =
        l.IsHorizontal

    /// Evaluate line at a given parameter (parameters 0.0 to 1.0 are on the line)
    static member inline evaluateAt t (ln:Line3D) =
        ln.EvaluateAt t

    /// Get point at center of line.
    static member inline mid (ln:Line3D) =
        ln.Mid

    /// Reverse or flip the 3D line (same as Line3D.flip)
    static member inline reverse (ln:Line3D) =
        ln.Reversed

    /// Reverse or flip the 3D line (same as Line3D.reverse)
    static member inline flip (ln:Line3D) =
        ln.Reversed

    /// Returns new 3D line from point at Parameter a to point at Parameter b.
    static member inline segment a b (ln:Line3D) =
        ln.Segment (a, b)

    /// Move a 3D line by a vector. (same as Line3D.move)
    static member inline translate (v:Vec) (ln:Line3D) =
        ln.Move(v)

    /// Returns a 3D line moved by a given distance in X direction.
    static member inline moveX (distance:float) (ln:Line3D) =
        ln.MoveX(distance)

    /// Returns a 3D line moved by a given distance in Y direction.
    static member inline moveY (distance:double) (ln:Line3D) =
        ln.MoveY(distance)

    /// Returns a 3D line moved by a given distance in Z direction.
    static member inline moveZ (distance:double) (ln:Line3D) =
        ln.MoveZ(distance)

    /// Move a 3D line by a vector. (same as Line3D.translate)
    static member inline move (v:Vec) (ln:Line3D) =
        ln.Move(v)

    /// Applies or multiplies a 4x4 transformation matrix to a 3D line.
    static member inline transform (m:Matrix) (ln:Line3D) =
        ln.Transform m

    /// Multiplies (or applies) a RigidMatrix to a 3D line .
    static member inline transformRigid (m:RigidMatrix) (ln:Line3D) =
        ln.TransformRigid m

    /// Multiplies (or applies) a Quaternion to a 3D line.
    /// The resulting line has the same length as the input.
    static member inline rotate(q:Quaternion) (ln:Line3D) =
        ln.Rotate q

    /// Multiplies (or applies) a Quaternion to a 3D line around a given center point.
    /// The resulting line has the same length as the input.
    static member inline rotateWithCenter (cen:Pnt) (q:Quaternion) (ln:Line3D) =
        ln.RotateWithCenter (cen, q)


    /// Rotation a 3D line around Z-Axis.
    static member inline rotate2D (r:Rotation2D) (ln:Line3D) =
        Line3D(Pnt.rotateZBy r ln.From, Pnt.rotateZBy r ln.To)

    /// Rotation a 3D line round given Center point an a local Z-axis.
    static member inline rotate2dOn (cen:Pnt) (r:Rotation2D) (ln:Line3D) =
        Line3D(Pnt.rotateZwithCenterBy cen r ln.From, Pnt.rotateZwithCenterBy cen r ln.To)

    /// Ensure 3D line has a positive dot product with given orientation line.
    static member inline matchOrientation (orientationToMatch:Line3D) (lineToFlip:Line3D) =
        if orientationToMatch.Vector *** lineToFlip.Vector  < 0.0 then lineToFlip.Reversed else lineToFlip

    /// Ensure 3D line has a positive dot product with given 3D vector.
    static member inline matchVecOrientation (orientationToMatch:Vec) (lineToFlip:Line3D) =
        if orientationToMatch *** lineToFlip.Vector  < 0.0 then lineToFlip.Reversed else lineToFlip

    /// Ensure 3D line has a positive dot product with given 3D vector.
    static member inline matchUnitVecOrientation (orientationToMatch:UnitVec) (lineToFlip:Line3D) =
        if orientationToMatch *** lineToFlip.Vector  < 0.0 then lineToFlip.Reversed else lineToFlip

    /// Checks if the angle between the two 3D lines is less than 180 degrees.
    /// Calculates the dot product of two 3D lines.
    /// Then checks if it is positive.
    static member inline matchesOrientation180 (l:Line3D) (ln:Line3D) =
        l.MatchesOrientation180 ln

    /// Checks if the angle between the two 3D lines is less than 90 degrees.
    /// Calculates the dot product of the unit-vectors of the two 3D lines.
    /// Then checks if it is bigger than 0.707107 (cosine of 90 degrees).
    static member inline matchesOrientation90 (l:Line3D) (ln:Line3D) =
        l.MatchesOrientation90 ln

    /// Checks if two 3D lines are parallel. Ignoring orientation.
    /// Calculates the Cross Product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline areParallel (l:Line3D) (ln:Line3D) =
        l.IsParallelTo ln

    /// Checks if two 3D lines are parallel and orientated the same way.
    /// Calculates the Cross Product of the two line vectors. (= the area of the parallelogram)
    /// And checks if it is smaller than 1e-9
    /// Then calculates the dot product and checks if it is positive.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline areParallelAndMatchOrientation (l:Line3D) (ln:Line3D) =
        l.IsParallelAndOrientedTo ln

    /// Checks if two 3D lines are perpendicular.
    /// Calculates the dot product and checks if it is smaller than 1e-9.
    /// (NOTE: for very long lines a higher tolerance might be needed)
    static member inline arePerpendicular(l:Line3D) (ln:Line3D) =
        l.IsPerpendicularTo(ln)

    /// Assumes Line3D to be infinite.
    /// Returns the parameter at which a point is closest to the infinite line.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    static member inline closestParameterInfinite (p:Pnt) (ln:Line3D) =
        ln.ClosestParameterInfinite p

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameter (p:Pnt) (ln:Line3D) =
        ln.ClosestParameter p

    /// Assumes Line3D to be infinite.
    /// Returns closest point on infinite line.
    static member inline closestPointInfinite (p:Pnt) (ln:Line3D) =
        ln.ClosestPointInfinite p

    /// Returns closest point on (finite) line.
    static member inline closestPoint (p:Pnt) (ln:Line3D) =
        ln.ClosestPoint p

    /// Assumes Line3D to be infinite.
    /// Returns the square distance from point to infinite line.
    static member inline distanceSqToPntInfinite(p:Pnt) (ln:Line3D) =
        ln.DistanceSqToPntInfinite p

    /// Assumes Line3D to be infinite.
    /// Returns distance from point to infinite line.
    static member inline distanceToPntInfinite(p:Pnt) (ln:Line3D) =
        ln.DistanceToPntInfinite p

    /// Returns the square distance from point to (finite) line.
    static member inline distanceSqToPnt(p:Pnt) (ln:Line3D) =
        ln.DistanceSqToPnt p

    /// Returns distance from point to (finite) line.
    static member inline distanceToPnt(p:Pnt) (ln:Line3D) =
        ln.DistanceToPnt p

    /// Get distance from start of line to point projected onto line, may be negative.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline lengthToPtOnLine (ln:Line3D) pt =
        let t = ln.Vector
        let l = t.Length
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.lengthToPtOnLine %O to short for finding length to point." ln
        (t/l) *** (pt-ln.From)

    /// Extend 3D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extend (distAtStart:float) (distAtEnd:float) (ln:Line3D) =
        ln.Extend(distAtStart, distAtEnd)

    /// Extend 3D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendStart (distAtStart:float) (ln:Line3D) =
        ln.ExtendStart(distAtStart)

    /// Extend 3D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendEnd (distAtEnd:float) (ln:Line3D) =
        ln.ExtendEnd(distAtEnd)

        /// Extend 3D line by relative amount at start and end.
    /// A relative amount of 0.5 extends the line by half its length on each side.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendRel (relAtStart:float) (relAtEnd:float) (ln:Line3D) =
        ln.ExtendRel(relAtStart, relAtEnd)

    /// Extend 3D line by relative amount at start.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendStartRel (relAtStart:float) (ln:Line3D) =
        ln.ExtendStartRel(relAtStart)

    /// Extend 3D line by relative amount at end.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendEndRel (relAtEnd:float) (ln:Line3D) =
        ln.ExtendEndRel(relAtEnd)

    /// Shrink 3D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrink (distAtStart:float) (distAtEnd:float) (ln:Line3D) =
        ln.Shrink(distAtStart, distAtEnd)

    /// Shrink 3D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrinkStart (distAtStart:float) (ln:Line3D) =
        ln.ShrinkStart(distAtStart)

    /// Shrink 3D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrinkEnd (distAtEnd:float) (ln:Line3D) =
        ln.ShrinkEnd(distAtEnd)

    /// Finds point at given distance from line start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline pointAtDistance dist (ln:Line3D) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let len = sqrt(x*x + y*y + z*z)
        if isTooTiny len then EuclidException.Raisef "Euclid.Line3D.pointAtDistance %O to short for finding point at a distance." ln
        let f = dist/len
        Pnt(ln.FromX + x*f,
            ln.FromY + y*f,
            ln.FromZ + z*f)

    /// Returns new Line3D with given length, going out from start in direction of end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline withLengthFromStart len (ln:Line3D) =
        let x = ln.ToX-ln.FromX
        let y = ln.ToY-ln.FromY
        let z = ln.ToZ-ln.FromZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.withLengthFromStart %O to short for finding point at a distance." ln
        let f = len/l
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ,
                ln.FromX + x*f,
                ln.FromY + y*f,
                ln.FromZ + z*f)

    /// Returns new Line3D ending at current LineEnd with given length coming from direction of start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline withLengthToEnd len (ln:Line3D) =
        let x = ln.FromX-ln.ToX
        let y = ln.FromY-ln.ToY
        let z = ln.FromZ-ln.ToZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.withLengthToEnd %O to short for finding point at a distance." ln
        let f = len/l
        Line3D( ln.ToX + x*f,
                ln.ToY + y*f,
                ln.ToZ + z*f,
                ln.ToX,
                ln.ToY,
                ln.ToZ)

    /// Returns new Line3D with given length. Fixed in the midpoint.
    /// Missing length is added to or subtracted from both the end and start of the line.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline withLengthFromMid len (ln:Line3D) =
        let x = ln.FromX-ln.ToX
        let y = ln.FromY-ln.ToY
        let z = ln.FromZ-ln.ToZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then EuclidException.Raisef "Euclid.Line3D.withLengthFromMid %O to short for finding point at a distance." ln
        let f = (len/l + 1.0) * 0.5
        Line3D( ln.ToX   + x*f,
                ln.ToY   + y*f,
                ln.ToZ   + z*f,
                ln.FromX - x*f,
                ln.FromY - y*f,
                ln.FromZ - z*f)

    /// Offset line parallel to XY-Plane to left side in line direction.
    /// Z values are not changed.
    /// Fails on vertical lines or lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// If amount is 0.0 no offset is computed and the input line is returned.
    static member offsetXY amount (ln:Line3D) =
        if amount = 0.0 then
            ln
        else
            let x = ln.ToX - ln.FromX
            let y = ln.ToY - ln.FromY
            let lenXY = sqrt (x*x + y*y)
            if isTooTiny (lenXY ) then EuclidException.Raisef "Euclid.Line3D.offset: Cannot offset vertical Line3D (by %g) %O" amount ln
            let ox = -y*amount/lenXY  // unitized, horizontal, perpendicular  vector
            let oy =  x*amount/lenXY  // unitized, horizontal, perpendicular  vector
            Line3D( ln.FromX+ox,
                    ln.FromY+oy,
                    ln.FromZ,
                    ln.ToX+ox,
                    ln.ToY+oy,
                    ln.ToZ)

    /// Divides a 3D line into given amount of segments.
    /// Returns an array of 3D points of length: segment count + 1.
    /// Includes start and endpoint of line.
    static member divide (segments:int) (ln:Line3D) : Pnt[] =
        match segments with
        | x when x < 1 -> EuclidException.Raisef "Euclid.Line3D.divide failed for %d segments. Minimum one. for %O"  segments ln
        | 1 -> [|ln.From;  ln.To|]
        | k ->
            let x = ln.ToX - ln.FromX
            let y = ln.ToY - ln.FromY
            let z = ln.ToZ - ln.FromZ
            let sx = ln.FromX
            let sy = ln.FromY
            let sz = ln.FromZ
            let kk = float k
            let r = Array.zeroCreate (k+1)
            r.[0] <- ln.From
            for i = 1 to k-1 do
                let t = float i / kk
                r.[i] <- Pnt(sx + x*t, sy + y*t, sz + z*t)
            r.[k] <- ln.To
            r


    /// Divides a 3D line into as many as segments as possible respecting the minimum segment length.
    /// Returned Array includes start and endpoint of line.
    /// The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    static member divideMinLength (minSegmentLength:float) (ln:Line3D) : Pnt[] =
        let len = ln.Length
        if len < minSegmentLength then
            EuclidException.Raisef "Euclid.Line3D.divideMinLength: minSegmentLength %g is bigger than line length %g for %O"  minSegmentLength len ln
        let k = int (len / (minSegmentLength*1.00000095367431640625)) // 8 float steps above 1.0 https://float.exposed/0x3f800008
        Line3D.divide k ln


    /// Divides a 3D line into as few as segments as possible respecting the maximum segment length.
    /// Returned Array includes start and endpoint of line.
    /// The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical  errors.
    /// That means in an edge case there are fewer segments returned, not more.
    static member divideMaxLength (maxSegmentLength:float) (ln:Line3D) : Pnt[] =
        let len = ln.Length
        let k = int (len / maxSegmentLength*0.999999523162841796875) + 1 // 8 float steps below 1.0 https://float.exposed/0x3f7ffff8
        Line3D.divide k ln

    /// Divides a 3D line into given amount of segments.
    /// Includes a gap between the segments. But not at the start or end.
    /// Returns an array of 3D Lines.
    /// Returns an empty array if the length of the line is less than gap-size x segment-count-minus-1.
    static member split (gap:float) (segments:int) (ln:Line3D) : Line3D[] =
        if segments <= 0  then
            EuclidException.Raisef "Euclid.Line3D.split failed for %d segments. Minimum one. for %O"  segments ln
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
            let vz = v.Z
            let x = ln.FromX
            let y = ln.FromY
            let z = ln.FromZ
            for i = 0 to segments-1 do
                let g = float i
                let s = float (i+1)
                let sf = (g*segLen + g*gap)/len
                let ef = (s*segLen + g*gap)/len
                let xs = x + vx*sf
                let ys = y + vy*sf
                let zs = z + vz*sf
                let xe = x + vx*ef
                let ye = y + vy*ef
                let ze = z + vz*ef
                lns.[i] <- Line3D(xs,ys,zs,xe,ye,ze)
            // correct last point to avoid numerical errors
            lns.[segments-1] <- Line3D.setEnd ln.To lns.[segments-1]
            lns

    /// Divides a 3D line into as many as segments as possible respecting the minimum segment length and the gap.
    /// Includes a gap between the segments. But not at the start or end.
    /// Returns an array ofe3D Lines
    /// The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    static member splitMinLength (gap:float) (minSegmentLength:float) (ln:Line3D) : Line3D[] =
        let len = ln.Length
        if len < minSegmentLength then
            EuclidException.Raisef "Euclid.Line3D.splitMinLength: minSegmentLength %g is bigger than line length %g for %O"  minSegmentLength len ln
        let k = int ((len+gap) / ((minSegmentLength+gap)*1.000000953)) // 8 float steps above 1.0 https://float.exposed/0x3f800008
        Line3D.split gap k ln


    /// Divides a 3D line into as few as segments as possible respecting the maximum segment length and the gap.
    /// Includes a gap between the segments. But not at the start or end.
    /// Returns an array ofe3D Lines
    /// The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    static member splitMaxLength (gap:float) (maxSegmentLength:float) (ln:Line3D)  : Line3D[] =
        let len = ln.Length
        let k = int ((len+gap) / (maxSegmentLength+gap)*0.999999523) + 1 // 8 float steps below 1.0 https://float.exposed/0x3f7ffff8
        Line3D.split gap k ln


    /// Divides a 2D line into segments of given length.
    /// Includes start and end point
    /// Adds end point only if there is a remainder bigger than 1% of the segment length.
    static member  divideEvery dist (l:Line3D) =
        let len = l.Length
        let div = len / dist
        let floor = System.Math.Floor div
        let step = 1.0 / floor
        let count = int floor
        let pts = ResizeArray<Pnt>(count + 2)
        pts.Add l.From
        for i = 1 to count do
            pts.Add <| l.EvaluateAt (step * float i)
        if div - floor > 0.01 then
            pts.Add l.To // add end point only if there is a remainder bigger than 1%
        pts

    /// Divides a 2D line into segments of given length.
    /// Excludes start and end point
    /// Adds last div point before end only if there is a remainder bigger than 1% of the segment length.
    static member  divideInsideEvery dist (l:Line3D) =
        let len = l.Length
        let div = len / dist
        let floor = System.Math.Floor div
        let step = 1.0 / floor
        let count = int floor
        let pts = ResizeArray<Pnt>(count)
        for i = 1 to count - 1 do
            pts.Add <| l.EvaluateAt (step * float i)
        if div - floor > 0.01 then
            pts.Add <| l.EvaluateAt (step * floor) // add last div point only if there is a remainder bigger than 1%
        pts

    //----------------------------------------------------------------------------------------------------------------
    //------------------------------Line Line Intersection : ----------------------------------------------------
    //----------------------------------------------------------------------------------------------------------------


    ///<summary> Gets the parameters at which two infinite 3D lines intersect. Or are closest to each other.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module Euclid.UtilEuclid.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this, then the 'Coincident' union case is returned.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An IntersectionParam Discriminated Union with the following cases:
    ///
    /// | TwoParam of twoParams : struct(float*float):
    /// The infinite lines are intersecting or skew.
    /// They have each one point where they are intersecting each other.
    /// Or are closest to each other in skew case.
    /// The tuple's order corresponds to the input order.
    ///
    /// | Parallel:
    /// The lines are parallel within 0.25 degrees.
    /// They have no points in common.
    ///
    /// | Coincident:
    /// The lines are coincident (or maybe even identical) .
    /// As infinite lines they have infinitely many points in common.
    /// They might still not have the same start and end points in their finit definition.
    ///
    /// | TooShort:
    /// One or both input lines is shorter than the given minimum Length tolerance. </returns>
    static member inline intersectionParamInfinite( lnA:Line3D,
                                                    lnB:Line3D,
                                                    [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                                    [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                                    [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                                    ) : IntersectionParam =
        //https://stackoverflow.com/a/34604574/969070 but DP and DQ are in wrong order !
        let ax = lnA.FromX - lnA.ToX
        let ay = lnA.FromY - lnA.ToY
        let az = lnA.FromZ - lnA.ToZ
        let bx = lnB.FromX - lnB.ToX
        let by = lnB.FromY - lnB.ToY
        let bz = lnB.FromZ - lnB.ToZ
        let a = ax*ax + ay*ay + az*az // square length of A
        let c = bx*bx + by*by + bz*bz // square length of B
        let shortSq = tooShortTolerance * tooShortTolerance
        if a < shortSq then  // vec A too short
            if c < shortSq then
                IntersectionParam.TooShortBoth
            else
                IntersectionParam.TooShortA
        elif c < shortSq then  // vec B too short
            IntersectionParam.TooShortB
        else
            let b = ax*bx + ay*by + az*bz // dot product of both lines
            let vx = lnB.FromX - lnA.FromX
            let vy = lnB.FromY - lnA.FromY
            let vz = lnB.FromZ - lnA.FromZ
            let ac = a * c // square of square length, never negative
            let bb = b * b // never negative
            let discriminant = ac - bb // never negative, the dot product cannot be bigger than the two square length multiplied with each other
            let div = ac + bb // never negative
            // getting the relation between the sum and the subtraction gives a good estimate of the angle between the lines
            // see module Euclid.UtilEuclid.RelAngleDiscriminant
            let rel = discriminant / div
            if rel < float relAngleDiscriminant then //parallel
                /// TODO: do this calculations later, just return one Parallel case here !
                let e = bx*vx + by*vy + bz*vz
                let t = e / c // c is already checked for being non zero. get closest parameter of lnA.From on lnB
                let p = lnB.EvaluateAt(t) //TODO could be inlined to optimize
                if Pnt.distanceSq p lnA.From < coincidentTolerance*coincidentTolerance then
                    IntersectionParam.Coincident
                else
                    IntersectionParam.Parallel
            else
                let e = bx*vx + by*vy + bz*vz
                let d = ax*vx + ay*vy + az*vz
                let t = (b * e - c * d) / discriminant
                let u = (a * e - b * d) / discriminant
                TwoParam (t, u)

    ///<summary> Gets the points at which two infinite 3D lines intersect. Or are closest to each other.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module Euclid.UtilEuclid.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this, then the 'Coincident' union case is returned.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    ///<returns> An IntersectionPoints3D Discriminated Union with the following cases:
    /// | TwoPoints of skewPoints : struct(Pnt*Pnt)
    ///     The lines are skew by mor than 1e-6. or the given tolerance
    ///     Contains the points on the first and second
    ///     line where they are closest to each other.
    ///
    /// | OnePoint of xPoint : Pnt
    ///     The 3D lines are intersection in exactly one point.
    ///     Or the points are closer than the skewTolerance.  1e-6 by default. The returned point is on line A.
    ///
    /// | Parallel:
    ///     The lines are parallel, within the given tolerance.
    ///
    /// | Coincident:
    ///     The lines are coincident or maybe even identical.
    ///     As infinite lines they have infinitely many points in common.
    ///
    /// | TooShort:
    ///     One or both input lines is shorter than the given minimum Length tolerance.
    /// </returns>
    static member inline intersectionInfinite ( lnA:Line3D,
                                                lnB:Line3D,
                                                [<OPT;DEF(1e-6)>] skewTolerance:float,
                                                [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                                [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                                [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                                ) : IntersectionPoints3D =
        match Line3D.intersectionParamInfinite(lnA, lnB, relAngleDiscriminant, coincidentTolerance, tooShortTolerance) with
        |TwoParam (u, v) ->
            let a = lnA.EvaluateAt u
            let b = lnB.EvaluateAt v
            if Pnt.distanceSq a b > skewTolerance*skewTolerance then
                IntersectionPoints3D.TwoPoints (a, b)
            else
                IntersectionPoints3D.OnePoint a // or (Pnt.midPt a b) ?
        |IntersectionParam.Parallel      ->  IntersectionPoints3D.Parallel
        |IntersectionParam.Coincident    ->  IntersectionPoints3D.Coincident
        |IntersectionParam.TooShortA
        |IntersectionParam.TooShortB
        |IntersectionParam.TooShortBoth  ->  IntersectionPoints3D.TooShort

    ///<summary>Gets the single points where these two infinite 3D lines actually intersect each other.
    /// The returned point is on line A. </summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle an EuclidException is raised.
    /// Use the module Euclid.UtilEuclid.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this, then an EuclidException is raised.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then an EuclidException is raised.</param>
    /// <returns> A single 3D point</returns>
    static member intersectionPointInfinite(lnA:Line3D,
                                            lnB:Line3D,
                                            [<OPT;DEF(1e-6)>] skewTolerance:float,
                                            [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                            [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                            [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                            ) : Pnt =
        match Line3D.intersectionInfinite(lnA, lnB, skewTolerance, relAngleDiscriminant, coincidentTolerance, tooShortTolerance) with
        |IntersectionPoints3D.OnePoint p  -> p
        |IntersectionPoints3D.TwoPoints _ -> EuclidException.Raisef "Euclid.Line3D.intersectionPointInfinite: Lines are skew lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionPoints3D.Parallel    -> EuclidException.Raisef "Euclid.Line3D.intersectionPointInfinite: Lines are parallel lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionPoints3D.Coincident  -> EuclidException.Raisef "Euclid.Line3D.intersectionPointInfinite: Lines are coincident lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionPoints3D.TooShort    -> EuclidException.Raisef "Euclid.Line3D.intersectionPointInfinite: Lines are tooShort lnA: \r\n%O and lnB: \r\n%O" lnA lnB

    /// Assumes Lines to be infinite.
    /// Returns the distance between two infinite 3D lines. At their closest point.
    /// Fails if one or both lines are shorter than 1e-6.
    /// Unless the lines are skew or parallel this returns 0.0.
    /// Uses the default tolerances from the Line3D.intersectionParamInfinite function.
    /// to detect parallel and coincident lines.
    static member inline distanceBetweenInfiniteLines(lnA, lnB) =
        match Line3D.intersectionParamInfinite(lnA, lnB) with
        |IntersectionParam.Coincident    ->  0.0
        |IntersectionParam.TooShortA     ->  EuclidException.Raisef "Euclid.Line3D.distanceBetweenInfiniteLines: Line A is tooShort lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionParam.TooShortB     ->  EuclidException.Raisef "Euclid.Line3D.distanceBetweenInfiniteLines: Line B is  tooShort lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionParam.TooShortBoth  ->  EuclidException.Raisef "Euclid.Line3D.distanceBetweenInfiniteLines: both Lines are tooShort lnA: \r\n%O and lnB: \r\n%O" lnA lnB
        |IntersectionParam.Parallel      -> lnA.DistanceToPntInfinite lnB.From
        |TwoParam (u, v) -> // skew or intersecting
            let a = lnA.EvaluateAt u
            let b = lnB.EvaluateAt v
            Pnt.distance a b


    /// <summary>Returns the intersection kind and the parameters at which two (finite) 3D Lines are intersecting or closest to each other.
    /// The threshold for skew intersection can be given as an optional tolerance input. The default is 1e-6.
    /// If the two points ar within this distance one of the Intersecting Cases is returned.
    /// (or Continuation Case if lines are colinear in one point)
    /// The returned parameters are both between 0.0 and 1.0.
    /// For parallel and coincident lines it still returns the two end points that are closest to each other or a point in the middle of their overlap.
    /// First parameter is on lnA, second parameter is on lnB.
    /// The possible result cases are:
    ///
    /// | Intersecting : The finite lines are intersecting each other in one point inside both lines.
    /// | IntersectingEndsBoth:   The finite lines are intersecting each other at one of their end or start points point.
    /// | IntersectingEndsFirst:  The finite lines are intersecting. The first line is touching the second one with its end or start point.
    /// | IntersectingEndsSecond: The finite lines are intersecting. The second line is touching the first one with its end or start point.
    ///
    /// | Skew:  The finite lines are skew to each other.
    /// Their closest points to each other are within the line.
    /// The returned parameters are between 0.0 and 1.0
    ///
    /// | Apart: The finite lines are not intersecting nor skew.
    /// At least one of the parameters of closets points would be outside of the range 0.0 and 1.0.
    /// The returned parameters still indicate where the finite lines are closest to each other.
    ///
    ///------- Parallel and other special cases for finite lines: ---------------
    ///
    /// | Parallel : The finite lines are parallel. Within 0.25 degrees.
    /// The returned parameters are in the middle of their overlap,
    /// or the two end points that are closest to each other.
    ///
    /// | Overlapping
    /// The lines are coincident, overlapping and parallel within 0.25 degrees.
    /// The returned parameters are at start and end of overlap.
    ///
    /// | CoincidentApart: The lines are coincident, parallel within 0.25 degrees.  But ends are apart.
    /// The returned parameters still indicate where the lines are closest to each other.
    ///
    /// | Continuation : The lines are coincident, parallel within 0.25 degrees.
    /// The ends are meeting in exactly one point. And Oriented the same way.
    /// The returned parameters indicate which ends these are.
    ///
    /// | ContinuationFlipped: The lines are coincident, parallel within 0.25 degrees.
    /// The ends are meeting in exactly one point. But orientation is flipped.
    /// The returned parameters indicate which ends these are.
    ///
    /// | Identical: The lines are identical, in orientation too with in 1e-6 tolerance.
    /// The returned parameters still indicate where the lines start and end.
    ///
    /// | IdenticalFlipped: The lines are identical. But orientation is flipped.
    /// The returned parameters still indicate where the lines start and end.
    ///
    /// ------------------------------Error Cases: -----------------------------
    ///
    /// | TooShortA
    /// Input line A is shorter than the given minimum Length tolerance.
    /// The returned parameters are 0.5 for line A and the closets point to lineB from the middle of line A.
    ///
    /// | TooShortB
    /// Input line B is shorter than the given minimum Length tolerance.
    /// The returned parameters are the closets point to lineA from the middle of line B and 0.5 for line B.
    ///
    /// | TooShortBoth
    /// Both input lines are shorter than the given minimum Length tolerance.
    /// The returned parameters are 0.5 and 0.5 for both lines. </summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module Euclid.UtilEuclid.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this, then the 'Coincident' union case is returned.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    static member inline intersectionParam (lnA:Line3D,
                                            lnB:Line3D,
                                            [<OPT;DEF(1e-6)>] skewTolerance:float,
                                            [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                            [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                            [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                            ) :  IntersectionKind*float*float =
        match Line3D.intersectionParamInfinite(lnA, lnB, relAngleDiscriminant, coincidentTolerance, tooShortTolerance) with
        | IntersectionParam.TwoParam (u, v) ->
            /// numerical error tolerance check to also find an intersection that happens just after the line end:
            let ur = isZeroOneOrBetween u // TODO the absolute tolerance for being at the Line end should be configurable
            let vr = isZeroOneOrBetween v // TODO the absolute tolerance for being at the Line end should be configurable

            let inline fix01 zt1 x = match zt1 with Zero -> 0.0 |One -> 1.0 |Between |Outside -> x

            if   ur = Outside || vr = Outside then
                // finite Lines are not intersecting, still find their closest Points:
                let pu = lnA.EvaluateAt  (clampBetweenZeroAndOne u)
                let vt = Line3D.closestParameter pu lnB
                let pv = lnB.EvaluateAt (clampBetweenZeroAndOne vt)
                let ut = Line3D.closestParameter pv lnA
                Apart, ut, vt

            else
                let a = lnA.EvaluateAt(u)
                let b = lnA.EvaluateAt(v)
                let d = Pnt.distanceSq a b
                if d > skewTolerance*skewTolerance then
                    Skew, u, v
                elif ur = Zero || ur = One then
                    if vr = Zero || vr = One then
                        IntersectingEndsBoth, (fix01 ur u), (fix01 vr  v)
                    else
                        IntersectingEndsFirst, (fix01 ur u), v
                elif vr = Zero || vr = One then
                    IntersectingEndsSecond, u, (fix01 vr  v)
                else
                    Intersecting, u, v

        | IntersectionParam.Parallel ->

            let lv = lnA.Direction
            let llv = lnB.Direction
            //make a new line k that is oriented the same way:
            let flip = lv *** llv < 0.0
            let k = if flip then lnB.Reversed else lnB
            let l0k0 = Vec.create(lnA.From, k.From)
            let l0k1 = Vec.create(lnA.From, k.To)
            let l1k0 = Vec.create(lnA.To  , k.From)
            let l1k1 = Vec.create(lnA.To  , k.To)
            // check if vectors between lines are in same orientation as line:
            let d00 = lv *** l0k0 > 0.
            let d01 = lv *** l0k1 > 0.
            let d11 = lv *** l1k1 > 0.
            let d10 = lv *** l1k0 > 0.

            // Full logic:
            // there are many valid parameters
            // Parameters are at the end or start of line lnA when possible
            //let u, v =
            //    if   not d00 && not d01 && not d10 && not d11 then
            //                                                        Printfn.gray "// lnA starts after k ends"
            //                                                        0.0, if flip then 0.0 else  1.0
            //    elif not d00 &&     d01 && not d10 && not d11 then
            //                                                        Printfn.gray "// k is overlapping lnA start"
            //                                                        0.0, lnB.ClosestParameter(lnA.From)
            //    elif     d00 &&     d01 && not d10 && not d11 then
            //                                                        Printfn.gray "// k is on both ends shorter than lnA  "
            //                                                        lnA.ClosestParameter(lnB.From), 0.0
            //    elif not d00 &&     d01 && not d10 &&     d11 then
            //                                                        Printfn.gray "// lnA is on both ends shorter than k  "
            //                                                        0.0  , lnB.ClosestParameter(lnA.From)
            //    elif     d00 &&     d01 && not d10 &&     d11 then
            //                                                        Printfn.gray "// k is overlapping lnA end "
            //                                                        1.0  , lnB.ClosestParameter(lnA.To)
            //    elif     d00 &&     d01 &&     d10 &&     d11 then
            //                                                        Printfn.gray "// k starts after lnA ends"
            //                                                        1.0, if flip then 1.0 else  0.0
            //    else failwith "Bad case in intersectLineParametersInfinite"
            //IntersectionKind.Parallel, u, v

            // Optimized logic:
            if d01 then
                if d10 then
                    IntersectionKind.Parallel, 1.0, if flip then 1.0 else  0.0   // k starts after lnA ends
                else
                    if d00 then
                        if d11 then IntersectionKind.Parallel, 1.0, lnB.ClosestParameter(lnA.To)   // k is overlapping lnA end
                        else        IntersectionKind.Parallel, lnA.ClosestParameter(lnB.From), 0.0  // k is on both ends shorter than lnA
                    else
                        IntersectionKind.Parallel, 0.0, lnB.ClosestParameter(lnA.From) // k is overlapping lnA start // lnA is on both ends shorter than k
            else
                IntersectionKind.Parallel, 0.0, if flip then 0.0 else  1.0  // lnA starts after k ends


        | IntersectionParam.Coincident ->
            // cases Overlapping | Continuation  | CoincidentApart | Identical
            let lv = lnA.Direction // Vec(ax, ay, az)
            let llv = lnB.Direction //Vec(bx, by, bz)
            let flip = lv *** llv < 0.0
            let k = if flip then lnB.Reversed else lnB
            let l0k0 = Vec.create(lnA.From, k.From)
            let l0k1 = Vec.create(lnA.From, k.To)
            let l1k0 = Vec.create(lnA.To  , k.From)
            let l1k1 = Vec.create(lnA.To  , k.To)
            let coTolSq = coincidentTolerance*coincidentTolerance
            let z00 = l0k0.LengthSq < coTolSq
            let z01 = l0k1.LengthSq < coTolSq
            let z10 = l1k0.LengthSq < coTolSq
            let z11 = l1k1.LengthSq < coTolSq

            if z00 && z11 then
                if flip then IdenticalFlipped , 0.0 , 1.0
                else         Identical        , 0.0 , 0.0
            elif z10  then
                if flip then ContinuationFlipped , 1.0 , 1.0
                else         Continuation        , 1.0 , 0.0
            elif z01  then
                if flip then ContinuationFlipped , 0.0 , 0.0
                else         Continuation        , 0.0 , 1.0
            else
                // check if vectors between lines are in same orientation as line:
                let d00 = lv *** l0k0 > 0.
                let d01 = lv *** l0k1 > 0.
                let d10 = lv *** l1k0 > 0.
                let d11 = lv *** l1k1 > 0.
                // Full logic:
                //if   not d00 && not d01 && not d10 && not d11 then
                //                                                    Printfn.gray "// lnA starts after k ends"
                //                                                    CoincidentApart, 0.0, if flip then 0.0 else  1.0
                //elif not d00 &&     d01 && not d10 && not d11 then
                //                                                    Printfn.gray "// k is overlapping lnA start"
                //                                                    Overlapping    , 0.0, if flip then 0.0 else  1.0
                //elif     d00 &&     d01 && not d10 && not d11 then
                //                                                    Printfn.gray "// k is on both ends shorter than lnA  "
                //                                                    Overlapping    , lnA.ClosestParameter(lnB.From), 1.0
                //elif not d00 &&     d01 && not d10 &&     d11 then
                //                                                    Printfn.gray "// lnA is on both ends shorter than k  "
                //                                                    Overlapping    , 0.0  , lnB.ClosestParameter(lnA.To)
                //elif     d00 &&     d01 && not d10 &&     d11 then
                //                                                    Printfn.gray "// k is overlapping lnA end "
                //                                                    Overlapping    , 1.0  , if flip then 1.0 else  0.0
                //elif     d00 &&     d01 &&     d10 &&     d11 then
                //                                                    Printfn.gray "// k starts after lnA ends"
                //                                                    CoincidentApart, 1.0, if flip then 1.0 else  0.0
                //else failwith "Bad case in intersectLineParametersInfinite"

                if d01 then
                    if d10 then
                        CoincidentApart, 1.0  , if flip then 1.0 else  0.0   // k starts after lnA ends
                    else
                        if d11 then
                            if  d00 then Overlapping  , 1.0  , if flip then 1.0 else  0.0  // k is overlapping lnA end
                            else         Overlapping  , 0.0  , lnB.ClosestParameter(lnA.To)   // lnA is on both ends shorter than k
                        else
                            if d00 then Overlapping, lnA.ClosestParameter(lnB.From), 1.0   // k is on both ends shorter than lnA
                            else        Overlapping, 0.0, if flip then 0.0 else  1.0   // k is overlapping lnA start
                else
                    CoincidentApart, 0.0, if flip then 0.0 else  1.0    // lnA starts after k ends

        |IntersectionParam.TooShortA     -> TooShortA    , 0.5 , lnB.ClosestParameter lnA.Mid
        |IntersectionParam.TooShortB     -> TooShortB    , lnA.ClosestParameter lnB.Mid, 0.5
        |IntersectionParam.TooShortBoth  -> TooShortBoth , 0.5, 0.5

    /// <summary>Returns the intersection kind and the points at which two (finite) 3D Lines are intersecting or closest to each other.
    /// The threshold for skew intersection can be given as an optional tolerance input. The default is 1e-6.
    /// If the two points ar within this distance one of the Intersecting Cases is returned.
    /// (or Continuation Case if lines are colinear in one point)
    /// one of the Intersecting Cases is returned. (or Continuation Case if lines are colinear in one point)
    /// The returned points are on the respective lines.
    /// For parallel and coincident lines it still returns the two end points that are closest to each other or a point in the middle of their overlap.
    /// First point is on lnA, second point is on lnB.
    /// The possible result cases are:
    ///
    /// | Intersecting : The finite lines are intersecting each other in one point.
    /// | IntersectingEndsBoth: The finite lines are intersecting each other at one of their end or start points point.
    /// | IntersectingEndsFirst: The finite lines are intersecting. The first line is touching the second one with its end or start point.
    /// | IntersectingEndsSecond:  The finite lines are intersecting. The second line is touching the first one with its end or start point.
    ///
    /// | Skew:  The finite lines are skew to each other.
    /// Their closest points to each other are within the line.
    /// The returned points are between 0.0 and 1.0
    ///
    /// | Apart: The finite lines are not intersecting nor skew.
    /// At least one of the points of closets points would be outside of the range 0.0 and 1.0.
    /// The returned points still indicate where the finite lines are closest to each other.
    ///
    ///------- Parallel and other special cases for finite lines: ---------------
    ///
    /// | Parallel : The finite lines are parallel. Within 0.25 degrees.
    /// The returned points are in the middle of their overlap,
    /// or the two end points that are closest to each other.
    ///
    /// | Overlapping
    /// The lines are coincident, overlapping and parallel within 0.25 degrees.
    /// The returned points are at start and end of overlap.
    ///
    /// | CoincidentApart: The lines are coincident, parallel within 0.25 degrees.  But ends are apart.
    /// The returned points still indicate where the lines are closest to each other.
    ///
    /// | Continuation : The lines are coincident, parallel within 0.25 degrees.
    /// The ends are meeting in exactly one point. And Oriented the same way.
    /// The returned points indicate which ends these are.
    ///
    /// | ContinuationFlipped: The lines are coincident, parallel within 0.25 degrees.
    /// The ends are meeting in exactly one point. But orientation is flipped.
    /// The returned points indicate which ends these are.
    ///
    /// | Identical: The lines are identical, in orientation too with in 1e-6 tolerance.
    /// The returned points still indicate where the lines start and end.
    ///
    /// | IdenticalFlipped: The lines are identical. But orientation is flipped.
    /// The returned points still indicate where the lines start and end.
    ///
    ///  ------------------------------Error Cases: -----------------------------
    ///
    /// | TooShortA
    /// Input line A is shorter than the given minimum Length tolerance.
    /// The returned points are the middle of line A and the closets point to it on line B.
    ///
    /// | TooShortB
    /// Input line B is shorter than the given minimum Length tolerance.
    /// The returned points are the closets point to lineA from the middle of line B and the middle of line B.
    ///
    /// | TooShortBoth
    /// Both input lines are shorter than the given minimum Length tolerance.
    /// The returned points are in the middle of both lines.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module Euclid.UtilEuclid.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this, then the 'Coincident' union case is returned.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    static member inline intersection  (lnA:Line3D,
                                        lnB:Line3D,
                                        [<OPT;DEF(1e-6)>] skewTolerance:float,
                                        [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                        [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                        [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                        ) : IntersectionKind*Pnt*Pnt =
        let k, u, v = Line3D.intersectionParam(lnA, lnB, skewTolerance, relAngleDiscriminant, coincidentTolerance, tooShortTolerance)
        match k with
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond
        | Continuation | ContinuationFlipped ->
            let p = lnA.EvaluateAt u
            k, p, p // return same point twice ?
        | Skew | Apart  | IntersectionKind.Parallel
        | Overlapping | CoincidentApart | Identical| IdenticalFlipped
        | TooShortA | TooShortB | TooShortBoth -> // TODO or check if the zero length line is actually on the other line ?
            let a =  lnA.EvaluateAt u
            let b = lnB.EvaluateAt v
            k, a, b


    /// <summary>Returns the single points where these two finite lines actually intersect each other. Or None
    /// The threshold for skew intersection can be given as an optional tolerance input.
    /// If the two points are within this distance a point is returned.
    /// This might also be the Continuation Case if lines are colinear in one point.</summary>
    ///<param name="lnA"> The first line.</param>
    ///<param name="lnB"> The second line.</param>
    ///<param name="skewTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If skew lines are closer than this distance they are considered intersecting in one point. The returned point is on line A.</param>
    ///<param name="relAngleDiscriminant"> This is an optional tolerance for the internally calculated relative Angle Discriminant.
    /// The default value is '0.00000952' this corresponds to approx 0.25 degree. Below this angle the 'Parallel' or 'Coincident' union case is returned.
    /// Use the module Euclid.UtilEuclid.RelAngleDiscriminant to set another tolerance here.</param>
    ///<param name="coincidentTolerance" > Is an optional distance tolerance. 1e-6 by default.
    ///  If parallel lines are closer than this, then the 'Coincident' union case is returned.</param>
    ///<param name="tooShortTolerance" > Is an optional length tolerance. 1e-6 by default.
    ///  If one or both lines are shorter than this, then the 'TooShort' union case is returned.</param>
    static member intersectionPoint (lnA:Line3D,
                                    lnB:Line3D,
                                    [<OPT;DEF(1e-6)>] skewTolerance:float,
                                    [<OPT;DEF(RelAngleDiscriminant.``0.25``)>] relAngleDiscriminant:float<RelAngleDiscriminant.relAngDiscr>,
                                    [<OPT;DEF(1e-6)>] coincidentTolerance:float,
                                    [<OPT;DEF(1e-6)>] tooShortTolerance:float
                                    ) :  option<Pnt> =
        let k, u, _ = Line3D.intersectionParam(lnA, lnB, skewTolerance, relAngleDiscriminant, coincidentTolerance, tooShortTolerance)
        match k with
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond
        | Continuation | ContinuationFlipped ->
            Some(lnA.EvaluateAt u)
        | Skew | Apart  | IntersectionKind.Parallel
        | Overlapping | CoincidentApart | Identical| IdenticalFlipped  ->
            None
        | TooShortA | TooShortB -> // TODO or check if the short line or zero length line is still exactly on the other line ?
            None
        | TooShortBoth -> // TODO or return a point if two zero length lines are on the same point?
            None

    /// Returns the distance between two finite 3D lines.
    /// For parallel lines the distance is calculate form the actual finit elements. (like in the other cases.)
    /// So it is maybe bigger than the parallel offset.
    /// For Coincident and intersecting lines below a tolerance of 1e-16 this always returns 0.0.
    static member inline distanceBetweenLines(lnA, lnB) : float=
        let k, u, v = Line3D.intersectionParam(lnA, lnB, 1e-16) // use lower skew tolerance here to not return 0.0 if it actually bigger
        match k with
        | Intersecting | IntersectingEndsBoth | IntersectingEndsFirst | IntersectingEndsSecond
        | Continuation | ContinuationFlipped | Overlapping   | Identical| IdenticalFlipped ->
            0.0
        | Skew | Apart  | IntersectionKind.Parallel | CoincidentApart
        | TooShortA | TooShortB | TooShortBoth ->
            let a = lnA.EvaluateAt u
            let b = lnB.EvaluateAt v
            Pnt.distance a b


    /// <summary>Checks if the two finite 3D lines are touching each other at any of end points
    /// within the given tolerance.
    /// This will also return found result if the lines are touching on both points.</summary>
    /// <param name="tol"> The tolerance for the distance between the end points.</param>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <returns>
    /// An integer value indicating the touching case:
    /// 0 if not touching,
    /// 1 if touching at End of A and Start of B,
    /// 2 if touching at Start of A and End of B,
    /// 3 if touching at Start of A and Start of B,
    /// 4 if touching at End of A and End of B
    /// </returns>
    static member areTouchingAny tol (a:Line3D) (b:Line3D)  : int =
        let tolSq = tol*tol
        if (
            let x = a.ToX-b.FromX
            let y = a.ToY-b.FromY
            let z = a.ToZ-b.FromZ
            x*x + y*y + z*z < tolSq) then 1
        elif (
            let x = a.FromX-b.ToX
            let y = a.FromY-b.ToY
            let z = a.FromZ-b.ToZ
            x*x + y*y + z*z < tolSq) then 2
        elif  (
            let x = a.FromX-b.FromX
            let y = a.FromY-b.FromY
            let z = a.FromZ-b.FromZ
            x*x + y*y + z*z < tolSq) then 3
        elif (
            let x = a.ToX-b.ToX
            let y = a.ToY-b.ToY
            let z = a.ToZ-b.ToZ
            x*x + y*y + z*z < tolSq) then 4
        else
            0




    /// <summary>Checks if the two finite 3D lines are touching each other at exactly one of their end points
    /// within the given tolerance.
    /// This will return a separate case (5 or 6) if the lines are touching on both points.</summary>
    /// <param name="tol"> The tolerance for the distance between the end points.</param>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <returns>
    /// An integer value indicating the touching case:
    /// 0 if not touching,
    /// 1 if touching at End of A and Start of B,
    /// 2 if touching at Start of A and End of B,
    /// 3 if touching at Start of A and Start of B,
    /// 4 if touching at End of A and End of B,
    /// 5 if touching at both Start and End. Lines are identical and in same orientation.
    /// 6 if touching at both at the other Start or End. Lines are identical but in opposite orientation.
    /// </returns>
    static member areTouchingEither tol (a:Line3D) (b:Line3D) =
        let tolSq = tol*tol
        if (
            let x = a.ToX-b.FromX
            let y = a.ToY-b.FromY
            let z = a.ToZ-b.FromZ
            x*x + y*y + z*z < tolSq) then
                if (
                    let x = a.FromX-b.ToX
                    let y = a.FromY-b.ToY
                    let z = a.FromZ-b.ToZ
                    x*x + y*y + z*z < tolSq) then 6
                else
                    1
        elif (
            let x = a.FromX-b.ToX
            let y = a.FromY-b.ToY
            let z = a.FromZ-b.ToZ
            x*x + y*y + z*z < tolSq) then 2
            // if ( // this cant happen because it would have been caught by the above if case
            //     let x = a.ToX-b.FromX
            //     let y = a.ToY-b.FromY
            //     let z = a.ToZ-b.FromZ
            //     x*x + y*y + z*z < tolSq) then 6
            // else
            //     2
        elif (
            let x = a.FromX-b.FromX
            let y = a.FromY-b.FromY
            let z = a.FromZ-b.FromZ
            x*x + y*y + z*z < tolSq) then
                if (
                    let x = a.ToX-b.ToX
                    let y = a.ToY-b.ToY
                    let z = a.ToZ-b.ToZ
                    x*x + y*y + z*z < tolSq) then 5
                else
                    3
        elif (
            let x = a.ToX-b.ToX
            let y = a.ToY-b.ToY
            let z = a.ToZ-b.ToZ
            x*x + y*y + z*z < tolSq) then 4
            // if ( // this cant happen because it would have been caught by the above if case
            //     let x = a.FromX-b.FromX
            //     let y = a.FromY-b.FromY
            //     let z = a.FromZ-b.FromZ
            //     x*x + y*y + z*z < tolSq) then 5
            // else
            //     4
        else
            0


