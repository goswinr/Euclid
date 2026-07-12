namespace Euclid

open System
open Euclid.UtilEuclid
open UtilEuclid
open EuclidErrors

/// When Euclid is opened this module will be auto-opened.
/// It only contains extension members for type Line3D.
[<AutoOpen>]
module AutoOpenLine3D =

  type Line3D with


    /// Checks if 3D line is parallel to the world X axis. Ignoring orientation.
    /// The absolute deviation tolerance along Y and Z axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsXAligned : bool =
        let x = abs ln.VectorX
        let y = abs ln.VectorY
        let z = abs ln.VectorZ
        if isTooSmall (x+y+z) then
            failTooSmall "Line3D.IsXAligned" ln
        y < axisAlignmentTolerance && z < axisAlignmentTolerance

    /// Checks if 3D line is parallel to the world X axis. Ignoring orientation.
    /// The absolute deviation tolerance along Y and Z axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    static member inline isXAligned (l:Line3D) : bool =
        l.IsXAligned

    /// Checks if 3D line is parallel to the world Y axis. Ignoring orientation.
    /// The absolute deviation tolerance along X and Z axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsYAligned : bool =
        let x = abs ln.VectorX
        let y = abs ln.VectorY
        let z = abs ln.VectorZ
        if isTooSmall (x+y+z) then
            failTooSmall "Line3D.IsYAligned" ln
        x < axisAlignmentTolerance && z < axisAlignmentTolerance

    /// Checks if 3D line is parallel to the world Y axis. Ignoring orientation.
    /// The absolute deviation tolerance along X and Z axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    static member inline isYAligned (l:Line3D) : bool =
        l.IsYAligned

    /// Checks if 3D line is parallel to the world Z axis. Ignoring orientation.
    /// The absolute deviation tolerance along X, Y and Z  axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    /// Same as ln.IsVertical
    member inline ln.IsZAligned : bool =
        let x = abs ln.VectorX
        let y = abs ln.VectorY
        let z = abs ln.VectorZ
        if isTooSmall (x+y+z) then
            failTooSmall "Line3D.IsZAligned" ln
        x < axisAlignmentTolerance && y < axisAlignmentTolerance

    /// Checks if 3D line is parallel to the world Z axis. Ignoring orientation.
    /// The absolute deviation tolerance along X, Y and Z  axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    /// Same as ln.IsVertical
    static member inline isZAligned (l:Line3D) : bool =
        l.IsZAligned

    /// Checks if 3D line is parallel to the world Z axis. Ignoring orientation.
    /// The absolute deviation tolerance along X, Y and Z  axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    /// Same as ln.IsZAligned
    member inline ln.IsVertical : bool =
        let x = abs ln.VectorX
        let y = abs ln.VectorY
        let z = abs ln.VectorZ
        if isTooSmall (x+y+z) then
            failTooSmall "Line3D.IsVertical" ln
        x < axisAlignmentTolerance && y < axisAlignmentTolerance

    /// Checks if 3D line is parallel to the world Z axis. Ignoring orientation.
    /// The absolute deviation tolerance along X, Y and Z  axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    /// Same as ln.IsZAligned
    static member inline isVertical (l:Line3D) : bool =
        l.IsVertical

    /// Checks if 3D line is horizontal.
    /// The absolute deviation tolerance along Z axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    member inline ln.IsHorizontal : bool =
        let x = abs ln.VectorX
        let y = abs ln.VectorY
        let z = abs ln.VectorZ
        if isTooSmall (x+y+z) then
            failTooSmall "Line3D.IsHorizontal" ln
        z < axisAlignmentTolerance

    /// Checks if 3D line is horizontal (Z component is almost zero).
    /// The absolute deviation tolerance along Z axis is 1e-9 (axisAlignmentTolerance).
    /// Fails on lines shorter than 1e-6.
    static member inline isHorizontal (l:Line3D) : bool =
        l.IsHorizontal

    /// Check if the 3D line has exactly the same starting and ending point.
    member inline ln.IsZeroLength : bool =
        ln.ToX = ln.FromX &&
        ln.ToY = ln.FromY &&
        ln.ToZ = ln.FromZ

    /// Check if the line has same starting and ending point.
    static member inline isZeroLength (l:Line3D) : bool =
        l.IsZeroLength

    /// Returns TRUE if 3D line is shorter than tolerance.
    /// Or contains a NaN value
    member inline ln.IsTiny tol : bool =
        not (ln.Length > tol) // use not(..) to catch NaN too

    /// Returns TRUE if line is shorter than tolerance.
    /// Also checks if any component is a NaN.
    static member inline isTiny tol (l:Line3D) : bool =
        not (l.Length > tol) // use not(..) to catch NaN too

    /// Returns TRUE if 3D line is shorter than the squared tolerance.
    /// Or contains a NaN value
    member inline ln.IsTinySq tol : bool =
        not (ln.LengthSq > tol)

    /// Returns TRUE if the lines square length is shorter than squared tolerance.
    /// Also checks if any component is a NaN.
    static member inline isTinySq tol (l:Line3D) : bool =
        not (l.LengthSq > tol)

    /// Evaluate 3D line at a given parameter.
    /// Parameters 0.0 to 1.0 are on the line.
    member inline ln.EvaluateAt (p:float) : Pnt =
        Pnt(ln.FromX + ln.VectorX*p,
            ln.FromY + ln.VectorY*p,
            ln.FromZ + ln.VectorZ*p)

    /// Evaluate line at a given parameter (parameters 0.0 to 1.0 are on the line)
    static member inline evaluateAt t (ln:Line3D) : Pnt =
        ln.EvaluateAt t

    /// Evaluate line at a given parameters (parameters 0.0 to 1.0 are on the line),
    /// Return a new line from evaluated points.
    /// Same as ln.Segment(start, ende).
    member inline ln.SubLine (start:float, ende:float) : Line3D =
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

    /// Evaluate line at a given parameters (parameters 0.0 to 1.0 are on the line),
    /// Return a new line from evaluated points.
    /// Same as Line3D.segment.
    static member inline subLine start ende (ln:Line3D) : Line3D =
        ln.SubLine(start, ende)

    /// Returns the length of the line segment from the start point to the given parameter.
    /// This length is negative if the parameter is negative.
    member inline ln.LengthTillParam (p:float) : float =
        let x = ln.VectorX * p
        let y = ln.VectorY * p
        let z = ln.VectorZ * p
        let l = sqrt(x*x + y*y + z*z)
        if p> 0.0 then l else -l

    /// Returns the length of the line segment from the start point to the given parameter.
    /// This length is negative if the parameter is negative.
    static member inline lengthTillParam p (ln:Line3D) : float =
        ln.LengthTillParam p

    /// Returns the length of the line segment from the given parameter till the line End.
    /// This length is negative if the parameter is bigger than 1.0.
    member inline ln.LengthFromParam (t:float) : float =
        let p = 1.0-t
        let x = ln.VectorX * p
        let y = ln.VectorY * p
        let z = ln.VectorZ * p
        let l = sqrt(x*x + y*y + z*z)
        if p> 0.0 then l else -l

    /// Returns the length of the line segment from the given parameter till the line End.
    /// This length is negative if the parameter is bigger than 1.0.
    static member inline lengthFromParam t (ln:Line3D) : float =
        ln.LengthFromParam t

    /// Returns the midpoint of the 3D line,
    member inline ln.Mid : Pnt =
        Pnt((ln.ToX + ln.FromX)*0.5,
            (ln.ToY + ln.FromY)*0.5,
            (ln.ToZ + ln.FromZ)*0.5)

    /// Get point at center of line.
    static member inline mid (ln:Line3D) : Pnt =
        ln.Mid

    /// Returns the 3D line reversed.
    member inline ln.Reversed : Line3D =
        Line3D(ln.ToX, ln.ToY, ln.ToZ, ln.FromX, ln.FromY, ln.FromZ)

    /// Returns the 3D line reversed.
    static member inline reversed (ln:Line3D) : Line3D =
        ln.Reversed

    /// Returns a Line3D from point at Parameter a to point at Parameter b.
    /// Same as ln.SubLine(a, b).
    member inline ln.Segment(a, b) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        Line3D( ln.FromX + x*a,
                ln.FromY + y*a,
                ln.FromZ + z*a,
                ln.FromX + x*b,
                ln.FromY + y*b,
                ln.FromZ + z*b)

    /// Returns new 3D line from point at Parameter a to point at Parameter b.
    /// Same as Line3D.subLine.
    static member inline segment a b (ln:Line3D) : Line3D =
        ln.Segment (a, b)

    /// Extend 3D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.Extend (distAtStart:float, distAtEnd:float) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then failTooSmall "Line3D.Extend" ln
        Line3D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.FromZ - z*distAtStart/l,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l,
                ln.ToZ   + z*distAtEnd/l)

    /// Extend 3D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extend (distAtStart:float) (distAtEnd:float) (ln:Line3D) : Line3D =
        ln.Extend(distAtStart, distAtEnd)

    /// Extend 3D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendStart (distAtStart:float) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then failTooSmall "Line3D.ExtendStart" ln
        Line3D( ln.FromX - x*distAtStart/l,
                ln.FromY - y*distAtStart/l,
                ln.FromZ - z*distAtStart/l,
                ln.ToX,
                ln.ToY,
                ln.ToZ)

    /// Extend 3D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendStart (distAtStart:float) (ln:Line3D) : Line3D =
        ln.ExtendStart(distAtStart)

    /// Extend 3D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendEnd (distAtEnd:float) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then failTooSmall "Line3D.ExtendEnd" ln
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ,
                ln.ToX   + x*distAtEnd/l,
                ln.ToY   + y*distAtEnd/l,
                ln.ToZ   + z*distAtEnd/l)

    /// Extend 3D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendEnd (distAtEnd:float) (ln:Line3D) : Line3D =
        ln.ExtendEnd(distAtEnd)

    /// Extend 3D line by relative amount at start and end.
    /// A relative amount of 0.5 extends the line by half its length on each side.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendRel (relAtStart:float, relAtEnd:float) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then failTooSmall "Line3D.ExtendRel" ln
        Line3D( ln.FromX - x*relAtStart,
                ln.FromY - y*relAtStart,
                ln.FromZ - z*relAtStart,
                ln.ToX   + x*relAtEnd,
                ln.ToY   + y*relAtEnd,
                ln.ToZ   + z*relAtEnd)

    /// Extend 3D line by relative amount at start and end.
    /// A relative amount of 0.5 extends the line by half its length on each side.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendRel (relAtStart:float) (relAtEnd:float) (ln:Line3D) : Line3D =
        ln.ExtendRel(relAtStart, relAtEnd)

    /// Extend 3D line by relative amount at start.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendStartRel (relAtStart:float) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then failTooSmall "Line3D.ExtendStartRel" ln
        Line3D( ln.FromX - x*relAtStart,
                ln.FromY - y*relAtStart,
                ln.FromZ - z*relAtStart,
                ln.ToX,
                ln.ToY,
                ln.ToZ)

    /// Extend 3D line by relative amount at start.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendStartRel (relAtStart:float) (ln:Line3D) : Line3D =
        ln.ExtendStartRel(relAtStart)

    /// Extend 3D line by relative amount at end.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ExtendEndRel (relAtEnd:float) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then failTooSmall "Line3D.ExtendEndRel" ln
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ,
                ln.ToX   + x*relAtEnd,
                ln.ToY   + y*relAtEnd,
                ln.ToZ   + z*relAtEnd)

    /// Extend 3D line by relative amount at end.
    /// A relative amount of 0.5 extends the line by half its length.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline extendEndRel (relAtEnd:float) (ln:Line3D) : Line3D =
        ln.ExtendEndRel(relAtEnd)

    /// Shrink 3D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.Shrink (distAtStart:float, distAtEnd:float) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then failTooSmall "Line3D.Shrink" ln
        Line3D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.FromZ + z*distAtStart/l,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l,
                ln.ToZ   - z*distAtEnd/l)

    /// Shrink 3D line by absolute amount at start and end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrink (distAtStart:float) (distAtEnd:float) (ln:Line3D) : Line3D =
        ln.Shrink(distAtStart, distAtEnd)

    /// Shrink 3D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ShrinkStart (distAtStart:float) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then failTooSmall "Line3D.ShrinkStart" ln
        Line3D( ln.FromX + x*distAtStart/l,
                ln.FromY + y*distAtStart/l,
                ln.FromZ + z*distAtStart/l,
                ln.ToX,
                ln.ToY,
                ln.ToZ)

    /// Shrink 3D line by absolute amount at start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrinkStart (distAtStart:float) (ln:Line3D) : Line3D =
        ln.ShrinkStart(distAtStart)

    /// Shrink 3D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.ShrinkEnd (distAtEnd:float) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x*x + y*y + z*z)
        if isTooTiny l then failTooSmall "Line3D.ShrinkEnd" ln
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ,
                ln.ToX   - x*distAtEnd/l,
                ln.ToY   - y*distAtEnd/l,
                ln.ToZ   - z*distAtEnd/l)

    /// Shrink 3D line by absolute amount at end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline shrinkEnd (distAtEnd:float) (ln:Line3D) : Line3D =
        ln.ShrinkEnd(distAtEnd)

    /// Returns a Line3D moved by a vector. Same as Line3D.move and Line3D.translate.
    member inline ln.Move (v:Vec) : Line3D =
        Line3D( ln.FromX + v.X,
                ln.FromY + v.Y,
                ln.FromZ + v.Z,
                ln.ToX   + v.X,
                ln.ToY   + v.Y,
                ln.ToZ   + v.Z)

    /// Move a 3D line by a vector. (same as Line3D.translate)
    static member inline move (v:Vec) (ln:Line3D)  : Line3D =
        ln.Move(v)

    /// Returns a Line3D moved by a given distance in X direction.
    member inline ln.MoveX (distance:float) : Line3D =
        Line3D( ln.FromX + distance,
                ln.FromY,
                ln.FromZ,
                ln.ToX  + distance,
                ln.ToY,
                ln.ToZ)

    /// Returns a 3D line moved by a given distance in X direction.
    static member inline moveX (distance:float) (ln:Line3D)  : Line3D =
        ln.MoveX(distance)

    /// Returns a Line3D moved by a given distance in Y direction.
    member inline ln.MoveY (distance:float) : Line3D =
        Line3D( ln.FromX,
                ln.FromY + distance,
                ln.FromZ,
                ln.ToX,
                ln.ToY + distance,
                ln.ToZ)

    /// Returns a 3D line moved by a given distance in Y direction.
    static member inline moveY (distance:float) (ln:Line3D)  : Line3D =
        ln.MoveY(distance)

    /// Returns a Line3D moved by a given distance in Z direction.
    member inline ln.MoveZ (distance:float) : Line3D =
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ + distance,
                ln.ToX,
                ln.ToY,
                ln.ToZ + distance)

    /// Returns a 3D line moved by a given distance in Z direction.
    static member inline moveZ (distance:float) (ln:Line3D)  : Line3D =
        ln.MoveZ(distance)

    /// Assumes Line3D to be an infinite ray.
    /// Returns the parameter at which a point is closest to the ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Fails on curves shorter than 1e-6 units. (ln.ClosestParameter does not)
    member inline ln.RayClosestParameterXYZ (x:float, y:float, z:float) : float =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let vx = ln.FromX - ln.ToX
        let vy = ln.FromY - ln.ToY
        let vz = ln.FromZ - ln.ToZ
        let lenSq = vx*vx + vy*vy + vz*vz
        if isTooSmallSq(lenSq) then // the parameter is infinite so we have to fail
            failTooSmall2 "Line3D.RayClosestParameter" ln (Pnt(x, y, z))
        let u = ln.FromX - x
        let v = ln.FromY - y
        let w = ln.FromZ - z
        let dot = vx*u + vy*v + vz*w
        dot / lenSq

    /// Assumes Line3D to be an infinite ray.
    /// Returns the parameter at which a point is closest to the ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Fails on curves shorter than 1e-6 units. (ln.ClosestParameter does not)
    member inline ln.RayClosestParameter (p:Pnt) : float =
        ln.RayClosestParameterXYZ (p.X, p.Y, p.Z)

    /// Assumes Line3D to be an infinite ray.
    /// Returns the parameter at which a point is closest to the ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    static member inline rayClosestParameterXYZ (x:float) (y:float) (z:float) (ln:Line3D) : float =
        ln.RayClosestParameterXYZ (x, y, z)

    /// Assumes Line3D to be an infinite ray.
    /// Returns the parameter at which a point is closest to the ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    static member inline rayClosestParameter (p:Pnt) (ln:Line3D) : float =
        ln.RayClosestParameter p

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    /// Does not fail on very short curves.
    member inline ln.ClosestParameterXYZ (x:float, y:float, z:float) : float =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let vx = ln.FromX - ln.ToX
        let vy = ln.FromY - ln.ToY
        let vz = ln.FromZ - ln.ToZ
        let u = ln.FromX - x
        let v = ln.FromY - y
        let w = ln.FromZ - z
        let dot = vx*u + vy*v + vz*w
        let lenSq = vx*vx + vy*vy + vz*vz
        if isTooSmallSq(lenSq) then // the parameter is infinite but we ar constarined to 0.0 till 1.0
            if dot < 0.0 then 0.0 else 1.0
        else
            dot / lenSq |> UtilEuclid.clamp01

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    /// Does not fail on very short curves.
    member inline ln.ClosestParameter (p:Pnt) : float =
        ln.ClosestParameterXYZ (p.X, p.Y, p.Z)

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameterXYZ (x:float) (y:float) (z:float) (ln:Line3D) : float =
        ln.ClosestParameterXYZ (x, y, z)

    /// Returns the parameter at which a point is closest to the (finite) line.
    /// The result is between 0.0 and 1.0.
    static member inline closestParameter (p:Pnt) (ln:Line3D) : float =
        ln.ClosestParameter p

    /// Assumes Line3D to be an infinite ray.
    /// Returns closest point on ray.
    /// Fails on curves shorter than 1e-6 units. (ln.ClosestPoint does not.)
    member inline ln.RayClosestPoint (p:Pnt) : Pnt =
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let x = ln.FromX - ln.ToX
        let y = ln.FromY - ln.ToY
        let z = ln.FromZ - ln.ToZ
        let lenSq = x*x + y*y + z*z
        if isTooSmallSq(lenSq) then // the parameter is infinite so we have to fail
            failTooSmall2 "Line3D.RayClosestPoint" ln p
        let u = ln.FromX-p.X
        let v = ln.FromY-p.Y
        let w = ln.FromZ-p.Z
        let dot = x*u + y*v + z*w
        let t = dot/lenSq
        let x' = ln.FromX - x*t
        let y' = ln.FromY - y*t
        let z' = ln.FromZ - z*t
        Pnt(x', y', z')

    /// Assumes Line3D to be an infinite ray.
    /// Returns closest point on ray.
    static member inline rayClosestPoint (p:Pnt) (ln:Line3D) : Pnt =
        ln.RayClosestPoint p

    /// Returns closest point on (finite) line.
    /// Does not fail on very short curves.
    member inline ln.ClosestPoint (p:Pnt) : Pnt =
        ln.EvaluateAt(ln.ClosestParameter(p))

    /// Returns closest point on (finite) line.
    static member inline closestPoint (p:Pnt) (ln:Line3D) : Pnt =
        ln.ClosestPoint p

    /// <summary>Finds the closest points between two finite 3D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnB">The second line.</param>
    /// <remarks>For parallel and overlapping lines the points returned are in the center of their overlap.
    /// If the lines intersect the returned points are exactly the same.
    /// For skew lines, returns the two closest points on each line.
    /// Lines shorter than 1e-6 are considered too short.
    /// Lines with an angle less than 0.25 degrees are considered parallel.
    /// For more control over tolerances, use <see cref="XLine3D.getClosestPoints"/>.</remarks>
    member lnA.ClosestPoints (lnB:Line3D) : Pnt * Pnt =
        match XLine3D.getClosestPoints(lnA, lnB) with
        | XLine3D.ClPts.Intersect p       -> p, p
        | XLine3D.ClPts.Skew (pA, pB, _)  -> pA, pB
        | XLine3D.ClPts.Parallel (pA, pB) -> pA, pB
        | XLine3D.ClPts.Apart (pA, pB, _) -> pA, pB
        | XLine3D.ClPts.TooShortBoth      -> lnA.From, lnB.From
        | XLine3D.ClPts.TooShortA         -> lnA.From, XLineXYZ.clPtLn'(lnB, lnA.FromX, lnA.FromY, lnA.FromZ)
        | XLine3D.ClPts.TooShortB         -> XLineXYZ.clPtLn'(lnA, lnB.FromX, lnB.FromY, lnB.FromZ), lnB.From

    /// <summary>Finds the closest points between two finite 3D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnA">The first line.</param>
    /// <param name="lnB">The second line.</param>
    /// <remarks>For parallel and overlapping lines the points returned are in the center of their overlap.
    /// If the lines intersect the returned points are exactly the same.
    /// For skew lines, returns the two closest points on each line.</remarks>
    static member closestPoints (lnA:Line3D) (lnB:Line3D) : Pnt * Pnt =
        lnA.ClosestPoints lnB

    /// <summary>Finds the parameters of closest points between two finite 3D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnB">The second line.</param>
    /// <remarks>For parallel and overlapping lines the parameters returned are in the center of their overlap.
    /// If the lines intersect the returned parameters are for the intersection point.
    /// For skew lines, returns the parameters at the closest approach.
    /// Lines shorter than 1e-6 are considered too short.
    /// Lines with an angle less than 0.25 degrees are considered parallel.
    /// For more control over tolerances, use <see cref="XLine3D.getClosestParameters"/>.</remarks>
    /// <returns>A tuple of two floats, the first is the parameter on lnA, the second on lnB.</returns>
    member lnA.ClosestParameters (lnB:Line3D) : float * float =
        match XLine3D.getClosestParameters(lnA, lnB) with
        | XLine3D.ClParams.Intersect (tA, tB)   -> tA, tB
        | XLine3D.ClParams.Skew (tA, tB, _)     -> tA, tB
        | XLine3D.ClParams.Parallel (tA, tB)    -> tA, tB
        | XLine3D.ClParams.Apart (tA, tB, _)    -> tA, tB
        | XLine3D.ClParams.TooShortBoth         -> 0.0, 0.0
        | XLine3D.ClParams.TooShortA            -> 0.0, XLineXYZ.clParamLnPt(lnB, lnA.FromX, lnA.FromY, lnA.FromZ)
        | XLine3D.ClParams.TooShortB            -> XLineXYZ.clParamLnPt(lnA, lnB.FromX, lnB.FromY, lnB.FromZ), 0.0

    /// <summary>Finds the parameters of closest points between two finite 3D Lines, also works on parallel and overlapping lines.</summary>
    /// <param name="lnA">The first line.</param>
    /// <param name="lnB">The second line.</param>
    /// <remarks>For parallel and overlapping lines the parameters returned are in the center of their overlap.
    /// If the lines intersect the returned parameters are for the intersection point.
    /// For skew lines, returns the parameters at the closest approach.</remarks>
    /// <returns>A tuple of two floats, the first is the parameter on lnA, the second on lnB.</returns>
    static member closestParameters (lnA:Line3D) (lnB:Line3D) : float * float =
        lnA.ClosestParameters lnB

    /// <summary>Finds the closest approach parameters of two infinite rays (Line3D treated as rays).</summary>
    /// <param name="lnB">The second line (treated as infinite ray).</param>
    /// <remarks>Lines shorter than 1e-6 are considered too short.
    /// Lines with an angle less than 0.25 degrees are considered parallel.
    /// For more control over tolerances, use <see cref="XLine3D.getRayClosestParam"/>.</remarks>
    /// <returns>Some tuple of two floats (parameter on this, parameter on lnB) if rays are not parallel and not too short, otherwise None.</returns>
    member lnA.RayClosestParameters (lnB:Line3D) : (float * float) option =
        match XLine3D.getRayClosestParam(lnA, lnB) with
        | XLine3D.XRayParam.SkewOrX (t, u) -> Some (t, u)
        | XLine3D.XRayParam.Parallel | XLine3D.XRayParam.TooShortA | XLine3D.XRayParam.TooShortB | XLine3D.XRayParam.TooShortBoth -> None

    /// <summary>Finds the closest approach parameters of two infinite rays (Line3D treated as rays).</summary>
    /// <param name="lnA">The first line (treated as infinite ray).</param>
    /// <param name="lnB">The second line (treated as infinite ray).</param>
    /// <returns>Some tuple of two floats (parameter on lnA, parameter on lnB) if rays are not parallel and not too short, otherwise None.</returns>
    static member rayClosestParameters (lnA:Line3D) (lnB:Line3D) : (float * float) option =
        lnA.RayClosestParameters lnB

    /// <summary>Finds the closest approach points of two infinite rays (Line3D treated as rays).</summary>
    /// <param name="lnB">The second line (treated as infinite ray).</param>
    /// <remarks>Lines shorter than 1e-6 are considered too short.
    /// Lines with an angle less than 0.25 degrees are considered parallel.
    /// For more control over tolerances, use <see cref="XLine3D.getRayClosestParam"/>.</remarks>
    /// <returns>Some tuple of two points (closest point on this, closest point on lnB) if rays are not parallel and not too short, otherwise None.</returns>
    member lnA.RayClosestPoints (lnB:Line3D) : (Pnt * Pnt) option =
        match XLine3D.getRayClosestParam(lnA, lnB) with
        | XLine3D.XRayParam.SkewOrX (t, u) -> Some (lnA.EvaluateAt t, lnB.EvaluateAt u)
        | XLine3D.XRayParam.Parallel | XLine3D.XRayParam.TooShortA | XLine3D.XRayParam.TooShortB | XLine3D.XRayParam.TooShortBoth -> None

    /// <summary>Finds the closest approach points of two infinite rays (Line3D treated as rays).</summary>
    /// <param name="lnA">The first line (treated as infinite ray).</param>
    /// <param name="lnB">The second line (treated as infinite ray).</param>
    /// <returns>Some tuple of two points (closest point on lnA, closest point on lnB) if rays are not parallel and not too short, otherwise None.</returns>
    static member rayClosestPoints (lnA:Line3D) (lnB:Line3D) : (Pnt * Pnt) option =
        lnA.RayClosestPoints lnB

    /// Assumes Line3D to be an infinite ray.
    /// Returns square distance from point to ray.
    /// Fails on curves shorter than 1e-6 units. (ln.SqDistanceFromPoint does not.)
    member ln.SqDistanceRayXYZ(x:float, y:float, z:float) : float =
        let lnFromX = ln.FromX
        let lnFromY = ln.FromY
        let lnFromZ = ln.FromZ
        //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        let vx = lnFromX - ln.ToX
        let vy = lnFromY - ln.ToY
        let vz = lnFromZ - ln.ToZ
        let lenSq = vx*vx + vy*vy + vz*vz
        if isTooSmallSq lenSq  then // corresponds to a line Length of 1e-6
            failTooSmall2 "Line3D.SqDistanceRayPoint" ln (Pnt(x, y, z))
        let u = lnFromX - x
        let v = lnFromY - y
        let w = lnFromZ - z
        let dot = vx*u + vy*v + vz*w
        let t = dot/lenSq
        let x' = lnFromX - vx*t
        let y' = lnFromY - vy*t
        let z' = lnFromZ - vz*t
        let u' = x' - x
        let v' = y' - y
        let w' = z' - z
        u'*u' + v'*v' + w'*w'

    /// Assumes Line3D to be an infinite ray.
    /// Returns square distance from point to ray.
    /// Fails on curves shorter than 1e-6 units. (ln.SqDistanceFromPoint does not.)
    member ln.SqDistanceRayPoint(p:Pnt) : float =
        ln.SqDistanceRayXYZ(p.X, p.Y, p.Z)

    /// Assumes Line3D to be an infinite ray.
    /// Returns the square distance from point to ray.
    static member inline sqDistanceRayXYZ(x:float) (y:float) (z:float) (ln:Line3D) : float =
        ln.SqDistanceRayXYZ (x, y, z)

    /// Assumes Line3D to be an infinite ray.
    /// Returns the square distance from point to ray.
    static member inline sqDistanceRayPoint(p:Pnt) (ln:Line3D) : float =
        ln.SqDistanceRayPoint p

    /// Assumes Line3D to be an infinite ray.
    /// Returns distance from point to ray.
    /// Fails on curves shorter than 1e-6 units. (ln.DistanceToPnt does not.)
    member inline ln.DistanceRayXYZ(x:float, y:float, z:float) : float =
        ln.SqDistanceRayXYZ(x, y, z) |> sqrt

    /// Assumes Line3D to be an infinite ray.
    /// Returns distance from point to ray.
    /// Fails on curves shorter than 1e-6 units. (ln.DistanceToPnt does not.)
    member inline ln.DistanceRayPoint(p:Pnt) : float =
        ln.DistanceRayXYZ(p.X, p.Y, p.Z)

    /// Assumes Line3D to be an infinite ray.
    /// Returns distance from point to ray.
    static member inline distanceRayXYZ(x:float) (y:float) (z:float) (ln:Line3D) : float =
        ln.DistanceRayXYZ (x, y, z)

    /// Assumes Line3D to be an infinite ray.
    /// Returns distance from point to ray.
    static member inline distanceRayPoint(p:Pnt) (ln:Line3D) : float =
        ln.DistanceRayPoint p

    /// Returns square distance from point to finite line.
    member inline ln.SqDistanceFromXYZ(x:float, y:float, z:float) : float =
        ln.ClosestParameterXYZ(x, y, z)
        |> ln.EvaluateAt
        |> Pnt.sqDist (Pnt(x, y, z))

    /// Returns square distance from point to finite line.
    member inline ln.SqDistanceFromPoint(p:Pnt) : float =
        ln.SqDistanceFromXYZ(p.X, p.Y, p.Z)

    /// Returns the square distance from point to (finite) line.
    static member inline sqDistanceFromXYZ(x:float) (y:float) (z:float) (ln:Line3D) : float =
        ln.SqDistanceFromXYZ (x, y, z)

    /// Returns the square distance from point to (finite) line.
    static member inline sqDistanceFromPoint(p:Pnt) (ln:Line3D) : float =
        ln.SqDistanceFromPoint p

    /// Returns distance from point to (finite) line.
    member inline ln.DistanceToXYZ(x:float, y:float, z:float) : float =
        ln.SqDistanceFromXYZ(x, y, z) |> sqrt

    /// Returns distance from point to (finite) line.
    member inline ln.DistanceToPnt(p:Pnt) : float =
        ln.DistanceToXYZ(p.X, p.Y, p.Z)

    /// Returns distance from point to (finite) line.
    static member inline distanceToXYZ(x:float) (y:float) (z:float) (ln:Line3D) : float =
        ln.DistanceToXYZ (x, y, z)

    /// Returns distance from point to (finite) line.
    static member inline distanceToPnt(p:Pnt) (ln:Line3D) : float =
        ln.DistanceToPnt p

        /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line3D.
    member inline ln.Direction : Vec =
        Vec(ln.VectorX, ln.VectorY, ln.VectorZ)

    /// Same as ln.Vector or ln.Tangent.
    /// The returned vector has the same length as the Line3D.
    static member inline direction (ln:Line3D) : Vec =
        ln.Direction

    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line3D.
    member inline ln.Tangent : Vec =
        Vec(ln.VectorX, ln.VectorY, ln.VectorZ)

    /// Same as ln.Vector or ln.Direction.
    /// The returned vector has the same length as the Line3D.
    static member inline tangent (ln:Line3D) : Vec =
        ln.Tangent

    /// Returns a unit-vector of the line Direction.
    member inline ln.UnitTangent : UnitVec =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = sqrt(x * x  + y * y + z * z)
        if UtilEuclid.isTooTiny l then
            EuclidErrors.failUnit3 "Line3D.UnitTangent" x y z
        let s = 1.0 / l
        UnitVec.createUnchecked (x*s, y*s, z*s)

    /// Returns a unit-vector of the line Direction.
    static member inline unitTangent (ln:Line3D) : UnitVec =
        ln.UnitTangent

    /// Multiplies (or applies) a Quaternion to a 3D line .
    /// The resulting line has the same length as the input.
    member inline l.Rotate (q:Quaternion) : Line3D =
        // adapted from https://github.com/mrdoob/three.js/blob/dev/src/math/Vector3.js
        let u = l.FromX
        let v = l.FromY
        let w = l.FromZ
        let x = l.ToX
        let y = l.ToY
        let z = l.ToZ
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let tu = 2.0 * ( qy * w - qz * v)
        let tv = 2.0 * ( qz * u - qx * w)
        let tw = 2.0 * ( qx * v - qy * u)
        let tx = 2.0 * ( qy * z - qz * y)
        let ty = 2.0 * ( qz * x - qx * z)
        let tz = 2.0 * ( qx * y - qy * x)
        Line3D( u + qw * tu + qy * tw - qz * tv ,
                v + qw * tv + qz * tu - qx * tw ,
                w + qw * tw + qx * tv - qy * tu ,
                x + qw * tx + qy * tz - qz * ty ,
                y + qw * ty + qz * tx - qx * tz ,
                z + qw * tz + qx * ty - qy * tx )

    /// Multiplies (or applies) a Quaternion to a 3D line .
    /// The resulting line has the same length as the input.
    static member inline rotate (q:Quaternion) (ln:Line3D) : Line3D =
        ln.Rotate(q)

    /// Multiplies (or applies) a Quaternion to a 3D line around a given center point.
    /// The resulting line has the same length as the input.
    member inline l.RotateWithCenter (cen:Pnt, q:Quaternion) : Line3D =
        let cX = cen.X
        let cY = cen.Y
        let cZ = cen.Z
        let u = l.FromX - cX
        let v = l.FromY - cY
        let w = l.FromZ - cZ
        let x = l.ToX   - cX
        let y = l.ToY   - cY
        let z = l.ToZ   - cZ
        let qx = q.X
        let qy = q.Y
        let qz = q.Z
        let qw = q.W
        let tu = 2.0 * ( qy * w - qz * v)
        let tv = 2.0 * ( qz * u - qx * w)
        let tw = 2.0 * ( qx * v - qy * u)
        let tx = 2.0 * ( qy * z - qz * y)
        let ty = 2.0 * ( qz * x - qx * z)
        let tz = 2.0 * ( qx * y - qy * x)
        Line3D( u + qw * tu + qy * tw - qz * tv + cX ,
                v + qw * tv + qz * tu - qx * tw + cY,
                w + qw * tw + qx * tv - qy * tu + cZ ,
                x + qw * tx + qy * tz - qz * ty + cX ,
                y + qw * ty + qz * tx - qx * tz + cY ,
                z + qw * tz + qx * ty - qy * tx + cZ )

    /// Multiplies (or applies) a Quaternion to a 3D line around a given center point.
    /// The resulting line has the same length as the input.
    static member inline rotateWithCenter (cen:Pnt) (q:Quaternion) (ln:Line3D) : Line3D =
        ln.RotateWithCenter (cen, q)

    /// Checks if the dot product between the two 3D lines is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation (otherLn:Line3D) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        let bx = otherLn.VectorX
        let by = otherLn.VectorY
        let bz = otherLn.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.MatchesOrientation this" ln otherLn
        if isTooTinySq(bx*bx + by*by + bz*bz) then failTooSmall2 "Line3D.MatchesOrientation other" otherLn ln
        ax*bx + ay*by + az*bz > 1e-12

    /// Checks if the dot product between the two 3D lines is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientation (l:Line3D) (ln:Line3D) : bool =
        l.MatchesOrientation ln

    /// Checks if the dot product between a 3D line and a vector is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation (v:Vec) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        if isTooTinySq(v.LengthSq)   then failTooSmall2 "Line3D.MatchesOrientation this" v ln
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.MatchesOrientation other" ln v
        ax*v.X + ay*v.Y + az*v.Z > 1e-12

    /// Checks if the dot product between a 3D line and a unit-vector is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation (v:UnitVec) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.MatchesOrientation other" ln v
        ax*v.X + ay*v.Y + az*v.Z > 1e-12

    /// Checks if the angle between the two 3D lines is less than 45 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.MatchesOrientation45 (l:Line3D) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        let bx = l.VectorX
        let by = l.VectorY
        let bz = l.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.MatchesOrientation45 this" ln l
        if isTooTinySq(bx*bx + by*by + bz*bz) then failTooSmall2 "Line3D.MatchesOrientation45 other" l ln
        let tan = XLineXYZ.tangent (ax, ay, az, bx, by, bz)
        tan < Tangent.``45.0``

    /// Checks if the angle between the two 3D lines is less than 45 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientation45 (l:Line3D) (ln:Line3D) : bool =
        l.MatchesOrientation45 ln

    /// Checks if two 3D lines are parallel.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:Line3D, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        let bx = other.VectorX
        let by = other.VectorY
        let bz = other.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsParallelTo this" ln other
        if isTooTinySq(bx*bx + by*by + bz*bz) then failTooSmall2 "Line3D.IsParallelTo other" other ln
        let tan = XLineXYZ.tangent (ax, ay, az, bx, by, bz)
        abs tan < minTangent

    /// Checks if two 3D lines are parallel. Ignoring orientation.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelTo (minTangent:float<Tangent.tangent>) (lnA:Line3D) (lnB:Line3D) : bool =
        lnA.IsParallelTo(lnB, minTangent)

    /// Checks if a 3D lines is parallel to a 3D vector.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:Vec, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsParallelTo" ln other
        if isTooTinySq(other.LengthSq) then failTooSmall2 "Line3D.IsParallelTo" other ln
        let tan = XLineXYZ.tangent (ax, ay, az, other.X, other.Y, other.Z)
        abs tan < minTangent

    /// Checks if a 3D lines is parallel to a 3D unit-vector.
    /// Ignores the line orientation.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelTo( other:UnitVec, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsParallelTo" ln other
        let tan = XLineXYZ.tangent (ax, ay, az, other.X, other.Y, other.Z)
        abs tan < minTangent

    /// Checks if two 3D lines are parallel.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees.Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:Line3D, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        let bx = other.VectorX
        let by = other.VectorY
        let bz = other.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsParallelAndOrientedTo this" ln other
        if isTooTinySq(bx*bx + by*by + bz*bz) then failTooSmall2 "Line3D.IsParallelAndOrientedTo other" other ln
        let tan = XLineXYZ.tangent (ax, ay, az, bx, by, bz)
        tan < minTangent

    /// Checks if two 3D lines are parallel and orientated the same way.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelAndOrientedTo (minTangent:float<Tangent.tangent>) (lnA:Line3D) (lnB:Line3D) : bool =
        lnA.IsParallelAndOrientedTo(lnB, minTangent)

    /// Checks if a 3D lines is parallel to a 3D vector.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:Vec, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsParallelAndOrientedTo" ln other
        if isTooTinySq(other.LengthSq) then failTooSmall2 "Line3D.IsParallelAndOrientedTo" other ln
        let tan = XLineXYZ.tangent (ax, ay, az, other.X, other.Y, other.Z)
        tan < minTangent

    /// Checks if a 3D lines is parallel to a 3D unit-vector.
    /// Takes the line orientation into account too.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsParallelAndOrientedTo (other:UnitVec, [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsParallelAndOrientedTo" ln other
        let tan = XLineXYZ.tangent (ax, ay, az, other.X, other.Y, other.Z)
        tan < minTangent

    /// Checks if two 3D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:Line3D, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        let bx = other.VectorX
        let by = other.VectorY
        let bz = other.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsPerpendicularTo this" ln other
        if isTooTinySq(bx*bx + by*by + bz*bz) then failTooSmall2 "Line3D.IsPerpendicularTo other" other ln
        let tan = XLineXYZ.tangent (ax, ay, az, bx, by, bz)
        abs tan > maxTangent

    /// Checks if two 3D lines are perpendicular to each other.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75`` is about 229.2.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as Line3D.isNormalTo
    static member inline isPerpendicularTo (maxTangent:float<Tangent.tangent>) (lnA:Line3D) (lnB:Line3D) : bool =
        lnA.IsPerpendicularTo(lnB, maxTangent)

    /// Checks if a 3D lines is perpendicular to a 3D vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:Vec, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsPerpendicularTo this" ln other
        if isTooTinySq(other.LengthSq) then failTooSmall2 "Line3D.IsPerpendicularTo other" other ln
        let tan = XLineXYZ.tangent (ax, ay, az, other.X, other.Y, other.Z)
        abs tan > maxTangent

    /// Checks if a 3D lines is perpendicular to a 3D unit-vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    member inline ln.IsPerpendicularTo (other:UnitVec, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsPerpendicularTo this" ln other
        let tan = XLineXYZ.tangent (ax, ay, az, other.X, other.Y, other.Z)
        abs tan > maxTangent

    /// Checks if two 3D lines are perpendicular to each other.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Alias for ln.IsPerpendicularTo.
    member inline ln.IsNormalTo (other:Line3D, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if two 3D lines are perpendicular to each other.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75`` is about 229.2.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Same as Line3D.isPerpendicularTo
    static member inline isNormalTo (maxTangent:float<Tangent.tangent>) (lnA:Line3D) (lnB:Line3D) : bool =
        lnA.IsNormalTo(lnB, maxTangent)

    /// Checks if a 3D lines is perpendicular to a 3D vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Alias for ln.IsPerpendicularTo.
    member inline ln.IsNormalTo (other:Vec, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if a 3D lines is perpendicular to a 3D unit-vector.
    /// The default angle tolerance is 89.75 to 90.25 degrees. Tangent.``89.75`` is about 229.2.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// Alias for ln.IsPerpendicularTo.
    member inline ln.IsNormalTo (other:UnitVec, [<OPT;DEF(Tangent.``89.75``)>] maxTangent:float<Tangent.tangent> ) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if two 3D lines are coincident within the distance tolerance. 1e-6 by default.
    /// This means that lines are parallel within the angle tolerance.
    /// and the distance of second start to the first line is less than the distance tolerance.
    /// Also returns false on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    member inline ln.IsCoincidentTo (other:Line3D,
                                    [<OPT;DEF(1e-6)>] distanceTolerance:float,
                                    [<OPT;DEF(Tangent.``0.25``)>] minTangent:float<Tangent.tangent>) : bool =
        let ax = ln.VectorX
        let ay = ln.VectorY
        let az = ln.VectorZ
        let bx = other.VectorX
        let by = other.VectorY
        let bz = other.VectorZ
        if isTooTinySq(ax*ax + ay*ay + az*az) then failTooSmall2 "Line3D.IsCoincidentTo this" ln other
        if isTooTinySq(bx*bx + by*by + bz*bz) then failTooSmall2 "Line3D.IsCoincidentTo other" other ln
        let tan = XLineXYZ.tangent (ax, ay, az, bx, by, bz)
        if abs tan < minTangent then
            // they are parallel, now check distance:
            let pX = other.FromX
            let pY = other.FromY
            let pZ = other.FromZ
            //http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
            let x = ax
            let y = ay
            let z = az
            let u = ln.FromX - pX
            let v = ln.FromY - pY
            let w = ln.FromZ - pZ
            let dot = x*u + y*v + z*w
            let sa = ax*ax + ay*ay + az*az
            let t = dot/sa
            let x' = ln.FromX - x*t
            let y' = ln.FromY - y*t
            let z' = ln.FromZ - z*t
            let u' = x' - pX
            let v' = y' - pY
            let w' = z' - pZ
            u'*u' + v'*v' + w'*w' < distanceTolerance * distanceTolerance
        else
            // they are not parallel within tolerance
            false

    /// Checks if two 3D lines are coincident within tolerance.
    /// This means that lines are parallel within the given angular tolerance
    /// and the distance of second start point to the first line is less than distanceTolerance.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    static member inline isCoincidentTo distanceTolerance (minTangent:float<Tangent.tangent>) (a:Line3D) (b:Line3D) : bool =
        a.IsCoincidentTo(b, distanceTolerance, minTangent)

    /// A fast version of IsParallelTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are parallel within a given tolerance for their cross product magnitude.
    /// Lines are parallel if cross product magnitude squared is less than squaredParallelogramAreaTolerance.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// ATTENTION this method returns TRUE on zero length or very small lines.
    /// It returns FALSE on almost parallel lines that are very long.
    /// Pick a bigger squaredParallelogramAreaTolerance for longer lines.
    member inline ln.IsParallelToFast( other:Line3D, [<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float ) : bool =
        // Get direction vectors
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d1z = ln.VectorZ
        let d2x = other.VectorX
        let d2y = other.VectorY
        let d2z = other.VectorZ
        // Cross product in 3D
        let cx = d1y * d2z - d1z * d2y
        let cy = d1z * d2x - d1x * d2z
        let cz = d1x * d2y - d1y * d2x
        // Lines are parallel if cross product magnitude is near zero
        let crossMagSq = cx*cx + cy*cy + cz*cz
        crossMagSq < squaredParallelogramAreaTolerance

    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// ATTENTION this method returns TRUE on zero length or very small lines.
    /// It returns FALSE on almost parallel lines that are very long.
    /// Pick a bigger squaredParallelogramAreaTolerance for longer lines.
    static member inline isParallelToFast ([<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float) other (ln:Line3D) : bool =
        ln.IsParallelToFast(other, squaredParallelogramAreaTolerance)

    /// A fast version of IsCoincidentTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are coincident within a given tolerance for their cross product magnitude.
    /// Lines are coincident if they are parallel (cross product magnitude squared less than squaredParallelogramAreaTolerance)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// ATTENTION this method returns TRUE on zero length or very small lines.
    /// It returns FALSE on almost coincident lines that are very long.
    /// Pick a bigger squaredParallelogramAreaTolerance for longer lines.
    member inline ln.IsCoincidentToFast ( other:Line3D, [<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float) : bool =
        // First check if parallel
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d1z = ln.VectorZ
        let d2x = other.VectorX
        let d2y = other.VectorY
        let d2z = other.VectorZ
        let cx = d1y * d2z - d1z * d2y
        let cy = d1z * d2x - d1x * d2z
        let cz = d1x * d2y - d1y * d2x
        let crossMagSq = cx*cx + cy*cy + cz*cz
        if crossMagSq < squaredParallelogramAreaTolerance then
            // Then check vector between line starts is also parallel to the lines
            let px = ln.FromX - other.FromX
            let py = ln.FromY - other.FromY
            let pz = ln.FromZ - other.FromZ
            let cx = py * d2z - pz * d2y
            let cy = pz * d2x - px * d2z
            let cz = px * d2y - py * d2x
            let crossMagSq = cx*cx + cy*cy + cz*cz
            crossMagSq < squaredParallelogramAreaTolerance
        else
            false

    /// A fast version of IsCoincidentTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are coincident within a given tolerance for their cross product magnitude.
    /// Lines are coincident if they are parallel (cross product magnitude squared less than squaredParallelogramAreaTolerance)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// ATTENTION this method returns TRUE on zero length or very small lines.
    /// It returns FALSE on almost coincident lines that are very long.
    /// Pick a bigger squaredParallelogramAreaTolerance for longer lines.
    static member inline isCoincidentToFast ([<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float) other (ln:Line3D) : bool =
        ln.IsCoincidentToFast(other, squaredParallelogramAreaTolerance)

    /// A fast version of IsParallelAndOrientedTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are parallel within a given tolerance for their cross product magnitude
    /// and oriented in the same direction by having a positive dot-product.
    /// Lines are parallel if cross product magnitude squared is less than given squaredParallelogramAreaTolerance.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// Lines are oriented if dot product is positive.
    /// minDotProduct must be POSITIVE! just above zero. 1e-6 by default.
    /// The minDotProduct argument helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsParallelAndOrientedToFast( other:Line3D, [<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float , [<OPT;DEF(1e-6)>]minDotProduct :float) : bool =
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d1z = ln.VectorZ
        let d2x = other.VectorX
        let d2y = other.VectorY
        let d2z = other.VectorZ
        let cx = d1y * d2z - d1z * d2y
        let cy = d1z * d2x - d1x * d2z
        let cz = d1x * d2y - d1y * d2x
        let crossMagSq = cx*cx + cy*cy + cz*cz
        // Lines are parallel if cross product magnitude is near zero
        if crossMagSq < squaredParallelogramAreaTolerance then
            let dot = d1x * d2x + d1y * d2y + d1z * d2z
            dot > minDotProduct // instead of checking bigger than zero we use minDotProduct to avoid true for tiny lines
        else
            false

    /// A fast version of IsParallelAndOrientedTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are parallel within a given tolerance for their cross product magnitude
    /// and oriented in the same direction by having a positive dot-product.
    /// Lines are parallel if cross product magnitude squared is less than given squaredParallelogramAreaTolerance.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// Lines are oriented if dot product is positive.
    /// minDotProduct must be POSITIVE! just above zero. 1e-6 by default.
    /// The minDotProduct argument helps to make sure FALSE is returned on very short or zero length lines.
    static member inline isParallelAndOrientedToFast ([<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float) ([<OPT;DEF(1e-6)>]minDotProduct :float) other (ln:Line3D) : bool =
        ln.IsParallelAndOrientedToFast(other, squaredParallelogramAreaTolerance, minDotProduct)

    /// A fast version of IsParallelAndOpposingTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are parallel within a given tolerance for their cross product magnitude
    /// and opposing by having a negative dot-product.
    /// Lines are parallel if cross product magnitude squared is less than given squaredParallelogramAreaTolerance.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// Lines are opposing if dot product is negative.
    /// maxDotProduct must be NEGATIVE! just below zero, -1e-6 by default !
    /// The maxDotProduct helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsParallelAndOpposingToFast( other:Line3D, [<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float , [<OPT;DEF(-1e-6)>]maxDotProduct :float) : bool =
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d1z = ln.VectorZ
        let d2x = other.VectorX
        let d2y = other.VectorY
        let d2z = other.VectorZ
        let cx = d1y * d2z - d1z * d2y
        let cy = d1z * d2x - d1x * d2z
        let cz = d1x * d2y - d1y * d2x
        let crossMagSq = cx*cx + cy*cy + cz*cz
        // Lines are parallel if cross product magnitude is near zero
        if crossMagSq < squaredParallelogramAreaTolerance then
            let dot = d1x * d2x + d1y * d2y + d1z * d2z // this is expected to be negative
            dot < maxDotProduct // instead of checking less than zero we use maxDotProduct to avoid true for tiny lines
        else
            false

    /// A fast version of IsParallelAndOpposingTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are parallel within a given tolerance for their cross product magnitude
    /// and opposing by having a negative dot-product.
    /// Lines are parallel if cross product magnitude squared is less than given squaredParallelogramAreaTolerance.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// Lines are opposing if dot product is negative.
    /// maxDotProduct must be NEGATIVE! just below zero, -1e-6 by default !
    /// The maxDotProduct helps to make sure FALSE is returned on very short or zero length lines.
    static member inline isParallelAndOpposingToFast ([<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float) ([<OPT;DEF(-1e-6)>]maxDotProduct :float) other (ln:Line3D) : bool =
        ln.IsParallelAndOpposingToFast(other, squaredParallelogramAreaTolerance, maxDotProduct)

    /// A fast version of IsCoincidentAndOrientedTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are coincident on the same ray within given tolerances.
    /// Lines are coincident if they are parallel (cross product magnitude squared less than squaredParallelogramAreaTolerance)
    /// and oriented in the same direction if the dot-product is positive (bigger than minDotProduct)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// Lines are oriented if dot product is positive.
    /// minDotProduct must be POSITIVE! just above zero. 1e-6 by default.
    /// The minDotProduct argument helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsCoincidentAndOrientedToFast ( other:Line3D, [<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float, [<OPT;DEF(1e-6)>]minDotProduct :float) : bool =
        // First check if parallel and oriented
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d1z = ln.VectorZ
        let d2x = other.VectorX
        let d2y = other.VectorY
        let d2z = other.VectorZ
        let cx = d1y * d2z - d1z * d2y
        let cy = d1z * d2x - d1x * d2z
        let cz = d1x * d2y - d1y * d2x
        let crossMagSq = cx*cx + cy*cy + cz*cz
        if crossMagSq < squaredParallelogramAreaTolerance then
            let dot = d1x * d2x + d1y * d2y + d1z * d2z
            if dot > minDotProduct then
                // Then check vector between line starts is also parallel to the lines
                let px = ln.FromX - other.FromX
                let py = ln.FromY - other.FromY
                let pz = ln.FromZ - other.FromZ
                let cx = py * d2z - pz * d2y
                let cy = pz * d2x - px * d2z
                let cz = px * d2y - py * d2x
                let crossMagSq = cx*cx + cy*cy + cz*cz
                crossMagSq < squaredParallelogramAreaTolerance
            else
                false
        else
            false

    /// A fast version of IsCoincidentAndOrientedTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are coincident on the same ray within given tolerances.
    /// Lines are coincident if they are parallel (cross product magnitude squared less than squaredParallelogramAreaTolerance)
    /// and oriented in the same direction if the dot-product is positive (bigger than minDotProduct)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// Lines are oriented if dot product is positive.
    /// minDotProduct must be POSITIVE! just above zero. 1e-6 by default.
    /// The minDotProduct argument helps to make sure FALSE is returned on very short or zero length lines.
    static member inline isCoincidentAndOrientedToFast ([<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float) ([<OPT;DEF(1e-6)>]minDotProduct :float) other (ln:Line3D) : bool =
        ln.IsCoincidentAndOrientedToFast(other, squaredParallelogramAreaTolerance, minDotProduct)

    /// A fast version of IsCoincidentAndOpposingTo that uses cross-product magnitude to determine parallelism.
    /// Checks if two 3D lines are coincident on opposing rays within given tolerances.
    /// Lines are coincident if they are parallel (cross product magnitude squared less than squaredParallelogramAreaTolerance)
    /// and opposing if the dot-product is negative (smaller than maxDotProduct)
    /// and the vector between their start points is also parallel to the lines.
    /// The cross product magnitude corresponds to the area of the parallelogram spanned by the two direction vectors.
    /// Lines are opposing if dot product is negative.
    /// maxDotProduct must be NEGATIVE! just below zero, -1e-6 by default !
    /// The maxDotProduct helps to make sure FALSE is returned on very short or zero length lines.
    member inline ln.IsCoincidentAndOpposingToFast( other:Line3D, [<OPT;DEF(1e-12)>]squaredParallelogramAreaTolerance:float, [<OPT;DEF(-1e-6)>]maxDotProduct :float) : bool =
        // First check if parallel and opposing
        let d1x = ln.VectorX
        let d1y = ln.VectorY
        let d1z = ln.VectorZ
        let d2x = other.VectorX
        let d2y = other.VectorY
        let d2z = other.VectorZ
        let cx = d1y * d2z - d1z * d2y
        let cy = d1z * d2x - d1x * d2z
        let cz = d1x * d2y - d1y * d2x
        let crossMagSq = cx*cx + cy*cy + cz*cz
        if crossMagSq < squaredParallelogramAreaTolerance then
            let dot = d1x * d2x + d1y * d2y + d1z * d2z
            if dot < maxDotProduct then
                // Then check vector between line starts is also parallel to the lines
                let px = ln.FromX - other.FromX
                let py = ln.FromY - other.FromY
                let pz = ln.FromZ - other.FromZ
                let cx = py * d2z - pz * d2y
                let cy = pz * d2x - px * d2z
                let cz = px * d2y - py * d2x
                let crossMagSq = cx*cx + cy*cy + cz*cz
                crossMagSq < squaredParallelogramAreaTolerance
            else
                false
        else
            false

    /// A faster Check if two 3D lines are parallel. Ignoring orientation.
    /// The angle tolerance is 0.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return false on zero-length lines.
    static member inline isParallelTo' (lnA:Line3D) (lnB:Line3D) : bool =
        let vtX = lnA.VectorX
        let vtY = lnA.VectorY
        let vtZ = lnA.VectorZ
        let voX = lnB.VectorX
        let voY = lnB.VectorY
        let voZ = lnB.VectorZ
        let dot = vtX*voX + vtY*voY + vtZ*voZ
        // cross product magnitude (squared)
        let cx = vtY*voZ - vtZ*voY
        let cy = vtZ*voX - vtX*voZ
        let cz = vtX*voY - vtY*voX
        let crossSq = cx*cx + cy*cy + cz*cz
        // squared parallel test (no sqrt): |cross| / |dot| < tan(0.25 deg)  <=>  crossSq < tan(0.25 deg)^2 * dot^2
        !^ crossSq < Tangent.``0.25`` * Tangent.``0.25`` * dot * dot

    /// A faster Check if two 3D lines are not parallel. Ignoring orientation.
    /// The angle tolerance is 0.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return false on zero-length lines.
    static member inline isNotParallelTo' (lnA:Line3D) (lnB:Line3D) : bool =
        let vtX = lnA.VectorX
        let vtY = lnA.VectorY
        let vtZ = lnA.VectorZ
        let voX = lnB.VectorX
        let voY = lnB.VectorY
        let voZ = lnB.VectorZ
        let dot = vtX*voX + vtY*voY + vtZ*voZ
        // cross product magnitude (squared)
        let cx = vtY*voZ - vtZ*voY
        let cy = vtZ*voX - vtX*voZ
        let cz = vtX*voY - vtY*voX
        let crossSq = cx*cx + cy*cy + cz*cz
        // squared parallel test (no sqrt): |cross| / |dot| > tan(0.25 deg)  <=>  crossSq > tan(0.25 deg)^2 * dot^2
        !^ crossSq > Tangent.``0.25`` * Tangent.``0.25`` * dot * dot

    /// A faster Check if two 3D lines are perpendicular to each other.
    /// The angle tolerance is 89.75 to 90.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return false on zero-length lines.
    /// Same as Line3D.isNormalTo'
    static member inline isPerpendicularTo' (lnA:Line3D) (lnB:Line3D) : bool =
        let vtX = lnA.VectorX
        let vtY = lnA.VectorY
        let vtZ = lnA.VectorZ
        let voX = lnB.VectorX
        let voY = lnB.VectorY
        let voZ = lnB.VectorZ
        let dot = vtX*voX + vtY*voY + vtZ*voZ
        // cross product magnitude (squared)
        let cx = vtY*voZ - vtZ*voY
        let cy = vtZ*voX - vtX*voZ
        let cz = vtX*voY - vtY*voX
        let crossSq = cx*cx + cy*cy + cz*cz
        // squared perpendicularity test (no sqrt): |cross| / |dot| > tan(89.75 deg)  <=>  crossSq > tan(89.75 deg)^2 * dot^2
        !^ crossSq > Tangent.``89.75`` * Tangent.``89.75`` * dot * dot

    /// A faster Check if two 3D lines are perpendicular to each other.
    /// The angle tolerance is 89.75 to 90.25 degrees.
    /// This function does not do a check for very short lines.
    /// But will return false on zero-length lines.
    /// Same as Line3D.isPerpendicularTo'
    static member inline isNormalTo' (lnA:Line3D) (lnB:Line3D) : bool =
        Line3D.isPerpendicularTo' lnA lnB

    /// Applies or multiplies a 4x4 transformation matrix to a 3D line.
    member inline l.Transform (m:Matrix) : Line3D =
        // w = 1.0
        let x' = m.M11*l.FromX + m.M21*l.FromY + m.M31*l.FromZ + m.X41 // * w
        let y' = m.M12*l.FromX + m.M22*l.FromY + m.M32*l.FromZ + m.Y42 // * w
        let z' = m.M13*l.FromX + m.M23*l.FromY + m.M33*l.FromZ + m.Z43 // * w
        let w' = m.M14*l.FromX + m.M24*l.FromY + m.M34*l.FromZ + m.M44 // * w
        let sc = 1.0 / w'
        let x'' = m.M11*l.ToX + m.M21*l.ToY + m.M31*l.ToZ + m.X41 // * w
        let y'' = m.M12*l.ToX + m.M22*l.ToY + m.M32*l.ToZ + m.Y42 // * w
        let z'' = m.M13*l.ToX + m.M23*l.ToY + m.M33*l.ToZ + m.Z43 // * w
        let w'' = m.M14*l.ToX + m.M24*l.ToY + m.M34*l.ToZ + m.M44 // * w
        let sc'' = 1.0 / w''
        Line3D(x' * sc, y'* sc, z'* sc, x'' * sc'', y'' * sc'', z'' * sc'')


    /// Applies or multiplies a 4x4 transformation matrix to a 3D line.
    static member inline transform (m:Matrix) (ln:Line3D) : Line3D =
        ln.Transform m

    /// Multiplies (or applies) a RigidMatrix to a 3D line .
    member inline l.TransformRigid (m:RigidMatrix) : Line3D =
        Line3D(
              m.M11*l.FromX + m.M21*l.FromY + m.M31*l.FromZ + m.X41
            , m.M12*l.FromX + m.M22*l.FromY + m.M32*l.FromZ + m.Y42
            , m.M13*l.FromX + m.M23*l.FromY + m.M33*l.FromZ + m.Z43
            , m.M11*l.ToX   + m.M21*l.ToY   + m.M31*l.ToZ   + m.X41
            , m.M12*l.ToX   + m.M22*l.ToY   + m.M32*l.ToZ   + m.Y42
            , m.M13*l.ToX   + m.M23*l.ToY   + m.M33*l.ToZ   + m.Z43
        )

    /// Multiplies (or applies) a RigidMatrix to a 3D line .
    static member inline transformRigid (m:RigidMatrix) (ln:Line3D) : Line3D =
        ln.TransformRigid m

    /// Scales the 3D line by a given factor.
    /// Scale center is World Origin 0,0
    member inline l.Scale (factor:float) : Line3D =
        Line3D(
            l.FromX * factor,
            l.FromY * factor,
            l.FromZ * factor,
            l.ToX   * factor,
            l.ToY   * factor,
            l.ToZ   * factor)

    /// Scale the 3D line by a given factor.
    /// Scale center is World Origin 0,0,0
    static member inline scale (factor:float) (ln:Line3D) : Line3D =
        Line3D(ln.FromX * factor,
               ln.FromY * factor,
               ln.FromZ * factor,
               ln.ToX * factor,
               ln.ToY * factor,
               ln.ToZ * factor)

    /// Scales the 3D line by a given factor on a given center point
    member inline l.ScaleOn (cen:Pnt) (factor:float) : Line3D =
        let cx = cen.X
        let cy = cen.Y
        let cz = cen.Z
        Line3D(
            cx + (l.FromX - cx) * factor,
            cy + (l.FromY - cy) * factor,
            cz + (l.FromZ - cz) * factor,
            cx + (l.ToX   - cx) * factor,
            cy + (l.ToY   - cy) * factor,
            cz + (l.ToZ   - cz) * factor)

    /// Scales the 3D line by a given factor on a given center point
    static member inline scaleOn (cen:Pnt) (factor:float) (ln:Line3D) : Line3D =
        ln.ScaleOn cen factor


    // #endregion
    // #region Static members

    /// Checks if two 3D lines are equal within tolerance.
    /// Identical Lines in opposite directions are not considered equal.
    /// Use a tolerance of 0.0 to check for an exact match.
    static member inline equals (tol:float) (a:Line3D) (b:Line3D)  : bool =
        abs (a.FromX - b.FromX) <= tol &&
        abs (a.FromY - b.FromY) <= tol &&
        abs (a.FromZ - b.FromZ) <= tol &&
        abs (a.ToX   - b.ToX  ) <= tol &&
        abs (a.ToY   - b.ToY  ) <= tol &&
        abs (a.ToZ   - b.ToZ  ) <= tol

    /// Check if two 3D lines are not equal within a given tolerance.
    /// Use a tolerance of 0.0 to check if the two lines are not exactly equal.
    static member notEquals (tol:float) (a:Line3D) (b:Line3D)  : bool =
        abs (a.FromX - b.FromX) > tol ||
        abs (a.FromY - b.FromY) > tol ||
        abs (a.FromZ - b.FromZ) > tol ||
        abs (a.ToX   - b.ToX  ) > tol ||
        abs (a.ToY   - b.ToY  ) > tol ||
        abs (a.ToZ   - b.ToZ  ) > tol

    /// Creates a 2D line from 3D line. Ignoring Z value.
    static member inline toLine2D (ln:Line3D) : Line2D =
        Line2D(ln.FromX, ln.FromY, ln.ToX, ln.ToY)

    /// Creates a 3D line from 2D line. Setting Z to 0.0
    static member inline createFromLine2D (ln:Line2D) : Line3D =
        Line3D(ln.FromX, ln.FromY, 0., ln.ToX, ln.ToY, 0.)

    /// Creates a 3D line from 2D line. Setting Z to given value.
    static member inline createFromLine2DWithZ (zLevel) (ln:Line2D) : Line3D =
        Line3D(ln.FromX, ln.FromY, zLevel, ln.ToX, ln.ToY, zLevel)

    /// Creates a 3D line starting at World Origin and going to along the given vector.
    static member inline createFromVec (v:Vec) : Line3D =
        Line3D(0., 0., 0., v.X, v.Y, v.Z)

    /// Creates a 3D line starting at given point and going to along the given vector.
    static member inline createFromPntAndVec (p:Pnt, v:Vec) : Line3D =
        Line3D(p.X, p.Y, p.Z, p.X+v.X, p.Y+v.Y, p.Z+v.Z)

    /// Returns the Start point of the line. Same as Line3D.from.
    static member inline start (l:Line3D) : Pnt =
        l.From

    /// Returns the Start point of the line. Same as Line3D.start.
    static member inline from (l:Line3D) : Pnt =
        l.From

    /// Returns the Start point's X coordinate of the line.
    static member inline fromX (l:Line3D) : float =
        l.FromX

    /// Returns the Start point's Y coordinate of the line.
    static member inline fromY (l:Line3D) : float =
        l.FromY

    /// Returns the Start point's Z coordinate of the line.
    static member inline fromZ (l:Line3D) : float =
        l.FromZ

    /// Returns the End point of the line. Same as Line3D.to'
    static member inline end' (l:Line3D) : Pnt =
        l.To

    /// Returns the End point of the line. Same as Line3D.end'.
    static member inline to' (l:Line3D) : Pnt =
        l.To

    /// Returns the End point's X coordinate of the line.
    static member inline toX (l:Line3D) : float =
        l.ToX

    /// Returns the End point's Y coordinate of the line.
    static member inline toY (l:Line3D) : float =
        l.ToY

    /// Returns the End point's Z coordinate of the line.
    static member inline toZ (l:Line3D) : float =
        l.ToZ

    /// Set Line3D start point, returns a new line.
    /// Same as Line3D.setFrom.
    static member inline setStart (pt:Pnt) (ln:Line3D) : Line3D =
        Line3D(pt.X, pt.Y, pt.Z, ln.ToX, ln.ToY, ln.ToZ)

    /// Set Line3D start point, returns a new line.
    /// Same as Line3D.setStart.
    static member inline setFrom (pt:Pnt) (ln:Line3D) : Line3D =
        Line3D(pt.X, pt.Y, pt.Z, ln.ToX, ln.ToY, ln.ToZ)

    /// Set Line3D end point, returns a new line.
    /// Same as Line3D.setTo.
    static member inline setEnd (pt:Pnt) (ln:Line3D) : Line3D =
        Line3D(ln.FromX, ln.FromY, ln.FromZ, pt.X, pt.Y, pt.Z)

    /// Set Line3D end point, returns a new line.
    /// Same as Line3D.setEnd.
    static member inline setTo (pt:Pnt) (ln:Line3D) : Line3D =
        Line3D(ln.FromX, ln.FromY, ln.FromZ, pt.X, pt.Y, pt.Z)

    /// Returns the length of the line.
    static member inline length (l:Line3D) : float =
        l.Length

    /// Returns the square length of the line.
    static member inline lengthSq (l:Line3D) : float =
        l.LengthSq

    /// Reverse or flip the 3D line (same as Line3D.flip)
    static member inline reverse (ln:Line3D) : Line3D =
        ln.Reversed

    /// Reverse or flip the 3D line (same as Line3D.reverse)
    static member inline flip (ln:Line3D) : Line3D =
        ln.Reversed

    /// Move a 3D line by a vector. (same as Line3D.move)
    static member inline translate (v:Vec) (ln:Line3D)  : Line3D =
        ln.Move(v)

    /// Rotation a 3D line around Z-Axis.
    static member inline rotateOnZ (r:Rotation2D) (ln:Line3D) : Line3D =
        Line3D(Pnt.rotateOnZ r ln.From, Pnt.rotateOnZ r ln.To)

    /// Rotation a 3D line round a given center point an a local Z-axis.
    static member inline rotateOnZWithCenter (cen:Pnt) (r:Rotation2D) (ln:Line3D) : Line3D =
        Line3D(Pnt.rotateOnZWithCenter cen r ln.From, Pnt.rotateOnZWithCenter cen r ln.To)

    /// Ensure 3D line has a positive dot product with given orientation line.
    static member inline matchOrientation (orientationToMatch:Line3D) (lineToFlip:Line3D) : Line3D =
        if orientationToMatch.VectorX * lineToFlip.VectorX +
            orientationToMatch.VectorY * lineToFlip.VectorY +
            orientationToMatch.VectorZ * lineToFlip.VectorZ < 0.0 then
                lineToFlip.Reversed
        else
            lineToFlip

    /// Ensure 3D line has a positive dot product with given 3D vector.
    static member inline matchVecOrientation (orientationToMatch:Vec) (lineToFlip:Line3D) : Line3D =
        if orientationToMatch.X * lineToFlip.VectorX +
            orientationToMatch.Y * lineToFlip.VectorY +
            orientationToMatch.Z * lineToFlip.VectorZ < 0.0 then
                lineToFlip.Reversed
        else
            lineToFlip

    /// Ensure 3D line has a positive dot product with given 3D vector.
    static member inline matchUnitVecOrientation (orientationToMatch:UnitVec) (lineToFlip:Line3D) : Line3D =
        if orientationToMatch.X * lineToFlip.VectorX +
            orientationToMatch.Y * lineToFlip.VectorY +
            orientationToMatch.Z * lineToFlip.VectorZ < 0.0 then
                lineToFlip.Reversed
        else
            lineToFlip

    /// Checks if the dot product between a 3D line and a vector is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientationVec (v:Vec) (ln:Line3D) : bool =
        ln.MatchesOrientation v

    /// Checks if the dot product between a 3D line and a unit-vector is positive.
    /// So if the angle between their direction vectors is less than 90 degrees.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline matchesOrientationUnitVec (v:UnitVec) (ln:Line3D) : bool =
        ln.MatchesOrientation v

    /// Checks if a 3D line is parallel to a 3D vector. Ignores orientation.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelToVec (minTangent:float<Tangent.tangent>) (other:Vec) (ln:Line3D) : bool =
        ln.IsParallelTo(other, minTangent)

    /// Checks if a 3D line is parallel to a 3D unit-vector. Ignores orientation.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25`` is about 0.0044.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelToUnitVec (minTangent:float<Tangent.tangent>) (other:UnitVec) (ln:Line3D) : bool =
        ln.IsParallelTo(other, minTangent)

    /// Checks if two 3D lines are Not parallel. Ignoring orientation.
    /// The default angle tolerance is 0.25 degrees. Tangent.``0.25`` is about 0.0044.
    /// This tolerance can be customized by an optional minimum Tangent value.
    /// See Euclid.Tangent module.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isNotParallelTo (lnA:Line3D) (lnB:Line3D) : bool =
        not <| lnA.IsParallelTo(lnB)

    /// Checks if a 3D line is parallel to a 3D vector and oriented the same way.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25``.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelAndOrientedToVec (minTangent:float<Tangent.tangent>) (other:Vec) (ln:Line3D) : bool =
        ln.IsParallelAndOrientedTo(other, minTangent)

    /// Checks if a 3D line is parallel to a 3D unit-vector and oriented the same way.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``0.25``.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isParallelAndOrientedToUnitVec (minTangent:float<Tangent.tangent>) (other:UnitVec) (ln:Line3D) : bool =
        ln.IsParallelAndOrientedTo(other, minTangent)

    /// Checks if a 3D line is perpendicular to a 3D vector.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75``.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isPerpendicularToVec (maxTangent:float<Tangent.tangent>) (other:Vec) (ln:Line3D) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if a 3D line is perpendicular to a 3D unit-vector.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75``.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isPerpendicularToUnitVec (maxTangent:float<Tangent.tangent>) (other:UnitVec) (ln:Line3D) : bool =
        ln.IsPerpendicularTo(other, maxTangent)

    /// Checks if a 3D line is perpendicular to a 3D vector.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75``.
    /// Fails on lines or vectors shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isNormalToVec (maxTangent:float<Tangent.tangent>) (other:Vec) (ln:Line3D) : bool =
        ln.IsNormalTo(other, maxTangent)

    /// Checks if a 3D line is perpendicular to a 3D unit-vector.
    /// Use a precomputed value from Euclid.Tangent module as tolerance.
    /// e.g. Tangent.``89.75``.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline isNormalToUnitVec (maxTangent:float<Tangent.tangent>) (other:UnitVec) (ln:Line3D) : bool =
        ln.IsNormalTo(other, maxTangent)

    /// Assumes Line3D to be an infinite ray!
    /// Returns the parameter at which a point is closest to the infinite ray.
    /// If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
    /// Get distance from start of line to point projected onto line, may be negative.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline lengthToPtOnLine (ln:Line3D) (pt:Pnt) : float =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = XYZ.length x y z
        if isTooTiny l then failTooSmall "Line3D.lengthToPtOnLine" ln
        let dx = pt.X - ln.FromX
        let dy = pt.Y - ln.FromY
        let dz = pt.Z - ln.FromZ
        let t = XYZ.dot x y z dx dy dz
        t / l


    /// Finds point at given distance from line start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline pointAtDistance dist (ln:Line3D) : Pnt =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let len = XYZ.length x y z
        if isTooTiny len then failTooSmall "Line3D.pointAtDistance" ln
        let f = dist/len
        Pnt(ln.FromX + x*f,
            ln.FromY + y*f,
            ln.FromZ + z*f)

    /// Returns new Line3D with given length, going out from start in direction of end.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline withLengthFromStart len (ln:Line3D) : Line3D =
        let x = ln.VectorX
        let y = ln.VectorY
        let z = ln.VectorZ
        let l = XYZ.length x y z
        if isTooTiny l then failTooSmall "Line3D.withLengthFromStart" ln
        let f = len/l
        Line3D( ln.FromX,
                ln.FromY,
                ln.FromZ,
                ln.FromX + x*f,
                ln.FromY + y*f,
                ln.FromZ + z*f)

    /// Returns new Line3D ending at current LineEnd with given length coming from direction of start.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member inline withLengthToEnd len (ln:Line3D) : Line3D =
        let x = ln.FromX-ln.ToX
        let y = ln.FromY-ln.ToY
        let z = ln.FromZ-ln.ToZ
        let l = XYZ.length x y z
        if isTooTiny l then failTooSmall "Line3D.withLengthToEnd" ln
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
    static member inline withLengthFromMid len (ln:Line3D) : Line3D =
        let x = ln.FromX-ln.ToX
        let y = ln.FromY-ln.ToY
        let z = ln.FromZ-ln.ToZ
        let l = XYZ.length x y z
        if isTooTiny l then failTooSmall "Line3D.withLengthFromMid" ln
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
    static member offsetXY amount (ln:Line3D) : Line3D =
        if amount = 0.0 then
            ln
        else
            let x = ln.VectorX
            let y = ln.VectorY
            let lenXY = sqrt (x*x + y*y)
            if isTooTiny (lenXY ) then failVertical "Line3D.offsetXY" ln
            let ox = -y*amount/lenXY  // unitized, horizontal, perpendicular  vector
            let oy =  x*amount/lenXY  // unitized, horizontal, perpendicular  vector
            Line3D( ln.FromX+ox,
                    ln.FromY+oy,
                    ln.FromZ,
                    ln.ToX+ox,
                    ln.ToY+oy,
                    ln.ToZ)

    /// Offsets a 3D line by two given distances.
    /// The fist distance (distHorizontal) is applied in in X-Y plane.
    /// The second distance (distNormal) is applied perpendicular to the line (made by the two 3D points)
    /// and perpendicular to the horizontal offset direction.
    /// This is in World.Z direction if both points are at the same Z level.
    /// If points are closer than 1e-6 units the World.Xaxis is used
    /// as first direction and World Z-axis as second direction.
    static member offset (distHorizontal:float) (distNormal:float) (l:Line3D ): Line3D=
        let vx = l.VectorX
        let vy = l.VectorY
        let vz = l.VectorZ
        let normHor =
            Vec(vy, -vx, 0.0)
            |> Vec.unitizeOrDefault UnitVec.Xaxis

        let normFree =
            Vec(vy * normHor.Z - vz * normHor.Y,
                vz * normHor.X - vx * normHor.Z,
                vx * normHor.Y - vy * normHor.X)
            |> Vec.unitizeOrDefault UnitVec.Zaxis

        let shift = distHorizontal * normHor + distNormal * normFree
        l
        |> Line3D.move shift

    /// Divides a 3D line into given amount of segments.
    /// Returns an array of 3D points of length: segment count + 1.
    /// Includes start and endpoint of line.
    static member divide (segments:int) (ln:Line3D) : Pnt[] =
        if segments < 1 then
            fail $"Line3D.divide: segments must be at least 1, was {segments}"
        if segments = 1 then
            [|ln.From;  ln.To|]
        else
            let x = ln.VectorX
            let y = ln.VectorY
            let z = ln.VectorZ
            let sx = ln.FromX
            let sy = ln.FromY
            let sz = ln.FromZ
            let kk = float segments
            let r = Array.zeroCreate (segments+1)
            r.[0] <- ln.From
            for i = 1 to segments-1 do
                let t = float i / kk
                r.[i] <- Pnt(sx + x*t, sy + y*t, sz + z*t)
            r.[segments] <- ln.To
            r

    /// Divides a 3D line into as many as segments as possible respecting the minimum segment length.
    /// Returned Array includes start and endpoint of line.
    /// The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    static member divideMinLength (minSegmentLength:float) (ln:Line3D) : Pnt[] =
        let len = ln.Length
        if len < 1e-6 then
            fail $"Line3D.divideMinLength: line length {len} is too small."
        if len < minSegmentLength then
            fail $"Line3D.divideMinLength: line length {len} is smaller than minSegmentLength {minSegmentLength}"
        let k = int (len / (minSegmentLength*1.000001))
        Line3D.divide k ln

    /// Divides a 3D line into as few as segments as possible respecting the maximum segment length.
    /// Returned Array includes start and endpoint of line.
    /// The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical  errors.
    /// That means in an edge case there are fewer segments returned, not more.
    static member divideMaxLength (maxSegmentLength:float) (ln:Line3D) : Pnt[] =
        let len = ln.Length
        if len < 1e-6 then
            fail $"Line3D.divideMaxLength: line length {len} is too small."
        if maxSegmentLength < 1e-6 then
            fail $"Line3D.divideMaxLength: maxSegmentLength must be greater than 0.0, was {maxSegmentLength}"
        let k = int (len / maxSegmentLength*0.999999) + 1
        Line3D.divide k ln

    /// Divides a 3D line into given amount of segments.
    /// Includes a gap between the segments. But not at the start or end.
    /// Returns an array of 3D lines.
    /// Returns an empty array if the length of the line is less than gap-size x segment-count-minus-1.
    static member split (gap:float) (segments:int) (ln:Line3D) : Line3D[] =
        let vx = ln.VectorX
        let vy = ln.VectorY
        let vz = ln.VectorZ
        let len = sqrt(vx*vx + vy*vy + vz*vz)
        if segments <= 0  then
            fail $"Line3D.split: segments must be at least 1, was {segments}"
        if len < 1e-6 then
            fail $"Line3D.split: line length {len} is too small."
        let lenMinusGaps = len - gap * float (segments-1)
        let segLen = lenMinusGaps / float segments
        if isTooTiny (segLen) then
            [||]
        else
            let lns = Array.zeroCreate segments
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
    /// Returns an array ofe3D lines
    /// The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
    /// That means in an edge case there are more segments returned, not fewer.
    static member splitMinLength (gap:float) (minSegmentLength:float) (ln:Line3D) : Line3D[] =
        let len = ln.Length
        if len < minSegmentLength then
            fail $"Line3D.splitMinLength: line length {len} is smaller than minSegmentLength {minSegmentLength}"
        if minSegmentLength < 1e-6 then
            fail $"Line3D.splitMinLength: minSegmentLength must be greater than 0.0, was {minSegmentLength}"
        let k = int ((len+gap) / ((minSegmentLength+gap)*1.000001))
        Line3D.split gap k ln

    /// Divides a 3D line into as few as segments as possible respecting the maximum segment length and the gap.
    /// Includes a gap between the segments. But not at the start or end.
    /// Returns an array ofe3D lines
    /// The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical errors.
    /// That means in an edge case there are fewer segments returned, not more.
    static member splitMaxLength (gap:float) (maxSegmentLength:float) (ln:Line3D)  : Line3D[] =
        let len = ln.Length
        if maxSegmentLength < 1e-6 then
            fail $"Line3D.splitMaxLength: maxSegmentLength must be greater than 0.0, was {maxSegmentLength}"
        if len < 1e-6 then
            fail $"Line3D.splitMaxLength: line length {len} is too small."
        let k = int ((len+gap) / (maxSegmentLength+gap)*0.999999) + 1
        Line3D.split gap k ln

    /// Divides a 3D line into segments of given length.
    /// Includes start and end point.
    /// If the line length is smaller than the given distance just the start and end point is returned.
    /// Adds end point only if there is a remainder bigger than 0.1% of the segment length.
    static member divideEvery dist (l:Line3D) : ResizeArray<Pnt> =
        let len = l.Length
        let div = len / dist
        let floor = Math.Floor div
        if floor = 0.0 then
            let pts = ResizeArray<Pnt>(2)
            pts.Add l.From
            pts.Add l.To
            pts
        else
            let step = dist / len
            let count = int floor
            let pts = ResizeArray<Pnt>(count + 2)
            pts.Add l.From
            for i = 1 to count do
                pts.Add <| l.EvaluateAt (step * float i)
            if div - floor > 0.001 then
                pts.Add l.To // add end point only if there is a remainder bigger than 0.1%
            pts

    /// Divides a 3D line into segments of given length.
    /// Excludes start and end point.
    /// If the line length is smaller than the given distance an empty array is returned.
    /// Adds last div point before end only if there is a remainder bigger than 0.1% of the segment length.
    static member divideInsideEvery dist (l:Line3D) : ResizeArray<Pnt> =
        let len = l.Length
        let div = len / dist
        let floor = Math.Floor div
        if floor = 0.0 then
            ResizeArray<Pnt>()
        else
            let step = dist / len
            let count = int floor
            let pts = ResizeArray<Pnt>(count)
            for i = 1 to count do
                if i < count || div - floor > 0.001 then
                    pts.Add <| l.EvaluateAt (step * float i)
            pts

    /// <summary>Intersects an infinite ray / line with the base-side nappe of a cone whose axis is the Z-axis.</summary>
    /// <param name="ray">The Line3D to intersect. It is considered infinite in both directions.</param>
    /// <param name="coneRadius">The positive radius of the cone at the base, parallel to the XY plane.</param>
    /// <param name="coneBaseZ">The Z coordinate of the cone base.</param>
    /// <param name="coneTipZ">The Z coordinate of the cone tip. Must differ from coneBaseZ.</param>
    /// <returns>The two parameters on the ray where the intersections occur.
    /// If there is only one touching point both parameters are the same.
    /// If there is no intersection None is returned.
    /// </returns>
    static member intersectCone (ray:Line3D, coneRadius, coneBaseZ, coneTipZ) : Option<float*float> =
        match XLine3D.intersectCone(ray, coneRadius, coneBaseZ, coneTipZ) with
        | XLine3D.XCone.NoIntersection -> None
        | XLine3D.XCone.Intersecting (t1, t2) -> Some (t1, t2)
        | XLine3D.XCone.IntersectingOne t -> Some (t, t)
        | XLine3D.XCone.Touching t -> Some (t, t)
        | XLine3D.XCone.ThroughTip t -> Some (t, t)
        | XLine3D.XCone.LineOnCone (tipParam, _) -> Some (tipParam, tipParam)




    /// Project a 3D line onto another line considered infinite in both directions.
    /// Returns the start and end parameters of the projected line on the target line.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member projectOntoRayParam (rayToProjectOnto:Line3D) (lineToProject:Line3D) : float*float =
        let osx = rayToProjectOnto.FromX
        let osy = rayToProjectOnto.FromY
        let osz = rayToProjectOnto.FromZ
        let ovx = rayToProjectOnto.ToX - osx
        let ovy = rayToProjectOnto.ToY - osy
        let ovz = rayToProjectOnto.ToZ - osz
        let lenSq = ovx*ovx + ovy*ovy + ovz*ovz
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line3D.projectOntoRayParam" rayToProjectOnto
        // start parameter
        let u = lineToProject.FromX - osx
        let v = lineToProject.FromY - osy
        let w = lineToProject.FromZ - osz
        let dot = ovx*u + ovy*v + ovz*w
        let s = dot / lenSq
        // end parameter
        let u = lineToProject.ToX - osx
        let v = lineToProject.ToY - osy
        let w = lineToProject.ToZ - osz
        let dot = ovx*u + ovy*v + ovz*w
        let e =  dot / lenSq
        s, e

    /// Project a 3D line onto another line considered infinite in both directions.
    /// Returns the projected line.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member projectOntoRay (rayToProjectOnto:Line3D) (lineToProject:Line3D) : Line3D =
        let osx = rayToProjectOnto.FromX
        let osy = rayToProjectOnto.FromY
        let osz = rayToProjectOnto.FromZ
        let ovx = rayToProjectOnto.ToX - osx
        let ovy = rayToProjectOnto.ToY - osy
        let ovz = rayToProjectOnto.ToZ - osz
        let lenSq = ovx*ovx + ovy*ovy + ovz*ovz
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line3D.projectOntoRay" rayToProjectOnto
        // start parameter
        let u = lineToProject.FromX - osx
        let v = lineToProject.FromY - osy
        let w = lineToProject.FromZ - osz
        let dot = ovx*u + ovy*v + ovz*w
        let s = dot / lenSq
        // end parameter
        let u = lineToProject.ToX - osx
        let v = lineToProject.ToY - osy
        let w = lineToProject.ToZ - osz
        let dot = ovx*u + ovy*v + ovz*w
        let e =  dot / lenSq
        Line3D( osx + ovx * s,
                osy + ovy * s,
                osz + ovz * s,
                osx + ovx * e,
                osy + ovy * e,
                osz + ovz * e)

    /// Tries to project a 3D line onto another line considered finite.
    /// Returns None if there is no overlap.
    /// Returns Some (startParam, endParam) if there is an overlap.
    /// The parameters are between 0.0 and 1.0 on the target line.
    /// The first parameter is from the start of the line to project.
    /// The second parameter is from the end of the line to project.
    /// So if the first parameter is bigger than the second, the lines are oriented in opposite direction.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member tryProjectOntoLineParam (onToLine:Line3D) (lineToProject:Line3D) : option<float*float> =
        let osx = onToLine.FromX
        let osy = onToLine.FromY
        let osz = onToLine.FromZ
        let ovx = onToLine.ToX - osx
        let ovy = onToLine.ToY - osy
        let ovz = onToLine.ToZ - osz
        let lenSq = ovx*ovx + ovy*ovy + ovz*ovz
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line3D.tryProjectOntoLineParam" onToLine
        // start parameter
        let u = lineToProject.FromX - osx
        let v = lineToProject.FromY - osy
        let w = lineToProject.FromZ - osz
        let dot = ovx*u + ovy*v + ovz*w
        let bStartOnA = dot / lenSq
        // end parameter
        let u = lineToProject.ToX - osx
        let v = lineToProject.ToY - osy
        let w = lineToProject.ToZ - osz
        let dot = ovx*u + ovy*v + ovz*w
        let bEndOnA =  dot / lenSq
        if bStartOnA < -1e-6 && bEndOnA < -1e-6 then
            None
        elif bStartOnA > ``1.0 + 1e-6`` && bEndOnA > ``1.0 + 1e-6`` then
            None
        else
            Some (clamp01 bStartOnA, clamp01 bEndOnA)

    /// Tries to project a 3D line onto another line considered finite.
    /// Returns Some Line3D if there is an overlap.
    /// Returns None if there is no overlap.
    /// Keeps the orientation of the line to project.
    /// Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
    static member tryProjectOntoLine (onToLine:Line3D) (lineToProject:Line3D) : option<Line3D> =
        let osx = onToLine.FromX
        let osy = onToLine.FromY
        let osz = onToLine.FromZ
        let ovx = onToLine.ToX - osx
        let ovy = onToLine.ToY - osy
        let ovz = onToLine.ToZ - osz
        let lenSq = ovx*ovx + ovy*ovy + ovz*ovz
        if UtilEuclid.isTooSmallSq(lenSq) then
            failTooSmall "Line3D.tryProjectOntoLine" onToLine
        // start parameter
        let u = lineToProject.FromX - osx
        let v = lineToProject.FromY - osy
        let w = lineToProject.FromZ - osz
        let dot = ovx*u + ovy*v + ovz*w
        let bStartOnA = dot / lenSq
        // end parameter
        let u = lineToProject.ToX - osx
        let v = lineToProject.ToY - osy
        let w = lineToProject.ToZ - osz
        let dot = ovx*u + ovy*v + ovz*w
        let bEndOnA =  dot / lenSq
        if bStartOnA < -1e-6 && bEndOnA < -1e-6 then
            None
        elif bStartOnA > ``1.0 + 1e-6`` && bEndOnA > ``1.0 + 1e-6`` then
            None
        else
            let st = clamp01 bStartOnA
            let en = clamp01 bEndOnA
            Some <| Line3D( osx + ovx * st,
                            osy + ovy * st,
                            osz + ovz * st,
                            osx + ovx * en,
                            osy + ovy * en,
                            osz + ovz * en)

    // #endregion
    // #region Intersection

    /// <summary>A fast test to check if two finite 3D lines truly intersect (or come very close in the skew case).
    /// Uses the default tolerance for parallel lines (0.25 degrees) and maximum skew distance (1e-6).
    /// Returns false on zero length lines or if lines are parallel, apart, or don't intersect within their finite segments.</summary>
    /// <param name="lnA">The first line.</param>
    /// <param name="lnB">The second line.</param>
    /// <remarks>Use the XLine3D module for more specialized intersection calculations with custom tolerances.</remarks>
    static member doIntersect (lnA:Line3D) (lnB:Line3D) : bool =
        XLine3D.doIntersect(lnA, lnB)

    /// <summary>A fast test to check if two infinite rays (3D lines extended infinitely) intersect (or come very close in the skew case).
    /// Uses a maximum skew distance of 1e-6.</summary>
    /// <param name="lnA">The first ray.</param>
    /// <param name="lnB">The second ray.</param>
    /// <remarks>Use the XLine3D module for more specialized intersection calculations with custom tolerances.</remarks>
    static member doRaysIntersect (lnA:Line3D) (lnB:Line3D) : bool =
        XLine3D.doRaysIntersect(lnA, lnB)

    /// <summary>Tests if two finite 3D lines intersect, touch, or overlap.
    /// Also returns TRUE if parallel lines are touching or overlapping each other.
    /// Also returns TRUE if zero length lines are at the same location.</summary>
    /// <param name="lnA">The first line.</param>
    /// <param name="lnB">The second line.</param>
    /// <remarks>This method uses an angle of 0.25 degrees to classify lines as parallel.
    /// In which case it also checks if they overlap or touch.
    /// Uses a skew distance of 1e-6 to classify lines as intersecting.
    /// Use the XLine3D module for more specialized intersection calculations.</remarks>
    static member doIntersectOrOverlap (lnA:Line3D) (lnB:Line3D) : bool =
        match XLine3D.getIntersectionParam(lnA, lnB ) with
        | XLine3D.XParam.Intersect _  -> true
        | XLine3D.XParam.Skew _       -> false
        | XLine3D.XParam.Parallel     -> XLine3D.doOverlap(lnA, lnB)
        | XLine3D.XParam.Apart        -> false
        | XLine3D.XParam.TooShortBoth -> XLineXYZ.sqDistLnFromLnFrom(lnA, lnB)                      < 1e-12
        | XLine3D.XParam.TooShortA    -> XLineXYZ.sqDistLnPt'(lnB, lnA.FromX, lnA.FromY, lnA.FromZ) < 1e-12
        | XLine3D.XParam.TooShortB    -> XLineXYZ.sqDistLnPt'(lnA, lnB.FromX, lnB.FromY, lnB.FromZ) < 1e-12

    /// <summary>A fast intersection of two finite 3D lines.
    /// Returns the intersection point or the midpoint of closest approach for skew lines within tolerance.
    /// Does not use a default tolerance for parallel or coincident lines.</summary>
    /// <param name="lnA">The first line.</param>
    /// <param name="lnB">The second line.</param>
    /// <returns>A Pnt on line A if the lines intersect or are very close (skew distance less than 1e-6),
    /// None if apart, lines too short, or if parallel lines are touching or overlapping.</returns>
    /// <remarks>Use the XLine3D module for more specialized intersection calculations.</remarks>
    static member tryIntersect (lnA:Line3D) (lnB:Line3D) : Pnt voption =
        XLine3D.tryIntersect(lnA, lnB)

    /// <summary>Tries to get intersection point of two rays (rays are 3D lines extended infinitely).</summary>
    /// <param name="lineA">First ray.</param>
    /// <param name="lineB">Second ray.</param>
    /// <returns>The point at which the two rays intersect or the closest point on line A for skew lines, or None.</returns>
    /// <remarks>If the lines are parallel or coincident, or if they are skew with distance > 1e-6, None is returned.
    /// For skew lines within tolerance, returns the closest point on line A.</remarks>
    static member tryIntersectRay (lineA:Line3D) (lineB:Line3D) : Pnt voption =
        XLine3D.tryIntersectRay(lineA, lineB)

    /// <summary>Intersects two finite 3D Lines.
    /// Also returns a point if parallel lines are touching or overlapping each other.
    /// Also returns a point if zero length lines are at the same location within 1e-6 distance.</summary>
    /// <param name="lnA">The first line.</param>
    /// <param name="lnB">The second line.</param>
    /// <returns>An option of Pnt.
    /// If the lines are coincident and overlapping this point is in the center of their overlap.</returns>
    /// <remarks>This method uses an angle of 0.25 degrees to classify Lines as parallel.
    /// In which case it also checks if they overlap or touch.
    /// For skew lines that are very close (less than 1e-6), returns the closest point on line A.
    /// Use the XLine3D module for more specialized intersection calculations.</remarks>
    static member tryIntersectOrOverlap (lnA:Line3D) (lnB:Line3D) : Pnt option =
        match XLine3D.getIntersection(lnA, lnB) with
        | XLine3D.XPnt.Intersect p  -> Some p
        | XLine3D.XPnt.Skew _       -> None
        | XLine3D.XPnt.Apart        -> None
        | XLine3D.XPnt.Parallel ->
            let sqDist = XLineXYZ.sqRayPtDist(lnA.FromX, lnA.FromY, lnA.FromZ, lnA.VectorX, lnA.VectorY, lnA.VectorZ, lnB.FromX, lnB.FromY, lnB.FromZ)
            if sqDist < 1e-12 then
                match Line3D.tryProjectOntoLineParam lnA lnB with
                | Some (s,e) ->Some <| lnA.EvaluateAt((s+e)*0.5)
                | None -> None
            else
                None
        | XLine3D.XPnt.TooShortBoth -> if XLineXYZ.sqDistLnFromLnFrom(lnA, lnB) < 1e-12 then Some lnA.From else None
        | XLine3D.XPnt.TooShortA    -> if XLineXYZ.sqDistLnPt'(lnB, lnA.FromX, lnA.FromY, lnA.FromZ) < 1e-12 then Some lnA.From else None
        | XLine3D.XPnt.TooShortB    -> if XLineXYZ.sqDistLnPt'(lnA, lnB.FromX, lnB.FromY, lnB.FromZ) < 1e-12 then Some lnB.From else None

    /// <summary>Checks if lines are parallel, coincident and overlapping.</summary>
    /// <param name="lnA">The first line.</param>
    /// <param name="lnB">The second line.</param>
    /// <returns>An option of a tuple of two floats.
    /// Returns the tuple containing the start and end parameter of the overlap on lnA if the lines are parallel and overlapping.
    /// Returns None if the lines are not parallel, not overlapping, or too short.</returns>
    /// <remarks>If the first parameter in the overlap is smaller than the second the lines are oriented in the same direction.
    /// If the first parameter is greater than the second the lines are oriented in the opposite direction.</remarks>
    static member tryGetOverlap (lnA:Line3D) (lnB:Line3D) : option<float*float> =
        let vax = lnA.VectorX
        let vay = lnA.VectorY
        let vaz = lnA.VectorZ
        let vbx = lnB.VectorX
        let vby = lnB.VectorY
        let vbz = lnB.VectorZ
        if XLineXYZ.isTooShort(vax, vay, vaz, 1e-6) || XLineXYZ.isTooShort(vbx, vby, vbz, 1e-6) then
            None
        else
            let tan = XLineXYZ.tangent(vax, vay, vaz, vbx, vby, vbz)
            if abs tan < Tangent.``0.25`` then
                let sqDist = XLineXYZ.sqRayPtDist(lnA.FromX, lnA.FromY, lnA.FromZ, lnA.VectorX, lnA.VectorY, lnA.VectorZ, lnB.FromX, lnB.FromY, lnB.FromZ)
                if sqDist < 1e-9 then
                    Line3D.tryProjectOntoLineParam lnA lnB
                else
                    None
            else
                None

    /// <summary>Computes the squared distance between two finite 3D lines.</summary>
    /// <param name="lnA">The first line.</param>
    /// <param name="lnB">The second line.</param>
    /// <returns>The squared distance between the two lines. Works correctly for zero-length, parallel, and skew lines.</returns>
    static member sqDistanceToLine (lnA:Line3D) (lnB:Line3D) : float =
        XLine3D.getSqDistance(lnA, lnB)

    /// <summary>Computes the distance between two finite 3D lines.</summary>
    /// <param name="lnA">The first line.</param>
    /// <param name="lnB">The second line.</param>
    /// <returns>The distance between the two lines. Works correctly for zero-length, parallel, and skew lines.</returns>
    static member distanceToLine (lnA:Line3D) (lnB:Line3D) : float =
        XLine3D.getSqDistance(lnA, lnB) |> sqrt

    /// <summary>Checks if the two finite 3D lines are touching each other at any of end points
    /// within the given tolerance.
    /// This will also return TRUE if the lines are touching on both points.</summary>
    /// <param name="squareTolerance"> The squared tolerance for the distance between the end points.</param>
    /// <param name="a"> The first line.</param>
    /// <param name="b"> The second line.</param>
    /// <remarks>Use <see cref="XLine3D.getEndsTouching"/> to get more detailed information about which ends are touching.</remarks>
    static member isTouchingEndOf squareTolerance (a:Line3D) (b:Line3D)  : bool =
        (
            let x = a.ToX-b.FromX
            let y = a.ToY-b.FromY
            let z = a.ToZ-b.FromZ
            x*x + y*y + z*z < squareTolerance
        ) || (
            let x = a.FromX-b.ToX
            let y = a.FromY-b.ToY
            let z = a.FromZ-b.ToZ
            x*x + y*y + z*z < squareTolerance
        ) || (
            let x = a.FromX-b.FromX
            let y = a.FromY-b.FromY
            let z = a.FromZ-b.FromZ
            x*x + y*y + z*z < squareTolerance
        ) || (
            let x = a.ToX-b.ToX
            let y = a.ToY-b.ToY
            let z = a.ToZ-b.ToZ
            x*x + y*y + z*z < squareTolerance
        )




    // #endregion
    // #region Obsolete





    // Instance members marked as Obsolete that have direct replacements:

    [<Obsolete("Use ln.MatchesOrientation45 instead")>]
    member inline ln.MatchesOrientation90 (l:Line3D) : bool =
        ln.MatchesOrientation45(l)

    [<Obsolete("Use matchesOrientation45 instead")>]
    static member inline matchesOrientation90 (l:Line3D) (ln:Line3D) : bool =
        l.MatchesOrientation45 ln

    [<Obsolete("Use ln.MatchesOrientation instead")>]
    member inline ln.MatchesOrientation180 (otherLn:Line3D) : bool =
        ln.MatchesOrientation(otherLn)

    [<Obsolete("Use ln.MatchesOrientation instead")>]
    member inline ln.MatchesOrientation180 (v:Vec) : bool =
        ln.MatchesOrientation(v)

    [<Obsolete("Use ln.MatchesOrientation instead")>]
    member inline ln.MatchesOrientation180 (v:UnitVec) : bool =
        ln.MatchesOrientation(v)

    [<Obsolete("Use matchesOrientation instead")>]
    static member inline matchesOrientation180 (l:Line3D) (ln:Line3D) : bool =
        l.MatchesOrientation ln

    [<Obsolete("Use this.RayClosestParameter instead")>]
    member ln.ClosestParameterInfinite (p:Pnt) : float =
        ln.RayClosestParameter(p)

    [<Obsolete("Use Line3D.rayClosestParameter instead")>]
    static member inline closestParameterInfinite (p:Pnt) (ln:Line3D) : float =
        ln.RayClosestParameter p

    [<Obsolete("Use this.RayClosestPoint instead")>]
    member ln.ClosestPointInfinite (p:Pnt) : Pnt =
        ln.RayClosestPoint(p)

    [<Obsolete("Use Line3D.rayClosestPoint instead")>]
    static member inline closestPointInfinite (p:Pnt) (ln:Line3D) : Pnt =
        ln.RayClosestPoint p

    [<Obsolete("Use this.SqDistanceRayPoint instead")>]
    member ln.DistanceSqToPntInfinite(p:Pnt) : float =
        ln.SqDistanceRayPoint(p)

    [<Obsolete("Use Line3D.sqDistanceRayPoint instead")>]
    static member inline distanceSqToPntInfinite(p:Pnt) (ln:Line3D) : float =
        ln.SqDistanceRayPoint p

    [<Obsolete("Use this.DistanceRayPoint instead")>]
    member inline ln.DistanceToPntInfinite(p:Pnt) : float =
        ln.DistanceRayPoint(p)

    [<Obsolete("Use Line3D.distanceRayPoint instead")>]
    static member inline distanceToPntInfinite(p:Pnt) (ln:Line3D) : float =
        ln.DistanceRayPoint p

    [<Obsolete("Use this.SqDistanceFromPoint instead")>]
    member inline ln.DistanceSqToPnt(p:Pnt) : float =
        ln.SqDistanceFromPoint(p)



    // Static members marked as Obsolete that have direct replacements:

    [<Obsolete("Use Line3D.sqDistanceFromPoint instead")>]
    static member inline distanceSqToPnt(p:Pnt) (ln:Line3D) : float =
        ln.SqDistanceFromPoint p

    [<Obsolete("Use Line3D.distanceToLine instead. Obsolete since 0.20.0")>]
    static member distanceBetweenLines(lnA:Line3D, lnB:Line3D) : float =
        Line3D.distanceToLine lnA lnB

    [<Obsolete("Use Line3D.projectOntoRay instead. Obsolete since 0.20.0")>]
    static member projectOn(onToLine:Line3D, lineToProject:Line3D) : Line3D =
        Line3D.projectOntoRay onToLine lineToProject

    [<Obsolete("Use isTouchingEndOf or XLine3D.getEndsTouching instead. Obsolete since 0.20.0")>]
    static member areTouchingAny(_tol:float, _a:Line3D, _b:Line3D) : 'b =
        failObsoleteV20 "Line3D.areTouchingAny" "XLine3D.getEndsTouching"

    [<Obsolete("Use isTouchingEndOf or XLine3D.getEndsTouching instead. Obsolete since 0.20.0")>]
    static member areTouchingEither(_tol:float, _a:Line3D, _b:Line3D) : 'a =
        failObsoleteV20 "Line3D.areTouchingEither" "XLine3D.getEndsTouching"

    [<Obsolete("Use Line3D.isCoincidentTo instead. Obsolete since 0.21.0")>]
    static member inline areCoincident (a:Line3D) (b:Line3D) : bool =
        Line3D.isCoincidentTo 1e-6 Tangent.``0.25`` a b

    [<Obsolete("Use Line3D.isParallelTo instead. Obsolete since 0.21.0")>]
    static member inline areParallel (l:Line3D) (ln:Line3D) : bool =
        Line3D.isParallelTo Tangent.``0.25`` l ln

    [<Obsolete("Use Line3D.isParallelAndOrientedTo instead. Obsolete since 0.21.0")>]
    static member inline areParallelAndMatchOrientation (l:Line3D) (ln:Line3D) : bool =
        Line3D.isParallelAndOrientedTo Tangent.``0.25`` l ln

    [<Obsolete("Use Line3D.isPerpendicularTo instead. Obsolete since 0.21.0")>]
    static member inline arePerpendicular(l:Line3D) (ln:Line3D) : bool =
        Line3D.isPerpendicularTo Tangent.``89.75`` l ln




    // Obsolete members that don't have a direct replacement:

    [<Obsolete("Use XLine3D.getIntersectionParam instead. Obsolete since 0.20.0", error=true)>]
    static member intersectionParamInfinite(_lnA:Line3D, _lnB:Line3D ) : float =
        failObsoleteV20 "Line3D.intersectionParamInfinite" "XLine3D.getIntersectionParam"

    [<Obsolete("Use XLine3D.getIntersection instead. Obsolete since 0.20.0", error=true)>]
    static member intersectionInfinite(_lnA:Line3D, _lnB:Line3D ) : Pnt =
        failObsoleteV20 "Line3D.intersectionInfinite" "XLine3D.getIntersection"

    [<Obsolete("Use Line3D.tryIntersectRay instead. Obsolete since 0.20.0", error=true)>]
    static member intersectionPointInfinite(_lnA:Line3D, _lnB:Line3D )  : Pnt =
        failObsoleteV20 "Line3D.intersectionPointInfinite" "Line3D.tryIntersectRay"

    [<Obsolete("Use XLine3D.getIntersectionParam instead. Obsolete since 0.20.0", error=true)>]
    static member intersectionParam(_lnA:Line3D, _lnB:Line3D )  : float =
        failObsoleteV20 "Line3D.intersectionParam" "XLine3D.getIntersectionParam"

    [<Obsolete("Use XLine3D.getIntersection instead. Obsolete since 0.20.0", error=true)>]
    static member intersection(_lnA:Line3D, _lnB:Line3D )  : Pnt =
        failObsoleteV20 "Line3D.intersection" "XLine3D.getIntersection"

    [<Obsolete("Use Line3D.tryIntersect instead. Obsolete since 0.20.0", error=true)>]
    static member intersectionPoint(_lnA:Line3D, _lnB:Line3D)  : Pnt =
        failObsoleteV20 "Line3D.intersectionPoint" "Line3D.tryIntersect"

