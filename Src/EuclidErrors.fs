namespace Euclid

open System

/// An internal module for Euclid specific exceptions and failure functions.
module EuclidErrors =

    /// Exception in Euclid.
    type EuclidException (s:string) =
        inherit Exception(s)

    /// Exception for attempting to divide by a 0.0 or almost 0.0 value.
    /// Almost 0.0 is defined by UtilEuclid.zeroLengthTolerance as 1e-12.
    type EuclidDivByZeroException (s:string) =
        inherit Exception(s)

    /// Exception for attempting to divide by a 0.0 or almost 0.0 value during Unitizing
    /// Almost 0.0 is defined by UtilEuclid.zeroLengthTolerance as 1e-12.
    type EuclidUnitizingException (s:string) =
        inherit Exception(s)

    /// Exception for NaN or Infinity values in inputs.
    type EuclidNanInfinityException (s:string) =
        inherit Exception(s)

    /// Exception for too small input values that don't give a clear vector direction.
    type EuclidTooSmallException (s:string) =
        inherit Exception(s)

    /// Exception for null references in inputs.
    type EuclidNullException (s:string) =
        inherit Exception(s)

    /// Exception for empty sequences in inputs.
    type EuclidEmptySeqException (s:string) =
        inherit Exception(s)

    /// Exception for Obsolete features.
    type EuclidObsoleteException (s:string) =
        inherit Exception(s)


    let internal nl = Environment.NewLine



    let fail (msg:string) :unit =
        raise <| EuclidException $"Euclid.{msg}"

    let failRarr (msg:string) (xs: ResizeArray<'T>) :unit =
        raise <| EuclidException $"Euclid.ResizeArr.{msg} failed on ResizeArray of {xs.Count} elements."

    let failNull (funcName:string) (arg:string) :unit =
        raise <| EuclidNullException $"Euclid.{funcName} argument {arg} is null."

    let failEmptySeq (funcName:string) (arg:string) :unit =
        raise <| EuclidEmptySeqException $"Euclid.{funcName} argument {arg} is an empty sequence."

    let fail1 (msg:string) (a: 'T) :unit =
        raise <| EuclidException $"{msg} failed on: {nl}  %O{a}."

    let fail2 (msg:string) (a: 'T) (b: 'U) :unit =
        raise <| EuclidException $"{msg} failed on {nl}  %O{a}{nl}  %O{b}."

    let fail3 (msg:string) (a: 'T) (b:'U) (c:'V) :unit =
        raise <| EuclidException $"{msg} failed on: {nl}  %O{a}{nl}  %O{b}{nl}  %O{c}."

    let failColinear (msg:string) (a: 'T) (b:'U) (c:'V) :unit =
        raise <| EuclidException $"{msg} failed on colinear points {nl}  %O{a}{nl}  %O{b}{nl}  %O{c}."

    let failDivide (msg:string) (div:float) (obj:'T) :unit =
        raise <| EuclidDivByZeroException $"{msg}: {nl}  %O{obj} {nl}  cannot be divided by {div}."

    let failRot (x:float) (y:float) :unit =
        raise <| EuclidUnitizingException $"Rotation2D(sine {x}, cosine {y}): sin*sin + cos*cos length is not one."

    let failQuat (w:float) (x:float) (y:float) (z:float) :unit =
        raise <| EuclidUnitizingException $"Quaternion(w:{w}, x:{x}, y:{y}, z:{z}): w*w + x*x + y*y + z*z length is not one."

    let failNotOne2 (msg:string) (x:float) (y:float) :unit =
        raise <| EuclidUnitizingException $"{msg}: length of vector with components X:{x}, Y:{y} is not one."

    let failNotOne3 (msg:string) (x:float) (y:float) (z:float) :unit =
        raise <| EuclidUnitizingException $"{msg}: length of vector with components X:{x}, Y:{y}, Z:{z} is not one."

    let failNaN2 (msg:string) (x:float) (y:float)  :unit =
        raise <| EuclidNanInfinityException $"NaN or Infinity in {msg}  X:{x}, Y:{y}."

    let failNaN3 (msg:string) (x:float) (y:float) (z:float) :unit =
        raise <| EuclidNanInfinityException $"NaN or Infinity in {msg}  X:{x}, Y:{y}, Z:{z}."

    let failUnit2 (msg:string) (x:float) (y:float) :unit =
        raise <| EuclidUnitizingException $"{msg} unitizing failed for too small input: X:{x}, Y:{y}."

    let failUnit3 (msg:string) (x:float) (y:float) (z:float) :unit =
        raise <| EuclidUnitizingException $"{msg} unitizing failed for too small input: X:{x}, Y:{y}, Z:{z}."

    let failTooSmall (msg:string) (this:'T)  :unit =
        raise <| EuclidTooSmallException $"{msg}: {nl}  %O{this} {nl}  is too small."

    let failTooSmall2 (msg:string) (this:'T) (other:'U) :unit =
        raise <| EuclidTooSmallException $"{msg}: {nl}  %O{this} {nl}  is too small. Other: {nl}  %O{other}."

    let failTooClose (msg:string) (this:'T) (other:'U) :unit =
        raise <| EuclidTooSmallException $"{msg}: {nl}  %O{this} {nl}  and {nl}  %O{other} are too close to get a direction."

    let failObsoleteV30 (funName:string) (newFunName:string) :unit =
        raise <| EuclidObsoleteException $"{funName} is obsolete from Euclid version 0.20.0 or higher. Use alternative functions in :{newFunName}."

    let failObsolete (funName:string) (newFunName:string) :unit =
        raise <| EuclidObsoleteException $"{funName} is obsolete. Use alternative functions in :{newFunName}."

    let failVertical (msg:string) (v:'t) :unit =
        raise <| EuclidException $"{msg}: vector is vertical or zero length: {v}"

    let failTooFewPoly2D (name:string) (minCount:int) (actual:int):unit =
        raise <| EuclidException $"Polyline2D.{name} failed on Polyline2D with {actual} points. Minimum required is {minCount} points."

    let failTooFewPoly3D (name:string) (minCount:int) (actual:int) :unit =
        raise <| EuclidException $"Polyline3D.{name} failed on Polyline3D with {actual} points. Minimum required is {minCount} points."

    /// Raises an EuclidException when offsetting a 2D rectangle edge fails due to insufficient size.
    let failRect2DOffsetEdge(offStart, offEnd, len, edgeIdx, d) : unit =
        fail $"Rect2D.offsetEdge: the 2D Rectangle is too small to offsetEdge by {d} at edgeIdx {edgeIdx}. offStart: {offStart}, offEnd: {offEnd}, Length: {len}"

    /// Raises an EuclidException when offsetting a 3D rectangle edge fails due to insufficient size.
    let failRect3DOffsetEdge(offStart, offEnd, len, edgeIdx, d) : unit =
        fail $"Rect3D.offsetEdge: the 3D-rectangle is too small to offsetEdge by {d} at edgeIdx {edgeIdx}. offStart: {offStart}, offEnd: {offEnd}, Length: {len}"