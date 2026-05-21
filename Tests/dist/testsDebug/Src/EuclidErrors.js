
import { Exception } from "../fable_modules/fable-library-js.5.0.0/Util.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { interpolate, concat } from "../fable_modules/fable-library-js.5.0.0/String.js";

/**
 * Exception in Euclid.
 */
export class EuclidException extends Exception {
    constructor(s) {
        super(s);
    }
}

export function EuclidException_$reflection() {
    return class_type("Euclid.EuclidErrors.EuclidException", undefined, EuclidException, class_type("System.Exception"));
}

export function EuclidException_$ctor_Z721C83C5(s) {
    return new EuclidException(s);
}

/**
 * Exception for attempting to divide by a 0.0 or almost 0.0 value.
 * Almost 0.0 is defined by UtilEuclid.zeroLengthTolerance as 1e-12.
 */
export class EuclidDivByZeroException extends Exception {
    constructor(s) {
        super(s);
    }
}

export function EuclidDivByZeroException_$reflection() {
    return class_type("Euclid.EuclidErrors.EuclidDivByZeroException", undefined, EuclidDivByZeroException, class_type("System.Exception"));
}

export function EuclidDivByZeroException_$ctor_Z721C83C5(s) {
    return new EuclidDivByZeroException(s);
}

/**
 * Exception for attempting to divide by a 0.0 or almost 0.0 value during Unitizing
 * Almost 0.0 is defined by UtilEuclid.zeroLengthTolerance as 1e-12.
 */
export class EuclidUnitizingException extends Exception {
    constructor(s) {
        super(s);
    }
}

export function EuclidUnitizingException_$reflection() {
    return class_type("Euclid.EuclidErrors.EuclidUnitizingException", undefined, EuclidUnitizingException, class_type("System.Exception"));
}

export function EuclidUnitizingException_$ctor_Z721C83C5(s) {
    return new EuclidUnitizingException(s);
}

/**
 * Exception for NaN or Infinity values in inputs.
 */
export class EuclidNanInfinityException extends Exception {
    constructor(s) {
        super(s);
    }
}

export function EuclidNanInfinityException_$reflection() {
    return class_type("Euclid.EuclidErrors.EuclidNanInfinityException", undefined, EuclidNanInfinityException, class_type("System.Exception"));
}

export function EuclidNanInfinityException_$ctor_Z721C83C5(s) {
    return new EuclidNanInfinityException(s);
}

/**
 * Exception for too small input values that don't give a clear vector direction.
 */
export class EuclidTooSmallException extends Exception {
    constructor(s) {
        super(s);
    }
}

export function EuclidTooSmallException_$reflection() {
    return class_type("Euclid.EuclidErrors.EuclidTooSmallException", undefined, EuclidTooSmallException, class_type("System.Exception"));
}

export function EuclidTooSmallException_$ctor_Z721C83C5(s) {
    return new EuclidTooSmallException(s);
}

/**
 * Exception for null references in inputs.
 */
export class EuclidNullException extends Exception {
    constructor(s) {
        super(s);
    }
}

export function EuclidNullException_$reflection() {
    return class_type("Euclid.EuclidErrors.EuclidNullException", undefined, EuclidNullException, class_type("System.Exception"));
}

export function EuclidNullException_$ctor_Z721C83C5(s) {
    return new EuclidNullException(s);
}

/**
 * Exception for empty sequences in inputs.
 */
export class EuclidEmptySeqException extends Exception {
    constructor(s) {
        super(s);
    }
}

export function EuclidEmptySeqException_$reflection() {
    return class_type("Euclid.EuclidErrors.EuclidEmptySeqException", undefined, EuclidEmptySeqException, class_type("System.Exception"));
}

export function EuclidEmptySeqException_$ctor_Z721C83C5(s) {
    return new EuclidEmptySeqException(s);
}

/**
 * Exception for Obsolete features.
 */
export class EuclidObsoleteException extends Exception {
    constructor(s) {
        super(s);
    }
}

export function EuclidObsoleteException_$reflection() {
    return class_type("Euclid.EuclidErrors.EuclidObsoleteException", undefined, EuclidObsoleteException, class_type("System.Exception"));
}

export function EuclidObsoleteException_$ctor_Z721C83C5(s) {
    return new EuclidObsoleteException(s);
}

export const nl = "\n";

export function fail(msg) {
    throw EuclidException_$ctor_Z721C83C5(concat("Euclid.", ...msg));
}

export function failRarr(msg, xs) {
    throw EuclidException_$ctor_Z721C83C5(`Euclid.ResizeArr.${msg} failed on ResizeArray of ${xs.length} elements.`);
}

export function failNull(funcName, arg) {
    throw EuclidNullException_$ctor_Z721C83C5(`Euclid.${funcName} argument ${arg} is null.`);
}

export function failEmptySeq(funcName, arg) {
    throw EuclidEmptySeqException_$ctor_Z721C83C5(`Euclid.${funcName} argument ${arg} is an empty sequence.`);
}

export function fail1(msg, a) {
    throw EuclidException_$ctor_Z721C83C5(`${msg} failed on: ${nl}  ${interpolate("%O%P()", [a])}.`);
}

export function fail2(msg, a, b) {
    throw EuclidException_$ctor_Z721C83C5(`${msg} failed on ${nl}  ${interpolate("%O%P()", [a])}${nl}  ${interpolate("%O%P()", [b])}.`);
}

export function fail3(msg, a, b, c) {
    throw EuclidException_$ctor_Z721C83C5(`${msg} failed on: ${nl}  ${interpolate("%O%P()", [a])}${nl}  ${interpolate("%O%P()", [b])}${nl}  ${interpolate("%O%P()", [c])}.`);
}

export function failColinear(msg, a, b, c) {
    throw EuclidException_$ctor_Z721C83C5(`${msg} failed on colinear points ${nl}  ${interpolate("%O%P()", [a])}${nl}  ${interpolate("%O%P()", [b])}${nl}  ${interpolate("%O%P()", [c])}.`);
}

export function failDivide(msg, div, obj) {
    throw EuclidDivByZeroException_$ctor_Z721C83C5(`${msg}: ${nl}  ${interpolate("%O%P()", [obj])} ${nl}  cannot be divided by ${div}.`);
}

export function failRot(x, y) {
    throw EuclidUnitizingException_$ctor_Z721C83C5(`Rotation2D(sine ${x}, cosine ${y}): sin*sin + cos*cos length is not one.`);
}

export function failQuat(w, x, y, z) {
    throw EuclidUnitizingException_$ctor_Z721C83C5(`Quaternion(w:${w}, x:${x}, y:${y}, z:${z}): w*w + x*x + y*y + z*z length is not one.`);
}

export function failNotOne2(msg, x, y) {
    throw EuclidUnitizingException_$ctor_Z721C83C5(`${msg}: length of vector with components X:${x}, Y:${y} is not one.`);
}

export function failNotOne3(msg, x, y, z) {
    throw EuclidUnitizingException_$ctor_Z721C83C5(`${msg}: length of vector with components X:${x}, Y:${y}, Z:${z} is not one.`);
}

export function failNaN2(msg, x, y) {
    throw EuclidNanInfinityException_$ctor_Z721C83C5(`NaN or Infinity in ${msg}  X:${x}, Y:${y}.`);
}

export function failNaN3(msg, x, y, z) {
    throw EuclidNanInfinityException_$ctor_Z721C83C5(`NaN or Infinity in ${msg}  X:${x}, Y:${y}, Z:${z}.`);
}

export function failUnit2(msg, x, y) {
    throw EuclidUnitizingException_$ctor_Z721C83C5(`${msg} unitizing failed for too small input: X:${x}, Y:${y}.`);
}

export function failUnit3(msg, x, y, z) {
    throw EuclidUnitizingException_$ctor_Z721C83C5(`${msg} unitizing failed for too small input: X:${x}, Y:${y}, Z:${z}.`);
}

export function failTooSmall(msg, this$) {
    throw EuclidTooSmallException_$ctor_Z721C83C5(`${msg}: ${nl}  ${interpolate("%O%P()", [this$])} ${nl}  is too small.`);
}

export function failTooSmall2(msg, this$, other) {
    throw EuclidTooSmallException_$ctor_Z721C83C5(`${msg}: ${nl}  ${interpolate("%O%P()", [this$])} ${nl}  is too small. Other: ${nl}  ${interpolate("%O%P()", [other])}.`);
}

export function failTooClose(msg, this$, other) {
    throw EuclidTooSmallException_$ctor_Z721C83C5(`${msg}: ${nl}  ${interpolate("%O%P()", [this$])} ${nl}  and ${nl}  ${interpolate("%O%P()", [other])} are too close to get a direction.`);
}

export function failObsoleteV30(funName, newFunName) {
    throw EuclidObsoleteException_$ctor_Z721C83C5(concat(funName, " is obsolete from Euclid version 0.20.0 or higher. Use alternative functions in :", newFunName, ..."."));
}

export function failObsolete(funName, newFunName) {
    throw EuclidObsoleteException_$ctor_Z721C83C5(concat(funName, " is obsolete. Use alternative functions in :", newFunName, ..."."));
}

export function failVertical(msg, v) {
    throw EuclidException_$ctor_Z721C83C5(`${msg}: vector is vertical or zero length: ${v}`);
}

export function failTooFewPoly2D(name, minCount, actual) {
    throw EuclidException_$ctor_Z721C83C5(`Polyline2D.${name} failed on Polyline2D with ${actual} points. Minimum required is ${minCount} points.`);
}

export function failTooFewPoly3D(name, minCount, actual) {
    throw EuclidException_$ctor_Z721C83C5(`Polyline3D.${name} failed on Polyline3D with ${actual} points. Minimum required is ${minCount} points.`);
}

/**
 * Raises an EuclidException when offsetting a 2D rectangle edge fails due to insufficient size.
 */
export function failRect2DOffsetEdge(offStart, offEnd, len, edgeIdx, d) {
    return fail(`Rect2D.offsetEdge: the 2D Rectangle is too small to offsetEdge by ${d} at edgeIdx ${edgeIdx}. offStart: ${offStart}, offEnd: ${offEnd}, Length: ${len}`);
}

/**
 * Raises an EuclidException when offsetting a 3D rectangle edge fails due to insufficient size.
 */
export function failRect3DOffsetEdge(offStart, offEnd, len, edgeIdx, d) {
    return fail(`Rect3D.offsetEdge: the 3D-rectangle is too small to offsetEdge by ${d} at edgeIdx ${edgeIdx}. offStart: ${offStart}, offEnd: ${offEnd}, Length: ${len}`);
}

