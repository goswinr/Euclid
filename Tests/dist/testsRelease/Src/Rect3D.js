
import { float } from "./Format.js";
import { Pnt_$ctor_Z7AD9E565, Pnt__get_AsFSharpCode, Pnt__get_AsString } from "./Pnt.js";
import { Vec_$ctor_Z7AD9E565, Vec__get_AsFSharpCode, Vec__get_AsString } from "./Vec.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { failRect3DOffsetEdge, failDivide, fail2, failTooSmall2, fail, failTooSmall } from "./EuclidErrors.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./UnitVec.js";
import { BRect_$ctor_77D16AC0 } from "./BRect.js";
import { max, min } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { BBox_$ctor_76A78260 } from "./BBox.js";
import { PPlane__get_AsString } from "./PPlane.js";
import { Pt_$ctor_7B00E9A0 } from "./Pt.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Pnt.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Vec.js";
import { failUnit3 } from "./EuclidErrors.js";
import { count } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { setItem, fill, item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { Line3D_$ctor_5A6659A0 } from "./Line3D.js";

/**
 * A struct containing a 3D Origin point and two 3D Edge vectors,
 * representing an immutable planar 3D-rectangle with any rotation in 3D space.
 * Similar to PPlane, however the two vectors are not unitized.
 * This implementation guarantees the 3D-rectangle to be always valid.
 * That means the  X and Y axes are always perpendicular to each other.
 * However the length of one of these axes might still be zero.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export class Rect3D extends Record {
    constructor(Origin, Xaxis, Yaxis) {
        super();
        this.Origin = Origin;
        this.Xaxis = Xaxis;
        this.Yaxis = Yaxis;
    }
    /**
     * Nicely formatted string representation of the 3D-rectangle including its size.
     */
    toString() {
        let v, v_1;
        const r = this;
        return `Euclid.Rect3D ${float((v = r.Xaxis, Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))))} x ${float((v_1 = r.Yaxis, Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))))} (Origin:${Pnt__get_AsString(r.Origin)}| X-ax:${Vec__get_AsString(r.Xaxis)}|Y-ax:${Vec__get_AsString(r.Yaxis)})`;
    }
}

export function Rect3D_$reflection() {
    return class_type("Euclid.Rect3D", undefined, Rect3D, class_type("System.ValueType"));
}

/**
 * Unchecked Internal Constructor Only.
 * Creates a 3D rectangle with X, Y Direction.
 */
export function Rect3D_$ctor_6181AF53(origin, axisX, axisY) {
    return new Rect3D(origin, axisX, axisY);
}

/**
 * Format the 3D-rectangle into string with nice floating point number formatting of X and Y size only.
 * But without type name as in v.ToString()
 */
export function Rect3D__get_AsString(r) {
    let v, v_1;
    return concat(float((v = r.Xaxis, Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z)))), " x ", ...float((v_1 = r.Yaxis, Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z)))));
}

/**
 * Format Rect3D into an F# code string that can be used to recreate the rectangle.
 */
export function Rect3D__get_AsFSharpCode(r) {
    return `Rect3D.createUnchecked(${Pnt__get_AsFSharpCode(r.Origin)}, ${Vec__get_AsFSharpCode(r.Xaxis)}, ${Vec__get_AsFSharpCode(r.Yaxis)})`;
}

/**
 * Returns the unitized Normal.
 * Resulting from the Cross Product of r.Xaxis with r.Yaxis.
 */
export function Rect3D__get_NormalUnit(r) {
    const a = r.Xaxis;
    const b = r.Yaxis;
    const x = (a.Y * b.Z) - (a.Z * b.Y);
    const y = (a.Z * b.X) - (a.X * b.Z);
    const z = (a.X * b.Y) - (a.Y * b.X);
    const len = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (!(len > 1E-12)) {
        failTooSmall("Rect3D.NormalUnit: rect", r);
    }
    const f = 1 / len;
    return UnitVec_$ctor_Z7AD9E565(x * f, y * f, z * f);
}

/**
 * Gets the axis aligned 2D Bounding Rectangle of the 3D-rectangle.
 * The z-coordinate components are ignored.
 */
export function Rect3D__get_BRect(r) {
    const y = r.Yaxis;
    const p0 = r.Origin;
    let p1;
    const p = p0;
    const v = r.Xaxis;
    p1 = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
    let p2;
    const p_1 = p1;
    const v_1 = y;
    p2 = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    let p3;
    const p_2 = p0;
    const v_2 = y;
    p3 = Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z);
    return BRect_$ctor_77D16AC0(min(min(min(p0.X, p1.X), p2.X), p3.X), min(min(min(p0.Y, p1.Y), p2.Y), p3.Y), max(max(max(p0.X, p1.X), p2.X), p3.X), max(max(max(p0.Y, p1.Y), p2.Y), p3.Y));
}

/**
 * Gets the axis aligned 3D Bounding Box of the 3D-rectangle.
 */
export function Rect3D__get_BBox(r) {
    const y = r.Yaxis;
    const p0 = r.Origin;
    let p1;
    const p = p0;
    const v = r.Xaxis;
    p1 = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
    let p2;
    const p_1 = p1;
    const v_1 = y;
    p2 = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    let p3;
    const p_2 = p0;
    const v_2 = y;
    p3 = Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z);
    return BBox_$ctor_76A78260(min(min(min(p0.X, p1.X), p2.X), p3.X), min(min(min(p0.Y, p1.Y), p2.Y), p3.Y), min(min(min(p0.Z, p1.Z), p2.Z), p3.Z), max(max(max(p0.X, p1.X), p2.X), p3.X), max(max(max(p0.Y, p1.Y), p2.Y), p3.Y), max(max(max(p0.Z, p1.Z), p2.Z), p3.Z));
}

/**
 * Checks if two 3D-rectangles are equal within tolerance.
 * Does not recognize congruent rectangles with different rotation as equal.
 * Use a tolerance of 0.0 to check for an exact match.
 */
export function Rect3D_equals(tol, a, b) {
    if ((((((((Math.abs(a.Origin.X - b.Origin.X) <= tol) && (Math.abs(a.Origin.Y - b.Origin.Y) <= tol)) && (Math.abs(a.Origin.Z - b.Origin.Z) <= tol)) && (Math.abs(a.Xaxis.X - b.Xaxis.X) <= tol)) && (Math.abs(a.Xaxis.Y - b.Xaxis.Y) <= tol)) && (Math.abs(a.Xaxis.Z - b.Xaxis.Z) <= tol)) && (Math.abs(a.Yaxis.X - b.Yaxis.X) <= tol)) && (Math.abs(a.Yaxis.Y - b.Yaxis.Y) <= tol)) {
        return Math.abs(a.Yaxis.Z - b.Yaxis.Z) <= tol;
    }
    else {
        return false;
    }
}

/**
 * Check if two 3D-rectangles are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two rectangles are not exactly equal.
 */
export function Rect3D_notEquals(tol, a, b) {
    if ((((((((Math.abs(a.Origin.X - b.Origin.X) > tol) ? true : (Math.abs(a.Origin.Y - b.Origin.Y) > tol)) ? true : (Math.abs(a.Origin.Z - b.Origin.Z) > tol)) ? true : (Math.abs(a.Xaxis.X - b.Xaxis.X) > tol)) ? true : (Math.abs(a.Xaxis.Y - b.Xaxis.Y) > tol)) ? true : (Math.abs(a.Xaxis.Z - b.Xaxis.Z) > tol)) ? true : (Math.abs(a.Yaxis.X - b.Yaxis.X) > tol)) ? true : (Math.abs(a.Yaxis.Y - b.Yaxis.Y) > tol)) {
        return true;
    }
    else {
        return Math.abs(a.Yaxis.Z - b.Yaxis.Z) > tol;
    }
}

/**
 * Returns the 3D-rectangle expanded by distance on all four sides.
 * Does check for underflow if distance is negative and raises EuclidException.
 */
export function Rect3D_expand(dist, r) {
    let p_1, p, v_2, v_3, a_3, b, a_2, a_5, b_1, a_4;
    let siX;
    const v = r.Xaxis;
    siX = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let siY;
    const v_1 = r.Yaxis;
    siY = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    const d = dist * -2;
    if ((siX <= d) ? true : (siY <= d)) {
        fail(concat("Rect3D.expand: the 3D-rectangle ", Rect3D__get_AsString(r), " is too small to expand by negative distance ", ...float(dist)));
    }
    let x;
    const a = r.Xaxis;
    const f = dist / siX;
    x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = r.Yaxis;
    const f_1 = dist / siY;
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    return Rect3D_$ctor_6181AF53((p_1 = ((p = r.Origin, (v_2 = x, Pnt_$ctor_Z7AD9E565(p.X - v_2.X, p.Y - v_2.Y, p.Z - v_2.Z)))), (v_3 = y, Pnt_$ctor_Z7AD9E565(p_1.X - v_3.X, p_1.Y - v_3.Y, p_1.Z - v_3.Z))), (a_3 = r.Xaxis, (b = ((a_2 = x, Vec_$ctor_Z7AD9E565(a_2.X * 2, a_2.Y * 2, a_2.Z * 2))), Vec_$ctor_Z7AD9E565(a_3.X + b.X, a_3.Y + b.Y, a_3.Z + b.Z))), (a_5 = r.Yaxis, (b_1 = ((a_4 = y, Vec_$ctor_Z7AD9E565(a_4.X * 2, a_4.Y * 2, a_4.Z * 2))), Vec_$ctor_Z7AD9E565(a_5.X + b_1.X, a_5.Y + b_1.Y, a_5.Z + b_1.Z))));
}

/**
 * Returns the 3D-rectangle expanded by respective distances on all four sides.
 * Does check for overflow if distance is negative and fails.
 * distX, distY are for the local X and Y-axis respectively.
 */
export function Rect3D_expandXY(distX, distY, r) {
    let v_2, v_3, p_1, p, v_4, v_5, a_3, b, a_2, a_5, b_1, a_4;
    let siX;
    const v = r.Xaxis;
    siX = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let siY;
    const v_1 = r.Yaxis;
    siY = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (siX <= (distX * -2)) {
        fail(concat("Rect3D.expandXY: the 3D-rectangle ", Rect3D__get_AsString(r), " is too small to expand by negative distance distX ", ...float(distX)));
    }
    if (siY <= (distY * -2)) {
        fail(concat("Rect3D.expandXY: the 3D-rectangle ", Rect3D__get_AsString(r), " is too small to expand by negative distance distY ", ...float(distY)));
    }
    let x;
    const a = r.Xaxis;
    const f = distX / ((v_2 = r.Xaxis, Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))));
    x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = r.Yaxis;
    const f_1 = distY / ((v_3 = r.Yaxis, Math.sqrt(((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z))));
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    return Rect3D_$ctor_6181AF53((p_1 = ((p = r.Origin, (v_4 = x, Pnt_$ctor_Z7AD9E565(p.X - v_4.X, p.Y - v_4.Y, p.Z - v_4.Z)))), (v_5 = y, Pnt_$ctor_Z7AD9E565(p_1.X - v_5.X, p_1.Y - v_5.Y, p_1.Z - v_5.Z))), (a_3 = r.Xaxis, (b = ((a_2 = x, Vec_$ctor_Z7AD9E565(a_2.X * 2, a_2.Y * 2, a_2.Z * 2))), Vec_$ctor_Z7AD9E565(a_3.X + b.X, a_3.Y + b.Y, a_3.Z + b.Z))), (a_5 = r.Yaxis, (b_1 = ((a_4 = y, Vec_$ctor_Z7AD9E565(a_4.X * 2, a_4.Y * 2, a_4.Z * 2))), Vec_$ctor_Z7AD9E565(a_5.X + b_1.X, a_5.Y + b_1.Y, a_5.Z + b_1.Z))));
}

/**
 * Returns the 3D-rectangle expanded by a relative factor on all four sides.
 * Values between 0.0 and 1.0 shrink the rectangle.
 * Values larger than 1.0 expand the rectangle.
 * Does check for underflow if factor is negative and raises EuclidException.
 */
export function Rect3D_expandRel(factor, r) {
    let p_3, p_2, r_1, p_1, p, v, a_2, v_1, a_3, v_2, a_4, v_3, a_5;
    if (factor < 0) {
        fail(`Rect3D.expandRel: a negative factor ${factor} is not allowed for expanding the 3D-rectangle ${Rect3D__get_AsString(r)}`);
    }
    let x;
    const a = r.Xaxis;
    const f = factor;
    x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = r.Yaxis;
    const f_1 = factor;
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    return Rect3D_$ctor_6181AF53((p_3 = ((p_2 = ((r_1 = r, (p_1 = ((p = r_1.Origin, (v = ((a_2 = r_1.Xaxis, Vec_$ctor_Z7AD9E565(a_2.X * 0.5, a_2.Y * 0.5, a_2.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z)))), (v_1 = ((a_3 = r_1.Yaxis, Vec_$ctor_Z7AD9E565(a_3.X * 0.5, a_3.Y * 0.5, a_3.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z))))), (v_2 = ((a_4 = x, Vec_$ctor_Z7AD9E565(a_4.X * 0.5, a_4.Y * 0.5, a_4.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_2.X - v_2.X, p_2.Y - v_2.Y, p_2.Z - v_2.Z)))), (v_3 = ((a_5 = y, Vec_$ctor_Z7AD9E565(a_5.X * 0.5, a_5.Y * 0.5, a_5.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_3.X - v_3.X, p_3.Y - v_3.Y, p_3.Z - v_3.Z))), x, y);
}

/**
 * Returns the 3D-rectangle expanded by a relative factor on all four sides.
 * Values between 0.0 and 1.0 shrink the rectangle.
 * Values larger than 1.0 expand the rectangle.
 * Does check for underflow if factor is negative and raises EuclidException.
 */
export function Rect3D_expandRelXY(factorX, factorY, r) {
    let p_3, p_2, r_1, p_1, p, v, a_2, v_1, a_3, v_2, a_4, v_3, a_5;
    if (factorX < 0) {
        fail(`Rect3D.expandRelXY: a negative factor ${factorX} is not allowed for expanding the 3D-rectangle ${Rect3D__get_AsString(r)}`);
    }
    if (factorY < 0) {
        fail(`Rect3D.expandRelXY: a negative factor ${factorY} is not allowed for expanding the 3D-rectangle ${Rect3D__get_AsString(r)}`);
    }
    let x;
    const a = r.Xaxis;
    const f = factorX;
    x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = r.Yaxis;
    const f_1 = factorY;
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    return Rect3D_$ctor_6181AF53((p_3 = ((p_2 = ((r_1 = r, (p_1 = ((p = r_1.Origin, (v = ((a_2 = r_1.Xaxis, Vec_$ctor_Z7AD9E565(a_2.X * 0.5, a_2.Y * 0.5, a_2.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z)))), (v_1 = ((a_3 = r_1.Yaxis, Vec_$ctor_Z7AD9E565(a_3.X * 0.5, a_3.Y * 0.5, a_3.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z))))), (v_2 = ((a_4 = x, Vec_$ctor_Z7AD9E565(a_4.X * 0.5, a_4.Y * 0.5, a_4.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_2.X - v_2.X, p_2.Y - v_2.Y, p_2.Z - v_2.Z)))), (v_3 = ((a_5 = y, Vec_$ctor_Z7AD9E565(a_5.X * 0.5, a_5.Y * 0.5, a_5.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_3.X - v_3.X, p_3.Y - v_3.Y, p_3.Z - v_3.Z))), x, y);
}

/**
 * Create a 3D-rectangle from the origin point, an x-edge and an y-edge.
 * Fails if x and y are not perpendicularity.
 * Fails on vectors shorter than 1e-12.
 */
export function Rect3D_createFromVectors_6181AF53(origin, x, y) {
    let v, v_1, a, b;
    if (!(((v = x, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) > 1E-12)) {
        failTooSmall2("Rect3D.createFromVectors x", x, y);
    }
    if (!(((v_1 = y, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) > 1E-12)) {
        failTooSmall2("Rect3D.createFromVectors y", y, x);
    }
    if (Math.abs((a = x, (b = y, ((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z)))) > 1E-12) {
        fail2("Rect3D.createFromVectors: X-axis and Y-axis are not perpendicular", x, y);
    }
    return Rect3D_$ctor_6181AF53(origin, x, y);
}

/**
 * Give PPlane and sizes.
 * The Rect3D's Origin will be at the plane's Origin.
 * Fails on negative sizes.
 */
export function Rect3D_createFromPlane_Z39588001(pl, sizeX, sizeY) {
    let a, f, a_1, f_1;
    if (!(sizeX >= 0)) {
        fail(`Rect3D.createFromPlane sizeX is negative: ${sizeX}, sizeY is: ${sizeY}, plane: ${PPlane__get_AsString(pl)}`);
    }
    if (!(sizeY >= 0)) {
        fail(`Rect3D.createFromPlane sizeY is negative: ${sizeY}, sizeX is: ${sizeX}, plane: ${PPlane__get_AsString(pl)}`);
    }
    return Rect3D_$ctor_6181AF53(pl.Origin, (a = pl.Xaxis, (f = sizeX, Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f))), (a_1 = pl.Yaxis, (f_1 = sizeY, Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1))));
}

/**
 * Give PPlane and sizes.
 * The Rect3D's Center will be at the plane's Origin.
 * Fails on negative sizes.
 */
export function Rect3D_createCenteredFromPlane_Z39588001(pl, sizeX, sizeY) {
    let p_1, p, v, a_2, v_1, a_3;
    if (!(sizeX >= 0)) {
        fail(`Rect3D.createCenteredFromPlane sizeX is negative: ${sizeX}, sizeY is: ${sizeY}, plane: ${PPlane__get_AsString(pl)}`);
    }
    if (!(sizeY >= 0)) {
        fail(`Rect3D.createCenteredFromPlane sizeY is negative: ${sizeY}, sizeX is: ${sizeX}, plane: ${PPlane__get_AsString(pl)}`);
    }
    let x_2;
    const a = pl.Xaxis;
    const f = sizeX;
    x_2 = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = pl.Yaxis;
    const f_1 = sizeY;
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    return Rect3D_$ctor_6181AF53((p_1 = ((p = pl.Origin, (v = ((a_2 = x_2, Vec_$ctor_Z7AD9E565(a_2.X * 0.5, a_2.Y * 0.5, a_2.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p.X - v.X, p.Y - v.Y, p.Z - v.Z)))), (v_1 = ((a_3 = y, Vec_$ctor_Z7AD9E565(a_3.X * 0.5, a_3.Y * 0.5, a_3.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_1.X - v_1.X, p_1.Y - v_1.Y, p_1.Z - v_1.Z))), x_2, y);
}

/**
 * Give 2D Bounding Rect.
 */
export function Rect3D_createFromBRect_38AA877B(b) {
    let p, r, a, f, r_1, a_1, f_1, r_2;
    return Rect3D_$ctor_6181AF53((p = ((r = b, Pt_$ctor_7B00E9A0(r.MinX, r.MinY))), Pnt_$ctor_Z7AD9E565_1(p.X, p.Y, 0)), (a = Vec_$ctor_Z7AD9E565_1(1, 0, 0), (f = ((r_1 = b, r_1.MaxX - r_1.MinX)), Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f))), (a_1 = Vec_$ctor_Z7AD9E565_1(0, 1, 0), (f_1 = ((r_2 = b, r_2.MaxY - r_2.MinY)), Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1))));
}

/**
 * Creates a 3D-rectangle from three points. Fails if points are too close to each other or all colinear.
 * The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
 * Origin and x-point define the X-axis orientation of the Rectangle.
 * The y-point only defines the length and side of the Y axis.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D_createFrom3Points_6180BC13(origin, xPt, yPt) {
    let v, v_1, v_2, a_5, f_1, a_4, b_4;
    let x;
    const a = xPt;
    const b = origin;
    x = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    if (!(((v = x, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) > 1E-12)) {
        fail(`Rect3D.createFrom3Points: X-Point ${Pnt__get_AsString(xPt)} too close to origin: ${Pnt__get_AsString(origin)}.`);
    }
    let y;
    const a_1 = yPt;
    const b_1 = origin;
    y = Vec_$ctor_Z7AD9E565(a_1.X - b_1.X, a_1.Y - b_1.Y, a_1.Z - b_1.Z);
    if (!(((v_1 = y, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) > 1E-12)) {
        fail(`Rect3D.createFrom3Points: Y-Point ${Pnt__get_AsString(yPt)} too close to origin: ${Pnt__get_AsString(origin)}.`);
    }
    let z;
    const a_2 = x;
    const b_2 = y;
    z = Vec_$ctor_Z7AD9E565((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X));
    if (!(((v_2 = z, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))) > 1E-12)) {
        fail(concat("Rect3D.createFrom3Points: Y-Point ", Pnt__get_AsString(yPt), ..." is too close to Xaxis."));
    }
    let yu;
    let v_3;
    const a_3 = z;
    const b_3 = x;
    v_3 = Vec_$ctor_Z7AD9E565((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X));
    const x_4 = v_3.X;
    const y_1 = v_3.Y;
    const z_1 = v_3.Z;
    const l = Math.sqrt(((x_4 * x_4) + (y_1 * y_1)) + (z_1 * z_1));
    if (!(l > 1E-12)) {
        failUnit3("Vec.Unitized", x_4, y_1, z_1);
    }
    const f = 1 / l;
    yu = UnitVec_$ctor_Z7AD9E565(f * x_4, f * y_1, f * z_1);
    return Rect3D_$ctor_6181AF53(origin, x, (a_5 = yu, (f_1 = ((a_4 = yu, (b_4 = y, ((a_4.X * b_4.X) + (a_4.Y * b_4.Y)) + (a_4.Z * b_4.Z)))), Vec_$ctor_Z7AD9E565(a_5.X * f_1, a_5.Y * f_1, a_5.Z * f_1))));
}

/**
 * Tries to create a 3D-rectangle from three points. Returns None if points are too close to each other or all colinear..
 * The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
 * Origin and x-point define the X-axis orientation of the Rectangle.
 * The y-point only defines the length and side of the Y axis.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D_tryCreateFrom3Points_6180BC13(origin, xPt, yPt) {
    let v, v_1, v_2, a_5, f_1, a_4, b_4;
    let x;
    const a = xPt;
    const b = origin;
    x = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    if (!(((v = x, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) > 1E-12)) {
        return undefined;
    }
    else {
        let y;
        const a_1 = yPt;
        const b_1 = origin;
        y = Vec_$ctor_Z7AD9E565(a_1.X - b_1.X, a_1.Y - b_1.Y, a_1.Z - b_1.Z);
        if (!(((v_1 = y, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) > 1E-12)) {
            return undefined;
        }
        else {
            let z;
            const a_2 = x;
            const b_2 = y;
            z = Vec_$ctor_Z7AD9E565((a_2.Y * b_2.Z) - (a_2.Z * b_2.Y), (a_2.Z * b_2.X) - (a_2.X * b_2.Z), (a_2.X * b_2.Y) - (a_2.Y * b_2.X));
            if (!(((v_2 = z, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))) > 1E-12)) {
                return undefined;
            }
            else {
                let yu;
                let v_3;
                const a_3 = z;
                const b_3 = x;
                v_3 = Vec_$ctor_Z7AD9E565((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X));
                const x_4 = v_3.X;
                const y_1 = v_3.Y;
                const z_1 = v_3.Z;
                const l = Math.sqrt(((x_4 * x_4) + (y_1 * y_1)) + (z_1 * z_1));
                if (!(l > 1E-12)) {
                    failUnit3("Vec.Unitized", x_4, y_1, z_1);
                }
                const f = 1 / l;
                yu = UnitVec_$ctor_Z7AD9E565(f * x_4, f * y_1, f * z_1);
                return Rect3D_$ctor_6181AF53(origin, x, (a_5 = yu, (f_1 = ((a_4 = yu, (b_4 = y, ((a_4.X * b_4.X) + (a_4.Y * b_4.Y)) + (a_4.Z * b_4.Z)))), Vec_$ctor_Z7AD9E565(a_5.X * f_1, a_5.Y * f_1, a_5.Z * f_1))));
            }
        }
    }
}

/**
 * Creates a new 3D rectangle( = oriented bounding rectangle ) to contain the projections of all given points.
 * But not the corners of the reference rectangle.
 * Keeps the same plane and the same X- and Y-axis orientation as the input rectangle.
 * For a 3D oriented bounding box use the Box.createFromPlaneAndPoints function.
 */
export function Rect3D_fitToPoints(pts, refRect) {
    let a_5, f_4, a_6, f_5;
    const o = refRect.Origin;
    let x_3;
    const v = refRect.Xaxis;
    const x = v.X;
    const y = v.Y;
    const z = v.Z;
    const l = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (!(l > 1E-12)) {
        failUnit3("Vec.Unitized", x, y, z);
    }
    const f = 1 / l;
    x_3 = UnitVec_$ctor_Z7AD9E565(f * x, f * y, f * z);
    let y_4;
    const v_1 = refRect.Yaxis;
    const x_4 = v_1.X;
    const y_2 = v_1.Y;
    const z_2 = v_1.Z;
    const l_1 = Math.sqrt(((x_4 * x_4) + (y_2 * y_2)) + (z_2 * z_2));
    if (!(l_1 > 1E-12)) {
        failUnit3("Vec.Unitized", x_4, y_2, z_2);
    }
    const f_1 = 1 / l_1;
    y_4 = UnitVec_$ctor_Z7AD9E565(f_1 * x_4, f_1 * y_2, f_1 * z_2);
    let minX = 1.7976931348623157E+308;
    let minY = 1.7976931348623157E+308;
    let maxX = -1.7976931348623157E+308;
    let maxY = -1.7976931348623157E+308;
    for (let i = 0; i <= (count(pts) - 1); i++) {
        let v_2;
        const a = item(i, pts);
        const b = o;
        v_2 = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
        let dotX;
        const a_1 = v_2;
        const b_1 = x_3;
        dotX = (((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z));
        minX = min(minX, dotX);
        maxX = max(maxX, dotX);
        let dotY;
        const a_2 = v_2;
        const b_2 = y_4;
        dotY = (((a_2.X * b_2.X) + (a_2.Y * b_2.Y)) + (a_2.Z * b_2.Z));
        minY = min(minY, dotY);
        maxY = max(maxY, dotY);
    }
    let bo;
    let p_1;
    const p = o;
    let v_3;
    const a_3 = x_3;
    const f_2 = minX;
    v_3 = Vec_$ctor_Z7AD9E565(a_3.X * f_2, a_3.Y * f_2, a_3.Z * f_2);
    p_1 = Pnt_$ctor_Z7AD9E565(p.X + v_3.X, p.Y + v_3.Y, p.Z + v_3.Z);
    let v_4;
    const a_4 = y_4;
    const f_3 = minY;
    v_4 = Vec_$ctor_Z7AD9E565(a_4.X * f_3, a_4.Y * f_3, a_4.Z * f_3);
    bo = Pnt_$ctor_Z7AD9E565(p_1.X + v_4.X, p_1.Y + v_4.Y, p_1.Z + v_4.Z);
    const sizeX = maxX - minX;
    const sizeY = maxY - minY;
    return Rect3D_$ctor_6181AF53(bo, (a_5 = x_3, (f_4 = sizeX, Vec_$ctor_Z7AD9E565(a_5.X * f_4, a_5.Y * f_4, a_5.Z * f_4))), (a_6 = y_4, (f_5 = sizeY, Vec_$ctor_Z7AD9E565(a_6.X * f_5, a_6.Y * f_5, a_6.Z * f_5))));
}

/**
 * Translate along the local X-axis of the 3D-rectangle.
 */
export function Rect3D_translateLocalX(distX, r) {
    let p, v_1, a, f;
    const x = r.Xaxis;
    let len;
    const v = x;
    len = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(len > 1E-12)) {
        failTooSmall("Rect3D.translateLocalX: Xaxis", r);
    }
    return Rect3D_$ctor_6181AF53((p = r.Origin, (v_1 = ((a = x, (f = (distX / len), Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z))), x, r.Yaxis);
}

/**
 * Translate along the local Y-axis of the 3D-rectangle.
 */
export function Rect3D_translateLocalY(distY, r) {
    let p, v_1, a, f;
    const y = r.Yaxis;
    let len;
    const v = y;
    len = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(len > 1E-12)) {
        failTooSmall("Rect3D.translateLocalY: Yaxis", r);
    }
    return Rect3D_$ctor_6181AF53((p = r.Origin, (v_1 = ((a = y, (f = (distY / len), Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z))), r.Xaxis, y);
}

/**
 * Translate by a 3D vector.(same as Rect3D.move)
 */
export function Rect3D_translate(v, r) {
    let p, v_1;
    return Rect3D_$ctor_6181AF53((p = r.Origin, (v_1 = v, Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z))), r.Xaxis, r.Yaxis);
}

/**
 * Move the 3D-rectangle by a vector.(same as Rect3D.translate)
 */
export function Rect3D_move(v, r) {
    let p, v_1;
    return Rect3D_$ctor_6181AF53((p = r.Origin, (v_1 = v, Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z))), r.Xaxis, r.Yaxis);
}

/**
 * Offset or Translate along the local Z-axis.
 * The local Z-axis is calculated from Cross Product of X and Y-axis of the 3D-rectangle.
 */
export function Rect3D_offsetZ(offsetDistance, r) {
    let p, v_1, a_1, f;
    let z;
    const a = r.Xaxis;
    const b = r.Yaxis;
    z = Vec_$ctor_Z7AD9E565((a.Y * b.Z) - (a.Z * b.Y), (a.Z * b.X) - (a.X * b.Z), (a.X * b.Y) - (a.Y * b.X));
    let len;
    const v = z;
    len = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(len > 1E-12)) {
        failTooSmall("Rect3D.offsetZ: rect", r);
    }
    return Rect3D_$ctor_6181AF53((p = r.Origin, (v_1 = ((a_1 = z, (f = (offsetDistance / len), Vec_$ctor_Z7AD9E565(a_1.X * f, a_1.Y * f, a_1.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z))), r.Xaxis, r.Yaxis);
}

/**
 * Offset a Rect3D like a Polyline inwards by a given distance.
 * Negative distances will offset outwards.
 * Fails if the distance is larger than half the size of the rectangle.
 */
export function Rect3D_offset(dist, rect) {
    let p_1, p, v_2, v_3, a_3, b, a_2, a_5, b_1, a_4;
    let xl;
    const v = rect.Xaxis;
    xl = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let yl;
    const v_1 = rect.Yaxis;
    yl = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if ((xl < (dist * 2)) ? true : (yl < (dist * 2))) {
        fail(concat("Rect3D.offset: the 3D-rectangle ", Rect3D__get_AsString(rect), " is too small to offset by distance ", ...float(dist)));
    }
    let x;
    const a = rect.Xaxis;
    const f = dist / xl;
    x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = rect.Yaxis;
    const f_1 = dist / yl;
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    return Rect3D_$ctor_6181AF53((p_1 = ((p = rect.Origin, (v_2 = x, Pnt_$ctor_Z7AD9E565(p.X + v_2.X, p.Y + v_2.Y, p.Z + v_2.Z)))), (v_3 = y, Pnt_$ctor_Z7AD9E565(p_1.X + v_3.X, p_1.Y + v_3.Y, p_1.Z + v_3.Z))), (a_3 = rect.Xaxis, (b = ((a_2 = x, Vec_$ctor_Z7AD9E565(a_2.X * 2, a_2.Y * 2, a_2.Z * 2))), Vec_$ctor_Z7AD9E565(a_3.X - b.X, a_3.Y - b.Y, a_3.Z - b.Z))), (a_5 = rect.Yaxis, (b_1 = ((a_4 = y, Vec_$ctor_Z7AD9E565(a_4.X * 2, a_4.Y * 2, a_4.Z * 2))), Vec_$ctor_Z7AD9E565(a_5.X - b_1.X, a_5.Y - b_1.Y, a_5.Z - b_1.Z))));
}

/**
 * Offset a Rect3D like a Polyline inwards by four distances.
 * The distance array is for Edge01, Edge12, Edge23, and Edge30 respectively.
 * Fails if the distance is larger than half the size of the rectangle.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D_offsetVar(dist, rect) {
    let p_1, p, v_2, v_3, a_5, a_4, b, b_1, a_7, a_6, b_2, b_3;
    if (dist.length !== 4) {
        fail(`Rect3D.offsetVar: the distance array must have 4 elements, but has ${dist.length}`);
    }
    let xl;
    const v = rect.Xaxis;
    xl = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let yl;
    const v_1 = rect.Yaxis;
    yl = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if ((xl < (item(1, dist) + item(3, dist))) ? true : (yl < (item(0, dist) + item(2, dist)))) {
        fail(`Rect3D.offsetVar: the 3D-rectangle ${Rect3D__get_AsString(rect)} is too small to offset by distances [|${float(item(0, dist))};${float(item(1, dist))};${float(item(2, dist))};${float(item(3, dist))}|]`);
    }
    let x0;
    const a = rect.Xaxis;
    const f = item(3, dist) / xl;
    x0 = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let x1;
    const a_1 = rect.Xaxis;
    const f_1 = item(1, dist) / xl;
    x1 = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    let y0;
    const a_2 = rect.Yaxis;
    const f_2 = item(0, dist) / yl;
    y0 = Vec_$ctor_Z7AD9E565(a_2.X * f_2, a_2.Y * f_2, a_2.Z * f_2);
    let y1;
    const a_3 = rect.Yaxis;
    const f_3 = item(2, dist) / yl;
    y1 = Vec_$ctor_Z7AD9E565(a_3.X * f_3, a_3.Y * f_3, a_3.Z * f_3);
    return Rect3D_$ctor_6181AF53((p_1 = ((p = rect.Origin, (v_2 = x0, Pnt_$ctor_Z7AD9E565(p.X + v_2.X, p.Y + v_2.Y, p.Z + v_2.Z)))), (v_3 = y0, Pnt_$ctor_Z7AD9E565(p_1.X + v_3.X, p_1.Y + v_3.Y, p_1.Z + v_3.Z))), (a_5 = ((a_4 = rect.Xaxis, (b = x0, Vec_$ctor_Z7AD9E565(a_4.X - b.X, a_4.Y - b.Y, a_4.Z - b.Z)))), (b_1 = x1, Vec_$ctor_Z7AD9E565(a_5.X - b_1.X, a_5.Y - b_1.Y, a_5.Z - b_1.Z))), (a_7 = ((a_6 = rect.Yaxis, (b_2 = y0, Vec_$ctor_Z7AD9E565(a_6.X - b_2.X, a_6.Y - b_2.Y, a_6.Z - b_2.Z)))), (b_3 = y1, Vec_$ctor_Z7AD9E565(a_7.X - b_3.X, a_7.Y - b_3.Y, a_7.Z - b_3.Z))));
}

/**
 * Offsets a local Rect3D at one of the four corners.
 */
export function Rect3D_offsetCorner_Z713DB0EE(rect, corner, xOffset, yOffset, xWidth, yHeight) {
    let p_1, p, v_4, v_5, p_3, p_2, v_8, v_9, p_5, p_4, v_12, v_13, p_7, p_6, v_16, v_17;
    const xa = rect.Xaxis;
    const ya = rect.Yaxis;
    let xl;
    const v = xa;
    xl = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let yl;
    const v_1 = ya;
    yl = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (!(xl > 1E-12)) {
        failTooSmall("Rect3D.offsetCorner: Xaxis", rect);
    }
    if (!(yl > 1E-12)) {
        failTooSmall("Rect3D.offsetCorner: Yaxis", rect);
    }
    let xv;
    const a = xa;
    const f = xWidth / xl;
    xv = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let yv;
    const a_1 = ya;
    const f_1 = yHeight / yl;
    yv = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    switch (corner) {
        case 0: {
            let x_3;
            let v_2;
            const a_2 = xa;
            const f_2 = xOffset;
            v_2 = Vec_$ctor_Z7AD9E565(a_2.X * f_2, a_2.Y * f_2, a_2.Z * f_2);
            const f_3 = xl;
            if (!(Math.abs(f_3) > 1E-12)) {
                failDivide("\'/\' operator", f_3, v_2);
            }
            x_3 = Vec_$ctor_Z7AD9E565(v_2.X / f_3, v_2.Y / f_3, v_2.Z / f_3);
            let y;
            let v_3;
            const a_3 = ya;
            const f_4 = yOffset;
            v_3 = Vec_$ctor_Z7AD9E565(a_3.X * f_4, a_3.Y * f_4, a_3.Z * f_4);
            const f_5 = yl;
            if (!(Math.abs(f_5) > 1E-12)) {
                failDivide("\'/\' operator", f_5, v_3);
            }
            y = Vec_$ctor_Z7AD9E565(v_3.X / f_5, v_3.Y / f_5, v_3.Z / f_5);
            return Rect3D_$ctor_6181AF53((p_1 = ((p = rect.Origin, (v_4 = x_3, Pnt_$ctor_Z7AD9E565(p.X + v_4.X, p.Y + v_4.Y, p.Z + v_4.Z)))), (v_5 = y, Pnt_$ctor_Z7AD9E565(p_1.X + v_5.X, p_1.Y + v_5.Y, p_1.Z + v_5.Z))), xv, yv);
        }
        case 1: {
            let x_7;
            let v_6;
            const a_4 = xa;
            const f_6 = (xl - xOffset) - xWidth;
            v_6 = Vec_$ctor_Z7AD9E565(a_4.X * f_6, a_4.Y * f_6, a_4.Z * f_6);
            const f_7 = xl;
            if (!(Math.abs(f_7) > 1E-12)) {
                failDivide("\'/\' operator", f_7, v_6);
            }
            x_7 = Vec_$ctor_Z7AD9E565(v_6.X / f_7, v_6.Y / f_7, v_6.Z / f_7);
            let y_2;
            let v_7;
            const a_5 = ya;
            const f_8 = yOffset;
            v_7 = Vec_$ctor_Z7AD9E565(a_5.X * f_8, a_5.Y * f_8, a_5.Z * f_8);
            const f_9 = yl;
            if (!(Math.abs(f_9) > 1E-12)) {
                failDivide("\'/\' operator", f_9, v_7);
            }
            y_2 = Vec_$ctor_Z7AD9E565(v_7.X / f_9, v_7.Y / f_9, v_7.Z / f_9);
            return Rect3D_$ctor_6181AF53((p_3 = ((p_2 = rect.Origin, (v_8 = x_7, Pnt_$ctor_Z7AD9E565(p_2.X + v_8.X, p_2.Y + v_8.Y, p_2.Z + v_8.Z)))), (v_9 = y_2, Pnt_$ctor_Z7AD9E565(p_3.X + v_9.X, p_3.Y + v_9.Y, p_3.Z + v_9.Z))), xv, yv);
        }
        case 2: {
            let x_11;
            let v_10;
            const a_6 = xa;
            const f_10 = (xl - xOffset) - xWidth;
            v_10 = Vec_$ctor_Z7AD9E565(a_6.X * f_10, a_6.Y * f_10, a_6.Z * f_10);
            const f_11 = xl;
            if (!(Math.abs(f_11) > 1E-12)) {
                failDivide("\'/\' operator", f_11, v_10);
            }
            x_11 = Vec_$ctor_Z7AD9E565(v_10.X / f_11, v_10.Y / f_11, v_10.Z / f_11);
            let y_4;
            let v_11;
            const a_7 = ya;
            const f_12 = (yl - yOffset) - yHeight;
            v_11 = Vec_$ctor_Z7AD9E565(a_7.X * f_12, a_7.Y * f_12, a_7.Z * f_12);
            const f_13 = yl;
            if (!(Math.abs(f_13) > 1E-12)) {
                failDivide("\'/\' operator", f_13, v_11);
            }
            y_4 = Vec_$ctor_Z7AD9E565(v_11.X / f_13, v_11.Y / f_13, v_11.Z / f_13);
            return Rect3D_$ctor_6181AF53((p_5 = ((p_4 = rect.Origin, (v_12 = x_11, Pnt_$ctor_Z7AD9E565(p_4.X + v_12.X, p_4.Y + v_12.Y, p_4.Z + v_12.Z)))), (v_13 = y_4, Pnt_$ctor_Z7AD9E565(p_5.X + v_13.X, p_5.Y + v_13.Y, p_5.Z + v_13.Z))), xv, yv);
        }
        case 3: {
            let x_15;
            let v_14;
            const a_8 = xa;
            const f_14 = xOffset;
            v_14 = Vec_$ctor_Z7AD9E565(a_8.X * f_14, a_8.Y * f_14, a_8.Z * f_14);
            const f_15 = xl;
            if (!(Math.abs(f_15) > 1E-12)) {
                failDivide("\'/\' operator", f_15, v_14);
            }
            x_15 = Vec_$ctor_Z7AD9E565(v_14.X / f_15, v_14.Y / f_15, v_14.Z / f_15);
            let y_6;
            let v_15;
            const a_9 = ya;
            const f_16 = (yl - yOffset) - yHeight;
            v_15 = Vec_$ctor_Z7AD9E565(a_9.X * f_16, a_9.Y * f_16, a_9.Z * f_16);
            const f_17 = yl;
            if (!(Math.abs(f_17) > 1E-12)) {
                failDivide("\'/\' operator", f_17, v_15);
            }
            y_6 = Vec_$ctor_Z7AD9E565(v_15.X / f_17, v_15.Y / f_17, v_15.Z / f_17);
            return Rect3D_$ctor_6181AF53((p_7 = ((p_6 = rect.Origin, (v_16 = x_15, Pnt_$ctor_Z7AD9E565(p_6.X + v_16.X, p_6.Y + v_16.Y, p_6.Z + v_16.Z)))), (v_17 = y_6, Pnt_$ctor_Z7AD9E565(p_7.X + v_17.X, p_7.Y + v_17.Y, p_7.Z + v_17.Z))), xv, yv);
        }
        default:
            return fail(`Rect3D.offsetCorner: corner ${corner} out of range 0..3`);
    }
}

/**
 * Offsets a local Rect3D at one of the four edges.
 */
export function Rect3D_offsetEdge_Z713DB0EE(rect, edgeIdx, offEdge, width, offStart, offEnd) {
    let p_1, p, v_2, a, f, v_3, a_1, f_1, a_2, f_2, a_3, f_3, p_3, p_2, v_4, a_4, f_4, v_5, a_5, f_5, a_6, f_6, a_7, f_7, yy_2, p_5, p_4, v_6, a_8, f_8, v_7, a_9, f_9, a_10, f_10, a_11, f_11, p_7, p_6, v_8, a_12, f_12, v_9, a_13, f_13, a_14, f_14, a_15, f_15, yy_4, p_9, p_8, v_10, a_16, f_16, v_11, a_17, f_17, a_18, f_18, a_19, f_19, p_11, p_10, v_12, a_20, f_20, v_13, a_21, f_21, a_22, f_22, a_23, f_23, yy_6, p_13, p_12, v_14, a_24, f_24, v_15, a_25, f_25, a_26, f_26, a_27, f_27, p_15, p_14, v_16, a_28, f_28, v_17, a_29, f_29, a_30, f_30, a_31, f_31;
    const x = rect.Xaxis;
    const y = rect.Yaxis;
    let lx;
    const v = x;
    lx = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let ly;
    const v_1 = y;
    ly = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (!(lx > 1E-12) ? true : !(ly > 1E-12)) {
        failTooSmall("Rect3D.offsetEdge: Xaxis or Yaxis", rect);
    }
    if (width > 1E-06) {
        switch (edgeIdx) {
            case 0: {
                let x_3;
                const d = (lx - offStart) - offEnd;
                x_3 = ((d > 1E-06) ? d : failRect3DOffsetEdge(offStart, offEnd, lx, edgeIdx, d));
                return Rect3D_$ctor_6181AF53((p_1 = ((p = rect.Origin, (v_2 = ((a = x, (f = (offStart / lx), Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_2.X, p.Y + v_2.Y, p.Z + v_2.Z)))), (v_3 = ((a_1 = y, (f_1 = (offEdge / ly), Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1)))), Pnt_$ctor_Z7AD9E565(p_1.X + v_3.X, p_1.Y + v_3.Y, p_1.Z + v_3.Z))), (a_2 = x, (f_2 = (x_3 / lx), Vec_$ctor_Z7AD9E565(a_2.X * f_2, a_2.Y * f_2, a_2.Z * f_2))), (a_3 = y, (f_3 = (width / ly), Vec_$ctor_Z7AD9E565(a_3.X * f_3, a_3.Y * f_3, a_3.Z * f_3))));
            }
            case 1: {
                let y_2;
                const d_5 = (ly - offStart) - offEnd;
                y_2 = ((d_5 > 1E-06) ? d_5 : failRect3DOffsetEdge(offStart, offEnd, ly, edgeIdx, d_5));
                return Rect3D_$ctor_6181AF53((p_3 = ((p_2 = rect.Origin, (v_4 = ((a_4 = x, (f_4 = (((lx - offEdge) - width) / lx), Vec_$ctor_Z7AD9E565(a_4.X * f_4, a_4.Y * f_4, a_4.Z * f_4)))), Pnt_$ctor_Z7AD9E565(p_2.X + v_4.X, p_2.Y + v_4.Y, p_2.Z + v_4.Z)))), (v_5 = ((a_5 = y, (f_5 = (offStart / ly), Vec_$ctor_Z7AD9E565(a_5.X * f_5, a_5.Y * f_5, a_5.Z * f_5)))), Pnt_$ctor_Z7AD9E565(p_3.X + v_5.X, p_3.Y + v_5.Y, p_3.Z + v_5.Z))), (a_6 = x, (f_6 = (width / lx), Vec_$ctor_Z7AD9E565(a_6.X * f_6, a_6.Y * f_6, a_6.Z * f_6))), (a_7 = y, (f_7 = (y_2 / ly), Vec_$ctor_Z7AD9E565(a_7.X * f_7, a_7.Y * f_7, a_7.Z * f_7))));
            }
            case 2: {
                let x_6;
                const d_10 = (lx - offStart) - offEnd;
                x_6 = ((d_10 > 1E-06) ? d_10 : failRect3DOffsetEdge(offStart, offEnd, lx, edgeIdx, d_10));
                return Rect3D_$ctor_6181AF53((yy_2 = ((ly - offEdge) - width), (p_5 = ((p_4 = rect.Origin, (v_6 = ((a_8 = x, (f_8 = (offEnd / lx), Vec_$ctor_Z7AD9E565(a_8.X * f_8, a_8.Y * f_8, a_8.Z * f_8)))), Pnt_$ctor_Z7AD9E565(p_4.X + v_6.X, p_4.Y + v_6.Y, p_4.Z + v_6.Z)))), (v_7 = ((a_9 = y, (f_9 = (yy_2 / ly), Vec_$ctor_Z7AD9E565(a_9.X * f_9, a_9.Y * f_9, a_9.Z * f_9)))), Pnt_$ctor_Z7AD9E565(p_5.X + v_7.X, p_5.Y + v_7.Y, p_5.Z + v_7.Z)))), (a_10 = x, (f_10 = (x_6 / lx), Vec_$ctor_Z7AD9E565(a_10.X * f_10, a_10.Y * f_10, a_10.Z * f_10))), (a_11 = y, (f_11 = (width / ly), Vec_$ctor_Z7AD9E565(a_11.X * f_11, a_11.Y * f_11, a_11.Z * f_11))));
            }
            case 3: {
                let y_5;
                const d_15 = (ly - offStart) - offEnd;
                y_5 = ((d_15 > 1E-06) ? d_15 : failRect3DOffsetEdge(offStart, offEnd, ly, edgeIdx, d_15));
                return Rect3D_$ctor_6181AF53((p_7 = ((p_6 = rect.Origin, (v_8 = ((a_12 = x, (f_12 = (offEdge / lx), Vec_$ctor_Z7AD9E565(a_12.X * f_12, a_12.Y * f_12, a_12.Z * f_12)))), Pnt_$ctor_Z7AD9E565(p_6.X + v_8.X, p_6.Y + v_8.Y, p_6.Z + v_8.Z)))), (v_9 = ((a_13 = y, (f_13 = (offEnd / ly), Vec_$ctor_Z7AD9E565(a_13.X * f_13, a_13.Y * f_13, a_13.Z * f_13)))), Pnt_$ctor_Z7AD9E565(p_7.X + v_9.X, p_7.Y + v_9.Y, p_7.Z + v_9.Z))), (a_14 = x, (f_14 = (width / lx), Vec_$ctor_Z7AD9E565(a_14.X * f_14, a_14.Y * f_14, a_14.Z * f_14))), (a_15 = y, (f_15 = (y_5 / ly), Vec_$ctor_Z7AD9E565(a_15.X * f_15, a_15.Y * f_15, a_15.Z * f_15))));
            }
            default:
                return fail(`Rect3D.offsetEdge: edgeIdx ${edgeIdx} out of range 0..3`);
        }
    }
    else if (width < -1E-06) {
        switch (edgeIdx) {
            case 0: {
                let x_9;
                const d_20 = (lx - offStart) - offEnd;
                x_9 = ((d_20 > 1E-06) ? d_20 : failRect3DOffsetEdge(offStart, offEnd, lx, edgeIdx, d_20));
                return Rect3D_$ctor_6181AF53((yy_4 = (offEdge + width), (p_9 = ((p_8 = rect.Origin, (v_10 = ((a_16 = x, (f_16 = (offStart / lx), Vec_$ctor_Z7AD9E565(a_16.X * f_16, a_16.Y * f_16, a_16.Z * f_16)))), Pnt_$ctor_Z7AD9E565(p_8.X + v_10.X, p_8.Y + v_10.Y, p_8.Z + v_10.Z)))), (v_11 = ((a_17 = y, (f_17 = (yy_4 / ly), Vec_$ctor_Z7AD9E565(a_17.X * f_17, a_17.Y * f_17, a_17.Z * f_17)))), Pnt_$ctor_Z7AD9E565(p_9.X + v_11.X, p_9.Y + v_11.Y, p_9.Z + v_11.Z)))), (a_18 = x, (f_18 = (x_9 / lx), Vec_$ctor_Z7AD9E565(a_18.X * f_18, a_18.Y * f_18, a_18.Z * f_18))), (a_19 = y, (f_19 = (-width / ly), Vec_$ctor_Z7AD9E565(a_19.X * f_19, a_19.Y * f_19, a_19.Z * f_19))));
            }
            case 1: {
                let y_8;
                const d_25 = (ly - offStart) - offEnd;
                y_8 = ((d_25 > 1E-06) ? d_25 : failRect3DOffsetEdge(offStart, offEnd, ly, edgeIdx, d_25));
                return Rect3D_$ctor_6181AF53((p_11 = ((p_10 = rect.Origin, (v_12 = ((a_20 = x, (f_20 = ((lx - offEdge) / lx), Vec_$ctor_Z7AD9E565(a_20.X * f_20, a_20.Y * f_20, a_20.Z * f_20)))), Pnt_$ctor_Z7AD9E565(p_10.X + v_12.X, p_10.Y + v_12.Y, p_10.Z + v_12.Z)))), (v_13 = ((a_21 = y, (f_21 = (offStart / ly), Vec_$ctor_Z7AD9E565(a_21.X * f_21, a_21.Y * f_21, a_21.Z * f_21)))), Pnt_$ctor_Z7AD9E565(p_11.X + v_13.X, p_11.Y + v_13.Y, p_11.Z + v_13.Z))), (a_22 = x, (f_22 = (-width / lx), Vec_$ctor_Z7AD9E565(a_22.X * f_22, a_22.Y * f_22, a_22.Z * f_22))), (a_23 = y, (f_23 = (y_8 / ly), Vec_$ctor_Z7AD9E565(a_23.X * f_23, a_23.Y * f_23, a_23.Z * f_23))));
            }
            case 2: {
                let x_12;
                const d_30 = (lx - offStart) - offEnd;
                x_12 = ((d_30 > 1E-06) ? d_30 : failRect3DOffsetEdge(offStart, offEnd, lx, edgeIdx, d_30));
                return Rect3D_$ctor_6181AF53((yy_6 = (ly - offEdge), (p_13 = ((p_12 = rect.Origin, (v_14 = ((a_24 = x, (f_24 = (offEnd / lx), Vec_$ctor_Z7AD9E565(a_24.X * f_24, a_24.Y * f_24, a_24.Z * f_24)))), Pnt_$ctor_Z7AD9E565(p_12.X + v_14.X, p_12.Y + v_14.Y, p_12.Z + v_14.Z)))), (v_15 = ((a_25 = y, (f_25 = (yy_6 / ly), Vec_$ctor_Z7AD9E565(a_25.X * f_25, a_25.Y * f_25, a_25.Z * f_25)))), Pnt_$ctor_Z7AD9E565(p_13.X + v_15.X, p_13.Y + v_15.Y, p_13.Z + v_15.Z)))), (a_26 = x, (f_26 = (x_12 / lx), Vec_$ctor_Z7AD9E565(a_26.X * f_26, a_26.Y * f_26, a_26.Z * f_26))), (a_27 = y, (f_27 = (-width / ly), Vec_$ctor_Z7AD9E565(a_27.X * f_27, a_27.Y * f_27, a_27.Z * f_27))));
            }
            case 3: {
                let y_11;
                const d_35 = (ly - offStart) - offEnd;
                y_11 = ((d_35 > 1E-06) ? d_35 : failRect3DOffsetEdge(offStart, offEnd, ly, edgeIdx, d_35));
                return Rect3D_$ctor_6181AF53((p_15 = ((p_14 = rect.Origin, (v_16 = ((a_28 = x, (f_28 = ((offEdge + width) / lx), Vec_$ctor_Z7AD9E565(a_28.X * f_28, a_28.Y * f_28, a_28.Z * f_28)))), Pnt_$ctor_Z7AD9E565(p_14.X + v_16.X, p_14.Y + v_16.Y, p_14.Z + v_16.Z)))), (v_17 = ((a_29 = y, (f_29 = (offEnd / ly), Vec_$ctor_Z7AD9E565(a_29.X * f_29, a_29.Y * f_29, a_29.Z * f_29)))), Pnt_$ctor_Z7AD9E565(p_15.X + v_17.X, p_15.Y + v_17.Y, p_15.Z + v_17.Z))), (a_30 = x, (f_30 = (-width / lx), Vec_$ctor_Z7AD9E565(a_30.X * f_30, a_30.Y * f_30, a_30.Z * f_30))), (a_31 = y, (f_31 = (y_11 / ly), Vec_$ctor_Z7AD9E565(a_31.X * f_31, a_31.Y * f_31, a_31.Z * f_31))));
            }
            default:
                return fail(`Rect3D.offsetEdge: edgeIdx ${edgeIdx} out of range 0..3`);
        }
    }
    else {
        return fail(`Rect3D.offsetEdge: width ${width} must be more than 1e-6`);
    }
}

/**
 * Divides a 3D-rectangle into a grid of sub-rectangles. The sub-rectangles are returned as an array of arrays.
 * The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D-rectangle.
 * The returned array has xCount elements, each element is an array of yCount sub-rectangles.
 */
export function Rect3D_subDivide_Z52A8DC92(rect, xCount, yCount, xGap, yGap) {
    if ((xCount <= 0) ? true : (yCount <= 0)) {
        fail(`Rect3D.subDivide: xCount ${xCount} and yCount ${yCount} must be 1 or more`);
    }
    const xa = rect.Xaxis;
    const ya = rect.Yaxis;
    let xl;
    const v = xa;
    xl = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let yl;
    const v_1 = ya;
    yl = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    const lx1 = (xl - (xGap * (xCount - 1))) / xCount;
    const ly1 = (yl - (yGap * (yCount - 1))) / yCount;
    if (!(lx1 > 1E-12) ? true : !(ly1 > 1E-12)) {
        return [];
    }
    else {
        let vx;
        const a = xa;
        const f = lx1 / xl;
        vx = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
        let vy;
        const a_1 = ya;
        const f_1 = ly1 / yl;
        vy = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
        const rss = fill(new Array(xCount), 0, xCount, null);
        for (let ix = 0; ix <= (xCount - 1); ix++) {
            const rs = fill(new Array(yCount), 0, yCount, new Rect3D(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 0)));
            for (let iy = 0; iy <= (yCount - 1); iy++) {
                let p_1, p, v_2, v_3;
                let x_2;
                const a_2 = xa;
                const f_2 = ((xGap * ix) / xl) + ((lx1 * ix) / xl);
                x_2 = Vec_$ctor_Z7AD9E565(a_2.X * f_2, a_2.Y * f_2, a_2.Z * f_2);
                let y;
                const a_3 = ya;
                const f_3 = ((yGap * iy) / yl) + ((ly1 * iy) / yl);
                y = Vec_$ctor_Z7AD9E565(a_3.X * f_3, a_3.Y * f_3, a_3.Z * f_3);
                setItem(rs, iy, Rect3D_$ctor_6181AF53((p_1 = ((p = rect.Origin, (v_2 = x_2, Pnt_$ctor_Z7AD9E565(p.X + v_2.X, p.Y + v_2.Y, p.Z + v_2.Z)))), (v_3 = y, Pnt_$ctor_Z7AD9E565(p_1.X + v_3.X, p_1.Y + v_3.Y, p_1.Z + v_3.Z))), vx, vy));
            }
            setItem(rss, ix, rs);
        }
        return rss;
    }
}

/**
 * Divides a a 3D-rectangle into a grid of sub-rectangles.
 * The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D-rectangle.
 * It will create as many sub-rectangles as possible, respecting the minimum side length for x and y.
 * The input minSegmentLength is multiplied by factor 0.99999 to avoid numerical errors.
 * That means in an edge case there are more segments returned, not fewer.
 * The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
 */
export function Rect3D_subDivideMinLength_Z52AC30F2(rect, xMinLen, yMinLen, xGap, yGap) {
    let xLen;
    const v = rect.Xaxis;
    xLen = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let yLen;
    const v_1 = rect.Yaxis;
    yLen = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (xLen < xMinLen) {
        fail(`Rect3D.subDivideMinLength: xMinLen ${xMinLen} is bigger than rect X-axis length ${xLen} for ${Rect3D__get_AsString(rect)}`);
    }
    if (yLen < yMinLen) {
        fail(`Rect3D.subDivideMinLength: yMinLen ${yMinLen} is bigger than rect Y-axis length ${yLen} for ${Rect3D__get_AsString(rect)}`);
    }
    return Rect3D_subDivide_Z52A8DC92(rect, ~~(xLen / (xMinLen * 0.9999)), ~~(yLen / (yMinLen * 0.9999)), xGap, yGap);
}

/**
 * Divides a a 3D-rectangle into a grid of sub-rectangles.
 * The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 3D-rectangle.
 * It will create as few segments as possible respecting the maximum segment length.
 * The input maxSegmentLength is multiplied by factor 1.00001 to avoid numerical errors.
 * That means in an edge case there are fewer segments returned, not more.
 * The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
 */
export function Rect3D_subDivideMaxLength_Z52AC30F2(rect, xMaxLen, yMaxLen, xGap, yGap) {
    let v, v_1;
    return Rect3D_subDivide_Z52A8DC92(rect, 1 + ~~(((v = rect.Xaxis, Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z)))) / (xMaxLen * 1.00001)), 1 + ~~(((v_1 = rect.Yaxis, Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z)))) / (yMaxLen * 1.00001)), xGap, yGap);
}

/**
 * Divides a 3D-rectangle into a grid of points. The points are returned as an array of arrays.
 * A xCount and yCount of 2 will only return the 4 corners of the rectangle.
 */
export function Rect3D_grid_638F50CE(rect, xCount, yCount) {
    if ((xCount <= 1) ? true : (yCount <= 1)) {
        fail(`Rect3D.grid: xCount ${xCount} and yCount ${yCount} must be 2 or more`);
    }
    const xa = rect.Xaxis;
    const ya = rect.Yaxis;
    let xl;
    const v = xa;
    xl = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let yl;
    const v_1 = ya;
    yl = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    const lx1 = xl / (xCount - 1);
    const ly1 = yl / (yCount - 1);
    const rss = fill(new Array(xCount), 0, xCount, null);
    for (let ix = 0; ix <= (xCount - 1); ix++) {
        const rs = fill(new Array(yCount), 0, yCount, Pnt_$ctor_Z7AD9E565(0, 0, 0));
        for (let iy = 0; iy <= (yCount - 1); iy++) {
            let p_1, p, v_2, v_3;
            let x;
            const a = xa;
            const f = (lx1 * ix) / xl;
            x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
            let y;
            const a_1 = ya;
            const f_1 = (ly1 * iy) / yl;
            y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
            setItem(rs, iy, (p_1 = ((p = rect.Origin, (v_2 = x, Pnt_$ctor_Z7AD9E565(p.X + v_2.X, p.Y + v_2.Y, p.Z + v_2.Z)))), (v_3 = y, Pnt_$ctor_Z7AD9E565(p_1.X + v_3.X, p_1.Y + v_3.Y, p_1.Z + v_3.Z))));
        }
        setItem(rss, ix, rs);
    }
    return rss;
}

/**
 * Divides a a 3D-rectangle into a grid of points.
 * It will create as many points as possible respecting the minimum side length for x and y.
 * The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
 * That means in an edge case there are more segments returned, not fewer.
 * The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
 */
export function Rect3D_gridMinLength_ZFDD8B12(rect, xMinLen, yMinLen) {
    let xLen;
    const v = rect.Xaxis;
    xLen = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let yLen;
    const v_1 = rect.Yaxis;
    yLen = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (xLen < xMinLen) {
        fail(`Rect3D.gridMinLength: xMinLen ${xMinLen} is bigger than rect X-axis length ${xLen} for ${Rect3D__get_AsString(rect)}`);
    }
    if (yLen < yMinLen) {
        fail(`Rect3D.gridMinLength: yMinLen ${yMinLen} is bigger than rect Y-axis length ${yLen} for ${Rect3D__get_AsString(rect)}`);
    }
    return Rect3D_grid_638F50CE(rect, 1 + ~~(xLen / (xMinLen * 0.9999)), 1 + ~~(yLen / (yMinLen * 0.9999)));
}

/**
 * Divides a a 3D-rectangle into a grid of points.
 * It will create as few as points as possible respecting the maximum segment length.
 * The input maxSegmentLength is multiplied by factor 0.0001 of to avoid numerical errors.
 * That means in an edge case there are fewer segments returned, not more.
 * The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
 */
export function Rect3D_gridMaxLength_ZFDD8B12(rect, xMaxLen, yMaxLen) {
    let v, v_1;
    return Rect3D_grid_638F50CE(rect, 2 + ~~(((v = rect.Xaxis, Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z)))) / (xMaxLen * 1.00001)), 2 + ~~(((v_1 = rect.Yaxis, Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z)))) / (yMaxLen * 1.00001)));
}

/**
 * Returns the line parameter and the X and Y parameters on the Rect3D as tuple (pLn, pPlX, pPlY).
 * The parameters is the intersection point of the ray with the infinitely extended Rect3D.
 * So if any of the parameters is outside of the range 0.0 to 1.0 the intersection point is actually outside of the rectangle.
 * Returns None if they are parallel or coincident.
 */
export function Rect3D_intersectRayParameters(ln, pl) {
    let ln_2, ln_3, ln_4, a_1, a_2, b_2, ln_5, b_1, a_5, b_4, v_2, a_6, b_5, v_3;
    const z = Rect3D__get_NormalUnit(pl);
    let v;
    const ln_1 = ln;
    v = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    let nenner;
    const a = v;
    const b = z;
    nenner = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    if (!(Math.abs(nenner) > 1E-06)) {
        return undefined;
    }
    else {
        const t = ((a_1 = ((a_2 = pl.Origin, (b_2 = ((ln_5 = ln, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ))), Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (b_1 = z, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))) / nenner;
        let vecInPlane;
        let a_4;
        let p;
        const ln_6 = ln;
        p = Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ);
        let v_1;
        const a_3 = v;
        const f = t;
        v_1 = Vec_$ctor_Z7AD9E565(a_3.X * f, a_3.Y * f, a_3.Z * f);
        a_4 = Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z);
        const b_3 = pl.Origin;
        vecInPlane = Vec_$ctor_Z7AD9E565(a_4.X - b_3.X, a_4.Y - b_3.Y, a_4.Z - b_3.Z);
        return [t, ((a_5 = pl.Xaxis, (b_4 = vecInPlane, ((a_5.X * b_4.X) + (a_5.Y * b_4.Y)) + (a_5.Z * b_4.Z)))) / ((v_2 = pl.Xaxis, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))), ((a_6 = pl.Yaxis, (b_5 = vecInPlane, ((a_6.X * b_5.X) + (a_6.Y * b_5.Y)) + (a_6.Z * b_5.Z)))) / ((v_3 = pl.Yaxis, ((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z)))];
    }
}

/**
 * Returns the line parameter.
 * The parameter is the intersection point of the ray with the infinitely extended Rect3D.
 * The line is outside of the rectangle if the range is 0.0 to 1.0 .
 * Returns None if they are parallel or coincident.
 */
export function Rect3D_intersectRayParameter(ln, pl) {
    let ln_2, ln_3, ln_4, a_1, a_2, b_2, ln_5, b_1;
    const z = Rect3D__get_NormalUnit(pl);
    let nenner;
    let a;
    const ln_1 = ln;
    a = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    const b = z;
    nenner = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    if (!(Math.abs(nenner) > 1E-06)) {
        return undefined;
    }
    else {
        return ((a_1 = ((a_2 = pl.Origin, (b_2 = ((ln_5 = ln, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ))), Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (b_1 = z, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))) / nenner;
    }
}

/**
 * Returns the line parameter and the X and Y parameters on the Rect3D as tuple (pLn, pPlX, pPlY).
 * These parameters ar all in the range 0.0 to 1.0.
 * Returns None if the intersection point is outside of their bounds.
 * Use Rect3D.intersectLineParameters to get the parameters of the intersection point.
 * Returns None if they are parallel or coincident.
 */
export function Rect3D_intersectLineParameters(ln, pl) {
    let ln_2, ln_3, ln_4, a_1, a_2, b_2, ln_5, b_1, a_5, b_4, v_2, a_6, b_5, v_3;
    const z = Rect3D__get_NormalUnit(pl);
    let v;
    const ln_1 = ln;
    v = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    let nenner;
    const a = v;
    const b = z;
    nenner = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    if (!(Math.abs(nenner) > 1E-06)) {
        return undefined;
    }
    else {
        const t = ((a_1 = ((a_2 = pl.Origin, (b_2 = ((ln_5 = ln, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ))), Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (b_1 = z, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))) / nenner;
        if ((t < 0) ? true : (t > 1)) {
            return undefined;
        }
        else {
            let vecInPlane;
            let a_4;
            let p;
            const ln_6 = ln;
            p = Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ);
            let v_1;
            const a_3 = v;
            const f = t;
            v_1 = Vec_$ctor_Z7AD9E565(a_3.X * f, a_3.Y * f, a_3.Z * f);
            a_4 = Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z);
            const b_3 = pl.Origin;
            vecInPlane = Vec_$ctor_Z7AD9E565(a_4.X - b_3.X, a_4.Y - b_3.Y, a_4.Z - b_3.Z);
            const tx = ((a_5 = pl.Xaxis, (b_4 = vecInPlane, ((a_5.X * b_4.X) + (a_5.Y * b_4.Y)) + (a_5.Z * b_4.Z)))) / ((v_2 = pl.Xaxis, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z)));
            if ((tx < 0) ? true : (tx > 1)) {
                return undefined;
            }
            else {
                const ty = ((a_6 = pl.Yaxis, (b_5 = vecInPlane, ((a_6.X * b_5.X) + (a_6.Y * b_5.Y)) + (a_6.Z * b_5.Z)))) / ((v_3 = pl.Yaxis, ((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z)));
                if ((ty < 0) ? true : (ty > 1)) {
                    return undefined;
                }
                else {
                    return [t, tx, ty];
                }
            }
        }
    }
}

/**
 * Returns intersection point of a Line3D with Rect3D.
 * Returns None if the intersection point is outside of their bounds.
 * Use Rect3D.intersectLineParameters to get the parameters of the intersection point.
 * Returns None if they are parallel or coincident.
 */
export function Rect3D_intersectLine(ln, pl) {
    let ln_2, ln_3, ln_4, a_1, a_2, b_2, ln_5, b_1, a_5, b_4, v_2, a_6, b_5, v_3;
    const z = Rect3D__get_NormalUnit(pl);
    let v;
    const ln_1 = ln;
    v = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    let nenner;
    const a = v;
    const b = z;
    nenner = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
    if (!(Math.abs(nenner) > 1E-06)) {
        return undefined;
    }
    else {
        const t = ((a_1 = ((a_2 = pl.Origin, (b_2 = ((ln_5 = ln, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ))), Vec_$ctor_Z7AD9E565(a_2.X - b_2.X, a_2.Y - b_2.Y, a_2.Z - b_2.Z)))), (b_1 = z, ((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z)))) / nenner;
        if ((t < 0) ? true : (t > 1)) {
            return undefined;
        }
        else {
            let xpt;
            let p;
            const ln_6 = ln;
            p = Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ);
            let v_1;
            const a_3 = v;
            const f = t;
            v_1 = Vec_$ctor_Z7AD9E565(a_3.X * f, a_3.Y * f, a_3.Z * f);
            xpt = Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z);
            let vecInPlane;
            const a_4 = xpt;
            const b_3 = pl.Origin;
            vecInPlane = Vec_$ctor_Z7AD9E565(a_4.X - b_3.X, a_4.Y - b_3.Y, a_4.Z - b_3.Z);
            const tx = ((a_5 = pl.Xaxis, (b_4 = vecInPlane, ((a_5.X * b_4.X) + (a_5.Y * b_4.Y)) + (a_5.Z * b_4.Z)))) / ((v_2 = pl.Xaxis, ((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z)));
            if ((tx < 0) ? true : (tx > 1)) {
                return undefined;
            }
            else {
                const ty = ((a_6 = pl.Yaxis, (b_5 = vecInPlane, ((a_6.X * b_5.X) + (a_6.Y * b_5.Y)) + (a_6.Z * b_5.Z)))) / ((v_3 = pl.Yaxis, ((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z)));
                if ((ty < 0) ? true : (ty > 1)) {
                    return undefined;
                }
                else {
                    return xpt;
                }
            }
        }
    }
}

/**
 * Returns the Rectangle flipped. Or rotated 180 around its diagonal from point 1 to 3.
 * The normal of the rectangle gets flipped.
 * Origin will be at point 2, X-axis points down to to point 1, Y-axis points left to point 3.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D__get_Flipped(r) {
    let p_1, p, v, v_1, v_2, v_3;
    return Rect3D_$ctor_6181AF53((p_1 = ((p = r.Origin, (v = r.Xaxis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z)))), (v_1 = r.Yaxis, Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z))), (v_2 = r.Yaxis, Vec_$ctor_Z7AD9E565(-v_2.X, -v_2.Y, -v_2.Z)), (v_3 = r.Xaxis, Vec_$ctor_Z7AD9E565(-v_3.X, -v_3.Y, -v_3.Z)));
}

/**
 * Returns the same rectangle with a new orientation rotated by 90 degrees clockwise around its center.
 * This only changes the internal representation of the rectangle, the appearance is not changed.
 * Origin will be at point 3, X-axis to to point 0, Y-axis to point 2.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D__get_RotateOrientation90CW(r) {
    let p, v, v_1;
    return Rect3D_$ctor_6181AF53((p = r.Origin, (v = r.Yaxis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z))), (v_1 = r.Yaxis, Vec_$ctor_Z7AD9E565(-v_1.X, -v_1.Y, -v_1.Z)), r.Xaxis);
}

/**
 * Returns the Rectangle rotated 180 degrees around its center.
 * Returns the same rectangle with a new orientation rotated by 180 degrees around its center.
 * This only changes the internal representation of the rectangle, the appearance is not changed.
 * Origin will be at point 2, X-axis to to point 3, Y-axis to point 1.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D__get_RotateOrientation180(r) {
    let p_1, p, v, v_1, v_2, v_3;
    return Rect3D_$ctor_6181AF53((p_1 = ((p = r.Origin, (v = r.Yaxis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z)))), (v_1 = r.Xaxis, Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z))), (v_2 = r.Xaxis, Vec_$ctor_Z7AD9E565(-v_2.X, -v_2.Y, -v_2.Z)), (v_3 = r.Yaxis, Vec_$ctor_Z7AD9E565(-v_3.X, -v_3.Y, -v_3.Z)));
}

/**
 * Returns the same rectangle with a new orientation rotated by 90 degrees counter clockwise around its center.
 * This only changes the internal representation of the rectangle, the appearance is not changed.
 * Origin will be at point 1, X-axis to to point 2, Y-axis to point 0.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D__get_RotateOrientation90CCW(r) {
    let p, v, v_1;
    return Rect3D_$ctor_6181AF53((p = r.Origin, (v = r.Xaxis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z))), r.Yaxis, (v_1 = r.Xaxis, Vec_$ctor_Z7AD9E565(-v_1.X, -v_1.Y, -v_1.Z)));
}

/**
 * Returns the 4 corners of the 3D-rectangle in Counter-Clockwise order, starting at Origin.
 * Returns an array of 4 Points: point 0 then 1, 2 and 3.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D__get_Points(r) {
    let p_1, v_1, p_2, v_2;
    const p0 = r.Origin;
    let p1;
    const p = p0;
    const v = r.Xaxis;
    p1 = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
    return [p0, p1, (p_1 = p1, (v_1 = r.Yaxis, Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z))), (p_2 = p0, (v_2 = r.Yaxis, Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z)))];
}

/**
 * Returns the 4 corners of the 3D-rectangle als closed loop in Counter-Clockwise order, starting at Origin.
 * First and last point are the same.
 * Returns an array of 5 Points: point 0 then 1, 2, 3 and again 0.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D__get_PointsLooped(r) {
    let p_1, v_1, p_2, v_2;
    const p0 = r.Origin;
    let p1;
    const p = p0;
    const v = r.Xaxis;
    p1 = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
    return [p0, p1, (p_1 = p1, (v_1 = r.Yaxis, Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z))), (p_2 = p0, (v_2 = r.Yaxis, Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z))), p0];
}

/**
 * Returns the 4 Edges of the 3D-rectangle in Counter-Clockwise order, starting at Origin.
 * Returns an array of 4 Lines: from point
 * 0 to 1,
 * 1 to 2,
 * 2 to 3,
 * 3 to 0.
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D__get_Edges(r) {
    const p0 = r.Origin;
    let p1;
    const p = p0;
    const v = r.Xaxis;
    p1 = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
    let p2;
    const p_1 = p1;
    const v_1 = r.Yaxis;
    p2 = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    let p3;
    const p_2 = p0;
    const v_2 = r.Yaxis;
    p3 = Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z);
    return [Line3D_$ctor_5A6659A0(p0, p1), Line3D_$ctor_5A6659A0(p1, p2), Line3D_$ctor_5A6659A0(p2, p3), Line3D_$ctor_5A6659A0(p3, p0)];
}

/**
 * Returns one of the 4 Edges as 3D Line:
 * Edge 0: from point  0 to 1
 * Edge 1: from point  1 to 2
 * Edge 2: from point  2 to 3
 * Edge 3: from point  3 to 0
 * <code>
 * local
 * Y-Axis
 * ^
 * |
 * |             2
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-Origin       1
 * </code>
 */
export function Rect3D__GetEdge_Z524259A4(r, i) {
    let p, v, p_1, v_1, p_3, p_2, v_2, v_3, p_5, p_4, v_4, v_5, p_6, v_6, p_7, v_7;
    switch (i) {
        case 0:
            return Line3D_$ctor_5A6659A0(r.Origin, (p = r.Origin, (v = r.Xaxis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z))));
        case 1:
            return Line3D_$ctor_5A6659A0((p_1 = r.Origin, (v_1 = r.Xaxis, Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z))), (p_3 = ((p_2 = r.Origin, (v_2 = r.Xaxis, Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z)))), (v_3 = r.Yaxis, Pnt_$ctor_Z7AD9E565(p_3.X + v_3.X, p_3.Y + v_3.Y, p_3.Z + v_3.Z))));
        case 2:
            return Line3D_$ctor_5A6659A0((p_5 = ((p_4 = r.Origin, (v_4 = r.Xaxis, Pnt_$ctor_Z7AD9E565(p_4.X + v_4.X, p_4.Y + v_4.Y, p_4.Z + v_4.Z)))), (v_5 = r.Yaxis, Pnt_$ctor_Z7AD9E565(p_5.X + v_5.X, p_5.Y + v_5.Y, p_5.Z + v_5.Z))), (p_6 = r.Origin, (v_6 = r.Yaxis, Pnt_$ctor_Z7AD9E565(p_6.X + v_6.X, p_6.Y + v_6.Y, p_6.Z + v_6.Z))));
        case 3:
            return Line3D_$ctor_5A6659A0((p_7 = r.Origin, (v_7 = r.Yaxis, Pnt_$ctor_Z7AD9E565(p_7.X + v_7.X, p_7.Y + v_7.Y, p_7.Z + v_7.Z))), r.Origin);
        default:
            return fail(`Rect3D.GetEdge: index ${i} out of range 0..3`);
    }
}

export function Rect3D_intersectLineParametersInfinite(ln, pl) {
    return Rect3D_intersectRayParameters(ln, pl);
}

export function Rect3D_intersectLineParameterInfinite(ln, pl) {
    return Rect3D_intersectRayParameter(ln, pl);
}

