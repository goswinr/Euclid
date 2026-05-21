
import { float } from "./Format.js";
import { Pnt_$ctor_Z7AD9E565, Pnt__get_AsFSharpCode, Pnt__get_AsString } from "./Pnt.js";
import { Vec_$ctor_Z7AD9E565, Vec__get_AsFSharpCode, Vec__get_AsString } from "./Vec.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { max, min } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { BBox_$ctor_76A78260 } from "./BBox.js";
import { failTooSmall, fail } from "./EuclidErrors.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Pnt.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Vec.js";
import { count } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53 } from "./TypeExtensions/PPlane.js";
import { Rect3D_$ctor_6181AF53 } from "./Rect3D.js";
import { Line3D_$ctor_5A6659A0 } from "./Line3D.js";

/**
 * A struct of one Pnt and three Vec, representing an immutable 3D Box with any rotation in 3D space.
 * Described by an Origin and three Edge vectors.
 * Similar to PPlane, however the three vectors are not unitized.
 * This implementation guarantees the box to be always valid.
 * That means the Min X, Y and Z axes cannot be flipped individually.
 * However the length of one of these axes might still be zero.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/     local
 * +---------------+----> X-Axis
 * 0               1
 * </code>
 */
export class Box extends Record {
    constructor(Origin, Xaxis, Yaxis, Zaxis) {
        super();
        this.Origin = Origin;
        this.Xaxis = Xaxis;
        this.Yaxis = Yaxis;
        this.Zaxis = Zaxis;
    }
    /**
     * Nicely formatted string representation of the Box including its size.
     */
    toString() {
        let v, v_1, v_2;
        const b = this;
        const sizeX = float((v = b.Xaxis, Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))));
        const sizeY = float((v_1 = b.Yaxis, Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))));
        const sizeZ = float((v_2 = b.Zaxis, Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))));
        const origin = Pnt__get_AsString(b.Origin);
        const xAxis = Vec__get_AsString(b.Xaxis);
        const yAxis = Vec__get_AsString(b.Yaxis);
        const zAxis = Vec__get_AsString(b.Zaxis);
        return `Euclid.Box ${sizeX} x ${sizeY} x ${sizeZ} (Origin:${origin}| X-ax:${xAxis}|Y-ax:${yAxis}|Z-ax:${zAxis})`;
    }
}

export function Box_$reflection() {
    return class_type("Euclid.Box", undefined, Box, class_type("System.ValueType"));
}

/**
 * Unsafe internal constructor, public only for inlining.
 */
export function Box_$ctor_5706A3BA(origin, axisX, axisY, axisZ) {
    return new Box(origin, axisX, axisY, axisZ);
}

/**
 * Format Box into string with nice floating point number formatting of X, Y and Z size only.
 * But without type name as in v.ToString()
 */
export function Box__get_AsString(b) {
    let v, v_1, v_2;
    const sizeX = float((v = b.Xaxis, Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))));
    const sizeY = float((v_1 = b.Yaxis, Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))));
    const sizeZ = float((v_2 = b.Zaxis, Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))));
    return `${sizeX} x ${sizeY} x ${sizeZ}`;
}

/**
 * Format Box into an F# code string that can be used to recreate the box.
 */
export function Box__get_AsFSharpCode(b) {
    return `Box.createUnchecked(${Pnt__get_AsFSharpCode(b.Origin)}, ${Vec__get_AsFSharpCode(b.Xaxis)}, ${Vec__get_AsFSharpCode(b.Yaxis)}, ${Vec__get_AsFSharpCode(b.Zaxis)})`;
}

/**
 * Check for point containment in the Box.
 * By doing 6 dot products with the sides of the rectangle.
 * A point exactly on the edge of the Box is considered inside.
 */
export function Box__Contains_Z394ECE4D(b, p) {
    let a_1, b_2, a_2, b_3, a_3, b_4, a_4, a_5, b_6, b_5, a_6, a_7, b_8, b_7, a_8, a_9, b_10, b_9;
    const x = b.Xaxis;
    const y = b.Yaxis;
    const z = b.Zaxis;
    const p0 = b.Origin;
    let v;
    const a = p;
    const b_1 = p0;
    v = Vec_$ctor_Z7AD9E565(a.X - b_1.X, a.Y - b_1.Y, a.Z - b_1.Z);
    let p1;
    const p_1 = p0;
    const v_1 = x;
    p1 = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    let p3;
    const p_2 = p0;
    const v_2 = y;
    p3 = Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z);
    let p4;
    const p_3 = p0;
    const v_3 = z;
    p4 = Pnt_$ctor_Z7AD9E565(p_3.X + v_3.X, p_3.Y + v_3.Y, p_3.Z + v_3.Z);
    if (((((((a_1 = v, (b_2 = x, ((a_1.X * b_2.X) + (a_1.Y * b_2.Y)) + (a_1.Z * b_2.Z)))) >= 0) && (((a_2 = v, (b_3 = y, ((a_2.X * b_3.X) + (a_2.Y * b_3.Y)) + (a_2.Z * b_3.Z)))) >= 0)) && (((a_3 = v, (b_4 = z, ((a_3.X * b_4.X) + (a_3.Y * b_4.Y)) + (a_3.Z * b_4.Z)))) >= 0)) && (((a_4 = ((a_5 = p, (b_6 = p3, Vec_$ctor_Z7AD9E565(a_5.X - b_6.X, a_5.Y - b_6.Y, a_5.Z - b_6.Z)))), (b_5 = y, ((a_4.X * b_5.X) + (a_4.Y * b_5.Y)) + (a_4.Z * b_5.Z)))) <= 0)) && (((a_6 = ((a_7 = p, (b_8 = p1, Vec_$ctor_Z7AD9E565(a_7.X - b_8.X, a_7.Y - b_8.Y, a_7.Z - b_8.Z)))), (b_7 = x, ((a_6.X * b_7.X) + (a_6.Y * b_7.Y)) + (a_6.Z * b_7.Z)))) <= 0)) {
        return ((a_8 = ((a_9 = p, (b_10 = p4, Vec_$ctor_Z7AD9E565(a_9.X - b_10.X, a_9.Y - b_10.Y, a_9.Z - b_10.Z)))), (b_9 = z, ((a_8.X * b_9.X) + (a_8.Y * b_9.Y)) + (a_8.Z * b_9.Z)))) <= 0;
    }
    else {
        return false;
    }
}

/**
 * Gets the axis aligned 3D Bounding Box of the Box.
 */
export function Box__get_BBox(b) {
    const p0 = b.Origin;
    let p1;
    const p = p0;
    const v = b.Xaxis;
    p1 = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
    const y = b.Yaxis;
    let p2;
    const p_1 = p1;
    const v_1 = y;
    p2 = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    let p3;
    const p_2 = p0;
    const v_2 = y;
    p3 = Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z);
    const z = b.Zaxis;
    let p4;
    const p_3 = p0;
    const v_3 = z;
    p4 = Pnt_$ctor_Z7AD9E565(p_3.X + v_3.X, p_3.Y + v_3.Y, p_3.Z + v_3.Z);
    let p5;
    const p_4 = p1;
    const v_4 = z;
    p5 = Pnt_$ctor_Z7AD9E565(p_4.X + v_4.X, p_4.Y + v_4.Y, p_4.Z + v_4.Z);
    let p6;
    const p_5 = p2;
    const v_5 = z;
    p6 = Pnt_$ctor_Z7AD9E565(p_5.X + v_5.X, p_5.Y + v_5.Y, p_5.Z + v_5.Z);
    let p7;
    const p_6 = p3;
    const v_6 = z;
    p7 = Pnt_$ctor_Z7AD9E565(p_6.X + v_6.X, p_6.Y + v_6.Y, p_6.Z + v_6.Z);
    const minX = min(min(min(min(min(min(min(p0.X, p1.X), p2.X), p3.X), p4.X), p5.X), p6.X), p7.X);
    const minY = min(min(min(min(min(min(min(p0.Y, p1.Y), p2.Y), p3.Y), p4.Y), p5.Y), p6.Y), p7.Y);
    const minZ = min(min(min(min(min(min(min(p0.Z, p1.Z), p2.Z), p3.Z), p4.Z), p5.Z), p6.Z), p7.Z);
    const maxX = max(max(max(max(max(max(max(p0.X, p1.X), p2.X), p3.X), p4.X), p5.X), p6.X), p7.X);
    const maxY = max(max(max(max(max(max(max(p0.Y, p1.Y), p2.Y), p3.Y), p4.Y), p5.Y), p6.Y), p7.Y);
    const maxZ = max(max(max(max(max(max(max(p0.Z, p1.Z), p2.Z), p3.Z), p4.Z), p5.Z), p6.Z), p7.Z);
    return BBox_$ctor_76A78260(minX, minY, minZ, maxX, maxY, maxZ);
}

/**
 * Returns the Area of the biggest face of the box.
 * This is the biggest of the three faces X*Y, X*Z and Y*Z.
 */
export function Box__get_AreaOfBiggestFace(b) {
    let x;
    const v = b.Xaxis;
    x = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let y;
    const v_1 = b.Yaxis;
    y = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    let z;
    const v_2 = b.Zaxis;
    z = Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z));
    return max(x * y, max(x * z, y * z));
}

/**
 * Returns the Area of the smallest face of the box.
 * This is the smallest of the three faces X*Y, X*Z and Y*Z.
 */
export function Box__get_AreaOfSmallestFace(b) {
    let x;
    const v = b.Xaxis;
    x = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let y;
    const v_1 = b.Yaxis;
    y = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    let z;
    const v_2 = b.Zaxis;
    z = Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z));
    return min(x * y, min(x * z, y * z));
}

/**
 * Checks if two 3D-boxes are equal within tolerance.
 * Does not recognize congruent boxes with different rotation as equal.
 * Use a tolerance of 0.0 to check for an exact match.
 */
export function Box_equals(tol, a, b) {
    if (((((((((((Math.abs(a.Origin.X - b.Origin.X) <= tol) && (Math.abs(a.Origin.Y - b.Origin.Y) <= tol)) && (Math.abs(a.Origin.Z - b.Origin.Z) <= tol)) && (Math.abs(a.Xaxis.X - b.Xaxis.X) <= tol)) && (Math.abs(a.Xaxis.Y - b.Xaxis.Y) <= tol)) && (Math.abs(a.Xaxis.Z - b.Xaxis.Z) <= tol)) && (Math.abs(a.Yaxis.X - b.Yaxis.X) <= tol)) && (Math.abs(a.Yaxis.Y - b.Yaxis.Y) <= tol)) && (Math.abs(a.Yaxis.Z - b.Yaxis.Z) <= tol)) && (Math.abs(a.Zaxis.X - b.Zaxis.X) <= tol)) && (Math.abs(a.Zaxis.Y - b.Zaxis.Y) <= tol)) {
        return Math.abs(a.Zaxis.Z - b.Zaxis.Z) <= tol;
    }
    else {
        return false;
    }
}

/**
 * Check if two 3D-boxes are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two 3D-boxes are not exactly equal.
 */
export function Box_notEquals(tol, a, b) {
    if (((((((((((Math.abs(a.Origin.X - b.Origin.X) > tol) ? true : (Math.abs(a.Origin.Y - b.Origin.Y) > tol)) ? true : (Math.abs(a.Origin.Z - b.Origin.Z) > tol)) ? true : (Math.abs(a.Xaxis.X - b.Xaxis.X) > tol)) ? true : (Math.abs(a.Xaxis.Y - b.Xaxis.Y) > tol)) ? true : (Math.abs(a.Xaxis.Z - b.Xaxis.Z) > tol)) ? true : (Math.abs(a.Yaxis.X - b.Yaxis.X) > tol)) ? true : (Math.abs(a.Yaxis.Y - b.Yaxis.Y) > tol)) ? true : (Math.abs(a.Yaxis.Z - b.Yaxis.Z) > tol)) ? true : (Math.abs(a.Zaxis.X - b.Zaxis.X) > tol)) ? true : (Math.abs(a.Zaxis.Y - b.Zaxis.Y) > tol)) {
        return true;
    }
    else {
        return Math.abs(a.Zaxis.Z - b.Zaxis.Z) > tol;
    }
}

/**
 * Returns Box expanded by distance on all six sides.
 * Does check for underflow if distance is negative and raises EuclidException.
 */
export function Box_expand(dist, b) {
    let p_2, p_1, p, v_3, v_4, v_5, a_4, b_4, a_3, a_6, b_5, a_5, a_8, b_6, a_7;
    let siX;
    const v = b.Xaxis;
    siX = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let siY;
    const v_1 = b.Yaxis;
    siY = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    let hei;
    const v_2 = b.Zaxis;
    hei = Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z));
    const d = dist * -2;
    if (((siX <= d) ? true : (siY <= d)) ? true : (hei <= d)) {
        fail(`Box.expand: Box ${Box__get_AsString(b)} is too small to expand by negative (=shrink) distance ${dist}`);
    }
    let x;
    const a = b.Xaxis;
    const f = dist / siX;
    x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = b.Yaxis;
    const f_1 = dist / siY;
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    let z;
    const a_2 = b.Zaxis;
    const f_2 = dist / hei;
    z = Vec_$ctor_Z7AD9E565(a_2.X * f_2, a_2.Y * f_2, a_2.Z * f_2);
    return Box_$ctor_5706A3BA((p_2 = ((p_1 = ((p = b.Origin, (v_3 = x, Pnt_$ctor_Z7AD9E565(p.X - v_3.X, p.Y - v_3.Y, p.Z - v_3.Z)))), (v_4 = y, Pnt_$ctor_Z7AD9E565(p_1.X - v_4.X, p_1.Y - v_4.Y, p_1.Z - v_4.Z)))), (v_5 = z, Pnt_$ctor_Z7AD9E565(p_2.X - v_5.X, p_2.Y - v_5.Y, p_2.Z - v_5.Z))), (a_4 = b.Xaxis, (b_4 = ((a_3 = x, Vec_$ctor_Z7AD9E565(a_3.X * 2, a_3.Y * 2, a_3.Z * 2))), Vec_$ctor_Z7AD9E565(a_4.X + b_4.X, a_4.Y + b_4.Y, a_4.Z + b_4.Z))), (a_6 = b.Yaxis, (b_5 = ((a_5 = y, Vec_$ctor_Z7AD9E565(a_5.X * 2, a_5.Y * 2, a_5.Z * 2))), Vec_$ctor_Z7AD9E565(a_6.X + b_5.X, a_6.Y + b_5.Y, a_6.Z + b_5.Z))), (a_8 = b.Zaxis, (b_6 = ((a_7 = z, Vec_$ctor_Z7AD9E565(a_7.X * 2, a_7.Y * 2, a_7.Z * 2))), Vec_$ctor_Z7AD9E565(a_8.X + b_6.X, a_8.Y + b_6.Y, a_8.Z + b_6.Z))));
}

/**
 * Returns Box expanded by respective distances on all six sides.
 * Does check for overflow if distance is negative and fails.
 * distX, distY and distZ are for X, Y and Z-axis respectively.
 */
export function Box_expandXYZ(distX, distY, distZ, b) {
    let v_3, v_4, v_5, p_2, p_1, p, v_6, v_7, v_8, a_4, b_7, a_3, a_6, b_8, a_5, a_8, b_9, a_7;
    let siX;
    const v = b.Xaxis;
    siX = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    let siY;
    const v_1 = b.Yaxis;
    siY = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    let hei;
    const v_2 = b.Zaxis;
    hei = Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z));
    if (siX <= (distX * -2)) {
        fail(`Box.expandXYZ: Box ${Box__get_AsString(b)} is too small to expand by negative (=shrink) distance distX ${distX}`);
    }
    if (siY <= (distY * -2)) {
        fail(`Box.expandXYZ: Box ${Box__get_AsString(b)} is too small to expand by negative (=shrink) distance distY ${distY}`);
    }
    if (hei <= (distZ * -2)) {
        fail(`Box.expandXYZ: Box ${Box__get_AsString(b)} is too small to expand by negative (=shrink) distance distZ ${distZ}`);
    }
    let x;
    const a = b.Xaxis;
    const f = distX / ((v_3 = b.Xaxis, Math.sqrt(((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z))));
    x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = b.Yaxis;
    const f_1 = distY / ((v_4 = b.Yaxis, Math.sqrt(((v_4.X * v_4.X) + (v_4.Y * v_4.Y)) + (v_4.Z * v_4.Z))));
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    let z;
    const a_2 = b.Zaxis;
    const f_2 = distZ / ((v_5 = b.Zaxis, Math.sqrt(((v_5.X * v_5.X) + (v_5.Y * v_5.Y)) + (v_5.Z * v_5.Z))));
    z = Vec_$ctor_Z7AD9E565(a_2.X * f_2, a_2.Y * f_2, a_2.Z * f_2);
    return Box_$ctor_5706A3BA((p_2 = ((p_1 = ((p = b.Origin, (v_6 = x, Pnt_$ctor_Z7AD9E565(p.X - v_6.X, p.Y - v_6.Y, p.Z - v_6.Z)))), (v_7 = y, Pnt_$ctor_Z7AD9E565(p_1.X - v_7.X, p_1.Y - v_7.Y, p_1.Z - v_7.Z)))), (v_8 = z, Pnt_$ctor_Z7AD9E565(p_2.X - v_8.X, p_2.Y - v_8.Y, p_2.Z - v_8.Z))), (a_4 = b.Xaxis, (b_7 = ((a_3 = x, Vec_$ctor_Z7AD9E565(a_3.X * 2, a_3.Y * 2, a_3.Z * 2))), Vec_$ctor_Z7AD9E565(a_4.X + b_7.X, a_4.Y + b_7.Y, a_4.Z + b_7.Z))), (a_6 = b.Yaxis, (b_8 = ((a_5 = y, Vec_$ctor_Z7AD9E565(a_5.X * 2, a_5.Y * 2, a_5.Z * 2))), Vec_$ctor_Z7AD9E565(a_6.X + b_8.X, a_6.Y + b_8.Y, a_6.Z + b_8.Z))), (a_8 = b.Zaxis, (b_9 = ((a_7 = z, Vec_$ctor_Z7AD9E565(a_7.X * 2, a_7.Y * 2, a_7.Z * 2))), Vec_$ctor_Z7AD9E565(a_8.X + b_9.X, a_8.Y + b_9.Y, a_8.Z + b_9.Z))));
}

/**
 * Returns the 3D box expanded by a relative factor on all six sides.
 * Values between 0.0 and 1.0 shrink the box.
 * Values larger than 1.0 expand the box.
 * Does check for underflow if factor is negative and raises EuclidException.
 */
export function Box_expandRel(factor, b) {
    let p_5, p_4, p_3, b_1, p_2, p_1, p, v, a_3, v_1, a_4, v_2, a_5, v_3, a_6, v_4, a_7, v_5, a_8;
    if (factor < 0) {
        fail(`Box.expandRel: a negative factor ${factor} is not allowed for expanding the 3D box ${Box__get_AsString(b)}`);
    }
    let x;
    const a = b.Xaxis;
    const f = factor;
    x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = b.Yaxis;
    const f_1 = factor;
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    let z;
    const a_2 = b.Zaxis;
    const f_2 = factor;
    z = Vec_$ctor_Z7AD9E565(a_2.X * f_2, a_2.Y * f_2, a_2.Z * f_2);
    return Box_$ctor_5706A3BA((p_5 = ((p_4 = ((p_3 = ((b_1 = b, (p_2 = ((p_1 = ((p = b_1.Origin, (v = ((a_3 = b_1.Xaxis, Vec_$ctor_Z7AD9E565(a_3.X * 0.5, a_3.Y * 0.5, a_3.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z)))), (v_1 = ((a_4 = b_1.Yaxis, Vec_$ctor_Z7AD9E565(a_4.X * 0.5, a_4.Y * 0.5, a_4.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z)))), (v_2 = ((a_5 = b_1.Zaxis, Vec_$ctor_Z7AD9E565(a_5.X * 0.5, a_5.Y * 0.5, a_5.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z))))), (v_3 = ((a_6 = x, Vec_$ctor_Z7AD9E565(a_6.X * 0.5, a_6.Y * 0.5, a_6.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_3.X - v_3.X, p_3.Y - v_3.Y, p_3.Z - v_3.Z)))), (v_4 = ((a_7 = y, Vec_$ctor_Z7AD9E565(a_7.X * 0.5, a_7.Y * 0.5, a_7.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_4.X - v_4.X, p_4.Y - v_4.Y, p_4.Z - v_4.Z)))), (v_5 = ((a_8 = z, Vec_$ctor_Z7AD9E565(a_8.X * 0.5, a_8.Y * 0.5, a_8.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_5.X - v_5.X, p_5.Y - v_5.Y, p_5.Z - v_5.Z))), x, y, z);
}

/**
 * Returns the 3D box expanded by a relative factor on all six sides, separately for X, Y, Z.
 * Values between 0.0 and 1.0 shrink the box.
 * Values larger than 1.0 expand the box.
 * Does check for underflow if any factor is negative and raises EuclidException.
 */
export function Box_expandRelXYZ(factorX, factorY, factorZ, b) {
    let p_5, p_4, p_3, b_1, p_2, p_1, p, v, a_3, v_1, a_4, v_2, a_5, v_3, a_6, v_4, a_7, v_5, a_8;
    if (factorX < 0) {
        fail(`Box.expandRelXYZ: a negative factorX ${factorX} is not allowed for expanding the 3D box ${Box__get_AsString(b)}`);
    }
    if (factorY < 0) {
        fail(`Box.expandRelXYZ: a negative factorY ${factorY} is not allowed for expanding the 3D box ${Box__get_AsString(b)}`);
    }
    if (factorZ < 0) {
        fail(`Box.expandRelXYZ: a negative factorZ ${factorZ} is not allowed for expanding the 3D box ${Box__get_AsString(b)}`);
    }
    let x;
    const a = b.Xaxis;
    const f = factorX;
    x = Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f);
    let y;
    const a_1 = b.Yaxis;
    const f_1 = factorY;
    y = Vec_$ctor_Z7AD9E565(a_1.X * f_1, a_1.Y * f_1, a_1.Z * f_1);
    let z;
    const a_2 = b.Zaxis;
    const f_2 = factorZ;
    z = Vec_$ctor_Z7AD9E565(a_2.X * f_2, a_2.Y * f_2, a_2.Z * f_2);
    return Box_$ctor_5706A3BA((p_5 = ((p_4 = ((p_3 = ((b_1 = b, (p_2 = ((p_1 = ((p = b_1.Origin, (v = ((a_3 = b_1.Xaxis, Vec_$ctor_Z7AD9E565(a_3.X * 0.5, a_3.Y * 0.5, a_3.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z)))), (v_1 = ((a_4 = b_1.Yaxis, Vec_$ctor_Z7AD9E565(a_4.X * 0.5, a_4.Y * 0.5, a_4.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z)))), (v_2 = ((a_5 = b_1.Zaxis, Vec_$ctor_Z7AD9E565(a_5.X * 0.5, a_5.Y * 0.5, a_5.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z))))), (v_3 = ((a_6 = x, Vec_$ctor_Z7AD9E565(a_6.X * 0.5, a_6.Y * 0.5, a_6.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_3.X - v_3.X, p_3.Y - v_3.Y, p_3.Z - v_3.Z)))), (v_4 = ((a_7 = y, Vec_$ctor_Z7AD9E565(a_7.X * 0.5, a_7.Y * 0.5, a_7.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_4.X - v_4.X, p_4.Y - v_4.Y, p_4.Z - v_4.Z)))), (v_5 = ((a_8 = z, Vec_$ctor_Z7AD9E565(a_8.X * 0.5, a_8.Y * 0.5, a_8.Z * 0.5))), Pnt_$ctor_Z7AD9E565(p_5.X - v_5.X, p_5.Y - v_5.Y, p_5.Z - v_5.Z))), x, y, z);
}

/**
 * Creates a 3D box from a 2D rectangle and Z lower and upper position.
 */
export function Box_createFromRect2D(zLow, zHigh, r) {
    let pt, v, v_1, a, f;
    return Box_$ctor_5706A3BA((pt = r.Origin, Pnt_$ctor_Z7AD9E565_1(pt.X, pt.Y, zLow)), (v = r.Xaxis, Vec_$ctor_Z7AD9E565_1(v.X, v.Y, 0)), (v_1 = r.Yaxis, Vec_$ctor_Z7AD9E565_1(v_1.X, v_1.Y, 0)), (a = Vec_$ctor_Z7AD9E565_1(0, 0, 1), (f = (zHigh - zLow), Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f))));
}

/**
 * Finds the oriented bounding box in 3D of a set of points.
 * The orientation of the X-axis is defined by the dirX vector.
 * The orientation of the Y-axis is defined by the dirY vector.
 */
export function Box_createFromPlaneAndPoints(pl, pts) {
    let a_7, f_3, a_8, f_4, a_9, f_5;
    if (count(pts) < 2) {
        fail(`Box.createFromPlaneAndPoints: cannot create a Box from just ${count(pts)} points`);
    }
    const o = pl.Origin;
    const x = pl.Xaxis;
    const y = pl.Yaxis;
    const z = pl.Zaxis;
    let minX = 1.7976931348623157E+308;
    let minY = 1.7976931348623157E+308;
    let maxX = -1.7976931348623157E+308;
    let maxY = -1.7976931348623157E+308;
    let minZ = 1.7976931348623157E+308;
    let maxZ = -1.7976931348623157E+308;
    for (let i = 1; i <= (count(pts) - 1); i++) {
        let v;
        const a = item(i, pts);
        const b = o;
        v = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
        let dotX;
        const a_1 = v;
        const b_1 = x;
        dotX = (((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z));
        minX = min(minX, dotX);
        maxX = max(maxX, dotX);
        let dotY;
        const a_2 = v;
        const b_2 = y;
        dotY = (((a_2.X * b_2.X) + (a_2.Y * b_2.Y)) + (a_2.Z * b_2.Z));
        minY = min(minY, dotY);
        maxY = max(maxY, dotY);
        let dotZ;
        const a_3 = v;
        const b_3 = z;
        dotZ = (((a_3.X * b_3.X) + (a_3.Y * b_3.Y)) + (a_3.Z * b_3.Z));
        minZ = min(minZ, dotZ);
        maxZ = max(maxZ, dotZ);
    }
    let bo;
    const p = pl;
    const px = minX;
    const py = minY;
    const pz = minZ;
    let p_3;
    let p_2;
    const p_1 = p.Origin;
    let v_1;
    const a_4 = p.Xaxis;
    const f = px;
    v_1 = Vec_$ctor_Z7AD9E565(a_4.X * f, a_4.Y * f, a_4.Z * f);
    p_2 = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    let v_2;
    const a_5 = p.Yaxis;
    const f_1 = py;
    v_2 = Vec_$ctor_Z7AD9E565(a_5.X * f_1, a_5.Y * f_1, a_5.Z * f_1);
    p_3 = Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z);
    let v_3;
    const a_6 = p.Zaxis;
    const f_2 = pz;
    v_3 = Vec_$ctor_Z7AD9E565(a_6.X * f_2, a_6.Y * f_2, a_6.Z * f_2);
    bo = Pnt_$ctor_Z7AD9E565(p_3.X + v_3.X, p_3.Y + v_3.Y, p_3.Z + v_3.Z);
    const sizeX = maxX - minX;
    const sizeY = maxY - minY;
    const sizeZ = maxZ - minZ;
    return Box_$ctor_5706A3BA(bo, (a_7 = x, (f_3 = sizeX, Vec_$ctor_Z7AD9E565(a_7.X * f_3, a_7.Y * f_3, a_7.Z * f_3))), (a_8 = y, (f_4 = sizeY, Vec_$ctor_Z7AD9E565(a_8.X * f_4, a_8.Y * f_4, a_8.Z * f_4))), (a_9 = z, (f_5 = sizeZ, Vec_$ctor_Z7AD9E565(a_9.X * f_5, a_9.Y * f_5, a_9.Z * f_5))));
}

/**
 * Finds the oriented bounding box in 3D of a set of points.
 * The orientation of the X-axis is defined by the dirX vector.
 * The orientation of the Y-axis is defined by the dirY vector.
 */
export function Box_createFromDirsAndPoints(dirX, dirY, pts) {
    let v, v_1;
    if (!(((v = dirX, ((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z))) > 1E-12)) {
        failTooSmall("Euclid.Box.createFromDirsAndPoints: dirX too short", dirX);
    }
    if (!(((v_1 = dirY, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) > 1E-12)) {
        failTooSmall("Euclid.Box.createFromDirsAndPoints: dirY too short", dirY);
    }
    if (count(pts) < 2) {
        fail(`Box.createFromDirsAndPoints: cannot create a Box from just ${count(pts)} points`);
    }
    const pl = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(item(0, pts), dirX, dirY);
    return Box_createFromPlaneAndPoints(pl, pts);
}

/**
 * Creates a 3D box translated along the local X-axis of the Box.
 */
export function Box_translateLocalX(distX, b) {
    let p, v_1, a, f;
    const x = b.Xaxis;
    let len;
    const v = x;
    len = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(len > 1E-12)) {
        failTooSmall("Box.translateLocalX Xaxis ", b);
    }
    return Box_$ctor_5706A3BA((p = b.Origin, (v_1 = ((a = x, (f = (distX / len), Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z))), x, b.Yaxis, b.Zaxis);
}

/**
 * Creates a 3D box translated along the local Y-axis of the Box.
 */
export function Box_translateLocalY(distY, b) {
    let p, v_1, a, f;
    const y = b.Yaxis;
    let len;
    const v = y;
    len = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(len > 1E-12)) {
        failTooSmall("Box.translateLocalY Yaxis", b);
    }
    return Box_$ctor_5706A3BA((p = b.Origin, (v_1 = ((a = y, (f = (distY / len), Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z))), b.Xaxis, y, b.Zaxis);
}

/**
 * Creates a 3D box translated along the local Z-axis of the Box.
 */
export function Box_translateLocalZ(distZ, b) {
    let p, v_1, a, f;
    const z = b.Zaxis;
    let len;
    const v = z;
    len = Math.sqrt(((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(len > 1E-12)) {
        failTooSmall("Box.translateLocalZ Zaxis", b);
    }
    return Box_$ctor_5706A3BA((p = b.Origin, (v_1 = ((a = z, (f = (distZ / len), Vec_$ctor_Z7AD9E565(a.X * f, a.Y * f, a.Z * f)))), Pnt_$ctor_Z7AD9E565(p.X + v_1.X, p.Y + v_1.Y, p.Z + v_1.Z))), b.Xaxis, b.Yaxis, z);
}

/**
 * Intersects an infinite ray (Line3D extended infinitely in both directions) with this Box.
 * Uses the slab intersection method in the box's local coordinate system.
 * Returns None if the ray does not intersect the box or if the ray direction is too short.
 * Returns Some with entry and exit parameters on the ray if it intersects.
 * A parameter of 0.0 corresponds to the ray's From point, 1.0 to its To point.
 */
export function Box__IntersectRay_4CC2E360(b, ray) {
    let ln_1, ln_2, ln_3;
    let rayDir;
    const ln = ray;
    rayDir = Vec_$ctor_Z7AD9E565((ln_1 = ln, ln_1.ToX - ln_1.FromX), (ln_2 = ln, ln_2.ToY - ln_2.FromY), (ln_3 = ln, ln_3.ToZ - ln_3.FromZ));
    let rayDirLenSq;
    const v = rayDir;
    rayDirLenSq = (((v.X * v.X) + (v.Y * v.Y)) + (v.Z * v.Z));
    if (!(rayDirLenSq > 1E-12)) {
        return undefined;
    }
    else {
        let rayOrigin;
        let a;
        const ln_4 = ray;
        a = Pnt_$ctor_Z7AD9E565(ln_4.FromX, ln_4.FromY, ln_4.FromZ);
        const b_1 = b.Origin;
        rayOrigin = Vec_$ctor_Z7AD9E565(a.X - b_1.X, a.Y - b_1.Y, a.Z - b_1.Z);
        const xAxis = b.Xaxis;
        const yAxis = b.Yaxis;
        const zAxis = b.Zaxis;
        let xLenSq;
        const v_1 = xAxis;
        xLenSq = (((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
        let yLenSq;
        const v_2 = yAxis;
        yLenSq = (((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z));
        let zLenSq;
        const v_3 = zAxis;
        zLenSq = (((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z));
        let tMin = -1.7976931348623157E+308;
        let tMax = 1.7976931348623157E+308;
        if (!(xLenSq > 1E-24)) {
            let originDotX;
            const a_1 = rayOrigin;
            const b_2 = xAxis;
            originDotX = (((a_1.X * b_2.X) + (a_1.Y * b_2.Y)) + (a_1.Z * b_2.Z));
            if ((originDotX < 0) ? true : (originDotX > xLenSq)) {
                tMin = 1.7976931348623157E+308;
            }
        }
        else {
            let dirDotX;
            const a_2 = rayDir;
            const b_3 = xAxis;
            dirDotX = (((a_2.X * b_3.X) + (a_2.Y * b_3.Y)) + (a_2.Z * b_3.Z));
            let originDotX_1;
            const a_3 = rayOrigin;
            const b_4 = xAxis;
            originDotX_1 = (((a_3.X * b_4.X) + (a_3.Y * b_4.Y)) + (a_3.Z * b_4.Z));
            if (Math.abs(dirDotX) < 1E-18) {
                if ((originDotX_1 < 0) ? true : (originDotX_1 > xLenSq)) {
                    tMin = 1.7976931348623157E+308;
                }
            }
            else {
                const t1 = -originDotX_1 / dirDotX;
                const t2 = (xLenSq - originDotX_1) / dirDotX;
                if (t1 < t2) {
                    tMin = max(tMin, t1);
                    tMax = min(tMax, t2);
                }
                else {
                    tMin = max(tMin, t2);
                    tMax = min(tMax, t1);
                }
            }
        }
        if (tMin <= tMax) {
            if (!(yLenSq > 1E-24)) {
                let originDotY;
                const a_4 = rayOrigin;
                const b_5 = yAxis;
                originDotY = (((a_4.X * b_5.X) + (a_4.Y * b_5.Y)) + (a_4.Z * b_5.Z));
                if ((originDotY < 0) ? true : (originDotY > yLenSq)) {
                    tMin = 1.7976931348623157E+308;
                }
            }
            else {
                let dirDotY;
                const a_5 = rayDir;
                const b_6 = yAxis;
                dirDotY = (((a_5.X * b_6.X) + (a_5.Y * b_6.Y)) + (a_5.Z * b_6.Z));
                let originDotY_1;
                const a_6 = rayOrigin;
                const b_7 = yAxis;
                originDotY_1 = (((a_6.X * b_7.X) + (a_6.Y * b_7.Y)) + (a_6.Z * b_7.Z));
                if (Math.abs(dirDotY) < 1E-18) {
                    if ((originDotY_1 < 0) ? true : (originDotY_1 > yLenSq)) {
                        tMin = 1.7976931348623157E+308;
                    }
                }
                else {
                    const t1_1 = -originDotY_1 / dirDotY;
                    const t2_1 = (yLenSq - originDotY_1) / dirDotY;
                    if (t1_1 < t2_1) {
                        tMin = max(tMin, t1_1);
                        tMax = min(tMax, t2_1);
                    }
                    else {
                        tMin = max(tMin, t2_1);
                        tMax = min(tMax, t1_1);
                    }
                }
            }
        }
        if (tMin <= tMax) {
            if (!(zLenSq > 1E-24)) {
                let originDotZ;
                const a_7 = rayOrigin;
                const b_8 = zAxis;
                originDotZ = (((a_7.X * b_8.X) + (a_7.Y * b_8.Y)) + (a_7.Z * b_8.Z));
                if ((originDotZ < 0) ? true : (originDotZ > zLenSq)) {
                    tMin = 1.7976931348623157E+308;
                }
            }
            else {
                let dirDotZ;
                const a_8 = rayDir;
                const b_9 = zAxis;
                dirDotZ = (((a_8.X * b_9.X) + (a_8.Y * b_9.Y)) + (a_8.Z * b_9.Z));
                let originDotZ_1;
                const a_9 = rayOrigin;
                const b_10 = zAxis;
                originDotZ_1 = (((a_9.X * b_10.X) + (a_9.Y * b_10.Y)) + (a_9.Z * b_10.Z));
                if (Math.abs(dirDotZ) < 1E-18) {
                    if ((originDotZ_1 < 0) ? true : (originDotZ_1 > zLenSq)) {
                        tMin = 1.7976931348623157E+308;
                    }
                }
                else {
                    const t1_2 = -originDotZ_1 / dirDotZ;
                    const t2_2 = (zLenSq - originDotZ_1) / dirDotZ;
                    if (t1_2 < t2_2) {
                        tMin = max(tMin, t1_2);
                        tMax = min(tMax, t2_2);
                    }
                    else {
                        tMin = max(tMin, t2_2);
                        tMax = min(tMax, t1_2);
                    }
                }
            }
        }
        if (tMin <= tMax) {
            return [tMin, tMax];
        }
        else {
            return undefined;
        }
    }
}

/**
 * Returns the bottom corners of the Box in Counter-Clockwise order, starting at Origin.
 * Then the top corners staring above Origin. Returns an array of 8 Points.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/     local
 * +---------------+----> X-Axis
 * 0               1
 * </code>
 */
export function Box__get_Points(b) {
    let p_3, v_3, p_4, v_4, p_5, v_5, p_6, v_6;
    const p0 = b.Origin;
    let p1;
    const p = p0;
    const v = b.Xaxis;
    p1 = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
    let p4;
    const p_1 = p0;
    const v_1 = b.Zaxis;
    p4 = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    let p5;
    const p_2 = p4;
    const v_2 = b.Xaxis;
    p5 = Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z);
    return [p0, p1, (p_3 = p1, (v_3 = b.Yaxis, Pnt_$ctor_Z7AD9E565(p_3.X + v_3.X, p_3.Y + v_3.Y, p_3.Z + v_3.Z))), (p_4 = p0, (v_4 = b.Yaxis, Pnt_$ctor_Z7AD9E565(p_4.X + v_4.X, p_4.Y + v_4.Y, p_4.Z + v_4.Z))), p4, p5, (p_5 = p5, (v_5 = b.Yaxis, Pnt_$ctor_Z7AD9E565(p_5.X + v_5.X, p_5.Y + v_5.Y, p_5.Z + v_5.Z))), (p_6 = p4, (v_6 = b.Yaxis, Pnt_$ctor_Z7AD9E565(p_6.X + v_6.X, p_6.Y + v_6.Y, p_6.Z + v_6.Z)))];
}

/**
 * Returns 6 face of the Box in
 * The normal of the Rect3Ds are oriented with the X-Axis, Y-Axis or Z-Axis.
 * The order of the Rect3D is: BottomFace, FrontFace, RightFace, BackFace, LeftFace, TopFace.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/     local
 * +---------------+----> X-Axis
 * 0               1
 * </code>
 */
export function Box__get_Faces(b) {
    return [Box__get_BottomFace(b), Box__get_FrontFace(b), Box__get_RightFace(b), Box__get_BackFace(b), Box__get_LeftFace(b), Box__get_TopFace(b)];
}

/**
 * Returns the top face of the Box in Counter-Clockwise order, looking from above.
 * Returns Origin at point 4, X-Axis to point 5, Y-Axis to point 7.
 * The normal of the Rect3D points away from the Box.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/     local
 * +---------------+----> X-Axis
 * 0               1
 * </code>
 */
export function Box__get_TopFace(b) {
    let p, v;
    return Rect3D_$ctor_6181AF53((p = b.Origin, (v = b.Zaxis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z))), b.Xaxis, b.Yaxis);
}

/**
 * Returns the bottom face of the Box in Counter-Clockwise order, looking from above.
 * Returns Origin at point 0, X-Axis to point 1, Y-Axis to point 3.
 * The normal of the Rect3D points into the Box.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/     local
 * +---------------+----> X-Axis
 * 0               1
 * </code>
 */
export function Box__get_BottomFace(b) {
    return Rect3D_$ctor_6181AF53(b.Origin, b.Xaxis, b.Yaxis);
}

/**
 * Returns the front face of the Box in Counter-Clockwise order, looking from front.
 * Returns Origin at point 0, X-Axis to point 1, Y-Axis to point 4.
 * The normal of the Rect3D points away from the Box.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/     local
 * +---------------+----> X-Axis
 * 0               1
 * </code>
 */
export function Box__get_FrontFace(b) {
    return Rect3D_$ctor_6181AF53(b.Origin, b.Xaxis, b.Zaxis);
}

/**
 * Returns the back face of the Box in Counter-Clockwise order, looking from front.
 * Returns Origin at point 3, X-Axis to point 2, Y-Axis to point 7.
 * The normal of the Rect3D points into the Box.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/     local
 * +---------------+----> X-Axis
 * 0               1
 * </code>
 */
export function Box__get_BackFace(b) {
    let p, v;
    return Rect3D_$ctor_6181AF53((p = b.Origin, (v = b.Yaxis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z))), b.Xaxis, b.Zaxis);
}

/**
 * Returns the right face of the Box in Counter-Clockwise order, looking from right.
 * Returns Origin at point 1, X-Axis to point 2, Y-Axis to point 5.
 * The normal of the Rect3D points away from the Box.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/     local
 * +---------------+----> X-Axis
 * 0               1
 * </code>
 */
export function Box__get_RightFace(b) {
    let p, v;
    return Rect3D_$ctor_6181AF53((p = b.Origin, (v = b.Xaxis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z))), b.Yaxis, b.Zaxis);
}

/**
 * Returns the left face of the Box in Counter-Clockwise order, looking from right.
 * Returns Origin at point 0, X-Axis to point 3, Y-Axis to point 4.
 * The normal of the Rect3D points into the Box.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/     local
 * +---------------+----> X-Axis
 * 0               1
 * </code>
 */
export function Box__get_LeftFace(b) {
    return Rect3D_$ctor_6181AF53(b.Origin, b.Yaxis, b.Zaxis);
}

/**
 * Returns the 12 box edges.
 * The returned line is parallel to and oriented with the X-Axis, Y-Axis or Z-Axis.
 * <code>
 * local        local
 * Z-Axis       Y-Axis
 * ^           /
 * |          /
 * |   +--------E6-----+
 * |  /|    /         /|
 * |E7 E11 /         E5|
 * |/  |  /         /  |
 * +--------E4-----+   E10
 * |   |/          E9  |
 * E8  +-----E2----|---+
 * |  /            |  /
 * | E3            | E1
 * |/              |/       local
 * +------E0 ------+------> X-Axis
 * </code>
 */
export function Box__get_Edges(b) {
    const p0 = b.Origin;
    let p1;
    const p = p0;
    const v = b.Xaxis;
    p1 = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
    let p4;
    const p_1 = p0;
    const v_1 = b.Zaxis;
    p4 = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
    let p5;
    const p_2 = p4;
    const v_2 = b.Xaxis;
    p5 = Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z);
    const y = b.Yaxis;
    let p2;
    const p_3 = p1;
    const v_3 = y;
    p2 = Pnt_$ctor_Z7AD9E565(p_3.X + v_3.X, p_3.Y + v_3.Y, p_3.Z + v_3.Z);
    let p3;
    const p_4 = p0;
    const v_4 = y;
    p3 = Pnt_$ctor_Z7AD9E565(p_4.X + v_4.X, p_4.Y + v_4.Y, p_4.Z + v_4.Z);
    let p6;
    const p_5 = p5;
    const v_5 = y;
    p6 = Pnt_$ctor_Z7AD9E565(p_5.X + v_5.X, p_5.Y + v_5.Y, p_5.Z + v_5.Z);
    let p7;
    const p_6 = p4;
    const v_6 = y;
    p7 = Pnt_$ctor_Z7AD9E565(p_6.X + v_6.X, p_6.Y + v_6.Y, p_6.Z + v_6.Z);
    return [Line3D_$ctor_5A6659A0(p0, p1), Line3D_$ctor_5A6659A0(p1, p2), Line3D_$ctor_5A6659A0(p3, p2), Line3D_$ctor_5A6659A0(p0, p3), Line3D_$ctor_5A6659A0(p4, p5), Line3D_$ctor_5A6659A0(p5, p6), Line3D_$ctor_5A6659A0(p7, p6), Line3D_$ctor_5A6659A0(p4, p7), Line3D_$ctor_5A6659A0(p0, p4), Line3D_$ctor_5A6659A0(p1, p5), Line3D_$ctor_5A6659A0(p2, p6), Line3D_$ctor_5A6659A0(p3, p7)];
}

