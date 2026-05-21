
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { Pnt__get_AsString, Pnt_$ctor_Z7AD9E565 } from "./Pnt.js";
import { Line3D_$ctor_5A6659A0 } from "./Line3D.js";
import { fail } from "./EuclidErrors.js";

/**
 * A struct of 6 floats representing an immutable 3D bounding box.
 * This implementation guarantees the box to always be valid.
 * That means the Min X, Y, and Z values are always smaller or equal to the respective Max values.
 * The X, Y, and Z axes are also called Width, Depth, and Height3D.
 * <code>
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6 MaxPt
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/
 * +---------------+----> X-Axis
 * 0 MinPt         1
 * </code>
 */
export class BBox extends Record {
    constructor(MinX, MinY, MinZ, MaxX, MaxY, MaxZ) {
        super();
        this.MinX = MinX;
        this.MinY = MinY;
        this.MinZ = MinZ;
        this.MaxX = MaxX;
        this.MaxY = MaxY;
        this.MaxZ = MaxZ;
    }
    /**
     * Nicely formatted string representation of the bounding box, including its size.
     */
    toString() {
        const b = this;
        const sizeX = float(b.MaxX - b.MinX);
        const sizeY = float(b.MaxY - b.MinY);
        const sizeZ = float(b.MaxZ - b.MinZ);
        const minX = float(b.MinX);
        const minY = float(b.MinY);
        const minZ = float(b.MinZ);
        return `Euclid.BBox: Size: x=${sizeX}|y=${sizeY}|z=${sizeZ} (at X=${minX}|Y=${minY}|Z=${minZ})`;
    }
}

export function BBox_$reflection() {
    return class_type("Euclid.BBox", undefined, BBox, class_type("System.ValueType"));
}

/**
 * Unsafe internal constructor, public only for inlining.
 */
export function BBox_$ctor_76A78260(minX, minY, minZ, maxX, maxY, maxZ) {
    return new BBox(minX, minY, minZ, maxX, maxY, maxZ);
}

/**
 * Format bounding box into string with nice floating point number formatting of size and position.
 * But without full type name as in bbox.ToString()
 */
export function BBox__get_AsString(b) {
    const sizeX = float(b.MaxX - b.MinX);
    const sizeY = float(b.MaxY - b.MinY);
    const sizeZ = float(b.MaxZ - b.MinZ);
    const minX = float(b.MinX);
    const minY = float(b.MinY);
    const minZ = float(b.MinZ);
    return `Size: x=${sizeX}|y=${sizeY}|z=${sizeZ} (at X=${minX}|Y=${minY}|Z=${minZ})`;
}

/**
 * Format bounding box into an F# code string that can be used to recreate the bounding box.
 */
export function BBox__get_AsFSharpCode(b) {
    return `BBox.createUnchecked(${b.MinX}, ${b.MinY}, ${b.MinZ}, ${b.MaxX}, ${b.MaxY}, ${b.MaxZ})`;
}

/**
 * Test if 3D bounding boxes are only touching each other from the Outside within a given tolerance.
 */
export function BBox__IsTouching_668B4595(b, a, tol = 1E-06) {
    const xOverlap = !((b.MinX > (a.MaxX + tol)) ? true : (a.MinX > (b.MaxX + tol)));
    const yOverlap = !((a.MinY > (b.MaxY + tol)) ? true : (b.MinY > (a.MaxY + tol)));
    const zOverlap = !((a.MinZ > (b.MaxZ + tol)) ? true : (b.MinZ > (a.MaxZ + tol)));
    const xTouch = (Math.abs(b.MinX - a.MaxX) <= tol) ? true : (Math.abs(a.MinX - b.MaxX) <= tol);
    const yTouch = (Math.abs(a.MinY - b.MaxY) <= tol) ? true : (Math.abs(b.MinY - a.MaxY) <= tol);
    const zTouch = (Math.abs(a.MinZ - b.MaxZ) <= tol) ? true : (Math.abs(b.MinZ - a.MaxZ) <= tol);
    if (((xOverlap && yOverlap) && zTouch) ? true : ((xOverlap && yTouch) && zOverlap)) {
        return true;
    }
    else if (xTouch && yOverlap) {
        return zOverlap;
    }
    else {
        return false;
    }
}

/**
 * Returns the bottom corners of this 3D bounding box in Counter-Clockwise order, starting at MinPt.
 * Then the top corners starting above MinPt. Returns an array of 8 Points.
 * <code>
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6 MaxPt
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/
 * +---------------+----> X-Axis
 * 0 MinPt         1
 * </code>
 */
export function BBox__get_Points(b) {
    let b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8;
    return [(b_1 = b, Pnt_$ctor_Z7AD9E565(b_1.MinX, b_1.MinY, b_1.MinZ)), (b_2 = b, Pnt_$ctor_Z7AD9E565(b_2.MaxX, b_2.MinY, b_2.MinZ)), (b_3 = b, Pnt_$ctor_Z7AD9E565(b_3.MaxX, b_3.MaxY, b_3.MinZ)), (b_4 = b, Pnt_$ctor_Z7AD9E565(b_4.MinX, b_4.MaxY, b_4.MinZ)), (b_5 = b, Pnt_$ctor_Z7AD9E565(b_5.MinX, b_5.MinY, b_5.MaxZ)), (b_6 = b, Pnt_$ctor_Z7AD9E565(b_6.MaxX, b_6.MinY, b_6.MaxZ)), (b_7 = b, Pnt_$ctor_Z7AD9E565(b_7.MaxX, b_7.MaxY, b_7.MaxZ)), (b_8 = b, Pnt_$ctor_Z7AD9E565(b_8.MinX, b_8.MaxY, b_8.MaxZ))];
}

/**
 * Returns the bottom of the box as a Counter-Clockwise array of 4 Points.
 * Starting at MinPt. Points 0, 1, 2, and 3.
 * Last and first point are NOT the same.
 * <code>
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6 MaxPt
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/
 * +---------------+----> X-Axis
 * 0 MinPt         1
 * </code>
 */
export function BBox__get_BottomPoints(b) {
    let b_1, b_2, b_3, b_4;
    return [(b_1 = b, Pnt_$ctor_Z7AD9E565(b_1.MinX, b_1.MinY, b_1.MinZ)), (b_2 = b, Pnt_$ctor_Z7AD9E565(b_2.MaxX, b_2.MinY, b_2.MinZ)), (b_3 = b, Pnt_$ctor_Z7AD9E565(b_3.MaxX, b_3.MaxY, b_3.MinZ)), (b_4 = b, Pnt_$ctor_Z7AD9E565(b_4.MinX, b_4.MaxY, b_4.MinZ))];
}

/**
 * Returns the bottom of the box as a Counter-Clockwise array of 5 Points, starting at MinPt.
 * Points 0, 1, 2, 3, and again 0.
 * Last and first point are the same.
 * <code>
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6 MaxPt
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/
 * +---------------+----> X-Axis
 * 0 MinPt         1
 * </code>
 */
export function BBox__get_BottomPointsLooped(b) {
    let b_1, b_2, b_3, b_4, b_5;
    return [(b_1 = b, Pnt_$ctor_Z7AD9E565(b_1.MinX, b_1.MinY, b_1.MinZ)), (b_2 = b, Pnt_$ctor_Z7AD9E565(b_2.MaxX, b_2.MinY, b_2.MinZ)), (b_3 = b, Pnt_$ctor_Z7AD9E565(b_3.MaxX, b_3.MaxY, b_3.MinZ)), (b_4 = b, Pnt_$ctor_Z7AD9E565(b_4.MinX, b_4.MaxY, b_4.MinZ)), (b_5 = b, Pnt_$ctor_Z7AD9E565(b_5.MinX, b_5.MinY, b_5.MinZ))];
}

/**
 * Returns the top of the box as a Counter-Clockwise array of 4 Points.
 * Starting at point 4 then 5, 6, and 7.
 * Last and first point are NOT the same.
 * <code>
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6 MaxPt
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/
 * +---------------+----> X-Axis
 * 0 MinPt         1
 * </code>
 */
export function BBox__get_TopPoints(b) {
    let b_1, b_2, b_3, b_4;
    return [(b_1 = b, Pnt_$ctor_Z7AD9E565(b_1.MinX, b_1.MinY, b_1.MaxZ)), (b_2 = b, Pnt_$ctor_Z7AD9E565(b_2.MaxX, b_2.MinY, b_2.MaxZ)), (b_3 = b, Pnt_$ctor_Z7AD9E565(b_3.MaxX, b_3.MaxY, b_3.MaxZ)), (b_4 = b, Pnt_$ctor_Z7AD9E565(b_4.MinX, b_4.MaxY, b_4.MaxZ))];
}

/**
 * Returns the top of the box as a Counter-Clockwise array of 5 Points.
 * Points 4, 5, 6, 7, and again 4.
 * Last and first point are the same.
 * <code>
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6 MaxPt
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/
 * +---------------+----> X-Axis
 * 0 MinPt         1
 * </code>
 */
export function BBox__get_TopPointsLooped(b) {
    let b_1, b_2, b_3, b_4, b_5;
    return [(b_1 = b, Pnt_$ctor_Z7AD9E565(b_1.MinX, b_1.MinY, b_1.MaxZ)), (b_2 = b, Pnt_$ctor_Z7AD9E565(b_2.MaxX, b_2.MinY, b_2.MaxZ)), (b_3 = b, Pnt_$ctor_Z7AD9E565(b_3.MaxX, b_3.MaxY, b_3.MaxZ)), (b_4 = b, Pnt_$ctor_Z7AD9E565(b_4.MinX, b_4.MaxY, b_4.MaxZ)), (b_5 = b, Pnt_$ctor_Z7AD9E565(b_5.MinX, b_5.MinY, b_5.MaxZ))];
}

/**
 * Returns the 12 edges of this 3D bounding box as an array of 12 Lines.
 * Pairs in this order:
 * 0-1, 1-2, 3-2, 0-3, 0-4, 1-5, 2-6, 3-7, 4-5, 5-6, 7-6, 4-7
 * <code>
 * Z-Axis       Y-Axis
 * ^           /
 * |   7      /        6 MaxPt
 * |   +---------------+
 * |  /|    /         /|
 * | / |   /         / |
 * 4 |/  |  /       5 /  |
 * +---------------+   |
 * |   |/          |   |
 * |   +-----------|---+
 * |  / 3          |  / 2
 * | /             | /
 * |/              |/
 * +---------------+----> X-Axis
 * 0 MinPt         1
 * </code>
 */
export function BBox__get_Edges(b) {
    let b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8, b_9, b_10, b_11, b_12, b_13, b_14, b_15, b_16, b_17, b_18, b_19, b_20, b_21, b_22, b_23, b_24, b_25, b_26, b_27, b_28, b_29, b_30, b_31, b_32, b_33, b_34, b_35, b_36;
    return [(b_1 = b, Line3D_$ctor_5A6659A0((b_2 = b_1, Pnt_$ctor_Z7AD9E565(b_2.MinX, b_2.MinY, b_2.MinZ)), (b_3 = b_1, Pnt_$ctor_Z7AD9E565(b_3.MaxX, b_3.MinY, b_3.MinZ)))), (b_4 = b, Line3D_$ctor_5A6659A0((b_5 = b_4, Pnt_$ctor_Z7AD9E565(b_5.MaxX, b_5.MinY, b_5.MinZ)), (b_6 = b_4, Pnt_$ctor_Z7AD9E565(b_6.MaxX, b_6.MaxY, b_6.MinZ)))), (b_7 = b, Line3D_$ctor_5A6659A0((b_8 = b_7, Pnt_$ctor_Z7AD9E565(b_8.MinX, b_8.MaxY, b_8.MinZ)), (b_9 = b_7, Pnt_$ctor_Z7AD9E565(b_9.MaxX, b_9.MaxY, b_9.MinZ)))), (b_10 = b, Line3D_$ctor_5A6659A0((b_11 = b_10, Pnt_$ctor_Z7AD9E565(b_11.MinX, b_11.MinY, b_11.MinZ)), (b_12 = b_10, Pnt_$ctor_Z7AD9E565(b_12.MinX, b_12.MaxY, b_12.MinZ)))), (b_13 = b, Line3D_$ctor_5A6659A0((b_14 = b_13, Pnt_$ctor_Z7AD9E565(b_14.MinX, b_14.MinY, b_14.MinZ)), (b_15 = b_13, Pnt_$ctor_Z7AD9E565(b_15.MinX, b_15.MinY, b_15.MaxZ)))), (b_16 = b, Line3D_$ctor_5A6659A0((b_17 = b_16, Pnt_$ctor_Z7AD9E565(b_17.MaxX, b_17.MinY, b_17.MinZ)), (b_18 = b_16, Pnt_$ctor_Z7AD9E565(b_18.MaxX, b_18.MinY, b_18.MaxZ)))), (b_19 = b, Line3D_$ctor_5A6659A0((b_20 = b_19, Pnt_$ctor_Z7AD9E565(b_20.MaxX, b_20.MaxY, b_20.MinZ)), (b_21 = b_19, Pnt_$ctor_Z7AD9E565(b_21.MaxX, b_21.MaxY, b_21.MaxZ)))), (b_22 = b, Line3D_$ctor_5A6659A0((b_23 = b_22, Pnt_$ctor_Z7AD9E565(b_23.MinX, b_23.MaxY, b_23.MinZ)), (b_24 = b_22, Pnt_$ctor_Z7AD9E565(b_24.MinX, b_24.MaxY, b_24.MaxZ)))), (b_25 = b, Line3D_$ctor_5A6659A0((b_26 = b_25, Pnt_$ctor_Z7AD9E565(b_26.MinX, b_26.MinY, b_26.MaxZ)), (b_27 = b_25, Pnt_$ctor_Z7AD9E565(b_27.MaxX, b_27.MinY, b_27.MaxZ)))), (b_28 = b, Line3D_$ctor_5A6659A0((b_29 = b_28, Pnt_$ctor_Z7AD9E565(b_29.MaxX, b_29.MinY, b_29.MaxZ)), (b_30 = b_28, Pnt_$ctor_Z7AD9E565(b_30.MaxX, b_30.MaxY, b_30.MaxZ)))), (b_31 = b, Line3D_$ctor_5A6659A0((b_32 = b_31, Pnt_$ctor_Z7AD9E565(b_32.MinX, b_32.MaxY, b_32.MaxZ)), (b_33 = b_31, Pnt_$ctor_Z7AD9E565(b_33.MaxX, b_33.MaxY, b_33.MaxZ)))), (b_34 = b, Line3D_$ctor_5A6659A0((b_35 = b_34, Pnt_$ctor_Z7AD9E565(b_35.MinX, b_35.MinY, b_35.MaxZ)), (b_36 = b_34, Pnt_$ctor_Z7AD9E565(b_36.MinX, b_36.MaxY, b_36.MaxZ))))];
}

/**
 * Checks if two 3D bounding boxes are equal within tolerance.
 * Use a tolerance of 0.0 to check for an exact match.
 */
export function BBox_equals(tol, a, b) {
    if (((((Math.abs(a.MinX - b.MinX) <= tol) && (Math.abs(a.MinY - b.MinY) <= tol)) && (Math.abs(a.MinZ - b.MinZ) <= tol)) && (Math.abs(a.MaxX - b.MaxX) <= tol)) && (Math.abs(a.MaxY - b.MaxY) <= tol)) {
        return Math.abs(a.MaxZ - b.MaxZ) <= tol;
    }
    else {
        return false;
    }
}

/**
 * Check if two 3D bounding boxes are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two 3D bounding boxes are not exactly equal.
 */
export function BBox_notEquals(tol, a, b) {
    if (((((Math.abs(a.MinX - b.MinX) > tol) ? true : (Math.abs(a.MinY - b.MinY) > tol)) ? true : (Math.abs(a.MinZ - b.MinZ) > tol)) ? true : (Math.abs(a.MaxX - b.MaxX) > tol)) ? true : (Math.abs(a.MaxY - b.MaxY) > tol)) {
        return true;
    }
    else {
        return Math.abs(a.MaxZ - b.MaxZ) > tol;
    }
}

/**
 * Returns a 3D bounding box expanded by distance.
 * Does check for underflow if distance is negative and raises EuclidException.
 */
export function BBox_expand(dist, b) {
    const b_1 = b;
    const dist_1 = dist;
    const n = BBox_$ctor_76A78260(b_1.MinX - dist_1, b_1.MinY - dist_1, b_1.MinZ - dist_1, b_1.MaxX + dist_1, b_1.MaxY + dist_1, b_1.MaxZ + dist_1);
    if ((dist_1 < 0) && (((n.MinX > n.MaxX) ? true : (n.MinY > n.MaxY)) ? true : (n.MinZ > n.MaxZ))) {
        fail(`BBox.Expand(dist): Negative distance ${dist_1} causes an underflow, on ${BBox__get_AsString(b_1)}`);
    }
    return n;
}

/**
 * Returns a 3D bounding box expanded by distance.
 * If expansion is negative it shrinks the Box without causing underflow.
 * When the negative expansion is bigger than the size on any axis, both Min and Max values
 * on that axis will be set to the midpoint of their original values (the box collapses to a plane or line on that axis).
 */
export function BBox_expandSafe(dist, b) {
    const dist_1 = dist;
    const b_2 = b;
    const xDist = dist_1;
    const yDist = dist_1;
    const zDist = dist_1;
    let minXCh = b_2.MinX - xDist;
    let maxXCh = b_2.MaxX + xDist;
    if (minXCh > maxXCh) {
        const mid = b_2.MinX + ((b_2.MaxX - b_2.MinX) * 0.5);
        minXCh = mid;
        maxXCh = mid;
    }
    let minYCh = b_2.MinY - yDist;
    let maxYCh = b_2.MaxY + yDist;
    if (minYCh > maxYCh) {
        const mid_1 = b_2.MinY + ((b_2.MaxY - b_2.MinY) * 0.5);
        minYCh = mid_1;
        maxYCh = mid_1;
    }
    let minZCh = b_2.MinZ - zDist;
    let maxZCh = b_2.MaxZ + zDist;
    if (minZCh > maxZCh) {
        const mid_2 = b_2.MinZ + ((b_2.MaxZ - b_2.MinZ) * 0.5);
        minZCh = mid_2;
        maxZCh = mid_2;
    }
    return BBox_$ctor_76A78260(minXCh, minYCh, minZCh, maxXCh, maxYCh, maxZCh);
}

/**
 * Returns a 3D bounding box expanded only in X direction by different distances for start(minX) and end (maxX).
 * Does check for underflow if distance is negative and raises EuclidException.
 */
export function BBox_expandXaxis(startDist, endDist, b) {
    const b_1 = b;
    const startDist_1 = startDist;
    const endDist_1 = endDist;
    const n = BBox_$ctor_76A78260(b_1.MinX - startDist_1, b_1.MinY, b_1.MinZ, b_1.MaxX + endDist_1, b_1.MaxY, b_1.MaxZ);
    if (n.MinX > n.MaxX) {
        fail(`BBox.ExpandXaxis: Negative distances for start(${startDist_1}) and end (${endDist_1}) cause an underflow, on ${BBox__get_AsString(b_1)}`);
    }
    return n;
}

/**
 * Returns a 3D bounding box expanded only in Y direction by different distances for start(minY) and end (maxY).
 * Does check for underflow if distance is negative and raises EuclidException.
 */
export function BBox_expandYaxis(startDist, endDist, b) {
    const b_1 = b;
    const startDist_1 = startDist;
    const endDist_1 = endDist;
    const n = BBox_$ctor_76A78260(b_1.MinX, b_1.MinY - startDist_1, b_1.MinZ, b_1.MaxX, b_1.MaxY + endDist_1, b_1.MaxZ);
    if (n.MinY > n.MaxY) {
        fail(`BBox.ExpandYaxis: Negative distances for start(${startDist_1}) and end(${endDist_1}) cause an underflow, on ${BBox__get_AsString(b_1)}`);
    }
    return n;
}

/**
 * Returns a 3D bounding box expanded only in Z direction by different distances for start(minZ) and end (maxZ).
 * Does check for underflow if distance is negative and raises EuclidException.
 */
export function BBox_expandZaxis(startDist, endDist, b) {
    const b_1 = b;
    const startDist_1 = startDist;
    const endDist_1 = endDist;
    const n = BBox_$ctor_76A78260(b_1.MinX, b_1.MinY, b_1.MinZ - startDist_1, b_1.MaxX, b_1.MaxY, b_1.MaxZ + endDist_1);
    if (n.MinZ > n.MaxZ) {
        fail(`BBox.ExpandZaxis: Negative distances for start(${startDist_1}) and end(${endDist_1}) cause an underflow, on ${BBox__get_AsString(b_1)}`);
    }
    return n;
}

/**
 * Returns the 3D bounding box expanded by a relative factor on all six sides.
 * Values between 0.0 and 1.0 shrink the box.
 * Values larger than 1.0 expand the box.
 * Does check for underflow if factor is negative and raises EuclidException.
 */
export function BBox_expandRel(factor, b) {
    let b_2, b_3, b_4;
    if (factor < 0) {
        fail(`BBox.expandRel: a negative factor ${factor} is not allowed for expanding the 3D bounding box ${BBox__get_AsString(b)}`);
    }
    let center;
    const b_1 = b;
    center = Pnt_$ctor_Z7AD9E565((b_1.MaxX + b_1.MinX) * 0.5, (b_1.MaxY + b_1.MinY) * 0.5, (b_1.MaxZ + b_1.MinZ) * 0.5);
    const sizeX = ((b_2 = b, b_2.MaxX - b_2.MinX)) * factor;
    const sizeY = ((b_3 = b, b_3.MaxY - b_3.MinY)) * factor;
    const sizeZ = ((b_4 = b, b_4.MaxZ - b_4.MinZ)) * factor;
    const center_1 = center;
    const sizeX_1 = sizeX;
    const sizeY_1 = sizeY;
    const sizeZ_1 = sizeZ;
    if (!(sizeX_1 >= 0)) {
        fail(`BBox.createFromCenter sizeX is negative: ${sizeX_1}, sizeY is: ${sizeY_1}, sizeZ is: ${sizeZ_1}, center: ${Pnt__get_AsString(center_1)}`);
    }
    if (!(sizeY_1 >= 0)) {
        fail(`BBox.createFromCenter sizeY is negative: ${sizeY_1}, sizeX is: ${sizeX_1}, sizeZ is: ${sizeZ_1}, center: ${Pnt__get_AsString(center_1)}`);
    }
    if (!(sizeZ_1 >= 0)) {
        fail(`BBox.createFromCenter sizeZ is negative: ${sizeZ_1}, sizeX is: ${sizeX_1}, sizeY is: ${sizeY_1}, center: ${Pnt__get_AsString(center_1)}`);
    }
    const minX = center_1.X - (sizeX_1 * 0.5);
    const minY = center_1.Y - (sizeY_1 * 0.5);
    const maxX = center_1.X + (sizeX_1 * 0.5);
    const maxY = center_1.Y + (sizeY_1 * 0.5);
    const minZ = center_1.Z - (sizeZ_1 * 0.5);
    const maxZ = center_1.Z + (sizeZ_1 * 0.5);
    return BBox_$ctor_76A78260(minX, minY, minZ, maxX, maxY, maxZ);
}

/**
 * Returns the 3D bounding box expanded by a relative factor on all six sides, separately for X, Y, Z.
 * Values between 0.0 and 1.0 shrink the box.
 * Values larger than 1.0 expand the box.
 * Does check for underflow if any factor is negative and raises EuclidException.
 */
export function BBox_expandRelXYZ(factorX, factorY, factorZ, b) {
    let b_2, b_3, b_4;
    if (factorX < 0) {
        fail(`BBox.expandRelXYZ: a negative factorX ${factorX} is not allowed for expanding the 3D bounding box ${BBox__get_AsString(b)}`);
    }
    if (factorY < 0) {
        fail(`BBox.expandRelXYZ: a negative factorY ${factorY} is not allowed for expanding the 3D bounding box ${BBox__get_AsString(b)}`);
    }
    if (factorZ < 0) {
        fail(`BBox.expandRelXYZ: a negative factorZ ${factorZ} is not allowed for expanding the 3D bounding box ${BBox__get_AsString(b)}`);
    }
    let center;
    const b_1 = b;
    center = Pnt_$ctor_Z7AD9E565((b_1.MaxX + b_1.MinX) * 0.5, (b_1.MaxY + b_1.MinY) * 0.5, (b_1.MaxZ + b_1.MinZ) * 0.5);
    const sizeX = ((b_2 = b, b_2.MaxX - b_2.MinX)) * factorX;
    const sizeY = ((b_3 = b, b_3.MaxY - b_3.MinY)) * factorY;
    const sizeZ = ((b_4 = b, b_4.MaxZ - b_4.MinZ)) * factorZ;
    const center_1 = center;
    const sizeX_1 = sizeX;
    const sizeY_1 = sizeY;
    const sizeZ_1 = sizeZ;
    if (!(sizeX_1 >= 0)) {
        fail(`BBox.createFromCenter sizeX is negative: ${sizeX_1}, sizeY is: ${sizeY_1}, sizeZ is: ${sizeZ_1}, center: ${Pnt__get_AsString(center_1)}`);
    }
    if (!(sizeY_1 >= 0)) {
        fail(`BBox.createFromCenter sizeY is negative: ${sizeY_1}, sizeX is: ${sizeX_1}, sizeZ is: ${sizeZ_1}, center: ${Pnt__get_AsString(center_1)}`);
    }
    if (!(sizeZ_1 >= 0)) {
        fail(`BBox.createFromCenter sizeZ is negative: ${sizeZ_1}, sizeX is: ${sizeX_1}, sizeY is: ${sizeY_1}, center: ${Pnt__get_AsString(center_1)}`);
    }
    const minX = center_1.X - (sizeX_1 * 0.5);
    const minY = center_1.Y - (sizeY_1 * 0.5);
    const maxX = center_1.X + (sizeX_1 * 0.5);
    const maxY = center_1.Y + (sizeY_1 * 0.5);
    const minZ = center_1.Z - (sizeZ_1 * 0.5);
    const maxZ = center_1.Z + (sizeZ_1 * 0.5);
    return BBox_$ctor_76A78260(minX, minY, minZ, maxX, maxY, maxZ);
}

/**
 * Returns a 3D bounding box moved by a vector.
 */
export function BBox_move(v, b) {
    return BBox_$ctor_76A78260(b.MinX + v.X, b.MinY + v.Y, b.MinZ + v.Z, b.MaxX + v.X, b.MaxY + v.Y, b.MaxZ + v.Z);
}

/**
 * Returns a 3D bounding box moved by a vector.
 * This is an alias for the 'move' function.
 */
export function BBox_translate(v, b) {
    return BBox_$ctor_76A78260(b.MinX + v.X, b.MinY + v.Y, b.MinZ + v.Z, b.MaxX + v.X, b.MaxY + v.Y, b.MaxZ + v.Z);
}

/**
 * Returns a new 3D bounding box moved in X-axis direction.
 */
export function BBox_moveX(translation, b) {
    return BBox_$ctor_76A78260(b.MinX + translation, b.MinY, b.MinZ, b.MaxX + translation, b.MaxY, b.MaxZ);
}

/**
 * Returns a new 3D bounding box moved in Y-axis direction.
 */
export function BBox_moveY(translation, b) {
    return BBox_$ctor_76A78260(b.MinX, b.MinY + translation, b.MinZ, b.MaxX, b.MaxY + translation, b.MaxZ);
}

/**
 * Returns a new 3D bounding box moved in Z-axis direction.
 */
export function BBox_moveZ(translation, b) {
    return BBox_$ctor_76A78260(b.MinX, b.MinY, b.MinZ + translation, b.MaxX, b.MaxY, b.MaxZ + translation);
}

export function BBox_expandSave(dist, b) {
    const dist_1 = dist;
    const b_2 = b;
    const xDist = dist_1;
    const yDist = dist_1;
    const zDist = dist_1;
    let minXCh = b_2.MinX - xDist;
    let maxXCh = b_2.MaxX + xDist;
    if (minXCh > maxXCh) {
        const mid = b_2.MinX + ((b_2.MaxX - b_2.MinX) * 0.5);
        minXCh = mid;
        maxXCh = mid;
    }
    let minYCh = b_2.MinY - yDist;
    let maxYCh = b_2.MaxY + yDist;
    if (minYCh > maxYCh) {
        const mid_1 = b_2.MinY + ((b_2.MaxY - b_2.MinY) * 0.5);
        minYCh = mid_1;
        maxYCh = mid_1;
    }
    let minZCh = b_2.MinZ - zDist;
    let maxZCh = b_2.MaxZ + zDist;
    if (minZCh > maxZCh) {
        const mid_2 = b_2.MinZ + ((b_2.MaxZ - b_2.MinZ) * 0.5);
        minZCh = mid_2;
        maxZCh = mid_2;
    }
    return BBox_$ctor_76A78260(minXCh, minYCh, minZCh, maxXCh, maxYCh, maxZCh);
}

