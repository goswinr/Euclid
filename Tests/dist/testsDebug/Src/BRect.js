
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { fail } from "./EuclidErrors.js";
import { Pt__get_AsString, Pt_$ctor_7B00E9A0 } from "./Pt.js";
import { Line2D_$ctor_77D16AC0 } from "./Line2D.js";

/**
 * A struct of 4 floats representing an immutable 2D bounding rectangle.
 * Sometimes also called 2D a bounding box.
 * This implementation guarantees the rectangle to be always valid.
 * That means the Min X and Y values are always smaller or equal than the respective Max values.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2 max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |       local
 * +------------+-----> X-Axis
 * 0-min X,Y      1
 * </code>
 */
export class BRect extends Record {
    constructor(MinX, MinY, MaxX, MaxY) {
        super();
        this.MinX = MinX;
        this.MinY = MinY;
        this.MaxX = MaxX;
        this.MaxY = MaxY;
    }
    /**
     * Nicely formatted string representation of the bounding rectangle, including its size.
     */
    toString() {
        const r = this;
        const sizeX = float(r.MaxX - r.MinX);
        const sizeY = float(r.MaxY - r.MinY);
        const atX = float(r.MinX);
        const atY = float(r.MinY);
        return `Euclid.BRect: sizeX=${sizeX}| sizeY=${sizeY}| at X=${atX}|Y=${atY}`;
    }
}

export function BRect_$reflection() {
    return class_type("Euclid.BRect", undefined, BRect, class_type("System.ValueType"));
}

/**
 * Unsafe internal constructor, public only for inlining.
 */
export function BRect_$ctor_77D16AC0(minX, minY, maxX, maxY) {
    return new BRect(minX, minY, maxX, maxY);
}

/**
 * Format bounding rectangle into string with nice floating point number formatting of size and position.
 * But without full type name as in rect.ToString()
 */
export function BRect__get_AsString(r) {
    const sizeX = float(r.MaxX - r.MinX);
    const sizeY = float(r.MaxY - r.MinY);
    const atX = float(r.MinX);
    const atY = float(r.MinY);
    return `sizeX=${sizeX}| sizeY=${sizeY}| at X=${atX}|Y=${atY}`;
}

/**
 * Format bounding rectangle into an F# code string that can be used to recreate the rectangle.
 */
export function BRect__get_AsFSharpCode(r) {
    return `BRect.createUnchecked(${r.MinX}, ${r.MinY}, ${r.MaxX}, ${r.MaxY})`;
}

/**
 * Test if 2D bounding rectangles are only touching each other from the Outside within a given tolerance.
 */
export function BRect__IsTouching_13C371E0(b, a, tol = 1E-06) {
    const xOverlap = !((b.MinX > (a.MaxX + tol)) ? true : (a.MinX > (b.MaxX + tol)));
    const yOverlap = !((a.MinY > (b.MaxY + tol)) ? true : (b.MinY > (a.MaxY + tol)));
    const xTouch = (Math.abs(b.MinX - a.MaxX) <= tol) ? true : (Math.abs(a.MinX - b.MaxX) <= tol);
    const yTouch = (Math.abs(a.MinY - b.MaxY) <= tol) ? true : (Math.abs(b.MinY - a.MaxY) <= tol);
    if (xOverlap && yTouch) {
        return true;
    }
    else if (xTouch) {
        return yOverlap;
    }
    else {
        return false;
    }
}

/**
 * Returns the 2D bounding rectangle expanded by a relative factor on all four sides.
 * Values between 0.0 and 1.0 shrink the rectangle.
 * Values larger than 1.0 expand the rectangle.
 * Does check for underflow if factor is negative and raises EuclidException.
 */
export function BRect_expandRel(factor, r) {
    let r_2, r_3;
    if (factor < 0) {
        fail(`BRect.expandRel: a negative factor ${factor} is not allowed for expanding the 2D bounding rectangle ${BRect__get_AsString(r)}`);
    }
    let center;
    const r_1 = r;
    center = Pt_$ctor_7B00E9A0((r_1.MaxX + r_1.MinX) * 0.5, (r_1.MaxY + r_1.MinY) * 0.5);
    const sizeX = ((r_2 = r, r_2.MaxX - r_2.MinX)) * factor;
    const sizeY = ((r_3 = r, r_3.MaxY - r_3.MinY)) * factor;
    const center_1 = center;
    const sizeX_1 = sizeX;
    const sizeY_1 = sizeY;
    if (!(sizeX_1 >= 0)) {
        fail(`BRect.createFromCenter sizeX is negative: ${sizeX_1}, sizeY is: ${sizeY_1}, center: ${Pt__get_AsString(center_1)}`);
    }
    if (!(sizeY_1 >= 0)) {
        fail(`BRect.createFromCenter sizeY is negative: ${sizeY_1}, sizeX is: ${sizeX_1}, center: ${Pt__get_AsString(center_1)}`);
    }
    const minX = center_1.X - (sizeX_1 * 0.5);
    const minY = center_1.Y - (sizeY_1 * 0.5);
    const maxX = center_1.X + (sizeX_1 * 0.5);
    const maxY = center_1.Y + (sizeY_1 * 0.5);
    return BRect_$ctor_77D16AC0(minX, minY, maxX, maxY);
}

/**
 * Returns the 2D bounding rectangle expanded by a relative factor on all four sides, separately for X and Y.
 * Values between 0.0 and 1.0 shrink the rectangle.
 * Values larger than 1.0 expand the rectangle.
 * Does check for underflow if any factor is negative and raises EuclidException.
 */
export function BRect_expandRelXY(factorX, factorY, r) {
    let r_2, r_3;
    if (factorX < 0) {
        fail(`BRect.expandRelXY: a negative factorX ${factorX} is not allowed for expanding the 2D bounding rectangle ${BRect__get_AsString(r)}`);
    }
    if (factorY < 0) {
        fail(`BRect.expandRelXY: a negative factorY ${factorY} is not allowed for expanding the 2D bounding rectangle ${BRect__get_AsString(r)}`);
    }
    let center;
    const r_1 = r;
    center = Pt_$ctor_7B00E9A0((r_1.MaxX + r_1.MinX) * 0.5, (r_1.MaxY + r_1.MinY) * 0.5);
    const sizeX = ((r_2 = r, r_2.MaxX - r_2.MinX)) * factorX;
    const sizeY = ((r_3 = r, r_3.MaxY - r_3.MinY)) * factorY;
    const center_1 = center;
    const sizeX_1 = sizeX;
    const sizeY_1 = sizeY;
    if (!(sizeX_1 >= 0)) {
        fail(`BRect.createFromCenter sizeX is negative: ${sizeX_1}, sizeY is: ${sizeY_1}, center: ${Pt__get_AsString(center_1)}`);
    }
    if (!(sizeY_1 >= 0)) {
        fail(`BRect.createFromCenter sizeY is negative: ${sizeY_1}, sizeX is: ${sizeX_1}, center: ${Pt__get_AsString(center_1)}`);
    }
    const minX = center_1.X - (sizeX_1 * 0.5);
    const minY = center_1.Y - (sizeY_1 * 0.5);
    const maxX = center_1.X + (sizeX_1 * 0.5);
    const maxY = center_1.Y + (sizeY_1 * 0.5);
    return BRect_$ctor_77D16AC0(minX, minY, maxX, maxY);
}

/**
 * Returns the point (0) or minX, minY.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2 = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y      1
 * </code>
 */
export function BRect__get_Pt0(r) {
    return Pt_$ctor_7B00E9A0(r.MinX, r.MinY);
}

/**
 * Returns the point (1) or maxX, minY.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2 = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y      1
 * </code>
 */
export function BRect__get_Pt1(r) {
    return Pt_$ctor_7B00E9A0(r.MaxX, r.MinY);
}

/**
 * Returns the point (2) or maxX, maxY.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2 = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y      1
 * </code>
 */
export function BRect__get_Pt2(r) {
    return Pt_$ctor_7B00E9A0(r.MaxX, r.MaxY);
}

/**
 * Returns the point (3) or minX, maxY.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2 = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y      1
 * </code>
 */
export function BRect__get_Pt3(r) {
    return Pt_$ctor_7B00E9A0(r.MinX, r.MaxY);
}

/**
 * Returns the corners of this bounding rectangle in Counter-Clockwise order, starting at MinPt.
 * Returns an array of 4 Points.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2  = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y    1
 * </code>
 */
export function BRect__get_Points(r) {
    return [Pt_$ctor_7B00E9A0(r.MinX, r.MinY), Pt_$ctor_7B00E9A0(r.MaxX, r.MinY), Pt_$ctor_7B00E9A0(r.MaxX, r.MaxY), Pt_$ctor_7B00E9A0(r.MinX, r.MaxY)];
}

/**
 * Returns a Counter-Clockwise array of 5 Points, starting at MinPt.
 * Last and first point are the same.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2  = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y    1
 * </code>
 */
export function BRect__get_PointsLooped(r) {
    return [Pt_$ctor_7B00E9A0(r.MinX, r.MinY), Pt_$ctor_7B00E9A0(r.MaxX, r.MinY), Pt_$ctor_7B00E9A0(r.MaxX, r.MaxY), Pt_$ctor_7B00E9A0(r.MinX, r.MaxY), Pt_$ctor_7B00E9A0(r.MinX, r.MinY)];
}

/**
 * The bottom edge. The line from point 0 to 1.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2  = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y    1
 * </code>
 */
export function BRect__get_Edge01(r) {
    return Line2D_$ctor_77D16AC0(r.MinX, r.MinY, r.MaxX, r.MinY);
}

/**
 * The right edge. The line from point 1 to 2.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2  = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y    1
 * </code>
 */
export function BRect__get_Edge12(r) {
    return Line2D_$ctor_77D16AC0(r.MaxX, r.MinY, r.MaxX, r.MaxY);
}

/**
 * The top edge. The line from point 2 to 3.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2  = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y    1
 * </code>
 */
export function BRect__get_Edge23(r) {
    return Line2D_$ctor_77D16AC0(r.MaxX, r.MaxY, r.MinX, r.MaxY);
}

/**
 * The left edge. The line from point 3 to 0.
 * <code>
 * Y-Axis
 * ^
 * |
 * |             2  = max X,Y
 * 3 +------------+
 * |            |
 * |            |
 * |            |
 * |            |
 * |            |
 * +------------+-----> X-Axis
 * 0 = min X,Y    1
 * </code>
 */
export function BRect__get_Edge30(r) {
    return Line2D_$ctor_77D16AC0(r.MinX, r.MaxY, r.MinX, r.MinY);
}

