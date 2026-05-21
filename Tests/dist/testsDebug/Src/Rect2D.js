
import { float } from "./Format.js";
import { Pt_$ctor_7B00E9A0, Pt__get_AsFSharpCode, Pt__get_AsString } from "./Pt.js";
import { Vc_$ctor_7B00E9A0, Vc__get_AsFSharpCode, Vc__get_AsString } from "./Vc.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { failRect2DOffsetEdge, failDivide, failTooSmall, fail, fail1, fail2, failTooSmall2 } from "./EuclidErrors.js";
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { max, min } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { BRect_$ctor_77D16AC0 } from "./BRect.js";
import { Vc_$ctor_7B00E9A0 as Vc_$ctor_7B00E9A0_1 } from "./Vc.js";
import { UnitVc_$ctor_7B00E9A0 } from "./UnitVc.js";
import { failTooSmall as failTooSmall_1 } from "./EuclidErrors.js";
import { count } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { setItem, fill, item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { Line2D_$ctor_Z53905FA0 } from "./Line2D.js";
import { Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD } from "./TypeExtensions/Line2D.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Pt.js";

/**
 * A struct containing a 2D Origin point and two 2D Edge vectors,
 * representing an immutable 2D Rectangle with any rotation in 2D space.
 * This implementation guarantees the 2D Rectangle to be always valid.
 * That means the X and Y axes are always perpendicular to each other.
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
export class Rect2D extends Record {
    constructor(Origin, Xaxis, Yaxis) {
        super();
        this.Origin = Origin;
        this.Xaxis = Xaxis;
        this.Yaxis = Yaxis;
    }
    /**
     * Nicely formatted string representation of the 2D Rectangle including its size.
     */
    toString() {
        let v, x, y, v_1, x_1, y_1;
        const r = this;
        const sizeX = float((v = r.Xaxis, (x = v.X, (y = v.Y, Math.sqrt((x * x) + (y * y))))));
        const sizeY = float((v_1 = r.Yaxis, (x_1 = v_1.X, (y_1 = v_1.Y, Math.sqrt((x_1 * x_1) + (y_1 * y_1))))));
        const origin = Pt__get_AsString(r.Origin);
        const xaxis = Vc__get_AsString(r.Xaxis);
        const yaxis = Vc__get_AsString(r.Yaxis);
        return `Euclid.Rect2D ${sizeX} x ${sizeY} (Origin:${origin}| X-ax:${xaxis}|Y-ax:${yaxis})`;
    }
}

export function Rect2D_$reflection() {
    return class_type("Euclid.Rect2D", undefined, Rect2D, class_type("System.ValueType"));
}

/**
 * Unchecked Internal Constructor Only.
 * Create a 2D Rectangle from an origin point and X and Y axis vectors.
 */
export function Rect2D_$ctor_50BD42FD(origin, axisX, axisY) {
    let a, b, a_1, b_1;
    let lenX;
    const v = axisX;
    const x = v.X;
    const y = v.Y;
    lenX = Math.sqrt((x * x) + (y * y));
    let lenY;
    const v_1 = axisY;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    lenY = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    if (!(lenX > 1E-06)) {
        failTooSmall2("Rect2D() axisX", axisX, axisY);
    }
    if (!(lenY > 1E-06)) {
        failTooSmall2("Rect2D() axisY", axisY, axisX);
    }
    if (Math.abs((a = axisX, (b = axisY, (a.X * b.X) + (a.Y * b.Y)))) > ((lenX + lenY) * 1E-09)) {
        fail2("Rect2D(): X-axis and Y-axis are not perpendicular", axisX, axisY);
    }
    if (!(((a_1 = axisX, (b_1 = axisY, (a_1.X * b_1.Y) - (a_1.Y * b_1.X)))) >= 0)) {
        fail2("Rect2D(): X-axis and Y-axis are not counter-clockwise", axisX, axisY);
    }
    return new Rect2D(origin, axisX, axisY);
}

/**
 * Format the 2D Rectangle into string with nice floating point number formatting of X and Y size only.
 * But without type name as in v.ToString()
 */
export function Rect2D__get_AsString(r) {
    let v, x, y, v_1, x_1, y_1;
    const sizeX = float((v = r.Xaxis, (x = v.X, (y = v.Y, Math.sqrt((x * x) + (y * y))))));
    const sizeY = float((v_1 = r.Yaxis, (x_1 = v_1.X, (y_1 = v_1.Y, Math.sqrt((x_1 * x_1) + (y_1 * y_1))))));
    return concat(sizeX, " x ", ...sizeY);
}

/**
 * Format Rect2D into an F# code string that can be used to recreate the rectangle.
 */
export function Rect2D__get_AsFSharpCode(r) {
    return `Rect2D.createUnchecked(${Pt__get_AsFSharpCode(r.Origin)}, ${Vc__get_AsFSharpCode(r.Xaxis)}, ${Vc__get_AsFSharpCode(r.Yaxis)})`;
}

/**
 * Check for point containment in the 2D Rectangle.
 * By doing 4 dot products with the sides of the rectangle.
 * A point exactly on the edge of the Box is considered inside.
 */
export function Rect2D__Contains_6ADE94FD(r, p) {
    let a_1, b_1, a_2, b_2, a_3, a_4, b_4, b_3, a_5, a_6, b_6, b_5;
    const p0 = r.Origin;
    const x = r.Xaxis;
    const y = r.Yaxis;
    let v;
    const a = p;
    const b = p0;
    v = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    let p1;
    const p_1 = p0;
    const v_1 = x;
    p1 = Pt_$ctor_7B00E9A0(p_1.X + v_1.X, p_1.Y + v_1.Y);
    let p3;
    const p_2 = p0;
    const v_2 = y;
    p3 = Pt_$ctor_7B00E9A0(p_2.X + v_2.X, p_2.Y + v_2.Y);
    if (((((a_1 = v, (b_1 = x, (a_1.X * b_1.X) + (a_1.Y * b_1.Y)))) >= 0) && (((a_2 = v, (b_2 = y, (a_2.X * b_2.X) + (a_2.Y * b_2.Y)))) >= 0)) && (((a_3 = ((a_4 = p, (b_4 = p3, Vc_$ctor_7B00E9A0(a_4.X - b_4.X, a_4.Y - b_4.Y)))), (b_3 = y, (a_3.X * b_3.X) + (a_3.Y * b_3.Y)))) <= 0)) {
        return ((a_5 = ((a_6 = p, (b_6 = p1, Vc_$ctor_7B00E9A0(a_6.X - b_6.X, a_6.Y - b_6.Y)))), (b_5 = x, (a_5.X * b_5.X) + (a_5.Y * b_5.Y)))) <= 0;
    }
    else {
        return false;
    }
}

/**
 * Get the axis aligned 2D Bounding Rectangle of the 2D Rectangle.
 */
export function Rect2D__get_BRect(r) {
    const x = r.Xaxis;
    const y = r.Yaxis;
    const p0 = r.Origin;
    let p1;
    const p = p0;
    const v = x;
    p1 = Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y);
    let p2;
    const p_1 = p1;
    const v_1 = y;
    p2 = Pt_$ctor_7B00E9A0(p_1.X + v_1.X, p_1.Y + v_1.Y);
    let p3;
    const p_2 = p0;
    const v_2 = y;
    p3 = Pt_$ctor_7B00E9A0(p_2.X + v_2.X, p_2.Y + v_2.Y);
    const minX = min(min(min(p0.X, p1.X), p2.X), p3.X);
    const minY = min(min(min(p0.Y, p1.Y), p2.Y), p3.Y);
    const maxX = max(max(max(p0.X, p1.X), p2.X), p3.X);
    const maxY = max(max(max(p0.Y, p1.Y), p2.Y), p3.Y);
    return BRect_$ctor_77D16AC0(minX, minY, maxX, maxY);
}

/**
 * Create a 2D Rectangle from the origin point, an x-edge and an y-edge.
 * Fails if x and y are not in counter-clockwise order.
 * Fails if x and y are not perpendicularity.
 * Fails on vectors shorter than 1e-9.
 */
export function Rect2D_createFromVectors_50BD42FD(origin, x, y) {
    let v, x_1, y_1, v_1, x_3, y_2, a, b, a_1, b_1;
    if (!(((v = x, (x_1 = v.X, (y_1 = v.Y, (x_1 * x_1) + (y_1 * y_1))))) > 1E-12)) {
        failTooSmall2("Rect2D.createFromVectors x", x, y);
    }
    if (!(((v_1 = y, (x_3 = v_1.X, (y_2 = v_1.Y, (x_3 * x_3) + (y_2 * y_2))))) > 1E-12)) {
        failTooSmall2("Rect2D.createFromVectors y", y, x);
    }
    if (Math.abs((a = x, (b = y, (a.X * b.X) + (a.Y * b.Y)))) > 1E-10) {
        fail2("Rect2D.createFromVectors: X-axis and Y-axis are not perpendicular", x, y);
    }
    if (!(((a_1 = x, (b_1 = y, (a_1.X * b_1.Y) - (a_1.Y * b_1.X)))) >= 0)) {
        fail2("Rect2D.createFromVectors: X-axis and Y-axis are not counter-clockwise", x, y);
    }
    return Rect2D_$ctor_50BD42FD(origin, x, y);
}

/**
 * Creates a 2D rectangle from a origin point, the X vector and Y size.
 * Fails on negative Y size.
 */
export function Rect2D_createFromXVectorAndWidth_645BD0EA(origin, x, sizeY) {
    if (!(sizeY >= 0)) {
        fail1("Rect2D.createFromXVectorAndWidth(): sizeY cannot be negative", sizeY);
    }
    let y;
    let v_1;
    const v = x;
    v_1 = Vc_$ctor_7B00E9A0_1(-v.Y, v.X);
    const f = sizeY;
    y = Vc_$ctor_7B00E9A0(v_1.X * f, v_1.Y * f);
    return Rect2D_$ctor_50BD42FD(origin, x, y);
}

/**
 * Creates a 2D rectangle from an origin point, the X direction as unit-vector, the size in  X and Y direction.
 * Fails on negative sizes.
 */
export function Rect2D_createFromDirectionAndSizes_2A9F2837(origin, directionX, sizeX, sizeY) {
    if (!(sizeX >= 0)) {
        fail2("Rect2D.createFromDirectionAndSizes(): sizeX cannot be negative", sizeX, sizeY);
    }
    if (!(sizeY >= 0)) {
        fail2("Rect2D.createFromDirectionAndSizes(): sizeY cannot be negative", sizeY, sizeX);
    }
    let x_2;
    const a = directionX;
    const f = sizeX;
    x_2 = Vc_$ctor_7B00E9A0(a.X * f, a.Y * f);
    let y_1;
    let a_1;
    const v = directionX;
    a_1 = UnitVc_$ctor_7B00E9A0(-v.Y, v.X);
    const f_1 = sizeY;
    y_1 = Vc_$ctor_7B00E9A0(a_1.X * f_1, a_1.Y * f_1);
    return Rect2D_$ctor_50BD42FD(origin, x_2, y_1);
}

/**
 * Create a 2D Rectangle from a 2D line and a  right and left offset.
 * The left offset is in the direction of the future Y-axis.
 */
export function Rect2D_createFromLine_7E37C061(line, offRight, offLeft) {
    let ln_1, ln_2;
    if (-offRight >= offLeft) {
        fail(`Rect2D.createFromLine: flipped Rect2D : minus offRight ${offRight} must be smaller than offLeft ${offLeft}.`);
    }
    let x;
    const ln = line;
    x = Vc_$ctor_7B00E9A0((ln_1 = ln, ln_1.ToX - ln_1.FromX), (ln_2 = ln, ln_2.ToY - ln_2.FromY));
    let len;
    const v = x;
    const x_1 = v.X;
    const y = v.Y;
    len = Math.sqrt((x_1 * x_1) + (y * y));
    if (!(len > 1E-06)) {
        failTooSmall("Rect2D.createFromLine", line);
    }
    let y_1;
    const v_1 = x;
    y_1 = Vc_$ctor_7B00E9A0_1(-v_1.Y, v_1.X);
    let o;
    let a;
    const ln_3 = line;
    a = Pt_$ctor_7B00E9A0(ln_3.FromX, ln_3.FromY);
    let b;
    const v_2 = y_1;
    const f = offRight / len;
    b = Vc_$ctor_7B00E9A0(v_2.X * f, v_2.Y * f);
    o = Pt_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    let y_2;
    const v_3 = y_1;
    const f_1 = (offLeft + offRight) / len;
    y_2 = Vc_$ctor_7B00E9A0(v_3.X * f_1, v_3.Y * f_1);
    return Rect2D_$ctor_50BD42FD(o, x, y_2);
}

/**
 * Create a 2D Rectangle from an axis-aligned 2D Bounding Rectangle.
 */
export function Rect2D_createFromBRect_38AA877B(b) {
    let r, v, f, r_1, v_1, f_1, r_2;
    return Rect2D_$ctor_50BD42FD((r = b, Pt_$ctor_7B00E9A0(r.MinX, r.MinY)), (v = Vc_$ctor_7B00E9A0_1(1, 0), (f = ((r_1 = b, r_1.MaxX - r_1.MinX)), Vc_$ctor_7B00E9A0(v.X * f, v.Y * f))), (v_1 = Vc_$ctor_7B00E9A0_1(0, 1), (f_1 = ((r_2 = b, r_2.MaxY - r_2.MinY)), Vc_$ctor_7B00E9A0(v_1.X * f_1, v_1.Y * f_1))));
}

/**
 * Creates a 2D rectangle from a center point, the X direction, the X and the Y size.
 * Fails on negative sizes.
 */
export function Rect2D_createFromCenterAndDirection_2A9F2837(center, directionX, sizeX, sizeY) {
    let a_3, a_2, b, v_1, b_1, v_2;
    if (!(sizeX >= 0)) {
        fail(`Rect2D.createFromCenterAndDirection(center:Pt, directionX:UnitVc, sizeX, sizeY) sizeX cannot be negative: ${sizeX}, sizeY is: ${sizeY}, center: ${Pt__get_AsString(center)}`);
    }
    if (!(sizeY >= 0)) {
        fail(`Rect2D.createFromCenterAndDirection(center:Pt, directionX:UnitVc, sizeX, sizeY) sizeY cannot be negative: ${sizeY}, sizeX is: ${sizeX}, center: ${Pt__get_AsString(center)}`);
    }
    let x_2;
    const a = directionX;
    const f = sizeX;
    x_2 = Vc_$ctor_7B00E9A0(a.X * f, a.Y * f);
    let y_1;
    let a_1;
    const v = directionX;
    a_1 = UnitVc_$ctor_7B00E9A0(-v.Y, v.X);
    const f_1 = sizeY;
    y_1 = Vc_$ctor_7B00E9A0(a_1.X * f_1, a_1.Y * f_1);
    return Rect2D_$ctor_50BD42FD((a_3 = ((a_2 = center, (b = ((v_1 = x_2, Vc_$ctor_7B00E9A0(v_1.X * 0.5, v_1.Y * 0.5))), Pt_$ctor_7B00E9A0(a_2.X - b.X, a_2.Y - b.Y)))), (b_1 = ((v_2 = y_1, Vc_$ctor_7B00E9A0(v_2.X * 0.5, v_2.Y * 0.5))), Pt_$ctor_7B00E9A0(a_3.X - b_1.X, a_3.Y - b_1.Y))), x_2, y_1);
}

/**
 * Creates a 2D rectangle from a center point, the X vector and the Y size.
 * Fails on negative Y size.
 */
export function Rect2D_createFromCenterAndVector_645BD0EA(center, x, sizeY) {
    let a_1, a, b, v_2, b_1, v_3;
    if (!(sizeY >= 0)) {
        fail(`Rect2D.createFromCenterAndVector(center:Pt, x:Vc, sizeY) sizeY cannot be negative: ${sizeY}, x is: ${Vc__get_AsString(x)}, center: ${Pt__get_AsString(center)}`);
    }
    let y;
    let v_1;
    const v = x;
    v_1 = Vc_$ctor_7B00E9A0_1(-v.Y, v.X);
    const f = sizeY;
    y = Vc_$ctor_7B00E9A0(v_1.X * f, v_1.Y * f);
    return Rect2D_$ctor_50BD42FD((a_1 = ((a = center, (b = ((v_2 = x, Vc_$ctor_7B00E9A0(v_2.X * 0.5, v_2.Y * 0.5))), Pt_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y)))), (b_1 = ((v_3 = y, Vc_$ctor_7B00E9A0(v_3.X * 0.5, v_3.Y * 0.5))), Pt_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y))), x, y);
}

/**
 * Checks if two 2D Rectangles are equal within tolerance.
 * Does not recognize congruent rectangles with different rotation as equal.
 * Use a tolerance of 0.0 to check for an exact match of exactly equal rectangles.
 */
export function Rect2D_equals(tol, a, b) {
    if (((((Math.abs(a.Origin.X - b.Origin.X) <= tol) && (Math.abs(a.Origin.Y - b.Origin.Y) <= tol)) && (Math.abs(a.Xaxis.X - b.Xaxis.X) <= tol)) && (Math.abs(a.Xaxis.Y - b.Xaxis.Y) <= tol)) && (Math.abs(a.Yaxis.X - b.Yaxis.X) <= tol)) {
        return Math.abs(a.Yaxis.Y - b.Yaxis.Y) <= tol;
    }
    else {
        return false;
    }
}

/**
 * Check if two 2D Rectangles are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two rectangles are not exactly equal.
 */
export function Rect2D_notEquals(tol, a, b) {
    if (((((Math.abs(a.Origin.X - b.Origin.X) > tol) ? true : (Math.abs(a.Origin.Y - b.Origin.Y) > tol)) ? true : (Math.abs(a.Xaxis.X - b.Xaxis.X) > tol)) ? true : (Math.abs(a.Xaxis.Y - b.Xaxis.Y) > tol)) ? true : (Math.abs(a.Yaxis.X - b.Yaxis.X) > tol)) {
        return true;
    }
    else {
        return Math.abs(a.Yaxis.Y - b.Yaxis.Y) > tol;
    }
}

/**
 * Returns the 2D Rectangle expanded by distance on all four sides.
 * Does check for underflow if distance is negative and raises EuclidException.
 */
export function Rect2D_expand(dist, r) {
    let a_1, a, b, b_1, a_2, b_2, v_4, a_3, b_3, v_5;
    let siX;
    const v = r.Xaxis;
    const x = v.X;
    const y = v.Y;
    siX = Math.sqrt((x * x) + (y * y));
    let siY;
    const v_1 = r.Yaxis;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    siY = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    const d = dist * -2;
    if ((siX <= d) ? true : (siY <= d)) {
        fail(`Rect2D.expand: the 2D Rectangle ${Rect2D__get_AsString(r)} is too small to expand by negative distance ${dist}`);
    }
    let x_2;
    const v_2 = r.Xaxis;
    const f = dist / siX;
    x_2 = Vc_$ctor_7B00E9A0(v_2.X * f, v_2.Y * f);
    let y_2;
    const v_3 = r.Yaxis;
    const f_1 = dist / siY;
    y_2 = Vc_$ctor_7B00E9A0(v_3.X * f_1, v_3.Y * f_1);
    return Rect2D_$ctor_50BD42FD((a_1 = ((a = r.Origin, (b = x_2, Pt_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y)))), (b_1 = y_2, Pt_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y))), (a_2 = r.Xaxis, (b_2 = ((v_4 = x_2, Vc_$ctor_7B00E9A0(v_4.X * 2, v_4.Y * 2))), Vc_$ctor_7B00E9A0(a_2.X + b_2.X, a_2.Y + b_2.Y))), (a_3 = r.Yaxis, (b_3 = ((v_5 = y_2, Vc_$ctor_7B00E9A0(v_5.X * 2, v_5.Y * 2))), Vc_$ctor_7B00E9A0(a_3.X + b_3.X, a_3.Y + b_3.Y))));
}

/**
 * Returns the 2D Rectangle expanded by respective distances on all four sides.
 * Does check for overflow if distance is negative and fails.
 * distX, distY are for the local X and Y-axis respectively.
 */
export function Rect2D_expandXY(distX, distY, r) {
    let v_2, x_2, y_2, v_4, x_4, y_3, a_1, a, b, b_1, a_2, b_2, v_6, a_3, b_3, v_7;
    let siX;
    const v = r.Xaxis;
    const x = v.X;
    const y = v.Y;
    siX = Math.sqrt((x * x) + (y * y));
    let siY;
    const v_1 = r.Yaxis;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    siY = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    if (siX <= (distX * -2)) {
        fail(`Rect2D.expandXY: the 2D Rectangle ${Rect2D__get_AsString(r)} is too small to expand by negative distance distX ${distX}`);
    }
    if (siY <= (distY * -2)) {
        fail(`Rect2D.expandXY: the 2D Rectangle ${Rect2D__get_AsString(r)} is too small to expand by negative distance distY ${distY}`);
    }
    let x_3;
    const v_3 = r.Xaxis;
    const f = distX / ((v_2 = r.Xaxis, (x_2 = v_2.X, (y_2 = v_2.Y, Math.sqrt((x_2 * x_2) + (y_2 * y_2))))));
    x_3 = Vc_$ctor_7B00E9A0(v_3.X * f, v_3.Y * f);
    let y_4;
    const v_5 = r.Yaxis;
    const f_1 = distY / ((v_4 = r.Yaxis, (x_4 = v_4.X, (y_3 = v_4.Y, Math.sqrt((x_4 * x_4) + (y_3 * y_3))))));
    y_4 = Vc_$ctor_7B00E9A0(v_5.X * f_1, v_5.Y * f_1);
    return Rect2D_$ctor_50BD42FD((a_1 = ((a = r.Origin, (b = x_3, Pt_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y)))), (b_1 = y_4, Pt_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y))), (a_2 = r.Xaxis, (b_2 = ((v_6 = x_3, Vc_$ctor_7B00E9A0(v_6.X * 2, v_6.Y * 2))), Vc_$ctor_7B00E9A0(a_2.X + b_2.X, a_2.Y + b_2.Y))), (a_3 = r.Yaxis, (b_3 = ((v_7 = y_4, Vc_$ctor_7B00E9A0(v_7.X * 2, v_7.Y * 2))), Vc_$ctor_7B00E9A0(a_3.X + b_3.X, a_3.Y + b_3.Y))));
}

/**
 * Returns the 2D-rectangle expanded by a relative factor on all four sides.
 * Values between 0.0 and 1.0 shrink the rectangle.
 * Values larger than 1.0 expand the rectangle.
 * Does check for underflow if factor is negative and raises EuclidException.
 */
export function Rect2D_expandRel(factor, r) {
    let a_1, a, r_1, p_1, p, v_3, v_2, v_5, v_4, b, v_6, b_1, v_7;
    if (factor < 0) {
        fail(`Rect2D.expandRel: a negative factor ${factor} is not allowed for expanding the 2D-rectangle ${Rect2D__get_AsString(r)}`);
    }
    let x;
    const v = r.Xaxis;
    const f = factor;
    x = Vc_$ctor_7B00E9A0(v.X * f, v.Y * f);
    let y;
    const v_1 = r.Yaxis;
    const f_1 = factor;
    y = Vc_$ctor_7B00E9A0(v_1.X * f_1, v_1.Y * f_1);
    return Rect2D_$ctor_50BD42FD((a_1 = ((a = ((r_1 = r, (p_1 = ((p = r_1.Origin, (v_3 = ((v_2 = r_1.Xaxis, Vc_$ctor_7B00E9A0(v_2.X * 0.5, v_2.Y * 0.5))), Pt_$ctor_7B00E9A0(p.X + v_3.X, p.Y + v_3.Y)))), (v_5 = ((v_4 = r_1.Yaxis, Vc_$ctor_7B00E9A0(v_4.X * 0.5, v_4.Y * 0.5))), Pt_$ctor_7B00E9A0(p_1.X + v_5.X, p_1.Y + v_5.Y))))), (b = ((v_6 = x, Vc_$ctor_7B00E9A0(v_6.X * 0.5, v_6.Y * 0.5))), Pt_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y)))), (b_1 = ((v_7 = y, Vc_$ctor_7B00E9A0(v_7.X * 0.5, v_7.Y * 0.5))), Pt_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y))), x, y);
}

/**
 * Returns the 2D-rectangle expanded by a relative factor on all four sides.
 * Values between 0.0 and 1.0 shrink the rectangle.
 * Values larger than 1.0 expand the rectangle.
 * Does check for underflow if factor is negative and raises EuclidException.
 */
export function Rect2D_expandRelXY(factorX, factorY, r) {
    let a_1, a, r_1, p_1, p, v_3, v_2, v_5, v_4, b, v_6, b_1, v_7;
    if (factorX < 0) {
        fail(`Rect2D.expandRelXY: a negative factor ${factorX} is not allowed for expanding the 2D-rectangle ${Rect2D__get_AsString(r)}`);
    }
    if (factorY < 0) {
        fail(`Rect2D.expandRelXY: a negative factor ${factorY} is not allowed for expanding the 2D-rectangle ${Rect2D__get_AsString(r)}`);
    }
    let x;
    const v = r.Xaxis;
    const f = factorX;
    x = Vc_$ctor_7B00E9A0(v.X * f, v.Y * f);
    let y;
    const v_1 = r.Yaxis;
    const f_1 = factorY;
    y = Vc_$ctor_7B00E9A0(v_1.X * f_1, v_1.Y * f_1);
    return Rect2D_$ctor_50BD42FD((a_1 = ((a = ((r_1 = r, (p_1 = ((p = r_1.Origin, (v_3 = ((v_2 = r_1.Xaxis, Vc_$ctor_7B00E9A0(v_2.X * 0.5, v_2.Y * 0.5))), Pt_$ctor_7B00E9A0(p.X + v_3.X, p.Y + v_3.Y)))), (v_5 = ((v_4 = r_1.Yaxis, Vc_$ctor_7B00E9A0(v_4.X * 0.5, v_4.Y * 0.5))), Pt_$ctor_7B00E9A0(p_1.X + v_5.X, p_1.Y + v_5.Y))))), (b = ((v_6 = x, Vc_$ctor_7B00E9A0(v_6.X * 0.5, v_6.Y * 0.5))), Pt_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y)))), (b_1 = ((v_7 = y, Vc_$ctor_7B00E9A0(v_7.X * 0.5, v_7.Y * 0.5))), Pt_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y))), x, y);
}

/**
 * Creates a 2D rectangle from three points. Fails if points are too close to each other or all colinear.
 * The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
 * Origin and x-point define the X-axis orientation of the Rectangle.
 * The y-point only defines the length and side of the Y axis.
 * If the y-point is on the left side of the X-axis the origin will be at point 0, X at point 1.
 * If the y-point is on the right side of the X-axis, the X-axis will be reversed. the origin will be at point x, and the end of the x-Axis at the origin.
 * E.G if called with points (origin=3,x=2,y=0) the origin will be at 2, X at 3, and y at 1.
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
export function Rect2D_createFrom3Points_50BD389D(origin, xPt, yPt) {
    let v, x_1, y, v_1, x_3, y_2, v_4;
    let x;
    const a = xPt;
    const b = origin;
    x = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    if (!(((v = x, (x_1 = v.X, (y = v.Y, (x_1 * x_1) + (y * y))))) > 1E-12)) {
        fail(`Rect2D.createFrom3Points: X-Point ${Pt__get_AsString(xPt)} too close to origin: ${Vc__get_AsString(x)}.`);
    }
    let y_1;
    const a_1 = yPt;
    const b_1 = origin;
    y_1 = Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y);
    if (!(((v_1 = y_1, (x_3 = v_1.X, (y_2 = v_1.Y, (x_3 * x_3) + (y_2 * y_2))))) > 1E-12)) {
        fail(`Rect2D.createFrom3Points: Y-Point ${Pt__get_AsString(yPt)} too close to origin: ${Vc__get_AsString(y_1)}.`);
    }
    let yu;
    let v_3;
    const v_2 = x;
    v_3 = Vc_$ctor_7B00E9A0_1(-v_2.Y, v_2.X);
    const x_5 = v_3.X;
    const y_3 = v_3.Y;
    const l = Math.sqrt((x_5 * x_5) + (y_3 * y_3));
    if (!(l > 1E-12)) {
        failTooSmall_1("Vc.Unitized", v_3);
    }
    yu = UnitVc_$ctor_7B00E9A0(x_5 / l, y_3 / l);
    let dot;
    const a_2 = yu;
    const b_2 = y_1;
    dot = ((a_2.X * b_2.X) + (a_2.Y * b_2.Y));
    if (!(Math.abs(dot) > 1E-06)) {
        fail(concat("Rect2D.createFrom3Points: Y-Point ", Vc__get_AsString(y_1), ..." is too close to Xaxis."));
    }
    let yr;
    const a_3 = yu;
    const f = dot;
    yr = Vc_$ctor_7B00E9A0(a_3.X * f, a_3.Y * f);
    if (dot > 0) {
        return Rect2D_$ctor_50BD42FD(origin, x, yr);
    }
    else {
        return Rect2D_$ctor_50BD42FD(xPt, (v_4 = x, Vc_$ctor_7B00E9A0(-v_4.X, -v_4.Y)), yr);
    }
}

/**
 * Tries to create a 2D rectangle from three points. Returns None if points are too close to each other or all colinear.
 * The Origin, a point in X-axis direction and length, and a point for the length in Y-axis direction.
 * Origin and x-point define the X-axis orientation of the Rectangle.
 * The y-point only defines the length and side of the Y axis.
 * If the y-point is on the left side of the X-axis the origin will be at point 0, X at point 1.
 * If the y-point is on the right side of the X-axis, the X-axis will be reversed. the origin will be at point x, and the end of the x-Axis at the origin.
 * E.G if called with points (origin=3,x=2,y=0) the origin will be at 2, X at 3, and y at 1.
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
export function Rect2D_tryCreateFrom3Points_50BD389D(origin, xPt, yPt) {
    let v, x_1, y, v_1, x_3, y_2, v_4;
    let x;
    const a = xPt;
    const b = origin;
    x = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    if (!(((v = x, (x_1 = v.X, (y = v.Y, (x_1 * x_1) + (y * y))))) > 1E-12)) {
        return undefined;
    }
    else {
        let y_1;
        const a_1 = yPt;
        const b_1 = origin;
        y_1 = Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y);
        if (!(((v_1 = y_1, (x_3 = v_1.X, (y_2 = v_1.Y, (x_3 * x_3) + (y_2 * y_2))))) > 1E-12)) {
            return undefined;
        }
        else {
            let yu;
            let v_3;
            const v_2 = x;
            v_3 = Vc_$ctor_7B00E9A0_1(-v_2.Y, v_2.X);
            const x_5 = v_3.X;
            const y_3 = v_3.Y;
            const l = Math.sqrt((x_5 * x_5) + (y_3 * y_3));
            if (!(l > 1E-12)) {
                failTooSmall_1("Vc.Unitized", v_3);
            }
            yu = UnitVc_$ctor_7B00E9A0(x_5 / l, y_3 / l);
            let dot;
            const a_2 = yu;
            const b_2 = y_1;
            dot = ((a_2.X * b_2.X) + (a_2.Y * b_2.Y));
            if (!(Math.abs(dot) > 1E-06)) {
                return undefined;
            }
            else {
                let yr;
                const a_3 = yu;
                const f = dot;
                yr = Vc_$ctor_7B00E9A0(a_3.X * f, a_3.Y * f);
                if (dot > 0) {
                    return Rect2D_$ctor_50BD42FD(origin, x, yr);
                }
                else {
                    return Rect2D_$ctor_50BD42FD(xPt, (v_4 = x, Vc_$ctor_7B00E9A0(-v_4.X, -v_4.Y)), yr);
                }
            }
        }
    }
}

/**
 * Finds the oriented bounding rectangle of a set of points.
 * The orientation of the X Axis is defined by the direction vector.
 */
export function Rect2D_createFromDirAndPoints(dirX, pts) {
    let v, x, y, p, v_1, p_1, v_3, v_2, ln_1, ln_2, ln_4, ln_5, ln_7, ln_8, ln_10, ln_11, ln_13, ln_14, ln_16, ln_17;
    if (!(((v = dirX, (x = v.X, (y = v.Y, (x * x) + (y * y))))) > 1E-12)) {
        failTooSmall("Rect2D.createFromDirAndPoints dirX", dirX);
    }
    if (count(pts) < 2) {
        fail(`Rect2D.createFromDirAndPoints: cannot create a 2D rectangle from ${count(pts)} points`);
    }
    const p0 = item(0, pts);
    const x_2 = Line2D_$ctor_Z53905FA0(p0, (p = p0, (v_1 = dirX, Pt_$ctor_7B00E9A0(p.X + v_1.X, p.Y + v_1.Y))));
    const y_1 = Line2D_$ctor_Z53905FA0(p0, (p_1 = p0, (v_3 = ((v_2 = dirX, Vc_$ctor_7B00E9A0_1(-v_2.Y, v_2.X))), Pt_$ctor_7B00E9A0(p_1.X + v_3.X, p_1.Y + v_3.Y))));
    let minX = 1.7976931348623157E+308;
    let minY = 1.7976931348623157E+308;
    let maxX = -1.7976931348623157E+308;
    let maxY = -1.7976931348623157E+308;
    for (let i = 0; i <= (count(pts) - 1); i++) {
        const p_2 = item(i, pts);
        const px = Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD(x_2, p_2);
        if (px < minX) {
            minX = px;
        }
        if (px > maxX) {
            maxX = px;
        }
        const py = Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD(y_1, p_2);
        if (py < minY) {
            minY = py;
        }
        if (py > maxY) {
            maxY = py;
        }
    }
    let vx;
    let a;
    const ln = x_2;
    const p_3 = maxX;
    a = Pt_$ctor_7B00E9A0_1(ln.FromX + (((ln_1 = ln, ln_1.ToX - ln_1.FromX)) * p_3), ln.FromY + (((ln_2 = ln, ln_2.ToY - ln_2.FromY)) * p_3));
    let b;
    const ln_3 = x_2;
    const p_4 = minX;
    b = Pt_$ctor_7B00E9A0_1(ln_3.FromX + (((ln_4 = ln_3, ln_4.ToX - ln_4.FromX)) * p_4), ln_3.FromY + (((ln_5 = ln_3, ln_5.ToY - ln_5.FromY)) * p_4));
    vx = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
    let vy;
    let a_1;
    const ln_6 = y_1;
    const p_5 = maxY;
    a_1 = Pt_$ctor_7B00E9A0_1(ln_6.FromX + (((ln_7 = ln_6, ln_7.ToX - ln_7.FromX)) * p_5), ln_6.FromY + (((ln_8 = ln_6, ln_8.ToY - ln_8.FromY)) * p_5));
    let b_1;
    const ln_9 = y_1;
    const p_6 = minY;
    b_1 = Pt_$ctor_7B00E9A0_1(ln_9.FromX + (((ln_10 = ln_9, ln_10.ToX - ln_10.FromX)) * p_6), ln_9.FromY + (((ln_11 = ln_9, ln_11.ToY - ln_11.FromY)) * p_6));
    vy = Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y);
    let o;
    let p_8;
    const p_7 = p0;
    let v_5;
    let v_4;
    const ln_12 = x_2;
    v_4 = Vc_$ctor_7B00E9A0((ln_13 = ln_12, ln_13.ToX - ln_13.FromX), (ln_14 = ln_12, ln_14.ToY - ln_14.FromY));
    const f = minX;
    v_5 = Vc_$ctor_7B00E9A0(v_4.X * f, v_4.Y * f);
    p_8 = Pt_$ctor_7B00E9A0(p_7.X + v_5.X, p_7.Y + v_5.Y);
    let v_7;
    let v_6;
    const ln_15 = y_1;
    v_6 = Vc_$ctor_7B00E9A0((ln_16 = ln_15, ln_16.ToX - ln_16.FromX), (ln_17 = ln_15, ln_17.ToY - ln_17.FromY));
    const f_1 = minY;
    v_7 = Vc_$ctor_7B00E9A0(v_6.X * f_1, v_6.Y * f_1);
    o = Pt_$ctor_7B00E9A0(p_8.X + v_7.X, p_8.Y + v_7.Y);
    return Rect2D_$ctor_50BD42FD(o, vx, vy);
}

/**
 * Creates a new 2D rectangle( = oriented bounding rectangle ) to contain the projections of all given points.
 * But not the corners of the reference rectangle.
 * Keeps the same X- and Y-axis orientation as the input rectangle.
 */
export function Rect2D_fitToPoints(pts, refRect) {
    let a_5, f_2, a_6, f_3;
    const o = refRect.Origin;
    let x_3;
    const v = refRect.Xaxis;
    const x = v.X;
    const y = v.Y;
    const l = Math.sqrt((x * x) + (y * y));
    if (!(l > 1E-12)) {
        failTooSmall_1("Vc.Unitized", v);
    }
    x_3 = UnitVc_$ctor_7B00E9A0(x / l, y / l);
    let y_4;
    const v_1 = refRect.Yaxis;
    const x_4 = v_1.X;
    const y_2 = v_1.Y;
    const l_1 = Math.sqrt((x_4 * x_4) + (y_2 * y_2));
    if (!(l_1 > 1E-12)) {
        failTooSmall_1("Vc.Unitized", v_1);
    }
    y_4 = UnitVc_$ctor_7B00E9A0(x_4 / l_1, y_2 / l_1);
    let minX = 1.7976931348623157E+308;
    let minY = 1.7976931348623157E+308;
    let maxX = -1.7976931348623157E+308;
    let maxY = -1.7976931348623157E+308;
    for (let i = 0; i <= (count(pts) - 1); i++) {
        let v_2;
        const a = item(i, pts);
        const b = o;
        v_2 = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
        let dotX;
        const a_1 = v_2;
        const b_1 = x_3;
        dotX = ((a_1.X * b_1.X) + (a_1.Y * b_1.Y));
        minX = min(minX, dotX);
        maxX = max(maxX, dotX);
        let dotY;
        const a_2 = v_2;
        const b_2 = y_4;
        dotY = ((a_2.X * b_2.X) + (a_2.Y * b_2.Y));
        minY = min(minY, dotY);
        maxY = max(maxY, dotY);
    }
    let bo;
    let p_1;
    const p = o;
    let v_3;
    const a_3 = x_3;
    const f = minX;
    v_3 = Vc_$ctor_7B00E9A0(a_3.X * f, a_3.Y * f);
    p_1 = Pt_$ctor_7B00E9A0(p.X + v_3.X, p.Y + v_3.Y);
    let v_4;
    const a_4 = y_4;
    const f_1 = minY;
    v_4 = Vc_$ctor_7B00E9A0(a_4.X * f_1, a_4.Y * f_1);
    bo = Pt_$ctor_7B00E9A0(p_1.X + v_4.X, p_1.Y + v_4.Y);
    const sizeX = maxX - minX;
    const sizeY = maxY - minY;
    return Rect2D_$ctor_50BD42FD(bo, (a_5 = x_3, (f_2 = sizeX, Vc_$ctor_7B00E9A0(a_5.X * f_2, a_5.Y * f_2))), (a_6 = y_4, (f_3 = sizeY, Vc_$ctor_7B00E9A0(a_6.X * f_3, a_6.Y * f_3))));
}

/**
 * Translate along the local X-axis of the 2D Rectangle.
 */
export function Rect2D_translateLocalX(distX, r) {
    let p, v_2, v_1, f;
    const x = r.Xaxis;
    let len;
    const v = x;
    const x_1 = v.X;
    const y = v.Y;
    len = Math.sqrt((x_1 * x_1) + (y * y));
    if (!(len > 1E-12)) {
        failTooSmall("Rect2D.translateLocalX: Xaxis", r);
    }
    return Rect2D_$ctor_50BD42FD((p = r.Origin, (v_2 = ((v_1 = x, (f = (distX / len), Vc_$ctor_7B00E9A0(v_1.X * f, v_1.Y * f)))), Pt_$ctor_7B00E9A0(p.X + v_2.X, p.Y + v_2.Y))), x, r.Yaxis);
}

/**
 * Translate along the local Y-axis of the 2D Rectangle.
 */
export function Rect2D_translateLocalY(distY, r) {
    let p, v_2, v_1, f;
    const y = r.Yaxis;
    let len;
    const v = y;
    const x = v.X;
    const y_1 = v.Y;
    len = Math.sqrt((x * x) + (y_1 * y_1));
    if (!(len > 1E-12)) {
        failTooSmall("Rect2D.translateLocalY: Yaxis", r);
    }
    return Rect2D_$ctor_50BD42FD((p = r.Origin, (v_2 = ((v_1 = y, (f = (distY / len), Vc_$ctor_7B00E9A0(v_1.X * f, v_1.Y * f)))), Pt_$ctor_7B00E9A0(p.X + v_2.X, p.Y + v_2.Y))), r.Xaxis, y);
}

/**
 * Translate by a 2D vector.(Same as Rect2D.move)
 */
export function Rect2D_translate(v, r) {
    let p, v_1;
    return Rect2D_$ctor_50BD42FD((p = r.Origin, (v_1 = v, Pt_$ctor_7B00E9A0(p.X + v_1.X, p.Y + v_1.Y))), r.Xaxis, r.Yaxis);
}

/**
 * Translate by a 2D vector.(Same as Rect2D.translate)
 */
export function Rect2D_move(v, r) {
    let p, v_1;
    return Rect2D_$ctor_50BD42FD((p = r.Origin, (v_1 = v, Pt_$ctor_7B00E9A0(p.X + v_1.X, p.Y + v_1.Y))), r.Xaxis, r.Yaxis);
}

/**
 * Rotation of a Rect2D.
 */
export function Rect2D_rotate(rot, rect) {
    let r_1, p_1, r_3, v_1, r_5, v_3;
    return Rect2D_$ctor_50BD42FD((r_1 = rot, (p_1 = rect.Origin, Pt_$ctor_7B00E9A0_1((r_1.Cos * p_1.X) - (r_1.Sin * p_1.Y), (r_1.Sin * p_1.X) + (r_1.Cos * p_1.Y)))), (r_3 = rot, (v_1 = rect.Xaxis, Vc_$ctor_7B00E9A0_1((r_3.Cos * v_1.X) - (r_3.Sin * v_1.Y), (r_3.Sin * v_1.X) + (r_3.Cos * v_1.Y)))), (r_5 = rot, (v_3 = rect.Yaxis, Vc_$ctor_7B00E9A0_1((r_5.Cos * v_3.X) - (r_5.Sin * v_3.Y), (r_5.Sin * v_3.X) + (r_5.Cos * v_3.Y)))));
}

/**
 * Rotation of a Rect2D. around a given Center.
 */
export function Rect2D_rotateWithCenter(cen, rot, rect) {
    let cen_2, r_1, pt_1, x, y, r_3, v_1, r_5, v_3;
    return Rect2D_$ctor_50BD42FD((cen_2 = cen, (r_1 = rot, (pt_1 = rect.Origin, (x = (pt_1.X - cen_2.X), (y = (pt_1.Y - cen_2.Y), Pt_$ctor_7B00E9A0_1(((r_1.Cos * x) - (r_1.Sin * y)) + cen_2.X, ((r_1.Sin * x) + (r_1.Cos * y)) + cen_2.Y)))))), (r_3 = rot, (v_1 = rect.Xaxis, Vc_$ctor_7B00E9A0_1((r_3.Cos * v_1.X) - (r_3.Sin * v_1.Y), (r_3.Sin * v_1.X) + (r_3.Cos * v_1.Y)))), (r_5 = rot, (v_3 = rect.Yaxis, Vc_$ctor_7B00E9A0_1((r_5.Cos * v_3.X) - (r_5.Sin * v_3.Y), (r_5.Sin * v_3.X) + (r_5.Cos * v_3.Y)))));
}

/**
 * Offset a Rect2D inwards by a given distance.
 * A negative distance will offset outwards.
 * Fails if the distance is larger than half the size of the rectangle.
 */
export function Rect2D_offset(dist, rect) {
    let p_1, p, v_4, v_5, a, b, v_6, a_1, b_1, v_7;
    let xl;
    const v = rect.Xaxis;
    const x = v.X;
    const y = v.Y;
    xl = Math.sqrt((x * x) + (y * y));
    let yl;
    const v_1 = rect.Yaxis;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    yl = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    if ((xl < (dist * 2)) ? true : (yl < (dist * 2))) {
        fail(concat("Rect2D.offset: the 2D Rectangle ", Rect2D__get_AsString(rect), " is too small to offset by distance ", ...float(dist)));
    }
    let x_2;
    const v_2 = rect.Xaxis;
    const f = dist / xl;
    x_2 = Vc_$ctor_7B00E9A0(v_2.X * f, v_2.Y * f);
    let y_2;
    const v_3 = rect.Yaxis;
    const f_1 = dist / yl;
    y_2 = Vc_$ctor_7B00E9A0(v_3.X * f_1, v_3.Y * f_1);
    return Rect2D_$ctor_50BD42FD((p_1 = ((p = rect.Origin, (v_4 = x_2, Pt_$ctor_7B00E9A0(p.X + v_4.X, p.Y + v_4.Y)))), (v_5 = y_2, Pt_$ctor_7B00E9A0(p_1.X + v_5.X, p_1.Y + v_5.Y))), (a = rect.Xaxis, (b = ((v_6 = x_2, Vc_$ctor_7B00E9A0(v_6.X * 2, v_6.Y * 2))), Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y))), (a_1 = rect.Yaxis, (b_1 = ((v_7 = y_2, Vc_$ctor_7B00E9A0(v_7.X * 2, v_7.Y * 2))), Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y))));
}

/**
 * Offset a Rect2D inwards by four distances.
 * Negative distances will offset outwards.
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
export function Rect2D_offsetVar(dist, rect) {
    let p_1, p, v_6, v_7, a_1, a, b, b_1, a_3, a_2, b_2, b_3;
    if (dist.length !== 4) {
        fail(`Rect2D.offsetVar: the distance array must have 4 elements, but has ${dist.length}`);
    }
    let xl;
    const v = rect.Xaxis;
    const x = v.X;
    const y = v.Y;
    xl = Math.sqrt((x * x) + (y * y));
    let yl;
    const v_1 = rect.Yaxis;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    yl = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    if ((xl < (item(1, dist) + item(3, dist))) ? true : (yl < (item(0, dist) + item(2, dist)))) {
        fail(`Rect2D.offsetVar: the 2D Rectangle ${Rect2D__get_AsString(rect)} is too small to offset by distances [|${float(item(0, dist))};${float(item(1, dist))};${float(item(2, dist))};${float(item(3, dist))}|]`);
    }
    let x0;
    const v_2 = rect.Xaxis;
    const f = item(3, dist) / xl;
    x0 = Vc_$ctor_7B00E9A0(v_2.X * f, v_2.Y * f);
    let x1;
    const v_3 = rect.Xaxis;
    const f_1 = item(1, dist) / xl;
    x1 = Vc_$ctor_7B00E9A0(v_3.X * f_1, v_3.Y * f_1);
    let y0;
    const v_4 = rect.Yaxis;
    const f_2 = item(0, dist) / yl;
    y0 = Vc_$ctor_7B00E9A0(v_4.X * f_2, v_4.Y * f_2);
    let y1;
    const v_5 = rect.Yaxis;
    const f_3 = item(2, dist) / yl;
    y1 = Vc_$ctor_7B00E9A0(v_5.X * f_3, v_5.Y * f_3);
    return Rect2D_$ctor_50BD42FD((p_1 = ((p = rect.Origin, (v_6 = x0, Pt_$ctor_7B00E9A0(p.X + v_6.X, p.Y + v_6.Y)))), (v_7 = y0, Pt_$ctor_7B00E9A0(p_1.X + v_7.X, p_1.Y + v_7.Y))), (a_1 = ((a = rect.Xaxis, (b = x0, Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y)))), (b_1 = x1, Vc_$ctor_7B00E9A0(a_1.X - b_1.X, a_1.Y - b_1.Y))), (a_3 = ((a_2 = rect.Yaxis, (b_2 = y0, Vc_$ctor_7B00E9A0(a_2.X - b_2.X, a_2.Y - b_2.Y)))), (b_3 = y1, Vc_$ctor_7B00E9A0(a_3.X - b_3.X, a_3.Y - b_3.Y))));
}

/**
 * Offsets a local Rect2D at one of the four corners.
 */
export function Rect2D_offsetCorner_Z3166AD2D(rect, corner, xOffset, yOffset, xWidth, yHeight) {
    let p_1, p, v_8, v_9, p_3, p_2, v_14, v_15, p_5, p_4, v_20, v_21, p_7, p_6, v_26, v_27;
    const xa = rect.Xaxis;
    const ya = rect.Yaxis;
    let xl;
    const v = xa;
    const x = v.X;
    const y = v.Y;
    xl = Math.sqrt((x * x) + (y * y));
    let yl;
    const v_1 = ya;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    yl = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    if (!(xl > 1E-12)) {
        failTooSmall("Rect2D.offsetCorner: Xaxis", rect);
    }
    if (!(yl > 1E-12)) {
        failTooSmall("Rect2D.offsetCorner: Yaxis", rect);
    }
    let xv;
    const v_2 = xa;
    const f = xWidth / xl;
    xv = Vc_$ctor_7B00E9A0(v_2.X * f, v_2.Y * f);
    let yv;
    const v_3 = ya;
    const f_1 = yHeight / yl;
    yv = Vc_$ctor_7B00E9A0(v_3.X * f_1, v_3.Y * f_1);
    switch (corner) {
        case 0: {
            let x_5;
            let v_5;
            const v_4 = xa;
            const f_2 = xOffset;
            v_5 = Vc_$ctor_7B00E9A0(v_4.X * f_2, v_4.Y * f_2);
            const f_3 = xl;
            if (!(Math.abs(f_3) > 1E-12)) {
                failDivide("\'/\' operator", f_3, v_5);
            }
            x_5 = Vc_$ctor_7B00E9A0(v_5.X / f_3, v_5.Y / f_3);
            let y_2;
            let v_7;
            const v_6 = ya;
            const f_4 = yOffset;
            v_7 = Vc_$ctor_7B00E9A0(v_6.X * f_4, v_6.Y * f_4);
            const f_5 = yl;
            if (!(Math.abs(f_5) > 1E-12)) {
                failDivide("\'/\' operator", f_5, v_7);
            }
            y_2 = Vc_$ctor_7B00E9A0(v_7.X / f_5, v_7.Y / f_5);
            return Rect2D_$ctor_50BD42FD((p_1 = ((p = rect.Origin, (v_8 = x_5, Pt_$ctor_7B00E9A0(p.X + v_8.X, p.Y + v_8.Y)))), (v_9 = y_2, Pt_$ctor_7B00E9A0(p_1.X + v_9.X, p_1.Y + v_9.Y))), xv, yv);
        }
        case 1: {
            let x_9;
            let v_11;
            const v_10 = xa;
            const f_6 = (xl - xOffset) - xWidth;
            v_11 = Vc_$ctor_7B00E9A0(v_10.X * f_6, v_10.Y * f_6);
            const f_7 = xl;
            if (!(Math.abs(f_7) > 1E-12)) {
                failDivide("\'/\' operator", f_7, v_11);
            }
            x_9 = Vc_$ctor_7B00E9A0(v_11.X / f_7, v_11.Y / f_7);
            let y_4;
            let v_13;
            const v_12 = ya;
            const f_8 = yOffset;
            v_13 = Vc_$ctor_7B00E9A0(v_12.X * f_8, v_12.Y * f_8);
            const f_9 = yl;
            if (!(Math.abs(f_9) > 1E-12)) {
                failDivide("\'/\' operator", f_9, v_13);
            }
            y_4 = Vc_$ctor_7B00E9A0(v_13.X / f_9, v_13.Y / f_9);
            return Rect2D_$ctor_50BD42FD((p_3 = ((p_2 = rect.Origin, (v_14 = x_9, Pt_$ctor_7B00E9A0(p_2.X + v_14.X, p_2.Y + v_14.Y)))), (v_15 = y_4, Pt_$ctor_7B00E9A0(p_3.X + v_15.X, p_3.Y + v_15.Y))), xv, yv);
        }
        case 2: {
            let x_13;
            let v_17;
            const v_16 = xa;
            const f_10 = (xl - xOffset) - xWidth;
            v_17 = Vc_$ctor_7B00E9A0(v_16.X * f_10, v_16.Y * f_10);
            const f_11 = xl;
            if (!(Math.abs(f_11) > 1E-12)) {
                failDivide("\'/\' operator", f_11, v_17);
            }
            x_13 = Vc_$ctor_7B00E9A0(v_17.X / f_11, v_17.Y / f_11);
            let y_6;
            let v_19;
            const v_18 = ya;
            const f_12 = (yl - yOffset) - yHeight;
            v_19 = Vc_$ctor_7B00E9A0(v_18.X * f_12, v_18.Y * f_12);
            const f_13 = yl;
            if (!(Math.abs(f_13) > 1E-12)) {
                failDivide("\'/\' operator", f_13, v_19);
            }
            y_6 = Vc_$ctor_7B00E9A0(v_19.X / f_13, v_19.Y / f_13);
            return Rect2D_$ctor_50BD42FD((p_5 = ((p_4 = rect.Origin, (v_20 = x_13, Pt_$ctor_7B00E9A0(p_4.X + v_20.X, p_4.Y + v_20.Y)))), (v_21 = y_6, Pt_$ctor_7B00E9A0(p_5.X + v_21.X, p_5.Y + v_21.Y))), xv, yv);
        }
        case 3: {
            let x_17;
            let v_23;
            const v_22 = xa;
            const f_14 = xOffset;
            v_23 = Vc_$ctor_7B00E9A0(v_22.X * f_14, v_22.Y * f_14);
            const f_15 = xl;
            if (!(Math.abs(f_15) > 1E-12)) {
                failDivide("\'/\' operator", f_15, v_23);
            }
            x_17 = Vc_$ctor_7B00E9A0(v_23.X / f_15, v_23.Y / f_15);
            let y_8;
            let v_25;
            const v_24 = ya;
            const f_16 = (yl - yOffset) - yHeight;
            v_25 = Vc_$ctor_7B00E9A0(v_24.X * f_16, v_24.Y * f_16);
            const f_17 = yl;
            if (!(Math.abs(f_17) > 1E-12)) {
                failDivide("\'/\' operator", f_17, v_25);
            }
            y_8 = Vc_$ctor_7B00E9A0(v_25.X / f_17, v_25.Y / f_17);
            return Rect2D_$ctor_50BD42FD((p_7 = ((p_6 = rect.Origin, (v_26 = x_17, Pt_$ctor_7B00E9A0(p_6.X + v_26.X, p_6.Y + v_26.Y)))), (v_27 = y_8, Pt_$ctor_7B00E9A0(p_7.X + v_27.X, p_7.Y + v_27.Y))), xv, yv);
        }
        default:
            return fail(`Rect2D.offsetCorner: corner ${corner} out of range 0..3`);
    }
}

/**
 * Offsets a local Rect2D at one of the four edges.
 */
export function Rect2D_offsetEdge_Z3166AD2D(rect, edgeIdx, offEdge, width, offStart, offEnd) {
    let p_1, p, v_3, v_2, f, v_5, v_4, f_1, v_6, f_2, v_7, f_3, p_3, p_2, v_9, v_8, f_4, v_11, v_10, f_5, v_12, f_6, v_13, f_7, yy_2, p_5, p_4, v_15, v_14, f_8, v_17, v_16, f_9, v_18, f_10, v_19, f_11, p_7, p_6, v_21, v_20, f_12, v_23, v_22, f_13, v_24, f_14, v_25, f_15, yy_4, p_9, p_8, v_27, v_26, f_16, v_29, v_28, f_17, v_30, f_18, v_31, f_19, p_11, p_10, v_33, v_32, f_20, v_35, v_34, f_21, v_36, f_22, v_37, f_23, yy_6, p_13, p_12, v_39, v_38, f_24, v_41, v_40, f_25, v_42, f_26, v_43, f_27, p_15, p_14, v_45, v_44, f_28, v_47, v_46, f_29, v_48, f_30, v_49, f_31;
    const x = rect.Xaxis;
    const y = rect.Yaxis;
    let lx;
    const v = x;
    const x_1 = v.X;
    const y_1 = v.Y;
    lx = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    let ly;
    const v_1 = y;
    const x_2 = v_1.X;
    const y_2 = v_1.Y;
    ly = Math.sqrt((x_2 * x_2) + (y_2 * y_2));
    if (!(lx > 1E-12)) {
        failTooSmall("Rect2D.offsetEdge: Xaxis", rect);
    }
    if (!(ly > 1E-12)) {
        failTooSmall("Rect2D.offsetEdge: Yaxis", rect);
    }
    if (width > 1E-06) {
        switch (edgeIdx) {
            case 0: {
                let x_5;
                const d = (lx - offStart) - offEnd;
                x_5 = ((d > 1E-06) ? d : failRect2DOffsetEdge(offStart, offEnd, lx, edgeIdx, d));
                return Rect2D_$ctor_50BD42FD((p_1 = ((p = rect.Origin, (v_3 = ((v_2 = x, (f = (offStart / lx), Vc_$ctor_7B00E9A0(v_2.X * f, v_2.Y * f)))), Pt_$ctor_7B00E9A0(p.X + v_3.X, p.Y + v_3.Y)))), (v_5 = ((v_4 = y, (f_1 = (offEdge / ly), Vc_$ctor_7B00E9A0(v_4.X * f_1, v_4.Y * f_1)))), Pt_$ctor_7B00E9A0(p_1.X + v_5.X, p_1.Y + v_5.Y))), (v_6 = x, (f_2 = (x_5 / lx), Vc_$ctor_7B00E9A0(v_6.X * f_2, v_6.Y * f_2))), (v_7 = y, (f_3 = (width / ly), Vc_$ctor_7B00E9A0(v_7.X * f_3, v_7.Y * f_3))));
            }
            case 1: {
                let y_4;
                const d_5 = (ly - offStart) - offEnd;
                y_4 = ((d_5 > 1E-06) ? d_5 : failRect2DOffsetEdge(offStart, offEnd, ly, edgeIdx, d_5));
                return Rect2D_$ctor_50BD42FD((p_3 = ((p_2 = rect.Origin, (v_9 = ((v_8 = x, (f_4 = (((lx - offEdge) - width) / lx), Vc_$ctor_7B00E9A0(v_8.X * f_4, v_8.Y * f_4)))), Pt_$ctor_7B00E9A0(p_2.X + v_9.X, p_2.Y + v_9.Y)))), (v_11 = ((v_10 = y, (f_5 = (offStart / ly), Vc_$ctor_7B00E9A0(v_10.X * f_5, v_10.Y * f_5)))), Pt_$ctor_7B00E9A0(p_3.X + v_11.X, p_3.Y + v_11.Y))), (v_12 = x, (f_6 = (width / lx), Vc_$ctor_7B00E9A0(v_12.X * f_6, v_12.Y * f_6))), (v_13 = y, (f_7 = (y_4 / ly), Vc_$ctor_7B00E9A0(v_13.X * f_7, v_13.Y * f_7))));
            }
            case 2: {
                let x_8;
                const d_10 = (lx - offStart) - offEnd;
                x_8 = ((d_10 > 1E-06) ? d_10 : failRect2DOffsetEdge(offStart, offEnd, lx, edgeIdx, d_10));
                return Rect2D_$ctor_50BD42FD((yy_2 = ((ly - offEdge) - width), (p_5 = ((p_4 = rect.Origin, (v_15 = ((v_14 = x, (f_8 = (offEnd / lx), Vc_$ctor_7B00E9A0(v_14.X * f_8, v_14.Y * f_8)))), Pt_$ctor_7B00E9A0(p_4.X + v_15.X, p_4.Y + v_15.Y)))), (v_17 = ((v_16 = y, (f_9 = (yy_2 / ly), Vc_$ctor_7B00E9A0(v_16.X * f_9, v_16.Y * f_9)))), Pt_$ctor_7B00E9A0(p_5.X + v_17.X, p_5.Y + v_17.Y)))), (v_18 = x, (f_10 = (x_8 / lx), Vc_$ctor_7B00E9A0(v_18.X * f_10, v_18.Y * f_10))), (v_19 = y, (f_11 = (width / ly), Vc_$ctor_7B00E9A0(v_19.X * f_11, v_19.Y * f_11))));
            }
            case 3: {
                let y_7;
                const d_15 = (ly - offStart) - offEnd;
                y_7 = ((d_15 > 1E-06) ? d_15 : failRect2DOffsetEdge(offStart, offEnd, ly, edgeIdx, d_15));
                return Rect2D_$ctor_50BD42FD((p_7 = ((p_6 = rect.Origin, (v_21 = ((v_20 = x, (f_12 = (offEdge / lx), Vc_$ctor_7B00E9A0(v_20.X * f_12, v_20.Y * f_12)))), Pt_$ctor_7B00E9A0(p_6.X + v_21.X, p_6.Y + v_21.Y)))), (v_23 = ((v_22 = y, (f_13 = (offEnd / ly), Vc_$ctor_7B00E9A0(v_22.X * f_13, v_22.Y * f_13)))), Pt_$ctor_7B00E9A0(p_7.X + v_23.X, p_7.Y + v_23.Y))), (v_24 = x, (f_14 = (width / lx), Vc_$ctor_7B00E9A0(v_24.X * f_14, v_24.Y * f_14))), (v_25 = y, (f_15 = (y_7 / ly), Vc_$ctor_7B00E9A0(v_25.X * f_15, v_25.Y * f_15))));
            }
            default:
                return fail(`Rect2D.offsetEdge: edgeIdx ${edgeIdx} out of range 0..3`);
        }
    }
    else if (width < -1E-06) {
        switch (edgeIdx) {
            case 0: {
                let x_11;
                const d_20 = (lx - offStart) - offEnd;
                x_11 = ((d_20 > 1E-06) ? d_20 : failRect2DOffsetEdge(offStart, offEnd, lx, edgeIdx, d_20));
                return Rect2D_$ctor_50BD42FD((yy_4 = (offEdge + width), (p_9 = ((p_8 = rect.Origin, (v_27 = ((v_26 = x, (f_16 = (offStart / lx), Vc_$ctor_7B00E9A0(v_26.X * f_16, v_26.Y * f_16)))), Pt_$ctor_7B00E9A0(p_8.X + v_27.X, p_8.Y + v_27.Y)))), (v_29 = ((v_28 = y, (f_17 = (yy_4 / ly), Vc_$ctor_7B00E9A0(v_28.X * f_17, v_28.Y * f_17)))), Pt_$ctor_7B00E9A0(p_9.X + v_29.X, p_9.Y + v_29.Y)))), (v_30 = x, (f_18 = (x_11 / lx), Vc_$ctor_7B00E9A0(v_30.X * f_18, v_30.Y * f_18))), (v_31 = y, (f_19 = (-width / ly), Vc_$ctor_7B00E9A0(v_31.X * f_19, v_31.Y * f_19))));
            }
            case 1: {
                let y_10;
                const d_25 = (ly - offStart) - offEnd;
                y_10 = ((d_25 > 1E-06) ? d_25 : failRect2DOffsetEdge(offStart, offEnd, ly, edgeIdx, d_25));
                return Rect2D_$ctor_50BD42FD((p_11 = ((p_10 = rect.Origin, (v_33 = ((v_32 = x, (f_20 = ((lx - offEdge) / lx), Vc_$ctor_7B00E9A0(v_32.X * f_20, v_32.Y * f_20)))), Pt_$ctor_7B00E9A0(p_10.X + v_33.X, p_10.Y + v_33.Y)))), (v_35 = ((v_34 = y, (f_21 = (offStart / ly), Vc_$ctor_7B00E9A0(v_34.X * f_21, v_34.Y * f_21)))), Pt_$ctor_7B00E9A0(p_11.X + v_35.X, p_11.Y + v_35.Y))), (v_36 = x, (f_22 = (-width / lx), Vc_$ctor_7B00E9A0(v_36.X * f_22, v_36.Y * f_22))), (v_37 = y, (f_23 = (y_10 / ly), Vc_$ctor_7B00E9A0(v_37.X * f_23, v_37.Y * f_23))));
            }
            case 2: {
                let x_14;
                const d_30 = (lx - offStart) - offEnd;
                x_14 = ((d_30 > 1E-06) ? d_30 : failRect2DOffsetEdge(offStart, offEnd, lx, edgeIdx, d_30));
                return Rect2D_$ctor_50BD42FD((yy_6 = (ly - offEdge), (p_13 = ((p_12 = rect.Origin, (v_39 = ((v_38 = x, (f_24 = (offEnd / lx), Vc_$ctor_7B00E9A0(v_38.X * f_24, v_38.Y * f_24)))), Pt_$ctor_7B00E9A0(p_12.X + v_39.X, p_12.Y + v_39.Y)))), (v_41 = ((v_40 = y, (f_25 = (yy_6 / ly), Vc_$ctor_7B00E9A0(v_40.X * f_25, v_40.Y * f_25)))), Pt_$ctor_7B00E9A0(p_13.X + v_41.X, p_13.Y + v_41.Y)))), (v_42 = x, (f_26 = (x_14 / lx), Vc_$ctor_7B00E9A0(v_42.X * f_26, v_42.Y * f_26))), (v_43 = y, (f_27 = (-width / ly), Vc_$ctor_7B00E9A0(v_43.X * f_27, v_43.Y * f_27))));
            }
            case 3: {
                let y_13;
                const d_35 = (ly - offStart) - offEnd;
                y_13 = ((d_35 > 1E-06) ? d_35 : failRect2DOffsetEdge(offStart, offEnd, ly, edgeIdx, d_35));
                return Rect2D_$ctor_50BD42FD((p_15 = ((p_14 = rect.Origin, (v_45 = ((v_44 = x, (f_28 = ((offEdge + width) / lx), Vc_$ctor_7B00E9A0(v_44.X * f_28, v_44.Y * f_28)))), Pt_$ctor_7B00E9A0(p_14.X + v_45.X, p_14.Y + v_45.Y)))), (v_47 = ((v_46 = y, (f_29 = (offEnd / ly), Vc_$ctor_7B00E9A0(v_46.X * f_29, v_46.Y * f_29)))), Pt_$ctor_7B00E9A0(p_15.X + v_47.X, p_15.Y + v_47.Y))), (v_48 = x, (f_30 = (-width / lx), Vc_$ctor_7B00E9A0(v_48.X * f_30, v_48.Y * f_30))), (v_49 = y, (f_31 = (y_13 / ly), Vc_$ctor_7B00E9A0(v_49.X * f_31, v_49.Y * f_31))));
            }
            default:
                return fail(`Rect2D.offsetEdge: edgeIdx ${edgeIdx} out of range 0..3`);
        }
    }
    else {
        return fail(`Rect2D.offsetEdge: width ${width} must be more than 1e-6`);
    }
}

/**
 * Divides a 2D Rectangle into a grid of sub-rectangles. The sub-rectangles are returned as an array of arrays.
 * The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 2D Rectangle.
 * The returned array has xCount elements, each element is an array of yCount sub-rectangles.
 */
export function Rect2D_subDivide_5FE3FC8F(rect, xCount, yCount, xGap, yGap) {
    if ((xCount <= 0) ? true : (yCount <= 0)) {
        fail(`Rect2D.subDivide: xCount ${xCount} and yCount ${yCount} must be 1 or more`);
    }
    const xa = rect.Xaxis;
    const ya = rect.Yaxis;
    let xl;
    const v = xa;
    const x = v.X;
    const y = v.Y;
    xl = Math.sqrt((x * x) + (y * y));
    let yl;
    const v_1 = ya;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    yl = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    const lx1 = (xl - (xGap * (xCount - 1))) / xCount;
    const ly1 = (yl - (yGap * (yCount - 1))) / yCount;
    if (!(lx1 > 1E-12) ? true : !(ly1 > 1E-12)) {
        return [];
    }
    else {
        const o = rect.Origin;
        let vx;
        const v_2 = xa;
        const f = lx1 / xl;
        vx = Vc_$ctor_7B00E9A0(v_2.X * f, v_2.Y * f);
        let vy;
        const v_3 = ya;
        const f_1 = ly1 / yl;
        vy = Vc_$ctor_7B00E9A0(v_3.X * f_1, v_3.Y * f_1);
        const rss = fill(new Array(xCount), 0, xCount, null);
        for (let ix = 0; ix <= (xCount - 1); ix++) {
            const rs = fill(new Array(yCount), 0, yCount, new Rect2D(Pt_$ctor_7B00E9A0(0, 0), Vc_$ctor_7B00E9A0(0, 0), Vc_$ctor_7B00E9A0(0, 0)));
            for (let iy = 0; iy <= (yCount - 1); iy++) {
                let p_1, p, v_6, v_7;
                let x_4;
                const v_4 = xa;
                const f_2 = ((xGap * ix) / xl) + ((lx1 * ix) / xl);
                x_4 = Vc_$ctor_7B00E9A0(v_4.X * f_2, v_4.Y * f_2);
                let y_2;
                const v_5 = ya;
                const f_3 = ((yGap * iy) / yl) + ((ly1 * iy) / yl);
                y_2 = Vc_$ctor_7B00E9A0(v_5.X * f_3, v_5.Y * f_3);
                setItem(rs, iy, Rect2D_$ctor_50BD42FD((p_1 = ((p = o, (v_6 = x_4, Pt_$ctor_7B00E9A0(p.X + v_6.X, p.Y + v_6.Y)))), (v_7 = y_2, Pt_$ctor_7B00E9A0(p_1.X + v_7.X, p_1.Y + v_7.Y))), vx, vy));
            }
            setItem(rss, ix, rs);
        }
        return rss;
    }
}

/**
 * Divides a a 2D Rectangle into a grid of sub-rectangles.
 * The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 2D Rectangle.
 * It will create as many sub-rectangles as possible respecting the minimum side length for x and y.
 * The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
 * That means in an edge case there are more segments returned, not fewer.
 * The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
 */
export function Rect2D_subDivideMinLength_Z549E38D1(rect, xMinLen, yMinLen, xGap, yGap) {
    let xLen;
    const v = rect.Xaxis;
    const x = v.X;
    const y = v.Y;
    xLen = Math.sqrt((x * x) + (y * y));
    let yLen;
    const v_1 = rect.Yaxis;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    yLen = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    if (xLen < xMinLen) {
        fail(`Rect2D.subDivideMinLength: xMinLen ${xMinLen} is bigger than rect X-axis length ${xLen} for ${rect}`);
    }
    if (yLen < yMinLen) {
        fail(`Rect2D.subDivideMinLength: yMinLen ${yMinLen} is bigger than rect Y-axis length ${yLen} for ${rect}`);
    }
    const xCount = ~~(xLen / (xMinLen * 0.9999)) | 0;
    const yCount = ~~(yLen / (yMinLen * 0.9999)) | 0;
    return Rect2D_subDivide_5FE3FC8F(rect, xCount, yCount, xGap, yGap);
}

/**
 * Divides a a 2D Rectangle into a grid of sub-rectangles.
 * The gap between the sub-rectangles is given in x and y direction. It does not apply to the outer edges of the 2D Rectangle.
 * It will create as few as segments as possible respecting the maximum segment length.
 * The input maxSegmentLength is multiplied by factor 1.00001 of to avoid numerical errors.
 * That means in an edge case there are fewer segments returned, not more.
 * The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
 */
export function Rect2D_subDivideMaxLength_Z549E38D1(rect, xMaxLen, yMaxLen, xGap, yGap) {
    let xLen;
    const v = rect.Xaxis;
    const x = v.X;
    const y = v.Y;
    xLen = Math.sqrt((x * x) + (y * y));
    let yLen;
    const v_1 = rect.Yaxis;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    yLen = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    const xCount = (1 + ~~(xLen / (xMaxLen * 1.00001))) | 0;
    const yCount = (1 + ~~(yLen / (yMaxLen * 1.00001))) | 0;
    return Rect2D_subDivide_5FE3FC8F(rect, xCount, yCount, xGap, yGap);
}

/**
 * Divides a 2D Rectangle into a grid of points. The points are returned as an array of arrays.
 * A xCount and yCount of 2 will only return just the 4 corners of the rectangle.
 * A xCount and yCount of 3 will return 9 points, including the 4 corners, the 4 mid points on the edges and the center.
 */
export function Rect2D_grid_638FCC2F(rect, xCount, yCount) {
    if ((xCount <= 1) ? true : (yCount <= 1)) {
        fail(`Rect2D.grid: xCount ${xCount} and yCount ${yCount} must be 2 or more`);
    }
    const xa = rect.Xaxis;
    const ya = rect.Yaxis;
    let xl;
    const v = xa;
    const x = v.X;
    const y = v.Y;
    xl = Math.sqrt((x * x) + (y * y));
    let yl;
    const v_1 = ya;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    yl = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    const lx1 = xl / (xCount - 1);
    const ly1 = yl / (yCount - 1);
    const o = rect.Origin;
    const rss = fill(new Array(xCount), 0, xCount, null);
    for (let ix = 0; ix <= (xCount - 1); ix++) {
        const rs = fill(new Array(yCount), 0, yCount, Pt_$ctor_7B00E9A0(0, 0));
        for (let iy = 0; iy <= (yCount - 1); iy++) {
            let p_1, p, v_4, v_5;
            let x_2;
            const v_2 = xa;
            const f = (lx1 * ix) / xl;
            x_2 = Vc_$ctor_7B00E9A0(v_2.X * f, v_2.Y * f);
            let y_2;
            const v_3 = ya;
            const f_1 = (ly1 * iy) / yl;
            y_2 = Vc_$ctor_7B00E9A0(v_3.X * f_1, v_3.Y * f_1);
            setItem(rs, iy, (p_1 = ((p = o, (v_4 = x_2, Pt_$ctor_7B00E9A0(p.X + v_4.X, p.Y + v_4.Y)))), (v_5 = y_2, Pt_$ctor_7B00E9A0(p_1.X + v_5.X, p_1.Y + v_5.Y))));
        }
        setItem(rss, ix, rs);
    }
    return rss;
}

/**
 * Divides a a 2D Rectangle into a grid of points.
 * It will create as many points as possible respecting the minimum side length for x and y.
 * The input minSegmentLength is multiplied by factor 0.9999 of to avoid numerical errors.
 * That means in an edge case there are more segments returned, not fewer.
 * The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
 */
export function Rect2D_gridMinLength_ZFDE0F31(rect, xMinLen, yMinLen) {
    let xLen;
    const v = rect.Xaxis;
    const x = v.X;
    const y = v.Y;
    xLen = Math.sqrt((x * x) + (y * y));
    let yLen;
    const v_1 = rect.Yaxis;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    yLen = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    if (xLen < xMinLen) {
        fail(`Rect2D.gridMinLength: xMinLen ${xMinLen} is bigger than rect X-axis length ${xLen} for ${rect}`);
    }
    if (yLen < yMinLen) {
        fail(`Rect2D.gridMinLength: yMinLen ${yMinLen} is bigger than rect Y-axis length ${yLen} for ${rect}`);
    }
    const xCount = (1 + ~~(xLen / (xMinLen * 0.9999))) | 0;
    const yCount = (1 + ~~(yLen / (yMinLen * 0.9999))) | 0;
    return Rect2D_grid_638FCC2F(rect, xCount, yCount);
}

/**
 * Divides a a 2D Rectangle into a grid of points.
 * It will create as few as points as possible respecting the maximum segment length.
 * The input maxSegmentLength is multiplied by factor 1.0001 of to avoid numerical errors.
 * That means in an edge case there are fewer segments returned, not more.
 * The returned array is divided along the x-axis. The sub-array is divided along the y-axis.
 */
export function Rect2D_gridMaxLength_ZFDE0F31(rect, xMaxLen, yMaxLen) {
    let xLen;
    const v = rect.Xaxis;
    const x = v.X;
    const y = v.Y;
    xLen = Math.sqrt((x * x) + (y * y));
    let yLen;
    const v_1 = rect.Yaxis;
    const x_1 = v_1.X;
    const y_1 = v_1.Y;
    yLen = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
    const xCount = (2 + ~~(xLen / (xMaxLen * 1.00001))) | 0;
    const yCount = (2 + ~~(yLen / (yMaxLen * 1.00001))) | 0;
    return Rect2D_grid_638FCC2F(rect, xCount, yCount);
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
export function Rect2D__get_RotateOrientation90CW(r) {
    let p, v, v_1;
    return Rect2D_$ctor_50BD42FD((p = r.Origin, (v = r.Yaxis, Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y))), (v_1 = r.Yaxis, Vc_$ctor_7B00E9A0(-v_1.X, -v_1.Y)), r.Xaxis);
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
export function Rect2D__get_RotateOrientation180(r) {
    let p_1, p, v, v_1, v_2, v_3;
    return Rect2D_$ctor_50BD42FD((p_1 = ((p = r.Origin, (v = r.Yaxis, Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y)))), (v_1 = r.Xaxis, Pt_$ctor_7B00E9A0(p_1.X + v_1.X, p_1.Y + v_1.Y))), (v_2 = r.Xaxis, Vc_$ctor_7B00E9A0(-v_2.X, -v_2.Y)), (v_3 = r.Yaxis, Vc_$ctor_7B00E9A0(-v_3.X, -v_3.Y)));
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
export function Rect2D__get_RotateOrientation90CCW(r) {
    let p, v, v_1;
    return Rect2D_$ctor_50BD42FD((p = r.Origin, (v = r.Xaxis, Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y))), r.Yaxis, (v_1 = r.Xaxis, Vc_$ctor_7B00E9A0(-v_1.X, -v_1.Y)));
}

/**
 * Returns the 4 corners of the 2D Rectangle in Counter-Clockwise order, starting at Origin.
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
export function Rect2D__get_Points(r) {
    let p_1, v_1, p_2, v_2;
    const p0 = r.Origin;
    let p1;
    const p = p0;
    const v = r.Xaxis;
    p1 = Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y);
    return [p0, p1, (p_1 = p1, (v_1 = r.Yaxis, Pt_$ctor_7B00E9A0(p_1.X + v_1.X, p_1.Y + v_1.Y))), (p_2 = p0, (v_2 = r.Yaxis, Pt_$ctor_7B00E9A0(p_2.X + v_2.X, p_2.Y + v_2.Y)))];
}

/**
 * Returns the 4 corners of the 2D Rectangle als closed loop in Counter-Clockwise order, starting at Origin.
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
export function Rect2D__get_PointsLooped(r) {
    let p_1, v_1, p_2, v_2;
    const p0 = r.Origin;
    let p1;
    const p = p0;
    const v = r.Xaxis;
    p1 = Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y);
    return [p0, p1, (p_1 = p1, (v_1 = r.Yaxis, Pt_$ctor_7B00E9A0(p_1.X + v_1.X, p_1.Y + v_1.Y))), (p_2 = p0, (v_2 = r.Yaxis, Pt_$ctor_7B00E9A0(p_2.X + v_2.X, p_2.Y + v_2.Y))), p0];
}

/**
 * Returns the 4 Edges of the 2D Rectangle in Counter-Clockwise order, starting at Origin.
 * Returns an array of 4 Lines: from point 0 to 1, 1 to 2 to 3 and 3 to 0.
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
export function Rect2D__get_Edges(r) {
    const p0 = r.Origin;
    let p1;
    const p = p0;
    const v = r.Xaxis;
    p1 = Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y);
    let p2;
    const p_1 = p1;
    const v_1 = r.Yaxis;
    p2 = Pt_$ctor_7B00E9A0(p_1.X + v_1.X, p_1.Y + v_1.Y);
    let p3;
    const p_2 = p0;
    const v_2 = r.Yaxis;
    p3 = Pt_$ctor_7B00E9A0(p_2.X + v_2.X, p_2.Y + v_2.Y);
    return [Line2D_$ctor_Z53905FA0(p0, p1), Line2D_$ctor_Z53905FA0(p1, p2), Line2D_$ctor_Z53905FA0(p2, p3), Line2D_$ctor_Z53905FA0(p3, p0)];
}

/**
 * Returns one of the 4 Edges as 2D Line:
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
export function Rect2D__GetEdge_Z524259A4(r, i) {
    let p, v, p_1, v_1, p_3, p_2, v_2, v_3, p_5, p_4, v_4, v_5, p_6, v_6, p_7, v_7;
    switch (i) {
        case 0:
            return Line2D_$ctor_Z53905FA0(r.Origin, (p = r.Origin, (v = r.Xaxis, Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y))));
        case 1:
            return Line2D_$ctor_Z53905FA0((p_1 = r.Origin, (v_1 = r.Xaxis, Pt_$ctor_7B00E9A0(p_1.X + v_1.X, p_1.Y + v_1.Y))), (p_3 = ((p_2 = r.Origin, (v_2 = r.Xaxis, Pt_$ctor_7B00E9A0(p_2.X + v_2.X, p_2.Y + v_2.Y)))), (v_3 = r.Yaxis, Pt_$ctor_7B00E9A0(p_3.X + v_3.X, p_3.Y + v_3.Y))));
        case 2:
            return Line2D_$ctor_Z53905FA0((p_5 = ((p_4 = r.Origin, (v_4 = r.Xaxis, Pt_$ctor_7B00E9A0(p_4.X + v_4.X, p_4.Y + v_4.Y)))), (v_5 = r.Yaxis, Pt_$ctor_7B00E9A0(p_5.X + v_5.X, p_5.Y + v_5.Y))), (p_6 = r.Origin, (v_6 = r.Yaxis, Pt_$ctor_7B00E9A0(p_6.X + v_6.X, p_6.Y + v_6.Y))));
        case 3:
            return Line2D_$ctor_Z53905FA0((p_7 = r.Origin, (v_7 = r.Yaxis, Pt_$ctor_7B00E9A0(p_7.X + v_7.X, p_7.Y + v_7.Y))), r.Origin);
        default:
            return fail(`Rect2D.GetEdge: index ${i} out of range 0..3`);
    }
}

