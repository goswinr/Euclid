
import { failObsoleteV30, fail, failTooSmall, failTooSmall2 } from "../EuclidErrors.js";
import { Pt_$ctor_7B00E9A0 } from "../Pt.js";
import { XLine2D_getSqDistance_Z6A6F0C80, XLine2D_getIntersection_40607834, XLine2D_doOverlap_199764BB, XLine2D_getIntersectionParam_40607834, XLine2D_getClosestParameters_40607834, XLine2D_getClosestPoints_40607834 } from "../XLine2D.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "../Pt.js";
import { Line2D, Line2D_$ctor_77D16AC0 } from "../Line2D.js";
import { item as item_2, setItem, fill } from "../../fable_modules/fable-library-js.5.0.0/Array.js";
import { Vc_$ctor_7B00E9A0 } from "../Vc.js";
import { XLine2D_getEndsTouching_Z44565CE5 } from "../XLine2D.js";

/**
 * Assumes the Line2D to be an infinite ray!
 * Returns the parameter at which a point is closest to the ray.
 * If it is smaller than 0.0 or bigger than 1.0 it is outside of the finite line.
 * Fails on curves shorter than 1e-6 units. (ln.ClosestParameter does not)
 */
export function Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD(ln, pt) {
    const x = ln.FromX - ln.ToX;
    const y = ln.FromY - ln.ToY;
    const lenSq = (x * x) + (y * y);
    if (!(lenSq > 1E-12)) {
        failTooSmall2("Line2D.RayClosestParameter", ln, pt);
    }
    const u = ln.FromX - pt.X;
    const v = ln.FromY - pt.Y;
    const dot = (x * u) + (y * v);
    return dot / lenSq;
}

/**
 * Assumes Line2D to be an infinite ray!
 * Returns closest point on ray.
 * Fails on curves shorter than 1e-6 units. (ln.ClosestPoint does not.)
 */
export function Euclid_Line2D__Line2D_RayClosestPoint_6ADE94FD(ln, pt) {
    const fromX = ln.FromX;
    const fromY = ln.FromY;
    const x = fromX - ln.ToX;
    const y = fromY - ln.ToY;
    const lenSq = (x * x) + (y * y);
    if (!(lenSq > 1E-12)) {
        failTooSmall2("Line2D.RayClosestPoint", ln, pt);
    }
    const u = fromX - pt.X;
    const v = fromY - pt.Y;
    const dot = (x * u) + (y * v);
    const t = dot / lenSq;
    const x$0027 = fromX - (x * t);
    const y$0027 = fromY - (y * t);
    return Pt_$ctor_7B00E9A0(x$0027, y$0027);
}

/**
 * Returns closest point on (finite) line.
 * Does not fail on very short curves.
 */
export function Euclid_Line2D__Line2D_ClosestPoint_6ADE94FD(ln, p) {
    let ln_3, ln_4;
    const ln_2 = ln;
    let p_2;
    const ln_1 = ln;
    const p_1 = p;
    const x = ln_1.FromX - ln_1.ToX;
    const y = ln_1.FromY - ln_1.ToY;
    const u = ln_1.FromX - p_1.X;
    const v = ln_1.FromY - p_1.Y;
    const dot = (x * u) + (y * v);
    const lenSq = (x * x) + (y * y);
    if (!(lenSq > 1E-12)) {
        p_2 = ((dot < 0) ? 0 : 1);
    }
    else {
        const x_2 = dot / lenSq;
        p_2 = ((x_2 > 0) ? ((x_2 < 1) ? x_2 : 1) : 0);
    }
    return Pt_$ctor_7B00E9A0(ln_2.FromX + (((ln_3 = ln_2, ln_3.ToX - ln_3.FromX)) * p_2), ln_2.FromY + (((ln_4 = ln_2, ln_4.ToY - ln_4.FromY)) * p_2));
}

/**
 * Finds the closest points between two finite 2D Lines, also works on parallel and overlapping lines.
 */
export function Euclid_Line2D__Line2D_ClosestPoints_4CC2E301(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5, ln_6, ln_7, pAx, pAy, vAx, ln_8, vAy, ln_9, t, vAx_1, vAy_1, u, v, dotV, lenSq, ln_10, pAx_2, pAy_2, vAx_2, ln_11, vAy_2, ln_12, t_1, vAx_3, vAy_3, u_1, v_1, dotV_1, lenSq_1, ln_13;
    let matchValue;
    const lineA = lnA;
    const lineB = lnB;
    matchValue = XLine2D_getClosestPoints_40607834(lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineB, ln_2.ToX - ln_2.FromX), (ln_3 = lineB, ln_3.ToY - ln_3.FromY), 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 1: {
            const b_1 = matchValue.fields[1];
            const a_1 = matchValue.fields[0];
            return [a_1, b_1];
        }
        case 0: {
            const a_2 = matchValue.fields[0];
            return [a_2, a_2];
        }
        case 5:
            return [(ln_4 = lnA, Pt_$ctor_7B00E9A0_1(ln_4.FromX, ln_4.FromY)), (ln_5 = lnB, Pt_$ctor_7B00E9A0_1(ln_5.FromX, ln_5.FromY))];
        case 3:
            return [(ln_6 = lnA, Pt_$ctor_7B00E9A0_1(ln_6.FromX, ln_6.FromY)), (ln_7 = lnB, (pAx = ln_7.FromX, (pAy = ln_7.FromY, (vAx = ((ln_8 = ln_7, ln_8.ToX - ln_8.FromX)), (vAy = ((ln_9 = ln_7, ln_9.ToY - ln_9.FromY)), (t = ((vAx_1 = vAx, (vAy_1 = vAy, (u = (lnA.FromX - pAx), (v = (lnA.FromY - pAy), (dotV = ((vAx_1 * u) + (vAy_1 * v)), (lenSq = ((vAx_1 * vAx_1) + (vAy_1 * vAy_1)), dotV / lenSq))))))), (t > -1E-06) ? ((t < 1.000001) ? Pt_$ctor_7B00E9A0_1(pAx + (vAx * t), pAy + (vAy * t)) : Pt_$ctor_7B00E9A0_1(pAx + vAx, pAy + vAy)) : Pt_$ctor_7B00E9A0_1(pAx, pAy)))))))];
        case 4:
            return [(ln_10 = lnA, (pAx_2 = ln_10.FromX, (pAy_2 = ln_10.FromY, (vAx_2 = ((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)), (vAy_2 = ((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)), (t_1 = ((vAx_3 = vAx_2, (vAy_3 = vAy_2, (u_1 = (lnB.FromX - pAx_2), (v_1 = (lnB.FromY - pAy_2), (dotV_1 = ((vAx_3 * u_1) + (vAy_3 * v_1)), (lenSq_1 = ((vAx_3 * vAx_3) + (vAy_3 * vAy_3)), dotV_1 / lenSq_1))))))), (t_1 > -1E-06) ? ((t_1 < 1.000001) ? Pt_$ctor_7B00E9A0_1(pAx_2 + (vAx_2 * t_1), pAy_2 + (vAy_2 * t_1)) : Pt_$ctor_7B00E9A0_1(pAx_2 + vAx_2, pAy_2 + vAy_2)) : Pt_$ctor_7B00E9A0_1(pAx_2, pAy_2))))))), (ln_13 = lnB, Pt_$ctor_7B00E9A0_1(ln_13.FromX, ln_13.FromY))];
        default: {
            const b = matchValue.fields[1];
            const a = matchValue.fields[0];
            return [a, b];
        }
    }
}

/**
 * Finds the parameters of closest points between two finite 2D Lines, also works on parallel and overlapping lines.
 */
export function Euclid_Line2D__Line2D_ClosestParameters_4CC2E301(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, x_2, vAx, ln_5, vAy, ln_6, u, v, dotV, lenSq, ln_7, x_5, vAx_1, ln_8, vAy_1, ln_9, u_1, v_1, dotV_1, lenSq_1;
    let matchValue;
    const lineA = lnA;
    const lineB = lnB;
    matchValue = XLine2D_getClosestParameters_40607834(lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineB, ln_2.ToX - ln_2.FromX), (ln_3 = lineB, ln_3.ToY - ln_3.FromY), 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 1: {
            const b_1 = matchValue.fields[1];
            const a_1 = matchValue.fields[0];
            return [a_1, b_1];
        }
        case 0: {
            const b_2 = matchValue.fields[1];
            const a_2 = matchValue.fields[0];
            return [a_2, b_2];
        }
        case 5:
            return [0, 0];
        case 3:
            return [0, (ln_4 = lnB, (x_2 = ((vAx = ((ln_5 = ln_4, ln_5.ToX - ln_5.FromX)), (vAy = ((ln_6 = ln_4, ln_6.ToY - ln_6.FromY)), (u = (lnA.FromX - ln_4.FromX), (v = (lnA.FromY - ln_4.FromY), (dotV = ((vAx * u) + (vAy * v)), (lenSq = ((vAx * vAx) + (vAy * vAy)), dotV / lenSq))))))), (x_2 > 0) ? ((x_2 < 1) ? x_2 : 1) : 0))];
        case 4:
            return [(ln_7 = lnA, (x_5 = ((vAx_1 = ((ln_8 = ln_7, ln_8.ToX - ln_8.FromX)), (vAy_1 = ((ln_9 = ln_7, ln_9.ToY - ln_9.FromY)), (u_1 = (lnB.FromX - ln_7.FromX), (v_1 = (lnB.FromY - ln_7.FromY), (dotV_1 = ((vAx_1 * u_1) + (vAy_1 * v_1)), (lenSq_1 = ((vAx_1 * vAx_1) + (vAy_1 * vAy_1)), dotV_1 / lenSq_1))))))), (x_5 > 0) ? ((x_5 < 1) ? x_5 : 1) : 0)), 0];
        default: {
            const b = matchValue.fields[1];
            const a = matchValue.fields[0];
            return [a, b];
        }
    }
}

/**
 * Assumes Line2D to be an infinite ray!
 * Returns square distance from point to ray.
 * Fails on curves shorter than 1e-6 units. (ln.DistanceSqFromPoint does not.)
 */
export function Euclid_Line2D__Line2D_SqDistanceRayPoint_6ADE94FD(ln, p) {
    let vAx;
    const ln_1 = ln;
    vAx = (ln_1.ToX - ln_1.FromX);
    let vAy;
    const ln_2 = ln;
    vAy = (ln_2.ToY - ln_2.FromY);
    const nx = -vAy;
    const ny = vAx;
    const lenSq = (nx * nx) + (ny * ny);
    if (!(lenSq > 1E-12)) {
        failTooSmall2("Line2D.SqDistanceRayPoint", ln, p);
    }
    const u = ln.FromX - p.X;
    const v = ln.FromY - p.Y;
    const dot = (nx * u) + (ny * v);
    return (dot * dot) / lenSq;
}

/**
 * Returns square distance from point to finite line.
 */
export function Euclid_Line2D__Line2D_SqDistanceFromPoint_6ADE94FD(ln, p) {
    const ln_1 = ln;
    const pAx = ln_1.FromX;
    const pAy = ln_1.FromY;
    let vAx;
    const ln_2 = ln_1;
    vAx = (ln_2.ToX - ln_2.FromX);
    let vAy;
    const ln_3 = ln_1;
    vAy = (ln_3.ToY - ln_3.FromY);
    const x_1 = p.X;
    const y_1 = p.Y;
    let t;
    const vAx_1 = vAx;
    const vAy_1 = vAy;
    const u = x_1 - pAx;
    const v = y_1 - pAy;
    const dotV = (vAx_1 * u) + (vAy_1 * v);
    const lenSq = (vAx_1 * vAx_1) + (vAy_1 * vAy_1);
    t = (dotV / lenSq);
    if (t > -1E-06) {
        if (t < 1.000001) {
            const clPtX = pAx + (vAx * t);
            const clPtY = pAy + (vAy * t);
            const vx = clPtX - x_1;
            const vy = clPtY - y_1;
            return (vx * vx) + (vy * vy);
        }
        else {
            const clPtX_1 = pAx + vAx;
            const clPtY_1 = pAy + vAy;
            const vx_1 = clPtX_1 - x_1;
            const vy_1 = clPtY_1 - y_1;
            return (vx_1 * vx_1) + (vy_1 * vy_1);
        }
    }
    else {
        const vX = pAx - x_1;
        const vY = pAy - y_1;
        return (vX * vX) + (vY * vY);
    }
}

/**
 * Check if two 2D lines are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two 2D-lines are not exactly equal.
 */
export function Euclid_Line2D__Line2D_notEquals_Static(tol, a, b) {
    if (((Math.abs(a.FromX - b.FromX) > tol) ? true : (Math.abs(a.FromY - b.FromY) > tol)) ? true : (Math.abs(a.ToX - b.ToX) > tol)) {
        return true;
    }
    else {
        return Math.abs(a.ToY - b.ToY) > tol;
    }
}

/**
 * Offset line in XY-Plane to left side in line direction.
 * Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
 * If amount is 0.0 no offset is computed and the input line is returned.
 */
export function Euclid_Line2D__Line2D_offset_Static(distance, ln) {
    if (distance === 0) {
        return ln;
    }
    else {
        let x;
        const ln_1 = ln;
        x = (ln_1.ToX - ln_1.FromX);
        let y;
        const ln_2 = ln;
        y = (ln_2.ToY - ln_2.FromY);
        const lenXY = Math.sqrt((x * x) + (y * y));
        if (!(lenXY > 1E-12)) {
            failTooSmall("Line2D.offset", ln);
        }
        const ox = (-y * distance) / lenXY;
        const oy = (x * distance) / lenXY;
        return Line2D_$ctor_77D16AC0(ln.FromX + ox, ln.FromY + oy, ln.ToX + ox, ln.ToY + oy);
    }
}

/**
 * Divides a 2D line into given amount of segments.
 * Returns an array of 2D points of length: segment count + 1.
 * Includes start and endpoint of line.
 */
export function Euclid_Line2D__Line2D_divide_Static(segments, ln) {
    let ln_1, ln_2, ln_5, ln_6;
    if (segments < 1) {
        fail(`Line2D.divide: segments < 1: ${segments}`);
    }
    if (segments === 1) {
        return [(ln_1 = ln, Pt_$ctor_7B00E9A0_1(ln_1.FromX, ln_1.FromY)), (ln_2 = ln, Pt_$ctor_7B00E9A0_1(ln_2.ToX, ln_2.ToY))];
    }
    else {
        let x;
        const ln_3 = ln;
        x = (ln_3.ToX - ln_3.FromX);
        let y;
        const ln_4 = ln;
        y = (ln_4.ToY - ln_4.FromY);
        const sx = ln.FromX;
        const sy = ln.FromY;
        const kk = segments;
        const r = fill(new Array(segments + 1), 0, segments + 1, Pt_$ctor_7B00E9A0_1(0, 0));
        setItem(r, 0, (ln_5 = ln, Pt_$ctor_7B00E9A0_1(ln_5.FromX, ln_5.FromY)));
        for (let i = 1; i <= (segments - 1); i++) {
            const t = i / kk;
            setItem(r, i, Pt_$ctor_7B00E9A0(sx + (x * t), sy + (y * t)));
        }
        setItem(r, segments, (ln_6 = ln, Pt_$ctor_7B00E9A0_1(ln_6.ToX, ln_6.ToY)));
        return r;
    }
}

/**
 * Divides a 2D line into as many as segments as possible respecting the minimum segment length.
 * Returned Array includes start and endpoint of line.
 * The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
 * That means in an edge case there are more segments returned, not fewer.
 */
export function Euclid_Line2D__Line2D_divideMinLength_Static(minSegmentLength, ln) {
    let len;
    const ln_1 = ln;
    let x;
    const ln_2 = ln_1;
    x = (ln_2.ToX - ln_2.FromX);
    let y;
    const ln_3 = ln_1;
    y = (ln_3.ToY - ln_3.FromY);
    len = Math.sqrt((x * x) + (y * y));
    if (len < 1E-06) {
        fail(`Line2D.divideMinLength: line length ${len} is too small.`);
    }
    if (len < minSegmentLength) {
        fail(`Line2D.divideMinLength: line length ${len} is smaller than minSegmentLength ${minSegmentLength}`);
    }
    const k = ~~(len / (minSegmentLength * 1.000001)) | 0;
    return Euclid_Line2D__Line2D_divide_Static(k, ln);
}

/**
 * Divides a 2D line into as few as segments as possible respecting the maximum segment length.
 * Returned Array includes start and endpoint of line.
 * The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical  errors.
 * That means in an edge case there are fewer segments returned, not more.
 */
export function Euclid_Line2D__Line2D_divideMaxLength_Static(maxSegmentLength, ln) {
    let len;
    const ln_1 = ln;
    let x;
    const ln_2 = ln_1;
    x = (ln_2.ToX - ln_2.FromX);
    let y;
    const ln_3 = ln_1;
    y = (ln_3.ToY - ln_3.FromY);
    len = Math.sqrt((x * x) + (y * y));
    if (len < 1E-06) {
        fail(`Line2D.divideMaxLength: line length ${len} is too small.`);
    }
    if (maxSegmentLength < 1E-06) {
        fail(`Line2D.divideMaxLength: maxSegmentLength must be greater than 0.0, was ${maxSegmentLength}`);
    }
    const k = (~~((len / maxSegmentLength) * 0.999999) + 1) | 0;
    return Euclid_Line2D__Line2D_divide_Static(k, ln);
}

/**
 * Divides a 2D line into given amount of segments.
 * Includes a gap between the segments. But not at the start or end.
 * Returns an array of 2D Lines.
 * Returns an empty array if the length of the line is less than gap-size x segment-count-minus-1.
 */
export function Euclid_Line2D__Line2D_split_Static(gap, segments, ln) {
    let ln_2, ln_3, pt_1, ln_6, ln_5;
    if (segments <= 0) {
        fail(`Line2D.split: invalid segments: ${segments}`);
    }
    let v;
    const ln_1 = ln;
    v = Vc_$ctor_7B00E9A0((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY));
    let len;
    const v_1 = v;
    const x = v_1.X;
    const y = v_1.Y;
    len = Math.sqrt((x * x) + (y * y));
    const lenMinusGaps = len - (gap * (segments - 1));
    const segLen = lenMinusGaps / segments;
    if (!(segLen > 1E-12)) {
        return [];
    }
    else {
        const lns = fill(new Array(segments), 0, segments, new Line2D(0, 0, 0, 0));
        const vx = v.X;
        const vy = v.Y;
        const x_2 = ln.FromX;
        const y_1 = ln.FromY;
        for (let i = 0; i <= (segments - 1); i++) {
            const g = i;
            const s = i + 1;
            const sf = ((g * segLen) + (g * gap)) / len;
            const ef = ((s * segLen) + (g * gap)) / len;
            const xs = x_2 + (vx * sf);
            const ys = y_1 + (vy * sf);
            const xe = x_2 + (vx * ef);
            const ye = y_1 + (vy * ef);
            setItem(lns, i, Line2D_$ctor_77D16AC0(xs, ys, xe, ye));
        }
        setItem(lns, segments - 1, (pt_1 = ((ln_6 = ln, Pt_$ctor_7B00E9A0_1(ln_6.ToX, ln_6.ToY))), (ln_5 = item_2(segments - 1, lns), Line2D_$ctor_77D16AC0(ln_5.FromX, ln_5.FromY, pt_1.X, pt_1.Y))));
        return lns;
    }
}

/**
 * Divides a 2D line into as many as segments as possible respecting the minimum segment length and the gap.
 * Includes a gap between the segments. But not at the start or end.
 * Returns an array of 2D lines
 * The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
 * That means in an edge case there are more segments returned, not fewer.
 */
export function Euclid_Line2D__Line2D_splitMinLength_Static(gap, minSegmentLength, ln) {
    let len;
    const ln_1 = ln;
    let x;
    const ln_2 = ln_1;
    x = (ln_2.ToX - ln_2.FromX);
    let y;
    const ln_3 = ln_1;
    y = (ln_3.ToY - ln_3.FromY);
    len = Math.sqrt((x * x) + (y * y));
    if (len < minSegmentLength) {
        fail(`Line2D.splitMinLength: line length ${len} is smaller than minSegmentLength ${minSegmentLength}`);
    }
    if (minSegmentLength < 1E-06) {
        fail(`Line2D.splitMinLength: minSegmentLength must be greater than 0.0, was ${minSegmentLength}`);
    }
    const k = ~~((len + gap) / ((minSegmentLength + gap) * 1.000001)) | 0;
    return Euclid_Line2D__Line2D_split_Static(gap, k, ln);
}

/**
 * Divides a 2D line into as few as segments as possible respecting the maximum segment length and the gap.
 * Includes a gap between the segments. But not at the start or end.
 * Returns an array of 2D lines
 * The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical errors.
 * That means in an edge case there are fewer segments returned, not more.
 */
export function Euclid_Line2D__Line2D_splitMaxLength_Static(gap, maxSegmentLength, ln) {
    let len;
    const ln_1 = ln;
    let x;
    const ln_2 = ln_1;
    x = (ln_2.ToX - ln_2.FromX);
    let y;
    const ln_3 = ln_1;
    y = (ln_3.ToY - ln_3.FromY);
    len = Math.sqrt((x * x) + (y * y));
    if (maxSegmentLength < 1E-06) {
        fail(`Line2D.splitMaxLength: maxSegmentLength must be greater than 0.0, was ${maxSegmentLength}`);
    }
    if (len < 1E-06) {
        fail(`Line2D.splitMaxLength: line length ${len} is too small.`);
    }
    const k = (~~(((len + gap) / (maxSegmentLength + gap)) * 0.999999) + 1) | 0;
    return Euclid_Line2D__Line2D_split_Static(gap, k, ln);
}

/**
 * Divides a 2D line into segments of given length.
 * Includes start and end point.
 * If the line length is smaller than the given distance just the start and end point is returned.
 * Adds end point only if there is a remainder bigger than 0.1% of the segment length.
 */
export function Euclid_Line2D__Line2D_divideEvery_Static(dist, l) {
    let ln_3, ln_4, ln_5, ln_9;
    let len;
    const ln = l;
    let x;
    const ln_1 = ln;
    x = (ln_1.ToX - ln_1.FromX);
    let y;
    const ln_2 = ln;
    y = (ln_2.ToY - ln_2.FromY);
    len = Math.sqrt((x * x) + (y * y));
    const div = len / dist;
    const floor = Math.floor(div);
    if (floor === 0) {
        const pts = [];
        void (pts.push((ln_3 = l, Pt_$ctor_7B00E9A0_1(ln_3.FromX, ln_3.FromY))));
        void (pts.push((ln_4 = l, Pt_$ctor_7B00E9A0_1(ln_4.ToX, ln_4.ToY))));
        return pts;
    }
    else {
        const step = 1 / floor;
        const count = ~~floor | 0;
        const pts_1 = [];
        void (pts_1.push((ln_5 = l, Pt_$ctor_7B00E9A0_1(ln_5.FromX, ln_5.FromY))));
        for (let i = 1; i <= count; i++) {
            let ln_7, ln_8;
            let item;
            const ln_6 = l;
            const p = step * i;
            item = Pt_$ctor_7B00E9A0(ln_6.FromX + (((ln_7 = ln_6, ln_7.ToX - ln_7.FromX)) * p), ln_6.FromY + (((ln_8 = ln_6, ln_8.ToY - ln_8.FromY)) * p));
            void (pts_1.push(item));
        }
        if ((div - floor) > 0.001) {
            void (pts_1.push((ln_9 = l, Pt_$ctor_7B00E9A0_1(ln_9.ToX, ln_9.ToY))));
        }
        return pts_1;
    }
}

/**
 * Divides a 2D line into segments of given length.
 * Excludes start and end point.
 * If the line length is smaller than the given distance an empty array is returned.
 * Adds last div point before end only if there is a remainder bigger than 0.1% of the segment length.
 */
export function Euclid_Line2D__Line2D_divideInsideEvery_Static(dist, l) {
    let ln_7, ln_8;
    let len;
    const ln = l;
    let x;
    const ln_1 = ln;
    x = (ln_1.ToX - ln_1.FromX);
    let y;
    const ln_2 = ln;
    y = (ln_2.ToY - ln_2.FromY);
    len = Math.sqrt((x * x) + (y * y));
    const div = len / dist;
    const floor = Math.floor(div);
    if (floor === 0) {
        return [];
    }
    else {
        const step = 1 / floor;
        const count = ~~floor | 0;
        const pts = [];
        for (let i = 1; i <= (count - 1); i++) {
            let ln_4, ln_5;
            let item;
            const ln_3 = l;
            const p = step * i;
            item = Pt_$ctor_7B00E9A0(ln_3.FromX + (((ln_4 = ln_3, ln_4.ToX - ln_4.FromX)) * p), ln_3.FromY + (((ln_5 = ln_3, ln_5.ToY - ln_5.FromY)) * p));
            void (pts.push(item));
        }
        if ((div - floor) > 0.001) {
            let item_1;
            const ln_6 = l;
            const p_1 = step * floor;
            item_1 = Pt_$ctor_7B00E9A0(ln_6.FromX + (((ln_7 = ln_6, ln_7.ToX - ln_7.FromX)) * p_1), ln_6.FromY + (((ln_8 = ln_6, ln_8.ToY - ln_8.FromY)) * p_1));
            void (pts.push(item_1));
        }
        return pts;
    }
}

/**
 * Project a line onto another line considered infinite in both directions.
 * Returns the start and end parameters of the projected line on the target line.
 */
export function Euclid_Line2D__Line2D_projectOntoRayParam_Static(rayToProjectOnto, lineToProject) {
    const osx = rayToProjectOnto.FromX;
    const osy = rayToProjectOnto.FromY;
    const ovx = rayToProjectOnto.ToX - osx;
    const ovy = rayToProjectOnto.ToY - osy;
    const lenSq = (ovx * ovx) + (ovy * ovy);
    if (!(lenSq > 1E-12)) {
        failTooSmall("Line2D.projectOntoRayParam", rayToProjectOnto);
    }
    const u = lineToProject.FromX - osx;
    const v = lineToProject.FromY - osy;
    const dot = (ovx * u) + (ovy * v);
    const s = dot / lenSq;
    const u_1 = lineToProject.ToX - osx;
    const v_1 = lineToProject.ToY - osy;
    const dot_1 = (ovx * u_1) + (ovy * v_1);
    const e = dot_1 / lenSq;
    return [s, e];
}

/**
 * Project a line onto another line considered infinite in both directions.
 */
export function Euclid_Line2D__Line2D_projectOntoRay_Static(rayToProjectOnto, lineToProject) {
    const osx = rayToProjectOnto.FromX;
    const osy = rayToProjectOnto.FromY;
    const ovx = rayToProjectOnto.ToX - osx;
    const ovy = rayToProjectOnto.ToY - osy;
    const lenSq = (ovx * ovx) + (ovy * ovy);
    if (!(lenSq > 1E-12)) {
        failTooSmall("Line2D.projectOntoRay", rayToProjectOnto);
    }
    const u = lineToProject.FromX - osx;
    const v = lineToProject.FromY - osy;
    const dot = (ovx * u) + (ovy * v);
    const s = dot / lenSq;
    const u_1 = lineToProject.ToX - osx;
    const v_1 = lineToProject.ToY - osy;
    const dot_1 = (ovx * u_1) + (ovy * v_1);
    const e = dot_1 / lenSq;
    return Line2D_$ctor_77D16AC0(osx + (ovx * s), osy + (ovy * s), osx + (ovx * e), osy + (ovy * e));
}

/**
 * Tries to project a line onto another line considered finite.
 * Returns None if there is no overlap.
 * Returns Some (startParam, endParam) if there is an overlap.
 * The parameters are between 0.0 and 1.0 on the target line
 * The first parameter is from the start of the line to project.
 * The second parameter is from the end of the line to project.
 * So the if the first parameter is bigger than the second, the lines are oriented in opposite direction.
 */
export function Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine, lineToProject) {
    let x_1, x_2;
    const osx = onToLine.FromX;
    const osy = onToLine.FromY;
    const ovx = onToLine.ToX - osx;
    const ovy = onToLine.ToY - osy;
    const lenSq = (ovx * ovx) + (ovy * ovy);
    if (!(lenSq > 1E-12)) {
        failTooSmall("Line2D.tryProjectOntoLineParam", onToLine);
    }
    const u = lineToProject.FromX - osx;
    const v = lineToProject.FromY - osy;
    const dot = (ovx * u) + (ovy * v);
    const bStartOnA = dot / lenSq;
    const u_1 = lineToProject.ToX - osx;
    const v_1 = lineToProject.ToY - osy;
    const dot_1 = (ovx * u_1) + (ovy * v_1);
    const bEndOnA = dot_1 / lenSq;
    if ((bStartOnA < -1E-06) && (bEndOnA < -1E-06)) {
        return undefined;
    }
    else if ((bStartOnA > 1.000001) && (bEndOnA > 1.000001)) {
        return undefined;
    }
    else {
        return [(x_1 = bStartOnA, (x_1 > 0) ? ((x_1 < 1) ? x_1 : 1) : 0), (x_2 = bEndOnA, (x_2 > 0) ? ((x_2 < 1) ? x_2 : 1) : 0)];
    }
}

/**
 * Tries to a line onto another line considered finite.
 * Returns Some Line2D if there is an overlap.
 * Returns None if there is no overlap.
 * Keeps the orientation of the line to project.
 */
export function Euclid_Line2D__Line2D_tryProjectOntoLine_Static(onToLine, lineToProject) {
    const osx = onToLine.FromX;
    const osy = onToLine.FromY;
    const ovx = onToLine.ToX - osx;
    const ovy = onToLine.ToY - osy;
    const lenSq = (ovx * ovx) + (ovy * ovy);
    if (!(lenSq > 1E-12)) {
        failTooSmall("Line2D.projectOntoLineParam", onToLine);
    }
    const u = lineToProject.FromX - osx;
    const v = lineToProject.FromY - osy;
    const dot = (ovx * u) + (ovy * v);
    const bStartOnA = dot / lenSq;
    const u_1 = lineToProject.ToX - osx;
    const v_1 = lineToProject.ToY - osy;
    const dot_1 = (ovx * u_1) + (ovy * v_1);
    const bEndOnA = dot_1 / lenSq;
    if ((bStartOnA < -1E-06) && (bEndOnA < -1E-06)) {
        return undefined;
    }
    else if ((bStartOnA > 1.000001) && (bEndOnA > 1.000001)) {
        return undefined;
    }
    else {
        let st;
        const x_1 = bStartOnA;
        st = ((x_1 > 0) ? ((x_1 < 1) ? x_1 : 1) : 0);
        let en;
        const x_2 = bEndOnA;
        en = ((x_2 > 0) ? ((x_2 < 1) ? x_2 : 1) : 0);
        return Line2D_$ctor_77D16AC0(osx + (ovx * st), osy + (ovy * st), osx + (ovx * en), osy + (ovy * en));
    }
}

/**
 * A fast test tests if two finite 2D lines truly intersect.
 * Does not use a default tolerance for parallel or coincident lines. Just checks within line range and not NaN.
 * Returns false on zero length lines or if parallel lines are touching or overlapping each other.
 */
export function Euclid_Line2D__Line2D_doIntersect_Static(lnA, lnB) {
    const lineA = lnA;
    const lineB = lnB;
    let vAx;
    const ln = lineA;
    vAx = (ln.ToX - ln.FromX);
    let vAy;
    const ln_1 = lineA;
    vAy = (ln_1.ToY - ln_1.FromY);
    let vBx;
    const ln_2 = lineB;
    vBx = (ln_2.ToX - ln_2.FromX);
    let vBy;
    const ln_3 = lineB;
    vBy = (ln_3.ToY - ln_3.FromY);
    const det = (vAx * vBy) - (vAy * vBx);
    const dx = lineB.FromX - lineA.FromX;
    const dy = lineB.FromY - lineA.FromY;
    const t = ((dx * vBy) - (dy * vBx)) / det;
    if ((t > -1E-06) && (t < 1.000001)) {
        const u = ((dx * vAy) - (dy * vAx)) / det;
        if ((u > -1E-06) && (u < 1.000001)) {
            return true;
        }
        else {
            return false;
        }
    }
    else {
        return false;
    }
}

/**
 * Tests if two finite 2D lines intersect or touch.
 * Also returns true if parallel lines are touching or overlapping each other.
 * Also returns true if a zero length lines are at the same location.
 */
export function Euclid_Line2D__Line2D_doIntersectOrOverlap_Static(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5, ln_6, ln_7, a, b, dx, dy, ln_8, pAx, pAy, vAx, ln_9, vAy, ln_10, x_1, y_1, t, vAx_1, vAy_1, u, v, dotV, lenSq, clPtX, clPtY, vx, vy, clPtX_1, clPtY_1, vx_1, vy_1, vX, vY, ln_11, pAx_2, pAy_2, vAx_2, ln_12, vAy_2, ln_13, x_4, y_4, t_1, vAx_3, vAy_3, u_1, v_1, dotV_1, lenSq_1, clPtX_2, clPtY_2, vx_2, vy_2, clPtX_1_1, clPtY_1_1, vx_1_1, vy_1_1, vX_1, vY_1;
    let matchValue;
    const lnA_1 = lnA;
    const lnB_1 = lnB;
    matchValue = XLine2D_getIntersectionParam_40607834(lnA_1.FromX, lnA_1.FromY, lnB_1.FromX, lnB_1.FromY, (ln = lnA_1, ln.ToX - ln.FromX), (ln_1 = lnA_1, ln_1.ToY - ln_1.FromY), (ln_2 = lnB_1, ln_2.ToX - ln_2.FromX), (ln_3 = lnB_1, ln_3.ToY - ln_3.FromY), 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 2: {
            const lineA = lnA;
            const lineB = lnB;
            return XLine2D_doOverlap_199764BB(lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, (ln_4 = lineA, ln_4.ToX - ln_4.FromX), (ln_5 = lineA, ln_5.ToY - ln_5.FromY), (ln_6 = lineB, ln_6.ToX - ln_6.FromX), (ln_7 = lineB, ln_7.ToY - ln_7.FromY), 1E-06);
        }
        case 1:
            return false;
        case 5:
            return ((a = lnA, (b = lnB, (dx = (a.FromX - b.FromX), (dy = (a.FromY - b.FromY), (dx * dx) + (dy * dy)))))) < 1E-12;
        case 3:
            return ((ln_8 = lnB, (pAx = ln_8.FromX, (pAy = ln_8.FromY, (vAx = ((ln_9 = ln_8, ln_9.ToX - ln_9.FromX)), (vAy = ((ln_10 = ln_8, ln_10.ToY - ln_10.FromY)), (x_1 = lnA.FromX, (y_1 = lnA.FromY, (t = ((vAx_1 = vAx, (vAy_1 = vAy, (u = (x_1 - pAx), (v = (y_1 - pAy), (dotV = ((vAx_1 * u) + (vAy_1 * v)), (lenSq = ((vAx_1 * vAx_1) + (vAy_1 * vAy_1)), dotV / lenSq))))))), (t > -1E-06) ? ((t < 1.000001) ? ((clPtX = (pAx + (vAx * t)), (clPtY = (pAy + (vAy * t)), (vx = (clPtX - x_1), (vy = (clPtY - y_1), (vx * vx) + (vy * vy)))))) : ((clPtX_1 = (pAx + vAx), (clPtY_1 = (pAy + vAy), (vx_1 = (clPtX_1 - x_1), (vy_1 = (clPtY_1 - y_1), (vx_1 * vx_1) + (vy_1 * vy_1))))))) : ((vX = (pAx - x_1), (vY = (pAy - y_1), (vX * vX) + (vY * vY))))))))))))) < 1E-12;
        case 4:
            return ((ln_11 = lnA, (pAx_2 = ln_11.FromX, (pAy_2 = ln_11.FromY, (vAx_2 = ((ln_12 = ln_11, ln_12.ToX - ln_12.FromX)), (vAy_2 = ((ln_13 = ln_11, ln_13.ToY - ln_13.FromY)), (x_4 = lnB.FromX, (y_4 = lnB.FromY, (t_1 = ((vAx_3 = vAx_2, (vAy_3 = vAy_2, (u_1 = (x_4 - pAx_2), (v_1 = (y_4 - pAy_2), (dotV_1 = ((vAx_3 * u_1) + (vAy_3 * v_1)), (lenSq_1 = ((vAx_3 * vAx_3) + (vAy_3 * vAy_3)), dotV_1 / lenSq_1))))))), (t_1 > -1E-06) ? ((t_1 < 1.000001) ? ((clPtX_2 = (pAx_2 + (vAx_2 * t_1)), (clPtY_2 = (pAy_2 + (vAy_2 * t_1)), (vx_2 = (clPtX_2 - x_4), (vy_2 = (clPtY_2 - y_4), (vx_2 * vx_2) + (vy_2 * vy_2)))))) : ((clPtX_1_1 = (pAx_2 + vAx_2), (clPtY_1_1 = (pAy_2 + vAy_2), (vx_1_1 = (clPtX_1_1 - x_4), (vy_1_1 = (clPtY_1_1 - y_4), (vx_1_1 * vx_1_1) + (vy_1_1 * vy_1_1))))))) : ((vX_1 = (pAx_2 - x_4), (vY_1 = (pAy_2 - y_4), (vX_1 * vX_1) + (vY_1 * vY_1))))))))))))) < 1E-12;
        default:
            return true;
    }
}

/**
 * A fast intersection of two finite 2D lines.
 * Does not use a default tolerance for parallel or coincident lines.
 * Just checks if both parameters are within line range and not NaN.
 */
export function Euclid_Line2D__Line2D_tryIntersect_Static(lnA, lnB) {
    const lineA = lnA;
    const lineB = lnB;
    const pAx = lineA.FromX;
    const pAy = lineA.FromY;
    let vAx;
    const ln = lineA;
    vAx = (ln.ToX - ln.FromX);
    let vAy;
    const ln_1 = lineA;
    vAy = (ln_1.ToY - ln_1.FromY);
    let vBx;
    const ln_2 = lineB;
    vBx = (ln_2.ToX - ln_2.FromX);
    let vBy;
    const ln_3 = lineB;
    vBy = (ln_3.ToY - ln_3.FromY);
    const det = (vAx * vBy) - (vAy * vBx);
    const dx = lineB.FromX - pAx;
    const dy = lineB.FromY - pAy;
    const t = ((dx * vBy) - (dy * vBx)) / det;
    if ((t >= -1E-06) && (t <= 1.000001)) {
        const u = ((dx * vAy) - (dy * vAx)) / det;
        if ((u >= -1E-06) && (u <= 1.000001)) {
            return Pt_$ctor_7B00E9A0_1(pAx + (t * vAx), pAy + (t * vAy));
        }
        else {
            return undefined;
        }
    }
    else {
        return undefined;
    }
}

/**
 * Tries to get intersection point of two rays (rays are 2D lines extended infinitely).
 */
export function Euclid_Line2D__Line2D_tryIntersectRay_Static(lineA, lineB) {
    const lineA_1 = lineA;
    const lineB_1 = lineB;
    const pAx = lineA_1.FromX;
    const pAy = lineA_1.FromY;
    let vAx;
    const ln = lineA_1;
    vAx = (ln.ToX - ln.FromX);
    let vAy;
    const ln_1 = lineA_1;
    vAy = (ln_1.ToY - ln_1.FromY);
    let vBx;
    const ln_2 = lineB_1;
    vBx = (ln_2.ToX - ln_2.FromX);
    let vBy;
    const ln_3 = lineB_1;
    vBy = (ln_3.ToY - ln_3.FromY);
    const det = (vAx * vBy) - (vAy * vBx);
    const dot = (vAx * vBx) + (vAy * vBy);
    const tan = det / dot;
    if ((Math.abs(tan) > 0.004363350820701567) && (Math.abs(det) > 1E-10)) {
        const dx = lineB_1.FromX - pAx;
        const dy = lineB_1.FromY - pAy;
        const t = ((dx * vBy) - (dy * vBx)) / det;
        return Pt_$ctor_7B00E9A0_1(pAx + (t * vAx), pAy + (t * vAy));
    }
    else {
        return undefined;
    }
}

/**
 * Intersects two finite 2D Lines.
 * Also returns a point if parallel lines are touching or overlapping each other.
 * Also returns a point a zero length lines are at the same location within 1e-6 distance.
 */
export function Euclid_Line2D__Line2D_tryIntersectOrOverlap_Static(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_5, ln_6, p_1, ln_7, ln_8, a, b, dx, dy, ln_9, ln_10, pAx_1, pAy_1, vAx_1, ln_11, vAy_1, ln_12, x_2, y_2, t, vAx_2, vAy_2, u_1, v_1, dotV, lenSq_1, clPtX, clPtY, vx, vy, clPtX_1, clPtY_1, vx_1, vy_1, vX, vY, ln_13, ln_14, pAx_3, pAy_3, vAx_3, ln_15, vAy_3, ln_16, x_5, y_5, t_1, vAx_4, vAy_4, u_2, v_2, dotV_1, lenSq_2, clPtX_2, clPtY_2, vx_2, vy_2, clPtX_1_1, clPtY_1_1, vx_1_1, vy_1_1, vX_1, vY_1, ln_17;
    let matchValue;
    const lineA = lnA;
    const lineB = lnB;
    matchValue = XLine2D_getIntersection_40607834(lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineB, ln_2.ToX - ln_2.FromX), (ln_3 = lineB, ln_3.ToY - ln_3.FromY), 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 1:
            return undefined;
        case 2: {
            let sqDist;
            const nx = -((ln_5 = lnA, ln_5.ToY - ln_5.FromY));
            let ny;
            const ln_4 = lnA;
            ny = (ln_4.ToX - ln_4.FromX);
            const lenSq = (nx * nx) + (ny * ny);
            const u = lnB.FromX - lnA.FromX;
            const v = lnB.FromY - lnA.FromY;
            const dotN = (nx * u) + (ny * v);
            sqDist = ((dotN * dotN) / lenSq);
            if (sqDist < 1E-12) {
                const matchValue_1 = Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(lnA, lnB);
                if (matchValue_1 == null) {
                    return undefined;
                }
                else {
                    const s = matchValue_1[0];
                    const e = matchValue_1[1];
                    return (ln_6 = lnA, (p_1 = ((s + e) * 0.5), Pt_$ctor_7B00E9A0(ln_6.FromX + (((ln_7 = ln_6, ln_7.ToX - ln_7.FromX)) * p_1), ln_6.FromY + (((ln_8 = ln_6, ln_8.ToY - ln_8.FromY)) * p_1))));
                }
            }
            else {
                return undefined;
            }
        }
        case 5:
            if (((a = lnA, (b = lnB, (dx = (a.FromX - b.FromX), (dy = (a.FromY - b.FromY), (dx * dx) + (dy * dy)))))) < 1E-12) {
                return (ln_9 = lnA, Pt_$ctor_7B00E9A0_1(ln_9.FromX, ln_9.FromY));
            }
            else {
                return undefined;
            }
        case 3:
            if (((ln_10 = lnB, (pAx_1 = ln_10.FromX, (pAy_1 = ln_10.FromY, (vAx_1 = ((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)), (vAy_1 = ((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)), (x_2 = lnA.FromX, (y_2 = lnA.FromY, (t = ((vAx_2 = vAx_1, (vAy_2 = vAy_1, (u_1 = (x_2 - pAx_1), (v_1 = (y_2 - pAy_1), (dotV = ((vAx_2 * u_1) + (vAy_2 * v_1)), (lenSq_1 = ((vAx_2 * vAx_2) + (vAy_2 * vAy_2)), dotV / lenSq_1))))))), (t > -1E-06) ? ((t < 1.000001) ? ((clPtX = (pAx_1 + (vAx_1 * t)), (clPtY = (pAy_1 + (vAy_1 * t)), (vx = (clPtX - x_2), (vy = (clPtY - y_2), (vx * vx) + (vy * vy)))))) : ((clPtX_1 = (pAx_1 + vAx_1), (clPtY_1 = (pAy_1 + vAy_1), (vx_1 = (clPtX_1 - x_2), (vy_1 = (clPtY_1 - y_2), (vx_1 * vx_1) + (vy_1 * vy_1))))))) : ((vX = (pAx_1 - x_2), (vY = (pAy_1 - y_2), (vX * vX) + (vY * vY))))))))))))) < 1E-12) {
                return (ln_13 = lnA, Pt_$ctor_7B00E9A0_1(ln_13.FromX, ln_13.FromY));
            }
            else {
                return undefined;
            }
        case 4:
            if (((ln_14 = lnA, (pAx_3 = ln_14.FromX, (pAy_3 = ln_14.FromY, (vAx_3 = ((ln_15 = ln_14, ln_15.ToX - ln_15.FromX)), (vAy_3 = ((ln_16 = ln_14, ln_16.ToY - ln_16.FromY)), (x_5 = lnB.FromX, (y_5 = lnB.FromY, (t_1 = ((vAx_4 = vAx_3, (vAy_4 = vAy_3, (u_2 = (x_5 - pAx_3), (v_2 = (y_5 - pAy_3), (dotV_1 = ((vAx_4 * u_2) + (vAy_4 * v_2)), (lenSq_2 = ((vAx_4 * vAx_4) + (vAy_4 * vAy_4)), dotV_1 / lenSq_2))))))), (t_1 > -1E-06) ? ((t_1 < 1.000001) ? ((clPtX_2 = (pAx_3 + (vAx_3 * t_1)), (clPtY_2 = (pAy_3 + (vAy_3 * t_1)), (vx_2 = (clPtX_2 - x_5), (vy_2 = (clPtY_2 - y_5), (vx_2 * vx_2) + (vy_2 * vy_2)))))) : ((clPtX_1_1 = (pAx_3 + vAx_3), (clPtY_1_1 = (pAy_3 + vAy_3), (vx_1_1 = (clPtX_1_1 - x_5), (vy_1_1 = (clPtY_1_1 - y_5), (vx_1_1 * vx_1_1) + (vy_1_1 * vy_1_1))))))) : ((vX_1 = (pAx_3 - x_5), (vY_1 = (pAy_3 - y_5), (vX_1 * vX_1) + (vY_1 * vY_1))))))))))))) < 1E-12) {
                return (ln_17 = lnB, Pt_$ctor_7B00E9A0_1(ln_17.FromX, ln_17.FromY));
            }
            else {
                return undefined;
            }
        default: {
            const p = matchValue.fields[0];
            return p;
        }
    }
}

/**
 * Finds the closest points between two finite 2D Lines, also works on parallel and overlapping lines.
 */
export function Euclid_Line2D__Line2D_closestPoints_Static(lnA, lnB) {
    return Euclid_Line2D__Line2D_ClosestPoints_4CC2E301(lnA, lnB);
}

/**
 * Finds the parameters of closest points between two finite 2D Lines, also works on parallel and overlapping lines.
 */
export function Euclid_Line2D__Line2D_closestParameters_Static(lnA, lnB) {
    return Euclid_Line2D__Line2D_ClosestParameters_4CC2E301(lnA, lnB);
}

/**
 * Checks if lines are parallel, coincident and overlapping.
 */
export function Euclid_Line2D__Line2D_tryGetOverlap_Static(lnA, lnB) {
    let ln_1, ln_2, ln_4, ln_5, ln_7;
    let va;
    const ln = lnA;
    va = Vc_$ctor_7B00E9A0((ln_1 = ln, ln_1.ToX - ln_1.FromX), (ln_2 = ln, ln_2.ToY - ln_2.FromY));
    let vb;
    const ln_3 = lnB;
    vb = Vc_$ctor_7B00E9A0((ln_4 = ln_3, ln_4.ToX - ln_4.FromX), (ln_5 = ln_3, ln_5.ToY - ln_5.FromY));
    if (((Math.abs(va.X) + Math.abs(va.Y)) < 1E-06) ? true : ((Math.abs(vb.X) + Math.abs(vb.Y)) < 1E-06)) {
        return undefined;
    }
    else {
        let tan;
        const vAx = va.X;
        const vAy = va.Y;
        const vBx = vb.X;
        const vBy = vb.Y;
        const det = (vAx * vBy) - (vAy * vBx);
        const dot = (vAx * vBx) + (vAy * vBy);
        tan = (det / dot);
        if (Math.abs(tan) < 0.004363350820701567) {
            let sqDist;
            const nx = -((ln_7 = lnA, ln_7.ToY - ln_7.FromY));
            let ny;
            const ln_6 = lnA;
            ny = (ln_6.ToX - ln_6.FromX);
            const lenSq = (nx * nx) + (ny * ny);
            const u = lnB.FromX - lnA.FromX;
            const v = lnB.FromY - lnA.FromY;
            const dotN = (nx * u) + (ny * v);
            sqDist = ((dotN * dotN) / lenSq);
            if (sqDist < 1E-09) {
                return Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(lnA, lnB);
            }
            else {
                return undefined;
            }
        }
        else {
            return undefined;
        }
    }
}

/**
 * Computes the squared distance between two finite 2D lines.
 */
export function Euclid_Line2D__Line2D_sqDistanceToLine_Static(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3;
    const lineA = lnA;
    const lineB = lnB;
    return XLine2D_getSqDistance_Z6A6F0C80(lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineB, ln_2.ToX - ln_2.FromX), (ln_3 = lineB, ln_3.ToY - ln_3.FromY));
}

/**
 * Computes the distance between two finite 2D lines.
 */
export function Euclid_Line2D__Line2D_distanceToLine_Static(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3;
    let value;
    const lineA = lnA;
    const lineB = lnB;
    value = XLine2D_getSqDistance_Z6A6F0C80(lineA.FromX, lineA.FromY, lineB.FromX, lineB.FromY, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineB, ln_2.ToX - ln_2.FromX), (ln_3 = lineB, ln_3.ToY - ln_3.FromY));
    return Math.sqrt(value);
}

/**
 * Checks if the two finite 2D lines are touching each other at any of end points
 * within the given tolerance.
 * This will also return TRUE if the lines are touching on both points.
 */
export function Euclid_Line2D__Line2D_isTouchingEndOf_Static(squareTolerance, a, b) {
    let x, y, x_1, y_1, x_2, y_2;
    if ((((x = (a.ToX - b.FromX), (y = (a.ToY - b.FromY), ((x * x) + (y * y)) < squareTolerance))) ? true : ((x_1 = (a.FromX - b.ToX), (y_1 = (a.FromY - b.ToY), ((x_1 * x_1) + (y_1 * y_1)) < squareTolerance)))) ? true : ((x_2 = (a.FromX - b.FromX), (y_2 = (a.FromY - b.FromY), ((x_2 * x_2) + (y_2 * y_2)) < squareTolerance)))) {
        return true;
    }
    else {
        const x_3 = a.ToX - b.ToX;
        const y_3 = a.ToY - b.ToY;
        return ((x_3 * x_3) + (y_3 * y_3)) < squareTolerance;
    }
}

export function Euclid_Line2D__Line2D_ClosestParameterInfinite_6ADE94FD(ln, p) {
    return Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD(ln, p);
}

export function Euclid_Line2D__Line2D_ClosestPointInfinite_6ADE94FD(ln, p) {
    return Euclid_Line2D__Line2D_RayClosestPoint_6ADE94FD(ln, p);
}

export function Euclid_Line2D__Line2D_DistanceSqFromPointInfinite_6ADE94FD(ln, p) {
    return Euclid_Line2D__Line2D_SqDistanceRayPoint_6ADE94FD(ln, p);
}

export function Euclid_Line2D__Line2D_DistanceSqFromPoint_6ADE94FD(ln, p) {
    const ln_1 = ln;
    const pAx = ln_1.FromX;
    const pAy = ln_1.FromY;
    let vAx;
    const ln_2 = ln_1;
    vAx = (ln_2.ToX - ln_2.FromX);
    let vAy;
    const ln_3 = ln_1;
    vAy = (ln_3.ToY - ln_3.FromY);
    const x_1 = p.X;
    const y_1 = p.Y;
    let t;
    const vAx_1 = vAx;
    const vAy_1 = vAy;
    const u = x_1 - pAx;
    const v = y_1 - pAy;
    const dotV = (vAx_1 * u) + (vAy_1 * v);
    const lenSq = (vAx_1 * vAx_1) + (vAy_1 * vAy_1);
    t = (dotV / lenSq);
    if (t > -1E-06) {
        if (t < 1.000001) {
            const clPtX = pAx + (vAx * t);
            const clPtY = pAy + (vAy * t);
            const vx = clPtX - x_1;
            const vy = clPtY - y_1;
            return (vx * vx) + (vy * vy);
        }
        else {
            const clPtX_1 = pAx + vAx;
            const clPtY_1 = pAy + vAy;
            const vx_1 = clPtX_1 - x_1;
            const vy_1 = clPtY_1 - y_1;
            return (vx_1 * vx_1) + (vy_1 * vy_1);
        }
    }
    else {
        const vX = pAx - x_1;
        const vY = pAy - y_1;
        return (vX * vX) + (vY * vY);
    }
}

export function Euclid_Line2D__Line2D_distanceBetweenLines_Static_Z56225FE0(lnA, lnB) {
    return Euclid_Line2D__Line2D_distanceToLine_Static(lnA, lnB);
}

export function Euclid_Line2D__Line2D_projectOn_Static(onToLine, lineToProject) {
    return Euclid_Line2D__Line2D_projectOntoRay_Static(onToLine, lineToProject);
}

export function Euclid_Line2D__Line2D_intersectionParamInfinite_Static_Z56225FE0(_lnA, _lnB) {
    return failObsoleteV30("Line2D.intersectionParamInfinite", "XLine2D.getIntersectionParam");
}

export function Euclid_Line2D__Line2D_intersectionInfinite_Static_Z56225FE0(_lnA, _lnB) {
    return failObsoleteV30("Line2D.intersectionInfinite", "XLine2D.getIntersection");
}

export function Euclid_Line2D__Line2D_intersectionPointInfinite_Static_Z56225FE0(_lnA, _lnB) {
    return failObsoleteV30("Line2D.intersectionPointInfinite", "XLine2D.tryIntersectRay");
}

export function Euclid_Line2D__Line2D_intersectionParam_Static_Z56225FE0(_lnA, _lnB) {
    return failObsoleteV30("Line2D.intersectionParam", "XLine2D.getClosestParameters");
}

export function Euclid_Line2D__Line2D_intersection_Static_Z56225FE0(_lnA, _lnB) {
    return failObsoleteV30("Line2D.intersection", "XLine2D.getClosestPoints");
}

export function Euclid_Line2D__Line2D_intersectionPoint_Static_Z56225FE0(_lnA, _lnB) {
    return failObsoleteV30("Line2D.intersectionPoint", "Line2D.tryIntersect or Line2D.tryIntersectOrOverlap");
}

export function Euclid_Line2D__Line2D_areTouchingAny_Static(tol, a, b) {
    return XLine2D_getEndsTouching_Z44565CE5(a, b, tol);
}

export function Euclid_Line2D__Line2D_areTouchingEither_Static(tol, a, b) {
    return XLine2D_getEndsTouching_Z44565CE5(a, b, tol);
}

