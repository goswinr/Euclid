
import { XLine3D_getSqDistance_Z15A9A3C0, XLine3D_getIntersection_ZA268E31, XLine3D_tryIntersectRay_ZA268E31, XLine3D_doOverlap_6B19E37B, XLine3D_getIntersectionParam_ZA268E31, XLine3D_tryIntersect_ZA268E31, XLine3D_getRayClosestParam_Z1F58440C, XLine3D_getClosestParameters_Z1F58440C, XLine3D_getClosestPoints_Z1F58440C } from "../XLine3D.js";
import { Pnt_$ctor_Z7AD9E565 } from "../Pnt.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "../Pnt.js";
import { failObsoleteV30, failTooSmall, fail, failVertical, failTooSmall2 } from "../EuclidErrors.js";
import { Line3D, Line3D_$ctor_76A78260 } from "../Line3D.js";
import { Vec_$ctor_Z7AD9E565 } from "../Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "../Vec.js";
import { UnitVec_$ctor_Z7AD9E565 } from "../UnitVec.js";
import { item as item_2, setItem, fill } from "../../fable_modules/fable-library-js.5.0.0/Array.js";
import { XLine3D_intersectCone_1D0AE3BB } from "../XLine3D.js";

/**
 * Finds the closest points between two finite 3D Lines, also works on parallel and overlapping lines.
 */
export function Euclid_Line3D__Line3D_ClosestPoints_4CC2E360(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5, ln_6, ln_7, ln_8, ln_9, pAx, pAy, pAz, vAx, ln_10, vAy, ln_11, vAz, ln_12, t, vAx_1, vAy_1, vAz_1, ln_13, pAx_2, pAy_2, pAz_2, vAx_2, ln_14, vAy_2, ln_15, vAz_2, ln_16, t_1, vAx_3, vAy_3, vAz_3, ln_17;
    let matchValue;
    const lineA = lnA;
    const lineB = lnB;
    matchValue = XLine3D_getClosestPoints_Z1F58440C(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineA, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB, ln_3.ToX - ln_3.FromX), (ln_4 = lineB, ln_4.ToY - ln_4.FromY), (ln_5 = lineB, ln_5.ToZ - ln_5.FromZ), 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 1:
            return [matchValue.fields[0], matchValue.fields[1]];
        case 2:
            return [matchValue.fields[0], matchValue.fields[1]];
        case 3:
            return [matchValue.fields[0], matchValue.fields[1]];
        case 6:
            return [(ln_6 = lnA, Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ)), (ln_7 = lnB, Pnt_$ctor_Z7AD9E565(ln_7.FromX, ln_7.FromY, ln_7.FromZ))];
        case 4:
            return [(ln_8 = lnA, Pnt_$ctor_Z7AD9E565(ln_8.FromX, ln_8.FromY, ln_8.FromZ)), (ln_9 = lnB, (pAx = ln_9.FromX, (pAy = ln_9.FromY, (pAz = ln_9.FromZ, (vAx = ((ln_10 = ln_9, ln_10.ToX - ln_10.FromX)), (vAy = ((ln_11 = ln_9, ln_11.ToY - ln_11.FromY)), (vAz = ((ln_12 = ln_9, ln_12.ToZ - ln_12.FromZ)), (t = ((vAx_1 = vAx, (vAy_1 = vAy, (vAz_1 = vAz, (((vAx_1 * (lnA.FromX - pAx)) + (vAy_1 * (lnA.FromY - pAy))) + (vAz_1 * (lnA.FromZ - pAz))) / (((vAx_1 * vAx_1) + (vAy_1 * vAy_1)) + (vAz_1 * vAz_1)))))), (t > -1E-06) ? ((t < 1.000001) ? Pnt_$ctor_Z7AD9E565(pAx + (vAx * t), pAy + (vAy * t), pAz + (vAz * t)) : Pnt_$ctor_Z7AD9E565(pAx + vAx, pAy + vAy, pAz + vAz)) : Pnt_$ctor_Z7AD9E565(pAx, pAy, pAz)))))))))];
        case 5:
            return [(ln_13 = lnA, (pAx_2 = ln_13.FromX, (pAy_2 = ln_13.FromY, (pAz_2 = ln_13.FromZ, (vAx_2 = ((ln_14 = ln_13, ln_14.ToX - ln_14.FromX)), (vAy_2 = ((ln_15 = ln_13, ln_15.ToY - ln_15.FromY)), (vAz_2 = ((ln_16 = ln_13, ln_16.ToZ - ln_16.FromZ)), (t_1 = ((vAx_3 = vAx_2, (vAy_3 = vAy_2, (vAz_3 = vAz_2, (((vAx_3 * (lnB.FromX - pAx_2)) + (vAy_3 * (lnB.FromY - pAy_2))) + (vAz_3 * (lnB.FromZ - pAz_2))) / (((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3)))))), (t_1 > -1E-06) ? ((t_1 < 1.000001) ? Pnt_$ctor_Z7AD9E565(pAx_2 + (vAx_2 * t_1), pAy_2 + (vAy_2 * t_1), pAz_2 + (vAz_2 * t_1)) : Pnt_$ctor_Z7AD9E565(pAx_2 + vAx_2, pAy_2 + vAy_2, pAz_2 + vAz_2)) : Pnt_$ctor_Z7AD9E565(pAx_2, pAy_2, pAz_2))))))))), (ln_17 = lnB, Pnt_$ctor_Z7AD9E565(ln_17.FromX, ln_17.FromY, ln_17.FromZ))];
        default: {
            const p = matchValue.fields[0];
            return [p, p];
        }
    }
}

/**
 * Finds the parameters of closest points between two finite 3D Lines, also works on parallel and overlapping lines.
 */
export function Euclid_Line3D__Line3D_ClosestParameters_4CC2E360(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5, ln_6, x_2, vAx, ln_7, vAy, ln_8, vAz, ln_9, ln_10, x_5, vAx_1, ln_11, vAy_1, ln_12, vAz_1, ln_13;
    let matchValue;
    const lineA = lnA;
    const lineB = lnB;
    matchValue = XLine3D_getClosestParameters_Z1F58440C(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineA, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB, ln_3.ToX - ln_3.FromX), (ln_4 = lineB, ln_4.ToY - ln_4.FromY), (ln_5 = lineB, ln_5.ToZ - ln_5.FromZ), 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 1:
            return [matchValue.fields[0], matchValue.fields[1]];
        case 2:
            return [matchValue.fields[0], matchValue.fields[1]];
        case 3:
            return [matchValue.fields[0], matchValue.fields[1]];
        case 6:
            return [0, 0];
        case 4:
            return [0, (ln_6 = lnB, (x_2 = ((vAx = ((ln_7 = ln_6, ln_7.ToX - ln_7.FromX)), (vAy = ((ln_8 = ln_6, ln_8.ToY - ln_8.FromY)), (vAz = ((ln_9 = ln_6, ln_9.ToZ - ln_9.FromZ)), (((vAx * (lnA.FromX - ln_6.FromX)) + (vAy * (lnA.FromY - ln_6.FromY))) + (vAz * (lnA.FromZ - ln_6.FromZ))) / (((vAx * vAx) + (vAy * vAy)) + (vAz * vAz)))))), (x_2 > 0) ? ((x_2 < 1) ? x_2 : 1) : 0))];
        case 5:
            return [(ln_10 = lnA, (x_5 = ((vAx_1 = ((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)), (vAy_1 = ((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)), (vAz_1 = ((ln_13 = ln_10, ln_13.ToZ - ln_13.FromZ)), (((vAx_1 * (lnB.FromX - ln_10.FromX)) + (vAy_1 * (lnB.FromY - ln_10.FromY))) + (vAz_1 * (lnB.FromZ - ln_10.FromZ))) / (((vAx_1 * vAx_1) + (vAy_1 * vAy_1)) + (vAz_1 * vAz_1)))))), (x_5 > 0) ? ((x_5 < 1) ? x_5 : 1) : 0)), 0];
        default:
            return [matchValue.fields[0], matchValue.fields[1]];
    }
}

/**
 * Finds the closest approach parameters of two infinite rays (Line3D treated as rays).
 */
export function Euclid_Line3D__Line3D_RayClosestParameters_4CC2E360(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5;
    let matchValue;
    const lineA = lnA;
    const lineB = lnB;
    matchValue = XLine3D_getRayClosestParam_Z1F58440C(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineA, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB, ln_3.ToX - ln_3.FromX), (ln_4 = lineB, ln_4.ToY - ln_4.FromY), (ln_5 = lineB, ln_5.ToZ - ln_5.FromZ), 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 1:
        case 2:
        case 3:
        case 4:
            return undefined;
        default:
            return [matchValue.fields[0], matchValue.fields[1]];
    }
}

/**
 * Finds the closest approach points of two infinite rays (Line3D treated as rays).
 */
export function Euclid_Line3D__Line3D_RayClosestPoints_4CC2E360(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5, ln_6, p, ln_7, ln_8, ln_9, ln_10, p_1, ln_11, ln_12, ln_13;
    let matchValue;
    const lineA = lnA;
    const lineB = lnB;
    matchValue = XLine3D_getRayClosestParam_Z1F58440C(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineA, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB, ln_3.ToX - ln_3.FromX), (ln_4 = lineB, ln_4.ToY - ln_4.FromY), (ln_5 = lineB, ln_5.ToZ - ln_5.FromZ), 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 1:
        case 2:
        case 3:
        case 4:
            return undefined;
        default:
            return [(ln_6 = lnA, (p = matchValue.fields[0], Pnt_$ctor_Z7AD9E565_1(ln_6.FromX + (((ln_7 = ln_6, ln_7.ToX - ln_7.FromX)) * p), ln_6.FromY + (((ln_8 = ln_6, ln_8.ToY - ln_8.FromY)) * p), ln_6.FromZ + (((ln_9 = ln_6, ln_9.ToZ - ln_9.FromZ)) * p)))), (ln_10 = lnB, (p_1 = matchValue.fields[1], Pnt_$ctor_Z7AD9E565_1(ln_10.FromX + (((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)) * p_1), ln_10.FromY + (((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)) * p_1), ln_10.FromZ + (((ln_13 = ln_10, ln_13.ToZ - ln_13.FromZ)) * p_1))))];
    }
}

/**
 * Assumes Line3D to be an infinite ray.
 * Returns square distance from point to ray.
 * Fails on curves shorter than 1e-6 units. (ln.SqDistanceFromPoint does not.)
 */
export function Euclid_Line3D__Line3D_SqDistanceRayPoint_Z394ECE4D(ln, p) {
    const lnFromX = ln.FromX;
    const lnFromY = ln.FromY;
    const lnFromZ = ln.FromZ;
    const x = lnFromX - ln.ToX;
    const y = lnFromY - ln.ToY;
    const z = lnFromZ - ln.ToZ;
    const lenSq = ((x * x) + (y * y)) + (z * z);
    if (!(lenSq > 1E-12)) {
        failTooSmall2("Line3D.SqDistanceRayPoint", ln, p);
    }
    const t = (((x * (lnFromX - p.X)) + (y * (lnFromY - p.Y))) + (z * (lnFromZ - p.Z))) / lenSq;
    const u$0027 = (lnFromX - (x * t)) - p.X;
    const v$0027 = (lnFromY - (y * t)) - p.Y;
    const w$0027 = (lnFromZ - (z * t)) - p.Z;
    return ((u$0027 * u$0027) + (v$0027 * v$0027)) + (w$0027 * w$0027);
}

/**
 * Check if two 3D lines are not equal within a given tolerance.
 * Use a tolerance of 0.0 to check if the two lines are not exactly equal.
 */
export function Euclid_Line3D__Line3D_notEquals_Static(tol, a, b) {
    if (((((Math.abs(a.FromX - b.FromX) > tol) ? true : (Math.abs(a.FromY - b.FromY) > tol)) ? true : (Math.abs(a.FromZ - b.FromZ) > tol)) ? true : (Math.abs(a.ToX - b.ToX) > tol)) ? true : (Math.abs(a.ToY - b.ToY) > tol)) {
        return true;
    }
    else {
        return Math.abs(a.ToZ - b.ToZ) > tol;
    }
}

/**
 * Finds the closest points between two finite 3D Lines, also works on parallel and overlapping lines.
 */
export function Euclid_Line3D__Line3D_closestPoints_Static(lnA, lnB) {
    return Euclid_Line3D__Line3D_ClosestPoints_4CC2E360(lnA, lnB);
}

/**
 * Finds the parameters of closest points between two finite 3D Lines, also works on parallel and overlapping lines.
 */
export function Euclid_Line3D__Line3D_closestParameters_Static(lnA, lnB) {
    return Euclid_Line3D__Line3D_ClosestParameters_4CC2E360(lnA, lnB);
}

/**
 * Finds the closest approach parameters of two infinite rays (Line3D treated as rays).
 */
export function Euclid_Line3D__Line3D_rayClosestParameters_Static(lnA, lnB) {
    return Euclid_Line3D__Line3D_RayClosestParameters_4CC2E360(lnA, lnB);
}

/**
 * Finds the closest approach points of two infinite rays (Line3D treated as rays).
 */
export function Euclid_Line3D__Line3D_rayClosestPoints_Static(lnA, lnB) {
    return Euclid_Line3D__Line3D_RayClosestPoints_4CC2E360(lnA, lnB);
}

/**
 * Offset line parallel to XY-Plane to left side in line direction.
 * Z values are not changed.
 * Fails on vertical lines or lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
 * If amount is 0.0 no offset is computed and the input line is returned.
 */
export function Euclid_Line3D__Line3D_offsetXY_Static(amount, ln) {
    if (amount === 0) {
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
            failVertical("Line3D.offsetXY", ln);
        }
        const ox = (-y * amount) / lenXY;
        const oy = (x * amount) / lenXY;
        return Line3D_$ctor_76A78260(ln.FromX + ox, ln.FromY + oy, ln.FromZ, ln.ToX + ox, ln.ToY + oy, ln.ToZ);
    }
}

/**
 * Offsets a 3D line by two given distances.
 * The fist distance (distHorizontal) is applied in in X-Y plane.
 * The second distance (distNormal) is applied perpendicular to the line (made by the two 3D points)
 * and perpendicular to the horizontal offset direction.
 * This is in World.Z direction if both points are at the same Z level.
 * If points are closer than 1e-6 units the World.Xaxis is used
 * as first direction and World Z-axis as second direction.
 */
export function Euclid_Line3D__Line3D_offset_Static(distHorizontal, distNormal, l) {
    let ln_1, ln_2, ln_3;
    let v;
    const ln = l;
    v = Vec_$ctor_Z7AD9E565((ln_1 = ln, ln_1.ToX - ln_1.FromX), (ln_2 = ln, ln_2.ToY - ln_2.FromY), (ln_3 = ln, ln_3.ToZ - ln_3.FromZ));
    let normHor;
    let v_2;
    const a = v;
    const b = Vec_$ctor_Z7AD9E565_1(0, 0, 1);
    v_2 = Vec_$ctor_Z7AD9E565((a.Y * b.Z) - (a.Z * b.Y), (a.Z * b.X) - (a.X * b.Z), (a.X * b.Y) - (a.Y * b.X));
    let l_1;
    const v_3 = v_2;
    l_1 = (((v_3.X * v_3.X) + (v_3.Y * v_3.Y)) + (v_3.Z * v_3.Z));
    if (l_1 < 1E-12) {
        normHor = UnitVec_$ctor_Z7AD9E565(1, 0, 0);
    }
    else {
        const f = 1 / Math.sqrt(l_1);
        normHor = UnitVec_$ctor_Z7AD9E565(v_2.X * f, v_2.Y * f, v_2.Z * f);
    }
    let normFree;
    let v_5;
    const a_1 = v;
    const b_1 = normHor;
    v_5 = Vec_$ctor_Z7AD9E565_1((a_1.Y * b_1.Z) - (a_1.Z * b_1.Y), (a_1.Z * b_1.X) - (a_1.X * b_1.Z), (a_1.X * b_1.Y) - (a_1.Y * b_1.X));
    let l_2;
    const v_6 = v_5;
    l_2 = (((v_6.X * v_6.X) + (v_6.Y * v_6.Y)) + (v_6.Z * v_6.Z));
    if (l_2 < 1E-12) {
        normFree = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    }
    else {
        const f_1 = 1 / Math.sqrt(l_2);
        normFree = UnitVec_$ctor_Z7AD9E565(v_5.X * f_1, v_5.Y * f_1, v_5.Z * f_1);
    }
    const ln_6 = l;
    let v_9;
    let a_4;
    const f_2 = distHorizontal;
    const a_2 = normHor;
    a_4 = Vec_$ctor_Z7AD9E565(a_2.X * f_2, a_2.Y * f_2, a_2.Z * f_2);
    let b_2;
    const f_3 = distNormal;
    const a_3 = normFree;
    b_2 = Vec_$ctor_Z7AD9E565(a_3.X * f_3, a_3.Y * f_3, a_3.Z * f_3);
    v_9 = Vec_$ctor_Z7AD9E565(a_4.X + b_2.X, a_4.Y + b_2.Y, a_4.Z + b_2.Z);
    return Line3D_$ctor_76A78260(ln_6.FromX + v_9.X, ln_6.FromY + v_9.Y, ln_6.FromZ + v_9.Z, ln_6.ToX + v_9.X, ln_6.ToY + v_9.Y, ln_6.ToZ + v_9.Z);
}

/**
 * Divides a 3D line into given amount of segments.
 * Returns an array of 3D points of length: segment count + 1.
 * Includes start and endpoint of line.
 */
export function Euclid_Line3D__Line3D_divide_Static(segments, ln) {
    let ln_1, ln_2, ln_6, ln_7;
    if (segments < 1) {
        fail(`Line3D.divide: segments must be at least 1, was ${segments}`);
    }
    if (segments === 1) {
        return [(ln_1 = ln, Pnt_$ctor_Z7AD9E565(ln_1.FromX, ln_1.FromY, ln_1.FromZ)), (ln_2 = ln, Pnt_$ctor_Z7AD9E565(ln_2.ToX, ln_2.ToY, ln_2.ToZ))];
    }
    else {
        let x;
        const ln_3 = ln;
        x = (ln_3.ToX - ln_3.FromX);
        let y;
        const ln_4 = ln;
        y = (ln_4.ToY - ln_4.FromY);
        let z;
        const ln_5 = ln;
        z = (ln_5.ToZ - ln_5.FromZ);
        const r = fill(new Array(segments + 1), 0, segments + 1, Pnt_$ctor_Z7AD9E565(0, 0, 0));
        setItem(r, 0, (ln_6 = ln, Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ)));
        for (let i = 1; i <= (segments - 1); i++) {
            const t = i / segments;
            setItem(r, i, Pnt_$ctor_Z7AD9E565_1(ln.FromX + (x * t), ln.FromY + (y * t), ln.FromZ + (z * t)));
        }
        setItem(r, segments, (ln_7 = ln, Pnt_$ctor_Z7AD9E565(ln_7.ToX, ln_7.ToY, ln_7.ToZ)));
        return r;
    }
}

/**
 * Divides a 3D line into as many as segments as possible respecting the minimum segment length.
 * Returned Array includes start and endpoint of line.
 * The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
 * That means in an edge case there are more segments returned, not fewer.
 */
export function Euclid_Line3D__Line3D_divideMinLength_Static(minSegmentLength, ln) {
    let len;
    const ln_1 = ln;
    let x;
    const ln_2 = ln_1;
    x = (ln_2.ToX - ln_2.FromX);
    let y;
    const ln_3 = ln_1;
    y = (ln_3.ToY - ln_3.FromY);
    let z;
    const ln_4 = ln_1;
    z = (ln_4.ToZ - ln_4.FromZ);
    len = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (len < 1E-06) {
        fail(`Line3D.divideMinLength: line length ${len} is too small.`);
    }
    if (len < minSegmentLength) {
        fail(`Line3D.divideMinLength: line length ${len} is smaller than minSegmentLength ${minSegmentLength}`);
    }
    return Euclid_Line3D__Line3D_divide_Static(~~(len / (minSegmentLength * 1.000001)), ln);
}

/**
 * Divides a 3D line into as few as segments as possible respecting the maximum segment length.
 * Returned Array includes start and endpoint of line.
 * The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical  errors.
 * That means in an edge case there are fewer segments returned, not more.
 */
export function Euclid_Line3D__Line3D_divideMaxLength_Static(maxSegmentLength, ln) {
    let len;
    const ln_1 = ln;
    let x;
    const ln_2 = ln_1;
    x = (ln_2.ToX - ln_2.FromX);
    let y;
    const ln_3 = ln_1;
    y = (ln_3.ToY - ln_3.FromY);
    let z;
    const ln_4 = ln_1;
    z = (ln_4.ToZ - ln_4.FromZ);
    len = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (len < 1E-06) {
        fail(`Line3D.divideMaxLength: line length ${len} is too small.`);
    }
    if (maxSegmentLength < 1E-06) {
        fail(`Line3D.divideMaxLength: maxSegmentLength must be greater than 0.0, was ${maxSegmentLength}`);
    }
    return Euclid_Line3D__Line3D_divide_Static(~~((len / maxSegmentLength) * 0.999999) + 1, ln);
}

/**
 * Divides a 3D line into given amount of segments.
 * Includes a gap between the segments. But not at the start or end.
 * Returns an array of 3D lines.
 * Returns an empty array if the length of the line is less than gap-size x segment-count-minus-1.
 */
export function Euclid_Line3D__Line3D_split_Static(gap, segments, ln) {
    let ln_2, ln_3, ln_4, pt_1, ln_7, ln_6;
    let v;
    const ln_1 = ln;
    v = Vec_$ctor_Z7AD9E565((ln_2 = ln_1, ln_2.ToX - ln_2.FromX), (ln_3 = ln_1, ln_3.ToY - ln_3.FromY), (ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ));
    let len;
    const v_1 = v;
    len = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
    if (segments <= 0) {
        fail(`Line3D.split: segments must be at least 1, was ${segments}`);
    }
    if (len < 1E-06) {
        fail(`Line3D.split: line length ${len} is too small.`);
    }
    const segLen = (len - (gap * (segments - 1))) / segments;
    if (!(segLen > 1E-12)) {
        return [];
    }
    else {
        const lns = fill(new Array(segments), 0, segments, new Line3D(0, 0, 0, 0, 0, 0));
        const vx = v.X;
        const vy = v.Y;
        const vz = v.Z;
        const x_1 = ln.FromX;
        const y = ln.FromY;
        const z = ln.FromZ;
        for (let i = 0; i <= (segments - 1); i++) {
            const g = i;
            const sf = ((g * segLen) + (g * gap)) / len;
            const ef = (((i + 1) * segLen) + (g * gap)) / len;
            const xs = x_1 + (vx * sf);
            const ys = y + (vy * sf);
            const zs = z + (vz * sf);
            const xe = x_1 + (vx * ef);
            const ye = y + (vy * ef);
            const ze = z + (vz * ef);
            setItem(lns, i, Line3D_$ctor_76A78260(xs, ys, zs, xe, ye, ze));
        }
        setItem(lns, segments - 1, (pt_1 = ((ln_7 = ln, Pnt_$ctor_Z7AD9E565(ln_7.ToX, ln_7.ToY, ln_7.ToZ))), (ln_6 = item_2(segments - 1, lns), Line3D_$ctor_76A78260(ln_6.FromX, ln_6.FromY, ln_6.FromZ, pt_1.X, pt_1.Y, pt_1.Z))));
        return lns;
    }
}

/**
 * Divides a 3D line into as many as segments as possible respecting the minimum segment length and the gap.
 * Includes a gap between the segments. But not at the start or end.
 * Returns an array ofe3D lines
 * The input minSegmentLength is multiplied by factor 1.000001 of to avoid numerical errors.
 * That means in an edge case there are more segments returned, not fewer.
 */
export function Euclid_Line3D__Line3D_splitMinLength_Static(gap, minSegmentLength, ln) {
    let len;
    const ln_1 = ln;
    let x;
    const ln_2 = ln_1;
    x = (ln_2.ToX - ln_2.FromX);
    let y;
    const ln_3 = ln_1;
    y = (ln_3.ToY - ln_3.FromY);
    let z;
    const ln_4 = ln_1;
    z = (ln_4.ToZ - ln_4.FromZ);
    len = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (len < minSegmentLength) {
        fail(`Line3D.splitMinLength: line length ${len} is smaller than minSegmentLength ${minSegmentLength}`);
    }
    if (minSegmentLength < 1E-06) {
        fail(`Line3D.splitMinLength: minSegmentLength must be greater than 0.0, was ${minSegmentLength}`);
    }
    return Euclid_Line3D__Line3D_split_Static(gap, ~~((len + gap) / ((minSegmentLength + gap) * 1.000001)), ln);
}

/**
 * Divides a 3D line into as few as segments as possible respecting the maximum segment length and the gap.
 * Includes a gap between the segments. But not at the start or end.
 * Returns an array ofe3D lines
 * The input maxSegmentLength is multiplied by factor 0.999999 of to avoid numerical errors.
 * That means in an edge case there are fewer segments returned, not more.
 */
export function Euclid_Line3D__Line3D_splitMaxLength_Static(gap, maxSegmentLength, ln) {
    let len;
    const ln_1 = ln;
    let x;
    const ln_2 = ln_1;
    x = (ln_2.ToX - ln_2.FromX);
    let y;
    const ln_3 = ln_1;
    y = (ln_3.ToY - ln_3.FromY);
    let z;
    const ln_4 = ln_1;
    z = (ln_4.ToZ - ln_4.FromZ);
    len = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (maxSegmentLength < 1E-06) {
        fail(`Line3D.splitMaxLength: maxSegmentLength must be greater than 0.0, was ${maxSegmentLength}`);
    }
    if (len < 1E-06) {
        fail(`Line3D.splitMaxLength: line length ${len} is too small.`);
    }
    return Euclid_Line3D__Line3D_split_Static(gap, ~~(((len + gap) / (maxSegmentLength + gap)) * 0.999999) + 1, ln);
}

/**
 * Divides a 3D line into segments of given length.
 * Includes start and end point.
 * If the line length is smaller than the given distance just the start and end point is returned.
 * Adds end point only if there is a remainder bigger than 0.1% of the segment length.
 */
export function Euclid_Line3D__Line3D_divideEvery_Static(dist, l) {
    let ln, x, ln_1, y, ln_2, z, ln_3, ln_4, ln_5, ln_6, ln_11;
    const div = ((ln = l, (x = ((ln_1 = ln, ln_1.ToX - ln_1.FromX)), (y = ((ln_2 = ln, ln_2.ToY - ln_2.FromY)), (z = ((ln_3 = ln, ln_3.ToZ - ln_3.FromZ)), Math.sqrt(((x * x) + (y * y)) + (z * z))))))) / dist;
    const floor = Math.floor(div);
    if (floor === 0) {
        const pts = [];
        void (pts.push((ln_4 = l, Pnt_$ctor_Z7AD9E565(ln_4.FromX, ln_4.FromY, ln_4.FromZ))));
        void (pts.push((ln_5 = l, Pnt_$ctor_Z7AD9E565(ln_5.ToX, ln_5.ToY, ln_5.ToZ))));
        return pts;
    }
    else {
        const step = 1 / floor;
        const count = ~~floor | 0;
        const pts_1 = [];
        void (pts_1.push((ln_6 = l, Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ))));
        for (let i = 1; i <= count; i++) {
            let ln_8, ln_9, ln_10;
            let item;
            const ln_7 = l;
            const p = step * i;
            item = Pnt_$ctor_Z7AD9E565_1(ln_7.FromX + (((ln_8 = ln_7, ln_8.ToX - ln_8.FromX)) * p), ln_7.FromY + (((ln_9 = ln_7, ln_9.ToY - ln_9.FromY)) * p), ln_7.FromZ + (((ln_10 = ln_7, ln_10.ToZ - ln_10.FromZ)) * p));
            void (pts_1.push(item));
        }
        if ((div - floor) > 0.001) {
            void (pts_1.push((ln_11 = l, Pnt_$ctor_Z7AD9E565(ln_11.ToX, ln_11.ToY, ln_11.ToZ))));
        }
        return pts_1;
    }
}

/**
 * Divides a 3D line into segments of given length.
 * Excludes start and end point.
 * If the line length is smaller than the given distance an empty array is returned.
 * Adds last div point before end only if there is a remainder bigger than 0.1% of the segment length.
 */
export function Euclid_Line3D__Line3D_divideInsideEvery_Static(dist, l) {
    let ln, x, ln_1, y, ln_2, z, ln_3, ln_9, ln_10, ln_11;
    const div = ((ln = l, (x = ((ln_1 = ln, ln_1.ToX - ln_1.FromX)), (y = ((ln_2 = ln, ln_2.ToY - ln_2.FromY)), (z = ((ln_3 = ln, ln_3.ToZ - ln_3.FromZ)), Math.sqrt(((x * x) + (y * y)) + (z * z))))))) / dist;
    const floor = Math.floor(div);
    if (floor === 0) {
        return [];
    }
    else {
        const step = 1 / floor;
        const count = ~~floor | 0;
        const pts = [];
        for (let i = 1; i <= (count - 1); i++) {
            let ln_5, ln_6, ln_7;
            let item;
            const ln_4 = l;
            const p = step * i;
            item = Pnt_$ctor_Z7AD9E565_1(ln_4.FromX + (((ln_5 = ln_4, ln_5.ToX - ln_5.FromX)) * p), ln_4.FromY + (((ln_6 = ln_4, ln_6.ToY - ln_6.FromY)) * p), ln_4.FromZ + (((ln_7 = ln_4, ln_7.ToZ - ln_7.FromZ)) * p));
            void (pts.push(item));
        }
        if ((div - floor) > 0.001) {
            let item_1;
            const ln_8 = l;
            const p_1 = step * floor;
            item_1 = Pnt_$ctor_Z7AD9E565_1(ln_8.FromX + (((ln_9 = ln_8, ln_9.ToX - ln_9.FromX)) * p_1), ln_8.FromY + (((ln_10 = ln_8, ln_10.ToY - ln_10.FromY)) * p_1), ln_8.FromZ + (((ln_11 = ln_8, ln_11.ToZ - ln_11.FromZ)) * p_1));
            void (pts.push(item_1));
        }
        return pts;
    }
}

/**
 * Intersects a ray with an infinite double cone that has its axis on the Z-axis.
 */
export function Euclid_Line3D__Line3D_intersectCone_Static_1D0AE3BB(ray, coneRadius, coneBaseZ, coneTipZ) {
    const matchValue = XLine3D_intersectCone_1D0AE3BB(ray, coneRadius, coneBaseZ, coneTipZ);
    switch (matchValue.tag) {
        case 1:
            return undefined;
        case 2: {
            const t = matchValue.fields[0];
            return [t, t];
        }
        case 3:
            return [matchValue.fields[0], matchValue.fields[1]];
        default:
            return undefined;
    }
}

/**
 * Project a 3D line onto another line considered infinite in both directions.
 * Returns the start and end parameters of the projected line on the target line.
 * Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
 */
export function Euclid_Line3D__Line3D_projectOntoRayParam_Static(rayToProjectOnto, lineToProject) {
    const osx = rayToProjectOnto.FromX;
    const osy = rayToProjectOnto.FromY;
    const osz = rayToProjectOnto.FromZ;
    const ovx = rayToProjectOnto.ToX - osx;
    const ovy = rayToProjectOnto.ToY - osy;
    const ovz = rayToProjectOnto.ToZ - osz;
    const lenSq = ((ovx * ovx) + (ovy * ovy)) + (ovz * ovz);
    if (!(lenSq > 1E-12)) {
        failTooSmall("Line3D.projectOntoRayParam", rayToProjectOnto);
    }
    return [(((ovx * (lineToProject.FromX - osx)) + (ovy * (lineToProject.FromY - osy))) + (ovz * (lineToProject.FromZ - osz))) / lenSq, (((ovx * (lineToProject.ToX - osx)) + (ovy * (lineToProject.ToY - osy))) + (ovz * (lineToProject.ToZ - osz))) / lenSq];
}

/**
 * Project a 3D line onto another line considered infinite in both directions.
 * Returns the projected line.
 * Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
 */
export function Euclid_Line3D__Line3D_projectOntoRay_Static(rayToProjectOnto, lineToProject) {
    const osx = rayToProjectOnto.FromX;
    const osy = rayToProjectOnto.FromY;
    const osz = rayToProjectOnto.FromZ;
    const ovx = rayToProjectOnto.ToX - osx;
    const ovy = rayToProjectOnto.ToY - osy;
    const ovz = rayToProjectOnto.ToZ - osz;
    const lenSq = ((ovx * ovx) + (ovy * ovy)) + (ovz * ovz);
    if (!(lenSq > 1E-12)) {
        failTooSmall("Line3D.projectOntoRay", rayToProjectOnto);
    }
    const s = (((ovx * (lineToProject.FromX - osx)) + (ovy * (lineToProject.FromY - osy))) + (ovz * (lineToProject.FromZ - osz))) / lenSq;
    const e = (((ovx * (lineToProject.ToX - osx)) + (ovy * (lineToProject.ToY - osy))) + (ovz * (lineToProject.ToZ - osz))) / lenSq;
    return Line3D_$ctor_76A78260(osx + (ovx * s), osy + (ovy * s), osz + (ovz * s), osx + (ovx * e), osy + (ovy * e), osz + (ovz * e));
}

/**
 * Tries to project a 3D line onto another line considered finite.
 * Returns None if there is no overlap.
 * Returns Some (startParam, endParam) if there is an overlap.
 * The parameters are between 0.0 and 1.0 on the target line.
 * The first parameter is from the start of the line to project.
 * The second parameter is from the end of the line to project.
 * So if the first parameter is bigger than the second, the lines are oriented in opposite direction.
 * Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
 */
export function Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine, lineToProject) {
    let x_1, x_2;
    const osx = onToLine.FromX;
    const osy = onToLine.FromY;
    const osz = onToLine.FromZ;
    const ovx = onToLine.ToX - osx;
    const ovy = onToLine.ToY - osy;
    const ovz = onToLine.ToZ - osz;
    const lenSq = ((ovx * ovx) + (ovy * ovy)) + (ovz * ovz);
    if (!(lenSq > 1E-12)) {
        failTooSmall("Line3D.tryProjectOntoLineParam", onToLine);
    }
    const bStartOnA = (((ovx * (lineToProject.FromX - osx)) + (ovy * (lineToProject.FromY - osy))) + (ovz * (lineToProject.FromZ - osz))) / lenSq;
    const bEndOnA = (((ovx * (lineToProject.ToX - osx)) + (ovy * (lineToProject.ToY - osy))) + (ovz * (lineToProject.ToZ - osz))) / lenSq;
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
 * Tries to project a 3D line onto another line considered finite.
 * Returns Some Line3D if there is an overlap.
 * Returns None if there is no overlap.
 * Keeps the orientation of the line to project.
 * Fails on lines shorter than UtilEuclid.zeroLengthTolerance (1e-12).
 */
export function Euclid_Line3D__Line3D_tryProjectOntoLine_Static(onToLine, lineToProject) {
    const osx = onToLine.FromX;
    const osy = onToLine.FromY;
    const osz = onToLine.FromZ;
    const ovx = onToLine.ToX - osx;
    const ovy = onToLine.ToY - osy;
    const ovz = onToLine.ToZ - osz;
    const lenSq = ((ovx * ovx) + (ovy * ovy)) + (ovz * ovz);
    if (!(lenSq > 1E-12)) {
        failTooSmall("Line3D.tryProjectOntoLine", onToLine);
    }
    const bStartOnA = (((ovx * (lineToProject.FromX - osx)) + (ovy * (lineToProject.FromY - osy))) + (ovz * (lineToProject.FromZ - osz))) / lenSq;
    const bEndOnA = (((ovx * (lineToProject.ToX - osx)) + (ovy * (lineToProject.ToY - osy))) + (ovz * (lineToProject.ToZ - osz))) / lenSq;
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
        return Line3D_$ctor_76A78260(osx + (ovx * st), osy + (ovy * st), osz + (ovz * st), osx + (ovx * en), osy + (ovy * en), osz + (ovz * en));
    }
}

/**
 * A fast test to check if two finite 3D lines truly intersect (or come very close in the skew case).
 * Uses the default tolerance for parallel lines (0.25 degrees) and maximum skew distance (1e-6).
 * Returns false on zero length lines or if lines are parallel, apart, or don't intersect within their finite segments.
 */
export function Euclid_Line3D__Line3D_doIntersect_Static(lnA, lnB) {
    let lineA, lineB, ln, ln_1, ln_2, ln_3, ln_4, ln_5;
    return ((lineA = lnA, (lineB = lnB, XLine3D_tryIntersect_ZA268E31(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineA, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB, ln_3.ToX - ln_3.FromX), (ln_4 = lineB, ln_4.ToY - ln_4.FromY), (ln_5 = lineB, ln_5.ToZ - ln_5.FromZ), 1E-06, 0.004363350820701567, 1E-06)))) != null;
}

/**
 * A fast test to check if two infinite rays (3D lines extended infinitely) intersect (or come very close in the skew case).
 * Uses a maximum skew distance of 1e-6.
 */
export function Euclid_Line3D__Line3D_doRaysIntersect_Static(lnA, lnB) {
    let pAx_2, pAy_2, pAz_2, vAx_2, vAy_2, vAz_2, x, y, z, t, vAx_3, vAy_3, vAz_3, vx, vy, vz;
    const lineA = lnA;
    const lineB = lnB;
    const pAx = lineA.FromX;
    const pAy = lineA.FromY;
    const pAz = lineA.FromZ;
    const pBx = lineB.FromX;
    const pBy = lineB.FromY;
    const pBz = lineB.FromZ;
    let vAx;
    const ln = lineA;
    vAx = (ln.ToX - ln.FromX);
    let vAy;
    const ln_1 = lineA;
    vAy = (ln_1.ToY - ln_1.FromY);
    let vAz;
    const ln_2 = lineA;
    vAz = (ln_2.ToZ - ln_2.FromZ);
    let vBx;
    const ln_3 = lineB;
    vBx = (ln_3.ToX - ln_3.FromX);
    let vBy;
    const ln_4 = lineB;
    vBy = (ln_4.ToY - ln_4.FromY);
    let vBz;
    const ln_5 = lineB;
    vBz = (ln_5.ToZ - ln_5.FromZ);
    let tA;
    const vAx_1 = vAx;
    const vAy_1 = vAy;
    const vAz_1 = vAz;
    const vBx_1 = vBx;
    const vBy_1 = vBy;
    const vBz_1 = vBz;
    const crossX = (vAy_1 * vBz_1) - (vAz_1 * vBy_1);
    const crossY = (vAz_1 * vBx_1) - (vAx_1 * vBz_1);
    const crossZ = (vAx_1 * vBy_1) - (vAy_1 * vBx_1);
    const dx = pBx - pAx;
    const dy = pBy - pAy;
    const dz = pBz - pAz;
    tA = ((((((dy * vBz_1) - (dz * vBy_1)) * crossX) + (((dz * vBx_1) - (dx * vBz_1)) * crossY)) + (((dx * vBy_1) - (dy * vBx_1)) * crossZ)) / (((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ)));
    if ((tA > -1000000000000) && (tA < 1000000000000)) {
        return ((pAx_2 = pBx, (pAy_2 = pBy, (pAz_2 = pBz, (vAx_2 = vBx, (vAy_2 = vBy, (vAz_2 = vBz, (x = (pAx + (tA * vAx)), (y = (pAy + (tA * vAy)), (z = (pAz + (tA * vAz)), (t = ((vAx_3 = vAx_2, (vAy_3 = vAy_2, (vAz_3 = vAz_2, (((vAx_3 * (x - pAx_2)) + (vAy_3 * (y - pAy_2))) + (vAz_3 * (z - pAz_2))) / (((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3)))))), (vx = ((pAx_2 + (vAx_2 * t)) - x), (vy = ((pAy_2 + (vAy_2 * t)) - y), (vz = ((pAz_2 + (vAz_2 * t)) - z), ((vx * vx) + (vy * vy)) + (vz * vz))))))))))))))) < (1E-06 * 1E-06);
    }
    else {
        return false;
    }
}

/**
 * Tests if two finite 3D lines intersect, touch, or overlap.
 * Also returns TRUE if parallel lines are touching or overlapping each other.
 * Also returns TRUE if zero length lines are at the same location.
 */
export function Euclid_Line3D__Line3D_doIntersectOrOverlap_Static(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5, ln_6, ln_7, ln_8, ln_9, ln_10, ln_11, a, b, dx, dy, dz, ln_12, pAx, pAy, pAz, vAx, ln_13, vAy, ln_14, vAz, ln_15, x_1, y_1, z_1, t, x_3, vAx_1, vAy_1, vAz_1, vx, vy, vz, vx_1, vy_1, vz_1, vx_2, vy_2, vz_2, ln_16, pAx_2, pAy_2, pAz_2, vAx_2, ln_17, vAy_2, ln_18, vAz_2, ln_19, x_5, y_4, z_4, t_1, x_7, vAx_3, vAy_3, vAz_3, vx_3, vy_3, vz_3, vx_1_1, vy_1_1, vz_1_1, vx_2_1, vy_2_1, vz_2_1;
    let matchValue;
    const lnA_1 = lnA;
    const lnB_1 = lnB;
    matchValue = XLine3D_getIntersectionParam_ZA268E31(lnA_1.FromX, lnA_1.FromY, lnA_1.FromZ, lnB_1.FromX, lnB_1.FromY, lnB_1.FromZ, (ln = lnA_1, ln.ToX - ln.FromX), (ln_1 = lnA_1, ln_1.ToY - ln_1.FromY), (ln_2 = lnA_1, ln_2.ToZ - ln_2.FromZ), (ln_3 = lnB_1, ln_3.ToX - ln_3.FromX), (ln_4 = lnB_1, ln_4.ToY - ln_4.FromY), (ln_5 = lnB_1, ln_5.ToZ - ln_5.FromZ), 1E-06, 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 1:
            return false;
        case 3: {
            const lineA = lnA;
            const lineB = lnB;
            return XLine3D_doOverlap_6B19E37B(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln_6 = lineA, ln_6.ToX - ln_6.FromX), (ln_7 = lineA, ln_7.ToY - ln_7.FromY), (ln_8 = lineA, ln_8.ToZ - ln_8.FromZ), (ln_9 = lineB, ln_9.ToX - ln_9.FromX), (ln_10 = lineB, ln_10.ToY - ln_10.FromY), (ln_11 = lineB, ln_11.ToZ - ln_11.FromZ), 1E-06);
        }
        case 2:
            return false;
        case 6:
            return ((a = lnA, (b = lnB, (dx = (a.FromX - b.FromX), (dy = (a.FromY - b.FromY), (dz = (a.FromZ - b.FromZ), ((dx * dx) + (dy * dy)) + (dz * dz))))))) < 1E-12;
        case 4:
            return ((ln_12 = lnB, (pAx = ln_12.FromX, (pAy = ln_12.FromY, (pAz = ln_12.FromZ, (vAx = ((ln_13 = ln_12, ln_13.ToX - ln_13.FromX)), (vAy = ((ln_14 = ln_12, ln_14.ToY - ln_14.FromY)), (vAz = ((ln_15 = ln_12, ln_15.ToZ - ln_15.FromZ)), (x_1 = lnA.FromX, (y_1 = lnA.FromY, (z_1 = lnA.FromZ, (t = ((x_3 = ((vAx_1 = vAx, (vAy_1 = vAy, (vAz_1 = vAz, (((vAx_1 * (x_1 - pAx)) + (vAy_1 * (y_1 - pAy))) + (vAz_1 * (z_1 - pAz))) / (((vAx_1 * vAx_1) + (vAy_1 * vAy_1)) + (vAz_1 * vAz_1)))))), (x_3 > 0) ? ((x_3 < 1) ? x_3 : 1) : 0)), (t > -1E-06) ? ((t < 1.000001) ? ((vx = ((pAx + (vAx * t)) - x_1), (vy = ((pAy + (vAy * t)) - y_1), (vz = ((pAz + (vAz * t)) - z_1), ((vx * vx) + (vy * vy)) + (vz * vz))))) : ((vx_1 = ((pAx + vAx) - x_1), (vy_1 = ((pAy + vAy) - y_1), (vz_1 = ((pAz + vAz) - z_1), ((vx_1 * vx_1) + (vy_1 * vy_1)) + (vz_1 * vz_1)))))) : ((vx_2 = (pAx - x_1), (vy_2 = (pAy - y_1), (vz_2 = (pAz - z_1), ((vx_2 * vx_2) + (vy_2 * vy_2)) + (vz_2 * vz_2))))))))))))))))) < 1E-12;
        case 5:
            return ((ln_16 = lnA, (pAx_2 = ln_16.FromX, (pAy_2 = ln_16.FromY, (pAz_2 = ln_16.FromZ, (vAx_2 = ((ln_17 = ln_16, ln_17.ToX - ln_17.FromX)), (vAy_2 = ((ln_18 = ln_16, ln_18.ToY - ln_18.FromY)), (vAz_2 = ((ln_19 = ln_16, ln_19.ToZ - ln_19.FromZ)), (x_5 = lnB.FromX, (y_4 = lnB.FromY, (z_4 = lnB.FromZ, (t_1 = ((x_7 = ((vAx_3 = vAx_2, (vAy_3 = vAy_2, (vAz_3 = vAz_2, (((vAx_3 * (x_5 - pAx_2)) + (vAy_3 * (y_4 - pAy_2))) + (vAz_3 * (z_4 - pAz_2))) / (((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3)))))), (x_7 > 0) ? ((x_7 < 1) ? x_7 : 1) : 0)), (t_1 > -1E-06) ? ((t_1 < 1.000001) ? ((vx_3 = ((pAx_2 + (vAx_2 * t_1)) - x_5), (vy_3 = ((pAy_2 + (vAy_2 * t_1)) - y_4), (vz_3 = ((pAz_2 + (vAz_2 * t_1)) - z_4), ((vx_3 * vx_3) + (vy_3 * vy_3)) + (vz_3 * vz_3))))) : ((vx_1_1 = ((pAx_2 + vAx_2) - x_5), (vy_1_1 = ((pAy_2 + vAy_2) - y_4), (vz_1_1 = ((pAz_2 + vAz_2) - z_4), ((vx_1_1 * vx_1_1) + (vy_1_1 * vy_1_1)) + (vz_1_1 * vz_1_1)))))) : ((vx_2_1 = (pAx_2 - x_5), (vy_2_1 = (pAy_2 - y_4), (vz_2_1 = (pAz_2 - z_4), ((vx_2_1 * vx_2_1) + (vy_2_1 * vy_2_1)) + (vz_2_1 * vz_2_1))))))))))))))))) < 1E-12;
        default:
            return true;
    }
}

/**
 * A fast intersection of two finite 3D lines.
 * Returns the intersection point or the midpoint of closest approach for skew lines within tolerance.
 * Does not use a default tolerance for parallel or coincident lines.
 */
export function Euclid_Line3D__Line3D_tryIntersect_Static(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5;
    const lineA = lnA;
    const lineB = lnB;
    return XLine3D_tryIntersect_ZA268E31(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineA, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB, ln_3.ToX - ln_3.FromX), (ln_4 = lineB, ln_4.ToY - ln_4.FromY), (ln_5 = lineB, ln_5.ToZ - ln_5.FromZ), 1E-06, 0.004363350820701567, 1E-06);
}

/**
 * Tries to get intersection point of two rays (rays are 3D lines extended infinitely).
 */
export function Euclid_Line3D__Line3D_tryIntersectRay_Static(lineA, lineB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5;
    const lineA_1 = lineA;
    const lineB_1 = lineB;
    return XLine3D_tryIntersectRay_ZA268E31(lineA_1.FromX, lineA_1.FromY, lineA_1.FromZ, lineB_1.FromX, lineB_1.FromY, lineB_1.FromZ, (ln = lineA_1, ln.ToX - ln.FromX), (ln_1 = lineA_1, ln_1.ToY - ln_1.FromY), (ln_2 = lineA_1, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB_1, ln_3.ToX - ln_3.FromX), (ln_4 = lineB_1, ln_4.ToY - ln_4.FromY), (ln_5 = lineB_1, ln_5.ToZ - ln_5.FromZ), 1E-06, 0.004363350820701567, 1E-06);
}

/**
 * Intersects two finite 3D Lines.
 * Also returns a point if parallel lines are touching or overlapping each other.
 * Also returns a point if zero length lines are at the same location within 1e-6 distance.
 */
export function Euclid_Line3D__Line3D_tryIntersectOrOverlap_Static(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5, pAx, pAy, pAz, vAx, ln_6, vAy, ln_7, vAz, ln_8, x, y, z, t, vAx_1, vAy_1, vAz_1, vx, vy, vz, ln_9, p_1, ln_10, ln_11, ln_12, a, b, dx, dy, dz, ln_13, ln_14, pAx_2, pAy_2, pAz_2, vAx_2, ln_15, vAy_2, ln_16, vAz_2, ln_17, x_3, y_3, z_3, t_1, x_5, vAx_3, vAy_3, vAz_3, vx_1, vy_1, vz_1, vx_1_1, vy_1_1, vz_1_1, vx_2, vy_2, vz_2, ln_18, ln_19, pAx_4, pAy_4, pAz_4, vAx_4, ln_20, vAy_4, ln_21, vAz_4, ln_22, x_7, y_6, z_6, t_2, x_9, vAx_5, vAy_5, vAz_5, vx_3, vy_3, vz_3, vx_1_2, vy_1_2, vz_1_2, vx_2_1, vy_2_1, vz_2_1, ln_23;
    let matchValue;
    const lineA = lnA;
    const lineB = lnB;
    matchValue = XLine3D_getIntersection_ZA268E31(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineA, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB, ln_3.ToX - ln_3.FromX), (ln_4 = lineB, ln_4.ToY - ln_4.FromY), (ln_5 = lineB, ln_5.ToZ - ln_5.FromZ), 1E-06, 0.004363350820701567, 1E-06);
    switch (matchValue.tag) {
        case 1:
            return undefined;
        case 2:
            return undefined;
        case 3:
            if (((pAx = lnA.FromX, (pAy = lnA.FromY, (pAz = lnA.FromZ, (vAx = ((ln_6 = lnA, ln_6.ToX - ln_6.FromX)), (vAy = ((ln_7 = lnA, ln_7.ToY - ln_7.FromY)), (vAz = ((ln_8 = lnA, ln_8.ToZ - ln_8.FromZ)), (x = lnB.FromX, (y = lnB.FromY, (z = lnB.FromZ, (t = ((vAx_1 = vAx, (vAy_1 = vAy, (vAz_1 = vAz, (((vAx_1 * (x - pAx)) + (vAy_1 * (y - pAy))) + (vAz_1 * (z - pAz))) / (((vAx_1 * vAx_1) + (vAy_1 * vAy_1)) + (vAz_1 * vAz_1)))))), (vx = ((pAx + (vAx * t)) - x), (vy = ((pAy + (vAy * t)) - y), (vz = ((pAz + (vAz * t)) - z), ((vx * vx) + (vy * vy)) + (vz * vz))))))))))))))) < 1E-12) {
                const matchValue_1 = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(lnA, lnB);
                if (matchValue_1 == null) {
                    return undefined;
                }
                else {
                    return (ln_9 = lnA, (p_1 = ((matchValue_1[0] + matchValue_1[1]) * 0.5), Pnt_$ctor_Z7AD9E565_1(ln_9.FromX + (((ln_10 = ln_9, ln_10.ToX - ln_10.FromX)) * p_1), ln_9.FromY + (((ln_11 = ln_9, ln_11.ToY - ln_11.FromY)) * p_1), ln_9.FromZ + (((ln_12 = ln_9, ln_12.ToZ - ln_12.FromZ)) * p_1))));
                }
            }
            else {
                return undefined;
            }
        case 6:
            if (((a = lnA, (b = lnB, (dx = (a.FromX - b.FromX), (dy = (a.FromY - b.FromY), (dz = (a.FromZ - b.FromZ), ((dx * dx) + (dy * dy)) + (dz * dz))))))) < 1E-12) {
                return (ln_13 = lnA, Pnt_$ctor_Z7AD9E565(ln_13.FromX, ln_13.FromY, ln_13.FromZ));
            }
            else {
                return undefined;
            }
        case 4:
            if (((ln_14 = lnB, (pAx_2 = ln_14.FromX, (pAy_2 = ln_14.FromY, (pAz_2 = ln_14.FromZ, (vAx_2 = ((ln_15 = ln_14, ln_15.ToX - ln_15.FromX)), (vAy_2 = ((ln_16 = ln_14, ln_16.ToY - ln_16.FromY)), (vAz_2 = ((ln_17 = ln_14, ln_17.ToZ - ln_17.FromZ)), (x_3 = lnA.FromX, (y_3 = lnA.FromY, (z_3 = lnA.FromZ, (t_1 = ((x_5 = ((vAx_3 = vAx_2, (vAy_3 = vAy_2, (vAz_3 = vAz_2, (((vAx_3 * (x_3 - pAx_2)) + (vAy_3 * (y_3 - pAy_2))) + (vAz_3 * (z_3 - pAz_2))) / (((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_3 * vAz_3)))))), (x_5 > 0) ? ((x_5 < 1) ? x_5 : 1) : 0)), (t_1 > -1E-06) ? ((t_1 < 1.000001) ? ((vx_1 = ((pAx_2 + (vAx_2 * t_1)) - x_3), (vy_1 = ((pAy_2 + (vAy_2 * t_1)) - y_3), (vz_1 = ((pAz_2 + (vAz_2 * t_1)) - z_3), ((vx_1 * vx_1) + (vy_1 * vy_1)) + (vz_1 * vz_1))))) : ((vx_1_1 = ((pAx_2 + vAx_2) - x_3), (vy_1_1 = ((pAy_2 + vAy_2) - y_3), (vz_1_1 = ((pAz_2 + vAz_2) - z_3), ((vx_1_1 * vx_1_1) + (vy_1_1 * vy_1_1)) + (vz_1_1 * vz_1_1)))))) : ((vx_2 = (pAx_2 - x_3), (vy_2 = (pAy_2 - y_3), (vz_2 = (pAz_2 - z_3), ((vx_2 * vx_2) + (vy_2 * vy_2)) + (vz_2 * vz_2))))))))))))))))) < 1E-12) {
                return (ln_18 = lnA, Pnt_$ctor_Z7AD9E565(ln_18.FromX, ln_18.FromY, ln_18.FromZ));
            }
            else {
                return undefined;
            }
        case 5:
            if (((ln_19 = lnA, (pAx_4 = ln_19.FromX, (pAy_4 = ln_19.FromY, (pAz_4 = ln_19.FromZ, (vAx_4 = ((ln_20 = ln_19, ln_20.ToX - ln_20.FromX)), (vAy_4 = ((ln_21 = ln_19, ln_21.ToY - ln_21.FromY)), (vAz_4 = ((ln_22 = ln_19, ln_22.ToZ - ln_22.FromZ)), (x_7 = lnB.FromX, (y_6 = lnB.FromY, (z_6 = lnB.FromZ, (t_2 = ((x_9 = ((vAx_5 = vAx_4, (vAy_5 = vAy_4, (vAz_5 = vAz_4, (((vAx_5 * (x_7 - pAx_4)) + (vAy_5 * (y_6 - pAy_4))) + (vAz_5 * (z_6 - pAz_4))) / (((vAx_5 * vAx_5) + (vAy_5 * vAy_5)) + (vAz_5 * vAz_5)))))), (x_9 > 0) ? ((x_9 < 1) ? x_9 : 1) : 0)), (t_2 > -1E-06) ? ((t_2 < 1.000001) ? ((vx_3 = ((pAx_4 + (vAx_4 * t_2)) - x_7), (vy_3 = ((pAy_4 + (vAy_4 * t_2)) - y_6), (vz_3 = ((pAz_4 + (vAz_4 * t_2)) - z_6), ((vx_3 * vx_3) + (vy_3 * vy_3)) + (vz_3 * vz_3))))) : ((vx_1_2 = ((pAx_4 + vAx_4) - x_7), (vy_1_2 = ((pAy_4 + vAy_4) - y_6), (vz_1_2 = ((pAz_4 + vAz_4) - z_6), ((vx_1_2 * vx_1_2) + (vy_1_2 * vy_1_2)) + (vz_1_2 * vz_1_2)))))) : ((vx_2_1 = (pAx_4 - x_7), (vy_2_1 = (pAy_4 - y_6), (vz_2_1 = (pAz_4 - z_6), ((vx_2_1 * vx_2_1) + (vy_2_1 * vy_2_1)) + (vz_2_1 * vz_2_1))))))))))))))))) < 1E-12) {
                return (ln_23 = lnB, Pnt_$ctor_Z7AD9E565(ln_23.FromX, ln_23.FromY, ln_23.FromZ));
            }
            else {
                return undefined;
            }
        default:
            return matchValue.fields[0];
    }
}

/**
 * Checks if lines are parallel, coincident and overlapping.
 */
export function Euclid_Line3D__Line3D_tryGetOverlap_Static(lnA, lnB) {
    let ln_1, ln_2, ln_3, ln_5, ln_6, ln_7, pAx, pAy, pAz, vAx_1, ln_8, vAy_1, ln_9, vAz_1, ln_10, x_2, y_2, z_2, t, vAx_2, vAy_2, vAz_2, vx, vy, vz;
    let va;
    const ln = lnA;
    va = Vec_$ctor_Z7AD9E565((ln_1 = ln, ln_1.ToX - ln_1.FromX), (ln_2 = ln, ln_2.ToY - ln_2.FromY), (ln_3 = ln, ln_3.ToZ - ln_3.FromZ));
    let vb;
    const ln_4 = lnB;
    vb = Vec_$ctor_Z7AD9E565((ln_5 = ln_4, ln_5.ToX - ln_5.FromX), (ln_6 = ln_4, ln_6.ToY - ln_6.FromY), (ln_7 = ln_4, ln_7.ToZ - ln_7.FromZ));
    if ((((Math.abs(va.X) + Math.abs(va.Y)) + Math.abs(va.Z)) < 1E-06) ? true : (((Math.abs(vb.X) + Math.abs(vb.Y)) + Math.abs(vb.Z)) < 1E-06)) {
        return undefined;
    }
    else {
        let tan;
        const vAx = va.X;
        const vAy = va.Y;
        const vAz = va.Z;
        const vBx = vb.X;
        const vBy = vb.Y;
        const vBz = vb.Z;
        const crossX = (vAy * vBz) - (vAz * vBy);
        const crossY = (vAz * vBx) - (vAx * vBz);
        const crossZ = (vAx * vBy) - (vAy * vBx);
        tan = (Math.sqrt(((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ)) / (((vAx * vBx) + (vAy * vBy)) + (vAz * vBz)));
        if (Math.abs(tan) < 0.004363350820701567) {
            if (((pAx = lnA.FromX, (pAy = lnA.FromY, (pAz = lnA.FromZ, (vAx_1 = ((ln_8 = lnA, ln_8.ToX - ln_8.FromX)), (vAy_1 = ((ln_9 = lnA, ln_9.ToY - ln_9.FromY)), (vAz_1 = ((ln_10 = lnA, ln_10.ToZ - ln_10.FromZ)), (x_2 = lnB.FromX, (y_2 = lnB.FromY, (z_2 = lnB.FromZ, (t = ((vAx_2 = vAx_1, (vAy_2 = vAy_1, (vAz_2 = vAz_1, (((vAx_2 * (x_2 - pAx)) + (vAy_2 * (y_2 - pAy))) + (vAz_2 * (z_2 - pAz))) / (((vAx_2 * vAx_2) + (vAy_2 * vAy_2)) + (vAz_2 * vAz_2)))))), (vx = ((pAx + (vAx_1 * t)) - x_2), (vy = ((pAy + (vAy_1 * t)) - y_2), (vz = ((pAz + (vAz_1 * t)) - z_2), ((vx * vx) + (vy * vy)) + (vz * vz))))))))))))))) < 1E-09) {
                return Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(lnA, lnB);
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
 * Computes the squared distance between two finite 3D lines.
 */
export function Euclid_Line3D__Line3D_sqDistanceToLine_Static(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5;
    const lineA = lnA;
    const lineB = lnB;
    return XLine3D_getSqDistance_Z15A9A3C0(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineA, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB, ln_3.ToX - ln_3.FromX), (ln_4 = lineB, ln_4.ToY - ln_4.FromY), (ln_5 = lineB, ln_5.ToZ - ln_5.FromZ));
}

/**
 * Computes the distance between two finite 3D lines.
 */
export function Euclid_Line3D__Line3D_distanceToLine_Static(lnA, lnB) {
    let ln, ln_1, ln_2, ln_3, ln_4, ln_5;
    let value;
    const lineA = lnA;
    const lineB = lnB;
    value = XLine3D_getSqDistance_Z15A9A3C0(lineA.FromX, lineA.FromY, lineA.FromZ, lineB.FromX, lineB.FromY, lineB.FromZ, (ln = lineA, ln.ToX - ln.FromX), (ln_1 = lineA, ln_1.ToY - ln_1.FromY), (ln_2 = lineA, ln_2.ToZ - ln_2.FromZ), (ln_3 = lineB, ln_3.ToX - ln_3.FromX), (ln_4 = lineB, ln_4.ToY - ln_4.FromY), (ln_5 = lineB, ln_5.ToZ - ln_5.FromZ));
    return Math.sqrt(value);
}

/**
 * Checks if the two finite 3D lines are touching each other at any of end points
 * within the given tolerance.
 * This will also return TRUE if the lines are touching on both points.
 */
export function Euclid_Line3D__Line3D_isTouchingEndOf_Static(squareTolerance, a, b) {
    let x, y, z, x_1, y_1, z_1, x_2, y_2, z_2;
    if ((((x = (a.ToX - b.FromX), (y = (a.ToY - b.FromY), (z = (a.ToZ - b.FromZ), (((x * x) + (y * y)) + (z * z)) < squareTolerance)))) ? true : ((x_1 = (a.FromX - b.ToX), (y_1 = (a.FromY - b.ToY), (z_1 = (a.FromZ - b.ToZ), (((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1)) < squareTolerance))))) ? true : ((x_2 = (a.FromX - b.FromX), (y_2 = (a.FromY - b.FromY), (z_2 = (a.FromZ - b.FromZ), (((x_2 * x_2) + (y_2 * y_2)) + (z_2 * z_2)) < squareTolerance))))) {
        return true;
    }
    else {
        const x_3 = a.ToX - b.ToX;
        const y_3 = a.ToY - b.ToY;
        const z_3 = a.ToZ - b.ToZ;
        return (((x_3 * x_3) + (y_3 * y_3)) + (z_3 * z_3)) < squareTolerance;
    }
}

export function Euclid_Line3D__Line3D_ClosestParameterInfinite_Z394ECE4D(ln, p) {
    const ln_1 = ln;
    const p_1 = p;
    const x = ln_1.FromX - ln_1.ToX;
    const y = ln_1.FromY - ln_1.ToY;
    const z = ln_1.FromZ - ln_1.ToZ;
    const lenSq = ((x * x) + (y * y)) + (z * z);
    if (!(lenSq > 1E-12)) {
        failTooSmall2("Line3D.RayClosestParameter", ln_1, p_1);
    }
    return (((x * (ln_1.FromX - p_1.X)) + (y * (ln_1.FromY - p_1.Y))) + (z * (ln_1.FromZ - p_1.Z))) / lenSq;
}

export function Euclid_Line3D__Line3D_ClosestPointInfinite_Z394ECE4D(ln, p) {
    const ln_1 = ln;
    const p_1 = p;
    const x = ln_1.FromX - ln_1.ToX;
    const y = ln_1.FromY - ln_1.ToY;
    const z = ln_1.FromZ - ln_1.ToZ;
    const lenSq = ((x * x) + (y * y)) + (z * z);
    if (!(lenSq > 1E-12)) {
        failTooSmall2("Line3D.RayClosestPoint", ln_1, p_1);
    }
    const t = (((x * (ln_1.FromX - p_1.X)) + (y * (ln_1.FromY - p_1.Y))) + (z * (ln_1.FromZ - p_1.Z))) / lenSq;
    return Pnt_$ctor_Z7AD9E565_1(ln_1.FromX - (x * t), ln_1.FromY - (y * t), ln_1.FromZ - (z * t));
}

export function Euclid_Line3D__Line3D_DistanceSqToPntInfinite_Z394ECE4D(ln, p) {
    return Euclid_Line3D__Line3D_SqDistanceRayPoint_Z394ECE4D(ln, p);
}

export function Euclid_Line3D__Line3D_distanceBetweenLines_Static_Z56225400(lnA, lnB) {
    return Euclid_Line3D__Line3D_distanceToLine_Static(lnA, lnB);
}

export function Euclid_Line3D__Line3D_projectOn_Static_Z56225400(onToLine, lineToProject) {
    return Euclid_Line3D__Line3D_projectOntoRay_Static(onToLine, lineToProject);
}

export function Euclid_Line3D__Line3D_areTouchingAny_Static_Z1A06D1C5(_tol, _a, _b) {
    return failObsoleteV30("Line3D.areTouchingAny", "XLine3D.getEndsTouching");
}

export function Euclid_Line3D__Line3D_areTouchingEither_Static_Z1A06D1C5(_tol, _a, _b) {
    return failObsoleteV30("Line3D.areTouchingEither", "XLine3D.getEndsTouching");
}

export function Euclid_Line3D__Line3D_intersectionParamInfinite_Static_Z56225400(_lnA, _lnB) {
    return failObsoleteV30("Line3D.intersectionParamInfinite", "XLine3D.getIntersectionParam");
}

export function Euclid_Line3D__Line3D_intersectionInfinite_Static_Z56225400(_lnA, _lnB) {
    return failObsoleteV30("Line3D.intersectionInfinite", "XLine3D.getIntersection");
}

export function Euclid_Line3D__Line3D_intersectionPointInfinite_Static_Z56225400(_lnA, _lnB) {
    return failObsoleteV30("Line3D.intersectionPointInfinite", "Line3D.tryIntersectRay");
}

export function Euclid_Line3D__Line3D_intersectionParam_Static_Z56225400(_lnA, _lnB) {
    return failObsoleteV30("Line3D.intersectionParam", "XLine3D.getIntersectionParam");
}

export function Euclid_Line3D__Line3D_intersection_Static_Z56225400(_lnA, _lnB) {
    return failObsoleteV30("Line3D.intersection", "XLine3D.getIntersection");
}

export function Euclid_Line3D__Line3D_intersectionPoint_Static_Z56225400(_lnA, _lnB) {
    return failObsoleteV30("Line3D.intersectionPoint", "Line3D.tryIntersect");
}

