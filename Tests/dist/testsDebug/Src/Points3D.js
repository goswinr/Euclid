
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { count } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { failDivide, failRarr, fail } from "./EuclidErrors.js";
import { insertRangeInPlace, addRangeInPlace, item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { ResizeArr_rev, ResizeArr_minIndexBy, ResizeArr_maxIndexBy } from "./ResizeArr.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Pnt.js";
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { iList } from "./Format.js";
import { Vec_$ctor_Z7AD9E565 } from "./Vec.js";
import { Tria3D_offsetVar_Z62609A8D, Tria3D_isLinear_Z305FC6B8 } from "./Tria3D.js";
import { Points2D_findContinuousPoints_53EC9D26, Points2D_cullDuplicatePointsInSeq_Z4CAB181, Points2D_mostDistantPoint_6A06E040, Points2D_mostDistantPointIdx_6A06E040, Points2D_minDistBetweenPointSets_6A06E040, Points2D_closestPointsIdx_6A06E040, Points2D_closestPoint_Z7C0841BD, Points2D_closestPointIdx_Z7C0841BD, Points2D_getSignedArea_Z7CD03502 } from "./Points2D.js";

/**
 * A type containing only static member functions for operating on multiple 3D points or set of 3D points.
 * Aka point-clouds
 */
export class Points3D {
    constructor() {
    }
}

export function Points3D_$reflection() {
    return class_type("Euclid.Points3D", undefined, Points3D);
}

/**
 * Returns the closest 3D point index from a 3D point list to a given 3D point.
 */
export function Points3D_closestPointIdx_Z371982BD(pts, pt) {
    if (count(pts) === 0) {
        fail("Points3D.closestPoint: empty List of Points: pts");
    }
    let mi = -1;
    let mid = 1.7976931348623157E+308;
    for (let i = 0; i <= (count(pts) - 1); i++) {
        const p = item(i, pts);
        let d;
        const a_1 = p;
        const b_1 = pt;
        const x = a_1.X - b_1.X;
        const y = a_1.Y - b_1.Y;
        const z = a_1.Z - b_1.Z;
        d = (((x * x) + (y * y)) + (z * z));
        if (d < mid) {
            mid = d;
            mi = (i | 0);
        }
    }
    return mi | 0;
}

/**
 * Returns the closest point from a 3D point list to a given 3D point.
 */
export function Points3D_closestPoint_Z371982BD(pts, pt) {
    return item(Points3D_closestPointIdx_Z371982BD(pts, pt), pts);
}

/**
 * Returns the closest of two 3D points to a given reference 3D point.
 * If both points are equidistant the first point is returned.
 */
export function Points3D_closestOfTwo(pt1, pt2, referencePoint) {
    let d1;
    const a_1 = pt1;
    const b_1 = referencePoint;
    const x = a_1.X - b_1.X;
    const y = a_1.Y - b_1.Y;
    const z = a_1.Z - b_1.Z;
    d1 = (((x * x) + (y * y)) + (z * z));
    let d2;
    const a_3 = pt2;
    const b_3 = referencePoint;
    const x_1 = a_3.X - b_3.X;
    const y_1 = a_3.Y - b_3.Y;
    const z_1 = a_3.Z - b_3.Z;
    d2 = (((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1));
    if (d1 <= d2) {
        return pt1;
    }
    else {
        return pt2;
    }
}

/**
 * Returns the indices of the 3D points that are closest to each other.
 */
export function Points3D_closestPointsIdx_Z1881DE00(xs, ys) {
    if (count(xs) === 0) {
        fail("Points3D.closestPointsIdx: empty List of Points: xs");
    }
    if (count(ys) === 0) {
        fail("Points3D.closestPointsIdx: empty List of Points: ys");
    }
    let xi = -1;
    let yj = -1;
    let minD = 1.7976931348623157E+308;
    for (let i = 0; i <= (count(xs) - 1); i++) {
        const pt = item(i, xs);
        for (let j = 0; j <= (count(ys) - 1); j++) {
            let d;
            const a_1 = pt;
            const b_1 = item(j, ys);
            const x = a_1.X - b_1.X;
            const y = a_1.Y - b_1.Y;
            const z = a_1.Z - b_1.Z;
            d = (((x * x) + (y * y)) + (z * z));
            if (d < minD) {
                minD = d;
                xi = (i | 0);
                yj = (j | 0);
            }
        }
    }
    return [xi, yj];
}

/**
 * Given two lists of 3D points finds the pair that are closest to each other and returns their distance.
 */
export function Points3D_minDistBetweenPointSets_Z1881DE00(xs, ys) {
    if (count(xs) === 0) {
        fail("Points3D.minDistBetweenPointSets: empty List of Points: xs");
    }
    if (count(ys) === 0) {
        fail("Points3D.minDistBetweenPointSets: empty List of Points: ys");
    }
    const patternInput = Points3D_closestPointsIdx_Z1881DE00(xs, ys);
    const j = patternInput[1] | 0;
    const i = patternInput[0] | 0;
    const a_1 = item(i, xs);
    const b_1 = item(j, ys);
    const x = a_1.X - b_1.X;
    const y = a_1.Y - b_1.Y;
    const z = a_1.Z - b_1.Z;
    return Math.sqrt(((x * x) + (y * y)) + (z * z));
}

/**
 * Find the index of the 3D point that has the biggest distance to any 3D point from the other set.
 * Basically the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list.
 * Returns findPointFromIdx * checkAgainstIdx
 */
export function Points3D_mostDistantPointIdx_Z1881DE00(findPointFrom, checkAgainst) {
    if (count(findPointFrom) === 0) {
        fail("Points3D.mostDistantPoint: empty List of Points: findPointFrom");
    }
    if (count(checkAgainst) === 0) {
        fail("Points3D.mostDistantPoint: empty List of Points: checkAgainst");
    }
    let maxD = -1.7976931348623157E+308;
    let findPointFromIdx = -1;
    let checkAgainstTempIdx = -1;
    let checkAgainstIdx = -1;
    for (let i = 0; i <= (count(findPointFrom) - 1); i++) {
        const pt = item(i, findPointFrom);
        let minD = 1.7976931348623157E+308;
        for (let j = 0; j <= (count(checkAgainst) - 1); j++) {
            let d;
            const a_1 = pt;
            const b_1 = item(j, checkAgainst);
            const x = a_1.X - b_1.X;
            const y = a_1.Y - b_1.Y;
            const z = a_1.Z - b_1.Z;
            d = (((x * x) + (y * y)) + (z * z));
            if (d < minD) {
                minD = d;
                checkAgainstTempIdx = (j | 0);
            }
        }
        if (minD > maxD) {
            maxD = minD;
            findPointFromIdx = (i | 0);
            checkAgainstIdx = (checkAgainstTempIdx | 0);
        }
    }
    return [findPointFromIdx, checkAgainstIdx];
}

/**
 * Find the 3D point that has the biggest distance to any 3D point from another set.
 */
export function Points3D_mostDistantPoint_Z1881DE00(findPointFrom, checkAgainst) {
    const i = Points3D_mostDistantPointIdx_Z1881DE00(findPointFrom, checkAgainst)[0] | 0;
    return item(i, findPointFrom);
}

/**
 * Culls 3D points if they are too close to previous or next item.
 * Last and first 3D points stay the same.
 */
export function Points3D_cullDuplicatePointsInSeq_21159971(pts, tolerance) {
    if (pts.length === 0) {
        fail("Points3D.cullDuplicatePointsInSeq: empty List of Points");
    }
    if (pts.length === 1) {
        return pts;
    }
    else {
        const tolSq = tolerance * tolerance;
        const res = [];
        let last = item(0, pts);
        void (res.push(last));
        const iLast = (pts.length - 1) | 0;
        for (let i = 1; i <= iLast; i++) {
            let a_1, b_1, x, y, z;
            const pt = item(i, pts);
            if (((a_1 = last, (b_1 = pt, (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x * x) + (y * y)) + (z * z))))))) > tolSq) {
                last = pt;
                void (res.push(last));
            }
            else if (i === iLast) {
                res.splice(res.length - 1, 1);
                void (res.push(pt));
            }
        }
        return res;
    }
}

/**
 * Similar to join polylines, this tries to find continuous sequences of 3D points.
 * 'tolGap' is the maximum allowable gap between the start and the endpoint of two segments.
 * Search starts from the segment with the most points.
 * Both start and end point of each 3D point list are checked for adjacency.
 */
export function Points3D_findContinuousPoints_503DFFA8(ptss, tolGap) {
    const i = ResizeArr_maxIndexBy((xs) => (xs.length | 0), ptss) | 0;
    let res;
    const this$ = ptss;
    const index = i | 0;
    if (index < 0) {
        failRarr(`Pop(${index})`, this$);
    }
    if (index >= this$.length) {
        failRarr(`Pop(${index})`, this$);
    }
    const v = item(index, this$);
    this$.splice(index, 1);
    res = v;
    let loop = true;
    while (loop && (ptss.length > 0)) {
        let this$_5, index_1, v_1, this$_6, index_2, v_2, this$_11, index_3, v_3, this$_12, index_4, v_4;
        const ende = item(res.length - 1, res);
        const si = ResizeArr_minIndexBy((ps) => {
            const a_1 = ende;
            let b_1;
            const this$_1 = ps;
            if (this$_1.length === 0) {
                failRarr("First.get", this$_1);
            }
            b_1 = item(0, this$_1);
            const x = a_1.X - b_1.X;
            const y = a_1.Y - b_1.Y;
            const z = a_1.Z - b_1.Z;
            return ((x * x) + (y * y)) + (z * z);
        }, ptss) | 0;
        const ei = ResizeArr_minIndexBy((ps_1) => {
            const a_3 = ende;
            let b_3;
            const this$_2 = ps_1;
            if (this$_2.length === 0) {
                failRarr("Last.get", this$_2);
            }
            b_3 = item(this$_2.length - 1, this$_2);
            const x_1 = a_3.X - b_3.X;
            const y_1 = a_3.Y - b_3.Y;
            const z_1 = a_3.Z - b_3.Z;
            return ((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1);
        }, ptss) | 0;
        let sd;
        const a_5 = ende;
        let b_5;
        const this$_3 = item(si, ptss);
        if (this$_3.length === 0) {
            failRarr("First.get", this$_3);
        }
        b_5 = item(0, this$_3);
        const x_2 = a_5.X - b_5.X;
        const y_2 = a_5.Y - b_5.Y;
        const z_2 = a_5.Z - b_5.Z;
        sd = Math.sqrt(((x_2 * x_2) + (y_2 * y_2)) + (z_2 * z_2));
        let ed;
        const a_7 = ende;
        let b_7;
        const this$_4 = item(ei, ptss);
        if (this$_4.length === 0) {
            failRarr("Last.get", this$_4);
        }
        b_7 = item(this$_4.length - 1, this$_4);
        const x_3 = a_7.X - b_7.X;
        const y_3 = a_7.Y - b_7.Y;
        const z_3 = a_7.Z - b_7.Z;
        ed = Math.sqrt(((x_3 * x_3) + (y_3 * y_3)) + (z_3 * z_3));
        if ((sd < tolGap) && (sd < ed)) {
            addRangeInPlace((this$_5 = ptss, (index_1 = (si | 0), ((index_1 < 0) ? failRarr(`Pop(${index_1})`, this$_5) : undefined, ((index_1 >= this$_5.length) ? failRarr(`Pop(${index_1})`, this$_5) : undefined, (v_1 = item(index_1, this$_5), (this$_5.splice(index_1, 1), v_1)))))), res);
        }
        else if ((ed < tolGap) && (ed < sd)) {
            addRangeInPlace(ResizeArr_rev((this$_6 = ptss, (index_2 = (ei | 0), ((index_2 < 0) ? failRarr(`Pop(${index_2})`, this$_6) : undefined, ((index_2 >= this$_6.length) ? failRarr(`Pop(${index_2})`, this$_6) : undefined, (v_2 = item(index_2, this$_6), (this$_6.splice(index_2, 1), v_2))))))), res);
        }
        else {
            const start = item(0, res);
            const si_1 = ResizeArr_minIndexBy((ps_2) => {
                const a_9 = start;
                let b_9;
                const this$_7 = ps_2;
                if (this$_7.length === 0) {
                    failRarr("First.get", this$_7);
                }
                b_9 = item(0, this$_7);
                const x_4 = a_9.X - b_9.X;
                const y_4 = a_9.Y - b_9.Y;
                const z_4 = a_9.Z - b_9.Z;
                return ((x_4 * x_4) + (y_4 * y_4)) + (z_4 * z_4);
            }, ptss) | 0;
            const ei_1 = ResizeArr_minIndexBy((ps_3) => {
                const a_11 = start;
                let b_11;
                const this$_8 = ps_3;
                if (this$_8.length === 0) {
                    failRarr("Last.get", this$_8);
                }
                b_11 = item(this$_8.length - 1, this$_8);
                const x_5 = a_11.X - b_11.X;
                const y_5 = a_11.Y - b_11.Y;
                const z_5 = a_11.Z - b_11.Z;
                return ((x_5 * x_5) + (y_5 * y_5)) + (z_5 * z_5);
            }, ptss) | 0;
            let sd_1;
            const a_13 = start;
            let b_13;
            const this$_9 = item(si_1, ptss);
            if (this$_9.length === 0) {
                failRarr("First.get", this$_9);
            }
            b_13 = item(0, this$_9);
            const x_6 = a_13.X - b_13.X;
            const y_6 = a_13.Y - b_13.Y;
            const z_6 = a_13.Z - b_13.Z;
            sd_1 = Math.sqrt(((x_6 * x_6) + (y_6 * y_6)) + (z_6 * z_6));
            let ed_1;
            const a_15 = start;
            let b_15;
            const this$_10 = item(ei_1, ptss);
            if (this$_10.length === 0) {
                failRarr("Last.get", this$_10);
            }
            b_15 = item(this$_10.length - 1, this$_10);
            const x_7 = a_15.X - b_15.X;
            const y_7 = a_15.Y - b_15.Y;
            const z_7 = a_15.Z - b_15.Z;
            ed_1 = Math.sqrt(((x_7 * x_7) + (y_7 * y_7)) + (z_7 * z_7));
            if ((sd_1 < tolGap) && (sd_1 < ed_1)) {
                insertRangeInPlace(0, ResizeArr_rev((this$_11 = ptss, (index_3 = (si_1 | 0), ((index_3 < 0) ? failRarr(`Pop(${index_3})`, this$_11) : undefined, ((index_3 >= this$_11.length) ? failRarr(`Pop(${index_3})`, this$_11) : undefined, (v_3 = item(index_3, this$_11), (this$_11.splice(index_3, 1), v_3))))))), res);
            }
            else if ((ed_1 < tolGap) && (ed_1 < sd_1)) {
                insertRangeInPlace(0, (this$_12 = ptss, (index_4 = (ei_1 | 0), ((index_4 < 0) ? failRarr(`Pop(${index_4})`, this$_12) : undefined, ((index_4 >= this$_12.length) ? failRarr(`Pop(${index_4})`, this$_12) : undefined, (v_4 = item(index_4, this$_12), (this$_12.splice(index_4, 1), v_4)))))), res);
            }
            else {
                loop = false;
            }
        }
    }
    return res;
}

/**
 * Finds the center, mean, or average point.
 */
export function Points3D_center_Z16D69110(pts) {
    let sum = Pnt_$ctor_Z7AD9E565(0, 0, 0);
    for (let i = 0; i <= (count(pts) - 1); i++) {
        let a, b;
        const pt = item(i, pts);
        sum = ((a = sum, (b = pt, Pnt_$ctor_Z7AD9E565(a.X + b.X, a.Y + b.Y, a.Z + b.Z))));
    }
    const p = sum;
    const f = count(pts);
    if (Math.abs(f) < 1E-12) {
        failDivide("\'/\' operator", f, p);
    }
    return Pnt_$ctor_Z7AD9E565(p.X / f, p.Y / f, p.Z / f);
}

/**
 * Finds the mean normal of many points.
 * It finds the center point and then takes cross-products, iterating all points in pairs of two.
 * The first three points define the orientation of the normal.
 * So it considers the current order of points too.
 * If the order is counterclockwise in the World X-Y plane, then the normal is in world Z orientation.
 * The sweep from the first to the second point (from the mean center) defines the orientation of the normal.
 */
export function Points3D_normalOfPoints_Z16D69110(pts) {
    let v_1, v_4;
    if (count(pts) <= 2) {
        fail(concat("Points3D.normalOfPoints can\'t find normal of two or less points: ", ...iList(pts)));
    }
    if (count(pts) === 3) {
        let a_1;
        const a = item(0, pts);
        const b = item(1, pts);
        a_1 = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
        let b_2;
        const a_2 = item(2, pts);
        const b_1 = item(1, pts);
        b_2 = Vec_$ctor_Z7AD9E565(a_2.X - b_1.X, a_2.Y - b_1.Y, a_2.Z - b_1.Z);
        let v;
        const a_3 = b_2;
        const b_3 = a_1;
        v = Vec_$ctor_Z7AD9E565((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X));
        if (!(((v_1 = v, ((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z))) > 1E-12)) {
            fail(concat("Points3D.normalOfPoints: three points are in a line: ", ...iList(pts)));
        }
        return v;
    }
    else {
        const cen = Points3D_center_Z16D69110(pts);
        let v_2 = Vec_$ctor_Z7AD9E565(0, 0, 0);
        let t = item(0, pts);
        for (let i = 1; i <= (count(pts) - 1); i++) {
            let a_7, b_7, a_8, b_8;
            const n = item(i, pts);
            let x_1;
            let vecToFlip_1;
            let a_6;
            const a_4 = t;
            const b_4 = cen;
            a_6 = Vec_$ctor_Z7AD9E565(a_4.X - b_4.X, a_4.Y - b_4.Y, a_4.Z - b_4.Z);
            let b_6;
            const a_5 = n;
            const b_5 = cen;
            b_6 = Vec_$ctor_Z7AD9E565(a_5.X - b_5.X, a_5.Y - b_5.Y, a_5.Z - b_5.Z);
            vecToFlip_1 = Vec_$ctor_Z7AD9E565((a_6.Y * b_6.Z) - (a_6.Z * b_6.Y), (a_6.Z * b_6.X) - (a_6.X * b_6.Z), (a_6.X * b_6.Y) - (a_6.Y * b_6.X));
            if (((a_7 = v_2, (b_7 = vecToFlip_1, ((a_7.X * b_7.X) + (a_7.Y * b_7.Y)) + (a_7.Z * b_7.Z)))) < 0) {
                const v_3 = vecToFlip_1;
                x_1 = Vec_$ctor_Z7AD9E565(-v_3.X, -v_3.Y, -v_3.Z);
            }
            else {
                x_1 = vecToFlip_1;
            }
            v_2 = ((a_8 = v_2, (b_8 = x_1, Vec_$ctor_Z7AD9E565(a_8.X + b_8.X, a_8.Y + b_8.Y, a_8.Z + b_8.Z))));
            t = n;
        }
        if (!(((v_4 = v_2, ((v_4.X * v_4.X) + (v_4.Y * v_4.Y)) + (v_4.Z * v_4.Z))) > 1E-12)) {
            fail(concat("Points3D.normalOfPoints: points are in a line or sphere without clear normal: ", ...iList(pts)));
        }
        return v_2;
    }
}

export class Points {
    constructor() {
    }
}

export function Points_$reflection() {
    return class_type("Euclid.Points", undefined, Points);
}

export function Points_$ctor() {
    return new Points();
}

export function Points_areInLine_Z305FC6B8(a, b, c, distanceTolerance = 1E-06) {
    return Tria3D_isLinear_Z305FC6B8(a, b, c, distanceTolerance);
}

export function Points_getSignedArea_Z5FD8CF3C(ps) {
    return Points2D_getSignedArea_Z7CD03502(ps);
}

export function Points_closestPointIdx_Z46635007(pts, pt) {
    return Points3D_closestPointIdx_Z371982BD(pts, pt) | 0;
}

export function Points_closestPointIdx_Z302C2247(pts, pt) {
    return Points2D_closestPointIdx_Z7C0841BD(pts, pt) | 0;
}

export function Points_closestPoint_Z46635007(pts, pt) {
    return Points3D_closestPoint_Z371982BD(pts, pt);
}

export function Points_closestPoint_Z302C2247(pts, pt) {
    return Points2D_closestPoint_Z7C0841BD(pts, pt);
}

export function Points_closestPointsIdx_2E406340(xs, ys) {
    return Points3D_closestPointsIdx_Z1881DE00(xs, ys);
}

export function Points_closestPointsIdx_52A7980(xs, ys) {
    return Points2D_closestPointsIdx_6A06E040(xs, ys);
}

export function Points_closestOfTwo(pt1, pt2, referencePoint) {
    return Points3D_closestOfTwo(pt1, pt2, referencePoint);
}

export function Points_minDistBetweenPointSets_2E406340(xs, ys) {
    return Points3D_minDistBetweenPointSets_Z1881DE00(xs, ys);
}

export function Points_minDistBetweenPointSets_52A7980(xs, ys) {
    return Points2D_minDistBetweenPointSets_6A06E040(xs, ys);
}

export function Points_mostDistantPointIdx_2E406340(findPointFrom, checkAgainst) {
    return Points3D_mostDistantPointIdx_Z1881DE00(findPointFrom, checkAgainst);
}

export function Points_mostDistantPointIdx_52A7980(findPointFrom, checkAgainst) {
    return Points2D_mostDistantPointIdx_6A06E040(findPointFrom, checkAgainst);
}

export function Points_mostDistantPoint_2E406340(findPointFrom, checkAgainst) {
    return Points3D_mostDistantPoint_Z1881DE00(findPointFrom, checkAgainst);
}

export function Points_mostDistantPoint_52A7980(findPointFrom, checkAgainst) {
    return Points2D_mostDistantPoint_6A06E040(findPointFrom, checkAgainst);
}

export function Points_cullDuplicatePointsInSeq_21159971(pts, tolerance) {
    return Points3D_cullDuplicatePointsInSeq_21159971(pts, tolerance);
}

export function Points_cullDuplicatePointsInSeq_Z4CAB181(pts, tolerance) {
    return Points2D_cullDuplicatePointsInSeq_Z4CAB181(pts, tolerance);
}

export function Points_findContinuousPoints_53EC9D26(ptss, tolGap) {
    return Points2D_findContinuousPoints_53EC9D26(ptss, tolGap);
}

export function Points_findContinuousPoints_503DFFA8(ptss, tolGap) {
    return Points3D_findContinuousPoints_503DFFA8(ptss, tolGap);
}

export function Points_findContinuosPoints_503DFFA8(ptss, tolGap) {
    return Points3D_findContinuousPoints_503DFFA8(ptss, tolGap);
}

export function Points_findContinuosPoints_53EC9D26(ptss, tolGap) {
    return Points2D_findContinuousPoints_53EC9D26(ptss, tolGap);
}

export function Points_center_516DFD0A(pts) {
    return Points3D_center_Z16D69110(pts);
}

export function Points_normalOfPoints_516DFD0A(pts) {
    return Points3D_normalOfPoints_Z16D69110(pts);
}

export function Points_normalOfPoints_Z6C25B48(pts) {
    return Points3D_normalOfPoints_Z16D69110(pts);
}

export function Points_offsetInCornerEx2D_Z40B1F03A(_thisPt, _prevToThis, _thisToNext, _prevDist, _nextDist, _referenceOrient) {
    return fail("Points.offsetInCornerEx2D with referenceOrient is deprecated, referenceOrient is ignored, use Tria2D.offsetPtVar instead.");
}

export function Points_offsetInCornerEx2D_5AB4BA6(_prevPt, _thisPt, _nextPt, _prevDist, _nextDist, _referenceOrient) {
    return fail("Points.offsetInCornerEx2D with referenceOrient is deprecated, referenceOrient is ignored, use Tria2D.offsetPtVar instead.");
}

export function Points_offsetInCorner_Z6044974D(thisPt, prevToThis, thisToNext, prevDist, nextDist) {
    let p, v, p_1, v_1;
    return Tria3D_offsetVar_Z62609A8D(thisPt, (p = thisPt, (v = prevToThis, Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z))), (p_1 = thisPt, (v_1 = thisToNext, Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z))), prevDist, nextDist);
}

export function Points_offsetInCorner_Z62609A8D(prevPt, thisPt, nextPt, prevDist, nextDist) {
    return Tria3D_offsetVar_Z62609A8D(prevPt, thisPt, nextPt, prevDist, nextDist);
}

export function Points_offsetInCornerEx_5199451A(_thisPt, _prevToThis, _thisToNext, _prevDist, _nextDist, _referenceNormal) {
    return fail("Points.offsetInCornerEx with referenceNormal is deprecated, referenceNormal is ignored, use Tria3D.offsetVar instead.");
}

export function Points_offsetInCornerEx_Z68C2D626(_prevPt, _thisPt, _nextPt, _prevDist, _nextDist, _referenceNormal) {
    return fail("Points.offsetInCornerEx with referenceNormal is deprecated, referenceNormal is ignored, use Tria3D.offsetVar instead.");
}

