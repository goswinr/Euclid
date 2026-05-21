
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { insertRangeInPlace, addRangeInPlace, item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { count } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { failRarr, fail } from "./EuclidErrors.js";
import { ResizeArr_rev, ResizeArr_minIndexBy, ResizeArr_maxIndexBy } from "./ResizeArr.js";

/**
 * A type containing only static member functions for operating on multiple 2D points or set of 2D points.
 * Aka point-clouds
 */
export class Points2D {
    constructor() {
    }
}

export function Points2D_$reflection() {
    return class_type("Euclid.Points2D", undefined, Points2D);
}

/**
 * The sign is negative if the loop is clockwise.
 * Last and first point should be the same.
 */
export function Points2D_getSignedArea_Z7CD03502(ps) {
    let area = 0;
    let t = item(0, ps);
    for (let i = 1; i <= (count(ps) - 1); i++) {
        const n = item(i, ps);
        const a = t.X - n.X;
        const b = n.Y + t.Y;
        area = (area + (a * b));
        t = n;
    }
    return area * 0.5;
}

/**
 * Returns the closest 2D point index from a 2D point list to a given 2D point.
 */
export function Points2D_closestPointIdx_Z7C0841BD(pts, pt) {
    if (count(pts) === 0) {
        fail("Points2D.closestPoint: empty List of Points: pts");
    }
    let mi = -1;
    let mid = 1.7976931348623157E+308;
    for (let i = 0; i <= (count(pts) - 1); i++) {
        const p = item(i, pts);
        let d;
        const a_1 = p;
        const b_1 = pt;
        const vx = a_1.X - b_1.X;
        const vy = a_1.Y - b_1.Y;
        d = ((vx * vx) + (vy * vy));
        if (d < mid) {
            mid = d;
            mi = (i | 0);
        }
    }
    return mi | 0;
}

/**
 * Returns the closest 2D point from a point list to a given 2D point.
 */
export function Points2D_closestPoint_Z7C0841BD(pts, pt) {
    return item(Points2D_closestPointIdx_Z7C0841BD(pts, pt), pts);
}

/**
 * Returns the closest of two 2D points to a given reference 2D point.
 * If both points are equidistant the first point is returned.
 */
export function Points2D_closestOfTwo(pt1, pt2, referencePoint) {
    let d1;
    const a_1 = pt1;
    const b_1 = referencePoint;
    const vx = a_1.X - b_1.X;
    const vy = a_1.Y - b_1.Y;
    d1 = ((vx * vx) + (vy * vy));
    let d2;
    const a_3 = pt2;
    const b_3 = referencePoint;
    const vx_1 = a_3.X - b_3.X;
    const vy_1 = a_3.Y - b_3.Y;
    d2 = ((vx_1 * vx_1) + (vy_1 * vy_1));
    if (d1 <= d2) {
        return pt1;
    }
    else {
        return pt2;
    }
}

/**
 * Returns the indices of the 2D points that are closest to each other.
 */
export function Points2D_closestPointsIdx_6A06E040(xs, ys) {
    if (count(xs) === 0) {
        fail("Points2D.closestPointsIdx: empty List of Points: xs");
    }
    if (count(ys) === 0) {
        fail("Points2D.closestPointsIdx: empty List of Points: ys");
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
            const vx = a_1.X - b_1.X;
            const vy = a_1.Y - b_1.Y;
            d = ((vx * vx) + (vy * vy));
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
 * Given two lists of 2D points finds the pair that are closest to each other and returns their distance.
 */
export function Points2D_minDistBetweenPointSets_6A06E040(xs, ys) {
    if (count(xs) === 0) {
        fail("Points2D.minDistBetweenPointSets: empty List of Points: xs");
    }
    if (count(ys) === 0) {
        fail("Points2D.minDistBetweenPointSets: empty List of Points: ys");
    }
    const patternInput = Points2D_closestPointsIdx_6A06E040(xs, ys);
    const j = patternInput[1] | 0;
    const i = patternInput[0] | 0;
    const a_1 = item(i, xs);
    const b_1 = item(j, ys);
    const vx = a_1.X - b_1.X;
    const vy = a_1.Y - b_1.Y;
    return Math.sqrt((vx * vx) + (vy * vy));
}

/**
 * Find the index of the 2D point that has the biggest distance to any 2D point from the other set.
 * Basically the most lonely point in 'findPointFrom' list with respect to 'checkAgainst' list.
 * Returns findPointFromIdx * checkAgainstIdx
 */
export function Points2D_mostDistantPointIdx_6A06E040(findPointFrom, checkAgainst) {
    if (count(findPointFrom) === 0) {
        fail("Points2D.mostDistantPoint: empty List of Points: findPointFrom");
    }
    if (count(checkAgainst) === 0) {
        fail("Points2D.mostDistantPoint: empty List of Points: checkAgainst");
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
            const vx = a_1.X - b_1.X;
            const vy = a_1.Y - b_1.Y;
            d = ((vx * vx) + (vy * vy));
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
 * Find the 2D point that has the biggest distance to any 2D point from another set.
 */
export function Points2D_mostDistantPoint_6A06E040(findPointFrom, checkAgainst) {
    const i = Points2D_mostDistantPointIdx_6A06E040(findPointFrom, checkAgainst)[0] | 0;
    return item(i, findPointFrom);
}

/**
 * Culls 2D points if they are too close to previous or next item.
 * Last and first 2D points stay the same.
 */
export function Points2D_cullDuplicatePointsInSeq_Z4CAB181(pts, tolerance) {
    if (pts.length === 0) {
        fail("Points2D.cullDuplicatePointsInSeq: empty List of Points");
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
            let a_1, b_1, vx, vy;
            const pt = item(i, pts);
            if (((a_1 = last, (b_1 = pt, (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), (vx * vx) + (vy * vy)))))) > tolSq) {
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
 * Similar to join polylines, this tries to find continuous sequences of 2D points.
 * 'tolGap' is the maximum allowable gap between the start and the endpoint of two segments.
 * Search starts from the segment with the most points.
 * Both start and end point of each 2D point list are checked for adjacency.
 */
export function Points2D_findContinuousPoints_53EC9D26(ptss, tolGap) {
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
            const vx = a_1.X - b_1.X;
            const vy = a_1.Y - b_1.Y;
            return (vx * vx) + (vy * vy);
        }, ptss) | 0;
        const ei = ResizeArr_minIndexBy((ps_1) => {
            const a_3 = ende;
            let b_3;
            const this$_2 = ps_1;
            if (this$_2.length === 0) {
                failRarr("Last.get", this$_2);
            }
            b_3 = item(this$_2.length - 1, this$_2);
            const vx_1 = a_3.X - b_3.X;
            const vy_1 = a_3.Y - b_3.Y;
            return (vx_1 * vx_1) + (vy_1 * vy_1);
        }, ptss) | 0;
        let sd;
        const a_5 = ende;
        let b_5;
        const this$_3 = item(si, ptss);
        if (this$_3.length === 0) {
            failRarr("First.get", this$_3);
        }
        b_5 = item(0, this$_3);
        const vx_2 = a_5.X - b_5.X;
        const vy_2 = a_5.Y - b_5.Y;
        sd = Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2));
        let ed;
        const a_7 = ende;
        let b_7;
        const this$_4 = item(ei, ptss);
        if (this$_4.length === 0) {
            failRarr("Last.get", this$_4);
        }
        b_7 = item(this$_4.length - 1, this$_4);
        const vx_3 = a_7.X - b_7.X;
        const vy_3 = a_7.Y - b_7.Y;
        ed = Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3));
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
                const vx_4 = a_9.X - b_9.X;
                const vy_4 = a_9.Y - b_9.Y;
                return (vx_4 * vx_4) + (vy_4 * vy_4);
            }, ptss) | 0;
            const ei_1 = ResizeArr_minIndexBy((ps_3) => {
                const a_11 = start;
                let b_11;
                const this$_8 = ps_3;
                if (this$_8.length === 0) {
                    failRarr("Last.get", this$_8);
                }
                b_11 = item(this$_8.length - 1, this$_8);
                const vx_5 = a_11.X - b_11.X;
                const vy_5 = a_11.Y - b_11.Y;
                return (vx_5 * vx_5) + (vy_5 * vy_5);
            }, ptss) | 0;
            let sd_1;
            const a_13 = start;
            let b_13;
            const this$_9 = item(si_1, ptss);
            if (this$_9.length === 0) {
                failRarr("First.get", this$_9);
            }
            b_13 = item(0, this$_9);
            const vx_6 = a_13.X - b_13.X;
            const vy_6 = a_13.Y - b_13.Y;
            sd_1 = Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6));
            let ed_1;
            const a_15 = start;
            let b_15;
            const this$_10 = item(ei_1, ptss);
            if (this$_10.length === 0) {
                failRarr("Last.get", this$_10);
            }
            b_15 = item(this$_10.length - 1, this$_10);
            const vx_7 = a_15.X - b_15.X;
            const vy_7 = a_15.Y - b_15.Y;
            ed_1 = Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7));
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

