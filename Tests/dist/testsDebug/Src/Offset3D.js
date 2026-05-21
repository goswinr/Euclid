
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { UnitVec_$ctor_Z7AD9E565, UnitVec_$reflection } from "./UnitVec.js";
import { int32_type, record_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { failRarr, fail } from "./EuclidErrors.js";
import { setItem, item as item_1 } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { Vec_$ctor_Z7AD9E565 } from "./Vec.js";
import { Euclid_Cosine_inDegrees } from "./UtilEuclid.js";
import { failTooSmall2, failUnit3 } from "./EuclidErrors.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Pnt.js";
import { Line3D_$ctor_5A6659A0 } from "./Line3D.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Pnt.js";
import { count } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";

/**
 * Holds the offset directions at a vertex.
 * `prevInPlane` is the unit vector in the local plane pointing perpendicular to the previous segment,
 * `nextInPlane` is the unit vector in the local plane pointing perpendicular to the next segment,
 * `perpDir` is the unit vector perpendicular to the local plane made by the adjacent segments.
 */
export class OffsetDirection extends Record {
    constructor(prevInPlane, nextInPlane, perpDir) {
        super();
        this.prevInPlane = prevInPlane;
        this.nextInPlane = nextInPlane;
        this.perpDir = perpDir;
    }
}

export function OffsetDirection_$reflection() {
    return record_type("Euclid.Offset3D.OffsetDirection", [], OffsetDirection, () => [["prevInPlane", UnitVec_$reflection()], ["nextInPlane", UnitVec_$reflection()], ["perpDir", UnitVec_$reflection()]]);
}

/**
 * For a point where the segments before and after are colinear,
 * this structure holds the index of the point, and the indices of the previous and next
 * points that have valid offset directions.
 * `prevOK` may be bigger than `nextOK` if the colinear point is at the start or end of a closed polyline.
 */
export class ColinearPnt extends Record {
    constructor(idx, prevOK, nextOK) {
        super();
        this.idx = (idx | 0);
        this.prevOK = (prevOK | 0);
        this.nextOK = (nextOK | 0);
    }
}

export function ColinearPnt_$reflection() {
    return record_type("Euclid.Offset3D.ColinearPnt", [], ColinearPnt, () => [["idx", int32_type], ["prevOK", int32_type], ["nextOK", int32_type]]);
}

/**
 * Computes the unit vectors of each segment in the polyline defined by pts.
 * Fails if any two consecutive points are identical.
 * Returns a ResizeArray of UnitVec, one less than the number of input points.
 */
export function getSegmentUnitVectors(pts) {
    if (pts.length < 2) {
        fail(`Offset3D.GetSegmentUnitVectors: pts.Count ${pts.length} must be at least 2 for a polyline.`);
    }
    const uvs = [];
    let prevPt = item_1(0, pts);
    for (let i = 1; i <= (pts.length - 1); i++) {
        const p = item_1(i, pts);
        let v;
        const a = p;
        const b = prevPt;
        v = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
        let len;
        const v_1 = v;
        len = Math.sqrt(((v_1.X * v_1.X) + (v_1.Y * v_1.Y)) + (v_1.Z * v_1.Z));
        if (len < 1E-06) {
            fail(`Offset3D.GetSegmentUnitVectors: pts.[${i}] and pts.[${i - 1}] are the same at ${prevPt}`);
        }
        const f = 1 / len;
        void (uvs.push(UnitVec_$ctor_Z7AD9E565(v.X * f, v.Y * f, v.Z * f)));
        prevPt = p;
    }
    return uvs;
}

/**
 * For points between colinear segments, ValueNone is returned.
 * For a open polyline, the first and last points will be ValueSome,
 * For a closed polyline, the first and last points will be identical, and they may be ValueNone if colinear.
 * Segments are considered colinear if the cosine of the angle between them is less than considerColinearBelow.
 * Special case: If all points are in a line the first and last point will have directions derived from the refNormal. The inner points will be ValueNone.
 * Fails if a U-turn exceeding failAtUTurnAbove is detected.
 */
export function getOffsetDirections(uvs, refNormal, isOpen, considerColinearBelow, failAtUTurnAbove) {
    const ptsCount = (uvs.length + 1) | 0;
    const dirs = [];
    let firstDirPending = isOpen;
    let fromIdx = 0;
    let uPrev;
    const this$ = uvs;
    if (this$.length === 0) {
        failRarr("Last.get", this$);
    }
    uPrev = item_1(this$.length - 1, this$);
    if (isOpen) {
        void (dirs.push(undefined));
        fromIdx = 1;
        uPrev = item_1(0, uvs);
    }
    let perp = refNormal;
    let nNext = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    let nPrev = UnitVec_$ctor_Z7AD9E565(0, 0, 1);
    for (let i = fromIdx; i <= (ptsCount - 2); i++) {
        let vecToFlip_1, v_1, a_1, b_1, x_5, y_2, z_2, l, f, a_2, b_2, v_2, a_3, b_3, a_4, b_4, a_5, b_5, a_6, b_6, a_7, b_7, a_8, b_8;
        const uNext = item_1(i, uvs);
        let dot;
        const a = uPrev;
        const b = uNext;
        dot = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
        if (dot < failAtUTurnAbove) {
            fail(`Offset3D.getOffsetDirections: ${Euclid_Cosine_inDegrees(dot)} degree U-turn detected at pts.[${i}]. exceeding failAtUTurnAbove: ${Euclid_Cosine_inDegrees(failAtUTurnAbove)}.`);
        }
        else if (dot > considerColinearBelow) {
            void (dirs.push(undefined));
        }
        else {
            perp = ((vecToFlip_1 = ((v_1 = ((a_1 = uPrev, (b_1 = uNext, Vec_$ctor_Z7AD9E565((a_1.Y * b_1.Z) - (a_1.Z * b_1.Y), (a_1.Z * b_1.X) - (a_1.X * b_1.Z), (a_1.X * b_1.Y) - (a_1.Y * b_1.X))))), (x_5 = v_1.X, (y_2 = v_1.Y, (z_2 = v_1.Z, (l = Math.sqrt(((x_5 * x_5) + (y_2 * y_2)) + (z_2 * z_2)), (!(l > 1E-12) ? failUnit3("Vec.unitize", x_5, y_2, z_2) : undefined, (f = (1 / l), UnitVec_$ctor_Z7AD9E565(f * x_5, f * y_2, f * z_2))))))))), (((a_2 = perp, (b_2 = vecToFlip_1, ((a_2.X * b_2.X) + (a_2.Y * b_2.Y)) + (a_2.Z * b_2.Z)))) < 0) ? ((v_2 = vecToFlip_1, UnitVec_$ctor_Z7AD9E565(-v_2.X, -v_2.Y, -v_2.Z))) : vecToFlip_1));
            nNext = ((a_3 = perp, (b_3 = uNext, ((Math.abs((a_4 = a_3, (b_4 = b_3, ((a_4.X * b_4.X) + (a_4.Y * b_4.Y)) + (a_4.Z * b_4.Z)))) > 1E-09) ? fail(`Offset3D.crossProduct: input UnitVec are not perpendicular: dot product is ${(a_5 = a_3, (b_5 = b_3, ((a_5.X * b_5.X) + (a_5.Y * b_5.Y)) + (a_5.Z * b_5.Z)))}.`) : undefined, UnitVec_$ctor_Z7AD9E565((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X))))));
            nPrev = ((a_6 = perp, (b_6 = uPrev, ((Math.abs((a_7 = a_6, (b_7 = b_6, ((a_7.X * b_7.X) + (a_7.Y * b_7.Y)) + (a_7.Z * b_7.Z)))) > 1E-09) ? fail(`Offset3D.crossProduct: input UnitVec are not perpendicular: dot product is ${(a_8 = a_6, (b_8 = b_6, ((a_8.X * b_8.X) + (a_8.Y * b_8.Y)) + (a_8.Z * b_8.Z)))}.`) : undefined, UnitVec_$ctor_Z7AD9E565((a_6.Y * b_6.Z) - (a_6.Z * b_6.Y), (a_6.Z * b_6.X) - (a_6.X * b_6.Z), (a_6.X * b_6.Y) - (a_6.Y * b_6.X))))));
            void (dirs.push(new OffsetDirection(nPrev, nNext, perp)));
            if (firstDirPending) {
                setItem(dirs, 0, new OffsetDirection(nPrev, nPrev, perp));
                firstDirPending = false;
            }
        }
        uPrev = uNext;
    }
    if (firstDirPending) {
        let n;
        let v_4;
        const a_9 = refNormal;
        const b_9 = item_1(0, uvs);
        v_4 = Vec_$ctor_Z7AD9E565((a_9.Y * b_9.Z) - (a_9.Z * b_9.Y), (a_9.Z * b_9.X) - (a_9.X * b_9.Z), (a_9.X * b_9.Y) - (a_9.Y * b_9.X));
        const x_11 = v_4.X;
        const y_7 = v_4.Y;
        const z_7 = v_4.Z;
        const l_1 = Math.sqrt(((x_11 * x_11) + (y_7 * y_7)) + (z_7 * z_7));
        if (!(l_1 > 1E-12)) {
            failUnit3("Vec.unitize", x_11, y_7, z_7);
        }
        const f_1 = 1 / l_1;
        n = UnitVec_$ctor_Z7AD9E565(f_1 * x_11, f_1 * y_7, f_1 * z_7);
        const dir = new OffsetDirection(n, n, refNormal);
        for (let i_1 = 0; i_1 <= (dirs.length - 1); i_1++) {
            setItem(dirs, i_1, dir);
        }
        void (dirs.push(dir));
    }
    else if (isOpen) {
        const item = new OffsetDirection(nNext, nNext, perp);
        void (dirs.push(item));
    }
    else {
        void (dirs.push(item_1(0, dirs)));
    }
    return dirs;
}

/**
 * For all dirs that are ValueNone this will find the index of adjacent points that are OK.
 * Works also for closed polylines where initial and end segments are colinear
 */
export function getColinearNeighbors(dirs) {
    let prevIdx = -1;
    let nextIdx = -1;
    let j = -1;
    const colinearPts = [];
    for (let i = 0; i <= (dirs.length - 1); i++) {
        const matchValue = item_1(i, dirs);
        if (matchValue == null) {
            if (nextIdx < i) {
                j = ((i - 1) | 0);
                while (j >= 0) {
                    const matchValue_1 = item_1(j, dirs);
                    if (matchValue_1 == null) {
                        j = ((j - 1) | 0);
                    }
                    else {
                        prevIdx = (j | 0);
                        j = -99;
                    }
                }
                if (j === -1) {
                    j = ((dirs.length - 1) | 0);
                    while (j >= 0) {
                        const matchValue_2 = item_1(j, dirs);
                        if (matchValue_2 == null) {
                            j = ((j - 1) | 0);
                        }
                        else {
                            prevIdx = (j | 0);
                            j = -999;
                        }
                    }
                    if (j === 0) {
                        fail("Offset3D.offsetConstant: could not find previous valid offset direction for colinear point in closed polyline.");
                    }
                }
                j = ((i + 1) | 0);
                while (j < dirs.length) {
                    const matchValue_3 = item_1(j, dirs);
                    if (matchValue_3 == null) {
                        j = ((j + 1) | 0);
                    }
                    else {
                        nextIdx = (j | 0);
                        j = 999999999;
                    }
                }
                if (j === dirs.length) {
                    j = 0;
                    while (j < dirs.length) {
                        const matchValue_4 = item_1(j, dirs);
                        if (matchValue_4 == null) {
                            j = ((j + 1) | 0);
                        }
                        else {
                            nextIdx = (j | 0);
                            j = 1000000000;
                        }
                    }
                    if (j === dirs.length) {
                        fail("Offset3D.offsetConstant: could not find next valid offset direction for colinear point in closed polyline.");
                    }
                }
            }
            void (colinearPts.push(new ColinearPnt(i, prevIdx, nextIdx)));
        }
    }
    return colinearPts;
}

/**
 * Offsets a polyline by a constant distance.
 */
export function offsetConstantWithDirections(pts, dirs, distInPlane, distPerpendicular) {
    if (pts.length < 2) {
        fail(`Offset3D.offsetConstant: pts.Count ${pts.length} must be at least 2 for a polyline.`);
    }
    const res = [];
    let needsSecondPass = false;
    for (let i = 0; i <= (pts.length - 1); i++) {
        const pt = item_1(i, pts);
        const matchValue = item_1(i, dirs);
        if (matchValue != null) {
            const dir = matchValue;
            let cosine;
            const a = dir.prevInPlane;
            const b = dir.nextInPlane;
            cosine = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
            let inPlanePt;
            const pt_1 = pt;
            const dist = distInPlane;
            const nPrev = dir.prevInPlane;
            const nNext = dir.nextInPlane;
            const cosine_1 = cosine;
            if (cosine_1 < -0.9999999) {
                fail(`Offset3D.setOffCorner: cosine is ${cosine_1} , a 180 degree U-turn, which is not allowed here for dist ${dist}, nPrev ${nPrev}, nNext ${nNext} at pt ${pt_1}.`);
            }
            const p = pt_1;
            let v;
            let a_2;
            const a_1 = nPrev;
            const b_1 = nNext;
            a_2 = Vec_$ctor_Z7AD9E565(a_1.X + b_1.X, a_1.Y + b_1.Y, a_1.Z + b_1.Z);
            const f = dist / (1 + cosine_1);
            v = Vec_$ctor_Z7AD9E565(a_2.X * f, a_2.Y * f, a_2.Z * f);
            inPlanePt = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
            let offsetPt;
            const p_1 = inPlanePt;
            let v_1;
            const a_3 = dir.perpDir;
            const f_1 = distPerpendicular;
            v_1 = Vec_$ctor_Z7AD9E565(a_3.X * f_1, a_3.Y * f_1, a_3.Z * f_1);
            offsetPt = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
            void (res.push(offsetPt));
        }
        else {
            needsSecondPass = true;
            void (res.push(pt));
        }
    }
    if (needsSecondPass) {
        const clPnts = getColinearNeighbors(dirs);
        for (let i_1 = 0; i_1 <= (clPnts.length - 1); i_1++) {
            let ln_3, p_4, x, y, z, lenSq, u, v_2, w, dot, t, x$0027, y$0027, z$0027;
            const cP = item_1(i_1, clPnts);
            const ln = Line3D_$ctor_5A6659A0(item_1(cP.prevOK, res), item_1(cP.nextOK, res));
            setItem(res, cP.idx, (ln_3 = ln, (p_4 = item_1(cP.idx, pts), (x = (ln_3.FromX - ln_3.ToX), (y = (ln_3.FromY - ln_3.ToY), (z = (ln_3.FromZ - ln_3.ToZ), (lenSq = (((x * x) + (y * y)) + (z * z)), (!(lenSq > 1E-12) ? failTooSmall2("Line3D.RayClosestPoint", ln_3, p_4) : undefined, (u = (ln_3.FromX - p_4.X), (v_2 = (ln_3.FromY - p_4.Y), (w = (ln_3.FromZ - p_4.Z), (dot = (((x * u) + (y * v_2)) + (z * w)), (t = (dot / lenSq), (x$0027 = (ln_3.FromX - (x * t)), (y$0027 = (ln_3.FromY - (y * t)), (z$0027 = (ln_3.FromZ - (z * t)), Pnt_$ctor_Z7AD9E565_1(x$0027, y$0027, z$0027)))))))))))))))));
        }
    }
    return res;
}

/**
 * Offsets a polyline by variable distances.
 */
export function offsetVariableWithDirections(pts, segmentDirs, offDirs, distsInPlane, distsPerpendicular, isClosed, varDistParallelBehavior) {
    if (pts.length < 2) {
        fail(`Offset3D.offsetVariable: pts.Count ${pts.length} must be at least 2 for a polyline.`);
    }
    if (pts.length !== (segmentDirs.length + 1)) {
        fail(`Offset3D.offsetVariableWithDirections:
   Point count must be 1 greater than normal directions count, but they are ${pts.length} and ${segmentDirs.length}.`);
    }
    if (pts.length !== (count(distsInPlane) + 1)) {
        fail(`Offset3D.offsetVariableWithDirections:
   Point count must be 1 greater than offset distances count, but they are ${pts.length} and ${count(distsInPlane)}.`);
    }
    let res = [];
    let needsSecondPass = false;
    let prevDistInPlane = isClosed ? item_1(count(distsInPlane) - 1, distsInPlane) : item_1(0, distsInPlane);
    for (let i = 0; i <= ((pts.length - 1) - 1); i++) {
        const pt = item_1(i, pts);
        const nextDistInPlane = item_1(i, distsInPlane);
        const distPerp = item_1(i, distsPerpendicular);
        const matchValue = item_1(i, offDirs);
        if (matchValue != null) {
            const dir = matchValue;
            let cosine;
            const a = dir.prevInPlane;
            const b = dir.nextInPlane;
            cosine = (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z));
            let inPlanePt;
            const distPrev = prevDistInPlane;
            const distNext = nextDistInPlane;
            const nPrev = dir.prevInPlane;
            const vNext = item_1(i, segmentDirs);
            const delta = distPrev - distNext;
            let cos2;
            const a_1 = nPrev;
            const b_1 = vNext;
            cos2 = (((a_1.X * b_1.X) + (a_1.Y * b_1.Y)) + (a_1.Z * b_1.Z));
            let p_1;
            const p = pt;
            let v;
            const a_2 = vNext;
            const f = delta / cos2;
            v = Vec_$ctor_Z7AD9E565(a_2.X * f, a_2.Y * f, a_2.Z * f);
            p_1 = Pnt_$ctor_Z7AD9E565(p.X + v.X, p.Y + v.Y, p.Z + v.Z);
            let v_1;
            let a_4;
            const a_3 = nPrev;
            const b_2 = dir.nextInPlane;
            a_4 = Vec_$ctor_Z7AD9E565(a_3.X + b_2.X, a_3.Y + b_2.Y, a_3.Z + b_2.Z);
            const f_1 = distNext / (1 + cosine);
            v_1 = Vec_$ctor_Z7AD9E565(a_4.X * f_1, a_4.Y * f_1, a_4.Z * f_1);
            inPlanePt = Pnt_$ctor_Z7AD9E565(p_1.X + v_1.X, p_1.Y + v_1.Y, p_1.Z + v_1.Z);
            let offsetPt;
            const p_2 = inPlanePt;
            let v_2;
            const a_5 = dir.perpDir;
            const f_2 = distPerp;
            v_2 = Vec_$ctor_Z7AD9E565(a_5.X * f_2, a_5.Y * f_2, a_5.Z * f_2);
            offsetPt = Pnt_$ctor_Z7AD9E565(p_2.X + v_2.X, p_2.Y + v_2.Y, p_2.Z + v_2.Z);
            void (res.push(offsetPt));
        }
        else {
            needsSecondPass = true;
            void (res.push(pt));
        }
    }
    let pt_2;
    const this$_1 = pts;
    if (this$_1.length === 0) {
        failRarr("Last.get", this$_1);
    }
    pt_2 = item_1(this$_1.length - 1, this$_1);
    const nextDistInPlane_1 = isClosed ? item_1(0, distsInPlane) : item_1(count(distsInPlane) - 1, distsInPlane);
    const distPerp_1 = item_1(count(distsPerpendicular) - 1, distsPerpendicular);
    let matchValue_1;
    const this$_2 = offDirs;
    if (this$_2.length === 0) {
        failRarr("Last.get", this$_2);
    }
    matchValue_1 = item_1(this$_2.length - 1, this$_2);
    if (matchValue_1 != null) {
        const dir_1 = matchValue_1;
        let cosine_2;
        const a_6 = dir_1.prevInPlane;
        const b_3 = dir_1.nextInPlane;
        cosine_2 = (((a_6.X * b_3.X) + (a_6.Y * b_3.Y)) + (a_6.Z * b_3.Z));
        let inPlanePt_1;
        const distPrev_1 = prevDistInPlane;
        const distNext_1 = nextDistInPlane_1;
        const nPrev_1 = dir_1.prevInPlane;
        const vNext_1 = item_1(segmentDirs.length - 1, segmentDirs);
        const delta_1 = distPrev_1 - distNext_1;
        let cos2_1;
        const a_7 = nPrev_1;
        const b_4 = vNext_1;
        cos2_1 = (((a_7.X * b_4.X) + (a_7.Y * b_4.Y)) + (a_7.Z * b_4.Z));
        let p_4;
        const p_3 = pt_2;
        let v_3;
        const a_8 = vNext_1;
        const f_3 = delta_1 / cos2_1;
        v_3 = Vec_$ctor_Z7AD9E565(a_8.X * f_3, a_8.Y * f_3, a_8.Z * f_3);
        p_4 = Pnt_$ctor_Z7AD9E565(p_3.X + v_3.X, p_3.Y + v_3.Y, p_3.Z + v_3.Z);
        let v_4;
        let a_10;
        const a_9 = nPrev_1;
        const b_5 = dir_1.nextInPlane;
        a_10 = Vec_$ctor_Z7AD9E565(a_9.X + b_5.X, a_9.Y + b_5.Y, a_9.Z + b_5.Z);
        const f_4 = distNext_1 / (1 + cosine_2);
        v_4 = Vec_$ctor_Z7AD9E565(a_10.X * f_4, a_10.Y * f_4, a_10.Z * f_4);
        inPlanePt_1 = Pnt_$ctor_Z7AD9E565(p_4.X + v_4.X, p_4.Y + v_4.Y, p_4.Z + v_4.Z);
        let offsetPt_1;
        const p_5 = inPlanePt_1;
        let v_5;
        const a_11 = dir_1.perpDir;
        const f_5 = distPerp_1;
        v_5 = Vec_$ctor_Z7AD9E565(a_11.X * f_5, a_11.Y * f_5, a_11.Z * f_5);
        offsetPt_1 = Pnt_$ctor_Z7AD9E565(p_5.X + v_5.X, p_5.Y + v_5.Y, p_5.Z + v_5.Z);
        void (res.push(offsetPt_1));
    }
    else {
        needsSecondPass = true;
        void (res.push(pt_2));
    }
    if (needsSecondPass) {
        switch (varDistParallelBehavior) {
            case 1: {
                fail("Offset3D.offsetVariable: colinear segments with different offset distances found, cannot offset with VarDistParallel.Fail behavior.");
                break;
            }
            case 2: {
                const newRes = [];
                for (let i_1 = 0; i_1 <= (pts.length - 1); i_1++) {
                    const matchValue_2 = item_1(i_1, offDirs);
                    if (matchValue_2 == null) {
                    }
                    else {
                        void (newRes.push(item_1(i_1, res)));
                    }
                }
                res = newRes;
                break;
            }
            case 3: {
                const clPnts = getColinearNeighbors(offDirs);
                for (let i_2 = 0; i_2 <= (clPnts.length - 1); i_2++) {
                    let ln_2, p_7, ln_3, ln_4, ln_5;
                    const cP = item_1(i_2, clPnts);
                    const origLn = Line3D_$ctor_5A6659A0(item_1(cP.prevOK, pts), item_1(cP.nextOK, pts));
                    let t;
                    const ln = origLn;
                    const p_6 = item_1(cP.idx, pts);
                    const x = ln.FromX - ln.ToX;
                    const y = ln.FromY - ln.ToY;
                    const z = ln.FromZ - ln.ToZ;
                    const lenSq = ((x * x) + (y * y)) + (z * z);
                    if (!(lenSq > 1E-12)) {
                        failTooSmall2("Line3D.RayClosestParameter", ln, p_6);
                    }
                    const u = ln.FromX - p_6.X;
                    const v_6 = ln.FromY - p_6.Y;
                    const w = ln.FromZ - p_6.Z;
                    const dot = ((x * u) + (y * v_6)) + (z * w);
                    t = (dot / lenSq);
                    const ln_1 = Line3D_$ctor_5A6659A0(item_1(cP.prevOK, res), item_1(cP.nextOK, res));
                    setItem(res, cP.idx, (ln_2 = ln_1, (p_7 = t, Pnt_$ctor_Z7AD9E565_1(ln_2.FromX + (((ln_3 = ln_2, ln_3.ToX - ln_3.FromX)) * p_7), ln_2.FromY + (((ln_4 = ln_2, ln_4.ToY - ln_4.FromY)) * p_7), ln_2.FromZ + (((ln_5 = ln_2, ln_5.ToZ - ln_5.FromZ)) * p_7)))));
                }
                break;
            }
            case 4: {
                const clPnts_1 = getColinearNeighbors(offDirs);
                for (let i_3 = 0; i_3 <= (clPnts_1.length - 1); i_3++) {
                    let ln_9, p_10, x_2, y_1, z_1, lenSq_1, u_1, v_7, w_1, dot_1, t_1, x$0027, y$0027, z$0027;
                    const cP_1 = item_1(i_3, clPnts_1);
                    const ln_6 = Line3D_$ctor_5A6659A0(item_1(cP_1.prevOK, res), item_1(cP_1.nextOK, res));
                    setItem(res, cP_1.idx, (ln_9 = ln_6, (p_10 = item_1(cP_1.idx, pts), (x_2 = (ln_9.FromX - ln_9.ToX), (y_1 = (ln_9.FromY - ln_9.ToY), (z_1 = (ln_9.FromZ - ln_9.ToZ), (lenSq_1 = (((x_2 * x_2) + (y_1 * y_1)) + (z_1 * z_1)), (!(lenSq_1 > 1E-12) ? failTooSmall2("Line3D.RayClosestPoint", ln_9, p_10) : undefined, (u_1 = (ln_9.FromX - p_10.X), (v_7 = (ln_9.FromY - p_10.Y), (w_1 = (ln_9.FromZ - p_10.Z), (dot_1 = (((x_2 * u_1) + (y_1 * v_7)) + (z_1 * w_1)), (t_1 = (dot_1 / lenSq_1), (x$0027 = (ln_9.FromX - (x_2 * t_1)), (y$0027 = (ln_9.FromY - (y_1 * t_1)), (z$0027 = (ln_9.FromZ - (z_1 * t_1)), Pnt_$ctor_Z7AD9E565_1(x$0027, y$0027, z$0027)))))))))))))))));
                }
                break;
            }
            default:
                fail(`Offset3D.offsetVariable: unknown VarDistParallel behavior ${varDistParallelBehavior}.`);
        }
    }
    return res;
}

/**
 * Returns the average normal vector of the Polyline3D.
 * It is calculated by summing up the cross products of all segments around the center point.
 * Does not check for bad input, may be zero length if points are colinear.
 */
export function averageNormal(pts) {
    let x = 0;
    let y = 0;
    let z = 0;
    for (let i = 0; i <= (pts.length - 1); i++) {
        const p = item_1(i, pts);
        x = (x + p.X);
        y = (y + p.Y);
        z = (z + p.Z);
    }
    const c = Pnt_$ctor_Z7AD9E565(x / pts.length, y / pts.length, z / pts.length);
    let normal = Vec_$ctor_Z7AD9E565(0, 0, 0);
    let a_1;
    const a = item_1(pts.length - 1, pts);
    const b = c;
    a_1 = Vec_$ctor_Z7AD9E565(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    for (let i_1 = 0; i_1 <= (pts.length - 1); i_1++) {
        let a_4, b_4, a_3, b_3;
        let b_2;
        const a_2 = item_1(i_1, pts);
        const b_1 = c;
        b_2 = Vec_$ctor_Z7AD9E565(a_2.X - b_1.X, a_2.Y - b_1.Y, a_2.Z - b_1.Z);
        normal = ((a_4 = normal, (b_4 = ((a_3 = a_1, (b_3 = b_2, Vec_$ctor_Z7AD9E565((a_3.Y * b_3.Z) - (a_3.Z * b_3.Y), (a_3.Z * b_3.X) - (a_3.X * b_3.Z), (a_3.X * b_3.Y) - (a_3.Y * b_3.X))))), Vec_$ctor_Z7AD9E565(a_4.X + b_4.X, a_4.Y + b_4.Y, a_4.Z + b_4.Z))));
        a_1 = b_2;
    }
    return normal;
}

/**
 * A constant-distance offset algorithm for closed or open polylines in 3D.
 * Uses default angles: colinear below 2.5 degrees, fails at U-turns above 175 degrees.
 */
export function offset$0027(refNormal, distInPlane, distPerpendicular, pts) {
    let a_1, this$, b_1, this$_1, x, y, z;
    const isOpen = ((a_1 = ((this$ = pts, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item_1(0, this$)))), (b_1 = ((this$_1 = pts, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item_1(this$_1.length - 1, this$_1)))), (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x * x) + (y * y)) + (z * z))))))) > 1E-12;
    const uvs = getSegmentUnitVectors(pts);
    const dirs = getOffsetDirections(uvs, refNormal, isOpen, 0.9990482215818578, -0.9961946980917455);
    return offsetConstantWithDirections(pts, dirs, distInPlane, distPerpendicular);
}

/**
 * A constant-distance offset algorithm for closed or open polylines in 3D.
 * Uses default angles: colinear below 2.5 degrees, fails at U-turns above 175 degrees.
 */
export function offset(distInPlane, distPerpendicular, pts) {
    let refNormal;
    const v_1 = averageNormal(pts);
    const x = v_1.X;
    const y = v_1.Y;
    const z = v_1.Z;
    const l = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (!(l > 1E-12)) {
        failUnit3("Vec.unitize", x, y, z);
    }
    const f = 1 / l;
    refNormal = UnitVec_$ctor_Z7AD9E565(f * x, f * y, f * z);
    return offset$0027(refNormal, distInPlane, distPerpendicular, pts);
}

/**
 * Offsetting each segment by its own distance. For closed or open polylines in 3D.
 * Uses default angles: colinear below 2.5 degrees, fails at U-turns above 175 degrees.
 */
export function offsetVariable$0027(varDistParallelBehavior, refNormal, distancesInPlane, distancesPerpendicular, pts) {
    let a_1, this$, b_1, this$_1, x, y, z;
    const isOpen = ((a_1 = ((this$ = pts, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item_1(0, this$)))), (b_1 = ((this$_1 = pts, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item_1(this$_1.length - 1, this$_1)))), (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x * x) + (y * y)) + (z * z))))))) > 1E-12;
    const uvs = getSegmentUnitVectors(pts);
    const dirs = getOffsetDirections(uvs, refNormal, isOpen, 0.9990482215818578, -0.9961946980917455);
    return offsetVariableWithDirections(pts, uvs, dirs, distancesInPlane, distancesPerpendicular, isOpen, varDistParallelBehavior);
}

/**
 * Offsetting each segment by its own distance. For closed or open polylines in 3D.
 * Fails if colinear segments are found (uses VarDistParallel.Fail).
 * Uses default angles: colinear below 2.5 degrees, fails at U-turns above 175 degrees.
 */
export function offsetVariable(distancesInPlane, distancesPerpendicular, pts) {
    let refNormal;
    const v_1 = averageNormal(pts);
    const x = v_1.X;
    const y = v_1.Y;
    const z = v_1.Z;
    const l = Math.sqrt(((x * x) + (y * y)) + (z * z));
    if (!(l > 1E-12)) {
        failUnit3("Vec.unitize", x, y, z);
    }
    const f = 1 / l;
    refNormal = UnitVec_$ctor_Z7AD9E565(f * x, f * y, f * z);
    return offsetVariable$0027(1, refNormal, distancesInPlane, distancesPerpendicular, pts);
}

