
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { record_type, int32_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { Vc_$ctor_7B00E9A0, Vc_$reflection } from "./Vc.js";
import { failRarr, fail } from "./EuclidErrors.js";
import { getRange, setItem, addRangeInPlace, item as item_4 } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { UnitVc_$ctor_7B00E9A0 } from "./UnitVc.js";
import { interpolate } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { failUnit2, failTooSmall } from "./EuclidErrors.js";
import { Pt_$ctor_7B00E9A0 } from "./Pt.js";
import { Rotation2D_$ctor_7B00E9A0 } from "./Rotation2D.js";
import { Line2D_$ctor_Z53905FA0 } from "./Line2D.js";
import { Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD } from "./TypeExtensions/Line2D.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Pt.js";
import { value as value_1 } from "../fable_modules/fable-library-js.5.0.0/Option.js";
import { count } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";

/**
 * Internal only.
 * So that colinear points can be fixed after the main offsetting loop is done.
 */
export class IndexToFixProportional extends Record {
    constructor(idxRes, idxOrig) {
        super();
        this.idxRes = (idxRes | 0);
        this.idxOrig = (idxOrig | 0);
    }
}

export function IndexToFixProportional_$reflection() {
    return record_type("Euclid.Offset2D.IndexToFixProportional", [], IndexToFixProportional, () => [["idxRes", int32_type], ["idxOrig", int32_type]]);
}

export class IndexToProject extends Record {
    constructor(idx, dir) {
        super();
        this.idx = (idx | 0);
        this.dir = dir;
    }
}

export function IndexToProject_$reflection() {
    return record_type("Euclid.Offset2D.IndexToProject", [], IndexToProject, () => [["idx", int32_type], ["dir", Vc_$reflection()]]);
}

/**
 * Returns the normals of the segments of a polyline; each segment is rotated 90 degrees in counter-clockwise order.
 * The count is one less than the input points.
 * Fails on duplicate points.
 */
export function makeOffsetDirections(pts) {
    if (pts.length < 2) {
        fail(`Offset2D.makeOffsetDirections: pts.Count ${pts.length} must be at least 2 for a polyline.`);
    }
    const normals = [];
    let pp = item_4(0, pts);
    for (let i = 1; i <= (pts.length - 1); i++) {
        let v_2;
        const p = item_4(i, pts);
        let v;
        const a = p;
        const b = pp;
        v = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
        let len;
        const v_1 = v;
        const x = v_1.X;
        const y = v_1.Y;
        len = Math.sqrt((x * x) + (y * y));
        if (len < 1E-06) {
            fail(`Offset2D.makeOffsetDirections: pts.[${i}] and pts.[${i - 1}] are the same at ${pp}`);
        }
        const u = UnitVc_$ctor_7B00E9A0(v.X / len, v.Y / len);
        void (normals.push((v_2 = u, UnitVc_$ctor_7B00E9A0(-v_2.Y, v_2.X))));
        pp = p;
    }
    return normals;
}

export function handleUTurn(pt, cosine, nPrev, nNext, dist, res, uTurnBehavior, useUTurnBehaviorAbove) {
    let a_1, b_1;
    switch (uTurnBehavior) {
        case 1: {
            fail(`Offset2D.handleUTurn: pt ${pt} makes a ${interpolate("%.4f%P()", [57.29577951308232 * Math.acos(cosine)])} degree U-turn, max ${57.29577951308232 * Math.acos(useUTurnBehaviorAbove)} is allowed.`);
            break;
        }
        case 2: {
            let chamferTangent;
            let v;
            const a = nPrev;
            const b = nNext;
            v = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
            const x = v.X;
            const y = v.Y;
            const l = Math.sqrt((x * x) + (y * y));
            if (!(l > 1E-12)) {
                failTooSmall("Vc.Unitized", v);
            }
            chamferTangent = UnitVc_$ctor_7B00E9A0(x / l, y / l);
            let chamferNormal;
            const v_1 = chamferTangent;
            chamferNormal = UnitVc_$ctor_7B00E9A0(-v_1.Y, v_1.X);
            let chamferNormalChecked;
            if (((a_1 = chamferNormal, (b_1 = nPrev, (a_1.X * b_1.X) + (a_1.Y * b_1.Y)))) >= 0) {
                chamferNormalChecked = chamferNormal;
            }
            else {
                const v_2 = chamferNormal;
                chamferNormalChecked = UnitVc_$ctor_7B00E9A0(-v_2.X, -v_2.Y);
            }
            let cos;
            const a_2 = nPrev;
            const b_2 = chamferNormal;
            cos = ((a_2.X * b_2.X) + (a_2.Y * b_2.Y));
            let item;
            const pt_1 = pt;
            const dist_1 = dist;
            const nPrev_1 = nPrev;
            const nNext_1 = chamferNormalChecked;
            const cosine_1 = cos;
            if (cosine_1 < -0.9999999) {
                fail(`Offset2D.setOffCorner: cosine is ${cosine_1} , a 180 degree U-turn, which is not allowed here for dist ${dist_1}, nPrev ${nPrev_1}, nNext ${nNext_1} at pt ${pt_1}.`);
            }
            const p = pt_1;
            let v_4;
            let v_3;
            const a_3 = nPrev_1;
            const b_3 = nNext_1;
            v_3 = Vc_$ctor_7B00E9A0(a_3.X + b_3.X, a_3.Y + b_3.Y);
            const f = dist_1 / (1 + cosine_1);
            v_4 = Vc_$ctor_7B00E9A0(v_3.X * f, v_3.Y * f);
            item = Pt_$ctor_7B00E9A0(p.X + v_4.X, p.Y + v_4.Y);
            void (res.push(item));
            let item_1;
            const pt_2 = pt;
            const dist_2 = dist;
            const nPrev_2 = chamferNormalChecked;
            const nNext_2 = nNext;
            const cosine_2 = cos;
            if (cosine_2 < -0.9999999) {
                fail(`Offset2D.setOffCorner: cosine is ${cosine_2} , a 180 degree U-turn, which is not allowed here for dist ${dist_2}, nPrev ${nPrev_2}, nNext ${nNext_2} at pt ${pt_2}.`);
            }
            const p_1 = pt_2;
            let v_6;
            let v_5;
            const a_4 = nPrev_2;
            const b_4 = nNext_2;
            v_5 = Vc_$ctor_7B00E9A0(a_4.X + b_4.X, a_4.Y + b_4.Y);
            const f_1 = dist_2 / (1 + cosine_2);
            v_6 = Vc_$ctor_7B00E9A0(v_5.X * f_1, v_5.Y * f_1);
            item_1 = Pt_$ctor_7B00E9A0(p_1.X + v_6.X, p_1.Y + v_6.Y);
            void (res.push(item_1));
            break;
        }
        case 3: {
            let midV;
            let a_5;
            const v_7 = nPrev;
            a_5 = UnitVc_$ctor_7B00E9A0(v_7.Y, -v_7.X);
            let b_5;
            const v_8 = nNext;
            b_5 = UnitVc_$ctor_7B00E9A0(-v_8.Y, v_8.X);
            midV = Vc_$ctor_7B00E9A0(a_5.X + b_5.X, a_5.Y + b_5.Y);
            const cosHalf = Math.sqrt((1 + useUTurnBehaviorAbove) / 2);
            let item_2;
            const a_6 = pt;
            let b_6;
            const v_9 = midV;
            const f_2 = (0.5 * dist) / cosHalf;
            b_6 = Vc_$ctor_7B00E9A0(v_9.X * f_2, v_9.Y * f_2);
            item_2 = Pt_$ctor_7B00E9A0(a_6.X - b_6.X, a_6.Y - b_6.Y);
            void (res.push(item_2));
            break;
        }
        case 4: {
            break;
        }
        default: {
            const x_7 = uTurnBehavior;
            fail(`Offset2D.UTurnBehavior: enum value ${x_7} not recognized.`);
        }
    }
}

export function handleUTurnVarDist(pt, cosine, nPrev, nNext, dPrev, dNext, res, uTurnBehavior, useUTurnBehaviorAbove) {
    let a_1, b_1;
    switch (uTurnBehavior) {
        case 1: {
            fail(`Offset2D.offsetVariableWithDirections: pt ${pt} makes a ${57.29577951308232 * Math.acos(cosine)} degree U-turn, max ${57.29577951308232 * Math.acos(useUTurnBehaviorAbove)} is allowed.`);
            break;
        }
        case 2: {
            let chamferTangent;
            let v;
            const a = nPrev;
            const b = nNext;
            v = Vc_$ctor_7B00E9A0(a.X - b.X, a.Y - b.Y);
            const x = v.X;
            const y = v.Y;
            const l = Math.sqrt((x * x) + (y * y));
            if (!(l > 1E-12)) {
                failTooSmall("Vc.Unitized", v);
            }
            chamferTangent = UnitVc_$ctor_7B00E9A0(x / l, y / l);
            let chamferNormal;
            const v_1 = chamferTangent;
            chamferNormal = UnitVc_$ctor_7B00E9A0(-v_1.Y, v_1.X);
            let nMid;
            if (((a_1 = chamferNormal, (b_1 = nPrev, (a_1.X * b_1.X) + (a_1.Y * b_1.Y)))) >= 0) {
                nMid = chamferNormal;
            }
            else {
                const v_2 = chamferNormal;
                nMid = UnitVc_$ctor_7B00E9A0(-v_2.X, -v_2.Y);
            }
            const dMid = (dPrev + dNext) * 0.5;
            let midOff;
            const p = pt;
            let v_3;
            const a_2 = nMid;
            const f = dMid;
            v_3 = Vc_$ctor_7B00E9A0(a_2.X * f, a_2.Y * f);
            midOff = Pt_$ctor_7B00E9A0(p.X + v_3.X, p.Y + v_3.Y);
            let item;
            let fromA;
            const p_1 = pt;
            let v_4;
            const a_3 = nPrev;
            const f_1 = dPrev;
            v_4 = Vc_$ctor_7B00E9A0(a_3.X * f_1, a_3.Y * f_1);
            fromA = Pt_$ctor_7B00E9A0(p_1.X + v_4.X, p_1.Y + v_4.Y);
            const nA = nPrev;
            const fromB = midOff;
            const nB = nMid;
            const ax = nA.Y;
            const ay = -nA.X;
            const bx = nB.Y;
            const by = -nB.X;
            let t;
            const vBx = bx;
            const vBy = by;
            const det = (ax * vBy) - (ay * vBx);
            const dx = fromB.X - fromA.X;
            const dy = fromB.Y - fromA.Y;
            t = (((dx * vBy) - (dy * vBx)) / det);
            item = Pt_$ctor_7B00E9A0(fromA.X + (ax * t), fromA.Y + (ay * t));
            void (res.push(item));
            let item_1;
            const fromA_1 = midOff;
            const nA_1 = nMid;
            let fromB_1;
            const p_2 = pt;
            let v_5;
            const a_4 = nNext;
            const f_2 = dNext;
            v_5 = Vc_$ctor_7B00E9A0(a_4.X * f_2, a_4.Y * f_2);
            fromB_1 = Pt_$ctor_7B00E9A0(p_2.X + v_5.X, p_2.Y + v_5.Y);
            const nB_1 = nNext;
            const ax_1 = nA_1.Y;
            const ay_1 = -nA_1.X;
            const bx_1 = nB_1.Y;
            const by_1 = -nB_1.X;
            let t_1;
            const vBx_1 = bx_1;
            const vBy_1 = by_1;
            const det_1 = (ax_1 * vBy_1) - (ay_1 * vBx_1);
            const dx_1 = fromB_1.X - fromA_1.X;
            const dy_1 = fromB_1.Y - fromA_1.Y;
            t_1 = (((dx_1 * vBy_1) - (dy_1 * vBx_1)) / det_1);
            item_1 = Pt_$ctor_7B00E9A0(fromA_1.X + (ax_1 * t_1), fromA_1.Y + (ay_1 * t_1));
            void (res.push(item_1));
            break;
        }
        case 3: {
            const cosHalf = Math.sqrt((1 + useUTurnBehaviorAbove) / 2);
            let rot;
            const cos = cosHalf;
            if ((cos < -1) ? true : (cos > 1)) {
                fail(`Rotation2D.createFromCosine: input cosine ${cos} is out of range [-1.0, +1.0].`);
            }
            const sin = Math.sqrt(1 - (cos * cos));
            rot = Rotation2D_$ctor_7B00E9A0(sin, cos);
            let nMid_1;
            let v_9;
            let a_5;
            const v_6 = nPrev;
            a_5 = UnitVc_$ctor_7B00E9A0(v_6.Y, -v_6.X);
            let b_2;
            const v_7 = nNext;
            b_2 = UnitVc_$ctor_7B00E9A0(-v_7.Y, v_7.X);
            v_9 = Vc_$ctor_7B00E9A0(a_5.X + b_2.X, a_5.Y + b_2.Y);
            const x_7 = v_9.X;
            const y_6 = v_9.Y;
            const l_1 = Math.sqrt((x_7 * x_7) + (y_6 * y_6));
            if (!(l_1 > 1E-12)) {
                failUnit2("Vc.unitize", x_7, y_6);
            }
            nMid_1 = UnitVc_$ctor_7B00E9A0(x_7 / l_1, y_6 / l_1);
            let nPrevThresh;
            const v_10 = nMid_1;
            let r_1;
            const r = rot;
            r_1 = Rotation2D_$ctor_7B00E9A0(-r.Sin, r.Cos);
            nPrevThresh = UnitVc_$ctor_7B00E9A0((r_1.Cos * v_10.X) - (r_1.Sin * v_10.Y), (r_1.Sin * v_10.X) + (r_1.Cos * v_10.Y));
            let nNextThresh;
            const v_11 = nMid_1;
            const r_2 = rot;
            nNextThresh = UnitVc_$ctor_7B00E9A0((r_2.Cos * v_11.X) - (r_2.Sin * v_11.Y), (r_2.Sin * v_11.X) + (r_2.Cos * v_11.Y));
            let item_2;
            let fromA_2;
            const p_3 = pt;
            let v_12;
            const a_6 = nPrev;
            const f_3 = dPrev;
            v_12 = Vc_$ctor_7B00E9A0(a_6.X * f_3, a_6.Y * f_3);
            fromA_2 = Pt_$ctor_7B00E9A0(p_3.X + v_12.X, p_3.Y + v_12.Y);
            const nA_2 = nPrevThresh;
            let fromB_2;
            const p_4 = pt;
            let v_13;
            const a_7 = nNext;
            const f_4 = dNext;
            v_13 = Vc_$ctor_7B00E9A0(a_7.X * f_4, a_7.Y * f_4);
            fromB_2 = Pt_$ctor_7B00E9A0(p_4.X + v_13.X, p_4.Y + v_13.Y);
            const nB_2 = nNextThresh;
            const ax_2 = nA_2.Y;
            const ay_2 = -nA_2.X;
            const bx_2 = nB_2.Y;
            const by_2 = -nB_2.X;
            let t_2;
            const vBx_2 = bx_2;
            const vBy_2 = by_2;
            const det_2 = (ax_2 * vBy_2) - (ay_2 * vBx_2);
            const dx_2 = fromB_2.X - fromA_2.X;
            const dy_2 = fromB_2.Y - fromA_2.Y;
            t_2 = (((dx_2 * vBy_2) - (dy_2 * vBx_2)) / det_2);
            item_2 = Pt_$ctor_7B00E9A0(fromA_2.X + (ax_2 * t_2), fromA_2.Y + (ay_2 * t_2));
            void (res.push(item_2));
            break;
        }
        case 4: {
            break;
        }
        default: {
            const x_12 = uTurnBehavior;
            fail(`Offset2D.UTurnBehavior: enum value ${x_12} not recognized.`);
        }
    }
}

/**
 * For closed or open polylines.
 */
export function offsetWithDirections(pts, dirs, dist, uTurnBehavior, useUTurnBehaviorAbove) {
    let a_1, this$, b_1, this$_1, vx, vy;
    if (pts.length < 2) {
        fail(`Offset2D.offsetWithDirections: pts.Count must be at least 2 but is ${pts.length}.`);
    }
    if (pts.length !== (dirs.length + 1)) {
        fail(`Offset2D.offsetWithDirections: pts.Count must be one greater than normals.Count but they are ${pts.length} and ${dirs.length}.`);
    }
    const res = [];
    const isOpen = ((a_1 = ((this$ = pts, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item_4(0, this$)))), (b_1 = ((this$_1 = pts, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item_4(this$_1.length - 1, this$_1)))), (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), (vx * vx) + (vy * vy)))))) > 1E-12;
    let fromIdx = 0;
    let nPrev = item_4(dirs.length - 1, dirs);
    if (isOpen) {
        fromIdx = 1;
        nPrev = item_4(0, dirs);
        let item;
        const p = item_4(0, pts);
        let v;
        const a_2 = nPrev;
        const f = dist;
        v = Vc_$ctor_7B00E9A0(a_2.X * f, a_2.Y * f);
        item = Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y);
        void (res.push(item));
    }
    for (let i = fromIdx; i <= (pts.length - 2); i++) {
        const nNext = item_4(i, dirs);
        const pt = item_4(i, pts);
        let cosine;
        const a_3 = nPrev;
        const b_2 = nNext;
        cosine = ((a_3.X * b_2.X) + (a_3.Y * b_2.Y));
        if (cosine > useUTurnBehaviorAbove) {
            let item_1;
            const pt_1 = pt;
            const dist_1 = dist;
            const nPrev_1 = nPrev;
            const nNext_1 = nNext;
            const cosine_1 = cosine;
            if (cosine_1 < -0.9999999) {
                fail(`Offset2D.setOffCorner: cosine is ${cosine_1} , a 180 degree U-turn, which is not allowed here for dist ${dist_1}, nPrev ${nPrev_1}, nNext ${nNext_1} at pt ${pt_1}.`);
            }
            const p_1 = pt_1;
            let v_2;
            let v_1;
            const a_4 = nPrev_1;
            const b_3 = nNext_1;
            v_1 = Vc_$ctor_7B00E9A0(a_4.X + b_3.X, a_4.Y + b_3.Y);
            const f_1 = dist_1 / (1 + cosine_1);
            v_2 = Vc_$ctor_7B00E9A0(v_1.X * f_1, v_1.Y * f_1);
            item_1 = Pt_$ctor_7B00E9A0(p_1.X + v_2.X, p_1.Y + v_2.Y);
            void (res.push(item_1));
        }
        else {
            handleUTurn(pt, cosine, nPrev, nNext, dist, res, uTurnBehavior, useUTurnBehaviorAbove);
        }
        nPrev = nNext;
    }
    if (isOpen) {
        let item_2;
        let p_2;
        const this$_2 = pts;
        if (this$_2.length === 0) {
            failRarr("Last.get", this$_2);
        }
        p_2 = item_4(this$_2.length - 1, this$_2);
        let v_3;
        let a_5;
        const this$_3 = dirs;
        if (this$_3.length === 0) {
            failRarr("Last.get", this$_3);
        }
        a_5 = item_4(this$_3.length - 1, this$_3);
        const f_2 = dist;
        v_3 = Vc_$ctor_7B00E9A0(a_5.X * f_2, a_5.Y * f_2);
        item_2 = Pt_$ctor_7B00E9A0(p_2.X + v_3.X, p_2.Y + v_3.Y);
        void (res.push(item_2));
    }
    else {
        void (res.push(item_4(0, res)));
    }
    return res;
}

/**
 * Split list into chunks. Starts a new chunk if comparing the current item with the previous one returns true
 */
export function chunkBy(split, res) {
    const chunks = [];
    for (let i = 0; i <= (res.length - 1); i++) {
        let this$;
        const item = item_4(i, res);
        if ((i === 0) ? true : split(item, item_4(i - 1, res))) {
            void (chunks.push([]));
        }
        void (((this$ = chunks, ((this$.length === 0) ? failRarr("Last.get", this$) : undefined, item_4(this$.length - 1, this$)))).push(item));
    }
    return chunks;
}

/**
 * If indices make a loop, merge the last chunk with the first chunk.
 */
export function reLoop(lastIdx, getIdx, idxs) {
    let this$_2, this$_3, this$_5, i, v_1;
    if (idxs.length > 1) {
        let firsts;
        const this$ = idxs;
        if (this$.length === 0) {
            failRarr("First.get", this$);
        }
        firsts = item_4(0, this$);
        let lasts;
        const this$_1 = idxs;
        if (this$_1.length === 0) {
            failRarr("Last.get", this$_1);
        }
        lasts = item_4(this$_1.length - 1, this$_1);
        if ((getIdx((this$_2 = firsts, ((this$_2.length === 0) ? failRarr("First.get", this$_2) : undefined, item_4(0, this$_2)))) === 0) && (getIdx((this$_3 = lasts, ((this$_3.length === 0) ? failRarr("Last.get", this$_3) : undefined, item_4(this$_3.length - 1, this$_3)))) === lastIdx)) {
            addRangeInPlace(firsts, lasts);
            const this$_4 = idxs;
            if (this$_4.length === 0) {
                failRarr("First.set", this$_4);
            }
            setItem(this$_4, 0, lasts);
            (this$_5 = idxs, ((this$_5.length === 0) ? failRarr("Pop", this$_5) : undefined, (i = ((this$_5.length - 1) | 0), (v_1 = item_4(i, this$_5), (this$_5.splice(i, 1), v_1)))));
        }
    }
}

/**
 * Only used when VarDistParallelBehavior.Proportional is selected.
 */
export function distributeProportionallyBadIdxs(res, colinearIdxs, origs) {
    const chunks = chunkBy((thisIdx, prevIdx) => (thisIdx.idxRes !== (prevIdx.idxRes + 1)), colinearIdxs);
    reLoop(res.length - 1, (i) => (i.idxRes | 0), chunks);
    for (let i_1 = 0; i_1 <= (chunks.length - 1); i_1++) {
        let this$_2, this$_3, this$_4, this$_5;
        const chunk = item_4(i_1, chunks);
        let offLn;
        let prevOkIdx;
        const i_2 = (((this$_2 = chunk, ((this$_2.length === 0) ? failRarr("First.get", this$_2) : undefined, item_4(0, this$_2)))).idxRes - 1) | 0;
        const length = res.length | 0;
        const t = (i_2 % length) | 0;
        prevOkIdx = ((t >= 0) ? t : (t + length));
        let nextOkIdx;
        const i_3 = (((this$_3 = chunk, ((this$_3.length === 0) ? failRarr("Last.get", this$_3) : undefined, item_4(this$_3.length - 1, this$_3)))).idxRes + 1) | 0;
        const length_1 = res.length | 0;
        const t_1 = (i_3 % length_1) | 0;
        nextOkIdx = ((t_1 >= 0) ? t_1 : (t_1 + length_1));
        offLn = Line2D_$ctor_Z53905FA0(item_4(prevOkIdx, res), item_4(nextOkIdx, res));
        let origLn;
        let origPrevOkIdx;
        const i_4 = (((this$_4 = chunk, ((this$_4.length === 0) ? failRarr("First.get", this$_4) : undefined, item_4(0, this$_4)))).idxOrig - 1) | 0;
        const length_2 = origs.length | 0;
        const t_2 = (i_4 % length_2) | 0;
        origPrevOkIdx = ((t_2 >= 0) ? t_2 : (t_2 + length_2));
        let origNextOkIdx;
        const i_5 = (((this$_5 = chunk, ((this$_5.length === 0) ? failRarr("Last.get", this$_5) : undefined, item_4(this$_5.length - 1, this$_5)))).idxOrig + 1) | 0;
        const length_3 = origs.length | 0;
        const t_3 = (i_5 % length_3) | 0;
        origNextOkIdx = ((t_3 >= 0) ? t_3 : (t_3 + length_3));
        origLn = Line2D_$ctor_Z53905FA0(item_4(origPrevOkIdx, origs), item_4(origNextOkIdx, origs));
        for (let j = 0; j <= (chunk.length - 1); j++) {
            let ln, p, ln_1, ln_2;
            const bi = item_4(j, chunk);
            const origPt = item_4(bi.idxOrig, origs);
            const t_4 = Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD(origLn, origPt);
            setItem(res, bi.idxRes, (ln = offLn, (p = t_4, Pt_$ctor_7B00E9A0_1(ln.FromX + (((ln_1 = ln, ln_1.ToX - ln_1.FromX)) * p), ln.FromY + (((ln_2 = ln, ln_2.ToY - ln_2.FromY)) * p)))));
        }
    }
}

/**
 * Only used when VarDistParallelBehavior.Project is selected.
 */
export function projectBadIdxs(res, colinearIdxs) {
    const chunks = chunkBy((thisIdx, prevIdx) => (thisIdx.idx !== (prevIdx.idx + 1)), colinearIdxs);
    reLoop(res.length - 1, (_arg) => (_arg.idx | 0), chunks);
    for (let i = 0; i <= (chunks.length - 1); i++) {
        let this$_2, this$_4;
        const chunk = item_4(i, chunks);
        let prev;
        const this$_3 = res;
        const t = ((((this$_2 = chunk, ((this$_2.length === 0) ? failRarr("First.get", this$_2) : undefined, item_4(0, this$_2)))).idx - 1) % this$_3.length) | 0;
        prev = ((t >= 0) ? item_4(t, this$_3) : item_4(t + this$_3.length, this$_3));
        let next;
        const this$_5 = res;
        const t_1 = ((((this$_4 = chunk, ((this$_4.length === 0) ? failRarr("Last.get", this$_4) : undefined, item_4(this$_4.length - 1, this$_4)))).idx + 1) % this$_5.length) | 0;
        next = ((t_1 >= 0) ? item_4(t_1, this$_5) : item_4(t_1 + this$_5.length, this$_5));
        const ln = Line2D_$ctor_Z53905FA0(prev, next);
        for (let j = 0; j <= (chunk.length - 1); j++) {
            let p, v, lineA, lineB, pAx, pAy, vAx, ln_1, vAy, ln_2, vBx, ln_3, vBy, ln_4, det, dot, tan, dx, dy, t_2;
            const itp = item_4(j, chunk);
            const pt = item_4(itp.idx, res);
            const pln = Line2D_$ctor_Z53905FA0(pt, (p = pt, (v = itp.dir, Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y))));
            setItem(res, itp.idx, value_1((lineA = ln, (lineB = pln, (pAx = lineA.FromX, (pAy = lineA.FromY, (vAx = ((ln_1 = lineA, ln_1.ToX - ln_1.FromX)), (vAy = ((ln_2 = lineA, ln_2.ToY - ln_2.FromY)), (vBx = ((ln_3 = lineB, ln_3.ToX - ln_3.FromX)), (vBy = ((ln_4 = lineB, ln_4.ToY - ln_4.FromY)), (det = ((vAx * vBy) - (vAy * vBx)), (dot = ((vAx * vBx) + (vAy * vBy)), (tan = (det / dot), ((Math.abs(tan) > 0.004363350820701567) && (Math.abs(det) > 1E-10)) ? ((dx = (lineB.FromX - pAx), (dy = (lineB.FromY - pAy), (t_2 = (((dx * vBy) - (dy * vBx)) / det), Pt_$ctor_7B00E9A0(pAx + (t_2 * vAx), pAy + (t_2 * vAy)))))) : undefined)))))))))))));
        }
    }
}

export function handleVarOffsetColinear(i, pt, nPrev, nNext, dPrev, dNext, res, idxsToFixProportional, projectIdxs, varDistParallelBehavior) {
    let a, b;
    switch (varDistParallelBehavior) {
        case 1: {
            fail(`Offset2D.offsetVariableWithDirections: pts.[${i}] and pts.[${i - 1}] are colinear but have different distances ${dPrev} and ${dNext}.`);
            break;
        }
        case 2: {
            break;
        }
        case 3: {
            void (idxsToFixProportional.push(new IndexToFixProportional(res.length, i)));
            void (res.push(pt));
            break;
        }
        case 4: {
            void (projectIdxs.push(new IndexToProject(res.length, (a = nPrev, (b = nNext, Vc_$ctor_7B00E9A0(a.X + b.X, a.Y + b.Y))))));
            void (res.push(pt));
            break;
        }
        case 5: {
            let item;
            const p = pt;
            let v;
            const a_1 = nPrev;
            const f = dPrev;
            v = Vc_$ctor_7B00E9A0(a_1.X * f, a_1.Y * f);
            item = Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y);
            void (res.push(item));
            let item_1;
            const p_1 = pt;
            let v_1;
            const a_2 = nNext;
            const f_1 = dNext;
            v_1 = Vc_$ctor_7B00E9A0(a_2.X * f_1, a_2.Y * f_1);
            item_1 = Pt_$ctor_7B00E9A0(p_1.X + v_1.X, p_1.Y + v_1.Y);
            void (res.push(item_1));
            break;
        }
        default: {
            const x = varDistParallelBehavior;
            fail(`Offset2D.VarDistParallelBehavior: enum value ${x} not recognized.`);
        }
    }
}

/**
 * Offsetting each segment by its own distance. For closed or open polylines. Adds 2 chamfer points at U-turns and skips colinear points.
 */
export function offsetVariableWithDirections(pts, nDirs, dists, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove) {
    let a_1, this$, b_1, this$_1, vx, vy, this$_4, this$_7, this$_9;
    if (pts.length < 2) {
        fail(`Offset2D.offsetVariableWithDirections:
  point count must be at least 2, but is ${pts.length}.`);
    }
    if (pts.length !== (nDirs.length + 1)) {
        fail(`Offset2D.offsetVariableWithDirections:
  point count must be 1 greater than normal directions count, but they are ${pts.length} and ${nDirs.length}.`);
    }
    if (pts.length !== (count(dists) + 1)) {
        fail(`Offset2D.offsetVariableWithDirections:
   point count must be 1 greater than offset distances count, but they are ${pts.length} and ${count(dists)}.`);
    }
    const res = [];
    const idxsToFixProportional = [];
    const projectIdxs = [];
    let dPrev = item_4(count(dists) - 1, dists);
    let nPrev = item_4(nDirs.length - 1, nDirs);
    const isOpen = ((a_1 = ((this$ = pts, ((this$.length === 0) ? failRarr("First.get", this$) : undefined, item_4(0, this$)))), (b_1 = ((this$_1 = pts, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item_4(this$_1.length - 1, this$_1)))), (vx = (a_1.X - b_1.X), (vy = (a_1.Y - b_1.Y), (vx * vx) + (vy * vy)))))) > 1E-12;
    let fromIdx = 0;
    if (isOpen) {
        fromIdx = 1;
        nPrev = item_4(0, nDirs);
        dPrev = item_4(0, dists);
        let item;
        const p = item_4(0, pts);
        let v;
        const a_2 = nPrev;
        const f = dPrev;
        v = Vc_$ctor_7B00E9A0(a_2.X * f, a_2.Y * f);
        item = Pt_$ctor_7B00E9A0(p.X + v.X, p.Y + v.Y);
        void (res.push(item));
    }
    for (let i = fromIdx; i <= (pts.length - 2); i++) {
        const nNext = item_4(i, nDirs);
        const dNext = item_4(i, dists);
        const pt = item_4(i, pts);
        let cosine;
        const a_3 = nPrev;
        const b_2 = nNext;
        cosine = ((a_3.X * b_2.X) + (a_3.Y * b_2.Y));
        if (Math.abs(dPrev - dNext) < 1E-06) {
            if (cosine > useUTurnBehaviorAbove) {
                let item_1;
                const pt_1 = pt;
                const dist = dPrev;
                const nPrev_1 = nPrev;
                const nNext_1 = nNext;
                const cosine_1 = cosine;
                if (cosine_1 < -0.9999999) {
                    fail(`Offset2D.setOffCorner: cosine is ${cosine_1} , a 180 degree U-turn, which is not allowed here for dist ${dist}, nPrev ${nPrev_1}, nNext ${nNext_1} at pt ${pt_1}.`);
                }
                const p_1 = pt_1;
                let v_2;
                let v_1;
                const a_4 = nPrev_1;
                const b_3 = nNext_1;
                v_1 = Vc_$ctor_7B00E9A0(a_4.X + b_3.X, a_4.Y + b_3.Y);
                const f_1 = dist / (1 + cosine_1);
                v_2 = Vc_$ctor_7B00E9A0(v_1.X * f_1, v_1.Y * f_1);
                item_1 = Pt_$ctor_7B00E9A0(p_1.X + v_2.X, p_1.Y + v_2.Y);
                void (res.push(item_1));
            }
            else {
                handleUTurn(pt, cosine, nPrev, nNext, dPrev, res, uTurnBehavior, useUTurnBehaviorAbove);
            }
        }
        else if ((cosine > useUTurnBehaviorAbove) ? true : ((i === 0) && isOpen)) {
            if (cosine < useVarDistParallelBehaviorBelow) {
                let item_2;
                const distPrev = dPrev;
                const distNext = dNext;
                const nPrev_2 = nPrev;
                const nNext_2 = nNext;
                const delta = distPrev - distNext;
                let vNext;
                const v_3 = nNext_2;
                vNext = UnitVc_$ctor_7B00E9A0(v_3.Y, -v_3.X);
                let cos2;
                const a_5 = nPrev_2;
                const b_4 = vNext;
                cos2 = ((a_5.X * b_4.X) + (a_5.Y * b_4.Y));
                let p_3;
                const p_2 = pt;
                let v_4;
                const a_6 = vNext;
                const f_2 = delta / cos2;
                v_4 = Vc_$ctor_7B00E9A0(a_6.X * f_2, a_6.Y * f_2);
                p_3 = Pt_$ctor_7B00E9A0(p_2.X + v_4.X, p_2.Y + v_4.Y);
                let v_6;
                let v_5;
                const a_7 = nPrev_2;
                const b_5 = nNext_2;
                v_5 = Vc_$ctor_7B00E9A0(a_7.X + b_5.X, a_7.Y + b_5.Y);
                const f_3 = distNext / (1 + cosine);
                v_6 = Vc_$ctor_7B00E9A0(v_5.X * f_3, v_5.Y * f_3);
                item_2 = Pt_$ctor_7B00E9A0(p_3.X + v_6.X, p_3.Y + v_6.Y);
                void (res.push(item_2));
            }
            else {
                handleVarOffsetColinear(i, pt, nPrev, nNext, dPrev, dNext, res, idxsToFixProportional, projectIdxs, varDistParallelBehavior);
            }
        }
        else {
            handleUTurnVarDist(pt, cosine, nPrev, nNext, dPrev, dNext, res, uTurnBehavior, useUTurnBehaviorAbove);
        }
        nPrev = nNext;
        dPrev = dNext;
    }
    if (isOpen) {
        let item_3;
        let p_4;
        const this$_2 = pts;
        if (this$_2.length === 0) {
            failRarr("Last.get", this$_2);
        }
        p_4 = item_4(this$_2.length - 1, this$_2);
        let v_7;
        let a_8;
        const this$_3 = nDirs;
        if (this$_3.length === 0) {
            failRarr("Last.get", this$_3);
        }
        a_8 = item_4(this$_3.length - 1, this$_3);
        const f_4 = item_4(count(dists) - 1, dists);
        v_7 = Vc_$ctor_7B00E9A0(a_8.X * f_4, a_8.Y * f_4);
        item_3 = Pt_$ctor_7B00E9A0(p_4.X + v_7.X, p_4.Y + v_7.Y);
        void (res.push(item_3));
    }
    else {
        void (res.push(item_4(0, res)));
    }
    if (idxsToFixProportional.length > 0) {
        if (((this$_4 = idxsToFixProportional, ((this$_4.length === 0) ? failRarr("First.get", this$_4) : undefined, item_4(0, this$_4)))).idxRes === 0) {
            void (idxsToFixProportional.push(new IndexToFixProportional(res.length - 1, pts.length - 1)));
        }
        distributeProportionallyBadIdxs(res, idxsToFixProportional, pts);
    }
    else if (projectIdxs.length > 0) {
        if (((this$_7 = projectIdxs, ((this$_7.length === 0) ? failRarr("First.get", this$_7) : undefined, item_4(0, this$_7)))).idx === 0) {
            void (projectIdxs.push(new IndexToProject(res.length - 1, ((this$_9 = projectIdxs, ((this$_9.length === 0) ? failRarr("First.get", this$_9) : undefined, item_4(0, this$_9)))).dir)));
        }
        projectBadIdxs(res, projectIdxs);
    }
    return res;
}

/**
 * A constant-distance offset algorithm for closed or open polylines.
 */
export function offset$0027$0027(useUTurnBehaviorAbove, uTurnBehavior, dist, pts) {
    if (pts.length < 2) {
        fail(`Offset2D.offset'': pts.Count must be at least 2 but is ${pts.length}.`);
    }
    if (Math.abs(dist) < 1E-12) {
        return getRange(pts, 0, pts.length - 1);
    }
    else {
        return offsetWithDirections(pts, makeOffsetDirections(pts), dist, uTurnBehavior, useUTurnBehaviorAbove);
    }
}

/**
 * A constant-distance offset algorithm for closed or open polylines.
 */
export function offset$0027(uTurnBehavior, dist, pts) {
    if (pts.length < 2) {
        fail(`Offset2D.offset': pts.Count must be at least 2 but is ${pts.length}.`);
    }
    if (Math.abs(dist) < 1E-12) {
        return getRange(pts, 0, pts.length - 1);
    }
    else {
        return offsetWithDirections(pts, makeOffsetDirections(pts), dist, uTurnBehavior, -0.9961946980917455);
    }
}

/**
 * A constant-distance offset algorithm for closed or open polylines.
 * Fails at corners or U-turns sharper than joints after 177.5° degrees.
 */
export function offset(dist, pts) {
    if (pts.length < 2) {
        fail(`Offset2D.offset: pts.Count must be at least 2 but is ${pts.length}.`);
    }
    if (Math.abs(dist) < 1E-12) {
        return getRange(pts, 0, pts.length - 1);
    }
    else {
        return offsetWithDirections(pts, makeOffsetDirections(pts), dist, 1, -0.9990482215818578);
    }
}

/**
 * Offsetting each segment by its own distance. For closed or open polylines.
 * The behaviour and the limits for colinear and 180 degree U-turns are configurable.
 */
export function offsetVariable$0027$0027(useUTurnBehaviorAbove, useVarDistParallelBehaviorBelow, uTurnBehavior, varDistParallelBehavior, dists, pts) {
    const nDirs = makeOffsetDirections(pts);
    return offsetVariableWithDirections(pts, nDirs, dists, varDistParallelBehavior, uTurnBehavior, useVarDistParallelBehaviorBelow, useUTurnBehaviorAbove);
}

/**
 * Offsetting each segment by its own distance.
 * The behaviour for colinear and 180 degree U-turns is configurable.
 */
export function offsetVariable$0027(uTurnBehavior, varDistParallelBehavior, dists, pts) {
    const nDirs = makeOffsetDirections(pts);
    return offsetVariableWithDirections(pts, nDirs, dists, varDistParallelBehavior, uTurnBehavior, 0.9990482215818578, -0.9961946980917455);
}

/**
 * Offsetting each segment by its own distance. For closed or open polylines.
 * Fails at U-turns above 175 degrees and at colinear segments within less than 2.5 degrees.
 */
export function offsetVariable(dists, pts) {
    const nDirs = makeOffsetDirections(pts);
    return offsetVariableWithDirections(pts, nDirs, dists, 1, 1, 0.9990482215818578, -0.9961946980917455);
}

