
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { record_type, array_type, string_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { BRect_$ctor_77D16AC0, BRect_$reflection } from "./BRect.js";
import { Pt_$ctor_7B00E9A0, Pt_$reflection } from "./Pt.js";
import { sortInPlaceBy, map, forAll2, setItem, fill, item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { ResizeArr_tryFindIndex, ResizeArr_sortBy } from "./ResizeArr.js";
import { max, min } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { fail2, failEmptySeq, failNull, fail } from "./EuclidErrors.js";
import { Vc_$ctor_7B00E9A0 } from "./Vc.js";
import { zip, empty, singleton, collect, delay, toArray } from "../fable_modules/fable-library-js.5.0.0/Seq.js";
import { disposeSafe, getEnumerator, comparePrimitives } from "../fable_modules/fable-library-js.5.0.0/Util.js";
import { Operators_IsNull } from "../fable_modules/fable-library-js.5.0.0/FSharp.Core.js";
import { count } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { getItemFromDict } from "../fable_modules/fable-library-js.5.0.0/MapUtil.js";

/**
 * A type used inside ObjectToCheck.
 * It represent a group of similar input.
 * The 'category' string is used to only compare groups of the same category.
 * The 'bounding Rectangle' of the points is used as a fast and first check for similarity.
 * Within one 'list of points' the order does not matter, but each location must exist only once
 * in order to be consider similar within the tolerance with another GroupInsideObjectToCheck.
 */
export class GroupInsideObjectToCheck extends Record {
    constructor(category, bRect, points) {
        super();
        this.category = category;
        this.bRect = bRect;
        this.points = points;
    }
}

export function GroupInsideObjectToCheck_$reflection() {
    return record_type("Euclid.Similarity2D.GroupInsideObjectToCheck", [], GroupInsideObjectToCheck, () => [["category", string_type], ["bRect", BRect_$reflection()], ["points", array_type(Pt_$reflection())]]);
}

/**
 * A type to represent on object that shall be compared to other objects.
 * 'extend' (just a 2D point)  represents the max value of a bounding Rectangle, min value must be x=0 and y=0.
 * This is used for a very fast initial similarity check.
 * 'groups' (an array of GroupInsideObjectToCheck) must be sorted by 'category' property.
 */
export class ObjectToCheck extends Record {
    constructor(extend, groups) {
        super();
        this.extend = extend;
        this.groups = groups;
    }
}

export function ObjectToCheck_$reflection() {
    return record_type("Euclid.Similarity2D.ObjectToCheck", [], ObjectToCheck, () => [["extend", Pt_$reflection()], ["groups", array_type(GroupInsideObjectToCheck_$reflection())]]);
}

function simPt(tol, ps, pt) {
    const x = pt.X;
    const y = pt.Y;
    const binSearchIdx = (lo_mut, hi_mut) => {
        binSearchIdx:
        while (true) {
            const lo = lo_mut, hi = hi_mut;
            if (lo <= hi) {
                const mid = (lo + ((hi - lo) >> 1)) | 0;
                const p = item(mid, ps);
                if (Math.abs(p.X - x) < tol) {
                    return mid | 0;
                }
                else if (p.X < x) {
                    lo_mut = (mid + 1);
                    hi_mut = hi;
                    continue binSearchIdx;
                }
                else {
                    lo_mut = lo;
                    hi_mut = (mid - 1);
                    continue binSearchIdx;
                }
            }
            else {
                return -1;
            }
            break;
        }
    };
    const matchValue = binSearchIdx(0, ps.length - 1) | 0;
    if (matchValue === -1) {
        return -1;
    }
    else {
        const ix = matchValue | 0;
        const searchIdxDown = (i_mut) => {
            searchIdxDown:
            while (true) {
                const i = i_mut;
                const p_1 = item(i, ps);
                if (Math.abs(p_1.X - x) < tol) {
                    if (Math.abs(p_1.Y - y) < tol) {
                        return i | 0;
                    }
                    else if (i === 0) {
                        return -1;
                    }
                    else {
                        i_mut = (i - 1);
                        continue searchIdxDown;
                    }
                }
                else {
                    return -1;
                }
                break;
            }
        };
        const matchValue_1 = searchIdxDown(ix) | 0;
        if (matchValue_1 === -1) {
            const last = (ps.length - 1) | 0;
            const searchIdxUp = (i_1_mut) => {
                searchIdxUp:
                while (true) {
                    const i_1 = i_1_mut;
                    const p_2 = item(i_1, ps);
                    if (Math.abs(p_2.X - x) < tol) {
                        if (Math.abs(p_2.Y - y) < tol) {
                            return i_1 | 0;
                        }
                        else if (i_1 === last) {
                            return -1;
                        }
                        else {
                            i_1_mut = (i_1 + 1);
                            continue searchIdxUp;
                        }
                    }
                    else {
                        return -1;
                    }
                    break;
                }
            };
            return searchIdxUp(ix) | 0;
        }
        else {
            return matchValue_1 | 0;
        }
    }
}

function simPts(tol, ps, cs) {
    const rs = fill(new Array(cs.length), 0, cs.length, false);
    if (ps.every((p) => {
        const matchValue = simPt(tol, cs, p) | 0;
        if (matchValue === -1) {
            return false;
        }
        else {
            setItem(rs, matchValue, true);
            return true;
        }
    })) {
        return rs.every((x) => x);
    }
    else {
        return false;
    }
}

/**
 * Takes transformed and pre sorted by category main groups.
 */
export function areSimilar(tol, a, b) {
    let a_1, b_1;
    if (((a_1 = a.extend, (b_1 = b.extend, (Math.abs(a_1.X - b_1.X) < tol) && (Math.abs(a_1.Y - b_1.Y) < tol)))) && (a.groups.length === b.groups.length)) {
        return forAll2((x, y) => {
            let a_2, b_2, a_3, r, b_3, r_1, a_4, r_2, b_4, r_3;
            if (((x.category === y.category) && (x.points.length === y.points.length)) && ((a_2 = x.bRect, (b_2 = y.bRect, ((a_3 = ((r = a_2, Pt_$ctor_7B00E9A0(r.MinX, r.MinY))), (b_3 = ((r_1 = b_2, Pt_$ctor_7B00E9A0(r_1.MinX, r_1.MinY))), (Math.abs(a_3.X - b_3.X) < tol) && (Math.abs(a_3.Y - b_3.Y) < tol)))) && ((a_4 = ((r_2 = a_2, Pt_$ctor_7B00E9A0(r_2.MaxX, r_2.MaxY))), (b_4 = ((r_3 = b_2, Pt_$ctor_7B00E9A0(r_3.MaxX, r_3.MaxY))), (Math.abs(a_4.X - b_4.X) < tol) && (Math.abs(a_4.Y - b_4.Y) < tol)))))))) {
                return simPts(tol, x.points, y.points);
            }
            else {
                return false;
            }
        }, a.groups, b.groups);
    }
    else {
        return false;
    }
}

/**
 * The returned ObjectToCheck will have the subgroups sorted by category
 * and each point will be transformed by the overall bounding Rectangle Min point to 0,0.
 * Input Position of points does not matter, they will be moved to origin by overall bounding Rectangle over all lists,
 * But any similarity that could be achieved by rotation will not be discovered.
 * The string is used as a unique category identifier.
 */
export function getSimilarityData(ptss) {
    let p_1, v;
    const sptss = ResizeArr_sortBy((tuple) => tuple[0], ptss);
    let minX = 1.7976931348623157E+308;
    let minY = 1.7976931348623157E+308;
    let maxX = -1.7976931348623157E+308;
    let maxY = -1.7976931348623157E+308;
    for (let i = 0; i <= (sptss.length - 1); i++) {
        const pts = item(i, sptss)[1];
        for (let j = 0; j <= (pts.length - 1); j++) {
            const p = item(j, pts);
            minX = min(minX, p.X);
            minY = min(minY, p.Y);
            maxX = max(maxX, p.X);
            maxY = max(maxY, p.Y);
        }
    }
    if ((((minX === 1.7976931348623157E+308) ? true : (minY === 1.7976931348623157E+308)) ? true : (maxX === -1.7976931348623157E+308)) ? true : (maxY === -1.7976931348623157E+308)) {
        fail("Similarity2D.getSimilarityData: point lists are empty");
    }
    const shift = Vc_$ctor_7B00E9A0(-minX, -minY);
    return new ObjectToCheck((p_1 = Pt_$ctor_7B00E9A0(maxX, maxY), (v = shift, Pt_$ctor_7B00E9A0(p_1.X + v.X, p_1.Y + v.Y))), toArray(delay(() => collect((matchValue) => {
        let ps_1, minX_1, minY_1, maxX_1, maxY_1;
        const pts_1 = matchValue[1];
        if (pts_1.length > 0) {
            const ps = map((a) => {
                const p_2 = a;
                const v_3 = shift;
                return Pt_$ctor_7B00E9A0(p_2.X + v_3.X, p_2.Y + v_3.Y);
            }, Array.from(pts_1));
            sortInPlaceBy((pt) => pt.X, ps, {
                Compare: (x, y) => (comparePrimitives(x, y) | 0),
            });
            return singleton(new GroupInsideObjectToCheck(matchValue[0], (ps_1 = ps, (Operators_IsNull(ps_1) ? failNull("BRect.createFromIList", "IList<Pt>") : undefined, ((count(ps_1) === 0) ? failEmptySeq("BRect.createFromIList", "IList<Pt>") : undefined, (minX_1 = 1.7976931348623157E+308, (minY_1 = 1.7976931348623157E+308, (maxX_1 = -1.7976931348623157E+308, (maxY_1 = -1.7976931348623157E+308, ((() => {
                for (let i_1 = 0; i_1 <= (count(ps_1) - 1); i_1++) {
                    const p_3 = item(i_1, ps_1);
                    minX_1 = min(minX_1, p_3.X);
                    minY_1 = min(minY_1, p_3.Y);
                    maxX_1 = max(maxX_1, p_3.X);
                    maxY_1 = max(maxY_1, p_3.Y);
                }
            })(), BRect_$ctor_77D16AC0(minX_1, minY_1, maxX_1, maxY_1))))))))), ps));
        }
        else {
            return empty();
        }
    }, sptss))));
}

/**
 * This will group similar generic items together based on their ObjectToCheck.
 * The ResizeArray of items and precomputed ResizeArray of ObjectToCheck must have the same count and correspond to each other at the same index.
 * Provide the ResizeArray of ObjectToCheck precomputed for better performance via getSimilarityData().
 */
export function getGrouped(tolerance, items, sims) {
    if (items.length !== sims.length) {
        fail2("Count mismatch in Similarity2D.getGrouped", items.length, sims.length);
    }
    const unique = [];
    const groups = new Map([]);
    const enumerator = getEnumerator(zip(sims, items));
    try {
        while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
            const forLoopVar = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
            const sid = forLoopVar[0];
            const it = forLoopVar[1];
            const matchValue = ResizeArr_tryFindIndex((b) => areSimilar(tolerance, sid, b), unique);
            if (matchValue == null) {
                const r = [];
                void (r.push(it));
                groups.set(unique.length, r);
                void (unique.push(sid));
            }
            else {
                const i = matchValue | 0;
                void (getItemFromDict(groups, i).push(it));
            }
        }
    }
    finally {
        disposeSafe(enumerator);
    }
    const r_1 = [];
    let enumerator_1 = getEnumerator(groups.values());
    try {
        while (enumerator_1["System.Collections.IEnumerator.MoveNext"]()) {
            const v = enumerator_1["System.Collections.Generic.IEnumerator`1.get_Current"]();
            void (r_1.push(v));
        }
    }
    finally {
        disposeSafe(enumerator_1);
    }
    return r_1;
}

