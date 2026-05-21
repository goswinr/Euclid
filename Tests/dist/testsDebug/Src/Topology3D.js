
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { addRangeInPlace, setItem, item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Pnt.js";
import { Topology2D_joinReversing_Z484E292B, Topology2D_join_Z484E292B, Topology2D_sortToLoopWithReversing_Z36DB21F5, Topology2D_sortToLoop_Z16A0C932, TopologyUtil_LoopCollector$1 } from "./Topology2D.js";
import { failRarr } from "./EuclidErrors.js";
import { ResizeArr_map } from "./ResizeArr.js";

/**
 * A type containing only static member functions for 3D topological operations.
 */
export class Topology3D {
    constructor() {
    }
}

export function Topology3D_$reflection() {
    return class_type("Euclid.Topology3D", undefined, Topology3D);
}

/**
 * Sorts elements in place to be in a circular structure.
 * This does not recognize if there are actually two loops, not just one.
 * Use Topology3D.join instead.
 * For each line end point it finds the next closest line start point.
 * (Does not check other line end points that might be closer)
 * Line3D is used as an abstraction to hold start and end of arbitrary object.
 */
export function Topology3D_sortToLoop_Z16A0DAF1(getLine, xs) {
    for (let i = 0; i <= (xs.length - 2); i++) {
        const thisLine = getLine(item(i, xs));
        let nextIdx;
        const compareBy_1 = (c) => {
            let a_1;
            let copyOfStruct = getLine(c);
            const ln = copyOfStruct;
            a_1 = Pnt_$ctor_Z7AD9E565(ln.FromX, ln.FromY, ln.FromZ);
            let b_1;
            const ln_1 = thisLine;
            b_1 = Pnt_$ctor_Z7AD9E565(ln_1.ToX, ln_1.ToY, ln_1.ToZ);
            const x = a_1.X - b_1.X;
            const y = a_1.Y - b_1.Y;
            const z = a_1.Z - b_1.Z;
            return ((x * x) + (y * y)) + (z * z);
        };
        const fromIdx_1 = (i + 1) | 0;
        const xs_2 = xs;
        let idx = fromIdx_1;
        let mi = compareBy_1(item(fromIdx_1, xs_2));
        for (let j = fromIdx_1 + 1; j <= (xs_2.length - 1); j++) {
            const this$_1 = compareBy_1(item(j, xs_2));
            if (this$_1 < mi) {
                idx = (j | 0);
                mi = this$_1;
            }
        }
        nextIdx = idx;
        const i_2 = (i + 1) | 0;
        const j_1 = nextIdx | 0;
        const xs_4 = xs;
        if (i_2 !== j_1) {
            const ti = item(i_2, xs_4);
            setItem(xs_4, i_2, item(j_1, xs_4));
            setItem(xs_4, j_1, ti);
        }
    }
}

/**
 * Sorts elements in place to be in a circular structure.
 * This does not recognize if there are actually two loops, not just one.
 * Use Topology3D.joinReversing instead.
 * For each line end it finds the next closest start point or end point.
 * Line3D is used as an abstraction to hold start and end of arbitrary object.
 * Reverses the input in place where required.
 * To reverse a 'T in place it uses the reverseInPlace function that takes the index of the element and the element itself as parameter.
 * e.g. the reverseInPlace function might just update an item at the given index in the array.
 * Depending on the structure of 'T the index might not be needed to reverse an element in place.
 */
export function Topology3D_sortToLoopWithReversing_Z36DD3996(getLine, reverseInPlace, xs) {
    for (let i = 0; i <= (xs.length - 2); i++) {
        let a_5, copyOfStruct_2, ln_4, b_5, ln_5, x_2, y_2, z_2, a_7, copyOfStruct_3, ln_6, b_7, ln_7, x_3, y_3, z_3;
        const thisLine = getLine(item(i, xs));
        let nextIdxSt;
        const compareBy_1 = (c) => {
            let a_1;
            let copyOfStruct = getLine(c);
            const ln = copyOfStruct;
            a_1 = Pnt_$ctor_Z7AD9E565(ln.FromX, ln.FromY, ln.FromZ);
            let b_1;
            const ln_1 = thisLine;
            b_1 = Pnt_$ctor_Z7AD9E565(ln_1.ToX, ln_1.ToY, ln_1.ToZ);
            const x = a_1.X - b_1.X;
            const y = a_1.Y - b_1.Y;
            const z = a_1.Z - b_1.Z;
            return ((x * x) + (y * y)) + (z * z);
        };
        const fromIdx_1 = (i + 1) | 0;
        const xs_2 = xs;
        let idx = fromIdx_1;
        let mi = compareBy_1(item(fromIdx_1, xs_2));
        for (let j = fromIdx_1 + 1; j <= (xs_2.length - 1); j++) {
            const this$_1 = compareBy_1(item(j, xs_2));
            if (this$_1 < mi) {
                idx = (j | 0);
                mi = this$_1;
            }
        }
        nextIdxSt = idx;
        let nextIdxEn;
        const compareBy_3 = (c_1) => {
            let a_3;
            let copyOfStruct_1 = getLine(c_1);
            const ln_2 = copyOfStruct_1;
            a_3 = Pnt_$ctor_Z7AD9E565(ln_2.ToX, ln_2.ToY, ln_2.ToZ);
            let b_3;
            const ln_3 = thisLine;
            b_3 = Pnt_$ctor_Z7AD9E565(ln_3.ToX, ln_3.ToY, ln_3.ToZ);
            const x_1 = a_3.X - b_3.X;
            const y_1 = a_3.Y - b_3.Y;
            const z_1 = a_3.Z - b_3.Z;
            return ((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1);
        };
        const fromIdx_3 = (i + 1) | 0;
        const xs_4 = xs;
        let idx_1 = fromIdx_3;
        let mi_1 = compareBy_3(item(fromIdx_3, xs_4));
        for (let j_1 = fromIdx_3 + 1; j_1 <= (xs_4.length - 1); j_1++) {
            const this$_3 = compareBy_3(item(j_1, xs_4));
            if (this$_3 < mi_1) {
                idx_1 = (j_1 | 0);
                mi_1 = this$_3;
            }
        }
        nextIdxEn = idx_1;
        if (((a_5 = ((copyOfStruct_2 = getLine(item(nextIdxSt, xs)), (ln_4 = copyOfStruct_2, Pnt_$ctor_Z7AD9E565(ln_4.FromX, ln_4.FromY, ln_4.FromZ)))), (b_5 = ((ln_5 = thisLine, Pnt_$ctor_Z7AD9E565(ln_5.ToX, ln_5.ToY, ln_5.ToZ))), (x_2 = (a_5.X - b_5.X), (y_2 = (a_5.Y - b_5.Y), (z_2 = (a_5.Z - b_5.Z), ((x_2 * x_2) + (y_2 * y_2)) + (z_2 * z_2))))))) <= ((a_7 = ((copyOfStruct_3 = getLine(item(nextIdxEn, xs)), (ln_6 = copyOfStruct_3, Pnt_$ctor_Z7AD9E565(ln_6.ToX, ln_6.ToY, ln_6.ToZ)))), (b_7 = ((ln_7 = thisLine, Pnt_$ctor_Z7AD9E565(ln_7.ToX, ln_7.ToY, ln_7.ToZ))), (x_3 = (a_7.X - b_7.X), (y_3 = (a_7.Y - b_7.Y), (z_3 = (a_7.Z - b_7.Z), ((x_3 * x_3) + (y_3 * y_3)) + (z_3 * z_3)))))))) {
            const i_2 = (i + 1) | 0;
            const j_2 = nextIdxSt | 0;
            const xs_6 = xs;
            if (i_2 !== j_2) {
                const ti = item(i_2, xs_6);
                setItem(xs_6, i_2, item(j_2, xs_6));
                setItem(xs_6, j_2, ti);
            }
        }
        else {
            reverseInPlace(nextIdxEn, item(nextIdxEn, xs));
            const i_4 = (i + 1) | 0;
            const j_3 = nextIdxEn | 0;
            const xs_8 = xs;
            if (i_4 !== j_3) {
                const ti_1 = item(i_4, xs_8);
                setItem(xs_8, i_4, item(j_3, xs_8));
                setItem(xs_8, j_3, ti_1);
            }
        }
    }
}

/**
 * Returns the groups of consecutive elements, loops or polylines.
 * They are split where the distance between the end point of one element and the start point of the next element is greater than 'splitDistance'.
 * For each element it will compute a line given the 'getLine' function.
 * The Line3D is used as an abstraction to hold start and end of arbitrary object.
 * Then for each line start and end point it finds the next closest line end or start point respectively.
 * Only start with end or end with start points are considered.
 * If the distance between two points is greater than the 'splitDistance'
 * it will be considered as a new group.
 */
export function Topology3D_join_Z4850C4CC(getLine, splitDistance, xs) {
    const loops = [];
    const distSq = splitDistance * splitDistance;
    let idx = 0;
    while (idx < xs.length) {
        let x;
        void (loops.push(new TopologyUtil_LoopCollector$1((x = item(idx, xs), [ x ]), [])));
        const ln = getLine(item(idx, xs));
        let en;
        const ln_1 = ln;
        en = Pnt_$ctor_Z7AD9E565(ln_1.ToX, ln_1.ToY, ln_1.ToZ);
        let i = idx + 1;
        while (i < xs.length) {
            let a_1, b_1, ln_2, x_1, y, z, this$, ln_3;
            const oth = getLine(item(i, xs));
            if (((a_1 = en, (b_1 = ((ln_2 = oth, Pnt_$ctor_Z7AD9E565(ln_2.FromX, ln_2.FromY, ln_2.FromZ))), (x_1 = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x_1 * x_1) + (y * y)) + (z * z))))))) < distSq) {
                void (((this$ = loops, ((this$.length === 0) ? failRarr("Last.get", this$) : undefined, item(this$.length - 1, this$)))).forward.push(item(i, xs)));
                const i_1 = i | 0;
                const j = (idx + 1) | 0;
                const xs_1 = xs;
                if (i_1 !== j) {
                    const ti = item(i_1, xs_1);
                    setItem(xs_1, i_1, item(j, xs_1));
                    setItem(xs_1, j, ti);
                }
                i = ((idx + 2) | 0);
                idx = ((idx + 1) | 0);
                en = ((ln_3 = oth, Pnt_$ctor_Z7AD9E565(ln_3.ToX, ln_3.ToY, ln_3.ToZ)));
            }
            else {
                i = ((i + 1) | 0);
            }
        }
        let st;
        const ln_4 = ln;
        st = Pnt_$ctor_Z7AD9E565(ln_4.FromX, ln_4.FromY, ln_4.FromZ);
        i = ((idx + 1) | 0);
        while (i < xs.length) {
            let a_3, b_3, ln_5, x_2, y_1, z_1, this$_1, ln_6;
            const oth_1 = getLine(item(i, xs));
            if (((a_3 = st, (b_3 = ((ln_5 = oth_1, Pnt_$ctor_Z7AD9E565(ln_5.ToX, ln_5.ToY, ln_5.ToZ))), (x_2 = (a_3.X - b_3.X), (y_1 = (a_3.Y - b_3.Y), (z_1 = (a_3.Z - b_3.Z), ((x_2 * x_2) + (y_1 * y_1)) + (z_1 * z_1))))))) < distSq) {
                void (((this$_1 = loops, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))).backward.push(item(i, xs)));
                const i_2 = i | 0;
                const j_1 = (idx + 1) | 0;
                const xs_2 = xs;
                if (i_2 !== j_1) {
                    const ti_1 = item(i_2, xs_2);
                    setItem(xs_2, i_2, item(j_1, xs_2));
                    setItem(xs_2, j_1, ti_1);
                }
                i = ((idx + 2) | 0);
                idx = ((idx + 1) | 0);
                st = ((ln_6 = oth_1, Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ)));
            }
            else {
                i = ((i + 1) | 0);
            }
        }
        idx = ((idx + 1) | 0);
    }
    return ResizeArr_map((l) => {
        l.backward.reverse();
        addRangeInPlace(l.forward, l.backward);
        return l.backward;
    }, loops);
}

/**
 * Returns the groups of consecutive elements, loops or polylines.
 * They are split where the distance between the end point of one element and the start point of the next element is greater than 'splitDistance'.
 * For each element it will compute a line given the 'getLine' function.
 * The Line3D is used as an abstraction to hold start and end of arbitrary object.
 * Then for each line start and end point it finds the next closest line end or start point respectively.
 * Start points can match with start points and end points can match with end points too.
 * If the distance between two points is greater than the 'splitDistance'
 * it will be considered as a new group.
 * The result will be a list of lists of 'T and a Boolean values indicating if the element was reversed.
 */
export function Topology3D_joinReversing_Z4850C4CC(getLine, splitDistance, xs) {
    const loops = [];
    const distSq = splitDistance * splitDistance;
    let idx = 0;
    while (idx < xs.length) {
        let x;
        void (loops.push(new TopologyUtil_LoopCollector$1((x = [item(idx, xs), false], [ x ]), [])));
        const ln = getLine(item(idx, xs));
        let en;
        const ln_1 = ln;
        en = Pnt_$ctor_Z7AD9E565(ln_1.ToX, ln_1.ToY, ln_1.ToZ);
        let i = idx + 1;
        while (i < xs.length) {
            let a_1, b_1, ln_2, x_1, y, z, this$, ln_3, a_3, b_3, ln_4, x_2, y_1, z_1, this$_1, ln_5;
            const oth = getLine(item(i, xs));
            if (((a_1 = en, (b_1 = ((ln_2 = oth, Pnt_$ctor_Z7AD9E565(ln_2.FromX, ln_2.FromY, ln_2.FromZ))), (x_1 = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), ((x_1 * x_1) + (y * y)) + (z * z))))))) < distSq) {
                void (((this$ = loops, ((this$.length === 0) ? failRarr("Last.get", this$) : undefined, item(this$.length - 1, this$)))).forward.push([item(i, xs), false]));
                const i_1 = i | 0;
                const j = (idx + 1) | 0;
                const xs_1 = xs;
                if (i_1 !== j) {
                    const ti = item(i_1, xs_1);
                    setItem(xs_1, i_1, item(j, xs_1));
                    setItem(xs_1, j, ti);
                }
                i = ((idx + 2) | 0);
                idx = ((idx + 1) | 0);
                en = ((ln_3 = oth, Pnt_$ctor_Z7AD9E565(ln_3.ToX, ln_3.ToY, ln_3.ToZ)));
            }
            else if (((a_3 = en, (b_3 = ((ln_4 = oth, Pnt_$ctor_Z7AD9E565(ln_4.ToX, ln_4.ToY, ln_4.ToZ))), (x_2 = (a_3.X - b_3.X), (y_1 = (a_3.Y - b_3.Y), (z_1 = (a_3.Z - b_3.Z), ((x_2 * x_2) + (y_1 * y_1)) + (z_1 * z_1))))))) < distSq) {
                void (((this$_1 = loops, ((this$_1.length === 0) ? failRarr("Last.get", this$_1) : undefined, item(this$_1.length - 1, this$_1)))).forward.push([item(i, xs), true]));
                const i_2 = i | 0;
                const j_1 = (idx + 1) | 0;
                const xs_2 = xs;
                if (i_2 !== j_1) {
                    const ti_1 = item(i_2, xs_2);
                    setItem(xs_2, i_2, item(j_1, xs_2));
                    setItem(xs_2, j_1, ti_1);
                }
                i = ((idx + 2) | 0);
                idx = ((idx + 1) | 0);
                en = ((ln_5 = oth, Pnt_$ctor_Z7AD9E565(ln_5.FromX, ln_5.FromY, ln_5.FromZ)));
            }
            else {
                i = ((i + 1) | 0);
            }
        }
        let st;
        const ln_6 = ln;
        st = Pnt_$ctor_Z7AD9E565(ln_6.FromX, ln_6.FromY, ln_6.FromZ);
        i = ((idx + 1) | 0);
        while (i < xs.length) {
            let a_5, b_5, ln_7, x_3, y_2, z_2, this$_2, ln_8, a_7, b_7, ln_9, x_4, y_3, z_3, this$_3, ln_10;
            const oth_1 = getLine(item(i, xs));
            if (((a_5 = st, (b_5 = ((ln_7 = oth_1, Pnt_$ctor_Z7AD9E565(ln_7.ToX, ln_7.ToY, ln_7.ToZ))), (x_3 = (a_5.X - b_5.X), (y_2 = (a_5.Y - b_5.Y), (z_2 = (a_5.Z - b_5.Z), ((x_3 * x_3) + (y_2 * y_2)) + (z_2 * z_2))))))) < distSq) {
                void (((this$_2 = loops, ((this$_2.length === 0) ? failRarr("Last.get", this$_2) : undefined, item(this$_2.length - 1, this$_2)))).backward.push([item(i, xs), false]));
                const i_3 = i | 0;
                const j_2 = (idx + 1) | 0;
                const xs_3 = xs;
                if (i_3 !== j_2) {
                    const ti_2 = item(i_3, xs_3);
                    setItem(xs_3, i_3, item(j_2, xs_3));
                    setItem(xs_3, j_2, ti_2);
                }
                i = ((idx + 2) | 0);
                idx = ((idx + 1) | 0);
                st = ((ln_8 = oth_1, Pnt_$ctor_Z7AD9E565(ln_8.FromX, ln_8.FromY, ln_8.FromZ)));
            }
            else if (((a_7 = st, (b_7 = ((ln_9 = oth_1, Pnt_$ctor_Z7AD9E565(ln_9.FromX, ln_9.FromY, ln_9.FromZ))), (x_4 = (a_7.X - b_7.X), (y_3 = (a_7.Y - b_7.Y), (z_3 = (a_7.Z - b_7.Z), ((x_4 * x_4) + (y_3 * y_3)) + (z_3 * z_3))))))) < distSq) {
                void (((this$_3 = loops, ((this$_3.length === 0) ? failRarr("Last.get", this$_3) : undefined, item(this$_3.length - 1, this$_3)))).backward.push([item(i, xs), true]));
                const i_4 = i | 0;
                const j_3 = (idx + 1) | 0;
                const xs_4 = xs;
                if (i_4 !== j_3) {
                    const ti_3 = item(i_4, xs_4);
                    setItem(xs_4, i_4, item(j_3, xs_4));
                    setItem(xs_4, j_3, ti_3);
                }
                i = ((idx + 2) | 0);
                idx = ((idx + 1) | 0);
                st = ((ln_10 = oth_1, Pnt_$ctor_Z7AD9E565(ln_10.ToX, ln_10.ToY, ln_10.ToZ)));
            }
            else {
                i = ((i + 1) | 0);
            }
        }
        idx = ((idx + 1) | 0);
    }
    return ResizeArr_map((l) => {
        l.backward.reverse();
        addRangeInPlace(l.forward, l.backward);
        return l.backward;
    }, loops);
}

export function Topology_sortToLoop3D(getLine, xs) {
    Topology3D_sortToLoop_Z16A0DAF1(getLine, xs);
}

export function Topology_sortToLoop2D(getLine, xs) {
    Topology2D_sortToLoop_Z16A0C932(getLine, xs);
}

export function Topology_sortToLoopWithReversing3D(getLine, reverseInPlace, xs) {
    Topology3D_sortToLoopWithReversing_Z36DD3996(getLine, reverseInPlace, xs);
}

export function Topology_sortToLoopWithReversing2D(getLine, reverseInPlace, xs) {
    Topology2D_sortToLoopWithReversing_Z36DB21F5(getLine, reverseInPlace, xs);
}

export function Topology_join2D(getLine, splitDistance, xs) {
    return Topology2D_join_Z484E292B(getLine, splitDistance, xs);
}

export function Topology_join3D(getLine, splitDistance, xs) {
    return Topology3D_join_Z4850C4CC(getLine, splitDistance, xs);
}

export function Topology_joinReversing2D(getLine, splitDistance, xs) {
    return Topology2D_joinReversing_Z484E292B(getLine, splitDistance, xs);
}

export function Topology_joinReversing3D(getLine, splitDistance, xs) {
    return Topology3D_joinReversing_Z4850C4CC(getLine, splitDistance, xs);
}

