
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type, record_type, float64_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { setItem, item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { fail } from "./EuclidErrors.js";

export class Cell extends Record {
    constructor(X, Y, H, Distance, MaxDistance) {
        super();
        this.X = X;
        this.Y = Y;
        this.H = H;
        this.Distance = Distance;
        this.MaxDistance = MaxDistance;
    }
}

export function Cell_$reflection() {
    return record_type("Euclid.Polylabel.Cell", [], Cell, () => [["X", float64_type], ["Y", float64_type], ["H", float64_type], ["Distance", float64_type], ["MaxDistance", float64_type]]);
}

export class CellHeap {
    constructor() {
        this.data = [];
    }
}

export function CellHeap_$reflection() {
    return class_type("Euclid.Polylabel.CellHeap", undefined, CellHeap);
}

export function CellHeap_$ctor() {
    return new CellHeap();
}

export function CellHeap__get_Count(_) {
    return _.data.length | 0;
}

export function CellHeap__Add_3F6CBBFD(_, c) {
    void (_.data.push(c));
    let i = _.data.length - 1;
    while (i > 0) {
        let a_1, b_1;
        const parent = ~~((i - 1) / 2) | 0;
        if ((a_1 = item(i, _.data), (b_1 = item(parent, _.data), (a_1.MaxDistance === b_1.MaxDistance) ? ((a_1.X === b_1.X) ? ((a_1.Y === b_1.Y) ? (a_1.H > b_1.H) : (a_1.Y > b_1.Y)) : (a_1.X > b_1.X)) : (a_1.MaxDistance > b_1.MaxDistance)))) {
            const tmp = item(parent, _.data);
            setItem(_.data, parent, item(i, _.data));
            setItem(_.data, i, tmp);
            i = (parent | 0);
        }
        else {
            i = 0;
        }
    }
}

export function CellHeap__Pop(_) {
    if (_.data.length === 0) {
        fail("Polylabel: CellHeap empty");
    }
    const root = item(0, _.data);
    const lastIdx = (_.data.length - 1) | 0;
    setItem(_.data, 0, item(lastIdx, _.data));
    _.data.splice(lastIdx, 1);
    let i = 0;
    const n = _.data.length | 0;
    let cont = true;
    while (cont) {
        let a_1, b_1, a_3, b_3;
        const l = ((2 * i) + 1) | 0;
        const r = (l + 1) | 0;
        if (l >= n) {
            cont = false;
        }
        else {
            let bestChild = l;
            if ((r < n) && ((a_1 = item(r, _.data), (b_1 = item(l, _.data), (a_1.MaxDistance === b_1.MaxDistance) ? ((a_1.X === b_1.X) ? ((a_1.Y === b_1.Y) ? (a_1.H > b_1.H) : (a_1.Y > b_1.Y)) : (a_1.X > b_1.X)) : (a_1.MaxDistance > b_1.MaxDistance))))) {
                bestChild = (r | 0);
            }
            if ((a_3 = item(bestChild, _.data), (b_3 = item(i, _.data), (a_3.MaxDistance === b_3.MaxDistance) ? ((a_3.X === b_3.X) ? ((a_3.Y === b_3.Y) ? (a_3.H > b_3.H) : (a_3.Y > b_3.Y)) : (a_3.X > b_3.X)) : (a_3.MaxDistance > b_3.MaxDistance)))) {
                const tmp = item(i, _.data);
                setItem(_.data, i, item(bestChild, _.data));
                setItem(_.data, bestChild, tmp);
                i = (bestChild | 0);
            }
            else {
                cont = false;
            }
        }
    }
    return root;
}

