
import { float } from "./Format.js";
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { isInfinity } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { failNotOne2, failNaN2 } from "./EuclidErrors.js";

/**
 * A struct containing 2 floats, representing an 2D unitized vector.
 * All instances of this type are guaranteed to be always unitized.
 * Never use the struct default constructor UnitVc()!
 * It will create an invalid zero length vector.
 * Use UnitVc.create or UnitVc.createUnchecked instead.
 */
export class UnitVc extends Record {
    constructor(X, Y) {
        super();
        this.X = X;
        this.Y = Y;
    }
    /**
     * Format 2D unit-vector into string including type name and nice floating point number formatting.
     */
    toString() {
        const v = this;
        const x = float(v.X);
        const y = float(v.Y);
        return concat("Euclid.UnitVc: X=", x, "|Y=", ...y);
    }
}

export function UnitVc_$reflection() {
    return class_type("Euclid.UnitVc", undefined, UnitVc, class_type("System.ValueType"));
}

/**
 * Unsafe internal constructor, doesn't check or unitize the input, public only for inlining.
 */
export function UnitVc_$ctor_7B00E9A0(x, y) {
    let x_2;
    if (((Number.isNaN(x) ? true : Number.isNaN(y)) ? true : isInfinity(x)) ? true : isInfinity(y)) {
        failNaN2("UnitVc()", x, y);
    }
    const lenSq = (x * x) + (y * y);
    if (!((x_2 = lenSq, (0.999999 < x_2) && (x_2 < 1.000001)))) {
        failNotOne2("UnitVc()", x, y);
    }
    return new UnitVc(x, y);
}

/**
 * Format 2D unit-vector into string with nice floating point number formatting of X and Y
 * But without full type name as in v.ToString()
 */
export function UnitVc__get_AsString(v) {
    const x = float(v.X);
    const y = float(v.Y);
    return concat("X=", x, "|Y=", ...y);
}

/**
 * Format 2D unit-vector into an F# code string that can be used to recreate the unit-vector.
 */
export function UnitVc__get_AsFSharpCode(v) {
    return `UnitVc.create(${v.X}, ${v.Y})`;
}

