
import { float } from "./Format.js";
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { isInfinity } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { failDivide, failNaN2 } from "./EuclidErrors.js";

/**
 * Pt is an immutable 2D point. Made up from 2 floats: X and Y.
 */
export class Pt extends Record {
    constructor(X, Y) {
        super();
        this.X = X;
        this.Y = Y;
    }
    /**
     * Format 2D point into string including type name and nice floating point number formatting.
     */
    toString() {
        const p = this;
        const x = float(p.X);
        const y = float(p.Y);
        return concat("Euclid.Pt: X=", x, "|Y=", ...y);
    }
}

export function Pt_$reflection() {
    return class_type("Euclid.Pt", undefined, Pt, class_type("System.ValueType"));
}

/**
 * Create a new 2D point from X and Y coordinates.
 */
export function Pt_$ctor_7B00E9A0(x, y) {
    let x_1, x_2;
    if (((x_1 = x, Number.isNaN(x_1) ? true : isInfinity(x_1))) ? true : ((x_2 = y, Number.isNaN(x_2) ? true : isInfinity(x_2)))) {
        failNaN2("Pt()", x, y);
    }
    return new Pt(x, y);
}

/**
 * Format 2D point into string with nice floating point number formatting of X and Y
 * But without full type name as in p.ToString()
 */
export function Pt__get_AsString(p) {
    const x = float(p.X);
    const y = float(p.Y);
    return concat("X=", x, "|Y=", ...y);
}

/**
 * Format 2D point into an F# code string that can be used to recreate the point.
 */
export function Pt__get_AsFSharpCode(p) {
    return `Pt(${p.X}, ${p.Y})`;
}

/**
 * Divides the 2D point by an integer.
 * (This member is needed by Array.average and similar functions)
 */
export function Pt_DivideByInt_6B0C92C1(pt, i) {
    if (i === 0) {
        failDivide("Pt.DivideByInt", 0, pt);
    }
    const d = i;
    return Pt_$ctor_7B00E9A0(pt.X / d, pt.Y / d);
}

