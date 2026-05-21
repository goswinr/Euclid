
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { failDivide } from "./EuclidErrors.js";

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
        return concat("Euclid.Pt: X=", float(p.X), "|Y=", ...float(p.Y));
    }
}

export function Pt_$reflection() {
    return class_type("Euclid.Pt", undefined, Pt, class_type("System.ValueType"));
}

/**
 * Create a new 2D point from X and Y coordinates.
 */
export function Pt_$ctor_7B00E9A0(x, y) {
    return new Pt(x, y);
}

/**
 * Format 2D point into string with nice floating point number formatting of X and Y
 * But without full type name as in p.ToString()
 */
export function Pt__get_AsString(p) {
    return concat("X=", float(p.X), "|Y=", ...float(p.Y));
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

