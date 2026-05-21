
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";
import { isInfinity } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { failDivide, failNaN2 } from "./EuclidErrors.js";
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";

/**
 * Vc is an immutable 2D vector with any length. Made up from 2 floats: X and Y.
 */
export class Vc extends Record {
    constructor(X, Y) {
        super();
        this.X = X;
        this.Y = Y;
    }
    /**
     * Format 2D vector into string including type name and nice floating point number formatting of X, Y and length.
     */
    toString() {
        const v = this;
        const x = float(v.X);
        const y = float(v.Y);
        const length = float(Math.sqrt((v.X * v.X) + (v.Y * v.Y)));
        return `Euclid.Vc: X=${x}|Y=${y}|length: ${length}`;
    }
}

export function Vc_$reflection() {
    return class_type("Euclid.Vc", undefined, Vc, class_type("System.ValueType"));
}

/**
 * Create a new 2D vector from X and Y coordinates.
 */
export function Vc_$ctor_7B00E9A0(x, y) {
    let x_1, x_2;
    if (((x_1 = x, Number.isNaN(x_1) ? true : isInfinity(x_1))) ? true : ((x_2 = y, Number.isNaN(x_2) ? true : isInfinity(x_2)))) {
        failNaN2("Vc()", x, y);
    }
    return new Vc(x, y);
}

/**
 * Format 2D vector into string with nice floating point number formatting of X and Y
 * But without full type name or length as in v.ToString()
 */
export function Vc__get_AsString(v) {
    const x = float(v.X);
    const y = float(v.Y);
    return concat("X=", x, "|Y=", ...y);
}

/**
 * Format 2D vector into an F# code string that can be used to recreate the vector.
 */
export function Vc__get_AsFSharpCode(v) {
    return `Vc(${v.X}, ${v.Y})`;
}

/**
 * Divides the vector by an integer.
 * (This member is needed by Array.average and similar functions)
 * Throws EuclidDivByZeroException if i is 0.
 */
export function Vc_DivideByInt_6B0C9CF0(v, i) {
    if (i === 0) {
        failDivide("Vc.DivideByInt by zero", 0, v);
    }
    const d = i;
    return Vc_$ctor_7B00E9A0(v.X / d, v.Y / d);
}

