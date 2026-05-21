
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";

/**
 * A struct containing 4 floats, representing an immutable finite line in 2D.
 */
export class Line2D extends Record {
    constructor(FromX, FromY, ToX, ToY) {
        super();
        this.FromX = FromX;
        this.FromY = FromY;
        this.ToX = ToX;
        this.ToY = ToY;
    }
    /**
     * Format 2D line into string including type name, X and Y for start and end points, and Length.
     * Using nice floating point number formatting.
     */
    toString() {
        let ln_1, x, ln_2, y, ln_3;
        const ln = this;
        return `Euclid.Line2D from X=${float(ln.FromX)}|Y=${float(ln.FromY)} to X=${float(ln.ToX)}|Y=${float(ln.ToY)} Length:${float((ln_1 = ln, (x = ((ln_2 = ln_1, ln_2.ToX - ln_2.FromX)), (y = ((ln_3 = ln_1, ln_3.ToY - ln_3.FromY)), Math.sqrt((x * x) + (y * y))))))}`;
    }
}

export function Line2D_$reflection() {
    return class_type("Euclid.Line2D", undefined, Line2D, class_type("System.ValueType"));
}

/**
 * Create a Line2D from 2D start point and 2D end point.
 */
export function Line2D_$ctor_Z53905FA0(fromPt, toPt) {
    return new Line2D(fromPt.X, fromPt.Y, toPt.X, toPt.Y);
}

/**
 * Create a Line2D from 2D start point's x and y and 2D end point's x and y.
 */
export function Line2D_$ctor_77D16AC0(fromX, fromY, toX, toY) {
    return new Line2D(fromX, fromY, toX, toY);
}

/**
 * Format 2D line into string from X and Y for start and end points.
 * Using nice floating point number formatting.
 * But without full type name as in ln.ToString()
 */
export function Line2D__get_AsString(ln) {
    return `X=${float(ln.FromX)}|Y=${float(ln.FromY)} to X=${float(ln.ToX)}|Y=${float(ln.ToY)}`;
}

/**
 * Format 2D line into an F# code string that can be used to recreate the line.
 */
export function Line2D__get_AsFSharpCode(ln) {
    return `Line2D(${ln.FromX}, ${ln.FromY}, ${ln.ToX}, ${ln.ToY})`;
}

