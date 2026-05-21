
import { float } from "./Format.js";
import { Record } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { class_type } from "../fable_modules/fable-library-js.5.0.0/Reflection.js";

/**
 * A struct containing 6 floats, representing an immutable finite line in 3D.
 */
export class Line3D extends Record {
    constructor(FromX, FromY, FromZ, ToX, ToY, ToZ) {
        super();
        this.FromX = FromX;
        this.FromY = FromY;
        this.FromZ = FromZ;
        this.ToX = ToX;
        this.ToY = ToY;
        this.ToZ = ToZ;
    }
    /**
     * Format 3D line into string including type name, X, Y and Z for start and end points, and Length.
     * Using nice floating point number formatting.
     */
    toString() {
        let ln_1, x, ln_2, y, ln_3, z, ln_4;
        const ln = this;
        return `Euclid.Line3D from X=${float(ln.FromX)}|Y=${float(ln.FromY)}|Z=${float(ln.FromZ)} to X=${float(ln.ToX)}|Y=${float(ln.ToY)}|Z=${float(ln.ToZ)} Length ${float((ln_1 = ln, (x = ((ln_2 = ln_1, ln_2.ToX - ln_2.FromX)), (y = ((ln_3 = ln_1, ln_3.ToY - ln_3.FromY)), (z = ((ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ)), Math.sqrt(((x * x) + (y * y)) + (z * z)))))))}`;
    }
}

export function Line3D_$reflection() {
    return class_type("Euclid.Line3D", undefined, Line3D, class_type("System.ValueType"));
}

/**
 * Create Line3D from 3D start point and 3D end point.
 */
export function Line3D_$ctor_5A6659A0(fromPt, toPt) {
    return new Line3D(fromPt.X, fromPt.Y, fromPt.Z, toPt.X, toPt.Y, toPt.Z);
}

/**
 * Create Line3D from 3D start point's x, y, and z and 3D end point's x, y, and z.
 */
export function Line3D_$ctor_76A78260(fromX, fromY, fromZ, toX, toY, toZ) {
    return new Line3D(fromX, fromY, fromZ, toX, toY, toZ);
}

/**
 * Format 3D line into string from X, Y and Z for start and end points.
 * Using nice floating point number formatting.
 * But without full type name as in ln.ToString()
 */
export function Line3D__get_AsString(ln) {
    return `${float(ln.FromX)}, ${float(ln.FromY)}, ${float(ln.FromZ)} to ${float(ln.ToX)}, ${float(ln.ToY)}, ${float(ln.ToZ)}`;
}

/**
 * Format 3D line into an F# code string that can be used to recreate the line.
 */
export function Line3D__get_AsFSharpCode(ln) {
    return `Line3D(${ln.FromX}, ${ln.FromY}, ${ln.FromZ}, ${ln.ToX}, ${ln.ToY}, ${ln.ToZ})`;
}

