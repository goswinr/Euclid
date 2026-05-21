
import { equals, defaultOf, createAtom } from "../fable_modules/fable-library-js.5.0.0/Util.js";
import { StringBuilder__Append_244C7CD6, StringBuilder_$ctor_Z524259A4 } from "../fable_modules/fable-library-js.5.0.0/System.Text.js";
import { max } from "../fable_modules/fable-library-js.5.0.0/Double.js";
import { toString } from "../fable_modules/fable-library-js.5.0.0/Types.js";
import { interpolate, substring } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { count } from "../fable_modules/fable-library-js.5.0.0/CollectionUtil.js";

export let userZeroTolerance = createAtom(1E-24);

export let globalThousandSeparator = createAtom("\'");

export const nl = "\n";

/**
 * Insert thousand separators into a string representing a float or int.
 * Before and after the decimal point.
 * Assumes a string that represent a float or int
 * with '.' as decimal separator and no other input formatting.
 */
export function addThousandSeparators(thousandSeparator, number) {
    const b = StringBuilder_$ctor_Z524259A4((number.length + ~~(number.length / 3)) + 1);
    let start;
    if (number[0] === "-") {
        StringBuilder__Append_244C7CD6(b, "-");
        start = 1;
    }
    else {
        start = 0;
    }
    const matchValue = number.indexOf(".") | 0;
    if (matchValue === -1) {
        const matchValue_1 = max(number.indexOf("e"), number.indexOf("E")) | 0;
        if (matchValue_1 === -1) {
            const en = (number.length - 1) | 0;
            for (let i = start; i <= (en - 1); i++) {
                const rest = (en - i) | 0;
                StringBuilder__Append_244C7CD6(b, number[i]);
                if ((rest % 3) === 0) {
                    StringBuilder__Append_244C7CD6(b, thousandSeparator);
                }
            }
            StringBuilder__Append_244C7CD6(b, number[en]);
        }
        else {
            const eIdx = matchValue_1 | 0;
            const en_1 = (eIdx - 1) | 0;
            for (let i_1 = start; i_1 <= (en_1 - 1); i_1++) {
                const rest_1 = (en_1 - i_1) | 0;
                StringBuilder__Append_244C7CD6(b, number[i_1]);
                if ((rest_1 % 3) === 0) {
                    StringBuilder__Append_244C7CD6(b, thousandSeparator);
                }
            }
            StringBuilder__Append_244C7CD6(b, number[en_1]);
            for (let e = eIdx; e <= (number.length - 1); e++) {
                StringBuilder__Append_244C7CD6(b, number[e]);
            }
        }
    }
    else {
        const periodIdx = matchValue | 0;
        if (periodIdx > start) {
            const en_2 = (periodIdx - 1) | 0;
            for (let i_2 = start; i_2 <= (en_2 - 1); i_2++) {
                const rest_2 = (en_2 - i_2) | 0;
                StringBuilder__Append_244C7CD6(b, number[i_2]);
                if ((rest_2 % 3) === 0) {
                    StringBuilder__Append_244C7CD6(b, thousandSeparator);
                }
            }
            StringBuilder__Append_244C7CD6(b, number[en_2]);
        }
        StringBuilder__Append_244C7CD6(b, ".");
        if (periodIdx < number.length) {
            const matchValue_2 = max(number.indexOf("e"), number.indexOf("E")) | 0;
            if (matchValue_2 === -1) {
                const st_3 = (periodIdx + 1) | 0;
                const en_3 = (number.length - 1) | 0;
                StringBuilder__Append_244C7CD6(b, number[st_3]);
                for (let i_3 = st_3 + 1; i_3 <= en_3; i_3++) {
                    if (((i_3 - st_3) % 3) === 0) {
                        StringBuilder__Append_244C7CD6(b, thousandSeparator);
                    }
                    StringBuilder__Append_244C7CD6(b, number[i_3]);
                }
            }
            else {
                const eIdx_1 = matchValue_2 | 0;
                const st_4 = (periodIdx + 1) | 0;
                const en_4 = (eIdx_1 - 1) | 0;
                StringBuilder__Append_244C7CD6(b, number[st_4]);
                for (let i_4 = st_4 + 1; i_4 <= en_4; i_4++) {
                    if (((i_4 - st_4) % 3) === 0) {
                        StringBuilder__Append_244C7CD6(b, thousandSeparator);
                    }
                    StringBuilder__Append_244C7CD6(b, number[i_4]);
                }
                for (let e_1 = eIdx_1; e_1 <= (number.length - 1); e_1++) {
                    StringBuilder__Append_244C7CD6(b, number[e_1]);
                }
            }
        }
    }
    return toString(b);
}

/**
 * Remove trailing zeros but keep last zero if dot reached
 */
export function ToMaxDigits_trimZeros(n) {
    let l = n.length;
    let i = n.length - 1;
    while (i >= 0) {
        const c = n[i];
        switch (c) {
            case "0": {
                l = ((l - 1) | 0);
                break;
            }
            case ".": {
                l = ((l + 1) | 0);
                i = -2;
                break;
            }
            default:
                i = -2;
        }
        i = ((i - 1) | 0);
    }
    return substring(n, 0, l);
}

export function ToMaxDigits_d0(x) {
    return x.toFixed(0);
}

export function ToMaxDigits_d1(x) {
    return ToMaxDigits_trimZeros(x.toFixed(1));
}

export function ToMaxDigits_d2(x) {
    return ToMaxDigits_trimZeros(x.toFixed(2));
}

export function ToMaxDigits_d3(x) {
    return ToMaxDigits_trimZeros(x.toFixed(3));
}

export function ToMaxDigits_d4(x) {
    return ToMaxDigits_trimZeros(x.toFixed(4));
}

export function ToMaxDigits_d5(x) {
    return ToMaxDigits_trimZeros(x.toFixed(5));
}

export function ToMaxDigits_d6(x) {
    return ToMaxDigits_trimZeros(x.toFixed(6));
}

export function ToMaxDigits_d7(x) {
    return ToMaxDigits_trimZeros(x.toFixed(7));
}

export function ToMaxDigits_d8(x) {
    return ToMaxDigits_trimZeros(x.toFixed(8));
}

export function ToMaxDigits_d9(x) {
    return ToMaxDigits_trimZeros(x.toFixed(9));
}

export function ToMaxDigits_d10(x) {
    return ToMaxDigits_trimZeros(x.toFixed(10));
}

/**
 * Formatting double precision floating point numbers with automatic precision.
 * e.g.: 0 digits behind comma if above 1000
 * If the value is smaller than 'userZeroTolerance' (1e-24)  '~0.0' will be shown.
 * If the value is smaller than  (1e-7)  '≈+0.0' will be shown.
 * The thousand separator character is used if more than 3 digits are next to each other before and after the comma.
 * If the separator is the NUL character '\000' no separator will be added.
 */
export function floatWithSeparator(thousandSeparator, x) {
    if (Number.isNaN(x)) {
        return "NaN";
    }
    else {
        switch (x) {
            case -Infinity:
                return "-∞";
            case Infinity:
                return "∞";
            case -1.23432101234321E+308:
                return "RhinoMath.UnsetDouble";
            case 0:
                return "0.0";
            default: {
                const a = Math.abs(x);
                if (a < userZeroTolerance()) {
                    return "~0.0";
                }
                else if (a >= 9999.5) {
                    return addThousandSeparators(thousandSeparator, ToMaxDigits_d0(x));
                }
                else if (a >= 1000) {
                    return ToMaxDigits_d0(x);
                }
                else if (a >= 100) {
                    return ToMaxDigits_d1(x);
                }
                else if (a >= 10) {
                    return ToMaxDigits_d2(x);
                }
                else if (a >= 1) {
                    return ToMaxDigits_d3(x);
                }
                else if (a >= 0.1) {
                    return ToMaxDigits_d4(x);
                }
                else if (a >= 0.01) {
                    return ToMaxDigits_d5(x);
                }
                else if (a >= 0.001) {
                    return addThousandSeparators(thousandSeparator, ToMaxDigits_d6(x));
                }
                else if (a >= 0.0001) {
                    return addThousandSeparators(thousandSeparator, ToMaxDigits_d7(x));
                }
                else if (a >= 1E-05) {
                    return addThousandSeparators(thousandSeparator, ToMaxDigits_d8(x));
                }
                else if (a >= 1E-06) {
                    return addThousandSeparators(thousandSeparator, ToMaxDigits_d9(x));
                }
                else if (a >= 1E-07) {
                    return addThousandSeparators(thousandSeparator, ToMaxDigits_d10(x));
                }
                else if (x >= 0) {
                    return "≈+0.0";
                }
                else {
                    return "≈-0.0";
                }
            }
        }
    }
}

/**
 * Formatting double precision floating point numbers with automatic precision.
 * e.g.: 0 digits behind comma if above 1000
 * If the value is smaller than 'userZeroTolerance' (1e-24)  '~0.0' will be shown.
 * If the value is smaller than  (1e-7)  '≈+0.0' will be shown.
 * The global thousand separator, a tick (') is used if more than 3 digits are next to each other before and after the comma.
 * change it at 'Euclid.Format.globalThousandSeparator'
 */
export function float(x) {
    return floatWithSeparator(globalThousandSeparator(), x);
}

export function rarr(xs) {
    if (xs === defaultOf()) {
        return "null-ResizeArray";
    }
    else {
        switch (xs.length) {
            case 0:
                return "empty ResizeArray";
            case 1:
                return `ResizeArray with one item: [${interpolate("%O%P()", [item(0, xs)])}]`;
            case 2:
                return `ResizeArray with two items: [${nl}  ${interpolate("%O%P()", [item(0, xs)])}${nl}  ${interpolate("%O%P()", [item(1, xs)])}${nl}  ]`;
            case 3:
                return `ResizeArray with three items: [${nl}  ${interpolate("%O%P()", [item(0, xs)])}${nl}  ${interpolate("%O%P()", [item(1, xs)])}${nl}  ${interpolate("%O%P()", [item(2, xs)])}${nl}  ]`;
            case 4:
                return `ResizeArray with four items: [${nl}  ${interpolate("%O%P()", [item(0, xs)])}${nl}  ${interpolate("%O%P()", [item(1, xs)])}${nl}  ${interpolate("%O%P()", [item(2, xs)])}${nl}  ${interpolate("%O%P()", [item(3, xs)])}${nl}  ]`;
            default:
                return `ResizeArray with ${xs.length} items: [${nl}  ${interpolate("%O%P()", [item(0, xs)])}${nl}  ${interpolate("%O%P()", [item(1, xs)])}${nl}  ${interpolate("%O%P()", [item(2, xs)])}${nl}  ${interpolate("%O%P()", [item(3, xs)])}${nl}  ...${nl}  ${interpolate("%O%P()", [item(xs.length - 1, xs)])}${nl}  ]`;
        }
    }
}

export function iList(xs) {
    if (equals(xs, defaultOf())) {
        return "null-IList<\'T>";
    }
    else if (count(xs) === 0) {
        return "empty IList<\'T>";
    }
    else if (count(xs) === 1) {
        return `IList<'T> with one item: [${interpolate("%O%P()", [item(0, xs)])}]`;
    }
    else if (count(xs) === 2) {
        return `IList<'T> with two items: [${nl}  ${interpolate("%O%P()", [item(0, xs)])}${nl}  ${interpolate("%O%P()", [item(1, xs)])}${nl}  ]`;
    }
    else if (count(xs) === 3) {
        return `IList<'T> with three items: [${nl}  ${interpolate("%O%P()", [item(0, xs)])}${nl}  ${interpolate("%O%P()", [item(1, xs)])}${nl}  ${interpolate("%O%P()", [item(2, xs)])}${nl}  ]`;
    }
    else if (count(xs) === 4) {
        return `IList<'T> with four items: [${nl}  ${interpolate("%O%P()", [item(0, xs)])}${nl}  ${interpolate("%O%P()", [item(1, xs)])}${nl}  ${interpolate("%O%P()", [item(2, xs)])}${nl}  ${interpolate("%O%P()", [item(3, xs)])}${nl}  ]`;
    }
    else {
        return `IList<'T> with ${count(xs)} items: [${nl}  ${interpolate("%O%P()", [item(0, xs)])}${nl}  ${interpolate("%O%P()", [item(1, xs)])}${nl}  ${interpolate("%O%P()", [item(2, xs)])}${nl}  ${interpolate("%O%P()", [item(3, xs)])}${nl}  ...${nl}  ${interpolate("%O%P()", [item(count(xs) - 1, xs)])}${nl}  ]`;
    }
}

