
import { fail } from "./EuclidErrors.js";
import { tryFind, getRange, item } from "../fable_modules/fable-library-js.5.0.0/Array.js";
import { defaultOf, compare } from "../fable_modules/fable-library-js.5.0.0/Util.js";
import { concat } from "../fable_modules/fable-library-js.5.0.0/String.js";
import { rarr as rarr_1, nl } from "./Format.js";
import { singleton, map, append, delay } from "../fable_modules/fable-library-js.5.0.0/Seq.js";
import { rangeDouble } from "../fable_modules/fable-library-js.5.0.0/Range.js";
import { some, defaultArg } from "../fable_modules/fable-library-js.5.0.0/Option.js";

/**
 * Returns the index of the smallest element.
 */
export function Arr_minIndex(xs) {
    if (xs.length < 1) {
        fail("Arr.minIndex failed on empty array.");
    }
    let f = item(0, xs);
    let mf = f;
    let ii = 0;
    for (let i = 1; i <= (xs.length - 1); i++) {
        f = item(i, xs);
        if (compare(f, mf) < 0) {
            ii = (i | 0);
            mf = f;
        }
    }
    return ii | 0;
}

/**
 * Returns the index of the biggest element.
 */
export function Arr_maxIndex(xs) {
    if (xs.length < 1) {
        fail("Arr.maxIndex failed on empty array.");
    }
    let f = item(0, xs);
    let mf = f;
    let ii = 0;
    for (let i = 1; i <= (xs.length - 1); i++) {
        f = item(i, xs);
        if (compare(f, mf) > 0) {
            ii = (i | 0);
            mf = f;
        }
    }
    return ii | 0;
}

/**
 * Yields looped Seq from (first, second)  up to (last, first).
 * The resulting seq has the same element count as the input Rarr.
 */
export function ResizeArr_thisNext(rarr) {
    if (rarr.length <= 2) {
        fail(concat("ResizeArr.thisNext input has less than two items:", nl, ...rarr_1(rarr)));
    }
    return delay(() => append(map((i) => [item(i, rarr), item(i + 1, rarr)], rangeDouble(0, 1, rarr.length - 2)), delay(() => singleton([item(rarr.length - 1, rarr), item(0, rarr)]))));
}

/**
 * Yields looped Seq from (1, last, first, second)  up to (lastIndex, second-last, last, first)
 * The resulting seq has the same element count as the input Rarr.
 */
export function ResizeArr_iPrevThisNext(xs) {
    if (xs.length <= 3) {
        fail(concat("ResizeArr.iPrevThisNext input has less than three items:", nl, ...rarr_1(xs)));
    }
    return delay(() => append(singleton([0, item(xs.length - 1, xs), item(0, xs), item(1, xs)]), delay(() => append(map((i) => [i + 1, item(i, xs), item(i + 1, xs), item(i + 2, xs)], rangeDouble(0, 1, xs.length - 3)), delay(() => singleton([xs.length - 1, item(xs.length - 2, xs), item(xs.length - 1, xs), item(0, xs)]))))));
}

/**
 * Sorts the elements of a ResizeArray, using the given projection for the keys and returning a new ResizeArr.
 * Elements are compared using <see cref="M:Microsoft.FSharp.Core.Operators.compare"/>.
 * This means "Z" is before "a". This is different from Collections.Generic.List.Sort() where "a" is before "Z" using IComparable interface.
 * This is NOT a stable sort, i.e. the original order of equal elements is not necessarily preserved.
 * For a stable sort, consider using Seq.sort.
 */
export function ResizeArr_sortBy(projection, xs) {
    const r = getRange(xs, 0, xs.length);
    r.sort((x, y) => (compare(projection(x), projection(y)) | 0));
    return r;
}

/**
 * Returns the index of the first element in the ResizeArray
 * that satisfies the given predicate.
 */
export function ResizeArr_tryFindIndex(predicate, xs) {
    const elementIndex = xs.findIndex(predicate) | 0;
    if (elementIndex === -1) {
        return undefined;
    }
    else {
        return elementIndex;
    }
}

/**
 * Returns the index of the first element in the ResizeArray
 * that satisfies the given predicate.
 */
export function ResizeArr_findIndex(predicate, xs) {
    return xs.findIndex(predicate) | 0;
}

export function ResizeArr_find(predicate, xs) {
    return defaultArg(tryFind(predicate, xs), defaultOf());
}

export function ResizeArr_findLast(predicate, xs) {
    let loopOn = false;
    let i = xs.length - 1;
    while (loopOn) {
        if (predicate(item(i, xs))) {
            loopOn = false;
        }
        else {
            i = ((i - 1) | 0);
            if (i < 0) {
                fail(`ResizeArr.findLast: None of the ${xs.length} elements satisfies the predicate.`);
            }
        }
    }
    return item(i, xs);
}

export function ResizeArr_findLastIndex(predicate, xs) {
    let r = -1;
    let i = xs.length - 1;
    while ((r === -1) && (i >= 0)) {
        if (predicate(item(i, xs))) {
            r = (i | 0);
        }
        i = ((i - 1) | 0);
    }
    return r | 0;
}

/**
 * Returns a new ResizeArray with the elements in reverse order.
 */
export function ResizeArr_rev(xs) {
                const rs = new Array(xs.length);
            const lastIdx = xs.length - 1; 
            for (let i = 0; i < xs.length; i++) 
                { rs[i] = xs[lastIdx - i]; }; 
            return rs ;
}

/**
 * Returns the index of the smallest of all elements of the ResizeArray, compared via Operators.max on the function result.
 */
export function ResizeArr_minIndexBy(projection, xs) {
    if (xs.length === 0) {
        fail("ResizeArr.minIndBy: Failed on empty ResizeArray.");
    }
    let f = projection(item(0, xs));
    let mf = f;
    let ii = 0;
    for (let i = 1; i <= (xs.length - 1); i++) {
        f = projection(item(i, xs));
        if (compare(f, mf) < 0) {
            ii = (i | 0);
            mf = f;
        }
    }
    return ii | 0;
}

/**
 * Returns the index of the greatest of all elements of the ResizeArray, compared via Operators.max on the function result.
 */
export function ResizeArr_maxIndexBy(projection, xs) {
    if (xs.length === 0) {
        fail("ResizeArr.maxIndBy: Failed on empty ResizeArray.");
    }
    let f = projection(item(0, xs));
    let mf = f;
    let ii = 0;
    for (let i = 1; i <= (xs.length - 1); i++) {
        f = projection(item(i, xs));
        if (compare(f, mf) > 0) {
            ii = (i | 0);
            mf = f;
        }
    }
    return ii | 0;
}

/**
 * Builds a new ResizeArray whose elements are the results of applying the given function
 * to each of the elements of the ResizeArr.
 */
export function ResizeArr_map(mapping, xs) {
    return xs.map(mapping);
}

/**
 * Creates a shallow copy of the input ResizeArray and adds the first element to the end, closing the loop.
 */
export function ResizeArr_closeLoop(pts) {
                const xs = new Array(pts.length + 1);            
            for (let i = 0; i < pts.length; i++) 
                { xs[i] = pts[i]; }; 
            xs[pts.length] = pts[0];
            return xs ;
}

/**
 * Returns the last element for which the given function returns <c>true</c>.
 * Return None if no such element exists.
 */
export function ResizeArr_tryFindBack(predicate, resizeArray) {
    let i = resizeArray.length - 1;
    let result = undefined;
    while (i >= 0) {
        const element = item(i, resizeArray);
        i = ((i - 1) | 0);
        if (predicate(element)) {
            result = some(element);
            i = -1;
        }
    }
    return result;
}

