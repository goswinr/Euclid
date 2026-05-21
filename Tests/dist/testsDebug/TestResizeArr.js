
import { Expect_isFalse, Expect_isTrue, Test_TestCaseBuilder__For_Z371464DD, Expect_throws, Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { ResizeArr_tryFindBack, ResizeArr_closeLoop, ResizeArr_map, ResizeArr_maxIndexBy, ResizeArr_minIndexBy, ResizeArr_rev, ResizeArr_findLastIndex, ResizeArr_find, ResizeArr_findIndex, ResizeArr_tryFindIndex, ResizeArr_sortBy, ResizeArr_iPrevThisNext, ResizeArr_thisNext, Arr_maxIndex, Arr_minIndex } from "./Src/ResizeArr.js";
import { equals as equals_1, equalArrays, Exception, int32ToString, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { option_type, tuple_type, equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { item as item_1, length, contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { setItem, item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { toArray, toList } from "./fable_modules/fable-library-js.5.0.0/Seq.js";
import { toString } from "./fable_modules/fable-library-js.5.0.0/Types.js";
import { rangeDouble } from "./fable_modules/fable-library-js.5.0.0/Range.js";
import { failRarr } from "./Src/EuclidErrors.js";

export const tests = Test_testList("ResizeArr", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.minIndex returns index of smallest element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const arr = new Int32Array([5, 3, 8, 1, 7]);
        const actual = Arr_minIndex(arr) | 0;
        if ((actual === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 3, "index of 1");
        }
        else {
            let valueType;
            let copyOfStruct = actual;
            valueType = int32_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            })) {
                const arg = int32ToString(3);
                const arg_1 = int32ToString(actual);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("index of 1");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual)("index of 1");
            }
            throw new Exception(errorMsg);
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.minIndex single element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const arr_1 = new Int32Array([42]);
        const actual_1 = Arr_minIndex(arr_1) | 0;
        if ((actual_1 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 0, "single element");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_1;
            valueType_1 = int32_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg_6 = int32ToString(0);
                const arg_1_1 = int32ToString(actual_1);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("single element");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_1)("single element");
            }
            throw new Exception(errorMsg_1);
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.minIndex first element is smallest", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const arr_2 = new Int32Array([1, 2, 3]);
        const actual_2 = Arr_minIndex(arr_2) | 0;
        if ((actual_2 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 0, "first is smallest");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_2;
            valueType_2 = int32_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_7 = int32ToString(0);
                const arg_1_2 = int32ToString(actual_2);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("first is smallest");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_2)("first is smallest");
            }
            throw new Exception(errorMsg_2);
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.minIndex last element is smallest", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const arr_3 = new Int32Array([3, 2, 1]);
        const actual_3 = Arr_minIndex(arr_3) | 0;
        if ((actual_3 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 2, "last is smallest");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_3;
            valueType_3 = int32_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            })) {
                const arg_8 = int32ToString(2);
                const arg_1_3 = int32ToString(actual_3);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("last is smallest");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_3)("last is smallest");
            }
            throw new Exception(errorMsg_3);
        }
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.minIndex duplicate minimums returns first", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const arr_4 = new Int32Array([5, 1, 3, 1, 7]);
        const actual_4 = Arr_minIndex(arr_4) | 0;
        if ((actual_4 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, 1, "first occurrence of min");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_4;
            valueType_4 = int32_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_9 = int32ToString(1);
                const arg_1_4 = int32ToString(actual_4);
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("first occurrence of min");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_4)("first occurrence of min");
            }
            throw new Exception(errorMsg_4);
        }
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.minIndex fails on empty array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        Expect_throws(() => {
            Arr_minIndex([]);
        }, "empty array fails");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.maxIndex returns index of biggest element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const arr_5 = new Int32Array([5, 3, 8, 1, 7]);
        const actual_5 = Arr_maxIndex(arr_5) | 0;
        if ((actual_5 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 2, "index of 8");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_5;
            valueType_5 = int32_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_10 = int32ToString(2);
                const arg_1_5 = int32ToString(actual_5);
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("index of 8");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_5)("index of 8");
            }
            throw new Exception(errorMsg_5);
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.maxIndex single element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const arr_6 = new Int32Array([42]);
        const actual_6 = Arr_maxIndex(arr_6) | 0;
        if ((actual_6 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, 0, "single element");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_6;
            valueType_6 = int32_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg_11 = int32ToString(0);
                const arg_1_6 = int32ToString(actual_6);
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("single element");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_6)("single element");
            }
            throw new Exception(errorMsg_6);
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.maxIndex first element is largest", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const arr_7 = new Int32Array([9, 2, 3]);
        const actual_7 = Arr_maxIndex(arr_7) | 0;
        if ((actual_7 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 0, "first is largest");
        }
        else {
            let valueType_7;
            let copyOfStruct_7 = actual_7;
            valueType_7 = int32_type;
            const primitiveTypes_7 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_7;
            if (contains(valueType_7, primitiveTypes_7, {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            })) {
                const arg_12 = int32ToString(0);
                const arg_1_7 = int32ToString(actual_7);
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("first is largest");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_7)("first is largest");
            }
            throw new Exception(errorMsg_7);
        }
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.maxIndex last element is largest", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const arr_8 = new Int32Array([1, 2, 9]);
        const actual_8 = Arr_maxIndex(arr_8) | 0;
        if ((actual_8 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 2, "last is largest");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_8;
            valueType_8 = int32_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            })) {
                const arg_13 = int32ToString(2);
                const arg_1_8 = int32ToString(actual_8);
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("last is largest");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_8)("last is largest");
            }
            throw new Exception(errorMsg_8);
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.maxIndex duplicate maximums returns first", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const arr_9 = new Int32Array([5, 9, 3, 9, 7]);
        const actual_9 = Arr_maxIndex(arr_9) | 0;
        if ((actual_9 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 1, "first occurrence of max");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_9;
            valueType_9 = int32_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            })) {
                const arg_14 = int32ToString(1);
                const arg_1_9 = int32ToString(actual_9);
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("first occurrence of max");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_9)("first occurrence of max");
            }
            throw new Exception(errorMsg_9);
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Arr.maxIndex fails on empty array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        Expect_throws(() => {
            Arr_maxIndex([]);
        }, "empty array fails");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.length returns Count", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const r = [];
        void (r.push(1));
        void (r.push(2));
        void (r.push(3));
        const actual_10 = r.length | 0;
        if ((actual_10 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 3, "length is 3");
        }
        else {
            let valueType_10;
            let copyOfStruct_10 = actual_10;
            valueType_10 = int32_type;
            const primitiveTypes_10 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_10;
            if (contains(valueType_10, primitiveTypes_10, {
                Equals: equals,
                GetHashCode: (x_10) => (structuralHash(x_10) | 0),
            })) {
                const arg_15 = int32ToString(3);
                const arg_1_10 = int32ToString(actual_10);
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("length is 3");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_10)("length is 3");
            }
            throw new Exception(errorMsg_10);
        }
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.init creates with indices", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let r_1;
                    const xs = new Array(4); 
            for (let i = 0; i < 4; i++) 
                { xs[i] = ((i) => ((i * 2) | 0))(i); }; 
            return xs ;
        const actual_11 = r_1.length | 0;
        if ((actual_11 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 4, "4 elements");
        }
        else {
            let valueType_11;
            let copyOfStruct_11 = actual_11;
            valueType_11 = int32_type;
            const primitiveTypes_11 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_11;
            if (contains(valueType_11, primitiveTypes_11, {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            })) {
                const arg_16 = int32ToString(4);
                const arg_1_11 = int32ToString(actual_11);
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("4 elements");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_11)("4 elements");
            }
            throw new Exception(errorMsg_11);
        }
        const actual_12 = item(0, r_1) | 0;
        if ((actual_12 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 0, "0*2");
        }
        else {
            let valueType_12;
            let copyOfStruct_12 = actual_12;
            valueType_12 = int32_type;
            const primitiveTypes_12 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_12;
            if (contains(valueType_12, primitiveTypes_12, {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            })) {
                const arg_17 = int32ToString(0);
                const arg_1_12 = int32ToString(actual_12);
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("0*2");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_12)("0*2");
            }
            throw new Exception(errorMsg_12);
        }
        const actual_13 = item(1, r_1) | 0;
        if ((actual_13 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 2, "1*2");
        }
        else {
            let valueType_13;
            let copyOfStruct_13 = actual_13;
            valueType_13 = int32_type;
            const primitiveTypes_13 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_13;
            if (contains(valueType_13, primitiveTypes_13, {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            })) {
                const arg_18 = int32ToString(2);
                const arg_1_13 = int32ToString(actual_13);
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("1*2");
            }
            else {
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_13)("1*2");
            }
            throw new Exception(errorMsg_13);
        }
        const actual_14 = item(2, r_1) | 0;
        if ((actual_14 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, 4, "2*2");
        }
        else {
            let valueType_14;
            let copyOfStruct_14 = actual_14;
            valueType_14 = int32_type;
            const primitiveTypes_14 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_14;
            if (contains(valueType_14, primitiveTypes_14, {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            })) {
                const arg_19 = int32ToString(4);
                const arg_1_14 = int32ToString(actual_14);
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("2*2");
            }
            else {
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_14)("2*2");
            }
            throw new Exception(errorMsg_14);
        }
        const actual_15 = item(3, r_1) | 0;
        if ((actual_15 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, 6, "3*2");
        }
        else {
            let valueType_15;
            let copyOfStruct_15 = actual_15;
            valueType_15 = int32_type;
            const primitiveTypes_15 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_15;
            if (contains(valueType_15, primitiveTypes_15, {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            })) {
                const arg_20 = int32ToString(6);
                const arg_1_15 = int32ToString(actual_15);
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("3*2");
            }
            else {
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_15)("3*2");
            }
            throw new Exception(errorMsg_15);
        }
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.init zero count", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let r_2;
                    const xs = new Array(0); 
            for (let i = 0; i < 0; i++) 
                { xs[i] = ((i_1) => (i_1 | 0))(i); }; 
            return xs ;
        const actual_16 = r_2.length | 0;
        if ((actual_16 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, 0, "empty");
        }
        else {
            let valueType_16;
            let copyOfStruct_16 = actual_16;
            valueType_16 = int32_type;
            const primitiveTypes_16 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_16;
            if (contains(valueType_16, primitiveTypes_16, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_21 = int32ToString(0);
                const arg_1_16 = int32ToString(actual_16);
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("empty");
            }
            else {
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_16)("empty");
            }
            throw new Exception(errorMsg_16);
        }
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.singleton creates single element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const r_3 = [ "hello" ];
        const actual_17 = r_3.length | 0;
        if ((actual_17 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, 1, "one element");
        }
        else {
            let valueType_17;
            let copyOfStruct_17 = actual_17;
            valueType_17 = int32_type;
            const primitiveTypes_17 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_17;
            if (contains(valueType_17, primitiveTypes_17, {
                Equals: equals,
                GetHashCode: (x_18) => (structuralHash(x_18) | 0),
            })) {
                const arg_22 = int32ToString(1);
                const arg_1_17 = int32ToString(actual_17);
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("one element");
            }
            else {
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_17)("one element");
            }
            throw new Exception(errorMsg_17);
        }
        const actual_18 = item(0, r_3);
        if ((actual_18 === "hello") ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, "hello", "value");
        }
        else {
            let valueType_18;
            let copyOfStruct_18 = actual_18;
            valueType_18 = string_type;
            const primitiveTypes_18 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            const errorMsg_18 = contains(valueType_18, primitiveTypes_18, {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            }) ? toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))("hello")(actual_18)("value") : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))("hello")(actual_18)("value");
            throw new Exception(errorMsg_18);
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.thisNext yields looped pairs", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const r_4 = [1, 2, 3];
        const pairs = toList(ResizeArr_thisNext(r_4));
        const actual_19 = length(pairs) | 0;
        if ((actual_19 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, 3, "3 pairs");
        }
        else {
            let valueType_19;
            let copyOfStruct_19 = actual_19;
            valueType_19 = int32_type;
            const primitiveTypes_19 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_19;
            if (contains(valueType_19, primitiveTypes_19, {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            })) {
                const arg_24 = int32ToString(3);
                const arg_1_19 = int32ToString(actual_19);
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("3 pairs");
            }
            else {
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_19)("3 pairs");
            }
            throw new Exception(errorMsg_19);
        }
        const actual_20 = item_1(0, pairs);
        const expected_20 = [1, 2];
        if (equalArrays(actual_20, expected_20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, expected_20, "first pair");
        }
        else {
            let valueType_20;
            let copyOfStruct_20 = actual_20;
            valueType_20 = tuple_type(int32_type, int32_type);
            const primitiveTypes_20 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_20;
            if (contains(valueType_20, primitiveTypes_20, {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            })) {
                const arg_25 = toString(expected_20);
                const arg_1_20 = toString(actual_20);
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("first pair");
            }
            else {
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_20)(actual_20)("first pair");
            }
            throw new Exception(errorMsg_20);
        }
        const actual_21 = item_1(1, pairs);
        const expected_21 = [2, 3];
        if (equalArrays(actual_21, expected_21) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, expected_21, "second pair");
        }
        else {
            let valueType_21;
            let copyOfStruct_21 = actual_21;
            valueType_21 = tuple_type(int32_type, int32_type);
            const primitiveTypes_21 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_21;
            if (contains(valueType_21, primitiveTypes_21, {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            })) {
                const arg_26 = toString(expected_21);
                const arg_1_21 = toString(actual_21);
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("second pair");
            }
            else {
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_21)(actual_21)("second pair");
            }
            throw new Exception(errorMsg_21);
        }
        const actual_22 = item_1(2, pairs);
        const expected_22 = [3, 1];
        if (equalArrays(actual_22, expected_22) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_22, expected_22, "wrapping pair");
        }
        else {
            let valueType_22;
            let copyOfStruct_22 = actual_22;
            valueType_22 = tuple_type(int32_type, int32_type);
            const primitiveTypes_22 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_22;
            if (contains(valueType_22, primitiveTypes_22, {
                Equals: equals,
                GetHashCode: (x_23) => (structuralHash(x_23) | 0),
            })) {
                const arg_27 = toString(expected_22);
                const arg_1_22 = toString(actual_22);
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("wrapping pair");
            }
            else {
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_22)(actual_22)("wrapping pair");
            }
            throw new Exception(errorMsg_22);
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.thisNext input unchanged", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const r_5 = [10, 20, 30];
        const originalCount = r_5.length | 0;
        toList(ResizeArr_thisNext(r_5));
        const actual_23 = r_5.length | 0;
        const expected_23 = originalCount | 0;
        if ((actual_23 === expected_23) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, expected_23, "count unchanged");
        }
        else {
            let valueType_23;
            let copyOfStruct_23 = actual_23;
            valueType_23 = int32_type;
            const primitiveTypes_23 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_23;
            if (contains(valueType_23, primitiveTypes_23, {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            })) {
                const arg_28 = int32ToString(expected_23);
                const arg_1_23 = int32ToString(actual_23);
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("count unchanged");
            }
            else {
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_23)(actual_23)("count unchanged");
            }
            throw new Exception(errorMsg_23);
        }
        const actual_24 = item(0, r_5) | 0;
        if ((actual_24 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, 10, "first unchanged");
        }
        else {
            let valueType_24;
            let copyOfStruct_24 = actual_24;
            valueType_24 = int32_type;
            const primitiveTypes_24 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_24;
            if (contains(valueType_24, primitiveTypes_24, {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            })) {
                const arg_29 = int32ToString(10);
                const arg_1_24 = int32ToString(actual_24);
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("first unchanged");
            }
            else {
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_24)("first unchanged");
            }
            throw new Exception(errorMsg_24);
        }
        const actual_25 = item(1, r_5) | 0;
        if ((actual_25 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, 20, "second unchanged");
        }
        else {
            let valueType_25;
            let copyOfStruct_25 = actual_25;
            valueType_25 = int32_type;
            const primitiveTypes_25 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_25;
            if (contains(valueType_25, primitiveTypes_25, {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            })) {
                const arg_30 = int32ToString(20);
                const arg_1_25 = int32ToString(actual_25);
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("second unchanged");
            }
            else {
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_25)("second unchanged");
            }
            throw new Exception(errorMsg_25);
        }
        const actual_26 = item(2, r_5) | 0;
        if ((actual_26 === 30) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, 30, "third unchanged");
        }
        else {
            let valueType_26;
            let copyOfStruct_26 = actual_26;
            valueType_26 = int32_type;
            const primitiveTypes_26 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_26;
            if (contains(valueType_26, primitiveTypes_26, {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            })) {
                const arg_31 = int32ToString(30);
                const arg_1_26 = int32ToString(actual_26);
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("third unchanged");
            }
            else {
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(30)(actual_26)("third unchanged");
            }
            throw new Exception(errorMsg_26);
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.thisNext fails on less than 3 items", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const r_6 = [1, 2];
        Expect_throws(() => {
            ResizeArr_thisNext(r_6);
        }, "needs 3+ items");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.iPrevThisNext yields looped tuples", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const r_7 = [1, 2, 3, 4];
        const tuples = toList(ResizeArr_iPrevThisNext(r_7));
        const actual_27 = length(tuples) | 0;
        if ((actual_27 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, 4, "4 tuples");
        }
        else {
            let valueType_27;
            let copyOfStruct_27 = actual_27;
            valueType_27 = int32_type;
            const primitiveTypes_27 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_27;
            if (contains(valueType_27, primitiveTypes_27, {
                Equals: equals,
                GetHashCode: (x_28) => (structuralHash(x_28) | 0),
            })) {
                const arg_32 = int32ToString(4);
                const arg_1_27 = int32ToString(actual_27);
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("4 tuples");
            }
            else {
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_27)("4 tuples");
            }
            throw new Exception(errorMsg_27);
        }
        const actual_28 = item_1(0, tuples);
        const expected_28 = [0, 4, 1, 2];
        if (equalArrays(actual_28, expected_28) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, expected_28, "first tuple with wrap");
        }
        else {
            let valueType_28;
            let copyOfStruct_28 = actual_28;
            valueType_28 = tuple_type(int32_type, int32_type, int32_type, int32_type);
            const primitiveTypes_28 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_28;
            if (contains(valueType_28, primitiveTypes_28, {
                Equals: equals,
                GetHashCode: (x_29) => (structuralHash(x_29) | 0),
            })) {
                const arg_33 = toString(expected_28);
                const arg_1_28 = toString(actual_28);
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("first tuple with wrap");
            }
            else {
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_28)(actual_28)("first tuple with wrap");
            }
            throw new Exception(errorMsg_28);
        }
        const actual_29 = item_1(1, tuples);
        const expected_29 = [1, 1, 2, 3];
        if (equalArrays(actual_29, expected_29) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, expected_29, "second tuple");
        }
        else {
            let valueType_29;
            let copyOfStruct_29 = actual_29;
            valueType_29 = tuple_type(int32_type, int32_type, int32_type, int32_type);
            const primitiveTypes_29 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_29;
            if (contains(valueType_29, primitiveTypes_29, {
                Equals: equals,
                GetHashCode: (x_30) => (structuralHash(x_30) | 0),
            })) {
                const arg_34 = toString(expected_29);
                const arg_1_29 = toString(actual_29);
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)("second tuple");
            }
            else {
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_29)(actual_29)("second tuple");
            }
            throw new Exception(errorMsg_29);
        }
        const actual_30 = item_1(2, tuples);
        const expected_30 = [2, 2, 3, 4];
        if (equalArrays(actual_30, expected_30) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, expected_30, "third tuple");
        }
        else {
            let valueType_30;
            let copyOfStruct_30 = actual_30;
            valueType_30 = tuple_type(int32_type, int32_type, int32_type, int32_type);
            const primitiveTypes_30 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_30;
            if (contains(valueType_30, primitiveTypes_30, {
                Equals: equals,
                GetHashCode: (x_31) => (structuralHash(x_31) | 0),
            })) {
                const arg_35 = toString(expected_30);
                const arg_1_30 = toString(actual_30);
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("third tuple");
            }
            else {
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_30)(actual_30)("third tuple");
            }
            throw new Exception(errorMsg_30);
        }
        const actual_31 = item_1(3, tuples);
        const expected_31 = [3, 3, 4, 1];
        if (equalArrays(actual_31, expected_31) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, expected_31, "last tuple with wrap");
        }
        else {
            let valueType_31;
            let copyOfStruct_31 = actual_31;
            valueType_31 = tuple_type(int32_type, int32_type, int32_type, int32_type);
            const primitiveTypes_31 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_31;
            if (contains(valueType_31, primitiveTypes_31, {
                Equals: equals,
                GetHashCode: (x_32) => (structuralHash(x_32) | 0),
            })) {
                const arg_36 = toString(expected_31);
                const arg_1_31 = toString(actual_31);
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("last tuple with wrap");
            }
            else {
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_31)(actual_31)("last tuple with wrap");
            }
            throw new Exception(errorMsg_31);
        }
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.iPrevThisNext input unchanged", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const r_8 = [1, 2, 3, 4];
        const originalValues = toArray(r_8);
        toList(ResizeArr_iPrevThisNext(r_8));
        const actual_32 = r_8.length | 0;
        if ((actual_32 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, 4, "count unchanged");
        }
        else {
            let valueType_32;
            let copyOfStruct_32 = actual_32;
            valueType_32 = int32_type;
            const primitiveTypes_32 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_32;
            if (contains(valueType_32, primitiveTypes_32, {
                Equals: equals,
                GetHashCode: (x_33) => (structuralHash(x_33) | 0),
            })) {
                const arg_37 = int32ToString(4);
                const arg_1_32 = int32ToString(actual_32);
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_32)("count unchanged");
            }
            else {
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_32)("count unchanged");
            }
            throw new Exception(errorMsg_32);
        }
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_20, rangeDouble(0, 1, 3), (_arg) => {
            const i_2 = _arg | 0;
            const actual_33 = item(i_2, r_8) | 0;
            const expected_33 = item(i_2, originalValues) | 0;
            const msg_33 = `element ${i_2} unchanged`;
            if ((actual_33 === expected_33) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_33, expected_33, msg_33);
            }
            else {
                let valueType_33;
                let copyOfStruct_33 = actual_33;
                valueType_33 = int32_type;
                const primitiveTypes_33 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_33;
                if (contains(valueType_33, primitiveTypes_33, {
                    Equals: equals,
                    GetHashCode: (x_34) => (structuralHash(x_34) | 0),
                })) {
                    const arg_38 = int32ToString(expected_33);
                    const arg_1_33 = int32ToString(actual_33);
                    errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_38)(arg_1_33)(msg_33);
                }
                else {
                    errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_33)(actual_33)(msg_33);
                }
                throw new Exception(errorMsg_33);
            }
            Test_TestCaseBuilder__Zero(builder$0040_20);
        });
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.iPrevThisNext fails on less than 4 items", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const r_9 = [1, 2, 3];
        Expect_throws(() => {
            ResizeArr_iPrevThisNext(r_9);
        }, "needs 4+ items");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.sortBy returns new sorted array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const r_10 = [3, 1, 4, 1, 5];
        const sorted = ResizeArr_sortBy((x_35) => (x_35 | 0), r_10);
        const actual_34 = sorted.length | 0;
        if ((actual_34 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, 5, "same count");
        }
        else {
            let valueType_34;
            let copyOfStruct_34 = actual_34;
            valueType_34 = int32_type;
            const primitiveTypes_34 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_34;
            if (contains(valueType_34, primitiveTypes_34, {
                Equals: equals,
                GetHashCode: (x_36) => (structuralHash(x_36) | 0),
            })) {
                const arg_39 = int32ToString(5);
                const arg_1_34 = int32ToString(actual_34);
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_39)(arg_1_34)("same count");
            }
            else {
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_34)("same count");
            }
            throw new Exception(errorMsg_34);
        }
        const actual_35 = item(0, sorted) | 0;
        if ((actual_35 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, 1, "sorted first");
        }
        else {
            let valueType_35;
            let copyOfStruct_35 = actual_35;
            valueType_35 = int32_type;
            const primitiveTypes_35 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_35;
            if (contains(valueType_35, primitiveTypes_35, {
                Equals: equals,
                GetHashCode: (x_37) => (structuralHash(x_37) | 0),
            })) {
                const arg_40 = int32ToString(1);
                const arg_1_35 = int32ToString(actual_35);
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_40)(arg_1_35)("sorted first");
            }
            else {
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_35)("sorted first");
            }
            throw new Exception(errorMsg_35);
        }
        const actual_36 = item(1, sorted) | 0;
        if ((actual_36 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, 1, "sorted second");
        }
        else {
            let valueType_36;
            let copyOfStruct_36 = actual_36;
            valueType_36 = int32_type;
            const primitiveTypes_36 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_36;
            if (contains(valueType_36, primitiveTypes_36, {
                Equals: equals,
                GetHashCode: (x_38) => (structuralHash(x_38) | 0),
            })) {
                const arg_41 = int32ToString(1);
                const arg_1_36 = int32ToString(actual_36);
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_41)(arg_1_36)("sorted second");
            }
            else {
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_36)("sorted second");
            }
            throw new Exception(errorMsg_36);
        }
        const actual_37 = item(2, sorted) | 0;
        if ((actual_37 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, 3, "sorted third");
        }
        else {
            let valueType_37;
            let copyOfStruct_37 = actual_37;
            valueType_37 = int32_type;
            const primitiveTypes_37 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_37;
            if (contains(valueType_37, primitiveTypes_37, {
                Equals: equals,
                GetHashCode: (x_39) => (structuralHash(x_39) | 0),
            })) {
                const arg_42 = int32ToString(3);
                const arg_1_37 = int32ToString(actual_37);
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_42)(arg_1_37)("sorted third");
            }
            else {
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_37)("sorted third");
            }
            throw new Exception(errorMsg_37);
        }
        const actual_38 = item(3, sorted) | 0;
        if ((actual_38 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, 4, "sorted fourth");
        }
        else {
            let valueType_38;
            let copyOfStruct_38 = actual_38;
            valueType_38 = int32_type;
            const primitiveTypes_38 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_38;
            if (contains(valueType_38, primitiveTypes_38, {
                Equals: equals,
                GetHashCode: (x_40) => (structuralHash(x_40) | 0),
            })) {
                const arg_43 = int32ToString(4);
                const arg_1_38 = int32ToString(actual_38);
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_43)(arg_1_38)("sorted fourth");
            }
            else {
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_38)("sorted fourth");
            }
            throw new Exception(errorMsg_38);
        }
        const actual_39 = item(4, sorted) | 0;
        if ((actual_39 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_39, 5, "sorted fifth");
        }
        else {
            let valueType_39;
            let copyOfStruct_39 = actual_39;
            valueType_39 = int32_type;
            const primitiveTypes_39 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_39;
            if (contains(valueType_39, primitiveTypes_39, {
                Equals: equals,
                GetHashCode: (x_41) => (structuralHash(x_41) | 0),
            })) {
                const arg_44 = int32ToString(5);
                const arg_1_39 = int32ToString(actual_39);
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_44)(arg_1_39)("sorted fifth");
            }
            else {
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_39)("sorted fifth");
            }
            throw new Exception(errorMsg_39);
        }
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.sortBy input unchanged", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const r_11 = [3, 1, 4, 1, 5];
        const originalValues_1 = toArray(r_11);
        ResizeArr_sortBy((x_42) => (x_42 | 0), r_11);
        const actual_40 = r_11.length | 0;
        if ((actual_40 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_40, 5, "count unchanged");
        }
        else {
            let valueType_40;
            let copyOfStruct_40 = actual_40;
            valueType_40 = int32_type;
            const primitiveTypes_40 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_40;
            if (contains(valueType_40, primitiveTypes_40, {
                Equals: equals,
                GetHashCode: (x_43) => (structuralHash(x_43) | 0),
            })) {
                const arg_45 = int32ToString(5);
                const arg_1_40 = int32ToString(actual_40);
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_45)(arg_1_40)("count unchanged");
            }
            else {
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_40)("count unchanged");
            }
            throw new Exception(errorMsg_40);
        }
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_23, rangeDouble(0, 1, 4), (_arg_1) => {
            const i_3 = _arg_1 | 0;
            const actual_41 = item(i_3, r_11) | 0;
            const expected_41 = item(i_3, originalValues_1) | 0;
            const msg_41 = `element ${i_3} unchanged`;
            if ((actual_41 === expected_41) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_41, expected_41, msg_41);
            }
            else {
                let valueType_41;
                let copyOfStruct_41 = actual_41;
                valueType_41 = int32_type;
                const primitiveTypes_41 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_41;
                if (contains(valueType_41, primitiveTypes_41, {
                    Equals: equals,
                    GetHashCode: (x_44) => (structuralHash(x_44) | 0),
                })) {
                    const arg_46 = int32ToString(expected_41);
                    const arg_1_41 = int32ToString(actual_41);
                    errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_46)(arg_1_41)(msg_41);
                }
                else {
                    errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_41)(actual_41)(msg_41);
                }
                throw new Exception(errorMsg_41);
            }
            Test_TestCaseBuilder__Zero(builder$0040_23);
        });
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.sortBy with projection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        const r_12 = ["ccc", "a", "bb"];
        const sorted_1 = ResizeArr_sortBy((str) => (str.length | 0), r_12);
        const actual_42 = item(0, sorted_1);
        if ((actual_42 === "a") ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_42, "a", "shortest first");
        }
        else {
            let valueType_42;
            let copyOfStruct_42 = actual_42;
            valueType_42 = string_type;
            const primitiveTypes_42 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            const errorMsg_42 = contains(valueType_42, primitiveTypes_42, {
                Equals: equals,
                GetHashCode: (x_45) => (structuralHash(x_45) | 0),
            }) ? toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))("a")(actual_42)("shortest first") : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))("a")(actual_42)("shortest first");
            throw new Exception(errorMsg_42);
        }
        const actual_43 = item(1, sorted_1);
        if ((actual_43 === "bb") ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_43, "bb", "medium second");
        }
        else {
            let valueType_43;
            let copyOfStruct_43 = actual_43;
            valueType_43 = string_type;
            const primitiveTypes_43 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            const errorMsg_43 = contains(valueType_43, primitiveTypes_43, {
                Equals: equals,
                GetHashCode: (x_46) => (structuralHash(x_46) | 0),
            }) ? toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))("bb")(actual_43)("medium second") : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))("bb")(actual_43)("medium second");
            throw new Exception(errorMsg_43);
        }
        const actual_44 = item(2, sorted_1);
        if ((actual_44 === "ccc") ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_44, "ccc", "longest last");
        }
        else {
            let valueType_44;
            let copyOfStruct_44 = actual_44;
            valueType_44 = string_type;
            const primitiveTypes_44 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            const errorMsg_44 = contains(valueType_44, primitiveTypes_44, {
                Equals: equals,
                GetHashCode: (x_47) => (structuralHash(x_47) | 0),
            }) ? toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))("ccc")(actual_44)("longest last") : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))("ccc")(actual_44)("longest last");
            throw new Exception(errorMsg_44);
        }
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.sortBy empty array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        const r_13 = [];
        const sorted_2 = ResizeArr_sortBy((x_48) => (x_48 | 0), r_13);
        const actual_45 = sorted_2.length | 0;
        if ((actual_45 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_45, 0, "empty remains empty");
        }
        else {
            let valueType_45;
            let copyOfStruct_45 = actual_45;
            valueType_45 = int32_type;
            const primitiveTypes_45 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_45;
            if (contains(valueType_45, primitiveTypes_45, {
                Equals: equals,
                GetHashCode: (x_49) => (structuralHash(x_49) | 0),
            })) {
                const arg_50 = int32ToString(0);
                const arg_1_45 = int32ToString(actual_45);
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_50)(arg_1_45)("empty remains empty");
            }
            else {
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_45)("empty remains empty");
            }
            throw new Exception(errorMsg_45);
        }
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.sortBy single element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        const r_14 = [42];
        const sorted_3 = ResizeArr_sortBy((x_50) => (x_50 | 0), r_14);
        const actual_46 = sorted_3.length | 0;
        if ((actual_46 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_46, 1, "single element");
        }
        else {
            let valueType_46;
            let copyOfStruct_46 = actual_46;
            valueType_46 = int32_type;
            const primitiveTypes_46 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_46;
            if (contains(valueType_46, primitiveTypes_46, {
                Equals: equals,
                GetHashCode: (x_51) => (structuralHash(x_51) | 0),
            })) {
                const arg_51 = int32ToString(1);
                const arg_1_46 = int32ToString(actual_46);
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_51)(arg_1_46)("single element");
            }
            else {
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_46)("single element");
            }
            throw new Exception(errorMsg_46);
        }
        const actual_47 = item(0, sorted_3) | 0;
        if ((actual_47 === 42) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_47, 42, "value preserved");
        }
        else {
            let valueType_47;
            let copyOfStruct_47 = actual_47;
            valueType_47 = int32_type;
            const primitiveTypes_47 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_47;
            if (contains(valueType_47, primitiveTypes_47, {
                Equals: equals,
                GetHashCode: (x_52) => (structuralHash(x_52) | 0),
            })) {
                const arg_52 = int32ToString(42);
                const arg_1_47 = int32ToString(actual_47);
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_52)(arg_1_47)("value preserved");
            }
            else {
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(42)(actual_47)("value preserved");
            }
            throw new Exception(errorMsg_47);
        }
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.tryFindIndex finds element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        const r_15 = [1, 2, 3, 4, 5];
        const idx = ResizeArr_tryFindIndex((x_53) => (x_53 === 3), r_15);
        const actual_48 = idx;
        const expected_48 = 2;
        if (equals_1(actual_48, expected_48) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_48, expected_48, "found at index 2");
        }
        else {
            let valueType_48;
            let copyOfStruct_48 = actual_48;
            valueType_48 = option_type(int32_type);
            const primitiveTypes_48 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_48;
            if (contains(valueType_48, primitiveTypes_48, {
                Equals: equals,
                GetHashCode: (x_54) => (structuralHash(x_54) | 0),
            })) {
                const arg_53 = toString(expected_48);
                const arg_1_48 = toString(actual_48);
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_53)(arg_1_48)("found at index 2");
            }
            else {
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_48)(actual_48)("found at index 2");
            }
            throw new Exception(errorMsg_48);
        }
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.tryFindIndex returns None when not found", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const r_16 = [1, 2, 3];
        const idx_1 = ResizeArr_tryFindIndex((x_55) => (x_55 === 99), r_16);
        const actual_49 = idx_1;
        const expected_49 = undefined;
        if (equals_1(actual_49, expected_49) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_49, expected_49, "not found");
        }
        else {
            let valueType_49;
            let copyOfStruct_49 = actual_49;
            valueType_49 = option_type(int32_type);
            const primitiveTypes_49 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_49;
            if (contains(valueType_49, primitiveTypes_49, {
                Equals: equals,
                GetHashCode: (x_56) => (structuralHash(x_56) | 0),
            })) {
                const arg_54 = toString(expected_49);
                const arg_1_49 = toString(actual_49);
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_54)(arg_1_49)("not found");
            }
            else {
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_49)(actual_49)("not found");
            }
            throw new Exception(errorMsg_49);
        }
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.tryFindIndex empty array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const r_17 = [];
        const idx_2 = ResizeArr_tryFindIndex((_arg_2) => true, r_17);
        const actual_50 = idx_2;
        const expected_50 = undefined;
        if (equals_1(actual_50, expected_50) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_50, expected_50, "empty returns None");
        }
        else {
            let valueType_50;
            let copyOfStruct_50 = actual_50;
            valueType_50 = option_type(int32_type);
            const primitiveTypes_50 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_50;
            if (contains(valueType_50, primitiveTypes_50, {
                Equals: equals,
                GetHashCode: (x_57) => (structuralHash(x_57) | 0),
            })) {
                const arg_55 = toString(expected_50);
                const arg_1_50 = toString(actual_50);
                errorMsg_50 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_55)(arg_1_50)("empty returns None");
            }
            else {
                errorMsg_50 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_50)(actual_50)("empty returns None");
            }
            throw new Exception(errorMsg_50);
        }
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.tryFindIndex returns first match", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        const r_18 = [1, 2, 2, 3];
        const idx_3 = ResizeArr_tryFindIndex((x_58) => (x_58 === 2), r_18);
        const actual_51 = idx_3;
        const expected_51 = 1;
        if (equals_1(actual_51, expected_51) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_51, expected_51, "first occurrence");
        }
        else {
            let valueType_51;
            let copyOfStruct_51 = actual_51;
            valueType_51 = option_type(int32_type);
            const primitiveTypes_51 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_51;
            if (contains(valueType_51, primitiveTypes_51, {
                Equals: equals,
                GetHashCode: (x_59) => (structuralHash(x_59) | 0),
            })) {
                const arg_56 = toString(expected_51);
                const arg_1_51 = toString(actual_51);
                errorMsg_51 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_56)(arg_1_51)("first occurrence");
            }
            else {
                errorMsg_51 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_51)(actual_51)("first occurrence");
            }
            throw new Exception(errorMsg_51);
        }
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.findIndex finds element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        const r_19 = [1, 2, 3, 4, 5];
        const idx_4 = ResizeArr_findIndex((x_60) => (x_60 === 4), r_19) | 0;
        const actual_52 = idx_4 | 0;
        if ((actual_52 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_52, 3, "found at index 3");
        }
        else {
            let valueType_52;
            let copyOfStruct_52 = actual_52;
            valueType_52 = int32_type;
            const primitiveTypes_52 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_52;
            if (contains(valueType_52, primitiveTypes_52, {
                Equals: equals,
                GetHashCode: (x_61) => (structuralHash(x_61) | 0),
            })) {
                const arg_57 = int32ToString(3);
                const arg_1_52 = int32ToString(actual_52);
                errorMsg_52 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_57)(arg_1_52)("found at index 3");
            }
            else {
                errorMsg_52 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_52)("found at index 3");
            }
            throw new Exception(errorMsg_52);
        }
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.findIndex returns -1 when not found", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        const r_20 = [1, 2, 3];
        const idx_5 = ResizeArr_findIndex((x_62) => (x_62 === 99), r_20) | 0;
        const actual_53 = idx_5 | 0;
        if ((actual_53 === -1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_53, -1, "not found");
        }
        else {
            let valueType_53;
            let copyOfStruct_53 = actual_53;
            valueType_53 = int32_type;
            const primitiveTypes_53 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_53;
            if (contains(valueType_53, primitiveTypes_53, {
                Equals: equals,
                GetHashCode: (x_63) => (structuralHash(x_63) | 0),
            })) {
                const arg_58 = int32ToString(-1);
                const arg_1_53 = int32ToString(actual_53);
                errorMsg_53 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_58)(arg_1_53)("not found");
            }
            else {
                errorMsg_53 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(-1)(actual_53)("not found");
            }
            throw new Exception(errorMsg_53);
        }
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.find finds element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        const r_21 = [1, 2, 3, 4, 5];
        const el = ResizeArr_find((x_64) => (x_64 > 3), r_21) | 0;
        const actual_54 = el | 0;
        if ((actual_54 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_54, 4, "first element > 3");
        }
        else {
            let valueType_54;
            let copyOfStruct_54 = actual_54;
            valueType_54 = int32_type;
            const primitiveTypes_54 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_54;
            if (contains(valueType_54, primitiveTypes_54, {
                Equals: equals,
                GetHashCode: (x_65) => (structuralHash(x_65) | 0),
            })) {
                const arg_59 = int32ToString(4);
                const arg_1_54 = int32ToString(actual_54);
                errorMsg_54 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_59)(arg_1_54)("first element > 3");
            }
            else {
                errorMsg_54 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_54)("first element > 3");
            }
            throw new Exception(errorMsg_54);
        }
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.findLastIndex finds last match", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        const r_22 = [1, 2, 3, 2, 1];
        const idx_6 = ResizeArr_findLastIndex((x_66) => (x_66 === 2), r_22) | 0;
        const actual_55 = idx_6 | 0;
        if ((actual_55 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_55, 3, "last occurrence at index 3");
        }
        else {
            let valueType_55;
            let copyOfStruct_55 = actual_55;
            valueType_55 = int32_type;
            const primitiveTypes_55 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_55;
            if (contains(valueType_55, primitiveTypes_55, {
                Equals: equals,
                GetHashCode: (x_67) => (structuralHash(x_67) | 0),
            })) {
                const arg_60 = int32ToString(3);
                const arg_1_55 = int32ToString(actual_55);
                errorMsg_55 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_60)(arg_1_55)("last occurrence at index 3");
            }
            else {
                errorMsg_55 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_55)("last occurrence at index 3");
            }
            throw new Exception(errorMsg_55);
        }
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.findLastIndex returns -1 when not found", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        const r_23 = [1, 2, 3];
        const idx_7 = ResizeArr_findLastIndex((x_68) => (x_68 === 99), r_23) | 0;
        const actual_56 = idx_7 | 0;
        if ((actual_56 === -1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_56, -1, "not found");
        }
        else {
            let valueType_56;
            let copyOfStruct_56 = actual_56;
            valueType_56 = int32_type;
            const primitiveTypes_56 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_56;
            if (contains(valueType_56, primitiveTypes_56, {
                Equals: equals,
                GetHashCode: (x_69) => (structuralHash(x_69) | 0),
            })) {
                const arg_61 = int32ToString(-1);
                const arg_1_56 = int32ToString(actual_56);
                errorMsg_56 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_61)(arg_1_56)("not found");
            }
            else {
                errorMsg_56 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(-1)(actual_56)("not found");
            }
            throw new Exception(errorMsg_56);
        }
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.findLastIndex empty array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        const r_24 = [];
        const idx_8 = ResizeArr_findLastIndex((_arg_3) => true, r_24) | 0;
        const actual_57 = idx_8 | 0;
        if ((actual_57 === -1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_57, -1, "empty returns -1");
        }
        else {
            let valueType_57;
            let copyOfStruct_57 = actual_57;
            valueType_57 = int32_type;
            const primitiveTypes_57 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_57;
            if (contains(valueType_57, primitiveTypes_57, {
                Equals: equals,
                GetHashCode: (x_70) => (structuralHash(x_70) | 0),
            })) {
                const arg_62 = int32ToString(-1);
                const arg_1_57 = int32ToString(actual_57);
                errorMsg_57 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_62)(arg_1_57)("empty returns -1");
            }
            else {
                errorMsg_57 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(-1)(actual_57)("empty returns -1");
            }
            throw new Exception(errorMsg_57);
        }
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.rev returns reversed array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        const r_25 = [1, 2, 3, 4, 5];
        const reversed = ResizeArr_rev(r_25);
        const actual_58 = reversed.length | 0;
        if ((actual_58 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_58, 5, "same count");
        }
        else {
            let valueType_58;
            let copyOfStruct_58 = actual_58;
            valueType_58 = int32_type;
            const primitiveTypes_58 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_58;
            if (contains(valueType_58, primitiveTypes_58, {
                Equals: equals,
                GetHashCode: (x_71) => (structuralHash(x_71) | 0),
            })) {
                const arg_63 = int32ToString(5);
                const arg_1_58 = int32ToString(actual_58);
                errorMsg_58 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_63)(arg_1_58)("same count");
            }
            else {
                errorMsg_58 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_58)("same count");
            }
            throw new Exception(errorMsg_58);
        }
        const actual_59 = item(0, reversed) | 0;
        if ((actual_59 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_59, 5, "first is last");
        }
        else {
            let valueType_59;
            let copyOfStruct_59 = actual_59;
            valueType_59 = int32_type;
            const primitiveTypes_59 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_59;
            if (contains(valueType_59, primitiveTypes_59, {
                Equals: equals,
                GetHashCode: (x_72) => (structuralHash(x_72) | 0),
            })) {
                const arg_64 = int32ToString(5);
                const arg_1_59 = int32ToString(actual_59);
                errorMsg_59 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_64)(arg_1_59)("first is last");
            }
            else {
                errorMsg_59 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_59)("first is last");
            }
            throw new Exception(errorMsg_59);
        }
        const actual_60 = item(1, reversed) | 0;
        if ((actual_60 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_60, 4, "second");
        }
        else {
            let valueType_60;
            let copyOfStruct_60 = actual_60;
            valueType_60 = int32_type;
            const primitiveTypes_60 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_60;
            if (contains(valueType_60, primitiveTypes_60, {
                Equals: equals,
                GetHashCode: (x_73) => (structuralHash(x_73) | 0),
            })) {
                const arg_65 = int32ToString(4);
                const arg_1_60 = int32ToString(actual_60);
                errorMsg_60 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_65)(arg_1_60)("second");
            }
            else {
                errorMsg_60 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_60)("second");
            }
            throw new Exception(errorMsg_60);
        }
        const actual_61 = item(2, reversed) | 0;
        if ((actual_61 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_61, 3, "third");
        }
        else {
            let valueType_61;
            let copyOfStruct_61 = actual_61;
            valueType_61 = int32_type;
            const primitiveTypes_61 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_61;
            if (contains(valueType_61, primitiveTypes_61, {
                Equals: equals,
                GetHashCode: (x_74) => (structuralHash(x_74) | 0),
            })) {
                const arg_66 = int32ToString(3);
                const arg_1_61 = int32ToString(actual_61);
                errorMsg_61 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_66)(arg_1_61)("third");
            }
            else {
                errorMsg_61 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_61)("third");
            }
            throw new Exception(errorMsg_61);
        }
        const actual_62 = item(3, reversed) | 0;
        if ((actual_62 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_62, 2, "fourth");
        }
        else {
            let valueType_62;
            let copyOfStruct_62 = actual_62;
            valueType_62 = int32_type;
            const primitiveTypes_62 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_62;
            if (contains(valueType_62, primitiveTypes_62, {
                Equals: equals,
                GetHashCode: (x_75) => (structuralHash(x_75) | 0),
            })) {
                const arg_67 = int32ToString(2);
                const arg_1_62 = int32ToString(actual_62);
                errorMsg_62 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_67)(arg_1_62)("fourth");
            }
            else {
                errorMsg_62 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_62)("fourth");
            }
            throw new Exception(errorMsg_62);
        }
        const actual_63 = item(4, reversed) | 0;
        if ((actual_63 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_63, 1, "fifth is first");
        }
        else {
            let valueType_63;
            let copyOfStruct_63 = actual_63;
            valueType_63 = int32_type;
            const primitiveTypes_63 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_63;
            if (contains(valueType_63, primitiveTypes_63, {
                Equals: equals,
                GetHashCode: (x_76) => (structuralHash(x_76) | 0),
            })) {
                const arg_68 = int32ToString(1);
                const arg_1_63 = int32ToString(actual_63);
                errorMsg_63 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_68)(arg_1_63)("fifth is first");
            }
            else {
                errorMsg_63 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_63)("fifth is first");
            }
            throw new Exception(errorMsg_63);
        }
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.rev input unchanged", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        const r_26 = [1, 2, 3, 4, 5];
        const originalValues_2 = toArray(r_26);
        ResizeArr_rev(r_26);
        const actual_64 = r_26.length | 0;
        if ((actual_64 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_64, 5, "count unchanged");
        }
        else {
            let valueType_64;
            let copyOfStruct_64 = actual_64;
            valueType_64 = int32_type;
            const primitiveTypes_64 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_64;
            if (contains(valueType_64, primitiveTypes_64, {
                Equals: equals,
                GetHashCode: (x_77) => (structuralHash(x_77) | 0),
            })) {
                const arg_69 = int32ToString(5);
                const arg_1_64 = int32ToString(actual_64);
                errorMsg_64 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_69)(arg_1_64)("count unchanged");
            }
            else {
                errorMsg_64 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_64)("count unchanged");
            }
            throw new Exception(errorMsg_64);
        }
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_38, rangeDouble(0, 1, 4), (_arg_4) => {
            const i_4 = _arg_4 | 0;
            const actual_65 = item(i_4, r_26) | 0;
            const expected_65 = item(i_4, originalValues_2) | 0;
            const msg_65 = `element ${i_4} unchanged`;
            if ((actual_65 === expected_65) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_65, expected_65, msg_65);
            }
            else {
                let valueType_65;
                let copyOfStruct_65 = actual_65;
                valueType_65 = int32_type;
                const primitiveTypes_65 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_65;
                if (contains(valueType_65, primitiveTypes_65, {
                    Equals: equals,
                    GetHashCode: (x_78) => (structuralHash(x_78) | 0),
                })) {
                    const arg_70 = int32ToString(expected_65);
                    const arg_1_65 = int32ToString(actual_65);
                    errorMsg_65 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_70)(arg_1_65)(msg_65);
                }
                else {
                    errorMsg_65 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_65)(actual_65)(msg_65);
                }
                throw new Exception(errorMsg_65);
            }
            Test_TestCaseBuilder__Zero(builder$0040_38);
        });
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.rev empty array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        const r_27 = [];
        const reversed_1 = ResizeArr_rev(r_27);
        const actual_66 = reversed_1.length | 0;
        if ((actual_66 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_66, 0, "empty");
        }
        else {
            let valueType_66;
            let copyOfStruct_66 = actual_66;
            valueType_66 = int32_type;
            const primitiveTypes_66 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_66;
            if (contains(valueType_66, primitiveTypes_66, {
                Equals: equals,
                GetHashCode: (x_79) => (structuralHash(x_79) | 0),
            })) {
                const arg_71 = int32ToString(0);
                const arg_1_66 = int32ToString(actual_66);
                errorMsg_66 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_71)(arg_1_66)("empty");
            }
            else {
                errorMsg_66 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_66)("empty");
            }
            throw new Exception(errorMsg_66);
        }
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.rev single element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        const r_28 = [42];
        const reversed_2 = ResizeArr_rev(r_28);
        const actual_67 = reversed_2.length | 0;
        if ((actual_67 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_67, 1, "one element");
        }
        else {
            let valueType_67;
            let copyOfStruct_67 = actual_67;
            valueType_67 = int32_type;
            const primitiveTypes_67 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_67;
            if (contains(valueType_67, primitiveTypes_67, {
                Equals: equals,
                GetHashCode: (x_80) => (structuralHash(x_80) | 0),
            })) {
                const arg_72 = int32ToString(1);
                const arg_1_67 = int32ToString(actual_67);
                errorMsg_67 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_72)(arg_1_67)("one element");
            }
            else {
                errorMsg_67 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_67)("one element");
            }
            throw new Exception(errorMsg_67);
        }
        const actual_68 = item(0, reversed_2) | 0;
        if ((actual_68 === 42) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_68, 42, "value preserved");
        }
        else {
            let valueType_68;
            let copyOfStruct_68 = actual_68;
            valueType_68 = int32_type;
            const primitiveTypes_68 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_68;
            if (contains(valueType_68, primitiveTypes_68, {
                Equals: equals,
                GetHashCode: (x_81) => (structuralHash(x_81) | 0),
            })) {
                const arg_73 = int32ToString(42);
                const arg_1_68 = int32ToString(actual_68);
                errorMsg_68 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_73)(arg_1_68)("value preserved");
            }
            else {
                errorMsg_68 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(42)(actual_68)("value preserved");
            }
            throw new Exception(errorMsg_68);
        }
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.minIndexBy returns index of min by projection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        const r_29 = ["aaa", "b", "cc"];
        const idx_9 = ResizeArr_minIndexBy((str_1) => (str_1.length | 0), r_29) | 0;
        const actual_69 = idx_9 | 0;
        if ((actual_69 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_69, 1, "shortest string at index 1");
        }
        else {
            let valueType_69;
            let copyOfStruct_69 = actual_69;
            valueType_69 = int32_type;
            const primitiveTypes_69 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_69;
            if (contains(valueType_69, primitiveTypes_69, {
                Equals: equals,
                GetHashCode: (x_82) => (structuralHash(x_82) | 0),
            })) {
                const arg_74 = int32ToString(1);
                const arg_1_69 = int32ToString(actual_69);
                errorMsg_69 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_74)(arg_1_69)("shortest string at index 1");
            }
            else {
                errorMsg_69 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_69)("shortest string at index 1");
            }
            throw new Exception(errorMsg_69);
        }
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.minIndexBy single element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        const r_30 = [99];
        const idx_10 = ResizeArr_minIndexBy((x_83) => (x_83 | 0), r_30) | 0;
        const actual_70 = idx_10 | 0;
        if ((actual_70 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_70, 0, "single element");
        }
        else {
            let valueType_70;
            let copyOfStruct_70 = actual_70;
            valueType_70 = int32_type;
            const primitiveTypes_70 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_70;
            if (contains(valueType_70, primitiveTypes_70, {
                Equals: equals,
                GetHashCode: (x_84) => (structuralHash(x_84) | 0),
            })) {
                const arg_75 = int32ToString(0);
                const arg_1_70 = int32ToString(actual_70);
                errorMsg_70 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_75)(arg_1_70)("single element");
            }
            else {
                errorMsg_70 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_70)("single element");
            }
            throw new Exception(errorMsg_70);
        }
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.minIndexBy fails on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        const r_31 = [];
        Expect_throws(() => {
            ResizeArr_minIndexBy((x_85) => (x_85 | 0), r_31);
        }, "empty fails");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.minIndexBy duplicate minimums returns first", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        const r_32 = ["aa", "b", "c", "dd"];
        const idx_11 = ResizeArr_minIndexBy((str_2) => (str_2.length | 0), r_32) | 0;
        const actual_71 = idx_11 | 0;
        if ((actual_71 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_71, 1, "first min");
        }
        else {
            let valueType_71;
            let copyOfStruct_71 = actual_71;
            valueType_71 = int32_type;
            const primitiveTypes_71 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_71;
            if (contains(valueType_71, primitiveTypes_71, {
                Equals: equals,
                GetHashCode: (x_86) => (structuralHash(x_86) | 0),
            })) {
                const arg_76 = int32ToString(1);
                const arg_1_71 = int32ToString(actual_71);
                errorMsg_71 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_76)(arg_1_71)("first min");
            }
            else {
                errorMsg_71 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_71)("first min");
            }
            throw new Exception(errorMsg_71);
        }
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.maxIndexBy returns index of max by projection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        const r_33 = ["a", "bbb", "cc"];
        const idx_12 = ResizeArr_maxIndexBy((str_3) => (str_3.length | 0), r_33) | 0;
        const actual_72 = idx_12 | 0;
        if ((actual_72 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_72, 1, "longest string at index 1");
        }
        else {
            let valueType_72;
            let copyOfStruct_72 = actual_72;
            valueType_72 = int32_type;
            const primitiveTypes_72 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_72;
            if (contains(valueType_72, primitiveTypes_72, {
                Equals: equals,
                GetHashCode: (x_87) => (structuralHash(x_87) | 0),
            })) {
                const arg_77 = int32ToString(1);
                const arg_1_72 = int32ToString(actual_72);
                errorMsg_72 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_77)(arg_1_72)("longest string at index 1");
            }
            else {
                errorMsg_72 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_72)("longest string at index 1");
            }
            throw new Exception(errorMsg_72);
        }
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.maxIndexBy single element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        const r_34 = [99];
        const idx_13 = ResizeArr_maxIndexBy((x_88) => (x_88 | 0), r_34) | 0;
        const actual_73 = idx_13 | 0;
        if ((actual_73 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_73, 0, "single element");
        }
        else {
            let valueType_73;
            let copyOfStruct_73 = actual_73;
            valueType_73 = int32_type;
            const primitiveTypes_73 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_73;
            if (contains(valueType_73, primitiveTypes_73, {
                Equals: equals,
                GetHashCode: (x_89) => (structuralHash(x_89) | 0),
            })) {
                const arg_78 = int32ToString(0);
                const arg_1_73 = int32ToString(actual_73);
                errorMsg_73 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_78)(arg_1_73)("single element");
            }
            else {
                errorMsg_73 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_73)("single element");
            }
            throw new Exception(errorMsg_73);
        }
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.maxIndexBy fails on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        const r_35 = [];
        Expect_throws(() => {
            ResizeArr_maxIndexBy((x_90) => (x_90 | 0), r_35);
        }, "empty fails");
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.maxIndexBy duplicate maximums returns first", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        const r_36 = ["a", "bb", "cc", "d"];
        const idx_14 = ResizeArr_maxIndexBy((str_4) => (str_4.length | 0), r_36) | 0;
        const actual_74 = idx_14 | 0;
        if ((actual_74 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_74, 1, "first max");
        }
        else {
            let valueType_74;
            let copyOfStruct_74 = actual_74;
            valueType_74 = int32_type;
            const primitiveTypes_74 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_74;
            if (contains(valueType_74, primitiveTypes_74, {
                Equals: equals,
                GetHashCode: (x_91) => (structuralHash(x_91) | 0),
            })) {
                const arg_79 = int32ToString(1);
                const arg_1_74 = int32ToString(actual_74);
                errorMsg_74 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_79)(arg_1_74)("first max");
            }
            else {
                errorMsg_74 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_74)("first max");
            }
            throw new Exception(errorMsg_74);
        }
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.map transforms elements", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        const r_37 = [1, 2, 3];
        const mapped = ResizeArr_map((x_92) => ((x_92 * 10) | 0), r_37);
        const actual_75 = mapped.length | 0;
        if ((actual_75 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_75, 3, "same count");
        }
        else {
            let valueType_75;
            let copyOfStruct_75 = actual_75;
            valueType_75 = int32_type;
            const primitiveTypes_75 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_75;
            if (contains(valueType_75, primitiveTypes_75, {
                Equals: equals,
                GetHashCode: (x_93) => (structuralHash(x_93) | 0),
            })) {
                const arg_80 = int32ToString(3);
                const arg_1_75 = int32ToString(actual_75);
                errorMsg_75 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_80)(arg_1_75)("same count");
            }
            else {
                errorMsg_75 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_75)("same count");
            }
            throw new Exception(errorMsg_75);
        }
        const actual_76 = item(0, mapped) | 0;
        if ((actual_76 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_76, 10, "first");
        }
        else {
            let valueType_76;
            let copyOfStruct_76 = actual_76;
            valueType_76 = int32_type;
            const primitiveTypes_76 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_76;
            if (contains(valueType_76, primitiveTypes_76, {
                Equals: equals,
                GetHashCode: (x_94) => (structuralHash(x_94) | 0),
            })) {
                const arg_81 = int32ToString(10);
                const arg_1_76 = int32ToString(actual_76);
                errorMsg_76 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_81)(arg_1_76)("first");
            }
            else {
                errorMsg_76 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_76)("first");
            }
            throw new Exception(errorMsg_76);
        }
        const actual_77 = item(1, mapped) | 0;
        if ((actual_77 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_77, 20, "second");
        }
        else {
            let valueType_77;
            let copyOfStruct_77 = actual_77;
            valueType_77 = int32_type;
            const primitiveTypes_77 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_77;
            if (contains(valueType_77, primitiveTypes_77, {
                Equals: equals,
                GetHashCode: (x_95) => (structuralHash(x_95) | 0),
            })) {
                const arg_82 = int32ToString(20);
                const arg_1_77 = int32ToString(actual_77);
                errorMsg_77 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_82)(arg_1_77)("second");
            }
            else {
                errorMsg_77 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_77)("second");
            }
            throw new Exception(errorMsg_77);
        }
        const actual_78 = item(2, mapped) | 0;
        if ((actual_78 === 30) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_78, 30, "third");
        }
        else {
            let valueType_78;
            let copyOfStruct_78 = actual_78;
            valueType_78 = int32_type;
            const primitiveTypes_78 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_78;
            if (contains(valueType_78, primitiveTypes_78, {
                Equals: equals,
                GetHashCode: (x_96) => (structuralHash(x_96) | 0),
            })) {
                const arg_83 = int32ToString(30);
                const arg_1_78 = int32ToString(actual_78);
                errorMsg_78 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_83)(arg_1_78)("third");
            }
            else {
                errorMsg_78 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(30)(actual_78)("third");
            }
            throw new Exception(errorMsg_78);
        }
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.map input unchanged", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        const r_38 = [1, 2, 3];
        const originalValues_3 = toArray(r_38);
        ResizeArr_map((x_97) => ((x_97 * 10) | 0), r_38);
        const actual_79 = r_38.length | 0;
        if ((actual_79 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_79, 3, "count unchanged");
        }
        else {
            let valueType_79;
            let copyOfStruct_79 = actual_79;
            valueType_79 = int32_type;
            const primitiveTypes_79 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_79;
            if (contains(valueType_79, primitiveTypes_79, {
                Equals: equals,
                GetHashCode: (x_98) => (structuralHash(x_98) | 0),
            })) {
                const arg_84 = int32ToString(3);
                const arg_1_79 = int32ToString(actual_79);
                errorMsg_79 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_84)(arg_1_79)("count unchanged");
            }
            else {
                errorMsg_79 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_79)("count unchanged");
            }
            throw new Exception(errorMsg_79);
        }
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_50, rangeDouble(0, 1, 2), (_arg_5) => {
            const i_5 = _arg_5 | 0;
            const actual_80 = item(i_5, r_38) | 0;
            const expected_80 = item(i_5, originalValues_3) | 0;
            const msg_80 = `element ${i_5} unchanged`;
            if ((actual_80 === expected_80) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_80, expected_80, msg_80);
            }
            else {
                let valueType_80;
                let copyOfStruct_80 = actual_80;
                valueType_80 = int32_type;
                const primitiveTypes_80 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_80;
                if (contains(valueType_80, primitiveTypes_80, {
                    Equals: equals,
                    GetHashCode: (x_99) => (structuralHash(x_99) | 0),
                })) {
                    const arg_85 = int32ToString(expected_80);
                    const arg_1_80 = int32ToString(actual_80);
                    errorMsg_80 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_85)(arg_1_80)(msg_80);
                }
                else {
                    errorMsg_80 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_80)(actual_80)(msg_80);
                }
                throw new Exception(errorMsg_80);
            }
            Test_TestCaseBuilder__Zero(builder$0040_50);
        });
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.map empty array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        const r_39 = [];
        const mapped_1 = ResizeArr_map((x_100) => ((x_100 * 10) | 0), r_39);
        const actual_81 = mapped_1.length | 0;
        if ((actual_81 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_81, 0, "empty");
        }
        else {
            let valueType_81;
            let copyOfStruct_81 = actual_81;
            valueType_81 = int32_type;
            const primitiveTypes_81 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_81;
            if (contains(valueType_81, primitiveTypes_81, {
                Equals: equals,
                GetHashCode: (x_101) => (structuralHash(x_101) | 0),
            })) {
                const arg_86 = int32ToString(0);
                const arg_1_81 = int32ToString(actual_81);
                errorMsg_81 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_86)(arg_1_81)("empty");
            }
            else {
                errorMsg_81 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_81)("empty");
            }
            throw new Exception(errorMsg_81);
        }
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.map type conversion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        const r_40 = [1, 2, 3];
        const mapped_2 = ResizeArr_map((x_102) => (x_102 * 10), r_40);
        const actual_82 = item(0, mapped_2);
        if ((actual_82 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_82, 10, "converted to float");
        }
        else {
            let valueType_82;
            let copyOfStruct_82 = actual_82;
            valueType_82 = float64_type;
            const primitiveTypes_82 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_82;
            if (contains(valueType_82, primitiveTypes_82, {
                Equals: equals,
                GetHashCode: (x_103) => (structuralHash(x_103) | 0),
            })) {
                const arg_87 = (10).toString();
                const arg_1_82 = actual_82.toString();
                errorMsg_82 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_87)(arg_1_82)("converted to float");
            }
            else {
                errorMsg_82 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_82)("converted to float");
            }
            throw new Exception(errorMsg_82);
        }
        const actual_83 = item(1, mapped_2);
        if ((actual_83 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_83, 20, "converted to float");
        }
        else {
            let valueType_83;
            let copyOfStruct_83 = actual_83;
            valueType_83 = float64_type;
            const primitiveTypes_83 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_83;
            if (contains(valueType_83, primitiveTypes_83, {
                Equals: equals,
                GetHashCode: (x_104) => (structuralHash(x_104) | 0),
            })) {
                const arg_88 = (20).toString();
                const arg_1_83 = actual_83.toString();
                errorMsg_83 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_88)(arg_1_83)("converted to float");
            }
            else {
                errorMsg_83 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_83)("converted to float");
            }
            throw new Exception(errorMsg_83);
        }
        const actual_84 = item(2, mapped_2);
        if ((actual_84 === 30) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_84, 30, "converted to float");
        }
        else {
            let valueType_84;
            let copyOfStruct_84 = actual_84;
            valueType_84 = float64_type;
            const primitiveTypes_84 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_84;
            if (contains(valueType_84, primitiveTypes_84, {
                Equals: equals,
                GetHashCode: (x_105) => (structuralHash(x_105) | 0),
            })) {
                const arg_89 = (30).toString();
                const arg_1_84 = actual_84.toString();
                errorMsg_84 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_89)(arg_1_84)("converted to float");
            }
            else {
                errorMsg_84 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(30)(actual_84)("converted to float");
            }
            throw new Exception(errorMsg_84);
        }
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.closeLoop adds first to end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        const r_41 = [1, 2, 3];
        const closed = ResizeArr_closeLoop(r_41);
        const actual_85 = closed.length | 0;
        if ((actual_85 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_85, 4, "one more element");
        }
        else {
            let valueType_85;
            let copyOfStruct_85 = actual_85;
            valueType_85 = int32_type;
            const primitiveTypes_85 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_85;
            if (contains(valueType_85, primitiveTypes_85, {
                Equals: equals,
                GetHashCode: (x_106) => (structuralHash(x_106) | 0),
            })) {
                const arg_90 = int32ToString(4);
                const arg_1_85 = int32ToString(actual_85);
                errorMsg_85 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_90)(arg_1_85)("one more element");
            }
            else {
                errorMsg_85 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_85)("one more element");
            }
            throw new Exception(errorMsg_85);
        }
        const actual_86 = item(0, closed) | 0;
        if ((actual_86 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_86, 1, "first");
        }
        else {
            let valueType_86;
            let copyOfStruct_86 = actual_86;
            valueType_86 = int32_type;
            const primitiveTypes_86 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_86;
            if (contains(valueType_86, primitiveTypes_86, {
                Equals: equals,
                GetHashCode: (x_107) => (structuralHash(x_107) | 0),
            })) {
                const arg_91 = int32ToString(1);
                const arg_1_86 = int32ToString(actual_86);
                errorMsg_86 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_91)(arg_1_86)("first");
            }
            else {
                errorMsg_86 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_86)("first");
            }
            throw new Exception(errorMsg_86);
        }
        const actual_87 = item(1, closed) | 0;
        if ((actual_87 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_87, 2, "second");
        }
        else {
            let valueType_87;
            let copyOfStruct_87 = actual_87;
            valueType_87 = int32_type;
            const primitiveTypes_87 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_87;
            if (contains(valueType_87, primitiveTypes_87, {
                Equals: equals,
                GetHashCode: (x_108) => (structuralHash(x_108) | 0),
            })) {
                const arg_92 = int32ToString(2);
                const arg_1_87 = int32ToString(actual_87);
                errorMsg_87 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_92)(arg_1_87)("second");
            }
            else {
                errorMsg_87 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_87)("second");
            }
            throw new Exception(errorMsg_87);
        }
        const actual_88 = item(2, closed) | 0;
        if ((actual_88 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_88, 3, "third");
        }
        else {
            let valueType_88;
            let copyOfStruct_88 = actual_88;
            valueType_88 = int32_type;
            const primitiveTypes_88 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_88;
            if (contains(valueType_88, primitiveTypes_88, {
                Equals: equals,
                GetHashCode: (x_109) => (structuralHash(x_109) | 0),
            })) {
                const arg_93 = int32ToString(3);
                const arg_1_88 = int32ToString(actual_88);
                errorMsg_88 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_93)(arg_1_88)("third");
            }
            else {
                errorMsg_88 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_88)("third");
            }
            throw new Exception(errorMsg_88);
        }
        const actual_89 = item(3, closed) | 0;
        if ((actual_89 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_89, 1, "last is copy of first");
        }
        else {
            let valueType_89;
            let copyOfStruct_89 = actual_89;
            valueType_89 = int32_type;
            const primitiveTypes_89 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_89;
            if (contains(valueType_89, primitiveTypes_89, {
                Equals: equals,
                GetHashCode: (x_110) => (structuralHash(x_110) | 0),
            })) {
                const arg_94 = int32ToString(1);
                const arg_1_89 = int32ToString(actual_89);
                errorMsg_89 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_94)(arg_1_89)("last is copy of first");
            }
            else {
                errorMsg_89 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_89)("last is copy of first");
            }
            throw new Exception(errorMsg_89);
        }
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.closeLoop input unchanged", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        const r_42 = [1, 2, 3];
        const originalValues_4 = toArray(r_42);
        ResizeArr_closeLoop(r_42);
        const actual_90 = r_42.length | 0;
        if ((actual_90 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_90, 3, "count unchanged");
        }
        else {
            let valueType_90;
            let copyOfStruct_90 = actual_90;
            valueType_90 = int32_type;
            const primitiveTypes_90 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_90;
            if (contains(valueType_90, primitiveTypes_90, {
                Equals: equals,
                GetHashCode: (x_111) => (structuralHash(x_111) | 0),
            })) {
                const arg_95 = int32ToString(3);
                const arg_1_90 = int32ToString(actual_90);
                errorMsg_90 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_95)(arg_1_90)("count unchanged");
            }
            else {
                errorMsg_90 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_90)("count unchanged");
            }
            throw new Exception(errorMsg_90);
        }
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_54, rangeDouble(0, 1, 2), (_arg_6) => {
            const i_6 = _arg_6 | 0;
            const actual_91 = item(i_6, r_42) | 0;
            const expected_91 = item(i_6, originalValues_4) | 0;
            const msg_91 = `element ${i_6} unchanged`;
            if ((actual_91 === expected_91) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_91, expected_91, msg_91);
            }
            else {
                let valueType_91;
                let copyOfStruct_91 = actual_91;
                valueType_91 = int32_type;
                const primitiveTypes_91 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_91;
                if (contains(valueType_91, primitiveTypes_91, {
                    Equals: equals,
                    GetHashCode: (x_112) => (structuralHash(x_112) | 0),
                })) {
                    const arg_96 = int32ToString(expected_91);
                    const arg_1_91 = int32ToString(actual_91);
                    errorMsg_91 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_96)(arg_1_91)(msg_91);
                }
                else {
                    errorMsg_91 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_91)(actual_91)(msg_91);
                }
                throw new Exception(errorMsg_91);
            }
            Test_TestCaseBuilder__Zero(builder$0040_54);
        });
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.closeLoop single element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        const r_43 = [42];
        const closed_1 = ResizeArr_closeLoop(r_43);
        const actual_92 = closed_1.length | 0;
        if ((actual_92 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_92, 2, "two elements");
        }
        else {
            let valueType_92;
            let copyOfStruct_92 = actual_92;
            valueType_92 = int32_type;
            const primitiveTypes_92 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_92;
            if (contains(valueType_92, primitiveTypes_92, {
                Equals: equals,
                GetHashCode: (x_113) => (structuralHash(x_113) | 0),
            })) {
                const arg_97 = int32ToString(2);
                const arg_1_92 = int32ToString(actual_92);
                errorMsg_92 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_97)(arg_1_92)("two elements");
            }
            else {
                errorMsg_92 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_92)("two elements");
            }
            throw new Exception(errorMsg_92);
        }
        const actual_93 = item(0, closed_1) | 0;
        if ((actual_93 === 42) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_93, 42, "first");
        }
        else {
            let valueType_93;
            let copyOfStruct_93 = actual_93;
            valueType_93 = int32_type;
            const primitiveTypes_93 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_93;
            if (contains(valueType_93, primitiveTypes_93, {
                Equals: equals,
                GetHashCode: (x_114) => (structuralHash(x_114) | 0),
            })) {
                const arg_98 = int32ToString(42);
                const arg_1_93 = int32ToString(actual_93);
                errorMsg_93 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_98)(arg_1_93)("first");
            }
            else {
                errorMsg_93 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(42)(actual_93)("first");
            }
            throw new Exception(errorMsg_93);
        }
        const actual_94 = item(1, closed_1) | 0;
        if ((actual_94 === 42) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_94, 42, "last is first");
        }
        else {
            let valueType_94;
            let copyOfStruct_94 = actual_94;
            valueType_94 = int32_type;
            const primitiveTypes_94 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_94;
            if (contains(valueType_94, primitiveTypes_94, {
                Equals: equals,
                GetHashCode: (x_115) => (structuralHash(x_115) | 0),
            })) {
                const arg_99 = int32ToString(42);
                const arg_1_94 = int32ToString(actual_94);
                errorMsg_94 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_99)(arg_1_94)("last is first");
            }
            else {
                errorMsg_94 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(42)(actual_94)("last is first");
            }
            throw new Exception(errorMsg_94);
        }
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.tryFindBack finds last match", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        const r_44 = [1, 2, 3, 2, 1];
        const result = ResizeArr_tryFindBack((x_116) => (x_116 === 2), r_44);
        const actual_95 = result;
        const expected_95 = 2;
        if (equals_1(actual_95, expected_95) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_95, expected_95, "found 2");
        }
        else {
            let valueType_95;
            let copyOfStruct_95 = actual_95;
            valueType_95 = option_type(int32_type);
            const primitiveTypes_95 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_95;
            if (contains(valueType_95, primitiveTypes_95, {
                Equals: equals,
                GetHashCode: (x_117) => (structuralHash(x_117) | 0),
            })) {
                const arg_100 = toString(expected_95);
                const arg_1_95 = toString(actual_95);
                errorMsg_95 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_100)(arg_1_95)("found 2");
            }
            else {
                errorMsg_95 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_95)(actual_95)("found 2");
            }
            throw new Exception(errorMsg_95);
        }
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.tryFindBack returns None when not found", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        const r_45 = [1, 2, 3];
        const result_1 = ResizeArr_tryFindBack((x_118) => (x_118 === 99), r_45);
        const actual_96 = result_1;
        const expected_96 = undefined;
        if (equals_1(actual_96, expected_96) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_96, expected_96, "not found");
        }
        else {
            let valueType_96;
            let copyOfStruct_96 = actual_96;
            valueType_96 = option_type(int32_type);
            const primitiveTypes_96 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_96;
            if (contains(valueType_96, primitiveTypes_96, {
                Equals: equals,
                GetHashCode: (x_119) => (structuralHash(x_119) | 0),
            })) {
                const arg_101 = toString(expected_96);
                const arg_1_96 = toString(actual_96);
                errorMsg_96 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_101)(arg_1_96)("not found");
            }
            else {
                errorMsg_96 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_96)(actual_96)("not found");
            }
            throw new Exception(errorMsg_96);
        }
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.tryFindBack empty array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        const r_46 = [];
        const result_2 = ResizeArr_tryFindBack((_arg_7) => true, r_46);
        const actual_97 = result_2;
        const expected_97 = undefined;
        if (equals_1(actual_97, expected_97) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_97, expected_97, "empty returns None");
        }
        else {
            let valueType_97;
            let copyOfStruct_97 = actual_97;
            valueType_97 = option_type(int32_type);
            const primitiveTypes_97 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_97;
            if (contains(valueType_97, primitiveTypes_97, {
                Equals: equals,
                GetHashCode: (x_120) => (structuralHash(x_120) | 0),
            })) {
                const arg_102 = toString(expected_97);
                const arg_1_97 = toString(actual_97);
                errorMsg_97 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_102)(arg_1_97)("empty returns None");
            }
            else {
                errorMsg_97 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_97)(actual_97)("empty returns None");
            }
            throw new Exception(errorMsg_97);
        }
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ResizeArr.tryFindBack input unchanged", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        const r_47 = [1, 2, 3, 4, 5];
        const originalValues_5 = toArray(r_47);
        ResizeArr_tryFindBack((x_121) => (x_121 > 2), r_47);
        const actual_98 = r_47.length | 0;
        if ((actual_98 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_98, 5, "count unchanged");
        }
        else {
            let valueType_98;
            let copyOfStruct_98 = actual_98;
            valueType_98 = int32_type;
            const primitiveTypes_98 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_98;
            if (contains(valueType_98, primitiveTypes_98, {
                Equals: equals,
                GetHashCode: (x_122) => (structuralHash(x_122) | 0),
            })) {
                const arg_103 = int32ToString(5);
                const arg_1_98 = int32ToString(actual_98);
                errorMsg_98 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_103)(arg_1_98)("count unchanged");
            }
            else {
                errorMsg_98 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_98)("count unchanged");
            }
            throw new Exception(errorMsg_98);
        }
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_59, rangeDouble(0, 1, 4), (_arg_8) => {
            const i_7 = _arg_8 | 0;
            const actual_99 = item(i_7, r_47) | 0;
            const expected_99 = item(i_7, originalValues_5) | 0;
            const msg_99 = `element ${i_7} unchanged`;
            if ((actual_99 === expected_99) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_99, expected_99, msg_99);
            }
            else {
                let valueType_99;
                let copyOfStruct_99 = actual_99;
                valueType_99 = int32_type;
                const primitiveTypes_99 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_99;
                if (contains(valueType_99, primitiveTypes_99, {
                    Equals: equals,
                    GetHashCode: (x_123) => (structuralHash(x_123) | 0),
                })) {
                    const arg_104 = int32ToString(expected_99);
                    const arg_1_99 = int32ToString(actual_99);
                    errorMsg_99 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_104)(arg_1_99)(msg_99);
                }
                else {
                    errorMsg_99 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_99)(actual_99)(msg_99);
                }
                throw new Exception(errorMsg_99);
            }
            Test_TestCaseBuilder__Zero(builder$0040_59);
        });
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LastIndex returns Count - 1", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        const r_48 = [1, 2, 3, 4, 5];
        const actual_100 = (r_48.length - 1) | 0;
        if ((actual_100 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_100, 4, "last index is 4");
        }
        else {
            let valueType_100;
            let copyOfStruct_100 = actual_100;
            valueType_100 = int32_type;
            const primitiveTypes_100 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_100;
            if (contains(valueType_100, primitiveTypes_100, {
                Equals: equals,
                GetHashCode: (x_124) => (structuralHash(x_124) | 0),
            })) {
                const arg_105 = int32ToString(4);
                const arg_1_100 = int32ToString(actual_100);
                errorMsg_100 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_105)(arg_1_100)("last index is 4");
            }
            else {
                errorMsg_100 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_100)("last index is 4");
            }
            throw new Exception(errorMsg_100);
        }
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Last get returns last element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        const r_49 = [1, 2, 3];
        let actual_101;
        const this$_1 = r_49;
        if (this$_1.length === 0) {
            failRarr("Last.get", this$_1);
        }
        actual_101 = item(this$_1.length - 1, this$_1);
        if ((actual_101 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_101, 3, "last is 3");
        }
        else {
            let valueType_101;
            let copyOfStruct_101 = actual_101;
            valueType_101 = int32_type;
            const primitiveTypes_101 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_101;
            if (contains(valueType_101, primitiveTypes_101, {
                Equals: equals,
                GetHashCode: (x_125) => (structuralHash(x_125) | 0),
            })) {
                const arg_106 = int32ToString(3);
                const arg_1_101 = int32ToString(actual_101);
                errorMsg_101 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_106)(arg_1_101)("last is 3");
            }
            else {
                errorMsg_101 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_101)("last is 3");
            }
            throw new Exception(errorMsg_101);
        }
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Last set modifies last element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        const r_50 = [1, 2, 3];
        const this$_2 = r_50;
        if (this$_2.length === 0) {
            failRarr("Last.set", this$_2);
        }
        setItem(this$_2, this$_2.length - 1, 99);
        const actual_102 = item(2, r_50) | 0;
        if ((actual_102 === 99) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_102, 99, "last modified to 99");
        }
        else {
            let valueType_102;
            let copyOfStruct_102 = actual_102;
            valueType_102 = int32_type;
            const primitiveTypes_102 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_102;
            if (contains(valueType_102, primitiveTypes_102, {
                Equals: equals,
                GetHashCode: (x_126) => (structuralHash(x_126) | 0),
            })) {
                const arg_107 = int32ToString(99);
                const arg_1_102 = int32ToString(actual_102);
                errorMsg_102 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_107)(arg_1_102)("last modified to 99");
            }
            else {
                errorMsg_102 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(99)(actual_102)("last modified to 99");
            }
            throw new Exception(errorMsg_102);
        }
        Test_TestCaseBuilder__Zero(builder$0040_62);
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SecondLast get returns second last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        const r_51 = [1, 2, 3, 4];
        let actual_103;
        const this$_3 = r_51;
        if (this$_3.length < 2) {
            failRarr("SecondLast.get", this$_3);
        }
        actual_103 = item(this$_3.length - 2, this$_3);
        if ((actual_103 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_103, 3, "second last is 3");
        }
        else {
            let valueType_103;
            let copyOfStruct_103 = actual_103;
            valueType_103 = int32_type;
            const primitiveTypes_103 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_103;
            if (contains(valueType_103, primitiveTypes_103, {
                Equals: equals,
                GetHashCode: (x_127) => (structuralHash(x_127) | 0),
            })) {
                const arg_108 = int32ToString(3);
                const arg_1_103 = int32ToString(actual_103);
                errorMsg_103 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_108)(arg_1_103)("second last is 3");
            }
            else {
                errorMsg_103 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_103)("second last is 3");
            }
            throw new Exception(errorMsg_103);
        }
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SecondLast set modifies second last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        const r_52 = [1, 2, 3, 4];
        const this$_4 = r_52;
        if (this$_4.length < 2) {
            failRarr("SecondLast.set", this$_4);
        }
        setItem(this$_4, this$_4.length - 2, 99);
        const actual_104 = item(2, r_52) | 0;
        if ((actual_104 === 99) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_104, 99, "second last modified");
        }
        else {
            let valueType_104;
            let copyOfStruct_104 = actual_104;
            valueType_104 = int32_type;
            const primitiveTypes_104 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_104;
            if (contains(valueType_104, primitiveTypes_104, {
                Equals: equals,
                GetHashCode: (x_128) => (structuralHash(x_128) | 0),
            })) {
                const arg_109 = int32ToString(99);
                const arg_1_104 = int32ToString(actual_104);
                errorMsg_104 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_109)(arg_1_104)("second last modified");
            }
            else {
                errorMsg_104 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(99)(actual_104)("second last modified");
            }
            throw new Exception(errorMsg_104);
        }
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ThirdLast get returns third last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        const r_53 = [1, 2, 3, 4, 5];
        let actual_105;
        const this$_5 = r_53;
        if (this$_5.length < 3) {
            failRarr("ThirdLast.get", this$_5);
        }
        actual_105 = item(this$_5.length - 3, this$_5);
        if ((actual_105 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_105, 3, "third last is 3");
        }
        else {
            let valueType_105;
            let copyOfStruct_105 = actual_105;
            valueType_105 = int32_type;
            const primitiveTypes_105 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_105;
            if (contains(valueType_105, primitiveTypes_105, {
                Equals: equals,
                GetHashCode: (x_129) => (structuralHash(x_129) | 0),
            })) {
                const arg_110 = int32ToString(3);
                const arg_1_105 = int32ToString(actual_105);
                errorMsg_105 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_110)(arg_1_105)("third last is 3");
            }
            else {
                errorMsg_105 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_105)("third last is 3");
            }
            throw new Exception(errorMsg_105);
        }
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})(), (() => {
    const builder$0040_66 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ThirdLast set modifies third last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_66, Test_TestCaseBuilder__Delay_1505(builder$0040_66, () => {
        const r_54 = [1, 2, 3, 4, 5];
        const this$_6 = r_54;
        if (this$_6.length < 3) {
            failRarr("ThirdLast.set", this$_6);
        }
        setItem(this$_6, this$_6.length - 3, 99);
        const actual_106 = item(2, r_54) | 0;
        if ((actual_106 === 99) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_106, 99, "third last modified");
        }
        else {
            let valueType_106;
            let copyOfStruct_106 = actual_106;
            valueType_106 = int32_type;
            const primitiveTypes_106 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_106;
            if (contains(valueType_106, primitiveTypes_106, {
                Equals: equals,
                GetHashCode: (x_130) => (structuralHash(x_130) | 0),
            })) {
                const arg_111 = int32ToString(99);
                const arg_1_106 = int32ToString(actual_106);
                errorMsg_106 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_111)(arg_1_106)("third last modified");
            }
            else {
                errorMsg_106 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(99)(actual_106)("third last modified");
            }
            throw new Exception(errorMsg_106);
        }
        Test_TestCaseBuilder__Zero(builder$0040_66);
    }));
})(), (() => {
    const builder$0040_67 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("First get returns first element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_67, Test_TestCaseBuilder__Delay_1505(builder$0040_67, () => {
        const r_55 = [1, 2, 3];
        let actual_107;
        const this$_7 = r_55;
        if (this$_7.length === 0) {
            failRarr("First.get", this$_7);
        }
        actual_107 = item(0, this$_7);
        if ((actual_107 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_107, 1, "first is 1");
        }
        else {
            let valueType_107;
            let copyOfStruct_107 = actual_107;
            valueType_107 = int32_type;
            const primitiveTypes_107 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_107;
            if (contains(valueType_107, primitiveTypes_107, {
                Equals: equals,
                GetHashCode: (x_131) => (structuralHash(x_131) | 0),
            })) {
                const arg_112 = int32ToString(1);
                const arg_1_107 = int32ToString(actual_107);
                errorMsg_107 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_112)(arg_1_107)("first is 1");
            }
            else {
                errorMsg_107 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_107)("first is 1");
            }
            throw new Exception(errorMsg_107);
        }
        Test_TestCaseBuilder__Zero(builder$0040_67);
    }));
})(), (() => {
    const builder$0040_68 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("First set modifies first element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_68, Test_TestCaseBuilder__Delay_1505(builder$0040_68, () => {
        const r_56 = [1, 2, 3];
        const this$_8 = r_56;
        if (this$_8.length === 0) {
            failRarr("First.set", this$_8);
        }
        setItem(this$_8, 0, 99);
        const actual_108 = item(0, r_56) | 0;
        if ((actual_108 === 99) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_108, 99, "first modified to 99");
        }
        else {
            let valueType_108;
            let copyOfStruct_108 = actual_108;
            valueType_108 = int32_type;
            const primitiveTypes_108 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_108;
            if (contains(valueType_108, primitiveTypes_108, {
                Equals: equals,
                GetHashCode: (x_132) => (structuralHash(x_132) | 0),
            })) {
                const arg_113 = int32ToString(99);
                const arg_1_108 = int32ToString(actual_108);
                errorMsg_108 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_113)(arg_1_108)("first modified to 99");
            }
            else {
                errorMsg_108 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(99)(actual_108)("first modified to 99");
            }
            throw new Exception(errorMsg_108);
        }
        Test_TestCaseBuilder__Zero(builder$0040_68);
    }));
})(), (() => {
    const builder$0040_69 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Second get returns second element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_69, Test_TestCaseBuilder__Delay_1505(builder$0040_69, () => {
        const r_57 = [1, 2, 3];
        let actual_109;
        const this$_9 = r_57;
        if (this$_9.length < 2) {
            failRarr("Second.get", this$_9);
        }
        actual_109 = item(1, this$_9);
        if ((actual_109 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_109, 2, "second is 2");
        }
        else {
            let valueType_109;
            let copyOfStruct_109 = actual_109;
            valueType_109 = int32_type;
            const primitiveTypes_109 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_109;
            if (contains(valueType_109, primitiveTypes_109, {
                Equals: equals,
                GetHashCode: (x_133) => (structuralHash(x_133) | 0),
            })) {
                const arg_114 = int32ToString(2);
                const arg_1_109 = int32ToString(actual_109);
                errorMsg_109 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_114)(arg_1_109)("second is 2");
            }
            else {
                errorMsg_109 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_109)("second is 2");
            }
            throw new Exception(errorMsg_109);
        }
        Test_TestCaseBuilder__Zero(builder$0040_69);
    }));
})(), (() => {
    const builder$0040_70 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Second set modifies second element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_70, Test_TestCaseBuilder__Delay_1505(builder$0040_70, () => {
        const r_58 = [1, 2, 3];
        const this$_10 = r_58;
        if (this$_10.length < 2) {
            failRarr("Second.set", this$_10);
        }
        setItem(this$_10, 1, 99);
        const actual_110 = item(1, r_58) | 0;
        if ((actual_110 === 99) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_110, 99, "second modified to 99");
        }
        else {
            let valueType_110;
            let copyOfStruct_110 = actual_110;
            valueType_110 = int32_type;
            const primitiveTypes_110 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_110;
            if (contains(valueType_110, primitiveTypes_110, {
                Equals: equals,
                GetHashCode: (x_134) => (structuralHash(x_134) | 0),
            })) {
                const arg_115 = int32ToString(99);
                const arg_1_110 = int32ToString(actual_110);
                errorMsg_110 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_115)(arg_1_110)("second modified to 99");
            }
            else {
                errorMsg_110 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(99)(actual_110)("second modified to 99");
            }
            throw new Exception(errorMsg_110);
        }
        Test_TestCaseBuilder__Zero(builder$0040_70);
    }));
})(), (() => {
    const builder$0040_71 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Third get returns third element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_71, Test_TestCaseBuilder__Delay_1505(builder$0040_71, () => {
        const r_59 = [1, 2, 3, 4];
        let actual_111;
        const this$_11 = r_59;
        if (this$_11.length < 3) {
            failRarr("Third.get", this$_11);
        }
        actual_111 = item(2, this$_11);
        if ((actual_111 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_111, 3, "third is 3");
        }
        else {
            let valueType_111;
            let copyOfStruct_111 = actual_111;
            valueType_111 = int32_type;
            const primitiveTypes_111 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_111;
            if (contains(valueType_111, primitiveTypes_111, {
                Equals: equals,
                GetHashCode: (x_135) => (structuralHash(x_135) | 0),
            })) {
                const arg_116 = int32ToString(3);
                const arg_1_111 = int32ToString(actual_111);
                errorMsg_111 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_116)(arg_1_111)("third is 3");
            }
            else {
                errorMsg_111 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_111)("third is 3");
            }
            throw new Exception(errorMsg_111);
        }
        Test_TestCaseBuilder__Zero(builder$0040_71);
    }));
})(), (() => {
    const builder$0040_72 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Third set modifies third element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_72, Test_TestCaseBuilder__Delay_1505(builder$0040_72, () => {
        const r_60 = [1, 2, 3, 4];
        const this$_12 = r_60;
        if (this$_12.length < 3) {
            failRarr("Third.set", this$_12);
        }
        setItem(this$_12, 2, 99);
        const actual_112 = item(2, r_60) | 0;
        if ((actual_112 === 99) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_112, 99, "third modified to 99");
        }
        else {
            let valueType_112;
            let copyOfStruct_112 = actual_112;
            valueType_112 = int32_type;
            const primitiveTypes_112 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_112;
            if (contains(valueType_112, primitiveTypes_112, {
                Equals: equals,
                GetHashCode: (x_136) => (structuralHash(x_136) | 0),
            })) {
                const arg_117 = int32ToString(99);
                const arg_1_112 = int32ToString(actual_112);
                errorMsg_112 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_117)(arg_1_112)("third modified to 99");
            }
            else {
                errorMsg_112 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(99)(actual_112)("third modified to 99");
            }
            throw new Exception(errorMsg_112);
        }
        Test_TestCaseBuilder__Zero(builder$0040_72);
    }));
})(), (() => {
    const builder$0040_73 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pop removes and returns last element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_73, Test_TestCaseBuilder__Delay_1505(builder$0040_73, () => {
        const r_61 = [1, 2, 3];
        let popped;
        const this$_13 = r_61;
        if (this$_13.length === 0) {
            failRarr("Pop", this$_13);
        }
        const i_8 = (this$_13.length - 1) | 0;
        const v_6 = item(i_8, this$_13) | 0;
        this$_13.splice(i_8, 1);
        popped = v_6;
        const actual_113 = popped | 0;
        if ((actual_113 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_113, 3, "popped is 3");
        }
        else {
            let valueType_113;
            let copyOfStruct_113 = actual_113;
            valueType_113 = int32_type;
            const primitiveTypes_113 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_113;
            if (contains(valueType_113, primitiveTypes_113, {
                Equals: equals,
                GetHashCode: (x_137) => (structuralHash(x_137) | 0),
            })) {
                const arg_118 = int32ToString(3);
                const arg_1_113 = int32ToString(actual_113);
                errorMsg_113 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_118)(arg_1_113)("popped is 3");
            }
            else {
                errorMsg_113 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_113)("popped is 3");
            }
            throw new Exception(errorMsg_113);
        }
        const actual_114 = r_61.length | 0;
        if ((actual_114 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_114, 2, "count reduced");
        }
        else {
            let valueType_114;
            let copyOfStruct_114 = actual_114;
            valueType_114 = int32_type;
            const primitiveTypes_114 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_114;
            if (contains(valueType_114, primitiveTypes_114, {
                Equals: equals,
                GetHashCode: (x_138) => (structuralHash(x_138) | 0),
            })) {
                const arg_119 = int32ToString(2);
                const arg_1_114 = int32ToString(actual_114);
                errorMsg_114 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_119)(arg_1_114)("count reduced");
            }
            else {
                errorMsg_114 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_114)("count reduced");
            }
            throw new Exception(errorMsg_114);
        }
        const actual_115 = item(0, r_61) | 0;
        if ((actual_115 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_115, 1, "first unchanged");
        }
        else {
            let valueType_115;
            let copyOfStruct_115 = actual_115;
            valueType_115 = int32_type;
            const primitiveTypes_115 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_115;
            if (contains(valueType_115, primitiveTypes_115, {
                Equals: equals,
                GetHashCode: (x_139) => (structuralHash(x_139) | 0),
            })) {
                const arg_120 = int32ToString(1);
                const arg_1_115 = int32ToString(actual_115);
                errorMsg_115 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_120)(arg_1_115)("first unchanged");
            }
            else {
                errorMsg_115 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_115)("first unchanged");
            }
            throw new Exception(errorMsg_115);
        }
        const actual_116 = item(1, r_61) | 0;
        if ((actual_116 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_116, 2, "second unchanged");
        }
        else {
            let valueType_116;
            let copyOfStruct_116 = actual_116;
            valueType_116 = int32_type;
            const primitiveTypes_116 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_116;
            if (contains(valueType_116, primitiveTypes_116, {
                Equals: equals,
                GetHashCode: (x_140) => (structuralHash(x_140) | 0),
            })) {
                const arg_121 = int32ToString(2);
                const arg_1_116 = int32ToString(actual_116);
                errorMsg_116 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_121)(arg_1_116)("second unchanged");
            }
            else {
                errorMsg_116 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_116)("second unchanged");
            }
            throw new Exception(errorMsg_116);
        }
        Test_TestCaseBuilder__Zero(builder$0040_73);
    }));
})(), (() => {
    const builder$0040_74 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pop with index removes and returns element", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_74, Test_TestCaseBuilder__Delay_1505(builder$0040_74, () => {
        const r_62 = [1, 2, 3, 4];
        let popped_1;
        const this$_14 = r_62;
        if (1 < 0) {
            failRarr(`Pop(${1})`, this$_14);
        }
        if (1 >= this$_14.length) {
            failRarr(`Pop(${1})`, this$_14);
        }
        const v_7 = item(1, this$_14) | 0;
        this$_14.splice(1, 1);
        popped_1 = v_7;
        const actual_117 = popped_1 | 0;
        if ((actual_117 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_117, 2, "popped is 2");
        }
        else {
            let valueType_117;
            let copyOfStruct_117 = actual_117;
            valueType_117 = int32_type;
            const primitiveTypes_117 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_117;
            if (contains(valueType_117, primitiveTypes_117, {
                Equals: equals,
                GetHashCode: (x_141) => (structuralHash(x_141) | 0),
            })) {
                const arg_122 = int32ToString(2);
                const arg_1_117 = int32ToString(actual_117);
                errorMsg_117 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_122)(arg_1_117)("popped is 2");
            }
            else {
                errorMsg_117 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_117)("popped is 2");
            }
            throw new Exception(errorMsg_117);
        }
        const actual_118 = r_62.length | 0;
        if ((actual_118 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_118, 3, "count reduced");
        }
        else {
            let valueType_118;
            let copyOfStruct_118 = actual_118;
            valueType_118 = int32_type;
            const primitiveTypes_118 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_118;
            if (contains(valueType_118, primitiveTypes_118, {
                Equals: equals,
                GetHashCode: (x_142) => (structuralHash(x_142) | 0),
            })) {
                const arg_123 = int32ToString(3);
                const arg_1_118 = int32ToString(actual_118);
                errorMsg_118 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_123)(arg_1_118)("count reduced");
            }
            else {
                errorMsg_118 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_118)("count reduced");
            }
            throw new Exception(errorMsg_118);
        }
        const actual_119 = item(0, r_62) | 0;
        if ((actual_119 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_119, 1, "first unchanged");
        }
        else {
            let valueType_119;
            let copyOfStruct_119 = actual_119;
            valueType_119 = int32_type;
            const primitiveTypes_119 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_119;
            if (contains(valueType_119, primitiveTypes_119, {
                Equals: equals,
                GetHashCode: (x_143) => (structuralHash(x_143) | 0),
            })) {
                const arg_124 = int32ToString(1);
                const arg_1_119 = int32ToString(actual_119);
                errorMsg_119 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_124)(arg_1_119)("first unchanged");
            }
            else {
                errorMsg_119 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_119)("first unchanged");
            }
            throw new Exception(errorMsg_119);
        }
        const actual_120 = item(1, r_62) | 0;
        if ((actual_120 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_120, 3, "shifted");
        }
        else {
            let valueType_120;
            let copyOfStruct_120 = actual_120;
            valueType_120 = int32_type;
            const primitiveTypes_120 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_120;
            if (contains(valueType_120, primitiveTypes_120, {
                Equals: equals,
                GetHashCode: (x_144) => (structuralHash(x_144) | 0),
            })) {
                const arg_125 = int32ToString(3);
                const arg_1_120 = int32ToString(actual_120);
                errorMsg_120 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_125)(arg_1_120)("shifted");
            }
            else {
                errorMsg_120 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_120)("shifted");
            }
            throw new Exception(errorMsg_120);
        }
        const actual_121 = item(2, r_62) | 0;
        if ((actual_121 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_121, 4, "shifted");
        }
        else {
            let valueType_121;
            let copyOfStruct_121 = actual_121;
            valueType_121 = int32_type;
            const primitiveTypes_121 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_121;
            if (contains(valueType_121, primitiveTypes_121, {
                Equals: equals,
                GetHashCode: (x_145) => (structuralHash(x_145) | 0),
            })) {
                const arg_126 = int32ToString(4);
                const arg_1_121 = int32ToString(actual_121);
                errorMsg_121 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_126)(arg_1_121)("shifted");
            }
            else {
                errorMsg_121 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_121)("shifted");
            }
            throw new Exception(errorMsg_121);
        }
        Test_TestCaseBuilder__Zero(builder$0040_74);
    }));
})(), (() => {
    const builder$0040_75 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("GetLooped with positive index", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_75, Test_TestCaseBuilder__Delay_1505(builder$0040_75, () => {
        const r_63 = [1, 2, 3];
        let actual_122;
        const this$_15 = r_63;
        const t = (0 % this$_15.length) | 0;
        actual_122 = ((t >= 0) ? item(t, this$_15) : item(t + this$_15.length, this$_15));
        if ((actual_122 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_122, 1, "index 0");
        }
        else {
            let valueType_122;
            let copyOfStruct_122 = actual_122;
            valueType_122 = int32_type;
            const primitiveTypes_122 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_122;
            if (contains(valueType_122, primitiveTypes_122, {
                Equals: equals,
                GetHashCode: (x_146) => (structuralHash(x_146) | 0),
            })) {
                const arg_127 = int32ToString(1);
                const arg_1_122 = int32ToString(actual_122);
                errorMsg_122 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_127)(arg_1_122)("index 0");
            }
            else {
                errorMsg_122 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_122)("index 0");
            }
            throw new Exception(errorMsg_122);
        }
        let actual_123;
        const this$_16 = r_63;
        const t_1 = (1 % this$_16.length) | 0;
        actual_123 = ((t_1 >= 0) ? item(t_1, this$_16) : item(t_1 + this$_16.length, this$_16));
        if ((actual_123 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_123, 2, "index 1");
        }
        else {
            let valueType_123;
            let copyOfStruct_123 = actual_123;
            valueType_123 = int32_type;
            const primitiveTypes_123 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_123;
            if (contains(valueType_123, primitiveTypes_123, {
                Equals: equals,
                GetHashCode: (x_147) => (structuralHash(x_147) | 0),
            })) {
                const arg_128 = int32ToString(2);
                const arg_1_123 = int32ToString(actual_123);
                errorMsg_123 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_128)(arg_1_123)("index 1");
            }
            else {
                errorMsg_123 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_123)("index 1");
            }
            throw new Exception(errorMsg_123);
        }
        let actual_124;
        const this$_17 = r_63;
        const t_2 = (2 % this$_17.length) | 0;
        actual_124 = ((t_2 >= 0) ? item(t_2, this$_17) : item(t_2 + this$_17.length, this$_17));
        if ((actual_124 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_124, 3, "index 2");
        }
        else {
            let valueType_124;
            let copyOfStruct_124 = actual_124;
            valueType_124 = int32_type;
            const primitiveTypes_124 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_124;
            if (contains(valueType_124, primitiveTypes_124, {
                Equals: equals,
                GetHashCode: (x_148) => (structuralHash(x_148) | 0),
            })) {
                const arg_129 = int32ToString(3);
                const arg_1_124 = int32ToString(actual_124);
                errorMsg_124 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_129)(arg_1_124)("index 2");
            }
            else {
                errorMsg_124 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_124)("index 2");
            }
            throw new Exception(errorMsg_124);
        }
        Test_TestCaseBuilder__Zero(builder$0040_75);
    }));
})(), (() => {
    const builder$0040_76 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("GetLooped wraps positive overflow", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_76, Test_TestCaseBuilder__Delay_1505(builder$0040_76, () => {
        const r_64 = [1, 2, 3];
        let actual_125;
        const this$_18 = r_64;
        const t_3 = (3 % this$_18.length) | 0;
        actual_125 = ((t_3 >= 0) ? item(t_3, this$_18) : item(t_3 + this$_18.length, this$_18));
        if ((actual_125 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_125, 1, "wraps to 0");
        }
        else {
            let valueType_125;
            let copyOfStruct_125 = actual_125;
            valueType_125 = int32_type;
            const primitiveTypes_125 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_125;
            if (contains(valueType_125, primitiveTypes_125, {
                Equals: equals,
                GetHashCode: (x_149) => (structuralHash(x_149) | 0),
            })) {
                const arg_130 = int32ToString(1);
                const arg_1_125 = int32ToString(actual_125);
                errorMsg_125 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_130)(arg_1_125)("wraps to 0");
            }
            else {
                errorMsg_125 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_125)("wraps to 0");
            }
            throw new Exception(errorMsg_125);
        }
        let actual_126;
        const this$_19 = r_64;
        const t_4 = (4 % this$_19.length) | 0;
        actual_126 = ((t_4 >= 0) ? item(t_4, this$_19) : item(t_4 + this$_19.length, this$_19));
        if ((actual_126 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_126, 2, "wraps to 1");
        }
        else {
            let valueType_126;
            let copyOfStruct_126 = actual_126;
            valueType_126 = int32_type;
            const primitiveTypes_126 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_126;
            if (contains(valueType_126, primitiveTypes_126, {
                Equals: equals,
                GetHashCode: (x_150) => (structuralHash(x_150) | 0),
            })) {
                const arg_131 = int32ToString(2);
                const arg_1_126 = int32ToString(actual_126);
                errorMsg_126 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_131)(arg_1_126)("wraps to 1");
            }
            else {
                errorMsg_126 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_126)("wraps to 1");
            }
            throw new Exception(errorMsg_126);
        }
        let actual_127;
        const this$_20 = r_64;
        const t_5 = (5 % this$_20.length) | 0;
        actual_127 = ((t_5 >= 0) ? item(t_5, this$_20) : item(t_5 + this$_20.length, this$_20));
        if ((actual_127 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_127, 3, "wraps to 2");
        }
        else {
            let valueType_127;
            let copyOfStruct_127 = actual_127;
            valueType_127 = int32_type;
            const primitiveTypes_127 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_127;
            if (contains(valueType_127, primitiveTypes_127, {
                Equals: equals,
                GetHashCode: (x_151) => (structuralHash(x_151) | 0),
            })) {
                const arg_132 = int32ToString(3);
                const arg_1_127 = int32ToString(actual_127);
                errorMsg_127 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_132)(arg_1_127)("wraps to 2");
            }
            else {
                errorMsg_127 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_127)("wraps to 2");
            }
            throw new Exception(errorMsg_127);
        }
        Test_TestCaseBuilder__Zero(builder$0040_76);
    }));
})(), (() => {
    const builder$0040_77 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("GetLooped wraps negative index", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_77, Test_TestCaseBuilder__Delay_1505(builder$0040_77, () => {
        const r_65 = [1, 2, 3];
        let actual_128;
        const this$_21 = r_65;
        const t_6 = (-1 % this$_21.length) | 0;
        actual_128 = ((t_6 >= 0) ? item(t_6, this$_21) : item(t_6 + this$_21.length, this$_21));
        if ((actual_128 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_128, 3, "-1 is last");
        }
        else {
            let valueType_128;
            let copyOfStruct_128 = actual_128;
            valueType_128 = int32_type;
            const primitiveTypes_128 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_128;
            if (contains(valueType_128, primitiveTypes_128, {
                Equals: equals,
                GetHashCode: (x_152) => (structuralHash(x_152) | 0),
            })) {
                const arg_133 = int32ToString(3);
                const arg_1_128 = int32ToString(actual_128);
                errorMsg_128 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_133)(arg_1_128)("-1 is last");
            }
            else {
                errorMsg_128 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_128)("-1 is last");
            }
            throw new Exception(errorMsg_128);
        }
        let actual_129;
        const this$_22 = r_65;
        const t_7 = (-2 % this$_22.length) | 0;
        actual_129 = ((t_7 >= 0) ? item(t_7, this$_22) : item(t_7 + this$_22.length, this$_22));
        if ((actual_129 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_129, 2, "-2 is second last");
        }
        else {
            let valueType_129;
            let copyOfStruct_129 = actual_129;
            valueType_129 = int32_type;
            const primitiveTypes_129 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_129;
            if (contains(valueType_129, primitiveTypes_129, {
                Equals: equals,
                GetHashCode: (x_153) => (structuralHash(x_153) | 0),
            })) {
                const arg_134 = int32ToString(2);
                const arg_1_129 = int32ToString(actual_129);
                errorMsg_129 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_134)(arg_1_129)("-2 is second last");
            }
            else {
                errorMsg_129 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_129)("-2 is second last");
            }
            throw new Exception(errorMsg_129);
        }
        let actual_130;
        const this$_23 = r_65;
        const t_8 = (-3 % this$_23.length) | 0;
        actual_130 = ((t_8 >= 0) ? item(t_8, this$_23) : item(t_8 + this$_23.length, this$_23));
        if ((actual_130 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_130, 1, "-3 is first");
        }
        else {
            let valueType_130;
            let copyOfStruct_130 = actual_130;
            valueType_130 = int32_type;
            const primitiveTypes_130 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_130;
            if (contains(valueType_130, primitiveTypes_130, {
                Equals: equals,
                GetHashCode: (x_154) => (structuralHash(x_154) | 0),
            })) {
                const arg_135 = int32ToString(1);
                const arg_1_130 = int32ToString(actual_130);
                errorMsg_130 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_135)(arg_1_130)("-3 is first");
            }
            else {
                errorMsg_130 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_130)("-3 is first");
            }
            throw new Exception(errorMsg_130);
        }
        let actual_131;
        const this$_24 = r_65;
        const t_9 = (-4 % this$_24.length) | 0;
        actual_131 = ((t_9 >= 0) ? item(t_9, this$_24) : item(t_9 + this$_24.length, this$_24));
        if ((actual_131 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_131, 3, "wraps around");
        }
        else {
            let valueType_131;
            let copyOfStruct_131 = actual_131;
            valueType_131 = int32_type;
            const primitiveTypes_131 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_131;
            if (contains(valueType_131, primitiveTypes_131, {
                Equals: equals,
                GetHashCode: (x_155) => (structuralHash(x_155) | 0),
            })) {
                const arg_136 = int32ToString(3);
                const arg_1_131 = int32ToString(actual_131);
                errorMsg_131 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_136)(arg_1_131)("wraps around");
            }
            else {
                errorMsg_131 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_131)("wraps around");
            }
            throw new Exception(errorMsg_131);
        }
        Test_TestCaseBuilder__Zero(builder$0040_77);
    }));
})(), (() => {
    const builder$0040_78 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SetIdx sets element at index", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_78, Test_TestCaseBuilder__Delay_1505(builder$0040_78, () => {
        const r_66 = [1, 2, 3];
        const this$_25 = r_66;
        if ((1 < 0) ? true : (1 >= this$_25.length)) {
            failRarr(`SetIdx(${1})`, this$_25);
        }
        setItem(this$_25, 1, 99);
        const actual_132 = item(1, r_66) | 0;
        if ((actual_132 === 99) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_132, 99, "element set");
        }
        else {
            let valueType_132;
            let copyOfStruct_132 = actual_132;
            valueType_132 = int32_type;
            const primitiveTypes_132 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_132;
            if (contains(valueType_132, primitiveTypes_132, {
                Equals: equals,
                GetHashCode: (x_156) => (structuralHash(x_156) | 0),
            })) {
                const arg_137 = int32ToString(99);
                const arg_1_132 = int32ToString(actual_132);
                errorMsg_132 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_137)(arg_1_132)("element set");
            }
            else {
                errorMsg_132 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(99)(actual_132)("element set");
            }
            throw new Exception(errorMsg_132);
        }
        Test_TestCaseBuilder__Zero(builder$0040_78);
    }));
})(), (() => {
    const builder$0040_79 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsEmpty true for empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_79, Test_TestCaseBuilder__Delay_1505(builder$0040_79, () => {
        const r_67 = [];
        Expect_isTrue(r_67.length === 0)("empty is true");
        Test_TestCaseBuilder__Zero(builder$0040_79);
    }));
})(), (() => {
    const builder$0040_80 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsEmpty false for non-empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_80, Test_TestCaseBuilder__Delay_1505(builder$0040_80, () => {
        const r_68 = [1];
        Expect_isFalse(r_68.length === 0)("non-empty is false");
        Test_TestCaseBuilder__Zero(builder$0040_80);
    }));
})()]));

