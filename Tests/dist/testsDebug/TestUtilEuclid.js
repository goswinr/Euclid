
import { AccuracyModule_veryHigh, Expect_floatClose, Test_TestCaseBuilder__Zero, Expect_isFalse, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Exception, int32ToString, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Euclid_UtilEuclid_matchSign } from "./Src/UtilEuclid.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";

export const tests = Test_testList("UtilEuclid", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTooSmall true for small", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        Expect_isTrue(!(1E-07 > 1E-06))("1e-7 should be too small");
        Expect_isTrue(!(NaN > 1E-06))("NaN treated as too small");
        Expect_isFalse(!(0.0001 > 1E-06))("1e-4 not too small");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTooTiny and countTooTinyOrNaN", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        Expect_isTrue(!(1E-13 > 1E-12))("1e-13 tiny (<1e-12)");
        Expect_isFalse(!(2E-12 > 1E-12))("> tolerance");
        const actual = ((1E-13 > 1E-12) ? 0 : 1) | 0;
        if ((actual === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 1, "tiny counts 1");
        }
        else {
            let valueType;
            let copyOfStruct = actual;
            valueType = int32_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg = int32ToString(1);
                const arg_1 = int32ToString(actual);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("tiny counts 1");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual)("tiny counts 1");
            }
            throw new Exception(errorMsg);
        }
        const actual_1 = ((2E-12 > 1E-12) ? 0 : 1) | 0;
        if ((actual_1 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 0, "not tiny counts 0");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_1;
            valueType_1 = int32_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            })) {
                const arg_6 = int32ToString(0);
                const arg_1_1 = int32ToString(actual_1);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("not tiny counts 0");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_1)("not tiny counts 0");
            }
            throw new Exception(errorMsg_1);
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTooTinySq and countTooTinySqOrNaN", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        Expect_isTrue(!(1E-25 > 1E-24))("<1e-24");
        Expect_isFalse(!(2E-24 > 1E-24))(">=1e-24");
        const actual_2 = ((1E-25 > 1E-24) ? 0 : 1) | 0;
        if ((actual_2 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 1, "sq tiny counts 1");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_2;
            valueType_2 = int32_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            })) {
                const arg_7 = int32ToString(1);
                const arg_1_2 = int32ToString(actual_2);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("sq tiny counts 1");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_2)("sq tiny counts 1");
            }
            throw new Exception(errorMsg_2);
        }
        const actual_3 = ((2E-24 > 1E-24) ? 0 : 1) | 0;
        if ((actual_3 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 0, "sq not tiny counts 0");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_3;
            valueType_3 = int32_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            })) {
                const arg_8 = int32ToString(0);
                const arg_1_3 = int32ToString(actual_3);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("sq not tiny counts 0");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_3)("sq not tiny counts 0");
            }
            throw new Exception(errorMsg_3);
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isNegative includes NaN", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        Expect_isTrue(!(NaN >= 0))("NaN counts as negative");
        Expect_isFalse(!(0 >= 0))("0 not negative");
        Expect_isTrue(!(-0.0001 >= 0))("negative");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("degree rad conversion round trip", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const d = 33.4;
        const r = 0.017453292519943295 * d;
        const d2 = 57.29577951308232 * r;
        Expect_floatClose(AccuracyModule_veryHigh, d, d2, "deg->rad->deg roundtrip");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("clampBetweenMinusOneAndOne", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const actual_4 = (-2 > -1) ? ((-2 < 1) ? -2 : 1) : -1;
        if ((actual_4 === -1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, -1, "low clamp");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_4;
            valueType_4 = float64_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            })) {
                const arg_9 = (-1).toString();
                const arg_1_4 = actual_4.toString();
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("low clamp");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(-1)(actual_4)("low clamp");
            }
            throw new Exception(errorMsg_4);
        }
        const actual_5 = (2 > -1) ? ((2 < 1) ? 2 : 1) : -1;
        if ((actual_5 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 1, "high clamp");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_5;
            valueType_5 = float64_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            })) {
                const arg_10 = (1).toString();
                const arg_1_5 = actual_5.toString();
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("high clamp");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_5)("high clamp");
            }
            throw new Exception(errorMsg_5);
        }
        const actual_6 = (0.5 > -1) ? ((0.5 < 1) ? 0.5 : 1) : -1;
        if ((actual_6 === 0.5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, 0.5, "inside");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_6;
            valueType_6 = float64_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_23) => (structuralHash(x_23) | 0),
            })) {
                const arg_11 = (0.5).toString();
                const arg_1_6 = actual_6.toString();
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("inside");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0.5)(actual_6)("inside");
            }
            throw new Exception(errorMsg_6);
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("clampBetweenZeroAndOne", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const actual_7 = (-0.2 > 0) ? ((-0.2 < 1) ? -0.2 : 1) : 0;
        if ((actual_7 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 0, "low clamp");
        }
        else {
            let valueType_7;
            let copyOfStruct_7 = actual_7;
            valueType_7 = float64_type;
            const primitiveTypes_7 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_7;
            if (contains(valueType_7, primitiveTypes_7, {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            })) {
                const arg_12 = (0).toString();
                const arg_1_7 = actual_7.toString();
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("low clamp");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_7)("low clamp");
            }
            throw new Exception(errorMsg_7);
        }
        const actual_8 = (1.2 > 0) ? ((1.2 < 1) ? 1.2 : 1) : 0;
        if ((actual_8 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 1, "high clamp");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_8;
            valueType_8 = float64_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            })) {
                const arg_13 = (1).toString();
                const arg_1_8 = actual_8.toString();
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("high clamp");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_8)("high clamp");
            }
            throw new Exception(errorMsg_8);
        }
        const actual_9 = (0.5 > 0) ? ((0.5 < 1) ? 0.5 : 1) : 0;
        if ((actual_9 === 0.5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 0.5, "inside");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_9;
            valueType_9 = float64_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_29) => (structuralHash(x_29) | 0),
            })) {
                const arg_14 = (0.5).toString();
                const arg_1_9 = actual_9.toString();
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("inside");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0.5)(actual_9)("inside");
            }
            throw new Exception(errorMsg_9);
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("asinSafe and acosSafe clamp", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_1;
        const d_1 = (2 > -1) ? ((2 < 1) ? 2 : 1) : -1;
        a_1 = Math.asin(d_1);
        let b;
        const d_2 = (-2 > -1) ? ((-2 < 1) ? -2 : 1) : -1;
        b = Math.acos(d_2);
        Expect_floatClose(AccuracyModule_veryHigh, a_1, Math.asin(1), "asin clamp");
        Expect_floatClose(AccuracyModule_veryHigh, b, Math.acos(-1), "acos clamp");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isOne/isNotOne", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        Expect_isTrue((0.999999 < 1) && (1 < 1.000001))("1 is one");
        Expect_isTrue(!((0.999999 < 1.1) && (1.1 < 1.000001)))("1.1 not one");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isZero/isNotZero", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        Expect_isTrue((-1E-06 < 1E-07) && (1E-07 < 1E-06))("within tolerance");
        Expect_isFalse((-1E-06 < 0.0001) && (0.0001 < 1E-06))("outside tolerance");
        Expect_isTrue(!((-1E-06 < 0.001) && (0.001 < 1E-06)))("not zero");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isBetweenZeroAndOne tolerant", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        Expect_isTrue((0.5 >= 0) && (0.5 <= 1))("inside");
        Expect_isFalse((-0.01 >= 0) && (-0.01 <= 1))("outside");
        Expect_isTrue((-1E-06 < -5E-07) && (-5E-07 < 1.000001))("tolerant lower");
        Expect_isTrue((-1E-06 < 1.0000005) && (1.0000005 < 1.000001))("tolerant upper");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("matchSign", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const actual_10 = Euclid_UtilEuclid_matchSign(-5, 3);
        if ((actual_10 === -3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, -3, "match negative");
        }
        else {
            let valueType_10;
            let copyOfStruct_10 = actual_10;
            valueType_10 = float64_type;
            const primitiveTypes_10 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_10;
            if (contains(valueType_10, primitiveTypes_10, {
                Equals: equals,
                GetHashCode: (x_45) => (structuralHash(x_45) | 0),
            })) {
                const arg_15 = (-3).toString();
                const arg_1_10 = actual_10.toString();
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("match negative");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(-3)(actual_10)("match negative");
            }
            throw new Exception(errorMsg_10);
        }
        const actual_11 = Euclid_UtilEuclid_matchSign(5, -3);
        if ((actual_11 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 3, "match positive");
        }
        else {
            let valueType_11;
            let copyOfStruct_11 = actual_11;
            valueType_11 = float64_type;
            const primitiveTypes_11 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_11;
            if (contains(valueType_11, primitiveTypes_11, {
                Equals: equals,
                GetHashCode: (x_46) => (structuralHash(x_46) | 0),
            })) {
                const arg_16 = (3).toString();
                const arg_1_11 = actual_11.toString();
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("match positive");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_11)("match positive");
            }
            throw new Exception(errorMsg_11);
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("saveIdx wraps", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const arr = new Int32Array([0, 1, 2, 3]);
        const l = arr.length | 0;
        let idx1;
        const length = l | 0;
        const t = (-1 % length) | 0;
        idx1 = ((t >= 0) ? t : (t + length));
        let idx2;
        const length_1 = l | 0;
        const t_1 = (5 % length_1) | 0;
        idx2 = ((t_1 >= 0) ? t_1 : (t_1 + length_1));
        const actual_12 = item(idx1, arr) | 0;
        if ((actual_12 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 3, "neg wraps");
        }
        else {
            let valueType_12;
            let copyOfStruct_12 = actual_12;
            valueType_12 = int32_type;
            const primitiveTypes_12 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_12;
            if (contains(valueType_12, primitiveTypes_12, {
                Equals: equals,
                GetHashCode: (x_47) => (structuralHash(x_47) | 0),
            })) {
                const arg_17 = int32ToString(3);
                const arg_1_12 = int32ToString(actual_12);
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("neg wraps");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_12)("neg wraps");
            }
            throw new Exception(errorMsg_12);
        }
        const actual_13 = item(idx2, arr) | 0;
        if ((actual_13 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 1, "overflow wraps");
        }
        else {
            let valueType_13;
            let copyOfStruct_13 = actual_13;
            valueType_13 = int32_type;
            const primitiveTypes_13 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_13;
            if (contains(valueType_13, primitiveTypes_13, {
                Equals: equals,
                GetHashCode: (x_48) => (structuralHash(x_48) | 0),
            })) {
                const arg_18 = int32ToString(1);
                const arg_1_13 = int32ToString(actual_13);
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("overflow wraps");
            }
            else {
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_13)("overflow wraps");
            }
            throw new Exception(errorMsg_13);
        }
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})()]));

