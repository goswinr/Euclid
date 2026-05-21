
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { Rect2D_subDivideMinLength_Z549E38D1, Rect2D_subDivideMaxLength_Z549E38D1, Rect2D_gridMinLength_ZFDE0F31, Rect2D_gridMaxLength_ZFDE0F31, Rect2D_grid_638FCC2F, Rect2D_createFromVectors_50BD42FD } from "./Src/Rect2D.js";
import { Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Exception, int32ToString, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { singleton, contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Src/Pt.js";

export const o = Pt_$ctor_7B00E9A0(0, 0);

export const x = Vc_$ctor_7B00E9A0(1, 0);

export const y = Vc_$ctor_7B00E9A0(0, 1);

export const rect = Rect2D_createFromVectors_50BD42FD(o, x, y);

export const tests = Test_testList("Rect2D", singleton((() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rect2D.grid", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_2, b_2, vx, vy, a_4, b_4, vx_1, vy_1, a_6, b_6, vx_2, vy_2, a_8, b_8, vx_3, vy_3, a_10, b_10, vx_4, vy_4, a_12, b_12, vx_5, vy_5;
        const grid = Rect2D_grid_638FCC2F(rect, 2, 2);
        const actual_1 = grid.length | 0;
        if ((actual_1 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 2, "2-Rect2D.grid: array outer length");
        }
        else {
            let valueType;
            let copyOfStruct = actual_1;
            valueType = int32_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg = int32ToString(2);
                const arg_1 = int32ToString(actual_1);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("2-Rect2D.grid: array outer length");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_1)("2-Rect2D.grid: array outer length");
            }
            throw new Exception(errorMsg);
        }
        const actual_3 = item(0, grid).length | 0;
        if ((actual_3 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 2, "2-Rect2D.grid: array inner length");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_3;
            valueType_1 = int32_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_6 = int32ToString(2);
                const arg_1_1 = int32ToString(actual_3);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("2-Rect2D.grid: array inner length");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_3)("2-Rect2D.grid: array inner length");
            }
            throw new Exception(errorMsg_1);
        }
        Expect_isTrue(((a_2 = item(1, item(1, grid)), (b_2 = Pt_$ctor_7B00E9A0_1(1, 1), (vx = (a_2.X - b_2.X), (vy = (a_2.Y - b_2.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("2-Corner1,1");
        Expect_isTrue(((a_4 = item(0, item(0, grid)), (b_4 = Pt_$ctor_7B00E9A0_1(0, 0), (vx_1 = (a_4.X - b_4.X), (vy_1 = (a_4.Y - b_4.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("2-Corner0,0");
        const grid_1 = Rect2D_grid_638FCC2F(rect, 3, 3);
        const actual_5 = grid_1.length | 0;
        if ((actual_5 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 3, "Rect2D.grid: array outer length");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_5;
            valueType_2 = int32_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            })) {
                const arg_7 = int32ToString(3);
                const arg_1_2 = int32ToString(actual_5);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("Rect2D.grid: array outer length");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_5)("Rect2D.grid: array outer length");
            }
            throw new Exception(errorMsg_2);
        }
        const actual_7 = item(0, grid_1).length | 0;
        if ((actual_7 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 3, "Rect2D.grid: array inner length");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_7;
            valueType_3 = int32_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_8 = int32ToString(3);
                const arg_1_3 = int32ToString(actual_7);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("Rect2D.grid: array inner length");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_7)("Rect2D.grid: array inner length");
            }
            throw new Exception(errorMsg_3);
        }
        Expect_isTrue(((a_6 = item(0, item(0, grid_1)), (b_6 = Pt_$ctor_7B00E9A0_1(0, 0), (vx_2 = (a_6.X - b_6.X), (vy_2 = (a_6.Y - b_6.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09)("3-Corner0,0");
        Expect_isTrue(((a_8 = item(1, item(1, grid_1)), (b_8 = Pt_$ctor_7B00E9A0_1(0.5, 0.5), (vx_3 = (a_8.X - b_8.X), (vy_3 = (a_8.Y - b_8.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09)("3-Corner1,1");
        Expect_isTrue(((a_10 = item(2, item(2, grid_1)), (b_10 = Pt_$ctor_7B00E9A0_1(1, 1), (vx_4 = (a_10.X - b_10.X), (vy_4 = (a_10.Y - b_10.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) < 1E-09)("3-Corner2,2");
        Expect_isTrue(((a_12 = item(2, item(0, grid_1)), (b_12 = Pt_$ctor_7B00E9A0_1(0, 1), (vx_5 = (a_12.X - b_12.X), (vy_5 = (a_12.Y - b_12.Y), Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5))))))) < 1E-09)("3-Corner0,0");
        const grid_2 = Rect2D_gridMaxLength_ZFDE0F31(rect, 0.4, 0.4);
        const actual_9 = grid_2.length | 0;
        if ((actual_9 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 4, "Rect2D.gridMaxLength: 0.4 -> 4 points");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_9;
            valueType_4 = int32_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_9 = int32ToString(4);
                const arg_1_4 = int32ToString(actual_9);
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("Rect2D.gridMaxLength: 0.4 -> 4 points");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_9)("Rect2D.gridMaxLength: 0.4 -> 4 points");
            }
            throw new Exception(errorMsg_4);
        }
        const grid_3 = Rect2D_gridMaxLength_ZFDE0F31(rect, 0.5, 0.5);
        const actual_11 = grid_3.length | 0;
        if ((actual_11 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 3, "Rect2D.gridMaxLength: 0.5 -> 3 points");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_11;
            valueType_5 = int32_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg_10 = int32ToString(3);
                const arg_1_5 = int32ToString(actual_11);
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("Rect2D.gridMaxLength: 0.5 -> 3 points");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_11)("Rect2D.gridMaxLength: 0.5 -> 3 points");
            }
            throw new Exception(errorMsg_5);
        }
        const grid_4 = Rect2D_gridMaxLength_ZFDE0F31(rect, 0.6, 0.6);
        const actual_13 = grid_4.length | 0;
        if ((actual_13 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 3, "Rect2D.gridMaxLength: 0.6 -> 3 points");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_13;
            valueType_6 = int32_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            })) {
                const arg_11 = int32ToString(3);
                const arg_1_6 = int32ToString(actual_13);
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("Rect2D.gridMaxLength: 0.6 -> 3 points");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_13)("Rect2D.gridMaxLength: 0.6 -> 3 points");
            }
            throw new Exception(errorMsg_6);
        }
        const grid_5 = Rect2D_gridMinLength_ZFDE0F31(rect, 0.4, 0.4);
        const actual_15 = grid_5.length | 0;
        if ((actual_15 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, 3, "Rect2D.gridMinLength: 1/0.4 -> 3 points");
        }
        else {
            let valueType_7;
            let copyOfStruct_7 = actual_15;
            valueType_7 = int32_type;
            const primitiveTypes_7 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_7;
            if (contains(valueType_7, primitiveTypes_7, {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            })) {
                const arg_12 = int32ToString(3);
                const arg_1_7 = int32ToString(actual_15);
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("Rect2D.gridMinLength: 1/0.4 -> 3 points");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_15)("Rect2D.gridMinLength: 1/0.4 -> 3 points");
            }
            throw new Exception(errorMsg_7);
        }
        const grid_6 = Rect2D_gridMinLength_ZFDE0F31(rect, 0.5, 0.5);
        const actual_17 = grid_6.length | 0;
        if ((actual_17 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, 3, "Rect2D.gridMinLength: 1/0.5 -> 3 points");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_17;
            valueType_8 = int32_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            })) {
                const arg_13 = int32ToString(3);
                const arg_1_8 = int32ToString(actual_17);
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("Rect2D.gridMinLength: 1/0.5 -> 3 points");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_17)("Rect2D.gridMinLength: 1/0.5 -> 3 points");
            }
            throw new Exception(errorMsg_8);
        }
        const grid_7 = Rect2D_gridMinLength_ZFDE0F31(rect, 0.6, 0.6);
        const actual_19 = grid_7.length | 0;
        if ((actual_19 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, 2, "Rect2D.gridMinLength: 1/0.6 -> 2 points");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_19;
            valueType_9 = int32_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_10) => (structuralHash(x_10) | 0),
            })) {
                const arg_14 = int32ToString(2);
                const arg_1_9 = int32ToString(actual_19);
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("Rect2D.gridMinLength: 1/0.6 -> 2 points");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_19)("Rect2D.gridMinLength: 1/0.6 -> 2 points");
            }
            throw new Exception(errorMsg_9);
        }
        const grid_8 = Rect2D_subDivideMaxLength_Z549E38D1(rect, 0.4, 0.4, 0, 0);
        const actual_21 = grid_8.length | 0;
        if ((actual_21 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 3, "Rect2D.subDivideMaxLength: 0.4 -> 3 rects");
        }
        else {
            let valueType_10;
            let copyOfStruct_10 = actual_21;
            valueType_10 = int32_type;
            const primitiveTypes_10 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_10;
            if (contains(valueType_10, primitiveTypes_10, {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            })) {
                const arg_15 = int32ToString(3);
                const arg_1_10 = int32ToString(actual_21);
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("Rect2D.subDivideMaxLength: 0.4 -> 3 rects");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_21)("Rect2D.subDivideMaxLength: 0.4 -> 3 rects");
            }
            throw new Exception(errorMsg_10);
        }
        const grid_9 = Rect2D_subDivideMaxLength_Z549E38D1(rect, 0.5, 0.5, 0, 0);
        const actual_23 = grid_9.length | 0;
        if ((actual_23 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, 2, "Rect2D.subDivideMaxLength: 0.5 -> 2 rects");
        }
        else {
            let valueType_11;
            let copyOfStruct_11 = actual_23;
            valueType_11 = int32_type;
            const primitiveTypes_11 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_11;
            if (contains(valueType_11, primitiveTypes_11, {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            })) {
                const arg_16 = int32ToString(2);
                const arg_1_11 = int32ToString(actual_23);
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("Rect2D.subDivideMaxLength: 0.5 -> 2 rects");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_23)("Rect2D.subDivideMaxLength: 0.5 -> 2 rects");
            }
            throw new Exception(errorMsg_11);
        }
        const grid_10 = Rect2D_subDivideMaxLength_Z549E38D1(rect, 0.6, 0.6, 0, 0);
        const actual_25 = grid_10.length | 0;
        if ((actual_25 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, 2, "Rect2D.subDivideMaxLength: 0.6 -> 2 rects");
        }
        else {
            let valueType_12;
            let copyOfStruct_12 = actual_25;
            valueType_12 = int32_type;
            const primitiveTypes_12 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_12;
            if (contains(valueType_12, primitiveTypes_12, {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            })) {
                const arg_17 = int32ToString(2);
                const arg_1_12 = int32ToString(actual_25);
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("Rect2D.subDivideMaxLength: 0.6 -> 2 rects");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_25)("Rect2D.subDivideMaxLength: 0.6 -> 2 rects");
            }
            throw new Exception(errorMsg_12);
        }
        const grid_11 = Rect2D_subDivideMinLength_Z549E38D1(rect, 0.4, 0.4, 0, 0);
        const actual_27 = grid_11.length | 0;
        if ((actual_27 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, 2, "Rect2D.subDivideMinLength: 1/0.4 -> 2 rects");
        }
        else {
            let valueType_13;
            let copyOfStruct_13 = actual_27;
            valueType_13 = int32_type;
            const primitiveTypes_13 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_13;
            if (contains(valueType_13, primitiveTypes_13, {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            })) {
                const arg_18 = int32ToString(2);
                const arg_1_13 = int32ToString(actual_27);
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("Rect2D.subDivideMinLength: 1/0.4 -> 2 rects");
            }
            else {
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_27)("Rect2D.subDivideMinLength: 1/0.4 -> 2 rects");
            }
            throw new Exception(errorMsg_13);
        }
        const grid_12 = Rect2D_subDivideMinLength_Z549E38D1(rect, 0.5, 0.5, 0, 0);
        const actual_29 = grid_12.length | 0;
        if ((actual_29 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, 2, "Rect2D.subDivideMinLength: 1/0.5 -> 2 rects");
        }
        else {
            let valueType_14;
            let copyOfStruct_14 = actual_29;
            valueType_14 = int32_type;
            const primitiveTypes_14 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_14;
            if (contains(valueType_14, primitiveTypes_14, {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            })) {
                const arg_19 = int32ToString(2);
                const arg_1_14 = int32ToString(actual_29);
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("Rect2D.subDivideMinLength: 1/0.5 -> 2 rects");
            }
            else {
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_29)("Rect2D.subDivideMinLength: 1/0.5 -> 2 rects");
            }
            throw new Exception(errorMsg_14);
        }
        const grid_13 = Rect2D_subDivideMinLength_Z549E38D1(rect, 0.6, 0.6, 0, 0);
        const actual_31 = grid_13.length | 0;
        if ((actual_31 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, 1, "Rect2D.subDivideMinLength: 1/0.6 -> 1 rects");
        }
        else {
            let valueType_15;
            let copyOfStruct_15 = actual_31;
            valueType_15 = int32_type;
            const primitiveTypes_15 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_15;
            if (contains(valueType_15, primitiveTypes_15, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_20 = int32ToString(1);
                const arg_1_15 = int32ToString(actual_31);
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("Rect2D.subDivideMinLength: 1/0.6 -> 1 rects");
            }
            else {
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_31)("Rect2D.subDivideMinLength: 1/0.6 -> 1 rects");
            }
            throw new Exception(errorMsg_15);
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})()));

