
import { Test_failtest, Expect_isFalse, Expect_throws, Expect_isTrue, Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { BRect__get_AsString, BRect_$ctor_77D16AC0 } from "./Src/BRect.js";
import { defaultOf, int32ToString, disposeSafe, getEnumerator, Exception, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, bool_type, int32_type, float64_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { singleton, contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Pt__get_AsString, Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Src/Pt.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { Vc_$ctor_7B00E9A0 as Vc_$ctor_7B00E9A0_1 } from "./Src/Vc.js";
import { Operators_IsNull } from "./fable_modules/fable-library-js.5.0.0/FSharp.Core.js";
import { fail, failEmptySeq, failNull } from "./Src/EuclidErrors.js";
import { empty, isEmpty } from "./fable_modules/fable-library-js.5.0.0/Seq.js";
import { max, min } from "./fable_modules/fable-library-js.5.0.0/Double.js";
import { count } from "./fable_modules/fable-library-js.5.0.0/CollectionUtil.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { Line2D_$ctor_77D16AC0 } from "./Src/Line2D.js";
import { BRect__get_AsFSharpCode, BRect__get_AsString as BRect__get_AsString_1, BRect__get_Edge30, BRect__get_Edge23, BRect__get_Edge12, BRect__get_Edge01, BRect__get_PointsLooped, BRect__get_Points, BRect__get_Pt3, BRect__get_Pt2, BRect__get_Pt1, BRect__get_Pt0, BRect_expandRelXY, BRect_expandRel, BRect__IsTouching_13C371E0 } from "./Src/BRect.js";
import { toString } from "./fable_modules/fable-library-js.5.0.0/Types.js";

export const tests = Test_testList("BRect", ofArray([Test_testList("Constructor and Basic Properties", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create from two points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let r;
        const a = Pt_$ctor_7B00E9A0(0, 0);
        const b = Pt_$ctor_7B00E9A0(10, 5);
        let minX = a.X;
        let maxX;
        if (b.X > minX) {
            maxX = b.X;
        }
        else {
            minX = b.X;
            maxX = a.X;
        }
        let minY = a.Y;
        let maxY;
        if (b.Y > minY) {
            maxY = b.Y;
        }
        else {
            minY = b.Y;
            maxY = a.Y;
        }
        r = BRect_$ctor_77D16AC0(minX, minY, maxX, maxY);
        const actual = r.MinX;
        if ((actual === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 0, "MinX should be 0");
        }
        else {
            let valueType;
            let copyOfStruct = actual;
            valueType = float64_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            })) {
                const arg = (0).toString();
                const arg_1 = actual.toString();
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("MinX should be 0");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual)("MinX should be 0");
            }
            throw new Exception(errorMsg);
        }
        const actual_1 = r.MinY;
        if ((actual_1 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 0, "MinY should be 0");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_1;
            valueType_1 = float64_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg_6 = (0).toString();
                const arg_1_1 = actual_1.toString();
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("MinY should be 0");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_1)("MinY should be 0");
            }
            throw new Exception(errorMsg_1);
        }
        const actual_2 = r.MaxX;
        if ((actual_2 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 10, "MaxX should be 10");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_2;
            valueType_2 = float64_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_7 = (10).toString();
                const arg_1_2 = actual_2.toString();
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("MaxX should be 10");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_2)("MaxX should be 10");
            }
            throw new Exception(errorMsg_2);
        }
        const actual_3 = r.MaxY;
        if ((actual_3 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 5, "MaxY should be 5");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_3;
            valueType_3 = float64_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            })) {
                const arg_8 = (5).toString();
                const arg_1_3 = actual_3.toString();
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("MaxY should be 5");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_3)("MaxY should be 5");
            }
            throw new Exception(errorMsg_3);
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create from two points with swapped coordinates", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let r_1;
        const a_1 = Pt_$ctor_7B00E9A0(10, 5);
        const b_1 = Pt_$ctor_7B00E9A0(0, 0);
        let minX_2 = a_1.X;
        let maxX_2;
        if (b_1.X > minX_2) {
            maxX_2 = b_1.X;
        }
        else {
            minX_2 = b_1.X;
            maxX_2 = a_1.X;
        }
        let minY_2 = a_1.Y;
        let maxY_2;
        if (b_1.Y > minY_2) {
            maxY_2 = b_1.Y;
        }
        else {
            minY_2 = b_1.Y;
            maxY_2 = a_1.Y;
        }
        r_1 = BRect_$ctor_77D16AC0(minX_2, minY_2, maxX_2, maxY_2);
        const actual_4 = r_1.MinX;
        if ((actual_4 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, 0, "MinX should be 0 after sorting");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_4;
            valueType_4 = float64_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_9 = (0).toString();
                const arg_1_4 = actual_4.toString();
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("MinX should be 0 after sorting");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_4)("MinX should be 0 after sorting");
            }
            throw new Exception(errorMsg_4);
        }
        const actual_5 = r_1.MinY;
        if ((actual_5 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 0, "MinY should be 0 after sorting");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_5;
            valueType_5 = float64_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_10 = (0).toString();
                const arg_1_5 = actual_5.toString();
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("MinY should be 0 after sorting");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_5)("MinY should be 0 after sorting");
            }
            throw new Exception(errorMsg_5);
        }
        const actual_6 = r_1.MaxX;
        if ((actual_6 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, 10, "MaxX should be 10 after sorting");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_6;
            valueType_6 = float64_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg_11 = (10).toString();
                const arg_1_6 = actual_6.toString();
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("MaxX should be 10 after sorting");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_6)("MaxX should be 10 after sorting");
            }
            throw new Exception(errorMsg_6);
        }
        const actual_7 = r_1.MaxY;
        if ((actual_7 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 5, "MaxY should be 5 after sorting");
        }
        else {
            let valueType_7;
            let copyOfStruct_7 = actual_7;
            valueType_7 = float64_type;
            const primitiveTypes_7 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_7;
            if (contains(valueType_7, primitiveTypes_7, {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            })) {
                const arg_12 = (5).toString();
                const arg_1_7 = actual_7.toString();
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("MaxY should be 5 after sorting");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_7)("MaxY should be 5 after sorting");
            }
            throw new Exception(errorMsg_7);
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MinPt and MaxPt properties", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_4, r_3, b_4, vx, vy, a_6, r_4, b_6, vx_1, vy_1;
        let r_2;
        const a_2 = Pt_$ctor_7B00E9A0(1, 2);
        const b_2 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_4 = a_2.X;
        let maxX_4;
        if (b_2.X > minX_4) {
            maxX_4 = b_2.X;
        }
        else {
            minX_4 = b_2.X;
            maxX_4 = a_2.X;
        }
        let minY_4 = a_2.Y;
        let maxY_4;
        if (b_2.Y > minY_4) {
            maxY_4 = b_2.Y;
        }
        else {
            minY_4 = b_2.Y;
            maxY_4 = a_2.Y;
        }
        r_2 = BRect_$ctor_77D16AC0(minX_4, minY_4, maxX_4, maxY_4);
        Expect_isTrue(((a_4 = ((r_3 = r_2, Pt_$ctor_7B00E9A0_1(r_3.MinX, r_3.MinY))), (b_4 = Pt_$ctor_7B00E9A0(1, 2), (vx = (a_4.X - b_4.X), (vy = (a_4.Y - b_4.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("MinPt should be (1, 2)");
        Expect_isTrue(((a_6 = ((r_4 = r_2, Pt_$ctor_7B00E9A0_1(r_4.MaxX, r_4.MaxY))), (b_6 = Pt_$ctor_7B00E9A0(5, 7), (vx_1 = (a_6.X - b_6.X), (vy_1 = (a_6.Y - b_6.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("MaxPt should be (5, 7)");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SizeX and SizeY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let r_5;
        const a_7 = Pt_$ctor_7B00E9A0(1, 2);
        const b_7 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_6 = a_7.X;
        let maxX_6;
        if (b_7.X > minX_6) {
            maxX_6 = b_7.X;
        }
        else {
            minX_6 = b_7.X;
            maxX_6 = a_7.X;
        }
        let minY_6 = a_7.Y;
        let maxY_6;
        if (b_7.Y > minY_6) {
            maxY_6 = b_7.Y;
        }
        else {
            minY_6 = b_7.Y;
            maxY_6 = a_7.Y;
        }
        r_5 = BRect_$ctor_77D16AC0(minX_6, minY_6, maxX_6, maxY_6);
        let actual_8;
        const r_6 = r_5;
        actual_8 = (r_6.MaxX - r_6.MinX);
        if ((actual_8 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 4, "SizeX should be 4");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_8;
            valueType_8 = float64_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            })) {
                const arg_13 = (4).toString();
                const arg_1_8 = actual_8.toString();
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("SizeX should be 4");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_8)("SizeX should be 4");
            }
            throw new Exception(errorMsg_8);
        }
        let actual_9;
        const r_7 = r_5;
        actual_9 = (r_7.MaxY - r_7.MinY);
        if ((actual_9 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 5, "SizeY should be 5");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_9;
            valueType_9 = float64_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            })) {
                const arg_14 = (5).toString();
                const arg_1_9 = actual_9.toString();
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("SizeY should be 5");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_9)("SizeY should be 5");
            }
            throw new Exception(errorMsg_9);
        }
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_10, r_9, b_10, vx_2, vy_2;
        let r_8;
        const a_8 = Pt_$ctor_7B00E9A0(0, 0);
        const b_8 = Pt_$ctor_7B00E9A0(10, 6);
        let minX_8 = a_8.X;
        let maxX_8;
        if (b_8.X > minX_8) {
            maxX_8 = b_8.X;
        }
        else {
            minX_8 = b_8.X;
            maxX_8 = a_8.X;
        }
        let minY_8 = a_8.Y;
        let maxY_8;
        if (b_8.Y > minY_8) {
            maxY_8 = b_8.Y;
        }
        else {
            minY_8 = b_8.Y;
            maxY_8 = a_8.Y;
        }
        r_8 = BRect_$ctor_77D16AC0(minX_8, minY_8, maxX_8, maxY_8);
        Expect_isTrue(((a_10 = ((r_9 = r_8, Pt_$ctor_7B00E9A0_1((r_9.MaxX + r_9.MinX) * 0.5, (r_9.MaxY + r_9.MinY) * 0.5))), (b_10 = Pt_$ctor_7B00E9A0(5, 3), (vx_2 = (a_10.X - b_10.X), (vy_2 = (a_10.Y - b_10.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09)("Center should be (5, 3)");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let v_1, a_12, r_11, b_12, x_10, y_10;
        let r_10;
        const a_11 = Pt_$ctor_7B00E9A0(1, 2);
        const b_11 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_10 = a_11.X;
        let maxX_10;
        if (b_11.X > minX_10) {
            maxX_10 = b_11.X;
        }
        else {
            minX_10 = b_11.X;
            maxX_10 = a_11.X;
        }
        let minY_10 = a_11.Y;
        let maxY_10;
        if (b_11.Y > minY_10) {
            maxY_10 = b_11.Y;
        }
        else {
            minY_10 = b_11.Y;
            maxY_10 = a_11.Y;
        }
        r_10 = BRect_$ctor_77D16AC0(minX_10, minY_10, maxX_10, maxY_10);
        Expect_isTrue(((v_1 = ((a_12 = ((r_11 = r_10, Vc_$ctor_7B00E9A0(r_11.MaxX - r_11.MinX, r_11.MaxY - r_11.MinY))), (b_12 = Vc_$ctor_7B00E9A0_1(4, 5), Vc_$ctor_7B00E9A0(a_12.X - b_12.X, a_12.Y - b_12.Y)))), (x_10 = v_1.X, (y_10 = v_1.Y, Math.sqrt((x_10 * x_10) + (y_10 * y_10)))))) < 1E-09)("Diagonal should be (4, 5)");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Area", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let r_14, r_15;
        let r_12;
        const a_14 = Pt_$ctor_7B00E9A0(0, 0);
        const b_14 = Pt_$ctor_7B00E9A0(10, 5);
        let minX_12 = a_14.X;
        let maxX_12;
        if (b_14.X > minX_12) {
            maxX_12 = b_14.X;
        }
        else {
            minX_12 = b_14.X;
            maxX_12 = a_14.X;
        }
        let minY_12 = a_14.Y;
        let maxY_12;
        if (b_14.Y > minY_12) {
            maxY_12 = b_14.Y;
        }
        else {
            minY_12 = b_14.Y;
            maxY_12 = a_14.Y;
        }
        r_12 = BRect_$ctor_77D16AC0(minX_12, minY_12, maxX_12, maxY_12);
        let actual_10;
        const r_13 = r_12;
        actual_10 = (((r_14 = r_13, r_14.MaxX - r_14.MinX)) * ((r_15 = r_13, r_15.MaxY - r_15.MinY)));
        if ((actual_10 === 50) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 50, "Area should be 50");
        }
        else {
            let valueType_10;
            let copyOfStruct_10 = actual_10;
            valueType_10 = float64_type;
            const primitiveTypes_10 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_10;
            if (contains(valueType_10, primitiveTypes_10, {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            })) {
                const arg_15 = (50).toString();
                const arg_1_10 = actual_10.toString();
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("Area should be 50");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(50)(actual_10)("Area should be 50");
            }
            throw new Exception(errorMsg_10);
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})()])), Test_testList("Creation Methods", ofArray([(() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromSeq with valid points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const pts = ofArray([Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(5, 3), Pt_$ctor_7B00E9A0(2, 7), Pt_$ctor_7B00E9A0(8, 1)]);
        let r_16;
        const ps = pts;
        if (Operators_IsNull(ps)) {
            failNull("BRect.createFromSeq", "seq<Pt>");
        }
        if (isEmpty(ps)) {
            failEmptySeq("BRect.createFromSeq", "seq<Pt>");
        }
        let minX_14 = 1.7976931348623157E+308;
        let minY_14 = 1.7976931348623157E+308;
        let maxX_14 = -1.7976931348623157E+308;
        let maxY_14 = -1.7976931348623157E+308;
        const enumerator = getEnumerator(ps);
        try {
            while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
                const p = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
                minX_14 = min(minX_14, p.X);
                minY_14 = min(minY_14, p.Y);
                maxX_14 = max(maxX_14, p.X);
                maxY_14 = max(maxY_14, p.Y);
            }
        }
        finally {
            disposeSafe(enumerator);
        }
        r_16 = BRect_$ctor_77D16AC0(minX_14, minY_14, maxX_14, maxY_14);
        const actual_11 = r_16.MinX;
        if ((actual_11 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 1, "MinX should be 1");
        }
        else {
            let valueType_11;
            let copyOfStruct_11 = actual_11;
            valueType_11 = float64_type;
            const primitiveTypes_11 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_11;
            if (contains(valueType_11, primitiveTypes_11, {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            })) {
                const arg_16 = (1).toString();
                const arg_1_11 = actual_11.toString();
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("MinX should be 1");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_11)("MinX should be 1");
            }
            throw new Exception(errorMsg_11);
        }
        const actual_12 = r_16.MinY;
        if ((actual_12 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 1, "MinY should be 1");
        }
        else {
            let valueType_12;
            let copyOfStruct_12 = actual_12;
            valueType_12 = float64_type;
            const primitiveTypes_12 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_12;
            if (contains(valueType_12, primitiveTypes_12, {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            })) {
                const arg_17 = (1).toString();
                const arg_1_12 = actual_12.toString();
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("MinY should be 1");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_12)("MinY should be 1");
            }
            throw new Exception(errorMsg_12);
        }
        const actual_13 = r_16.MaxX;
        if ((actual_13 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 8, "MaxX should be 8");
        }
        else {
            let valueType_13;
            let copyOfStruct_13 = actual_13;
            valueType_13 = float64_type;
            const primitiveTypes_13 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_13;
            if (contains(valueType_13, primitiveTypes_13, {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            })) {
                const arg_18 = (8).toString();
                const arg_1_13 = actual_13.toString();
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("MaxX should be 8");
            }
            else {
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_13)("MaxX should be 8");
            }
            throw new Exception(errorMsg_13);
        }
        const actual_14 = r_16.MaxY;
        if ((actual_14 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, 7, "MaxY should be 7");
        }
        else {
            let valueType_14;
            let copyOfStruct_14 = actual_14;
            valueType_14 = float64_type;
            const primitiveTypes_14 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_14;
            if (contains(valueType_14, primitiveTypes_14, {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            })) {
                const arg_19 = (7).toString();
                const arg_1_14 = actual_14.toString();
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("MaxY should be 7");
            }
            else {
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_14)("MaxY should be 7");
            }
            throw new Exception(errorMsg_14);
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromSeq with single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const pts_1 = singleton(Pt_$ctor_7B00E9A0(5, 3));
        let r_17;
        const ps_1 = pts_1;
        if (Operators_IsNull(ps_1)) {
            failNull("BRect.createFromSeq", "seq<Pt>");
        }
        if (isEmpty(ps_1)) {
            failEmptySeq("BRect.createFromSeq", "seq<Pt>");
        }
        let minX_16 = 1.7976931348623157E+308;
        let minY_16 = 1.7976931348623157E+308;
        let maxX_16 = -1.7976931348623157E+308;
        let maxY_16 = -1.7976931348623157E+308;
        const enumerator_1 = getEnumerator(ps_1);
        try {
            while (enumerator_1["System.Collections.IEnumerator.MoveNext"]()) {
                const p_1 = enumerator_1["System.Collections.Generic.IEnumerator`1.get_Current"]();
                minX_16 = min(minX_16, p_1.X);
                minY_16 = min(minY_16, p_1.Y);
                maxX_16 = max(maxX_16, p_1.X);
                maxY_16 = max(maxY_16, p_1.Y);
            }
        }
        finally {
            disposeSafe(enumerator_1);
        }
        r_17 = BRect_$ctor_77D16AC0(minX_16, minY_16, maxX_16, maxY_16);
        const actual_15 = r_17.MinX;
        if ((actual_15 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, 5, "MinX should be 5");
        }
        else {
            let valueType_15;
            let copyOfStruct_15 = actual_15;
            valueType_15 = float64_type;
            const primitiveTypes_15 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_15;
            if (contains(valueType_15, primitiveTypes_15, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_20 = (5).toString();
                const arg_1_15 = actual_15.toString();
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("MinX should be 5");
            }
            else {
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_15)("MinX should be 5");
            }
            throw new Exception(errorMsg_15);
        }
        const actual_16 = r_17.MinY;
        if ((actual_16 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, 3, "MinY should be 3");
        }
        else {
            let valueType_16;
            let copyOfStruct_16 = actual_16;
            valueType_16 = float64_type;
            const primitiveTypes_16 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_16;
            if (contains(valueType_16, primitiveTypes_16, {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            })) {
                const arg_21 = (3).toString();
                const arg_1_16 = actual_16.toString();
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("MinY should be 3");
            }
            else {
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_16)("MinY should be 3");
            }
            throw new Exception(errorMsg_16);
        }
        const actual_17 = r_17.MaxX;
        if ((actual_17 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, 5, "MaxX should be 5");
        }
        else {
            let valueType_17;
            let copyOfStruct_17 = actual_17;
            valueType_17 = float64_type;
            const primitiveTypes_17 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_17;
            if (contains(valueType_17, primitiveTypes_17, {
                Equals: equals,
                GetHashCode: (x_18) => (structuralHash(x_18) | 0),
            })) {
                const arg_22 = (5).toString();
                const arg_1_17 = actual_17.toString();
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("MaxX should be 5");
            }
            else {
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_17)("MaxX should be 5");
            }
            throw new Exception(errorMsg_17);
        }
        const actual_18 = r_17.MaxY;
        if ((actual_18 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, 3, "MaxY should be 3");
        }
        else {
            let valueType_18;
            let copyOfStruct_18 = actual_18;
            valueType_18 = float64_type;
            const primitiveTypes_18 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_18;
            if (contains(valueType_18, primitiveTypes_18, {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            })) {
                const arg_23 = (3).toString();
                const arg_1_18 = actual_18.toString();
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("MaxY should be 3");
            }
            else {
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_18)("MaxY should be 3");
            }
            throw new Exception(errorMsg_18);
        }
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromIList with valid points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const pts_2 = [Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(5, 3), Pt_$ctor_7B00E9A0(2, 7)];
        let r_18;
        const ps_2 = pts_2;
        if (Operators_IsNull(ps_2)) {
            failNull("BRect.createFromIList", "IList<Pt>");
        }
        if (count(ps_2) === 0) {
            failEmptySeq("BRect.createFromIList", "IList<Pt>");
        }
        let minX_18 = 1.7976931348623157E+308;
        let minY_18 = 1.7976931348623157E+308;
        let maxX_18 = -1.7976931348623157E+308;
        let maxY_18 = -1.7976931348623157E+308;
        for (let i = 0; i <= (count(ps_2) - 1); i++) {
            const p_2 = item(i, ps_2);
            minX_18 = min(minX_18, p_2.X);
            minY_18 = min(minY_18, p_2.Y);
            maxX_18 = max(maxX_18, p_2.X);
            maxY_18 = max(maxY_18, p_2.Y);
        }
        r_18 = BRect_$ctor_77D16AC0(minX_18, minY_18, maxX_18, maxY_18);
        const actual_19 = r_18.MinX;
        if ((actual_19 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, 1, "MinX should be 1");
        }
        else {
            let valueType_19;
            let copyOfStruct_19 = actual_19;
            valueType_19 = float64_type;
            const primitiveTypes_19 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_19;
            if (contains(valueType_19, primitiveTypes_19, {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            })) {
                const arg_24 = (1).toString();
                const arg_1_19 = actual_19.toString();
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("MinX should be 1");
            }
            else {
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_19)("MinX should be 1");
            }
            throw new Exception(errorMsg_19);
        }
        const actual_20 = r_18.MaxY;
        if ((actual_20 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, 7, "MaxY should be 7");
        }
        else {
            let valueType_20;
            let copyOfStruct_20 = actual_20;
            valueType_20 = float64_type;
            const primitiveTypes_20 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_20;
            if (contains(valueType_20, primitiveTypes_20, {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            })) {
                const arg_25 = (7).toString();
                const arg_1_20 = actual_20.toString();
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("MaxY should be 7");
            }
            else {
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_20)("MaxY should be 7");
            }
            throw new Exception(errorMsg_20);
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromCenter with valid size", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let r_19;
        const center = Pt_$ctor_7B00E9A0(5, 5);
        if (!(10 >= 0)) {
            fail(`BRect.createFromCenter sizeX is negative: ${10}, sizeY is: ${6}, center: ${Pt__get_AsString(center)}`);
        }
        if (!(6 >= 0)) {
            fail(`BRect.createFromCenter sizeY is negative: ${6}, sizeX is: ${10}, center: ${Pt__get_AsString(center)}`);
        }
        const minX_20 = center.X - (10 * 0.5);
        const minY_20 = center.Y - (6 * 0.5);
        const maxX_20 = center.X + (10 * 0.5);
        const maxY_20 = center.Y + (6 * 0.5);
        r_19 = BRect_$ctor_77D16AC0(minX_20, minY_20, maxX_20, maxY_20);
        const actual_21 = r_19.MinX;
        if ((actual_21 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 0, "MinX should be 0");
        }
        else {
            let valueType_21;
            let copyOfStruct_21 = actual_21;
            valueType_21 = float64_type;
            const primitiveTypes_21 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_21;
            if (contains(valueType_21, primitiveTypes_21, {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            })) {
                const arg_26 = (0).toString();
                const arg_1_21 = actual_21.toString();
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("MinX should be 0");
            }
            else {
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_21)("MinX should be 0");
            }
            throw new Exception(errorMsg_21);
        }
        const actual_22 = r_19.MinY;
        if ((actual_22 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_22, 2, "MinY should be 2");
        }
        else {
            let valueType_22;
            let copyOfStruct_22 = actual_22;
            valueType_22 = float64_type;
            const primitiveTypes_22 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_22;
            if (contains(valueType_22, primitiveTypes_22, {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            })) {
                const arg_27 = (2).toString();
                const arg_1_22 = actual_22.toString();
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("MinY should be 2");
            }
            else {
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_22)("MinY should be 2");
            }
            throw new Exception(errorMsg_22);
        }
        const actual_23 = r_19.MaxX;
        if ((actual_23 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, 10, "MaxX should be 10");
        }
        else {
            let valueType_23;
            let copyOfStruct_23 = actual_23;
            valueType_23 = float64_type;
            const primitiveTypes_23 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_23;
            if (contains(valueType_23, primitiveTypes_23, {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            })) {
                const arg_28 = (10).toString();
                const arg_1_23 = actual_23.toString();
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("MaxX should be 10");
            }
            else {
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_23)("MaxX should be 10");
            }
            throw new Exception(errorMsg_23);
        }
        const actual_24 = r_19.MaxY;
        if ((actual_24 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, 8, "MaxY should be 8");
        }
        else {
            let valueType_24;
            let copyOfStruct_24 = actual_24;
            valueType_24 = float64_type;
            const primitiveTypes_24 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_24;
            if (contains(valueType_24, primitiveTypes_24, {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            })) {
                const arg_29 = (8).toString();
                const arg_1_24 = actual_24.toString();
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("MaxY should be 8");
            }
            else {
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_24)("MaxY should be 8");
            }
            throw new Exception(errorMsg_24);
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromCenter rejects negative sizeX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        Expect_throws(() => {
            let center_1, minX_22, minY_22, maxX_22, maxY_22;
            (center_1 = Pt_$ctor_7B00E9A0(5, 5), (!(-10 >= 0) ? fail(`BRect.createFromCenter sizeX is negative: ${-10}, sizeY is: ${6}, center: ${Pt__get_AsString(center_1)}`) : undefined, (!(6 >= 0) ? fail(`BRect.createFromCenter sizeY is negative: ${6}, sizeX is: ${-10}, center: ${Pt__get_AsString(center_1)}`) : undefined, (minX_22 = (center_1.X - (-10 * 0.5)), (minY_22 = (center_1.Y - (6 * 0.5)), (maxX_22 = (center_1.X + (-10 * 0.5)), (maxY_22 = (center_1.Y + (6 * 0.5)), BRect_$ctor_77D16AC0(minX_22, minY_22, maxX_22, maxY_22))))))));
        }, "Should throw on negative sizeX");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromCenter rejects negative sizeY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        Expect_throws(() => {
            let center_2, minX_24, minY_24, maxX_24, maxY_24;
            (center_2 = Pt_$ctor_7B00E9A0(5, 5), (!(10 >= 0) ? fail(`BRect.createFromCenter sizeX is negative: ${10}, sizeY is: ${-6}, center: ${Pt__get_AsString(center_2)}`) : undefined, (!(-6 >= 0) ? fail(`BRect.createFromCenter sizeY is negative: ${-6}, sizeX is: ${10}, center: ${Pt__get_AsString(center_2)}`) : undefined, (minX_24 = (center_2.X - (10 * 0.5)), (minY_24 = (center_2.Y - (-6 * 0.5)), (maxX_24 = (center_2.X + (10 * 0.5)), (maxY_24 = (center_2.Y + (-6 * 0.5)), BRect_$ctor_77D16AC0(minX_24, minY_24, maxX_24, maxY_24))))))));
        }, "Should throw on negative sizeY");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromLine", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const line = Line2D_$ctor_77D16AC0(1, 2, 5, 7);
        let r_20;
        const l = line;
        const minX_26 = min(l.FromX, l.ToX);
        const maxX_26 = max(l.FromX, l.ToX);
        const minY_26 = min(l.FromY, l.ToY);
        const maxY_26 = max(l.FromY, l.ToY);
        r_20 = BRect_$ctor_77D16AC0(minX_26, minY_26, maxX_26, maxY_26);
        const actual_25 = r_20.MinX;
        if ((actual_25 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, 1, "MinX should be 1");
        }
        else {
            let valueType_25;
            let copyOfStruct_25 = actual_25;
            valueType_25 = float64_type;
            const primitiveTypes_25 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_25;
            if (contains(valueType_25, primitiveTypes_25, {
                Equals: equals,
                GetHashCode: (x_32) => (structuralHash(x_32) | 0),
            })) {
                const arg_30 = (1).toString();
                const arg_1_25 = actual_25.toString();
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("MinX should be 1");
            }
            else {
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_25)("MinX should be 1");
            }
            throw new Exception(errorMsg_25);
        }
        const actual_26 = r_20.MinY;
        if ((actual_26 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, 2, "MinY should be 2");
        }
        else {
            let valueType_26;
            let copyOfStruct_26 = actual_26;
            valueType_26 = float64_type;
            const primitiveTypes_26 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_26;
            if (contains(valueType_26, primitiveTypes_26, {
                Equals: equals,
                GetHashCode: (x_33) => (structuralHash(x_33) | 0),
            })) {
                const arg_31 = (2).toString();
                const arg_1_26 = actual_26.toString();
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("MinY should be 2");
            }
            else {
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_26)("MinY should be 2");
            }
            throw new Exception(errorMsg_26);
        }
        const actual_27 = r_20.MaxX;
        if ((actual_27 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, 5, "MaxX should be 5");
        }
        else {
            let valueType_27;
            let copyOfStruct_27 = actual_27;
            valueType_27 = float64_type;
            const primitiveTypes_27 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_27;
            if (contains(valueType_27, primitiveTypes_27, {
                Equals: equals,
                GetHashCode: (x_34) => (structuralHash(x_34) | 0),
            })) {
                const arg_32 = (5).toString();
                const arg_1_27 = actual_27.toString();
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("MaxX should be 5");
            }
            else {
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_27)("MaxX should be 5");
            }
            throw new Exception(errorMsg_27);
        }
        const actual_28 = r_20.MaxY;
        if ((actual_28 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, 7, "MaxY should be 7");
        }
        else {
            let valueType_28;
            let copyOfStruct_28 = actual_28;
            valueType_28 = float64_type;
            const primitiveTypes_28 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_28;
            if (contains(valueType_28, primitiveTypes_28, {
                Equals: equals,
                GetHashCode: (x_35) => (structuralHash(x_35) | 0),
            })) {
                const arg_33 = (7).toString();
                const arg_1_28 = actual_28.toString();
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("MaxY should be 7");
            }
            else {
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_28)("MaxY should be 7");
            }
            throw new Exception(errorMsg_28);
        }
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromLine with reversed line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const line_1 = Line2D_$ctor_77D16AC0(5, 7, 1, 2);
        let r_21;
        const l_1 = line_1;
        const minX_28 = min(l_1.FromX, l_1.ToX);
        const maxX_28 = max(l_1.FromX, l_1.ToX);
        const minY_28 = min(l_1.FromY, l_1.ToY);
        const maxY_28 = max(l_1.FromY, l_1.ToY);
        r_21 = BRect_$ctor_77D16AC0(minX_28, minY_28, maxX_28, maxY_28);
        const actual_29 = r_21.MinX;
        if ((actual_29 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, 1, "MinX should be 1 after sorting");
        }
        else {
            let valueType_29;
            let copyOfStruct_29 = actual_29;
            valueType_29 = float64_type;
            const primitiveTypes_29 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_29;
            if (contains(valueType_29, primitiveTypes_29, {
                Equals: equals,
                GetHashCode: (x_36) => (structuralHash(x_36) | 0),
            })) {
                const arg_34 = (1).toString();
                const arg_1_29 = actual_29.toString();
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)("MinX should be 1 after sorting");
            }
            else {
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_29)("MinX should be 1 after sorting");
            }
            throw new Exception(errorMsg_29);
        }
        const actual_30 = r_21.MinY;
        if ((actual_30 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, 2, "MinY should be 2 after sorting");
        }
        else {
            let valueType_30;
            let copyOfStruct_30 = actual_30;
            valueType_30 = float64_type;
            const primitiveTypes_30 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_30;
            if (contains(valueType_30, primitiveTypes_30, {
                Equals: equals,
                GetHashCode: (x_37) => (structuralHash(x_37) | 0),
            })) {
                const arg_35 = (2).toString();
                const arg_1_30 = actual_30.toString();
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("MinY should be 2 after sorting");
            }
            else {
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_30)("MinY should be 2 after sorting");
            }
            throw new Exception(errorMsg_30);
        }
        const actual_31 = r_21.MaxX;
        if ((actual_31 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, 5, "MaxX should be 5 after sorting");
        }
        else {
            let valueType_31;
            let copyOfStruct_31 = actual_31;
            valueType_31 = float64_type;
            const primitiveTypes_31 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_31;
            if (contains(valueType_31, primitiveTypes_31, {
                Equals: equals,
                GetHashCode: (x_38) => (structuralHash(x_38) | 0),
            })) {
                const arg_36 = (5).toString();
                const arg_1_31 = actual_31.toString();
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("MaxX should be 5 after sorting");
            }
            else {
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_31)("MaxX should be 5 after sorting");
            }
            throw new Exception(errorMsg_31);
        }
        const actual_32 = r_21.MaxY;
        if ((actual_32 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, 7, "MaxY should be 7 after sorting");
        }
        else {
            let valueType_32;
            let copyOfStruct_32 = actual_32;
            valueType_32 = float64_type;
            const primitiveTypes_32 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_32;
            if (contains(valueType_32, primitiveTypes_32, {
                Equals: equals,
                GetHashCode: (x_39) => (structuralHash(x_39) | 0),
            })) {
                const arg_37 = (7).toString();
                const arg_1_32 = actual_32.toString();
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_32)("MaxY should be 7 after sorting");
            }
            else {
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_32)("MaxY should be 7 after sorting");
            }
            throw new Exception(errorMsg_32);
        }
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})()])), Test_testList("Expand Methods", ofArray([(() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Expand with positive distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let r_22;
        const a_15 = Pt_$ctor_7B00E9A0(2, 3);
        const b_15 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_30 = a_15.X;
        let maxX_30;
        if (b_15.X > minX_30) {
            maxX_30 = b_15.X;
        }
        else {
            minX_30 = b_15.X;
            maxX_30 = a_15.X;
        }
        let minY_30 = a_15.Y;
        let maxY_30;
        if (b_15.Y > minY_30) {
            maxY_30 = b_15.Y;
        }
        else {
            minY_30 = b_15.Y;
            maxY_30 = a_15.Y;
        }
        r_22 = BRect_$ctor_77D16AC0(minX_30, minY_30, maxX_30, maxY_30);
        let expanded;
        const r_23 = r_22;
        const n = BRect_$ctor_77D16AC0(r_23.MinX - 1, r_23.MinY - 1, r_23.MaxX + 1, r_23.MaxY + 1);
        if ((1 < 0) && ((n.MinX > n.MaxX) ? true : (n.MinY > n.MaxY))) {
            fail(`BRect.Expand(dist): Negative distance ${1} causes an underflow, on ${BRect__get_AsString(r_23)}`);
        }
        expanded = n;
        const actual_33 = expanded.MinX;
        if ((actual_33 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, 1, "MinX should be 1");
        }
        else {
            let valueType_33;
            let copyOfStruct_33 = actual_33;
            valueType_33 = float64_type;
            const primitiveTypes_33 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_33;
            if (contains(valueType_33, primitiveTypes_33, {
                Equals: equals,
                GetHashCode: (x_40) => (structuralHash(x_40) | 0),
            })) {
                const arg_38 = (1).toString();
                const arg_1_33 = actual_33.toString();
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_38)(arg_1_33)("MinX should be 1");
            }
            else {
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_33)("MinX should be 1");
            }
            throw new Exception(errorMsg_33);
        }
        const actual_34 = expanded.MinY;
        if ((actual_34 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, 2, "MinY should be 2");
        }
        else {
            let valueType_34;
            let copyOfStruct_34 = actual_34;
            valueType_34 = float64_type;
            const primitiveTypes_34 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_34;
            if (contains(valueType_34, primitiveTypes_34, {
                Equals: equals,
                GetHashCode: (x_41) => (structuralHash(x_41) | 0),
            })) {
                const arg_39 = (2).toString();
                const arg_1_34 = actual_34.toString();
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_39)(arg_1_34)("MinY should be 2");
            }
            else {
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_34)("MinY should be 2");
            }
            throw new Exception(errorMsg_34);
        }
        const actual_35 = expanded.MaxX;
        if ((actual_35 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, 9, "MaxX should be 9");
        }
        else {
            let valueType_35;
            let copyOfStruct_35 = actual_35;
            valueType_35 = float64_type;
            const primitiveTypes_35 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_35;
            if (contains(valueType_35, primitiveTypes_35, {
                Equals: equals,
                GetHashCode: (x_42) => (structuralHash(x_42) | 0),
            })) {
                const arg_40 = (9).toString();
                const arg_1_35 = actual_35.toString();
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_40)(arg_1_35)("MaxX should be 9");
            }
            else {
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_35)("MaxX should be 9");
            }
            throw new Exception(errorMsg_35);
        }
        const actual_36 = expanded.MaxY;
        if ((actual_36 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, 10, "MaxY should be 10");
        }
        else {
            let valueType_36;
            let copyOfStruct_36 = actual_36;
            valueType_36 = float64_type;
            const primitiveTypes_36 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_36;
            if (contains(valueType_36, primitiveTypes_36, {
                Equals: equals,
                GetHashCode: (x_43) => (structuralHash(x_43) | 0),
            })) {
                const arg_41 = (10).toString();
                const arg_1_36 = actual_36.toString();
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_41)(arg_1_36)("MaxY should be 10");
            }
            else {
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_36)("MaxY should be 10");
            }
            throw new Exception(errorMsg_36);
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Expand with negative distance causing underflow throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let r_24;
        const a_16 = Pt_$ctor_7B00E9A0(2, 3);
        const b_16 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_33 = a_16.X;
        let maxX_33;
        if (b_16.X > minX_33) {
            maxX_33 = b_16.X;
        }
        else {
            minX_33 = b_16.X;
            maxX_33 = a_16.X;
        }
        let minY_33 = a_16.Y;
        let maxY_33;
        if (b_16.Y > minY_33) {
            maxY_33 = b_16.Y;
        }
        else {
            minY_33 = b_16.Y;
            maxY_33 = a_16.Y;
        }
        r_24 = BRect_$ctor_77D16AC0(minX_33, minY_33, maxX_33, maxY_33);
        Expect_throws(() => {
            let r_25, n_1;
            (r_25 = r_24, (n_1 = BRect_$ctor_77D16AC0(r_25.MinX - -10, r_25.MinY - -10, r_25.MaxX + -10, r_25.MaxY + -10), (((-10 < 0) && ((n_1.MinX > n_1.MaxX) ? true : (n_1.MinY > n_1.MaxY))) ? fail(`BRect.Expand(dist): Negative distance ${-10} causes an underflow, on ${BRect__get_AsString(r_25)}`) : undefined, n_1)));
        }, "Should throw on underflow");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Expand(xDist, yDist) with positive distances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let r_26;
        const a_17 = Pt_$ctor_7B00E9A0(2, 3);
        const b_17 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_36 = a_17.X;
        let maxX_36;
        if (b_17.X > minX_36) {
            maxX_36 = b_17.X;
        }
        else {
            minX_36 = b_17.X;
            maxX_36 = a_17.X;
        }
        let minY_36 = a_17.Y;
        let maxY_36;
        if (b_17.Y > minY_36) {
            maxY_36 = b_17.Y;
        }
        else {
            minY_36 = b_17.Y;
            maxY_36 = a_17.Y;
        }
        r_26 = BRect_$ctor_77D16AC0(minX_36, minY_36, maxX_36, maxY_36);
        let expanded_1;
        const r_27 = r_26;
        const n_2 = BRect_$ctor_77D16AC0(r_27.MinX - 1, r_27.MinY - 2, r_27.MaxX + 1, r_27.MaxY + 2);
        if ((n_2.MinX > n_2.MaxX) ? true : (n_2.MinY > n_2.MaxY)) {
            fail(`BRect.Expand(x, y): Distance(s) X: ${1} and Y: ${2} cause an underflow, on ${BRect__get_AsString(r_27)}`);
        }
        expanded_1 = n_2;
        const actual_37 = expanded_1.MinX;
        if ((actual_37 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, 1, "MinX should be 1");
        }
        else {
            let valueType_37;
            let copyOfStruct_37 = actual_37;
            valueType_37 = float64_type;
            const primitiveTypes_37 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_37;
            if (contains(valueType_37, primitiveTypes_37, {
                Equals: equals,
                GetHashCode: (x_44) => (structuralHash(x_44) | 0),
            })) {
                const arg_42 = (1).toString();
                const arg_1_37 = actual_37.toString();
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_42)(arg_1_37)("MinX should be 1");
            }
            else {
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_37)("MinX should be 1");
            }
            throw new Exception(errorMsg_37);
        }
        const actual_38 = expanded_1.MinY;
        if ((actual_38 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, 1, "MinY should be 1");
        }
        else {
            let valueType_38;
            let copyOfStruct_38 = actual_38;
            valueType_38 = float64_type;
            const primitiveTypes_38 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_38;
            if (contains(valueType_38, primitiveTypes_38, {
                Equals: equals,
                GetHashCode: (x_45) => (structuralHash(x_45) | 0),
            })) {
                const arg_43 = (1).toString();
                const arg_1_38 = actual_38.toString();
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_43)(arg_1_38)("MinY should be 1");
            }
            else {
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_38)("MinY should be 1");
            }
            throw new Exception(errorMsg_38);
        }
        const actual_39 = expanded_1.MaxX;
        if ((actual_39 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_39, 9, "MaxX should be 9");
        }
        else {
            let valueType_39;
            let copyOfStruct_39 = actual_39;
            valueType_39 = float64_type;
            const primitiveTypes_39 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_39;
            if (contains(valueType_39, primitiveTypes_39, {
                Equals: equals,
                GetHashCode: (x_46) => (structuralHash(x_46) | 0),
            })) {
                const arg_44 = (9).toString();
                const arg_1_39 = actual_39.toString();
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_44)(arg_1_39)("MaxX should be 9");
            }
            else {
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_39)("MaxX should be 9");
            }
            throw new Exception(errorMsg_39);
        }
        const actual_40 = expanded_1.MaxY;
        if ((actual_40 === 11) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_40, 11, "MaxY should be 11");
        }
        else {
            let valueType_40;
            let copyOfStruct_40 = actual_40;
            valueType_40 = float64_type;
            const primitiveTypes_40 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_40;
            if (contains(valueType_40, primitiveTypes_40, {
                Equals: equals,
                GetHashCode: (x_47) => (structuralHash(x_47) | 0),
            })) {
                const arg_45 = (11).toString();
                const arg_1_40 = actual_40.toString();
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_45)(arg_1_40)("MaxY should be 11");
            }
            else {
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(11)(actual_40)("MaxY should be 11");
            }
            throw new Exception(errorMsg_40);
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Expand(xDist, yDist) with distances causing underflow throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let r_28;
        const a_18 = Pt_$ctor_7B00E9A0(2, 3);
        const b_18 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_39 = a_18.X;
        let maxX_39;
        if (b_18.X > minX_39) {
            maxX_39 = b_18.X;
        }
        else {
            minX_39 = b_18.X;
            maxX_39 = a_18.X;
        }
        let minY_39 = a_18.Y;
        let maxY_39;
        if (b_18.Y > minY_39) {
            maxY_39 = b_18.Y;
        }
        else {
            minY_39 = b_18.Y;
            maxY_39 = a_18.Y;
        }
        r_28 = BRect_$ctor_77D16AC0(minX_39, minY_39, maxX_39, maxY_39);
        Expect_throws(() => {
            let r_29, n_3;
            (r_29 = r_28, (n_3 = BRect_$ctor_77D16AC0(r_29.MinX - -10, r_29.MinY - 0, r_29.MaxX + -10, r_29.MaxY + 0), (((n_3.MinX > n_3.MaxX) ? true : (n_3.MinY > n_3.MaxY)) ? fail(`BRect.Expand(x, y): Distance(s) X: ${-10} and Y: ${0} cause an underflow, on ${BRect__get_AsString(r_29)}`) : undefined, n_3)));
        }, "Should throw on X underflow");
        Expect_throws(() => {
            let r_30, n_4;
            (r_30 = r_28, (n_4 = BRect_$ctor_77D16AC0(r_30.MinX - 0, r_30.MinY - -10, r_30.MaxX + 0, r_30.MaxY + -10), (((n_4.MinX > n_4.MaxX) ? true : (n_4.MinY > n_4.MaxY)) ? fail(`BRect.Expand(x, y): Distance(s) X: ${0} and Y: ${-10} cause an underflow, on ${BRect__get_AsString(r_30)}`) : undefined, n_4)));
        }, "Should throw on Y underflow");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandSafe with positive distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let r_31;
        const a_19 = Pt_$ctor_7B00E9A0(2, 3);
        const b_19 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_43 = a_19.X;
        let maxX_43;
        if (b_19.X > minX_43) {
            maxX_43 = b_19.X;
        }
        else {
            minX_43 = b_19.X;
            maxX_43 = a_19.X;
        }
        let minY_43 = a_19.Y;
        let maxY_43;
        if (b_19.Y > minY_43) {
            maxY_43 = b_19.Y;
        }
        else {
            minY_43 = b_19.Y;
            maxY_43 = a_19.Y;
        }
        r_31 = BRect_$ctor_77D16AC0(minX_43, minY_43, maxX_43, maxY_43);
        let expanded_2;
        const b_21 = r_31;
        let minXCh = b_21.MinX - 1;
        let maxXCh = b_21.MaxX + 1;
        if (minXCh > maxXCh) {
            const mid = b_21.MinX + ((b_21.MaxX - b_21.MinX) * 0.5);
            minXCh = mid;
            maxXCh = mid;
        }
        let minYCh = b_21.MinY - 1;
        let maxYCh = b_21.MaxY + 1;
        if (minYCh > maxYCh) {
            const mid_1 = b_21.MinY + ((b_21.MaxY - b_21.MinY) * 0.5);
            minYCh = mid_1;
            maxYCh = mid_1;
        }
        expanded_2 = BRect_$ctor_77D16AC0(minXCh, minYCh, maxXCh, maxYCh);
        const actual_41 = expanded_2.MinX;
        if ((actual_41 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_41, 1, "MinX should be 1");
        }
        else {
            let valueType_41;
            let copyOfStruct_41 = actual_41;
            valueType_41 = float64_type;
            const primitiveTypes_41 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_41;
            if (contains(valueType_41, primitiveTypes_41, {
                Equals: equals,
                GetHashCode: (x_48) => (structuralHash(x_48) | 0),
            })) {
                const arg_46 = (1).toString();
                const arg_1_41 = actual_41.toString();
                errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_46)(arg_1_41)("MinX should be 1");
            }
            else {
                errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_41)("MinX should be 1");
            }
            throw new Exception(errorMsg_41);
        }
        const actual_42 = expanded_2.MaxX;
        if ((actual_42 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_42, 9, "MaxX should be 9");
        }
        else {
            let valueType_42;
            let copyOfStruct_42 = actual_42;
            valueType_42 = float64_type;
            const primitiveTypes_42 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_42;
            if (contains(valueType_42, primitiveTypes_42, {
                Equals: equals,
                GetHashCode: (x_49) => (structuralHash(x_49) | 0),
            })) {
                const arg_47 = (9).toString();
                const arg_1_42 = actual_42.toString();
                errorMsg_42 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_47)(arg_1_42)("MaxX should be 9");
            }
            else {
                errorMsg_42 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_42)("MaxX should be 9");
            }
            throw new Exception(errorMsg_42);
        }
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandSafe with negative distance causing underflow collapses to midpoint", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let r_32;
        const a_20 = Pt_$ctor_7B00E9A0(2, 3);
        const b_22 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_46 = a_20.X;
        let maxX_46;
        if (b_22.X > minX_46) {
            maxX_46 = b_22.X;
        }
        else {
            minX_46 = b_22.X;
            maxX_46 = a_20.X;
        }
        let minY_46 = a_20.Y;
        let maxY_46;
        if (b_22.Y > minY_46) {
            maxY_46 = b_22.Y;
        }
        else {
            minY_46 = b_22.Y;
            maxY_46 = a_20.Y;
        }
        r_32 = BRect_$ctor_77D16AC0(minX_46, minY_46, maxX_46, maxY_46);
        let shrunk;
        const b_24 = r_32;
        let minXCh_1 = b_24.MinX - -10;
        let maxXCh_1 = b_24.MaxX + -10;
        if (minXCh_1 > maxXCh_1) {
            const mid_2 = b_24.MinX + ((b_24.MaxX - b_24.MinX) * 0.5);
            minXCh_1 = mid_2;
            maxXCh_1 = mid_2;
        }
        let minYCh_1 = b_24.MinY - -10;
        let maxYCh_1 = b_24.MaxY + -10;
        if (minYCh_1 > maxYCh_1) {
            const mid_1_1 = b_24.MinY + ((b_24.MaxY - b_24.MinY) * 0.5);
            minYCh_1 = mid_1_1;
            maxYCh_1 = mid_1_1;
        }
        shrunk = BRect_$ctor_77D16AC0(minXCh_1, minYCh_1, maxXCh_1, maxYCh_1);
        const actual_43 = shrunk.MinX;
        if ((actual_43 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_43, 5, "MinX should collapse to center X (5)");
        }
        else {
            let valueType_43;
            let copyOfStruct_43 = actual_43;
            valueType_43 = float64_type;
            const primitiveTypes_43 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_43;
            if (contains(valueType_43, primitiveTypes_43, {
                Equals: equals,
                GetHashCode: (x_50) => (structuralHash(x_50) | 0),
            })) {
                const arg_48 = (5).toString();
                const arg_1_43 = actual_43.toString();
                errorMsg_43 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_48)(arg_1_43)("MinX should collapse to center X (5)");
            }
            else {
                errorMsg_43 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_43)("MinX should collapse to center X (5)");
            }
            throw new Exception(errorMsg_43);
        }
        const actual_44 = shrunk.MaxX;
        if ((actual_44 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_44, 5, "MaxX should collapse to center X (5)");
        }
        else {
            let valueType_44;
            let copyOfStruct_44 = actual_44;
            valueType_44 = float64_type;
            const primitiveTypes_44 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_44;
            if (contains(valueType_44, primitiveTypes_44, {
                Equals: equals,
                GetHashCode: (x_51) => (structuralHash(x_51) | 0),
            })) {
                const arg_49 = (5).toString();
                const arg_1_44 = actual_44.toString();
                errorMsg_44 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_49)(arg_1_44)("MaxX should collapse to center X (5)");
            }
            else {
                errorMsg_44 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_44)("MaxX should collapse to center X (5)");
            }
            throw new Exception(errorMsg_44);
        }
        const actual_45 = shrunk.MinY;
        if ((actual_45 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_45, 6, "MinY should collapse to center Y (6)");
        }
        else {
            let valueType_45;
            let copyOfStruct_45 = actual_45;
            valueType_45 = float64_type;
            const primitiveTypes_45 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_45;
            if (contains(valueType_45, primitiveTypes_45, {
                Equals: equals,
                GetHashCode: (x_52) => (structuralHash(x_52) | 0),
            })) {
                const arg_50 = (6).toString();
                const arg_1_45 = actual_45.toString();
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_50)(arg_1_45)("MinY should collapse to center Y (6)");
            }
            else {
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_45)("MinY should collapse to center Y (6)");
            }
            throw new Exception(errorMsg_45);
        }
        const actual_46 = shrunk.MaxY;
        if ((actual_46 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_46, 6, "MaxY should collapse to center Y (6)");
        }
        else {
            let valueType_46;
            let copyOfStruct_46 = actual_46;
            valueType_46 = float64_type;
            const primitiveTypes_46 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_46;
            if (contains(valueType_46, primitiveTypes_46, {
                Equals: equals,
                GetHashCode: (x_53) => (structuralHash(x_53) | 0),
            })) {
                const arg_51 = (6).toString();
                const arg_1_46 = actual_46.toString();
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_51)(arg_1_46)("MaxY should collapse to center Y (6)");
            }
            else {
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_46)("MaxY should collapse to center Y (6)");
            }
            throw new Exception(errorMsg_46);
        }
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandSafe(xDist, yDist) with different underflow per axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let r_33;
        const a_21 = Pt_$ctor_7B00E9A0(0, 0);
        const b_25 = Pt_$ctor_7B00E9A0(4, 10);
        let minX_49 = a_21.X;
        let maxX_49;
        if (b_25.X > minX_49) {
            maxX_49 = b_25.X;
        }
        else {
            minX_49 = b_25.X;
            maxX_49 = a_21.X;
        }
        let minY_49 = a_21.Y;
        let maxY_49;
        if (b_25.Y > minY_49) {
            maxY_49 = b_25.Y;
        }
        else {
            minY_49 = b_25.Y;
            maxY_49 = a_21.Y;
        }
        r_33 = BRect_$ctor_77D16AC0(minX_49, minY_49, maxX_49, maxY_49);
        let shrunk_1;
        const b_26 = r_33;
        let minXCh_2 = b_26.MinX - -5;
        let maxXCh_2 = b_26.MaxX + -5;
        if (minXCh_2 > maxXCh_2) {
            const mid_3 = b_26.MinX + ((b_26.MaxX - b_26.MinX) * 0.5);
            minXCh_2 = mid_3;
            maxXCh_2 = mid_3;
        }
        let minYCh_2 = b_26.MinY - -2;
        let maxYCh_2 = b_26.MaxY + -2;
        if (minYCh_2 > maxYCh_2) {
            const mid_1_2 = b_26.MinY + ((b_26.MaxY - b_26.MinY) * 0.5);
            minYCh_2 = mid_1_2;
            maxYCh_2 = mid_1_2;
        }
        shrunk_1 = BRect_$ctor_77D16AC0(minXCh_2, minYCh_2, maxXCh_2, maxYCh_2);
        const actual_47 = shrunk_1.MinX;
        if ((actual_47 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_47, 2, "MinX should collapse to center X (2)");
        }
        else {
            let valueType_47;
            let copyOfStruct_47 = actual_47;
            valueType_47 = float64_type;
            const primitiveTypes_47 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_47;
            if (contains(valueType_47, primitiveTypes_47, {
                Equals: equals,
                GetHashCode: (x_54) => (structuralHash(x_54) | 0),
            })) {
                const arg_52 = (2).toString();
                const arg_1_47 = actual_47.toString();
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_52)(arg_1_47)("MinX should collapse to center X (2)");
            }
            else {
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_47)("MinX should collapse to center X (2)");
            }
            throw new Exception(errorMsg_47);
        }
        const actual_48 = shrunk_1.MaxX;
        if ((actual_48 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_48, 2, "MaxX should collapse to center X (2)");
        }
        else {
            let valueType_48;
            let copyOfStruct_48 = actual_48;
            valueType_48 = float64_type;
            const primitiveTypes_48 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_48;
            if (contains(valueType_48, primitiveTypes_48, {
                Equals: equals,
                GetHashCode: (x_55) => (structuralHash(x_55) | 0),
            })) {
                const arg_53 = (2).toString();
                const arg_1_48 = actual_48.toString();
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_53)(arg_1_48)("MaxX should collapse to center X (2)");
            }
            else {
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_48)("MaxX should collapse to center X (2)");
            }
            throw new Exception(errorMsg_48);
        }
        const actual_49 = shrunk_1.MinY;
        if ((actual_49 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_49, 2, "MinY should be 2 (still valid)");
        }
        else {
            let valueType_49;
            let copyOfStruct_49 = actual_49;
            valueType_49 = float64_type;
            const primitiveTypes_49 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_49;
            if (contains(valueType_49, primitiveTypes_49, {
                Equals: equals,
                GetHashCode: (x_56) => (structuralHash(x_56) | 0),
            })) {
                const arg_54 = (2).toString();
                const arg_1_49 = actual_49.toString();
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_54)(arg_1_49)("MinY should be 2 (still valid)");
            }
            else {
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_49)("MinY should be 2 (still valid)");
            }
            throw new Exception(errorMsg_49);
        }
        const actual_50 = shrunk_1.MaxY;
        if ((actual_50 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_50, 8, "MaxY should be 8 (still valid)");
        }
        else {
            let valueType_50;
            let copyOfStruct_50 = actual_50;
            valueType_50 = float64_type;
            const primitiveTypes_50 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_50;
            if (contains(valueType_50, primitiveTypes_50, {
                Equals: equals,
                GetHashCode: (x_57) => (structuralHash(x_57) | 0),
            })) {
                const arg_55 = (8).toString();
                const arg_1_50 = actual_50.toString();
                errorMsg_50 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_55)(arg_1_50)("MaxY should be 8 (still valid)");
            }
            else {
                errorMsg_50 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_50)("MaxY should be 8 (still valid)");
            }
            throw new Exception(errorMsg_50);
        }
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandXaxis with positive distances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let r_34;
        const a_22 = Pt_$ctor_7B00E9A0(2, 3);
        const b_27 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_52 = a_22.X;
        let maxX_52;
        if (b_27.X > minX_52) {
            maxX_52 = b_27.X;
        }
        else {
            minX_52 = b_27.X;
            maxX_52 = a_22.X;
        }
        let minY_52 = a_22.Y;
        let maxY_52;
        if (b_27.Y > minY_52) {
            maxY_52 = b_27.Y;
        }
        else {
            minY_52 = b_27.Y;
            maxY_52 = a_22.Y;
        }
        r_34 = BRect_$ctor_77D16AC0(minX_52, minY_52, maxX_52, maxY_52);
        let expanded_3;
        const r_35 = r_34;
        const n_5 = BRect_$ctor_77D16AC0(r_35.MinX - 1, r_35.MinY, r_35.MaxX + 2, r_35.MaxY);
        if (n_5.MinX > n_5.MaxX) {
            fail(`BRect.ExpandXaxis: Negative distances for start(${1}) and end (${2}) cause an underflow, on ${BRect__get_AsString(r_35)}`);
        }
        expanded_3 = n_5;
        const actual_51 = expanded_3.MinX;
        if ((actual_51 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_51, 1, "MinX should be 1");
        }
        else {
            let valueType_51;
            let copyOfStruct_51 = actual_51;
            valueType_51 = float64_type;
            const primitiveTypes_51 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_51;
            if (contains(valueType_51, primitiveTypes_51, {
                Equals: equals,
                GetHashCode: (x_58) => (structuralHash(x_58) | 0),
            })) {
                const arg_56 = (1).toString();
                const arg_1_51 = actual_51.toString();
                errorMsg_51 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_56)(arg_1_51)("MinX should be 1");
            }
            else {
                errorMsg_51 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_51)("MinX should be 1");
            }
            throw new Exception(errorMsg_51);
        }
        const actual_52 = expanded_3.MaxX;
        if ((actual_52 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_52, 10, "MaxX should be 10");
        }
        else {
            let valueType_52;
            let copyOfStruct_52 = actual_52;
            valueType_52 = float64_type;
            const primitiveTypes_52 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_52;
            if (contains(valueType_52, primitiveTypes_52, {
                Equals: equals,
                GetHashCode: (x_59) => (structuralHash(x_59) | 0),
            })) {
                const arg_57 = (10).toString();
                const arg_1_52 = actual_52.toString();
                errorMsg_52 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_57)(arg_1_52)("MaxX should be 10");
            }
            else {
                errorMsg_52 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_52)("MaxX should be 10");
            }
            throw new Exception(errorMsg_52);
        }
        const actual_53 = expanded_3.MinY;
        if ((actual_53 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_53, 3, "MinY should remain 3");
        }
        else {
            let valueType_53;
            let copyOfStruct_53 = actual_53;
            valueType_53 = float64_type;
            const primitiveTypes_53 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_53;
            if (contains(valueType_53, primitiveTypes_53, {
                Equals: equals,
                GetHashCode: (x_60) => (structuralHash(x_60) | 0),
            })) {
                const arg_58 = (3).toString();
                const arg_1_53 = actual_53.toString();
                errorMsg_53 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_58)(arg_1_53)("MinY should remain 3");
            }
            else {
                errorMsg_53 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_53)("MinY should remain 3");
            }
            throw new Exception(errorMsg_53);
        }
        const actual_54 = expanded_3.MaxY;
        if ((actual_54 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_54, 9, "MaxY should remain 9");
        }
        else {
            let valueType_54;
            let copyOfStruct_54 = actual_54;
            valueType_54 = float64_type;
            const primitiveTypes_54 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_54;
            if (contains(valueType_54, primitiveTypes_54, {
                Equals: equals,
                GetHashCode: (x_61) => (structuralHash(x_61) | 0),
            })) {
                const arg_59 = (9).toString();
                const arg_1_54 = actual_54.toString();
                errorMsg_54 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_59)(arg_1_54)("MaxY should remain 9");
            }
            else {
                errorMsg_54 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_54)("MaxY should remain 9");
            }
            throw new Exception(errorMsg_54);
        }
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandXaxis with underflow throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let r_36;
        const a_23 = Pt_$ctor_7B00E9A0(2, 3);
        const b_28 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_55 = a_23.X;
        let maxX_55;
        if (b_28.X > minX_55) {
            maxX_55 = b_28.X;
        }
        else {
            minX_55 = b_28.X;
            maxX_55 = a_23.X;
        }
        let minY_55 = a_23.Y;
        let maxY_55;
        if (b_28.Y > minY_55) {
            maxY_55 = b_28.Y;
        }
        else {
            minY_55 = b_28.Y;
            maxY_55 = a_23.Y;
        }
        r_36 = BRect_$ctor_77D16AC0(minX_55, minY_55, maxX_55, maxY_55);
        Expect_throws(() => {
            let r_37, n_6;
            (r_37 = r_36, (n_6 = BRect_$ctor_77D16AC0(r_37.MinX - -10, r_37.MinY, r_37.MaxX + 0, r_37.MaxY), ((n_6.MinX > n_6.MaxX) ? fail(`BRect.ExpandXaxis: Negative distances for start(${-10}) and end (${0}) cause an underflow, on ${BRect__get_AsString(r_37)}`) : undefined, n_6)));
        }, "Should throw on X underflow");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandYaxis with positive distances", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let r_38;
        const a_24 = Pt_$ctor_7B00E9A0(2, 3);
        const b_29 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_58 = a_24.X;
        let maxX_58;
        if (b_29.X > minX_58) {
            maxX_58 = b_29.X;
        }
        else {
            minX_58 = b_29.X;
            maxX_58 = a_24.X;
        }
        let minY_58 = a_24.Y;
        let maxY_58;
        if (b_29.Y > minY_58) {
            maxY_58 = b_29.Y;
        }
        else {
            minY_58 = b_29.Y;
            maxY_58 = a_24.Y;
        }
        r_38 = BRect_$ctor_77D16AC0(minX_58, minY_58, maxX_58, maxY_58);
        let expanded_4;
        const r_39 = r_38;
        const n_7 = BRect_$ctor_77D16AC0(r_39.MinX, r_39.MinY - 1, r_39.MaxX, r_39.MaxY + 2);
        if (n_7.MinY > n_7.MaxY) {
            fail(`BRect.ExpandYaxis: Negative distances for start(${1}) and end(${2}) cause an underflow, on ${BRect__get_AsString(r_39)}`);
        }
        expanded_4 = n_7;
        const actual_55 = expanded_4.MinY;
        if ((actual_55 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_55, 2, "MinY should be 2");
        }
        else {
            let valueType_55;
            let copyOfStruct_55 = actual_55;
            valueType_55 = float64_type;
            const primitiveTypes_55 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_55;
            if (contains(valueType_55, primitiveTypes_55, {
                Equals: equals,
                GetHashCode: (x_62) => (structuralHash(x_62) | 0),
            })) {
                const arg_60 = (2).toString();
                const arg_1_55 = actual_55.toString();
                errorMsg_55 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_60)(arg_1_55)("MinY should be 2");
            }
            else {
                errorMsg_55 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_55)("MinY should be 2");
            }
            throw new Exception(errorMsg_55);
        }
        const actual_56 = expanded_4.MaxY;
        if ((actual_56 === 11) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_56, 11, "MaxY should be 11");
        }
        else {
            let valueType_56;
            let copyOfStruct_56 = actual_56;
            valueType_56 = float64_type;
            const primitiveTypes_56 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_56;
            if (contains(valueType_56, primitiveTypes_56, {
                Equals: equals,
                GetHashCode: (x_63) => (structuralHash(x_63) | 0),
            })) {
                const arg_61 = (11).toString();
                const arg_1_56 = actual_56.toString();
                errorMsg_56 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_61)(arg_1_56)("MaxY should be 11");
            }
            else {
                errorMsg_56 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(11)(actual_56)("MaxY should be 11");
            }
            throw new Exception(errorMsg_56);
        }
        const actual_57 = expanded_4.MinX;
        if ((actual_57 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_57, 2, "MinX should remain 2");
        }
        else {
            let valueType_57;
            let copyOfStruct_57 = actual_57;
            valueType_57 = float64_type;
            const primitiveTypes_57 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_57;
            if (contains(valueType_57, primitiveTypes_57, {
                Equals: equals,
                GetHashCode: (x_64) => (structuralHash(x_64) | 0),
            })) {
                const arg_62 = (2).toString();
                const arg_1_57 = actual_57.toString();
                errorMsg_57 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_62)(arg_1_57)("MinX should remain 2");
            }
            else {
                errorMsg_57 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_57)("MinX should remain 2");
            }
            throw new Exception(errorMsg_57);
        }
        const actual_58 = expanded_4.MaxX;
        if ((actual_58 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_58, 8, "MaxX should remain 8");
        }
        else {
            let valueType_58;
            let copyOfStruct_58 = actual_58;
            valueType_58 = float64_type;
            const primitiveTypes_58 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_58;
            if (contains(valueType_58, primitiveTypes_58, {
                Equals: equals,
                GetHashCode: (x_65) => (structuralHash(x_65) | 0),
            })) {
                const arg_63 = (8).toString();
                const arg_1_58 = actual_58.toString();
                errorMsg_58 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_63)(arg_1_58)("MaxX should remain 8");
            }
            else {
                errorMsg_58 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_58)("MaxX should remain 8");
            }
            throw new Exception(errorMsg_58);
        }
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExpandYaxis with underflow throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let r_40;
        const a_25 = Pt_$ctor_7B00E9A0(2, 3);
        const b_30 = Pt_$ctor_7B00E9A0(8, 9);
        let minX_61 = a_25.X;
        let maxX_61;
        if (b_30.X > minX_61) {
            maxX_61 = b_30.X;
        }
        else {
            minX_61 = b_30.X;
            maxX_61 = a_25.X;
        }
        let minY_61 = a_25.Y;
        let maxY_61;
        if (b_30.Y > minY_61) {
            maxY_61 = b_30.Y;
        }
        else {
            minY_61 = b_30.Y;
            maxY_61 = a_25.Y;
        }
        r_40 = BRect_$ctor_77D16AC0(minX_61, minY_61, maxX_61, maxY_61);
        Expect_throws(() => {
            let r_41, n_8;
            (r_41 = r_40, (n_8 = BRect_$ctor_77D16AC0(r_41.MinX, r_41.MinY - -10, r_41.MaxX, r_41.MaxY + 0), ((n_8.MinY > n_8.MaxY) ? fail(`BRect.ExpandYaxis: Negative distances for start(${-10}) and end(${0}) cause an underflow, on ${BRect__get_AsString(r_41)}`) : undefined, n_8)));
        }, "Should throw on Y underflow");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})()])), Test_testList("Overlap and Containment", ofArray([(() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith two overlapping rectangles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let r_42, a_28;
        let r1;
        const a_26 = Pt_$ctor_7B00E9A0(0, 0);
        const b_31 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_64 = a_26.X;
        let maxX_64;
        if (b_31.X > minX_64) {
            maxX_64 = b_31.X;
        }
        else {
            minX_64 = b_31.X;
            maxX_64 = a_26.X;
        }
        let minY_64 = a_26.Y;
        let maxY_64;
        if (b_31.Y > minY_64) {
            maxY_64 = b_31.Y;
        }
        else {
            minY_64 = b_31.Y;
            maxY_64 = a_26.Y;
        }
        r1 = BRect_$ctor_77D16AC0(minX_64, minY_64, maxX_64, maxY_64);
        let r2;
        const a_27 = Pt_$ctor_7B00E9A0(5, 5);
        const b_32 = Pt_$ctor_7B00E9A0(15, 15);
        let minX_66 = a_27.X;
        let maxX_66;
        if (b_32.X > minX_66) {
            maxX_66 = b_32.X;
        }
        else {
            minX_66 = b_32.X;
            maxX_66 = a_27.X;
        }
        let minY_66 = a_27.Y;
        let maxY_66;
        if (b_32.Y > minY_66) {
            maxY_66 = b_32.Y;
        }
        else {
            minY_66 = b_32.Y;
            maxY_66 = a_27.Y;
        }
        r2 = BRect_$ctor_77D16AC0(minX_66, minY_66, maxX_66, maxY_66);
        Expect_isTrue((r_42 = r1, (a_28 = r2, !((((r_42.MinX > a_28.MaxX) ? true : (a_28.MinX > r_42.MaxX)) ? true : (a_28.MinY > r_42.MaxY)) ? true : (r_42.MinY > a_28.MaxY)))))("Should overlap");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith two touching rectangles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let r_43, a_31;
        let r1_1;
        const a_29 = Pt_$ctor_7B00E9A0(0, 0);
        const b_33 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_68 = a_29.X;
        let maxX_68;
        if (b_33.X > minX_68) {
            maxX_68 = b_33.X;
        }
        else {
            minX_68 = b_33.X;
            maxX_68 = a_29.X;
        }
        let minY_68 = a_29.Y;
        let maxY_68;
        if (b_33.Y > minY_68) {
            maxY_68 = b_33.Y;
        }
        else {
            minY_68 = b_33.Y;
            maxY_68 = a_29.Y;
        }
        r1_1 = BRect_$ctor_77D16AC0(minX_68, minY_68, maxX_68, maxY_68);
        let r2_1;
        const a_30 = Pt_$ctor_7B00E9A0(10, 0);
        const b_34 = Pt_$ctor_7B00E9A0(20, 10);
        let minX_70 = a_30.X;
        let maxX_70;
        if (b_34.X > minX_70) {
            maxX_70 = b_34.X;
        }
        else {
            minX_70 = b_34.X;
            maxX_70 = a_30.X;
        }
        let minY_70 = a_30.Y;
        let maxY_70;
        if (b_34.Y > minY_70) {
            maxY_70 = b_34.Y;
        }
        else {
            minY_70 = b_34.Y;
            maxY_70 = a_30.Y;
        }
        r2_1 = BRect_$ctor_77D16AC0(minX_70, minY_70, maxX_70, maxY_70);
        Expect_isTrue((r_43 = r1_1, (a_31 = r2_1, !((((r_43.MinX > a_31.MaxX) ? true : (a_31.MinX > r_43.MaxX)) ? true : (a_31.MinY > r_43.MaxY)) ? true : (r_43.MinY > a_31.MaxY)))))("Should be touching (considered overlapping)");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith two separate rectangles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let r_44, a_34;
        let r1_2;
        const a_32 = Pt_$ctor_7B00E9A0(0, 0);
        const b_35 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_72 = a_32.X;
        let maxX_72;
        if (b_35.X > minX_72) {
            maxX_72 = b_35.X;
        }
        else {
            minX_72 = b_35.X;
            maxX_72 = a_32.X;
        }
        let minY_72 = a_32.Y;
        let maxY_72;
        if (b_35.Y > minY_72) {
            maxY_72 = b_35.Y;
        }
        else {
            minY_72 = b_35.Y;
            maxY_72 = a_32.Y;
        }
        r1_2 = BRect_$ctor_77D16AC0(minX_72, minY_72, maxX_72, maxY_72);
        let r2_2;
        const a_33 = Pt_$ctor_7B00E9A0(11, 11);
        const b_36 = Pt_$ctor_7B00E9A0(20, 20);
        let minX_74 = a_33.X;
        let maxX_74;
        if (b_36.X > minX_74) {
            maxX_74 = b_36.X;
        }
        else {
            minX_74 = b_36.X;
            maxX_74 = a_33.X;
        }
        let minY_74 = a_33.Y;
        let maxY_74;
        if (b_36.Y > minY_74) {
            maxY_74 = b_36.Y;
        }
        else {
            minY_74 = b_36.Y;
            maxY_74 = a_33.Y;
        }
        r2_2 = BRect_$ctor_77D16AC0(minX_74, minY_74, maxX_74, maxY_74);
        Expect_isFalse((r_44 = r1_2, (a_34 = r2_2, !((((r_44.MinX > a_34.MaxX) ? true : (a_34.MinX > r_44.MaxX)) ? true : (a_34.MinY > r_44.MaxY)) ? true : (r_44.MinY > a_34.MaxY)))))("Should not overlap");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith with tolerance - overlapping", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let r_45, a_37;
        let r1_3;
        const a_35 = Pt_$ctor_7B00E9A0(0, 0);
        const b_37 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_76 = a_35.X;
        let maxX_76;
        if (b_37.X > minX_76) {
            maxX_76 = b_37.X;
        }
        else {
            minX_76 = b_37.X;
            maxX_76 = a_35.X;
        }
        let minY_76 = a_35.Y;
        let maxY_76;
        if (b_37.Y > minY_76) {
            maxY_76 = b_37.Y;
        }
        else {
            minY_76 = b_37.Y;
            maxY_76 = a_35.Y;
        }
        r1_3 = BRect_$ctor_77D16AC0(minX_76, minY_76, maxX_76, maxY_76);
        let r2_3;
        const a_36 = Pt_$ctor_7B00E9A0(9, 9);
        const b_38 = Pt_$ctor_7B00E9A0(15, 15);
        let minX_78 = a_36.X;
        let maxX_78;
        if (b_38.X > minX_78) {
            maxX_78 = b_38.X;
        }
        else {
            minX_78 = b_38.X;
            maxX_78 = a_36.X;
        }
        let minY_78 = a_36.Y;
        let maxY_78;
        if (b_38.Y > minY_78) {
            maxY_78 = b_38.Y;
        }
        else {
            minY_78 = b_38.Y;
            maxY_78 = a_36.Y;
        }
        r2_3 = BRect_$ctor_77D16AC0(minX_78, minY_78, maxX_78, maxY_78);
        Expect_isTrue((r_45 = r1_3, (a_37 = r2_3, !((((r_45.MinX > (a_37.MaxX - 1)) ? true : (a_37.MinX > (r_45.MaxX - 1))) ? true : (a_37.MinY > (r_45.MaxY - 1))) ? true : (r_45.MinY > (a_37.MaxY - 1))))))("Should overlap with tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith with tolerance - just touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let r_46, a_40;
        let r1_4;
        const a_38 = Pt_$ctor_7B00E9A0(0, 0);
        const b_39 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_80 = a_38.X;
        let maxX_80;
        if (b_39.X > minX_80) {
            maxX_80 = b_39.X;
        }
        else {
            minX_80 = b_39.X;
            maxX_80 = a_38.X;
        }
        let minY_80 = a_38.Y;
        let maxY_80;
        if (b_39.Y > minY_80) {
            maxY_80 = b_39.Y;
        }
        else {
            minY_80 = b_39.Y;
            maxY_80 = a_38.Y;
        }
        r1_4 = BRect_$ctor_77D16AC0(minX_80, minY_80, maxX_80, maxY_80);
        let r2_4;
        const a_39 = Pt_$ctor_7B00E9A0(10, 0);
        const b_40 = Pt_$ctor_7B00E9A0(20, 10);
        let minX_82 = a_39.X;
        let maxX_82;
        if (b_40.X > minX_82) {
            maxX_82 = b_40.X;
        }
        else {
            minX_82 = b_40.X;
            maxX_82 = a_39.X;
        }
        let minY_82 = a_39.Y;
        let maxY_82;
        if (b_40.Y > minY_82) {
            maxY_82 = b_40.Y;
        }
        else {
            minY_82 = b_40.Y;
            maxY_82 = a_39.Y;
        }
        r2_4 = BRect_$ctor_77D16AC0(minX_82, minY_82, maxX_82, maxY_82);
        Expect_isFalse((r_46 = r1_4, (a_40 = r2_4, !((((r_46.MinX > (a_40.MaxX - 1)) ? true : (a_40.MinX > (r_46.MaxX - 1))) ? true : (a_40.MinY > (r_46.MaxY - 1))) ? true : (r_46.MinY > (a_40.MaxY - 1))))))("Should not overlap with positive tolerance (just touching)");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains point inside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let r_48, p_3;
        let r_47;
        const a_41 = Pt_$ctor_7B00E9A0(0, 0);
        const b_41 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_84 = a_41.X;
        let maxX_84;
        if (b_41.X > minX_84) {
            maxX_84 = b_41.X;
        }
        else {
            minX_84 = b_41.X;
            maxX_84 = a_41.X;
        }
        let minY_84 = a_41.Y;
        let maxY_84;
        if (b_41.Y > minY_84) {
            maxY_84 = b_41.Y;
        }
        else {
            minY_84 = b_41.Y;
            maxY_84 = a_41.Y;
        }
        r_47 = BRect_$ctor_77D16AC0(minX_84, minY_84, maxX_84, maxY_84);
        Expect_isTrue((r_48 = r_47, (p_3 = Pt_$ctor_7B00E9A0(5, 5), (((p_3.X >= r_48.MinX) && (p_3.X <= r_48.MaxX)) && (p_3.Y >= r_48.MinY)) && (p_3.Y <= r_48.MaxY))))("Point (5,5) should be inside");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains point on edge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let r_50, p_4;
        let r_49;
        const a_42 = Pt_$ctor_7B00E9A0(0, 0);
        const b_42 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_86 = a_42.X;
        let maxX_86;
        if (b_42.X > minX_86) {
            maxX_86 = b_42.X;
        }
        else {
            minX_86 = b_42.X;
            maxX_86 = a_42.X;
        }
        let minY_86 = a_42.Y;
        let maxY_86;
        if (b_42.Y > minY_86) {
            maxY_86 = b_42.Y;
        }
        else {
            minY_86 = b_42.Y;
            maxY_86 = a_42.Y;
        }
        r_49 = BRect_$ctor_77D16AC0(minX_86, minY_86, maxX_86, maxY_86);
        Expect_isTrue((r_50 = r_49, (p_4 = Pt_$ctor_7B00E9A0(10, 5), (((p_4.X >= r_50.MinX) && (p_4.X <= r_50.MaxX)) && (p_4.Y >= r_50.MinY)) && (p_4.Y <= r_50.MaxY))))("Point on edge should be contained");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains point outside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let r_52, p_5;
        let r_51;
        const a_43 = Pt_$ctor_7B00E9A0(0, 0);
        const b_43 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_88 = a_43.X;
        let maxX_88;
        if (b_43.X > minX_88) {
            maxX_88 = b_43.X;
        }
        else {
            minX_88 = b_43.X;
            maxX_88 = a_43.X;
        }
        let minY_88 = a_43.Y;
        let maxY_88;
        if (b_43.Y > minY_88) {
            maxY_88 = b_43.Y;
        }
        else {
            minY_88 = b_43.Y;
            maxY_88 = a_43.Y;
        }
        r_51 = BRect_$ctor_77D16AC0(minX_88, minY_88, maxX_88, maxY_88);
        Expect_isFalse((r_52 = r_51, (p_5 = Pt_$ctor_7B00E9A0(11, 5), (((p_5.X >= r_52.MinX) && (p_5.X <= r_52.MaxX)) && (p_5.Y >= r_52.MinY)) && (p_5.Y <= r_52.MaxY))))("Point outside should not be contained");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains rectangle inside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let r_53, o, r_55, p_6, r_54, r_57, p_7, r_56;
        let r1_5;
        const a_44 = Pt_$ctor_7B00E9A0(0, 0);
        const b_44 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_90 = a_44.X;
        let maxX_90;
        if (b_44.X > minX_90) {
            maxX_90 = b_44.X;
        }
        else {
            minX_90 = b_44.X;
            maxX_90 = a_44.X;
        }
        let minY_90 = a_44.Y;
        let maxY_90;
        if (b_44.Y > minY_90) {
            maxY_90 = b_44.Y;
        }
        else {
            minY_90 = b_44.Y;
            maxY_90 = a_44.Y;
        }
        r1_5 = BRect_$ctor_77D16AC0(minX_90, minY_90, maxX_90, maxY_90);
        let r2_5;
        const a_45 = Pt_$ctor_7B00E9A0(2, 2);
        const b_45 = Pt_$ctor_7B00E9A0(8, 8);
        let minX_92 = a_45.X;
        let maxX_92;
        if (b_45.X > minX_92) {
            maxX_92 = b_45.X;
        }
        else {
            minX_92 = b_45.X;
            maxX_92 = a_45.X;
        }
        let minY_92 = a_45.Y;
        let maxY_92;
        if (b_45.Y > minY_92) {
            maxY_92 = b_45.Y;
        }
        else {
            minY_92 = b_45.Y;
            maxY_92 = a_45.Y;
        }
        r2_5 = BRect_$ctor_77D16AC0(minX_92, minY_92, maxX_92, maxY_92);
        Expect_isTrue((r_53 = r1_5, (o = r2_5, ((r_55 = r_53, (p_6 = ((r_54 = o, Pt_$ctor_7B00E9A0_1(r_54.MinX, r_54.MinY))), (((p_6.X >= r_55.MinX) && (p_6.X <= r_55.MaxX)) && (p_6.Y >= r_55.MinY)) && (p_6.Y <= r_55.MaxY)))) && ((r_57 = r_53, (p_7 = ((r_56 = o, Pt_$ctor_7B00E9A0_1(r_56.MaxX, r_56.MaxY))), (((p_7.X >= r_57.MinX) && (p_7.X <= r_57.MaxX)) && (p_7.Y >= r_57.MinY)) && (p_7.Y <= r_57.MaxY)))))))("Inner rectangle should be contained");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Contains rectangle partially overlapping", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        let r_58, o_1, r_60, p_8, r_59, r_62, p_9, r_61;
        let r1_6;
        const a_46 = Pt_$ctor_7B00E9A0(0, 0);
        const b_46 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_94 = a_46.X;
        let maxX_94;
        if (b_46.X > minX_94) {
            maxX_94 = b_46.X;
        }
        else {
            minX_94 = b_46.X;
            maxX_94 = a_46.X;
        }
        let minY_94 = a_46.Y;
        let maxY_94;
        if (b_46.Y > minY_94) {
            maxY_94 = b_46.Y;
        }
        else {
            minY_94 = b_46.Y;
            maxY_94 = a_46.Y;
        }
        r1_6 = BRect_$ctor_77D16AC0(minX_94, minY_94, maxX_94, maxY_94);
        let r2_6;
        const a_47 = Pt_$ctor_7B00E9A0(5, 5);
        const b_47 = Pt_$ctor_7B00E9A0(15, 15);
        let minX_96 = a_47.X;
        let maxX_96;
        if (b_47.X > minX_96) {
            maxX_96 = b_47.X;
        }
        else {
            minX_96 = b_47.X;
            maxX_96 = a_47.X;
        }
        let minY_96 = a_47.Y;
        let maxY_96;
        if (b_47.Y > minY_96) {
            maxY_96 = b_47.Y;
        }
        else {
            minY_96 = b_47.Y;
            maxY_96 = a_47.Y;
        }
        r2_6 = BRect_$ctor_77D16AC0(minX_96, minY_96, maxX_96, maxY_96);
        Expect_isFalse((r_58 = r1_6, (o_1 = r2_6, ((r_60 = r_58, (p_8 = ((r_59 = o_1, Pt_$ctor_7B00E9A0_1(r_59.MinX, r_59.MinY))), (((p_8.X >= r_60.MinX) && (p_8.X <= r_60.MaxX)) && (p_8.Y >= r_60.MinY)) && (p_8.Y <= r_60.MaxY)))) && ((r_62 = r_58, (p_9 = ((r_61 = o_1, Pt_$ctor_7B00E9A0_1(r_61.MaxX, r_61.MaxY))), (((p_9.X >= r_62.MinX) && (p_9.X <= r_62.MaxX)) && (p_9.Y >= r_62.MinY)) && (p_9.Y <= r_62.MaxY)))))))("Partially overlapping rectangle should not be contained");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching detects edge touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let r1_7;
        const a_48 = Pt_$ctor_7B00E9A0(0, 0);
        const b_48 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_98 = a_48.X;
        let maxX_98;
        if (b_48.X > minX_98) {
            maxX_98 = b_48.X;
        }
        else {
            minX_98 = b_48.X;
            maxX_98 = a_48.X;
        }
        let minY_98 = a_48.Y;
        let maxY_98;
        if (b_48.Y > minY_98) {
            maxY_98 = b_48.Y;
        }
        else {
            minY_98 = b_48.Y;
            maxY_98 = a_48.Y;
        }
        r1_7 = BRect_$ctor_77D16AC0(minX_98, minY_98, maxX_98, maxY_98);
        let r2_7;
        const a_49 = Pt_$ctor_7B00E9A0(10, 0);
        const b_49 = Pt_$ctor_7B00E9A0(20, 10);
        let minX_100 = a_49.X;
        let maxX_100;
        if (b_49.X > minX_100) {
            maxX_100 = b_49.X;
        }
        else {
            minX_100 = b_49.X;
            maxX_100 = a_49.X;
        }
        let minY_100 = a_49.Y;
        let maxY_100;
        if (b_49.Y > minY_100) {
            maxY_100 = b_49.Y;
        }
        else {
            minY_100 = b_49.Y;
            maxY_100 = a_49.Y;
        }
        r2_7 = BRect_$ctor_77D16AC0(minX_100, minY_100, maxX_100, maxY_100);
        Expect_isTrue(BRect__IsTouching_13C371E0(r1_7, r2_7, 0.001))("Rectangles touching at edge should return true");
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching rejects overlapping", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let r1_8;
        const a_50 = Pt_$ctor_7B00E9A0(0, 0);
        const b_50 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_102 = a_50.X;
        let maxX_102;
        if (b_50.X > minX_102) {
            maxX_102 = b_50.X;
        }
        else {
            minX_102 = b_50.X;
            maxX_102 = a_50.X;
        }
        let minY_102 = a_50.Y;
        let maxY_102;
        if (b_50.Y > minY_102) {
            maxY_102 = b_50.Y;
        }
        else {
            minY_102 = b_50.Y;
            maxY_102 = a_50.Y;
        }
        r1_8 = BRect_$ctor_77D16AC0(minX_102, minY_102, maxX_102, maxY_102);
        let r2_8;
        const a_51 = Pt_$ctor_7B00E9A0(5, 5);
        const b_51 = Pt_$ctor_7B00E9A0(15, 15);
        let minX_104 = a_51.X;
        let maxX_104;
        if (b_51.X > minX_104) {
            maxX_104 = b_51.X;
        }
        else {
            minX_104 = b_51.X;
            maxX_104 = a_51.X;
        }
        let minY_104 = a_51.Y;
        let maxY_104;
        if (b_51.Y > minY_104) {
            maxY_104 = b_51.Y;
        }
        else {
            minY_104 = b_51.Y;
            maxY_104 = a_51.Y;
        }
        r2_8 = BRect_$ctor_77D16AC0(minX_104, minY_104, maxX_104, maxY_104);
        Expect_isFalse(BRect__IsTouching_13C371E0(r1_8, r2_8, 0.001))("Overlapping rectangles should not be \'touching\'");
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})()])), Test_testList("Intersection", ofArray([(() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection of overlapping rectangles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let r1_9;
        const a_52 = Pt_$ctor_7B00E9A0(0, 0);
        const b_52 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_106 = a_52.X;
        let maxX_106;
        if (b_52.X > minX_106) {
            maxX_106 = b_52.X;
        }
        else {
            minX_106 = b_52.X;
            maxX_106 = a_52.X;
        }
        let minY_106 = a_52.Y;
        let maxY_106;
        if (b_52.Y > minY_106) {
            maxY_106 = b_52.Y;
        }
        else {
            minY_106 = b_52.Y;
            maxY_106 = a_52.Y;
        }
        r1_9 = BRect_$ctor_77D16AC0(minX_106, minY_106, maxX_106, maxY_106);
        let r2_9;
        const a_53 = Pt_$ctor_7B00E9A0(5, 5);
        const b_53 = Pt_$ctor_7B00E9A0(15, 15);
        let minX_108 = a_53.X;
        let maxX_108;
        if (b_53.X > minX_108) {
            maxX_108 = b_53.X;
        }
        else {
            minX_108 = b_53.X;
            maxX_108 = a_53.X;
        }
        let minY_108 = a_53.Y;
        let maxY_108;
        if (b_53.Y > minY_108) {
            maxY_108 = b_53.Y;
        }
        else {
            minY_108 = b_53.Y;
            maxY_108 = a_53.Y;
        }
        r2_9 = BRect_$ctor_77D16AC0(minX_108, minY_108, maxX_108, maxY_108);
        let matchValue;
        const b_54 = r1_9;
        const a_54 = r2_9;
        const minX_110 = max(a_54.MinX, b_54.MinX);
        const minY_110 = max(a_54.MinY, b_54.MinY);
        const maxX_110 = min(a_54.MaxX, b_54.MaxX);
        const maxY_110 = min(a_54.MaxY, b_54.MaxY);
        matchValue = (((minX_110 <= maxX_110) && (minY_110 <= maxY_110)) ? BRect_$ctor_77D16AC0(minX_110, minY_110, maxX_110, maxY_110) : undefined);
        if (matchValue == null) {
            Test_failtest("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_38);
        }
        else {
            const inter = matchValue;
            const actual_59 = inter.MinX;
            if ((actual_59 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_59, 5, "Intersection MinX should be 5");
            }
            else {
                let valueType_59;
                let copyOfStruct_59 = actual_59;
                valueType_59 = float64_type;
                const primitiveTypes_59 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_59;
                if (contains(valueType_59, primitiveTypes_59, {
                    Equals: equals,
                    GetHashCode: (x_66) => (structuralHash(x_66) | 0),
                })) {
                    const arg_64 = (5).toString();
                    const arg_1_59 = actual_59.toString();
                    errorMsg_59 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_64)(arg_1_59)("Intersection MinX should be 5");
                }
                else {
                    errorMsg_59 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_59)("Intersection MinX should be 5");
                }
                throw new Exception(errorMsg_59);
            }
            const actual_60 = inter.MinY;
            if ((actual_60 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_60, 5, "Intersection MinY should be 5");
            }
            else {
                let valueType_60;
                let copyOfStruct_60 = actual_60;
                valueType_60 = float64_type;
                const primitiveTypes_60 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_60;
                if (contains(valueType_60, primitiveTypes_60, {
                    Equals: equals,
                    GetHashCode: (x_67) => (structuralHash(x_67) | 0),
                })) {
                    const arg_65 = (5).toString();
                    const arg_1_60 = actual_60.toString();
                    errorMsg_60 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_65)(arg_1_60)("Intersection MinY should be 5");
                }
                else {
                    errorMsg_60 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_60)("Intersection MinY should be 5");
                }
                throw new Exception(errorMsg_60);
            }
            const actual_61 = inter.MaxX;
            if ((actual_61 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_61, 10, "Intersection MaxX should be 10");
            }
            else {
                let valueType_61;
                let copyOfStruct_61 = actual_61;
                valueType_61 = float64_type;
                const primitiveTypes_61 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_61;
                if (contains(valueType_61, primitiveTypes_61, {
                    Equals: equals,
                    GetHashCode: (x_68) => (structuralHash(x_68) | 0),
                })) {
                    const arg_66 = (10).toString();
                    const arg_1_61 = actual_61.toString();
                    errorMsg_61 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_66)(arg_1_61)("Intersection MaxX should be 10");
                }
                else {
                    errorMsg_61 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_61)("Intersection MaxX should be 10");
                }
                throw new Exception(errorMsg_61);
            }
            const actual_62 = inter.MaxY;
            if ((actual_62 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_62, 10, "Intersection MaxY should be 10");
            }
            else {
                let valueType_62;
                let copyOfStruct_62 = actual_62;
                valueType_62 = float64_type;
                const primitiveTypes_62 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_62;
                if (contains(valueType_62, primitiveTypes_62, {
                    Equals: equals,
                    GetHashCode: (x_69) => (structuralHash(x_69) | 0),
                })) {
                    const arg_67 = (10).toString();
                    const arg_1_62 = actual_62.toString();
                    errorMsg_62 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_67)(arg_1_62)("Intersection MaxY should be 10");
                }
                else {
                    errorMsg_62 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_62)("Intersection MaxY should be 10");
                }
                throw new Exception(errorMsg_62);
            }
            Test_TestCaseBuilder__Zero(builder$0040_38);
        }
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection of touching rectangles returns zero-area BRect", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let r_64, r_65;
        let r1_10;
        const a_55 = Pt_$ctor_7B00E9A0(0, 0);
        const b_55 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_112 = a_55.X;
        let maxX_112;
        if (b_55.X > minX_112) {
            maxX_112 = b_55.X;
        }
        else {
            minX_112 = b_55.X;
            maxX_112 = a_55.X;
        }
        let minY_112 = a_55.Y;
        let maxY_112;
        if (b_55.Y > minY_112) {
            maxY_112 = b_55.Y;
        }
        else {
            minY_112 = b_55.Y;
            maxY_112 = a_55.Y;
        }
        r1_10 = BRect_$ctor_77D16AC0(minX_112, minY_112, maxX_112, maxY_112);
        let r2_10;
        const a_56 = Pt_$ctor_7B00E9A0(10, 0);
        const b_56 = Pt_$ctor_7B00E9A0(20, 10);
        let minX_114 = a_56.X;
        let maxX_114;
        if (b_56.X > minX_114) {
            maxX_114 = b_56.X;
        }
        else {
            minX_114 = b_56.X;
            maxX_114 = a_56.X;
        }
        let minY_114 = a_56.Y;
        let maxY_114;
        if (b_56.Y > minY_114) {
            maxY_114 = b_56.Y;
        }
        else {
            minY_114 = b_56.Y;
            maxY_114 = a_56.Y;
        }
        r2_10 = BRect_$ctor_77D16AC0(minX_114, minY_114, maxX_114, maxY_114);
        let matchValue_1;
        const b_57 = r1_10;
        const a_57 = r2_10;
        const minX_116 = max(a_57.MinX, b_57.MinX);
        const minY_116 = max(a_57.MinY, b_57.MinY);
        const maxX_116 = min(a_57.MaxX, b_57.MaxX);
        const maxY_116 = min(a_57.MaxY, b_57.MaxY);
        matchValue_1 = (((minX_116 <= maxX_116) && (minY_116 <= maxY_116)) ? BRect_$ctor_77D16AC0(minX_116, minY_116, maxX_116, maxY_116) : undefined);
        if (matchValue_1 == null) {
            Test_failtest("Touching rectangles should return zero-area intersection");
            Test_TestCaseBuilder__Zero(builder$0040_39);
        }
        else {
            const inter_1 = matchValue_1;
            const actual_63 = inter_1.MinX;
            if ((actual_63 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_63, 10, "Intersection MinX should be 10");
            }
            else {
                let valueType_63;
                let copyOfStruct_63 = actual_63;
                valueType_63 = float64_type;
                const primitiveTypes_63 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_63;
                if (contains(valueType_63, primitiveTypes_63, {
                    Equals: equals,
                    GetHashCode: (x_70) => (structuralHash(x_70) | 0),
                })) {
                    const arg_68 = (10).toString();
                    const arg_1_63 = actual_63.toString();
                    errorMsg_63 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_68)(arg_1_63)("Intersection MinX should be 10");
                }
                else {
                    errorMsg_63 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_63)("Intersection MinX should be 10");
                }
                throw new Exception(errorMsg_63);
            }
            const actual_64 = inter_1.MaxX;
            if ((actual_64 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_64, 10, "Intersection MaxX should be 10");
            }
            else {
                let valueType_64;
                let copyOfStruct_64 = actual_64;
                valueType_64 = float64_type;
                const primitiveTypes_64 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_64;
                if (contains(valueType_64, primitiveTypes_64, {
                    Equals: equals,
                    GetHashCode: (x_71) => (structuralHash(x_71) | 0),
                })) {
                    const arg_69 = (10).toString();
                    const arg_1_64 = actual_64.toString();
                    errorMsg_64 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_69)(arg_1_64)("Intersection MaxX should be 10");
                }
                else {
                    errorMsg_64 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_64)("Intersection MaxX should be 10");
                }
                throw new Exception(errorMsg_64);
            }
            let actual_65;
            const r_63 = inter_1;
            actual_65 = (((r_64 = r_63, r_64.MaxX - r_64.MinX)) * ((r_65 = r_63, r_65.MaxY - r_65.MinY)));
            if ((actual_65 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_65, 0, "Intersection should have zero area");
            }
            else {
                let valueType_65;
                let copyOfStruct_65 = actual_65;
                valueType_65 = float64_type;
                const primitiveTypes_65 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_65;
                if (contains(valueType_65, primitiveTypes_65, {
                    Equals: equals,
                    GetHashCode: (x_72) => (structuralHash(x_72) | 0),
                })) {
                    const arg_70 = (0).toString();
                    const arg_1_65 = actual_65.toString();
                    errorMsg_65 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_70)(arg_1_65)("Intersection should have zero area");
                }
                else {
                    errorMsg_65 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_65)("Intersection should have zero area");
                }
                throw new Exception(errorMsg_65);
            }
            Test_TestCaseBuilder__Zero(builder$0040_39);
        }
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection of non-overlapping rectangles returns None", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let r1_11;
        const a_58 = Pt_$ctor_7B00E9A0(0, 0);
        const b_58 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_118 = a_58.X;
        let maxX_118;
        if (b_58.X > minX_118) {
            maxX_118 = b_58.X;
        }
        else {
            minX_118 = b_58.X;
            maxX_118 = a_58.X;
        }
        let minY_118 = a_58.Y;
        let maxY_118;
        if (b_58.Y > minY_118) {
            maxY_118 = b_58.Y;
        }
        else {
            minY_118 = b_58.Y;
            maxY_118 = a_58.Y;
        }
        r1_11 = BRect_$ctor_77D16AC0(minX_118, minY_118, maxX_118, maxY_118);
        let r2_11;
        const a_59 = Pt_$ctor_7B00E9A0(11, 11);
        const b_59 = Pt_$ctor_7B00E9A0(20, 20);
        let minX_120 = a_59.X;
        let maxX_120;
        if (b_59.X > minX_120) {
            maxX_120 = b_59.X;
        }
        else {
            minX_120 = b_59.X;
            maxX_120 = a_59.X;
        }
        let minY_120 = a_59.Y;
        let maxY_120;
        if (b_59.Y > minY_120) {
            maxY_120 = b_59.Y;
        }
        else {
            minY_120 = b_59.Y;
            maxY_120 = a_59.Y;
        }
        r2_11 = BRect_$ctor_77D16AC0(minX_120, minY_120, maxX_120, maxY_120);
        let matchValue_2;
        const b_60 = r1_11;
        const a_60 = r2_11;
        const minX_122 = max(a_60.MinX, b_60.MinX);
        const minY_122 = max(a_60.MinY, b_60.MinY);
        const maxX_122 = min(a_60.MaxX, b_60.MaxX);
        const maxY_122 = min(a_60.MaxY, b_60.MaxY);
        matchValue_2 = (((minX_122 <= maxX_122) && (minY_122 <= maxY_122)) ? BRect_$ctor_77D16AC0(minX_122, minY_122, maxX_122, maxY_122) : undefined);
        if (matchValue_2 == null) {
            Test_TestCaseBuilder__Zero(builder$0040_40);
        }
        else {
            Test_failtest("Should not have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_40);
        }
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Static intersection method matches member method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let a_68, b_68;
        let r1_12;
        const a_61 = Pt_$ctor_7B00E9A0(0, 0);
        const b_61 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_124 = a_61.X;
        let maxX_124;
        if (b_61.X > minX_124) {
            maxX_124 = b_61.X;
        }
        else {
            minX_124 = b_61.X;
            maxX_124 = a_61.X;
        }
        let minY_124 = a_61.Y;
        let maxY_124;
        if (b_61.Y > minY_124) {
            maxY_124 = b_61.Y;
        }
        else {
            minY_124 = b_61.Y;
            maxY_124 = a_61.Y;
        }
        r1_12 = BRect_$ctor_77D16AC0(minX_124, minY_124, maxX_124, maxY_124);
        let r2_12;
        const a_62 = Pt_$ctor_7B00E9A0(5, 5);
        const b_62 = Pt_$ctor_7B00E9A0(15, 15);
        let minX_126 = a_62.X;
        let maxX_126;
        if (b_62.X > minX_126) {
            maxX_126 = b_62.X;
        }
        else {
            minX_126 = b_62.X;
            maxX_126 = a_62.X;
        }
        let minY_126 = a_62.Y;
        let maxY_126;
        if (b_62.Y > minY_126) {
            maxY_126 = b_62.Y;
        }
        else {
            minY_126 = b_62.Y;
            maxY_126 = a_62.Y;
        }
        r2_12 = BRect_$ctor_77D16AC0(minX_126, minY_126, maxX_126, maxY_126);
        let memberResult;
        const b_63 = r1_12;
        const a_63 = r2_12;
        const minX_128 = max(a_63.MinX, b_63.MinX);
        const minY_128 = max(a_63.MinY, b_63.MinY);
        const maxX_128 = min(a_63.MaxX, b_63.MaxX);
        const maxY_128 = min(a_63.MaxY, b_63.MaxY);
        memberResult = (((minX_128 <= maxX_128) && (minY_128 <= maxY_128)) ? BRect_$ctor_77D16AC0(minX_128, minY_128, maxX_128, maxY_128) : undefined);
        let staticResult;
        const b_66 = r1_12;
        const a_66 = r2_12;
        const minX_130 = max(a_66.MinX, b_66.MinX);
        const minY_130 = max(a_66.MinY, b_66.MinY);
        const maxX_130 = min(a_66.MaxX, b_66.MaxX);
        const maxY_130 = min(a_66.MaxY, b_66.MaxY);
        staticResult = (((minX_130 <= maxX_130) && (minY_130 <= maxY_130)) ? BRect_$ctor_77D16AC0(minX_130, minY_130, maxX_130, maxY_130) : undefined);
        let matchResult, m, s;
        const copyOfStruct_66 = memberResult;
        if (copyOfStruct_66 == null) {
            if (staticResult == null) {
                matchResult = 1;
            }
            else {
                matchResult = 2;
            }
        }
        else {
            const copyOfStruct_68 = staticResult;
            if (copyOfStruct_68 != null) {
                matchResult = 0;
                m = copyOfStruct_66;
                s = copyOfStruct_68;
            }
            else {
                matchResult = 2;
            }
        }
        switch (matchResult) {
            case 0: {
                Expect_isTrue((a_68 = m, (b_68 = s, (((Math.abs(a_68.MinX - b_68.MinX) <= 0) && (Math.abs(a_68.MinY - b_68.MinY) <= 0)) && (Math.abs(a_68.MaxX - b_68.MaxX) <= 0)) && (Math.abs(a_68.MaxY - b_68.MaxY) <= 0))))("Member and static methods should return same result");
                Test_TestCaseBuilder__Zero(builder$0040_41);
                break;
            }
            case 1: {
                Test_TestCaseBuilder__Zero(builder$0040_41);
                break;
            }
            case 2: {
                Test_failtest("Member and static methods should match");
                Test_TestCaseBuilder__Zero(builder$0040_41);
                break;
            }
        }
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Static intersection of touching rectangles matches member", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let a_76, b_76;
        let r1_13;
        const a_69 = Pt_$ctor_7B00E9A0(0, 0);
        const b_69 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_132 = a_69.X;
        let maxX_132;
        if (b_69.X > minX_132) {
            maxX_132 = b_69.X;
        }
        else {
            minX_132 = b_69.X;
            maxX_132 = a_69.X;
        }
        let minY_132 = a_69.Y;
        let maxY_132;
        if (b_69.Y > minY_132) {
            maxY_132 = b_69.Y;
        }
        else {
            minY_132 = b_69.Y;
            maxY_132 = a_69.Y;
        }
        r1_13 = BRect_$ctor_77D16AC0(minX_132, minY_132, maxX_132, maxY_132);
        let r2_13;
        const a_70 = Pt_$ctor_7B00E9A0(10, 0);
        const b_70 = Pt_$ctor_7B00E9A0(20, 10);
        let minX_134 = a_70.X;
        let maxX_134;
        if (b_70.X > minX_134) {
            maxX_134 = b_70.X;
        }
        else {
            minX_134 = b_70.X;
            maxX_134 = a_70.X;
        }
        let minY_134 = a_70.Y;
        let maxY_134;
        if (b_70.Y > minY_134) {
            maxY_134 = b_70.Y;
        }
        else {
            minY_134 = b_70.Y;
            maxY_134 = a_70.Y;
        }
        r2_13 = BRect_$ctor_77D16AC0(minX_134, minY_134, maxX_134, maxY_134);
        let memberResult_1;
        const b_71 = r1_13;
        const a_71 = r2_13;
        const minX_136 = max(a_71.MinX, b_71.MinX);
        const minY_136 = max(a_71.MinY, b_71.MinY);
        const maxX_136 = min(a_71.MaxX, b_71.MaxX);
        const maxY_136 = min(a_71.MaxY, b_71.MaxY);
        memberResult_1 = (((minX_136 <= maxX_136) && (minY_136 <= maxY_136)) ? BRect_$ctor_77D16AC0(minX_136, minY_136, maxX_136, maxY_136) : undefined);
        let staticResult_1;
        const b_74 = r1_13;
        const a_74 = r2_13;
        const minX_138 = max(a_74.MinX, b_74.MinX);
        const minY_138 = max(a_74.MinY, b_74.MinY);
        const maxX_138 = min(a_74.MaxX, b_74.MaxX);
        const maxY_138 = min(a_74.MaxY, b_74.MaxY);
        staticResult_1 = (((minX_138 <= maxX_138) && (minY_138 <= maxY_138)) ? BRect_$ctor_77D16AC0(minX_138, minY_138, maxX_138, maxY_138) : undefined);
        let matchResult_1, m_1, s_1;
        const copyOfStruct_69 = memberResult_1;
        if (copyOfStruct_69 == null) {
            if (staticResult_1 == null) {
                matchResult_1 = 1;
            }
            else {
                matchResult_1 = 2;
            }
        }
        else {
            const copyOfStruct_71 = staticResult_1;
            if (copyOfStruct_71 != null) {
                matchResult_1 = 0;
                m_1 = copyOfStruct_69;
                s_1 = copyOfStruct_71;
            }
            else {
                matchResult_1 = 2;
            }
        }
        switch (matchResult_1) {
            case 0: {
                Expect_isTrue((a_76 = m_1, (b_76 = s_1, (((Math.abs(a_76.MinX - b_76.MinX) <= 0) && (Math.abs(a_76.MinY - b_76.MinY) <= 0)) && (Math.abs(a_76.MaxX - b_76.MaxX) <= 0)) && (Math.abs(a_76.MaxY - b_76.MaxY) <= 0))))("Both should return zero-area intersection");
                Test_TestCaseBuilder__Zero(builder$0040_42);
                break;
            }
            case 1: {
                Test_failtest("Both should return zero-area intersection, not None");
                Test_TestCaseBuilder__Zero(builder$0040_42);
                break;
            }
            case 2: {
                Test_failtest("Member and static intersection methods should behave identically");
                Test_TestCaseBuilder__Zero(builder$0040_42);
                break;
            }
        }
    }));
})()])), Test_testList("Union", ofArray([(() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Union of two rectangles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        let r1_14;
        const a_77 = Pt_$ctor_7B00E9A0(0, 0);
        const b_77 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_140 = a_77.X;
        let maxX_140;
        if (b_77.X > minX_140) {
            maxX_140 = b_77.X;
        }
        else {
            minX_140 = b_77.X;
            maxX_140 = a_77.X;
        }
        let minY_140 = a_77.Y;
        let maxY_140;
        if (b_77.Y > minY_140) {
            maxY_140 = b_77.Y;
        }
        else {
            minY_140 = b_77.Y;
            maxY_140 = a_77.Y;
        }
        r1_14 = BRect_$ctor_77D16AC0(minX_140, minY_140, maxX_140, maxY_140);
        let r2_14;
        const a_78 = Pt_$ctor_7B00E9A0(5, 5);
        const b_78 = Pt_$ctor_7B00E9A0(15, 15);
        let minX_142 = a_78.X;
        let maxX_142;
        if (b_78.X > minX_142) {
            maxX_142 = b_78.X;
        }
        else {
            minX_142 = b_78.X;
            maxX_142 = a_78.X;
        }
        let minY_142 = a_78.Y;
        let maxY_142;
        if (b_78.Y > minY_142) {
            maxY_142 = b_78.Y;
        }
        else {
            minY_142 = b_78.Y;
            maxY_142 = a_78.Y;
        }
        r2_14 = BRect_$ctor_77D16AC0(minX_142, minY_142, maxX_142, maxY_142);
        let union;
        const r_66 = r1_14;
        const b_79 = r2_14;
        union = BRect_$ctor_77D16AC0(min(b_79.MinX, r_66.MinX), min(b_79.MinY, r_66.MinY), max(b_79.MaxX, r_66.MaxX), max(b_79.MaxY, r_66.MaxY));
        const actual_66 = union.MinX;
        if ((actual_66 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_66, 0, "Union MinX should be 0");
        }
        else {
            let valueType_66;
            let copyOfStruct_72 = actual_66;
            valueType_66 = float64_type;
            const primitiveTypes_66 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_66;
            if (contains(valueType_66, primitiveTypes_66, {
                Equals: equals,
                GetHashCode: (x_73) => (structuralHash(x_73) | 0),
            })) {
                const arg_71 = (0).toString();
                const arg_1_66 = actual_66.toString();
                errorMsg_66 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_71)(arg_1_66)("Union MinX should be 0");
            }
            else {
                errorMsg_66 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_66)("Union MinX should be 0");
            }
            throw new Exception(errorMsg_66);
        }
        const actual_67 = union.MinY;
        if ((actual_67 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_67, 0, "Union MinY should be 0");
        }
        else {
            let valueType_67;
            let copyOfStruct_73 = actual_67;
            valueType_67 = float64_type;
            const primitiveTypes_67 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_67;
            if (contains(valueType_67, primitiveTypes_67, {
                Equals: equals,
                GetHashCode: (x_74) => (structuralHash(x_74) | 0),
            })) {
                const arg_72 = (0).toString();
                const arg_1_67 = actual_67.toString();
                errorMsg_67 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_72)(arg_1_67)("Union MinY should be 0");
            }
            else {
                errorMsg_67 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_67)("Union MinY should be 0");
            }
            throw new Exception(errorMsg_67);
        }
        const actual_68 = union.MaxX;
        if ((actual_68 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_68, 15, "Union MaxX should be 15");
        }
        else {
            let valueType_68;
            let copyOfStruct_74 = actual_68;
            valueType_68 = float64_type;
            const primitiveTypes_68 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_68;
            if (contains(valueType_68, primitiveTypes_68, {
                Equals: equals,
                GetHashCode: (x_75) => (structuralHash(x_75) | 0),
            })) {
                const arg_73 = (15).toString();
                const arg_1_68 = actual_68.toString();
                errorMsg_68 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_73)(arg_1_68)("Union MaxX should be 15");
            }
            else {
                errorMsg_68 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_68)("Union MaxX should be 15");
            }
            throw new Exception(errorMsg_68);
        }
        const actual_69 = union.MaxY;
        if ((actual_69 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_69, 15, "Union MaxY should be 15");
        }
        else {
            let valueType_69;
            let copyOfStruct_75 = actual_69;
            valueType_69 = float64_type;
            const primitiveTypes_69 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_69;
            if (contains(valueType_69, primitiveTypes_69, {
                Equals: equals,
                GetHashCode: (x_76) => (structuralHash(x_76) | 0),
            })) {
                const arg_74 = (15).toString();
                const arg_1_69 = actual_69.toString();
                errorMsg_69 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_74)(arg_1_69)("Union MaxY should be 15");
            }
            else {
                errorMsg_69 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_69)("Union MaxY should be 15");
            }
            throw new Exception(errorMsg_69);
        }
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Union with point inside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let a_81, b_82;
        let r_67;
        const a_79 = Pt_$ctor_7B00E9A0(0, 0);
        const b_80 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_145 = a_79.X;
        let maxX_145;
        if (b_80.X > minX_145) {
            maxX_145 = b_80.X;
        }
        else {
            minX_145 = b_80.X;
            maxX_145 = a_79.X;
        }
        let minY_145 = a_79.Y;
        let maxY_145;
        if (b_80.Y > minY_145) {
            maxY_145 = b_80.Y;
        }
        else {
            minY_145 = b_80.Y;
            maxY_145 = a_79.Y;
        }
        r_67 = BRect_$ctor_77D16AC0(minX_145, minY_145, maxX_145, maxY_145);
        let union_1;
        const r_68 = r_67;
        const p_10 = Pt_$ctor_7B00E9A0(5, 5);
        union_1 = BRect_$ctor_77D16AC0(min(r_68.MinX, p_10.X), min(r_68.MinY, p_10.Y), max(r_68.MaxX, p_10.X), max(r_68.MaxY, p_10.Y));
        Expect_isTrue((a_81 = r_67, (b_82 = union_1, (((Math.abs(a_81.MinX - b_82.MinX) <= 0) && (Math.abs(a_81.MinY - b_82.MinY) <= 0)) && (Math.abs(a_81.MaxX - b_82.MaxX) <= 0)) && (Math.abs(a_81.MaxY - b_82.MaxY) <= 0))))("Union with interior point should not change rectangle");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Union with point outside", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let r_69;
        const a_82 = Pt_$ctor_7B00E9A0(0, 0);
        const b_83 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_148 = a_82.X;
        let maxX_148;
        if (b_83.X > minX_148) {
            maxX_148 = b_83.X;
        }
        else {
            minX_148 = b_83.X;
            maxX_148 = a_82.X;
        }
        let minY_148 = a_82.Y;
        let maxY_148;
        if (b_83.Y > minY_148) {
            maxY_148 = b_83.Y;
        }
        else {
            minY_148 = b_83.Y;
            maxY_148 = a_82.Y;
        }
        r_69 = BRect_$ctor_77D16AC0(minX_148, minY_148, maxX_148, maxY_148);
        let union_2;
        const r_70 = r_69;
        const p_11 = Pt_$ctor_7B00E9A0(15, 15);
        union_2 = BRect_$ctor_77D16AC0(min(r_70.MinX, p_11.X), min(r_70.MinY, p_11.Y), max(r_70.MaxX, p_11.X), max(r_70.MaxY, p_11.Y));
        const actual_70 = union_2.MinX;
        if ((actual_70 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_70, 0, "Union MinX should be 0");
        }
        else {
            let valueType_70;
            let copyOfStruct_76 = actual_70;
            valueType_70 = float64_type;
            const primitiveTypes_70 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_70;
            if (contains(valueType_70, primitiveTypes_70, {
                Equals: equals,
                GetHashCode: (x_77) => (structuralHash(x_77) | 0),
            })) {
                const arg_75 = (0).toString();
                const arg_1_70 = actual_70.toString();
                errorMsg_70 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_75)(arg_1_70)("Union MinX should be 0");
            }
            else {
                errorMsg_70 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_70)("Union MinX should be 0");
            }
            throw new Exception(errorMsg_70);
        }
        const actual_71 = union_2.MaxX;
        if ((actual_71 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_71, 15, "Union MaxX should be 15");
        }
        else {
            let valueType_71;
            let copyOfStruct_77 = actual_71;
            valueType_71 = float64_type;
            const primitiveTypes_71 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_71;
            if (contains(valueType_71, primitiveTypes_71, {
                Equals: equals,
                GetHashCode: (x_78) => (structuralHash(x_78) | 0),
            })) {
                const arg_76 = (15).toString();
                const arg_1_71 = actual_71.toString();
                errorMsg_71 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_76)(arg_1_71)("Union MaxX should be 15");
            }
            else {
                errorMsg_71 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_71)("Union MaxX should be 15");
            }
            throw new Exception(errorMsg_71);
        }
        const actual_72 = union_2.MaxY;
        if ((actual_72 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_72, 15, "Union MaxY should be 15");
        }
        else {
            let valueType_72;
            let copyOfStruct_78 = actual_72;
            valueType_72 = float64_type;
            const primitiveTypes_72 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_72;
            if (contains(valueType_72, primitiveTypes_72, {
                Equals: equals,
                GetHashCode: (x_79) => (structuralHash(x_79) | 0),
            })) {
                const arg_77 = (15).toString();
                const arg_1_72 = actual_72.toString();
                errorMsg_72 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_77)(arg_1_72)("Union MaxY should be 15");
            }
            else {
                errorMsg_72 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_72)("Union MaxY should be 15");
            }
            throw new Exception(errorMsg_72);
        }
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})()])), Test_testList("Scale and Transform", ofArray([(() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale with positive factor", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        let r_71;
        const a_83 = Pt_$ctor_7B00E9A0(1, 2);
        const b_84 = Pt_$ctor_7B00E9A0(5, 6);
        let minX_151 = a_83.X;
        let maxX_151;
        if (b_84.X > minX_151) {
            maxX_151 = b_84.X;
        }
        else {
            minX_151 = b_84.X;
            maxX_151 = a_83.X;
        }
        let minY_151 = a_83.Y;
        let maxY_151;
        if (b_84.Y > minY_151) {
            maxY_151 = b_84.Y;
        }
        else {
            minY_151 = b_84.Y;
            maxY_151 = a_83.Y;
        }
        r_71 = BRect_$ctor_77D16AC0(minX_151, minY_151, maxX_151, maxY_151);
        let scaled;
        const r_73 = r_71;
        if (2 < 0) {
            fail(`BRect.scale: Negative factor ${2} is not allowed, would flip the rectangle on ${BRect__get_AsString(r_73)}`);
        }
        scaled = BRect_$ctor_77D16AC0(r_73.MinX * 2, r_73.MinY * 2, r_73.MaxX * 2, r_73.MaxY * 2);
        const actual_73 = scaled.MinX;
        if ((actual_73 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_73, 2, "MinX should be 2");
        }
        else {
            let valueType_73;
            let copyOfStruct_79 = actual_73;
            valueType_73 = float64_type;
            const primitiveTypes_73 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_73;
            if (contains(valueType_73, primitiveTypes_73, {
                Equals: equals,
                GetHashCode: (x_80) => (structuralHash(x_80) | 0),
            })) {
                const arg_78 = (2).toString();
                const arg_1_73 = actual_73.toString();
                errorMsg_73 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_78)(arg_1_73)("MinX should be 2");
            }
            else {
                errorMsg_73 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_73)("MinX should be 2");
            }
            throw new Exception(errorMsg_73);
        }
        const actual_74 = scaled.MinY;
        if ((actual_74 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_74, 4, "MinY should be 4");
        }
        else {
            let valueType_74;
            let copyOfStruct_80 = actual_74;
            valueType_74 = float64_type;
            const primitiveTypes_74 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_74;
            if (contains(valueType_74, primitiveTypes_74, {
                Equals: equals,
                GetHashCode: (x_81) => (structuralHash(x_81) | 0),
            })) {
                const arg_79 = (4).toString();
                const arg_1_74 = actual_74.toString();
                errorMsg_74 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_79)(arg_1_74)("MinY should be 4");
            }
            else {
                errorMsg_74 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_74)("MinY should be 4");
            }
            throw new Exception(errorMsg_74);
        }
        const actual_75 = scaled.MaxX;
        if ((actual_75 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_75, 10, "MaxX should be 10");
        }
        else {
            let valueType_75;
            let copyOfStruct_81 = actual_75;
            valueType_75 = float64_type;
            const primitiveTypes_75 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_75;
            if (contains(valueType_75, primitiveTypes_75, {
                Equals: equals,
                GetHashCode: (x_82) => (structuralHash(x_82) | 0),
            })) {
                const arg_80 = (10).toString();
                const arg_1_75 = actual_75.toString();
                errorMsg_75 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_80)(arg_1_75)("MaxX should be 10");
            }
            else {
                errorMsg_75 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_75)("MaxX should be 10");
            }
            throw new Exception(errorMsg_75);
        }
        const actual_76 = scaled.MaxY;
        if ((actual_76 === 12) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_76, 12, "MaxY should be 12");
        }
        else {
            let valueType_76;
            let copyOfStruct_82 = actual_76;
            valueType_76 = float64_type;
            const primitiveTypes_76 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_76;
            if (contains(valueType_76, primitiveTypes_76, {
                Equals: equals,
                GetHashCode: (x_83) => (structuralHash(x_83) | 0),
            })) {
                const arg_81 = (12).toString();
                const arg_1_76 = actual_76.toString();
                errorMsg_76 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_81)(arg_1_76)("MaxY should be 12");
            }
            else {
                errorMsg_76 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(12)(actual_76)("MaxY should be 12");
            }
            throw new Exception(errorMsg_76);
        }
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale with zero factor", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let r_74;
        const a_84 = Pt_$ctor_7B00E9A0(1, 2);
        const b_85 = Pt_$ctor_7B00E9A0(5, 6);
        let minX_154 = a_84.X;
        let maxX_154;
        if (b_85.X > minX_154) {
            maxX_154 = b_85.X;
        }
        else {
            minX_154 = b_85.X;
            maxX_154 = a_84.X;
        }
        let minY_154 = a_84.Y;
        let maxY_154;
        if (b_85.Y > minY_154) {
            maxY_154 = b_85.Y;
        }
        else {
            minY_154 = b_85.Y;
            maxY_154 = a_84.Y;
        }
        r_74 = BRect_$ctor_77D16AC0(minX_154, minY_154, maxX_154, maxY_154);
        let scaled_1;
        const r_76 = r_74;
        if (0 < 0) {
            fail(`BRect.scale: Negative factor ${0} is not allowed, would flip the rectangle on ${BRect__get_AsString(r_76)}`);
        }
        scaled_1 = BRect_$ctor_77D16AC0(r_76.MinX * 0, r_76.MinY * 0, r_76.MaxX * 0, r_76.MaxY * 0);
        const actual_77 = scaled_1.MinX;
        if ((actual_77 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_77, 0, "All coordinates should be 0");
        }
        else {
            let valueType_77;
            let copyOfStruct_83 = actual_77;
            valueType_77 = float64_type;
            const primitiveTypes_77 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_77;
            if (contains(valueType_77, primitiveTypes_77, {
                Equals: equals,
                GetHashCode: (x_84) => (structuralHash(x_84) | 0),
            })) {
                const arg_82 = (0).toString();
                const arg_1_77 = actual_77.toString();
                errorMsg_77 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_82)(arg_1_77)("All coordinates should be 0");
            }
            else {
                errorMsg_77 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_77)("All coordinates should be 0");
            }
            throw new Exception(errorMsg_77);
        }
        const actual_78 = scaled_1.MaxX;
        if ((actual_78 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_78, 0, "All coordinates should be 0");
        }
        else {
            let valueType_78;
            let copyOfStruct_84 = actual_78;
            valueType_78 = float64_type;
            const primitiveTypes_78 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_78;
            if (contains(valueType_78, primitiveTypes_78, {
                Equals: equals,
                GetHashCode: (x_85) => (structuralHash(x_85) | 0),
            })) {
                const arg_83 = (0).toString();
                const arg_1_78 = actual_78.toString();
                errorMsg_78 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_83)(arg_1_78)("All coordinates should be 0");
            }
            else {
                errorMsg_78 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_78)("All coordinates should be 0");
            }
            throw new Exception(errorMsg_78);
        }
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale with negative factor throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        let r_77;
        const a_85 = Pt_$ctor_7B00E9A0(1, 2);
        const b_86 = Pt_$ctor_7B00E9A0(5, 6);
        let minX_157 = a_85.X;
        let maxX_157;
        if (b_86.X > minX_157) {
            maxX_157 = b_86.X;
        }
        else {
            minX_157 = b_86.X;
            maxX_157 = a_85.X;
        }
        let minY_157 = a_85.Y;
        let maxY_157;
        if (b_86.Y > minY_157) {
            maxY_157 = b_86.Y;
        }
        else {
            minY_157 = b_86.Y;
            maxY_157 = a_85.Y;
        }
        r_77 = BRect_$ctor_77D16AC0(minX_157, minY_157, maxX_157, maxY_157);
        Expect_throws(() => {
            let r_79;
            (r_79 = r_77, ((-2 < 0) ? fail(`BRect.scale: Negative factor ${-2} is not allowed, would flip the rectangle on ${BRect__get_AsString(r_79)}`) : undefined, BRect_$ctor_77D16AC0(r_79.MinX * -2, r_79.MinY * -2, r_79.MaxX * -2, r_79.MaxY * -2)));
        }, "Should throw on negative scale factor");
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move by vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        let r_80;
        const a_86 = Pt_$ctor_7B00E9A0(0, 0);
        const b_87 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_160 = a_86.X;
        let maxX_160;
        if (b_87.X > minX_160) {
            maxX_160 = b_87.X;
        }
        else {
            minX_160 = b_87.X;
            maxX_160 = a_86.X;
        }
        let minY_160 = a_86.Y;
        let maxY_160;
        if (b_87.Y > minY_160) {
            maxY_160 = b_87.Y;
        }
        else {
            minY_160 = b_87.Y;
            maxY_160 = a_86.Y;
        }
        r_80 = BRect_$ctor_77D16AC0(minX_160, minY_160, maxX_160, maxY_160);
        let moved;
        const v_3 = Vc_$ctor_7B00E9A0_1(5, 3);
        const r_82 = r_80;
        moved = BRect_$ctor_77D16AC0(r_82.MinX + v_3.X, r_82.MinY + v_3.Y, r_82.MaxX + v_3.X, r_82.MaxY + v_3.Y);
        const actual_79 = moved.MinX;
        if ((actual_79 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_79, 5, "MinX should be 5");
        }
        else {
            let valueType_79;
            let copyOfStruct_85 = actual_79;
            valueType_79 = float64_type;
            const primitiveTypes_79 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_79;
            if (contains(valueType_79, primitiveTypes_79, {
                Equals: equals,
                GetHashCode: (x_86) => (structuralHash(x_86) | 0),
            })) {
                const arg_84 = (5).toString();
                const arg_1_79 = actual_79.toString();
                errorMsg_79 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_84)(arg_1_79)("MinX should be 5");
            }
            else {
                errorMsg_79 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_79)("MinX should be 5");
            }
            throw new Exception(errorMsg_79);
        }
        const actual_80 = moved.MinY;
        if ((actual_80 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_80, 3, "MinY should be 3");
        }
        else {
            let valueType_80;
            let copyOfStruct_86 = actual_80;
            valueType_80 = float64_type;
            const primitiveTypes_80 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_80;
            if (contains(valueType_80, primitiveTypes_80, {
                Equals: equals,
                GetHashCode: (x_87) => (structuralHash(x_87) | 0),
            })) {
                const arg_85 = (3).toString();
                const arg_1_80 = actual_80.toString();
                errorMsg_80 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_85)(arg_1_80)("MinY should be 3");
            }
            else {
                errorMsg_80 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_80)("MinY should be 3");
            }
            throw new Exception(errorMsg_80);
        }
        const actual_81 = moved.MaxX;
        if ((actual_81 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_81, 15, "MaxX should be 15");
        }
        else {
            let valueType_81;
            let copyOfStruct_87 = actual_81;
            valueType_81 = float64_type;
            const primitiveTypes_81 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_81;
            if (contains(valueType_81, primitiveTypes_81, {
                Equals: equals,
                GetHashCode: (x_88) => (structuralHash(x_88) | 0),
            })) {
                const arg_86 = (15).toString();
                const arg_1_81 = actual_81.toString();
                errorMsg_81 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_86)(arg_1_81)("MaxX should be 15");
            }
            else {
                errorMsg_81 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_81)("MaxX should be 15");
            }
            throw new Exception(errorMsg_81);
        }
        const actual_82 = moved.MaxY;
        if ((actual_82 === 13) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_82, 13, "MaxY should be 13");
        }
        else {
            let valueType_82;
            let copyOfStruct_88 = actual_82;
            valueType_82 = float64_type;
            const primitiveTypes_82 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_82;
            if (contains(valueType_82, primitiveTypes_82, {
                Equals: equals,
                GetHashCode: (x_89) => (structuralHash(x_89) | 0),
            })) {
                const arg_87 = (13).toString();
                const arg_1_82 = actual_82.toString();
                errorMsg_82 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_87)(arg_1_82)("MaxY should be 13");
            }
            else {
                errorMsg_82 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(13)(actual_82)("MaxY should be 13");
            }
            throw new Exception(errorMsg_82);
        }
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translate is same as move", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        let a_89, b_90;
        let r_83;
        const a_87 = Pt_$ctor_7B00E9A0(0, 0);
        const b_88 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_163 = a_87.X;
        let maxX_163;
        if (b_88.X > minX_163) {
            maxX_163 = b_88.X;
        }
        else {
            minX_163 = b_88.X;
            maxX_163 = a_87.X;
        }
        let minY_163 = a_87.Y;
        let maxY_163;
        if (b_88.Y > minY_163) {
            maxY_163 = b_88.Y;
        }
        else {
            minY_163 = b_88.Y;
            maxY_163 = a_87.Y;
        }
        r_83 = BRect_$ctor_77D16AC0(minX_163, minY_163, maxX_163, maxY_163);
        let moved_1;
        const v_5 = Vc_$ctor_7B00E9A0_1(5, 3);
        const r_85 = r_83;
        moved_1 = BRect_$ctor_77D16AC0(r_85.MinX + v_5.X, r_85.MinY + v_5.Y, r_85.MaxX + v_5.X, r_85.MaxY + v_5.Y);
        let translated;
        const v_7 = Vc_$ctor_7B00E9A0_1(5, 3);
        const r_87 = r_83;
        translated = BRect_$ctor_77D16AC0(r_87.MinX + v_7.X, r_87.MinY + v_7.Y, r_87.MaxX + v_7.X, r_87.MaxY + v_7.Y);
        Expect_isTrue((a_89 = moved_1, (b_90 = translated, (((Math.abs(a_89.MinX - b_90.MinX) <= 0) && (Math.abs(a_89.MinY - b_90.MinY) <= 0)) && (Math.abs(a_89.MaxX - b_90.MaxX) <= 0)) && (Math.abs(a_89.MaxY - b_90.MaxY) <= 0))))("move and translate should be identical");
        Test_TestCaseBuilder__Zero(builder$0040_50);
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        let r_88;
        const a_90 = Pt_$ctor_7B00E9A0(0, 0);
        const b_91 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_167 = a_90.X;
        let maxX_167;
        if (b_91.X > minX_167) {
            maxX_167 = b_91.X;
        }
        else {
            minX_167 = b_91.X;
            maxX_167 = a_90.X;
        }
        let minY_167 = a_90.Y;
        let maxY_167;
        if (b_91.Y > minY_167) {
            maxY_167 = b_91.Y;
        }
        else {
            minY_167 = b_91.Y;
            maxY_167 = a_90.Y;
        }
        r_88 = BRect_$ctor_77D16AC0(minX_167, minY_167, maxX_167, maxY_167);
        let moved_2;
        const r_90 = r_88;
        moved_2 = BRect_$ctor_77D16AC0(r_90.MinX + 5, r_90.MinY, r_90.MaxX + 5, r_90.MaxY);
        const actual_83 = moved_2.MinX;
        if ((actual_83 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_83, 5, "MinX should be 5");
        }
        else {
            let valueType_83;
            let copyOfStruct_89 = actual_83;
            valueType_83 = float64_type;
            const primitiveTypes_83 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_83;
            if (contains(valueType_83, primitiveTypes_83, {
                Equals: equals,
                GetHashCode: (x_90) => (structuralHash(x_90) | 0),
            })) {
                const arg_88 = (5).toString();
                const arg_1_83 = actual_83.toString();
                errorMsg_83 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_88)(arg_1_83)("MinX should be 5");
            }
            else {
                errorMsg_83 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_83)("MinX should be 5");
            }
            throw new Exception(errorMsg_83);
        }
        const actual_84 = moved_2.MinY;
        if ((actual_84 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_84, 0, "MinY should remain 0");
        }
        else {
            let valueType_84;
            let copyOfStruct_90 = actual_84;
            valueType_84 = float64_type;
            const primitiveTypes_84 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_84;
            if (contains(valueType_84, primitiveTypes_84, {
                Equals: equals,
                GetHashCode: (x_91) => (structuralHash(x_91) | 0),
            })) {
                const arg_89 = (0).toString();
                const arg_1_84 = actual_84.toString();
                errorMsg_84 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_89)(arg_1_84)("MinY should remain 0");
            }
            else {
                errorMsg_84 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_84)("MinY should remain 0");
            }
            throw new Exception(errorMsg_84);
        }
        const actual_85 = moved_2.MaxX;
        if ((actual_85 === 15) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_85, 15, "MaxX should be 15");
        }
        else {
            let valueType_85;
            let copyOfStruct_91 = actual_85;
            valueType_85 = float64_type;
            const primitiveTypes_85 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_85;
            if (contains(valueType_85, primitiveTypes_85, {
                Equals: equals,
                GetHashCode: (x_92) => (structuralHash(x_92) | 0),
            })) {
                const arg_90 = (15).toString();
                const arg_1_85 = actual_85.toString();
                errorMsg_85 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_90)(arg_1_85)("MaxX should be 15");
            }
            else {
                errorMsg_85 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(15)(actual_85)("MaxX should be 15");
            }
            throw new Exception(errorMsg_85);
        }
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        let r_91;
        const a_91 = Pt_$ctor_7B00E9A0(0, 0);
        const b_92 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_170 = a_91.X;
        let maxX_170;
        if (b_92.X > minX_170) {
            maxX_170 = b_92.X;
        }
        else {
            minX_170 = b_92.X;
            maxX_170 = a_91.X;
        }
        let minY_170 = a_91.Y;
        let maxY_170;
        if (b_92.Y > minY_170) {
            maxY_170 = b_92.Y;
        }
        else {
            minY_170 = b_92.Y;
            maxY_170 = a_91.Y;
        }
        r_91 = BRect_$ctor_77D16AC0(minX_170, minY_170, maxX_170, maxY_170);
        let moved_3;
        const r_93 = r_91;
        moved_3 = BRect_$ctor_77D16AC0(r_93.MinX, r_93.MinY + 3, r_93.MaxX, r_93.MaxY + 3);
        const actual_86 = moved_3.MinX;
        if ((actual_86 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_86, 0, "MinX should remain 0");
        }
        else {
            let valueType_86;
            let copyOfStruct_92 = actual_86;
            valueType_86 = float64_type;
            const primitiveTypes_86 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_86;
            if (contains(valueType_86, primitiveTypes_86, {
                Equals: equals,
                GetHashCode: (x_93) => (structuralHash(x_93) | 0),
            })) {
                const arg_91 = (0).toString();
                const arg_1_86 = actual_86.toString();
                errorMsg_86 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_91)(arg_1_86)("MinX should remain 0");
            }
            else {
                errorMsg_86 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_86)("MinX should remain 0");
            }
            throw new Exception(errorMsg_86);
        }
        const actual_87 = moved_3.MinY;
        if ((actual_87 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_87, 3, "MinY should be 3");
        }
        else {
            let valueType_87;
            let copyOfStruct_93 = actual_87;
            valueType_87 = float64_type;
            const primitiveTypes_87 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_87;
            if (contains(valueType_87, primitiveTypes_87, {
                Equals: equals,
                GetHashCode: (x_94) => (structuralHash(x_94) | 0),
            })) {
                const arg_92 = (3).toString();
                const arg_1_87 = actual_87.toString();
                errorMsg_87 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_92)(arg_1_87)("MinY should be 3");
            }
            else {
                errorMsg_87 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_87)("MinY should be 3");
            }
            throw new Exception(errorMsg_87);
        }
        const actual_88 = moved_3.MaxY;
        if ((actual_88 === 13) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_88, 13, "MaxY should be 13");
        }
        else {
            let valueType_88;
            let copyOfStruct_94 = actual_88;
            valueType_88 = float64_type;
            const primitiveTypes_88 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_88;
            if (contains(valueType_88, primitiveTypes_88, {
                Equals: equals,
                GetHashCode: (x_95) => (structuralHash(x_95) | 0),
            })) {
                const arg_93 = (13).toString();
                const arg_1_88 = actual_88.toString();
                errorMsg_88 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_93)(arg_1_88)("MaxY should be 13");
            }
            else {
                errorMsg_88 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(13)(actual_88)("MaxY should be 13");
            }
            throw new Exception(errorMsg_88);
        }
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})()])), Test_testList("Relative Expansion", ofArray([(() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRel with factor 1.0 keeps same size", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        let a_94, b_95;
        let r_94;
        const a_92 = Pt_$ctor_7B00E9A0(0, 0);
        const b_93 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_173 = a_92.X;
        let maxX_173;
        if (b_93.X > minX_173) {
            maxX_173 = b_93.X;
        }
        else {
            minX_173 = b_93.X;
            maxX_173 = a_92.X;
        }
        let minY_173 = a_92.Y;
        let maxY_173;
        if (b_93.Y > minY_173) {
            maxY_173 = b_93.Y;
        }
        else {
            minY_173 = b_93.Y;
            maxY_173 = a_92.Y;
        }
        r_94 = BRect_$ctor_77D16AC0(minX_173, minY_173, maxX_173, maxY_173);
        const expanded_5 = BRect_expandRel(1, r_94);
        Expect_isTrue((a_94 = r_94, (b_95 = expanded_5, (((Math.abs(a_94.MinX - b_95.MinX) <= 0.001) && (Math.abs(a_94.MinY - b_95.MinY) <= 0.001)) && (Math.abs(a_94.MaxX - b_95.MaxX) <= 0.001)) && (Math.abs(a_94.MaxY - b_95.MaxY) <= 0.001))))("Factor 1.0 should keep same size");
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRel with factor 2.0 doubles size", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        let a_97, r_100, b_98, r_101, vx_3, vy_3;
        let r_96;
        const a_95 = Pt_$ctor_7B00E9A0(0, 0);
        const b_96 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_175 = a_95.X;
        let maxX_175;
        if (b_96.X > minX_175) {
            maxX_175 = b_96.X;
        }
        else {
            minX_175 = b_96.X;
            maxX_175 = a_95.X;
        }
        let minY_175 = a_95.Y;
        let maxY_175;
        if (b_96.Y > minY_175) {
            maxY_175 = b_96.Y;
        }
        else {
            minY_175 = b_96.Y;
            maxY_175 = a_95.Y;
        }
        r_96 = BRect_$ctor_77D16AC0(minX_175, minY_175, maxX_175, maxY_175);
        const expanded_6 = BRect_expandRel(2, r_96);
        let actual_89;
        const r_98 = expanded_6;
        actual_89 = (r_98.MaxX - r_98.MinX);
        if ((actual_89 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_89, 20, "SizeX should double");
        }
        else {
            let valueType_89;
            let copyOfStruct_95 = actual_89;
            valueType_89 = float64_type;
            const primitiveTypes_89 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_89;
            if (contains(valueType_89, primitiveTypes_89, {
                Equals: equals,
                GetHashCode: (x_96) => (structuralHash(x_96) | 0),
            })) {
                const arg_94 = (20).toString();
                const arg_1_89 = actual_89.toString();
                errorMsg_89 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_94)(arg_1_89)("SizeX should double");
            }
            else {
                errorMsg_89 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_89)("SizeX should double");
            }
            throw new Exception(errorMsg_89);
        }
        let actual_90;
        const r_99 = expanded_6;
        actual_90 = (r_99.MaxY - r_99.MinY);
        if ((actual_90 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_90, 20, "SizeY should double");
        }
        else {
            let valueType_90;
            let copyOfStruct_96 = actual_90;
            valueType_90 = float64_type;
            const primitiveTypes_90 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_90;
            if (contains(valueType_90, primitiveTypes_90, {
                Equals: equals,
                GetHashCode: (x_97) => (structuralHash(x_97) | 0),
            })) {
                const arg_95 = (20).toString();
                const arg_1_90 = actual_90.toString();
                errorMsg_90 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_95)(arg_1_90)("SizeY should double");
            }
            else {
                errorMsg_90 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_90)("SizeY should double");
            }
            throw new Exception(errorMsg_90);
        }
        Expect_isTrue(((a_97 = ((r_100 = expanded_6, Pt_$ctor_7B00E9A0_1((r_100.MaxX + r_100.MinX) * 0.5, (r_100.MaxY + r_100.MinY) * 0.5))), (b_98 = ((r_101 = r_96, Pt_$ctor_7B00E9A0_1((r_101.MaxX + r_101.MinX) * 0.5, (r_101.MaxY + r_101.MinY) * 0.5))), (vx_3 = (a_97.X - b_98.X), (vy_3 = (a_97.Y - b_98.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09)("Center should remain the same");
        Test_TestCaseBuilder__Zero(builder$0040_54);
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRel with factor 0.5 halves size", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        let a_100, r_106, b_101, r_107, vx_4, vy_4;
        let r_102;
        const a_98 = Pt_$ctor_7B00E9A0(0, 0);
        const b_99 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_177 = a_98.X;
        let maxX_177;
        if (b_99.X > minX_177) {
            maxX_177 = b_99.X;
        }
        else {
            minX_177 = b_99.X;
            maxX_177 = a_98.X;
        }
        let minY_177 = a_98.Y;
        let maxY_177;
        if (b_99.Y > minY_177) {
            maxY_177 = b_99.Y;
        }
        else {
            minY_177 = b_99.Y;
            maxY_177 = a_98.Y;
        }
        r_102 = BRect_$ctor_77D16AC0(minX_177, minY_177, maxX_177, maxY_177);
        const expanded_7 = BRect_expandRel(0.5, r_102);
        let actual_91;
        const r_104 = expanded_7;
        actual_91 = (r_104.MaxX - r_104.MinX);
        if ((actual_91 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_91, 5, "SizeX should halve");
        }
        else {
            let valueType_91;
            let copyOfStruct_97 = actual_91;
            valueType_91 = float64_type;
            const primitiveTypes_91 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_91;
            if (contains(valueType_91, primitiveTypes_91, {
                Equals: equals,
                GetHashCode: (x_98) => (structuralHash(x_98) | 0),
            })) {
                const arg_96 = (5).toString();
                const arg_1_91 = actual_91.toString();
                errorMsg_91 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_96)(arg_1_91)("SizeX should halve");
            }
            else {
                errorMsg_91 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_91)("SizeX should halve");
            }
            throw new Exception(errorMsg_91);
        }
        let actual_92;
        const r_105 = expanded_7;
        actual_92 = (r_105.MaxY - r_105.MinY);
        if ((actual_92 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_92, 5, "SizeY should halve");
        }
        else {
            let valueType_92;
            let copyOfStruct_98 = actual_92;
            valueType_92 = float64_type;
            const primitiveTypes_92 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_92;
            if (contains(valueType_92, primitiveTypes_92, {
                Equals: equals,
                GetHashCode: (x_99) => (structuralHash(x_99) | 0),
            })) {
                const arg_97 = (5).toString();
                const arg_1_92 = actual_92.toString();
                errorMsg_92 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_97)(arg_1_92)("SizeY should halve");
            }
            else {
                errorMsg_92 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_92)("SizeY should halve");
            }
            throw new Exception(errorMsg_92);
        }
        Expect_isTrue(((a_100 = ((r_106 = expanded_7, Pt_$ctor_7B00E9A0_1((r_106.MaxX + r_106.MinX) * 0.5, (r_106.MaxY + r_106.MinY) * 0.5))), (b_101 = ((r_107 = r_102, Pt_$ctor_7B00E9A0_1((r_107.MaxX + r_107.MinX) * 0.5, (r_107.MaxY + r_107.MinY) * 0.5))), (vx_4 = (a_100.X - b_101.X), (vy_4 = (a_100.Y - b_101.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) < 1E-09)("Center should remain the same");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRel with negative factor throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        let r_108;
        const a_101 = Pt_$ctor_7B00E9A0(0, 0);
        const b_102 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_179 = a_101.X;
        let maxX_179;
        if (b_102.X > minX_179) {
            maxX_179 = b_102.X;
        }
        else {
            minX_179 = b_102.X;
            maxX_179 = a_101.X;
        }
        let minY_179 = a_101.Y;
        let maxY_179;
        if (b_102.Y > minY_179) {
            maxY_179 = b_102.Y;
        }
        else {
            minY_179 = b_102.Y;
            maxY_179 = a_101.Y;
        }
        r_108 = BRect_$ctor_77D16AC0(minX_179, minY_179, maxX_179, maxY_179);
        Expect_throws(() => {
            BRect_expandRel(-0.5, r_108);
        }, "Should throw on negative factor");
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRelXY with different factors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        let a_104, r_114, b_105, r_115, vx_5, vy_5;
        let r_110;
        const a_102 = Pt_$ctor_7B00E9A0(0, 0);
        const b_103 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_181 = a_102.X;
        let maxX_181;
        if (b_103.X > minX_181) {
            maxX_181 = b_103.X;
        }
        else {
            minX_181 = b_103.X;
            maxX_181 = a_102.X;
        }
        let minY_181 = a_102.Y;
        let maxY_181;
        if (b_103.Y > minY_181) {
            maxY_181 = b_103.Y;
        }
        else {
            minY_181 = b_103.Y;
            maxY_181 = a_102.Y;
        }
        r_110 = BRect_$ctor_77D16AC0(minX_181, minY_181, maxX_181, maxY_181);
        const expanded_8 = BRect_expandRelXY(2, 0.5, r_110);
        let actual_93;
        const r_112 = expanded_8;
        actual_93 = (r_112.MaxX - r_112.MinX);
        if ((actual_93 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_93, 20, "SizeX should double");
        }
        else {
            let valueType_93;
            let copyOfStruct_99 = actual_93;
            valueType_93 = float64_type;
            const primitiveTypes_93 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_93;
            if (contains(valueType_93, primitiveTypes_93, {
                Equals: equals,
                GetHashCode: (x_100) => (structuralHash(x_100) | 0),
            })) {
                const arg_98 = (20).toString();
                const arg_1_93 = actual_93.toString();
                errorMsg_93 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_98)(arg_1_93)("SizeX should double");
            }
            else {
                errorMsg_93 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_93)("SizeX should double");
            }
            throw new Exception(errorMsg_93);
        }
        let actual_94;
        const r_113 = expanded_8;
        actual_94 = (r_113.MaxY - r_113.MinY);
        if ((actual_94 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_94, 5, "SizeY should halve");
        }
        else {
            let valueType_94;
            let copyOfStruct_100 = actual_94;
            valueType_94 = float64_type;
            const primitiveTypes_94 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_94;
            if (contains(valueType_94, primitiveTypes_94, {
                Equals: equals,
                GetHashCode: (x_101) => (structuralHash(x_101) | 0),
            })) {
                const arg_99 = (5).toString();
                const arg_1_94 = actual_94.toString();
                errorMsg_94 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_99)(arg_1_94)("SizeY should halve");
            }
            else {
                errorMsg_94 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_94)("SizeY should halve");
            }
            throw new Exception(errorMsg_94);
        }
        Expect_isTrue(((a_104 = ((r_114 = expanded_8, Pt_$ctor_7B00E9A0_1((r_114.MaxX + r_114.MinX) * 0.5, (r_114.MaxY + r_114.MinY) * 0.5))), (b_105 = ((r_115 = r_110, Pt_$ctor_7B00E9A0_1((r_115.MaxX + r_115.MinX) * 0.5, (r_115.MaxY + r_115.MinY) * 0.5))), (vx_5 = (a_104.X - b_105.X), (vy_5 = (a_104.Y - b_105.Y), Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5))))))) < 1E-09)("Center should remain the same");
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRelXY with negative factorX throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let r_116;
        const a_105 = Pt_$ctor_7B00E9A0(0, 0);
        const b_106 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_183 = a_105.X;
        let maxX_183;
        if (b_106.X > minX_183) {
            maxX_183 = b_106.X;
        }
        else {
            minX_183 = b_106.X;
            maxX_183 = a_105.X;
        }
        let minY_183 = a_105.Y;
        let maxY_183;
        if (b_106.Y > minY_183) {
            maxY_183 = b_106.Y;
        }
        else {
            minY_183 = b_106.Y;
            maxY_183 = a_105.Y;
        }
        r_116 = BRect_$ctor_77D16AC0(minX_183, minY_183, maxX_183, maxY_183);
        Expect_throws(() => {
            BRect_expandRelXY(-2, 0.5, r_116);
        }, "Should throw on negative factorX");
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandRelXY with negative factorY throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let r_118;
        const a_106 = Pt_$ctor_7B00E9A0(0, 0);
        const b_107 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_185 = a_106.X;
        let maxX_185;
        if (b_107.X > minX_185) {
            maxX_185 = b_107.X;
        }
        else {
            minX_185 = b_107.X;
            maxX_185 = a_106.X;
        }
        let minY_185 = a_106.Y;
        let maxY_185;
        if (b_107.Y > minY_185) {
            maxY_185 = b_107.Y;
        }
        else {
            minY_185 = b_107.Y;
            maxY_185 = a_106.Y;
        }
        r_118 = BRect_$ctor_77D16AC0(minX_185, minY_185, maxX_185, maxY_185);
        Expect_throws(() => {
            BRect_expandRelXY(2, -0.5, r_118);
        }, "Should throw on negative factorY");
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})()])), Test_testList("Utility Methods", ofArray([(() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt corners", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let a_109, b_109, b_111, vx_6, vy_6, a_111, b_112, b_114, vx_7, vy_7, a_113, b_115, b_117, vx_8, vy_8;
        let r_120;
        const a_107 = Pt_$ctor_7B00E9A0(0, 0);
        const b_108 = Pt_$ctor_7B00E9A0(10, 20);
        let minX_187 = a_107.X;
        let maxX_187;
        if (b_108.X > minX_187) {
            maxX_187 = b_108.X;
        }
        else {
            minX_187 = b_108.X;
            maxX_187 = a_107.X;
        }
        let minY_187 = a_107.Y;
        let maxY_187;
        if (b_108.Y > minY_187) {
            maxY_187 = b_108.Y;
        }
        else {
            minY_187 = b_108.Y;
            maxY_187 = a_107.Y;
        }
        r_120 = BRect_$ctor_77D16AC0(minX_187, minY_187, maxX_187, maxY_187);
        Expect_isTrue(((a_109 = ((b_109 = r_120, Pt_$ctor_7B00E9A0_1(b_109.MinX + ((b_109.MaxX - b_109.MinX) * 0), b_109.MinY + ((b_109.MaxY - b_109.MinY) * 0)))), (b_111 = Pt_$ctor_7B00E9A0(0, 0), (vx_6 = (a_109.X - b_111.X), (vy_6 = (a_109.Y - b_111.Y), Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6))))))) < 1E-09)("0,0 should be MinPt");
        Expect_isTrue(((a_111 = ((b_112 = r_120, Pt_$ctor_7B00E9A0_1(b_112.MinX + ((b_112.MaxX - b_112.MinX) * 1), b_112.MinY + ((b_112.MaxY - b_112.MinY) * 1)))), (b_114 = Pt_$ctor_7B00E9A0(10, 20), (vx_7 = (a_111.X - b_114.X), (vy_7 = (a_111.Y - b_114.Y), Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7))))))) < 1E-09)("1,1 should be MaxPt");
        Expect_isTrue(((a_113 = ((b_115 = r_120, Pt_$ctor_7B00E9A0_1(b_115.MinX + ((b_115.MaxX - b_115.MinX) * 0.5), b_115.MinY + ((b_115.MaxY - b_115.MinY) * 0.5)))), (b_117 = Pt_$ctor_7B00E9A0(5, 10), (vx_8 = (a_113.X - b_117.X), (vy_8 = (a_113.Y - b_117.Y), Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8))))))) < 1E-09)("0.5,0.5 should be center");
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LongestEdge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let r_121;
        const a_114 = Pt_$ctor_7B00E9A0(0, 0);
        const b_118 = Pt_$ctor_7B00E9A0(10, 5);
        let minX_189 = a_114.X;
        let maxX_189;
        if (b_118.X > minX_189) {
            maxX_189 = b_118.X;
        }
        else {
            minX_189 = b_118.X;
            maxX_189 = a_114.X;
        }
        let minY_189 = a_114.Y;
        let maxY_189;
        if (b_118.Y > minY_189) {
            maxY_189 = b_118.Y;
        }
        else {
            minY_189 = b_118.Y;
            maxY_189 = a_114.Y;
        }
        r_121 = BRect_$ctor_77D16AC0(minX_189, minY_189, maxX_189, maxY_189);
        let actual_95;
        const b_119 = r_121;
        const x_102 = b_119.MaxX - b_119.MinX;
        const y_96 = b_119.MaxY - b_119.MinY;
        actual_95 = max(x_102, y_96);
        if ((actual_95 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_95, 10, "Longest edge should be 10");
        }
        else {
            let valueType_95;
            let copyOfStruct_101 = actual_95;
            valueType_95 = float64_type;
            const primitiveTypes_95 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_95;
            if (contains(valueType_95, primitiveTypes_95, {
                Equals: equals,
                GetHashCode: (x_103) => (structuralHash(x_103) | 0),
            })) {
                const arg_100 = (10).toString();
                const arg_1_95 = actual_95.toString();
                errorMsg_95 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_100)(arg_1_95)("Longest edge should be 10");
            }
            else {
                errorMsg_95 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_95)("Longest edge should be 10");
            }
            throw new Exception(errorMsg_95);
        }
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ShortestEdge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        let r_122;
        const a_115 = Pt_$ctor_7B00E9A0(0, 0);
        const b_120 = Pt_$ctor_7B00E9A0(10, 5);
        let minX_191 = a_115.X;
        let maxX_191;
        if (b_120.X > minX_191) {
            maxX_191 = b_120.X;
        }
        else {
            minX_191 = b_120.X;
            maxX_191 = a_115.X;
        }
        let minY_191 = a_115.Y;
        let maxY_191;
        if (b_120.Y > minY_191) {
            maxY_191 = b_120.Y;
        }
        else {
            minY_191 = b_120.Y;
            maxY_191 = a_115.Y;
        }
        r_122 = BRect_$ctor_77D16AC0(minX_191, minY_191, maxX_191, maxY_191);
        let actual_96;
        const b_121 = r_122;
        const x_104 = b_121.MaxX - b_121.MinX;
        const y_98 = b_121.MaxY - b_121.MinY;
        actual_96 = min(x_104, y_98);
        if ((actual_96 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_96, 5, "Shortest edge should be 5");
        }
        else {
            let valueType_96;
            let copyOfStruct_102 = actual_96;
            valueType_96 = float64_type;
            const primitiveTypes_96 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_96;
            if (contains(valueType_96, primitiveTypes_96, {
                Equals: equals,
                GetHashCode: (x_105) => (structuralHash(x_105) | 0),
            })) {
                const arg_101 = (5).toString();
                const arg_1_96 = actual_96.toString();
                errorMsg_96 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_101)(arg_1_96)("Shortest edge should be 5");
            }
            else {
                errorMsg_96 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_96)("Shortest edge should be 5");
            }
            throw new Exception(errorMsg_96);
        }
        Test_TestCaseBuilder__Zero(builder$0040_62);
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsZero for tiny rectangle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        let b_123;
        let r_123;
        const a_116 = Pt_$ctor_7B00E9A0(0, 0);
        const b_122 = Pt_$ctor_7B00E9A0(1E-20, 1E-20);
        let minX_193 = a_116.X;
        let maxX_193;
        if (b_122.X > minX_193) {
            maxX_193 = b_122.X;
        }
        else {
            minX_193 = b_122.X;
            maxX_193 = a_116.X;
        }
        let minY_193 = a_116.Y;
        let maxY_193;
        if (b_122.Y > minY_193) {
            maxY_193 = b_122.Y;
        }
        else {
            minY_193 = b_122.Y;
            maxY_193 = a_116.Y;
        }
        r_123 = BRect_$ctor_77D16AC0(minX_193, minY_193, maxX_193, maxY_193);
        Expect_isTrue((b_123 = r_123, !((b_123.MaxX - b_123.MinX) > 1E-12) && !((b_123.MaxY - b_123.MinY) > 1E-12)))("Tiny rectangle should be zero");
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsPoint same as IsZero", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let r_124;
        const a_117 = Pt_$ctor_7B00E9A0(0, 0);
        const b_124 = Pt_$ctor_7B00E9A0(1E-20, 1E-20);
        let minX_195 = a_117.X;
        let maxX_195;
        if (b_124.X > minX_195) {
            maxX_195 = b_124.X;
        }
        else {
            minX_195 = b_124.X;
            maxX_195 = a_117.X;
        }
        let minY_195 = a_117.Y;
        let maxY_195;
        if (b_124.Y > minY_195) {
            maxY_195 = b_124.Y;
        }
        else {
            minY_195 = b_124.Y;
            maxY_195 = a_117.Y;
        }
        r_124 = BRect_$ctor_77D16AC0(minX_195, minY_195, maxX_195, maxY_195);
        let actual_97;
        const b_126 = r_124;
        actual_97 = (!((b_126.MaxX - b_126.MinX) > 1E-12) && !((b_126.MaxY - b_126.MinY) > 1E-12));
        let expected_97;
        const b_127 = r_124;
        expected_97 = (!((b_127.MaxX - b_127.MinX) > 1E-12) && !((b_127.MaxY - b_127.MinY) > 1E-12));
        if ((actual_97 === expected_97) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_97, expected_97, "IsPoint should equal IsZero");
        }
        else {
            let valueType_97;
            let copyOfStruct_103 = actual_97;
            valueType_97 = bool_type;
            const primitiveTypes_97 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_97;
            if (contains(valueType_97, primitiveTypes_97, {
                Equals: equals,
                GetHashCode: (x_112) => (structuralHash(x_112) | 0),
            })) {
                const arg_102 = toString(expected_97);
                const arg_1_97 = toString(actual_97);
                errorMsg_97 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_102)(arg_1_97)("IsPoint should equal IsZero");
            }
            else {
                errorMsg_97 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_97)(actual_97)("IsPoint should equal IsZero");
            }
            throw new Exception(errorMsg_97);
        }
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsLine for line-like rectangle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        let b_130;
        let r_125;
        const a_118 = Pt_$ctor_7B00E9A0(0, 0);
        const b_128 = Pt_$ctor_7B00E9A0(10, 1E-20);
        let minX_197 = a_118.X;
        let maxX_197;
        if (b_128.X > minX_197) {
            maxX_197 = b_128.X;
        }
        else {
            minX_197 = b_128.X;
            maxX_197 = a_118.X;
        }
        let minY_197 = a_118.Y;
        let maxY_197;
        if (b_128.Y > minY_197) {
            maxY_197 = b_128.Y;
        }
        else {
            minY_197 = b_128.Y;
            maxY_197 = a_118.Y;
        }
        r_125 = BRect_$ctor_77D16AC0(minX_197, minY_197, maxX_197, maxY_197);
        Expect_isTrue(((b_130 = r_125, (((b_130.MaxX - b_130.MinX) > 1E-12) ? 0 : 1) + (((b_130.MaxY - b_130.MinY) > 1E-12) ? 0 : 1))) === 1)("Rectangle with one tiny dimension should be a line");
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})(), (() => {
    const builder$0040_66 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("HasArea for valid rectangle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_66, Test_TestCaseBuilder__Delay_1505(builder$0040_66, () => {
        let b_133;
        let r_126;
        const a_119 = Pt_$ctor_7B00E9A0(0, 0);
        const b_131 = Pt_$ctor_7B00E9A0(10, 5);
        let minX_199 = a_119.X;
        let maxX_199;
        if (b_131.X > minX_199) {
            maxX_199 = b_131.X;
        }
        else {
            minX_199 = b_131.X;
            maxX_199 = a_119.X;
        }
        let minY_199 = a_119.Y;
        let maxY_199;
        if (b_131.Y > minY_199) {
            maxY_199 = b_131.Y;
        }
        else {
            minY_199 = b_131.Y;
            maxY_199 = a_119.Y;
        }
        r_126 = BRect_$ctor_77D16AC0(minX_199, minY_199, maxX_199, maxY_199);
        Expect_isTrue(((b_133 = r_126, (((b_133.MaxX - b_133.MinX) > 1E-12) ? 0 : 1) + (((b_133.MaxY - b_133.MinY) > 1E-12) ? 0 : 1))) === 0)("Valid rectangle should have area");
        Test_TestCaseBuilder__Zero(builder$0040_66);
    }));
})(), (() => {
    const builder$0040_67 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsValid same as HasArea", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_67, Test_TestCaseBuilder__Delay_1505(builder$0040_67, () => {
        let b_136, b_138;
        let r_127;
        const a_120 = Pt_$ctor_7B00E9A0(0, 0);
        const b_134 = Pt_$ctor_7B00E9A0(10, 5);
        let minX_201 = a_120.X;
        let maxX_201;
        if (b_134.X > minX_201) {
            maxX_201 = b_134.X;
        }
        else {
            minX_201 = b_134.X;
            maxX_201 = a_120.X;
        }
        let minY_201 = a_120.Y;
        let maxY_201;
        if (b_134.Y > minY_201) {
            maxY_201 = b_134.Y;
        }
        else {
            minY_201 = b_134.Y;
            maxY_201 = a_120.Y;
        }
        r_127 = BRect_$ctor_77D16AC0(minX_201, minY_201, maxX_201, maxY_201);
        const actual_98 = ((b_136 = r_127, (((b_136.MaxX - b_136.MinX) > 1E-12) ? 0 : 1) + (((b_136.MaxY - b_136.MinY) > 1E-12) ? 0 : 1))) === 0;
        const expected_98 = ((b_138 = r_127, (((b_138.MaxX - b_138.MinX) > 1E-12) ? 0 : 1) + (((b_138.MaxY - b_138.MinY) > 1E-12) ? 0 : 1))) === 0;
        if ((actual_98 === expected_98) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_98, expected_98, "IsValid should equal HasArea");
        }
        else {
            let valueType_98;
            let copyOfStruct_104 = actual_98;
            valueType_98 = bool_type;
            const primitiveTypes_98 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_98;
            if (contains(valueType_98, primitiveTypes_98, {
                Equals: equals,
                GetHashCode: (x_121) => (structuralHash(x_121) | 0),
            })) {
                const arg_103 = toString(expected_98);
                const arg_1_98 = toString(actual_98);
                errorMsg_98 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_103)(arg_1_98)("IsValid should equal HasArea");
            }
            else {
                errorMsg_98 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_98)(actual_98)("IsValid should equal HasArea");
            }
            throw new Exception(errorMsg_98);
        }
        Test_TestCaseBuilder__Zero(builder$0040_67);
    }));
})(), (() => {
    const builder$0040_68 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Corner points Pt0-Pt3", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_68, Test_TestCaseBuilder__Delay_1505(builder$0040_68, () => {
        let a_123, b_141, vx_9, vy_9, a_125, b_143, vx_10, vy_10, a_127, b_145, vx_11, vy_11, a_129, b_147, vx_12, vy_12;
        let r_128;
        const a_121 = Pt_$ctor_7B00E9A0(1, 2);
        const b_139 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_203 = a_121.X;
        let maxX_203;
        if (b_139.X > minX_203) {
            maxX_203 = b_139.X;
        }
        else {
            minX_203 = b_139.X;
            maxX_203 = a_121.X;
        }
        let minY_203 = a_121.Y;
        let maxY_203;
        if (b_139.Y > minY_203) {
            maxY_203 = b_139.Y;
        }
        else {
            minY_203 = b_139.Y;
            maxY_203 = a_121.Y;
        }
        r_128 = BRect_$ctor_77D16AC0(minX_203, minY_203, maxX_203, maxY_203);
        Expect_isTrue(((a_123 = BRect__get_Pt0(r_128), (b_141 = Pt_$ctor_7B00E9A0(1, 2), (vx_9 = (a_123.X - b_141.X), (vy_9 = (a_123.Y - b_141.Y), Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9))))))) < 1E-09)("Pt0 should be MinX, MinY");
        Expect_isTrue(((a_125 = BRect__get_Pt1(r_128), (b_143 = Pt_$ctor_7B00E9A0(5, 2), (vx_10 = (a_125.X - b_143.X), (vy_10 = (a_125.Y - b_143.Y), Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10))))))) < 1E-09)("Pt1 should be MaxX, MinY");
        Expect_isTrue(((a_127 = BRect__get_Pt2(r_128), (b_145 = Pt_$ctor_7B00E9A0(5, 7), (vx_11 = (a_127.X - b_145.X), (vy_11 = (a_127.Y - b_145.Y), Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11))))))) < 1E-09)("Pt2 should be MaxX, MaxY");
        Expect_isTrue(((a_129 = BRect__get_Pt3(r_128), (b_147 = Pt_$ctor_7B00E9A0(1, 7), (vx_12 = (a_129.X - b_147.X), (vy_12 = (a_129.Y - b_147.Y), Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12))))))) < 1E-09)("Pt3 should be MinX, MaxY");
        Test_TestCaseBuilder__Zero(builder$0040_68);
    }));
})(), (() => {
    const builder$0040_69 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Points array has 4 corners in CCW order", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_69, Test_TestCaseBuilder__Delay_1505(builder$0040_69, () => {
        let a_132, b_150, vx_13, vy_13, a_134, b_152, vx_14, vy_14, a_136, b_154, vx_15, vy_15, a_138, b_156, vx_16, vy_16;
        let r_129;
        const a_130 = Pt_$ctor_7B00E9A0(1, 2);
        const b_148 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_205 = a_130.X;
        let maxX_205;
        if (b_148.X > minX_205) {
            maxX_205 = b_148.X;
        }
        else {
            minX_205 = b_148.X;
            maxX_205 = a_130.X;
        }
        let minY_205 = a_130.Y;
        let maxY_205;
        if (b_148.Y > minY_205) {
            maxY_205 = b_148.Y;
        }
        else {
            minY_205 = b_148.Y;
            maxY_205 = a_130.Y;
        }
        r_129 = BRect_$ctor_77D16AC0(minX_205, minY_205, maxX_205, maxY_205);
        const pts_3 = BRect__get_Points(r_129);
        const actual_99 = pts_3.length | 0;
        if ((actual_99 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_99, 4, "Should have 4 points");
        }
        else {
            let valueType_99;
            let copyOfStruct_105 = actual_99;
            valueType_99 = int32_type;
            const primitiveTypes_99 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_99;
            if (contains(valueType_99, primitiveTypes_99, {
                Equals: equals,
                GetHashCode: (x_122) => (structuralHash(x_122) | 0),
            })) {
                const arg_104 = int32ToString(4);
                const arg_1_99 = int32ToString(actual_99);
                errorMsg_99 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_104)(arg_1_99)("Should have 4 points");
            }
            else {
                errorMsg_99 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_99)("Should have 4 points");
            }
            throw new Exception(errorMsg_99);
        }
        Expect_isTrue(((a_132 = item(0, pts_3), (b_150 = BRect__get_Pt0(r_129), (vx_13 = (a_132.X - b_150.X), (vy_13 = (a_132.Y - b_150.Y), Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13))))))) < 1E-09)("First point should be Pt0");
        Expect_isTrue(((a_134 = item(1, pts_3), (b_152 = BRect__get_Pt1(r_129), (vx_14 = (a_134.X - b_152.X), (vy_14 = (a_134.Y - b_152.Y), Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14))))))) < 1E-09)("Second point should be Pt1");
        Expect_isTrue(((a_136 = item(2, pts_3), (b_154 = BRect__get_Pt2(r_129), (vx_15 = (a_136.X - b_154.X), (vy_15 = (a_136.Y - b_154.Y), Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15))))))) < 1E-09)("Third point should be Pt2");
        Expect_isTrue(((a_138 = item(3, pts_3), (b_156 = BRect__get_Pt3(r_129), (vx_16 = (a_138.X - b_156.X), (vy_16 = (a_138.Y - b_156.Y), Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16))))))) < 1E-09)("Fourth point should be Pt3");
        Test_TestCaseBuilder__Zero(builder$0040_69);
    }));
})(), (() => {
    const builder$0040_70 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("PointsLooped has 5 points with first and last same", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_70, Test_TestCaseBuilder__Delay_1505(builder$0040_70, () => {
        let a_141, b_159, vx_17, vy_17, a_143, b_161, vx_18, vy_18;
        let r_130;
        const a_139 = Pt_$ctor_7B00E9A0(1, 2);
        const b_157 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_207 = a_139.X;
        let maxX_207;
        if (b_157.X > minX_207) {
            maxX_207 = b_157.X;
        }
        else {
            minX_207 = b_157.X;
            maxX_207 = a_139.X;
        }
        let minY_207 = a_139.Y;
        let maxY_207;
        if (b_157.Y > minY_207) {
            maxY_207 = b_157.Y;
        }
        else {
            minY_207 = b_157.Y;
            maxY_207 = a_139.Y;
        }
        r_130 = BRect_$ctor_77D16AC0(minX_207, minY_207, maxX_207, maxY_207);
        const pts_4 = BRect__get_PointsLooped(r_130);
        const actual_100 = pts_4.length | 0;
        if ((actual_100 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_100, 5, "Should have 5 points");
        }
        else {
            let valueType_100;
            let copyOfStruct_106 = actual_100;
            valueType_100 = int32_type;
            const primitiveTypes_100 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_100;
            if (contains(valueType_100, primitiveTypes_100, {
                Equals: equals,
                GetHashCode: (x_123) => (structuralHash(x_123) | 0),
            })) {
                const arg_105 = int32ToString(5);
                const arg_1_100 = int32ToString(actual_100);
                errorMsg_100 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_105)(arg_1_100)("Should have 5 points");
            }
            else {
                errorMsg_100 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_100)("Should have 5 points");
            }
            throw new Exception(errorMsg_100);
        }
        Expect_isTrue(((a_141 = item(0, pts_4), (b_159 = item(4, pts_4), (vx_17 = (a_141.X - b_159.X), (vy_17 = (a_141.Y - b_159.Y), Math.sqrt((vx_17 * vx_17) + (vy_17 * vy_17))))))) < 1E-09)("First and last should be same");
        Expect_isTrue(((a_143 = item(0, pts_4), (b_161 = BRect__get_Pt0(r_130), (vx_18 = (a_143.X - b_161.X), (vy_18 = (a_143.Y - b_161.Y), Math.sqrt((vx_18 * vx_18) + (vy_18 * vy_18))))))) < 1E-09)("First should be Pt0");
        Test_TestCaseBuilder__Zero(builder$0040_70);
    }));
})(), (() => {
    const builder$0040_71 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Edges are correct", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_71, Test_TestCaseBuilder__Delay_1505(builder$0040_71, () => {
        let a_146, copyOfStruct_107, ln, b_164, vx_19, vy_19, a_148, copyOfStruct_108, ln_1, b_166, vx_20, vy_20, a_150, copyOfStruct_109, ln_2, b_168, vx_21, vy_21, a_152, copyOfStruct_110, ln_3, b_170, vx_22, vy_22, a_154, copyOfStruct_111, ln_4, b_172, vx_23, vy_23, a_156, copyOfStruct_112, ln_5, b_174, vx_24, vy_24, a_158, copyOfStruct_113, ln_6, b_176, vx_25, vy_25, a_160, copyOfStruct_114, ln_7, b_178, vx_26, vy_26;
        let r_131;
        const a_144 = Pt_$ctor_7B00E9A0(1, 2);
        const b_162 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_209 = a_144.X;
        let maxX_209;
        if (b_162.X > minX_209) {
            maxX_209 = b_162.X;
        }
        else {
            minX_209 = b_162.X;
            maxX_209 = a_144.X;
        }
        let minY_209 = a_144.Y;
        let maxY_209;
        if (b_162.Y > minY_209) {
            maxY_209 = b_162.Y;
        }
        else {
            minY_209 = b_162.Y;
            maxY_209 = a_144.Y;
        }
        r_131 = BRect_$ctor_77D16AC0(minX_209, minY_209, maxX_209, maxY_209);
        Expect_isTrue(((a_146 = ((copyOfStruct_107 = BRect__get_Edge01(r_131), (ln = copyOfStruct_107, Pt_$ctor_7B00E9A0_1(ln.FromX, ln.FromY)))), (b_164 = BRect__get_Pt0(r_131), (vx_19 = (a_146.X - b_164.X), (vy_19 = (a_146.Y - b_164.Y), Math.sqrt((vx_19 * vx_19) + (vy_19 * vy_19))))))) < 1E-09)("Edge01 should start at Pt0");
        Expect_isTrue(((a_148 = ((copyOfStruct_108 = BRect__get_Edge01(r_131), (ln_1 = copyOfStruct_108, Pt_$ctor_7B00E9A0_1(ln_1.ToX, ln_1.ToY)))), (b_166 = BRect__get_Pt1(r_131), (vx_20 = (a_148.X - b_166.X), (vy_20 = (a_148.Y - b_166.Y), Math.sqrt((vx_20 * vx_20) + (vy_20 * vy_20))))))) < 1E-09)("Edge01 should end at Pt1");
        Expect_isTrue(((a_150 = ((copyOfStruct_109 = BRect__get_Edge12(r_131), (ln_2 = copyOfStruct_109, Pt_$ctor_7B00E9A0_1(ln_2.FromX, ln_2.FromY)))), (b_168 = BRect__get_Pt1(r_131), (vx_21 = (a_150.X - b_168.X), (vy_21 = (a_150.Y - b_168.Y), Math.sqrt((vx_21 * vx_21) + (vy_21 * vy_21))))))) < 1E-09)("Edge12 should start at Pt1");
        Expect_isTrue(((a_152 = ((copyOfStruct_110 = BRect__get_Edge12(r_131), (ln_3 = copyOfStruct_110, Pt_$ctor_7B00E9A0_1(ln_3.ToX, ln_3.ToY)))), (b_170 = BRect__get_Pt2(r_131), (vx_22 = (a_152.X - b_170.X), (vy_22 = (a_152.Y - b_170.Y), Math.sqrt((vx_22 * vx_22) + (vy_22 * vy_22))))))) < 1E-09)("Edge12 should end at Pt2");
        Expect_isTrue(((a_154 = ((copyOfStruct_111 = BRect__get_Edge23(r_131), (ln_4 = copyOfStruct_111, Pt_$ctor_7B00E9A0_1(ln_4.FromX, ln_4.FromY)))), (b_172 = BRect__get_Pt2(r_131), (vx_23 = (a_154.X - b_172.X), (vy_23 = (a_154.Y - b_172.Y), Math.sqrt((vx_23 * vx_23) + (vy_23 * vy_23))))))) < 1E-09)("Edge23 should start at Pt2");
        Expect_isTrue(((a_156 = ((copyOfStruct_112 = BRect__get_Edge23(r_131), (ln_5 = copyOfStruct_112, Pt_$ctor_7B00E9A0_1(ln_5.ToX, ln_5.ToY)))), (b_174 = BRect__get_Pt3(r_131), (vx_24 = (a_156.X - b_174.X), (vy_24 = (a_156.Y - b_174.Y), Math.sqrt((vx_24 * vx_24) + (vy_24 * vy_24))))))) < 1E-09)("Edge23 should end at Pt3");
        Expect_isTrue(((a_158 = ((copyOfStruct_113 = BRect__get_Edge30(r_131), (ln_6 = copyOfStruct_113, Pt_$ctor_7B00E9A0_1(ln_6.FromX, ln_6.FromY)))), (b_176 = BRect__get_Pt3(r_131), (vx_25 = (a_158.X - b_176.X), (vy_25 = (a_158.Y - b_176.Y), Math.sqrt((vx_25 * vx_25) + (vy_25 * vy_25))))))) < 1E-09)("Edge30 should start at Pt3");
        Expect_isTrue(((a_160 = ((copyOfStruct_114 = BRect__get_Edge30(r_131), (ln_7 = copyOfStruct_114, Pt_$ctor_7B00E9A0_1(ln_7.ToX, ln_7.ToY)))), (b_178 = BRect__get_Pt0(r_131), (vx_26 = (a_160.X - b_178.X), (vy_26 = (a_160.Y - b_178.Y), Math.sqrt((vx_26 * vx_26) + (vy_26 * vy_26))))))) < 1E-09)("Edge30 should end at Pt0");
        Test_TestCaseBuilder__Zero(builder$0040_71);
    }));
})()])), Test_testList("Equality", ofArray([(() => {
    const builder$0040_72 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with exact match", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_72, Test_TestCaseBuilder__Delay_1505(builder$0040_72, () => {
        let a_164, b_182;
        let r1_15;
        const a_161 = Pt_$ctor_7B00E9A0(1, 2);
        const b_179 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_211 = a_161.X;
        let maxX_211;
        if (b_179.X > minX_211) {
            maxX_211 = b_179.X;
        }
        else {
            minX_211 = b_179.X;
            maxX_211 = a_161.X;
        }
        let minY_211 = a_161.Y;
        let maxY_211;
        if (b_179.Y > minY_211) {
            maxY_211 = b_179.Y;
        }
        else {
            minY_211 = b_179.Y;
            maxY_211 = a_161.Y;
        }
        r1_15 = BRect_$ctor_77D16AC0(minX_211, minY_211, maxX_211, maxY_211);
        let r2_15;
        const a_162 = Pt_$ctor_7B00E9A0(1, 2);
        const b_180 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_213 = a_162.X;
        let maxX_213;
        if (b_180.X > minX_213) {
            maxX_213 = b_180.X;
        }
        else {
            minX_213 = b_180.X;
            maxX_213 = a_162.X;
        }
        let minY_213 = a_162.Y;
        let maxY_213;
        if (b_180.Y > minY_213) {
            maxY_213 = b_180.Y;
        }
        else {
            minY_213 = b_180.Y;
            maxY_213 = a_162.Y;
        }
        r2_15 = BRect_$ctor_77D16AC0(minX_213, minY_213, maxX_213, maxY_213);
        Expect_isTrue((a_164 = r1_15, (b_182 = r2_15, (((Math.abs(a_164.MinX - b_182.MinX) <= 0) && (Math.abs(a_164.MinY - b_182.MinY) <= 0)) && (Math.abs(a_164.MaxX - b_182.MaxX) <= 0)) && (Math.abs(a_164.MaxY - b_182.MaxY) <= 0))))("Identical rectangles should be equal");
        Test_TestCaseBuilder__Zero(builder$0040_72);
    }));
})(), (() => {
    const builder$0040_73 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_73, Test_TestCaseBuilder__Delay_1505(builder$0040_73, () => {
        let a_168, b_186;
        let r1_16;
        const a_165 = Pt_$ctor_7B00E9A0(1, 2);
        const b_183 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_215 = a_165.X;
        let maxX_215;
        if (b_183.X > minX_215) {
            maxX_215 = b_183.X;
        }
        else {
            minX_215 = b_183.X;
            maxX_215 = a_165.X;
        }
        let minY_215 = a_165.Y;
        let maxY_215;
        if (b_183.Y > minY_215) {
            maxY_215 = b_183.Y;
        }
        else {
            minY_215 = b_183.Y;
            maxY_215 = a_165.Y;
        }
        r1_16 = BRect_$ctor_77D16AC0(minX_215, minY_215, maxX_215, maxY_215);
        let r2_16;
        const a_166 = Pt_$ctor_7B00E9A0(1.001, 2.001);
        const b_184 = Pt_$ctor_7B00E9A0(5.001, 7.001);
        let minX_217 = a_166.X;
        let maxX_217;
        if (b_184.X > minX_217) {
            maxX_217 = b_184.X;
        }
        else {
            minX_217 = b_184.X;
            maxX_217 = a_166.X;
        }
        let minY_217 = a_166.Y;
        let maxY_217;
        if (b_184.Y > minY_217) {
            maxY_217 = b_184.Y;
        }
        else {
            minY_217 = b_184.Y;
            maxY_217 = a_166.Y;
        }
        r2_16 = BRect_$ctor_77D16AC0(minX_217, minY_217, maxX_217, maxY_217);
        Expect_isTrue((a_168 = r1_16, (b_186 = r2_16, (((Math.abs(a_168.MinX - b_186.MinX) <= 0.01) && (Math.abs(a_168.MinY - b_186.MinY) <= 0.01)) && (Math.abs(a_168.MaxX - b_186.MaxX) <= 0.01)) && (Math.abs(a_168.MaxY - b_186.MaxY) <= 0.01))))("Should be equal within tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_73);
    }));
})(), (() => {
    const builder$0040_74 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("notEquals with different rectangles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_74, Test_TestCaseBuilder__Delay_1505(builder$0040_74, () => {
        let a_172, b_190;
        let r1_17;
        const a_169 = Pt_$ctor_7B00E9A0(1, 2);
        const b_187 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_219 = a_169.X;
        let maxX_219;
        if (b_187.X > minX_219) {
            maxX_219 = b_187.X;
        }
        else {
            minX_219 = b_187.X;
            maxX_219 = a_169.X;
        }
        let minY_219 = a_169.Y;
        let maxY_219;
        if (b_187.Y > minY_219) {
            maxY_219 = b_187.Y;
        }
        else {
            minY_219 = b_187.Y;
            maxY_219 = a_169.Y;
        }
        r1_17 = BRect_$ctor_77D16AC0(minX_219, minY_219, maxX_219, maxY_219);
        let r2_17;
        const a_170 = Pt_$ctor_7B00E9A0(2, 3);
        const b_188 = Pt_$ctor_7B00E9A0(6, 8);
        let minX_221 = a_170.X;
        let maxX_221;
        if (b_188.X > minX_221) {
            maxX_221 = b_188.X;
        }
        else {
            minX_221 = b_188.X;
            maxX_221 = a_170.X;
        }
        let minY_221 = a_170.Y;
        let maxY_221;
        if (b_188.Y > minY_221) {
            maxY_221 = b_188.Y;
        }
        else {
            minY_221 = b_188.Y;
            maxY_221 = a_170.Y;
        }
        r2_17 = BRect_$ctor_77D16AC0(minX_221, minY_221, maxX_221, maxY_221);
        Expect_isTrue((a_172 = r1_17, (b_190 = r2_17, (((Math.abs(a_172.MinX - b_190.MinX) > 0.1) ? true : (Math.abs(a_172.MinY - b_190.MinY) > 0.1)) ? true : (Math.abs(a_172.MaxX - b_190.MaxX) > 0.1)) ? true : (Math.abs(a_172.MaxY - b_190.MaxY) > 0.1))))("Different rectangles should not be equal");
        Test_TestCaseBuilder__Zero(builder$0040_74);
    }));
})(), (() => {
    const builder$0040_75 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals outside tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_75, Test_TestCaseBuilder__Delay_1505(builder$0040_75, () => {
        let a_176, b_194;
        let r1_18;
        const a_173 = Pt_$ctor_7B00E9A0(1, 2);
        const b_191 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_223 = a_173.X;
        let maxX_223;
        if (b_191.X > minX_223) {
            maxX_223 = b_191.X;
        }
        else {
            minX_223 = b_191.X;
            maxX_223 = a_173.X;
        }
        let minY_223 = a_173.Y;
        let maxY_223;
        if (b_191.Y > minY_223) {
            maxY_223 = b_191.Y;
        }
        else {
            minY_223 = b_191.Y;
            maxY_223 = a_173.Y;
        }
        r1_18 = BRect_$ctor_77D16AC0(minX_223, minY_223, maxX_223, maxY_223);
        let r2_18;
        const a_174 = Pt_$ctor_7B00E9A0(1.1, 2);
        const b_192 = Pt_$ctor_7B00E9A0(5, 7);
        let minX_225 = a_174.X;
        let maxX_225;
        if (b_192.X > minX_225) {
            maxX_225 = b_192.X;
        }
        else {
            minX_225 = b_192.X;
            maxX_225 = a_174.X;
        }
        let minY_225 = a_174.Y;
        let maxY_225;
        if (b_192.Y > minY_225) {
            maxY_225 = b_192.Y;
        }
        else {
            minY_225 = b_192.Y;
            maxY_225 = a_174.Y;
        }
        r2_18 = BRect_$ctor_77D16AC0(minX_225, minY_225, maxX_225, maxY_225);
        Expect_isFalse((a_176 = r1_18, (b_194 = r2_18, (((Math.abs(a_176.MinX - b_194.MinX) <= 0.01) && (Math.abs(a_176.MinY - b_194.MinY) <= 0.01)) && (Math.abs(a_176.MaxX - b_194.MaxX) <= 0.01)) && (Math.abs(a_176.MaxY - b_194.MaxY) <= 0.01))))("Should not be equal outside tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_75);
    }));
})()])), Test_testList("Static Methods", ofArray([(() => {
    const builder$0040_76 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doOverlap static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_76, Test_TestCaseBuilder__Delay_1505(builder$0040_76, () => {
        let r_132, a_181;
        let r1_19;
        const a_177 = Pt_$ctor_7B00E9A0(0, 0);
        const b_195 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_227 = a_177.X;
        let maxX_227;
        if (b_195.X > minX_227) {
            maxX_227 = b_195.X;
        }
        else {
            minX_227 = b_195.X;
            maxX_227 = a_177.X;
        }
        let minY_227 = a_177.Y;
        let maxY_227;
        if (b_195.Y > minY_227) {
            maxY_227 = b_195.Y;
        }
        else {
            minY_227 = b_195.Y;
            maxY_227 = a_177.Y;
        }
        r1_19 = BRect_$ctor_77D16AC0(minX_227, minY_227, maxX_227, maxY_227);
        let r2_19;
        const a_178 = Pt_$ctor_7B00E9A0(5, 5);
        const b_196 = Pt_$ctor_7B00E9A0(15, 15);
        let minX_229 = a_178.X;
        let maxX_229;
        if (b_196.X > minX_229) {
            maxX_229 = b_196.X;
        }
        else {
            minX_229 = b_196.X;
            maxX_229 = a_178.X;
        }
        let minY_229 = a_178.Y;
        let maxY_229;
        if (b_196.Y > minY_229) {
            maxY_229 = b_196.Y;
        }
        else {
            minY_229 = b_196.Y;
            maxY_229 = a_178.Y;
        }
        r2_19 = BRect_$ctor_77D16AC0(minX_229, minY_229, maxX_229, maxY_229);
        Expect_isTrue((r_132 = r2_19, (a_181 = r1_19, !((((r_132.MinX > a_181.MaxX) ? true : (a_181.MinX > r_132.MaxX)) ? true : (a_181.MinY > r_132.MaxY)) ? true : (r_132.MinY > a_181.MaxY)))))("Should overlap");
        Test_TestCaseBuilder__Zero(builder$0040_76);
    }));
})(), (() => {
    const builder$0040_77 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doOverlapMoreThan with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_77, Test_TestCaseBuilder__Delay_1505(builder$0040_77, () => {
        let r_133, a_186, r_134, a_189;
        let r1_20;
        const a_182 = Pt_$ctor_7B00E9A0(0, 0);
        const b_199 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_231 = a_182.X;
        let maxX_231;
        if (b_199.X > minX_231) {
            maxX_231 = b_199.X;
        }
        else {
            minX_231 = b_199.X;
            maxX_231 = a_182.X;
        }
        let minY_231 = a_182.Y;
        let maxY_231;
        if (b_199.Y > minY_231) {
            maxY_231 = b_199.Y;
        }
        else {
            minY_231 = b_199.Y;
            maxY_231 = a_182.Y;
        }
        r1_20 = BRect_$ctor_77D16AC0(minX_231, minY_231, maxX_231, maxY_231);
        let r2_20;
        const a_183 = Pt_$ctor_7B00E9A0(9, 0);
        const b_200 = Pt_$ctor_7B00E9A0(15, 10);
        let minX_233 = a_183.X;
        let maxX_233;
        if (b_200.X > minX_233) {
            maxX_233 = b_200.X;
        }
        else {
            minX_233 = b_200.X;
            maxX_233 = a_183.X;
        }
        let minY_233 = a_183.Y;
        let maxY_233;
        if (b_200.Y > minY_233) {
            maxY_233 = b_200.Y;
        }
        else {
            minY_233 = b_200.Y;
            maxY_233 = a_183.Y;
        }
        r2_20 = BRect_$ctor_77D16AC0(minX_233, minY_233, maxX_233, maxY_233);
        Expect_isTrue((r_133 = r2_20, (a_186 = r1_20, !((((r_133.MinX > (a_186.MaxX - 0.5)) ? true : (a_186.MinX > (r_133.MaxX - 0.5))) ? true : (a_186.MinY > (r_133.MaxY - 0.5))) ? true : (r_133.MinY > (a_186.MaxY - 0.5))))))("Should overlap more than 0.5");
        Expect_isFalse((r_134 = r2_20, (a_189 = r1_20, !((((r_134.MinX > (a_189.MaxX - 2)) ? true : (a_189.MinX > (r_134.MaxX - 2))) ? true : (a_189.MinY > (r_134.MaxY - 2))) ? true : (r_134.MinY > (a_189.MaxY - 2))))))("Should not overlap more than 2.0");
        Test_TestCaseBuilder__Zero(builder$0040_77);
    }));
})(), (() => {
    const builder$0040_78 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("contains static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_78, Test_TestCaseBuilder__Delay_1505(builder$0040_78, () => {
        let r_135, o_2, r_137, p_12, r_136, r_139, p_13, r_138, r_140, o_3, r_142, p_14, r_141, r_144, p_15, r_143;
        let outer;
        const a_190 = Pt_$ctor_7B00E9A0(0, 0);
        const b_205 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_235 = a_190.X;
        let maxX_235;
        if (b_205.X > minX_235) {
            maxX_235 = b_205.X;
        }
        else {
            minX_235 = b_205.X;
            maxX_235 = a_190.X;
        }
        let minY_235 = a_190.Y;
        let maxY_235;
        if (b_205.Y > minY_235) {
            maxY_235 = b_205.Y;
        }
        else {
            minY_235 = b_205.Y;
            maxY_235 = a_190.Y;
        }
        outer = BRect_$ctor_77D16AC0(minX_235, minY_235, maxX_235, maxY_235);
        let inner;
        const a_191 = Pt_$ctor_7B00E9A0(2, 2);
        const b_206 = Pt_$ctor_7B00E9A0(8, 8);
        let minX_237 = a_191.X;
        let maxX_237;
        if (b_206.X > minX_237) {
            maxX_237 = b_206.X;
        }
        else {
            minX_237 = b_206.X;
            maxX_237 = a_191.X;
        }
        let minY_237 = a_191.Y;
        let maxY_237;
        if (b_206.Y > minY_237) {
            maxY_237 = b_206.Y;
        }
        else {
            minY_237 = b_206.Y;
            maxY_237 = a_191.Y;
        }
        inner = BRect_$ctor_77D16AC0(minX_237, minY_237, maxX_237, maxY_237);
        Expect_isTrue((r_135 = outer, (o_2 = inner, ((r_137 = r_135, (p_12 = ((r_136 = o_2, Pt_$ctor_7B00E9A0_1(r_136.MinX, r_136.MinY))), (((p_12.X >= r_137.MinX) && (p_12.X <= r_137.MaxX)) && (p_12.Y >= r_137.MinY)) && (p_12.Y <= r_137.MaxY)))) && ((r_139 = r_135, (p_13 = ((r_138 = o_2, Pt_$ctor_7B00E9A0_1(r_138.MaxX, r_138.MaxY))), (((p_13.X >= r_139.MinX) && (p_13.X <= r_139.MaxX)) && (p_13.Y >= r_139.MinY)) && (p_13.Y <= r_139.MaxY)))))))("Inner should be contained in outer");
        Expect_isFalse((r_140 = inner, (o_3 = outer, ((r_142 = r_140, (p_14 = ((r_141 = o_3, Pt_$ctor_7B00E9A0_1(r_141.MinX, r_141.MinY))), (((p_14.X >= r_142.MinX) && (p_14.X <= r_142.MaxX)) && (p_14.Y >= r_142.MinY)) && (p_14.Y <= r_142.MaxY)))) && ((r_144 = r_140, (p_15 = ((r_143 = o_3, Pt_$ctor_7B00E9A0_1(r_143.MaxX, r_143.MaxY))), (((p_15.X >= r_144.MinX) && (p_15.X <= r_144.MaxX)) && (p_15.Y >= r_144.MinY)) && (p_15.Y <= r_144.MaxY)))))))("Outer should not be contained in inner");
        Test_TestCaseBuilder__Zero(builder$0040_78);
    }));
})(), (() => {
    const builder$0040_79 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("containsPt static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_79, Test_TestCaseBuilder__Delay_1505(builder$0040_79, () => {
        let r_146, p_16, r_147, p_17;
        let r_145;
        const a_192 = Pt_$ctor_7B00E9A0(0, 0);
        const b_207 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_239 = a_192.X;
        let maxX_239;
        if (b_207.X > minX_239) {
            maxX_239 = b_207.X;
        }
        else {
            minX_239 = b_207.X;
            maxX_239 = a_192.X;
        }
        let minY_239 = a_192.Y;
        let maxY_239;
        if (b_207.Y > minY_239) {
            maxY_239 = b_207.Y;
        }
        else {
            minY_239 = b_207.Y;
            maxY_239 = a_192.Y;
        }
        r_145 = BRect_$ctor_77D16AC0(minX_239, minY_239, maxX_239, maxY_239);
        Expect_isTrue((r_146 = r_145, (p_16 = Pt_$ctor_7B00E9A0(5, 5), (((p_16.X >= r_146.MinX) && (p_16.X <= r_146.MaxX)) && (p_16.Y >= r_146.MinY)) && (p_16.Y <= r_146.MaxY))))("Point inside");
        Expect_isFalse((r_147 = r_145, (p_17 = Pt_$ctor_7B00E9A0(15, 5), (((p_17.X >= r_147.MinX) && (p_17.X <= r_147.MaxX)) && (p_17.Y >= r_147.MinY)) && (p_17.Y <= r_147.MaxY))))("Point outside");
        Test_TestCaseBuilder__Zero(builder$0040_79);
    }));
})(), (() => {
    const builder$0040_80 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("union static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_80, Test_TestCaseBuilder__Delay_1505(builder$0040_80, () => {
        let r1_21;
        const a_193 = Pt_$ctor_7B00E9A0(0, 0);
        const b_208 = Pt_$ctor_7B00E9A0(5, 5);
        let minX_241 = a_193.X;
        let maxX_241;
        if (b_208.X > minX_241) {
            maxX_241 = b_208.X;
        }
        else {
            minX_241 = b_208.X;
            maxX_241 = a_193.X;
        }
        let minY_241 = a_193.Y;
        let maxY_241;
        if (b_208.Y > minY_241) {
            maxY_241 = b_208.Y;
        }
        else {
            minY_241 = b_208.Y;
            maxY_241 = a_193.Y;
        }
        r1_21 = BRect_$ctor_77D16AC0(minX_241, minY_241, maxX_241, maxY_241);
        let r2_21;
        const a_194 = Pt_$ctor_7B00E9A0(3, 3);
        const b_209 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_243 = a_194.X;
        let maxX_243;
        if (b_209.X > minX_243) {
            maxX_243 = b_209.X;
        }
        else {
            minX_243 = b_209.X;
            maxX_243 = a_194.X;
        }
        let minY_243 = a_194.Y;
        let maxY_243;
        if (b_209.Y > minY_243) {
            maxY_243 = b_209.Y;
        }
        else {
            minY_243 = b_209.Y;
            maxY_243 = a_194.Y;
        }
        r2_21 = BRect_$ctor_77D16AC0(minX_243, minY_243, maxX_243, maxY_243);
        let u;
        const a_196 = r1_21;
        const b_211 = r2_21;
        u = BRect_$ctor_77D16AC0(min(b_211.MinX, a_196.MinX), min(b_211.MinY, a_196.MinY), max(b_211.MaxX, a_196.MaxX), max(b_211.MaxY, a_196.MaxY));
        const actual_101 = u.MinX;
        if ((actual_101 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_101, 0, "MinX");
        }
        else {
            let valueType_101;
            let copyOfStruct_115 = actual_101;
            valueType_101 = float64_type;
            const primitiveTypes_101 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_101;
            if (contains(valueType_101, primitiveTypes_101, {
                Equals: equals,
                GetHashCode: (x_124) => (structuralHash(x_124) | 0),
            })) {
                const arg_106 = (0).toString();
                const arg_1_101 = actual_101.toString();
                errorMsg_101 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_106)(arg_1_101)("MinX");
            }
            else {
                errorMsg_101 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_101)("MinX");
            }
            throw new Exception(errorMsg_101);
        }
        const actual_102 = u.MaxX;
        if ((actual_102 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_102, 10, "MaxX");
        }
        else {
            let valueType_102;
            let copyOfStruct_116 = actual_102;
            valueType_102 = float64_type;
            const primitiveTypes_102 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_102;
            if (contains(valueType_102, primitiveTypes_102, {
                Equals: equals,
                GetHashCode: (x_125) => (structuralHash(x_125) | 0),
            })) {
                const arg_107 = (10).toString();
                const arg_1_102 = actual_102.toString();
                errorMsg_102 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_107)(arg_1_102)("MaxX");
            }
            else {
                errorMsg_102 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_102)("MaxX");
            }
            throw new Exception(errorMsg_102);
        }
        const actual_103 = u.MaxY;
        if ((actual_103 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_103, 10, "MaxY");
        }
        else {
            let valueType_103;
            let copyOfStruct_117 = actual_103;
            valueType_103 = float64_type;
            const primitiveTypes_103 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_103;
            if (contains(valueType_103, primitiveTypes_103, {
                Equals: equals,
                GetHashCode: (x_126) => (structuralHash(x_126) | 0),
            })) {
                const arg_108 = (10).toString();
                const arg_1_103 = actual_103.toString();
                errorMsg_103 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_108)(arg_1_103)("MaxY");
            }
            else {
                errorMsg_103 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_103)("MaxY");
            }
            throw new Exception(errorMsg_103);
        }
        Test_TestCaseBuilder__Zero(builder$0040_80);
    }));
})(), (() => {
    const builder$0040_81 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("unionPt static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_81, Test_TestCaseBuilder__Delay_1505(builder$0040_81, () => {
        let r_148;
        const a_197 = Pt_$ctor_7B00E9A0(0, 0);
        const b_212 = Pt_$ctor_7B00E9A0(5, 5);
        let minX_246 = a_197.X;
        let maxX_246;
        if (b_212.X > minX_246) {
            maxX_246 = b_212.X;
        }
        else {
            minX_246 = b_212.X;
            maxX_246 = a_197.X;
        }
        let minY_246 = a_197.Y;
        let maxY_246;
        if (b_212.Y > minY_246) {
            maxY_246 = b_212.Y;
        }
        else {
            minY_246 = b_212.Y;
            maxY_246 = a_197.Y;
        }
        r_148 = BRect_$ctor_77D16AC0(minX_246, minY_246, maxX_246, maxY_246);
        let u_1;
        const p_19 = Pt_$ctor_7B00E9A0(10, 10);
        const r_150 = r_148;
        u_1 = BRect_$ctor_77D16AC0(min(r_150.MinX, p_19.X), min(r_150.MinY, p_19.Y), max(r_150.MaxX, p_19.X), max(r_150.MaxY, p_19.Y));
        const actual_104 = u_1.MaxX;
        if ((actual_104 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_104, 10, "MaxX");
        }
        else {
            let valueType_104;
            let copyOfStruct_118 = actual_104;
            valueType_104 = float64_type;
            const primitiveTypes_104 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_104;
            if (contains(valueType_104, primitiveTypes_104, {
                Equals: equals,
                GetHashCode: (x_127) => (structuralHash(x_127) | 0),
            })) {
                const arg_109 = (10).toString();
                const arg_1_104 = actual_104.toString();
                errorMsg_104 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_109)(arg_1_104)("MaxX");
            }
            else {
                errorMsg_104 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_104)("MaxX");
            }
            throw new Exception(errorMsg_104);
        }
        const actual_105 = u_1.MaxY;
        if ((actual_105 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_105, 10, "MaxY");
        }
        else {
            let valueType_105;
            let copyOfStruct_119 = actual_105;
            valueType_105 = float64_type;
            const primitiveTypes_105 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_105;
            if (contains(valueType_105, primitiveTypes_105, {
                Equals: equals,
                GetHashCode: (x_128) => (structuralHash(x_128) | 0),
            })) {
                const arg_110 = (10).toString();
                const arg_1_105 = actual_105.toString();
                errorMsg_105 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_110)(arg_1_105)("MaxY");
            }
            else {
                errorMsg_105 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_105)("MaxY");
            }
            throw new Exception(errorMsg_105);
        }
        Test_TestCaseBuilder__Zero(builder$0040_81);
    }));
})(), (() => {
    const builder$0040_82 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("area static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_82, Test_TestCaseBuilder__Delay_1505(builder$0040_82, () => {
        let r_153, r_154;
        let r_151;
        const a_198 = Pt_$ctor_7B00E9A0(0, 0);
        const b_213 = Pt_$ctor_7B00E9A0(4, 5);
        let minX_249 = a_198.X;
        let maxX_249;
        if (b_213.X > minX_249) {
            maxX_249 = b_213.X;
        }
        else {
            minX_249 = b_213.X;
            maxX_249 = a_198.X;
        }
        let minY_249 = a_198.Y;
        let maxY_249;
        if (b_213.Y > minY_249) {
            maxY_249 = b_213.Y;
        }
        else {
            minY_249 = b_213.Y;
            maxY_249 = a_198.Y;
        }
        r_151 = BRect_$ctor_77D16AC0(minX_249, minY_249, maxX_249, maxY_249);
        let actual_106;
        const r_152 = r_151;
        actual_106 = (((r_153 = r_152, r_153.MaxX - r_153.MinX)) * ((r_154 = r_152, r_154.MaxY - r_154.MinY)));
        if ((actual_106 === 20) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_106, 20, "Area should be 20");
        }
        else {
            let valueType_106;
            let copyOfStruct_120 = actual_106;
            valueType_106 = float64_type;
            const primitiveTypes_106 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_106;
            if (contains(valueType_106, primitiveTypes_106, {
                Equals: equals,
                GetHashCode: (x_129) => (structuralHash(x_129) | 0),
            })) {
                const arg_111 = (20).toString();
                const arg_1_106 = actual_106.toString();
                errorMsg_106 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_111)(arg_1_106)("Area should be 20");
            }
            else {
                errorMsg_106 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(20)(actual_106)("Area should be 20");
            }
            throw new Exception(errorMsg_106);
        }
        Test_TestCaseBuilder__Zero(builder$0040_82);
    }));
})(), (() => {
    const builder$0040_83 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expand static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_83, Test_TestCaseBuilder__Delay_1505(builder$0040_83, () => {
        let r_155;
        const a_199 = Pt_$ctor_7B00E9A0(0, 0);
        const b_214 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_251 = a_199.X;
        let maxX_251;
        if (b_214.X > minX_251) {
            maxX_251 = b_214.X;
        }
        else {
            minX_251 = b_214.X;
            maxX_251 = a_199.X;
        }
        let minY_251 = a_199.Y;
        let maxY_251;
        if (b_214.Y > minY_251) {
            maxY_251 = b_214.Y;
        }
        else {
            minY_251 = b_214.Y;
            maxY_251 = a_199.Y;
        }
        r_155 = BRect_$ctor_77D16AC0(minX_251, minY_251, maxX_251, maxY_251);
        let expanded_9;
        const r_158 = r_155;
        const n_9 = BRect_$ctor_77D16AC0(r_158.MinX - 2, r_158.MinY - 2, r_158.MaxX + 2, r_158.MaxY + 2);
        if ((2 < 0) && ((n_9.MinX > n_9.MaxX) ? true : (n_9.MinY > n_9.MaxY))) {
            fail(`BRect.Expand(dist): Negative distance ${2} causes an underflow, on ${BRect__get_AsString(r_158)}`);
        }
        expanded_9 = n_9;
        const actual_107 = expanded_9.MinX;
        if ((actual_107 === -2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_107, -2, "MinX");
        }
        else {
            let valueType_107;
            let copyOfStruct_121 = actual_107;
            valueType_107 = float64_type;
            const primitiveTypes_107 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_107;
            if (contains(valueType_107, primitiveTypes_107, {
                Equals: equals,
                GetHashCode: (x_130) => (structuralHash(x_130) | 0),
            })) {
                const arg_112 = (-2).toString();
                const arg_1_107 = actual_107.toString();
                errorMsg_107 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_112)(arg_1_107)("MinX");
            }
            else {
                errorMsg_107 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(-2)(actual_107)("MinX");
            }
            throw new Exception(errorMsg_107);
        }
        const actual_108 = expanded_9.MaxX;
        if ((actual_108 === 12) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_108, 12, "MaxX");
        }
        else {
            let valueType_108;
            let copyOfStruct_122 = actual_108;
            valueType_108 = float64_type;
            const primitiveTypes_108 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_108;
            if (contains(valueType_108, primitiveTypes_108, {
                Equals: equals,
                GetHashCode: (x_131) => (structuralHash(x_131) | 0),
            })) {
                const arg_113 = (12).toString();
                const arg_1_108 = actual_108.toString();
                errorMsg_108 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_113)(arg_1_108)("MaxX");
            }
            else {
                errorMsg_108 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(12)(actual_108)("MaxX");
            }
            throw new Exception(errorMsg_108);
        }
        Test_TestCaseBuilder__Zero(builder$0040_83);
    }));
})(), (() => {
    const builder$0040_84 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandSafe static method collapses to center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_84, Test_TestCaseBuilder__Delay_1505(builder$0040_84, () => {
        let r_159;
        const a_200 = Pt_$ctor_7B00E9A0(0, 0);
        const b_215 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_254 = a_200.X;
        let maxX_254;
        if (b_215.X > minX_254) {
            maxX_254 = b_215.X;
        }
        else {
            minX_254 = b_215.X;
            maxX_254 = a_200.X;
        }
        let minY_254 = a_200.Y;
        let maxY_254;
        if (b_215.Y > minY_254) {
            maxY_254 = b_215.Y;
        }
        else {
            minY_254 = b_215.Y;
            maxY_254 = a_200.Y;
        }
        r_159 = BRect_$ctor_77D16AC0(minX_254, minY_254, maxX_254, maxY_254);
        let shrunk_2;
        const b_217 = r_159;
        const xDist_6 = -20;
        const yDist_6 = -20;
        let minXCh_3 = b_217.MinX - xDist_6;
        let maxXCh_3 = b_217.MaxX + xDist_6;
        if (minXCh_3 > maxXCh_3) {
            const mid_4 = b_217.MinX + ((b_217.MaxX - b_217.MinX) * 0.5);
            minXCh_3 = mid_4;
            maxXCh_3 = mid_4;
        }
        let minYCh_3 = b_217.MinY - yDist_6;
        let maxYCh_3 = b_217.MaxY + yDist_6;
        if (minYCh_3 > maxYCh_3) {
            const mid_1_3 = b_217.MinY + ((b_217.MaxY - b_217.MinY) * 0.5);
            minYCh_3 = mid_1_3;
            maxYCh_3 = mid_1_3;
        }
        shrunk_2 = BRect_$ctor_77D16AC0(minXCh_3, minYCh_3, maxXCh_3, maxYCh_3);
        const actual_109 = shrunk_2.MinX;
        if ((actual_109 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_109, 5, "MinX at center");
        }
        else {
            let valueType_109;
            let copyOfStruct_123 = actual_109;
            valueType_109 = float64_type;
            const primitiveTypes_109 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_109;
            if (contains(valueType_109, primitiveTypes_109, {
                Equals: equals,
                GetHashCode: (x_132) => (structuralHash(x_132) | 0),
            })) {
                const arg_114 = (5).toString();
                const arg_1_109 = actual_109.toString();
                errorMsg_109 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_114)(arg_1_109)("MinX at center");
            }
            else {
                errorMsg_109 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_109)("MinX at center");
            }
            throw new Exception(errorMsg_109);
        }
        const actual_110 = shrunk_2.MaxX;
        if ((actual_110 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_110, 5, "MaxX at center");
        }
        else {
            let valueType_110;
            let copyOfStruct_124 = actual_110;
            valueType_110 = float64_type;
            const primitiveTypes_110 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_110;
            if (contains(valueType_110, primitiveTypes_110, {
                Equals: equals,
                GetHashCode: (x_133) => (structuralHash(x_133) | 0),
            })) {
                const arg_115 = (5).toString();
                const arg_1_110 = actual_110.toString();
                errorMsg_110 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_115)(arg_1_110)("MaxX at center");
            }
            else {
                errorMsg_110 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_110)("MaxX at center");
            }
            throw new Exception(errorMsg_110);
        }
        Test_TestCaseBuilder__Zero(builder$0040_84);
    }));
})(), (() => {
    const builder$0040_85 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandXaxis static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_85, Test_TestCaseBuilder__Delay_1505(builder$0040_85, () => {
        let r_162;
        const a_201 = Pt_$ctor_7B00E9A0(0, 0);
        const b_218 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_257 = a_201.X;
        let maxX_257;
        if (b_218.X > minX_257) {
            maxX_257 = b_218.X;
        }
        else {
            minX_257 = b_218.X;
            maxX_257 = a_201.X;
        }
        let minY_257 = a_201.Y;
        let maxY_257;
        if (b_218.Y > minY_257) {
            maxY_257 = b_218.Y;
        }
        else {
            minY_257 = b_218.Y;
            maxY_257 = a_201.Y;
        }
        r_162 = BRect_$ctor_77D16AC0(minX_257, minY_257, maxX_257, maxY_257);
        let expanded_10;
        const r_165 = r_162;
        const n_10 = BRect_$ctor_77D16AC0(r_165.MinX - 1, r_165.MinY, r_165.MaxX + 2, r_165.MaxY);
        if (n_10.MinX > n_10.MaxX) {
            fail(`BRect.ExpandXaxis: Negative distances for start(${1}) and end (${2}) cause an underflow, on ${BRect__get_AsString(r_165)}`);
        }
        expanded_10 = n_10;
        const actual_111 = expanded_10.MinX;
        if ((actual_111 === -1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_111, -1, "MinX");
        }
        else {
            let valueType_111;
            let copyOfStruct_125 = actual_111;
            valueType_111 = float64_type;
            const primitiveTypes_111 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_111;
            if (contains(valueType_111, primitiveTypes_111, {
                Equals: equals,
                GetHashCode: (x_134) => (structuralHash(x_134) | 0),
            })) {
                const arg_116 = (-1).toString();
                const arg_1_111 = actual_111.toString();
                errorMsg_111 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_116)(arg_1_111)("MinX");
            }
            else {
                errorMsg_111 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(-1)(actual_111)("MinX");
            }
            throw new Exception(errorMsg_111);
        }
        const actual_112 = expanded_10.MaxX;
        if ((actual_112 === 12) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_112, 12, "MaxX");
        }
        else {
            let valueType_112;
            let copyOfStruct_126 = actual_112;
            valueType_112 = float64_type;
            const primitiveTypes_112 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_112;
            if (contains(valueType_112, primitiveTypes_112, {
                Equals: equals,
                GetHashCode: (x_135) => (structuralHash(x_135) | 0),
            })) {
                const arg_117 = (12).toString();
                const arg_1_112 = actual_112.toString();
                errorMsg_112 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_117)(arg_1_112)("MaxX");
            }
            else {
                errorMsg_112 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(12)(actual_112)("MaxX");
            }
            throw new Exception(errorMsg_112);
        }
        Test_TestCaseBuilder__Zero(builder$0040_85);
    }));
})(), (() => {
    const builder$0040_86 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("expandYaxis static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_86, Test_TestCaseBuilder__Delay_1505(builder$0040_86, () => {
        let r_166;
        const a_202 = Pt_$ctor_7B00E9A0(0, 0);
        const b_219 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_260 = a_202.X;
        let maxX_260;
        if (b_219.X > minX_260) {
            maxX_260 = b_219.X;
        }
        else {
            minX_260 = b_219.X;
            maxX_260 = a_202.X;
        }
        let minY_260 = a_202.Y;
        let maxY_260;
        if (b_219.Y > minY_260) {
            maxY_260 = b_219.Y;
        }
        else {
            minY_260 = b_219.Y;
            maxY_260 = a_202.Y;
        }
        r_166 = BRect_$ctor_77D16AC0(minX_260, minY_260, maxX_260, maxY_260);
        let expanded_11;
        const r_169 = r_166;
        const n_11 = BRect_$ctor_77D16AC0(r_169.MinX, r_169.MinY - 1, r_169.MaxX, r_169.MaxY + 2);
        if (n_11.MinY > n_11.MaxY) {
            fail(`BRect.ExpandYaxis: Negative distances for start(${1}) and end(${2}) cause an underflow, on ${BRect__get_AsString(r_169)}`);
        }
        expanded_11 = n_11;
        const actual_113 = expanded_11.MinY;
        if ((actual_113 === -1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_113, -1, "MinY");
        }
        else {
            let valueType_113;
            let copyOfStruct_127 = actual_113;
            valueType_113 = float64_type;
            const primitiveTypes_113 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_113;
            if (contains(valueType_113, primitiveTypes_113, {
                Equals: equals,
                GetHashCode: (x_136) => (structuralHash(x_136) | 0),
            })) {
                const arg_118 = (-1).toString();
                const arg_1_113 = actual_113.toString();
                errorMsg_113 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_118)(arg_1_113)("MinY");
            }
            else {
                errorMsg_113 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(-1)(actual_113)("MinY");
            }
            throw new Exception(errorMsg_113);
        }
        const actual_114 = expanded_11.MaxY;
        if ((actual_114 === 12) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_114, 12, "MaxY");
        }
        else {
            let valueType_114;
            let copyOfStruct_128 = actual_114;
            valueType_114 = float64_type;
            const primitiveTypes_114 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_114;
            if (contains(valueType_114, primitiveTypes_114, {
                Equals: equals,
                GetHashCode: (x_137) => (structuralHash(x_137) | 0),
            })) {
                const arg_119 = (12).toString();
                const arg_1_114 = actual_114.toString();
                errorMsg_114 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_119)(arg_1_114)("MaxY");
            }
            else {
                errorMsg_114 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(12)(actual_114)("MaxY");
            }
            throw new Exception(errorMsg_114);
        }
        Test_TestCaseBuilder__Zero(builder$0040_86);
    }));
})()])), Test_testList("Edge Cases", ofArray([(() => {
    const builder$0040_87 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromSeq with empty sequence throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_87, Test_TestCaseBuilder__Delay_1505(builder$0040_87, () => {
        Expect_throws(() => {
            let ps_3, minX_263, minY_263, maxX_263, maxY_263, enumerator_2;
            (ps_3 = empty(), (Operators_IsNull(ps_3) ? failNull("BRect.createFromSeq", "seq<Pt>") : undefined, (isEmpty(ps_3) ? failEmptySeq("BRect.createFromSeq", "seq<Pt>") : undefined, (minX_263 = 1.7976931348623157E+308, (minY_263 = 1.7976931348623157E+308, (maxX_263 = -1.7976931348623157E+308, (maxY_263 = -1.7976931348623157E+308, ((enumerator_2 = getEnumerator(ps_3), (() => {
                try {
                    while (enumerator_2["System.Collections.IEnumerator.MoveNext"]()) {
                        const p_20 = enumerator_2["System.Collections.Generic.IEnumerator`1.get_Current"]();
                        minX_263 = min(minX_263, p_20.X);
                        minY_263 = min(minY_263, p_20.Y);
                        maxX_263 = max(maxX_263, p_20.X);
                        maxY_263 = max(maxY_263, p_20.Y);
                    }
                }
                finally {
                    disposeSafe(enumerator_2);
                }
            })()), BRect_$ctor_77D16AC0(minX_263, minY_263, maxX_263, maxY_263)))))))));
        }, "Should throw on empty sequence");
        Test_TestCaseBuilder__Zero(builder$0040_87);
    }));
})(), (() => {
    const builder$0040_88 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromSeq with null throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_88, Test_TestCaseBuilder__Delay_1505(builder$0040_88, () => {
        Expect_throws(() => {
            let ps_4, minX_265, minY_265, maxX_265, maxY_265, enumerator_3;
            (ps_4 = defaultOf(), (Operators_IsNull(ps_4) ? failNull("BRect.createFromSeq", "seq<Pt>") : undefined, (isEmpty(ps_4) ? failEmptySeq("BRect.createFromSeq", "seq<Pt>") : undefined, (minX_265 = 1.7976931348623157E+308, (minY_265 = 1.7976931348623157E+308, (maxX_265 = -1.7976931348623157E+308, (maxY_265 = -1.7976931348623157E+308, ((enumerator_3 = getEnumerator(ps_4), (() => {
                try {
                    while (enumerator_3["System.Collections.IEnumerator.MoveNext"]()) {
                        const p_21 = enumerator_3["System.Collections.Generic.IEnumerator`1.get_Current"]();
                        minX_265 = min(minX_265, p_21.X);
                        minY_265 = min(minY_265, p_21.Y);
                        maxX_265 = max(maxX_265, p_21.X);
                        maxY_265 = max(maxY_265, p_21.Y);
                    }
                }
                finally {
                    disposeSafe(enumerator_3);
                }
            })()), BRect_$ctor_77D16AC0(minX_265, minY_265, maxX_265, maxY_265)))))))));
        }, "Should throw on null");
        Test_TestCaseBuilder__Zero(builder$0040_88);
    }));
})(), (() => {
    const builder$0040_89 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromIList with empty list throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_89, Test_TestCaseBuilder__Delay_1505(builder$0040_89, () => {
        const pts_5 = [];
        Expect_throws(() => {
            let ps_5, minX_267, minY_267, maxX_267, maxY_267;
            (ps_5 = pts_5, (Operators_IsNull(ps_5) ? failNull("BRect.createFromIList", "IList<Pt>") : undefined, ((count(ps_5) === 0) ? failEmptySeq("BRect.createFromIList", "IList<Pt>") : undefined, (minX_267 = 1.7976931348623157E+308, (minY_267 = 1.7976931348623157E+308, (maxX_267 = -1.7976931348623157E+308, (maxY_267 = -1.7976931348623157E+308, ((() => {
                for (let i_1 = 0; i_1 <= (count(ps_5) - 1); i_1++) {
                    const p_22 = item(i_1, ps_5);
                    minX_267 = min(minX_267, p_22.X);
                    minY_267 = min(minY_267, p_22.Y);
                    maxX_267 = max(maxX_267, p_22.X);
                    maxY_267 = max(maxY_267, p_22.Y);
                }
            })(), BRect_$ctor_77D16AC0(minX_267, minY_267, maxX_267, maxY_267)))))))));
        }, "Should throw on empty list");
        Test_TestCaseBuilder__Zero(builder$0040_89);
    }));
})(), (() => {
    const builder$0040_90 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromIList with null throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_90, Test_TestCaseBuilder__Delay_1505(builder$0040_90, () => {
        Expect_throws(() => {
            let ps_6, minX_269, minY_269, maxX_269, maxY_269;
            (ps_6 = defaultOf(), (Operators_IsNull(ps_6) ? failNull("BRect.createFromIList", "IList<Pt>") : undefined, ((count(ps_6) === 0) ? failEmptySeq("BRect.createFromIList", "IList<Pt>") : undefined, (minX_269 = 1.7976931348623157E+308, (minY_269 = 1.7976931348623157E+308, (maxX_269 = -1.7976931348623157E+308, (maxY_269 = -1.7976931348623157E+308, ((() => {
                for (let i_2 = 0; i_2 <= (count(ps_6) - 1); i_2++) {
                    const p_23 = item(i_2, ps_6);
                    minX_269 = min(minX_269, p_23.X);
                    minY_269 = min(minY_269, p_23.Y);
                    maxX_269 = max(maxX_269, p_23.X);
                    maxY_269 = max(maxY_269, p_23.Y);
                }
            })(), BRect_$ctor_77D16AC0(minX_269, minY_269, maxX_269, maxY_269)))))))));
        }, "Should throw on null");
        Test_TestCaseBuilder__Zero(builder$0040_90);
    }));
})(), (() => {
    const builder$0040_91 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for point (2 zero sides)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_91, Test_TestCaseBuilder__Delay_1505(builder$0040_91, () => {
        let r_170;
        const a_203 = Pt_$ctor_7B00E9A0(5, 5);
        const b_220 = Pt_$ctor_7B00E9A0(5, 5);
        let minX_271 = a_203.X;
        let maxX_271;
        if (b_220.X > minX_271) {
            maxX_271 = b_220.X;
        }
        else {
            minX_271 = b_220.X;
            maxX_271 = a_203.X;
        }
        let minY_271 = a_203.Y;
        let maxY_271;
        if (b_220.Y > minY_271) {
            maxY_271 = b_220.Y;
        }
        else {
            minY_271 = b_220.Y;
            maxY_271 = a_203.Y;
        }
        r_170 = BRect_$ctor_77D16AC0(minX_271, minY_271, maxX_271, maxY_271);
        let actual_115;
        const b_221 = r_170;
        actual_115 = ((((b_221.MaxX - b_221.MinX) > 1E-12) ? 0 : 1) + (((b_221.MaxY - b_221.MinY) > 1E-12) ? 0 : 1));
        if ((actual_115 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_115, 2, "Point should have 2 zero sides");
        }
        else {
            let valueType_115;
            let copyOfStruct_129 = actual_115;
            valueType_115 = int32_type;
            const primitiveTypes_115 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_115;
            if (contains(valueType_115, primitiveTypes_115, {
                Equals: equals,
                GetHashCode: (x_140) => (structuralHash(x_140) | 0),
            })) {
                const arg_120 = int32ToString(2);
                const arg_1_115 = int32ToString(actual_115);
                errorMsg_115 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_120)(arg_1_115)("Point should have 2 zero sides");
            }
            else {
                errorMsg_115 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_115)("Point should have 2 zero sides");
            }
            throw new Exception(errorMsg_115);
        }
        Test_TestCaseBuilder__Zero(builder$0040_91);
    }));
})(), (() => {
    const builder$0040_92 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for horizontal line (1 zero side)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_92, Test_TestCaseBuilder__Delay_1505(builder$0040_92, () => {
        const r_171 = BRect_$ctor_77D16AC0(0, 5, 10, 5);
        let actual_116;
        const b_222 = r_171;
        actual_116 = ((((b_222.MaxX - b_222.MinX) > 1E-12) ? 0 : 1) + (((b_222.MaxY - b_222.MinY) > 1E-12) ? 0 : 1));
        if ((actual_116 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_116, 1, "Horizontal line should have 1 zero side");
        }
        else {
            let valueType_116;
            let copyOfStruct_130 = actual_116;
            valueType_116 = int32_type;
            const primitiveTypes_116 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_116;
            if (contains(valueType_116, primitiveTypes_116, {
                Equals: equals,
                GetHashCode: (x_143) => (structuralHash(x_143) | 0),
            })) {
                const arg_121 = int32ToString(1);
                const arg_1_116 = int32ToString(actual_116);
                errorMsg_116 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_121)(arg_1_116)("Horizontal line should have 1 zero side");
            }
            else {
                errorMsg_116 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_116)("Horizontal line should have 1 zero side");
            }
            throw new Exception(errorMsg_116);
        }
        Test_TestCaseBuilder__Zero(builder$0040_92);
    }));
})(), (() => {
    const builder$0040_93 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for vertical line (1 zero side)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_93, Test_TestCaseBuilder__Delay_1505(builder$0040_93, () => {
        const r_172 = BRect_$ctor_77D16AC0(5, 0, 5, 10);
        let actual_117;
        const b_223 = r_172;
        actual_117 = ((((b_223.MaxX - b_223.MinX) > 1E-12) ? 0 : 1) + (((b_223.MaxY - b_223.MinY) > 1E-12) ? 0 : 1));
        if ((actual_117 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_117, 1, "Vertical line should have 1 zero side");
        }
        else {
            let valueType_117;
            let copyOfStruct_131 = actual_117;
            valueType_117 = int32_type;
            const primitiveTypes_117 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_117;
            if (contains(valueType_117, primitiveTypes_117, {
                Equals: equals,
                GetHashCode: (x_146) => (structuralHash(x_146) | 0),
            })) {
                const arg_122 = int32ToString(1);
                const arg_1_117 = int32ToString(actual_117);
                errorMsg_117 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_122)(arg_1_117)("Vertical line should have 1 zero side");
            }
            else {
                errorMsg_117 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_117)("Vertical line should have 1 zero side");
            }
            throw new Exception(errorMsg_117);
        }
        Test_TestCaseBuilder__Zero(builder$0040_93);
    }));
})(), (() => {
    const builder$0040_94 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CountZeroSides for normal rect (0 zero sides)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_94, Test_TestCaseBuilder__Delay_1505(builder$0040_94, () => {
        let r_173;
        const a_204 = Pt_$ctor_7B00E9A0(0, 0);
        const b_224 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_275 = a_204.X;
        let maxX_275;
        if (b_224.X > minX_275) {
            maxX_275 = b_224.X;
        }
        else {
            minX_275 = b_224.X;
            maxX_275 = a_204.X;
        }
        let minY_275 = a_204.Y;
        let maxY_275;
        if (b_224.Y > minY_275) {
            maxY_275 = b_224.Y;
        }
        else {
            minY_275 = b_224.Y;
            maxY_275 = a_204.Y;
        }
        r_173 = BRect_$ctor_77D16AC0(minX_275, minY_275, maxX_275, maxY_275);
        let actual_118;
        const b_225 = r_173;
        actual_118 = ((((b_225.MaxX - b_225.MinX) > 1E-12) ? 0 : 1) + (((b_225.MaxY - b_225.MinY) > 1E-12) ? 0 : 1));
        if ((actual_118 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_118, 0, "Normal rect should have 0 zero sides");
        }
        else {
            let valueType_118;
            let copyOfStruct_132 = actual_118;
            valueType_118 = int32_type;
            const primitiveTypes_118 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_118;
            if (contains(valueType_118, primitiveTypes_118, {
                Equals: equals,
                GetHashCode: (x_149) => (structuralHash(x_149) | 0),
            })) {
                const arg_123 = int32ToString(0);
                const arg_1_118 = int32ToString(actual_118);
                errorMsg_118 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_123)(arg_1_118)("Normal rect should have 0 zero sides");
            }
            else {
                errorMsg_118 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_118)("Normal rect should have 0 zero sides");
            }
            throw new Exception(errorMsg_118);
        }
        Test_TestCaseBuilder__Zero(builder$0040_94);
    }));
})(), (() => {
    const builder$0040_95 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching at corner", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_95, Test_TestCaseBuilder__Delay_1505(builder$0040_95, () => {
        let r1_22;
        const a_205 = Pt_$ctor_7B00E9A0(0, 0);
        const b_226 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_277 = a_205.X;
        let maxX_277;
        if (b_226.X > minX_277) {
            maxX_277 = b_226.X;
        }
        else {
            minX_277 = b_226.X;
            maxX_277 = a_205.X;
        }
        let minY_277 = a_205.Y;
        let maxY_277;
        if (b_226.Y > minY_277) {
            maxY_277 = b_226.Y;
        }
        else {
            minY_277 = b_226.Y;
            maxY_277 = a_205.Y;
        }
        r1_22 = BRect_$ctor_77D16AC0(minX_277, minY_277, maxX_277, maxY_277);
        let r2_22;
        const a_206 = Pt_$ctor_7B00E9A0(10, 10);
        const b_227 = Pt_$ctor_7B00E9A0(20, 20);
        let minX_279 = a_206.X;
        let maxX_279;
        if (b_227.X > minX_279) {
            maxX_279 = b_227.X;
        }
        else {
            minX_279 = b_227.X;
            maxX_279 = a_206.X;
        }
        let minY_279 = a_206.Y;
        let maxY_279;
        if (b_227.Y > minY_279) {
            maxY_279 = b_227.Y;
        }
        else {
            minY_279 = b_227.Y;
            maxY_279 = a_206.Y;
        }
        r2_22 = BRect_$ctor_77D16AC0(minX_279, minY_279, maxX_279, maxY_279);
        Expect_isTrue(BRect__IsTouching_13C371E0(r1_22, r2_22, 0.001))("Rectangles touching at corner");
        Test_TestCaseBuilder__Zero(builder$0040_95);
    }));
})(), (() => {
    const builder$0040_96 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTouching separate rectangles", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_96, Test_TestCaseBuilder__Delay_1505(builder$0040_96, () => {
        let r1_23;
        const a_207 = Pt_$ctor_7B00E9A0(0, 0);
        const b_228 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_281 = a_207.X;
        let maxX_281;
        if (b_228.X > minX_281) {
            maxX_281 = b_228.X;
        }
        else {
            minX_281 = b_228.X;
            maxX_281 = a_207.X;
        }
        let minY_281 = a_207.Y;
        let maxY_281;
        if (b_228.Y > minY_281) {
            maxY_281 = b_228.Y;
        }
        else {
            minY_281 = b_228.Y;
            maxY_281 = a_207.Y;
        }
        r1_23 = BRect_$ctor_77D16AC0(minX_281, minY_281, maxX_281, maxY_281);
        let r2_23;
        const a_208 = Pt_$ctor_7B00E9A0(20, 20);
        const b_229 = Pt_$ctor_7B00E9A0(30, 30);
        let minX_283 = a_208.X;
        let maxX_283;
        if (b_229.X > minX_283) {
            maxX_283 = b_229.X;
        }
        else {
            minX_283 = b_229.X;
            maxX_283 = a_208.X;
        }
        let minY_283 = a_208.Y;
        let maxY_283;
        if (b_229.Y > minY_283) {
            maxY_283 = b_229.Y;
        }
        else {
            minY_283 = b_229.Y;
            maxY_283 = a_208.Y;
        }
        r2_23 = BRect_$ctor_77D16AC0(minX_283, minY_283, maxX_283, maxY_283);
        Expect_isFalse(BRect__IsTouching_13C371E0(r1_23, r2_23, 0.001))("Separate rectangles not touching");
        Test_TestCaseBuilder__Zero(builder$0040_96);
    }));
})(), (() => {
    const builder$0040_97 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("OverlapsWith with negative tolerance counts apart as overlap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_97, Test_TestCaseBuilder__Delay_1505(builder$0040_97, () => {
        let r_174, a_211, r_175, a_212;
        let r1_24;
        const a_209 = Pt_$ctor_7B00E9A0(0, 0);
        const b_230 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_285 = a_209.X;
        let maxX_285;
        if (b_230.X > minX_285) {
            maxX_285 = b_230.X;
        }
        else {
            minX_285 = b_230.X;
            maxX_285 = a_209.X;
        }
        let minY_285 = a_209.Y;
        let maxY_285;
        if (b_230.Y > minY_285) {
            maxY_285 = b_230.Y;
        }
        else {
            minY_285 = b_230.Y;
            maxY_285 = a_209.Y;
        }
        r1_24 = BRect_$ctor_77D16AC0(minX_285, minY_285, maxX_285, maxY_285);
        let r2_24;
        const a_210 = Pt_$ctor_7B00E9A0(11, 0);
        const b_231 = Pt_$ctor_7B00E9A0(20, 10);
        let minX_287 = a_210.X;
        let maxX_287;
        if (b_231.X > minX_287) {
            maxX_287 = b_231.X;
        }
        else {
            minX_287 = b_231.X;
            maxX_287 = a_210.X;
        }
        let minY_287 = a_210.Y;
        let maxY_287;
        if (b_231.Y > minY_287) {
            maxY_287 = b_231.Y;
        }
        else {
            minY_287 = b_231.Y;
            maxY_287 = a_210.Y;
        }
        r2_24 = BRect_$ctor_77D16AC0(minX_287, minY_287, maxX_287, maxY_287);
        Expect_isFalse((r_174 = r1_24, (a_211 = r2_24, !((((r_174.MinX > (a_211.MaxX - 0)) ? true : (a_211.MinX > (r_174.MaxX - 0))) ? true : (a_211.MinY > (r_174.MaxY - 0))) ? true : (r_174.MinY > (a_211.MaxY - 0))))))("No overlap at zero tolerance");
        Expect_isTrue((r_175 = r1_24, (a_212 = r2_24, !((((r_175.MinX > (a_212.MaxX - -2)) ? true : (a_212.MinX > (r_175.MaxX - -2))) ? true : (a_212.MinY > (r_175.MaxY - -2))) ? true : (r_175.MinY > (a_212.MaxY - -2))))))("Should count as overlap with negative tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_97);
    }));
})(), (() => {
    const builder$0040_98 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Expand negative within bounds", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_98, Test_TestCaseBuilder__Delay_1505(builder$0040_98, () => {
        let r_176;
        const a_213 = Pt_$ctor_7B00E9A0(0, 0);
        const b_232 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_289 = a_213.X;
        let maxX_289;
        if (b_232.X > minX_289) {
            maxX_289 = b_232.X;
        }
        else {
            minX_289 = b_232.X;
            maxX_289 = a_213.X;
        }
        let minY_289 = a_213.Y;
        let maxY_289;
        if (b_232.Y > minY_289) {
            maxY_289 = b_232.Y;
        }
        else {
            minY_289 = b_232.Y;
            maxY_289 = a_213.Y;
        }
        r_176 = BRect_$ctor_77D16AC0(minX_289, minY_289, maxX_289, maxY_289);
        let shrunk_3;
        const r_177 = r_176;
        const n_12 = BRect_$ctor_77D16AC0(r_177.MinX - -2, r_177.MinY - -2, r_177.MaxX + -2, r_177.MaxY + -2);
        if ((-2 < 0) && ((n_12.MinX > n_12.MaxX) ? true : (n_12.MinY > n_12.MaxY))) {
            fail(`BRect.Expand(dist): Negative distance ${-2} causes an underflow, on ${BRect__get_AsString(r_177)}`);
        }
        shrunk_3 = n_12;
        const actual_119 = shrunk_3.MinX;
        if ((actual_119 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_119, 2, "MinX");
        }
        else {
            let valueType_119;
            let copyOfStruct_133 = actual_119;
            valueType_119 = float64_type;
            const primitiveTypes_119 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_119;
            if (contains(valueType_119, primitiveTypes_119, {
                Equals: equals,
                GetHashCode: (x_150) => (structuralHash(x_150) | 0),
            })) {
                const arg_124 = (2).toString();
                const arg_1_119 = actual_119.toString();
                errorMsg_119 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_124)(arg_1_119)("MinX");
            }
            else {
                errorMsg_119 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_119)("MinX");
            }
            throw new Exception(errorMsg_119);
        }
        const actual_120 = shrunk_3.MaxX;
        if ((actual_120 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_120, 8, "MaxX");
        }
        else {
            let valueType_120;
            let copyOfStruct_134 = actual_120;
            valueType_120 = float64_type;
            const primitiveTypes_120 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_120;
            if (contains(valueType_120, primitiveTypes_120, {
                Equals: equals,
                GetHashCode: (x_151) => (structuralHash(x_151) | 0),
            })) {
                const arg_125 = (8).toString();
                const arg_1_120 = actual_120.toString();
                errorMsg_120 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_125)(arg_1_120)("MaxX");
            }
            else {
                errorMsg_120 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_120)("MaxX");
            }
            throw new Exception(errorMsg_120);
        }
        const actual_121 = shrunk_3.MinY;
        if ((actual_121 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_121, 2, "MinY");
        }
        else {
            let valueType_121;
            let copyOfStruct_135 = actual_121;
            valueType_121 = float64_type;
            const primitiveTypes_121 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_121;
            if (contains(valueType_121, primitiveTypes_121, {
                Equals: equals,
                GetHashCode: (x_152) => (structuralHash(x_152) | 0),
            })) {
                const arg_126 = (2).toString();
                const arg_1_121 = actual_121.toString();
                errorMsg_121 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_126)(arg_1_121)("MinY");
            }
            else {
                errorMsg_121 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_121)("MinY");
            }
            throw new Exception(errorMsg_121);
        }
        const actual_122 = shrunk_3.MaxY;
        if ((actual_122 === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_122, 8, "MaxY");
        }
        else {
            let valueType_122;
            let copyOfStruct_136 = actual_122;
            valueType_122 = float64_type;
            const primitiveTypes_122 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_122;
            if (contains(valueType_122, primitiveTypes_122, {
                Equals: equals,
                GetHashCode: (x_153) => (structuralHash(x_153) | 0),
            })) {
                const arg_127 = (8).toString();
                const arg_1_122 = actual_122.toString();
                errorMsg_122 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_127)(arg_1_122)("MaxY");
            }
            else {
                errorMsg_122 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual_122)("MaxY");
            }
            throw new Exception(errorMsg_122);
        }
        Test_TestCaseBuilder__Zero(builder$0040_98);
    }));
})(), (() => {
    const builder$0040_99 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFromCenter with zero size creates point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_99, Test_TestCaseBuilder__Delay_1505(builder$0040_99, () => {
        let b_234;
        let r_178;
        const center_3 = Pt_$ctor_7B00E9A0(5, 5);
        if (!(0 >= 0)) {
            fail(`BRect.createFromCenter sizeX is negative: ${0}, sizeY is: ${0}, center: ${Pt__get_AsString(center_3)}`);
        }
        if (!(0 >= 0)) {
            fail(`BRect.createFromCenter sizeY is negative: ${0}, sizeX is: ${0}, center: ${Pt__get_AsString(center_3)}`);
        }
        const minX_292 = center_3.X - (0 * 0.5);
        const minY_292 = center_3.Y - (0 * 0.5);
        const maxX_292 = center_3.X + (0 * 0.5);
        const maxY_292 = center_3.Y + (0 * 0.5);
        r_178 = BRect_$ctor_77D16AC0(minX_292, minY_292, maxX_292, maxY_292);
        Expect_isTrue((b_234 = r_178, !((b_234.MaxX - b_234.MinX) > 1E-12) && !((b_234.MaxY - b_234.MinY) > 1E-12)))("Should be a point");
        const actual_123 = r_178.MinX;
        if ((actual_123 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_123, 5, "MinX at center");
        }
        else {
            let valueType_123;
            let copyOfStruct_137 = actual_123;
            valueType_123 = float64_type;
            const primitiveTypes_123 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_123;
            if (contains(valueType_123, primitiveTypes_123, {
                Equals: equals,
                GetHashCode: (x_158) => (structuralHash(x_158) | 0),
            })) {
                const arg_128 = (5).toString();
                const arg_1_123 = actual_123.toString();
                errorMsg_123 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_128)(arg_1_123)("MinX at center");
            }
            else {
                errorMsg_123 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_123)("MinX at center");
            }
            throw new Exception(errorMsg_123);
        }
        const actual_124 = r_178.MaxX;
        if ((actual_124 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_124, 5, "MaxX at center");
        }
        else {
            let valueType_124;
            let copyOfStruct_138 = actual_124;
            valueType_124 = float64_type;
            const primitiveTypes_124 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_124;
            if (contains(valueType_124, primitiveTypes_124, {
                Equals: equals,
                GetHashCode: (x_159) => (structuralHash(x_159) | 0),
            })) {
                const arg_129 = (5).toString();
                const arg_1_124 = actual_124.toString();
                errorMsg_124 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_129)(arg_1_124)("MaxX at center");
            }
            else {
                errorMsg_124 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_124)("MaxX at center");
            }
            throw new Exception(errorMsg_124);
        }
        Test_TestCaseBuilder__Zero(builder$0040_99);
    }));
})(), (() => {
    const builder$0040_100 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection of one rect inside another", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_100, Test_TestCaseBuilder__Delay_1505(builder$0040_100, () => {
        let a_218, b_239;
        let outer_1;
        const a_214 = Pt_$ctor_7B00E9A0(0, 0);
        const b_235 = Pt_$ctor_7B00E9A0(20, 20);
        let minX_294 = a_214.X;
        let maxX_294;
        if (b_235.X > minX_294) {
            maxX_294 = b_235.X;
        }
        else {
            minX_294 = b_235.X;
            maxX_294 = a_214.X;
        }
        let minY_294 = a_214.Y;
        let maxY_294;
        if (b_235.Y > minY_294) {
            maxY_294 = b_235.Y;
        }
        else {
            minY_294 = b_235.Y;
            maxY_294 = a_214.Y;
        }
        outer_1 = BRect_$ctor_77D16AC0(minX_294, minY_294, maxX_294, maxY_294);
        let inner_1;
        const a_215 = Pt_$ctor_7B00E9A0(5, 5);
        const b_236 = Pt_$ctor_7B00E9A0(15, 15);
        let minX_296 = a_215.X;
        let maxX_296;
        if (b_236.X > minX_296) {
            maxX_296 = b_236.X;
        }
        else {
            minX_296 = b_236.X;
            maxX_296 = a_215.X;
        }
        let minY_296 = a_215.Y;
        let maxY_296;
        if (b_236.Y > minY_296) {
            maxY_296 = b_236.Y;
        }
        else {
            minY_296 = b_236.Y;
            maxY_296 = a_215.Y;
        }
        inner_1 = BRect_$ctor_77D16AC0(minX_296, minY_296, maxX_296, maxY_296);
        let matchValue_5;
        const b_237 = outer_1;
        const a_216 = inner_1;
        const minX_298 = max(a_216.MinX, b_237.MinX);
        const minY_298 = max(a_216.MinY, b_237.MinY);
        const maxX_298 = min(a_216.MaxX, b_237.MaxX);
        const maxY_298 = min(a_216.MaxY, b_237.MaxY);
        matchValue_5 = (((minX_298 <= maxX_298) && (minY_298 <= maxY_298)) ? BRect_$ctor_77D16AC0(minX_298, minY_298, maxX_298, maxY_298) : undefined);
        if (matchValue_5 == null) {
            Test_failtest("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_100);
        }
        else {
            const i_3 = matchValue_5;
            Expect_isTrue((a_218 = i_3, (b_239 = inner_1, (((Math.abs(a_218.MinX - b_239.MinX) <= 0) && (Math.abs(a_218.MinY - b_239.MinY) <= 0)) && (Math.abs(a_218.MaxX - b_239.MaxX) <= 0)) && (Math.abs(a_218.MaxY - b_239.MaxY) <= 0))))("Intersection should equal inner rect");
            Test_TestCaseBuilder__Zero(builder$0040_100);
        }
    }));
})(), (() => {
    const builder$0040_101 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Union of identical rects returns same rect", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_101, Test_TestCaseBuilder__Delay_1505(builder$0040_101, () => {
        let a_221, b_243;
        let r_179;
        const a_219 = Pt_$ctor_7B00E9A0(0, 0);
        const b_240 = Pt_$ctor_7B00E9A0(10, 10);
        let minX_300 = a_219.X;
        let maxX_300;
        if (b_240.X > minX_300) {
            maxX_300 = b_240.X;
        }
        else {
            minX_300 = b_240.X;
            maxX_300 = a_219.X;
        }
        let minY_300 = a_219.Y;
        let maxY_300;
        if (b_240.Y > minY_300) {
            maxY_300 = b_240.Y;
        }
        else {
            minY_300 = b_240.Y;
            maxY_300 = a_219.Y;
        }
        r_179 = BRect_$ctor_77D16AC0(minX_300, minY_300, maxX_300, maxY_300);
        let u_2;
        const r_180 = r_179;
        const b_241 = r_179;
        u_2 = BRect_$ctor_77D16AC0(min(b_241.MinX, r_180.MinX), min(b_241.MinY, r_180.MinY), max(b_241.MaxX, r_180.MaxX), max(b_241.MaxY, r_180.MaxY));
        Expect_isTrue((a_221 = u_2, (b_243 = r_179, (((Math.abs(a_221.MinX - b_243.MinX) <= 0) && (Math.abs(a_221.MinY - b_243.MinY) <= 0)) && (Math.abs(a_221.MaxX - b_243.MaxX) <= 0)) && (Math.abs(a_221.MaxY - b_243.MaxY) <= 0))))("Union of same rect should be identical");
        Test_TestCaseBuilder__Zero(builder$0040_101);
    }));
})()])), Test_testList("String Representations", ofArray([(() => {
    const builder$0040_102 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ToString contains size information", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_102, Test_TestCaseBuilder__Delay_1505(builder$0040_102, () => {
        let r_181;
        const a_222 = Pt_$ctor_7B00E9A0(0, 0);
        const b_244 = Pt_$ctor_7B00E9A0(10, 5);
        let minX_303 = a_222.X;
        let maxX_303;
        if (b_244.X > minX_303) {
            maxX_303 = b_244.X;
        }
        else {
            minX_303 = b_244.X;
            maxX_303 = a_222.X;
        }
        let minY_303 = a_222.Y;
        let maxY_303;
        if (b_244.Y > minY_303) {
            maxY_303 = b_244.Y;
        }
        else {
            minY_303 = b_244.Y;
            maxY_303 = a_222.Y;
        }
        r_181 = BRect_$ctor_77D16AC0(minX_303, minY_303, maxX_303, maxY_303);
        const s_2 = toString(r_181);
        Expect_isTrue(s_2.indexOf("BRect") >= 0)("Should contain type name");
        Expect_isTrue(s_2.indexOf("sizeX") >= 0)("Should contain sizeX");
        Test_TestCaseBuilder__Zero(builder$0040_102);
    }));
})(), (() => {
    const builder$0040_103 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AsString contains size information", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_103, Test_TestCaseBuilder__Delay_1505(builder$0040_103, () => {
        let r_182;
        const a_223 = Pt_$ctor_7B00E9A0(0, 0);
        const b_245 = Pt_$ctor_7B00E9A0(10, 5);
        let minX_305 = a_223.X;
        let maxX_305;
        if (b_245.X > minX_305) {
            maxX_305 = b_245.X;
        }
        else {
            minX_305 = b_245.X;
            maxX_305 = a_223.X;
        }
        let minY_305 = a_223.Y;
        let maxY_305;
        if (b_245.Y > minY_305) {
            maxY_305 = b_245.Y;
        }
        else {
            minY_305 = b_245.Y;
            maxY_305 = a_223.Y;
        }
        r_182 = BRect_$ctor_77D16AC0(minX_305, minY_305, maxX_305, maxY_305);
        const s_3 = BRect__get_AsString_1(r_182);
        Expect_isTrue(s_3.indexOf("sizeX") >= 0)("Should contain sizeX");
        Expect_isFalse(s_3.indexOf("BRect") >= 0)("Should not contain full type name");
        Test_TestCaseBuilder__Zero(builder$0040_103);
    }));
})(), (() => {
    const builder$0040_104 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AsFSharpCode produces valid code string", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_104, Test_TestCaseBuilder__Delay_1505(builder$0040_104, () => {
        const r_183 = BRect_$ctor_77D16AC0(1, 2, 3, 4);
        const code = BRect__get_AsFSharpCode(r_183);
        Expect_isTrue(code.indexOf("BRect.createUnchecked") >= 0)("Should contain constructor");
        Expect_isTrue(code.indexOf("1") >= 0)("Should contain coordinates");
        Test_TestCaseBuilder__Zero(builder$0040_104);
    }));
})()]))]));

