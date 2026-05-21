
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Polyline2D__Contains_6ADE94FD as Polyline2D__Contains_6ADE94FD_1, Polyline2D__WindingNumber_6ADE94FD as Polyline2D__WindingNumber_6ADE94FD_1, Polyline2D__DistanceTo_6ADE94FD as Polyline2D__DistanceTo_6ADE94FD_1, Polyline2D__ClosestVertex_6ADE94FD, Polyline2D__ClosestPoint_6ADE94FD as Polyline2D__ClosestPoint_6ADE94FD_1, Polyline2D__ClosestParameter_6ADE94FD as Polyline2D__ClosestParameter_6ADE94FD_1, Polyline2D__get_SegmentCount as Polyline2D__get_SegmentCount_1, Polyline2D__get_Points as Polyline2D__get_Points_1, Polyline2D__get_Length as Polyline2D__get_Length_1, Polyline2D_$ctor_Z5FD8CF3C } from "./Src/Polyline2D.js";
import { Polyline2D_move, Polyline2D__set_LastPoint_6ADE94FD, Polyline2D__set_FirstPoint_6ADE94FD, Polyline2D__set_End_6ADE94FD, Polyline2D__set_Start_6ADE94FD, Polyline2D_tryFindSelfIntersection_Z5A89AEF5, Polyline2D__ClosestVertex_6ADE94FD as Polyline2D__ClosestVertex_6ADE94FD_1, Polyline2D_pointsUnsafeInternal_Z5A89AEF5, Polyline2D_createDirectlyUnsafe_Z5FD8CF3C, Polyline2D_scale, Polyline2D_evaluateAt, Polyline2D_reverse_Z5A89AEF5, Polyline2D_reverseInPlace_Z5A89AEF5, Polyline2D_ende_Z5A89AEF5, Polyline2D_start_Z5A89AEF5, Polyline2D_rotateWithCenter, Polyline2D_equals, Polyline2D_closeInPlace_Z5A89AEF5, Polyline2D_close_Z5A89AEF5, Polyline2D__get_AsString, Polyline2D__SignedDistanceTo_6ADE94FD, Polyline2D__get_SegmentVectors, Polyline2D__get_Segments, Polyline2D__IsAlmostClosed_5E38073B, Polyline2D__get_LastSegmentIndex, Polyline2D__get_LastPointIndex, Polyline2D__set_SecondLastPoint_6ADE94FD, Polyline2D__get_SecondLastPoint, Polyline2D__set_SecondPoint_6ADE94FD, Polyline2D__get_SecondPoint, Polyline2D__SetVertex, Polyline2D__get_AsFSharpCode, Polyline2D_removeColinearAndDuplicatePoints, Polyline2D_removeDuplicatePoints, Polyline2D_offsetVar_357F0A77, Polyline2D_offset_Z45C468A5, Polyline2D_rotate, Polyline2D_map, Polyline2D_subPolyline, Polyline2D__CloseInPlace_5E38073B, Polyline2D__Duplicate, Polyline2D__Contains_6ADE94FD, Polyline2D__WindingNumber_6ADE94FD, Polyline2D__ReverseInPlace, Polyline2D__Clone, Polyline2D__Reverse, Polyline2D_moveY, Polyline2D_moveX, Polyline2D_translate, Polyline2D__ScaleOn, Polyline2D__get_Points, Polyline2D__Scale_5E38073B, Polyline2D__DistanceTo_6ADE94FD, Polyline2D__ClosestParameter_6ADE94FD, Polyline2D__ClosestPoint_6ADE94FD, Polyline2D__EvaluateAt_5E38073B, Polyline2D__get_FirstSegment, Polyline2D__get_LastSegment, Polyline2D__GetSegment_Z524259A4, Polyline2D__get_Center, Polyline2D__get_IsClockwise, Polyline2D__get_IsCounterClockwise, Polyline2D__get_SignedArea, Polyline2D__get_Area, Polyline2D__get_Length, Polyline2D__get_BoundingRectangle, Polyline2D__get_IsClosed, Polyline2D__get_LastPoint, Polyline2D__get_FirstPoint, Polyline2D__get_End, Polyline2D__get_Start, Polyline2D__get_SegmentCount, Polyline2D__get_PointCount, Polyline2D_$ctor } from "./Src/Polyline2D.js";
import { Test_TestCaseBuilder__For_Z371464DD, Expect_isSome, Expect_isNone, Expect_stringContains, Expect_throws, Expect_isFalse, Expect_isTrue, Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Exception, int32ToString, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { singleton, replicate, contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { concat, printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { toString } from "./fable_modules/fable-library-js.5.0.0/Types.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Src/Pt.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { Rotation2D_$ctor_7B00E9A0 } from "./Src/Rotation2D.js";
import { rangeDouble } from "./fable_modules/fable-library-js.5.0.0/Range.js";

export const plClosed = (() => {
    const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const plOpen = (() => {
    const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const plTriangle = (() => {
    const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(2.5, 4.33), Pt_$ctor_7B00E9A0(0, 0)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const plLine = (() => {
    const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const plLShape = (() => {
    const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(5, 3), Pt_$ctor_7B00E9A0(2, 3), Pt_$ctor_7B00E9A0(2, 5), Pt_$ctor_7B00E9A0(0, 5)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const plSinglePoint = (() => {
    const points = [Pt_$ctor_7B00E9A0(1, 1)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const plEmpty = Polyline2D_$ctor();

export const tests = Test_testList("Polyline2D", ofArray([Test_testList("Basic Properties", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("point count", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const actual = Polyline2D__get_PointCount(plClosed) | 0;
        if ((actual === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 5, "closed polyline has 5 points");
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
                const arg = int32ToString(5);
                const arg_1 = int32ToString(actual);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("closed polyline has 5 points");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual)("closed polyline has 5 points");
            }
            throw new Exception(errorMsg);
        }
        const actual_1 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_1 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 4, "open polyline has 4 points");
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
                const arg_6 = int32ToString(4);
                const arg_1_1 = int32ToString(actual_1);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("open polyline has 4 points");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_1)("open polyline has 4 points");
            }
            throw new Exception(errorMsg_1);
        }
        const actual_2 = Polyline2D__get_PointCount(plSinglePoint) | 0;
        if ((actual_2 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 1, "single point polyline has 1 point");
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
                const arg_7 = int32ToString(1);
                const arg_1_2 = int32ToString(actual_2);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("single point polyline has 1 point");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_2)("single point polyline has 1 point");
            }
            throw new Exception(errorMsg_2);
        }
        const actual_3 = Polyline2D__get_PointCount(plEmpty) | 0;
        if ((actual_3 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 0, "empty polyline has 0 points");
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
                const arg_8 = int32ToString(0);
                const arg_1_3 = int32ToString(actual_3);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("empty polyline has 0 points");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_3)("empty polyline has 0 points");
            }
            throw new Exception(errorMsg_3);
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segment count", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const actual_4 = Polyline2D__get_SegmentCount(plClosed) | 0;
        if ((actual_4 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, 4, "closed polyline has 4 segments");
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
                const arg_9 = int32ToString(4);
                const arg_1_4 = int32ToString(actual_4);
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("closed polyline has 4 segments");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_4)("closed polyline has 4 segments");
            }
            throw new Exception(errorMsg_4);
        }
        const actual_5 = Polyline2D__get_SegmentCount(plOpen) | 0;
        if ((actual_5 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 3, "open polyline has 3 segments");
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
                const arg_10 = int32ToString(3);
                const arg_1_5 = int32ToString(actual_5);
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("open polyline has 3 segments");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_5)("open polyline has 3 segments");
            }
            throw new Exception(errorMsg_5);
        }
        const actual_6 = Polyline2D__get_SegmentCount(plLine) | 0;
        if ((actual_6 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, 1, "line has 1 segment");
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
                const arg_11 = int32ToString(1);
                const arg_1_6 = int32ToString(actual_6);
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("line has 1 segment");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_6)("line has 1 segment");
            }
            throw new Exception(errorMsg_6);
        }
        const actual_7 = Polyline2D__get_SegmentCount(plEmpty) | 0;
        if ((actual_7 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 0, "empty polyline has 0 segments");
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
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("empty polyline has 0 segments");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_7)("empty polyline has 0 segments");
            }
            throw new Exception(errorMsg_7);
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("start and end points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const p = plOpen;
        let d;
        const a_2 = Polyline2D__get_Start(p);
        const b_2 = Pt_$ctor_7B00E9A0(0, 0);
        const vx = a_2.X - b_2.X;
        const vy = a_2.Y - b_2.Y;
        d = Math.sqrt((vx * vx) + (vy * vy));
        const r = d < 1E-09;
        const actual_8 = r;
        if ((actual_8 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, true, "start point");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_8;
            valueType_8 = bool_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            })) {
                const arg_13 = toString(true);
                const arg_1_8 = toString(actual_8);
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("start point");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_8)("start point");
            }
            throw new Exception(errorMsg_8);
        }
        let d_1;
        const a_4 = Polyline2D__get_End(p);
        const b_4 = Pt_$ctor_7B00E9A0(0, 10);
        const vx_1 = a_4.X - b_4.X;
        const vy_1 = a_4.Y - b_4.Y;
        d_1 = Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1));
        const r_1 = d_1 < 1E-09;
        const actual_9 = r_1;
        if ((actual_9 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, true, "end point");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_9;
            valueType_9 = bool_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            })) {
                const arg_14 = toString(true);
                const arg_1_9 = toString(actual_9);
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("end point");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_9)("end point");
            }
            throw new Exception(errorMsg_9);
        }
        let d_2;
        const a_6 = Polyline2D__get_FirstPoint(p);
        const b_6 = Polyline2D__get_Start(p);
        const vx_2 = a_6.X - b_6.X;
        const vy_2 = a_6.Y - b_6.Y;
        d_2 = Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2));
        const r_2 = d_2 < 1E-09;
        const actual_10 = r_2;
        if ((actual_10 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, true, "first point same as start");
        }
        else {
            let valueType_10;
            let copyOfStruct_10 = actual_10;
            valueType_10 = bool_type;
            const primitiveTypes_10 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_10;
            if (contains(valueType_10, primitiveTypes_10, {
                Equals: equals,
                GetHashCode: (x_10) => (structuralHash(x_10) | 0),
            })) {
                const arg_15 = toString(true);
                const arg_1_10 = toString(actual_10);
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("first point same as start");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_10)("first point same as start");
            }
            throw new Exception(errorMsg_10);
        }
        let d_3;
        const a_8 = Polyline2D__get_LastPoint(p);
        const b_8 = Polyline2D__get_End(p);
        const vx_3 = a_8.X - b_8.X;
        const vy_3 = a_8.Y - b_8.Y;
        d_3 = Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3));
        const r_3 = d_3 < 1E-09;
        const actual_11 = r_3;
        if ((actual_11 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, true, "last point same as end");
        }
        else {
            let valueType_11;
            let copyOfStruct_11 = actual_11;
            valueType_11 = bool_type;
            const primitiveTypes_11 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_11;
            if (contains(valueType_11, primitiveTypes_11, {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            })) {
                const arg_16 = toString(true);
                const arg_1_11 = toString(actual_11);
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("last point same as end");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_11)("last point same as end");
            }
            throw new Exception(errorMsg_11);
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("is closed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        Expect_isTrue(Polyline2D__get_IsClosed(plClosed))("closed polyline should be closed");
        Expect_isFalse(Polyline2D__get_IsClosed(plOpen))("open polyline should not be closed");
        Expect_isTrue(Polyline2D__get_IsClosed(plTriangle))("triangle should be closed");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("bounding rectangle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const bbox = Polyline2D__get_BoundingRectangle(plOpen);
        Expect_isTrue(Math.abs(bbox.MinX - 0) < 1E-09)("min x");
        Expect_isTrue(Math.abs(bbox.MaxX - 10) < 1E-09)("max x");
        Expect_isTrue(Math.abs(bbox.MinY - 0) < 1E-09)("min y");
        Expect_isTrue(Math.abs(bbox.MaxY - 10) < 1E-09)("max y");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})()])), Test_testList("Geometric Operations", ofArray([(() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("length calculation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const length = Polyline2D__get_Length(plOpen);
        const expected_16 = 30;
        Expect_isTrue(Math.abs(length - expected_16) < 1E-09)("length");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("area calculation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const area = Polyline2D__get_Area(plClosed);
        const expected_17 = 100;
        Expect_isTrue(Math.abs(area - expected_17) < 1E-09)("area");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("signed area", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const signedArea = Polyline2D__get_SignedArea(plClosed);
        const expected_18 = 100;
        Expect_isTrue(Math.abs(signedArea - expected_18) < 1E-09)("signed area");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("is counter clockwise", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        Expect_isTrue(Polyline2D__get_IsCounterClockwise(plClosed))("square should be counter-clockwise");
        Expect_isFalse(Polyline2D__get_IsClockwise(plClosed))("square should not be clockwise");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("center point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const center = Polyline2D__get_Center(plOpen);
        const expected_19 = Pt_$ctor_7B00E9A0(5, 5);
        let d_4;
        const a_10 = center;
        const b_10 = expected_19;
        const vx_4 = a_10.X - b_10.X;
        const vy_4 = a_10.Y - b_10.Y;
        d_4 = Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4));
        const r_4 = d_4 < 1E-09;
        const actual_12 = r_4;
        if ((actual_12 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, true, "center");
        }
        else {
            let valueType_12;
            let copyOfStruct_12 = actual_12;
            valueType_12 = bool_type;
            const primitiveTypes_12 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_12;
            if (contains(valueType_12, primitiveTypes_12, {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            })) {
                const arg_17 = toString(true);
                const arg_1_12 = toString(actual_12);
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("center");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_12)("center");
            }
            throw new Exception(errorMsg_12);
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})()])), Test_testList("Point Access and Modification", ofArray([(() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("get segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const seg0 = Polyline2D__GetSegment_Z524259A4(plOpen, 0);
        let d_5;
        let a_12;
        const ln = seg0;
        a_12 = Pt_$ctor_7B00E9A0_1(ln.FromX, ln.FromY);
        const b_12 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_5 = a_12.X - b_12.X;
        const vy_5 = a_12.Y - b_12.Y;
        d_5 = Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5));
        const r_5 = d_5 < 1E-09;
        const actual_13 = r_5;
        if ((actual_13 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, true, "first segment start");
        }
        else {
            let valueType_13;
            let copyOfStruct_13 = actual_13;
            valueType_13 = bool_type;
            const primitiveTypes_13 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_13;
            if (contains(valueType_13, primitiveTypes_13, {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            })) {
                const arg_18 = toString(true);
                const arg_1_13 = toString(actual_13);
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("first segment start");
            }
            else {
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_13)("first segment start");
            }
            throw new Exception(errorMsg_13);
        }
        let d_6;
        let a_14;
        const ln_1 = seg0;
        a_14 = Pt_$ctor_7B00E9A0_1(ln_1.ToX, ln_1.ToY);
        const b_14 = Pt_$ctor_7B00E9A0(10, 0);
        const vx_6 = a_14.X - b_14.X;
        const vy_6 = a_14.Y - b_14.Y;
        d_6 = Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6));
        const r_6 = d_6 < 1E-09;
        const actual_14 = r_6;
        if ((actual_14 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, true, "first segment end");
        }
        else {
            let valueType_14;
            let copyOfStruct_14 = actual_14;
            valueType_14 = bool_type;
            const primitiveTypes_14 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_14;
            if (contains(valueType_14, primitiveTypes_14, {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            })) {
                const arg_19 = toString(true);
                const arg_1_14 = toString(actual_14);
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("first segment end");
            }
            else {
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_14)("first segment end");
            }
            throw new Exception(errorMsg_14);
        }
        const lastSeg = Polyline2D__get_LastSegment(plOpen);
        let d_7;
        let a_16;
        const ln_2 = lastSeg;
        a_16 = Pt_$ctor_7B00E9A0_1(ln_2.FromX, ln_2.FromY);
        const b_16 = Pt_$ctor_7B00E9A0(10, 10);
        const vx_7 = a_16.X - b_16.X;
        const vy_7 = a_16.Y - b_16.Y;
        d_7 = Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7));
        const r_7 = d_7 < 1E-09;
        const actual_15 = r_7;
        if ((actual_15 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, true, "last segment start");
        }
        else {
            let valueType_15;
            let copyOfStruct_15 = actual_15;
            valueType_15 = bool_type;
            const primitiveTypes_15 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_15;
            if (contains(valueType_15, primitiveTypes_15, {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            })) {
                const arg_20 = toString(true);
                const arg_1_15 = toString(actual_15);
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("last segment start");
            }
            else {
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_15)("last segment start");
            }
            throw new Exception(errorMsg_15);
        }
        let d_8;
        let a_18;
        const ln_3 = lastSeg;
        a_18 = Pt_$ctor_7B00E9A0_1(ln_3.ToX, ln_3.ToY);
        const b_18 = Pt_$ctor_7B00E9A0(0, 10);
        const vx_8 = a_18.X - b_18.X;
        const vy_8 = a_18.Y - b_18.Y;
        d_8 = Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8));
        const r_8 = d_8 < 1E-09;
        const actual_16 = r_8;
        if ((actual_16 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, true, "last segment end");
        }
        else {
            let valueType_16;
            let copyOfStruct_16 = actual_16;
            valueType_16 = bool_type;
            const primitiveTypes_16 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_16;
            if (contains(valueType_16, primitiveTypes_16, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_21 = toString(true);
                const arg_1_16 = toString(actual_16);
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("last segment end");
            }
            else {
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_16)("last segment end");
            }
            throw new Exception(errorMsg_16);
        }
        const firstSeg = Polyline2D__get_FirstSegment(plOpen);
        let d_9;
        let a_20;
        const ln_4 = firstSeg;
        a_20 = Pt_$ctor_7B00E9A0_1(ln_4.FromX, ln_4.FromY);
        let b_20;
        const ln_5 = seg0;
        b_20 = Pt_$ctor_7B00E9A0_1(ln_5.FromX, ln_5.FromY);
        const vx_9 = a_20.X - b_20.X;
        const vy_9 = a_20.Y - b_20.Y;
        d_9 = Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9));
        const r_9 = d_9 < 1E-09;
        const actual_17 = r_9;
        if ((actual_17 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, true, "first segment matches GetSegment(0).From");
        }
        else {
            let valueType_17;
            let copyOfStruct_17 = actual_17;
            valueType_17 = bool_type;
            const primitiveTypes_17 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_17;
            if (contains(valueType_17, primitiveTypes_17, {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            })) {
                const arg_22 = toString(true);
                const arg_1_17 = toString(actual_17);
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("first segment matches GetSegment(0).From");
            }
            else {
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_17)("first segment matches GetSegment(0).From");
            }
            throw new Exception(errorMsg_17);
        }
        let d_10;
        let a_22;
        const ln_6 = firstSeg;
        a_22 = Pt_$ctor_7B00E9A0_1(ln_6.ToX, ln_6.ToY);
        let b_22;
        const ln_7 = seg0;
        b_22 = Pt_$ctor_7B00E9A0_1(ln_7.ToX, ln_7.ToY);
        const vx_10 = a_22.X - b_22.X;
        const vy_10 = a_22.Y - b_22.Y;
        d_10 = Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10));
        const r_10 = d_10 < 1E-09;
        const actual_18 = r_10;
        if ((actual_18 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, true, "first segment matches GetSegment(0).To");
        }
        else {
            let valueType_18;
            let copyOfStruct_18 = actual_18;
            valueType_18 = bool_type;
            const primitiveTypes_18 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_18;
            if (contains(valueType_18, primitiveTypes_18, {
                Equals: equals,
                GetHashCode: (x_18) => (structuralHash(x_18) | 0),
            })) {
                const arg_23 = toString(true);
                const arg_1_18 = toString(actual_18);
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("first segment matches GetSegment(0).To");
            }
            else {
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_18)("first segment matches GetSegment(0).To");
            }
            throw new Exception(errorMsg_18);
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("evaluate at parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const pt0 = Polyline2D__EvaluateAt_5E38073B(plOpen, 0);
        let d_11;
        const a_24 = pt0;
        const b_24 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_11 = a_24.X - b_24.X;
        const vy_11 = a_24.Y - b_24.Y;
        d_11 = Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11));
        const r_11 = d_11 < 1E-09;
        const actual_19 = r_11;
        if ((actual_19 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, true, "at parameter 0");
        }
        else {
            let valueType_19;
            let copyOfStruct_19 = actual_19;
            valueType_19 = bool_type;
            const primitiveTypes_19 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_19;
            if (contains(valueType_19, primitiveTypes_19, {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            })) {
                const arg_24 = toString(true);
                const arg_1_19 = toString(actual_19);
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("at parameter 0");
            }
            else {
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_19)("at parameter 0");
            }
            throw new Exception(errorMsg_19);
        }
        const pt1 = Polyline2D__EvaluateAt_5E38073B(plOpen, 1);
        let d_12;
        const a_26 = pt1;
        const b_26 = Pt_$ctor_7B00E9A0(10, 0);
        const vx_12 = a_26.X - b_26.X;
        const vy_12 = a_26.Y - b_26.Y;
        d_12 = Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12));
        const r_12 = d_12 < 1E-09;
        const actual_20 = r_12;
        if ((actual_20 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, true, "at parameter 1");
        }
        else {
            let valueType_20;
            let copyOfStruct_20 = actual_20;
            valueType_20 = bool_type;
            const primitiveTypes_20 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_20;
            if (contains(valueType_20, primitiveTypes_20, {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            })) {
                const arg_25 = toString(true);
                const arg_1_20 = toString(actual_20);
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("at parameter 1");
            }
            else {
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_20)("at parameter 1");
            }
            throw new Exception(errorMsg_20);
        }
        const ptMid = Polyline2D__EvaluateAt_5E38073B(plOpen, 0.5);
        let d_13;
        const a_28 = ptMid;
        const b_28 = Pt_$ctor_7B00E9A0(5, 0);
        const vx_13 = a_28.X - b_28.X;
        const vy_13 = a_28.Y - b_28.Y;
        d_13 = Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13));
        const r_13 = d_13 < 1E-09;
        const actual_21 = r_13;
        if ((actual_21 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, true, "at parameter 0.5");
        }
        else {
            let valueType_21;
            let copyOfStruct_21 = actual_21;
            valueType_21 = bool_type;
            const primitiveTypes_21 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_21;
            if (contains(valueType_21, primitiveTypes_21, {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            })) {
                const arg_26 = toString(true);
                const arg_1_21 = toString(actual_21);
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("at parameter 0.5");
            }
            else {
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_21)("at parameter 0.5");
            }
            throw new Exception(errorMsg_21);
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closest point and parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const testPt = Pt_$ctor_7B00E9A0(5, -1);
        const closestPt = Polyline2D__ClosestPoint_6ADE94FD(plOpen, testPt);
        const expectedClosest = Pt_$ctor_7B00E9A0(5, 0);
        let d_14;
        const a_30 = closestPt;
        const b_30 = expectedClosest;
        const vx_14 = a_30.X - b_30.X;
        const vy_14 = a_30.Y - b_30.Y;
        d_14 = Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14));
        const r_14 = d_14 < 1E-09;
        const actual_22 = r_14;
        if ((actual_22 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_22, true, "closest point");
        }
        else {
            let valueType_22;
            let copyOfStruct_22 = actual_22;
            valueType_22 = bool_type;
            const primitiveTypes_22 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_22;
            if (contains(valueType_22, primitiveTypes_22, {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            })) {
                const arg_27 = toString(true);
                const arg_1_22 = toString(actual_22);
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("closest point");
            }
            else {
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_22)("closest point");
            }
            throw new Exception(errorMsg_22);
        }
        const param = Polyline2D__ClosestParameter_6ADE94FD(plOpen, testPt);
        Expect_isTrue(Math.abs(param - 0.5) < 1E-09)("closest parameter");
        const distance = Polyline2D__DistanceTo_6ADE94FD(plOpen, testPt);
        Expect_isTrue(Math.abs(distance - 1) < 1E-09)("distance");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})()])), Test_testList("Transformations", ofArray([(() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scaling", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const scaled = Polyline2D__Scale_5E38073B(plOpen, 2);
        let d_15;
        const a_32 = item(0, Polyline2D__get_Points(scaled));
        const b_32 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_15 = a_32.X - b_32.X;
        const vy_15 = a_32.Y - b_32.Y;
        d_15 = Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15));
        const r_15 = d_15 < 1E-09;
        const actual_23 = r_15;
        if ((actual_23 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, true, "first point scaled");
        }
        else {
            let valueType_23;
            let copyOfStruct_23 = actual_23;
            valueType_23 = bool_type;
            const primitiveTypes_23 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_23;
            if (contains(valueType_23, primitiveTypes_23, {
                Equals: equals,
                GetHashCode: (x_23) => (structuralHash(x_23) | 0),
            })) {
                const arg_28 = toString(true);
                const arg_1_23 = toString(actual_23);
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("first point scaled");
            }
            else {
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_23)("first point scaled");
            }
            throw new Exception(errorMsg_23);
        }
        let d_16;
        const a_34 = item(1, Polyline2D__get_Points(scaled));
        const b_34 = Pt_$ctor_7B00E9A0(20, 0);
        const vx_16 = a_34.X - b_34.X;
        const vy_16 = a_34.Y - b_34.Y;
        d_16 = Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16));
        const r_16 = d_16 < 1E-09;
        const actual_24 = r_16;
        if ((actual_24 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, true, "second point scaled");
        }
        else {
            let valueType_24;
            let copyOfStruct_24 = actual_24;
            valueType_24 = bool_type;
            const primitiveTypes_24 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_24;
            if (contains(valueType_24, primitiveTypes_24, {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            })) {
                const arg_29 = toString(true);
                const arg_1_24 = toString(actual_24);
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("second point scaled");
            }
            else {
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_24)("second point scaled");
            }
            throw new Exception(errorMsg_24);
        }
        let d_17;
        const a_36 = item(2, Polyline2D__get_Points(scaled));
        const b_36 = Pt_$ctor_7B00E9A0(20, 20);
        const vx_17 = a_36.X - b_36.X;
        const vy_17 = a_36.Y - b_36.Y;
        d_17 = Math.sqrt((vx_17 * vx_17) + (vy_17 * vy_17));
        const r_17 = d_17 < 1E-09;
        const actual_25 = r_17;
        if ((actual_25 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, true, "third point scaled");
        }
        else {
            let valueType_25;
            let copyOfStruct_25 = actual_25;
            valueType_25 = bool_type;
            const primitiveTypes_25 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_25;
            if (contains(valueType_25, primitiveTypes_25, {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            })) {
                const arg_30 = toString(true);
                const arg_1_25 = toString(actual_25);
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("third point scaled");
            }
            else {
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_25)("third point scaled");
            }
            throw new Exception(errorMsg_25);
        }
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale on center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const center_1 = Pt_$ctor_7B00E9A0(5, 5);
        const scaled_1 = Polyline2D__ScaleOn(plOpen, center_1, 2);
        let d_18;
        const a_38 = item(0, Polyline2D__get_Points(scaled_1));
        const b_38 = Pt_$ctor_7B00E9A0(-5, -5);
        const vx_18 = a_38.X - b_38.X;
        const vy_18 = a_38.Y - b_38.Y;
        d_18 = Math.sqrt((vx_18 * vx_18) + (vy_18 * vy_18));
        const r_18 = d_18 < 1E-09;
        const actual_26 = r_18;
        if ((actual_26 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, true, "first point scaled on center");
        }
        else {
            let valueType_26;
            let copyOfStruct_26 = actual_26;
            valueType_26 = bool_type;
            const primitiveTypes_26 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_26;
            if (contains(valueType_26, primitiveTypes_26, {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            })) {
                const arg_31 = toString(true);
                const arg_1_26 = toString(actual_26);
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("first point scaled on center");
            }
            else {
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_26)("first point scaled on center");
            }
            throw new Exception(errorMsg_26);
        }
        let d_19;
        const a_40 = item(1, Polyline2D__get_Points(scaled_1));
        const b_40 = Pt_$ctor_7B00E9A0(15, -5);
        const vx_19 = a_40.X - b_40.X;
        const vy_19 = a_40.Y - b_40.Y;
        d_19 = Math.sqrt((vx_19 * vx_19) + (vy_19 * vy_19));
        const r_19 = d_19 < 1E-09;
        const actual_27 = r_19;
        if ((actual_27 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, true, "second point scaled on center");
        }
        else {
            let valueType_27;
            let copyOfStruct_27 = actual_27;
            valueType_27 = bool_type;
            const primitiveTypes_27 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_27;
            if (contains(valueType_27, primitiveTypes_27, {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            })) {
                const arg_32 = toString(true);
                const arg_1_27 = toString(actual_27);
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("second point scaled on center");
            }
            else {
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_27)("second point scaled on center");
            }
            throw new Exception(errorMsg_27);
        }
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const vec = Vc_$ctor_7B00E9A0(5, 3);
        const moved = Polyline2D_translate(vec, plOpen);
        let d_20;
        const a_42 = item(0, Polyline2D__get_Points(moved));
        const b_42 = Pt_$ctor_7B00E9A0(5, 3);
        const vx_20 = a_42.X - b_42.X;
        const vy_20 = a_42.Y - b_42.Y;
        d_20 = Math.sqrt((vx_20 * vx_20) + (vy_20 * vy_20));
        const r_20 = d_20 < 1E-09;
        const actual_28 = r_20;
        if ((actual_28 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, true, "first point translated");
        }
        else {
            let valueType_28;
            let copyOfStruct_28 = actual_28;
            valueType_28 = bool_type;
            const primitiveTypes_28 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_28;
            if (contains(valueType_28, primitiveTypes_28, {
                Equals: equals,
                GetHashCode: (x_28) => (structuralHash(x_28) | 0),
            })) {
                const arg_33 = toString(true);
                const arg_1_28 = toString(actual_28);
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("first point translated");
            }
            else {
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_28)("first point translated");
            }
            throw new Exception(errorMsg_28);
        }
        let d_21;
        const a_44 = item(1, Polyline2D__get_Points(moved));
        const b_44 = Pt_$ctor_7B00E9A0(15, 3);
        const vx_21 = a_44.X - b_44.X;
        const vy_21 = a_44.Y - b_44.Y;
        d_21 = Math.sqrt((vx_21 * vx_21) + (vy_21 * vy_21));
        const r_21 = d_21 < 1E-09;
        const actual_29 = r_21;
        if ((actual_29 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, true, "second point translated");
        }
        else {
            let valueType_29;
            let copyOfStruct_29 = actual_29;
            valueType_29 = bool_type;
            const primitiveTypes_29 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_29;
            if (contains(valueType_29, primitiveTypes_29, {
                Equals: equals,
                GetHashCode: (x_29) => (structuralHash(x_29) | 0),
            })) {
                const arg_34 = toString(true);
                const arg_1_29 = toString(actual_29);
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)("second point translated");
            }
            else {
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_29)("second point translated");
            }
            throw new Exception(errorMsg_29);
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move X and Y", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const movedX = Polyline2D_moveX(5, plOpen);
        let d_22;
        const a_46 = item(0, Polyline2D__get_Points(movedX));
        const b_46 = Pt_$ctor_7B00E9A0(5, 0);
        const vx_22 = a_46.X - b_46.X;
        const vy_22 = a_46.Y - b_46.Y;
        d_22 = Math.sqrt((vx_22 * vx_22) + (vy_22 * vy_22));
        const r_22 = d_22 < 1E-09;
        const actual_30 = r_22;
        if ((actual_30 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, true, "moved X");
        }
        else {
            let valueType_30;
            let copyOfStruct_30 = actual_30;
            valueType_30 = bool_type;
            const primitiveTypes_30 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_30;
            if (contains(valueType_30, primitiveTypes_30, {
                Equals: equals,
                GetHashCode: (x_30) => (structuralHash(x_30) | 0),
            })) {
                const arg_35 = toString(true);
                const arg_1_30 = toString(actual_30);
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("moved X");
            }
            else {
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_30)("moved X");
            }
            throw new Exception(errorMsg_30);
        }
        const movedY = Polyline2D_moveY(3, plOpen);
        let d_23;
        const a_48 = item(0, Polyline2D__get_Points(movedY));
        const b_48 = Pt_$ctor_7B00E9A0(0, 3);
        const vx_23 = a_48.X - b_48.X;
        const vy_23 = a_48.Y - b_48.Y;
        d_23 = Math.sqrt((vx_23 * vx_23) + (vy_23 * vy_23));
        const r_23 = d_23 < 1E-09;
        const actual_31 = r_23;
        if ((actual_31 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, true, "moved Y");
        }
        else {
            let valueType_31;
            let copyOfStruct_31 = actual_31;
            valueType_31 = bool_type;
            const primitiveTypes_31 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_31;
            if (contains(valueType_31, primitiveTypes_31, {
                Equals: equals,
                GetHashCode: (x_31) => (structuralHash(x_31) | 0),
            })) {
                const arg_36 = toString(true);
                const arg_1_31 = toString(actual_31);
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("moved Y");
            }
            else {
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_31)("moved Y");
            }
            throw new Exception(errorMsg_31);
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("reverse", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const reversed = Polyline2D__Reverse(plOpen);
        let d_24;
        const a_50 = item(0, Polyline2D__get_Points(reversed));
        const b_50 = Pt_$ctor_7B00E9A0(0, 10);
        const vx_24 = a_50.X - b_50.X;
        const vy_24 = a_50.Y - b_50.Y;
        d_24 = Math.sqrt((vx_24 * vx_24) + (vy_24 * vy_24));
        const r_24 = d_24 < 1E-09;
        const actual_32 = r_24;
        if ((actual_32 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, true, "first point after reverse");
        }
        else {
            let valueType_32;
            let copyOfStruct_32 = actual_32;
            valueType_32 = bool_type;
            const primitiveTypes_32 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_32;
            if (contains(valueType_32, primitiveTypes_32, {
                Equals: equals,
                GetHashCode: (x_32) => (structuralHash(x_32) | 0),
            })) {
                const arg_37 = toString(true);
                const arg_1_32 = toString(actual_32);
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_32)("first point after reverse");
            }
            else {
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_32)("first point after reverse");
            }
            throw new Exception(errorMsg_32);
        }
        let d_25;
        const a_52 = item(3, Polyline2D__get_Points(reversed));
        const b_52 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_25 = a_52.X - b_52.X;
        const vy_25 = a_52.Y - b_52.Y;
        d_25 = Math.sqrt((vx_25 * vx_25) + (vy_25 * vy_25));
        const r_25 = d_25 < 1E-09;
        const actual_33 = r_25;
        if ((actual_33 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, true, "last point after reverse");
        }
        else {
            let valueType_33;
            let copyOfStruct_33 = actual_33;
            valueType_33 = bool_type;
            const primitiveTypes_33 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_33;
            if (contains(valueType_33, primitiveTypes_33, {
                Equals: equals,
                GetHashCode: (x_33) => (structuralHash(x_33) | 0),
            })) {
                const arg_38 = toString(true);
                const arg_1_33 = toString(actual_33);
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_38)(arg_1_33)("last point after reverse");
            }
            else {
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_33)("last point after reverse");
            }
            throw new Exception(errorMsg_33);
        }
        let d_26;
        const a_54 = item(0, Polyline2D__get_Points(plOpen));
        const b_54 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_26 = a_54.X - b_54.X;
        const vy_26 = a_54.Y - b_54.Y;
        d_26 = Math.sqrt((vx_26 * vx_26) + (vy_26 * vy_26));
        const r_26 = d_26 < 1E-09;
        const actual_34 = r_26;
        if ((actual_34 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, true, "original unchanged");
        }
        else {
            let valueType_34;
            let copyOfStruct_34 = actual_34;
            valueType_34 = bool_type;
            const primitiveTypes_34 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_34;
            if (contains(valueType_34, primitiveTypes_34, {
                Equals: equals,
                GetHashCode: (x_34) => (structuralHash(x_34) | 0),
            })) {
                const arg_39 = toString(true);
                const arg_1_34 = toString(actual_34);
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_39)(arg_1_34)("original unchanged");
            }
            else {
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_34)("original unchanged");
            }
            throw new Exception(errorMsg_34);
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("reverse in place", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const copy = Polyline2D__Clone(plOpen);
        Polyline2D__ReverseInPlace(copy);
        let d_27;
        const a_56 = item(0, Polyline2D__get_Points(copy));
        const b_56 = Pt_$ctor_7B00E9A0(0, 10);
        const vx_27 = a_56.X - b_56.X;
        const vy_27 = a_56.Y - b_56.Y;
        d_27 = Math.sqrt((vx_27 * vx_27) + (vy_27 * vy_27));
        const r_27 = d_27 < 1E-09;
        const actual_35 = r_27;
        if ((actual_35 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, true, "first point after reverse in place");
        }
        else {
            let valueType_35;
            let copyOfStruct_35 = actual_35;
            valueType_35 = bool_type;
            const primitiveTypes_35 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_35;
            if (contains(valueType_35, primitiveTypes_35, {
                Equals: equals,
                GetHashCode: (x_35) => (structuralHash(x_35) | 0),
            })) {
                const arg_40 = toString(true);
                const arg_1_35 = toString(actual_35);
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_40)(arg_1_35)("first point after reverse in place");
            }
            else {
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_35)("first point after reverse in place");
            }
            throw new Exception(errorMsg_35);
        }
        let d_28;
        const a_58 = item(3, Polyline2D__get_Points(copy));
        const b_58 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_28 = a_58.X - b_58.X;
        const vy_28 = a_58.Y - b_58.Y;
        d_28 = Math.sqrt((vx_28 * vx_28) + (vy_28 * vy_28));
        const r_28 = d_28 < 1E-09;
        const actual_36 = r_28;
        if ((actual_36 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, true, "last point after reverse in place");
        }
        else {
            let valueType_36;
            let copyOfStruct_36 = actual_36;
            valueType_36 = bool_type;
            const primitiveTypes_36 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_36;
            if (contains(valueType_36, primitiveTypes_36, {
                Equals: equals,
                GetHashCode: (x_36) => (structuralHash(x_36) | 0),
            })) {
                const arg_41 = toString(true);
                const arg_1_36 = toString(actual_36);
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_41)(arg_1_36)("last point after reverse in place");
            }
            else {
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_36)("last point after reverse in place");
            }
            throw new Exception(errorMsg_36);
        }
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})()])), Test_testList("Point Containment", ofArray([(() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("winding number and contains", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const insidePoint = Pt_$ctor_7B00E9A0(5, 5);
        const outsidePoint = Pt_$ctor_7B00E9A0(15, 5);
        const windingInside = Polyline2D__WindingNumber_6ADE94FD(plClosed, insidePoint) | 0;
        const windingOutside = Polyline2D__WindingNumber_6ADE94FD(plClosed, outsidePoint) | 0;
        Expect_isTrue(windingInside !== 0)("inside point should have non-zero winding");
        Expect_isTrue(windingOutside === 0)("outside point should have zero winding");
        Expect_isTrue(Polyline2D__Contains_6ADE94FD(plClosed, insidePoint))("should contain inside point");
        Expect_isFalse(Polyline2D__Contains_6ADE94FD(plClosed, outsidePoint))("should not contain outside point");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("WindingNumber - various cases", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let square;
        const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
        square = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
        const inside = Pt_$ctor_7B00E9A0(5, 5);
        const windingInside_1 = Polyline2D__WindingNumber_6ADE94FD(square, inside) | 0;
        Expect_isTrue(windingInside_1 !== 0)("winding number for inside point should be non-zero");
        const outside = Pt_$ctor_7B00E9A0(15, 5);
        const windingOutside_1 = Polyline2D__WindingNumber_6ADE94FD(square, outside) | 0;
        const actual_37 = windingOutside_1 | 0;
        if ((actual_37 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, 0, "winding number for outside point should be zero");
        }
        else {
            let valueType_37;
            let copyOfStruct_37 = actual_37;
            valueType_37 = int32_type;
            const primitiveTypes_37 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_37;
            if (contains(valueType_37, primitiveTypes_37, {
                Equals: equals,
                GetHashCode: (x_37) => (structuralHash(x_37) | 0),
            })) {
                const arg_42 = int32ToString(0);
                const arg_1_37 = int32ToString(actual_37);
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_42)(arg_1_37)("winding number for outside point should be zero");
            }
            else {
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_37)("winding number for outside point should be zero");
            }
            throw new Exception(errorMsg_37);
        }
        const empty = Polyline2D_$ctor();
        const windingEmpty = Polyline2D__WindingNumber_6ADE94FD(empty, Pt_$ctor_7B00E9A0(5, 5)) | 0;
        const actual_38 = windingEmpty | 0;
        if ((actual_38 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, 0, "winding number for empty polyline should be zero");
        }
        else {
            let valueType_38;
            let copyOfStruct_38 = actual_38;
            valueType_38 = int32_type;
            const primitiveTypes_38 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_38;
            if (contains(valueType_38, primitiveTypes_38, {
                Equals: equals,
                GetHashCode: (x_38) => (structuralHash(x_38) | 0),
            })) {
                const arg_43 = int32ToString(0);
                const arg_1_38 = int32ToString(actual_38);
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_43)(arg_1_38)("winding number for empty polyline should be zero");
            }
            else {
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_38)("winding number for empty polyline should be zero");
            }
            throw new Exception(errorMsg_38);
        }
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})()])), Test_testList("Creation and Manipulation", ofArray([(() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("clone and duplicate", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const cloned = Polyline2D__Clone(plOpen);
        const duplicated = Polyline2D__Duplicate(plOpen);
        const actual_40 = Polyline2D__get_PointCount(cloned) | 0;
        const expected_75 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_40 === expected_75) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_40, expected_75, "cloned point count");
        }
        else {
            let valueType_39;
            let copyOfStruct_39 = actual_40;
            valueType_39 = int32_type;
            const primitiveTypes_39 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_39;
            if (contains(valueType_39, primitiveTypes_39, {
                Equals: equals,
                GetHashCode: (x_39) => (structuralHash(x_39) | 0),
            })) {
                const arg_44 = int32ToString(expected_75);
                const arg_1_39 = int32ToString(actual_40);
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_44)(arg_1_39)("cloned point count");
            }
            else {
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_75)(actual_40)("cloned point count");
            }
            throw new Exception(errorMsg_39);
        }
        const actual_42 = Polyline2D__get_PointCount(duplicated) | 0;
        const expected_77 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_42 === expected_77) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_42, expected_77, "duplicated point count");
        }
        else {
            let valueType_40;
            let copyOfStruct_40 = actual_42;
            valueType_40 = int32_type;
            const primitiveTypes_40 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_40;
            if (contains(valueType_40, primitiveTypes_40, {
                Equals: equals,
                GetHashCode: (x_40) => (structuralHash(x_40) | 0),
            })) {
                const arg_45 = int32ToString(expected_77);
                const arg_1_40 = int32ToString(actual_42);
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_45)(arg_1_40)("duplicated point count");
            }
            else {
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_77)(actual_42)("duplicated point count");
            }
            throw new Exception(errorMsg_40);
        }
        let d_29;
        const a_60 = item(0, Polyline2D__get_Points(cloned));
        const b_60 = item(0, Polyline2D__get_Points(plOpen));
        const vx_29 = a_60.X - b_60.X;
        const vy_29 = a_60.Y - b_60.Y;
        d_29 = Math.sqrt((vx_29 * vx_29) + (vy_29 * vy_29));
        const r_29 = d_29 < 1E-09;
        const actual_43 = r_29;
        if ((actual_43 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_43, true, "cloned first point");
        }
        else {
            let valueType_41;
            let copyOfStruct_41 = actual_43;
            valueType_41 = bool_type;
            const primitiveTypes_41 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_41;
            if (contains(valueType_41, primitiveTypes_41, {
                Equals: equals,
                GetHashCode: (x_41) => (structuralHash(x_41) | 0),
            })) {
                const arg_46 = toString(true);
                const arg_1_41 = toString(actual_43);
                errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_46)(arg_1_41)("cloned first point");
            }
            else {
                errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_43)("cloned first point");
            }
            throw new Exception(errorMsg_41);
        }
        let d_30;
        const a_62 = item(0, Polyline2D__get_Points(duplicated));
        const b_62 = item(0, Polyline2D__get_Points(plOpen));
        const vx_30 = a_62.X - b_62.X;
        const vy_30 = a_62.Y - b_62.Y;
        d_30 = Math.sqrt((vx_30 * vx_30) + (vy_30 * vy_30));
        const r_30 = d_30 < 1E-09;
        const actual_44 = r_30;
        if ((actual_44 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_44, true, "duplicated first point");
        }
        else {
            let valueType_42;
            let copyOfStruct_42 = actual_44;
            valueType_42 = bool_type;
            const primitiveTypes_42 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_42;
            if (contains(valueType_42, primitiveTypes_42, {
                Equals: equals,
                GetHashCode: (x_42) => (structuralHash(x_42) | 0),
            })) {
                const arg_47 = toString(true);
                const arg_1_42 = toString(actual_44);
                errorMsg_42 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_47)(arg_1_42)("duplicated first point");
            }
            else {
                errorMsg_42 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_44)("duplicated first point");
            }
            throw new Exception(errorMsg_42);
        }
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close if open", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const copy_1 = Polyline2D__Clone(plOpen);
        Polyline2D__CloseInPlace_5E38073B(copy_1, 1E-06);
        const actual_46 = Polyline2D__get_PointCount(copy_1) | 0;
        const expected_83 = (Polyline2D__get_PointCount(plOpen) + 1) | 0;
        if ((actual_46 === expected_83) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_46, expected_83, "closed polyline point count");
        }
        else {
            let valueType_43;
            let copyOfStruct_43 = actual_46;
            valueType_43 = int32_type;
            const primitiveTypes_43 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_43;
            if (contains(valueType_43, primitiveTypes_43, {
                Equals: equals,
                GetHashCode: (x_43) => (structuralHash(x_43) | 0),
            })) {
                const arg_48 = int32ToString(expected_83);
                const arg_1_43 = int32ToString(actual_46);
                errorMsg_43 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_48)(arg_1_43)("closed polyline point count");
            }
            else {
                errorMsg_43 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_83)(actual_46)("closed polyline point count");
            }
            throw new Exception(errorMsg_43);
        }
        let d_31;
        const a_64 = item(Polyline2D__get_PointCount(copy_1) - 1, Polyline2D__get_Points(copy_1));
        const b_64 = item(0, Polyline2D__get_Points(copy_1));
        const vx_31 = a_64.X - b_64.X;
        const vy_31 = a_64.Y - b_64.Y;
        d_31 = Math.sqrt((vx_31 * vx_31) + (vy_31 * vy_31));
        const r_31 = d_31 < 1E-09;
        const actual_47 = r_31;
        if ((actual_47 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_47, true, "last point equals first");
        }
        else {
            let valueType_44;
            let copyOfStruct_44 = actual_47;
            valueType_44 = bool_type;
            const primitiveTypes_44 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_44;
            if (contains(valueType_44, primitiveTypes_44, {
                Equals: equals,
                GetHashCode: (x_44) => (structuralHash(x_44) | 0),
            })) {
                const arg_49 = toString(true);
                const arg_1_44 = toString(actual_47);
                errorMsg_44 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_49)(arg_1_44)("last point equals first");
            }
            else {
                errorMsg_44 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_47)("last point equals first");
            }
            throw new Exception(errorMsg_44);
        }
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("sub polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const sub = Polyline2D_subPolyline(0.5, 2.5, plOpen);
        const actual_49 = Polyline2D__get_PointCount(sub) | 0;
        if ((actual_49 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_49, 4, "sub polyline point count");
        }
        else {
            let valueType_45;
            let copyOfStruct_45 = actual_49;
            valueType_45 = int32_type;
            const primitiveTypes_45 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_45;
            if (contains(valueType_45, primitiveTypes_45, {
                Equals: equals,
                GetHashCode: (x_45) => (structuralHash(x_45) | 0),
            })) {
                const arg_50 = int32ToString(4);
                const arg_1_45 = int32ToString(actual_49);
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_50)(arg_1_45)("sub polyline point count");
            }
            else {
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_49)("sub polyline point count");
            }
            throw new Exception(errorMsg_45);
        }
        let d_32;
        const a_67 = item(0, Polyline2D__get_Points(sub));
        const b_67 = Pt_$ctor_7B00E9A0(5, 0);
        const vx_32 = a_67.X - b_67.X;
        const vy_32 = a_67.Y - b_67.Y;
        d_32 = Math.sqrt((vx_32 * vx_32) + (vy_32 * vy_32));
        const r_32 = d_32 < 1E-09;
        const actual_50 = r_32;
        if ((actual_50 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_50, true, "sub polyline start");
        }
        else {
            let valueType_46;
            let copyOfStruct_46 = actual_50;
            valueType_46 = bool_type;
            const primitiveTypes_46 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_46;
            if (contains(valueType_46, primitiveTypes_46, {
                Equals: equals,
                GetHashCode: (x_46) => (structuralHash(x_46) | 0),
            })) {
                const arg_51 = toString(true);
                const arg_1_46 = toString(actual_50);
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_51)(arg_1_46)("sub polyline start");
            }
            else {
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_50)("sub polyline start");
            }
            throw new Exception(errorMsg_46);
        }
        let d_33;
        const a_69 = item(3, Polyline2D__get_Points(sub));
        const b_69 = Pt_$ctor_7B00E9A0(5, 10);
        const vx_33 = a_69.X - b_69.X;
        const vy_33 = a_69.Y - b_69.Y;
        d_33 = Math.sqrt((vx_33 * vx_33) + (vy_33 * vy_33));
        const r_33 = d_33 < 1E-09;
        const actual_51 = r_33;
        if ((actual_51 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_51, true, "sub polyline end");
        }
        else {
            let valueType_47;
            let copyOfStruct_47 = actual_51;
            valueType_47 = bool_type;
            const primitiveTypes_47 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_47;
            if (contains(valueType_47, primitiveTypes_47, {
                Equals: equals,
                GetHashCode: (x_47) => (structuralHash(x_47) | 0),
            })) {
                const arg_52 = toString(true);
                const arg_1_47 = toString(actual_51);
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_52)(arg_1_47)("sub polyline end");
            }
            else {
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_51)("sub polyline end");
            }
            throw new Exception(errorMsg_47);
        }
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})()])), Test_testList("Error Handling", ofArray([(() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("empty polyline errors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        Expect_throws(() => {
            Polyline2D__get_Start(plEmpty);
        }, "start on empty should throw");
        Expect_throws(() => {
            Polyline2D__get_End(plEmpty);
        }, "end on empty should throw");
        const actual_53 = Polyline2D__get_Length(plEmpty);
        if ((actual_53 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_53, 0, "length on empty =0");
        }
        else {
            let valueType_48;
            let copyOfStruct_48 = actual_53;
            valueType_48 = float64_type;
            const primitiveTypes_48 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_48;
            if (contains(valueType_48, primitiveTypes_48, {
                Equals: equals,
                GetHashCode: (x_48) => (structuralHash(x_48) | 0),
            })) {
                const arg_53 = (0).toString();
                const arg_1_48 = actual_53.toString();
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_53)(arg_1_48)("length on empty =0");
            }
            else {
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_53)("length on empty =0");
            }
            throw new Exception(errorMsg_48);
        }
        Expect_throws(() => {
            Polyline2D__get_FirstSegment(plEmpty);
        }, "first segment on empty should throw");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("single point polyline errors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        Expect_throws(() => {
            Polyline2D__get_FirstSegment(plSinglePoint);
        }, "first segment on single point should throw");
        Expect_isFalse(Polyline2D__get_IsClosed(plSinglePoint))("is closed on single point should be false");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("invalid parameters", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        Expect_throws(() => {
            Polyline2D__EvaluateAt_5E38073B(plOpen, -1);
        }, "negative parameter should throw");
        Expect_throws(() => {
            Polyline2D__EvaluateAt_5E38073B(plOpen, 10);
        }, "too large parameter should throw");
        Expect_throws(() => {
            Polyline2D__GetSegment_Z524259A4(plOpen, -1);
        }, "negative segment index should throw");
        Expect_throws(() => {
            Polyline2D__GetSegment_Z524259A4(plOpen, 10);
        }, "too large segment index should throw");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})()])), Test_testList("Static Methods", ofArray([(() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create methods", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let fromSeq;
        const points_1 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 1)];
        fromSeq = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_1));
        const empty_1 = Polyline2D_$ctor_Z5FD8CF3C([]);
        const actual_54 = Polyline2D__get_PointCount(fromSeq) | 0;
        if ((actual_54 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_54, 2, "created from sequence has correct count");
        }
        else {
            let valueType_49;
            let copyOfStruct_49 = actual_54;
            valueType_49 = int32_type;
            const primitiveTypes_49 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_49;
            if (contains(valueType_49, primitiveTypes_49, {
                Equals: equals,
                GetHashCode: (x_49) => (structuralHash(x_49) | 0),
            })) {
                const arg_54 = int32ToString(2);
                const arg_1_49 = int32ToString(actual_54);
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_54)(arg_1_49)("created from sequence has correct count");
            }
            else {
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_54)("created from sequence has correct count");
            }
            throw new Exception(errorMsg_49);
        }
        const actual_55 = Polyline2D__get_PointCount(empty_1) | 0;
        if ((actual_55 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_55, 0, "empty has no points");
        }
        else {
            let valueType_50;
            let copyOfStruct_50 = actual_55;
            valueType_50 = int32_type;
            const primitiveTypes_50 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_50;
            if (contains(valueType_50, primitiveTypes_50, {
                Equals: equals,
                GetHashCode: (x_50) => (structuralHash(x_50) | 0),
            })) {
                const arg_55 = int32ToString(0);
                const arg_1_50 = int32ToString(actual_55);
                errorMsg_50 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_55)(arg_1_50)("empty has no points");
            }
            else {
                errorMsg_50 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_55)("empty has no points");
            }
            throw new Exception(errorMsg_50);
        }
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("map function", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const scaled_2 = Polyline2D_map((pt) => {
            const a_70 = pt;
            return Pt_$ctor_7B00E9A0_1(a_70.X * 2, a_70.Y * 2);
        }, plOpen);
        let d_34;
        const a_72 = item(0, Polyline2D__get_Points(scaled_2));
        const b_71 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_34 = a_72.X - b_71.X;
        const vy_34 = a_72.Y - b_71.Y;
        d_34 = Math.sqrt((vx_34 * vx_34) + (vy_34 * vy_34));
        const r_34 = d_34 < 1E-09;
        const actual_56 = r_34;
        if ((actual_56 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_56, true, "mapped first point");
        }
        else {
            let valueType_51;
            let copyOfStruct_51 = actual_56;
            valueType_51 = bool_type;
            const primitiveTypes_51 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_51;
            if (contains(valueType_51, primitiveTypes_51, {
                Equals: equals,
                GetHashCode: (x_51) => (structuralHash(x_51) | 0),
            })) {
                const arg_56 = toString(true);
                const arg_1_51 = toString(actual_56);
                errorMsg_51 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_56)(arg_1_51)("mapped first point");
            }
            else {
                errorMsg_51 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_56)("mapped first point");
            }
            throw new Exception(errorMsg_51);
        }
        let d_35;
        const a_74 = item(1, Polyline2D__get_Points(scaled_2));
        const b_73 = Pt_$ctor_7B00E9A0(20, 0);
        const vx_35 = a_74.X - b_73.X;
        const vy_35 = a_74.Y - b_73.Y;
        d_35 = Math.sqrt((vx_35 * vx_35) + (vy_35 * vy_35));
        const r_35 = d_35 < 1E-09;
        const actual_57 = r_35;
        if ((actual_57 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_57, true, "mapped second point");
        }
        else {
            let valueType_52;
            let copyOfStruct_52 = actual_57;
            valueType_52 = bool_type;
            const primitiveTypes_52 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_52;
            if (contains(valueType_52, primitiveTypes_52, {
                Equals: equals,
                GetHashCode: (x_52) => (structuralHash(x_52) | 0),
            })) {
                const arg_57 = toString(true);
                const arg_1_52 = toString(actual_57);
                errorMsg_52 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_57)(arg_1_52)("mapped second point");
            }
            else {
                errorMsg_52 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_57)("mapped second point");
            }
            throw new Exception(errorMsg_52);
        }
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let rotation;
        const rad = 3.141592653589793 / 2;
        rotation = Rotation2D_$ctor_7B00E9A0(Math.sin(rad), Math.cos(rad));
        const rotated = Polyline2D_rotate(rotation, plLine);
        let d_36;
        const a_76 = item(1, Polyline2D__get_Points(rotated));
        const b_75 = Pt_$ctor_7B00E9A0(0, 10);
        const vx_36 = a_76.X - b_75.X;
        const vy_36 = a_76.Y - b_75.Y;
        d_36 = Math.sqrt((vx_36 * vx_36) + (vy_36 * vy_36));
        const r_37 = d_36 < 1E-09;
        const actual_58 = r_37;
        if ((actual_58 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_58, true, "rotated point");
        }
        else {
            let valueType_53;
            let copyOfStruct_53 = actual_58;
            valueType_53 = bool_type;
            const primitiveTypes_53 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_53;
            if (contains(valueType_53, primitiveTypes_53, {
                Equals: equals,
                GetHashCode: (x_53) => (structuralHash(x_53) | 0),
            })) {
                const arg_58 = toString(true);
                const arg_1_53 = toString(actual_58);
                errorMsg_53 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_58)(arg_1_53)("rotated point");
            }
            else {
                errorMsg_53 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_58)("rotated point");
            }
            throw new Exception(errorMsg_53);
        }
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})()])), Test_testList("Offsetting", ofArray([(() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("5 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        const o = Polyline2D_offset_Z45C468A5(plClosed, 2, false, true, 1, -0.9961946980917455);
        let d_37;
        const a_78 = item(0, Polyline2D__get_Points(o));
        const b_77 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_37 = a_78.X - b_77.X;
        const vy_37 = a_78.Y - b_77.Y;
        d_37 = Math.sqrt((vx_37 * vx_37) + (vy_37 * vy_37));
        const r_38 = d_37 < 1E-09;
        const actual_59 = r_38;
        if ((actual_59 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_59, true, "pt 0 ok");
        }
        else {
            let valueType_54;
            let copyOfStruct_54 = actual_59;
            valueType_54 = bool_type;
            const primitiveTypes_54 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_54;
            if (contains(valueType_54, primitiveTypes_54, {
                Equals: equals,
                GetHashCode: (x_54) => (structuralHash(x_54) | 0),
            })) {
                const arg_59 = toString(true);
                const arg_1_54 = toString(actual_59);
                errorMsg_54 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_59)(arg_1_54)("pt 0 ok");
            }
            else {
                errorMsg_54 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_59)("pt 0 ok");
            }
            throw new Exception(errorMsg_54);
        }
        let d_38;
        const a_80 = item(1, Polyline2D__get_Points(o));
        const b_79 = Pt_$ctor_7B00E9A0(8, 2);
        const vx_38 = a_80.X - b_79.X;
        const vy_38 = a_80.Y - b_79.Y;
        d_38 = Math.sqrt((vx_38 * vx_38) + (vy_38 * vy_38));
        const r_39 = d_38 < 1E-09;
        const actual_60 = r_39;
        if ((actual_60 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_60, true, "pt 1 ok");
        }
        else {
            let valueType_55;
            let copyOfStruct_55 = actual_60;
            valueType_55 = bool_type;
            const primitiveTypes_55 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_55;
            if (contains(valueType_55, primitiveTypes_55, {
                Equals: equals,
                GetHashCode: (x_55) => (structuralHash(x_55) | 0),
            })) {
                const arg_60 = toString(true);
                const arg_1_55 = toString(actual_60);
                errorMsg_55 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_60)(arg_1_55)("pt 1 ok");
            }
            else {
                errorMsg_55 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_60)("pt 1 ok");
            }
            throw new Exception(errorMsg_55);
        }
        let d_39;
        const a_82 = item(2, Polyline2D__get_Points(o));
        const b_81 = Pt_$ctor_7B00E9A0(8, 8);
        const vx_39 = a_82.X - b_81.X;
        const vy_39 = a_82.Y - b_81.Y;
        d_39 = Math.sqrt((vx_39 * vx_39) + (vy_39 * vy_39));
        const r_40 = d_39 < 1E-09;
        const actual_61 = r_40;
        if ((actual_61 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_61, true, "pt 2 ok");
        }
        else {
            let valueType_56;
            let copyOfStruct_56 = actual_61;
            valueType_56 = bool_type;
            const primitiveTypes_56 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_56;
            if (contains(valueType_56, primitiveTypes_56, {
                Equals: equals,
                GetHashCode: (x_56) => (structuralHash(x_56) | 0),
            })) {
                const arg_61 = toString(true);
                const arg_1_56 = toString(actual_61);
                errorMsg_56 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_61)(arg_1_56)("pt 2 ok");
            }
            else {
                errorMsg_56 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_61)("pt 2 ok");
            }
            throw new Exception(errorMsg_56);
        }
        let d_40;
        const a_84 = item(3, Polyline2D__get_Points(o));
        const b_83 = Pt_$ctor_7B00E9A0(2, 8);
        const vx_40 = a_84.X - b_83.X;
        const vy_40 = a_84.Y - b_83.Y;
        d_40 = Math.sqrt((vx_40 * vx_40) + (vy_40 * vy_40));
        const r_41 = d_40 < 1E-09;
        const actual_62 = r_41;
        if ((actual_62 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_62, true, "pt 3 ok");
        }
        else {
            let valueType_57;
            let copyOfStruct_57 = actual_62;
            valueType_57 = bool_type;
            const primitiveTypes_57 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_57;
            if (contains(valueType_57, primitiveTypes_57, {
                Equals: equals,
                GetHashCode: (x_57) => (structuralHash(x_57) | 0),
            })) {
                const arg_62 = toString(true);
                const arg_1_57 = toString(actual_62);
                errorMsg_57 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_62)(arg_1_57)("pt 3 ok");
            }
            else {
                errorMsg_57 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_62)("pt 3 ok");
            }
            throw new Exception(errorMsg_57);
        }
        let d_41;
        const a_86 = item(4, Polyline2D__get_Points(o));
        const b_85 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_41 = a_86.X - b_85.X;
        const vy_41 = a_86.Y - b_85.Y;
        d_41 = Math.sqrt((vx_41 * vx_41) + (vy_41 * vy_41));
        const r_42 = d_41 < 1E-09;
        const actual_63 = r_42;
        if ((actual_63 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_63, true, "pt 4 ok");
        }
        else {
            let valueType_58;
            let copyOfStruct_58 = actual_63;
            valueType_58 = bool_type;
            const primitiveTypes_58 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_58;
            if (contains(valueType_58, primitiveTypes_58, {
                Equals: equals,
                GetHashCode: (x_58) => (structuralHash(x_58) | 0),
            })) {
                const arg_63 = toString(true);
                const arg_1_58 = toString(actual_63);
                errorMsg_58 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_63)(arg_1_58)("pt 4 ok");
            }
            else {
                errorMsg_58 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_63)("pt 4 ok");
            }
            throw new Exception(errorMsg_58);
        }
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points open", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        const o_1 = Polyline2D_offset_Z45C468A5(plOpen, 2, false, true, 1, -0.9961946980917455);
        let d_42;
        const a_88 = item(0, Polyline2D__get_Points(o_1));
        const b_87 = Pt_$ctor_7B00E9A0(0, 2);
        const vx_42 = a_88.X - b_87.X;
        const vy_42 = a_88.Y - b_87.Y;
        d_42 = Math.sqrt((vx_42 * vx_42) + (vy_42 * vy_42));
        const r_43 = d_42 < 1E-09;
        const actual_64 = r_43;
        if ((actual_64 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_64, true, "pt 0 ok");
        }
        else {
            let valueType_59;
            let copyOfStruct_59 = actual_64;
            valueType_59 = bool_type;
            const primitiveTypes_59 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_59;
            if (contains(valueType_59, primitiveTypes_59, {
                Equals: equals,
                GetHashCode: (x_59) => (structuralHash(x_59) | 0),
            })) {
                const arg_64 = toString(true);
                const arg_1_59 = toString(actual_64);
                errorMsg_59 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_64)(arg_1_59)("pt 0 ok");
            }
            else {
                errorMsg_59 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_64)("pt 0 ok");
            }
            throw new Exception(errorMsg_59);
        }
        let d_43;
        const a_90 = item(1, Polyline2D__get_Points(o_1));
        const b_89 = Pt_$ctor_7B00E9A0(8, 2);
        const vx_43 = a_90.X - b_89.X;
        const vy_43 = a_90.Y - b_89.Y;
        d_43 = Math.sqrt((vx_43 * vx_43) + (vy_43 * vy_43));
        const r_44 = d_43 < 1E-09;
        const actual_65 = r_44;
        if ((actual_65 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_65, true, "pt 1 ok");
        }
        else {
            let valueType_60;
            let copyOfStruct_60 = actual_65;
            valueType_60 = bool_type;
            const primitiveTypes_60 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_60;
            if (contains(valueType_60, primitiveTypes_60, {
                Equals: equals,
                GetHashCode: (x_60) => (structuralHash(x_60) | 0),
            })) {
                const arg_65 = toString(true);
                const arg_1_60 = toString(actual_65);
                errorMsg_60 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_65)(arg_1_60)("pt 1 ok");
            }
            else {
                errorMsg_60 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_65)("pt 1 ok");
            }
            throw new Exception(errorMsg_60);
        }
        let d_44;
        const a_92 = item(2, Polyline2D__get_Points(o_1));
        const b_91 = Pt_$ctor_7B00E9A0(8, 8);
        const vx_44 = a_92.X - b_91.X;
        const vy_44 = a_92.Y - b_91.Y;
        d_44 = Math.sqrt((vx_44 * vx_44) + (vy_44 * vy_44));
        const r_45 = d_44 < 1E-09;
        const actual_66 = r_45;
        if ((actual_66 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_66, true, "pt 2 ok");
        }
        else {
            let valueType_61;
            let copyOfStruct_61 = actual_66;
            valueType_61 = bool_type;
            const primitiveTypes_61 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_61;
            if (contains(valueType_61, primitiveTypes_61, {
                Equals: equals,
                GetHashCode: (x_61) => (structuralHash(x_61) | 0),
            })) {
                const arg_66 = toString(true);
                const arg_1_61 = toString(actual_66);
                errorMsg_61 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_66)(arg_1_61)("pt 2 ok");
            }
            else {
                errorMsg_61 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_66)("pt 2 ok");
            }
            throw new Exception(errorMsg_61);
        }
        let d_45;
        const a_94 = item(3, Polyline2D__get_Points(o_1));
        const b_93 = Pt_$ctor_7B00E9A0(0, 8);
        const vx_45 = a_94.X - b_93.X;
        const vy_45 = a_94.Y - b_93.Y;
        d_45 = Math.sqrt((vx_45 * vx_45) + (vy_45 * vy_45));
        const r_46 = d_45 < 1E-09;
        const actual_67 = r_46;
        if ((actual_67 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_67, true, "pt 3 ok");
        }
        else {
            let valueType_62;
            let copyOfStruct_62 = actual_67;
            valueType_62 = bool_type;
            const primitiveTypes_62 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_62;
            if (contains(valueType_62, primitiveTypes_62, {
                Equals: equals,
                GetHashCode: (x_62) => (structuralHash(x_62) | 0),
            })) {
                const arg_67 = toString(true);
                const arg_1_62 = toString(actual_67);
                errorMsg_62 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_67)(arg_1_62)("pt 3 ok");
            }
            else {
                errorMsg_62 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_67)("pt 3 ok");
            }
            throw new Exception(errorMsg_62);
        }
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points looped", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        const o_2 = Polyline2D_offset_Z45C468A5(plOpen, 2, true, true, 1, -0.9961946980917455);
        let d_46;
        const a_96 = item(0, Polyline2D__get_Points(o_2));
        const b_95 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_46 = a_96.X - b_95.X;
        const vy_46 = a_96.Y - b_95.Y;
        d_46 = Math.sqrt((vx_46 * vx_46) + (vy_46 * vy_46));
        const r_47 = d_46 < 1E-09;
        const actual_68 = r_47;
        if ((actual_68 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_68, true, "pt 0 ok");
        }
        else {
            let valueType_63;
            let copyOfStruct_63 = actual_68;
            valueType_63 = bool_type;
            const primitiveTypes_63 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_63;
            if (contains(valueType_63, primitiveTypes_63, {
                Equals: equals,
                GetHashCode: (x_63) => (structuralHash(x_63) | 0),
            })) {
                const arg_68 = toString(true);
                const arg_1_63 = toString(actual_68);
                errorMsg_63 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_68)(arg_1_63)("pt 0 ok");
            }
            else {
                errorMsg_63 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_68)("pt 0 ok");
            }
            throw new Exception(errorMsg_63);
        }
        let d_47;
        const a_98 = item(1, Polyline2D__get_Points(o_2));
        const b_97 = Pt_$ctor_7B00E9A0(8, 2);
        const vx_47 = a_98.X - b_97.X;
        const vy_47 = a_98.Y - b_97.Y;
        d_47 = Math.sqrt((vx_47 * vx_47) + (vy_47 * vy_47));
        const r_48 = d_47 < 1E-09;
        const actual_69 = r_48;
        if ((actual_69 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_69, true, "pt 1 ok");
        }
        else {
            let valueType_64;
            let copyOfStruct_64 = actual_69;
            valueType_64 = bool_type;
            const primitiveTypes_64 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_64;
            if (contains(valueType_64, primitiveTypes_64, {
                Equals: equals,
                GetHashCode: (x_64) => (structuralHash(x_64) | 0),
            })) {
                const arg_69 = toString(true);
                const arg_1_64 = toString(actual_69);
                errorMsg_64 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_69)(arg_1_64)("pt 1 ok");
            }
            else {
                errorMsg_64 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_69)("pt 1 ok");
            }
            throw new Exception(errorMsg_64);
        }
        let d_48;
        const a_100 = item(2, Polyline2D__get_Points(o_2));
        const b_99 = Pt_$ctor_7B00E9A0(8, 8);
        const vx_48 = a_100.X - b_99.X;
        const vy_48 = a_100.Y - b_99.Y;
        d_48 = Math.sqrt((vx_48 * vx_48) + (vy_48 * vy_48));
        const r_49 = d_48 < 1E-09;
        const actual_70 = r_49;
        if ((actual_70 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_70, true, "pt 2 ok");
        }
        else {
            let valueType_65;
            let copyOfStruct_65 = actual_70;
            valueType_65 = bool_type;
            const primitiveTypes_65 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_65;
            if (contains(valueType_65, primitiveTypes_65, {
                Equals: equals,
                GetHashCode: (x_65) => (structuralHash(x_65) | 0),
            })) {
                const arg_70 = toString(true);
                const arg_1_65 = toString(actual_70);
                errorMsg_65 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_70)(arg_1_65)("pt 2 ok");
            }
            else {
                errorMsg_65 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_70)("pt 2 ok");
            }
            throw new Exception(errorMsg_65);
        }
        let d_49;
        const a_102 = item(3, Polyline2D__get_Points(o_2));
        const b_101 = Pt_$ctor_7B00E9A0(2, 8);
        const vx_49 = a_102.X - b_101.X;
        const vy_49 = a_102.Y - b_101.Y;
        d_49 = Math.sqrt((vx_49 * vx_49) + (vy_49 * vy_49));
        const r_50 = d_49 < 1E-09;
        const actual_71 = r_50;
        if ((actual_71 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_71, true, "pt 3 ok");
        }
        else {
            let valueType_66;
            let copyOfStruct_66 = actual_71;
            valueType_66 = bool_type;
            const primitiveTypes_66 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_66;
            if (contains(valueType_66, primitiveTypes_66, {
                Equals: equals,
                GetHashCode: (x_66) => (structuralHash(x_66) | 0),
            })) {
                const arg_71 = toString(true);
                const arg_1_66 = toString(actual_71);
                errorMsg_66 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_71)(arg_1_66)("pt 3 ok");
            }
            else {
                errorMsg_66 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_71)("pt 3 ok");
            }
            throw new Exception(errorMsg_66);
        }
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points looped  2 loop", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        const o_3 = Polyline2D_offset_Z45C468A5(plOpen, 2, true, true, 1, -0.9961946980917455);
        let d_50;
        const a_104 = item(0, Polyline2D__get_Points(o_3));
        const b_103 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_50 = a_104.X - b_103.X;
        const vy_50 = a_104.Y - b_103.Y;
        d_50 = Math.sqrt((vx_50 * vx_50) + (vy_50 * vy_50));
        const r_51 = d_50 < 1E-09;
        const actual_72 = r_51;
        if ((actual_72 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_72, true, "pt 0 ok");
        }
        else {
            let valueType_67;
            let copyOfStruct_67 = actual_72;
            valueType_67 = bool_type;
            const primitiveTypes_67 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_67;
            if (contains(valueType_67, primitiveTypes_67, {
                Equals: equals,
                GetHashCode: (x_67) => (structuralHash(x_67) | 0),
            })) {
                const arg_72 = toString(true);
                const arg_1_67 = toString(actual_72);
                errorMsg_67 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_72)(arg_1_67)("pt 0 ok");
            }
            else {
                errorMsg_67 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_72)("pt 0 ok");
            }
            throw new Exception(errorMsg_67);
        }
        let d_51;
        const a_106 = item(1, Polyline2D__get_Points(o_3));
        const b_105 = Pt_$ctor_7B00E9A0(8, 2);
        const vx_51 = a_106.X - b_105.X;
        const vy_51 = a_106.Y - b_105.Y;
        d_51 = Math.sqrt((vx_51 * vx_51) + (vy_51 * vy_51));
        const r_52 = d_51 < 1E-09;
        const actual_73 = r_52;
        if ((actual_73 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_73, true, "pt 1 ok");
        }
        else {
            let valueType_68;
            let copyOfStruct_68 = actual_73;
            valueType_68 = bool_type;
            const primitiveTypes_68 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_68;
            if (contains(valueType_68, primitiveTypes_68, {
                Equals: equals,
                GetHashCode: (x_68) => (structuralHash(x_68) | 0),
            })) {
                const arg_73 = toString(true);
                const arg_1_68 = toString(actual_73);
                errorMsg_68 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_73)(arg_1_68)("pt 1 ok");
            }
            else {
                errorMsg_68 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_73)("pt 1 ok");
            }
            throw new Exception(errorMsg_68);
        }
        let d_52;
        const a_108 = item(2, Polyline2D__get_Points(o_3));
        const b_107 = Pt_$ctor_7B00E9A0(8, 8);
        const vx_52 = a_108.X - b_107.X;
        const vy_52 = a_108.Y - b_107.Y;
        d_52 = Math.sqrt((vx_52 * vx_52) + (vy_52 * vy_52));
        const r_53 = d_52 < 1E-09;
        const actual_74 = r_53;
        if ((actual_74 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_74, true, "pt 2 ok");
        }
        else {
            let valueType_69;
            let copyOfStruct_69 = actual_74;
            valueType_69 = bool_type;
            const primitiveTypes_69 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_69;
            if (contains(valueType_69, primitiveTypes_69, {
                Equals: equals,
                GetHashCode: (x_69) => (structuralHash(x_69) | 0),
            })) {
                const arg_74 = toString(true);
                const arg_1_69 = toString(actual_74);
                errorMsg_69 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_74)(arg_1_69)("pt 2 ok");
            }
            else {
                errorMsg_69 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_74)("pt 2 ok");
            }
            throw new Exception(errorMsg_69);
        }
        let d_53;
        const a_110 = item(3, Polyline2D__get_Points(o_3));
        const b_109 = Pt_$ctor_7B00E9A0(2, 8);
        const vx_53 = a_110.X - b_109.X;
        const vy_53 = a_110.Y - b_109.Y;
        d_53 = Math.sqrt((vx_53 * vx_53) + (vy_53 * vy_53));
        const r_54 = d_53 < 1E-09;
        const actual_75 = r_54;
        if ((actual_75 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_75, true, "pt 3 ok");
        }
        else {
            let valueType_70;
            let copyOfStruct_70 = actual_75;
            valueType_70 = bool_type;
            const primitiveTypes_70 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_70;
            if (contains(valueType_70, primitiveTypes_70, {
                Equals: equals,
                GetHashCode: (x_70) => (structuralHash(x_70) | 0),
            })) {
                const arg_75 = toString(true);
                const arg_1_70 = toString(actual_75);
                errorMsg_70 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_75)(arg_1_70)("pt 3 ok");
            }
            else {
                errorMsg_70 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_75)("pt 3 ok");
            }
            throw new Exception(errorMsg_70);
        }
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points looped -2 loop", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        const o_4 = Polyline2D_offset_Z45C468A5(plOpen, -2, true, true, 1, -0.9961946980917455);
        let d_54;
        const a_112 = item(0, Polyline2D__get_Points(o_4));
        const b_111 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_54 = a_112.X - b_111.X;
        const vy_54 = a_112.Y - b_111.Y;
        d_54 = Math.sqrt((vx_54 * vx_54) + (vy_54 * vy_54));
        const r_55 = d_54 < 1E-09;
        const actual_76 = r_55;
        if ((actual_76 === false) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_76, false, "pt 0 ok");
        }
        else {
            let valueType_71;
            let copyOfStruct_71 = actual_76;
            valueType_71 = bool_type;
            const primitiveTypes_71 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_71;
            if (contains(valueType_71, primitiveTypes_71, {
                Equals: equals,
                GetHashCode: (x_71) => (structuralHash(x_71) | 0),
            })) {
                const arg_76 = toString(false);
                const arg_1_71 = toString(actual_76);
                errorMsg_71 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_76)(arg_1_71)("pt 0 ok");
            }
            else {
                errorMsg_71 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(false)(actual_76)("pt 0 ok");
            }
            throw new Exception(errorMsg_71);
        }
        let d_55;
        const a_114 = item(1, Polyline2D__get_Points(o_4));
        const b_113 = Pt_$ctor_7B00E9A0(8, 2);
        const vx_55 = a_114.X - b_113.X;
        const vy_55 = a_114.Y - b_113.Y;
        d_55 = Math.sqrt((vx_55 * vx_55) + (vy_55 * vy_55));
        const r_56 = d_55 < 1E-09;
        const actual_77 = r_56;
        if ((actual_77 === false) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_77, false, "pt 1 ok");
        }
        else {
            let valueType_72;
            let copyOfStruct_72 = actual_77;
            valueType_72 = bool_type;
            const primitiveTypes_72 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_72;
            if (contains(valueType_72, primitiveTypes_72, {
                Equals: equals,
                GetHashCode: (x_72) => (structuralHash(x_72) | 0),
            })) {
                const arg_77 = toString(false);
                const arg_1_72 = toString(actual_77);
                errorMsg_72 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_77)(arg_1_72)("pt 1 ok");
            }
            else {
                errorMsg_72 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(false)(actual_77)("pt 1 ok");
            }
            throw new Exception(errorMsg_72);
        }
        let d_56;
        const a_116 = item(2, Polyline2D__get_Points(o_4));
        const b_115 = Pt_$ctor_7B00E9A0(8, 8);
        const vx_56 = a_116.X - b_115.X;
        const vy_56 = a_116.Y - b_115.Y;
        d_56 = Math.sqrt((vx_56 * vx_56) + (vy_56 * vy_56));
        const r_57 = d_56 < 1E-09;
        const actual_78 = r_57;
        if ((actual_78 === false) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_78, false, "pt 2 ok");
        }
        else {
            let valueType_73;
            let copyOfStruct_73 = actual_78;
            valueType_73 = bool_type;
            const primitiveTypes_73 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_73;
            if (contains(valueType_73, primitiveTypes_73, {
                Equals: equals,
                GetHashCode: (x_73) => (structuralHash(x_73) | 0),
            })) {
                const arg_78 = toString(false);
                const arg_1_73 = toString(actual_78);
                errorMsg_73 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_78)(arg_1_73)("pt 2 ok");
            }
            else {
                errorMsg_73 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(false)(actual_78)("pt 2 ok");
            }
            throw new Exception(errorMsg_73);
        }
        let d_57;
        const a_118 = item(3, Polyline2D__get_Points(o_4));
        const b_117 = Pt_$ctor_7B00E9A0(2, 8);
        const vx_57 = a_118.X - b_117.X;
        const vy_57 = a_118.Y - b_117.Y;
        d_57 = Math.sqrt((vx_57 * vx_57) + (vy_57 * vy_57));
        const r_58 = d_57 < 1E-09;
        const actual_79 = r_58;
        if ((actual_79 === false) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_79, false, "pt 3 ok");
        }
        else {
            let valueType_74;
            let copyOfStruct_74 = actual_79;
            valueType_74 = bool_type;
            const primitiveTypes_74 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_74;
            if (contains(valueType_74, primitiveTypes_74, {
                Equals: equals,
                GetHashCode: (x_74) => (structuralHash(x_74) | 0),
            })) {
                const arg_79 = toString(false);
                const arg_1_74 = toString(actual_79);
                errorMsg_74 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_79)(arg_1_74)("pt 3 ok");
            }
            else {
                errorMsg_74 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(false)(actual_79)("pt 3 ok");
            }
            throw new Exception(errorMsg_74);
        }
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("wrong dist count ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        Expect_throws(() => {
            Polyline2D_offsetVar_357F0A77(plOpen, new Float64Array([0, 0]), false, true, 1, 1, 0.9961946980917455, -0.9961946980917455);
        }, " just two distances");
        Expect_throws(() => {
            Polyline2D_offsetVar_357F0A77(plOpen, new Float64Array([0, 0, 0, 0]), false, true, 1, 1, 0.9961946980917455, -0.9961946980917455);
        }, " four but 3 wanted distances");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points looped, 4 params", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        const o_5 = Polyline2D_offsetVar_357F0A77(plOpen, new Float64Array([4, 2, 2, 2]), true, true, 1, 1, 0.9961946980917455, -0.9961946980917455);
        let d_58;
        const a_120 = item(0, Polyline2D__get_Points(o_5));
        const b_119 = Pt_$ctor_7B00E9A0(2, 4);
        const vx_58 = a_120.X - b_119.X;
        const vy_58 = a_120.Y - b_119.Y;
        d_58 = Math.sqrt((vx_58 * vx_58) + (vy_58 * vy_58));
        const r_59 = d_58 < 1E-09;
        const actual_80 = r_59;
        if ((actual_80 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_80, true, "pt 0 ok");
        }
        else {
            let valueType_75;
            let copyOfStruct_75 = actual_80;
            valueType_75 = bool_type;
            const primitiveTypes_75 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_75;
            if (contains(valueType_75, primitiveTypes_75, {
                Equals: equals,
                GetHashCode: (x_75) => (structuralHash(x_75) | 0),
            })) {
                const arg_80 = toString(true);
                const arg_1_75 = toString(actual_80);
                errorMsg_75 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_80)(arg_1_75)("pt 0 ok");
            }
            else {
                errorMsg_75 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_80)("pt 0 ok");
            }
            throw new Exception(errorMsg_75);
        }
        let d_59;
        const a_122 = item(1, Polyline2D__get_Points(o_5));
        const b_121 = Pt_$ctor_7B00E9A0(8, 4);
        const vx_59 = a_122.X - b_121.X;
        const vy_59 = a_122.Y - b_121.Y;
        d_59 = Math.sqrt((vx_59 * vx_59) + (vy_59 * vy_59));
        const r_60 = d_59 < 1E-09;
        const actual_81 = r_60;
        if ((actual_81 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_81, true, "pt 1 ok");
        }
        else {
            let valueType_76;
            let copyOfStruct_76 = actual_81;
            valueType_76 = bool_type;
            const primitiveTypes_76 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_76;
            if (contains(valueType_76, primitiveTypes_76, {
                Equals: equals,
                GetHashCode: (x_76) => (structuralHash(x_76) | 0),
            })) {
                const arg_81 = toString(true);
                const arg_1_76 = toString(actual_81);
                errorMsg_76 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_81)(arg_1_76)("pt 1 ok");
            }
            else {
                errorMsg_76 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_81)("pt 1 ok");
            }
            throw new Exception(errorMsg_76);
        }
        let d_60;
        const a_124 = item(2, Polyline2D__get_Points(o_5));
        const b_123 = Pt_$ctor_7B00E9A0(8, 8);
        const vx_60 = a_124.X - b_123.X;
        const vy_60 = a_124.Y - b_123.Y;
        d_60 = Math.sqrt((vx_60 * vx_60) + (vy_60 * vy_60));
        const r_61 = d_60 < 1E-09;
        const actual_82 = r_61;
        if ((actual_82 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_82, true, "pt 2 ok");
        }
        else {
            let valueType_77;
            let copyOfStruct_77 = actual_82;
            valueType_77 = bool_type;
            const primitiveTypes_77 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_77;
            if (contains(valueType_77, primitiveTypes_77, {
                Equals: equals,
                GetHashCode: (x_77) => (structuralHash(x_77) | 0),
            })) {
                const arg_82 = toString(true);
                const arg_1_77 = toString(actual_82);
                errorMsg_77 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_82)(arg_1_77)("pt 2 ok");
            }
            else {
                errorMsg_77 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_82)("pt 2 ok");
            }
            throw new Exception(errorMsg_77);
        }
        let d_61;
        const a_126 = item(3, Polyline2D__get_Points(o_5));
        const b_125 = Pt_$ctor_7B00E9A0(2, 8);
        const vx_61 = a_126.X - b_125.X;
        const vy_61 = a_126.Y - b_125.Y;
        d_61 = Math.sqrt((vx_61 * vx_61) + (vy_61 * vy_61));
        const r_62 = d_61 < 1E-09;
        const actual_83 = r_62;
        if ((actual_83 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_83, true, "pt 3 ok");
        }
        else {
            let valueType_78;
            let copyOfStruct_78 = actual_83;
            valueType_78 = bool_type;
            const primitiveTypes_78 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_78;
            if (contains(valueType_78, primitiveTypes_78, {
                Equals: equals,
                GetHashCode: (x_78) => (structuralHash(x_78) | 0),
            })) {
                const arg_83 = toString(true);
                const arg_1_78 = toString(actual_83);
                errorMsg_78 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_83)(arg_1_78)("pt 3 ok");
            }
            else {
                errorMsg_78 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_83)("pt 3 ok");
            }
            throw new Exception(errorMsg_78);
        }
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points open, 3 params", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        const o_6 = Polyline2D_offsetVar_357F0A77(plOpen, new Float64Array([4, 2, 2]), false, true, 1, 1, 0.9961946980917455, -0.9961946980917455);
        let d_62;
        const a_128 = item(0, Polyline2D__get_Points(o_6));
        const b_127 = Pt_$ctor_7B00E9A0(0, 4);
        const vx_62 = a_128.X - b_127.X;
        const vy_62 = a_128.Y - b_127.Y;
        d_62 = Math.sqrt((vx_62 * vx_62) + (vy_62 * vy_62));
        const r_63 = d_62 < 1E-09;
        const actual_84 = r_63;
        if ((actual_84 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_84, true, "pt 0 ok");
        }
        else {
            let valueType_79;
            let copyOfStruct_79 = actual_84;
            valueType_79 = bool_type;
            const primitiveTypes_79 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_79;
            if (contains(valueType_79, primitiveTypes_79, {
                Equals: equals,
                GetHashCode: (x_79) => (structuralHash(x_79) | 0),
            })) {
                const arg_84 = toString(true);
                const arg_1_79 = toString(actual_84);
                errorMsg_79 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_84)(arg_1_79)("pt 0 ok");
            }
            else {
                errorMsg_79 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_84)("pt 0 ok");
            }
            throw new Exception(errorMsg_79);
        }
        let d_63;
        const a_130 = item(1, Polyline2D__get_Points(o_6));
        const b_129 = Pt_$ctor_7B00E9A0(8, 4);
        const vx_63 = a_130.X - b_129.X;
        const vy_63 = a_130.Y - b_129.Y;
        d_63 = Math.sqrt((vx_63 * vx_63) + (vy_63 * vy_63));
        const r_64 = d_63 < 1E-09;
        const actual_85 = r_64;
        if ((actual_85 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_85, true, "pt 1 ok");
        }
        else {
            let valueType_80;
            let copyOfStruct_80 = actual_85;
            valueType_80 = bool_type;
            const primitiveTypes_80 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_80;
            if (contains(valueType_80, primitiveTypes_80, {
                Equals: equals,
                GetHashCode: (x_80) => (structuralHash(x_80) | 0),
            })) {
                const arg_85 = toString(true);
                const arg_1_80 = toString(actual_85);
                errorMsg_80 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_85)(arg_1_80)("pt 1 ok");
            }
            else {
                errorMsg_80 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_85)("pt 1 ok");
            }
            throw new Exception(errorMsg_80);
        }
        let d_64;
        const a_132 = item(2, Polyline2D__get_Points(o_6));
        const b_131 = Pt_$ctor_7B00E9A0(8, 8);
        const vx_64 = a_132.X - b_131.X;
        const vy_64 = a_132.Y - b_131.Y;
        d_64 = Math.sqrt((vx_64 * vx_64) + (vy_64 * vy_64));
        const r_65 = d_64 < 1E-09;
        const actual_86 = r_65;
        if ((actual_86 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_86, true, "pt 2 ok");
        }
        else {
            let valueType_81;
            let copyOfStruct_81 = actual_86;
            valueType_81 = bool_type;
            const primitiveTypes_81 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_81;
            if (contains(valueType_81, primitiveTypes_81, {
                Equals: equals,
                GetHashCode: (x_81) => (structuralHash(x_81) | 0),
            })) {
                const arg_86 = toString(true);
                const arg_1_81 = toString(actual_86);
                errorMsg_81 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_86)(arg_1_81)("pt 2 ok");
            }
            else {
                errorMsg_81 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_86)("pt 2 ok");
            }
            throw new Exception(errorMsg_81);
        }
        let d_65;
        const a_134 = item(3, Polyline2D__get_Points(o_6));
        const b_133 = Pt_$ctor_7B00E9A0(0, 8);
        const vx_65 = a_134.X - b_133.X;
        const vy_65 = a_134.Y - b_133.Y;
        d_65 = Math.sqrt((vx_65 * vx_65) + (vy_65 * vy_65));
        const r_66 = d_65 < 1E-09;
        const actual_87 = r_66;
        if ((actual_87 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_87, true, "pt 3 ok");
        }
        else {
            let valueType_82;
            let copyOfStruct_82 = actual_87;
            valueType_82 = bool_type;
            const primitiveTypes_82 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_82;
            if (contains(valueType_82, primitiveTypes_82, {
                Equals: equals,
                GetHashCode: (x_82) => (structuralHash(x_82) | 0),
            })) {
                const arg_87 = toString(true);
                const arg_1_82 = toString(actual_87);
                errorMsg_82 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_87)(arg_1_82)("pt 3 ok");
            }
            else {
                errorMsg_82 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_87)("pt 3 ok");
            }
            throw new Exception(errorMsg_82);
        }
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("5 points, 4 params", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        const o_7 = Polyline2D_offsetVar_357F0A77(plClosed, new Float64Array([4, 2, 2, 2]), true, true, 1, 1, 0.9961946980917455, -0.9961946980917455);
        let d_66;
        const a_136 = item(0, Polyline2D__get_Points(o_7));
        const b_135 = Pt_$ctor_7B00E9A0(2, 4);
        const vx_66 = a_136.X - b_135.X;
        const vy_66 = a_136.Y - b_135.Y;
        d_66 = Math.sqrt((vx_66 * vx_66) + (vy_66 * vy_66));
        const r_67 = d_66 < 1E-09;
        const actual_88 = r_67;
        if ((actual_88 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_88, true, "pt 0 ok");
        }
        else {
            let valueType_83;
            let copyOfStruct_83 = actual_88;
            valueType_83 = bool_type;
            const primitiveTypes_83 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_83;
            if (contains(valueType_83, primitiveTypes_83, {
                Equals: equals,
                GetHashCode: (x_83) => (structuralHash(x_83) | 0),
            })) {
                const arg_88 = toString(true);
                const arg_1_83 = toString(actual_88);
                errorMsg_83 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_88)(arg_1_83)("pt 0 ok");
            }
            else {
                errorMsg_83 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_88)("pt 0 ok");
            }
            throw new Exception(errorMsg_83);
        }
        let d_67;
        const a_138 = item(1, Polyline2D__get_Points(o_7));
        const b_137 = Pt_$ctor_7B00E9A0(8, 4);
        const vx_67 = a_138.X - b_137.X;
        const vy_67 = a_138.Y - b_137.Y;
        d_67 = Math.sqrt((vx_67 * vx_67) + (vy_67 * vy_67));
        const r_68 = d_67 < 1E-09;
        const actual_89 = r_68;
        if ((actual_89 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_89, true, "pt 1 ok");
        }
        else {
            let valueType_84;
            let copyOfStruct_84 = actual_89;
            valueType_84 = bool_type;
            const primitiveTypes_84 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_84;
            if (contains(valueType_84, primitiveTypes_84, {
                Equals: equals,
                GetHashCode: (x_84) => (structuralHash(x_84) | 0),
            })) {
                const arg_89 = toString(true);
                const arg_1_84 = toString(actual_89);
                errorMsg_84 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_89)(arg_1_84)("pt 1 ok");
            }
            else {
                errorMsg_84 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_89)("pt 1 ok");
            }
            throw new Exception(errorMsg_84);
        }
        let d_68;
        const a_140 = item(2, Polyline2D__get_Points(o_7));
        const b_139 = Pt_$ctor_7B00E9A0(8, 8);
        const vx_68 = a_140.X - b_139.X;
        const vy_68 = a_140.Y - b_139.Y;
        d_68 = Math.sqrt((vx_68 * vx_68) + (vy_68 * vy_68));
        const r_69 = d_68 < 1E-09;
        const actual_90 = r_69;
        if ((actual_90 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_90, true, "pt 2 ok");
        }
        else {
            let valueType_85;
            let copyOfStruct_85 = actual_90;
            valueType_85 = bool_type;
            const primitiveTypes_85 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_85;
            if (contains(valueType_85, primitiveTypes_85, {
                Equals: equals,
                GetHashCode: (x_85) => (structuralHash(x_85) | 0),
            })) {
                const arg_90 = toString(true);
                const arg_1_85 = toString(actual_90);
                errorMsg_85 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_90)(arg_1_85)("pt 2 ok");
            }
            else {
                errorMsg_85 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_90)("pt 2 ok");
            }
            throw new Exception(errorMsg_85);
        }
        let d_69;
        const a_142 = item(3, Polyline2D__get_Points(o_7));
        const b_141 = Pt_$ctor_7B00E9A0(2, 8);
        const vx_69 = a_142.X - b_141.X;
        const vy_69 = a_142.Y - b_141.Y;
        d_69 = Math.sqrt((vx_69 * vx_69) + (vy_69 * vy_69));
        const r_70 = d_69 < 1E-09;
        const actual_91 = r_70;
        if ((actual_91 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_91, true, "pt 3 ok");
        }
        else {
            let valueType_86;
            let copyOfStruct_86 = actual_91;
            valueType_86 = bool_type;
            const primitiveTypes_86 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_86;
            if (contains(valueType_86, primitiveTypes_86, {
                Equals: equals,
                GetHashCode: (x_86) => (structuralHash(x_86) | 0),
            })) {
                const arg_91 = toString(true);
                const arg_1_86 = toString(actual_91);
                errorMsg_86 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_91)(arg_1_86)("pt 3 ok");
            }
            else {
                errorMsg_86 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_91)("pt 3 ok");
            }
            throw new Exception(errorMsg_86);
        }
        let d_70;
        const a_144 = item(4, Polyline2D__get_Points(o_7));
        const b_143 = Pt_$ctor_7B00E9A0(2, 4);
        const vx_70 = a_144.X - b_143.X;
        const vy_70 = a_144.Y - b_143.Y;
        d_70 = Math.sqrt((vx_70 * vx_70) + (vy_70 * vy_70));
        const r_71 = d_70 < 1E-09;
        const actual_92 = r_71;
        if ((actual_92 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_92, true, "pt 4 ok");
        }
        else {
            let valueType_87;
            let copyOfStruct_87 = actual_92;
            valueType_87 = bool_type;
            const primitiveTypes_87 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_87;
            if (contains(valueType_87, primitiveTypes_87, {
                Equals: equals,
                GetHashCode: (x_87) => (structuralHash(x_87) | 0),
            })) {
                const arg_92 = toString(true);
                const arg_1_87 = toString(actual_92);
                errorMsg_87 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_92)(arg_1_87)("pt 4 ok");
            }
            else {
                errorMsg_87 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_92)("pt 4 ok");
            }
            throw new Exception(errorMsg_87);
        }
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("5 points outwards", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        const o_8 = Polyline2D_offset_Z45C468A5(plClosed, -2, false, true, 1, -0.9961946980917455);
        let d_71;
        const a_146 = item(0, Polyline2D__get_Points(o_8));
        const b_145 = Pt_$ctor_7B00E9A0(-2, -2);
        const vx_71 = a_146.X - b_145.X;
        const vy_71 = a_146.Y - b_145.Y;
        d_71 = Math.sqrt((vx_71 * vx_71) + (vy_71 * vy_71));
        const r_72 = d_71 < 1E-09;
        const actual_93 = r_72;
        if ((actual_93 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_93, true, "pt 0 ok");
        }
        else {
            let valueType_88;
            let copyOfStruct_88 = actual_93;
            valueType_88 = bool_type;
            const primitiveTypes_88 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_88;
            if (contains(valueType_88, primitiveTypes_88, {
                Equals: equals,
                GetHashCode: (x_88) => (structuralHash(x_88) | 0),
            })) {
                const arg_93 = toString(true);
                const arg_1_88 = toString(actual_93);
                errorMsg_88 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_93)(arg_1_88)("pt 0 ok");
            }
            else {
                errorMsg_88 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_93)("pt 0 ok");
            }
            throw new Exception(errorMsg_88);
        }
        let d_72;
        const a_148 = item(1, Polyline2D__get_Points(o_8));
        const b_147 = Pt_$ctor_7B00E9A0(12, -2);
        const vx_72 = a_148.X - b_147.X;
        const vy_72 = a_148.Y - b_147.Y;
        d_72 = Math.sqrt((vx_72 * vx_72) + (vy_72 * vy_72));
        const r_73 = d_72 < 1E-09;
        const actual_94 = r_73;
        if ((actual_94 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_94, true, "pt 1 ok");
        }
        else {
            let valueType_89;
            let copyOfStruct_89 = actual_94;
            valueType_89 = bool_type;
            const primitiveTypes_89 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_89;
            if (contains(valueType_89, primitiveTypes_89, {
                Equals: equals,
                GetHashCode: (x_89) => (structuralHash(x_89) | 0),
            })) {
                const arg_94 = toString(true);
                const arg_1_89 = toString(actual_94);
                errorMsg_89 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_94)(arg_1_89)("pt 1 ok");
            }
            else {
                errorMsg_89 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_94)("pt 1 ok");
            }
            throw new Exception(errorMsg_89);
        }
        let d_73;
        const a_150 = item(2, Polyline2D__get_Points(o_8));
        const b_149 = Pt_$ctor_7B00E9A0(12, 12);
        const vx_73 = a_150.X - b_149.X;
        const vy_73 = a_150.Y - b_149.Y;
        d_73 = Math.sqrt((vx_73 * vx_73) + (vy_73 * vy_73));
        const r_74 = d_73 < 1E-09;
        const actual_95 = r_74;
        if ((actual_95 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_95, true, "pt 2 ok");
        }
        else {
            let valueType_90;
            let copyOfStruct_90 = actual_95;
            valueType_90 = bool_type;
            const primitiveTypes_90 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_90;
            if (contains(valueType_90, primitiveTypes_90, {
                Equals: equals,
                GetHashCode: (x_90) => (structuralHash(x_90) | 0),
            })) {
                const arg_95 = toString(true);
                const arg_1_90 = toString(actual_95);
                errorMsg_90 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_95)(arg_1_90)("pt 2 ok");
            }
            else {
                errorMsg_90 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_95)("pt 2 ok");
            }
            throw new Exception(errorMsg_90);
        }
        let d_74;
        const a_152 = item(3, Polyline2D__get_Points(o_8));
        const b_151 = Pt_$ctor_7B00E9A0(-2, 12);
        const vx_74 = a_152.X - b_151.X;
        const vy_74 = a_152.Y - b_151.Y;
        d_74 = Math.sqrt((vx_74 * vx_74) + (vy_74 * vy_74));
        const r_75 = d_74 < 1E-09;
        const actual_96 = r_75;
        if ((actual_96 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_96, true, "pt 3 ok");
        }
        else {
            let valueType_91;
            let copyOfStruct_91 = actual_96;
            valueType_91 = bool_type;
            const primitiveTypes_91 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_91;
            if (contains(valueType_91, primitiveTypes_91, {
                Equals: equals,
                GetHashCode: (x_91) => (structuralHash(x_91) | 0),
            })) {
                const arg_96 = toString(true);
                const arg_1_91 = toString(actual_96);
                errorMsg_91 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_96)(arg_1_91)("pt 3 ok");
            }
            else {
                errorMsg_91 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_96)("pt 3 ok");
            }
            throw new Exception(errorMsg_91);
        }
        let d_75;
        const a_154 = item(4, Polyline2D__get_Points(o_8));
        const b_153 = Pt_$ctor_7B00E9A0(-2, -2);
        const vx_75 = a_154.X - b_153.X;
        const vy_75 = a_154.Y - b_153.Y;
        d_75 = Math.sqrt((vx_75 * vx_75) + (vy_75 * vy_75));
        const r_76 = d_75 < 1E-09;
        const actual_97 = r_76;
        if ((actual_97 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_97, true, "pt 4 ok");
        }
        else {
            let valueType_92;
            let copyOfStruct_92 = actual_97;
            valueType_92 = bool_type;
            const primitiveTypes_92 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_92;
            if (contains(valueType_92, primitiveTypes_92, {
                Equals: equals,
                GetHashCode: (x_92) => (structuralHash(x_92) | 0),
            })) {
                const arg_97 = toString(true);
                const arg_1_92 = toString(actual_97);
                errorMsg_92 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_97)(arg_1_92)("pt 4 ok");
            }
            else {
                errorMsg_92 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_97)("pt 4 ok");
            }
            throw new Exception(errorMsg_92);
        }
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("triangle offset inward", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        const o_9 = Polyline2D_offset_Z45C468A5(plTriangle, 0.5, false, true, 1, -0.9961946980917455);
        Expect_isTrue(Polyline2D__get_PointCount(o_9) === 4)("triangle offset should have 4 points");
        Expect_isTrue(Polyline2D__get_IsClosed(o_9))("triangle offset should be closed");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("L-shape offset", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        const o_10 = Polyline2D_offset_Z45C468A5(plLShape, 0.5, true, true, 1, -0.9961946980917455);
        Expect_isTrue(Polyline2D__get_PointCount(o_10) === 6)("L-shape offset should have 6 points");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("zero offset", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        const o_11 = Polyline2D_offset_Z45C468A5(plClosed, 0, false, true, 1, -0.9961946980917455);
        Expect_isTrue(Polyline2D__get_PointCount(o_11) === Polyline2D__get_PointCount(plClosed))("zero offset preserves shape");
        let d_76;
        const a_156 = item(0, Polyline2D__get_Points(o_11));
        const b_155 = item(0, Polyline2D__get_Points(plClosed));
        const vx_76 = a_156.X - b_155.X;
        const vy_76 = a_156.Y - b_155.Y;
        d_76 = Math.sqrt((vx_76 * vx_76) + (vy_76 * vy_76));
        const r_77 = d_76 < 1E-09;
        const actual_98 = r_77;
        if ((actual_98 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_98, true, "zero offset first point");
        }
        else {
            let valueType_93;
            let copyOfStruct_93 = actual_98;
            valueType_93 = bool_type;
            const primitiveTypes_93 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_93;
            if (contains(valueType_93, primitiveTypes_93, {
                Equals: equals,
                GetHashCode: (x_93) => (structuralHash(x_93) | 0),
            })) {
                const arg_98 = toString(true);
                const arg_1_93 = toString(actual_98);
                errorMsg_93 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_98)(arg_1_93)("zero offset first point");
            }
            else {
                errorMsg_93 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_98)("zero offset first point");
            }
            throw new Exception(errorMsg_93);
        }
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("small offset validation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        const o_12 = Polyline2D_offset_Z45C468A5(plClosed, 0.1, false, true, 1, -0.9961946980917455);
        Expect_isTrue(Polyline2D__get_PointCount(o_12) === Polyline2D__get_PointCount(plClosed))("small offset maintains point count");
        Expect_isTrue(Polyline2D__get_IsClosed(o_12))("small offset maintains closure");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("large offset validation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        const o_13 = Polyline2D_offset_Z45C468A5(plClosed, 4.5, false, true, 1, -0.9961946980917455);
        Expect_isTrue(Polyline2D__get_PointCount(o_13) === Polyline2D__get_PointCount(plClosed))("large offset maintains point count");
        Expect_isTrue(Polyline2D__get_IsClosed(o_13))("large offset maintains closure");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("colinear segments offset", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let plColinear;
        const points_2 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(15, 0), Pt_$ctor_7B00E9A0(20, 0)];
        plColinear = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_2));
        const o_14 = Polyline2D_offset_Z45C468A5(plColinear, 2, false, true, 1, -0.9961946980917455);
        const actual_100 = Polyline2D__get_PointCount(o_14) | 0;
        const expected_183 = Polyline2D__get_PointCount(plColinear) | 0;
        if ((actual_100 === expected_183) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_100, expected_183, "colinear offset point count");
        }
        else {
            let valueType_94;
            let copyOfStruct_94 = actual_100;
            valueType_94 = int32_type;
            const primitiveTypes_94 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_94;
            if (contains(valueType_94, primitiveTypes_94, {
                Equals: equals,
                GetHashCode: (x_94) => (structuralHash(x_94) | 0),
            })) {
                const arg_99 = int32ToString(expected_183);
                const arg_1_94 = int32ToString(actual_100);
                errorMsg_94 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_99)(arg_1_94)("colinear offset point count");
            }
            else {
                errorMsg_94 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_183)(actual_100)("colinear offset point count");
            }
            throw new Exception(errorMsg_94);
        }
        let d_77;
        const a_158 = item(0, Polyline2D__get_Points(o_14));
        const b_157 = Pt_$ctor_7B00E9A0(0, 2);
        const vx_77 = a_158.X - b_157.X;
        const vy_77 = a_158.Y - b_157.Y;
        d_77 = Math.sqrt((vx_77 * vx_77) + (vy_77 * vy_77));
        const r_78 = d_77 < 1E-09;
        const actual_101 = r_78;
        if ((actual_101 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_101, true, "colinear offset pt 0");
        }
        else {
            let valueType_95;
            let copyOfStruct_95 = actual_101;
            valueType_95 = bool_type;
            const primitiveTypes_95 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_95;
            if (contains(valueType_95, primitiveTypes_95, {
                Equals: equals,
                GetHashCode: (x_95) => (structuralHash(x_95) | 0),
            })) {
                const arg_100 = toString(true);
                const arg_1_95 = toString(actual_101);
                errorMsg_95 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_100)(arg_1_95)("colinear offset pt 0");
            }
            else {
                errorMsg_95 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_101)("colinear offset pt 0");
            }
            throw new Exception(errorMsg_95);
        }
        let d_78;
        const a_160 = item(1, Polyline2D__get_Points(o_14));
        const b_159 = Pt_$ctor_7B00E9A0(5, 2);
        const vx_78 = a_160.X - b_159.X;
        const vy_78 = a_160.Y - b_159.Y;
        d_78 = Math.sqrt((vx_78 * vx_78) + (vy_78 * vy_78));
        const r_79 = d_78 < 1E-09;
        const actual_102 = r_79;
        if ((actual_102 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_102, true, "colinear offset pt 1");
        }
        else {
            let valueType_96;
            let copyOfStruct_96 = actual_102;
            valueType_96 = bool_type;
            const primitiveTypes_96 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_96;
            if (contains(valueType_96, primitiveTypes_96, {
                Equals: equals,
                GetHashCode: (x_96) => (structuralHash(x_96) | 0),
            })) {
                const arg_101 = toString(true);
                const arg_1_96 = toString(actual_102);
                errorMsg_96 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_101)(arg_1_96)("colinear offset pt 1");
            }
            else {
                errorMsg_96 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_102)("colinear offset pt 1");
            }
            throw new Exception(errorMsg_96);
        }
        let d_79;
        const a_162 = item(2, Polyline2D__get_Points(o_14));
        const b_161 = Pt_$ctor_7B00E9A0(10, 2);
        const vx_79 = a_162.X - b_161.X;
        const vy_79 = a_162.Y - b_161.Y;
        d_79 = Math.sqrt((vx_79 * vx_79) + (vy_79 * vy_79));
        const r_80 = d_79 < 1E-09;
        const actual_103 = r_80;
        if ((actual_103 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_103, true, "colinear offset pt 2");
        }
        else {
            let valueType_97;
            let copyOfStruct_97 = actual_103;
            valueType_97 = bool_type;
            const primitiveTypes_97 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_97;
            if (contains(valueType_97, primitiveTypes_97, {
                Equals: equals,
                GetHashCode: (x_97) => (structuralHash(x_97) | 0),
            })) {
                const arg_102 = toString(true);
                const arg_1_97 = toString(actual_103);
                errorMsg_97 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_102)(arg_1_97)("colinear offset pt 2");
            }
            else {
                errorMsg_97 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_103)("colinear offset pt 2");
            }
            throw new Exception(errorMsg_97);
        }
        let d_80;
        const a_164 = item(3, Polyline2D__get_Points(o_14));
        const b_163 = Pt_$ctor_7B00E9A0(15, 2);
        const vx_80 = a_164.X - b_163.X;
        const vy_80 = a_164.Y - b_163.Y;
        d_80 = Math.sqrt((vx_80 * vx_80) + (vy_80 * vy_80));
        const r_81 = d_80 < 1E-09;
        const actual_104 = r_81;
        if ((actual_104 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_104, true, "colinear offset pt 3");
        }
        else {
            let valueType_98;
            let copyOfStruct_98 = actual_104;
            valueType_98 = bool_type;
            const primitiveTypes_98 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_98;
            if (contains(valueType_98, primitiveTypes_98, {
                Equals: equals,
                GetHashCode: (x_98) => (structuralHash(x_98) | 0),
            })) {
                const arg_103 = toString(true);
                const arg_1_98 = toString(actual_104);
                errorMsg_98 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_103)(arg_1_98)("colinear offset pt 3");
            }
            else {
                errorMsg_98 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_104)("colinear offset pt 3");
            }
            throw new Exception(errorMsg_98);
        }
        let d_81;
        const a_166 = item(4, Polyline2D__get_Points(o_14));
        const b_165 = Pt_$ctor_7B00E9A0(20, 2);
        const vx_81 = a_166.X - b_165.X;
        const vy_81 = a_166.Y - b_165.Y;
        d_81 = Math.sqrt((vx_81 * vx_81) + (vy_81 * vy_81));
        const r_82 = d_81 < 1E-09;
        const actual_105 = r_82;
        if ((actual_105 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_105, true, "colinear offset pt 4");
        }
        else {
            let valueType_99;
            let copyOfStruct_99 = actual_105;
            valueType_99 = bool_type;
            const primitiveTypes_99 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_99;
            if (contains(valueType_99, primitiveTypes_99, {
                Equals: equals,
                GetHashCode: (x_99) => (structuralHash(x_99) | 0),
            })) {
                const arg_104 = toString(true);
                const arg_1_99 = toString(actual_105);
                errorMsg_99 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_104)(arg_1_99)("colinear offset pt 4");
            }
            else {
                errorMsg_99 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_105)("colinear offset pt 4");
            }
            throw new Exception(errorMsg_99);
        }
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("L-shape with colinear extension", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        let plLExtended;
        const points_3 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 5), Pt_$ctor_7B00E9A0(10, 10)];
        plLExtended = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_3));
        const o_15 = Polyline2D_offset_Z45C468A5(plLExtended, 1, false, true, 1, -0.9961946980917455);
        const actual_107 = Polyline2D__get_Points(o_15).length | 0;
        const expected_195 = Polyline2D__get_Points(plLExtended).length | 0;
        if ((actual_107 === expected_195) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_107, expected_195, "L-extended offset point count");
        }
        else {
            let valueType_100;
            let copyOfStruct_100 = actual_107;
            valueType_100 = int32_type;
            const primitiveTypes_100 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_100;
            if (contains(valueType_100, primitiveTypes_100, {
                Equals: equals,
                GetHashCode: (x_100) => (structuralHash(x_100) | 0),
            })) {
                const arg_105 = int32ToString(expected_195);
                const arg_1_100 = int32ToString(actual_107);
                errorMsg_100 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_105)(arg_1_100)("L-extended offset point count");
            }
            else {
                errorMsg_100 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_195)(actual_107)("L-extended offset point count");
            }
            throw new Exception(errorMsg_100);
        }
        Expect_isTrue(Polyline2D__get_Points(o_15).length === 5)("L-extended offset maintains structure");
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rect with colinear segments offset", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let plOpen_1;
        const points_4 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
        plOpen_1 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_4));
        const o_16 = Polyline2D_offset_Z45C468A5(plOpen_1, 2, false, true, 1, -0.9961946980917455);
        const actual_109 = Polyline2D__get_PointCount(o_16) | 0;
        const expected_197 = Polyline2D__get_PointCount(plOpen_1) | 0;
        if ((actual_109 === expected_197) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_109, expected_197, "point count");
        }
        else {
            let valueType_101;
            let copyOfStruct_101 = actual_109;
            valueType_101 = int32_type;
            const primitiveTypes_101 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_101;
            if (contains(valueType_101, primitiveTypes_101, {
                Equals: equals,
                GetHashCode: (x_101) => (structuralHash(x_101) | 0),
            })) {
                const arg_106 = int32ToString(expected_197);
                const arg_1_101 = int32ToString(actual_109);
                errorMsg_101 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_106)(arg_1_101)("point count");
            }
            else {
                errorMsg_101 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_197)(actual_109)("point count");
            }
            throw new Exception(errorMsg_101);
        }
        let d_82;
        const a_168 = item(0, Polyline2D__get_Points(o_16));
        const b_167 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_82 = a_168.X - b_167.X;
        const vy_82 = a_168.Y - b_167.Y;
        d_82 = Math.sqrt((vx_82 * vx_82) + (vy_82 * vy_82));
        const r_83 = d_82 < 1E-09;
        const actual_110 = r_83;
        if ((actual_110 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_110, true, "colinear different offsets pt 0");
        }
        else {
            let valueType_102;
            let copyOfStruct_102 = actual_110;
            valueType_102 = bool_type;
            const primitiveTypes_102 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_102;
            if (contains(valueType_102, primitiveTypes_102, {
                Equals: equals,
                GetHashCode: (x_102) => (structuralHash(x_102) | 0),
            })) {
                const arg_107 = toString(true);
                const arg_1_102 = toString(actual_110);
                errorMsg_102 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_107)(arg_1_102)("colinear different offsets pt 0");
            }
            else {
                errorMsg_102 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_110)("colinear different offsets pt 0");
            }
            throw new Exception(errorMsg_102);
        }
        let d_83;
        const a_170 = item(1, Polyline2D__get_Points(o_16));
        const b_169 = Pt_$ctor_7B00E9A0(5, 2);
        const vx_83 = a_170.X - b_169.X;
        const vy_83 = a_170.Y - b_169.Y;
        d_83 = Math.sqrt((vx_83 * vx_83) + (vy_83 * vy_83));
        const r_84 = d_83 < 1E-09;
        const actual_111 = r_84;
        if ((actual_111 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_111, true, "colinear different offsets pt 1");
        }
        else {
            let valueType_103;
            let copyOfStruct_103 = actual_111;
            valueType_103 = bool_type;
            const primitiveTypes_103 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_103;
            if (contains(valueType_103, primitiveTypes_103, {
                Equals: equals,
                GetHashCode: (x_103) => (structuralHash(x_103) | 0),
            })) {
                const arg_108 = toString(true);
                const arg_1_103 = toString(actual_111);
                errorMsg_103 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_108)(arg_1_103)("colinear different offsets pt 1");
            }
            else {
                errorMsg_103 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_111)("colinear different offsets pt 1");
            }
            throw new Exception(errorMsg_103);
        }
        let d_84;
        const a_172 = item(2, Polyline2D__get_Points(o_16));
        const b_171 = Pt_$ctor_7B00E9A0(8, 2);
        const vx_84 = a_172.X - b_171.X;
        const vy_84 = a_172.Y - b_171.Y;
        d_84 = Math.sqrt((vx_84 * vx_84) + (vy_84 * vy_84));
        const r_85 = d_84 < 1E-09;
        const actual_112 = r_85;
        if ((actual_112 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_112, true, "colinear different offsets pt 2");
        }
        else {
            let valueType_104;
            let copyOfStruct_104 = actual_112;
            valueType_104 = bool_type;
            const primitiveTypes_104 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_104;
            if (contains(valueType_104, primitiveTypes_104, {
                Equals: equals,
                GetHashCode: (x_104) => (structuralHash(x_104) | 0),
            })) {
                const arg_109 = toString(true);
                const arg_1_104 = toString(actual_112);
                errorMsg_104 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_109)(arg_1_104)("colinear different offsets pt 2");
            }
            else {
                errorMsg_104 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_112)("colinear different offsets pt 2");
            }
            throw new Exception(errorMsg_104);
        }
        let d_85;
        const a_174 = item(3, Polyline2D__get_Points(o_16));
        const b_173 = Pt_$ctor_7B00E9A0(8, 8);
        const vx_85 = a_174.X - b_173.X;
        const vy_85 = a_174.Y - b_173.Y;
        d_85 = Math.sqrt((vx_85 * vx_85) + (vy_85 * vy_85));
        const r_86 = d_85 < 1E-09;
        const actual_113 = r_86;
        if ((actual_113 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_113, true, "colinear different offsets pt 3");
        }
        else {
            let valueType_105;
            let copyOfStruct_105 = actual_113;
            valueType_105 = bool_type;
            const primitiveTypes_105 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_105;
            if (contains(valueType_105, primitiveTypes_105, {
                Equals: equals,
                GetHashCode: (x_105) => (structuralHash(x_105) | 0),
            })) {
                const arg_110 = toString(true);
                const arg_1_105 = toString(actual_113);
                errorMsg_105 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_110)(arg_1_105)("colinear different offsets pt 3");
            }
            else {
                errorMsg_105 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_113)("colinear different offsets pt 3");
            }
            throw new Exception(errorMsg_105);
        }
        let d_86;
        const a_176 = item(4, Polyline2D__get_Points(o_16));
        const b_175 = Pt_$ctor_7B00E9A0(2, 8);
        const vx_86 = a_176.X - b_175.X;
        const vy_86 = a_176.Y - b_175.Y;
        d_86 = Math.sqrt((vx_86 * vx_86) + (vy_86 * vy_86));
        const r_87 = d_86 < 1E-09;
        const actual_114 = r_87;
        if ((actual_114 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_114, true, "colinear different offsets pt 4");
        }
        else {
            let valueType_106;
            let copyOfStruct_106 = actual_114;
            valueType_106 = bool_type;
            const primitiveTypes_106 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_106;
            if (contains(valueType_106, primitiveTypes_106, {
                Equals: equals,
                GetHashCode: (x_106) => (structuralHash(x_106) | 0),
            })) {
                const arg_111 = toString(true);
                const arg_1_106 = toString(actual_114);
                errorMsg_106 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_111)(arg_1_106)("colinear different offsets pt 4");
            }
            else {
                errorMsg_106 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_114)("colinear different offsets pt 4");
            }
            throw new Exception(errorMsg_106);
        }
        let d_87;
        const a_178 = item(5, Polyline2D__get_Points(o_16));
        const b_177 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_87 = a_178.X - b_177.X;
        const vy_87 = a_178.Y - b_177.Y;
        d_87 = Math.sqrt((vx_87 * vx_87) + (vy_87 * vy_87));
        const r_88 = d_87 < 1E-09;
        const actual_115 = r_88;
        if ((actual_115 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_115, true, "colinear different offsets pt 5");
        }
        else {
            let valueType_107;
            let copyOfStruct_107 = actual_115;
            valueType_107 = bool_type;
            const primitiveTypes_107 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_107;
            if (contains(valueType_107, primitiveTypes_107, {
                Equals: equals,
                GetHashCode: (x_107) => (structuralHash(x_107) | 0),
            })) {
                const arg_112 = toString(true);
                const arg_1_107 = toString(actual_115);
                errorMsg_107 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_112)(arg_1_107)("colinear different offsets pt 5");
            }
            else {
                errorMsg_107 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_115)("colinear different offsets pt 5");
            }
            throw new Exception(errorMsg_107);
        }
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})()]))]));

export const plWithDuplicates = (() => {
    const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const plConsecutiveDuplicates = (() => {
    const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 5), Pt_$ctor_7B00E9A0(5, 5), Pt_$ctor_7B00E9A0(5, 5), Pt_$ctor_7B00E9A0(10, 10)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const plAllDuplicates = (() => {
    const points = [Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 1)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const plStartEndDuplicate = (() => {
    const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(5, 5), Pt_$ctor_7B00E9A0(0, 5), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 0)];
    return Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
})();

export const testsDup = Test_testList("Polyline2D Extended Tests", ofArray([Test_testList("Duplicate Points Handling", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("polyline with duplicate points - basic properties", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const actual = Polyline2D__get_PointCount(plWithDuplicates) | 0;
        if ((actual === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 8, "should preserve all points including duplicates");
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
                const arg = int32ToString(8);
                const arg_1 = int32ToString(actual);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("should preserve all points including duplicates");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual)("should preserve all points including duplicates");
            }
            throw new Exception(errorMsg);
        }
        Expect_isTrue(Polyline2D__get_IsClosed(plWithDuplicates))("should be closed when start/end are same");
        let d;
        const a_2 = Polyline2D__get_Start(plWithDuplicates);
        const b_2 = Pt_$ctor_7B00E9A0(0, 0);
        const vx = a_2.X - b_2.X;
        const vy = a_2.Y - b_2.Y;
        d = Math.sqrt((vx * vx) + (vy * vy));
        const r = d < 1E-09;
        const actual_1 = r;
        if ((actual_1 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, true, "start point");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_1;
            valueType_1 = bool_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg_6 = toString(true);
                const arg_1_1 = toString(actual_1);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("start point");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_1)("start point");
            }
            throw new Exception(errorMsg_1);
        }
        let d_1;
        const a_4 = Polyline2D__get_End(plWithDuplicates);
        const b_4 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_1 = a_4.X - b_4.X;
        const vy_1 = a_4.Y - b_4.Y;
        d_1 = Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1));
        const r_1 = d_1 < 1E-09;
        const actual_2 = r_1;
        if ((actual_2 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, true, "end point");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_2;
            valueType_2 = bool_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_7 = toString(true);
                const arg_1_2 = toString(actual_2);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("end point");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_2)("end point");
            }
            throw new Exception(errorMsg_2);
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("consecutive duplicate points - segment count", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const actual_3 = Polyline2D__get_SegmentCount(plConsecutiveDuplicates) | 0;
        if ((actual_3 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 4, "should count all segments including zero-length");
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
                const arg_8 = int32ToString(4);
                const arg_1_3 = int32ToString(actual_3);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("should count all segments including zero-length");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_3)("should count all segments including zero-length");
            }
            throw new Exception(errorMsg_3);
        }
        const seg1 = Polyline2D__GetSegment_Z524259A4(plConsecutiveDuplicates, 1);
        const seg2 = Polyline2D__GetSegment_Z524259A4(plConsecutiveDuplicates, 2);
        let d_2;
        let a_6;
        const ln = seg1;
        a_6 = Pt_$ctor_7B00E9A0_1(ln.FromX, ln.FromY);
        const b_6 = Pt_$ctor_7B00E9A0(5, 5);
        const vx_2 = a_6.X - b_6.X;
        const vy_2 = a_6.Y - b_6.Y;
        d_2 = Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2));
        const r_2 = d_2 < 1E-09;
        const actual_4 = r_2;
        if ((actual_4 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, true, "zero length segment start");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_4;
            valueType_4 = bool_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_9 = toString(true);
                const arg_1_4 = toString(actual_4);
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("zero length segment start");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_4)("zero length segment start");
            }
            throw new Exception(errorMsg_4);
        }
        let d_3;
        let a_8;
        const ln_1 = seg1;
        a_8 = Pt_$ctor_7B00E9A0_1(ln_1.ToX, ln_1.ToY);
        const b_8 = Pt_$ctor_7B00E9A0(5, 5);
        const vx_3 = a_8.X - b_8.X;
        const vy_3 = a_8.Y - b_8.Y;
        d_3 = Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3));
        const r_3 = d_3 < 1E-09;
        const actual_5 = r_3;
        if ((actual_5 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, true, "zero length segment end");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_5;
            valueType_5 = bool_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_10 = toString(true);
                const arg_1_5 = toString(actual_5);
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("zero length segment end");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_5)("zero length segment end");
            }
            throw new Exception(errorMsg_5);
        }
        let d_4;
        let a_10;
        const ln_2 = seg2;
        a_10 = Pt_$ctor_7B00E9A0_1(ln_2.FromX, ln_2.FromY);
        const b_10 = Pt_$ctor_7B00E9A0(5, 5);
        const vx_4 = a_10.X - b_10.X;
        const vy_4 = a_10.Y - b_10.Y;
        d_4 = Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4));
        const r_4 = d_4 < 1E-09;
        const actual_6 = r_4;
        if ((actual_6 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, true, "consecutive zero length segment");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_6;
            valueType_6 = bool_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg_11 = toString(true);
                const arg_1_6 = toString(actual_6);
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("consecutive zero length segment");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_6)("consecutive zero length segment");
            }
            throw new Exception(errorMsg_6);
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("all duplicate points polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const actual_7 = Polyline2D__get_PointCount(plAllDuplicates) | 0;
        if ((actual_7 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 4, "should preserve all duplicate points");
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
                const arg_12 = int32ToString(4);
                const arg_1_7 = int32ToString(actual_7);
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("should preserve all duplicate points");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_7)("should preserve all duplicate points");
            }
            throw new Exception(errorMsg_7);
        }
        const actual_8 = Polyline2D__get_SegmentCount(plAllDuplicates) | 0;
        if ((actual_8 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 3, "should have segments even if zero length");
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
                const arg_13 = int32ToString(3);
                const arg_1_8 = int32ToString(actual_8);
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("should have segments even if zero length");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_8)("should have segments even if zero length");
            }
            throw new Exception(errorMsg_8);
        }
        Expect_isTrue(Math.abs(Polyline2D__get_Length(plAllDuplicates)) < 1E-09)("length should be zero");
        let d_5;
        const a_12 = item(2, Polyline2D__get_Points(plAllDuplicates));
        const b_12 = Polyline2D__get_Start(plAllDuplicates);
        const vx_5 = a_12.X - b_12.X;
        const vy_5 = a_12.Y - b_12.Y;
        d_5 = Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5));
        const r_5 = d_5 < 1E-09;
        const actual_9 = r_5;
        if ((actual_9 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, true, "all points same as start");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_9;
            valueType_9 = bool_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            })) {
                const arg_14 = toString(true);
                const arg_1_9 = toString(actual_9);
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("all points same as start");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_9)("all points same as start");
            }
            throw new Exception(errorMsg_9);
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points at start and end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        Expect_isTrue(Polyline2D__get_IsClosed(plStartEndDuplicate))("should be closed with duplicate start/end");
        const actual_10 = Polyline2D__get_PointCount(plStartEndDuplicate) | 0;
        if ((actual_10 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 6, "should count all points including duplicates");
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
                const arg_15 = int32ToString(6);
                const arg_1_10 = int32ToString(actual_10);
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("should count all points including duplicates");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_10)("should count all points including duplicates");
            }
            throw new Exception(errorMsg_10);
        }
        let d_6;
        const a_14 = item(0, Polyline2D__get_Points(plStartEndDuplicate));
        const b_14 = item(5, Polyline2D__get_Points(plStartEndDuplicate));
        const vx_6 = a_14.X - b_14.X;
        const vy_6 = a_14.Y - b_14.Y;
        d_6 = Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6));
        const r_6 = d_6 < 1E-09;
        const actual_11 = r_6;
        if ((actual_11 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, true, "first and last points equal");
        }
        else {
            let valueType_11;
            let copyOfStruct_11 = actual_11;
            valueType_11 = bool_type;
            const primitiveTypes_11 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_11;
            if (contains(valueType_11, primitiveTypes_11, {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            })) {
                const arg_16 = toString(true);
                const arg_1_11 = toString(actual_11);
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("first and last points equal");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_11)("first and last points equal");
            }
            throw new Exception(errorMsg_11);
        }
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points - closest point operations", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const testPt = Pt_$ctor_7B00E9A0(2, 2);
        const closest = Polyline2D__ClosestPoint_6ADE94FD(plConsecutiveDuplicates, testPt);
        const param = Polyline2D__ClosestParameter_6ADE94FD(plConsecutiveDuplicates, testPt);
        const distance = Polyline2D__DistanceTo_6ADE94FD(plConsecutiveDuplicates, testPt);
        Expect_isTrue((closest.X >= 0) && (closest.Y >= 0))("closest point with duplicates");
        Expect_isTrue((param >= 0) && (param <= Polyline2D__get_SegmentCount(plConsecutiveDuplicates)))("closest parameter with duplicates");
        Expect_isTrue(distance >= 0)("distance with duplicates");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points - evaluation at parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const pt0 = Polyline2D__EvaluateAt_5E38073B(plConsecutiveDuplicates, 0);
        const pt1 = Polyline2D__EvaluateAt_5E38073B(plConsecutiveDuplicates, 1);
        const ptMid = Polyline2D__EvaluateAt_5E38073B(plConsecutiveDuplicates, 1.5);
        let d_7;
        const a_16 = pt0;
        const b_16 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_7 = a_16.X - b_16.X;
        const vy_7 = a_16.Y - b_16.Y;
        d_7 = Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7));
        const r_7 = d_7 < 1E-09;
        const actual_12 = r_7;
        if ((actual_12 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, true, "evaluate at start with duplicates");
        }
        else {
            let valueType_12;
            let copyOfStruct_12 = actual_12;
            valueType_12 = bool_type;
            const primitiveTypes_12 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_12;
            if (contains(valueType_12, primitiveTypes_12, {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            })) {
                const arg_17 = toString(true);
                const arg_1_12 = toString(actual_12);
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("evaluate at start with duplicates");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_12)("evaluate at start with duplicates");
            }
            throw new Exception(errorMsg_12);
        }
        let d_8;
        const a_18 = pt1;
        const b_18 = Pt_$ctor_7B00E9A0(5, 5);
        const vx_8 = a_18.X - b_18.X;
        const vy_8 = a_18.Y - b_18.Y;
        d_8 = Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8));
        const r_8 = d_8 < 1E-09;
        const actual_13 = r_8;
        if ((actual_13 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, true, "evaluate at segment 1 with duplicates");
        }
        else {
            let valueType_13;
            let copyOfStruct_13 = actual_13;
            valueType_13 = bool_type;
            const primitiveTypes_13 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_13;
            if (contains(valueType_13, primitiveTypes_13, {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            })) {
                const arg_18 = toString(true);
                const arg_1_13 = toString(actual_13);
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("evaluate at segment 1 with duplicates");
            }
            else {
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_13)("evaluate at segment 1 with duplicates");
            }
            throw new Exception(errorMsg_13);
        }
        let d_9;
        const a_20 = ptMid;
        const b_20 = Pt_$ctor_7B00E9A0(5, 5);
        const vx_9 = a_20.X - b_20.X;
        const vy_9 = a_20.Y - b_20.Y;
        d_9 = Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9));
        const r_9 = d_9 < 1E-09;
        const actual_14 = r_9;
        if ((actual_14 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, true, "evaluate in duplicate segment range");
        }
        else {
            let valueType_14;
            let copyOfStruct_14 = actual_14;
            valueType_14 = bool_type;
            const primitiveTypes_14 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_14;
            if (contains(valueType_14, primitiveTypes_14, {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            })) {
                const arg_19 = toString(true);
                const arg_1_14 = toString(actual_14);
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("evaluate in duplicate segment range");
            }
            else {
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_14)("evaluate in duplicate segment range");
            }
            throw new Exception(errorMsg_14);
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points - winding number", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        if (Polyline2D__get_IsClosed(plWithDuplicates)) {
            const insidePoint = Pt_$ctor_7B00E9A0(5, 5);
            const winding = Polyline2D__WindingNumber_6ADE94FD(plWithDuplicates, insidePoint) | 0;
            Expect_isTrue(winding !== 0)("winding number with duplicates should work");
            Expect_isTrue(Polyline2D__Contains_6ADE94FD(plWithDuplicates, insidePoint))("contains with duplicates");
            Test_TestCaseBuilder__Zero(builder$0040_6);
        }
        else {
            Test_TestCaseBuilder__Zero(builder$0040_6);
        }
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points - transformations", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const scaled = Polyline2D__Scale_5E38073B(plWithDuplicates, 2);
        const actual_16 = Polyline2D__get_PointCount(scaled) | 0;
        const expected_26 = Polyline2D__get_PointCount(plWithDuplicates) | 0;
        if ((actual_16 === expected_26) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, expected_26, "scaled polyline preserves duplicate count");
        }
        else {
            let valueType_15;
            let copyOfStruct_15 = actual_16;
            valueType_15 = int32_type;
            const primitiveTypes_15 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_15;
            if (contains(valueType_15, primitiveTypes_15, {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            })) {
                const arg_20 = int32ToString(expected_26);
                const arg_1_15 = int32ToString(actual_16);
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("scaled polyline preserves duplicate count");
            }
            else {
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_26)(actual_16)("scaled polyline preserves duplicate count");
            }
            throw new Exception(errorMsg_15);
        }
        let d_10;
        const a_22 = item(0, Polyline2D__get_Points(scaled));
        const b_22 = item(1, Polyline2D__get_Points(scaled));
        const vx_10 = a_22.X - b_22.X;
        const vy_10 = a_22.Y - b_22.Y;
        d_10 = Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10));
        const r_10 = d_10 < 1E-09;
        const actual_17 = r_10;
        if ((actual_17 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, true, "scaled duplicate points remain duplicates");
        }
        else {
            let valueType_16;
            let copyOfStruct_16 = actual_17;
            valueType_16 = bool_type;
            const primitiveTypes_16 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_16;
            if (contains(valueType_16, primitiveTypes_16, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_21 = toString(true);
                const arg_1_16 = toString(actual_17);
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("scaled duplicate points remain duplicates");
            }
            else {
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_17)("scaled duplicate points remain duplicates");
            }
            throw new Exception(errorMsg_16);
        }
        const translated = Polyline2D_translate(Vc_$ctor_7B00E9A0(1, 1), plAllDuplicates);
        let d_11;
        const a_24 = item(0, Polyline2D__get_Points(translated));
        const b_24 = item(3, Polyline2D__get_Points(translated));
        const vx_11 = a_24.X - b_24.X;
        const vy_11 = a_24.Y - b_24.Y;
        d_11 = Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11));
        const r_11 = d_11 < 1E-09;
        const actual_18 = r_11;
        if ((actual_18 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, true, "translated all duplicates remain same");
        }
        else {
            let valueType_17;
            let copyOfStruct_17 = actual_18;
            valueType_17 = bool_type;
            const primitiveTypes_17 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_17;
            if (contains(valueType_17, primitiveTypes_17, {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            })) {
                const arg_22 = toString(true);
                const arg_1_17 = toString(actual_18);
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("translated all duplicates remain same");
            }
            else {
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_18)("translated all duplicates remain same");
            }
            throw new Exception(errorMsg_17);
        }
        let d_12;
        const a_26 = item(0, Polyline2D__get_Points(translated));
        const b_26 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_12 = a_26.X - b_26.X;
        const vy_12 = a_26.Y - b_26.Y;
        d_12 = Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12));
        const r_12 = d_12 < 1E-09;
        const actual_19 = r_12;
        if ((actual_19 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, true, "translated duplicate points moved correctly");
        }
        else {
            let valueType_18;
            let copyOfStruct_18 = actual_19;
            valueType_18 = bool_type;
            const primitiveTypes_18 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_18;
            if (contains(valueType_18, primitiveTypes_18, {
                Equals: equals,
                GetHashCode: (x_18) => (structuralHash(x_18) | 0),
            })) {
                const arg_23 = toString(true);
                const arg_1_18 = toString(actual_19);
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("translated duplicate points moved correctly");
            }
            else {
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_19)("translated duplicate points moved correctly");
            }
            throw new Exception(errorMsg_18);
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points - sub polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const sub = Polyline2D_subPolyline(0.5, 2.5, plConsecutiveDuplicates);
        Expect_isTrue(Polyline2D__get_PointCount(sub) > 0)("sub polyline with duplicates should work");
        const subAcrossDuplicates = Polyline2D_subPolyline(1, 3, plConsecutiveDuplicates);
        Expect_isTrue(Polyline2D__get_PointCount(subAcrossDuplicates) > 0)("sub polyline across duplicates");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points - edge cases", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let plTwoDuplicates;
        const points = [Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 1)];
        plTwoDuplicates = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
        const actual_20 = Polyline2D__get_PointCount(plTwoDuplicates) | 0;
        if ((actual_20 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, 2, "two duplicate points");
        }
        else {
            let valueType_19;
            let copyOfStruct_19 = actual_20;
            valueType_19 = int32_type;
            const primitiveTypes_19 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_19;
            if (contains(valueType_19, primitiveTypes_19, {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            })) {
                const arg_24 = int32ToString(2);
                const arg_1_19 = int32ToString(actual_20);
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("two duplicate points");
            }
            else {
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_20)("two duplicate points");
            }
            throw new Exception(errorMsg_19);
        }
        const actual_21 = Polyline2D__get_SegmentCount(plTwoDuplicates) | 0;
        if ((actual_21 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 1, "one zero-length segment");
        }
        else {
            let valueType_20;
            let copyOfStruct_20 = actual_21;
            valueType_20 = int32_type;
            const primitiveTypes_20 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_20;
            if (contains(valueType_20, primitiveTypes_20, {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            })) {
                const arg_25 = int32ToString(1);
                const arg_1_20 = int32ToString(actual_21);
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("one zero-length segment");
            }
            else {
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_21)("one zero-length segment");
            }
            throw new Exception(errorMsg_20);
        }
        Expect_isTrue(Math.abs(Polyline2D__get_Length(plTwoDuplicates)) < 1E-09)("length is zero");
        let plManyDuplicates;
        const points_1 = replicate(10, Pt_$ctor_7B00E9A0(3, 3));
        plManyDuplicates = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_1));
        const actual_22 = Polyline2D__get_PointCount(plManyDuplicates) | 0;
        if ((actual_22 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_22, 10, "many duplicate points preserved");
        }
        else {
            let valueType_21;
            let copyOfStruct_21 = actual_22;
            valueType_21 = int32_type;
            const primitiveTypes_21 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_21;
            if (contains(valueType_21, primitiveTypes_21, {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            })) {
                const arg_26 = int32ToString(10);
                const arg_1_21 = int32ToString(actual_22);
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("many duplicate points preserved");
            }
            else {
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_22)("many duplicate points preserved");
            }
            throw new Exception(errorMsg_21);
        }
        let d_13;
        const a_30 = item(0, Polyline2D__get_Points(plManyDuplicates));
        const b_30 = item(9, Polyline2D__get_Points(plManyDuplicates));
        const vx_13 = a_30.X - b_30.X;
        const vy_13 = a_30.Y - b_30.Y;
        d_13 = Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13));
        const r_13 = d_13 < 1E-09;
        const actual_23 = r_13;
        if ((actual_23 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, true, "all points are same");
        }
        else {
            let valueType_22;
            let copyOfStruct_22 = actual_23;
            valueType_22 = bool_type;
            const primitiveTypes_22 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_22;
            if (contains(valueType_22, primitiveTypes_22, {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            })) {
                const arg_27 = toString(true);
                const arg_1_22 = toString(actual_23);
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("all points are same");
            }
            else {
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_23)("all points are same");
            }
            throw new Exception(errorMsg_22);
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("nearly duplicate points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let plNearlyDup;
        const points_2 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 1E-12), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 1E-12), Pt_$ctor_7B00E9A0(0, 0)];
        plNearlyDup = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_2));
        const actual_24 = Polyline2D__get_PointCount(plNearlyDup) | 0;
        if ((actual_24 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, 5, "nearly duplicate points preserved");
        }
        else {
            let valueType_23;
            let copyOfStruct_23 = actual_24;
            valueType_23 = int32_type;
            const primitiveTypes_23 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_23;
            if (contains(valueType_23, primitiveTypes_23, {
                Equals: equals,
                GetHashCode: (x_23) => (structuralHash(x_23) | 0),
            })) {
                const arg_28 = int32ToString(5);
                const arg_1_23 = int32ToString(actual_24);
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("nearly duplicate points preserved");
            }
            else {
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_24)("nearly duplicate points preserved");
            }
            throw new Exception(errorMsg_23);
        }
        Expect_isTrue(Polyline2D__get_IsClosed(plNearlyDup))("should be closed");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points with different operations", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const reversed = Polyline2D__Reverse(plWithDuplicates);
        const actual_26 = Polyline2D__get_PointCount(reversed) | 0;
        const expected_40 = Polyline2D__get_PointCount(plWithDuplicates) | 0;
        if ((actual_26 === expected_40) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, expected_40, "reversed preserves duplicates");
        }
        else {
            let valueType_24;
            let copyOfStruct_24 = actual_26;
            valueType_24 = int32_type;
            const primitiveTypes_24 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_24;
            if (contains(valueType_24, primitiveTypes_24, {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            })) {
                const arg_29 = int32ToString(expected_40);
                const arg_1_24 = int32ToString(actual_26);
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("reversed preserves duplicates");
            }
            else {
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_40)(actual_26)("reversed preserves duplicates");
            }
            throw new Exception(errorMsg_24);
        }
        const cloned = Polyline2D__Clone(plWithDuplicates);
        const actual_28 = Polyline2D__get_PointCount(cloned) | 0;
        const expected_42 = Polyline2D__get_PointCount(plWithDuplicates) | 0;
        if ((actual_28 === expected_42) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, expected_42, "cloned preserves duplicates");
        }
        else {
            let valueType_25;
            let copyOfStruct_25 = actual_28;
            valueType_25 = int32_type;
            const primitiveTypes_25 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_25;
            if (contains(valueType_25, primitiveTypes_25, {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            })) {
                const arg_30 = int32ToString(expected_42);
                const arg_1_25 = int32ToString(actual_28);
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("cloned preserves duplicates");
            }
            else {
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_42)(actual_28)("cloned preserves duplicates");
            }
            throw new Exception(errorMsg_25);
        }
        let d_14;
        const a_32 = item(0, Polyline2D__get_Points(cloned));
        const b_32 = item(1, Polyline2D__get_Points(cloned));
        const vx_14 = a_32.X - b_32.X;
        const vy_14 = a_32.Y - b_32.Y;
        d_14 = Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14));
        const r_14 = d_14 < 1E-09;
        const actual_29 = r_14;
        if ((actual_29 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, true, "cloned duplicate points match");
        }
        else {
            let valueType_26;
            let copyOfStruct_26 = actual_29;
            valueType_26 = bool_type;
            const primitiveTypes_26 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_26;
            if (contains(valueType_26, primitiveTypes_26, {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            })) {
                const arg_31 = toString(true);
                const arg_1_26 = toString(actual_29);
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("cloned duplicate points match");
            }
            else {
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_29)("cloned duplicate points match");
            }
            throw new Exception(errorMsg_26);
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("bounding rectangle with duplicates", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const bbox = Polyline2D__get_BoundingRectangle(plAllDuplicates);
        Expect_isTrue(Math.abs(bbox.MinX - 1) < 1E-09)("bbox min X with duplicates");
        Expect_isTrue(Math.abs(bbox.MaxX - 1) < 1E-09)("bbox max X with duplicates");
        Expect_isTrue(Math.abs(bbox.MinY - 1) < 1E-09)("bbox min Y with duplicates");
        Expect_isTrue(Math.abs(bbox.MaxY - 1) < 1E-09)("bbox max Y with duplicates");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})()])), Test_testList("Remove Duplicate / Colinear", ofArray([(() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("removeDuplicatePoints open", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let pl_3;
        const points_3 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)];
        pl_3 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_3));
        const cleaned = Polyline2D_removeDuplicatePoints(1E-09, pl_3);
        const actual_30 = Polyline2D__get_PointCount(cleaned) | 0;
        if ((actual_30 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, 3, "duplicates removed keeping endpoints");
        }
        else {
            let valueType_27;
            let copyOfStruct_27 = actual_30;
            valueType_27 = int32_type;
            const primitiveTypes_27 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_27;
            if (contains(valueType_27, primitiveTypes_27, {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            })) {
                const arg_32 = int32ToString(3);
                const arg_1_27 = int32ToString(actual_30);
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("duplicates removed keeping endpoints");
            }
            else {
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_30)("duplicates removed keeping endpoints");
            }
            throw new Exception(errorMsg_27);
        }
        Expect_isFalse(Polyline2D__get_IsClosed(cleaned))("open polyline remains open");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("removeDuplicatePoints closed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let pl_5;
        const points_4 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(0, 0)];
        pl_5 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_4));
        const cleaned_1 = Polyline2D_removeDuplicatePoints(1E-09, pl_5);
        const actual_31 = Polyline2D__get_PointCount(cleaned_1) | 0;
        if ((actual_31 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, 5, "one duplicate removed");
        }
        else {
            let valueType_28;
            let copyOfStruct_28 = actual_31;
            valueType_28 = int32_type;
            const primitiveTypes_28 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_28;
            if (contains(valueType_28, primitiveTypes_28, {
                Equals: equals,
                GetHashCode: (x_28) => (structuralHash(x_28) | 0),
            })) {
                const arg_33 = int32ToString(5);
                const arg_1_28 = int32ToString(actual_31);
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("one duplicate removed");
            }
            else {
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_31)("one duplicate removed");
            }
            throw new Exception(errorMsg_28);
        }
        Expect_isTrue(Polyline2D__get_IsClosed(cleaned_1))("should stay closed");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("removeColinearAndDuplicatePoints square", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let pl_7;
        const points_5 = [Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(4, 0), Pt_$ctor_7B00E9A0(4, 2), Pt_$ctor_7B00E9A0(4, 4), Pt_$ctor_7B00E9A0(2, 4), Pt_$ctor_7B00E9A0(0, 4), Pt_$ctor_7B00E9A0(0, 2), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0)];
        pl_7 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_5));
        const simplified = Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, pl_7);
        const actual_32 = Polyline2D__get_PointCount(simplified) | 0;
        const msg_47 = concat("no colinear simplification with loose angle tolerance ", ...Polyline2D__get_AsFSharpCode(simplified));
        if ((actual_32 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, 5, msg_47);
        }
        else {
            let valueType_29;
            let copyOfStruct_29 = actual_32;
            valueType_29 = int32_type;
            const primitiveTypes_29 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_29;
            if (contains(valueType_29, primitiveTypes_29, {
                Equals: equals,
                GetHashCode: (x_29) => (structuralHash(x_29) | 0),
            })) {
                const arg_34 = int32ToString(5);
                const arg_1_29 = int32ToString(actual_32);
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)(msg_47);
            }
            else {
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_32)(msg_47);
            }
            throw new Exception(errorMsg_29);
        }
        Expect_isTrue(Polyline2D__get_IsClosed(simplified))("remains closed");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("removeColinearAndDuplicatePoints open line with repeats", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let pl_9;
        const points_6 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0), Pt_$ctor_7B00E9A0(3, 0)];
        pl_9 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_6));
        const simplified_1 = Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, pl_9);
        const actual_33 = Polyline2D__get_PointCount(simplified_1) | 0;
        if ((actual_33 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, 2, "duplicate end removed; interior colinear points removed");
        }
        else {
            let valueType_30;
            let copyOfStruct_30 = actual_33;
            valueType_30 = int32_type;
            const primitiveTypes_30 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_30;
            if (contains(valueType_30, primitiveTypes_30, {
                Equals: equals,
                GetHashCode: (x_30) => (structuralHash(x_30) | 0),
            })) {
                const arg_35 = int32ToString(2);
                const arg_1_30 = int32ToString(actual_33);
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("duplicate end removed; interior colinear points removed");
            }
            else {
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_33)("duplicate end removed; interior colinear points removed");
            }
            throw new Exception(errorMsg_30);
        }
        let d_15;
        const a_34 = item(0, Polyline2D__get_Points(simplified_1));
        const b_34 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_15 = a_34.X - b_34.X;
        const vy_15 = a_34.Y - b_34.Y;
        d_15 = Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15));
        const r_15 = d_15 < 1E-09;
        const actual_34 = r_15;
        if ((actual_34 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, true, "start");
        }
        else {
            let valueType_31;
            let copyOfStruct_31 = actual_34;
            valueType_31 = bool_type;
            const primitiveTypes_31 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_31;
            if (contains(valueType_31, primitiveTypes_31, {
                Equals: equals,
                GetHashCode: (x_31) => (structuralHash(x_31) | 0),
            })) {
                const arg_36 = toString(true);
                const arg_1_31 = toString(actual_34);
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("start");
            }
            else {
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_34)("start");
            }
            throw new Exception(errorMsg_31);
        }
        let d_16;
        const a_36 = item(Polyline2D__get_Points(simplified_1).length - 1, Polyline2D__get_Points(simplified_1));
        const b_36 = Pt_$ctor_7B00E9A0(3, 0);
        const vx_16 = a_36.X - b_36.X;
        const vy_16 = a_36.Y - b_36.Y;
        d_16 = Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16));
        const r_16 = d_16 < 1E-09;
        const actual_35 = r_16;
        if ((actual_35 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, true, "end");
        }
        else {
            let valueType_32;
            let copyOfStruct_32 = actual_35;
            valueType_32 = bool_type;
            const primitiveTypes_32 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_32;
            if (contains(valueType_32, primitiveTypes_32, {
                Equals: equals,
                GetHashCode: (x_32) => (structuralHash(x_32) | 0),
            })) {
                const arg_37 = toString(true);
                const arg_1_32 = toString(actual_35);
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_32)("end");
            }
            else {
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_35)("end");
            }
            throw new Exception(errorMsg_32);
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("colinear start/end segments closed polygon", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let pl_11;
        const points_7 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(2, 2), Pt_$ctor_7B00E9A0(2, 4), Pt_$ctor_7B00E9A0(1, 4), Pt_$ctor_7B00E9A0(0, 4), Pt_$ctor_7B00E9A0(0, 2), Pt_$ctor_7B00E9A0(0, 0)];
        pl_11 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_7));
        const simplified_2 = Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, pl_11);
        Expect_isTrue(Polyline2D__get_IsClosed(simplified_2))("closed retained");
        const actual_36 = Polyline2D__get_PointCount(simplified_2) | 0;
        if ((actual_36 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, 5, " colinear removal at this tolerance");
        }
        else {
            let valueType_33;
            let copyOfStruct_33 = actual_36;
            valueType_33 = int32_type;
            const primitiveTypes_33 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_33;
            if (contains(valueType_33, primitiveTypes_33, {
                Equals: equals,
                GetHashCode: (x_33) => (structuralHash(x_33) | 0),
            })) {
                const arg_38 = int32ToString(5);
                const arg_1_33 = int32ToString(actual_36);
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_38)(arg_1_33)(" colinear removal at this tolerance");
            }
            else {
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_36)(" colinear removal at this tolerance");
            }
            throw new Exception(errorMsg_33);
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("almost colinear tiny angle retained", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let pl_13;
        const points_8 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(20, 0.2), Pt_$ctor_7B00E9A0(30, 0)];
        pl_13 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_8));
        const simplified_3 = Polyline2D_removeColinearAndDuplicatePoints(0.9999999847691291, 1E-09, pl_13);
        const actual_37 = Polyline2D__get_PointCount(simplified_3) | 0;
        if ((actual_37 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, 4, "almost straight kept");
        }
        else {
            let valueType_34;
            let copyOfStruct_34 = actual_37;
            valueType_34 = int32_type;
            const primitiveTypes_34 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_34;
            if (contains(valueType_34, primitiveTypes_34, {
                Equals: equals,
                GetHashCode: (x_34) => (structuralHash(x_34) | 0),
            })) {
                const arg_39 = int32ToString(4);
                const arg_1_34 = int32ToString(actual_37);
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_39)(arg_1_34)("almost straight kept");
            }
            else {
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_37)("almost straight kept");
            }
            throw new Exception(errorMsg_34);
        }
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("open U-turn 180 collapse prevented (since method only removes colinear not u-turn)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let pl_15;
        const points_9 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(-5, 0)];
        pl_15 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_9));
        const simplified_4 = Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, pl_15);
        const actual_38 = Polyline2D__get_PointCount(simplified_4) | 0;
        if ((actual_38 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, 3, "collapsed to endpoints after duplicate removal");
        }
        else {
            let valueType_35;
            let copyOfStruct_35 = actual_38;
            valueType_35 = int32_type;
            const primitiveTypes_35 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_35;
            if (contains(valueType_35, primitiveTypes_35, {
                Equals: equals,
                GetHashCode: (x_35) => (structuralHash(x_35) | 0),
            })) {
                const arg_40 = int32ToString(3);
                const arg_1_35 = int32ToString(actual_38);
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_40)(arg_1_35)("collapsed to endpoints after duplicate removal");
            }
            else {
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_38)("collapsed to endpoints after duplicate removal");
            }
            throw new Exception(errorMsg_35);
        }
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointin polyline special cases", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let failPoly;
        const points_10 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0), Pt_$ctor_7B00E9A0(4, 0), Pt_$ctor_7B00E9A0(3, 1), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(0, 2), Pt_$ctor_7B00E9A0(0, 3), Pt_$ctor_7B00E9A0(0, 4), Pt_$ctor_7B00E9A0(0, 5), Pt_$ctor_7B00E9A0(14, 0), Pt_$ctor_7B00E9A0(12, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(9, 0), Pt_$ctor_7B00E9A0(0, -0.741577), Pt_$ctor_7B00E9A0(0, -3), Pt_$ctor_7B00E9A0(0, -4), Pt_$ctor_7B00E9A0(0, -5), Pt_$ctor_7B00E9A0(0, -6), Pt_$ctor_7B00E9A0(-1.47302, 0), Pt_$ctor_7B00E9A0(-3, 0), Pt_$ctor_7B00E9A0(-5, -3), Pt_$ctor_7B00E9A0(-9.10595, -3), Pt_$ctor_7B00E9A0(-12, -3), Pt_$ctor_7B00E9A0(-13, 0), Pt_$ctor_7B00E9A0(-10, 0), Pt_$ctor_7B00E9A0(-9, 0), Pt_$ctor_7B00E9A0(-7, 0), Pt_$ctor_7B00E9A0(-5, 0), Pt_$ctor_7B00E9A0(0, 14), Pt_$ctor_7B00E9A0(0, 13), Pt_$ctor_7B00E9A0(0, 11), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 9), Pt_$ctor_7B00E9A0(-2, 5), Pt_$ctor_7B00E9A0(-2, 3), Pt_$ctor_7B00E9A0(-1.71199, 1.28105), Pt_$ctor_7B00E9A0(0, 0)];
        failPoly = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_10));
        const expectedInside = [Pt_$ctor_7B00E9A0(-3.1, 0), Pt_$ctor_7B00E9A0(0, -0.1), Pt_$ctor_7B00E9A0(0, -0.2), Pt_$ctor_7B00E9A0(0, -0.3), Pt_$ctor_7B00E9A0(0, -0.4), Pt_$ctor_7B00E9A0(0, -0.5), Pt_$ctor_7B00E9A0(0, -0.6), Pt_$ctor_7B00E9A0(-0.1, 0), Pt_$ctor_7B00E9A0(-0.2, 0), Pt_$ctor_7B00E9A0(-0.3, 0), Pt_$ctor_7B00E9A0(-0.4, 0), Pt_$ctor_7B00E9A0(-0.5, 0), Pt_$ctor_7B00E9A0(-0.6, 0), Pt_$ctor_7B00E9A0(-0.7, 0), Pt_$ctor_7B00E9A0(-0.8, 0), Pt_$ctor_7B00E9A0(-0.9, 0), Pt_$ctor_7B00E9A0(-1, 0), Pt_$ctor_7B00E9A0(-1.1, 0), Pt_$ctor_7B00E9A0(-1.2, 0), Pt_$ctor_7B00E9A0(-1.3, 0), Pt_$ctor_7B00E9A0(-1.4, 0), Pt_$ctor_7B00E9A0(-3.2, 0), Pt_$ctor_7B00E9A0(-3.3, 0), Pt_$ctor_7B00E9A0(-3.4, 0), Pt_$ctor_7B00E9A0(-3.5, 0), Pt_$ctor_7B00E9A0(-3.6, 0), Pt_$ctor_7B00E9A0(-3.7, 0), Pt_$ctor_7B00E9A0(-3.8, 0), Pt_$ctor_7B00E9A0(-3.9, 0), Pt_$ctor_7B00E9A0(-4, 0), Pt_$ctor_7B00E9A0(-4.1, 0), Pt_$ctor_7B00E9A0(-4.2, 0), Pt_$ctor_7B00E9A0(-4.3, 0), Pt_$ctor_7B00E9A0(-4.4, 0), Pt_$ctor_7B00E9A0(-4.5, 0), Pt_$ctor_7B00E9A0(-4.6, 0), Pt_$ctor_7B00E9A0(-4.7, 0), Pt_$ctor_7B00E9A0(-4.8, 0), Pt_$ctor_7B00E9A0(-4.9, 0)];
        const expectedOutside = [Pt_$ctor_7B00E9A0(0, 0.6), Pt_$ctor_7B00E9A0(0, 0.5), Pt_$ctor_7B00E9A0(0, 0.4), Pt_$ctor_7B00E9A0(0, 0.3), Pt_$ctor_7B00E9A0(0, 0.2), Pt_$ctor_7B00E9A0(0, 0.1)];
        const allInside = expectedInside.every((pt) => Polyline2D__Contains_6ADE94FD(failPoly, pt));
        Expect_isTrue(allInside)("all expected inside points should be inside, winding");
        const allOutside = !expectedOutside.every((pt_1) => Polyline2D__Contains_6ADE94FD(failPoly, pt_1));
        Expect_isTrue(allOutside)("all expected outside points should be outside, winding");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})()]))]));

export const testsComprehensive = Test_testList("Polyline2D Comprehensive", ofArray([Test_testList("SetVertex", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set vertex on open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let pl;
        const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0)];
        pl = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
        Polyline2D__SetVertex(pl, 1, Pt_$ctor_7B00E9A0(1, 5));
        let d;
        const a_2 = item(1, Polyline2D__get_Points(pl));
        const b_2 = Pt_$ctor_7B00E9A0(1, 5);
        const vx = a_2.X - b_2.X;
        const vy = a_2.Y - b_2.Y;
        d = Math.sqrt((vx * vx) + (vy * vy));
        const r = d < 1E-09;
        const actual = r;
        if ((actual === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, true, "vertex 1 changed");
        }
        else {
            let valueType;
            let copyOfStruct = actual;
            valueType = bool_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            })) {
                const arg = toString(true);
                const arg_1 = toString(actual);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("vertex 1 changed");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual)("vertex 1 changed");
            }
            throw new Exception(errorMsg);
        }
        let d_1;
        const a_4 = item(0, Polyline2D__get_Points(pl));
        const b_4 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_1 = a_4.X - b_4.X;
        const vy_1 = a_4.Y - b_4.Y;
        d_1 = Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1));
        const r_1 = d_1 < 1E-09;
        const actual_1 = r_1;
        if ((actual_1 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, true, "vertex 0 unchanged");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_1;
            valueType_1 = bool_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg_6 = toString(true);
                const arg_1_1 = toString(actual_1);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("vertex 0 unchanged");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_1)("vertex 0 unchanged");
            }
            throw new Exception(errorMsg_1);
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set first vertex on closed polyline updates last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let pl_1;
        const points_1 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
        pl_1 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_1));
        Polyline2D__SetVertex(pl_1, 0, Pt_$ctor_7B00E9A0(1, 1));
        let d_2;
        const a_6 = item(0, Polyline2D__get_Points(pl_1));
        const b_6 = Pt_$ctor_7B00E9A0(1, 1);
        const vx_2 = a_6.X - b_6.X;
        const vy_2 = a_6.Y - b_6.Y;
        d_2 = Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2));
        const r_2 = d_2 < 1E-09;
        const actual_2 = r_2;
        if ((actual_2 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, true, "first vertex updated");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_2;
            valueType_2 = bool_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_7 = toString(true);
                const arg_1_2 = toString(actual_2);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("first vertex updated");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_2)("first vertex updated");
            }
            throw new Exception(errorMsg_2);
        }
        let d_3;
        const a_8 = item(4, Polyline2D__get_Points(pl_1));
        const b_8 = Pt_$ctor_7B00E9A0(1, 1);
        const vx_3 = a_8.X - b_8.X;
        const vy_3 = a_8.Y - b_8.Y;
        d_3 = Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3));
        const r_3 = d_3 < 1E-09;
        const actual_3 = r_3;
        if ((actual_3 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, true, "last vertex updated too");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_3;
            valueType_3 = bool_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            })) {
                const arg_8 = toString(true);
                const arg_1_3 = toString(actual_3);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("last vertex updated too");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_3)("last vertex updated too");
            }
            throw new Exception(errorMsg_3);
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set last vertex on closed polyline updates first", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let pl_2;
        const points_2 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
        pl_2 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_2));
        Polyline2D__SetVertex(pl_2, 4, Pt_$ctor_7B00E9A0(2, 2));
        let d_4;
        const a_10 = item(4, Polyline2D__get_Points(pl_2));
        const b_10 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_4 = a_10.X - b_10.X;
        const vy_4 = a_10.Y - b_10.Y;
        d_4 = Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4));
        const r_4 = d_4 < 1E-09;
        const actual_4 = r_4;
        if ((actual_4 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, true, "last vertex updated");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_4;
            valueType_4 = bool_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_9 = toString(true);
                const arg_1_4 = toString(actual_4);
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("last vertex updated");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_4)("last vertex updated");
            }
            throw new Exception(errorMsg_4);
        }
        let d_5;
        const a_12 = item(0, Polyline2D__get_Points(pl_2));
        const b_12 = Pt_$ctor_7B00E9A0(2, 2);
        const vx_5 = a_12.X - b_12.X;
        const vy_5 = a_12.Y - b_12.Y;
        d_5 = Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5));
        const r_5 = d_5 < 1E-09;
        const actual_5 = r_5;
        if ((actual_5 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, true, "first vertex updated too");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_5;
            valueType_5 = bool_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_10 = toString(true);
                const arg_1_5 = toString(actual_5);
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("first vertex updated too");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_5)("first vertex updated too");
            }
            throw new Exception(errorMsg_5);
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set vertex out of range throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let pl_3;
        const points_3 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)];
        pl_3 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_3));
        Expect_throws(() => {
            Polyline2D__SetVertex(pl_3, -1, Pt_$ctor_7B00E9A0(0, 0));
        }, "negative index");
        Expect_throws(() => {
            Polyline2D__SetVertex(pl_3, 2, Pt_$ctor_7B00E9A0(0, 0));
        }, "too large index");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})()])), Test_testList("SecondPoint and SecondLastPoint", ofArray([(() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("get second point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let pl_4;
        const points_4 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(3, 4)];
        pl_4 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_4));
        let d_6;
        const a_14 = Polyline2D__get_SecondPoint(pl_4);
        const b_14 = Pt_$ctor_7B00E9A0(1, 2);
        const vx_6 = a_14.X - b_14.X;
        const vy_6 = a_14.Y - b_14.Y;
        d_6 = Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6));
        const r_6 = d_6 < 1E-09;
        const actual_6 = r_6;
        if ((actual_6 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, true, "second point");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_6;
            valueType_6 = bool_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg_11 = toString(true);
                const arg_1_6 = toString(actual_6);
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("second point");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_6)("second point");
            }
            throw new Exception(errorMsg_6);
        }
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set second point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let pl_5;
        const points_5 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(3, 4)];
        pl_5 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_5));
        Polyline2D__set_SecondPoint_6ADE94FD(pl_5, Pt_$ctor_7B00E9A0(9, 9));
        let d_7;
        const a_16 = item(1, Polyline2D__get_Points(pl_5));
        const b_16 = Pt_$ctor_7B00E9A0(9, 9);
        const vx_7 = a_16.X - b_16.X;
        const vy_7 = a_16.Y - b_16.Y;
        d_7 = Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7));
        const r_7 = d_7 < 1E-09;
        const actual_7 = r_7;
        if ((actual_7 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, true, "second point set");
        }
        else {
            let valueType_7;
            let copyOfStruct_7 = actual_7;
            valueType_7 = bool_type;
            const primitiveTypes_7 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_7;
            if (contains(valueType_7, primitiveTypes_7, {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            })) {
                const arg_12 = toString(true);
                const arg_1_7 = toString(actual_7);
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("second point set");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_7)("second point set");
            }
            throw new Exception(errorMsg_7);
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("get second last point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let pl_6;
        const points_6 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(3, 4)];
        pl_6 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_6));
        let d_8;
        const a_18 = Polyline2D__get_SecondLastPoint(pl_6);
        const b_18 = Pt_$ctor_7B00E9A0(1, 2);
        const vx_8 = a_18.X - b_18.X;
        const vy_8 = a_18.Y - b_18.Y;
        d_8 = Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8));
        const r_8 = d_8 < 1E-09;
        const actual_8 = r_8;
        if ((actual_8 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, true, "second last point");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_8;
            valueType_8 = bool_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            })) {
                const arg_13 = toString(true);
                const arg_1_8 = toString(actual_8);
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("second last point");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_8)("second last point");
            }
            throw new Exception(errorMsg_8);
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set second last point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let pl_7;
        const points_7 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(3, 4)];
        pl_7 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_7));
        Polyline2D__set_SecondLastPoint_6ADE94FD(pl_7, Pt_$ctor_7B00E9A0(7, 7));
        let d_9;
        const a_20 = item(1, Polyline2D__get_Points(pl_7));
        const b_20 = Pt_$ctor_7B00E9A0(7, 7);
        const vx_9 = a_20.X - b_20.X;
        const vy_9 = a_20.Y - b_20.Y;
        d_9 = Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9));
        const r_9 = d_9 < 1E-09;
        const actual_9 = r_9;
        if ((actual_9 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, true, "second last point set");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_9;
            valueType_9 = bool_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            })) {
                const arg_14 = toString(true);
                const arg_1_9 = toString(actual_9);
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("second last point set");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_9)("second last point set");
            }
            throw new Exception(errorMsg_9);
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("second point fails on single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        Expect_throws(() => {
            Polyline2D__get_SecondPoint(plSinglePoint);
        }, "get second point on single");
        Expect_throws(() => {
            Polyline2D__get_SecondLastPoint(plSinglePoint);
        }, "get second last on single");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})()])), Test_testList("LastPointIndex and LastSegmentIndex", ofArray([(() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("last point index", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const actual_10 = Polyline2D__get_LastPointIndex(plOpen) | 0;
        if ((actual_10 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 3, "open polyline last point index");
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
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("open polyline last point index");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_10)("open polyline last point index");
            }
            throw new Exception(errorMsg_10);
        }
        const actual_11 = Polyline2D__get_LastPointIndex(plClosed) | 0;
        if ((actual_11 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 4, "closed polyline last point index");
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
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("closed polyline last point index");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_11)("closed polyline last point index");
            }
            throw new Exception(errorMsg_11);
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("last segment index", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const actual_12 = Polyline2D__get_LastSegmentIndex(plOpen) | 0;
        if ((actual_12 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 2, "open polyline last segment index");
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
                const arg_17 = int32ToString(2);
                const arg_1_12 = int32ToString(actual_12);
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("open polyline last segment index");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_12)("open polyline last segment index");
            }
            throw new Exception(errorMsg_12);
        }
        const actual_13 = Polyline2D__get_LastSegmentIndex(plClosed) | 0;
        if ((actual_13 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 3, "closed polyline last segment index");
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
                const arg_18 = int32ToString(3);
                const arg_1_13 = int32ToString(actual_13);
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("closed polyline last segment index");
            }
            else {
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_13)("closed polyline last segment index");
            }
            throw new Exception(errorMsg_13);
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})()])), Test_testList("IsAlmostClosed", ofArray([(() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("exactly closed is almost closed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        Expect_isTrue(Polyline2D__IsAlmostClosed_5E38073B(plClosed, 1E-06))("exactly closed should be almost closed");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("open is not almost closed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        Expect_isFalse(Polyline2D__IsAlmostClosed_5E38073B(plOpen, 1E-06))("open should not be almost closed");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("nearly closed within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let pl_8;
        const points_8 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 0.0001)];
        pl_8 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_8));
        Expect_isTrue(Polyline2D__IsAlmostClosed_5E38073B(pl_8, 0.001))("nearly closed within tolerance");
        Expect_isFalse(Polyline2D__IsAlmostClosed_5E38073B(pl_8, 1E-05))("nearly closed outside tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("too few points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let pl_9;
        const points_9 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 0)];
        pl_9 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_9));
        Expect_isFalse(Polyline2D__IsAlmostClosed_5E38073B(pl_9, 1))("two points should not be almost closed");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})()])), Test_testList("Segments property", ofArray([(() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segments of open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const segs = Polyline2D__get_Segments(plOpen);
        const actual_14 = segs.length | 0;
        if ((actual_14 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, 3, "3 segments");
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
                const arg_19 = int32ToString(3);
                const arg_1_14 = int32ToString(actual_14);
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("3 segments");
            }
            else {
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_14)("3 segments");
            }
            throw new Exception(errorMsg_14);
        }
        let d_10;
        let a_22;
        let copyOfStruct_15 = item(0, segs);
        const ln = copyOfStruct_15;
        a_22 = Pt_$ctor_7B00E9A0_1(ln.FromX, ln.FromY);
        const b_22 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_10 = a_22.X - b_22.X;
        const vy_10 = a_22.Y - b_22.Y;
        d_10 = Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10));
        const r_10 = d_10 < 1E-09;
        const actual_15 = r_10;
        if ((actual_15 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, true, "seg 0 from");
        }
        else {
            let valueType_15;
            let copyOfStruct_16 = actual_15;
            valueType_15 = bool_type;
            const primitiveTypes_15 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_15;
            if (contains(valueType_15, primitiveTypes_15, {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            })) {
                const arg_20 = toString(true);
                const arg_1_15 = toString(actual_15);
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("seg 0 from");
            }
            else {
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_15)("seg 0 from");
            }
            throw new Exception(errorMsg_15);
        }
        let d_11;
        let a_24;
        let copyOfStruct_17 = item(0, segs);
        const ln_1 = copyOfStruct_17;
        a_24 = Pt_$ctor_7B00E9A0_1(ln_1.ToX, ln_1.ToY);
        const b_24 = Pt_$ctor_7B00E9A0(10, 0);
        const vx_11 = a_24.X - b_24.X;
        const vy_11 = a_24.Y - b_24.Y;
        d_11 = Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11));
        const r_11 = d_11 < 1E-09;
        const actual_16 = r_11;
        if ((actual_16 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, true, "seg 0 to");
        }
        else {
            let valueType_16;
            let copyOfStruct_18 = actual_16;
            valueType_16 = bool_type;
            const primitiveTypes_16 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_16;
            if (contains(valueType_16, primitiveTypes_16, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_21 = toString(true);
                const arg_1_16 = toString(actual_16);
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("seg 0 to");
            }
            else {
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_16)("seg 0 to");
            }
            throw new Exception(errorMsg_16);
        }
        let d_12;
        let a_26;
        let copyOfStruct_19 = item(2, segs);
        const ln_2 = copyOfStruct_19;
        a_26 = Pt_$ctor_7B00E9A0_1(ln_2.ToX, ln_2.ToY);
        const b_26 = Pt_$ctor_7B00E9A0(0, 10);
        const vx_12 = a_26.X - b_26.X;
        const vy_12 = a_26.Y - b_26.Y;
        d_12 = Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12));
        const r_12 = d_12 < 1E-09;
        const actual_17 = r_12;
        if ((actual_17 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, true, "seg 2 to");
        }
        else {
            let valueType_17;
            let copyOfStruct_20 = actual_17;
            valueType_17 = bool_type;
            const primitiveTypes_17 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_17;
            if (contains(valueType_17, primitiveTypes_17, {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            })) {
                const arg_22 = toString(true);
                const arg_1_17 = toString(actual_17);
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("seg 2 to");
            }
            else {
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_17)("seg 2 to");
            }
            throw new Exception(errorMsg_17);
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segments of empty polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const segs_1 = Polyline2D__get_Segments(plEmpty);
        const actual_18 = segs_1.length | 0;
        if ((actual_18 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, 0, "empty has no segments");
        }
        else {
            let valueType_18;
            let copyOfStruct_21 = actual_18;
            valueType_18 = int32_type;
            const primitiveTypes_18 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_18;
            if (contains(valueType_18, primitiveTypes_18, {
                Equals: equals,
                GetHashCode: (x_18) => (structuralHash(x_18) | 0),
            })) {
                const arg_23 = int32ToString(0);
                const arg_1_18 = int32ToString(actual_18);
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("empty has no segments");
            }
            else {
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_18)("empty has no segments");
            }
            throw new Exception(errorMsg_18);
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segments of single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const segs_2 = Polyline2D__get_Segments(plSinglePoint);
        const actual_19 = segs_2.length | 0;
        if ((actual_19 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, 0, "single point has no segments");
        }
        else {
            let valueType_19;
            let copyOfStruct_22 = actual_19;
            valueType_19 = int32_type;
            const primitiveTypes_19 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_19;
            if (contains(valueType_19, primitiveTypes_19, {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            })) {
                const arg_24 = int32ToString(0);
                const arg_1_19 = int32ToString(actual_19);
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("single point has no segments");
            }
            else {
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_19)("single point has no segments");
            }
            throw new Exception(errorMsg_19);
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})()])), Test_testList("SegmentVectors property", ofArray([(() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segment vectors of open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const vecs = Polyline2D__get_SegmentVectors(plOpen);
        const actual_20 = vecs.length | 0;
        if ((actual_20 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, 3, "3 vectors");
        }
        else {
            let valueType_20;
            let copyOfStruct_23 = actual_20;
            valueType_20 = int32_type;
            const primitiveTypes_20 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_20;
            if (contains(valueType_20, primitiveTypes_20, {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            })) {
                const arg_25 = int32ToString(3);
                const arg_1_20 = int32ToString(actual_20);
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("3 vectors");
            }
            else {
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_20)("3 vectors");
            }
            throw new Exception(errorMsg_20);
        }
        Expect_isTrue((Math.abs(item(0, vecs).X - 10) < 1E-09) && (Math.abs(item(0, vecs).Y) < 1E-09))("vec 0");
        Expect_isTrue((Math.abs(item(1, vecs).X) < 1E-09) && (Math.abs(item(1, vecs).Y - 10) < 1E-09))("vec 1");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segment vectors of empty polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const vecs_1 = Polyline2D__get_SegmentVectors(plEmpty);
        const actual_21 = vecs_1.length | 0;
        if ((actual_21 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 0, "empty has no vectors");
        }
        else {
            let valueType_21;
            let copyOfStruct_28 = actual_21;
            valueType_21 = int32_type;
            const primitiveTypes_21 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_21;
            if (contains(valueType_21, primitiveTypes_21, {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            })) {
                const arg_26 = int32ToString(0);
                const arg_1_21 = int32ToString(actual_21);
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("empty has no vectors");
            }
            else {
                errorMsg_21 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_21)("empty has no vectors");
            }
            throw new Exception(errorMsg_21);
        }
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})()])), Test_testList("SignedDistanceTo", ofArray([(() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("inside point positive distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const inside = Pt_$ctor_7B00E9A0(5, 5);
        const sd = Polyline2D__SignedDistanceTo_6ADE94FD(plClosed, inside);
        Expect_isTrue(sd > 0)("inside positive");
        Expect_isTrue(Math.abs(sd - 5) < 1E-09)("inside distance value");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("outside point negative distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const outside = Pt_$ctor_7B00E9A0(15, 5);
        const sd_1 = Polyline2D__SignedDistanceTo_6ADE94FD(plClosed, outside);
        Expect_isTrue(sd_1 < 0)("outside negative");
        Expect_isTrue(Math.abs(sd_1 + 5) < 1E-09)("outside distance value");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("point on edge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const onEdge = Pt_$ctor_7B00E9A0(5, 0);
        const sd_2 = Polyline2D__SignedDistanceTo_6ADE94FD(plClosed, onEdge);
        Expect_isTrue(Math.abs(sd_2) < 1E-06)("on edge distance near zero");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})()])), Test_testList("ToString and AsString", ofArray([(() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("toString on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const s = toString(plEmpty);
        Expect_stringContains(s, "empty", "should say empty");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("toString on closed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        const s_1 = toString(plClosed);
        Expect_stringContains(s_1, "closed", "should say closed");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("toString on open", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        const s_2 = toString(plOpen);
        Expect_stringContains(s_2, "open", "should say open");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("asString on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        const s_3 = Polyline2D__get_AsString(plEmpty);
        Expect_stringContains(s_3, "empty", "should say empty");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("asString on closed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        const s_4 = Polyline2D__get_AsString(plClosed);
        Expect_stringContains(s_4, "closed", "should say closed");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})()])), Test_testList("AsFSharpCode", singleton((() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("produces valid code string", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const code = Polyline2D__get_AsFSharpCode(plLine);
        Expect_stringContains(code, "Polyline2D.create", "should contain create");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})())), Test_testList("Static close", ofArray([(() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const closed = Polyline2D_close_Z5A89AEF5(plOpen);
        Expect_isTrue(Polyline2D__get_IsClosed(closed))("should be closed");
        const actual_22 = Polyline2D__get_PointCount(closed) | 0;
        const expected_35 = (Polyline2D__get_PointCount(plOpen) + 1) | 0;
        if ((actual_22 === expected_35) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_22, expected_35, "one point added");
        }
        else {
            let valueType_22;
            let copyOfStruct_29 = actual_22;
            valueType_22 = int32_type;
            const primitiveTypes_22 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_22;
            if (contains(valueType_22, primitiveTypes_22, {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            })) {
                const arg_27 = int32ToString(expected_35);
                const arg_1_22 = int32ToString(actual_22);
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("one point added");
            }
            else {
                errorMsg_22 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_35)(actual_22)("one point added");
            }
            throw new Exception(errorMsg_22);
        }
        let d_13;
        const a_28 = item(0, Polyline2D__get_Points(closed));
        const b_28 = item(Polyline2D__get_PointCount(closed) - 1, Polyline2D__get_Points(closed));
        const vx_13 = a_28.X - b_28.X;
        const vy_13 = a_28.Y - b_28.Y;
        d_13 = Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13));
        const r_13 = d_13 < 1E-09;
        const actual_23 = r_13;
        if ((actual_23 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, true, "first equals last");
        }
        else {
            let valueType_23;
            let copyOfStruct_30 = actual_23;
            valueType_23 = bool_type;
            const primitiveTypes_23 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_23;
            if (contains(valueType_23, primitiveTypes_23, {
                Equals: equals,
                GetHashCode: (x_23) => (structuralHash(x_23) | 0),
            })) {
                const arg_28 = toString(true);
                const arg_1_23 = toString(actual_23);
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("first equals last");
            }
            else {
                errorMsg_23 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_23)("first equals last");
            }
            throw new Exception(errorMsg_23);
        }
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close already closed polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        const closed_1 = Polyline2D_close_Z5A89AEF5(plClosed);
        Expect_isTrue(Polyline2D__get_IsClosed(closed_1))("should remain closed");
        const actual_24 = Polyline2D__get_PointCount(closed_1) | 0;
        const expected_38 = Polyline2D__get_PointCount(plClosed) | 0;
        if ((actual_24 === expected_38) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, expected_38, "no extra point added");
        }
        else {
            let valueType_24;
            let copyOfStruct_31 = actual_24;
            valueType_24 = int32_type;
            const primitiveTypes_24 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_24;
            if (contains(valueType_24, primitiveTypes_24, {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            })) {
                const arg_29 = int32ToString(expected_38);
                const arg_1_24 = int32ToString(actual_24);
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("no extra point added");
            }
            else {
                errorMsg_24 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_38)(actual_24)("no extra point added");
            }
            throw new Exception(errorMsg_24);
        }
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close nearly closed polyline snaps last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let pl_10;
        const points_10 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 1E-08)];
        pl_10 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_10));
        const closed_2 = Polyline2D_close_Z5A89AEF5(pl_10);
        Expect_isTrue(Polyline2D__get_IsClosed(closed_2))("should be closed");
        let d_14;
        const a_30 = item(0, Polyline2D__get_Points(closed_2));
        const b_30 = item(Polyline2D__get_PointCount(closed_2) - 1, Polyline2D__get_Points(closed_2));
        const vx_14 = a_30.X - b_30.X;
        const vy_14 = a_30.Y - b_30.Y;
        d_14 = Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14));
        const r_14 = d_14 < 1E-09;
        const actual_25 = r_14;
        if ((actual_25 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, true, "last snapped to first");
        }
        else {
            let valueType_25;
            let copyOfStruct_32 = actual_25;
            valueType_25 = bool_type;
            const primitiveTypes_25 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_25;
            if (contains(valueType_25, primitiveTypes_25, {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            })) {
                const arg_30 = toString(true);
                const arg_1_25 = toString(actual_25);
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("last snapped to first");
            }
            else {
                errorMsg_25 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_25)("last snapped to first");
            }
            throw new Exception(errorMsg_25);
        }
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close fails with less than 2 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        Expect_throws(() => {
            Polyline2D_close_Z5A89AEF5(plSinglePoint);
        }, "single point close should fail");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})()])), Test_testList("Static closeInPlace", ofArray([(() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closeInPlace on open", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        const pl_11 = Polyline2D__Clone(plOpen);
        Polyline2D_closeInPlace_Z5A89AEF5(pl_11);
        Expect_isTrue(Polyline2D__get_IsClosed(pl_11))("should be closed after closeInPlace");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closeInPlace on nearly closed snaps", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let pl_12;
        const points_11 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 1E-08)];
        pl_12 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_11));
        Polyline2D_closeInPlace_Z5A89AEF5(pl_12);
        let d_15;
        const a_32 = item(0, Polyline2D__get_Points(pl_12));
        const b_32 = item(Polyline2D__get_PointCount(pl_12) - 1, Polyline2D__get_Points(pl_12));
        const vx_15 = a_32.X - b_32.X;
        const vy_15 = a_32.Y - b_32.Y;
        d_15 = Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15));
        const r_15 = d_15 < 1E-09;
        const actual_26 = r_15;
        if ((actual_26 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, true, "last snapped to first");
        }
        else {
            let valueType_26;
            let copyOfStruct_33 = actual_26;
            valueType_26 = bool_type;
            const primitiveTypes_26 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_26;
            if (contains(valueType_26, primitiveTypes_26, {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            })) {
                const arg_31 = toString(true);
                const arg_1_26 = toString(actual_26);
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("last snapped to first");
            }
            else {
                errorMsg_26 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_26)("last snapped to first");
            }
            throw new Exception(errorMsg_26);
        }
        const actual_27 = Polyline2D__get_PointCount(pl_12) | 0;
        if ((actual_27 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, 4, "no extra point, just snapped");
        }
        else {
            let valueType_27;
            let copyOfStruct_34 = actual_27;
            valueType_27 = int32_type;
            const primitiveTypes_27 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_27;
            if (contains(valueType_27, primitiveTypes_27, {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            })) {
                const arg_32 = int32ToString(4);
                const arg_1_27 = int32ToString(actual_27);
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("no extra point, just snapped");
            }
            else {
                errorMsg_27 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_27)("no extra point, just snapped");
            }
            throw new Exception(errorMsg_27);
        }
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})()])), Test_testList("Static equals", ofArray([(() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("same polylines are equal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        Expect_isTrue(Polyline2D_equals(1E-09, plOpen, Polyline2D__Clone(plOpen)))("clones should be equal");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("different polylines are not equal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        Expect_isFalse(Polyline2D_equals(1E-09, plOpen, plClosed))("open vs closed not equal");
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("almost equal within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let pl1;
        const points_12 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)];
        pl1 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_12));
        let pl2;
        const points_13 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1.0001, 0)];
        pl2 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_13));
        Expect_isTrue(Polyline2D_equals(0.001, pl1, pl2))("within tolerance");
        Expect_isFalse(Polyline2D_equals(1E-05, pl1, pl2))("outside tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("different counts not equal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let pl1_1;
        const points_14 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)];
        pl1_1 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_14));
        let pl2_1;
        const points_15 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)];
        pl2_1 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_15));
        Expect_isFalse(Polyline2D_equals(1E-09, pl1_1, pl2_1))("different counts");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})()])), Test_testList("Static rotateWithCenter", singleton((() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotate 90 degrees around center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let r_16;
        const rad = 0.017453292519943295 * 90;
        r_16 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad), Math.cos(rad));
        const cen = Pt_$ctor_7B00E9A0(5, 5);
        const rotated = Polyline2D_rotateWithCenter(cen, r_16, plLine);
        let d_16;
        const a_39 = item(0, Polyline2D__get_Points(rotated));
        const b_39 = Pt_$ctor_7B00E9A0(10, 0);
        const vx_16 = a_39.X - b_39.X;
        const vy_16 = a_39.Y - b_39.Y;
        d_16 = Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16));
        const r_18 = d_16 < 1E-09;
        const actual_28 = r_18;
        if ((actual_28 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, true, "rotated first point");
        }
        else {
            let valueType_28;
            let copyOfStruct_35 = actual_28;
            valueType_28 = bool_type;
            const primitiveTypes_28 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_28;
            if (contains(valueType_28, primitiveTypes_28, {
                Equals: equals,
                GetHashCode: (x_28) => (structuralHash(x_28) | 0),
            })) {
                const arg_33 = toString(true);
                const arg_1_28 = toString(actual_28);
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("rotated first point");
            }
            else {
                errorMsg_28 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_28)("rotated first point");
            }
            throw new Exception(errorMsg_28);
        }
        let d_17;
        const a_41 = item(1, Polyline2D__get_Points(rotated));
        const b_41 = Pt_$ctor_7B00E9A0(10, 10);
        const vx_17 = a_41.X - b_41.X;
        const vy_17 = a_41.Y - b_41.Y;
        d_17 = Math.sqrt((vx_17 * vx_17) + (vy_17 * vy_17));
        const r_19 = d_17 < 1E-09;
        const actual_29 = r_19;
        if ((actual_29 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, true, "rotated second point");
        }
        else {
            let valueType_29;
            let copyOfStruct_36 = actual_29;
            valueType_29 = bool_type;
            const primitiveTypes_29 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_29;
            if (contains(valueType_29, primitiveTypes_29, {
                Equals: equals,
                GetHashCode: (x_29) => (structuralHash(x_29) | 0),
            })) {
                const arg_34 = toString(true);
                const arg_1_29 = toString(actual_29);
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)("rotated second point");
            }
            else {
                errorMsg_29 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_29)("rotated second point");
            }
            throw new Exception(errorMsg_29);
        }
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})())), Test_testList("Static wrappers", ofArray([(() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let d_18;
        const a_43 = Polyline2D_start_Z5A89AEF5(plOpen);
        const b_43 = Polyline2D__get_Start(plOpen);
        const vx_18 = a_43.X - b_43.X;
        const vy_18 = a_43.Y - b_43.Y;
        d_18 = Math.sqrt((vx_18 * vx_18) + (vy_18 * vy_18));
        const r_20 = d_18 < 1E-09;
        const actual_30 = r_20;
        if ((actual_30 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, true, "static start");
        }
        else {
            let valueType_30;
            let copyOfStruct_37 = actual_30;
            valueType_30 = bool_type;
            const primitiveTypes_30 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_30;
            if (contains(valueType_30, primitiveTypes_30, {
                Equals: equals,
                GetHashCode: (x_30) => (structuralHash(x_30) | 0),
            })) {
                const arg_35 = toString(true);
                const arg_1_30 = toString(actual_30);
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("static start");
            }
            else {
                errorMsg_30 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_30)("static start");
            }
            throw new Exception(errorMsg_30);
        }
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static ende", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let d_19;
        const a_45 = Polyline2D_ende_Z5A89AEF5(plOpen);
        const b_45 = Polyline2D__get_End(plOpen);
        const vx_19 = a_45.X - b_45.X;
        const vy_19 = a_45.Y - b_45.Y;
        d_19 = Math.sqrt((vx_19 * vx_19) + (vy_19 * vy_19));
        const r_21 = d_19 < 1E-09;
        const actual_31 = r_21;
        if ((actual_31 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, true, "static ende");
        }
        else {
            let valueType_31;
            let copyOfStruct_38 = actual_31;
            valueType_31 = bool_type;
            const primitiveTypes_31 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_31;
            if (contains(valueType_31, primitiveTypes_31, {
                Equals: equals,
                GetHashCode: (x_31) => (structuralHash(x_31) | 0),
            })) {
                const arg_36 = toString(true);
                const arg_1_31 = toString(actual_31);
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("static ende");
            }
            else {
                errorMsg_31 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_31)("static ende");
            }
            throw new Exception(errorMsg_31);
        }
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        Expect_isTrue(Math.abs(Polyline2D__get_Length_1(plOpen) - Polyline2D__get_Length(plOpen)) < 1E-09)("static length");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static pointCount", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        const actual_32 = Polyline2D__get_Points_1(plOpen).length | 0;
        const expected_52 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_32 === expected_52) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, expected_52, "static pointCount");
        }
        else {
            let valueType_32;
            let copyOfStruct_39 = actual_32;
            valueType_32 = int32_type;
            const primitiveTypes_32 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_32;
            if (contains(valueType_32, primitiveTypes_32, {
                Equals: equals,
                GetHashCode: (x_32) => (structuralHash(x_32) | 0),
            })) {
                const arg_37 = int32ToString(expected_52);
                const arg_1_32 = int32ToString(actual_32);
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_32)("static pointCount");
            }
            else {
                errorMsg_32 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_52)(actual_32)("static pointCount");
            }
            throw new Exception(errorMsg_32);
        }
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static segmentCount", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        const actual_33 = Polyline2D__get_SegmentCount_1(plOpen) | 0;
        const expected_53 = Polyline2D__get_SegmentCount(plOpen) | 0;
        if ((actual_33 === expected_53) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, expected_53, "static segmentCount");
        }
        else {
            let valueType_33;
            let copyOfStruct_40 = actual_33;
            valueType_33 = int32_type;
            const primitiveTypes_33 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_33;
            if (contains(valueType_33, primitiveTypes_33, {
                Equals: equals,
                GetHashCode: (x_33) => (structuralHash(x_33) | 0),
            })) {
                const arg_38 = int32ToString(expected_53);
                const arg_1_33 = int32ToString(actual_33);
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_38)(arg_1_33)("static segmentCount");
            }
            else {
                errorMsg_33 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_53)(actual_33)("static segmentCount");
            }
            throw new Exception(errorMsg_33);
        }
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static reverseInPlace", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        const copy = Polyline2D__Clone(plOpen);
        Polyline2D_reverseInPlace_Z5A89AEF5(copy);
        let d_20;
        const a_47 = item(0, Polyline2D__get_Points(copy));
        const b_47 = Pt_$ctor_7B00E9A0(0, 10);
        const vx_20 = a_47.X - b_47.X;
        const vy_20 = a_47.Y - b_47.Y;
        d_20 = Math.sqrt((vx_20 * vx_20) + (vy_20 * vy_20));
        const r_22 = d_20 < 1E-09;
        const actual_34 = r_22;
        if ((actual_34 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, true, "first after reverse");
        }
        else {
            let valueType_34;
            let copyOfStruct_41 = actual_34;
            valueType_34 = bool_type;
            const primitiveTypes_34 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_34;
            if (contains(valueType_34, primitiveTypes_34, {
                Equals: equals,
                GetHashCode: (x_34) => (structuralHash(x_34) | 0),
            })) {
                const arg_39 = toString(true);
                const arg_1_34 = toString(actual_34);
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_39)(arg_1_34)("first after reverse");
            }
            else {
                errorMsg_34 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_34)("first after reverse");
            }
            throw new Exception(errorMsg_34);
        }
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static reverse", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        const rev = Polyline2D_reverse_Z5A89AEF5(plOpen);
        let d_21;
        const a_49 = item(0, Polyline2D__get_Points(rev));
        const b_49 = Pt_$ctor_7B00E9A0(0, 10);
        const vx_21 = a_49.X - b_49.X;
        const vy_21 = a_49.Y - b_49.Y;
        d_21 = Math.sqrt((vx_21 * vx_21) + (vy_21 * vy_21));
        const r_23 = d_21 < 1E-09;
        const actual_35 = r_23;
        if ((actual_35 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, true, "first after reverse");
        }
        else {
            let valueType_35;
            let copyOfStruct_42 = actual_35;
            valueType_35 = bool_type;
            const primitiveTypes_35 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_35;
            if (contains(valueType_35, primitiveTypes_35, {
                Equals: equals,
                GetHashCode: (x_35) => (structuralHash(x_35) | 0),
            })) {
                const arg_40 = toString(true);
                const arg_1_35 = toString(actual_35);
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_40)(arg_1_35)("first after reverse");
            }
            else {
                errorMsg_35 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_35)("first after reverse");
            }
            throw new Exception(errorMsg_35);
        }
        let d_22;
        const a_51 = item(0, Polyline2D__get_Points(plOpen));
        const b_51 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_22 = a_51.X - b_51.X;
        const vy_22 = a_51.Y - b_51.Y;
        d_22 = Math.sqrt((vx_22 * vx_22) + (vy_22 * vy_22));
        const r_24 = d_22 < 1E-09;
        const actual_36 = r_24;
        if ((actual_36 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, true, "original unchanged");
        }
        else {
            let valueType_36;
            let copyOfStruct_43 = actual_36;
            valueType_36 = bool_type;
            const primitiveTypes_36 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_36;
            if (contains(valueType_36, primitiveTypes_36, {
                Equals: equals,
                GetHashCode: (x_36) => (structuralHash(x_36) | 0),
            })) {
                const arg_41 = toString(true);
                const arg_1_36 = toString(actual_36);
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_41)(arg_1_36)("original unchanged");
            }
            else {
                errorMsg_36 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_36)("original unchanged");
            }
            throw new Exception(errorMsg_36);
        }
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static evaluateAt", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        const pt_5 = Polyline2D_evaluateAt(0.5, plOpen);
        let d_23;
        const a_53 = pt_5;
        const b_53 = Pt_$ctor_7B00E9A0(5, 0);
        const vx_23 = a_53.X - b_53.X;
        const vy_23 = a_53.Y - b_53.Y;
        d_23 = Math.sqrt((vx_23 * vx_23) + (vy_23 * vy_23));
        const r_25 = d_23 < 1E-09;
        const actual_37 = r_25;
        if ((actual_37 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, true, "static evaluateAt");
        }
        else {
            let valueType_37;
            let copyOfStruct_44 = actual_37;
            valueType_37 = bool_type;
            const primitiveTypes_37 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_37;
            if (contains(valueType_37, primitiveTypes_37, {
                Equals: equals,
                GetHashCode: (x_37) => (structuralHash(x_37) | 0),
            })) {
                const arg_42 = toString(true);
                const arg_1_37 = toString(actual_37);
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_42)(arg_1_37)("static evaluateAt");
            }
            else {
                errorMsg_37 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_37)("static evaluateAt");
            }
            throw new Exception(errorMsg_37);
        }
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static closestParameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        const p_3 = Polyline2D__ClosestParameter_6ADE94FD_1(plOpen, Pt_$ctor_7B00E9A0(5, -1));
        Expect_isTrue(Math.abs(p_3 - 0.5) < 1E-09)("static closestParameter");
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static closestPoint", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        const pt_10 = Polyline2D__ClosestPoint_6ADE94FD_1(plOpen, Pt_$ctor_7B00E9A0(5, -1));
        let d_24;
        const a_55 = pt_10;
        const b_55 = Pt_$ctor_7B00E9A0(5, 0);
        const vx_24 = a_55.X - b_55.X;
        const vy_24 = a_55.Y - b_55.Y;
        d_24 = Math.sqrt((vx_24 * vx_24) + (vy_24 * vy_24));
        const r_26 = d_24 < 1E-09;
        const actual_38 = r_26;
        if ((actual_38 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, true, "static closestPoint");
        }
        else {
            let valueType_38;
            let copyOfStruct_45 = actual_38;
            valueType_38 = bool_type;
            const primitiveTypes_38 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_38;
            if (contains(valueType_38, primitiveTypes_38, {
                Equals: equals,
                GetHashCode: (x_38) => (structuralHash(x_38) | 0),
            })) {
                const arg_43 = toString(true);
                const arg_1_38 = toString(actual_38);
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_43)(arg_1_38)("static closestPoint");
            }
            else {
                errorMsg_38 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_38)("static closestPoint");
            }
            throw new Exception(errorMsg_38);
        }
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static closestVertex", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        const idx_5 = Polyline2D__ClosestVertex_6ADE94FD(plOpen, Pt_$ctor_7B00E9A0(9, 0.5)) | 0;
        const actual_39 = idx_5 | 0;
        if ((actual_39 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_39, 1, "static closestVertex nearest to (10,0)");
        }
        else {
            let valueType_39;
            let copyOfStruct_46 = actual_39;
            valueType_39 = int32_type;
            const primitiveTypes_39 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_39;
            if (contains(valueType_39, primitiveTypes_39, {
                Equals: equals,
                GetHashCode: (x_39) => (structuralHash(x_39) | 0),
            })) {
                const arg_44 = int32ToString(1);
                const arg_1_39 = int32ToString(actual_39);
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_44)(arg_1_39)("static closestVertex nearest to (10,0)");
            }
            else {
                errorMsg_39 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_39)("static closestVertex nearest to (10,0)");
            }
            throw new Exception(errorMsg_39);
        }
        Test_TestCaseBuilder__Zero(builder$0040_50);
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static distanceTo", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        const d_25 = Polyline2D__DistanceTo_6ADE94FD_1(plOpen, Pt_$ctor_7B00E9A0(5, -3));
        Expect_isTrue(Math.abs(d_25 - 3) < 1E-09)("static distanceTo");
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static scale", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        const s_5 = Polyline2D_scale(3, plLine);
        let d_26;
        const a_57 = item(1, Polyline2D__get_Points(s_5));
        const b_57 = Pt_$ctor_7B00E9A0(30, 0);
        const vx_25 = a_57.X - b_57.X;
        const vy_25 = a_57.Y - b_57.Y;
        d_26 = Math.sqrt((vx_25 * vx_25) + (vy_25 * vy_25));
        const r_27 = d_26 < 1E-09;
        const actual_40 = r_27;
        if ((actual_40 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_40, true, "static scale");
        }
        else {
            let valueType_40;
            let copyOfStruct_47 = actual_40;
            valueType_40 = bool_type;
            const primitiveTypes_40 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_40;
            if (contains(valueType_40, primitiveTypes_40, {
                Equals: equals,
                GetHashCode: (x_40) => (structuralHash(x_40) | 0),
            })) {
                const arg_45 = toString(true);
                const arg_1_40 = toString(actual_40);
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_45)(arg_1_40)("static scale");
            }
            else {
                errorMsg_40 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_40)("static scale");
            }
            throw new Exception(errorMsg_40);
        }
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static windingNumber", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        const wn = Polyline2D__WindingNumber_6ADE94FD_1(plClosed, Pt_$ctor_7B00E9A0(5, 5)) | 0;
        Expect_isTrue(wn !== 0)("static windingNumber inside");
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static contains", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        Expect_isTrue(Polyline2D__Contains_6ADE94FD_1(plClosed, Pt_$ctor_7B00E9A0(5, 5)))("static contains inside");
        Expect_isFalse(Polyline2D__Contains_6ADE94FD_1(plClosed, Pt_$ctor_7B00E9A0(15, 5)))("static contains outside");
        Test_TestCaseBuilder__Zero(builder$0040_54);
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static start fails on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        Expect_throws(() => {
            Polyline2D_start_Z5A89AEF5(plEmpty);
        }, "start on empty");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static ende fails on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        Expect_throws(() => {
            Polyline2D_ende_Z5A89AEF5(plEmpty);
        }, "ende on empty");
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})()])), Test_testList("createDirectlyUnsafe and pointsUnsafeInternal", ofArray([(() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createDirectlyUnsafe shares array", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        const ra = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 1)];
        const pl_30 = Polyline2D_createDirectlyUnsafe_Z5FD8CF3C(ra);
        void (ra.push(Pt_$ctor_7B00E9A0(2, 2)));
        const actual_41 = Polyline2D__get_PointCount(pl_30) | 0;
        if ((actual_41 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_41, 3, "shared array reflects changes");
        }
        else {
            let valueType_41;
            let copyOfStruct_48 = actual_41;
            valueType_41 = int32_type;
            const primitiveTypes_41 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_41;
            if (contains(valueType_41, primitiveTypes_41, {
                Equals: equals,
                GetHashCode: (x_41) => (structuralHash(x_41) | 0),
            })) {
                const arg_46 = int32ToString(3);
                const arg_1_41 = int32ToString(actual_41);
                errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_46)(arg_1_41)("shared array reflects changes");
            }
            else {
                errorMsg_41 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_41)("shared array reflects changes");
            }
            throw new Exception(errorMsg_41);
        }
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointsUnsafeInternal returns same reference", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let pl_31;
        const points_16 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 1)];
        pl_31 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_16));
        const pts = Polyline2D_pointsUnsafeInternal_Z5A89AEF5(pl_31);
        const actual_42 = pts.length | 0;
        if ((actual_42 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_42, 2, "same count");
        }
        else {
            let valueType_42;
            let copyOfStruct_49 = actual_42;
            valueType_42 = int32_type;
            const primitiveTypes_42 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_42;
            if (contains(valueType_42, primitiveTypes_42, {
                Equals: equals,
                GetHashCode: (x_42) => (structuralHash(x_42) | 0),
            })) {
                const arg_47 = int32ToString(2);
                const arg_1_42 = int32ToString(actual_42);
                errorMsg_42 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_47)(arg_1_42)("same count");
            }
            else {
                errorMsg_42 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_42)("same count");
            }
            throw new Exception(errorMsg_42);
        }
        void (pts.push(Pt_$ctor_7B00E9A0(2, 2)));
        const actual_43 = Polyline2D__get_PointCount(pl_31) | 0;
        if ((actual_43 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_43, 3, "mutation reflected in polyline");
        }
        else {
            let valueType_43;
            let copyOfStruct_50 = actual_43;
            valueType_43 = int32_type;
            const primitiveTypes_43 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_43;
            if (contains(valueType_43, primitiveTypes_43, {
                Equals: equals,
                GetHashCode: (x_43) => (structuralHash(x_43) | 0),
            })) {
                const arg_48 = int32ToString(3);
                const arg_1_43 = int32ToString(actual_43);
                errorMsg_43 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_48)(arg_1_43)("mutation reflected in polyline");
            }
            else {
                errorMsg_43 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_43)("mutation reflected in polyline");
            }
            throw new Exception(errorMsg_43);
        }
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})()])), Test_testList("EvaluateAt edge cases", ofArray([(() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("evaluate at near-integer snaps to vertex", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let pl_32;
        const points_17 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10)];
        pl_32 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_17));
        const pt_19 = Polyline2D__EvaluateAt_5E38073B(pl_32, 0.9999999);
        let d_27;
        const a_59 = pt_19;
        const b_59 = Pt_$ctor_7B00E9A0(10, 0);
        const vx_26 = a_59.X - b_59.X;
        const vy_26 = a_59.Y - b_59.Y;
        d_27 = Math.sqrt((vx_26 * vx_26) + (vy_26 * vy_26));
        const r_28 = d_27 < 1E-09;
        const actual_44 = r_28;
        if ((actual_44 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_44, true, "near integer snaps");
        }
        else {
            let valueType_44;
            let copyOfStruct_51 = actual_44;
            valueType_44 = bool_type;
            const primitiveTypes_44 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_44;
            if (contains(valueType_44, primitiveTypes_44, {
                Equals: equals,
                GetHashCode: (x_44) => (structuralHash(x_44) | 0),
            })) {
                const arg_49 = toString(true);
                const arg_1_44 = toString(actual_44);
                errorMsg_44 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_49)(arg_1_44)("near integer snaps");
            }
            else {
                errorMsg_44 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_44)("near integer snaps");
            }
            throw new Exception(errorMsg_44);
        }
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("evaluate at exactly last parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let pl_33;
        const points_18 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10)];
        pl_33 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_18));
        const pt_20 = Polyline2D__EvaluateAt_5E38073B(pl_33, 2);
        let d_28;
        const a_61 = pt_20;
        const b_61 = Pt_$ctor_7B00E9A0(10, 10);
        const vx_27 = a_61.X - b_61.X;
        const vy_27 = a_61.Y - b_61.Y;
        d_28 = Math.sqrt((vx_27 * vx_27) + (vy_27 * vy_27));
        const r_29 = d_28 < 1E-09;
        const actual_45 = r_29;
        if ((actual_45 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_45, true, "at last");
        }
        else {
            let valueType_45;
            let copyOfStruct_52 = actual_45;
            valueType_45 = bool_type;
            const primitiveTypes_45 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_45;
            if (contains(valueType_45, primitiveTypes_45, {
                Equals: equals,
                GetHashCode: (x_45) => (structuralHash(x_45) | 0),
            })) {
                const arg_50 = toString(true);
                const arg_1_45 = toString(actual_45);
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_50)(arg_1_45)("at last");
            }
            else {
                errorMsg_45 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_45)("at last");
            }
            throw new Exception(errorMsg_45);
        }
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("evaluate at near zero negative", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let pl_34;
        const points_19 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0)];
        pl_34 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_19));
        const pt_21 = Polyline2D__EvaluateAt_5E38073B(pl_34, -1E-07);
        let d_29;
        const a_63 = pt_21;
        const b_63 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_28 = a_63.X - b_63.X;
        const vy_28 = a_63.Y - b_63.Y;
        d_29 = Math.sqrt((vx_28 * vx_28) + (vy_28 * vy_28));
        const r_30 = d_29 < 1E-09;
        const actual_46 = r_30;
        if ((actual_46 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_46, true, "near zero negative");
        }
        else {
            let valueType_46;
            let copyOfStruct_53 = actual_46;
            valueType_46 = bool_type;
            const primitiveTypes_46 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_46;
            if (contains(valueType_46, primitiveTypes_46, {
                Equals: equals,
                GetHashCode: (x_46) => (structuralHash(x_46) | 0),
            })) {
                const arg_51 = toString(true);
                const arg_1_46 = toString(actual_46);
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_51)(arg_1_46)("near zero negative");
            }
            else {
                errorMsg_46 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_46)("near zero negative");
            }
            throw new Exception(errorMsg_46);
        }
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("evaluate at beyond end throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        let pl_35;
        const points_20 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0)];
        pl_35 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_20));
        Expect_throws(() => {
            Polyline2D__EvaluateAt_5E38073B(pl_35, 1.01);
        }, "beyond end throws");
        Test_TestCaseBuilder__Zero(builder$0040_62);
    }));
})()])), Test_testList("ClosestPoint edge cases", ofArray([(() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closest point on vertex", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        const cp = Polyline2D__ClosestPoint_6ADE94FD(plOpen, Pt_$ctor_7B00E9A0(10, 0));
        let d_30;
        const a_65 = cp;
        const b_65 = Pt_$ctor_7B00E9A0(10, 0);
        const vx_29 = a_65.X - b_65.X;
        const vy_29 = a_65.Y - b_65.Y;
        d_30 = Math.sqrt((vx_29 * vx_29) + (vy_29 * vy_29));
        const r_31 = d_30 < 1E-09;
        const actual_47 = r_31;
        if ((actual_47 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_47, true, "on vertex");
        }
        else {
            let valueType_47;
            let copyOfStruct_54 = actual_47;
            valueType_47 = bool_type;
            const primitiveTypes_47 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_47;
            if (contains(valueType_47, primitiveTypes_47, {
                Equals: equals,
                GetHashCode: (x_47) => (structuralHash(x_47) | 0),
            })) {
                const arg_52 = toString(true);
                const arg_1_47 = toString(actual_47);
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_52)(arg_1_47)("on vertex");
            }
            else {
                errorMsg_47 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_47)("on vertex");
            }
            throw new Exception(errorMsg_47);
        }
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closest point single point polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        const cp_1 = Polyline2D__ClosestPoint_6ADE94FD(plSinglePoint, Pt_$ctor_7B00E9A0(5, 5));
        let d_31;
        const a_67 = cp_1;
        const b_67 = Pt_$ctor_7B00E9A0(1, 1);
        const vx_30 = a_67.X - b_67.X;
        const vy_30 = a_67.Y - b_67.Y;
        d_31 = Math.sqrt((vx_30 * vx_30) + (vy_30 * vy_30));
        const r_32 = d_31 < 1E-09;
        const actual_48 = r_32;
        if ((actual_48 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_48, true, "single point closest");
        }
        else {
            let valueType_48;
            let copyOfStruct_55 = actual_48;
            valueType_48 = bool_type;
            const primitiveTypes_48 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_48;
            if (contains(valueType_48, primitiveTypes_48, {
                Equals: equals,
                GetHashCode: (x_48) => (structuralHash(x_48) | 0),
            })) {
                const arg_53 = toString(true);
                const arg_1_48 = toString(actual_48);
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_53)(arg_1_48)("single point closest");
            }
            else {
                errorMsg_48 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_48)("single point closest");
            }
            throw new Exception(errorMsg_48);
        }
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closest parameter single point polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        const cp_2 = Polyline2D__ClosestParameter_6ADE94FD(plSinglePoint, Pt_$ctor_7B00E9A0(5, 5));
        Expect_isTrue(Math.abs(cp_2) < 1E-09)("single point param is 0");
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})(), (() => {
    const builder$0040_66 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closest vertex single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_66, Test_TestCaseBuilder__Delay_1505(builder$0040_66, () => {
        const idx_6 = Polyline2D__ClosestVertex_6ADE94FD_1(plSinglePoint, Pt_$ctor_7B00E9A0(5, 5)) | 0;
        const actual_49 = idx_6 | 0;
        if ((actual_49 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_49, 0, "single point vertex is 0");
        }
        else {
            let valueType_49;
            let copyOfStruct_56 = actual_49;
            valueType_49 = int32_type;
            const primitiveTypes_49 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_49;
            if (contains(valueType_49, primitiveTypes_49, {
                Equals: equals,
                GetHashCode: (x_49) => (structuralHash(x_49) | 0),
            })) {
                const arg_54 = int32ToString(0);
                const arg_1_49 = int32ToString(actual_49);
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_54)(arg_1_49)("single point vertex is 0");
            }
            else {
                errorMsg_49 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_49)("single point vertex is 0");
            }
            throw new Exception(errorMsg_49);
        }
        Test_TestCaseBuilder__Zero(builder$0040_66);
    }));
})(), (() => {
    const builder$0040_67 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("distance to single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_67, Test_TestCaseBuilder__Delay_1505(builder$0040_67, () => {
        const d_32 = Polyline2D__DistanceTo_6ADE94FD(plSinglePoint, Pt_$ctor_7B00E9A0(4, 1));
        Expect_isTrue(Math.abs(d_32 - 3) < 1E-09)("distance to single point");
        Test_TestCaseBuilder__Zero(builder$0040_67);
    }));
})(), (() => {
    const builder$0040_68 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closest point/parameter/vertex fails on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_68, Test_TestCaseBuilder__Delay_1505(builder$0040_68, () => {
        Expect_throws(() => {
            Polyline2D__ClosestPoint_6ADE94FD(plEmpty, Pt_$ctor_7B00E9A0(0, 0));
        }, "closest point empty");
        Expect_throws(() => {
            Polyline2D__ClosestParameter_6ADE94FD(plEmpty, Pt_$ctor_7B00E9A0(0, 0));
        }, "closest parameter empty");
        Expect_throws(() => {
            Polyline2D__ClosestVertex_6ADE94FD_1(plEmpty, Pt_$ctor_7B00E9A0(0, 0));
        }, "closest vertex empty");
        Expect_throws(() => {
            Polyline2D__DistanceTo_6ADE94FD(plEmpty, Pt_$ctor_7B00E9A0(0, 0));
        }, "distance to empty");
        Test_TestCaseBuilder__Zero(builder$0040_68);
    }));
})()])), Test_testList("Contains edge cases", singleton((() => {
    const builder$0040_69 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("contains with less than 3 points always false", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_69, Test_TestCaseBuilder__Delay_1505(builder$0040_69, () => {
        Expect_isFalse(Polyline2D__Contains_6ADE94FD(plLine, Pt_$ctor_7B00E9A0(5, 0)))("line segment contains false");
        Expect_isFalse(Polyline2D__Contains_6ADE94FD(plSinglePoint, Pt_$ctor_7B00E9A0(1, 1)))("single point contains false");
        Expect_isFalse(Polyline2D__Contains_6ADE94FD(plEmpty, Pt_$ctor_7B00E9A0(0, 0)))("empty contains false");
        Test_TestCaseBuilder__Zero(builder$0040_69);
    }));
})())), Test_testList("Area and orientation edge cases", ofArray([(() => {
    const builder$0040_70 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("area fails on open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_70, Test_TestCaseBuilder__Delay_1505(builder$0040_70, () => {
        Expect_throws(() => {
            Polyline2D__get_Area(plOpen);
        }, "area on open throws");
        Test_TestCaseBuilder__Zero(builder$0040_70);
    }));
})(), (() => {
    const builder$0040_71 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isCounterClockwise fails on zero area", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_71, Test_TestCaseBuilder__Delay_1505(builder$0040_71, () => {
        let pl_36;
        const points_21 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(0, 0)];
        pl_36 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_21));
        Expect_throws(() => {
            Polyline2D__get_IsCounterClockwise(pl_36);
        }, "zero area ccw throws");
        Test_TestCaseBuilder__Zero(builder$0040_71);
    }));
})(), (() => {
    const builder$0040_72 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isClockwise fails on zero area", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_72, Test_TestCaseBuilder__Delay_1505(builder$0040_72, () => {
        let pl_37;
        const points_22 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(0, 0)];
        pl_37 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_22));
        Expect_throws(() => {
            Polyline2D__get_IsClockwise(pl_37);
        }, "zero area cw throws");
        Test_TestCaseBuilder__Zero(builder$0040_72);
    }));
})(), (() => {
    const builder$0040_73 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("clockwise polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_73, Test_TestCaseBuilder__Delay_1505(builder$0040_73, () => {
        let pl_38;
        const points_23 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(0, 0)];
        pl_38 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_23));
        Expect_isTrue(Polyline2D__get_IsClockwise(pl_38))("CW square");
        Expect_isFalse(Polyline2D__get_IsCounterClockwise(pl_38))("CW square not CCW");
        Expect_isTrue(Polyline2D__get_SignedArea(pl_38) < 0)("negative signed area");
        Test_TestCaseBuilder__Zero(builder$0040_73);
    }));
})()])), Test_testList("Length edge cases", ofArray([(() => {
    const builder$0040_74 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("length of single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_74, Test_TestCaseBuilder__Delay_1505(builder$0040_74, () => {
        const actual_50 = Polyline2D__get_Length(plSinglePoint);
        if ((actual_50 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_50, 0, "single point length is 0");
        }
        else {
            let valueType_50;
            let copyOfStruct_57 = actual_50;
            valueType_50 = float64_type;
            const primitiveTypes_50 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_50;
            if (contains(valueType_50, primitiveTypes_50, {
                Equals: equals,
                GetHashCode: (x_50) => (structuralHash(x_50) | 0),
            })) {
                const arg_55 = (0).toString();
                const arg_1_50 = actual_50.toString();
                errorMsg_50 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_55)(arg_1_50)("single point length is 0");
            }
            else {
                errorMsg_50 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_50)("single point length is 0");
            }
            throw new Exception(errorMsg_50);
        }
        Test_TestCaseBuilder__Zero(builder$0040_74);
    }));
})(), (() => {
    const builder$0040_75 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closed polyline length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_75, Test_TestCaseBuilder__Delay_1505(builder$0040_75, () => {
        Expect_isTrue(Math.abs(Polyline2D__get_Length(plClosed) - 40) < 1E-09)("closed length");
        Test_TestCaseBuilder__Zero(builder$0040_75);
    }));
})()])), Test_testList("Center edge cases", ofArray([(() => {
    const builder$0040_76 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("center of single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_76, Test_TestCaseBuilder__Delay_1505(builder$0040_76, () => {
        let d_33;
        const a_69 = Polyline2D__get_Center(plSinglePoint);
        const b_69 = Pt_$ctor_7B00E9A0(1, 1);
        const vx_31 = a_69.X - b_69.X;
        const vy_31 = a_69.Y - b_69.Y;
        d_33 = Math.sqrt((vx_31 * vx_31) + (vy_31 * vy_31));
        const r_33 = d_33 < 1E-09;
        const actual_51 = r_33;
        if ((actual_51 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_51, true, "single point center");
        }
        else {
            let valueType_51;
            let copyOfStruct_58 = actual_51;
            valueType_51 = bool_type;
            const primitiveTypes_51 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_51;
            if (contains(valueType_51, primitiveTypes_51, {
                Equals: equals,
                GetHashCode: (x_51) => (structuralHash(x_51) | 0),
            })) {
                const arg_56 = toString(true);
                const arg_1_51 = toString(actual_51);
                errorMsg_51 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_56)(arg_1_51)("single point center");
            }
            else {
                errorMsg_51 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_51)("single point center");
            }
            throw new Exception(errorMsg_51);
        }
        Test_TestCaseBuilder__Zero(builder$0040_76);
    }));
})(), (() => {
    const builder$0040_77 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("center fails on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_77, Test_TestCaseBuilder__Delay_1505(builder$0040_77, () => {
        Expect_throws(() => {
            Polyline2D__get_Center(plEmpty);
        }, "center on empty");
        Test_TestCaseBuilder__Zero(builder$0040_77);
    }));
})()])), Test_testList("subPolyline edge cases", ofArray([(() => {
    const builder$0040_78 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("sub polyline reversed when a > b", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_78, Test_TestCaseBuilder__Delay_1505(builder$0040_78, () => {
        const sub = Polyline2D_subPolyline(2.5, 0.5, plOpen);
        let d_34;
        const a_72 = item(0, Polyline2D__get_Points(sub));
        const b_72 = Pt_$ctor_7B00E9A0(5, 10);
        const vx_32 = a_72.X - b_72.X;
        const vy_32 = a_72.Y - b_72.Y;
        d_34 = Math.sqrt((vx_32 * vx_32) + (vy_32 * vy_32));
        const r_34 = d_34 < 1E-09;
        const actual_52 = r_34;
        if ((actual_52 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_52, true, "reversed sub start");
        }
        else {
            let valueType_52;
            let copyOfStruct_59 = actual_52;
            valueType_52 = bool_type;
            const primitiveTypes_52 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_52;
            if (contains(valueType_52, primitiveTypes_52, {
                Equals: equals,
                GetHashCode: (x_52) => (structuralHash(x_52) | 0),
            })) {
                const arg_57 = toString(true);
                const arg_1_52 = toString(actual_52);
                errorMsg_52 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_57)(arg_1_52)("reversed sub start");
            }
            else {
                errorMsg_52 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_52)("reversed sub start");
            }
            throw new Exception(errorMsg_52);
        }
        let d_35;
        const a_74 = item(Polyline2D__get_PointCount(sub) - 1, Polyline2D__get_Points(sub));
        const b_74 = Pt_$ctor_7B00E9A0(5, 0);
        const vx_33 = a_74.X - b_74.X;
        const vy_33 = a_74.Y - b_74.Y;
        d_35 = Math.sqrt((vx_33 * vx_33) + (vy_33 * vy_33));
        const r_35 = d_35 < 1E-09;
        const actual_53 = r_35;
        if ((actual_53 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_53, true, "reversed sub end");
        }
        else {
            let valueType_53;
            let copyOfStruct_60 = actual_53;
            valueType_53 = bool_type;
            const primitiveTypes_53 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_53;
            if (contains(valueType_53, primitiveTypes_53, {
                Equals: equals,
                GetHashCode: (x_53) => (structuralHash(x_53) | 0),
            })) {
                const arg_58 = toString(true);
                const arg_1_53 = toString(actual_53);
                errorMsg_53 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_58)(arg_1_53)("reversed sub end");
            }
            else {
                errorMsg_53 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_53)("reversed sub end");
            }
            throw new Exception(errorMsg_53);
        }
        Test_TestCaseBuilder__Zero(builder$0040_78);
    }));
})(), (() => {
    const builder$0040_79 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("sub polyline at integer params", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_79, Test_TestCaseBuilder__Delay_1505(builder$0040_79, () => {
        const sub_1 = Polyline2D_subPolyline(1, 2, plOpen);
        let d_36;
        const a_77 = item(0, Polyline2D__get_Points(sub_1));
        const b_77 = Pt_$ctor_7B00E9A0(10, 0);
        const vx_34 = a_77.X - b_77.X;
        const vy_34 = a_77.Y - b_77.Y;
        d_36 = Math.sqrt((vx_34 * vx_34) + (vy_34 * vy_34));
        const r_36 = d_36 < 1E-09;
        const actual_54 = r_36;
        if ((actual_54 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_54, true, "integer start");
        }
        else {
            let valueType_54;
            let copyOfStruct_61 = actual_54;
            valueType_54 = bool_type;
            const primitiveTypes_54 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_54;
            if (contains(valueType_54, primitiveTypes_54, {
                Equals: equals,
                GetHashCode: (x_54) => (structuralHash(x_54) | 0),
            })) {
                const arg_59 = toString(true);
                const arg_1_54 = toString(actual_54);
                errorMsg_54 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_59)(arg_1_54)("integer start");
            }
            else {
                errorMsg_54 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_54)("integer start");
            }
            throw new Exception(errorMsg_54);
        }
        let d_37;
        const a_79 = item(Polyline2D__get_PointCount(sub_1) - 1, Polyline2D__get_Points(sub_1));
        const b_79 = Pt_$ctor_7B00E9A0(10, 10);
        const vx_35 = a_79.X - b_79.X;
        const vy_35 = a_79.Y - b_79.Y;
        d_37 = Math.sqrt((vx_35 * vx_35) + (vy_35 * vy_35));
        const r_37 = d_37 < 1E-09;
        const actual_55 = r_37;
        if ((actual_55 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_55, true, "integer end");
        }
        else {
            let valueType_55;
            let copyOfStruct_62 = actual_55;
            valueType_55 = bool_type;
            const primitiveTypes_55 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_55;
            if (contains(valueType_55, primitiveTypes_55, {
                Equals: equals,
                GetHashCode: (x_55) => (structuralHash(x_55) | 0),
            })) {
                const arg_60 = toString(true);
                const arg_1_55 = toString(actual_55);
                errorMsg_55 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_60)(arg_1_55)("integer end");
            }
            else {
                errorMsg_55 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_55)("integer end");
            }
            throw new Exception(errorMsg_55);
        }
        Test_TestCaseBuilder__Zero(builder$0040_79);
    }));
})()])), Test_testList("tryFindSelfIntersection", ofArray([(() => {
    const builder$0040_80 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("no intersection in simple polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_80, Test_TestCaseBuilder__Delay_1505(builder$0040_80, () => {
        const result = Polyline2D_tryFindSelfIntersection_Z5A89AEF5(plOpen);
        Expect_isNone(result, "simple polyline has no self intersection");
        Test_TestCaseBuilder__Zero(builder$0040_80);
    }));
})(), (() => {
    const builder$0040_81 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("no intersection in L-shape open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_81, Test_TestCaseBuilder__Delay_1505(builder$0040_81, () => {
        const result_1 = Polyline2D_tryFindSelfIntersection_Z5A89AEF5(plLShape);
        Expect_isNone(result_1, "L-shape open polyline has no self intersection");
        Test_TestCaseBuilder__Zero(builder$0040_81);
    }));
})(), (() => {
    const builder$0040_82 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("self intersection in figure-8", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_82, Test_TestCaseBuilder__Delay_1505(builder$0040_82, () => {
        let fig8;
        const points_24 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
        fig8 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_24));
        const result_2 = Polyline2D_tryFindSelfIntersection_Z5A89AEF5(fig8);
        Expect_isSome(result_2, "figure 8 has self intersection");
        if (result_2 == null) {
            Test_TestCaseBuilder__Zero(builder$0040_82);
        }
        else {
            const pt_22 = result_2[0];
            const _j = result_2[2] | 0;
            const _i = result_2[1] | 0;
            let d_38;
            const a_81 = pt_22;
            const b_81 = Pt_$ctor_7B00E9A0(5, 5);
            const vx_36 = a_81.X - b_81.X;
            const vy_36 = a_81.Y - b_81.Y;
            d_38 = Math.sqrt((vx_36 * vx_36) + (vy_36 * vy_36));
            const r_38 = d_38 < 1E-09;
            const actual_56 = r_38;
            if ((actual_56 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_56, true, "intersection near center");
            }
            else {
                let valueType_56;
                let copyOfStruct_63 = actual_56;
                valueType_56 = bool_type;
                const primitiveTypes_56 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_56;
                if (contains(valueType_56, primitiveTypes_56, {
                    Equals: equals,
                    GetHashCode: (x_56) => (structuralHash(x_56) | 0),
                })) {
                    const arg_61 = toString(true);
                    const arg_1_56 = toString(actual_56);
                    errorMsg_56 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_61)(arg_1_56)("intersection near center");
                }
                else {
                    errorMsg_56 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_56)("intersection near center");
                }
                throw new Exception(errorMsg_56);
            }
            Test_TestCaseBuilder__Zero(builder$0040_82);
        }
    }));
})(), (() => {
    const builder$0040_83 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("touching segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_83, Test_TestCaseBuilder__Delay_1505(builder$0040_83, () => {
        let pl_41;
        const points_25 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 5), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(5, 5), Pt_$ctor_7B00E9A0(0, 10)];
        pl_41 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_25));
        const result_3 = Polyline2D_tryFindSelfIntersection_Z5A89AEF5(pl_41);
        Expect_isSome(result_3, "touching segments should find intersection");
        Test_TestCaseBuilder__Zero(builder$0040_83);
    }));
})()])), Test_testList("CloseInPlace member", ofArray([(() => {
    const builder$0040_84 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close in place with close ends snaps", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_84, Test_TestCaseBuilder__Delay_1505(builder$0040_84, () => {
        let pl_42;
        const points_26 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 0.0001)];
        pl_42 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_26));
        Polyline2D__CloseInPlace_5E38073B(pl_42, 0.001);
        let d_39;
        const a_83 = item(Polyline2D__get_PointCount(pl_42) - 1, Polyline2D__get_Points(pl_42));
        const b_83 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_37 = a_83.X - b_83.X;
        const vy_37 = a_83.Y - b_83.Y;
        d_39 = Math.sqrt((vx_37 * vx_37) + (vy_37 * vy_37));
        const r_39 = d_39 < 1E-09;
        const actual_57 = r_39;
        if ((actual_57 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_57, true, "last snapped to first");
        }
        else {
            let valueType_57;
            let copyOfStruct_64 = actual_57;
            valueType_57 = bool_type;
            const primitiveTypes_57 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_57;
            if (contains(valueType_57, primitiveTypes_57, {
                Equals: equals,
                GetHashCode: (x_57) => (structuralHash(x_57) | 0),
            })) {
                const arg_62 = toString(true);
                const arg_1_57 = toString(actual_57);
                errorMsg_57 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_62)(arg_1_57)("last snapped to first");
            }
            else {
                errorMsg_57 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_57)("last snapped to first");
            }
            throw new Exception(errorMsg_57);
        }
        const actual_58 = Polyline2D__get_PointCount(pl_42) | 0;
        if ((actual_58 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_58, 4, "no point added, snapped");
        }
        else {
            let valueType_58;
            let copyOfStruct_65 = actual_58;
            valueType_58 = int32_type;
            const primitiveTypes_58 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_58;
            if (contains(valueType_58, primitiveTypes_58, {
                Equals: equals,
                GetHashCode: (x_58) => (structuralHash(x_58) | 0),
            })) {
                const arg_63 = int32ToString(4);
                const arg_1_58 = int32ToString(actual_58);
                errorMsg_58 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_63)(arg_1_58)("no point added, snapped");
            }
            else {
                errorMsg_58 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_58)("no point added, snapped");
            }
            throw new Exception(errorMsg_58);
        }
        Test_TestCaseBuilder__Zero(builder$0040_84);
    }));
})(), (() => {
    const builder$0040_85 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close in place with far ends adds point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_85, Test_TestCaseBuilder__Delay_1505(builder$0040_85, () => {
        let pl_43;
        const points_27 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10)];
        pl_43 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_27));
        Polyline2D__CloseInPlace_5E38073B(pl_43, 0.001);
        const actual_59 = Polyline2D__get_PointCount(pl_43) | 0;
        if ((actual_59 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_59, 4, "point added");
        }
        else {
            let valueType_59;
            let copyOfStruct_66 = actual_59;
            valueType_59 = int32_type;
            const primitiveTypes_59 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_59;
            if (contains(valueType_59, primitiveTypes_59, {
                Equals: equals,
                GetHashCode: (x_59) => (structuralHash(x_59) | 0),
            })) {
                const arg_64 = int32ToString(4);
                const arg_1_59 = int32ToString(actual_59);
                errorMsg_59 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_64)(arg_1_59)("point added");
            }
            else {
                errorMsg_59 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_59)("point added");
            }
            throw new Exception(errorMsg_59);
        }
        let d_40;
        const a_85 = item(3, Polyline2D__get_Points(pl_43));
        const b_85 = Pt_$ctor_7B00E9A0(0, 0);
        const vx_38 = a_85.X - b_85.X;
        const vy_38 = a_85.Y - b_85.Y;
        d_40 = Math.sqrt((vx_38 * vx_38) + (vy_38 * vy_38));
        const r_40 = d_40 < 1E-09;
        const actual_60 = r_40;
        if ((actual_60 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_60, true, "added first as last");
        }
        else {
            let valueType_60;
            let copyOfStruct_67 = actual_60;
            valueType_60 = bool_type;
            const primitiveTypes_60 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_60;
            if (contains(valueType_60, primitiveTypes_60, {
                Equals: equals,
                GetHashCode: (x_60) => (structuralHash(x_60) | 0),
            })) {
                const arg_65 = toString(true);
                const arg_1_60 = toString(actual_60);
                errorMsg_60 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_65)(arg_1_60)("added first as last");
            }
            else {
                errorMsg_60 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_60)("added first as last");
            }
            throw new Exception(errorMsg_60);
        }
        Test_TestCaseBuilder__Zero(builder$0040_85);
    }));
})(), (() => {
    const builder$0040_86 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close in place fails with less than 3 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_86, Test_TestCaseBuilder__Delay_1505(builder$0040_86, () => {
        let pl_44;
        const points_28 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)];
        pl_44 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_28));
        Expect_throws(() => {
            Polyline2D__CloseInPlace_5E38073B(pl_44, 0.001);
        }, "less than 3 points");
        Test_TestCaseBuilder__Zero(builder$0040_86);
    }));
})()])), Test_testList("GetSegment edge cases", ofArray([(() => {
    const builder$0040_87 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("get segment at last valid index", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_87, Test_TestCaseBuilder__Delay_1505(builder$0040_87, () => {
        const seg = Polyline2D__GetSegment_Z524259A4(plOpen, 2);
        let d_41;
        let a_87;
        const ln_3 = seg;
        a_87 = Pt_$ctor_7B00E9A0_1(ln_3.FromX, ln_3.FromY);
        const b_87 = Pt_$ctor_7B00E9A0(10, 10);
        const vx_39 = a_87.X - b_87.X;
        const vy_39 = a_87.Y - b_87.Y;
        d_41 = Math.sqrt((vx_39 * vx_39) + (vy_39 * vy_39));
        const r_41 = d_41 < 1E-09;
        const actual_61 = r_41;
        if ((actual_61 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_61, true, "last segment from");
        }
        else {
            let valueType_61;
            let copyOfStruct_68 = actual_61;
            valueType_61 = bool_type;
            const primitiveTypes_61 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_61;
            if (contains(valueType_61, primitiveTypes_61, {
                Equals: equals,
                GetHashCode: (x_61) => (structuralHash(x_61) | 0),
            })) {
                const arg_66 = toString(true);
                const arg_1_61 = toString(actual_61);
                errorMsg_61 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_66)(arg_1_61)("last segment from");
            }
            else {
                errorMsg_61 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_61)("last segment from");
            }
            throw new Exception(errorMsg_61);
        }
        let d_42;
        let a_89;
        const ln_4 = seg;
        a_89 = Pt_$ctor_7B00E9A0_1(ln_4.ToX, ln_4.ToY);
        const b_89 = Pt_$ctor_7B00E9A0(0, 10);
        const vx_40 = a_89.X - b_89.X;
        const vy_40 = a_89.Y - b_89.Y;
        d_42 = Math.sqrt((vx_40 * vx_40) + (vy_40 * vy_40));
        const r_42 = d_42 < 1E-09;
        const actual_62 = r_42;
        if ((actual_62 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_62, true, "last segment to");
        }
        else {
            let valueType_62;
            let copyOfStruct_69 = actual_62;
            valueType_62 = bool_type;
            const primitiveTypes_62 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_62;
            if (contains(valueType_62, primitiveTypes_62, {
                Equals: equals,
                GetHashCode: (x_62) => (structuralHash(x_62) | 0),
            })) {
                const arg_67 = toString(true);
                const arg_1_62 = toString(actual_62);
                errorMsg_62 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_67)(arg_1_62)("last segment to");
            }
            else {
                errorMsg_62 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_62)("last segment to");
            }
            throw new Exception(errorMsg_62);
        }
        Test_TestCaseBuilder__Zero(builder$0040_87);
    }));
})(), (() => {
    const builder$0040_88 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("last segment matches GetSegment last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_88, Test_TestCaseBuilder__Delay_1505(builder$0040_88, () => {
        const seg1 = Polyline2D__get_LastSegment(plOpen);
        const seg2 = Polyline2D__GetSegment_Z524259A4(plOpen, Polyline2D__get_LastSegmentIndex(plOpen));
        let d_43;
        let a_91;
        const ln_5 = seg1;
        a_91 = Pt_$ctor_7B00E9A0_1(ln_5.FromX, ln_5.FromY);
        let b_91;
        const ln_6 = seg2;
        b_91 = Pt_$ctor_7B00E9A0_1(ln_6.FromX, ln_6.FromY);
        const vx_41 = a_91.X - b_91.X;
        const vy_41 = a_91.Y - b_91.Y;
        d_43 = Math.sqrt((vx_41 * vx_41) + (vy_41 * vy_41));
        const r_43 = d_43 < 1E-09;
        const actual_63 = r_43;
        if ((actual_63 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_63, true, "from matches");
        }
        else {
            let valueType_63;
            let copyOfStruct_70 = actual_63;
            valueType_63 = bool_type;
            const primitiveTypes_63 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_63;
            if (contains(valueType_63, primitiveTypes_63, {
                Equals: equals,
                GetHashCode: (x_63) => (structuralHash(x_63) | 0),
            })) {
                const arg_68 = toString(true);
                const arg_1_63 = toString(actual_63);
                errorMsg_63 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_68)(arg_1_63)("from matches");
            }
            else {
                errorMsg_63 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_63)("from matches");
            }
            throw new Exception(errorMsg_63);
        }
        let d_44;
        let a_93;
        const ln_7 = seg1;
        a_93 = Pt_$ctor_7B00E9A0_1(ln_7.ToX, ln_7.ToY);
        let b_93;
        const ln_8 = seg2;
        b_93 = Pt_$ctor_7B00E9A0_1(ln_8.ToX, ln_8.ToY);
        const vx_42 = a_93.X - b_93.X;
        const vy_42 = a_93.Y - b_93.Y;
        d_44 = Math.sqrt((vx_42 * vx_42) + (vy_42 * vy_42));
        const r_44 = d_44 < 1E-09;
        const actual_64 = r_44;
        if ((actual_64 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_64, true, "to matches");
        }
        else {
            let valueType_64;
            let copyOfStruct_71 = actual_64;
            valueType_64 = bool_type;
            const primitiveTypes_64 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_64;
            if (contains(valueType_64, primitiveTypes_64, {
                Equals: equals,
                GetHashCode: (x_64) => (structuralHash(x_64) | 0),
            })) {
                const arg_69 = toString(true);
                const arg_1_64 = toString(actual_64);
                errorMsg_64 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_69)(arg_1_64)("to matches");
            }
            else {
                errorMsg_64 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_64)("to matches");
            }
            throw new Exception(errorMsg_64);
        }
        Test_TestCaseBuilder__Zero(builder$0040_88);
    }));
})()])), Test_testList("BoundingRectangle edge cases", ofArray([(() => {
    const builder$0040_89 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("single point bounding rect", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_89, Test_TestCaseBuilder__Delay_1505(builder$0040_89, () => {
        const br = Polyline2D__get_BoundingRectangle(plSinglePoint);
        Expect_isTrue(Math.abs(br.MinX - 1) < 1E-09)("min x");
        Expect_isTrue(Math.abs(br.MaxX - 1) < 1E-09)("max x");
        Test_TestCaseBuilder__Zero(builder$0040_89);
    }));
})(), (() => {
    const builder$0040_90 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("line bounding rect", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_90, Test_TestCaseBuilder__Delay_1505(builder$0040_90, () => {
        const br_1 = Polyline2D__get_BoundingRectangle(plLine);
        Expect_isTrue(Math.abs(br_1.MinX - 0) < 1E-09)("min x");
        Expect_isTrue(Math.abs(br_1.MaxX - 10) < 1E-09)("max x");
        Expect_isTrue(Math.abs(br_1.MinY - 0) < 1E-09)("min y");
        Expect_isTrue(Math.abs(br_1.MaxY - 0) < 1E-09)("max y");
        Test_TestCaseBuilder__Zero(builder$0040_90);
    }));
})()])), Test_testList("Start and End setters", ofArray([(() => {
    const builder$0040_91 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_91, Test_TestCaseBuilder__Delay_1505(builder$0040_91, () => {
        const pl_45 = Polyline2D__Clone(plOpen);
        Polyline2D__set_Start_6ADE94FD(pl_45, Pt_$ctor_7B00E9A0(99, 99));
        let d_45;
        const a_95 = item(0, Polyline2D__get_Points(pl_45));
        const b_95 = Pt_$ctor_7B00E9A0(99, 99);
        const vx_43 = a_95.X - b_95.X;
        const vy_43 = a_95.Y - b_95.Y;
        d_45 = Math.sqrt((vx_43 * vx_43) + (vy_43 * vy_43));
        const r_45 = d_45 < 1E-09;
        const actual_65 = r_45;
        if ((actual_65 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_65, true, "start set");
        }
        else {
            let valueType_65;
            let copyOfStruct_72 = actual_65;
            valueType_65 = bool_type;
            const primitiveTypes_65 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_65;
            if (contains(valueType_65, primitiveTypes_65, {
                Equals: equals,
                GetHashCode: (x_65) => (structuralHash(x_65) | 0),
            })) {
                const arg_70 = toString(true);
                const arg_1_65 = toString(actual_65);
                errorMsg_65 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_70)(arg_1_65)("start set");
            }
            else {
                errorMsg_65 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_65)("start set");
            }
            throw new Exception(errorMsg_65);
        }
        Test_TestCaseBuilder__Zero(builder$0040_91);
    }));
})(), (() => {
    const builder$0040_92 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_92, Test_TestCaseBuilder__Delay_1505(builder$0040_92, () => {
        const pl_46 = Polyline2D__Clone(plOpen);
        Polyline2D__set_End_6ADE94FD(pl_46, Pt_$ctor_7B00E9A0(88, 88));
        let d_46;
        const a_97 = item(Polyline2D__get_PointCount(pl_46) - 1, Polyline2D__get_Points(pl_46));
        const b_97 = Pt_$ctor_7B00E9A0(88, 88);
        const vx_44 = a_97.X - b_97.X;
        const vy_44 = a_97.Y - b_97.Y;
        d_46 = Math.sqrt((vx_44 * vx_44) + (vy_44 * vy_44));
        const r_46 = d_46 < 1E-09;
        const actual_66 = r_46;
        if ((actual_66 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_66, true, "end set");
        }
        else {
            let valueType_66;
            let copyOfStruct_73 = actual_66;
            valueType_66 = bool_type;
            const primitiveTypes_66 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_66;
            if (contains(valueType_66, primitiveTypes_66, {
                Equals: equals,
                GetHashCode: (x_66) => (structuralHash(x_66) | 0),
            })) {
                const arg_71 = toString(true);
                const arg_1_66 = toString(actual_66);
                errorMsg_66 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_71)(arg_1_66)("end set");
            }
            else {
                errorMsg_66 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_66)("end set");
            }
            throw new Exception(errorMsg_66);
        }
        Test_TestCaseBuilder__Zero(builder$0040_92);
    }));
})(), (() => {
    const builder$0040_93 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set first point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_93, Test_TestCaseBuilder__Delay_1505(builder$0040_93, () => {
        const pl_47 = Polyline2D__Clone(plOpen);
        Polyline2D__set_FirstPoint_6ADE94FD(pl_47, Pt_$ctor_7B00E9A0(77, 77));
        let d_47;
        const a_99 = item(0, Polyline2D__get_Points(pl_47));
        const b_99 = Pt_$ctor_7B00E9A0(77, 77);
        const vx_45 = a_99.X - b_99.X;
        const vy_45 = a_99.Y - b_99.Y;
        d_47 = Math.sqrt((vx_45 * vx_45) + (vy_45 * vy_45));
        const r_47 = d_47 < 1E-09;
        const actual_67 = r_47;
        if ((actual_67 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_67, true, "first point set");
        }
        else {
            let valueType_67;
            let copyOfStruct_74 = actual_67;
            valueType_67 = bool_type;
            const primitiveTypes_67 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_67;
            if (contains(valueType_67, primitiveTypes_67, {
                Equals: equals,
                GetHashCode: (x_67) => (structuralHash(x_67) | 0),
            })) {
                const arg_72 = toString(true);
                const arg_1_67 = toString(actual_67);
                errorMsg_67 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_72)(arg_1_67)("first point set");
            }
            else {
                errorMsg_67 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_67)("first point set");
            }
            throw new Exception(errorMsg_67);
        }
        Test_TestCaseBuilder__Zero(builder$0040_93);
    }));
})(), (() => {
    const builder$0040_94 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set last point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_94, Test_TestCaseBuilder__Delay_1505(builder$0040_94, () => {
        const pl_48 = Polyline2D__Clone(plOpen);
        Polyline2D__set_LastPoint_6ADE94FD(pl_48, Pt_$ctor_7B00E9A0(66, 66));
        let d_48;
        const a_101 = item(Polyline2D__get_PointCount(pl_48) - 1, Polyline2D__get_Points(pl_48));
        const b_101 = Pt_$ctor_7B00E9A0(66, 66);
        const vx_46 = a_101.X - b_101.X;
        const vy_46 = a_101.Y - b_101.Y;
        d_48 = Math.sqrt((vx_46 * vx_46) + (vy_46 * vy_46));
        const r_48 = d_48 < 1E-09;
        const actual_68 = r_48;
        if ((actual_68 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_68, true, "last point set");
        }
        else {
            let valueType_68;
            let copyOfStruct_75 = actual_68;
            valueType_68 = bool_type;
            const primitiveTypes_68 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_68;
            if (contains(valueType_68, primitiveTypes_68, {
                Equals: equals,
                GetHashCode: (x_68) => (structuralHash(x_68) | 0),
            })) {
                const arg_73 = toString(true);
                const arg_1_68 = toString(actual_68);
                errorMsg_68 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_73)(arg_1_68)("last point set");
            }
            else {
                errorMsg_68 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_68)("last point set");
            }
            throw new Exception(errorMsg_68);
        }
        Test_TestCaseBuilder__Zero(builder$0040_94);
    }));
})()])), Test_testList("Offset edge cases", ofArray([(() => {
    const builder$0040_95 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset fails with less than 2 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_95, Test_TestCaseBuilder__Delay_1505(builder$0040_95, () => {
        Expect_throws(() => {
            Polyline2D_offset_Z45C468A5(plSinglePoint, 1, false, true, 1, -0.9961946980917455);
        }, "single point offset");
        Test_TestCaseBuilder__Zero(builder$0040_95);
    }));
})(), (() => {
    const builder$0040_96 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetVar fails with less than 2 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_96, Test_TestCaseBuilder__Delay_1505(builder$0040_96, () => {
        Expect_throws(() => {
            Polyline2D_offsetVar_357F0A77(plSinglePoint, new Float64Array([1]), false, true, 1, 1, 0.9961946980917455, -0.9961946980917455);
        }, "single point offsetVar");
        Test_TestCaseBuilder__Zero(builder$0040_96);
    }));
})()])), Test_testList("removeColinearAndDuplicatePoints edge cases", ofArray([(() => {
    const builder$0040_97 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("single point returns same", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_97, Test_TestCaseBuilder__Delay_1505(builder$0040_97, () => {
        const result_4 = Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, plSinglePoint);
        const actual_69 = Polyline2D__get_PointCount(result_4) | 0;
        if ((actual_69 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_69, 1, "single point unchanged");
        }
        else {
            let valueType_69;
            let copyOfStruct_76 = actual_69;
            valueType_69 = int32_type;
            const primitiveTypes_69 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_69;
            if (contains(valueType_69, primitiveTypes_69, {
                Equals: equals,
                GetHashCode: (x_69) => (structuralHash(x_69) | 0),
            })) {
                const arg_74 = int32ToString(1);
                const arg_1_69 = int32ToString(actual_69);
                errorMsg_69 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_74)(arg_1_69)("single point unchanged");
            }
            else {
                errorMsg_69 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_69)("single point unchanged");
            }
            throw new Exception(errorMsg_69);
        }
        Test_TestCaseBuilder__Zero(builder$0040_97);
    }));
})(), (() => {
    const builder$0040_98 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("empty returns same", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_98, Test_TestCaseBuilder__Delay_1505(builder$0040_98, () => {
        const result_5 = Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, plEmpty);
        const actual_70 = Polyline2D__get_PointCount(result_5) | 0;
        if ((actual_70 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_70, 0, "empty unchanged");
        }
        else {
            let valueType_70;
            let copyOfStruct_77 = actual_70;
            valueType_70 = int32_type;
            const primitiveTypes_70 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_70;
            if (contains(valueType_70, primitiveTypes_70, {
                Equals: equals,
                GetHashCode: (x_70) => (structuralHash(x_70) | 0),
            })) {
                const arg_75 = int32ToString(0);
                const arg_1_70 = int32ToString(actual_70);
                errorMsg_70 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_75)(arg_1_70)("empty unchanged");
            }
            else {
                errorMsg_70 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_70)("empty unchanged");
            }
            throw new Exception(errorMsg_70);
        }
        Test_TestCaseBuilder__Zero(builder$0040_98);
    }));
})(), (() => {
    const builder$0040_99 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tolerance too tight throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_99, Test_TestCaseBuilder__Delay_1505(builder$0040_99, () => {
        const tooTight = 0.9999999999;
        Expect_throws(() => {
            Polyline2D_removeColinearAndDuplicatePoints(tooTight, 1E-09, plOpen);
        }, "tolerance tighter than 0.01 degrees should throw");
        Test_TestCaseBuilder__Zero(builder$0040_99);
    }));
})()])), Test_testList("removeDuplicatePoints edge cases", ofArray([(() => {
    const builder$0040_100 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("no duplicates returns same count", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_100, Test_TestCaseBuilder__Delay_1505(builder$0040_100, () => {
        const result_6 = Polyline2D_removeDuplicatePoints(1E-09, plOpen);
        const actual_71 = Polyline2D__get_PointCount(result_6) | 0;
        const expected_118 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_71 === expected_118) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_71, expected_118, "no duplicates, same count");
        }
        else {
            let valueType_71;
            let copyOfStruct_78 = actual_71;
            valueType_71 = int32_type;
            const primitiveTypes_71 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_71;
            if (contains(valueType_71, primitiveTypes_71, {
                Equals: equals,
                GetHashCode: (x_72) => (structuralHash(x_72) | 0),
            })) {
                const arg_76 = int32ToString(expected_118);
                const arg_1_71 = int32ToString(actual_71);
                errorMsg_71 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_76)(arg_1_71)("no duplicates, same count");
            }
            else {
                errorMsg_71 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_118)(actual_71)("no duplicates, same count");
            }
            throw new Exception(errorMsg_71);
        }
        Test_TestCaseBuilder__Zero(builder$0040_100);
    }));
})(), (() => {
    const builder$0040_101 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("all same points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_101, Test_TestCaseBuilder__Delay_1505(builder$0040_101, () => {
        let pl_53;
        const points_29 = [Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 1)];
        pl_53 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_29));
        const result_7 = Polyline2D_removeDuplicatePoints(1E-09, pl_53);
        const actual_72 = Polyline2D__get_PointCount(result_7) | 0;
        if ((actual_72 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_72, 1, "all same reduces to 1");
        }
        else {
            let valueType_72;
            let copyOfStruct_79 = actual_72;
            valueType_72 = int32_type;
            const primitiveTypes_72 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_72;
            if (contains(valueType_72, primitiveTypes_72, {
                Equals: equals,
                GetHashCode: (x_73) => (structuralHash(x_73) | 0),
            })) {
                const arg_77 = int32ToString(1);
                const arg_1_72 = int32ToString(actual_72);
                errorMsg_72 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_77)(arg_1_72)("all same reduces to 1");
            }
            else {
                errorMsg_72 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_72)("all same reduces to 1");
            }
            throw new Exception(errorMsg_72);
        }
        Test_TestCaseBuilder__Zero(builder$0040_101);
    }));
})(), (() => {
    const builder$0040_102 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_102, Test_TestCaseBuilder__Delay_1505(builder$0040_102, () => {
        const result_8 = Polyline2D_removeDuplicatePoints(1E-09, plSinglePoint);
        const actual_73 = Polyline2D__get_PointCount(result_8) | 0;
        if ((actual_73 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_73, 1, "single point unchanged");
        }
        else {
            let valueType_73;
            let copyOfStruct_80 = actual_73;
            valueType_73 = int32_type;
            const primitiveTypes_73 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_73;
            if (contains(valueType_73, primitiveTypes_73, {
                Equals: equals,
                GetHashCode: (x_74) => (structuralHash(x_74) | 0),
            })) {
                const arg_78 = int32ToString(1);
                const arg_1_73 = int32ToString(actual_73);
                errorMsg_73 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_78)(arg_1_73)("single point unchanged");
            }
            else {
                errorMsg_73 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_73)("single point unchanged");
            }
            throw new Exception(errorMsg_73);
        }
        Test_TestCaseBuilder__Zero(builder$0040_102);
    }));
})()])), Test_testList("WindingNumber edge cases", ofArray([(() => {
    const builder$0040_103 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("point on vertex", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_103, Test_TestCaseBuilder__Delay_1505(builder$0040_103, () => {
        const _wn = Polyline2D__WindingNumber_6ADE94FD(plClosed, Pt_$ctor_7B00E9A0(0, 0)) | 0;
        Expect_isTrue(true)("doesn\'t crash on vertex");
        Test_TestCaseBuilder__Zero(builder$0040_103);
    }));
})(), (() => {
    const builder$0040_104 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("point far away", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_104, Test_TestCaseBuilder__Delay_1505(builder$0040_104, () => {
        const wn_1 = Polyline2D__WindingNumber_6ADE94FD(plClosed, Pt_$ctor_7B00E9A0(1000, 1000)) | 0;
        const actual_74 = wn_1 | 0;
        if ((actual_74 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_74, 0, "far away point is outside");
        }
        else {
            let valueType_74;
            let copyOfStruct_81 = actual_74;
            valueType_74 = int32_type;
            const primitiveTypes_74 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_74;
            if (contains(valueType_74, primitiveTypes_74, {
                Equals: equals,
                GetHashCode: (x_75) => (structuralHash(x_75) | 0),
            })) {
                const arg_79 = int32ToString(0);
                const arg_1_74 = int32ToString(actual_74);
                errorMsg_74 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_79)(arg_1_74)("far away point is outside");
            }
            else {
                errorMsg_74 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_74)("far away point is outside");
            }
            throw new Exception(errorMsg_74);
        }
        Test_TestCaseBuilder__Zero(builder$0040_104);
    }));
})(), (() => {
    const builder$0040_105 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("single point polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_105, Test_TestCaseBuilder__Delay_1505(builder$0040_105, () => {
        const wn_2 = Polyline2D__WindingNumber_6ADE94FD(plSinglePoint, Pt_$ctor_7B00E9A0(1, 1)) | 0;
        const actual_75 = wn_2 | 0;
        if ((actual_75 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_75, 0, "single point winding is 0");
        }
        else {
            let valueType_75;
            let copyOfStruct_82 = actual_75;
            valueType_75 = int32_type;
            const primitiveTypes_75 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_75;
            if (contains(valueType_75, primitiveTypes_75, {
                Equals: equals,
                GetHashCode: (x_76) => (structuralHash(x_76) | 0),
            })) {
                const arg_80 = int32ToString(0);
                const arg_1_75 = int32ToString(actual_75);
                errorMsg_75 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_80)(arg_1_75)("single point winding is 0");
            }
            else {
                errorMsg_75 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_75)("single point winding is 0");
            }
            throw new Exception(errorMsg_75);
        }
        Test_TestCaseBuilder__Zero(builder$0040_105);
    }));
})()])), Test_testList("move static same as translate", singleton((() => {
    const builder$0040_106 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move equals translate", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_106, Test_TestCaseBuilder__Delay_1505(builder$0040_106, () => {
        const v = Vc_$ctor_7B00E9A0(3, 4);
        const moved = Polyline2D_move(v, plOpen);
        const translated = Polyline2D_translate(v, plOpen);
        Test_TestCaseBuilder__For_Z371464DD(builder$0040_106, rangeDouble(0, 1, Polyline2D__get_PointCount(moved) - 1), (_arg) => {
            const i = _arg | 0;
            let d_49;
            const a_103 = item(i, Polyline2D__get_Points(moved));
            const b_103 = item(i, Polyline2D__get_Points(translated));
            const vx_47 = a_103.X - b_103.X;
            const vy_47 = a_103.Y - b_103.Y;
            d_49 = Math.sqrt((vx_47 * vx_47) + (vy_47 * vy_47));
            const r_49 = d_49 < 1E-09;
            const actual_76 = r_49;
            const msg_124 = `point ${i}`;
            if ((actual_76 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_76, true, msg_124);
            }
            else {
                let valueType_76;
                let copyOfStruct_83 = actual_76;
                valueType_76 = bool_type;
                const primitiveTypes_76 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
                let errorMsg_76;
                if (contains(valueType_76, primitiveTypes_76, {
                    Equals: equals,
                    GetHashCode: (x_77) => (structuralHash(x_77) | 0),
                })) {
                    const arg_81 = toString(true);
                    const arg_1_76 = toString(actual_76);
                    errorMsg_76 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_81)(arg_1_76)(msg_124);
                }
                else {
                    errorMsg_76 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_76)(msg_124);
                }
                throw new Exception(errorMsg_76);
            }
            Test_TestCaseBuilder__Zero(builder$0040_106);
        });
    }));
})()))]));

