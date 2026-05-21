
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Polyline2D__Contains_6ADE94FD as Polyline2D__Contains_6ADE94FD_1, Polyline2D__WindingNumber_6ADE94FD as Polyline2D__WindingNumber_6ADE94FD_1, Polyline2D__DistanceTo_6ADE94FD as Polyline2D__DistanceTo_6ADE94FD_1, Polyline2D__ClosestVertex_6ADE94FD, Polyline2D__ClosestPoint_6ADE94FD as Polyline2D__ClosestPoint_6ADE94FD_1, Polyline2D__ClosestParameter_6ADE94FD as Polyline2D__ClosestParameter_6ADE94FD_1, Polyline2D__get_SegmentCount as Polyline2D__get_SegmentCount_1, Polyline2D__get_Points as Polyline2D__get_Points_1, Polyline2D__get_Length as Polyline2D__get_Length_1, Polyline2D_$ctor_Z5FD8CF3C } from "./Src/Polyline2D.js";
import { Polyline2D_move, Polyline2D__set_LastPoint_6ADE94FD, Polyline2D__set_FirstPoint_6ADE94FD, Polyline2D__set_End_6ADE94FD, Polyline2D__set_Start_6ADE94FD, Polyline2D_tryFindSelfIntersection_Z5A89AEF5, Polyline2D__ClosestVertex_6ADE94FD as Polyline2D__ClosestVertex_6ADE94FD_1, Polyline2D_pointsUnsafeInternal_Z5A89AEF5, Polyline2D_createDirectlyUnsafe_Z5FD8CF3C, Polyline2D_scale, Polyline2D_evaluateAt, Polyline2D_reverse_Z5A89AEF5, Polyline2D_reverseInPlace_Z5A89AEF5, Polyline2D_ende_Z5A89AEF5, Polyline2D_start_Z5A89AEF5, Polyline2D_rotateWithCenter, Polyline2D_equals, Polyline2D_closeInPlace_Z5A89AEF5, Polyline2D_close_Z5A89AEF5, Polyline2D__get_AsString, Polyline2D__SignedDistanceTo_6ADE94FD, Polyline2D__get_SegmentVectors, Polyline2D__get_Segments, Polyline2D__IsAlmostClosed_5E38073B, Polyline2D__get_LastSegmentIndex, Polyline2D__get_LastPointIndex, Polyline2D__set_SecondLastPoint_6ADE94FD, Polyline2D__get_SecondLastPoint, Polyline2D__set_SecondPoint_6ADE94FD, Polyline2D__get_SecondPoint, Polyline2D__SetVertex, Polyline2D__get_AsFSharpCode, Polyline2D_removeColinearAndDuplicatePoints, Polyline2D_removeDuplicatePoints, Polyline2D_offsetVar_357F0A77, Polyline2D_offset_Z45C468A5, Polyline2D_rotate, Polyline2D_map, Polyline2D_subPolyline, Polyline2D__CloseInPlace_5E38073B, Polyline2D__Duplicate, Polyline2D__Contains_6ADE94FD, Polyline2D__WindingNumber_6ADE94FD, Polyline2D__ReverseInPlace, Polyline2D__Clone, Polyline2D__Reverse, Polyline2D_moveY, Polyline2D_moveX, Polyline2D_translate, Polyline2D__ScaleOn, Polyline2D__get_Points, Polyline2D__Scale_5E38073B, Polyline2D__DistanceTo_6ADE94FD, Polyline2D__ClosestParameter_6ADE94FD, Polyline2D__ClosestPoint_6ADE94FD, Polyline2D__EvaluateAt_5E38073B, Polyline2D__get_FirstSegment, Polyline2D__get_LastSegment, Polyline2D__GetSegment_Z524259A4, Polyline2D__get_Center, Polyline2D__get_IsClockwise, Polyline2D__get_IsCounterClockwise, Polyline2D__get_SignedArea, Polyline2D__get_Area, Polyline2D__get_Length, Polyline2D__get_BoundingRectangle, Polyline2D__get_IsClosed, Polyline2D__get_LastPoint, Polyline2D__get_FirstPoint, Polyline2D__get_End, Polyline2D__get_Start, Polyline2D__get_SegmentCount, Polyline2D__get_PointCount, Polyline2D_$ctor } from "./Src/Polyline2D.js";
import { Test_TestCaseBuilder__For_Z371464DD, Expect_isSome, Expect_isNone, Expect_stringContains, Expect_throws, Expect_isFalse, Expect_isTrue, Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { int32ToString, structuralHash, Exception, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { singleton, replicate, ofArray, contains } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
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
        let copyOfStruct, arg, arg_1, copyOfStruct_1, arg_6, arg_1_1, copyOfStruct_2, arg_7, arg_1_2, copyOfStruct_3, arg_8, arg_1_3;
        const actual = Polyline2D__get_PointCount(plClosed) | 0;
        if ((actual === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 5, "closed polyline has 5 points");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            }) ? ((arg = int32ToString(5), (arg_1 = int32ToString(actual), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("closed polyline has 5 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual)("closed polyline has 5 points"));
        }
        const actual_1 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_1 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 4, "open polyline has 4 points");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_1, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            }) ? ((arg_6 = int32ToString(4), (arg_1_1 = int32ToString(actual_1), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("open polyline has 4 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_1)("open polyline has 4 points"));
        }
        const actual_2 = Polyline2D__get_PointCount(plSinglePoint) | 0;
        if ((actual_2 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 1, "single point polyline has 1 point");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_2, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            }) ? ((arg_7 = int32ToString(1), (arg_1_2 = int32ToString(actual_2), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("single point polyline has 1 point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_2)("single point polyline has 1 point"));
        }
        const actual_3 = Polyline2D__get_PointCount(plEmpty) | 0;
        if ((actual_3 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 0, "empty polyline has 0 points");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_3, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            }) ? ((arg_8 = int32ToString(0), (arg_1_3 = int32ToString(actual_3), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("empty polyline has 0 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_3)("empty polyline has 0 points"));
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segment count", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let copyOfStruct_4, arg_9, arg_1_4, copyOfStruct_5, arg_10, arg_1_5, copyOfStruct_6, arg_11, arg_1_6, copyOfStruct_7, arg_12, arg_1_7;
        const actual_4 = Polyline2D__get_SegmentCount(plClosed) | 0;
        if ((actual_4 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, 4, "closed polyline has 4 segments");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_4, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            }) ? ((arg_9 = int32ToString(4), (arg_1_4 = int32ToString(actual_4), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("closed polyline has 4 segments")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_4)("closed polyline has 4 segments"));
        }
        const actual_5 = Polyline2D__get_SegmentCount(plOpen) | 0;
        if ((actual_5 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 3, "open polyline has 3 segments");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_5, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            }) ? ((arg_10 = int32ToString(3), (arg_1_5 = int32ToString(actual_5), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("open polyline has 3 segments")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_5)("open polyline has 3 segments"));
        }
        const actual_6 = Polyline2D__get_SegmentCount(plLine) | 0;
        if ((actual_6 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, 1, "line has 1 segment");
        }
        else {
            throw new Exception(contains((copyOfStruct_6 = actual_6, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            }) ? ((arg_11 = int32ToString(1), (arg_1_6 = int32ToString(actual_6), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("line has 1 segment")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_6)("line has 1 segment"));
        }
        const actual_7 = Polyline2D__get_SegmentCount(plEmpty) | 0;
        if ((actual_7 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 0, "empty polyline has 0 segments");
        }
        else {
            throw new Exception(contains((copyOfStruct_7 = actual_7, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            }) ? ((arg_12 = int32ToString(0), (arg_1_7 = int32ToString(actual_7), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("empty polyline has 0 segments")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_7)("empty polyline has 0 segments"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("start and end points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_2, b_2, vx, vy, copyOfStruct_8, arg_13, arg_1_8, a_4, b_4, vx_1, vy_1, copyOfStruct_9, arg_14, arg_1_9, a_6, b_6, vx_2, vy_2, copyOfStruct_10, arg_15, arg_1_10, a_8, b_8, vx_3, vy_3, copyOfStruct_11, arg_16, arg_1_11;
        const p = plOpen;
        const actual_8 = ((a_2 = Polyline2D__get_Start(p), (b_2 = Pt_$ctor_7B00E9A0(0, 0), (vx = (a_2.X - b_2.X), (vy = (a_2.Y - b_2.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09;
        if ((actual_8 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, true, "start point");
        }
        else {
            throw new Exception(contains((copyOfStruct_8 = actual_8, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            }) ? ((arg_13 = toString(true), (arg_1_8 = toString(actual_8), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("start point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_8)("start point"));
        }
        const actual_9 = ((a_4 = Polyline2D__get_End(p), (b_4 = Pt_$ctor_7B00E9A0(0, 10), (vx_1 = (a_4.X - b_4.X), (vy_1 = (a_4.Y - b_4.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09;
        if ((actual_9 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, true, "end point");
        }
        else {
            throw new Exception(contains((copyOfStruct_9 = actual_9, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            }) ? ((arg_14 = toString(true), (arg_1_9 = toString(actual_9), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("end point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_9)("end point"));
        }
        const actual_10 = ((a_6 = Polyline2D__get_FirstPoint(p), (b_6 = Polyline2D__get_Start(p), (vx_2 = (a_6.X - b_6.X), (vy_2 = (a_6.Y - b_6.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09;
        if ((actual_10 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, true, "first point same as start");
        }
        else {
            throw new Exception(contains((copyOfStruct_10 = actual_10, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_10) => (structuralHash(x_10) | 0),
            }) ? ((arg_15 = toString(true), (arg_1_10 = toString(actual_10), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("first point same as start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_10)("first point same as start"));
        }
        const actual_11 = ((a_8 = Polyline2D__get_LastPoint(p), (b_8 = Polyline2D__get_End(p), (vx_3 = (a_8.X - b_8.X), (vy_3 = (a_8.Y - b_8.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09;
        if ((actual_11 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, true, "last point same as end");
        }
        else {
            throw new Exception(contains((copyOfStruct_11 = actual_11, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            }) ? ((arg_16 = toString(true), (arg_1_11 = toString(actual_11), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("last point same as end")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_11)("last point same as end"));
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
        Expect_isTrue(Math.abs(length - 30) < 1E-09)("length");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("area calculation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const area = Polyline2D__get_Area(plClosed);
        Expect_isTrue(Math.abs(area - 100) < 1E-09)("area");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("signed area", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const signedArea = Polyline2D__get_SignedArea(plClosed);
        Expect_isTrue(Math.abs(signedArea - 100) < 1E-09)("signed area");
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
        let a_10, b_10, vx_4, vy_4, copyOfStruct_12, arg_17, arg_1_12;
        const actual_12 = ((a_10 = Polyline2D__get_Center(plOpen), (b_10 = Pt_$ctor_7B00E9A0(5, 5), (vx_4 = (a_10.X - b_10.X), (vy_4 = (a_10.Y - b_10.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) < 1E-09;
        if ((actual_12 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, true, "center");
        }
        else {
            throw new Exception(contains((copyOfStruct_12 = actual_12, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            }) ? ((arg_17 = toString(true), (arg_1_12 = toString(actual_12), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("center")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_12)("center"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})()])), Test_testList("Point Access and Modification", ofArray([(() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("get segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_12, ln, b_12, vx_5, vy_5, copyOfStruct_13, arg_18, arg_1_13, a_14, ln_1, b_14, vx_6, vy_6, copyOfStruct_14, arg_19, arg_1_14, a_16, ln_2, b_16, vx_7, vy_7, copyOfStruct_15, arg_20, arg_1_15, a_18, ln_3, b_18, vx_8, vy_8, copyOfStruct_16, arg_21, arg_1_16, a_20, ln_4, b_20, ln_5, vx_9, vy_9, copyOfStruct_17, arg_22, arg_1_17, a_22, ln_6, b_22, ln_7, vx_10, vy_10, copyOfStruct_18, arg_23, arg_1_18;
        const seg0 = Polyline2D__GetSegment_Z524259A4(plOpen, 0);
        const actual_13 = ((a_12 = ((ln = seg0, Pt_$ctor_7B00E9A0_1(ln.FromX, ln.FromY))), (b_12 = Pt_$ctor_7B00E9A0(0, 0), (vx_5 = (a_12.X - b_12.X), (vy_5 = (a_12.Y - b_12.Y), Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5))))))) < 1E-09;
        if ((actual_13 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, true, "first segment start");
        }
        else {
            throw new Exception(contains((copyOfStruct_13 = actual_13, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            }) ? ((arg_18 = toString(true), (arg_1_13 = toString(actual_13), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("first segment start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_13)("first segment start"));
        }
        const actual_14 = ((a_14 = ((ln_1 = seg0, Pt_$ctor_7B00E9A0_1(ln_1.ToX, ln_1.ToY))), (b_14 = Pt_$ctor_7B00E9A0(10, 0), (vx_6 = (a_14.X - b_14.X), (vy_6 = (a_14.Y - b_14.Y), Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6))))))) < 1E-09;
        if ((actual_14 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, true, "first segment end");
        }
        else {
            throw new Exception(contains((copyOfStruct_14 = actual_14, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            }) ? ((arg_19 = toString(true), (arg_1_14 = toString(actual_14), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("first segment end")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_14)("first segment end"));
        }
        const lastSeg = Polyline2D__get_LastSegment(plOpen);
        const actual_15 = ((a_16 = ((ln_2 = lastSeg, Pt_$ctor_7B00E9A0_1(ln_2.FromX, ln_2.FromY))), (b_16 = Pt_$ctor_7B00E9A0(10, 10), (vx_7 = (a_16.X - b_16.X), (vy_7 = (a_16.Y - b_16.Y), Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7))))))) < 1E-09;
        if ((actual_15 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, true, "last segment start");
        }
        else {
            throw new Exception(contains((copyOfStruct_15 = actual_15, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            }) ? ((arg_20 = toString(true), (arg_1_15 = toString(actual_15), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("last segment start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_15)("last segment start"));
        }
        const actual_16 = ((a_18 = ((ln_3 = lastSeg, Pt_$ctor_7B00E9A0_1(ln_3.ToX, ln_3.ToY))), (b_18 = Pt_$ctor_7B00E9A0(0, 10), (vx_8 = (a_18.X - b_18.X), (vy_8 = (a_18.Y - b_18.Y), Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8))))))) < 1E-09;
        if ((actual_16 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, true, "last segment end");
        }
        else {
            throw new Exception(contains((copyOfStruct_16 = actual_16, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            }) ? ((arg_21 = toString(true), (arg_1_16 = toString(actual_16), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("last segment end")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_16)("last segment end"));
        }
        const firstSeg = Polyline2D__get_FirstSegment(plOpen);
        const actual_17 = ((a_20 = ((ln_4 = firstSeg, Pt_$ctor_7B00E9A0_1(ln_4.FromX, ln_4.FromY))), (b_20 = ((ln_5 = seg0, Pt_$ctor_7B00E9A0_1(ln_5.FromX, ln_5.FromY))), (vx_9 = (a_20.X - b_20.X), (vy_9 = (a_20.Y - b_20.Y), Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9))))))) < 1E-09;
        if ((actual_17 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, true, "first segment matches GetSegment(0).From");
        }
        else {
            throw new Exception(contains((copyOfStruct_17 = actual_17, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            }) ? ((arg_22 = toString(true), (arg_1_17 = toString(actual_17), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("first segment matches GetSegment(0).From")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_17)("first segment matches GetSegment(0).From"));
        }
        const actual_18 = ((a_22 = ((ln_6 = firstSeg, Pt_$ctor_7B00E9A0_1(ln_6.ToX, ln_6.ToY))), (b_22 = ((ln_7 = seg0, Pt_$ctor_7B00E9A0_1(ln_7.ToX, ln_7.ToY))), (vx_10 = (a_22.X - b_22.X), (vy_10 = (a_22.Y - b_22.Y), Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10))))))) < 1E-09;
        if ((actual_18 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, true, "first segment matches GetSegment(0).To");
        }
        else {
            throw new Exception(contains((copyOfStruct_18 = actual_18, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_18) => (structuralHash(x_18) | 0),
            }) ? ((arg_23 = toString(true), (arg_1_18 = toString(actual_18), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("first segment matches GetSegment(0).To")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_18)("first segment matches GetSegment(0).To"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("evaluate at parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_24, b_24, vx_11, vy_11, copyOfStruct_19, arg_24, arg_1_19, a_26, b_26, vx_12, vy_12, copyOfStruct_20, arg_25, arg_1_20, a_28, b_28, vx_13, vy_13, copyOfStruct_21, arg_26, arg_1_21;
        const actual_19 = ((a_24 = Polyline2D__EvaluateAt_5E38073B(plOpen, 0), (b_24 = Pt_$ctor_7B00E9A0(0, 0), (vx_11 = (a_24.X - b_24.X), (vy_11 = (a_24.Y - b_24.Y), Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11))))))) < 1E-09;
        if ((actual_19 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, true, "at parameter 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_19 = actual_19, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            }) ? ((arg_24 = toString(true), (arg_1_19 = toString(actual_19), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("at parameter 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_19)("at parameter 0"));
        }
        const actual_20 = ((a_26 = Polyline2D__EvaluateAt_5E38073B(plOpen, 1), (b_26 = Pt_$ctor_7B00E9A0(10, 0), (vx_12 = (a_26.X - b_26.X), (vy_12 = (a_26.Y - b_26.Y), Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12))))))) < 1E-09;
        if ((actual_20 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, true, "at parameter 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_20 = actual_20, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            }) ? ((arg_25 = toString(true), (arg_1_20 = toString(actual_20), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("at parameter 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_20)("at parameter 1"));
        }
        const actual_21 = ((a_28 = Polyline2D__EvaluateAt_5E38073B(plOpen, 0.5), (b_28 = Pt_$ctor_7B00E9A0(5, 0), (vx_13 = (a_28.X - b_28.X), (vy_13 = (a_28.Y - b_28.Y), Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13))))))) < 1E-09;
        if ((actual_21 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, true, "at parameter 0.5");
        }
        else {
            throw new Exception(contains((copyOfStruct_21 = actual_21, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            }) ? ((arg_26 = toString(true), (arg_1_21 = toString(actual_21), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("at parameter 0.5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_21)("at parameter 0.5"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closest point and parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let a_30, b_30, vx_14, vy_14, copyOfStruct_22, arg_27, arg_1_22;
        const testPt = Pt_$ctor_7B00E9A0(5, -1);
        const actual_22 = ((a_30 = Polyline2D__ClosestPoint_6ADE94FD(plOpen, testPt), (b_30 = Pt_$ctor_7B00E9A0(5, 0), (vx_14 = (a_30.X - b_30.X), (vy_14 = (a_30.Y - b_30.Y), Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14))))))) < 1E-09;
        if ((actual_22 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_22, true, "closest point");
        }
        else {
            throw new Exception(contains((copyOfStruct_22 = actual_22, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            }) ? ((arg_27 = toString(true), (arg_1_22 = toString(actual_22), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("closest point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_22)("closest point"));
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
        let a_32, b_32, vx_15, vy_15, copyOfStruct_23, arg_28, arg_1_23, a_34, b_34, vx_16, vy_16, copyOfStruct_24, arg_29, arg_1_24, a_36, b_36, vx_17, vy_17, copyOfStruct_25, arg_30, arg_1_25;
        const scaled = Polyline2D__Scale_5E38073B(plOpen, 2);
        const actual_23 = ((a_32 = item(0, Polyline2D__get_Points(scaled)), (b_32 = Pt_$ctor_7B00E9A0(0, 0), (vx_15 = (a_32.X - b_32.X), (vy_15 = (a_32.Y - b_32.Y), Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15))))))) < 1E-09;
        if ((actual_23 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, true, "first point scaled");
        }
        else {
            throw new Exception(contains((copyOfStruct_23 = actual_23, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_23) => (structuralHash(x_23) | 0),
            }) ? ((arg_28 = toString(true), (arg_1_23 = toString(actual_23), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("first point scaled")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_23)("first point scaled"));
        }
        const actual_24 = ((a_34 = item(1, Polyline2D__get_Points(scaled)), (b_34 = Pt_$ctor_7B00E9A0(20, 0), (vx_16 = (a_34.X - b_34.X), (vy_16 = (a_34.Y - b_34.Y), Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16))))))) < 1E-09;
        if ((actual_24 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, true, "second point scaled");
        }
        else {
            throw new Exception(contains((copyOfStruct_24 = actual_24, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            }) ? ((arg_29 = toString(true), (arg_1_24 = toString(actual_24), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("second point scaled")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_24)("second point scaled"));
        }
        const actual_25 = ((a_36 = item(2, Polyline2D__get_Points(scaled)), (b_36 = Pt_$ctor_7B00E9A0(20, 20), (vx_17 = (a_36.X - b_36.X), (vy_17 = (a_36.Y - b_36.Y), Math.sqrt((vx_17 * vx_17) + (vy_17 * vy_17))))))) < 1E-09;
        if ((actual_25 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, true, "third point scaled");
        }
        else {
            throw new Exception(contains((copyOfStruct_25 = actual_25, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            }) ? ((arg_30 = toString(true), (arg_1_25 = toString(actual_25), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("third point scaled")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_25)("third point scaled"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale on center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_38, b_38, vx_18, vy_18, copyOfStruct_26, arg_31, arg_1_26, a_40, b_40, vx_19, vy_19, copyOfStruct_27, arg_32, arg_1_27;
        const scaled_1 = Polyline2D__ScaleOn(plOpen, Pt_$ctor_7B00E9A0(5, 5), 2);
        const actual_26 = ((a_38 = item(0, Polyline2D__get_Points(scaled_1)), (b_38 = Pt_$ctor_7B00E9A0(-5, -5), (vx_18 = (a_38.X - b_38.X), (vy_18 = (a_38.Y - b_38.Y), Math.sqrt((vx_18 * vx_18) + (vy_18 * vy_18))))))) < 1E-09;
        if ((actual_26 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, true, "first point scaled on center");
        }
        else {
            throw new Exception(contains((copyOfStruct_26 = actual_26, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            }) ? ((arg_31 = toString(true), (arg_1_26 = toString(actual_26), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("first point scaled on center")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_26)("first point scaled on center"));
        }
        const actual_27 = ((a_40 = item(1, Polyline2D__get_Points(scaled_1)), (b_40 = Pt_$ctor_7B00E9A0(15, -5), (vx_19 = (a_40.X - b_40.X), (vy_19 = (a_40.Y - b_40.Y), Math.sqrt((vx_19 * vx_19) + (vy_19 * vy_19))))))) < 1E-09;
        if ((actual_27 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, true, "second point scaled on center");
        }
        else {
            throw new Exception(contains((copyOfStruct_27 = actual_27, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            }) ? ((arg_32 = toString(true), (arg_1_27 = toString(actual_27), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("second point scaled on center")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_27)("second point scaled on center"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_42, b_42, vx_20, vy_20, copyOfStruct_28, arg_33, arg_1_28, a_44, b_44, vx_21, vy_21, copyOfStruct_29, arg_34, arg_1_29;
        const moved = Polyline2D_translate(Vc_$ctor_7B00E9A0(5, 3), plOpen);
        const actual_28 = ((a_42 = item(0, Polyline2D__get_Points(moved)), (b_42 = Pt_$ctor_7B00E9A0(5, 3), (vx_20 = (a_42.X - b_42.X), (vy_20 = (a_42.Y - b_42.Y), Math.sqrt((vx_20 * vx_20) + (vy_20 * vy_20))))))) < 1E-09;
        if ((actual_28 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, true, "first point translated");
        }
        else {
            throw new Exception(contains((copyOfStruct_28 = actual_28, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_28) => (structuralHash(x_28) | 0),
            }) ? ((arg_33 = toString(true), (arg_1_28 = toString(actual_28), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("first point translated")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_28)("first point translated"));
        }
        const actual_29 = ((a_44 = item(1, Polyline2D__get_Points(moved)), (b_44 = Pt_$ctor_7B00E9A0(15, 3), (vx_21 = (a_44.X - b_44.X), (vy_21 = (a_44.Y - b_44.Y), Math.sqrt((vx_21 * vx_21) + (vy_21 * vy_21))))))) < 1E-09;
        if ((actual_29 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, true, "second point translated");
        }
        else {
            throw new Exception(contains((copyOfStruct_29 = actual_29, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_29) => (structuralHash(x_29) | 0),
            }) ? ((arg_34 = toString(true), (arg_1_29 = toString(actual_29), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)("second point translated")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_29)("second point translated"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move X and Y", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_46, b_46, vx_22, vy_22, copyOfStruct_30, arg_35, arg_1_30, a_48, b_48, vx_23, vy_23, copyOfStruct_31, arg_36, arg_1_31;
        const actual_30 = ((a_46 = item(0, Polyline2D__get_Points(Polyline2D_moveX(5, plOpen))), (b_46 = Pt_$ctor_7B00E9A0(5, 0), (vx_22 = (a_46.X - b_46.X), (vy_22 = (a_46.Y - b_46.Y), Math.sqrt((vx_22 * vx_22) + (vy_22 * vy_22))))))) < 1E-09;
        if ((actual_30 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, true, "moved X");
        }
        else {
            throw new Exception(contains((copyOfStruct_30 = actual_30, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_30) => (structuralHash(x_30) | 0),
            }) ? ((arg_35 = toString(true), (arg_1_30 = toString(actual_30), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("moved X")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_30)("moved X"));
        }
        const actual_31 = ((a_48 = item(0, Polyline2D__get_Points(Polyline2D_moveY(3, plOpen))), (b_48 = Pt_$ctor_7B00E9A0(0, 3), (vx_23 = (a_48.X - b_48.X), (vy_23 = (a_48.Y - b_48.Y), Math.sqrt((vx_23 * vx_23) + (vy_23 * vy_23))))))) < 1E-09;
        if ((actual_31 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, true, "moved Y");
        }
        else {
            throw new Exception(contains((copyOfStruct_31 = actual_31, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_31) => (structuralHash(x_31) | 0),
            }) ? ((arg_36 = toString(true), (arg_1_31 = toString(actual_31), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("moved Y")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_31)("moved Y"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("reverse", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let a_50, b_50, vx_24, vy_24, copyOfStruct_32, arg_37, arg_1_32, a_52, b_52, vx_25, vy_25, copyOfStruct_33, arg_38, arg_1_33, a_54, b_54, vx_26, vy_26, copyOfStruct_34, arg_39, arg_1_34;
        const reversed = Polyline2D__Reverse(plOpen);
        const actual_32 = ((a_50 = item(0, Polyline2D__get_Points(reversed)), (b_50 = Pt_$ctor_7B00E9A0(0, 10), (vx_24 = (a_50.X - b_50.X), (vy_24 = (a_50.Y - b_50.Y), Math.sqrt((vx_24 * vx_24) + (vy_24 * vy_24))))))) < 1E-09;
        if ((actual_32 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, true, "first point after reverse");
        }
        else {
            throw new Exception(contains((copyOfStruct_32 = actual_32, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_32) => (structuralHash(x_32) | 0),
            }) ? ((arg_37 = toString(true), (arg_1_32 = toString(actual_32), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_32)("first point after reverse")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_32)("first point after reverse"));
        }
        const actual_33 = ((a_52 = item(3, Polyline2D__get_Points(reversed)), (b_52 = Pt_$ctor_7B00E9A0(0, 0), (vx_25 = (a_52.X - b_52.X), (vy_25 = (a_52.Y - b_52.Y), Math.sqrt((vx_25 * vx_25) + (vy_25 * vy_25))))))) < 1E-09;
        if ((actual_33 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, true, "last point after reverse");
        }
        else {
            throw new Exception(contains((copyOfStruct_33 = actual_33, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_33) => (structuralHash(x_33) | 0),
            }) ? ((arg_38 = toString(true), (arg_1_33 = toString(actual_33), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_38)(arg_1_33)("last point after reverse")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_33)("last point after reverse"));
        }
        const actual_34 = ((a_54 = item(0, Polyline2D__get_Points(plOpen)), (b_54 = Pt_$ctor_7B00E9A0(0, 0), (vx_26 = (a_54.X - b_54.X), (vy_26 = (a_54.Y - b_54.Y), Math.sqrt((vx_26 * vx_26) + (vy_26 * vy_26))))))) < 1E-09;
        if ((actual_34 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, true, "original unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_34 = actual_34, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_34) => (structuralHash(x_34) | 0),
            }) ? ((arg_39 = toString(true), (arg_1_34 = toString(actual_34), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_39)(arg_1_34)("original unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_34)("original unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("reverse in place", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let a_56, b_56, vx_27, vy_27, copyOfStruct_35, arg_40, arg_1_35, a_58, b_58, vx_28, vy_28, copyOfStruct_36, arg_41, arg_1_36;
        const copy = Polyline2D__Clone(plOpen);
        Polyline2D__ReverseInPlace(copy);
        const actual_35 = ((a_56 = item(0, Polyline2D__get_Points(copy)), (b_56 = Pt_$ctor_7B00E9A0(0, 10), (vx_27 = (a_56.X - b_56.X), (vy_27 = (a_56.Y - b_56.Y), Math.sqrt((vx_27 * vx_27) + (vy_27 * vy_27))))))) < 1E-09;
        if ((actual_35 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, true, "first point after reverse in place");
        }
        else {
            throw new Exception(contains((copyOfStruct_35 = actual_35, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_35) => (structuralHash(x_35) | 0),
            }) ? ((arg_40 = toString(true), (arg_1_35 = toString(actual_35), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_40)(arg_1_35)("first point after reverse in place")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_35)("first point after reverse in place"));
        }
        const actual_36 = ((a_58 = item(3, Polyline2D__get_Points(copy)), (b_58 = Pt_$ctor_7B00E9A0(0, 0), (vx_28 = (a_58.X - b_58.X), (vy_28 = (a_58.Y - b_58.Y), Math.sqrt((vx_28 * vx_28) + (vy_28 * vy_28))))))) < 1E-09;
        if ((actual_36 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, true, "last point after reverse in place");
        }
        else {
            throw new Exception(contains((copyOfStruct_36 = actual_36, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_36) => (structuralHash(x_36) | 0),
            }) ? ((arg_41 = toString(true), (arg_1_36 = toString(actual_36), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_41)(arg_1_36)("last point after reverse in place")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_36)("last point after reverse in place"));
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
        let copyOfStruct_37, arg_42, arg_1_37, copyOfStruct_38, arg_43, arg_1_38;
        let square;
        const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
        square = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
        Expect_isTrue(Polyline2D__WindingNumber_6ADE94FD(square, Pt_$ctor_7B00E9A0(5, 5)) !== 0)("winding number for inside point should be non-zero");
        const actual_37 = Polyline2D__WindingNumber_6ADE94FD(square, Pt_$ctor_7B00E9A0(15, 5)) | 0;
        if ((actual_37 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, 0, "winding number for outside point should be zero");
        }
        else {
            throw new Exception(contains((copyOfStruct_37 = actual_37, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_37) => (structuralHash(x_37) | 0),
            }) ? ((arg_42 = int32ToString(0), (arg_1_37 = int32ToString(actual_37), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_42)(arg_1_37)("winding number for outside point should be zero")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_37)("winding number for outside point should be zero"));
        }
        const actual_38 = Polyline2D__WindingNumber_6ADE94FD(Polyline2D_$ctor(), Pt_$ctor_7B00E9A0(5, 5)) | 0;
        if ((actual_38 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, 0, "winding number for empty polyline should be zero");
        }
        else {
            throw new Exception(contains((copyOfStruct_38 = actual_38, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_38) => (structuralHash(x_38) | 0),
            }) ? ((arg_43 = int32ToString(0), (arg_1_38 = int32ToString(actual_38), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_43)(arg_1_38)("winding number for empty polyline should be zero")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_38)("winding number for empty polyline should be zero"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})()])), Test_testList("Creation and Manipulation", ofArray([(() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("clone and duplicate", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let copyOfStruct_39, arg_44, arg_1_39, copyOfStruct_40, arg_45, arg_1_40, a_60, b_60, vx_29, vy_29, copyOfStruct_41, arg_46, arg_1_41, a_62, b_62, vx_30, vy_30, copyOfStruct_42, arg_47, arg_1_42;
        const cloned = Polyline2D__Clone(plOpen);
        const duplicated = Polyline2D__Duplicate(plOpen);
        const actual_40 = Polyline2D__get_PointCount(cloned) | 0;
        const expected_75 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_40 === expected_75) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_40, expected_75, "cloned point count");
        }
        else {
            throw new Exception(contains((copyOfStruct_39 = actual_40, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_39) => (structuralHash(x_39) | 0),
            }) ? ((arg_44 = int32ToString(expected_75), (arg_1_39 = int32ToString(actual_40), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_44)(arg_1_39)("cloned point count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_75)(actual_40)("cloned point count"));
        }
        const actual_42 = Polyline2D__get_PointCount(duplicated) | 0;
        const expected_77 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_42 === expected_77) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_42, expected_77, "duplicated point count");
        }
        else {
            throw new Exception(contains((copyOfStruct_40 = actual_42, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_40) => (structuralHash(x_40) | 0),
            }) ? ((arg_45 = int32ToString(expected_77), (arg_1_40 = int32ToString(actual_42), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_45)(arg_1_40)("duplicated point count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_77)(actual_42)("duplicated point count"));
        }
        const actual_43 = ((a_60 = item(0, Polyline2D__get_Points(cloned)), (b_60 = item(0, Polyline2D__get_Points(plOpen)), (vx_29 = (a_60.X - b_60.X), (vy_29 = (a_60.Y - b_60.Y), Math.sqrt((vx_29 * vx_29) + (vy_29 * vy_29))))))) < 1E-09;
        if ((actual_43 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_43, true, "cloned first point");
        }
        else {
            throw new Exception(contains((copyOfStruct_41 = actual_43, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_41) => (structuralHash(x_41) | 0),
            }) ? ((arg_46 = toString(true), (arg_1_41 = toString(actual_43), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_46)(arg_1_41)("cloned first point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_43)("cloned first point"));
        }
        const actual_44 = ((a_62 = item(0, Polyline2D__get_Points(duplicated)), (b_62 = item(0, Polyline2D__get_Points(plOpen)), (vx_30 = (a_62.X - b_62.X), (vy_30 = (a_62.Y - b_62.Y), Math.sqrt((vx_30 * vx_30) + (vy_30 * vy_30))))))) < 1E-09;
        if ((actual_44 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_44, true, "duplicated first point");
        }
        else {
            throw new Exception(contains((copyOfStruct_42 = actual_44, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_42) => (structuralHash(x_42) | 0),
            }) ? ((arg_47 = toString(true), (arg_1_42 = toString(actual_44), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_47)(arg_1_42)("duplicated first point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_44)("duplicated first point"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close if open", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let copyOfStruct_43, arg_48, arg_1_43, a_64, b_64, vx_31, vy_31, copyOfStruct_44, arg_49, arg_1_44;
        const copy_1 = Polyline2D__Clone(plOpen);
        Polyline2D__CloseInPlace_5E38073B(copy_1, 1E-06);
        const actual_46 = Polyline2D__get_PointCount(copy_1) | 0;
        const expected_83 = (Polyline2D__get_PointCount(plOpen) + 1) | 0;
        if ((actual_46 === expected_83) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_46, expected_83, "closed polyline point count");
        }
        else {
            throw new Exception(contains((copyOfStruct_43 = actual_46, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_43) => (structuralHash(x_43) | 0),
            }) ? ((arg_48 = int32ToString(expected_83), (arg_1_43 = int32ToString(actual_46), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_48)(arg_1_43)("closed polyline point count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_83)(actual_46)("closed polyline point count"));
        }
        const actual_47 = ((a_64 = item(Polyline2D__get_PointCount(copy_1) - 1, Polyline2D__get_Points(copy_1)), (b_64 = item(0, Polyline2D__get_Points(copy_1)), (vx_31 = (a_64.X - b_64.X), (vy_31 = (a_64.Y - b_64.Y), Math.sqrt((vx_31 * vx_31) + (vy_31 * vy_31))))))) < 1E-09;
        if ((actual_47 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_47, true, "last point equals first");
        }
        else {
            throw new Exception(contains((copyOfStruct_44 = actual_47, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_44) => (structuralHash(x_44) | 0),
            }) ? ((arg_49 = toString(true), (arg_1_44 = toString(actual_47), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_49)(arg_1_44)("last point equals first")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_47)("last point equals first"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("sub polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let copyOfStruct_45, arg_50, arg_1_45, a_67, b_67, vx_32, vy_32, copyOfStruct_46, arg_51, arg_1_46, a_69, b_69, vx_33, vy_33, copyOfStruct_47, arg_52, arg_1_47;
        const sub = Polyline2D_subPolyline(0.5, 2.5, plOpen);
        const actual_49 = Polyline2D__get_PointCount(sub) | 0;
        if ((actual_49 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_49, 4, "sub polyline point count");
        }
        else {
            throw new Exception(contains((copyOfStruct_45 = actual_49, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_45) => (structuralHash(x_45) | 0),
            }) ? ((arg_50 = int32ToString(4), (arg_1_45 = int32ToString(actual_49), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_50)(arg_1_45)("sub polyline point count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_49)("sub polyline point count"));
        }
        const actual_50 = ((a_67 = item(0, Polyline2D__get_Points(sub)), (b_67 = Pt_$ctor_7B00E9A0(5, 0), (vx_32 = (a_67.X - b_67.X), (vy_32 = (a_67.Y - b_67.Y), Math.sqrt((vx_32 * vx_32) + (vy_32 * vy_32))))))) < 1E-09;
        if ((actual_50 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_50, true, "sub polyline start");
        }
        else {
            throw new Exception(contains((copyOfStruct_46 = actual_50, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_46) => (structuralHash(x_46) | 0),
            }) ? ((arg_51 = toString(true), (arg_1_46 = toString(actual_50), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_51)(arg_1_46)("sub polyline start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_50)("sub polyline start"));
        }
        const actual_51 = ((a_69 = item(3, Polyline2D__get_Points(sub)), (b_69 = Pt_$ctor_7B00E9A0(5, 10), (vx_33 = (a_69.X - b_69.X), (vy_33 = (a_69.Y - b_69.Y), Math.sqrt((vx_33 * vx_33) + (vy_33 * vy_33))))))) < 1E-09;
        if ((actual_51 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_51, true, "sub polyline end");
        }
        else {
            throw new Exception(contains((copyOfStruct_47 = actual_51, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_47) => (structuralHash(x_47) | 0),
            }) ? ((arg_52 = toString(true), (arg_1_47 = toString(actual_51), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_52)(arg_1_47)("sub polyline end")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_51)("sub polyline end"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})()])), Test_testList("Error Handling", ofArray([(() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("empty polyline errors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let copyOfStruct_48, arg_53, arg_1_48;
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
            throw new Exception(contains((copyOfStruct_48 = actual_53, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_48) => (structuralHash(x_48) | 0),
            }) ? ((arg_53 = (0).toString(), (arg_1_48 = actual_53.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_53)(arg_1_48)("length on empty =0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_53)("length on empty =0"));
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
        let copyOfStruct_49, arg_54, arg_1_49, copyOfStruct_50, arg_55, arg_1_50;
        let fromSeq;
        const points_1 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 1)];
        fromSeq = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_1));
        const empty_1 = Polyline2D_$ctor_Z5FD8CF3C([]);
        const actual_54 = Polyline2D__get_PointCount(fromSeq) | 0;
        if ((actual_54 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_54, 2, "created from sequence has correct count");
        }
        else {
            throw new Exception(contains((copyOfStruct_49 = actual_54, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_49) => (structuralHash(x_49) | 0),
            }) ? ((arg_54 = int32ToString(2), (arg_1_49 = int32ToString(actual_54), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_54)(arg_1_49)("created from sequence has correct count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_54)("created from sequence has correct count"));
        }
        const actual_55 = Polyline2D__get_PointCount(empty_1) | 0;
        if ((actual_55 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_55, 0, "empty has no points");
        }
        else {
            throw new Exception(contains((copyOfStruct_50 = actual_55, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_50) => (structuralHash(x_50) | 0),
            }) ? ((arg_55 = int32ToString(0), (arg_1_50 = int32ToString(actual_55), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_55)(arg_1_50)("empty has no points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_55)("empty has no points"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("map function", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let a_72, b_71, vx_34, vy_34, copyOfStruct_51, arg_56, arg_1_51, a_74, b_73, vx_35, vy_35, copyOfStruct_52, arg_57, arg_1_52;
        const scaled_2 = Polyline2D_map((pt) => {
            const a_70 = pt;
            return Pt_$ctor_7B00E9A0_1(a_70.X * 2, a_70.Y * 2);
        }, plOpen);
        const actual_56 = ((a_72 = item(0, Polyline2D__get_Points(scaled_2)), (b_71 = Pt_$ctor_7B00E9A0(0, 0), (vx_34 = (a_72.X - b_71.X), (vy_34 = (a_72.Y - b_71.Y), Math.sqrt((vx_34 * vx_34) + (vy_34 * vy_34))))))) < 1E-09;
        if ((actual_56 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_56, true, "mapped first point");
        }
        else {
            throw new Exception(contains((copyOfStruct_51 = actual_56, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_51) => (structuralHash(x_51) | 0),
            }) ? ((arg_56 = toString(true), (arg_1_51 = toString(actual_56), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_56)(arg_1_51)("mapped first point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_56)("mapped first point"));
        }
        const actual_57 = ((a_74 = item(1, Polyline2D__get_Points(scaled_2)), (b_73 = Pt_$ctor_7B00E9A0(20, 0), (vx_35 = (a_74.X - b_73.X), (vy_35 = (a_74.Y - b_73.Y), Math.sqrt((vx_35 * vx_35) + (vy_35 * vy_35))))))) < 1E-09;
        if ((actual_57 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_57, true, "mapped second point");
        }
        else {
            throw new Exception(contains((copyOfStruct_52 = actual_57, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_52) => (structuralHash(x_52) | 0),
            }) ? ((arg_57 = toString(true), (arg_1_52 = toString(actual_57), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_57)(arg_1_52)("mapped second point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_57)("mapped second point"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let a_76, rad, b_75, vx_36, vy_36, copyOfStruct_53, arg_58, arg_1_53;
        const actual_58 = ((a_76 = item(1, Polyline2D__get_Points(Polyline2D_rotate((rad = (3.141592653589793 / 2), Rotation2D_$ctor_7B00E9A0(Math.sin(rad), Math.cos(rad))), plLine))), (b_75 = Pt_$ctor_7B00E9A0(0, 10), (vx_36 = (a_76.X - b_75.X), (vy_36 = (a_76.Y - b_75.Y), Math.sqrt((vx_36 * vx_36) + (vy_36 * vy_36))))))) < 1E-09;
        if ((actual_58 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_58, true, "rotated point");
        }
        else {
            throw new Exception(contains((copyOfStruct_53 = actual_58, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_53) => (structuralHash(x_53) | 0),
            }) ? ((arg_58 = toString(true), (arg_1_53 = toString(actual_58), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_58)(arg_1_53)("rotated point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_58)("rotated point"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})()])), Test_testList("Offsetting", ofArray([(() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("5 points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let a_78, b_77, vx_37, vy_37, copyOfStruct_54, arg_59, arg_1_54, a_80, b_79, vx_38, vy_38, copyOfStruct_55, arg_60, arg_1_55, a_82, b_81, vx_39, vy_39, copyOfStruct_56, arg_61, arg_1_56, a_84, b_83, vx_40, vy_40, copyOfStruct_57, arg_62, arg_1_57, a_86, b_85, vx_41, vy_41, copyOfStruct_58, arg_63, arg_1_58;
        const o = Polyline2D_offset_Z45C468A5(plClosed, 2, false, true, 1, -0.9961946980917455);
        const actual_59 = ((a_78 = item(0, Polyline2D__get_Points(o)), (b_77 = Pt_$ctor_7B00E9A0(2, 2), (vx_37 = (a_78.X - b_77.X), (vy_37 = (a_78.Y - b_77.Y), Math.sqrt((vx_37 * vx_37) + (vy_37 * vy_37))))))) < 1E-09;
        if ((actual_59 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_59, true, "pt 0 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_54 = actual_59, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_54) => (structuralHash(x_54) | 0),
            }) ? ((arg_59 = toString(true), (arg_1_54 = toString(actual_59), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_59)(arg_1_54)("pt 0 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_59)("pt 0 ok"));
        }
        const actual_60 = ((a_80 = item(1, Polyline2D__get_Points(o)), (b_79 = Pt_$ctor_7B00E9A0(8, 2), (vx_38 = (a_80.X - b_79.X), (vy_38 = (a_80.Y - b_79.Y), Math.sqrt((vx_38 * vx_38) + (vy_38 * vy_38))))))) < 1E-09;
        if ((actual_60 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_60, true, "pt 1 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_55 = actual_60, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_55) => (structuralHash(x_55) | 0),
            }) ? ((arg_60 = toString(true), (arg_1_55 = toString(actual_60), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_60)(arg_1_55)("pt 1 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_60)("pt 1 ok"));
        }
        const actual_61 = ((a_82 = item(2, Polyline2D__get_Points(o)), (b_81 = Pt_$ctor_7B00E9A0(8, 8), (vx_39 = (a_82.X - b_81.X), (vy_39 = (a_82.Y - b_81.Y), Math.sqrt((vx_39 * vx_39) + (vy_39 * vy_39))))))) < 1E-09;
        if ((actual_61 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_61, true, "pt 2 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_56 = actual_61, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_56) => (structuralHash(x_56) | 0),
            }) ? ((arg_61 = toString(true), (arg_1_56 = toString(actual_61), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_61)(arg_1_56)("pt 2 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_61)("pt 2 ok"));
        }
        const actual_62 = ((a_84 = item(3, Polyline2D__get_Points(o)), (b_83 = Pt_$ctor_7B00E9A0(2, 8), (vx_40 = (a_84.X - b_83.X), (vy_40 = (a_84.Y - b_83.Y), Math.sqrt((vx_40 * vx_40) + (vy_40 * vy_40))))))) < 1E-09;
        if ((actual_62 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_62, true, "pt 3 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_57 = actual_62, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_57) => (structuralHash(x_57) | 0),
            }) ? ((arg_62 = toString(true), (arg_1_57 = toString(actual_62), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_62)(arg_1_57)("pt 3 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_62)("pt 3 ok"));
        }
        const actual_63 = ((a_86 = item(4, Polyline2D__get_Points(o)), (b_85 = Pt_$ctor_7B00E9A0(2, 2), (vx_41 = (a_86.X - b_85.X), (vy_41 = (a_86.Y - b_85.Y), Math.sqrt((vx_41 * vx_41) + (vy_41 * vy_41))))))) < 1E-09;
        if ((actual_63 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_63, true, "pt 4 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_58 = actual_63, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_58) => (structuralHash(x_58) | 0),
            }) ? ((arg_63 = toString(true), (arg_1_58 = toString(actual_63), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_63)(arg_1_58)("pt 4 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_63)("pt 4 ok"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points open", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let a_88, b_87, vx_42, vy_42, copyOfStruct_59, arg_64, arg_1_59, a_90, b_89, vx_43, vy_43, copyOfStruct_60, arg_65, arg_1_60, a_92, b_91, vx_44, vy_44, copyOfStruct_61, arg_66, arg_1_61, a_94, b_93, vx_45, vy_45, copyOfStruct_62, arg_67, arg_1_62;
        const o_1 = Polyline2D_offset_Z45C468A5(plOpen, 2, false, true, 1, -0.9961946980917455);
        const actual_64 = ((a_88 = item(0, Polyline2D__get_Points(o_1)), (b_87 = Pt_$ctor_7B00E9A0(0, 2), (vx_42 = (a_88.X - b_87.X), (vy_42 = (a_88.Y - b_87.Y), Math.sqrt((vx_42 * vx_42) + (vy_42 * vy_42))))))) < 1E-09;
        if ((actual_64 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_64, true, "pt 0 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_59 = actual_64, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_59) => (structuralHash(x_59) | 0),
            }) ? ((arg_64 = toString(true), (arg_1_59 = toString(actual_64), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_64)(arg_1_59)("pt 0 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_64)("pt 0 ok"));
        }
        const actual_65 = ((a_90 = item(1, Polyline2D__get_Points(o_1)), (b_89 = Pt_$ctor_7B00E9A0(8, 2), (vx_43 = (a_90.X - b_89.X), (vy_43 = (a_90.Y - b_89.Y), Math.sqrt((vx_43 * vx_43) + (vy_43 * vy_43))))))) < 1E-09;
        if ((actual_65 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_65, true, "pt 1 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_60 = actual_65, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_60) => (structuralHash(x_60) | 0),
            }) ? ((arg_65 = toString(true), (arg_1_60 = toString(actual_65), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_65)(arg_1_60)("pt 1 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_65)("pt 1 ok"));
        }
        const actual_66 = ((a_92 = item(2, Polyline2D__get_Points(o_1)), (b_91 = Pt_$ctor_7B00E9A0(8, 8), (vx_44 = (a_92.X - b_91.X), (vy_44 = (a_92.Y - b_91.Y), Math.sqrt((vx_44 * vx_44) + (vy_44 * vy_44))))))) < 1E-09;
        if ((actual_66 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_66, true, "pt 2 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_61 = actual_66, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_61) => (structuralHash(x_61) | 0),
            }) ? ((arg_66 = toString(true), (arg_1_61 = toString(actual_66), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_66)(arg_1_61)("pt 2 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_66)("pt 2 ok"));
        }
        const actual_67 = ((a_94 = item(3, Polyline2D__get_Points(o_1)), (b_93 = Pt_$ctor_7B00E9A0(0, 8), (vx_45 = (a_94.X - b_93.X), (vy_45 = (a_94.Y - b_93.Y), Math.sqrt((vx_45 * vx_45) + (vy_45 * vy_45))))))) < 1E-09;
        if ((actual_67 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_67, true, "pt 3 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_62 = actual_67, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_62) => (structuralHash(x_62) | 0),
            }) ? ((arg_67 = toString(true), (arg_1_62 = toString(actual_67), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_67)(arg_1_62)("pt 3 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_67)("pt 3 ok"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points looped", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let a_96, b_95, vx_46, vy_46, copyOfStruct_63, arg_68, arg_1_63, a_98, b_97, vx_47, vy_47, copyOfStruct_64, arg_69, arg_1_64, a_100, b_99, vx_48, vy_48, copyOfStruct_65, arg_70, arg_1_65, a_102, b_101, vx_49, vy_49, copyOfStruct_66, arg_71, arg_1_66;
        const o_2 = Polyline2D_offset_Z45C468A5(plOpen, 2, true, true, 1, -0.9961946980917455);
        const actual_68 = ((a_96 = item(0, Polyline2D__get_Points(o_2)), (b_95 = Pt_$ctor_7B00E9A0(2, 2), (vx_46 = (a_96.X - b_95.X), (vy_46 = (a_96.Y - b_95.Y), Math.sqrt((vx_46 * vx_46) + (vy_46 * vy_46))))))) < 1E-09;
        if ((actual_68 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_68, true, "pt 0 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_63 = actual_68, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_63) => (structuralHash(x_63) | 0),
            }) ? ((arg_68 = toString(true), (arg_1_63 = toString(actual_68), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_68)(arg_1_63)("pt 0 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_68)("pt 0 ok"));
        }
        const actual_69 = ((a_98 = item(1, Polyline2D__get_Points(o_2)), (b_97 = Pt_$ctor_7B00E9A0(8, 2), (vx_47 = (a_98.X - b_97.X), (vy_47 = (a_98.Y - b_97.Y), Math.sqrt((vx_47 * vx_47) + (vy_47 * vy_47))))))) < 1E-09;
        if ((actual_69 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_69, true, "pt 1 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_64 = actual_69, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_64) => (structuralHash(x_64) | 0),
            }) ? ((arg_69 = toString(true), (arg_1_64 = toString(actual_69), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_69)(arg_1_64)("pt 1 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_69)("pt 1 ok"));
        }
        const actual_70 = ((a_100 = item(2, Polyline2D__get_Points(o_2)), (b_99 = Pt_$ctor_7B00E9A0(8, 8), (vx_48 = (a_100.X - b_99.X), (vy_48 = (a_100.Y - b_99.Y), Math.sqrt((vx_48 * vx_48) + (vy_48 * vy_48))))))) < 1E-09;
        if ((actual_70 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_70, true, "pt 2 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_65 = actual_70, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_65) => (structuralHash(x_65) | 0),
            }) ? ((arg_70 = toString(true), (arg_1_65 = toString(actual_70), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_70)(arg_1_65)("pt 2 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_70)("pt 2 ok"));
        }
        const actual_71 = ((a_102 = item(3, Polyline2D__get_Points(o_2)), (b_101 = Pt_$ctor_7B00E9A0(2, 8), (vx_49 = (a_102.X - b_101.X), (vy_49 = (a_102.Y - b_101.Y), Math.sqrt((vx_49 * vx_49) + (vy_49 * vy_49))))))) < 1E-09;
        if ((actual_71 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_71, true, "pt 3 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_66 = actual_71, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_66) => (structuralHash(x_66) | 0),
            }) ? ((arg_71 = toString(true), (arg_1_66 = toString(actual_71), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_71)(arg_1_66)("pt 3 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_71)("pt 3 ok"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points looped  2 loop", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let a_104, b_103, vx_50, vy_50, copyOfStruct_67, arg_72, arg_1_67, a_106, b_105, vx_51, vy_51, copyOfStruct_68, arg_73, arg_1_68, a_108, b_107, vx_52, vy_52, copyOfStruct_69, arg_74, arg_1_69, a_110, b_109, vx_53, vy_53, copyOfStruct_70, arg_75, arg_1_70;
        const o_3 = Polyline2D_offset_Z45C468A5(plOpen, 2, true, true, 1, -0.9961946980917455);
        const actual_72 = ((a_104 = item(0, Polyline2D__get_Points(o_3)), (b_103 = Pt_$ctor_7B00E9A0(2, 2), (vx_50 = (a_104.X - b_103.X), (vy_50 = (a_104.Y - b_103.Y), Math.sqrt((vx_50 * vx_50) + (vy_50 * vy_50))))))) < 1E-09;
        if ((actual_72 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_72, true, "pt 0 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_67 = actual_72, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_67) => (structuralHash(x_67) | 0),
            }) ? ((arg_72 = toString(true), (arg_1_67 = toString(actual_72), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_72)(arg_1_67)("pt 0 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_72)("pt 0 ok"));
        }
        const actual_73 = ((a_106 = item(1, Polyline2D__get_Points(o_3)), (b_105 = Pt_$ctor_7B00E9A0(8, 2), (vx_51 = (a_106.X - b_105.X), (vy_51 = (a_106.Y - b_105.Y), Math.sqrt((vx_51 * vx_51) + (vy_51 * vy_51))))))) < 1E-09;
        if ((actual_73 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_73, true, "pt 1 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_68 = actual_73, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_68) => (structuralHash(x_68) | 0),
            }) ? ((arg_73 = toString(true), (arg_1_68 = toString(actual_73), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_73)(arg_1_68)("pt 1 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_73)("pt 1 ok"));
        }
        const actual_74 = ((a_108 = item(2, Polyline2D__get_Points(o_3)), (b_107 = Pt_$ctor_7B00E9A0(8, 8), (vx_52 = (a_108.X - b_107.X), (vy_52 = (a_108.Y - b_107.Y), Math.sqrt((vx_52 * vx_52) + (vy_52 * vy_52))))))) < 1E-09;
        if ((actual_74 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_74, true, "pt 2 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_69 = actual_74, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_69) => (structuralHash(x_69) | 0),
            }) ? ((arg_74 = toString(true), (arg_1_69 = toString(actual_74), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_74)(arg_1_69)("pt 2 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_74)("pt 2 ok"));
        }
        const actual_75 = ((a_110 = item(3, Polyline2D__get_Points(o_3)), (b_109 = Pt_$ctor_7B00E9A0(2, 8), (vx_53 = (a_110.X - b_109.X), (vy_53 = (a_110.Y - b_109.Y), Math.sqrt((vx_53 * vx_53) + (vy_53 * vy_53))))))) < 1E-09;
        if ((actual_75 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_75, true, "pt 3 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_70 = actual_75, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_70) => (structuralHash(x_70) | 0),
            }) ? ((arg_75 = toString(true), (arg_1_70 = toString(actual_75), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_75)(arg_1_70)("pt 3 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_75)("pt 3 ok"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points looped -2 loop", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let a_112, b_111, vx_54, vy_54, copyOfStruct_71, arg_76, arg_1_71, a_114, b_113, vx_55, vy_55, copyOfStruct_72, arg_77, arg_1_72, a_116, b_115, vx_56, vy_56, copyOfStruct_73, arg_78, arg_1_73, a_118, b_117, vx_57, vy_57, copyOfStruct_74, arg_79, arg_1_74;
        const o_4 = Polyline2D_offset_Z45C468A5(plOpen, -2, true, true, 1, -0.9961946980917455);
        const actual_76 = ((a_112 = item(0, Polyline2D__get_Points(o_4)), (b_111 = Pt_$ctor_7B00E9A0(2, 2), (vx_54 = (a_112.X - b_111.X), (vy_54 = (a_112.Y - b_111.Y), Math.sqrt((vx_54 * vx_54) + (vy_54 * vy_54))))))) < 1E-09;
        if ((actual_76 === false) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_76, false, "pt 0 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_71 = actual_76, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_71) => (structuralHash(x_71) | 0),
            }) ? ((arg_76 = toString(false), (arg_1_71 = toString(actual_76), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_76)(arg_1_71)("pt 0 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(false)(actual_76)("pt 0 ok"));
        }
        const actual_77 = ((a_114 = item(1, Polyline2D__get_Points(o_4)), (b_113 = Pt_$ctor_7B00E9A0(8, 2), (vx_55 = (a_114.X - b_113.X), (vy_55 = (a_114.Y - b_113.Y), Math.sqrt((vx_55 * vx_55) + (vy_55 * vy_55))))))) < 1E-09;
        if ((actual_77 === false) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_77, false, "pt 1 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_72 = actual_77, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_72) => (structuralHash(x_72) | 0),
            }) ? ((arg_77 = toString(false), (arg_1_72 = toString(actual_77), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_77)(arg_1_72)("pt 1 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(false)(actual_77)("pt 1 ok"));
        }
        const actual_78 = ((a_116 = item(2, Polyline2D__get_Points(o_4)), (b_115 = Pt_$ctor_7B00E9A0(8, 8), (vx_56 = (a_116.X - b_115.X), (vy_56 = (a_116.Y - b_115.Y), Math.sqrt((vx_56 * vx_56) + (vy_56 * vy_56))))))) < 1E-09;
        if ((actual_78 === false) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_78, false, "pt 2 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_73 = actual_78, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_73) => (structuralHash(x_73) | 0),
            }) ? ((arg_78 = toString(false), (arg_1_73 = toString(actual_78), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_78)(arg_1_73)("pt 2 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(false)(actual_78)("pt 2 ok"));
        }
        const actual_79 = ((a_118 = item(3, Polyline2D__get_Points(o_4)), (b_117 = Pt_$ctor_7B00E9A0(2, 8), (vx_57 = (a_118.X - b_117.X), (vy_57 = (a_118.Y - b_117.Y), Math.sqrt((vx_57 * vx_57) + (vy_57 * vy_57))))))) < 1E-09;
        if ((actual_79 === false) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_79, false, "pt 3 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_74 = actual_79, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_74) => (structuralHash(x_74) | 0),
            }) ? ((arg_79 = toString(false), (arg_1_74 = toString(actual_79), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_79)(arg_1_74)("pt 3 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(false)(actual_79)("pt 3 ok"));
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
        let a_120, b_119, vx_58, vy_58, copyOfStruct_75, arg_80, arg_1_75, a_122, b_121, vx_59, vy_59, copyOfStruct_76, arg_81, arg_1_76, a_124, b_123, vx_60, vy_60, copyOfStruct_77, arg_82, arg_1_77, a_126, b_125, vx_61, vy_61, copyOfStruct_78, arg_83, arg_1_78;
        const o_5 = Polyline2D_offsetVar_357F0A77(plOpen, new Float64Array([4, 2, 2, 2]), true, true, 1, 1, 0.9961946980917455, -0.9961946980917455);
        const actual_80 = ((a_120 = item(0, Polyline2D__get_Points(o_5)), (b_119 = Pt_$ctor_7B00E9A0(2, 4), (vx_58 = (a_120.X - b_119.X), (vy_58 = (a_120.Y - b_119.Y), Math.sqrt((vx_58 * vx_58) + (vy_58 * vy_58))))))) < 1E-09;
        if ((actual_80 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_80, true, "pt 0 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_75 = actual_80, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_75) => (structuralHash(x_75) | 0),
            }) ? ((arg_80 = toString(true), (arg_1_75 = toString(actual_80), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_80)(arg_1_75)("pt 0 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_80)("pt 0 ok"));
        }
        const actual_81 = ((a_122 = item(1, Polyline2D__get_Points(o_5)), (b_121 = Pt_$ctor_7B00E9A0(8, 4), (vx_59 = (a_122.X - b_121.X), (vy_59 = (a_122.Y - b_121.Y), Math.sqrt((vx_59 * vx_59) + (vy_59 * vy_59))))))) < 1E-09;
        if ((actual_81 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_81, true, "pt 1 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_76 = actual_81, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_76) => (structuralHash(x_76) | 0),
            }) ? ((arg_81 = toString(true), (arg_1_76 = toString(actual_81), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_81)(arg_1_76)("pt 1 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_81)("pt 1 ok"));
        }
        const actual_82 = ((a_124 = item(2, Polyline2D__get_Points(o_5)), (b_123 = Pt_$ctor_7B00E9A0(8, 8), (vx_60 = (a_124.X - b_123.X), (vy_60 = (a_124.Y - b_123.Y), Math.sqrt((vx_60 * vx_60) + (vy_60 * vy_60))))))) < 1E-09;
        if ((actual_82 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_82, true, "pt 2 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_77 = actual_82, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_77) => (structuralHash(x_77) | 0),
            }) ? ((arg_82 = toString(true), (arg_1_77 = toString(actual_82), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_82)(arg_1_77)("pt 2 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_82)("pt 2 ok"));
        }
        const actual_83 = ((a_126 = item(3, Polyline2D__get_Points(o_5)), (b_125 = Pt_$ctor_7B00E9A0(2, 8), (vx_61 = (a_126.X - b_125.X), (vy_61 = (a_126.Y - b_125.Y), Math.sqrt((vx_61 * vx_61) + (vy_61 * vy_61))))))) < 1E-09;
        if ((actual_83 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_83, true, "pt 3 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_78 = actual_83, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_78) => (structuralHash(x_78) | 0),
            }) ? ((arg_83 = toString(true), (arg_1_78 = toString(actual_83), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_83)(arg_1_78)("pt 3 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_83)("pt 3 ok"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("4 points open, 3 params", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let a_128, b_127, vx_62, vy_62, copyOfStruct_79, arg_84, arg_1_79, a_130, b_129, vx_63, vy_63, copyOfStruct_80, arg_85, arg_1_80, a_132, b_131, vx_64, vy_64, copyOfStruct_81, arg_86, arg_1_81, a_134, b_133, vx_65, vy_65, copyOfStruct_82, arg_87, arg_1_82;
        const o_6 = Polyline2D_offsetVar_357F0A77(plOpen, new Float64Array([4, 2, 2]), false, true, 1, 1, 0.9961946980917455, -0.9961946980917455);
        const actual_84 = ((a_128 = item(0, Polyline2D__get_Points(o_6)), (b_127 = Pt_$ctor_7B00E9A0(0, 4), (vx_62 = (a_128.X - b_127.X), (vy_62 = (a_128.Y - b_127.Y), Math.sqrt((vx_62 * vx_62) + (vy_62 * vy_62))))))) < 1E-09;
        if ((actual_84 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_84, true, "pt 0 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_79 = actual_84, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_79) => (structuralHash(x_79) | 0),
            }) ? ((arg_84 = toString(true), (arg_1_79 = toString(actual_84), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_84)(arg_1_79)("pt 0 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_84)("pt 0 ok"));
        }
        const actual_85 = ((a_130 = item(1, Polyline2D__get_Points(o_6)), (b_129 = Pt_$ctor_7B00E9A0(8, 4), (vx_63 = (a_130.X - b_129.X), (vy_63 = (a_130.Y - b_129.Y), Math.sqrt((vx_63 * vx_63) + (vy_63 * vy_63))))))) < 1E-09;
        if ((actual_85 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_85, true, "pt 1 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_80 = actual_85, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_80) => (structuralHash(x_80) | 0),
            }) ? ((arg_85 = toString(true), (arg_1_80 = toString(actual_85), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_85)(arg_1_80)("pt 1 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_85)("pt 1 ok"));
        }
        const actual_86 = ((a_132 = item(2, Polyline2D__get_Points(o_6)), (b_131 = Pt_$ctor_7B00E9A0(8, 8), (vx_64 = (a_132.X - b_131.X), (vy_64 = (a_132.Y - b_131.Y), Math.sqrt((vx_64 * vx_64) + (vy_64 * vy_64))))))) < 1E-09;
        if ((actual_86 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_86, true, "pt 2 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_81 = actual_86, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_81) => (structuralHash(x_81) | 0),
            }) ? ((arg_86 = toString(true), (arg_1_81 = toString(actual_86), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_86)(arg_1_81)("pt 2 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_86)("pt 2 ok"));
        }
        const actual_87 = ((a_134 = item(3, Polyline2D__get_Points(o_6)), (b_133 = Pt_$ctor_7B00E9A0(0, 8), (vx_65 = (a_134.X - b_133.X), (vy_65 = (a_134.Y - b_133.Y), Math.sqrt((vx_65 * vx_65) + (vy_65 * vy_65))))))) < 1E-09;
        if ((actual_87 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_87, true, "pt 3 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_82 = actual_87, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_82) => (structuralHash(x_82) | 0),
            }) ? ((arg_87 = toString(true), (arg_1_82 = toString(actual_87), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_87)(arg_1_82)("pt 3 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_87)("pt 3 ok"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("5 points, 4 params", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let a_136, b_135, vx_66, vy_66, copyOfStruct_83, arg_88, arg_1_83, a_138, b_137, vx_67, vy_67, copyOfStruct_84, arg_89, arg_1_84, a_140, b_139, vx_68, vy_68, copyOfStruct_85, arg_90, arg_1_85, a_142, b_141, vx_69, vy_69, copyOfStruct_86, arg_91, arg_1_86, a_144, b_143, vx_70, vy_70, copyOfStruct_87, arg_92, arg_1_87;
        const o_7 = Polyline2D_offsetVar_357F0A77(plClosed, new Float64Array([4, 2, 2, 2]), true, true, 1, 1, 0.9961946980917455, -0.9961946980917455);
        const actual_88 = ((a_136 = item(0, Polyline2D__get_Points(o_7)), (b_135 = Pt_$ctor_7B00E9A0(2, 4), (vx_66 = (a_136.X - b_135.X), (vy_66 = (a_136.Y - b_135.Y), Math.sqrt((vx_66 * vx_66) + (vy_66 * vy_66))))))) < 1E-09;
        if ((actual_88 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_88, true, "pt 0 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_83 = actual_88, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_83) => (structuralHash(x_83) | 0),
            }) ? ((arg_88 = toString(true), (arg_1_83 = toString(actual_88), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_88)(arg_1_83)("pt 0 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_88)("pt 0 ok"));
        }
        const actual_89 = ((a_138 = item(1, Polyline2D__get_Points(o_7)), (b_137 = Pt_$ctor_7B00E9A0(8, 4), (vx_67 = (a_138.X - b_137.X), (vy_67 = (a_138.Y - b_137.Y), Math.sqrt((vx_67 * vx_67) + (vy_67 * vy_67))))))) < 1E-09;
        if ((actual_89 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_89, true, "pt 1 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_84 = actual_89, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_84) => (structuralHash(x_84) | 0),
            }) ? ((arg_89 = toString(true), (arg_1_84 = toString(actual_89), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_89)(arg_1_84)("pt 1 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_89)("pt 1 ok"));
        }
        const actual_90 = ((a_140 = item(2, Polyline2D__get_Points(o_7)), (b_139 = Pt_$ctor_7B00E9A0(8, 8), (vx_68 = (a_140.X - b_139.X), (vy_68 = (a_140.Y - b_139.Y), Math.sqrt((vx_68 * vx_68) + (vy_68 * vy_68))))))) < 1E-09;
        if ((actual_90 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_90, true, "pt 2 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_85 = actual_90, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_85) => (structuralHash(x_85) | 0),
            }) ? ((arg_90 = toString(true), (arg_1_85 = toString(actual_90), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_90)(arg_1_85)("pt 2 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_90)("pt 2 ok"));
        }
        const actual_91 = ((a_142 = item(3, Polyline2D__get_Points(o_7)), (b_141 = Pt_$ctor_7B00E9A0(2, 8), (vx_69 = (a_142.X - b_141.X), (vy_69 = (a_142.Y - b_141.Y), Math.sqrt((vx_69 * vx_69) + (vy_69 * vy_69))))))) < 1E-09;
        if ((actual_91 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_91, true, "pt 3 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_86 = actual_91, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_86) => (structuralHash(x_86) | 0),
            }) ? ((arg_91 = toString(true), (arg_1_86 = toString(actual_91), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_91)(arg_1_86)("pt 3 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_91)("pt 3 ok"));
        }
        const actual_92 = ((a_144 = item(4, Polyline2D__get_Points(o_7)), (b_143 = Pt_$ctor_7B00E9A0(2, 4), (vx_70 = (a_144.X - b_143.X), (vy_70 = (a_144.Y - b_143.Y), Math.sqrt((vx_70 * vx_70) + (vy_70 * vy_70))))))) < 1E-09;
        if ((actual_92 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_92, true, "pt 4 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_87 = actual_92, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_87) => (structuralHash(x_87) | 0),
            }) ? ((arg_92 = toString(true), (arg_1_87 = toString(actual_92), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_92)(arg_1_87)("pt 4 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_92)("pt 4 ok"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("5 points outwards", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let a_146, b_145, vx_71, vy_71, copyOfStruct_88, arg_93, arg_1_88, a_148, b_147, vx_72, vy_72, copyOfStruct_89, arg_94, arg_1_89, a_150, b_149, vx_73, vy_73, copyOfStruct_90, arg_95, arg_1_90, a_152, b_151, vx_74, vy_74, copyOfStruct_91, arg_96, arg_1_91, a_154, b_153, vx_75, vy_75, copyOfStruct_92, arg_97, arg_1_92;
        const o_8 = Polyline2D_offset_Z45C468A5(plClosed, -2, false, true, 1, -0.9961946980917455);
        const actual_93 = ((a_146 = item(0, Polyline2D__get_Points(o_8)), (b_145 = Pt_$ctor_7B00E9A0(-2, -2), (vx_71 = (a_146.X - b_145.X), (vy_71 = (a_146.Y - b_145.Y), Math.sqrt((vx_71 * vx_71) + (vy_71 * vy_71))))))) < 1E-09;
        if ((actual_93 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_93, true, "pt 0 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_88 = actual_93, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_88) => (structuralHash(x_88) | 0),
            }) ? ((arg_93 = toString(true), (arg_1_88 = toString(actual_93), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_93)(arg_1_88)("pt 0 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_93)("pt 0 ok"));
        }
        const actual_94 = ((a_148 = item(1, Polyline2D__get_Points(o_8)), (b_147 = Pt_$ctor_7B00E9A0(12, -2), (vx_72 = (a_148.X - b_147.X), (vy_72 = (a_148.Y - b_147.Y), Math.sqrt((vx_72 * vx_72) + (vy_72 * vy_72))))))) < 1E-09;
        if ((actual_94 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_94, true, "pt 1 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_89 = actual_94, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_89) => (structuralHash(x_89) | 0),
            }) ? ((arg_94 = toString(true), (arg_1_89 = toString(actual_94), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_94)(arg_1_89)("pt 1 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_94)("pt 1 ok"));
        }
        const actual_95 = ((a_150 = item(2, Polyline2D__get_Points(o_8)), (b_149 = Pt_$ctor_7B00E9A0(12, 12), (vx_73 = (a_150.X - b_149.X), (vy_73 = (a_150.Y - b_149.Y), Math.sqrt((vx_73 * vx_73) + (vy_73 * vy_73))))))) < 1E-09;
        if ((actual_95 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_95, true, "pt 2 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_90 = actual_95, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_90) => (structuralHash(x_90) | 0),
            }) ? ((arg_95 = toString(true), (arg_1_90 = toString(actual_95), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_95)(arg_1_90)("pt 2 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_95)("pt 2 ok"));
        }
        const actual_96 = ((a_152 = item(3, Polyline2D__get_Points(o_8)), (b_151 = Pt_$ctor_7B00E9A0(-2, 12), (vx_74 = (a_152.X - b_151.X), (vy_74 = (a_152.Y - b_151.Y), Math.sqrt((vx_74 * vx_74) + (vy_74 * vy_74))))))) < 1E-09;
        if ((actual_96 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_96, true, "pt 3 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_91 = actual_96, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_91) => (structuralHash(x_91) | 0),
            }) ? ((arg_96 = toString(true), (arg_1_91 = toString(actual_96), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_96)(arg_1_91)("pt 3 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_96)("pt 3 ok"));
        }
        const actual_97 = ((a_154 = item(4, Polyline2D__get_Points(o_8)), (b_153 = Pt_$ctor_7B00E9A0(-2, -2), (vx_75 = (a_154.X - b_153.X), (vy_75 = (a_154.Y - b_153.Y), Math.sqrt((vx_75 * vx_75) + (vy_75 * vy_75))))))) < 1E-09;
        if ((actual_97 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_97, true, "pt 4 ok");
        }
        else {
            throw new Exception(contains((copyOfStruct_92 = actual_97, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_92) => (structuralHash(x_92) | 0),
            }) ? ((arg_97 = toString(true), (arg_1_92 = toString(actual_97), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_97)(arg_1_92)("pt 4 ok")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_97)("pt 4 ok"));
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
        Expect_isTrue(Polyline2D__get_PointCount(Polyline2D_offset_Z45C468A5(plLShape, 0.5, true, true, 1, -0.9961946980917455)) === 6)("L-shape offset should have 6 points");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("zero offset", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let a_156, b_155, vx_76, vy_76, copyOfStruct_93, arg_98, arg_1_93;
        const o_11 = Polyline2D_offset_Z45C468A5(plClosed, 0, false, true, 1, -0.9961946980917455);
        Expect_isTrue(Polyline2D__get_PointCount(o_11) === Polyline2D__get_PointCount(plClosed))("zero offset preserves shape");
        const actual_98 = ((a_156 = item(0, Polyline2D__get_Points(o_11)), (b_155 = item(0, Polyline2D__get_Points(plClosed)), (vx_76 = (a_156.X - b_155.X), (vy_76 = (a_156.Y - b_155.Y), Math.sqrt((vx_76 * vx_76) + (vy_76 * vy_76))))))) < 1E-09;
        if ((actual_98 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_98, true, "zero offset first point");
        }
        else {
            throw new Exception(contains((copyOfStruct_93 = actual_98, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_93) => (structuralHash(x_93) | 0),
            }) ? ((arg_98 = toString(true), (arg_1_93 = toString(actual_98), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_98)(arg_1_93)("zero offset first point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_98)("zero offset first point"));
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
        let copyOfStruct_94, arg_99, arg_1_94, a_158, b_157, vx_77, vy_77, copyOfStruct_95, arg_100, arg_1_95, a_160, b_159, vx_78, vy_78, copyOfStruct_96, arg_101, arg_1_96, a_162, b_161, vx_79, vy_79, copyOfStruct_97, arg_102, arg_1_97, a_164, b_163, vx_80, vy_80, copyOfStruct_98, arg_103, arg_1_98, a_166, b_165, vx_81, vy_81, copyOfStruct_99, arg_104, arg_1_99;
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
            throw new Exception(contains((copyOfStruct_94 = actual_100, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_94) => (structuralHash(x_94) | 0),
            }) ? ((arg_99 = int32ToString(expected_183), (arg_1_94 = int32ToString(actual_100), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_99)(arg_1_94)("colinear offset point count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_183)(actual_100)("colinear offset point count"));
        }
        const actual_101 = ((a_158 = item(0, Polyline2D__get_Points(o_14)), (b_157 = Pt_$ctor_7B00E9A0(0, 2), (vx_77 = (a_158.X - b_157.X), (vy_77 = (a_158.Y - b_157.Y), Math.sqrt((vx_77 * vx_77) + (vy_77 * vy_77))))))) < 1E-09;
        if ((actual_101 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_101, true, "colinear offset pt 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_95 = actual_101, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_95) => (structuralHash(x_95) | 0),
            }) ? ((arg_100 = toString(true), (arg_1_95 = toString(actual_101), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_100)(arg_1_95)("colinear offset pt 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_101)("colinear offset pt 0"));
        }
        const actual_102 = ((a_160 = item(1, Polyline2D__get_Points(o_14)), (b_159 = Pt_$ctor_7B00E9A0(5, 2), (vx_78 = (a_160.X - b_159.X), (vy_78 = (a_160.Y - b_159.Y), Math.sqrt((vx_78 * vx_78) + (vy_78 * vy_78))))))) < 1E-09;
        if ((actual_102 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_102, true, "colinear offset pt 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_96 = actual_102, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_96) => (structuralHash(x_96) | 0),
            }) ? ((arg_101 = toString(true), (arg_1_96 = toString(actual_102), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_101)(arg_1_96)("colinear offset pt 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_102)("colinear offset pt 1"));
        }
        const actual_103 = ((a_162 = item(2, Polyline2D__get_Points(o_14)), (b_161 = Pt_$ctor_7B00E9A0(10, 2), (vx_79 = (a_162.X - b_161.X), (vy_79 = (a_162.Y - b_161.Y), Math.sqrt((vx_79 * vx_79) + (vy_79 * vy_79))))))) < 1E-09;
        if ((actual_103 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_103, true, "colinear offset pt 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_97 = actual_103, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_97) => (structuralHash(x_97) | 0),
            }) ? ((arg_102 = toString(true), (arg_1_97 = toString(actual_103), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_102)(arg_1_97)("colinear offset pt 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_103)("colinear offset pt 2"));
        }
        const actual_104 = ((a_164 = item(3, Polyline2D__get_Points(o_14)), (b_163 = Pt_$ctor_7B00E9A0(15, 2), (vx_80 = (a_164.X - b_163.X), (vy_80 = (a_164.Y - b_163.Y), Math.sqrt((vx_80 * vx_80) + (vy_80 * vy_80))))))) < 1E-09;
        if ((actual_104 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_104, true, "colinear offset pt 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_98 = actual_104, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_98) => (structuralHash(x_98) | 0),
            }) ? ((arg_103 = toString(true), (arg_1_98 = toString(actual_104), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_103)(arg_1_98)("colinear offset pt 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_104)("colinear offset pt 3"));
        }
        const actual_105 = ((a_166 = item(4, Polyline2D__get_Points(o_14)), (b_165 = Pt_$ctor_7B00E9A0(20, 2), (vx_81 = (a_166.X - b_165.X), (vy_81 = (a_166.Y - b_165.Y), Math.sqrt((vx_81 * vx_81) + (vy_81 * vy_81))))))) < 1E-09;
        if ((actual_105 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_105, true, "colinear offset pt 4");
        }
        else {
            throw new Exception(contains((copyOfStruct_99 = actual_105, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_99) => (structuralHash(x_99) | 0),
            }) ? ((arg_104 = toString(true), (arg_1_99 = toString(actual_105), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_104)(arg_1_99)("colinear offset pt 4")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_105)("colinear offset pt 4"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("L-shape with colinear extension", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        let copyOfStruct_100, arg_105, arg_1_100;
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
            throw new Exception(contains((copyOfStruct_100 = actual_107, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_100) => (structuralHash(x_100) | 0),
            }) ? ((arg_105 = int32ToString(expected_195), (arg_1_100 = int32ToString(actual_107), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_105)(arg_1_100)("L-extended offset point count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_195)(actual_107)("L-extended offset point count"));
        }
        Expect_isTrue(Polyline2D__get_Points(o_15).length === 5)("L-extended offset maintains structure");
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rect with colinear segments offset", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let copyOfStruct_101, arg_106, arg_1_101, a_168, b_167, vx_82, vy_82, copyOfStruct_102, arg_107, arg_1_102, a_170, b_169, vx_83, vy_83, copyOfStruct_103, arg_108, arg_1_103, a_172, b_171, vx_84, vy_84, copyOfStruct_104, arg_109, arg_1_104, a_174, b_173, vx_85, vy_85, copyOfStruct_105, arg_110, arg_1_105, a_176, b_175, vx_86, vy_86, copyOfStruct_106, arg_111, arg_1_106, a_178, b_177, vx_87, vy_87, copyOfStruct_107, arg_112, arg_1_107;
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
            throw new Exception(contains((copyOfStruct_101 = actual_109, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_101) => (structuralHash(x_101) | 0),
            }) ? ((arg_106 = int32ToString(expected_197), (arg_1_101 = int32ToString(actual_109), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_106)(arg_1_101)("point count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_197)(actual_109)("point count"));
        }
        const actual_110 = ((a_168 = item(0, Polyline2D__get_Points(o_16)), (b_167 = Pt_$ctor_7B00E9A0(2, 2), (vx_82 = (a_168.X - b_167.X), (vy_82 = (a_168.Y - b_167.Y), Math.sqrt((vx_82 * vx_82) + (vy_82 * vy_82))))))) < 1E-09;
        if ((actual_110 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_110, true, "colinear different offsets pt 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_102 = actual_110, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_102) => (structuralHash(x_102) | 0),
            }) ? ((arg_107 = toString(true), (arg_1_102 = toString(actual_110), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_107)(arg_1_102)("colinear different offsets pt 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_110)("colinear different offsets pt 0"));
        }
        const actual_111 = ((a_170 = item(1, Polyline2D__get_Points(o_16)), (b_169 = Pt_$ctor_7B00E9A0(5, 2), (vx_83 = (a_170.X - b_169.X), (vy_83 = (a_170.Y - b_169.Y), Math.sqrt((vx_83 * vx_83) + (vy_83 * vy_83))))))) < 1E-09;
        if ((actual_111 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_111, true, "colinear different offsets pt 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_103 = actual_111, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_103) => (structuralHash(x_103) | 0),
            }) ? ((arg_108 = toString(true), (arg_1_103 = toString(actual_111), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_108)(arg_1_103)("colinear different offsets pt 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_111)("colinear different offsets pt 1"));
        }
        const actual_112 = ((a_172 = item(2, Polyline2D__get_Points(o_16)), (b_171 = Pt_$ctor_7B00E9A0(8, 2), (vx_84 = (a_172.X - b_171.X), (vy_84 = (a_172.Y - b_171.Y), Math.sqrt((vx_84 * vx_84) + (vy_84 * vy_84))))))) < 1E-09;
        if ((actual_112 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_112, true, "colinear different offsets pt 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_104 = actual_112, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_104) => (structuralHash(x_104) | 0),
            }) ? ((arg_109 = toString(true), (arg_1_104 = toString(actual_112), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_109)(arg_1_104)("colinear different offsets pt 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_112)("colinear different offsets pt 2"));
        }
        const actual_113 = ((a_174 = item(3, Polyline2D__get_Points(o_16)), (b_173 = Pt_$ctor_7B00E9A0(8, 8), (vx_85 = (a_174.X - b_173.X), (vy_85 = (a_174.Y - b_173.Y), Math.sqrt((vx_85 * vx_85) + (vy_85 * vy_85))))))) < 1E-09;
        if ((actual_113 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_113, true, "colinear different offsets pt 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_105 = actual_113, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_105) => (structuralHash(x_105) | 0),
            }) ? ((arg_110 = toString(true), (arg_1_105 = toString(actual_113), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_110)(arg_1_105)("colinear different offsets pt 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_113)("colinear different offsets pt 3"));
        }
        const actual_114 = ((a_176 = item(4, Polyline2D__get_Points(o_16)), (b_175 = Pt_$ctor_7B00E9A0(2, 8), (vx_86 = (a_176.X - b_175.X), (vy_86 = (a_176.Y - b_175.Y), Math.sqrt((vx_86 * vx_86) + (vy_86 * vy_86))))))) < 1E-09;
        if ((actual_114 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_114, true, "colinear different offsets pt 4");
        }
        else {
            throw new Exception(contains((copyOfStruct_106 = actual_114, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_106) => (structuralHash(x_106) | 0),
            }) ? ((arg_111 = toString(true), (arg_1_106 = toString(actual_114), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_111)(arg_1_106)("colinear different offsets pt 4")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_114)("colinear different offsets pt 4"));
        }
        const actual_115 = ((a_178 = item(5, Polyline2D__get_Points(o_16)), (b_177 = Pt_$ctor_7B00E9A0(2, 2), (vx_87 = (a_178.X - b_177.X), (vy_87 = (a_178.Y - b_177.Y), Math.sqrt((vx_87 * vx_87) + (vy_87 * vy_87))))))) < 1E-09;
        if ((actual_115 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_115, true, "colinear different offsets pt 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_107 = actual_115, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_107) => (structuralHash(x_107) | 0),
            }) ? ((arg_112 = toString(true), (arg_1_107 = toString(actual_115), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_112)(arg_1_107)("colinear different offsets pt 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_115)("colinear different offsets pt 5"));
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
        let copyOfStruct, arg, arg_1, a_2, b_2, vx, vy, copyOfStruct_1, arg_6, arg_1_1, a_4, b_4, vx_1, vy_1, copyOfStruct_2, arg_7, arg_1_2;
        const actual = Polyline2D__get_PointCount(plWithDuplicates) | 0;
        if ((actual === 8) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 8, "should preserve all points including duplicates");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            }) ? ((arg = int32ToString(8), (arg_1 = int32ToString(actual), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("should preserve all points including duplicates")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(8)(actual)("should preserve all points including duplicates"));
        }
        Expect_isTrue(Polyline2D__get_IsClosed(plWithDuplicates))("should be closed when start/end are same");
        const actual_1 = ((a_2 = Polyline2D__get_Start(plWithDuplicates), (b_2 = Pt_$ctor_7B00E9A0(0, 0), (vx = (a_2.X - b_2.X), (vy = (a_2.Y - b_2.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09;
        if ((actual_1 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, true, "start point");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_1, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            }) ? ((arg_6 = toString(true), (arg_1_1 = toString(actual_1), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("start point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_1)("start point"));
        }
        const actual_2 = ((a_4 = Polyline2D__get_End(plWithDuplicates), (b_4 = Pt_$ctor_7B00E9A0(0, 0), (vx_1 = (a_4.X - b_4.X), (vy_1 = (a_4.Y - b_4.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09;
        if ((actual_2 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, true, "end point");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_2, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            }) ? ((arg_7 = toString(true), (arg_1_2 = toString(actual_2), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("end point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_2)("end point"));
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("consecutive duplicate points - segment count", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let copyOfStruct_3, arg_8, arg_1_3, a_6, ln, b_6, vx_2, vy_2, copyOfStruct_4, arg_9, arg_1_4, a_8, ln_1, b_8, vx_3, vy_3, copyOfStruct_5, arg_10, arg_1_5, a_10, ln_2, b_10, vx_4, vy_4, copyOfStruct_6, arg_11, arg_1_6;
        const actual_3 = Polyline2D__get_SegmentCount(plConsecutiveDuplicates) | 0;
        if ((actual_3 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 4, "should count all segments including zero-length");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_3, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            }) ? ((arg_8 = int32ToString(4), (arg_1_3 = int32ToString(actual_3), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("should count all segments including zero-length")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_3)("should count all segments including zero-length"));
        }
        const seg1 = Polyline2D__GetSegment_Z524259A4(plConsecutiveDuplicates, 1);
        const seg2 = Polyline2D__GetSegment_Z524259A4(plConsecutiveDuplicates, 2);
        const actual_4 = ((a_6 = ((ln = seg1, Pt_$ctor_7B00E9A0_1(ln.FromX, ln.FromY))), (b_6 = Pt_$ctor_7B00E9A0(5, 5), (vx_2 = (a_6.X - b_6.X), (vy_2 = (a_6.Y - b_6.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09;
        if ((actual_4 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, true, "zero length segment start");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_4, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            }) ? ((arg_9 = toString(true), (arg_1_4 = toString(actual_4), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("zero length segment start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_4)("zero length segment start"));
        }
        const actual_5 = ((a_8 = ((ln_1 = seg1, Pt_$ctor_7B00E9A0_1(ln_1.ToX, ln_1.ToY))), (b_8 = Pt_$ctor_7B00E9A0(5, 5), (vx_3 = (a_8.X - b_8.X), (vy_3 = (a_8.Y - b_8.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09;
        if ((actual_5 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, true, "zero length segment end");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_5, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            }) ? ((arg_10 = toString(true), (arg_1_5 = toString(actual_5), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("zero length segment end")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_5)("zero length segment end"));
        }
        const actual_6 = ((a_10 = ((ln_2 = seg2, Pt_$ctor_7B00E9A0_1(ln_2.FromX, ln_2.FromY))), (b_10 = Pt_$ctor_7B00E9A0(5, 5), (vx_4 = (a_10.X - b_10.X), (vy_4 = (a_10.Y - b_10.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) < 1E-09;
        if ((actual_6 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, true, "consecutive zero length segment");
        }
        else {
            throw new Exception(contains((copyOfStruct_6 = actual_6, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            }) ? ((arg_11 = toString(true), (arg_1_6 = toString(actual_6), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("consecutive zero length segment")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_6)("consecutive zero length segment"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("all duplicate points polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let copyOfStruct_7, arg_12, arg_1_7, copyOfStruct_8, arg_13, arg_1_8, a_12, b_12, vx_5, vy_5, copyOfStruct_9, arg_14, arg_1_9;
        const actual_7 = Polyline2D__get_PointCount(plAllDuplicates) | 0;
        if ((actual_7 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 4, "should preserve all duplicate points");
        }
        else {
            throw new Exception(contains((copyOfStruct_7 = actual_7, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            }) ? ((arg_12 = int32ToString(4), (arg_1_7 = int32ToString(actual_7), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("should preserve all duplicate points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_7)("should preserve all duplicate points"));
        }
        const actual_8 = Polyline2D__get_SegmentCount(plAllDuplicates) | 0;
        if ((actual_8 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 3, "should have segments even if zero length");
        }
        else {
            throw new Exception(contains((copyOfStruct_8 = actual_8, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            }) ? ((arg_13 = int32ToString(3), (arg_1_8 = int32ToString(actual_8), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("should have segments even if zero length")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_8)("should have segments even if zero length"));
        }
        Expect_isTrue(Math.abs(Polyline2D__get_Length(plAllDuplicates)) < 1E-09)("length should be zero");
        const actual_9 = ((a_12 = item(2, Polyline2D__get_Points(plAllDuplicates)), (b_12 = Polyline2D__get_Start(plAllDuplicates), (vx_5 = (a_12.X - b_12.X), (vy_5 = (a_12.Y - b_12.Y), Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5))))))) < 1E-09;
        if ((actual_9 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, true, "all points same as start");
        }
        else {
            throw new Exception(contains((copyOfStruct_9 = actual_9, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            }) ? ((arg_14 = toString(true), (arg_1_9 = toString(actual_9), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("all points same as start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_9)("all points same as start"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points at start and end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let copyOfStruct_10, arg_15, arg_1_10, a_14, b_14, vx_6, vy_6, copyOfStruct_11, arg_16, arg_1_11;
        Expect_isTrue(Polyline2D__get_IsClosed(plStartEndDuplicate))("should be closed with duplicate start/end");
        const actual_10 = Polyline2D__get_PointCount(plStartEndDuplicate) | 0;
        if ((actual_10 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 6, "should count all points including duplicates");
        }
        else {
            throw new Exception(contains((copyOfStruct_10 = actual_10, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_10) => (structuralHash(x_10) | 0),
            }) ? ((arg_15 = int32ToString(6), (arg_1_10 = int32ToString(actual_10), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("should count all points including duplicates")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_10)("should count all points including duplicates"));
        }
        const actual_11 = ((a_14 = item(0, Polyline2D__get_Points(plStartEndDuplicate)), (b_14 = item(5, Polyline2D__get_Points(plStartEndDuplicate)), (vx_6 = (a_14.X - b_14.X), (vy_6 = (a_14.Y - b_14.Y), Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6))))))) < 1E-09;
        if ((actual_11 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, true, "first and last points equal");
        }
        else {
            throw new Exception(contains((copyOfStruct_11 = actual_11, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            }) ? ((arg_16 = toString(true), (arg_1_11 = toString(actual_11), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("first and last points equal")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_11)("first and last points equal"));
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
        let a_16, b_16, vx_7, vy_7, copyOfStruct_12, arg_17, arg_1_12, a_18, b_18, vx_8, vy_8, copyOfStruct_13, arg_18, arg_1_13, a_20, b_20, vx_9, vy_9, copyOfStruct_14, arg_19, arg_1_14;
        const pt0 = Polyline2D__EvaluateAt_5E38073B(plConsecutiveDuplicates, 0);
        const pt1 = Polyline2D__EvaluateAt_5E38073B(plConsecutiveDuplicates, 1);
        const ptMid = Polyline2D__EvaluateAt_5E38073B(plConsecutiveDuplicates, 1.5);
        const actual_12 = ((a_16 = pt0, (b_16 = Pt_$ctor_7B00E9A0(0, 0), (vx_7 = (a_16.X - b_16.X), (vy_7 = (a_16.Y - b_16.Y), Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7))))))) < 1E-09;
        if ((actual_12 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, true, "evaluate at start with duplicates");
        }
        else {
            throw new Exception(contains((copyOfStruct_12 = actual_12, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            }) ? ((arg_17 = toString(true), (arg_1_12 = toString(actual_12), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("evaluate at start with duplicates")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_12)("evaluate at start with duplicates"));
        }
        const actual_13 = ((a_18 = pt1, (b_18 = Pt_$ctor_7B00E9A0(5, 5), (vx_8 = (a_18.X - b_18.X), (vy_8 = (a_18.Y - b_18.Y), Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8))))))) < 1E-09;
        if ((actual_13 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, true, "evaluate at segment 1 with duplicates");
        }
        else {
            throw new Exception(contains((copyOfStruct_13 = actual_13, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            }) ? ((arg_18 = toString(true), (arg_1_13 = toString(actual_13), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("evaluate at segment 1 with duplicates")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_13)("evaluate at segment 1 with duplicates"));
        }
        const actual_14 = ((a_20 = ptMid, (b_20 = Pt_$ctor_7B00E9A0(5, 5), (vx_9 = (a_20.X - b_20.X), (vy_9 = (a_20.Y - b_20.Y), Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9))))))) < 1E-09;
        if ((actual_14 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, true, "evaluate in duplicate segment range");
        }
        else {
            throw new Exception(contains((copyOfStruct_14 = actual_14, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            }) ? ((arg_19 = toString(true), (arg_1_14 = toString(actual_14), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("evaluate in duplicate segment range")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_14)("evaluate in duplicate segment range"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points - winding number", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        if (Polyline2D__get_IsClosed(plWithDuplicates)) {
            const insidePoint = Pt_$ctor_7B00E9A0(5, 5);
            Expect_isTrue(Polyline2D__WindingNumber_6ADE94FD(plWithDuplicates, insidePoint) !== 0)("winding number with duplicates should work");
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
        let copyOfStruct_15, arg_20, arg_1_15, a_22, b_22, vx_10, vy_10, copyOfStruct_16, arg_21, arg_1_16, a_24, b_24, vx_11, vy_11, copyOfStruct_17, arg_22, arg_1_17, a_26, b_26, vx_12, vy_12, copyOfStruct_18, arg_23, arg_1_18;
        const scaled = Polyline2D__Scale_5E38073B(plWithDuplicates, 2);
        const actual_16 = Polyline2D__get_PointCount(scaled) | 0;
        const expected_26 = Polyline2D__get_PointCount(plWithDuplicates) | 0;
        if ((actual_16 === expected_26) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, expected_26, "scaled polyline preserves duplicate count");
        }
        else {
            throw new Exception(contains((copyOfStruct_15 = actual_16, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            }) ? ((arg_20 = int32ToString(expected_26), (arg_1_15 = int32ToString(actual_16), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("scaled polyline preserves duplicate count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_26)(actual_16)("scaled polyline preserves duplicate count"));
        }
        const actual_17 = ((a_22 = item(0, Polyline2D__get_Points(scaled)), (b_22 = item(1, Polyline2D__get_Points(scaled)), (vx_10 = (a_22.X - b_22.X), (vy_10 = (a_22.Y - b_22.Y), Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10))))))) < 1E-09;
        if ((actual_17 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, true, "scaled duplicate points remain duplicates");
        }
        else {
            throw new Exception(contains((copyOfStruct_16 = actual_17, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            }) ? ((arg_21 = toString(true), (arg_1_16 = toString(actual_17), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("scaled duplicate points remain duplicates")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_17)("scaled duplicate points remain duplicates"));
        }
        const translated = Polyline2D_translate(Vc_$ctor_7B00E9A0(1, 1), plAllDuplicates);
        const actual_18 = ((a_24 = item(0, Polyline2D__get_Points(translated)), (b_24 = item(3, Polyline2D__get_Points(translated)), (vx_11 = (a_24.X - b_24.X), (vy_11 = (a_24.Y - b_24.Y), Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11))))))) < 1E-09;
        if ((actual_18 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, true, "translated all duplicates remain same");
        }
        else {
            throw new Exception(contains((copyOfStruct_17 = actual_18, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            }) ? ((arg_22 = toString(true), (arg_1_17 = toString(actual_18), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("translated all duplicates remain same")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_18)("translated all duplicates remain same"));
        }
        const actual_19 = ((a_26 = item(0, Polyline2D__get_Points(translated)), (b_26 = Pt_$ctor_7B00E9A0(2, 2), (vx_12 = (a_26.X - b_26.X), (vy_12 = (a_26.Y - b_26.Y), Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12))))))) < 1E-09;
        if ((actual_19 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, true, "translated duplicate points moved correctly");
        }
        else {
            throw new Exception(contains((copyOfStruct_18 = actual_19, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_18) => (structuralHash(x_18) | 0),
            }) ? ((arg_23 = toString(true), (arg_1_18 = toString(actual_19), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("translated duplicate points moved correctly")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_19)("translated duplicate points moved correctly"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points - sub polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        Expect_isTrue(Polyline2D__get_PointCount(Polyline2D_subPolyline(0.5, 2.5, plConsecutiveDuplicates)) > 0)("sub polyline with duplicates should work");
        Expect_isTrue(Polyline2D__get_PointCount(Polyline2D_subPolyline(1, 3, plConsecutiveDuplicates)) > 0)("sub polyline across duplicates");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points - edge cases", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let copyOfStruct_19, arg_24, arg_1_19, copyOfStruct_20, arg_25, arg_1_20, copyOfStruct_21, arg_26, arg_1_21, a_30, b_30, vx_13, vy_13, copyOfStruct_22, arg_27, arg_1_22;
        let plTwoDuplicates;
        const points = [Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 1)];
        plTwoDuplicates = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
        const actual_20 = Polyline2D__get_PointCount(plTwoDuplicates) | 0;
        if ((actual_20 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, 2, "two duplicate points");
        }
        else {
            throw new Exception(contains((copyOfStruct_19 = actual_20, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            }) ? ((arg_24 = int32ToString(2), (arg_1_19 = int32ToString(actual_20), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("two duplicate points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_20)("two duplicate points"));
        }
        const actual_21 = Polyline2D__get_SegmentCount(plTwoDuplicates) | 0;
        if ((actual_21 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 1, "one zero-length segment");
        }
        else {
            throw new Exception(contains((copyOfStruct_20 = actual_21, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            }) ? ((arg_25 = int32ToString(1), (arg_1_20 = int32ToString(actual_21), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("one zero-length segment")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_21)("one zero-length segment"));
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
            throw new Exception(contains((copyOfStruct_21 = actual_22, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            }) ? ((arg_26 = int32ToString(10), (arg_1_21 = int32ToString(actual_22), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("many duplicate points preserved")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_22)("many duplicate points preserved"));
        }
        const actual_23 = ((a_30 = item(0, Polyline2D__get_Points(plManyDuplicates)), (b_30 = item(9, Polyline2D__get_Points(plManyDuplicates)), (vx_13 = (a_30.X - b_30.X), (vy_13 = (a_30.Y - b_30.Y), Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13))))))) < 1E-09;
        if ((actual_23 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, true, "all points are same");
        }
        else {
            throw new Exception(contains((copyOfStruct_22 = actual_23, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            }) ? ((arg_27 = toString(true), (arg_1_22 = toString(actual_23), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("all points are same")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_23)("all points are same"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("nearly duplicate points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let copyOfStruct_23, arg_28, arg_1_23;
        let plNearlyDup;
        const points_2 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 1E-12), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 1E-12), Pt_$ctor_7B00E9A0(0, 0)];
        plNearlyDup = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_2));
        const actual_24 = Polyline2D__get_PointCount(plNearlyDup) | 0;
        if ((actual_24 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, 5, "nearly duplicate points preserved");
        }
        else {
            throw new Exception(contains((copyOfStruct_23 = actual_24, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_23) => (structuralHash(x_23) | 0),
            }) ? ((arg_28 = int32ToString(5), (arg_1_23 = int32ToString(actual_24), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("nearly duplicate points preserved")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_24)("nearly duplicate points preserved"));
        }
        Expect_isTrue(Polyline2D__get_IsClosed(plNearlyDup))("should be closed");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("duplicate points with different operations", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let copyOfStruct_24, arg_29, arg_1_24, copyOfStruct_25, arg_30, arg_1_25, a_32, b_32, vx_14, vy_14, copyOfStruct_26, arg_31, arg_1_26;
        const actual_26 = Polyline2D__get_PointCount(Polyline2D__Reverse(plWithDuplicates)) | 0;
        const expected_40 = Polyline2D__get_PointCount(plWithDuplicates) | 0;
        if ((actual_26 === expected_40) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, expected_40, "reversed preserves duplicates");
        }
        else {
            throw new Exception(contains((copyOfStruct_24 = actual_26, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            }) ? ((arg_29 = int32ToString(expected_40), (arg_1_24 = int32ToString(actual_26), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("reversed preserves duplicates")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_40)(actual_26)("reversed preserves duplicates"));
        }
        const cloned = Polyline2D__Clone(plWithDuplicates);
        const actual_28 = Polyline2D__get_PointCount(cloned) | 0;
        const expected_42 = Polyline2D__get_PointCount(plWithDuplicates) | 0;
        if ((actual_28 === expected_42) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, expected_42, "cloned preserves duplicates");
        }
        else {
            throw new Exception(contains((copyOfStruct_25 = actual_28, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            }) ? ((arg_30 = int32ToString(expected_42), (arg_1_25 = int32ToString(actual_28), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("cloned preserves duplicates")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_42)(actual_28)("cloned preserves duplicates"));
        }
        const actual_29 = ((a_32 = item(0, Polyline2D__get_Points(cloned)), (b_32 = item(1, Polyline2D__get_Points(cloned)), (vx_14 = (a_32.X - b_32.X), (vy_14 = (a_32.Y - b_32.Y), Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14))))))) < 1E-09;
        if ((actual_29 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, true, "cloned duplicate points match");
        }
        else {
            throw new Exception(contains((copyOfStruct_26 = actual_29, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            }) ? ((arg_31 = toString(true), (arg_1_26 = toString(actual_29), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("cloned duplicate points match")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_29)("cloned duplicate points match"));
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
        let points_3, copyOfStruct_27, arg_32, arg_1_27;
        const cleaned = Polyline2D_removeDuplicatePoints(1E-09, (points_3 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_3))));
        const actual_30 = Polyline2D__get_PointCount(cleaned) | 0;
        if ((actual_30 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, 3, "duplicates removed keeping endpoints");
        }
        else {
            throw new Exception(contains((copyOfStruct_27 = actual_30, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            }) ? ((arg_32 = int32ToString(3), (arg_1_27 = int32ToString(actual_30), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("duplicates removed keeping endpoints")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_30)("duplicates removed keeping endpoints"));
        }
        Expect_isFalse(Polyline2D__get_IsClosed(cleaned))("open polyline remains open");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("removeDuplicatePoints closed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let points_4, copyOfStruct_28, arg_33, arg_1_28;
        const cleaned_1 = Polyline2D_removeDuplicatePoints(1E-09, (points_4 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(0, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_4))));
        const actual_31 = Polyline2D__get_PointCount(cleaned_1) | 0;
        if ((actual_31 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, 5, "one duplicate removed");
        }
        else {
            throw new Exception(contains((copyOfStruct_28 = actual_31, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_28) => (structuralHash(x_28) | 0),
            }) ? ((arg_33 = int32ToString(5), (arg_1_28 = int32ToString(actual_31), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("one duplicate removed")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_31)("one duplicate removed"));
        }
        Expect_isTrue(Polyline2D__get_IsClosed(cleaned_1))("should stay closed");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("removeColinearAndDuplicatePoints square", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let points_5, copyOfStruct_29, arg_34, arg_1_29;
        const simplified = Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, (points_5 = [Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(4, 0), Pt_$ctor_7B00E9A0(4, 2), Pt_$ctor_7B00E9A0(4, 4), Pt_$ctor_7B00E9A0(2, 4), Pt_$ctor_7B00E9A0(0, 4), Pt_$ctor_7B00E9A0(0, 2), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_5))));
        const actual_32 = Polyline2D__get_PointCount(simplified) | 0;
        const msg_47 = concat("no colinear simplification with loose angle tolerance ", ...Polyline2D__get_AsFSharpCode(simplified));
        if ((actual_32 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, 5, msg_47);
        }
        else {
            throw new Exception(contains((copyOfStruct_29 = actual_32, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_29) => (structuralHash(x_29) | 0),
            }) ? ((arg_34 = int32ToString(5), (arg_1_29 = int32ToString(actual_32), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)(msg_47)))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_32)(msg_47));
        }
        Expect_isTrue(Polyline2D__get_IsClosed(simplified))("remains closed");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("removeColinearAndDuplicatePoints open line with repeats", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let points_6, copyOfStruct_30, arg_35, arg_1_30, a_34, b_34, vx_15, vy_15, copyOfStruct_31, arg_36, arg_1_31, a_36, b_36, vx_16, vy_16, copyOfStruct_32, arg_37, arg_1_32;
        const simplified_1 = Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, (points_6 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0), Pt_$ctor_7B00E9A0(3, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_6))));
        const actual_33 = Polyline2D__get_PointCount(simplified_1) | 0;
        if ((actual_33 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, 2, "duplicate end removed; interior colinear points removed");
        }
        else {
            throw new Exception(contains((copyOfStruct_30 = actual_33, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_30) => (structuralHash(x_30) | 0),
            }) ? ((arg_35 = int32ToString(2), (arg_1_30 = int32ToString(actual_33), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("duplicate end removed; interior colinear points removed")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_33)("duplicate end removed; interior colinear points removed"));
        }
        const actual_34 = ((a_34 = item(0, Polyline2D__get_Points(simplified_1)), (b_34 = Pt_$ctor_7B00E9A0(0, 0), (vx_15 = (a_34.X - b_34.X), (vy_15 = (a_34.Y - b_34.Y), Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15))))))) < 1E-09;
        if ((actual_34 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, true, "start");
        }
        else {
            throw new Exception(contains((copyOfStruct_31 = actual_34, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_31) => (structuralHash(x_31) | 0),
            }) ? ((arg_36 = toString(true), (arg_1_31 = toString(actual_34), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_34)("start"));
        }
        const actual_35 = ((a_36 = item(Polyline2D__get_Points(simplified_1).length - 1, Polyline2D__get_Points(simplified_1)), (b_36 = Pt_$ctor_7B00E9A0(3, 0), (vx_16 = (a_36.X - b_36.X), (vy_16 = (a_36.Y - b_36.Y), Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16))))))) < 1E-09;
        if ((actual_35 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, true, "end");
        }
        else {
            throw new Exception(contains((copyOfStruct_32 = actual_35, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_32) => (structuralHash(x_32) | 0),
            }) ? ((arg_37 = toString(true), (arg_1_32 = toString(actual_35), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_32)("end")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_35)("end"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("colinear start/end segments closed polygon", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let points_7, copyOfStruct_33, arg_38, arg_1_33;
        const simplified_2 = Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, (points_7 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(2, 2), Pt_$ctor_7B00E9A0(2, 4), Pt_$ctor_7B00E9A0(1, 4), Pt_$ctor_7B00E9A0(0, 4), Pt_$ctor_7B00E9A0(0, 2), Pt_$ctor_7B00E9A0(0, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_7))));
        Expect_isTrue(Polyline2D__get_IsClosed(simplified_2))("closed retained");
        const actual_36 = Polyline2D__get_PointCount(simplified_2) | 0;
        if ((actual_36 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, 5, " colinear removal at this tolerance");
        }
        else {
            throw new Exception(contains((copyOfStruct_33 = actual_36, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_33) => (structuralHash(x_33) | 0),
            }) ? ((arg_38 = int32ToString(5), (arg_1_33 = int32ToString(actual_36), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_38)(arg_1_33)(" colinear removal at this tolerance")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_36)(" colinear removal at this tolerance"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("almost colinear tiny angle retained", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let points_8, copyOfStruct_34, arg_39, arg_1_34;
        const actual_37 = Polyline2D__get_PointCount(Polyline2D_removeColinearAndDuplicatePoints(0.9999999847691291, 1E-09, (points_8 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(20, 0.2), Pt_$ctor_7B00E9A0(30, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_8))))) | 0;
        if ((actual_37 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, 4, "almost straight kept");
        }
        else {
            throw new Exception(contains((copyOfStruct_34 = actual_37, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_34) => (structuralHash(x_34) | 0),
            }) ? ((arg_39 = int32ToString(4), (arg_1_34 = int32ToString(actual_37), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_39)(arg_1_34)("almost straight kept")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_37)("almost straight kept"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("open U-turn 180 collapse prevented (since method only removes colinear not u-turn)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let points_9, copyOfStruct_35, arg_40, arg_1_35;
        const actual_38 = Polyline2D__get_PointCount(Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, (points_9 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(-5, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_9))))) | 0;
        if ((actual_38 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, 3, "collapsed to endpoints after duplicate removal");
        }
        else {
            throw new Exception(contains((copyOfStruct_35 = actual_38, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_35) => (structuralHash(x_35) | 0),
            }) ? ((arg_40 = int32ToString(3), (arg_1_35 = int32ToString(actual_38), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_40)(arg_1_35)("collapsed to endpoints after duplicate removal")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_38)("collapsed to endpoints after duplicate removal"));
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
        Expect_isTrue(expectedInside.every((pt) => Polyline2D__Contains_6ADE94FD(failPoly, pt)))("all expected inside points should be inside, winding");
        Expect_isTrue(!expectedOutside.every((pt_1) => Polyline2D__Contains_6ADE94FD(failPoly, pt_1)))("all expected outside points should be outside, winding");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})()]))]));

export const testsComprehensive = Test_testList("Polyline2D Comprehensive", ofArray([Test_testList("SetVertex", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set vertex on open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_2, b_2, vx, vy, copyOfStruct, arg, arg_1, a_4, b_4, vx_1, vy_1, copyOfStruct_1, arg_6, arg_1_1;
        let pl;
        const points = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0)];
        pl = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points));
        Polyline2D__SetVertex(pl, 1, Pt_$ctor_7B00E9A0(1, 5));
        const actual = ((a_2 = item(1, Polyline2D__get_Points(pl)), (b_2 = Pt_$ctor_7B00E9A0(1, 5), (vx = (a_2.X - b_2.X), (vy = (a_2.Y - b_2.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09;
        if ((actual === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, true, "vertex 1 changed");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            }) ? ((arg = toString(true), (arg_1 = toString(actual), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("vertex 1 changed")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual)("vertex 1 changed"));
        }
        const actual_1 = ((a_4 = item(0, Polyline2D__get_Points(pl)), (b_4 = Pt_$ctor_7B00E9A0(0, 0), (vx_1 = (a_4.X - b_4.X), (vy_1 = (a_4.Y - b_4.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09;
        if ((actual_1 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, true, "vertex 0 unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_1, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            }) ? ((arg_6 = toString(true), (arg_1_1 = toString(actual_1), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("vertex 0 unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_1)("vertex 0 unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set first vertex on closed polyline updates last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_6, b_6, vx_2, vy_2, copyOfStruct_2, arg_7, arg_1_2, a_8, b_8, vx_3, vy_3, copyOfStruct_3, arg_8, arg_1_3;
        let pl_1;
        const points_1 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
        pl_1 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_1));
        Polyline2D__SetVertex(pl_1, 0, Pt_$ctor_7B00E9A0(1, 1));
        const actual_2 = ((a_6 = item(0, Polyline2D__get_Points(pl_1)), (b_6 = Pt_$ctor_7B00E9A0(1, 1), (vx_2 = (a_6.X - b_6.X), (vy_2 = (a_6.Y - b_6.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09;
        if ((actual_2 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, true, "first vertex updated");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_2, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            }) ? ((arg_7 = toString(true), (arg_1_2 = toString(actual_2), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("first vertex updated")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_2)("first vertex updated"));
        }
        const actual_3 = ((a_8 = item(4, Polyline2D__get_Points(pl_1)), (b_8 = Pt_$ctor_7B00E9A0(1, 1), (vx_3 = (a_8.X - b_8.X), (vy_3 = (a_8.Y - b_8.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09;
        if ((actual_3 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, true, "last vertex updated too");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_3, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            }) ? ((arg_8 = toString(true), (arg_1_3 = toString(actual_3), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("last vertex updated too")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_3)("last vertex updated too"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set last vertex on closed polyline updates first", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_10, b_10, vx_4, vy_4, copyOfStruct_4, arg_9, arg_1_4, a_12, b_12, vx_5, vy_5, copyOfStruct_5, arg_10, arg_1_5;
        let pl_2;
        const points_2 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)];
        pl_2 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_2));
        Polyline2D__SetVertex(pl_2, 4, Pt_$ctor_7B00E9A0(2, 2));
        const actual_4 = ((a_10 = item(4, Polyline2D__get_Points(pl_2)), (b_10 = Pt_$ctor_7B00E9A0(2, 2), (vx_4 = (a_10.X - b_10.X), (vy_4 = (a_10.Y - b_10.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) < 1E-09;
        if ((actual_4 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, true, "last vertex updated");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_4, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            }) ? ((arg_9 = toString(true), (arg_1_4 = toString(actual_4), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("last vertex updated")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_4)("last vertex updated"));
        }
        const actual_5 = ((a_12 = item(0, Polyline2D__get_Points(pl_2)), (b_12 = Pt_$ctor_7B00E9A0(2, 2), (vx_5 = (a_12.X - b_12.X), (vy_5 = (a_12.Y - b_12.Y), Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5))))))) < 1E-09;
        if ((actual_5 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, true, "first vertex updated too");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_5, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            }) ? ((arg_10 = toString(true), (arg_1_5 = toString(actual_5), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("first vertex updated too")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_5)("first vertex updated too"));
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
        let a_14, points_4, b_14, vx_6, vy_6, copyOfStruct_6, arg_11, arg_1_6;
        const actual_6 = ((a_14 = Polyline2D__get_SecondPoint((points_4 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(3, 4)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_4)))), (b_14 = Pt_$ctor_7B00E9A0(1, 2), (vx_6 = (a_14.X - b_14.X), (vy_6 = (a_14.Y - b_14.Y), Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6))))))) < 1E-09;
        if ((actual_6 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_6, true, "second point");
        }
        else {
            throw new Exception(contains((copyOfStruct_6 = actual_6, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            }) ? ((arg_11 = toString(true), (arg_1_6 = toString(actual_6), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("second point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_6)("second point"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set second point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_16, b_16, vx_7, vy_7, copyOfStruct_7, arg_12, arg_1_7;
        let pl_5;
        const points_5 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(3, 4)];
        pl_5 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_5));
        Polyline2D__set_SecondPoint_6ADE94FD(pl_5, Pt_$ctor_7B00E9A0(9, 9));
        const actual_7 = ((a_16 = item(1, Polyline2D__get_Points(pl_5)), (b_16 = Pt_$ctor_7B00E9A0(9, 9), (vx_7 = (a_16.X - b_16.X), (vy_7 = (a_16.Y - b_16.Y), Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7))))))) < 1E-09;
        if ((actual_7 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, true, "second point set");
        }
        else {
            throw new Exception(contains((copyOfStruct_7 = actual_7, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            }) ? ((arg_12 = toString(true), (arg_1_7 = toString(actual_7), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("second point set")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_7)("second point set"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("get second last point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_18, points_6, b_18, vx_8, vy_8, copyOfStruct_8, arg_13, arg_1_8;
        const actual_8 = ((a_18 = Polyline2D__get_SecondLastPoint((points_6 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(3, 4)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_6)))), (b_18 = Pt_$ctor_7B00E9A0(1, 2), (vx_8 = (a_18.X - b_18.X), (vy_8 = (a_18.Y - b_18.Y), Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8))))))) < 1E-09;
        if ((actual_8 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, true, "second last point");
        }
        else {
            throw new Exception(contains((copyOfStruct_8 = actual_8, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            }) ? ((arg_13 = toString(true), (arg_1_8 = toString(actual_8), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("second last point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_8)("second last point"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set second last point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_20, b_20, vx_9, vy_9, copyOfStruct_9, arg_14, arg_1_9;
        let pl_7;
        const points_7 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(3, 4)];
        pl_7 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_7));
        Polyline2D__set_SecondLastPoint_6ADE94FD(pl_7, Pt_$ctor_7B00E9A0(7, 7));
        const actual_9 = ((a_20 = item(1, Polyline2D__get_Points(pl_7)), (b_20 = Pt_$ctor_7B00E9A0(7, 7), (vx_9 = (a_20.X - b_20.X), (vy_9 = (a_20.Y - b_20.Y), Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9))))))) < 1E-09;
        if ((actual_9 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, true, "second last point set");
        }
        else {
            throw new Exception(contains((copyOfStruct_9 = actual_9, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            }) ? ((arg_14 = toString(true), (arg_1_9 = toString(actual_9), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("second last point set")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_9)("second last point set"));
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
        let copyOfStruct_10, arg_15, arg_1_10, copyOfStruct_11, arg_16, arg_1_11;
        const actual_10 = Polyline2D__get_LastPointIndex(plOpen) | 0;
        if ((actual_10 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 3, "open polyline last point index");
        }
        else {
            throw new Exception(contains((copyOfStruct_10 = actual_10, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_10) => (structuralHash(x_10) | 0),
            }) ? ((arg_15 = int32ToString(3), (arg_1_10 = int32ToString(actual_10), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("open polyline last point index")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_10)("open polyline last point index"));
        }
        const actual_11 = Polyline2D__get_LastPointIndex(plClosed) | 0;
        if ((actual_11 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 4, "closed polyline last point index");
        }
        else {
            throw new Exception(contains((copyOfStruct_11 = actual_11, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            }) ? ((arg_16 = int32ToString(4), (arg_1_11 = int32ToString(actual_11), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("closed polyline last point index")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_11)("closed polyline last point index"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("last segment index", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let copyOfStruct_12, arg_17, arg_1_12, copyOfStruct_13, arg_18, arg_1_13;
        const actual_12 = Polyline2D__get_LastSegmentIndex(plOpen) | 0;
        if ((actual_12 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 2, "open polyline last segment index");
        }
        else {
            throw new Exception(contains((copyOfStruct_12 = actual_12, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            }) ? ((arg_17 = int32ToString(2), (arg_1_12 = int32ToString(actual_12), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("open polyline last segment index")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_12)("open polyline last segment index"));
        }
        const actual_13 = Polyline2D__get_LastSegmentIndex(plClosed) | 0;
        if ((actual_13 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 3, "closed polyline last segment index");
        }
        else {
            throw new Exception(contains((copyOfStruct_13 = actual_13, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            }) ? ((arg_18 = int32ToString(3), (arg_1_13 = int32ToString(actual_13), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("closed polyline last segment index")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_13)("closed polyline last segment index"));
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
        let points_9;
        Expect_isFalse(Polyline2D__IsAlmostClosed_5E38073B((points_9 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_9))), 1))("two points should not be almost closed");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})()])), Test_testList("Segments property", ofArray([(() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segments of open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let copyOfStruct_14, arg_19, arg_1_14, a_22, copyOfStruct_15, ln, b_22, vx_10, vy_10, copyOfStruct_16, arg_20, arg_1_15, a_24, copyOfStruct_17, ln_1, b_24, vx_11, vy_11, copyOfStruct_18, arg_21, arg_1_16, a_26, copyOfStruct_19, ln_2, b_26, vx_12, vy_12, copyOfStruct_20, arg_22, arg_1_17;
        const segs = Polyline2D__get_Segments(plOpen);
        const actual_14 = segs.length | 0;
        if ((actual_14 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_14, 3, "3 segments");
        }
        else {
            throw new Exception(contains((copyOfStruct_14 = actual_14, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            }) ? ((arg_19 = int32ToString(3), (arg_1_14 = int32ToString(actual_14), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("3 segments")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_14)("3 segments"));
        }
        const actual_15 = ((a_22 = ((copyOfStruct_15 = item(0, segs), (ln = copyOfStruct_15, Pt_$ctor_7B00E9A0_1(ln.FromX, ln.FromY)))), (b_22 = Pt_$ctor_7B00E9A0(0, 0), (vx_10 = (a_22.X - b_22.X), (vy_10 = (a_22.Y - b_22.Y), Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10))))))) < 1E-09;
        if ((actual_15 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, true, "seg 0 from");
        }
        else {
            throw new Exception(contains((copyOfStruct_16 = actual_15, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            }) ? ((arg_20 = toString(true), (arg_1_15 = toString(actual_15), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("seg 0 from")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_15)("seg 0 from"));
        }
        const actual_16 = ((a_24 = ((copyOfStruct_17 = item(0, segs), (ln_1 = copyOfStruct_17, Pt_$ctor_7B00E9A0_1(ln_1.ToX, ln_1.ToY)))), (b_24 = Pt_$ctor_7B00E9A0(10, 0), (vx_11 = (a_24.X - b_24.X), (vy_11 = (a_24.Y - b_24.Y), Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11))))))) < 1E-09;
        if ((actual_16 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_16, true, "seg 0 to");
        }
        else {
            throw new Exception(contains((copyOfStruct_18 = actual_16, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            }) ? ((arg_21 = toString(true), (arg_1_16 = toString(actual_16), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("seg 0 to")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_16)("seg 0 to"));
        }
        const actual_17 = ((a_26 = ((copyOfStruct_19 = item(2, segs), (ln_2 = copyOfStruct_19, Pt_$ctor_7B00E9A0_1(ln_2.ToX, ln_2.ToY)))), (b_26 = Pt_$ctor_7B00E9A0(0, 10), (vx_12 = (a_26.X - b_26.X), (vy_12 = (a_26.Y - b_26.Y), Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12))))))) < 1E-09;
        if ((actual_17 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, true, "seg 2 to");
        }
        else {
            throw new Exception(contains((copyOfStruct_20 = actual_17, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            }) ? ((arg_22 = toString(true), (arg_1_17 = toString(actual_17), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("seg 2 to")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_17)("seg 2 to"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segments of empty polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let copyOfStruct_21, arg_23, arg_1_18;
        const segs_1 = Polyline2D__get_Segments(plEmpty);
        const actual_18 = segs_1.length | 0;
        if ((actual_18 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_18, 0, "empty has no segments");
        }
        else {
            throw new Exception(contains((copyOfStruct_21 = actual_18, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_18) => (structuralHash(x_18) | 0),
            }) ? ((arg_23 = int32ToString(0), (arg_1_18 = int32ToString(actual_18), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("empty has no segments")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_18)("empty has no segments"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segments of single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let copyOfStruct_22, arg_24, arg_1_19;
        const segs_2 = Polyline2D__get_Segments(plSinglePoint);
        const actual_19 = segs_2.length | 0;
        if ((actual_19 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, 0, "single point has no segments");
        }
        else {
            throw new Exception(contains((copyOfStruct_22 = actual_19, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            }) ? ((arg_24 = int32ToString(0), (arg_1_19 = int32ToString(actual_19), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("single point has no segments")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_19)("single point has no segments"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})()])), Test_testList("SegmentVectors property", ofArray([(() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segment vectors of open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let copyOfStruct_23, arg_25, arg_1_20;
        const vecs = Polyline2D__get_SegmentVectors(plOpen);
        const actual_20 = vecs.length | 0;
        if ((actual_20 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_20, 3, "3 vectors");
        }
        else {
            throw new Exception(contains((copyOfStruct_23 = actual_20, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            }) ? ((arg_25 = int32ToString(3), (arg_1_20 = int32ToString(actual_20), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("3 vectors")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_20)("3 vectors"));
        }
        Expect_isTrue((Math.abs(item(0, vecs).X - 10) < 1E-09) && (Math.abs(item(0, vecs).Y) < 1E-09))("vec 0");
        Expect_isTrue((Math.abs(item(1, vecs).X) < 1E-09) && (Math.abs(item(1, vecs).Y - 10) < 1E-09))("vec 1");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("segment vectors of empty polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let copyOfStruct_28, arg_26, arg_1_21;
        const vecs_1 = Polyline2D__get_SegmentVectors(plEmpty);
        const actual_21 = vecs_1.length | 0;
        if ((actual_21 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 0, "empty has no vectors");
        }
        else {
            throw new Exception(contains((copyOfStruct_28 = actual_21, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            }) ? ((arg_26 = int32ToString(0), (arg_1_21 = int32ToString(actual_21), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_26)(arg_1_21)("empty has no vectors")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_21)("empty has no vectors"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})()])), Test_testList("SignedDistanceTo", ofArray([(() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("inside point positive distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const sd = Polyline2D__SignedDistanceTo_6ADE94FD(plClosed, Pt_$ctor_7B00E9A0(5, 5));
        Expect_isTrue(sd > 0)("inside positive");
        Expect_isTrue(Math.abs(sd - 5) < 1E-09)("inside distance value");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("outside point negative distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const sd_1 = Polyline2D__SignedDistanceTo_6ADE94FD(plClosed, Pt_$ctor_7B00E9A0(15, 5));
        Expect_isTrue(sd_1 < 0)("outside negative");
        Expect_isTrue(Math.abs(sd_1 + 5) < 1E-09)("outside distance value");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("point on edge", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const sd_2 = Polyline2D__SignedDistanceTo_6ADE94FD(plClosed, Pt_$ctor_7B00E9A0(5, 0));
        Expect_isTrue(Math.abs(sd_2) < 1E-06)("on edge distance near zero");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})()])), Test_testList("ToString and AsString", ofArray([(() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("toString on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        Expect_stringContains(toString(plEmpty), "empty", "should say empty");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("toString on closed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        Expect_stringContains(toString(plClosed), "closed", "should say closed");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("toString on open", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        Expect_stringContains(toString(plOpen), "open", "should say open");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("asString on empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        Expect_stringContains(Polyline2D__get_AsString(plEmpty), "empty", "should say empty");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("asString on closed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        Expect_stringContains(Polyline2D__get_AsString(plClosed), "closed", "should say closed");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})()])), Test_testList("AsFSharpCode", singleton((() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("produces valid code string", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        Expect_stringContains(Polyline2D__get_AsFSharpCode(plLine), "Polyline2D.create", "should contain create");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})())), Test_testList("Static close", ofArray([(() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let copyOfStruct_29, arg_27, arg_1_22, a_28, b_28, vx_13, vy_13, copyOfStruct_30, arg_28, arg_1_23;
        const closed = Polyline2D_close_Z5A89AEF5(plOpen);
        Expect_isTrue(Polyline2D__get_IsClosed(closed))("should be closed");
        const actual_22 = Polyline2D__get_PointCount(closed) | 0;
        const expected_35 = (Polyline2D__get_PointCount(plOpen) + 1) | 0;
        if ((actual_22 === expected_35) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_22, expected_35, "one point added");
        }
        else {
            throw new Exception(contains((copyOfStruct_29 = actual_22, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            }) ? ((arg_27 = int32ToString(expected_35), (arg_1_22 = int32ToString(actual_22), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_27)(arg_1_22)("one point added")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_35)(actual_22)("one point added"));
        }
        const actual_23 = ((a_28 = item(0, Polyline2D__get_Points(closed)), (b_28 = item(Polyline2D__get_PointCount(closed) - 1, Polyline2D__get_Points(closed)), (vx_13 = (a_28.X - b_28.X), (vy_13 = (a_28.Y - b_28.Y), Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13))))))) < 1E-09;
        if ((actual_23 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, true, "first equals last");
        }
        else {
            throw new Exception(contains((copyOfStruct_30 = actual_23, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_23) => (structuralHash(x_23) | 0),
            }) ? ((arg_28 = toString(true), (arg_1_23 = toString(actual_23), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_28)(arg_1_23)("first equals last")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_23)("first equals last"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close already closed polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let copyOfStruct_31, arg_29, arg_1_24;
        const closed_1 = Polyline2D_close_Z5A89AEF5(plClosed);
        Expect_isTrue(Polyline2D__get_IsClosed(closed_1))("should remain closed");
        const actual_24 = Polyline2D__get_PointCount(closed_1) | 0;
        const expected_38 = Polyline2D__get_PointCount(plClosed) | 0;
        if ((actual_24 === expected_38) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_24, expected_38, "no extra point added");
        }
        else {
            throw new Exception(contains((copyOfStruct_31 = actual_24, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            }) ? ((arg_29 = int32ToString(expected_38), (arg_1_24 = int32ToString(actual_24), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_29)(arg_1_24)("no extra point added")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_38)(actual_24)("no extra point added"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close nearly closed polyline snaps last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let points_10, a_30, b_30, vx_14, vy_14, copyOfStruct_32, arg_30, arg_1_25;
        const closed_2 = Polyline2D_close_Z5A89AEF5((points_10 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 1E-08)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_10))));
        Expect_isTrue(Polyline2D__get_IsClosed(closed_2))("should be closed");
        const actual_25 = ((a_30 = item(0, Polyline2D__get_Points(closed_2)), (b_30 = item(Polyline2D__get_PointCount(closed_2) - 1, Polyline2D__get_Points(closed_2)), (vx_14 = (a_30.X - b_30.X), (vy_14 = (a_30.Y - b_30.Y), Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14))))))) < 1E-09;
        if ((actual_25 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, true, "last snapped to first");
        }
        else {
            throw new Exception(contains((copyOfStruct_32 = actual_25, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            }) ? ((arg_30 = toString(true), (arg_1_25 = toString(actual_25), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_30)(arg_1_25)("last snapped to first")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_25)("last snapped to first"));
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
        let a_32, b_32, vx_15, vy_15, copyOfStruct_33, arg_31, arg_1_26, copyOfStruct_34, arg_32, arg_1_27;
        let pl_12;
        const points_11 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 1E-08)];
        pl_12 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_11));
        Polyline2D_closeInPlace_Z5A89AEF5(pl_12);
        const actual_26 = ((a_32 = item(0, Polyline2D__get_Points(pl_12)), (b_32 = item(Polyline2D__get_PointCount(pl_12) - 1, Polyline2D__get_Points(pl_12)), (vx_15 = (a_32.X - b_32.X), (vy_15 = (a_32.Y - b_32.Y), Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15))))))) < 1E-09;
        if ((actual_26 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_26, true, "last snapped to first");
        }
        else {
            throw new Exception(contains((copyOfStruct_33 = actual_26, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            }) ? ((arg_31 = toString(true), (arg_1_26 = toString(actual_26), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_31)(arg_1_26)("last snapped to first")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_26)("last snapped to first"));
        }
        const actual_27 = Polyline2D__get_PointCount(pl_12) | 0;
        if ((actual_27 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, 4, "no extra point, just snapped");
        }
        else {
            throw new Exception(contains((copyOfStruct_34 = actual_27, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            }) ? ((arg_32 = int32ToString(4), (arg_1_27 = int32ToString(actual_27), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_32)(arg_1_27)("no extra point, just snapped")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_27)("no extra point, just snapped"));
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
        let points_14, points_15;
        Expect_isFalse(Polyline2D_equals(1E-09, (points_14 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_14))), (points_15 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_15)))))("different counts");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})()])), Test_testList("Static rotateWithCenter", singleton((() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotate 90 degrees around center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let a_39, b_39, vx_16, vy_16, copyOfStruct_35, arg_33, arg_1_28, a_41, b_41, vx_17, vy_17, copyOfStruct_36, arg_34, arg_1_29;
        let r_16;
        const rad = 0.017453292519943295 * 90;
        r_16 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad), Math.cos(rad));
        const rotated = Polyline2D_rotateWithCenter(Pt_$ctor_7B00E9A0(5, 5), r_16, plLine);
        const actual_28 = ((a_39 = item(0, Polyline2D__get_Points(rotated)), (b_39 = Pt_$ctor_7B00E9A0(10, 0), (vx_16 = (a_39.X - b_39.X), (vy_16 = (a_39.Y - b_39.Y), Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16))))))) < 1E-09;
        if ((actual_28 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, true, "rotated first point");
        }
        else {
            throw new Exception(contains((copyOfStruct_35 = actual_28, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_28) => (structuralHash(x_28) | 0),
            }) ? ((arg_33 = toString(true), (arg_1_28 = toString(actual_28), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_28)("rotated first point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_28)("rotated first point"));
        }
        const actual_29 = ((a_41 = item(1, Polyline2D__get_Points(rotated)), (b_41 = Pt_$ctor_7B00E9A0(10, 10), (vx_17 = (a_41.X - b_41.X), (vy_17 = (a_41.Y - b_41.Y), Math.sqrt((vx_17 * vx_17) + (vy_17 * vy_17))))))) < 1E-09;
        if ((actual_29 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, true, "rotated second point");
        }
        else {
            throw new Exception(contains((copyOfStruct_36 = actual_29, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_29) => (structuralHash(x_29) | 0),
            }) ? ((arg_34 = toString(true), (arg_1_29 = toString(actual_29), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_34)(arg_1_29)("rotated second point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_29)("rotated second point"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})())), Test_testList("Static wrappers", ofArray([(() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let a_43, b_43, vx_18, vy_18, copyOfStruct_37, arg_35, arg_1_30;
        const actual_30 = ((a_43 = Polyline2D_start_Z5A89AEF5(plOpen), (b_43 = Polyline2D__get_Start(plOpen), (vx_18 = (a_43.X - b_43.X), (vy_18 = (a_43.Y - b_43.Y), Math.sqrt((vx_18 * vx_18) + (vy_18 * vy_18))))))) < 1E-09;
        if ((actual_30 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, true, "static start");
        }
        else {
            throw new Exception(contains((copyOfStruct_37 = actual_30, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_30) => (structuralHash(x_30) | 0),
            }) ? ((arg_35 = toString(true), (arg_1_30 = toString(actual_30), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_30)("static start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_30)("static start"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static ende", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let a_45, b_45, vx_19, vy_19, copyOfStruct_38, arg_36, arg_1_31;
        const actual_31 = ((a_45 = Polyline2D_ende_Z5A89AEF5(plOpen), (b_45 = Polyline2D__get_End(plOpen), (vx_19 = (a_45.X - b_45.X), (vy_19 = (a_45.Y - b_45.Y), Math.sqrt((vx_19 * vx_19) + (vy_19 * vy_19))))))) < 1E-09;
        if ((actual_31 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, true, "static ende");
        }
        else {
            throw new Exception(contains((copyOfStruct_38 = actual_31, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_31) => (structuralHash(x_31) | 0),
            }) ? ((arg_36 = toString(true), (arg_1_31 = toString(actual_31), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_31)("static ende")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_31)("static ende"));
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
        let copyOfStruct_39, arg_37, arg_1_32;
        const actual_32 = Polyline2D__get_Points_1(plOpen).length | 0;
        const expected_52 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_32 === expected_52) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_32, expected_52, "static pointCount");
        }
        else {
            throw new Exception(contains((copyOfStruct_39 = actual_32, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_32) => (structuralHash(x_32) | 0),
            }) ? ((arg_37 = int32ToString(expected_52), (arg_1_32 = int32ToString(actual_32), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_32)("static pointCount")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_52)(actual_32)("static pointCount"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static segmentCount", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let copyOfStruct_40, arg_38, arg_1_33;
        const actual_33 = Polyline2D__get_SegmentCount_1(plOpen) | 0;
        const expected_53 = Polyline2D__get_SegmentCount(plOpen) | 0;
        if ((actual_33 === expected_53) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, expected_53, "static segmentCount");
        }
        else {
            throw new Exception(contains((copyOfStruct_40 = actual_33, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_33) => (structuralHash(x_33) | 0),
            }) ? ((arg_38 = int32ToString(expected_53), (arg_1_33 = int32ToString(actual_33), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_38)(arg_1_33)("static segmentCount")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_53)(actual_33)("static segmentCount"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static reverseInPlace", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let a_47, b_47, vx_20, vy_20, copyOfStruct_41, arg_39, arg_1_34;
        const copy = Polyline2D__Clone(plOpen);
        Polyline2D_reverseInPlace_Z5A89AEF5(copy);
        const actual_34 = ((a_47 = item(0, Polyline2D__get_Points(copy)), (b_47 = Pt_$ctor_7B00E9A0(0, 10), (vx_20 = (a_47.X - b_47.X), (vy_20 = (a_47.Y - b_47.Y), Math.sqrt((vx_20 * vx_20) + (vy_20 * vy_20))))))) < 1E-09;
        if ((actual_34 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_34, true, "first after reverse");
        }
        else {
            throw new Exception(contains((copyOfStruct_41 = actual_34, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_34) => (structuralHash(x_34) | 0),
            }) ? ((arg_39 = toString(true), (arg_1_34 = toString(actual_34), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_39)(arg_1_34)("first after reverse")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_34)("first after reverse"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static reverse", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        let a_49, b_49, vx_21, vy_21, copyOfStruct_42, arg_40, arg_1_35, a_51, b_51, vx_22, vy_22, copyOfStruct_43, arg_41, arg_1_36;
        const actual_35 = ((a_49 = item(0, Polyline2D__get_Points(Polyline2D_reverse_Z5A89AEF5(plOpen))), (b_49 = Pt_$ctor_7B00E9A0(0, 10), (vx_21 = (a_49.X - b_49.X), (vy_21 = (a_49.Y - b_49.Y), Math.sqrt((vx_21 * vx_21) + (vy_21 * vy_21))))))) < 1E-09;
        if ((actual_35 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, true, "first after reverse");
        }
        else {
            throw new Exception(contains((copyOfStruct_42 = actual_35, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_35) => (structuralHash(x_35) | 0),
            }) ? ((arg_40 = toString(true), (arg_1_35 = toString(actual_35), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_40)(arg_1_35)("first after reverse")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_35)("first after reverse"));
        }
        const actual_36 = ((a_51 = item(0, Polyline2D__get_Points(plOpen)), (b_51 = Pt_$ctor_7B00E9A0(0, 0), (vx_22 = (a_51.X - b_51.X), (vy_22 = (a_51.Y - b_51.Y), Math.sqrt((vx_22 * vx_22) + (vy_22 * vy_22))))))) < 1E-09;
        if ((actual_36 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_36, true, "original unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_43 = actual_36, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_36) => (structuralHash(x_36) | 0),
            }) ? ((arg_41 = toString(true), (arg_1_36 = toString(actual_36), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_41)(arg_1_36)("original unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_36)("original unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static evaluateAt", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let a_53, b_53, vx_23, vy_23, copyOfStruct_44, arg_42, arg_1_37;
        const actual_37 = ((a_53 = Polyline2D_evaluateAt(0.5, plOpen), (b_53 = Pt_$ctor_7B00E9A0(5, 0), (vx_23 = (a_53.X - b_53.X), (vy_23 = (a_53.Y - b_53.Y), Math.sqrt((vx_23 * vx_23) + (vy_23 * vy_23))))))) < 1E-09;
        if ((actual_37 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, true, "static evaluateAt");
        }
        else {
            throw new Exception(contains((copyOfStruct_44 = actual_37, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_37) => (structuralHash(x_37) | 0),
            }) ? ((arg_42 = toString(true), (arg_1_37 = toString(actual_37), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_42)(arg_1_37)("static evaluateAt")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_37)("static evaluateAt"));
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
        let a_55, b_55, vx_24, vy_24, copyOfStruct_45, arg_43, arg_1_38;
        const actual_38 = ((a_55 = Polyline2D__ClosestPoint_6ADE94FD_1(plOpen, Pt_$ctor_7B00E9A0(5, -1)), (b_55 = Pt_$ctor_7B00E9A0(5, 0), (vx_24 = (a_55.X - b_55.X), (vy_24 = (a_55.Y - b_55.Y), Math.sqrt((vx_24 * vx_24) + (vy_24 * vy_24))))))) < 1E-09;
        if ((actual_38 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_38, true, "static closestPoint");
        }
        else {
            throw new Exception(contains((copyOfStruct_45 = actual_38, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_38) => (structuralHash(x_38) | 0),
            }) ? ((arg_43 = toString(true), (arg_1_38 = toString(actual_38), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_43)(arg_1_38)("static closestPoint")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_38)("static closestPoint"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static closestVertex", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        let copyOfStruct_46, arg_44, arg_1_39;
        const actual_39 = Polyline2D__ClosestVertex_6ADE94FD(plOpen, Pt_$ctor_7B00E9A0(9, 0.5)) | 0;
        if ((actual_39 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_39, 1, "static closestVertex nearest to (10,0)");
        }
        else {
            throw new Exception(contains((copyOfStruct_46 = actual_39, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_39) => (structuralHash(x_39) | 0),
            }) ? ((arg_44 = int32ToString(1), (arg_1_39 = int32ToString(actual_39), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_44)(arg_1_39)("static closestVertex nearest to (10,0)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_39)("static closestVertex nearest to (10,0)"));
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
        let a_57, b_57, vx_25, vy_25, copyOfStruct_47, arg_45, arg_1_40;
        const actual_40 = ((a_57 = item(1, Polyline2D__get_Points(Polyline2D_scale(3, plLine))), (b_57 = Pt_$ctor_7B00E9A0(30, 0), (vx_25 = (a_57.X - b_57.X), (vy_25 = (a_57.Y - b_57.Y), Math.sqrt((vx_25 * vx_25) + (vy_25 * vy_25))))))) < 1E-09;
        if ((actual_40 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_40, true, "static scale");
        }
        else {
            throw new Exception(contains((copyOfStruct_47 = actual_40, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_40) => (structuralHash(x_40) | 0),
            }) ? ((arg_45 = toString(true), (arg_1_40 = toString(actual_40), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_45)(arg_1_40)("static scale")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_40)("static scale"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("static windingNumber", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        Expect_isTrue(Polyline2D__WindingNumber_6ADE94FD_1(plClosed, Pt_$ctor_7B00E9A0(5, 5)) !== 0)("static windingNumber inside");
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
        let copyOfStruct_48, arg_46, arg_1_41;
        const ra = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 1)];
        const pl_30 = Polyline2D_createDirectlyUnsafe_Z5FD8CF3C(ra);
        void (ra.push(Pt_$ctor_7B00E9A0(2, 2)));
        const actual_41 = Polyline2D__get_PointCount(pl_30) | 0;
        if ((actual_41 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_41, 3, "shared array reflects changes");
        }
        else {
            throw new Exception(contains((copyOfStruct_48 = actual_41, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_41) => (structuralHash(x_41) | 0),
            }) ? ((arg_46 = int32ToString(3), (arg_1_41 = int32ToString(actual_41), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_46)(arg_1_41)("shared array reflects changes")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_41)("shared array reflects changes"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointsUnsafeInternal returns same reference", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let copyOfStruct_49, arg_47, arg_1_42, copyOfStruct_50, arg_48, arg_1_43;
        let pl_31;
        const points_16 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 1)];
        pl_31 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_16));
        const pts = Polyline2D_pointsUnsafeInternal_Z5A89AEF5(pl_31);
        const actual_42 = pts.length | 0;
        if ((actual_42 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_42, 2, "same count");
        }
        else {
            throw new Exception(contains((copyOfStruct_49 = actual_42, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_42) => (structuralHash(x_42) | 0),
            }) ? ((arg_47 = int32ToString(2), (arg_1_42 = int32ToString(actual_42), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_47)(arg_1_42)("same count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_42)("same count"));
        }
        void (pts.push(Pt_$ctor_7B00E9A0(2, 2)));
        const actual_43 = Polyline2D__get_PointCount(pl_31) | 0;
        if ((actual_43 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_43, 3, "mutation reflected in polyline");
        }
        else {
            throw new Exception(contains((copyOfStruct_50 = actual_43, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_43) => (structuralHash(x_43) | 0),
            }) ? ((arg_48 = int32ToString(3), (arg_1_43 = int32ToString(actual_43), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_48)(arg_1_43)("mutation reflected in polyline")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_43)("mutation reflected in polyline"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})()])), Test_testList("EvaluateAt edge cases", ofArray([(() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("evaluate at near-integer snaps to vertex", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let a_59, points_17, b_59, vx_26, vy_26, copyOfStruct_51, arg_49, arg_1_44;
        const actual_44 = ((a_59 = Polyline2D__EvaluateAt_5E38073B((points_17 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_17))), 0.9999999), (b_59 = Pt_$ctor_7B00E9A0(10, 0), (vx_26 = (a_59.X - b_59.X), (vy_26 = (a_59.Y - b_59.Y), Math.sqrt((vx_26 * vx_26) + (vy_26 * vy_26))))))) < 1E-09;
        if ((actual_44 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_44, true, "near integer snaps");
        }
        else {
            throw new Exception(contains((copyOfStruct_51 = actual_44, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_44) => (structuralHash(x_44) | 0),
            }) ? ((arg_49 = toString(true), (arg_1_44 = toString(actual_44), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_49)(arg_1_44)("near integer snaps")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_44)("near integer snaps"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("evaluate at exactly last parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let a_61, points_18, b_61, vx_27, vy_27, copyOfStruct_52, arg_50, arg_1_45;
        const actual_45 = ((a_61 = Polyline2D__EvaluateAt_5E38073B((points_18 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_18))), 2), (b_61 = Pt_$ctor_7B00E9A0(10, 10), (vx_27 = (a_61.X - b_61.X), (vy_27 = (a_61.Y - b_61.Y), Math.sqrt((vx_27 * vx_27) + (vy_27 * vy_27))))))) < 1E-09;
        if ((actual_45 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_45, true, "at last");
        }
        else {
            throw new Exception(contains((copyOfStruct_52 = actual_45, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_45) => (structuralHash(x_45) | 0),
            }) ? ((arg_50 = toString(true), (arg_1_45 = toString(actual_45), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_50)(arg_1_45)("at last")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_45)("at last"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("evaluate at near zero negative", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let a_63, points_19, b_63, vx_28, vy_28, copyOfStruct_53, arg_51, arg_1_46;
        const actual_46 = ((a_63 = Polyline2D__EvaluateAt_5E38073B((points_19 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_19))), -1E-07), (b_63 = Pt_$ctor_7B00E9A0(0, 0), (vx_28 = (a_63.X - b_63.X), (vy_28 = (a_63.Y - b_63.Y), Math.sqrt((vx_28 * vx_28) + (vy_28 * vy_28))))))) < 1E-09;
        if ((actual_46 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_46, true, "near zero negative");
        }
        else {
            throw new Exception(contains((copyOfStruct_53 = actual_46, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_46) => (structuralHash(x_46) | 0),
            }) ? ((arg_51 = toString(true), (arg_1_46 = toString(actual_46), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_51)(arg_1_46)("near zero negative")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_46)("near zero negative"));
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
        let a_65, b_65, vx_29, vy_29, copyOfStruct_54, arg_52, arg_1_47;
        const actual_47 = ((a_65 = Polyline2D__ClosestPoint_6ADE94FD(plOpen, Pt_$ctor_7B00E9A0(10, 0)), (b_65 = Pt_$ctor_7B00E9A0(10, 0), (vx_29 = (a_65.X - b_65.X), (vy_29 = (a_65.Y - b_65.Y), Math.sqrt((vx_29 * vx_29) + (vy_29 * vy_29))))))) < 1E-09;
        if ((actual_47 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_47, true, "on vertex");
        }
        else {
            throw new Exception(contains((copyOfStruct_54 = actual_47, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_47) => (structuralHash(x_47) | 0),
            }) ? ((arg_52 = toString(true), (arg_1_47 = toString(actual_47), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_52)(arg_1_47)("on vertex")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_47)("on vertex"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closest point single point polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let a_67, b_67, vx_30, vy_30, copyOfStruct_55, arg_53, arg_1_48;
        const actual_48 = ((a_67 = Polyline2D__ClosestPoint_6ADE94FD(plSinglePoint, Pt_$ctor_7B00E9A0(5, 5)), (b_67 = Pt_$ctor_7B00E9A0(1, 1), (vx_30 = (a_67.X - b_67.X), (vy_30 = (a_67.Y - b_67.Y), Math.sqrt((vx_30 * vx_30) + (vy_30 * vy_30))))))) < 1E-09;
        if ((actual_48 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_48, true, "single point closest");
        }
        else {
            throw new Exception(contains((copyOfStruct_55 = actual_48, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_48) => (structuralHash(x_48) | 0),
            }) ? ((arg_53 = toString(true), (arg_1_48 = toString(actual_48), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_53)(arg_1_48)("single point closest")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_48)("single point closest"));
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
        let copyOfStruct_56, arg_54, arg_1_49;
        const actual_49 = Polyline2D__ClosestVertex_6ADE94FD_1(plSinglePoint, Pt_$ctor_7B00E9A0(5, 5)) | 0;
        if ((actual_49 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_49, 0, "single point vertex is 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_56 = actual_49, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_49) => (structuralHash(x_49) | 0),
            }) ? ((arg_54 = int32ToString(0), (arg_1_49 = int32ToString(actual_49), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_54)(arg_1_49)("single point vertex is 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_49)("single point vertex is 0"));
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
        let copyOfStruct_57, arg_55, arg_1_50;
        const actual_50 = Polyline2D__get_Length(plSinglePoint);
        if ((actual_50 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_50, 0, "single point length is 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_57 = actual_50, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_50) => (structuralHash(x_50) | 0),
            }) ? ((arg_55 = (0).toString(), (arg_1_50 = actual_50.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_55)(arg_1_50)("single point length is 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_50)("single point length is 0"));
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
        let a_69, b_69, vx_31, vy_31, copyOfStruct_58, arg_56, arg_1_51;
        const actual_51 = ((a_69 = Polyline2D__get_Center(plSinglePoint), (b_69 = Pt_$ctor_7B00E9A0(1, 1), (vx_31 = (a_69.X - b_69.X), (vy_31 = (a_69.Y - b_69.Y), Math.sqrt((vx_31 * vx_31) + (vy_31 * vy_31))))))) < 1E-09;
        if ((actual_51 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_51, true, "single point center");
        }
        else {
            throw new Exception(contains((copyOfStruct_58 = actual_51, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_51) => (structuralHash(x_51) | 0),
            }) ? ((arg_56 = toString(true), (arg_1_51 = toString(actual_51), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_56)(arg_1_51)("single point center")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_51)("single point center"));
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
        let a_72, b_72, vx_32, vy_32, copyOfStruct_59, arg_57, arg_1_52, a_74, b_74, vx_33, vy_33, copyOfStruct_60, arg_58, arg_1_53;
        const sub = Polyline2D_subPolyline(2.5, 0.5, plOpen);
        const actual_52 = ((a_72 = item(0, Polyline2D__get_Points(sub)), (b_72 = Pt_$ctor_7B00E9A0(5, 10), (vx_32 = (a_72.X - b_72.X), (vy_32 = (a_72.Y - b_72.Y), Math.sqrt((vx_32 * vx_32) + (vy_32 * vy_32))))))) < 1E-09;
        if ((actual_52 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_52, true, "reversed sub start");
        }
        else {
            throw new Exception(contains((copyOfStruct_59 = actual_52, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_52) => (structuralHash(x_52) | 0),
            }) ? ((arg_57 = toString(true), (arg_1_52 = toString(actual_52), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_57)(arg_1_52)("reversed sub start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_52)("reversed sub start"));
        }
        const actual_53 = ((a_74 = item(Polyline2D__get_PointCount(sub) - 1, Polyline2D__get_Points(sub)), (b_74 = Pt_$ctor_7B00E9A0(5, 0), (vx_33 = (a_74.X - b_74.X), (vy_33 = (a_74.Y - b_74.Y), Math.sqrt((vx_33 * vx_33) + (vy_33 * vy_33))))))) < 1E-09;
        if ((actual_53 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_53, true, "reversed sub end");
        }
        else {
            throw new Exception(contains((copyOfStruct_60 = actual_53, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_53) => (structuralHash(x_53) | 0),
            }) ? ((arg_58 = toString(true), (arg_1_53 = toString(actual_53), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_58)(arg_1_53)("reversed sub end")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_53)("reversed sub end"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_78);
    }));
})(), (() => {
    const builder$0040_79 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("sub polyline at integer params", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_79, Test_TestCaseBuilder__Delay_1505(builder$0040_79, () => {
        let a_77, b_77, vx_34, vy_34, copyOfStruct_61, arg_59, arg_1_54, a_79, b_79, vx_35, vy_35, copyOfStruct_62, arg_60, arg_1_55;
        const sub_1 = Polyline2D_subPolyline(1, 2, plOpen);
        const actual_54 = ((a_77 = item(0, Polyline2D__get_Points(sub_1)), (b_77 = Pt_$ctor_7B00E9A0(10, 0), (vx_34 = (a_77.X - b_77.X), (vy_34 = (a_77.Y - b_77.Y), Math.sqrt((vx_34 * vx_34) + (vy_34 * vy_34))))))) < 1E-09;
        if ((actual_54 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_54, true, "integer start");
        }
        else {
            throw new Exception(contains((copyOfStruct_61 = actual_54, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_54) => (structuralHash(x_54) | 0),
            }) ? ((arg_59 = toString(true), (arg_1_54 = toString(actual_54), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_59)(arg_1_54)("integer start")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_54)("integer start"));
        }
        const actual_55 = ((a_79 = item(Polyline2D__get_PointCount(sub_1) - 1, Polyline2D__get_Points(sub_1)), (b_79 = Pt_$ctor_7B00E9A0(10, 10), (vx_35 = (a_79.X - b_79.X), (vy_35 = (a_79.Y - b_79.Y), Math.sqrt((vx_35 * vx_35) + (vy_35 * vy_35))))))) < 1E-09;
        if ((actual_55 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_55, true, "integer end");
        }
        else {
            throw new Exception(contains((copyOfStruct_62 = actual_55, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_55) => (structuralHash(x_55) | 0),
            }) ? ((arg_60 = toString(true), (arg_1_55 = toString(actual_55), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_60)(arg_1_55)("integer end")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_55)("integer end"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_79);
    }));
})()])), Test_testList("tryFindSelfIntersection", ofArray([(() => {
    const builder$0040_80 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("no intersection in simple polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_80, Test_TestCaseBuilder__Delay_1505(builder$0040_80, () => {
        Expect_isNone(Polyline2D_tryFindSelfIntersection_Z5A89AEF5(plOpen), "simple polyline has no self intersection");
        Test_TestCaseBuilder__Zero(builder$0040_80);
    }));
})(), (() => {
    const builder$0040_81 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("no intersection in L-shape open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_81, Test_TestCaseBuilder__Delay_1505(builder$0040_81, () => {
        Expect_isNone(Polyline2D_tryFindSelfIntersection_Z5A89AEF5(plLShape), "L-shape open polyline has no self intersection");
        Test_TestCaseBuilder__Zero(builder$0040_81);
    }));
})(), (() => {
    const builder$0040_82 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("self intersection in figure-8", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_82, Test_TestCaseBuilder__Delay_1505(builder$0040_82, () => {
        let points_24, a_81, b_81, vx_36, vy_36, copyOfStruct_63, arg_61, arg_1_56;
        const result_2 = Polyline2D_tryFindSelfIntersection_Z5A89AEF5((points_24 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(0, 0)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_24))));
        Expect_isSome(result_2, "figure 8 has self intersection");
        if (result_2 == null) {
            Test_TestCaseBuilder__Zero(builder$0040_82);
        }
        else {
            const _j = result_2[2] | 0;
            const _i = result_2[1] | 0;
            const actual_56 = ((a_81 = result_2[0], (b_81 = Pt_$ctor_7B00E9A0(5, 5), (vx_36 = (a_81.X - b_81.X), (vy_36 = (a_81.Y - b_81.Y), Math.sqrt((vx_36 * vx_36) + (vy_36 * vy_36))))))) < 1E-09;
            if ((actual_56 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_56, true, "intersection near center");
            }
            else {
                throw new Exception(contains((copyOfStruct_63 = actual_56, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                    Equals: equals,
                    GetHashCode: (x_56) => (structuralHash(x_56) | 0),
                }) ? ((arg_61 = toString(true), (arg_1_56 = toString(actual_56), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_61)(arg_1_56)("intersection near center")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_56)("intersection near center"));
            }
            Test_TestCaseBuilder__Zero(builder$0040_82);
        }
    }));
})(), (() => {
    const builder$0040_83 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("touching segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_83, Test_TestCaseBuilder__Delay_1505(builder$0040_83, () => {
        let points_25;
        Expect_isSome(Polyline2D_tryFindSelfIntersection_Z5A89AEF5((points_25 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 5), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(5, 5), Pt_$ctor_7B00E9A0(0, 10)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_25)))), "touching segments should find intersection");
        Test_TestCaseBuilder__Zero(builder$0040_83);
    }));
})()])), Test_testList("CloseInPlace member", ofArray([(() => {
    const builder$0040_84 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close in place with close ends snaps", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_84, Test_TestCaseBuilder__Delay_1505(builder$0040_84, () => {
        let a_83, b_83, vx_37, vy_37, copyOfStruct_64, arg_62, arg_1_57, copyOfStruct_65, arg_63, arg_1_58;
        let pl_42;
        const points_26 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0, 0.0001)];
        pl_42 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_26));
        Polyline2D__CloseInPlace_5E38073B(pl_42, 0.001);
        const actual_57 = ((a_83 = item(Polyline2D__get_PointCount(pl_42) - 1, Polyline2D__get_Points(pl_42)), (b_83 = Pt_$ctor_7B00E9A0(0, 0), (vx_37 = (a_83.X - b_83.X), (vy_37 = (a_83.Y - b_83.Y), Math.sqrt((vx_37 * vx_37) + (vy_37 * vy_37))))))) < 1E-09;
        if ((actual_57 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_57, true, "last snapped to first");
        }
        else {
            throw new Exception(contains((copyOfStruct_64 = actual_57, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_57) => (structuralHash(x_57) | 0),
            }) ? ((arg_62 = toString(true), (arg_1_57 = toString(actual_57), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_62)(arg_1_57)("last snapped to first")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_57)("last snapped to first"));
        }
        const actual_58 = Polyline2D__get_PointCount(pl_42) | 0;
        if ((actual_58 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_58, 4, "no point added, snapped");
        }
        else {
            throw new Exception(contains((copyOfStruct_65 = actual_58, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_58) => (structuralHash(x_58) | 0),
            }) ? ((arg_63 = int32ToString(4), (arg_1_58 = int32ToString(actual_58), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_63)(arg_1_58)("no point added, snapped")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_58)("no point added, snapped"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_84);
    }));
})(), (() => {
    const builder$0040_85 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("close in place with far ends adds point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_85, Test_TestCaseBuilder__Delay_1505(builder$0040_85, () => {
        let copyOfStruct_66, arg_64, arg_1_59, a_85, b_85, vx_38, vy_38, copyOfStruct_67, arg_65, arg_1_60;
        let pl_43;
        const points_27 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 10)];
        pl_43 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_27));
        Polyline2D__CloseInPlace_5E38073B(pl_43, 0.001);
        const actual_59 = Polyline2D__get_PointCount(pl_43) | 0;
        if ((actual_59 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_59, 4, "point added");
        }
        else {
            throw new Exception(contains((copyOfStruct_66 = actual_59, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_59) => (structuralHash(x_59) | 0),
            }) ? ((arg_64 = int32ToString(4), (arg_1_59 = int32ToString(actual_59), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_64)(arg_1_59)("point added")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_59)("point added"));
        }
        const actual_60 = ((a_85 = item(3, Polyline2D__get_Points(pl_43)), (b_85 = Pt_$ctor_7B00E9A0(0, 0), (vx_38 = (a_85.X - b_85.X), (vy_38 = (a_85.Y - b_85.Y), Math.sqrt((vx_38 * vx_38) + (vy_38 * vy_38))))))) < 1E-09;
        if ((actual_60 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_60, true, "added first as last");
        }
        else {
            throw new Exception(contains((copyOfStruct_67 = actual_60, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_60) => (structuralHash(x_60) | 0),
            }) ? ((arg_65 = toString(true), (arg_1_60 = toString(actual_60), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_65)(arg_1_60)("added first as last")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_60)("added first as last"));
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
        let a_87, ln_3, b_87, vx_39, vy_39, copyOfStruct_68, arg_66, arg_1_61, a_89, ln_4, b_89, vx_40, vy_40, copyOfStruct_69, arg_67, arg_1_62;
        const seg = Polyline2D__GetSegment_Z524259A4(plOpen, 2);
        const actual_61 = ((a_87 = ((ln_3 = seg, Pt_$ctor_7B00E9A0_1(ln_3.FromX, ln_3.FromY))), (b_87 = Pt_$ctor_7B00E9A0(10, 10), (vx_39 = (a_87.X - b_87.X), (vy_39 = (a_87.Y - b_87.Y), Math.sqrt((vx_39 * vx_39) + (vy_39 * vy_39))))))) < 1E-09;
        if ((actual_61 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_61, true, "last segment from");
        }
        else {
            throw new Exception(contains((copyOfStruct_68 = actual_61, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_61) => (structuralHash(x_61) | 0),
            }) ? ((arg_66 = toString(true), (arg_1_61 = toString(actual_61), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_66)(arg_1_61)("last segment from")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_61)("last segment from"));
        }
        const actual_62 = ((a_89 = ((ln_4 = seg, Pt_$ctor_7B00E9A0_1(ln_4.ToX, ln_4.ToY))), (b_89 = Pt_$ctor_7B00E9A0(0, 10), (vx_40 = (a_89.X - b_89.X), (vy_40 = (a_89.Y - b_89.Y), Math.sqrt((vx_40 * vx_40) + (vy_40 * vy_40))))))) < 1E-09;
        if ((actual_62 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_62, true, "last segment to");
        }
        else {
            throw new Exception(contains((copyOfStruct_69 = actual_62, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_62) => (structuralHash(x_62) | 0),
            }) ? ((arg_67 = toString(true), (arg_1_62 = toString(actual_62), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_67)(arg_1_62)("last segment to")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_62)("last segment to"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_87);
    }));
})(), (() => {
    const builder$0040_88 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("last segment matches GetSegment last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_88, Test_TestCaseBuilder__Delay_1505(builder$0040_88, () => {
        let a_91, ln_5, b_91, ln_6, vx_41, vy_41, copyOfStruct_70, arg_68, arg_1_63, a_93, ln_7, b_93, ln_8, vx_42, vy_42, copyOfStruct_71, arg_69, arg_1_64;
        const seg1 = Polyline2D__get_LastSegment(plOpen);
        const seg2 = Polyline2D__GetSegment_Z524259A4(plOpen, Polyline2D__get_LastSegmentIndex(plOpen));
        const actual_63 = ((a_91 = ((ln_5 = seg1, Pt_$ctor_7B00E9A0_1(ln_5.FromX, ln_5.FromY))), (b_91 = ((ln_6 = seg2, Pt_$ctor_7B00E9A0_1(ln_6.FromX, ln_6.FromY))), (vx_41 = (a_91.X - b_91.X), (vy_41 = (a_91.Y - b_91.Y), Math.sqrt((vx_41 * vx_41) + (vy_41 * vy_41))))))) < 1E-09;
        if ((actual_63 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_63, true, "from matches");
        }
        else {
            throw new Exception(contains((copyOfStruct_70 = actual_63, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_63) => (structuralHash(x_63) | 0),
            }) ? ((arg_68 = toString(true), (arg_1_63 = toString(actual_63), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_68)(arg_1_63)("from matches")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_63)("from matches"));
        }
        const actual_64 = ((a_93 = ((ln_7 = seg1, Pt_$ctor_7B00E9A0_1(ln_7.ToX, ln_7.ToY))), (b_93 = ((ln_8 = seg2, Pt_$ctor_7B00E9A0_1(ln_8.ToX, ln_8.ToY))), (vx_42 = (a_93.X - b_93.X), (vy_42 = (a_93.Y - b_93.Y), Math.sqrt((vx_42 * vx_42) + (vy_42 * vy_42))))))) < 1E-09;
        if ((actual_64 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_64, true, "to matches");
        }
        else {
            throw new Exception(contains((copyOfStruct_71 = actual_64, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_64) => (structuralHash(x_64) | 0),
            }) ? ((arg_69 = toString(true), (arg_1_64 = toString(actual_64), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_69)(arg_1_64)("to matches")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_64)("to matches"));
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
        let a_95, b_95, vx_43, vy_43, copyOfStruct_72, arg_70, arg_1_65;
        const pl_45 = Polyline2D__Clone(plOpen);
        Polyline2D__set_Start_6ADE94FD(pl_45, Pt_$ctor_7B00E9A0(99, 99));
        const actual_65 = ((a_95 = item(0, Polyline2D__get_Points(pl_45)), (b_95 = Pt_$ctor_7B00E9A0(99, 99), (vx_43 = (a_95.X - b_95.X), (vy_43 = (a_95.Y - b_95.Y), Math.sqrt((vx_43 * vx_43) + (vy_43 * vy_43))))))) < 1E-09;
        if ((actual_65 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_65, true, "start set");
        }
        else {
            throw new Exception(contains((copyOfStruct_72 = actual_65, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_65) => (structuralHash(x_65) | 0),
            }) ? ((arg_70 = toString(true), (arg_1_65 = toString(actual_65), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_70)(arg_1_65)("start set")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_65)("start set"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_91);
    }));
})(), (() => {
    const builder$0040_92 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_92, Test_TestCaseBuilder__Delay_1505(builder$0040_92, () => {
        let a_97, b_97, vx_44, vy_44, copyOfStruct_73, arg_71, arg_1_66;
        const pl_46 = Polyline2D__Clone(plOpen);
        Polyline2D__set_End_6ADE94FD(pl_46, Pt_$ctor_7B00E9A0(88, 88));
        const actual_66 = ((a_97 = item(Polyline2D__get_PointCount(pl_46) - 1, Polyline2D__get_Points(pl_46)), (b_97 = Pt_$ctor_7B00E9A0(88, 88), (vx_44 = (a_97.X - b_97.X), (vy_44 = (a_97.Y - b_97.Y), Math.sqrt((vx_44 * vx_44) + (vy_44 * vy_44))))))) < 1E-09;
        if ((actual_66 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_66, true, "end set");
        }
        else {
            throw new Exception(contains((copyOfStruct_73 = actual_66, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_66) => (structuralHash(x_66) | 0),
            }) ? ((arg_71 = toString(true), (arg_1_66 = toString(actual_66), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_71)(arg_1_66)("end set")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_66)("end set"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_92);
    }));
})(), (() => {
    const builder$0040_93 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set first point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_93, Test_TestCaseBuilder__Delay_1505(builder$0040_93, () => {
        let a_99, b_99, vx_45, vy_45, copyOfStruct_74, arg_72, arg_1_67;
        const pl_47 = Polyline2D__Clone(plOpen);
        Polyline2D__set_FirstPoint_6ADE94FD(pl_47, Pt_$ctor_7B00E9A0(77, 77));
        const actual_67 = ((a_99 = item(0, Polyline2D__get_Points(pl_47)), (b_99 = Pt_$ctor_7B00E9A0(77, 77), (vx_45 = (a_99.X - b_99.X), (vy_45 = (a_99.Y - b_99.Y), Math.sqrt((vx_45 * vx_45) + (vy_45 * vy_45))))))) < 1E-09;
        if ((actual_67 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_67, true, "first point set");
        }
        else {
            throw new Exception(contains((copyOfStruct_74 = actual_67, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_67) => (structuralHash(x_67) | 0),
            }) ? ((arg_72 = toString(true), (arg_1_67 = toString(actual_67), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_72)(arg_1_67)("first point set")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_67)("first point set"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_93);
    }));
})(), (() => {
    const builder$0040_94 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("set last point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_94, Test_TestCaseBuilder__Delay_1505(builder$0040_94, () => {
        let a_101, b_101, vx_46, vy_46, copyOfStruct_75, arg_73, arg_1_68;
        const pl_48 = Polyline2D__Clone(plOpen);
        Polyline2D__set_LastPoint_6ADE94FD(pl_48, Pt_$ctor_7B00E9A0(66, 66));
        const actual_68 = ((a_101 = item(Polyline2D__get_PointCount(pl_48) - 1, Polyline2D__get_Points(pl_48)), (b_101 = Pt_$ctor_7B00E9A0(66, 66), (vx_46 = (a_101.X - b_101.X), (vy_46 = (a_101.Y - b_101.Y), Math.sqrt((vx_46 * vx_46) + (vy_46 * vy_46))))))) < 1E-09;
        if ((actual_68 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_68, true, "last point set");
        }
        else {
            throw new Exception(contains((copyOfStruct_75 = actual_68, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_68) => (structuralHash(x_68) | 0),
            }) ? ((arg_73 = toString(true), (arg_1_68 = toString(actual_68), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_73)(arg_1_68)("last point set")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_68)("last point set"));
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
        let copyOfStruct_76, arg_74, arg_1_69;
        const actual_69 = Polyline2D__get_PointCount(Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, plSinglePoint)) | 0;
        if ((actual_69 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_69, 1, "single point unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_76 = actual_69, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_69) => (structuralHash(x_69) | 0),
            }) ? ((arg_74 = int32ToString(1), (arg_1_69 = int32ToString(actual_69), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_74)(arg_1_69)("single point unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_69)("single point unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_97);
    }));
})(), (() => {
    const builder$0040_98 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("empty returns same", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_98, Test_TestCaseBuilder__Delay_1505(builder$0040_98, () => {
        let copyOfStruct_77, arg_75, arg_1_70;
        const actual_70 = Polyline2D__get_PointCount(Polyline2D_removeColinearAndDuplicatePoints(0.9999984769132877, 1E-09, plEmpty)) | 0;
        if ((actual_70 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_70, 0, "empty unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_77 = actual_70, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_70) => (structuralHash(x_70) | 0),
            }) ? ((arg_75 = int32ToString(0), (arg_1_70 = int32ToString(actual_70), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_75)(arg_1_70)("empty unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_70)("empty unchanged"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_98);
    }));
})(), (() => {
    const builder$0040_99 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tolerance too tight throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_99, Test_TestCaseBuilder__Delay_1505(builder$0040_99, () => {
        Expect_throws(() => {
            Polyline2D_removeColinearAndDuplicatePoints(0.9999999999, 1E-09, plOpen);
        }, "tolerance tighter than 0.01 degrees should throw");
        Test_TestCaseBuilder__Zero(builder$0040_99);
    }));
})()])), Test_testList("removeDuplicatePoints edge cases", ofArray([(() => {
    const builder$0040_100 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("no duplicates returns same count", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_100, Test_TestCaseBuilder__Delay_1505(builder$0040_100, () => {
        let copyOfStruct_78, arg_76, arg_1_71;
        const actual_71 = Polyline2D__get_PointCount(Polyline2D_removeDuplicatePoints(1E-09, plOpen)) | 0;
        const expected_118 = Polyline2D__get_PointCount(plOpen) | 0;
        if ((actual_71 === expected_118) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_71, expected_118, "no duplicates, same count");
        }
        else {
            throw new Exception(contains((copyOfStruct_78 = actual_71, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_72) => (structuralHash(x_72) | 0),
            }) ? ((arg_76 = int32ToString(expected_118), (arg_1_71 = int32ToString(actual_71), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_76)(arg_1_71)("no duplicates, same count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_118)(actual_71)("no duplicates, same count"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_100);
    }));
})(), (() => {
    const builder$0040_101 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("all same points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_101, Test_TestCaseBuilder__Delay_1505(builder$0040_101, () => {
        let points_29, copyOfStruct_79, arg_77, arg_1_72;
        const actual_72 = Polyline2D__get_PointCount(Polyline2D_removeDuplicatePoints(1E-09, (points_29 = [Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 1)], Polyline2D_$ctor_Z5FD8CF3C(Array.from(points_29))))) | 0;
        if ((actual_72 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_72, 1, "all same reduces to 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_79 = actual_72, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_73) => (structuralHash(x_73) | 0),
            }) ? ((arg_77 = int32ToString(1), (arg_1_72 = int32ToString(actual_72), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_77)(arg_1_72)("all same reduces to 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_72)("all same reduces to 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_101);
    }));
})(), (() => {
    const builder$0040_102 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_102, Test_TestCaseBuilder__Delay_1505(builder$0040_102, () => {
        let copyOfStruct_80, arg_78, arg_1_73;
        const actual_73 = Polyline2D__get_PointCount(Polyline2D_removeDuplicatePoints(1E-09, plSinglePoint)) | 0;
        if ((actual_73 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_73, 1, "single point unchanged");
        }
        else {
            throw new Exception(contains((copyOfStruct_80 = actual_73, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_74) => (structuralHash(x_74) | 0),
            }) ? ((arg_78 = int32ToString(1), (arg_1_73 = int32ToString(actual_73), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_78)(arg_1_73)("single point unchanged")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_73)("single point unchanged"));
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
        let copyOfStruct_81, arg_79, arg_1_74;
        const actual_74 = Polyline2D__WindingNumber_6ADE94FD(plClosed, Pt_$ctor_7B00E9A0(1000, 1000)) | 0;
        if ((actual_74 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_74, 0, "far away point is outside");
        }
        else {
            throw new Exception(contains((copyOfStruct_81 = actual_74, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_75) => (structuralHash(x_75) | 0),
            }) ? ((arg_79 = int32ToString(0), (arg_1_74 = int32ToString(actual_74), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_79)(arg_1_74)("far away point is outside")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_74)("far away point is outside"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_104);
    }));
})(), (() => {
    const builder$0040_105 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("single point polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_105, Test_TestCaseBuilder__Delay_1505(builder$0040_105, () => {
        let copyOfStruct_82, arg_80, arg_1_75;
        const actual_75 = Polyline2D__WindingNumber_6ADE94FD(plSinglePoint, Pt_$ctor_7B00E9A0(1, 1)) | 0;
        if ((actual_75 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_75, 0, "single point winding is 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_82 = actual_75, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_76) => (structuralHash(x_76) | 0),
            }) ? ((arg_80 = int32ToString(0), (arg_1_75 = int32ToString(actual_75), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_80)(arg_1_75)("single point winding is 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_75)("single point winding is 0"));
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
            let a_103, b_103, vx_47, vy_47, copyOfStruct_83, arg_81, arg_1_76;
            const i = _arg | 0;
            const actual_76 = ((a_103 = item(i, Polyline2D__get_Points(moved)), (b_103 = item(i, Polyline2D__get_Points(translated)), (vx_47 = (a_103.X - b_103.X), (vy_47 = (a_103.Y - b_103.Y), Math.sqrt((vx_47 * vx_47) + (vy_47 * vy_47))))))) < 1E-09;
            const msg_124 = `point ${i}`;
            if ((actual_76 === true) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
                assertEqual(actual_76, true, msg_124);
            }
            else {
                throw new Exception(contains((copyOfStruct_83 = actual_76, bool_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                    Equals: equals,
                    GetHashCode: (x_77) => (structuralHash(x_77) | 0),
                }) ? ((arg_81 = toString(true), (arg_1_76 = toString(actual_76), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_81)(arg_1_76)(msg_124)))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(true)(actual_76)(msg_124));
            }
            Test_TestCaseBuilder__Zero(builder$0040_106);
        });
    }));
})()))]));

