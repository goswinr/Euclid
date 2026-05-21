
import { AccuracyModule_medium, Expect_floatClose, Expect_throws, Expect_isTrue, Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, AccuracyModule_veryHigh } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Points2D_getSignedArea_Z7CD03502, Points2D_mostDistantPointIdx_6A06E040, Points2D_minDistBetweenPointSets_6A06E040, Points2D_closestPointsIdx_6A06E040, Points2D_closestOfTwo, Points2D_closestPoint_Z7C0841BD, Points2D_closestPointIdx_Z7C0841BD } from "./Src/Points2D.js";
import { Exception, int32ToString, structuralHash, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Line2D_$ctor_Z53905FA0 } from "./Src/Line2D.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Points3D_mostDistantPoint_Z1881DE00, Points3D_mostDistantPointIdx_Z1881DE00, Points3D_minDistBetweenPointSets_Z1881DE00, Points3D_closestPointsIdx_Z1881DE00, Points3D_closestOfTwo, Points3D_closestPoint_Z371982BD, Points3D_closestPointIdx_Z371982BD } from "./Src/Points3D.js";
import { Line3D_$ctor_5A6659A0 } from "./Src/Line3D.js";

export const tol = AccuracyModule_veryHigh;

export const tests = Test_testList("Points2D and Points3D", ofArray([Test_testList("Points2D", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx finds closest point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const pts = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)];
        const target = Pt_$ctor_7B00E9A0(0.9, 0.1);
        const idx = Points2D_closestPointIdx_Z7C0841BD(pts, target) | 0;
        const actual = idx | 0;
        if ((actual === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 1, "closest point is at index 1");
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
                const arg = int32ToString(1);
                const arg_1 = int32ToString(actual);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("closest point is at index 1");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual)("closest point is at index 1");
            }
            throw new Exception(errorMsg);
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPoint returns correct point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_2, b_2, vx, vy;
        const pts_1 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)];
        const target_1 = Pt_$ctor_7B00E9A0(0.9, 0.1);
        const closest = Points2D_closestPoint_Z7C0841BD(pts_1, target_1);
        Expect_isTrue(((a_2 = closest, (b_2 = Pt_$ctor_7B00E9A0(1, 0), (vx = (a_2.X - b_2.X), (vy = (a_2.Y - b_2.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("closest point is (1,0)");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx on empty list throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const pts_2 = [];
        const target_2 = Pt_$ctor_7B00E9A0(0, 0);
        const f = () => {
            Points2D_closestPointIdx_Z7C0841BD(pts_2, target_2);
        };
        Expect_throws(f, "throws on empty list");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestOfTwo returns closer point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_4, b_4, vx_1, vy_1;
        const pt1 = Pt_$ctor_7B00E9A0(0, 0);
        const pt2 = Pt_$ctor_7B00E9A0(10, 0);
        const ref = Pt_$ctor_7B00E9A0(1, 0);
        const closest_1 = Points2D_closestOfTwo(pt1, pt2, ref);
        Expect_isTrue(((a_4 = closest_1, (b_4 = pt1, (vx_1 = (a_4.X - b_4.X), (vy_1 = (a_4.Y - b_4.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("closer point is pt1");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestOfTwo with equal distances returns first", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_6, b_6, vx_2, vy_2;
        const pt1_1 = Pt_$ctor_7B00E9A0(1, 0);
        const pt2_1 = Pt_$ctor_7B00E9A0(-1, 0);
        const ref_1 = Pt_$ctor_7B00E9A0(0, 0);
        const closest_2 = Points2D_closestOfTwo(pt1_1, pt2_1, ref_1);
        Expect_isTrue(((a_6 = closest_2, (b_6 = pt1_1, (vx_2 = (a_6.X - b_6.X), (vy_2 = (a_6.Y - b_6.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09)("equal distance returns first");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx finds closest pair", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const xs = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)];
        const ys = [Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0.1, 0.1)];
        const patternInput = Points2D_closestPointsIdx_6A06E040(xs, ys);
        const yi = patternInput[1] | 0;
        const xi = patternInput[0] | 0;
        const actual_1 = xi | 0;
        if ((actual_1 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 0, "x index is 0");
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
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("x index is 0");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_1)("x index is 0");
            }
            throw new Exception(errorMsg_1);
        }
        const actual_2 = yi | 0;
        if ((actual_2 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 1, "y index is 1");
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
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("y index is 1");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_2)("y index is 1");
            }
            throw new Exception(errorMsg_2);
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("minDistBetweenPointSets calculates correct distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const xs_1 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)];
        const ys_1 = [Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(0, 2)];
        const dist = Points2D_minDistBetweenPointSets_6A06E040(xs_1, ys_1);
        Expect_floatClose(tol, dist, 1, "min distance is 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPointIdx finds most lonely point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const findFrom = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0)];
        const checkAgainst = [Pt_$ctor_7B00E9A0(0, 1)];
        const patternInput_1 = Points2D_mostDistantPointIdx_6A06E040(findFrom, checkAgainst);
        const fromIdx = patternInput_1[0] | 0;
        const _againstIdx = patternInput_1[1] | 0;
        const actual_3 = fromIdx | 0;
        if ((actual_3 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 1, "most distant is at index 1");
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
                const arg_8 = int32ToString(1);
                const arg_1_3 = int32ToString(actual_3);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("most distant is at index 1");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_3)("most distant is at index 1");
            }
            throw new Exception(errorMsg_3);
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSignedArea for CCW square", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const pts_3 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(0, 0)];
        const area = Points2D_getSignedArea_Z7CD03502(pts_3);
        Expect_isTrue(area > 0)("CCW square has positive area");
        Expect_floatClose(AccuracyModule_medium, Math.abs(area), 1, "area is approximately 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSignedArea for CW square", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const pts_4 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, 0)];
        const area_1 = Points2D_getSignedArea_Z7CD03502(pts_4);
        Expect_isTrue(area_1 < 0)("CW square has negative area");
        Expect_floatClose(AccuracyModule_medium, Math.abs(area_1), 1, "area magnitude is approximately 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSignedArea for triangle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const pts_5 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(0, 0)];
        const area_2 = Points2D_getSignedArea_Z7CD03502(pts_5);
        Expect_floatClose(AccuracyModule_medium, Math.abs(area_2), 1, "triangle area is approximately 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx with single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const pts_6 = [Pt_$ctor_7B00E9A0(5, 5)];
        const target_3 = Pt_$ctor_7B00E9A0(10, 10);
        const idx_1 = Points2D_closestPointIdx_Z7C0841BD(pts_6, target_3) | 0;
        const actual_7 = idx_1 | 0;
        if ((actual_7 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 0, "single point returns index 0");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_7;
            valueType_4 = int32_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_9 = int32ToString(0);
                const arg_1_4 = int32ToString(actual_7);
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("single point returns index 0");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_7)("single point returns index 0");
            }
            throw new Exception(errorMsg_4);
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx on empty xs throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const xs_2 = [];
        const ys_2 = [Pt_$ctor_7B00E9A0(0, 0)];
        const f_1 = () => {
            Points2D_closestPointsIdx_6A06E040(xs_2, ys_2);
        };
        Expect_throws(f_1, "throws on empty xs");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx on empty ys throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const xs_3 = [Pt_$ctor_7B00E9A0(0, 0)];
        const ys_3 = [];
        const f_2 = () => {
            Points2D_closestPointsIdx_6A06E040(xs_3, ys_3);
        };
        Expect_throws(f_2, "throws on empty ys");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("dist to line is 3.0", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const pt_4 = Pt_$ctor_7B00E9A0(1, 1);
        const lineStart = Pt_$ctor_7B00E9A0(4, 0);
        const lineEnd = Pt_$ctor_7B00E9A0(4, 9);
        const ln = Line2D_$ctor_Z53905FA0(lineStart, lineEnd);
        let dist_1;
        let value_3;
        const testPt_1 = pt_4;
        const ln_2 = ln;
        const pAx = ln_2.FromX;
        const pAy = ln_2.FromY;
        let vAx;
        const ln_3 = ln_2;
        vAx = (ln_3.ToX - ln_3.FromX);
        let vAy;
        const ln_4 = ln_2;
        vAy = (ln_4.ToY - ln_4.FromY);
        const x_5 = testPt_1.X;
        const y_5 = testPt_1.Y;
        let t;
        const vAx_1 = vAx;
        const vAy_1 = vAy;
        const u = x_5 - pAx;
        const v = y_5 - pAy;
        const dotV = (vAx_1 * u) + (vAy_1 * v);
        const lenSq = (vAx_1 * vAx_1) + (vAy_1 * vAy_1);
        t = (dotV / lenSq);
        if (t > -1E-06) {
            if (t < 1.000001) {
                const clPtX = pAx + (vAx * t);
                const clPtY = pAy + (vAy * t);
                const vx_3 = clPtX - x_5;
                const vy_3 = clPtY - y_5;
                value_3 = ((vx_3 * vx_3) + (vy_3 * vy_3));
            }
            else {
                const clPtX_1 = pAx + vAx;
                const clPtY_1 = pAy + vAy;
                const vx_1_1 = clPtX_1 - x_5;
                const vy_1_1 = clPtY_1 - y_5;
                value_3 = ((vx_1_1 * vx_1_1) + (vy_1_1 * vy_1_1));
            }
        }
        else {
            const vX = pAx - x_5;
            const vY = pAy - y_5;
            value_3 = ((vX * vX) + (vY * vY));
        }
        dist_1 = Math.sqrt(value_3);
        Expect_floatClose(tol, dist_1, 3, "distance is 3.0");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})()])), Test_testList("Points3D", ofArray([(() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx finds closest point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const pts_7 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0)];
        const target_4 = Pnt_$ctor_Z7AD9E565(0.9, 0.1, 0.1);
        const idx_2 = Points3D_closestPointIdx_Z371982BD(pts_7, target_4) | 0;
        const actual_8 = idx_2 | 0;
        if ((actual_8 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 1, "closest point is at index 1");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_8;
            valueType_5 = int32_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            })) {
                const arg_10 = int32ToString(1);
                const arg_1_5 = int32ToString(actual_8);
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("closest point is at index 1");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_8)("closest point is at index 1");
            }
            throw new Exception(errorMsg_5);
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPoint returns correct point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_8, b_8, x_8, y_8, z;
        const pts_8 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0)];
        const target_5 = Pnt_$ctor_Z7AD9E565(0.9, 0.1, 0.1);
        const closest_3 = Points3D_closestPoint_Z371982BD(pts_8, target_5);
        Expect_isTrue(((a_8 = closest_3, (b_8 = Pnt_$ctor_Z7AD9E565(1, 0, 0), (x_8 = (a_8.X - b_8.X), (y_8 = (a_8.Y - b_8.Y), (z = (a_8.Z - b_8.Z), Math.sqrt(((x_8 * x_8) + (y_8 * y_8)) + (z * z)))))))) < 1E-09)("closest point is (1,0,0)");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx on empty list throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const pts_9 = [];
        const target_6 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const f_3 = () => {
            Points3D_closestPointIdx_Z371982BD(pts_9, target_6);
        };
        Expect_throws(f_3, "throws on empty list");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestOfTwo returns closer point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let a_10, b_10, x_9, y_9, z_1;
        const pt1_2 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pt2_2 = Pnt_$ctor_Z7AD9E565(10, 0, 0);
        const ref_2 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const closest_4 = Points3D_closestOfTwo(pt1_2, pt2_2, ref_2);
        Expect_isTrue(((a_10 = closest_4, (b_10 = pt1_2, (x_9 = (a_10.X - b_10.X), (y_9 = (a_10.Y - b_10.Y), (z_1 = (a_10.Z - b_10.Z), Math.sqrt(((x_9 * x_9) + (y_9 * y_9)) + (z_1 * z_1)))))))) < 1E-09)("closer point is pt1");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestOfTwo in 3D space", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_12, b_12, x_10, y_10, z_2;
        const pt1_3 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        const pt2_3 = Pnt_$ctor_Z7AD9E565(-1, -1, -1);
        const ref_3 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const closest_5 = Points3D_closestOfTwo(pt1_3, pt2_3, ref_3);
        Expect_isTrue(((a_12 = closest_5, (b_12 = pt1_3, (x_10 = (a_12.X - b_12.X), (y_10 = (a_12.Y - b_12.Y), (z_2 = (a_12.Z - b_12.Z), Math.sqrt(((x_10 * x_10) + (y_10 * y_10)) + (z_2 * z_2)))))))) < 1E-09)("equal distance returns first");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx finds closest pair", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const xs_4 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)];
        const ys_4 = [Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0.1, 0.1, 0.1)];
        const patternInput_2 = Points3D_closestPointsIdx_Z1881DE00(xs_4, ys_4);
        const yi_1 = patternInput_2[1] | 0;
        const xi_1 = patternInput_2[0] | 0;
        const actual_9 = xi_1 | 0;
        if ((actual_9 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 0, "x index is 0");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_9;
            valueType_6 = int32_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            })) {
                const arg_11 = int32ToString(0);
                const arg_1_6 = int32ToString(actual_9);
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("x index is 0");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_9)("x index is 0");
            }
            throw new Exception(errorMsg_6);
        }
        const actual_10 = yi_1 | 0;
        if ((actual_10 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 1, "y index is 1");
        }
        else {
            let valueType_7;
            let copyOfStruct_7 = actual_10;
            valueType_7 = int32_type;
            const primitiveTypes_7 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_7;
            if (contains(valueType_7, primitiveTypes_7, {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            })) {
                const arg_12 = int32ToString(1);
                const arg_1_7 = int32ToString(actual_10);
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("y index is 1");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_10)("y index is 1");
            }
            throw new Exception(errorMsg_7);
        }
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("minDistBetweenPointSets calculates correct distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const xs_5 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)];
        const ys_5 = [Pnt_$ctor_Z7AD9E565(0, 0, 1), Pnt_$ctor_Z7AD9E565(0, 0, 2)];
        const dist_2 = Points3D_minDistBetweenPointSets_Z1881DE00(xs_5, ys_5);
        Expect_floatClose(tol, dist_2, 1, "min distance is 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("minDistBetweenPointSets with diagonal distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const xs_6 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        const ys_6 = [Pnt_$ctor_Z7AD9E565(1, 1, 1)];
        const dist_3 = Points3D_minDistBetweenPointSets_Z1881DE00(xs_6, ys_6);
        Expect_floatClose(tol, dist_3, Math.sqrt(3), "distance is sqrt(3)");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPointIdx finds most lonely point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const findFrom_1 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)];
        const checkAgainst_1 = [Pnt_$ctor_Z7AD9E565(0, 0, 1)];
        const patternInput_3 = Points3D_mostDistantPointIdx_Z1881DE00(findFrom_1, checkAgainst_1);
        const fromIdx_1 = patternInput_3[0] | 0;
        const _againstIdx_1 = patternInput_3[1] | 0;
        const actual_11 = fromIdx_1 | 0;
        if ((actual_11 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 1, "most distant is at index 1");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_11;
            valueType_8 = int32_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            })) {
                const arg_13 = int32ToString(1);
                const arg_1_8 = int32ToString(actual_11);
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("most distant is at index 1");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_11)("most distant is at index 1");
            }
            throw new Exception(errorMsg_8);
        }
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPoint returns correct point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let a_14, b_14, x_14, y_14, z_3;
        const findFrom_2 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 10)];
        const checkAgainst_2 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        const pt_9 = Points3D_mostDistantPoint_Z1881DE00(findFrom_2, checkAgainst_2);
        Expect_isTrue(((a_14 = pt_9, (b_14 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (x_14 = (a_14.X - b_14.X), (y_14 = (a_14.Y - b_14.Y), (z_3 = (a_14.Z - b_14.Z), Math.sqrt(((x_14 * x_14) + (y_14 * y_14)) + (z_3 * z_3)))))))) < 1E-09)("most distant point is (10,10,10)");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx with single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        const pts_10 = [Pnt_$ctor_Z7AD9E565(5, 5, 5)];
        const target_7 = Pnt_$ctor_Z7AD9E565(10, 10, 10);
        const idx_3 = Points3D_closestPointIdx_Z371982BD(pts_10, target_7) | 0;
        const actual_12 = idx_3 | 0;
        if ((actual_12 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 0, "single point returns index 0");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_12;
            valueType_9 = int32_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            })) {
                const arg_14 = int32ToString(0);
                const arg_1_9 = int32ToString(actual_12);
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("single point returns index 0");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_12)("single point returns index 0");
            }
            throw new Exception(errorMsg_9);
        }
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx on empty xs throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        const xs_7 = [];
        const ys_7 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        const f_4 = () => {
            Points3D_closestPointsIdx_Z1881DE00(xs_7, ys_7);
        };
        Expect_throws(f_4, "throws on empty xs");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx on empty ys throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        const xs_8 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        const ys_8 = [];
        const f_5 = () => {
            Points3D_closestPointsIdx_Z1881DE00(xs_8, ys_8);
        };
        Expect_throws(f_5, "throws on empty ys");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPointIdx on empty findFrom throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const findFrom_3 = [];
        const checkAgainst_3 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        const f_6 = () => {
            Points3D_mostDistantPointIdx_Z1881DE00(findFrom_3, checkAgainst_3);
        };
        Expect_throws(f_6, "throws on empty findFrom");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPointIdx on empty checkAgainst throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const findFrom_4 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        const checkAgainst_4 = [];
        const f_7 = () => {
            Points3D_mostDistantPointIdx_Z1881DE00(findFrom_4, checkAgainst_4);
        };
        Expect_throws(f_7, "throws on empty checkAgainst");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx with identical points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        const pts_11 = [Pnt_$ctor_Z7AD9E565(1, 1, 1), Pnt_$ctor_Z7AD9E565(1, 1, 1), Pnt_$ctor_Z7AD9E565(1, 1, 1)];
        const target_8 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        const idx_4 = Points3D_closestPointIdx_Z371982BD(pts_11, target_8) | 0;
        const actual_13 = idx_4 | 0;
        if ((actual_13 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 0, "returns first matching point");
        }
        else {
            let valueType_10;
            let copyOfStruct_10 = actual_13;
            valueType_10 = int32_type;
            const primitiveTypes_10 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_10;
            if (contains(valueType_10, primitiveTypes_10, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_15 = int32ToString(0);
                const arg_1_10 = int32ToString(actual_13);
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("returns first matching point");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_13)("returns first matching point");
            }
            throw new Exception(errorMsg_10);
        }
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("dist to line is 3.0", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        const pt_10 = Pnt_$ctor_Z7AD9E565(1, 1, 7);
        const lineStart_1 = Pnt_$ctor_Z7AD9E565(4, 0, 7);
        const lineEnd_1 = Pnt_$ctor_Z7AD9E565(4, 9, 7);
        const ln_5 = Line3D_$ctor_5A6659A0(lineStart_1, lineEnd_1);
        let dist_4;
        let value_9;
        const testPt_3 = pt_10;
        const ln_7 = ln_5;
        const pAx_2 = ln_7.FromX;
        const pAy_2 = ln_7.FromY;
        const pAz = ln_7.FromZ;
        let vAx_2;
        const ln_8 = ln_7;
        vAx_2 = (ln_8.ToX - ln_8.FromX);
        let vAy_2;
        const ln_9 = ln_7;
        vAy_2 = (ln_9.ToY - ln_9.FromY);
        let vAz;
        const ln_10 = ln_7;
        vAz = (ln_10.ToZ - ln_10.FromZ);
        const x_17 = testPt_3.X;
        const y_17 = testPt_3.Y;
        const z_4 = testPt_3.Z;
        let t_1;
        let x_19;
        const vAx_3 = vAx_2;
        const vAy_3 = vAy_2;
        const vAz_1 = vAz;
        const u_1 = x_17 - pAx_2;
        const v_1 = y_17 - pAy_2;
        const w = z_4 - pAz;
        const dotV_1 = ((vAx_3 * u_1) + (vAy_3 * v_1)) + (vAz_1 * w);
        const lenSq_1 = ((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_1 * vAz_1);
        x_19 = (dotV_1 / lenSq_1);
        t_1 = ((x_19 > 0) ? ((x_19 < 1) ? x_19 : 1) : 0);
        if (t_1 > -1E-06) {
            if (t_1 < 1.000001) {
                const clPtX_2 = pAx_2 + (vAx_2 * t_1);
                const clPtY_2 = pAy_2 + (vAy_2 * t_1);
                const clPtZ = pAz + (vAz * t_1);
                const vx_4 = clPtX_2 - x_17;
                const vy_4 = clPtY_2 - y_17;
                const vz = clPtZ - z_4;
                value_9 = (((vx_4 * vx_4) + (vy_4 * vy_4)) + (vz * vz));
            }
            else {
                const vx_1_2 = (pAx_2 + vAx_2) - x_17;
                const vy_1_2 = (pAy_2 + vAy_2) - y_17;
                const vz_1 = (pAz + vAz) - z_4;
                value_9 = (((vx_1_2 * vx_1_2) + (vy_1_2 * vy_1_2)) + (vz_1 * vz_1));
            }
        }
        else {
            const vx_2_1 = pAx_2 - x_17;
            const vy_2_1 = pAy_2 - y_17;
            const vz_2 = pAz - z_4;
            value_9 = (((vx_2_1 * vx_2_1) + (vy_2_1 * vy_2_1)) + (vz_2 * vz_2));
        }
        dist_4 = Math.sqrt(value_9);
        Expect_floatClose(tol, dist_4, 3, "distance is 3.0");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})()]))]));

