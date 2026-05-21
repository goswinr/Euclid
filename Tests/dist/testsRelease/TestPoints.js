
import { AccuracyModule_medium, Expect_floatClose, Expect_throws, Expect_isTrue, Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, AccuracyModule_veryHigh } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Points2D_getSignedArea_Z7CD03502, Points2D_mostDistantPointIdx_6A06E040, Points2D_minDistBetweenPointSets_6A06E040, Points2D_closestPointsIdx_6A06E040, Points2D_closestOfTwo, Points2D_closestPoint_Z7C0841BD, Points2D_closestPointIdx_Z7C0841BD } from "./Src/Points2D.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { int32ToString, structuralHash, Exception, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { ofArray, contains } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Line2D_$ctor_Z53905FA0 } from "./Src/Line2D.js";
import { Points3D_mostDistantPoint_Z1881DE00, Points3D_mostDistantPointIdx_Z1881DE00, Points3D_minDistBetweenPointSets_Z1881DE00, Points3D_closestPointsIdx_Z1881DE00, Points3D_closestOfTwo, Points3D_closestPoint_Z371982BD, Points3D_closestPointIdx_Z371982BD } from "./Src/Points3D.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Line3D_$ctor_5A6659A0 } from "./Src/Line3D.js";

export const tol = AccuracyModule_veryHigh;

export const tests = Test_testList("Points2D and Points3D", ofArray([Test_testList("Points2D", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx finds closest point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let copyOfStruct, arg, arg_1;
        const actual = Points2D_closestPointIdx_Z7C0841BD([Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)], Pt_$ctor_7B00E9A0(0.9, 0.1)) | 0;
        if ((actual === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 1, "closest point is at index 1");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            }) ? ((arg = int32ToString(1), (arg_1 = int32ToString(actual), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("closest point is at index 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual)("closest point is at index 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPoint returns correct point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_2, b_2, vx, vy;
        Expect_isTrue(((a_2 = Points2D_closestPoint_Z7C0841BD([Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)], Pt_$ctor_7B00E9A0(0.9, 0.1)), (b_2 = Pt_$ctor_7B00E9A0(1, 0), (vx = (a_2.X - b_2.X), (vy = (a_2.Y - b_2.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("closest point is (1,0)");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx on empty list throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const pts_2 = [];
        const target_2 = Pt_$ctor_7B00E9A0(0, 0);
        Expect_throws(() => {
            Points2D_closestPointIdx_Z7C0841BD(pts_2, target_2);
        }, "throws on empty list");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestOfTwo returns closer point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_4, b_4, vx_1, vy_1;
        const pt1 = Pt_$ctor_7B00E9A0(0, 0);
        Expect_isTrue(((a_4 = Points2D_closestOfTwo(pt1, Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(1, 0)), (b_4 = pt1, (vx_1 = (a_4.X - b_4.X), (vy_1 = (a_4.Y - b_4.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("closer point is pt1");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestOfTwo with equal distances returns first", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_6, b_6, vx_2, vy_2;
        const pt1_1 = Pt_$ctor_7B00E9A0(1, 0);
        Expect_isTrue(((a_6 = Points2D_closestOfTwo(pt1_1, Pt_$ctor_7B00E9A0(-1, 0), Pt_$ctor_7B00E9A0(0, 0)), (b_6 = pt1_1, (vx_2 = (a_6.X - b_6.X), (vy_2 = (a_6.Y - b_6.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09)("equal distance returns first");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx finds closest pair", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let copyOfStruct_1, arg_6, arg_1_1, copyOfStruct_2, arg_7, arg_1_2;
        const patternInput = Points2D_closestPointsIdx_6A06E040([Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)], [Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(0.1, 0.1)]);
        const actual_1 = patternInput[0] | 0;
        if ((actual_1 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 0, "x index is 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_1, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            }) ? ((arg_6 = int32ToString(0), (arg_1_1 = int32ToString(actual_1), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("x index is 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_1)("x index is 0"));
        }
        const actual_2 = patternInput[1] | 0;
        if ((actual_2 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 1, "y index is 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_2, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            }) ? ((arg_7 = int32ToString(1), (arg_1_2 = int32ToString(actual_2), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("y index is 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_2)("y index is 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("minDistBetweenPointSets calculates correct distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        Expect_floatClose(tol, Points2D_minDistBetweenPointSets_6A06E040([Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)], [Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(0, 2)]), 1, "min distance is 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPointIdx finds most lonely point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let copyOfStruct_3, arg_8, arg_1_3;
        const actual_3 = Points2D_mostDistantPointIdx_6A06E040([Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0)], [Pt_$ctor_7B00E9A0(0, 1)])[0] | 0;
        if ((actual_3 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 1, "most distant is at index 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_3, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            }) ? ((arg_8 = int32ToString(1), (arg_1_3 = int32ToString(actual_3), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("most distant is at index 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_3)("most distant is at index 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSignedArea for CCW square", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const area = Points2D_getSignedArea_Z7CD03502([Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(0, 0)]);
        Expect_isTrue(area > 0)("CCW square has positive area");
        Expect_floatClose(AccuracyModule_medium, Math.abs(area), 1, "area is approximately 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSignedArea for CW square", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const area_1 = Points2D_getSignedArea_Z7CD03502([Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, 0)]);
        Expect_isTrue(area_1 < 0)("CW square has negative area");
        Expect_floatClose(AccuracyModule_medium, Math.abs(area_1), 1, "area magnitude is approximately 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSignedArea for triangle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const area_2 = Points2D_getSignedArea_Z7CD03502([Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(0, 0)]);
        Expect_floatClose(AccuracyModule_medium, Math.abs(area_2), 1, "triangle area is approximately 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx with single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let copyOfStruct_4, arg_9, arg_1_4;
        const actual_7 = Points2D_closestPointIdx_Z7C0841BD([Pt_$ctor_7B00E9A0(5, 5)], Pt_$ctor_7B00E9A0(10, 10)) | 0;
        if ((actual_7 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 0, "single point returns index 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_7, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            }) ? ((arg_9 = int32ToString(0), (arg_1_4 = int32ToString(actual_7), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("single point returns index 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_7)("single point returns index 0"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx on empty xs throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const xs_2 = [];
        const ys_2 = [Pt_$ctor_7B00E9A0(0, 0)];
        Expect_throws(() => {
            Points2D_closestPointsIdx_6A06E040(xs_2, ys_2);
        }, "throws on empty xs");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx on empty ys throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const xs_3 = [Pt_$ctor_7B00E9A0(0, 0)];
        const ys_3 = [];
        Expect_throws(() => {
            Points2D_closestPointsIdx_6A06E040(xs_3, ys_3);
        }, "throws on empty ys");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("dist to line is 3.0", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let value_3, testPt_1, ln_2, pAx, pAy, vAx, ln_3, vAy, ln_4, x_5, y_5, t, vAx_1, vAy_1, vx_3, vy_3, vx_1_1, vy_1_1, vX, vY;
        Expect_floatClose(tol, (value_3 = ((testPt_1 = Pt_$ctor_7B00E9A0(1, 1), (ln_2 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(4, 0), Pt_$ctor_7B00E9A0(4, 9)), (pAx = ln_2.FromX, (pAy = ln_2.FromY, (vAx = ((ln_3 = ln_2, ln_3.ToX - ln_3.FromX)), (vAy = ((ln_4 = ln_2, ln_4.ToY - ln_4.FromY)), (x_5 = testPt_1.X, (y_5 = testPt_1.Y, (t = ((vAx_1 = vAx, (vAy_1 = vAy, ((vAx_1 * (x_5 - pAx)) + (vAy_1 * (y_5 - pAy))) / ((vAx_1 * vAx_1) + (vAy_1 * vAy_1))))), (t > -1E-06) ? ((t < 1.000001) ? ((vx_3 = ((pAx + (vAx * t)) - x_5), (vy_3 = ((pAy + (vAy * t)) - y_5), (vx_3 * vx_3) + (vy_3 * vy_3)))) : ((vx_1_1 = ((pAx + vAx) - x_5), (vy_1_1 = ((pAy + vAy) - y_5), (vx_1_1 * vx_1_1) + (vy_1_1 * vy_1_1))))) : ((vX = (pAx - x_5), (vY = (pAy - y_5), (vX * vX) + (vY * vY)))))))))))))), Math.sqrt(value_3)), 3, "distance is 3.0");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})()])), Test_testList("Points3D", ofArray([(() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx finds closest point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let copyOfStruct_5, arg_10, arg_1_5;
        const actual_8 = Points3D_closestPointIdx_Z371982BD([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0)], Pnt_$ctor_Z7AD9E565(0.9, 0.1, 0.1)) | 0;
        if ((actual_8 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_8, 1, "closest point is at index 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_8, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            }) ? ((arg_10 = int32ToString(1), (arg_1_5 = int32ToString(actual_8), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("closest point is at index 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_8)("closest point is at index 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPoint returns correct point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_8, b_8, x_8, y_8, z;
        Expect_isTrue(((a_8 = Points3D_closestPoint_Z371982BD([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0)], Pnt_$ctor_Z7AD9E565(0.9, 0.1, 0.1)), (b_8 = Pnt_$ctor_Z7AD9E565(1, 0, 0), (x_8 = (a_8.X - b_8.X), (y_8 = (a_8.Y - b_8.Y), (z = (a_8.Z - b_8.Z), Math.sqrt(((x_8 * x_8) + (y_8 * y_8)) + (z * z)))))))) < 1E-09)("closest point is (1,0,0)");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx on empty list throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const pts_9 = [];
        const target_6 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        Expect_throws(() => {
            Points3D_closestPointIdx_Z371982BD(pts_9, target_6);
        }, "throws on empty list");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestOfTwo returns closer point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let a_10, b_10, x_9, y_9, z_1;
        const pt1_2 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        Expect_isTrue(((a_10 = Points3D_closestOfTwo(pt1_2, Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), (b_10 = pt1_2, (x_9 = (a_10.X - b_10.X), (y_9 = (a_10.Y - b_10.Y), (z_1 = (a_10.Z - b_10.Z), Math.sqrt(((x_9 * x_9) + (y_9 * y_9)) + (z_1 * z_1)))))))) < 1E-09)("closer point is pt1");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestOfTwo in 3D space", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_12, b_12, x_10, y_10, z_2;
        const pt1_3 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        Expect_isTrue(((a_12 = Points3D_closestOfTwo(pt1_3, Pnt_$ctor_Z7AD9E565(-1, -1, -1), Pnt_$ctor_Z7AD9E565(0, 0, 0)), (b_12 = pt1_3, (x_10 = (a_12.X - b_12.X), (y_10 = (a_12.Y - b_12.Y), (z_2 = (a_12.Z - b_12.Z), Math.sqrt(((x_10 * x_10) + (y_10 * y_10)) + (z_2 * z_2)))))))) < 1E-09)("equal distance returns first");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx finds closest pair", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let copyOfStruct_6, arg_11, arg_1_6, copyOfStruct_7, arg_12, arg_1_7;
        const patternInput_2 = Points3D_closestPointsIdx_Z1881DE00([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)], [Pnt_$ctor_Z7AD9E565(10, 10, 10), Pnt_$ctor_Z7AD9E565(0.1, 0.1, 0.1)]);
        const actual_9 = patternInput_2[0] | 0;
        if ((actual_9 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 0, "x index is 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_6 = actual_9, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_11) => (structuralHash(x_11) | 0),
            }) ? ((arg_11 = int32ToString(0), (arg_1_6 = int32ToString(actual_9), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("x index is 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_9)("x index is 0"));
        }
        const actual_10 = patternInput_2[1] | 0;
        if ((actual_10 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_10, 1, "y index is 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_7 = actual_10, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            }) ? ((arg_12 = int32ToString(1), (arg_1_7 = int32ToString(actual_10), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("y index is 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_10)("y index is 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("minDistBetweenPointSets calculates correct distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        Expect_floatClose(tol, Points3D_minDistBetweenPointSets_Z1881DE00([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)], [Pnt_$ctor_Z7AD9E565(0, 0, 1), Pnt_$ctor_Z7AD9E565(0, 0, 2)]), 1, "min distance is 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("minDistBetweenPointSets with diagonal distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        Expect_floatClose(tol, Points3D_minDistBetweenPointSets_Z1881DE00([Pnt_$ctor_Z7AD9E565(0, 0, 0)], [Pnt_$ctor_Z7AD9E565(1, 1, 1)]), Math.sqrt(3), "distance is sqrt(3)");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPointIdx finds most lonely point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let copyOfStruct_8, arg_13, arg_1_8;
        const actual_11 = Points3D_mostDistantPointIdx_Z1881DE00([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)], [Pnt_$ctor_Z7AD9E565(0, 0, 1)])[0] | 0;
        if ((actual_11 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 1, "most distant is at index 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_8 = actual_11, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_13) => (structuralHash(x_13) | 0),
            }) ? ((arg_13 = int32ToString(1), (arg_1_8 = int32ToString(actual_11), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("most distant is at index 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_11)("most distant is at index 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPoint returns correct point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let a_14, b_14, x_14, y_14, z_3;
        Expect_isTrue(((a_14 = Points3D_mostDistantPoint_Z1881DE00([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 10)], [Pnt_$ctor_Z7AD9E565(0, 0, 0)]), (b_14 = Pnt_$ctor_Z7AD9E565(10, 10, 10), (x_14 = (a_14.X - b_14.X), (y_14 = (a_14.Y - b_14.Y), (z_3 = (a_14.Z - b_14.Z), Math.sqrt(((x_14 * x_14) + (y_14 * y_14)) + (z_3 * z_3)))))))) < 1E-09)("most distant point is (10,10,10)");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx with single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let copyOfStruct_9, arg_14, arg_1_9;
        const actual_12 = Points3D_closestPointIdx_Z371982BD([Pnt_$ctor_Z7AD9E565(5, 5, 5)], Pnt_$ctor_Z7AD9E565(10, 10, 10)) | 0;
        if ((actual_12 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_12, 0, "single point returns index 0");
        }
        else {
            throw new Exception(contains((copyOfStruct_9 = actual_12, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            }) ? ((arg_14 = int32ToString(0), (arg_1_9 = int32ToString(actual_12), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("single point returns index 0")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_12)("single point returns index 0"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx on empty xs throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        const xs_7 = [];
        const ys_7 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        Expect_throws(() => {
            Points3D_closestPointsIdx_Z1881DE00(xs_7, ys_7);
        }, "throws on empty xs");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointsIdx on empty ys throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        const xs_8 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        const ys_8 = [];
        Expect_throws(() => {
            Points3D_closestPointsIdx_Z1881DE00(xs_8, ys_8);
        }, "throws on empty ys");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPointIdx on empty findFrom throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const findFrom_3 = [];
        const checkAgainst_3 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        Expect_throws(() => {
            Points3D_mostDistantPointIdx_Z1881DE00(findFrom_3, checkAgainst_3);
        }, "throws on empty findFrom");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("mostDistantPointIdx on empty checkAgainst throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const findFrom_4 = [Pnt_$ctor_Z7AD9E565(0, 0, 0)];
        const checkAgainst_4 = [];
        Expect_throws(() => {
            Points3D_mostDistantPointIdx_Z1881DE00(findFrom_4, checkAgainst_4);
        }, "throws on empty checkAgainst");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPointIdx with identical points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let copyOfStruct_10, arg_15, arg_1_10;
        const actual_13 = Points3D_closestPointIdx_Z371982BD([Pnt_$ctor_Z7AD9E565(1, 1, 1), Pnt_$ctor_Z7AD9E565(1, 1, 1), Pnt_$ctor_Z7AD9E565(1, 1, 1)], Pnt_$ctor_Z7AD9E565(1, 1, 1)) | 0;
        if ((actual_13 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 0, "returns first matching point");
        }
        else {
            throw new Exception(contains((copyOfStruct_10 = actual_13, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            }) ? ((arg_15 = int32ToString(0), (arg_1_10 = int32ToString(actual_13), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("returns first matching point")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_13)("returns first matching point"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("dist to line is 3.0", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let value_9, testPt_3, ln_7, pAx_2, pAy_2, pAz, vAx_2, ln_8, vAy_2, ln_9, vAz, ln_10, x_17, y_17, z_4, t_1, x_19, vAx_3, vAy_3, vAz_1, vx_4, vy_4, vz, vx_1_2, vy_1_2, vz_1, vx_2_1, vy_2_1, vz_2;
        Expect_floatClose(tol, (value_9 = ((testPt_3 = Pnt_$ctor_Z7AD9E565(1, 1, 7), (ln_7 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(4, 0, 7), Pnt_$ctor_Z7AD9E565(4, 9, 7)), (pAx_2 = ln_7.FromX, (pAy_2 = ln_7.FromY, (pAz = ln_7.FromZ, (vAx_2 = ((ln_8 = ln_7, ln_8.ToX - ln_8.FromX)), (vAy_2 = ((ln_9 = ln_7, ln_9.ToY - ln_9.FromY)), (vAz = ((ln_10 = ln_7, ln_10.ToZ - ln_10.FromZ)), (x_17 = testPt_3.X, (y_17 = testPt_3.Y, (z_4 = testPt_3.Z, (t_1 = ((x_19 = ((vAx_3 = vAx_2, (vAy_3 = vAy_2, (vAz_1 = vAz, (((vAx_3 * (x_17 - pAx_2)) + (vAy_3 * (y_17 - pAy_2))) + (vAz_1 * (z_4 - pAz))) / (((vAx_3 * vAx_3) + (vAy_3 * vAy_3)) + (vAz_1 * vAz_1)))))), (x_19 > 0) ? ((x_19 < 1) ? x_19 : 1) : 0)), (t_1 > -1E-06) ? ((t_1 < 1.000001) ? ((vx_4 = ((pAx_2 + (vAx_2 * t_1)) - x_17), (vy_4 = ((pAy_2 + (vAy_2 * t_1)) - y_17), (vz = ((pAz + (vAz * t_1)) - z_4), ((vx_4 * vx_4) + (vy_4 * vy_4)) + (vz * vz))))) : ((vx_1_2 = ((pAx_2 + vAx_2) - x_17), (vy_1_2 = ((pAy_2 + vAy_2) - y_17), (vz_1 = ((pAz + vAz) - z_4), ((vx_1_2 * vx_1_2) + (vy_1_2 * vy_1_2)) + (vz_1 * vz_1)))))) : ((vx_2_1 = (pAx_2 - x_17), (vy_2_1 = (pAy_2 - y_17), (vz_2 = (pAz - z_4), ((vx_2_1 * vx_2_1) + (vy_2_1 * vy_2_1)) + (vz_2 * vz_2)))))))))))))))))), Math.sqrt(value_9)), 3, "distance is 3.0");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})()]))]));

