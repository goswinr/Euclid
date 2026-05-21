
import { AccuracyModule_low, AccuracyModule_veryHigh, Expect_isNone, Test_failtest, Expect_isFalse, AccuracyModule_medium, Expect_isTrue, Test_TestCaseBuilder__Zero, AccuracyModule_high, Expect_floatClose, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { isInfinity } from "./fable_modules/fable-library-js.5.0.0/Double.js";
import { Line2D_$ctor_77D16AC0, Line2D_$ctor_Z53905FA0 } from "./Src/Line2D.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Src/Pt.js";
import { XLine2D_getSqDistance_Z6A6F0C80, XLine2D_getClosestPoints_40607834, XLine2D_getClosestParameters_40607834, XLine2D_getIntersection_40607834, XLine2D_getIntersectionParam_40607834, XLine2D_getRayIntersection_40607834, XLine2D_getRayIntersectionParam_40607834, XLine2D_doOverlap_199764BB } from "./Src/XLine2D.js";
import { toFail, printf, toText, interpolate } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_2 } from "./Src/Pt.js";
import { XLine2D_getSqDistance_Z6A6F0C80 as XLine2D_getSqDistance_Z6A6F0C80_1, XLine2D_getClosestPoints_40607834 as XLine2D_getClosestPoints_40607834_1, XLine2D_getEndsTouching_Z44565CE5 } from "./Src/XLine2D.js";
import { Line2D_$ctor_77D16AC0 as Line2D_$ctor_77D16AC0_1 } from "./Src/Line2D.js";

export const tests = Test_testList("XLine2D Tests", ofArray([Test_testList("parameterANaN tests", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines - basic case", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const pA = Pt_$ctor_7B00E9A0(0, 0);
        const pB = Pt_$ctor_7B00E9A0(1, 0);
        const vA = Vc_$ctor_7B00E9A0(1, 0);
        const vB = Vc_$ctor_7B00E9A0(0, 1);
        let t;
        const pA_1 = pA;
        const pB_1 = pB;
        const vA_1 = vA;
        const vB_1 = vB;
        const vBx = vB_1.X;
        const vBy = vB_1.Y;
        const det = (vA_1.X * vBy) - (vA_1.Y * vBx);
        const dx = pB_1.X - pA_1.X;
        const dy = pB_1.Y - pA_1.Y;
        t = (((dx * vBy) - (dy * vBx)) / det);
        Expect_floatClose(AccuracyModule_high, t, 1, "Parameter should be 1.0");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines - negative parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const pA_2 = Pt_$ctor_7B00E9A0(5, 0);
        const pB_2 = Pt_$ctor_7B00E9A0(0, 5);
        const vA_2 = Vc_$ctor_7B00E9A0(1, 0);
        const vB_2 = Vc_$ctor_7B00E9A0(0, -1);
        let t_1;
        const pA_3 = pA_2;
        const pB_3 = pB_2;
        const vA_3 = vA_2;
        const vB_3 = vB_2;
        const vBx_1 = vB_3.X;
        const vBy_1 = vB_3.Y;
        const det_1 = (vA_3.X * vBy_1) - (vA_3.Y * vBx_1);
        const dx_1 = pB_3.X - pA_3.X;
        const dy_1 = pB_3.Y - pA_3.Y;
        t_1 = (((dx_1 * vBy_1) - (dy_1 * vBx_1)) / det_1);
        Expect_floatClose(AccuracyModule_high, t_1, -5, "Parameter should be -5.0");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines - returns NaN or Infinity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const pA_4 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_4 = Pt_$ctor_7B00E9A0(0, 1);
        const vA_4 = Vc_$ctor_7B00E9A0(1, 0);
        const vB_4 = Vc_$ctor_7B00E9A0(1, 0);
        let t_2;
        const pA_5 = pA_4;
        const pB_5 = pB_4;
        const vA_5 = vA_4;
        const vB_5 = vB_4;
        const vBx_2 = vB_5.X;
        const vBy_2 = vB_5.Y;
        const det_2 = (vA_5.X * vBy_2) - (vA_5.Y * vBx_2);
        const dx_2 = pB_5.X - pA_5.X;
        const dy_2 = pB_5.Y - pA_5.Y;
        t_2 = (((dx_2 * vBy_2) - (dy_2 * vBx_2)) / det_2);
        Expect_isTrue(Number.isNaN(t_2) ? true : isInfinity(t_2))("Should return NaN or Infinity for parallel lines");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Coincident lines - same start and direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const pA_6 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_6 = Pt_$ctor_7B00E9A0(0, 0);
        const vA_6 = Vc_$ctor_7B00E9A0(1, 1);
        const vB_6 = Vc_$ctor_7B00E9A0(1, 1);
        let t_3;
        const pA_7 = pA_6;
        const pB_7 = pB_6;
        const vA_7 = vA_6;
        const vB_7 = vB_6;
        const vBx_3 = vB_7.X;
        const vBy_3 = vB_7.Y;
        const det_3 = (vA_7.X * vBy_3) - (vA_7.Y * vBx_3);
        const dx_3 = pB_7.X - pA_7.X;
        const dy_3 = pB_7.Y - pA_7.Y;
        t_3 = (((dx_3 * vBy_3) - (dy_3 * vBx_3)) / det_3);
        Expect_isTrue(Number.isNaN(t_3))("Should return NaN for coincident lines");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Nearly parallel lines - returns large value", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const pA_8 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_8 = Pt_$ctor_7B00E9A0(0, 1);
        const vA_8 = Vc_$ctor_7B00E9A0(1, 0);
        const vB_8 = Vc_$ctor_7B00E9A0(1, 0.001);
        let t_4;
        const pA_9 = pA_8;
        const pB_9 = pB_8;
        const vA_9 = vA_8;
        const vB_9 = vB_8;
        const vBx_4 = vB_9.X;
        const vBy_4 = vB_9.Y;
        const det_4 = (vA_9.X * vBy_4) - (vA_9.Y * vBx_4);
        const dx_4 = pB_9.X - pA_9.X;
        const dy_4 = pB_9.Y - pA_9.Y;
        t_4 = (((dx_4 * vBy_4) - (dy_4 * vBx_4)) / det_4);
        Expect_isTrue(isInfinity(t_4) ? true : (Math.abs(t_4) > 100))("Should return Infinity or very large value");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Line2D overload", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let ln, ln_1;
        const lineA = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let t_5;
        const lineA_1 = lineA;
        const lineB_1 = lineB;
        let vBx_5;
        const ln_2 = lineB_1;
        vBx_5 = (ln_2.ToX - ln_2.FromX);
        let vBy_5;
        const ln_3 = lineB_1;
        vBy_5 = (ln_3.ToY - ln_3.FromY);
        const det_5 = (((ln = lineA_1, ln.ToX - ln.FromX)) * vBy_5) - (((ln_1 = lineA_1, ln_1.ToY - ln_1.FromY)) * vBx_5);
        const dx_5 = lineB_1.FromX - lineA_1.FromX;
        const dy_5 = lineB_1.FromY - lineA_1.FromY;
        t_5 = (((dx_5 * vBy_5) - (dy_5 * vBx_5)) / det_5);
        Expect_floatClose(AccuracyModule_high, t_5, 0.5, "Parameter should be 0.5");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})()])), Test_testList("parameters tests", ofArray([(() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines - basic cross", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const pA_10 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_10 = Pt_$ctor_7B00E9A0(0, 0);
        const vA_10 = Vc_$ctor_7B00E9A0(1, 0);
        const vB_10 = Vc_$ctor_7B00E9A0(0, 1);
        let patternInput;
        const pA_11 = pA_10;
        const pB_11 = pB_10;
        const vA_11 = vA_10;
        const vB_11 = vB_10;
        const vAx_6 = vA_11.X;
        const vAy_6 = vA_11.Y;
        const vBx_6 = vB_11.X;
        const vBy_6 = vB_11.Y;
        const det_6 = (vAx_6 * vBy_6) - (vAy_6 * vBx_6);
        const dx_6 = pB_11.X - pA_11.X;
        const dy_6 = pB_11.Y - pA_11.Y;
        const t_6 = ((dx_6 * vBy_6) - (dy_6 * vBx_6)) / det_6;
        const u = ((dx_6 * vAy_6) - (dy_6 * vAx_6)) / det_6;
        patternInput = [t_6, u];
        const u_1 = patternInput[1];
        const t_7 = patternInput[0];
        Expect_floatClose(AccuracyModule_high, t_7, 0, "Parameter t should be 0.0");
        Expect_floatClose(AccuracyModule_high, u_1, 0, "Parameter u should be 0.0");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines - offset cross", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let pA_13, pB_13, vA_13, vB_13, vAx_7, vAy_7, vBx_7, vBy_7, det_7, dx_7, dy_7, t_8, u_2;
        const pA_12 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_12 = Pt_$ctor_7B00E9A0(2, 0);
        const vA_12 = Vc_$ctor_7B00E9A0(0, 1);
        const vB_12 = Vc_$ctor_7B00E9A0(0, 1);
        const t_9 = ((pA_13 = pA_12, (pB_13 = pB_12, (vA_13 = vA_12, (vB_13 = vB_12, (vAx_7 = vA_13.X, (vAy_7 = vA_13.Y, (vBx_7 = vB_13.X, (vBy_7 = vB_13.Y, (det_7 = ((vAx_7 * vBy_7) - (vAy_7 * vBx_7)), (dx_7 = (pB_13.X - pA_13.X), (dy_7 = (pB_13.Y - pA_13.Y), (t_8 = (((dx_7 * vBy_7) - (dy_7 * vBx_7)) / det_7), (u_2 = (((dx_7 * vAy_7) - (dy_7 * vAx_7)) / det_7), [t_8, u_2]))))))))))))))[0];
        Expect_isTrue(Number.isNaN(t_9) ? true : isInfinity(t_9))("Parallel lines should give NaN or Infinity");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Diagonal intersection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const pA_14 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_14 = Pt_$ctor_7B00E9A0(1, 0);
        const vA_14 = Vc_$ctor_7B00E9A0(1, 1);
        const vB_14 = Vc_$ctor_7B00E9A0(1, -1);
        let patternInput_2;
        const pA_15 = pA_14;
        const pB_15 = pB_14;
        const vA_15 = vA_14;
        const vB_15 = vB_14;
        const vAx_8 = vA_15.X;
        const vAy_8 = vA_15.Y;
        const vBx_8 = vB_15.X;
        const vBy_8 = vB_15.Y;
        const det_8 = (vAx_8 * vBy_8) - (vAy_8 * vBx_8);
        const dx_8 = pB_15.X - pA_15.X;
        const dy_8 = pB_15.Y - pA_15.Y;
        const t_10 = ((dx_8 * vBy_8) - (dy_8 * vBx_8)) / det_8;
        const u_3 = ((dx_8 * vAy_8) - (dy_8 * vAx_8)) / det_8;
        patternInput_2 = [t_10, u_3];
        const u_4 = patternInput_2[1];
        const t_11 = patternInput_2[0];
        Expect_floatClose(AccuracyModule_medium, t_11, 0.5, "Parameter t should be 0.5");
        Expect_floatClose(AccuracyModule_medium, Math.abs(u_4), 0.5, "Parameter u absolute value should be 0.5");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})()])), Test_testList("isWithinRanges tests", ofArray([(() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection within both ranges", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const lineA_2 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_2 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result;
        const lineA_3 = lineA_2;
        const lineB_3 = lineB_2;
        let vAx_9;
        const ln_4 = lineA_3;
        vAx_9 = (ln_4.ToX - ln_4.FromX);
        let vAy_9;
        const ln_5 = lineA_3;
        vAy_9 = (ln_5.ToY - ln_5.FromY);
        let vBx_9;
        const ln_6 = lineB_3;
        vBx_9 = (ln_6.ToX - ln_6.FromX);
        let vBy_9;
        const ln_7 = lineB_3;
        vBy_9 = (ln_7.ToY - ln_7.FromY);
        const det_9 = (vAx_9 * vBy_9) - (vAy_9 * vBx_9);
        const dx_9 = lineB_3.FromX - lineA_3.FromX;
        const dy_9 = lineB_3.FromY - lineA_3.FromY;
        const t_12 = ((dx_9 * vBy_9) - (dy_9 * vBx_9)) / det_9;
        if ((t_12 >= 0) && (t_12 <= 1)) {
            const u_5 = ((dx_9 * vAy_9) - (dy_9 * vAx_9)) / det_9;
            result = ((u_5 >= 0) && (u_5 <= 1));
        }
        else {
            result = false;
        }
        Expect_isTrue(result)("Lines should intersect within ranges");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection outside range A", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const lineA_4 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_4 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_1;
        const lineA_5 = lineA_4;
        const lineB_5 = lineB_4;
        let vAx_10;
        const ln_8 = lineA_5;
        vAx_10 = (ln_8.ToX - ln_8.FromX);
        let vAy_10;
        const ln_9 = lineA_5;
        vAy_10 = (ln_9.ToY - ln_9.FromY);
        let vBx_10;
        const ln_10 = lineB_5;
        vBx_10 = (ln_10.ToX - ln_10.FromX);
        let vBy_10;
        const ln_11 = lineB_5;
        vBy_10 = (ln_11.ToY - ln_11.FromY);
        const det_10 = (vAx_10 * vBy_10) - (vAy_10 * vBx_10);
        const dx_10 = lineB_5.FromX - lineA_5.FromX;
        const dy_10 = lineB_5.FromY - lineA_5.FromY;
        const t_13 = ((dx_10 * vBy_10) - (dy_10 * vBx_10)) / det_10;
        if ((t_13 >= 0.6) && (t_13 <= 1)) {
            const u_6 = ((dx_10 * vAy_10) - (dy_10 * vAx_10)) / det_10;
            result_1 = ((u_6 >= 0) && (u_6 <= 1));
        }
        else {
            result_1 = false;
        }
        Expect_isFalse(result_1)("Intersection is outside range A");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection outside range B", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const lineA_6 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_6 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_2;
        const lineA_7 = lineA_6;
        const lineB_7 = lineB_6;
        let vAx_11;
        const ln_12 = lineA_7;
        vAx_11 = (ln_12.ToX - ln_12.FromX);
        let vAy_11;
        const ln_13 = lineA_7;
        vAy_11 = (ln_13.ToY - ln_13.FromY);
        let vBx_11;
        const ln_14 = lineB_7;
        vBx_11 = (ln_14.ToX - ln_14.FromX);
        let vBy_11;
        const ln_15 = lineB_7;
        vBy_11 = (ln_15.ToY - ln_15.FromY);
        const det_11 = (vAx_11 * vBy_11) - (vAy_11 * vBx_11);
        const dx_11 = lineB_7.FromX - lineA_7.FromX;
        const dy_11 = lineB_7.FromY - lineA_7.FromY;
        const t_14 = ((dx_11 * vBy_11) - (dy_11 * vBx_11)) / det_11;
        if ((t_14 >= 0) && (t_14 <= 1)) {
            const u_7 = ((dx_11 * vAy_11) - (dy_11 * vAx_11)) / det_11;
            result_2 = ((u_7 >= 0.6) && (u_7 <= 1));
        }
        else {
            result_2 = false;
        }
        Expect_isFalse(result_2)("Intersection is outside range B");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const lineA_8 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_8 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(10, 1));
        let result_3;
        const lineA_9 = lineA_8;
        const lineB_9 = lineB_8;
        let vAx_12;
        const ln_16 = lineA_9;
        vAx_12 = (ln_16.ToX - ln_16.FromX);
        let vAy_12;
        const ln_17 = lineA_9;
        vAy_12 = (ln_17.ToY - ln_17.FromY);
        let vBx_12;
        const ln_18 = lineB_9;
        vBx_12 = (ln_18.ToX - ln_18.FromX);
        let vBy_12;
        const ln_19 = lineB_9;
        vBy_12 = (ln_19.ToY - ln_19.FromY);
        const det_12 = (vAx_12 * vBy_12) - (vAy_12 * vBx_12);
        const dx_12 = lineB_9.FromX - lineA_9.FromX;
        const dy_12 = lineB_9.FromY - lineA_9.FromY;
        const t_15 = ((dx_12 * vBy_12) - (dy_12 * vBx_12)) / det_12;
        if ((t_15 >= 0) && (t_15 <= 1)) {
            const u_8 = ((dx_12 * vAy_12) - (dy_12 * vAx_12)) / det_12;
            result_3 = ((u_8 >= 0) && (u_8 <= 1));
        }
        else {
            result_3 = false;
        }
        Expect_isFalse(result_3)("Parallel lines should return false");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Custom parameter ranges", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const lineA_10 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_10 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_4;
        const lineA_11 = lineA_10;
        const lineB_11 = lineB_10;
        let vAx_13;
        const ln_20 = lineA_11;
        vAx_13 = (ln_20.ToX - ln_20.FromX);
        let vAy_13;
        const ln_21 = lineA_11;
        vAy_13 = (ln_21.ToY - ln_21.FromY);
        let vBx_13;
        const ln_22 = lineB_11;
        vBx_13 = (ln_22.ToX - ln_22.FromX);
        let vBy_13;
        const ln_23 = lineB_11;
        vBy_13 = (ln_23.ToY - ln_23.FromY);
        const det_13 = (vAx_13 * vBy_13) - (vAy_13 * vBx_13);
        const dx_13 = lineB_11.FromX - lineA_11.FromX;
        const dy_13 = lineB_11.FromY - lineA_11.FromY;
        const t_16 = ((dx_13 * vBy_13) - (dy_13 * vBx_13)) / det_13;
        if ((t_16 >= 0.4) && (t_16 <= 0.6)) {
            const u_9 = ((dx_13 * vAy_13) - (dy_13 * vAx_13)) / det_13;
            result_4 = ((u_9 >= 0.4) && (u_9 <= 0.6));
        }
        else {
            result_4 = false;
        }
        Expect_isTrue(result_4)("Intersection at 0.5, 0.5 should be within ranges");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})()])), Test_testList("tryIntersectInRangeA tests", ofArray([(() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection within range", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const lineA_12 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_12 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_5;
        const lineA_13 = lineA_12;
        const lineB_13 = lineB_12;
        const pAx_14 = lineA_13.FromX;
        const pAy_14 = lineA_13.FromY;
        let vAx_14;
        const ln_24 = lineA_13;
        vAx_14 = (ln_24.ToX - ln_24.FromX);
        let vAy_14;
        const ln_25 = lineA_13;
        vAy_14 = (ln_25.ToY - ln_25.FromY);
        let vBx_14;
        const ln_26 = lineB_13;
        vBx_14 = (ln_26.ToX - ln_26.FromX);
        let vBy_14;
        const ln_27 = lineB_13;
        vBy_14 = (ln_27.ToY - ln_27.FromY);
        const det_14 = (vAx_14 * vBy_14) - (vAy_14 * vBx_14);
        const dx_14 = lineB_13.FromX - pAx_14;
        const dy_14 = lineB_13.FromY - pAy_14;
        const t_17 = ((dx_14 * vBy_14) - (dy_14 * vBx_14)) / det_14;
        result_5 = (((t_17 >= 0) && (t_17 <= 1)) ? Pt_$ctor_7B00E9A0_1(pAx_14 + (t_17 * vAx_14), pAy_14 + (t_17 * vAy_14)) : undefined);
        if (result_5 == null) {
            Test_failtest("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_14);
        }
        else {
            const pt = result_5;
            Expect_floatClose(AccuracyModule_high, pt.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt.Y, 0, "Y should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_14);
        }
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection outside range", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const lineA_14 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_14 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_6;
        const lineA_15 = lineA_14;
        const lineB_15 = lineB_14;
        const pAx_15 = lineA_15.FromX;
        const pAy_15 = lineA_15.FromY;
        let vAx_15;
        const ln_28 = lineA_15;
        vAx_15 = (ln_28.ToX - ln_28.FromX);
        let vAy_15;
        const ln_29 = lineA_15;
        vAy_15 = (ln_29.ToY - ln_29.FromY);
        let vBx_15;
        const ln_30 = lineB_15;
        vBx_15 = (ln_30.ToX - ln_30.FromX);
        let vBy_15;
        const ln_31 = lineB_15;
        vBy_15 = (ln_31.ToY - ln_31.FromY);
        const det_15 = (vAx_15 * vBy_15) - (vAy_15 * vBx_15);
        const dx_15 = lineB_15.FromX - pAx_15;
        const dy_15 = lineB_15.FromY - pAy_15;
        const t_18 = ((dx_15 * vBy_15) - (dy_15 * vBx_15)) / det_15;
        result_6 = (((t_18 >= 0.6) && (t_18 <= 1)) ? Pt_$ctor_7B00E9A0_1(pAx_15 + (t_18 * vAx_15), pAy_15 + (t_18 * vAy_15)) : undefined);
        Expect_isNone(result_6, "Should be None when outside range");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const lineA_16 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_16 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(10, 1));
        let result_7;
        const lineA_17 = lineA_16;
        const lineB_17 = lineB_16;
        const pAx_16 = lineA_17.FromX;
        const pAy_16 = lineA_17.FromY;
        let vAx_16;
        const ln_32 = lineA_17;
        vAx_16 = (ln_32.ToX - ln_32.FromX);
        let vAy_16;
        const ln_33 = lineA_17;
        vAy_16 = (ln_33.ToY - ln_33.FromY);
        let vBx_16;
        const ln_34 = lineB_17;
        vBx_16 = (ln_34.ToX - ln_34.FromX);
        let vBy_16;
        const ln_35 = lineB_17;
        vBy_16 = (ln_35.ToY - ln_35.FromY);
        const det_16 = (vAx_16 * vBy_16) - (vAy_16 * vBx_16);
        const dx_16 = lineB_17.FromX - pAx_16;
        const dy_16 = lineB_17.FromY - pAy_16;
        const t_19 = ((dx_16 * vBy_16) - (dy_16 * vBx_16)) / det_16;
        result_7 = (((t_19 >= 0) && (t_19 <= 1)) ? Pt_$ctor_7B00E9A0_1(pAx_16 + (t_19 * vAx_16), pAy_16 + (t_19 * vAy_16)) : undefined);
        Expect_isNone(result_7, "Parallel lines should return None");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})()])), Test_testList("tryIntersectInRanges tests", ofArray([(() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection within both ranges", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const lineA_18 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_18 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_8;
        const lineA_19 = lineA_18;
        const lineB_19 = lineB_18;
        const pAx_17 = lineA_19.FromX;
        const pAy_17 = lineA_19.FromY;
        let vAx_17;
        const ln_36 = lineA_19;
        vAx_17 = (ln_36.ToX - ln_36.FromX);
        let vAy_17;
        const ln_37 = lineA_19;
        vAy_17 = (ln_37.ToY - ln_37.FromY);
        let vBx_17;
        const ln_38 = lineB_19;
        vBx_17 = (ln_38.ToX - ln_38.FromX);
        let vBy_17;
        const ln_39 = lineB_19;
        vBy_17 = (ln_39.ToY - ln_39.FromY);
        const det_17 = (vAx_17 * vBy_17) - (vAy_17 * vBx_17);
        const dx_17 = lineB_19.FromX - pAx_17;
        const dy_17 = lineB_19.FromY - pAy_17;
        const t_20 = ((dx_17 * vBy_17) - (dy_17 * vBx_17)) / det_17;
        if ((t_20 >= 0) && (t_20 <= 1)) {
            const u_10 = ((dx_17 * vAy_17) - (dy_17 * vAx_17)) / det_17;
            result_8 = (((u_10 >= 0) && (u_10 <= 1)) ? Pt_$ctor_7B00E9A0_1(pAx_17 + (t_20 * vAx_17), pAy_17 + (t_20 * vAy_17)) : undefined);
        }
        else {
            result_8 = undefined;
        }
        if (result_8 == null) {
            Test_failtest("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
        else {
            const pt_1 = result_8;
            Expect_floatClose(AccuracyModule_high, pt_1.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_1.Y, 0, "Y should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersection outside range B", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const lineA_20 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_20 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_9;
        const lineA_21 = lineA_20;
        const lineB_21 = lineB_20;
        const pAx_18 = lineA_21.FromX;
        const pAy_18 = lineA_21.FromY;
        let vAx_18;
        const ln_40 = lineA_21;
        vAx_18 = (ln_40.ToX - ln_40.FromX);
        let vAy_18;
        const ln_41 = lineA_21;
        vAy_18 = (ln_41.ToY - ln_41.FromY);
        let vBx_18;
        const ln_42 = lineB_21;
        vBx_18 = (ln_42.ToX - ln_42.FromX);
        let vBy_18;
        const ln_43 = lineB_21;
        vBy_18 = (ln_43.ToY - ln_43.FromY);
        const det_18 = (vAx_18 * vBy_18) - (vAy_18 * vBx_18);
        const dx_18 = lineB_21.FromX - pAx_18;
        const dy_18 = lineB_21.FromY - pAy_18;
        const t_21 = ((dx_18 * vBy_18) - (dy_18 * vBx_18)) / det_18;
        if ((t_21 >= 0) && (t_21 <= 1)) {
            const u_11 = ((dx_18 * vAy_18) - (dy_18 * vAx_18)) / det_18;
            result_9 = (((u_11 >= 0.6) && (u_11 <= 1)) ? Pt_$ctor_7B00E9A0_1(pAx_18 + (t_21 * vAx_18), pAy_18 + (t_21 * vAy_18)) : undefined);
        }
        else {
            result_9 = undefined;
        }
        Expect_isNone(result_9, "Should be None when outside range B");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})()])), Test_testList("tryIntersect tests", ofArray([(() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Lines intersecting within segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const lineA_22 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_22 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_10;
        const lineA_23 = lineA_22;
        const lineB_23 = lineB_22;
        const pAx_20 = lineA_23.FromX;
        const pAy_20 = lineA_23.FromY;
        let vAx_20;
        const ln_44 = lineA_23;
        vAx_20 = (ln_44.ToX - ln_44.FromX);
        let vAy_20;
        const ln_45 = lineA_23;
        vAy_20 = (ln_45.ToY - ln_45.FromY);
        let vBx_20;
        const ln_46 = lineB_23;
        vBx_20 = (ln_46.ToX - ln_46.FromX);
        let vBy_20;
        const ln_47 = lineB_23;
        vBy_20 = (ln_47.ToY - ln_47.FromY);
        const det_19 = (vAx_20 * vBy_20) - (vAy_20 * vBx_20);
        const dx_19 = lineB_23.FromX - pAx_20;
        const dy_19 = lineB_23.FromY - pAy_20;
        const t_22 = ((dx_19 * vBy_20) - (dy_19 * vBx_20)) / det_19;
        if ((t_22 >= -1E-06) && (t_22 <= 1.000001)) {
            const u_12 = ((dx_19 * vAy_20) - (dy_19 * vAx_20)) / det_19;
            result_10 = (((u_12 >= -1E-06) && (u_12 <= 1.000001)) ? Pt_$ctor_7B00E9A0_1(pAx_20 + (t_22 * vAx_20), pAy_20 + (t_22 * vAy_20)) : undefined);
        }
        else {
            result_10 = undefined;
        }
        if (result_10 == null) {
            Test_failtest("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_19);
        }
        else {
            const pt_2 = result_10;
            Expect_floatClose(AccuracyModule_high, pt_2.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_2.Y, 0, "Y should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_19);
        }
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Lines not intersecting - parallel", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const lineA_24 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_24 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(10, 1));
        let result_11;
        const lineA_25 = lineA_24;
        const lineB_25 = lineB_24;
        const pAx_22 = lineA_25.FromX;
        const pAy_22 = lineA_25.FromY;
        let vAx_22;
        const ln_48 = lineA_25;
        vAx_22 = (ln_48.ToX - ln_48.FromX);
        let vAy_22;
        const ln_49 = lineA_25;
        vAy_22 = (ln_49.ToY - ln_49.FromY);
        let vBx_22;
        const ln_50 = lineB_25;
        vBx_22 = (ln_50.ToX - ln_50.FromX);
        let vBy_22;
        const ln_51 = lineB_25;
        vBy_22 = (ln_51.ToY - ln_51.FromY);
        const det_20 = (vAx_22 * vBy_22) - (vAy_22 * vBx_22);
        const dx_20 = lineB_25.FromX - pAx_22;
        const dy_20 = lineB_25.FromY - pAy_22;
        const t_23 = ((dx_20 * vBy_22) - (dy_20 * vBx_22)) / det_20;
        if ((t_23 >= -1E-06) && (t_23 <= 1.000001)) {
            const u_13 = ((dx_20 * vAy_22) - (dy_20 * vAx_22)) / det_20;
            result_11 = (((u_13 >= -1E-06) && (u_13 <= 1.000001)) ? Pt_$ctor_7B00E9A0_1(pAx_22 + (t_23 * vAx_22), pAy_22 + (t_23 * vAy_22)) : undefined);
        }
        else {
            result_11 = undefined;
        }
        Expect_isNone(result_11, "Parallel lines should return None");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Lines not intersecting - apart", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const lineA_26 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_26 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(2, -1), Pt_$ctor_7B00E9A0(2, 1));
        let result_12;
        const lineA_27 = lineA_26;
        const lineB_27 = lineB_26;
        const pAx_24 = lineA_27.FromX;
        const pAy_24 = lineA_27.FromY;
        let vAx_24;
        const ln_52 = lineA_27;
        vAx_24 = (ln_52.ToX - ln_52.FromX);
        let vAy_24;
        const ln_53 = lineA_27;
        vAy_24 = (ln_53.ToY - ln_53.FromY);
        let vBx_24;
        const ln_54 = lineB_27;
        vBx_24 = (ln_54.ToX - ln_54.FromX);
        let vBy_24;
        const ln_55 = lineB_27;
        vBy_24 = (ln_55.ToY - ln_55.FromY);
        const det_21 = (vAx_24 * vBy_24) - (vAy_24 * vBx_24);
        const dx_21 = lineB_27.FromX - pAx_24;
        const dy_21 = lineB_27.FromY - pAy_24;
        const t_24 = ((dx_21 * vBy_24) - (dy_21 * vBx_24)) / det_21;
        if ((t_24 >= -1E-06) && (t_24 <= 1.000001)) {
            const u_14 = ((dx_21 * vAy_24) - (dy_21 * vAx_24)) / det_21;
            result_12 = (((u_14 >= -1E-06) && (u_14 <= 1.000001)) ? Pt_$ctor_7B00E9A0_1(pAx_24 + (t_24 * vAx_24), pAy_24 + (t_24 * vAy_24)) : undefined);
        }
        else {
            result_12 = undefined;
        }
        Expect_isNone(result_12, "Apart lines should return None");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})()])), Test_testList("doOverlap tests", ofArray([(() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel overlapping lines on same ray", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let ln_56, ln_57, ln_58, ln_59;
        const lineA_28 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_28 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(15, 0));
        let result_13;
        const lineA_29 = lineA_28;
        const lineB_29 = lineB_28;
        result_13 = XLine2D_doOverlap_199764BB(lineA_29.FromX, lineA_29.FromY, lineB_29.FromX, lineB_29.FromY, (ln_56 = lineA_29, ln_56.ToX - ln_56.FromX), (ln_57 = lineA_29, ln_57.ToY - ln_57.FromY), (ln_58 = lineB_29, ln_58.ToX - ln_58.FromX), (ln_59 = lineB_29, ln_59.ToY - ln_59.FromY), 1E-06);
        Expect_isTrue(result_13)("Overlapping coincident lines should return true");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel non-overlapping lines on same ray", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let ln_60, ln_61, ln_62, ln_63;
        const lineA_30 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0));
        const lineB_30 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(15, 0));
        let result_14;
        const lineA_31 = lineA_30;
        const lineB_31 = lineB_30;
        result_14 = XLine2D_doOverlap_199764BB(lineA_31.FromX, lineA_31.FromY, lineB_31.FromX, lineB_31.FromY, (ln_60 = lineA_31, ln_60.ToX - ln_60.FromX), (ln_61 = lineA_31, ln_61.ToY - ln_61.FromY), (ln_62 = lineB_31, ln_62.ToX - ln_62.FromX), (ln_63 = lineB_31, ln_63.ToY - ln_63.FromY), 1E-06);
        Expect_isFalse(result_14)("Non-overlapping coincident lines should return false");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines touching at end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let ln_64, ln_65, ln_66, ln_67;
        const lineA_32 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0));
        const lineB_32 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(10, 0));
        let result_15;
        const lineA_33 = lineA_32;
        const lineB_33 = lineB_32;
        result_15 = XLine2D_doOverlap_199764BB(lineA_33.FromX, lineA_33.FromY, lineB_33.FromX, lineB_33.FromY, (ln_64 = lineA_33, ln_64.ToX - ln_64.FromX), (ln_65 = lineA_33, ln_65.ToY - ln_65.FromY), (ln_66 = lineB_33, ln_66.ToX - ln_66.FromX), (ln_67 = lineB_33, ln_67.ToY - ln_67.FromY), 1E-06);
        Expect_isTrue(result_15)("Touching lines should return true");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines on different rays", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let ln_68, ln_69, ln_70, ln_71;
        const lineA_34 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_34 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(10, 1));
        let result_16;
        const lineA_35 = lineA_34;
        const lineB_35 = lineB_34;
        result_16 = XLine2D_doOverlap_199764BB(lineA_35.FromX, lineA_35.FromY, lineB_35.FromX, lineB_35.FromY, (ln_68 = lineA_35, ln_68.ToX - ln_68.FromX), (ln_69 = lineA_35, ln_69.ToY - ln_69.FromY), (ln_70 = lineB_35, ln_70.ToX - ln_70.FromX), (ln_71 = lineB_35, ln_71.ToY - ln_71.FromY), 1E-06);
        Expect_isFalse(result_16)("Parallel non-coincident lines should return false");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines barely within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let ln_72, ln_73, ln_74, ln_75;
        const lineA_36 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_36 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, 1E-08), Pt_$ctor_7B00E9A0(15, 1E-08));
        let result_17;
        const lineA_37 = lineA_36;
        const lineB_37 = lineB_36;
        result_17 = XLine2D_doOverlap_199764BB(lineA_37.FromX, lineA_37.FromY, lineB_37.FromX, lineB_37.FromY, (ln_72 = lineA_37, ln_72.ToX - ln_72.FromX), (ln_73 = lineA_37, ln_73.ToY - ln_73.FromY), (ln_74 = lineB_37, ln_74.ToX - ln_74.FromX), (ln_75 = lineB_37, ln_75.ToY - ln_75.FromY), 1E-06);
        Expect_isTrue(result_17)("Lines within tolerance should overlap");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})()])), Test_testList("getRayIntersectionParam tests", ofArray([(() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rays intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        const pA_16 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_16 = Pt_$ctor_7B00E9A0(1, 0);
        const vA_16 = Vc_$ctor_7B00E9A0(1, 0);
        const vB_16 = Vc_$ctor_7B00E9A0(0, 1);
        let result_18;
        const pA_17 = pA_16;
        const pB_17 = pB_16;
        const vA_17 = vA_16;
        const vB_17 = vB_16;
        result_18 = XLine2D_getRayIntersectionParam_40607834(pA_17.X, pA_17.Y, pB_17.X, pB_17.Y, vA_17.X, vA_17.Y, vB_17.X, vB_17.Y, 0.004363350820701567, 1E-06);
        switch (result_18.tag) {
            case 0: {
                const u_15 = result_18.fields[1];
                const t_25 = result_18.fields[0];
                Expect_floatClose(AccuracyModule_high, t_25, 1, "Parameter t should be 1.0");
                Expect_floatClose(AccuracyModule_high, u_15, 0, "Parameter u should be 0.0");
                Test_TestCaseBuilder__Zero(builder$0040_27);
                break;
            }
            case 1: {
                Test_failtest("These lines are not parallel");
                Test_TestCaseBuilder__Zero(builder$0040_27);
                break;
            }
            default: {
                const other = result_18;
                Test_failtest(`Should intersect but got: ${interpolate("%A%P()", [other])}`);
                Test_TestCaseBuilder__Zero(builder$0040_27);
            }
        }
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rays nearly parallel within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const pA_18 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_18 = Pt_$ctor_7B00E9A0(0, 1);
        const vA_18 = Vc_$ctor_7B00E9A0(1, 0);
        const vB_18 = Vc_$ctor_7B00E9A0(1, 0.0001);
        let result_19;
        const pA_19 = pA_18;
        const pB_19 = pB_18;
        const vA_19 = vA_18;
        const vB_19 = vB_18;
        result_19 = XLine2D_getRayIntersectionParam_40607834(pA_19.X, pA_19.Y, pB_19.X, pB_19.Y, vA_19.X, vA_19.Y, vB_19.X, vB_19.Y, 0.004363350820701567, 1E-06);
        if (result_19.tag === 1) {
            Test_TestCaseBuilder__Zero(builder$0040_28);
        }
        else {
            const other_1 = result_19;
            Test_failtest(`Should be parallel within tolerance but got: ${interpolate("%A%P()", [other_1])}`);
            Test_TestCaseBuilder__Zero(builder$0040_28);
        }
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Ray A too short", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const pA_20 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_20 = Pt_$ctor_7B00E9A0(1, 0);
        const vA_20 = Vc_$ctor_7B00E9A0(1E-07, 0);
        const vB_20 = Vc_$ctor_7B00E9A0(0, 1);
        let result_20;
        const pA_21 = pA_20;
        const pB_21 = pB_20;
        const vA_21 = vA_20;
        const vB_21 = vB_20;
        result_20 = XLine2D_getRayIntersectionParam_40607834(pA_21.X, pA_21.Y, pB_21.X, pB_21.Y, vA_21.X, vA_21.Y, vB_21.X, vB_21.Y, 0.004363350820701567, 1E-06);
        if (result_20.tag === 2) {
            Test_TestCaseBuilder__Zero(builder$0040_29);
        }
        else {
            const other_2 = result_20;
            Test_failtest(`Should be TooShortA but got: ${interpolate("%A%P()", [other_2])}`);
            Test_TestCaseBuilder__Zero(builder$0040_29);
        }
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Both rays too short", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        const pA_22 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_22 = Pt_$ctor_7B00E9A0(1, 0);
        const vA_22 = Vc_$ctor_7B00E9A0(1E-07, 0);
        const vB_22 = Vc_$ctor_7B00E9A0(0, 1E-07);
        let result_21;
        const pA_23 = pA_22;
        const pB_23 = pB_22;
        const vA_23 = vA_22;
        const vB_23 = vB_22;
        result_21 = XLine2D_getRayIntersectionParam_40607834(pA_23.X, pA_23.Y, pB_23.X, pB_23.Y, vA_23.X, vA_23.Y, vB_23.X, vB_23.Y, 0.004363350820701567, 1E-06);
        if (result_21.tag === 4) {
            Test_TestCaseBuilder__Zero(builder$0040_30);
        }
        else {
            const other_3 = result_21;
            Test_failtest(`Should be TooShortBoth but got: ${interpolate("%A%P()", [other_3])}`);
            Test_TestCaseBuilder__Zero(builder$0040_30);
        }
    }));
})()])), Test_testList("getRayIntersection tests", ofArray([(() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rays intersecting at specific point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        const pA_24 = Pt_$ctor_7B00E9A0(0, 0);
        const pB_24 = Pt_$ctor_7B00E9A0(5, 0);
        const vA_24 = Vc_$ctor_7B00E9A0(1, 0);
        const vB_24 = Vc_$ctor_7B00E9A0(0, 1);
        let result_22;
        const pA_25 = pA_24;
        const pB_25 = pB_24;
        const vA_25 = vA_24;
        const vB_25 = vB_24;
        result_22 = XLine2D_getRayIntersection_40607834(pA_25.X, pA_25.Y, pB_25.X, pB_25.Y, vA_25.X, vA_25.Y, vB_25.X, vB_25.Y, 0.004363350820701567, 1E-06);
        if (result_22.tag === 0) {
            const pt_3 = result_22.fields[0];
            Expect_floatClose(AccuracyModule_high, pt_3.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_3.Y, 0, "Y should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_31);
        }
        else {
            const other_4 = result_22;
            Test_failtest(`Should intersect but got: ${interpolate("%A%P()", [other_4])}`);
            Test_TestCaseBuilder__Zero(builder$0040_31);
        }
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rays intersecting at origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let ln_76, ln_77, ln_78, ln_79;
        const lineA_38 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(-5, 0), Pt_$ctor_7B00E9A0(5, 0));
        const lineB_38 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, -5), Pt_$ctor_7B00E9A0(0, 5));
        let result_23;
        const lineA_39 = lineA_38;
        const lineB_39 = lineB_38;
        result_23 = XLine2D_getRayIntersection_40607834(lineA_39.FromX, lineA_39.FromY, lineB_39.FromX, lineB_39.FromY, (ln_76 = lineA_39, ln_76.ToX - ln_76.FromX), (ln_77 = lineA_39, ln_77.ToY - ln_77.FromY), (ln_78 = lineB_39, ln_78.ToX - ln_78.FromX), (ln_79 = lineB_39, ln_79.ToY - ln_79.FromY), 0.004363350820701567, 1E-06);
        if (result_23.tag === 0) {
            const pt_4 = result_23.fields[0];
            Expect_floatClose(AccuracyModule_high, pt_4.X, 0, "X should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_4.Y, 0, "Y should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_32);
        }
        else {
            const other_5 = result_23;
            Test_failtest(`Should intersect but got: ${interpolate("%A%P()", [other_5])}`);
            Test_TestCaseBuilder__Zero(builder$0040_32);
        }
    }));
})()])), Test_testList("getIntersectionParam tests", ofArray([(() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let ln_80, ln_81, ln_82, ln_83;
        const lineA_40 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_40 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_24;
        const lnA = lineA_40;
        const lnB = lineB_40;
        result_24 = XLine2D_getIntersectionParam_40607834(lnA.FromX, lnA.FromY, lnB.FromX, lnB.FromY, (ln_80 = lnA, ln_80.ToX - ln_80.FromX), (ln_81 = lnA, ln_81.ToY - ln_81.FromY), (ln_82 = lnB, ln_82.ToX - ln_82.FromX), (ln_83 = lnB, ln_83.ToY - ln_83.FromY), 0.004363350820701567, 1E-06);
        if (result_24.tag === 0) {
            const u_16 = result_24.fields[1];
            const t_26 = result_24.fields[0];
            Expect_floatClose(AccuracyModule_high, t_26, 0.5, "Parameter t should be 0.5");
            Expect_floatClose(AccuracyModule_high, u_16, 0.5, "Parameter u should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_33);
        }
        else {
            const other_6 = result_24;
            Test_failtest(`Should intersect but got: ${interpolate("%A%P()", [other_6])}`);
            Test_TestCaseBuilder__Zero(builder$0040_33);
        }
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines would intersect but outside segment (apart)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let ln_84, ln_85, ln_86, ln_87;
        const lineA_41 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_41 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(2, -1), Pt_$ctor_7B00E9A0(2, 1));
        let result_25;
        const lnA_1 = lineA_41;
        const lnB_1 = lineB_41;
        result_25 = XLine2D_getIntersectionParam_40607834(lnA_1.FromX, lnA_1.FromY, lnB_1.FromX, lnB_1.FromY, (ln_84 = lnA_1, ln_84.ToX - ln_84.FromX), (ln_85 = lnA_1, ln_85.ToY - ln_85.FromY), (ln_86 = lnB_1, ln_86.ToX - ln_86.FromX), (ln_87 = lnB_1, ln_87.ToY - ln_87.FromY), 0.004363350820701567, 1E-06);
        if (result_25.tag === 1) {
            Test_TestCaseBuilder__Zero(builder$0040_34);
        }
        else {
            const other_7 = result_25;
            Test_failtest(`Should be apart - lines would intersect if extended but got: ${interpolate("%A%P()", [other_7])}`);
            Test_TestCaseBuilder__Zero(builder$0040_34);
        }
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines nearly parallel within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        let ln_88, ln_89, ln_90, ln_91;
        const lineA_42 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_42 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(10, 1.0001));
        let result_26;
        const lnA_2 = lineA_42;
        const lnB_2 = lineB_42;
        result_26 = XLine2D_getIntersectionParam_40607834(lnA_2.FromX, lnA_2.FromY, lnB_2.FromX, lnB_2.FromY, (ln_88 = lnA_2, ln_88.ToX - ln_88.FromX), (ln_89 = lnA_2, ln_89.ToY - ln_89.FromY), (ln_90 = lnB_2, ln_90.ToX - ln_90.FromX), (ln_91 = lnB_2, ln_91.ToY - ln_91.FromY), 0.004363350820701567, 1E-06);
        if (result_26.tag === 2) {
            Test_TestCaseBuilder__Zero(builder$0040_35);
        }
        else {
            const other_8 = result_26;
            Test_failtest(`Should be parallel within tolerance but got: ${interpolate("%A%P()", [other_8])}`);
            Test_TestCaseBuilder__Zero(builder$0040_35);
        }
    }));
})()])), Test_testList("getIntersection tests", ofArray([(() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines intersecting - returns point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let ln_92, ln_93, ln_94, ln_95;
        const lineA_43 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_43 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_27;
        const lineA_44 = lineA_43;
        const lineB_44 = lineB_43;
        result_27 = XLine2D_getIntersection_40607834(lineA_44.FromX, lineA_44.FromY, lineB_44.FromX, lineB_44.FromY, (ln_92 = lineA_44, ln_92.ToX - ln_92.FromX), (ln_93 = lineA_44, ln_93.ToY - ln_93.FromY), (ln_94 = lineB_44, ln_94.ToX - ln_94.FromX), (ln_95 = lineB_44, ln_95.ToY - ln_95.FromY), 0.004363350820701567, 1E-06);
        if (result_27.tag === 0) {
            const pt_5 = result_27.fields[0];
            Expect_floatClose(AccuracyModule_high, pt_5.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_5.Y, 0, "Y should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_36);
        }
        else {
            const other_9 = result_27;
            Test_failtest(`Should intersect but got: ${interpolate("%A%P()", [other_9])}`);
            Test_TestCaseBuilder__Zero(builder$0040_36);
        }
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines would intersect but outside segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let ln_96, ln_97, ln_98, ln_99;
        const lineA_45 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_45 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(2, -1), Pt_$ctor_7B00E9A0(2, 1));
        let result_28;
        const lineA_46 = lineA_45;
        const lineB_46 = lineB_45;
        result_28 = XLine2D_getIntersection_40607834(lineA_46.FromX, lineA_46.FromY, lineB_46.FromX, lineB_46.FromY, (ln_96 = lineA_46, ln_96.ToX - ln_96.FromX), (ln_97 = lineA_46, ln_97.ToY - ln_97.FromY), (ln_98 = lineB_46, ln_98.ToX - ln_98.FromX), (ln_99 = lineB_46, ln_99.ToY - ln_99.FromY), 0.004363350820701567, 1E-06);
        if (result_28.tag === 1) {
            Test_TestCaseBuilder__Zero(builder$0040_37);
        }
        else {
            const other_10 = result_28;
            Test_failtest(`Should be apart but got: ${interpolate("%A%P()", [other_10])}`);
            Test_TestCaseBuilder__Zero(builder$0040_37);
        }
    }));
})()])), Test_testList("getClosestParameters tests", ofArray([(() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let ln_100, ln_101, ln_102, ln_103;
        const lineA_47 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_47 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_29;
        const lineA_48 = lineA_47;
        const lineB_48 = lineB_47;
        result_29 = XLine2D_getClosestParameters_40607834(lineA_48.FromX, lineA_48.FromY, lineB_48.FromX, lineB_48.FromY, (ln_100 = lineA_48, ln_100.ToX - ln_100.FromX), (ln_101 = lineA_48, ln_101.ToY - ln_101.FromY), (ln_102 = lineB_48, ln_102.ToX - ln_102.FromX), (ln_103 = lineB_48, ln_103.ToY - ln_103.FromY), 0.004363350820701567, 1E-06);
        if (result_29.tag === 0) {
            const u_17 = result_29.fields[1];
            const t_27 = result_29.fields[0];
            Expect_floatClose(AccuracyModule_high, t_27, 0.5, "Parameter t should be 0.5");
            Expect_floatClose(AccuracyModule_high, u_17, 0.5, "Parameter u should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_38);
        }
        else {
            const other_11 = result_29;
            Test_failtest(`Should intersect but got: ${interpolate("%A%P()", [other_11])}`);
            Test_TestCaseBuilder__Zero(builder$0040_38);
        }
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Apart lines - closest points at endpoints", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let ln_104, ln_105, ln_106, ln_107, ln_109, ln_110, ln_112, ln_113;
        const lineA_49 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_49 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(1, 2), Pt_$ctor_7B00E9A0(3, 3));
        let result_30;
        const lineA_50 = lineA_49;
        const lineB_50 = lineB_49;
        result_30 = XLine2D_getClosestParameters_40607834(lineA_50.FromX, lineA_50.FromY, lineB_50.FromX, lineB_50.FromY, (ln_104 = lineA_50, ln_104.ToX - ln_104.FromX), (ln_105 = lineA_50, ln_105.ToY - ln_105.FromY), (ln_106 = lineB_50, ln_106.ToX - ln_106.FromX), (ln_107 = lineB_50, ln_107.ToY - ln_107.FromY), 0.004363350820701567, 1E-06);
        if (result_30.tag === 2) {
            const u_18 = result_30.fields[1];
            const t_28 = result_30.fields[0];
            const sqdist = result_30.fields[2];
            const dist = Math.sqrt(sqdist);
            let d;
            let a_1;
            const ln_108 = lineA_49;
            const p = t_28;
            a_1 = Pt_$ctor_7B00E9A0_2(ln_108.FromX + (((ln_109 = ln_108, ln_109.ToX - ln_109.FromX)) * p), ln_108.FromY + (((ln_110 = ln_108, ln_110.ToY - ln_110.FromY)) * p));
            let b_1;
            const ln_111 = lineB_49;
            const p_1 = u_18;
            b_1 = Pt_$ctor_7B00E9A0_2(ln_111.FromX + (((ln_112 = ln_111, ln_112.ToX - ln_112.FromX)) * p_1), ln_111.FromY + (((ln_113 = ln_111, ln_113.ToY - ln_113.FromY)) * p_1));
            const vx = a_1.X - b_1.X;
            const vy = a_1.Y - b_1.Y;
            d = Math.sqrt((vx * vx) + (vy * vy));
            Expect_floatClose(AccuracyModule_high, t_28, 1, "Parameter t should be 1.0");
            Expect_floatClose(AccuracyModule_high, d, dist, "Distance should match computed distance");
            Expect_floatClose(AccuracyModule_high, d, 2, "Distance should match computed distance");
            Test_TestCaseBuilder__Zero(builder$0040_39);
        }
        else {
            const other_12 = result_30;
            Test_failtest(`Should be apart but got: ${interpolate("%A%P()", [other_12])}`);
            Test_TestCaseBuilder__Zero(builder$0040_39);
        }
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Nearly parallel lines within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let ln_114, ln_115, ln_116, ln_117;
        const lineA_51 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_51 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(10, 1.000001));
        let result_31;
        const lineA_52 = lineA_51;
        const lineB_52 = lineB_51;
        result_31 = XLine2D_getClosestParameters_40607834(lineA_52.FromX, lineA_52.FromY, lineB_52.FromX, lineB_52.FromY, (ln_114 = lineA_52, ln_114.ToX - ln_114.FromX), (ln_115 = lineA_52, ln_115.ToY - ln_115.FromY), (ln_116 = lineB_52, ln_116.ToX - ln_116.FromX), (ln_117 = lineB_52, ln_117.ToY - ln_117.FromY), 0.004363350820701567, 1E-06);
        if (result_31.tag === 1) {
            const u_19 = result_31.fields[1];
            const t_29 = result_31.fields[0];
            Expect_floatClose(AccuracyModule_medium, t_29, 0.5, "Parameter t should be around 0.5");
            Expect_floatClose(AccuracyModule_medium, u_19, 0.5, "Parameter u should be around 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_40);
        }
        else {
            const other_13 = result_31;
            Test_failtest(`Should be parallel within tolerance but got: ${interpolate("%A%P()", [other_13])}`);
            Test_TestCaseBuilder__Zero(builder$0040_40);
        }
    }));
})()])), Test_testList("getClosestPoints tests", ofArray([(() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let ln_118, ln_119, ln_120, ln_121;
        const lineA_53 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_53 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let result_32;
        const lineA_54 = lineA_53;
        const lineB_54 = lineB_53;
        result_32 = XLine2D_getClosestPoints_40607834(lineA_54.FromX, lineA_54.FromY, lineB_54.FromX, lineB_54.FromY, (ln_118 = lineA_54, ln_118.ToX - ln_118.FromX), (ln_119 = lineA_54, ln_119.ToY - ln_119.FromY), (ln_120 = lineB_54, ln_120.ToX - ln_120.FromX), (ln_121 = lineB_54, ln_121.ToY - ln_121.FromY), 0.004363350820701567, 1E-06);
        if (result_32.tag === 0) {
            const pt_6 = result_32.fields[0];
            Expect_floatClose(AccuracyModule_high, pt_6.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_6.Y, 0, "Y should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_41);
        }
        else {
            const other_14 = result_32;
            Test_failtest(`Should intersect but got: ${interpolate("%A%P()", [other_14])}`);
            Test_TestCaseBuilder__Zero(builder$0040_41);
        }
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Nearly parallel lines within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let ln_122, ln_123, ln_124, ln_125;
        const lineA_55 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_55 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(10, 1.00001));
        let result_33;
        const lineA_56 = lineA_55;
        const lineB_56 = lineB_55;
        result_33 = XLine2D_getClosestPoints_40607834(lineA_56.FromX, lineA_56.FromY, lineB_56.FromX, lineB_56.FromY, (ln_122 = lineA_56, ln_122.ToX - ln_122.FromX), (ln_123 = lineA_56, ln_123.ToY - ln_123.FromY), (ln_124 = lineB_56, ln_124.ToX - ln_124.FromX), (ln_125 = lineB_56, ln_125.ToY - ln_125.FromY), 0.004363350820701567, 1E-06);
        if (result_33.tag === 1) {
            const ptB = result_33.fields[1];
            const ptA = result_33.fields[0];
            let d_1;
            const a_3 = ptA;
            const b_3 = ptB;
            const vx_1 = a_3.X - b_3.X;
            const vy_1 = a_3.Y - b_3.Y;
            d_1 = Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1));
            Expect_floatClose(AccuracyModule_medium, d_1, 1, "Distance should be around 1.0");
            Test_TestCaseBuilder__Zero(builder$0040_42);
        }
        else {
            const other_15 = result_33;
            Test_failtest(`Should be parallel within tolerance but got: ${interpolate("%A%P()", [other_15])}`);
            Test_TestCaseBuilder__Zero(builder$0040_42);
        }
    }));
})()])), Test_testList("getSqDistance tests", ofArray([(() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines have zero distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        let ln_126, ln_127, ln_128, ln_129;
        const lineA_57 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_57 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, -5), Pt_$ctor_7B00E9A0(5, 5));
        let dist_1;
        const lineA_58 = lineA_57;
        const lineB_58 = lineB_57;
        dist_1 = XLine2D_getSqDistance_Z6A6F0C80(lineA_58.FromX, lineA_58.FromY, lineB_58.FromX, lineB_58.FromY, (ln_126 = lineA_58, ln_126.ToX - ln_126.FromX), (ln_127 = lineA_58, ln_127.ToY - ln_127.FromY), (ln_128 = lineB_58, ln_128.ToX - ln_128.FromX), (ln_129 = lineB_58, ln_129.ToY - ln_129.FromY));
        Expect_floatClose(AccuracyModule_high, dist_1, 0, "Distance should be 0.0");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Apart lines have positive distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let ln_130, ln_131, ln_132, ln_133;
        const lineA_59 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_59 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(2, 1), Pt_$ctor_7B00E9A0(3, 1));
        let dist_2;
        const lineA_60 = lineA_59;
        const lineB_60 = lineB_59;
        dist_2 = XLine2D_getSqDistance_Z6A6F0C80(lineA_60.FromX, lineA_60.FromY, lineB_60.FromX, lineB_60.FromY, (ln_130 = lineA_60, ln_130.ToX - ln_130.FromX), (ln_131 = lineA_60, ln_131.ToY - ln_131.FromY), (ln_132 = lineB_60, ln_132.ToX - ln_132.FromX), (ln_133 = lineB_60, ln_133.ToY - ln_133.FromY));
        Expect_isTrue(dist_2 > 0)("Distance should be positive");
        Expect_floatClose(AccuracyModule_high, dist_2, 2, "Distance should be 2.0");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let ln_134, ln_135, ln_136, ln_137;
        const lineA_61 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_61 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(10, 1));
        let dist_3;
        const lineA_62 = lineA_61;
        const lineB_62 = lineB_61;
        dist_3 = XLine2D_getSqDistance_Z6A6F0C80(lineA_62.FromX, lineA_62.FromY, lineB_62.FromX, lineB_62.FromY, (ln_134 = lineA_62, ln_134.ToX - ln_134.FromX), (ln_135 = lineA_62, ln_135.ToY - ln_135.FromY), (ln_136 = lineB_62, ln_136.ToX - ln_136.FromX), (ln_137 = lineB_62, ln_137.ToY - ln_137.FromY));
        Expect_floatClose(AccuracyModule_high, dist_3, 1, "Distance should be 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})()])), Test_testList("areEndsTouching tests", ofArray([(() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Not touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        const lineA_63 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_63 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0));
        const result_34 = XLine2D_getEndsTouching_Z44565CE5(lineA_63, lineB_63, 1E-06);
        if (result_34.tag === 0) {
            Test_TestCaseBuilder__Zero(builder$0040_46);
        }
        else {
            const other_16 = result_34;
            Test_failtest(`Should not be touching but got: ${interpolate("%A%P()", [other_16])}`);
            Test_TestCaseBuilder__Zero(builder$0040_46);
        }
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("StartA_StartB touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        const lineA_64 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_64 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 1));
        const result_35 = XLine2D_getEndsTouching_Z44565CE5(lineA_64, lineB_64, 1E-06);
        if (result_35.tag === 1) {
            Test_TestCaseBuilder__Zero(builder$0040_47);
        }
        else {
            const other_17 = result_35;
            Test_failtest(`Should touch at StartA_StartB but got: ${interpolate("%A%P()", [other_17])}`);
            Test_TestCaseBuilder__Zero(builder$0040_47);
        }
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EndA_EndB touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        const lineA_65 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_65 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(1, 0));
        const result_36 = XLine2D_getEndsTouching_Z44565CE5(lineA_65, lineB_65, 1E-06);
        if (result_36.tag === 2) {
            Test_TestCaseBuilder__Zero(builder$0040_48);
        }
        else {
            const other_18 = result_36;
            Test_failtest(`Should touch at EndA_EndB but got: ${interpolate("%A%P()", [other_18])}`);
            Test_TestCaseBuilder__Zero(builder$0040_48);
        }
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EndA_StartB touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        const lineA_66 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_66 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0));
        const result_37 = XLine2D_getEndsTouching_Z44565CE5(lineA_66, lineB_66, 1E-06);
        if (result_37.tag === 3) {
            Test_TestCaseBuilder__Zero(builder$0040_49);
        }
        else {
            const other_19 = result_37;
            Test_failtest(`Should touch at EndA_StartB but got: ${interpolate("%A%P()", [other_19])}`);
            Test_TestCaseBuilder__Zero(builder$0040_49);
        }
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("StartA_EndB touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        const lineA_67 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0));
        const lineB_67 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const result_38 = XLine2D_getEndsTouching_Z44565CE5(lineA_67, lineB_67, 1E-06);
        if (result_38.tag === 4) {
            Test_TestCaseBuilder__Zero(builder$0040_50);
        }
        else {
            const other_20 = result_38;
            Test_failtest(`Should touch at StartA_EndB but got: ${interpolate("%A%P()", [other_20])}`);
            Test_TestCaseBuilder__Zero(builder$0040_50);
        }
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Identical lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        const lineA_68 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_68 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const result_39 = XLine2D_getEndsTouching_Z44565CE5(lineA_68, lineB_68, 1E-06);
        if (result_39.tag === 5) {
            Test_TestCaseBuilder__Zero(builder$0040_51);
        }
        else {
            const other_21 = result_39;
            Test_failtest(`Should be identical but got: ${interpolate("%A%P()", [other_21])}`);
            Test_TestCaseBuilder__Zero(builder$0040_51);
        }
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Identical flipped lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        const lineA_69 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0));
        const lineB_69 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, 0));
        const result_40 = XLine2D_getEndsTouching_Z44565CE5(lineA_69, lineB_69, 1E-06);
        if (result_40.tag === 6) {
            Test_TestCaseBuilder__Zero(builder$0040_52);
        }
        else {
            const other_22 = result_40;
            Test_failtest(`Should be identical flipped but got: ${interpolate("%A%P()", [other_22])}`);
            Test_TestCaseBuilder__Zero(builder$0040_52);
        }
    }));
})()])), Test_testList("Edge cases and numerical stability", ofArray([(() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Very small lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        let ln_138, ln_139, ln_140, ln_141;
        const lineA_70 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1E-08, 0));
        const lineB_70 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(0, 1E-08));
        let result_41;
        const lineA_71 = lineA_70;
        const lineB_71 = lineB_70;
        result_41 = XLine2D_getIntersection_40607834(lineA_71.FromX, lineA_71.FromY, lineB_71.FromX, lineB_71.FromY, (ln_138 = lineA_71, ln_138.ToX - ln_138.FromX), (ln_139 = lineA_71, ln_139.ToY - ln_139.FromY), (ln_140 = lineB_71, ln_140.ToX - ln_140.FromX), (ln_141 = lineB_71, ln_141.ToY - ln_141.FromY), 0.004363350820701567, 1E-06);
        if (result_41.tag === 5) {
            Test_TestCaseBuilder__Zero(builder$0040_53);
        }
        else {
            const other_23 = result_41;
            Test_failtest(`Should be too short both but got: ${interpolate("%A%P()", [other_23])}`);
            Test_TestCaseBuilder__Zero(builder$0040_53);
        }
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Very large coordinates", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        const lineA_72 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(10000000000, 0), Pt_$ctor_7B00E9A0(10000000000 + 10, 0));
        const lineB_72 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(10000000000 + 5, -5), Pt_$ctor_7B00E9A0(10000000000 + 5, 5));
        let result_42;
        const lineA_73 = lineA_72;
        const lineB_73 = lineB_72;
        const pAx_26 = lineA_73.FromX;
        const pAy_26 = lineA_73.FromY;
        let vAx_26;
        const ln_142 = lineA_73;
        vAx_26 = (ln_142.ToX - ln_142.FromX);
        let vAy_26;
        const ln_143 = lineA_73;
        vAy_26 = (ln_143.ToY - ln_143.FromY);
        let vBx_26;
        const ln_144 = lineB_73;
        vBx_26 = (ln_144.ToX - ln_144.FromX);
        let vBy_26;
        const ln_145 = lineB_73;
        vBy_26 = (ln_145.ToY - ln_145.FromY);
        const det_22 = (vAx_26 * vBy_26) - (vAy_26 * vBx_26);
        const dx_22 = lineB_73.FromX - pAx_26;
        const dy_22 = lineB_73.FromY - pAy_26;
        const t_30 = ((dx_22 * vBy_26) - (dy_22 * vBx_26)) / det_22;
        if ((t_30 >= -1E-06) && (t_30 <= 1.000001)) {
            const u_20 = ((dx_22 * vAy_26) - (dy_22 * vAx_26)) / det_22;
            result_42 = (((u_20 >= -1E-06) && (u_20 <= 1.000001)) ? Pt_$ctor_7B00E9A0_1(pAx_26 + (t_30 * vAx_26), pAy_26 + (t_30 * vAy_26)) : undefined);
        }
        else {
            result_42 = undefined;
        }
        if (result_42 == null) {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_54);
        }
        else {
            const pt_7 = result_42;
            Expect_floatClose(AccuracyModule_medium, pt_7.X, 10000000000 + 5, "X should be 1e10 + 5.0");
            Expect_floatClose(AccuracyModule_medium, pt_7.Y, 0, "Y should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_54);
        }
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Nearly parallel lines with custom tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        let ln_146, ln_147, ln_148, ln_149;
        const lineA_74 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        const lineB_74 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0.01));
        let result_43;
        const lineA_75 = lineA_74;
        const lineB_75 = lineB_74;
        result_43 = XLine2D_getRayIntersectionParam_40607834(lineA_75.FromX, lineA_75.FromY, lineB_75.FromX, lineB_75.FromY, (ln_146 = lineA_75, ln_146.ToX - ln_146.FromX), (ln_147 = lineA_75, ln_147.ToY - ln_147.FromY), (ln_148 = lineB_75, ln_148.ToX - ln_148.FromX), (ln_149 = lineB_75, ln_149.ToY - ln_149.FromY), 0.004363350820701567, 1E-06);
        switch (result_43.tag) {
            case 1: {
                Test_TestCaseBuilder__Zero(builder$0040_55);
                break;
            }
            case 0: {
                Test_TestCaseBuilder__Zero(builder$0040_55);
                break;
            }
            default: {
                Test_TestCaseBuilder__Zero(builder$0040_55);
            }
        }
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Zero-length line detection - line A", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        let ln_150, ln_151, ln_152, ln_153;
        const lineA_76 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(5, 5), Pt_$ctor_7B00E9A0(5, 5));
        const lineB_76 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0));
        let result_44;
        const lineA_77 = lineA_76;
        const lineB_77 = lineB_76;
        result_44 = XLine2D_getIntersection_40607834(lineA_77.FromX, lineA_77.FromY, lineB_77.FromX, lineB_77.FromY, (ln_150 = lineA_77, ln_150.ToX - ln_150.FromX), (ln_151 = lineA_77, ln_151.ToY - ln_151.FromY), (ln_152 = lineB_77, ln_152.ToX - ln_152.FromX), (ln_153 = lineB_77, ln_153.ToY - ln_153.FromY), 0.004363350820701567, 1E-06);
        switch (result_44.tag) {
            case 3:
            case 5: {
                Test_TestCaseBuilder__Zero(builder$0040_56);
                break;
            }
            default: {
                const other_24 = result_44;
                Test_failtest(`Should be too short but got: ${interpolate("%A%P()", [other_24])}`);
                Test_TestCaseBuilder__Zero(builder$0040_56);
            }
        }
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Diagonal lines at 45 degrees", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        const lineA_78 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 10));
        const lineB_78 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, 10), Pt_$ctor_7B00E9A0(10, 0));
        let result_45;
        const lineA_79 = lineA_78;
        const lineB_79 = lineB_78;
        const pAx_28 = lineA_79.FromX;
        const pAy_28 = lineA_79.FromY;
        let vAx_28;
        const ln_154 = lineA_79;
        vAx_28 = (ln_154.ToX - ln_154.FromX);
        let vAy_28;
        const ln_155 = lineA_79;
        vAy_28 = (ln_155.ToY - ln_155.FromY);
        let vBx_28;
        const ln_156 = lineB_79;
        vBx_28 = (ln_156.ToX - ln_156.FromX);
        let vBy_28;
        const ln_157 = lineB_79;
        vBy_28 = (ln_157.ToY - ln_157.FromY);
        const det_23 = (vAx_28 * vBy_28) - (vAy_28 * vBx_28);
        const dx_23 = lineB_79.FromX - pAx_28;
        const dy_23 = lineB_79.FromY - pAy_28;
        const t_31 = ((dx_23 * vBy_28) - (dy_23 * vBx_28)) / det_23;
        if ((t_31 >= -1E-06) && (t_31 <= 1.000001)) {
            const u_21 = ((dx_23 * vAy_28) - (dy_23 * vAx_28)) / det_23;
            result_45 = (((u_21 >= -1E-06) && (u_21 <= 1.000001)) ? Pt_$ctor_7B00E9A0_1(pAx_28 + (t_31 * vAx_28), pAy_28 + (t_31 * vAy_28)) : undefined);
        }
        else {
            result_45 = undefined;
        }
        if (result_45 == null) {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_57);
        }
        else {
            const pt_8 = result_45;
            Expect_floatClose(AccuracyModule_high, pt_8.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_8.Y, 5, "Y should be 5.0");
            Test_TestCaseBuilder__Zero(builder$0040_57);
        }
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Lines with negative coordinates", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        const lineA_80 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(-10, -10), Pt_$ctor_7B00E9A0(10, -10));
        const lineB_80 = Line2D_$ctor_Z53905FA0(Pt_$ctor_7B00E9A0(0, -20), Pt_$ctor_7B00E9A0(0, 0));
        let result_46;
        const lineA_81 = lineA_80;
        const lineB_81 = lineB_80;
        const pAx_30 = lineA_81.FromX;
        const pAy_30 = lineA_81.FromY;
        let vAx_30;
        const ln_158 = lineA_81;
        vAx_30 = (ln_158.ToX - ln_158.FromX);
        let vAy_30;
        const ln_159 = lineA_81;
        vAy_30 = (ln_159.ToY - ln_159.FromY);
        let vBx_30;
        const ln_160 = lineB_81;
        vBx_30 = (ln_160.ToX - ln_160.FromX);
        let vBy_30;
        const ln_161 = lineB_81;
        vBy_30 = (ln_161.ToY - ln_161.FromY);
        const det_24 = (vAx_30 * vBy_30) - (vAy_30 * vBx_30);
        const dx_24 = lineB_81.FromX - pAx_30;
        const dy_24 = lineB_81.FromY - pAy_30;
        const t_32 = ((dx_24 * vBy_30) - (dy_24 * vBx_30)) / det_24;
        if ((t_32 >= -1E-06) && (t_32 <= 1.000001)) {
            const u_22 = ((dx_24 * vAy_30) - (dy_24 * vAx_30)) / det_24;
            result_46 = (((u_22 >= -1E-06) && (u_22 <= 1.000001)) ? Pt_$ctor_7B00E9A0_1(pAx_30 + (t_32 * vAx_30), pAy_30 + (t_32 * vAy_30)) : undefined);
        }
        else {
            result_46 = undefined;
        }
        if (result_46 == null) {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_58);
        }
        else {
            const pt_9 = result_46;
            Expect_floatClose(AccuracyModule_high, pt_9.X, 0, "X should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_9.Y, -10, "Y should be -10.0");
            Test_TestCaseBuilder__Zero(builder$0040_58);
        }
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getClosestParameters returns Intersect for crossing lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let ln_162, ln_163, ln_164, ln_165;
        const lnA_3 = Line2D_$ctor_77D16AC0(0, 0, 4, 0);
        const lnB_3 = Line2D_$ctor_77D16AC0(2, -2, 2, 2);
        let result_47;
        const lineA_82 = lnA_3;
        const lineB_82 = lnB_3;
        result_47 = XLine2D_getClosestParameters_40607834(lineA_82.FromX, lineA_82.FromY, lineB_82.FromX, lineB_82.FromY, (ln_162 = lineA_82, ln_162.ToX - ln_162.FromX), (ln_163 = lineA_82, ln_163.ToY - ln_163.FromY), (ln_164 = lineB_82, ln_164.ToX - ln_164.FromX), (ln_165 = lineB_82, ln_165.ToY - ln_165.FromY), 0.004363350820701567, 1E-06);
        if (result_47.tag === 0) {
            const u_23 = result_47.fields[1];
            const t_33 = result_47.fields[0];
            Expect_floatClose(AccuracyModule_veryHigh, t_33, 0.5, "t should be 0.5");
            Expect_floatClose(AccuracyModule_veryHigh, u_23, 0.5, "u should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_59);
        }
        else {
            Expect_isTrue(false)("Should return Intersect");
            Test_TestCaseBuilder__Zero(builder$0040_59);
        }
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getClosestParameters returns Apart for skew non-intersecting lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let ln_166, ln_167, ln_168, ln_169;
        const lnA_4 = Line2D_$ctor_77D16AC0(0, 0, 1, 0);
        const lnB_4 = Line2D_$ctor_77D16AC0(0, 2, 1, 2);
        let result_48;
        const lineA_83 = lnA_4;
        const lineB_83 = lnB_4;
        result_48 = XLine2D_getClosestParameters_40607834(lineA_83.FromX, lineA_83.FromY, lineB_83.FromX, lineB_83.FromY, (ln_166 = lineA_83, ln_166.ToX - ln_166.FromX), (ln_167 = lineA_83, ln_167.ToY - ln_167.FromY), (ln_168 = lineB_83, ln_168.ToX - ln_168.FromX), (ln_169 = lineB_83, ln_169.ToY - ln_169.FromY), 0.004363350820701567, 1E-06);
        if (result_48.tag === 1) {
            const u_24 = result_48.fields[1];
            const t_34 = result_48.fields[0];
            Expect_floatClose(AccuracyModule_high, t_34, 0.5, "t should be midpoint");
            Expect_floatClose(AccuracyModule_high, u_24, 0.5, "u should be midpoint");
            Test_TestCaseBuilder__Zero(builder$0040_60);
        }
        else {
            Expect_isTrue(false)(toText(printf("Should return Parallel for parallel lines, got %A"))(result_48));
            Test_TestCaseBuilder__Zero(builder$0040_60);
        }
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getClosestParameters returns Parallel for parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let ln_170, ln_171, ln_172, ln_173;
        const lnA_5 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const lnB_5 = Line2D_$ctor_77D16AC0(0, 1, 10, 1);
        let result_49;
        const lineA_84 = lnA_5;
        const lineB_84 = lnB_5;
        result_49 = XLine2D_getClosestParameters_40607834(lineA_84.FromX, lineA_84.FromY, lineB_84.FromX, lineB_84.FromY, (ln_170 = lineA_84, ln_170.ToX - ln_170.FromX), (ln_171 = lineA_84, ln_171.ToY - ln_171.FromY), (ln_172 = lineB_84, ln_172.ToX - ln_172.FromX), (ln_173 = lineB_84, ln_173.ToY - ln_173.FromY), 0.004363350820701567, 1E-06);
        if (result_49.tag === 1) {
            const u_25 = result_49.fields[1];
            const t_35 = result_49.fields[0];
            Expect_floatClose(AccuracyModule_high, t_35, 0.5, "t should be midpoint");
            Expect_floatClose(AccuracyModule_high, u_25, 0.5, "u should be midpoint");
            Test_TestCaseBuilder__Zero(builder$0040_61);
        }
        else {
            Expect_isTrue(false)("Should return Parallel");
            Test_TestCaseBuilder__Zero(builder$0040_61);
        }
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getClosestPoints returns Intersect point for crossing lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        let ln_174, ln_175, ln_176, ln_177;
        const lnA_6 = Line2D_$ctor_77D16AC0(0, 0, 4, 0);
        const lnB_6 = Line2D_$ctor_77D16AC0(2, -2, 2, 2);
        let result_50;
        const lineA_85 = lnA_6;
        const lineB_85 = lnB_6;
        result_50 = XLine2D_getClosestPoints_40607834(lineA_85.FromX, lineA_85.FromY, lineB_85.FromX, lineB_85.FromY, (ln_174 = lineA_85, ln_174.ToX - ln_174.FromX), (ln_175 = lineA_85, ln_175.ToY - ln_175.FromY), (ln_176 = lineB_85, ln_176.ToX - ln_176.FromX), (ln_177 = lineB_85, ln_177.ToY - ln_177.FromY), 0.004363350820701567, 1E-06);
        if (result_50.tag === 0) {
            const pt_10 = result_50.fields[0];
            Expect_floatClose(AccuracyModule_veryHigh, pt_10.X, 2, "X should be 2");
            Expect_floatClose(AccuracyModule_veryHigh, pt_10.Y, 0, "Y should be 0");
            Test_TestCaseBuilder__Zero(builder$0040_62);
        }
        else {
            Expect_isTrue(false)("Should return Intersect");
            Test_TestCaseBuilder__Zero(builder$0040_62);
        }
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getClosestPoints returns Apart with two points for skew non-parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        let ln_178, ln_179, ln_180, ln_181;
        const lnA_7 = Line2D_$ctor_77D16AC0(0, 0, 1, 0);
        const lnB_7 = Line2D_$ctor_77D16AC0(0, 1, 0.5, 2);
        const result_51 = XLine2D_getClosestPoints_40607834_1(lnA_7.FromX, lnA_7.FromY, lnB_7.FromX, lnB_7.FromY, (ln_178 = lnA_7, ln_178.ToX - ln_178.FromX), (ln_179 = lnA_7, ln_179.ToY - ln_179.FromY), (ln_180 = lnB_7, ln_180.ToX - ln_180.FromX), (ln_181 = lnB_7, ln_181.ToY - ln_181.FromY), 0.004363350820701567, 1E-06);
        if (result_51.tag === 2) {
            const sqDist = result_51.fields[2];
            const ptB_1 = result_51.fields[1];
            Expect_isTrue(sqDist > 0)("square distance positive");
            Expect_floatClose(AccuracyModule_medium, ptB_1.Y, 1, "ptB should be at Y=1");
            Test_TestCaseBuilder__Zero(builder$0040_63);
        }
        else {
            Expect_isTrue(false)(toText(printf("Should return Apart, got %A"))(result_51));
            Test_TestCaseBuilder__Zero(builder$0040_63);
        }
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getClosestPoints returns Parallel with two points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let ln_182, ln_183, ln_184, ln_185;
        const lnA_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const lnB_8 = Line2D_$ctor_77D16AC0(0, 1, 10, 1);
        const result_52 = XLine2D_getClosestPoints_40607834_1(lnA_8.FromX, lnA_8.FromY, lnB_8.FromX, lnB_8.FromY, (ln_182 = lnA_8, ln_182.ToX - ln_182.FromX), (ln_183 = lnA_8, ln_183.ToY - ln_183.FromY), (ln_184 = lnB_8, ln_184.ToX - ln_184.FromX), (ln_185 = lnB_8, ln_185.ToY - ln_185.FromY), 0.004363350820701567, 1E-06);
        if (result_52.tag === 1) {
            const ptB_2 = result_52.fields[1];
            const ptA_1 = result_52.fields[0];
            Expect_floatClose(AccuracyModule_high, ptA_1.X, 5, "ptA X midpoint");
            Expect_floatClose(AccuracyModule_high, ptA_1.Y, 0, "ptA Y");
            Expect_floatClose(AccuracyModule_high, ptB_2.X, 5, "ptB X midpoint");
            Expect_floatClose(AccuracyModule_high, ptB_2.Y, 1, "ptB Y");
            Test_TestCaseBuilder__Zero(builder$0040_64);
        }
        else {
            Expect_isTrue(false)("Should return Parallel");
            Test_TestCaseBuilder__Zero(builder$0040_64);
        }
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance returns 0 for intersecting lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        let ln_186, ln_187, ln_188, ln_189;
        const lnA_9 = Line2D_$ctor_77D16AC0(0, 0, 4, 0);
        const lnB_9 = Line2D_$ctor_77D16AC0(2, -2, 2, 2);
        const sqDist_1 = XLine2D_getSqDistance_Z6A6F0C80_1(lnA_9.FromX, lnA_9.FromY, lnB_9.FromX, lnB_9.FromY, (ln_186 = lnA_9, ln_186.ToX - ln_186.FromX), (ln_187 = lnA_9, ln_187.ToY - ln_187.FromY), (ln_188 = lnB_9, ln_188.ToX - ln_188.FromX), (ln_189 = lnB_9, ln_189.ToY - ln_189.FromY));
        Expect_floatClose(AccuracyModule_veryHigh, sqDist_1, 0, "Intersecting lines should have 0 distance");
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})(), (() => {
    const builder$0040_66 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance returns 0 for touching endpoints", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_66, Test_TestCaseBuilder__Delay_1505(builder$0040_66, () => {
        let ln_190, ln_191, ln_192, ln_193;
        const lnA_10 = Line2D_$ctor_77D16AC0(0, 0, 2, 0);
        const lnB_10 = Line2D_$ctor_77D16AC0(2, 0, 4, 2);
        let sqDist_2;
        const lineA_86 = lnA_10;
        const lineB_86 = lnB_10;
        sqDist_2 = XLine2D_getSqDistance_Z6A6F0C80(lineA_86.FromX, lineA_86.FromY, lineB_86.FromX, lineB_86.FromY, (ln_190 = lineA_86, ln_190.ToX - ln_190.FromX), (ln_191 = lineA_86, ln_191.ToY - ln_191.FromY), (ln_192 = lineB_86, ln_192.ToX - ln_192.FromX), (ln_193 = lineB_86, ln_193.ToY - ln_193.FromY));
        Expect_floatClose(AccuracyModule_veryHigh, sqDist_2, 0, "Touching endpoints should have 0 distance");
        Test_TestCaseBuilder__Zero(builder$0040_66);
    }));
})(), (() => {
    const builder$0040_67 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance for parallel horizontal lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_67, Test_TestCaseBuilder__Delay_1505(builder$0040_67, () => {
        let ln_194, ln_195, ln_196, ln_197;
        const lnA_11 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const lnB_11 = Line2D_$ctor_77D16AC0(0, 3, 10, 3);
        let sqDist_3;
        const lineA_87 = lnA_11;
        const lineB_87 = lnB_11;
        sqDist_3 = XLine2D_getSqDistance_Z6A6F0C80(lineA_87.FromX, lineA_87.FromY, lineB_87.FromX, lineB_87.FromY, (ln_194 = lineA_87, ln_194.ToX - ln_194.FromX), (ln_195 = lineA_87, ln_195.ToY - ln_195.FromY), (ln_196 = lineB_87, ln_196.ToX - ln_196.FromX), (ln_197 = lineB_87, ln_197.ToY - ln_197.FromY));
        Expect_floatClose(AccuracyModule_veryHigh, sqDist_3, 9, "Parallel lines 3 units apart: 3^2 = 9");
        Test_TestCaseBuilder__Zero(builder$0040_67);
    }));
})(), (() => {
    const builder$0040_68 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance for parallel vertical lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_68, Test_TestCaseBuilder__Delay_1505(builder$0040_68, () => {
        let ln_198, ln_199, ln_200, ln_201;
        const lnA_12 = Line2D_$ctor_77D16AC0(0, 0, 0, 10);
        const lnB_12 = Line2D_$ctor_77D16AC0(4, 0, 4, 10);
        let sqDist_4;
        const lineA_88 = lnA_12;
        const lineB_88 = lnB_12;
        sqDist_4 = XLine2D_getSqDistance_Z6A6F0C80(lineA_88.FromX, lineA_88.FromY, lineB_88.FromX, lineB_88.FromY, (ln_198 = lineA_88, ln_198.ToX - ln_198.FromX), (ln_199 = lineA_88, ln_199.ToY - ln_199.FromY), (ln_200 = lineB_88, ln_200.ToX - ln_200.FromX), (ln_201 = lineB_88, ln_201.ToY - ln_201.FromY));
        Expect_floatClose(AccuracyModule_veryHigh, sqDist_4, 16, "Parallel lines 4 units apart: 4^2 = 16");
        Test_TestCaseBuilder__Zero(builder$0040_68);
    }));
})(), (() => {
    const builder$0040_69 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance for non-overlapping parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_69, Test_TestCaseBuilder__Delay_1505(builder$0040_69, () => {
        let ln_202, ln_203, ln_204, ln_205;
        const lnA_13 = Line2D_$ctor_77D16AC0(0, 0, 5, 0);
        const lnB_13 = Line2D_$ctor_77D16AC0(10, 2, 15, 2);
        let sqDist_5;
        const lineA_89 = lnA_13;
        const lineB_89 = lnB_13;
        sqDist_5 = XLine2D_getSqDistance_Z6A6F0C80(lineA_89.FromX, lineA_89.FromY, lineB_89.FromX, lineB_89.FromY, (ln_202 = lineA_89, ln_202.ToX - ln_202.FromX), (ln_203 = lineA_89, ln_203.ToY - ln_203.FromY), (ln_204 = lineB_89, ln_204.ToX - ln_204.FromX), (ln_205 = lineB_89, ln_205.ToY - ln_205.FromY));
        Expect_floatClose(AccuracyModule_high, sqDist_5, 29, "Distance should be sqrt(29)");
        Test_TestCaseBuilder__Zero(builder$0040_69);
    }));
})(), (() => {
    const builder$0040_70 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance for skew lines (would intersect if extended)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_70, Test_TestCaseBuilder__Delay_1505(builder$0040_70, () => {
        let ln_206, ln_207, ln_208, ln_209;
        const lnA_14 = Line2D_$ctor_77D16AC0(0, 0, 1, 0);
        const lnB_14 = Line2D_$ctor_77D16AC0(2, -1, 2, 1);
        let sqDist_6;
        const lineA_90 = lnA_14;
        const lineB_90 = lnB_14;
        sqDist_6 = XLine2D_getSqDistance_Z6A6F0C80(lineA_90.FromX, lineA_90.FromY, lineB_90.FromX, lineB_90.FromY, (ln_206 = lineA_90, ln_206.ToX - ln_206.FromX), (ln_207 = lineA_90, ln_207.ToY - ln_207.FromY), (ln_208 = lineB_90, ln_208.ToX - ln_208.FromX), (ln_209 = lineB_90, ln_209.ToY - ln_209.FromY));
        Expect_floatClose(AccuracyModule_veryHigh, sqDist_6, 1, "Distance should be 1^2 = 1");
        Test_TestCaseBuilder__Zero(builder$0040_70);
    }));
})(), (() => {
    const builder$0040_71 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance for perpendicular non-intersecting lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_71, Test_TestCaseBuilder__Delay_1505(builder$0040_71, () => {
        let ln_210, ln_211, ln_212, ln_213;
        const lnA_15 = Line2D_$ctor_77D16AC0(0, 0, 5, 0);
        const lnB_15 = Line2D_$ctor_77D16AC0(0, 2, 0, 7);
        let sqDist_7;
        const lineA_91 = lnA_15;
        const lineB_91 = lnB_15;
        sqDist_7 = XLine2D_getSqDistance_Z6A6F0C80(lineA_91.FromX, lineA_91.FromY, lineB_91.FromX, lineB_91.FromY, (ln_210 = lineA_91, ln_210.ToX - ln_210.FromX), (ln_211 = lineA_91, ln_211.ToY - ln_211.FromY), (ln_212 = lineB_91, ln_212.ToX - ln_212.FromX), (ln_213 = lineB_91, ln_213.ToY - ln_213.FromY));
        Expect_floatClose(AccuracyModule_veryHigh, sqDist_7, 4, "Distance should be 2^2 = 4");
        Test_TestCaseBuilder__Zero(builder$0040_71);
    }));
})(), (() => {
    const builder$0040_72 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance for diagonal lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_72, Test_TestCaseBuilder__Delay_1505(builder$0040_72, () => {
        let ln_214, ln_215, ln_216, ln_217;
        const lnA_16 = Line2D_$ctor_77D16AC0(0, 0, 2, 2);
        const lnB_16 = Line2D_$ctor_77D16AC0(3, 0, 5, 2);
        let sqDist_8;
        const lineA_92 = lnA_16;
        const lineB_92 = lnB_16;
        sqDist_8 = XLine2D_getSqDistance_Z6A6F0C80(lineA_92.FromX, lineA_92.FromY, lineB_92.FromX, lineB_92.FromY, (ln_214 = lineA_92, ln_214.ToX - ln_214.FromX), (ln_215 = lineA_92, ln_215.ToY - ln_215.FromY), (ln_216 = lineB_92, ln_216.ToX - ln_216.FromX), (ln_217 = lineB_92, ln_217.ToY - ln_217.FromY));
        Expect_floatClose(AccuracyModule_veryHigh, sqDist_8, (1.5 * 1.5) * 2, "Diagonal lines should have positive distance");
        Test_TestCaseBuilder__Zero(builder$0040_72);
    }));
})(), (() => {
    const builder$0040_73 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance for coincident overlapping lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_73, Test_TestCaseBuilder__Delay_1505(builder$0040_73, () => {
        let ln_218, ln_219, ln_220, ln_221;
        const lnA_17 = Line2D_$ctor_77D16AC0(0, 0, 5, 0);
        const lnB_17 = Line2D_$ctor_77D16AC0(2, 0, 7, 0);
        let sqDist_9;
        const lineA_93 = lnA_17;
        const lineB_93 = lnB_17;
        sqDist_9 = XLine2D_getSqDistance_Z6A6F0C80(lineA_93.FromX, lineA_93.FromY, lineB_93.FromX, lineB_93.FromY, (ln_218 = lineA_93, ln_218.ToX - ln_218.FromX), (ln_219 = lineA_93, ln_219.ToY - ln_219.FromY), (ln_220 = lineB_93, ln_220.ToX - ln_220.FromX), (ln_221 = lineB_93, ln_221.ToY - ln_221.FromY));
        Expect_floatClose(AccuracyModule_veryHigh, sqDist_9, 0, "Overlapping coincident lines should have 0 distance");
        Test_TestCaseBuilder__Zero(builder$0040_73);
    }));
})(), (() => {
    const builder$0040_74 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance with very small lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_74, Test_TestCaseBuilder__Delay_1505(builder$0040_74, () => {
        let ln_222, ln_223, ln_224, ln_225;
        const lnA_18 = Line2D_$ctor_77D16AC0(0, 0, 0.001, 0);
        const lnB_18 = Line2D_$ctor_77D16AC0(1, 0, 1.001, 0);
        let sqDist_10;
        const lineA_94 = lnA_18;
        const lineB_94 = lnB_18;
        sqDist_10 = XLine2D_getSqDistance_Z6A6F0C80(lineA_94.FromX, lineA_94.FromY, lineB_94.FromX, lineB_94.FromY, (ln_222 = lineA_94, ln_222.ToX - ln_222.FromX), (ln_223 = lineA_94, ln_223.ToY - ln_223.FromY), (ln_224 = lineB_94, ln_224.ToX - ln_224.FromX), (ln_225 = lineB_94, ln_225.ToY - ln_225.FromY));
        Expect_floatClose(AccuracyModule_low, sqDist_10, 0.999, "Small lines1 ~1 unit apart");
        Test_TestCaseBuilder__Zero(builder$0040_74);
    }));
})(), (() => {
    const builder$0040_75 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance with very small lines2", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_75, Test_TestCaseBuilder__Delay_1505(builder$0040_75, () => {
        let ln_226, ln_227, ln_228, ln_229;
        const lnA_19 = Line2D_$ctor_77D16AC0(0, 0, 0.001, 0);
        const lnB_19 = Line2D_$ctor_77D16AC0(0, 1, 0.001, 1);
        let sqDist_11;
        const lineA_95 = lnA_19;
        const lineB_95 = lnB_19;
        sqDist_11 = XLine2D_getSqDistance_Z6A6F0C80(lineA_95.FromX, lineA_95.FromY, lineB_95.FromX, lineB_95.FromY, (ln_226 = lineA_95, ln_226.ToX - ln_226.FromX), (ln_227 = lineA_95, ln_227.ToY - ln_227.FromY), (ln_228 = lineB_95, ln_228.ToX - ln_228.FromX), (ln_229 = lineB_95, ln_229.ToY - ln_229.FromY));
        Expect_floatClose(AccuracyModule_low, sqDist_11, 1, "Small line2 ~1 unit apart");
        Test_TestCaseBuilder__Zero(builder$0040_75);
    }));
})(), (() => {
    const builder$0040_76 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getClosestParams parallel", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_76, Test_TestCaseBuilder__Delay_1505(builder$0040_76, () => {
        let ln_230, ln_231, ln_232, ln_233;
        const lnA_20 = Line2D_$ctor_77D16AC0(0, 0, 2, 2);
        const lnB_20 = Line2D_$ctor_77D16AC0(3, 0, 5, 2);
        let patternInput_3;
        let matchValue;
        const lineA_96 = lnA_20;
        const lineB_96 = lnB_20;
        matchValue = XLine2D_getClosestParameters_40607834(lineA_96.FromX, lineA_96.FromY, lineB_96.FromX, lineB_96.FromY, (ln_230 = lineA_96, ln_230.ToX - ln_230.FromX), (ln_231 = lineA_96, ln_231.ToY - ln_231.FromY), (ln_232 = lineB_96, ln_232.ToX - ln_232.FromX), (ln_233 = lineB_96, ln_233.ToY - ln_233.FromY), 0.004363350820701567, 1E-06);
        switch (matchValue.tag) {
            case 0: {
                const u_26 = matchValue.fields[1];
                const t_36 = matchValue.fields[0];
                patternInput_3 = [t_36, u_26];
                break;
            }
            case 1: {
                const u_27 = matchValue.fields[1];
                const t_37 = matchValue.fields[0];
                patternInput_3 = [t_37, u_27];
                break;
            }
            case 2: {
                const u_28 = matchValue.fields[1];
                const t_38 = matchValue.fields[0];
                patternInput_3 = [t_38, u_28];
                break;
            }
            default: {
                const r = matchValue;
                patternInput_3 = toFail(`Unexpected result: ${r}`);
            }
        }
        const v = patternInput_3[1];
        const u_29 = patternInput_3[0];
        Expect_floatClose(AccuracyModule_veryHigh, u_29, 0.75 + 0.125, "getClosestParams u should be 0.75+0.125");
        Expect_floatClose(AccuracyModule_veryHigh, v, 0.125, "getClosestParams v should be 0.125");
        Test_TestCaseBuilder__Zero(builder$0040_76);
    }));
})(), (() => {
    const builder$0040_77 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getClosestParams apart", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_77, Test_TestCaseBuilder__Delay_1505(builder$0040_77, () => {
        let ln_234, ln_235, ln_236, ln_237;
        const lnA_21 = Line2D_$ctor_77D16AC0(0, 0, 2, 2);
        const lnB_21 = Line2D_$ctor_77D16AC0(2, 1, 4, 2);
        let patternInput_4;
        let matchValue_1;
        const lineA_97 = lnA_21;
        const lineB_97 = lnB_21;
        matchValue_1 = XLine2D_getClosestParameters_40607834(lineA_97.FromX, lineA_97.FromY, lineB_97.FromX, lineB_97.FromY, (ln_234 = lineA_97, ln_234.ToX - ln_234.FromX), (ln_235 = lineA_97, ln_235.ToY - ln_235.FromY), (ln_236 = lineB_97, ln_236.ToX - ln_236.FromX), (ln_237 = lineB_97, ln_237.ToY - ln_237.FromY), 0.004363350820701567, 1E-06);
        switch (matchValue_1.tag) {
            case 0: {
                const u_30 = matchValue_1.fields[1];
                const t_39 = matchValue_1.fields[0];
                patternInput_4 = [t_39, u_30];
                break;
            }
            case 1: {
                const u_31 = matchValue_1.fields[1];
                const t_40 = matchValue_1.fields[0];
                patternInput_4 = [t_40, u_31];
                break;
            }
            case 2: {
                const u_32 = matchValue_1.fields[1];
                const t_41 = matchValue_1.fields[0];
                patternInput_4 = [t_41, u_32];
                break;
            }
            default: {
                const r_1 = matchValue_1;
                patternInput_4 = toFail(`Unexpected result: ${r_1}`);
            }
        }
        const v_1 = patternInput_4[1];
        const u_33 = patternInput_4[0];
        Expect_floatClose(AccuracyModule_veryHigh, u_33, 0.75, "getClosestParams apart u should be 0.75");
        Expect_floatClose(AccuracyModule_veryHigh, v_1, 0, "getClosestParams apart v should be 0.0");
        Test_TestCaseBuilder__Zero(builder$0040_77);
    }));
})(), (() => {
    const builder$0040_78 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance with apart 0.71", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_78, Test_TestCaseBuilder__Delay_1505(builder$0040_78, () => {
        let ln_238, ln_239, ln_240, ln_241;
        const lnA_22 = Line2D_$ctor_77D16AC0(0, 0, 2, 2);
        const lnB_22 = Line2D_$ctor_77D16AC0(2, 1, 4, 2);
        let sqDist_12;
        let value;
        const lineA_98 = lnA_22;
        const lineB_98 = lnB_22;
        value = XLine2D_getSqDistance_Z6A6F0C80(lineA_98.FromX, lineA_98.FromY, lineB_98.FromX, lineB_98.FromY, (ln_238 = lineA_98, ln_238.ToX - ln_238.FromX), (ln_239 = lineA_98, ln_239.ToY - ln_239.FromY), (ln_240 = lineB_98, ln_240.ToX - ln_240.FromX), (ln_241 = lineB_98, ln_241.ToY - ln_241.FromY));
        sqDist_12 = Math.sqrt(value);
        Expect_floatClose(AccuracyModule_high, sqDist_12, 0.70710678, "getSqDistance with apart 0.71");
        Test_TestCaseBuilder__Zero(builder$0040_78);
    }));
})(), (() => {
    const builder$0040_79 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance with apart 0.71 rev", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_79, Test_TestCaseBuilder__Delay_1505(builder$0040_79, () => {
        let ln_245, ln_246, ln_247, ln_248;
        const lnA_23 = Line2D_$ctor_77D16AC0(0, 0, 2, 2);
        let lnB_23;
        const ln_244 = Line2D_$ctor_77D16AC0(2, 1, 4, 2);
        lnB_23 = Line2D_$ctor_77D16AC0_1(ln_244.ToX, ln_244.ToY, ln_244.FromX, ln_244.FromY);
        let sqDist_13;
        let value_1;
        const lineA_99 = lnA_23;
        const lineB_99 = lnB_23;
        value_1 = XLine2D_getSqDistance_Z6A6F0C80(lineA_99.FromX, lineA_99.FromY, lineB_99.FromX, lineB_99.FromY, (ln_245 = lineA_99, ln_245.ToX - ln_245.FromX), (ln_246 = lineA_99, ln_246.ToY - ln_246.FromY), (ln_247 = lineB_99, ln_247.ToX - ln_247.FromX), (ln_248 = lineB_99, ln_248.ToY - ln_248.FromY));
        sqDist_13 = Math.sqrt(value_1);
        Expect_floatClose(AccuracyModule_high, sqDist_13, 0.70710678, "getSqDistance with apart 0.71 rev");
        Test_TestCaseBuilder__Zero(builder$0040_79);
    }));
})(), (() => {
    const builder$0040_80 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSqDistance with apart 0.71 rev2", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_80, Test_TestCaseBuilder__Delay_1505(builder$0040_80, () => {
        let ln_252, ln_253, ln_254, ln_255;
        let lnA_24;
        const ln_251 = Line2D_$ctor_77D16AC0(0, 0, 2, 2);
        lnA_24 = Line2D_$ctor_77D16AC0_1(ln_251.ToX, ln_251.ToY, ln_251.FromX, ln_251.FromY);
        const lnB_24 = Line2D_$ctor_77D16AC0(2, 1, 4, 2);
        let sqDist_14;
        let value_2;
        const lineA_100 = lnA_24;
        const lineB_100 = lnB_24;
        value_2 = XLine2D_getSqDistance_Z6A6F0C80(lineA_100.FromX, lineA_100.FromY, lineB_100.FromX, lineB_100.FromY, (ln_252 = lineA_100, ln_252.ToX - ln_252.FromX), (ln_253 = lineA_100, ln_253.ToY - ln_253.FromY), (ln_254 = lineB_100, ln_254.ToX - ln_254.FromX), (ln_255 = lineB_100, ln_255.ToY - ln_255.FromY));
        sqDist_14 = Math.sqrt(value_2);
        Expect_floatClose(AccuracyModule_high, sqDist_14, 0.70710678, "getSqDistance with apart 0.71 rev2");
        Test_TestCaseBuilder__Zero(builder$0040_80);
    }));
})()]))]));

