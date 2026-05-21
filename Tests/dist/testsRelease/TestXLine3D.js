
import { Expect_isNone, Test_failtest, Expect_isFalse, AccuracyModule_medium, Expect_isTrue, Test_TestCaseBuilder__Zero, AccuracyModule_high, Expect_floatClose, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { isInfinity } from "./fable_modules/fable-library-js.5.0.0/Double.js";
import { Line3D_$ctor_5A6659A0 } from "./Src/Line3D.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { XLine3D_getSqDistance_Z15A9A3C0, XLine3D_getClosestPoints_Z1F58440C, XLine3D_getClosestParameters_Z1F58440C, XLine3D_getIntersection_ZA268E31, XLine3D_getIntersectionParam_ZA268E31, XLine3D_getRayIntersection_ZA268E31, XLine3D_getRayClosestParam_Z1F58440C, XLine3D_tryIntersect_ZA268E31, XLine3D_tryIntersectRay_ZA268E31, XLine3D_doOverlap_6B19E37B } from "./Src/XLine3D.js";
import { XLine3D_getEndsTouching_Z4454D4C5 } from "./Src/XLine3D.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";

export const tests = Test_testList("XLine3D Tests", ofArray([Test_testList("parameterA tests", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines - basic case", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let pA_1, pB_1, vA_1, vB_1, vAx, vAy, vAz, vBx, vBy, vBz, crossX, crossY, crossZ, dx, dy, dz;
        Expect_floatClose(AccuracyModule_high, (pA_1 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (pB_1 = Pnt_$ctor_Z7AD9E565(1, 0, 0), (vA_1 = Vec_$ctor_Z7AD9E565(1, 0, 0), (vB_1 = Vec_$ctor_Z7AD9E565(0, 1, 0), (vAx = vA_1.X, (vAy = vA_1.Y, (vAz = vA_1.Z, (vBx = vB_1.X, (vBy = vB_1.Y, (vBz = vB_1.Z, (crossX = ((vAy * vBz) - (vAz * vBy)), (crossY = ((vAz * vBx) - (vAx * vBz)), (crossZ = ((vAx * vBy) - (vAy * vBx)), (dx = (pB_1.X - pA_1.X), (dy = (pB_1.Y - pA_1.Y), (dz = (pB_1.Z - pA_1.Z), (((((dy * vBz) - (dz * vBy)) * crossX) + (((dz * vBx) - (dx * vBz)) * crossY)) + (((dx * vBy) - (dy * vBx)) * crossZ)) / (((crossX * crossX) + (crossY * crossY)) + (crossZ * crossZ)))))))))))))))))), 1, "Parameter should be 1.0");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines - vertical and horizontal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let pA_3, pB_3, vA_3, vB_3, vAx_1, vAy_1, vAz_1, vBx_1, vBy_1, vBz_1, crossX_1, crossY_1, crossZ_1, dx_1, dy_1, dz_1;
        Expect_floatClose(AccuracyModule_high, (pA_3 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (pB_3 = Pnt_$ctor_Z7AD9E565(5, 5, 0), (vA_3 = Vec_$ctor_Z7AD9E565(0, 1, 0), (vB_3 = Vec_$ctor_Z7AD9E565(1, 0, 0), (vAx_1 = vA_3.X, (vAy_1 = vA_3.Y, (vAz_1 = vA_3.Z, (vBx_1 = vB_3.X, (vBy_1 = vB_3.Y, (vBz_1 = vB_3.Z, (crossX_1 = ((vAy_1 * vBz_1) - (vAz_1 * vBy_1)), (crossY_1 = ((vAz_1 * vBx_1) - (vAx_1 * vBz_1)), (crossZ_1 = ((vAx_1 * vBy_1) - (vAy_1 * vBx_1)), (dx_1 = (pB_3.X - pA_3.X), (dy_1 = (pB_3.Y - pA_3.Y), (dz_1 = (pB_3.Z - pA_3.Z), (((((dy_1 * vBz_1) - (dz_1 * vBy_1)) * crossX_1) + (((dz_1 * vBx_1) - (dx_1 * vBz_1)) * crossY_1)) + (((dx_1 * vBy_1) - (dy_1 * vBx_1)) * crossZ_1)) / (((crossX_1 * crossX_1) + (crossY_1 * crossY_1)) + (crossZ_1 * crossZ_1)))))))))))))))))), 5, "Parameter should be 5.0");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew lines in 3D", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let pA_5, pB_5, vA_5, vB_5, vAx_2, vAy_2, vAz_2, vBx_2, vBy_2, vBz_2, crossX_2, crossY_2, crossZ_2, dx_2, dy_2, dz_2;
        Expect_floatClose(AccuracyModule_high, (pA_5 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (pB_5 = Pnt_$ctor_Z7AD9E565(1, 0, 1), (vA_5 = Vec_$ctor_Z7AD9E565(1, 0, 0), (vB_5 = Vec_$ctor_Z7AD9E565(0, 1, 0), (vAx_2 = vA_5.X, (vAy_2 = vA_5.Y, (vAz_2 = vA_5.Z, (vBx_2 = vB_5.X, (vBy_2 = vB_5.Y, (vBz_2 = vB_5.Z, (crossX_2 = ((vAy_2 * vBz_2) - (vAz_2 * vBy_2)), (crossY_2 = ((vAz_2 * vBx_2) - (vAx_2 * vBz_2)), (crossZ_2 = ((vAx_2 * vBy_2) - (vAy_2 * vBx_2)), (dx_2 = (pB_5.X - pA_5.X), (dy_2 = (pB_5.Y - pA_5.Y), (dz_2 = (pB_5.Z - pA_5.Z), (((((dy_2 * vBz_2) - (dz_2 * vBy_2)) * crossX_2) + (((dz_2 * vBx_2) - (dx_2 * vBz_2)) * crossY_2)) + (((dx_2 * vBy_2) - (dy_2 * vBx_2)) * crossZ_2)) / (((crossX_2 * crossX_2) + (crossY_2 * crossY_2)) + (crossZ_2 * crossZ_2)))))))))))))))))), 1, "Parameter should be 1.0 for closest point");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines - returns NaN or Infinity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let t_3;
        const pA_7 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pB_7 = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        const vA_7 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        const vB_7 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        const vAx_3 = vA_7.X;
        const vAy_3 = vA_7.Y;
        const vAz_3 = vA_7.Z;
        const vBx_3 = vB_7.X;
        const vBy_3 = vB_7.Y;
        const vBz_3 = vB_7.Z;
        const crossX_3 = (vAy_3 * vBz_3) - (vAz_3 * vBy_3);
        const crossY_3 = (vAz_3 * vBx_3) - (vAx_3 * vBz_3);
        const crossZ_3 = (vAx_3 * vBy_3) - (vAy_3 * vBx_3);
        const dx_3 = pB_7.X - pA_7.X;
        const dy_3 = pB_7.Y - pA_7.Y;
        const dz_3 = pB_7.Z - pA_7.Z;
        t_3 = ((((((dy_3 * vBz_3) - (dz_3 * vBy_3)) * crossX_3) + (((dz_3 * vBx_3) - (dx_3 * vBz_3)) * crossY_3)) + (((dx_3 * vBy_3) - (dy_3 * vBx_3)) * crossZ_3)) / (((crossX_3 * crossX_3) + (crossY_3 * crossY_3)) + (crossZ_3 * crossZ_3)));
        Expect_isTrue(Number.isNaN(t_3) ? true : isInfinity(t_3))("Should return NaN or Infinity for parallel lines");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Line3D overload - intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let lineA_1, lineB_1, vAx_4, ln, vAy_4, ln_1, vAz_4, ln_2, vBx_4, ln_3, vBy_4, ln_4, vBz_4, ln_5, crossX_4, crossY_4, crossZ_4, dx_4, dy_4, dz_4;
        Expect_floatClose(AccuracyModule_high, (lineA_1 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_1 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0)), (vAx_4 = ((ln = lineA_1, ln.ToX - ln.FromX)), (vAy_4 = ((ln_1 = lineA_1, ln_1.ToY - ln_1.FromY)), (vAz_4 = ((ln_2 = lineA_1, ln_2.ToZ - ln_2.FromZ)), (vBx_4 = ((ln_3 = lineB_1, ln_3.ToX - ln_3.FromX)), (vBy_4 = ((ln_4 = lineB_1, ln_4.ToY - ln_4.FromY)), (vBz_4 = ((ln_5 = lineB_1, ln_5.ToZ - ln_5.FromZ)), (crossX_4 = ((vAy_4 * vBz_4) - (vAz_4 * vBy_4)), (crossY_4 = ((vAz_4 * vBx_4) - (vAx_4 * vBz_4)), (crossZ_4 = ((vAx_4 * vBy_4) - (vAy_4 * vBx_4)), (dx_4 = (lineB_1.FromX - lineA_1.FromX), (dy_4 = (lineB_1.FromY - lineA_1.FromY), (dz_4 = (lineB_1.FromZ - lineA_1.FromZ), (((((dy_4 * vBz_4) - (dz_4 * vBy_4)) * crossX_4) + (((dz_4 * vBx_4) - (dx_4 * vBz_4)) * crossY_4)) + (((dx_4 * vBy_4) - (dy_4 * vBx_4)) * crossZ_4)) / (((crossX_4 * crossX_4) + (crossY_4 * crossY_4)) + (crossZ_4 * crossZ_4)))))))))))))))), 0.5, "Parameter should be 0.5");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D crossing lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let pA_9, pB_9, vA_9, vB_9, vAx_5, vAy_5, vAz_5, vBx_5, vBy_5, vBz_5, crossX_5, crossY_5, crossZ_5, dx_5, dy_5, dz_5;
        Expect_floatClose(AccuracyModule_high, (pA_9 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (pB_9 = Pnt_$ctor_Z7AD9E565(0, 0, 5), (vA_9 = Vec_$ctor_Z7AD9E565(0, 0, 1), (vB_9 = Vec_$ctor_Z7AD9E565(1, 0, 0), (vAx_5 = vA_9.X, (vAy_5 = vA_9.Y, (vAz_5 = vA_9.Z, (vBx_5 = vB_9.X, (vBy_5 = vB_9.Y, (vBz_5 = vB_9.Z, (crossX_5 = ((vAy_5 * vBz_5) - (vAz_5 * vBy_5)), (crossY_5 = ((vAz_5 * vBx_5) - (vAx_5 * vBz_5)), (crossZ_5 = ((vAx_5 * vBy_5) - (vAy_5 * vBx_5)), (dx_5 = (pB_9.X - pA_9.X), (dy_5 = (pB_9.Y - pA_9.Y), (dz_5 = (pB_9.Z - pA_9.Z), (((((dy_5 * vBz_5) - (dz_5 * vBy_5)) * crossX_5) + (((dz_5 * vBx_5) - (dx_5 * vBz_5)) * crossY_5)) + (((dx_5 * vBy_5) - (dy_5 * vBx_5)) * crossZ_5)) / (((crossX_5 * crossX_5) + (crossY_5 * crossY_5)) + (crossZ_5 * crossZ_5)))))))))))))))))), 5, "Parameter should be 5.0");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})()])), Test_testList("parameters tests", ofArray([(() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines - basic", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let patternInput;
        const pA_11 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pB_11 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const vA_11 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        const vB_11 = Vec_$ctor_Z7AD9E565(0, 1, 0);
        const vAx_6 = vA_11.X;
        const vAy_6 = vA_11.Y;
        const vAz_6 = vA_11.Z;
        const vBx_6 = vB_11.X;
        const vBy_6 = vB_11.Y;
        const vBz_6 = vB_11.Z;
        const crossX_6 = (vAy_6 * vBz_6) - (vAz_6 * vBy_6);
        const crossY_6 = (vAz_6 * vBx_6) - (vAx_6 * vBz_6);
        const crossZ_6 = (vAx_6 * vBy_6) - (vAy_6 * vBx_6);
        const crossMagSq_6 = ((crossX_6 * crossX_6) + (crossY_6 * crossY_6)) + (crossZ_6 * crossZ_6);
        const dx_6 = pB_11.X - pA_11.X;
        const dy_6 = pB_11.Y - pA_11.Y;
        const dz_6 = pB_11.Z - pA_11.Z;
        patternInput = [(((((dy_6 * vBz_6) - (dz_6 * vBy_6)) * crossX_6) + (((dz_6 * vBx_6) - (dx_6 * vBz_6)) * crossY_6)) + (((dx_6 * vBy_6) - (dy_6 * vBx_6)) * crossZ_6)) / crossMagSq_6, (((((dy_6 * vAz_6) - (dz_6 * vAy_6)) * crossX_6) + (((dz_6 * vAx_6) - (dx_6 * vAz_6)) * crossY_6)) + (((dx_6 * vAy_6) - (dy_6 * vAx_6)) * crossZ_6)) / crossMagSq_6];
        Expect_floatClose(AccuracyModule_high, patternInput[0], 0, "Parameter t should be 0.0");
        Expect_floatClose(AccuracyModule_high, patternInput[1], 0, "Parameter u should be 0.0");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew lines - closest approach", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let patternInput_1;
        const pA_13 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pB_13 = Pnt_$ctor_Z7AD9E565(1, 1, 1);
        const vA_13 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        const vB_13 = Vec_$ctor_Z7AD9E565(0, 1, 0);
        const vAx_7 = vA_13.X;
        const vAy_7 = vA_13.Y;
        const vAz_7 = vA_13.Z;
        const vBx_7 = vB_13.X;
        const vBy_7 = vB_13.Y;
        const vBz_7 = vB_13.Z;
        const crossX_7 = (vAy_7 * vBz_7) - (vAz_7 * vBy_7);
        const crossY_7 = (vAz_7 * vBx_7) - (vAx_7 * vBz_7);
        const crossZ_7 = (vAx_7 * vBy_7) - (vAy_7 * vBx_7);
        const crossMagSq_7 = ((crossX_7 * crossX_7) + (crossY_7 * crossY_7)) + (crossZ_7 * crossZ_7);
        const dx_7 = pB_13.X - pA_13.X;
        const dy_7 = pB_13.Y - pA_13.Y;
        const dz_7 = pB_13.Z - pA_13.Z;
        patternInput_1 = [(((((dy_7 * vBz_7) - (dz_7 * vBy_7)) * crossX_7) + (((dz_7 * vBx_7) - (dx_7 * vBz_7)) * crossY_7)) + (((dx_7 * vBy_7) - (dy_7 * vBx_7)) * crossZ_7)) / crossMagSq_7, (((((dy_7 * vAz_7) - (dz_7 * vAy_7)) * crossX_7) + (((dz_7 * vAx_7) - (dx_7 * vAz_7)) * crossY_7)) + (((dx_7 * vAy_7) - (dy_7 * vAx_7)) * crossZ_7)) / crossMagSq_7];
        Expect_floatClose(AccuracyModule_medium, patternInput_1[0], 1, "Parameter t should be 1.0");
        Expect_floatClose(AccuracyModule_medium, patternInput_1[1], -1, "Parameter u should be -1.0");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines - returns NaN or Infinity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let pA_15, pB_15, vA_15, vB_15, vAx_8, vAy_8, vAz_8, vBx_8, vBy_8, vBz_8, crossX_8, crossY_8, crossZ_8, crossMagSq_8, dx_8, dy_8, dz_8;
        const t_11 = ((pA_15 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (pB_15 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (vA_15 = Vec_$ctor_Z7AD9E565(1, 0, 0), (vB_15 = Vec_$ctor_Z7AD9E565(1, 0, 0), (vAx_8 = vA_15.X, (vAy_8 = vA_15.Y, (vAz_8 = vA_15.Z, (vBx_8 = vB_15.X, (vBy_8 = vB_15.Y, (vBz_8 = vB_15.Z, (crossX_8 = ((vAy_8 * vBz_8) - (vAz_8 * vBy_8)), (crossY_8 = ((vAz_8 * vBx_8) - (vAx_8 * vBz_8)), (crossZ_8 = ((vAx_8 * vBy_8) - (vAy_8 * vBx_8)), (crossMagSq_8 = (((crossX_8 * crossX_8) + (crossY_8 * crossY_8)) + (crossZ_8 * crossZ_8)), (dx_8 = (pB_15.X - pA_15.X), (dy_8 = (pB_15.Y - pA_15.Y), (dz_8 = (pB_15.Z - pA_15.Z), [(((((dy_8 * vBz_8) - (dz_8 * vBy_8)) * crossX_8) + (((dz_8 * vBx_8) - (dx_8 * vBz_8)) * crossY_8)) + (((dx_8 * vBy_8) - (dy_8 * vBx_8)) * crossZ_8)) / crossMagSq_8, (((((dy_8 * vAz_8) - (dz_8 * vAy_8)) * crossX_8) + (((dz_8 * vAx_8) - (dx_8 * vAz_8)) * crossY_8)) + (((dx_8 * vAy_8) - (dy_8 * vAx_8)) * crossZ_8)) / crossMagSq_8]))))))))))))))))))[0];
        Expect_isTrue(Number.isNaN(t_11) ? true : isInfinity(t_11))("Parallel lines should give NaN or Infinity");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Line3D overload", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let patternInput_3;
        const lineA_3 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_3 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        let vAx_9;
        const ln_6 = lineA_3;
        vAx_9 = (ln_6.ToX - ln_6.FromX);
        let vAy_9;
        const ln_7 = lineA_3;
        vAy_9 = (ln_7.ToY - ln_7.FromY);
        let vAz_9;
        const ln_8 = lineA_3;
        vAz_9 = (ln_8.ToZ - ln_8.FromZ);
        let vBx_9;
        const ln_9 = lineB_3;
        vBx_9 = (ln_9.ToX - ln_9.FromX);
        let vBy_9;
        const ln_10 = lineB_3;
        vBy_9 = (ln_10.ToY - ln_10.FromY);
        let vBz_9;
        const ln_11 = lineB_3;
        vBz_9 = (ln_11.ToZ - ln_11.FromZ);
        const crossX_9 = (vAy_9 * vBz_9) - (vAz_9 * vBy_9);
        const crossY_9 = (vAz_9 * vBx_9) - (vAx_9 * vBz_9);
        const crossZ_9 = (vAx_9 * vBy_9) - (vAy_9 * vBx_9);
        const crossMagSq_9 = ((crossX_9 * crossX_9) + (crossY_9 * crossY_9)) + (crossZ_9 * crossZ_9);
        const dx_9 = lineB_3.FromX - lineA_3.FromX;
        const dy_9 = lineB_3.FromY - lineA_3.FromY;
        const dz_9 = lineB_3.FromZ - lineA_3.FromZ;
        patternInput_3 = [(((((dy_9 * vBz_9) - (dz_9 * vBy_9)) * crossX_9) + (((dz_9 * vBx_9) - (dx_9 * vBz_9)) * crossY_9)) + (((dx_9 * vBy_9) - (dy_9 * vBx_9)) * crossZ_9)) / crossMagSq_9, (((((dy_9 * vAz_9) - (dz_9 * vAy_9)) * crossX_9) + (((dz_9 * vAx_9) - (dx_9 * vAz_9)) * crossY_9)) + (((dx_9 * vAy_9) - (dy_9 * vAx_9)) * crossZ_9)) / crossMagSq_9];
        Expect_floatClose(AccuracyModule_high, patternInput_3[0], 0.5, "Parameter t should be 0.5");
        Expect_floatClose(AccuracyModule_high, patternInput_3[1], 0.5, "Parameter u should be 0.5");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})()])), Test_testList("doOverlap tests", ofArray([(() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel overlapping lines on same ray", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let lineA_5, lineB_5, ln_12, ln_13, ln_14, ln_15, ln_16, ln_17;
        Expect_isTrue((lineA_5 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_5 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 0), Pnt_$ctor_Z7AD9E565(15, 0, 0)), XLine3D_doOverlap_6B19E37B(lineA_5.FromX, lineA_5.FromY, lineA_5.FromZ, lineB_5.FromX, lineB_5.FromY, lineB_5.FromZ, (ln_12 = lineA_5, ln_12.ToX - ln_12.FromX), (ln_13 = lineA_5, ln_13.ToY - ln_13.FromY), (ln_14 = lineA_5, ln_14.ToZ - ln_14.FromZ), (ln_15 = lineB_5, ln_15.ToX - ln_15.FromX), (ln_16 = lineB_5, ln_16.ToY - ln_16.FromY), (ln_17 = lineB_5, ln_17.ToZ - ln_17.FromZ), 1E-06))))("Overlapping coincident lines should return true");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel non-overlapping lines on same ray", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let lineA_7, lineB_7, ln_18, ln_19, ln_20, ln_21, ln_22, ln_23;
        Expect_isFalse((lineA_7 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(5, 0, 0)), (lineB_7 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(10, 0, 0), Pnt_$ctor_Z7AD9E565(15, 0, 0)), XLine3D_doOverlap_6B19E37B(lineA_7.FromX, lineA_7.FromY, lineA_7.FromZ, lineB_7.FromX, lineB_7.FromY, lineB_7.FromZ, (ln_18 = lineA_7, ln_18.ToX - ln_18.FromX), (ln_19 = lineA_7, ln_19.ToY - ln_19.FromY), (ln_20 = lineA_7, ln_20.ToZ - ln_20.FromZ), (ln_21 = lineB_7, ln_21.ToX - ln_21.FromX), (ln_22 = lineB_7, ln_22.ToY - ln_22.FromY), (ln_23 = lineB_7, ln_23.ToZ - ln_23.FromZ), 1E-06))))("Non-overlapping coincident lines should return false");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines touching at end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let lineA_9, lineB_9, ln_24, ln_25, ln_26, ln_27, ln_28, ln_29;
        Expect_isTrue((lineA_9 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(5, 0, 0)), (lineB_9 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), XLine3D_doOverlap_6B19E37B(lineA_9.FromX, lineA_9.FromY, lineA_9.FromZ, lineB_9.FromX, lineB_9.FromY, lineB_9.FromZ, (ln_24 = lineA_9, ln_24.ToX - ln_24.FromX), (ln_25 = lineA_9, ln_25.ToY - ln_25.FromY), (ln_26 = lineA_9, ln_26.ToZ - ln_26.FromZ), (ln_27 = lineB_9, ln_27.ToX - ln_27.FromX), (ln_28 = lineB_9, ln_28.ToY - ln_28.FromY), (ln_29 = lineB_9, ln_29.ToZ - ln_29.FromZ), 1E-06))))("Touching lines should return true");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines on different rays", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let lineA_11, lineB_11, ln_30, ln_31, ln_32, ln_33, ln_34, ln_35;
        Expect_isFalse((lineA_11 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_11 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), XLine3D_doOverlap_6B19E37B(lineA_11.FromX, lineA_11.FromY, lineA_11.FromZ, lineB_11.FromX, lineB_11.FromY, lineB_11.FromZ, (ln_30 = lineA_11, ln_30.ToX - ln_30.FromX), (ln_31 = lineA_11, ln_31.ToY - ln_31.FromY), (ln_32 = lineA_11, ln_32.ToZ - ln_32.FromZ), (ln_33 = lineB_11, ln_33.ToX - ln_33.FromX), (ln_34 = lineB_11, ln_34.ToY - ln_34.FromY), (ln_35 = lineB_11, ln_35.ToZ - ln_35.FromZ), 1E-06))))("Parallel non-coincident lines should return false");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D parallel lines on same ray", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let lineA_13, lineB_13, ln_36, ln_37, ln_38, ln_39, ln_40, ln_41;
        Expect_isTrue((lineA_13 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(5, 5, 5)), (lineB_13 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(3, 3, 3), Pnt_$ctor_Z7AD9E565(8, 8, 8)), XLine3D_doOverlap_6B19E37B(lineA_13.FromX, lineA_13.FromY, lineA_13.FromZ, lineB_13.FromX, lineB_13.FromY, lineB_13.FromZ, (ln_36 = lineA_13, ln_36.ToX - ln_36.FromX), (ln_37 = lineA_13, ln_37.ToY - ln_37.FromY), (ln_38 = lineA_13, ln_38.ToZ - ln_38.FromZ), (ln_39 = lineB_13, ln_39.ToX - ln_39.FromX), (ln_40 = lineB_13, ln_40.ToY - ln_40.FromY), (ln_41 = lineB_13, ln_41.ToZ - ln_41.FromZ), 1E-06))))("Overlapping 3D coincident lines should return true");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})()])), Test_testList("tryIntersectRay tests", ofArray([(() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rays intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let ln_42, ln_43, ln_44, ln_45, ln_46, ln_47;
        let result_5;
        const lineA_15 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_15 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        result_5 = XLine3D_tryIntersectRay_ZA268E31(lineA_15.FromX, lineA_15.FromY, lineA_15.FromZ, lineB_15.FromX, lineB_15.FromY, lineB_15.FromZ, (ln_42 = lineA_15, ln_42.ToX - ln_42.FromX), (ln_43 = lineA_15, ln_43.ToY - ln_43.FromY), (ln_44 = lineA_15, ln_44.ToZ - ln_44.FromZ), (ln_45 = lineB_15, ln_45.ToX - ln_45.FromX), (ln_46 = lineB_15, ln_46.ToY - ln_46.FromY), (ln_47 = lineB_15, ln_47.ToZ - ln_47.FromZ), 1E-06, 0.004363350820701567, 1E-06);
        if (result_5 == null) {
            Test_failtest("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
        else {
            const pt = result_5;
            Expect_floatClose(AccuracyModule_high, pt.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt.Y, 0, "Y should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt.Z, 0, "Z should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel rays", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let lineA_17, lineB_17, ln_48, ln_49, ln_50, ln_51, ln_52, ln_53;
        Expect_isNone((lineA_17 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_17 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), XLine3D_tryIntersectRay_ZA268E31(lineA_17.FromX, lineA_17.FromY, lineA_17.FromZ, lineB_17.FromX, lineB_17.FromY, lineB_17.FromZ, (ln_48 = lineA_17, ln_48.ToX - ln_48.FromX), (ln_49 = lineA_17, ln_49.ToY - ln_49.FromY), (ln_50 = lineA_17, ln_50.ToZ - ln_50.FromZ), (ln_51 = lineB_17, ln_51.ToX - ln_51.FromX), (ln_52 = lineB_17, ln_52.ToY - ln_52.FromY), (ln_53 = lineB_17, ln_53.ToZ - ln_53.FromZ), 1E-06, 0.004363350820701567, 1E-06))), "Parallel lines should return None");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew rays - returns closest if within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let ln_54, ln_55, ln_56, ln_57, ln_58, ln_59;
        let result_7;
        const lineA_19 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_19 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 0.001), Pnt_$ctor_Z7AD9E565(5, 10, 0.001));
        result_7 = XLine3D_tryIntersectRay_ZA268E31(lineA_19.FromX, lineA_19.FromY, lineA_19.FromZ, lineB_19.FromX, lineB_19.FromY, lineB_19.FromZ, (ln_54 = lineA_19, ln_54.ToX - ln_54.FromX), (ln_55 = lineA_19, ln_55.ToY - ln_55.FromY), (ln_56 = lineA_19, ln_56.ToZ - ln_56.FromZ), (ln_57 = lineB_19, ln_57.ToX - ln_57.FromX), (ln_58 = lineB_19, ln_58.ToY - ln_58.FromY), (ln_59 = lineB_19, ln_59.ToZ - ln_59.FromZ), 1E-05, 0.004363350820701567, 1E-06);
        if (result_7 == null) {
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
        else {
            Expect_floatClose(AccuracyModule_medium, result_7.X, 5, "X should be 5.0");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
    }));
})()])), Test_testList("tryIntersect tests", ofArray([(() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Lines intersecting within segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let ln_60, ln_61, ln_62, ln_63, ln_64, ln_65;
        let result_8;
        const lineA_21 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_21 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        result_8 = XLine3D_tryIntersect_ZA268E31(lineA_21.FromX, lineA_21.FromY, lineA_21.FromZ, lineB_21.FromX, lineB_21.FromY, lineB_21.FromZ, (ln_60 = lineA_21, ln_60.ToX - ln_60.FromX), (ln_61 = lineA_21, ln_61.ToY - ln_61.FromY), (ln_62 = lineA_21, ln_62.ToZ - ln_62.FromZ), (ln_63 = lineB_21, ln_63.ToX - ln_63.FromX), (ln_64 = lineB_21, ln_64.ToY - ln_64.FromY), (ln_65 = lineB_21, ln_65.ToZ - ln_65.FromZ), 1E-06, 0.004363350820701567, 1E-06);
        if (result_8 == null) {
            Test_failtest("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_18);
        }
        else {
            const pt_2 = result_8;
            Expect_floatClose(AccuracyModule_high, pt_2.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_2.Y, 0, "Y should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_2.Z, 0, "Z should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_18);
        }
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Lines not intersecting - parallel", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let lineA_23, lineB_23, ln_66, ln_67, ln_68, ln_69, ln_70, ln_71;
        Expect_isNone((lineA_23 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_23 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), XLine3D_tryIntersect_ZA268E31(lineA_23.FromX, lineA_23.FromY, lineA_23.FromZ, lineB_23.FromX, lineB_23.FromY, lineB_23.FromZ, (ln_66 = lineA_23, ln_66.ToX - ln_66.FromX), (ln_67 = lineA_23, ln_67.ToY - ln_67.FromY), (ln_68 = lineA_23, ln_68.ToZ - ln_68.FromZ), (ln_69 = lineB_23, ln_69.ToX - ln_69.FromX), (ln_70 = lineB_23, ln_70.ToY - ln_70.FromY), (ln_71 = lineB_23, ln_71.ToZ - ln_71.FromZ), 1E-06, 0.004363350820701567, 1E-06))), "Parallel lines should return None");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Lines not intersecting - apart", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let lineA_25, lineB_25, ln_72, ln_73, ln_74, ln_75, ln_76, ln_77;
        Expect_isNone((lineA_25 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), (lineB_25 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(2, -1, 0), Pnt_$ctor_Z7AD9E565(2, 1, 0)), XLine3D_tryIntersect_ZA268E31(lineA_25.FromX, lineA_25.FromY, lineA_25.FromZ, lineB_25.FromX, lineB_25.FromY, lineB_25.FromZ, (ln_72 = lineA_25, ln_72.ToX - ln_72.FromX), (ln_73 = lineA_25, ln_73.ToY - ln_73.FromY), (ln_74 = lineA_25, ln_74.ToZ - ln_74.FromZ), (ln_75 = lineB_25, ln_75.ToX - ln_75.FromX), (ln_76 = lineB_25, ln_76.ToY - ln_76.FromY), (ln_77 = lineB_25, ln_77.ToZ - ln_77.FromZ), 1E-06, 0.004363350820701567, 1E-06))), "Apart lines should return None");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D coplanar intersection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let ln_78, ln_79, ln_80, ln_81, ln_82, ln_83;
        let result_11;
        const lineA_27 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 5), Pnt_$ctor_Z7AD9E565(10, 0, 5));
        const lineB_27 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 5), Pnt_$ctor_Z7AD9E565(5, 5, 5));
        result_11 = XLine3D_tryIntersect_ZA268E31(lineA_27.FromX, lineA_27.FromY, lineA_27.FromZ, lineB_27.FromX, lineB_27.FromY, lineB_27.FromZ, (ln_78 = lineA_27, ln_78.ToX - ln_78.FromX), (ln_79 = lineA_27, ln_79.ToY - ln_79.FromY), (ln_80 = lineA_27, ln_80.ToZ - ln_80.FromZ), (ln_81 = lineB_27, ln_81.ToX - ln_81.FromX), (ln_82 = lineB_27, ln_82.ToY - ln_82.FromY), (ln_83 = lineB_27, ln_83.ToZ - ln_83.FromZ), 1E-06, 0.004363350820701567, 1E-06);
        if (result_11 == null) {
            Test_failtest("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_21);
        }
        else {
            const pt_3 = result_11;
            Expect_floatClose(AccuracyModule_high, pt_3.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_3.Y, 0, "Y should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_3.Z, 5, "Z should be 5.0");
            Test_TestCaseBuilder__Zero(builder$0040_21);
        }
    }));
})()])), Test_testList("getRayClosestParam tests", ofArray([(() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rays intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let ln_84, ln_85, ln_86, ln_87, ln_88, ln_89;
        let result_12;
        const lineA_29 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_29 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        result_12 = XLine3D_getRayClosestParam_Z1F58440C(lineA_29.FromX, lineA_29.FromY, lineA_29.FromZ, lineB_29.FromX, lineB_29.FromY, lineB_29.FromZ, (ln_84 = lineA_29, ln_84.ToX - ln_84.FromX), (ln_85 = lineA_29, ln_85.ToY - ln_85.FromY), (ln_86 = lineA_29, ln_86.ToZ - ln_86.FromZ), (ln_87 = lineB_29, ln_87.ToX - ln_87.FromX), (ln_88 = lineB_29, ln_88.ToY - ln_88.FromY), (ln_89 = lineB_29, ln_89.ToZ - ln_89.FromZ), 0.004363350820701567, 1E-06);
        if (result_12.tag === 0) {
            Expect_floatClose(AccuracyModule_high, result_12.fields[0], 0.5, "Parameter t should be 0.5");
            Expect_floatClose(AccuracyModule_high, result_12.fields[1], 0.5, "Parameter u should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_22);
        }
        else {
            Test_failtest("Should return SkewOrX");
            Test_TestCaseBuilder__Zero(builder$0040_22);
        }
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel rays", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let lineA_31, lineB_31, ln_90, ln_91, ln_92, ln_93, ln_94, ln_95;
        if (((lineA_31 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_31 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), XLine3D_getRayClosestParam_Z1F58440C(lineA_31.FromX, lineA_31.FromY, lineA_31.FromZ, lineB_31.FromX, lineB_31.FromY, lineB_31.FromZ, (ln_90 = lineA_31, ln_90.ToX - ln_90.FromX), (ln_91 = lineA_31, ln_91.ToY - ln_91.FromY), (ln_92 = lineA_31, ln_92.ToZ - ln_92.FromZ), (ln_93 = lineB_31, ln_93.ToX - ln_93.FromX), (ln_94 = lineB_31, ln_94.ToY - ln_94.FromY), (ln_95 = lineB_31, ln_95.ToZ - ln_95.FromZ), 0.004363350820701567, 1E-06)))).tag === 1) {
            Test_TestCaseBuilder__Zero(builder$0040_23);
        }
        else {
            Test_failtest("Should return Parallel");
            Test_TestCaseBuilder__Zero(builder$0040_23);
        }
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew rays in 3D", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let ln_96, ln_97, ln_98, ln_99, ln_100, ln_101;
        let result_14;
        const lineA_33 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_33 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 1), Pnt_$ctor_Z7AD9E565(5, 10, 1));
        result_14 = XLine3D_getRayClosestParam_Z1F58440C(lineA_33.FromX, lineA_33.FromY, lineA_33.FromZ, lineB_33.FromX, lineB_33.FromY, lineB_33.FromZ, (ln_96 = lineA_33, ln_96.ToX - ln_96.FromX), (ln_97 = lineA_33, ln_97.ToY - ln_97.FromY), (ln_98 = lineA_33, ln_98.ToZ - ln_98.FromZ), (ln_99 = lineB_33, ln_99.ToX - ln_99.FromX), (ln_100 = lineB_33, ln_100.ToY - ln_100.FromY), (ln_101 = lineB_33, ln_101.ToZ - ln_101.FromZ), 0.004363350820701567, 1E-06);
        if (result_14.tag === 0) {
            Expect_floatClose(AccuracyModule_high, result_14.fields[0], 0.5, "Parameter t should be 0.5");
            Expect_floatClose(AccuracyModule_high, result_14.fields[1], 0, "Parameter u should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_24);
        }
        else {
            Test_failtest("Should return SkewOrX for skew lines");
            Test_TestCaseBuilder__Zero(builder$0040_24);
        }
    }));
})()])), Test_testList("getRayIntersection tests", ofArray([(() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rays intersecting at specific point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let ln_102, ln_103, ln_104, ln_105, ln_106, ln_107;
        let result_15;
        const lineA_35 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_35 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        result_15 = XLine3D_getRayIntersection_ZA268E31(lineA_35.FromX, lineA_35.FromY, lineA_35.FromZ, lineB_35.FromX, lineB_35.FromY, lineB_35.FromZ, (ln_102 = lineA_35, ln_102.ToX - ln_102.FromX), (ln_103 = lineA_35, ln_103.ToY - ln_103.FromY), (ln_104 = lineA_35, ln_104.ToZ - ln_104.FromZ), (ln_105 = lineB_35, ln_105.ToX - ln_105.FromX), (ln_106 = lineB_35, ln_106.ToY - ln_106.FromY), (ln_107 = lineB_35, ln_107.ToZ - ln_107.FromZ), 1E-06, 0.004363350820701567, 1E-06);
        if (result_15.tag === 0) {
            const pt_4 = result_15.fields[0];
            Expect_floatClose(AccuracyModule_high, pt_4.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_4.Y, 0, "Y should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_4.Z, 0, "Z should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_25);
        }
        else {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_25);
        }
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew rays with small distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let ln_108, ln_109, ln_110, ln_111, ln_112, ln_113;
        let result_16;
        const lineA_37 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_37 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 0.001), Pnt_$ctor_Z7AD9E565(5, 10, 0.001));
        result_16 = XLine3D_getRayIntersection_ZA268E31(lineA_37.FromX, lineA_37.FromY, lineA_37.FromZ, lineB_37.FromX, lineB_37.FromY, lineB_37.FromZ, (ln_108 = lineA_37, ln_108.ToX - ln_108.FromX), (ln_109 = lineA_37, ln_109.ToY - ln_109.FromY), (ln_110 = lineA_37, ln_110.ToZ - ln_110.FromZ), (ln_111 = lineB_37, ln_111.ToX - ln_111.FromX), (ln_112 = lineB_37, ln_112.ToY - ln_112.FromY), (ln_113 = lineB_37, ln_113.ToZ - ln_113.FromZ), 1E-05, 0.004363350820701567, 1E-06);
        switch (result_16.tag) {
            case 0: {
                Expect_floatClose(AccuracyModule_medium, result_16.fields[0].X, 5, "X should be around 5.0");
                Test_TestCaseBuilder__Zero(builder$0040_26);
                break;
            }
            case 1: {
                Test_TestCaseBuilder__Zero(builder$0040_26);
                break;
            }
            default: {
                Test_failtest("Should be intersect or skew");
                Test_TestCaseBuilder__Zero(builder$0040_26);
            }
        }
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel rays", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let lineA_39, lineB_39, ln_114, ln_115, ln_116, ln_117, ln_118, ln_119;
        if (((lineA_39 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_39 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), XLine3D_getRayIntersection_ZA268E31(lineA_39.FromX, lineA_39.FromY, lineA_39.FromZ, lineB_39.FromX, lineB_39.FromY, lineB_39.FromZ, (ln_114 = lineA_39, ln_114.ToX - ln_114.FromX), (ln_115 = lineA_39, ln_115.ToY - ln_115.FromY), (ln_116 = lineA_39, ln_116.ToZ - ln_116.FromZ), (ln_117 = lineB_39, ln_117.ToX - ln_117.FromX), (ln_118 = lineB_39, ln_118.ToY - ln_118.FromY), (ln_119 = lineB_39, ln_119.ToZ - ln_119.FromZ), 1E-06, 0.004363350820701567, 1E-06)))).tag === 2) {
            Test_TestCaseBuilder__Zero(builder$0040_27);
        }
        else {
            Test_failtest("Should be parallel");
            Test_TestCaseBuilder__Zero(builder$0040_27);
        }
    }));
})()])), Test_testList("getIntersectionParam tests", ofArray([(() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let ln_120, ln_121, ln_122, ln_123, ln_124, ln_125;
        let result_18;
        const lnA = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lnB = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        result_18 = XLine3D_getIntersectionParam_ZA268E31(lnA.FromX, lnA.FromY, lnA.FromZ, lnB.FromX, lnB.FromY, lnB.FromZ, (ln_120 = lnA, ln_120.ToX - ln_120.FromX), (ln_121 = lnA, ln_121.ToY - ln_121.FromY), (ln_122 = lnA, ln_122.ToZ - ln_122.FromZ), (ln_123 = lnB, ln_123.ToX - ln_123.FromX), (ln_124 = lnB, ln_124.ToY - ln_124.FromY), (ln_125 = lnB, ln_125.ToZ - ln_125.FromZ), 1E-06, 0.004363350820701567, 1E-06);
        if (result_18.tag === 0) {
            Expect_floatClose(AccuracyModule_high, result_18.fields[0], 0.5, "Parameter t should be 0.5");
            Expect_floatClose(AccuracyModule_high, result_18.fields[1], 0.5, "Parameter u should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_28);
        }
        else {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_28);
        }
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines apart", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let lnA_1, lnB_1, ln_126, ln_127, ln_128, ln_129, ln_130, ln_131;
        if (((lnA_1 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), (lnB_1 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(2, -1, 0), Pnt_$ctor_Z7AD9E565(2, 1, 0)), XLine3D_getIntersectionParam_ZA268E31(lnA_1.FromX, lnA_1.FromY, lnA_1.FromZ, lnB_1.FromX, lnB_1.FromY, lnB_1.FromZ, (ln_126 = lnA_1, ln_126.ToX - ln_126.FromX), (ln_127 = lnA_1, ln_127.ToY - ln_127.FromY), (ln_128 = lnA_1, ln_128.ToZ - ln_128.FromZ), (ln_129 = lnB_1, ln_129.ToX - ln_129.FromX), (ln_130 = lnB_1, ln_130.ToY - ln_130.FromY), (ln_131 = lnB_1, ln_131.ToZ - ln_131.FromZ), 1E-06, 0.004363350820701567, 1E-06)))).tag === 2) {
            Test_TestCaseBuilder__Zero(builder$0040_29);
        }
        else {
            Test_failtest("Should be apart");
            Test_TestCaseBuilder__Zero(builder$0040_29);
        }
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines parallel", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let lnA_2, lnB_2, ln_132, ln_133, ln_134, ln_135, ln_136, ln_137;
        if (((lnA_2 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lnB_2 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), XLine3D_getIntersectionParam_ZA268E31(lnA_2.FromX, lnA_2.FromY, lnA_2.FromZ, lnB_2.FromX, lnB_2.FromY, lnB_2.FromZ, (ln_132 = lnA_2, ln_132.ToX - ln_132.FromX), (ln_133 = lnA_2, ln_133.ToY - ln_133.FromY), (ln_134 = lnA_2, ln_134.ToZ - ln_134.FromZ), (ln_135 = lnB_2, ln_135.ToX - ln_135.FromX), (ln_136 = lnB_2, ln_136.ToY - ln_136.FromY), (ln_137 = lnB_2, ln_137.ToZ - ln_137.FromZ), 1E-06, 0.004363350820701567, 1E-06)))).tag === 3) {
            Test_TestCaseBuilder__Zero(builder$0040_30);
        }
        else {
            Test_failtest("Should be parallel");
            Test_TestCaseBuilder__Zero(builder$0040_30);
        }
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew lines - closest within segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let ln_138, ln_139, ln_140, ln_141, ln_142, ln_143;
        let result_21;
        const lnA_3 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lnB_3 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 1), Pnt_$ctor_Z7AD9E565(5, 10, 1));
        result_21 = XLine3D_getIntersectionParam_ZA268E31(lnA_3.FromX, lnA_3.FromY, lnA_3.FromZ, lnB_3.FromX, lnB_3.FromY, lnB_3.FromZ, (ln_138 = lnA_3, ln_138.ToX - ln_138.FromX), (ln_139 = lnA_3, ln_139.ToY - ln_139.FromY), (ln_140 = lnA_3, ln_140.ToZ - ln_140.FromZ), (ln_141 = lnB_3, ln_141.ToX - ln_141.FromX), (ln_142 = lnB_3, ln_142.ToY - ln_142.FromY), (ln_143 = lnB_3, ln_143.ToZ - ln_143.FromZ), 2, 0.004363350820701567, 1E-06);
        switch (result_21.tag) {
            case 0: {
                Expect_floatClose(AccuracyModule_high, result_21.fields[0], 0.5, "Parameter t should be 0.5");
                Expect_floatClose(AccuracyModule_high, result_21.fields[1], 0, "Parameter u should be 0.0");
                Test_TestCaseBuilder__Zero(builder$0040_31);
                break;
            }
            case 1: {
                Expect_floatClose(AccuracyModule_high, result_21.fields[0], 0.5, "Parameter t should be 0.5");
                Expect_floatClose(AccuracyModule_high, result_21.fields[1], 0, "Parameter u should be 0.0");
                Expect_floatClose(AccuracyModule_high, result_21.fields[2], 1, "Squared distance should be 1.0");
                Test_TestCaseBuilder__Zero(builder$0040_31);
                break;
            }
            default: {
                Test_failtest("Should be skew or intersect");
                Test_TestCaseBuilder__Zero(builder$0040_31);
            }
        }
    }));
})()])), Test_testList("getIntersection tests", ofArray([(() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines intersecting - returns point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let ln_144, ln_145, ln_146, ln_147, ln_148, ln_149;
        let result_22;
        const lineA_45 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_45 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        result_22 = XLine3D_getIntersection_ZA268E31(lineA_45.FromX, lineA_45.FromY, lineA_45.FromZ, lineB_45.FromX, lineB_45.FromY, lineB_45.FromZ, (ln_144 = lineA_45, ln_144.ToX - ln_144.FromX), (ln_145 = lineA_45, ln_145.ToY - ln_145.FromY), (ln_146 = lineA_45, ln_146.ToZ - ln_146.FromZ), (ln_147 = lineB_45, ln_147.ToX - ln_147.FromX), (ln_148 = lineB_45, ln_148.ToY - ln_148.FromY), (ln_149 = lineB_45, ln_149.ToZ - ln_149.FromZ), 1E-06, 0.004363350820701567, 1E-06);
        if (result_22.tag === 0) {
            const pt_6 = result_22.fields[0];
            Expect_floatClose(AccuracyModule_high, pt_6.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_6.Y, 0, "Y should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_6.Z, 0, "Z should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_32);
        }
        else {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_32);
        }
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Finite lines apart", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let lineA_47, lineB_47, ln_150, ln_151, ln_152, ln_153, ln_154, ln_155;
        if (((lineA_47 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), (lineB_47 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(2, -1, 0), Pnt_$ctor_Z7AD9E565(2, 1, 0)), XLine3D_getIntersection_ZA268E31(lineA_47.FromX, lineA_47.FromY, lineA_47.FromZ, lineB_47.FromX, lineB_47.FromY, lineB_47.FromZ, (ln_150 = lineA_47, ln_150.ToX - ln_150.FromX), (ln_151 = lineA_47, ln_151.ToY - ln_151.FromY), (ln_152 = lineA_47, ln_152.ToZ - ln_152.FromZ), (ln_153 = lineB_47, ln_153.ToX - ln_153.FromX), (ln_154 = lineB_47, ln_154.ToY - ln_154.FromY), (ln_155 = lineB_47, ln_155.ToZ - ln_155.FromZ), 1E-06, 0.004363350820701567, 1E-06)))).tag === 2) {
            Test_TestCaseBuilder__Zero(builder$0040_33);
        }
        else {
            Test_failtest("Should be apart");
            Test_TestCaseBuilder__Zero(builder$0040_33);
        }
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew lines - returns closest points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let ln_156, ln_157, ln_158, ln_159, ln_160, ln_161;
        let result_24;
        const lineA_49 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_49 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 1), Pnt_$ctor_Z7AD9E565(5, 10, 1));
        result_24 = XLine3D_getIntersection_ZA268E31(lineA_49.FromX, lineA_49.FromY, lineA_49.FromZ, lineB_49.FromX, lineB_49.FromY, lineB_49.FromZ, (ln_156 = lineA_49, ln_156.ToX - ln_156.FromX), (ln_157 = lineA_49, ln_157.ToY - ln_157.FromY), (ln_158 = lineA_49, ln_158.ToZ - ln_158.FromZ), (ln_159 = lineB_49, ln_159.ToX - ln_159.FromX), (ln_160 = lineB_49, ln_160.ToY - ln_160.FromY), (ln_161 = lineB_49, ln_161.ToZ - ln_161.FromZ), 2, 0.004363350820701567, 1E-06);
        switch (result_24.tag) {
            case 0: {
                Expect_floatClose(AccuracyModule_high, result_24.fields[0].X, 5, "X should be around 5.0");
                Test_TestCaseBuilder__Zero(builder$0040_34);
                break;
            }
            case 1: {
                Expect_floatClose(AccuracyModule_high, result_24.fields[0].X, 5, "ptA X should be 5.0");
                Expect_floatClose(AccuracyModule_high, result_24.fields[1].Z, 1, "ptB Z should be 1.0");
                Expect_floatClose(AccuracyModule_high, result_24.fields[2], 1, "Squared distance should be 1.0");
                Test_TestCaseBuilder__Zero(builder$0040_34);
                break;
            }
            default: {
                Test_failtest("Should be skew or intersect");
                Test_TestCaseBuilder__Zero(builder$0040_34);
            }
        }
    }));
})()])), Test_testList("getClosestParameters tests", ofArray([(() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        let ln_162, ln_163, ln_164, ln_165, ln_166, ln_167;
        let result_25;
        const lineA_51 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_51 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        result_25 = XLine3D_getClosestParameters_Z1F58440C(lineA_51.FromX, lineA_51.FromY, lineA_51.FromZ, lineB_51.FromX, lineB_51.FromY, lineB_51.FromZ, (ln_162 = lineA_51, ln_162.ToX - ln_162.FromX), (ln_163 = lineA_51, ln_163.ToY - ln_163.FromY), (ln_164 = lineA_51, ln_164.ToZ - ln_164.FromZ), (ln_165 = lineB_51, ln_165.ToX - ln_165.FromX), (ln_166 = lineB_51, ln_166.ToY - ln_166.FromY), (ln_167 = lineB_51, ln_167.ToZ - ln_167.FromZ), 0.004363350820701567, 1E-06);
        if (result_25.tag === 0) {
            Expect_floatClose(AccuracyModule_high, result_25.fields[0], 0.5, "Parameter t should be 0.5");
            Expect_floatClose(AccuracyModule_high, result_25.fields[1], 0.5, "Parameter u should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_35);
        }
        else {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_35);
        }
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Apart lines - closest points at endpoints", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let ln_168, ln_169, ln_170, ln_171, ln_172, ln_173;
        let result_26;
        const lineA_53 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0));
        const lineB_53 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(1, 2, 0), Pnt_$ctor_Z7AD9E565(3, 3, 0));
        result_26 = XLine3D_getClosestParameters_Z1F58440C(lineA_53.FromX, lineA_53.FromY, lineA_53.FromZ, lineB_53.FromX, lineB_53.FromY, lineB_53.FromZ, (ln_168 = lineA_53, ln_168.ToX - ln_168.FromX), (ln_169 = lineA_53, ln_169.ToY - ln_169.FromY), (ln_170 = lineA_53, ln_170.ToZ - ln_170.FromZ), (ln_171 = lineB_53, ln_171.ToX - ln_171.FromX), (ln_172 = lineB_53, ln_172.ToY - ln_172.FromY), (ln_173 = lineB_53, ln_173.ToZ - ln_173.FromZ), 0.004363350820701567, 1E-06);
        if (result_26.tag === 3) {
            const dist_2 = Math.sqrt(result_26.fields[2]);
            Expect_floatClose(AccuracyModule_high, result_26.fields[0], 1, "Parameter t should be 1.0");
            Expect_floatClose(AccuracyModule_high, dist_2, 2, "Distance should be 2.0");
            Test_TestCaseBuilder__Zero(builder$0040_36);
        }
        else {
            Test_failtest("Should be apart");
            Test_TestCaseBuilder__Zero(builder$0040_36);
        }
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let ln_174, ln_175, ln_176, ln_177, ln_178, ln_179;
        let result_27;
        const lineA_55 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_55 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0));
        result_27 = XLine3D_getClosestParameters_Z1F58440C(lineA_55.FromX, lineA_55.FromY, lineA_55.FromZ, lineB_55.FromX, lineB_55.FromY, lineB_55.FromZ, (ln_174 = lineA_55, ln_174.ToX - ln_174.FromX), (ln_175 = lineA_55, ln_175.ToY - ln_175.FromY), (ln_176 = lineA_55, ln_176.ToZ - ln_176.FromZ), (ln_177 = lineB_55, ln_177.ToX - ln_177.FromX), (ln_178 = lineB_55, ln_178.ToY - ln_178.FromY), (ln_179 = lineB_55, ln_179.ToZ - ln_179.FromZ), 0.004363350820701567, 1E-06);
        if (result_27.tag === 2) {
            Expect_floatClose(AccuracyModule_medium, result_27.fields[0], 0.5, "Parameter t should be around 0.5");
            Expect_floatClose(AccuracyModule_medium, result_27.fields[1], 0.5, "Parameter u should be around 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_37);
        }
        else {
            Test_failtest("Should be parallel");
            Test_TestCaseBuilder__Zero(builder$0040_37);
        }
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew lines in 3D", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let ln_180, ln_181, ln_182, ln_183, ln_184, ln_185;
        let result_28;
        const lineA_57 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_57 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 1), Pnt_$ctor_Z7AD9E565(5, 10, 1));
        result_28 = XLine3D_getClosestParameters_Z1F58440C(lineA_57.FromX, lineA_57.FromY, lineA_57.FromZ, lineB_57.FromX, lineB_57.FromY, lineB_57.FromZ, (ln_180 = lineA_57, ln_180.ToX - ln_180.FromX), (ln_181 = lineA_57, ln_181.ToY - ln_181.FromY), (ln_182 = lineA_57, ln_182.ToZ - ln_182.FromZ), (ln_183 = lineB_57, ln_183.ToX - ln_183.FromX), (ln_184 = lineB_57, ln_184.ToY - ln_184.FromY), (ln_185 = lineB_57, ln_185.ToZ - ln_185.FromZ), 0.004363350820701567, 1E-06);
        if (result_28.tag === 1) {
            Expect_floatClose(AccuracyModule_high, result_28.fields[0], 0.5, "Parameter t should be 0.5");
            Expect_floatClose(AccuracyModule_high, result_28.fields[1], 0, "Parameter u should be 0.0");
            Expect_floatClose(AccuracyModule_high, result_28.fields[2], 1, "Squared distance should be 1.0");
            Test_TestCaseBuilder__Zero(builder$0040_38);
        }
        else {
            Test_failtest("Should be skew");
            Test_TestCaseBuilder__Zero(builder$0040_38);
        }
    }));
})()])), Test_testList("getClosestPoints tests", ofArray([(() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let ln_186, ln_187, ln_188, ln_189, ln_190, ln_191;
        let result_29;
        const lineA_59 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_59 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        result_29 = XLine3D_getClosestPoints_Z1F58440C(lineA_59.FromX, lineA_59.FromY, lineA_59.FromZ, lineB_59.FromX, lineB_59.FromY, lineB_59.FromZ, (ln_186 = lineA_59, ln_186.ToX - ln_186.FromX), (ln_187 = lineA_59, ln_187.ToY - ln_187.FromY), (ln_188 = lineA_59, ln_188.ToZ - ln_188.FromZ), (ln_189 = lineB_59, ln_189.ToX - ln_189.FromX), (ln_190 = lineB_59, ln_190.ToY - ln_190.FromY), (ln_191 = lineB_59, ln_191.ToZ - ln_191.FromZ), 0.004363350820701567, 1E-06);
        if (result_29.tag === 0) {
            const pt_8 = result_29.fields[0];
            Expect_floatClose(AccuracyModule_high, pt_8.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_8.Y, 0, "Y should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_8.Z, 0, "Z should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_39);
        }
        else {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_39);
        }
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let ln_192, ln_193, ln_194, ln_195, ln_196, ln_197, a_1, b_1, x, y, z;
        let result_30;
        const lineA_61 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_61 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0));
        result_30 = XLine3D_getClosestPoints_Z1F58440C(lineA_61.FromX, lineA_61.FromY, lineA_61.FromZ, lineB_61.FromX, lineB_61.FromY, lineB_61.FromZ, (ln_192 = lineA_61, ln_192.ToX - ln_192.FromX), (ln_193 = lineA_61, ln_193.ToY - ln_193.FromY), (ln_194 = lineA_61, ln_194.ToZ - ln_194.FromZ), (ln_195 = lineB_61, ln_195.ToX - ln_195.FromX), (ln_196 = lineB_61, ln_196.ToY - ln_196.FromY), (ln_197 = lineB_61, ln_197.ToZ - ln_197.FromZ), 0.004363350820701567, 1E-06);
        if (result_30.tag === 2) {
            Expect_floatClose(AccuracyModule_medium, (a_1 = result_30.fields[0], (b_1 = result_30.fields[1], (x = (a_1.X - b_1.X), (y = (a_1.Y - b_1.Y), (z = (a_1.Z - b_1.Z), Math.sqrt(((x * x) + (y * y)) + (z * z))))))), 1, "Distance should be 1.0");
            Test_TestCaseBuilder__Zero(builder$0040_40);
        }
        else {
            Test_failtest("Should be parallel");
            Test_TestCaseBuilder__Zero(builder$0040_40);
        }
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let ln_198, ln_199, ln_200, ln_201, ln_202, ln_203;
        let result_31;
        const lineA_63 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_63 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 1), Pnt_$ctor_Z7AD9E565(5, 10, 1));
        result_31 = XLine3D_getClosestPoints_Z1F58440C(lineA_63.FromX, lineA_63.FromY, lineA_63.FromZ, lineB_63.FromX, lineB_63.FromY, lineB_63.FromZ, (ln_198 = lineA_63, ln_198.ToX - ln_198.FromX), (ln_199 = lineA_63, ln_199.ToY - ln_199.FromY), (ln_200 = lineA_63, ln_200.ToZ - ln_200.FromZ), (ln_201 = lineB_63, ln_201.ToX - ln_201.FromX), (ln_202 = lineB_63, ln_202.ToY - ln_202.FromY), (ln_203 = lineB_63, ln_203.ToZ - ln_203.FromZ), 0.004363350820701567, 1E-06);
        if (result_31.tag === 1) {
            Expect_floatClose(AccuracyModule_high, result_31.fields[0].X, 5, "ptA X should be 5.0");
            Expect_floatClose(AccuracyModule_high, result_31.fields[1].X, 5, "ptB X should be 5.0");
            Expect_floatClose(AccuracyModule_high, result_31.fields[2], 1, "Squared distance should be 1.0");
            Test_TestCaseBuilder__Zero(builder$0040_41);
        }
        else {
            Test_failtest("Should be skew");
            Test_TestCaseBuilder__Zero(builder$0040_41);
        }
    }));
})()])), Test_testList("getSqDistance tests", ofArray([(() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting lines have zero distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let lineA_65, lineB_65, ln_204, ln_205, ln_206, ln_207, ln_208, ln_209;
        Expect_floatClose(AccuracyModule_high, (lineA_65 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_65 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0)), XLine3D_getSqDistance_Z15A9A3C0(lineA_65.FromX, lineA_65.FromY, lineA_65.FromZ, lineB_65.FromX, lineB_65.FromY, lineB_65.FromZ, (ln_204 = lineA_65, ln_204.ToX - ln_204.FromX), (ln_205 = lineA_65, ln_205.ToY - ln_205.FromY), (ln_206 = lineA_65, ln_206.ToZ - ln_206.FromZ), (ln_207 = lineB_65, ln_207.ToX - ln_207.FromX), (ln_208 = lineB_65, ln_208.ToY - ln_208.FromY), (ln_209 = lineB_65, ln_209.ToZ - ln_209.FromZ)))), 0, "Distance should be 0.0");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Apart lines have positive distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        let ln_210, ln_211, ln_212, ln_213, ln_214, ln_215;
        let dist_5;
        const lineA_67 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0));
        const lineB_67 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(2, 1, 0), Pnt_$ctor_Z7AD9E565(3, 1, 0));
        dist_5 = XLine3D_getSqDistance_Z15A9A3C0(lineA_67.FromX, lineA_67.FromY, lineA_67.FromZ, lineB_67.FromX, lineB_67.FromY, lineB_67.FromZ, (ln_210 = lineA_67, ln_210.ToX - ln_210.FromX), (ln_211 = lineA_67, ln_211.ToY - ln_211.FromY), (ln_212 = lineA_67, ln_212.ToZ - ln_212.FromZ), (ln_213 = lineB_67, ln_213.ToX - ln_213.FromX), (ln_214 = lineB_67, ln_214.ToY - ln_214.FromY), (ln_215 = lineB_67, ln_215.ToZ - ln_215.FromZ));
        Expect_isTrue(dist_5 > 0)("Distance should be positive");
        Expect_floatClose(AccuracyModule_high, dist_5, 2, "Squared distance should be 2.0");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel lines distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let lineA_69, lineB_69, ln_216, ln_217, ln_218, ln_219, ln_220, ln_221;
        Expect_floatClose(AccuracyModule_high, (lineA_69 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_69 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), XLine3D_getSqDistance_Z15A9A3C0(lineA_69.FromX, lineA_69.FromY, lineA_69.FromZ, lineB_69.FromX, lineB_69.FromY, lineB_69.FromZ, (ln_216 = lineA_69, ln_216.ToX - ln_216.FromX), (ln_217 = lineA_69, ln_217.ToY - ln_217.FromY), (ln_218 = lineA_69, ln_218.ToZ - ln_218.FromZ), (ln_219 = lineB_69, ln_219.ToX - ln_219.FromX), (ln_220 = lineB_69, ln_220.ToY - ln_220.FromY), (ln_221 = lineB_69, ln_221.ToZ - ln_221.FromZ)))), 1, "Squared distance should be 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew lines distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let lineA_71, lineB_71, ln_222, ln_223, ln_224, ln_225, ln_226, ln_227;
        Expect_floatClose(AccuracyModule_high, (lineA_71 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_71 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 2), Pnt_$ctor_Z7AD9E565(5, 10, 2)), XLine3D_getSqDistance_Z15A9A3C0(lineA_71.FromX, lineA_71.FromY, lineA_71.FromZ, lineB_71.FromX, lineB_71.FromY, lineB_71.FromZ, (ln_222 = lineA_71, ln_222.ToX - ln_222.FromX), (ln_223 = lineA_71, ln_223.ToY - ln_223.FromY), (ln_224 = lineA_71, ln_224.ToZ - ln_224.FromZ), (ln_225 = lineB_71, ln_225.ToX - ln_225.FromX), (ln_226 = lineB_71, ln_226.ToY - ln_226.FromY), (ln_227 = lineB_71, ln_227.ToZ - ln_227.FromZ)))), 4, "Squared distance should be 4.0");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})()])), Test_testList("getEndsTouching tests", ofArray([(() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Not touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        if (XLine3D_getEndsTouching_Z4454D4C5(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(2, 0, 0), Pnt_$ctor_Z7AD9E565(3, 0, 0)), 1E-06).tag === 0) {
            Test_TestCaseBuilder__Zero(builder$0040_46);
        }
        else {
            Test_failtest("Should not be touching");
            Test_TestCaseBuilder__Zero(builder$0040_46);
        }
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("StartA_StartB touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        if (XLine3D_getEndsTouching_Z4454D4C5(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 1, 0)), 1E-06).tag === 1) {
            Test_TestCaseBuilder__Zero(builder$0040_47);
        }
        else {
            Test_failtest("Should touch at StartA_StartB");
            Test_TestCaseBuilder__Zero(builder$0040_47);
        }
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EndA_EndB touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        if (XLine3D_getEndsTouching_Z4454D4C5(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), 1E-06).tag === 2) {
            Test_TestCaseBuilder__Zero(builder$0040_48);
        }
        else {
            Test_failtest("Should touch at EndA_EndB");
            Test_TestCaseBuilder__Zero(builder$0040_48);
        }
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EndA_StartB touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        if (XLine3D_getEndsTouching_Z4454D4C5(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0)), 1E-06).tag === 3) {
            Test_TestCaseBuilder__Zero(builder$0040_49);
        }
        else {
            Test_failtest("Should touch at EndA_StartB");
            Test_TestCaseBuilder__Zero(builder$0040_49);
        }
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("StartA_EndB touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        if (XLine3D_getEndsTouching_Z4454D4C5(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0)), Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), 1E-06).tag === 4) {
            Test_TestCaseBuilder__Zero(builder$0040_50);
        }
        else {
            Test_failtest("Should touch at StartA_EndB");
            Test_TestCaseBuilder__Zero(builder$0040_50);
        }
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Identical lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        if (XLine3D_getEndsTouching_Z4454D4C5(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), 1E-06).tag === 5) {
            Test_TestCaseBuilder__Zero(builder$0040_51);
        }
        else {
            Test_failtest("Should be identical");
            Test_TestCaseBuilder__Zero(builder$0040_51);
        }
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Identical flipped lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        if (XLine3D_getEndsTouching_Z4454D4C5(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)), 1E-06).tag === 6) {
            Test_TestCaseBuilder__Zero(builder$0040_52);
        }
        else {
            Test_failtest("Should be identical flipped");
            Test_TestCaseBuilder__Zero(builder$0040_52);
        }
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D lines touching at end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        if (XLine3D_getEndsTouching_Z4454D4C5(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 1)), Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(1, 1, 1), Pnt_$ctor_Z7AD9E565(2, 2, 2)), 1E-06).tag === 3) {
            Test_TestCaseBuilder__Zero(builder$0040_53);
        }
        else {
            Test_failtest("Should touch at EndA_StartB");
            Test_TestCaseBuilder__Zero(builder$0040_53);
        }
    }));
})()])), Test_testList("Edge cases and numerical stability", ofArray([(() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Very small lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        let lineA_81, lineB_81, ln_228, ln_229, ln_230, ln_231, ln_232, ln_233;
        if (((lineA_81 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1E-08, 0, 0)), (lineB_81 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 1E-08, 0)), XLine3D_getIntersection_ZA268E31(lineA_81.FromX, lineA_81.FromY, lineA_81.FromZ, lineB_81.FromX, lineB_81.FromY, lineB_81.FromZ, (ln_228 = lineA_81, ln_228.ToX - ln_228.FromX), (ln_229 = lineA_81, ln_229.ToY - ln_229.FromY), (ln_230 = lineA_81, ln_230.ToZ - ln_230.FromZ), (ln_231 = lineB_81, ln_231.ToX - ln_231.FromX), (ln_232 = lineB_81, ln_232.ToY - ln_232.FromY), (ln_233 = lineB_81, ln_233.ToZ - ln_233.FromZ), 1E-06, 0.004363350820701567, 1E-06)))).tag === 6) {
            Test_TestCaseBuilder__Zero(builder$0040_54);
        }
        else {
            Test_failtest("Should be too short both");
            Test_TestCaseBuilder__Zero(builder$0040_54);
        }
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Very large coordinates", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        let ln_234, ln_235, ln_236, ln_237, ln_238, ln_239;
        let result_41;
        const lineA_83 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(10000000000, 0, 0), Pnt_$ctor_Z7AD9E565(10000000000 + 10, 0, 0));
        const lineB_83 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(10000000000 + 5, -5, 0), Pnt_$ctor_Z7AD9E565(10000000000 + 5, 5, 0));
        result_41 = XLine3D_tryIntersect_ZA268E31(lineA_83.FromX, lineA_83.FromY, lineA_83.FromZ, lineB_83.FromX, lineB_83.FromY, lineB_83.FromZ, (ln_234 = lineA_83, ln_234.ToX - ln_234.FromX), (ln_235 = lineA_83, ln_235.ToY - ln_235.FromY), (ln_236 = lineA_83, ln_236.ToZ - ln_236.FromZ), (ln_237 = lineB_83, ln_237.ToX - ln_237.FromX), (ln_238 = lineB_83, ln_238.ToY - ln_238.FromY), (ln_239 = lineB_83, ln_239.ToZ - ln_239.FromZ), 1E-06, 0.004363350820701567, 1E-06);
        if (result_41 == null) {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_55);
        }
        else {
            const pt_9 = result_41;
            Expect_floatClose(AccuracyModule_medium, pt_9.X, 10000000000 + 5, "X should be 1e10 + 5.0");
            Expect_floatClose(AccuracyModule_medium, pt_9.Y, 0, "Y should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_55);
        }
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Diagonal lines at 45 degrees in 3D", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        let ln_240, ln_241, ln_242, ln_243, ln_244, ln_245;
        let result_42;
        const lineA_85 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 10, 0));
        const lineB_85 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 10, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        result_42 = XLine3D_tryIntersect_ZA268E31(lineA_85.FromX, lineA_85.FromY, lineA_85.FromZ, lineB_85.FromX, lineB_85.FromY, lineB_85.FromZ, (ln_240 = lineA_85, ln_240.ToX - ln_240.FromX), (ln_241 = lineA_85, ln_241.ToY - ln_241.FromY), (ln_242 = lineA_85, ln_242.ToZ - ln_242.FromZ), (ln_243 = lineB_85, ln_243.ToX - ln_243.FromX), (ln_244 = lineB_85, ln_244.ToY - ln_244.FromY), (ln_245 = lineB_85, ln_245.ToZ - ln_245.FromZ), 1E-06, 0.004363350820701567, 1E-06);
        if (result_42 == null) {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_56);
        }
        else {
            const pt_10 = result_42;
            Expect_floatClose(AccuracyModule_high, pt_10.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_10.Y, 5, "Y should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_10.Z, 0, "Z should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_56);
        }
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Lines with negative coordinates", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        let ln_246, ln_247, ln_248, ln_249, ln_250, ln_251;
        let result_43;
        const lineA_87 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(-10, -10, -10), Pnt_$ctor_Z7AD9E565(10, -10, -10));
        const lineB_87 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, -20, -10), Pnt_$ctor_Z7AD9E565(0, 0, -10));
        result_43 = XLine3D_tryIntersect_ZA268E31(lineA_87.FromX, lineA_87.FromY, lineA_87.FromZ, lineB_87.FromX, lineB_87.FromY, lineB_87.FromZ, (ln_246 = lineA_87, ln_246.ToX - ln_246.FromX), (ln_247 = lineA_87, ln_247.ToY - ln_247.FromY), (ln_248 = lineA_87, ln_248.ToZ - ln_248.FromZ), (ln_249 = lineB_87, ln_249.ToX - ln_249.FromX), (ln_250 = lineB_87, ln_250.ToY - ln_250.FromY), (ln_251 = lineB_87, ln_251.ToZ - ln_251.FromZ), 1E-06, 0.004363350820701567, 1E-06);
        if (result_43 == null) {
            Test_failtest("Should intersect");
            Test_TestCaseBuilder__Zero(builder$0040_57);
        }
        else {
            const pt_11 = result_43;
            Expect_floatClose(AccuracyModule_high, pt_11.X, 0, "X should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_11.Y, -10, "Y should be -10.0");
            Expect_floatClose(AccuracyModule_high, pt_11.Z, -10, "Z should be -10.0");
            Test_TestCaseBuilder__Zero(builder$0040_57);
        }
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("True 3D skew lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let lineA_89, lineB_89, ln_252, ln_253, ln_254, ln_255, ln_256, ln_257;
        Expect_floatClose(AccuracyModule_high, (lineA_89 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_89 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 10, 10), Pnt_$ctor_Z7AD9E565(10, 10, 10)), XLine3D_getSqDistance_Z15A9A3C0(lineA_89.FromX, lineA_89.FromY, lineA_89.FromZ, lineB_89.FromX, lineB_89.FromY, lineB_89.FromZ, (ln_252 = lineA_89, ln_252.ToX - ln_252.FromX), (ln_253 = lineA_89, ln_253.ToY - ln_253.FromY), (ln_254 = lineA_89, ln_254.ToZ - ln_254.FromZ), (ln_255 = lineB_89, ln_255.ToX - ln_255.FromX), (ln_256 = lineB_89, ln_256.ToY - ln_256.FromY), (ln_257 = lineB_89, ln_257.ToZ - ln_257.FromZ)))), 200, "Squared distance should be 200.0 (10^2 + 10^2)");
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Nearly coplanar skew lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let ln_258, ln_259, ln_260, ln_261, ln_262, ln_263;
        let result_44;
        const lineA_91 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_91 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 0.001), Pnt_$ctor_Z7AD9E565(5, 10, 0.001));
        result_44 = XLine3D_getIntersection_ZA268E31(lineA_91.FromX, lineA_91.FromY, lineA_91.FromZ, lineB_91.FromX, lineB_91.FromY, lineB_91.FromZ, (ln_258 = lineA_91, ln_258.ToX - ln_258.FromX), (ln_259 = lineA_91, ln_259.ToY - ln_259.FromY), (ln_260 = lineA_91, ln_260.ToZ - ln_260.FromZ), (ln_261 = lineB_91, ln_261.ToX - ln_261.FromX), (ln_262 = lineB_91, ln_262.ToY - ln_262.FromY), (ln_263 = lineB_91, ln_263.ToZ - ln_263.FromZ), 1E-05, 0.004363350820701567, 1E-06);
        switch (result_44.tag) {
            case 0:
            case 1: {
                Test_TestCaseBuilder__Zero(builder$0040_59);
                break;
            }
            default: {
                Test_failtest("Should be intersect or skew");
                Test_TestCaseBuilder__Zero(builder$0040_59);
            }
        }
    }));
})()])), Test_testList("doRaysIntersect tests", ofArray([(() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rays intersecting within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let lineA_93, lineB_93, pAx_10, pAy_10, pAz_10, pBx_10, pBy_10, pBz_10, vAx_10, ln_264, vAy_10, ln_265, vAz_10, ln_266, vBx_10, ln_267, vBy_10, ln_268, vBz_10, ln_269, tA, vAx_11, vAy_11, vAz_11, vBx_11, vBy_11, vBz_11, crossX_10, crossY_10, crossZ_10, dx_10, dy_10, dz_10, pAx_12, pAy_12, pAz_12, vAx_12, vAy_12, vAz_12, x_1, y_1, z_1, t_23, vAx_13, vAy_13, vAz_13, vx, vy, vz;
        Expect_isTrue((lineA_93 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_93 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0)), (pAx_10 = lineA_93.FromX, (pAy_10 = lineA_93.FromY, (pAz_10 = lineA_93.FromZ, (pBx_10 = lineB_93.FromX, (pBy_10 = lineB_93.FromY, (pBz_10 = lineB_93.FromZ, (vAx_10 = ((ln_264 = lineA_93, ln_264.ToX - ln_264.FromX)), (vAy_10 = ((ln_265 = lineA_93, ln_265.ToY - ln_265.FromY)), (vAz_10 = ((ln_266 = lineA_93, ln_266.ToZ - ln_266.FromZ)), (vBx_10 = ((ln_267 = lineB_93, ln_267.ToX - ln_267.FromX)), (vBy_10 = ((ln_268 = lineB_93, ln_268.ToY - ln_268.FromY)), (vBz_10 = ((ln_269 = lineB_93, ln_269.ToZ - ln_269.FromZ)), (tA = ((vAx_11 = vAx_10, (vAy_11 = vAy_10, (vAz_11 = vAz_10, (vBx_11 = vBx_10, (vBy_11 = vBy_10, (vBz_11 = vBz_10, (crossX_10 = ((vAy_11 * vBz_11) - (vAz_11 * vBy_11)), (crossY_10 = ((vAz_11 * vBx_11) - (vAx_11 * vBz_11)), (crossZ_10 = ((vAx_11 * vBy_11) - (vAy_11 * vBx_11)), (dx_10 = (pBx_10 - pAx_10), (dy_10 = (pBy_10 - pAy_10), (dz_10 = (pBz_10 - pAz_10), (((((dy_10 * vBz_11) - (dz_10 * vBy_11)) * crossX_10) + (((dz_10 * vBx_11) - (dx_10 * vBz_11)) * crossY_10)) + (((dx_10 * vBy_11) - (dy_10 * vBx_11)) * crossZ_10)) / (((crossX_10 * crossX_10) + (crossY_10 * crossY_10)) + (crossZ_10 * crossZ_10))))))))))))))), ((tA > -1000000000000) && (tA < 1000000000000)) && (((pAx_12 = pBx_10, (pAy_12 = pBy_10, (pAz_12 = pBz_10, (vAx_12 = vBx_10, (vAy_12 = vBy_10, (vAz_12 = vBz_10, (x_1 = (pAx_10 + (tA * vAx_10)), (y_1 = (pAy_10 + (tA * vAy_10)), (z_1 = (pAz_10 + (tA * vAz_10)), (t_23 = ((vAx_13 = vAx_12, (vAy_13 = vAy_12, (vAz_13 = vAz_12, (((vAx_13 * (x_1 - pAx_12)) + (vAy_13 * (y_1 - pAy_12))) + (vAz_13 * (z_1 - pAz_12))) / (((vAx_13 * vAx_13) + (vAy_13 * vAy_13)) + (vAz_13 * vAz_13)))))), (vx = ((pAx_12 + (vAx_12 * t_23)) - x_1), (vy = ((pAy_12 + (vAy_12 * t_23)) - y_1), (vz = ((pAz_12 + (vAz_12 * t_23)) - z_1), ((vx * vx) + (vy * vy)) + (vz * vz))))))))))))))) < (1E-06 * 1E-06))))))))))))))))))("Rays should intersect");
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})(), (() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rays parallel - not intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let lineA_95, lineB_95, pAx_14, pAy_14, pAz_14, pBx_12, pBy_12, pBz_12, vAx_14, ln_270, vAy_14, ln_271, vAz_14, ln_272, vBx_12, ln_273, vBy_12, ln_274, vBz_12, ln_275, tA_1, vAx_15, vAy_15, vAz_15, vBx_13, vBy_13, vBz_13, crossX_11, crossY_11, crossZ_11, dx_11, dy_11, dz_11, pAx_16, pAy_16, pAz_16, vAx_16, vAy_16, vAz_16, x_3, y_3, z_3, t_24, vAx_17, vAy_17, vAz_17, vx_1, vy_1, vz_1;
        Expect_isFalse((lineA_95 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_95 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), (pAx_14 = lineA_95.FromX, (pAy_14 = lineA_95.FromY, (pAz_14 = lineA_95.FromZ, (pBx_12 = lineB_95.FromX, (pBy_12 = lineB_95.FromY, (pBz_12 = lineB_95.FromZ, (vAx_14 = ((ln_270 = lineA_95, ln_270.ToX - ln_270.FromX)), (vAy_14 = ((ln_271 = lineA_95, ln_271.ToY - ln_271.FromY)), (vAz_14 = ((ln_272 = lineA_95, ln_272.ToZ - ln_272.FromZ)), (vBx_12 = ((ln_273 = lineB_95, ln_273.ToX - ln_273.FromX)), (vBy_12 = ((ln_274 = lineB_95, ln_274.ToY - ln_274.FromY)), (vBz_12 = ((ln_275 = lineB_95, ln_275.ToZ - ln_275.FromZ)), (tA_1 = ((vAx_15 = vAx_14, (vAy_15 = vAy_14, (vAz_15 = vAz_14, (vBx_13 = vBx_12, (vBy_13 = vBy_12, (vBz_13 = vBz_12, (crossX_11 = ((vAy_15 * vBz_13) - (vAz_15 * vBy_13)), (crossY_11 = ((vAz_15 * vBx_13) - (vAx_15 * vBz_13)), (crossZ_11 = ((vAx_15 * vBy_13) - (vAy_15 * vBx_13)), (dx_11 = (pBx_12 - pAx_14), (dy_11 = (pBy_12 - pAy_14), (dz_11 = (pBz_12 - pAz_14), (((((dy_11 * vBz_13) - (dz_11 * vBy_13)) * crossX_11) + (((dz_11 * vBx_13) - (dx_11 * vBz_13)) * crossY_11)) + (((dx_11 * vBy_13) - (dy_11 * vBx_13)) * crossZ_11)) / (((crossX_11 * crossX_11) + (crossY_11 * crossY_11)) + (crossZ_11 * crossZ_11))))))))))))))), ((tA_1 > -1000000000000) && (tA_1 < 1000000000000)) && (((pAx_16 = pBx_12, (pAy_16 = pBy_12, (pAz_16 = pBz_12, (vAx_16 = vBx_12, (vAy_16 = vBy_12, (vAz_16 = vBz_12, (x_3 = (pAx_14 + (tA_1 * vAx_14)), (y_3 = (pAy_14 + (tA_1 * vAy_14)), (z_3 = (pAz_14 + (tA_1 * vAz_14)), (t_24 = ((vAx_17 = vAx_16, (vAy_17 = vAy_16, (vAz_17 = vAz_16, (((vAx_17 * (x_3 - pAx_16)) + (vAy_17 * (y_3 - pAy_16))) + (vAz_17 * (z_3 - pAz_16))) / (((vAx_17 * vAx_17) + (vAy_17 * vAy_17)) + (vAz_17 * vAz_17)))))), (vx_1 = ((pAx_16 + (vAx_16 * t_24)) - x_3), (vy_1 = ((pAy_16 + (vAy_16 * t_24)) - y_3), (vz_1 = ((pAz_16 + (vAz_16 * t_24)) - z_3), ((vx_1 * vx_1) + (vy_1 * vy_1)) + (vz_1 * vz_1))))))))))))))) < (1E-06 * 1E-06))))))))))))))))))("Parallel rays should not intersect");
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew rays - distance exceeds tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        let lineA_97, lineB_97, pAx_18, pAy_18, pAz_18, pBx_14, pBy_14, pBz_14, vAx_18, ln_276, vAy_18, ln_277, vAz_18, ln_278, vBx_14, ln_279, vBy_14, ln_280, vBz_14, ln_281, tA_2, vAx_19, vAy_19, vAz_19, vBx_15, vBy_15, vBz_15, crossX_12, crossY_12, crossZ_12, dx_12, dy_12, dz_12, pAx_20, pAy_20, pAz_20, vAx_20, vAy_20, vAz_20, x_5, y_5, z_5, t_25, vAx_21, vAy_21, vAz_21, vx_2, vy_2, vz_2;
        Expect_isFalse((lineA_97 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_97 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 10), Pnt_$ctor_Z7AD9E565(5, 10, 10)), (pAx_18 = lineA_97.FromX, (pAy_18 = lineA_97.FromY, (pAz_18 = lineA_97.FromZ, (pBx_14 = lineB_97.FromX, (pBy_14 = lineB_97.FromY, (pBz_14 = lineB_97.FromZ, (vAx_18 = ((ln_276 = lineA_97, ln_276.ToX - ln_276.FromX)), (vAy_18 = ((ln_277 = lineA_97, ln_277.ToY - ln_277.FromY)), (vAz_18 = ((ln_278 = lineA_97, ln_278.ToZ - ln_278.FromZ)), (vBx_14 = ((ln_279 = lineB_97, ln_279.ToX - ln_279.FromX)), (vBy_14 = ((ln_280 = lineB_97, ln_280.ToY - ln_280.FromY)), (vBz_14 = ((ln_281 = lineB_97, ln_281.ToZ - ln_281.FromZ)), (tA_2 = ((vAx_19 = vAx_18, (vAy_19 = vAy_18, (vAz_19 = vAz_18, (vBx_15 = vBx_14, (vBy_15 = vBy_14, (vBz_15 = vBz_14, (crossX_12 = ((vAy_19 * vBz_15) - (vAz_19 * vBy_15)), (crossY_12 = ((vAz_19 * vBx_15) - (vAx_19 * vBz_15)), (crossZ_12 = ((vAx_19 * vBy_15) - (vAy_19 * vBx_15)), (dx_12 = (pBx_14 - pAx_18), (dy_12 = (pBy_14 - pAy_18), (dz_12 = (pBz_14 - pAz_18), (((((dy_12 * vBz_15) - (dz_12 * vBy_15)) * crossX_12) + (((dz_12 * vBx_15) - (dx_12 * vBz_15)) * crossY_12)) + (((dx_12 * vBy_15) - (dy_12 * vBx_15)) * crossZ_12)) / (((crossX_12 * crossX_12) + (crossY_12 * crossY_12)) + (crossZ_12 * crossZ_12))))))))))))))), ((tA_2 > -1000000000000) && (tA_2 < 1000000000000)) && (((pAx_20 = pBx_14, (pAy_20 = pBy_14, (pAz_20 = pBz_14, (vAx_20 = vBx_14, (vAy_20 = vBy_14, (vAz_20 = vBz_14, (x_5 = (pAx_18 + (tA_2 * vAx_18)), (y_5 = (pAy_18 + (tA_2 * vAy_18)), (z_5 = (pAz_18 + (tA_2 * vAz_18)), (t_25 = ((vAx_21 = vAx_20, (vAy_21 = vAy_20, (vAz_21 = vAz_20, (((vAx_21 * (x_5 - pAx_20)) + (vAy_21 * (y_5 - pAy_20))) + (vAz_21 * (z_5 - pAz_20))) / (((vAx_21 * vAx_21) + (vAy_21 * vAy_21)) + (vAz_21 * vAz_21)))))), (vx_2 = ((pAx_20 + (vAx_20 * t_25)) - x_5), (vy_2 = ((pAy_20 + (vAy_20 * t_25)) - y_5), (vz_2 = ((pAz_20 + (vAz_20 * t_25)) - z_5), ((vx_2 * vx_2) + (vy_2 * vy_2)) + (vz_2 * vz_2))))))))))))))) < (1E-06 * 1E-06))))))))))))))))))("Skew rays with large distance should not intersect");
        Test_TestCaseBuilder__Zero(builder$0040_62);
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew rays - within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        let lineA_99, lineB_99, pAx_22, pAy_22, pAz_22, pBx_16, pBy_16, pBz_16, vAx_22, ln_282, vAy_22, ln_283, vAz_22, ln_284, vBx_16, ln_285, vBy_16, ln_286, vBz_16, ln_287, tA_3, vAx_23, vAy_23, vAz_23, vBx_17, vBy_17, vBz_17, crossX_13, crossY_13, crossZ_13, dx_13, dy_13, dz_13, pAx_24, pAy_24, pAz_24, vAx_24, vAy_24, vAz_24, x_7, y_7, z_7, t_26, vAx_25, vAy_25, vAz_25, vx_3, vy_3, vz_3;
        Expect_isTrue((lineA_99 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_99 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 0.001), Pnt_$ctor_Z7AD9E565(5, 10, 0.001)), (pAx_22 = lineA_99.FromX, (pAy_22 = lineA_99.FromY, (pAz_22 = lineA_99.FromZ, (pBx_16 = lineB_99.FromX, (pBy_16 = lineB_99.FromY, (pBz_16 = lineB_99.FromZ, (vAx_22 = ((ln_282 = lineA_99, ln_282.ToX - ln_282.FromX)), (vAy_22 = ((ln_283 = lineA_99, ln_283.ToY - ln_283.FromY)), (vAz_22 = ((ln_284 = lineA_99, ln_284.ToZ - ln_284.FromZ)), (vBx_16 = ((ln_285 = lineB_99, ln_285.ToX - ln_285.FromX)), (vBy_16 = ((ln_286 = lineB_99, ln_286.ToY - ln_286.FromY)), (vBz_16 = ((ln_287 = lineB_99, ln_287.ToZ - ln_287.FromZ)), (tA_3 = ((vAx_23 = vAx_22, (vAy_23 = vAy_22, (vAz_23 = vAz_22, (vBx_17 = vBx_16, (vBy_17 = vBy_16, (vBz_17 = vBz_16, (crossX_13 = ((vAy_23 * vBz_17) - (vAz_23 * vBy_17)), (crossY_13 = ((vAz_23 * vBx_17) - (vAx_23 * vBz_17)), (crossZ_13 = ((vAx_23 * vBy_17) - (vAy_23 * vBx_17)), (dx_13 = (pBx_16 - pAx_22), (dy_13 = (pBy_16 - pAy_22), (dz_13 = (pBz_16 - pAz_22), (((((dy_13 * vBz_17) - (dz_13 * vBy_17)) * crossX_13) + (((dz_13 * vBx_17) - (dx_13 * vBz_17)) * crossY_13)) + (((dx_13 * vBy_17) - (dy_13 * vBx_17)) * crossZ_13)) / (((crossX_13 * crossX_13) + (crossY_13 * crossY_13)) + (crossZ_13 * crossZ_13))))))))))))))), ((tA_3 > -1000000000000) && (tA_3 < 1000000000000)) && (((pAx_24 = pBx_16, (pAy_24 = pBy_16, (pAz_24 = pBz_16, (vAx_24 = vBx_16, (vAy_24 = vBy_16, (vAz_24 = vBz_16, (x_7 = (pAx_22 + (tA_3 * vAx_22)), (y_7 = (pAy_22 + (tA_3 * vAy_22)), (z_7 = (pAz_22 + (tA_3 * vAz_22)), (t_26 = ((vAx_25 = vAx_24, (vAy_25 = vAy_24, (vAz_25 = vAz_24, (((vAx_25 * (x_7 - pAx_24)) + (vAy_25 * (y_7 - pAy_24))) + (vAz_25 * (z_7 - pAz_24))) / (((vAx_25 * vAx_25) + (vAy_25 * vAy_25)) + (vAz_25 * vAz_25)))))), (vx_3 = ((pAx_24 + (vAx_24 * t_26)) - x_7), (vy_3 = ((pAy_24 + (vAy_24 * t_26)) - y_7), (vz_3 = ((pAz_24 + (vAz_24 * t_26)) - z_7), ((vx_3 * vx_3) + (vy_3 * vy_3)) + (vz_3 * vz_3))))))))))))))) < (0.01 * 0.01))))))))))))))))))("Skew rays within tolerance should be considered intersecting");
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})(), (() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Zero length line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let lineA_101, lineB_101, pAx_26, pAy_26, pAz_26, pBx_18, pBy_18, pBz_18, vAx_26, ln_288, vAy_26, ln_289, vAz_26, ln_290, vBx_18, ln_291, vBy_18, ln_292, vBz_18, ln_293, tA_4, vAx_27, vAy_27, vAz_27, vBx_19, vBy_19, vBz_19, crossX_14, crossY_14, crossZ_14, dx_14, dy_14, dz_14, pAx_28, pAy_28, pAz_28, vAx_28, vAy_28, vAz_28, x_9, y_9, z_9, t_27, vAx_29, vAy_29, vAz_29, vx_4, vy_4, vz_4;
        Expect_isFalse((lineA_101 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)), (lineB_101 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (pAx_26 = lineA_101.FromX, (pAy_26 = lineA_101.FromY, (pAz_26 = lineA_101.FromZ, (pBx_18 = lineB_101.FromX, (pBy_18 = lineB_101.FromY, (pBz_18 = lineB_101.FromZ, (vAx_26 = ((ln_288 = lineA_101, ln_288.ToX - ln_288.FromX)), (vAy_26 = ((ln_289 = lineA_101, ln_289.ToY - ln_289.FromY)), (vAz_26 = ((ln_290 = lineA_101, ln_290.ToZ - ln_290.FromZ)), (vBx_18 = ((ln_291 = lineB_101, ln_291.ToX - ln_291.FromX)), (vBy_18 = ((ln_292 = lineB_101, ln_292.ToY - ln_292.FromY)), (vBz_18 = ((ln_293 = lineB_101, ln_293.ToZ - ln_293.FromZ)), (tA_4 = ((vAx_27 = vAx_26, (vAy_27 = vAy_26, (vAz_27 = vAz_26, (vBx_19 = vBx_18, (vBy_19 = vBy_18, (vBz_19 = vBz_18, (crossX_14 = ((vAy_27 * vBz_19) - (vAz_27 * vBy_19)), (crossY_14 = ((vAz_27 * vBx_19) - (vAx_27 * vBz_19)), (crossZ_14 = ((vAx_27 * vBy_19) - (vAy_27 * vBx_19)), (dx_14 = (pBx_18 - pAx_26), (dy_14 = (pBy_18 - pAy_26), (dz_14 = (pBz_18 - pAz_26), (((((dy_14 * vBz_19) - (dz_14 * vBy_19)) * crossX_14) + (((dz_14 * vBx_19) - (dx_14 * vBz_19)) * crossY_14)) + (((dx_14 * vBy_19) - (dy_14 * vBx_19)) * crossZ_14)) / (((crossX_14 * crossX_14) + (crossY_14 * crossY_14)) + (crossZ_14 * crossZ_14))))))))))))))), ((tA_4 > -1000000000000) && (tA_4 < 1000000000000)) && (((pAx_28 = pBx_18, (pAy_28 = pBy_18, (pAz_28 = pBz_18, (vAx_28 = vBx_18, (vAy_28 = vBy_18, (vAz_28 = vBz_18, (x_9 = (pAx_26 + (tA_4 * vAx_26)), (y_9 = (pAy_26 + (tA_4 * vAy_26)), (z_9 = (pAz_26 + (tA_4 * vAz_26)), (t_27 = ((vAx_29 = vAx_28, (vAy_29 = vAy_28, (vAz_29 = vAz_28, (((vAx_29 * (x_9 - pAx_28)) + (vAy_29 * (y_9 - pAy_28))) + (vAz_29 * (z_9 - pAz_28))) / (((vAx_29 * vAx_29) + (vAy_29 * vAy_29)) + (vAz_29 * vAz_29)))))), (vx_4 = ((pAx_28 + (vAx_28 * t_27)) - x_9), (vy_4 = ((pAy_28 + (vAy_28 * t_27)) - y_9), (vz_4 = ((pAz_28 + (vAz_28 * t_27)) - z_9), ((vx_4 * vx_4) + (vy_4 * vy_4)) + (vz_4 * vz_4))))))))))))))) < (1E-06 * 1E-06))))))))))))))))))("Zero length line should not intersect");
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pnt/Vec overload", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        let pA_17, pB_17, vA_17, vB_17, pAx_30, pAy_30, pAz_30, pBx_20, pBy_20, pBz_20, vAx_30, vAy_30, vAz_30, vBx_20, vBy_20, vBz_20, tA_5, vAx_31, vAy_31, vAz_31, vBx_21, vBy_21, vBz_21, crossX_15, crossY_15, crossZ_15, dx_15, dy_15, dz_15, pAx_32, pAy_32, pAz_32, vAx_32, vAy_32, vAz_32, x_11, y_11, z_11, t_28, vAx_33, vAy_33, vAz_33, vx_5, vy_5, vz_5;
        Expect_isTrue((pA_17 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (pB_17 = Pnt_$ctor_Z7AD9E565(5, -5, 0), (vA_17 = Vec_$ctor_Z7AD9E565(10, 0, 0), (vB_17 = Vec_$ctor_Z7AD9E565(0, 10, 0), (pAx_30 = pA_17.X, (pAy_30 = pA_17.Y, (pAz_30 = pA_17.Z, (pBx_20 = pB_17.X, (pBy_20 = pB_17.Y, (pBz_20 = pB_17.Z, (vAx_30 = vA_17.X, (vAy_30 = vA_17.Y, (vAz_30 = vA_17.Z, (vBx_20 = vB_17.X, (vBy_20 = vB_17.Y, (vBz_20 = vB_17.Z, (tA_5 = ((vAx_31 = vAx_30, (vAy_31 = vAy_30, (vAz_31 = vAz_30, (vBx_21 = vBx_20, (vBy_21 = vBy_20, (vBz_21 = vBz_20, (crossX_15 = ((vAy_31 * vBz_21) - (vAz_31 * vBy_21)), (crossY_15 = ((vAz_31 * vBx_21) - (vAx_31 * vBz_21)), (crossZ_15 = ((vAx_31 * vBy_21) - (vAy_31 * vBx_21)), (dx_15 = (pBx_20 - pAx_30), (dy_15 = (pBy_20 - pAy_30), (dz_15 = (pBz_20 - pAz_30), (((((dy_15 * vBz_21) - (dz_15 * vBy_21)) * crossX_15) + (((dz_15 * vBx_21) - (dx_15 * vBz_21)) * crossY_15)) + (((dx_15 * vBy_21) - (dy_15 * vBx_21)) * crossZ_15)) / (((crossX_15 * crossX_15) + (crossY_15 * crossY_15)) + (crossZ_15 * crossZ_15))))))))))))))), ((tA_5 > -1000000000000) && (tA_5 < 1000000000000)) && (((pAx_32 = pBx_20, (pAy_32 = pBy_20, (pAz_32 = pBz_20, (vAx_32 = vBx_20, (vAy_32 = vBy_20, (vAz_32 = vBz_20, (x_11 = (pAx_30 + (tA_5 * vAx_30)), (y_11 = (pAy_30 + (tA_5 * vAy_30)), (z_11 = (pAz_30 + (tA_5 * vAz_30)), (t_28 = ((vAx_33 = vAx_32, (vAy_33 = vAy_32, (vAz_33 = vAz_32, (((vAx_33 * (x_11 - pAx_32)) + (vAy_33 * (y_11 - pAy_32))) + (vAz_33 * (z_11 - pAz_32))) / (((vAx_33 * vAx_33) + (vAy_33 * vAy_33)) + (vAz_33 * vAz_33)))))), (vx_5 = ((pAx_32 + (vAx_32 * t_28)) - x_11), (vy_5 = ((pAy_32 + (vAy_32 * t_28)) - y_11), (vz_5 = ((pAz_32 + (vAz_32 * t_28)) - z_11), ((vx_5 * vx_5) + (vy_5 * vy_5)) + (vz_5 * vz_5))))))))))))))) < (1E-06 * 1E-06))))))))))))))))))))("Rays should intersect");
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})(), (() => {
    const builder$0040_66 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Float components overload", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_66, Test_TestCaseBuilder__Delay_1505(builder$0040_66, () => {
        let tA_6, crossX_16, crossY_16, crossZ_16, dx_16, dy_16, dz_16, x_13, y_13, z_13, t_29, vAx_37, vAy_37, vAz_37, vx_6, vy_6, vz_6;
        Expect_isTrue((tA_6 = ((crossX_16 = ((0 * 0) - (0 * 10)), (crossY_16 = ((0 * 0) - (10 * 0)), (crossZ_16 = ((10 * 10) - (0 * 0)), (dx_16 = (5 - 0), (dy_16 = (-5 - 0), (dz_16 = (0 - 0), (((((dy_16 * 0) - (dz_16 * 10)) * crossX_16) + (((dz_16 * 0) - (dx_16 * 0)) * crossY_16)) + (((dx_16 * 10) - (dy_16 * 0)) * crossZ_16)) / (((crossX_16 * crossX_16) + (crossY_16 * crossY_16)) + (crossZ_16 * crossZ_16))))))))), ((tA_6 > -1000000000000) && (tA_6 < 1000000000000)) && (((x_13 = (0 + (tA_6 * 10)), (y_13 = (0 + (tA_6 * 0)), (z_13 = (0 + (tA_6 * 0)), (t_29 = ((vAx_37 = 0, (vAy_37 = 10, (vAz_37 = 0, (((vAx_37 * (x_13 - 5)) + (vAy_37 * (y_13 - -5))) + (vAz_37 * (z_13 - 0))) / (((vAx_37 * vAx_37) + (vAy_37 * vAy_37)) + (vAz_37 * vAz_37)))))), (vx_6 = ((5 + (0 * t_29)) - x_13), (vy_6 = ((-5 + (10 * t_29)) - y_13), (vz_6 = ((0 + (0 * t_29)) - z_13), ((vx_6 * vx_6) + (vy_6 * vy_6)) + (vz_6 * vz_6))))))))) < (1E-06 * 1E-06))))("Rays should intersect");
        Test_TestCaseBuilder__Zero(builder$0040_66);
    }));
})()])), Test_testList("tryClosestParameterRayA tests", ofArray([(() => {
    const builder$0040_67 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting rays - parameter in range", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_67, Test_TestCaseBuilder__Delay_1505(builder$0040_67, () => {
        let result_52;
        const lineA_103 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_103 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        let vAx_38;
        const ln_294 = lineA_103;
        vAx_38 = (ln_294.ToX - ln_294.FromX);
        let vAy_38;
        const ln_295 = lineA_103;
        vAy_38 = (ln_295.ToY - ln_295.FromY);
        let vAz_38;
        const ln_296 = lineA_103;
        vAz_38 = (ln_296.ToZ - ln_296.FromZ);
        let vBx_24;
        const ln_297 = lineB_103;
        vBx_24 = (ln_297.ToX - ln_297.FromX);
        let vBy_24;
        const ln_298 = lineB_103;
        vBy_24 = (ln_298.ToY - ln_298.FromY);
        let vBz_24;
        const ln_299 = lineB_103;
        vBz_24 = (ln_299.ToZ - ln_299.FromZ);
        const crossX_17 = (vAy_38 * vBz_24) - (vAz_38 * vBy_24);
        const crossY_17 = (vAz_38 * vBx_24) - (vAx_38 * vBz_24);
        const crossZ_17 = (vAx_38 * vBy_24) - (vAy_38 * vBx_24);
        const dx_17 = lineB_103.FromX - lineA_103.FromX;
        const dy_17 = lineB_103.FromY - lineA_103.FromY;
        const dz_17 = lineB_103.FromZ - lineA_103.FromZ;
        const t_30 = (((((dy_17 * vBz_24) - (dz_17 * vBy_24)) * crossX_17) + (((dz_17 * vBx_24) - (dx_17 * vBz_24)) * crossY_17)) + (((dx_17 * vBy_24) - (dy_17 * vBx_24)) * crossZ_17)) / (((crossX_17 * crossX_17) + (crossY_17 * crossY_17)) + (crossZ_17 * crossZ_17));
        result_52 = (((t_30 > -1000000000000) && (t_30 < 1000000000000)) ? t_30 : undefined);
        if (result_52 == null) {
            Test_failtest("Should return a parameter");
            Test_TestCaseBuilder__Zero(builder$0040_67);
        }
        else {
            Expect_floatClose(AccuracyModule_high, result_52, 0.5, "Parameter should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_67);
        }
    }));
})(), (() => {
    const builder$0040_68 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel rays - returns None", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_68, Test_TestCaseBuilder__Delay_1505(builder$0040_68, () => {
        let lineA_105, lineB_105, vAx_39, ln_300, vAy_39, ln_301, vAz_39, ln_302, vBx_25, ln_303, vBy_25, ln_304, vBz_25, ln_305, crossX_18, crossY_18, crossZ_18, dx_18, dy_18, dz_18, t_32;
        Expect_isNone((lineA_105 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_105 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), (vAx_39 = ((ln_300 = lineA_105, ln_300.ToX - ln_300.FromX)), (vAy_39 = ((ln_301 = lineA_105, ln_301.ToY - ln_301.FromY)), (vAz_39 = ((ln_302 = lineA_105, ln_302.ToZ - ln_302.FromZ)), (vBx_25 = ((ln_303 = lineB_105, ln_303.ToX - ln_303.FromX)), (vBy_25 = ((ln_304 = lineB_105, ln_304.ToY - ln_304.FromY)), (vBz_25 = ((ln_305 = lineB_105, ln_305.ToZ - ln_305.FromZ)), (crossX_18 = ((vAy_39 * vBz_25) - (vAz_39 * vBy_25)), (crossY_18 = ((vAz_39 * vBx_25) - (vAx_39 * vBz_25)), (crossZ_18 = ((vAx_39 * vBy_25) - (vAy_39 * vBx_25)), (dx_18 = (lineB_105.FromX - lineA_105.FromX), (dy_18 = (lineB_105.FromY - lineA_105.FromY), (dz_18 = (lineB_105.FromZ - lineA_105.FromZ), (t_32 = ((((((dy_18 * vBz_25) - (dz_18 * vBy_25)) * crossX_18) + (((dz_18 * vBx_25) - (dx_18 * vBz_25)) * crossY_18)) + (((dx_18 * vBy_25) - (dy_18 * vBx_25)) * crossZ_18)) / (((crossX_18 * crossX_18) + (crossY_18 * crossY_18)) + (crossZ_18 * crossZ_18))), ((t_32 > -1000000000000) && (t_32 < 1000000000000)) ? t_32 : undefined))))))))))))))), "Parallel rays should return None");
        Test_TestCaseBuilder__Zero(builder$0040_68);
    }));
})(), (() => {
    const builder$0040_69 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew rays - returns parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_69, Test_TestCaseBuilder__Delay_1505(builder$0040_69, () => {
        let result_54;
        const lineA_107 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_107 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 1), Pnt_$ctor_Z7AD9E565(5, 10, 1));
        let vAx_40;
        const ln_306 = lineA_107;
        vAx_40 = (ln_306.ToX - ln_306.FromX);
        let vAy_40;
        const ln_307 = lineA_107;
        vAy_40 = (ln_307.ToY - ln_307.FromY);
        let vAz_40;
        const ln_308 = lineA_107;
        vAz_40 = (ln_308.ToZ - ln_308.FromZ);
        let vBx_26;
        const ln_309 = lineB_107;
        vBx_26 = (ln_309.ToX - ln_309.FromX);
        let vBy_26;
        const ln_310 = lineB_107;
        vBy_26 = (ln_310.ToY - ln_310.FromY);
        let vBz_26;
        const ln_311 = lineB_107;
        vBz_26 = (ln_311.ToZ - ln_311.FromZ);
        const crossX_19 = (vAy_40 * vBz_26) - (vAz_40 * vBy_26);
        const crossY_19 = (vAz_40 * vBx_26) - (vAx_40 * vBz_26);
        const crossZ_19 = (vAx_40 * vBy_26) - (vAy_40 * vBx_26);
        const dx_19 = lineB_107.FromX - lineA_107.FromX;
        const dy_19 = lineB_107.FromY - lineA_107.FromY;
        const dz_19 = lineB_107.FromZ - lineA_107.FromZ;
        const t_33 = (((((dy_19 * vBz_26) - (dz_19 * vBy_26)) * crossX_19) + (((dz_19 * vBx_26) - (dx_19 * vBz_26)) * crossY_19)) + (((dx_19 * vBy_26) - (dy_19 * vBx_26)) * crossZ_19)) / (((crossX_19 * crossX_19) + (crossY_19 * crossY_19)) + (crossZ_19 * crossZ_19));
        result_54 = (((t_33 > -1000000000000) && (t_33 < 1000000000000)) ? t_33 : undefined);
        if (result_54 == null) {
            Test_failtest("Should return a parameter");
            Test_TestCaseBuilder__Zero(builder$0040_69);
        }
        else {
            Expect_floatClose(AccuracyModule_high, result_54, 0.5, "Parameter should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_69);
        }
    }));
})(), (() => {
    const builder$0040_70 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parameter too large - returns None", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_70, Test_TestCaseBuilder__Delay_1505(builder$0040_70, () => {
        let lineA_109, lineB_109, vAx_41, ln_312, vAy_41, ln_313, vAz_41, ln_314, vBx_27, ln_315, vBy_27, ln_316, vBz_27, ln_317, crossX_20, crossY_20, crossZ_20, dx_20, dy_20, dz_20, t_35;
        Expect_isNone((lineA_109 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)), (lineB_109 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(1000000000000000, -5, 0), Pnt_$ctor_Z7AD9E565(1000000000000000, 5, 0)), (vAx_41 = ((ln_312 = lineA_109, ln_312.ToX - ln_312.FromX)), (vAy_41 = ((ln_313 = lineA_109, ln_313.ToY - ln_313.FromY)), (vAz_41 = ((ln_314 = lineA_109, ln_314.ToZ - ln_314.FromZ)), (vBx_27 = ((ln_315 = lineB_109, ln_315.ToX - ln_315.FromX)), (vBy_27 = ((ln_316 = lineB_109, ln_316.ToY - ln_316.FromY)), (vBz_27 = ((ln_317 = lineB_109, ln_317.ToZ - ln_317.FromZ)), (crossX_20 = ((vAy_41 * vBz_27) - (vAz_41 * vBy_27)), (crossY_20 = ((vAz_41 * vBx_27) - (vAx_41 * vBz_27)), (crossZ_20 = ((vAx_41 * vBy_27) - (vAy_41 * vBx_27)), (dx_20 = (lineB_109.FromX - lineA_109.FromX), (dy_20 = (lineB_109.FromY - lineA_109.FromY), (dz_20 = (lineB_109.FromZ - lineA_109.FromZ), (t_35 = ((((((dy_20 * vBz_27) - (dz_20 * vBy_27)) * crossX_20) + (((dz_20 * vBx_27) - (dx_20 * vBz_27)) * crossY_20)) + (((dx_20 * vBy_27) - (dy_20 * vBx_27)) * crossZ_20)) / (((crossX_20 * crossX_20) + (crossY_20 * crossY_20)) + (crossZ_20 * crossZ_20))), ((t_35 > -1000000000000) && (t_35 < 1000000000000)) ? t_35 : undefined))))))))))))))), "Parameter too large should return None");
        Test_TestCaseBuilder__Zero(builder$0040_70);
    }));
})(), (() => {
    const builder$0040_71 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pnt/Vec overload", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_71, Test_TestCaseBuilder__Delay_1505(builder$0040_71, () => {
        let result_56;
        const pA_19 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pB_19 = Pnt_$ctor_Z7AD9E565(5, -5, 0);
        const vA_19 = Vec_$ctor_Z7AD9E565(10, 0, 0);
        const vB_19 = Vec_$ctor_Z7AD9E565(0, 10, 0);
        const vAx_42 = vA_19.X;
        const vAy_42 = vA_19.Y;
        const vAz_42 = vA_19.Z;
        const vBx_28 = vB_19.X;
        const vBy_28 = vB_19.Y;
        const vBz_28 = vB_19.Z;
        const crossX_21 = (vAy_42 * vBz_28) - (vAz_42 * vBy_28);
        const crossY_21 = (vAz_42 * vBx_28) - (vAx_42 * vBz_28);
        const crossZ_21 = (vAx_42 * vBy_28) - (vAy_42 * vBx_28);
        const dx_21 = pB_19.X - pA_19.X;
        const dy_21 = pB_19.Y - pA_19.Y;
        const dz_21 = pB_19.Z - pA_19.Z;
        const t_36 = (((((dy_21 * vBz_28) - (dz_21 * vBy_28)) * crossX_21) + (((dz_21 * vBx_28) - (dx_21 * vBz_28)) * crossY_21)) + (((dx_21 * vBy_28) - (dy_21 * vBx_28)) * crossZ_21)) / (((crossX_21 * crossX_21) + (crossY_21 * crossY_21)) + (crossZ_21 * crossZ_21));
        result_56 = (((t_36 > -1000000000000) && (t_36 < 1000000000000)) ? t_36 : undefined);
        if (result_56 == null) {
            Test_failtest("Should return a parameter");
            Test_TestCaseBuilder__Zero(builder$0040_71);
        }
        else {
            Expect_floatClose(AccuracyModule_high, result_56, 0.5, "Parameter should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_71);
        }
    }));
})(), (() => {
    const builder$0040_72 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Float components overload", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_72, Test_TestCaseBuilder__Delay_1505(builder$0040_72, () => {
        let result_57;
        const crossX_22 = (0 * 0) - (0 * 10);
        const crossY_22 = (0 * 0) - (10 * 0);
        const crossZ_22 = (10 * 10) - (0 * 0);
        const dx_22 = 5 - 0;
        const dy_22 = -5 - 0;
        const dz_22 = 0 - 0;
        const t_38 = (((((dy_22 * 0) - (dz_22 * 10)) * crossX_22) + (((dz_22 * 0) - (dx_22 * 0)) * crossY_22)) + (((dx_22 * 10) - (dy_22 * 0)) * crossZ_22)) / (((crossX_22 * crossX_22) + (crossY_22 * crossY_22)) + (crossZ_22 * crossZ_22));
        result_57 = (((t_38 > -1000000000000) && (t_38 < 1000000000000)) ? t_38 : undefined);
        if (result_57 == null) {
            Test_failtest("Should return a parameter");
            Test_TestCaseBuilder__Zero(builder$0040_72);
        }
        else {
            Expect_floatClose(AccuracyModule_high, result_57, 0.5, "Parameter should be 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_72);
        }
    }));
})()])), Test_testList("tryClosestPntRayA tests", ofArray([(() => {
    const builder$0040_73 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Intersecting rays - returns point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_73, Test_TestCaseBuilder__Delay_1505(builder$0040_73, () => {
        let result_58;
        const lineA_111 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_111 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, -5, 0), Pnt_$ctor_Z7AD9E565(5, 5, 0));
        const pAx_44 = lineA_111.FromX;
        const pAy_44 = lineA_111.FromY;
        const pAz_44 = lineA_111.FromZ;
        let vAx_44;
        const ln_318 = lineA_111;
        vAx_44 = (ln_318.ToX - ln_318.FromX);
        let vAy_44;
        const ln_319 = lineA_111;
        vAy_44 = (ln_319.ToY - ln_319.FromY);
        let vAz_44;
        const ln_320 = lineA_111;
        vAz_44 = (ln_320.ToZ - ln_320.FromZ);
        let vBx_30;
        const ln_321 = lineB_111;
        vBx_30 = (ln_321.ToX - ln_321.FromX);
        let vBy_30;
        const ln_322 = lineB_111;
        vBy_30 = (ln_322.ToY - ln_322.FromY);
        let vBz_30;
        const ln_323 = lineB_111;
        vBz_30 = (ln_323.ToZ - ln_323.FromZ);
        const crossX_23 = (vAy_44 * vBz_30) - (vAz_44 * vBy_30);
        const crossY_23 = (vAz_44 * vBx_30) - (vAx_44 * vBz_30);
        const crossZ_23 = (vAx_44 * vBy_30) - (vAy_44 * vBx_30);
        const dx_23 = lineB_111.FromX - pAx_44;
        const dy_23 = lineB_111.FromY - pAy_44;
        const dz_23 = lineB_111.FromZ - pAz_44;
        const t_40 = (((((dy_23 * vBz_30) - (dz_23 * vBy_30)) * crossX_23) + (((dz_23 * vBx_30) - (dx_23 * vBz_30)) * crossY_23)) + (((dx_23 * vBy_30) - (dy_23 * vBx_30)) * crossZ_23)) / (((crossX_23 * crossX_23) + (crossY_23 * crossY_23)) + (crossZ_23 * crossZ_23));
        result_58 = (((t_40 > -1000000000000) && (t_40 < 1000000000000)) ? Pnt_$ctor_Z7AD9E565_1(pAx_44 + (t_40 * vAx_44), pAy_44 + (t_40 * vAy_44), pAz_44 + (t_40 * vAz_44)) : undefined);
        if (result_58 == null) {
            Test_failtest("Should return a point");
            Test_TestCaseBuilder__Zero(builder$0040_73);
        }
        else {
            const pt_12 = result_58;
            Expect_floatClose(AccuracyModule_high, pt_12.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_12.Y, 0, "Y should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_12.Z, 0, "Z should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_73);
        }
    }));
})(), (() => {
    const builder$0040_74 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Parallel rays - returns None", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_74, Test_TestCaseBuilder__Delay_1505(builder$0040_74, () => {
        let lineA_113, lineB_113, pAx_45, pAy_45, pAz_45, vAx_45, ln_324, vAy_45, ln_325, vAz_45, ln_326, vBx_31, ln_327, vBy_31, ln_328, vBz_31, ln_329, crossX_24, crossY_24, crossZ_24, dx_24, dy_24, dz_24, t_41;
        Expect_isNone((lineA_113 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (lineB_113 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(10, 1, 0)), (pAx_45 = lineA_113.FromX, (pAy_45 = lineA_113.FromY, (pAz_45 = lineA_113.FromZ, (vAx_45 = ((ln_324 = lineA_113, ln_324.ToX - ln_324.FromX)), (vAy_45 = ((ln_325 = lineA_113, ln_325.ToY - ln_325.FromY)), (vAz_45 = ((ln_326 = lineA_113, ln_326.ToZ - ln_326.FromZ)), (vBx_31 = ((ln_327 = lineB_113, ln_327.ToX - ln_327.FromX)), (vBy_31 = ((ln_328 = lineB_113, ln_328.ToY - ln_328.FromY)), (vBz_31 = ((ln_329 = lineB_113, ln_329.ToZ - ln_329.FromZ)), (crossX_24 = ((vAy_45 * vBz_31) - (vAz_45 * vBy_31)), (crossY_24 = ((vAz_45 * vBx_31) - (vAx_45 * vBz_31)), (crossZ_24 = ((vAx_45 * vBy_31) - (vAy_45 * vBx_31)), (dx_24 = (lineB_113.FromX - pAx_45), (dy_24 = (lineB_113.FromY - pAy_45), (dz_24 = (lineB_113.FromZ - pAz_45), (t_41 = ((((((dy_24 * vBz_31) - (dz_24 * vBy_31)) * crossX_24) + (((dz_24 * vBx_31) - (dx_24 * vBz_31)) * crossY_24)) + (((dx_24 * vBy_31) - (dy_24 * vBx_31)) * crossZ_24)) / (((crossX_24 * crossX_24) + (crossY_24 * crossY_24)) + (crossZ_24 * crossZ_24))), ((t_41 > -1000000000000) && (t_41 < 1000000000000)) ? Pnt_$ctor_Z7AD9E565_1(pAx_45 + (t_41 * vAx_45), pAy_45 + (t_41 * vAy_45), pAz_45 + (t_41 * vAz_45)) : undefined)))))))))))))))))), "Parallel rays should return None");
        Test_TestCaseBuilder__Zero(builder$0040_74);
    }));
})(), (() => {
    const builder$0040_75 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skew rays - returns closest point on A", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_75, Test_TestCaseBuilder__Delay_1505(builder$0040_75, () => {
        let result_60;
        const lineA_115 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        const lineB_115 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 1), Pnt_$ctor_Z7AD9E565(5, 10, 1));
        const pAx_46 = lineA_115.FromX;
        const pAy_46 = lineA_115.FromY;
        const pAz_46 = lineA_115.FromZ;
        let vAx_46;
        const ln_330 = lineA_115;
        vAx_46 = (ln_330.ToX - ln_330.FromX);
        let vAy_46;
        const ln_331 = lineA_115;
        vAy_46 = (ln_331.ToY - ln_331.FromY);
        let vAz_46;
        const ln_332 = lineA_115;
        vAz_46 = (ln_332.ToZ - ln_332.FromZ);
        let vBx_32;
        const ln_333 = lineB_115;
        vBx_32 = (ln_333.ToX - ln_333.FromX);
        let vBy_32;
        const ln_334 = lineB_115;
        vBy_32 = (ln_334.ToY - ln_334.FromY);
        let vBz_32;
        const ln_335 = lineB_115;
        vBz_32 = (ln_335.ToZ - ln_335.FromZ);
        const crossX_25 = (vAy_46 * vBz_32) - (vAz_46 * vBy_32);
        const crossY_25 = (vAz_46 * vBx_32) - (vAx_46 * vBz_32);
        const crossZ_25 = (vAx_46 * vBy_32) - (vAy_46 * vBx_32);
        const dx_25 = lineB_115.FromX - pAx_46;
        const dy_25 = lineB_115.FromY - pAy_46;
        const dz_25 = lineB_115.FromZ - pAz_46;
        const t_42 = (((((dy_25 * vBz_32) - (dz_25 * vBy_32)) * crossX_25) + (((dz_25 * vBx_32) - (dx_25 * vBz_32)) * crossY_25)) + (((dx_25 * vBy_32) - (dy_25 * vBx_32)) * crossZ_25)) / (((crossX_25 * crossX_25) + (crossY_25 * crossY_25)) + (crossZ_25 * crossZ_25));
        result_60 = (((t_42 > -1000000000000) && (t_42 < 1000000000000)) ? Pnt_$ctor_Z7AD9E565_1(pAx_46 + (t_42 * vAx_46), pAy_46 + (t_42 * vAy_46), pAz_46 + (t_42 * vAz_46)) : undefined);
        if (result_60 == null) {
            Test_failtest("Should return a point");
            Test_TestCaseBuilder__Zero(builder$0040_75);
        }
        else {
            const pt_13 = result_60;
            Expect_floatClose(AccuracyModule_high, pt_13.X, 5, "X should be 5.0");
            Expect_floatClose(AccuracyModule_high, pt_13.Y, 0, "Y should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_13.Z, 0, "Z should be 0.0");
            Test_TestCaseBuilder__Zero(builder$0040_75);
        }
    }));
})(), (() => {
    const builder$0040_76 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("3D rays - returns point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_76, Test_TestCaseBuilder__Delay_1505(builder$0040_76, () => {
        let result_61;
        const lineA_117 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10));
        const lineB_117 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(5, 0, 5), Pnt_$ctor_Z7AD9E565(-5, 0, 5));
        const pAx_47 = lineA_117.FromX;
        const pAy_47 = lineA_117.FromY;
        const pAz_47 = lineA_117.FromZ;
        let vAx_47;
        const ln_336 = lineA_117;
        vAx_47 = (ln_336.ToX - ln_336.FromX);
        let vAy_47;
        const ln_337 = lineA_117;
        vAy_47 = (ln_337.ToY - ln_337.FromY);
        let vAz_47;
        const ln_338 = lineA_117;
        vAz_47 = (ln_338.ToZ - ln_338.FromZ);
        let vBx_33;
        const ln_339 = lineB_117;
        vBx_33 = (ln_339.ToX - ln_339.FromX);
        let vBy_33;
        const ln_340 = lineB_117;
        vBy_33 = (ln_340.ToY - ln_340.FromY);
        let vBz_33;
        const ln_341 = lineB_117;
        vBz_33 = (ln_341.ToZ - ln_341.FromZ);
        const crossX_26 = (vAy_47 * vBz_33) - (vAz_47 * vBy_33);
        const crossY_26 = (vAz_47 * vBx_33) - (vAx_47 * vBz_33);
        const crossZ_26 = (vAx_47 * vBy_33) - (vAy_47 * vBx_33);
        const dx_26 = lineB_117.FromX - pAx_47;
        const dy_26 = lineB_117.FromY - pAy_47;
        const dz_26 = lineB_117.FromZ - pAz_47;
        const t_43 = (((((dy_26 * vBz_33) - (dz_26 * vBy_33)) * crossX_26) + (((dz_26 * vBx_33) - (dx_26 * vBz_33)) * crossY_26)) + (((dx_26 * vBy_33) - (dy_26 * vBx_33)) * crossZ_26)) / (((crossX_26 * crossX_26) + (crossY_26 * crossY_26)) + (crossZ_26 * crossZ_26));
        result_61 = (((t_43 > -1000000000000) && (t_43 < 1000000000000)) ? Pnt_$ctor_Z7AD9E565_1(pAx_47 + (t_43 * vAx_47), pAy_47 + (t_43 * vAy_47), pAz_47 + (t_43 * vAz_47)) : undefined);
        if (result_61 == null) {
            Test_failtest("Should return a point");
            Test_TestCaseBuilder__Zero(builder$0040_76);
        }
        else {
            const pt_14 = result_61;
            Expect_floatClose(AccuracyModule_high, pt_14.X, 0, "X should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_14.Y, 0, "Y should be 0.0");
            Expect_floatClose(AccuracyModule_high, pt_14.Z, 5, "Z should be 5.0");
            Test_TestCaseBuilder__Zero(builder$0040_76);
        }
    }));
})(), (() => {
    const builder$0040_77 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Pnt/Vec overload", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_77, Test_TestCaseBuilder__Delay_1505(builder$0040_77, () => {
        let result_62;
        const pA_21 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const pB_21 = Pnt_$ctor_Z7AD9E565(5, -5, 0);
        const vA_21 = Vec_$ctor_Z7AD9E565(10, 0, 0);
        const vB_21 = Vec_$ctor_Z7AD9E565(0, 10, 0);
        const pAx_48 = pA_21.X;
        const pAy_48 = pA_21.Y;
        const pAz_48 = pA_21.Z;
        const vAx_48 = vA_21.X;
        const vAy_48 = vA_21.Y;
        const vAz_48 = vA_21.Z;
        const vBx_34 = vB_21.X;
        const vBy_34 = vB_21.Y;
        const vBz_34 = vB_21.Z;
        const crossX_27 = (vAy_48 * vBz_34) - (vAz_48 * vBy_34);
        const crossY_27 = (vAz_48 * vBx_34) - (vAx_48 * vBz_34);
        const crossZ_27 = (vAx_48 * vBy_34) - (vAy_48 * vBx_34);
        const dx_27 = pB_21.X - pAx_48;
        const dy_27 = pB_21.Y - pAy_48;
        const dz_27 = pB_21.Z - pAz_48;
        const t_44 = (((((dy_27 * vBz_34) - (dz_27 * vBy_34)) * crossX_27) + (((dz_27 * vBx_34) - (dx_27 * vBz_34)) * crossY_27)) + (((dx_27 * vBy_34) - (dy_27 * vBx_34)) * crossZ_27)) / (((crossX_27 * crossX_27) + (crossY_27 * crossY_27)) + (crossZ_27 * crossZ_27));
        result_62 = (((t_44 > -1000000000000) && (t_44 < 1000000000000)) ? Pnt_$ctor_Z7AD9E565_1(pAx_48 + (t_44 * vAx_48), pAy_48 + (t_44 * vAy_48), pAz_48 + (t_44 * vAz_48)) : undefined);
        if (result_62 == null) {
            Test_failtest("Should return a point");
            Test_TestCaseBuilder__Zero(builder$0040_77);
        }
        else {
            Expect_floatClose(AccuracyModule_high, result_62.X, 5, "X should be 5.0");
            Test_TestCaseBuilder__Zero(builder$0040_77);
        }
    }));
})(), (() => {
    const builder$0040_78 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Float components overload", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_78, Test_TestCaseBuilder__Delay_1505(builder$0040_78, () => {
        let result_63;
        const crossX_28 = (0 * 0) - (0 * 10);
        const crossY_28 = (0 * 0) - (10 * 0);
        const crossZ_28 = (10 * 10) - (0 * 0);
        const dx_28 = 5 - 0;
        const dy_28 = -5 - 0;
        const dz_28 = 0 - 0;
        const t_45 = (((((dy_28 * 0) - (dz_28 * 10)) * crossX_28) + (((dz_28 * 0) - (dx_28 * 0)) * crossY_28)) + (((dx_28 * 10) - (dy_28 * 0)) * crossZ_28)) / (((crossX_28 * crossX_28) + (crossY_28 * crossY_28)) + (crossZ_28 * crossZ_28));
        result_63 = (((t_45 > -1000000000000) && (t_45 < 1000000000000)) ? Pnt_$ctor_Z7AD9E565_1(0 + (t_45 * 10), 0 + (t_45 * 0), 0 + (t_45 * 0)) : undefined);
        if (result_63 == null) {
            Test_failtest("Should return a point");
            Test_TestCaseBuilder__Zero(builder$0040_78);
        }
        else {
            Expect_floatClose(AccuracyModule_high, result_63.X, 5, "X should be 5.0");
            Test_TestCaseBuilder__Zero(builder$0040_78);
        }
    }));
})()]))]));

