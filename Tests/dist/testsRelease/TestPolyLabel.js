
import { AccuracyModule_medium, Test_TestCaseBuilder__Zero, AccuracyModule_low, Expect_floatClose, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Polyline2D__get_BoundingRectangle, Polyline2D__FindLablePoint_5E38073B } from "./Src/Polyline2D.js";
import { Polyline2D_$ctor_Z5FD8CF3C } from "./Src/Polyline2D.js";
import { ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";

export const tests = Test_testList("PolyLabel", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Square center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_2, b_2, vx, vy;
        const pts = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1), Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(0, 0)];
        const patternInput = Polyline2D__FindLablePoint_5E38073B(Polyline2D_$ctor_Z5FD8CF3C(Array.from(pts)), 1E-06);
        const p = patternInput[0];
        Expect_isTrue(((a_2 = p, (b_2 = Pt_$ctor_7B00E9A0(0.5, 0.5), (vx = (a_2.X - b_2.X), (vy = (a_2.Y - b_2.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 0.001)(`expected center got ${p}`);
        Expect_floatClose(AccuracyModule_low, patternInput[1], 0.5, "distance = 0.5");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rectangle center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_4, b_4, vx_1, vy_1;
        const pts_1 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(4, 0), Pt_$ctor_7B00E9A0(4, 2), Pt_$ctor_7B00E9A0(0, 2), Pt_$ctor_7B00E9A0(0, 0)];
        const patternInput_1 = Polyline2D__FindLablePoint_5E38073B(Polyline2D_$ctor_Z5FD8CF3C(Array.from(pts_1)), 0.001);
        const p_1 = patternInput_1[0];
        Expect_isTrue(((a_4 = p_1, (b_4 = Pt_$ctor_7B00E9A0(2, 1), (vx_1 = (a_4.X - b_4.X), (vy_1 = (a_4.Y - b_4.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 0.001)(`expected center got ${p_1}`);
        Expect_floatClose(AccuracyModule_low, patternInput_1[1], 1, "distance = min half-side");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Right triangle incenter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_6, b_6, vx_2, vy_2;
        const pts_2 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(4, 0), Pt_$ctor_7B00E9A0(0, 3), Pt_$ctor_7B00E9A0(0, 0)];
        const patternInput_2 = Polyline2D__FindLablePoint_5E38073B(Polyline2D_$ctor_Z5FD8CF3C(Array.from(pts_2)), 0.0001);
        const p_2 = patternInput_2[0];
        const per = (5 + 3) + 4;
        const expected = Pt_$ctor_7B00E9A0((((5 * 0) + (3 * 4)) + (4 * 0)) / per, (((5 * 0) + (3 * 0)) + (4 * 3)) / per);
        Expect_isTrue(((a_6 = p_2, (b_6 = expected, (vx_2 = (a_6.X - b_6.X), (vy_2 = (a_6.Y - b_6.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 0.001)(`expected incenter ${expected} got ${p_2}`);
        Expect_floatClose(AccuracyModule_low, patternInput_2[1], 1, "inradius for 3-4-5 triangle is 1");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Precision refinement", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_8, b_8, vx_3, vy_3, a_10, b_10, vx_4, vy_4;
        const pts_3 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(2, 2), Pt_$ctor_7B00E9A0(0, 2), Pt_$ctor_7B00E9A0(0, 0)];
        const pl_3 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(pts_3));
        const patternInput_3 = Polyline2D__FindLablePoint_5E38073B(pl_3, 0.1);
        const patternInput_4 = Polyline2D__FindLablePoint_5E38073B(pl_3, 0.0001);
        Expect_isTrue(patternInput_4[1] >= (patternInput_3[1] - 1E-06))("distance monotonic");
        Expect_isTrue(((a_8 = patternInput_4[0], (b_8 = Pt_$ctor_7B00E9A0(1, 1), (vx_3 = (a_8.X - b_8.X), (vy_3 = (a_8.Y - b_8.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < (((a_10 = patternInput_3[0], (b_10 = Pt_$ctor_7B00E9A0(1, 1), (vx_4 = (a_10.X - b_10.X), (vy_4 = (a_10.Y - b_10.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) + 0.01))("higher precision closer to center or similar");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Equilateral triangle incenter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_12, b_12, vx_5, vy_5;
        const h = Math.sqrt(3);
        const pts_4 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(1, h), Pt_$ctor_7B00E9A0(0, 0)];
        const patternInput_5 = Polyline2D__FindLablePoint_5E38073B(Polyline2D_$ctor_Z5FD8CF3C(Array.from(pts_4)), 0.0001);
        const p_3 = patternInput_5[0];
        const expected_1 = Pt_$ctor_7B00E9A0(1, h / 3);
        Expect_isTrue(((a_12 = p_3, (b_12 = expected_1, (vx_5 = (a_12.X - b_12.X), (vy_5 = (a_12.Y - b_12.Y), Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5))))))) < 0.001)(`expected ${expected_1} got ${p_3}`);
        Expect_floatClose(AccuracyModule_low, patternInput_5[1], h / 3, "inradius eq tri");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Thin rectangle center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_14, b_14, vx_6, vy_6;
        const pts_5 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(10, 0), Pt_$ctor_7B00E9A0(10, 0.2), Pt_$ctor_7B00E9A0(0, 0.2), Pt_$ctor_7B00E9A0(0, 0)];
        const patternInput_6 = Polyline2D__FindLablePoint_5E38073B(Polyline2D_$ctor_Z5FD8CF3C(Array.from(pts_5)), 0.001);
        const p_4 = patternInput_6[0];
        Expect_isTrue(((a_14 = p_4, (b_14 = Pt_$ctor_7B00E9A0(5, 0.1), (vx_6 = (a_14.X - b_14.X), (vy_6 = (a_14.Y - b_14.Y), Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6))))))) < 0.001)(`expected near center got ${p_4}`);
        Expect_floatClose(AccuracyModule_low, patternInput_6[1], 0.1, "half of minor side");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Diamond center", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_16, b_16, vx_7, vy_7;
        const pts_6 = [Pt_$ctor_7B00E9A0(0, 1), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, -1), Pt_$ctor_7B00E9A0(-1, 0), Pt_$ctor_7B00E9A0(0, 1)];
        const patternInput_7 = Polyline2D__FindLablePoint_5E38073B(Polyline2D_$ctor_Z5FD8CF3C(Array.from(pts_6)), 0.0001);
        const p_5 = patternInput_7[0];
        Expect_isTrue(((a_16 = p_5, (b_16 = Pt_$ctor_7B00E9A0(0, 0), (vx_7 = (a_16.X - b_16.X), (vy_7 = (a_16.Y - b_16.Y), Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7))))))) < 0.0001)(`expected origin got ${p_5}`);
        Expect_floatClose(AccuracyModule_medium, patternInput_7[1], 1 / Math.sqrt(2), "diamond inradius");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Skinny polygon", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const pts_7 = [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(5, 0), Pt_$ctor_7B00E9A0(5, 0.05), Pt_$ctor_7B00E9A0(4, 0.05), Pt_$ctor_7B00E9A0(4, 0.1), Pt_$ctor_7B00E9A0(3, 0.1), Pt_$ctor_7B00E9A0(3, 0.05), Pt_$ctor_7B00E9A0(0, 0.05), Pt_$ctor_7B00E9A0(0, 0)];
        const patternInput_8 = Polyline2D__FindLablePoint_5E38073B(Polyline2D_$ctor_Z5FD8CF3C(Array.from(pts_7)), 0.001);
        const p_6 = patternInput_8[0];
        const d_6 = patternInput_8[1];
        Expect_isTrue((p_6.Y > 0.02) && (p_6.Y < 0.08))(`y inside strip ${p_6}`);
        Expect_isTrue((d_6 < 0.06) && (d_6 > 0.02))(`distance within width bounds ${d_6}`);
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Large coords stability", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const poly119Pts = [Pt_$ctor_7B00E9A0(-111.1815838, 45.7768091), Pt_$ctor_7B00E9A0(-111.1816944, 45.776695), Pt_$ctor_7B00E9A0(-111.1812484, 45.7764847), Pt_$ctor_7B00E9A0(-111.1811378, 45.7765988), Pt_$ctor_7B00E9A0(-111.1815838, 45.7768091)];
        const pl_8 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(poly119Pts));
        const patternInput_9 = Polyline2D__FindLablePoint_5E38073B(pl_8, 0.001);
        const p_7 = patternInput_9[0];
        const d_7 = patternInput_9[1];
        const bb = Polyline2D__get_BoundingRectangle(pl_8);
        Expect_isTrue((((p_7.X >= bb.MinX) && (p_7.X <= bb.MaxX)) && (p_7.Y >= bb.MinY)) && (p_7.Y <= bb.MaxY))(`point outside bbox ${p_7}`);
        Expect_isTrue(d_7 > 0)(`expected positive distance got ${d_7}`);
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Large coords stability high precision", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const poly119Pts_1 = [Pt_$ctor_7B00E9A0(-111.1815838, 45.7768091), Pt_$ctor_7B00E9A0(-111.1816944, 45.776695), Pt_$ctor_7B00E9A0(-111.1812484, 45.7764847), Pt_$ctor_7B00E9A0(-111.1811378, 45.7765988), Pt_$ctor_7B00E9A0(-111.1815838, 45.7768091)];
        const pl_9 = Polyline2D_$ctor_Z5FD8CF3C(Array.from(poly119Pts_1));
        const patternInput_10 = Polyline2D__FindLablePoint_5E38073B(pl_9, 1E-08);
        const p_8 = patternInput_10[0];
        const d_8 = patternInput_10[1];
        const bb_1 = Polyline2D__get_BoundingRectangle(pl_9);
        Expect_isTrue((((p_8.X >= bb_1.MinX) && (p_8.X <= bb_1.MaxX)) && (p_8.Y >= bb_1.MinY)) && (p_8.Y <= bb_1.MaxY))(`point outside bbox ${p_8}`);
        Expect_isTrue(d_8 > 0)(`expected positive distance got ${d_8}`);
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})()]));

