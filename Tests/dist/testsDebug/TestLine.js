
import { nonSeeded } from "./fable_modules/fable-library-js.5.0.0/Random.js";
import { printf, toText, toConsole } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Expect_throws, Expect_isFalse, Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, Expect_isTrue } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { empty, collect, singleton, append, delay, toList } from "./fable_modules/fable-library-js.5.0.0/Seq.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Line2D_$ctor_77D16AC0 } from "./Src/Line2D.js";
import { failUnit3, failTooSmall, failTooSmall2 } from "./Src/EuclidErrors.js";
import { Vc_$ctor_7B00E9A0 } from "./Src/Vc.js";
import { Rotation2D_$ctor_7B00E9A0 } from "./Src/Rotation2D.js";
import { Vc_$ctor_7B00E9A0 as Vc_$ctor_7B00E9A0_1 } from "./Src/Vc.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { Line2D_$ctor_77D16AC0 as Line2D_$ctor_77D16AC0_1 } from "./Src/Line2D.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_1 } from "./Src/Pt.js";
import { rangeDouble } from "./fable_modules/fable-library-js.5.0.0/Range.js";
import { UnitVc_$ctor_7B00E9A0 } from "./Src/UnitVc.js";
import { Vc_$ctor_7B00E9A0 as Vc_$ctor_7B00E9A0_2 } from "./Src/Vc.js";
import { Pt_$ctor_7B00E9A0 as Pt_$ctor_7B00E9A0_2 } from "./Src/Pt.js";
import { Euclid_Line2D__Line2D_tryIntersectOrOverlap_Static, Euclid_Line2D__Line2D_tryIntersectRay_Static, Euclid_Line2D__Line2D_tryIntersect_Static, Euclid_Line2D__Line2D_doIntersectOrOverlap_Static, Euclid_Line2D__Line2D_doIntersect_Static, Euclid_Line2D__Line2D_offset_Static, Euclid_Line2D__Line2D_divideInsideEvery_Static, Euclid_Line2D__Line2D_divideEvery_Static, Euclid_Line2D__Line2D_splitMaxLength_Static, Euclid_Line2D__Line2D_splitMinLength_Static, Euclid_Line2D__Line2D_split_Static, Euclid_Line2D__Line2D_divideMaxLength_Static, Euclid_Line2D__Line2D_divideMinLength_Static, Euclid_Line2D__Line2D_divide_Static, Euclid_Line2D__Line2D_tryProjectOntoLine_Static, Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static, Euclid_Line2D__Line2D_projectOntoRay_Static, Euclid_Line2D__Line2D_projectOntoRayParam_Static, Euclid_Line2D__Line2D_SqDistanceFromPoint_6ADE94FD, Euclid_Line2D__Line2D_ClosestPoint_6ADE94FD, Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD, Euclid_Line2D__Line2D_distanceToLine_Static } from "./Src/TypeExtensions/Line2D.js";
import { contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { int32ToString, structuralHash, assertEqual, Exception } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { XLine2D_getEndsTouching_Z44565CE5 } from "./Src/XLine2D.js";
import { Line3D_$ctor_76A78260 } from "./Src/Line3D.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Line3D_$ctor_76A78260 as Line3D_$ctor_76A78260_1 } from "./Src/Line3D.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Quaternion_createFromRadians_Z3D1F83EE } from "./Src/Quaternion.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { Line3D_$ctor_76A78260 as Line3D_$ctor_76A78260_2 } from "./Src/Line3D.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_2 } from "./Src/Pnt.js";
import { Euclid_Line3D__Line3D_tryGetOverlap_Static, Euclid_Line3D__Line3D_closestParameters_Static, Euclid_Line3D__Line3D_closestPoints_Static, Euclid_Line3D__Line3D_sqDistanceToLine_Static, Euclid_Line3D__Line3D_distanceToLine_Static, Euclid_Line3D__Line3D_tryIntersectRay_Static, Euclid_Line3D__Line3D_tryIntersect_Static, Euclid_Line3D__Line3D_doIntersectOrOverlap_Static, Euclid_Line3D__Line3D_doRaysIntersect_Static, Euclid_Line3D__Line3D_doIntersect_Static, Euclid_Line3D__Line3D_offset_Static, Euclid_Line3D__Line3D_offsetXY_Static, Euclid_Line3D__Line3D_divideInsideEvery_Static, Euclid_Line3D__Line3D_divideEvery_Static, Euclid_Line3D__Line3D_splitMaxLength_Static, Euclid_Line3D__Line3D_splitMinLength_Static, Euclid_Line3D__Line3D_split_Static, Euclid_Line3D__Line3D_divideMaxLength_Static, Euclid_Line3D__Line3D_divideMinLength_Static, Euclid_Line3D__Line3D_divide_Static, Euclid_Line3D__Line3D_tryProjectOntoLine_Static, Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static, Euclid_Line3D__Line3D_projectOntoRay_Static, Euclid_Line3D__Line3D_projectOntoRayParam_Static } from "./Src/TypeExtensions/Line3D.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_2 } from "./Src/Vec.js";
import { XLine3D_getEndsTouching_Z4454D4C5 } from "./Src/XLine3D.js";

export const rnd = nonSeeded();

export function rFloat(min, max) {
    return (rnd.NextDouble() * (max - min)) + min;
}

export function expectEqualEpsilon(a, b, txt) {
    const ok = Math.abs(a - b) < 1E-09;
    if (!ok) {
        toConsole(`expectEqualEpsilon: ${b}, and: ${a}`);
    }
    Expect_isTrue(ok)(txt);
}

export const testsIsCoincident = Test_testList("Line2D IsCoincidentTo", toList(delay(() => {
    let builder$0040;
    return append(singleton((builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo identical lines", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let ln, other, vtX, ln_1, vtY, ln_2, voX, ln_3, voY, ln_4, dot, det, tan, value, x_3, y, dotN, lenSq, dist;
        const a = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_isTrue((ln = a, (other = b, (vtX = ((ln_1 = ln, ln_1.ToX - ln_1.FromX)), (vtY = ((ln_2 = ln, ln_2.ToY - ln_2.FromY)), (voX = ((ln_3 = other, ln_3.ToX - ln_3.FromX)), (voY = ((ln_4 = other, ln_4.ToY - ln_4.FromY)), (!(((vtX * vtX) + (vtY * vtY)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln, other) : undefined, (!(((voX * voX) + (voY * voY)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other, ln) : undefined, (dot = ((vtX * voX) + (vtY * voY)), (det = ((vtX * voY) - (vtY * voX)), (tan = ((value = (det / dot), Math.abs(value))), (tan < 0.004363350820701567) && ((x_3 = (other.FromX - ln.FromX), (y = (other.FromY - ln.FromY), (dotN = ((x_3 * vtY) - (y * vtX)), (lenSq = ((vtX * vtX) + (vtY * vtY)), (dist = ((dotN * dotN) / lenSq), dist < 1E-06))))))))))))))))))("identical lines are coincident");
        Test_TestCaseBuilder__Zero(builder$0040);
    })))), delay(() => {
        let builder$0040_1;
        return append(singleton((builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo same ray different lengths", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
            let ln_5, other_1, vtX_1, ln_6, vtY_1, ln_7, voX_1, ln_8, voY_1, ln_9, dot_1, det_1, tan_1, value_1, x_7, y_1, dotN_1, lenSq_1, dist_1;
            const a_1 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
            const b_1 = Line2D_$ctor_77D16AC0(0, 0, 5, 0);
            Expect_isTrue((ln_5 = a_1, (other_1 = b_1, (vtX_1 = ((ln_6 = ln_5, ln_6.ToX - ln_6.FromX)), (vtY_1 = ((ln_7 = ln_5, ln_7.ToY - ln_7.FromY)), (voX_1 = ((ln_8 = other_1, ln_8.ToX - ln_8.FromX)), (voY_1 = ((ln_9 = other_1, ln_9.ToY - ln_9.FromY)), (!(((vtX_1 * vtX_1) + (vtY_1 * vtY_1)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_5, other_1) : undefined, (!(((voX_1 * voX_1) + (voY_1 * voY_1)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_1, ln_5) : undefined, (dot_1 = ((vtX_1 * voX_1) + (vtY_1 * voY_1)), (det_1 = ((vtX_1 * voY_1) - (vtY_1 * voX_1)), (tan_1 = ((value_1 = (det_1 / dot_1), Math.abs(value_1))), (tan_1 < 0.004363350820701567) && ((x_7 = (other_1.FromX - ln_5.FromX), (y_1 = (other_1.FromY - ln_5.FromY), (dotN_1 = ((x_7 * vtY_1) - (y_1 * vtX_1)), (lenSq_1 = ((vtX_1 * vtX_1) + (vtY_1 * vtY_1)), (dist_1 = ((dotN_1 * dotN_1) / lenSq_1), dist_1 < 1E-06))))))))))))))))))("same ray different lengths are coincident");
            Test_TestCaseBuilder__Zero(builder$0040_1);
        })))), delay(() => {
            let builder$0040_2;
            return append(singleton((builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo same ray offset start", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
                let ln_10, other_2, vtX_2, ln_11, vtY_2, ln_12, voX_2, ln_13, voY_2, ln_14, dot_2, det_2, tan_2, value_2, x_11, y_2, dotN_2, lenSq_2, dist_2;
                const a_2 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                const b_2 = Line2D_$ctor_77D16AC0(5, 0, 15, 0);
                Expect_isTrue((ln_10 = a_2, (other_2 = b_2, (vtX_2 = ((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)), (vtY_2 = ((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)), (voX_2 = ((ln_13 = other_2, ln_13.ToX - ln_13.FromX)), (voY_2 = ((ln_14 = other_2, ln_14.ToY - ln_14.FromY)), (!(((vtX_2 * vtX_2) + (vtY_2 * vtY_2)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_10, other_2) : undefined, (!(((voX_2 * voX_2) + (voY_2 * voY_2)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_2, ln_10) : undefined, (dot_2 = ((vtX_2 * voX_2) + (vtY_2 * voY_2)), (det_2 = ((vtX_2 * voY_2) - (vtY_2 * voX_2)), (tan_2 = ((value_2 = (det_2 / dot_2), Math.abs(value_2))), (tan_2 < 0.004363350820701567) && ((x_11 = (other_2.FromX - ln_10.FromX), (y_2 = (other_2.FromY - ln_10.FromY), (dotN_2 = ((x_11 * vtY_2) - (y_2 * vtX_2)), (lenSq_2 = ((vtX_2 * vtX_2) + (vtY_2 * vtY_2)), (dist_2 = ((dotN_2 * dotN_2) / lenSq_2), dist_2 < 1E-06))))))))))))))))))("same ray offset start are coincident");
                Test_TestCaseBuilder__Zero(builder$0040_2);
            })))), delay(() => {
                let builder$0040_3;
                return append(singleton((builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo parallel but offset vertically", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
                    let ln_15, other_3, vtX_3, ln_16, vtY_3, ln_17, voX_3, ln_18, voY_3, ln_19, dot_3, det_3, tan_3, value_3, x_15, y_3, dotN_3, lenSq_3, dist_3;
                    const a_3 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                    const b_3 = Line2D_$ctor_77D16AC0(0, 2, 10, 2);
                    Expect_isFalse((ln_15 = a_3, (other_3 = b_3, (vtX_3 = ((ln_16 = ln_15, ln_16.ToX - ln_16.FromX)), (vtY_3 = ((ln_17 = ln_15, ln_17.ToY - ln_17.FromY)), (voX_3 = ((ln_18 = other_3, ln_18.ToX - ln_18.FromX)), (voY_3 = ((ln_19 = other_3, ln_19.ToY - ln_19.FromY)), (!(((vtX_3 * vtX_3) + (vtY_3 * vtY_3)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_15, other_3) : undefined, (!(((voX_3 * voX_3) + (voY_3 * voY_3)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_3, ln_15) : undefined, (dot_3 = ((vtX_3 * voX_3) + (vtY_3 * voY_3)), (det_3 = ((vtX_3 * voY_3) - (vtY_3 * voX_3)), (tan_3 = ((value_3 = (det_3 / dot_3), Math.abs(value_3))), (tan_3 < 0.004363350820701567) && ((x_15 = (other_3.FromX - ln_15.FromX), (y_3 = (other_3.FromY - ln_15.FromY), (dotN_3 = ((x_15 * vtY_3) - (y_3 * vtX_3)), (lenSq_3 = ((vtX_3 * vtX_3) + (vtY_3 * vtY_3)), (dist_3 = ((dotN_3 * dotN_3) / lenSq_3), dist_3 < 1E-06))))))))))))))))))("parallel lines with vertical offset should not be coincident");
                    Test_TestCaseBuilder__Zero(builder$0040_3);
                })))), delay(() => {
                    let builder$0040_4;
                    return append(singleton((builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo parallel within distance tolerance", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
                        let ln_20, other_4, vtX_4, ln_21, vtY_4, ln_22, voX_4, ln_23, voY_4, ln_24, dot_4, det_4, tan_4, value_4, x_19, y_4, dotN_4, lenSq_4, dist_4;
                        const a_4 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                        const b_4 = Line2D_$ctor_77D16AC0(0, 1E-07, 10, 1E-07);
                        Expect_isTrue((ln_20 = a_4, (other_4 = b_4, (vtX_4 = ((ln_21 = ln_20, ln_21.ToX - ln_21.FromX)), (vtY_4 = ((ln_22 = ln_20, ln_22.ToY - ln_22.FromY)), (voX_4 = ((ln_23 = other_4, ln_23.ToX - ln_23.FromX)), (voY_4 = ((ln_24 = other_4, ln_24.ToY - ln_24.FromY)), (!(((vtX_4 * vtX_4) + (vtY_4 * vtY_4)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_20, other_4) : undefined, (!(((voX_4 * voX_4) + (voY_4 * voY_4)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_4, ln_20) : undefined, (dot_4 = ((vtX_4 * voX_4) + (vtY_4 * voY_4)), (det_4 = ((vtX_4 * voY_4) - (vtY_4 * voX_4)), (tan_4 = ((value_4 = (det_4 / dot_4), Math.abs(value_4))), (tan_4 < 0.004363350820701567) && ((x_19 = (other_4.FromX - ln_20.FromX), (y_4 = (other_4.FromY - ln_20.FromY), (dotN_4 = ((x_19 * vtY_4) - (y_4 * vtX_4)), (lenSq_4 = ((vtX_4 * vtX_4) + (vtY_4 * vtY_4)), (dist_4 = ((dotN_4 * dotN_4) / lenSq_4), dist_4 < 1E-06))))))))))))))))))("parallel lines within distance tolerance are coincident");
                        Test_TestCaseBuilder__Zero(builder$0040_4);
                    })))), delay(() => {
                        let builder$0040_5;
                        return append(singleton((builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo parallel outside distance tolerance", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
                            let ln_25, other_5, vtX_5, ln_26, vtY_5, ln_27, voX_5, ln_28, voY_5, ln_29, dot_5, det_5, tan_5, value_5, x_23, y_5, dotN_5, lenSq_5, dist_5;
                            const a_5 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                            const b_5 = Line2D_$ctor_77D16AC0(0, 0.001, 10, 0.001);
                            Expect_isFalse((ln_25 = a_5, (other_5 = b_5, (vtX_5 = ((ln_26 = ln_25, ln_26.ToX - ln_26.FromX)), (vtY_5 = ((ln_27 = ln_25, ln_27.ToY - ln_27.FromY)), (voX_5 = ((ln_28 = other_5, ln_28.ToX - ln_28.FromX)), (voY_5 = ((ln_29 = other_5, ln_29.ToY - ln_29.FromY)), (!(((vtX_5 * vtX_5) + (vtY_5 * vtY_5)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_25, other_5) : undefined, (!(((voX_5 * voX_5) + (voY_5 * voY_5)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_5, ln_25) : undefined, (dot_5 = ((vtX_5 * voX_5) + (vtY_5 * voY_5)), (det_5 = ((vtX_5 * voY_5) - (vtY_5 * voX_5)), (tan_5 = ((value_5 = (det_5 / dot_5), Math.abs(value_5))), (tan_5 < 0.004363350820701567) && ((x_23 = (other_5.FromX - ln_25.FromX), (y_5 = (other_5.FromY - ln_25.FromY), (dotN_5 = ((x_23 * vtY_5) - (y_5 * vtX_5)), (lenSq_5 = ((vtX_5 * vtX_5) + (vtY_5 * vtY_5)), (dist_5 = ((dotN_5 * dotN_5) / lenSq_5), dist_5 < 1E-06))))))))))))))))))("parallel lines outside distance tolerance are not coincident");
                            Test_TestCaseBuilder__Zero(builder$0040_5);
                        })))), delay(() => {
                            let builder$0040_6;
                            return append(singleton((builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo perpendicular lines", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
                                let ln_30, other_6, vtX_6, ln_31, vtY_6, ln_32, voX_6, ln_33, voY_6, ln_34, dot_6, det_6, tan_6, value_6, x_27, y_6, dotN_6, lenSq_6, dist_6;
                                const a_6 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                const b_6 = Line2D_$ctor_77D16AC0(0, 0, 0, 10);
                                Expect_isFalse((ln_30 = a_6, (other_6 = b_6, (vtX_6 = ((ln_31 = ln_30, ln_31.ToX - ln_31.FromX)), (vtY_6 = ((ln_32 = ln_30, ln_32.ToY - ln_32.FromY)), (voX_6 = ((ln_33 = other_6, ln_33.ToX - ln_33.FromX)), (voY_6 = ((ln_34 = other_6, ln_34.ToY - ln_34.FromY)), (!(((vtX_6 * vtX_6) + (vtY_6 * vtY_6)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_30, other_6) : undefined, (!(((voX_6 * voX_6) + (voY_6 * voY_6)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_6, ln_30) : undefined, (dot_6 = ((vtX_6 * voX_6) + (vtY_6 * voY_6)), (det_6 = ((vtX_6 * voY_6) - (vtY_6 * voX_6)), (tan_6 = ((value_6 = (det_6 / dot_6), Math.abs(value_6))), (tan_6 < 0.004363350820701567) && ((x_27 = (other_6.FromX - ln_30.FromX), (y_6 = (other_6.FromY - ln_30.FromY), (dotN_6 = ((x_27 * vtY_6) - (y_6 * vtX_6)), (lenSq_6 = ((vtX_6 * vtX_6) + (vtY_6 * vtY_6)), (dist_6 = ((dotN_6 * dotN_6) / lenSq_6), dist_6 < 1E-06))))))))))))))))))("perpendicular lines are not coincident");
                                Test_TestCaseBuilder__Zero(builder$0040_6);
                            })))), delay(() => {
                                let builder$0040_7;
                                return append(singleton((builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo nearly parallel within angle tolerance", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
                                    let ln_35, other_7, vtX_7, ln_36, vtY_7, ln_37, voX_7, ln_38, voY_7, ln_39, dot_7, det_7, tan_7, value_7, x_31, y_7, dotN_7, lenSq_7, dist_7;
                                    const a_7 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                    const b_7 = Line2D_$ctor_77D16AC0(0, 0, 10, 0.01);
                                    Expect_isTrue((ln_35 = a_7, (other_7 = b_7, (vtX_7 = ((ln_36 = ln_35, ln_36.ToX - ln_36.FromX)), (vtY_7 = ((ln_37 = ln_35, ln_37.ToY - ln_37.FromY)), (voX_7 = ((ln_38 = other_7, ln_38.ToX - ln_38.FromX)), (voY_7 = ((ln_39 = other_7, ln_39.ToY - ln_39.FromY)), (!(((vtX_7 * vtX_7) + (vtY_7 * vtY_7)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_35, other_7) : undefined, (!(((voX_7 * voX_7) + (voY_7 * voY_7)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_7, ln_35) : undefined, (dot_7 = ((vtX_7 * voX_7) + (vtY_7 * voY_7)), (det_7 = ((vtX_7 * voY_7) - (vtY_7 * voX_7)), (tan_7 = ((value_7 = (det_7 / dot_7), Math.abs(value_7))), (tan_7 < 0.004363350820701567) && ((x_31 = (other_7.FromX - ln_35.FromX), (y_7 = (other_7.FromY - ln_35.FromY), (dotN_7 = ((x_31 * vtY_7) - (y_7 * vtX_7)), (lenSq_7 = ((vtX_7 * vtX_7) + (vtY_7 * vtY_7)), (dist_7 = ((dotN_7 * dotN_7) / lenSq_7), dist_7 < 1E-06))))))))))))))))))("nearly parallel within tolerance are coincident");
                                    Test_TestCaseBuilder__Zero(builder$0040_7);
                                })))), delay(() => {
                                    let builder$0040_8;
                                    return append(singleton((builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo nearly parallel outside angle tolerance", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
                                        let ln_40, other_8, vtX_8, ln_41, vtY_8, ln_42, voX_8, ln_43, voY_8, ln_44, dot_8, det_8, tan_8, value_8, x_35, y_8, dotN_8, lenSq_8, dist_8;
                                        const a_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                        const b_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 1);
                                        Expect_isFalse((ln_40 = a_8, (other_8 = b_8, (vtX_8 = ((ln_41 = ln_40, ln_41.ToX - ln_41.FromX)), (vtY_8 = ((ln_42 = ln_40, ln_42.ToY - ln_42.FromY)), (voX_8 = ((ln_43 = other_8, ln_43.ToX - ln_43.FromX)), (voY_8 = ((ln_44 = other_8, ln_44.ToY - ln_44.FromY)), (!(((vtX_8 * vtX_8) + (vtY_8 * vtY_8)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_40, other_8) : undefined, (!(((voX_8 * voX_8) + (voY_8 * voY_8)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_8, ln_40) : undefined, (dot_8 = ((vtX_8 * voX_8) + (vtY_8 * voY_8)), (det_8 = ((vtX_8 * voY_8) - (vtY_8 * voX_8)), (tan_8 = ((value_8 = (det_8 / dot_8), Math.abs(value_8))), (tan_8 < 0.004363350820701567) && ((x_35 = (other_8.FromX - ln_40.FromX), (y_8 = (other_8.FromY - ln_40.FromY), (dotN_8 = ((x_35 * vtY_8) - (y_8 * vtX_8)), (lenSq_8 = ((vtX_8 * vtX_8) + (vtY_8 * vtY_8)), (dist_8 = ((dotN_8 * dotN_8) / lenSq_8), dist_8 < 1E-06))))))))))))))))))("nearly parallel outside tolerance are not coincident");
                                        Test_TestCaseBuilder__Zero(builder$0040_8);
                                    })))), delay(() => {
                                        let builder$0040_9;
                                        return append(singleton((builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo zero length first line throws", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
                                            const a_9 = Line2D_$ctor_77D16AC0(0, 0, 0, 0);
                                            const b_9 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                            Expect_throws(() => {
                                                let ln_45, other_9, vtX_9, ln_46, vtY_9, ln_47, voX_9, ln_48, voY_9, ln_49, dot_9, det_9, tan_9, value_9, x_39, y_9, dotN_9, lenSq_9, dist_9;
                                                (ln_45 = a_9, (other_9 = b_9, (vtX_9 = ((ln_46 = ln_45, ln_46.ToX - ln_46.FromX)), (vtY_9 = ((ln_47 = ln_45, ln_47.ToY - ln_47.FromY)), (voX_9 = ((ln_48 = other_9, ln_48.ToX - ln_48.FromX)), (voY_9 = ((ln_49 = other_9, ln_49.ToY - ln_49.FromY)), (!(((vtX_9 * vtX_9) + (vtY_9 * vtY_9)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_45, other_9) : undefined, (!(((voX_9 * voX_9) + (voY_9 * voY_9)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_9, ln_45) : undefined, (dot_9 = ((vtX_9 * voX_9) + (vtY_9 * voY_9)), (det_9 = ((vtX_9 * voY_9) - (vtY_9 * voX_9)), (tan_9 = ((value_9 = (det_9 / dot_9), Math.abs(value_9))), (tan_9 < 0.004363350820701567) && ((x_39 = (other_9.FromX - ln_45.FromX), (y_9 = (other_9.FromY - ln_45.FromY), (dotN_9 = ((x_39 * vtY_9) - (y_9 * vtX_9)), (lenSq_9 = ((vtX_9 * vtX_9) + (vtY_9 * vtY_9)), (dist_9 = ((dotN_9 * dotN_9) / lenSq_9), dist_9 < 1E-06)))))))))))))))));
                                            }, "zero length first line throws exception");
                                            Test_TestCaseBuilder__Zero(builder$0040_9);
                                        })))), delay(() => {
                                            let builder$0040_10;
                                            return append(singleton((builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo zero length second line throws", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
                                                const a_10 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                const b_10 = Line2D_$ctor_77D16AC0(5, 0, 5, 0);
                                                Expect_throws(() => {
                                                    let ln_50, other_10, vtX_10, ln_51, vtY_10, ln_52, voX_10, ln_53, voY_10, ln_54, dot_10, det_10, tan_10, value_11, x_43, y_10, dotN_10, lenSq_10, dist_10;
                                                    (ln_50 = a_10, (other_10 = b_10, (vtX_10 = ((ln_51 = ln_50, ln_51.ToX - ln_51.FromX)), (vtY_10 = ((ln_52 = ln_50, ln_52.ToY - ln_52.FromY)), (voX_10 = ((ln_53 = other_10, ln_53.ToX - ln_53.FromX)), (voY_10 = ((ln_54 = other_10, ln_54.ToY - ln_54.FromY)), (!(((vtX_10 * vtX_10) + (vtY_10 * vtY_10)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_50, other_10) : undefined, (!(((voX_10 * voX_10) + (voY_10 * voY_10)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_10, ln_50) : undefined, (dot_10 = ((vtX_10 * voX_10) + (vtY_10 * voY_10)), (det_10 = ((vtX_10 * voY_10) - (vtY_10 * voX_10)), (tan_10 = ((value_11 = (det_10 / dot_10), Math.abs(value_11))), (tan_10 < 0.004363350820701567) && ((x_43 = (other_10.FromX - ln_50.FromX), (y_10 = (other_10.FromY - ln_50.FromY), (dotN_10 = ((x_43 * vtY_10) - (y_10 * vtX_10)), (lenSq_10 = ((vtX_10 * vtX_10) + (vtY_10 * vtY_10)), (dist_10 = ((dotN_10 * dotN_10) / lenSq_10), dist_10 < 1E-06)))))))))))))))));
                                                }, "zero length second line throws exception");
                                                Test_TestCaseBuilder__Zero(builder$0040_10);
                                            })))), delay(() => {
                                                let builder$0040_11;
                                                return append(singleton((builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo both zero length throws", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
                                                    const a_11 = Line2D_$ctor_77D16AC0(0, 0, 0, 0);
                                                    const b_11 = Line2D_$ctor_77D16AC0(0, 0, 0, 0);
                                                    Expect_throws(() => {
                                                        let ln_55, other_11, vtX_11, ln_56, vtY_11, ln_57, voX_11, ln_58, voY_11, ln_59, dot_11, det_11, tan_11, value_13, x_47, y_11, dotN_11, lenSq_11, dist_11;
                                                        (ln_55 = a_11, (other_11 = b_11, (vtX_11 = ((ln_56 = ln_55, ln_56.ToX - ln_56.FromX)), (vtY_11 = ((ln_57 = ln_55, ln_57.ToY - ln_57.FromY)), (voX_11 = ((ln_58 = other_11, ln_58.ToX - ln_58.FromX)), (voY_11 = ((ln_59 = other_11, ln_59.ToY - ln_59.FromY)), (!(((vtX_11 * vtX_11) + (vtY_11 * vtY_11)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_55, other_11) : undefined, (!(((voX_11 * voX_11) + (voY_11 * voY_11)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_11, ln_55) : undefined, (dot_11 = ((vtX_11 * voX_11) + (vtY_11 * voY_11)), (det_11 = ((vtX_11 * voY_11) - (vtY_11 * voX_11)), (tan_11 = ((value_13 = (det_11 / dot_11), Math.abs(value_13))), (tan_11 < 0.004363350820701567) && ((x_47 = (other_11.FromX - ln_55.FromX), (y_11 = (other_11.FromY - ln_55.FromY), (dotN_11 = ((x_47 * vtY_11) - (y_11 * vtX_11)), (lenSq_11 = ((vtX_11 * vtX_11) + (vtY_11 * vtY_11)), (dist_11 = ((dotN_11 * dotN_11) / lenSq_11), dist_11 < 1E-06)))))))))))))))));
                                                    }, "both zero length throws exception");
                                                    Test_TestCaseBuilder__Zero(builder$0040_11);
                                                })))), delay(() => {
                                                    let builder$0040_12;
                                                    return append(singleton((builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo very short lines same direction", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
                                                        let ln_60, other_12, vtX_12, ln_61, vtY_12, ln_62, voX_12, ln_63, voY_12, ln_64, dot_12, det_12, tan_12, value_15, x_51, y_12, dotN_12, lenSq_12, dist_12;
                                                        const a_12 = Line2D_$ctor_77D16AC0(0, 0, 1E-10, 0);
                                                        const b_12 = Line2D_$ctor_77D16AC0(0, 0, 1E-11, 0);
                                                        Expect_isTrue((ln_60 = a_12, (other_12 = b_12, (vtX_12 = ((ln_61 = ln_60, ln_61.ToX - ln_61.FromX)), (vtY_12 = ((ln_62 = ln_60, ln_62.ToY - ln_62.FromY)), (voX_12 = ((ln_63 = other_12, ln_63.ToX - ln_63.FromX)), (voY_12 = ((ln_64 = other_12, ln_64.ToY - ln_64.FromY)), (!(((vtX_12 * vtX_12) + (vtY_12 * vtY_12)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_60, other_12) : undefined, (!(((voX_12 * voX_12) + (voY_12 * voY_12)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_12, ln_60) : undefined, (dot_12 = ((vtX_12 * voX_12) + (vtY_12 * voY_12)), (det_12 = ((vtX_12 * voY_12) - (vtY_12 * voX_12)), (tan_12 = ((value_15 = (det_12 / dot_12), Math.abs(value_15))), (tan_12 < 0.004363350820701567) && ((x_51 = (other_12.FromX - ln_60.FromX), (y_12 = (other_12.FromY - ln_60.FromY), (dotN_12 = ((x_51 * vtY_12) - (y_12 * vtX_12)), (lenSq_12 = ((vtX_12 * vtX_12) + (vtY_12 * vtY_12)), (dist_12 = ((dotN_12 * dotN_12) / lenSq_12), dist_12 < 1E-06))))))))))))))))))("very short lines on same ray are coincident");
                                                        Test_TestCaseBuilder__Zero(builder$0040_12);
                                                    })))), delay(() => {
                                                        let builder$0040_13;
                                                        return append(singleton((builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo opposite directions on same ray", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
                                                            let ln_65, other_13, vtX_13, ln_66, vtY_13, ln_67, voX_13, ln_68, voY_13, ln_69, dot_13, det_13, tan_13, value_16, x_55, y_13, dotN_13, lenSq_13, dist_13;
                                                            const a_13 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                            const b_13 = Line2D_$ctor_77D16AC0(10, 0, 0, 0);
                                                            Expect_isTrue((ln_65 = a_13, (other_13 = b_13, (vtX_13 = ((ln_66 = ln_65, ln_66.ToX - ln_66.FromX)), (vtY_13 = ((ln_67 = ln_65, ln_67.ToY - ln_67.FromY)), (voX_13 = ((ln_68 = other_13, ln_68.ToX - ln_68.FromX)), (voY_13 = ((ln_69 = other_13, ln_69.ToY - ln_69.FromY)), (!(((vtX_13 * vtX_13) + (vtY_13 * vtY_13)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_65, other_13) : undefined, (!(((voX_13 * voX_13) + (voY_13 * voY_13)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_13, ln_65) : undefined, (dot_13 = ((vtX_13 * voX_13) + (vtY_13 * voY_13)), (det_13 = ((vtX_13 * voY_13) - (vtY_13 * voX_13)), (tan_13 = ((value_16 = (det_13 / dot_13), Math.abs(value_16))), (tan_13 < 0.004363350820701567) && ((x_55 = (other_13.FromX - ln_65.FromX), (y_13 = (other_13.FromY - ln_65.FromY), (dotN_13 = ((x_55 * vtY_13) - (y_13 * vtX_13)), (lenSq_13 = ((vtX_13 * vtX_13) + (vtY_13 * vtY_13)), (dist_13 = ((dotN_13 * dotN_13) / lenSq_13), dist_13 < 1E-06))))))))))))))))))("opposite directions on same ray are still coincident");
                                                            Test_TestCaseBuilder__Zero(builder$0040_13);
                                                        })))), delay(() => {
                                                            let builder$0040_14;
                                                            return append(singleton((builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo diagonal lines same ray", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
                                                                let ln_70, other_14, vtX_14, ln_71, vtY_14, ln_72, voX_14, ln_73, voY_14, ln_74, dot_14, det_14, tan_14, value_17, x_59, y_14, dotN_14, lenSq_14, dist_14;
                                                                const a_14 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
                                                                const b_14 = Line2D_$ctor_77D16AC0(5, 5, 15, 15);
                                                                Expect_isTrue((ln_70 = a_14, (other_14 = b_14, (vtX_14 = ((ln_71 = ln_70, ln_71.ToX - ln_71.FromX)), (vtY_14 = ((ln_72 = ln_70, ln_72.ToY - ln_72.FromY)), (voX_14 = ((ln_73 = other_14, ln_73.ToX - ln_73.FromX)), (voY_14 = ((ln_74 = other_14, ln_74.ToY - ln_74.FromY)), (!(((vtX_14 * vtX_14) + (vtY_14 * vtY_14)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_70, other_14) : undefined, (!(((voX_14 * voX_14) + (voY_14 * voY_14)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_14, ln_70) : undefined, (dot_14 = ((vtX_14 * voX_14) + (vtY_14 * voY_14)), (det_14 = ((vtX_14 * voY_14) - (vtY_14 * voX_14)), (tan_14 = ((value_17 = (det_14 / dot_14), Math.abs(value_17))), (tan_14 < 0.004363350820701567) && ((x_59 = (other_14.FromX - ln_70.FromX), (y_14 = (other_14.FromY - ln_70.FromY), (dotN_14 = ((x_59 * vtY_14) - (y_14 * vtX_14)), (lenSq_14 = ((vtX_14 * vtX_14) + (vtY_14 * vtY_14)), (dist_14 = ((dotN_14 * dotN_14) / lenSq_14), dist_14 < 1E-06))))))))))))))))))("diagonal lines same ray are coincident");
                                                                Test_TestCaseBuilder__Zero(builder$0040_14);
                                                            })))), delay(() => {
                                                                let builder$0040_15;
                                                                return append(singleton((builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo diagonal lines parallel but offset", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
                                                                    let ln_75, other_15, vtX_15, ln_76, vtY_15, ln_77, voX_15, ln_78, voY_15, ln_79, dot_15, det_15, tan_15, value_18, x_63, y_15, dotN_15, lenSq_15, dist_15;
                                                                    const a_15 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
                                                                    const b_15 = Line2D_$ctor_77D16AC0(1, 0, 11, 10);
                                                                    Expect_isFalse((ln_75 = a_15, (other_15 = b_15, (vtX_15 = ((ln_76 = ln_75, ln_76.ToX - ln_76.FromX)), (vtY_15 = ((ln_77 = ln_75, ln_77.ToY - ln_77.FromY)), (voX_15 = ((ln_78 = other_15, ln_78.ToX - ln_78.FromX)), (voY_15 = ((ln_79 = other_15, ln_79.ToY - ln_79.FromY)), (!(((vtX_15 * vtX_15) + (vtY_15 * vtY_15)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_75, other_15) : undefined, (!(((voX_15 * voX_15) + (voY_15 * voY_15)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_15, ln_75) : undefined, (dot_15 = ((vtX_15 * voX_15) + (vtY_15 * voY_15)), (det_15 = ((vtX_15 * voY_15) - (vtY_15 * voX_15)), (tan_15 = ((value_18 = (det_15 / dot_15), Math.abs(value_18))), (tan_15 < 0.004363350820701567) && ((x_63 = (other_15.FromX - ln_75.FromX), (y_15 = (other_15.FromY - ln_75.FromY), (dotN_15 = ((x_63 * vtY_15) - (y_15 * vtX_15)), (lenSq_15 = ((vtX_15 * vtX_15) + (vtY_15 * vtY_15)), (dist_15 = ((dotN_15 * dotN_15) / lenSq_15), dist_15 < 1E-06))))))))))))))))))("diagonal lines parallel but offset are not coincident");
                                                                    Test_TestCaseBuilder__Zero(builder$0040_15);
                                                                })))), delay(() => {
                                                                    let builder$0040_16;
                                                                    return append(singleton((builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo vertical lines same ray", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
                                                                        let ln_80, other_16, vtX_16, ln_81, vtY_16, ln_82, voX_16, ln_83, voY_16, ln_84, dot_16, det_16, tan_16, value_19, x_67, y_16, dotN_16, lenSq_16, dist_16;
                                                                        const a_16 = Line2D_$ctor_77D16AC0(5, 0, 5, 10);
                                                                        const b_16 = Line2D_$ctor_77D16AC0(5, 3, 5, 13);
                                                                        Expect_isTrue((ln_80 = a_16, (other_16 = b_16, (vtX_16 = ((ln_81 = ln_80, ln_81.ToX - ln_81.FromX)), (vtY_16 = ((ln_82 = ln_80, ln_82.ToY - ln_82.FromY)), (voX_16 = ((ln_83 = other_16, ln_83.ToX - ln_83.FromX)), (voY_16 = ((ln_84 = other_16, ln_84.ToY - ln_84.FromY)), (!(((vtX_16 * vtX_16) + (vtY_16 * vtY_16)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_80, other_16) : undefined, (!(((voX_16 * voX_16) + (voY_16 * voY_16)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_16, ln_80) : undefined, (dot_16 = ((vtX_16 * voX_16) + (vtY_16 * voY_16)), (det_16 = ((vtX_16 * voY_16) - (vtY_16 * voX_16)), (tan_16 = ((value_19 = (det_16 / dot_16), Math.abs(value_19))), (tan_16 < 0.004363350820701567) && ((x_67 = (other_16.FromX - ln_80.FromX), (y_16 = (other_16.FromY - ln_80.FromY), (dotN_16 = ((x_67 * vtY_16) - (y_16 * vtX_16)), (lenSq_16 = ((vtX_16 * vtX_16) + (vtY_16 * vtY_16)), (dist_16 = ((dotN_16 * dotN_16) / lenSq_16), dist_16 < 1E-06))))))))))))))))))("vertical lines same ray are coincident");
                                                                        Test_TestCaseBuilder__Zero(builder$0040_16);
                                                                    })))), delay(() => {
                                                                        let builder$0040_17;
                                                                        return append(singleton((builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo vertical lines parallel but offset", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
                                                                            let ln_85, other_17, vtX_17, ln_86, vtY_17, ln_87, voX_17, ln_88, voY_17, ln_89, dot_17, det_17, tan_17, value_20, x_71, y_17, dotN_17, lenSq_17, dist_17;
                                                                            const a_17 = Line2D_$ctor_77D16AC0(5, 0, 5, 10);
                                                                            const b_17 = Line2D_$ctor_77D16AC0(7, 0, 7, 10);
                                                                            Expect_isFalse((ln_85 = a_17, (other_17 = b_17, (vtX_17 = ((ln_86 = ln_85, ln_86.ToX - ln_86.FromX)), (vtY_17 = ((ln_87 = ln_85, ln_87.ToY - ln_87.FromY)), (voX_17 = ((ln_88 = other_17, ln_88.ToX - ln_88.FromX)), (voY_17 = ((ln_89 = other_17, ln_89.ToY - ln_89.FromY)), (!(((vtX_17 * vtX_17) + (vtY_17 * vtY_17)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_85, other_17) : undefined, (!(((voX_17 * voX_17) + (voY_17 * voY_17)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_17, ln_85) : undefined, (dot_17 = ((vtX_17 * voX_17) + (vtY_17 * voY_17)), (det_17 = ((vtX_17 * voY_17) - (vtY_17 * voX_17)), (tan_17 = ((value_20 = (det_17 / dot_17), Math.abs(value_20))), (tan_17 < 0.004363350820701567) && ((x_71 = (other_17.FromX - ln_85.FromX), (y_17 = (other_17.FromY - ln_85.FromY), (dotN_17 = ((x_71 * vtY_17) - (y_17 * vtX_17)), (lenSq_17 = ((vtX_17 * vtX_17) + (vtY_17 * vtY_17)), (dist_17 = ((dotN_17 * dotN_17) / lenSq_17), dist_17 < 1E-06))))))))))))))))))("vertical lines parallel but offset are not coincident");
                                                                            Test_TestCaseBuilder__Zero(builder$0040_17);
                                                                        })))), delay(() => {
                                                                            let builder$0040_18;
                                                                            return append(singleton((builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo custom distance tolerance strict", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
                                                                                let ln_90, other_18, vtX_18, ln_91, vtY_18, ln_92, voX_18, ln_93, voY_18, ln_94, dot_18, det_18, tan_18, value_21, x_75, y_18, dotN_18, lenSq_18, dist_18, ln_95, other_19, vtX_19, ln_96, vtY_19, ln_97, voX_19, ln_98, voY_19, ln_99, dot_19, det_19, tan_19, value_22, x_79, y_19, dotN_19, lenSq_19, dist_19;
                                                                                const a_18 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                                const b_18 = Line2D_$ctor_77D16AC0(0, 0.0005, 10, 0.0005);
                                                                                Expect_isFalse((ln_90 = a_18, (other_18 = b_18, (vtX_18 = ((ln_91 = ln_90, ln_91.ToX - ln_91.FromX)), (vtY_18 = ((ln_92 = ln_90, ln_92.ToY - ln_92.FromY)), (voX_18 = ((ln_93 = other_18, ln_93.ToX - ln_93.FromX)), (voY_18 = ((ln_94 = other_18, ln_94.ToY - ln_94.FromY)), (!(((vtX_18 * vtX_18) + (vtY_18 * vtY_18)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_90, other_18) : undefined, (!(((voX_18 * voX_18) + (voY_18 * voY_18)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_18, ln_90) : undefined, (dot_18 = ((vtX_18 * voX_18) + (vtY_18 * voY_18)), (det_18 = ((vtX_18 * voY_18) - (vtY_18 * voX_18)), (tan_18 = ((value_21 = (det_18 / dot_18), Math.abs(value_21))), (tan_18 < 0.004363350820701567) && ((x_75 = (other_18.FromX - ln_90.FromX), (y_18 = (other_18.FromY - ln_90.FromY), (dotN_18 = ((x_75 * vtY_18) - (y_18 * vtX_18)), (lenSq_18 = ((vtX_18 * vtX_18) + (vtY_18 * vtY_18)), (dist_18 = ((dotN_18 * dotN_18) / lenSq_18), dist_18 < 1E-07))))))))))))))))))("with strict tolerance not coincident");
                                                                                Expect_isTrue((ln_95 = a_18, (other_19 = b_18, (vtX_19 = ((ln_96 = ln_95, ln_96.ToX - ln_96.FromX)), (vtY_19 = ((ln_97 = ln_95, ln_97.ToY - ln_97.FromY)), (voX_19 = ((ln_98 = other_19, ln_98.ToX - ln_98.FromX)), (voY_19 = ((ln_99 = other_19, ln_99.ToY - ln_99.FromY)), (!(((vtX_19 * vtX_19) + (vtY_19 * vtY_19)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_95, other_19) : undefined, (!(((voX_19 * voX_19) + (voY_19 * voY_19)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_19, ln_95) : undefined, (dot_19 = ((vtX_19 * voX_19) + (vtY_19 * voY_19)), (det_19 = ((vtX_19 * voY_19) - (vtY_19 * voX_19)), (tan_19 = ((value_22 = (det_19 / dot_19), Math.abs(value_22))), (tan_19 < 0.004363350820701567) && ((x_79 = (other_19.FromX - ln_95.FromX), (y_19 = (other_19.FromY - ln_95.FromY), (dotN_19 = ((x_79 * vtY_19) - (y_19 * vtX_19)), (lenSq_19 = ((vtX_19 * vtX_19) + (vtY_19 * vtY_19)), (dist_19 = ((dotN_19 * dotN_19) / lenSq_19), dist_19 < 1E-05))))))))))))))))))("with relaxed tolerance coincident");
                                                                                Test_TestCaseBuilder__Zero(builder$0040_18);
                                                                            })))), delay(() => {
                                                                                let builder$0040_19;
                                                                                return append(singleton((builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo custom angle tolerance strict", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
                                                                                    let ln_100, other_20, vtX_20, ln_101, vtY_20, ln_102, voX_20, ln_103, voY_20, ln_104, dot_20, det_20, tan_20, value_23, x_83, y_20, dotN_20, lenSq_20, dist_20, ln_105, other_21, vtX_21, ln_106, vtY_21, ln_107, voX_21, ln_108, voY_21, ln_109, dot_21, det_21, tan_21, value_24, x_87, y_21, dotN_21, lenSq_21, dist_21;
                                                                                    const a_19 = Line2D_$ctor_77D16AC0(0, 0, 100, 0);
                                                                                    const b_19 = Line2D_$ctor_77D16AC0(0, 0, 100, 0.1);
                                                                                    Expect_isTrue((ln_100 = a_19, (other_20 = b_19, (vtX_20 = ((ln_101 = ln_100, ln_101.ToX - ln_101.FromX)), (vtY_20 = ((ln_102 = ln_100, ln_102.ToY - ln_102.FromY)), (voX_20 = ((ln_103 = other_20, ln_103.ToX - ln_103.FromX)), (voY_20 = ((ln_104 = other_20, ln_104.ToY - ln_104.FromY)), (!(((vtX_20 * vtX_20) + (vtY_20 * vtY_20)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_100, other_20) : undefined, (!(((voX_20 * voX_20) + (voY_20 * voY_20)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_20, ln_100) : undefined, (dot_20 = ((vtX_20 * voX_20) + (vtY_20 * voY_20)), (det_20 = ((vtX_20 * voY_20) - (vtY_20 * voX_20)), (tan_20 = ((value_23 = (det_20 / dot_20), Math.abs(value_23))), (tan_20 < 0.004363350820701567) && ((x_83 = (other_20.FromX - ln_100.FromX), (y_20 = (other_20.FromY - ln_100.FromY), (dotN_20 = ((x_83 * vtY_20) - (y_20 * vtX_20)), (lenSq_20 = ((vtX_20 * vtX_20) + (vtY_20 * vtY_20)), (dist_20 = ((dotN_20 * dotN_20) / lenSq_20), dist_20 < 1E-06))))))))))))))))))("with default angle tolerance (0.25 deg) coincident");
                                                                                    Expect_isFalse((ln_105 = a_19, (other_21 = b_19, (vtX_21 = ((ln_106 = ln_105, ln_106.ToX - ln_106.FromX)), (vtY_21 = ((ln_107 = ln_105, ln_107.ToY - ln_107.FromY)), (voX_21 = ((ln_108 = other_21, ln_108.ToX - ln_108.FromX)), (voY_21 = ((ln_109 = other_21, ln_109.ToY - ln_109.FromY)), (!(((vtX_21 * vtX_21) + (vtY_21 * vtY_21)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_105, other_21) : undefined, (!(((voX_21 * voX_21) + (voY_21 * voY_21)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_21, ln_105) : undefined, (dot_21 = ((vtX_21 * voX_21) + (vtY_21 * voY_21)), (det_21 = ((vtX_21 * voY_21) - (vtY_21 * voX_21)), (tan_21 = ((value_24 = (det_21 / dot_21), Math.abs(value_24))), (tan_21 < 0.0001745329269716253) && ((x_87 = (other_21.FromX - ln_105.FromX), (y_21 = (other_21.FromY - ln_105.FromY), (dotN_21 = ((x_87 * vtY_21) - (y_21 * vtX_21)), (lenSq_21 = ((vtX_21 * vtX_21) + (vtY_21 * vtY_21)), (dist_21 = ((dotN_21 * dotN_21) / lenSq_21), dist_21 < 1E-06))))))))))))))))))("with strict angle tolerance not coincident");
                                                                                    Test_TestCaseBuilder__Zero(builder$0040_19);
                                                                                })))), delay(() => {
                                                                                    let builder$0040_20;
                                                                                    return append(singleton((builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo negative coordinates", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
                                                                                        let ln_110, other_22, vtX_22, ln_111, vtY_22, ln_112, voX_22, ln_113, voY_22, ln_114, dot_22, det_22, tan_22, value_25, x_91, y_22, dotN_22, lenSq_22, dist_22;
                                                                                        const a_20 = Line2D_$ctor_77D16AC0(-10, -5, -20, -15);
                                                                                        const b_20 = Line2D_$ctor_77D16AC0(-15, -10, -25, -20);
                                                                                        Expect_isTrue((ln_110 = a_20, (other_22 = b_20, (vtX_22 = ((ln_111 = ln_110, ln_111.ToX - ln_111.FromX)), (vtY_22 = ((ln_112 = ln_110, ln_112.ToY - ln_112.FromY)), (voX_22 = ((ln_113 = other_22, ln_113.ToX - ln_113.FromX)), (voY_22 = ((ln_114 = other_22, ln_114.ToY - ln_114.FromY)), (!(((vtX_22 * vtX_22) + (vtY_22 * vtY_22)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_110, other_22) : undefined, (!(((voX_22 * voX_22) + (voY_22 * voY_22)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_22, ln_110) : undefined, (dot_22 = ((vtX_22 * voX_22) + (vtY_22 * voY_22)), (det_22 = ((vtX_22 * voY_22) - (vtY_22 * voX_22)), (tan_22 = ((value_25 = (det_22 / dot_22), Math.abs(value_25))), (tan_22 < 0.004363350820701567) && ((x_91 = (other_22.FromX - ln_110.FromX), (y_22 = (other_22.FromY - ln_110.FromY), (dotN_22 = ((x_91 * vtY_22) - (y_22 * vtX_22)), (lenSq_22 = ((vtX_22 * vtX_22) + (vtY_22 * vtY_22)), (dist_22 = ((dotN_22 * dotN_22) / lenSq_22), dist_22 < 1E-06))))))))))))))))))("negative coordinates same ray are coincident");
                                                                                        Test_TestCaseBuilder__Zero(builder$0040_20);
                                                                                    })))), delay(() => {
                                                                                        let builder$0040_21;
                                                                                        return append(singleton((builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo mixed positive negative", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
                                                                                            let ln_115, other_23, vtX_23, ln_116, vtY_23, ln_117, voX_23, ln_118, voY_23, ln_119, dot_23, det_23, tan_23, value_26, x_95, y_23, dotN_23, lenSq_23, dist_23;
                                                                                            const a_21 = Line2D_$ctor_77D16AC0(-5, -5, 5, 5);
                                                                                            const b_21 = Line2D_$ctor_77D16AC0(-10, -10, 10, 10);
                                                                                            Expect_isTrue((ln_115 = a_21, (other_23 = b_21, (vtX_23 = ((ln_116 = ln_115, ln_116.ToX - ln_116.FromX)), (vtY_23 = ((ln_117 = ln_115, ln_117.ToY - ln_117.FromY)), (voX_23 = ((ln_118 = other_23, ln_118.ToX - ln_118.FromX)), (voY_23 = ((ln_119 = other_23, ln_119.ToY - ln_119.FromY)), (!(((vtX_23 * vtX_23) + (vtY_23 * vtY_23)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_115, other_23) : undefined, (!(((voX_23 * voX_23) + (voY_23 * voY_23)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_23, ln_115) : undefined, (dot_23 = ((vtX_23 * voX_23) + (vtY_23 * voY_23)), (det_23 = ((vtX_23 * voY_23) - (vtY_23 * voX_23)), (tan_23 = ((value_26 = (det_23 / dot_23), Math.abs(value_26))), (tan_23 < 0.004363350820701567) && ((x_95 = (other_23.FromX - ln_115.FromX), (y_23 = (other_23.FromY - ln_115.FromY), (dotN_23 = ((x_95 * vtY_23) - (y_23 * vtX_23)), (lenSq_23 = ((vtX_23 * vtX_23) + (vtY_23 * vtY_23)), (dist_23 = ((dotN_23 * dotN_23) / lenSq_23), dist_23 < 1E-06))))))))))))))))))("mixed sign coordinates same ray are coincident");
                                                                                            Test_TestCaseBuilder__Zero(builder$0040_21);
                                                                                        })))), delay(() => {
                                                                                            let builder$0040_22;
                                                                                            return append(singleton((builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo almost parallel lines small angle", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
                                                                                                let ln_120, other_24, vtX_24, ln_121, vtY_24, ln_122, voX_24, ln_123, voY_24, ln_124, dot_24, det_24, tan_24, value_27, x_99, y_24, dotN_24, lenSq_24, dist_24;
                                                                                                const a_22 = Line2D_$ctor_77D16AC0(0, 0, 1000, 0);
                                                                                                const b_22 = Line2D_$ctor_77D16AC0(0, 0, 1000, 1);
                                                                                                Expect_isTrue((ln_120 = a_22, (other_24 = b_22, (vtX_24 = ((ln_121 = ln_120, ln_121.ToX - ln_121.FromX)), (vtY_24 = ((ln_122 = ln_120, ln_122.ToY - ln_122.FromY)), (voX_24 = ((ln_123 = other_24, ln_123.ToX - ln_123.FromX)), (voY_24 = ((ln_124 = other_24, ln_124.ToY - ln_124.FromY)), (!(((vtX_24 * vtX_24) + (vtY_24 * vtY_24)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_120, other_24) : undefined, (!(((voX_24 * voX_24) + (voY_24 * voY_24)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_24, ln_120) : undefined, (dot_24 = ((vtX_24 * voX_24) + (vtY_24 * voY_24)), (det_24 = ((vtX_24 * voY_24) - (vtY_24 * voX_24)), (tan_24 = ((value_27 = (det_24 / dot_24), Math.abs(value_27))), (tan_24 < 0.004363350820701567) && ((x_99 = (other_24.FromX - ln_120.FromX), (y_24 = (other_24.FromY - ln_120.FromY), (dotN_24 = ((x_99 * vtY_24) - (y_24 * vtX_24)), (lenSq_24 = ((vtX_24 * vtX_24) + (vtY_24 * vtY_24)), (dist_24 = ((dotN_24 * dotN_24) / lenSq_24), dist_24 < 1E-06))))))))))))))))))("lines at 0.057 degrees are coincident with default tolerance");
                                                                                                Test_TestCaseBuilder__Zero(builder$0040_22);
                                                                                            })))), delay(() => append(collect((i) => {
                                                                                                let ln_126, ln_127, ln_129, ln_130, builder$0040_23;
                                                                                                const scale = rFloat(0.1, 100);
                                                                                                const angle = rFloat(-180, 180);
                                                                                                let vec;
                                                                                                let v_2;
                                                                                                const v_1 = Vc_$ctor_7B00E9A0(1, 0);
                                                                                                let r;
                                                                                                const rad = 0.017453292519943295 * angle;
                                                                                                r = Rotation2D_$ctor_7B00E9A0(Math.sin(rad), Math.cos(rad));
                                                                                                v_2 = Vc_$ctor_7B00E9A0((r.Cos * v_1.X) - (r.Sin * v_1.Y), (r.Sin * v_1.X) + (r.Cos * v_1.Y));
                                                                                                const f_3 = scale;
                                                                                                vec = Vc_$ctor_7B00E9A0_1(v_2.X * f_3, v_2.Y * f_3);
                                                                                                let a_23;
                                                                                                const p = Pt_$ctor_7B00E9A0(0, 0);
                                                                                                const v_3 = vec;
                                                                                                a_23 = Line2D_$ctor_77D16AC0_1(p.X, p.Y, p.X + v_3.X, p.Y + v_3.Y);
                                                                                                let b_23;
                                                                                                let p_2;
                                                                                                const ln_125 = a_23;
                                                                                                const p_1 = rFloat(-10, 10);
                                                                                                p_2 = Pt_$ctor_7B00E9A0_1(ln_125.FromX + (((ln_126 = ln_125, ln_126.ToX - ln_126.FromX)) * p_1), ln_125.FromY + (((ln_127 = ln_125, ln_127.ToY - ln_127.FromY)) * p_1));
                                                                                                let v_5;
                                                                                                let v_4;
                                                                                                const ln_128 = a_23;
                                                                                                v_4 = Vc_$ctor_7B00E9A0_1((ln_129 = ln_128, ln_129.ToX - ln_129.FromX), (ln_130 = ln_128, ln_130.ToY - ln_130.FromY));
                                                                                                const f_4 = rFloat(0.1, 2);
                                                                                                v_5 = Vc_$ctor_7B00E9A0_1(v_4.X * f_4, v_4.Y * f_4);
                                                                                                b_23 = Line2D_$ctor_77D16AC0_1(p_2.X, p_2.Y, p_2.X + v_5.X, p_2.Y + v_5.Y);
                                                                                                return singleton((builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F(toText(printf("IsCoincidentTo random same ray %d"))(i), new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
                                                                                                    let ln_131, other_25, vtX_25, ln_132, vtY_25, ln_133, voX_25, ln_134, voY_25, ln_135, dot_25, det_25, tan_25, value_28, x_103, y_25, dotN_25, lenSq_25, dist_25;
                                                                                                    Expect_isTrue((ln_131 = a_23, (other_25 = b_23, (vtX_25 = ((ln_132 = ln_131, ln_132.ToX - ln_132.FromX)), (vtY_25 = ((ln_133 = ln_131, ln_133.ToY - ln_133.FromY)), (voX_25 = ((ln_134 = other_25, ln_134.ToX - ln_134.FromX)), (voY_25 = ((ln_135 = other_25, ln_135.ToY - ln_135.FromY)), (!(((vtX_25 * vtX_25) + (vtY_25 * vtY_25)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_131, other_25) : undefined, (!(((voX_25 * voX_25) + (voY_25 * voY_25)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_25, ln_131) : undefined, (dot_25 = ((vtX_25 * voX_25) + (vtY_25 * voY_25)), (det_25 = ((vtX_25 * voY_25) - (vtY_25 * voX_25)), (tan_25 = ((value_28 = (det_25 / dot_25), Math.abs(value_28))), (tan_25 < 0.004363350820701567) && ((x_103 = (other_25.FromX - ln_131.FromX), (y_25 = (other_25.FromY - ln_131.FromY), (dotN_25 = ((x_103 * vtY_25) - (y_25 * vtX_25)), (lenSq_25 = ((vtX_25 * vtX_25) + (vtY_25 * vtY_25)), (dist_25 = ((dotN_25 * dotN_25) / lenSq_25), dist_25 < 1E-06))))))))))))))))))("random lines on same ray are coincident");
                                                                                                    Test_TestCaseBuilder__Zero(builder$0040_23);
                                                                                                }))));
                                                                                            }, rangeDouble(1, 1, 10)), delay(() => append(collect((i_1) => {
                                                                                                let ln_137, ln_138, ln_140, ln_141, builder$0040_24;
                                                                                                const scale_1 = rFloat(0.1, 100);
                                                                                                const angle_1 = rFloat(-180, 180);
                                                                                                const dist_26 = rFloat(0.5, 5);
                                                                                                let vec_1;
                                                                                                let v_8;
                                                                                                const v_7 = Vc_$ctor_7B00E9A0(1, 0);
                                                                                                let r_1;
                                                                                                const rad_1 = 0.017453292519943295 * angle_1;
                                                                                                r_1 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_1), Math.cos(rad_1));
                                                                                                v_8 = Vc_$ctor_7B00E9A0((r_1.Cos * v_7.X) - (r_1.Sin * v_7.Y), (r_1.Sin * v_7.X) + (r_1.Cos * v_7.Y));
                                                                                                const f_5 = scale_1;
                                                                                                vec_1 = Vc_$ctor_7B00E9A0_1(v_8.X * f_5, v_8.Y * f_5);
                                                                                                let a_24;
                                                                                                const p_3 = Pt_$ctor_7B00E9A0(0, 0);
                                                                                                const v_9 = vec_1;
                                                                                                a_24 = Line2D_$ctor_77D16AC0_1(p_3.X, p_3.Y, p_3.X + v_9.X, p_3.Y + v_9.Y);
                                                                                                let normal;
                                                                                                let v_11;
                                                                                                let v_10;
                                                                                                const ln_136 = a_24;
                                                                                                v_10 = Vc_$ctor_7B00E9A0_1((ln_137 = ln_136, ln_137.ToX - ln_137.FromX), (ln_138 = ln_136, ln_138.ToY - ln_138.FromY));
                                                                                                v_11 = Vc_$ctor_7B00E9A0(-v_10.Y, v_10.X);
                                                                                                const x_104 = v_11.X;
                                                                                                const y_26 = v_11.Y;
                                                                                                const l = Math.sqrt((x_104 * x_104) + (y_26 * y_26));
                                                                                                if (!(l > 1E-12)) {
                                                                                                    failTooSmall("Vc.Unitized", v_11);
                                                                                                }
                                                                                                normal = UnitVc_$ctor_7B00E9A0(x_104 / l, y_26 / l);
                                                                                                let offset;
                                                                                                const p_4 = Pt_$ctor_7B00E9A0(0, 0);
                                                                                                let v_12;
                                                                                                const a_25 = normal;
                                                                                                const f_6 = dist_26;
                                                                                                v_12 = Vc_$ctor_7B00E9A0_1(a_25.X * f_6, a_25.Y * f_6);
                                                                                                offset = Pt_$ctor_7B00E9A0(p_4.X + v_12.X, p_4.Y + v_12.Y);
                                                                                                let b_24;
                                                                                                const p_5 = offset;
                                                                                                let v_14;
                                                                                                let v_13;
                                                                                                const ln_139 = a_24;
                                                                                                v_13 = Vc_$ctor_7B00E9A0_1((ln_140 = ln_139, ln_140.ToX - ln_140.FromX), (ln_141 = ln_139, ln_141.ToY - ln_141.FromY));
                                                                                                const f_7 = rFloat(0.1, 2);
                                                                                                v_14 = Vc_$ctor_7B00E9A0_1(v_13.X * f_7, v_13.Y * f_7);
                                                                                                b_24 = Line2D_$ctor_77D16AC0_1(p_5.X, p_5.Y, p_5.X + v_14.X, p_5.Y + v_14.Y);
                                                                                                return singleton((builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F(toText(printf("IsCoincidentTo random parallel offset %d"))(i_1), new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
                                                                                                    let ln_142, other_26, vtX_26, ln_143, vtY_26, ln_144, voX_26, ln_145, voY_26, ln_146, dot_26, det_26, tan_26, value_29, x_110, y_28, dotN_26, lenSq_26, dist_27;
                                                                                                    Expect_isFalse((ln_142 = a_24, (other_26 = b_24, (vtX_26 = ((ln_143 = ln_142, ln_143.ToX - ln_143.FromX)), (vtY_26 = ((ln_144 = ln_142, ln_144.ToY - ln_144.FromY)), (voX_26 = ((ln_145 = other_26, ln_145.ToX - ln_145.FromX)), (voY_26 = ((ln_146 = other_26, ln_146.ToY - ln_146.FromY)), (!(((vtX_26 * vtX_26) + (vtY_26 * vtY_26)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_142, other_26) : undefined, (!(((voX_26 * voX_26) + (voY_26 * voY_26)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_26, ln_142) : undefined, (dot_26 = ((vtX_26 * voX_26) + (vtY_26 * voY_26)), (det_26 = ((vtX_26 * voY_26) - (vtY_26 * voX_26)), (tan_26 = ((value_29 = (det_26 / dot_26), Math.abs(value_29))), (tan_26 < 0.004363350820701567) && ((x_110 = (other_26.FromX - ln_142.FromX), (y_28 = (other_26.FromY - ln_142.FromY), (dotN_26 = ((x_110 * vtY_26) - (y_28 * vtX_26)), (lenSq_26 = ((vtX_26 * vtX_26) + (vtY_26 * vtY_26)), (dist_27 = ((dotN_26 * dotN_26) / lenSq_26), dist_27 < 1E-06))))))))))))))))))("random parallel lines with offset are not coincident");
                                                                                                    Test_TestCaseBuilder__Zero(builder$0040_24);
                                                                                                }))));
                                                                                            }, rangeDouble(1, 1, 10)), delay(() => collect((i_2) => {
                                                                                                let builder$0040_25;
                                                                                                const scale_2 = rFloat(0.1, 100);
                                                                                                const angle1 = rFloat(-180, 180);
                                                                                                const angle2 = angle1 + rFloat(5, 175);
                                                                                                let vec1;
                                                                                                let v_17;
                                                                                                const v_16 = Vc_$ctor_7B00E9A0(1, 0);
                                                                                                let r_2;
                                                                                                const rad_2 = 0.017453292519943295 * angle1;
                                                                                                r_2 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_2), Math.cos(rad_2));
                                                                                                v_17 = Vc_$ctor_7B00E9A0((r_2.Cos * v_16.X) - (r_2.Sin * v_16.Y), (r_2.Sin * v_16.X) + (r_2.Cos * v_16.Y));
                                                                                                const f_8 = scale_2;
                                                                                                vec1 = Vc_$ctor_7B00E9A0_1(v_17.X * f_8, v_17.Y * f_8);
                                                                                                let vec2;
                                                                                                let v_20;
                                                                                                const v_19 = Vc_$ctor_7B00E9A0(1, 0);
                                                                                                let r_3;
                                                                                                const rad_3 = 0.017453292519943295 * angle2;
                                                                                                r_3 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_3), Math.cos(rad_3));
                                                                                                v_20 = Vc_$ctor_7B00E9A0((r_3.Cos * v_19.X) - (r_3.Sin * v_19.Y), (r_3.Sin * v_19.X) + (r_3.Cos * v_19.Y));
                                                                                                const f_9 = scale_2;
                                                                                                vec2 = Vc_$ctor_7B00E9A0_1(v_20.X * f_9, v_20.Y * f_9);
                                                                                                let a_26;
                                                                                                const p_6 = Pt_$ctor_7B00E9A0(0, 0);
                                                                                                const v_21 = vec1;
                                                                                                a_26 = Line2D_$ctor_77D16AC0_1(p_6.X, p_6.Y, p_6.X + v_21.X, p_6.Y + v_21.Y);
                                                                                                let b_25;
                                                                                                const p_7 = Pt_$ctor_7B00E9A0(0, 0);
                                                                                                const v_22 = vec2;
                                                                                                b_25 = Line2D_$ctor_77D16AC0_1(p_7.X, p_7.Y, p_7.X + v_22.X, p_7.Y + v_22.Y);
                                                                                                return singleton((builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F(toText(printf("IsCoincidentTo random non-parallel %d"))(i_2), new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
                                                                                                    let ln_147, other_27, vtX_27, ln_148, vtY_27, ln_149, voX_27, ln_150, voY_27, ln_151, dot_27, det_27, tan_27, value_30, x_114, y_29, dotN_27, lenSq_27, dist_28;
                                                                                                    Expect_isFalse((ln_147 = a_26, (other_27 = b_25, (vtX_27 = ((ln_148 = ln_147, ln_148.ToX - ln_148.FromX)), (vtY_27 = ((ln_149 = ln_147, ln_149.ToY - ln_149.FromY)), (voX_27 = ((ln_150 = other_27, ln_150.ToX - ln_150.FromX)), (voY_27 = ((ln_151 = other_27, ln_151.ToY - ln_151.FromY)), (!(((vtX_27 * vtX_27) + (vtY_27 * vtY_27)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo this", ln_147, other_27) : undefined, (!(((voX_27 * voX_27) + (voY_27 * voY_27)) > 1E-24) ? failTooSmall2("Line2D.IsCoincidentTo other", other_27, ln_147) : undefined, (dot_27 = ((vtX_27 * voX_27) + (vtY_27 * voY_27)), (det_27 = ((vtX_27 * voY_27) - (vtY_27 * voX_27)), (tan_27 = ((value_30 = (det_27 / dot_27), Math.abs(value_30))), (tan_27 < 0.004363350820701567) && ((x_114 = (other_27.FromX - ln_147.FromX), (y_29 = (other_27.FromY - ln_147.FromY), (dotN_27 = ((x_114 * vtY_27) - (y_29 * vtX_27)), (lenSq_27 = ((vtX_27 * vtX_27) + (vtY_27 * vtY_27)), (dist_28 = ((dotN_27 * dotN_27) / lenSq_27), dist_28 < 1E-06))))))))))))))))))("random non-parallel lines are not coincident");
                                                                                                    Test_TestCaseBuilder__Zero(builder$0040_25);
                                                                                                }))));
                                                                                            }, rangeDouble(1, 1, 10))))))));
                                                                                        }));
                                                                                    }));
                                                                                }));
                                                                            }));
                                                                        }));
                                                                    }));
                                                                }));
                                                            }));
                                                        }));
                                                    }));
                                                }));
                                            }));
                                        }));
                                    }));
                                }));
                            }));
                        }));
                    }));
                }));
            }));
        }));
    }));
})));

export const testsFastMethods = Test_testList("Line2D Fast Methods", toList(delay(() => {
    let builder$0040;
    return append(singleton((builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast identical lines", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let ln, other, d1x, ln_1, d1y, ln_2, d2x, ln_3, d2y, ln_4, cross;
        const a = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_isTrue((ln = a, (other = b, (d1x = ((ln_1 = ln, ln_1.ToX - ln_1.FromX)), (d1y = ((ln_2 = ln, ln_2.ToY - ln_2.FromY)), (d2x = ((ln_3 = other, ln_3.ToX - ln_3.FromX)), (d2y = ((ln_4 = other, ln_4.ToY - ln_4.FromY)), (cross = ((d1x * d2y) - (d1y * d2x)), Math.abs(cross) < 1E-06))))))))("identical lines are parallel");
        Test_TestCaseBuilder__Zero(builder$0040);
    })))), delay(() => {
        let builder$0040_1;
        return append(singleton((builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast parallel same direction", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
            let ln_5, other_1, d1x_1, ln_6, d1y_1, ln_7, d2x_1, ln_8, d2y_1, ln_9, cross_1;
            const a_1 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
            const b_1 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
            Expect_isTrue((ln_5 = a_1, (other_1 = b_1, (d1x_1 = ((ln_6 = ln_5, ln_6.ToX - ln_6.FromX)), (d1y_1 = ((ln_7 = ln_5, ln_7.ToY - ln_7.FromY)), (d2x_1 = ((ln_8 = other_1, ln_8.ToX - ln_8.FromX)), (d2y_1 = ((ln_9 = other_1, ln_9.ToY - ln_9.FromY)), (cross_1 = ((d1x_1 * d2y_1) - (d1y_1 * d2x_1)), Math.abs(cross_1) < 1E-06))))))))("parallel lines same direction");
            Test_TestCaseBuilder__Zero(builder$0040_1);
        })))), delay(() => {
            let builder$0040_2;
            return append(singleton((builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast parallel opposite direction", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
                let ln_10, other_2, d1x_2, ln_11, d1y_2, ln_12, d2x_2, ln_13, d2y_2, ln_14, cross_2;
                const a_2 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                const b_2 = Line2D_$ctor_77D16AC0(10, 5, 0, 5);
                Expect_isTrue((ln_10 = a_2, (other_2 = b_2, (d1x_2 = ((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)), (d1y_2 = ((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)), (d2x_2 = ((ln_13 = other_2, ln_13.ToX - ln_13.FromX)), (d2y_2 = ((ln_14 = other_2, ln_14.ToY - ln_14.FromY)), (cross_2 = ((d1x_2 * d2y_2) - (d1y_2 * d2x_2)), Math.abs(cross_2) < 1E-06))))))))("parallel lines opposite direction");
                Test_TestCaseBuilder__Zero(builder$0040_2);
            })))), delay(() => {
                let builder$0040_3;
                return append(singleton((builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast perpendicular lines", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
                    let ln_15, other_3, d1x_3, ln_16, d1y_3, ln_17, d2x_3, ln_18, d2y_3, ln_19, cross_3;
                    const a_3 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                    const b_3 = Line2D_$ctor_77D16AC0(0, 0, 0, 10);
                    Expect_isFalse((ln_15 = a_3, (other_3 = b_3, (d1x_3 = ((ln_16 = ln_15, ln_16.ToX - ln_16.FromX)), (d1y_3 = ((ln_17 = ln_15, ln_17.ToY - ln_17.FromY)), (d2x_3 = ((ln_18 = other_3, ln_18.ToX - ln_18.FromX)), (d2y_3 = ((ln_19 = other_3, ln_19.ToY - ln_19.FromY)), (cross_3 = ((d1x_3 * d2y_3) - (d1y_3 * d2x_3)), Math.abs(cross_3) < 1E-06))))))))("perpendicular lines are not parallel");
                    Test_TestCaseBuilder__Zero(builder$0040_3);
                })))), delay(() => {
                    let builder$0040_4;
                    return append(singleton((builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast zero length lines returns true", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
                        let ln_20, other_4, d1x_4, ln_21, d1y_4, ln_22, d2x_4, ln_23, d2y_4, ln_24, cross_4;
                        const a_4 = Line2D_$ctor_77D16AC0(0, 0, 0, 0);
                        const b_4 = Line2D_$ctor_77D16AC0(5, 5, 5, 5);
                        Expect_isTrue((ln_20 = a_4, (other_4 = b_4, (d1x_4 = ((ln_21 = ln_20, ln_21.ToX - ln_21.FromX)), (d1y_4 = ((ln_22 = ln_20, ln_22.ToY - ln_22.FromY)), (d2x_4 = ((ln_23 = other_4, ln_23.ToX - ln_23.FromX)), (d2y_4 = ((ln_24 = other_4, ln_24.ToY - ln_24.FromY)), (cross_4 = ((d1x_4 * d2y_4) - (d1y_4 * d2x_4)), Math.abs(cross_4) < 1E-06))))))))("zero length lines return true (documented behavior)");
                        Test_TestCaseBuilder__Zero(builder$0040_4);
                    })))), delay(() => {
                        let builder$0040_5;
                        return append(singleton((builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast very long almost parallel lines", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
                            let ln_25, other_5, d1x_5, ln_26, d1y_5, ln_27, d2x_5, ln_28, d2y_5, ln_29, cross_5, ln_30, other_6, d1x_6, ln_31, d1y_6, ln_32, d2x_6, ln_33, d2y_6, ln_34, cross_6;
                            const a_5 = Line2D_$ctor_77D16AC0(0, 0, 1000, 0);
                            const b_5 = Line2D_$ctor_77D16AC0(0, 0, 1000, 1);
                            Expect_isFalse((ln_25 = a_5, (other_5 = b_5, (d1x_5 = ((ln_26 = ln_25, ln_26.ToX - ln_26.FromX)), (d1y_5 = ((ln_27 = ln_25, ln_27.ToY - ln_27.FromY)), (d2x_5 = ((ln_28 = other_5, ln_28.ToX - ln_28.FromX)), (d2y_5 = ((ln_29 = other_5, ln_29.ToY - ln_29.FromY)), (cross_5 = ((d1x_5 * d2y_5) - (d1y_5 * d2x_5)), Math.abs(cross_5) < 1E-06))))))))("long almost parallel lines with default tolerance");
                            Expect_isTrue((ln_30 = a_5, (other_6 = b_5, (d1x_6 = ((ln_31 = ln_30, ln_31.ToX - ln_31.FromX)), (d1y_6 = ((ln_32 = ln_30, ln_32.ToY - ln_32.FromY)), (d2x_6 = ((ln_33 = other_6, ln_33.ToX - ln_33.FromX)), (d2y_6 = ((ln_34 = other_6, ln_34.ToY - ln_34.FromY)), (cross_6 = ((d1x_6 * d2y_6) - (d1y_6 * d2x_6)), Math.abs(cross_6) < 1001))))))))("long almost parallel lines with larger tolerance");
                            Test_TestCaseBuilder__Zero(builder$0040_5);
                        })))), delay(() => {
                            let builder$0040_6;
                            return append(singleton((builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast identical lines", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
                                let ln_35, other_7, d1x_7, ln_36, d1y_7, ln_37, d2x_7, ln_38, d2y_7, ln_39, cross_7, px, py, cross_1_1;
                                const a_6 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                const b_6 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                Expect_isTrue((ln_35 = a_6, (other_7 = b_6, (d1x_7 = ((ln_36 = ln_35, ln_36.ToX - ln_36.FromX)), (d1y_7 = ((ln_37 = ln_35, ln_37.ToY - ln_37.FromY)), (d2x_7 = ((ln_38 = other_7, ln_38.ToX - ln_38.FromX)), (d2y_7 = ((ln_39 = other_7, ln_39.ToY - ln_39.FromY)), (cross_7 = ((d1x_7 * d2y_7) - (d1y_7 * d2x_7)), (Math.abs(cross_7) < 1E-06) && ((px = (ln_35.FromX - other_7.FromX), (py = (ln_35.FromY - other_7.FromY), (cross_1_1 = ((px * d2y_7) - (py * d2x_7)), Math.abs(cross_1_1) < 1E-06))))))))))))("identical lines are coincident");
                                Test_TestCaseBuilder__Zero(builder$0040_6);
                            })))), delay(() => {
                                let builder$0040_7;
                                return append(singleton((builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast same ray offset start", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
                                    let ln_40, other_8, d1x_8, ln_41, d1y_8, ln_42, d2x_8, ln_43, d2y_8, ln_44, cross_8, px_1, py_1, cross_1_2;
                                    const a_7 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                    const b_7 = Line2D_$ctor_77D16AC0(5, 0, 15, 0);
                                    Expect_isTrue((ln_40 = a_7, (other_8 = b_7, (d1x_8 = ((ln_41 = ln_40, ln_41.ToX - ln_41.FromX)), (d1y_8 = ((ln_42 = ln_40, ln_42.ToY - ln_42.FromY)), (d2x_8 = ((ln_43 = other_8, ln_43.ToX - ln_43.FromX)), (d2y_8 = ((ln_44 = other_8, ln_44.ToY - ln_44.FromY)), (cross_8 = ((d1x_8 * d2y_8) - (d1y_8 * d2x_8)), (Math.abs(cross_8) < 1E-06) && ((px_1 = (ln_40.FromX - other_8.FromX), (py_1 = (ln_40.FromY - other_8.FromY), (cross_1_2 = ((px_1 * d2y_8) - (py_1 * d2x_8)), Math.abs(cross_1_2) < 1E-06))))))))))))("same ray offset start are coincident");
                                    Test_TestCaseBuilder__Zero(builder$0040_7);
                                })))), delay(() => {
                                    let builder$0040_8;
                                    return append(singleton((builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast parallel but offset", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
                                        let ln_45, other_9, d1x_9, ln_46, d1y_9, ln_47, d2x_9, ln_48, d2y_9, ln_49, cross_9, px_2, py_2, cross_1_3;
                                        const a_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                        const b_8 = Line2D_$ctor_77D16AC0(0, 2, 10, 2);
                                        Expect_isFalse((ln_45 = a_8, (other_9 = b_8, (d1x_9 = ((ln_46 = ln_45, ln_46.ToX - ln_46.FromX)), (d1y_9 = ((ln_47 = ln_45, ln_47.ToY - ln_47.FromY)), (d2x_9 = ((ln_48 = other_9, ln_48.ToX - ln_48.FromX)), (d2y_9 = ((ln_49 = other_9, ln_49.ToY - ln_49.FromY)), (cross_9 = ((d1x_9 * d2y_9) - (d1y_9 * d2x_9)), (Math.abs(cross_9) < 1E-06) && ((px_2 = (ln_45.FromX - other_9.FromX), (py_2 = (ln_45.FromY - other_9.FromY), (cross_1_3 = ((px_2 * d2y_9) - (py_2 * d2x_9)), Math.abs(cross_1_3) < 1E-06))))))))))))("parallel lines with offset are not coincident");
                                        Test_TestCaseBuilder__Zero(builder$0040_8);
                                    })))), delay(() => {
                                        let builder$0040_9;
                                        return append(singleton((builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast perpendicular lines", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
                                            let ln_50, other_10, d1x_10, ln_51, d1y_10, ln_52, d2x_10, ln_53, d2y_10, ln_54, cross_10, px_3, py_3, cross_1_4;
                                            const a_9 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                            const b_9 = Line2D_$ctor_77D16AC0(0, 0, 0, 10);
                                            Expect_isFalse((ln_50 = a_9, (other_10 = b_9, (d1x_10 = ((ln_51 = ln_50, ln_51.ToX - ln_51.FromX)), (d1y_10 = ((ln_52 = ln_50, ln_52.ToY - ln_52.FromY)), (d2x_10 = ((ln_53 = other_10, ln_53.ToX - ln_53.FromX)), (d2y_10 = ((ln_54 = other_10, ln_54.ToY - ln_54.FromY)), (cross_10 = ((d1x_10 * d2y_10) - (d1y_10 * d2x_10)), (Math.abs(cross_10) < 1E-06) && ((px_3 = (ln_50.FromX - other_10.FromX), (py_3 = (ln_50.FromY - other_10.FromY), (cross_1_4 = ((px_3 * d2y_10) - (py_3 * d2x_10)), Math.abs(cross_1_4) < 1E-06))))))))))))("perpendicular lines are not coincident");
                                            Test_TestCaseBuilder__Zero(builder$0040_9);
                                        })))), delay(() => {
                                            let builder$0040_10;
                                            return append(singleton((builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast opposite directions same line", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
                                                let ln_55, other_11, d1x_11, ln_56, d1y_11, ln_57, d2x_11, ln_58, d2y_11, ln_59, cross_11, px_4, py_4, cross_1_5;
                                                const a_10 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                const b_10 = Line2D_$ctor_77D16AC0(10, 0, 0, 0);
                                                Expect_isTrue((ln_55 = a_10, (other_11 = b_10, (d1x_11 = ((ln_56 = ln_55, ln_56.ToX - ln_56.FromX)), (d1y_11 = ((ln_57 = ln_55, ln_57.ToY - ln_57.FromY)), (d2x_11 = ((ln_58 = other_11, ln_58.ToX - ln_58.FromX)), (d2y_11 = ((ln_59 = other_11, ln_59.ToY - ln_59.FromY)), (cross_11 = ((d1x_11 * d2y_11) - (d1y_11 * d2x_11)), (Math.abs(cross_11) < 1E-06) && ((px_4 = (ln_55.FromX - other_11.FromX), (py_4 = (ln_55.FromY - other_11.FromY), (cross_1_5 = ((px_4 * d2y_11) - (py_4 * d2x_11)), Math.abs(cross_1_5) < 1E-06))))))))))))("opposite directions on same line are coincident");
                                                Test_TestCaseBuilder__Zero(builder$0040_10);
                                            })))), delay(() => {
                                                let builder$0040_11;
                                                return append(singleton((builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast same direction", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
                                                    let ln_60, other_12, d1x_12, ln_61, d1y_12, ln_62, d2x_12, ln_63, d2y_12, ln_64, cross_12, dot;
                                                    const a_11 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                    const b_11 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
                                                    Expect_isTrue((ln_60 = a_11, (other_12 = b_11, (d1x_12 = ((ln_61 = ln_60, ln_61.ToX - ln_61.FromX)), (d1y_12 = ((ln_62 = ln_60, ln_62.ToY - ln_62.FromY)), (d2x_12 = ((ln_63 = other_12, ln_63.ToX - ln_63.FromX)), (d2y_12 = ((ln_64 = other_12, ln_64.ToY - ln_64.FromY)), (cross_12 = ((d1x_12 * d2y_12) - (d1y_12 * d2x_12)), (Math.abs(cross_12) < 1E-06) && ((dot = ((d1x_12 * d2x_12) + (d1y_12 * d2y_12)), dot > 1E-06))))))))))("parallel same direction are oriented");
                                                    Test_TestCaseBuilder__Zero(builder$0040_11);
                                                })))), delay(() => {
                                                    let builder$0040_12;
                                                    return append(singleton((builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast opposite direction", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
                                                        let ln_65, other_13, d1x_13, ln_66, d1y_13, ln_67, d2x_13, ln_68, d2y_13, ln_69, cross_13, dot_1;
                                                        const a_12 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                        const b_12 = Line2D_$ctor_77D16AC0(10, 5, 0, 5);
                                                        Expect_isFalse((ln_65 = a_12, (other_13 = b_12, (d1x_13 = ((ln_66 = ln_65, ln_66.ToX - ln_66.FromX)), (d1y_13 = ((ln_67 = ln_65, ln_67.ToY - ln_67.FromY)), (d2x_13 = ((ln_68 = other_13, ln_68.ToX - ln_68.FromX)), (d2y_13 = ((ln_69 = other_13, ln_69.ToY - ln_69.FromY)), (cross_13 = ((d1x_13 * d2y_13) - (d1y_13 * d2x_13)), (Math.abs(cross_13) < 1E-06) && ((dot_1 = ((d1x_13 * d2x_13) + (d1y_13 * d2y_13)), dot_1 > 1E-06))))))))))("parallel opposite direction are not oriented");
                                                        Test_TestCaseBuilder__Zero(builder$0040_12);
                                                    })))), delay(() => {
                                                        let builder$0040_13;
                                                        return append(singleton((builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast perpendicular", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
                                                            let ln_70, other_14, d1x_14, ln_71, d1y_14, ln_72, d2x_14, ln_73, d2y_14, ln_74, cross_14, dot_2;
                                                            const a_13 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                            const b_13 = Line2D_$ctor_77D16AC0(0, 0, 0, 10);
                                                            Expect_isFalse((ln_70 = a_13, (other_14 = b_13, (d1x_14 = ((ln_71 = ln_70, ln_71.ToX - ln_71.FromX)), (d1y_14 = ((ln_72 = ln_70, ln_72.ToY - ln_72.FromY)), (d2x_14 = ((ln_73 = other_14, ln_73.ToX - ln_73.FromX)), (d2y_14 = ((ln_74 = other_14, ln_74.ToY - ln_74.FromY)), (cross_14 = ((d1x_14 * d2y_14) - (d1y_14 * d2x_14)), (Math.abs(cross_14) < 1E-06) && ((dot_2 = ((d1x_14 * d2x_14) + (d1y_14 * d2y_14)), dot_2 > 1E-06))))))))))("perpendicular lines are not parallel and oriented");
                                                            Test_TestCaseBuilder__Zero(builder$0040_13);
                                                        })))), delay(() => {
                                                            let builder$0040_14;
                                                            return append(singleton((builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast zero length returns false", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
                                                                let ln_75, other_15, d1x_15, ln_76, d1y_15, ln_77, d2x_15, ln_78, d2y_15, ln_79, cross_15, dot_3;
                                                                const a_14 = Line2D_$ctor_77D16AC0(0, 0, 0, 0);
                                                                const b_14 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                Expect_isFalse((ln_75 = a_14, (other_15 = b_14, (d1x_15 = ((ln_76 = ln_75, ln_76.ToX - ln_76.FromX)), (d1y_15 = ((ln_77 = ln_75, ln_77.ToY - ln_77.FromY)), (d2x_15 = ((ln_78 = other_15, ln_78.ToX - ln_78.FromX)), (d2y_15 = ((ln_79 = other_15, ln_79.ToY - ln_79.FromY)), (cross_15 = ((d1x_15 * d2y_15) - (d1y_15 * d2x_15)), (Math.abs(cross_15) < 1E-06) && ((dot_3 = ((d1x_15 * d2x_15) + (d1y_15 * d2y_15)), dot_3 > 1E-06))))))))))("zero length line returns false due to minDotProduct");
                                                                Test_TestCaseBuilder__Zero(builder$0040_14);
                                                            })))), delay(() => {
                                                                let builder$0040_15;
                                                                return append(singleton((builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast very short lines", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
                                                                    let ln_80, other_16, d1x_16, ln_81, d1y_16, ln_82, d2x_16, ln_83, d2y_16, ln_84, cross_16, dot_4;
                                                                    const a_15 = Line2D_$ctor_77D16AC0(0, 0, 1E-07, 0);
                                                                    const b_15 = Line2D_$ctor_77D16AC0(0, 0, 1E-07, 0);
                                                                    Expect_isFalse((ln_80 = a_15, (other_16 = b_15, (d1x_16 = ((ln_81 = ln_80, ln_81.ToX - ln_81.FromX)), (d1y_16 = ((ln_82 = ln_80, ln_82.ToY - ln_82.FromY)), (d2x_16 = ((ln_83 = other_16, ln_83.ToX - ln_83.FromX)), (d2y_16 = ((ln_84 = other_16, ln_84.ToY - ln_84.FromY)), (cross_16 = ((d1x_16 * d2y_16) - (d1y_16 * d2x_16)), (Math.abs(cross_16) < 1E-06) && ((dot_4 = ((d1x_16 * d2x_16) + (d1y_16 * d2y_16)), dot_4 > 1E-06))))))))))("very short lines below minDotProduct");
                                                                    Test_TestCaseBuilder__Zero(builder$0040_15);
                                                                })))), delay(() => {
                                                                    let builder$0040_16;
                                                                    return append(singleton((builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast opposite direction", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
                                                                        let ln_85, other_17, d1x_17, ln_86, d1y_17, ln_87, d2x_17, ln_88, d2y_17, ln_89, cross_17, dot_5;
                                                                        const a_16 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                        const b_16 = Line2D_$ctor_77D16AC0(10, 5, 0, 5);
                                                                        Expect_isTrue((ln_85 = a_16, (other_17 = b_16, (d1x_17 = ((ln_86 = ln_85, ln_86.ToX - ln_86.FromX)), (d1y_17 = ((ln_87 = ln_85, ln_87.ToY - ln_87.FromY)), (d2x_17 = ((ln_88 = other_17, ln_88.ToX - ln_88.FromX)), (d2y_17 = ((ln_89 = other_17, ln_89.ToY - ln_89.FromY)), (cross_17 = ((d1x_17 * d2y_17) - (d1y_17 * d2x_17)), (Math.abs(cross_17) < 1E-06) && ((dot_5 = ((d1x_17 * d2x_17) + (d1y_17 * d2y_17)), dot_5 < -1E-06))))))))))("parallel opposite direction are opposing");
                                                                        Test_TestCaseBuilder__Zero(builder$0040_16);
                                                                    })))), delay(() => {
                                                                        let builder$0040_17;
                                                                        return append(singleton((builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast same direction", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
                                                                            let ln_90, other_18, d1x_18, ln_91, d1y_18, ln_92, d2x_18, ln_93, d2y_18, ln_94, cross_18, dot_6;
                                                                            const a_17 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                            const b_17 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
                                                                            Expect_isFalse((ln_90 = a_17, (other_18 = b_17, (d1x_18 = ((ln_91 = ln_90, ln_91.ToX - ln_91.FromX)), (d1y_18 = ((ln_92 = ln_90, ln_92.ToY - ln_92.FromY)), (d2x_18 = ((ln_93 = other_18, ln_93.ToX - ln_93.FromX)), (d2y_18 = ((ln_94 = other_18, ln_94.ToY - ln_94.FromY)), (cross_18 = ((d1x_18 * d2y_18) - (d1y_18 * d2x_18)), (Math.abs(cross_18) < 1E-06) && ((dot_6 = ((d1x_18 * d2x_18) + (d1y_18 * d2y_18)), dot_6 < -1E-06))))))))))("parallel same direction are not opposing");
                                                                            Test_TestCaseBuilder__Zero(builder$0040_17);
                                                                        })))), delay(() => {
                                                                            let builder$0040_18;
                                                                            return append(singleton((builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast perpendicular", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
                                                                                let ln_95, other_19, d1x_19, ln_96, d1y_19, ln_97, d2x_19, ln_98, d2y_19, ln_99, cross_19, dot_7;
                                                                                const a_18 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                                const b_18 = Line2D_$ctor_77D16AC0(0, 0, 0, 10);
                                                                                Expect_isFalse((ln_95 = a_18, (other_19 = b_18, (d1x_19 = ((ln_96 = ln_95, ln_96.ToX - ln_96.FromX)), (d1y_19 = ((ln_97 = ln_95, ln_97.ToY - ln_97.FromY)), (d2x_19 = ((ln_98 = other_19, ln_98.ToX - ln_98.FromX)), (d2y_19 = ((ln_99 = other_19, ln_99.ToY - ln_99.FromY)), (cross_19 = ((d1x_19 * d2y_19) - (d1y_19 * d2x_19)), (Math.abs(cross_19) < 1E-06) && ((dot_7 = ((d1x_19 * d2x_19) + (d1y_19 * d2y_19)), dot_7 < -1E-06))))))))))("perpendicular lines are not parallel and opposing");
                                                                                Test_TestCaseBuilder__Zero(builder$0040_18);
                                                                            })))), delay(() => {
                                                                                let builder$0040_19;
                                                                                return append(singleton((builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast zero length returns false", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
                                                                                    let ln_100, other_20, d1x_20, ln_101, d1y_20, ln_102, d2x_20, ln_103, d2y_20, ln_104, cross_20, dot_8;
                                                                                    const a_19 = Line2D_$ctor_77D16AC0(0, 0, 0, 0);
                                                                                    const b_19 = Line2D_$ctor_77D16AC0(10, 0, 0, 0);
                                                                                    Expect_isFalse((ln_100 = a_19, (other_20 = b_19, (d1x_20 = ((ln_101 = ln_100, ln_101.ToX - ln_101.FromX)), (d1y_20 = ((ln_102 = ln_100, ln_102.ToY - ln_102.FromY)), (d2x_20 = ((ln_103 = other_20, ln_103.ToX - ln_103.FromX)), (d2y_20 = ((ln_104 = other_20, ln_104.ToY - ln_104.FromY)), (cross_20 = ((d1x_20 * d2y_20) - (d1y_20 * d2x_20)), (Math.abs(cross_20) < 1E-06) && ((dot_8 = ((d1x_20 * d2x_20) + (d1y_20 * d2y_20)), dot_8 < -1E-06))))))))))("zero length line returns false due to maxDotProduct");
                                                                                    Test_TestCaseBuilder__Zero(builder$0040_19);
                                                                                })))), delay(() => {
                                                                                    let builder$0040_20;
                                                                                    return append(singleton((builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast same direction same line", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
                                                                                        let ln_105, other_21, d1x_21, ln_106, d1y_21, ln_107, d2x_21, ln_108, d2y_21, ln_109, cross_21, dot_9, px_5, py_5, cross_1_6;
                                                                                        const a_20 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                                        const b_20 = Line2D_$ctor_77D16AC0(5, 0, 15, 0);
                                                                                        Expect_isTrue((ln_105 = a_20, (other_21 = b_20, (d1x_21 = ((ln_106 = ln_105, ln_106.ToX - ln_106.FromX)), (d1y_21 = ((ln_107 = ln_105, ln_107.ToY - ln_107.FromY)), (d2x_21 = ((ln_108 = other_21, ln_108.ToX - ln_108.FromX)), (d2y_21 = ((ln_109 = other_21, ln_109.ToY - ln_109.FromY)), (cross_21 = ((d1x_21 * d2y_21) - (d1y_21 * d2x_21)), (Math.abs(cross_21) < 1E-06) && ((dot_9 = ((d1x_21 * d2x_21) + (d1y_21 * d2y_21)), (dot_9 > 1E-06) && ((px_5 = (ln_105.FromX - other_21.FromX), (py_5 = (ln_105.FromY - other_21.FromY), (cross_1_6 = ((px_5 * d2y_21) - (py_5 * d2x_21)), Math.abs(cross_1_6) < 1E-06))))))))))))))("same direction same line are coincident and oriented");
                                                                                        Test_TestCaseBuilder__Zero(builder$0040_20);
                                                                                    })))), delay(() => {
                                                                                        let builder$0040_21;
                                                                                        return append(singleton((builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast opposite direction same line", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
                                                                                            let ln_110, other_22, d1x_22, ln_111, d1y_22, ln_112, d2x_22, ln_113, d2y_22, ln_114, cross_22, dot_10, px_6, py_6, cross_1_7;
                                                                                            const a_21 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                                            const b_21 = Line2D_$ctor_77D16AC0(10, 0, 0, 0);
                                                                                            Expect_isFalse((ln_110 = a_21, (other_22 = b_21, (d1x_22 = ((ln_111 = ln_110, ln_111.ToX - ln_111.FromX)), (d1y_22 = ((ln_112 = ln_110, ln_112.ToY - ln_112.FromY)), (d2x_22 = ((ln_113 = other_22, ln_113.ToX - ln_113.FromX)), (d2y_22 = ((ln_114 = other_22, ln_114.ToY - ln_114.FromY)), (cross_22 = ((d1x_22 * d2y_22) - (d1y_22 * d2x_22)), (Math.abs(cross_22) < 1E-06) && ((dot_10 = ((d1x_22 * d2x_22) + (d1y_22 * d2y_22)), (dot_10 > 1E-06) && ((px_6 = (ln_110.FromX - other_22.FromX), (py_6 = (ln_110.FromY - other_22.FromY), (cross_1_7 = ((px_6 * d2y_22) - (py_6 * d2x_22)), Math.abs(cross_1_7) < 1E-06))))))))))))))("opposite direction same line are not oriented");
                                                                                            Test_TestCaseBuilder__Zero(builder$0040_21);
                                                                                        })))), delay(() => {
                                                                                            let builder$0040_22;
                                                                                            return append(singleton((builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast parallel offset", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
                                                                                                let ln_115, other_23, d1x_23, ln_116, d1y_23, ln_117, d2x_23, ln_118, d2y_23, ln_119, cross_23, dot_11, px_7, py_7, cross_1_8;
                                                                                                const a_22 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                                                const b_22 = Line2D_$ctor_77D16AC0(0, 2, 10, 2);
                                                                                                Expect_isFalse((ln_115 = a_22, (other_23 = b_22, (d1x_23 = ((ln_116 = ln_115, ln_116.ToX - ln_116.FromX)), (d1y_23 = ((ln_117 = ln_115, ln_117.ToY - ln_117.FromY)), (d2x_23 = ((ln_118 = other_23, ln_118.ToX - ln_118.FromX)), (d2y_23 = ((ln_119 = other_23, ln_119.ToY - ln_119.FromY)), (cross_23 = ((d1x_23 * d2y_23) - (d1y_23 * d2x_23)), (Math.abs(cross_23) < 1E-06) && ((dot_11 = ((d1x_23 * d2x_23) + (d1y_23 * d2y_23)), (dot_11 > 1E-06) && ((px_7 = (ln_115.FromX - other_23.FromX), (py_7 = (ln_115.FromY - other_23.FromY), (cross_1_8 = ((px_7 * d2y_23) - (py_7 * d2x_23)), Math.abs(cross_1_8) < 1E-06))))))))))))))("parallel with offset are not coincident");
                                                                                                Test_TestCaseBuilder__Zero(builder$0040_22);
                                                                                            })))), delay(() => {
                                                                                                let builder$0040_23;
                                                                                                return append(singleton((builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast diagonal same ray", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
                                                                                                    let ln_120, other_24, d1x_24, ln_121, d1y_24, ln_122, d2x_24, ln_123, d2y_24, ln_124, cross_24, dot_12, px_8, py_8, cross_1_9;
                                                                                                    const a_23 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
                                                                                                    const b_23 = Line2D_$ctor_77D16AC0(5, 5, 15, 15);
                                                                                                    Expect_isTrue((ln_120 = a_23, (other_24 = b_23, (d1x_24 = ((ln_121 = ln_120, ln_121.ToX - ln_121.FromX)), (d1y_24 = ((ln_122 = ln_120, ln_122.ToY - ln_122.FromY)), (d2x_24 = ((ln_123 = other_24, ln_123.ToX - ln_123.FromX)), (d2y_24 = ((ln_124 = other_24, ln_124.ToY - ln_124.FromY)), (cross_24 = ((d1x_24 * d2y_24) - (d1y_24 * d2x_24)), (Math.abs(cross_24) < 1E-06) && ((dot_12 = ((d1x_24 * d2x_24) + (d1y_24 * d2y_24)), (dot_12 > 1E-06) && ((px_8 = (ln_120.FromX - other_24.FromX), (py_8 = (ln_120.FromY - other_24.FromY), (cross_1_9 = ((px_8 * d2y_24) - (py_8 * d2x_24)), Math.abs(cross_1_9) < 1E-06))))))))))))))("diagonal same ray are coincident and oriented");
                                                                                                    Test_TestCaseBuilder__Zero(builder$0040_23);
                                                                                                })))), delay(() => {
                                                                                                    let builder$0040_24;
                                                                                                    return append(singleton((builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast opposite direction same line", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
                                                                                                        let ln_125, other_25, d1x_25, ln_126, d1y_25, ln_127, d2x_25, ln_128, d2y_25, ln_129, cross_25, dot_13, px_9, py_9, cross_1_10;
                                                                                                        const a_24 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                                                        const b_24 = Line2D_$ctor_77D16AC0(10, 0, 0, 0);
                                                                                                        Expect_isTrue((ln_125 = a_24, (other_25 = b_24, (d1x_25 = ((ln_126 = ln_125, ln_126.ToX - ln_126.FromX)), (d1y_25 = ((ln_127 = ln_125, ln_127.ToY - ln_127.FromY)), (d2x_25 = ((ln_128 = other_25, ln_128.ToX - ln_128.FromX)), (d2y_25 = ((ln_129 = other_25, ln_129.ToY - ln_129.FromY)), (cross_25 = ((d1x_25 * d2y_25) - (d1y_25 * d2x_25)), (Math.abs(cross_25) < 1E-06) && ((dot_13 = ((d1x_25 * d2x_25) + (d1y_25 * d2y_25)), (dot_13 < -1E-06) && ((px_9 = (ln_125.FromX - other_25.FromX), (py_9 = (ln_125.FromY - other_25.FromY), (cross_1_10 = ((px_9 * d2y_25) - (py_9 * d2x_25)), Math.abs(cross_1_10) < 1E-06))))))))))))))("opposite direction same line are coincident and opposing");
                                                                                                        Test_TestCaseBuilder__Zero(builder$0040_24);
                                                                                                    })))), delay(() => {
                                                                                                        let builder$0040_25;
                                                                                                        return append(singleton((builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast same direction same line", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
                                                                                                            let ln_130, other_26, d1x_26, ln_131, d1y_26, ln_132, d2x_26, ln_133, d2y_26, ln_134, cross_26, dot_14, px_10, py_10, cross_1_11;
                                                                                                            const a_25 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                                                            const b_25 = Line2D_$ctor_77D16AC0(5, 0, 15, 0);
                                                                                                            Expect_isFalse((ln_130 = a_25, (other_26 = b_25, (d1x_26 = ((ln_131 = ln_130, ln_131.ToX - ln_131.FromX)), (d1y_26 = ((ln_132 = ln_130, ln_132.ToY - ln_132.FromY)), (d2x_26 = ((ln_133 = other_26, ln_133.ToX - ln_133.FromX)), (d2y_26 = ((ln_134 = other_26, ln_134.ToY - ln_134.FromY)), (cross_26 = ((d1x_26 * d2y_26) - (d1y_26 * d2x_26)), (Math.abs(cross_26) < 1E-06) && ((dot_14 = ((d1x_26 * d2x_26) + (d1y_26 * d2y_26)), (dot_14 < -1E-06) && ((px_10 = (ln_130.FromX - other_26.FromX), (py_10 = (ln_130.FromY - other_26.FromY), (cross_1_11 = ((px_10 * d2y_26) - (py_10 * d2x_26)), Math.abs(cross_1_11) < 1E-06))))))))))))))("same direction same line are not opposing");
                                                                                                            Test_TestCaseBuilder__Zero(builder$0040_25);
                                                                                                        })))), delay(() => {
                                                                                                            let builder$0040_26;
                                                                                                            return append(singleton((builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast parallel offset", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
                                                                                                                let ln_135, other_27, d1x_27, ln_136, d1y_27, ln_137, d2x_27, ln_138, d2y_27, ln_139, cross_27, dot_15, px_11, py_11, cross_1_12;
                                                                                                                const a_26 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                                                                const b_26 = Line2D_$ctor_77D16AC0(10, 2, 0, 2);
                                                                                                                Expect_isFalse((ln_135 = a_26, (other_27 = b_26, (d1x_27 = ((ln_136 = ln_135, ln_136.ToX - ln_136.FromX)), (d1y_27 = ((ln_137 = ln_135, ln_137.ToY - ln_137.FromY)), (d2x_27 = ((ln_138 = other_27, ln_138.ToX - ln_138.FromX)), (d2y_27 = ((ln_139 = other_27, ln_139.ToY - ln_139.FromY)), (cross_27 = ((d1x_27 * d2y_27) - (d1y_27 * d2x_27)), (Math.abs(cross_27) < 1E-06) && ((dot_15 = ((d1x_27 * d2x_27) + (d1y_27 * d2y_27)), (dot_15 < -1E-06) && ((px_11 = (ln_135.FromX - other_27.FromX), (py_11 = (ln_135.FromY - other_27.FromY), (cross_1_12 = ((px_11 * d2y_27) - (py_11 * d2x_27)), Math.abs(cross_1_12) < 1E-06))))))))))))))("parallel with offset are not coincident");
                                                                                                                Test_TestCaseBuilder__Zero(builder$0040_26);
                                                                                                            })))), delay(() => {
                                                                                                                let builder$0040_27;
                                                                                                                return append(singleton((builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast diagonal opposing", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
                                                                                                                    let ln_140, other_28, d1x_28, ln_141, d1y_28, ln_142, d2x_28, ln_143, d2y_28, ln_144, cross_28, dot_16, px_12, py_12, cross_1_13;
                                                                                                                    const a_27 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
                                                                                                                    const b_27 = Line2D_$ctor_77D16AC0(15, 15, 5, 5);
                                                                                                                    Expect_isTrue((ln_140 = a_27, (other_28 = b_27, (d1x_28 = ((ln_141 = ln_140, ln_141.ToX - ln_141.FromX)), (d1y_28 = ((ln_142 = ln_140, ln_142.ToY - ln_142.FromY)), (d2x_28 = ((ln_143 = other_28, ln_143.ToX - ln_143.FromX)), (d2y_28 = ((ln_144 = other_28, ln_144.ToY - ln_144.FromY)), (cross_28 = ((d1x_28 * d2y_28) - (d1y_28 * d2x_28)), (Math.abs(cross_28) < 1E-06) && ((dot_16 = ((d1x_28 * d2x_28) + (d1y_28 * d2y_28)), (dot_16 < -1E-06) && ((px_12 = (ln_140.FromX - other_28.FromX), (py_12 = (ln_140.FromY - other_28.FromY), (cross_1_13 = ((px_12 * d2y_28) - (py_12 * d2x_28)), Math.abs(cross_1_13) < 1E-06))))))))))))))("diagonal opposing same line are coincident and opposing");
                                                                                                                    Test_TestCaseBuilder__Zero(builder$0040_27);
                                                                                                                })))), delay(() => {
                                                                                                                    let builder$0040_28;
                                                                                                                    return append(singleton((builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast custom tolerance strict", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
                                                                                                                        let ln_145, other_29, d1x_29, ln_146, d1y_29, ln_147, d2x_29, ln_148, d2y_29, ln_149, cross_29, ln_150, other_30, d1x_30, ln_151, d1y_30, ln_152, d2x_30, ln_153, d2y_30, ln_154, cross_30;
                                                                                                                        const a_28 = Line2D_$ctor_77D16AC0(0, 0, 100, 0);
                                                                                                                        const b_28 = Line2D_$ctor_77D16AC0(0, 0, 100, 0.5);
                                                                                                                        Expect_isFalse((ln_145 = a_28, (other_29 = b_28, (d1x_29 = ((ln_146 = ln_145, ln_146.ToX - ln_146.FromX)), (d1y_29 = ((ln_147 = ln_145, ln_147.ToY - ln_147.FromY)), (d2x_29 = ((ln_148 = other_29, ln_148.ToX - ln_148.FromX)), (d2y_29 = ((ln_149 = other_29, ln_149.ToY - ln_149.FromY)), (cross_29 = ((d1x_29 * d2y_29) - (d1y_29 * d2x_29)), Math.abs(cross_29) < 1E-09))))))))("with strict tolerance not parallel");
                                                                                                                        Expect_isTrue((ln_150 = a_28, (other_30 = b_28, (d1x_30 = ((ln_151 = ln_150, ln_151.ToX - ln_151.FromX)), (d1y_30 = ((ln_152 = ln_150, ln_152.ToY - ln_152.FromY)), (d2x_30 = ((ln_153 = other_30, ln_153.ToX - ln_153.FromX)), (d2y_30 = ((ln_154 = other_30, ln_154.ToY - ln_154.FromY)), (cross_30 = ((d1x_30 * d2y_30) - (d1y_30 * d2x_30)), Math.abs(cross_30) < 100))))))))("with relaxed tolerance parallel");
                                                                                                                        Test_TestCaseBuilder__Zero(builder$0040_28);
                                                                                                                    })))), delay(() => {
                                                                                                                        let builder$0040_29;
                                                                                                                        return append(singleton((builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast custom tolerances", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
                                                                                                                            let ln_155, other_31, d1x_31, ln_156, d1y_31, ln_157, d2x_31, ln_158, d2y_31, ln_159, cross_31, dot_17, px_13, py_13, cross_1_14, ln_160, other_32, d1x_32, ln_161, d1y_32, ln_162, d2x_32, ln_163, d2y_32, ln_164, cross_32, dot_18, px_14, py_14, cross_1_15;
                                                                                                                            const a_29 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
                                                                                                                            const b_29 = Line2D_$ctor_77D16AC0(0, 0, 5, 0);
                                                                                                                            Expect_isTrue((ln_155 = a_29, (other_31 = b_29, (d1x_31 = ((ln_156 = ln_155, ln_156.ToX - ln_156.FromX)), (d1y_31 = ((ln_157 = ln_155, ln_157.ToY - ln_157.FromY)), (d2x_31 = ((ln_158 = other_31, ln_158.ToX - ln_158.FromX)), (d2y_31 = ((ln_159 = other_31, ln_159.ToY - ln_159.FromY)), (cross_31 = ((d1x_31 * d2y_31) - (d1y_31 * d2x_31)), (Math.abs(cross_31) < 1E-06) && ((dot_17 = ((d1x_31 * d2x_31) + (d1y_31 * d2y_31)), (dot_17 > 1E-06) && ((px_13 = (ln_155.FromX - other_31.FromX), (py_13 = (ln_155.FromY - other_31.FromY), (cross_1_14 = ((px_13 * d2y_31) - (py_13 * d2x_31)), Math.abs(cross_1_14) < 1E-06))))))))))))))("with default tolerances coincident");
                                                                                                                            Expect_isFalse((ln_160 = a_29, (other_32 = b_29, (d1x_32 = ((ln_161 = ln_160, ln_161.ToX - ln_161.FromX)), (d1y_32 = ((ln_162 = ln_160, ln_162.ToY - ln_162.FromY)), (d2x_32 = ((ln_163 = other_32, ln_163.ToX - ln_163.FromX)), (d2y_32 = ((ln_164 = other_32, ln_164.ToY - ln_164.FromY)), (cross_32 = ((d1x_32 * d2y_32) - (d1y_32 * d2x_32)), (Math.abs(cross_32) < 1E-06) && ((dot_18 = ((d1x_32 * d2x_32) + (d1y_32 * d2y_32)), (dot_18 > 100) && ((px_14 = (ln_160.FromX - other_32.FromX), (py_14 = (ln_160.FromY - other_32.FromY), (cross_1_15 = ((px_14 * d2y_32) - (py_14 * d2x_32)), Math.abs(cross_1_15) < 1E-06))))))))))))))("with very strict minDotProduct not coincident");
                                                                                                                            Test_TestCaseBuilder__Zero(builder$0040_29);
                                                                                                                        })))), delay(() => append(collect((i) => {
                                                                                                                            let ln_166, ln_167, ln_169, ln_170, builder$0040_30;
                                                                                                                            const angle = rFloat(-180, 180);
                                                                                                                            const scale = rFloat(1, 100);
                                                                                                                            let vec;
                                                                                                                            let v_2;
                                                                                                                            const v_1 = Vc_$ctor_7B00E9A0(1, 0);
                                                                                                                            let r;
                                                                                                                            const rad = 0.017453292519943295 * angle;
                                                                                                                            r = Rotation2D_$ctor_7B00E9A0(Math.sin(rad), Math.cos(rad));
                                                                                                                            v_2 = Vc_$ctor_7B00E9A0((r.Cos * v_1.X) - (r.Sin * v_1.Y), (r.Sin * v_1.X) + (r.Cos * v_1.Y));
                                                                                                                            const f = scale;
                                                                                                                            vec = Vc_$ctor_7B00E9A0_1(v_2.X * f, v_2.Y * f);
                                                                                                                            let a_30;
                                                                                                                            const p = Pt_$ctor_7B00E9A0(0, 0);
                                                                                                                            const v_3 = vec;
                                                                                                                            a_30 = Line2D_$ctor_77D16AC0_1(p.X, p.Y, p.X + v_3.X, p.Y + v_3.Y);
                                                                                                                            let b_30;
                                                                                                                            let p_2;
                                                                                                                            const ln_165 = a_30;
                                                                                                                            const p_1 = rFloat(0, 1);
                                                                                                                            p_2 = Pt_$ctor_7B00E9A0_1(ln_165.FromX + (((ln_166 = ln_165, ln_166.ToX - ln_166.FromX)) * p_1), ln_165.FromY + (((ln_167 = ln_165, ln_167.ToY - ln_167.FromY)) * p_1));
                                                                                                                            let v_5;
                                                                                                                            let v_4;
                                                                                                                            const ln_168 = a_30;
                                                                                                                            v_4 = Vc_$ctor_7B00E9A0_1((ln_169 = ln_168, ln_169.ToX - ln_169.FromX), (ln_170 = ln_168, ln_170.ToY - ln_170.FromY));
                                                                                                                            const f_1 = rFloat(0.5, 2);
                                                                                                                            v_5 = Vc_$ctor_7B00E9A0_1(v_4.X * f_1, v_4.Y * f_1);
                                                                                                                            b_30 = Line2D_$ctor_77D16AC0_1(p_2.X, p_2.Y, p_2.X + v_5.X, p_2.Y + v_5.Y);
                                                                                                                            return singleton((builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F(toText(printf("Fast methods random same ray oriented %d"))(i), new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
                                                                                                                                let ln_171, other_33, d1x_33, ln_172, d1y_33, ln_173, d2x_33, ln_174, d2y_33, ln_175, cross_33, dot_19, ln_176, other_34, d1x_34, ln_177, d1y_34, ln_178, d2x_34, ln_179, d2y_34, ln_180, cross_34, dot_20, px_15, py_15, cross_1_16;
                                                                                                                                Expect_isTrue((ln_171 = a_30, (other_33 = b_30, (d1x_33 = ((ln_172 = ln_171, ln_172.ToX - ln_172.FromX)), (d1y_33 = ((ln_173 = ln_171, ln_173.ToY - ln_173.FromY)), (d2x_33 = ((ln_174 = other_33, ln_174.ToX - ln_174.FromX)), (d2y_33 = ((ln_175 = other_33, ln_175.ToY - ln_175.FromY)), (cross_33 = ((d1x_33 * d2y_33) - (d1y_33 * d2x_33)), (Math.abs(cross_33) < 1E-06) && ((dot_19 = ((d1x_33 * d2x_33) + (d1y_33 * d2y_33)), dot_19 > 1E-06))))))))))("random same ray are parallel and oriented");
                                                                                                                                Expect_isTrue((ln_176 = a_30, (other_34 = b_30, (d1x_34 = ((ln_177 = ln_176, ln_177.ToX - ln_177.FromX)), (d1y_34 = ((ln_178 = ln_176, ln_178.ToY - ln_178.FromY)), (d2x_34 = ((ln_179 = other_34, ln_179.ToX - ln_179.FromX)), (d2y_34 = ((ln_180 = other_34, ln_180.ToY - ln_180.FromY)), (cross_34 = ((d1x_34 * d2y_34) - (d1y_34 * d2x_34)), (Math.abs(cross_34) < 1E-06) && ((dot_20 = ((d1x_34 * d2x_34) + (d1y_34 * d2y_34)), (dot_20 > 1E-06) && ((px_15 = (ln_176.FromX - other_34.FromX), (py_15 = (ln_176.FromY - other_34.FromY), (cross_1_16 = ((px_15 * d2y_34) - (py_15 * d2x_34)), Math.abs(cross_1_16) < 1E-06))))))))))))))("random same ray are coincident and oriented");
                                                                                                                                Test_TestCaseBuilder__Zero(builder$0040_30);
                                                                                                                            }))));
                                                                                                                        }, rangeDouble(1, 1, 10)), delay(() => collect((i_1) => {
                                                                                                                            let ln_182, ln_183, ln_185, ln_186, builder$0040_31;
                                                                                                                            const angle_1 = rFloat(-180, 180);
                                                                                                                            const scale_1 = rFloat(1, 100);
                                                                                                                            let vec_1;
                                                                                                                            let v_8;
                                                                                                                            const v_7 = Vc_$ctor_7B00E9A0(1, 0);
                                                                                                                            let r_1;
                                                                                                                            const rad_1 = 0.017453292519943295 * angle_1;
                                                                                                                            r_1 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_1), Math.cos(rad_1));
                                                                                                                            v_8 = Vc_$ctor_7B00E9A0((r_1.Cos * v_7.X) - (r_1.Sin * v_7.Y), (r_1.Sin * v_7.X) + (r_1.Cos * v_7.Y));
                                                                                                                            const f_2 = scale_1;
                                                                                                                            vec_1 = Vc_$ctor_7B00E9A0_1(v_8.X * f_2, v_8.Y * f_2);
                                                                                                                            let a_31;
                                                                                                                            const p_3 = Pt_$ctor_7B00E9A0(0, 0);
                                                                                                                            const v_9 = vec_1;
                                                                                                                            a_31 = Line2D_$ctor_77D16AC0_1(p_3.X, p_3.Y, p_3.X + v_9.X, p_3.Y + v_9.Y);
                                                                                                                            let b_31;
                                                                                                                            let p_5;
                                                                                                                            const ln_181 = a_31;
                                                                                                                            const p_4 = rFloat(0, 1);
                                                                                                                            p_5 = Pt_$ctor_7B00E9A0_1(ln_181.FromX + (((ln_182 = ln_181, ln_182.ToX - ln_182.FromX)) * p_4), ln_181.FromY + (((ln_183 = ln_181, ln_183.ToY - ln_183.FromY)) * p_4));
                                                                                                                            let v_12;
                                                                                                                            let v_11;
                                                                                                                            let v_10;
                                                                                                                            const ln_184 = a_31;
                                                                                                                            v_10 = Vc_$ctor_7B00E9A0_1((ln_185 = ln_184, ln_185.ToX - ln_185.FromX), (ln_186 = ln_184, ln_186.ToY - ln_186.FromY));
                                                                                                                            v_11 = Vc_$ctor_7B00E9A0_1(-v_10.X, -v_10.Y);
                                                                                                                            const f_3 = rFloat(0.5, 2);
                                                                                                                            v_12 = Vc_$ctor_7B00E9A0_1(v_11.X * f_3, v_11.Y * f_3);
                                                                                                                            b_31 = Line2D_$ctor_77D16AC0_1(p_5.X, p_5.Y, p_5.X + v_12.X, p_5.Y + v_12.Y);
                                                                                                                            return singleton((builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F(toText(printf("Fast methods random same ray opposing %d"))(i_1), new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
                                                                                                                                let ln_187, other_35, d1x_35, ln_188, d1y_35, ln_189, d2x_35, ln_190, d2y_35, ln_191, cross_35, dot_21, ln_192, other_36, d1x_36, ln_193, d1y_36, ln_194, d2x_36, ln_195, d2y_36, ln_196, cross_36, dot_22, px_16, py_16, cross_1_17;
                                                                                                                                Expect_isTrue((ln_187 = a_31, (other_35 = b_31, (d1x_35 = ((ln_188 = ln_187, ln_188.ToX - ln_188.FromX)), (d1y_35 = ((ln_189 = ln_187, ln_189.ToY - ln_189.FromY)), (d2x_35 = ((ln_190 = other_35, ln_190.ToX - ln_190.FromX)), (d2y_35 = ((ln_191 = other_35, ln_191.ToY - ln_191.FromY)), (cross_35 = ((d1x_35 * d2y_35) - (d1y_35 * d2x_35)), (Math.abs(cross_35) < 1E-06) && ((dot_21 = ((d1x_35 * d2x_35) + (d1y_35 * d2y_35)), dot_21 < -1E-06))))))))))("random opposing are parallel and opposing");
                                                                                                                                Expect_isTrue((ln_192 = a_31, (other_36 = b_31, (d1x_36 = ((ln_193 = ln_192, ln_193.ToX - ln_193.FromX)), (d1y_36 = ((ln_194 = ln_192, ln_194.ToY - ln_194.FromY)), (d2x_36 = ((ln_195 = other_36, ln_195.ToX - ln_195.FromX)), (d2y_36 = ((ln_196 = other_36, ln_196.ToY - ln_196.FromY)), (cross_36 = ((d1x_36 * d2y_36) - (d1y_36 * d2x_36)), (Math.abs(cross_36) < 1E-06) && ((dot_22 = ((d1x_36 * d2x_36) + (d1y_36 * d2y_36)), (dot_22 < -1E-06) && ((px_16 = (ln_192.FromX - other_36.FromX), (py_16 = (ln_192.FromY - other_36.FromY), (cross_1_17 = ((px_16 * d2y_36) - (py_16 * d2x_36)), Math.abs(cross_1_17) < 1E-06))))))))))))))("random opposing are coincident and opposing");
                                                                                                                                Test_TestCaseBuilder__Zero(builder$0040_31);
                                                                                                                            }))));
                                                                                                                        }, rangeDouble(1, 1, 10))))));
                                                                                                                    }));
                                                                                                                }));
                                                                                                            }));
                                                                                                        }));
                                                                                                    }));
                                                                                                }));
                                                                                            }));
                                                                                        }));
                                                                                    }));
                                                                                }));
                                                                            }));
                                                                        }));
                                                                    }));
                                                                }));
                                                            }));
                                                        }));
                                                    }));
                                                }));
                                            }));
                                        }));
                                    }));
                                }));
                            }));
                        }));
                    }));
                }));
            }));
        }));
    }));
})));

export const testsDistanceBetweenLines = Test_testList("Line2D distanceBetweenLines", toList(delay(() => append(collect((i) => {
    let ln_1, ln_2;
    const v = Vc_$ctor_7B00E9A0_2(rFloat(-99, 99), rFloat(-99, 99));
    const p = Pt_$ctor_7B00E9A0_2(rFloat(-99, 99), rFloat(-99, 99));
    let a;
    const p_1 = p;
    const v_1 = v;
    a = Line2D_$ctor_77D16AC0_1(p_1.X, p_1.Y, p_1.X + v_1.X, p_1.Y + v_1.Y);
    let t;
    const ln = a;
    const p_2 = rFloat(0, 1);
    t = Pt_$ctor_7B00E9A0_1(ln.FromX + (((ln_1 = ln, ln_1.ToX - ln_1.FromX)) * p_2), ln.FromY + (((ln_2 = ln, ln_2.ToY - ln_2.FromY)) * p_2));
    let b;
    const p_3 = t;
    let v_3;
    const v_2 = v;
    const f = rFloat(-2, 2);
    v_3 = Vc_$ctor_7B00E9A0_1(v_2.X * f, v_2.Y * f);
    b = Line2D_$ctor_77D16AC0_1(p_3.X, p_3.Y, p_3.X + v_3.X, p_3.Y + v_3.Y);
    const d = Euclid_Line2D__Line2D_distanceToLine_Static(a, b);
    return append((d > 1E-09) ? ((toConsole(`line A from to: (${a.FromX}, ${a.FromY}) , (${a.ToX}, ${a.ToY})`), (toConsole(`line B from to: (${b.FromX}, ${b.FromY}) , (${b.ToX}, ${b.ToY})`), (toConsole(`distance: ${d}`), empty())))) : empty(), delay(() => {
        let builder$0040;
        return singleton((builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F(toText(printf("distanceBetweenLines random %d"))(i), new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
            Expect_isTrue(d < 1E-09)("distance ok");
            Test_TestCaseBuilder__Zero(builder$0040);
        }))));
    }));
}, rangeDouble(1, 1, 10)), delay(() => {
    let builder$0040_1;
    return append(singleton((builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Line distanceBetweenLines manual 1", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_1;
        const p_4 = Pt_$ctor_7B00E9A0_2(0, 0);
        const v_4 = Vc_$ctor_7B00E9A0(1, 0);
        a_1 = Line2D_$ctor_77D16AC0_1(p_4.X, p_4.Y, p_4.X + v_4.X, p_4.Y + v_4.Y);
        let b_1;
        const p_5 = Pt_$ctor_7B00E9A0_2(0, 2);
        const v_5 = Vc_$ctor_7B00E9A0(1, 0);
        b_1 = Line2D_$ctor_77D16AC0_1(p_5.X, p_5.Y, p_5.X + v_5.X, p_5.Y + v_5.Y);
        const d_1 = Euclid_Line2D__Line2D_distanceToLine_Static(a_1, b_1);
        expectEqualEpsilon(d_1, 2, "distance is 2.");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    })))), delay(() => {
        let builder$0040_2;
        return append(singleton((builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Line distanceBetweenLines manual 2", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
            let a_2;
            const p_6 = Pt_$ctor_7B00E9A0_2(0, 0);
            const v_6 = Vc_$ctor_7B00E9A0(1, 0);
            a_2 = Line2D_$ctor_77D16AC0_1(p_6.X, p_6.Y, p_6.X + v_6.X, p_6.Y + v_6.Y);
            let b_3;
            const p_7 = Pt_$ctor_7B00E9A0_2(1, 2);
            const v_7 = Vc_$ctor_7B00E9A0(1, 0);
            b_3 = Line2D_$ctor_77D16AC0_1(p_7.X, p_7.Y, p_7.X + v_7.X, p_7.Y + v_7.Y);
            const d_2 = Euclid_Line2D__Line2D_distanceToLine_Static(a_2, b_3);
            expectEqualEpsilon(d_2, 2, "distance is 2.");
            Test_TestCaseBuilder__Zero(builder$0040_2);
        })))), delay(() => {
            let builder$0040_3;
            return append(singleton((builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Line distanceBetweenLines manual 3", new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
                let a_3;
                const p_8 = Pt_$ctor_7B00E9A0_2(0, 0);
                const v_8 = Vc_$ctor_7B00E9A0(0, 1);
                a_3 = Line2D_$ctor_77D16AC0_1(p_8.X, p_8.Y, p_8.X + v_8.X, p_8.Y + v_8.Y);
                let b_5;
                const p_9 = Pt_$ctor_7B00E9A0_2(2, 1);
                const v_9 = Vc_$ctor_7B00E9A0(0, 1);
                b_5 = Line2D_$ctor_77D16AC0_1(p_9.X, p_9.Y, p_9.X + v_9.X, p_9.Y + v_9.Y);
                const d_3 = Euclid_Line2D__Line2D_distanceToLine_Static(a_3, b_5);
                expectEqualEpsilon(d_3, 2, "distance is 2.");
                Test_TestCaseBuilder__Zero(builder$0040_3);
            })))), delay(() => {
                let continueShift = true;
                return collect((i_1) => {
                    let ln_9, ln_10, a_9, v_10, ln_5, ln_6, ln_7, x, y, l, b_12, v_11, x_3, y_2, l_1, dot, a_10, b_13, x_6, y_4, x_7, y_5;
                    if (continueShift) {
                        const ii = i_1;
                        const shift = ((ii * ii) * ii) * 0.001;
                        const a_4 = Line2D_$ctor_77D16AC0(0, 0, 0, 100);
                        const b_7 = Line2D_$ctor_77D16AC0(0, -50, shift, 50);
                        let b_8;
                        if ((i_1 % 2) === 0) {
                            b_8 = b_7;
                        }
                        else {
                            const ln_3 = b_7;
                            b_8 = Line2D_$ctor_77D16AC0_1(ln_3.ToX, ln_3.ToY, ln_3.FromX, ln_3.FromY);
                        }
                        let a_5;
                        if ((i_1 % 3) === 0) {
                            a_5 = a_4;
                        }
                        else {
                            const ln_4 = a_4;
                            a_5 = Line2D_$ctor_77D16AC0_1(ln_4.ToX, ln_4.ToY, ln_4.FromX, ln_4.FromY);
                        }
                        let ang;
                        let b_9;
                        const ln_8 = b_8;
                        b_9 = Vc_$ctor_7B00E9A0_1((ln_9 = ln_8, ln_9.ToX - ln_9.FromX), (ln_10 = ln_8, ln_10.ToY - ln_10.FromY));
                        ang = (57.29577951308232 * ((a_9 = ((v_10 = ((ln_5 = a_5, Vc_$ctor_7B00E9A0_1((ln_6 = ln_5, ln_6.ToX - ln_6.FromX), (ln_7 = ln_5, ln_7.ToY - ln_7.FromY)))), (x = v_10.X, (y = v_10.Y, (l = Math.sqrt((x * x) + (y * y)), (!(l > 1E-12) ? failTooSmall("Vc.Unitized", v_10) : undefined, UnitVc_$ctor_7B00E9A0(x / l, y / l))))))), (b_12 = ((v_11 = b_9, (x_3 = v_11.X, (y_2 = v_11.Y, (l_1 = Math.sqrt((x_3 * x_3) + (y_2 * y_2)), (!(l_1 > 1E-12) ? failTooSmall("Vc.Unitized", v_11) : undefined, UnitVc_$ctor_7B00E9A0(x_3 / l_1, y_2 / l_1))))))), (dot = ((a_10 = a_9, (b_13 = b_12, (a_10.X * b_13.X) + (a_10.Y * b_13.Y)))), ((-0.98 < dot) && (dot < 0.98)) ? Math.acos(dot) : ((dot < 0) ? (3.141592653589793 - (2 * Math.asin(((x_6 = (b_12.X - -a_9.X), (y_4 = (b_12.Y - -a_9.Y), Math.sqrt((x_6 * x_6) + (y_4 * y_4))))) * 0.5))) : (2 * Math.asin(((x_7 = (b_12.X - a_9.X), (y_5 = (b_12.Y - a_9.Y), Math.sqrt((x_7 * x_7) + (y_5 * y_5))))) * 0.5))))))));
                        const d_4 = Euclid_Line2D__Line2D_distanceToLine_Static(a_5, b_8);
                        const ok = d_4 <= (shift * 0.55);
                        return append(!ok ? ((toConsole(`  *  angle ${ang}`), (toConsole(`  *  distance: ${d_4}`), (toConsole(`  *  shift: ${shift}`), (toConsole(`  *  line A from to: Line2D(${a_5.FromX}, ${a_5.FromY}, ${a_5.ToX}, ${a_5.ToY})`), (toConsole(`  *  line B from to: Line2D(${b_8.FromX}, ${b_8.FromY}, ${b_8.ToX}, ${b_8.ToY})`), (continueShift = false, empty()))))))) : empty(), delay(() => {
                            let builder$0040_4;
                            return singleton((builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F(toText(`distance of almost Parallel ${i_1},  angle ${ang} (= ${180 - ang})`), new FocusState(0, [])), Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
                                Expect_isTrue(ok)("distance is as expected in almost Parallel Lines");
                                Test_TestCaseBuilder__Zero(builder$0040_4);
                            }))));
                        }));
                    }
                    else {
                        return empty();
                    }
                }, rangeDouble(0, 1, 50));
            }));
        }));
    }));
})))));

export const testsLine2DBasics = Test_testList("Line2D Basics", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsZeroLength true for zero length line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let ln_1;
        const ln = Line2D_$ctor_77D16AC0(5, 3, 5, 3);
        Expect_isTrue((ln_1 = ln, (ln_1.ToX === ln_1.FromX) && (ln_1.ToY === ln_1.FromY)))("zero length line");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsZeroLength false for non-zero line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let ln_3;
        const ln_2 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_isFalse((ln_3 = ln_2, (ln_3.ToX === ln_3.FromX) && (ln_3.ToY === ln_3.FromY)))("non-zero length line");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTiny detects short lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let ln_6, x, ln_7, y, ln_8, ln_10, x_1, ln_11, y_1, ln_12;
        const ln_4 = Line2D_$ctor_77D16AC0(0, 0, 1E-10, 0);
        Expect_isTrue(!(((ln_6 = ln_4, (x = ((ln_7 = ln_6, ln_7.ToX - ln_7.FromX)), (y = ((ln_8 = ln_6, ln_8.ToY - ln_8.FromY)), Math.sqrt((x * x) + (y * y)))))) > 1E-09))("line shorter than tolerance");
        Expect_isFalse(!(((ln_10 = ln_4, (x_1 = ((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)), (y_1 = ((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)), Math.sqrt((x_1 * x_1) + (y_1 * y_1)))))) > 1E-11))("line not shorter than smaller tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsTinySq detects short lines with squared tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let ln_18, x_3, ln_19, y_3, ln_20, ln_22, x_4, ln_23, y_4, ln_24;
        const ln_13 = Line2D_$ctor_77D16AC0(0, 0, 1E-05, 0);
        let lenSq;
        const ln_14 = ln_13;
        let x_2;
        const ln_15 = ln_14;
        x_2 = (ln_15.ToX - ln_15.FromX);
        let y_2;
        const ln_16 = ln_14;
        y_2 = (ln_16.ToY - ln_16.FromY);
        lenSq = ((x_2 * x_2) + (y_2 * y_2));
        Expect_isTrue(!(((ln_18 = ln_13, (x_3 = ((ln_19 = ln_18, ln_19.ToX - ln_19.FromX)), (y_3 = ((ln_20 = ln_18, ln_20.ToY - ln_20.FromY)), (x_3 * x_3) + (y_3 * y_3))))) > (lenSq * 2)))("line shorter than squared tolerance");
        Expect_isFalse(!(((ln_22 = ln_13, (x_4 = ((ln_23 = ln_22, ln_23.ToX - ln_23.FromX)), (y_4 = ((ln_24 = ln_22, ln_24.ToY - ln_24.FromY)), (x_4 * x_4) + (y_4 * y_4))))) > (lenSq * 0.5)))("line not shorter than squared tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsXAligned horizontal line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let ln_26, x_5, ln_27, y_5, ln_28;
        const ln_25 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_isTrue((ln_26 = ln_25, (x_5 = Math.abs((ln_27 = ln_26, ln_27.ToX - ln_27.FromX)), (y_5 = Math.abs((ln_28 = ln_26, ln_28.ToY - ln_28.FromY)), (!((x_5 + y_5) > 1E-06) ? failTooSmall("Line2D.IsXAligned", ln_26) : undefined, y_5 < 1E-09)))))("horizontal line is X aligned");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsXAligned vertical line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let ln_30, x_7, ln_31, y_6, ln_32;
        const ln_29 = Line2D_$ctor_77D16AC0(0, 0, 0, 10);
        Expect_isFalse((ln_30 = ln_29, (x_7 = Math.abs((ln_31 = ln_30, ln_31.ToX - ln_31.FromX)), (y_6 = Math.abs((ln_32 = ln_30, ln_32.ToY - ln_32.FromY)), (!((x_7 + y_6) > 1E-06) ? failTooSmall("Line2D.IsXAligned", ln_30) : undefined, y_6 < 1E-09)))))("vertical line is not X aligned");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsXAligned nearly horizontal within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let ln_34, x_9, ln_35, y_7, ln_36;
        const ln_33 = Line2D_$ctor_77D16AC0(0, 0, 10, 1E-10);
        Expect_isTrue((ln_34 = ln_33, (x_9 = Math.abs((ln_35 = ln_34, ln_35.ToX - ln_35.FromX)), (y_7 = Math.abs((ln_36 = ln_34, ln_36.ToY - ln_36.FromY)), (!((x_9 + y_7) > 1E-06) ? failTooSmall("Line2D.IsXAligned", ln_34) : undefined, y_7 < 1E-09)))))("nearly horizontal within tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsXAligned zero length throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const ln_37 = Line2D_$ctor_77D16AC0(0, 0, 0, 0);
        Expect_throws(() => {
            let ln_38, x_11, ln_39, y_8, ln_40;
            (ln_38 = ln_37, (x_11 = Math.abs((ln_39 = ln_38, ln_39.ToX - ln_39.FromX)), (y_8 = Math.abs((ln_40 = ln_38, ln_40.ToY - ln_40.FromY)), (!((x_11 + y_8) > 1E-06) ? failTooSmall("Line2D.IsXAligned", ln_38) : undefined, y_8 < 1E-09))));
        }, "zero length throws");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsYAligned vertical line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let ln_42, x_13, ln_43, y_9, ln_44;
        const ln_41 = Line2D_$ctor_77D16AC0(0, 0, 0, 10);
        Expect_isTrue((ln_42 = ln_41, (x_13 = Math.abs((ln_43 = ln_42, ln_43.ToX - ln_43.FromX)), (y_9 = Math.abs((ln_44 = ln_42, ln_44.ToY - ln_44.FromY)), (!((x_13 + y_9) > 1E-06) ? failTooSmall("Line2D.IsYAligned", ln_42) : undefined, x_13 < 1E-09)))))("vertical line is Y aligned");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsYAligned horizontal line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let ln_46, x_15, ln_47, y_10, ln_48;
        const ln_45 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_isFalse((ln_46 = ln_45, (x_15 = Math.abs((ln_47 = ln_46, ln_47.ToX - ln_47.FromX)), (y_10 = Math.abs((ln_48 = ln_46, ln_48.ToY - ln_48.FromY)), (!((x_15 + y_10) > 1E-06) ? failTooSmall("Line2D.IsYAligned", ln_46) : undefined, x_15 < 1E-09)))))("horizontal line is not Y aligned");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsYAligned nearly vertical within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let ln_50, x_17, ln_51, y_11, ln_52;
        const ln_49 = Line2D_$ctor_77D16AC0(0, 0, 1E-10, 10);
        Expect_isTrue((ln_50 = ln_49, (x_17 = Math.abs((ln_51 = ln_50, ln_51.ToX - ln_51.FromX)), (y_11 = Math.abs((ln_52 = ln_50, ln_52.ToY - ln_52.FromY)), (!((x_17 + y_11) > 1E-06) ? failTooSmall("Line2D.IsYAligned", ln_50) : undefined, x_17 < 1E-09)))))("nearly vertical within tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Length calculation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let ln_54, x_19, ln_55, y_12, ln_56;
        const ln_53 = Line2D_$ctor_77D16AC0(0, 0, 3, 4);
        expectEqualEpsilon((ln_54 = ln_53, (x_19 = ((ln_55 = ln_54, ln_55.ToX - ln_55.FromX)), (y_12 = ((ln_56 = ln_54, ln_56.ToY - ln_56.FromY)), Math.sqrt((x_19 * x_19) + (y_12 * y_12))))), 5, "length is 5");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LengthSq calculation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let ln_58, x_20, ln_59, y_13, ln_60;
        const ln_57 = Line2D_$ctor_77D16AC0(0, 0, 3, 4);
        expectEqualEpsilon((ln_58 = ln_57, (x_20 = ((ln_59 = ln_58, ln_59.ToX - ln_59.FromX)), (y_13 = ((ln_60 = ln_58, ln_60.ToY - ln_60.FromY)), (x_20 * x_20) + (y_13 * y_13)))), 25, "length squared is 25");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Mid point calculation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const ln_61 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
        let mid;
        const ln_62 = ln_61;
        const x_21 = (ln_62.ToX + ln_62.FromX) * 0.5;
        const y_14 = (ln_62.ToY + ln_62.FromY) * 0.5;
        mid = Pt_$ctor_7B00E9A0_1(x_21, y_14);
        expectEqualEpsilon(mid.X, 5, "midpoint X is 5");
        expectEqualEpsilon(mid.Y, 5, "midpoint Y is 5");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Reversed line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_5, ln_65, b_5, ln_66, vx, vy, a_7, ln_67, b_7, ln_68, vx_1, vy_1;
        const ln_63 = Line2D_$ctor_77D16AC0(0, 0, 10, 5);
        let rev;
        const ln_64 = ln_63;
        rev = Line2D_$ctor_77D16AC0_1(ln_64.ToX, ln_64.ToY, ln_64.FromX, ln_64.FromY);
        Expect_isTrue(((a_5 = ((ln_65 = rev, Pt_$ctor_7B00E9A0(ln_65.FromX, ln_65.FromY))), (b_5 = ((ln_66 = ln_63, Pt_$ctor_7B00E9A0(ln_66.ToX, ln_66.ToY))), (vx = (a_5.X - b_5.X), (vy = (a_5.Y - b_5.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("reversed from equals original to");
        Expect_isTrue(((a_7 = ((ln_67 = rev, Pt_$ctor_7B00E9A0(ln_67.ToX, ln_67.ToY))), (b_7 = ((ln_68 = ln_63, Pt_$ctor_7B00E9A0(ln_68.FromX, ln_68.FromY))), (vx_1 = (a_7.X - b_7.X), (vy_1 = (a_7.Y - b_7.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("reversed to equals original from");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt parameter 0", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let ln_71, ln_72, a_9, b_9, ln_73, vx_2, vy_2;
        const ln_69 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let pt;
        const ln_70 = ln_69;
        pt = Pt_$ctor_7B00E9A0_1(ln_70.FromX + (((ln_71 = ln_70, ln_71.ToX - ln_71.FromX)) * 0), ln_70.FromY + (((ln_72 = ln_70, ln_72.ToY - ln_72.FromY)) * 0));
        Expect_isTrue(((a_9 = pt, (b_9 = ((ln_73 = ln_69, Pt_$ctor_7B00E9A0(ln_73.FromX, ln_73.FromY))), (vx_2 = (a_9.X - b_9.X), (vy_2 = (a_9.Y - b_9.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09)("parameter 0 equals start");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt parameter 1", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let ln_76, ln_77, a_11, b_11, ln_78, vx_3, vy_3;
        const ln_74 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let pt_1;
        const ln_75 = ln_74;
        pt_1 = Pt_$ctor_7B00E9A0_1(ln_75.FromX + (((ln_76 = ln_75, ln_76.ToX - ln_76.FromX)) * 1), ln_75.FromY + (((ln_77 = ln_75, ln_77.ToY - ln_77.FromY)) * 1));
        Expect_isTrue(((a_11 = pt_1, (b_11 = ((ln_78 = ln_74, Pt_$ctor_7B00E9A0(ln_78.ToX, ln_78.ToY))), (vx_3 = (a_11.X - b_11.X), (vy_3 = (a_11.Y - b_11.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09)("parameter 1 equals end");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt parameter 0.5", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let ln_81, ln_82, a_13, b_13, ln_83, x_22, y_15, vx_4, vy_4;
        const ln_79 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let pt_2;
        const ln_80 = ln_79;
        pt_2 = Pt_$ctor_7B00E9A0_1(ln_80.FromX + (((ln_81 = ln_80, ln_81.ToX - ln_81.FromX)) * 0.5), ln_80.FromY + (((ln_82 = ln_80, ln_82.ToY - ln_82.FromY)) * 0.5));
        Expect_isTrue(((a_13 = pt_2, (b_13 = ((ln_83 = ln_79, (x_22 = ((ln_83.ToX + ln_83.FromX) * 0.5), (y_15 = ((ln_83.ToY + ln_83.FromY) * 0.5), Pt_$ctor_7B00E9A0_1(x_22, y_15))))), (vx_4 = (a_13.X - b_13.X), (vy_4 = (a_13.Y - b_13.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) < 1E-09)("parameter 0.5 equals mid");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt parameter 2", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let ln_86, ln_87;
        const ln_84 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let pt_3;
        const ln_85 = ln_84;
        pt_3 = Pt_$ctor_7B00E9A0_1(ln_85.FromX + (((ln_86 = ln_85, ln_86.ToX - ln_86.FromX)) * 2), ln_85.FromY + (((ln_87 = ln_85, ln_87.ToY - ln_87.FromY)) * 2));
        expectEqualEpsilon(pt_3.X, 20, "parameter 2 extends beyond end");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SubLine creates segment", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const ln_88 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let sub;
        const ln_89 = ln_88;
        const fromX = ln_89.FromX;
        const fromY = ln_89.FromY;
        const x_23 = ln_89.ToX - fromX;
        const y_16 = ln_89.ToY - fromY;
        sub = Line2D_$ctor_77D16AC0_1(fromX + (x_23 * 0.25), fromY + (y_16 * 0.25), fromX + (x_23 * 0.75), fromY + (y_16 * 0.75));
        expectEqualEpsilon(sub.FromX, 2.5, "subline starts at 0.25");
        expectEqualEpsilon(sub.ToX, 7.5, "subline ends at 0.75");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LengthTillParam positive parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let ln_92, ln_93;
        const ln_90 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let len;
        const ln_91 = ln_90;
        const x_24 = ((ln_92 = ln_91, ln_92.ToX - ln_92.FromX)) * 0.5;
        const y_17 = ((ln_93 = ln_91, ln_93.ToY - ln_93.FromY)) * 0.5;
        const l = Math.sqrt((x_24 * x_24) + (y_17 * y_17));
        len = ((0.5 > 0) ? l : -l);
        expectEqualEpsilon(len, 5, "length till 0.5 is 5");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LengthTillParam negative parameter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let ln_96, ln_97;
        const ln_94 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let len_1;
        const ln_95 = ln_94;
        const x_25 = ((ln_96 = ln_95, ln_96.ToX - ln_96.FromX)) * -0.5;
        const y_18 = ((ln_97 = ln_95, ln_97.ToY - ln_97.FromY)) * -0.5;
        const l_1 = Math.sqrt((x_25 * x_25) + (y_18 * y_18));
        len_1 = ((-0.5 > 0) ? l_1 : -l_1);
        expectEqualEpsilon(len_1, -5, "length till -0.5 is -5");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LengthFromParam to end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let ln_100, ln_101;
        const ln_98 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let len_2;
        const ln_99 = ln_98;
        const p_6 = 1 - 0.5;
        const x_26 = ((ln_100 = ln_99, ln_100.ToX - ln_100.FromX)) * p_6;
        const y_19 = ((ln_101 = ln_99, ln_101.ToY - ln_101.FromY)) * p_6;
        const l_2 = Math.sqrt((x_26 * x_26) + (y_19 * y_19));
        len_2 = ((p_6 > 0) ? l_2 : -l_2);
        expectEqualEpsilon(len_2, 5, "length from 0.5 to end is 5");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LengthFromParam beyond end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let ln_104, ln_105;
        const ln_102 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let len_3;
        const ln_103 = ln_102;
        const p_7 = 1 - 1.5;
        const x_27 = ((ln_104 = ln_103, ln_104.ToX - ln_104.FromX)) * p_7;
        const y_20 = ((ln_105 = ln_103, ln_105.ToY - ln_105.FromY)) * p_7;
        const l_3 = Math.sqrt((x_27 * x_27) + (y_20 * y_20));
        len_3 = ((p_7 > 0) ? l_3 : -l_3);
        expectEqualEpsilon(len_3, -5, "length from 1.5 is negative");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})()]));

export const testsLine2DExtendShrink = Test_testList("Line2D Extend/Shrink", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Extend both ends", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ln = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let ext;
        const ln_1 = ln;
        let x;
        const ln_2 = ln_1;
        x = (ln_2.ToX - ln_2.FromX);
        let y;
        const ln_3 = ln_1;
        y = (ln_3.ToY - ln_3.FromY);
        const l = Math.sqrt((x * x) + (y * y));
        if (!(l > 1E-12)) {
            failTooSmall("Line2D.Extend", ln_1);
        }
        ext = Line2D_$ctor_77D16AC0_1(ln_1.FromX - ((x * 5) / l), ln_1.FromY - ((y * 5) / l), ln_1.ToX + ((x * 5) / l), ln_1.ToY + ((y * 5) / l));
        expectEqualEpsilon(ext.FromX, -5, "extended start");
        expectEqualEpsilon(ext.ToX, 15, "extended end");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Extend too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ln_4 = Line2D_$ctor_77D16AC0(0, 0, 1E-13, 0);
        Expect_throws(() => {
            let ln_5, x_2, ln_6, y_1, ln_7, l_1;
            (ln_5 = ln_4, (x_2 = ((ln_6 = ln_5, ln_6.ToX - ln_6.FromX)), (y_1 = ((ln_7 = ln_5, ln_7.ToY - ln_7.FromY)), (l_1 = Math.sqrt((x_2 * x_2) + (y_1 * y_1)), (!(l_1 > 1E-12) ? failTooSmall("Line2D.Extend", ln_5) : undefined, Line2D_$ctor_77D16AC0_1(ln_5.FromX - ((x_2 * 1) / l_1), ln_5.FromY - ((y_1 * 1) / l_1), ln_5.ToX + ((x_2 * 1) / l_1), ln_5.ToY + ((y_1 * 1) / l_1)))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExtendStart", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const ln_8 = Line2D_$ctor_77D16AC0(5, 0, 10, 0);
        let ext_1;
        const ln_9 = ln_8;
        let x_4;
        const ln_10 = ln_9;
        x_4 = (ln_10.ToX - ln_10.FromX);
        let y_2;
        const ln_11 = ln_9;
        y_2 = (ln_11.ToY - ln_11.FromY);
        const l_2 = Math.sqrt((x_4 * x_4) + (y_2 * y_2));
        if (!(l_2 > 1E-12)) {
            failTooSmall("Line2D.ExtendStart", ln_9);
        }
        ext_1 = Line2D_$ctor_77D16AC0_1(ln_9.FromX - ((x_4 * 5) / l_2), ln_9.FromY - ((y_2 * 5) / l_2), ln_9.ToX, ln_9.ToY);
        expectEqualEpsilon(ext_1.FromX, 0, "extended start");
        expectEqualEpsilon(ext_1.ToX, 10, "end unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExtendEnd", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ln_12 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let ext_2;
        const ln_13 = ln_12;
        let x_6;
        const ln_14 = ln_13;
        x_6 = (ln_14.ToX - ln_14.FromX);
        let y_3;
        const ln_15 = ln_13;
        y_3 = (ln_15.ToY - ln_15.FromY);
        const l_3 = Math.sqrt((x_6 * x_6) + (y_3 * y_3));
        if (!(l_3 > 1E-12)) {
            failTooSmall("Line2D.ExtendEnd", ln_13);
        }
        ext_2 = Line2D_$ctor_77D16AC0_1(ln_13.FromX, ln_13.FromY, ln_13.ToX + ((x_6 * 5) / l_3), ln_13.ToY + ((y_3 * 5) / l_3));
        expectEqualEpsilon(ext_2.FromX, 0, "start unchanged");
        expectEqualEpsilon(ext_2.ToX, 15, "extended end");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExtendRel both ends", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ln_16 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let ext_3;
        const ln_17 = ln_16;
        let x_8;
        const ln_18 = ln_17;
        x_8 = (ln_18.ToX - ln_18.FromX);
        let y_4;
        const ln_19 = ln_17;
        y_4 = (ln_19.ToY - ln_19.FromY);
        const l_4 = Math.sqrt((x_8 * x_8) + (y_4 * y_4));
        if (!(l_4 > 1E-12)) {
            failTooSmall("Line2D.ExtendRel", ln_17);
        }
        ext_3 = Line2D_$ctor_77D16AC0_1(ln_17.FromX - (x_8 * 0.5), ln_17.FromY - (y_4 * 0.5), ln_17.ToX + (x_8 * 0.5), ln_17.ToY + (y_4 * 0.5));
        expectEqualEpsilon(ext_3.FromX, -5, "extended start by half length");
        expectEqualEpsilon(ext_3.ToX, 15, "extended end by half length");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExtendStartRel", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const ln_20 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let ext_4;
        const ln_21 = ln_20;
        let x_10;
        const ln_22 = ln_21;
        x_10 = (ln_22.ToX - ln_22.FromX);
        let y_5;
        const ln_23 = ln_21;
        y_5 = (ln_23.ToY - ln_23.FromY);
        const l_5 = Math.sqrt((x_10 * x_10) + (y_5 * y_5));
        if (!(l_5 > 1E-12)) {
            failTooSmall("Line2D.ExtendStartRel", ln_21);
        }
        ext_4 = Line2D_$ctor_77D16AC0_1(ln_21.FromX - (x_10 * 0.5), ln_21.FromY - (y_5 * 0.5), ln_21.ToX, ln_21.ToY);
        expectEqualEpsilon(ext_4.FromX, -5, "extended start by half length");
        expectEqualEpsilon(ext_4.ToX, 10, "end unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExtendEndRel", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const ln_24 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let ext_5;
        const ln_25 = ln_24;
        let x_12;
        const ln_26 = ln_25;
        x_12 = (ln_26.ToX - ln_26.FromX);
        let y_6;
        const ln_27 = ln_25;
        y_6 = (ln_27.ToY - ln_27.FromY);
        const l_6 = Math.sqrt((x_12 * x_12) + (y_6 * y_6));
        if (!(l_6 > 1E-12)) {
            failTooSmall("Line2D.ExtendEndRel", ln_25);
        }
        ext_5 = Line2D_$ctor_77D16AC0_1(ln_25.FromX, ln_25.FromY, ln_25.ToX + (x_12 * 0.5), ln_25.ToY + (y_6 * 0.5));
        expectEqualEpsilon(ext_5.FromX, 0, "start unchanged");
        expectEqualEpsilon(ext_5.ToX, 15, "extended end by half length");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Shrink both ends", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const ln_28 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let shr;
        const ln_29 = ln_28;
        let x_14;
        const ln_30 = ln_29;
        x_14 = (ln_30.ToX - ln_30.FromX);
        let y_7;
        const ln_31 = ln_29;
        y_7 = (ln_31.ToY - ln_31.FromY);
        const l_7 = Math.sqrt((x_14 * x_14) + (y_7 * y_7));
        if (!(l_7 > 1E-12)) {
            failTooSmall("Line2D.Shrink", ln_29);
        }
        shr = Line2D_$ctor_77D16AC0_1(ln_29.FromX + ((x_14 * 2) / l_7), ln_29.FromY + ((y_7 * 2) / l_7), ln_29.ToX - ((x_14 * 3) / l_7), ln_29.ToY - ((y_7 * 3) / l_7));
        expectEqualEpsilon(shr.FromX, 2, "shrunk start");
        expectEqualEpsilon(shr.ToX, 7, "shrunk end");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Shrink too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const ln_32 = Line2D_$ctor_77D16AC0(0, 0, 1E-13, 0);
        Expect_throws(() => {
            let ln_33, x_16, ln_34, y_8, ln_35, l_8;
            (ln_33 = ln_32, (x_16 = ((ln_34 = ln_33, ln_34.ToX - ln_34.FromX)), (y_8 = ((ln_35 = ln_33, ln_35.ToY - ln_35.FromY)), (l_8 = Math.sqrt((x_16 * x_16) + (y_8 * y_8)), (!(l_8 > 1E-12) ? failTooSmall("Line2D.Shrink", ln_33) : undefined, Line2D_$ctor_77D16AC0_1(ln_33.FromX + ((x_16 * 1) / l_8), ln_33.FromY + ((y_8 * 1) / l_8), ln_33.ToX - ((x_16 * 1) / l_8), ln_33.ToY - ((y_8 * 1) / l_8)))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ShrinkStart", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const ln_36 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let shr_1;
        const ln_37 = ln_36;
        let x_18;
        const ln_38 = ln_37;
        x_18 = (ln_38.ToX - ln_38.FromX);
        let y_9;
        const ln_39 = ln_37;
        y_9 = (ln_39.ToY - ln_39.FromY);
        const l_9 = Math.sqrt((x_18 * x_18) + (y_9 * y_9));
        if (!(l_9 > 1E-12)) {
            failTooSmall("Line2D.ShrinkStart", ln_37);
        }
        shr_1 = Line2D_$ctor_77D16AC0_1(ln_37.FromX + ((x_18 * 2) / l_9), ln_37.FromY + ((y_9 * 2) / l_9), ln_37.ToX, ln_37.ToY);
        expectEqualEpsilon(shr_1.FromX, 2, "shrunk start");
        expectEqualEpsilon(shr_1.ToX, 10, "end unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ShrinkEnd", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const ln_40 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let shr_2;
        const ln_41 = ln_40;
        let x_20;
        const ln_42 = ln_41;
        x_20 = (ln_42.ToX - ln_42.FromX);
        let y_10;
        const ln_43 = ln_41;
        y_10 = (ln_43.ToY - ln_43.FromY);
        const l_10 = Math.sqrt((x_20 * x_20) + (y_10 * y_10));
        if (!(l_10 > 1E-12)) {
            failTooSmall("Line2D.ShrinkEnd", ln_41);
        }
        shr_2 = Line2D_$ctor_77D16AC0_1(ln_41.FromX, ln_41.FromY, ln_41.ToX - ((x_20 * 3) / l_10), ln_41.ToY - ((y_10 * 3) / l_10));
        expectEqualEpsilon(shr_2.FromX, 0, "start unchanged");
        expectEqualEpsilon(shr_2.ToX, 7, "shrunk end");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})()]));

export const testsLine2DMove = Test_testList("Line2D Move/Transform", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Move by vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ln = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let moved;
        const ln_1 = ln;
        const v = Vc_$ctor_7B00E9A0_2(5, 3);
        moved = Line2D_$ctor_77D16AC0_1(ln_1.FromX + v.X, ln_1.FromY + v.Y, ln_1.ToX + v.X, ln_1.ToY + v.Y);
        expectEqualEpsilon(moved.FromX, 5, "from moved correctly");
        expectEqualEpsilon(moved.FromY, 3, "from moved correctly Y");
        expectEqualEpsilon(moved.ToX, 15, "to moved correctly");
        expectEqualEpsilon(moved.ToY, 3, "to moved correctly Y");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ln_2 = Line2D_$ctor_77D16AC0(0, 0, 10, 5);
        let moved_1;
        const ln_3 = ln_2;
        moved_1 = Line2D_$ctor_77D16AC0_1(ln_3.FromX + 7, ln_3.FromY, ln_3.ToX + 7, ln_3.ToY);
        expectEqualEpsilon(moved_1.FromX, 7, "from X moved");
        expectEqualEpsilon(moved_1.FromY, 0, "from Y unchanged");
        expectEqualEpsilon(moved_1.ToX, 17, "to X moved");
        expectEqualEpsilon(moved_1.ToY, 5, "to Y unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const ln_4 = Line2D_$ctor_77D16AC0(0, 0, 10, 5);
        let moved_2;
        const ln_5 = ln_4;
        moved_2 = Line2D_$ctor_77D16AC0_1(ln_5.FromX, ln_5.FromY + 3, ln_5.ToX, ln_5.ToY + 3);
        expectEqualEpsilon(moved_2.FromX, 0, "from X unchanged");
        expectEqualEpsilon(moved_2.FromY, 3, "from Y moved");
        expectEqualEpsilon(moved_2.ToX, 10, "to X unchanged");
        expectEqualEpsilon(moved_2.ToY, 8, "to Y moved");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Scale from origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ln_6 = Line2D_$ctor_77D16AC0(1, 2, 3, 4);
        let scaled;
        const l = ln_6;
        scaled = Line2D_$ctor_77D16AC0_1(l.FromX * 2, l.FromY * 2, l.ToX * 2, l.ToY * 2);
        expectEqualEpsilon(scaled.FromX, 2, "from X scaled");
        expectEqualEpsilon(scaled.FromY, 4, "from Y scaled");
        expectEqualEpsilon(scaled.ToX, 6, "to X scaled");
        expectEqualEpsilon(scaled.ToY, 8, "to Y scaled");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ScaleOn center point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ln_7 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const cen = Pt_$ctor_7B00E9A0_2(5, 0);
        let scaled_1;
        const l_1 = ln_7;
        const cen_2 = cen;
        const cx = cen_2.X;
        const cy = cen_2.Y;
        scaled_1 = Line2D_$ctor_77D16AC0_1(cx + ((l_1.FromX - cx) * 2), cy + ((l_1.FromY - cy) * 2), cx + ((l_1.ToX - cx) * 2), cy + ((l_1.ToY - cy) * 2));
        expectEqualEpsilon(scaled_1.FromX, -5, "from X scaled from center");
        expectEqualEpsilon(scaled_1.ToX, 15, "to X scaled from center");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate 90 degrees", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const ln_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let rot;
        const rad = 0.017453292519943295 * 90;
        rot = Rotation2D_$ctor_7B00E9A0(Math.sin(rad), Math.cos(rad));
        let rotated;
        const ln_9 = ln_8;
        const r = rot;
        const fx = ln_9.FromX;
        const fy = ln_9.FromY;
        const tx = ln_9.ToX;
        const ty = ln_9.ToY;
        const c = r.Cos;
        const s = r.Sin;
        rotated = Line2D_$ctor_77D16AC0_1((c * fx) - (s * fy), (s * fx) + (c * fy), (c * tx) - (s * ty), (s * tx) + (c * ty));
        expectEqualEpsilon(rotated.FromX, 0, "from unchanged at origin");
        expectEqualEpsilon(rotated.ToX, 0, "rotated to X");
        expectEqualEpsilon(rotated.ToY, 10, "rotated to Y");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RotateWithCenter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const ln_10 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const cen_3 = Pt_$ctor_7B00E9A0_2(5, 0);
        let rot_1;
        const rad_1 = 0.017453292519943295 * 90;
        rot_1 = Rotation2D_$ctor_7B00E9A0(Math.sin(rad_1), Math.cos(rad_1));
        let rotated_1;
        const ln_11 = ln_10;
        const cen_4 = cen_3;
        const r_1 = rot_1;
        const cx_1 = cen_4.X;
        const cy_1 = cen_4.Y;
        const fx_1 = ln_11.FromX - cx_1;
        const fy_1 = ln_11.FromY - cy_1;
        const tx_1 = ln_11.ToX - cx_1;
        const ty_1 = ln_11.ToY - cy_1;
        const c_1 = r_1.Cos;
        const s_1 = r_1.Sin;
        rotated_1 = Line2D_$ctor_77D16AC0_1(((c_1 * fx_1) - (s_1 * fy_1)) + cx_1, ((s_1 * fx_1) + (c_1 * fy_1)) + cy_1, ((c_1 * tx_1) - (s_1 * ty_1)) + cx_1, ((s_1 * tx_1) + (c_1 * ty_1)) + cy_1);
        expectEqualEpsilon(rotated_1.FromX, 5, "rotated around center from X");
        expectEqualEpsilon(rotated_1.FromY, -5, "rotated around center from Y");
        expectEqualEpsilon(rotated_1.ToX, 5, "rotated around center to X");
        expectEqualEpsilon(rotated_1.ToY, 5, "rotated around center to Y");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})()]));

export const testsLine2DClosestPoint = Test_testList("Line2D Closest Point", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RayClosestParameter on ray", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ln = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt = Pt_$ctor_7B00E9A0_2(5, 5);
        const param = Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD(ln, pt);
        expectEqualEpsilon(param, 0.5, "closest parameter");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RayClosestParameter before start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ln_1 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt_1 = Pt_$ctor_7B00E9A0_2(-5, 5);
        const param_1 = Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD(ln_1, pt_1);
        Expect_isTrue(param_1 < 0)("parameter is negative");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RayClosestParameter beyond end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const ln_2 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt_2 = Pt_$ctor_7B00E9A0_2(15, 5);
        const param_2 = Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD(ln_2, pt_2);
        Expect_isTrue(param_2 > 1)("parameter is beyond 1");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RayClosestParameter too short throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ln_3 = Line2D_$ctor_77D16AC0(0, 0, 1E-10, 0);
        const pt_3 = Pt_$ctor_7B00E9A0_2(5, 5);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_RayClosestParameter_6ADE94FD(ln_3, pt_3);
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestParameter on line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ln_4 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt_4 = Pt_$ctor_7B00E9A0_2(5, 5);
        let param_3;
        const ln_5 = ln_4;
        const p = pt_4;
        const x = ln_5.FromX - ln_5.ToX;
        const y = ln_5.FromY - ln_5.ToY;
        const u = ln_5.FromX - p.X;
        const v = ln_5.FromY - p.Y;
        const dot = (x * u) + (y * v);
        const lenSq = (x * x) + (y * y);
        if (!(lenSq > 1E-12)) {
            param_3 = ((dot < 0) ? 0 : 1);
        }
        else {
            const x_2 = dot / lenSq;
            param_3 = ((x_2 > 0) ? ((x_2 < 1) ? x_2 : 1) : 0);
        }
        expectEqualEpsilon(param_3, 0.5, "closest parameter");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestParameter before start clamped", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const ln_6 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt_5 = Pt_$ctor_7B00E9A0_2(-5, 5);
        let param_4;
        const ln_7 = ln_6;
        const p_1 = pt_5;
        const x_3 = ln_7.FromX - ln_7.ToX;
        const y_1 = ln_7.FromY - ln_7.ToY;
        const u_1 = ln_7.FromX - p_1.X;
        const v_1 = ln_7.FromY - p_1.Y;
        const dot_1 = (x_3 * u_1) + (y_1 * v_1);
        const lenSq_1 = (x_3 * x_3) + (y_1 * y_1);
        if (!(lenSq_1 > 1E-12)) {
            param_4 = ((dot_1 < 0) ? 0 : 1);
        }
        else {
            const x_5 = dot_1 / lenSq_1;
            param_4 = ((x_5 > 0) ? ((x_5 < 1) ? x_5 : 1) : 0);
        }
        expectEqualEpsilon(param_4, 0, "clamped to 0");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestParameter beyond end clamped", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const ln_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt_6 = Pt_$ctor_7B00E9A0_2(15, 5);
        let param_5;
        const ln_9 = ln_8;
        const p_2 = pt_6;
        const x_6 = ln_9.FromX - ln_9.ToX;
        const y_2 = ln_9.FromY - ln_9.ToY;
        const u_2 = ln_9.FromX - p_2.X;
        const v_2 = ln_9.FromY - p_2.Y;
        const dot_2 = (x_6 * u_2) + (y_2 * v_2);
        const lenSq_2 = (x_6 * x_6) + (y_2 * y_2);
        if (!(lenSq_2 > 1E-12)) {
            param_5 = ((dot_2 < 0) ? 0 : 1);
        }
        else {
            const x_8 = dot_2 / lenSq_2;
            param_5 = ((x_8 > 0) ? ((x_8 < 1) ? x_8 : 1) : 0);
        }
        expectEqualEpsilon(param_5, 1, "clamped to 1");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestPoint on line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const ln_10 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt_7 = Pt_$ctor_7B00E9A0_2(5, 5);
        const cl = Euclid_Line2D__Line2D_ClosestPoint_6ADE94FD(ln_10, pt_7);
        expectEqualEpsilon(cl.X, 5, "closest point X");
        expectEqualEpsilon(cl.Y, 0, "closest point Y");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestPoint before start clamped", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_3, b_7, ln_12, vx, vy;
        const ln_11 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt_8 = Pt_$ctor_7B00E9A0_2(-5, 5);
        const cl_1 = Euclid_Line2D__Line2D_ClosestPoint_6ADE94FD(ln_11, pt_8);
        Expect_isTrue(((a_3 = cl_1, (b_7 = ((ln_12 = ln_11, Pt_$ctor_7B00E9A0(ln_12.FromX, ln_12.FromY))), (vx = (a_3.X - b_7.X), (vy = (a_3.Y - b_7.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("clamped to start");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SqDistanceFromPoint", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const ln_13 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt_9 = Pt_$ctor_7B00E9A0_2(5, 3);
        const sqDist = Euclid_Line2D__Line2D_SqDistanceFromPoint_6ADE94FD(ln_13, pt_9);
        expectEqualEpsilon(sqDist, 9, "squared distance is 9");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("DistanceToPt", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const ln_14 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pt_10 = Pt_$ctor_7B00E9A0_2(5, 4);
        let dist;
        const p_3 = pt_10;
        let value_1;
        const ln_16 = ln_14;
        const pAx = ln_16.FromX;
        const pAy = ln_16.FromY;
        let vAx;
        const ln_17 = ln_16;
        vAx = (ln_17.ToX - ln_17.FromX);
        let vAy;
        const ln_18 = ln_16;
        vAy = (ln_18.ToY - ln_18.FromY);
        const x_10 = p_3.X;
        const y_4 = p_3.Y;
        let t;
        const vAx_1 = vAx;
        const vAy_1 = vAy;
        const u_3 = x_10 - pAx;
        const v_3 = y_4 - pAy;
        const dotV = (vAx_1 * u_3) + (vAy_1 * v_3);
        const lenSq_3 = (vAx_1 * vAx_1) + (vAy_1 * vAy_1);
        t = (dotV / lenSq_3);
        if (t > -1E-06) {
            if (t < 1.000001) {
                const clPtX = pAx + (vAx * t);
                const clPtY = pAy + (vAy * t);
                const vx_1 = clPtX - x_10;
                const vy_1 = clPtY - y_4;
                value_1 = ((vx_1 * vx_1) + (vy_1 * vy_1));
            }
            else {
                const clPtX_1 = pAx + vAx;
                const clPtY_1 = pAy + vAy;
                const vx_1_1 = clPtX_1 - x_10;
                const vy_1_1 = clPtY_1 - y_4;
                value_1 = ((vx_1_1 * vx_1_1) + (vy_1_1 * vy_1_1));
            }
        }
        else {
            const vX = pAx - x_10;
            const vY = pAy - y_4;
            value_1 = ((vX * vX) + (vY * vY));
        }
        dist = Math.sqrt(value_1);
        expectEqualEpsilon(dist, 4, "distance is 4");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})()]));

export const testsLine2DProjection = Test_testList("Line2D Projection Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ray = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln = Line2D_$ctor_77D16AC0(2, 5, 8, 5);
        const patternInput = Euclid_Line2D__Line2D_projectOntoRayParam_Static(ray, ln);
        const p2 = patternInput[1];
        const p1 = patternInput[0];
        expectEqualEpsilon(p1, 0.2, "start parameter");
        expectEqualEpsilon(p2, 0.8, "end parameter");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam opposite direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ray_1 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_1 = Line2D_$ctor_77D16AC0(8, 5, 2, 5);
        const patternInput_1 = Euclid_Line2D__Line2D_projectOntoRayParam_Static(ray_1, ln_1);
        const p2_1 = patternInput_1[1];
        const p1_1 = patternInput_1[0];
        expectEqualEpsilon(p1_1, 0.8, "start parameter from");
        expectEqualEpsilon(p2_1, 0.2, "end parameter from");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam beyond ray start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const ray_2 = Line2D_$ctor_77D16AC0(5, 0, 10, 0);
        const ln_2 = Line2D_$ctor_77D16AC0(0, 5, 3, 5);
        const patternInput_2 = Euclid_Line2D__Line2D_projectOntoRayParam_Static(ray_2, ln_2);
        const p2_2 = patternInput_2[1];
        const p1_2 = patternInput_2[0];
        Expect_isTrue(p1_2 < 0)("parameters can be negative");
        Expect_isTrue(p2_2 < 0)("second param less than zero");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam beyond ray end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ray_3 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_3 = Line2D_$ctor_77D16AC0(12, 5, 18, 5);
        const patternInput_3 = Euclid_Line2D__Line2D_projectOntoRayParam_Static(ray_3, ln_3);
        const p2_3 = patternInput_3[1];
        const p1_3 = patternInput_3[0];
        Expect_isTrue(p1_3 > 1)("parameters beyond 1.0");
        Expect_isTrue(p2_3 > 1)("second param beyond 1.0");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam diagonal lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ray_4 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
        const ln_4 = Line2D_$ctor_77D16AC0(5, 0, 10, 5);
        const patternInput_4 = Euclid_Line2D__Line2D_projectOntoRayParam_Static(ray_4, ln_4);
        const p2_4 = patternInput_4[1];
        const p1_4 = patternInput_4[0];
        Expect_isTrue(Math.abs(p1_4 - 0.25) < 1E-09)("start parameter calculated via projection");
        Expect_isTrue(Math.abs(p2_4 - 0.75) < 1E-09)("end parameter calculated via projection");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam perpendicular projection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const ray_5 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_5 = Line2D_$ctor_77D16AC0(5, 0, 5, 10);
        const patternInput_5 = Euclid_Line2D__Line2D_projectOntoRayParam_Static(ray_5, ln_5);
        const p2_5 = patternInput_5[1];
        const p1_5 = patternInput_5[0];
        expectEqualEpsilon(p1_5, p2_5, "perpendicular collapses to point");
        expectEqualEpsilon(p1_5, 0.5, "projection at 0.5");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam too short ray throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const ray_6 = Line2D_$ctor_77D16AC0(0, 0, 1E-13, 0);
        const ln_6 = Line2D_$ctor_77D16AC0(2, 5, 8, 5);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_projectOntoRayParam_Static(ray_6, ln_6);
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRay same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const ray_7 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_7 = Line2D_$ctor_77D16AC0(2, 5, 8, 5);
        const proj = Euclid_Line2D__Line2D_projectOntoRay_Static(ray_7, ln_7);
        expectEqualEpsilon(proj.FromX, 2, "projection from X");
        expectEqualEpsilon(proj.ToX, 8, "projection to X");
        expectEqualEpsilon(proj.FromY, 0, "projection on ray");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRay opposite direction preserves orientation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const ray_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_8 = Line2D_$ctor_77D16AC0(8, 5, 2, 5);
        const proj_1 = Euclid_Line2D__Line2D_projectOntoRay_Static(ray_8, ln_8);
        expectEqualEpsilon(proj_1.FromX, 8, "preserves line direction from");
        expectEqualEpsilon(proj_1.ToX, 2, "preserves line direction to");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRay diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let ln_10, x, ln_11, y, ln_12;
        const ray_9 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
        const ln_9 = Line2D_$ctor_77D16AC0(0, 5, 5, 10);
        const proj_2 = Euclid_Line2D__Line2D_projectOntoRay_Static(ray_9, ln_9);
        Expect_isTrue(((ln_10 = proj_2, (x = ((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)), (y = ((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)), Math.sqrt((x * x) + (y * y)))))) > 0)("diagonal projection length preserves order");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam full overlap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const onToLine = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_13 = Line2D_$ctor_77D16AC0(2, 5, 8, 5);
        const matchValue = Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine, ln_13);
        if (matchValue == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
        else {
            const p2_6 = matchValue[1];
            const p1_6 = matchValue[0];
            expectEqualEpsilon(p1_6, 0.2, "start in range");
            expectEqualEpsilon(p2_6, 0.8, "end in range");
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam partial overlap at start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const onToLine_2 = Line2D_$ctor_77D16AC0(5, 0, 10, 0);
        const ln_14 = Line2D_$ctor_77D16AC0(0, 5, 7, 5);
        const matchValue_1 = Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine_2, ln_14);
        if (matchValue_1 == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
        else {
            const p2_7 = matchValue_1[1];
            const p1_7 = matchValue_1[0];
            expectEqualEpsilon(p1_7, 0, "clamped start");
            expectEqualEpsilon(p2_7, 0.4, "end parameter");
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam partial overlap at end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const onToLine_4 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_15 = Line2D_$ctor_77D16AC0(8, 5, 15, 5);
        const matchValue_2 = Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine_4, ln_15);
        if (matchValue_2 == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
        else {
            const p2_8 = matchValue_2[1];
            const p1_8 = matchValue_2[0];
            expectEqualEpsilon(p1_8, 0.8, "start parameter");
            expectEqualEpsilon(p2_8, 1, "clamped end");
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam no overlap before line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const onToLine_6 = Line2D_$ctor_77D16AC0(10, 0, 20, 0);
        const ln_16 = Line2D_$ctor_77D16AC0(0, 5, 5, 5);
        const matchValue_3 = Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine_6, ln_16);
        if (matchValue_3 == null) {
            Expect_isTrue(true)("no overlap returns None");
            Test_TestCaseBuilder__Zero(builder$0040_13);
        }
        else {
            throw new Exception("expected None");
            Test_TestCaseBuilder__Zero(builder$0040_13);
        }
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam no overlap after line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const onToLine_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_17 = Line2D_$ctor_77D16AC0(15, 5, 20, 5);
        const matchValue_4 = Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine_8, ln_17);
        if (matchValue_4 == null) {
            Expect_isTrue(true)("no overlap returns None");
            Test_TestCaseBuilder__Zero(builder$0040_14);
        }
        else {
            throw new Exception("expected None");
            Test_TestCaseBuilder__Zero(builder$0040_14);
        }
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam opposite direction overlap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const onToLine_10 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_18 = Line2D_$ctor_77D16AC0(8, 5, 2, 5);
        const matchValue_5 = Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine_10, ln_18);
        if (matchValue_5 == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
        else {
            const p2_9 = matchValue_5[1];
            const p1_9 = matchValue_5[0];
            expectEqualEpsilon(p1_9, 0.8, "from param");
            expectEqualEpsilon(p2_9, 0.2, "to param");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam encompassing line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const onToLine_12 = Line2D_$ctor_77D16AC0(5, 0, 7, 0);
        const ln_19 = Line2D_$ctor_77D16AC0(0, 5, 20, 5);
        const matchValue_6 = Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine_12, ln_19);
        if (matchValue_6 == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
        else {
            const p2_10 = matchValue_6[1];
            const p1_10 = matchValue_6[0];
            expectEqualEpsilon(p1_10, 0, "clamped to 0");
            expectEqualEpsilon(p2_10, 1, "clamped to 1");
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam zero length line to project", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const onToLine_14 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_20 = Line2D_$ctor_77D16AC0(5, 5, 5, 5);
        const matchValue_7 = Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine_14, ln_20);
        if (matchValue_7 == null) {
            throw new Exception("expected Some for zero length");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
        else {
            const p2_11 = matchValue_7[1];
            const p1_11 = matchValue_7[0];
            expectEqualEpsilon(p1_11, p2_11, "zero length projects to point");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam too short target line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const onToLine_16 = Line2D_$ctor_77D16AC0(0, 0, 1E-13, 0);
        const ln_21 = Line2D_$ctor_77D16AC0(2, 5, 8, 5);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_tryProjectOntoLineParam_Static(onToLine_16, ln_21);
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine full overlap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const onToLine_18 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_22 = Line2D_$ctor_77D16AC0(2, 5, 8, 5);
        const matchValue_8 = Euclid_Line2D__Line2D_tryProjectOntoLine_Static(onToLine_18, ln_22);
        if (matchValue_8 == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_19);
        }
        else {
            const proj_3 = matchValue_8;
            expectEqualEpsilon(proj_3.FromX, 2, "projection from");
            expectEqualEpsilon(proj_3.ToX, 8, "projection to");
            Test_TestCaseBuilder__Zero(builder$0040_19);
        }
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine partial overlap preserves orientation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const onToLine_20 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_23 = Line2D_$ctor_77D16AC0(8, 5, 15, 5);
        const matchValue_9 = Euclid_Line2D__Line2D_tryProjectOntoLine_Static(onToLine_20, ln_23);
        if (matchValue_9 == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_20);
        }
        else {
            const proj_4 = matchValue_9;
            expectEqualEpsilon(proj_4.FromX, 8, "from X");
            expectEqualEpsilon(proj_4.ToX, 10, "to X at boundary");
            Test_TestCaseBuilder__Zero(builder$0040_20);
        }
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine no overlap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const onToLine_22 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_24 = Line2D_$ctor_77D16AC0(15, 5, 20, 5);
        const matchValue_10 = Euclid_Line2D__Line2D_tryProjectOntoLine_Static(onToLine_22, ln_24);
        if (matchValue_10 == null) {
            Expect_isTrue(true)("no overlap");
            Test_TestCaseBuilder__Zero(builder$0040_21);
        }
        else {
            throw new Exception("expected None");
            Test_TestCaseBuilder__Zero(builder$0040_21);
        }
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let ln_26, x_1, ln_27, y_1, ln_28;
        const onToLine_24 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
        const ln_25 = Line2D_$ctor_77D16AC0(0, 5, 5, 10);
        const matchValue_11 = Euclid_Line2D__Line2D_tryProjectOntoLine_Static(onToLine_24, ln_25);
        if (matchValue_11 == null) {
            throw new Exception("expected Some for diagonal");
            Test_TestCaseBuilder__Zero(builder$0040_22);
        }
        else {
            const proj_5 = matchValue_11;
            Expect_isTrue(((ln_26 = proj_5, (x_1 = ((ln_27 = ln_26, ln_27.ToX - ln_27.FromX)), (y_1 = ((ln_28 = ln_26, ln_28.ToY - ln_28.FromY)), Math.sqrt((x_1 * x_1) + (y_1 * y_1)))))) > 0)("projection exists");
            Test_TestCaseBuilder__Zero(builder$0040_22);
        }
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine perpendicular collapse", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let ln_30, x_2, ln_31, y_2, ln_32;
        const onToLine_26 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_29 = Line2D_$ctor_77D16AC0(5, 2, 5, 8);
        const matchValue_12 = Euclid_Line2D__Line2D_tryProjectOntoLine_Static(onToLine_26, ln_29);
        if (matchValue_12 == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_23);
        }
        else {
            const proj_6 = matchValue_12;
            Expect_isTrue(((ln_30 = proj_6, (x_2 = ((ln_31 = ln_30, ln_31.ToX - ln_31.FromX)), (y_2 = ((ln_32 = ln_30, ln_32.ToY - ln_32.FromY)), Math.sqrt((x_2 * x_2) + (y_2 * y_2)))))) < 1E-09)("collapses to zero length");
            expectEqualEpsilon(proj_6.FromX, 5, "at parameter 0.5");
            Test_TestCaseBuilder__Zero(builder$0040_23);
        }
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine within tolerance at start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        const onToLine_28 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ln_33 = Line2D_$ctor_77D16AC0(-1E-07, 5, 5, 5);
        const matchValue_13 = Euclid_Line2D__Line2D_tryProjectOntoLine_Static(onToLine_28, ln_33);
        if (matchValue_13 == null) {
            throw new Exception("expected Some within tolerance");
            Test_TestCaseBuilder__Zero(builder$0040_24);
        }
        else {
            const proj_7 = matchValue_13;
            Expect_isTrue(Math.abs(proj_7.FromX) < 1E-06)("tolerance allows start");
            Test_TestCaseBuilder__Zero(builder$0040_24);
        }
    }));
})()]));

export const testsLine2DDivide = Test_testList("Line2D Divide/Split Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide into 1 segment", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_2, b_2, ln_2, vx, vy, a_4, b_4, ln_3, vx_1, vy_1;
        const ln = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pts = Euclid_Line2D__Line2D_divide_Static(1, ln);
        const actual_1 = pts.length | 0;
        if ((actual_1 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 2, "returns 2 points");
        }
        else {
            let valueType;
            let copyOfStruct = actual_1;
            valueType = int32_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            })) {
                const arg = int32ToString(2);
                const arg_1 = int32ToString(actual_1);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("returns 2 points");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_1)("returns 2 points");
            }
            throw new Exception(errorMsg);
        }
        Expect_isTrue(((a_2 = item(0, pts), (b_2 = ((ln_2 = ln, Pt_$ctor_7B00E9A0(ln_2.FromX, ln_2.FromY))), (vx = (a_2.X - b_2.X), (vy = (a_2.Y - b_2.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("start point");
        Expect_isTrue(((a_4 = item(1, pts), (b_4 = ((ln_3 = ln, Pt_$ctor_7B00E9A0(ln_3.ToX, ln_3.ToY))), (vx_1 = (a_4.X - b_4.X), (vy_1 = (a_4.Y - b_4.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("end point");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide into 5 segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_6, b_6, vx_2, vy_2, a_8, b_8, vx_3, vy_3, a_10, b_10, vx_4, vy_4, a_12, b_12, vx_5, vy_5, a_14, b_14, vx_6, vy_6, a_16, b_16, vx_7, vy_7;
        const ln_4 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pts_1 = Euclid_Line2D__Line2D_divide_Static(5, ln_4);
        const actual_3 = pts_1.length | 0;
        if ((actual_3 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 6, "returns 6 points");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_3;
            valueType_1 = int32_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg_6 = int32ToString(6);
                const arg_1_1 = int32ToString(actual_3);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("returns 6 points");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_3)("returns 6 points");
            }
            throw new Exception(errorMsg_1);
        }
        Expect_isTrue(((a_6 = item(0, pts_1), (b_6 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_2 = (a_6.X - b_6.X), (vy_2 = (a_6.Y - b_6.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_8 = item(1, pts_1), (b_8 = Pt_$ctor_7B00E9A0_2(2, 0), (vx_3 = (a_8.X - b_8.X), (vy_3 = (a_8.Y - b_8.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09)("point 1");
        Expect_isTrue(((a_10 = item(2, pts_1), (b_10 = Pt_$ctor_7B00E9A0_2(4, 0), (vx_4 = (a_10.X - b_10.X), (vy_4 = (a_10.Y - b_10.Y), Math.sqrt((vx_4 * vx_4) + (vy_4 * vy_4))))))) < 1E-09)("point 2");
        Expect_isTrue(((a_12 = item(3, pts_1), (b_12 = Pt_$ctor_7B00E9A0_2(6, 0), (vx_5 = (a_12.X - b_12.X), (vy_5 = (a_12.Y - b_12.Y), Math.sqrt((vx_5 * vx_5) + (vy_5 * vy_5))))))) < 1E-09)("point 3");
        Expect_isTrue(((a_14 = item(4, pts_1), (b_14 = Pt_$ctor_7B00E9A0_2(8, 0), (vx_6 = (a_14.X - b_14.X), (vy_6 = (a_14.Y - b_14.Y), Math.sqrt((vx_6 * vx_6) + (vy_6 * vy_6))))))) < 1E-09)("point 4");
        Expect_isTrue(((a_16 = item(5, pts_1), (b_16 = Pt_$ctor_7B00E9A0_2(10, 0), (vx_7 = (a_16.X - b_16.X), (vy_7 = (a_16.Y - b_16.Y), Math.sqrt((vx_7 * vx_7) + (vy_7 * vy_7))))))) < 1E-09)("point 5");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide diagonal line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_18, b_18, vx_8, vy_8, a_20, b_20, vx_9, vy_9, a_22, b_22, vx_10, vy_10, a_24, b_24, vx_11, vy_11, a_26, b_26, vx_12, vy_12;
        const ln_6 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
        const pts_2 = Euclid_Line2D__Line2D_divide_Static(4, ln_6);
        const actual_5 = pts_2.length | 0;
        if ((actual_5 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 5, "returns 5 points");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_5;
            valueType_2 = int32_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_7 = int32ToString(5);
                const arg_1_2 = int32ToString(actual_5);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("returns 5 points");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_5)("returns 5 points");
            }
            throw new Exception(errorMsg_2);
        }
        Expect_isTrue(((a_18 = item(0, pts_2), (b_18 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_8 = (a_18.X - b_18.X), (vy_8 = (a_18.Y - b_18.Y), Math.sqrt((vx_8 * vx_8) + (vy_8 * vy_8))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_20 = item(1, pts_2), (b_20 = Pt_$ctor_7B00E9A0_2(2.5, 2.5), (vx_9 = (a_20.X - b_20.X), (vy_9 = (a_20.Y - b_20.Y), Math.sqrt((vx_9 * vx_9) + (vy_9 * vy_9))))))) < 1E-09)("point 1");
        Expect_isTrue(((a_22 = item(2, pts_2), (b_22 = Pt_$ctor_7B00E9A0_2(5, 5), (vx_10 = (a_22.X - b_22.X), (vy_10 = (a_22.Y - b_22.Y), Math.sqrt((vx_10 * vx_10) + (vy_10 * vy_10))))))) < 1E-09)("point 2");
        Expect_isTrue(((a_24 = item(3, pts_2), (b_24 = Pt_$ctor_7B00E9A0_2(7.5, 7.5), (vx_11 = (a_24.X - b_24.X), (vy_11 = (a_24.Y - b_24.Y), Math.sqrt((vx_11 * vx_11) + (vy_11 * vy_11))))))) < 1E-09)("point 3");
        Expect_isTrue(((a_26 = item(4, pts_2), (b_26 = Pt_$ctor_7B00E9A0_2(10, 10), (vx_12 = (a_26.X - b_26.X), (vy_12 = (a_26.Y - b_26.Y), Math.sqrt((vx_12 * vx_12) + (vy_12 * vy_12))))))) < 1E-09)("point 4");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide with 0 segments throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ln_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_divide_Static(0, ln_8);
        }, "zero segments throws");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide with negative segments throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ln_10 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_divide_Static(-1, ln_10);
        }, "negative segments throws");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength exactly fitting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let a_28, b_28, vx_13, vy_13, a_30, b_30, vx_14, vy_14, a_32, b_32, vx_15, vy_15, a_34, b_34, vx_16, vy_16, a_36, b_36, vx_17, vy_17;
        const ln_12 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pts_3 = Euclid_Line2D__Line2D_divideMinLength_Static(2, ln_12);
        const actual_7 = pts_3.length | 0;
        if ((actual_7 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 5, "returns 5 points (4 segments)");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_7;
            valueType_3 = int32_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            })) {
                const arg_8 = int32ToString(5);
                const arg_1_3 = int32ToString(actual_7);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("returns 5 points (4 segments)");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_7)("returns 5 points (4 segments)");
            }
            throw new Exception(errorMsg_3);
        }
        Expect_isTrue(((a_28 = item(0, pts_3), (b_28 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_13 = (a_28.X - b_28.X), (vy_13 = (a_28.Y - b_28.Y), Math.sqrt((vx_13 * vx_13) + (vy_13 * vy_13))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_30 = item(1, pts_3), (b_30 = Pt_$ctor_7B00E9A0_2(2.5, 0), (vx_14 = (a_30.X - b_30.X), (vy_14 = (a_30.Y - b_30.Y), Math.sqrt((vx_14 * vx_14) + (vy_14 * vy_14))))))) < 1E-09)("point 1");
        Expect_isTrue(((a_32 = item(2, pts_3), (b_32 = Pt_$ctor_7B00E9A0_2(5, 0), (vx_15 = (a_32.X - b_32.X), (vy_15 = (a_32.Y - b_32.Y), Math.sqrt((vx_15 * vx_15) + (vy_15 * vy_15))))))) < 1E-09)("point 2");
        Expect_isTrue(((a_34 = item(3, pts_3), (b_34 = Pt_$ctor_7B00E9A0_2(7.5, 0), (vx_16 = (a_34.X - b_34.X), (vy_16 = (a_34.Y - b_34.Y), Math.sqrt((vx_16 * vx_16) + (vy_16 * vy_16))))))) < 1E-09)("point 3");
        Expect_isTrue(((a_36 = item(4, pts_3), (b_36 = Pt_$ctor_7B00E9A0_2(10, 0), (vx_17 = (a_36.X - b_36.X), (vy_17 = (a_36.Y - b_36.Y), Math.sqrt((vx_17 * vx_17) + (vy_17 * vy_17))))))) < 1E-09)("point 4");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength with remainder", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_38, b_38, vx_18, vy_18, a_40, b_40, vx_19, vy_19, a_42, b_42, vx_20, vy_20, a_44, b_44, vx_21, vy_21, a_46, b_46, vx_22, vy_22, a_48, b_48, vx_23, vy_23;
        const ln_14 = Line2D_$ctor_77D16AC0(0, 0, 11, 0);
        const pts_4 = Euclid_Line2D__Line2D_divideMinLength_Static(2, ln_14);
        const actual_9 = pts_4.length | 0;
        if ((actual_9 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 6, "returns 6 points (5 segments)");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_9;
            valueType_4 = int32_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_9 = int32ToString(6);
                const arg_1_4 = int32ToString(actual_9);
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("returns 6 points (5 segments)");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_9)("returns 6 points (5 segments)");
            }
            throw new Exception(errorMsg_4);
        }
        Expect_isTrue(((a_38 = item(0, pts_4), (b_38 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_18 = (a_38.X - b_38.X), (vy_18 = (a_38.Y - b_38.Y), Math.sqrt((vx_18 * vx_18) + (vy_18 * vy_18))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_40 = item(1, pts_4), (b_40 = Pt_$ctor_7B00E9A0_2(2.2, 0), (vx_19 = (a_40.X - b_40.X), (vy_19 = (a_40.Y - b_40.Y), Math.sqrt((vx_19 * vx_19) + (vy_19 * vy_19))))))) < 1E-09)("point 1");
        Expect_isTrue(((a_42 = item(2, pts_4), (b_42 = Pt_$ctor_7B00E9A0_2(4.4, 0), (vx_20 = (a_42.X - b_42.X), (vy_20 = (a_42.Y - b_42.Y), Math.sqrt((vx_20 * vx_20) + (vy_20 * vy_20))))))) < 1E-09)("point 2");
        Expect_isTrue(((a_44 = item(3, pts_4), (b_44 = Pt_$ctor_7B00E9A0_2(6.6, 0), (vx_21 = (a_44.X - b_44.X), (vy_21 = (a_44.Y - b_44.Y), Math.sqrt((vx_21 * vx_21) + (vy_21 * vy_21))))))) < 1E-09)("point 3");
        Expect_isTrue(((a_46 = item(4, pts_4), (b_46 = Pt_$ctor_7B00E9A0_2(8.8, 0), (vx_22 = (a_46.X - b_46.X), (vy_22 = (a_46.Y - b_46.Y), Math.sqrt((vx_22 * vx_22) + (vy_22 * vy_22))))))) < 1E-09)("point 4");
        Expect_isTrue(((a_48 = item(5, pts_4), (b_48 = Pt_$ctor_7B00E9A0_2(11, 0), (vx_23 = (a_48.X - b_48.X), (vy_23 = (a_48.Y - b_48.Y), Math.sqrt((vx_23 * vx_23) + (vy_23 * vy_23))))))) < 1E-09)("point 5");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength too large throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const ln_16 = Line2D_$ctor_77D16AC0(0, 0, 5, 0);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_divideMinLength_Static(10, ln_16);
        }, "throws when minLength > line length");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength very small creates many segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_50, b_50, vx_24, vy_24, a_53, b_53, vx_25, vy_25;
        const ln_18 = Line2D_$ctor_77D16AC0(0, 0, 1, 0);
        const pts_5 = Euclid_Line2D__Line2D_divideMinLength_Static(0.1, ln_18);
        const actual_11 = pts_5.length | 0;
        if ((actual_11 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 10, "returns 10 points (9 segments)");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_11;
            valueType_5 = int32_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_10 = int32ToString(10);
                const arg_1_5 = int32ToString(actual_11);
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("returns 10 points (9 segments)");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_11)("returns 10 points (9 segments)");
            }
            throw new Exception(errorMsg_5);
        }
        Expect_isTrue(((a_50 = item(0, pts_5), (b_50 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_24 = (a_50.X - b_50.X), (vy_24 = (a_50.Y - b_50.Y), Math.sqrt((vx_24 * vx_24) + (vy_24 * vy_24))))))) < 1E-09)("point 0");
        expectEqualEpsilon(item(5, pts_5).X, 5 / 9, "point 5");
        Expect_isTrue(((a_53 = item(9, pts_5), (b_53 = Pt_$ctor_7B00E9A0_2(1, 0), (vx_25 = (a_53.X - b_53.X), (vy_25 = (a_53.Y - b_53.Y), Math.sqrt((vx_25 * vx_25) + (vy_25 * vy_25))))))) < 1E-09)("point 9");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength zero or negative throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const ln_20 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_divideMinLength_Static(0, ln_20);
        }, "zero throws");
        Expect_throws(() => {
            Euclid_Line2D__Line2D_divideMinLength_Static(-1, ln_20);
        }, "negative throws");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMaxLength exactly fitting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_55, b_55, vx_26, vy_26, a_57, b_57, vx_27, vy_27, a_59, b_59, vx_28, vy_28, a_61, b_61, vx_29, vy_29, a_63, b_63, vx_30, vy_30, a_65, b_65, vx_31, vy_31;
        const ln_23 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pts_6 = Euclid_Line2D__Line2D_divideMaxLength_Static(2, ln_23);
        const actual_13 = pts_6.length | 0;
        if ((actual_13 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 6, "returns 6 points (5 segments)");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_13;
            valueType_6 = int32_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg_11 = int32ToString(6);
                const arg_1_6 = int32ToString(actual_13);
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("returns 6 points (5 segments)");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_13)("returns 6 points (5 segments)");
            }
            throw new Exception(errorMsg_6);
        }
        Expect_isTrue(((a_55 = item(0, pts_6), (b_55 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_26 = (a_55.X - b_55.X), (vy_26 = (a_55.Y - b_55.Y), Math.sqrt((vx_26 * vx_26) + (vy_26 * vy_26))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_57 = item(1, pts_6), (b_57 = Pt_$ctor_7B00E9A0_2(2, 0), (vx_27 = (a_57.X - b_57.X), (vy_27 = (a_57.Y - b_57.Y), Math.sqrt((vx_27 * vx_27) + (vy_27 * vy_27))))))) < 1E-09)("point 1");
        Expect_isTrue(((a_59 = item(2, pts_6), (b_59 = Pt_$ctor_7B00E9A0_2(4, 0), (vx_28 = (a_59.X - b_59.X), (vy_28 = (a_59.Y - b_59.Y), Math.sqrt((vx_28 * vx_28) + (vy_28 * vy_28))))))) < 1E-09)("point 2");
        Expect_isTrue(((a_61 = item(3, pts_6), (b_61 = Pt_$ctor_7B00E9A0_2(6, 0), (vx_29 = (a_61.X - b_61.X), (vy_29 = (a_61.Y - b_61.Y), Math.sqrt((vx_29 * vx_29) + (vy_29 * vy_29))))))) < 1E-09)("point 3");
        Expect_isTrue(((a_63 = item(4, pts_6), (b_63 = Pt_$ctor_7B00E9A0_2(8, 0), (vx_30 = (a_63.X - b_63.X), (vy_30 = (a_63.Y - b_63.Y), Math.sqrt((vx_30 * vx_30) + (vy_30 * vy_30))))))) < 1E-09)("point 4");
        Expect_isTrue(((a_65 = item(5, pts_6), (b_65 = Pt_$ctor_7B00E9A0_2(10, 0), (vx_31 = (a_65.X - b_65.X), (vy_31 = (a_65.Y - b_65.Y), Math.sqrt((vx_31 * vx_31) + (vy_31 * vy_31))))))) < 1E-09)("point 5");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMaxLength with small remainder", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_67, b_67, vx_32, vy_32, a_69, b_69, vx_33, vy_33, a_72, b_71, vx_34, vy_34;
        const ln_25 = Line2D_$ctor_77D16AC0(0, 0, 10.1, 0);
        const pts_7 = Euclid_Line2D__Line2D_divideMaxLength_Static(2, ln_25);
        const actual_15 = pts_7.length | 0;
        if ((actual_15 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, 7, "returns 7 points (6 segments)");
        }
        else {
            let valueType_7;
            let copyOfStruct_7 = actual_15;
            valueType_7 = int32_type;
            const primitiveTypes_7 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_7;
            if (contains(valueType_7, primitiveTypes_7, {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            })) {
                const arg_12 = int32ToString(7);
                const arg_1_7 = int32ToString(actual_15);
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("returns 7 points (6 segments)");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_15)("returns 7 points (6 segments)");
            }
            throw new Exception(errorMsg_7);
        }
        Expect_isTrue(((a_67 = item(0, pts_7), (b_67 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_32 = (a_67.X - b_67.X), (vy_32 = (a_67.Y - b_67.Y), Math.sqrt((vx_32 * vx_32) + (vy_32 * vy_32))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_69 = item(6, pts_7), (b_69 = Pt_$ctor_7B00E9A0_2(10.1, 0), (vx_33 = (a_69.X - b_69.X), (vy_33 = (a_69.Y - b_69.Y), Math.sqrt((vx_33 * vx_33) + (vy_33 * vy_33))))))) < 1E-09)("point 6");
        expectEqualEpsilon(10.1 / 6, (a_72 = item(0, pts_7), (b_71 = item(1, pts_7), (vx_34 = (a_72.X - b_71.X), (vy_34 = (a_72.Y - b_71.Y), Math.sqrt((vx_34 * vx_34) + (vy_34 * vy_34)))))), "segment length");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMaxLength very large returns endpoints", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let a_74, b_74, vx_35, vy_35, a_76, b_76, vx_36, vy_36;
        const ln_27 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pts_8 = Euclid_Line2D__Line2D_divideMaxLength_Static(100, ln_27);
        const actual_17 = pts_8.length | 0;
        if ((actual_17 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, 2, "returns 2 points (1 segment)");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_17;
            valueType_8 = int32_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_8) => (structuralHash(x_8) | 0),
            })) {
                const arg_13 = int32ToString(2);
                const arg_1_8 = int32ToString(actual_17);
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("returns 2 points (1 segment)");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_17)("returns 2 points (1 segment)");
            }
            throw new Exception(errorMsg_8);
        }
        Expect_isTrue(((a_74 = item(0, pts_8), (b_74 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_35 = (a_74.X - b_74.X), (vy_35 = (a_74.Y - b_74.Y), Math.sqrt((vx_35 * vx_35) + (vy_35 * vy_35))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_76 = item(1, pts_8), (b_76 = Pt_$ctor_7B00E9A0_2(10, 0), (vx_36 = (a_76.X - b_76.X), (vy_36 = (a_76.Y - b_76.Y), Math.sqrt((vx_36 * vx_36) + (vy_36 * vy_36))))))) < 1E-09)("point 1");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMaxLength zero or negative throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const ln_29 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_divideMaxLength_Static(0, ln_29);
        }, "zero maxLength throws");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with gap into 2 segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_78, ln_33, b_78, vx_37, vy_37, a_80, ln_34, b_80, vx_38, vy_38, ln_35, x_10, ln_36, y_10, ln_37, a_83, ln_38, b_83, vx_39, vy_39, a_85, ln_39, b_85, vx_40, vy_40, ln_40, x_11, ln_41, y_11, ln_42;
        const ln_31 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const lns = Euclid_Line2D__Line2D_split_Static(2, 2, ln_31);
        const actual_19 = lns.length | 0;
        if ((actual_19 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, 2, "returns 2 lines");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_19;
            valueType_9 = int32_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_9) => (structuralHash(x_9) | 0),
            })) {
                const arg_14 = int32ToString(2);
                const arg_1_9 = int32ToString(actual_19);
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("returns 2 lines");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_19)("returns 2 lines");
            }
            throw new Exception(errorMsg_9);
        }
        Expect_isTrue(((a_78 = ((ln_33 = item(0, lns), Pt_$ctor_7B00E9A0(ln_33.FromX, ln_33.FromY))), (b_78 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_37 = (a_78.X - b_78.X), (vy_37 = (a_78.Y - b_78.Y), Math.sqrt((vx_37 * vx_37) + (vy_37 * vy_37))))))) < 1E-09)("first line from");
        Expect_isTrue(((a_80 = ((ln_34 = item(0, lns), Pt_$ctor_7B00E9A0(ln_34.ToX, ln_34.ToY))), (b_80 = Pt_$ctor_7B00E9A0_2(4, 0), (vx_38 = (a_80.X - b_80.X), (vy_38 = (a_80.Y - b_80.Y), Math.sqrt((vx_38 * vx_38) + (vy_38 * vy_38))))))) < 1E-09)("first line to");
        expectEqualEpsilon((ln_35 = item(0, lns), (x_10 = ((ln_36 = ln_35, ln_36.ToX - ln_36.FromX)), (y_10 = ((ln_37 = ln_35, ln_37.ToY - ln_37.FromY)), Math.sqrt((x_10 * x_10) + (y_10 * y_10))))), 4, "first line length");
        Expect_isTrue(((a_83 = ((ln_38 = item(1, lns), Pt_$ctor_7B00E9A0(ln_38.FromX, ln_38.FromY))), (b_83 = Pt_$ctor_7B00E9A0_2(6, 0), (vx_39 = (a_83.X - b_83.X), (vy_39 = (a_83.Y - b_83.Y), Math.sqrt((vx_39 * vx_39) + (vy_39 * vy_39))))))) < 1E-09)("second line from");
        Expect_isTrue(((a_85 = ((ln_39 = item(1, lns), Pt_$ctor_7B00E9A0(ln_39.ToX, ln_39.ToY))), (b_85 = Pt_$ctor_7B00E9A0_2(10, 0), (vx_40 = (a_85.X - b_85.X), (vy_40 = (a_85.Y - b_85.Y), Math.sqrt((vx_40 * vx_40) + (vy_40 * vy_40))))))) < 1E-09)("second line to");
        expectEqualEpsilon((ln_40 = item(1, lns), (x_11 = ((ln_41 = ln_40, ln_41.ToX - ln_41.FromX)), (y_11 = ((ln_42 = ln_40, ln_42.ToY - ln_42.FromY)), Math.sqrt((x_11 * x_11) + (y_11 * y_11))))), 4, "second line length");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with gap into 3 segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_88, ln_45, b_88, vx_41, vy_41, ln_46, ln_47, x_13, ln_48, y_13, ln_49, ln_50, ln_51, ln_52, x_14, ln_53, y_14, ln_54, ln_55, a_96, ln_56, b_96, vx_42, vy_42, ln_57, x_15, ln_58, y_15, ln_59;
        const ln_43 = Line2D_$ctor_77D16AC0(0, 0, 15, 0);
        const lns_1 = Euclid_Line2D__Line2D_split_Static(1, 3, ln_43);
        const actual_21 = lns_1.length | 0;
        if ((actual_21 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 3, "returns 3 lines");
        }
        else {
            let valueType_10;
            let copyOfStruct_10 = actual_21;
            valueType_10 = int32_type;
            const primitiveTypes_10 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_10;
            if (contains(valueType_10, primitiveTypes_10, {
                Equals: equals,
                GetHashCode: (x_12) => (structuralHash(x_12) | 0),
            })) {
                const arg_15 = int32ToString(3);
                const arg_1_10 = int32ToString(actual_21);
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("returns 3 lines");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_21)("returns 3 lines");
            }
            throw new Exception(errorMsg_10);
        }
        Expect_isTrue(((a_88 = ((ln_45 = item(0, lns_1), Pt_$ctor_7B00E9A0(ln_45.FromX, ln_45.FromY))), (b_88 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_41 = (a_88.X - b_88.X), (vy_41 = (a_88.Y - b_88.Y), Math.sqrt((vx_41 * vx_41) + (vy_41 * vy_41))))))) < 1E-09)("first line from");
        expectEqualEpsilon(((ln_46 = item(0, lns_1), Pt_$ctor_7B00E9A0(ln_46.ToX, ln_46.ToY))).X, 13 / 3, "first line to");
        expectEqualEpsilon((ln_47 = item(0, lns_1), (x_13 = ((ln_48 = ln_47, ln_48.ToX - ln_48.FromX)), (y_13 = ((ln_49 = ln_47, ln_49.ToY - ln_49.FromY)), Math.sqrt((x_13 * x_13) + (y_13 * y_13))))), 13 / 3, "first line length");
        expectEqualEpsilon(((ln_50 = item(1, lns_1), Pt_$ctor_7B00E9A0(ln_50.FromX, ln_50.FromY))).X, (13 / 3) + 1, "second line from");
        expectEqualEpsilon(((ln_51 = item(1, lns_1), Pt_$ctor_7B00E9A0(ln_51.ToX, ln_51.ToY))).X, (26 / 3) + 1, "second line to");
        expectEqualEpsilon((ln_52 = item(1, lns_1), (x_14 = ((ln_53 = ln_52, ln_53.ToX - ln_53.FromX)), (y_14 = ((ln_54 = ln_52, ln_54.ToY - ln_54.FromY)), Math.sqrt((x_14 * x_14) + (y_14 * y_14))))), 13 / 3, "second line length");
        expectEqualEpsilon(((ln_55 = item(2, lns_1), Pt_$ctor_7B00E9A0(ln_55.FromX, ln_55.FromY))).X, (26 / 3) + 2, "third line from");
        Expect_isTrue(((a_96 = ((ln_56 = item(2, lns_1), Pt_$ctor_7B00E9A0(ln_56.ToX, ln_56.ToY))), (b_96 = Pt_$ctor_7B00E9A0_2(15, 0), (vx_42 = (a_96.X - b_96.X), (vy_42 = (a_96.Y - b_96.Y), Math.sqrt((vx_42 * vx_42) + (vy_42 * vy_42))))))) < 1E-09)("third line to");
        expectEqualEpsilon((ln_57 = item(2, lns_1), (x_15 = ((ln_58 = ln_57, ln_58.ToX - ln_58.FromX)), (y_15 = ((ln_59 = ln_57, ln_59.ToY - ln_59.FromY)), Math.sqrt((x_15 * x_15) + (y_15 * y_15))))), 13 / 3, "third line length");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with zero gap same as divide", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_99, ln_62, b_99, vx_43, vy_43, a_101, ln_63, b_101, vx_44, vy_44, a_103, ln_64, b_103, vx_45, vy_45, a_105, ln_65, b_105, vx_46, vy_46, a_107, ln_66, b_107, vx_47, vy_47, a_109, ln_67, b_109, vx_48, vy_48, a_111, ln_68, b_111, vx_49, vy_49, a_113, ln_69, b_113, vx_50, vy_50, a_115, ln_70, b_115, vx_51, vy_51, a_117, ln_71, b_117, vx_52, vy_52;
        const ln_60 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const lns_2 = Euclid_Line2D__Line2D_split_Static(0, 5, ln_60);
        const actual_23 = lns_2.length | 0;
        if ((actual_23 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, 5, "returns 5 lines");
        }
        else {
            let valueType_11;
            let copyOfStruct_15 = actual_23;
            valueType_11 = int32_type;
            const primitiveTypes_11 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_11;
            if (contains(valueType_11, primitiveTypes_11, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_16 = int32ToString(5);
                const arg_1_11 = int32ToString(actual_23);
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("returns 5 lines");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_23)("returns 5 lines");
            }
            throw new Exception(errorMsg_11);
        }
        Expect_isTrue(((a_99 = ((ln_62 = item(0, lns_2), Pt_$ctor_7B00E9A0(ln_62.FromX, ln_62.FromY))), (b_99 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_43 = (a_99.X - b_99.X), (vy_43 = (a_99.Y - b_99.Y), Math.sqrt((vx_43 * vx_43) + (vy_43 * vy_43))))))) < 1E-09)("line 0 from");
        Expect_isTrue(((a_101 = ((ln_63 = item(0, lns_2), Pt_$ctor_7B00E9A0(ln_63.ToX, ln_63.ToY))), (b_101 = Pt_$ctor_7B00E9A0_2(2, 0), (vx_44 = (a_101.X - b_101.X), (vy_44 = (a_101.Y - b_101.Y), Math.sqrt((vx_44 * vx_44) + (vy_44 * vy_44))))))) < 1E-09)("line 0 to");
        Expect_isTrue(((a_103 = ((ln_64 = item(1, lns_2), Pt_$ctor_7B00E9A0(ln_64.FromX, ln_64.FromY))), (b_103 = Pt_$ctor_7B00E9A0_2(2, 0), (vx_45 = (a_103.X - b_103.X), (vy_45 = (a_103.Y - b_103.Y), Math.sqrt((vx_45 * vx_45) + (vy_45 * vy_45))))))) < 1E-09)("line 1 from");
        Expect_isTrue(((a_105 = ((ln_65 = item(1, lns_2), Pt_$ctor_7B00E9A0(ln_65.ToX, ln_65.ToY))), (b_105 = Pt_$ctor_7B00E9A0_2(4, 0), (vx_46 = (a_105.X - b_105.X), (vy_46 = (a_105.Y - b_105.Y), Math.sqrt((vx_46 * vx_46) + (vy_46 * vy_46))))))) < 1E-09)("line 1 to");
        Expect_isTrue(((a_107 = ((ln_66 = item(2, lns_2), Pt_$ctor_7B00E9A0(ln_66.FromX, ln_66.FromY))), (b_107 = Pt_$ctor_7B00E9A0_2(4, 0), (vx_47 = (a_107.X - b_107.X), (vy_47 = (a_107.Y - b_107.Y), Math.sqrt((vx_47 * vx_47) + (vy_47 * vy_47))))))) < 1E-09)("line 2 from");
        Expect_isTrue(((a_109 = ((ln_67 = item(2, lns_2), Pt_$ctor_7B00E9A0(ln_67.ToX, ln_67.ToY))), (b_109 = Pt_$ctor_7B00E9A0_2(6, 0), (vx_48 = (a_109.X - b_109.X), (vy_48 = (a_109.Y - b_109.Y), Math.sqrt((vx_48 * vx_48) + (vy_48 * vy_48))))))) < 1E-09)("line 2 to");
        Expect_isTrue(((a_111 = ((ln_68 = item(3, lns_2), Pt_$ctor_7B00E9A0(ln_68.FromX, ln_68.FromY))), (b_111 = Pt_$ctor_7B00E9A0_2(6, 0), (vx_49 = (a_111.X - b_111.X), (vy_49 = (a_111.Y - b_111.Y), Math.sqrt((vx_49 * vx_49) + (vy_49 * vy_49))))))) < 1E-09)("line 3 from");
        Expect_isTrue(((a_113 = ((ln_69 = item(3, lns_2), Pt_$ctor_7B00E9A0(ln_69.ToX, ln_69.ToY))), (b_113 = Pt_$ctor_7B00E9A0_2(8, 0), (vx_50 = (a_113.X - b_113.X), (vy_50 = (a_113.Y - b_113.Y), Math.sqrt((vx_50 * vx_50) + (vy_50 * vy_50))))))) < 1E-09)("line 3 to");
        Expect_isTrue(((a_115 = ((ln_70 = item(4, lns_2), Pt_$ctor_7B00E9A0(ln_70.FromX, ln_70.FromY))), (b_115 = Pt_$ctor_7B00E9A0_2(8, 0), (vx_51 = (a_115.X - b_115.X), (vy_51 = (a_115.Y - b_115.Y), Math.sqrt((vx_51 * vx_51) + (vy_51 * vy_51))))))) < 1E-09)("line 4 from");
        Expect_isTrue(((a_117 = ((ln_71 = item(4, lns_2), Pt_$ctor_7B00E9A0(ln_71.ToX, ln_71.ToY))), (b_117 = Pt_$ctor_7B00E9A0_2(10, 0), (vx_52 = (a_117.X - b_117.X), (vy_52 = (a_117.Y - b_117.Y), Math.sqrt((vx_52 * vx_52) + (vy_52 * vy_52))))))) < 1E-09)("line 4 to");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with large gap returns empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const ln_72 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const lns_3 = Euclid_Line2D__Line2D_split_Static(20, 2, ln_72);
        const actual_25 = lns_3.length | 0;
        if ((actual_25 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, 0, "returns empty array");
        }
        else {
            let valueType_12;
            let copyOfStruct_16 = actual_25;
            valueType_12 = int32_type;
            const primitiveTypes_12 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_12;
            if (contains(valueType_12, primitiveTypes_12, {
                Equals: equals,
                GetHashCode: (x_17) => (structuralHash(x_17) | 0),
            })) {
                const arg_17 = int32ToString(0);
                const arg_1_12 = int32ToString(actual_25);
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("returns empty array");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_25)("returns empty array");
            }
            throw new Exception(errorMsg_12);
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with 0 segments throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const ln_74 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_split_Static(1, 0, ln_74);
        }, "zero segments throws");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with negative segments throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const ln_76 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_split_Static(1, -1, ln_76);
        }, "negative segments throws");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("splitMinLength with gap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const ln_78 = Line2D_$ctor_77D16AC0(0, 0, 20, 0);
        const lns_4 = Euclid_Line2D__Line2D_splitMinLength_Static(1, 2, ln_78);
        Expect_isTrue(lns_4.length > 1)("returns multiple lines");
        Expect_isTrue(lns_4.every((l) => {
            let ln_80, x_18, ln_81, y_18, ln_82;
            return ((ln_80 = l, (x_18 = ((ln_81 = ln_80, ln_81.ToX - ln_81.FromX)), (y_18 = ((ln_82 = ln_80, ln_82.ToY - ln_82.FromY)), Math.sqrt((x_18 * x_18) + (y_18 * y_18)))))) >= 1.9;
        }))("each segment >= minLength");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("splitMinLength line too short throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const ln_83 = Line2D_$ctor_77D16AC0(0, 0, 1, 0);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_splitMinLength_Static(0.5, 5, ln_83);
        }, "throws when line too short");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("splitMaxLength with gap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const ln_85 = Line2D_$ctor_77D16AC0(0, 0, 20, 0);
        const lns_5 = Euclid_Line2D__Line2D_splitMaxLength_Static(1, 3, ln_85);
        Expect_isTrue(lns_5.length > 1)("returns multiple lines");
        Expect_isTrue(lns_5.every((l_1) => {
            let ln_87, x_19, ln_88, y_19, ln_89;
            return ((ln_87 = l_1, (x_19 = ((ln_88 = ln_87, ln_88.ToX - ln_88.FromX)), (y_19 = ((ln_89 = ln_87, ln_89.ToY - ln_89.FromY)), Math.sqrt((x_19 * x_19) + (y_19 * y_19)))))) <= 3.1;
        }))("each segment <= maxLength");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery basic", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let a_119, b_119, vx_53, vy_53, a_121, b_121, vx_54, vy_54, a_123, b_123, vx_55, vy_55, a_125, b_125, vx_56, vy_56, a_127, b_127, vx_57, vy_57, a_129, b_129, vx_58, vy_58;
        const ln_90 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pts_9 = Euclid_Line2D__Line2D_divideEvery_Static(2, ln_90);
        const actual_27 = pts_9.length | 0;
        if ((actual_27 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_27, 6, "includes start and end");
        }
        else {
            let valueType_13;
            let copyOfStruct_17 = actual_27;
            valueType_13 = int32_type;
            const primitiveTypes_13 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_13;
            if (contains(valueType_13, primitiveTypes_13, {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            })) {
                const arg_18 = int32ToString(6);
                const arg_1_13 = int32ToString(actual_27);
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_18)(arg_1_13)("includes start and end");
            }
            else {
                errorMsg_13 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_27)("includes start and end");
            }
            throw new Exception(errorMsg_13);
        }
        Expect_isTrue(((a_119 = item(0, pts_9), (b_119 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_53 = (a_119.X - b_119.X), (vy_53 = (a_119.Y - b_119.Y), Math.sqrt((vx_53 * vx_53) + (vy_53 * vy_53))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_121 = item(1, pts_9), (b_121 = Pt_$ctor_7B00E9A0_2(2, 0), (vx_54 = (a_121.X - b_121.X), (vy_54 = (a_121.Y - b_121.Y), Math.sqrt((vx_54 * vx_54) + (vy_54 * vy_54))))))) < 1E-09)("point 1");
        Expect_isTrue(((a_123 = item(2, pts_9), (b_123 = Pt_$ctor_7B00E9A0_2(4, 0), (vx_55 = (a_123.X - b_123.X), (vy_55 = (a_123.Y - b_123.Y), Math.sqrt((vx_55 * vx_55) + (vy_55 * vy_55))))))) < 1E-09)("point 2");
        Expect_isTrue(((a_125 = item(3, pts_9), (b_125 = Pt_$ctor_7B00E9A0_2(6, 0), (vx_56 = (a_125.X - b_125.X), (vy_56 = (a_125.Y - b_125.Y), Math.sqrt((vx_56 * vx_56) + (vy_56 * vy_56))))))) < 1E-09)("point 3");
        Expect_isTrue(((a_127 = item(4, pts_9), (b_127 = Pt_$ctor_7B00E9A0_2(8, 0), (vx_57 = (a_127.X - b_127.X), (vy_57 = (a_127.Y - b_127.Y), Math.sqrt((vx_57 * vx_57) + (vy_57 * vy_57))))))) < 1E-09)("point 4");
        Expect_isTrue(((a_129 = item(5, pts_9), (b_129 = Pt_$ctor_7B00E9A0_2(10, 0), (vx_58 = (a_129.X - b_129.X), (vy_58 = (a_129.Y - b_129.Y), Math.sqrt((vx_58 * vx_58) + (vy_58 * vy_58))))))) < 1E-09)("point 5");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery exact fit includes end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let a_131, b_131, vx_59, vy_59, a_133, b_133, vx_60, vy_60;
        const ln_91 = Line2D_$ctor_77D16AC0(0, 0, 5, 0);
        const pts_10 = Euclid_Line2D__Line2D_divideEvery_Static(1, ln_91);
        const actual_29 = pts_10.length | 0;
        if ((actual_29 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_29, 6, "includes end point");
        }
        else {
            let valueType_14;
            let copyOfStruct_18 = actual_29;
            valueType_14 = int32_type;
            const primitiveTypes_14 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_14;
            if (contains(valueType_14, primitiveTypes_14, {
                Equals: equals,
                GetHashCode: (x_21) => (structuralHash(x_21) | 0),
            })) {
                const arg_19 = int32ToString(6);
                const arg_1_14 = int32ToString(actual_29);
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_19)(arg_1_14)("includes end point");
            }
            else {
                errorMsg_14 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_29)("includes end point");
            }
            throw new Exception(errorMsg_14);
        }
        Expect_isTrue(((a_131 = item(0, pts_10), (b_131 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_59 = (a_131.X - b_131.X), (vy_59 = (a_131.Y - b_131.Y), Math.sqrt((vx_59 * vx_59) + (vy_59 * vy_59))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_133 = item(5, pts_10), (b_133 = Pt_$ctor_7B00E9A0_2(5, 0), (vx_60 = (a_133.X - b_133.X), (vy_60 = (a_133.Y - b_133.Y), Math.sqrt((vx_60 * vx_60) + (vy_60 * vy_60))))))) < 1E-09)("point 5");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery with tiny remainder", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let a_135, b_135, vx_61, vy_61, a_137, b_137, vx_62, vy_62;
        const ln_92 = Line2D_$ctor_77D16AC0(0, 0, 5.001, 0);
        const pts_11 = Euclid_Line2D__Line2D_divideEvery_Static(1, ln_92);
        const actual_31 = pts_11.length | 0;
        if ((actual_31 === 7) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_31, 7, "includes end due to remainder > 0.1%");
        }
        else {
            let valueType_15;
            let copyOfStruct_19 = actual_31;
            valueType_15 = int32_type;
            const primitiveTypes_15 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_15;
            if (contains(valueType_15, primitiveTypes_15, {
                Equals: equals,
                GetHashCode: (x_22) => (structuralHash(x_22) | 0),
            })) {
                const arg_20 = int32ToString(7);
                const arg_1_15 = int32ToString(actual_31);
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_20)(arg_1_15)("includes end due to remainder > 0.1%");
            }
            else {
                errorMsg_15 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(7)(actual_31)("includes end due to remainder > 0.1%");
            }
            throw new Exception(errorMsg_15);
        }
        Expect_isTrue(((a_135 = item(0, pts_11), (b_135 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_61 = (a_135.X - b_135.X), (vy_61 = (a_135.Y - b_135.Y), Math.sqrt((vx_61 * vx_61) + (vy_61 * vy_61))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_137 = item(6, pts_11), (b_137 = Pt_$ctor_7B00E9A0_2(5.001, 0), (vx_62 = (a_137.X - b_137.X), (vy_62 = (a_137.Y - b_137.Y), Math.sqrt((vx_62 * vx_62) + (vy_62 * vy_62))))))) < 1E-09)("point 6");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery with very tiny remainder includes end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let a_139, b_139, vx_63, vy_63, a_141, b_141, vx_64, vy_64;
        const ln_93 = Line2D_$ctor_77D16AC0(0, 0, 5.0001, 0);
        const pts_12 = Euclid_Line2D__Line2D_divideEvery_Static(1, ln_93);
        const actual_33 = pts_12.length | 0;
        if ((actual_33 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_33, 6, "0.0001 / 5.0001 = 0.002% < 0.1% threshold so end not included");
        }
        else {
            let valueType_16;
            let copyOfStruct_20 = actual_33;
            valueType_16 = int32_type;
            const primitiveTypes_16 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_16;
            if (contains(valueType_16, primitiveTypes_16, {
                Equals: equals,
                GetHashCode: (x_23) => (structuralHash(x_23) | 0),
            })) {
                const arg_21 = int32ToString(6);
                const arg_1_16 = int32ToString(actual_33);
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_21)(arg_1_16)("0.0001 / 5.0001 = 0.002% < 0.1% threshold so end not included");
            }
            else {
                errorMsg_16 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_33)("0.0001 / 5.0001 = 0.002% < 0.1% threshold so end not included");
            }
            throw new Exception(errorMsg_16);
        }
        Expect_isTrue(((a_139 = item(0, pts_12), (b_139 = Pt_$ctor_7B00E9A0_2(0, 0), (vx_63 = (a_139.X - b_139.X), (vy_63 = (a_139.Y - b_139.Y), Math.sqrt((vx_63 * vx_63) + (vy_63 * vy_63))))))) < 1E-09)("point 0");
        Expect_isTrue(((a_141 = item(5, pts_12), (b_141 = Pt_$ctor_7B00E9A0_2(5.0001, 0), (vx_64 = (a_141.X - b_141.X), (vy_64 = (a_141.Y - b_141.Y), Math.sqrt((vx_64 * vx_64) + (vy_64 * vy_64))))))) < 1E-09)("point 5 at line end");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideInsideEvery excludes endpoints", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let a_143, b_143, vx_65, vy_65, a_145, b_145, vx_66, vy_66, a_147, b_147, vx_67, vy_67, a_149, b_149, vx_68, vy_68;
        const ln_94 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pts_13 = Euclid_Line2D__Line2D_divideInsideEvery_Static(2, ln_94);
        const actual_35 = pts_13.length | 0;
        if ((actual_35 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_35, 4, "returns 4 interior points");
        }
        else {
            let valueType_17;
            let copyOfStruct_21 = actual_35;
            valueType_17 = int32_type;
            const primitiveTypes_17 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_17;
            if (contains(valueType_17, primitiveTypes_17, {
                Equals: equals,
                GetHashCode: (x_24) => (structuralHash(x_24) | 0),
            })) {
                const arg_22 = int32ToString(4);
                const arg_1_17 = int32ToString(actual_35);
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_22)(arg_1_17)("returns 4 interior points");
            }
            else {
                errorMsg_17 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_35)("returns 4 interior points");
            }
            throw new Exception(errorMsg_17);
        }
        Expect_isTrue(((a_143 = item(0, pts_13), (b_143 = Pt_$ctor_7B00E9A0_2(2, 0), (vx_65 = (a_143.X - b_143.X), (vy_65 = (a_143.Y - b_143.Y), Math.sqrt((vx_65 * vx_65) + (vy_65 * vy_65))))))) < 1E-09)("point 0 at 20%");
        Expect_isTrue(((a_145 = item(1, pts_13), (b_145 = Pt_$ctor_7B00E9A0_2(4, 0), (vx_66 = (a_145.X - b_145.X), (vy_66 = (a_145.Y - b_145.Y), Math.sqrt((vx_66 * vx_66) + (vy_66 * vy_66))))))) < 1E-09)("point 1 at 40%");
        Expect_isTrue(((a_147 = item(2, pts_13), (b_147 = Pt_$ctor_7B00E9A0_2(6, 0), (vx_67 = (a_147.X - b_147.X), (vy_67 = (a_147.Y - b_147.Y), Math.sqrt((vx_67 * vx_67) + (vy_67 * vy_67))))))) < 1E-09)("point 2 at 60%");
        Expect_isTrue(((a_149 = item(3, pts_13), (b_149 = Pt_$ctor_7B00E9A0_2(8, 0), (vx_68 = (a_149.X - b_149.X), (vy_68 = (a_149.Y - b_149.Y), Math.sqrt((vx_68 * vx_68) + (vy_68 * vy_68))))))) < 1E-09)("point 3 at 80%");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideInsideEvery with 2 segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let a_151, b_151, vx_69, vy_69;
        const ln_95 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const pts_14 = Euclid_Line2D__Line2D_divideInsideEvery_Static(5, ln_95);
        const actual_37 = pts_14.length | 0;
        if ((actual_37 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_37, 1, "returns 1 interior point");
        }
        else {
            let valueType_18;
            let copyOfStruct_22 = actual_37;
            valueType_18 = int32_type;
            const primitiveTypes_18 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_18;
            if (contains(valueType_18, primitiveTypes_18, {
                Equals: equals,
                GetHashCode: (x_25) => (structuralHash(x_25) | 0),
            })) {
                const arg_23 = int32ToString(1);
                const arg_1_18 = int32ToString(actual_37);
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_23)(arg_1_18)("returns 1 interior point");
            }
            else {
                errorMsg_18 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_37)("returns 1 interior point");
            }
            throw new Exception(errorMsg_18);
        }
        Expect_isTrue(((a_151 = item(0, pts_14), (b_151 = Pt_$ctor_7B00E9A0_2(5, 0), (vx_69 = (a_151.X - b_151.X), (vy_69 = (a_151.Y - b_151.Y), Math.sqrt((vx_69 * vx_69) + (vy_69 * vy_69))))))) < 1E-09)("point 0 at 50%");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideInsideEvery very small distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let a_153, b_153, vx_70, vy_70, a_155, b_155, vx_71, vy_71, a_157, b_157, vx_72, vy_72;
        const ln_96 = Line2D_$ctor_77D16AC0(0, 0, 1, 0);
        const pts_15 = Euclid_Line2D__Line2D_divideInsideEvery_Static(0.1, ln_96);
        const actual_39 = pts_15.length | 0;
        if ((actual_39 === 9) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_39, 9, "returns 9 interior points");
        }
        else {
            let valueType_19;
            let copyOfStruct_23 = actual_39;
            valueType_19 = int32_type;
            const primitiveTypes_19 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_19;
            if (contains(valueType_19, primitiveTypes_19, {
                Equals: equals,
                GetHashCode: (x_26) => (structuralHash(x_26) | 0),
            })) {
                const arg_24 = int32ToString(9);
                const arg_1_19 = int32ToString(actual_39);
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_24)(arg_1_19)("returns 9 interior points");
            }
            else {
                errorMsg_19 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(9)(actual_39)("returns 9 interior points");
            }
            throw new Exception(errorMsg_19);
        }
        Expect_isTrue(((a_153 = item(0, pts_15), (b_153 = Pt_$ctor_7B00E9A0_2(0.1, 0), (vx_70 = (a_153.X - b_153.X), (vy_70 = (a_153.Y - b_153.Y), Math.sqrt((vx_70 * vx_70) + (vy_70 * vy_70))))))) < 1E-09)("point 0 at 10%");
        Expect_isTrue(((a_155 = item(4, pts_15), (b_155 = Pt_$ctor_7B00E9A0_2(0.5, 0), (vx_71 = (a_155.X - b_155.X), (vy_71 = (a_155.Y - b_155.Y), Math.sqrt((vx_71 * vx_71) + (vy_71 * vy_71))))))) < 1E-09)("point 4 at 50%");
        Expect_isTrue(((a_157 = item(8, pts_15), (b_157 = Pt_$ctor_7B00E9A0_2(0.9, 0), (vx_72 = (a_157.X - b_157.X), (vy_72 = (a_157.Y - b_157.Y), Math.sqrt((vx_72 * vx_72) + (vy_72 * vy_72))))))) < 1E-09)("point 8 at 90%");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery distance larger than line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let a_159, b_159, ln_98, a_161, b_161, ln_99;
        const ln_97 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const ps = Euclid_Line2D__Line2D_divideEvery_Static(11, ln_97);
        const actual_41 = ps.length | 0;
        if ((actual_41 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_41, 2, "returns 2 points");
        }
        else {
            let valueType_20;
            let copyOfStruct_24 = actual_41;
            valueType_20 = int32_type;
            const primitiveTypes_20 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_20;
            if (contains(valueType_20, primitiveTypes_20, {
                Equals: equals,
                GetHashCode: (x_27) => (structuralHash(x_27) | 0),
            })) {
                const arg_25 = int32ToString(2);
                const arg_1_20 = int32ToString(actual_41);
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_25)(arg_1_20)("returns 2 points");
            }
            else {
                errorMsg_20 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_41)("returns 2 points");
            }
            throw new Exception(errorMsg_20);
        }
        Expect_isTrue((a_159 = item(0, ps), (b_159 = ((ln_98 = ln_97, Pt_$ctor_7B00E9A0(ln_98.FromX, ln_98.FromY))), (Math.abs(a_159.X - b_159.X) <= 1E-12) && (Math.abs(a_159.Y - b_159.Y) <= 1E-12))))("first point is start");
        Expect_isTrue((a_161 = item(1, ps), (b_161 = ((ln_99 = ln_97, Pt_$ctor_7B00E9A0(ln_99.ToX, ln_99.ToY))), (Math.abs(a_161.X - b_161.X) <= 1E-12) && (Math.abs(a_161.Y - b_161.Y) <= 1E-12))))("second point is end");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})()]));

export const testsLine2DOffset = Test_testList("Line2D Offset Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset positive amount", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ln = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const off = Euclid_Line2D__Line2D_offset_Static(2, ln);
        expectEqualEpsilon(off.FromY, 2, "offset to left (positive Y)");
        expectEqualEpsilon(off.ToY, 2, "end also offset");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset negative amount", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ln_2 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const off_1 = Euclid_Line2D__Line2D_offset_Static(-3, ln_2);
        expectEqualEpsilon(off_1.FromY, -3, "offset to right (negative Y)");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset zero amount returns same line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const ln_4 = Line2D_$ctor_77D16AC0(0, 0, 10, 5);
        const off_2 = Euclid_Line2D__Line2D_offset_Static(0, ln_4);
        expectEqualEpsilon(off_2.FromX, ln_4.FromX, "from X unchanged");
        expectEqualEpsilon(off_2.FromY, ln_4.FromY, "from Y unchanged");
        expectEqualEpsilon(off_2.ToX, ln_4.ToX, "to X unchanged");
        expectEqualEpsilon(off_2.ToY, ln_4.ToY, "to Y unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset diagonal line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let ln_8, x, ln_9, y, ln_10, ln_14, ln_15, a_8, b_8;
        const ln_6 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
        const off_3 = Euclid_Line2D__Line2D_offset_Static(1, ln_6);
        Expect_isTrue(((ln_8 = off_3, (x = ((ln_9 = ln_8, ln_9.ToX - ln_9.FromX)), (y = ((ln_10 = ln_8, ln_10.ToY - ln_10.FromY)), Math.sqrt((x * x) + (y * y)))))) > 0)("diagonal offset perpendicular");
        let perp;
        let v;
        let a_7;
        const ln_11 = off_3;
        a_7 = Pt_$ctor_7B00E9A0(ln_11.FromX, ln_11.FromY);
        let b_7;
        const ln_12 = ln_6;
        b_7 = Pt_$ctor_7B00E9A0(ln_12.FromX, ln_12.FromY);
        v = Vc_$ctor_7B00E9A0_1(a_7.X - b_7.X, a_7.Y - b_7.Y);
        const x_1 = v.X;
        const y_1 = v.Y;
        const l = Math.sqrt((x_1 * x_1) + (y_1 * y_1));
        if (!(l > 1E-12)) {
            failTooSmall("Vc.Unitized", v);
        }
        perp = UnitVc_$ctor_7B00E9A0(x_1 / l, y_1 / l);
        let tang;
        let v_1;
        const ln_13 = ln_6;
        v_1 = Vc_$ctor_7B00E9A0_1((ln_14 = ln_13, ln_14.ToX - ln_14.FromX), (ln_15 = ln_13, ln_15.ToY - ln_15.FromY));
        const x_4 = v_1.X;
        const y_3 = v_1.Y;
        const l_1 = Math.sqrt((x_4 * x_4) + (y_3 * y_3));
        if (!(l_1 > 1E-12)) {
            failTooSmall("Vc.Unitized", v_1);
        }
        tang = UnitVc_$ctor_7B00E9A0(x_4 / l_1, y_3 / l_1);
        Expect_isTrue(Math.abs((a_8 = perp, (b_8 = tang, (a_8.X * b_8.X) + (a_8.Y * b_8.Y)))) < 1E-09)("perpendicular to original");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset vertical line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ln_16 = Line2D_$ctor_77D16AC0(5, 0, 5, 10);
        const off_4 = Euclid_Line2D__Line2D_offset_Static(2, ln_16);
        expectEqualEpsilon(off_4.FromX, 3, "offset horizontally for vertical line");
        expectEqualEpsilon(off_4.FromY, 0, "Y coordinates unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const ln_18 = Line2D_$ctor_77D16AC0(0, 0, 1E-13, 0);
        Expect_throws(() => {
            Euclid_Line2D__Line2D_offset_Static(1, ln_18);
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})()]));

export const testsLine2DWithLength = Test_testList("Line2D WithLength Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromStart shorter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let ln_5, x_2, ln_6, y_1, ln_7;
        const ln = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let result;
        const ln_2 = ln;
        let x;
        const ln_3 = ln_2;
        x = (ln_3.ToX - ln_3.FromX);
        let y;
        const ln_4 = ln_2;
        y = (ln_4.ToY - ln_4.FromY);
        const l = Math.sqrt((x * x) + (y * y));
        if (!(l > 1E-12)) {
            failTooSmall("Line2D.withLengthFromStart", ln_2);
        }
        result = Line2D_$ctor_77D16AC0_1(ln_2.FromX, ln_2.FromY, ln_2.FromX + ((x * 5) / l), ln_2.FromY + ((y * 5) / l));
        expectEqualEpsilon(result.FromX, 0, "starts at same point");
        expectEqualEpsilon((ln_5 = result, (x_2 = ((ln_6 = ln_5, ln_6.ToX - ln_6.FromX)), (y_1 = ((ln_7 = ln_5, ln_7.ToY - ln_7.FromY)), Math.sqrt((x_2 * x_2) + (y_1 * y_1))))), 5, "new length is 5");
        expectEqualEpsilon(result.ToX, 5, "ends at 5");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromStart longer", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let ln_13, x_5, ln_14, y_3, ln_15;
        const ln_8 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let result_1;
        const ln_10 = ln_8;
        let x_3;
        const ln_11 = ln_10;
        x_3 = (ln_11.ToX - ln_11.FromX);
        let y_2;
        const ln_12 = ln_10;
        y_2 = (ln_12.ToY - ln_12.FromY);
        const l_1 = Math.sqrt((x_3 * x_3) + (y_2 * y_2));
        if (!(l_1 > 1E-12)) {
            failTooSmall("Line2D.withLengthFromStart", ln_10);
        }
        result_1 = Line2D_$ctor_77D16AC0_1(ln_10.FromX, ln_10.FromY, ln_10.FromX + ((x_3 * 15) / l_1), ln_10.FromY + ((y_2 * 15) / l_1));
        expectEqualEpsilon(result_1.FromX, 0, "starts at same point");
        expectEqualEpsilon((ln_13 = result_1, (x_5 = ((ln_14 = ln_13, ln_14.ToX - ln_14.FromX)), (y_3 = ((ln_15 = ln_13, ln_15.ToY - ln_15.FromY)), Math.sqrt((x_5 * x_5) + (y_3 * y_3))))), 15, "new length is 15");
        expectEqualEpsilon(result_1.ToX, 15, "ends at 15");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromStart diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let ln_21, x_8, ln_22, y_5, ln_23, a_8, ln_24, b_8, ln_25, vx, vy;
        const ln_16 = Line2D_$ctor_77D16AC0(0, 0, 3, 4);
        let result_2;
        const ln_18 = ln_16;
        let x_6;
        const ln_19 = ln_18;
        x_6 = (ln_19.ToX - ln_19.FromX);
        let y_4;
        const ln_20 = ln_18;
        y_4 = (ln_20.ToY - ln_20.FromY);
        const l_2 = Math.sqrt((x_6 * x_6) + (y_4 * y_4));
        if (!(l_2 > 1E-12)) {
            failTooSmall("Line2D.withLengthFromStart", ln_18);
        }
        result_2 = Line2D_$ctor_77D16AC0_1(ln_18.FromX, ln_18.FromY, ln_18.FromX + ((x_6 * 10) / l_2), ln_18.FromY + ((y_4 * 10) / l_2));
        expectEqualEpsilon((ln_21 = result_2, (x_8 = ((ln_22 = ln_21, ln_22.ToX - ln_22.FromX)), (y_5 = ((ln_23 = ln_21, ln_23.ToY - ln_23.FromY)), Math.sqrt((x_8 * x_8) + (y_5 * y_5))))), 10, "length is 10");
        Expect_isTrue(((a_8 = ((ln_24 = result_2, Pt_$ctor_7B00E9A0(ln_24.FromX, ln_24.FromY))), (b_8 = ((ln_25 = ln_16, Pt_$ctor_7B00E9A0(ln_25.FromX, ln_25.FromY))), (vx = (a_8.X - b_8.X), (vy = (a_8.Y - b_8.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("starts at origin");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromStart too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ln_26 = Line2D_$ctor_77D16AC0(0, 0, 1E-13, 0);
        Expect_throws(() => {
            let ln_28, x_9, ln_29, y_6, ln_30, l_3;
            (ln_28 = ln_26, (x_9 = ((ln_29 = ln_28, ln_29.ToX - ln_29.FromX)), (y_6 = ((ln_30 = ln_28, ln_30.ToY - ln_30.FromY)), (l_3 = Math.sqrt((x_9 * x_9) + (y_6 * y_6)), (!(l_3 > 1E-12) ? failTooSmall("Line2D.withLengthFromStart", ln_28) : undefined, Line2D_$ctor_77D16AC0_1(ln_28.FromX, ln_28.FromY, ln_28.FromX + ((x_9 * 5) / l_3), ln_28.FromY + ((y_6 * 5) / l_3)))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthToEnd shorter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let ln_34, x_13, ln_35, y_8, ln_36;
        const ln_31 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let result_3;
        const ln_33 = ln_31;
        const x_11 = ln_33.FromX - ln_33.ToX;
        const y_7 = ln_33.FromY - ln_33.ToY;
        const l_4 = Math.sqrt((x_11 * x_11) + (y_7 * y_7));
        if (!(l_4 > 1E-12)) {
            failTooSmall("Line2D.withLengthToEnd", ln_33);
        }
        result_3 = Line2D_$ctor_77D16AC0_1(ln_33.ToX + ((x_11 * 5) / l_4), ln_33.ToY + ((y_7 * 5) / l_4), ln_33.ToX, ln_33.ToY);
        expectEqualEpsilon(result_3.ToX, 10, "ends at same point");
        expectEqualEpsilon((ln_34 = result_3, (x_13 = ((ln_35 = ln_34, ln_35.ToX - ln_35.FromX)), (y_8 = ((ln_36 = ln_34, ln_36.ToY - ln_36.FromY)), Math.sqrt((x_13 * x_13) + (y_8 * y_8))))), 5, "new length is 5");
        expectEqualEpsilon(result_3.FromX, 5, "starts at 5");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthToEnd longer", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let ln_40, x_16, ln_41, y_10, ln_42;
        const ln_37 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let result_4;
        const ln_39 = ln_37;
        const x_14 = ln_39.FromX - ln_39.ToX;
        const y_9 = ln_39.FromY - ln_39.ToY;
        const l_5 = Math.sqrt((x_14 * x_14) + (y_9 * y_9));
        if (!(l_5 > 1E-12)) {
            failTooSmall("Line2D.withLengthToEnd", ln_39);
        }
        result_4 = Line2D_$ctor_77D16AC0_1(ln_39.ToX + ((x_14 * 15) / l_5), ln_39.ToY + ((y_9 * 15) / l_5), ln_39.ToX, ln_39.ToY);
        expectEqualEpsilon(result_4.ToX, 10, "ends at same point");
        expectEqualEpsilon((ln_40 = result_4, (x_16 = ((ln_41 = ln_40, ln_41.ToX - ln_41.FromX)), (y_10 = ((ln_42 = ln_40, ln_42.ToY - ln_42.FromY)), Math.sqrt((x_16 * x_16) + (y_10 * y_10))))), 15, "new length is 15");
        expectEqualEpsilon(result_4.FromX, -5, "starts at -5");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthToEnd too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const ln_43 = Line2D_$ctor_77D16AC0(0, 0, 1E-13, 0);
        Expect_throws(() => {
            let ln_45, x_17, y_11, l_6;
            (ln_45 = ln_43, (x_17 = (ln_45.FromX - ln_45.ToX), (y_11 = (ln_45.FromY - ln_45.ToY), (l_6 = Math.sqrt((x_17 * x_17) + (y_11 * y_11)), (!(l_6 > 1E-12) ? failTooSmall("Line2D.withLengthToEnd", ln_45) : undefined, Line2D_$ctor_77D16AC0_1(ln_45.ToX + ((x_17 * 5) / l_6), ln_45.ToY + ((y_11 * 5) / l_6), ln_45.ToX, ln_45.ToY))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromMid shorter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let ln_49, x_21, ln_50, y_13, ln_51;
        const ln_46 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let result_5;
        const ln_48 = ln_46;
        const x_19 = ln_48.FromX - ln_48.ToX;
        const y_12 = ln_48.FromY - ln_48.ToY;
        const l_7 = Math.sqrt((x_19 * x_19) + (y_12 * y_12));
        if (!(l_7 > 1E-12)) {
            failTooSmall("Line2D.withLengthFromMid", ln_48);
        }
        const f_2 = ((4 / l_7) + 1) * 0.5;
        result_5 = Line2D_$ctor_77D16AC0_1(ln_48.ToX + (x_19 * f_2), ln_48.ToY + (y_12 * f_2), ln_48.FromX - (x_19 * f_2), ln_48.FromY - (y_12 * f_2));
        expectEqualEpsilon((ln_49 = result_5, (x_21 = ((ln_50 = ln_49, ln_50.ToX - ln_50.FromX)), (y_13 = ((ln_51 = ln_49, ln_51.ToY - ln_51.FromY)), Math.sqrt((x_21 * x_21) + (y_13 * y_13))))), 4, "new length is 4");
        expectEqualEpsilon(result_5.FromX, 3, "from at 3");
        expectEqualEpsilon(result_5.ToX, 7, "to at 7");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromMid longer", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let ln_55, x_24, ln_56, y_15, ln_57;
        const ln_52 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let result_6;
        const ln_54 = ln_52;
        const x_22 = ln_54.FromX - ln_54.ToX;
        const y_14 = ln_54.FromY - ln_54.ToY;
        const l_8 = Math.sqrt((x_22 * x_22) + (y_14 * y_14));
        if (!(l_8 > 1E-12)) {
            failTooSmall("Line2D.withLengthFromMid", ln_54);
        }
        const f_3 = ((20 / l_8) + 1) * 0.5;
        result_6 = Line2D_$ctor_77D16AC0_1(ln_54.ToX + (x_22 * f_3), ln_54.ToY + (y_14 * f_3), ln_54.FromX - (x_22 * f_3), ln_54.FromY - (y_14 * f_3));
        expectEqualEpsilon((ln_55 = result_6, (x_24 = ((ln_56 = ln_55, ln_56.ToX - ln_56.FromX)), (y_15 = ((ln_57 = ln_55, ln_57.ToY - ln_57.FromY)), Math.sqrt((x_24 * x_24) + (y_15 * y_15))))), 20, "new length is 20");
        expectEqualEpsilon(result_6.FromX, -5, "from at -5");
        expectEqualEpsilon(result_6.ToX, 15, "to at 15");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromMid preserves midpoint", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let ln_61, x_27, y_17;
        const ln_58 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let result_7;
        const ln_60 = ln_58;
        const x_25 = ln_60.FromX - ln_60.ToX;
        const y_16 = ln_60.FromY - ln_60.ToY;
        const l_9 = Math.sqrt((x_25 * x_25) + (y_16 * y_16));
        if (!(l_9 > 1E-12)) {
            failTooSmall("Line2D.withLengthFromMid", ln_60);
        }
        const f_4 = ((6 / l_9) + 1) * 0.5;
        result_7 = Line2D_$ctor_77D16AC0_1(ln_60.ToX + (x_25 * f_4), ln_60.ToY + (y_16 * f_4), ln_60.FromX - (x_25 * f_4), ln_60.FromY - (y_16 * f_4));
        expectEqualEpsilon(((ln_61 = result_7, (x_27 = ((ln_61.ToX + ln_61.FromX) * 0.5), (y_17 = ((ln_61.ToY + ln_61.FromY) * 0.5), Pt_$ctor_7B00E9A0_1(x_27, y_17))))).X, 5, "midpoint unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromMid too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const ln_62 = Line2D_$ctor_77D16AC0(0, 0, 1E-13, 0);
        Expect_throws(() => {
            let ln_64, x_28, y_18, l_10, f_5;
            (ln_64 = ln_62, (x_28 = (ln_64.FromX - ln_64.ToX), (y_18 = (ln_64.FromY - ln_64.ToY), (l_10 = Math.sqrt((x_28 * x_28) + (y_18 * y_18)), (!(l_10 > 1E-12) ? failTooSmall("Line2D.withLengthFromMid", ln_64) : undefined, (f_5 = (((5 / l_10) + 1) * 0.5), Line2D_$ctor_77D16AC0_1(ln_64.ToX + (x_28 * f_5), ln_64.ToY + (y_18 * f_5), ln_64.FromX - (x_28 * f_5), ln_64.FromY - (y_18 * f_5))))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance positive", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const ln_65 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let pt;
        const ln_67 = ln_65;
        let x_30;
        const ln_68 = ln_67;
        x_30 = (ln_68.ToX - ln_68.FromX);
        let y_19;
        const ln_69 = ln_67;
        y_19 = (ln_69.ToY - ln_69.FromY);
        const len_22 = Math.sqrt((x_30 * x_30) + (y_19 * y_19));
        if (!(len_22 > 1E-12)) {
            failTooSmall("Line2D.pointAtDistance", ln_67);
        }
        pt = Pt_$ctor_7B00E9A0_1(ln_67.FromX + ((x_30 * 3) / len_22), ln_67.FromY + ((y_19 * 3) / len_22));
        expectEqualEpsilon(pt.X, 3, "point at distance 3");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance negative", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const ln_70 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let pt_1;
        const ln_72 = ln_70;
        let x_32;
        const ln_73 = ln_72;
        x_32 = (ln_73.ToX - ln_73.FromX);
        let y_20;
        const ln_74 = ln_72;
        y_20 = (ln_74.ToY - ln_74.FromY);
        const len_23 = Math.sqrt((x_32 * x_32) + (y_20 * y_20));
        if (!(len_23 > 1E-12)) {
            failTooSmall("Line2D.pointAtDistance", ln_72);
        }
        pt_1 = Pt_$ctor_7B00E9A0_1(ln_72.FromX + ((x_32 * -2) / len_23), ln_72.FromY + ((y_20 * -2) / len_23));
        expectEqualEpsilon(pt_1.X, -2, "negative goes before start");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance beyond end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const ln_75 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        let pt_2;
        const ln_77 = ln_75;
        let x_34;
        const ln_78 = ln_77;
        x_34 = (ln_78.ToX - ln_78.FromX);
        let y_21;
        const ln_79 = ln_77;
        y_21 = (ln_79.ToY - ln_79.FromY);
        const len_24 = Math.sqrt((x_34 * x_34) + (y_21 * y_21));
        if (!(len_24 > 1E-12)) {
            failTooSmall("Line2D.pointAtDistance", ln_77);
        }
        pt_2 = Pt_$ctor_7B00E9A0_1(ln_77.FromX + ((x_34 * 15) / len_24), ln_77.FromY + ((y_21 * 15) / len_24));
        expectEqualEpsilon(pt_2.X, 15, "extends beyond end");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_26, b_26, ln_85, vx_1, vy_1;
        const ln_80 = Line2D_$ctor_77D16AC0(0, 0, 3, 4);
        let pt_3;
        const ln_82 = ln_80;
        let x_36;
        const ln_83 = ln_82;
        x_36 = (ln_83.ToX - ln_83.FromX);
        let y_22;
        const ln_84 = ln_82;
        y_22 = (ln_84.ToY - ln_84.FromY);
        const len_25 = Math.sqrt((x_36 * x_36) + (y_22 * y_22));
        if (!(len_25 > 1E-12)) {
            failTooSmall("Line2D.pointAtDistance", ln_82);
        }
        pt_3 = Pt_$ctor_7B00E9A0_1(ln_82.FromX + ((x_36 * 5) / len_25), ln_82.FromY + ((y_22 * 5) / len_25));
        Expect_isTrue(((a_26 = pt_3, (b_26 = ((ln_85 = ln_80, Pt_$ctor_7B00E9A0(ln_85.ToX, ln_85.ToY))), (vx_1 = (a_26.X - b_26.X), (vy_1 = (a_26.Y - b_26.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("at line end for 3-4-5 triangle");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const ln_86 = Line2D_$ctor_77D16AC0(0, 0, 1E-13, 0);
        Expect_throws(() => {
            let ln_88, x_38, ln_89, y_23, ln_90, len_26;
            (ln_88 = ln_86, (x_38 = ((ln_89 = ln_88, ln_89.ToX - ln_89.FromX)), (y_23 = ((ln_90 = ln_88, ln_90.ToY - ln_90.FromY)), (len_26 = Math.sqrt((x_38 * x_38) + (y_23 * y_23)), (!(len_26 > 1E-12) ? failTooSmall("Line2D.pointAtDistance", ln_88) : undefined, Pt_$ctor_7B00E9A0_1(ln_88.FromX + ((x_38 * 1) / len_26), ln_88.FromY + ((y_23 * 1) / len_26)))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})()]));

export const testsLine2DIntersection = Test_testList("Line2D Intersection Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersect crossing lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const a = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
        const b = Line2D_$ctor_77D16AC0(5, 0, 5, 10);
        Expect_isTrue(Euclid_Line2D__Line2D_doIntersect_Static(a, b))("crossing lines intersect");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersect parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const a_1 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_1 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
        Expect_isFalse(Euclid_Line2D__Line2D_doIntersect_Static(a_1, b_1))("parallel lines don\'t intersect");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersect non-intersecting within segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const a_2 = Line2D_$ctor_77D16AC0(0, 0, 5, 0);
        const b_2 = Line2D_$ctor_77D16AC0(10, -5, 10, 5);
        Expect_isFalse(Euclid_Line2D__Line2D_doIntersect_Static(a_2, b_2))("segments don\'t reach intersection");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersect touching endpoints", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const a_3 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_3 = Line2D_$ctor_77D16AC0(10, 0, 10, 10);
        Expect_isTrue(Euclid_Line2D__Line2D_doIntersect_Static(a_3, b_3))("touching at endpoint");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersectOrOverlap crossing", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const a_4 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
        const b_4 = Line2D_$ctor_77D16AC0(5, 0, 5, 10);
        Expect_isTrue(Euclid_Line2D__Line2D_doIntersectOrOverlap_Static(a_4, b_4))("crossing lines");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersectOrOverlap overlapping collinear", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const a_5 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_5 = Line2D_$ctor_77D16AC0(5, 0, 15, 0);
        Expect_isTrue(Euclid_Line2D__Line2D_doIntersectOrOverlap_Static(a_5, b_5))("overlapping returns true");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersectOrOverlap parallel non-overlapping", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const a_6 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_6 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
        Expect_isFalse(Euclid_Line2D__Line2D_doIntersectOrOverlap_Static(a_6, b_6))("parallel don\'t overlap");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersect returns intersection point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_9, b_9, vx, vy;
        const a_7 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
        const b_7 = Line2D_$ctor_77D16AC0(5, 0, 5, 10);
        const matchValue = Euclid_Line2D__Line2D_tryIntersect_Static(a_7, b_7);
        if (matchValue == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_7);
        }
        else {
            const pt = matchValue;
            Expect_isTrue(((a_9 = pt, (b_9 = Pt_$ctor_7B00E9A0_2(5, 5), (vx = (a_9.X - b_9.X), (vy = (a_9.Y - b_9.Y), Math.sqrt((vx * vx) + (vy * vy))))))) < 1E-09)("intersection at (5,5)");
            Test_TestCaseBuilder__Zero(builder$0040_7);
        }
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersect no intersection returns None", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const a_10 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_10 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
        const matchValue_1 = Euclid_Line2D__Line2D_tryIntersect_Static(a_10, b_10);
        if (matchValue_1 == null) {
            Expect_isTrue(true)("parallel returns None");
            Test_TestCaseBuilder__Zero(builder$0040_8);
        }
        else {
            throw new Exception("expected None");
            Test_TestCaseBuilder__Zero(builder$0040_8);
        }
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersect diagonal intersection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let a_13, b_13, vx_1, vy_1;
        const a_11 = Line2D_$ctor_77D16AC0(0, 0, 10, 10);
        const b_11 = Line2D_$ctor_77D16AC0(0, 10, 10, 0);
        const matchValue_2 = Euclid_Line2D__Line2D_tryIntersect_Static(a_11, b_11);
        if (matchValue_2 == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_9);
        }
        else {
            const pt_1 = matchValue_2;
            Expect_isTrue(((a_13 = pt_1, (b_13 = Pt_$ctor_7B00E9A0_2(5, 5), (vx_1 = (a_13.X - b_13.X), (vy_1 = (a_13.Y - b_13.Y), Math.sqrt((vx_1 * vx_1) + (vy_1 * vy_1))))))) < 1E-09)("intersection at center");
            Test_TestCaseBuilder__Zero(builder$0040_9);
        }
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersectRay returns ray intersection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const a_14 = Line2D_$ctor_77D16AC0(0, 5, 5, 5);
        const b_14 = Line2D_$ctor_77D16AC0(10, 0, 10, 10);
        const matchValue_3 = Euclid_Line2D__Line2D_tryIntersectRay_Static(a_14, b_14);
        if (matchValue_3 == null) {
            throw new Exception("expected Some for ray intersection");
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
        else {
            const pt_2 = matchValue_3;
            expectEqualEpsilon(pt_2.X, 10, "ray extends to intersection");
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersectRay parallel returns None", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const a_16 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_16 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
        const matchValue_4 = Euclid_Line2D__Line2D_tryIntersectRay_Static(a_16, b_16);
        if (matchValue_4 == null) {
            Expect_isTrue(true)("parallel rays return None");
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
        else {
            throw new Exception("expected None");
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersectOrOverlap intersection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let a_19, b_19, vx_2, vy_2;
        const a_17 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
        const b_17 = Line2D_$ctor_77D16AC0(5, 0, 5, 10);
        const matchValue_5 = Euclid_Line2D__Line2D_tryIntersectOrOverlap_Static(a_17, b_17);
        if (matchValue_5 == null) {
            throw new Exception("expected Some");
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
        else {
            const pt_3 = matchValue_5;
            Expect_isTrue(((a_19 = pt_3, (b_19 = Pt_$ctor_7B00E9A0_2(5, 5), (vx_2 = (a_19.X - b_19.X), (vy_2 = (a_19.Y - b_19.Y), Math.sqrt((vx_2 * vx_2) + (vy_2 * vy_2))))))) < 1E-09)("intersection point");
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersectOrOverlap overlapping returns touching point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let a_22, b_22, vx_3, vy_3;
        const a_20 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_20 = Line2D_$ctor_77D16AC0(10, 0, 20, 0);
        const matchValue_6 = Euclid_Line2D__Line2D_tryIntersectOrOverlap_Static(a_20, b_20);
        if (matchValue_6 == null) {
            throw new Exception("expected Some for touching");
            Test_TestCaseBuilder__Zero(builder$0040_13);
        }
        else {
            const pt_4 = matchValue_6;
            Expect_isTrue(((a_22 = pt_4, (b_22 = Pt_$ctor_7B00E9A0_2(10, 0), (vx_3 = (a_22.X - b_22.X), (vy_3 = (a_22.Y - b_22.Y), Math.sqrt((vx_3 * vx_3) + (vy_3 * vy_3))))))) < 1E-09)("touching point");
            Test_TestCaseBuilder__Zero(builder$0040_13);
        }
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTouchingEndsOf not touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const a_23 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_23 = Line2D_$ctor_77D16AC0(0, 5, 10, 5);
        const result = XLine2D_getEndsTouching_Z44565CE5(a_23, b_23, 1E-06);
        if (result.tag === 0) {
            Expect_isTrue(true)("not touching");
            Test_TestCaseBuilder__Zero(builder$0040_14);
        }
        else {
            throw new Exception("expected NotTouching");
            Test_TestCaseBuilder__Zero(builder$0040_14);
        }
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTouchingEndsOf StartA_StartB", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const a_24 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_24 = Line2D_$ctor_77D16AC0(0, 0, 0, 10);
        const result_1 = XLine2D_getEndsTouching_Z44565CE5(a_24, b_24, 1E-06);
        if (result_1.tag === 1) {
            Expect_isTrue(true)("StartA_StartB");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
        else {
            throw new Exception("expected StartA_StartB");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTouchingEndsOf EndA_EndB", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const a_25 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_25 = Line2D_$ctor_77D16AC0(0, 10, 10, 0);
        const result_2 = XLine2D_getEndsTouching_Z44565CE5(a_25, b_25, 1E-06);
        if (result_2.tag === 2) {
            Expect_isTrue(true)("EndA_EndB");
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
        else {
            throw new Exception("expected EndA_EndB");
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTouchingEndsOf identical", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const a_26 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const b_26 = Line2D_$ctor_77D16AC0(0, 0, 10, 0);
        const result_3 = XLine2D_getEndsTouching_Z44565CE5(a_26, b_26, 1E-06);
        if (result_3.tag === 5) {
            Expect_isTrue(true)("identical");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
        else {
            throw new Exception("expected Identical");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
    }));
})()]));

export const testsLine3DBasics = Test_testList("Line3D Basics", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsZeroLength true", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let ln_1;
        const ln = Line3D_$ctor_76A78260(5, 3, 7, 5, 3, 7);
        Expect_isTrue((ln_1 = ln, ((ln_1.ToX === ln_1.FromX) && (ln_1.ToY === ln_1.FromY)) && (ln_1.ToZ === ln_1.FromZ)))("zero length line");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsZeroLength false", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let ln_3;
        const ln_2 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_isFalse((ln_3 = ln_2, ((ln_3.ToX === ln_3.FromX) && (ln_3.ToY === ln_3.FromY)) && (ln_3.ToZ === ln_3.FromZ)))("non-zero length line");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsXAligned", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let ln_5, x, ln_6, y, ln_7, z, ln_8;
        const ln_4 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_isTrue((ln_5 = ln_4, (x = Math.abs((ln_6 = ln_5, ln_6.ToX - ln_6.FromX)), (y = Math.abs((ln_7 = ln_5, ln_7.ToY - ln_7.FromY)), (z = Math.abs((ln_8 = ln_5, ln_8.ToZ - ln_8.FromZ)), (!(((x + y) + z) > 1E-06) ? failTooSmall("Line3D.IsXAligned", ln_5) : undefined, (y < 1E-09) && (z < 1E-09)))))))("X aligned line");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsYAligned", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let ln_10, x_2, ln_11, y_1, ln_12, z_1, ln_13;
        const ln_9 = Line3D_$ctor_76A78260(0, 0, 0, 0, 10, 0);
        Expect_isTrue((ln_10 = ln_9, (x_2 = Math.abs((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)), (y_1 = Math.abs((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)), (z_1 = Math.abs((ln_13 = ln_10, ln_13.ToZ - ln_13.FromZ)), (!(((x_2 + y_1) + z_1) > 1E-06) ? failTooSmall("Line3D.IsYAligned", ln_10) : undefined, (x_2 < 1E-09) && (z_1 < 1E-09)))))))("Y aligned line");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsZAligned", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let ln_15, x_4, ln_16, y_2, ln_17, z_2, ln_18, ln_19, x_6, ln_20, y_3, ln_21, z_3, ln_22;
        const ln_14 = Line3D_$ctor_76A78260(0, 0, 0, 0, 0, 10);
        Expect_isTrue((ln_15 = ln_14, (x_4 = Math.abs((ln_16 = ln_15, ln_16.ToX - ln_16.FromX)), (y_2 = Math.abs((ln_17 = ln_15, ln_17.ToY - ln_17.FromY)), (z_2 = Math.abs((ln_18 = ln_15, ln_18.ToZ - ln_18.FromZ)), (!(((x_4 + y_2) + z_2) > 1E-06) ? failTooSmall("Line3D.IsZAligned", ln_15) : undefined, (x_4 < 1E-09) && (y_2 < 1E-09)))))))("Z aligned line");
        Expect_isTrue((ln_19 = ln_14, (x_6 = Math.abs((ln_20 = ln_19, ln_20.ToX - ln_20.FromX)), (y_3 = Math.abs((ln_21 = ln_19, ln_21.ToY - ln_21.FromY)), (z_3 = Math.abs((ln_22 = ln_19, ln_22.ToZ - ln_22.FromZ)), (!(((x_6 + y_3) + z_3) > 1E-06) ? failTooSmall("Line3D.IsVertical", ln_19) : undefined, (x_6 < 1E-09) && (y_3 < 1E-09)))))))("IsVertical same as IsZAligned");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsHorizontal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let ln_24, x_8, ln_25, y_4, ln_26, z_4, ln_27;
        const ln_23 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 0);
        Expect_isTrue((ln_24 = ln_23, (x_8 = Math.abs((ln_25 = ln_24, ln_25.ToX - ln_25.FromX)), (y_4 = Math.abs((ln_26 = ln_24, ln_26.ToY - ln_26.FromY)), (z_4 = Math.abs((ln_27 = ln_24, ln_27.ToZ - ln_27.FromZ)), (!(((x_8 + y_4) + z_4) > 1E-06) ? failTooSmall("Line3D.IsHorizontal", ln_24) : undefined, z_4 < 1E-09))))))("horizontal line");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsHorizontal vertical line false", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let ln_29, x_10, ln_30, y_5, ln_31, z_5, ln_32;
        const ln_28 = Line3D_$ctor_76A78260(0, 0, 0, 0, 0, 10);
        Expect_isFalse((ln_29 = ln_28, (x_10 = Math.abs((ln_30 = ln_29, ln_30.ToX - ln_30.FromX)), (y_5 = Math.abs((ln_31 = ln_29, ln_31.ToY - ln_31.FromY)), (z_5 = Math.abs((ln_32 = ln_29, ln_32.ToZ - ln_32.FromZ)), (!(((x_10 + y_5) + z_5) > 1E-06) ? failTooSmall("Line3D.IsHorizontal", ln_29) : undefined, z_5 < 1E-09))))))("vertical line not horizontal");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Length calculation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let ln_34, x_12, ln_35, y_6, ln_36, z_6, ln_37;
        const ln_33 = Line3D_$ctor_76A78260(0, 0, 0, 3, 4, 0);
        expectEqualEpsilon((ln_34 = ln_33, (x_12 = ((ln_35 = ln_34, ln_35.ToX - ln_35.FromX)), (y_6 = ((ln_36 = ln_34, ln_36.ToY - ln_36.FromY)), (z_6 = ((ln_37 = ln_34, ln_37.ToZ - ln_37.FromZ)), Math.sqrt(((x_12 * x_12) + (y_6 * y_6)) + (z_6 * z_6)))))), 5, "length is 5");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LengthSq calculation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let ln_39, x_13, ln_40, y_7, ln_41, z_7, ln_42;
        const ln_38 = Line3D_$ctor_76A78260(0, 0, 0, 3, 4, 0);
        expectEqualEpsilon((ln_39 = ln_38, (x_13 = ((ln_40 = ln_39, ln_40.ToX - ln_40.FromX)), (y_7 = ((ln_41 = ln_39, ln_41.ToY - ln_41.FromY)), (z_7 = ((ln_42 = ln_39, ln_42.ToZ - ln_42.FromZ)), ((x_13 * x_13) + (y_7 * y_7)) + (z_7 * z_7))))), 25, "length squared is 25");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Mid point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const ln_43 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        let mid;
        const ln_44 = ln_43;
        mid = Pnt_$ctor_Z7AD9E565((ln_44.ToX + ln_44.FromX) * 0.5, (ln_44.ToY + ln_44.FromY) * 0.5, (ln_44.ToZ + ln_44.FromZ) * 0.5);
        expectEqualEpsilon(mid.X, 5, "midpoint X");
        expectEqualEpsilon(mid.Y, 5, "midpoint Y");
        expectEqualEpsilon(mid.Z, 5, "midpoint Z");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Reversed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_6, ln_47, b_6, ln_48, x_14, y_8, z_8, a_8, ln_49, b_8, ln_50, x_15, y_9, z_9;
        const ln_45 = Line3D_$ctor_76A78260(0, 0, 0, 10, 5, 3);
        let rev;
        const ln_46 = ln_45;
        rev = Line3D_$ctor_76A78260_1(ln_46.ToX, ln_46.ToY, ln_46.ToZ, ln_46.FromX, ln_46.FromY, ln_46.FromZ);
        Expect_isTrue(((a_6 = ((ln_47 = rev, Pnt_$ctor_Z7AD9E565_1(ln_47.FromX, ln_47.FromY, ln_47.FromZ))), (b_6 = ((ln_48 = ln_45, Pnt_$ctor_Z7AD9E565_1(ln_48.ToX, ln_48.ToY, ln_48.ToZ))), (x_14 = (a_6.X - b_6.X), (y_8 = (a_6.Y - b_6.Y), (z_8 = (a_6.Z - b_6.Z), Math.sqrt(((x_14 * x_14) + (y_8 * y_8)) + (z_8 * z_8)))))))) < 1E-09)("reversed from equals original to");
        Expect_isTrue(((a_8 = ((ln_49 = rev, Pnt_$ctor_Z7AD9E565_1(ln_49.ToX, ln_49.ToY, ln_49.ToZ))), (b_8 = ((ln_50 = ln_45, Pnt_$ctor_Z7AD9E565_1(ln_50.FromX, ln_50.FromY, ln_50.FromZ))), (x_15 = (a_8.X - b_8.X), (y_9 = (a_8.Y - b_8.Y), (z_9 = (a_8.Z - b_8.Z), Math.sqrt(((x_15 * x_15) + (y_9 * y_9)) + (z_9 * z_9)))))))) < 1E-09)("reversed to equals original from");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt 0.5", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let ln_53, ln_54, ln_55, a_10, b_10, ln_56, x_16, y_10, z_10;
        const ln_51 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        let pt;
        const ln_52 = ln_51;
        pt = Pnt_$ctor_Z7AD9E565(ln_52.FromX + (((ln_53 = ln_52, ln_53.ToX - ln_53.FromX)) * 0.5), ln_52.FromY + (((ln_54 = ln_52, ln_54.ToY - ln_54.FromY)) * 0.5), ln_52.FromZ + (((ln_55 = ln_52, ln_55.ToZ - ln_55.FromZ)) * 0.5));
        Expect_isTrue(((a_10 = pt, (b_10 = ((ln_56 = ln_51, Pnt_$ctor_Z7AD9E565((ln_56.ToX + ln_56.FromX) * 0.5, (ln_56.ToY + ln_56.FromY) * 0.5, (ln_56.ToZ + ln_56.FromZ) * 0.5))), (x_16 = (a_10.X - b_10.X), (y_10 = (a_10.Y - b_10.Y), (z_10 = (a_10.Z - b_10.Z), Math.sqrt(((x_16 * x_16) + (y_10 * y_10)) + (z_10 * z_10)))))))) < 1E-09)("evaluates to midpoint");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SubLine segment", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const ln_57 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let sub;
        const ln_58 = ln_57;
        const fromX = ln_58.FromX;
        const fromY = ln_58.FromY;
        const fromZ = ln_58.FromZ;
        const x_17 = ln_58.ToX - fromX;
        const y_11 = ln_58.ToY - fromY;
        const z_11 = ln_58.ToZ - fromZ;
        sub = Line3D_$ctor_76A78260_1(fromX + (x_17 * 0.2), fromY + (y_11 * 0.2), fromZ + (z_11 * 0.2), fromX + (x_17 * 0.8), fromY + (y_11 * 0.8), fromZ + (z_11 * 0.8));
        expectEqualEpsilon(sub.FromX, 2, "subline start");
        expectEqualEpsilon(sub.ToX, 8, "subline end");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})()]));

export const testsLine3DExtendShrink = Test_testList("Line3D Extend/Shrink", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Extend both ends", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ln = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let ext;
        const ln_1 = ln;
        let x;
        const ln_2 = ln_1;
        x = (ln_2.ToX - ln_2.FromX);
        let y;
        const ln_3 = ln_1;
        y = (ln_3.ToY - ln_3.FromY);
        let z;
        const ln_4 = ln_1;
        z = (ln_4.ToZ - ln_4.FromZ);
        const l = Math.sqrt(((x * x) + (y * y)) + (z * z));
        if (!(l > 1E-12)) {
            failTooSmall("Line3D.Extend", ln_1);
        }
        ext = Line3D_$ctor_76A78260_1(ln_1.FromX - ((x * 5) / l), ln_1.FromY - ((y * 5) / l), ln_1.FromZ - ((z * 5) / l), ln_1.ToX + ((x * 5) / l), ln_1.ToY + ((y * 5) / l), ln_1.ToZ + ((z * 5) / l));
        expectEqualEpsilon(ext.FromX, -5, "extended start");
        expectEqualEpsilon(ext.ToX, 15, "extended end");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExtendStart", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ln_5 = Line3D_$ctor_76A78260(5, 0, 0, 10, 0, 0);
        let ext_1;
        const ln_6 = ln_5;
        let x_2;
        const ln_7 = ln_6;
        x_2 = (ln_7.ToX - ln_7.FromX);
        let y_1;
        const ln_8 = ln_6;
        y_1 = (ln_8.ToY - ln_8.FromY);
        let z_1;
        const ln_9 = ln_6;
        z_1 = (ln_9.ToZ - ln_9.FromZ);
        const l_1 = Math.sqrt(((x_2 * x_2) + (y_1 * y_1)) + (z_1 * z_1));
        if (!(l_1 > 1E-12)) {
            failTooSmall("Line3D.ExtendStart", ln_6);
        }
        ext_1 = Line3D_$ctor_76A78260_1(ln_6.FromX - ((x_2 * 5) / l_1), ln_6.FromY - ((y_1 * 5) / l_1), ln_6.FromZ - ((z_1 * 5) / l_1), ln_6.ToX, ln_6.ToY, ln_6.ToZ);
        expectEqualEpsilon(ext_1.FromX, 0, "extended start");
        expectEqualEpsilon(ext_1.ToX, 10, "end unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExtendEnd", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const ln_10 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let ext_2;
        const ln_11 = ln_10;
        let x_4;
        const ln_12 = ln_11;
        x_4 = (ln_12.ToX - ln_12.FromX);
        let y_2;
        const ln_13 = ln_11;
        y_2 = (ln_13.ToY - ln_13.FromY);
        let z_2;
        const ln_14 = ln_11;
        z_2 = (ln_14.ToZ - ln_14.FromZ);
        const l_2 = Math.sqrt(((x_4 * x_4) + (y_2 * y_2)) + (z_2 * z_2));
        if (!(l_2 > 1E-12)) {
            failTooSmall("Line3D.ExtendEnd", ln_11);
        }
        ext_2 = Line3D_$ctor_76A78260_1(ln_11.FromX, ln_11.FromY, ln_11.FromZ, ln_11.ToX + ((x_4 * 5) / l_2), ln_11.ToY + ((y_2 * 5) / l_2), ln_11.ToZ + ((z_2 * 5) / l_2));
        expectEqualEpsilon(ext_2.FromX, 0, "start unchanged");
        expectEqualEpsilon(ext_2.ToX, 15, "extended end");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ExtendRel both ends", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ln_15 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let ext_3;
        const ln_16 = ln_15;
        let x_6;
        const ln_17 = ln_16;
        x_6 = (ln_17.ToX - ln_17.FromX);
        let y_3;
        const ln_18 = ln_16;
        y_3 = (ln_18.ToY - ln_18.FromY);
        let z_3;
        const ln_19 = ln_16;
        z_3 = (ln_19.ToZ - ln_19.FromZ);
        const l_3 = Math.sqrt(((x_6 * x_6) + (y_3 * y_3)) + (z_3 * z_3));
        if (!(l_3 > 1E-12)) {
            failTooSmall("Line3D.ExtendRel", ln_16);
        }
        ext_3 = Line3D_$ctor_76A78260_1(ln_16.FromX - (x_6 * 0.5), ln_16.FromY - (y_3 * 0.5), ln_16.FromZ - (z_3 * 0.5), ln_16.ToX + (x_6 * 0.5), ln_16.ToY + (y_3 * 0.5), ln_16.ToZ + (z_3 * 0.5));
        expectEqualEpsilon(ext_3.FromX, -5, "extended start by half");
        expectEqualEpsilon(ext_3.ToX, 15, "extended end by half");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Shrink both ends", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ln_20 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let shr;
        const ln_21 = ln_20;
        let x_8;
        const ln_22 = ln_21;
        x_8 = (ln_22.ToX - ln_22.FromX);
        let y_4;
        const ln_23 = ln_21;
        y_4 = (ln_23.ToY - ln_23.FromY);
        let z_4;
        const ln_24 = ln_21;
        z_4 = (ln_24.ToZ - ln_24.FromZ);
        const l_4 = Math.sqrt(((x_8 * x_8) + (y_4 * y_4)) + (z_4 * z_4));
        if (!(l_4 > 1E-12)) {
            failTooSmall("Line3D.Shrink", ln_21);
        }
        shr = Line3D_$ctor_76A78260_1(ln_21.FromX + ((x_8 * 2) / l_4), ln_21.FromY + ((y_4 * 2) / l_4), ln_21.FromZ + ((z_4 * 2) / l_4), ln_21.ToX - ((x_8 * 3) / l_4), ln_21.ToY - ((y_4 * 3) / l_4), ln_21.ToZ - ((z_4 * 3) / l_4));
        expectEqualEpsilon(shr.FromX, 2, "shrunk start");
        expectEqualEpsilon(shr.ToX, 7, "shrunk end");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ShrinkStart", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const ln_25 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let shr_1;
        const ln_26 = ln_25;
        let x_10;
        const ln_27 = ln_26;
        x_10 = (ln_27.ToX - ln_27.FromX);
        let y_5;
        const ln_28 = ln_26;
        y_5 = (ln_28.ToY - ln_28.FromY);
        let z_5;
        const ln_29 = ln_26;
        z_5 = (ln_29.ToZ - ln_29.FromZ);
        const l_5 = Math.sqrt(((x_10 * x_10) + (y_5 * y_5)) + (z_5 * z_5));
        if (!(l_5 > 1E-12)) {
            failTooSmall("Line3D.ShrinkStart", ln_26);
        }
        shr_1 = Line3D_$ctor_76A78260_1(ln_26.FromX + ((x_10 * 2) / l_5), ln_26.FromY + ((y_5 * 2) / l_5), ln_26.FromZ + ((z_5 * 2) / l_5), ln_26.ToX, ln_26.ToY, ln_26.ToZ);
        expectEqualEpsilon(shr_1.FromX, 2, "shrunk start");
        expectEqualEpsilon(shr_1.ToX, 10, "end unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ShrinkEnd", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const ln_30 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let shr_2;
        const ln_31 = ln_30;
        let x_12;
        const ln_32 = ln_31;
        x_12 = (ln_32.ToX - ln_32.FromX);
        let y_6;
        const ln_33 = ln_31;
        y_6 = (ln_33.ToY - ln_33.FromY);
        let z_6;
        const ln_34 = ln_31;
        z_6 = (ln_34.ToZ - ln_34.FromZ);
        const l_6 = Math.sqrt(((x_12 * x_12) + (y_6 * y_6)) + (z_6 * z_6));
        if (!(l_6 > 1E-12)) {
            failTooSmall("Line3D.ShrinkEnd", ln_31);
        }
        shr_2 = Line3D_$ctor_76A78260_1(ln_31.FromX, ln_31.FromY, ln_31.FromZ, ln_31.ToX - ((x_12 * 3) / l_6), ln_31.ToY - ((y_6 * 3) / l_6), ln_31.ToZ - ((z_6 * 3) / l_6));
        expectEqualEpsilon(shr_2.FromX, 0, "start unchanged");
        expectEqualEpsilon(shr_2.ToX, 7, "shrunk end");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})()]));

export const testsLine3DMove = Test_testList("Line3D Move/Transform", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Move by vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ln = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let moved;
        const ln_1 = ln;
        const v = Vec_$ctor_Z7AD9E565(5, 3, 2);
        moved = Line3D_$ctor_76A78260_1(ln_1.FromX + v.X, ln_1.FromY + v.Y, ln_1.FromZ + v.Z, ln_1.ToX + v.X, ln_1.ToY + v.Y, ln_1.ToZ + v.Z);
        expectEqualEpsilon(moved.FromX, 5, "from X moved");
        expectEqualEpsilon(moved.FromY, 3, "from Y moved");
        expectEqualEpsilon(moved.FromZ, 2, "from Z moved");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveX", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ln_2 = Line3D_$ctor_76A78260(0, 0, 0, 10, 5, 3);
        let moved_1;
        const ln_3 = ln_2;
        moved_1 = Line3D_$ctor_76A78260_1(ln_3.FromX + 7, ln_3.FromY, ln_3.FromZ, ln_3.ToX + 7, ln_3.ToY, ln_3.ToZ);
        expectEqualEpsilon(moved_1.FromX, 7, "from X moved");
        expectEqualEpsilon(moved_1.FromY, 0, "from Y unchanged");
        expectEqualEpsilon(moved_1.FromZ, 0, "from Z unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const ln_4 = Line3D_$ctor_76A78260(0, 0, 0, 10, 5, 3);
        let moved_2;
        const ln_5 = ln_4;
        moved_2 = Line3D_$ctor_76A78260_1(ln_5.FromX, ln_5.FromY + 4, ln_5.FromZ, ln_5.ToX, ln_5.ToY + 4, ln_5.ToZ);
        expectEqualEpsilon(moved_2.FromY, 4, "from Y moved");
        expectEqualEpsilon(moved_2.FromX, 0, "from X unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveZ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ln_6 = Line3D_$ctor_76A78260(0, 0, 0, 10, 5, 3);
        let moved_3;
        const ln_7 = ln_6;
        moved_3 = Line3D_$ctor_76A78260_1(ln_7.FromX, ln_7.FromY, ln_7.FromZ + 2, ln_7.ToX, ln_7.ToY, ln_7.ToZ + 2);
        expectEqualEpsilon(moved_3.FromZ, 2, "from Z moved");
        expectEqualEpsilon(moved_3.ToZ, 5, "to Z moved");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Scale from origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ln_8 = Line3D_$ctor_76A78260(1, 2, 3, 4, 5, 6);
        let scaled;
        const l = ln_8;
        scaled = Line3D_$ctor_76A78260_1(l.FromX * 2, l.FromY * 2, l.FromZ * 2, l.ToX * 2, l.ToY * 2, l.ToZ * 2);
        expectEqualEpsilon(scaled.FromX, 2, "from X scaled");
        expectEqualEpsilon(scaled.ToZ, 12, "to Z scaled");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate with quaternion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const ln_9 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const q = Quaternion_createFromRadians_Z3D1F83EE(Vec_$ctor_Z7AD9E565_1(0, 0, 1), 3.141592653589793 / 2);
        let rotated;
        const l_1 = ln_9;
        const q_1 = q;
        const u = l_1.FromX;
        const v_1 = l_1.FromY;
        const w = l_1.FromZ;
        const x = l_1.ToX;
        const y = l_1.ToY;
        const z = l_1.ToZ;
        const qx = q_1.X;
        const qy = q_1.Y;
        const qz = q_1.Z;
        const qw = q_1.W;
        const tu = 2 * ((qy * w) - (qz * v_1));
        const tv = 2 * ((qz * u) - (qx * w));
        const tw = 2 * ((qx * v_1) - (qy * u));
        const tx = 2 * ((qy * z) - (qz * y));
        const ty = 2 * ((qz * x) - (qx * z));
        const tz = 2 * ((qx * y) - (qy * x));
        rotated = Line3D_$ctor_76A78260_2(((u + (qw * tu)) + (qy * tw)) - (qz * tv), ((v_1 + (qw * tv)) + (qz * tu)) - (qx * tw), ((w + (qw * tw)) + (qx * tv)) - (qy * tu), ((x + (qw * tx)) + (qy * tz)) - (qz * ty), ((y + (qw * ty)) + (qz * tx)) - (qx * tz), ((z + (qw * tz)) + (qx * ty)) - (qy * tx));
        Expect_isTrue(Math.abs(rotated.ToX) < 1E-09)("rotated to X near zero");
        expectEqualEpsilon(rotated.ToY, 10, "rotated to Y near 10");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})()]));

export const testsLine3DClosestPoint = Test_testList("Line3D Closest Point", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RayClosestParameter on ray", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ln = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pt = Pnt_$ctor_Z7AD9E565_2(5, 5, 0);
        let param;
        const ln_1 = ln;
        const p = pt;
        const x = ln_1.FromX - ln_1.ToX;
        const y = ln_1.FromY - ln_1.ToY;
        const z = ln_1.FromZ - ln_1.ToZ;
        const lenSq = ((x * x) + (y * y)) + (z * z);
        if (!(lenSq > 1E-12)) {
            failTooSmall2("Line3D.RayClosestParameter", ln_1, p);
        }
        const u = ln_1.FromX - p.X;
        const v = ln_1.FromY - p.Y;
        const w = ln_1.FromZ - p.Z;
        const dot = ((x * u) + (y * v)) + (z * w);
        param = (dot / lenSq);
        expectEqualEpsilon(param, 0.5, "closest parameter");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestParameter clamped to 0", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ln_2 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pt_1 = Pnt_$ctor_Z7AD9E565_2(-5, 5, 0);
        let param_1;
        const ln_3 = ln_2;
        const p_1 = pt_1;
        const x_2 = ln_3.FromX - ln_3.ToX;
        const y_1 = ln_3.FromY - ln_3.ToY;
        const z_1 = ln_3.FromZ - ln_3.ToZ;
        const u_1 = ln_3.FromX - p_1.X;
        const v_1 = ln_3.FromY - p_1.Y;
        const w_1 = ln_3.FromZ - p_1.Z;
        const dot_1 = ((x_2 * u_1) + (y_1 * v_1)) + (z_1 * w_1);
        const lenSq_1 = ((x_2 * x_2) + (y_1 * y_1)) + (z_1 * z_1);
        if (!(lenSq_1 > 1E-12)) {
            param_1 = ((dot_1 < 0) ? 0 : 1);
        }
        else {
            const x_4 = dot_1 / lenSq_1;
            param_1 = ((x_4 > 0) ? ((x_4 < 1) ? x_4 : 1) : 0);
        }
        expectEqualEpsilon(param_1, 0, "clamped to 0");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestParameter clamped to 1", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const ln_4 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pt_2 = Pnt_$ctor_Z7AD9E565_2(15, 5, 0);
        let param_2;
        const ln_5 = ln_4;
        const p_2 = pt_2;
        const x_5 = ln_5.FromX - ln_5.ToX;
        const y_2 = ln_5.FromY - ln_5.ToY;
        const z_2 = ln_5.FromZ - ln_5.ToZ;
        const u_2 = ln_5.FromX - p_2.X;
        const v_2 = ln_5.FromY - p_2.Y;
        const w_2 = ln_5.FromZ - p_2.Z;
        const dot_2 = ((x_5 * u_2) + (y_2 * v_2)) + (z_2 * w_2);
        const lenSq_2 = ((x_5 * x_5) + (y_2 * y_2)) + (z_2 * z_2);
        if (!(lenSq_2 > 1E-12)) {
            param_2 = ((dot_2 < 0) ? 0 : 1);
        }
        else {
            const x_7 = dot_2 / lenSq_2;
            param_2 = ((x_7 > 0) ? ((x_7 < 1) ? x_7 : 1) : 0);
        }
        expectEqualEpsilon(param_2, 1, "clamped to 1");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestPoint on line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let ln_10, ln_11, ln_12;
        const ln_6 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pt_3 = Pnt_$ctor_Z7AD9E565_2(5, 5, 0);
        let cl;
        const ln_7 = ln_6;
        const ln_9 = ln_7;
        let p_5;
        const ln_8 = ln_7;
        const p_4 = pt_3;
        const x_8 = ln_8.FromX - ln_8.ToX;
        const y_3 = ln_8.FromY - ln_8.ToY;
        const z_3 = ln_8.FromZ - ln_8.ToZ;
        const u_3 = ln_8.FromX - p_4.X;
        const v_3 = ln_8.FromY - p_4.Y;
        const w_3 = ln_8.FromZ - p_4.Z;
        const dot_3 = ((x_8 * u_3) + (y_3 * v_3)) + (z_3 * w_3);
        const lenSq_3 = ((x_8 * x_8) + (y_3 * y_3)) + (z_3 * z_3);
        if (!(lenSq_3 > 1E-12)) {
            p_5 = ((dot_3 < 0) ? 0 : 1);
        }
        else {
            const x_10 = dot_3 / lenSq_3;
            p_5 = ((x_10 > 0) ? ((x_10 < 1) ? x_10 : 1) : 0);
        }
        cl = Pnt_$ctor_Z7AD9E565(ln_9.FromX + (((ln_10 = ln_9, ln_10.ToX - ln_10.FromX)) * p_5), ln_9.FromY + (((ln_11 = ln_9, ln_11.ToY - ln_11.FromY)) * p_5), ln_9.FromZ + (((ln_12 = ln_9, ln_12.ToZ - ln_12.FromZ)) * p_5));
        expectEqualEpsilon(cl.X, 5, "closest point X");
        expectEqualEpsilon(cl.Y, 0, "closest point Y");
        expectEqualEpsilon(cl.Z, 0, "closest point Z");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SqDistanceFromPoint", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let ln_17, ln_18, ln_19;
        const ln_13 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pt_4 = Pnt_$ctor_Z7AD9E565_2(5, 3, 4);
        let sqDist;
        const ln_14 = ln_13;
        const p_6 = pt_4;
        const a_4 = p_6;
        let b_7;
        const ln_16 = ln_14;
        let p_8;
        const ln_15 = ln_14;
        const p_7 = p_6;
        const x_11 = ln_15.FromX - ln_15.ToX;
        const y_4 = ln_15.FromY - ln_15.ToY;
        const z_4 = ln_15.FromZ - ln_15.ToZ;
        const u_4 = ln_15.FromX - p_7.X;
        const v_4 = ln_15.FromY - p_7.Y;
        const w_4 = ln_15.FromZ - p_7.Z;
        const dot_4 = ((x_11 * u_4) + (y_4 * v_4)) + (z_4 * w_4);
        const lenSq_4 = ((x_11 * x_11) + (y_4 * y_4)) + (z_4 * z_4);
        if (!(lenSq_4 > 1E-12)) {
            p_8 = ((dot_4 < 0) ? 0 : 1);
        }
        else {
            const x_13 = dot_4 / lenSq_4;
            p_8 = ((x_13 > 0) ? ((x_13 < 1) ? x_13 : 1) : 0);
        }
        b_7 = Pnt_$ctor_Z7AD9E565(ln_16.FromX + (((ln_17 = ln_16, ln_17.ToX - ln_17.FromX)) * p_8), ln_16.FromY + (((ln_18 = ln_16, ln_18.ToY - ln_18.FromY)) * p_8), ln_16.FromZ + (((ln_19 = ln_16, ln_19.ToZ - ln_19.FromZ)) * p_8));
        const x_14 = a_4.X - b_7.X;
        const y_5 = a_4.Y - b_7.Y;
        const z_5 = a_4.Z - b_7.Z;
        sqDist = (((x_14 * x_14) + (y_5 * y_5)) + (z_5 * z_5));
        expectEqualEpsilon(sqDist, 25, "squared distance is 25");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("DistanceToPnt", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let ln_25, ln_26, ln_27;
        const ln_20 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pt_5 = Pnt_$ctor_Z7AD9E565_2(5, 3, 4);
        let dist;
        let value;
        const ln_22 = ln_20;
        const p_10 = pt_5;
        const a_6 = p_10;
        let b_10;
        const ln_24 = ln_22;
        let p_12;
        const ln_23 = ln_22;
        const p_11 = p_10;
        const x_15 = ln_23.FromX - ln_23.ToX;
        const y_6 = ln_23.FromY - ln_23.ToY;
        const z_6 = ln_23.FromZ - ln_23.ToZ;
        const u_5 = ln_23.FromX - p_11.X;
        const v_5 = ln_23.FromY - p_11.Y;
        const w_5 = ln_23.FromZ - p_11.Z;
        const dot_5 = ((x_15 * u_5) + (y_6 * v_5)) + (z_6 * w_5);
        const lenSq_5 = ((x_15 * x_15) + (y_6 * y_6)) + (z_6 * z_6);
        if (!(lenSq_5 > 1E-12)) {
            p_12 = ((dot_5 < 0) ? 0 : 1);
        }
        else {
            const x_17 = dot_5 / lenSq_5;
            p_12 = ((x_17 > 0) ? ((x_17 < 1) ? x_17 : 1) : 0);
        }
        b_10 = Pnt_$ctor_Z7AD9E565(ln_24.FromX + (((ln_25 = ln_24, ln_25.ToX - ln_25.FromX)) * p_12), ln_24.FromY + (((ln_26 = ln_24, ln_26.ToY - ln_26.FromY)) * p_12), ln_24.FromZ + (((ln_27 = ln_24, ln_27.ToZ - ln_27.FromZ)) * p_12));
        const x_18 = a_6.X - b_10.X;
        const y_7 = a_6.Y - b_10.Y;
        const z_7 = a_6.Z - b_10.Z;
        value = (((x_18 * x_18) + (y_7 * y_7)) + (z_7 * z_7));
        dist = Math.sqrt(value);
        expectEqualEpsilon(dist, 5, "distance is 5");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})()]));

export const testsLine3DProjection = Test_testList("Line3D Projection Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const rayToProjectOnto = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject = Line3D_$ctor_76A78260(2, 5, 0, 8, 5, 0);
        const patternInput = Euclid_Line3D__Line3D_projectOntoRayParam_Static(rayToProjectOnto, lineToProject);
        const s = patternInput[0];
        const e = patternInput[1];
        expectEqualEpsilon(s, 0.2, "start parameter");
        expectEqualEpsilon(e, 0.8, "end parameter");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam opposite direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const rayToProjectOnto_2 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_2 = Line3D_$ctor_76A78260(8, 5, 0, 2, 5, 0);
        const patternInput_1 = Euclid_Line3D__Line3D_projectOntoRayParam_Static(rayToProjectOnto_2, lineToProject_2);
        const s_1 = patternInput_1[0];
        const e_1 = patternInput_1[1];
        expectEqualEpsilon(s_1, 0.8, "start parameter");
        expectEqualEpsilon(e_1, 0.2, "end parameter");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam beyond ray start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const rayToProjectOnto_4 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_4 = Line3D_$ctor_76A78260(-5, 5, 0, -2, 5, 0);
        const patternInput_2 = Euclid_Line3D__Line3D_projectOntoRayParam_Static(rayToProjectOnto_4, lineToProject_4);
        const s_2 = patternInput_2[0];
        const e_2 = patternInput_2[1];
        expectEqualEpsilon(s_2, -0.5, "start parameter is negative");
        expectEqualEpsilon(e_2, -0.2, "end parameter is negative");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam beyond ray end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const rayToProjectOnto_6 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_6 = Line3D_$ctor_76A78260(12, 5, 0, 15, 5, 0);
        const patternInput_3 = Euclid_Line3D__Line3D_projectOntoRayParam_Static(rayToProjectOnto_6, lineToProject_6);
        const s_3 = patternInput_3[0];
        const e_3 = patternInput_3[1];
        expectEqualEpsilon(s_3, 1.2, "start parameter beyond 1");
        expectEqualEpsilon(e_3, 1.5, "end parameter beyond 1");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam diagonal lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const rayToProjectOnto_8 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const lineToProject_8 = Line3D_$ctor_76A78260(5, 5, 5, 8, 8, 8);
        const patternInput_4 = Euclid_Line3D__Line3D_projectOntoRayParam_Static(rayToProjectOnto_8, lineToProject_8);
        const s_4 = patternInput_4[0];
        const e_4 = patternInput_4[1];
        expectEqualEpsilon(s_4, 0.5, "start parameter on diagonal");
        expectEqualEpsilon(e_4, 0.8, "end parameter on diagonal");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam perpendicular projection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const rayToProjectOnto_10 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_10 = Line3D_$ctor_76A78260(5, 0, 0, 5, 10, 0);
        const patternInput_5 = Euclid_Line3D__Line3D_projectOntoRayParam_Static(rayToProjectOnto_10, lineToProject_10);
        const s_5 = patternInput_5[0];
        const e_5 = patternInput_5[1];
        expectEqualEpsilon(s_5, 0.5, "both project to same parameter");
        expectEqualEpsilon(e_5, 0.5, "both project to same parameter");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRayParam too short ray throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const rayToProjectOnto_12 = Line3D_$ctor_76A78260(0, 0, 0, 1E-13, 0, 0);
        const lineToProject_12 = Line3D_$ctor_76A78260(5, 0, 0, 8, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_projectOntoRayParam_Static(rayToProjectOnto_12, lineToProject_12);
        }, "too short ray throws");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRay same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const rayToProjectOnto_14 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_14 = Line3D_$ctor_76A78260(2, 5, 0, 8, 5, 0);
        const projected = Euclid_Line3D__Line3D_projectOntoRay_Static(rayToProjectOnto_14, lineToProject_14);
        expectEqualEpsilon(projected.FromX, 2, "projected start X");
        expectEqualEpsilon(projected.ToX, 8, "projected end X");
        expectEqualEpsilon(projected.FromY, 0, "projected start Y");
        expectEqualEpsilon(projected.ToY, 0, "projected end Y");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRay opposite direction preserves orientation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const rayToProjectOnto_16 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_16 = Line3D_$ctor_76A78260(8, 5, 0, 2, 5, 0);
        const projected_1 = Euclid_Line3D__Line3D_projectOntoRay_Static(rayToProjectOnto_16, lineToProject_16);
        expectEqualEpsilon(projected_1.FromX, 8, "projected start X");
        expectEqualEpsilon(projected_1.ToX, 2, "projected end X");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("projectOntoRay 3D projection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let ln_1, ln_2, ln_3, ln_5, ln_6, ln_7;
        const rayToProjectOnto_18 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const lineToProject_18 = Line3D_$ctor_76A78260(5, 5, 0, 8, 8, 0);
        const projected_2 = Euclid_Line3D__Line3D_projectOntoRay_Static(rayToProjectOnto_18, lineToProject_18);
        const patternInput_6 = Euclid_Line3D__Line3D_projectOntoRayParam_Static(rayToProjectOnto_18, lineToProject_18);
        const s_6 = patternInput_6[0];
        const e_6 = patternInput_6[1];
        let expectedStart;
        const ln = rayToProjectOnto_18;
        const p = s_6;
        expectedStart = Pnt_$ctor_Z7AD9E565(ln.FromX + (((ln_1 = ln, ln_1.ToX - ln_1.FromX)) * p), ln.FromY + (((ln_2 = ln, ln_2.ToY - ln_2.FromY)) * p), ln.FromZ + (((ln_3 = ln, ln_3.ToZ - ln_3.FromZ)) * p));
        let expectedEnd;
        const ln_4 = rayToProjectOnto_18;
        const p_1 = e_6;
        expectedEnd = Pnt_$ctor_Z7AD9E565(ln_4.FromX + (((ln_5 = ln_4, ln_5.ToX - ln_5.FromX)) * p_1), ln_4.FromY + (((ln_6 = ln_4, ln_6.ToY - ln_6.FromY)) * p_1), ln_4.FromZ + (((ln_7 = ln_4, ln_7.ToZ - ln_7.FromZ)) * p_1));
        let dist1;
        let a_7;
        const ln_8 = projected_2;
        a_7 = Pnt_$ctor_Z7AD9E565_1(ln_8.FromX, ln_8.FromY, ln_8.FromZ);
        const b_19 = expectedStart;
        const x = a_7.X - b_19.X;
        const y = a_7.Y - b_19.Y;
        const z = a_7.Z - b_19.Z;
        dist1 = Math.sqrt(((x * x) + (y * y)) + (z * z));
        let dist2;
        let a_9;
        const ln_9 = projected_2;
        a_9 = Pnt_$ctor_Z7AD9E565_1(ln_9.ToX, ln_9.ToY, ln_9.ToZ);
        const b_21 = expectedEnd;
        const x_1 = a_9.X - b_21.X;
        const y_1 = a_9.Y - b_21.Y;
        const z_1 = a_9.Z - b_21.Z;
        dist2 = Math.sqrt(((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1));
        Expect_isTrue(dist1 < 1E-09)("projected start near expected");
        Expect_isTrue(dist2 < 1E-09)("projected end near expected");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam full overlap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const onToLine = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_21 = Line3D_$ctor_76A78260(2, 5, 0, 8, 5, 0);
        const result = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine, lineToProject_21);
        if (result == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
        else {
            const s_7 = result[0];
            const e_7 = result[1];
            expectEqualEpsilon(s_7, 0.2, "start parameter");
            expectEqualEpsilon(e_7, 0.8, "end parameter");
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam partial overlap at start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const onToLine_2 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_23 = Line3D_$ctor_76A78260(-2, 5, 0, 5, 5, 0);
        const result_1 = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine_2, lineToProject_23);
        if (result_1 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
        else {
            const s_8 = result_1[0];
            const e_8 = result_1[1];
            expectEqualEpsilon(s_8, 0, "start parameter clamped to 0");
            expectEqualEpsilon(e_8, 0.5, "end parameter");
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam partial overlap at end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const onToLine_4 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_25 = Line3D_$ctor_76A78260(5, 5, 0, 15, 5, 0);
        const result_2 = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine_4, lineToProject_25);
        if (result_2 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
        else {
            const s_9 = result_2[0];
            const e_9 = result_2[1];
            expectEqualEpsilon(s_9, 0.5, "start parameter");
            expectEqualEpsilon(e_9, 1, "end parameter clamped to 1");
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam no overlap before line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const onToLine_6 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_27 = Line3D_$ctor_76A78260(-10, 5, 0, -5, 5, 0);
        const result_3 = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine_6, lineToProject_27);
        Expect_isTrue(result_3 == null)("no overlap returns None");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam no overlap after line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const onToLine_8 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_29 = Line3D_$ctor_76A78260(15, 5, 0, 20, 5, 0);
        const result_4 = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine_8, lineToProject_29);
        Expect_isTrue(result_4 == null)("no overlap returns None");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam opposite direction overlap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const onToLine_10 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_31 = Line3D_$ctor_76A78260(8, 5, 0, 2, 5, 0);
        const result_5 = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine_10, lineToProject_31);
        if (result_5 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
        else {
            const s_10 = result_5[0];
            const e_10 = result_5[1];
            expectEqualEpsilon(s_10, 0.8, "start parameter from lineToProject start");
            expectEqualEpsilon(e_10, 0.2, "end parameter from lineToProject end");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam encompassing line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const onToLine_12 = Line3D_$ctor_76A78260(2, 0, 0, 8, 0, 0);
        const lineToProject_33 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        const result_6 = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine_12, lineToProject_33);
        if (result_6 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
        else {
            const s_11 = result_6[0];
            const e_11 = result_6[1];
            expectEqualEpsilon(s_11, 0, "start parameter clamped");
            expectEqualEpsilon(e_11, 1, "end parameter clamped");
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam zero length line to project", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const onToLine_14 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_35 = Line3D_$ctor_76A78260(5, 5, 0, 5, 5, 0);
        const result_7 = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine_14, lineToProject_35);
        if (result_7 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
        else {
            const s_12 = result_7[0];
            const e_12 = result_7[1];
            expectEqualEpsilon(s_12, 0.5, "both parameters equal");
            expectEqualEpsilon(e_12, 0.5, "both parameters equal");
            Test_TestCaseBuilder__Zero(builder$0040_17);
        }
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLineParam too short target line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const onToLine_16 = Line3D_$ctor_76A78260(0, 0, 0, 1E-13, 0, 0);
        const lineToProject_37 = Line3D_$ctor_76A78260(5, 0, 0, 8, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine_16, lineToProject_37);
        }, "too short target throws");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine full overlap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const onToLine_18 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_39 = Line3D_$ctor_76A78260(2, 5, 0, 8, 5, 0);
        const result_8 = Euclid_Line3D__Line3D_tryProjectOntoLine_Static(onToLine_18, lineToProject_39);
        if (result_8 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_19);
        }
        else {
            const projected_3 = result_8;
            expectEqualEpsilon(projected_3.FromX, 2, "projected start X");
            expectEqualEpsilon(projected_3.ToX, 8, "projected end X");
            expectEqualEpsilon(projected_3.FromY, 0, "projected Y is 0");
            Test_TestCaseBuilder__Zero(builder$0040_19);
        }
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine partial overlap preserves orientation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const onToLine_20 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_41 = Line3D_$ctor_76A78260(12, 5, 0, 5, 5, 0);
        const result_9 = Euclid_Line3D__Line3D_tryProjectOntoLine_Static(onToLine_20, lineToProject_41);
        if (result_9 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_20);
        }
        else {
            const projected_4 = result_9;
            expectEqualEpsilon(projected_4.FromX, 10, "projected starts at clamped end");
            expectEqualEpsilon(projected_4.ToX, 5, "projected ends inside");
            Test_TestCaseBuilder__Zero(builder$0040_20);
        }
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine no overlap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const onToLine_22 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_43 = Line3D_$ctor_76A78260(-10, 5, 0, -5, 5, 0);
        const result_10 = Euclid_Line3D__Line3D_tryProjectOntoLine_Static(onToLine_22, lineToProject_43);
        Expect_isTrue(result_10 == null)("no overlap returns None");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine diagonal 3D", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let ln_11, ln_12, ln_13, ln_15, ln_16, ln_17;
        const onToLine_24 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const lineToProject_45 = Line3D_$ctor_76A78260(3, 3, 0, 7, 7, 0);
        const result_11 = Euclid_Line3D__Line3D_tryProjectOntoLine_Static(onToLine_24, lineToProject_45);
        if (result_11 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_22);
        }
        else {
            const projected_5 = result_11;
            let patternInput_7;
            const matchValue = Euclid_Line3D__Line3D_tryProjectOntoLineParam_Static(onToLine_24, lineToProject_45);
            if (matchValue == null) {
                patternInput_7 = [0, 0];
            }
            else {
                const p_2 = matchValue;
                patternInput_7 = p_2;
            }
            const s_13 = patternInput_7[0];
            const e_13 = patternInput_7[1];
            let expectedStart_1;
            const ln_10 = onToLine_24;
            const p_3 = s_13;
            expectedStart_1 = Pnt_$ctor_Z7AD9E565(ln_10.FromX + (((ln_11 = ln_10, ln_11.ToX - ln_11.FromX)) * p_3), ln_10.FromY + (((ln_12 = ln_10, ln_12.ToY - ln_12.FromY)) * p_3), ln_10.FromZ + (((ln_13 = ln_10, ln_13.ToZ - ln_13.FromZ)) * p_3));
            let expectedEnd_1;
            const ln_14 = onToLine_24;
            const p_4 = e_13;
            expectedEnd_1 = Pnt_$ctor_Z7AD9E565(ln_14.FromX + (((ln_15 = ln_14, ln_15.ToX - ln_15.FromX)) * p_4), ln_14.FromY + (((ln_16 = ln_14, ln_16.ToY - ln_16.FromY)) * p_4), ln_14.FromZ + (((ln_17 = ln_14, ln_17.ToZ - ln_17.FromZ)) * p_4));
            let dist1_1;
            let a_16;
            const ln_18 = projected_5;
            a_16 = Pnt_$ctor_Z7AD9E565_1(ln_18.FromX, ln_18.FromY, ln_18.FromZ);
            const b_40 = expectedStart_1;
            const x_2 = a_16.X - b_40.X;
            const y_2 = a_16.Y - b_40.Y;
            const z_2 = a_16.Z - b_40.Z;
            dist1_1 = Math.sqrt(((x_2 * x_2) + (y_2 * y_2)) + (z_2 * z_2));
            let dist2_1;
            let a_18;
            const ln_19 = projected_5;
            a_18 = Pnt_$ctor_Z7AD9E565_1(ln_19.ToX, ln_19.ToY, ln_19.ToZ);
            const b_42 = expectedEnd_1;
            const x_3 = a_18.X - b_42.X;
            const y_3 = a_18.Y - b_42.Y;
            const z_3 = a_18.Z - b_42.Z;
            dist2_1 = Math.sqrt(((x_3 * x_3) + (y_3 * y_3)) + (z_3 * z_3));
            Expect_isTrue(dist1_1 < 1E-09)("projected start matches parameter");
            Expect_isTrue(dist2_1 < 1E-09)("projected end matches parameter");
            Test_TestCaseBuilder__Zero(builder$0040_22);
        }
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine perpendicular collapse", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let ln_20;
        const onToLine_27 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_48 = Line3D_$ctor_76A78260(5, 0, 0, 5, 10, 0);
        const result_12 = Euclid_Line3D__Line3D_tryProjectOntoLine_Static(onToLine_27, lineToProject_48);
        if (result_12 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_23);
        }
        else {
            const projected_6 = result_12;
            expectEqualEpsilon(projected_6.FromX, 5, "collapsed to point start X");
            expectEqualEpsilon(projected_6.ToX, 5, "collapsed to point end X");
            Expect_isTrue((ln_20 = projected_6, ((ln_20.ToX === ln_20.FromX) && (ln_20.ToY === ln_20.FromY)) && (ln_20.ToZ === ln_20.FromZ)))("zero length result");
            Test_TestCaseBuilder__Zero(builder$0040_23);
        }
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryProjectOntoLine within tolerance at start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        const onToLine_29 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lineToProject_50 = Line3D_$ctor_76A78260(10 - 1E-08, 5, 0, 15, 5, 0);
        const result_13 = Euclid_Line3D__Line3D_tryProjectOntoLine_Static(onToLine_29, lineToProject_50);
        Expect_isTrue(result_13 != null)("just within tolerance returns Some");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})()]));

export const testsLine3DDivide = Test_testList("Line3D Divide/Split Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide into 1 segment", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ln = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts = Euclid_Line3D__Line3D_divide_Static(1, ln);
        const actual_1 = pts.length | 0;
        if ((actual_1 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 2, "returns 2 points");
        }
        else {
            let valueType;
            let copyOfStruct = actual_1;
            valueType = int32_type;
            const primitiveTypes = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg;
            if (contains(valueType, primitiveTypes, {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            })) {
                const arg = int32ToString(2);
                const arg_1 = int32ToString(actual_1);
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("returns 2 points");
            }
            else {
                errorMsg = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_1)("returns 2 points");
            }
            throw new Exception(errorMsg);
        }
        expectEqualEpsilon(item(0, pts).X, 0, "first point is start");
        expectEqualEpsilon(item(1, pts).X, 10, "second point is end");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide into 5 segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ln_2 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_1 = Euclid_Line3D__Line3D_divide_Static(5, ln_2);
        const actual_3 = pts_1.length | 0;
        if ((actual_3 === 6) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 6, "returns 6 points");
        }
        else {
            let valueType_1;
            let copyOfStruct_1 = actual_3;
            valueType_1 = int32_type;
            const primitiveTypes_1 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_1;
            if (contains(valueType_1, primitiveTypes_1, {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            })) {
                const arg_6 = int32ToString(6);
                const arg_1_1 = int32ToString(actual_3);
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("returns 6 points");
            }
            else {
                errorMsg_1 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(6)(actual_3)("returns 6 points");
            }
            throw new Exception(errorMsg_1);
        }
        expectEqualEpsilon(item(0, pts_1).X, 0, "first point");
        expectEqualEpsilon(item(1, pts_1).X, 2, "second point");
        expectEqualEpsilon(item(2, pts_1).X, 4, "third point");
        expectEqualEpsilon(item(5, pts_1).X, 10, "last point");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide diagonal line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const ln_4 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const pts_2 = Euclid_Line3D__Line3D_divide_Static(2, ln_4);
        const actual_5 = pts_2.length | 0;
        if ((actual_5 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 3, "returns 3 points");
        }
        else {
            let valueType_2;
            let copyOfStruct_2 = actual_5;
            valueType_2 = int32_type;
            const primitiveTypes_2 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_2;
            if (contains(valueType_2, primitiveTypes_2, {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            })) {
                const arg_7 = int32ToString(3);
                const arg_1_2 = int32ToString(actual_5);
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("returns 3 points");
            }
            else {
                errorMsg_2 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_5)("returns 3 points");
            }
            throw new Exception(errorMsg_2);
        }
        expectEqualEpsilon(item(1, pts_2).X, 5, "midpoint X");
        expectEqualEpsilon(item(1, pts_2).Y, 5, "midpoint Y");
        expectEqualEpsilon(item(1, pts_2).Z, 5, "midpoint Z");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide with 0 segments throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ln_6 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_divide_Static(0, ln_6);
        }, "zero segments throws");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divide with negative segments throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ln_8 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_divide_Static(-1, ln_8);
        }, "negative segments throws");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength exactly fitting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const ln_10 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_3 = Euclid_Line3D__Line3D_divideMinLength_Static(2, ln_10);
        Expect_isTrue(pts_3.length >= 2)("returns points for segments respecting minLength");
        Expect_isTrue(pts_3.length >= 5)("at least 4 segments due to tolerance factor");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength with remainder", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const ln_12 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_4 = Euclid_Line3D__Line3D_divideMinLength_Static(3, ln_12);
        const actual_7 = pts_4.length | 0;
        if ((actual_7 === 4) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 4, "returns 4 points for 3 segments");
        }
        else {
            let valueType_3;
            let copyOfStruct_3 = actual_7;
            valueType_3 = int32_type;
            const primitiveTypes_3 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_3;
            if (contains(valueType_3, primitiveTypes_3, {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            })) {
                const arg_8 = int32ToString(4);
                const arg_1_3 = int32ToString(actual_7);
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("returns 4 points for 3 segments");
            }
            else {
                errorMsg_3 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(4)(actual_7)("returns 4 points for 3 segments");
            }
            throw new Exception(errorMsg_3);
        }
        expectEqualEpsilon(item(0, pts_4).X, 0, "first point");
        expectEqualEpsilon(item(3, pts_4).X, 10, "last point");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength too large throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const ln_14 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_divideMinLength_Static(11, ln_14);
        }, "minLength larger than line throws");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength very small creates many segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const ln_16 = Line3D_$ctor_76A78260(0, 0, 0, 1, 0, 0);
        const pts_5 = Euclid_Line3D__Line3D_divideMinLength_Static(0.1, ln_16);
        Expect_isTrue(pts_5.length >= 10)("creates many segments");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMinLength zero or negative throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const ln_18 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_divideMinLength_Static(0, ln_18);
        }, "zero minLength throws");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMaxLength exactly fitting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const ln_20 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_6 = Euclid_Line3D__Line3D_divideMaxLength_Static(5, ln_20);
        const actual_9 = pts_6.length | 0;
        if ((actual_9 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 3, "returns 3 points for 2 segments");
        }
        else {
            let valueType_4;
            let copyOfStruct_4 = actual_9;
            valueType_4 = int32_type;
            const primitiveTypes_4 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_4;
            if (contains(valueType_4, primitiveTypes_4, {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            })) {
                const arg_9 = int32ToString(3);
                const arg_1_4 = int32ToString(actual_9);
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("returns 3 points for 2 segments");
            }
            else {
                errorMsg_4 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_9)("returns 3 points for 2 segments");
            }
            throw new Exception(errorMsg_4);
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMaxLength with small remainder", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const ln_22 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_7 = Euclid_Line3D__Line3D_divideMaxLength_Static(3, ln_22);
        const actual_11 = pts_7.length | 0;
        if ((actual_11 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 5, "returns 5 points for 4 segments");
        }
        else {
            let valueType_5;
            let copyOfStruct_5 = actual_11;
            valueType_5 = int32_type;
            const primitiveTypes_5 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_5;
            if (contains(valueType_5, primitiveTypes_5, {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            })) {
                const arg_10 = int32ToString(5);
                const arg_1_5 = int32ToString(actual_11);
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("returns 5 points for 4 segments");
            }
            else {
                errorMsg_5 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_11)("returns 5 points for 4 segments");
            }
            throw new Exception(errorMsg_5);
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMaxLength very large returns endpoints", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const ln_24 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_8 = Euclid_Line3D__Line3D_divideMaxLength_Static(100, ln_24);
        const actual_13 = pts_8.length | 0;
        if ((actual_13 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 2, "returns 2 points");
        }
        else {
            let valueType_6;
            let copyOfStruct_6 = actual_13;
            valueType_6 = int32_type;
            const primitiveTypes_6 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_6;
            if (contains(valueType_6, primitiveTypes_6, {
                Equals: equals,
                GetHashCode: (x_6) => (structuralHash(x_6) | 0),
            })) {
                const arg_11 = int32ToString(2);
                const arg_1_6 = int32ToString(actual_13);
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("returns 2 points");
            }
            else {
                errorMsg_6 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_13)("returns 2 points");
            }
            throw new Exception(errorMsg_6);
        }
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideMaxLength zero or negative throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const ln_26 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_divideMaxLength_Static(0, ln_26);
        }, "zero maxLength throws");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with gap into 2 segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let ln_30, x_8, ln_31, y_8, ln_32, z, ln_33, ln_34, x_9, ln_35, y_9, ln_36, z_1, ln_37;
        const ln_28 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lns = Euclid_Line3D__Line3D_split_Static(2, 2, ln_28);
        const actual_15 = lns.length | 0;
        if ((actual_15 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, 2, "returns 2 lines");
        }
        else {
            let valueType_7;
            let copyOfStruct_7 = actual_15;
            valueType_7 = int32_type;
            const primitiveTypes_7 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_7;
            if (contains(valueType_7, primitiveTypes_7, {
                Equals: equals,
                GetHashCode: (x_7) => (structuralHash(x_7) | 0),
            })) {
                const arg_12 = int32ToString(2);
                const arg_1_7 = int32ToString(actual_15);
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("returns 2 lines");
            }
            else {
                errorMsg_7 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_15)("returns 2 lines");
            }
            throw new Exception(errorMsg_7);
        }
        expectEqualEpsilon((ln_30 = item(0, lns), (x_8 = ((ln_31 = ln_30, ln_31.ToX - ln_31.FromX)), (y_8 = ((ln_32 = ln_30, ln_32.ToY - ln_32.FromY)), (z = ((ln_33 = ln_30, ln_33.ToZ - ln_33.FromZ)), Math.sqrt(((x_8 * x_8) + (y_8 * y_8)) + (z * z)))))), 4, "first line length");
        expectEqualEpsilon((ln_34 = item(1, lns), (x_9 = ((ln_35 = ln_34, ln_35.ToX - ln_35.FromX)), (y_9 = ((ln_36 = ln_34, ln_36.ToY - ln_36.FromY)), (z_1 = ((ln_37 = ln_34, ln_37.ToZ - ln_37.FromZ)), Math.sqrt(((x_9 * x_9) + (y_9 * y_9)) + (z_1 * z_1)))))), 4, "second line length");
        expectEqualEpsilon(item(1, lns).FromX - item(0, lns).ToX, 2, "gap between lines");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with gap into 3 segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let ln_40, x_11, ln_41, y_11, ln_42, z_2, ln_43, ln_44, x_12, ln_45, y_12, ln_46, z_3, ln_47, ln_48, x_13, ln_49, y_13, ln_50, z_4, ln_51;
        const ln_38 = Line3D_$ctor_76A78260(0, 0, 0, 12, 0, 0);
        const lns_1 = Euclid_Line3D__Line3D_split_Static(1, 3, ln_38);
        const actual_17 = lns_1.length | 0;
        if ((actual_17 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_17, 3, "returns 3 lines");
        }
        else {
            let valueType_8;
            let copyOfStruct_8 = actual_17;
            valueType_8 = int32_type;
            const primitiveTypes_8 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_8;
            if (contains(valueType_8, primitiveTypes_8, {
                Equals: equals,
                GetHashCode: (x_10) => (structuralHash(x_10) | 0),
            })) {
                const arg_13 = int32ToString(3);
                const arg_1_8 = int32ToString(actual_17);
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("returns 3 lines");
            }
            else {
                errorMsg_8 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_17)("returns 3 lines");
            }
            throw new Exception(errorMsg_8);
        }
        expectEqualEpsilon((((ln_40 = item(0, lns_1), (x_11 = ((ln_41 = ln_40, ln_41.ToX - ln_41.FromX)), (y_11 = ((ln_42 = ln_40, ln_42.ToY - ln_42.FromY)), (z_2 = ((ln_43 = ln_40, ln_43.ToZ - ln_43.FromZ)), Math.sqrt(((x_11 * x_11) + (y_11 * y_11)) + (z_2 * z_2))))))) + ((ln_44 = item(1, lns_1), (x_12 = ((ln_45 = ln_44, ln_45.ToX - ln_45.FromX)), (y_12 = ((ln_46 = ln_44, ln_46.ToY - ln_46.FromY)), (z_3 = ((ln_47 = ln_44, ln_47.ToZ - ln_47.FromZ)), Math.sqrt(((x_12 * x_12) + (y_12 * y_12)) + (z_3 * z_3)))))))) + ((ln_48 = item(2, lns_1), (x_13 = ((ln_49 = ln_48, ln_49.ToX - ln_49.FromX)), (y_13 = ((ln_50 = ln_48, ln_50.ToY - ln_50.FromY)), (z_4 = ((ln_51 = ln_48, ln_51.ToZ - ln_51.FromZ)), Math.sqrt(((x_13 * x_13) + (y_13 * y_13)) + (z_4 * z_4))))))), 10, "total line length plus gaps");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with zero gap same as divide", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let ln_54, x_15, ln_55, y_15, ln_56, z_5, ln_57;
        const ln_52 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lns_2 = Euclid_Line3D__Line3D_split_Static(0, 5, ln_52);
        const actual_19 = lns_2.length | 0;
        if ((actual_19 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_19, 5, "returns 5 lines");
        }
        else {
            let valueType_9;
            let copyOfStruct_9 = actual_19;
            valueType_9 = int32_type;
            const primitiveTypes_9 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_9;
            if (contains(valueType_9, primitiveTypes_9, {
                Equals: equals,
                GetHashCode: (x_14) => (structuralHash(x_14) | 0),
            })) {
                const arg_14 = int32ToString(5);
                const arg_1_9 = int32ToString(actual_19);
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_14)(arg_1_9)("returns 5 lines");
            }
            else {
                errorMsg_9 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_19)("returns 5 lines");
            }
            throw new Exception(errorMsg_9);
        }
        expectEqualEpsilon((ln_54 = item(0, lns_2), (x_15 = ((ln_55 = ln_54, ln_55.ToX - ln_55.FromX)), (y_15 = ((ln_56 = ln_54, ln_56.ToY - ln_56.FromY)), (z_5 = ((ln_57 = ln_54, ln_57.ToZ - ln_57.FromZ)), Math.sqrt(((x_15 * x_15) + (y_15 * y_15)) + (z_5 * z_5)))))), 2, "each segment length");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with large gap returns empty", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const ln_58 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lns_3 = Euclid_Line3D__Line3D_split_Static(5, 3, ln_58);
        const actual_21 = lns_3.length | 0;
        if ((actual_21 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_21, 0, "returns empty array");
        }
        else {
            let valueType_10;
            let copyOfStruct_10 = actual_21;
            valueType_10 = int32_type;
            const primitiveTypes_10 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_10;
            if (contains(valueType_10, primitiveTypes_10, {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            })) {
                const arg_15 = int32ToString(0);
                const arg_1_10 = int32ToString(actual_21);
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_15)(arg_1_10)("returns empty array");
            }
            else {
                errorMsg_10 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_21)("returns empty array");
            }
            throw new Exception(errorMsg_10);
        }
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with 0 segments throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        const ln_60 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_split_Static(1, 0, ln_60);
        }, "zero segments throws");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("split with negative segments throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        const ln_62 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_split_Static(1, -1, ln_62);
        }, "negative segments throws");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("splitMinLength with gap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        const ln_64 = Line3D_$ctor_76A78260(0, 0, 0, 20, 0, 0);
        const lns_4 = Euclid_Line3D__Line3D_splitMinLength_Static(1, 3, ln_64);
        Expect_isTrue(lns_4.length > 0)("returns multiple lines");
        Expect_isTrue(lns_4.every((l) => {
            let ln_66, x_17, ln_67, y_17, ln_68, z_6, ln_69;
            return ((ln_66 = l, (x_17 = ((ln_67 = ln_66, ln_67.ToX - ln_67.FromX)), (y_17 = ((ln_68 = ln_66, ln_68.ToY - ln_68.FromY)), (z_6 = ((ln_69 = ln_66, ln_69.ToZ - ln_69.FromZ)), Math.sqrt(((x_17 * x_17) + (y_17 * y_17)) + (z_6 * z_6))))))) >= (3 - 1E-09);
        }))("each segment at least minLength");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("splitMinLength line too short throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        const ln_70 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_splitMinLength_Static(5, 20, ln_70);
        }, "line too short for minLength throws");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("splitMaxLength with gap", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        const ln_72 = Line3D_$ctor_76A78260(0, 0, 0, 20, 0, 0);
        const lns_5 = Euclid_Line3D__Line3D_splitMaxLength_Static(1, 5, ln_72);
        Expect_isTrue(lns_5.length > 0)("returns multiple lines");
        Expect_isTrue(lns_5.every((l_1) => {
            let ln_74, x_18, ln_75, y_18, ln_76, z_7, ln_77;
            return ((ln_74 = l_1, (x_18 = ((ln_75 = ln_74, ln_75.ToX - ln_75.FromX)), (y_18 = ((ln_76 = ln_74, ln_76.ToY - ln_76.FromY)), (z_7 = ((ln_77 = ln_74, ln_77.ToZ - ln_77.FromZ)), Math.sqrt(((x_18 * x_18) + (y_18 * y_18)) + (z_7 * z_7))))))) <= (5 + 1E-09);
        }))("each segment at most maxLength");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery basic", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        const ln_78 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_9 = Euclid_Line3D__Line3D_divideEvery_Static(2, ln_78);
        expectEqualEpsilon(item(0, pts_9).X, 0, "includes start");
        Expect_isTrue(pts_9.length > 2)("includes divisions");
        Expect_isTrue(item(pts_9.length - 1, pts_9).X >= 8)("includes end if remainder significant");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery exact fit includes end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        const ln_79 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_10 = Euclid_Line3D__Line3D_divideEvery_Static(5, ln_79);
        const actual_23 = pts_10.length | 0;
        if ((actual_23 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, 3, "includes start and end");
        }
        else {
            let valueType_11;
            let copyOfStruct_13 = actual_23;
            valueType_11 = int32_type;
            const primitiveTypes_11 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_11;
            if (contains(valueType_11, primitiveTypes_11, {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            })) {
                const arg_16 = int32ToString(3);
                const arg_1_11 = int32ToString(actual_23);
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_16)(arg_1_11)("includes start and end");
            }
            else {
                errorMsg_11 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_23)("includes start and end");
            }
            throw new Exception(errorMsg_11);
        }
        expectEqualEpsilon(item(2, pts_10).X, 10, "last point is end");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery with tiny remainder", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        const ln_80 = Line3D_$ctor_76A78260(0, 0, 0, 10.001, 0, 0);
        const pts_11 = Euclid_Line3D__Line3D_divideEvery_Static(5, ln_80);
        expectEqualEpsilon(item(pts_11.length - 1, pts_11).X, 10.001, "includes end due to 1% rule");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery with very tiny remainder includes end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        const ln_81 = Line3D_$ctor_76A78260(0, 0, 0, 10.0001, 0, 0);
        const pts_12 = Euclid_Line3D__Line3D_divideEvery_Static(5, ln_81);
        Expect_isTrue(item(pts_12.length - 1, pts_12).X >= 10)("includes end if remainder > 1%");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideInsideEvery excludes endpoints", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        const ln_82 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_13 = Euclid_Line3D__Line3D_divideInsideEvery_Static(2, ln_82);
        Expect_isTrue(item(0, pts_13).X > 0)("first point not at start");
        Expect_isTrue(item(pts_13.length - 1, pts_13).X < 10)("last point not at end");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideInsideEvery with 2 segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const ln_83 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const pts_14 = Euclid_Line3D__Line3D_divideInsideEvery_Static(5, ln_83);
        const actual_25 = pts_14.length | 0;
        if ((actual_25 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_25, 1, "one interior point");
        }
        else {
            let valueType_12;
            let copyOfStruct_19 = actual_25;
            valueType_12 = int32_type;
            const primitiveTypes_12 = ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]);
            let errorMsg_12;
            if (contains(valueType_12, primitiveTypes_12, {
                Equals: equals,
                GetHashCode: (x_20) => (structuralHash(x_20) | 0),
            })) {
                const arg_17 = int32ToString(1);
                const arg_1_12 = int32ToString(actual_25);
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_17)(arg_1_12)("one interior point");
            }
            else {
                errorMsg_12 = toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_25)("one interior point");
            }
            throw new Exception(errorMsg_12);
        }
        expectEqualEpsilon(item(0, pts_14).X, 5, "midpoint");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideInsideEvery very small distance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        const ln_84 = Line3D_$ctor_76A78260(0, 0, 0, 1, 0, 0);
        const pts_15 = Euclid_Line3D__Line3D_divideInsideEvery_Static(0.1, ln_84);
        Expect_isTrue(pts_15.length > 1)("multiple interior points");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("divideEvery distance larger than line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let a_21, b_21, ln_86, a_23, b_23, ln_87;
        const ln_85 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const ps = Euclid_Line3D__Line3D_divideEvery_Static(11, ln_85);
        Expect_isTrue((a_21 = item(0, ps), (b_21 = ((ln_86 = ln_85, Pnt_$ctor_Z7AD9E565_1(ln_86.FromX, ln_86.FromY, ln_86.FromZ))), ((Math.abs(a_21.X - b_21.X) <= 1E-12) && (Math.abs(a_21.Y - b_21.Y) <= 1E-12)) && (Math.abs(a_21.Z - b_21.Z) <= 1E-12))))("first point beyond line length");
        Expect_isTrue((a_23 = item(1, ps), (b_23 = ((ln_87 = ln_85, Pnt_$ctor_Z7AD9E565_1(ln_87.ToX, ln_87.ToY, ln_87.ToZ))), ((Math.abs(a_23.X - b_23.X) <= 1E-12) && (Math.abs(a_23.Y - b_23.Y) <= 1E-12)) && (Math.abs(a_23.Z - b_23.Z) <= 1E-12))))("second point beyond line length");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})()]));

export const testsLine3DOffset = Test_testList("Line3D Offset Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetXY positive amount", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const ln = Line3D_$ctor_76A78260(0, 0, 5, 10, 0, 5);
        const offset = Euclid_Line3D__Line3D_offsetXY_Static(2, ln);
        expectEqualEpsilon(offset.FromY, 2, "offset to left in XY plane");
        expectEqualEpsilon(offset.FromZ, 5, "Z unchanged");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetXY negative amount", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const ln_2 = Line3D_$ctor_76A78260(0, 0, 5, 10, 0, 5);
        const offset_1 = Euclid_Line3D__Line3D_offsetXY_Static(-2, ln_2);
        expectEqualEpsilon(offset_1.FromY, -2, "offset to right in XY plane");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetXY zero amount returns same line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let a_4, ln_6, b_4, ln_7, x, y, z;
        const ln_4 = Line3D_$ctor_76A78260(0, 0, 5, 10, 0, 5);
        const offset_2 = Euclid_Line3D__Line3D_offsetXY_Static(0, ln_4);
        Expect_isTrue(((a_4 = ((ln_6 = offset_2, Pnt_$ctor_Z7AD9E565_1(ln_6.FromX, ln_6.FromY, ln_6.FromZ))), (b_4 = ((ln_7 = ln_4, Pnt_$ctor_Z7AD9E565_1(ln_7.FromX, ln_7.FromY, ln_7.FromZ))), (x = (a_4.X - b_4.X), (y = (a_4.Y - b_4.Y), (z = (a_4.Z - b_4.Z), Math.sqrt(((x * x) + (y * y)) + (z * z)))))))) < 1E-09)("zero offset returns input");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetXY diagonal line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_6, v_1, x_4, y_3, z_3, l_1, f_1, b_6;
        const ln_8 = Line3D_$ctor_76A78260(0, 0, 5, 10, 10, 5);
        const offset_3 = Euclid_Line3D__Line3D_offsetXY_Static(1, ln_8);
        let expectedDir;
        const v = Vec_$ctor_Z7AD9E565(-1, 1, 0);
        const x_1 = v.X;
        const y_1 = v.Y;
        const z_1 = v.Z;
        const l = Math.sqrt(((x_1 * x_1) + (y_1 * y_1)) + (z_1 * z_1));
        if (!(l > 1E-12)) {
            failUnit3("Vec.Unitized", x_1, y_1, z_1);
        }
        const f = 1 / l;
        expectedDir = UnitVec_$ctor_Z7AD9E565(f * x_1, f * y_1, f * z_1);
        let actualOffset;
        let a_5;
        const ln_10 = offset_3;
        a_5 = Pnt_$ctor_Z7AD9E565_1(ln_10.FromX, ln_10.FromY, ln_10.FromZ);
        let b_5;
        const ln_11 = ln_8;
        b_5 = Pnt_$ctor_Z7AD9E565_1(ln_11.FromX, ln_11.FromY, ln_11.FromZ);
        actualOffset = Vec_$ctor_Z7AD9E565_2(a_5.X - b_5.X, a_5.Y - b_5.Y, a_5.Z - b_5.Z);
        expectEqualEpsilon((a_6 = ((v_1 = actualOffset, (x_4 = v_1.X, (y_3 = v_1.Y, (z_3 = v_1.Z, (l_1 = Math.sqrt(((x_4 * x_4) + (y_3 * y_3)) + (z_3 * z_3)), (!(l_1 > 1E-12) ? failUnit3("Vec.Unitized", x_4, y_3, z_3) : undefined, (f_1 = (1 / l_1), UnitVec_$ctor_Z7AD9E565(f_1 * x_4, f_1 * y_3, f_1 * z_3))))))))), (b_6 = expectedDir, ((a_6.X * b_6.X) + (a_6.Y * b_6.Y)) + (a_6.Z * b_6.Z))), 1, "offset perpendicular to XY projection");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetXY vertical line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const ln_12 = Line3D_$ctor_76A78260(0, 0, 0, 0, 0, 10);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_offsetXY_Static(1, ln_12);
        }, "vertical line throws");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetXY too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const ln_14 = Line3D_$ctor_76A78260(0, 0, 0, 1E-13, 0, 0);
        Expect_throws(() => {
            Euclid_Line3D__Line3D_offsetXY_Static(1, ln_14);
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset horizontal and normal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const ln_16 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const offset_4 = Euclid_Line3D__Line3D_offset_Static(2, 3, ln_16);
        expectEqualEpsilon(offset_4.FromY, -2, "horizontal offset (cross with Z-axis points to -Y)");
        expectEqualEpsilon(offset_4.FromZ, -3, "normal offset (cross with normHor points to -Z)");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset zero distances returns same line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_11, ln_18, b_11, ln_19, x_7, y_5, z_5;
        const ln_17 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const offset_5 = Euclid_Line3D__Line3D_offset_Static(0, 0, ln_17);
        Expect_isTrue(((a_11 = ((ln_18 = offset_5, Pnt_$ctor_Z7AD9E565_1(ln_18.FromX, ln_18.FromY, ln_18.FromZ))), (b_11 = ((ln_19 = ln_17, Pnt_$ctor_Z7AD9E565_1(ln_19.FromX, ln_19.FromY, ln_19.FromZ))), (x_7 = (a_11.X - b_11.X), (y_5 = (a_11.Y - b_11.Y), (z_5 = (a_11.Z - b_11.Z), Math.sqrt(((x_7 * x_7) + (y_5 * y_5)) + (z_5 * z_5)))))))) < 1E-09)("zero offsets return input");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset diagonal 3D line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let ln_21, x_8, ln_22, y_6, ln_23, z_6, ln_24, ln_25, x_9, ln_26, y_7, ln_27, z_7, ln_28, copyOfStruct, a_13, ln_29, b_13, ln_30, v_2;
        const ln_20 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const offset_6 = Euclid_Line3D__Line3D_offset_Static(1, 1, ln_20);
        expectEqualEpsilon((ln_21 = offset_6, (x_8 = ((ln_22 = ln_21, ln_22.ToX - ln_22.FromX)), (y_6 = ((ln_23 = ln_21, ln_23.ToY - ln_23.FromY)), (z_6 = ((ln_24 = ln_21, ln_24.ToZ - ln_24.FromZ)), Math.sqrt(((x_8 * x_8) + (y_6 * y_6)) + (z_6 * z_6)))))), (ln_25 = ln_20, (x_9 = ((ln_26 = ln_25, ln_26.ToX - ln_26.FromX)), (y_7 = ((ln_27 = ln_25, ln_27.ToY - ln_27.FromY)), (z_7 = ((ln_28 = ln_25, ln_28.ToZ - ln_28.FromZ)), Math.sqrt(((x_9 * x_9) + (y_7 * y_7)) + (z_7 * z_7)))))), "offset line length same");
        Expect_isTrue(((copyOfStruct = ((a_13 = ((ln_29 = offset_6, Pnt_$ctor_Z7AD9E565_1(ln_29.FromX, ln_29.FromY, ln_29.FromZ))), (b_13 = ((ln_30 = ln_20, Pnt_$ctor_Z7AD9E565_1(ln_30.FromX, ln_30.FromY, ln_30.FromZ))), Vec_$ctor_Z7AD9E565_2(a_13.X - b_13.X, a_13.Y - b_13.Y, a_13.Z - b_13.Z)))), (v_2 = copyOfStruct, Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z))))) > 0)("offset distance from original");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset very close points uses Z axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const ln_31 = Line3D_$ctor_76A78260(0, 0, 0, 1E-07, 0, 0);
        const offset_7 = Euclid_Line3D__Line3D_offset_Static(0, 1, ln_31);
        expectEqualEpsilon(offset_7.FromZ, 1, "uses Z axis as second direction");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})()]));

export const testsLine3DWithLength = Test_testList("Line3D WithLength Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromStart shorter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let ln_6, x_2, ln_7, y_1, ln_8, z_1, ln_9;
        const ln = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let shortened;
        const ln_2 = ln;
        let x;
        const ln_3 = ln_2;
        x = (ln_3.ToX - ln_3.FromX);
        let y;
        const ln_4 = ln_2;
        y = (ln_4.ToY - ln_4.FromY);
        let z;
        const ln_5 = ln_2;
        z = (ln_5.ToZ - ln_5.FromZ);
        const l = Math.sqrt(((x * x) + (y * y)) + (z * z));
        if (!(l > 1E-12)) {
            failTooSmall("Line3D.withLengthFromStart", ln_2);
        }
        const f = 5 / l;
        shortened = Line3D_$ctor_76A78260_1(ln_2.FromX, ln_2.FromY, ln_2.FromZ, ln_2.FromX + (x * f), ln_2.FromY + (y * f), ln_2.FromZ + (z * f));
        expectEqualEpsilon(shortened.FromX, 0, "start unchanged");
        expectEqualEpsilon(shortened.ToX, 5, "end at new length");
        expectEqualEpsilon((ln_6 = shortened, (x_2 = ((ln_7 = ln_6, ln_7.ToX - ln_7.FromX)), (y_1 = ((ln_8 = ln_6, ln_8.ToY - ln_8.FromY)), (z_1 = ((ln_9 = ln_6, ln_9.ToZ - ln_9.FromZ)), Math.sqrt(((x_2 * x_2) + (y_1 * y_1)) + (z_1 * z_1)))))), 5, "new length");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromStart longer", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let ln_16, x_5, ln_17, y_3, ln_18, z_3, ln_19;
        const ln_10 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let extended;
        const ln_12 = ln_10;
        let x_3;
        const ln_13 = ln_12;
        x_3 = (ln_13.ToX - ln_13.FromX);
        let y_2;
        const ln_14 = ln_12;
        y_2 = (ln_14.ToY - ln_14.FromY);
        let z_2;
        const ln_15 = ln_12;
        z_2 = (ln_15.ToZ - ln_15.FromZ);
        const l_1 = Math.sqrt(((x_3 * x_3) + (y_2 * y_2)) + (z_2 * z_2));
        if (!(l_1 > 1E-12)) {
            failTooSmall("Line3D.withLengthFromStart", ln_12);
        }
        const f_1 = 15 / l_1;
        extended = Line3D_$ctor_76A78260_1(ln_12.FromX, ln_12.FromY, ln_12.FromZ, ln_12.FromX + (x_3 * f_1), ln_12.FromY + (y_2 * f_1), ln_12.FromZ + (z_2 * f_1));
        expectEqualEpsilon(extended.FromX, 0, "start unchanged");
        expectEqualEpsilon(extended.ToX, 15, "end at new length");
        expectEqualEpsilon((ln_16 = extended, (x_5 = ((ln_17 = ln_16, ln_17.ToX - ln_17.FromX)), (y_3 = ((ln_18 = ln_16, ln_18.ToY - ln_18.FromY)), (z_3 = ((ln_19 = ln_16, ln_19.ToZ - ln_19.FromZ)), Math.sqrt(((x_5 * x_5) + (y_3 * y_3)) + (z_3 * z_3)))))), 15, "new length");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromStart diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let ln_26, x_8, ln_27, y_5, ln_28, z_5, ln_29;
        const ln_20 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const newLen = 5;
        let result;
        const ln_22 = ln_20;
        let x_6;
        const ln_23 = ln_22;
        x_6 = (ln_23.ToX - ln_23.FromX);
        let y_4;
        const ln_24 = ln_22;
        y_4 = (ln_24.ToY - ln_24.FromY);
        let z_4;
        const ln_25 = ln_22;
        z_4 = (ln_25.ToZ - ln_25.FromZ);
        const l_2 = Math.sqrt(((x_6 * x_6) + (y_4 * y_4)) + (z_4 * z_4));
        if (!(l_2 > 1E-12)) {
            failTooSmall("Line3D.withLengthFromStart", ln_22);
        }
        const f_2 = newLen / l_2;
        result = Line3D_$ctor_76A78260_1(ln_22.FromX, ln_22.FromY, ln_22.FromZ, ln_22.FromX + (x_6 * f_2), ln_22.FromY + (y_4 * f_2), ln_22.FromZ + (z_4 * f_2));
        expectEqualEpsilon((ln_26 = result, (x_8 = ((ln_27 = ln_26, ln_27.ToX - ln_27.FromX)), (y_5 = ((ln_28 = ln_26, ln_28.ToY - ln_28.FromY)), (z_5 = ((ln_29 = ln_26, ln_29.ToZ - ln_29.FromZ)), Math.sqrt(((x_8 * x_8) + (y_5 * y_5)) + (z_5 * z_5)))))), newLen, "new length correct");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromStart too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const ln_30 = Line3D_$ctor_76A78260(0, 0, 0, 1E-13, 0, 0);
        Expect_throws(() => {
            let ln_32, x_9, ln_33, y_6, ln_34, z_6, ln_35, l_3, f_3;
            (ln_32 = ln_30, (x_9 = ((ln_33 = ln_32, ln_33.ToX - ln_33.FromX)), (y_6 = ((ln_34 = ln_32, ln_34.ToY - ln_34.FromY)), (z_6 = ((ln_35 = ln_32, ln_35.ToZ - ln_35.FromZ)), (l_3 = Math.sqrt(((x_9 * x_9) + (y_6 * y_6)) + (z_6 * z_6)), (!(l_3 > 1E-12) ? failTooSmall("Line3D.withLengthFromStart", ln_32) : undefined, (f_3 = (5 / l_3), Line3D_$ctor_76A78260_1(ln_32.FromX, ln_32.FromY, ln_32.FromZ, ln_32.FromX + (x_9 * f_3), ln_32.FromY + (y_6 * f_3), ln_32.FromZ + (z_6 * f_3)))))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthToEnd shorter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let ln_39, x_13, ln_40, y_8, ln_41, z_8, ln_42;
        const ln_36 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let shortened_1;
        const ln_38 = ln_36;
        const x_11 = ln_38.FromX - ln_38.ToX;
        const y_7 = ln_38.FromY - ln_38.ToY;
        const z_7 = ln_38.FromZ - ln_38.ToZ;
        const l_4 = Math.sqrt(((x_11 * x_11) + (y_7 * y_7)) + (z_7 * z_7));
        if (!(l_4 > 1E-12)) {
            failTooSmall("Line3D.withLengthToEnd", ln_38);
        }
        const f_5 = 5 / l_4;
        shortened_1 = Line3D_$ctor_76A78260_1(ln_38.ToX + (x_11 * f_5), ln_38.ToY + (y_7 * f_5), ln_38.ToZ + (z_7 * f_5), ln_38.ToX, ln_38.ToY, ln_38.ToZ);
        expectEqualEpsilon(shortened_1.FromX, 5, "start moved");
        expectEqualEpsilon(shortened_1.ToX, 10, "end unchanged");
        expectEqualEpsilon((ln_39 = shortened_1, (x_13 = ((ln_40 = ln_39, ln_40.ToX - ln_40.FromX)), (y_8 = ((ln_41 = ln_39, ln_41.ToY - ln_41.FromY)), (z_8 = ((ln_42 = ln_39, ln_42.ToZ - ln_42.FromZ)), Math.sqrt(((x_13 * x_13) + (y_8 * y_8)) + (z_8 * z_8)))))), 5, "new length");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthToEnd longer", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let ln_46, x_16, ln_47, y_10, ln_48, z_10, ln_49;
        const ln_43 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let extended_1;
        const ln_45 = ln_43;
        const x_14 = ln_45.FromX - ln_45.ToX;
        const y_9 = ln_45.FromY - ln_45.ToY;
        const z_9 = ln_45.FromZ - ln_45.ToZ;
        const l_5 = Math.sqrt(((x_14 * x_14) + (y_9 * y_9)) + (z_9 * z_9));
        if (!(l_5 > 1E-12)) {
            failTooSmall("Line3D.withLengthToEnd", ln_45);
        }
        const f_6 = 15 / l_5;
        extended_1 = Line3D_$ctor_76A78260_1(ln_45.ToX + (x_14 * f_6), ln_45.ToY + (y_9 * f_6), ln_45.ToZ + (z_9 * f_6), ln_45.ToX, ln_45.ToY, ln_45.ToZ);
        expectEqualEpsilon(extended_1.FromX, -5, "start moved back");
        expectEqualEpsilon(extended_1.ToX, 10, "end unchanged");
        expectEqualEpsilon((ln_46 = extended_1, (x_16 = ((ln_47 = ln_46, ln_47.ToX - ln_47.FromX)), (y_10 = ((ln_48 = ln_46, ln_48.ToY - ln_48.FromY)), (z_10 = ((ln_49 = ln_46, ln_49.ToZ - ln_49.FromZ)), Math.sqrt(((x_16 * x_16) + (y_10 * y_10)) + (z_10 * z_10)))))), 15, "new length");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthToEnd too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const ln_50 = Line3D_$ctor_76A78260(0, 0, 0, 1E-13, 0, 0);
        Expect_throws(() => {
            let ln_52, x_17, y_11, z_11, l_6, f_7;
            (ln_52 = ln_50, (x_17 = (ln_52.FromX - ln_52.ToX), (y_11 = (ln_52.FromY - ln_52.ToY), (z_11 = (ln_52.FromZ - ln_52.ToZ), (l_6 = Math.sqrt(((x_17 * x_17) + (y_11 * y_11)) + (z_11 * z_11)), (!(l_6 > 1E-12) ? failTooSmall("Line3D.withLengthToEnd", ln_52) : undefined, (f_7 = (5 / l_6), Line3D_$ctor_76A78260_1(ln_52.ToX + (x_17 * f_7), ln_52.ToY + (y_11 * f_7), ln_52.ToZ + (z_11 * f_7), ln_52.ToX, ln_52.ToY, ln_52.ToZ))))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromMid shorter", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let ln_56, x_21, ln_57, y_13, ln_58, z_13, ln_59;
        const ln_53 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let shortened_2;
        const ln_55 = ln_53;
        const x_19 = ln_55.FromX - ln_55.ToX;
        const y_12 = ln_55.FromY - ln_55.ToY;
        const z_12 = ln_55.FromZ - ln_55.ToZ;
        const l_7 = Math.sqrt(((x_19 * x_19) + (y_12 * y_12)) + (z_12 * z_12));
        if (!(l_7 > 1E-12)) {
            failTooSmall("Line3D.withLengthFromMid", ln_55);
        }
        const f_9 = ((6 / l_7) + 1) * 0.5;
        shortened_2 = Line3D_$ctor_76A78260_1(ln_55.ToX + (x_19 * f_9), ln_55.ToY + (y_12 * f_9), ln_55.ToZ + (z_12 * f_9), ln_55.FromX - (x_19 * f_9), ln_55.FromY - (y_12 * f_9), ln_55.FromZ - (z_12 * f_9));
        expectEqualEpsilon(shortened_2.FromX, 2, "centered on midpoint start");
        expectEqualEpsilon(shortened_2.ToX, 8, "centered on midpoint end");
        expectEqualEpsilon((ln_56 = shortened_2, (x_21 = ((ln_57 = ln_56, ln_57.ToX - ln_57.FromX)), (y_13 = ((ln_58 = ln_56, ln_58.ToY - ln_58.FromY)), (z_13 = ((ln_59 = ln_56, ln_59.ToZ - ln_59.FromZ)), Math.sqrt(((x_21 * x_21) + (y_13 * y_13)) + (z_13 * z_13)))))), 6, "new length");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromMid longer", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let ln_63, x_24, ln_64, y_15, ln_65, z_15, ln_66;
        const ln_60 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let extended_2;
        const ln_62 = ln_60;
        const x_22 = ln_62.FromX - ln_62.ToX;
        const y_14 = ln_62.FromY - ln_62.ToY;
        const z_14 = ln_62.FromZ - ln_62.ToZ;
        const l_8 = Math.sqrt(((x_22 * x_22) + (y_14 * y_14)) + (z_14 * z_14));
        if (!(l_8 > 1E-12)) {
            failTooSmall("Line3D.withLengthFromMid", ln_62);
        }
        const f_10 = ((20 / l_8) + 1) * 0.5;
        extended_2 = Line3D_$ctor_76A78260_1(ln_62.ToX + (x_22 * f_10), ln_62.ToY + (y_14 * f_10), ln_62.ToZ + (z_14 * f_10), ln_62.FromX - (x_22 * f_10), ln_62.FromY - (y_14 * f_10), ln_62.FromZ - (z_14 * f_10));
        expectEqualEpsilon(extended_2.FromX, -5, "centered on midpoint start");
        expectEqualEpsilon(extended_2.ToX, 15, "centered on midpoint end");
        expectEqualEpsilon((ln_63 = extended_2, (x_24 = ((ln_64 = ln_63, ln_64.ToX - ln_64.FromX)), (y_15 = ((ln_65 = ln_63, ln_65.ToY - ln_65.FromY)), (z_15 = ((ln_66 = ln_63, ln_66.ToZ - ln_66.FromZ)), Math.sqrt(((x_24 * x_24) + (y_15 * y_15)) + (z_15 * z_15)))))), 20, "new length");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromMid preserves midpoint", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const ln_67 = Line3D_$ctor_76A78260(2, 3, 4, 12, 13, 14);
        let mid;
        const ln_68 = ln_67;
        mid = Pnt_$ctor_Z7AD9E565((ln_68.ToX + ln_68.FromX) * 0.5, (ln_68.ToY + ln_68.FromY) * 0.5, (ln_68.ToZ + ln_68.FromZ) * 0.5);
        let result_1;
        const ln_70 = ln_67;
        const x_25 = ln_70.FromX - ln_70.ToX;
        const y_16 = ln_70.FromY - ln_70.ToY;
        const z_16 = ln_70.FromZ - ln_70.ToZ;
        const l_9 = Math.sqrt(((x_25 * x_25) + (y_16 * y_16)) + (z_16 * z_16));
        if (!(l_9 > 1E-12)) {
            failTooSmall("Line3D.withLengthFromMid", ln_70);
        }
        const f_11 = ((20 / l_9) + 1) * 0.5;
        result_1 = Line3D_$ctor_76A78260_1(ln_70.ToX + (x_25 * f_11), ln_70.ToY + (y_16 * f_11), ln_70.ToZ + (z_16 * f_11), ln_70.FromX - (x_25 * f_11), ln_70.FromY - (y_16 * f_11), ln_70.FromZ - (z_16 * f_11));
        let newMid;
        const ln_71 = result_1;
        newMid = Pnt_$ctor_Z7AD9E565((ln_71.ToX + ln_71.FromX) * 0.5, (ln_71.ToY + ln_71.FromY) * 0.5, (ln_71.ToZ + ln_71.FromZ) * 0.5);
        expectEqualEpsilon(newMid.X, mid.X, "midpoint X preserved");
        expectEqualEpsilon(newMid.Y, mid.Y, "midpoint Y preserved");
        expectEqualEpsilon(newMid.Z, mid.Z, "midpoint Z preserved");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("withLengthFromMid too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const ln_72 = Line3D_$ctor_76A78260(0, 0, 0, 1E-13, 0, 0);
        Expect_throws(() => {
            let ln_74, x_27, y_17, z_17, l_10, f_12;
            (ln_74 = ln_72, (x_27 = (ln_74.FromX - ln_74.ToX), (y_17 = (ln_74.FromY - ln_74.ToY), (z_17 = (ln_74.FromZ - ln_74.ToZ), (l_10 = Math.sqrt(((x_27 * x_27) + (y_17 * y_17)) + (z_17 * z_17)), (!(l_10 > 1E-12) ? failTooSmall("Line3D.withLengthFromMid", ln_74) : undefined, (f_12 = (((5 / l_10) + 1) * 0.5), Line3D_$ctor_76A78260_1(ln_74.ToX + (x_27 * f_12), ln_74.ToY + (y_17 * f_12), ln_74.ToZ + (z_17 * f_12), ln_74.FromX - (x_27 * f_12), ln_74.FromY - (y_17 * f_12), ln_74.FromZ - (z_17 * f_12)))))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance positive", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const ln_75 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let pt;
        const ln_77 = ln_75;
        let x_29;
        const ln_78 = ln_77;
        x_29 = (ln_78.ToX - ln_78.FromX);
        let y_18;
        const ln_79 = ln_77;
        y_18 = (ln_79.ToY - ln_79.FromY);
        let z_18;
        const ln_80 = ln_77;
        z_18 = (ln_80.ToZ - ln_80.FromZ);
        const len_22 = Math.sqrt(((x_29 * x_29) + (y_18 * y_18)) + (z_18 * z_18));
        if (!(len_22 > 1E-12)) {
            failTooSmall("Line3D.pointAtDistance", ln_77);
        }
        const f_14 = 3 / len_22;
        pt = Pnt_$ctor_Z7AD9E565(ln_77.FromX + (x_29 * f_14), ln_77.FromY + (y_18 * f_14), ln_77.FromZ + (z_18 * f_14));
        expectEqualEpsilon(pt.X, 3, "point at distance 3");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance negative", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const ln_81 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let pt_1;
        const ln_83 = ln_81;
        let x_31;
        const ln_84 = ln_83;
        x_31 = (ln_84.ToX - ln_84.FromX);
        let y_19;
        const ln_85 = ln_83;
        y_19 = (ln_85.ToY - ln_85.FromY);
        let z_19;
        const ln_86 = ln_83;
        z_19 = (ln_86.ToZ - ln_86.FromZ);
        const len_23 = Math.sqrt(((x_31 * x_31) + (y_19 * y_19)) + (z_19 * z_19));
        if (!(len_23 > 1E-12)) {
            failTooSmall("Line3D.pointAtDistance", ln_83);
        }
        const f_15 = -3 / len_23;
        pt_1 = Pnt_$ctor_Z7AD9E565(ln_83.FromX + (x_31 * f_15), ln_83.FromY + (y_19 * f_15), ln_83.FromZ + (z_19 * f_15));
        expectEqualEpsilon(pt_1.X, -3, "point before start");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance beyond end", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const ln_87 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        let pt_2;
        const ln_89 = ln_87;
        let x_33;
        const ln_90 = ln_89;
        x_33 = (ln_90.ToX - ln_90.FromX);
        let y_20;
        const ln_91 = ln_89;
        y_20 = (ln_91.ToY - ln_91.FromY);
        let z_20;
        const ln_92 = ln_89;
        z_20 = (ln_92.ToZ - ln_92.FromZ);
        const len_24 = Math.sqrt(((x_33 * x_33) + (y_20 * y_20)) + (z_20 * z_20));
        if (!(len_24 > 1E-12)) {
            failTooSmall("Line3D.pointAtDistance", ln_89);
        }
        const f_16 = 15 / len_24;
        pt_2 = Pnt_$ctor_Z7AD9E565(ln_89.FromX + (x_33 * f_16), ln_89.FromY + (y_20 * f_16), ln_89.FromZ + (z_20 * f_16));
        expectEqualEpsilon(pt_2.X, 15, "point beyond end");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let ln_99, x_37, ln_100, y_22, ln_101, z_22, ln_102, a_26, b_25, ln_103, x_38, y_23, z_23;
        const ln_93 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        let pt_3;
        const dist_6 = ((ln_99 = ln_93, (x_37 = ((ln_100 = ln_99, ln_100.ToX - ln_100.FromX)), (y_22 = ((ln_101 = ln_99, ln_101.ToY - ln_101.FromY)), (z_22 = ((ln_102 = ln_99, ln_102.ToZ - ln_102.FromZ)), Math.sqrt(((x_37 * x_37) + (y_22 * y_22)) + (z_22 * z_22))))))) / 2;
        const ln_95 = ln_93;
        let x_35;
        const ln_96 = ln_95;
        x_35 = (ln_96.ToX - ln_96.FromX);
        let y_21;
        const ln_97 = ln_95;
        y_21 = (ln_97.ToY - ln_97.FromY);
        let z_21;
        const ln_98 = ln_95;
        z_21 = (ln_98.ToZ - ln_98.FromZ);
        const len_25 = Math.sqrt(((x_35 * x_35) + (y_21 * y_21)) + (z_21 * z_21));
        if (!(len_25 > 1E-12)) {
            failTooSmall("Line3D.pointAtDistance", ln_95);
        }
        const f_17 = dist_6 / len_25;
        pt_3 = Pnt_$ctor_Z7AD9E565(ln_95.FromX + (x_35 * f_17), ln_95.FromY + (y_21 * f_17), ln_95.FromZ + (z_21 * f_17));
        Expect_isTrue(((a_26 = pt_3, (b_25 = ((ln_103 = ln_93, Pnt_$ctor_Z7AD9E565((ln_103.ToX + ln_103.FromX) * 0.5, (ln_103.ToY + ln_103.FromY) * 0.5, (ln_103.ToZ + ln_103.FromZ) * 0.5))), (x_38 = (a_26.X - b_25.X), (y_23 = (a_26.Y - b_25.Y), (z_23 = (a_26.Z - b_25.Z), Math.sqrt(((x_38 * x_38) + (y_23 * y_23)) + (z_23 * z_23)))))))) < 1E-09)("midpoint");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("pointAtDistance too short line throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const ln_104 = Line3D_$ctor_76A78260(0, 0, 0, 1E-13, 0, 0);
        Expect_throws(() => {
            let ln_106, x_39, ln_107, y_24, ln_108, z_24, ln_109, len_26, f_18;
            (ln_106 = ln_104, (x_39 = ((ln_107 = ln_106, ln_107.ToX - ln_107.FromX)), (y_24 = ((ln_108 = ln_106, ln_108.ToY - ln_108.FromY)), (z_24 = ((ln_109 = ln_106, ln_109.ToZ - ln_109.FromZ)), (len_26 = Math.sqrt(((x_39 * x_39) + (y_24 * y_24)) + (z_24 * z_24)), (!(len_26 > 1E-12) ? failTooSmall("Line3D.pointAtDistance", ln_106) : undefined, (f_18 = (3 / len_26), Pnt_$ctor_Z7AD9E565(ln_106.FromX + (x_39 * f_18), ln_106.FromY + (y_24 * f_18), ln_106.FromZ + (z_24 * f_18)))))))));
        }, "too short throws");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})()]));

export const testsLine3DIntersection = Test_testList("Line3D Intersection Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersect crossing lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const lnA = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB = Line3D_$ctor_76A78260(5, -5, 0, 5, 5, 0);
        Expect_isTrue(Euclid_Line3D__Line3D_doIntersect_Static(lnA, lnB))("crossing lines intersect");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersect parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const lnA_2 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_2 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        Expect_isFalse(Euclid_Line3D__Line3D_doIntersect_Static(lnA_2, lnB_2))("parallel lines don\'t intersect");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersect skew lines close", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const lnA_4 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_4 = Line3D_$ctor_76A78260(5, 0, 1E-07, 5, 10, 1E-07);
        Expect_isTrue(Euclid_Line3D__Line3D_doIntersect_Static(lnA_4, lnB_4))("skew lines within tolerance intersect");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersect skew lines far", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const lnA_6 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_6 = Line3D_$ctor_76A78260(5, 0, 5, 5, 10, 5);
        Expect_isFalse(Euclid_Line3D__Line3D_doIntersect_Static(lnA_6, lnB_6))("skew lines beyond tolerance don\'t intersect");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersect non-intersecting within segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const lnA_8 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_8 = Line3D_$ctor_76A78260(15, -5, 0, 15, 5, 0);
        Expect_isFalse(Euclid_Line3D__Line3D_doIntersect_Static(lnA_8, lnB_8))("rays would intersect but segments don\'t");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doRaysIntersect extending beyond segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const lnA_10 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_10 = Line3D_$ctor_76A78260(15, -5, 0, 15, 5, 0);
        Expect_isTrue(Euclid_Line3D__Line3D_doRaysIntersect_Static(lnA_10, lnB_10))("rays intersect");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doRaysIntersect parallel rays", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const lnA_12 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_12 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        Expect_isFalse(Euclid_Line3D__Line3D_doRaysIntersect_Static(lnA_12, lnB_12))("parallel rays don\'t intersect");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersectOrOverlap touching endpoints", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const lnA_14 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_14 = Line3D_$ctor_76A78260(10, 0, 0, 20, 0, 0);
        Expect_isTrue(Euclid_Line3D__Line3D_doIntersectOrOverlap_Static(lnA_14, lnB_14))("touching at endpoints");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersectOrOverlap overlapping parallel", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const lnA_16 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_16 = Line3D_$ctor_76A78260(5, 0, 0, 15, 0, 0);
        Expect_isTrue(Euclid_Line3D__Line3D_doIntersectOrOverlap_Static(lnA_16, lnB_16))("overlapping collinear lines");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doIntersectOrOverlap zero length at same point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const lnA_18 = Line3D_$ctor_76A78260(5, 5, 5, 5, 5, 5);
        const lnB_18 = Line3D_$ctor_76A78260(5, 5, 5, 5, 5, 5);
        Expect_isTrue(Euclid_Line3D__Line3D_doIntersectOrOverlap_Static(lnA_18, lnB_18))("zero length at same location");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersect returns intersection point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const lnA_20 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_20 = Line3D_$ctor_76A78260(5, -5, 0, 5, 5, 0);
        const result = Euclid_Line3D__Line3D_tryIntersect_Static(lnA_20, lnB_20);
        if (result == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
        else {
            const pt = result;
            expectEqualEpsilon(pt.X, 5, "intersection at X=5");
            expectEqualEpsilon(pt.Y, 0, "intersection at Y=0");
            Test_TestCaseBuilder__Zero(builder$0040_10);
        }
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersect no intersection returns None", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const lnA_22 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_22 = Line3D_$ctor_76A78260(15, -5, 0, 15, 5, 0);
        const result_1 = Euclid_Line3D__Line3D_tryIntersect_Static(lnA_22, lnB_22);
        Expect_isTrue(result_1 == null)("no intersection returns None");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersectRay returns ray intersection", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const lnA_24 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_24 = Line3D_$ctor_76A78260(15, -5, 0, 15, 5, 0);
        const result_2 = Euclid_Line3D__Line3D_tryIntersectRay_Static(lnA_24, lnB_24);
        if (result_2 == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
        else {
            const pt_1 = result_2;
            expectEqualEpsilon(pt_1.X, 15, "ray intersection found");
            Test_TestCaseBuilder__Zero(builder$0040_12);
        }
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryIntersectRay parallel returns None", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const lnA_25 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_25 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        const result_3 = Euclid_Line3D__Line3D_tryIntersectRay_Static(lnA_25, lnB_25);
        Expect_isTrue(result_3 == null)("parallel rays return None");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTouchingEndsOf touching at ends", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const lnA_26 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_26 = Line3D_$ctor_76A78260(10, 0, 0, 20, 0, 0);
        const result_4 = XLine3D_getEndsTouching_Z4454D4C5(lnA_26, lnB_26, 1E-06);
        if (result_4.tag === 3) {
            Expect_isTrue(true)("correct end touching");
            Test_TestCaseBuilder__Zero(builder$0040_14);
        }
        else {
            Expect_isTrue(false)("wrong touching type");
            Test_TestCaseBuilder__Zero(builder$0040_14);
        }
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTouchingEndsOf not touching", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const lnA_27 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_27 = Line3D_$ctor_76A78260(15, 0, 0, 20, 0, 0);
        const result_5 = XLine3D_getEndsTouching_Z4454D4C5(lnA_27, lnB_27, 1E-06);
        if (result_5.tag === 0) {
            Expect_isTrue(true)("correctly not touching");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
        else {
            Expect_isTrue(false)("should not be touching");
            Test_TestCaseBuilder__Zero(builder$0040_15);
        }
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("isTouchingEndsOf identical", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        const lnA_28 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_28 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const result_6 = XLine3D_getEndsTouching_Z4454D4C5(lnA_28, lnB_28, 1E-06);
        if (result_6.tag === 5) {
            Expect_isTrue(true)("correctly identical");
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
        else {
            Expect_isTrue(false)("should be identical");
            Test_TestCaseBuilder__Zero(builder$0040_16);
        }
    }));
})()]));

export const testsLine3DDistance = Test_testList("Line3D Distance Methods", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("distanceToLine parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const lnA = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        const dist = Euclid_Line3D__Line3D_distanceToLine_Static(lnA, lnB);
        expectEqualEpsilon(dist, 5, "distance is 5");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("distanceToLine intersecting lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const lnA_2 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_2 = Line3D_$ctor_76A78260(5, -5, 0, 5, 5, 0);
        const dist_1 = Euclid_Line3D__Line3D_distanceToLine_Static(lnA_2, lnB_2);
        expectEqualEpsilon(dist_1, 0, "distance is 0");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("distanceToLine skew lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const lnA_4 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_4 = Line3D_$ctor_76A78260(0, 5, 2, 10, 5, 2);
        const dist_2 = Euclid_Line3D__Line3D_distanceToLine_Static(lnA_4, lnB_4);
        Expect_isTrue(Math.abs(dist_2 - Math.sqrt(29)) < 1E-09)("distance between skew lines");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("sqDistanceToLine matches distanceToLine", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const lnA_6 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_6 = Line3D_$ctor_76A78260(0, 3, 4, 10, 3, 4);
        const sqDist = Euclid_Line3D__Line3D_sqDistanceToLine_Static(lnA_6, lnB_6);
        const dist_3 = Euclid_Line3D__Line3D_distanceToLine_Static(lnA_6, lnB_6);
        expectEqualEpsilon(Math.sqrt(sqDist), dist_3, "squared distance matches");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPoints on intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let a_2, b_3, x, y, z;
        const lnA_9 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_9 = Line3D_$ctor_76A78260(5, -5, 0, 5, 5, 0);
        const patternInput = Euclid_Line3D__Line3D_closestPoints_Static(lnA_9, lnB_9);
        const ptB = patternInput[1];
        const ptA = patternInput[0];
        Expect_isTrue(((a_2 = ptA, (b_3 = ptB, (x = (a_2.X - b_3.X), (y = (a_2.Y - b_3.Y), (z = (a_2.Z - b_3.Z), Math.sqrt(((x * x) + (y * y)) + (z * z)))))))) < 1E-09)("closest points are same");
        expectEqualEpsilon(ptA.X, 5, "at intersection");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPoints on parallel", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const lnA_11 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_11 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        const patternInput_1 = Euclid_Line3D__Line3D_closestPoints_Static(lnA_11, lnB_11);
        const ptB_1 = patternInput_1[1];
        const ptA_1 = patternInput_1[0];
        expectEqualEpsilon(Math.abs(ptB_1.Y - ptA_1.Y), 5, "closest points Y distance");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestPoints on skew", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const lnA_13 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_13 = Line3D_$ctor_76A78260(5, 0, 2, 5, 10, 2);
        const patternInput_2 = Euclid_Line3D__Line3D_closestPoints_Static(lnA_13, lnB_13);
        const ptB_2 = patternInput_2[1];
        const ptA_2 = patternInput_2[0];
        expectEqualEpsilon(ptA_2.X, 5, "closest point A on lnA");
        expectEqualEpsilon(ptB_2.X, 5, "closest point B on lnB");
        expectEqualEpsilon(Math.abs(ptB_2.Z - ptA_2.Z), 2, "Z distance is 2");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closestParameters", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const lnA_15 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_15 = Line3D_$ctor_76A78260(5, -10, 0, 5, 10, 0);
        const patternInput_3 = Euclid_Line3D__Line3D_closestParameters_Static(lnA_15, lnB_15);
        const tB = patternInput_3[1];
        const tA = patternInput_3[0];
        expectEqualEpsilon(tA, 0.5, "parameter on A");
        expectEqualEpsilon(tB, 0.5, "parameter on B");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryGetOverlap overlapping collinear", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const lnA_17 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_17 = Line3D_$ctor_76A78260(5, 0, 0, 15, 0, 0);
        const result = Euclid_Line3D__Line3D_tryGetOverlap_Static(lnA_17, lnB_17);
        if (result == null) {
            Expect_isTrue(false)("expected Some but got None");
            Test_TestCaseBuilder__Zero(builder$0040_8);
        }
        else {
            const s = result[0];
            const e = result[1];
            expectEqualEpsilon(s, 0.5, "overlap start");
            expectEqualEpsilon(e, 1, "overlap end");
            Test_TestCaseBuilder__Zero(builder$0040_8);
        }
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryGetOverlap no overlap collinear", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        const lnA_19 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_19 = Line3D_$ctor_76A78260(15, 0, 0, 20, 0, 0);
        const result_1 = Euclid_Line3D__Line3D_tryGetOverlap_Static(lnA_19, lnB_19);
        Expect_isTrue(result_1 == null)("no overlap returns None");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryGetOverlap non-collinear returns None", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        const lnA_21 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_21 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        const result_2 = Euclid_Line3D__Line3D_tryGetOverlap_Static(lnA_21, lnB_21);
        Expect_isTrue(result_2 == null)("non-collinear returns None");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("tryGetOverlap touching at point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        const lnA_23 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const lnB_23 = Line3D_$ctor_76A78260(10, 0, 0, 20, 0, 0);
        const result_3 = Euclid_Line3D__Line3D_tryGetOverlap_Static(lnA_23, lnB_23);
        if (result_3 == null) {
            Expect_isTrue(false)("touching should return Some");
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
        else {
            const s_1 = result_3[0];
            const e_1 = result_3[1];
            expectEqualEpsilon(s_1, 1, "touching at end");
            expectEqualEpsilon(e_1, 1, "touching at end");
            Test_TestCaseBuilder__Zero(builder$0040_11);
        }
    }));
})()]));

export const testsFastParallel3D = Test_testList("Line3D Fast Parallel Tests", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast identical lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let ln, other, d1x, ln_1, d1y, ln_2, d1z, ln_3, d2x, ln_4, d2y, ln_5, d2z, ln_6, cx, cy, cz, crossMagSq;
        const a = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_isTrue((ln = a, (other = b, (d1x = ((ln_1 = ln, ln_1.ToX - ln_1.FromX)), (d1y = ((ln_2 = ln, ln_2.ToY - ln_2.FromY)), (d1z = ((ln_3 = ln, ln_3.ToZ - ln_3.FromZ)), (d2x = ((ln_4 = other, ln_4.ToX - ln_4.FromX)), (d2y = ((ln_5 = other, ln_5.ToY - ln_5.FromY)), (d2z = ((ln_6 = other, ln_6.ToZ - ln_6.FromZ)), (cx = ((d1y * d2z) - (d1z * d2y)), (cy = ((d1z * d2x) - (d1x * d2z)), (cz = ((d1x * d2y) - (d1y * d2x)), (crossMagSq = (((cx * cx) + (cy * cy)) + (cz * cz)), crossMagSq < 1E-12)))))))))))))("identical lines are parallel");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast same direction different lengths", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let ln_7, other_1, d1x_1, ln_8, d1y_1, ln_9, d1z_1, ln_10, d2x_1, ln_11, d2y_1, ln_12, d2z_1, ln_13, cx_1, cy_1, cz_1, crossMagSq_1;
        const a_1 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_1 = Line3D_$ctor_76A78260(0, 0, 0, 5, 0, 0);
        Expect_isTrue((ln_7 = a_1, (other_1 = b_1, (d1x_1 = ((ln_8 = ln_7, ln_8.ToX - ln_8.FromX)), (d1y_1 = ((ln_9 = ln_7, ln_9.ToY - ln_9.FromY)), (d1z_1 = ((ln_10 = ln_7, ln_10.ToZ - ln_10.FromZ)), (d2x_1 = ((ln_11 = other_1, ln_11.ToX - ln_11.FromX)), (d2y_1 = ((ln_12 = other_1, ln_12.ToY - ln_12.FromY)), (d2z_1 = ((ln_13 = other_1, ln_13.ToZ - ln_13.FromZ)), (cx_1 = ((d1y_1 * d2z_1) - (d1z_1 * d2y_1)), (cy_1 = ((d1z_1 * d2x_1) - (d1x_1 * d2z_1)), (cz_1 = ((d1x_1 * d2y_1) - (d1y_1 * d2x_1)), (crossMagSq_1 = (((cx_1 * cx_1) + (cy_1 * cy_1)) + (cz_1 * cz_1)), crossMagSq_1 < 1E-12)))))))))))))("same direction different lengths are parallel");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast opposite directions", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let ln_14, other_2, d1x_2, ln_15, d1y_2, ln_16, d1z_2, ln_17, d2x_2, ln_18, d2y_2, ln_19, d2z_2, ln_20, cx_2, cy_2, cz_2, crossMagSq_2;
        const a_2 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_2 = Line3D_$ctor_76A78260(10, 0, 0, 0, 0, 0);
        Expect_isTrue((ln_14 = a_2, (other_2 = b_2, (d1x_2 = ((ln_15 = ln_14, ln_15.ToX - ln_15.FromX)), (d1y_2 = ((ln_16 = ln_14, ln_16.ToY - ln_16.FromY)), (d1z_2 = ((ln_17 = ln_14, ln_17.ToZ - ln_17.FromZ)), (d2x_2 = ((ln_18 = other_2, ln_18.ToX - ln_18.FromX)), (d2y_2 = ((ln_19 = other_2, ln_19.ToY - ln_19.FromY)), (d2z_2 = ((ln_20 = other_2, ln_20.ToZ - ln_20.FromZ)), (cx_2 = ((d1y_2 * d2z_2) - (d1z_2 * d2y_2)), (cy_2 = ((d1z_2 * d2x_2) - (d1x_2 * d2z_2)), (cz_2 = ((d1x_2 * d2y_2) - (d1y_2 * d2x_2)), (crossMagSq_2 = (((cx_2 * cx_2) + (cy_2 * cy_2)) + (cz_2 * cz_2)), crossMagSq_2 < 1E-12)))))))))))))("opposite directions are parallel");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast parallel offset lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let ln_21, other_3, d1x_3, ln_22, d1y_3, ln_23, d1z_3, ln_24, d2x_3, ln_25, d2y_3, ln_26, d2z_3, ln_27, cx_3, cy_3, cz_3, crossMagSq_3;
        const a_3 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_3 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        Expect_isTrue((ln_21 = a_3, (other_3 = b_3, (d1x_3 = ((ln_22 = ln_21, ln_22.ToX - ln_22.FromX)), (d1y_3 = ((ln_23 = ln_21, ln_23.ToY - ln_23.FromY)), (d1z_3 = ((ln_24 = ln_21, ln_24.ToZ - ln_24.FromZ)), (d2x_3 = ((ln_25 = other_3, ln_25.ToX - ln_25.FromX)), (d2y_3 = ((ln_26 = other_3, ln_26.ToY - ln_26.FromY)), (d2z_3 = ((ln_27 = other_3, ln_27.ToZ - ln_27.FromZ)), (cx_3 = ((d1y_3 * d2z_3) - (d1z_3 * d2y_3)), (cy_3 = ((d1z_3 * d2x_3) - (d1x_3 * d2z_3)), (cz_3 = ((d1x_3 * d2y_3) - (d1y_3 * d2x_3)), (crossMagSq_3 = (((cx_3 * cx_3) + (cy_3 * cy_3)) + (cz_3 * cz_3)), crossMagSq_3 < 1E-12)))))))))))))("parallel offset lines are parallel");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast perpendicular lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let ln_28, other_4, d1x_4, ln_29, d1y_4, ln_30, d1z_4, ln_31, d2x_4, ln_32, d2y_4, ln_33, d2z_4, ln_34, cx_4, cy_4, cz_4, crossMagSq_4;
        const a_4 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_4 = Line3D_$ctor_76A78260(0, 0, 0, 0, 10, 0);
        Expect_isFalse((ln_28 = a_4, (other_4 = b_4, (d1x_4 = ((ln_29 = ln_28, ln_29.ToX - ln_29.FromX)), (d1y_4 = ((ln_30 = ln_28, ln_30.ToY - ln_30.FromY)), (d1z_4 = ((ln_31 = ln_28, ln_31.ToZ - ln_31.FromZ)), (d2x_4 = ((ln_32 = other_4, ln_32.ToX - ln_32.FromX)), (d2y_4 = ((ln_33 = other_4, ln_33.ToY - ln_33.FromY)), (d2z_4 = ((ln_34 = other_4, ln_34.ToZ - ln_34.FromZ)), (cx_4 = ((d1y_4 * d2z_4) - (d1z_4 * d2y_4)), (cy_4 = ((d1z_4 * d2x_4) - (d1x_4 * d2z_4)), (cz_4 = ((d1x_4 * d2y_4) - (d1y_4 * d2x_4)), (crossMagSq_4 = (((cx_4 * cx_4) + (cy_4 * cy_4)) + (cz_4 * cz_4)), crossMagSq_4 < 1E-12)))))))))))))("perpendicular lines are not parallel");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast nearly parallel within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        let ln_35, other_5, d1x_5, ln_36, d1y_5, ln_37, d1z_5, ln_38, d2x_5, ln_39, d2y_5, ln_40, d2z_5, ln_41, cx_5, cy_5, cz_5, crossMagSq_5;
        const a_5 = Line3D_$ctor_76A78260(0, 0, 0, 100, 0, 0);
        const b_5 = Line3D_$ctor_76A78260(0, 0, 0, 100, 5E-09, 0);
        Expect_isTrue((ln_35 = a_5, (other_5 = b_5, (d1x_5 = ((ln_36 = ln_35, ln_36.ToX - ln_36.FromX)), (d1y_5 = ((ln_37 = ln_35, ln_37.ToY - ln_37.FromY)), (d1z_5 = ((ln_38 = ln_35, ln_38.ToZ - ln_38.FromZ)), (d2x_5 = ((ln_39 = other_5, ln_39.ToX - ln_39.FromX)), (d2y_5 = ((ln_40 = other_5, ln_40.ToY - ln_40.FromY)), (d2z_5 = ((ln_41 = other_5, ln_41.ToZ - ln_41.FromZ)), (cx_5 = ((d1y_5 * d2z_5) - (d1z_5 * d2y_5)), (cy_5 = ((d1z_5 * d2x_5) - (d1x_5 * d2z_5)), (cz_5 = ((d1x_5 * d2y_5) - (d1y_5 * d2x_5)), (crossMagSq_5 = (((cx_5 * cx_5) + (cy_5 * cy_5)) + (cz_5 * cz_5)), crossMagSq_5 < 1E-12)))))))))))))("nearly parallel within default tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast nearly parallel outside tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let ln_42, other_6, d1x_6, ln_43, d1y_6, ln_44, d1z_6, ln_45, d2x_6, ln_46, d2y_6, ln_47, d2z_6, ln_48, cx_6, cy_6, cz_6, crossMagSq_6;
        const a_6 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_6 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0.5, 0);
        Expect_isFalse((ln_42 = a_6, (other_6 = b_6, (d1x_6 = ((ln_43 = ln_42, ln_43.ToX - ln_43.FromX)), (d1y_6 = ((ln_44 = ln_42, ln_44.ToY - ln_44.FromY)), (d1z_6 = ((ln_45 = ln_42, ln_45.ToZ - ln_45.FromZ)), (d2x_6 = ((ln_46 = other_6, ln_46.ToX - ln_46.FromX)), (d2y_6 = ((ln_47 = other_6, ln_47.ToY - ln_47.FromY)), (d2z_6 = ((ln_48 = other_6, ln_48.ToZ - ln_48.FromZ)), (cx_6 = ((d1y_6 * d2z_6) - (d1z_6 * d2y_6)), (cy_6 = ((d1z_6 * d2x_6) - (d1x_6 * d2z_6)), (cz_6 = ((d1x_6 * d2y_6) - (d1y_6 * d2x_6)), (crossMagSq_6 = (((cx_6 * cx_6) + (cy_6 * cy_6)) + (cz_6 * cz_6)), crossMagSq_6 < 1E-12)))))))))))))("nearly parallel outside tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast zero length lines returns true", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let ln_49, other_7, d1x_7, ln_50, d1y_7, ln_51, d1z_7, ln_52, d2x_7, ln_53, d2y_7, ln_54, d2z_7, ln_55, cx_7, cy_7, cz_7, crossMagSq_7;
        const a_7 = Line3D_$ctor_76A78260(0, 0, 0, 0, 0, 0);
        const b_7 = Line3D_$ctor_76A78260(5, 5, 5, 5, 5, 5);
        Expect_isTrue((ln_49 = a_7, (other_7 = b_7, (d1x_7 = ((ln_50 = ln_49, ln_50.ToX - ln_50.FromX)), (d1y_7 = ((ln_51 = ln_49, ln_51.ToY - ln_51.FromY)), (d1z_7 = ((ln_52 = ln_49, ln_52.ToZ - ln_52.FromZ)), (d2x_7 = ((ln_53 = other_7, ln_53.ToX - ln_53.FromX)), (d2y_7 = ((ln_54 = other_7, ln_54.ToY - ln_54.FromY)), (d2z_7 = ((ln_55 = other_7, ln_55.ToZ - ln_55.FromZ)), (cx_7 = ((d1y_7 * d2z_7) - (d1z_7 * d2y_7)), (cy_7 = ((d1z_7 * d2x_7) - (d1x_7 * d2z_7)), (cz_7 = ((d1x_7 * d2y_7) - (d1y_7 * d2x_7)), (crossMagSq_7 = (((cx_7 * cx_7) + (cy_7 * cy_7)) + (cz_7 * cz_7)), crossMagSq_7 < 1E-12)))))))))))))("zero length lines return true (as documented)");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast very short parallel lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let ln_56, other_8, d1x_8, ln_57, d1y_8, ln_58, d1z_8, ln_59, d2x_8, ln_60, d2y_8, ln_61, d2z_8, ln_62, cx_8, cy_8, cz_8, crossMagSq_8;
        const a_8 = Line3D_$ctor_76A78260(0, 0, 0, 1E-09, 0, 0);
        const b_8 = Line3D_$ctor_76A78260(0, 0, 0, 1E-10, 0, 0);
        Expect_isTrue((ln_56 = a_8, (other_8 = b_8, (d1x_8 = ((ln_57 = ln_56, ln_57.ToX - ln_57.FromX)), (d1y_8 = ((ln_58 = ln_56, ln_58.ToY - ln_58.FromY)), (d1z_8 = ((ln_59 = ln_56, ln_59.ToZ - ln_59.FromZ)), (d2x_8 = ((ln_60 = other_8, ln_60.ToX - ln_60.FromX)), (d2y_8 = ((ln_61 = other_8, ln_61.ToY - ln_61.FromY)), (d2z_8 = ((ln_62 = other_8, ln_62.ToZ - ln_62.FromZ)), (cx_8 = ((d1y_8 * d2z_8) - (d1z_8 * d2y_8)), (cy_8 = ((d1z_8 * d2x_8) - (d1x_8 * d2z_8)), (cz_8 = ((d1x_8 * d2y_8) - (d1y_8 * d2x_8)), (crossMagSq_8 = (((cx_8 * cx_8) + (cy_8 * cy_8)) + (cz_8 * cz_8)), crossMagSq_8 < 1E-12)))))))))))))("very short parallel lines are parallel");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast diagonal lines parallel", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let ln_63, other_9, d1x_9, ln_64, d1y_9, ln_65, d1z_9, ln_66, d2x_9, ln_67, d2y_9, ln_68, d2z_9, ln_69, cx_9, cy_9, cz_9, crossMagSq_9;
        const a_9 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const b_9 = Line3D_$ctor_76A78260(5, 5, 5, 15, 15, 15);
        Expect_isTrue((ln_63 = a_9, (other_9 = b_9, (d1x_9 = ((ln_64 = ln_63, ln_64.ToX - ln_64.FromX)), (d1y_9 = ((ln_65 = ln_63, ln_65.ToY - ln_65.FromY)), (d1z_9 = ((ln_66 = ln_63, ln_66.ToZ - ln_66.FromZ)), (d2x_9 = ((ln_67 = other_9, ln_67.ToX - ln_67.FromX)), (d2y_9 = ((ln_68 = other_9, ln_68.ToY - ln_68.FromY)), (d2z_9 = ((ln_69 = other_9, ln_69.ToZ - ln_69.FromZ)), (cx_9 = ((d1y_9 * d2z_9) - (d1z_9 * d2y_9)), (cy_9 = ((d1z_9 * d2x_9) - (d1x_9 * d2z_9)), (cz_9 = ((d1x_9 * d2y_9) - (d1y_9 * d2x_9)), (crossMagSq_9 = (((cx_9 * cx_9) + (cy_9 * cy_9)) + (cz_9 * cz_9)), crossMagSq_9 < 1E-12)))))))))))))("diagonal parallel lines are parallel");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelToFast custom tolerance strict", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let ln_70, other_10, d1x_10, ln_71, d1y_10, ln_72, d1z_10, ln_73, d2x_10, ln_74, d2y_10, ln_75, d2z_10, ln_76, cx_10, cy_10, cz_10, crossMagSq_10, ln_77, other_11, d1x_11, ln_78, d1y_11, ln_79, d1z_11, ln_80, d2x_11, ln_81, d2y_11, ln_82, d2z_11, ln_83, cx_11, cy_11, cz_11, crossMagSq_11;
        const a_10 = Line3D_$ctor_76A78260(0, 0, 0, 100, 0, 0);
        const b_10 = Line3D_$ctor_76A78260(0, 0, 0, 100, 2E-07, 0);
        Expect_isFalse((ln_70 = a_10, (other_10 = b_10, (d1x_10 = ((ln_71 = ln_70, ln_71.ToX - ln_71.FromX)), (d1y_10 = ((ln_72 = ln_70, ln_72.ToY - ln_72.FromY)), (d1z_10 = ((ln_73 = ln_70, ln_73.ToZ - ln_73.FromZ)), (d2x_10 = ((ln_74 = other_10, ln_74.ToX - ln_74.FromX)), (d2y_10 = ((ln_75 = other_10, ln_75.ToY - ln_75.FromY)), (d2z_10 = ((ln_76 = other_10, ln_76.ToZ - ln_76.FromZ)), (cx_10 = ((d1y_10 * d2z_10) - (d1z_10 * d2y_10)), (cy_10 = ((d1z_10 * d2x_10) - (d1x_10 * d2z_10)), (cz_10 = ((d1x_10 * d2y_10) - (d1y_10 * d2x_10)), (crossMagSq_10 = (((cx_10 * cx_10) + (cy_10 * cy_10)) + (cz_10 * cz_10)), crossMagSq_10 < 1E-12)))))))))))))("with default tolerance not parallel due to larger deviation");
        Expect_isTrue((ln_77 = a_10, (other_11 = b_10, (d1x_11 = ((ln_78 = ln_77, ln_78.ToX - ln_78.FromX)), (d1y_11 = ((ln_79 = ln_77, ln_79.ToY - ln_79.FromY)), (d1z_11 = ((ln_80 = ln_77, ln_80.ToZ - ln_80.FromZ)), (d2x_11 = ((ln_81 = other_11, ln_81.ToX - ln_81.FromX)), (d2y_11 = ((ln_82 = other_11, ln_82.ToY - ln_82.FromY)), (d2z_11 = ((ln_83 = other_11, ln_83.ToZ - ln_83.FromZ)), (cx_11 = ((d1y_11 * d2z_11) - (d1z_11 * d2y_11)), (cy_11 = ((d1z_11 * d2x_11) - (d1x_11 * d2z_11)), (cz_11 = ((d1x_11 * d2y_11) - (d1y_11 * d2x_11)), (crossMagSq_11 = (((cx_11 * cx_11) + (cy_11 * cy_11)) + (cz_11 * cz_11)), crossMagSq_11 < 1E-09)))))))))))))("with looser tolerance parallel");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast identical lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let ln_84, other_12, d1x_12, ln_85, d1y_12, ln_86, d1z_12, ln_87, d2x_12, ln_88, d2y_12, ln_89, d2z_12, ln_90, cx_12, cy_12, cz_12, crossMagSq_12, px, py, pz, cx_1_1, cy_1_1, cz_1_1, crossMagSq_1_1;
        const a_11 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_11 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_isTrue((ln_84 = a_11, (other_12 = b_11, (d1x_12 = ((ln_85 = ln_84, ln_85.ToX - ln_85.FromX)), (d1y_12 = ((ln_86 = ln_84, ln_86.ToY - ln_86.FromY)), (d1z_12 = ((ln_87 = ln_84, ln_87.ToZ - ln_87.FromZ)), (d2x_12 = ((ln_88 = other_12, ln_88.ToX - ln_88.FromX)), (d2y_12 = ((ln_89 = other_12, ln_89.ToY - ln_89.FromY)), (d2z_12 = ((ln_90 = other_12, ln_90.ToZ - ln_90.FromZ)), (cx_12 = ((d1y_12 * d2z_12) - (d1z_12 * d2y_12)), (cy_12 = ((d1z_12 * d2x_12) - (d1x_12 * d2z_12)), (cz_12 = ((d1x_12 * d2y_12) - (d1y_12 * d2x_12)), (crossMagSq_12 = (((cx_12 * cx_12) + (cy_12 * cy_12)) + (cz_12 * cz_12)), (crossMagSq_12 < 1E-12) && ((px = (ln_84.FromX - other_12.FromX), (py = (ln_84.FromY - other_12.FromY), (pz = (ln_84.FromZ - other_12.FromZ), (cx_1_1 = ((py * d2z_12) - (pz * d2y_12)), (cy_1_1 = ((pz * d2x_12) - (px * d2z_12)), (cz_1_1 = ((px * d2y_12) - (py * d2x_12)), (crossMagSq_1_1 = (((cx_1_1 * cx_1_1) + (cy_1_1 * cy_1_1)) + (cz_1_1 * cz_1_1)), crossMagSq_1_1 < 1E-12)))))))))))))))))))))("identical lines are coincident");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast same ray different lengths", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let ln_91, other_13, d1x_13, ln_92, d1y_13, ln_93, d1z_13, ln_94, d2x_13, ln_95, d2y_13, ln_96, d2z_13, ln_97, cx_13, cy_13, cz_13, crossMagSq_13, px_1, py_1, pz_1, cx_1_2, cy_1_2, cz_1_2, crossMagSq_1_2;
        const a_12 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_12 = Line3D_$ctor_76A78260(0, 0, 0, 5, 0, 0);
        Expect_isTrue((ln_91 = a_12, (other_13 = b_12, (d1x_13 = ((ln_92 = ln_91, ln_92.ToX - ln_92.FromX)), (d1y_13 = ((ln_93 = ln_91, ln_93.ToY - ln_93.FromY)), (d1z_13 = ((ln_94 = ln_91, ln_94.ToZ - ln_94.FromZ)), (d2x_13 = ((ln_95 = other_13, ln_95.ToX - ln_95.FromX)), (d2y_13 = ((ln_96 = other_13, ln_96.ToY - ln_96.FromY)), (d2z_13 = ((ln_97 = other_13, ln_97.ToZ - ln_97.FromZ)), (cx_13 = ((d1y_13 * d2z_13) - (d1z_13 * d2y_13)), (cy_13 = ((d1z_13 * d2x_13) - (d1x_13 * d2z_13)), (cz_13 = ((d1x_13 * d2y_13) - (d1y_13 * d2x_13)), (crossMagSq_13 = (((cx_13 * cx_13) + (cy_13 * cy_13)) + (cz_13 * cz_13)), (crossMagSq_13 < 1E-12) && ((px_1 = (ln_91.FromX - other_13.FromX), (py_1 = (ln_91.FromY - other_13.FromY), (pz_1 = (ln_91.FromZ - other_13.FromZ), (cx_1_2 = ((py_1 * d2z_13) - (pz_1 * d2y_13)), (cy_1_2 = ((pz_1 * d2x_13) - (px_1 * d2z_13)), (cz_1_2 = ((px_1 * d2y_13) - (py_1 * d2x_13)), (crossMagSq_1_2 = (((cx_1_2 * cx_1_2) + (cy_1_2 * cy_1_2)) + (cz_1_2 * cz_1_2)), crossMagSq_1_2 < 1E-12)))))))))))))))))))))("same ray different lengths are coincident");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast same ray offset start", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let ln_98, other_14, d1x_14, ln_99, d1y_14, ln_100, d1z_14, ln_101, d2x_14, ln_102, d2y_14, ln_103, d2z_14, ln_104, cx_14, cy_14, cz_14, crossMagSq_14, px_2, py_2, pz_2, cx_1_3, cy_1_3, cz_1_3, crossMagSq_1_3;
        const a_13 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_13 = Line3D_$ctor_76A78260(5, 0, 0, 15, 0, 0);
        Expect_isTrue((ln_98 = a_13, (other_14 = b_13, (d1x_14 = ((ln_99 = ln_98, ln_99.ToX - ln_99.FromX)), (d1y_14 = ((ln_100 = ln_98, ln_100.ToY - ln_100.FromY)), (d1z_14 = ((ln_101 = ln_98, ln_101.ToZ - ln_101.FromZ)), (d2x_14 = ((ln_102 = other_14, ln_102.ToX - ln_102.FromX)), (d2y_14 = ((ln_103 = other_14, ln_103.ToY - ln_103.FromY)), (d2z_14 = ((ln_104 = other_14, ln_104.ToZ - ln_104.FromZ)), (cx_14 = ((d1y_14 * d2z_14) - (d1z_14 * d2y_14)), (cy_14 = ((d1z_14 * d2x_14) - (d1x_14 * d2z_14)), (cz_14 = ((d1x_14 * d2y_14) - (d1y_14 * d2x_14)), (crossMagSq_14 = (((cx_14 * cx_14) + (cy_14 * cy_14)) + (cz_14 * cz_14)), (crossMagSq_14 < 1E-12) && ((px_2 = (ln_98.FromX - other_14.FromX), (py_2 = (ln_98.FromY - other_14.FromY), (pz_2 = (ln_98.FromZ - other_14.FromZ), (cx_1_3 = ((py_2 * d2z_14) - (pz_2 * d2y_14)), (cy_1_3 = ((pz_2 * d2x_14) - (px_2 * d2z_14)), (cz_1_3 = ((px_2 * d2y_14) - (py_2 * d2x_14)), (crossMagSq_1_3 = (((cx_1_3 * cx_1_3) + (cy_1_3 * cy_1_3)) + (cz_1_3 * cz_1_3)), crossMagSq_1_3 < 1E-12)))))))))))))))))))))("same ray offset start are coincident");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast parallel but offset", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let ln_105, other_15, d1x_15, ln_106, d1y_15, ln_107, d1z_15, ln_108, d2x_15, ln_109, d2y_15, ln_110, d2z_15, ln_111, cx_15, cy_15, cz_15, crossMagSq_15, px_3, py_3, pz_3, cx_1_4, cy_1_4, cz_1_4, crossMagSq_1_4;
        const a_14 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_14 = Line3D_$ctor_76A78260(0, 2, 0, 10, 2, 0);
        Expect_isFalse((ln_105 = a_14, (other_15 = b_14, (d1x_15 = ((ln_106 = ln_105, ln_106.ToX - ln_106.FromX)), (d1y_15 = ((ln_107 = ln_105, ln_107.ToY - ln_107.FromY)), (d1z_15 = ((ln_108 = ln_105, ln_108.ToZ - ln_108.FromZ)), (d2x_15 = ((ln_109 = other_15, ln_109.ToX - ln_109.FromX)), (d2y_15 = ((ln_110 = other_15, ln_110.ToY - ln_110.FromY)), (d2z_15 = ((ln_111 = other_15, ln_111.ToZ - ln_111.FromZ)), (cx_15 = ((d1y_15 * d2z_15) - (d1z_15 * d2y_15)), (cy_15 = ((d1z_15 * d2x_15) - (d1x_15 * d2z_15)), (cz_15 = ((d1x_15 * d2y_15) - (d1y_15 * d2x_15)), (crossMagSq_15 = (((cx_15 * cx_15) + (cy_15 * cy_15)) + (cz_15 * cz_15)), (crossMagSq_15 < 1E-12) && ((px_3 = (ln_105.FromX - other_15.FromX), (py_3 = (ln_105.FromY - other_15.FromY), (pz_3 = (ln_105.FromZ - other_15.FromZ), (cx_1_4 = ((py_3 * d2z_15) - (pz_3 * d2y_15)), (cy_1_4 = ((pz_3 * d2x_15) - (px_3 * d2z_15)), (cz_1_4 = ((px_3 * d2y_15) - (py_3 * d2x_15)), (crossMagSq_1_4 = (((cx_1_4 * cx_1_4) + (cy_1_4 * cy_1_4)) + (cz_1_4 * cz_1_4)), crossMagSq_1_4 < 1E-12)))))))))))))))))))))("parallel lines with offset should not be coincident");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast opposite directions on same ray", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let ln_112, other_16, d1x_16, ln_113, d1y_16, ln_114, d1z_16, ln_115, d2x_16, ln_116, d2y_16, ln_117, d2z_16, ln_118, cx_16, cy_16, cz_16, crossMagSq_16, px_4, py_4, pz_4, cx_1_5, cy_1_5, cz_1_5, crossMagSq_1_5;
        const a_15 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_15 = Line3D_$ctor_76A78260(10, 0, 0, 0, 0, 0);
        Expect_isTrue((ln_112 = a_15, (other_16 = b_15, (d1x_16 = ((ln_113 = ln_112, ln_113.ToX - ln_113.FromX)), (d1y_16 = ((ln_114 = ln_112, ln_114.ToY - ln_114.FromY)), (d1z_16 = ((ln_115 = ln_112, ln_115.ToZ - ln_115.FromZ)), (d2x_16 = ((ln_116 = other_16, ln_116.ToX - ln_116.FromX)), (d2y_16 = ((ln_117 = other_16, ln_117.ToY - ln_117.FromY)), (d2z_16 = ((ln_118 = other_16, ln_118.ToZ - ln_118.FromZ)), (cx_16 = ((d1y_16 * d2z_16) - (d1z_16 * d2y_16)), (cy_16 = ((d1z_16 * d2x_16) - (d1x_16 * d2z_16)), (cz_16 = ((d1x_16 * d2y_16) - (d1y_16 * d2x_16)), (crossMagSq_16 = (((cx_16 * cx_16) + (cy_16 * cy_16)) + (cz_16 * cz_16)), (crossMagSq_16 < 1E-12) && ((px_4 = (ln_112.FromX - other_16.FromX), (py_4 = (ln_112.FromY - other_16.FromY), (pz_4 = (ln_112.FromZ - other_16.FromZ), (cx_1_5 = ((py_4 * d2z_16) - (pz_4 * d2y_16)), (cy_1_5 = ((pz_4 * d2x_16) - (px_4 * d2z_16)), (cz_1_5 = ((px_4 * d2y_16) - (py_4 * d2x_16)), (crossMagSq_1_5 = (((cx_1_5 * cx_1_5) + (cy_1_5 * cy_1_5)) + (cz_1_5 * cz_1_5)), crossMagSq_1_5 < 1E-12)))))))))))))))))))))("opposite directions on same ray are coincident");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast diagonal lines same ray", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let ln_119, other_17, d1x_17, ln_120, d1y_17, ln_121, d1z_17, ln_122, d2x_17, ln_123, d2y_17, ln_124, d2z_17, ln_125, cx_17, cy_17, cz_17, crossMagSq_17, px_5, py_5, pz_5, cx_1_6, cy_1_6, cz_1_6, crossMagSq_1_6;
        const a_16 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const b_16 = Line3D_$ctor_76A78260(5, 5, 5, 15, 15, 15);
        Expect_isTrue((ln_119 = a_16, (other_17 = b_16, (d1x_17 = ((ln_120 = ln_119, ln_120.ToX - ln_120.FromX)), (d1y_17 = ((ln_121 = ln_119, ln_121.ToY - ln_121.FromY)), (d1z_17 = ((ln_122 = ln_119, ln_122.ToZ - ln_122.FromZ)), (d2x_17 = ((ln_123 = other_17, ln_123.ToX - ln_123.FromX)), (d2y_17 = ((ln_124 = other_17, ln_124.ToY - ln_124.FromY)), (d2z_17 = ((ln_125 = other_17, ln_125.ToZ - ln_125.FromZ)), (cx_17 = ((d1y_17 * d2z_17) - (d1z_17 * d2y_17)), (cy_17 = ((d1z_17 * d2x_17) - (d1x_17 * d2z_17)), (cz_17 = ((d1x_17 * d2y_17) - (d1y_17 * d2x_17)), (crossMagSq_17 = (((cx_17 * cx_17) + (cy_17 * cy_17)) + (cz_17 * cz_17)), (crossMagSq_17 < 1E-12) && ((px_5 = (ln_119.FromX - other_17.FromX), (py_5 = (ln_119.FromY - other_17.FromY), (pz_5 = (ln_119.FromZ - other_17.FromZ), (cx_1_6 = ((py_5 * d2z_17) - (pz_5 * d2y_17)), (cy_1_6 = ((pz_5 * d2x_17) - (px_5 * d2z_17)), (cz_1_6 = ((px_5 * d2y_17) - (py_5 * d2x_17)), (crossMagSq_1_6 = (((cx_1_6 * cx_1_6) + (cy_1_6 * cy_1_6)) + (cz_1_6 * cz_1_6)), crossMagSq_1_6 < 1E-12)))))))))))))))))))))("diagonal lines same ray are coincident");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast perpendicular lines", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let ln_126, other_18, d1x_18, ln_127, d1y_18, ln_128, d1z_18, ln_129, d2x_18, ln_130, d2y_18, ln_131, d2z_18, ln_132, cx_18, cy_18, cz_18, crossMagSq_18, px_6, py_6, pz_6, cx_1_7, cy_1_7, cz_1_7, crossMagSq_1_7;
        const a_17 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_17 = Line3D_$ctor_76A78260(0, 0, 0, 0, 10, 0);
        Expect_isFalse((ln_126 = a_17, (other_18 = b_17, (d1x_18 = ((ln_127 = ln_126, ln_127.ToX - ln_127.FromX)), (d1y_18 = ((ln_128 = ln_126, ln_128.ToY - ln_128.FromY)), (d1z_18 = ((ln_129 = ln_126, ln_129.ToZ - ln_129.FromZ)), (d2x_18 = ((ln_130 = other_18, ln_130.ToX - ln_130.FromX)), (d2y_18 = ((ln_131 = other_18, ln_131.ToY - ln_131.FromY)), (d2z_18 = ((ln_132 = other_18, ln_132.ToZ - ln_132.FromZ)), (cx_18 = ((d1y_18 * d2z_18) - (d1z_18 * d2y_18)), (cy_18 = ((d1z_18 * d2x_18) - (d1x_18 * d2z_18)), (cz_18 = ((d1x_18 * d2y_18) - (d1y_18 * d2x_18)), (crossMagSq_18 = (((cx_18 * cx_18) + (cy_18 * cy_18)) + (cz_18 * cz_18)), (crossMagSq_18 < 1E-12) && ((px_6 = (ln_126.FromX - other_18.FromX), (py_6 = (ln_126.FromY - other_18.FromY), (pz_6 = (ln_126.FromZ - other_18.FromZ), (cx_1_7 = ((py_6 * d2z_18) - (pz_6 * d2y_18)), (cy_1_7 = ((pz_6 * d2x_18) - (px_6 * d2z_18)), (cz_1_7 = ((px_6 * d2y_18) - (py_6 * d2x_18)), (crossMagSq_1_7 = (((cx_1_7 * cx_1_7) + (cy_1_7 * cy_1_7)) + (cz_1_7 * cz_1_7)), crossMagSq_1_7 < 1E-12)))))))))))))))))))))("perpendicular lines are not coincident");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentToFast zero length lines returns true", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let ln_133, other_19, d1x_19, ln_134, d1y_19, ln_135, d1z_19, ln_136, d2x_19, ln_137, d2y_19, ln_138, d2z_19, ln_139, cx_19, cy_19, cz_19, crossMagSq_19, px_7, py_7, pz_7, cx_1_8, cy_1_8, cz_1_8, crossMagSq_1_8;
        const a_18 = Line3D_$ctor_76A78260(0, 0, 0, 0, 0, 0);
        const b_18 = Line3D_$ctor_76A78260(5, 5, 5, 5, 5, 5);
        Expect_isTrue((ln_133 = a_18, (other_19 = b_18, (d1x_19 = ((ln_134 = ln_133, ln_134.ToX - ln_134.FromX)), (d1y_19 = ((ln_135 = ln_133, ln_135.ToY - ln_135.FromY)), (d1z_19 = ((ln_136 = ln_133, ln_136.ToZ - ln_136.FromZ)), (d2x_19 = ((ln_137 = other_19, ln_137.ToX - ln_137.FromX)), (d2y_19 = ((ln_138 = other_19, ln_138.ToY - ln_138.FromY)), (d2z_19 = ((ln_139 = other_19, ln_139.ToZ - ln_139.FromZ)), (cx_19 = ((d1y_19 * d2z_19) - (d1z_19 * d2y_19)), (cy_19 = ((d1z_19 * d2x_19) - (d1x_19 * d2z_19)), (cz_19 = ((d1x_19 * d2y_19) - (d1y_19 * d2x_19)), (crossMagSq_19 = (((cx_19 * cx_19) + (cy_19 * cy_19)) + (cz_19 * cz_19)), (crossMagSq_19 < 1E-12) && ((px_7 = (ln_133.FromX - other_19.FromX), (py_7 = (ln_133.FromY - other_19.FromY), (pz_7 = (ln_133.FromZ - other_19.FromZ), (cx_1_8 = ((py_7 * d2z_19) - (pz_7 * d2y_19)), (cy_1_8 = ((pz_7 * d2x_19) - (px_7 * d2z_19)), (cz_1_8 = ((px_7 * d2y_19) - (py_7 * d2x_19)), (crossMagSq_1_8 = (((cx_1_8 * cx_1_8) + (cy_1_8 * cy_1_8)) + (cz_1_8 * cz_1_8)), crossMagSq_1_8 < 1E-12)))))))))))))))))))))("zero length lines return true (as documented)");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let ln_140, other_20, d1x_20, ln_141, d1y_20, ln_142, d1z_20, ln_143, d2x_20, ln_144, d2y_20, ln_145, d2z_20, ln_146, cx_20, cy_20, cz_20, crossMagSq_20, dot;
        const a_19 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_19 = Line3D_$ctor_76A78260(0, 0, 0, 5, 0, 0);
        Expect_isTrue((ln_140 = a_19, (other_20 = b_19, (d1x_20 = ((ln_141 = ln_140, ln_141.ToX - ln_141.FromX)), (d1y_20 = ((ln_142 = ln_140, ln_142.ToY - ln_142.FromY)), (d1z_20 = ((ln_143 = ln_140, ln_143.ToZ - ln_143.FromZ)), (d2x_20 = ((ln_144 = other_20, ln_144.ToX - ln_144.FromX)), (d2y_20 = ((ln_145 = other_20, ln_145.ToY - ln_145.FromY)), (d2z_20 = ((ln_146 = other_20, ln_146.ToZ - ln_146.FromZ)), (cx_20 = ((d1y_20 * d2z_20) - (d1z_20 * d2y_20)), (cy_20 = ((d1z_20 * d2x_20) - (d1x_20 * d2z_20)), (cz_20 = ((d1x_20 * d2y_20) - (d1y_20 * d2x_20)), (crossMagSq_20 = (((cx_20 * cx_20) + (cy_20 * cy_20)) + (cz_20 * cz_20)), (crossMagSq_20 < 1E-12) && ((dot = (((d1x_20 * d2x_20) + (d1y_20 * d2y_20)) + (d1z_20 * d2z_20)), dot > 1E-06)))))))))))))))("same direction are parallel and oriented");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast opposite directions", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let ln_147, other_21, d1x_21, ln_148, d1y_21, ln_149, d1z_21, ln_150, d2x_21, ln_151, d2y_21, ln_152, d2z_21, ln_153, cx_21, cy_21, cz_21, crossMagSq_21, dot_1;
        const a_20 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_20 = Line3D_$ctor_76A78260(10, 0, 0, 0, 0, 0);
        Expect_isFalse((ln_147 = a_20, (other_21 = b_20, (d1x_21 = ((ln_148 = ln_147, ln_148.ToX - ln_148.FromX)), (d1y_21 = ((ln_149 = ln_147, ln_149.ToY - ln_149.FromY)), (d1z_21 = ((ln_150 = ln_147, ln_150.ToZ - ln_150.FromZ)), (d2x_21 = ((ln_151 = other_21, ln_151.ToX - ln_151.FromX)), (d2y_21 = ((ln_152 = other_21, ln_152.ToY - ln_152.FromY)), (d2z_21 = ((ln_153 = other_21, ln_153.ToZ - ln_153.FromZ)), (cx_21 = ((d1y_21 * d2z_21) - (d1z_21 * d2y_21)), (cy_21 = ((d1z_21 * d2x_21) - (d1x_21 * d2z_21)), (cz_21 = ((d1x_21 * d2y_21) - (d1y_21 * d2x_21)), (crossMagSq_21 = (((cx_21 * cx_21) + (cy_21 * cy_21)) + (cz_21 * cz_21)), (crossMagSq_21 < 1E-12) && ((dot_1 = (((d1x_21 * d2x_21) + (d1y_21 * d2y_21)) + (d1z_21 * d2z_21)), dot_1 > 1E-06)))))))))))))))("opposite directions are not oriented");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast parallel offset same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let ln_154, other_22, d1x_22, ln_155, d1y_22, ln_156, d1z_22, ln_157, d2x_22, ln_158, d2y_22, ln_159, d2z_22, ln_160, cx_22, cy_22, cz_22, crossMagSq_22, dot_2;
        const a_21 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_21 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        Expect_isTrue((ln_154 = a_21, (other_22 = b_21, (d1x_22 = ((ln_155 = ln_154, ln_155.ToX - ln_155.FromX)), (d1y_22 = ((ln_156 = ln_154, ln_156.ToY - ln_156.FromY)), (d1z_22 = ((ln_157 = ln_154, ln_157.ToZ - ln_157.FromZ)), (d2x_22 = ((ln_158 = other_22, ln_158.ToX - ln_158.FromX)), (d2y_22 = ((ln_159 = other_22, ln_159.ToY - ln_159.FromY)), (d2z_22 = ((ln_160 = other_22, ln_160.ToZ - ln_160.FromZ)), (cx_22 = ((d1y_22 * d2z_22) - (d1z_22 * d2y_22)), (cy_22 = ((d1z_22 * d2x_22) - (d1x_22 * d2z_22)), (cz_22 = ((d1x_22 * d2y_22) - (d1y_22 * d2x_22)), (crossMagSq_22 = (((cx_22 * cx_22) + (cy_22 * cy_22)) + (cz_22 * cz_22)), (crossMagSq_22 < 1E-12) && ((dot_2 = (((d1x_22 * d2x_22) + (d1y_22 * d2y_22)) + (d1z_22 * d2z_22)), dot_2 > 1E-06)))))))))))))))("parallel offset same direction");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast parallel offset opposite direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let ln_161, other_23, d1x_23, ln_162, d1y_23, ln_163, d1z_23, ln_164, d2x_23, ln_165, d2y_23, ln_166, d2z_23, ln_167, cx_23, cy_23, cz_23, crossMagSq_23, dot_3;
        const a_22 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_22 = Line3D_$ctor_76A78260(10, 5, 0, 0, 5, 0);
        Expect_isFalse((ln_161 = a_22, (other_23 = b_22, (d1x_23 = ((ln_162 = ln_161, ln_162.ToX - ln_162.FromX)), (d1y_23 = ((ln_163 = ln_161, ln_163.ToY - ln_163.FromY)), (d1z_23 = ((ln_164 = ln_161, ln_164.ToZ - ln_164.FromZ)), (d2x_23 = ((ln_165 = other_23, ln_165.ToX - ln_165.FromX)), (d2y_23 = ((ln_166 = other_23, ln_166.ToY - ln_166.FromY)), (d2z_23 = ((ln_167 = other_23, ln_167.ToZ - ln_167.FromZ)), (cx_23 = ((d1y_23 * d2z_23) - (d1z_23 * d2y_23)), (cy_23 = ((d1z_23 * d2x_23) - (d1x_23 * d2z_23)), (cz_23 = ((d1x_23 * d2y_23) - (d1y_23 * d2x_23)), (crossMagSq_23 = (((cx_23 * cx_23) + (cy_23 * cy_23)) + (cz_23 * cz_23)), (crossMagSq_23 < 1E-12) && ((dot_3 = (((d1x_23 * d2x_23) + (d1y_23 * d2y_23)) + (d1z_23 * d2z_23)), dot_3 > 1E-06)))))))))))))))("parallel offset opposite direction are not oriented");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast perpendicular", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let ln_168, other_24, d1x_24, ln_169, d1y_24, ln_170, d1z_24, ln_171, d2x_24, ln_172, d2y_24, ln_173, d2z_24, ln_174, cx_24, cy_24, cz_24, crossMagSq_24, dot_4;
        const a_23 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_23 = Line3D_$ctor_76A78260(0, 0, 0, 0, 10, 0);
        Expect_isFalse((ln_168 = a_23, (other_24 = b_23, (d1x_24 = ((ln_169 = ln_168, ln_169.ToX - ln_169.FromX)), (d1y_24 = ((ln_170 = ln_168, ln_170.ToY - ln_170.FromY)), (d1z_24 = ((ln_171 = ln_168, ln_171.ToZ - ln_171.FromZ)), (d2x_24 = ((ln_172 = other_24, ln_172.ToX - ln_172.FromX)), (d2y_24 = ((ln_173 = other_24, ln_173.ToY - ln_173.FromY)), (d2z_24 = ((ln_174 = other_24, ln_174.ToZ - ln_174.FromZ)), (cx_24 = ((d1y_24 * d2z_24) - (d1z_24 * d2y_24)), (cy_24 = ((d1z_24 * d2x_24) - (d1x_24 * d2z_24)), (cz_24 = ((d1x_24 * d2y_24) - (d1y_24 * d2x_24)), (crossMagSq_24 = (((cx_24 * cx_24) + (cy_24 * cy_24)) + (cz_24 * cz_24)), (crossMagSq_24 < 1E-12) && ((dot_4 = (((d1x_24 * d2x_24) + (d1y_24 * d2y_24)) + (d1z_24 * d2z_24)), dot_4 > 1E-06)))))))))))))))("perpendicular lines fail both parallel and orientation");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOrientedToFast zero length returns false", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let ln_175, other_25, d1x_25, ln_176, d1y_25, ln_177, d1z_25, ln_178, d2x_25, ln_179, d2y_25, ln_180, d2z_25, ln_181, cx_25, cy_25, cz_25, crossMagSq_25, dot_5;
        const a_24 = Line3D_$ctor_76A78260(0, 0, 0, 0, 0, 0);
        const b_24 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        Expect_isFalse((ln_175 = a_24, (other_25 = b_24, (d1x_25 = ((ln_176 = ln_175, ln_176.ToX - ln_176.FromX)), (d1y_25 = ((ln_177 = ln_175, ln_177.ToY - ln_177.FromY)), (d1z_25 = ((ln_178 = ln_175, ln_178.ToZ - ln_178.FromZ)), (d2x_25 = ((ln_179 = other_25, ln_179.ToX - ln_179.FromX)), (d2y_25 = ((ln_180 = other_25, ln_180.ToY - ln_180.FromY)), (d2z_25 = ((ln_181 = other_25, ln_181.ToZ - ln_181.FromZ)), (cx_25 = ((d1y_25 * d2z_25) - (d1z_25 * d2y_25)), (cy_25 = ((d1z_25 * d2x_25) - (d1x_25 * d2z_25)), (cz_25 = ((d1x_25 * d2y_25) - (d1y_25 * d2x_25)), (crossMagSq_25 = (((cx_25 * cx_25) + (cy_25 * cy_25)) + (cz_25 * cz_25)), (crossMagSq_25 < 1E-12) && ((dot_5 = (((d1x_25 * d2x_25) + (d1y_25 * d2y_25)) + (d1z_25 * d2z_25)), dot_5 > 1E-06)))))))))))))))("zero length line fails dot product check");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast opposite directions", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let ln_182, other_26, d1x_26, ln_183, d1y_26, ln_184, d1z_26, ln_185, d2x_26, ln_186, d2y_26, ln_187, d2z_26, ln_188, cx_26, cy_26, cz_26, crossMagSq_26, dot_6;
        const a_25 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_25 = Line3D_$ctor_76A78260(10, 0, 0, 0, 0, 0);
        Expect_isTrue((ln_182 = a_25, (other_26 = b_25, (d1x_26 = ((ln_183 = ln_182, ln_183.ToX - ln_183.FromX)), (d1y_26 = ((ln_184 = ln_182, ln_184.ToY - ln_184.FromY)), (d1z_26 = ((ln_185 = ln_182, ln_185.ToZ - ln_185.FromZ)), (d2x_26 = ((ln_186 = other_26, ln_186.ToX - ln_186.FromX)), (d2y_26 = ((ln_187 = other_26, ln_187.ToY - ln_187.FromY)), (d2z_26 = ((ln_188 = other_26, ln_188.ToZ - ln_188.FromZ)), (cx_26 = ((d1y_26 * d2z_26) - (d1z_26 * d2y_26)), (cy_26 = ((d1z_26 * d2x_26) - (d1x_26 * d2z_26)), (cz_26 = ((d1x_26 * d2y_26) - (d1y_26 * d2x_26)), (crossMagSq_26 = (((cx_26 * cx_26) + (cy_26 * cy_26)) + (cz_26 * cz_26)), (crossMagSq_26 < 1E-12) && ((dot_6 = (((d1x_26 * d2x_26) + (d1y_26 * d2y_26)) + (d1z_26 * d2z_26)), dot_6 < -1E-06)))))))))))))))("opposite directions are parallel and opposing");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let ln_189, other_27, d1x_27, ln_190, d1y_27, ln_191, d1z_27, ln_192, d2x_27, ln_193, d2y_27, ln_194, d2z_27, ln_195, cx_27, cy_27, cz_27, crossMagSq_27, dot_7;
        const a_26 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_26 = Line3D_$ctor_76A78260(0, 0, 0, 5, 0, 0);
        Expect_isFalse((ln_189 = a_26, (other_27 = b_26, (d1x_27 = ((ln_190 = ln_189, ln_190.ToX - ln_190.FromX)), (d1y_27 = ((ln_191 = ln_189, ln_191.ToY - ln_191.FromY)), (d1z_27 = ((ln_192 = ln_189, ln_192.ToZ - ln_192.FromZ)), (d2x_27 = ((ln_193 = other_27, ln_193.ToX - ln_193.FromX)), (d2y_27 = ((ln_194 = other_27, ln_194.ToY - ln_194.FromY)), (d2z_27 = ((ln_195 = other_27, ln_195.ToZ - ln_195.FromZ)), (cx_27 = ((d1y_27 * d2z_27) - (d1z_27 * d2y_27)), (cy_27 = ((d1z_27 * d2x_27) - (d1x_27 * d2z_27)), (cz_27 = ((d1x_27 * d2y_27) - (d1y_27 * d2x_27)), (crossMagSq_27 = (((cx_27 * cx_27) + (cy_27 * cy_27)) + (cz_27 * cz_27)), (crossMagSq_27 < 1E-12) && ((dot_7 = (((d1x_27 * d2x_27) + (d1y_27 * d2y_27)) + (d1z_27 * d2z_27)), dot_7 < -1E-06)))))))))))))))("same direction are not opposing");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast parallel offset opposing", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        let ln_196, other_28, d1x_28, ln_197, d1y_28, ln_198, d1z_28, ln_199, d2x_28, ln_200, d2y_28, ln_201, d2z_28, ln_202, cx_28, cy_28, cz_28, crossMagSq_28, dot_8;
        const a_27 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_27 = Line3D_$ctor_76A78260(10, 5, 0, 0, 5, 0);
        Expect_isTrue((ln_196 = a_27, (other_28 = b_27, (d1x_28 = ((ln_197 = ln_196, ln_197.ToX - ln_197.FromX)), (d1y_28 = ((ln_198 = ln_196, ln_198.ToY - ln_198.FromY)), (d1z_28 = ((ln_199 = ln_196, ln_199.ToZ - ln_199.FromZ)), (d2x_28 = ((ln_200 = other_28, ln_200.ToX - ln_200.FromX)), (d2y_28 = ((ln_201 = other_28, ln_201.ToY - ln_201.FromY)), (d2z_28 = ((ln_202 = other_28, ln_202.ToZ - ln_202.FromZ)), (cx_28 = ((d1y_28 * d2z_28) - (d1z_28 * d2y_28)), (cy_28 = ((d1z_28 * d2x_28) - (d1x_28 * d2z_28)), (cz_28 = ((d1x_28 * d2y_28) - (d1y_28 * d2x_28)), (crossMagSq_28 = (((cx_28 * cx_28) + (cy_28 * cy_28)) + (cz_28 * cz_28)), (crossMagSq_28 < 1E-12) && ((dot_8 = (((d1x_28 * d2x_28) + (d1y_28 * d2y_28)) + (d1z_28 * d2z_28)), dot_8 < -1E-06)))))))))))))))("parallel offset opposing directions");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast parallel offset same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let ln_203, other_29, d1x_29, ln_204, d1y_29, ln_205, d1z_29, ln_206, d2x_29, ln_207, d2y_29, ln_208, d2z_29, ln_209, cx_29, cy_29, cz_29, crossMagSq_29, dot_9;
        const a_28 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_28 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        Expect_isFalse((ln_203 = a_28, (other_29 = b_28, (d1x_29 = ((ln_204 = ln_203, ln_204.ToX - ln_204.FromX)), (d1y_29 = ((ln_205 = ln_203, ln_205.ToY - ln_205.FromY)), (d1z_29 = ((ln_206 = ln_203, ln_206.ToZ - ln_206.FromZ)), (d2x_29 = ((ln_207 = other_29, ln_207.ToX - ln_207.FromX)), (d2y_29 = ((ln_208 = other_29, ln_208.ToY - ln_208.FromY)), (d2z_29 = ((ln_209 = other_29, ln_209.ToZ - ln_209.FromZ)), (cx_29 = ((d1y_29 * d2z_29) - (d1z_29 * d2y_29)), (cy_29 = ((d1z_29 * d2x_29) - (d1x_29 * d2z_29)), (cz_29 = ((d1x_29 * d2y_29) - (d1y_29 * d2x_29)), (crossMagSq_29 = (((cx_29 * cx_29) + (cy_29 * cy_29)) + (cz_29 * cz_29)), (crossMagSq_29 < 1E-12) && ((dot_9 = (((d1x_29 * d2x_29) + (d1y_29 * d2y_29)) + (d1z_29 * d2z_29)), dot_9 < -1E-06)))))))))))))))("parallel offset same direction are not opposing");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast perpendicular", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let ln_210, other_30, d1x_30, ln_211, d1y_30, ln_212, d1z_30, ln_213, d2x_30, ln_214, d2y_30, ln_215, d2z_30, ln_216, cx_30, cy_30, cz_30, crossMagSq_30, dot_10;
        const a_29 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_29 = Line3D_$ctor_76A78260(0, 0, 0, 0, 10, 0);
        Expect_isFalse((ln_210 = a_29, (other_30 = b_29, (d1x_30 = ((ln_211 = ln_210, ln_211.ToX - ln_211.FromX)), (d1y_30 = ((ln_212 = ln_210, ln_212.ToY - ln_212.FromY)), (d1z_30 = ((ln_213 = ln_210, ln_213.ToZ - ln_213.FromZ)), (d2x_30 = ((ln_214 = other_30, ln_214.ToX - ln_214.FromX)), (d2y_30 = ((ln_215 = other_30, ln_215.ToY - ln_215.FromY)), (d2z_30 = ((ln_216 = other_30, ln_216.ToZ - ln_216.FromZ)), (cx_30 = ((d1y_30 * d2z_30) - (d1z_30 * d2y_30)), (cy_30 = ((d1z_30 * d2x_30) - (d1x_30 * d2z_30)), (cz_30 = ((d1x_30 * d2y_30) - (d1y_30 * d2x_30)), (crossMagSq_30 = (((cx_30 * cx_30) + (cy_30 * cy_30)) + (cz_30 * cz_30)), (crossMagSq_30 < 1E-12) && ((dot_10 = (((d1x_30 * d2x_30) + (d1y_30 * d2y_30)) + (d1z_30 * d2z_30)), dot_10 < -1E-06)))))))))))))))("perpendicular lines fail both parallel and opposing");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsParallelAndOpposingToFast zero length returns false", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let ln_217, other_31, d1x_31, ln_218, d1y_31, ln_219, d1z_31, ln_220, d2x_31, ln_221, d2y_31, ln_222, d2z_31, ln_223, cx_31, cy_31, cz_31, crossMagSq_31, dot_11;
        const a_30 = Line3D_$ctor_76A78260(0, 0, 0, 0, 0, 0);
        const b_30 = Line3D_$ctor_76A78260(10, 0, 0, 0, 0, 0);
        Expect_isFalse((ln_217 = a_30, (other_31 = b_30, (d1x_31 = ((ln_218 = ln_217, ln_218.ToX - ln_218.FromX)), (d1y_31 = ((ln_219 = ln_217, ln_219.ToY - ln_219.FromY)), (d1z_31 = ((ln_220 = ln_217, ln_220.ToZ - ln_220.FromZ)), (d2x_31 = ((ln_221 = other_31, ln_221.ToX - ln_221.FromX)), (d2y_31 = ((ln_222 = other_31, ln_222.ToY - ln_222.FromY)), (d2z_31 = ((ln_223 = other_31, ln_223.ToZ - ln_223.FromZ)), (cx_31 = ((d1y_31 * d2z_31) - (d1z_31 * d2y_31)), (cy_31 = ((d1z_31 * d2x_31) - (d1x_31 * d2z_31)), (cz_31 = ((d1x_31 * d2y_31) - (d1y_31 * d2x_31)), (crossMagSq_31 = (((cx_31 * cx_31) + (cy_31 * cy_31)) + (cz_31 * cz_31)), (crossMagSq_31 < 1E-12) && ((dot_11 = (((d1x_31 * d2x_31) + (d1y_31 * d2y_31)) + (d1z_31 * d2z_31)), dot_11 < -1E-06)))))))))))))))("zero length line fails dot product check");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast same ray same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let ln_224, other_32, d1x_32, ln_225, d1y_32, ln_226, d1z_32, ln_227, d2x_32, ln_228, d2y_32, ln_229, d2z_32, ln_230, cx_32, cy_32, cz_32, crossMagSq_32, dot_12, px_8, py_8, pz_8, cx_1_9, cy_1_9, cz_1_9, crossMagSq_1_9;
        const a_31 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_31 = Line3D_$ctor_76A78260(0, 0, 0, 5, 0, 0);
        Expect_isTrue((ln_224 = a_31, (other_32 = b_31, (d1x_32 = ((ln_225 = ln_224, ln_225.ToX - ln_225.FromX)), (d1y_32 = ((ln_226 = ln_224, ln_226.ToY - ln_226.FromY)), (d1z_32 = ((ln_227 = ln_224, ln_227.ToZ - ln_227.FromZ)), (d2x_32 = ((ln_228 = other_32, ln_228.ToX - ln_228.FromX)), (d2y_32 = ((ln_229 = other_32, ln_229.ToY - ln_229.FromY)), (d2z_32 = ((ln_230 = other_32, ln_230.ToZ - ln_230.FromZ)), (cx_32 = ((d1y_32 * d2z_32) - (d1z_32 * d2y_32)), (cy_32 = ((d1z_32 * d2x_32) - (d1x_32 * d2z_32)), (cz_32 = ((d1x_32 * d2y_32) - (d1y_32 * d2x_32)), (crossMagSq_32 = (((cx_32 * cx_32) + (cy_32 * cy_32)) + (cz_32 * cz_32)), (crossMagSq_32 < 1E-12) && ((dot_12 = (((d1x_32 * d2x_32) + (d1y_32 * d2y_32)) + (d1z_32 * d2z_32)), (dot_12 > 1E-06) && ((px_8 = (ln_224.FromX - other_32.FromX), (py_8 = (ln_224.FromY - other_32.FromY), (pz_8 = (ln_224.FromZ - other_32.FromZ), (cx_1_9 = ((py_8 * d2z_32) - (pz_8 * d2y_32)), (cy_1_9 = ((pz_8 * d2x_32) - (px_8 * d2z_32)), (cz_1_9 = ((px_8 * d2y_32) - (py_8 * d2x_32)), (crossMagSq_1_9 = (((cx_1_9 * cx_1_9) + (cy_1_9 * cy_1_9)) + (cz_1_9 * cz_1_9)), crossMagSq_1_9 < 1E-12)))))))))))))))))))))))("same ray same direction are coincident and oriented");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast same ray opposite direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let ln_231, other_33, d1x_33, ln_232, d1y_33, ln_233, d1z_33, ln_234, d2x_33, ln_235, d2y_33, ln_236, d2z_33, ln_237, cx_33, cy_33, cz_33, crossMagSq_33, dot_13, px_9, py_9, pz_9, cx_1_10, cy_1_10, cz_1_10, crossMagSq_1_10;
        const a_32 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_32 = Line3D_$ctor_76A78260(10, 0, 0, 0, 0, 0);
        Expect_isFalse((ln_231 = a_32, (other_33 = b_32, (d1x_33 = ((ln_232 = ln_231, ln_232.ToX - ln_232.FromX)), (d1y_33 = ((ln_233 = ln_231, ln_233.ToY - ln_233.FromY)), (d1z_33 = ((ln_234 = ln_231, ln_234.ToZ - ln_234.FromZ)), (d2x_33 = ((ln_235 = other_33, ln_235.ToX - ln_235.FromX)), (d2y_33 = ((ln_236 = other_33, ln_236.ToY - ln_236.FromY)), (d2z_33 = ((ln_237 = other_33, ln_237.ToZ - ln_237.FromZ)), (cx_33 = ((d1y_33 * d2z_33) - (d1z_33 * d2y_33)), (cy_33 = ((d1z_33 * d2x_33) - (d1x_33 * d2z_33)), (cz_33 = ((d1x_33 * d2y_33) - (d1y_33 * d2x_33)), (crossMagSq_33 = (((cx_33 * cx_33) + (cy_33 * cy_33)) + (cz_33 * cz_33)), (crossMagSq_33 < 1E-12) && ((dot_13 = (((d1x_33 * d2x_33) + (d1y_33 * d2y_33)) + (d1z_33 * d2z_33)), (dot_13 > 1E-06) && ((px_9 = (ln_231.FromX - other_33.FromX), (py_9 = (ln_231.FromY - other_33.FromY), (pz_9 = (ln_231.FromZ - other_33.FromZ), (cx_1_10 = ((py_9 * d2z_33) - (pz_9 * d2y_33)), (cy_1_10 = ((pz_9 * d2x_33) - (px_9 * d2z_33)), (cz_1_10 = ((px_9 * d2y_33) - (py_9 * d2x_33)), (crossMagSq_1_10 = (((cx_1_10 * cx_1_10) + (cy_1_10 * cy_1_10)) + (cz_1_10 * cz_1_10)), crossMagSq_1_10 < 1E-12)))))))))))))))))))))))("same ray opposite direction are not oriented");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast same ray offset start same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let ln_238, other_34, d1x_34, ln_239, d1y_34, ln_240, d1z_34, ln_241, d2x_34, ln_242, d2y_34, ln_243, d2z_34, ln_244, cx_34, cy_34, cz_34, crossMagSq_34, dot_14, px_10, py_10, pz_10, cx_1_11, cy_1_11, cz_1_11, crossMagSq_1_11;
        const a_33 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_33 = Line3D_$ctor_76A78260(5, 0, 0, 15, 0, 0);
        Expect_isTrue((ln_238 = a_33, (other_34 = b_33, (d1x_34 = ((ln_239 = ln_238, ln_239.ToX - ln_239.FromX)), (d1y_34 = ((ln_240 = ln_238, ln_240.ToY - ln_240.FromY)), (d1z_34 = ((ln_241 = ln_238, ln_241.ToZ - ln_241.FromZ)), (d2x_34 = ((ln_242 = other_34, ln_242.ToX - ln_242.FromX)), (d2y_34 = ((ln_243 = other_34, ln_243.ToY - ln_243.FromY)), (d2z_34 = ((ln_244 = other_34, ln_244.ToZ - ln_244.FromZ)), (cx_34 = ((d1y_34 * d2z_34) - (d1z_34 * d2y_34)), (cy_34 = ((d1z_34 * d2x_34) - (d1x_34 * d2z_34)), (cz_34 = ((d1x_34 * d2y_34) - (d1y_34 * d2x_34)), (crossMagSq_34 = (((cx_34 * cx_34) + (cy_34 * cy_34)) + (cz_34 * cz_34)), (crossMagSq_34 < 1E-12) && ((dot_14 = (((d1x_34 * d2x_34) + (d1y_34 * d2y_34)) + (d1z_34 * d2z_34)), (dot_14 > 1E-06) && ((px_10 = (ln_238.FromX - other_34.FromX), (py_10 = (ln_238.FromY - other_34.FromY), (pz_10 = (ln_238.FromZ - other_34.FromZ), (cx_1_11 = ((py_10 * d2z_34) - (pz_10 * d2y_34)), (cy_1_11 = ((pz_10 * d2x_34) - (px_10 * d2z_34)), (cz_1_11 = ((px_10 * d2y_34) - (py_10 * d2x_34)), (crossMagSq_1_11 = (((cx_1_11 * cx_1_11) + (cy_1_11 * cy_1_11)) + (cz_1_11 * cz_1_11)), crossMagSq_1_11 < 1E-12)))))))))))))))))))))))("same ray offset start same direction");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast parallel offset same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let ln_245, other_35, d1x_35, ln_246, d1y_35, ln_247, d1z_35, ln_248, d2x_35, ln_249, d2y_35, ln_250, d2z_35, ln_251, cx_35, cy_35, cz_35, crossMagSq_35, dot_15, px_11, py_11, pz_11, cx_1_12, cy_1_12, cz_1_12, crossMagSq_1_12;
        const a_34 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_34 = Line3D_$ctor_76A78260(0, 5, 0, 10, 5, 0);
        Expect_isFalse((ln_245 = a_34, (other_35 = b_34, (d1x_35 = ((ln_246 = ln_245, ln_246.ToX - ln_246.FromX)), (d1y_35 = ((ln_247 = ln_245, ln_247.ToY - ln_247.FromY)), (d1z_35 = ((ln_248 = ln_245, ln_248.ToZ - ln_248.FromZ)), (d2x_35 = ((ln_249 = other_35, ln_249.ToX - ln_249.FromX)), (d2y_35 = ((ln_250 = other_35, ln_250.ToY - ln_250.FromY)), (d2z_35 = ((ln_251 = other_35, ln_251.ToZ - ln_251.FromZ)), (cx_35 = ((d1y_35 * d2z_35) - (d1z_35 * d2y_35)), (cy_35 = ((d1z_35 * d2x_35) - (d1x_35 * d2z_35)), (cz_35 = ((d1x_35 * d2y_35) - (d1y_35 * d2x_35)), (crossMagSq_35 = (((cx_35 * cx_35) + (cy_35 * cy_35)) + (cz_35 * cz_35)), (crossMagSq_35 < 1E-12) && ((dot_15 = (((d1x_35 * d2x_35) + (d1y_35 * d2y_35)) + (d1z_35 * d2z_35)), (dot_15 > 1E-06) && ((px_11 = (ln_245.FromX - other_35.FromX), (py_11 = (ln_245.FromY - other_35.FromY), (pz_11 = (ln_245.FromZ - other_35.FromZ), (cx_1_12 = ((py_11 * d2z_35) - (pz_11 * d2y_35)), (cy_1_12 = ((pz_11 * d2x_35) - (px_11 * d2z_35)), (cz_1_12 = ((px_11 * d2y_35) - (py_11 * d2x_35)), (crossMagSq_1_12 = (((cx_1_12 * cx_1_12) + (cy_1_12 * cy_1_12)) + (cz_1_12 * cz_1_12)), crossMagSq_1_12 < 1E-12)))))))))))))))))))))))("parallel offset same direction are not coincident");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast diagonal same ray same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        let ln_252, other_36, d1x_36, ln_253, d1y_36, ln_254, d1z_36, ln_255, d2x_36, ln_256, d2y_36, ln_257, d2z_36, ln_258, cx_36, cy_36, cz_36, crossMagSq_36, dot_16, px_12, py_12, pz_12, cx_1_13, cy_1_13, cz_1_13, crossMagSq_1_13;
        const a_35 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const b_35 = Line3D_$ctor_76A78260(5, 5, 5, 15, 15, 15);
        Expect_isTrue((ln_252 = a_35, (other_36 = b_35, (d1x_36 = ((ln_253 = ln_252, ln_253.ToX - ln_253.FromX)), (d1y_36 = ((ln_254 = ln_252, ln_254.ToY - ln_254.FromY)), (d1z_36 = ((ln_255 = ln_252, ln_255.ToZ - ln_255.FromZ)), (d2x_36 = ((ln_256 = other_36, ln_256.ToX - ln_256.FromX)), (d2y_36 = ((ln_257 = other_36, ln_257.ToY - ln_257.FromY)), (d2z_36 = ((ln_258 = other_36, ln_258.ToZ - ln_258.FromZ)), (cx_36 = ((d1y_36 * d2z_36) - (d1z_36 * d2y_36)), (cy_36 = ((d1z_36 * d2x_36) - (d1x_36 * d2z_36)), (cz_36 = ((d1x_36 * d2y_36) - (d1y_36 * d2x_36)), (crossMagSq_36 = (((cx_36 * cx_36) + (cy_36 * cy_36)) + (cz_36 * cz_36)), (crossMagSq_36 < 1E-12) && ((dot_16 = (((d1x_36 * d2x_36) + (d1y_36 * d2y_36)) + (d1z_36 * d2z_36)), (dot_16 > 1E-06) && ((px_12 = (ln_252.FromX - other_36.FromX), (py_12 = (ln_252.FromY - other_36.FromY), (pz_12 = (ln_252.FromZ - other_36.FromZ), (cx_1_13 = ((py_12 * d2z_36) - (pz_12 * d2y_36)), (cy_1_13 = ((pz_12 * d2x_36) - (px_12 * d2z_36)), (cz_1_13 = ((px_12 * d2y_36) - (py_12 * d2x_36)), (crossMagSq_1_13 = (((cx_1_13 * cx_1_13) + (cy_1_13 * cy_1_13)) + (cz_1_13 * cz_1_13)), crossMagSq_1_13 < 1E-12)))))))))))))))))))))))("diagonal same ray same direction");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOrientedToFast diagonal same ray opposite direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let ln_259, other_37, d1x_37, ln_260, d1y_37, ln_261, d1z_37, ln_262, d2x_37, ln_263, d2y_37, ln_264, d2z_37, ln_265, cx_37, cy_37, cz_37, crossMagSq_37, dot_17, px_13, py_13, pz_13, cx_1_14, cy_1_14, cz_1_14, crossMagSq_1_14;
        const a_36 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const b_36 = Line3D_$ctor_76A78260(10, 10, 10, 0, 0, 0);
        Expect_isFalse((ln_259 = a_36, (other_37 = b_36, (d1x_37 = ((ln_260 = ln_259, ln_260.ToX - ln_260.FromX)), (d1y_37 = ((ln_261 = ln_259, ln_261.ToY - ln_261.FromY)), (d1z_37 = ((ln_262 = ln_259, ln_262.ToZ - ln_262.FromZ)), (d2x_37 = ((ln_263 = other_37, ln_263.ToX - ln_263.FromX)), (d2y_37 = ((ln_264 = other_37, ln_264.ToY - ln_264.FromY)), (d2z_37 = ((ln_265 = other_37, ln_265.ToZ - ln_265.FromZ)), (cx_37 = ((d1y_37 * d2z_37) - (d1z_37 * d2y_37)), (cy_37 = ((d1z_37 * d2x_37) - (d1x_37 * d2z_37)), (cz_37 = ((d1x_37 * d2y_37) - (d1y_37 * d2x_37)), (crossMagSq_37 = (((cx_37 * cx_37) + (cy_37 * cy_37)) + (cz_37 * cz_37)), (crossMagSq_37 < 1E-12) && ((dot_17 = (((d1x_37 * d2x_37) + (d1y_37 * d2y_37)) + (d1z_37 * d2z_37)), (dot_17 > 1E-06) && ((px_13 = (ln_259.FromX - other_37.FromX), (py_13 = (ln_259.FromY - other_37.FromY), (pz_13 = (ln_259.FromZ - other_37.FromZ), (cx_1_14 = ((py_13 * d2z_37) - (pz_13 * d2y_37)), (cy_1_14 = ((pz_13 * d2x_37) - (px_13 * d2z_37)), (cz_1_14 = ((px_13 * d2y_37) - (py_13 * d2x_37)), (crossMagSq_1_14 = (((cx_1_14 * cx_1_14) + (cy_1_14 * cy_1_14)) + (cz_1_14 * cz_1_14)), crossMagSq_1_14 < 1E-12)))))))))))))))))))))))("diagonal same ray opposite direction are not oriented");
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast same ray opposite direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let ln_266, other_38, d1x_38, ln_267, d1y_38, ln_268, d1z_38, ln_269, d2x_38, ln_270, d2y_38, ln_271, d2z_38, ln_272, cx_38, cy_38, cz_38, crossMagSq_38, dot_18, px_14, py_14, pz_14, cx_1_15, cy_1_15, cz_1_15, crossMagSq_1_15;
        const a_37 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_37 = Line3D_$ctor_76A78260(10, 0, 0, 0, 0, 0);
        Expect_isTrue((ln_266 = a_37, (other_38 = b_37, (d1x_38 = ((ln_267 = ln_266, ln_267.ToX - ln_267.FromX)), (d1y_38 = ((ln_268 = ln_266, ln_268.ToY - ln_268.FromY)), (d1z_38 = ((ln_269 = ln_266, ln_269.ToZ - ln_269.FromZ)), (d2x_38 = ((ln_270 = other_38, ln_270.ToX - ln_270.FromX)), (d2y_38 = ((ln_271 = other_38, ln_271.ToY - ln_271.FromY)), (d2z_38 = ((ln_272 = other_38, ln_272.ToZ - ln_272.FromZ)), (cx_38 = ((d1y_38 * d2z_38) - (d1z_38 * d2y_38)), (cy_38 = ((d1z_38 * d2x_38) - (d1x_38 * d2z_38)), (cz_38 = ((d1x_38 * d2y_38) - (d1y_38 * d2x_38)), (crossMagSq_38 = (((cx_38 * cx_38) + (cy_38 * cy_38)) + (cz_38 * cz_38)), (crossMagSq_38 < 1E-12) && ((dot_18 = (((d1x_38 * d2x_38) + (d1y_38 * d2y_38)) + (d1z_38 * d2z_38)), (dot_18 < -1E-06) && ((px_14 = (ln_266.FromX - other_38.FromX), (py_14 = (ln_266.FromY - other_38.FromY), (pz_14 = (ln_266.FromZ - other_38.FromZ), (cx_1_15 = ((py_14 * d2z_38) - (pz_14 * d2y_38)), (cy_1_15 = ((pz_14 * d2x_38) - (px_14 * d2z_38)), (cz_1_15 = ((px_14 * d2y_38) - (py_14 * d2x_38)), (crossMagSq_1_15 = (((cx_1_15 * cx_1_15) + (cy_1_15 * cy_1_15)) + (cz_1_15 * cz_1_15)), crossMagSq_1_15 < 1E-12)))))))))))))))))))))))("same ray opposite direction are coincident and opposing");
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast same ray same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        let ln_273, other_39, d1x_39, ln_274, d1y_39, ln_275, d1z_39, ln_276, d2x_39, ln_277, d2y_39, ln_278, d2z_39, ln_279, cx_39, cy_39, cz_39, crossMagSq_39, dot_19, px_15, py_15, pz_15, cx_1_16, cy_1_16, cz_1_16, crossMagSq_1_16;
        const a_38 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_38 = Line3D_$ctor_76A78260(0, 0, 0, 5, 0, 0);
        Expect_isFalse((ln_273 = a_38, (other_39 = b_38, (d1x_39 = ((ln_274 = ln_273, ln_274.ToX - ln_274.FromX)), (d1y_39 = ((ln_275 = ln_273, ln_275.ToY - ln_275.FromY)), (d1z_39 = ((ln_276 = ln_273, ln_276.ToZ - ln_276.FromZ)), (d2x_39 = ((ln_277 = other_39, ln_277.ToX - ln_277.FromX)), (d2y_39 = ((ln_278 = other_39, ln_278.ToY - ln_278.FromY)), (d2z_39 = ((ln_279 = other_39, ln_279.ToZ - ln_279.FromZ)), (cx_39 = ((d1y_39 * d2z_39) - (d1z_39 * d2y_39)), (cy_39 = ((d1z_39 * d2x_39) - (d1x_39 * d2z_39)), (cz_39 = ((d1x_39 * d2y_39) - (d1y_39 * d2x_39)), (crossMagSq_39 = (((cx_39 * cx_39) + (cy_39 * cy_39)) + (cz_39 * cz_39)), (crossMagSq_39 < 1E-12) && ((dot_19 = (((d1x_39 * d2x_39) + (d1y_39 * d2y_39)) + (d1z_39 * d2z_39)), (dot_19 < -1E-06) && ((px_15 = (ln_273.FromX - other_39.FromX), (py_15 = (ln_273.FromY - other_39.FromY), (pz_15 = (ln_273.FromZ - other_39.FromZ), (cx_1_16 = ((py_15 * d2z_39) - (pz_15 * d2y_39)), (cy_1_16 = ((pz_15 * d2x_39) - (px_15 * d2z_39)), (cz_1_16 = ((px_15 * d2y_39) - (py_15 * d2x_39)), (crossMagSq_1_16 = (((cx_1_16 * cx_1_16) + (cy_1_16 * cy_1_16)) + (cz_1_16 * cz_1_16)), crossMagSq_1_16 < 1E-12)))))))))))))))))))))))("same ray same direction are not opposing");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast same ray offset opposite direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let ln_280, other_40, d1x_40, ln_281, d1y_40, ln_282, d1z_40, ln_283, d2x_40, ln_284, d2y_40, ln_285, d2z_40, ln_286, cx_40, cy_40, cz_40, crossMagSq_40, dot_20, px_16, py_16, pz_16, cx_1_17, cy_1_17, cz_1_17, crossMagSq_1_17;
        const a_39 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_39 = Line3D_$ctor_76A78260(15, 0, 0, 5, 0, 0);
        Expect_isTrue((ln_280 = a_39, (other_40 = b_39, (d1x_40 = ((ln_281 = ln_280, ln_281.ToX - ln_281.FromX)), (d1y_40 = ((ln_282 = ln_280, ln_282.ToY - ln_282.FromY)), (d1z_40 = ((ln_283 = ln_280, ln_283.ToZ - ln_283.FromZ)), (d2x_40 = ((ln_284 = other_40, ln_284.ToX - ln_284.FromX)), (d2y_40 = ((ln_285 = other_40, ln_285.ToY - ln_285.FromY)), (d2z_40 = ((ln_286 = other_40, ln_286.ToZ - ln_286.FromZ)), (cx_40 = ((d1y_40 * d2z_40) - (d1z_40 * d2y_40)), (cy_40 = ((d1z_40 * d2x_40) - (d1x_40 * d2z_40)), (cz_40 = ((d1x_40 * d2y_40) - (d1y_40 * d2x_40)), (crossMagSq_40 = (((cx_40 * cx_40) + (cy_40 * cy_40)) + (cz_40 * cz_40)), (crossMagSq_40 < 1E-12) && ((dot_20 = (((d1x_40 * d2x_40) + (d1y_40 * d2y_40)) + (d1z_40 * d2z_40)), (dot_20 < -1E-06) && ((px_16 = (ln_280.FromX - other_40.FromX), (py_16 = (ln_280.FromY - other_40.FromY), (pz_16 = (ln_280.FromZ - other_40.FromZ), (cx_1_17 = ((py_16 * d2z_40) - (pz_16 * d2y_40)), (cy_1_17 = ((pz_16 * d2x_40) - (px_16 * d2z_40)), (cz_1_17 = ((px_16 * d2y_40) - (py_16 * d2x_40)), (crossMagSq_1_17 = (((cx_1_17 * cx_1_17) + (cy_1_17 * cy_1_17)) + (cz_1_17 * cz_1_17)), crossMagSq_1_17 < 1E-12)))))))))))))))))))))))("same ray offset opposite direction");
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), (() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast parallel offset opposite direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let ln_287, other_41, d1x_41, ln_288, d1y_41, ln_289, d1z_41, ln_290, d2x_41, ln_291, d2y_41, ln_292, d2z_41, ln_293, cx_41, cy_41, cz_41, crossMagSq_41, dot_21, px_17, py_17, pz_17, cx_1_18, cy_1_18, cz_1_18, crossMagSq_1_18;
        const a_40 = Line3D_$ctor_76A78260(0, 0, 0, 10, 0, 0);
        const b_40 = Line3D_$ctor_76A78260(10, 5, 0, 0, 5, 0);
        Expect_isFalse((ln_287 = a_40, (other_41 = b_40, (d1x_41 = ((ln_288 = ln_287, ln_288.ToX - ln_288.FromX)), (d1y_41 = ((ln_289 = ln_287, ln_289.ToY - ln_289.FromY)), (d1z_41 = ((ln_290 = ln_287, ln_290.ToZ - ln_290.FromZ)), (d2x_41 = ((ln_291 = other_41, ln_291.ToX - ln_291.FromX)), (d2y_41 = ((ln_292 = other_41, ln_292.ToY - ln_292.FromY)), (d2z_41 = ((ln_293 = other_41, ln_293.ToZ - ln_293.FromZ)), (cx_41 = ((d1y_41 * d2z_41) - (d1z_41 * d2y_41)), (cy_41 = ((d1z_41 * d2x_41) - (d1x_41 * d2z_41)), (cz_41 = ((d1x_41 * d2y_41) - (d1y_41 * d2x_41)), (crossMagSq_41 = (((cx_41 * cx_41) + (cy_41 * cy_41)) + (cz_41 * cz_41)), (crossMagSq_41 < 1E-12) && ((dot_21 = (((d1x_41 * d2x_41) + (d1y_41 * d2y_41)) + (d1z_41 * d2z_41)), (dot_21 < -1E-06) && ((px_17 = (ln_287.FromX - other_41.FromX), (py_17 = (ln_287.FromY - other_41.FromY), (pz_17 = (ln_287.FromZ - other_41.FromZ), (cx_1_18 = ((py_17 * d2z_41) - (pz_17 * d2y_41)), (cy_1_18 = ((pz_17 * d2x_41) - (px_17 * d2z_41)), (cz_1_18 = ((px_17 * d2y_41) - (py_17 * d2x_41)), (crossMagSq_1_18 = (((cx_1_18 * cx_1_18) + (cy_1_18 * cy_1_18)) + (cz_1_18 * cz_1_18)), crossMagSq_1_18 < 1E-12)))))))))))))))))))))))("parallel offset opposite direction are not coincident");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast diagonal same ray opposite direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let ln_294, other_42, d1x_42, ln_295, d1y_42, ln_296, d1z_42, ln_297, d2x_42, ln_298, d2y_42, ln_299, d2z_42, ln_300, cx_42, cy_42, cz_42, crossMagSq_42, dot_22, px_18, py_18, pz_18, cx_1_19, cy_1_19, cz_1_19, crossMagSq_1_19;
        const a_41 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const b_41 = Line3D_$ctor_76A78260(10, 10, 10, 0, 0, 0);
        Expect_isTrue((ln_294 = a_41, (other_42 = b_41, (d1x_42 = ((ln_295 = ln_294, ln_295.ToX - ln_295.FromX)), (d1y_42 = ((ln_296 = ln_294, ln_296.ToY - ln_296.FromY)), (d1z_42 = ((ln_297 = ln_294, ln_297.ToZ - ln_297.FromZ)), (d2x_42 = ((ln_298 = other_42, ln_298.ToX - ln_298.FromX)), (d2y_42 = ((ln_299 = other_42, ln_299.ToY - ln_299.FromY)), (d2z_42 = ((ln_300 = other_42, ln_300.ToZ - ln_300.FromZ)), (cx_42 = ((d1y_42 * d2z_42) - (d1z_42 * d2y_42)), (cy_42 = ((d1z_42 * d2x_42) - (d1x_42 * d2z_42)), (cz_42 = ((d1x_42 * d2y_42) - (d1y_42 * d2x_42)), (crossMagSq_42 = (((cx_42 * cx_42) + (cy_42 * cy_42)) + (cz_42 * cz_42)), (crossMagSq_42 < 1E-12) && ((dot_22 = (((d1x_42 * d2x_42) + (d1y_42 * d2y_42)) + (d1z_42 * d2z_42)), (dot_22 < -1E-06) && ((px_18 = (ln_294.FromX - other_42.FromX), (py_18 = (ln_294.FromY - other_42.FromY), (pz_18 = (ln_294.FromZ - other_42.FromZ), (cx_1_19 = ((py_18 * d2z_42) - (pz_18 * d2y_42)), (cy_1_19 = ((pz_18 * d2x_42) - (px_18 * d2z_42)), (cz_1_19 = ((px_18 * d2y_42) - (py_18 * d2x_42)), (crossMagSq_1_19 = (((cx_1_19 * cx_1_19) + (cy_1_19 * cy_1_19)) + (cz_1_19 * cz_1_19)), crossMagSq_1_19 < 1E-12)))))))))))))))))))))))("diagonal same ray opposite direction");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentAndOpposingToFast diagonal same ray same direction", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let ln_301, other_43, d1x_43, ln_302, d1y_43, ln_303, d1z_43, ln_304, d2x_43, ln_305, d2y_43, ln_306, d2z_43, ln_307, cx_43, cy_43, cz_43, crossMagSq_43, dot_23, px_19, py_19, pz_19, cx_1_20, cy_1_20, cz_1_20, crossMagSq_1_20;
        const a_42 = Line3D_$ctor_76A78260(0, 0, 0, 10, 10, 10);
        const b_42 = Line3D_$ctor_76A78260(5, 5, 5, 15, 15, 15);
        Expect_isFalse((ln_301 = a_42, (other_43 = b_42, (d1x_43 = ((ln_302 = ln_301, ln_302.ToX - ln_302.FromX)), (d1y_43 = ((ln_303 = ln_301, ln_303.ToY - ln_303.FromY)), (d1z_43 = ((ln_304 = ln_301, ln_304.ToZ - ln_304.FromZ)), (d2x_43 = ((ln_305 = other_43, ln_305.ToX - ln_305.FromX)), (d2y_43 = ((ln_306 = other_43, ln_306.ToY - ln_306.FromY)), (d2z_43 = ((ln_307 = other_43, ln_307.ToZ - ln_307.FromZ)), (cx_43 = ((d1y_43 * d2z_43) - (d1z_43 * d2y_43)), (cy_43 = ((d1z_43 * d2x_43) - (d1x_43 * d2z_43)), (cz_43 = ((d1x_43 * d2y_43) - (d1y_43 * d2x_43)), (crossMagSq_43 = (((cx_43 * cx_43) + (cy_43 * cy_43)) + (cz_43 * cz_43)), (crossMagSq_43 < 1E-12) && ((dot_23 = (((d1x_43 * d2x_43) + (d1y_43 * d2y_43)) + (d1z_43 * d2z_43)), (dot_23 < -1E-06) && ((px_19 = (ln_301.FromX - other_43.FromX), (py_19 = (ln_301.FromY - other_43.FromY), (pz_19 = (ln_301.FromZ - other_43.FromZ), (cx_1_20 = ((py_19 * d2z_43) - (pz_19 * d2y_43)), (cy_1_20 = ((pz_19 * d2x_43) - (px_19 * d2z_43)), (cz_1_20 = ((px_19 * d2y_43) - (py_19 * d2x_43)), (crossMagSq_1_20 = (((cx_1_20 * cx_1_20) + (cy_1_20 * cy_1_20)) + (cz_1_20 * cz_1_20)), crossMagSq_1_20 < 1E-12)))))))))))))))))))))))("diagonal same ray same direction are not opposing");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})()]));

export const tests = Test_testList("All Line Tests", ofArray([testsIsCoincident, testsFastMethods, testsDistanceBetweenLines, testsLine2DBasics, testsLine2DExtendShrink, testsLine2DMove, testsLine2DClosestPoint, testsLine2DProjection, testsLine2DDivide, testsLine2DOffset, testsLine2DWithLength, testsLine2DIntersection, testsFastParallel3D, testsLine3DBasics, testsLine3DExtendShrink, testsLine3DMove, testsLine3DClosestPoint, testsLine3DProjection, testsLine3DDivide, testsLine3DOffset, testsLine3DWithLength, testsLine3DIntersection, testsLine3DDistance]));

