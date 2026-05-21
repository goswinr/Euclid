
import { Expect_isFalse, Expect_throws, Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { NPlane_equals, NPlane_intersectRay, NPlane_intersectLineParameter, NPlane_intersect, NPlane_create_5A66521A } from "./Src/NPlane.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_1 } from "./Src/Vec.js";
import { Vec_$ctor_Z7AD9E565 as Vec_$ctor_Z7AD9E565_2 } from "./Src/Vec.js";
import { NPlane_$ctor_Z2DDF2344 } from "./Src/NPlane.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { failUnit3 as failUnit3_1, failColinear } from "./Src/EuclidErrors.js";
import { failTooSmall, failUnit3 } from "./Src/EuclidErrors.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { contains, ofArray } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { structuralHash, Exception, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { equals, class_type, decimal_type, string_type, bool_type, int32_type, float64_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Line3D_$ctor_5A6659A0 } from "./Src/Line3D.js";
import { Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53 } from "./Src/TypeExtensions/PPlane.js";
import { PPlane_$ctor_3CB4665C } from "./Src/PPlane.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_2 } from "./Src/Pnt.js";

export const tests = Test_testList("Planes", ofArray([Test_testList("NPlane - Construction", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create from point and vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let a_2, b_2, x, y, z, v_2, a_3, v, b_3;
        const origin = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const plane = NPlane_create_5A66521A(origin, Vec_$ctor_Z7AD9E565(0, 0, 1));
        Expect_isTrue(((a_2 = plane.Origin, (b_2 = origin, (x = (a_2.X - b_2.X), (y = (a_2.Y - b_2.Y), (z = (a_2.Z - b_2.Z), Math.sqrt(((x * x) + (y * y)) + (z * z)))))))) < 1E-09)("Origin should match");
        Expect_isTrue(((v_2 = ((a_3 = ((v = plane.Normal, Vec_$ctor_Z7AD9E565_1(v.X, v.Y, v.Z))), (b_3 = Vec_$ctor_Z7AD9E565(0, 0, 1), Vec_$ctor_Z7AD9E565_2(a_3.X - b_3.X, a_3.Y - b_3.Y, a_3.Z - b_3.Z)))), Math.sqrt(((v_2.X * v_2.X) + (v_2.Y * v_2.Y)) + (v_2.Z * v_2.Z)))) < 1E-09)("Normal should be unitized");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create unitizes the normal vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let a_5, v_5, v_3;
        Expect_isTrue((a_5 = ((v_5 = ((v_3 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 10)).Normal, Vec_$ctor_Z7AD9E565_1(v_3.X, v_3.Y, v_3.Z))), Math.sqrt(((v_5.X * v_5.X) + (v_5.Y * v_5.Y)) + (v_5.Z * v_5.Z)))), Math.abs(a_5 - 1) < 1E-09))("Normal should be unit length");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create rejects zero-length normal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const origin_2 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const normal_2 = Vec_$ctor_Z7AD9E565_2(0, 0, 0);
        Expect_throws(() => {
            NPlane_create_5A66521A(origin_2, normal_2);
        }, "Should throw for zero normal");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create from UnitVec", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        let a_7, b_7, x_2, y_2, z_2, v_9, a_8, v_6, b_8, v_7;
        const origin_3 = Pnt_$ctor_Z7AD9E565(1, 2, 3);
        const plane_2 = NPlane_$ctor_Z2DDF2344(origin_3, UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        Expect_isTrue(((a_7 = plane_2.Origin, (b_7 = origin_3, (x_2 = (a_7.X - b_7.X), (y_2 = (a_7.Y - b_7.Y), (z_2 = (a_7.Z - b_7.Z), Math.sqrt(((x_2 * x_2) + (y_2 * y_2)) + (z_2 * z_2)))))))) < 1E-09)("Origin should match");
        Expect_isTrue(((v_9 = ((a_8 = ((v_6 = plane_2.Normal, Vec_$ctor_Z7AD9E565_1(v_6.X, v_6.Y, v_6.Z))), (b_8 = ((v_7 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), Vec_$ctor_Z7AD9E565_1(v_7.X, v_7.Y, v_7.Z))), Vec_$ctor_Z7AD9E565_2(a_8.X - b_8.X, a_8.Y - b_8.Y, a_8.Z - b_8.Z)))), Math.sqrt(((v_9.X * v_9.X) + (v_9.Y * v_9.Y)) + (v_9.Z * v_9.Z)))) < 1E-09)("Normal should match");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFrom3Points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        let v_10, v_11, x_5, y_4, z_4, l, f, a_17, b_17, x_8, y_6, z_6, a_20, a_18, b_18, a_19, b_19, a_23, a_21, b_21, a_22, b_22;
        const a_10 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_10 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const c = Pnt_$ctor_Z7AD9E565(0, 1, 0);
        let plane_3;
        const a_12 = a_10;
        const b_12 = b_10;
        const c_2 = c;
        let n;
        let a_15;
        const a_13 = c_2;
        const b_13 = b_12;
        a_15 = Vec_$ctor_Z7AD9E565_2(a_13.X - b_13.X, a_13.Y - b_13.Y, a_13.Z - b_13.Z);
        let b_15;
        const a_14 = a_12;
        const b_14 = b_12;
        b_15 = Vec_$ctor_Z7AD9E565_2(a_14.X - b_14.X, a_14.Y - b_14.Y, a_14.Z - b_14.Z);
        n = Vec_$ctor_Z7AD9E565_2((a_15.Y * b_15.Z) - (a_15.Z * b_15.Y), (a_15.Z * b_15.X) - (a_15.X * b_15.Z), (a_15.X * b_15.Y) - (a_15.Y * b_15.X));
        if (!(((v_10 = n, ((v_10.X * v_10.X) + (v_10.Y * v_10.Y)) + (v_10.Z * v_10.Z))) > 1E-12)) {
            failColinear("NPlane.createFrom3Points", a_12, b_12, c_2);
        }
        plane_3 = NPlane_$ctor_Z2DDF2344(a_12, (v_11 = n, (x_5 = v_11.X, (y_4 = v_11.Y, (z_4 = v_11.Z, (l = Math.sqrt(((x_5 * x_5) + (y_4 * y_4)) + (z_4 * z_4)), (!(l > 1E-12) ? failUnit3("Vec.Unitized", x_5, y_4, z_4) : undefined, (f = (1 / l), UnitVec_$ctor_Z7AD9E565(f * x_5, f * y_4, f * z_4)))))))));
        Expect_isTrue(((a_17 = plane_3.Origin, (b_17 = a_10, (x_8 = (a_17.X - b_17.X), (y_6 = (a_17.Y - b_17.Y), (z_6 = (a_17.Z - b_17.Z), Math.sqrt(((x_8 * x_8) + (y_6 * y_6)) + (z_6 * z_6)))))))) < 1E-09)("Origin should be at first point");
        const normal_5 = plane_3.Normal;
        Expect_isTrue((a_20 = ((a_18 = normal_5, (b_18 = ((a_19 = b_10, (b_19 = a_10, Vec_$ctor_Z7AD9E565_2(a_19.X - b_19.X, a_19.Y - b_19.Y, a_19.Z - b_19.Z)))), ((a_18.X * b_18.X) + (a_18.Y * b_18.Y)) + (a_18.Z * b_18.Z)))), Math.abs(a_20 - 0) < 1E-09))("Normal should be perpendicular to edge 1");
        Expect_isTrue((a_23 = ((a_21 = normal_5, (b_21 = ((a_22 = c, (b_22 = a_10, Vec_$ctor_Z7AD9E565_2(a_22.X - b_22.X, a_22.Y - b_22.Y, a_22.Z - b_22.Z)))), ((a_21.X * b_21.X) + (a_21.Y * b_21.Y)) + (a_21.Z * b_21.Z)))), Math.abs(a_23 - 0) < 1E-09))("Normal should be perpendicular to edge 2");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createFrom3Points rejects colinear points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const a_24 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const b_24 = Pnt_$ctor_Z7AD9E565(1, 0, 0);
        const c_3 = Pnt_$ctor_Z7AD9E565(2, 0, 0);
        Expect_throws(() => {
            let a_26, b_26, c_5, n_1, a_29, a_27, b_27, b_29, a_28, b_28, v_12, v_13, x_10, y_7, z_7, l_1, f_1;
            (a_26 = a_24, (b_26 = b_24, (c_5 = c_3, (n_1 = ((a_29 = ((a_27 = c_5, (b_27 = b_26, Vec_$ctor_Z7AD9E565_2(a_27.X - b_27.X, a_27.Y - b_27.Y, a_27.Z - b_27.Z)))), (b_29 = ((a_28 = a_26, (b_28 = b_26, Vec_$ctor_Z7AD9E565_2(a_28.X - b_28.X, a_28.Y - b_28.Y, a_28.Z - b_28.Z)))), Vec_$ctor_Z7AD9E565_2((a_29.Y * b_29.Z) - (a_29.Z * b_29.Y), (a_29.Z * b_29.X) - (a_29.X * b_29.Z), (a_29.X * b_29.Y) - (a_29.Y * b_29.X))))), (!(((v_12 = n_1, ((v_12.X * v_12.X) + (v_12.Y * v_12.Y)) + (v_12.Z * v_12.Z))) > 1E-12) ? failColinear("NPlane.createFrom3Points", a_26, b_26, c_5) : undefined, NPlane_$ctor_Z2DDF2344(a_26, (v_13 = n_1, (x_10 = v_13.X, (y_7 = v_13.Y, (z_7 = v_13.Z, (l_1 = Math.sqrt(((x_10 * x_10) + (y_7 * y_7)) + (z_7 * z_7)), (!(l_1 > 1E-12) ? failUnit3("Vec.Unitized", x_10, y_7, z_7) : undefined, (f_1 = (1 / l_1), UnitVec_$ctor_Z7AD9E565(f_1 * x_10, f_1 * y_7, f_1 * z_7))))))))))))));
        }, "Should throw for colinear points");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("xyPlane is at world origin with Z normal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        let a_31, b_31, x_14, y_10, z_10, v_16, a_32, v_14, b_32;
        const plane_4 = NPlane_$ctor_Z2DDF2344(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        Expect_isTrue(((a_31 = plane_4.Origin, (b_31 = Pnt_$ctor_Z7AD9E565_1(0, 0, 0), (x_14 = (a_31.X - b_31.X), (y_10 = (a_31.Y - b_31.Y), (z_10 = (a_31.Z - b_31.Z), Math.sqrt(((x_14 * x_14) + (y_10 * y_10)) + (z_10 * z_10)))))))) < 1E-09)("Origin should be at world origin");
        Expect_isTrue(((v_16 = ((a_32 = ((v_14 = plane_4.Normal, Vec_$ctor_Z7AD9E565_1(v_14.X, v_14.Y, v_14.Z))), (b_32 = Vec_$ctor_Z7AD9E565(0, 0, 1), Vec_$ctor_Z7AD9E565_2(a_32.X - b_32.X, a_32.Y - b_32.Y, a_32.Z - b_32.Z)))), Math.sqrt(((v_16.X * v_16.X) + (v_16.Y * v_16.Y)) + (v_16.Z * v_16.Z)))) < 1E-09)("Normal should be Z-axis");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})()])), Test_testList("NPlane - Distance and Projection", ofArray([(() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("DistanceToPt for point above plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let copyOfStruct, arg_5, arg_1_2;
        let actual;
        const pl = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const a_35 = pl.Normal;
        let b_35;
        const a_34 = Pnt_$ctor_Z7AD9E565(5, 5, 10);
        const b_34 = pl.Origin;
        b_35 = Vec_$ctor_Z7AD9E565_2(a_34.X - b_34.X, a_34.Y - b_34.Y, a_34.Z - b_34.Z);
        actual = (((a_35.X * b_35.X) + (a_35.Y * b_35.Y)) + (a_35.Z * b_35.Z));
        if ((actual === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual, 10, "Distance should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_15) => (structuralHash(x_15) | 0),
            }) ? ((arg_5 = (10).toString(), (arg_1_2 = actual.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_5)(arg_1_2)("Distance should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual)("Distance should be 10"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("DistanceToPt for point below plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let copyOfStruct_1, arg_7, arg_1_4;
        let actual_1;
        const pl_1 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const a_37 = pl_1.Normal;
        let b_37;
        const a_36 = Pnt_$ctor_Z7AD9E565(5, 5, -10);
        const b_36 = pl_1.Origin;
        b_37 = Vec_$ctor_Z7AD9E565_2(a_36.X - b_36.X, a_36.Y - b_36.Y, a_36.Z - b_36.Z);
        actual_1 = (((a_37.X * b_37.X) + (a_37.Y * b_37.Y)) + (a_37.Z * b_37.Z));
        if ((actual_1 === -10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, -10, "Distance should be -10 (signed)");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_1, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_16) => (structuralHash(x_16) | 0),
            }) ? ((arg_7 = (-10).toString(), (arg_1_4 = actual_1.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_4)("Distance should be -10 (signed)")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(-10)(actual_1)("Distance should be -10 (signed)"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("DistanceToPt for point on plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let dist_2;
        const pl_2 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 5), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const a_39 = pl_2.Normal;
        let b_39;
        const a_38 = Pnt_$ctor_Z7AD9E565(10, 10, 5);
        const b_38 = pl_2.Origin;
        b_39 = Vec_$ctor_Z7AD9E565_2(a_38.X - b_38.X, a_38.Y - b_38.Y, a_38.Z - b_38.Z);
        dist_2 = (((a_39.X * b_39.X) + (a_39.Y * b_39.Y)) + (a_39.Z * b_39.Z));
        Expect_isTrue(Math.abs(dist_2 - 0) < 1E-09)("Distance should be 0");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestPoint on plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_45, pl_3, pt_8, p, v_17, a_43, f_2, pl_4, a_42, b_42, a_41, b_41, b_44, x_17, y_13, z_11;
        Expect_isTrue(((a_45 = ((pl_3 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), (pt_8 = Pnt_$ctor_Z7AD9E565(5, 5, 10), (p = pt_8, (v_17 = ((a_43 = pl_3.Normal, (f_2 = ((pl_4 = pl_3, (a_42 = pl_4.Normal, (b_42 = ((a_41 = pt_8, (b_41 = pl_4.Origin, Vec_$ctor_Z7AD9E565_2(a_41.X - b_41.X, a_41.Y - b_41.Y, a_41.Z - b_41.Z)))), ((a_42.X * b_42.X) + (a_42.Y * b_42.Y)) + (a_42.Z * b_42.Z))))), Vec_$ctor_Z7AD9E565_2(a_43.X * f_2, a_43.Y * f_2, a_43.Z * f_2)))), Pnt_$ctor_Z7AD9E565_1(p.X - v_17.X, p.Y - v_17.Y, p.Z - v_17.Z)))))), (b_44 = Pnt_$ctor_Z7AD9E565(5, 5, 0), (x_17 = (a_45.X - b_44.X), (y_13 = (a_45.Y - b_44.Y), (z_11 = (a_45.Z - b_44.Z), Math.sqrt(((x_17 * x_17) + (y_13 * y_13)) + (z_11 * z_11)))))))) < 1E-09)("Closest point should be (5, 5, 0)");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestPoint for point already on plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_50, pl_5, pt_11, p_1, v_18, a_48, f_3, pl_6, a_47, b_46, a_46, b_45, b_48, x_18, y_14, z_12;
        const plane_9 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const pt_10 = Pnt_$ctor_Z7AD9E565(5, 5, 0);
        Expect_isTrue(((a_50 = ((pl_5 = plane_9, (pt_11 = pt_10, (p_1 = pt_11, (v_18 = ((a_48 = pl_5.Normal, (f_3 = ((pl_6 = pl_5, (a_47 = pl_6.Normal, (b_46 = ((a_46 = pt_11, (b_45 = pl_6.Origin, Vec_$ctor_Z7AD9E565_2(a_46.X - b_45.X, a_46.Y - b_45.Y, a_46.Z - b_45.Z)))), ((a_47.X * b_46.X) + (a_47.Y * b_46.Y)) + (a_47.Z * b_46.Z))))), Vec_$ctor_Z7AD9E565_2(a_48.X * f_3, a_48.Y * f_3, a_48.Z * f_3)))), Pnt_$ctor_Z7AD9E565_1(p_1.X - v_18.X, p_1.Y - v_18.Y, p_1.Z - v_18.Z)))))), (b_48 = pt_10, (x_18 = (a_50.X - b_48.X), (y_14 = (a_50.Y - b_48.Y), (z_12 = (a_50.Z - b_48.Z), Math.sqrt(((x_18 * x_18) + (y_14 * y_14)) + (z_12 * z_12)))))))) < 1E-09)("Closest point should be same as input");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("PlaneAtClPt", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let p_2, v_19, a_53, f_4, pl_8, a_52, b_50, a_51, b_49, a_55, b_52, x_19, y_15, z_13, v_23, a_56, v_20, b_53, v_21;
        const plane_10 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        let newPlane;
        const pl_7 = plane_10;
        const pt_14 = Pnt_$ctor_Z7AD9E565(5, 5, 10);
        newPlane = NPlane_$ctor_Z2DDF2344((p_2 = pt_14, (v_19 = ((a_53 = pl_7.Normal, (f_4 = ((pl_8 = pl_7, (a_52 = pl_8.Normal, (b_50 = ((a_51 = pt_14, (b_49 = pl_8.Origin, Vec_$ctor_Z7AD9E565_2(a_51.X - b_49.X, a_51.Y - b_49.Y, a_51.Z - b_49.Z)))), ((a_52.X * b_50.X) + (a_52.Y * b_50.Y)) + (a_52.Z * b_50.Z))))), Vec_$ctor_Z7AD9E565_2(a_53.X * f_4, a_53.Y * f_4, a_53.Z * f_4)))), Pnt_$ctor_Z7AD9E565_1(p_2.X - v_19.X, p_2.Y - v_19.Y, p_2.Z - v_19.Z))), pl_7.Normal);
        Expect_isTrue(((a_55 = newPlane.Origin, (b_52 = Pnt_$ctor_Z7AD9E565(5, 5, 0), (x_19 = (a_55.X - b_52.X), (y_15 = (a_55.Y - b_52.Y), (z_13 = (a_55.Z - b_52.Z), Math.sqrt(((x_19 * x_19) + (y_15 * y_15)) + (z_13 * z_13)))))))) < 1E-09)("New plane origin should be at closest point");
        Expect_isTrue(((v_23 = ((a_56 = ((v_20 = newPlane.Normal, Vec_$ctor_Z7AD9E565_1(v_20.X, v_20.Y, v_20.Z))), (b_53 = ((v_21 = plane_10.Normal, Vec_$ctor_Z7AD9E565_1(v_21.X, v_21.Y, v_21.Z))), Vec_$ctor_Z7AD9E565_2(a_56.X - b_53.X, a_56.Y - b_53.Y, a_56.Z - b_53.Z)))), Math.sqrt(((v_23.X * v_23.X) + (v_23.Y * v_23.Y)) + (v_23.Z * v_23.Z)))) < 1E-09)("Normal should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})()])), Test_testList("NPlane - Angles", ofArray([(() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToPlane for parallel planes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        let a_61, b_58, dot, a_62, b_59, dotAbs, x_20, y_16, z_14, x_21, y_17, z_15;
        const angle = 57.29577951308232 * ((a_61 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)).Normal, (b_58 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 10), Vec_$ctor_Z7AD9E565(0, 0, 1)).Normal, (dot = ((a_62 = a_61, (b_59 = b_58, ((a_62.X * b_59.X) + (a_62.Y * b_59.Y)) + (a_62.Z * b_59.Z)))), (dotAbs = Math.abs(dot), (dotAbs < 0.98) ? Math.acos(dotAbs) : ((dot < 0) ? (2 * Math.asin(((x_20 = (b_58.X - -a_61.X), (y_16 = (b_58.Y - -a_61.Y), (z_14 = (b_58.Z - -a_61.Z), Math.sqrt(((x_20 * x_20) + (y_16 * y_16)) + (z_14 * z_14)))))) * 0.5)) : (2 * Math.asin(((x_21 = (b_58.X - a_61.X), (y_17 = (b_58.Y - a_61.Y), (z_15 = (b_58.Z - a_61.Z), Math.sqrt(((x_21 * x_21) + (y_17 * y_17)) + (z_15 * z_15)))))) * 0.5))))))));
        Expect_isTrue(Math.abs(angle - 0) < 1E-09)("Parallel planes should have 0 angle");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToPlane for perpendicular planes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        let a_67, b_64, dot_1, a_68, b_65, dotAbs_1, x_22, y_18, z_16, x_23, y_19, z_17;
        const angle_1 = 57.29577951308232 * ((a_67 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)).Normal, (b_64 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(1, 0, 0)).Normal, (dot_1 = ((a_68 = a_67, (b_65 = b_64, ((a_68.X * b_65.X) + (a_68.Y * b_65.Y)) + (a_68.Z * b_65.Z)))), (dotAbs_1 = Math.abs(dot_1), (dotAbs_1 < 0.98) ? Math.acos(dotAbs_1) : ((dot_1 < 0) ? (2 * Math.asin(((x_22 = (b_64.X - -a_67.X), (y_18 = (b_64.Y - -a_67.Y), (z_16 = (b_64.Z - -a_67.Z), Math.sqrt(((x_22 * x_22) + (y_18 * y_18)) + (z_16 * z_16)))))) * 0.5)) : (2 * Math.asin(((x_23 = (b_64.X - a_67.X), (y_19 = (b_64.Y - a_67.Y), (z_17 = (b_64.Z - a_67.Z), Math.sqrt(((x_23 * x_23) + (y_19 * y_19)) + (z_17 * z_17)))))) * 0.5))))))));
        Expect_isTrue(Math.abs(angle_1 - 90) < 1E-09)("Perpendicular planes should have 90 degree angle");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToVec for parallel vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        let a_72, v_25, x_24, y_20, z_18, l_2, f_5, b_69, dot_2, a_73, b_70, dotAbs_2, x_27, y_22, z_20, x_28, y_23, z_21;
        const plane_11 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const angle_2 = 90 - (57.29577951308232 * ((a_72 = ((v_25 = Vec_$ctor_Z7AD9E565(1, 0, 0), (x_24 = v_25.X, (y_20 = v_25.Y, (z_18 = v_25.Z, (l_2 = Math.sqrt(((x_24 * x_24) + (y_20 * y_20)) + (z_18 * z_18)), (!(l_2 > 1E-12) ? failUnit3("Vec.Unitized", x_24, y_20, z_18) : undefined, (f_5 = (1 / l_2), UnitVec_$ctor_Z7AD9E565(f_5 * x_24, f_5 * y_20, f_5 * z_18))))))))), (b_69 = plane_11.Normal, (dot_2 = ((a_73 = a_72, (b_70 = b_69, ((a_73.X * b_70.X) + (a_73.Y * b_70.Y)) + (a_73.Z * b_70.Z)))), (dotAbs_2 = Math.abs(dot_2), (dotAbs_2 < 0.98) ? Math.acos(dotAbs_2) : ((dot_2 < 0) ? (2 * Math.asin(((x_27 = (b_69.X - -a_72.X), (y_22 = (b_69.Y - -a_72.Y), (z_20 = (b_69.Z - -a_72.Z), Math.sqrt(((x_27 * x_27) + (y_22 * y_22)) + (z_20 * z_20)))))) * 0.5)) : (2 * Math.asin(((x_28 = (b_69.X - a_72.X), (y_23 = (b_69.Y - a_72.Y), (z_21 = (b_69.Z - a_72.Z), Math.sqrt(((x_28 * x_28) + (y_23 * y_23)) + (z_21 * z_21)))))) * 0.5)))))))));
        Expect_isTrue(Math.abs(angle_2 - 0) < 1E-09)("Parallel vector should have 0 angle");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToVec for perpendicular vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let a_77, v_27, x_29, y_24, z_22, l_3, f_6, b_74, dot_3, a_78, b_75, dotAbs_3, x_32, y_26, z_24, x_33, y_27, z_25;
        const plane_12 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const angle_3 = 90 - (57.29577951308232 * ((a_77 = ((v_27 = Vec_$ctor_Z7AD9E565(0, 0, 1), (x_29 = v_27.X, (y_24 = v_27.Y, (z_22 = v_27.Z, (l_3 = Math.sqrt(((x_29 * x_29) + (y_24 * y_24)) + (z_22 * z_22)), (!(l_3 > 1E-12) ? failUnit3("Vec.Unitized", x_29, y_24, z_22) : undefined, (f_6 = (1 / l_3), UnitVec_$ctor_Z7AD9E565(f_6 * x_29, f_6 * y_24, f_6 * z_22))))))))), (b_74 = plane_12.Normal, (dot_3 = ((a_78 = a_77, (b_75 = b_74, ((a_78.X * b_75.X) + (a_78.Y * b_75.Y)) + (a_78.Z * b_75.Z)))), (dotAbs_3 = Math.abs(dot_3), (dotAbs_3 < 0.98) ? Math.acos(dotAbs_3) : ((dot_3 < 0) ? (2 * Math.asin(((x_32 = (b_74.X - -a_77.X), (y_26 = (b_74.Y - -a_77.Y), (z_24 = (b_74.Z - -a_77.Z), Math.sqrt(((x_32 * x_32) + (y_26 * y_26)) + (z_24 * z_24)))))) * 0.5)) : (2 * Math.asin(((x_33 = (b_74.X - a_77.X), (y_27 = (b_74.Y - a_77.Y), (z_25 = (b_74.Z - a_77.Z), Math.sqrt(((x_33 * x_33) + (y_27 * y_27)) + (z_25 * z_25)))))) * 0.5)))))))));
        Expect_isTrue(Math.abs(angle_3 - 90) < 1E-09)("Perpendicular vector should have 90 degree angle");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToLine for parallel line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        let a_82, ln_1, x_34, ln_2, y_28, ln_3, z_26, ln_4, l_4, s, b_79, dot_4, a_83, b_80, dotAbs_4, x_37, y_30, z_28, x_38, y_31, z_29;
        const plane_13 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const angle_4 = 90 - (57.29577951308232 * ((a_82 = ((ln_1 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), (x_34 = ((ln_2 = ln_1, ln_2.ToX - ln_2.FromX)), (y_28 = ((ln_3 = ln_1, ln_3.ToY - ln_3.FromY)), (z_26 = ((ln_4 = ln_1, ln_4.ToZ - ln_4.FromZ)), (l_4 = Math.sqrt(((x_34 * x_34) + (y_28 * y_28)) + (z_26 * z_26)), (!(l_4 > 1E-12) ? failUnit3_1("Line3D.UnitTangent", x_34, y_28, z_26) : undefined, (s = (1 / l_4), UnitVec_$ctor_Z7AD9E565(x_34 * s, y_28 * s, z_26 * s))))))))), (b_79 = plane_13.Normal, (dot_4 = ((a_83 = a_82, (b_80 = b_79, ((a_83.X * b_80.X) + (a_83.Y * b_80.Y)) + (a_83.Z * b_80.Z)))), (dotAbs_4 = Math.abs(dot_4), (dotAbs_4 < 0.98) ? Math.acos(dotAbs_4) : ((dot_4 < 0) ? (2 * Math.asin(((x_37 = (b_79.X - -a_82.X), (y_30 = (b_79.Y - -a_82.Y), (z_28 = (b_79.Z - -a_82.Z), Math.sqrt(((x_37 * x_37) + (y_30 * y_30)) + (z_28 * z_28)))))) * 0.5)) : (2 * Math.asin(((x_38 = (b_79.X - a_82.X), (y_31 = (b_79.Y - a_82.Y), (z_29 = (b_79.Z - a_82.Z), Math.sqrt(((x_38 * x_38) + (y_31 * y_31)) + (z_29 * z_29)))))) * 0.5)))))))));
        Expect_isTrue(Math.abs(angle_4 - 0) < 1E-09)("Parallel line should have 0 angle");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToLine for perpendicular line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let a_87, ln_6, x_39, ln_7, y_32, ln_8, z_30, ln_9, l_5, s_1, b_84, dot_5, a_88, b_85, dotAbs_5, x_42, y_34, z_32, x_43, y_35, z_33;
        const plane_14 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const angle_5 = 90 - (57.29577951308232 * ((a_87 = ((ln_6 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10)), (x_39 = ((ln_7 = ln_6, ln_7.ToX - ln_7.FromX)), (y_32 = ((ln_8 = ln_6, ln_8.ToY - ln_8.FromY)), (z_30 = ((ln_9 = ln_6, ln_9.ToZ - ln_9.FromZ)), (l_5 = Math.sqrt(((x_39 * x_39) + (y_32 * y_32)) + (z_30 * z_30)), (!(l_5 > 1E-12) ? failUnit3_1("Line3D.UnitTangent", x_39, y_32, z_30) : undefined, (s_1 = (1 / l_5), UnitVec_$ctor_Z7AD9E565(x_39 * s_1, y_32 * s_1, z_30 * s_1))))))))), (b_84 = plane_14.Normal, (dot_5 = ((a_88 = a_87, (b_85 = b_84, ((a_88.X * b_85.X) + (a_88.Y * b_85.Y)) + (a_88.Z * b_85.Z)))), (dotAbs_5 = Math.abs(dot_5), (dotAbs_5 < 0.98) ? Math.acos(dotAbs_5) : ((dot_5 < 0) ? (2 * Math.asin(((x_42 = (b_84.X - -a_87.X), (y_34 = (b_84.Y - -a_87.Y), (z_32 = (b_84.Z - -a_87.Z), Math.sqrt(((x_42 * x_42) + (y_34 * y_34)) + (z_32 * z_32)))))) * 0.5)) : (2 * Math.asin(((x_43 = (b_84.X - a_87.X), (y_35 = (b_84.Y - a_87.Y), (z_33 = (b_84.Z - a_87.Z), Math.sqrt(((x_43 * x_43) + (y_35 * y_35)) + (z_33 * z_33)))))) * 0.5)))))))));
        Expect_isTrue(Math.abs(angle_5 - 90) < 1E-09)("Perpendicular line should have 90 degree angle");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})()])), Test_testList("NPlane - Coincidence", ofArray([(() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo for same plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let pl_15, other, a_91, b_88, pl_16, a_93, b_90, a_92, b_89;
        Expect_isTrue((pl_15 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), (other = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), (Math.abs((a_91 = other.Normal, (b_88 = pl_15.Normal, ((a_91.X * b_88.X) + (a_91.Y * b_88.Y)) + (a_91.Z * b_88.Z)))) > 0.9999904807207345) && (((pl_16 = pl_15, (a_93 = pl_16.Normal, (b_90 = ((a_92 = other.Origin, (b_89 = pl_16.Origin, Vec_$ctor_Z7AD9E565_2(a_92.X - b_89.X, a_92.Y - b_89.Y, a_92.Z - b_89.Z)))), ((a_93.X * b_90.X) + (a_93.Y * b_90.Y)) + (a_93.Z * b_90.Z))))) < 1E-06))))("Same plane should be coincident");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo for parallel planes at same height", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let pl_17, other_2, a_95, b_92, pl_18, a_97, b_94, a_96, b_93;
        Expect_isTrue((pl_17 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 5), Vec_$ctor_Z7AD9E565(0, 0, 1)), (other_2 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(10, 10, 5), Vec_$ctor_Z7AD9E565(0, 0, 1)), (Math.abs((a_95 = other_2.Normal, (b_92 = pl_17.Normal, ((a_95.X * b_92.X) + (a_95.Y * b_92.Y)) + (a_95.Z * b_92.Z)))) > 0.9999904807207345) && (((pl_18 = pl_17, (a_97 = pl_18.Normal, (b_94 = ((a_96 = other_2.Origin, (b_93 = pl_18.Origin, Vec_$ctor_Z7AD9E565_2(a_96.X - b_93.X, a_96.Y - b_93.Y, a_96.Z - b_93.Z)))), ((a_97.X * b_94.X) + (a_97.Y * b_94.Y)) + (a_97.Z * b_94.Z))))) < 1E-06))))("Parallel planes at same height should be coincident");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo for parallel but separated planes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let pl_19, other_4, a_99, b_96, pl_20, a_101, b_98, a_100, b_97;
        Expect_isFalse((pl_19 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), (other_4 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 10), Vec_$ctor_Z7AD9E565(0, 0, 1)), (Math.abs((a_99 = other_4.Normal, (b_96 = pl_19.Normal, ((a_99.X * b_96.X) + (a_99.Y * b_96.Y)) + (a_99.Z * b_96.Z)))) > 0.9999904807207345) && (((pl_20 = pl_19, (a_101 = pl_20.Normal, (b_98 = ((a_100 = other_4.Origin, (b_97 = pl_20.Origin, Vec_$ctor_Z7AD9E565_2(a_100.X - b_97.X, a_100.Y - b_97.Y, a_100.Z - b_97.Z)))), ((a_101.X * b_98.X) + (a_101.Y * b_98.Y)) + (a_101.Z * b_98.Z))))) < 1E-06))))("Separated parallel planes should not be coincident");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo for non-parallel planes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let pl_21, other_6, a_103, b_100, pl_22, a_105, b_102, a_104, b_101;
        Expect_isFalse((pl_21 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), (other_6 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(1, 0, 0)), (Math.abs((a_103 = other_6.Normal, (b_100 = pl_21.Normal, ((a_103.X * b_100.X) + (a_103.Y * b_100.Y)) + (a_103.Z * b_100.Z)))) > 0.9999904807207345) && (((pl_22 = pl_21, (a_105 = pl_22.Normal, (b_102 = ((a_104 = other_6.Origin, (b_101 = pl_22.Origin, Vec_$ctor_Z7AD9E565_2(a_104.X - b_101.X, a_104.Y - b_101.Y, a_104.Z - b_101.Z)))), ((a_105.X * b_102.X) + (a_105.Y * b_102.Y)) + (a_105.Z * b_102.Z))))) < 1E-06))))("Non-parallel planes should not be coincident");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areCoincident static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let pl_23, other_8, a_109, b_106, pl_24, a_111, b_108, a_110, b_107;
        Expect_isTrue((pl_23 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 5), Vec_$ctor_Z7AD9E565(0, 0, 1)), (other_8 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(10, 10, 5), Vec_$ctor_Z7AD9E565(0, 0, 1)), (Math.abs((a_109 = other_8.Normal, (b_106 = pl_23.Normal, ((a_109.X * b_106.X) + (a_109.Y * b_106.Y)) + (a_109.Z * b_106.Z)))) > 0.9999904807207345) && (((pl_24 = pl_23, (a_111 = pl_24.Normal, (b_108 = ((a_110 = other_8.Origin, (b_107 = pl_24.Origin, Vec_$ctor_Z7AD9E565_2(a_110.X - b_107.X, a_110.Y - b_107.Y, a_110.Z - b_107.Z)))), ((a_111.X * b_108.X) + (a_111.Y * b_108.Y)) + (a_111.Z * b_108.Z))))) < 1E-06))))("Static method should work");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})()])), Test_testList("NPlane - Intersection", ofArray([(() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("intersect two planes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let a_115, ln_10, b_112, x_44, y_36, z_34;
        const matchValue = NPlane_intersect(NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 1, 0)));
        if (matchValue == null) {
            throw new Exception("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_24);
        }
        else {
            Expect_isTrue(((a_115 = ((ln_10 = matchValue, Pnt_$ctor_Z7AD9E565_1(ln_10.FromX, ln_10.FromY, ln_10.FromZ))), (b_112 = Pnt_$ctor_Z7AD9E565_1(0, 0, 0), (x_44 = (a_115.X - b_112.X), (y_36 = (a_115.Y - b_112.Y), (z_34 = (a_115.Z - b_112.Z), Math.sqrt(((x_44 * x_44) + (y_36 * y_36)) + (z_34 * z_34)))))))) < 1E-09)("Intersection line should pass through origin");
            Test_TestCaseBuilder__Zero(builder$0040_24);
        }
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("intersect parallel planes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        const matchValue_1 = NPlane_intersect(NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 10), Vec_$ctor_Z7AD9E565(0, 0, 1)));
        if (matchValue_1 == null) {
            Test_TestCaseBuilder__Zero(builder$0040_25);
        }
        else {
            throw new Exception("Parallel planes should not intersect");
            Test_TestCaseBuilder__Zero(builder$0040_25);
        }
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("intersectLineParameter for intersecting line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        const plane_15 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 5), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const matchValue_2 = NPlane_intersectLineParameter(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10)), plane_15);
        if (matchValue_2 == null) {
            throw new Exception("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_26);
        }
        else {
            const t = matchValue_2;
            Expect_isTrue(Math.abs(t - 0.5) < 1E-09)("Should intersect at t=0.5");
            Test_TestCaseBuilder__Zero(builder$0040_26);
        }
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("intersectLineParameter for parallel line", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        const plane_16 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 5), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const matchValue_3 = NPlane_intersectLineParameter(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0)), plane_16);
        if (matchValue_3 == null) {
            Test_TestCaseBuilder__Zero(builder$0040_27);
        }
        else {
            throw new Exception("Parallel line should not intersect");
            Test_TestCaseBuilder__Zero(builder$0040_27);
        }
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("intersectRay returns point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        let a_120, b_117, x_45, y_37, z_35;
        const plane_17 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 5), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const matchValue_4 = NPlane_intersectRay(Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10)), plane_17);
        if (matchValue_4 == null) {
            throw new Exception("Should have intersection");
            Test_TestCaseBuilder__Zero(builder$0040_28);
        }
        else {
            Expect_isTrue(((a_120 = matchValue_4, (b_117 = Pnt_$ctor_Z7AD9E565(0, 0, 5), (x_45 = (a_120.X - b_117.X), (y_37 = (a_120.Y - b_117.Y), (z_35 = (a_120.Z - b_117.Z), Math.sqrt(((x_45 * x_45) + (y_37 * y_37)) + (z_35 * z_35)))))))) < 1E-09)("Intersection should be at (0, 0, 5)");
            Test_TestCaseBuilder__Zero(builder$0040_28);
        }
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doLinePlaneIntersect for finite line intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        let ln_15, pl_29, n_2, nenner, a_121, ln_16, ln_17, ln_18, ln_19, b_118, x_46, a_123, a_122, b_119, ln_20, b_120;
        const plane_18 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 5), Vec_$ctor_Z7AD9E565(0, 0, 1));
        Expect_isTrue((ln_15 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 10)), (pl_29 = plane_18, (n_2 = pl_29.Normal, (nenner = ((a_121 = ((ln_16 = ln_15, Vec_$ctor_Z7AD9E565_2((ln_17 = ln_16, ln_17.ToX - ln_17.FromX), (ln_18 = ln_16, ln_18.ToY - ln_18.FromY), (ln_19 = ln_16, ln_19.ToZ - ln_19.FromZ)))), (b_118 = n_2, ((a_121.X * b_118.X) + (a_121.Y * b_118.Y)) + (a_121.Z * b_118.Z)))), (x_46 = (((a_123 = ((a_122 = pl_29.Origin, (b_119 = ((ln_20 = ln_15, Pnt_$ctor_Z7AD9E565_1(ln_20.FromX, ln_20.FromY, ln_20.FromZ))), Vec_$ctor_Z7AD9E565_2(a_122.X - b_119.X, a_122.Y - b_119.Y, a_122.Z - b_119.Z)))), (b_120 = n_2, ((a_123.X * b_120.X) + (a_123.Y * b_120.Y)) + (a_123.Z * b_120.Z)))) / nenner), (-1E-06 < x_46) && (x_46 < 1.000001)))))))("Line should intersect plane");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("doLinePlaneIntersect for finite line not intersecting", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        let ln_22, pl_31, n_3, nenner_1, a_124, ln_23, ln_24, ln_25, ln_26, b_121, x_47, a_126, a_125, b_122, ln_27, b_123;
        const plane_19 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 5), Vec_$ctor_Z7AD9E565(0, 0, 1));
        Expect_isFalse((ln_22 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(0, 0, 3)), (pl_31 = plane_19, (n_3 = pl_31.Normal, (nenner_1 = ((a_124 = ((ln_23 = ln_22, Vec_$ctor_Z7AD9E565_2((ln_24 = ln_23, ln_24.ToX - ln_24.FromX), (ln_25 = ln_23, ln_25.ToY - ln_25.FromY), (ln_26 = ln_23, ln_26.ToZ - ln_26.FromZ)))), (b_121 = n_3, ((a_124.X * b_121.X) + (a_124.Y * b_121.Y)) + (a_124.Z * b_121.Z)))), (x_47 = (((a_126 = ((a_125 = pl_31.Origin, (b_122 = ((ln_27 = ln_22, Pnt_$ctor_Z7AD9E565_1(ln_27.FromX, ln_27.FromY, ln_27.FromZ))), Vec_$ctor_Z7AD9E565_2(a_125.X - b_122.X, a_125.Y - b_122.Y, a_125.Z - b_122.Z)))), (b_123 = n_3, ((a_126.X * b_123.X) + (a_126.Y * b_123.Y)) + (a_126.Z * b_123.Z)))) / nenner_1), (-1E-06 < x_47) && (x_47 < 1.000001)))))))("Line should not intersect plane");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})()])), Test_testList("NPlane - Transformation", ofArray([(() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Flipped reverses normal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        let v_28, v_31, a_127, v_29, b_124, a_130, b_127, x_49, y_39, z_37;
        const plane_20 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        let flipped;
        const pl_32 = plane_20;
        flipped = NPlane_$ctor_Z2DDF2344(pl_32.Origin, (v_28 = pl_32.Normal, UnitVec_$ctor_Z7AD9E565(-v_28.X, -v_28.Y, -v_28.Z)));
        Expect_isTrue(((v_31 = ((a_127 = ((v_29 = flipped.Normal, Vec_$ctor_Z7AD9E565_1(v_29.X, v_29.Y, v_29.Z))), (b_124 = Vec_$ctor_Z7AD9E565(0, 0, -1), Vec_$ctor_Z7AD9E565_2(a_127.X - b_124.X, a_127.Y - b_124.Y, a_127.Z - b_124.Z)))), Math.sqrt(((v_31.X * v_31.X) + (v_31.Y * v_31.Y)) + (v_31.Z * v_31.Z)))) < 1E-09)("Normal should be flipped");
        Expect_isTrue(((a_130 = flipped.Origin, (b_127 = plane_20.Origin, (x_49 = (a_130.X - b_127.X), (y_39 = (a_130.Y - b_127.Y), (z_37 = (a_130.Z - b_127.Z), Math.sqrt(((x_49 * x_49) + (y_39 * y_39)) + (z_37 * z_37)))))))) < 1E-09)("Origin should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offset along normal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        let p_3, v_32, a_131, a_133, b_129, x_50, y_40, z_38, v_36, a_134, v_33, b_130, v_34;
        const plane_21 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        let offset;
        const pl_34 = plane_21;
        offset = NPlane_$ctor_Z2DDF2344((p_3 = pl_34.Origin, (v_32 = ((a_131 = pl_34.Normal, Vec_$ctor_Z7AD9E565_2(a_131.X * 5, a_131.Y * 5, a_131.Z * 5))), Pnt_$ctor_Z7AD9E565_1(p_3.X + v_32.X, p_3.Y + v_32.Y, p_3.Z + v_32.Z))), pl_34.Normal);
        Expect_isTrue(((a_133 = offset.Origin, (b_129 = Pnt_$ctor_Z7AD9E565(0, 0, 5), (x_50 = (a_133.X - b_129.X), (y_40 = (a_133.Y - b_129.Y), (z_38 = (a_133.Z - b_129.Z), Math.sqrt(((x_50 * x_50) + (y_40 * y_40)) + (z_38 * z_38)))))))) < 1E-09)("Origin should be offset");
        Expect_isTrue(((v_36 = ((a_134 = ((v_33 = offset.Normal, Vec_$ctor_Z7AD9E565_1(v_33.X, v_33.Y, v_33.Z))), (b_130 = ((v_34 = plane_21.Normal, Vec_$ctor_Z7AD9E565_1(v_34.X, v_34.Y, v_34.Z))), Vec_$ctor_Z7AD9E565_2(a_134.X - b_130.X, a_134.Y - b_130.Y, a_134.Z - b_130.Z)))), Math.sqrt(((v_36.X * v_36.X) + (v_36.Y * v_36.Y)) + (v_36.Z * v_36.Z)))) < 1E-09)("Normal should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetInDir towards point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        let a_141, pl_36, a_137, b_133, a_136, b_132, p_4, v_37, a_138, f_8, p_5, v_38, a_139, f_9, b_135, x_51, y_41, z_39;
        Expect_isTrue(((a_141 = ((pl_36 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), (((a_137 = pl_36.Normal, (b_133 = ((a_136 = Pnt_$ctor_Z7AD9E565(0, 0, 10), (b_132 = pl_36.Origin, Vec_$ctor_Z7AD9E565_2(a_136.X - b_132.X, a_136.Y - b_132.Y, a_136.Z - b_132.Z)))), ((a_137.X * b_133.X) + (a_137.Y * b_133.Y)) + (a_137.Z * b_133.Z)))) >= 0) ? NPlane_$ctor_Z2DDF2344((p_4 = pl_36.Origin, (v_37 = ((a_138 = pl_36.Normal, (f_8 = 5, Vec_$ctor_Z7AD9E565_2(a_138.X * f_8, a_138.Y * f_8, a_138.Z * f_8)))), Pnt_$ctor_Z7AD9E565_1(p_4.X + v_37.X, p_4.Y + v_37.Y, p_4.Z + v_37.Z))), pl_36.Normal) : NPlane_$ctor_Z2DDF2344((p_5 = pl_36.Origin, (v_38 = ((a_139 = pl_36.Normal, (f_9 = 5, Vec_$ctor_Z7AD9E565_2(a_139.X * f_9, a_139.Y * f_9, a_139.Z * f_9)))), Pnt_$ctor_Z7AD9E565_1(p_5.X - v_38.X, p_5.Y - v_38.Y, p_5.Z - v_38.Z))), pl_36.Normal))).Origin, (b_135 = Pnt_$ctor_Z7AD9E565(0, 0, 5), (x_51 = (a_141.X - b_135.X), (y_41 = (a_141.Y - b_135.Y), (z_39 = (a_141.Z - b_135.Z), Math.sqrt(((x_51 * x_51) + (y_41 * y_41)) + (z_39 * z_39)))))))) < 1E-09)("Should offset towards dirPt");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("offsetInDir away from point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        let a_147, pl_38, a_143, b_137, a_142, b_136, p_6, v_39, a_144, f_10, p_7, v_40, a_145, f_11, b_139, x_52, y_42, z_40;
        Expect_isTrue(((a_147 = ((pl_38 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), (((a_143 = pl_38.Normal, (b_137 = ((a_142 = Pnt_$ctor_Z7AD9E565(0, 0, -10), (b_136 = pl_38.Origin, Vec_$ctor_Z7AD9E565_2(a_142.X - b_136.X, a_142.Y - b_136.Y, a_142.Z - b_136.Z)))), ((a_143.X * b_137.X) + (a_143.Y * b_137.Y)) + (a_143.Z * b_137.Z)))) >= 0) ? NPlane_$ctor_Z2DDF2344((p_6 = pl_38.Origin, (v_39 = ((a_144 = pl_38.Normal, (f_10 = 5, Vec_$ctor_Z7AD9E565_2(a_144.X * f_10, a_144.Y * f_10, a_144.Z * f_10)))), Pnt_$ctor_Z7AD9E565_1(p_6.X + v_39.X, p_6.Y + v_39.Y, p_6.Z + v_39.Z))), pl_38.Normal) : NPlane_$ctor_Z2DDF2344((p_7 = pl_38.Origin, (v_40 = ((a_145 = pl_38.Normal, (f_11 = 5, Vec_$ctor_Z7AD9E565_2(a_145.X * f_11, a_145.Y * f_11, a_145.Z * f_11)))), Pnt_$ctor_Z7AD9E565_1(p_7.X - v_40.X, p_7.Y - v_40.Y, p_7.Z - v_40.Z))), pl_38.Normal))).Origin, (b_139 = Pnt_$ctor_Z7AD9E565(0, 0, -5), (x_52 = (a_147.X - b_139.X), (y_42 = (a_147.Y - b_139.Y), (z_40 = (a_147.Z - b_139.Z), Math.sqrt(((x_52 * x_52) + (y_42 * y_42)) + (z_40 * z_40)))))))) < 1E-09)("Should offset away from dirPt");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale from world origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        let a_148, a_150, b_141, x_53, y_43, z_41, v_44, a_151, v_41, b_142, v_42;
        const plane_24 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565(0, 0, 1));
        let scaled;
        const pl_40 = plane_24;
        scaled = NPlane_$ctor_Z2DDF2344((a_148 = pl_40.Origin, Pnt_$ctor_Z7AD9E565_1(a_148.X * 2, a_148.Y * 2, a_148.Z * 2)), pl_40.Normal);
        Expect_isTrue(((a_150 = scaled.Origin, (b_141 = Pnt_$ctor_Z7AD9E565(2, 4, 6), (x_53 = (a_150.X - b_141.X), (y_43 = (a_150.Y - b_141.Y), (z_41 = (a_150.Z - b_141.Z), Math.sqrt(((x_53 * x_53) + (y_43 * y_43)) + (z_41 * z_41)))))))) < 1E-09)("Origin should be scaled");
        Expect_isTrue(((v_44 = ((a_151 = ((v_41 = scaled.Normal, Vec_$ctor_Z7AD9E565_1(v_41.X, v_41.Y, v_41.Z))), (b_142 = ((v_42 = plane_24.Normal, Vec_$ctor_Z7AD9E565_1(v_42.X, v_42.Y, v_42.Z))), Vec_$ctor_Z7AD9E565_2(a_151.X - b_142.X, a_151.Y - b_142.Y, a_151.Z - b_142.Z)))), Math.sqrt(((v_44.X * v_44.X) + (v_44.Y * v_44.Y)) + (v_44.Z * v_44.Z)))) < 1E-09)("Normal should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move by vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        let p_8, v_45, a_154, b_145, x_54, y_44, z_42, v_49, a_155, v_46, b_146, v_47;
        const plane_25 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        let moved;
        const pl_42 = plane_25;
        moved = NPlane_$ctor_Z2DDF2344((p_8 = pl_42.Origin, (v_45 = Vec_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565_1(p_8.X + v_45.X, p_8.Y + v_45.Y, p_8.Z + v_45.Z))), pl_42.Normal);
        Expect_isTrue(((a_154 = moved.Origin, (b_145 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_54 = (a_154.X - b_145.X), (y_44 = (a_154.Y - b_145.Y), (z_42 = (a_154.Z - b_145.Z), Math.sqrt(((x_54 * x_54) + (y_44 * y_44)) + (z_42 * z_42)))))))) < 1E-09)("Origin should be moved");
        Expect_isTrue(((v_49 = ((a_155 = ((v_46 = moved.Normal, Vec_$ctor_Z7AD9E565_1(v_46.X, v_46.Y, v_46.Z))), (b_146 = ((v_47 = plane_25.Normal, Vec_$ctor_Z7AD9E565_1(v_47.X, v_47.Y, v_47.Z))), Vec_$ctor_Z7AD9E565_2(a_155.X - b_146.X, a_155.Y - b_146.Y, a_155.Z - b_146.Z)))), Math.sqrt(((v_49.X * v_49.X) + (v_49.Y * v_49.Y)) + (v_49.Z * v_49.Z)))) < 1E-09)("Normal should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translate is alias for move", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        let a_158, pl_44, p_9, v_50, b_149, x_55, y_45, z_43;
        Expect_isTrue(((a_158 = ((pl_44 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), NPlane_$ctor_Z2DDF2344((p_9 = pl_44.Origin, (v_50 = Vec_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565_1(p_9.X + v_50.X, p_9.Y + v_50.Y, p_9.Z + v_50.Z))), pl_44.Normal))).Origin, (b_149 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_55 = (a_158.X - b_149.X), (y_45 = (a_158.Y - b_149.Y), (z_43 = (a_158.Z - b_149.Z), Math.sqrt(((x_55 * x_55) + (y_45 * y_45)) + (z_43 * z_43)))))))) < 1E-09)("Origin should be moved");
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})()])), Test_testList("NPlane - Equality", ofArray([(() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with exact match", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        Expect_isTrue(NPlane_equals(0, NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1)), NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1))))("Exact planes should be equal");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        const a_161 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0, 0, 0), Vec_$ctor_Z7AD9E565(0, 0, 1));
        const b_152 = NPlane_create_5A66521A(Pnt_$ctor_Z7AD9E565(0.001, 0.001, 0.001), Vec_$ctor_Z7AD9E565(0, 0, 1));
        Expect_isTrue(NPlane_equals(0.01, a_161, b_152))("Planes should be equal within tolerance");
        Expect_isFalse(NPlane_equals(0.0001, a_161, b_152))("Planes should not be equal with small tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})()])), Test_testList("PPlane - Construction", ofArray([(() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createOriginXaxisYaxis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let a_165, b_156, x_56, y_46, z_44, v_53, a_166, v_51, b_157, v_56, a_168, v_54, b_159;
        const origin_4 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const plane_27 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(origin_4, Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565(0, 1, 0));
        Expect_isTrue(((a_165 = plane_27.Origin, (b_156 = origin_4, (x_56 = (a_165.X - b_156.X), (y_46 = (a_165.Y - b_156.Y), (z_44 = (a_165.Z - b_156.Z), Math.sqrt(((x_56 * x_56) + (y_46 * y_46)) + (z_44 * z_44)))))))) < 1E-09)("Origin should match");
        Expect_isTrue(((v_53 = ((a_166 = ((v_51 = plane_27.Xaxis, Vec_$ctor_Z7AD9E565_1(v_51.X, v_51.Y, v_51.Z))), (b_157 = Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565_2(a_166.X - b_157.X, a_166.Y - b_157.Y, a_166.Z - b_157.Z)))), Math.sqrt(((v_53.X * v_53.X) + (v_53.Y * v_53.Y)) + (v_53.Z * v_53.Z)))) < 1E-09)("Xaxis should be unitized");
        Expect_isTrue(((v_56 = ((a_168 = ((v_54 = plane_27.Yaxis, Vec_$ctor_Z7AD9E565_1(v_54.X, v_54.Y, v_54.Z))), (b_159 = Vec_$ctor_Z7AD9E565(0, 1, 0), Vec_$ctor_Z7AD9E565_2(a_168.X - b_159.X, a_168.Y - b_159.Y, a_168.Z - b_159.Z)))), Math.sqrt(((v_56.X * v_56.X) + (v_56.Y * v_56.Y)) + (v_56.Z * v_56.Z)))) < 1E-09)("Yaxis should be unitized");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createOriginXaxisYaxis rejects zero X-axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        const origin_5 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        Expect_throws(() => {
            Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(origin_5, Vec_$ctor_Z7AD9E565_2(0, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0));
        }, "Should throw for zero X-axis");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createOriginXaxisYaxis rejects zero Y-axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        const origin_6 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        Expect_throws(() => {
            Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(origin_6, Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_2(0, 0, 0));
        }, "Should throw for zero Y-axis");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("createOriginXaxisYaxis rejects parallel axes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        const origin_7 = Pnt_$ctor_Z7AD9E565(0, 0, 0);
        const xAxis_1 = Vec_$ctor_Z7AD9E565(1, 0, 0);
        const yAxis_1 = Vec_$ctor_Z7AD9E565(2, 0, 0);
        Expect_throws(() => {
            Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(origin_7, xAxis_1, yAxis_1);
        }, "Should throw for parallel axes");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("WorldXY plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let a_171, b_162, x_60, y_50, z_48, v_59, a_172, v_57, b_163, v_62, a_174, v_60, b_165, v_65, a_176, v_63, b_167;
        const plane_28 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        Expect_isTrue(((a_171 = plane_28.Origin, (b_162 = Pnt_$ctor_Z7AD9E565_1(0, 0, 0), (x_60 = (a_171.X - b_162.X), (y_50 = (a_171.Y - b_162.Y), (z_48 = (a_171.Z - b_162.Z), Math.sqrt(((x_60 * x_60) + (y_50 * y_50)) + (z_48 * z_48)))))))) < 1E-09)("Origin should be at world origin");
        Expect_isTrue(((v_59 = ((a_172 = ((v_57 = plane_28.Xaxis, Vec_$ctor_Z7AD9E565_1(v_57.X, v_57.Y, v_57.Z))), (b_163 = Vec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565_2(a_172.X - b_163.X, a_172.Y - b_163.Y, a_172.Z - b_163.Z)))), Math.sqrt(((v_59.X * v_59.X) + (v_59.Y * v_59.Y)) + (v_59.Z * v_59.Z)))) < 1E-09)("Xaxis should be world X");
        Expect_isTrue(((v_62 = ((a_174 = ((v_60 = plane_28.Yaxis, Vec_$ctor_Z7AD9E565_1(v_60.X, v_60.Y, v_60.Z))), (b_165 = Vec_$ctor_Z7AD9E565(0, 1, 0), Vec_$ctor_Z7AD9E565_2(a_174.X - b_165.X, a_174.Y - b_165.Y, a_174.Z - b_165.Z)))), Math.sqrt(((v_62.X * v_62.X) + (v_62.Y * v_62.Y)) + (v_62.Z * v_62.Z)))) < 1E-09)("Yaxis should be world Y");
        Expect_isTrue(((v_65 = ((a_176 = ((v_63 = plane_28.Zaxis, Vec_$ctor_Z7AD9E565_1(v_63.X, v_63.Y, v_63.Z))), (b_167 = Vec_$ctor_Z7AD9E565(0, 0, 1), Vec_$ctor_Z7AD9E565_2(a_176.X - b_167.X, a_176.Y - b_167.Y, a_176.Z - b_167.Z)))), Math.sqrt(((v_65.X * v_65.X) + (v_65.Y * v_65.Y)) + (v_65.Z * v_65.Z)))) < 1E-09)("Zaxis should be world Z");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("WorldTop is same as WorldXY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let a_179, b_170;
        Expect_isTrue((a_179 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), (b_170 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), ((((((((Math.abs(a_179.Origin.X - b_170.Origin.X) <= 0) && (Math.abs(a_179.Origin.Y - b_170.Origin.Y) <= 0)) && (Math.abs(a_179.Origin.Z - b_170.Origin.Z) <= 0)) && (Math.abs(a_179.Xaxis.X - b_170.Xaxis.X) <= 0)) && (Math.abs(a_179.Xaxis.Y - b_170.Xaxis.Y) <= 0)) && (Math.abs(a_179.Xaxis.Z - b_170.Xaxis.Z) <= 0)) && (Math.abs(a_179.Yaxis.X - b_170.Yaxis.X) <= 0)) && (Math.abs(a_179.Yaxis.Y - b_170.Yaxis.Y) <= 0)) && (Math.abs(a_179.Yaxis.Z - b_170.Yaxis.Z) <= 0))))("WorldTop should equal WorldXY");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})()])), Test_testList("PPlane - Distance and Evaluation", ofArray([(() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("DistanceToPt for point above plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        let copyOfStruct_2, arg_33, arg_1_30;
        let actual_2;
        const pl_45 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        const a_181 = pl_45.Zaxis;
        let b_172;
        const a_180 = Pnt_$ctor_Z7AD9E565(5, 5, 10);
        const b_171 = pl_45.Origin;
        b_172 = Vec_$ctor_Z7AD9E565_2(a_180.X - b_171.X, a_180.Y - b_171.Y, a_180.Z - b_171.Z);
        actual_2 = (((a_181.X * b_172.X) + (a_181.Y * b_172.Y)) + (a_181.Z * b_172.Z));
        if ((actual_2 === 10) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_2, 10, "Distance should be 10");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_2, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_70) => (structuralHash(x_70) | 0),
            }) ? ((arg_33 = (10).toString(), (arg_1_30 = actual_2.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_33)(arg_1_30)("Distance should be 10")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(10)(actual_2)("Distance should be 10"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt origin (0, 0, 0)", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let a_186, p_10, p_13, p_12, p_11, v_66, a_182, v_67, a_183, v_68, a_184, b_174, x_71, y_61, z_58;
        const plane_30 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0));
        Expect_isTrue(((a_186 = ((p_10 = plane_30, (p_13 = ((p_12 = ((p_11 = p_10.Origin, (v_66 = ((a_182 = p_10.Xaxis, Vec_$ctor_Z7AD9E565_2(a_182.X * 0, a_182.Y * 0, a_182.Z * 0))), Pnt_$ctor_Z7AD9E565_1(p_11.X + v_66.X, p_11.Y + v_66.Y, p_11.Z + v_66.Z)))), (v_67 = ((a_183 = p_10.Yaxis, Vec_$ctor_Z7AD9E565_2(a_183.X * 0, a_183.Y * 0, a_183.Z * 0))), Pnt_$ctor_Z7AD9E565_1(p_12.X + v_67.X, p_12.Y + v_67.Y, p_12.Z + v_67.Z)))), (v_68 = ((a_184 = p_10.Zaxis, Vec_$ctor_Z7AD9E565_2(a_184.X * 0, a_184.Y * 0, a_184.Z * 0))), Pnt_$ctor_Z7AD9E565_1(p_13.X + v_68.X, p_13.Y + v_68.Y, p_13.Z + v_68.Z))))), (b_174 = plane_30.Origin, (x_71 = (a_186.X - b_174.X), (y_61 = (a_186.Y - b_174.Y), (z_58 = (a_186.Z - b_174.Z), Math.sqrt(((x_71 * x_71) + (y_61 * y_61)) + (z_58 * z_58)))))))) < 1E-09)("Should return Origin at (0, 0, 0)");
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAt with positive parameters", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        let a_191, p_14, p_17, p_16, p_15, v_69, a_187, v_70, a_188, v_71, a_189, b_176, x_75, y_65, z_62;
        Expect_isTrue(((a_191 = ((p_14 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), (p_17 = ((p_16 = ((p_15 = p_14.Origin, (v_69 = ((a_187 = p_14.Xaxis, Vec_$ctor_Z7AD9E565_2(a_187.X * 5, a_187.Y * 5, a_187.Z * 5))), Pnt_$ctor_Z7AD9E565_1(p_15.X + v_69.X, p_15.Y + v_69.Y, p_15.Z + v_69.Z)))), (v_70 = ((a_188 = p_14.Yaxis, Vec_$ctor_Z7AD9E565_2(a_188.X * 3, a_188.Y * 3, a_188.Z * 3))), Pnt_$ctor_Z7AD9E565_1(p_16.X + v_70.X, p_16.Y + v_70.Y, p_16.Z + v_70.Z)))), (v_71 = ((a_189 = p_14.Zaxis, Vec_$ctor_Z7AD9E565_2(a_189.X * 2, a_189.Y * 2, a_189.Z * 2))), Pnt_$ctor_Z7AD9E565_1(p_17.X + v_71.X, p_17.Y + v_71.Y, p_17.Z + v_71.Z))))), (b_176 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_75 = (a_191.X - b_176.X), (y_65 = (a_191.Y - b_176.Y), (z_62 = (a_191.Z - b_176.Z), Math.sqrt(((x_75 * x_75) + (y_65 * y_65)) + (z_62 * z_62)))))))) < 1E-09)("Should evaluate correctly");
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("EvaluateAtXY", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        let a_195, p_18, p_20, p_19, v_72, a_192, v_73, a_193, b_178, x_79, y_69, z_66;
        Expect_isTrue(((a_195 = ((p_18 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), (p_20 = ((p_19 = p_18.Origin, (v_72 = ((a_192 = p_18.Xaxis, Vec_$ctor_Z7AD9E565_2(a_192.X * 5, a_192.Y * 5, a_192.Z * 5))), Pnt_$ctor_Z7AD9E565_1(p_19.X + v_72.X, p_19.Y + v_72.Y, p_19.Z + v_72.Z)))), (v_73 = ((a_193 = p_18.Yaxis, Vec_$ctor_Z7AD9E565_2(a_193.X * 3, a_193.Y * 3, a_193.Z * 3))), Pnt_$ctor_Z7AD9E565_1(p_20.X + v_73.X, p_20.Y + v_73.Y, p_20.Z + v_73.Z))))), (b_178 = Pnt_$ctor_Z7AD9E565(5, 3, 0), (x_79 = (a_195.X - b_178.X), (y_69 = (a_195.Y - b_178.Y), (z_66 = (a_195.Z - b_178.Z), Math.sqrt(((x_79 * x_79) + (y_69 * y_69)) + (z_66 * z_66)))))))) < 1E-09)("Should evaluate in XY only");
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("PointParameters", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        let a_197, b_180, a_198, b_181, a_199, b_182, copyOfStruct_3, arg_35, arg_1_32, copyOfStruct_4, arg_36, arg_1_33, copyOfStruct_5, arg_37, arg_1_34;
        let patternInput;
        const pl_46 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        let v_74;
        const a_196 = Pnt_$ctor_Z7AD9E565(5, 3, 2);
        const b_179 = pl_46.Origin;
        v_74 = Vec_$ctor_Z7AD9E565_2(a_196.X - b_179.X, a_196.Y - b_179.Y, a_196.Z - b_179.Z);
        patternInput = [(a_197 = pl_46.Xaxis, (b_180 = v_74, ((a_197.X * b_180.X) + (a_197.Y * b_180.Y)) + (a_197.Z * b_180.Z))), (a_198 = pl_46.Yaxis, (b_181 = v_74, ((a_198.X * b_181.X) + (a_198.Y * b_181.Y)) + (a_198.Z * b_181.Z))), (a_199 = pl_46.Zaxis, (b_182 = v_74, ((a_199.X * b_182.X) + (a_199.Y * b_182.Y)) + (a_199.Z * b_182.Z)))];
        const actual_3 = patternInput[0];
        if ((actual_3 === 5) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 5, "X parameter should be 5");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_3, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_84) => (structuralHash(x_84) | 0),
            }) ? ((arg_35 = (5).toString(), (arg_1_32 = actual_3.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_35)(arg_1_32)("X parameter should be 5")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(5)(actual_3)("X parameter should be 5"));
        }
        const actual_4 = patternInput[1];
        if ((actual_4 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_4, 3, "Y parameter should be 3");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_4, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_85) => (structuralHash(x_85) | 0),
            }) ? ((arg_36 = (3).toString(), (arg_1_33 = actual_4.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_36)(arg_1_33)("Y parameter should be 3")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_4)("Y parameter should be 3"));
        }
        const actual_5 = patternInput[2];
        if ((actual_5 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 2, "Z parameter should be 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_5, float64_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_86) => (structuralHash(x_86) | 0),
            }) ? ((arg_37 = (2).toString(), (arg_1_34 = actual_5.toString(), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_37)(arg_1_34)("Z parameter should be 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_5)("Z parameter should be 2"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_50);
    }));
})()])), Test_testList("PPlane - Angles", ofArray([(() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToPlane for parallel planes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        let a_203, b_186, dot_6, a_204, b_187, dotAbs_6, x_90, y_80, z_74, x_91, y_81, z_75;
        const angle_6 = 57.29577951308232 * ((a_203 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)).Zaxis, (b_186 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565(0, 0, 10), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0)).Zaxis, (dot_6 = ((a_204 = a_203, (b_187 = b_186, ((a_204.X * b_187.X) + (a_204.Y * b_187.Y)) + (a_204.Z * b_187.Z)))), (dotAbs_6 = Math.abs(dot_6), (dotAbs_6 < 0.98) ? Math.acos(dotAbs_6) : ((dot_6 < 0) ? (2 * Math.asin(((x_90 = (b_186.X - -a_203.X), (y_80 = (b_186.Y - -a_203.Y), (z_74 = (b_186.Z - -a_203.Z), Math.sqrt(((x_90 * x_90) + (y_80 * y_80)) + (z_74 * z_74)))))) * 0.5)) : (2 * Math.asin(((x_91 = (b_186.X - a_203.X), (y_81 = (b_186.Y - a_203.Y), (z_75 = (b_186.Z - a_203.Z), Math.sqrt(((x_91 * x_91) + (y_81 * y_81)) + (z_75 * z_75)))))) * 0.5))))))));
        Expect_isTrue(Math.abs(angle_6 - 0) < 1E-09)("Parallel planes should have 0 angle");
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToPlane for perpendicular planes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        let a_209, b_192, v_75, dot_7, a_210, b_193, dotAbs_7, x_99, y_89, z_83, x_100, y_90, z_84;
        const angle_7 = 57.29577951308232 * ((a_209 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)).Zaxis, (b_192 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1), (v_75 = UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(-v_75.X, -v_75.Y, -v_75.Z))).Zaxis, (dot_7 = ((a_210 = a_209, (b_193 = b_192, ((a_210.X * b_193.X) + (a_210.Y * b_193.Y)) + (a_210.Z * b_193.Z)))), (dotAbs_7 = Math.abs(dot_7), (dotAbs_7 < 0.98) ? Math.acos(dotAbs_7) : ((dot_7 < 0) ? (2 * Math.asin(((x_99 = (b_192.X - -a_209.X), (y_89 = (b_192.Y - -a_209.Y), (z_83 = (b_192.Z - -a_209.Z), Math.sqrt(((x_99 * x_99) + (y_89 * y_89)) + (z_83 * z_83)))))) * 0.5)) : (2 * Math.asin(((x_100 = (b_192.X - a_209.X), (y_90 = (b_192.Y - a_209.Y), (z_84 = (b_192.Z - a_209.Z), Math.sqrt(((x_100 * x_100) + (y_90 * y_90)) + (z_84 * z_84)))))) * 0.5))))))));
        Expect_isTrue(Math.abs(angle_7 - 90) < 1E-09)("Perpendicular planes should have 90 degree angle");
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToVec for parallel vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        let a_214, v_77, x_104, y_94, z_88, l_6, f_21, b_197, dot_8, a_215, b_198, dotAbs_8, x_107, y_96, z_90, x_108, y_97, z_91;
        const plane_34 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        const angle_8 = 90 - (57.29577951308232 * ((a_214 = ((v_77 = Vec_$ctor_Z7AD9E565(1, 0, 0), (x_104 = v_77.X, (y_94 = v_77.Y, (z_88 = v_77.Z, (l_6 = Math.sqrt(((x_104 * x_104) + (y_94 * y_94)) + (z_88 * z_88)), (!(l_6 > 1E-12) ? failUnit3("Vec.Unitized", x_104, y_94, z_88) : undefined, (f_21 = (1 / l_6), UnitVec_$ctor_Z7AD9E565(f_21 * x_104, f_21 * y_94, f_21 * z_88))))))))), (b_197 = plane_34.Zaxis, (dot_8 = ((a_215 = a_214, (b_198 = b_197, ((a_215.X * b_198.X) + (a_215.Y * b_198.Y)) + (a_215.Z * b_198.Z)))), (dotAbs_8 = Math.abs(dot_8), (dotAbs_8 < 0.98) ? Math.acos(dotAbs_8) : ((dot_8 < 0) ? (2 * Math.asin(((x_107 = (b_197.X - -a_214.X), (y_96 = (b_197.Y - -a_214.Y), (z_90 = (b_197.Z - -a_214.Z), Math.sqrt(((x_107 * x_107) + (y_96 * y_96)) + (z_90 * z_90)))))) * 0.5)) : (2 * Math.asin(((x_108 = (b_197.X - a_214.X), (y_97 = (b_197.Y - a_214.Y), (z_91 = (b_197.Z - a_214.Z), Math.sqrt(((x_108 * x_108) + (y_97 * y_97)) + (z_91 * z_91)))))) * 0.5)))))))));
        Expect_isTrue(Math.abs(angle_8 - 0) < 1E-09)("Parallel vector should have 0 angle");
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Angle90ToLine", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        let a_219, b_202, dot_9, a_220, b_203, dotAbs_9, x_115, y_103, z_97, x_116, y_104, z_98;
        const plane_35 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        let angle_9;
        const ln_28 = Line3D_$ctor_5A6659A0(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Pnt_$ctor_Z7AD9E565(10, 0, 0));
        let x_112;
        const ln_29 = ln_28;
        x_112 = (ln_29.ToX - ln_29.FromX);
        let y_101;
        const ln_30 = ln_28;
        y_101 = (ln_30.ToY - ln_30.FromY);
        let z_95;
        const ln_31 = ln_28;
        z_95 = (ln_31.ToZ - ln_31.FromZ);
        const l_7 = Math.sqrt(((x_112 * x_112) + (y_101 * y_101)) + (z_95 * z_95));
        if (!(l_7 > 1E-12)) {
            failTooSmall("PPlane.Angle90ToLine", ln_28);
        }
        angle_9 = (90 - (57.29577951308232 * ((a_219 = UnitVec_$ctor_Z7AD9E565(x_112 / l_7, y_101 / l_7, z_95 / l_7), (b_202 = plane_35.Zaxis, (dot_9 = ((a_220 = a_219, (b_203 = b_202, ((a_220.X * b_203.X) + (a_220.Y * b_203.Y)) + (a_220.Z * b_203.Z)))), (dotAbs_9 = Math.abs(dot_9), (dotAbs_9 < 0.98) ? Math.acos(dotAbs_9) : ((dot_9 < 0) ? (2 * Math.asin(((x_115 = (b_202.X - -a_219.X), (y_103 = (b_202.Y - -a_219.Y), (z_97 = (b_202.Z - -a_219.Z), Math.sqrt(((x_115 * x_115) + (y_103 * y_103)) + (z_97 * z_97)))))) * 0.5)) : (2 * Math.asin(((x_116 = (b_202.X - a_219.X), (y_104 = (b_202.Y - a_219.Y), (z_98 = (b_202.Z - a_219.Z), Math.sqrt(((x_116 * x_116) + (y_104 * y_104)) + (z_98 * z_98)))))) * 0.5))))))))));
        Expect_isTrue(Math.abs(angle_9 - 0) < 1E-09)("Parallel line should have 0 angle");
        Test_TestCaseBuilder__Zero(builder$0040_54);
    }));
})()])), Test_testList("PPlane - Coincidence", ofArray([(() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo for same plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        let pl_51, other_10, a_223, b_206, pl_52, a_225, b_208, a_224, b_207;
        Expect_isTrue((pl_51 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), (other_10 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), (Math.abs((a_223 = other_10.Zaxis, (b_206 = pl_51.Zaxis, ((a_223.X * b_206.X) + (a_223.Y * b_206.Y)) + (a_223.Z * b_206.Z)))) > 0.9999904807207345) && (((pl_52 = pl_51, (a_225 = pl_52.Zaxis, (b_208 = ((a_224 = other_10.Origin, (b_207 = pl_52.Origin, Vec_$ctor_Z7AD9E565_2(a_224.X - b_207.X, a_224.Y - b_207.Y, a_224.Z - b_207.Z)))), ((a_225.X * b_208.X) + (a_225.Y * b_208.Y)) + (a_225.Z * b_208.Z))))) < 1E-06))))("Same plane should be coincident");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsCoincidentTo for coincident but rotated planes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        let pl_53, other_12, a_227, b_210, pl_54, a_229, b_212, a_228, b_211;
        Expect_isTrue((pl_53 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0)), (other_12 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565(10, 10, 0), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0)), (Math.abs((a_227 = other_12.Zaxis, (b_210 = pl_53.Zaxis, ((a_227.X * b_210.X) + (a_227.Y * b_210.Y)) + (a_227.Z * b_210.Z)))) > 0.9999904807207345) && (((pl_54 = pl_53, (a_229 = pl_54.Zaxis, (b_212 = ((a_228 = other_12.Origin, (b_211 = pl_54.Origin, Vec_$ctor_Z7AD9E565_2(a_228.X - b_211.X, a_228.Y - b_211.Y, a_228.Z - b_211.Z)))), ((a_229.X * b_212.X) + (a_229.Y * b_212.Y)) + (a_229.Z * b_212.Z))))) < 1E-06))))("Planes at same height should be coincident");
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areCoincident static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        let pl_55, other_14, a_233, b_216, pl_56, a_235, b_218, a_234, b_217;
        Expect_isTrue((pl_55 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), (other_14 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565(10, 10, 0), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0)), (Math.abs((a_233 = other_14.Zaxis, (b_216 = pl_55.Zaxis, ((a_233.X * b_216.X) + (a_233.Y * b_216.Y)) + (a_233.Z * b_216.Z)))) > 0.9999904807207345) && (((pl_56 = pl_55, (a_235 = pl_56.Zaxis, (b_218 = ((a_234 = other_14.Origin, (b_217 = pl_56.Origin, Vec_$ctor_Z7AD9E565_2(a_234.X - b_217.X, a_234.Y - b_217.Y, a_234.Z - b_217.Z)))), ((a_235.X * b_218.X) + (a_235.Y * b_218.Y)) + (a_235.Z * b_218.Z))))) < 1E-06))))("Static method should work");
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})()])), Test_testList("PPlane - Transformation", ofArray([(() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale from world origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let a_237, b_220, x_126, y_114, z_108, v_81, a_238, v_78, b_221, v_79;
        const plane_36 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565(1, 2, 3), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0));
        let scaled_1;
        const pl_58 = plane_36;
        const o = pl_58.Origin;
        scaled_1 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_2(o.X * 2, o.Y * 2, o.Z * 2), pl_58.Xaxis, pl_58.Yaxis, pl_58.Zaxis);
        Expect_isTrue(((a_237 = scaled_1.Origin, (b_220 = Pnt_$ctor_Z7AD9E565(2, 4, 6), (x_126 = (a_237.X - b_220.X), (y_114 = (a_237.Y - b_220.Y), (z_108 = (a_237.Z - b_220.Z), Math.sqrt(((x_126 * x_126) + (y_114 * y_114)) + (z_108 * z_108)))))))) < 1E-09)("Origin should be scaled");
        Expect_isTrue(((v_81 = ((a_238 = ((v_78 = scaled_1.Xaxis, Vec_$ctor_Z7AD9E565_1(v_78.X, v_78.Y, v_78.Z))), (b_221 = ((v_79 = plane_36.Xaxis, Vec_$ctor_Z7AD9E565_1(v_79.X, v_79.Y, v_79.Z))), Vec_$ctor_Z7AD9E565_2(a_238.X - b_221.X, a_238.Y - b_221.Y, a_238.Z - b_221.Z)))), Math.sqrt(((v_81.X * v_81.X) + (v_81.Y * v_81.Y)) + (v_81.Z * v_81.Z)))) < 1E-09)("Axes should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move by vector", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let p_21, v_82, a_241, b_224, x_130, y_118, z_112, v_86, a_242, v_83, b_225, v_84;
        let moved_2;
        const pl_60 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        moved_2 = PPlane_$ctor_3CB4665C((p_21 = pl_60.Origin, (v_82 = Vec_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565_1(p_21.X + v_82.X, p_21.Y + v_82.Y, p_21.Z + v_82.Z))), pl_60.Xaxis, pl_60.Yaxis, pl_60.Zaxis);
        Expect_isTrue(((a_241 = moved_2.Origin, (b_224 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_130 = (a_241.X - b_224.X), (y_118 = (a_241.Y - b_224.Y), (z_112 = (a_241.Z - b_224.Z), Math.sqrt(((x_130 * x_130) + (y_118 * y_118)) + (z_112 * z_112)))))))) < 1E-09)("Origin should be moved");
        Expect_isTrue(((v_86 = ((a_242 = ((v_83 = moved_2.Xaxis, Vec_$ctor_Z7AD9E565_1(v_83.X, v_83.Y, v_83.Z))), (b_225 = ((v_84 = UnitVec_$ctor_Z7AD9E565(1, 0, 0), Vec_$ctor_Z7AD9E565_1(v_84.X, v_84.Y, v_84.Z))), Vec_$ctor_Z7AD9E565_2(a_242.X - b_225.X, a_242.Y - b_225.Y, a_242.Z - b_225.Z)))), Math.sqrt(((v_86.X * v_86.X) + (v_86.Y * v_86.Y)) + (v_86.Z * v_86.Z)))) < 1E-09)("Axes should be unchanged");
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})(), (() => {
    const builder$0040_60 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translate is alias for move", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_60, Test_TestCaseBuilder__Delay_1505(builder$0040_60, () => {
        let a_245, pl_62, p_22, v_87, b_228, x_135, y_123, z_117;
        Expect_isTrue(((a_245 = ((pl_62 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), PPlane_$ctor_3CB4665C((p_22 = pl_62.Origin, (v_87 = Vec_$ctor_Z7AD9E565(5, 5, 5), Pnt_$ctor_Z7AD9E565_1(p_22.X + v_87.X, p_22.Y + v_87.Y, p_22.Z + v_87.Z))), pl_62.Xaxis, pl_62.Yaxis, pl_62.Zaxis))).Origin, (b_228 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_135 = (a_245.X - b_228.X), (y_123 = (a_245.Y - b_228.Y), (z_117 = (a_245.Z - b_228.Z), Math.sqrt(((x_135 * x_135) + (y_123 * y_123)) + (z_117 * z_117)))))))) < 1E-09)("Origin should be moved");
        Test_TestCaseBuilder__Zero(builder$0040_60);
    }));
})()])), Test_testList("PPlane - Equality", ofArray([(() => {
    const builder$0040_61 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with exact match", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_61, Test_TestCaseBuilder__Delay_1505(builder$0040_61, () => {
        let a_248, b_231;
        Expect_isTrue((a_248 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), (b_231 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), ((((((((Math.abs(a_248.Origin.X - b_231.Origin.X) <= 0) && (Math.abs(a_248.Origin.Y - b_231.Origin.Y) <= 0)) && (Math.abs(a_248.Origin.Z - b_231.Origin.Z) <= 0)) && (Math.abs(a_248.Xaxis.X - b_231.Xaxis.X) <= 0)) && (Math.abs(a_248.Xaxis.Y - b_231.Xaxis.Y) <= 0)) && (Math.abs(a_248.Xaxis.Z - b_231.Xaxis.Z) <= 0)) && (Math.abs(a_248.Yaxis.X - b_231.Yaxis.X) <= 0)) && (Math.abs(a_248.Yaxis.Y - b_231.Yaxis.Y) <= 0)) && (Math.abs(a_248.Yaxis.Z - b_231.Yaxis.Z) <= 0))))("Exact planes should be equal");
        Test_TestCaseBuilder__Zero(builder$0040_61);
    }));
})(), (() => {
    const builder$0040_62 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("equals with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_62, Test_TestCaseBuilder__Delay_1505(builder$0040_62, () => {
        let a_251, b_234, a_253, b_236;
        const a_249 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1));
        const b_232 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565(0.001, 0.001, 0.001), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0));
        Expect_isTrue((a_251 = a_249, (b_234 = b_232, ((((((((Math.abs(a_251.Origin.X - b_234.Origin.X) <= 0.01) && (Math.abs(a_251.Origin.Y - b_234.Origin.Y) <= 0.01)) && (Math.abs(a_251.Origin.Z - b_234.Origin.Z) <= 0.01)) && (Math.abs(a_251.Xaxis.X - b_234.Xaxis.X) <= 0.01)) && (Math.abs(a_251.Xaxis.Y - b_234.Xaxis.Y) <= 0.01)) && (Math.abs(a_251.Xaxis.Z - b_234.Xaxis.Z) <= 0.01)) && (Math.abs(a_251.Yaxis.X - b_234.Yaxis.X) <= 0.01)) && (Math.abs(a_251.Yaxis.Y - b_234.Yaxis.Y) <= 0.01)) && (Math.abs(a_251.Yaxis.Z - b_234.Yaxis.Z) <= 0.01))))("Planes should be equal within tolerance");
        Expect_isFalse((a_253 = a_249, (b_236 = b_232, ((((((((Math.abs(a_253.Origin.X - b_236.Origin.X) <= 0.0001) && (Math.abs(a_253.Origin.Y - b_236.Origin.Y) <= 0.0001)) && (Math.abs(a_253.Origin.Z - b_236.Origin.Z) <= 0.0001)) && (Math.abs(a_253.Xaxis.X - b_236.Xaxis.X) <= 0.0001)) && (Math.abs(a_253.Xaxis.Y - b_236.Xaxis.Y) <= 0.0001)) && (Math.abs(a_253.Xaxis.Z - b_236.Xaxis.Z) <= 0.0001)) && (Math.abs(a_253.Yaxis.X - b_236.Yaxis.X) <= 0.0001)) && (Math.abs(a_253.Yaxis.Y - b_236.Yaxis.Y) <= 0.0001)) && (Math.abs(a_253.Yaxis.Z - b_236.Yaxis.Z) <= 0.0001))))("Planes should not be equal with small tolerance");
        Test_TestCaseBuilder__Zero(builder$0040_62);
    }));
})(), (() => {
    const builder$0040_63 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("notEquals", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_63, Test_TestCaseBuilder__Delay_1505(builder$0040_63, () => {
        let a_256, b_239;
        Expect_isTrue((a_256 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), (b_239 = Euclid_PPlane__PPlane_createOriginXaxisYaxis_Static_6181AF53(Pnt_$ctor_Z7AD9E565(1, 1, 1), Vec_$ctor_Z7AD9E565_1(1, 0, 0), Vec_$ctor_Z7AD9E565_1(0, 1, 0)), ((((((((Math.abs(a_256.Origin.X - b_239.Origin.X) > 0.5) ? true : (Math.abs(a_256.Origin.Y - b_239.Origin.Y) > 0.5)) ? true : (Math.abs(a_256.Origin.Z - b_239.Origin.Z) > 0.5)) ? true : (Math.abs(a_256.Xaxis.X - b_239.Xaxis.X) > 0.5)) ? true : (Math.abs(a_256.Xaxis.Y - b_239.Xaxis.Y) > 0.5)) ? true : (Math.abs(a_256.Xaxis.Z - b_239.Xaxis.Z) > 0.5)) ? true : (Math.abs(a_256.Yaxis.X - b_239.Yaxis.X) > 0.5)) ? true : (Math.abs(a_256.Yaxis.Y - b_239.Yaxis.Y) > 0.5)) ? true : (Math.abs(a_256.Yaxis.Z - b_239.Yaxis.Z) > 0.5))))("Different planes should not be equal");
        Test_TestCaseBuilder__Zero(builder$0040_63);
    }));
})()])), Test_testList("PPlane - Projection and Closest Point", ofArray([(() => {
    const builder$0040_64 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ClosestPoint on plane", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_64, Test_TestCaseBuilder__Delay_1505(builder$0040_64, () => {
        let a_261, pl_63, pt_33, p_23, v_88, a_259, f_22, pl_64, a_258, b_241, a_257, b_240, b_243, x_151, y_139, z_133;
        Expect_isTrue(((a_261 = ((pl_63 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), (pt_33 = Pnt_$ctor_Z7AD9E565(5, 5, 10), (p_23 = pt_33, (v_88 = ((a_259 = pl_63.Zaxis, (f_22 = ((pl_64 = pl_63, (a_258 = pl_64.Zaxis, (b_241 = ((a_257 = pt_33, (b_240 = pl_64.Origin, Vec_$ctor_Z7AD9E565_2(a_257.X - b_240.X, a_257.Y - b_240.Y, a_257.Z - b_240.Z)))), ((a_258.X * b_241.X) + (a_258.Y * b_241.Y)) + (a_258.Z * b_241.Z))))), Vec_$ctor_Z7AD9E565_2(a_259.X * f_22, a_259.Y * f_22, a_259.Z * f_22)))), Pnt_$ctor_Z7AD9E565_1(p_23.X - v_88.X, p_23.Y - v_88.Y, p_23.Z - v_88.Z)))))), (b_243 = Pnt_$ctor_Z7AD9E565(5, 5, 0), (x_151 = (a_261.X - b_243.X), (y_139 = (a_261.Y - b_243.Y), (z_133 = (a_261.Z - b_243.Z), Math.sqrt(((x_151 * x_151) + (y_139 * y_139)) + (z_133 * z_133)))))))) < 1E-09)("Closest point should be (5, 5, 0)");
        Test_TestCaseBuilder__Zero(builder$0040_64);
    }));
})(), (() => {
    const builder$0040_65 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("PlaneAtClPt", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_65, Test_TestCaseBuilder__Delay_1505(builder$0040_65, () => {
        let a_266, pl_65, pl_66, pt_37, p_24, v_89, a_264, f_23, pl_67, a_263, b_245, a_262, b_244, b_247, x_155, y_143, z_137;
        Expect_isTrue(((a_266 = ((pl_65 = PPlane_$ctor_3CB4665C(Pnt_$ctor_Z7AD9E565_1(0, 0, 0), UnitVec_$ctor_Z7AD9E565(1, 0, 0), UnitVec_$ctor_Z7AD9E565(0, 1, 0), UnitVec_$ctor_Z7AD9E565(0, 0, 1)), PPlane_$ctor_3CB4665C((pl_66 = pl_65, (pt_37 = Pnt_$ctor_Z7AD9E565(5, 5, 10), (p_24 = pt_37, (v_89 = ((a_264 = pl_66.Zaxis, (f_23 = ((pl_67 = pl_66, (a_263 = pl_67.Zaxis, (b_245 = ((a_262 = pt_37, (b_244 = pl_67.Origin, Vec_$ctor_Z7AD9E565_2(a_262.X - b_244.X, a_262.Y - b_244.Y, a_262.Z - b_244.Z)))), ((a_263.X * b_245.X) + (a_263.Y * b_245.Y)) + (a_263.Z * b_245.Z))))), Vec_$ctor_Z7AD9E565_2(a_264.X * f_23, a_264.Y * f_23, a_264.Z * f_23)))), Pnt_$ctor_Z7AD9E565_1(p_24.X - v_89.X, p_24.Y - v_89.Y, p_24.Z - v_89.Z))))), pl_65.Xaxis, pl_65.Yaxis, pl_65.Zaxis))).Origin, (b_247 = Pnt_$ctor_Z7AD9E565(5, 5, 0), (x_155 = (a_266.X - b_247.X), (y_143 = (a_266.Y - b_247.Y), (z_137 = (a_266.Z - b_247.Z), Math.sqrt(((x_155 * x_155) + (y_143 * y_143)) + (z_137 * z_137)))))))) < 1E-09)("New plane origin should be at closest point");
        Test_TestCaseBuilder__Zero(builder$0040_65);
    }));
})()]))]));

