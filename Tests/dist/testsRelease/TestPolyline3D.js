
import { Expect_throws, Expect_stringContains, Expect_isFalse, Expect_isTrue, Expect_floatClose, Test_TestCaseBuilder__Zero, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, AccuracyModule_veryHigh } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Polyline3D_rotate2D, Polyline3D_scale, Polyline3D__Scale_5E38073B, Polyline3D_rotateWithCenterByQuaternion, Polyline3D__RotateWithCenter_4928E16A, Polyline3D__get_Center, Polyline3D_rotateByQuaternion, Polyline3D__Rotate_Z2A007687, Polyline3D_transformRigid, Polyline3D__TransformRigid_Z625426AD, Polyline3D_transform, Polyline3D__Transform_3CAE9522, Polyline3D_moveZ, Polyline3D_moveY, Polyline3D_moveX, Polyline3D_translate, Polyline3D_move, Polyline3D__MoveZ_5E38073B, Polyline3D__MoveY_5E38073B, Polyline3D__MoveX_5E38073B, Polyline3D__Move_Z394EC5F7, Polyline3D__get_LastSegmentIndex, Polyline3D__get_AsFSharpCode, Polyline3D__get_AsString, Polyline3D__get_LastPointIndex, Polyline3D__get_Points, Polyline3D__SetVertex, Polyline3D__CloseInPlace_5E38073B, Polyline3D__Reverse, Polyline3D__ReverseInPlace, Polyline3D__Clone, Polyline3D__Duplicate, Polyline3D__get_BoundingBox, Polyline3D__IsAlmostClosed_5E38073B, Polyline3D__get_IsClosed, Polyline3D__get_Segments, Polyline3D__get_LastSegment, Polyline3D__get_FirstSegment, Polyline3D__GetSegment_Z524259A4, Polyline3D__get_SecondLastPoint, Polyline3D__get_SecondPoint, Polyline3D__get_End, Polyline3D__get_Start, Polyline3D__get_LastPoint, Polyline3D__get_FirstPoint, Polyline3D__get_Length, Polyline3D_$ctor_516DFD0A, Polyline3D_$ctor_Z524259A4, Polyline3D__get_SegmentCount, Polyline3D__get_PointCount, Polyline3D_$ctor } from "./Src/Polyline3D.js";
import { int32ToString, structuralHash, Exception, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { ofArray, contains } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";
import { Pnt_$ctor_Z7AD9E565 } from "./Src/Pnt.js";
import { Pnt_$ctor_Z7AD9E565 as Pnt_$ctor_Z7AD9E565_1 } from "./Src/Pnt.js";
import { item, setItem } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { toString } from "./fable_modules/fable-library-js.5.0.0/Types.js";
import { Vec_$ctor_Z7AD9E565 } from "./Src/Vec.js";
import { Matrix_createTranslation_Z394EC5F7, Matrix_get_identity } from "./Src/Matrix.js";
import { RigidMatrix_createTranslation_Z394EC5F7 } from "./Src/RigidMatrix.js";
import { Quaternion_$ctor_77D16AC0 } from "./Src/Quaternion.js";
import { UnitVec_$ctor_Z7AD9E565 } from "./Src/UnitVec.js";
import { Rotation2D_$ctor_7B00E9A0 } from "./Src/Rotation2D.js";

export const tol = AccuracyModule_veryHigh;

export const tests = Test_testList("Polyline3D", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create empty polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        let copyOfStruct, arg, arg_1, copyOfStruct_1, arg_6, arg_1_1;
        const pl = Polyline3D_$ctor();
        const actual_1 = Polyline3D__get_PointCount(pl) | 0;
        if ((actual_1 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 0, "empty polyline has 0 points");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual_1, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            }) ? ((arg = int32ToString(0), (arg_1 = int32ToString(actual_1), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("empty polyline has 0 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_1)("empty polyline has 0 points"));
        }
        const actual_3 = Polyline3D__get_SegmentCount(pl) | 0;
        if ((actual_3 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 0, "empty polyline has 0 segments");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_3, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            }) ? ((arg_6 = int32ToString(0), (arg_1_1 = int32ToString(actual_3), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("empty polyline has 0 segments")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_3)("empty polyline has 0 segments"));
        }
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create polyline with capacity", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        let copyOfStruct_2, arg_7, arg_1_2;
        const actual_5 = Polyline3D__get_PointCount(Polyline3D_$ctor_Z524259A4(10)) | 0;
        if ((actual_5 === 0) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 0, "polyline with capacity has 0 points");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_5, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            }) ? ((arg_7 = int32ToString(0), (arg_1_2 = int32ToString(actual_5), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("polyline with capacity has 0 points")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(0)(actual_5)("polyline with capacity has 0 points"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("create polyline from points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        let copyOfStruct_3, arg_8, arg_1_3, copyOfStruct_4, arg_9, arg_1_4;
        const pl_2 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        const actual_7 = Polyline3D__get_PointCount(pl_2) | 0;
        if ((actual_7 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 3, "polyline has correct point count");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_7, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            }) ? ((arg_8 = int32ToString(3), (arg_1_3 = int32ToString(actual_7), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("polyline has correct point count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_7)("polyline has correct point count"));
        }
        const actual_9 = Polyline3D__get_SegmentCount(pl_2) | 0;
        if ((actual_9 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_9, 2, "polyline has correct segment count");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_9, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            }) ? ((arg_9 = int32ToString(2), (arg_1_4 = int32ToString(actual_9), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("polyline has correct segment count")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_9)("polyline has correct segment count"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("polyline length calculation", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        Expect_floatClose(tol, Polyline3D__get_Length(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)])), 2, "polyline length is correct");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("polyline length with 3D diagonal", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const pl_4 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 1)]);
        const expectedLength_1 = Math.sqrt(3);
        Expect_floatClose(tol, Polyline3D__get_Length(pl_4), expectedLength_1, "polyline length is sqrt(3)");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("empty polyline has zero length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        Expect_floatClose(tol, Polyline3D__get_Length(Polyline3D_$ctor()), 0, "empty polyline has zero length");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("single point polyline has zero length", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        Expect_floatClose(tol, Polyline3D__get_Length(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0)])), 0, "single point polyline has zero length");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("FirstPoint and LastPoint accessors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        let a_2, b_2, x_5, y_5, z, a_4, b_4, x_6, y_6, z_1;
        const pl_7 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        Expect_isTrue(((a_2 = Polyline3D__get_FirstPoint(pl_7), (b_2 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_5 = (a_2.X - b_2.X), (y_5 = (a_2.Y - b_2.Y), (z = (a_2.Z - b_2.Z), Math.sqrt(((x_5 * x_5) + (y_5 * y_5)) + (z * z)))))))) < 1E-09)("FirstPoint is correct");
        Expect_isTrue(((a_4 = Polyline3D__get_LastPoint(pl_7), (b_4 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_6 = (a_4.X - b_4.X), (y_6 = (a_4.Y - b_4.Y), (z_1 = (a_4.Z - b_4.Z), Math.sqrt(((x_6 * x_6) + (y_6 * y_6)) + (z_1 * z_1)))))))) < 1E-09)("LastPoint is correct");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Start and End accessors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        let a_6, b_6, x_7, y_7, z_2, a_8, b_8, x_8, y_8, z_3;
        const pl_8 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        Expect_isTrue(((a_6 = Polyline3D__get_Start(pl_8), (b_6 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_7 = (a_6.X - b_6.X), (y_7 = (a_6.Y - b_6.Y), (z_2 = (a_6.Z - b_6.Z), Math.sqrt(((x_7 * x_7) + (y_7 * y_7)) + (z_2 * z_2)))))))) < 1E-09)("Start is correct");
        Expect_isTrue(((a_8 = Polyline3D__get_End(pl_8), (b_8 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_8 = (a_8.X - b_8.X), (y_8 = (a_8.Y - b_8.Y), (z_3 = (a_8.Z - b_8.Z), Math.sqrt(((x_8 * x_8) + (y_8 * y_8)) + (z_3 * z_3)))))))) < 1E-09)("End is correct");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SecondPoint and SecondLastPoint accessors", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let a_10, b_10, x_9, y_9, z_4, a_12, b_12, x_10, y_10, z_5;
        const pl_9 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(0, 1, 0)]);
        Expect_isTrue(((a_10 = Polyline3D__get_SecondPoint(pl_9), (b_10 = Pnt_$ctor_Z7AD9E565(1, 0, 0), (x_9 = (a_10.X - b_10.X), (y_9 = (a_10.Y - b_10.Y), (z_4 = (a_10.Z - b_10.Z), Math.sqrt(((x_9 * x_9) + (y_9 * y_9)) + (z_4 * z_4)))))))) < 1E-09)("SecondPoint is correct");
        Expect_isTrue(((a_12 = Polyline3D__get_SecondLastPoint(pl_9), (b_12 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_10 = (a_12.X - b_12.X), (y_10 = (a_12.Y - b_12.Y), (z_5 = (a_12.Z - b_12.Z), Math.sqrt(((x_10 * x_10) + (y_10 * y_10)) + (z_5 * z_5)))))))) < 1E-09)("SecondLastPoint is correct");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("GetSegment returns correct segment", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let a_14, ln, b_14, x_11, y_11, z_6, a_16, ln_1, b_16, x_12, y_12, z_7, a_18, ln_2, b_18, x_13, y_13, z_8, a_20, ln_3, b_20, x_14, y_14, z_9;
        const pl_10 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        const seg0 = Polyline3D__GetSegment_Z524259A4(pl_10, 0);
        const seg1 = Polyline3D__GetSegment_Z524259A4(pl_10, 1);
        Expect_isTrue(((a_14 = ((ln = seg0, Pnt_$ctor_Z7AD9E565_1(ln.FromX, ln.FromY, ln.FromZ))), (b_14 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_11 = (a_14.X - b_14.X), (y_11 = (a_14.Y - b_14.Y), (z_6 = (a_14.Z - b_14.Z), Math.sqrt(((x_11 * x_11) + (y_11 * y_11)) + (z_6 * z_6)))))))) < 1E-09)("segment 0 from is correct");
        Expect_isTrue(((a_16 = ((ln_1 = seg0, Pnt_$ctor_Z7AD9E565_1(ln_1.ToX, ln_1.ToY, ln_1.ToZ))), (b_16 = Pnt_$ctor_Z7AD9E565(1, 0, 0), (x_12 = (a_16.X - b_16.X), (y_12 = (a_16.Y - b_16.Y), (z_7 = (a_16.Z - b_16.Z), Math.sqrt(((x_12 * x_12) + (y_12 * y_12)) + (z_7 * z_7)))))))) < 1E-09)("segment 0 to is correct");
        Expect_isTrue(((a_18 = ((ln_2 = seg1, Pnt_$ctor_Z7AD9E565_1(ln_2.FromX, ln_2.FromY, ln_2.FromZ))), (b_18 = Pnt_$ctor_Z7AD9E565(1, 0, 0), (x_13 = (a_18.X - b_18.X), (y_13 = (a_18.Y - b_18.Y), (z_8 = (a_18.Z - b_18.Z), Math.sqrt(((x_13 * x_13) + (y_13 * y_13)) + (z_8 * z_8)))))))) < 1E-09)("segment 1 from is correct");
        Expect_isTrue(((a_20 = ((ln_3 = seg1, Pnt_$ctor_Z7AD9E565_1(ln_3.ToX, ln_3.ToY, ln_3.ToZ))), (b_20 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_14 = (a_20.X - b_20.X), (y_14 = (a_20.Y - b_20.Y), (z_9 = (a_20.Z - b_20.Z), Math.sqrt(((x_14 * x_14) + (y_14 * y_14)) + (z_9 * z_9)))))))) < 1E-09)("segment 1 to is correct");
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("FirstSegment and LastSegment", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let a_22, ln_4, b_22, x_15, y_15, z_10, a_24, ln_5, b_24, x_16, y_16, z_11, a_26, ln_6, b_26, x_17, y_17, z_12, a_28, ln_7, b_28, x_18, y_18, z_13;
        const pl_11 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        const first = Polyline3D__get_FirstSegment(pl_11);
        const last = Polyline3D__get_LastSegment(pl_11);
        Expect_isTrue(((a_22 = ((ln_4 = first, Pnt_$ctor_Z7AD9E565_1(ln_4.FromX, ln_4.FromY, ln_4.FromZ))), (b_22 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_15 = (a_22.X - b_22.X), (y_15 = (a_22.Y - b_22.Y), (z_10 = (a_22.Z - b_22.Z), Math.sqrt(((x_15 * x_15) + (y_15 * y_15)) + (z_10 * z_10)))))))) < 1E-09)("FirstSegment from is correct");
        Expect_isTrue(((a_24 = ((ln_5 = first, Pnt_$ctor_Z7AD9E565_1(ln_5.ToX, ln_5.ToY, ln_5.ToZ))), (b_24 = Pnt_$ctor_Z7AD9E565(1, 0, 0), (x_16 = (a_24.X - b_24.X), (y_16 = (a_24.Y - b_24.Y), (z_11 = (a_24.Z - b_24.Z), Math.sqrt(((x_16 * x_16) + (y_16 * y_16)) + (z_11 * z_11)))))))) < 1E-09)("FirstSegment to is correct");
        Expect_isTrue(((a_26 = ((ln_6 = last, Pnt_$ctor_Z7AD9E565_1(ln_6.FromX, ln_6.FromY, ln_6.FromZ))), (b_26 = Pnt_$ctor_Z7AD9E565(1, 0, 0), (x_17 = (a_26.X - b_26.X), (y_17 = (a_26.Y - b_26.Y), (z_12 = (a_26.Z - b_26.Z), Math.sqrt(((x_17 * x_17) + (y_17 * y_17)) + (z_12 * z_12)))))))) < 1E-09)("LastSegment from is correct");
        Expect_isTrue(((a_28 = ((ln_7 = last, Pnt_$ctor_Z7AD9E565_1(ln_7.ToX, ln_7.ToY, ln_7.ToZ))), (b_28 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_18 = (a_28.X - b_28.X), (y_18 = (a_28.Y - b_28.Y), (z_13 = (a_28.Z - b_28.Z), Math.sqrt(((x_18 * x_18) + (y_18 * y_18)) + (z_13 * z_13)))))))) < 1E-09)("LastSegment to is correct");
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Segments returns all segments", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        let copyOfStruct_5, arg_10, arg_1_5;
        const segs = Polyline3D__get_Segments(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]));
        const actual_15 = segs.length | 0;
        if ((actual_15 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_15, 2, "segments count is correct");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_15, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_19) => (structuralHash(x_19) | 0),
            }) ? ((arg_10 = int32ToString(2), (arg_1_5 = int32ToString(actual_15), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("segments count is correct")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_15)("segments count is correct"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsClosed returns true for closed polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        Expect_isTrue(Polyline3D__get_IsClosed(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)])))("polyline is closed");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsClosed returns false for open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        Expect_isFalse(Polyline3D__get_IsClosed(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)])))("polyline is not closed");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsAlmostClosed returns true within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        Expect_isTrue(Polyline3D__IsAlmostClosed_5E38073B(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(0.001, 0, 0)]), 0.01))("polyline is almost closed with tolerance 0.01");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("IsAlmostClosed returns false outside tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        Expect_isFalse(Polyline3D__IsAlmostClosed_5E38073B(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(0.1, 0, 0)]), 0.01))("polyline is not almost closed with tolerance 0.01");
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("BoundingBox is correct", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const bbox = Polyline3D__get_BoundingBox(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(2, 3, 4), Pnt_$ctor_Z7AD9E565(1, 1, 1)]));
        Expect_floatClose(tol, bbox.MinX, 0, "bounding box MinX is 0");
        Expect_floatClose(tol, bbox.MinY, 0, "bounding box MinY is 0");
        Expect_floatClose(tol, bbox.MinZ, 0, "bounding box MinZ is 0");
        Expect_floatClose(tol, bbox.MaxX, 2, "bounding box MaxX is 2");
        Expect_floatClose(tol, bbox.MaxY, 3, "bounding box MaxY is 3");
        Expect_floatClose(tol, bbox.MaxZ, 4, "bounding box MaxZ is 4");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})(), (() => {
    const builder$0040_18 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Duplicate creates independent copy", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_18, Test_TestCaseBuilder__Delay_1505(builder$0040_18, () => {
        let a_30, b_30, x_20, y_20, z_14, a_32, b_32, x_21, y_21, z_15;
        const pts_15 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)];
        const pl_18 = Polyline3D_$ctor_516DFD0A(pts_15);
        const dup = Polyline3D__Duplicate(pl_18);
        setItem(pts_15, 0, Pnt_$ctor_Z7AD9E565(99, 99, 99));
        Expect_isTrue(((a_30 = Polyline3D__get_FirstPoint(pl_18), (b_30 = Pnt_$ctor_Z7AD9E565(99, 99, 99), (x_20 = (a_30.X - b_30.X), (y_20 = (a_30.Y - b_30.Y), (z_14 = (a_30.Z - b_30.Z), Math.sqrt(((x_20 * x_20) + (y_20 * y_20)) + (z_14 * z_14)))))))) < 1E-09)("original changed");
        Expect_isTrue(((a_32 = Polyline3D__get_FirstPoint(dup), (b_32 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_21 = (a_32.X - b_32.X), (y_21 = (a_32.Y - b_32.Y), (z_15 = (a_32.Z - b_32.Z), Math.sqrt(((x_21 * x_21) + (y_21 * y_21)) + (z_15 * z_15)))))))) < 1E-09)("duplicate not changed");
        Test_TestCaseBuilder__Zero(builder$0040_18);
    }));
})(), (() => {
    const builder$0040_19 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Clone creates independent copy", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_19, Test_TestCaseBuilder__Delay_1505(builder$0040_19, () => {
        let a_34, b_34, x_22, y_22, z_16, a_36, b_36, x_23, y_23, z_17;
        const pts_16 = [Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)];
        const pl_19 = Polyline3D_$ctor_516DFD0A(pts_16);
        const clone = Polyline3D__Clone(pl_19);
        setItem(pts_16, 0, Pnt_$ctor_Z7AD9E565(99, 99, 99));
        Expect_isTrue(((a_34 = Polyline3D__get_FirstPoint(pl_19), (b_34 = Pnt_$ctor_Z7AD9E565(99, 99, 99), (x_22 = (a_34.X - b_34.X), (y_22 = (a_34.Y - b_34.Y), (z_16 = (a_34.Z - b_34.Z), Math.sqrt(((x_22 * x_22) + (y_22 * y_22)) + (z_16 * z_16)))))))) < 1E-09)("original changed");
        Expect_isTrue(((a_36 = Polyline3D__get_FirstPoint(clone), (b_36 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_23 = (a_36.X - b_36.X), (y_23 = (a_36.Y - b_36.Y), (z_17 = (a_36.Z - b_36.Z), Math.sqrt(((x_23 * x_23) + (y_23 * y_23)) + (z_17 * z_17)))))))) < 1E-09)("clone not changed");
        Test_TestCaseBuilder__Zero(builder$0040_19);
    }));
})(), (() => {
    const builder$0040_20 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ReverseInPlace reverses the polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_20, Test_TestCaseBuilder__Delay_1505(builder$0040_20, () => {
        let a_38, b_38, x_24, y_24, z_18, a_40, b_40, x_25, y_25, z_19;
        const pl_20 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        Polyline3D__ReverseInPlace(pl_20);
        Expect_isTrue(((a_38 = Polyline3D__get_FirstPoint(pl_20), (b_38 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_24 = (a_38.X - b_38.X), (y_24 = (a_38.Y - b_38.Y), (z_18 = (a_38.Z - b_38.Z), Math.sqrt(((x_24 * x_24) + (y_24 * y_24)) + (z_18 * z_18)))))))) < 1E-09)("first point after reverse");
        Expect_isTrue(((a_40 = Polyline3D__get_LastPoint(pl_20), (b_40 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_25 = (a_40.X - b_40.X), (y_25 = (a_40.Y - b_40.Y), (z_19 = (a_40.Z - b_40.Z), Math.sqrt(((x_25 * x_25) + (y_25 * y_25)) + (z_19 * z_19)))))))) < 1E-09)("last point after reverse");
        Test_TestCaseBuilder__Zero(builder$0040_20);
    }));
})(), (() => {
    const builder$0040_21 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Reverse returns new reversed polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_21, Test_TestCaseBuilder__Delay_1505(builder$0040_21, () => {
        let a_42, b_42, x_26, y_26, z_20, a_44, b_44, x_27, y_27, z_21, a_46, b_46, x_28, y_28, z_22;
        const pl_21 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        const rev = Polyline3D__Reverse(pl_21);
        Expect_isTrue(((a_42 = Polyline3D__get_FirstPoint(pl_21), (b_42 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_26 = (a_42.X - b_42.X), (y_26 = (a_42.Y - b_42.Y), (z_20 = (a_42.Z - b_42.Z), Math.sqrt(((x_26 * x_26) + (y_26 * y_26)) + (z_20 * z_20)))))))) < 1E-09)("original first point unchanged");
        Expect_isTrue(((a_44 = Polyline3D__get_FirstPoint(rev), (b_44 = Pnt_$ctor_Z7AD9E565(1, 1, 0), (x_27 = (a_44.X - b_44.X), (y_27 = (a_44.Y - b_44.Y), (z_21 = (a_44.Z - b_44.Z), Math.sqrt(((x_27 * x_27) + (y_27 * y_27)) + (z_21 * z_21)))))))) < 1E-09)("reversed first point");
        Expect_isTrue(((a_46 = Polyline3D__get_LastPoint(rev), (b_46 = Pnt_$ctor_Z7AD9E565(0, 0, 0), (x_28 = (a_46.X - b_46.X), (y_28 = (a_46.Y - b_46.Y), (z_22 = (a_46.Z - b_46.Z), Math.sqrt(((x_28 * x_28) + (y_28 * y_28)) + (z_22 * z_22)))))))) < 1E-09)("reversed last point");
        Test_TestCaseBuilder__Zero(builder$0040_21);
    }));
})(), (() => {
    const builder$0040_22 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CloseIfOpen closes almost closed polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_22, Test_TestCaseBuilder__Delay_1505(builder$0040_22, () => {
        let a_48, b_48, x_29, y_29, z_23;
        const pl_22 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(0.001, 0, 0)]);
        Polyline3D__CloseInPlace_5E38073B(pl_22, 0.01);
        Expect_isTrue(Polyline3D__get_IsClosed(pl_22))("polyline is now closed");
        Expect_isTrue(((a_48 = Polyline3D__get_FirstPoint(pl_22), (b_48 = Polyline3D__get_LastPoint(pl_22), (x_29 = (a_48.X - b_48.X), (y_29 = (a_48.Y - b_48.Y), (z_23 = (a_48.Z - b_48.Z), Math.sqrt(((x_29 * x_29) + (y_29 * y_29)) + (z_23 * z_23)))))))) < 1E-09)("last point equals first");
        Test_TestCaseBuilder__Zero(builder$0040_22);
    }));
})(), (() => {
    const builder$0040_23 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("CloseIfOpen adds point if too far", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_23, Test_TestCaseBuilder__Delay_1505(builder$0040_23, () => {
        let copyOfStruct_6, arg_11, arg_1_6;
        const pl_23 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        const origCount = Polyline3D__get_PointCount(pl_23) | 0;
        Polyline3D__CloseInPlace_5E38073B(pl_23, 0.01);
        const actual_23 = Polyline3D__get_PointCount(pl_23) | 0;
        const expected_21 = (origCount + 1) | 0;
        if ((actual_23 === expected_21) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_23, expected_21, "point count increased");
        }
        else {
            throw new Exception(contains((copyOfStruct_6 = actual_23, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_30) => (structuralHash(x_30) | 0),
            }) ? ((arg_11 = int32ToString(expected_21), (arg_1_6 = int32ToString(actual_23), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_11)(arg_1_6)("point count increased")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(expected_21)(actual_23)("point count increased"));
        }
        Expect_isTrue(Polyline3D__get_IsClosed(pl_23))("polyline is now closed");
        Test_TestCaseBuilder__Zero(builder$0040_23);
    }));
})(), (() => {
    const builder$0040_24 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SetVertex updates vertex", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_24, Test_TestCaseBuilder__Delay_1505(builder$0040_24, () => {
        let a_50, b_50, x_31, y_31, z_24;
        const pl_24 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        Polyline3D__SetVertex(pl_24, 1, Pnt_$ctor_Z7AD9E565(2, 2, 2));
        Expect_isTrue(((a_50 = item(1, Polyline3D__get_Points(pl_24)), (b_50 = Pnt_$ctor_Z7AD9E565(2, 2, 2), (x_31 = (a_50.X - b_50.X), (y_31 = (a_50.Y - b_50.Y), (z_24 = (a_50.Z - b_50.Z), Math.sqrt(((x_31 * x_31) + (y_31 * y_31)) + (z_24 * z_24)))))))) < 1E-09)("vertex updated");
        Test_TestCaseBuilder__Zero(builder$0040_24);
    }));
})(), (() => {
    const builder$0040_25 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SetVertex on closed polyline updates both first and last", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_25, Test_TestCaseBuilder__Delay_1505(builder$0040_25, () => {
        let a_52, b_52, x_32, y_32, z_25, a_54, b_54, x_33, y_33, z_26;
        const pl_25 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
        Polyline3D__SetVertex(pl_25, 0, Pnt_$ctor_Z7AD9E565(5, 5, 5));
        Expect_isTrue(((a_52 = Polyline3D__get_FirstPoint(pl_25), (b_52 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_32 = (a_52.X - b_52.X), (y_32 = (a_52.Y - b_52.Y), (z_25 = (a_52.Z - b_52.Z), Math.sqrt(((x_32 * x_32) + (y_32 * y_32)) + (z_25 * z_25)))))))) < 1E-09)("first point updated");
        Expect_isTrue(((a_54 = Polyline3D__get_LastPoint(pl_25), (b_54 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_33 = (a_54.X - b_54.X), (y_33 = (a_54.Y - b_54.Y), (z_26 = (a_54.Z - b_54.Z), Math.sqrt(((x_33 * x_33) + (y_33 * y_33)) + (z_26 * z_26)))))))) < 1E-09)("last point also updated");
        Test_TestCaseBuilder__Zero(builder$0040_25);
    }));
})(), (() => {
    const builder$0040_26 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SetVertex with last index on closed polyline updates both", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_26, Test_TestCaseBuilder__Delay_1505(builder$0040_26, () => {
        let a_56, b_56, x_34, y_34, z_27, a_58, b_58, x_35, y_35, z_28;
        const pl_26 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
        Polyline3D__SetVertex(pl_26, Polyline3D__get_LastPointIndex(pl_26), Pnt_$ctor_Z7AD9E565(5, 5, 5));
        Expect_isTrue(((a_56 = Polyline3D__get_LastPoint(pl_26), (b_56 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_34 = (a_56.X - b_56.X), (y_34 = (a_56.Y - b_56.Y), (z_27 = (a_56.Z - b_56.Z), Math.sqrt(((x_34 * x_34) + (y_34 * y_34)) + (z_27 * z_27)))))))) < 1E-09)("last point updated");
        Expect_isTrue(((a_58 = Polyline3D__get_FirstPoint(pl_26), (b_58 = Pnt_$ctor_Z7AD9E565(5, 5, 5), (x_35 = (a_58.X - b_58.X), (y_35 = (a_58.Y - b_58.Y), (z_28 = (a_58.Z - b_58.Z), Math.sqrt(((x_35 * x_35) + (y_35 * y_35)) + (z_28 * z_28)))))))) < 1E-09)("first point also updated");
        Test_TestCaseBuilder__Zero(builder$0040_26);
    }));
})(), (() => {
    const builder$0040_27 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ToString works for empty polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_27, Test_TestCaseBuilder__Delay_1505(builder$0040_27, () => {
        Expect_stringContains(toString(Polyline3D_$ctor()), "empty", "ToString contains \'empty\'");
        Test_TestCaseBuilder__Zero(builder$0040_27);
    }));
})(), (() => {
    const builder$0040_28 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("ToString works for non-empty polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_28, Test_TestCaseBuilder__Delay_1505(builder$0040_28, () => {
        const s_1 = toString(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]));
        Expect_stringContains(s_1, "Polyline3D", "ToString contains \'Polyline3D\'");
        Expect_stringContains(s_1, "length", "ToString contains \'length\'");
        Test_TestCaseBuilder__Zero(builder$0040_28);
    }));
})(), (() => {
    const builder$0040_29 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AsString for open polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_29, Test_TestCaseBuilder__Delay_1505(builder$0040_29, () => {
        Expect_stringContains(Polyline3D__get_AsString(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)])), "open", "AsString contains \'open\'");
        Test_TestCaseBuilder__Zero(builder$0040_29);
    }));
})(), (() => {
    const builder$0040_30 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AsString for closed polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_30, Test_TestCaseBuilder__Delay_1505(builder$0040_30, () => {
        Expect_stringContains(Polyline3D__get_AsString(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)])), "closed", "AsString contains \'closed\'");
        Test_TestCaseBuilder__Zero(builder$0040_30);
    }));
})(), (() => {
    const builder$0040_31 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("GetSegment out of range throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_31, Test_TestCaseBuilder__Delay_1505(builder$0040_31, () => {
        const pl_31 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]);
        Expect_throws(() => {
            Polyline3D__GetSegment_Z524259A4(pl_31, 5);
        }, "GetSegment out of range throws");
        Test_TestCaseBuilder__Zero(builder$0040_31);
    }));
})(), (() => {
    const builder$0040_32 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("FirstPoint on empty polyline throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_32, Test_TestCaseBuilder__Delay_1505(builder$0040_32, () => {
        const pl_32 = Polyline3D_$ctor();
        Expect_throws(() => {
            Polyline3D__get_FirstPoint(pl_32);
        }, "FirstPoint on empty polyline throws");
        Test_TestCaseBuilder__Zero(builder$0040_32);
    }));
})(), (() => {
    const builder$0040_33 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("SecondPoint on single point polyline throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_33, Test_TestCaseBuilder__Delay_1505(builder$0040_33, () => {
        const pl_33 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
        Expect_throws(() => {
            Polyline3D__get_SecondPoint(pl_33);
        }, "SecondPoint on single point polyline throws");
        Test_TestCaseBuilder__Zero(builder$0040_33);
    }));
})(), (() => {
    const builder$0040_34 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LastSegment on single point polyline throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_34, Test_TestCaseBuilder__Delay_1505(builder$0040_34, () => {
        const pl_34 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
        Expect_throws(() => {
            Polyline3D__get_LastSegment(pl_34);
        }, "LastSegment on single point polyline throws");
        Test_TestCaseBuilder__Zero(builder$0040_34);
    }));
})(), (() => {
    const builder$0040_35 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("AsFSharpCode generates valid code", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_35, Test_TestCaseBuilder__Delay_1505(builder$0040_35, () => {
        Expect_stringContains(Polyline3D__get_AsFSharpCode(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)])), "Polyline3D.create", "AsFSharpCode contains \'Polyline3D.create\'");
        Test_TestCaseBuilder__Zero(builder$0040_35);
    }));
})(), (() => {
    const builder$0040_36 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("polyline with collinear points", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_36, Test_TestCaseBuilder__Delay_1505(builder$0040_36, () => {
        Expect_floatClose(tol, Polyline3D__get_Length(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0), Pnt_$ctor_Z7AD9E565(3, 0, 0)])), 3, "length of collinear points");
        Test_TestCaseBuilder__Zero(builder$0040_36);
    }));
})(), (() => {
    const builder$0040_37 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("polyline in 3D space", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_37, Test_TestCaseBuilder__Delay_1505(builder$0040_37, () => {
        const pl_37 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 1), Pnt_$ctor_Z7AD9E565(2, 2, 2)]);
        const expectedLength_3 = 2 * Math.sqrt(3);
        Expect_floatClose(tol, Polyline3D__get_Length(pl_37), expectedLength_3, "length in 3D space");
        Test_TestCaseBuilder__Zero(builder$0040_37);
    }));
})(), (() => {
    const builder$0040_38 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("closed cube edge polyline", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_38, Test_TestCaseBuilder__Delay_1505(builder$0040_38, () => {
        const pl_38 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0), Pnt_$ctor_Z7AD9E565(0, 1, 0), Pnt_$ctor_Z7AD9E565(0, 0, 0)]);
        Expect_isTrue(Polyline3D__get_IsClosed(pl_38))("is closed");
        Expect_floatClose(tol, Polyline3D__get_Length(pl_38), 4, "length is 4");
        Test_TestCaseBuilder__Zero(builder$0040_38);
    }));
})(), (() => {
    const builder$0040_39 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("LastPointIndex and LastSegmentIndex", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_39, Test_TestCaseBuilder__Delay_1505(builder$0040_39, () => {
        let copyOfStruct_7, arg_12, arg_1_7, copyOfStruct_8, arg_13, arg_1_8;
        const pl_39 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]);
        const actual_28 = Polyline3D__get_LastPointIndex(pl_39) | 0;
        if ((actual_28 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_28, 2, "LastPointIndex is 2");
        }
        else {
            throw new Exception(contains((copyOfStruct_7 = actual_28, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_36) => (structuralHash(x_36) | 0),
            }) ? ((arg_12 = int32ToString(2), (arg_1_7 = int32ToString(actual_28), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_12)(arg_1_7)("LastPointIndex is 2")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_28)("LastPointIndex is 2"));
        }
        const actual_30 = Polyline3D__get_LastSegmentIndex(pl_39) | 0;
        if ((actual_30 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_30, 1, "LastSegmentIndex is 1");
        }
        else {
            throw new Exception(contains((copyOfStruct_8 = actual_30, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_37) => (structuralHash(x_37) | 0),
            }) ? ((arg_13 = int32ToString(1), (arg_1_8 = int32ToString(actual_30), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_13)(arg_1_8)("LastSegmentIndex is 1")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_30)("LastSegmentIndex is 1"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_39);
    }));
})(), Test_testList("Transformation Methods", ofArray([(() => {
    const builder$0040_40 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Move instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_40, Test_TestCaseBuilder__Delay_1505(builder$0040_40, () => {
        let a_60, b_60, x_38, y_38, z_29, a_62, b_62, x_39, y_39, z_30;
        const moved = Polyline3D__Move_Z394EC5F7(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(1, 1, 0)]), Vec_$ctor_Z7AD9E565(5, 3, 2));
        Expect_isTrue(((a_60 = Polyline3D__get_FirstPoint(moved), (b_60 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_38 = (a_60.X - b_60.X), (y_38 = (a_60.Y - b_60.Y), (z_29 = (a_60.Z - b_60.Z), Math.sqrt(((x_38 * x_38) + (y_38 * y_38)) + (z_29 * z_29)))))))) < 1E-09)("Move - first point");
        Expect_isTrue(((a_62 = Polyline3D__get_LastPoint(moved), (b_62 = Pnt_$ctor_Z7AD9E565(6, 4, 2), (x_39 = (a_62.X - b_62.X), (y_39 = (a_62.Y - b_62.Y), (z_30 = (a_62.Z - b_62.Z), Math.sqrt(((x_39 * x_39) + (y_39 * y_39)) + (z_30 * z_30)))))))) < 1E-09)("Move - last point");
        Test_TestCaseBuilder__Zero(builder$0040_40);
    }));
})(), (() => {
    const builder$0040_41 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveX instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_41, Test_TestCaseBuilder__Delay_1505(builder$0040_41, () => {
        let a_64, b_64, x_40, y_40, z_31;
        Expect_isTrue(((a_64 = Polyline3D__get_FirstPoint(Polyline3D__MoveX_5E38073B(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]), 5)), (b_64 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (x_40 = (a_64.X - b_64.X), (y_40 = (a_64.Y - b_64.Y), (z_31 = (a_64.Z - b_64.Z), Math.sqrt(((x_40 * x_40) + (y_40 * y_40)) + (z_31 * z_31)))))))) < 1E-09)("MoveX - first point");
        Test_TestCaseBuilder__Zero(builder$0040_41);
    }));
})(), (() => {
    const builder$0040_42 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveY instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_42, Test_TestCaseBuilder__Delay_1505(builder$0040_42, () => {
        let a_66, b_66, x_41, y_41, z_32;
        Expect_isTrue(((a_66 = Polyline3D__get_FirstPoint(Polyline3D__MoveY_5E38073B(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]), 3)), (b_66 = Pnt_$ctor_Z7AD9E565(0, 3, 0), (x_41 = (a_66.X - b_66.X), (y_41 = (a_66.Y - b_66.Y), (z_32 = (a_66.Z - b_66.Z), Math.sqrt(((x_41 * x_41) + (y_41 * y_41)) + (z_32 * z_32)))))))) < 1E-09)("MoveY - first point");
        Test_TestCaseBuilder__Zero(builder$0040_42);
    }));
})(), (() => {
    const builder$0040_43 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("MoveZ instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_43, Test_TestCaseBuilder__Delay_1505(builder$0040_43, () => {
        let a_68, b_68, x_42, y_42, z_33;
        Expect_isTrue(((a_68 = Polyline3D__get_FirstPoint(Polyline3D__MoveZ_5E38073B(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]), 2)), (b_68 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_42 = (a_68.X - b_68.X), (y_42 = (a_68.Y - b_68.Y), (z_33 = (a_68.Z - b_68.Z), Math.sqrt(((x_42 * x_42) + (y_42 * y_42)) + (z_33 * z_33)))))))) < 1E-09)("MoveZ - first point");
        Test_TestCaseBuilder__Zero(builder$0040_43);
    }));
})(), (() => {
    const builder$0040_44 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("move static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_44, Test_TestCaseBuilder__Delay_1505(builder$0040_44, () => {
        let a_70, b_70, x_43, y_43, z_34;
        const pl_44 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]);
        Expect_isTrue(((a_70 = Polyline3D__get_FirstPoint(Polyline3D_move(Vec_$ctor_Z7AD9E565(5, 3, 2), pl_44)), (b_70 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_43 = (a_70.X - b_70.X), (y_43 = (a_70.Y - b_70.Y), (z_34 = (a_70.Z - b_70.Z), Math.sqrt(((x_43 * x_43) + (y_43 * y_43)) + (z_34 * z_34)))))))) < 1E-09)("move static - first point");
        Test_TestCaseBuilder__Zero(builder$0040_44);
    }));
})(), (() => {
    const builder$0040_45 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("translate static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_45, Test_TestCaseBuilder__Delay_1505(builder$0040_45, () => {
        let a_72, b_72, x_44, y_44, z_35;
        const pl_46 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]);
        Expect_isTrue(((a_72 = Polyline3D__get_FirstPoint(Polyline3D_translate(Vec_$ctor_Z7AD9E565(5, 3, 2), pl_46)), (b_72 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_44 = (a_72.X - b_72.X), (y_44 = (a_72.Y - b_72.Y), (z_35 = (a_72.Z - b_72.Z), Math.sqrt(((x_44 * x_44) + (y_44 * y_44)) + (z_35 * z_35)))))))) < 1E-09)("translate static - first point");
        Test_TestCaseBuilder__Zero(builder$0040_45);
    }));
})(), (() => {
    const builder$0040_46 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("moveX, moveY, moveZ static methods", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_46, Test_TestCaseBuilder__Delay_1505(builder$0040_46, () => {
        let a_74, b_74, x_45, y_45, z_36, a_76, b_76, x_46, y_46, z_37, a_78, b_78, x_47, y_47, z_38;
        const pl_48 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]);
        const movedX = Polyline3D_moveX(5, pl_48);
        const movedY = Polyline3D_moveY(3, pl_48);
        const movedZ = Polyline3D_moveZ(2, pl_48);
        Expect_isTrue(((a_74 = Polyline3D__get_FirstPoint(movedX), (b_74 = Pnt_$ctor_Z7AD9E565(5, 0, 0), (x_45 = (a_74.X - b_74.X), (y_45 = (a_74.Y - b_74.Y), (z_36 = (a_74.Z - b_74.Z), Math.sqrt(((x_45 * x_45) + (y_45 * y_45)) + (z_36 * z_36)))))))) < 1E-09)("moveX static");
        Expect_isTrue(((a_76 = Polyline3D__get_FirstPoint(movedY), (b_76 = Pnt_$ctor_Z7AD9E565(0, 3, 0), (x_46 = (a_76.X - b_76.X), (y_46 = (a_76.Y - b_76.Y), (z_37 = (a_76.Z - b_76.Z), Math.sqrt(((x_46 * x_46) + (y_46 * y_46)) + (z_37 * z_37)))))))) < 1E-09)("moveY static");
        Expect_isTrue(((a_78 = Polyline3D__get_FirstPoint(movedZ), (b_78 = Pnt_$ctor_Z7AD9E565(0, 0, 2), (x_47 = (a_78.X - b_78.X), (y_47 = (a_78.Y - b_78.Y), (z_38 = (a_78.Z - b_78.Z), Math.sqrt(((x_47 * x_47) + (y_47 * y_47)) + (z_38 * z_38)))))))) < 1E-09)("moveZ static");
        Test_TestCaseBuilder__Zero(builder$0040_46);
    }));
})(), (() => {
    const builder$0040_47 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Transform with identity matrix", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_47, Test_TestCaseBuilder__Delay_1505(builder$0040_47, () => {
        let a_80, b_80, x_48, y_48, z_39;
        const pl_52 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(4, 5, 6)]);
        Expect_isTrue(((a_80 = Polyline3D__get_FirstPoint(Polyline3D__Transform_3CAE9522(pl_52, Matrix_get_identity())), (b_80 = Polyline3D__get_FirstPoint(pl_52), (x_48 = (a_80.X - b_80.X), (y_48 = (a_80.Y - b_80.Y), (z_39 = (a_80.Z - b_80.Z), Math.sqrt(((x_48 * x_48) + (y_48 * y_48)) + (z_39 * z_39)))))))) < 1E-09)("Transform identity - first point");
        Test_TestCaseBuilder__Zero(builder$0040_47);
    }));
})(), (() => {
    const builder$0040_48 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Transform with translation matrix", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_48, Test_TestCaseBuilder__Delay_1505(builder$0040_48, () => {
        let a_82, b_82, x_49, y_49, z_40;
        Expect_isTrue(((a_82 = Polyline3D__get_FirstPoint(Polyline3D__Transform_3CAE9522(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]), Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2)))), (b_82 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_49 = (a_82.X - b_82.X), (y_49 = (a_82.Y - b_82.Y), (z_40 = (a_82.Z - b_82.Z), Math.sqrt(((x_49 * x_49) + (y_49 * y_49)) + (z_40 * z_40)))))))) < 1E-09)("Transform translation - first point");
        Test_TestCaseBuilder__Zero(builder$0040_48);
    }));
})(), (() => {
    const builder$0040_49 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("transform static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_49, Test_TestCaseBuilder__Delay_1505(builder$0040_49, () => {
        let a_84, b_84, x_50, y_50, z_41;
        const pl_54 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]);
        Expect_isTrue(((a_84 = Polyline3D__get_FirstPoint(Polyline3D_transform(Matrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2)), pl_54)), (b_84 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_50 = (a_84.X - b_84.X), (y_50 = (a_84.Y - b_84.Y), (z_41 = (a_84.Z - b_84.Z), Math.sqrt(((x_50 * x_50) + (y_50 * y_50)) + (z_41 * z_41)))))))) < 1E-09)("transform static - first point");
        Test_TestCaseBuilder__Zero(builder$0040_49);
    }));
})(), (() => {
    const builder$0040_50 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("TransformRigid instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_50, Test_TestCaseBuilder__Delay_1505(builder$0040_50, () => {
        let a_86, b_86, x_51, y_51, z_42;
        Expect_isTrue(((a_86 = Polyline3D__get_FirstPoint(Polyline3D__TransformRigid_Z625426AD(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]), RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2)))), (b_86 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_51 = (a_86.X - b_86.X), (y_51 = (a_86.Y - b_86.Y), (z_42 = (a_86.Z - b_86.Z), Math.sqrt(((x_51 * x_51) + (y_51 * y_51)) + (z_42 * z_42)))))))) < 1E-09)("TransformRigid - first point");
        Test_TestCaseBuilder__Zero(builder$0040_50);
    }));
})(), (() => {
    const builder$0040_51 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("transformRigid static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_51, Test_TestCaseBuilder__Delay_1505(builder$0040_51, () => {
        let a_88, b_88, x_52, y_52, z_43;
        const pl_57 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(1, 0, 0)]);
        Expect_isTrue(((a_88 = Polyline3D__get_FirstPoint(Polyline3D_transformRigid(RigidMatrix_createTranslation_Z394EC5F7(Vec_$ctor_Z7AD9E565(5, 3, 2)), pl_57)), (b_88 = Pnt_$ctor_Z7AD9E565(5, 3, 2), (x_52 = (a_88.X - b_88.X), (y_52 = (a_88.Y - b_88.Y), (z_43 = (a_88.Z - b_88.Z), Math.sqrt(((x_52 * x_52) + (y_52 * y_52)) + (z_43 * z_43)))))))) < 1E-09)("transformRigid static - first point");
        Test_TestCaseBuilder__Zero(builder$0040_51);
    }));
})(), (() => {
    const builder$0040_52 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate with identity quaternion", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_52, Test_TestCaseBuilder__Delay_1505(builder$0040_52, () => {
        let a_90, b_90, x_54, y_54, z_45;
        const pl_59 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(4, 5, 6)]);
        Expect_isTrue(((a_90 = Polyline3D__get_FirstPoint(Polyline3D__Rotate_Z2A007687(pl_59, Quaternion_$ctor_77D16AC0(0, 0, 0, 1))), (b_90 = Polyline3D__get_FirstPoint(pl_59), (x_54 = (a_90.X - b_90.X), (y_54 = (a_90.Y - b_90.Y), (z_45 = (a_90.Z - b_90.Z), Math.sqrt(((x_54 * x_54) + (y_54 * y_54)) + (z_45 * z_45)))))))) < 1E-09)("Rotate identity - first point");
        Test_TestCaseBuilder__Zero(builder$0040_52);
    }));
})(), (() => {
    const builder$0040_53 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Rotate 90 degrees around Z axis", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_53, Test_TestCaseBuilder__Delay_1505(builder$0040_53, () => {
        let a_92, axis_1, angHalf, sa, b_92, x_57, y_57, z_48;
        Expect_isTrue(((a_92 = Polyline3D__get_FirstPoint(Polyline3D__Rotate_Z2A007687(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0)]), (axis_1 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), (angHalf = ((0.017453292519943295 * 90) * 0.5), (sa = Math.sin(angHalf), Quaternion_$ctor_77D16AC0(axis_1.X * sa, axis_1.Y * sa, axis_1.Z * sa, Math.cos(angHalf))))))), (b_92 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (x_57 = (a_92.X - b_92.X), (y_57 = (a_92.Y - b_92.Y), (z_48 = (a_92.Z - b_92.Z), Math.sqrt(((x_57 * x_57) + (y_57 * y_57)) + (z_48 * z_48)))))))) < 1E-09)("Rotate 90 - first point");
        Test_TestCaseBuilder__Zero(builder$0040_53);
    }));
})(), (() => {
    const builder$0040_54 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotateByQuaternion static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_54, Test_TestCaseBuilder__Delay_1505(builder$0040_54, () => {
        let a_94, axis_3, angHalf_1, sa_1, b_94, x_60, y_60, z_51;
        const pl_61 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0)]);
        Expect_isTrue(((a_94 = Polyline3D__get_FirstPoint(Polyline3D_rotateByQuaternion((axis_3 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), (angHalf_1 = ((0.017453292519943295 * 90) * 0.5), (sa_1 = Math.sin(angHalf_1), Quaternion_$ctor_77D16AC0(axis_3.X * sa_1, axis_3.Y * sa_1, axis_3.Z * sa_1, Math.cos(angHalf_1))))), pl_61)), (b_94 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (x_60 = (a_94.X - b_94.X), (y_60 = (a_94.Y - b_94.Y), (z_51 = (a_94.Z - b_94.Z), Math.sqrt(((x_60 * x_60) + (y_60 * y_60)) + (z_51 * z_51)))))))) < 1E-09)("rotateByQuaternion - first point");
        Test_TestCaseBuilder__Zero(builder$0040_54);
    }));
})(), (() => {
    const builder$0040_55 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("RotateWithCenter keeps center fixed", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_55, Test_TestCaseBuilder__Delay_1505(builder$0040_55, () => {
        let a_96, axis_5, angHalf_2, sa_2, b_96, x_63, y_63, z_54;
        const pl_63 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0), Pnt_$ctor_Z7AD9E565(2, 2, 0), Pnt_$ctor_Z7AD9E565(0, 2, 0)]);
        const center = Polyline3D__get_Center(pl_63);
        Expect_isTrue(((a_96 = Polyline3D__get_Center(Polyline3D__RotateWithCenter_4928E16A(pl_63, center, (axis_5 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), (angHalf_2 = ((0.017453292519943295 * 90) * 0.5), (sa_2 = Math.sin(angHalf_2), Quaternion_$ctor_77D16AC0(axis_5.X * sa_2, axis_5.Y * sa_2, axis_5.Z * sa_2, Math.cos(angHalf_2))))))), (b_96 = center, (x_63 = (a_96.X - b_96.X), (y_63 = (a_96.Y - b_96.Y), (z_54 = (a_96.Z - b_96.Z), Math.sqrt(((x_63 * x_63) + (y_63 * y_63)) + (z_54 * z_54)))))))) < 1E-09)("RotateWithCenter - center");
        Test_TestCaseBuilder__Zero(builder$0040_55);
    }));
})(), (() => {
    const builder$0040_56 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotateWithCenterByQuaternion static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_56, Test_TestCaseBuilder__Delay_1505(builder$0040_56, () => {
        let a_98, axis_7, angHalf_3, sa_3, b_98, x_66, y_66, z_57;
        const pl_64 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(0, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0), Pnt_$ctor_Z7AD9E565(2, 2, 0), Pnt_$ctor_Z7AD9E565(0, 2, 0)]);
        const center_1 = Polyline3D__get_Center(pl_64);
        Expect_isTrue(((a_98 = Polyline3D__get_Center(Polyline3D_rotateWithCenterByQuaternion(center_1, (axis_7 = UnitVec_$ctor_Z7AD9E565(0, 0, 1), (angHalf_3 = ((0.017453292519943295 * 90) * 0.5), (sa_3 = Math.sin(angHalf_3), Quaternion_$ctor_77D16AC0(axis_7.X * sa_3, axis_7.Y * sa_3, axis_7.Z * sa_3, Math.cos(angHalf_3))))), pl_64)), (b_98 = center_1, (x_66 = (a_98.X - b_98.X), (y_66 = (a_98.Y - b_98.Y), (z_57 = (a_98.Z - b_98.Z), Math.sqrt(((x_66 * x_66) + (y_66 * y_66)) + (z_57 * z_57)))))))) < 1E-09)("rotateWithCenterByQuaternion - center");
        Test_TestCaseBuilder__Zero(builder$0040_56);
    }));
})(), (() => {
    const builder$0040_57 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("Scale instance method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_57, Test_TestCaseBuilder__Delay_1505(builder$0040_57, () => {
        let a_100, b_100, x_67, y_67, z_58, a_102, b_102, x_68, y_68, z_59;
        const scaled = Polyline3D__Scale_5E38073B(Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(2, 4, 6)]), 2);
        Expect_isTrue(((a_100 = Polyline3D__get_FirstPoint(scaled), (b_100 = Pnt_$ctor_Z7AD9E565(2, 4, 6), (x_67 = (a_100.X - b_100.X), (y_67 = (a_100.Y - b_100.Y), (z_58 = (a_100.Z - b_100.Z), Math.sqrt(((x_67 * x_67) + (y_67 * y_67)) + (z_58 * z_58)))))))) < 1E-09)("Scale - first point");
        Expect_isTrue(((a_102 = Polyline3D__get_LastPoint(scaled), (b_102 = Pnt_$ctor_Z7AD9E565(4, 8, 12), (x_68 = (a_102.X - b_102.X), (y_68 = (a_102.Y - b_102.Y), (z_59 = (a_102.Z - b_102.Z), Math.sqrt(((x_68 * x_68) + (y_68 * y_68)) + (z_59 * z_59)))))))) < 1E-09)("Scale - last point");
        Test_TestCaseBuilder__Zero(builder$0040_57);
    }));
})(), (() => {
    const builder$0040_58 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("scale static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_58, Test_TestCaseBuilder__Delay_1505(builder$0040_58, () => {
        let a_104, b_104, x_69, y_69, z_60;
        Expect_isTrue(((a_104 = Polyline3D__get_FirstPoint(Polyline3D_scale(2, Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(1, 2, 3), Pnt_$ctor_Z7AD9E565(2, 4, 6)]))), (b_104 = Pnt_$ctor_Z7AD9E565(2, 4, 6), (x_69 = (a_104.X - b_104.X), (y_69 = (a_104.Y - b_104.Y), (z_60 = (a_104.Z - b_104.Z), Math.sqrt(((x_69 * x_69) + (y_69 * y_69)) + (z_60 * z_60)))))))) < 1E-09)("scale static - first point");
        Test_TestCaseBuilder__Zero(builder$0040_58);
    }));
})(), (() => {
    const builder$0040_59 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("rotate2D static method", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_59, Test_TestCaseBuilder__Delay_1505(builder$0040_59, () => {
        let a_106, rad, b_106, x_70, y_70, z_61;
        const pl_69 = Polyline3D_$ctor_516DFD0A([Pnt_$ctor_Z7AD9E565(1, 0, 0), Pnt_$ctor_Z7AD9E565(2, 0, 0)]);
        Expect_isTrue(((a_106 = Polyline3D__get_FirstPoint(Polyline3D_rotate2D((rad = (0.017453292519943295 * 90), Rotation2D_$ctor_7B00E9A0(Math.sin(rad), Math.cos(rad))), pl_69)), (b_106 = Pnt_$ctor_Z7AD9E565(0, 1, 0), (x_70 = (a_106.X - b_106.X), (y_70 = (a_106.Y - b_106.Y), (z_61 = (a_106.Z - b_106.Z), Math.sqrt(((x_70 * x_70) + (y_70 * y_70)) + (z_61 * z_61)))))))) < 1E-09)("rotate2D - first point");
        Test_TestCaseBuilder__Zero(builder$0040_59);
    }));
})()]))]));

