
import { Expect_floatClose, Expect_throws, Expect_isFalse, Test_TestCaseBuilder__Zero, Expect_isTrue, Test_TestCaseBuilder__Delay_1505, Test_TestCaseBuilder__Run_3A5B6456, FocusState, Test_testList, AccuracyModule_veryHigh } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Test_TestCaseBuilder_$ctor_Z7EF1EC3F } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { Pt_$ctor_7B00E9A0 } from "./Src/Pt.js";
import { getGrouped, getSimilarityData, areSimilar } from "./Src/Similarity2D.js";
import { item } from "./fable_modules/fable-library-js.5.0.0/Array.js";
import { int32ToString, structuralHash, Exception, assertEqual } from "./fable_modules/fable-library-js.5.0.0/Util.js";
import { ofArray, contains } from "./fable_modules/fable-library-js.5.0.0/List.js";
import { equals, class_type, decimal_type, string_type, float64_type, bool_type, int32_type } from "./fable_modules/fable-library-js.5.0.0/Reflection.js";
import { printf, toText } from "./fable_modules/fable-library-js.5.0.0/String.js";

export const tol = AccuracyModule_veryHigh;

export const tests = Test_testList("Similarity2D", ofArray([(() => {
    const builder$0040 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns true for identical objects", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040, Test_TestCaseBuilder__Delay_1505(builder$0040, () => {
        const pts1 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1)]]];
        Expect_isTrue(areSimilar(0.01, getSimilarityData(pts1), getSimilarityData(pts1)))("identical objects are similar");
        Test_TestCaseBuilder__Zero(builder$0040);
    }));
})(), (() => {
    const builder$0040_1 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns false for different sized objects", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_1, Test_TestCaseBuilder__Delay_1505(builder$0040_1, () => {
        const pts1_1 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0)]]];
        Expect_isFalse(areSimilar(0.01, getSimilarityData(pts1_1), getSimilarityData(pts2)))("different sized objects are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_1);
    }));
})(), (() => {
    const builder$0040_2 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns true for shifted objects within tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_2, Test_TestCaseBuilder__Delay_1505(builder$0040_2, () => {
        const pts1_2 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1)]]];
        const pts2_1 = [["cat1", [Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(11, 10), Pt_$ctor_7B00E9A0(11, 11)]]];
        Expect_isTrue(areSimilar(0.01, getSimilarityData(pts1_2), getSimilarityData(pts2_1)))("shifted objects are similar (normalized to origin)");
        Test_TestCaseBuilder__Zero(builder$0040_2);
    }));
})(), (() => {
    const builder$0040_3 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns false for different shapes", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_3, Test_TestCaseBuilder__Delay_1505(builder$0040_3, () => {
        const pts1_3 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(1, 1)]]];
        const pts2_2 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(0, 1)]]];
        Expect_isFalse(areSimilar(0.01, getSimilarityData(pts1_3), getSimilarityData(pts2_2)))("different shapes are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_3);
    }));
})(), (() => {
    const builder$0040_4 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar handles multiple categories", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_4, Test_TestCaseBuilder__Delay_1505(builder$0040_4, () => {
        const pts1_4 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]], ["cat2", [Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0)]]];
        const pts2_3 = [["cat1", [Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(11, 10)]], ["cat2", [Pt_$ctor_7B00E9A0(12, 10), Pt_$ctor_7B00E9A0(13, 10)]]];
        Expect_isTrue(areSimilar(0.01, getSimilarityData(pts1_4), getSimilarityData(pts2_3)))("objects with multiple categories are similar");
        Test_TestCaseBuilder__Zero(builder$0040_4);
    }));
})(), (() => {
    const builder$0040_5 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns false if category count differs", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_5, Test_TestCaseBuilder__Delay_1505(builder$0040_5, () => {
        const pts1_5 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2_4 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]], ["cat2", [Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(3, 0)]]];
        Expect_isFalse(areSimilar(0.01, getSimilarityData(pts1_5), getSimilarityData(pts2_4)))("different category counts are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_5);
    }));
})(), (() => {
    const builder$0040_6 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar returns false if category names differ", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_6, Test_TestCaseBuilder__Delay_1505(builder$0040_6, () => {
        const pts1_6 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2_5 = [["cat2", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        Expect_isFalse(areSimilar(0.01, getSimilarityData(pts1_6), getSimilarityData(pts2_5)))("different category names are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_6);
    }));
})(), (() => {
    const builder$0040_7 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSimilarityData normalizes to origin", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_7, Test_TestCaseBuilder__Delay_1505(builder$0040_7, () => {
        const grp = item(0, getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(10, 10), Pt_$ctor_7B00E9A0(11, 10), Pt_$ctor_7B00E9A0(11, 11)]]]).groups);
        Expect_isTrue(item(0, grp.points).X >= 0)("first point X >= 0");
        Expect_isTrue(item(0, grp.points).Y >= 0)("first point Y >= 0");
        Test_TestCaseBuilder__Zero(builder$0040_7);
    }));
})(), (() => {
    const builder$0040_8 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSimilarityData sorts points by X", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_8, Test_TestCaseBuilder__Delay_1505(builder$0040_8, () => {
        const grp_1 = item(0, getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(2, 0), Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]]).groups);
        Expect_isTrue(item(0, grp_1.points).X <= item(1, grp_1.points).X)("first point has smallest X");
        Expect_isTrue(item(1, grp_1.points).X <= item(2, grp_1.points).X)("second point X <= third point X");
        Test_TestCaseBuilder__Zero(builder$0040_8);
    }));
})(), (() => {
    const builder$0040_9 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getGrouped groups similar items", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_9, Test_TestCaseBuilder__Delay_1505(builder$0040_9, () => {
        let copyOfStruct, arg, arg_1;
        const groups = getGrouped(0.01, [1, 2, 3, 4], [getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]])]);
        const actual_1 = groups.length | 0;
        if ((actual_1 === 2) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_1, 2, "two groups created");
        }
        else {
            throw new Exception(contains((copyOfStruct = actual_1, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x) => (structuralHash(x) | 0),
            }) ? ((arg = int32ToString(2), (arg_1 = int32ToString(actual_1), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg)(arg_1)("two groups created")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(2)(actual_1)("two groups created"));
        }
        const group1Size = item(0, groups).length | 0;
        const group2Size = item(1, groups).length | 0;
        Expect_isTrue(((group1Size === 3) && (group2Size === 1)) ? true : ((group1Size === 1) && (group2Size === 3)))("groups have correct sizes");
        Test_TestCaseBuilder__Zero(builder$0040_9);
    }));
})(), (() => {
    const builder$0040_10 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getGrouped with all unique items", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_10, Test_TestCaseBuilder__Delay_1505(builder$0040_10, () => {
        let copyOfStruct_1, arg_6, arg_1_1;
        const groups_1 = getGrouped(0.01, [1, 2, 3], [getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(2, 0)]]]), getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(3, 0)]]])]);
        const actual_3 = groups_1.length | 0;
        if ((actual_3 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_3, 3, "three unique groups");
        }
        else {
            throw new Exception(contains((copyOfStruct_1 = actual_3, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_1) => (structuralHash(x_1) | 0),
            }) ? ((arg_6 = int32ToString(3), (arg_1_1 = int32ToString(actual_3), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_6)(arg_1_1)("three unique groups")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_3)("three unique groups"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_10);
    }));
})(), (() => {
    const builder$0040_11 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getGrouped with all identical items", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_11, Test_TestCaseBuilder__Delay_1505(builder$0040_11, () => {
        let copyOfStruct_2, arg_7, arg_1_2, copyOfStruct_3, arg_8, arg_1_3;
        const items_2 = [1, 2, 3];
        const simData = getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]]);
        const groups_2 = getGrouped(0.01, items_2, [simData, simData, simData]);
        const actual_5 = groups_2.length | 0;
        if ((actual_5 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_5, 1, "one group for identical items");
        }
        else {
            throw new Exception(contains((copyOfStruct_2 = actual_5, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_2) => (structuralHash(x_2) | 0),
            }) ? ((arg_7 = int32ToString(1), (arg_1_2 = int32ToString(actual_5), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_7)(arg_1_2)("one group for identical items")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_5)("one group for identical items"));
        }
        const actual_7 = item(0, groups_2).length | 0;
        if ((actual_7 === 3) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_7, 3, "group contains all items");
        }
        else {
            throw new Exception(contains((copyOfStruct_3 = actual_7, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_3) => (structuralHash(x_3) | 0),
            }) ? ((arg_8 = int32ToString(3), (arg_1_3 = int32ToString(actual_7), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_8)(arg_1_3)("group contains all items")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(3)(actual_7)("group contains all items"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_11);
    }));
})(), (() => {
    const builder$0040_12 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getGrouped throws on count mismatch", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_12, Test_TestCaseBuilder__Delay_1505(builder$0040_12, () => {
        const items_3 = [1, 2];
        const sims_3 = [getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0)]]])];
        Expect_throws(() => {
            getGrouped(0.01, items_3, sims_3);
        }, "throws on count mismatch");
        Test_TestCaseBuilder__Zero(builder$0040_12);
    }));
})(), (() => {
    const builder$0040_13 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar with tolerance", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_13, Test_TestCaseBuilder__Delay_1505(builder$0040_13, () => {
        const pts1_7 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2_6 = [["cat1", [Pt_$ctor_7B00E9A0(0.005, 0.005), Pt_$ctor_7B00E9A0(1, 0)]]];
        const obj1_7 = getSimilarityData(pts1_7);
        const obj2_7 = getSimilarityData(pts2_6);
        Expect_isTrue(areSimilar(0.01, obj1_7, obj2_7))("similar within tolerance 0.01");
        Expect_isFalse(areSimilar(0.001, obj1_7, obj2_7))("not similar with tolerance 0.001");
        Test_TestCaseBuilder__Zero(builder$0040_13);
    }));
})(), (() => {
    const builder$0040_14 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar with different point counts", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_14, Test_TestCaseBuilder__Delay_1505(builder$0040_14, () => {
        const pts1_8 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0)]]];
        const pts2_7 = [["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(1, 0), Pt_$ctor_7B00E9A0(2, 0)]]];
        Expect_isFalse(areSimilar(0.01, getSimilarityData(pts1_8), getSimilarityData(pts2_7)))("different point counts are not similar");
        Test_TestCaseBuilder__Zero(builder$0040_14);
    }));
})(), (() => {
    const builder$0040_15 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSimilarityData creates correct bounding rectangle", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_15, Test_TestCaseBuilder__Delay_1505(builder$0040_15, () => {
        const obj_2 = getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(0, 0), Pt_$ctor_7B00E9A0(3, 4)]]]);
        Expect_floatClose(tol, obj_2.extend.X, 3, "extent X is 3");
        Expect_floatClose(tol, obj_2.extend.Y, 4, "extent Y is 4");
        Test_TestCaseBuilder__Zero(builder$0040_15);
    }));
})(), (() => {
    const builder$0040_16 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("getSimilarityData handles single point", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_16, Test_TestCaseBuilder__Delay_1505(builder$0040_16, () => {
        let copyOfStruct_4, arg_9, arg_1_4, copyOfStruct_5, arg_10, arg_1_5;
        const obj_3 = getSimilarityData([["cat1", [Pt_$ctor_7B00E9A0(5, 5)]]]);
        const actual_11 = obj_3.groups.length | 0;
        if ((actual_11 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_11, 1, "single point object created");
        }
        else {
            throw new Exception(contains((copyOfStruct_4 = actual_11, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_4) => (structuralHash(x_4) | 0),
            }) ? ((arg_9 = int32ToString(1), (arg_1_4 = int32ToString(actual_11), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_9)(arg_1_4)("single point object created")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_11)("single point object created"));
        }
        const actual_13 = item(0, obj_3.groups).points.length | 0;
        if ((actual_13 === 1) ? true : !(new Function("try {return this===window;}catch(e){ return false;}"))()) {
            assertEqual(actual_13, 1, "single point in group");
        }
        else {
            throw new Exception(contains((copyOfStruct_5 = actual_13, int32_type), ofArray([int32_type, bool_type, float64_type, string_type, decimal_type, class_type("System.Guid")]), {
                Equals: equals,
                GetHashCode: (x_5) => (structuralHash(x_5) | 0),
            }) ? ((arg_10 = int32ToString(1), (arg_1_5 = int32ToString(actual_13), toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%s</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%s</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(arg_10)(arg_1_5)("single point in group")))) : toText(printf("<span style=\'color:black\'>Expected:</span> <br /><div style=\'margin-left:20px; color:crimson\'>%A</div><br /><span style=\'color:black\'>Actual:</span> </br ><div style=\'margin-left:20px;color:crimson\'>%A</div><br /><span style=\'color:black\'>Message:</span> </br ><div style=\'margin-left:20px; color:crimson\'>%s</div>"))(1)(actual_13)("single point in group"));
        }
        Test_TestCaseBuilder__Zero(builder$0040_16);
    }));
})(), (() => {
    const builder$0040_17 = Test_TestCaseBuilder_$ctor_Z7EF1EC3F("areSimilar with empty point sets throws", new FocusState(0, []));
    return Test_TestCaseBuilder__Run_3A5B6456(builder$0040_17, Test_TestCaseBuilder__Delay_1505(builder$0040_17, () => {
        const pts1_9 = [["cat1", []]];
        const pts2_8 = [["cat1", []]];
        Expect_throws(() => {
            areSimilar(0.01, getSimilarityData(pts1_9), getSimilarityData(pts2_8));
        }, "empty point sets throw");
        Test_TestCaseBuilder__Zero(builder$0040_17);
    }));
})()]));

