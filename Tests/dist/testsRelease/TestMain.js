
import { Mocha_runTests } from "./fable_modules/Fable.Mocha.2.17.0/Mocha.fs.js";
import { tests, testsFastParallel3D, testsFastMethods, testsIsCoincident } from "./TestLine.js";
import { tests as tests_1 } from "./TestXLine2D.js";
import { tests as tests_2 } from "./TestXLine3D.js";
import { tests as tests_3 } from "./TestBBox.js";
import { tests as tests_4 } from "./TestBox.js";
import { tests as tests_5 } from "./TestFreeBox.js";
import { tests as tests_6 } from "./TestPlane.js";
import { tests as tests_7 } from "./TestBRect.js";
import { tests as tests_8 } from "./TestRect2D.js";
import { tests as tests_9 } from "./TestRect3D.js";
import { testsComprehensive, testsDup, tests as tests_10 } from "./TestPolyline.js";
import { tests as tests_11 } from "./TestPolyline3D.js";
import { tests as tests_12 } from "./TestTopo.js";
import { tests as tests_13 } from "./TestPoints.js";
import { tests as tests_14 } from "./TestSimilarity2D.js";
import { tests as tests_15 } from "./TestRotation2D.js";
import { tests as tests_16 } from "./TestQuat.js";
import { tests as tests_17 } from "./TestMatrix.js";
import { tests as tests_18 } from "./TestRigidMatrix.js";
import { tests as tests_19 } from "./TestTria2D.js";
import { tests as tests_20 } from "./TestTria3D.js";
import { tests as tests_21 } from "./TestOffset2D.js";
import { tests as tests_22 } from "./TestOffset3D.js";
import { tests as tests_23 } from "./TestHarmonization.js";
import { tests as tests_24 } from "./TestVectors.js";
import { tests as tests_25 } from "./TestFormat.js";
import { tests as tests_26 } from "./TestUtilEuclid.js";
import { tests as tests_27 } from "./TestPolyLabel.js";
import { tests as tests_28 } from "./TestAsFSharpCode.js";
import { tests as tests_29 } from "./TestResizeArr.js";

export function test(x) {
    return Mocha_runTests(x) | 0;
}

export function run() {
    return ((((((((((((((((((((((((((((((((((test(testsIsCoincident) | test(testsFastMethods)) | test(testsFastParallel3D)) | test(tests)) | test(tests_1)) | test(tests_2)) | test(tests_3)) | test(tests_4)) | test(tests_5)) | test(tests_6)) | test(tests_7)) | test(tests_8)) | test(tests_9)) | test(tests_10)) | test(testsDup)) | test(testsComprehensive)) | test(tests_11)) | test(tests_12)) | test(tests_13)) | test(tests_14)) | test(tests_15)) | test(tests_16)) | test(tests_17)) | test(tests_18)) | test(tests_19)) | test(tests_20)) | test(tests_21)) | test(tests_22)) | test(tests_23)) | test(tests_24)) | test(tests_25)) | test(tests_26)) | test(tests_27)) | test(tests_28)) | test(tests_29)) | 0;
}

run();

