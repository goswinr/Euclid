module Euclid.Tests

open Scriptorium.Quill
open type Scriptorium.Quill.Runner

#if !FABLE_COMPILER
open System.Globalization
open System.Threading

// Keep assertion messages stable across machines. and ensure floats use '.' as decimal separator not ','
Thread.CurrentThread.CurrentCulture   <- CultureInfo.GetCultureInfo "en-US"
Thread.CurrentThread.CurrentUICulture <- CultureInfo.GetCultureInfo "en-US"
#endif

// Several geometry tests compare against quadratic brute-force references.
// Give those tests enough time on slower targets.
[<EntryPoint>]
let main _ =
    runTestsWith (
        noTimeout >> slowThreshold 2000,
        [
            TestLine.testsIsCoincident
            TestLine.testsFastMethods
            TestLine.testsFastParallel3D
            TestLine.tests
            TestXLine2D.tests
            TestXLine3D.tests
            TestBBox.tests
            TestBox.tests
            TestFreeBox.tests
            TestPlane.tests
            TestBRect.tests
            TestRect2D.tests
            TestRect3D.tests
            TestPolyline.tests
            TestPolyline.testsDup
            TestPolyline.testsComprehensive
            TestPolyline.testsSpecial
            TestPolyline3D.tests
            TestTopo.tests
            TestTopo.tests3D
            TestTopo.testsCached
            TestPoints.tests
            TestSimilarity2D.tests
            TestRotation2D.tests
            TestQuat.tests
            TestMatrix.tests
            TestRigidMatrix.tests
            TestTria2D.tests
            TestTria3D.tests
            TestOffset2D.tests
            TestOffset2D.testsExtra
            TestOffset3D.tests
            TestHarmonization.tests
            TestVectors.tests
            TestJson.tests
            TestFormat.tests
            TestUtilEuclid.tests
            TestPolyLabel.tests
            TestAsFSharpCode.tests
            TestResizeArr.tests
        ])
