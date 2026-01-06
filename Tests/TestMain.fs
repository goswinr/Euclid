module Euclid.Tests
open System

#if FABLE_COMPILER
open Fable.Mocha
let test x = Mocha.runTests x
#else
open Expecto
open System.Globalization
open System.Threading
Thread.CurrentThread.CurrentCulture   <- CultureInfo.GetCultureInfo "en-US" // so that a float never has a comma as decimal separator
Thread.CurrentThread.CurrentUICulture <- CultureInfo.GetCultureInfo "en-US"
let test x =  runTestsWithCLIArgs [] [||] x
#endif




let run () =

    test TestLine.testsIsCoincident
    |||
    test TestLine.testsFastMethods
    |||
    test TestLine.testsFastParallel3D
    |||
    test TestLine.tests
    |||
    test TestXLine2D.tests
    |||
    test TestXLine3D.tests
    |||
    test TestBBox.tests
    |||
    test TestBox.tests
    |||
    test TestFreeBox.tests
    |||
    test TestPlane.tests
    |||
    test TestBRect.tests
    |||
    test TestRect2D.tests
    |||
    test TestRect3D.tests
    |||
    test TestPolyline.tests
    |||
    test TestPolyline.testsDup
    |||
    test TestPolyline3D.tests
    |||
    test TestTopo.tests
    |||
    test TestPoints.tests
    |||
    test TestSimilarity2D.tests
    |||
    test TestRotation2D.tests
    |||
    test TestQuat.tests
    |||
    test TestMatrix.tests
    |||
    test TestRigidMatrix.tests
    |||
    test TestTria2D.tests
    |||
    test TestTria3D.tests
    |||
    test TestOffset2D.tests
    |||
    test TestOffset3D.tests
    |||
    test TestHarmonization.tests
    |||
    test TestVectors.tests
    |||
    test TestFormat.tests
    |||
    test TestUtilEuclid.tests
    |||
    test TestPolyLabel.tests
    |||
    test TestAsFSharpCode.tests


#if FABLE_COMPILER
# nowarn "20" //The result of this expression has type 'int' and is implicitly ignored.
run()

#else

[<EntryPoint>]
let main ([<ParamArray>] _args: string[]) = run()
#endif



