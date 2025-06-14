module Euclid.Tests
open System

#if FABLE_COMPILER
open Fable.Mocha
let test x = Mocha.runTests x
#else
open Expecto
open System.Globalization
open System.Threading
Thread.CurrentThread.CurrentCulture <- CultureInfo.GetCultureInfo("en-US") // so that a float never has a comma as decimal separator
Thread.CurrentThread.CurrentUICulture <- CultureInfo.GetCultureInfo("en-US")
let test x =  runTestsWithCLIArgs [] [||] x
#endif




let run () =

    test TestLine.tests
    |||
    test TestRect2D.tests
    |||
    test TestRect3D.tests
    |||
    test TestPolyline.tests
    |||
    test TestTopo.tests
    |||
    test TestQuat.tests
    |||
    test TestMatrix.tests


#if FABLE_COMPILER
# nowarn "20" //The result of this expression has type 'int' and is implicitly ignored.
run()

#else

[<EntryPoint>]
let main ([<ParamArray>] _args: string[]) = run()
#endif



