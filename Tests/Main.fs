module Euclid.Tests

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

    test Line.tests
    |||
    test Rect2D.tests
    |||
    test Polyline.tests
    |||
    test Topo.tests


#if FABLE_COMPILER
run() |> ignore<int>
#else
let [<EntryPoint>] main _ = run()
#endif



