module EuclidTestSupport

/// Compatibility values for the accuracy levels used by the existing suite.
/// The values preserve the suite's previous absolute and relative tolerances.
type Accuracy =
    { absolute: float
      relative: float }

module Accuracy =
    let low =
        { absolute = 1e-6
          relative = 1e-3 }

    let medium =
        { absolute = 1e-8
          relative = 1e-5 }

    let high =
        { absolute = 1e-10
          relative = 1e-7 }

    let veryHigh =
        { absolute = 1e-12
          relative = 1e-9 }


module Expect =
    let private tagged message assertion =
        Scriptorium.Nib.Assertion.tag message >> assertion

    let equal (actual: 'T) (expected: 'T) =
        fun (message: string) ->
            Scriptorium.Nib.Assertion.assertThat actual (tagged message (Scriptorium.Nib.Assertion.isEqualTo expected))

    let equalWithDiffPrinter (printer: 'T -> 'T -> string) (actual: 'T) (expected: 'T) =
        // Nib already supplies a structural diff for equality failures. Keep the
        // old printer argument in the compatibility surface so the test body stays unchanged.
        let _ = printer
        fun (_message: string) ->
            Scriptorium.Nib.Assertion.assertThat actual (Scriptorium.Nib.Assertion.isEqualTo expected)

    let floatClose (accuracy: Accuracy) (actual: float) (expected: float) (message: string) =
        let close value =
            abs (value - expected) <= accuracy.absolute + accuracy.relative * max (abs value) (abs expected)
        let describe value =
            $"given {value} should be close to {expected}"
        let assertion = Scriptorium.Nib.Assertion.assertion close describe
        Scriptorium.Nib.Assertion.assertThat actual (tagged message assertion)

    let isTrue (actual: bool) (message: string) =
        Scriptorium.Nib.Assertion.assertThat actual (tagged message Scriptorium.Nib.Assertion.isTrue)

    let isFalse (actual: bool) (message: string) =
        Scriptorium.Nib.Assertion.assertThat actual (tagged message Scriptorium.Nib.Assertion.isFalse)

    let isSome (actual: 'T option) (message: string) =
        Scriptorium.Nib.Assertion.assertThat actual (tagged message Scriptorium.Nib.Assertion.Option.isSome)

    let isNone (actual: 'T option) (message: string) =
        Scriptorium.Nib.Assertion.assertThat actual (tagged message Scriptorium.Nib.Assertion.Option.isNone)

    let throws (actual: unit -> unit) (message: string) =
        Scriptorium.Nib.Assertion.assertThat actual (tagged message Scriptorium.Nib.Assertion.throws)

    let stringContains (actual: string) (expected: string) (message: string) =
        let assertion =
            Scriptorium.Nib.Assertion.assertion
                (fun (value: string) -> value.Contains expected)
                (fun value -> $"given {value} should contain {expected}")
        Scriptorium.Nib.Assertion.assertThat actual (tagged message assertion)


type TestBuilder(name: string) =
    member _.Zero() = ()
    member _.Yield(_: unit) = ()
    member _.Combine(first: 'T, right: unit -> unit) : 'T =
        right()
        first
    member _.Delay(body: 'T) = body
    member _.For(sequence: seq<'T>, body: 'T -> unit) =
        for item in sequence do
            body item
    member _.TryWith(body: unit -> 'T, handler: exn -> 'T) =
        try
            body()
        with ex ->
            handler ex
    member _.TryFinally(body: unit -> 'T, compensation: unit -> unit) =
        try
            body()
        finally
            compensation()
    member _.Run(body: unit -> unit) =
        Scriptorium.Quill.Test.test (name, fun _ -> body())

let test name =
    TestBuilder(name)

let testCase name body =
    Scriptorium.Quill.Test.test (name, body)

let testList name tests =
    Scriptorium.Quill.Test.testList (name, tests)

let failtest message =
    raise (System.Exception message)
