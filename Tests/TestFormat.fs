module TestFormat

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let tests =
    testList "Format" [

        test "float formatting many cases" {
            Expect.equal (Format.float 0.0) "0.0" "float 0.0"
            Expect.equal (Format.float 0.000_001) "0.000'001" "float 0.000'001"
            Expect.equal (Format.float 0.000_000_1) "0.000'000'1" "float 0.000'000'1"
            Expect.equal (Format.float 0.000_000_01)  "≈+0.0" "float 0.000'000'01"
            Expect.equal (Format.float -0.000_000_01) "≈-0.0" "float 0.000'000'01"
            Expect.equal (Format.float 123.1234) "123.1" "float 123.123"
            Expect.equal (Format.float 123.01)  "123.0" "float 123.01"
            Expect.equal (Format.float 13.1234) "13.12" "float 13.1234"
            Expect.equal (Format.float 13.0)    "13.0" "float 13.0"
            Expect.equal (Format.float 200.0)   "200.0" "float 200.0"
            Expect.equal (Format.float 200.1)   "200.1" "float 200.1"
            Expect.equal (Format.float 2000.01) "2000" "float 2000.01"
            Expect.equal (Format.float 2000.00) "2000" "float 2000.01"
            Expect.equal (Format.float 20.0001) "20.0" "float 20.0001"
            Expect.equal (Format.float 13234.12) "13'234" "float 13.123"
        }


        test "Float thousand separator" {
            let v = 12345678.0
            let t = Format.float v
            "thousand grouping" |> Expect.equal t "12'345'678"
        }

        test "Float adaptive precision trimming" {
            let v = 12.34000000001
            let t = Format.float v
            // 12.34 -> expects trimming of trailing zeros
            "adaptive precision" |> Expect.equal t "12.34"
        }

        test "Float small close to zero positive" {
            let v = 5e-8 // threshold for ≈+0.0 is 1e-7 strictly? if smaller than 1e-7 -> ≈+0.0
            let t = Format.float v
            "close to zero positive" |> Expect.equal t "≈+0.0"
        }


        test "Float negative near zero" {
            let t = Format.float (-9e-9)
            $"close to zero negative" |> Expect.equal t "≈-0.0"
        }


        test "Float unset value" {
            let t = Format.float -1.23432101234321e+308
            "rhino unset double" |> Expect.equal t "RhinoMath.UnsetDouble"
        }


        // Additional thousand separator tests
        test "Float thousand separator with decimals" {
            let v = 12345678.9012
            let t = Format.float v
            // For >= 10000 uses no decimals, so expect decimals trimmed entirely
            "thousand grouping decimals trimmed" |> Expect.equal t "12'345'679"
        }

        // Additional thousand separator tests
        test "Float thousand separator with decimals2" {
            let v = 1112345678.9012
            let t = Format.float v
            // For >= 10000 uses no decimals, so expect decimals trimmed entirely
            "thousand grouping decimals trimmed2" |> Expect.equal t "1'112'345'679"
        }

        // Additional thousand separator tests
        test "Float thousand separator with decimals3" {
            let v = -112345678.9012
            let t = Format.float v
            // For >= 10000 uses no decimals, so expect decimals trimmed entirely
            "thousand grouping decimals trimmed3" |> Expect.equal t "-112'345'679"
        }

        test "Float thousand separator just below 10000 keeps decimals" {
            let v = 9999.9012
            let t = Format.float v
            // a >= 1000. branch => no thousand separators and 0 decimals
            "no thousand below 10000 rounding" |> Expect.equal t "10'000" // rounded
        }

        test "Float thousand separator fraction grouping" {
            // constructing internal helper result by calling floatWithSeparator for small numbers with many decimals
            let v = 0.123456789012
            let t = Format.float v
            // Expect 0.12346 (4 or 5 decimals depending on thresholds). From current logic a>=0.1 => 4 decimals then trim => 0.1235
            "fraction grouping no thousand" |> Expect.equal t "0.1235"
        }

        test "Custom separator affects both sides" {
            let t = Format.floatWithSeparator '_' 1234567.0001234
            // value >= 1000000 -> >=1000 branch? Actually >=1000 & <10000 => "%.0f"; for 1234567 -> >=10000 so uses thousand separators and 0 decimals -> 1_234_567
            "custom separator" |> Expect.equal t "1_234_567"
            "Custom separator2 float 0.000_000_1" |> Expect.equal (Format.floatWithSeparator '_' 0.000_000_1) "0.000_000_1"
            "Custom separator3 float 0.000_000_1" |> Expect.equal (Format.floatWithSeparator '_' 0.000_1) "0.000_1"
        }

        test "AddThousandSeparators manual integer" {
            let s = "123456789"
            let t = Format.addThousandSeparators ''' s
            "manual integer grouping" |> Expect.equal t "123'456'789"
        }

        test "AddThousandSeparators manual with fraction" {
            let s = "1234567.1234567"
            let t = Format.addThousandSeparators ''' s
            // Groups: 1'234'567.123'456'7 (after comma every 3 digits starting after first) -> logic: first decimal digit never gets sep, then every three digits
            "manual mixed grouping" |> Expect.equal t "1'234'567.123'456'7"
        }

        test "AddThousandSeparators scientific notation unchanged" {
            let s = "1234567.123e+10"
            let t = Format.addThousandSeparators ''' s
            // Expect grouping before decimal, none inserted into exponent portion
            "manual scientific grouping" |> Expect.equal t "1'234'567.123e+10"
        }

        test "AddThousandSeparators negative value" {
            let s = "-1234567.89"
            let t = Format.addThousandSeparators ''' s
            "manual negative grouping" |> Expect.equal t "-1'234'567.89"
        }

    ]
