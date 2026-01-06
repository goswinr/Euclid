module TestUtilEuclid

open Euclid
open Euclid.UtilEuclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let tests =
    testList "UtilEuclid" [

        test "isTooSmall true for small" {
            Expect.isTrue (isTooSmall 1e-7) "1e-7 should be too small"
            Expect.isTrue (isTooSmall System.Double.NaN) "NaN treated as too small"
            Expect.isFalse (isTooSmall 1e-4) "1e-4 not too small"
        }

        test "isTooTiny and countTooTinyOrNaN" {
            Expect.isTrue (isTooTiny 1e-13) "1e-13 tiny (<1e-12)"
            Expect.isFalse (isTooTiny 2e-12) "> tolerance"
            Expect.equal (countTooTinyOrNaN 1e-13) 1 "tiny counts 1"
            Expect.equal (countTooTinyOrNaN 2e-12) 0 "not tiny counts 0"
        }

        test "isTooTinySq and countTooTinySqOrNaN" {
            Expect.isTrue (isTooTinySq 1e-25) "<1e-24"
            Expect.isFalse (isTooTinySq 2e-24) ">=1e-24"
            Expect.equal (countTooTinySqOrNaN 1e-25) 1 "sq tiny counts 1"
            Expect.equal (countTooTinySqOrNaN 2e-24) 0 "sq not tiny counts 0"
        }

        test "isNegative includes NaN" {
            Expect.isTrue (isNegative System.Double.NaN) "NaN counts as negative"
            Expect.isFalse (isNegative 0.0) "0 not negative"
            Expect.isTrue (isNegative -0.0001) "negative"
        }

        test "degree rad conversion round trip" {
            let d = 33.4
            let r = toRadians d
            let d2 = toDegrees r
            Expect.floatClose Accuracy.veryHigh d d2 "deg->rad->deg roundtrip"
        }

        test "clampBetweenMinusOneAndOne" {
            Expect.equal (clampBetweenMinusOneAndOne -2.0) -1.0 "low clamp"
            Expect.equal (clampBetweenMinusOneAndOne 2.0) 1.0 "high clamp"
            Expect.equal (clampBetweenMinusOneAndOne 0.5) 0.5 "inside"
        }

        test "clampBetweenZeroAndOne" {
            Expect.equal (clampBetweenZeroAndOne -0.2) 0.0 "low clamp"
            Expect.equal (clampBetweenZeroAndOne 1.2) 1.0 "high clamp"
            Expect.equal (clampBetweenZeroAndOne 0.5) 0.5 "inside"
        }

        test "asinSafe and acosSafe clamp" {
            let a = asinSafe 2.0 // should clamp to 1
            let b = acosSafe -2.0 // should clamp to -1
            Expect.floatClose Accuracy.veryHigh a (System.Math.Asin 1.0) "asin clamp"
            Expect.floatClose Accuracy.veryHigh b (System.Math.Acos -1.0) "acos clamp"
        }

        test "isOne/isNotOne" {
            Expect.isTrue (isOne 1.0) "1 is one"
            Expect.isTrue (isNotOne 1.1) "1.1 not one"
        }

        test "isZero/isNotZero" {
            Expect.isTrue (isZero 1e-7) "within tolerance"
            Expect.isFalse (isZero 1e-4) "outside tolerance"
            Expect.isTrue (isNotZero 1e-3) "not zero"
        }

        test "isBetweenZeroAndOne tolerant" {
            Expect.isTrue (isBetweenZeroAndOne 0.5) "inside"
            Expect.isFalse (isBetweenZeroAndOne -0.01) "outside"
            Expect.isTrue (isBetweenZeroAndOneTolerantIncl -5e-7) "tolerant lower"
            Expect.isTrue (isBetweenZeroAndOneTolerantIncl 1.0000005) "tolerant upper"
        }

        test "matchSign" {
            Expect.equal (matchSign -5.0 3.0) -3.0 "match negative"
            Expect.equal (matchSign 5.0 -3.0) 3.0 "match positive"
        }

        test "saveIdx wraps" {
            let arr = [|0;1;2;3|]
            let l = arr.Length
            let idx1 = saveIdx -1 l
            let idx2 = saveIdx 5 l
            Expect.equal arr[idx1] 3 "neg wraps"
            Expect.equal arr[idx2] 1 "overflow wraps"
        }





    ]
