# Euclid General API

The general utilities and helper types of Euclid, used by both the 2D and the 3D types:
exceptions, tolerances, angle measures and formatting.
The geometry types are in [ApiTable2D.md](ApiTable2D.md) and [ApiTable3D.md](ApiTable3D.md).

- Instance members and fields start with `.`, e.g. `.DistanceTo`.
- Static members are qualified with their type name. They are listed right below their instance counterpart,
  also when its name differs, like `Vc.areParallel` below `.IsParallelTo`.
  The static members without an instance counterpart follow at the end of each table.
- A property shows only its type. `with get, set` marks a property that can be set. `?name` marks an optional parameter.

Generated from the sources by `API-Table/ApiTable.fsx`. Do not edit by hand.

## Contents

- [module EuclidErrors](#module-eucliderrors)
- [EuclidErrors.EuclidException](#eucliderrorseuclidexception)
- [EuclidErrors.EuclidDivByZeroException](#eucliderrorseucliddivbyzeroexception)
- [EuclidErrors.EuclidUnitizingException](#eucliderrorseuclidunitizingexception)
- [EuclidErrors.EuclidNanInfinityException](#eucliderrorseuclidnaninfinityexception)
- [EuclidErrors.EuclidTooSmallException](#eucliderrorseuclidtoosmallexception)
- [EuclidErrors.EuclidNullException](#eucliderrorseuclidnullexception)
- [EuclidErrors.EuclidEmptySeqException](#eucliderrorseuclidemptyseqexception)
- [EuclidErrors.EuclidObsoleteException](#eucliderrorseuclidobsoleteexception)
- [module UtilEuclid](#module-utileuclid)
- [module Cosine](#module-cosine)
- [module Tangent](#module-tangent)
- [module Format](#module-format)
- [module Format.ToMaxDigits](#module-formattomaxdigits)

## module EuclidErrors

| EuclidErrors | Signature |
|:--|:--|
| `EuclidErrors.fail` | `msg: string -> 'Failed` |
| `EuclidErrors.fail1` | `msg: string -> a: 'T -> 'Failed` |
| `EuclidErrors.fail2` | `msg: string -> a: 'T -> b: 'U -> 'Failed` |
| `EuclidErrors.fail3` | `msg: string -> a: 'T -> b: 'U -> c: 'V -> 'Failed` |
| `EuclidErrors.failCollinear` | `msg: string -> a: 'T -> b: 'U -> c: 'V -> 'Failed` |
| `EuclidErrors.failDivide` | `msg: string -> div: float -> obj: 'T -> 'Failed` |
| `EuclidErrors.failEmptySeq` | `funcName: string -> arg: string -> 'Failed` |
| `EuclidErrors.failEven` | `methodName: 'a -> xys: ResizeArray<float> -> 'b` |
| `EuclidErrors.failMod3` | `methodName: 'a -> xyzs: ResizeArray<float> -> 'b` |
| `EuclidErrors.failNaN2` | `msg: string -> x: float -> y: float -> 'Failed` |
| `EuclidErrors.failNaN3` | `msg: string -> x: float -> y: float -> z: float -> 'Failed` |
| `EuclidErrors.failNotOne2` | `msg: string -> x: float -> y: float -> 'Failed` |
| `EuclidErrors.failNotOne3` | `msg: string -> x: float -> y: float -> z: float -> 'Failed` |
| `EuclidErrors.failNull` | `funcName: string -> arg: string -> 'Failed` |
| `EuclidErrors.failObsolete` | `funName: string -> newFunName: string -> 'Failed` |
| `EuclidErrors.failTooClose` | `msg: string -> this: 'T -> other: 'U -> 'Failed` |
| `EuclidErrors.failTooSmall` | `msg: string -> this: 'T -> 'Failed` |
| `EuclidErrors.failTooSmall2` | `msg: string -> this: 'T -> other: 'U -> 'Failed` |
| `EuclidErrors.failUnit2` | `msg: string -> x: float -> y: float -> 'Failed` |
| `EuclidErrors.failUnit3` | `msg: string -> x: float -> y: float -> z: float -> 'Failed` |
| `EuclidErrors.failVertical` | `msg: string -> v: 'T -> 'Failed` |

## EuclidErrors.EuclidException

| EuclidException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidException` |

## EuclidErrors.EuclidDivByZeroException

| EuclidDivByZeroException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidDivByZeroException` |

## EuclidErrors.EuclidUnitizingException

| EuclidUnitizingException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidUnitizingException` |

## EuclidErrors.EuclidNanInfinityException

| EuclidNanInfinityException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidNanInfinityException` |

## EuclidErrors.EuclidTooSmallException

| EuclidTooSmallException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidTooSmallException` |

## EuclidErrors.EuclidNullException

| EuclidNullException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidNullException` |

## EuclidErrors.EuclidEmptySeqException

| EuclidEmptySeqException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidEmptySeqException` |

## EuclidErrors.EuclidObsoleteException

| EuclidObsoleteException | Signature |
|:--|:--|
| `new` | `s: string -> EuclidObsoleteException` |

## module UtilEuclid

| UtilEuclid | Signature |
|:--|:--|
| `UtilEuclid.acosSafe` | `a: float -> float` |
| `UtilEuclid.asinSafe` | `a: float -> float` |
| `UtilEuclid.axisAlignmentTolerance` | `float` |
| `UtilEuclid.clamp01` | `x: float -> float` |
| `UtilEuclid.clamp01NaN` | `x: float -> float` |
| `UtilEuclid.clampBetweenMinusOneAndOne` | `x: float -> float` |
| `UtilEuclid.countTooTinyOrNaN` | `x: float -> int` |
| `UtilEuclid.countTooTinySqOrNaN` | `x: float -> int` |
| `UtilEuclid.halfPi` | `float` |
| `UtilEuclid.isBetweenZeroAndOne` | `x: float -> bool` |
| `UtilEuclid.isBetweenZeroAndOneTolerantExcl` | `x: float -> bool` |
| `UtilEuclid.isBetweenZeroAndOneTolerantIncl` | `x: float -> bool` |
| `UtilEuclid.isMinusOne` | `x: float -> bool` |
| `UtilEuclid.isNanInfinity` | `x: float -> bool` |
| `UtilEuclid.isNegative` | `x: float -> bool` |
| `UtilEuclid.isNotOne` | `x: float -> bool` |
| `UtilEuclid.isNotZero` | `x: float -> bool` |
| `UtilEuclid.isOne` | `x: float -> bool` |
| `UtilEuclid.isTooSmall` | `x: float -> bool` |
| `UtilEuclid.isTooSmallSq` | `x: float -> bool` |
| `UtilEuclid.isTooTiny` | `x: float -> bool` |
| `UtilEuclid.isTooTinySq` | `x: float -> bool` |
| `UtilEuclid.isZero` | `x: float -> bool` |
| `UtilEuclid.matchSign` | `signedValue: float -> numToMatch: float -> float` |
| `UtilEuclid.safeIdx` | `i: int -> length: int -> int` |
| `UtilEuclid.toDegrees` | `radians: float -> float` |
| `UtilEuclid.toRadians` | `degrees: float -> float` |
| `UtilEuclid.twoPi` | `float` |
| `UtilEuclid.withMeasure` | `x: float -> float<'measure>` |
| `UtilEuclid.zeroLengthTolerance` | `float` |
| ``` UtilEuclid.``-1.0 + 1e-6`` ``` | `float` |
| ``` UtilEuclid.``-1.0 - 1e-6`` ``` | `float` |
| ``` UtilEuclid.``1.0 + 1e-6`` ``` | `float` |
| ``` UtilEuclid.``1.0 - 1e-6`` ``` | `float` |
| `( !^ )` | `x: float -> float<'measure>` |

## module Cosine

| Cosine | Signature |
|:--|:--|
| `cosine` | `[<Measure>] type` |
| **Functions and values** | |
| `Cosine.inDegrees` | `c: float<cosine> -> float` |
| ``` Cosine.``0.0+`` ``` | `float<cosine>` |
| ``` Cosine.``0.0`` ``` | `float<cosine>` |
| ``` Cosine.``0.01`` ``` | `float<cosine>` |
| ``` Cosine.``0.025`` ``` | `float<cosine>` |
| ``` Cosine.``0.05`` ``` | `float<cosine>` |
| ``` Cosine.``0.1`` ``` | `float<cosine>` |
| ``` Cosine.``0.25`` ``` | `float<cosine>` |
| ``` Cosine.``0.5`` ``` | `float<cosine>` |
| ``` Cosine.``1.0`` ``` | `float<cosine>` |
| ``` Cosine.``10.0`` ``` | `float<cosine>` |
| ``` Cosine.``15.0`` ``` | `float<cosine>` |
| ``` Cosine.``100.0`` ``` | `float<cosine>` |
| ``` Cosine.``105.0`` ``` | `float<cosine>` |
| ``` Cosine.``110.0`` ``` | `float<cosine>` |
| ``` Cosine.``120.0`` ``` | `float<cosine>` |
| ``` Cosine.``135.0`` ``` | `float<cosine>` |
| ``` Cosine.``150.0`` ``` | `float<cosine>` |
| ``` Cosine.``160.0`` ``` | `float<cosine>` |
| ``` Cosine.``165.0`` ``` | `float<cosine>` |
| ``` Cosine.``170.0`` ``` | `float<cosine>` |
| ``` Cosine.``175.0`` ``` | `float<cosine>` |
| ``` Cosine.``177.5`` ``` | `float<cosine>` |
| ``` Cosine.``179.0`` ``` | `float<cosine>` |
| ``` Cosine.``179.5`` ``` | `float<cosine>` |
| ``` Cosine.``179.75`` ``` | `float<cosine>` |
| ``` Cosine.``179.9`` ``` | `float<cosine>` |
| ``` Cosine.``179.95`` ``` | `float<cosine>` |
| ``` Cosine.``179.975`` ``` | `float<cosine>` |
| ``` Cosine.``179.99`` ``` | `float<cosine>` |
| ``` Cosine.``180.0-`` ``` | `float<cosine>` |
| ``` Cosine.``2.5`` ``` | `float<cosine>` |
| ``` Cosine.``5.0`` ``` | `float<cosine>` |
| ``` Cosine.``20.0`` ``` | `float<cosine>` |
| ``` Cosine.``30.0`` ``` | `float<cosine>` |
| ``` Cosine.``45.0`` ``` | `float<cosine>` |
| ``` Cosine.``60.0`` ``` | `float<cosine>` |
| ``` Cosine.``70.0`` ``` | `float<cosine>` |
| ``` Cosine.``75.0`` ``` | `float<cosine>` |
| ``` Cosine.``80.0`` ``` | `float<cosine>` |
| ``` Cosine.``85.0`` ``` | `float<cosine>` |
| ``` Cosine.``87.5`` ``` | `float<cosine>` |
| ``` Cosine.``89.0`` ``` | `float<cosine>` |
| ``` Cosine.``89.5`` ``` | `float<cosine>` |
| ``` Cosine.``89.75`` ``` | `float<cosine>` |
| ``` Cosine.``89.9`` ``` | `float<cosine>` |
| ``` Cosine.``89.95`` ``` | `float<cosine>` |
| ``` Cosine.``89.975`` ``` | `float<cosine>` |
| ``` Cosine.``89.99`` ``` | `float<cosine>` |
| ``` Cosine.``90.01`` ``` | `float<cosine>` |
| ``` Cosine.``90.025`` ``` | `float<cosine>` |
| ``` Cosine.``90.05`` ``` | `float<cosine>` |
| ``` Cosine.``90.1`` ``` | `float<cosine>` |
| ``` Cosine.``90.25`` ``` | `float<cosine>` |
| ``` Cosine.``90.5`` ``` | `float<cosine>` |
| ``` Cosine.``91.0`` ``` | `float<cosine>` |
| ``` Cosine.``92.5`` ``` | `float<cosine>` |
| ``` Cosine.``95.0`` ``` | `float<cosine>` |
| ``` Cosine.``180.0`` ``` | `float<cosine>` |

## module Tangent

| Tangent | Signature |
|:--|:--|
| `tangent` | `[<Measure>] type` |
| **Functions and values** | |
| `Tangent.inDegrees` | `t: float<tangent> -> float` |
| ``` Tangent.``0.01`` ``` | `float<tangent>` |
| ``` Tangent.``0.025`` ``` | `float<tangent>` |
| ``` Tangent.``0.05`` ``` | `float<tangent>` |
| ``` Tangent.``0.1`` ``` | `float<tangent>` |
| ``` Tangent.``0.25`` ``` | `float<tangent>` |
| ``` Tangent.``0.5`` ``` | `float<tangent>` |
| ``` Tangent.``1.0`` ``` | `float<tangent>` |
| ``` Tangent.``2.5`` ``` | `float<tangent>` |
| ``` Tangent.``5.0`` ``` | `float<tangent>` |
| ``` Tangent.``10.0`` ``` | `float<tangent>` |
| ``` Tangent.``15.0`` ``` | `float<tangent>` |
| ``` Tangent.``20.0`` ``` | `float<tangent>` |
| ``` Tangent.``30.0`` ``` | `float<tangent>` |
| ``` Tangent.``45.0`` ``` | `float<tangent>` |
| ``` Tangent.``60.0`` ``` | `float<tangent>` |
| ``` Tangent.``70.0`` ``` | `float<tangent>` |
| ``` Tangent.``75.0`` ``` | `float<tangent>` |
| ``` Tangent.``80.0`` ``` | `float<tangent>` |
| ``` Tangent.``85.0`` ``` | `float<tangent>` |
| ``` Tangent.``87.5`` ``` | `float<tangent>` |
| ``` Tangent.``89.0`` ``` | `float<tangent>` |
| ``` Tangent.``89.5`` ``` | `float<tangent>` |
| ``` Tangent.``89.75`` ``` | `float<tangent>` |
| ``` Tangent.``89.9`` ``` | `float<tangent>` |
| ``` Tangent.``89.95`` ``` | `float<tangent>` |
| ``` Tangent.``89.975`` ``` | `float<tangent>` |
| ``` Tangent.``89.99`` ``` | `float<tangent>` |
| ``` Tangent.``90.01`` ``` | `float<tangent>` |
| ``` Tangent.``90.025`` ``` | `float<tangent>` |
| ``` Tangent.``90.05`` ``` | `float<tangent>` |
| ``` Tangent.``90.1`` ``` | `float<tangent>` |
| ``` Tangent.``90.25`` ``` | `float<tangent>` |
| ``` Tangent.``90.5`` ``` | `float<tangent>` |
| ``` Tangent.``91.0`` ``` | `float<tangent>` |
| ``` Tangent.``92.5`` ``` | `float<tangent>` |
| ``` Tangent.``95.0`` ``` | `float<tangent>` |
| ``` Tangent.``100.0`` ``` | `float<tangent>` |
| ``` Tangent.``105.0`` ``` | `float<tangent>` |
| ``` Tangent.``110.0`` ``` | `float<tangent>` |
| ``` Tangent.``120.0`` ``` | `float<tangent>` |
| ``` Tangent.``135.0`` ``` | `float<tangent>` |
| ``` Tangent.``150.0`` ``` | `float<tangent>` |
| ``` Tangent.``160.0`` ``` | `float<tangent>` |
| ``` Tangent.``165.0`` ``` | `float<tangent>` |
| ``` Tangent.``170.0`` ``` | `float<tangent>` |
| ``` Tangent.``175.0`` ``` | `float<tangent>` |
| ``` Tangent.``177.5`` ``` | `float<tangent>` |
| ``` Tangent.``179.0`` ``` | `float<tangent>` |
| ``` Tangent.``179.5`` ``` | `float<tangent>` |
| ``` Tangent.``179.75`` ``` | `float<tangent>` |
| ``` Tangent.``179.9`` ``` | `float<tangent>` |
| ``` Tangent.``179.95`` ``` | `float<tangent>` |
| ``` Tangent.``179.975`` ``` | `float<tangent>` |
| ``` Tangent.``179.99`` ``` | `float<tangent>` |

## module Format

| Format | Signature |
|:--|:--|
| `Format.addThousandSeparators` | `thousandSeparator: char -> number: string -> string` |
| `Format.float` | `x: float -> string` |
| `Format.floatAsFSharpCode` | `x: float -> string` |
| `Format.floatWithSeparator` | `thousandSeparator: char -> x: float -> string` |
| `Format.globalThousandSeparator` | `char` |
| `Format.nl` | `string` |
| `Format.userZeroTolerance` | `float` |

## module Format.ToMaxDigits

| ToMaxDigits | Signature |
|:--|:--|
| `ToMaxDigits.d0` | `x: float -> string` |
| `ToMaxDigits.d1` | `x: float -> string` |
| `ToMaxDigits.d10` | `x: float -> string` |
| `ToMaxDigits.d2` | `x: float -> string` |
| `ToMaxDigits.d3` | `x: float -> string` |
| `ToMaxDigits.d4` | `x: float -> string` |
| `ToMaxDigits.d5` | `x: float -> string` |
| `ToMaxDigits.d6` | `x: float -> string` |
| `ToMaxDigits.d7` | `x: float -> string` |
| `ToMaxDigits.d8` | `x: float -> string` |
| `ToMaxDigits.d9` | `x: float -> string` |
| `ToMaxDigits.trimZeros` | `n: string -> string` |
