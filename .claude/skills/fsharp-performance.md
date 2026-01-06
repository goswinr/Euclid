# F# Performance and Correctness Skill

This skill provides patterns for writing high-performance, correct F# code based on production-quality patterns from the Euclid geometry library.

---

## Core Principles

1. **Minimize allocations** - Use `ResizeArray<_>` instead of F# lists, avoid intermediate collections
2. **Inline hot paths** - Mark small numeric operations `inline` for JIT optimization
3. **Fail fast with context** - Validate early, provide detailed error messages
4. **Immutable structs + extension modules** - Keep core types minimal, add functionality via extensions
5. **Discriminated Unions for variants** - Model algorithm options and states as DUs

---

## 1. RESIZEARRAY OVER LIST

### Why
- F# `list` is immutable linked list - O(n) append, poor cache locality
- `ResizeArray<_>` (System.Collections.Generic.List) is contiguous memory, O(1) amortized append

### Patterns

**Pre-allocate with known capacity:**

```fsharp
// BAD: List with O(n²) append pattern
let results =
    items |> List.fold (fun acc x -> acc @ [transform x]) []

// GOOD: ResizeArray with pre-allocation and index loop
let results = ResizeArray<Result>(items.Count)
for i = 0 to items.LastIndex do
    results.Add(transform items.[i])
```

**Shallow copy via GetRange (fastest):**

```fsharp
let sorted = xs.GetRange(0, xs.Count)  // Shallow copy
sorted.Sort(comparison)
sorted
```


**Iterate by index using LastIndex (avoids enumerator allocation):**

```fsharp
// BAD: for-in allocates an enumerator object
for item in xs do
    process item

// GOOD: Index loop with LastIndex - no allocation
for i = 0 to xs.LastIndex do
    process xs.[i]
```

> **Note:** `LastIndex` is an extension property returning `Count - 1`. Using it makes loops cleaner and avoids off-by-one errors.

---

## 2. MUTABLE LOOP VARIABLES

### When Appropriate
- Hot path accumulation (min/max finding, searching)
- Building collections in loops
- Tracking state across iterations

### Patterns

**Search with early exit:**

```fsharp
let tryFindBack predicate (xs: ResizeArray<'T>) =
    let mutable i = xs.Count - 1
    let mutable result = None
    while i >= 0 do
        let element = xs.[i]
        i <- i - 1
        if predicate element then
            result <- Some element
            i <- -1  // Break the loop
    result
```

**Min/max by projection:**

```fsharp
let minIndexBy projection (xs: ResizeArray<'T>) =
    if xs.Count = 0 then failwith "Empty collection"
    let mutable minVal = projection xs.[0]
    let mutable minIdx = 0
    for i = 1 to xs.LastIndex do
        let v = projection xs.[i]
        if v < minVal then
            minIdx <- i
            minVal <- v
    minIdx
```

**Accumulating with previous value:**

```fsharp
let computeDeltas (pts: ResizeArray<Pt>) =
    let deltas = ResizeArray<float>(pts.Count - 1)
    let mutable prev = pts.[0]
    for i = 1 to pts.LastIndex do
        let curr = pts.[i]
        deltas.Add(distance prev curr)
        prev <- curr
    deltas
```

---

## 3. INLINE FUNCTIONS

### When to Use
- Small numeric operations (< 5-10 IL instructions)
- Generic math that benefits from specialization
- Operators on value types

### Patterns

**Inline operators:**

```fsharp
static member inline ( + ) (a: Vec, b: Vec) =
    Vec(a.X + b.X, a.Y + b.Y, a.Z + b.Z)

static member inline ( * ) (v: Vec, f: float) =
    Vec(v.X * f, v.Y * f, v.Z * f)
```

**Inline member accessors:**

```fsharp
member inline v.LengthSq =
    v.X * v.X + v.Y * v.Y + v.Z * v.Z

member inline v.Length =
    sqrt v.LengthSq
```

**Inline tolerance checks:**

```fsharp
let inline isTooSmall x = not (x > 1e-6)
let inline isTooSmallSq x = not (x > 1e-12)
let inline isOne x = 0.999999 < x && x < 1.000001
```

---

## 4. SQUARED VALUES OVER SQRT

### Why
- `sqrt` is expensive (10-20x slower than multiply)
- Many comparisons don't need actual distance, just relative ordering

### Patterns

**Distance comparison without sqrt:**

```fsharp
// BAD: Computing sqrt unnecessarily
if point.DistanceTo(target) < threshold then ...

// GOOD: Compare squared values
if point.DistanceToSq(target) < threshold * threshold then ...
```

**Provide both versions:**

```fsharp
member v.LengthSq = v.X * v.X + v.Y * v.Y
member v.Length = sqrt v.LengthSq

member p.DistanceToSq(other: Pt) =
    let dx = p.X - other.X
    let dy = p.Y - other.Y
    dx * dx + dy * dy

member p.DistanceTo(other: Pt) =
    sqrt (p.DistanceToSq other)
```

---

## 5. DISCRIMINATED UNIONS and Enums FOR CORRECTNESS

### Algorithm Options as
Enums just compiled to integers in Fable, while DU create objects.
However Enums don't offer exhausting pattern matching checks.

In general, prefer DUs for options and states unless performance in JS is critical.

```fsharp
type UTurnBehavior =
    | Fail = 1        // Throw on U-turn
    | Chamfer = 2     // Add chamfer points
    | Skip = 3        // Ignore U-turn points
    | Threshold = 4   // Use threshold angle

type ParallelHandling =
    | Fail = 1
    | Skip = 2
    | Proportional = 3
    | Project = 4
```

### Pattern Match

Ensure all cases are handled explicitly.
The compiler will warn if any case is missing only for DUs, not Enums.

```fsharp
let handleCase behavior value =
    match behavior with
    | UTurnBehavior.Fail ->
        failwith $"U-turn at {value}"
    | UTurnBehavior.Chamfer ->
        createChamfer value
    | UTurnBehavior.Skip ->
        ()  // Skip silently
    | UTurnBehavior.Threshold ->
        useThreshold value
    | _ ->
        failwith $"Unknown behavior: {behavior}"
```

### Result Types for Recoverable Errors

```fsharp
type IntersectionResult =
    | Point of Pt
    | Parallel
    | Coincident
    | NoIntersection

let intersectLines line1 line2 : IntersectionResult =
    let det = crossProduct line1.Dir line2.Dir
    if isTooSmall (abs det) then
        if areCoincident line1 line2 then Coincident
        else Parallel
    else
        Point (computeIntersection line1 line2 det)
```

---

## 6. IMMUTABLE STRUCTS WITH EXTENSION MODULES

### Core Struct Pattern

```fsharp
// Core type - minimal, inline-friendly
[<Struct; NoEquality; NoComparison>]
[<IsReadOnly>]
[<DataContract>]
type Pt =
    [<DataMember>] val X: float
    [<DataMember>] val Y: float

    new(x, y) = { X = x; Y = y }

    // Only operators here
    static member inline (+) (p: Pt, v: Vc) = Pt(p.X + v.X, p.Y + v.Y)
    static member inline (-) (a: Pt, b: Pt) = Vc(a.X - b.X, a.Y - b.Y)
```

### Extension Module Pattern

```fsharp
// Separate file: TypeExtensions/Pt.fs
[<AutoOpen>]
module AutoOpenPt =
    type Pt with
        member inline p.IsValid =
            not (Double.IsNaN p.X || Double.IsNaN p.Y)

        member inline p.DistanceTo(b: Pt) =
            let dx = p.X - b.X
            let dy = p.Y - b.Y
            sqrt(dx * dx + dy * dy)

        member inline p.WithX(x) = Pt(x, p.Y)
        member inline p.WithY(y) = Pt(p.X, y)
```

### Why This Pattern
- Core struct stays small (better inlining)
- `[<IsReadOnly>]` enables pass-by-reference optimization
- `[<NoEquality; NoComparison>]` prevents buggy auto-generated equality for floats
- Extension modules add rich API without bloating core type

---

## 7. ERROR HANDLING

### Domain-Specific Exceptions

```fsharp
module Errors =
    type DivByZeroException(msg: string) = inherit Exception(msg)
    type TooSmallException(msg: string) = inherit Exception(msg)
    type NanInfinityException(msg: string) = inherit Exception(msg)

    let failDivide msg divisor obj =
        raise <| DivByZeroException $"{msg}: cannot divide {obj} by {divisor}"

    let failTooSmall msg obj =
        raise <| TooSmallException $"{msg}: {obj} is too small"

    let failNaN msg x y =
        raise <| NanInfinityException $"NaN/Infinity in {msg}: X={x}, Y={y}"
```

### Fail-Fast with Context

```fsharp
// Division with zero check
static member (/) (v: Vec, f: float) =
    if isTooSmall (abs f) then
        failDivide "Vec.(/)" f v
    Vec(v.X / f, v.Y / f, v.Z / f)

// Constructor validation (DEBUG only for performance)
new(x, y) =
    #if DEBUG
    if Double.IsNaN x || Double.IsNaN y then
        failNaN "Pt()" x y
    #endif
    { X = x; Y = y }
```

### Checked vs Unchecked Constructors

```fsharp
type UnitVec =
    val X: float
    val Y: float
    val Z: float

    // Safe public constructor
    static member create(x, y, z) =
        let len = sqrt(x*x + y*y + z*z)
        if isTooSmall len then failTooSmall "UnitVec.create" (x, y, z)
        UnitVec.createUnchecked(x/len, y/len, z/len)

    // Fast internal constructor (mark Obsolete to prevent misuse)
    [<Obsolete("Unchecked - use create() instead")>]
    static member inline createUnchecked(x, y, z) =
        { X = x; Y = y; Z = z }
```

---

## 8. UNITS OF MEASURE FOR TYPE SAFETY

Precomputed constants and thresholds can be modeled with units of measure to prevent mixing raw floats with domain-specific values.

### Angle Measures

Avoid computing cosine or tangent values at runtime by predefining common thresholds.

```fsharp
[<Measure>] type cosine
[<Measure>] type tangent


module Cosine =
    [<Literal>] let ``0.0`` = 1.0<cosine>
    [<Literal>] let ``45.0`` = 0.7071067811865476<cosine>
    [<Literal>] let ``90.0`` = 0.0<cosine>
    [<Literal>] let ``175.0`` = -0.9961946980917455<cosine>

// Usage: Prevents mixing raw floats with angle thresholds
let isNearlyParallel (dot: float) (threshold: float<cosine>) =
    abs dot > float threshold
```

### Lifting Raw Values

```fsharp
let inline withMeasure<[<Measure>] 'M> (x: float) : float<'M> =
    LanguagePrimitives.FloatWithMeasure x

// Or define an operator
let inline (!^) x = withMeasure<cosine> x

// Usage
if dotProduct > !^ 0.999 then ...
```

---

## 9. CONDITIONAL COMPILATION

### Debug-Only Validation

```fsharp
new(x, y, z) =
    #if DEBUG || CHECK_EUCLID
    if Double.IsNaN x || Double.IsNaN y || Double.IsNaN z then
        failNaN "Vec()" x y z
    #endif
    { X = x; Y = y; Z = z }
```

### Fable Compatibility

```fsharp
let formatFloat (x: float) =
    #if FABLE_COMPILER
    x?toFixed(3)
    #else
    x.ToString("0.000", CultureInfo.InvariantCulture)
    #endif
```

---

## 10. NAMING CONVENTIONS

### Type Naming
- **PascalCase** for types: `Pt`, `Vec`, `UnitVec`, `Polyline2D`
- **2D/3D symmetry**: `Pt`/`Pnt`, `Vc`/`Vec`, `UnitVc`/`UnitVec`
- **Suffix for dimension**: `Line2D`, `Line3D`, `BBox` (2D), `Box` (3D)

### Function/Variable Naming
- **camelCase** for functions: `isTooSmall`, `failDivide`, `createUnchecked`
- **Double backticks** for special values: `` `1.0 + 1e-6` ``, `` Cosine.`45.0` ``

### Module Naming
- **PascalCase**: `UtilEuclid`, `EuclidErrors`, `ResizeArr`
- **AutoOpen** for extensions: `AutoOpenPt`, `AutoOpenVec`

---

## Quick Reference Checklist

- [ ] Using `ResizeArray<_>` instead of F# `list` for collections?
- [ ] Pre-allocating ResizeArray with known capacity?
- [ ] Using index loops (`for i = 0 to xs.LastIndex do`) instead of `for-in` to avoid enumerator allocation?
- [ ] Using `mutable` for loop variables in hot paths?
- [ ] Marking small numeric operations `inline`?
- [ ] Using `LengthSq` instead of `Length` where possible?
- [ ] Modeling options/variants as Discriminated Unions?
- [ ] Keeping core structs minimal with `[<IsReadOnly>]`?
- [ ] Adding rich functionality via extension modules?
- [ ] Failing fast with detailed context in errors?
- [ ] Using units of measure for type safety where appropriate?
