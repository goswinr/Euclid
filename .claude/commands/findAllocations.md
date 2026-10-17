---
description: Find and remove intermediate point and vector allocations while keeping results bit-identical
allowed-tools: read_file, replace_string_in_file, multi_replace_string_in_file, grep_search, file_search, list_code_usages, run_in_terminal, create_file
arguments:
  - name: scope
    description: The file to scan (relative or absolute path), or * for the whole library
    required: true
---

# Find avoidable point and vector allocations in $scope

Find places in `$scope` that build a `Pnt`, `Pt`, `Vec`, `Vc`, `UnitVec` or `UnitVc` (or a `Matrix`/`RigidMatrix`) only to read its components, and compute with plain floats instead.
If `$scope` is `*`, scan all of `Src/` and `Src/TypeExtensions/`.

The results must stay **bit-identical**. This is a performance refactor, not a change in behavior.

## Why

These types are structs. On .NET a temporary is a stack copy that the JIT may or may not remove. Compiled with Fable to JavaScript, every one is a heap object.
The flat-storage types (`FreeBox`, `Polyline2D`, `Polyline3D`) and the float-field types (`Line2D`/`Line3D`, `PPlane`, `NPlane`, `Box`, `Rect2D`, `Rect3D`, `BBox`, `BRect`, `Matrix`, `RigidMatrix`) exist so that this work can stay in floats.

## 1. Where it pays off

- **Worth doing:**
  - Loops that run once per point or segment (`Points2D`/`Points3D`, `Polyline*`, `Offset*`, `Topology*`).
  - Distance and closest-point queries.
  - Parallel, perpendicular and angle predicates.
  - Members of the flat-storage types.
- **Skip:**
  - `ToString`, `AsString`, `AsFSharpCode` and JSON converters.
  - Failure branches.
  - Constructors that are called rarely (`PPlane.create*`, `Rect3D.createFrom3Points`, `Matrix.createLookAt`, `createVecToVec`).
  - Code where the struct is the actual result.

## 2. Source patterns to grep for

| Pattern | Replace with |
|---|---|
| `let p = x.Prop` and `p` is only read through `.X` `.Y` `.Z` | read the fields directly |
| `p.Xaxis.X`, `pl.Origin.X`, `pl.Normal.X`, `ln.From.X` | `p.XaxisX`, `pl.OriginX`, `pl.NormalX`, `ln.FromX` (also `ln.VectorX`, `b.MinX`, `m.M11`, `box.Pt0X` ... `box.Pt7Z`) |
| `createUncheckedVec(pl.Origin, pl.Xaxis*x, ...)` | `createUnchecked(pl.OriginX, ..., pl.XaxisX*x, ...)` |
| `(a - b).Length`, `Vec.cross(b-a, c-a)`, `(p-q) *** v` | coordinate differences written out |
| `a * (1.0 / sqrt sa)` followed by `***` | per-component dot product with the same factors |
| `a.Unitized` / `b.Unitized` only to get an angle | `anglePiOfVecs` / `angleHalfPiOfVecs` (see section 4) |
| `sum <- sum + pt`, `minPt <- Pnt(...)` in a loop | mutable float accumulators, build the result once |
| `getPt i xyzs` or `XY.getPt` in a loop | read `xyzs.[i*3]` etc., `GetX`/`GetY`/`GetZ` |
| `Line3D(getPt i xyzs, getPt (i+1) xyzs)` | the six-float `Line3D` constructor |
| A matrix built only to use part of it | compute only the entries you need |

Some useful searches:

```sh
grep -rnE "\.(Origin|Xaxis|Yaxis|Zaxis|Normal|From|To|Vector|Center|Pt[0-7])\.(X|Y|Z)\b" Src
grep -rnE "create(Unchecked)?Vec\(" Src
grep -rnE "\* \(1\.0 / sqrt s[ab]\)" Src
grep -rnE "\)\.(Length|LengthSq|Unitized)\b" Src
```

## 3. Scan the compiled IL (finds what grep can't see)

Most temporaries hide inside `inline` operators and members (`a - b`, `.Unitized`, `pl.Normal`).
The current F# compiler does not inline `inline` members in **Debug** builds. So in a Debug build every hidden temporary shows up as a call, such as `call Pnt.op_Subtraction` or `call AutoOpenVec.Vec.get_Unitized`.

1. Build with `dotnet build Test/Test.fsproj` (Debug).
2. Save the script below to your scratchpad (not into the repo).
3. Run `dotnet fsi ilscan.fsx Test/bin/Debug/net10.0/Euclid.dll > scan.txt`.

```fsharp
// Lists, per method, every construction of a small geometry struct and every call that returns one.
// Usage: dotnet fsi ilscan.fsx <path to Euclid.dll from a Debug build>
open System
open System.Reflection
open System.Reflection.Emit

let structs = set ["Pnt"; "Vec"; "UnitVec"; "Pt"; "Vc"; "UnitVc"]
let asm = Assembly.LoadFrom fsi.CommandLineArgs.[1]

let oneByte = Array.zeroCreate<OpCode> 256
let twoByte = Array.zeroCreate<OpCode> 256
for f in typeof<OpCodes>.GetFields(BindingFlags.Public ||| BindingFlags.Static) do
    let oc = f.GetValue null :?> OpCode
    if oc.Size = 1 then oneByte.[int (uint16 oc.Value)] <- oc else twoByte.[int (uint16 oc.Value &&& 0xFFus)] <- oc

let operandSize (oc:OpCode) (il:byte[]) pos =
    match oc.OperandType with
    | OperandType.InlineNone -> 0
    | OperandType.ShortInlineBrTarget | OperandType.ShortInlineI | OperandType.ShortInlineVar -> 1
    | OperandType.InlineVar -> 2
    | OperandType.InlineI8 | OperandType.InlineR -> 8
    | OperandType.InlineSwitch -> 4 + 4 * BitConverter.ToInt32(il, pos)
    | _ -> 4

let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly
let types = try asm.GetTypes() with :? ReflectionTypeLoadException as e -> e.Types |> Array.filter (isNull >> not)
let rows = ResizeArray()
for t in types do
    let methods = Array.append (t.GetMethods flags |> Array.map (fun m -> m :> MethodBase)) (t.GetConstructors flags |> Array.map (fun c -> c :> MethodBase))
    for m in methods do
        match (try m.GetMethodBody() with _ -> null) with
        | null -> ()
        | body ->
            let il = body.GetILAsByteArray()
            let counts = Collections.Generic.Dictionary<string,int>()
            let add k = counts.[k] <- (match counts.TryGetValue k with | true, c -> c + 1 | _ -> 1)
            let mutable pos = 0
            while pos < il.Length do
                let oc =
                    if il.[pos] = 0xFEuy then pos <- pos + 2; twoByte.[int il.[pos - 1]]
                    else                      pos <- pos + 1; oneByte.[int il.[pos - 1]]
                if oc = OpCodes.Newobj || oc = OpCodes.Call || oc = OpCodes.Callvirt then
                    let target =
                        try m.Module.ResolveMethod(BitConverter.ToInt32(il, pos),
                                                   (if t.IsGenericType then t.GetGenericArguments() else null),
                                                   (if m.IsGenericMethod then m.GetGenericArguments() else null))
                        with _ -> null
                    match target with
                    | null -> ()
                    | c when c.IsConstructor -> if structs.Contains c.DeclaringType.Name then add $"new {c.DeclaringType.Name}"
                    | :? MethodInfo as mi when structs.Contains mi.ReturnType.Name -> add $"call {mi.DeclaringType.Name}.{mi.Name}"
                    | _ -> ()
                pos <- pos + operandSize oc il pos
            if counts.Count > 0 then
                let ret = match m with :? MethodInfo as mi -> mi.ReturnType.Name | _ -> "ctor"
                let detail = counts |> Seq.map (fun kv -> $"{kv.Key}:{kv.Value}") |> String.concat "  "
                rows.Add((Seq.sum counts.Values, $"{t.FullName}.{m.Name}", ret, detail))

for (n, name, ret, detail) in rows |> Seq.sortByDescending (fun (n, _, _, _) -> n) do
    printfn "%3d  %-70s -> %-14s %s" n name ret detail
```

**Reading the output:**

- **Candidates:**
  - A method that returns `Double`, `Boolean`, `Line3D`, `Box` or similar, but still has `call ...op_*`, `get_Unitized`, `cross`, `get_Origin`, `get_Normal`, `get_Xaxis` and the like.
  - A method that returns a struct and has more than one struct-producing call or `new`.
- **Not candidates:**
  - `IList.get_Item`, `List.get_Item` and `FSharpFunc.Invoke`: these are reads from inputs and user callbacks.
  - `new` of the returned value, or of the elements of a returned array or list.
  - `__debug` closures.
- The counts are call sites, not runtime counts. A single site inside a per-point loop matters more than ten sites in a constructor.

## 4. Rewrite rules

**Same arithmetic, same order.** Write out exactly what the replaced operator does. Floating-point multiplication is commutative but not associative. So `(b.X*fb)*(a.X*fa) + ...` is the same as `(b*fb) *** (a*fa)`, but `dot(a,b)*fa*fb` is not.

The operators being replaced:

| Expression | Computes |
|---|---|
| `a - b` (two points) | `a.X - b.X`, ... |
| `p + v` | `p.X + v.X`, ... |
| `v * f`, `f * v` | `v.X * f`, ... |
| `a *** b` | `a.X*b.X + a.Y*b.Y + a.Z*b.Z` |
| `Vec.cross(a, b)` | `a.Y*b.Z - a.Z*b.Y`, `a.Z*b.X - a.X*b.Z`, `a.X*b.Y - a.Y*b.X` |
| `p / f` | `p.X / f` |
| `Vec.Unitized` | `l = sqrt(x*x + y*y + z*z)`, `failUnit3` if tiny, `f = 1. / l`, then `f*x` |
| `Vc.Unitized` | `l = sqrt(x*x + y*y)`, `failTooSmall` if tiny, then `x/l` (division, not multiplication) |

- **Exact simplifications:**
  - Multiplying by 0.5 or another power of two is exact, so it can be folded, for example `size * (factor * 0.5)`.
  - `v *** Vec.Zaxis` is exactly `v.Z` for finite values.
  - The 3×3 part of a translation × rotation × translation product equals the rotation's 3×3 part.
- **Exceptions keep their type:**
  - `EuclidUnitizingException` via `failUnit3`.
  - `EuclidTooSmallException` via `failTooSmall` / `failTooSmall2`.
  - `EuclidDivByZeroException` via `failDivide`.
  - Build a `Vec` or `Vc` for the error message only inside the failure branch.
  - Name the function that was called in the message.
- **Prefer copying the original algorithm over a similar helper.** For example, `XLineXYZ.sqDistLnPt'` treats near-zero-length lines differently from `Line3D.ClosestParameterXYZ`.
- **No tuples** for returning several floats: they allocate on .NET and in Fable. Use mutable locals or write the expressions out.
- **Write `f (-x)`**, not `f -x`.
- **`internal` or `private` inline helpers** may be used inside public `inline` members. The internal `XYZ`/`XY` modules are already used this way.
- **Keep** tolerances, branch structure, evaluation order (it decides which exception is raised first), `&&` short-circuits, and existing comments and TODOs.

Float helpers that already exist (use or extend them, don't duplicate them):

- **`XYZ` / `XY`** (`Src/XYZ.fs`, `Src/XY.fs`): `length`, `sqLength`, `dot`, `dist`, `anglePiOfUnit`, `angleHalfPiOfUnit`.
- **`AutoOpenVec.anglePiOfVecs` / `angleHalfPiOfVecs`** (`Src/TypeExtensions/Vec.fs`): unitize like `Vec.Unitized`, then compute the angle.
- **`AutoOpenVc.anglePiOfVcs` / `angleHalfPiOfVcs`** (`Src/TypeExtensions/Vc.fs`): the same for `Vc`.
- **`XLineXY*` / `XLineXYZ*`** (`Src/XLine2D.fs`, `Src/XLine3D.fs`): `clParamRayPt`, `sqDistLnPt`, `clPtLn` and more.
- **Float overloads** such as `RayClosestParameterXY`, `DistanceToXYZ`, `SqDistanceToXYZ`, `SetPtXYZ`, `AddXYZ`, `EvaluateAtXYZ`.

## 5. Verify

1. Run `dotnet run --project ./Test/Test.fsproj` and `cd Test && npm run test && cd ..` (Fable, Debug and Release, plus the TypeScript check).
2. **Compare the old and new build** for every rewritten function that isn't covered by exact-value tests:
   1. Copy `Test/bin/Debug/net10.0/Euclid.dll` into one scratch folder before the change, and into another after the change and a rebuild.
   2. Run the same script in both folders and `diff` the outputs. They must be identical, exception type names included.

   Use a seeded `System.Random`, plus near-parallel inputs (offsets of about `1e-3`, to reach the `asin` branch of the angle code), anti-parallel inputs, and zero-length inputs.

   ```fsharp
   #r "Euclid.dll"
   open Euclid
   let r (v:float) = v.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
   let rnd = System.Random(42)
   let f () = rnd.NextDouble() * 4.0 - 2.0
   let lines = ResizeArray()
   for i in 0 .. 2000 do
       let a = Vec(f(), f(), f())
       let b = if i % 2 = 0 then a + Vec(f(), f(), f()) * 1e-3 else Vec(f(), f(), f())
       lines.Add $"{r (Vec.anglePi a b)} {r (Vec.angle90 a b)}"   // the functions under test
   let tryIt name fn = try fn () |> ignore; lines.Add $"{name} no exception" with e -> lines.Add $"{name} {e.GetType().Name}"
   tryIt "zero" (fun () -> Vec.anglePi Vec.Zero Vec.Xaxis)
   System.IO.File.WriteAllLines("out.txt", lines)
   ```

3. If a rewritten function has no tests, add tests that pin the exact values of the old build (`Expect.equal` on floats) and its failure cases.
4. When fixing a bug, `git stash` the source change and check that the new test fails without it.

## 6. Bugs you find while reading

Report each one and fix it in its own commit, with a test and an entry under `### Fixed` in `CHANGELOG.md`. Kinds of bug found this way so far:

- A min/max loop started at index 1 while its accumulators started at `Double.MaxValue` / `Double.MinValue`, so the first point was ignored (`Box.createFromPlaneAndPoints`).
- An offset point was moved along the normal instead of the line direction (`Tria3D.offsetVar`). Its tests only checked +d / −d round trips, which the wrong formula passed too.

Report problems in obsolete code instead of fixing them silently. For example, the `Points.offsetInCorner` wrappers pass the wrong arguments to `Tria3D.offsetVar`.

## 7. Commits and report

- Make one commit per group of related functions.
- Allocation-only commits don't get a changelog entry. Their message should say what was built before, that the results are unchanged, and how that was checked.
- Working copies use CRLF while the repo stores LF (`.gitattributes` has `eol=lf`). Scripted edits must not mix line endings. On Windows, Python's `read_text`/`write_text` keeps CRLF. Check with ``grep -c $'\r$' file``.
- End with a table of commits, plus the candidates you skipped and why.

**Known candidates left on purpose (low value):**

- `Box.tryGetBoxBetween`
- the `PPlane.create*` constructors and `Rect3D.createFrom3Points`
- `RigidMatrix.create` and the `createVecToVec` constructors
- `Pnt.offsetTwoPt` and `Line3D.offset`
- the `Tria2D`/`Tria3D` offset helpers
- `Vec.slerp`, `Vc.slerp`, `UnitVec.slerp`, `UnitVc.slerp`
- `Offset3D.getOffsetDirections`, which stores `UnitVec`s in its result records
