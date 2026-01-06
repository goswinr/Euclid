---
description: Add or create tests to verify code functionality for a specific source file
allowed-tools: read_file, replace_string_in_file, multi_replace_string_in_file, create_file, semantic_search, grep_search, file_search, runTests, run_in_terminal, manage_todo_list
arguments:
  - name: file
    description: The source file in Src/ to check test coverage for (e.g., Pnt.fs, Vec.fs, Offset2D.fs)
    required: true
---

# Check Tests for Source File $file

Add or create tests to verify code functionality for `$file`:
If `$file` is `*`, check all source files in current project.

## Workflow

1. **Read the source file** (`Src/$file`) to understand all public functions, types, and extension members
2. **Find the corresponding test file** in `Tests/` (e.g., `Pnt.fs` → `TestVectors.fs`, `Offset2D.fs` → `TestOffset2D.fs`)
   - Use `file_search` and `grep_search` to locate test files that reference the source file's types or functions
   - Check `Tests/TestMain.fs` to understand the test structure
3. **Analyze existing test coverage**:
   - Read the test file and identify which functions/members are already tested
   - Note any missing coverage for public APIs
4. **Add missing tests** systematically:
   - For each untested function, create comprehensive test cases
   - Follow the existing test structure (Expecto/Fable.Mocha compatible)
   - Use `manage_todo_list` to track test creation progress
5. **Run tests** after adding new ones:
   - Execute: `dotnet run --project ./Tests/Euclid.Tests.fsproj` for .NET
   - Execute: `npm run test --prefix ./Tests` for Node.js
6. **Fix any failures**:
   - If tests fail, first check if the implementation in `Src/` has a bug
   - Only update tests if they incorrectly validate the documented behavior
   - Report ambiguities if unclear

## Testing Standards

Follow the coding standards in `.github/copilot-instructions.md` when writing tests.

### Test Structure

- Use Expecto's `testList` and `testCase` functions
- Group related tests logically (e.g., "constructors", "operations", "edge cases")
- Name tests descriptively: `"functionName with specific condition"`
- Use `Expect.equal`, `Expect.isTrue`, `Expect.throws` for assertions
- For geometry, test exact expected output when possible

### Critical Edge Cases (Always Test)

- **Vectors**: zero length, very short (< 1e-12), parallel, almost parallel, perpendicular
- **Lines**: coincident, parallel, skew, intersecting, degenerate (zero length)
- **Angles**: 0°, 90°, 180°, 360°, negative angles
- **Collections**: empty, single element, duplicates, all identical elements
- **Numeric**: NaN, Infinity, very large values, very small values, negative zero
- **Tolerance**: values just inside/outside tolerance thresholds
- **Division**: zero denominators (should throw via `EuclidErrors`)
- **Matrices/Transformations**: identity, singular, near-singular, orthogonal
- **Planes**: degenerate (collinear points), parallel, perpendicular
- **2D/3D symmetry**: test equivalent operations in both dimensions where applicable

### F# Test Code Style

- Use 4 spaces indentation
- Follow PascalCase for test list names, camelCase for variables
- Use `let` bindings for test setup, avoid mutation unless necessary
- Prefer pattern matching over conditionals
- Use explicit type annotations when it improves clarity
- Format floats consistently: `1.0`, `0.0`, `-1.0` (always include decimal point)

### Numeric Tolerance Testing

- Use `UtilEuclid.isTooTiny`, `isTooSmall`, etc. for tolerance checks
- Test values at boundary: `threshold - epsilon`, `threshold`, `threshold + epsilon`
- For squared comparisons, use `isTooSmallSq` to avoid redundant sqrt calculations

### Error Testing

- Verify that invalid inputs throw exceptions via `EuclidErrors` module
- Use `Expect.throws` or `Expect.throwsT` to validate error conditions
- Test error messages are informative (include context: indices, values, thresholds)

## Example Test Pattern

```fsharp
testList "FunctionName" [
    testCase "basic functionality" <| fun _ ->
        let result = FunctionName input
        "should compute correct output" |> Expect.equal result expected

    testCase "zero length vector" <| fun _ ->
        let zero = Vec.Zero
        "should fail on zero vector" |> Expect.throws (fun () -> FunctionName zero |> ignore)

    testCase "parallel vectors" <| fun _ ->
        let v1 = Vec(1., 0., 0.)
        let v2 = Vec(2., 0., 0.)
        let result = FunctionName v1 v2
        "should detect parallel" |> Expect.isTrue (UtilEuclid.isTooSmall result)
]
```

## Notes

- Check if the source file has a corresponding `TypeExtensions/` file with additional members
- For 2D types (`Pt`, `Vc`, `Line2D`), ensure 3D counterparts (`Pnt`, `Vec`, `Line3D`) have symmetric tests
- Run both .NET and Node.js tests to verify cross-platform compatibility
- Keep tests focused: one logical assertion per test case when possible
