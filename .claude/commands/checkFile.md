---
description: Check that docstrings match implementation in a given file
allowed-tools: read_file, replace_string_in_file, multi_replace_string_in_file, semantic_search, grep_search, list_code_usages
arguments:
  - name: file
    description: The file to check (relative or absolute path)
    required: true
---

# Check Docstrings and Implementation for File $file

Perform a comprehensive docstring validation for `$file`:
If `$file` is `*`, check all source files in current project.

## 1. Docstring Quality Check

For **every public member** (functions, types, methods, properties):

- Verify the docstring clearly describes the purpose, parameters, and return value
- Check that parameter names in docstrings match actual parameter names
- Ensure return value descriptions match the actual return type
- Confirm edge cases and error conditions are documented
- Validate that examples (if present) are accurate
- Check spelling and grammar for clarity.

## 2. Implementation Verification

For each member:

- Verify the implementation matches the docstring's description
- Check that all documented parameters are actually used
- Ensure all error conditions mentioned in docstrings are actually thrown
- Validate edge case handling:
  - Zero-length vectors
  - Very short vectors (near tolerance thresholds)
  - Parallel or colinear cases
  - Almost parallel cases (within angle tolerance)
  - Empty collections
  - Single-element collections
  - Duplicate elements
  - Division by zero checks
  - NaN/Infinity handling where applicable

## 3. Code Quality Review

- Verify proper use of `EuclidErrors` module for all error handling
- Check division operations have zero-denominator guards
- Confirm tolerance checks use appropriate functions from `UtilEuclid.fs` (`isTooSmall`, `isTooSmallSq`, etc.)
- Validate angle comparisons use `Cosine` or `Tangent` measure type correctly
- Ensure naming conventions follow project standards (PascalCase for types, camelCase for functions)
- Check whitespace/indentation is consistent (4 spaces)

## 4. Report Findings

For each issue found:

- Quote the relevant docstring
- Explain the discrepancy or ambiguity
- Suggest a specific correction with exact wording

If suggesting code changes:

- Provide the exact fix
- Explain why it's needed
- Note any breaking changes or behavioral differences

## 5. Fix Issues (if appropriate)

If discrepancies are minor and unambiguous to fix, apply corrections directly using the replace tools. Otherwise, present findings and await user approval before making changes.

Follow the coding standards in `.github/copilot-instructions.md` throughout the review.
