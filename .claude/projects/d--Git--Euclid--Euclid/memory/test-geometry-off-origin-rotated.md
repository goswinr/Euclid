---
name: test-geometry-off-origin-rotated
description: When writing tests for Euclid geometry types, use shapes that are off-origin and rotated
metadata:
  type: feedback
---

When adding tests for geometry types in this repo, the test shapes should NOT sit at the world origin and SHOULD have some rotation. Axis-aligned shapes at (0,0) hide bugs that only show under translation/rotation.

**Why:** The user explicitly asked for this on Rect2D tests; origin-aligned unit shapes mask off-by-one corner/axis and normalization bugs.

**How to apply:** Pick exact-arithmetic rotated axes so expected corner coordinates stay clean. The Pythagorean-style pair X=(8,6) (len 10) and Y=(-3,4) (len 5) from origin (5,3) gives integer corners p0=(5,3) p1=(13,9) p2=(10,13) p3=(2,7), center (7.5,8) — used in `Test/TestRect2D.fs`. Also: if a test fails, suspect the implementation or its docstring before the test (see `.github/copilot-instructions.md`). This caught `Rect2D.createFromXVectorAndWidth`/`createFromCenterAndVector` scaling `sizeY` by the X length instead of treating it as an absolute size.
