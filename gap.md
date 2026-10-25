# API gap checklist

Completed checklist based on the original **77 candidate rows** in [ApiTable.md](Scripts/ApiTable.md). Counts in the headings describe that original snapshot. All uncommented selections are implemented or mapped; H1 was stale. Entries inside `<!-- ... -->` are intentional differences and have not been added. The regenerated API table has **0 unexplained gaps**.

**Add** = direct counterpart; **Choose** = semantics/signature need a decision; **Map** = existing equivalent or intentional dimensional difference. New names follow AGENTS.md; legacy names may need explicit pairing in `ApiTable.fsx`.

## Vc / Vec and UnitVc / UnitVec — 8 gaps

<!-- - [ ] **V1 · Choose · 4:** `Vec.angleToXDegreesInXY`, `Vec.angleToXRadiansInXY`, and the same on `UnitVec`. Match the signed XY angle of `angleToX180` / `angleToXPi`; define failure for a zero XY projection, including vertical unit vectors. Alternatively mark the existing operations as planar only. -->
- [x] **V2 · Add · 2:** `Vc.isParallelAndOrientedToUnitVc minCosine other vector` and `Vc.isPerpendicularToUnitVc maxCosine other vector` — mirror `Vec`, including zero-length handling.
- [x] **V3 · Add · 2:** `UnitVec.sine` / `tangent` — unsigned 3D angles; tangent is negative for obtuse angles and returns Infinity at perpendicularity. Both dimensions document their sign conventions.
## Rotation2D / Quaternion — 14 gaps

- [x] **R1 · Add · 1:** `Quaternion.Half : Quaternion` — shortest-rotation half; preserve the encoded axis at exactly 180 degrees and handle both identity representations. `half` static wrappers on both types as extra API beyond these gaps.
<!-- - [ ] **R2 · Add · 2:** `Rotation2D.Normalize()` / `normalize rotation` — renormalize sine/cosine; reject a zero or invalid pair. -->
<!-- - [ ] **R3 · Add · 4:** `Rotation2D.WithAngleInDegrees` / `withAngleInDegrees`, `WithAngleInRadians` / `withAngleInRadians` — replace the angle, matching the quaternion names; no axis ambiguity in 2D. -->
- [x] **R4 · Map · 1:** `Rotation2D.equalsRotationWithin tolerance a b` — is not applicable in 2d
- [x] **R5 · Add · 1:** `Rotation2D.identity : Rotation2D` — sine 0, cosine 1.
<!-- - [ ] **R6 · Add · 1:** `Rotation2D.multiply(l, r)` — match quaternion multiplication: apply `r`, then `l`; 2D rotations commute, but document the convention. -->
<!-- - [ ] **R7 · Add · 4:** `Rotation2D.( *** )` overloads for `Rotation2D * Rotation2D`, `Vc * Rotation2D`, `UnitVc * Rotation2D`, `Pt * Rotation2D` — rotation and application about the origin. -->

## XLineXY / XLineXYZ and XLine2D / XLine3D — 6 gaps

- [x] **L1 · Add · 3:** `XLineXYZ.closestParameters(...)`, `XLine3D.closestParameters lineA lineB`, `XLine3D.closestParametersFromPntsAndVecs pA pB vA vB` → `struct(float * float)`. Closest **finite-segment** parameters, clamped to [0,1], including parallel and zero-length segments; preserve the 2D tie-breaking policy rather than using the classifiers' overlap midpoint.
- [x] **L2 · Add · 3:** `XLineXYZ.isWithinRanges(...)`, `XLine3D.isWithinRanges(...)`, `XLine3D.isWithinRangesFromPntsAndVecs(...)`. with a skew distance tolerance.

## Rect2D / Rect3D — 13 gaps

- [x] **D1 · Map · 4:** Pair `Rect2D.PointsXY_CCW` / `pointsXY_CCW` and `PointsXYLooped_CCW` / `pointsXYLooped_CCW` with the existing 3D XYZ members.
<!-- - [ ] **D2 · Choose · 2:** `Rect3D.tryClipLine` / `clipLine` — choose coplanar segment clipping, intersection with the planar rectangle, or projection into its plane. The first preserves the 2D clipped-segment meaning but needs a coplanarity tolerance. -->
- [x] **D3 · Map · 6:** Counterparts of `createFromVc`, `createFromUnitVc`, `createCenteredFromVc`, `createCenteredFromUnitVc`, `createFromLine`, `createFromVcAndPts`.do not exist in 3d, they would need a normal direction.
- [x] **D4 · Add · 1:** `Rect3D.fitToPointsXYZ xyzs refRect` — interleaved-coordinate counterpart of `Rect2D.fitToPointsXY`, retaining the reference frame and the projection behavior of `Rect3D.fitToPoints`.

## BRect / BBox — 11 gaps

- [x] **B1 · Add · 1:** `BRect.asRect2D rect : Rect2D` — wrapper for existing `.AsRect2D`, paired with `BBox.asBox`.
- [x] **B2 · Add · 6:** `BRect.GetPoint` / `getPoint`, `GetEdge` / `getEdge`, `GetLine` / `getLine` — mirror `BBox`; use existing corner/edge order, check indices, reject equal endpoints for `GetLine`.
<!-- - [ ] **B3 · Choose · 2:** `BBox.PointsLooped` / `pointsLooped` — a box has no single perimeter. Prefer marking the gap intentional; existing `BottomPointsLooped` / `TopPointsLooped` cover faces. A traversal of all box vertices needs an explicit order and meaning. -->
<!-- - [ ] **B4 · Map · 1:** `BBox.createFromBox` has no additional 2D counterpart to add: `BRect.createFromRect2D` already exists and is paired with `BBox.createFromRect3D`. Mark the box conversion as 3D only, or show the existing 2D factory in both conceptual rows. -->
- [x] **B5 · Add · 1:** `BBox.createXYZ(ax, ay, az, bx, by, bz)` — scalar counterpart of `BRect.createXY`, with the same bounds normalization.

## Tria2D / Tria3D — 10 gaps

<!-- - [ ] **T1 · Choose · 2:** `Tria3D.tryOffsetPntVariableByNormals` / `offsetPntVariableByNormals` — extend the 2D two-normal construction to a specified local plane. Decide whether normals alone determine the desired solution or an explicit plane normal is required; handle parallel/opposite normals. -->
- [x] **T2 · Map · 8:** Keep `Tria3D.[try]intersectLine`, `[try]intersectRay`, `[try]intersectRayXYZ`, `[try]intersectTriaVectorsWithRay` as 3D only. A line can cut a 2D triangle in a segment, so a single-point/parameter counterpart is not equivalent. A separate 2D clipping API could be proposed later.

## Offset2D / Offset3D, including VarDistParallel — 9 gaps

- [x] **O1 · Add · 1:** `Offset3D.toStringXYZs xyzs : string` — diagnostic formatter matching `Offset2D.toStringXYs`.
- [x] **O2 · Add · 1:** `Offset2D.openTolerance : float` — expose the existing unsquared 1e-6 tolerance, consistent with `openToleranceSq`.
<!-- - [ ] **O3 · Choose · 2:** `Offset3D.offsetCustom(...)` / `offsetVariableCustom(...)` — expose collinearity/U-turn thresholds and variable-distance behavior; decide whether to port the 2D U-turn policies or retain 3D's fail behavior. Explicit reference normals would use `offsetByNormalCustom` / existing `offsetVariableByNormalCustom`. Full 2D policy parity requires more than wrappers. -->
<!-- - [ ] **O4 · Choose · 1:** `Offset3D.VarDistParallel.StepWithTwoPoints = 5` — implement the actual two-point step for unequal collinear offsets, including perpendicular distances; adding an enum case alone is insufficient. -->
- [x] **O5 · Map · 4:** Do not add mechanical counterparts for `Offset3D.getCollinearNeighbors`, `getOffsetDirections`, `setOffCornerVar`, or `Offset2D.makeOffsetDirections`. These expose different internal representations; `Offset2D.setOffCornerVar3` is internal. Document these as algorithm-specific, or separately choose a shared kernel design.

## Polyline2D / Polyline3D — 4 gaps

- [x] **P1 · Add · 1:** `Polyline2D(points: seq<Pt>)` constructor — mirror the 3D constructor; copy coordinates into the mutable buffer.
- [x] **P2 · Map · 1:** Pair `Polyline2D.createFromRectCCW` with `Polyline3D.createFromRect3D`; both produce the same local closed corner order.
- [x] **P3 · Add · 2:** `Polyline3D.tryFindSelfIntersection` and `tryFindSelfIntersectionCustom`, with optional skew-distance tolerance on the latter. Mirror XLine intersection exclusions, and add throwing counterparts (including `Polyline2D.findSelfIntersection`).

## Topology2D / Topology3D — 2 gaps

- [x] **H1 · Stale:** The cached sort methods are absent in both dimensions in this checkout. Keep the existing topology API unchanged, as agreed.

Implemented additions have XML documentation and edge-case tests. Pairing and exclusion rules live in `Scripts/ApiTable.fsx`; all three API tables were regenerated. Mapping-only choices added no geometry members. As separately agreed, `Box.offset` now rejects degenerate inputs and results while unchecked construction remains available.
