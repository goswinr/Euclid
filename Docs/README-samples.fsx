#r "../bin/Release/net6.0/Euclid.dll"

open Euclid
open Euclid.EuclidErrors

// Quick Start
let point1 = Pnt(1.0, 2.0, 3.0)
let point2 = Pnt(4.0, 5.0, 6.0)
let vector = Vec(1.0, 1.0, 0.0)
let distance = point1.DistanceTo point2
let unitVec = vector.Unitized
let matrix =
    Matrix.createShear(3.0, 0, 0, 0, 0, 0)
    ***
    Matrix.createRotationZ 45

let _quickStartResult =
    point1
    |> Pnt.translate vector
    |> Pnt.scale 3.0
    |> Pnt.transform matrix

// Points and Vectors (2D)
let a = Pt(1.0, 2.0)
let b = Pt(4.0, 6.0)
let dist = a.DistanceTo b
let mid = Pt.midPt a b
let v = Vc(3.0, 4.0)
let len = v.Length
let half = v.Half
let perp = v.Rotate90CCW
let moved = a + v
let diff = b - a
let dir = v.Unitized
let angle = v.Direction360
let dot = v.Dot(Vc(1.0, 0.0))
let cross = v.Cross(Vc(1.0, 0.0))

// Points and Vectors (3D)
let p1 = Pnt(1.0, 2.0, 3.0)
let p2 = Pnt(4.0, 6.0, 3.0)
let dist3d = p1.DistanceTo p2
let mid3d = Pnt.midPt p1 p2
let lerp = Pnt.divPt(p1, p2, 0.25)
let v1 = Vec(1.0, 0.0, 0.0)
let v2 = Vec(0.0, 1.0, 0.0)
let normal = v1.Cross v2
let dot3d = v1.Dot v2
let angle3d = Vec.angle180 v1 v2
let dir3d = Vec(3.0, 4.0, 0.0).Unitized
let isPerp = dir3d.IsPerpendicularTo UnitVec.Zaxis
let pt2d = p1.AsPt
let pt3d = Pt(1.0, 2.0).WithZ 5.0

// Lines
let ln2d = Line2D(Pt(0, 0), Pt(10, 0))
let ln3d = Line3D(Pnt(0, 0, 0), Pnt(10, 5, 3))
let ln = Line2D(0.0, 0.0, 10.0, 5.0)
let lenLn = ln2d.Length
let midPt = ln2d.Mid
let dirLn = ln3d.Direction
let tang = ln3d.Tangent
let quarterPt = ln2d.EvaluateAt 0.25
let longer = ln2d.Extend(2.0, 3.0)
let shorter = ln2d.Shrink(1.0, 1.0)
let flipped = ln2d.Reversed
let testPt = Pt(5.0, 3.0)
let closest = ln2d.ClosestPoint testPt
let param = ln2d.ClosestParameter testPt
let other = Line2D(Pt(5, -5), Pt(5, 5))
let isParallel = ln2d.IsParallelTo other
let isPerpLn = ln2d.IsPerpendicularTo other
let hit = Line2D.tryIntersect ln2d other
let offsetLn = Line2D.offset 2.0 ln2d
let movedLn = ln2d.Move(Vc(0.0, 5.0))

// Line Intersections (Discriminated Unions)
let lineA = Line2D(Pt(0, 0), Pt(10, 0))
let lineB = Line2D(Pt(5, -5), Pt(5, 5))

match XLine2D.getIntersection(lineA, lineB) with
| XLine2D.XPt.Intersect pt -> printfn $"Lines cross at {pt}"
| XLine2D.XPt.Apart -> printfn "Segments don't reach each other"
| XLine2D.XPt.Parallel -> printfn "Lines are parallel"
| XLine2D.XPt.TooShortA -> printfn "Line A is too short"
| XLine2D.XPt.TooShortB -> printfn "Line B is too short"
| XLine2D.XPt.TooShortBoth -> printfn "Both lines are too short"

match XLine2D.getRayIntersection(lineA, lineB) with
| XLine2D.XRay.Intersect pt -> printfn $"Rays meet at {pt}"
| XLine2D.XRay.Parallel -> printfn "Rays are parallel"
| _ -> ()

let ln3A = Line3D(Pnt(0, 0, 0), Pnt(10, 0, 0))
let ln3B = Line3D(Pnt(5, 0, 5), Pnt(5, 10, 5))

match XLine3D.getIntersection(ln3A, ln3B) with
| XLine3D.XPnt.Intersect pt -> printfn $"Lines intersect at {pt}"
| XLine3D.XPnt.Skew (ptA, ptB, d) -> printfn $"Closest approach: {d} between {ptA} and {ptB}"
| XLine3D.XPnt.Apart -> printfn "Segments don't reach each other"
| XLine3D.XPnt.Parallel -> printfn "Lines are parallel"
| _ -> ()

// Planes
let npl = NPlane(Pnt(0, 0, 5), UnitVec.Zaxis)
let d = npl.DistanceToPt (Pnt(3, 4, 8))
let d2 = npl.DistanceToPt (Pnt(3, 4, 2))
let proj = npl.ClosestPoint (Pnt(3, 4, 8))
let ang = npl.Angle90ToVec (Vec(1, 0, 0))
let ppl =
    PPlane.createThreePoints
        (Pnt(0, 0, 0))
        (Pnt(10, 0, 0))
        (Pnt(0, 10, 0))

// Rectangles
let rect = Rect2D.createFromXVectorAndWidth(Pt(0, 0), Vc(10, 0), 5.0)
let dirRect = UnitVc.rotate 45.0 UnitVc.Xaxis
let rotRect = Rect2D.createFromDirectionAndSizes(Pt(0, 0), dirRect, 10.0, 5.0)
let area = rect.Area
let cx = rect.SizeX
let cy = rect.SizeY
let center = rect.Center
let c0 = rect.Origin
let c2 = rect.FarCorner
let midPoint = rect.EvaluateAt(0.5, 0.5)

// Bounding Boxes
let points = [ Pnt(0, 0, 0); Pnt(10, 5, 3); Pnt(-2, 8, 1) ]
let bbox = BBox.createFromSeq points
let size = bbox.SizeX, bbox.SizeY, bbox.SizeZ
let minPt = bbox.MinPnt
let maxPt = bbox.MaxPnt
let centerBbox = bbox.Center
let vol = bbox.Volume
let box2 = BBox.createFromCenter(Pnt(0, 0, 0), 10.0, 10.0, 10.0)
let bigger = bbox.Expand 1.0
let inside = bbox.Contains(Pnt(5, 4, 2))
let combined = bbox.Union box2

// Polylines
let pl2d = Polyline2D.createFromPts(ResizeArray [ Pt(0, 0); Pt(10, 0); Pt(10, 5); Pt(0, 5) ])
let lenPolyline = pl2d.Length
let count = pl2d.PointCount
let segs = pl2d.SegmentCount
let firstSeg = pl2d.FirstSegment
let seg = pl2d.GetSegment(1)
pl2d.SetPt (2, Pt(12, 5))
let copy = pl2d.Duplicate()
let pl3d = Polyline3D.createFromPts [ Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 5, 3) ]
let totalLen = pl3d.Length

// Transformations with Matrices
let pt = Pnt(5.0, 0.0, 0.0)
let m1 = Matrix.createTranslation(10.0, 0.0, 0.0)
let m2 = Matrix.createRotationZ 90.0
let m3 = Matrix.createRotationX 45.0
let m4 = Matrix.createRotationAxis(UnitVec.Zaxis, 30.0)
let m5 = Matrix.createRotationAxisCenter(Vec(0, 0, 1), Pnt(5, 5, 0), 45.0)
let m6 = Matrix.createScale(2.0, 2.0, 1.0)
let m7 = Matrix.createShear(0.5, 0, 0, 0, 0, 0)
let combinedTransform = m1 *** m2 *** m6
let pt2 = pt.Transform combinedTransform
let result =
    pt
    |> Pnt.translate (Vec(10, 0, 0))
    |> Pnt.rotateOnZDeg 90.0
    |> Pnt.scale 2.0
    |> Pnt.transform combinedTransform
let fromPlane = PPlane.createThreePoints (Pnt(0,0,0)) (Pnt(1,0,0)) (Pnt(0,1,0))
let toPlane = PPlane.createThreePoints (Pnt(5,5,0)) (Pnt(6,5,0)) (Pnt(5,6,0))
let planeXform = Matrix.createPlaneToPlane(fromPlane, toPlane)
let mirror = Matrix.createMirror fromPlane

let rigid = RigidMatrix.createRotationZ 90.0
let vTransform = Vec(1, 0, 0)
let v2Transform = vTransform.TransformRigid rigid

// Quaternion Rotations
let q1 = Quaternion.createFromDegree(Vec(0, 0, 1), 90.0)
let q2 = Quaternion.createFromRadians(UnitVec.Xaxis, 1.5708)
let q3 = Quaternion.createVecToVec(UnitVec.Xaxis, UnitVec.Yaxis)
let angleQ = q1.AngleInDegrees
let axis = q1.Axis
let inv = q1.Inverse
let q4 = q1 *** q2
let rotated = Pnt.rotate q1 (Pnt(1, 0, 0))
let mat = Matrix.createFromQuaternion q1

// Point Cloud Operations
let cloud = ResizeArray [ Pnt(0,0,0); Pnt(1,0,0); Pnt(5,5,5); Pnt(10,10,10) ]
let nearest = Points3D.closestPoint(cloud, Pnt(1.1, 0.1, 0.0))
let idx = Points3D.closestPointIdx(cloud, Pnt(1.1, 0.1, 0.0))
let setA = ResizeArray [ Pnt(0,0,0); Pnt(10,10,10) ]
let setB = ResizeArray [ Pnt(1,0,0); Pnt(20,20,20) ]
let iA, iB = Points3D.closestPointsIdx(setA, setB)
let farthest = Points3D.mostDistantPoint(cloud, setB)
let centerPoints = Points3D.center cloud

// Error Handling
try
    let bad = Vec(0, 0, 0).Unitized
    ()
with :? EuclidUnitizingException as e ->
    printfn $"Caught: {e.Message}"
