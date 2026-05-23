// Auto-generated test data from Offset3D tests
// Generated: 1/11/2026 9:38:51 PM

#r "nuget: Euclid"
open Euclid

/// Record type for Offset3D test case data
type Offset3DTestCase = {
    Name: string
    Input: Polyline3D
    RefNormal: UnitVec
    InPlane: float
    Perp: float
    Output: Polyline3D
}

/// All Offset3D test cases for visualization
let offset3DTestCases = [|
    { Name = "open L-shape with perpendicular offset"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 0
      Perp = 2
      Output = Polyline3D.create [| Pnt(0, 0, 2); Pnt(10, 0, 2); Pnt(10, 10, 2) |] }

    { Name = "closed square outward"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0); Pnt(0, 10, 0); Pnt(0, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = -1
      Perp = 0
      Output = Polyline3D.create [| Pnt(-1, -1, 0); Pnt(11, -1, 0); Pnt(11, 11, 0); Pnt(-1, 11, 0); Pnt(-1, -1, 0) |] }

    { Name = "closed triangle"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(5, 8.66, 0); Pnt(0, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(1.7320762121916802, 1, 0); Pnt(8.26792378780832, 1, 0); Pnt(5, 6.66004400048401, 0); Pnt(1.7320762121916802, 1, 0) |] }

    { Name = "open L-shape with loop=true"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(2.4142135623730945, 1, 0); Pnt(9, 1, 0); Pnt(9, 7.585786437626906, 0) |] }

    { Name = "combined in-plane and perpendicular"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 2
      Perp = 3
      Output = Polyline3D.create [| Pnt(0, 2, 3); Pnt(10, 2, 3) |] }

    { Name = "zero distances returns original"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 0
      Perp = 0
      Output = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0) |] }

    { Name = "closed square inward"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0); Pnt(0, 10, 0); Pnt(0, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(1, 1, 0); Pnt(9, 1, 0); Pnt(9, 9, 0); Pnt(1, 9, 0); Pnt(1, 1, 0) |] }

    { Name = "L-shape in XZ plane"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 0, 10) |]
      RefNormal = UnitVec.create(0, 1, 0)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(0, 0, -1); Pnt(11, 0, -1); Pnt(11, 0, 10) |] }

    { Name = "open straight line (colinear)"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(5, 0, 0); Pnt(10, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(0, 1, 0); Pnt(5, 1, 0); Pnt(10, 1, 0) |] }

    { Name = "minimum 2 points"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(0, 1, 0); Pnt(10, 1, 0) |] }

    { Name = "open L-shape in XY plane"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(0, 1, 0); Pnt(9, 1, 0); Pnt(9, 10, 0) |] }

    { Name = "negative perpendicular offset"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 0
      Perp = -2
      Output = Polyline3D.create [| Pnt(0, 0, -2); Pnt(10, 0, -2) |] }

    { Name = "hexagon"
      Input = Polyline3D.create [| Pnt(10, 0, 0); Pnt(5.000000000000001, 8.660254037844386, 0); Pnt(-4.999999999999998, 8.660254037844387, 0); Pnt(-10, 1.2246467991473533E-15, 0); Pnt(-5.000000000000004, -8.660254037844386, 0); Pnt(5.000000000000001, -8.660254037844386, 0); Pnt(10, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1.5
      Perp = 0
      Output = Polyline3D.create [| Pnt(8.267949192431123, 0, 0); Pnt(4.133974596215562, 7.1602540378443855, 0); Pnt(-4.13397459621556, 7.160254037844387, 0); Pnt(-8.267949192431123, 6.695352868347751E-16, 0); Pnt(-4.133974596215565, -7.1602540378443855, 0); Pnt(4.1339745962155625, -7.1602540378443855, 0); Pnt(8.267949192431123, 0, 0) |] }

    { Name = "square with colinear point"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(5, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0); Pnt(0, 10, 0); Pnt(0, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(1, 1, 0); Pnt(5, 1, 0); Pnt(9, 1, 0); Pnt(9, 9, 0); Pnt(1, 9, 0); Pnt(1, 1, 0) |] }

    { Name = "open line with multiple colinear points"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(5, 0, 0); Pnt(10, 0, 0); Pnt(15, 0, 0); Pnt(20, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 2
      Perp = 0
      Output = Polyline3D.create [| Pnt(0, 2, 0); Pnt(5, 2, 0); Pnt(10, 2, 0); Pnt(15, 2, 0); Pnt(20, 2, 0) |] }

    { Name = "tilted plane triangle"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 10); Pnt(5, 8.66, 5); Pnt(0, 0, 0) |]
      RefNormal = UnitVec.create(0.7071067811865475, 0, -0.7071067811865475)
      InPlane = 0.5
      Perp = 0
      Output = Polyline3D.create [| Pnt(-0.7451244231512386, -0.49999999999999994, -0.7451244231512386); Pnt(10.745124423151239, -0.49999999999999994, 10.745124423151239); Pnt(5, 9.450555500897945, 5); Pnt(-0.7451244231512386, -0.49999999999999994, -0.7451244231512386) |] }

    { Name = "YZ plane square"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(0, 10, 0); Pnt(0, 10, 10); Pnt(0, 0, 10); Pnt(0, 0, 0) |]
      RefNormal = UnitVec.create(1, 0, 0)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(0, 1, 1); Pnt(0, 9, 1); Pnt(0, 9, 9); Pnt(0, 1, 9); Pnt(0, 1, 1) |] }

    { Name = "open L with loop=true"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(2.4142135623730945, 1, 0); Pnt(9, 1, 0); Pnt(9, 7.585786437626906, 0) |] }

    { Name = "open L with loop=false"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(0, 1, 0); Pnt(9, 1, 0); Pnt(9, 10, 0) |] }

    { Name = "3D spiral staircase"
      Input = Polyline3D.create [| Pnt(10, 0, 0); Pnt(0, 10, 5); Pnt(-10, 0, 10); Pnt(0, -10, 15); Pnt(10, 0, 20) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(9.403715206000056, -0.7453559924999299, 0.29814239699997197); Pnt(0, 8.658359213500127, 5); Pnt(-8.658359213500127, 0, 10); Pnt(0, -8.658359213500127, 15); Pnt(9.403715206000056, 0.7453559924999299, 19.70185760300003) |] }

    { Name = "open 3-point line distance check"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 10, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 3
      Perp = 0
      Output = Polyline3D.create [| Pnt(0, 3, 0); Pnt(7, 3, 0); Pnt(7, 10, 0) |] }

    { Name = "XZ plane square"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(10, 0, 0); Pnt(10, 0, 10); Pnt(0, 0, 10); Pnt(0, 0, 0) |]
      RefNormal = UnitVec.create(0, 1, 0)
      InPlane = 1
      Perp = 0
      Output = Polyline3D.create [| Pnt(-1, 0, -1); Pnt(11, 0, -1); Pnt(11, 0, 11); Pnt(-1, 0, 11); Pnt(-1, 0, -1) |] }

    { Name = "closed rectangle 20x10"
      Input = Polyline3D.create [| Pnt(0, 0, 0); Pnt(20, 0, 0); Pnt(20, 10, 0); Pnt(0, 10, 0); Pnt(0, 0, 0) |]
      RefNormal = UnitVec.create(0, 0, 1)
      InPlane = 2
      Perp = 0
      Output = Polyline3D.create [| Pnt(2, 2, 0); Pnt(18, 2, 0); Pnt(18, 8, 0); Pnt(2, 8, 0); Pnt(2, 2, 0) |] }
|]

// Total test cases: 23

// Example: iterate and visualize all test cases
// for tc in offset3DTestCases do
//     printfn "Test: %s" tc.Name
//     // Draw tc.Input and tc.Output with your visualization tool
