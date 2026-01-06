module TestPolyLabel

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline close (a:Pt) (b:Pt) = Pt.distance a b < 1e-3

let tests =
    testList "PolyLabel" [

        test "Square center" {
            // unit square
            let pts = [| Pt(0,0); Pt(1,0); Pt(1,1); Pt(0,1); Pt(0,0) |]
            let pl = Polyline2D.create pts
            let p,d = pl.FindLablePoint 1e-6
            Expect.isTrue (close p (Pt(0.5,0.5))) $"expected center got {p}"
            Expect.floatClose Accuracy.low d 0.5 "distance = 0.5"
        }

        test "Rectangle center" {
            let pts = [| Pt(0,0); Pt(4,0); Pt(4,2); Pt(0,2); Pt(0,0) |]
            let pl = Polyline2D.create pts
            let p,d = pl.FindLablePoint 1e-3
            Expect.isTrue (close p (Pt(2.0,1.0))) $"expected center got {p}"
            Expect.floatClose Accuracy.low d 1.0 "distance = min half-side"
        }

        test "Right triangle incenter" {
            // Right triangle (0,0)-(4,0)-(0,3)
            let pts = [| Pt(0,0); Pt(4,0); Pt(0,3); Pt(0,0) |]
            let pl = Polyline2D.create pts
            let p,d = pl.FindLablePoint 1e-4
            // Incenter coordinates (a,b,c sides opposite angles) => ( (ax1 + bx2 + cx3)/(a+b+c), ... ) here x1=0,y1=0,x2=4,y2=0,x3=0,y3=3
            // sides: a= length BC between (4,0)-(0,3)=5, b= length AC between (0,0)-(0,3)=3, c= length AB between (0,0)-(4,0)=4
            let per = 5.+3.+4.
            let expected = Pt( (5.*0. + 3.*4. + 4.*0.)/per, (5.*0. + 3.*0. + 4.*3.)/per ) // (12/12,12/12)=(1,1)
            Expect.isTrue (close p expected) $"expected incenter {expected} got {p}"
            Expect.floatClose Accuracy.low d 1.0 "inradius for 3-4-5 triangle is 1"
        }

        test "Precision refinement" {
            let pts = [| Pt(0,0); Pt(2,0); Pt(2,2); Pt(0,2); Pt(0,0) |]
            let pl = Polyline2D.create pts
            let p1,d1 = pl.FindLablePoint 1e-1
            let p2,d2 = pl.FindLablePoint 1e-4
            // high precision should not worsen distance
            Expect.isTrue (d2 >= d1 - 1e-6) "distance monotonic"
            Expect.isTrue (Pt.distance p2 (Pt(1,1)) < Pt.distance p1 (Pt(1,1)) + 1e-2) "higher precision closer to center or similar"
        }

        // Additional special shapes
        test "Equilateral triangle incenter" {
            // Side length 2, points at (0,0), (2,0), (1,√3)
            let h = sqrt 3.0
            let pts = [| Pt(0,0); Pt(2,0); Pt(1,h); Pt(0,0) |]
            let pl = Polyline2D.create pts
            let p,d = pl.FindLablePoint 1e-4
            // Incenter = centroid for equilateral triangle: (1, h/3*2?) Actually centroid/incenter at (1, h/3)
            let expected = Pt(1.0, h/3.0)
            Expect.isTrue (Pt.distance p expected < 1e-3) $"expected {expected} got {p}"
            // Inradius = side * sqrt(3)/6 = 2*sqrt3/6 = sqrt3/3 = h/3
            Expect.floatClose Accuracy.low d (h/3.0) "inradius eq tri"
        }

        test "Thin rectangle center" {
            // 10 x 0.2
            let pts = [| Pt(0,0); Pt(10,0); Pt(10,0.2); Pt(0,0.2); Pt(0,0) |]
            let pl = Polyline2D.create pts
            let p,d = pl.FindLablePoint 1e-3
            Expect.isTrue (Pt.distance p (Pt(5.0,0.1)) < 1e-3) $"expected near center got {p}"
            Expect.floatClose Accuracy.low d 0.1 "half of minor side"
        }

        test "Diamond center" {
            // Diamond (rhombus) rotated square side length sqrt2 => coords
            let pts = [| Pt(0,1); Pt(1,0); Pt(0,-1); Pt(-1,0); Pt(0,1) |]
            let pl = Polyline2D.create pts
            let p,d = pl.FindLablePoint 1e-4
            Expect.isTrue (Pt.distance p (Pt(0,0)) < 1e-4) $"expected origin got {p}"
            // distance to edge is 1 (inradius of diamond with diagonals 2 and 2 is 1?) Actually inradius for rhombus with diagonals 2,2 equals area/semiperimeter. Area=2, side length = sqrt2, perimeter=4*sqrt2, inradius=Area/(Per/2)=2/(2*sqrt2)=1/sqrt2≈0.7071.
            Expect.floatClose Accuracy.medium d (1.0/sqrt 2.0) "diamond inradius"
        }

        test "Skinny polygon" {
            // A zigzag skinny polygon approx width 0.1
            let pts = [|
                Pt(0,0); Pt(5,0); Pt(5,0.05); Pt(4,0.05); Pt(4,0.1); Pt(3,0.1); Pt(3,0.05); Pt(0,0.05); Pt(0,0)
            |]
            let pl = Polyline2D.create pts
            let p,d = pl.FindLablePoint 1e-3
            Expect.isTrue (p.Y > 0.02 && p.Y < 0.08) $"y inside strip {p}"
            Expect.isTrue (d < 0.06 && d > 0.02) $"distance within width bounds {d}"
        }



        test "Large coords stability" {
            // https://github.com/mapbox/polylabel/issues/119
            let poly119Pts =
                [| Pt(-111.1815838, 45.7768091);
                Pt(-111.1816944, 45.7766950);
                Pt(-111.1812484, 45.7764847);
                Pt(-111.1811378, 45.7765988);
                Pt(-111.1815838, 45.7768091) |]
            let pl = Polyline2D.create poly119Pts
            let p,d = pl.FindLablePoint 0.001
            // After normalization fix, point should lie inside bounding box center-ish
            let bb = pl.BoundingRectangle
            Expect.isTrue (p.X >= bb.MinX && p.X <= bb.MaxX && p.Y >= bb.MinY && p.Y <= bb.MaxY) $"point outside bbox {p}"
            // Distance should be positive (inside) and small given skinny shape
            Expect.isTrue (d > 0.0) $"expected positive distance got {d}"
        }

        test "Large coords stability high precision" {
            // https://github.com/mapbox/polylabel/issues/119
            let poly119Pts =
                [| Pt(-111.1815838, 45.7768091);
                Pt(-111.1816944, 45.7766950);
                Pt(-111.1812484, 45.7764847);
                Pt(-111.1811378, 45.7765988);
                Pt(-111.1815838, 45.7768091) |]
            let pl = Polyline2D.create poly119Pts
            let p,d = pl.FindLablePoint 0.00000001
            // After normalization fix, point should lie inside bounding box center-ish
            let bb = pl.BoundingRectangle
            Expect.isTrue (p.X >= bb.MinX && p.X <= bb.MaxX && p.Y >= bb.MinY && p.Y <= bb.MaxY) $"point outside bbox {p}"
            // Distance should be positive (inside) and small given skinny shape
            Expect.isTrue (d > 0.0) $"expected positive distance got {d}"
        }
    ]
