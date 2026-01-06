namespace Euclid

open System
open UtilEuclid

/// Provides operations on 2D Triangles.
type Tria2D =

    /// Returns the determinant (2D cross product), i.e., the signed double area of a triangle.
    /// If the result is positive, the points are in counter-clockwise order.
    static member inline  det (a:Pt, b:Pt, c:Pt) :float =
        // 2 edges of the triangle as vectors
        let vX = b.X - a.X
        let vY = b.Y - a.Y
        let wX = c.X - a.X
        let wY = c.Y - a.Y
        // Cross Product of the two edges
        vX * wY - vY * wX

    /// Returns the signed area of a triangle.
    /// If the result is positive, the points are in counter-clockwise order.
    static member inline areaSigned (a:Pt, b:Pt, c:Pt) :float =
        Tria2D.det(a, b, c) * 0.5

    /// Returns the double area of a triangle.
    /// This is the fastest way to get a comparison or sorting value for the areas of triangles.
    /// This is just the absolute value of the 2D Cross Product.
    static member inline areaDouble (a:Pt, b:Pt, c:Pt) :float =
        abs(Tria2D.det(a, b, c))

    /// Returns the area of a triangle described by 3 points.
    static member inline area (a:Pt, b:Pt, c:Pt) :float =
        Tria2D.areaDouble(a, b, c) *  0.5


    /// Checks if three points are in one line.
    /// This is a very fast check, but it is hard to find an appropriate tolerance. (Default is 0.001)
    /// This tolerance is the area of the parallelogram described by two vectors created from the 3 points.
    /// So it also returns true if the points are equal or very close to each other.
    /// Returns false for NaN input values.
    /// Use Points.areInLine if you need better control over the actual tolerance distance.
    static member inline isLinearFast (a:Pt, b:Pt, c:Pt, [<OPT;DEF(0.001)>] maxAreaParallelogram:float) =
        let doubleArea = Tria2D.areaDouble(a, b, c)
        doubleArea < maxAreaParallelogram


    /// Checks if three points are in one line.
    /// By finding the biggest angle in the triangle.
    /// And then measuring the distance from this point to the line defined by the other two points.
    static member isLinear (a:Pt, b:Pt, c:Pt, [<OPT;DEF(1e-6)>] distanceTolerance:float) =
        let inline distanceSqLineToPtInfinite(lnFrom:Pt, lnTo:Pt, p:Pt,  lnSqLen:float) =
            // rewritten from Line3D.distanceLineToPtInfinite
            // http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
            let x = lnFrom.X - lnTo.X
            let y = lnFrom.Y - lnTo.Y
            let u = lnFrom.X - p.X
            let v = lnFrom.Y - p.Y
            let dot = x*u + y*v
            let t = dot/lnSqLen
            let x' = lnFrom.X - x * t
            let y' = lnFrom.Y - y * t
            let u' = x' - p.X
            let v' = y' - p.Y
            u'*u' + v'*v'

        let ab = b-a
        let bc = c-b
        let ca = a-c
        let abLenSq = ab.LengthSq
        let bcLenSq = bc.LengthSq
        let caLenSq = ca.LengthSq
        let distSq = distanceTolerance*distanceTolerance
        // previously:
        // Fails if any of the points are closer than the given distance tolerance to each other.
        // if not(distSq < abLenSq) then EuclidException.Raisef "Points.areInLine failed on very short line %O to %O " a b
        // if not(distSq < bcLenSq) then EuclidException.Raisef "Points.areInLine failed on very short line %O to %O " b c
        // if not(distSq < caLenSq) then EuclidException.Raisef "Points.areInLine failed on very short line %O to %O " c a
        if abLenSq < distSq || bcLenSq < distSq || caLenSq < distSq then // two or more points are too close to each other
            true
        else
            let dotA = ab *** ca
            let dotB = bc *** ab
            let dotC = ca *** bc
            // the corner with the biggest dot product is the one with the biggest angle,
            // so the one with the closest distance to the opposite line in this triangle
            if dotA > dotB && dotA > dotC then
                distanceSqLineToPtInfinite(b, c, a, bcLenSq) < distSq
            elif dotB > dotA && dotB > dotC then
                distanceSqLineToPtInfinite(c, a, b, caLenSq) < distSq
            else
                distanceSqLineToPtInfinite(a, b, c, abLenSq) < distSq


    /// Returns the offset point based on the previous and next normals, the distance and the precomputed cosine (= dot product of nPrev * nNext).
    /// Returns Infinity if the cosine is -1.0 (normals in opposite directions at a 180-degree U-turn).
    static member inline private offsetPtCore (pt:Pt) (dist:float) (nPrev:UnitVc) (nNext:UnitVc) (cosine:float) =
        // based on https://www.angusj.com/clipper2/Docs/Trigonometry.htm
        pt + (nPrev + nNext) * (dist / (1.0 + cosine)) // offset point


    /// Returns the offset point based on the previous and next normals, the distance and the precomputed cosine (= dot product of nPrev * nNext).
    /// Checks for 180 degrees U-turns. sets it to 179 degrees
    static member inline private offsetPtCoreSafe (pt:Pt) (dist:float) (nPrev:UnitVc) (nNext:UnitVc) (cosine:float) =
        if withMeasure cosine < Cosine.``179.0`` then // check for 180 degrees U-turns, (0.0 degrees are handled fine)
            let v = nPrev.Rotate90CCW + nNext.Rotate90CW // the offset vector with length of 2.0
            pt + v * 28.645 //tangent function of 89 degrees * 0.5
        else
            Tria2D.offsetPtCore pt dist nPrev nNext cosine


    /// Offsets one point by a given distance.
    /// If the points 'prev', 'this' and 'next' are in counter-clockwise order, the offset is inwards.
    /// Otherwise it is outwards.
    /// A negative offset distance inverts the direction.
    static member offsetPt(ptToOffset:Pt, prev:Pt,  next:Pt, dist:float) =
        let vPrev = ptToOffset - prev |> Vc.unitize
        let vNext = next - ptToOffset |> Vc.unitize
        let nPrev = vPrev |> UnitVc.rotate90CCW
        let nNext = vNext |> UnitVc.rotate90CCW
        let mutable cosine = nPrev *** nNext
        // check for 180 degrees U-turns. 179 degrees is exactly -0.9998476951563913
        // 0 degrees are handled fine
        if withMeasure cosine < Cosine.``179.0`` then // check for 180 degrees U-turns, (0.0 degrees are handled fine)
            let v = vPrev - vNext // the offset vector with length of almost 2.0
            ptToOffset + v * 28.645 //tangent function of 89 degrees * 0.5
        else
            Tria2D.offsetPtCore ptToOffset dist nPrev nNext cosine


    /// Offsets all points by a given distance.
    /// If the points are in counter-clockwise order, the offset is inwards.
    /// Otherwise it is outwards.
    /// A negative offset distance inverts the direction.
    static member offset(a:Pt, b:Pt, c:Pt, dist:float) : Pt*Pt*Pt =
        let na = b-a |> Vc.unitize |> UnitVc.rotate90CCW
        let nb = c-b |> Vc.unitize |> UnitVc.rotate90CCW
        let nc = a-c |> Vc.unitize |> UnitVc.rotate90CCW
        let mutable cosineA = na *** nc
        let mutable cosineB = na *** nb
        let mutable cosineC = nb *** nc
        (Tria2D.offsetPtCoreSafe a dist na nc cosineA,
         Tria2D.offsetPtCoreSafe b dist na nb cosineB,
         Tria2D.offsetPtCoreSafe c dist nb nc cosineC)


    /// Finds the offset point based on
    /// the previous and next unit normals (= offset direction),
    /// and their offset distances.
    /// This is the core function for offsetting variable. it is used by Tria2D.offsetVar(..).
    static member offsetPtVarByNormals (ptToOffset:Pt, prevN:UnitVc, nextN:UnitVc, prevDist:float, nextDist:float) : ValueOption<Pt> =
        // first get two offset lines to intersect:
        let prevPt = ptToOffset + prevN * prevDist
        let nextPt = ptToOffset + nextN * nextDist
        let prevDir = prevN.Rotate90CCW // for line-line intersection it doesn't matter which side they are rotated to
        let nextDir = nextN.Rotate90CCW

        // Start line-line intersection:
        // first check if the two lines are parallel:
        let aXb = prevDir.Cross nextDir // the cross product of the two vectors

        if abs aXb > zeroLengthTolerance then  // not parallel
            // find line-line intersection using 2D Cross Product:
            // https://www.youtube.com/watch?v=c065KoXooSw and
            // https://gist.github.com/EgoMoose/f3d22a503e0598c12d1e3926dc72fb19det
            let tPrev = ((nextPt-prevPt).Cross nextDir) / aXb
            // evaluate the parameter on prev
            let xPt = prevPt + prevDir * tPrev // the offset point
            ValueSome xPt

        elif abs (prevDist - nextDist) < zeroLengthTolerance && prevDir *** nextDir > 0.0 then // parallel, same direction, and offset distances are the same, so no intersection is needed
            ValueSome prevPt //(pt + prevN * prevDist + nextN * nextDist * 0.5) // average out?

        else
            ValueNone

    /// Finds the offset point based on
    /// the previous and next points,
    /// and their offset distances.
    /// A positive offset distance will be to the left side of the line from 'prev' to 'ptToOffset'.
    /// A negative offset distance inverts the direction.
    /// This function use Tria2D.offsetVarByNormals(..) internally.
    static member offsetPtVar (ptToOffset:Pt, prev:Pt, next:Pt, distPrev:float, distNext:float) : ValueOption<Pt> =
        let vPrev = ptToOffset - prev |> Vc.unitize
        let vNext = next - ptToOffset |> Vc.unitize
        let nPrev = vPrev |> UnitVc.rotate90CCW
        let nNext = vNext |> UnitVc.rotate90CCW
        Tria2D.offsetPtVarByNormals(ptToOffset, nPrev, nNext, distPrev, distNext)

