namespace Euclid

open System
open System.Collections.Generic
open UtilEuclid
open EuclidErrors




module LoopIntersectionCore =


    /// A Discriminated Union with the result cases
    /// from the first step of a 2D line-line intersection test.
    type LineLineRelation =
        // TODO: this DU could also be encoded via Float NaN and infinity to avoid an extra object allocation (using ref out parameters?)
        |NoIntersection
        |Colinear // within threshold, might still not overlap, needs to be checked via BRect
        |Parallel // more than threshold apart
        |BfromRight of struct (float * float) // parameters for unit-vector, might be out of bounds by snapThreshold
        |BfromLeft  of struct (float * float) // parameters for unit-vector, might be out of bounds by snapThreshold

    /// Returns the parameter on vector 'va' where 'va' and 'vb' intersect as endless rays.
    /// If they start from points 'a' and 'b' respectively.
    /// Pass in va.Cross vb as precomputed and inverted.
    let inline private getXPara (a:Pt, vaXvbInverse:float, b:Pt, vb:UnitVc) :float =
        // find intersection using 2D Cross Product:
        // https://www.youtube.com/watch?v=c065KoXooSw and https://gist.github.com/EgoMoose/f3d22a503e0598c12d1e3926dc72fb19
        ((b-a).Cross vb) * vaXvbInverse // va.Cross vb is precomputed and inverted

    let inline private isParamStillBelowZeroAfterOffsets(ap:Pt, au:UnitVc, aXbInverse:float, bp:Pt, bu:UnitVc, snapThreshold:float) :bool =
        let n = au.Rotate90CCW * snapThreshold
        // TODO would it be enough to only compute one of these two? depending on the sign of aXbInverse ?
        getXPara(ap + n, aXbInverse, bp, bu) <  -snapThreshold //with threshold subtracted the  range factor is 1 to 1.4 . without 0.7 to 1 of threshold
        &&
        getXPara(ap - n, aXbInverse, bp, bu) <  -snapThreshold

    let inline private isParamStillMoreThanLengthAfterOffsets(ap:Pt, au:UnitVc, aXbInverse:float, al:float, bp:Pt, bu:UnitVc, snapThreshold:float) :bool =
        let n = au.Rotate90CCW * snapThreshold
        getXPara(ap + n, aXbInverse, bp, bu) > al + snapThreshold //with threshold added the range factor is 1 to 1.4 . without 0.7 to 1 of threshold
        &&
        getXPara(ap - n, aXbInverse, bp, bu) > al + snapThreshold



    /// A call to this should be preceded by a bounding box check to exit quickly if apart.
    /// For line A and line B, give for each:
    /// Start point, unitized direction, line length.
    /// And finally a tolerance: Curve A will be extended on both ends and offset to both sides.
    /// These offsets will also be checked with curve B that is also extended by this amount.
    let internal getRelation (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) : LineLineRelation =
        let aXb = au.Cross bu // precomputed Cross Product

        if abs(aXb) > zeroLengthTolerance then  // not parallel
            let aXbInverse = 1./aXb // invert only once, then pass it on as inverted value
            let ta = getXPara (ap, aXbInverse, bp, bu)

            // parameter on first is below zero, so probably no intersection  unless closer than snapThreshold and almost colinear
            if ta < -snapThreshold && isParamStillBelowZeroAfterOffsets (ap, au, aXbInverse, bp, bu, snapThreshold) then
                NoIntersection // no need to even check parameter on second segment

            // parameter on first segment is  beyond length, so probably no intersection  unless closer than snapThreshold and colinear
            elif ta > al+snapThreshold && isParamStillMoreThanLengthAfterOffsets(ap, au, aXbInverse, al, bp, bu, snapThreshold) then
                NoIntersection // no need to even check parameter on second segment

            // now checking if parameter on second line is inside too:
            else
                // this might still be a very shallow intersection that counts as parallel
                let bXaInverse = -aXbInverse
                let tb = getXPara (bp, bXaInverse, ap, au)

                // parameter on second segment is  below zero, so probably no intersection  unless closer than snapThreshold and colinear
                if tb < -snapThreshold && isParamStillBelowZeroAfterOffsets (bp, bu, bXaInverse, ap, au, snapThreshold) then
                    NoIntersection

                // parameter on second segment is  beyond length, so probably false unless closer than snapThreshold and colinear
                elif tb > bl + snapThreshold && isParamStillMoreThanLengthAfterOffsets (bp, bu, bXaInverse, bl, ap, au, snapThreshold) then
                    NoIntersection
                else
                    if aXb > 0.0 then BfromRight (ta, tb) // TODO might still be almost colinear. was an intersection very far outside bounding Rectangles.
                    else              BfromLeft  (ta, tb) // TODO could to be almost colinear too, check offset  !!

        else // Colinear
            // probably no intersection unless closer than snapThreshold
            let perp = au.Rotate90CCW // unit v
            let vab = ap-bp
            let dot = perp *** vab // project vab onto unit-vector
            if abs dot < snapThreshold then
                Colinear // parallel distance is less than snapThreshold distance, TODO but actual overlap needs to be confirmed via BRect
            else
                Parallel // parallel distance is more than snapThreshold distance,

    /// This function includes an initial call to BRect.doOverlap.
    let inline internal doIntersectOrOverlapColinear (ap:Pt, au:UnitVc, al:float, abb:BRect, bp:Pt, bu:UnitVc, bl:float, bbb:BRect, snapThreshold:float) : bool =
        BRect.doOverlap abb bbb
        &&
        match getRelation(ap, au, al, bp, bu, bl, snapThreshold)   with
        |NoIntersection -> false
        |Parallel       -> false
        |BfromLeft _    -> true
        |BfromRight _   -> true
        |Colinear       -> true


    /// Returns the intersection point or midpoint between two 2D lines.
    /// (Used mainly for drawing debug notes at this point)
    let internal getXPointOrMid (ap:Pt, au:UnitVc, al:float, bp:Pt, bu:UnitVc, bl:float, snapThreshold:float) : Pt =
        match getRelation(ap, au, al, bp, bu, bl, snapThreshold)   with
        |NoIntersection
        |Colinear
        |Parallel            -> (ap + ap + bp + bp + au*al + bu*bl) * 0.25
        |BfromLeft  (ta, _) ->  ap + au * ta // clamp point to actually be on line even if it is not quite in case of PreStart or PostEnd
        |BfromRight (ta, _) ->  ap + au * ta



/// Discriminated union for the three possible relations of a point to a closed polyline Loop.
/// Points are considered on loop if they are within loop.SnapThreshold tolerance.
[<RequireQualifiedAccess>]
type PointLoopRel =
    In | On | Out

    member r.IsInside =
        r = PointLoopRel.In

    member r.IsOutside =
        r = PointLoopRel.Out


/// A counter-clockwise, closed series of 2D points.
/// Checked for too short segments and duplicate points but might have collinear points.
/// Checked for self intersection.
/// This class stores for each segment precomputed list of unit-vectors, lengths and bounding Rectangles.
/// This is to have better performance when calculating Loop with Loop intersections or point containment.
type Loop private   ( pts:ResizeArray<Pt>
                    , unitVcts:UnitVc[]
                    , bRects:BRect[]
                    , lens:float[]
                    //, xys:float[] // TODO is this needed to be always precomputed ?
                    , area:float
                    , minSegmentLength:float
                    , snapThreshold:float
                    , bRect:BRect
                    ) =

    /// Without sign, since loop is guaranteed to be Counter Clockwise.
    /// This Value is precomputed in constructor.
    member _.Area : float = area

    /// A List of precomputed bounding rectangles for each segment.
    /// Each bounding rectangle is expanded by the SnapThreshold.
    /// This list is one item shorter than Points.
    member _.BRects = bRects

    /// A List of precomputed UnitVectors for each segment.
    /// This list is one item shorter than Points.
    member _.UnitVectors = unitVcts

    /// A List of the lengths of each segment.
    /// This list is one item shorter than Points.
    member _.Lengths = lens

   // List of X and Y coordinates of Points.
   // Even indices are X
   // Odd  indices are Y
   // Length is double of SegmentCount
   // Last pair is not equal first pair.
   //member _.XYs = xys

    /// This list is one item Longer than vectors, BRects or Lengths.
    /// Last point equals the first point.
    member _.Points = pts

    /// One less than Points count.
    member val SegmentCount = unitVcts.Length

    /// The overall bounding rectangle.
    /// Including an expansion by snapThreshold.
    member _.ExpandedBoundingRect = bRect

    /// The minimum distance between points in this loop, This is a parameter at creation.
    member _.MinSegmentLength = minSegmentLength

   /// This value is used when calculating intersection or point containment.
   /// Points within this distance of the segment will be considered on the segment.
   /// This parameter needs to be set at creation since it is used in pre-computations of bounding rectangles.
    member _.SnapThreshold = snapThreshold

    /// Creates a deep copy.
    member _.Clone() = Loop(pts.GetRange(0, pts.Count), Array.copy unitVcts, Array.copy bRects, Array.copy lens, //Array.copy xys,
                            area, minSegmentLength, snapThreshold, bRect)

    /// Returns closest segment index.
    member lo.ClosestSegment (pt:Pt) :int=
        let ps = lo.Points
        let us = lo.UnitVectors
        let ls = lo.Lengths
        let mutable dMin = Double.MaxValue
        let mutable iMin = -1
        for i = 0 to lo.SegmentCount-1 do
            let t = ps.[i]
            let n = ps.[i+1]
            let u = us.[i]
            let l = ls.[i]
            let d = pt.SqDistanceToLine(t, n, u, l)
            if d < dMin then
                dMin  <- d
                iMin  <- i
        iMin

     /// Returns closest and second closest segment index.
     /// They might both contain the closest point. ( in the corner where they meet)
    member lo.ClosestSegments (pt:Pt) =
        let ps = lo.Points
        let us = lo.UnitVectors
        let ls = lo.Lengths
        let mutable dMin = Double.MaxValue
        let mutable iFst = 0
        let mutable iSnd = 0
        for i = 0 to lo.SegmentCount-1 do
            let t = ps.[i]
            let n = ps.[i+1]
            let u = us.[i]
            let l = ls.[i]
            let d = pt.SqDistanceToLine(t, n, u, l)
            if d <= dMin then
                dMin  <- d
                iSnd <- iFst
                iFst <- i
        iFst, iSnd

    /// Returns the closest point.
    member lo.ClosestPoint (pt:Pt) :Pt=
        let i = lo.ClosestSegment(pt)
        let t = lo.Points.[i]
        let u = lo.UnitVectors.[i]
        let l = lo.Lengths.[i]
        pt.ClosestPointOnLine(t, u, l)


    /// <summary>Returns the winding number for this polygon, around a given point</summary>
    /// <param name="point">The point to check winding around</param>
    /// <returns>The winding number, if it is not 0 then point is contained in the Loop</returns>
    member lo.WindingNumber (point:Pt) : int =
        // from https://github.com/FreyaHolmer/Mathfs/blob/master/Runtime/Geometric%20Shapes/Polygon.cs#L92
        // https://x.com/FreyaHolmer/status/1232826293902888960
        // or use ? https://github.com/blenderfan/AdvancedGamedevTutorials/blob/main/AdvancedGamedev-WindingNumbers/Polygon2D.cs
        // https://www.youtube.com/watch?v=E51LrZQuuPE
        let rect = lo.ExpandedBoundingRect
        if not <| rect.Contains point then  // the bounding Rectangle includes an expansion by snap SnapThreshold
            0 // no looping needed, point is outside
        else
            let inline isLeft (a:Pt) (b:Pt) (p:Pt) =
                let det = Vc.cross (p-a, b-a)
                if   det >  1e-12 then  1
                elif det < -1e-12 then -1
                else 0

            let mutable winding = 0
            let pts = lo.Points
            let mutable this = pts.[0]
            for i = 1 to pts.LastIndex do  //start at 1 because our Points list has the first point repeated at the end
                let next = pts.[i]
                if this.Y <= point.Y then
                    if next.Y > point.Y && isLeft this next point > 0 then
                        winding <- winding - 1
                else
                    if next.Y <= point.Y && isLeft this next point < 0 then
                        winding <- winding + 1
                this <- next

            winding




    /// Returns Relation between point and Loop: Inside, On or Outside.
    /// Tolerance for being on Loop is SnapThreshold.
    /// this implementation using the closest two segments is always correct.
    /// faster implementations such as WindingNumber or  counting the crossings of a horizontal ray do fail if several loop segments are on the Y-axis and the test point too.
    member lo.ContainsPoint(pt:Pt) =
        let rect = lo.ExpandedBoundingRect
        if not <| rect.Contains pt then  // the bounding Rectangle includes an expansion by snap SnapThreshold
            PointLoopRel.Out
        else
            let ps = lo.Points
            let us = lo.UnitVectors
            let ls = lo.Lengths
            let j, k = lo.ClosestSegments pt

            let pj = ps.[j]
            let uj = us.[j]
            let lj = ls.[j]
            let dj = pt.DistanceToLine(pj, uj, lj)
            if dj < snapThreshold then
                PointLoopRel.On
            else
                let pk = ps.[k]
                let uk = us.[k]
                let lk = ls.[k]
                let dk = pt.DistanceToLine(pk, uk, lk)
                let i =
                    if dj + lo.SnapThreshold < dk then // check if dj is closer than dk by more than the tolerance
                        j
                    else
                        // explicitly compare both offset of the two closest segments
                        let uj90 = uj.Rotate90CW * lo.SnapThreshold
                        let ddj = min (pt.SqDistanceToLine(pj+uj90, uj, lj)) (pt.SqDistanceToLine(pj-uj90, uj, lj))
                        let uk90 = uk.Rotate90CW * lo.SnapThreshold
                        let ddk = min (pt.SqDistanceToLine(pk+uk90, uk, lk)) (pt.SqDistanceToLine(pk-uk90, uk, lk))
                        if ddj <= ddk then j else k

                // once correct index is found check on which side the point is
                if Vc.cross (lo.UnitVectors.[i], pt-lo.Points.[i] ) < 0.0 then
                    PointLoopRel.Out
                else
                    PointLoopRel.In

    /// Creates a Loop from series of points.
    /// Checks for too short segments.
    /// Closes loop if not closed yet.
    /// Makes it Counterclockwise.
    /// Also checks for self intersection.
    /// Does NOT remove collinear points.
    static member create (minSegmentLength:float) (snapThreshold:float) (points:IList<Pt>)=
        let pts =
            if isNegative minSegmentLength then fail $"Loop constructor: minSegmentLength < 0.0:  {minSegmentLength}"
            if isNegative snapThreshold    then fail $"Loop constructor: snapThreshold < 0.0:  {snapThreshold}"
            if points.Count<3 then                fail $"Loop constructor: Input ResizeArray needs to have at least three points, not  {points.Count}"

            let ps= ResizeArray<Pt>(points.Count+1)
            // check gap sizes
            let minLenSq = minSegmentLength * minSegmentLength
            ps.Add points.[0]
            for i = 1 to points.Count-1 do
                let pt = points.[i]
                if Pt.distanceSq ps.Last pt > minLenSq then
                    ps.Add pt
                else
                    // set last to average
                    ps.Last <- (ps.Last + pt) *0.5
                    #if DEBUG || CHECK_EUCLID // CHECK_EUCLID so checks can still be enabled when using with Fable release mode
                    // Debug2D.drawDot (sprintf "short segment: %d" (i-1), pt)
                    eprintfn "Loop constructor: Segment %d shorter than %g was skipped, it was just %g long." (i-1) snapThreshold (Pt.distance ps.Last pt)
                    #endif
            // close
            if Pt.distanceSq ps.Last ps.First > minLenSq then
                ps.Add ps.First // add first to close loop
            else
                ps.Last <- ps.First // make sure they are exactly the same
            ps

        let segCount = pts.Count-1

        let segLastIdx = pts.Count-2


        // get Area, and Reverse loop to make loop Counter-Clockwise, if needed
        let area =
            //get signed area:
            //https://helloacm.com/sign-area-of-irregular-polygon/
            let mutable sArea = 0.0
            let mutable t = pts.[0]
            for i=1 to pts.Count-1 do
                let n = pts.[i]
                let a = t.X - n.X
                let b = n.Y + t.Y
                sArea <- sArea + a*b
                t <- n
            sArea <-sArea * 0.5

            if sArea < 0. then
                pts.Reverse()
                -sArea
            else
                sArea

        let mutable xMin, yMin = Double.MaxValue, Double.MaxValue // for overall bounding Rectangle
        let mutable xMax, yMax = Double.MinValue, Double.MinValue

        // loop again to precalculate vectors, unit-vectors, BRects, and lengths
        let unitVcts, bRects, lens = //, xys=
            let uvs  = Array.zeroCreate (segCount)
            let bs   = Array.zeroCreate (segCount)
            let lens = Array.zeroCreate (segCount)
            //let xy = Array.zeroCreate (segCount*2)
            let mutable xyi = 0
            let mutable t = pts.[0]
            for ii = 1 to pts.Count-1 do // start at +1, last = first
                let n = pts.[ii]
                let v = Vc.create(t, n)
                let l = v.Length
                let i = ii-1 // because loop starts at 1
                uvs.[ i] <- UnitVc.createUnchecked( v.X/l, v.Y/l) // no check for div by zero needed, since minSpacing is already checked
                bs.[  i] <- BRect.create(t, n)|> BRect.expandSafe snapThreshold
                lens.[i] <- l
                //xy.[xyi  ] <- t.X
                //xy.[xyi+1] <- t.Y
                xyi <- xyi + 2

                // overall bounding box:
                xMin <- min xMin t.X
                yMin <- min yMin t.Y
                xMax <- max xMax t.X
                yMax <- max yMax t.Y
                // swap:
                t <- n
            uvs, bs, lens//, xy


        // Test for 180 U-turns
        // angle 160 Degrees, dot product of unit-vectors: -0.93969
        // angle 170 Degrees, dot product of unit-vectors: -0.984808
        // angle 178 Degrees, dot product of unit-vectors: -0.999391
        // Check there is no U-Turn between 170 and 180 Degrees
        let mutable t = unitVcts.[0]
        for ii=1 to segLastIdx do
            let n = unitVcts.[ii]
            if withMeasure (t *** n) <  Cosine.``177.5`` then
                // Debug2D.drawDot ("+177.5° degree turn in Loop?", pts.[ii])
                fail "Euclid.Loop: Lines for Loop make a kink or U-Turn bigger than 177.5 degrees."
            t <- n

        // Check for self intersection,
        let inline selfIntersectionCheck(i, from, till)=
            // TODO quadratic runtime !  replace with sweep line algorithm ?
            let ap = pts.[i]
            let au = unitVcts.[i]
            let al = lens.[i]
            let abb = bRects.[i]
            for j = from to till do
                let bbb = bRects.[j]
                // test on BRect overlap could be done here already instead of in doIntersectOrOverlapCollinear.
                let bp = pts.[j]
                let bu = unitVcts.[j]
                let bl = lens.[j]
                if LoopIntersectionCore.doIntersectOrOverlapColinear(ap, au, al, abb, bp, bu, bl, bbb, snapThreshold) then
                    // Debug2D.drawDot (sprintf "self X: %O + %O"  i j, LoopIntersectionCore.getXPointOrMid(ap, au, al, bp, bu, bl, snapThreshold))
                    // Debug2D.drawLineFromTo(ap, ap+au*al)
                    // Debug2D.drawLineFromTo(bp, bp+bu*bl)
                    fail $"Loop: Loop of {points.Count} Points has self intersection."

        if unitVcts.Length > 3 then // a triangle is covered by angle checks above
            // checking second last and last
            selfIntersectionCheck(segLastIdx  , 1, 1)
            selfIntersectionCheck(segLastIdx-1, 0, 0)
            for i = 1 to segLastIdx do // start at second, (last and first do intersect)
                // TODO quadratic runtime !  replace with sweep line algorithm ?
                selfIntersectionCheck(i, i+2, segLastIdx)

        let erect = BRect.createUnchecked(xMin-snapThreshold, yMin-snapThreshold, xMax+snapThreshold, yMax+snapThreshold)

        Loop(   pts, unitVcts, bRects, lens, //xys,
                area, minSegmentLength, snapThreshold, erect)