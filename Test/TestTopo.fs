module TestTopo

open Euclid

#if FABLE_COMPILER_JAVASCRIPT || FABLE_COMPILER_TYPESCRIPT
open Fable.Mocha
#else
open Expecto
#endif

let inline eq a b = Pt.dist a b < 1e-9


/// Element type used in all tests. Carries an index so we can verify ordering
/// after Topology2D rewrites the underlying ResizeArray.
type Li = {
    idx:int
    ln:Line2D
}

/// Build an Li from raw coordinates.
let inline mkLi i fx fy tx ty : Li =
    {idx = i; ln = Line2D(Pt(float fx, float fy), Pt(float tx, float ty))}

let getLn (l:Li) : Line2D = l.ln

/// Return the idx sequence of a group of Li.
let idxSeq (g: ResizeArray<Li>) : int list =
    [ for x in g -> x.idx ]

/// Return the (idx, reversed) sequence for joinReversing results.
let idxRevSeq (g: ResizeArray<Li*bool>) : (int * bool) list =
    [ for x, r in g -> x.idx, r ]


/// 3D element type, mirroring Li but carrying a Line3D.
/// The line field is named 'ln3' (not 'ln') so the 2D 'Li' record literals
/// above are not accidentally resolved to this type by field-name inference.
type Li3 = {
    idx:int
    ln3:Line3D
}

/// Build an Li3 from raw coordinates.
let inline mkLi3 i fx fy fz tx ty tz : Li3 =
    {idx = i; ln3 = Line3D(Pnt(float fx, float fy, float fz), Pnt(float tx, float ty, float tz))}

let getLn3 (l:Li3) : Line3D = l.ln3

let idx3Seq (g: ResizeArray<Li3>) : int list =
    [ for x in g -> x.idx ]

let idxRev3Seq (g: ResizeArray<Li3*bool>) : (int * bool) list =
    [ for x, r in g -> x.idx, r ]

let inline eq3 (a:Pnt) (b:Pnt) = Pnt.dist a b < 1e-9

/// Check the [FromX;FromY;ToX;ToY] cache returned by the 2D cached sorts is
/// coordinate-consistent with the (possibly reordered/reversed) elements.
let cache2DMatches (xs:ResizeArray<Li>) (cache:float[]) : bool =
    let mutable ok = cache.Length = xs.Count * 4
    for k in 0 .. xs.Count - 1 do
        let l = xs.[k].ln
        let b = k * 4
        ok <- ok
              && abs(cache.[b    ] - l.FromX) < 1e-9
              && abs(cache.[b + 1] - l.FromY) < 1e-9
              && abs(cache.[b + 2] - l.ToX)   < 1e-9
              && abs(cache.[b + 3] - l.ToY)   < 1e-9
    ok

/// Check the [FromX;FromY;FromZ;ToX;ToY;ToZ] cache returned by the 3D cached sorts
/// is coordinate-consistent with the (possibly reordered/reversed) elements.
let cache3DMatches (xs:ResizeArray<Li3>) (cache:float[]) : bool =
    let mutable ok = cache.Length = xs.Count * 6
    for k in 0 .. xs.Count - 1 do
        let l = xs.[k].ln3
        let b = k * 6
        ok <- ok
              && abs(cache.[b    ] - l.FromX) < 1e-9
              && abs(cache.[b + 1] - l.FromY) < 1e-9
              && abs(cache.[b + 2] - l.FromZ) < 1e-9
              && abs(cache.[b + 3] - l.ToX)   < 1e-9
              && abs(cache.[b + 4] - l.ToY)   < 1e-9
              && abs(cache.[b + 5] - l.ToZ)   < 1e-9
    ok


// ---------------------------------------------------------------------------
// Original setup, preserved so the existing test below keeps validating the
// same scenario it did before.
// ---------------------------------------------------------------------------

let v1 = Vc(0,1)
let h1 = Vc(1,0)
let lns =
    [|
    {idx = 0; ln = Line2D.createFromPtAndVc (Pt(0,2), v1)}
    {idx = 1; ln = Line2D.createFromPtAndVc (Pt(0,3), v1)}
    {idx = 2; ln = Line2D.createFromPtAndVc (Pt(0,1), v1)}
    {idx = 3; ln = Line2D.createFromPtAndVc (Pt(0,0), v1)}

    {idx = 4; ln = Line2D.createFromPtAndVc (Pt(1,5), h1)}
    {idx = 5; ln = Line2D.createFromPtAndVc (Pt(0,5), h1)}
    |]
    |> ResizeArray



let tests =
    testList "Topology " [

        test "join2D lines" {
            let gs = Topology2D.join( (fun (ln:Li) -> ln.ln), 0.001, lns)
            "gs length" |> Expect.equal gs.Count  2
            "gs[0] length" |> Expect.equal gs.[0].Count  4
            "gs[1] length" |> Expect.equal gs.[1].Count  2
            "gs[0][0] idx=3" |> Expect.equal gs.[0].[0].idx  3
            "gs[0][3] idx=1" |> Expect.equal gs.[0].[3].idx  1
            "gs[1][0] idx=5" |> Expect.equal gs.[1].[0].idx  5
            "gs[1][1] idx=4" |> Expect.equal gs.[1].[1].idx  4

        }


        // -------------------------------------------------------------------
        // Topology2D.join
        // -------------------------------------------------------------------

        test "join: empty input returns empty result" {
            // Docstring: groups returned reflect the input; an empty input
            // therefore yields zero groups.
            let xs = ResizeArray<Li>()
            let gs = Topology2D.join(getLn, 0.001, xs)
            "no groups for empty input" |> Expect.equal gs.Count 0
        }

        test "join: single element returns one group of one" {
            // Docstring: every element must end up in some group; a single
            // disconnected element therefore forms its own singleton group.
            let xs = ResizeArray [ mkLi 0 0 0 1 0 ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "exactly one group" |> Expect.equal gs.Count 1
            "group has one element" |> Expect.equal gs.[0].Count 1
            "preserves the element" |> Expect.equal gs.[0].[0].idx 0
        }

        test "join: two disconnected elements form two singleton groups" {
            // Docstring: when the gap exceeds splitDistance a new group is
            // started. With splitDistance=0.1 the gap of 10.0 between these
            // lines means each must stand alone.
            let xs = ResizeArray [
                mkLi 0  0 0 1 0
                mkLi 1 10 0 11 0
            ]
            let gs = Topology2D.join(getLn, 0.1, xs)
            "two singleton groups" |> Expect.equal gs.Count 2
            "first group is element 0" |> Expect.equal (idxSeq gs.[0]) [0]
            "second group is element 1" |> Expect.equal (idxSeq gs.[1]) [1]
        }

        test "join: pre-ordered open polyline stays in one group" {
            // Docstring: 'For each line end point it finds the next closest
            // line start point.' Already-ordered tail-to-head segments must
            // collapse into a single forward chain.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 1 0 2 0
                mkLi 2 2 0 3 0
                mkLi 3 3 0 4 0
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "one chain"   |> Expect.equal gs.Count 1
            "chain order" |> Expect.equal (idxSeq gs.[0]) [0;1;2;3]
        }

        test "join: reverse-ordered chain joined via backward extension" {
            // Docstring: it searches forward AND backward from the first
            // element. Here the first input element is the END of the chain,
            // so every other element must be picked up by the backward search
            // (matching end-point of next to start-point of head).
            // Expected final order is the natural head-to-tail polyline.
            let xs = ResizeArray [
                mkLi 0 3 0 4 0   // last segment
                mkLi 1 2 0 3 0
                mkLi 2 1 0 2 0
                mkLi 3 0 0 1 0   // first segment
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "one chain"        |> Expect.equal gs.Count 1
            "chain head-to-tail" |> Expect.equal (idxSeq gs.[0]) [3;2;1;0]
        }

        test "join: closed quadrilateral forms one group of four" {
            // Docstring: a closed loop is just a chain whose last endpoint
            // happens to coincide with the first start-point. The function
            // does not detect closure explicitly, but should still produce a
            // single group of all four sides.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 1 0 1 1
                mkLi 2 1 1 0 1
                mkLi 3 0 1 0 0
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "one closed loop" |> Expect.equal gs.Count 1
            "sides in order"  |> Expect.equal (idxSeq gs.[0]) [0;1;2;3]
        }

        test "join: scrambled chain reorders into a single polyline" {
            // Docstring: 'Sorts elements ... into loops or polylines.'
            // Input is shuffled; the algorithm must reconnect them using
            // both forward and backward searches.
            // Starting element is idx=2 (segment (2,0)->(3,0)), so we expect
            // idx 0,1 to be added by backward search and idx 3,4 by forward.
            let xs = ResizeArray [
                mkLi 2 2 0 3 0   // middle segment, starting head
                mkLi 4 4 0 5 0
                mkLi 0 0 0 1 0
                mkLi 3 3 0 4 0
                mkLi 1 1 0 2 0
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "one polyline" |> Expect.equal gs.Count 1
            "elements in head-to-tail order" |> Expect.equal (idxSeq gs.[0]) [0;1;2;3;4]
        }

        test "join: distance exactly at splitDistance keeps elements together" {
            // Docstring: 'If the distance between two points is GREATER than
            // splitDistance a new loop is started.' Distance exactly equal
            // to splitDistance is therefore NOT greater, so it must remain
            // in the same group. The implementation uses <= which is correct.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0   // end at (1,0)
                mkLi 1 2 0 3 0   // start at (2,0), exactly 1.0 away
            ]
            let gs = Topology2D.join(getLn, 1.0, xs)
            "joined in one group" |> Expect.equal gs.Count 1
            "ordered head-to-tail" |> Expect.equal (idxSeq gs.[0]) [0;1]
        }

        test "join: distance just over splitDistance splits the group" {
            // Inverse of the boundary test above: a tiny epsilon over the
            // threshold must put the elements into separate groups.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0           // end at (1,0)
                mkLi 1 2.001 0 3.001 0   // start at (2.001,0) – just over 1.0 from previous end
            ]
            let gs = Topology2D.join(getLn, 1.0, xs)
            "two singleton groups" |> Expect.equal gs.Count 2
            "first group" |> Expect.equal (idxSeq gs.[0]) [0]
            "second group" |> Expect.equal (idxSeq gs.[1]) [1]
        }

        test "join: multiple disconnected polylines yield multiple groups" {
            // Docstring: distinct loops/polylines in the input must come back
            // as distinct ResizeArrays in the result.
            let xs = ResizeArray [
                // Polyline A: (0,0)->(1,0)->(2,0)
                mkLi 0  0  0   1  0
                mkLi 1  1  0   2  0
                // Polyline B: (0,10)->(1,10)->(2,10)
                mkLi 2  0 10   1 10
                mkLi 3  1 10   2 10
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "two groups" |> Expect.equal gs.Count 2
            "group A length" |> Expect.equal gs.[0].Count 2
            "group B length" |> Expect.equal gs.[1].Count 2
            "group A order" |> Expect.equal (idxSeq gs.[0]) [0;1]
            "group B order" |> Expect.equal (idxSeq gs.[1]) [2;3]
        }


        // -------------------------------------------------------------------
        // Topology2D.joinReversing
        // -------------------------------------------------------------------

        test "joinReversing: empty input returns empty result" {
            let xs = ResizeArray<Li>()
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "no groups for empty input" |> Expect.equal gs.Count 0
        }

        test "joinReversing: single element returns one group of one, not reversed" {
            // Docstring: result is 'T*bool where the bool indicates if the
            // element was reversed. A standalone element keeps its native
            // orientation, i.e. false.
            let xs = ResizeArray [ mkLi 0 0 0 1 0 ]
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "exactly one group" |> Expect.equal gs.Count 1
            "group has one element" |> Expect.equal gs.[0].Count 1
            "not reversed" |> Expect.equal (idxRevSeq gs.[0]) [(0, false)]
        }

        test "joinReversing: chain with consistent orientation needs no reversal" {
            // Docstring: 'Start and end points can match with both start and
            // end points.' When all natural matches are head-to-tail, no
            // reversal flags should be set.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 1 0 2 0
                mkLi 2 2 0 3 0
            ]
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "one chain"   |> Expect.equal gs.Count 1
            "all unreversed in order" |> Expect.equal (idxRevSeq gs.[0]) [(0,false);(1,false);(2,false)]
        }

        test "joinReversing: forward search reverses a back-to-back line" {
            // Docstring: the boolean tells the caller to reverse the element.
            // Here line idx=1 is oriented (2,0)->(1,0); to chain to line 0's
            // end at (1,0) the algorithm matches end-to-end and flags it as
            // reversed (true).
            let xs = ResizeArray [
                mkLi 0 0 0 1 0   // ends at (1,0)
                mkLi 1 2 0 1 0   // also ends at (1,0); must be flagged reversed
            ]
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "one chain" |> Expect.equal gs.Count 1
            "second element reversed" |> Expect.equal (idxRevSeq gs.[0]) [(0,false);(1,true)]
        }

        test "joinReversing: backward search reverses a head-to-head line" {
            // Docstring: the backward search extends behind the head of the
            // chain. Here the chain's first start is (1,0); idx=1 starts at
            // (1,0) too, so to attach BEFORE the head it must be reversed.
            let xs = ResizeArray [
                mkLi 0 1 0 2 0   // starts at (1,0)
                mkLi 1 1 0 0 0   // also starts at (1,0); should be reversed and prepended
            ]
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "one chain" |> Expect.equal gs.Count 1
            "reversed element first, original second" |> Expect.equal (idxRevSeq gs.[0]) [(1,true);(0,false)]
        }

        test "joinReversing: mixed orientations form one polyline" {
            // Docstring: the function copes with any mix of orientations.
            // Build a 4-segment polyline where each adjacent pair meets at a
            // different combination of endpoints.
            // Geometry: (0,0) - (1,0) - (2,0) - (3,0) - (4,0)
            //   seg A (0): (0,0)->(1,0)    natural
            //   seg B (1): (2,0)->(1,0)    reversed: shares (1,0) with A.To
            //   seg C (2): (2,0)->(3,0)    natural: shares (2,0) with B.From
            //   seg D (3): (4,0)->(3,0)    reversed: shares (3,0) with C.To
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 2 0 1 0
                mkLi 2 2 0 3 0
                mkLi 3 4 0 3 0
            ]
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "one polyline" |> Expect.equal gs.Count 1
            "expected orientation flags" |> Expect.equal
                (idxRevSeq gs.[0]) [(0,false);(1,true);(2,false);(3,true)]
        }

        test "joinReversing: scrambled input with reversals is reconnected" {
            // Same final chain as the test above, but input order is
            // shuffled to exercise both swap-into-place and reverse-flag
            // handling.
            let xs = ResizeArray [
                mkLi 2 2 0 3 0
                mkLi 0 0 0 1 0
                mkLi 3 4 0 3 0
                mkLi 1 2 0 1 0
            ]
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "one polyline" |> Expect.equal gs.Count 1
            "elements in chain order" |> Expect.equal
                (idxRevSeq gs.[0] |> List.map fst) [0;1;2;3]
        }

        test "joinReversing: disconnected elements form separate groups" {
            // Docstring: groups split where the gap exceeds splitDistance.
            // Two lines 10 units apart with splitDistance=0.1 must end up in
            // their own groups, each unreversed.
            let xs = ResizeArray [
                mkLi 0  0 0  1 0
                mkLi 1 10 0 11 0
            ]
            let gs = Topology2D.joinReversing(getLn, 0.1, xs)
            "two groups" |> Expect.equal gs.Count 2
            "group 0 contents" |> Expect.equal (idxRevSeq gs.[0]) [(0,false)]
            "group 1 contents" |> Expect.equal (idxRevSeq gs.[1]) [(1,false)]
        }

        test "joinReversing: distance exactly at splitDistance keeps elements together" {
            // Docstring: 'If the distance between two points is greater than
            // splitDistance it will be considered as a new group.'
            // At distance == splitDistance the relation is NOT 'greater than',
            // so the two elements must remain in the same group — matching
            // Topology2D.join's behaviour at the same boundary.
            //
            // NOTE: the current implementation uses '<' instead of '<=' on
            // the squared-distance comparisons (Topology2D.fs lines 191, 197,
            // 211, 217). This test will fail until the comparisons are
            // changed to '<='. Topology2D.join already does this correctly.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0   // ends at (1,0)
                mkLi 1 2 0 3 0   // starts at (2,0), exactly 1.0 from previous end
            ]
            let gs = Topology2D.joinReversing(getLn, 1.0, xs)
            "joined in one group" |> Expect.equal gs.Count 1
            "ordered head-to-tail" |> Expect.equal
                (idxRevSeq gs.[0]) [(0,false);(1,false)]
        }


        // -------------------------------------------------------------------
        // Topology2D.sortToLoop
        // -------------------------------------------------------------------

        test "sortToLoop: empty input is a no-op" {
            // Docstring: 'Sorts elements in place'. An empty ResizeArray
            // simply stays empty; no exception must escape.
            let xs = ResizeArray<Li>()
            Topology2D.sortToLoop(getLn, xs)
            "still empty" |> Expect.equal xs.Count 0
        }

        test "sortToLoop: single element is a no-op" {
            // Docstring: sort acts in place; with one element there is
            // nothing to reorder and the array must be untouched.
            let xs = ResizeArray [ mkLi 7 0 0 1 0 ]
            Topology2D.sortToLoop(getLn, xs)
            "still one element" |> Expect.equal xs.Count 1
            "same element"      |> Expect.equal xs.[0].idx 7
        }

        test "sortToLoop: scrambled square sorts into loop order" {
            // Docstring: 'For each line end point it finds the next closest
            // line start point.' Starting from idx=0 at (0,0)->(1,0) the
            // nearest unprocessed From is (1,0) on idx=1, then (1,1) on
            // idx=2, then (0,1) on idx=3 — exactly the loop order.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 2 1 1 0 1
                mkLi 3 0 1 0 0
                mkLi 1 1 0 1 1
            ]
            Topology2D.sortToLoop(getLn, xs)
            "sorted in loop order" |> Expect.equal (idxSeq xs) [0;1;2;3]
        }

        test "sortToLoop: open polyline scrambled gets ordered head-to-tail" {
            // Same idea as the square but on an open chain. Idx is set so
            // that the natural chain order is 0,1,2,3.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 2 2 0 3 0
                mkLi 3 3 0 4 0
                mkLi 1 1 0 2 0
            ]
            Topology2D.sortToLoop(getLn, xs)
            "sorted in chain order" |> Expect.equal (idxSeq xs) [0;1;2;3]
        }


        // -------------------------------------------------------------------
        // Topology2D.sortToLoopWithReversing
        // -------------------------------------------------------------------

        test "sortToLoopWithReversing: empty input is a no-op" {
            let xs = ResizeArray<Li>()
            let revInPlace (i:int) (_x:Li) =
                xs.[i] <- {xs.[i] with ln = Line2D(xs.[i].ln.To, xs.[i].ln.From)}
            Topology2D.sortToLoopWithReversing(getLn, revInPlace, xs)
            "still empty" |> Expect.equal xs.Count 0
        }

        test "sortToLoopWithReversing: single element is a no-op" {
            let xs = ResizeArray [ mkLi 7 0 0 1 0 ]
            let originalFrom = xs.[0].ln.From
            let originalTo   = xs.[0].ln.To
            let revInPlace (i:int) (_x:Li) =
                xs.[i] <- {xs.[i] with ln = Line2D(xs.[i].ln.To, xs.[i].ln.From)}
            Topology2D.sortToLoopWithReversing(getLn, revInPlace, xs)
            "still one element" |> Expect.equal xs.Count 1
            "same idx"          |> Expect.equal xs.[0].idx 7
            "orientation From untouched" |> Expect.equal (eq xs.[0].ln.From originalFrom) true
            "orientation To untouched"   |> Expect.equal (eq xs.[0].ln.To   originalTo)   true
        }

        test "sortToLoopWithReversing: scrambled chain with mixed orientations" {
            // Docstring: 'For each line end it finds the next closest start
            // point or end point.' Where the closest match is an end-point
            // the element is reversed in place via the supplied callback.
            //
            // Geometry (target chain): (0,0)-(1,0)-(2,0)-(3,0)-(4,0)
            //   seg A (idx 0): (0,0)->(1,0)    natural
            //   seg B (idx 1): (2,0)->(1,0)    reversed orientation
            //   seg C (idx 2): (2,0)->(3,0)    natural
            //   seg D (idx 3): (4,0)->(3,0)    reversed orientation
            //
            // After sort starting from A, B must be flipped to (1,0)->(2,0),
            // C is already aligned, D must be flipped to (3,0)->(4,0).
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 2 2 0 3 0
                mkLi 3 4 0 3 0
                mkLi 1 2 0 1 0
            ]
            let revInPlace (i:int) (_x:Li) =
                let cur = xs.[i]
                xs.[i] <- {cur with ln = Line2D(cur.ln.To, cur.ln.From)}
            Topology2D.sortToLoopWithReversing(getLn, revInPlace, xs)
            "idx order" |> Expect.equal (idxSeq xs) [0;1;2;3]
            "seg A start" |> Expect.equal (eq xs.[0].ln.From (Pt(0., 0.))) true
            "seg A end"   |> Expect.equal (eq xs.[0].ln.To   (Pt(1., 0.))) true
            "seg B start (flipped)" |> Expect.equal (eq xs.[1].ln.From (Pt(1., 0.))) true
            "seg B end (flipped)"   |> Expect.equal (eq xs.[1].ln.To   (Pt(2., 0.))) true
            "seg C start" |> Expect.equal (eq xs.[2].ln.From (Pt(2., 0.))) true
            "seg C end"   |> Expect.equal (eq xs.[2].ln.To   (Pt(3., 0.))) true
            "seg D start (flipped)" |> Expect.equal (eq xs.[3].ln.From (Pt(3., 0.))) true
            "seg D end (flipped)"   |> Expect.equal (eq xs.[3].ln.To   (Pt(4., 0.))) true
        }


        // -------------------------------------------------------------------
        // Special cases: duplicates, zero-length, Y-junctions, splitDistance
        // extremes, near-tolerance matches, big inputs.
        // -------------------------------------------------------------------

        test "join: exact duplicate line creates a second singleton group" {
            // Two identical (0,0)->(1,0) lines cannot tail-chain (neither
            // end matches the other's start). The first becomes the head of
            // a one-element group; the duplicate must form its own group.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 0 0 1 0
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "two groups"   |> Expect.equal gs.Count 2
            "group 0 idx"  |> Expect.equal (idxSeq gs.[0]) [0]
            "group 1 idx"  |> Expect.equal (idxSeq gs.[1]) [1]
        }

        test "join: tail-to-tail pair cannot chain without reversal" {
            // L0 ends at (1,0); L1 also ends at (1,0). The only matching
            // endpoint is To<->To, which join (no reversal) refuses. They
            // therefore land in separate groups; joinReversing can merge
            // them — see the corresponding test below.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 2 0 1 0
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "two groups" |> Expect.equal gs.Count 2
            "group 0" |> Expect.equal (idxSeq gs.[0]) [0]
            "group 1" |> Expect.equal (idxSeq gs.[1]) [1]
        }

        test "join: head-to-head pair cannot chain without reversal" {
            // L0 starts at (0,0); L1 also starts at (0,0). The only matching
            // endpoint is From<->From, which join (no reversal) refuses.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 0 0 0 1
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "two groups" |> Expect.equal gs.Count 2
        }

        test "join: reversed duplicate chains in forward direction" {
            // (0,0)->(1,0) and (1,0)->(0,0) — although geometrically a
            // mirror image, L1.From=(1,0) DOES equal L0.To=(1,0), so join
            // can chain them tail-to-head as ordinary forward segments,
            // without needing to know about reversal. The chain effectively
            // doubles back on itself.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 1 0 0 0
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "single combined group" |> Expect.equal gs.Count 1
            "chain order" |> Expect.equal (idxSeq gs.[0]) [0;1]
        }

        test "joinReversing: exact duplicate line chains via end-to-end match" {
            // For joinReversing the duplicate's end (1,0) matches the head's
            // end (1,0), so the duplicate is flagged reversed and added to
            // the forward chain. Single combined group expected.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 0 0 1 0
            ]
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "one chain"            |> Expect.equal gs.Count 1
            "duplicate is reversed"|> Expect.equal (idxRevSeq gs.[0]) [(0,false);(1,true)]
        }

        test "joinReversing: reversed duplicate is picked up forward" {
            // (1,0)->(0,0) is the natural reverse of (0,0)->(1,0). The
            // forward search finds From=(1,0) matching head's To=(1,0) and
            // adds the line unreversed.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 1 0 0 0
            ]
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "one chain" |> Expect.equal gs.Count 1
            "reversed duplicate added unreversed"
                |> Expect.equal (idxRevSeq gs.[0]) [(0,false);(1,false)]
        }

        test "join: Y-junction is order-sensitive; first match wins" {
            // Three lines share the point (1,0): the main chain L0->L1 and a
            // branch L2 going up. join is greedy and picks the first
            // tail-matching line it sees; with input order [L0, L1, L2] the
            // chain takes L1 and L2 falls out as its own group.
            let xs = ResizeArray [
                mkLi 0  0 0  1 0      // (0,0) -> (1,0)
                mkLi 1  1 0  2 0      // (1,0) -> (2,0) — main chain continuation
                mkLi 2  1 0  1 1      // (1,0) -> (1,1) — branch
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "two groups"            |> Expect.equal gs.Count 2
            "main chain captured"   |> Expect.equal (idxSeq gs.[0]) [0;1]
            "branch is singleton"   |> Expect.equal (idxSeq gs.[1]) [2]
        }

        test "join: Y-junction with branch first swallows branch into chain" {
            // Same geometry as above but with the branch listed before the
            // main continuation. The greedy first-match rule means the chain
            // now takes the branch L2 and the original continuation L1
            // becomes the singleton group — explicit evidence that join's
            // behaviour at junctions depends on input order.
            let xs = ResizeArray [
                mkLi 0  0 0  1 0
                mkLi 2  1 0  1 1      // branch first
                mkLi 1  1 0  2 0
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "two groups"               |> Expect.equal gs.Count 2
            "branch taken by chain"    |> Expect.equal (idxSeq gs.[0]) [0;2]
            "continuation is orphaned" |> Expect.equal (idxSeq gs.[1]) [1]
        }

        test "join: two scrambled closed loops produce exactly two groups" {
            // Two unrelated unit squares whose segments are interleaved in
            // the input. Each loop must come back as its own group of four
            // segments in head-to-tail order.
            let xs = ResizeArray [
                // Square A around origin
                mkLi 0  0 0  1 0
                // Square B offset by (10,0)
                mkLi 4 10 0 11 0
                mkLi 1  1 0  1 1
                mkLi 5 11 0 11 1
                mkLi 2  1 1  0 1
                mkLi 6 11 1 10 1
                mkLi 3  0 1  0 0
                mkLi 7 10 1 10 0
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "two loops" |> Expect.equal gs.Count 2
            "loop A order" |> Expect.equal (idxSeq gs.[0]) [0;1;2;3]
            "loop B order" |> Expect.equal (idxSeq gs.[1]) [4;5;6;7]
        }

        test "join: tolerance accepts small floating-point gaps between segments" {
            // A real-world geometry rarely has bit-exact endpoints. With
            // splitDistance=1e-3 a gap of 1e-6 between consecutive segments
            // must still be considered the same chain.
            let xs = ResizeArray [
                mkLi 0 0.0      0.0  1.0      0.0
                mkLi 1 1.000001 0.0  2.0      0.0
                mkLi 2 1.999999 0.0  3.0      0.0
            ]
            let gs = Topology2D.join(getLn, 1e-3, xs)
            "one chain" |> Expect.equal gs.Count 1
            "all three in order" |> Expect.equal (idxSeq gs.[0]) [0;1;2]
        }

        test "join: zero splitDistance keeps only bit-exact matches together" {
            // splitDistance = 0 means even a one-bit gap starts a new group.
            // Here all three segments have exact integer endpoints that
            // touch, so they should still chain together.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 1 0 2 0
                mkLi 2 2 0 3 0
            ]
            let gs = Topology2D.join(getLn, 0.0, xs)
            "one chain by exact match" |> Expect.equal gs.Count 1
            "ordered head-to-tail" |> Expect.equal (idxSeq gs.[0]) [0;1;2]
        }

        test "join: zero splitDistance and any FP gap forces split" {
            // With splitDistance = 0 a sub-epsilon gap is enough to start a
            // new group. This guards against accidental tolerance leaking
            // into the comparison.
            let xs = ResizeArray [
                mkLi 0 0.0      0.0  1.0      0.0
                mkLi 1 1.000001 0.0  2.0      0.0
            ]
            let gs = Topology2D.join(getLn, 0.0, xs)
            "two singleton groups" |> Expect.equal gs.Count 2
        }

        test "join: huge splitDistance merges unrelated segments by proximity" {
            // When splitDistance dwarfs every gap, the algorithm chains
            // whatever it finds first. The two horizontal segments are 10
            // units apart but well within splitDistance=100, so they end up
            // in a single group.
            let xs = ResizeArray [
                mkLi 0 0 0  1 0
                mkLi 1 11 0 12 0
            ]
            let gs = Topology2D.join(getLn, 100.0, xs)
            "merged into one group" |> Expect.equal gs.Count 1
            "order preserved"       |> Expect.equal (idxSeq gs.[0]) [0;1]
        }

        test "join: zero-length segment is treated as a connection point" {
            // A degenerate (0,0)->(0,0) segment has identical From/To.
            // Both endpoints of the chain match that point, so the
            // zero-length segment must be picked up by either forward or
            // backward search and end up in the same group.
            let xs = ResizeArray [
                mkLi 0 0 0  1 0    // (0,0) -> (1,0); chain start at (0,0)
                mkLi 1 0 0  0 0    // zero-length at (0,0)
            ]
            let gs = Topology2D.join(getLn, 0.001, xs)
            "single combined group" |> Expect.equal gs.Count 1
            "contains both elements" |> Expect.equal gs.[0].Count 2
        }

        test "join: long pre-ordered chain (50 segments) stays as one group" {
            // Stress test: a sequence of 50 unit segments laid head-to-tail
            // must reduce to a single group of length 50, with order
            // preserved.
            let n = 50
            let xs = ResizeArray<Li>()
            for k in 0 .. n - 1 do
                xs.Add (mkLi k k 0 (k + 1) 0)
            let gs = Topology2D.join(getLn, 0.001, xs)
            "single group" |> Expect.equal gs.Count 1
            "length matches input" |> Expect.equal gs.[0].Count n
            "indices in order"     |> Expect.equal (idxSeq gs.[0]) [for k in 0 .. n - 1 -> k]
        }

        test "joinReversing: two scrambled square loops with mixed orientations" {
            // Same idea as the join two-loop test, but every other segment
            // is oriented backwards. joinReversing must still recover two
            // groups of four segments each.
            let xs = ResizeArray [
                // Square A — alternating orientations
                mkLi 0  0 0  1 0      // natural
                mkLi 1  1 1  1 0      // reversed: shares (1,0) with seg 0
                mkLi 2  1 1  0 1      // natural
                mkLi 3  0 0  0 1      // reversed: shares (0,1) with seg 2 and (0,0) with seg 0
                // Square B — same pattern, offset by (10,0)
                mkLi 4 10 0 11 0
                mkLi 5 11 1 11 0
                mkLi 6 11 1 10 1
                mkLi 7 10 0 10 1
            ]
            let gs = Topology2D.joinReversing(getLn, 0.001, xs)
            "two groups" |> Expect.equal gs.Count 2
            "loop A length" |> Expect.equal gs.[0].Count 4
            "loop B length" |> Expect.equal gs.[1].Count 4
        }

        test "joinReversing: tolerance accepts small gaps just like join" {
            // Same numeric setup as the join tolerance test. joinReversing
            // should behave identically when the natural orientation
            // already chains correctly.
            let xs = ResizeArray [
                mkLi 0 0.0      0.0  1.0      0.0
                mkLi 1 1.000001 0.0  2.0      0.0
                mkLi 2 1.999999 0.0  3.0      0.0
            ]
            let gs = Topology2D.joinReversing(getLn, 1e-3, xs)
            "one chain" |> Expect.equal gs.Count 1
            "all unreversed in order"
                |> Expect.equal (idxRevSeq gs.[0]) [(0,false);(1,false);(2,false)]
        }

        test "sortToLoop: preserves element count and contains every input" {
            // Independent of ordering, sortToLoop must not lose or duplicate
            // any element — it is documented to be an in-place reorder.
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 1 1 0 1 1
                mkLi 2 1 1 0 1
                mkLi 3 0 1 0 0
                mkLi 4 2 2 3 2
                mkLi 5 3 2 3 3
            ]
            let (_:unit) = Topology2D.sortToLoop(getLn, xs)
            "count preserved" |> Expect.equal xs.Count 6
            let idxs = idxSeq xs |> List.sort
            "all original indices present" |> Expect.equal idxs [0;1;2;3;4;5]
        }

        test "sortToLoop: scrambled closed loop of 5 segments" {
            // A pentagon-shaped closed loop (just integer coords for
            // simplicity) scrambled in input. The greedy nearest-start
            // algorithm walks from the FIRST input element (idx=2) along
            // the chain, so the recovered order starts at idx 2 and wraps
            // around: 2 -> 3 -> 4 -> 0 -> 1.
            let xs = ResizeArray [
                mkLi 2  2 1  1 2
                mkLi 0  0 0  2 0
                mkLi 4  0 1  0 0
                mkLi 1  2 0  2 1
                mkLi 3  1 2  0 1
            ]
            let (_:unit) = Topology2D.sortToLoop(getLn, xs)
            "loop order recovered from start element" |> Expect.equal (idxSeq xs) [2;3;4;0;1]
        }

        test "sortToLoopWithReversing: long polyline with random orientations" {
            // A 6-segment chain where every other segment is reversed. After
            // sorting, the in-place reversal callback should leave the
            // array as a clean head-to-tail polyline (0,0) -> (6,0).
            let xs = ResizeArray [
                mkLi 0 0 0 1 0       // natural
                mkLi 2 3 0 2 0       // reversed (target span (2,0)->(3,0))
                mkLi 4 5 0 4 0       // reversed
                mkLi 1 2 0 1 0       // reversed
                mkLi 3 3 0 4 0       // natural
                mkLi 5 5 0 6 0       // natural
            ]
            let revInPlace (i:int) (_x:Li) =
                let cur = xs.[i]
                xs.[i] <- {cur with ln = Line2D(cur.ln.To, cur.ln.From)}
            let (_:unit) = Topology2D.sortToLoopWithReversing(getLn, revInPlace, xs)
            "idx in chain order" |> Expect.equal (idxSeq xs) [0;1;2;3;4;5]
            for k in 0 .. 5 do
                let expFrom = Pt(float k,     0.)
                let expTo   = Pt(float (k+1), 0.)
                sprintf "seg %d From" k |> Expect.equal (eq xs.[k].ln.From expFrom) true
                sprintf "seg %d To"   k |> Expect.equal (eq xs.[k].ln.To   expTo)   true
        }

    ]


// ---------------------------------------------------------------------------
// Topology3D — mirrors the 2D suite above but with genuinely 3D geometry so the
// Z component of the distance is exercised (not just X/Y). Chains run along the
// diagonal Pk = (k, 0, k); the closed loop uses a non-planar quadrilateral.
// ---------------------------------------------------------------------------

let tests3D =
    testList "Topology3D" [

        // -------------------------------------------------------------------
        // Topology3D.join
        // -------------------------------------------------------------------

        test "join3D: empty input returns empty result" {
            let xs = ResizeArray<Li3>()
            let gs = Topology3D.join(getLn3, 0.001, xs)
            "no groups for empty input" |> Expect.equal gs.Count 0
        }

        test "join3D: single element returns one group of one" {
            let xs = ResizeArray [ mkLi3 0  0 0 0  1 1 1 ]
            let gs = Topology3D.join(getLn3, 0.001, xs)
            "exactly one group" |> Expect.equal gs.Count 1
            "group has one element" |> Expect.equal gs.[0].Count 1
            "preserves the element" |> Expect.equal gs.[0].[0].idx 0
        }

        test "join3D: two disconnected elements form two singleton groups" {
            let xs = ResizeArray [
                mkLi3 0  0 0 0   1 0 0
                mkLi3 1 10 0 0  11 0 0
            ]
            let gs = Topology3D.join(getLn3, 0.1, xs)
            "two singleton groups" |> Expect.equal gs.Count 2
            "first group is element 0" |> Expect.equal (idx3Seq gs.[0]) [0]
            "second group is element 1" |> Expect.equal (idx3Seq gs.[1]) [1]
        }

        test "join3D: pre-ordered open polyline along a 3D diagonal stays in one group" {
            let xs = ResizeArray [
                mkLi3 0  0 0 0  1 1 1
                mkLi3 1  1 1 1  2 2 2
                mkLi3 2  2 2 2  3 3 3
            ]
            let gs = Topology3D.join(getLn3, 0.001, xs)
            "one chain"   |> Expect.equal gs.Count 1
            "chain order" |> Expect.equal (idx3Seq gs.[0]) [0;1;2]
        }

        test "join3D: scrambled chain reorders into a single polyline" {
            // Chain Pk = (k,0,k); starting element is the middle segment idx=2,
            // so idx 0,1 are added by backward search and idx 3,4 by forward.
            let xs = ResizeArray [
                mkLi3 2  2 0 2  3 0 3
                mkLi3 4  4 0 4  5 0 5
                mkLi3 0  0 0 0  1 0 1
                mkLi3 3  3 0 3  4 0 4
                mkLi3 1  1 0 1  2 0 2
            ]
            let gs = Topology3D.join(getLn3, 0.001, xs)
            "one polyline" |> Expect.equal gs.Count 1
            "elements head-to-tail" |> Expect.equal (idx3Seq gs.[0]) [0;1;2;3;4]
        }

        test "join3D: non-planar closed quadrilateral forms one group of four" {
            // A=(0,0,0) B=(1,0,1) C=(1,1,2) D=(0,1,1) — a genuinely 3D loop.
            let xs = ResizeArray [
                mkLi3 0  0 0 0  1 0 1   // A->B
                mkLi3 1  1 0 1  1 1 2   // B->C
                mkLi3 2  1 1 2  0 1 1   // C->D
                mkLi3 3  0 1 1  0 0 0   // D->A
            ]
            let gs = Topology3D.join(getLn3, 0.001, xs)
            "one closed loop" |> Expect.equal gs.Count 1
            "sides in order"  |> Expect.equal (idx3Seq gs.[0]) [0;1;2;3]
        }

        test "join3D: distance exactly at splitDistance keeps elements together" {
            // 3-4-5 triangle keeps the boundary distance exact in floating point:
            // seg0 ends at (1,0,0); seg1 starts at (1,3,4) — distance exactly 5.0.
            let xs = ResizeArray [
                mkLi3 0  0 0 0   1 0 0
                mkLi3 1  1 3 4   1 6 8
            ]
            let gs = Topology3D.join(getLn3, 5.0, xs)
            "joined in one group"  |> Expect.equal gs.Count 1
            "ordered head-to-tail" |> Expect.equal (idx3Seq gs.[0]) [0;1]
        }

        test "join3D: distance just over splitDistance splits the group" {
            // Same geometry (gap of exactly 5.0) but splitDistance 4.9 < 5.0.
            let xs = ResizeArray [
                mkLi3 0  0 0 0   1 0 0
                mkLi3 1  1 3 4   1 6 8
            ]
            let gs = Topology3D.join(getLn3, 4.9, xs)
            "two singleton groups" |> Expect.equal gs.Count 2
            "first group"  |> Expect.equal (idx3Seq gs.[0]) [0]
            "second group" |> Expect.equal (idx3Seq gs.[1]) [1]
        }


        // -------------------------------------------------------------------
        // Topology3D.joinReversing
        // -------------------------------------------------------------------

        test "joinReversing3D: single element returns one group of one, not reversed" {
            let xs = ResizeArray [ mkLi3 0  0 0 0  1 1 1 ]
            let gs = Topology3D.joinReversing(getLn3, 0.001, xs)
            "exactly one group" |> Expect.equal gs.Count 1
            "not reversed" |> Expect.equal (idxRev3Seq gs.[0]) [(0, false)]
        }

        test "joinReversing3D: forward search reverses an end-to-end line" {
            // seg1's To coincides with seg0's To, so it must be flagged reversed.
            let xs = ResizeArray [
                mkLi3 0  0 0 1   1 0 1   // ends at (1,0,1)
                mkLi3 1  2 5 9   1 0 1   // also ends at (1,0,1)
            ]
            let gs = Topology3D.joinReversing(getLn3, 0.001, xs)
            "one chain" |> Expect.equal gs.Count 1
            "second element reversed" |> Expect.equal (idxRev3Seq gs.[0]) [(0,false);(1,true)]
        }

        test "joinReversing3D: backward search reverses a head-to-head line" {
            // seg1's From coincides with seg0's From, so it is reversed and prepended.
            let xs = ResizeArray [
                mkLi3 0  1 0 2   2 0 2   // starts at (1,0,2)
                mkLi3 1  1 0 2   0 9 9   // also starts at (1,0,2)
            ]
            let gs = Topology3D.joinReversing(getLn3, 0.001, xs)
            "one chain" |> Expect.equal gs.Count 1
            "reversed element first" |> Expect.equal (idxRev3Seq gs.[0]) [(1,true);(0,false)]
        }

        test "joinReversing3D: mixed orientations form one polyline" {
            // Chain Pk=(k,0,k): A natural, B reversed, C natural, D reversed.
            let xs = ResizeArray [
                mkLi3 0  0 0 0   1 0 1   // P0->P1
                mkLi3 1  2 0 2   1 0 1   // P2->P1 (reversed)
                mkLi3 2  2 0 2   3 0 3   // P2->P3
                mkLi3 3  4 0 4   3 0 3   // P4->P3 (reversed)
            ]
            let gs = Topology3D.joinReversing(getLn3, 0.001, xs)
            "one polyline" |> Expect.equal gs.Count 1
            "expected orientation flags" |> Expect.equal
                (idxRev3Seq gs.[0]) [(0,false);(1,true);(2,false);(3,true)]
        }

        test "joinReversing3D: distance exactly at splitDistance keeps elements together" {
            let xs = ResizeArray [
                mkLi3 0  0 0 0   1 0 0
                mkLi3 1  1 3 4   1 6 8   // starts 5.0 away from previous end
            ]
            let gs = Topology3D.joinReversing(getLn3, 5.0, xs)
            "joined in one group" |> Expect.equal gs.Count 1
            "ordered head-to-tail" |> Expect.equal
                (idxRev3Seq gs.[0]) [(0,false);(1,false)]
        }


        // -------------------------------------------------------------------
        // Topology3D.sortToLoop
        // -------------------------------------------------------------------

        test "sortToLoop3D: empty input is a no-op" {
            let xs = ResizeArray<Li3>()
            Topology3D.sortToLoop(getLn3, xs)
            "still empty" |> Expect.equal xs.Count 0
        }

        test "sortToLoop3D: scrambled non-planar quad sorts into loop order" {
            let xs = ResizeArray [
                mkLi3 0  0 0 0  1 0 1   // A->B
                mkLi3 2  1 1 2  0 1 1   // C->D
                mkLi3 3  0 1 1  0 0 0   // D->A
                mkLi3 1  1 0 1  1 1 2   // B->C
            ]
            Topology3D.sortToLoop(getLn3, xs)
            "sorted in loop order" |> Expect.equal (idx3Seq xs) [0;1;2;3]
        }


        // -------------------------------------------------------------------
        // Topology3D.sortToLoopWithReversing
        // -------------------------------------------------------------------

        test "sortToLoopWithReversing3D: scrambled chain with mixed orientations" {
            // Chain Pk=(k,0,k). After sorting from A, every reversed segment is
            // flipped in place so the array becomes a clean head-to-tail polyline.
            let xs = ResizeArray [
                mkLi3 0  0 0 0  1 0 1   // P0->P1 natural
                mkLi3 2  2 0 2  3 0 3   // P2->P3 natural
                mkLi3 3  4 0 4  3 0 3   // P4->P3 reversed
                mkLi3 1  2 0 2  1 0 1   // P2->P1 reversed
            ]
            let revInPlace3 (i:int) (_x:Li3) =
                let cur = xs.[i]
                xs.[i] <- {cur with ln3 = Line3D(cur.ln3.To, cur.ln3.From)}
            Topology3D.sortToLoopWithReversing(getLn3, revInPlace3, xs)
            "idx order" |> Expect.equal (idx3Seq xs) [0;1;2;3]
            for k in 0 .. 3 do
                let expFrom = Pnt(float k,     0., float k)
                let expTo   = Pnt(float (k+1), 0., float (k+1))
                sprintf "seg %d From" k |> Expect.equal (eq3 xs.[k].ln3.From expFrom) true
                sprintf "seg %d To"   k |> Expect.equal (eq3 xs.[k].ln3.To   expTo)   true
        }

    ]


// ---------------------------------------------------------------------------
// Cached sort variants (2D and 3D). These exercise the parallel float-array
// bookkeeping: the returned cache must stay coordinate-consistent with the
// reordered (and, for the reversing variants, reversed) elements.
// ---------------------------------------------------------------------------

let testsCached =
    testList "Topology cached" [

        test "sortToLoopCached 2D: scrambled square sorts into loop order with consistent cache" {
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 2 1 1 0 1
                mkLi 3 0 1 0 0
                mkLi 1 1 0 1 1
            ]
            let cache = Topology2D.sortToLoopCached(getLn, xs)
            "loop order"        |> Expect.equal (idxSeq xs) [0;1;2;3]
            "cache has 4 floats per line" |> Expect.equal cache.Length (xs.Count * 4)
            "cache consistent with elements" |> Expect.equal (cache2DMatches xs cache) true
        }

        test "sortToLoopWithReversingCached 2D: mixed orientations sorted with consistent cache" {
            let xs = ResizeArray [
                mkLi 0 0 0 1 0
                mkLi 2 3 0 2 0
                mkLi 4 5 0 4 0
                mkLi 1 2 0 1 0
                mkLi 3 3 0 4 0
                mkLi 5 5 0 6 0
            ]
            let revInPlace (i:int) (_x:Li) =
                let cur = xs.[i]
                xs.[i] <- {cur with ln = Line2D(cur.ln.To, cur.ln.From)}
            let cache = Topology2D.sortToLoopWithReversingCached(getLn, revInPlace, xs)
            "idx in chain order" |> Expect.equal (idxSeq xs) [0;1;2;3;4;5]
            "cache consistent with reversed elements" |> Expect.equal (cache2DMatches xs cache) true
            for k in 0 .. 5 do
                let expFrom = Pt(float k,     0.)
                let expTo   = Pt(float (k+1), 0.)
                sprintf "seg %d From" k |> Expect.equal (eq xs.[k].ln.From expFrom) true
                sprintf "seg %d To"   k |> Expect.equal (eq xs.[k].ln.To   expTo)   true
        }

        test "sortToLoopCached 3D: scrambled non-planar quad sorts into loop order with consistent cache" {
            let xs = ResizeArray [
                mkLi3 0  0 0 0  1 0 1   // A->B
                mkLi3 2  1 1 2  0 1 1   // C->D
                mkLi3 3  0 1 1  0 0 0   // D->A
                mkLi3 1  1 0 1  1 1 2   // B->C
            ]
            let cache = Topology3D.sortToLoopCached(getLn3, xs)
            "loop order"        |> Expect.equal (idx3Seq xs) [0;1;2;3]
            "cache has 6 floats per line" |> Expect.equal cache.Length (xs.Count * 6)
            "cache consistent with elements" |> Expect.equal (cache3DMatches xs cache) true
        }

        test "sortToLoopWithReversingCached 3D: mixed orientations sorted with consistent cache" {
            let xs = ResizeArray [
                mkLi3 0  0 0 0  1 0 1   // P0->P1 natural
                mkLi3 2  2 0 2  3 0 3   // P2->P3 natural
                mkLi3 3  4 0 4  3 0 3   // P4->P3 reversed
                mkLi3 1  2 0 2  1 0 1   // P2->P1 reversed
            ]
            let revInPlace3 (i:int) (_x:Li3) =
                let cur = xs.[i]
                xs.[i] <- {cur with ln3 = Line3D(cur.ln3.To, cur.ln3.From)}
            let cache = Topology3D.sortToLoopWithReversingCached(getLn3, revInPlace3, xs)
            "idx order" |> Expect.equal (idx3Seq xs) [0;1;2;3]
            "cache consistent with reversed elements" |> Expect.equal (cache3DMatches xs cache) true
            for k in 0 .. 3 do
                let expFrom = Pnt(float k,     0., float k)
                let expTo   = Pnt(float (k+1), 0., float (k+1))
                sprintf "seg %d From" k |> Expect.equal (eq3 xs.[k].ln3.From expFrom) true
                sprintf "seg %d To"   k |> Expect.equal (eq3 xs.[k].ln3.To   expTo)   true
        }

    ]
