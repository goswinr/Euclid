module TestSimilarity2D

open Euclid

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let inline eqPt a b = Pt.distance a b < 1e-9

let tol = Accuracy.veryHigh

let tests =
    testList "Similarity2D" [

        test "areSimilar returns true for identical objects" {
            let pts1 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1)]))])
            let obj1 = Similarity2D.getSimilarityData(pts1)
            let obj2 = Similarity2D.getSimilarityData(pts1)
            "identical objects are similar" |> Expect.isTrue (Similarity2D.areSimilar 0.01 obj1 obj2)
        }

        test "areSimilar returns false for different sized objects" {
            let pts1 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))])
            let pts2 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(2,0)]))])
            let obj1 = Similarity2D.getSimilarityData(pts1)
            let obj2 = Similarity2D.getSimilarityData(pts2)
            "different sized objects are not similar" |> Expect.isFalse (Similarity2D.areSimilar 0.01 obj1 obj2)
        }

        test "areSimilar returns true for shifted objects within tolerance" {
            let pts1 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1)]))])
            let pts2 = ResizeArray([("cat1", ResizeArray([Pt(10,10); Pt(11,10); Pt(11,11)]))])
            let obj1 = Similarity2D.getSimilarityData(pts1)
            let obj2 = Similarity2D.getSimilarityData(pts2)
            "shifted objects are similar (normalized to origin)" |> Expect.isTrue (Similarity2D.areSimilar 0.01 obj1 obj2)
        }

        test "areSimilar returns false for different shapes" {
            let pts1 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0); Pt(1,1)]))])
            let pts2 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0); Pt(0,1)]))])
            let obj1 = Similarity2D.getSimilarityData(pts1)
            let obj2 = Similarity2D.getSimilarityData(pts2)
            "different shapes are not similar" |> Expect.isFalse (Similarity2D.areSimilar 0.01 obj1 obj2)
        }

        test "areSimilar handles multiple categories" {
            let pts1 = ResizeArray([
                ("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))
                ("cat2", ResizeArray([Pt(2,0); Pt(3,0)]))
            ])
            let pts2 = ResizeArray([
                ("cat1", ResizeArray([Pt(10,10); Pt(11,10)]))
                ("cat2", ResizeArray([Pt(12,10); Pt(13,10)]))
            ])
            let obj1 = Similarity2D.getSimilarityData(pts1)
            let obj2 = Similarity2D.getSimilarityData(pts2)
            "objects with multiple categories are similar" |> Expect.isTrue (Similarity2D.areSimilar 0.01 obj1 obj2)
        }

        test "areSimilar returns false if category count differs" {
            let pts1 = ResizeArray([
                ("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))
            ])
            let pts2 = ResizeArray([
                ("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))
                ("cat2", ResizeArray([Pt(2,0); Pt(3,0)]))
            ])
            let obj1 = Similarity2D.getSimilarityData(pts1)
            let obj2 = Similarity2D.getSimilarityData(pts2)
            "different category counts are not similar" |> Expect.isFalse (Similarity2D.areSimilar 0.01 obj1 obj2)
        }

        test "areSimilar returns false if category names differ" {
            let pts1 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))])
            let pts2 = ResizeArray([("cat2", ResizeArray([Pt(0,0); Pt(1,0)]))])
            let obj1 = Similarity2D.getSimilarityData(pts1)
            let obj2 = Similarity2D.getSimilarityData(pts2)
            "different category names are not similar" |> Expect.isFalse (Similarity2D.areSimilar 0.01 obj1 obj2)
        }

        test "getSimilarityData normalizes to origin" {
            let pts = ResizeArray([("cat1", ResizeArray([Pt(10,10); Pt(11,10); Pt(11,11)]))])
            let obj = Similarity2D.getSimilarityData(pts)
            let grp = obj.groups.[0]
            // After normalization, min point should be near origin
            "first point X >= 0" |> Expect.isTrue (grp.points.[0].X >= 0.0)
            "first point Y >= 0" |> Expect.isTrue (grp.points.[0].Y >= 0.0)
        }

        test "getSimilarityData sorts points by X" {
            let pts = ResizeArray([("cat1", ResizeArray([Pt(2,0); Pt(0,0); Pt(1,0)]))])
            let obj = Similarity2D.getSimilarityData(pts)
            let grp = obj.groups.[0]
            // Points should be sorted by X
            "first point has smallest X" |> Expect.isTrue (grp.points.[0].X <= grp.points.[1].X)
            "second point X <= third point X" |> Expect.isTrue (grp.points.[1].X <= grp.points.[2].X)
        }

        test "getGrouped groups similar items" {
            let items = ResizeArray([1; 2; 3; 4])
            let sims = ResizeArray([
                Similarity2D.getSimilarityData(ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))]))
                Similarity2D.getSimilarityData(ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))]))
                Similarity2D.getSimilarityData(ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(2,0)]))]))
                Similarity2D.getSimilarityData(ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))]))
            ])
            let groups = Similarity2D.getGrouped(0.01, items, sims)
            "two groups created" |> Expect.equal groups.Count 2
            // Items 1, 2, and 4 should be in one group (similar)
            // Item 3 should be in another group (different)
            let group1Size = groups.[0].Count
            let group2Size = groups.[1].Count
            "groups have correct sizes" |> Expect.isTrue ((group1Size = 3 && group2Size = 1) || (group1Size = 1 && group2Size = 3))
        }

        test "getGrouped with all unique items" {
            let items = ResizeArray([1; 2; 3])
            let sims = ResizeArray([
                Similarity2D.getSimilarityData(ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))]))
                Similarity2D.getSimilarityData(ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(2,0)]))]))
                Similarity2D.getSimilarityData(ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(3,0)]))]))
            ])
            let groups = Similarity2D.getGrouped(0.01, items, sims)
            "three unique groups" |> Expect.equal groups.Count 3
        }

        test "getGrouped with all identical items" {
            let items = ResizeArray([1; 2; 3])
            let simData = Similarity2D.getSimilarityData(ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))]))
            let sims = ResizeArray([simData; simData; simData])
            let groups = Similarity2D.getGrouped(0.01, items, sims)
            "one group for identical items" |> Expect.equal groups.Count 1
            "group contains all items" |> Expect.equal groups.[0].Count 3
        }

        test "getGrouped throws on count mismatch" {
            let items = ResizeArray([1; 2])
            let sims = ResizeArray([
                Similarity2D.getSimilarityData(ResizeArray([("cat1", ResizeArray([Pt(0,0)]))]))
            ])
            let f() = Similarity2D.getGrouped(0.01, items, sims) |> ignore
            "throws on count mismatch" |> Expect.throws f
        }

        test "areSimilar with tolerance" {
            let pts1 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))])
            let pts2 = ResizeArray([("cat1", ResizeArray([Pt(0.005, 0.005); Pt(1, 0)]))])
            let obj1 = Similarity2D.getSimilarityData(pts1)
            let obj2 = Similarity2D.getSimilarityData(pts2)
            "similar within tolerance 0.01" |> Expect.isTrue (Similarity2D.areSimilar 0.01 obj1 obj2)
            "not similar with tolerance 0.001" |> Expect.isFalse (Similarity2D.areSimilar 0.001 obj1 obj2)
        }

        test "areSimilar with different point counts" {
            let pts1 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0)]))])
            let pts2 = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(1,0); Pt(2,0)]))])
            let obj1 = Similarity2D.getSimilarityData(pts1)
            let obj2 = Similarity2D.getSimilarityData(pts2)
            "different point counts are not similar" |> Expect.isFalse (Similarity2D.areSimilar 0.01 obj1 obj2)
        }

        test "getSimilarityData creates correct bounding rectangle" {
            let pts = ResizeArray([("cat1", ResizeArray([Pt(0,0); Pt(3,4)]))])
            let obj = Similarity2D.getSimilarityData(pts)
            // After normalization to origin, extent should represent the size
            "extent X is 3" |> Expect.floatClose tol obj.extend.X 3.0
            "extent Y is 4" |> Expect.floatClose tol obj.extend.Y 4.0
        }

        test "getSimilarityData handles single point" {
            let pts = ResizeArray([("cat1", ResizeArray([Pt(5,5)]))])
            let obj = Similarity2D.getSimilarityData(pts)
            "single point object created" |> Expect.equal obj.groups.Length 1
            "single point in group" |> Expect.equal obj.groups.[0].points.Length 1
        }

        test "areSimilar with empty point sets throws" {
            let pts1 = ResizeArray([("cat1", ResizeArray<Pt>([]))])
            let pts2 = ResizeArray([("cat1", ResizeArray<Pt>([]))])
            "empty point sets throw" |> Expect.throws ( fun () ->
                let obj1 = Similarity2D.getSimilarityData(pts1)
                let obj2 = Similarity2D.getSimilarityData(pts2)
                Similarity2D.areSimilar 0.01 obj1 obj2
                |> ignore
                )
        }
    ]
