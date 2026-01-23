module TestResizeArr

open Euclid
open Euclid.EuclidCollectionUtilities

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let tests =
    testList "ResizeArr" [

        // ==============================================================
        // Arr module tests
        // ==============================================================

        test "Arr.minIndex returns index of smallest element" {
            let arr = [| 5; 3; 8; 1; 7 |]
            Expect.equal (Arr.minIndex arr) 3 "index of 1"
        }

        test "Arr.minIndex single element" {
            let arr = [| 42 |]
            Expect.equal (Arr.minIndex arr) 0 "single element"
        }

        test "Arr.minIndex first element is smallest" {
            let arr = [| 1; 2; 3 |]
            Expect.equal (Arr.minIndex arr) 0 "first is smallest"
        }

        test "Arr.minIndex last element is smallest" {
            let arr = [| 3; 2; 1 |]
            Expect.equal (Arr.minIndex arr) 2 "last is smallest"
        }

        test "Arr.minIndex duplicate minimums returns first" {
            let arr = [| 5; 1; 3; 1; 7 |]
            Expect.equal (Arr.minIndex arr) 1 "first occurrence of min"
        }

        test "Arr.minIndex fails on empty array" {
            Expect.throws (fun () -> Arr.minIndex [||] |> ignore) "empty array fails"
        }

        test "Arr.maxIndex returns index of biggest element" {
            let arr = [| 5; 3; 8; 1; 7 |]
            Expect.equal (Arr.maxIndex arr) 2 "index of 8"
        }

        test "Arr.maxIndex single element" {
            let arr = [| 42 |]
            Expect.equal (Arr.maxIndex arr) 0 "single element"
        }

        test "Arr.maxIndex first element is largest" {
            let arr = [| 9; 2; 3 |]
            Expect.equal (Arr.maxIndex arr) 0 "first is largest"
        }

        test "Arr.maxIndex last element is largest" {
            let arr = [| 1; 2; 9 |]
            Expect.equal (Arr.maxIndex arr) 2 "last is largest"
        }

        test "Arr.maxIndex duplicate maximums returns first" {
            let arr = [| 5; 9; 3; 9; 7 |]
            Expect.equal (Arr.maxIndex arr) 1 "first occurrence of max"
        }

        test "Arr.maxIndex fails on empty array" {
            Expect.throws (fun () -> Arr.maxIndex [||] |> ignore) "empty array fails"
        }

        // ==============================================================
        // ResizeArr module tests
        // ==============================================================

        test "ResizeArr.create creates array with count elements" {
            let r = ResizeArr.create 5 42
            Expect.equal r.Count 5 "5 elements"
            for i = 0 to 4 do
                Expect.equal r.[i] 42 $"element {i} is 42"
        }

        test "ResizeArr.create zero count" {
            let r = ResizeArr.create 0 99
            Expect.equal r.Count 0 "empty"
        }

        test "ResizeArr.length returns Count" {
            let r = ResizeArray<int>()
            r.Add(1); r.Add(2); r.Add(3)
            Expect.equal (ResizeArr.length r) 3 "length is 3"
        }

        test "ResizeArr.init creates with indices" {
            let r = ResizeArr.init 4 (fun i -> i * 2)
            Expect.equal r.Count 4 "4 elements"
            Expect.equal r.[0] 0 "0*2"
            Expect.equal r.[1] 2 "1*2"
            Expect.equal r.[2] 4 "2*2"
            Expect.equal r.[3] 6 "3*2"
        }

        test "ResizeArr.init zero count" {
            let r = ResizeArr.init 0 (fun i -> i)
            Expect.equal r.Count 0 "empty"
        }

        test "ResizeArr.singleton creates single element" {
            let r = ResizeArr.singleton "hello"
            Expect.equal r.Count 1 "one element"
            Expect.equal r.[0] "hello" "value"
        }

        test "ResizeArr.thisNext yields looped pairs" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let pairs = ResizeArr.thisNext r |> Seq.toList
            Expect.equal pairs.Length 3 "3 pairs"
            Expect.equal pairs.[0] (1, 2) "first pair"
            Expect.equal pairs.[1] (2, 3) "second pair"
            Expect.equal pairs.[2] (3, 1) "wrapping pair"
        }

        test "ResizeArr.thisNext input unchanged" {
            let r = ResizeArray<int>([| 10; 20; 30 |])
            let originalCount = r.Count
            let _ = ResizeArr.thisNext r |> Seq.toList
            Expect.equal r.Count originalCount "count unchanged"
            Expect.equal r.[0] 10 "first unchanged"
            Expect.equal r.[1] 20 "second unchanged"
            Expect.equal r.[2] 30 "third unchanged"
        }

        test "ResizeArr.thisNext fails on less than 3 items" {
            let r = ResizeArray<int>([| 1; 2 |])
            Expect.throws (fun () -> ResizeArr.thisNext r |> ignore) "needs 3+ items"
        }

        test "ResizeArr.iPrevThisNext yields looped tuples" {
            let r = ResizeArray<int>([| 1; 2; 3; 4 |])
            let tuples = ResizeArr.iPrevThisNext r |> Seq.toList
            Expect.equal tuples.Length 4 "4 tuples"
            Expect.equal tuples.[0] (0, 4, 1, 2) "first tuple with wrap"
            Expect.equal tuples.[1] (1, 1, 2, 3) "second tuple"
            Expect.equal tuples.[2] (2, 2, 3, 4) "third tuple"
            Expect.equal tuples.[3] (3, 3, 4, 1) "last tuple with wrap"
        }

        test "ResizeArr.iPrevThisNext input unchanged" {
            let r = ResizeArray<int>([| 1; 2; 3; 4 |])
            let originalValues = r |> Seq.toArray
            let _ = ResizeArr.iPrevThisNext r |> Seq.toList
            Expect.equal r.Count 4 "count unchanged"
            for i = 0 to 3 do
                Expect.equal r.[i] originalValues.[i] $"element {i} unchanged"
        }

        test "ResizeArr.iPrevThisNext fails on less than 4 items" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            Expect.throws (fun () -> ResizeArr.iPrevThisNext r |> ignore) "needs 4+ items"
        }

        test "ResizeArr.sortBy returns new sorted array" {
            let r = ResizeArray<int>([| 3; 1; 4; 1; 5 |])
            let sorted = ResizeArr.sortBy id r
            Expect.equal sorted.Count 5 "same count"
            Expect.equal sorted.[0] 1 "sorted first"
            Expect.equal sorted.[1] 1 "sorted second"
            Expect.equal sorted.[2] 3 "sorted third"
            Expect.equal sorted.[3] 4 "sorted fourth"
            Expect.equal sorted.[4] 5 "sorted fifth"
        }

        test "ResizeArr.sortBy input unchanged" {
            let r = ResizeArray<int>([| 3; 1; 4; 1; 5 |])
            let originalValues = r |> Seq.toArray
            let _ = ResizeArr.sortBy id r
            Expect.equal r.Count 5 "count unchanged"
            for i = 0 to 4 do
                Expect.equal r.[i] originalValues.[i] $"element {i} unchanged"
        }

        test "ResizeArr.sortBy with projection" {
            let r = ResizeArray<string>([| "ccc"; "a"; "bb" |])
            let sorted = ResizeArr.sortBy String.length r
            Expect.equal sorted.[0] "a" "shortest first"
            Expect.equal sorted.[1] "bb" "medium second"
            Expect.equal sorted.[2] "ccc" "longest last"
        }

        test "ResizeArr.sortBy empty array" {
            let r = ResizeArray<int>()
            let sorted = ResizeArr.sortBy id r
            Expect.equal sorted.Count 0 "empty remains empty"
        }

        test "ResizeArr.sortBy single element" {
            let r = ResizeArray<int>([| 42 |])
            let sorted = ResizeArr.sortBy id r
            Expect.equal sorted.Count 1 "single element"
            Expect.equal sorted.[0] 42 "value preserved"
        }

        test "ResizeArr.tryFindIndex finds element" {
            let r = ResizeArray<int>([| 1; 2; 3; 4; 5 |])
            let idx = ResizeArr.tryFindIndex (fun x -> x = 3) r
            Expect.equal idx (Some 2) "found at index 2"
        }

        test "ResizeArr.tryFindIndex returns None when not found" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let idx = ResizeArr.tryFindIndex (fun x -> x = 99) r
            Expect.equal idx None "not found"
        }

        test "ResizeArr.tryFindIndex empty array" {
            let r = ResizeArray<int>()
            let idx = ResizeArr.tryFindIndex (fun _ -> true) r
            Expect.equal idx None "empty returns None"
        }

        test "ResizeArr.tryFindIndex returns first match" {
            let r = ResizeArray<int>([| 1; 2; 2; 3 |])
            let idx = ResizeArr.tryFindIndex (fun x -> x = 2) r
            Expect.equal idx (Some 1) "first occurrence"
        }

        test "ResizeArr.findIndex finds element" {
            let r = ResizeArray<int>([| 1; 2; 3; 4; 5 |])
            let idx = ResizeArr.findIndex (fun x -> x = 4) r
            Expect.equal idx 3 "found at index 3"
        }

        test "ResizeArr.findIndex returns -1 when not found" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let idx = ResizeArr.findIndex (fun x -> x = 99) r
            Expect.equal idx -1 "not found"
        }

        test "ResizeArr.find finds element" {
            let r = ResizeArray<int>([| 1; 2; 3; 4; 5 |])
            let el = ResizeArr.find (fun x -> x > 3) r
            Expect.equal el 4 "first element > 3"
        }

        test "ResizeArr.findLastIndex finds last match" {
            let r = ResizeArray<int>([| 1; 2; 3; 2; 1 |])
            let idx = ResizeArr.findLastIndex (fun x -> x = 2) r
            Expect.equal idx 3 "last occurrence at index 3"
        }

        test "ResizeArr.findLastIndex returns -1 when not found" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let idx = ResizeArr.findLastIndex (fun x -> x = 99) r
            Expect.equal idx -1 "not found"
        }

        test "ResizeArr.findLastIndex empty array" {
            let r = ResizeArray<int>()
            let idx = ResizeArr.findLastIndex (fun _ -> true) r
            Expect.equal idx -1 "empty returns -1"
        }

        test "ResizeArr.rev returns reversed array" {
            let r = ResizeArray<int>([| 1; 2; 3; 4; 5 |])
            let reversed = ResizeArr.rev r
            Expect.equal reversed.Count 5 "same count"
            Expect.equal reversed.[0] 5 "first is last"
            Expect.equal reversed.[1] 4 "second"
            Expect.equal reversed.[2] 3 "third"
            Expect.equal reversed.[3] 2 "fourth"
            Expect.equal reversed.[4] 1 "fifth is first"
        }

        test "ResizeArr.rev input unchanged" {
            let r = ResizeArray<int>([| 1; 2; 3; 4; 5 |])
            let originalValues = r |> Seq.toArray
            let _ = ResizeArr.rev r
            Expect.equal r.Count 5 "count unchanged"
            for i = 0 to 4 do
                Expect.equal r.[i] originalValues.[i] $"element {i} unchanged"
        }

        test "ResizeArr.rev empty array" {
            let r = ResizeArray<int>()
            let reversed = ResizeArr.rev r
            Expect.equal reversed.Count 0 "empty"
        }

        test "ResizeArr.rev single element" {
            let r = ResizeArray<int>([| 42 |])
            let reversed = ResizeArr.rev r
            Expect.equal reversed.Count 1 "one element"
            Expect.equal reversed.[0] 42 "value preserved"
        }

        test "ResizeArr.minIndexBy returns index of min by projection" {
            let r = ResizeArray<string>([| "aaa"; "b"; "cc" |])
            let idx = ResizeArr.minIndexBy String.length r
            Expect.equal idx 1 "shortest string at index 1"
        }

        test "ResizeArr.minIndexBy single element" {
            let r = ResizeArray<int>([| 99 |])
            let idx = ResizeArr.minIndexBy id r
            Expect.equal idx 0 "single element"
        }

        test "ResizeArr.minIndexBy fails on empty" {
            let r = ResizeArray<int>()
            Expect.throws (fun () -> ResizeArr.minIndexBy id r |> ignore) "empty fails"
        }

        test "ResizeArr.minIndexBy duplicate minimums returns first" {
            let r = ResizeArray<string>([| "aa"; "b"; "c"; "dd" |])
            let idx = ResizeArr.minIndexBy String.length r
            Expect.equal idx 1 "first min"
        }

        test "ResizeArr.maxIndexBy returns index of max by projection" {
            let r = ResizeArray<string>([| "a"; "bbb"; "cc" |])
            let idx = ResizeArr.maxIndexBy String.length r
            Expect.equal idx 1 "longest string at index 1"
        }

        test "ResizeArr.maxIndexBy single element" {
            let r = ResizeArray<int>([| 99 |])
            let idx = ResizeArr.maxIndexBy id r
            Expect.equal idx 0 "single element"
        }

        test "ResizeArr.maxIndexBy fails on empty" {
            let r = ResizeArray<int>()
            Expect.throws (fun () -> ResizeArr.maxIndexBy id r |> ignore) "empty fails"
        }

        test "ResizeArr.maxIndexBy duplicate maximums returns first" {
            let r = ResizeArray<string>([| "a"; "bb"; "cc"; "d" |])
            let idx = ResizeArr.maxIndexBy String.length r
            Expect.equal idx 1 "first max"
        }

        test "ResizeArr.map transforms elements" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let mapped = ResizeArr.map (fun x -> x * 10) r
            Expect.equal mapped.Count 3 "same count"
            Expect.equal mapped.[0] 10 "first"
            Expect.equal mapped.[1] 20 "second"
            Expect.equal mapped.[2] 30 "third"
        }

        test "ResizeArr.map input unchanged" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let originalValues = r |> Seq.toArray
            let _ = ResizeArr.map (fun x -> x * 10) r
            Expect.equal r.Count 3 "count unchanged"
            for i = 0 to 2 do
                Expect.equal r.[i] originalValues.[i] $"element {i} unchanged"
        }

        test "ResizeArr.map empty array" {
            let r = ResizeArray<int>()
            let mapped = ResizeArr.map (fun x -> x * 10) r
            Expect.equal mapped.Count 0 "empty"
        }

        test "ResizeArr.map type conversion" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let mapped = ResizeArr.map (fun x -> float x * 10.0) r
            Expect.equal mapped.[0] 10.0 "converted to float"
            Expect.equal mapped.[1] 20.0 "converted to float"
            Expect.equal mapped.[2] 30.0 "converted to float"
        }

        test "ResizeArr.closeLoop adds first to end" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let closed = ResizeArr.closeLoop r
            Expect.equal closed.Count 4 "one more element"
            Expect.equal closed.[0] 1 "first"
            Expect.equal closed.[1] 2 "second"
            Expect.equal closed.[2] 3 "third"
            Expect.equal closed.[3] 1 "last is copy of first"
        }

        test "ResizeArr.closeLoop input unchanged" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let originalValues = r |> Seq.toArray
            let _ = ResizeArr.closeLoop r
            Expect.equal r.Count 3 "count unchanged"
            for i = 0 to 2 do
                Expect.equal r.[i] originalValues.[i] $"element {i} unchanged"
        }

        test "ResizeArr.closeLoop single element" {
            let r = ResizeArray<int>([| 42 |])
            let closed = ResizeArr.closeLoop r
            Expect.equal closed.Count 2 "two elements"
            Expect.equal closed.[0] 42 "first"
            Expect.equal closed.[1] 42 "last is first"
        }

        test "ResizeArr.tryFindBack finds last match" {
            let r = ResizeArray<int>([| 1; 2; 3; 2; 1 |])
            let result = ResizeArr.tryFindBack (fun x -> x = 2) r
            Expect.equal result (Some 2) "found 2"
        }

        test "ResizeArr.tryFindBack returns None when not found" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let result = ResizeArr.tryFindBack (fun x -> x = 99) r
            Expect.equal result None "not found"
        }

        test "ResizeArr.tryFindBack empty array" {
            let r = ResizeArray<int>()
            let result = ResizeArr.tryFindBack (fun _ -> true) r
            Expect.equal result None "empty returns None"
        }

        test "ResizeArr.tryFindBack input unchanged" {
            let r = ResizeArray<int>([| 1; 2; 3; 4; 5 |])
            let originalValues = r |> Seq.toArray
            let _ = ResizeArr.tryFindBack (fun x -> x > 2) r
            Expect.equal r.Count 5 "count unchanged"
            for i = 0 to 4 do
                Expect.equal r.[i] originalValues.[i] $"element {i} unchanged"
        }

        // ==============================================================
        // Extension members tests
        // ==============================================================

        test "LastIndex returns Count - 1" {
            let r = ResizeArray<int>([| 1; 2; 3; 4; 5 |])
            Expect.equal r.LastIndex 4 "last index is 4"
        }

        test "Last get returns last element" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            Expect.equal r.Last 3 "last is 3"
        }

        test "Last set modifies last element" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            r.Last <- 99
            Expect.equal r.[2] 99 "last modified to 99"
        }

        test "SecondLast get returns second last" {
            let r = ResizeArray<int>([| 1; 2; 3; 4 |])
            Expect.equal r.SecondLast 3 "second last is 3"
        }

        test "SecondLast set modifies second last" {
            let r = ResizeArray<int>([| 1; 2; 3; 4 |])
            r.SecondLast <- 99
            Expect.equal r.[2] 99 "second last modified"
        }

        test "ThirdLast get returns third last" {
            let r = ResizeArray<int>([| 1; 2; 3; 4; 5 |])
            Expect.equal r.ThirdLast 3 "third last is 3"
        }

        test "ThirdLast set modifies third last" {
            let r = ResizeArray<int>([| 1; 2; 3; 4; 5 |])
            r.ThirdLast <- 99
            Expect.equal r.[2] 99 "third last modified"
        }

        test "First get returns first element" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            Expect.equal r.First 1 "first is 1"
        }

        test "First set modifies first element" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            r.First <- 99
            Expect.equal r.[0] 99 "first modified to 99"
        }

        test "Second get returns second element" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            Expect.equal r.Second 2 "second is 2"
        }

        test "Second set modifies second element" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            r.Second <- 99
            Expect.equal r.[1] 99 "second modified to 99"
        }

        test "Third get returns third element" {
            let r = ResizeArray<int>([| 1; 2; 3; 4 |])
            Expect.equal r.Third 3 "third is 3"
        }

        test "Third set modifies third element" {
            let r = ResizeArray<int>([| 1; 2; 3; 4 |])
            r.Third <- 99
            Expect.equal r.[2] 99 "third modified to 99"
        }

        test "Pop removes and returns last element" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            let popped = r.Pop()
            Expect.equal popped 3 "popped is 3"
            Expect.equal r.Count 2 "count reduced"
            Expect.equal r.[0] 1 "first unchanged"
            Expect.equal r.[1] 2 "second unchanged"
        }

        test "Pop with index removes and returns element" {
            let r = ResizeArray<int>([| 1; 2; 3; 4 |])
            let popped = r.Pop(1)
            Expect.equal popped 2 "popped is 2"
            Expect.equal r.Count 3 "count reduced"
            Expect.equal r.[0] 1 "first unchanged"
            Expect.equal r.[1] 3 "shifted"
            Expect.equal r.[2] 4 "shifted"
        }

        test "GetLooped with positive index" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            Expect.equal (r.GetLooped 0) 1 "index 0"
            Expect.equal (r.GetLooped 1) 2 "index 1"
            Expect.equal (r.GetLooped 2) 3 "index 2"
        }

        test "GetLooped wraps positive overflow" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            Expect.equal (r.GetLooped 3) 1 "wraps to 0"
            Expect.equal (r.GetLooped 4) 2 "wraps to 1"
            Expect.equal (r.GetLooped 5) 3 "wraps to 2"
        }

        test "GetLooped wraps negative index" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            Expect.equal (r.GetLooped -1) 3 "-1 is last"
            Expect.equal (r.GetLooped -2) 2 "-2 is second last"
            Expect.equal (r.GetLooped -3) 1 "-3 is first"
            Expect.equal (r.GetLooped -4) 3 "wraps around"
        }

        test "SetIdx sets element at index" {
            let r = ResizeArray<int>([| 1; 2; 3 |])
            r.SetIdx 1 99
            Expect.equal r.[1] 99 "element set"
        }

        test "IsEmpty true for empty" {
            let r = ResizeArray<int>()
            Expect.isTrue r.IsEmpty "empty is true"
        }

        test "IsEmpty false for non-empty" {
            let r = ResizeArray<int>([| 1 |])
            Expect.isFalse r.IsEmpty "non-empty is false"
        }
    ]
