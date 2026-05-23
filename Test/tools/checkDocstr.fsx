#r "nuget: Str"
#r "nuget: Fesher"
#r "nuget: ResizeArrayT"

open Str
open System
open Fesher
open System.IO
open ResizeArrayT


let files =
    [|
    Directory.GetFiles(@"D:\Git\_Euclid_\Euclid\Src", "*.fs")  |> Array.filter (fun f -> not (f.Contains "PolyLabel"))
    Directory.GetFiles(@"D:\Git\_Euclid_\Euclid\Src\TypeExtensions", "*.fs")
    |]
    |> Array.concat


//fsi.ShowDeclarationValues <- false
//fsi.ShowProperties <- false
//fsi.ShowIEnumerable <- false

let mutable isType = false
for file in files do
    isType <- false
    let lns = File.ReadAllLines(file)
    for i, ln in Seq.indexed lns do

        let l = ln.Trim()
        if l.StartsWith("member ") ||  l.StartsWith("static ")then
            if lns[i-1].DoesNotContain "///"
            && lns[i-1].DoesNotContain "[<DataMember>]"
            && lns[i-1].DoesNotContain "[<Obsolete"  then
                Printfn.red $"{lns[i-1]}"
                Printfn.red $"{lns[i]}"
                Printfn.gray $"{file}:{i+1}\n"




