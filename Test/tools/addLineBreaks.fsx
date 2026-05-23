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
    Directory.GetFiles(@"D:\Git\_Euclid_\Euclid\Src", "*.fs")
    Directory.GetFiles(@"D:\Git\_Euclid_\Euclid\Src\TypeExtensions", "*.fs")
    |]
    |> Array.concat


let getSpaces (s:string) =
    let rec loop (i:int) (c:int) =
        if i >= s.Length then c
        elif s.[i] = ' ' then loop (i + 1) (c + 1)
        else c
    String(' ', loop 0 0)



let mutable isType = false
for file in files do
    isType <- false
    let lns = File.ReadAllLines(file)
    let nls = ResizeArray<string>(lns.Length + 100)
    for i, ln in Seq.indexed lns do
        nls.Add ln
        if not isType && ln.Contains "[<DataContract>]" || ln.Contains "module AutoOpen" then
            isType <- true
            Printfn.purple $"{file}"

        let checkTrail (s:string)  =
            if s.Contains "=" then
                let b, a = s |> Str.splitOnce "="
                let r = a.Trim()
                if r.Length > 0 && not <| r.StartsWith "//" then
                    Printfn.red $"{s}"
                    nls.Pop()  |> ignore
                    nls.Add $"{b}= "
                    nls.Add $"{getSpaces s}    {r}"

        if isType then
            let l = ln.Trim()
            if l.StartsWith("member ") then
                let clean = l |> Str.delete "member " |> Str.delete "inline "
                //Printfn.gray $"  {clean}"
                checkTrail ln

            if l.StartsWith("static ") then
                let clean = l |> Str.delete "static member " |> Str.delete "inline "
                //Printfn.green $"  {clean}"
                checkTrail ln
                
        IO.File.WriteAllLines(file,  nls)



