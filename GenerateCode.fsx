// #load "src/Client/SkyblockHelper.fs"
// load DUs and create lists of their caseNames
#r @"src\Client\bin\Debug\netstandard2.0\Client.dll"

open Microsoft.FSharp.Reflection
open SkyblockHelper

type OptionBuilder() =
    member __.Bind(v,f) = Option.bind f v
    member __.Return v = Some v
    member __.ReturnFrom o = o
    member __.Zero () = None

let maybe = OptionBuilder()

let beforeOrSelf d =
    if System.String.IsNullOrEmpty d then failwithf "Delimiter was empty"
    function
    | null | "" as x -> x
    | x  ->
        let i = x.IndexOf d
        if i >=0 then
            x.[ .. i - 1]
        else x

let afterOrSelf d =
    if System.String.IsNullOrEmpty d then failwithf "Delimiter was empty"
    function
    | null | "" as x -> x
    | x ->
        let i = x.IndexOf d
        if i >= 0 then
            let j = i+d.Length
            let result = x.[j ..]
            // printfn "%s(%i) (%i -> %i)> %s(%i) = %s(%i)" x x.Length i j d d.Length result result.Length
            result
        else x

module Reflect =
    let getNamespace (t:System.Type) = // so incredibly hacky, but seems to work here
        System.IO.Path.GetFileNameWithoutExtension t.FullName

    let getUnionCaseNames (t:System.Type) =
        if not <| FSharpType.IsUnion t then
            None
        else // https://stackoverflow.com/questions/2181956/how-to-enumerate-an-enum-type-in-f
            FSharpType.GetUnionCases t
            |> Array.map(fun uc -> uc.Name,uc.GetFields() |> Seq.length)
            |> Some

    // take a type in the target namespace and find Unions
    let findUnionsInNamespace (tNamespace:System.Type) =
        let ns = getNamespace tNamespace
        // printfn "Search for types in namespace %s" ns
        tNamespace.Assembly.ExportedTypes
        |> Seq.filter(fun t -> t.FullName.StartsWith ns)

module Gen =
    // take a union generate the cases binding
    let generateUnionBinding initial indent ns u =
        maybe {
            let! cases = Reflect.getUnionCaseNames u
            // printfn "gen check %s -> %s" ns u.FullName
            let pname = u.FullName |> afterOrSelf ns |> afterOrSelf "."
            let name = pname |> afterOrSelf "+"
            let mname = pname |> beforeOrSelf "+"
            let cases = cases |> Seq.filter(fun (_,fieldCount) -> fieldCount < 1) |> Seq.map fst |> List.ofSeq
            if not <| List.isEmpty cases then
                return seq {
                    yield sprintf "%s// %s" initial u.FullName
                    yield sprintf "%slet %sCases =" initial name
                    yield sprintf "%s%s[" initial indent
                    yield! cases |> Seq.map(fun n -> sprintf "%s%s%s\"%s\", %s.%s.%s" initial indent indent n mname name n)
                    yield sprintf "%s%s]" initial indent
                }
            else ()
        }

    let generateUnionLists nsType =
        let ns = Reflect.getNamespace nsType
        let body =
            Reflect.findUnionsInNamespace nsType
            |> Seq.filter(fun t -> t.FullName |> Seq.filter (fun n -> n = '+') |> Seq.length < 2)
            |> Seq.choose(generateUnionBinding "" "  " ns)
            |> Seq.collect id
        seq {
            yield sprintf "module %s.Gen" ns
            yield "open SkyblockHelper"
            yield! body
        }
        |> String.concat System.Environment.NewLine

let text = Gen.generateUnionLists typeof<Resources.Resource>
System.IO.File.WriteAllText(@"src/Client/Generated.fs", text)
