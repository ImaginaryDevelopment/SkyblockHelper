module BrowserStorage
open Fable.Import
open Browser.Types
open Fable.Core.Util
open Fable.Core.JsInterop

// for debugging
let toGlobal (name:string) value =
    Browser.Dom.self?(name) <- value
open Fable.Core.DynamicExtensions
// type JSON =
//     abstract member stringify: 't -> string
//     abstract member parse: string -> 't option
// let json: JSON = jsNative
// type Window with
//     member __.JSON:JSON = json
module Internal =
    let private localStorage = Browser.Dom.self.localStorage
    let private json = Fable.Core.JS.JSON
    let tryGet<'t when 't : equality > (key) : 't option =
        printfn "Getting %s from storage" key
        localStorage.getItem key
        |> Option.ofObj
        |> Option.map (fun x ->
            printfn "Get finished"
            x |> unbox
            |> fun s ->
                printfn "Found %s -> %s" key s
                let result:'t =
                    json.parse(s)
                    |> unbox
                let verify () =
                    let reserial = result |> json.stringify
                    if reserial = s then
                        printfn "test 1 is ok"
                    else printfn "test 1 fail"
                    let reParse = reserial |> json.parse |> unbox
                    if reParse = result then
                        printfn "test 2 is ok"
                    else
                        eprintfn "test 2 fail"
                        eprintfn "reparse : %s" <| json.stringify reParse
                        eprintfn "result: %s" <| json.stringify result
                // verify()
                result
            )

    let trySave (key:string) value: Result<unit,string>  =
        try
            // let pojo = Fable.Core.JsInterop.toPlainJsObj value
            let serial = json.stringify(value)
            // let serial = json.stringify pojo

            localStorage.setItem(key,serial)
            |> Ok
        with ex ->
            toGlobal "self" Browser.Dom.self
            Error(ex.Message)
// assumes we never want to clear a key entirely
type StorageAccess<'t> = {Get: unit -> 't option; Save: 't -> Result<unit,string>}

let createStorage<'t when 't : equality > name =
    let getter () = Internal.tryGet<'t> name
    let saver (x:'t) = Internal.trySave name x
    {   Get= getter
        Save= saver
    }

// keys should probably only be simple types
// module MapStorage =
//     let get () =
//         let result =
//             storage.Get() |> Option.defaultValue Map.empty
//         printfn "Got from storage map: %A" (result |> Map.toSeq |> Seq.map fst |> List.ofSeq)
//         result

//     let add storage key value =
//         get storage ()
//         |> Map.add key value
//         |> storage.Save
//     let remove storage key =
//         get storage ()
//         |> Map.remove key
//         |> storage.Save

// perf? -> in the interest of not writing a singleton or enforcing one, we'll fetch from localstorage on each operation
type LookupStorage<'tvalue when 'tvalue : equality >(key) =
    let storage : StorageAccess<(string*'tvalue)[]> = createStorage key

    member __.Get():Map<string,'tvalue>=
        storage.Get()
        |> Option.defaultValue Array.empty
        |> Map.ofArray
    member __.ToGlobal() =
        storage.Get()
        |> toGlobal (sprintf "%sMap" key)

    member x.TryFind key: 'tvalue option =
        x.Get()
        |> Map.tryFind key

    member x.Save(key,value) =
        x.Get()
        |> Map.add key value
        |> Map.toArray
        |> storage.Save
    member x.Remove key =
        x.Get()
        |> Map.remove key
        |> Map.toArray
        |> storage.Save

