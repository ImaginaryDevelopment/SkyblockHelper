module BrowserStorage
open Fable.Import
open Browser.Types
open Fable.Core.Util
open Fable.Core.JsInterop
open CodeHelpers.FableHelpers

module Internal =
    let private localStorage = Browser.Dom.self.localStorage
    let private json = Fable.Core.JS.JSON
    let tryGet<'t when 't : equality > (key) : 't option =
        localStorage.getItem key
        |> Option.ofObj
        |> Option.map (fun x ->
            x |> unbox
            |> fun s ->
                // printfn "Found %s -> %s" key s
                let result:'t =
                    // Thoth.Json.Decode.Auto.fromString s
                    json.parse(s)
                    |> unbox
                result
            )

    let trySave (key:string) (valueOpt: 't option) : Result<unit,string>  =
        printfn "trying to save"
        try
            // let pojo = Fable.Core.JsInterop.toPlainJsObj value
            let serial =
                match valueOpt with
                | Some (value: 't) ->
                    let stringy = json.stringify(value)
                    let nonstringy = json.parse(stringy) |> unbox
                    let restringy = json.stringify nonstringy
                    if restringy <> stringy then
                        eprintfn "Serialize to deserialize and back failed"
                    if nonstringy <> value then
                        eprintfn "Serialize fail"
                        Fable.Core.JS.console.log("old,new", value,nonstringy)
                    stringy
                | None -> null
            // let serial = json.stringify pojo
            printfn "Saving to key %s" key

            localStorage.setItem(key,serial)
            printfn "Saved -> %s" serial
            Ok ()
        with ex ->
            toGlobal "self" Browser.Dom.self
            Error(ex.Message)

// assumes we never want to clear a key entirely
type StorageAccess<'t> = {Get: unit -> 't option; Save: 't option -> Result<unit,string>}


let createStorage<'t when 't : equality > name =
    let getter () = Internal.tryGet<'t> name
    let saver (x:'t option) = Internal.trySave name x
    {   Get= getter
        Save= saver
    }

// perf? -> in the interest of not writing a singleton or enforcing one, we'll fetch from localstorage on each operation
type LookupStorage<'tvalue when 'tvalue : equality >(key) =
    let storage : StorageAccess<(string*'tvalue)[]> = createStorage key
    do
        toGlobal (sprintf "storage_%s" key) storage

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
        |> Some
        |> storage.Save
    member x.Remove key =
        x.Get()
        |> Map.remove key
        |> Map.toArray
        |> Some
        |> storage.Save

type IStorageAccess =
    abstract member Create<'t when 't : equality> : key:string -> StorageAccess<'t>
