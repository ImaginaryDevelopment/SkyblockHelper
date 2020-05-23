module BrowserStorage
open Fable.Import
open Browser.Types
open Fable.Core.Util
open Fable.Core.JsInterop

// for debugging
let toGlobal (name:string) value =
    printfn "Adding global %s" name
    Browser.Dom.self?(name) <- value

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
                    json.parse(s)
                    |> unbox
                result
            )

    let trySave (key:string) value : Result<unit,string>  =
        printfn "trying to save"
        try
            // let pojo = Fable.Core.JsInterop.toPlainJsObj value
            let serial = json.stringify(value)
            // let serial = json.stringify pojo
            printfn "Saving to key %s" key
            System.Diagnostics.Debugger.Break()

            localStorage.setItem(key,serial)
            printfn "Saved -> %s" serial
            Ok ()
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
        |> storage.Save
    member x.Remove key =
        x.Get()
        |> Map.remove key
        |> Map.toArray
        |> storage.Save

