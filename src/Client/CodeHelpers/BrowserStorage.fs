module BrowserStorage
open Fable.Import
open Browser.Types
open Fable.Core.Util
open Fable.Core.JsInterop
open CodeHelpers.FableHelpers
open Fable.Core

let private localStorage = Browser.Dom.self.localStorage

type Internal =
    // let private localStorage = Browser.Dom.self.localStorage
    // let private json = Fable.Core.JS.JSON
    static member tryGet<'t when 't : equality > (key,[<Inject>] ?resolver: ITypeResolver<'t>) : 't option =
        localStorage.getItem key
        |> Option.ofObj
        |> Option.bind (fun x ->
                // printfn "Found %s -> %s" key s
                let result:'t option =
                    // Thoth.Json.Decode.Auto.fromString s
                    Resolver.deserialize(x,resolver.Value)
                    // json.parse(s)
                    // |> unbox
                result
            )

    static member trySave (key:string, valueOpt: 't option, [<Inject>] ?resolver: ITypeResolver<'t>) : Result<unit,string>  =
        printfn "trying to save"
        try
            // let pojo = Fable.Core.JsInterop.toPlainJsObj value
            let serial =
                match valueOpt with
                | Some (value: 't) ->
                    let stringy = Resolver.serialize(value,resolver=resolver.Value)
                    // let nonstringy = json.parse(stringy) |> unbox
                    // let restringy = json.Resolver.serialize nonstringy
                    // if restringy <> stringy then
                    //     eprintfn "Serialize to deserialize and back failed"
                    // if nonstringy <> value then
                    //     eprintfn "Serialize fail"
                    //     Fable.Core.JS.console.log("old,new", value,nonstringy)
                    stringy
                | None -> null
            // let serial = json.Resolver.serialize pojo
            printfn "Saving to key %s" key

            localStorage.setItem(key,serial)
            printfn "Saved -> %s" serial
            Ok ()
        with ex ->
            toGlobal "self" Browser.Dom.self
            Error(ex.Message)

// assumes we never want to clear a key entirely
type StorageAccess<'t when 't : equality> = {Get: unit -> 't option; Save: 't option -> Result<unit,string>} with
    static member createStorage<'t when 't : equality > (name,[<Inject>] ?resolver: ITypeResolver<'t>) =
        let getter () = Internal.tryGet<'t>(name,resolver.Value)
        let saver (x:'t option) = Internal.trySave (name,x,resolver=resolver.Value)
        {   Get= getter
            Save= saver
        }

// perf? -> in the interest of not writing a singleton or enforcing one, we'll fetch from localstorage on each operation
type LookupStorage<'tvalue when 'tvalue : equality >(key) =
    let storage : StorageAccess<(string*'tvalue)[]> = StorageAccess.createStorage key
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
