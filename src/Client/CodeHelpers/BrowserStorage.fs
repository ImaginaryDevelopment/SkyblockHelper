module BrowserStorage
open Fable.Import
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
type StorageAccess<'t when 't : equality >(name) =
    static member createStorage (name) = StorageAccess(name)
    member _.Get([<Inject>] ?resolver: ITypeResolver<'t>) =  Internal.tryGet<'t>(name,resolver.Value)
    member _.Save(x:'t option,[<Inject>] ?resolver: ITypeResolver<'t>) = Internal.trySave (name,x,resolver=resolver.Value)

// perf? -> in the interest of not writing a singleton or enforcing one, we'll fetch from localstorage on each operation
type LookupStorage<'tvalue when 'tvalue : equality >(key) =
    let storage : StorageAccess<(string*'tvalue)[]> = StorageAccess.createStorage key
    do
        toGlobal (sprintf "storage_%s" key) storage

    member inline __.Get():Map<string,'tvalue>=
        storage.Get()
        |> Option.defaultValue Array.empty
        |> Map.ofArray
    member inline __.ToGlobal() =
        storage.Get()
        |> toGlobal (sprintf "%sMap" key)

    member inline x.TryFind key: 'tvalue option =
        x.Get()
        |> Map.tryFind key

    member inline x.Save(key,value) =
        x.Get()
        |> Map.add key value
        |> Map.toArray
        |> Some
        |> storage.Save
    member inline x.Remove key =
        x.Get()
        |> Map.remove key
        |> Map.toArray
        |> Some
        |> storage.Save

type IStorageAccess =
    abstract member Create<'t when 't : equality> : key:string -> StorageAccess<'t>
