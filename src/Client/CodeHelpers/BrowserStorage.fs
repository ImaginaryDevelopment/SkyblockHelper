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
                    json.parse(s)
                    |> unbox
                result
            )

    let trySave (key:string) value : Result<unit,string>  =
        printfn "trying to save"
        try
            // let pojo = Fable.Core.JsInterop.toPlainJsObj value
            let serial =
                match value with
                | Some value -> json.stringify(value)
                | None -> null
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

type IProperty<'t when 't : equality> =
    abstract member Value: 't option with get,set

type IHierarchyFactory =
    abstract member Create<'t2 when 't2 : equality> : unit -> IHierarchyAccess<'t2>
and IHierarchyAccess<'t when 't : equality> =
    inherit IProperty<'t>
    abstract member MakeBaby : string -> IHierarchyFactory

module Factory =
    let createProperty (isa:IStorageAccess) =
        let f key =
            let storage = isa.Create key
            let mutable value = storage.Get()
            {new IProperty<'t> with
                member __.Value
                    with get() = value
                    and set v = value <- v
            }
        f
    type HierarchyAccess<'t when 't : equality> private (isa,key,f) =
        let prop = createProperty isa key
        static member CreateMe isa key f =
            HierarchyAccess(isa,key,f)
        interface IProperty<'t> with
            member __.Value
                with get() = prop.Value
                and set v = prop.Value <- v
        interface IHierarchyAccess<'t> with
            member __.MakeBaby subkey =
                f isa (key + "_" + subkey)
    let rec makeFactory (isa:IStorageAccess) key :IHierarchyFactory=
        {
            new IHierarchyFactory with
                member __.Create<'t when 't : equality>() =
                    let ha = HierarchyAccess<'t>.CreateMe isa key makeFactory
                    upcast ha
        }
    
    let unify isa key: IHierarchyAccess<'t> =
        let ha = HierarchyAccess.CreateMe isa key makeFactory
        upcast ha

// let rec createHA key (isa:IStorageAccess) =
//     let storage = isa.Create key
//     {
//         new IHierarchyAccess<'t> with
//             member _.Value
//                 with get() = value
//                 and set v = value <- v
//             member __.Create (key2): IHierarchyAccess<_> =
//                 createHA (key + "_" + key2) isa
//     }

// type StorageComponent<'t when 't : equality> (key,isa:IStorageAccess) =
//     let storage = isa.Create key
//     let mutable value: 't option = storage.Get()
//     // new(iSA,key) =
//     //     StorageComponent<'t>(key=key,isa=iSA)

//     member __.Value
//         with get() = value
//         and set v =
//             match storage.Save v with
//             | Ok () ->
//                 value <- v
//             | _ -> ()
//     member __.Create key =
//         // let x = DewIt2It(isa,key)
//         let x = StorageComponent(key,isa)
//         x :> StorageComponent<_>
//     // interface IDewIt<'t> with
//     //     // member __.Create<'tChild when 'tChild : equality> key=
//     //     member __.Create key=
//     //         // let x = DewIt2It(isa,key)
//     //         let x = StorageComponent(isa,key)
//     //         x :> IDewIt<'tChild>



// type StorageComponent<'t when 't : equality>(key, isa:IStorageAccess) =
//     let storage = isa.Create<'t> key
//     let mutable value = storage.Get()
//     // member __.Create(subkey) =
//     //     IAmADirtyHack.Create(key + subkey, isa, StorageComponent<'tChild>)
//     member __.Value
//         with get() = value
//         and set v =
//             match storage.Save v with
//             | Ok () ->
//                 value <- v
//             | _ -> ()
//     interface IAmADirtyHack<'t> with
//         member __.Create<'tChild when 'tChild : equality>(subkey) : IAmADirtyHack<'tChild> =
//             upcast StorageComponent(key + subkey, isa)
