module CodeHelpers.FableHelpers
open Fable.Core.JS
open Fable.Core.JsInterop
open Shared

let stringify x = JSON.stringify x
let parse x = JSON.parse(x)


// for debugging
let inline toGlobal (name:string) value =
    printfn "Adding global %s" name
    Browser.Dom.self?(name) <- value
    ()
// let addDays (x:Date,days:int):Date =
//     let date = Constructors.Date.Create(x.valueOf())
//     date.AddDays(days)

let pascal (x:string) =
    (string x.[0]).ToUpper() + x.[1..]

let formatNumber (num:float,places: int option): string =
    let places = Option.defaultValue 2 places
    if isNull <| box num then "null"
    elif isNaN num then "NaN"
    else num.ToString("n" + string places)

let getAttrValue name (x:Browser.Types.HTMLElement) =
    if not <| isNull x.attributes then
        x.attributes.getNamedItem name
        |> Option.ofObj
        |> Option.bind (fun x -> x.value |> Option.ofObj)
    else None

let getTargetAsHtml (ev:Browser.Types.Event) =
    Option.ofObj ev
    |> Option.bind (fun ev -> Option.ofObj ev.target)
    |> Option.map(box >> unbox<Browser.Types.HTMLElement>)

let getName (ev:Browser.Types.Event) =
    match getTargetAsHtml ev with
    | Some target ->
        getAttrValue"data-name" target
        |> Option.orElseWith (fun () -> getAttrValue "name" target)
        |> Option.defaultValue ""
    | None -> ""
let getValue(ev:Browser.Types.Event) =
    match getTargetAsHtml ev with
    | Some target ->
        getAttrValue"value" target
        |> Option.defaultValue ""
    | None -> ""


// let getTargetInfo =

let getTargetName title ev =
    try
        let name = getName ev
        Ok name 
    with ex ->
        console.error(title + ".getTargetName")
        Error ex.Message

let getTargetValue title (ev:Browser.Types.Event):string option =
    try
        console.log("getTargetValue.target", ev.target)
        let value =
            // https://stackoverflow.com/questions/55093894/how-to-add-the-selected-attribute-to-a-select-option-in-fable
            ev.target?value
            |> Option.ofObj
        value
    with e ->
        console.error(title + ".getTargetValue",e)
        None

let toggleArrayValue (source: _[],target) =
    if source |> FSharp.Collections.Array.contains target then
        source |> FSharp.Collections.Array.filter(fun x -> x <> target)
    else source |> FSharp.Collections.Array.append ([| target |])
let toggleListValue (source: _ list) target =
    if source |> List.contains target then
        source |> List.filter(fun x -> x <> target)
    else target::source

// module StorageHelp =
//     let mutable logggedStorageFailure = false

//     type StorageAccess< 't > = {
//         GetIsLocalStorageAvailable: unit -> bool
//         // option for clearing the value
//         StoreIt: 't option -> unit
//         ReadIt: unit -> 't option
//         GetKeys: unit -> string list
//     }

//     let getIsLocalStorageAvailable () =
//         not <| isNull Browser.WebStorage.localStorage && not <| isNull (box Browser.WebStorage.localStorage.setItem)
//     let private setItem key value = 
//         Browser.WebStorage.localStorage.setItem(key,value)
//     let private getItem key = 
//         Browser.WebStorage.localStorage.getItem(key)
//         |> Option.ofValueString

//     let createStorageAccess key =
//         {
//             GetIsLocalStorageAvailable= getIsLocalStorageAvailable
//             StoreIt= fun (value:'t option) ->
//                 if getIsLocalStorageAvailable() then
//                     match value with
//                     | Some x ->
//                         stringify x
//                     | None ->
//                         null
//                     |> setItem key
//             ReadIt= fun () ->
//                 getItem key
//                 |> Option.map(JSON.parse >> (fun x -> x :?> 't))
//             GetKeys= fun () ->
//                 [ 0 .. Browser.WebStorage.localStorage.length - 1]
//                 |> List.map (float >> Browser.WebStorage.localStorage.key)
//         }
