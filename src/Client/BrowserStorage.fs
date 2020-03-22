module BrowserStorage
open Fable.Import
open Browser.Types
open Fable.Core.Util
type JSON =
    abstract member stringify: 't -> string
    abstract member parse: string -> 't option
type Window with
    member __.JSON:JSON = jsNative
let private localStorage = Browser.Dom.self.localStorage
let tryGet<'t> (key:string) : 't option =
    localStorage.getItem key
    |> Option.ofObj
    |> Option.bind (unbox >> Browser.Dom.self.JSON.parse)

let trySave (key:string) value: Result<unit,string>  =
    try
        localStorage.setItem(key,Browser.Dom.self.JSON.stringify value)
        |> Ok
    with ex ->
        Error(ex.Message)