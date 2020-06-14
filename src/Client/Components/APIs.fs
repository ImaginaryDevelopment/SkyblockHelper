module Components.Api

// https://hypixel.net/threads/skyblock-api.2942479/

open Fable.React
open Fable.React.Props
open Elmish
open Fulma

open Shared
open Shared.Helpers
open CodeHelpers.HypixelApi
open CodeHelpers.FableHelpers
open Components.SharedComponents

let useProxy = true

type Model = {
    Key: string
    Name: string // user name
    Uuid: string
    Loading: bool
    SkyblockProfile: string
}

type Msg =
    | FetchRequested
    | ClearRequested
    | ResultLoaded of Result<string,exn>
    | KeyChange of string
    | NameChange of string
    | UuidChange of string
    | SkyblockDataChange of string
    | SkyblockProfileLoadClick

let init initOverride : Model * Cmd<Msg> =
    initOverride |> Option.defaultValue {Key="";Name="";Uuid="";Loading=false;SkyblockProfile=""}, Cmd.none


let update msg model : Model * Cmd<Msg> =
        match msg with
        | SkyblockDataChange x ->
            {model with SkyblockProfile= x}, Cmd.none
        | SkyblockProfileLoadClick ->
            model.SkyblockProfile
            |> Option.ofValueString
            |> Option.map(Resolver.Deserialize)
            |> function
                | Some x ->
                    Fable.Core.JS.console.log("skyblock profile", x)
                    model, Cmd.none
                | None ->
                    eprintfn "Failed to deserialize skyblock profile"
                    model, Cmd.none

        | UuidChange x ->
            {model with Uuid=x}, Cmd.none
        | KeyChange x ->
            {model with Key=x}, Cmd.none
        | NameChange x ->
            {model with Name = x}, Cmd.none
        | ResultLoaded(Ok x) ->
            eprintfn "ApiLoaded?"
            Fable.Core.JS.console.log("ApiLoaded?",x)
            {model with Loading = false}, Cmd.none
        | ResultLoaded(Error ex) ->
            eprintfn "ApiLoaded?"
            Fable.Core.JS.console.log("ApiLoaded?",ex.Message)
            {model with Loading = false}, Cmd.none
        | ClearRequested ->
            {model with Loading = false}, Cmd.none
        | FetchRequested ->
            if model.Loading then
                eprintfn "Clicked while loading"
                model, Cmd.none
            else
                match model.Key, model.Uuid, model.Name with
                | ValueString k, ValueString u, _ ->
                    let f = Cmd.OfPromise.either (fetch useProxy) (getUrl (ApiReqType.HypixelSkyblockProfile(k,u))) (Ok >> Msg.ResultLoaded) ( Error >> Msg.ResultLoaded)
                    {model with Loading=true}, f

                | ValueString k, _, ValueString n ->
                    // let f = Cmd.OfPromise.either (CodeHelpers.HypixelAPI.fetchCharacter useProxy) (k,n) (Ok>>Msg.ResultLoaded) (Error>>Msg.ResultLoaded)
                    let f = Cmd.OfPromise.either (fetch useProxy) (getUrl(ApiReqType.HypixelProfile(k,n))) (Ok>>Msg.ResultLoaded) (Error>>Msg.ResultLoaded)
                    {model with Loading = true}, f

                // TODO: temp hack not a good idea, quick cors test
                | _, _, ValueString name ->
                    let url = getUrl(ApiReqType.MinecraftUuid (name,None))
                    let f = Cmd.OfPromise.either (fetch useProxy) url (Ok >> Msg.ResultLoaded) (Error >> Msg.ResultLoaded)
                    {model with Loading = true}, f
                | ValueString _, _,  _ ->
                    eprintfn "Api attempted without a name"
                    model, Cmd.none
                | _ , _, ValueString _ ->
                    eprintfn "Api attempted without a key"
                    model, Cmd.none
                | _ ->
                    eprintfn "Api attempted without a name or key"
                    model, Cmd.none
module Internals =
    let labeledColumn (label:string) elements =
        Columns.columns[] [
            Column.column [][
                unbox label
            ]
            Column.column [ Column.Option.Width(Screen.All, Column.IsThreeFifths) ] elements
        ]
    let taColumn dispatch label fMsg attr children =
        labeledColumn label [
            Fulma.Textarea.textarea (Textarea.OnChange (getEvValue >> fMsg >> dispatch)::attr) children
        ]

    let inputColumn dispatch label inputType fMsg value =
        labeledColumn label [
            Fulma.Input.input [Input.Option.Type inputType; Input.Option.DefaultValue value; Input.Option.OnChange (getEvValue>> fMsg >> dispatch)]
        ]

    let inline apiButton dispatch (label:string) fMsg =
        button [Class "button"; OnClick(fun _ -> fMsg |> dispatch)][
            unbox label
        ]

open Internals

let view (model:Model) (dispatch:Msg -> unit) =
    let inputColumn = inputColumn dispatch
    let apiButton = apiButton dispatch

    div[][
        inputColumn "Account Name" Input.IInputType.Text Msg.NameChange model.Name
        inputColumn "Minecraft Account Uuid" Input.IInputType.Text Msg.UuidChange model.Uuid
        inputColumn "Hypixel ApiKey" Input.IInputType.Password Msg.KeyChange model.Key
        apiButton "Fetch Hypixel profile" Msg.FetchRequested
        apiButton "Clear loading state" Msg.ClearRequested
        taColumn dispatch "Skyblock Info" Msg.SkyblockDataChange
            [Textarea.DefaultValue model.SkyblockProfile][]
        apiButton "Load Skyblock Profile" Msg.SkyblockProfileLoadClick
        hr []

        ul [](
            let lia (title:string) (link:string) =
                li [] [
                    a [Href link; Target "_blank"] [unbox title]
            ]

        // https://api.hypixel.net/Skyblock/profiles?key=[KEY]&uuid=[UUID]
        // param `at` is optional
        // https://api.mojang.com/users/profiles/minecraft/<username>?at=<timestamp>
            [
                match model.Key, model.Uuid with
                | ValueString k, ValueString u ->
                    yield lia "View Hypixel Skyblock data in new tab" <| getUrl (ApiReqType.HypixelSkyblockProfile(k,u))
                | _ ->
                    yield li [][
                        unbox "Hypixel data requires Hypixel Api Key and Minecraft Account Uuid"
                    ]
                match model.Key, model.Name with
                | ValueString k, ValueString n ->
                    yield lia "View Hypixel data in new tab" <| getUrl (ApiReqType.HypixelProfile(k, n))
                | _ ->
                    yield li [][
                        unbox "Hypixel data requires Hypixel Api Key and Account Name"
                    ]
                match model.Name with
                | ValueString n ->
                    yield lia "View Data in new tab" <| getUrl (ApiReqType.MinecraftUuid (n,None))
                | _ -> ()
            ]
        )
    ]