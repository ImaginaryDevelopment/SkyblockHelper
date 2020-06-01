module Client
// plans:
// profile fetcher for damage calc

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.Import
open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fulma

open Shared
open Shared.Helpers
open CodeHelpers.FableHelpers


open SkyblockHelper
open Components.SharedComponents
open Components.SharedComponents.TabLink

let private debug = false

type Component =
    | Bazaar
    | Brewing
    | Enchanting
    // | Events
    // | Minions
    | Collections
    | Damage
    | ApiExperiment

    with
        static member All =
            [
                ApiExperiment
                Bazaar
                Brewing
                Enchanting
                // Events
                // Minions
                Collections
                Damage
            ]

type ApiState = {
    Key: string
    Name: string
    Loading: bool
}

type ComponentStates = {
    Api : ApiState
    Bazaar: Components.Bazaar.Model
    Brewing: Components.Brewing.Model
    Enchanting: Components.Enchanting.Model
    Collections: Components.Collections.Component.Model
    Damage: Components.Damage.Model
}

type State = {
    ActiveTab: Component
    ShowTextMenus: bool
    Theme: string
}

type Model = {
    ComponentStates: ComponentStates
    AppState:State
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type ApiMsg =
    | ApiClick
    | ApiClear
    | ApiLoaded of Result<string,exn>
    | ApiKeyChange of string
    | ApiNameChange of string

type ComponentMsg =
    | BazaarMsg of Components.Bazaar.Msg
    | BrewMsg of Components.Brewing.Msg
    | EnchMsg of Components.Enchanting.Msg
    | CollMsg of Components.Collections.Component.Msg
    | DmgMsg of Components.Damage.Msg
    | ApiMsg of ApiMsg
type Msg =
    | TabChange of Component
    | ThemeChange of string option
    | TextMenuChange
    | CMsg of ComponentMsg

type ComponentInit =
    | BazInit of Components.Bazaar.Model option
    | BrewInit

#if DEBUG
// model, msg, init, update
[<RequireQualifiedAccess>]
type InitType<'tState,'tInit,'tMsg> =
    | Value of 'tState*Cmd<'tMsg>
    | Method of ('tInit -> 'tState*Cmd<'tMsg>)

type SubComponent<'tProps,'tState,'tMsg, 'tInit> = {
    Wrapper: 'tMsg -> Msg
    Init: InitType<'tState,'tInit,'tMsg>
    View: 'tProps -> 'tState -> ('tMsg -> unit) -> ReactElement
    Update: 'tMsg -> 'tState -> 'tState * Cmd<'tMsg>
}

let subcomponents x =
    match x with
    | ApiExperiment _ -> ()
    | Bazaar ->
        let __ = {
            Wrapper= BazaarMsg >> CMsg
            Init= InitType.Method Components.Bazaar.init
            View= Components.Bazaar.view
            Update= Components.Bazaar.update
        }
        ()
    | Brewing ->
        {
            Wrapper= BrewMsg >> CMsg
            Init= InitType.Method Components.Brewing.init
            View= Components.Brewing.view
            Update= Components.Brewing.update
        }
        |> ignore
    | Damage ->
        {
            Wrapper= DmgMsg >> CMsg
            Init= InitType.Method Components.Damage.init
            View= Components.Damage.view
            Update= Components.Damage.update
        }
        |> ignore
    | Enchanting ->
        {
            Wrapper = EnchMsg >> CMsg
            Init= InitType.Method Components.Enchanting.init
            View= Components.Enchanting.view
            Update= Components.Enchanting.update
        }
        |> ignore
    | Collections ->
        {
            Wrapper = CollMsg >> CMsg
            Init= InitType.Method Components.Collections.Component.init
            View= Components.Collections.Component.view
            Update= Components.Collections.Component.update
        }
        |> ignore

#endif

module Storage =
    open BrowserStorage
    let app : StorageAccess<State> =  BrowserStorage.StorageAccess.createStorage "AppState"
    let api = BrowserStorage.StorageAccess.createStorage "AppState_Api"
    let baz = BrowserStorage.StorageAccess.createStorage "AppState_Bazaar"
    let brew = BrowserStorage.StorageAccess.createStorage "AppState_Brew"
    let coll = BrowserStorage.StorageAccess.createStorage "AppState_Coll"
    let dmg = BrowserStorage.StorageAccess.createStorage "AppState_Dmg"
    let ench = BrowserStorage.StorageAccess.createStorage "AppState_Ench"


let init () =
    let inline mapCmd title (wrapper: _ -> Msg) (cmd1:Cmd<Msg>) init fOverride : 't * Cmd<Msg> =
        let m,cmd =
            try
                fOverride()
            with ex ->
                eprintfn "Failed to deserialize for %s: %s" title ex.Message
                None
            |> init
            
        m, cmd |> Cmd.map wrapper |> List.append cmd1

    let api, cmd = mapCmd "ApiInit" (ApiMsg>>CMsg) Cmd.none (fun x -> x |> Option.defaultValue {Key="";Name="";Loading=false}, Cmd.none) Storage.api.Get
    let baz,cmd = mapCmd "BazaarInit" (BazaarMsg>>CMsg) Cmd.none Components.Bazaar.init Storage.baz.Get
    let brew,cmd = mapCmd "BrewInit" (BrewMsg>>CMsg) cmd Components.Brewing.init Storage.brew.Get
    let coll, cmd = mapCmd "CollectionInit" (CollMsg>>CMsg) cmd Components.Collections.Component.init Storage.coll.Get
    let dmg, cmd = mapCmd "DamageInit" (DmgMsg>>CMsg) cmd Components.Damage.init Storage.dmg.Get
    let ench,cmd = mapCmd "EnchantingInit" (EnchMsg>>CMsg) cmd Components.Enchanting.init Storage.ench.Get
    let app =
        Storage.app.Get()
        |> function
            | Some x -> x
            | None ->
                eprintfn "init: no stored site"
                { ActiveTab= Bazaar; ShowTextMenus= false; Theme= ""}
    if debug then Fable.Core.JS.console.log("starting up app with state", Resolver.serialize app)

    let model =
        {   AppState = app
            ComponentStates= {
                            Api = api
                            Bazaar= baz
                            Brewing= brew
                            Collections= coll
                            Damage= dmg
                            Enchanting= ench
            }
        }
    // Fable.Core.JS.console.log("starting up app with comstate", model.ComponentStates)
    model,cmd

let updateC msg cs =
    let inline fRegular fu msg model save fmsg fmodel =
        let next,cmd = fu msg model
        Some next
        |> save
        |> ignore
        fmodel cs next, cmd |> Cmd.map fmsg


    match msg with
    | BazaarMsg msg ->
        fRegular Components.Bazaar.update msg cs.Bazaar Storage.baz.Save 
            BazaarMsg
            <| fun model next -> {model with Bazaar= next}
    | BrewMsg msg ->
        fRegular Components.Brewing.update msg cs.Brewing Storage.brew.Save
            BrewMsg
            <| fun model next -> {model with Brewing= next}
    | CollMsg msg ->
        fRegular Components.Collections.Component.update msg cs.Collections Storage.coll.Save
            CollMsg
            <| fun model next -> {model with Collections= next}
    | DmgMsg msg ->
        fRegular Components.Damage.update msg cs.Damage Storage.dmg.Save
            DmgMsg
            <| fun model next -> {model with Damage= next}
    | EnchMsg msg ->
        fRegular Components.Enchanting.update msg cs.Enchanting Storage.ench.Save
            EnchMsg
            <| fun model next -> {model with Enchanting= next}
    | ApiMsg msg ->
        match msg with
        | ApiKeyChange x ->
            {cs with Api = {cs.Api with Key=x} }, Cmd.none
        | ApiNameChange x ->
            {cs with Api = {cs.Api with Name = x}}, Cmd.none
        | ApiLoaded(Ok x) ->
            eprintfn "ApiLoaded?"
            Fable.Core.JS.console.log("ApiLoaded?",x)
            {cs with Api = {cs.Api with Loading = false}}, Cmd.none
        | ApiLoaded(Error ex) ->
            eprintfn "ApiLoaded?"
            Fable.Core.JS.console.log("ApiLoaded?",ex.Message)
            {cs with Api = {cs.Api with Loading = false}}, Cmd.none
        | ApiClear ->
            {cs with Api = {cs.Api with Loading = false}}, Cmd.none
        | ApiClick _ ->
            if cs.Api.Loading then
                eprintfn "Clicked while loading"
                cs, Cmd.none
            else
                match cs.Api.Key, cs.Api.Name with
                | ValueString k, ValueString n ->
                    let f = Cmd.OfPromise.either CodeHelpers.HypixelAPI.fetchExample () (Ok>>ApiMsg.ApiLoaded>>ApiMsg) (Error>>ApiMsg.ApiLoaded>>ApiMsg)
                    {cs with Api = {cs.Api with Loading = true}}, f
                | ValueString _, _ ->
                    eprintfn "Api attempted without a name"
                    cs, Cmd.none
                | _ , ValueString _ ->
                    eprintfn "Api attempted without a key"
                    cs, Cmd.none
                | _ -> 
                    eprintfn "Api attempted without a name or key"
                    cs, Cmd.none

let update (msg:Msg) (model:Model) =
    eprintfn "Client update: %A" msg
    let lensState f =
        let next = f model.AppState
        Storage.app.Save (Some next)
        |> function
            | Ok () -> ()
            | Error e ->
                eprintfn "Storage failed"
        {model with AppState= next}
    match msg with
    | TabChange c ->
        lensState (fun s -> {s with ActiveTab= c}), Cmd.none
    | ThemeChange t ->
        lensState (fun s -> {s with Theme = t |> Option.defaultValue ""}), Cmd.none
    | TextMenuChange ->
        lensState (fun s -> {s with ShowTextMenus = not s.ShowTextMenus}), Cmd.none

    | CMsg msg ->
        let next,cmd = updateC msg model.ComponentStates
        {model with ComponentStates = next},cmd |> Cmd.map CMsg

importAll "./style.scss"

let tabSelector ({AppState={Theme=theme;ActiveTab=at};ComponentStates=cs} as x) (dispatch:ComponentMsg -> unit) =
    try
        match at with
        | ApiExperiment ->
            div[][
                Fulma.Columns.columns[] [
                    Fulma.Column.column [][
                        unbox "ApiKey"
                    ]
                    Fulma.Column.column [][
                        input [Type "password"; DefaultValue x.ComponentStates.Api.Key; OnChange (getEvValue>>ApiMsg.ApiKeyChange>> ApiMsg >> dispatch)]
                    ]
                ]
                Fulma.Columns.columns[] [
                    Fulma.Column.column [][
                        unbox "Account Name"
                    ]
                    Fulma.Column.column [][
                        input [Type "text"; DefaultValue x.ComponentStates.Api.Name; OnChange(getEvValue >> ApiMsg.ApiNameChange >> ApiMsg >> dispatch)]
                    ]
                ]
                button [Class "button"; OnClick(fun _ -> ApiMsg.ApiClick |> ApiMsg |> dispatch)][
                    unbox "Fetch"
                ]
                button [Class "button"; OnClick(fun _ -> ApiMsg.ApiClear |> ApiMsg |> dispatch)][
                    unbox "Clear loading state"
                ]
            ]
        | Bazaar ->
            Components.Bazaar.view {Theme=theme} cs.Bazaar (BazaarMsg >> dispatch)
        | Brewing ->
            Components.Brewing.view {Theme=theme} cs.Brewing (BrewMsg >> dispatch)
        | Collections ->
            Components.Collections.Component.view () cs.Collections (CollMsg >> dispatch)
        | Damage ->
            Components.Damage.view () cs.Damage (DmgMsg >> dispatch)
        | Enchanting ->
            Components.Enchanting.view {Theme=theme} cs.Enchanting (EnchMsg >> dispatch)
    with ex ->
        div [] [
            unbox <| Resolver.serialize ex
        ]

let view (model : Model) (dispatch : Msg -> unit) =
    let tabs =
        Component.All
        |> List.map(fun x ->
            let icon =
                match x with
                | Bazaar -> Fa.Solid.DollarSign
                | Brewing -> Fa.Solid.Flask
                | Collections -> Fa.Solid.Warehouse
                | Enchanting -> Fa.Solid.HatWizard
                | Damage -> Fa.Solid.Biohazard
                | ApiExperiment -> Fa.Solid.Brain


            {| c= x; icon = icon |}
        )

    let tabIt (c:Component) (icon:Fa.IconOption) =
        TabLink {Name= string c; Active=Some <| string model.AppState.ActiveTab
                 Title= None; OnClick= fun _ -> TabChange c |> dispatch
                 Children= [
                    Fa.FaIcon List.empty icon
                 ] }
    div []
        [

            TabContainer None None (
                [
                    yield! tabs
                    |> List.map(fun x ->
                        if model.AppState.ShowTextMenus then
                            TabTextLink (string x.c) (Some <| string model.AppState.ActiveTab) (fun _ -> TabChange x.c |> dispatch)
                        else
                            tabIt x.c (x.icon)
                    )
                    yield li [Class "select is-pulled-right"][
                        select [
                            OnChange (getTargetValue("theme select") >> Msg.ThemeChange >> dispatch)
                            Value model.AppState.Theme
                        ][
                            option [Value ""][unbox "Themes..."]
                            option [Value "callout"][unbox "Callout"]
                            option [Value "text"] [unbox "Text"]
                        ]
                    ]
                    yield li [Class "m-left"][
                        label [Class "checkbox"][
                            input [ Type "checkbox"; Checked model.AppState.ShowTextMenus
                                    OnChange (fun _ -> dispatch Msg.TextMenuChange)
                            ]
                            unbox "Text menus"
                        ]

                    ]
                ]
            )

            Container.container [] []
            h2 [Class "is-size-2 has-text-centered"][
                unbox (string model.AppState.ActiveTab)
            ]
            tabSelector model (CMsg >> dispatch)
        ]
#if DEBUG
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
// #if DEBUG
// |> Program.withDebugger
// #endif
|> Program.run
