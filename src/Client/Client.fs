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

    with
        static member All =
            [
                Bazaar
                Brewing
                Enchanting
                // Events
                // Minions
                Collections
            ]

type ComponentStates = {
        Bazaar: Components.Bazaar.Model
        Brewing: Components.Brewing.Model
        Enchanting: Components.Enchanting.Model
        Collections: Components.Collections.Component.Model
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
type ComponentMsg =
    | BazaarMsg of Components.Bazaar.Msg
    | BrewMsg of Components.Brewing.Msg
    | EnchMsg of Components.Enchanting.Msg
    | CollMsg of Components.Collections.Component.Msg

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
    let baz = BrowserStorage.StorageAccess.createStorage "AppState_Bazaar"
    let brew = BrowserStorage.StorageAccess.createStorage "AppState_Brew"
    let ench = BrowserStorage.StorageAccess.createStorage "AppState_Ench"
    let coll = BrowserStorage.StorageAccess.createStorage "AppState_Coll"

let init () =
    let mapCmd (wrapper: _ -> Msg) (cmd1:Cmd<Msg>) init : 't * Cmd<Msg> =
        let m,cmd = init
        m, cmd |> Cmd.map wrapper |> List.append cmd1

    let baz,cmd = mapCmd (BazaarMsg>>CMsg) Cmd.none <| Components.Bazaar.init (Storage.baz.Get())
    let brew,cmd = mapCmd (BrewMsg>>CMsg) cmd <| Components.Brewing.init (Storage.brew.Get())
    let ench,cmd = mapCmd (EnchMsg>>CMsg) cmd <| Components.Enchanting.init (Storage.ench.Get())
    let coll, cmd = mapCmd (CollMsg>>CMsg) cmd <| Components.Collections.Component.init (Storage.coll.Get())
    let app =
        Storage.app.Get()
        |> function
            | Some x -> x
            | None ->
                eprintfn "init: no stored site"
                { ActiveTab= Bazaar; ShowTextMenus= false; Theme= "" }
    if debug then Fable.Core.JS.console.log("starting up app with state", Resolver.serialize app)

    let model =
        {   AppState = app
            ComponentStates= {
                            Bazaar= baz
                            Brewing= brew
                            Enchanting= ench
                            Collections= coll
            }
        }
    // Fable.Core.JS.console.log("starting up app with comstate", model.ComponentStates)
    model,cmd

let updateC msg cs =
    match msg with
    | BazaarMsg msg ->
        let next,cmd = Components.Bazaar.update msg cs.Bazaar
        Some next
        |> Storage.baz.Save 
        |> ignore
        {cs with Bazaar=next}, cmd |> Cmd.map BazaarMsg
    | BrewMsg msg ->
        let next, cmd = Components.Brewing.update msg cs.Brewing
        Some next
        |> Storage.brew.Save 
        |> ignore
        {cs with Brewing=next},cmd |> Cmd.map BrewMsg
    | EnchMsg msg ->
        let next, cmd = Components.Enchanting.update msg cs.Enchanting
        Some next
        |> Storage.ench.Save 
        |> ignore
        {cs with Enchanting= next}, cmd |> Cmd.map EnchMsg
    | CollMsg msg ->
        let next,cmd = Components.Collections.Component.update msg cs.Collections
        Some next
        |> Storage.coll.Save 
        |> ignore
        {cs with Collections= next}, cmd |> Cmd.map CollMsg

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

let tabSelector ({AppState={Theme=theme;ActiveTab=at};ComponentStates=cs}) dispatch =
    try
        match at with
        | Bazaar ->
            Components.Bazaar.view {Theme=theme} cs.Bazaar (BazaarMsg >> dispatch)
        | Brewing ->
            Components.Brewing.view {Theme=theme} cs.Brewing (BrewMsg >> dispatch)
        | Enchanting ->
            Components.Enchanting.view {Theme=theme} cs.Enchanting (EnchMsg >> dispatch)
        | Collections ->
            let result = Components.Collections.Component.view () cs.Collections (CollMsg >> dispatch)
            result
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
                | Enchanting -> Fa.Solid.HatWizard
                | Collections -> Fa.Solid.Warehouse
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
