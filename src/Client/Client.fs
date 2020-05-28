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

type Component =
    | Bazaar
    | Brewing
    | Enchanting
    with
        static member All =
            [
                Bazaar
                Brewing
                Enchanting
            ]

type ComponentStates = {
        Bazaar: Components.Bazaar.Model
        Brewing: Components.Brewing.Model
        Enchanting: Components.Enchanting.Model
}

type Model = {
    ActiveTab: Component
    ShowTextMenus: bool
    ComponentStates: ComponentStates
    Theme: string
}

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type ComponentMsg =
    | BazaarMsg of Components.Bazaar.Msg
    | BrewMsg of Components.Brewing.Msg
    | EnchMsg of Components.Enchanting.Msg

type Msg =
    | TabChange of Component
    | ThemeChange of string option
    | TextMenuChange
    | CMsg of ComponentMsg

type ComponentInit =
    | BazInit of Components.Bazaar.Model option
    | BrewInit

// model, msg, init, update, view
// let inline subComponentX
//     (name: Component)
//     (icon: Fable.FontAwesome.Fa.IconOption)
//     (fState: ComponentStates -> 'tState)
//     (fMsgWrap: 'tMsg -> Msg)
//     // (init: 'arg -> 'tState*Cmd<'tMsg>)
//     (update: 'tMsg -> 'tState -> 'tState * Cmd<'tMsg>)
//     (view: 'tProps -> 'tState -> ('tMsg -> unit) -> ReactElement) wrapper =
//     // let fInit arg =
//     //     let next, cmd = init arg
//     //     next, cmd |> Cmd.map (wrapper >> CMsg)
//     name, icon, update, view

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
        let __ = {
            Wrapper= BrewMsg >> CMsg
            Init= InitType.Value Components.Brewing.init
            View= Components.Brewing.view
            Update= Components.Brewing.update
        }
        ()
    | Enchanting ->
        let __ = {
            Wrapper = EnchMsg >> CMsg
            Init= InitType.Method Components.Enchanting.init
            View= Components.Enchanting.view
            Update= Components.Enchanting.update
        }
        ()

#endif

let init () = 
    // TODO: plumb browser local storage
    let mapCmd (wrapper: _ -> Msg) (cmd1:Cmd<Msg>) init : 't * Cmd<Msg> =
        let m,cmd = init
        m, cmd |> Cmd.map wrapper |> List.append cmd1

    let baz,cmd = mapCmd (BazaarMsg>>CMsg) Cmd.none <| Components.Bazaar.init None
    let brew,cmd = mapCmd (BrewMsg>>CMsg) cmd <| Components.Brewing.init
    let ench,cmd = mapCmd (EnchMsg>>CMsg) cmd <| Components.Enchanting.init None
    let model =
        {   ActiveTab=Bazaar; ShowTextMenus=false; Theme=""
            ComponentStates= {
                            // TODO: plumb browser local storage result
                            Bazaar= baz
                            Brewing= brew
                            Enchanting= ench
            }
        }
    model,cmd
let updateC msg cs =
    match msg with
    | BazaarMsg msg ->
        let next,cmd = Components.Bazaar.update msg cs.Bazaar
        {cs with Bazaar=next}, cmd |> Cmd.map BazaarMsg
    | BrewMsg msg ->
        let next, cmd = Components.Brewing.update msg cs.Brewing
        {cs with Brewing=next},cmd |> Cmd.map BrewMsg
    | EnchMsg msg ->
        let next, cmd = Components.Enchanting.update msg cs.Enchanting
        {cs with Enchanting= next}, cmd |> Cmd.map EnchMsg

let update (msg:Msg) (model:Model) =
    match msg with
    | TabChange c ->
        {model with ActiveTab= c}, Cmd.none
    | ThemeChange t ->
        {model with Theme = t |> Option.defaultValue ""}, Cmd.none
    | TextMenuChange ->
        {model with ShowTextMenus = not model.ShowTextMenus}, Cmd.none
    | CMsg msg ->
        let next,cmd = updateC msg model.ComponentStates
        {model with ComponentStates = next},cmd |> Cmd.map CMsg


importAll "./style.scss"

let tabSelector (model:Model) dispatch =
    try
        match model.ActiveTab with
        |Bazaar ->
            // Bazaar.view {Theme = model.Theme} model dispatch
            Components.Bazaar.view {Theme=model.Theme} model.ComponentStates.Bazaar (BazaarMsg >> dispatch)
        |Brewing ->
            Components.Brewing.view {Theme=model.Theme} model.ComponentStates.Brewing (BrewMsg >> dispatch)
        | Enchanting ->
            Components.Enchanting.view {Theme=model.Theme} model.ComponentStates.Enchanting (EnchMsg >> dispatch)
    with ex ->
        div [] [
            unbox <| stringify(ex,null,4)

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
            {| c= x; icon = icon |}
        )

    let tabIt (c:Component) (icon:Fa.IconOption) =
        TabLink {name= string c; active=Some <| string model.ActiveTab
                 title= None; onClick= fun _ -> TabChange c |> dispatch
                 children= [
                    Fa.FaIcon List.empty icon
                 ] }
    div []
        [
            TabContainer None None (
                [
                    yield! tabs
                    |> List.map(fun x ->
                        if model.ShowTextMenus then
                            TabTextLink (string x.c) (Some <| string model.ActiveTab) (fun _ -> TabChange x.c |> dispatch)
                        else
                            tabIt x.c (x.icon)

                    )
                    yield li [Class "select is-pulled-right"][
                        select [
                            OnChange (getTargetValue("theme select") >> Msg.ThemeChange >> dispatch)
                            Value model.Theme
                        ][
                            option [Value ""][unbox "Themes..."]
                            option [Value "callout"][unbox "Callout"]
                            option [Value "text"] [unbox "Text"]
                        ]
                    ]
                    yield li [Class "m-left"][
                        label [Class "checkbox"][
                            input [ Type "checkbox"; Checked model.ShowTextMenus
                                    OnChange (fun _ -> dispatch Msg.TextMenuChange)
                            ]
                            unbox "Text menus"
                        ]

                    ]
                ]
            )

            Container.container [] []
            Container.container [] [
              div [] [
                  unbox "testing"
                //   Components.Bazaar.RateDisplay {Mode=Components.Bazaar.Sell;Values = List.empty}
                  Components.ProfileMgmt.profileLink "Sammy314" "Grapes" [
                    str "Profile"
                  ]
              ]
            ]
            h2 [Class "is-size-2 has-text-centered"][
                unbox (string model.ActiveTab)
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
