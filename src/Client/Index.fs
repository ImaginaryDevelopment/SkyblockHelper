module Index

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
    | ApiExperiment
    | Bazaar
    | Brewing
    | Collections
    | Damage
    | Enchanting
    | EventCalc
    | Minions
    | Pets

    with
        static member All =
            [
                ApiExperiment
                Bazaar
                Brewing
                Enchanting
                EventCalc
                Minions
                Collections
                Damage
                Pets
            ]

type ComponentStates = {
    Api : Components.Api.Model
    Bazaar: Components.Bazaar.Model
    Brewing: Components.Brewing.Model
    Collections: Components.Collections.Component.Model
    Damage: Components.Damage.Model
    Enchanting: Components.Enchanting.Model
    EventCalc: Components.EventCalc.Model
    Mins: Components.Minions.Model
    Pets: Components.Pets.Leveling.Model
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
    | ApiMsg of Components.Api.Msg
    | BazMsg of Components.Bazaar.Msg
    | BrewMsg of Components.Brewing.Msg
    | CollMsg of Components.Collections.Component.Msg
    | DmgMsg of Components.Damage.Msg
    | EnchMsg of Components.Enchanting.Msg
    | EvtMsg of Components.EventCalc.Msg
    | MinMsg of Components.Minions.Msg
    | PetMsg of Components.Pets.Leveling.Msg

type Msg =
    | TabChange of Component
    | ThemeChange of string option
    | TextMenuChange
    | CMsg of ComponentMsg

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
    | ApiExperiment _ ->
        {
            Wrapper= ApiMsg >> CMsg
            Init= InitType.Method Components.Api.init
            View= fun _ -> Components.Api.view // ignore 'props
            Update= Components.Api.update
        }
        |> ignore
    | Bazaar ->
        {
            Wrapper= BazMsg >> CMsg
            Init= InitType.Method Components.Bazaar.init
            View= Components.Bazaar.view
            Update= Components.Bazaar.update
        }
        |> ignore
    | Brewing ->
        {
            Wrapper= BrewMsg >> CMsg
            Init= InitType.Method Components.Brewing.init
            View= Components.Brewing.view
            Update= Components.Brewing.update
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
    | EventCalc ->
        {
            Wrapper= EvtMsg >> CMsg
            Init= InitType.Method Components.EventCalc.init
            View= Components.EventCalc.view
            Update= Components.EventCalc.update
        }
        |> ignore
    | Minions ->
        {
            Wrapper= MinMsg >> CMsg
            Init= InitType.Method Components.Minions.init
            View= Components.Minions.view
            Update= Components.Minions.update
        }
        |> ignore
    | Pets ->
        {
            Wrapper= PetMsg >> CMsg
            Init= InitType.Method Components.Pets.Leveling.init
            View= fun _ -> Components.Pets.Leveling.view
            Update= Components.Pets.Leveling.update
        }
        |> ignore


#endif

module Storage =
    open BrowserStorage
    let app : StorageAccess<State> =  BrowserStorage.StorageAccess.CreateStorage "AppState"
    let api = BrowserStorage.StorageAccess.CreateStorage "AppState_Api"
    let baz = BrowserStorage.StorageAccess.CreateStorage "AppState_Bazaar"
    let brew = BrowserStorage.StorageAccess.CreateStorage "AppState_Brew"
    let coll = BrowserStorage.StorageAccess.CreateStorage "AppState_Coll"
    let dmg = BrowserStorage.StorageAccess.CreateStorage "AppState_Dmg"
    let ench = BrowserStorage.StorageAccess.CreateStorage "AppState_Ench"
    let evt = StorageAccess.CreateStorage "AppState_Evt"
    let minio = StorageAccess.CreateStorage "AppState_Minions"
    let pet = StorageAccess.CreateStorage "AppState_Pet"

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

    let api, cmd = mapCmd "ApiInit" (ApiMsg>>CMsg) Cmd.none Components.Api.init Storage.api.Get
    let baz,cmd = mapCmd "BazaarInit" (BazMsg>>CMsg) cmd Components.Bazaar.init Storage.baz.Get
    let brew,cmd = mapCmd "BrewInit" (BrewMsg>>CMsg) cmd Components.Brewing.init Storage.brew.Get
    let coll, cmd = mapCmd "CollectionInit" (CollMsg>>CMsg) cmd Components.Collections.Component.init Storage.coll.Get
    let dmg, cmd = mapCmd "DamageInit" (DmgMsg>>CMsg) cmd Components.Damage.init Storage.dmg.Get
    let ench,cmd = mapCmd "EnchantingInit" (EnchMsg>>CMsg) cmd Components.Enchanting.init Storage.ench.Get
    let evt,cmd = mapCmd "EventInit" (EvtMsg>>CMsg) cmd Components.EventCalc.init Storage.evt.Get
    let minio, cmd = mapCmd "MinsInit" (MinMsg>>CMsg) cmd Components.Minions.init Storage.minio.Get
    let pet, cmd = mapCmd "PetsInit" (PetMsg>>CMsg) cmd Components.Pets.Leveling.init Storage.pet.Get
    let app =
        Storage.app.Get()
        |> function
            | Some x -> x
            | None ->
                eprintfn "init: no stored site"
                { ActiveTab= Bazaar; ShowTextMenus= false; Theme= ""}
    if debug then Fable.Core.JS.console.log("starting up app with state", Resolver.Serialize app)

    let model =
        {   AppState = app
            ComponentStates= {
                            Api = api
                            Bazaar= baz
                            Brewing= brew
                            Collections= coll
                            Damage= dmg
                            Enchanting= ench
                            EventCalc= evt
                            Mins= minio
                            Pets= pet
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
    | ApiMsg msg ->
        fRegular Components.Api.update msg cs.Api Storage.api.Save
            ApiMsg
            <| fun model next -> {model with Api= next}
    | BazMsg msg ->
        fRegular Components.Bazaar.update msg cs.Bazaar Storage.baz.Save
            BazMsg
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
    | EvtMsg msg ->
        fRegular Components.EventCalc.update msg cs.EventCalc Storage.evt.Save
            EvtMsg
            <| fun model next -> {model with EventCalc= next}
    | MinMsg msg ->
        fRegular Components.Minions.update msg cs.Mins Storage.minio.Save
            MinMsg
            <| fun model next -> {model with Mins= next}
    | PetMsg msg ->
        fRegular Components.Pets.Leveling.update msg cs.Pets Storage.pet.Save
            PetMsg
            <| fun model next -> {model with Pets= next}


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
        | ApiExperiment -> Components.Api.view cs.Api (ApiMsg >> dispatch)
        | Bazaar ->
            Components.Bazaar.view {Theme=theme} cs.Bazaar (BazMsg >> dispatch)
        | Brewing ->
            Components.Brewing.view {Theme=theme} cs.Brewing (BrewMsg >> dispatch)
        | Collections ->
            Components.Collections.Component.view () cs.Collections (CollMsg >> dispatch)
        | Damage ->
            Components.Damage.view {Theme=theme} cs.Damage (DmgMsg >> dispatch)
        | Enchanting ->
            Components.Enchanting.view {Theme=theme} cs.Enchanting (EnchMsg >> dispatch)
        | EventCalc ->
            Components.EventCalc.view theme cs.EventCalc (EvtMsg >> dispatch)
        | Minions ->
            Components.Minions.view {Theme=theme} cs.Mins (MinMsg >> dispatch)
        | Pets ->
            Components.Pets.Leveling.view cs.Pets (PetMsg >> dispatch)
    with ex ->
        div [] [
            unbox ex.Message
        ]

let view (model : Model) (dispatch : Msg -> unit) =
    let tabs =
        Component.All
        |> List.map(fun x ->
            let icon =
                match x with
                | ApiExperiment -> Fa.Solid.Brain
                | Bazaar -> Fa.Solid.DollarSign
                | Brewing -> Fa.Solid.Flask
                | Collections -> Fa.Solid.Warehouse
                | Damage -> Fa.Solid.Biohazard
                | Enchanting -> Fa.Solid.HatWizard
                | EventCalc -> Fa.Solid.CalendarAlt
                | Minions -> Fa.Solid.HardHat
                | Pets -> Fa.Solid.Bone


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
                    yield li [Class "select is-pulled-right"] [
                        select [
                            OnChange (getTargetValue("theme select") >> Msg.ThemeChange >> dispatch)
                            Value model.AppState.Theme
                        ] [
                            option [Value ""] [unbox "Themes..."]
                            option [Value "callout"] [unbox "Callout"]
                            option [Value "text"] [unbox "Text"]
                        ]
                    ]
                    yield li [Class "m-left"] [
                        label [Class "checkbox"] [
                            input [ Type "checkbox"; Checked model.AppState.ShowTextMenus
                                    OnChange (fun _ -> dispatch Msg.TextMenuChange)
                            ]
                            unbox "Text menus"
                        ]
                    ]
                ]
            )

            Container.container [] []
            h2 [Class "is-size-2 has-text-centered"] [
                unbox (string model.AppState.ActiveTab)
            ]
            tabSelector model (CMsg >> dispatch)
        ]