module Components.Collections.Component

open AppDomain.Collections.Collection
open BrowserStorage
open Fable.React
open Elmish
open CodeHelpers.FableHelpers
type ComponentState = {
    WeaponsState: Weapons.Model
    ArmorState: Armor.Model
}
type Model = {
    Subtab: CollectionType
    ComponentStates: ComponentState
}

type CMessages=
    | WMsg of Weapons.Msg
    | AMsg of Armor.Msg
type Msg =
    | TabChange of CollectionType
    | CMsg of CMessages

let init overrideOpt =
    overrideOpt 
    |> Option.defaultValue {
        Subtab= Weapons
        ComponentStates={
            WeaponsState= Weapons.init None
            ArmorState= Armor.init None
        }
    }
    |> fun x -> x, Cmd.none

let cUpdate msg (state:ComponentState):ComponentState * Cmd<CMessages> =
    match msg with
    | WMsg x ->
        Weapons.update x state.WeaponsState
        ||> mapUpdate (fun s -> {state with WeaponsState= s}) WMsg
    | AMsg x ->
        Armor.update x state.ArmorState
        ||> mapUpdate (fun s -> {state with ArmorState= s}) AMsg

let update msg model :Model * Cmd<Msg> =
    match msg with
    | Msg.TabChange x ->
        {model with Subtab = x}, Cmd.none
    | CMsg x ->
        cUpdate x model.ComponentStates
        ||> mapUpdate (fun cs -> {model with ComponentStates= cs}) CMsg

module private Internal =

    let getTab model dispatch =
        match model.Subtab with
        | Weapons ->
            Weapons.view () model.ComponentStates.WeaponsState (WMsg >> dispatch)
        | Armor ->
            Armor.view model.ComponentStates.ArmorState (AMsg >> dispatch)
        | _ ->
            div[][
                unbox "not implemented"
            ]

open Internal
open Components.SharedComponents

    let view props state dispatch =
        div [] [
            unbox "Collections"
            TabContainer None (
                Some {|
                        names= CollectionType.All
                        map= string
                        active= Some state.Subtab
                        onClick= Msg.TabChange >> dispatch
            |}) []
            (
                getTab state (Msg.CMsg >> dispatch)
            )
        ]
