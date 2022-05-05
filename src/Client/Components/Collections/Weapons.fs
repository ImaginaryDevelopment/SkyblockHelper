module Components.Collections.Weapons

open AppDomain.Collections.Collection
open AppDomain.Collections.Weapons
open CodeHelpers.FableHelpers
open Components.SharedComponents
open Components.Collections.Shared
open Fable.React
open Fable.React.Props
open Shared.Helpers
open SkyblockHelper

// model msg init update view

type Model = {
    Subtab: WeaponType
    BowChecked: string list
    SwordChecked: string list
    BowFolded: Rarity list
    SwordFolded: Rarity list
}

type Msg =
    | TabChange of WeaponType
    | Fold of WeaponType * Rarity
    | Check of WeaponType * string

let init overrideOpt =
    overrideOpt
    |> Option.defaultValue {
            Subtab= Sword
            BowChecked= List.empty
            SwordChecked= List.empty
            BowFolded= Rarity.All
            SwordFolded= Rarity.All
    }

module private Internal =
    let displayWeapon (item:Weapon) isChecked dispatch =
        li [Key item.Name; Class "list-item"; Title item.Name ] [
            div [Class "columns"] [
                div [Class "column"] [
                    input [Class "checkbox"; Type "checkbox"; Checked isChecked; OnChange (fun _ -> dispatch item.Name)]
                    unbox item.Name
                ]
                div [Class "column"; Title "Collection"] [
                    match item.CraftType with
                    | Some(Collection null) ->
                        yield span [Class "star";Title "unlocked at start"] [unbox "craftable"]
                    | Some(Collection x) ->
                        yield unbox x
                    | _ -> ()
                ]
                div [Class "column"; Title "Slayer"] [
                    match item.CraftType with
                    | Some(Slayer(s,i)) ->
                        yield unbox <| sprintf "%A Slayer %i" s i
                    | _ -> ()
                ]
                div [Class "column"; Title "Upgrades"] [
                    match item.UpgradesTo with
                    | ValueString x ->
                        yield unbox <| sprintf "upgrades to %s" x
                    | _ -> ()
                ]
            ]

        ]
    let displayChecks items isChecked dispatch =
        ul [Class "is-horizontal list"](
            items |> List.map(fun x ->
                displayWeapon x (isChecked |> List.contains x.Name) dispatch
            )
        )

open Internal
open Elmish
let update msg state =
    match msg with
    | Msg.Check(Sword,x) ->
        {state with SwordChecked= toggleListValue state.SwordChecked x}, Cmd.none
    | Msg.Check(Bow,x) ->
        {state with BowChecked= toggleListValue state.BowChecked x}, Cmd.none
    | Msg.Fold (Sword,r) ->
        {state with SwordFolded= toggleListValue state.SwordFolded r}, Cmd.none
    | Msg.Fold (Bow,r) ->
        {state with BowFolded= toggleListValue state.BowFolded r}, Cmd.none
    | Msg.TabChange wt ->
        {state with Subtab= wt}, Cmd.none

let view props state dispatch =
    let makeTab wt (items:Weapon list) folded checkedItems =
        div [] [
            ul [](
                Rarity.All
                |> List.filter(fun r -> items |> List.map (fun x -> x.Rarity) |> List.contains r)
                |> List.map(fun r ->
                    foldyListItem {Name= string r; Title= string r; Folded= folded |> List.contains r; OnToggle= fun _ -> Msg.Fold (wt,r) |> dispatch }(
                        [displayChecks items checkedItems (fun name -> Msg.Check (wt,name) |> dispatch)]
                    )
                )
            )
        ]
    let tab =
        match state.Subtab with
        | Sword ->
            makeTab Sword swords state.SwordFolded state.SwordChecked
        | Bow ->
            makeTab Bow bows state.BowFolded state.BowChecked
    div [] [
        TabContainer None (Some {|
                                names= [Sword;Bow]
                                map= string
                                active= Some state.Subtab
                                onClick= (Msg.TabChange>>dispatch)
        |}) []
        tab
    ]