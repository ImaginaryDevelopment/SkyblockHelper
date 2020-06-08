module Components.Collections.Armor

open AppDomain.Collections.Armor
open CodeHelpers.FableHelpers
open Components.SharedComponents
open Elmish
open Fable.React
open Fable.React.Props
open SkyblockHelper

type Model = {
    Checked: Map<string,ArmorPartType list>
    FoldedArmor: string list
    FoldedRarity: Rarity option list
}

type Msg =
    | FoldAll
    | FoldRarity of Rarity option
    | FoldArmor of setname:string
    | Check of setname:string * part: ArmorPartType

let allRarities = None :: (Rarity.All |> List.map Some)

let init initOverride =
    initOverride
    |> Option.defaultValue {
        Checked= Map.empty
        FoldedRarity= allRarities
        FoldedArmor= List.empty
    }


let update (msg:Msg) (model:Model): Model*Cmd<Msg> =
    match msg with
    | FoldAll ->
        {model with FoldedRarity= allRarities}, Cmd.none  //armorSets |> List.map(fun (n,_,_) -> n)}, Cmd.none
    | FoldArmor n ->
        {model with FoldedArmor = toggleListValue model.FoldedArmor n}, Cmd.none
    | FoldRarity r ->
        {model with FoldedRarity= toggleListValue model.FoldedRarity r}, Cmd.none
    | Check (n,p) ->
        {model with Checked= toggleMapListValue model.Checked n p }, Cmd.none

module Internals =
    let getATitle = fst
    let getPartTitle =
        function
        | Unnamed x -> string x
        | Named(name,_) -> name
    let getPartType =
        function
        | Unnamed x -> x
        | Named(_,x) -> x
    let getParts =
        function
        | _, Standard _ -> ArmorPartType.All |> List.map Unnamed
        | _, Special(_,parts) -> parts
    let getRarityTitle =
        function
        | None -> "Mixed"
        | Some (r:Rarity) -> string r

    let displayChecks armorSet checkedParts onChange =
        ul [Class "is-horizontal list"] (
            getParts armorSet
            |> List.map(fun p ->
                let tit,typ = getPartTitle p, getPartType p
                li [Key tit; Class "list-item"; Title tit][
                    input [ Class "checkbox"; Type "checkbox"
                            Checked (checkedParts |> List.contains typ)
                            OnChange (fun _ -> onChange typ)
                    ]
                    unbox (string typ)
                ]
            )
        )

    let armorDisplay armor checkedParts folded onToggle onChange =
        let title = getATitle armor

        // li [Key title; Class "list-item"][
        div [] [
            Fulma.Columns.columns [][
                Fulma.Column.column [][
                    FoldMaster {| title=title; isFolded= folded; onToggle= onToggle |}
                ]
                FoldTarget folded (div [Class "column is-a-fifth"] [
                    displayChecks armor checkedParts onChange
                ])
            ]
        ]

open Internals
open Components.Collections.Shared



let view (model:Model) dispatch =
    div[][
        button [Class "button"; OnClick (fun _ -> Msg.FoldAll |> dispatch)][unbox "Fold All"]
        ul [Class "list"] (
            let onCheck name x = Msg.Check(name,x) |> dispatch
            armorSets
            |> List.groupBy(fun (_,r,_) -> r)
            |> List.map(fun (r,sets) ->
                let folded = model.FoldedRarity |> List.contains r
                let setView =
                    sets
                    |> List.sortBy(fun (a,_,_) -> a)
                    |> List.map(fun (a,_,t) ->
                        let checkedParts = model.Checked |> Map.tryFind a |> Option.defaultValue List.empty
                            // [displayChecks items checkedItems (fun name -> Msg.Check (wt,name) |> dispatch)]
                        (
                            let isSetFolded = model.FoldedArmor |> List.contains a
                            let ad = armorDisplay (a,t) checkedParts isSetFolded (fun _ -> Msg.FoldArmor a |> dispatch) (onCheck a)
                            ad
                        )
                    )
                foldyListItem {Name= string r; Title= getRarityTitle r; Folded= folded; OnToggle= fun _ -> Msg.FoldRarity r |> dispatch }(
                    setView
                )
            )
        )
    ]