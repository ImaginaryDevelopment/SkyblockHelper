module Components.Enchanting

open Components.SharedComponents
open Fable.React
open Fable.React.Props
open CodeHelpers.FableHelpers
open AppDomain.SalesReference
open Shared
open Shared.Helpers
open Elmish
open SharedComponents.TabLink
open AppDomain.EnchantingReference

// model msg init update view

// type ToolType =
//     |

// type EnchantState = {
//     OnBook:bool
//     Exclude:bool
//     Filter:bool
//     HaveRecipe:bool
// }

type StruckTracker = Map<EnchantType,string list>

type Model = {
    Submenu: EnchantType
    Struck: StruckTracker
    ColorRecommendations: bool
}

type Msg =
    | EnchantClick of EnchantType * string
    | ClearClick of EnchantType
    | ChangeSubmenu of EnchantType
    | ToggleColors

let init overrideOpt =
    let m =
        overrideOpt
        |> Option.defaultValue {
            Submenu = EnchantType.Sword
            Struck= Map.empty
            ColorRecommendations= true
        }
    m, Cmd.none

let update msg model =
    match msg with
    | ToggleColors ->
        {model with
            ColorRecommendations = not model.ColorRecommendations}
        , Cmd.none
    | Msg.ChangeSubmenu et ->
        {model with Submenu = et}, Cmd.none
    | ClearClick et ->
        let next =
            { model
                with Struck =
                        model.Struck
                        |> Map.remove et
        }
        next,Cmd.none
    | EnchantClick (et,name) ->
        let next =
            model.Struck
            |> Map.tryFind et
            |> function 
                | Some l ->
                    if l |> List.contains name then
                        let l = l |> List.except [name]
                        model.Struck
                        |> Map.add et l
                        |> Some
                    else
                        None
                | None -> None
            |> function
                | Some x -> x
                | None ->
                        model.Struck
                        |> Map.add et [name]
        {model with Struck = next}, Cmd.none
type EnchantingProps = {
    Theme: string
}
module Internal =
    let trStrike struck =
        tr[Class (if struck then "strikethrough" else "")]

    type ERProps = {
        Key:string
        Struck:string list
        ColorRec:bool
        E: EnchantType*Enchant
    }

    let enchantRow (props: ERProps) dispatch =
        let et,e = props.E
        let sucMods = Fulma.Modifier.parseModifiers [Fulma.Modifier.TextColor Fulma.Color.IsSuccess]
        let suc = if props.ColorRec then sucMods |> String.concat " " else ""
        let isStruck = props.Struck |> List.contains e.Base.Name
        let bcls = BFulma.addClasses [
            yield "button"
            if props.ColorRec then yield suc
            if isStruck then
                yield "strikethrough"
        ]
        trStrike isStruck [
            td [
                Class suc
                OnClick (fun _ -> Msg.EnchantClick(et,e.Base.Name) |> dispatch)
            ][
                button [
                    Class bcls
                    Name e.Base.Name
                ][
                    unbox e.Base.Name
                ]
            ]
            td [][ unbox <| string e.Base.TargetLvl ]
            td [](
                match e with
                |Craftable c -> [unbox <| string c.MinEnchantTbl]
                |Uncraftable _ -> []
            )
            td[][
                div[if String.isValueString e.Base.VendorTitle then yield Class "star"][
                    match e with
                    |Craftable c -> yield unbox c.Collection
                    | _ -> ()
                ]
            ]
            td[][
                div[][
                    match e with
                    |Craftable c -> yield unbox <| string c.CraftLvlCreated
                    | _ -> ()
                ]
            ]
            td [](
                match e with
                |Craftable c -> [unbox c.Components]
                |Uncraftable _ -> []
            )

        ]
    let eBody colorRec struck (et,enchants:Enchant list) dispatch =
                tbody [](
                    enchants
                    |> List.map( fun x ->
                        enchantRow {
                            Key=
                                match x with
                                | Craftable c ->
                                    c.Base.Name
                                | Uncraftable u ->
                                    u.Name
                            Struck= struck
                            ColorRec= colorRec
                            E= et, x
                        } dispatch
                    )
                    
                )
    let enchantTable (props:{|
                            colorRec: bool
                            struck: string list
                            enchants: EnchantType*(Enchant list) |}) dispatch =
        div[] [
            table [Class "table"][
                thead [][
                    tr[][
                        td [] [unbox "Enchant Name"]
                        td [Title "Does not include special books"][
                            unbox "Max Craft Level"
                        ]
                        td [Title "Minimum enchant level for the max craft level to appear on the table"][
                            unbox "Min Enchant Tbl"
                        ]
                        td [Title "Craft collection for unlock"][
                            unbox "Collection"
                        ]
                        td [Title "If crafted from components, what level book is created"][
                            unbox "Craft Level Created"
                        ]
                        td [Title "If craftings,  what level book is created"][
                            unbox "Special Components"
                        ]
                    ]
                ]
                eBody props.colorRec props.struck props.enchants dispatch
                // tbody [](
                //     snd props.enchants
                //     |> List.map( fun e ->
                //         enchantRow {
                //             Key= e.Base.Name
                //             Struck= props.struck
                //             ColorRec= props.colorRec
                //             E= fst props.enchants ,e
                //         } dispatch
                //     )
                    
                // )
            ]
        ]
    let armorEnchant useColor strikes enchants dispatch =
        div [][

        ]
    let bowEnchant useColor strikes enchants dispatch =
        div [][

        ]
    let swordEnchant useColor strikes (enchants: _*(Enchant list)) dispatch =
        div[][
            div[][
                input [
                    Type "Checkbox"
                    Checked useColor
                    OnChange (fun _ -> Msg.ToggleColors |> dispatch)
                ]
                unbox "Color Recommendation Levels"
            ]
            div[][
                unbox <| sprintf "9 recommended damage enchants, found %i" (snd enchants |> List.length)
            ]
            enchantTable {| colorRec= useColor
                            enchants= enchants
                            struck= strikes

            |} dispatch
        ]

open Internal
open AppDomain.EnchantingReference
let view props model dispatch =
    let stdTabs = {|
        names= EnchantType.All
        active= Some model.Submenu
        map= string
        onClick= (Msg.ChangeSubmenu >> dispatch)
    |}
    div[][
        TabContainer None (Some stdTabs) [
            ul [] (
                EnchantType.All
                |> List.map(fun et ->
                    TabTextLink (string et) (Some <| string model.Submenu) (fun _ -> Msg.ChangeSubmenu et |> dispatch)
                )
            )
        ]
        div [Class props.Theme](
            let strikes =
                model.Struck
                |> Map.tryFind model.Submenu
                |> Option.defaultValue List.empty
            match model.Submenu with
            | Sword ->
                swordEnchant model.ColorRecommendations strikes sEnchants dispatch
            | Armor ->
                armorEnchant model.ColorRecommendations strikes aEnchants dispatch
            | Bow ->
                bowEnchant model.ColorRecommendations strikes bEnchants dispatch
            | Tool -> div [][]
            |> List.singleton


        )
    ]