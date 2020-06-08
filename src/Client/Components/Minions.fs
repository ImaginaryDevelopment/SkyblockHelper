module Components.Minions

open Components.SharedComponents
open Elmish
open Fable.React
open Fable.React.Props
open Shared
open Shared.Helpers

type TargetItem = {
    Name:string
    Resource:string
    ResourceAmount: int
}

type Model = {
    Delay:int
    Count:int
    Production:int
    BazaarPlain:float // estimated value to sell in the bazaar
    // ItemCost:int // for later when we allow custom inputs instead of only selecting items
    SelectedItem:TargetItem option
}

type Msg =
    | DelayChange of int
    | CountChange of int
    | ProductionChange of int
    | BazaarChange of float
    | TargetItemChange of TargetItem option
    | ItemNameChange of string
    | ItemResourceNameChange of string
    | ItemResourceAmountChange of int

let init initOverride: Model * Cmd<Msg> = 
    initOverride 
    |> Option.defaultValue {
        Delay=26
        Count=1
        Production=1
        BazaarPlain=10.0
        SelectedItem=None
    }, Cmd.none

let update (msg:Msg) (model:Model): Model * Cmd<Msg> =
    let changeItem f =
        let ti =
            model.SelectedItem
            |> function
                | None -> {Name="";Resource="";ResourceAmount=0}
                | Some ti -> ti
            |> f
        {model with SelectedItem = Some ti}
    match msg with
    |DelayChange x ->
        {model with Delay=x}, Cmd.none
    | CountChange x ->
        {model with Count=x}, Cmd.none
    | ProductionChange x ->
        {model with Production=x}, Cmd.none
    | BazaarChange x ->
        {model with BazaarPlain=x}, Cmd.none
    | TargetItemChange ti ->
        {model with SelectedItem= ti}, Cmd.none
    | ItemNameChange x ->
        changeItem (fun ti -> {ti with Name=x}), Cmd.none
    | ItemResourceNameChange x ->
        changeItem (fun ti -> {ti with Resource=x}), Cmd.none
    | ItemResourceAmountChange x ->
        changeItem (fun ti -> {ti with ResourceAmount=x}), Cmd.none
module Internals =
    let itemSelector items selectedItemOpt onSelected =
        SelectOpt {|
                    active=selectedItemOpt
                    addedClasses=[]
                    emptyLabel= "Items..."
                    items= items
                    map= (fun item -> sprintf "%s(%s)" item.Name item.Resource)
                    parse= (function
                        | ValueString x ->
                            let name = x.[0..x.IndexOf "("]
                            let resource = x.[x.IndexOf "(" + 1 .. x.IndexOf(")")]
                            items
                            |> List.tryFind(fun x -> x.Name= name && x.Resource = resource)
                        | _ -> None
                    )
                    onChange= onSelected |}
    let item name resource amount =
        {
            Name= name
            Resource= resource
            ResourceAmount= amount
        }
    let targetItems = [
        item "Leaping Sword" "Spider Eye" 245_760
        item "Runaan's Bow" "String" 36_864
        item "Runaan's Bow" "String" 30_720
        item "Treecapitator" "Obsidian" 81_920
    ]
    let fuelColumn (name:string) unfueledCoins (multiplier:float) durationHr asterisk =
        // per hour
        let fuelproduces = unfueledCoins * multiplier
        let fuelCoinOverage = fuelproduces - unfueledCoins
        let breakeven = fuelCoinOverage * durationHr
        let fuelPercentDisplay = (multiplier - 1.0) * 100.0

        Fulma.Column.column [][
            span [
                Class (if String.isValueString asterisk then "star" else "")
                Title (sprintf "break-even point %s" asterisk)][unbox name]
            span [][
                unbox (sprintf "< %.2f then buy." breakeven)
            ]
            span [][
                unbox (sprintf "%.0f%% fuel makes " fuelPercentDisplay)
            ]
            unbox <| sprintf "%.2f coins per hour with %.0f%% fueld. Fuel earns %.1f in 1 hour." fuelproduces fuelPercentDisplay fuelCoinOverage
        ]
    let fuelAnalysis (productionPerHr:float) bazaarPlain =
        let unfueledCoins = productionPerHr * bazaarPlain
        let fuels = [
            "Coal", 1.05, 0.5
            "Block of Coal", 1.05, 5.0
            "Enchanted Bread", 1.05, 12.0
            "Enchanted Coal", 1.1, 24.0
            "Enchanted Charcoal", 1.2, 36.
            "Hamster Wheel", 1.5, 24.
            "Foul Flesh", 1.9, 5.
            "Catalyst", 2., 3.
        ]
        div[] [
            yield Fulma.Column.column [][
                unbox <| sprintf "%.2f items per hour" productionPerHr
            ]
            yield Fulma.Column.column [][
                unbox <| sprintf "%.2f coins per hour" unfueledCoins
            ]
            yield! (
                fuels
                |> List.map(fun (n,m,duration) ->
                    fuelColumn n unfueledCoins m duration ""
                )
            )
        ]
    [<RequireQualifiedAccess>]
    type ModelPropType =
        | Delay
        | Production
        | BazaarPlain
        | Count
        with
            static member All =
                [
                    Delay
                    Production
                    BazaarPlain
                    Count
                ]
open Internals

let view (model:Model) (dispatch:Msg -> unit) =
    let productionPerHrPerMinion = if model.Delay > 0 then float model.Production * 3600.0 / 2.0 / float model.Delay |> Some else None
    let reqHours = model.SelectedItem |> Option.bind(fun si -> if model.Count > 0 then  float si.ResourceAmount / float model.Count |> Some else None)
    let reqDays = reqHours |> Option.map(fun hr -> if hr > 24.0 then hr / 24.0 else 0.0)
    // SelectOpt
    div[][
        div [Class "bd-callout"](
            let hfield label title value onChange =
                HField {|
                        label= label
                        title= title
                        input=(fun cls -> NumberInput {
                                                    Name= ""
                                                    Value= Some value
                                                    Placeholder= None
                                                    OnChange= fun nv -> nv.Value |> Option.iter onChange
                                    }
                        )
                |}
            [
                hfield "Delay" "How much is the delay between actions for this minion?" (float model.Delay) (int >> DelayChange >> dispatch)
                hfield "Production""How much is produced for each action? (Glowstone, lapis, and others produce more than 1 per action)"
                    (float model.Production) (int >> ProductionChange >> dispatch)
                hfield "Est. Value" "How much do you think 1 unit would sell for?" model.BazaarPlain (BazaarChange >> dispatch)
                hfield "Count" "How many minions of this tier will you be using?" (float model.Count) (int >> Msg.CountChange >> dispatch)
            ]


                )
        fuelAnalysis (productionPerHrPerMinion |> Option.defaultValue 0.0) model.BazaarPlain
        hr []
        Fulma.Column.column [][
            itemSelector targetItems model.SelectedItem (Msg.TargetItemChange >> dispatch)
            span[Class "Span"][
                match model.SelectedItem with
                | None -> ()
                | Some si -> yield unbox si.Resource
            ]
        ]
        Fulma.Columns.columns [](
            [
                [
                    yield unbox <| sprintf "%.0f" model.BazaarPlain
                    match reqDays with
                    | Some reqDays ->
                        yield unbox <| sprintf "%.1f days" reqDays
                    | None -> yield unbox "0 days"
                    match reqHours with
                    | Some reqHours ->
                        yield unbox <| sprintf "%.2f hours" reqHours
                    | None -> yield unbox "0 hours"
                ]
            ]
            |> List.map (Fulma.Column.column [])
        )
    ]