module Components.Minions

open Components.SharedComponents
open Elmish
open Fable.React
open Fable.React.Props
open Shared
open Shared.Helpers
open SharedComponents.TabLink
open SkyblockHelper
open CodeHelpers.FableHelpers

module MinionProduction =

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

    let init : Model = {
            Delay= 26
            Count= 1
            Production= 1
            BazaarPlain= 10.0
            SelectedItem= None
    }

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
            {model with SelectedItem=ti}, Cmd.none
        | ItemNameChange x ->
            changeItem (fun ti -> {ti with Name=x}), Cmd.none
        | ItemResourceNameChange x ->
            changeItem (fun ti -> {ti with Resource=x}), Cmd.none
        | ItemResourceAmountChange x ->
            changeItem (fun ti -> {ti with ResourceAmount=x}), Cmd.none

    module Internals =
        let itemSelector items selectedItemOpt onSelected =
            SelectOpt {|
                        active= selectedItemOpt
                        addedClasses= []
                        emptyLabel= "Items..."
                        items= items
                        map= (fun item -> sprintf "%s(%s)" item.Name item.Resource)
                        parse= (function
                            | ValueString x ->
                                let name = x.[0..x.IndexOf "(" - 1]
                                let resource = x.[x.IndexOf "(" + 1 .. x.IndexOf(")") - 1]
                                printfn "Selecting via text %s - %s" name resource
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
            item "Runaan's Bow" "Bone" 30_720
            item "Runaan's Bow" "String" 36_864
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
                unbox <| sprintf "%.2f coins per hour with %.0f%% fuel. Fuel earns %.1f in 1 hour." fuelproduces fuelPercentDisplay fuelCoinOverage
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
        let reqHours =
            match productionPerHrPerMinion,model.SelectedItem with
            | Some prod, Some si when model.Count > 0  ->
                float si.ResourceAmount / (float model.Count * prod)
                |> Some
            | _ -> None
            // model.SelectedItem |> Option.bind(fun si -> if model.Count > 0 then float si.ResourceAmount / float model.Count |> Some else None)
        let reqDays = reqHours |> Option.map(fun hr -> if hr > 24.0 then hr / 24.0 else 0.0)
        div[][
            div [Class "bd-callout"](
                let hfield label title value onChange =
                    HField {|
                            label= label
                            title= title
                            input=(fun _cls -> NumberInput {
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
                    | Some si -> yield unbox <| sprintf "%i - %s" si.ResourceAmount si.Resource
                ]
            ]
            Fulma.Columns.columns [](
                [
                    [
                        match reqDays with
                        | Some reqDays ->
                            yield unbox <| sprintf " %.1f days " reqDays
                        | None -> yield unbox "0 days"
                        match reqHours with
                        | Some reqHours ->
                            yield unbox <| sprintf "or %.2f hours" reqHours
                        | None -> yield unbox "0 hours"
                    ]
                ]
                |> List.map (Fulma.Column.column [])
            )
        ]

module Leveling =

    type CountMode =
        | Individual
        | Cumulative
        with
            static member All =
                [
                    Individual
                    Cumulative
                ]
            static member Humanize:CountMode -> string = string
            static member Parse =
                function
                | EqualsI (string Individual) -> Some Individual
                | EqualsI (string Cumulative) -> Some Cumulative
                | _ -> None
    type LevelMode =
        | Xp
        // PerLevel
        // | Cumulative
        | Zealots // https://hypixel-skyblock.fandom.com/wiki/Combat
        | Candies
        with
            static member All =
                [
                    // PerLevel
                    // Cumulative
                    Xp
                    Zealots
                    Candies
                ]
            static member Humanize : LevelMode -> string = string
            static member Parse =
                function
                | EqualsI (string Xp) -> Some Xp
                // | EqualsI (string Cumulative) -> Some Cumulative
                | EqualsI (string Zealots) -> Some Zealots
                | EqualsI (string Candies) -> Some Candies
                | _ -> None


    let getXpTo100 =
        function
        | Common -> 5_624_785
        | Uncommon -> 8_644_220
        | Rare -> 12_626_665
        | Epic -> 18_608_500
        | Legendary -> 25_353_2300

    let getLvlXp =
        fun lvl -> lvl + 1
        >> min 100
        >> max 2
        >> function
        | 2 -> [100;175;275;440;660]
        | 3 -> [ 110;190;300;490;730 ]
        | 4 -> [ 120;210;330;540;800 ]
        | 5 -> [ 130;230;360;600;880 ]
        | 6 -> [ 145;250;400;660;960 ]
        | 7 -> [ 160;275;440;730;1_050 ]
        | 8 -> [ 175;300;490;800;1_150 ]
        | 9 -> [ 190;330;540;880;1_260 ]
        | 10 -> [ 210;360;600;960;1_380 ]
        | 11 -> [ 230;400;660;1_050;1_510 ]
        | 12 -> [ 250;440;730;1_150;1_650 ]
        | 13 -> [ 275;490;800;1_260;1_800 ]
        | 14 -> [ 300;540;880;1_380;1_960 ]
        | 15 -> [ 330;600;960;1_510;2_130 ]
        | 16 -> [ 360;660;1_050;1_650;2_310 ]
        | 17 -> [ 400;730;1_150;1_800;2_500 ]
        | 18 -> [ 440;800;1_260;1_960;2_700 ]
        | 19 -> [ 490;880;1_380;2_130;2_920 ]
        | 20 -> [ 540;960;1_510;2_310;3_160 ]
        | 21 -> [ 600;1_050;1_650;2_500;3_420 ]
        | 22 -> [ 660;1_150;1_800;2_700;3_700 ]
        | 23 -> [ 730;1_260;1_960;2_920;4_000 ]
        | 24 -> [ 800;1_380;2_130;3_160;4_350 ]
        | 25 -> [ 880;1_510;2_310;3_420;4_750 ]
        | 26 -> [ 960;1_650;2_500;3_700;5_200 ]
        | 27 -> [ 1_050;1_800;2_700;4_000;5_700 ]
        | 28 -> [ 1_150;1_960;2_920;4_350;6_300 ]
        | 29 -> [ 1_260;2_130;3_160;4_750;7_000 ]
        | 30 -> [ 1_380;2_310;3_420;5_200;7_800 ]
        | 31 -> [ 1_510;2_500;3_700;5_700;8_700 ]
        | 32 -> [ 1_650;2_700;4_000;6_300;9_700 ]
        | 33 -> [ 1_800;2_920;4_350;7_000;10_800 ]
        | 34 -> [ 1_960;3_160;4_750;7_800;12_000 ]
        | 35 -> [ 2_130;3_420;5_200;8_700;13_300 ]
        | 36 -> [ 2_310;3_700;5_700;9_700;14_700 ]
        | 37 -> [ 2_500;4_000;6_300;10_800;16_200 ]
        | 38 -> [ 2_700;4_350;7_000;12_000;17_800 ]
        | 39 -> [ 2_920;4_750;7_800;13_300;19_500 ]
        | 40 -> [ 3_160;5_200;8_700;14_700;21_300 ]
        | 41 -> [ 3_420;5_700;9_700;16_200;23_200 ]
        | 42 -> [ 3_700;6_300;10_800;17_800;25_200 ]
        | 43 -> [ 4_000;7_000;12_000;19_500;27_400 ]
        | 44 -> [ 4_350;7_800;13_300;21_300;29_800 ]
        | 45 -> [ 4_750;8_700;14_700;23_200;32_400 ]
        | 46 -> [ 5_200;9_700;16_200;25_200;35_200 ]
        | 47 -> [ 5_700;10_800;17_800;27_400;38_200 ]
        | 48 -> [ 6_300;12_000;19_500;29_800;41_400 ]
        | 49 -> [ 7_000;13_300;21_300;32_400;44_800 ]
        | 50 -> [ 7_800;14_700;23_200;35_200;48_400 ]
        | 51 -> [ 8_700;16_200;25_200;38_200;52_200 ]
        | 52 -> [ 9_700;17_800;27_400;41_400;56_200 ]
        | 53 -> [ 10_800;19_500;29_800;44_800;60_400 ]
        | 54 -> [ 12_000;21_300;32_400;48_400;64_800 ]
        | 55 -> [ 13_300;23_200;35_200;52_200;69_400 ]
        | 56 -> [ 14_700;25_200;38_200;56_200;74_200 ]
        | 57 -> [ 16_200;27_400;41_400;60_400;79_200 ]
        | 58 -> [ 17_800;29_800;44_800;64_800;84_700 ]
        | 59 -> [ 19_500;32_400;48_400;69_400;90_700 ]
        | 60 -> [ 21_300;35_200;52_200;74_200;97_200 ]
        | 61 -> [ 23_200;38_200;56_200;79_200;104_200 ]
        | 62 -> [ 25_200;41_400;60_400;84_700;111_700 ]
        | 63 -> [ 27_400;44_800;64_800;90_700;119_700 ]
        | 64 -> [ 29_800;48_400;69_400;97_200;128_200 ]
        | 65 -> [ 32_400;52_200;74_200;104_200;137_200 ]
        | 66 -> [ 35_200;56_200;79_200;111_700;146_700 ]
        | 67 -> [ 38_200;60_400;84_700;119_700;156_700 ]
        | 68 -> [ 41_400;64_800;90_700;128_200;167_700 ]
        | 69 -> [ 44_800;69_400;97_200;137_200;179_700 ]
        | 70 -> [ 48_400;74_200;104_200;146_700;192_700 ]
        | 71 -> [ 52_200;79_200;111_700;156_700;206_700 ]
        | 72 -> [ 56_200;84_700;119_700;167_700;221_700 ]
        | 73 -> [ 60_400;90_700;128_200;179_700;237_700 ]
        | 74 -> [ 64_800;97_200;137_200;192_700;254_700 ]
        | 75 -> [ 69_400;104_200;146_700;206_700;272_700 ]
        | 76 -> [ 74_200;111_700;156_700;221_700;291_700 ]
        | 77 -> [ 79_200;119_700;167_700;237_700;311_700 ]
        | 78 -> [ 84_700;128_200;179_700;254_700;333_700 ]
        | 79 -> [ 90_700;137_200;192_700;272_700;357_700 ]
        | 80 -> [ 97_200;146_700;206_700;291_700;383_700 ]
        | 81 -> [ 104_200;156_700;221_700;311_700;411_700 ]
        | 82 -> [ 111_700;167_700;237_700;333_700;441_700 ]
        | 83 -> [ 119_700;179_700;254_700;357_700;476_700 ]
        | 84 -> [ 128_200;192_700;272_700;383_700;516_700 ]
        | 85 -> [ 137_200;206_700;291_700;411_700;561_700 ]
        | 86 -> [ 146_700;221_700;311_700;441_700;611_700 ]
        | 87 -> [ 156_700;237_700;333_700;476_700;666_700 ]
        | 88 -> [ 167_700;254_700;357_700;516_700;726_700 ]
        | 89 -> [ 179_700;272_700;383_700;561_700;791_700 ]
        | 90 -> [ 192_700;291_700;411_700;611_700;861_700 ]
        | 91 -> [ 206_700;311_700;441_700;666_700;936_700 ]
        | 92 -> [ 221_700;333_700;476_700;726_700;1_016_700 ]
        | 93 -> [ 237_700;357_700;516_700;791_700;1_101_700 ]
        | 94 -> [ 254_700;383_700;561_700;861_700;1_191_700 ]
        | 95 -> [ 272_700;411_700;611_700;936_700;1_286_700 ]
        | 96 -> [ 291_700;441_700;666_700;1_016_700;1_386_700 ]
        | 97 -> [ 311_700;476_700;726_700;1_101_700;1_496_700 ]
        | 98 -> [ 333_700;516_700;791_700;1_191_700;1_616_700 ]
        | 99 -> [ 357_700;561_700;861_700;1_286_700;1_746_700 ]
        | 100 -> [ 383_700;611_700;936_700;1_386_700;1_886_700 ]
        | x -> failwithf "Invalid level target %i" x
        >> (fun lvls r ->
            match r with
            | Common -> lvls.[0]
            | Uncommon -> lvls.[1]
            | Rare -> lvls.[2]
            | Epic -> lvls.[3]
            | Legendary -> lvls.[4]
        )

    type Model = {
        LevelMode: LevelMode option
        CountMode: CountMode option
        Lvl: int
        Rarity: Rarity
        Candies: Candy list
    }

    type Msg =
        | LvlChange of int
        | RarityChange of Rarity
        | LevelModeChange of LevelMode
        | CountModeChange of CountMode

    let init = {
        LevelMode= Some LevelMode.Xp
        CountMode= Some CountMode.Individual
        Lvl= 1
        Rarity= Epic
        Candies= List.empty
    }

    let update msg model =
        match msg with
        | LvlChange i ->
            {model with Lvl= i}, Cmd.none
        | RarityChange r ->
            {model with Rarity= r}, Cmd.none
        | LevelModeChange lm ->
            {model with LevelMode= Some lm}, Cmd.none
        | CountModeChange cm ->
            {model with CountMode= Some cm}, Cmd.none

    let view (model:Model) dispatch =
        let inline c15 attr children = Fulma.Column.column ( Fulma.Column.Option.Width(Fulma.Screen.All, Fulma.Column.Is1)::attr) children
        section [] [
            Fulma.Columns.columns [][
                c15 [] [
                    Fulma.Label.label[][
                        str "Rarity"
                    ]
                    Fulma.Select.select [] [
                        Select {|
                                active= model.Rarity
                                addedClasses= List.empty
                                items= Rarity.All
                                map= Rarity.Humanize
                                parse= Rarity.Parse
                                onChange= Msg.RarityChange >> dispatch
                        |}
                    ]
                ]
                c15 [] [
                    Fulma.Label.label[][
                        str "Level"
                    ]
                    Fulma.Input.number [
                        Fulma.Input.Option.Props [ Min 1;Max 100 ]
                        Fulma.Input.DefaultValue <| string (min model.Lvl 100 |> max 1)
                        Fulma.Input.OnChange (getEvValue >> tryParseInt >> Option.iter( Msg.LvlChange >> dispatch))]
                ]
                c15 [Fulma.Column.Props [Title "Candies already used"]] [
                    Fulma.Label.label[][ str "Candies" ]
                    Fulma.Input.number [
                        Fulma.Input.Option.Props [ Min 1;Max 10 ]
                        Fulma.Input.DefaultValue <| string model.Candies
                        Fulma.Input.OnChange (getEvValue >> tryParseInt >> Option.iter( Msg.LvlChange >> dispatch))]
                ]
            ]

            Fulma.Container.container [][
                Fulma.Notification.notification [][
                    ul[][
                        let currentXp =
                            [2..model.Lvl ] |> List.sumBy(fun i -> getLvlXp (i-1) model.Rarity)
                        let xpTo100 =
                            if model.Lvl < 100 then
                                [ model.Lvl .. 99 ]
                                |> List.sumBy(fun i -> getLvlXp i model.Rarity)
                            else 0
                        printfn "Lvl: %i - Current:%i -> to 100 current: %i" model.Lvl currentXp xpTo100
                        yield li [] [
                            yield
                                currentXp
                                |> float
                                |> formatNumber (Some 0)
                                |> sprintf "Current Cumulative Xp: %s"
                                |> str
                        ]
                        match model.Lvl with
                        | x when x > 0 && x < 101 ->
                            // xp to next level
                            yield
                                getLvlXp model.Lvl model.Rarity
                                |> formatInt
                                |> sprintf "Xp to %i - %s" (model.Lvl+1)
                                |> str
                                |> List.singleton
                                |> li []
                            if xpTo100 > 0 && model.Lvl < 99 then
                                yield
                                    xpTo100
                                    |> formatInt
                                    |> sprintf "Xp to 100 : %s" 
                                    |> str
                                    |> List.singleton
                                    |> li []
                            // if x < 100 || model.Rarity <> Legendary then
                            //     // needs work
                            //     yield
                            //         [ model.Lvl + 1 .. 100 ]
                            //         |> List.sumBy(fun i -> getLvlXp i Legendary)
                            //         |> formatInt
                            //         |> sprintf "Xp to Legendary 100: %s"
                            //         |> str
                            //         |> List.singleton
                            //         |> li []


                                ()
                        | _ -> ()

                        // xp to level 100 as same rarity
                        // xp to level 100 as legendary
                    ]
                ]
            ]
            Fulma.Container.container [][
                yield Select {|
                                active= model.LevelMode |> Option.orElse init.LevelMode |> Option.defaultValue LevelMode.Xp
                                addedClasses= List.empty
                                items= LevelMode.All
                                map= LevelMode.Humanize
                                parse= LevelMode.Parse
                                onChange= (Msg.LevelModeChange >> dispatch)
                |}
                yield Select {|
                                active= model.CountMode |> Option.orElse init.CountMode|> Option.defaultValue CountMode.Individual
                                addedClasses= List.empty
                                items= CountMode.All
                                map= CountMode.Humanize
                                parse= CountMode.Parse
                                onChange= (Msg.CountModeChange >> dispatch)
                |}
                let titleColumn x = Fulma.Column.column [][ str x ]
                let createTable title f = [
                    yield Fulma.Heading.h2 [] [ str title ] // "Xp Per Level"
                    yield Fulma.Columns.columns[][
                        titleColumn "Level"
                        titleColumn "Common"
                        titleColumn "Uncommon"
                        titleColumn "Rare"
                        titleColumn "Epic"
                        titleColumn "Legendary"
                    ]
                    yield! [1..100]
                    |> List.map(fun i ->
                        Fulma.Columns.columns[][
                                    yield Fulma.Column.column [][
                                        str <| sprintf "Level %i" i
                                    ]
                                    yield!
                                        Rarity.All
                                        |> List.rev
                                        |> List.map(fun r ->
                                            f r i
                                            // match i with
                                            // | 1 -> 0
                                            // | x -> getLvlXp (i-1) r
                                            // |> formatInt
                                            |> str
                                            |> List.singleton
                                            |> Fulma.Column.column []
                                        )
                        ]
                    )
                ]
                yield!
                    match model.LevelMode, model.CountMode with
                    | _, None
                    | None, _ ->
                        printfn "modes: %A, %A" model.LevelMode model.CountMode
                        [div[][ str "No level mode found"]]
                    | Some Xp, Some Individual ->
                        createTable "Xp Per Level" (fun r i ->
                                                match i with
                                                | 1 -> 0
                                                | _ -> getLvlXp (i-1) r
                                                |> formatInt
                        )
                    | Some Xp, Some Cumulative ->
                        createTable "Levels by Accumulated Xp" (fun r i ->
                            match i with
                            | 1 -> 0
                            | _ ->
                                [2..i]
                                |> List.sumBy (fun i -> getLvlXp (i-1) r)
                            |> formatInt
                        )
                    | Some Zealots, Some Individual ->
                        createTable "Levels by Zealot kills(@40 xp per)" (fun r i ->
                            match i with
                            | 1 -> "0"
                            | _ ->
                                let x = getLvlXp (i-1) r
                                float x / 40.0
                                |> formatNumber (Some 0)

                        )

                    | Some l, Some c -> [ div [] [str <| sprintf "not implemented %A,%A" l c]]
            ]
        ]

type Submenu =
    | Production
    | Leveling

type CState = {
    Prod: MinionProduction.Model
    Level: Leveling.Model
}

type Model = {
    Submenu: Submenu
    CState: CState
}

type ModelVersion =
    | Version1 of MinionProduction.Model
    | Version2

type CMsg =
    | LevelMsg of Leveling.Msg
    | ProdMsg of MinionProduction.Msg

type Msg =
    | SubmenuChange of Submenu
    | CMsg of CMsg

let init initOverride =
    initOverride
    |> Option.defaultValue {
        Submenu= Leveling
        CState= {
            Prod= MinionProduction.init
            Level= Leveling.init
        }
    }, Cmd.none

let update (msg:Msg) (model:Model) =
    let cLens f fMsg =
        let next, cmd = f model.CState
        {model with CState= next}, cmd |> Cmd.map (fMsg>>Msg.CMsg)

    match msg with
    | SubmenuChange s ->
        {model with Submenu= s}, Cmd.none
    | CMsg msg ->
        match msg with
        | CMsg.ProdMsg msg ->
            // let next, cmd = MinionProduction.update msg model.CState.Prod
            cLens (fun cs ->
                     let next,cmd = MinionProduction.update msg cs.Prod
                     {cs with Prod= next},cmd
                     )
                ProdMsg
        | CMsg.LevelMsg msg ->
            cLens (fun cs ->
                    let next,cmd = Leveling.update msg cs.Level
                    {cs with Level= next}, cmd
                    ) LevelMsg

let view (props:ThemeProps) (model:Model) (dispatch: Msg -> unit) =
    let tab =
        match model.Submenu with
        | Submenu.Production ->
            MinionProduction.view model.CState.Prod (CMsg.ProdMsg >> Msg.CMsg >> dispatch)
        | Submenu.Leveling ->
            Leveling.view model.CState.Level (CMsg.LevelMsg >> Msg.CMsg >> dispatch)
    div [] [
        TabContainer (Option.ofValueString props.Theme) None (
            [Submenu.Production;Submenu.Leveling] |> List.map(fun sm ->
                TabTextLink (string sm) (string model.Submenu |> Some) (fun _ -> Msg.SubmenuChange sm |> dispatch)
            )
        )
        div [Class props.Theme][ tab ]
    ]