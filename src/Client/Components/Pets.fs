module Components.Pets
open AppDomain.Collections.Pets
open Shared.Helpers
open SkyblockHelper
open Elmish
open Fulma
open Fable.React.Helpers
open Fable.React.Props
open Components.SharedComponents
open Fable.React.Standard
open CodeHelpers.FableHelpers


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
        | Pumpkins
        | Candies
        with
            static member All =
                [
                    // PerLevel
                    // Cumulative
                    Xp
                    Zealots
                    Pumpkins
                    Candies
                ]
            static member Humanize : LevelMode -> string = string
            static member Parse x =
                LevelMode.All
                |> List.tryPick(flip equalsIStr x)



    type Model = {
        LevelMode: LevelMode option
        CountMode: CountMode option
        PetType: PetType option
        Lvl: int
        Rarity: Rarity
        Candies: Candy list
    }

    type Msg =
        | LvlChange of int
        | RarityChange of Rarity
        | LevelModeChange of LevelMode
        | CountModeChange of CountMode
        | PetTypeChange of PetType
    let private defaultLevelMode = LevelMode.Xp
    let private defaultCountMode = CountMode.Individual

    let init initOverride :Model*Cmd<Msg>=
        initOverride
        |> Option.defaultValue {
            LevelMode= Some defaultLevelMode
            CountMode= Some defaultCountMode
            Lvl= 1
            PetType= Some Combat
            Rarity= Epic
            Candies= List.empty
        }, Cmd.none

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
        | PetTypeChange pt ->
            {model with PetType= Some pt}, Cmd.none

    let view (model:Model) dispatch =
        let inline cMine sz attr children =
            Fulma.Column.column (
                Fulma.Column.Option.Width(Fulma.Screen.All, sz)::attr)
                    children

        let inline c15 attr children = cMine Fulma.Column.Is1 attr children

        Section.section [] [
            Fulma.Columns.columns [][
                cMine Fulma.Column.Is2 [] [
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
                cMine Fulma.Column.Is2 [] [
                    Fulma.Label.label[][
                        str "Type"
                    ]
                    Fulma.Select.select [] [
                        Select {|
                                active= model.PetType |> Option.defaultValue Combat
                                addedClasses= List.empty
                                items= PetType.All
                                map= PetType.Humanize
                                parse= PetType.Parse
                                onChange= Msg.PetTypeChange >> dispatch
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
                // c15 [Fulma.Column.Props [Title "Candies already used"]] [
                //     Fulma.Label.label[][ str "Candies" ]
                //     Fulma.Input.number [
                //         Fulma.Input.Option.Props [ Min 1;Max 10 ]
                //         Fulma.Input.DefaultValue <| string model.Candies
                //         Fulma.Input.OnChange (getEvValue >> tryParseInt >> Option.iter( Msg.LvlChange >> dispatch))]
                // ]
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
                                active= model.LevelMode |> Option.defaultValue defaultLevelMode
                                addedClasses= List.empty
                                items= LevelMode.All
                                map= LevelMode.Humanize
                                parse= LevelMode.Parse
                                onChange= (Msg.LevelModeChange >> dispatch)
                |}
                yield Select {|
                                active= model.CountMode |> Option.defaultValue defaultCountMode
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
                let xpformatting divisor = formatNumber (Some <| if divisor > 1000.0 then 4 elif divisor > 100.0 then 3 elif divisor > 1.0 then 2 else 0)
                let createIndiTable title divisor =
                    createTable title (fun r i ->
                        match i with
                        | 1 -> 0.0
                        | _ ->
                            let x = getLvlXp (i-1) r
                            float x / divisor
                        // |> formatNumber (Some <| if divisor > 1.0 then 2 else 0)
                        |> xpformatting divisor
                    )
                let createCumTable title divisor =
                    createTable title (fun r i ->
                        match i with
                        | 1 -> 0.0
                        | _ ->
                            [2..i]
                            |> List.sumBy (fun i -> getLvlXp (i-1) r)
                            |> float
                            |> fun x -> x / divisor
                        |> xpformatting divisor
                    )

                let candyValue = 500_000
                let getXp t fullValue fmt =
                    let xp =
                        match model.PetType with
                        | Some t' when t' = t -> fullValue
                        | _ -> fullValue * 0.25
                    let txt =
                        xp
                        |> formatNumber (Some 0)
                        |> sprintf fmt
                    txt,xp
                yield!
                    match model.LevelMode, model.CountMode with
                    | _, None
                    | None, _ ->
                        printfn "modes: %A, %A" model.LevelMode model.CountMode
                        [div[][ str "No level mode found"]]
                    | Some x, Some Cumulative ->
                        match x with
                        | Zealots ->
                            let killXp =
                                match model.PetType with
                                | None | Some Combat -> 40.0
                                | _ -> 40.0 * 0.25
                            // sprintf "Levels by Zealot kills(@%s xp per)" (formatNumber (Some 0) killXp) , killXp
                            getXp Combat 40.0 "Levels by Zealot kills(@%s xp per)" 
                        | Pumpkins ->
                            // let xp =
                            //     match model.PetType with
                            //     | None | Some Farming -> 4.5
                            //     | _ -> 4.5 * 0.25
                            getXp Farming 4.5 "Levels by Pumpkin farming(@%s xp per)"
                        | Xp -> "Levels by Accumulated Xp", 1.0
                        | Candies -> "Levels by Accumulated Candy", float candyValue
                        |> fun (t,d) -> createCumTable t d
                    | Some x, Some Individual ->
                        match x with
                        | Zealots ->
                            getXp Combat 40.0"Levels by Zealot kills(@%s xp per)"
                        | Xp -> "Xp Per Level",1.0
                        | Pumpkins -> getXp Farming 4.5 "Levels by Pumpkin farming(@%s xp per)"
                        | Candies ->
                            sprintf "Superb Per Level(@%s per)" (formatInt candyValue),float candyValue
                        |> fun (t,d) -> createIndiTable t d

                    // | Some l, Some c -> [ div [] [str <| sprintf "not implemented %A,%A" l c]]
            ]
        ]

// | CMsg.LevelMsg msg ->
//     cLens (fun cs ->
//             let next,cmd = Leveling.update msg cs.Level
//             {cs with Level= next}, cmd
//             ) LevelMsg