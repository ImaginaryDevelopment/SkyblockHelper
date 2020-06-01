module Components.Damage

open AppDomain.Damage
open Components.SharedComponents
open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open AppDomain.Collections.Weapons

type DictType<'k,'v when 'k : comparison> = Map<'k,'v>

let addValues ds (maps:DictType<DmgStat,float> seq) =
    maps
    |> Seq.choose (Map.tryFind ds)
    |> Seq.sum

type Weapon = {
    Name: string
    WeaponType: WeaponType
    Damage: int
    Stats: DictType<DmgStat,float>
    Enchants: DictType<Enchant, int>
}

let numberRow name value dispatch: IHTMLProp seq * _= (seq [Class "columns"],[
                        div [Class "column"] [unbox name]
                        Column.column [Column.Width(Screen.All, Fulma.Column.IsFourFifths)] [
                            NumberInput {
                                Name=name
                                OnChange= (fun nv -> dispatch nv.Name nv.Value)
                                Placeholder= Some name
                                Value= value
                            }
                        ]])

module DmgStatComponent =
    type Model = DictType<DmgStat,float>
    type Msg = Update of DmgStat * float

    // skipping init, not really necessary to have private state here

    let update msg (model:Model) =
        match msg with
        | Update (ds,v) ->
            if v > 0.0 then
                model |> Map.add ds v, Cmd.none
            else
                model |> Map.remove ds, Cmd.none
    let view (model:Model) dispatch =
        div[][
            ul[](
                DmgStat.All
                |> Seq.map(fun ds ->
                    let n = string ds
                    let dsv =
                        if model.ContainsKey ds then
                            Some model.[ds]
                        else
                            None
                    li <|| numberRow n dsv (fun _ v -> Update (ds,v |> Option.defaultValue 0.0) |> dispatch)
                )
            )
        ]

module CharacterComponent =
    type Model = {
        CombatLevel:int
        MaxHealth: int
        Stats: DictType<DmgStat,float>
    }
    type Msg =
        | HealthChange of int
        | StatChange of DmgStatComponent.Msg
        | CLvlChange of int
    let update msg (model:Model) =
        match msg with
        | HealthChange i ->
            {model with MaxHealth= i}, Cmd.none
        | StatChange msg ->
            let next,cmd = DmgStatComponent.update msg model.Stats
            {model with Stats=next}, cmd |> Cmd.map StatChange
        | CLvlChange i ->
            let next = {model with CombatLevel= i}
            next, Cmd.none
    let view (model:Model) dispatch = 
        div [Class "bd-outline"][
            Heading.h2 [] [unbox "Character"]
            Heading.h3[Heading.IsSubtitle][
                // unbox "Your stats without a weapon, pet, or pot"
                unbox "Your stats without a weapon equipped"
            ]
            ul [][
                li <|| numberRow "Combat Level" (Some <| float model.CombatLevel) (
                    fun _ v -> v |> Option.map int |> Option.defaultValue 0 |> CLvlChange |> dispatch)
                li <|| numberRow "Max Health" (Some <| float model.MaxHealth) (
                    fun _ v -> v |> Option.map int |> Option.defaultValue 0 |> HealthChange |> dispatch)
                li [][
                    DmgStatComponent.view model.Stats (StatChange>>dispatch)
                ]
            ]
        ]

module EnchantComponent =
    type Model = DictType<Enchant, int>
    type Msg =
        | ChangeEnchant of Enchant * int

    let update msg (model:Model) =
        match msg with
        | ChangeEnchant (e,v) ->
            if v > 0 then
                let enchants = model |> Map.add e v
                enchants, Cmd.none
            else
                model |> Map.remove e, Cmd.none
    let view wt (model:Model) dispatch =
        ul[](
            Enchant.All
            |> Seq.filter(Enchant.CanApplyTo wt)
            |> Seq.map(fun e ->
                li <|| numberRow (Enchant.Humanize e) (model|> Map.tryFind e |> Option.map float) (fun _ v ->
                    ChangeEnchant(e,v |> Option.map int |> Option.defaultValue 0) |> dispatch
                )
            )
        )

module WeaponComponent =
    type Model = Weapon
    type Msg =
        | UpdateName of string
        | UpdateStat of DmgStatComponent.Msg
        | ChangeDmg of int
        | ChangeEnchant of EnchantComponent.Msg
        | ChangeWeaponType of WeaponType

    let init initOverride =
        initOverride
        |> Option.defaultValue {
            Name= "Diamond Sword"
            WeaponType= Sword
            Damage= 20
            Stats= Map.empty
            Enchants= Map.empty
        }

    let update msg model =
        match msg with
        | UpdateName n ->
            {model with Model.Name= n}, Cmd.none
        | ChangeDmg v ->
            {model with Damage= v}, Cmd.none
        | UpdateStat msg ->
            let next,cmd = DmgStatComponent.update msg model.Stats
            {model with Stats= next}, cmd |> Cmd.map UpdateStat
        | ChangeEnchant msg ->
            let next,cmd = EnchantComponent.update msg model.Enchants
            {model with Enchants= next}, cmd |> Cmd.map ChangeEnchant
        | ChangeWeaponType t ->
            {model with WeaponType = t}, Cmd.none

    let view (model:Model) dispatch = 
        div [Class "bd-outline"][
            Heading.h2 [] [unbox "Weapon"]
            Heading.h3[Heading.IsSubtitle][
                // unbox "Your stats without a weapon, pet, or pot"
                unbox "Your weapon's stats"
            ]
            Select {|
                    active= model.WeaponType
                    addedClasses= List.empty
                    items= WeaponType.All
                    map= string
                    parse= WeaponType.Parse
                    onChange= (fun x -> Msg.ChangeWeaponType x |> dispatch)
                    |}
            ul [][
                li <|| numberRow "Damage" (Some <| float model.Damage) (
                    fun _ v -> v |> Option.map int |> Option.defaultValue 0 |> ChangeDmg |> dispatch)
                li [] [
                    DmgStatComponent.view model.Stats (UpdateStat>>dispatch)
                ]
                li [] [
                        EnchantComponent.view model.WeaponType model.Enchants (ChangeEnchant>>dispatch)
                ]
            ]
        ]
module EnemyComponent =
    type Model = {
        MaxHealth:int
        CurrentHealth:int
        Type:EnemyEnchantClassification option
    }

    type Msg =
        | ChangeMax of int
        | ChangeCurrent of int
        | ChangeType of EnemyEnchantClassification option

    let update msg model =
        match msg with
        |ChangeMax i ->
            {model with MaxHealth = i}, Cmd.none
        | ChangeCurrent i ->
            {model with CurrentHealth= i}, Cmd.none
        | ChangeType (tOpt) ->
            {model with Type= tOpt}, Cmd.none
    let view model (dispatch:Msg -> unit) =
        let dispatchInt f _ (v:float option) : unit =
            v
            |> Option.map int
            |> Option.defaultValue 0
            |> f
            |> dispatch
        div [Class "bd-outline"][
            SelectOpt{|
                        emptyLabel= "EnemyType..."
                        active= model.Type
                        addedClasses= List.empty
                        items= EnemyEnchantClassification.All
                        map= string
                        parse= EnemyEnchantClassification.Parse 
                        onChange= (ChangeType >> dispatch)
            |}
            ul[][
                li <|| numberRow "Max Health" (Some <| float model.MaxHealth) (dispatchInt Msg.ChangeMax)
                li <|| numberRow "Current Health" (Some <| float model.CurrentHealth) (dispatchInt Msg.ChangeCurrent)
            ]
        ]

type Model = {
    Character: CharacterComponent.Model
    Enemy: EnemyComponent.Model
    WeaponComponent:WeaponComponent.Model
    // no pet, no weapon, no pots
    Pet: DictType<DmgStat,float>
    Potion: DictType<DmgStat,float>
}

type Msg =
    | WeaponChange of WeaponComponent.Msg
    | CharacterChange of CharacterComponent.Msg
    | EnemyChange of EnemyComponent.Msg

let init initOverride =
    initOverride
    |> Option.defaultValue {
        Character= {
            MaxHealth= 1000
            Stats= Map[ Strength, 100.0 ]
            CombatLevel= 10
        }
        Enemy= {
            MaxHealth= 13000
            CurrentHealth= 13000
            Type= Some EnemyEnchantClassification.Ender
        }
        WeaponComponent= WeaponComponent.init None
        Pet= Map.empty
        Potion= Map.empty
    }, Cmd.none

let update msg model =
    match msg with
    |WeaponChange msg ->
        let next,cmd = WeaponComponent.update msg model.WeaponComponent
        {model with WeaponComponent= next}, cmd |> Cmd.map WeaponChange
    | CharacterChange msg ->
        let next,cmd = CharacterComponent.update msg model.Character
        {model with Character= next}, cmd |> Cmd.map CharacterChange
    | EnemyChange msg ->
        let next,cmd = EnemyComponent.update msg model.Enemy
        {model with Enemy= next}, cmd |> Cmd.map EnemyChange

module Internal =
    let dmgView model =
        let cl = model.Character.CombatLevel
        let stats = [model.Character.Stats;model.WeaponComponent.Stats]
        let clv = getCombatLevelValue cl
        let e =
            getEValue 
                AppDomain.Collections.Weapons.WeaponType.Sword
                (float model.Character.MaxHealth)
                (float model.Enemy.MaxHealth, float model.Enemy.CurrentHealth, model.Enemy.Type)
                model.WeaponComponent.Enchants
        let wb = 0.0 // getWbValue 
        let str = addValues Strength stats
        let x = getBasedmg (float model.WeaponComponent.Damage) str
        let m = getMult cl e wb
        let ab = 1.0
        let cd = addValues CritDmg stats
        let normal = getNormalDamage x m ab
        let crit = getTotalCritDamage x m ab cd
        let cc = addValues CritChance stats

        div[][
            Content.content [Content.Size IsLarge][
                pre [] [unbox <| sprintf "Str:%.1f,CC:%.1f, CD: %.0f%%" str cc cd]
                pre [] [unbox "Base= (5 + WeaponDMG + Strength / 5) * (1 + Strength / 100)"]
                // would be nice if details were foldable, or toggled
                pre [] [unbox <| sprintf "Base = (5 + %i + %.2f / 5) * %.2f" model.WeaponComponent.Damage str (1.0 + str / 100.0)]
                pre [] [unbox <| sprintf "Base = %.2f * %.2f" (5.0 + float model.WeaponComponent.Damage + str / 5.0) (1.0 + str / 100.0)]
                pre [] [unbox <| sprintf "Base = %.2f" x]
                hr []
                pre [][unbox "1 + CombatLevel * 0.04 + Enchants + WeaponBonus"]
                pre [][unbox <| sprintf "5 + %.2f + %.2f + %.2f" clv e wb]
                pre [][unbox <| sprintf "Multiplier = %.2f" m]
                hr []
                pre [][unbox "Base Damage * Multiplier * ArmorBonus * (1 + CritDamage)"]
                pre [][unbox <| sprintf "%.2f * %.2f * %.2f * (1 + %.2f%%)" x m ab cd]
                pre [][unbox <| sprintf "Crit=%.2f, Normal=%.2f" crit normal]
            ]
        ]

let view (props:obj) (model:Model) dispatch =
    div [] [
        Section.section [][
            (
                try
                    CharacterComponent.view model.Character (Msg.CharacterChange >> dispatch)
                with ex ->
                    pre [][
                        unbox ex.Message
                    ]
            )
        ]
        Section.section [][
            (
                try
                    WeaponComponent.view model.WeaponComponent (Msg.WeaponChange >> dispatch)
                with ex ->
                    pre [][
                        unbox ex.Message
                    ]
            )
        ]
        Section.section [][
            (
                try
                    EnemyComponent.view model.Enemy (Msg.EnemyChange >> dispatch)
                with ex ->
                    pre [][
                        unbox ex.Message
                    ]
            )
        ]
        Section.section [][ Internal.dmgView model ]
    ]