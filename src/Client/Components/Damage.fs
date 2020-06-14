module Components.Damage

open AppDomain.Damage
open Components.SharedComponents
open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open AppDomain.Collections.Weapons
open Shared
open SharedComponents.TabLink
open SkyblockHelper
open CodeHelpers.FableHelpers

type DictType<'k,'v when 'k : comparison> = Map<'k,'v>

[<RequireQualifiedAccess>]
type Submenu = | Custom | Accessorize

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

    let init = {
            MaxHealth= 1000
            Stats= Map[ Strength, 100.0 ]
            CombatLevel= 10 }

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
    let view (title:string) (model:Model) dispatch = 
        div [Class "bd-outline"][
            Heading.h2 [] [unbox "Character"]
            Heading.h3[Heading.IsSubtitle][
                // unbox "Your stats without a weapon, pet, or pot"
                unbox title 
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

module AccessoryComponent =
    open AppDomain.Accessories
    type OwnedAccessory =
        {   FormName:string
            Rarity:Rarity
            Reforge: Reforge option
        }

    type Model = {
        Character: CharacterComponent.Model
        // use BaseName
        OwnedAccessories: Map<string,OwnedAccessory>
    }
    type UpdateFormValue = string*Rarity
    type UpdateFormArgs = {
        BaseName:string 
        // selectedFormName
        FormOpt: UpdateFormValue option
    }

    type Msg =
        | UpdateCharacter of CharacterComponent.Msg
        | UpdateForm of UpdateFormArgs
        | UpdateReforge of baseName:string * Reforge option

    let init = {
        Character= CharacterComponent.init
        OwnedAccessories= Map.empty
    }

    let update msg (model:Model) =
        let tryFindOwned name = model.OwnedAccessories |> Map.tryFind name

        let inline lensOwned f =
            {model with OwnedAccessories= f model.OwnedAccessories}
        match msg with
        | UpdateCharacter msg ->
            let next, cmd = CharacterComponent.update msg model.Character
            {model with Character= next}, cmd |> Cmd.map UpdateCharacter
        | UpdateForm {BaseName=baseName;FormOpt=None} ->
            lensOwned (fun oa -> oa |> Map.remove baseName), Cmd.none
        | UpdateForm{BaseName=baseName;FormOpt=Some (selectedFormName,selectedRarity)} ->
            let baseAcc = Accessory.TryFindBase baseName accessories
            let actualAcc = baseAcc |> Option.bind (Accessory.TryFindForm selectedFormName)
            let nextOwned = {
                FormName= actualAcc |> Option.map(fun aa -> aa.Name) |> Option.defaultValue selectedFormName
                Rarity= actualAcc |> Option.map(fun aa -> aa.Rarity) |> Option.defaultValue selectedRarity
                Reforge= tryFindOwned baseName |> Option.bind(fun x -> x.Reforge)
            }
            lensOwned (fun oa -> oa |> Map.add baseName nextOwned ), Cmd.none

        | UpdateReforge (baseName,r) ->
            let baseAcc = Accessory.TryFindBase baseName accessories
            // let actualAcc = baseAcc |> Option.bind (Accessory.TryFindForm selectedFormName)
            match model.OwnedAccessories |> Map.tryFind baseName with
            | Some oi ->
                let next = model.OwnedAccessories |> Map.add baseName {oi with Reforge=r}
                {model with OwnedAccessories= next}, Cmd.none
            | None ->
                eprintfn "This shouldn't happen, can't find %s" baseName
                model, Cmd.none

    let displayAccessory (ownedInfo:OwnedAccessory option) (baseAcc:Accessory) dispatch =
        let forms = Accessory.Unfold baseAcc
        let name = ownedInfo |> Option.map(fun oi -> oi.FormName) |> Option.defaultValue baseAcc.Name
        let onChange =
            getEvValue
            >> Option.ofValueString
            >> Option.bind Resolver.Deserialize<UpdateFormValue>
            >> (fun x ->
                Msg.UpdateForm{BaseName=baseAcc.Name; FormOpt= x}
            )

        Fulma.Columns.columns [][
            Fulma.Column.column [][
                Fulma.Select.select [][
                    select [
                        match ownedInfo with
                        | Some oi ->
                            let x = (oi.FormName,oi.Rarity)
                            yield DefaultValue (Resolver.Serialize x)
                        | None -> ()
                        yield OnChange (onChange>> dispatch)
                    ][
                        yield option [Value ""][ str <| sprintf "No %s or higher owned" baseAcc.Name]
                        yield!
                            forms
                            |> List.map(fun f ->
                                let v = (f.Name,f.Rarity)
                                option [Resolver.Serialize v |> box<string> |> Value ][
                                    str <| sprintf "%s - %A" f.Name f.Rarity

                                ]
                            )
                    ]
                ]
            ]
            Fulma.Column.column [Column.Option.Props [Title baseAcc.Name]][
                Fulma.Select.select [][
                    select [
                        match ownedInfo with
                        | Some {FormName= fn;Reforge= Some rfg} ->
                            let x = (fn,rfg)
                            yield DefaultValue (Resolver.Serialize x)
                        | _ -> ()
                        yield OnChange (onChange>> dispatch)
                    ] [
                        yield option [Value ""][str "No Reforge..."]
                        yield!
                            Reforge.All
                            |> List.map(fun rfg ->
                                option [string rfg|> box<string> |> Value][
                                    str <| string rfg
                                ]
                            )
                    ]
                ]
            ]
        ]


    let accview (props:ThemeProps) (model:Model) dispatch =
        div [][
            Section.section [Section.Option.CustomClass props.Theme][
                (
                    try
                        CharacterComponent.view
                            "Character stats without accessories or weapon equipped (armor included for now)"
                            model.Character (Msg.UpdateCharacter >> dispatch)
                    with ex ->
                        pre [][
                            unbox ex.Message
                        ]
                )
            ]
            Section.section [] [
                let rarityCounts =
                    model.OwnedAccessories
                    |> Map.toSeq
                    |> Seq.map(fun (_,oa) ->
                        oa.Rarity
                    )
                    |> Seq.groupBy id
                    |> Seq.map(fun (g,items) -> g, items |> Seq.length)
                    |> List.ofSeq
                let countRarity r = 
                    rarityCounts |> List.tryFind (fst >> (=) r) |> Option.map snd |> Option.defaultValue 0
                let l,e,r,u,c = countRarity Legendary, countRarity Epic, countRarity Rare, countRarity Uncommon, countRarity Common
                yield Fulma.Heading.h2 [][
                    str <| sprintf "Owned - %i Legendary, %i Epic, %i Rare, %i Uncommon, %i Common" l e r u c

                ]
                yield! 
                    accessories
                    |> List.map (fun acc ->
                        let forms = Accessory.Unfold acc
                        let ownedInfo =
                            forms
                            |> Seq.tryPick(fun accForm ->
                                model.OwnedAccessories
                                |> Map.tryFind acc.Name
                            )
                        displayAccessory ownedInfo acc dispatch
                )
            ]
        ]

type Model = {
    Submenu: Submenu
    CustomCharacter: CharacterComponent.Model
    AccessoryModel: AccessoryComponent.Model
    Enemy: EnemyComponent.Model
    WeaponComponent:WeaponComponent.Model
    // no pet, no weapon, no pots
    Pet: DictType<DmgStat,float>
    Potion: DictType<DmgStat,float>
}

type CustomMsg =
    | CharacterChange of CharacterComponent.Msg
    | WeaponChange of WeaponComponent.Msg
    | EnemyChange of EnemyComponent.Msg

type SubMsg =
    | CustomChange of CustomMsg
    | AccChange of AccessoryComponent.Msg

type Msg =
    | ComponentChange of SubMsg
    | SubmenuChange of Submenu

let init initOverride : Model * Cmd<Msg> =
    initOverride
    |> Option.defaultValue {
        Submenu= Submenu.Custom
        AccessoryModel= AccessoryComponent.init
        CustomCharacter= CharacterComponent.init
        Enemy= {
            MaxHealth= 13000
            CurrentHealth= 13000
            Type= Some EnemyEnchantClassification.Ender
        }
        WeaponComponent= WeaponComponent.init None
        Pet= Map.empty
        Potion= Map.empty
    }, Cmd.none

let update (msg:Msg) model:Model * Cmd<Msg> =
    match msg with
    | Msg.ComponentChange (AccChange msg) ->
        let next, cmd = AccessoryComponent.update msg model.AccessoryModel
        {model with AccessoryModel= next}, cmd |> Cmd.map (AccChange >> ComponentChange)
    | Msg.ComponentChange (CustomChange msg) ->
        match msg with
        |WeaponChange msg ->
            let next,cmd = WeaponComponent.update msg model.WeaponComponent
            {model with WeaponComponent= next}, cmd |> Cmd.map WeaponChange
        | CharacterChange msg ->
            let next,cmd = CharacterComponent.update msg model.CustomCharacter
            {model with CustomCharacter= next}, cmd |> Cmd.map CharacterChange
        | EnemyChange msg ->
            let next,cmd = EnemyComponent.update msg model.Enemy
            {model with Enemy= next}, cmd |> Cmd.map EnemyChange
        |> function
            | m,c ->
                m, c |> Cmd.map (CustomChange >> ComponentChange)
    | SubmenuChange sm ->
        {model with Submenu= sm}, Cmd.none

module Internal =
    let dmgView (model:Model) =
        let cl = model.CustomCharacter.CombatLevel
        let stats = [model.CustomCharacter.Stats;model.WeaponComponent.Stats]
        let clv = getCombatLevelValue cl
        let e =
            getEValue 
                AppDomain.Collections.Weapons.WeaponType.Sword
                (float model.CustomCharacter.MaxHealth)
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

let customview props model (dispatch:CustomMsg -> unit) =
    div [] [
        Section.section [][
            (
                try
                    CharacterComponent.view "Your stats without a weapon equipped" model.CustomCharacter (CharacterChange >> dispatch)
                with ex ->
                    pre [][
                        unbox ex.Message
                    ]
            )
        ]
        Section.section [][
            (
                try
                    WeaponComponent.view model.WeaponComponent (WeaponChange >> dispatch)
                with ex ->
                    pre [][
                        unbox ex.Message
                    ]
            )
        ]
        Section.section [][
            (
                try
                    EnemyComponent.view model.Enemy (EnemyChange >> dispatch)
                with ex ->
                    pre [][
                        unbox ex.Message
                    ]
            )
        ]
        Section.section [][ Internal.dmgView model ]
    ]

let view (props:ThemeProps) (model:Model) (dispatch: Msg -> unit) =
    let tab = 
        match model.Submenu with
        | Submenu.Custom ->
            customview props model (SubMsg.CustomChange >> Msg.ComponentChange >> dispatch)
        | Submenu.Accessorize ->
            AccessoryComponent.accview props model.AccessoryModel (SubMsg.AccChange >> Msg.ComponentChange >> dispatch)
    div [] [
        TabContainer (Option.ofValueString props.Theme) None (
            [Submenu.Accessorize;Submenu.Custom] |> List.map(fun sm ->
                TabTextLink (string sm) (string model.Submenu |> Some) (fun _ -> Msg.SubmenuChange sm |> dispatch)
            )
        )
        div [Class props.Theme][ tab ]
    ]