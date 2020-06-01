module AppDomain.Damage

open AppDomain.Collections.Weapons
open Shared.Helpers

type EnemyEnchantClassification =
    | Arthropod
    | Cubish
    | Dragon
    | Ender
    | Impale
    | Smiteish
    with
        static member Parse x =
            match x with
            | EqualsI (string Arthropod) -> Some Arthropod
            | EqualsI (string Cubish) -> Some Cubish
            | EqualsI (string Dragon) -> Some Dragon
            | EqualsI (string Ender) -> Some Ender
            | EqualsI (string Impale) -> Some Impale
            | EqualsI (string Smiteish) -> Some Smiteish
            | _ -> None


        static member All = [
            Arthropod
            Cubish
            Dragon
            Ender
            Impale
            Smiteish
        ]


type Enchant =
    | Aiming
    | BaneOfArthropods
    | Cubism
    | DragonHunter
    | EnderSlayer
    | Execute
    | Flame
    | FirstStrike
    | GiantKiller
    | Impaling
    | InfiniteQuiver
    | Piercing
    | Power
    | Punch
    | Sharpness
    | Smite
    | Snipe
    with
        static member All = [
            Aiming
            BaneOfArthropods
            Cubism
            DragonHunter
            EnderSlayer
            Execute
            Flame
            FirstStrike
            GiantKiller
            Impaling
            InfiniteQuiver
            Piercing
            Power
            Punch
            Sharpness
            Smite
            Snipe
        ]
        static member Humanize =
            function
            | BaneOfArthropods -> "Bane of Arthropods"
            | GiantKiller -> "Giant Killer"
            | EnderSlayer -> "Ender Slayer"
            | DragonHunter -> "Dragon Hunter"
            | FirstStrike -> "First Strike"
            | InfiniteQuiver -> "Infinite Quiver"
            | x -> string x

module Enchant =
    let (|Both|SwordOnly|BowOnly|) =
        function
        | Aiming
        | Flame
        | InfiniteQuiver
        | Piercing
        | Power
        | Punch
        | Snipe
             -> BowOnly
        | Cubism
        | DragonHunter
        | Impaling
            -> Both
        | BaneOfArthropods
        | EnderSlayer
        | Execute
        | FirstStrike
        | GiantKiller
        | Sharpness
        | Smite
            -> SwordOnly
    let CanApplyTo wt e =
        match e,wt with
        | Both,_ -> true
        | SwordOnly, Sword -> true
        | BowOnly, Bow -> true
        | _ -> false

// all percent values should be converted to a decimal fraction 62.5% -> 0.625

let getCombatLevelValue = float >> (*) 0.04
let getGiantKillerRaw maxHealth mobMaxHealth gkLevel =
    let hpercent:float = (mobMaxHealth - maxHealth) / maxHealth
    hpercent * 0.1 * float gkLevel

let getGiantKillerBonus maxHealth mobMaxHealth gkLevel =
    getGiantKillerRaw maxHealth mobMaxHealth gkLevel
    |> min 0.25

let getExecuteBonus mobMaxHealth mobCurrentHealth eLevel =
    let hpercent:float = (mobMaxHealth - mobCurrentHealth) / mobCurrentHealth
    float eLevel * 0.02 * hpercent

let getEValue weaponType maxHealth (mobMaxHealth,mobCurrentHealth,mobtype) (enchants:Map<Enchant,int>) =
    (0.0, enchants)
    ||> Map.fold(fun e k v ->
        match weaponType,k,mobtype with
        | _, Aiming, _ -> 0.0
        | _, Flame, _ -> 0.0
        | _, InfiniteQuiver, _ -> 0.0
        | _, Piercing, _ -> 0.0
        | _, Punch, _ -> 0.0
        | _, Snipe, _ -> 0.0
        | Bow, Power, _ -> float v * 0.08
        | _, Power, _ -> 0.0
        | Sword,BaneOfArthropods,Some Arthropod -> float v * 0.08
        | _, BaneOfArthropods, _ -> 0.0
        | _, Cubism, Some Cubish -> float v * 0.1
        | _,Cubism, _ -> 0.0
        | _,DragonHunter, Some Dragon -> float v * 0.08
        | _,DragonHunter, _ -> 0.0
        | Sword,EnderSlayer, Some Ender -> float v * 0.12
        | _,EnderSlayer, _ -> 0.0
        | Sword,FirstStrike,_ -> float v * 0.25
        | Bow,FirstStrike,_ -> 0.0
        | Sword,Execute,_ ->
            getExecuteBonus mobMaxHealth mobCurrentHealth v
        | _,Execute,_ -> 0.0
        | Sword,GiantKiller,_ ->
            getGiantKillerBonus maxHealth mobMaxHealth v
        | Bow,GiantKiller,_ -> 0.0
        | _,Impaling, Some Impale -> float v * 0.125
        | _,Impaling, _ -> 0.0
        | Bow, Sharpness,_ -> 0.0
        | Sword,Sharpness,_ -> float v * 0.05
        | Sword,Smite, Some Smiteish -> float v * 0.08
        | _,Smite, _ -> 0.0

        |> fun v -> e + v
    )

let getBasedmg (wd:float) strength = 
    let b = 5.0 + wd + strength / 5.0
    b * (1.0 + strength / 100.0)

// ring of love is weapon bonus
let getMult cl enchants wb =
    1.0 + getCombatLevelValue cl + enchants + wb

let getNormalDamage (bd:float) mult armorbonus =
    bd * mult * armorbonus 
let getTotalCritDamage bd mult armorbonus critdmg = 
    bd * mult * armorbonus * (1.0 + critdmg * 0.01)

let finalweighted cc bd mult armorbonus critdmg =
    let crit = getTotalCritDamage bd mult armorbonus critdmg
    let noncrit = getNormalDamage bd mult armorbonus
    crit * cc + (noncrit - (1.0 - cc))

type WeaponBonus = // TODO: figure out if this covers weapon bonuses
    | Tarantula4thShot
    | RingOfLoveProc
    | MoreDmg
    with
        static member All =
            [
                Tarantula4thShot
                RingOfLoveProc
                MoreDmg
            ]

type DmgStat =
    // | WeaponDmg
    | CritChance
    | CritDmg
    | Strength
    with
        static member All =
            [
                // WeaponDmg
                CritChance
                CritDmg
                Strength
            ]
