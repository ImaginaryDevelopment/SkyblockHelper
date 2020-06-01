module AppDomain.Collections.Weapons

open SkyblockHelper
open Shared.Helpers

type CraftType =
    | Slayer of Slayer*int
    | Collection of string
type Weapon = {
    Name: string
    Rarity: Rarity 
    CraftType: CraftType option
    UpgradesTo: string
}
module private Internal =

    let fWeapon name rarity =
        {   Name= name
            Rarity= rarity
            CraftType= None
            UpgradesTo= null
        }
    let cWeapon name rarity collection =
        {
            Name= name
            Rarity= rarity
            CraftType= Some <| Collection collection
            UpgradesTo= null
        }

    let sWeapon name rarity s slvl =
        {
            Name= name
            Rarity= rarity
            CraftType = Some (Slayer (s,slvl))
            UpgradesTo= null
        }

open Internal

let swords = [
    fWeapon "Yeti Sword" Legendary
    {   Name= "Reaper Scythe"
        Rarity= Legendary
        CraftType= Some <| Slayer (Zombie,7)
        UpgradesTo= null
    }
    sWeapon "Pooch Sword" Legendary Wolf 6
    cWeapon "Pigman Sword" Legendary "Porkchop 9"
    fWeapon "Midas Sword" Legendary
    fWeapon "Aspect of the Dragon" Legendary
    fWeapon "Silk-edge Sword" Epic
    {sWeapon "Shaman Sword" Epic Wolf 3 with UpgradesTo= "Pooch Sword"}
    {sWeapon "Scorpion Foil" Epic Spider 6 with UpgradesTo= "Thick Scorpion Foil"}
    sWeapon "Reaper Falchion" Epic Zombie 6
    //crafted, but requires no collection
    cWeapon "Ornate Zombie Sword" Epic null
    {sWeapon "Leaping Sword" Epic Spider 9 with UpgradesTo= "Silk-edge Sword"}
    cWeapon "Ink Wand" Epic "Ink Sack 9"
    cWeapon "End Stone Sword" Epic "End Stone 9"
    cWeapon "Emerald Blade" Epic "Emerald 8"
    fWeapon "Ember Rod" Epic
    {cWeapon "Zombie Sword" Rare "Rotten Flesh 7" with UpgradesTo= "Ornate Zombie Sword"}
    {fWeapon "Tactician's Sword" Rare with UpgradesTo= "Thick Tactician's Sword"}
    {sWeapon "Revenant Falchion" Rare Zombie 3 with UpgradesTo= "Reaper Falchion"}
    sWeapon "Recluse Fang" Rare Spider 2
    fWeapon "Raider Axe" Rare
    cWeapon "Golem Sword" Rare "Iron 8"
    cWeapon "Frozen Scythe" Rare "Ice 8"
    sWeapon "Edible Mace" Rare Wolf 5
    cWeapon "Aspect of the End" Rare "Ender Pearl 8"
    cWeapon "Silver Fang" Uncommon "Ghast Tear 6"
    cWeapon "Prismarine Blade" Uncommon "Prismarine Shard 2"
    fWeapon "Hunter Knife" Uncommon
    fWeapon "Flaming Sword" Uncommon
    fWeapon "End Sword" Uncommon
    cWeapon "Cleaver" Uncommon "Gold 2"
    {cWeapon "Undead Sword" Common null with UpgradesTo="Revenant Falchion"}
    {cWeapon "Spider Sword" Common null with UpgradesTo="Recluse Fang"}
    fWeapon "Rogue Sword" Common
    fWeapon "Fancy Sword" Common
    {fWeapon "Aspect of the Jerry" Common with UpgradesTo="Thick Aspect of the Jerry"}
]

let bows =[
    sWeapon "Mosquito" Legendary Spider 5
    cWeapon "Runaan's" Legendary "Bone 9"
    fWeapon "End Stone" Epic
    cWeapon "Hurricane" Epic "Bone 7"
    sWeapon "Scorpion" Epic Spider 3
    cWeapon "Slime" Epic "Slimeball 9"
    cWeapon "Magma" Epic "Magma Cream 9"
    cWeapon "Explosive" Epic "Gunpowder 9"
    cWeapon "Ender" Rare "Ender Pearl 5"
    cWeapon "Prismarine" Uncommon "Prismarine Shard 5 "
    cWeapon "Savanna" Uncommon "Acacia 7"
    fWeapon "Wither" Uncommon
    fWeapon "Decent" Uncommon
]

type WeaponType = Sword | Bow with
    static member All =
        [
            Sword
            Bow
        ]
    static member Parse =
        function
        | EqualsI (string Sword) -> Some Sword
        | EqualsI (string Bow) -> Some Bow
        | _ -> None
    static member Is x y =
        match x, y with
        | Sword, Sword -> true
        | Bow, Bow -> true
        | _ -> false

