module AppDomain.Collections.Armor

open SkyblockHelper
open Shared.Helpers

type ArmorPartType =
    |Helm
    |Chest
    |Leg
    |Boot
    with
        static member All =
            [
                Helm
                Chest
                Leg
                Boot
            ]

type ArmorPart =
    | Named of name:string * ArmorPartType 
    | Unnamed of ArmorPartType 

type ArmorSetType =
    | Standard of special:string
    | Special of special:string * ArmorPart list

type ArmorSet = string * Rarity option * ArmorSetType

let private stdNo name rarity: ArmorSet = name, Some rarity, Standard null
let private makeHeadless name rarity special : ArmorSet  =
    name, Some rarity, Special(special,[Unnamed Chest; Unnamed Leg; Unnamed Boot])

// https://hypixel-skyblock.fandom.com/wiki/Armor
let armorSets : ArmorSet list = [
    stdNo "Superior Dragon" Legendary
    stdNo "Strong Dragon" Legendary
    stdNo "Young Dragon" Legendary
    stdNo "Wise Dragon" Legendary
    stdNo "Unstable Dragon" Legendary
    stdNo "Old Dragon" Legendary
    stdNo "Protector Dragon" Legendary
    "Perfect VIII-XII",Some Legendary, Standard null
    stdNo "Diver's" Legendary
    stdNo "Bat Person" Legendary
    makeHeadless "Elegant Tuxedo" Legendary "Max Health set to 250, Deal 150% more damage"
    makeHeadless "Fancy Tuxedo" Legendary "Max Health set to 150, Deal 100% more damage"

    "Revenant", Some Epic, Special("Zombie Set",[Unnamed Chest])
    "Mastiff", Some Epic, Standard "Wolf Set"
    "Tarantula", Some Epic, Standard "Spider Set"
    "Perfect IV-VII", Some Epic, Standard null
    stdNo "Ender" Epic
    stdNo "Snow Suit" Epic
    stdNo "Spooky" Epic
    stdNo "Crystal" Epic
    makeHeadless "Cheap Tuxedo" Epic "Max Health set to 75, Deal 50% more damage"
    stdNo "Blaze" Epic
    stdNo "Frozen Blaze" Epic
    stdNo "Magma" Epic
    stdNo "Emerald" Epic
    stdNo "Speedster" Epic
    stdNo "Sponge" Epic
    stdNo "Zombie" Epic

    stdNo "Pack" Epic
    "Monster Raider",None,Special("-35% dmg taken, +35% dmg to monsters", [
        Named("Skeleton's Helmet",Helm)
        Named("Guardian Chestplate", Chest)
        Named("Creeper Pants", Leg)
        Named("Spider's Boots", Boot)
    ])
    "Monster Hunter",Some Rare, Special("-30% dmg taken, +30% dmg to monsters", [
        Named("Skeleton's Helmet",Helm)
        Named("Guardian Chestplate", Chest)
        Named("Creeper Pants", Leg)
        Named("Tarantula's Boots", Boot)
    ])
    "Perfect I-III", Some Rare, Standard null
    stdNo "Golem" Rare
    stdNo "Hardened Diamond" Rare
    stdNo "Growth" Rare
    stdNo "Miner Armor" Rare
    stdNo "Fairy" Rare
    stdNo "Farm" Rare

    stdNo "Lapis" Uncommon
    stdNo "Miner's Outfit" Uncommon
    stdNo "Angler" Common
    stdNo "Leaflet" Common
    stdNo "Cactus" Common
    stdNo "Pumpkin" Common
    stdNo "Mushroom" Common
    stdNo "Farm Suit" Common

]