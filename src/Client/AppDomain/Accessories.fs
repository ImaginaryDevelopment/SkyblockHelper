module AppDomain.Accessories

open SkyblockHelper

type Accessory = {
    Name:string
    Rarity: Rarity
    Obtained: string
    // StatChanges: DmgStat list

    // Effect: string
    // not all upgrades use the lower version for creating another in the line
    UpgradedForm: Accessory option
} with
    static member TryFindBase name (x:Accessory list) =
        let rec findBase (acc:Accessory) =
            if acc.Name = name then
                Some acc
            else
                match acc.UpgradedForm |> Option.bind findBase with
                | Some _ -> Some acc
                | None -> None
        x
        |> List.tryPick findBase

    static member Unfold (x:Accessory) =
        let rec getForms (x:Accessory) =
            [
                yield x
                yield! x.UpgradedForm |> Option.map getForms |> Option.defaultValue List.empty
            ]
        getForms x

    static member TryFindForm name (x:Accessory) =
        Accessory.Unfold x
        |> List.tryFind(fun acc -> acc.Name = name)

    static member GetUpgradeNames (x:Accessory) =
        let rec getUpgradeNames (x:Accessory) =
            [
                yield x.Name, x.Rarity
                yield! x.UpgradedForm |> Option.map getUpgradeNames |> Option.defaultValue List.empty
            ]
        getUpgradeNames x


type Reforge =
    | Demonic
    | Forceful
    | Gentle
    | Godly
    | Hurtful
    | Keen
    | Strong
    | Superior
    | Unpleasant
    | Zealous
    with 
        static member All =
            [
                Demonic
                Forceful
                Gentle
                Godly
                Hurtful
                Keen
                Strong
                Superior
                Unpleasant
                Zealous
            ]

let private noUpgrade name r obt =
    {
        Name= name
        Rarity= r
        Obtained= obt
        UpgradedForm= None
        // StatChanges= List.empty
    }

let private threeChain nameBase (r1,o1) (r2,o2) (r3,o3) =
    let tali,ring,arti =
        sprintf "%s Talisman" nameBase, sprintf "%s Ring" nameBase, sprintf "%s Artifact" nameBase
    let o2 = sprintf "%s + %s" o2 tali
    let o3 = sprintf "%s + %s" o3 ring
    {
        Name= tali
        Rarity= r1
        Obtained= o1
        UpgradedForm= Some {
            Name= ring
            Rarity= r2
            Obtained= o2
            UpgradedForm= Some <| noUpgrade arti r3 o3
        }
    }
let accessories = [
    threeChain "Zombie"
        (Common, "Adventurer Merchant - 500 coins")
        (Uncommon, "Zombie Slayer 2 - 64 Rev Flesh")
        (Rare, "Zombie Slayer 7 - 32 E-Iron + 16 E-Diamond + 48 Rev Viscera")
    noUpgrade "Skeleton Talisman" Common "Adventurer Merchant - 500 coins"
    threeChain "Intimidation"
        (Common,  "Adventurer Merchant - 10,000 coins")
        (Uncommon,  "Spooky Festival - 100 Green Candy + Intimidation Talisman")
        (Rare,  "Spooky Festival - 100 Purple Candy + Intimidation Ring")
    noUpgrade "Scavenger Talisman" Common "Adventurer Merchant 10,000 coins"
    noUpgrade "Talisman of Coins" Common "Emerald 2 - 20 Emerald + 5 Gold Ingot"
    noUpgrade "Village Affinity Talisman" Common "Adventurer Merchant 2,500 coins"
    noUpgrade "Mine Affinity Talisman" Common "Adventurer Merchant 2,500 coins"
    noUpgrade "Farming Talisman" Common "Wheat 4 - 5 Hay Bale + 4 Seed"
    threeChain "Speed"
        (Common,  "Sugar Cane 2 - 108 Sugar Cane")
        (Uncommon, "Sugar Cane 5 - 96 E-Sugar + Speed Talisman")
        (Rare, "48 E-Sugar Cane + Speed Ring")
    noUpgrade "Fire Talisman" Common "Blaze Rod 5 - 9 E-Blaze Powder"
    noUpgrade "Vaccine Talisman" Common "Potato 3 - 9 Poisonous Potato"
    {
        Name= "Wolf Talisman"
        Rarity = Common
        Obtained= "1/200 (0.5%) drop from Old Wolves"
        UpgradedForm= Some <| noUpgrade "Wolf Ring" Rare "14 E-Bone + 1 Weak Wolf Catalyst + Wolf Talisman"
    }
    noUpgrade "Night Vision Charm" Common "Mushroom 7 - 4 E-Brown Mushroom + 4 E-Red Mushroom"
    threeChain "Feather"
        (Common, "Feather 4 - 108 Feather")
        (Uncommon, "Feather 7 - 7 E-Feather + Feather Talisman")
        (Rare, "Feather 9 - 128 E-Feather + Feather Ring")
    threeChain "Potion Affinity"
        (Common, "Nether Wart 3 - 144 Nether Wart")
        (Uncommon, "Nether Wart 7 - 8 E-Nether Wart + Potion Affinity Talisman")
        (Rare, "Nether Wart 9 - 256 E-Nether Wart + Potion Afinity Ring")
    threeChain "Sea Creature"
        (Common, "Sponge 4 - 18 Sponge")
        (Uncommon, "Sponge 6 - 2 E-Sponge + 6 Sponge")
        (Rare, "Sponge 8 - 64 E-Sponge")
    {
        Name= "Campfire Initiate Badge"
        Rarity= Common
        Obtained= "Trial of Fire 1-4"
        UpgradedForm= Some {
            Name= "Campfire Adept Badge"
            Rarity= Uncommon
            Obtained= "Trial of Fire 5-8 + 160 Dark Oak"
            UpgradedForm= Some {
                Name= "Campfire Cultist Badge"
                Rarity= Rare
                Obtained= "Trial of Fire 9-13 + 1000 Spruce"
                UpgradedForm= Some {
                    Name= "Campfire Scion Badge"
                    Rarity= Epic
                    Obtained= "Trial of Fire 14-21 + 100 E-Acacia"
                    UpgradedForm= Some <| noUpgrade "Campfire God Badge" Legendary "Trial of Fire 22-30 + 1500 E-Jungle"
                } 
            }
        }
    }
    {
        Name= "Shiny Yellow Rock"
        Rarity= Common
        Obtained= "Romeo Quest - 2 Yellow Rock"
        UpgradedForm= Some {
            Name= "YellowRock of Love"
            Rarity= Common
            Obtained= "Romeo Quest - 15 Poppy"
            UpgradedForm= Some{
                Name= "Mediocre Ring of Love"
                Rarity= Uncommon
                Obtained= "Romeo Quest - 64 Emerald"
                UpgradedForm= Some {
                    Name= "Rubbish Ring of Love"
                    Rarity= Uncommon
                    Obtained= "Romeo Quest - 1 E-Red Mushroom Block"
                    UpgradedForm= Some {
                        Name= "Modes Ring of Love"
                        Rarity= Rare
                        Obtained= "Romeo Quest - 1 Rabbit VI Potion with Coffee base"
                        UpgradedForm= Some {
                            Name= "Refined Ring of Love"
                            Rarity= Rare
                            Obtained= "Romeo Quest - 1 E-Lava Bucket"
                            UpgradedForm= Some {
                                Name= "Class Ring of Love"
                                Rarity= Rare
                                Obtained= "Romeo Quest - 1292 Base Mana"
                                UpgradedForm= Some {
                                    Name= "Exquisite Ring of Love"
                                    Rarity= Epic
                                    Obtained= "Romeo Quest - Emerald Blade"
                                    UpgradedForm= Some {
                                        Name="Invaluable Ring of Love"
                                        Rarity= Epic
                                        Obtained= "Romeo Quest - Flower Minion"
                                        UpgradedForm= Some <|noUpgrade "Legendary Ring of Love" Legendary "Romeo Quest - any Tuxedo from Tailor"
                                    }
                                }
                            }
                        }

                    }
                }
            }
        }
    }
    {
        Name= "Healing Talisman"
        Rarity= Common
        Obtained= "Lily Pad 3 - 144 Lily Pad"
        UpgradedForm= Some <| noUpgrade "Healing Ring" Uncommon "Lily Pad 8 - 4 E-Lily Pad + Healing Talisman"
    }
    noUpgrade "Wood Affinity Talisman" Uncommon "Oak Wood 8 - 1 E-Oak + 8 Oak Leaves"
    noUpgrade "Lava Talisman" Uncommon "Magma Cream 7 - 9 E-Magma Cream"
    threeChain "Spider"
        (Uncommon, "Spider's Den - Brood Mother")
        (Rare, "Spider Slayer 1 - 64 Tarantula Web")
        (Epic, "Spider Slayer 7 - 32 Tarantula Silk + 32 E-Emerald")
    noUpgrade "Magnetic Talisman" Uncommon "Emerald 3 - 128 Emerald + 32 Redstone"
    noUpgrade "Gravity Talisman" Uncommon "Obsidian 3 - 144 Obsidian"
    noUpgrade "Farmer Orb" Uncommon "Pumpkin 6 - 8 E-Pumpkin + 1 E-Glowstone"
    {
        Name="Broken Piggy Bank"
        Rarity=Uncommon
        Obtained="Die with a cracked piggy bank in your inventory"
        UpgradedForm=Some {
            Name="Cracked Piggy Bank"
            Rarity= Uncommon
            Obtained="Die with a piggy bank in your inventory"
            UpgradedForm= Some <| noUpgrade "Piggy Bank" Uncommon "Porkchop 5 - 40 E-Pork + 1 Chest"
        }
    }
    threeChain "Red Claw"
        (Uncommon, "Wolf Slayer 1 - 8 Wolf Tooth + 1 E-Bone")
        (Rare, "Wolf Slayer 5 - 8 Golden Tooth + 32 E-Leather")
        (Epic, "Wolf Slayer 5 - 1 Red Claw Egg + 54 Golden Tooth + 128 E-Leather")
    threeChain "Candy"
        (Uncommon, "Spooky Festival - 16 Green Candy")
        (Rare, "Spooky Festival - 64 Green Candy")
        (Epic, "Spooky Festival - 64 Purple Candy")
    {
        Name="Hunter Talisman"
        Rarity= Uncommon
        Obtained= "Wolf Slayer 7 - 64 Golden Tooth + 32 E-Rotten Flesh"
        UpgradedForm= Some <| noUpgrade "Hunter Ring" Rare "Wolf Slayer 7 - 256 Golden Tooth + 1 Grizzly Bait"
    }
    noUpgrade "New Year Cake Bag" Uncommon "New Year Celebration - 250,000 coins"
    noUpgrade "Wolf Paw" Uncommon "Woods Race - 32 Seconds or less"
    {
        Name="Shady Ring"
        Rarity= Uncommon
        Obtained= "Lucius Tier 1 - Buy 5 things from Dark Auction + 500,000 coins"
        UpgradedForm= Some {
            Name="Crooked Artifact"
            Rarity= Rare
            Obtained= "Lucius Tier 2 - Buy 10 things from Dark Auction + 2,000,000 coins + Shady Ring"
            UpgradedForm= Some <| noUpgrade "Seal of the Family" Epic "Lucius Tier 3 - Buy 15 things from Dark Auction + 10,000,000 coins + Crooked Artifact"
        }
    }
    noUpgrade "Frozen Chicken" Rare "Season of Jerry - Chicken Race Stage 3"
    noUpgrade "Haste Ring" Rare "Cobblestone 8 - 256 E-Cobble"
    noUpgrade "Night Crystal" Rare "Quartz 7 - 4 E-Quartz + 1 E-Quartz Block"
    noUpgrade "Day Crystal" Rare "Quartz 8 - 4 E-Quartz + 1 E-Quartz Block"
    noUpgrade "Fish Affinity Talisman" Rare "Fishing 19 - Water Hydra"
    noUpgrade "Bait Ring" Rare "Ink Sac 8 - 288 E-Ink Sac"
    threeChain "Bat"
        (Rare, "Bats")
        (Epic, "Spooky Festival - 64 Green Candy")
        (Legendary, "Spooky Festival - Purple Candy")
    noUpgrade "Pig's Foot" Rare "End Race - 48 seconds or less"
    noUpgrade "Devour Ring" Rare "Zombie Slayer 5 - 2 E-Raw Chicken + 4 E-Raw Salmon + 39 Rev Viscera"
    noUpgrade "Survivor Cube" Rare "Reward for Spider Slayer 7"
    noUpgrade "Tarantula Talisman" Epic "Spider Slayer Drop Broodfather Tier 3"
    noUpgrade "Ender Artifact" Epic "Dark Auction"
    noUpgrade "Melody's Hair" Epic "Melody"
    noUpgrade "Wither Artifact" Epic "Dark Auction"
    noUpgrade "Experience Artifact" Epic "Lapis 9 - 9 E-Lapis Block"
]