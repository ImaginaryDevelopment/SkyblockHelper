module AppDomain.EnchantingReference

open Shared.Helpers


type EnchantType =
    | Armor
    | Bow
    | Sword
    | Tool
    with 
     static member All =
        [
            Armor
            Bow
            Sword
            Tool
        ]

type Roman =
    | I
    | II
    | III
    | IV
    | V
    with
        static member toNumber =
            function
            | I -> 1
            | II -> 2
            | III -> 3
            | IV -> 4
            | V -> 5

type EnchantBase = {
    Name:string
    TargetLvl: Roman
    VendorTitle: string
    IsRecommended: bool
}

type Craftable = {
    Base: EnchantBase
    MinEnchantTbl: int
    Collection: string
    CraftLvlCreated: Roman
    Components: string
}

type Enchant =
    | Uncraftable of EnchantBase
    | Craftable of Craftable
    with
        member x.Base =
            match x with
            | Uncraftable eb -> eb
            | Craftable c -> c.Base

let cBase name tlvl vt isRec = {
        Name= name
        TargetLvl= tlvl
        VendorTitle= vt
        IsRecommended= isRec
}
type private EnchantingHelpers =

    static member CEnchant name tlvl meTbl c clc comp vt isRecommended =
        Craftable {
            Base= cBase name tlvl vt isRecommended
            MinEnchantTbl = meTbl
            Collection = c
            CraftLvlCreated = clc
            Components = comp
        }


// cEnchant\(("[^"]+")\s?,\s?"([^"]+)"\s?,\s?(\d+)\s?,\s?("[^"]+")\s?,\s?"([^"]+)"\s?,\s?("[^"]+")
// cBase $1 $2 vt 

let sEnchants = Sword,[
    Craftable {
        Base = cBase "Critical" V 
            "VI - Sven Packmaster - Wolf Slayer IV required" true
        MinEnchantTbl= 44
        Collection= "Diamond 5"
        CraftLvlCreated= IV
        Components= "8 Enchanted Diamond"
    }
    Craftable {
        Base= cBase "Sharpness" V "VI - Dark Auction" true
        MinEnchantTbl= 44
        Collection= "Gravel 4"
        CraftLvlCreated= IV
        Components= "Iron Sword + 8 Flint"
    }

    EnchantingHelpers.CEnchant "Execute" V 40 "Diamond 2" IV "40 Flint + 40 Diamond" null true
    EnchantingHelpers.CEnchant "Giant Killer" V 40 "Ghast 3" IV "8 Ghast Tears" "VI - Dark Auction" true
    EnchantingHelpers.CEnchant "First Strike" IV 24 "Gravel 6" III "4 Enchanted Flint" null true
    EnchantingHelpers.CEnchant "Lethality" V 40 "Obsidian 2" IV "24 Obsidian" null true
    EnchantingHelpers.CEnchant "Cubism" V 44 "Pumpkin 4" IV "32 Pumpkin" null true
    EnchantingHelpers.CEnchant "Ender Slayer" V 48 "Ender Pearl 3" IV "8 Enchanted Ender Pearls" "VI - Pearl Dealer - $1.5m" true
    EnchantingHelpers.CEnchant "Impaling" III 32 "Prismarine Shard 1" II "20 Prismarine Shards" null false
    EnchantingHelpers.CEnchant "Life Steal" III 36 "None needed" II "2 Enchanted Golden Apple" null false
    EnchantingHelpers.CEnchant "Vampirism" V 40 "Ghast 5" IV "8 Enchanted Ghast" "VI - Spooky Festival" false
    EnchantingHelpers.CEnchant "Luck" V 40 "Rabbit 5" IV "8 Rabbit Hide" "IV from gifts" false
    EnchantingHelpers.CEnchant "Looting" III 28 "Gold 3" II "4 Gold Block" "IV from gifts" false
    EnchantingHelpers.CEnchant "Scavenger" III 18 "Gold 6" II "Golden Sword" "IV from gifts" false
    EnchantingHelpers.CEnchant "Experience" III 24 "Lapis 3" II "2 Lapis" null false
    EnchantingHelpers.CEnchant "Venomous" V 46 "Spider Eye 6" IV "8 Enchanted Spider Eye" null false
    EnchantingHelpers.CEnchant "Thunderlord" V 15 "Gunpowder 5" IV "8 Enchanted Gunpowder" null false
    EnchantingHelpers.CEnchant "Cleave" V 40 "Pufferfish 3" IV "40 Pufferfish" null false
]

let aEnchants = Armor, [
    EnchantingHelpers.CEnchant "Protection" V 40 "Iron 3" IV "8 Iron" "VI - Dark Auction" true
    EnchantingHelpers.CEnchant "Growth" V 40 "Dark Oak 7" IV "8 Enchanted Dark Oak" "VI - Dark Auction" true
    EnchantingHelpers.CEnchant "Depth Strider(boots)" III 30 "Pufferfish 4" II "2 Salmon 2 Lily Pad" null false
    EnchantingHelpers.CEnchant "Feather Fall(boots)" V 42 "Feather 2" IV "40 feathers" null true
    Uncraftable { 
                    Name="True Protection(chest)"; TargetLvl= I
                    IsRecommended= false
                    VendorTitle="Birch Park - Howling Cave - $900k"
            }
]

let bEnchants = Bow, [
    EnchantingHelpers.CEnchant "Power" V 38 "Bone 3" IV "40 bone" "VI - Dark Auction" true
    EnchantingHelpers.CEnchant "Aiming" V 48 "Feather 6" IV "1 Compass and 8 Arrows" null true
    EnchantingHelpers.CEnchant "Infinite Quiver" V 44 "String 6" IV "1 Bow" null true
    EnchantingHelpers.CEnchant "Piercing" I 23 "Cactus 6" I "10 Cacti and 1 arrow" null true
    EnchantingHelpers.CEnchant "Cubism" V 44 "Pumpkin 4" IV "32 Pumpkin" null true
    EnchantingHelpers.CEnchant "Snipe" III 27 "Feather 8" II "2 Feather and 2 arrow" null true
    EnchantingHelpers.CEnchant "Impaling" III 32 "Prismarine Shard 1" II "20 Prismarine Shards" null false
]

