module AppDomain.SalesReference

open Shared
open Shared.Helpers

type Category = Farming | Mining | Combat | WoodsOrFishes with
    static member FromString =
        function
        | EqualsI "farming" -> Some Farming
        | EqualsI "mining" -> Some Mining
        | EqualsI "combat" -> Some Combat
        | EqualsI "WoodsOrFishes" -> Some WoodsOrFishes
        | x ->
            eprintfn "Category.FromString could not interpret %s" x
            None

type ItemForm = {
  Label:string
  Div:int option
  Vend:float option
  Asterisk:string option
} with
    static member CreateEmpty lbl div = {Label=lbl;Div=Some div;Vend=None;Asterisk=None}
    static member CreateSpecial lbl = {Label=lbl;Div=None;Vend=None;Asterisk=None}
    static member MakeStandards (name,?adds:{| plainsuffix:string option; blockname:string option |}) =
        let plainSuffix =
            adds
            |> Option.bind(fun a -> a.plainsuffix)
            |> Option.bind (|ValueString|_|)
            |> Option.defaultValue ""
        let blockname =
            adds
            |> Option.bind(fun a -> a.blockname)
            |> Option.bind (|ValueString|_|)
            |> Option.defaultValue ""
        [
            ItemForm.CreateEmpty (name + (if plainSuffix <> "" then " " + plainSuffix else "")) 1
            ItemForm.CreateEmpty ("Enchanted " + name) 160
            ItemForm.CreateEmpty (if blockname <> "" then blockname else (sprintf "Enchanted %s Block" name)) (160*160)
        ]

type Preconfiguration = {
  Name:string
  Category:Category
  Forms:ItemForm list
} with
    // static member CreateEmpty name category = {Name=name;Category=Category;Forms=List.empty}
    static member MakeSimple name category =
        {Name=name;Category=category;Forms=[
            ItemForm.CreateEmpty name 1
        ]}
    static member MakeDual (name,category,?emult) =
        let emult = Option.defaultValue 160 emult 
        {Name=name;Category=category;Forms= [
            ItemForm.CreateEmpty name 1
            ItemForm.CreateEmpty (sprintf "Enchanted %s" name) emult
        ]}

// type Suffixes = {plain?:string,enchanted?:string,eblock?:string}
let makeStandardMining (name:string,plainsuffix:string option) : Preconfiguration  = {
  Name= name
  Category= Category.Mining
  Forms= ItemForm.MakeStandards(name,{|blockname=None;plainsuffix=plainsuffix|})
}

let preconfigurations = [
  {
    Name="Wheat"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Wheat" 1
      ItemForm.CreateEmpty "Enchanted Bread" 60
      ItemForm.CreateEmpty "Hay Bale" 9
      ItemForm.CreateEmpty "Enchanted Hay Bale" (9*16*9)
    ]
  }
  {
    Name="Carrot"
    Category=Farming
    Forms=[
      {ItemForm.CreateEmpty "Carrot" 1 with  Vend= Some (float <| 3m / 7m) }
      ItemForm.CreateEmpty "Enchanted Carrot" 160
      ItemForm.CreateSpecial "Enchanted Carrot on a Stick"
      ItemForm.CreateSpecial "Enchanted Golden Carrot"
    ]
  }
  {
    Name="Potato"
    Category=Farming
    Forms=ItemForm.MakeStandards("Potato", {| blockname=Some "Enchanted Baked Potato";plainsuffix=None |})
  }
  Preconfiguration.MakeDual("Pumpkin",Farming)
  {
    Name="Melon"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Melon" 1
      ItemForm.CreateEmpty "Enchanted Melon" 160
      ItemForm.CreateSpecial "Enchanted Glistening Melon"
      ItemForm.CreateEmpty "Enchanted Melon Block" <| 160*160
    ]
  }
  Preconfiguration.MakeDual("Seeds", Farming)
  {
    Name="Red Mushrooms"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Red Mushroom" 1
      ItemForm.CreateEmpty "Enchanted Red Mushroom" 160
      ItemForm.CreateEmpty "Red Mushroom Block" 9
      ItemForm.CreateEmpty "Enchanted Red Mushroom Block" <| 9*64*9
    ]
  }
  {
    Name="Brown Mushrooms"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Brown Mushroom" 1
      ItemForm.CreateEmpty "Enchanted Brown Mushroom" 160
      ItemForm.CreateEmpty "Brown Mushroom Block" 9
      ItemForm.CreateEmpty "Enchanted Brown Mushroom Block" <| 9*64*9
    ]
  }
  {
    Name="Cocoa Beans"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Cocoa Beans" 1
      ItemForm.CreateEmpty "Enchanted Cocoa Beans" 160
      {ItemForm.CreateSpecial "Enchanted Cookie" with Asterisk= Some "Requires Wheat"}
    ]
  }
  {
    Name="Cactus"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Cactus" 1
      ItemForm.CreateEmpty "Enchanted Cactus Green" 160
      ItemForm.CreateEmpty "Enchanted Cactus" <| 160*160
    ]
  }
  {
    Name="Sugar Cane"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Sugar Cane" 1
      ItemForm.CreateEmpty "Enchanted Sugar" 160
      ItemForm.CreateEmpty "Enchanted Paper" <| 64 * 3
      ItemForm.CreateEmpty "Enchanted Sugar Cane" <| 160*160
    ]
  }
  Preconfiguration.MakeDual("Feather",Farming)
  Preconfiguration.MakeDual("Leather", Farming)
  Preconfiguration.MakeDual("Raw Beef", Farming)
  {
    Name="Pork"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Raw Pork" 1
      ItemForm.CreateEmpty "Enchanted Pork" 160
      ItemForm.CreateEmpty "Enchanted Grilled Pork" <| 160 * 160
    ]
  }
  {
    Name="Chicken(Egg)"
    Category=Farming
    Forms=[
      {ItemForm.CreateEmpty "Enchanted Egg" 1 with Asterisk= Some "144(16*9) eggs"}
      {ItemForm.CreateSpecial "EnchantedCake" with Asterisk= Some "Needs Enchanted Eggs, Wheat, Milk, & Enchanted Sugar"}
      {(ItemForm.CreateEmpty "Super Enchanted Egg" <| 16*9) with Asterisk= Some "20,736 eggs"}
      // ItemForm.CreateEmpty "Enchanted Grilled Pork" 160 * 160
    ]
  }
  Preconfiguration.MakeDual("Raw Chicken", Farming)
  {
    Name="Cobblestone"
    Category=Mining
    Forms=[
      ItemForm.CreateEmpty "Cobblestone" 1
      ItemForm.CreateEmpty "Enchanted Cobblestone" 160
      {ItemForm.CreateEmpty "Auto Smelter" 64 with Asterisk= Some "Needs 1 coal"}
    ]
  }
  {
    Name="Mutton"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Raw Mutton" 1
      ItemForm.CreateEmpty "Enchanted Mutton" 160
      ItemForm.CreateEmpty "Enchanted Cooked Mutton" <| 160*160
    ]
  }
  {
    Name="Rabbit"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Raw Rabbit" 1
      ItemForm.CreateEmpty "Enchanted Raw Rabbit" 160
    ]
  }
  {
    Name="Rabbit(Feetsies)"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Rabbit\"s Foot" 1
      ItemForm.CreateEmpty "Enchanted Rabbit Foot" 160
    ]
  }
  {
    Name="Rabbit(Hide)"
    Category=Farming
    Forms=[
      ItemForm.CreateEmpty "Rabbit Hide" <| 1
      ItemForm.CreateEmpty "Enchanted Rabbit Hide" <| 64*9
    ]
  }
  Preconfiguration.MakeDual("Nether Wart",Farming)
  {
    Name="Coal"
    Category=Mining
    Forms=[
      ItemForm.CreateEmpty "Coal Ore" 1
      ItemForm.CreateEmpty "Enchanted Coal" 160
      {(ItemForm.CreateEmpty "Enchanted Charcoal" <| 128*160) with Asterisk = Some "Needs 32 Wood"}
      {(ItemForm.CreateEmpty "Enchanted Block of Coal" <| 160*160) with Asterisk = Some "Made from Enchanted Coal"}
    ]
  }
  makeStandardMining("Iron",Some "Ingot")
  makeStandardMining("Gold",Some "Ingot")
  makeStandardMining("Lapis",None)
  makeStandardMining("Emerald",None)
  makeStandardMining("Redstone",None)
  makeStandardMining("Quartz",None)
  Preconfiguration.MakeDual("Obsidian", Mining)
  {
    Name="Glowstone"
    Category=Mining
    Forms=[
      ItemForm.CreateEmpty "Glowstone Dust" 1
      ItemForm.CreateEmpty "Enchanted Glowstone Dust" 160
      ItemForm.CreateEmpty "Enchanted Glowstone"<| 48 * 4 * 160
    ]
  }
  Preconfiguration.MakeSimple "Gravel" Mining
  Preconfiguration.MakeDual("Flint",Mining)
  {
    Name="Ice"
    Category=Mining
    Forms=[
      ItemForm.CreateEmpty "Ice" <| 1
      // is there a packed ice -> enchanted ice recipe?
      ItemForm.CreateEmpty "Packed Ice" <| 9
      ItemForm.CreateEmpty "Enchanted Ice" 160
      ItemForm.CreateEmpty "Enchanted Packed Ice" <| 160 * 160
      // use costs to determine price from 
      // {Label="Frost Walker book", isCost=true, uses="Ice",vend=""}
      // {Label="Ice Minion I", isCost=true, uses="Ice"; Vend=""}
    ]
  }
  Preconfiguration.MakeSimple "Netherrack" Mining
  Preconfiguration.MakeDual("Sand", Mining)
  Preconfiguration.MakeDual("End Stone",Mining)
  {
    Name="Snow"
    Category=Mining
    Forms=[
      ItemForm.CreateEmpty "Snowball" 1
      ItemForm.CreateEmpty "Snow Block" 4
      ItemForm.CreateEmpty "Enchanted Snow Block" <| 4 * 160
    ]
  }
  Preconfiguration.MakeDual("Rotten Flesh", Combat)
  Preconfiguration.MakeDual("Bone", Combat)
  Preconfiguration.MakeDual("String", Combat)
  {
    Name="Spider Eye"
    Category= Combat
    Forms=[
      ItemForm.CreateEmpty "Spider Eye" 1
      ItemForm.CreateEmpty "Enchanted Spider Eye" 160
      {ItemForm.CreateEmpty "Enchanted Fermented Spider Eye" 64 with Asterisk = Some "Needs 64 sugar and 64 brown mushrooms"}
    ]
  }
  {
    Name="Gunpowder"
    Category=Combat
    Forms=[
      ItemForm.CreateEmpty "Gunpowder" 1
      ItemForm.CreateEmpty "Enchanted Gunpowder" 160
      {(ItemForm.CreateEmpty "Enchanted Firework Rocket" <| 160*64) with Asterisk = Some "Needs 16 paper"}
    ]
  }
  {
    Name="Ender Pearl"
    Category=Combat
    Forms=[
      ItemForm.CreateEmpty "Ender Pearl" <| 1
      ItemForm.CreateEmpty "Enchanted Ender Pearl" <| 4*5
      {(ItemForm.CreateEmpty "Enchanted Eye of Ender" <| 4*5*16) with Asterisk = Some "Needs 64 Blaze Powder (32 Blaze Rods)"}
    ]
  }
  Preconfiguration.MakeDual("Ghast Tear", Combat, 5)
  {
    Name="Slimeball"
    Category=Combat
    Forms=ItemForm.MakeStandards("Slimeball",{|blockname=Some "Enchanted Slime Block";plainsuffix=None|})
  }
  {
    Name="Blaze Rod"
    Category=Combat
    Forms=[
      ItemForm.CreateEmpty "Blaze Rod" <| 1
      ItemForm.CreateEmpty "Enchanted Blaze Powder" 160
      ItemForm.CreateEmpty "Enchanted Blaze Rod" <| 160 * 160
    ]
  }
  Preconfiguration.MakeDual("Magma Cream",Combat)

  Preconfiguration.MakeDual("Oak",WoodsOrFishes)
  Preconfiguration.MakeDual("Spruce", WoodsOrFishes)
  Preconfiguration.MakeDual("Birch", WoodsOrFishes)
  Preconfiguration.MakeDual("Dark Oak", WoodsOrFishes)
  Preconfiguration.MakeDual("Acacia", WoodsOrFishes)
  Preconfiguration.MakeDual("Jungle", WoodsOrFishes)
  {
    Name="Raw Fish"
    Category= WoodsOrFishes
    Forms=ItemForm.MakeStandards("Raw Fish",{| blockname=Some "Enchanted Cooked Fish";plainsuffix=None |})
  }
  {
    Name="Salmon"
    Category=WoodsOrFishes
    Forms=ItemForm.MakeStandards("Salmon", {| blockname=Some "Enchanted Cooked Salmon";plainsuffix=None |})
  }
  Preconfiguration.MakeDual("Clownfish",WoodsOrFishes)
  Preconfiguration.MakeDual("Pufferfish",WoodsOrFishes)
  Preconfiguration.MakeDual("Prismarine Shard",WoodsOrFishes)
  Preconfiguration.MakeDual("Prismarine Crystals",WoodsOrFishes)
  Preconfiguration.MakeDual("Clay", WoodsOrFishes)
  Preconfiguration.MakeDual("Lily Pad", WoodsOrFishes)
  Preconfiguration.MakeDual("Ink Sac", WoodsOrFishes)
  Preconfiguration.MakeDual("Sponge", WoodsOrFishes)
]
type NameValue = {
    Name:string
    Value:float
}

type VendorReference = {
  Name: string
  Values: NameValue list
}
let crv (name,value) = {Name=name;Value=value}
let referenceValues = [
    {Name="Adventurer";Values=[
      crv("Rotten flesh",8.0)
      crv("Bone",8.0)
      crv("String",10.0)
      crv("Gunpowder",10.0)
    ]}
    {Name="Lumber Merchant";Values=[
      crv("Oak Wood",5.0)
      crv("Birch Wood",5.0)
      crv("Spruce Wood",5.0)
      crv("Dark Oak Wood",5.0)
      crv("Acacia Wood",5.0)
      crv("Jungle Wood",5.0)
    ]}
    {Name="Farm Merchant";Values=[
      crv("Wheat", 2.33)
      crv("Carrot",2.33)
      crv("Potato",2.33)
      crv("Melon",2.0)
      crv("Sugar Cane",5.0)
      crv("Pumpkin",8.0)
      crv("Cocoa Beans",5.0)
      crv("Red Mushroom",12.0)
      crv("Brown Mushroom",12.0)
      crv("Sand",4.0)
      crv("Enchanted Bonemeal",2.0)
    ]}
    {Name="Mine Merchant";Values=[
      crv("Coal", 8.0 / 2.0)
      crv("Iron Ingot", 22.0 / 4.0)
      crv("Gold Ingot", 12.0 / 2.0)
      crv("Gravel", 12.0 / 2.0)
      crv("Cobblestone", 3.0)
    ]}
    {Name="Fish Merchant"; Values=[
      crv("Raw Fish", 20.0)
      crv("Raw Salmon", 30.0)
      crv("Clownfish", 100.0)
      crv("Pufferfish", 40.0)
    ]}
];

