module AppDomain.BrewingReference

open Shared.Helpers

type PotionLevel = {Source:string;Lvl:int}

type Potion = {
  Name: string
  IsVanilla: bool
  Bases: string list
  IsDebuff: bool
  Levels: PotionLevel list
}
// type so we can use optional and named params
type private BrewHelpers =
    static member PotPh (name,isVanilla,lvl1source:string,?isDebuff:bool,?bases:string list): Potion =
      {
        Name= name
        IsVanilla= isVanilla
        IsDebuff= isDebuff |> Option.defaultValue false
        Bases= bases |> Option.defaultValue ["Awkward"]
        Levels= [{Source=lvl1source; Lvl=1}]
      }

let potions : Potion list = [
  { Name="Speed";IsVanilla=true; IsDebuff=false
    Bases=[
      "Awkward"
      "Cheap Coffee"
      "Decent Coffee"
    ]
    Levels=[
      {Source="sugar";Lvl=1}
      {Source="esugar"; Lvl=3}
      {Source="eSugarcane"; Lvl=5}
    ]
  }
  {
    Name="Jump Boost"; IsVanilla=true;IsDebuff=false
    Bases=[

    ]
    Levels=[
      {Source="Rabbit\"s Foot";Lvl=1}
    ]
  }
  {
    Name="Healing"; IsVanilla= true;IsDebuff=false
    Bases=[]
    Levels=[
      {Source="Glistering Melon";Lvl=1}
      {Source="Encahnted Melon"; Lvl=3}
      {Source="Encahnted Glistering Melon"; Lvl=5}
    ]
    }
  BrewHelpers.PotPh("Poison", true,"Spider Eye",true)
  { Name="Water Breathing"; IsVanilla= true;IsDebuff=false
    Bases=[]
    Levels=[
      {Source="Pufferfish";Lvl=1}
      {Source="Enchanted Pufferfish";Lvl=3}]
  }
  BrewHelpers.PotPh("Fire Resistance", true,"Magma Cream",false)
  BrewHelpers.PotPh("Night Vision", true,"Golden Carrot",false)
  { Name="Strength"; IsVanilla= true;IsDebuff=false
    Bases=[]
    Levels=[
      {Source="Blazing Powder";Lvl=1}
      {Source="Enchanted Blazing Powder";Lvl=3}
      {Source="Enchanted Blazing Rod";Lvl=5}
    ]}
  BrewHelpers.PotPh("Invisibility", true,"Fermented Spider Eye",false,["Night Vision"])
  { Name="Regeneration"; IsVanilla= true; IsDebuff=false
    Bases=[]
    Levels=[
        {Source="Ghast Tear";Lvl=1}
        {Source="Enchanted Ghast Tear";Lvl=5}
  ]}
  { Name="Weakness"; IsVanilla= true; IsDebuff=true
    Bases=[]
    Levels=[
      {Source="Fermented Spider Eye";Lvl=1}
      {Source="Enchanted Spider Eye"; Lvl=3}
      {Source="Enchanted Fermented Spider Eye";Lvl=5}
    ]
  }
  BrewHelpers.PotPh("Slowness", true,"Fermented Spider Eye",true,["Speed"])
  BrewHelpers.PotPh("Damage", true,"Fermented Spider Eye",true,["Health"])
  // start nonVanilla
  BrewHelpers.PotPh("Haste", false,"Coal")
  { Name="Rabbit";IsVanilla= false; IsDebuff=false
    Bases=[]
    Levels=[
      {Source="Raw Rabbit";Lvl=1}
      {Source="Enchanted Rabbit\"s Foot";Lvl=3}
    ]
  }
  BrewHelpers.PotPh("Burning", false,"Red Sand",true)
  BrewHelpers.PotPh("Knockback", false,"Slimeball")
  BrewHelpers.PotPh("Venomous", false,"Poisonous Potato",true)
  BrewHelpers.PotPh("Stun", false,"Obsidian")
  BrewHelpers.PotPh("Archery", false,"Feather")
  { Name="Absorption"; IsVanilla= false; IsDebuff= false
    Bases=[]
    Levels=[
      {Source="Gold Ingot";Lvl=1}
      {Source="Enchanted Gold"; Lvl=3}
      {Source="Enchanted Gold Block";Lvl=5}
    ]
  }
  { Name="Adrenaline"; IsVanilla= false; IsDebuff= false
    Bases=[]
    Levels=[
      {Source="Cocoa Beans";Lvl=1}
      {Source="Enchanted Cocoa Beans"; Lvl=3}
      {Source="Enchanted Cookie";Lvl=5}
    ]
  }
  { Name="Critical";IsVanilla=false;IsDebuff=false
    Bases=[]
    Levels=[{Source="Flint";Lvl=1}]
  }
  BrewHelpers.PotPh("Dodge", false,"Raw Salmon")
  BrewHelpers.PotPh("Agility", false,"Enchanted Cake")
  BrewHelpers.PotPh("Wounded", false,"Netherrack",true)
  BrewHelpers.PotPh("Experience", false,"Lapis Lazuli")
  { Name="Resistance"; IsVanilla= false; IsDebuff= false
    Bases=[]
    Levels=[
      {Source="Cactus";Lvl=1}
      {Source="Enchanted Cactus Green"; Lvl=3}
      {Source="Enchanted Cactus";Lvl=5}
    ]
  }
  { Name="Mana"; IsVanilla= false; IsDebuff= false
    Bases=[]
    Levels=[
      {Source="Raw Mutton";Lvl=1}
      {Source="Enchanted Mutton"; Lvl=3}
      {Source="Enchanted Cooked Mutton";Lvl=5}
    ]
  }
  BrewHelpers.PotPh("Stamina", false,"Foul Flesh")
  { Name="Blindness"; IsVanilla= false; IsDebuff= true
    Bases=[]
    Levels=[
      {Source="Ink Sack";Lvl=1}
      {Source="Enchanted Ink Sack"; Lvl=3}
    ]
  }
  BrewHelpers.PotPh("True Resistance", false,"True Essence")
]
type ModType =
    | Duration
    | Level
    | Splash
    | DurationAndLevel
// type SplashMod = {Mod:"Splash"; Value:string}
type ModValue = 
    | Duration of minutes:int
    | Level of int
    | Splash of penaltyPercent:decimal
    | DurationAndLevel of minutes:int*level:int

type PotModifier = {Mat:string;Mod:ModValue} with
    member x.ModType = 
        match x.Mod with
        | Duration _ -> ModType.Duration
        | Level _ -> ModType.Level
        | Splash _ -> ModType.Splash
        | DurationAndLevel _ -> ModType.DurationAndLevel

let modifiers:PotModifier list = [
  {Mod=Duration 8; Mat="Redstone"} // duration in minutes
  {Mod=Duration 16; Mat="Enchanted Redstone"}
  {Mod=Duration 40; Mat="Enchanted Redstone Block"}
//   {Mod=Duration 16; Mat="Enchanted Redstone Lamp"}
  {Mod=Level 1; Mat="Glowstone Dust"}
  {Mod=Level 2; Mat="Enchanted Glowstone Dust"}
  {Mod=Level 3; Mat="Enchanted Glowstone"}
  {Mod=DurationAndLevel(16,3); Mat="Enchanted Redstone Lamp"}
  {Mod=Splash 0.5m; Mat="Gunpowder"}
  {Mod=Splash 0m; Mat="Enchanted Gunpowder"}
]

// let getBases = (selected:string) : string[] => 
//   selected != "" ? potions.find(p => p.name == selected).bases : distinct(potions.map(p => p.bases).flat());

let getBases = 
    function
    | ValueString x ->
        potions |> List.find(fun pot -> pot.Name = x) |> fun x -> x.Bases
    | _ ->
        potions
        |> Seq.collect(fun x -> x.Bases)
        |> Seq.distinct
        |> Seq.sort
        |> List.ofSeq
