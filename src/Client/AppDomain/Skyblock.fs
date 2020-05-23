namespace SkyblockHelper
module Resources =
    // assuming all that can compact have part/epart and block/eblock possibilities
    type CompactableState =
        | Part
        | Compacted
    type IronState =
        | Ore
        | Ingot
        | Block
        | Enchanted
        | EnchantedBlock

    type Resource =
        | Wheat
        | Seeds
        | RawBeef
        | Watermelon
        | Pumpkin
        | Cobblestone
        | Coal
        | Iron
        | Gold
        | Lapis
        | Emerald
        | Diamond
        | Obsidian
        | Glowstone
        | Gravel
        | Custom of string
        with
            member x.GetLabel () =
                match x with
                | Custom s -> s
                | _ -> string x
            static member IsMinionType x =
                match x with 
                | Custom _ -> true
                | Seeds -> false
                | RawBeef -> true
                | _ -> true

            member x.GetMinion () =
                match x with
                | Custom s -> Some s
                | Seeds -> None
                | RawBeef -> Some "Cow"
                | x -> x.GetLabel() |> Some
    let orderByCraftList customIndex =
        List.sortBy(
            function
            | Custom _ -> customIndex
            | Cobblestone -> 0
            | Obsidian -> 1
            | Glowstone -> 2
            | Gravel -> 3
        )

open Resources
// open Fable.Core
type Minion = {Resource: Resource; Level:int}
// module Minions =
//     [<Erase>] // maybe this would work?
//     type Minion =
//         | Minion of Resource * level:int

// open Minions
type Profile = {Minions:Minion [];CombatLevel:int;ForageLevel:int;AlchemyLevel:int} with
    static member empty = {Minions=Array.empty;CombatLevel=0;ForageLevel=0;AlchemyLevel=0}
    static member UpdateMinion r v model =
        {model with Minions = model.Minions |> Array.map(function | {Resource=r'} as x when r' = r -> {x with Level=v} | x -> x )}

