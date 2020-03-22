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
        | Iron
        | Custom of string
        with
            member x.GetLabel () =
                match x with
                |Custom s -> s
                | _ -> string x
            member x.GetMinion () =
                match x with
                |Custom s -> Some s
                |Seeds -> None
                |RawBeef -> Some "Cow"
                | x -> x.GetLabel() |> Some

open Resources

module Minions =
    type Minion =
        |Minion of Resource * level:int

open Minions
type Profile = {Minions:Minion list;CombatLevel:int;ForageLevel:int;AlchemyLevel:int} with
    static member empty = {Minions=List.empty;CombatLevel=0;ForageLevel=0;AlchemyLevel=0}

