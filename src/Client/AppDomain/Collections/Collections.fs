module AppDomain.Collections.Collection

open SkyblockHelper

type CraftType =
    | Slayer of Slayer*int
    | Collection of string

type CollectionType =
    | Weapons
    | Armor
    | Minions
    | Resources
    | Pets
    with
        static member All =
            [
                Weapons
                Armor
                Minions
                Resources
                Pets
            ]
