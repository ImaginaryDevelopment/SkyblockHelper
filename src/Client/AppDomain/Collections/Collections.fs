module AppDomain.Collections.Collection

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
