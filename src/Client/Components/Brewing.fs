module Components.Brewing

open AppDomain.BrewingReference
open CodeHelpers.FableHelpers
open Components.SharedComponents
open Fable.React
open Fable.React.Props
open Fable.Core.JS
open Shared
open Shared.Helpers

module Internal =
    type TargetPotion = {
        Name: string option
        TBase: string option
        TLvl: int option
        DMod: float option
        LMod: int option
        Splash: string option
    } with
        static member Empty = {Name=None;TBase=None;TLvl=None;DMod=None;LMod=None;Splash=None}
    let allBases = 
        ul[] (
            getBases null
            |> List.map(fun b ->
                li [Key b][unbox b])
        )
    let BrewBases tpot onChange =
        select [Value (tpot |> Option.map (fun t -> t.TBase)); OnChange (getTargetValue("BrewBases.select") >> onChange)][
            yield option[Value ""][unbox "Bases..."]
            yield!
                getBases (tpot |> Option.bind(fun tpot -> tpot.Name)|> Option.defaultValue null)
                |> List.map(fun b ->
                    option [Key b;Value b][unbox b]
                )
        ]

    type Subtab =
        |Brew
        |Reference
        |Modifications with
            static member FromString (x:string) =
                match x with
                | EqualsI "brew" -> Some Brew
                | EqualsI "reference" -> Some Reference
                | EqualsI "modifications" -> Some Modifications
                | _ -> None
            static member All =
                [
                    Brew
                    Reference
                    Modifications
                ]


    let showSource tpotname =
      let pot = potions |> List.find(fun x -> x.Name = tpotname)
      span [](
          pot.Levels
          |> List.map(fun lvl ->
            sprintf "%A-%s" lvl.Lvl lvl.Source
          )
          |> List.reduce(fun x y ->
            x + "," + y
          )
          |> unbox
      )

    let reference =
      div [][
          pre [][
            unbox <| Resolver.serialize potions
          ]
      ]

    type BrewProps = {
      TPot:TargetPotion
      ShowDebuffs:bool
    }

    let sorted : Potion list =
        potions
        |> List.sortBy(fun x -> x.Name)

    let TargetSelector tpot showDebuffs f = // (props:{tpot:TargetPotion,showDebuffs:boolean, onTPotStateChange:BrewProps["onTPotStateChange"]}) => (
          try
            console.log("targetselector.props.tpot", tpot, sorted)
            select [Class "select";Value (tpot.Name |> Option.defaultValue "")
                    OnChange (getTargetValue("Brew.TargetSelector") >> f)
            ][
                yield option [Value ""][unbox "Potions..."]
                yield! (
                    if showDebuffs then sorted else sorted |> List.filter(fun x -> x.IsDebuff |> not)
                    |> List.map (fun pot ->
                        option [Key pot.Name;Value pot.Name][unbox pot.Name]
                    )
                )
            ]
          with e ->
            div [][
                unbox <| Resolver.serialize e
            ]

    [<RequireQualifiedAccess>]
    type BrewChange =
        | Name of string option
        | Base of string option
        | Lvl of string option
        | DMod of string option
        | LMod of string option
        | SplashMod of string option

    // if we are changing to or from "Enchanted Redstone Lamp" then the values must agree
    let brew tpot showDebuffs dispatch =
        let ss =
                tpot.Name
                |> Option.map showSource
        let bselect name (unselectedTitle:string) fProp msg options =
            select [Value (tpot |> fProp |> Option.defaultValue "")
                    OnChange (getTargetValue name >> msg >> dispatch)
            ][
                yield option [Value ""][unbox unselectedTitle]
                yield! options
            ]
        div [][
            h1 [Class "is-large"][unbox "Target"]
            TargetSelector tpot showDebuffs (BrewChange.Name >> dispatch)
            pre [][
                match ss with
                | None -> ()
                | Some ss ->
                    yield ss
            ]
            h2 [Class "h2"][
                unbox "Base - "
                a [Class "a"; Href "https://hypixel-skyblock.fandom.com/wiki/Brews"][unbox "wiki"]
                unbox " - "
                a [Href "http://www.minecraft101.net/t/potion-brewer.html"][unbox "automation"]
            ]
            (
                match tpot.Name with
                | Some (ValueString _) ->
                    BrewBases (Some tpot) (BrewChange.Base >> dispatch)
                | _ ->
                    allBases
            )
            bselect "Brew.lmod" "LevelMods..." (fun x -> x.LMod |> Option.map string) BrewChange.LMod (
                        modifiers
                        |> List.choose(
                            function | {Mat=m;Mod = Level _}
                                     | {Mat=m; Mod=DurationAndLevel(_)} -> Some m
                                     | _ -> None
                        )
                        |> List.map(fun m -> option [Key m; Value m][unbox m])

            )
            bselect "Brew.dmod" "Duration Mods..." (fun x -> x.DMod |> Option.map (sprintf "%.1f")) BrewChange.DMod (
                modifiers
                |> List.choose(function
                    | {Mat=m;Mod=Duration _}
                    | {Mat=m;Mod=DurationAndLevel(_)} -> Some m 
                    | _ -> None)
                |> List.map(fun m -> option [Key m;Value m][unbox m]
                )
            )
            bselect "Brew.splash" "Splash Mods..." (fun x -> x.Splash) BrewChange.SplashMod (
                modifiers
                |> List.choose(
                    function
                    | {Mat=m; Mod=Splash _} -> Some m
                    | _ -> None
                )
                |> List.map(fun m -> option [Key m;Value m][unbox m])
            )
        ]

    let ModDisplay modifier = //(props: { mod: PotModifier["mod"]; }) => {
      let mods = modifiers |> List.filter(fun x -> ModValue.GetType x.Mod = modifier)
      console.log("ModDisplay", modifier, mods)
      div [][
          h3[Class"is-size-3"][unbox <| ModType.Humanize modifier]
          Table {| headers= ["Material";"Value"]
                   children=
                   (
                      mods
                      |> List.map(fun m ->
                        tr [][
                            td[][unbox m.Mat]
                            td[][
                                match m.Mod with
                                | DurationAndLevel(i,l) -> sprintf "%i minutes - %i levels" i l
                                | Duration i -> sprintf "%i minutes" i
                                | Level i -> sprintf "%i levels" i
                                | Splash penalty -> sprintf "-%0.0f%% minutes" penalty
                                |> unbox
                            ]
                        ]
                      )
                    )
          |}
      ]

    let modifications =
        let modTypes = ModType.All
        div [Class "bd-outline"](
            modTypes
            |> List.map ModDisplay
        )
    let specialMod = "Enchanted Redstone Lamp"

open Internal
open Elmish

type Model = {
  Subtab: Subtab
  // target potion
  TPot: TargetPotion
  ShowDebuffs: bool
}

type Props = {
  Theme:string
}

type Msg =
    | TabChange of Subtab
    | BrewChangeMsg of BrewChange

let init overrideOpt : Model * Cmd<Msg> = (
    overrideOpt
    |> Option.defaultValue {
      ShowDebuffs= false
      TPot= TargetPotion.Empty
      Subtab= Brew
}, Cmd.none)

let update msg (model:Model) =
    let lensTPot fSet =
        {model with TPot = fSet model.TPot}
    match msg with
    | TabChange st ->
        {model with Subtab = st}, Cmd.none
    | BrewChangeMsg msg ->
        match msg with
        | BrewChange.Name x ->
            lensTPot (fun prev -> {prev with Name= x}), Cmd.none
        | BrewChange.Base x ->
            lensTPot (fun prev -> {prev with TBase= x}), Cmd.none
        | BrewChange.Lvl x ->
            lensTPot (fun prev -> {prev with TLvl= x |> Option.bind tryParseInt}), Cmd.none
        | BrewChange.DMod x ->
            lensTPot (fun prev -> {prev with DMod= x |> Option.bind tryParseDec}), Cmd.none
        | BrewChange.LMod x ->
            lensTPot (fun prev -> {prev with LMod= x |> Option.bind tryParseInt}), Cmd.none
        | BrewChange.SplashMod x ->
            lensTPot (fun prev -> {prev with Splash= x}), Cmd.none

let view (props:Props) state (dispatch:Msg -> unit) =
    let stdTabs = {|    names= Subtab.All
                        active= Some state.Subtab
                        map= string
                        onClick= TabChange >> dispatch // props.onStateChange(copyUpdate(props.state,"subtab",x))
    |}

    let tab =
        match state.Subtab with
        | Brew ->
            brew state.TPot state.ShowDebuffs (BrewChangeMsg >> dispatch)
        | Reference -> reference
        | Modifications -> modifications

    div [Class props.Theme][
        TabContainer (Option.ofValueString props.Theme) (Some stdTabs) Seq.empty
        div [Class props.Theme][
            tab
        ]
        Diagnostic Shown state
    ]