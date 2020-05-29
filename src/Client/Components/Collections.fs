module Components.Collections.Component

open AppDomain.Collections.Collection
open BrowserStorage
open Fable.React
open Elmish
open CodeHelpers.FableHelpers
type ComponentState = {
    WeaponsState: Weapons.Model
}
type Model = {
    Subtab: CollectionType
    ComponentStates: ComponentState
}
type Props = {
    Theme:string
}

type CMessages=
    | WMsg of Weapons.Msg
type Msg =
    | TabChange of CollectionType
    | CMsg of CMessages

let init overrideOpt =
    overrideOpt 
    |> Option.defaultValue {
        Subtab= Weapons
        ComponentStates={
            WeaponsState= Weapons.init None
        }
    }
    |> fun x -> x, Cmd.none

let cUpdate msg (state:ComponentState):ComponentState * Cmd<CMessages> =
    match msg with
    | WMsg x ->
        Weapons.update x state.WeaponsState
        ||> mapUpdate (fun s -> {state with WeaponsState= s}) WMsg

let update msg model :Model * Cmd<Msg> =
    match msg with
    | Msg.TabChange x ->
        {model with Subtab = x}, Cmd.none
    | CMsg x ->
        cUpdate x model.ComponentStates
        ||> mapUpdate (fun cs -> {model with ComponentStates= cs}) CMsg

module private Internal =

    let getTab model dispatch =
        match model.Subtab with
        | Weapons ->
            Weapons.view () model.ComponentStates.WeaponsState (WMsg >> dispatch)
        | _ ->
            div[][
                unbox "not implemented"
            ]

open Internal
open Components.SharedComponents


// module Component =
    // let view(iStorage:IHierarchyFactory):FunctionComponent<IHierarchyFactory> = 
    //     printfn "Making collection component"
    //     let collectionStorage = iStorage.Create "CollectionComponent"
    //     eprintfn "Made storage"
    //     let oldState : Model = collectionStorage.Value |> Option.defaultValue {Subtab=Weapons}
    //     let f props =
    //         let state,update =
    //             eprintfn "Made old state"
    //             let state = Hooks.useState props
    //             eprintfn "got hooked state"
    //             state.current,
    //                 fun (x:Model) ->
    //                     collectionStorage.Value <- Some x
    //                     state.update(x)
    //         let dispatch =
    //             function
    //             | Msg.TabChange x ->
    //                 update {state with Subtab = x}
    //         eprintfn "Making tab"
    //         let tab = getTab state.Subtab
    //         eprintfn "Made tab"
    //         let result =
    //             div [] [
    //                 unbox "Collections"
    //                 TabContainer None (
    //                     Some {|
    //                             names= CollectionType.All
    //                             map= string
    //                             active= Some state.Subtab
    //                             onClick= Msg.TabChange >> dispatch
    //                 |}) []
    //             ]
    //         eprintfn "Made result"
    //         result
    //     let fof = FunctionComponent.Of(f,"Collections")
    //     fof oldState
    let view props state dispatch =
        div [] [
            unbox "Collections"
            TabContainer None (
                Some {|
                        names= CollectionType.All
                        map= string
                        active= Some state.Subtab
                        onClick= Msg.TabChange >> dispatch
            |}) []
            (
                getTab state (Msg.CMsg >> dispatch)
            )
        ]