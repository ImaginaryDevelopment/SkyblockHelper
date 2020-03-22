module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.Import
open Fable.React.Props
open Fable.FontAwesome
open Fable.Core.JsInterop
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open Shared

open SkyblockHelper

type Model = {
    ProfileNames: string list
    CurrentProfile: Profile
    ProfileName: string
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | NewProfileNameChange of string
    | LoadProfile
    | ProfileLoad of Result<string list,exn>
    | CreateProfile
    | ProfileCreate
    | ProfileSelected of string
    | PopulateMinions


// let initialCounter () = Fetch.fetchAs<Counter> "/api/init"
let fetchProfiles ():Async<string list> = async{
    let profiles =
        BrowserStorage.tryGet<string list> "ProfileList"
        |> Option.defaultValue List.empty
    return profiles
    }


// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { ProfileNames = List.empty; CurrentProfile = Profile.empty; ProfileName = System.String.Empty }
    let loadProfileListCmd = Cmd.OfAsync.either fetchProfiles () (Ok>> ProfileLoad) (Error>>ProfileLoad)
    // let loadCountCmd =
    //     Cmd.OfPromise.perform initialCounter () InitialCountLoaded
    initialModel, loadProfileListCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (m : Model) : Model * Cmd<Msg> =
    match msg with
    | ProfileLoad (Ok names) ->
        {m with ProfileNames = names}, Cmd.none

    // | _ -> m, Cmd.none


let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]

           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]
let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]
let dropdownSample =
    Dropdown.dropdown [ Dropdown.IsHoverable ]
        [ Text.div [CustomClass "dropdown-trigger"]
            [ Button.button [ ]
                [ span [ ]
                    [ str "Dropdown" ]
                  Icon.icon [ Icon.Size IsSmall ]
                    [ Fa.i [ Fa.Solid.AngleDown ]
                        [ ] ] ] ]
          Dropdown.menu [ ]
            [ Dropdown.content [ ]
                [ Dropdown.Item.a [ ]
                    [ str "Item n°1" ]
                  Dropdown.Item.a [ ]
                    [ str "Item n°2" ]
                  Dropdown.Item.a [ Dropdown.Item.IsActive true ]
                    [ str "Item n°3" ]
                  Dropdown.Item.a [ ]
                    [ str "Item n°4" ]
                  Dropdown.divider [ ]
                  Dropdown.Item.a [ ]
                    [ str "Item n°5" ] ] ] ]

let dropdown labelText (selectedItem:string) items onChange onNewClick =
        Dropdown.dropdown [
                if Seq.length items > 0 then 
                    yield Dropdown.IsHoverable
                    yield Dropdown.Props [ OnChange onChange ]
            ]
            [
                Text.div [CustomClass "dropdown-trigger"]
                    [ Button.button [ ]
                        [ span [ ]
                            [ str labelText ]
                          Icon.icon [ Icon.Size IsSmall ]
                            [ Fa.i [ Fa.Solid.AngleDown ]
                                [ ] ] ] ]
                Dropdown.menu [] (List.map(fun n -> Dropdown.Item.a [if n=selectedItem then yield Dropdown.Item.IsActive true][str n]) items)
                button "Create Profile" onNewClick
            ]

let profileList names txt onTextChange onSelectChange onNewClick = 
    Text.div [] [
        Input.text [
            Input.DefaultValue txt
            Input.OnChange onTextChange
        ]
        dropdown "Profiles" txt names onSelectChange onNewClick
    ]
let minion =
    function
    |SkyblockHelper.Minions.Minion (t,lvl) ->
        div [][string t |> str; string lvl |> str]
let minionList minions =
    minions |> List.map minion

let getEvValue:Browser.Types.Event -> string =
    fun e -> e.target?Value
let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE Template" ] ] ]

          Container.container [] [
            //   dropdownSample
              profileList model.ProfileNames model.ProfileName
                (getEvValue >> Msg.NewProfileNameChange >> dispatch)
                (getEvValue >> Msg.ProfileSelected >> dispatch)
                (fun _ -> Msg.CreateProfile |> dispatch)
          ]
          Container.container [] [
              
              div [] [str "hello"]
              Level.level [] [
                  yield Level.left [][Level.heading [][str "Minions"]]
                  if model.CurrentProfile.Minions.Length > 0 then
                      yield Level.item [] (minionList model.CurrentProfile.Minions)
                  elif model.ProfileNames.Length < 1 then
                      yield Level.item [] [button "Initialize" (fun _ -> dispatch Msg.PopulateMinions)]

              ]
          ]

            //   [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            //         [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
            //     Columns.columns []
            //         [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
            //           Column.column [] [ button "+" (fun _ -> dispatch Increment) ] ] ]

          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
