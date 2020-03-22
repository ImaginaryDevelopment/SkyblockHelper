module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.Import
open Fable.React.Props
open Fable.FontAwesome
open Fable.Core.JsInterop
open Fulma

open Shared

let stringify x = Fable.Core.JS.JSON.stringify x
let parse x = Fable.Core.JS.JSON.parse(x)
open SkyblockHelper

type Model = {
    Account: string
    ProfileNames: string list
    CurrentProfile: Profile
    ProfileName: string
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | NewProfileNameChange of string
    | LoadProfileNames
    | ProfileNamesLoad of Result<string list,exn>
    | CreateProfile
    | ProfileCreate
    | ProfileSelected of string
    | PopulateMinions
    | MinionChange of Resources.Resource * int
    | SaveProfile


let profileNameStore = BrowserStorage.createStorage "ProfileNames"
let profileStore = BrowserStorage.LookupStorage<Profile>("Profiles")
// let initialCounter () = Fetch.fetchAs<Counter> "/api/init"
let fetchProfiles ():Async<string list> = async{
    let profiles = profileNameStore.Get() |> Option.defaultValue List.empty
    printfn "Fetched profiles, names: %A" profiles
    return profiles
    }
let empty = System.String.Empty
let initialModel = { Account = empty; ProfileNames = List.empty; CurrentProfile = Profile.empty; ProfileName = empty }
let defaultMinions =
        SkyblockHelper.Gen.ResourceCases |> Seq.filter(snd>>Resources.Resource.IsMinionType) |> Seq.map(fun (_name,v) ->
            {Resource=v;Level=0}
        )
        |> List.ofSeq

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let profileNamesLoadCmd = Cmd.OfAsync.either fetchProfiles () (Ok>> ProfileNamesLoad) (Error>>ProfileNamesLoad)
    // let loadCountCmd =
    //     Cmd.OfPromise.perform initialCounter () InitialCountLoaded
    initialModel, profileNamesLoadCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (m : Model) : Model * Cmd<Msg> =
    match msg with
    | MinionChange(r,v) ->
        let profile = m.CurrentProfile |> Profile.UpdateMinion r v
        {m with CurrentProfile = profile} , Cmd.none
    | ProfileSelected n ->
        profileStore.TryFind n
        |> function
            | Some profile ->
                printfn "Loading profile %s:%A" n profile
                { m with ProfileName = n; CurrentProfile = profile }, Cmd.none
            | None ->
                eprintfn "No profile found for %s in %A" n map
                BrowserStorage.toGlobal "profileMap" map
                m, Cmd.none

    | CreateProfile ->
        printfn "CreateProfile old names:%A" m.ProfileNames
        let pn = if m.ProfileNames |> Seq.contains m.ProfileName then "" else m.ProfileName
        { initialModel with ProfileName = pn; ProfileNames = m.ProfileNames}, Cmd.none
    | NewProfileNameChange n ->
        if isNull n then
            eprintfn "Profile name change passed null"
            m,Cmd.none
        else
            {m with ProfileName = n}, Cmd.none
    | ProfileNamesLoad (Ok names) ->
        printfn "Loaded, setting names: %A" names
        {m with ProfileNames = names}, Cmd.none
    | PopulateMinions ->
        let cp ={m.CurrentProfile with Minions = Array.ofSeq defaultMinions}
        let next = {m with CurrentProfile = cp}
        next, Cmd.none
    | SaveProfile ->
        if m.ProfileName |> String.isValueString && m.CurrentProfile.Minions.Length > 0 then
            profileStore.Save (m.ProfileName, m.CurrentProfile)
            |> function
                | Ok () -> printfn "Saved"
                | Error exn -> printfn "Exception %A" exn
            |> ignore // todo? no error message or success indication
            if m.ProfileNames |> Seq.contains m.ProfileName |> not then
                let profileNames = m.ProfileName::m.ProfileNames
                printfn "profilenames will be %A" profileNames
                profileNameStore.Save profileNames
                |> function
                    | Ok () -> printfn "Saved"
                    | Error exn -> printfn "Exception %A" exn
                {m with ProfileNames = profileNames }, Cmd.none
            else m,Cmd.none

        else
            printfn "ProfileName was %s and minions were %i" m.ProfileName m.CurrentProfile.Minions.Length
            m, Cmd.none // todo? no error message or anything

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

let profileDropdown labelText (selectedItem:string) items onChange buttonState =
    printfn "rerendering drop down"
    div [] [
        BFulma.dropdown labelText selectedItem items (fun e -> printfn "selected firing"; onChange e)
        BFulma.button "New Profile" false buttonState 
    ]

let profileList names txt onTextChange onSelectChange onNewClick = 
    printfn "Using profiles: %A" names
    Text.div [] [
        BFulma.horizontalInput "Profile Name" 
            <| Input.text [
                Input.DefaultValue txt
                Input.OnChange onTextChange
            ]
        profileDropdown "Profiles" txt names onSelectChange onNewClick
    ]

let profileLink account profileOpt =
    let link x = sprintf "https://sky.lea.moe/stats/%s" x
    if String.isValueString profileOpt then
        sprintf "%s/%s" account profileOpt
    else
        account
    |> fun x ->
        a [link x |> Href]

let minion (x:Minion) onChange =
    try
        match x with
        |{Resource=t;Level=lvl} ->
            let minput = Input.number [
                Input.DefaultValue <| string lvl
                Input.Props [ Min "0"; Max "20"]
                Input.OnChange onChange
                ]
            let tip = t.GetLabel()
            let text = t.GetMinion() |> Option.defaultValue tip
            // BFulma.horizontalInput (string t) minput
            tr [ ][
                td [] [minput]
                td [] [div [Title tip] [ str text ]]
            ]
    with ex ->
        eprintfn "Error rendering minion %s" <| stringify x
        div [] [ stringify ex |> sprintf "Error:%s" |> str ]

let minionList minions onChange =
    printfn "Rendering a minion list"
    div [ Class "table-container"][
        table [] [
            thead [] [
                tr [] [
                    th [] [ str "Level"]
                    th [] [ str "Name"]
                ]
            ]
            tbody [] [
                yield! minions |> Array.map (fun mn -> minion mn (fun ev -> onChange ev mn.Resource))
            ]
        ]
    ]

let getEvValue:Browser.Types.Event -> string =
    fun e ->
        Browser.Dom.console.log(e.currentTarget)
        let result = e.Value
        printfn "EvTarget is %A" result
        result

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE Template" ] ] ]

          Container.container [] [

              if String.isValueString model.Account then
                  yield profileLink model.Account model.ProfileName [
                    str "Profile"
                  ]
              yield profileList model.ProfileNames model.ProfileName
                (getEvValue >> Msg.NewProfileNameChange >> dispatch)
                (Msg.ProfileSelected >> dispatch)
                (BFulma.BtnEnabled (fun _ -> Msg.CreateProfile |> dispatch))
          ]

          Container.container [] [
              div [] [
                  if model.CurrentProfile.Minions.Length > 0 then
                      yield minionList model.CurrentProfile.Minions (getEvValue>>(fun v r -> dispatch (Msg.MinionChange(r,int v))))
                  elif model.CurrentProfile.Minions.Length < 1 then
                      yield BFulma.button "Initialize" false (BFulma.BtnEnabled (fun _ -> dispatch Msg.PopulateMinions))
              ]
              BFulma.button "Save" true (BFulma.BtnEnabled (fun _ -> dispatch Msg.SaveProfile))
          ]

          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]

#if DEBUG
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
// #if DEBUG
// |> Program.withDebugger
// #endif
|> Program.run
