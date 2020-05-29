module Components.ProfileMgmt

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.Import
open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fulma

open CodeHelpers.FableHelpers
open SkyblockHelper
open Shared
open Shared.Helpers
open Components.SharedComponents


type Msg =
    | AccountChange of string
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
type Model = {
    Account: string
    SavedNames: string list
    CurrentProfile : Profile
    ProfileName:string
}
let initialModel = { Account = empty; SavedNames = List.empty; CurrentProfile = Profile.empty; ProfileName = empty }
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
    | AccountChange x ->
        {m with Account = x}, Cmd.none
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
                toGlobal "profileMap" map
                m, Cmd.none

    | CreateProfile ->
        printfn "CreateProfile old names:%A" m.SavedNames
        let pn = if m.SavedNames |> Seq.contains m.ProfileName then "" else m.ProfileName
        { initialModel with ProfileName = pn; SavedNames = m.SavedNames }, Cmd.none
    | NewProfileNameChange n ->
        if isNull n then
            eprintfn "Profile name change passed null"
            m,Cmd.none
        else
            {m with ProfileName = n}, Cmd.none
    | ProfileNamesLoad (Ok names) ->
        printfn "Loaded, setting names: %A" names
        {m with SavedNames = names}, Cmd.none
    | PopulateMinions ->
        let cp ={m.CurrentProfile with Minions = Array.ofSeq defaultMinions}
        let next = {m with CurrentProfile = cp}
        next, Cmd.none
    | SaveProfile ->
        printfn "Saving profile"
        if m.CurrentProfile.Minions.Length > 0 then
            match m.Account,m.ProfileName with
            | ValueString _, ValueString _ ->
                sprintf "%s/%s" m.Account m.ProfileName
                |> Some
            | ValueString _,_ ->
                m.Account
                |> Some
            | _, ValueString _ ->
                // we can't let account and profiles both have the same lookup
                sprintf "/%s" m.ProfileName
                |> Some
            | _ ->
                None
            |> Option.defaultValue "default"
            |> fun storageName ->
                toGlobal "profileStore" profileStore
                profileStore.Save (storageName, m.CurrentProfile)
                |> function
                    | Ok () -> printfn "Saved"
                    | Error exn -> printfn "Exception %A" exn
                |> ignore // todo? no error message or success indication
                // no storage name found, no account or profile, or either
            if m.SavedNames |> Seq.contains m.ProfileName |> not then
                let saved = m.ProfileName::m.SavedNames
                printfn "savedNames will be %A" saved
                profileNameStore.Save <| Some saved
                |> function
                    | Ok () -> printfn "Saved"
                    | Error exn -> printfn "Exception %A" exn
                {m with SavedNames = saved }, Cmd.none
            else m,Cmd.none
        else
            printfn "ProfileName was %s and minions were %i" m.ProfileName m.CurrentProfile.Minions.Length
            m, Cmd.none // todo? no error message or anything

let profileDropdown labelText (selectedItem:string) items onChange buttonState =
    printfn "rerendering drop down"
    div [] [
        BFulma.dropdown labelText selectedItem items (fun e -> printfn "selected firing"; onChange e)
        BFulma.button "New Profile" false buttonState 
    ]

let profileList names txt onTextChange = 
    printfn "Using profiles: %A" names
    Text.div [] [
        BFulma.horizontalInput "Profile Name" 
            <| Input.text [
                Input.DefaultValue txt
                Input.OnChange onTextChange
            ]
    ]


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
let profileLink account profileOpt children =

    let link x = sprintf "https://sky.lea.moe/stats/%s" x
    if String.isValueString profileOpt then
        sprintf "%s/%s" account profileOpt
    else
        account
    |> fun x ->
        a [link x |> Href; Target "_blank"] children
let view (model : Model) (dispatch : Msg -> unit) =
    div [] [

          Container.container [] [

              yield profileDropdown "Load Saved..." model.ProfileName model.SavedNames
                  (Msg.ProfileSelected >> dispatch)
                  (BFulma.BtnEnabled (fun _ -> Msg.CreateProfile |> dispatch))
              yield BFulma.horizontalInput "Account" <|
                    Input.text [Input.DefaultValue model.Account; Input.OnChange (getEvValue>> Msg.AccountChange >> dispatch)]
              yield profileList model.SavedNames model.ProfileName
                (getEvValue >> Msg.NewProfileNameChange >> dispatch)
              if String.isValueString model.Account then
                  yield profileLink model.Account model.ProfileName [
                    str "Profile"
                  ]
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
    ]