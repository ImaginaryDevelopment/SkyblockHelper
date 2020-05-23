module Client
// plans:
// profile fetcher for damage calc

open Elmish
open Elmish.React
open Fable.Core.JsInterop
open Fable.Import
open Fable.FontAwesome
open Fable.React
open Fable.React.Props
open Fulma

open Shared
open Shared.Helpers
open CodeHelpers.FableHelpers


open SkyblockHelper

type Model = {
    hello:System.String
}


// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg = 
    | Msg

let init () = 
    {hello="world"}, Cmd.none

let update dispatch model =
    model,Cmd.none

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

let getEvValue:Browser.Types.Event -> string =
    fun e ->
        Browser.Dom.console.log(e.currentTarget)
        let result = e.Value
        printfn "EvTarget is %A" result
        result

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [
            Navbar.navbar [ Navbar.Color IsPrimary ]
                [ Navbar.Item.div [ ]
                    [ Heading.h2 [ ]
                        [ str "SAFE Template" ] ] ]

            Container.container [] []
            Container.container [] [
              div [] [
              ]
                ]
        ]

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
