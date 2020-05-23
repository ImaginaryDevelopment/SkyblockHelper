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

// type AppComponents = {
    
// }
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
                  unbox "testing"
                  Components.Bazaar.RateDisplay {Mode=Components.Bazaar.Sell;Values = Array.empty}
                  Components.ProfileMgmt.profileLink "Sammy314" "Grapes" [
                    str "Profile"
                  ]
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
