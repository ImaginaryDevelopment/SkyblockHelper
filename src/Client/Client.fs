module Client

open Elmish

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram Index.init Index.update Index.view
#if DEBUG
// |> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
// |> Program.withDebugger
#endif
|> Program.run
