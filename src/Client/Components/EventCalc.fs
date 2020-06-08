module Components.EventCalc

open CodeHelpers.FableHelpers
open Elmish
open Fable.React
open Fable.React.Props
open Shared.Helpers

type Model = {
    Days:int
    Hours: int
    Minutes: int
}

type Msg =
    | DayChange of int
    | HourChange of int
    | MinuteChange of int

let init initOverride =
    initOverride
    |> Option.defaultValue {
        Days = 1
        Hours = 3
        Minutes = 30
    }, Cmd.none

let update (msg:Msg) (model:Model) : Model * Cmd<Msg> =
    match msg with
    |DayChange x ->
        {model with Days = x}, Cmd.none
    | HourChange x ->
        {model with Hours = x}, Cmd.none
    | MinuteChange x ->
        {model with Minutes = x}, Cmd.none

let view theme (model:Model) (dispatch:Msg -> unit) =
    let dt = System.DateTime.Now
    let onChange name f = getTargetValue ("EventCalc-" + name) >> Option.bind tryParseInt >> Option.iter(f >> dispatch)
    let numberInput name dv fMsg = input [Type "number"; DefaultValue dv; Class "input"; OnChange <| onChange name fMsg]
    let bdSpan = span [Class "bd-outline"]
    let fDt (dt:System.DateTime) =
        dt.ToLocalTime().ToString()
    let pluralize =
        function
        | 1 -> ""
        | _ -> "s"
    let spanTimeDisplay name value =
        bdSpan [ unbox (sprintf "%i %s%s" value name <| pluralize value)]

    let columns items =
        Fulma.Columns.columns [](
            items
            |> List.map (Fulma.Column.column [])
        )

    div [Class theme][
        columns [
            [   unbox "Days: "
                numberInput "Days" model.Days Msg.DayChange ]
            [   unbox "Hours: "
                numberInput "Hours" model.Hours Msg.HourChange ]
            [   unbox "Minutes"
                numberInput "Minutes" model.Minutes Msg.MinuteChange ]
        ]
        columns [
            [   bdSpan [ unbox <| fDt dt]
                unbox " + "
                spanTimeDisplay "day" model.Days
                unbox " + "
                spanTimeDisplay "hour" model.Hours
                unbox " + "
                spanTimeDisplay "minute" model.Minutes
                span [] [unbox " = "]
                bdSpan (
                    let evtDt = dt.AddDays(float model.Days).AddHours(float model.Hours).AddMinutes(float model.Minutes)
                    [
                        unbox <| fDt evtDt
                    ]
                )
            ]
        ]
    ]
