module BFulma

open Fulma
open Fable.React
open Fable.React.Props
open Fable.FontAwesome

open Shared

let horizontalInput labelTxt input =
    div [Class "field is-horizontal"][
        div [Class "field-label is-normal"] [label [Class "label"][str labelTxt]]
        div [Class "field-body"][
            div [Class "field"][
                p [Class "control"][
                    input
                ]
            ]
        ]
    ]

type ButtonState =
    | BtnEnabled of (Browser.Types.MouseEvent -> unit)
    | BtnDisabled
let button txt isPrimary buttonState =
    Button.button
        [ yield Button.IsFullWidth
          if isPrimary then yield Button.Color IsPrimary
          match buttonState with
          | BtnEnabled onClick -> yield Button.OnClick onClick 
          | BtnDisabled -> ()
        ]
        [ str txt ]

// https://github.com/Fulma/Fulma/blob/master/docs/src/Fulma/Components/Dropdown.fs
let dropdown labelText (selectedItem:string) items onClick =
    let count = Seq.length items
    printfn "Item count for dropdown is %i" count
    let itemElements =
        items
        |> Seq.map(fun n ->
            Dropdown.Item.div [
                if n=selectedItem then yield Dropdown.Item.IsActive true
                else yield Dropdown.Item.Option.Props [OnClick (fun _ -> printfn "she clicked me!";onClick n)]][str n])
        |> List.ofSeq
    Dropdown.dropdown [
            if count > 0 then
                yield Dropdown.IsHoverable
                // yield Dropdown.Props [ OnChange (fun e -> printfn "onchange dropdown"; onChange e) 
        ]
        [
            Dropdown.trigger []
                [ Button.button [ ]
                    [ span [ ]
                        [ str labelText ]
                      Icon.icon [ Icon.Size IsSmall ]
                        [ Fa.i [ Fa.Solid.AngleDown ]
                            [ ] ] ] ]
            Dropdown.menu [] itemElements
        ]
let addClasses items =
    items
    |> List.choose Option.ofValueString
    |> List.map String.trim
    |> String.concat " "