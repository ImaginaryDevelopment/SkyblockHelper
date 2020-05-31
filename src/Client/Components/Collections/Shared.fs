module Components.Collections.Shared

open Components.SharedComponents
open Fable.React
open Fable.React.Props

type FoldListItemProps = {
    Name:string
    Title:string
    Folded:bool
    OnToggle: bool -> unit
}
// let FoldTarget (props:{| isFolded:bool;children:ReactElement |}) =
// let FoldMaster (props:{| title:string;isFolded:bool;onToggle:bool -> unit |}) =
let foldyListItem props children =
    li [Key props.Name;  Class "list-item"][
        div[Class "columns"][
            div[Class "column"][
                FoldMaster {| title=props.Title; isFolded= props.Folded; onToggle= props.OnToggle |}
            ]
            FoldTarget props.Folded (div [Class "column is-a-fifth"] children)
        ]

    ]
