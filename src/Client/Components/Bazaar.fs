module Components.Bazaar

open Components.SharedComponents
open Fable.React
open Fable.React.Props
open CodeHelpers.FableHelpers
open AppDomain.SalesReference
open Shared
open Shared.Helpers
open Elmish

type BazaarMode = Buy | Sell
module Internal =
    type RateDisplayProps = {
        Mode:BazaarMode
        Values: {| name:string;value:decimal option;div:int option |} list
    }

    let RateDisplay (props) =
        if props.Values |> Seq.exists(fun _ -> true) then
            div [Class "bd-outline"][
                table [Class "table"][
                    thead [][
                        tr[][
                            th [][unbox "Name"]
                            th [][unbox <| string props.Mode]
                        ]
                    ]
                    tbody [](
                        props.Values
                        |> Seq.map(fun x ->
                            tr[Key x.name][
                                td[][unbox x.name]
                                td[][
                                    match x.value, x.div with
                                    | Some v, Some d when d > 0 ->
                                        yield unbox <| formatNumber(float v / float d, Some 2)
                                    | _ -> ()

                                ]
                            ]
                        )

                    )
                ]
            ]
        else div [][]


    let BazaarTable (props:{| preHeaders:string list; addedHeaders:string list|}, children) =
        let h = props.preHeaders @ ["Label";"Value";"Divisor";"Vendor"] @ props.addedHeaders 
        Table {|headers=h; children=children|}

    module Preconfigured =
        type Model = {
            Selected:string
            Category:Category option
            Values:Map<string,decimal>
        } with
            static member Create model : Model = model

        type Props = {
            Mode:BazaarMode
        }

        type Msg =
            | CategoryChange of Category option
            | ItemChange of string
            | ValueChange of string * string

        let init overrideOpt =
            match overrideOpt with
            | None ->
                {
                    Category= None
                    Selected= "Ice"
                    Values= Map.empty
                }
            | Some x -> x

        let update msg (model:Model) =
            printfn "Preconfigured update"
            match msg with
            | Msg.CategoryChange c ->
                let next = {model with Category = c}
                printfn "Category changed? %A" next
                next,Cmd.none
            | Msg.ItemChange x ->
                {model with Selected = x}, Cmd.none
            | ValueChange (n,v) ->
                let next =
                        {model with Values =
                                    match tryParseDec v with
                                    | Some v ->
                                        model.Values |> Map.add n v
                                    | None ->
                                        model.Values |> Map.remove n
                        }
                next, Cmd.none

        let view props model dispatch =
            let forms = preconfigurations |> List.find(fun x -> x.Name = model.Selected) |> fun x -> x.Forms
            let getValueKey lbl = model.Selected + "."+ lbl
            let getKeyValue lbl = model.Values |> Map.tryFind (getValueKey lbl)
            let formTypes = preconfigurations |> Seq.map(fun x -> x.Category) |> Seq.distinct
            let items =
                model.Category
                |> function
                    |Some cat ->
                        preconfigurations
                        |> List.filter(fun x -> x.Category = cat)
                    |None ->
                        preconfigurations |> List.sortBy(fun x -> x.Name)
            let onCategoryChange =
                getTargetValue ("categorySelect")
                >> Option.iter(
                        Category.FromString
                        >> CategoryChange
                        >> dispatch
                    )
            div [][
                select [    Class "select"
                            Value (
                                match model.Category with
                                | None -> ""
                                | Some cat -> string cat
                            )
                            OnChange onCategoryChange
                ][
                    yield option [Value ""][unbox "Filter..."]
                    yield! formTypes |> Seq.map(string >> fun x -> option [Key x; Value x][unbox x])
                ]

                select [
                    Class "select"
                    Value model.Selected 
                    OnChange (getTargetValue("Preconfigured.item.selected") >> Option.defaultValue "" >> Msg.ItemChange>>dispatch)
                    ][
                        yield option [Value ""][unbox "Item"]
                        yield! items |> Seq.map(fun pre -> option [Key pre.Name;Value pre.Name][unbox pre.Name])
                ]
                div [][
                    unbox model.Selected
                    BazaarTable ({| preHeaders=List.empty;addedHeaders=List.empty |},
                        (
                            forms |> List.map(fun form ->
                                tr [Key form.Label;Class "tr"][
                                    td [
                                        Title (Option.defaultValue null form.Asterisk)
                                        Class (form.Asterisk |> Option.map(fun _ -> "star") |> Option.defaultValue null |> (+) "td ")
                                        ][
                                            unbox form.Label
                                    ]
                                    td [][
                                        NumberInput {
                                            Name= form.Label
                                            Value= getKeyValue form.Label
                                            OnChange=(fun nv -> Msg.ValueChange(nv.Name,nv.Value) |> dispatch)
                                            Placeholder= None
                                        }
                                    ]
                                    td [][
                                        match form.Div with
                                        | Some x -> yield unbox x
                                        | None -> ()
                                    ]
                                    td [][
                                        match form.Vend with
                                        | Some x -> yield unbox x
                                        | None -> ()
                                    ]
                                ]
                            )
                        )
                    )
                ]
                RateDisplay {   Mode= props.Mode
                                Values=
                                    forms
                                    |> List.map(fun form -> {| name=form.Label;value=getKeyValue(form.Label); div=form.Div |})

                }
                hr []
                Diagnostic DiagnosticMode.Shown model
            ]

let merchants =
    div [](
        referenceValues
        |> List.map(fun r ->
            div[] [
                unbox r.Name
                ul [Class "list ul bd-outline"](
                    r.Values
                    |> List.map(fun x -> 
                        li [Class "list-item"][unbox (sprintf "%s - %.1f" x.Name x.Value)]
                    )
                )
            ]
        )
    )
        
[<RequireQualifiedAccess>]
type Submenu = | Preconfigured | Custom | Merchants

open Internal
open SharedComponents.TabLink

type Props = {
    Theme:string
}
type Model = {
        Submenu:Submenu
        Mode: BazaarMode
        Preconfigured:Preconfigured.Model
        // TODO:
        // CustomState:CustomState
} with
    // might work: work around for having to open a namespace to create a model, but each namespace defines a model
    static member Create model : Model =
        model

type Msg =
    | ModeChange
    | SubmenuChange of Submenu
    | PreconfiguredMsg of Preconfigured.Msg

let init overrideOpt : Model * Cmd<Msg> =
    match overrideOpt with
    | None ->
        {
            Submenu= Submenu.Preconfigured
            Mode= Sell
            Preconfigured= Preconfigured.init None 
        }, Cmd.none
    | Some x ->
        x, Cmd.none
let update msg (model:Model) : Model * Cmd<Msg> =
    printfn "Bazaar update"

    match msg with
    | ModeChange ->
        { model with Mode = match model.Mode with |Sell -> Buy | Buy -> Sell} , Cmd.none
    | SubmenuChange sm ->
        {model with Submenu = sm}, Cmd.none
    | PreconfiguredMsg msg ->
        printfn "Bazaar.update.PreconfiguredMsg %A" msg
        let cm,cmd =
            Preconfigured.update msg model.Preconfigured
        {model with Preconfigured = cm}, cmd |> Cmd.map PreconfiguredMsg

let view (props:Props) (model : Model) (dispatch : Msg -> unit) =
    let tab =
        // TODO: Custom
        match model.Submenu with
        | Submenu.Preconfigured ->
            let cm = model.Preconfigured
            Internal.Preconfigured.view {Mode=model.Mode} cm (Msg.PreconfiguredMsg >> dispatch)
        | Submenu.Merchants -> merchants

    div [] [
        select [Value model.Mode;OnChange (fun _ -> ModeChange |> dispatch)](
            [Buy;Sell] |> List.map(string >> fun n -> option[Key n][unbox n])
        )
        unbox (string model.Mode)
        TabContainer (Option.ofValueString props.Theme) None (
            [Submenu.Preconfigured;Submenu.Merchants] |> List.map(fun sm ->
                TabTextLink (string sm) (string model.Submenu|> Some) (fun _ -> Msg.SubmenuChange sm |> dispatch)
            )
        )
        div [Class props.Theme][
            tab
        ]
        Diagnostic Shown model

    ]

