//translate SharedComponents.tsx
module Components.SharedComponents

open Fable.React
open Fable.React.Props
open Fulma

open Shared

type Children = ReactElement seq
// https://bulma.io/documentation/modifiers/syntax/
// type SizeClass = 'is-large' | 'is-small';
type SizeClass =
    |Small
    |Large
let getSizeText =
    function
    |Small -> "is-small"
    |Large -> "is-large"

type NumberInputProps = {
    Name:string
    OnChange: NameValue -> unit
    Placeholder: string option
    Value: float option
}
open CodeHelpers.FableHelpers
open Shared.Helpers

let Select<'t> 
    (props:{|
            addedClasses: string list
            items: 't list
            map: 't -> string
            parse: string -> 't option
            onChange: 't -> unit
            active: 't |})
    =
        select [
            yield Value (props.active |> props.map)
            yield Class ("select" :: props.addedClasses |> String.concat " ")
            yield OnChange (getTargetValue "SelectOpt.OnChange" >> Option.bind props.parse >> Option.iter props.onChange)
            ](props.items |> Seq.map(fun item -> option [props.map item |> box |> Value ][unbox <| props.map item]))

let SelectOpt<'t> 
    (props:{|
            addedClasses: string list
            emptyLabel: string
            items: 't list
            map: 't -> string
            parse: string -> 't option
            onChange: 't option -> unit
            active: 't option |})
    =
        select [
            yield Value (props.active |> Option.map props.map |> Option.defaultValue "")
            yield Class ("select" :: props.addedClasses |> String.concat " ")
            yield OnChange (getTargetValue "SelectOpt.OnChange" >> Option.bind props.parse >> props.onChange)
            ][
                yield option [Value ""][unbox props.emptyLabel]
                yield! (props.items |> Seq.map(fun item -> option [props.map item |> box |> Value ][unbox <| props.map item]))
            ]

let NumberInput (props:NumberInputProps) =
    let parser = 
        Option.bind tryParseDec
    input [ 
        yield Class "input"
        yield OnChange (fun x -> {Name= props.Name;Value= getTargetValue (sprintf "NumberInput.%s" props.Name) x |> parser } |> props.OnChange)
        match props.Placeholder with
        | Some x ->
            yield Placeholder x
        | None -> ()
        match props.Value with
        | Some x ->
            yield DefaultValue x
        | None -> ()
        ]

// type InputColumnProps

// type InputType = React.DetailedHTMLProps<
// let inputb = <select />
// let inputa = <input />
// export let HField = (props: { title: string; label: string; input: React.DetailedReactHTMLElement<{ className: string; }, HTMLElement>; }) =>(
let HField<'t> (props:{|title:string;label:string;input:string -> ReactElement|}): ReactElement =
    div [Title props.title][
        div [Class "field is-horizontal"][
            div [Class "field-label is-normal"][
                label [Class "label"][unbox props.label]
            ]
            div [Class "field-body"][
                div [Class "field"][
                    p [Class "control"][
                        (props.input " input ")
                    ]
                ]
            ]
        ]
    ]

module TabLink =
    // export type TabLinkProps<T extends string> = {
    //   name:T
    //   onClick:Types.Action1<T>
    //   active: T | undefined
    //   children: React.ReactNode
    //   title?:string
    // }
    type TabLinkProps = {
        Name:string
        OnClick: string -> unit
        Active: string option
        Children: Children // Fable.React.ReactNode
        Title:string option
    }

    let TabLink props =
        li [
            Key props.Name
            Title (props.Title |> Option.defaultValue props.Name)
            Class (match props.Active with | Some active when active = props.Name -> "active" | _ -> "")
           ][
               a[
                   OnClick (fun _ -> props.OnClick props.Name)
                   Data("name",props.Name)
                ] props.Children
            ]

    let TabTextLink name active onClick = 
        TabLink {Name=name;Active=active;OnClick=onClick;Title=None;Children=[unbox name]}

open TabLink

let TabContainer<'t> addedClasses 
    (stdTabs:{|
                names:'t list
                map: 't -> string
                onClick:'t -> unit
                active:'t option |} option)
    (children: ReactElement seq) =
        div [Class ("tabs is-centered is-boxed" + (addedClasses |> Option.defaultValue ""))][
            ul [] [
                match stdTabs with
                | None -> ()
                | Some tabs ->
                    yield!
                        tabs.names
                        |> Seq.map(fun n ->
                            TabTextLink (tabs.map n) (tabs.active |> Option.map tabs.map) (fun _ -> tabs.onClick n)
                        )

                yield! children
            ]
        ]

type DiagnosticMode =
    |Shown
    |Hidden

let Diagnostic mode (value:obj) =
    try
        pre[][
            match mode with
            | Shown ->
                yield unbox (Resolver.serialize value)
            | _ -> ()
        ]
    with ex ->
        pre[][
            yield unbox ex.Message
        ]

let Table (props:{| headers:string list;children:ReactElement seq |}) =
    table [Class "table"][
        thead [][
            tr[][
                yield! props.headers |> Seq.map(fun h -> 
                    th [Key h;Class "th"][unbox h]
                )
            ]
        ]
        tbody [] props.children
    ]

type FoldableState = {
  IsFolded:bool
}

type FoldableProps = {
  DefaultFold:bool
  Title:string
  Children:ReactElement
}

// export let FoldTarget = (props:{isFolded:boolean, children:React.ReactNode}) => (<React.Fragment>
//   {props.isFolded? null : props.children}
// </React.Fragment>);
let FoldTarget isFolded element : ReactElement =
    fragment [] [
        if isFolded then yield element
    ]

// export let FoldMaster = (props:{title:string,isFolded:boolean,onToggle:Types.Action1<boolean>}) =>
// {
//   let toggle = () => props.onToggle(props.isFolded == false);
//   return (<div onClick={toggle}>{<FontAwesomeIcon icon={props.isFolded === true ? faPlus : faMinus} />} {props.title}</div>);
// }
open Fable.FontAwesome

module Fa =
    let FaIcon opts fa =
        Icon.icon opts [Fa.i [fa][]]

let FoldMaster (props:{| title:string;isFolded:bool;onToggle:bool -> unit |}) =
    let icon = if props.isFolded then Fa.Solid.Minus else Fa.Solid.Plus 
    let toggle = fun _ -> props.onToggle(not props.isFolded)
    div [OnClick toggle][
        Fa.FaIcon [] icon
        unbox props.title
    ]

let getEvValue:Browser.Types.Event -> string =
    fun e ->
        Browser.Dom.console.log(e.currentTarget)
        let result = e.Value
        printfn "EvTarget is %A" result
        result
