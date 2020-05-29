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
    Value: decimal option
}
open CodeHelpers.FableHelpers

let NumberInput (props:NumberInputProps) =
    input [ 
        yield Class "input"
        yield OnChange (fun x -> {Name= getName x;Value= getValue x} |> props.OnChange)
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
        name:string
        onClick: string -> unit
        active: string option
        children: Children // Fable.React.ReactNode
        title:string option
    }
    // export let TabLink = <T extends string>(props:TabLinkProps<T>) => (
    //     <li key={props.name} title={props.title || props.name} className={props.active==props.name?'active':''}><a onClick={() => props.onClick(props.name)} data-name={props.name}>{props.children}</a></li>);
    let TabLink props =
        li [
            Key props.name
            Title (props.title |> Option.defaultValue props.name)
            Class (match props.active with | Some active when active = props.name -> "active" | _ -> "")
           ][
               a[
                   OnClick (fun _ -> props.onClick props.name)
                   Data("name",props.name)
                ] props.children
            ]
    // export type TabTextLinkProps<T extends string> = {
    //   name:T
    //   onClick:Types.Action1<T>
    //   active:T | undefined
    // }
    // type TabTextLinkProps = {
    //     name:string
    //     onClick: string -> unit
    //     active: string option
    // }
    // export let TabTextLink = <T extends string>(props:TabTextLinkProps<T>) => (
    //     <TabLink name={props.name} active={props.active} onClick={props.onClick}>{props.name}</TabLink>);
    // let TabTextLink (props:TabTextLinkProps) = 
    let TabTextLink name active onClick = 
        TabLink {name=name;active=active;onClick=onClick;title=None;children=[unbox name]}

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
    pre[][
        match mode with
        | Shown ->
            yield unbox (Fable.Core.JS.JSON.stringify(value,space=4))
        | _ -> ()
    ]
// type TextLIProps = {
//   key:string
// }
// export let TextLI = (props:Types.NameValue) => (
//   <li key={props.value} className='li ' data-name={props.value}>{props.value}</li>
// )

// export let Table = (props:{headers:string[], children:React.ReactNode}) => (
//   <table className="table">
//             <thead>
//               <tr>
//                 {props.headers.map(h =>
//                   <th key={h} className='th'>{h}</th>
//                 )}
//               </tr>
//             </thead>
//             <tbody>
//               {props.children}
//             </tbody>
//           </table>
// )
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

// old name: FoldableListState
type FoldableState = {
  isFolded:bool
}
// old name: FoldableListProps
type FoldableProps = {
  defaultFold:bool
  title:string
  children:ReactElement
}
let initState (props:FoldableProps) = ({
  isFolded= props.defaultFold
})

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
    let icon = if props.isFolded then Fa.Solid.Plus else Fa.Solid.Minus
    let toggle = fun _ -> props.onToggle(not props.isFolded)
    div [OnClick toggle][
        Fa.FaIcon [] icon
    ]

let getEvValue:Browser.Types.Event -> string =
    fun e ->
        Browser.Dom.console.log(e.currentTarget)
        let result = e.Value
        printfn "EvTarget is %A" result
        result
