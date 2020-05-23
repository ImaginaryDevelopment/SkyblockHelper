//translate SharedComponents.tsx
module Components.SharedComponents

open Fable.React
open Fable.React.Props
open Fulma

// https://bulma.io/documentation/modifiers/syntax/
// type SizeClass = 'is-large' | 'is-small';
type SizeClass =
    |Small
    |Large
let getSizeText =
    function
    |Small -> "is-small"
    |Large -> "is-large"

// type NumberInputProps

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
        children: obj // Fable.React.ReactNode
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
                ][
                    unbox props.children
               ]
            ]
    // export type TabTextLinkProps<T extends string> = {
    //   name:T
    //   onClick:Types.Action1<T>
    //   active:T | undefined
    // }
    type TabTextLinkProps = {
        name:string
        onClick: string -> unit
        active: string option
    }
    // export let TabTextLink = <T extends string>(props:TabTextLinkProps<T>) => (
    //     <TabLink name={props.name} active={props.active} onClick={props.onClick}>{props.name}</TabLink>);
    let TabTextLink (props:TabTextLinkProps) = 
        TabLink {name=props.name;active=props.active;onClick=props.onClick;title=None;children=props.name}


module TabContainer =
    open TabLink

    // export type TabContainerProps<T extends string>  = {
    //   addedClasses?: string
    //   children?:React.ReactNode
    //   stdTabs?:{names:T[],onClick:Types.Action1<T>,active:T}
    // }
    type TabContainerProps = {
        addedClasses: string option
        children : obj option
        stdTabs: {| names:string[];onClick:string -> unit;active:string option |} option
    }
    // export let TabContainer = <T extends string>(props:TabContainerProps<T>) => (
    //   <div className={'tabs is-centered is-boxed ' + (props.addedClasses || '')}>
    //     <ul>
    //       {
    //         props.stdTabs == null ? '' : (props.stdTabs.names.map(n => (<TabTextLink key={n} name={n} active={props.stdTabs.active} onClick={props.stdTabs.onClick} />)))
    //       }
    //     {props.children}
    //     </ul>
    //   </div>
    // );
    let TabContainer (props:TabContainerProps) =
        div [Class ("tabs is-centered is-boxed" + (props.addedClasses |> Option.defaultValue ""))][
            ul [] [
                match props.stdTabs with
                | None -> ()
                | Some tabs ->
                    yield! 
                        tabs.names
                        |> Array.map(fun n -> 
                            TabTextLink {name =n;onClick=tabs.onClick;active=tabs.active}
                        )


                yield unbox props.children
            ]
        ]

// export interface StatefulProps<TState> {
//   state:TState
//   onStateChange: Types.Action1<TState>
// }
type DiagnosticProps ={
  show:bool
  value:obj
}
// export let Diagnostic = (props:DiagnosticProps) => (
//   <pre>
//     {props.show?JSON.stringify(props.value,undefined,4): ''}
//   </pre>
// )
let Diagnostic (props:DiagnosticProps) =
    pre[][
        if props.show then
            yield unbox (Fable.Core.JS.JSON.stringify(props.value,space=4))
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
let Table (props:{| headers:string[];children:ReactElement|}) =
    table [Class "table"][
        thead [][
            tr[][
                yield! props.headers |> Array.map(fun h -> 
                    th [Key h;Class "th"][unbox h]
                )
            ]
        ]
        tbody [][
            props.children
        ]
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
let FoldTarget (props:{| isFolded:bool;children:ReactElement |}) =
    if props.isFolded then
        props.children
    else null

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

// mostly just an example, the html structure not useful in most cases
// export let Foldable : CreateFunctionalComponent<FoldableListProps,FoldableListState> = props =>
// {
//   const component : Component<FoldableListProps,FoldableListState> = new React.Component(props);
//   component.state = initState(props);
//   let toggle = () => component.setState({isFolded:component.state.isFolded == false});
//   component.render = () => {
//     return (<div>
//       <div onClick={toggle}>{<FontAwesomeIcon icon={component.state.isFolded? faPlus : faMinus} />} {props.title}</div>
//       {component.state.isFolded? null : <div>{props.children}</div>}
//     </div>);
//   }
//   return component;
// }


// export type ComRenderer <TProps,TState> = (props:React.PropsWithChildren<TProps>,currentState:Readonly<TState>, updateState:Types.Action1<TState>) => React.ReactElement<TProps>;

// type StatedComponentCreator = <TProps,TState>(initialState:TState,f:Types.Func3<TProps,TState,Types.Action1<TState>,React.ReactElement<TProps>>, 
//   fStateObserver?:Types.Action1<TState>) => React.FC<TProps>

// does not account for the possibility that initial state depends on initial props (as in input's defaultValue for instance)
// export const createStatedComponent : StatedComponentCreator = <TProps,TState>(initialState:TState,f:ComRenderer<TProps,TState>, 
//   fStateObserver?:Types.Action1<TState>) : React.FC<TProps> =>
//   (props) =>
//     {
//     const [state,setState] = React.useState<TState>(initialState);
//     return (f(props,state, x => {if(fStateObserver != null) fStateObserver(x);
//       setState(x);
//     }));
//   };

// export const createStoredComponent = <TProps,TState>(storage:StorageAccess<TState>) => {
//   return (initialState:TState,
//   f:Types.Func3<TProps,TState,Types.Action1<TState>,React.ReactElement<TProps>>,
//   fStateObserver?:Types.Action1<TState>) =>
//   (props:TProps) => {
//     let fStateObserver2 = (state:TState) => {
//       if(fStateObserver != null){
//         fStateObserver(state);
//         storage.storeIt(state);
//       }
//     };
//     let component = createStatedComponent(initialState,f,fStateObserver2)(props);
//     return component;
//   };
// };

// export let createStoredComponent = <TProps,TState>(key:string, initialProps:TProps, storage:StorageAccess<TState>) =>
//   (fState:Types.Func1<TProps,TState>, render:ComRenderer<TState>):CreateFunctionalComponent<TProps,TState> => (props:TProps):React.Component<TProps,TState> => {
//   // storage will be updated before component
//   let onChange = (f:Types.Action1<TState>) => (next:TState) => {
//     storage.storeIt(key,next);
//     // update component state
//     f(next);
//   }
//   console.log('component created:' + key);
//   let stcmp = createStatedComponent<TProps,TState>(initialProps);
//   let Component = stcmp(fState,(currentState,onStateChange) => render(currentState,onChange(onStateChange)));
//   return (<Component {...props} />)
// }
// export let FAIcon = props =>(
//   <span className={'icon ' + props.addedClasses}><i className={'fa '+props.icon}></i></span>);
  