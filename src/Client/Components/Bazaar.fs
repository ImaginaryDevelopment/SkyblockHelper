module Components.Bazaar

open Components.SharedComponents
open Fable.React
open Fable.React.Props
open CodeHelpers.FableHelpers

type BazaarMode = Buy | Sell
type RateDisplayProps = {
    Mode:BazaarMode
    Values: {| name:string;value:int;div:int |} []
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
                        let v = formatNumber(float x.value/ float x.div, Some 2)
                        tr[Key x.name][
                            td[][unbox x.name]
                            td[][
                                unbox v
                            ]
                        ]
                    )

                )
            ]
        ]
    else div [][]


type PreconfiguredState = {
    Selected:string
    Category:string
    Values:Map<string,float>
}
type PreconfiguredProps = {
    State:PreconfiguredState
    OnStateChange: PreconfiguredState -> unit
    Mode:BazaarMode
}
let BazaarTable (props:{| preHeaders:string list; addedHeaders:string list|}, children) =
    let h = props.preHeaders @ ["Label";"Value";"Divisor";"Vendor"] @ props.addedHeaders 
    Table {|headers=Array.ofList h; children=children|}

let Preconfigured (props:PreconfiguredProps) = 
    let forms = AppDomain.SalesReference.preconfigurations
    ()

type Submenu = |Preconfigured | Custom | Merchants



type Model = {
    theme:string
    state:{|
        submenu:Submenu
        mode: BazaarMode
        Preconfigured:PreconfiguredState
        |}
}

