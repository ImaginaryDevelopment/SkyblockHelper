module CodeHelpers.FableHelpers
open Fable.Core.JS
open Fable.Core.JsInterop
open Shared
open Elmish
open Fable.Core
open Thoth.Json

let private debug = false
// let Resolver.serialize x = JSON.Resolver.serialize x
// let parse x = JSON.parse(x)
type Resolver =
    // type Encoder<'T> = 'T -> JsonValue
    static member MapEncoder<'t>(coders) = // : Encoder<FSharp.Collections.Map<_,_>> =
        let mapEncoder : Encoder<_> =
            fun (x:FSharp.Collections.Map<string,float>) ->
            Thoth.Json.Encode.object (
                                        x
                                        |> FSharp.Collections.Map.toSeq
                                        |> Seq.map(fun (k,v) ->
                                            k, Thoth.Json.Encode.float v
                                        )
                    )
        // type Decoder<'T> = string -> JsonValue -> Result<'T, DecoderError>
        let mapDecoder : Decoder<FSharp.Collections.Map<string,float>> =
            fun x jv ->
                let decoderish= Decode.dict Decode.float
                decoderish x jv
            // let decoderish : Decoder< (string*float)[] > = Thoth.Json.Decode.Auto.generateDecoder()
            // fun (x:string) jv ->
            //     Decode.map2 Decode.di
            //     match Decode.fromString decoderish x with
            //     | Ok x ->
            //         let result =
            //             x
            //             |> FSharp.Collections.Map.ofArray
            //             |> Result<_,string>.Ok
            //         result
            //     | Error e -> Error e
        let coders =
            coders
            |> Extra.withCustom mapEncoder mapDecoder
        coders
    static member serialize(x:'t, [<Inject>] ?resolver: ITypeResolver<'t>): string =
        let extra = Resolver.MapEncoder Extra.empty
        Thoth.Json.Encode.Auto.toString(2,x,extra=extra,resolver=resolver.Value)
    static member deserialize<'t>(x:string, [<Inject>] ?resolver: ITypeResolver<'t>) : 't option =
        let extra = Resolver.MapEncoder Extra.empty
        match Thoth.Json.Decode.Auto.fromString(x, extra= extra, resolver= resolver.Value) with
        | Ok v -> Some v
        | _ -> 
            None

// for debugging
let inline toGlobal (name:string) value =
    printfn "Adding global %s" name
    Browser.Dom.self?(name) <- value
    ()
// let addDays (x:Date,days:int):Date =
//     let date = Constructors.Date.Create(x.valueOf())
//     date.AddDays(days)

let pascal (x:string) =
    (string x.[0]).ToUpper() + x.[1..]

let formatNumber (num:float,places: int option): string =
    let places = Option.defaultValue 2 places
    if isNull <| box num then "null"
    elif isNaN num then "NaN"
    else num.ToString("n" + string places)

let getAttrValue name (x:Browser.Types.HTMLElement) =
    if not <| isNull x.attributes then
        if debug then
            eprintfn "Found attribute values %i" x.attributes.length
            console.log(x)
            [0 .. x.attributes.length - 1]
            |> Seq.iter(fun i ->
                let attr = x.attributes.[i]
                eprintfn "\tFound attribute: %s %A" attr.name attr.value
            )
        x.attributes.getNamedItem name
        |> Option.ofObj
        |> Option.bind (fun x -> x.value |> Option.ofObj)
    else None

let getTargetAsHtml (ev:Browser.Types.Event) =
    Option.ofObj ev
    |> Option.bind (fun ev -> Option.ofObj ev.target)
    |> Option.map(box >> unbox<Browser.Types.HTMLElement>)

let getName (ev:Browser.Types.Event) =
    match getTargetAsHtml ev with
    | Some target ->
        getAttrValue"data-name" target
        |> Option.orElseWith (fun () -> getAttrValue "name" target)
        |> Option.defaultValue ""
    | None -> ""

// let getValue(ev:Browser.Types.Event) =
//     match getTargetAsHtml ev with
//     | Some target ->
//         getAttrValue"value" target
//         |> Option.defaultValue ""
//     | None -> ""


// let getTargetInfo =

let getTargetName title ev =
    try
        let name = getName ev
        Ok name 
    with ex ->
        console.error(title + ".getTargetName")
        Error ex.Message

let getTargetValue title (ev:Browser.Types.Event):string option =
    try
        console.log("getTargetValue.target", ev.target)
        let value =
            // https://stackoverflow.com/questions/55093894/how-to-add-the-selected-attribute-to-a-select-option-in-fable
            ev.target?value
            |> Option.ofObj
        value
    with e ->
        console.error(title + ".getTargetValue",e)
        None

let toggleArrayValue (source: _[],target) =
    if source |> FSharp.Collections.Array.contains target then
        source |> FSharp.Collections.Array.filter(fun x -> x <> target)
    else source |> FSharp.Collections.Array.append ([| target |])
let toggleListValue (source: _ list) target =
    if source |> List.contains target then
        source |> List.filter(fun x -> x <> target)
    else target::source

let mapCmd f model cmd =
    model, cmd |> Cmd.map f
let mapUpdate fModel fMsg model cmd =
    fModel model, cmd |> Cmd.map fMsg