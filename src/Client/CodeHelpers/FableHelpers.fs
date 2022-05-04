module CodeHelpers.FableHelpers
#line 2
open Fable.Core.JS
open Fable.Core.JsInterop
open Shared
open Shared.Helpers
open Elmish
open Fable.Core
open Thoth.Json

module Map = FSharp.Collections.Map
type Map<'k,'v when 'k : comparison> = FSharp.Collections.Map<'k,'v>

let private debug = false
// let Resolver.serialize x = JSON.Resolver.serialize x
// let parse x = JSON.parse(x)
type Resolver =
    // type Encoder<'T> = 'T -> JsonValue
    static member inline MapEncoder<'t>(coders) = // : Encoder<FSharp.Collections.Map<_,_>> =
        let mapEncoder : Encoder<_> =
            fun (x:Map<string,float>) ->
            Thoth.Json.Encode.object (
                                        x
                                        |> Map.toSeq
                                        |> Seq.map(fun (k,v) ->
                                            k, Thoth.Json.Encode.float v
                                        )
                    )
        // type Decoder<'T> = string -> JsonValue -> Result<'T, DecoderError>
        let mapDecoder : Decoder<Map<string,float>> =
            fun x jv ->
                let decoderish= Decode.dict Decode.float
                decoderish x jv
        let coders =
            coders
            |> Extra.withCustom mapEncoder mapDecoder
        coders
    // static member Serialize(x:'t, [<Inject>] ?resolver: ITypeResolver<'t>): string =
    //     let extra = Resolver.MapEncoder Extra.empty
    //     Thoth.Json.Encode.Auto.toString(2,x,extra=extra, resolver=resolver.Value)
    static member inline Serialize(x:'t): string =
        let extra = Resolver.MapEncoder Extra.empty
        Thoth.Json.Encode.Auto.toString(2,x,extra=extra)
    static member inline Deserialize<'t>(x:string) : 't option =
        let extra = Resolver.MapEncoder Extra.empty
        match Thoth.Json.Decode.Auto.fromString(x, extra= extra) with
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

let formatNumber (places: int option) (num:float) : string =
    let formatCommas (x:string) =
        // printfn "Format %s" x
        [0 .. x.Length - 1]
        |> List.map(fun i -> string x.[i])
        // |> List.skip 1
        |> List.rev
        |> List.mapi (fun i c ->
            if (i+1) % 3 = 0 then
                sprintf ",%s" c
            else
                c
        )
        |> List.rev
        |> String.concat ""
        |> String.trim1 [","]
    let places = Option.defaultValue 2 places
    if isNull <| box num then "null"
    elif isNaN num then "NaN"
    else
        let input = string num
        // printfn "Testing input %s %.2f" input num
        match input with
        | Before "." b & After "." aft ->
            b, if places > 0 then Some aft else None
        | _ -> input, None
        |> function // fix up commas
            | b, aft when b.Length > 3 ->
                formatCommas b, aft
            | b,aft -> b,aft
        |> function
            | b, None -> if places > 0 then sprintf "%s.%s" b <| String.replicate places "0" else b
            | b, Some aft ->
                if places > aft.Length then
                    "0"
                    |> String.replicate (places - aft.Length)
                    |> fun padding -> aft + padding
                    |> sprintf "%s.%s" b
                elif places < aft.Length then
                    aft.[0 .. places - 1]
                    |> sprintf "%s.%s" b
                else sprintf "%s.%s" b aft


let formatInt (num:int) : string =
    formatNumber (Some 0) (float num)

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

let toggleMapListValue (source: Map<_,_ list>) k v =
    source
    |> Map.tryFind k
    |> function
        | None ->
            source |> Map.add k [v]
        | Some [x] when x = v ->
            source |> Map.remove k
        | Some items when items |> List.contains v ->
            source |> Map.add k (items |> List.filter(fun x -> x = v))
        | Some items ->
            source |> Map.add k (v::items)



let mapCmd f model cmd =
    model, cmd |> Cmd.map f
let mapUpdate fModel fMsg model cmd =
    fModel model, cmd |> Cmd.map fMsg