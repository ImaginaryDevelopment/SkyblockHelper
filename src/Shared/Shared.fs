namespace Shared

module String =

    let isValueString =
        function
        | null | "" -> false
        | x when System.String.IsNullOrWhiteSpace x -> false
        | _ -> true

    let trim =
        function
        | null | "" -> ""
        | x -> x.Trim()

    let after delimiter (x:string) =
        if not <| isValueString delimiter then
            failwithf "no delimiter passed"
        let i = x.IndexOf(delimiter)
        x.[i+delimiter.Length..]

// did not appear to work
// module DU =
//     open Microsoft.FSharp.Reflection
//     let inline fromString<'t>(s:string) =
//         match Microsoft.FSharp.Reflection.FSharpType.GetUnionCases typeof<'t> |> Array.filter(fun case -> case.Name = s) with
//         | [| case |] -> Some(FSharpValue.MakeUnion(case, Array.empty):?> 't)
//         | _ -> None

module Helpers =

    let flip f x y = f y x

    let (|ValueString|_|) =
        function
        | x when String.isValueString x -> Some x
        | _ -> None
    let (|EqualsI|_|) y =
        function
        | ValueString x ->
            if x.Equals(y,System.StringComparison.InvariantCultureIgnoreCase) then
                Some ()
            else None
        | _ -> None

    let inline tryParse f x =
        match f x with
        | true, x -> Some x
        | _ -> None

    let tryParseInt x = tryParse System.Int32.TryParse x
    let tryParseDec x = tryParse System.Decimal.TryParse x



open Helpers

module Option =
    let ofValueString =
        function
        | ValueString x -> Some x
        |_ -> None
    let ofResult x =
        match x with
        | Ok x -> Some x
        | _ -> None

module Result =
    let iter f (x:Result<_,_>)=
        match x with
        |Ok x -> f x
        | _ -> ()

type NameValue = {Name:string;Value:string}
