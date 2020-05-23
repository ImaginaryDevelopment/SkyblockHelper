namespace Shared

module String =
    let isValueString =
        function
        | null | "" -> false
        | x when System.String.IsNullOrWhiteSpace x -> false
        | _ -> true
    let after delimiter (x:string) =
        if not <| isValueString delimiter then
            failwithf "no delimiter passed"
        let i = x.IndexOf(delimiter)
        x.[i+delimiter.Length..]

module Helpers =
    let flip f x y = f y x
    let (|ValueString|_|) =
        function
        | x when String.isValueString x -> Some x
        | _ -> None

