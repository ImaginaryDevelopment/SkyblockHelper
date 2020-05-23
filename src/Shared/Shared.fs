namespace Shared

module String =
    let isValueString =
        function
        | null | "" -> false
        | x when System.String.IsNullOrWhiteSpace x -> false
        | _ -> true
module Helpers =
    let (|ValueString|_|) =
        function
        | x when String.isValueString x -> Some x
        | _ -> None

