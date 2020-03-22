namespace Shared

module String =
    let isValueString =
        function
        | null | "" -> false
        | x when System.String.IsNullOrWhiteSpace x -> false
        | _ -> true

