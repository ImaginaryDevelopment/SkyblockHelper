module CodeHelpers.FableHelpers
open Fable.Core.JS

let stringify x = JSON.stringify x
let parse x = JSON.parse(x)

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