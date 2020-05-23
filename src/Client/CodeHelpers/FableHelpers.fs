module CodeHelpers.FableHelpers

let stringify x = Fable.Core.JS.JSON.stringify x
let parse x = Fable.Core.JS.JSON.parse(x)