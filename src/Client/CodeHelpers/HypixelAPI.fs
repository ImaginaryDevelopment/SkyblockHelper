module CodeHelpers.HypixelAPI

open Elmish
open Microsoft.FSharp.Collections
open CodeHelpers.FableHelpers

type Method = GET | POST

type Promise<'t> = Fable.Core.JS.Promise<'t>
let inline promise f =
    Fable.Core.JS.Constructors.Promise.Create f
// let inline ``then`` f (x:Promise<'t>) =
//     x.``then``()
let inline ``then`` fResolve fReject (x:Promise<_>) =
    x.``then``(fResolve,fReject)

type XOptions = {
    Method: Method
    Url: string
    Headers: Map<string,string>
    Body: obj option
}

module CorsPromise =
    let request xopt =
        let p = promise (fun resolve reject->
            let xhr = Browser.XMLHttpRequest.XMLHttpRequest.Create()
            xhr.``open``(string xopt.Method, xopt.Url,true) //"http://www.example.org/example.txt")
            // xhr.setRequestHeader("Content-Type","text/plain")
            xopt.Headers
            |> Map.iter(fun k v ->
                xhr.setRequestHeader(k,v)
            )
            let f _ =
                if xhr.status >= 200 && xhr.status < 300 then
                    resolve(xhr.response)
                else reject(xhr.statusText)
            xhr.addEventListener("load",f)
            xhr.addEventListener("error", fun _ ->
                reject(xhr.statusText)
            )
            // toGlobal "hypixelXhr" xhr
            match xopt.Body with
            | Some v -> xhr.send(v)
            | None -> xhr.send()
        )
        p

let fetchExample (): Promise<string> =
    CorsPromise.request {
        Method= GET
        Url= "https://api.github.com"
        Headers= Map.empty
        Body= None
    }

let fetchCharacter (key,name) =
    CorsPromise.request {
        Method= GET
        Url= sprintf "http://api.hypixel.net/player?key=%s&name=%s" key name
        Headers= Map.empty
        Body= None
    }

