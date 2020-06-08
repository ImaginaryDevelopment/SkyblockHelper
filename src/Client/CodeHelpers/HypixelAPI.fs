module CodeHelpers.HypixelApi

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

// https://cors-anywhere.herokuapp.com/
// https://robwu.nl/cors-anywhere.html
let fetchHerokuProxy url =
    CorsPromise.request {
        Method= GET
        Url= sprintf "https://cors-anywhere.herokuapp.com/%s" url
        Headers= Map.empty
        Body= None
    }

let fetch useProxy url =
    if useProxy then
        fetchHerokuProxy url
    else
        CorsPromise.request {
            Method= GET
            Url= url
            Headers= Map.empty
            Body= None
        }


type ApiReqType =
    | MinecraftUuid of name:string * timestamp: string option
    | HypixelProfile of key:string * name:string
    | HypixelSkyblockProfile of key:string * uuid:string

let getUrl =
    function
    | MinecraftUuid (n,tsOpt) ->
        // https://api.mojang.com/users/profiles/minecraft/<username>?at=<timestamp>
        let qs = tsOpt |> Option.map (sprintf "?at=%s") |> Option.defaultValue ""
        let url = sprintf "https://api.mojang.com/users/profiles/minecraft/%s%s" n qs
        url
    | HypixelProfile(k,n) ->
        let url = sprintf "http://api.hypixel.net/player?key=%s&name=%s" k n
        url
    | HypixelSkyblockProfile(k,u) ->
        let url = sprintf "https://api.hypixel.net/Skyblock/profiles?key=%s&uuid=%s" k u
        url

// let fetchUuid useProxy (name,tsOpt) =
//     // https://api.mojang.com/users/profiles/minecraft/<username>?at=<timestamp>
//     let qs = tsOpt |> Option.map (sprintf "?at=%s") |> Option.defaultValue ""
//     let url = sprintf "https://api.mojang.com/users/profiles/minecraft/%s%s" name qs
//     if useProxy then
//         fetchHerokuProxy url
//     else
//         CorsPromise.request {
//             Method= GET
//             Url= url
//             Headers= Map.empty
//             Body= None
//         }


// let fetchHypixelProfile useProxy (key,name) =
//     let url = sprintf "http://api.hypixel.net/player?key=%s&name=%s" key name
//     if useProxy then
//         fetchHerokuProxy url
//     else
//         CorsPromise.request {
//             Method= GET
//             Url= url
//             Headers= Map.empty
//             Body= None
//         }

// // https://api.hypixel.net/Skyblock/profiles?key=[KEY]&uuid=[UUID]
// let fetchSkyblockProfile useProxy (key,uuid) =
//     let url = sprintf "https://api.hypixel.net/Skyblock/profiles?key=%s&uuid=%s" key uuid
//     if useProxy then
//         fetchHerokuProxy url
//     else
//         CorsPromise.request {
//             Method= GET
//             Url= url
//             Headers= Map.empty
//             Body= None
//         }
