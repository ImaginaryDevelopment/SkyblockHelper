open Fake.Core
open Fake.IO

open Helpers

initializeContext()

let hasServer = false
let hasShared = false
let baseDir = "."
// let dirExistsOrNull x = if IO.dirExists x then x else null
let sharedPath = if hasShared then Path.getFullName $"{baseDir}/src/Shared" else null
let serverPath = if hasServer then Path.getFullName $"{baseDir}/src/Server" else null
let clientPath = Path.getFullName $"{baseDir}/src/Client"
let deployPath = Path.getFullName $"{baseDir}/deploy"
let sharedTestsPath = if hasShared then Path.getFullName $"{baseDir}/tests/Shared" else null
let serverTestsPath = if hasServer then Path.getFullName $"{baseDir}/tests/Server" else null
let clientTestsPath = Path.getFullName $"{baseDir}/tests/Client"

[
    sharedPath
    serverPath
    clientPath
]
|> List.filter(System.String.IsNullOrEmpty >> not)
|> List.iter(fun path ->
    if System.IO.Directory.Exists path |> not then
        eprintfn "Directory '%s' not found from '%s' ('%s')" path System.Environment.CurrentDirectory (Path.getFullName System.Environment.CurrentDirectory)
        failwith "bad build.fs"
)

Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet "fable clean --yes" clientPath
)

Target.create "InstallClient" (fun _ ->
    let wd = Path.getFullName "."
    Trace.logf "running install in '%s'" wd
    run npm "install" wd
)

Target.create "Bundle" (fun _ ->
    [
        if hasServer then
            "server", dotnet $"publish -c Release -o \"{deployPath}\"" serverPath
        "client", dotnet "fable -o output -s --run webpack -p" clientPath
    ] |> runParallel
)
#if FARMER
Target.create "Azure" (fun _ ->
    let web = webApp {
        name "fable_elmish"
        zip_deploy "deploy"
    }
    let deployment = arm {
        location Location.WestEurope
        add_resource web
    }

    deployment
    |> Deploy.execute "fable_elmish" Deploy.NoParameters
    |> ignore
)
#endif

Target.create "Run" (fun _ ->
    if hasShared then
        run dotnet "build" sharedPath
    [
        if hasServer then
            "server", dotnet "watch run" serverPath
        "client", dotnet "fable watch -o output -s --run webpack-dev-server" clientPath
    ]
    |> runParallel
)

Target.create "RunTests" (fun _ ->
    if hasShared then
        run dotnet "build" sharedTestsPath
    [
        if hasServer then
            "server", dotnet "watch run" serverTestsPath
        "client", dotnet "fable watch -o output -s --run webpack-dev-server --config ../../webpackage.tests.config.js" clientTestsPath
    ]
    |> runParallel
)

Target.create "Format" (fun _ ->
    run dotnet "fantomas .. -r" "src"
)

open Fake.Core.TargetOperators

let dependencies = [
    #if FARMER
    "Clean"
        ==> "InstallClient"
        ==> "Bundle"
        ==> "Azure"
    #endif
    "Clean"
        ==> "InstallClient"
        ==> "Run"
    "Clean"
        ==> "InstallClient"
        ==> "RunTests"


]
[<EntryPoint>]
let main args = runOrDefault args