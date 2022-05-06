open Fake.Core
open Fake.IO
open Fake.Core.TargetOperators

open Helpers

let sln =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "SkyblockHelper.sln")

let slnRootDir = System.IO.Path.GetDirectoryName sln

let hasServer = false
let hasShared = false
System.Environment.CurrentDirectory <- slnRootDir
let baseDir = slnRootDir
// let dirExistsOrNull x = if IO.dirExists x then x else null
let sharedPath = if hasShared then Path.getFullName $"{baseDir}/src/Shared" else null
let serverPath = if hasServer then Path.getFullName $"{baseDir}/src/Server" else null
let clientPath = Path.getFullName $"{baseDir}/src/Client"
let clientProjPath = System.IO.Path.Combine(clientPath, "Client.fsproj")
let serverDeployPath = Path.getFullName $"{baseDir}/deploy"
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

let initTargets() =
    Target.create "Clean" (fun _ ->
        Shell.cleanDir serverDeployPath
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
                "server", dotnet $"publish -c Release -o \"{serverDeployPath}\"" serverPath
            "client", dotnet (sprintf "fable \"%s\" -o output -s --run webpack" clientProjPath) baseDir
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
            let args = sprintf "fable watch \"%s\" -o output -s --run webpack-dev-server" clientProjPath
            "client", dotnet args baseDir
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
    ()

[<EntryPoint>]
let main args =
    eprintfn "Args:%A" args
    printfn "Args:%A" args
    let fakeContext =
        args
        |> Array.toList
        |> Context.FakeExecutionContext.Create false "../build.fsx"
    let context = fakeContext |> Context.RuntimeContext.Fake
    context |> Context.setExecutionContext
    initTargets()
    // initializeContext()
    printfn "Hello?"
    runOrDefault args
    // Fake.Core.Target.runOrDefaultWithArguments "Build"
    0