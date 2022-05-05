echo off
set PAKET_SKIP_RESTORE_TARGETS=true
rem https://stackoverflow.com/questions/980331/redirecting-passed-arguments-to-a-windows-batch-file
dotnet tool restore
if errorlevel 1 (
    echo "tool restore failed"
    exit /b %errorlevel%
)

set FAKE_DETAILED_ERRORS=true

@REM dotnet fake build -t "%*"
dotnet build
echo "starting build fsproj"
dotnet run --project .\fBuild\Build.fsproj -- -t %*