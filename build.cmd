echo off
set PAKET_SKIP_RESTORE_TARGETS=true
rem https://stackoverflow.com/questions/980331/redirecting-passed-arguments-to-a-windows-batch-file
dotnet fake build -t "%*"