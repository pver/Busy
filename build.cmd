@echo off
cls

REM restore dotnet tools:
dotnet tool restore

dotnet paket restore

if errorlevel 1 (
  exit /b %errorlevel%
)

packages\FAKE\tools\FAKE.exe build.fsx %*
