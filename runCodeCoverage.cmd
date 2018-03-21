copy Busy.Tests\bin\Debug\netcoreapp2.0\* build

cd Busy.Tests
dotnet minicover instrument --workdir ../codeCoverage --assemblies ../build/*.dll --sources ../**/*.fs
dotnet minicover reset

dotnet exec ..\build\Busy.Tests.dll

dotnet minicover uninstrument --workdir ../codeCoverage 

dotnet minicover htmlreport --workdir ../codeCoverage

dotnet minicover report --workdir ../codeCoverage

cd ..