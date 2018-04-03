del /F /Q .\codecoverage

dotnet .\packages\altcover\tools\netcoreapp2.0\AltCover.dll /i=.\Busy.Tests\bin\Debug\netcoreapp2.0 /o=.\codecoverage -x=.\codecoverage\BusyCoverage.xml --opencover

dotnet .\packages\altcover\tools\netcoreapp2.0\AltCover.dll runner -x "dotnet" -r ".\codecoverage" -- exec .\codecoverage\Busy.Tests.dll

copy .\tools\ThresholdConsoleSummary.dll .\packages\ReportGenerator\tools\
set REPORT_THRESHOLD=85
.\packages\ReportGenerator\tools\ReportGenerator.exe "-reports:.\codecoverage\BusyCoverage.xml" "-targetdir:.\codecoverage\report" "-reporttypes:Html;ThresholdConsoleSummary" "-assemblyfilters:-Busy.Tests"
