del /F /Q .\codecoverage

dotnet altcover /i=.\tests\Busy.Tests\bin\Debug\netcoreapp3.1 /o=.\codecoverage -r=.\codecoverage\BusyCoverage.xml -s=Busy.Tests --reportFormat=opencover
dotnet altcover /i=.\tests\Busy.Objects.Tests\bin\Debug\netcoreapp3.1 /o=.\codecoverage -r=.\codecoverage\BusyCoverage.xml -s=Busy.Objects.Tests --reportFormat=opencover

dotnet altcover runner -x "dotnet" -r ".\codecoverage" -- exec .\codecoverage\Busy.Tests.dll
dotnet altcover runner -x "dotnet" -r ".\codecoverage" -- exec .\codecoverage\Busy.Objects.Tests.dll

set REPORT_THRESHOLD=85
dotnet reportgenerator "-reports:.\codecoverage\BusyCoverage.xml" "-targetdir:.\codecoverage\report" "-reporttypes:Html;ThresholdConsoleSummary" -plugins:%cd%\tools\ThresholdConsoleSummaryReportBuilder.dll
