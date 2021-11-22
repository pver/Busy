del /F /Q .\codecoverage

dotnet altcover /i=.\tests\Busy.Tests\bin\Debug\netcoreapp3.1 /o=.\codecoverage -r=.\codecoverage\BusyCoverage.xml -s=Busy.Tests --reportFormat=opencover

dotnet altcover runner -x "dotnet" -r ".\codecoverage" -- exec .\codecoverage\Busy.Tests.dll

copy .\tools\ThresholdConsoleSummary.dll .\packages\ReportGenerator\tools\
set REPORT_THRESHOLD=85
.\packages\ReportGenerator\tools\ReportGenerator.exe "-reports:.\codecoverage\BusyCoverage.xml" "-targetdir:.\codecoverage\report" "-reporttypes:Html;ThresholdConsoleSummary"
