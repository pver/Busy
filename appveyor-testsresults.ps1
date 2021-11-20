# upload test results to AppVeyor
$wc = New-Object 'System.Net.WebClient'
$wc.UploadFile("https://ci.appveyor.com/api/testresults/nunit/$($env:APPVEYOR_JOB_ID)", (Join-Path $env:APPVEYOR_BUILD_FOLDER .\tests\Busy.Tests\bin\Debug\netcoreapp2.0\Busy.Tests.TestResults.xml))