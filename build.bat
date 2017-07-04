nuget restore

nuget install NUnit.Runners -Version 2.6.4 -OutputDirectory testrunner

msbuild /p:Configuration=Release /p:Platform="Any CPU" ParsecClone.sln

"testrunner/NUnit.Runners.2.6.4/tools/nunit-console-x86.exe"  /framework:net-4.5./Samples/FsTests/bin/Release/FsTests.dll

