rmdir /s /q nuget\lib
del nuget\*.nupkg

msbuild combinator.msbuild.proj

cd nuget

nuget pack