REM Double click this file to start Visual Studio with the projects loaded

REM Specify the environment variables
call setup.bat

REM only change this if your Visual Studio .NET is installed somewhere else
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv.exe" %VE_SUITE_HOME%\VE_Open\VE_XML\VE_CAD\CAD.sln
cmd
