REM Double click this file to start Visual Studio with the projects loaded

REM Specify the environment variables
call setup.aspen.bat

REM only change this if your Visual Studio .NET is installed somewhere else

"C:\Program Files (x86)\Microsoft Visual Studio 8\Common7\IDE\devenv.exe" %VE_SUITE_HOME%\share\tools\aspen\VE_AspenUnit\VE_AspenUnit.sln
cmd
