REM Double click this file to start Visual Studio with the projects loaded

REM Specify the environment variables
call setup.bat

REM only change this if your Visual Studio .NET is installed somewhere else

%comspec% /k ""C:\Program Files (x86)\Microsoft Visual Studio 8\VC\vcvarsall.bat"" x86
REM "C:\Program Files (x86)\Microsoft Visual Studio 8\Common7\IDE\devenv.exe" %VE_SUITE_HOME%\VE_Suite.sln
cmd
