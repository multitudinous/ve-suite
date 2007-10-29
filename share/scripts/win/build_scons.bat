REM Double click this file to start Visual Studio with the projects loaded
@ECHO OFF
REM Specify the environment variables
REM pass in DEBUG or RELEASE
set VE_CONFIGURATION=%1

if "%VE_CONFIGURATION%"=="RELEASE" (@ECHO OFF) else (@ECHO ON)
ECHO You chose to build %VE_CONFIGURATION%

REM only change this if your Visual Studio .NET is installed somewhere else

REM %comspec% /k ""C:\Program Files\Microsoft Visual Studio 8\VC\vcvarsall.bat"" x86
call "C:\Program Files\Microsoft Visual Studio 8\VC\vcvarsall.bat"
call setupXP.bat %VE_CONFIGURATION%

REM "C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\devenv.exe" %VE_SUITE_HOME%\VE_Suite.sln
cd %VE_SUITE_HOME%

if "%VE_CONFIGURATION%" == "RELEASE" GOTO Release else GOTO Debug
:Release
set
scons default_debug_level=none install

:Debug
scons default_debug_level=standard install
cmd
