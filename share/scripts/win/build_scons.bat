REM Double click this file to start Visual Studio with the projects loaded
@ECHO ON
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

if "%VE_CONFIGURATION%" == "RELEASE" (GOTO ReleaseBUILD) else (GOTO DebugBUILD)

:ReleaseBUILD
scons default_debug_level=none prefix=C:\dev\VE_Suite\install-release  install
REM options_file=D:\devEnv\VES\options.cache.win32.XP.ia32.release
:DebugBUILD
scons default_debug_level=standard prefix=D:\devEnv\VES\install\debug options_file=D:\devEnv\VES\options.cache.win32.XP.ia32.debug install
cmd
