REM Double click this file to start the app in the test directory

REM Specify the environment variables
call ..\VE_Installer\setup.bat

cd %VE_SUITE_HOME%\VE_TestSuite

%VE_SUITE_HOME%\bin\win32\project.exe %VE_SUITE_HOME%\VE_Xplorer\vjconfig\sim.base.jconf %VJ_BASE_DIR%\share\vrjuggler\data\configFiles\sim.wand.mixin.jconf
cmd
