REM Double click this file to start the app in the test directory

REM Specify the environment variables
call ..\VE_Installer\setup.bat

cd %VE_SUITE_HOME%\VE_TestSuite

project_taod.exe %VJ_BASE_DIR%/share/vrjuggler/data/configFiles/simstandalone.jconf -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1
cmd
