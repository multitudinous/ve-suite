REM Double click this file to start the app in the test directory

REM Specify the environment variables
call ..\VE_Installer\setup.bat

REM Go to where your parameter file is...
cd %VE_SUITE_HOME%\VE_TestSuite

%VE_SUITE_HOME%\bin\win32\project_taod.exe %VJ_BASE_DIR%/share/vrjuggler/data/configFiles/simstandalone.jconf -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1
cmd
