REM Double click this file to start the app in the test directory

REM Specify the environment variables
call ..\VE_Installer\setup.bat

REM Go to where your parameter file is...
REM cd %VE_SUITE_HOME%\VE_TestSuite
REM cd C:\VE_Suite_112204\VE_TestSuite\dClarke
REM cd C:\VE_Suite_112204\VE_TestSuite\cpicker
REM cd C:\VE_Suite_112204\VE_TestSuite\deereEngine
cd C:\VE_Suite_112204\VE_TestSuite\angran

project_tao_osgd.exe %VJ_BASE_DIR%/share/vrjuggler/data/configFiles/simstandalone.jconf -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1
cmd
