@ECHO OFF
REM Double click this file to start the app in the test directory

call .\setup.bat

REM Go to where your parameter file is...
cd %VE_WORKING_DIR%

%VE_INSTALL_DIR%/bin/project_taod.exe  %VJ_BASE_DIR%/configFiles/simstandalone.jconf -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1
