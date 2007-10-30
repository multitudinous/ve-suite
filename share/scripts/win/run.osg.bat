@ECHO OFF
REM Double click this file to start the app in the test directory

REM set up the environment...
call .\setup.bat

REM the directory where the parameter file
cd %VE_WORKING_DIR%

project_tao_osg_d.exe %VJ_BASE_DIR%/configFiles/simstandalone.jconf  -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1
