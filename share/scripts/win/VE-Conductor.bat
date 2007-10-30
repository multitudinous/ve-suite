@ECHO OFF
REM Double click this file to run winClient

REM Specify the environment variables
call .\setup.bat

REM Go to where your GUIPlugins directory is...
cd %VE_WORKING_DIR%

WinClientd.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1
