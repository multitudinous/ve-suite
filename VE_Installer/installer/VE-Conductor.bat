@ECHO OFF
REM Double click this file to run winClient

REM Specify the environment variables
call .\setup.bat

REM Go to where your GUIPlugins directory is...
cd %VE_USER_PLUGIN_DIR%

%VE_INSTALL_DIR%/bin/WinClientd.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1
