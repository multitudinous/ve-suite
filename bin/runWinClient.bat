REM Double click this file to run winClient

REM Specify the environment variables
call ..\VE_Installer\setup.bat

REM Go to where your GUIPlugins directory is...
cd %VE_SUITE_HOME%\futuregen\Framework

%VE_SUITE_HOME%\futuregen\Framework\WinClient.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService 
cmd
