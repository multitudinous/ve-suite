REM Double click this file to run winClient

REM Specify the environment variables
call ..\VE_Installer\setup.bat

REM Go to where your GUIPlugins directory is...
REM cd %VE_SUITE_HOME%\VE_Conductor\Framework\
REM cd %VE_SUITE_HOME%\..\deere\harvester

%VE_SUITE_HOME%\VE_Conductor\Framework\WinClient.exe -ORBDebugLevel 10 -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService 
cmd
