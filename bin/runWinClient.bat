REM Double click this file to run winClient

REM Specify the environment variables
call ..\VE_Installer\setup.bat

REM Go to where your GUIPlugins directory is...
REM cd %VE_SUITE_HOME%\VE_Conductor\Framework\
cd %VE_SUITE_HOME%\VE_Conductor\Framework\

%VE_SUITE_HOME%\VE_Conductor\Framework\WinClient.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService 
REM cmd
