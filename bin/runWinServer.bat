REM Specify the environment variables
call ..\VE_Installer\setup.bat

%VE_SUITE_HOME%\futuregen\Executive\Debug\WinServer.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
cmd
