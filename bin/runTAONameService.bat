REM Specify the environment variables
call ..\VE_Installer\setup.bat

start /B %TAO_ROOT%/orbsvcs/Naming_Service/Naming_Service.exe -ORBEndPoint iiop://%TAO_MACHINE%:%TAO_PORT%
PING 1.1.1.1 -n 1 -w 5000 >NUL
start /B %VE_SUITE_HOME%/VE_CE/Debug/WinServer.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
cmd
