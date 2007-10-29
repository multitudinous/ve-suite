REM Specify the environment variables
call ..\VE_Installer\setup.bat

start /B %TAO_ROOT%/orbsvcs/Naming_Service/Naming_Service.exe -ORBEndPoint iiop://%TAO_MACHINE%:%TAO_PORT%
PING 1.1.1.1 -n 1 -w 5000 >NUL
start /B WinServerd.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1 
cmd
REM -ORBDebugLevel 10