@echo off
REM Specify the environment variables

call .\setup.bat

REM shouldn't need to edit these
REM If pre-compiled dependencies are used, use the following call to the Naming service to:
start /B Naming_Service.exe -ORBEndPoint iiop://%TAO_MACHINE%:%TAO_PORT%

REM If user built dependencies are used, use the following call to the Naming service to:
REM start /B Naming_Service.exe -ORBEndPoint iiop://%TAO_MACHINE%:%TAO_PORT%

PING 1.1.1.1 -n 1 -w 5000 >NUL
start /B WinServerd.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1 
