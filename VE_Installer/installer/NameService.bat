@echo off
REM Specify the environment variables

call .\setup.bat

REM shouldn't need to edit these
REM If pre-compiled dependencies are used, use the following call to the Naming service to:
start /B %VE_DEPS_DIR%\bin\Naming_Service.exe -ORBEndPoint iiop://%TAO_MACHINE%:%TAO_PORT%

REM If user built dependencies are used, use the following call to the Naming service to:
REM start /B %TAO_ROOT%\orbsvcs\Naming_Service\Naming_Service.exe -ORBEndPoint iiop://%TAO_MACHINE%:%TAO_PORT%

PING 1.1.1.1 -n 1 -w 5000 >NUL
start /B %VE_INSTALL_DIR%/bin/WinServerd.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1 
