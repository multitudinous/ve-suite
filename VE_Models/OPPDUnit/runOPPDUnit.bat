REM Double click this file to run Dave's demo

REM Specify the environment variables
call ..\..\VE_Installer\setup.bat

%VE_SUITE_HOME%\VE_Models\OPPDUnit\Debug\OPPDUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
cmd
