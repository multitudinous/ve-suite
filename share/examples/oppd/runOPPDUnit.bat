REM Specify the environment variables
call C:\VE_Suite.0.9.3\setup.bat

cd C:\TSVEG\oppd\OPPDUnit

OPPDUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
cmd
