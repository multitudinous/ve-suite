REM Specify the environment variables
call ..\VE_Installer\setup.bat

cd C:\TSVEG\netl\hyper\hyperUnit

NETL_hyperUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
cmd