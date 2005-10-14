REM Specify the environment variables
call ..\..\VE_Installer\setup.bat

cd C:\TSVEG\VE_Suite\examples\SampleMFC_Gauges\Units

SampleMFCUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
cmd
