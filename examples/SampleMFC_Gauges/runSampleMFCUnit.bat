REM Specify the environment variables
call ..\..\..\VE_Suite\VE_Installer\setup.bat

cd C:\TSVEG\CAS\sampleapps\SampleMFC_Gauges\SampleMFC_GaugesUnit

SampleMFCUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
cmd
