REM Specify the environment variables
call .\setup.bat

cd C:\TSVEG\VE_Suite\examples\SampleMFC_Gauges\Units

start SampleMFCUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1 
cmd
