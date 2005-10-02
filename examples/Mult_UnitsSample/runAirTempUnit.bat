REM Specify the environment variables
call ..\VE_Suite\VE_Installer\setup.bat

cd C:\TSVEG\VE_Suite\examples\Mult_UnitsSample\units

AverageAirTempUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
cmd