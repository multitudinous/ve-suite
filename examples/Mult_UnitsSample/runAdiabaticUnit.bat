REM Specify the environment variables
call ..\..\VE_Installer\setup.bat

cd C:\TSVEG\VE_Suite\examples\Mult_UnitsSample\units

start AdiabaticFlameTempUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
start AverageAirTempUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService
cmd
