REM Specify the environment variables
cd %VE_INSTALL_DIR%
call ./setup.bat

cd C:\TSVEG\VE_Suite\examples\Mult_UnitsSample\units

start AdiabaticFlameTempUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1 
start AverageAirTempUnit.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1 
cmd
