@ECHO ON
REM Double click this file to start the app in the test directory

call ..\setup.bat
REM Go to where your parameter file is...

cd .\

set VEXMASTER=costello
project_tao_osg_vep_d.exe ..\..\share\vecr_configs\vecr.jconf  -ORBInitRef NameService=corbaloc:iiop:costello:1239/NameService -ORBDottedDecimalAddresses 1 -VESCluster
pause