@ECHO OFF
REM Double click this file to start the app in the test directory

call .\setup.bat
REM Go to where your parameter file is...

cd %VE_WORKING_DIR%

project_tao_osg_vep_cluster_d.exe %VJ_BASE_DIR%/configFiles/simstandalone.jconf  -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1 <%1
