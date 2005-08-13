REM Double click this file to start the app in the test directory

call .\setup.bat
REM Go to where your parameter file is...
set VEXMASTER=localhost
cd %VE_WORKING_DIR%

%VE_INSTALL_DIR%/bin/project_tao_osg_vep_cluster_d.exe %VJ_BASE_DIR%/share/vrjuggler/data/configFiles/simstandalone.jconf  -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1
