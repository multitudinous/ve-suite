REM Specify the environment variables
call ..\VE_Installer\setup.bat

%TAO_ROOT%\orbsvcs\Naming_Service\Naming_Service -ORBEndPoint iiop://%TAO_MACHINE%:%TAO_PORT%
cmd
