REM Double click this file to run winClient

REM Specify the environment variables
call ..\VE_Installer\setup.bat

REM Go to where your GUIPlugins directory is...
REM cd %VE_SUITE_HOME%\VE_TestSuite
REM cd C:\Demos\problemVTKs
REM cd C:\Demos\Jets_Data\transientData
REM cd C:\Demos\OPPD

WinClientd.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1 -VESDesktop -VESFile C:\devEnv\VE_Suite_1.0\VE_TestSuite\ss.ves
REM WinClientd.exe -ORBInitRef NameService=corbaloc:iiop:%TAO_MACHINE%:%TAO_PORT%/NameService -ORBDottedDecimalAddresses 1
cmd
