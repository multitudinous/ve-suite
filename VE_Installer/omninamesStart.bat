REM Double click this file to start the nameserver

REM Location of VE_Suite directory and OmniORB are defined here
REM The locations should mirror the settings in vesuite.OMNIBUILD.setup.bat

set VE_SUITE_HOME=C:\VE_Suite

set OMNI_HOME=C:\omniORB-4.0.4

REM No editing should be needed below this line

set OMNIORB_CONFIG=%VE_SUITE_HOME%\VE_Installer\omniORB4.cfg
set OMNINAMES_LOGDIR=%VE_SUITE_HOME%\VE_Installer

%OMNI_HOME%\bin\x86_win32\omniNames.exe -start 2809
cmd
