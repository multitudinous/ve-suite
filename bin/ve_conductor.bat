REM Double click this file to start the gui

REM Some locations are defined here
REM The locations should mirror the settings in vesuite.OMNIBUILD.setup.bat

set VJ_BASE_DIR=C:\vrjuggler-2.0-alpha4.win32-vc71
set VJ_DEPS_DIR=C:\vrjuggler-2.0-alpha4.win32-vc71-deps

REM location of VE_Suite directory
set VE_SUITE_HOME=C:\VE_Suite

REM location of VTK
set VTK_HOME=C:\VTK-4.4-LatestRelease\install

REM location of java
set JAVA_HOME=C:\j2sdk1.4.2_05

REM OmniORB build environment varibles
set OMNI_HOME=C:\omniORB-4.0.4

REM Location for TAO and ACE 
REM set ACE_ROOT=C:\ACE_wrappers
REM set TAO_ROOT=%ACE_ROOT%\TAO

REM Location for xerces
REM set XERCESCROOT=C:\xerces-c-src_2_5_0

set WX_HOME=C:\wxWindows-2.4.2

REM No editing should be needed below this line

REM Conductor directory--shouldn't be modified!!
set CONDUCTOR_BASE_DIR=%VE_SUITE_HOME%\VE_Conductor

REM OpenGl performer variable
set PHSHAREDSIZE=534773700

REM juggler debug output level
set VPR_DEBUG_NFY_LEVEL=1
set VPR_DEBUG_ENABLE=1

REM Juggler dependencies
set JCCL_DEFINITION_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set VJ_CFG_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set NSPR_ROOT=%VJ_DEPS_DIR%

REM OmniORB build environment varibles
set OMNIORB_CONFIG=%VE_SUITE_HOME%\VE_Installer\omniORB4.cfg
set OMNINAMES_LOGDIR=%VE_SUITE_HOME%\VE_Installer

REM Python build environment variables
set PYTHONPATH=%VJ_DEPS_DIR%\lib\python

set Path=%Path%;%OMNI_HOME%\bin\x86_win32;%VJ_DEPS_DIR%\bin;%VJ_DEPS_DIR%\lib;%VJ_BASE_DIR%\lib;%VE_SUITE_HOME%\bin;%VTK_HOME%\bin
set Path=%Path%;%ACE_ROOT%\bin;%XERCESCROOT%\Build\Win32\VC7\Debug;%WX_HOME%\lib

%VE_SUITE_HOME%\bin\win32\VE_Conductor.exe
cmd
