REM Edit this file to reflect your local environment for VE_Suite

set VE_SUITE_HOME=C:\VE_Suite
set VJ_BASE_DIR=C:\vrjuggler-2.0-alpha4.win32-vc71
set VJ_DEPS_DIR=C:\vrjuggler-2.0-alpha4.win32-vc71-deps
set VTK_HOME=C:\VTK-4.4-LatestRelease\Install
set OMNI_HOME=C:\omniORB-4.0.5
set WX_HOME=C:\wxWindows-2.4.2
set JAVA_HOME=C:\j2sdk1.4.2_06
set ACE_ROOT=C:\ACE_wrappers
set TAO_ROOT=%ACE_ROOT%\TAO
set XERCESCROOT=C:\xerces-c-src_2_5_0

REM NOTE: On Windows the juggler location must point to a Windows
REM (local or on network) machine, otherwise user may run into runtime
REM problems when loading config files across the network

REM Everything below is relative to the above environment so user
REM shouldn't have to modify UNLESS "local" builds (debug for example)
REM are desired. In this case modify the appropriate variables accordingly.

set CONDUCTOR_BASE_DIR=%VE_SUITE_HOME%\VE_Conductor

set PHSHAREDSIZE=534773700

REM juggler debug output level
set VPR_DEBUG_ENABLE=1
set VPR_DEBUG_NFY_LEVEL=0

REM Juggler dependencies
set JCCL_DEFINITION_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set VJ_CFG_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set NSPR_ROOT=%VJ_DEPS_DIR%

set OMNIORB_CONFIG=%VE_SUITE_HOME%\VE_Installer\omniORB4.cfg
set OMNINAMES_LOGDIR=%VE_SUITE_HOME%\VE_Installer

REM Python build environment variables
set PYTHONPATH=%VJ_DEPS_DIR%\lib\python

set Path=%Path%;%VJ_DEPS_DIR%\bin;%VJ_DEPS_DIR%\lib;%VJ_BASE_DIR%\lib
set Path=%Path%;%VE_SUITE_HOME%\bin;%VTK_HOME%\bin;%WX_HOME%\lib
set Path=%Path%;%OMNI_HOME%\bin\x86_win32
set Path=%Path%;%ACE_ROOT%\bin;%XERCESCROOT%\Build\Win32\VC7\Debug

