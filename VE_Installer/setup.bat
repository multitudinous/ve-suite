REM Edit this file to reflect your local environment for VE_Suite

set VE_SUITE_HOME=C:\TSVEG\VE_Suite
set VJ_BASE_DIR=C:\vrjuggler-2.0-beta2.win32-vc71
set VJ_DEPS_DIR=C:\vrjuggler-2.0-beta2.win32-vc71-deps
set VTK_HOME=C:\VTK_4.4_WinXP
set OMNI_HOME=C:\vrjuggler-2.0-beta2.win32-vc71-deps
set WX_HOME=C:\wxWidgets-2.6.0
set JAVA_HOME=C:\j2sdk1.4.2_06
set ACE_ROOT=C:\ACE_wrappers
set TAO_ROOT=%ACE_ROOT%\TAO
set XERCESCROOT=C:\xerces-c-src_2_6_0
set WX_ROOT=C:\wxWidgets-2.6.0
set OSGHOME=C:\OSG_OP_OT-0.9.9\OpenSceneGraph
set TAO_MACHINE=localhost
set TAO_PORT=1239
set SNX_BASE_DIR=%VJ_BASE_DIR%

REM NOTE: On Windows the juggler location must point to a Windows
REM (local or on network) machine, otherwise user may run into runtime
REM problems when loading config files across the network

REM Everything below is relative to the above environment so user
REM shouldn't have to modify UNLESS "local" builds (debug for example)
REM are desired. In this case modify the appropriate variables accordingly.

set CFDHOSTTYPE=WIN32

set CONDUCTOR_BASE_DIR=%VE_SUITE_HOME%\VE_Conductor

REM set PHSHAREDSIZE=534773700

REM juggler debug output level
set VPR_DEBUG_ENABLE=1
set VPR_DEBUG_NFY_LEVEL=1
set NO_PERF_PLUGIN=TRUE
set NO_RTRC_PLUGIN=TRUE
set PFNFYLEVEL=0
set OSG_THREAD_SAFE_REF_UNREF=1

REM Juggler dependencies
set JCCL_DEFINITION_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set VJ_CFG_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set NSPR_ROOT=%VJ_DEPS_DIR%

set OMNIORB_CONFIG=%VE_SUITE_HOME%\VE_Installer\omniORB4.cfg
set OMNINAMES_LOGDIR=%VE_SUITE_HOME%\VE_Installer

REM Python build environment variables
set PYTHONPATH=%VJ_DEPS_DIR%\lib\python

set Path=%Path%;%VJ_DEPS_DIR%\bin;%VJ_DEPS_DIR%\lib;%VJ_BASE_DIR%\lib
set Path=%Path%;%VE_SUITE_HOME%\bin;%VTK_HOME%\bin;%WX_HOME%\lib\vc_dll
set Path=%Path%;%OMNI_HOME%\bin\x86_win32;%VE_SUITE_HOME%\bin\win32
set Path=%Path%;%ACE_ROOT%\bin;%XERCESCROOT%\Build\Win32\VC7\Debug
set Path=%Path%;%OSGHOME%\bin;%VE_SUITE_HOME%\lib\win32