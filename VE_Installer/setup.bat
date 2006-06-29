REM Edit this file to reflect your local environment for VE_Suite

set VE_SUITE_HOME=C:\devEnv\VE_Suite_1.0
set VE_INSTALL_DIR=C:\devEnv\VE_Suite_1.0

set VJ_BASE_DIR=C:\devEnv\vrjuggler-2.0.1-win32-vc71
set VJ_DEPS_DIR=C:\devEnv\vrjuggler-2.0.1-win32-vc71-deps
set VTK_HOME=C:\devEnv\VES.1.0-Deps\vtk-5.0.0_Install
set WX_HOME=C:\devEnv\VES.1.0-deps\wxWidgets-2.6.3
set WX_ROOT=C:\devEnv\VES.1.0-Deps\wxWidgets-2.6.3
set JAVA_HOME=C:\j2sdk1.4.2_06
set ACE_ROOT=C:\devEnv\VES.1.0-Deps\ACE_wrappers
set TAO_ROOT=%ACE_ROOT%\TAO
set XERCESCROOT=C:\devEnv\VES.1.0-Deps\xerces-c-src_2_7_0
set OSGHOME=C:\devEnv\VES.1.0-Deps\OSG_OP_OT-1.0\OpenSceneGraph
set DEMETER_HOME=C:\devEnv\demeter-3.21
set TAO_MACHINE=localhost
REM set TAO_MACHINE=costello.vrac.iastate.edu
set TAO_PORT=1237
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
set VPR_DEBUG_NFY_LEVEL=2
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
set Path=%Path%;%VE_SUITE_HOME%\bin\Plugins\UI\win32;
set Path=%Path%;%VE_SUITE_HOME%\bin\Plugins\GE\win32;
set Path=%Path%;%ACE_ROOT%\bin;%ACE_ROOT%\lib;%XERCESCROOT%\Build\Win32\VC7.1\Debug
set Path=%Path%;%VE_SUITE_HOME%\bin\win32
set Path=%Path%;%OSGHOME%\bin;%VE_SUITE_HOME%\lib\win32
set Path=%Path%;%DEMETER_HOME%\bin;
