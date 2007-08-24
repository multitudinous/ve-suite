REM Edit this file to reflect your local environment for VE_Suite

set VE_SUITE_HOME=C:\devEnv\VE_Suite_1.0
set VE_INSTALL_DIR=C:\devEnv\VE_Suite_1.0

REM set VJ_BASE_DIR=C:\devEnv\VES.1.0-Deps\vrjuggler-2.0-svn\vrjuggler-2.0.3-win32-vc80
set VJ_DEPS_DIR=C:\devEnv\VES.1.0-Deps\VRJuggler2.0.3-1
set VJ_BASE_DIR=C:\devEnv\VES.1.0-Deps\VRJuggler2.0.3-1
REM set VJ_BASE_DIR=C:\devEnv\VES.1.0-Deps\vrjuggler-2.0.1-win32-vc80
REM set VJ_DEPS_DIR=C:\devEnv\VES.1.0-Deps\vrjuggler-2.0.1-win32-vc80-deps
REM set VTK_HOME=C:\devEnv\VES.1.0-Deps\vtk-5.0.0_Install_vc8
set VTK_HOME=C:\devEnv\VES.1.0-Deps\vtk_cvs\vtk_vc80_install
REM set WX_HOME=C:\WxWidgets_2.8.0_Pre-Compile_vc8.0
REM set WX_HOME=C:\WxWidgets_2.8.0_Pre-Compile_vc8.0
set WX_HOME=C:\devEnv\VES.1.0-deps\wxWidgets-2.8.3
set WX_ROOT=C:\devEnv\VES.1.0-Deps\wxWidgets-2.8.3
set JAVA_HOME=C:\j2sdk1.4.2_06
set ACE_ROOT=C:\devEnv\VES.1.0-Deps\ACE_wrappers
REM set ACE_ROOT=C:\ACETAO_5.5.1_Pre-Compile_vc8.0
set TAO_ROOT=%ACE_ROOT%\TAO
set XERCESCROOT=C:\devEnv\VES.1.0-Deps\xerces-c-src_2_7_0
set BULLET_HOME=C:\devEnv\VES.1.0-Deps\bullet_svn
REM set BULLET_HOME=C:\Bullet_Pre-Compile_2.42
REM set OSGHOME=C:\OSG_1.2_Pre-Compile_vc8.0
REM PRODUCER_HOME=%OSGHOME%
REM OPENTHREADS_HOME=%OSGHOME%
set OSGHOME=C:\devEnv\VES.1.0-Deps\OSG_OP_OT-1.2\OpenSceneGraph
set PRODUCER_HOME=C:\devEnv\VES.1.0-Deps\OSG_OP_OT-1.2\Producer
set OPENTHREADS_HOME=C:\devEnv\VES.1.0-Deps\OSG_OP_OT-1.2\OpenThreads
set DEMETER_HOME=C:\devEnv\demeter-3.21
set APR_HOME=C:\devEnv\VES.1.0-Deps\apr
set APR_UTIL_HOME=C:\devEnv\VES.1.0-Deps\apr-util
set APR_ICONV_HOME=C:\devEnv\VES.1.0-Deps\apr-iconv
set COIN_HOME=C:\devEnv\VES.1.0-Deps\Coin-2.4.5
set TAO_MACHINE=localhost
REM set TAO_MACHINE=costello.vrac.iastate.edu
REM set TAO_MACHINE=keymaker.vrac.iastate.edu
set VEXMASTER=costello
set TAO_PORT=1239
set SNX_BASE_DIR=%VJ_BASE_DIR%

set OPENAL_LIB_DIR=C:\devEnv\VES.1.0-Deps\openal\OpenAL-Windows\OpenAL32\Debug
REM NOTE: On Windows the juggler location must point to a Windows
REM (local or on network) machine, otherwise user may run into runtime
REM problems when loading config files across the network

REM Everything below is relative to the above environment so user
REM shouldn't have to modify UNLESS "local" builds (debug for example)
REM are desired. In this case modify the appropriate variables accordingly.

set CFDHOSTTYPE=WIN32
set OSGNOTIFYLEVEL=DEBUG_INFO
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

set Path=%VJ_BASE_DIR%\lib;%VJ_DEPS_DIR%\bin;%VJ_DEPS_DIR%\lib;%Path%
set Path=%VE_SUITE_HOME%\bin;%VTK_HOME%\bin;%WX_HOME%\lib\vc_dll;%Path%
set Path=%ACE_ROOT%\bin;%ACE_ROOT%\lib;%XERCESCROOT%\Build\Win32\VC7.1\Debug;%Path%
set Path=%XERCESCROOT%\Build\Win32\VC7.1\Release;%Path%
set Path=%TAO_ROOT%\orbsvcs\Naming_Service;%Path%
set Path=%VE_SUITE_HOME%\bin\win32;%Path%
set Path=%OSGHOME%\bin\win32;%PRODUCER_HOME%\bin\win32;%OPENTHREADS_HOME%\bin\win32;%VE_SUITE_HOME%\lib\win32;%Path%
set Path=%DEMETER_HOME%\bin;%Path%
set Path=%COIN_HOME%\bin;%Path%
set Path=%APR_HOME%\Debug;%Path%
set Path=%APR_UTIL_HOME%\Debug;%Path%
set Path=%APR_ICONV_HOME%\Debug;%Path%
set Path=%APR_HOME%\Release;%Path%
set Path=%APR_UTIL_HOME%\Release;%Path%
set Path=%APR_ICONV_HOME%\Release;%Path%
set Path=%OPENAL_LIB_DIR%;%PATH%


call %VE_SUITE_HOME%\VE_Installer\defineSVNVersion.bat