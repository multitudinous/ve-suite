REM Edit this file to reflect your local environment for VE_Suite

set VE_SUITE_HOME=C:\devEnv\research
REM set VJ_BASE_DIR=C:\devEnv\VRAC\vrjuggler-2.0.1-win32-vc71
REM set VJ_DEPS_DIR=C:\devEnv\VRAC\vrjuggler-2.0.1-win32-vc71-deps
set VJ_BASE_DIR=C:\devEnv\VRAC\vrjuggler-2.0.1-win32-vc80
set VJ_DEPS_DIR=C:\devEnv\VRAC\vrjuggler-2.0.1-win32-vc80-deps
set VTK_HOME=C:\devEnv\externalAPIs\installs
set WX_HOME=C:\devEnv\externalAPIs\wxWidgets-2.6.3
set JAVA_HOME=C:\Progra~1\Java\j2re1.4.2_09
set ACE_ROOT=C:\devEnv\externalAPIs\ACE_wrappers
set XERCESCROOT=C:\devEnv\externalAPIs\xerces-c-src_2_7_0
set WX_ROOT=C:\devEnv\externalAPIs\wxWidgets-2.6.3
set OSGHOME=C:\devEnv\externalAPIs\OSG_cvs\OpenSceneGraph
set TAO_ROOT=C:\devEnv\externalAPIs\ACE_wrappers\TAO
set DEMETER_HOME=C:\devEnv\externalAPIs\demeter\demeter-3.21
set SAPI_HOME=C:\devEnv\externalAPIs\installs

REM set TAO_MACHINE=lego.vrac.iastate.edu
set APR_HOME=C:\devEnv\externalAPIs\apr
set APR_UTIL_HOME=C:\devEnv\externalAPIs\apr-util
set APR_ICONV_HOME=C:\devEnv\externalAPIs\apr-iconv
set TAO_MACHINE=localhost
set TAO_PORT=1237

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
set VPR_DEBUG_NFY_LEVEL=0
set NO_PERF_PLUGIN=TRUE
set NO_RTRC_PLUGIN=TRUE
set PFNFYLEVEL=0
REM set OSGNOTIFYLEVEL=DEBUG_INFO

REM Juggler dependencies
set JCCL_DEFINITION_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set VJ_CFG_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set NSPR_ROOT=%VJ_DEPS_DIR%

set OMNIORB_CONFIG=%VE_SUITE_HOME%\VE_Installer\omniORB4.cfg
set OMNINAMES_LOGDIR=%VE_SUITE_HOME%\VE_Installer

REM Python build environment variables
set PYTHONPATH=%VJ_DEPS_DIR%\lib\python

set Path=%ACE_ROOT%\lib;%Path%
set Path=%Path%;%ACE_ROOT%\bin;%ACE_ROOT%\lib;%XERCESCROOT%\Build\Win32\VC7.1\Debug
set Path=%Path%;%VJ_DEPS_DIR%\bin;%VJ_DEPS_DIR%\lib;%VJ_BASE_DIR%\lib
set Path=%Path%;%VE_SUITE_HOME%\bin;%VTK_HOME%\bin;%WX_HOME%\lib\vc_dll
set Path=%Path%;%VE_SUITE_HOME%\bin\Plugins\UI\win32;
set Path=%Path%;%VE_SUITE_HOME%\bin\Plugins\GE\win32;
set Path=%Path%;%ACE_ROOT%\bin;%ACE_ROOT%\lib;%XERCESCROOT%\Build\Win32\VC7.1\Debug
set Path=%Path%;%TAO_ROOT%\orbsvcs\Naming_Service
set Path=%Path%;%VE_SUITE_HOME%\bin\win32
set Path=%Path%;%OSGHOME%\bin;%VE_SUITE_HOME%\lib\win32
set Path=%Path%;%DEMETER_HOME%\bin;
set Path=%Path%;%APR_HOME%\Debug;
set Path=%Path%;%APR_UTIL_HOME%\Debug;
set Path=%Path%;%APR_ICONV_HOME%\Debug;
