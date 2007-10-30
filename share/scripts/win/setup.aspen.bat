REM Edit this file to reflect your local environment for VE_Suite

set VE_SUITE_HOME=C:\Work\VE_Suite\suite
set VE_INSTALL_DIR=C:\Work\VE_Suite\suite

set VJ_DEPS_DIR=C:\Work\VE_Suite\Dependencies\VR_Juggler-2.2.0
set VJ_BASE_DIR=C:\Work\VE_Suite\Dependencies\VR_Juggler-2.2.0

set VE_SUITE_INCLUDES=C:\usr\local\include
set VE_SUITE_LIBS=C:\usr\local\lib

set VTK_HOME=C:\Work\VE_Suite\Dependencies\VTK_Pre-Compile_5.1
set WX_HOME=C:\Work\VE_Suite\Dependencies\WxWidgets_2.8.4_Pre-Compile_vc8.0_SP1
set WX_ROOT=C:\Work\VE_Suite\Dependencies\WxWidgets_2.8.4_Pre-Compile_vc8.0_SP1
set JAVA_HOME=C:\j2sdk1.4.2_06
set ACE_ROOT=C:\Work\VE_Suite\Dependencies\ACETAO_5.5.10_Pre-Compile_vc8.0_SP1
set TAO_ROOT=%ACE_ROOT%\TAO
set XERCESCROOT=C:\Work\VE_Suite\Dependencies\Xerces-c_2.7_Pre-Compile
set BULLET_HOME=C:\Work\VE_Suite\Dependencies\Bullet_Pre-Compile_2.60
set OSGHOME=C:\Work\VE_Suite\Dependencies\OSG_1.2_Pre-Compile_vc8.0
set PRODUCER_HOME=C:\Work\VE_Suite\Dependencies\OSG_1.2_Pre-Compile_vc8.0
set OPENTHREADS_HOME=F:\OSG_1.2_Pre-Compile_vc8.0
REM set DEMETER_HOME=C:\devEnv\demeter-3.21
set APR_HOME=C:\Work\VE_Suite\Dependencies\apr-vc8\apr
set APR_UTIL_HOME=C:\Work\VE_Suite\Dependencies\apr-vc8\apr-util
set APR_ICONV_HOME=C:\Work\VE_Suite\Dependencies\apr-vc8\apr-iconv
REM set COIN_HOME=C:\devEnv\VES.1.0-Deps\Coin-2.4.5
set TAO_MACHINE=localhost
set VEXMASTER=costello
set TAO_PORT=1239
set SNX_BASE_DIR=%VJ_BASE_DIR%

REM set OPENAL_LIB_DIR=C:\devEnv\VES.1.0-Deps\openal\OpenAL-Windows\OpenAL32\Debug

set CFDHOSTTYPE=WIN32
set OSGNOTIFYLEVEL=DEBUG_INFO
set CONDUCTOR_BASE_DIR=%VE_SUITE_HOME%\VE_Conductor

set VPR_DEBUG_ENABLE=1
set VPR_DEBUG_NFY_LEVEL=1
set NO_PERF_PLUGIN=TRUE
set NO_RTRC_PLUGIN=TRUE
set PFNFYLEVEL=0
set OSG_THREAD_SAFE_REF_UNREF=1

set JCCL_DEFINITION_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set VJ_CFG_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set NSPR_ROOT=%VJ_DEPS_DIR%

set OMNIORB_CONFIG=%VE_SUITE_HOME%\VE_Installer\omniORB4.cfg
set OMNINAMES_LOGDIR=%VE_SUITE_HOME%\VE_Installer

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
set Path=C:\Python25;%PATH%

set FLAGPOLL_Path=C:\Work\VE_Suite\Dependencies\win_deps_fpc_files\debug;C:\Work\VE_Suite\Dependencies\VR_Juggler-2.2.0\lib\flagpoll
call %VE_SUITE_HOME%\VE_Installer\defineSVNVersion.bat