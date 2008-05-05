REM Edit this file to reflect your local environment for VE_Suite

set VE_SUITE_HOME=C:\VE-Suite_1.1.7
set VE_INSTALL_DIR=C:\VE_Suite_Head

set VE_DEPS_DIR=C:\VE_Suite.1.1_Dependencies_1.1.7

set VJ_DEPS_DIR=C:\VRJuggler2.2.0
set VJ_BASE_DIR=C:\VRJuggler2.2.0

set VTK_HOME=C:\VTK_Pre-Compile_5.1

set WX_HOME=C:\WxWidgets_2.8.7_Pre-Compile_vc8.0_SP1
set WX_ROOT=C:\WxWidgets_2.8.7_Pre-Compile_vc8.0_SP1

set JAVA_HOME=C:\j2sdk1.4.2_06
set ACE_ROOT=C:\ACETAO_5.5_Pre-Compile_vc8.0_SP1
set TAO_ROOT=%ACE_ROOT%\TAO

set XERCESCROOT=C:\Xerces-c_2.7_Pre-Compile

set BULLET_HOME=C:\Bullet_Pre-Compile_2.64

set LOKI_HOME=%VE_SUITE_HOME%\external\loki-0.1.6

set OSGHOME=C:\OSG_2.2.0_Pre-Compile_vc8.0_SP1
set PRODUCER_HOME=%OSGHOME%
set OPENTHREADS_HOME=%OSGHOME%

set OPENGL_HOME=C:\OGLSDK

set APR_HOME=C:\apr-vc8\apr
set APR_UTIL_HOME=C:\apr-vc8\apr-util
set APR_ICONV_HOME=C:\apr-vc8\apr-iconv
REM set TAO_MACHINE=localhost
REM set VEXMASTER=costello
REM set TAO_PORT=1239

set OPENAL_LIB_DIR=C:\osgAL_0.6.1_Pre-Compile_vc8.0_SP1
REM NOTE: On Windows the juggler location must point to a Windows
REM (local or on network) machine, otherwise user may run into runtime
REM problems when loading config files across the network

REM Everything below is relative to the above environment so user
REM shouldn't have to modify UNLESS "local" builds (debug for example)
REM are desired. In this case modify the appropriate variables accordingly.

set CFDHOSTTYPE=WIN32

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
 set PYTHONPATH=%VE_SUITE_HOME%/VE_Installer/installer/python
set Path=%VJ_BASE_DIR%\lib;%Path%
set Path=%VJ_BASE_DIR%\bin;%Path%
set Path=%VE_SUITE_HOME%\bin\win32;%VE_SUITE_HOME%\bin\Plugins\UI\win32;%VTK_HOME%\bin;%WX_HOME%\lib\vc_dll;%Path%
set Path=%ACE_ROOT%\bin;%ACE_ROOT%\lib;%XERCESCROOT%\Build\Win32\VC7.1\Debug;%Path%
set Path=%XERCESCROOT%\Build\Win32\VC7.1\Debug;%Path%
set Path=%TAO_ROOT%\orbsvcs\Naming_Service;%Path%
set Path=%OSGHOME%\bin\win32;%VE_SUITE_HOME%\lib\win32;%Path%
set Path=%APR_HOME%\Debug;%Path%
set Path=%APR_UTIL_HOME%\Debug;%Path%
set Path=%APR_ICONV_HOME%\Debug;%Path%
set Path=%VE_SUITE_HOME%\install\release\bin;%Path%
set Path=%VE_SUITE_HOME%\install\release\lib;%Path%
set Path=%OSGHOME%\bin;%Path%

set Path=%APR_HOME%\Debug;%Path%
set Path=%APR_UTIL_HOME%\Debug;%Path%
set Path=%APR_ICONV_HOME%\Debug;%Path%
set Path=%OPENAL_LIB_DIR%\bin;%PATH%


call %VE_SUITE_HOME%\VE_Installer\defineSVNVersion.bat
