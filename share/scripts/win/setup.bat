REM Edit this file to reflect your local environment for VE_Suite
set FLAGPOLL_PATH=C:\dev\ves_deps\fpc_deps_files\release

set VE_SUITE_HOME=C:\dev\VE_Suite
set VE_INSTALL_DIR=C:\dev\VE_Suite\install-release

set VJ_BASE_DIR=C:\dev\ves_deps\VRJuggler_Pre-Compile_2.2_SP1
set VTK_HOME=C:\dev\ves_deps\vtk-5.2.0-install
set WX_HOME=C:\dev\ves_deps\WxWidgets_2.8.7_Pre-Compile_vc8.0_SP1
set ACE_ROOT=C:\dev\ves_deps\ACETAO_5.6.6_Pre-Compile_vc8.0_SP1
set TAO_ROOT=%ACE_ROOT%\TAO
set XERCESCROOT=C:\dev\ves_deps\Xerces-c_2.7_Pre-Compile
set BULLET_HOME=C:\dev\ves_deps\Bullet_Pre-Compile_2.64
set OSGHOME=C:\dev\ves_deps\OpenSceneGraph-2.6.1\install-win32
set APR_HOME=C:\dev\ves_deps\apr-vc8\apr
set APR_UTIL_HOME=C:\dev\ves_deps\apr-vc8\apr-util
set APR_ICONV_HOME=C:\dev\ves_deps\apr-vc8\apr-iconv

set OPENAL_LIB_DIR=C:\devEnv\VES.1.0-Deps\openal\OpenAL-Windows\OpenAL32\Debug
REM NOTE: On Windows the juggler location must point to a Windows
REM (local or on network) machine, otherwise user may run into runtime
REM problems when loading config files across the network

REM Everything below is relative to the above environment so user
REM shouldn't have to modify UNLESS "local" builds (debug for example)
REM are desired. In this case modify the appropriate variables accordingly.

set CFDHOSTTYPE=WIN32
set OSGNOTIFYLEVEL=DEBUG_INFO

REM set PHSHAREDSIZE=534773700

REM juggler debug output level
set VPR_DEBUG_ENABLE=1
set VPR_DEBUG_NFY_LEVEL=1
set NO_PERF_PLUGIN=TRUE
set NO_RTRC_PLUGIN=TRUE
set PFNFYLEVEL=0
set OSG_THREAD_SAFE_REF_UNREF=1


set Path=%VJ_BASE_DIR%\lib;%Path%
set Path=%VE_INSTALL_DIR%\bin;%VTK_HOME%\bin;%WX_HOME%\lib\vc_dll;%Path%
set Path=%ACE_ROOT%\bin;%ACE_ROOT%\lib;%XERCESCROOT%\Build\Win32\VC7.1\Debug;%Path%
set Path=%XERCESCROOT%\Build\Win32\VC7.1\Release;%Path%
set Path=%TAO_ROOT%\orbsvcs\Naming_Service;%Path%
set Path=%VE_SUITE_HOME%\bin\win32;%Path%
set Path=%OSGHOME%\bin;%VE_INSTALL_DIR%\lib;%Path%
set Path=%APR_HOME%\Release;%Path%
set Path=%APR_UTIL_HOME%\Release;%Path%
set Path=%APR_ICONV_HOME%\Release;%Path%
set Path=%Path%;C:\dev\ves_deps\svn-win32-1.5.0\bin
set Path=%Path%;C:\dev\VE_Suite\external\FreezePython\upx301w

call %VE_SUITE_HOME%\share\scripts\win\defineSVNVersion.bat