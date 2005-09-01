@ECHO OFF
REM Edit this file to reflect your local environment for VE_Suite

REM where VE-Suite installed
set VE_INSTALL_DIR="C:\VE_Suite.0.9.0"

REM the directory containing datasets
set VE_WORKING_DIR=%VE_INSTALL_DIR%\exampleDatasets

REM the directory containing user defined plugins
set VE_USER_PLUGIN_DIR=.\

REM VE-Suite pre-complied dependencies
set VE_DEPS_DIR=C:\VE_Suite.0.9.0_Dependencies

REM vrJuggler  
REM These are setup for using VE-Suite dependency install's location
REM change only if you are using your own build
set VJ_BASE_DIR=%VE_DEPS_DIR%\vrjuggler2.0.1
set VJ_DEPS_DIR=%VE_DEPS_DIR%\vrjuggler2.0.1

REM used for cluster apps
REM set VEXMASTER=localhost

REM these can be used to point to user built dependencies
REM set OSGHOME=C:\OpenSceneGraph
REM set VTK_HOME=C:\VTK_Install
REM set WX_HOME=C:\devEnv\externalAPIs\wxWidgets-2.6.0
REM set ACE_ROOT=C:\ACE_wrappers
REM set XERCESCROOT=C:\devEnv\externalAPIs\xerces-c-src_2_6_0
REM set WX_ROOT=C:\devEnv\externalAPIs\wxWidgets-2.6.0
REM set TAO_ROOT=C:\ACE_wrappers\TAO

set TAO_MACHINE=localhost
set TAO_PORT=1237

REM NOTE: On Windows the juggler location must point to a Windows
REM (local or on network) machine, otherwise user may run into runtime
REM problems when loading config files across the network

REM Everything below is relative to the above environment so user
REM shouldn't have to modify UNLESS "local" builds (debug for example)
REM are desired. In this case modify the appropriate variables accordingly.

set CFDHOSTTYPE=WIN32

REM set PHSHAREDSIZE=534773700

REM juggler debug output level
set VPR_DEBUG_ENABLE=0
set VPR_DEBUG_NFY_LEVEL=1
set NO_PERF_PLUGIN=TRUE
set NO_RTRC_PLUGIN=TRUE
set PFNFYLEVEL=0
REM set OSGNOTIFYLEVEL=DEBUG_INFO

REM Juggler dependencies
REM These are currently set relative to VE-Suite's install
REM if the user downloads there own juggler, these paths will have to be modified
set JCCL_BASE_DIR=%VJ_BASE_DIR%
set JCCL_DEFINITION_PATH=%VJ_BASE_DIR%\definitions
set VJ_CFG_PATH=%VJ_BASE_DIR%\definitions
set NSPR_ROOT=%VJ_DEPS_DIR%
set SNX_BASE_DIR=%VJ_BASE_DIR%

REM Python build environment variables
set PYTHONPATH=%VJ_DEPS_DIR%\lib\python

set Path=%Path%;%VJ_DEPS_DIR%\bin;%VJ_DEPS_DIR%\lib;%VJ_BASE_DIR%\lib
REM set Path=%Path%;%VTK_HOME%\bin;%WX_HOME%\lib\vc_dll
REM set Path=%Path%;%ACE_ROOT%\bin;%XERCESCROOT%\Build\Win32\VC7\Debug
REM set Path=%Path%;%OSGHOME%\bin
set Path=%Path%;%VE_INSTALL_DIR%\bin
set Path=%Path%;%VE_DEPS_DIR%\bin
set Path=%Path%;%CD%\bin
