REM edit this file to reflect your local environment for building
REM VE_Suite
REM
REM After editting, just double click the file and build the
REM appropriate configuration(Release_OMNI,Debug_OMNI)
REM That's it!!!!(For now)
REM 
REM
REM Location of juggler install
REM NOTE: On Windows this location must point to a Windows
REM (local or on network) machine, otherwise
REM user may run into runtime problems when loading config files across
REM the network

set VJ_BASE_DIR=C:\vrjuggler
set VJ_DEPS_DIR=C:\vrjuggler-2.0-alpha4.win32-vc71-deps

REM location of VE_Suite directory
REM example C:\VE_Dev\VE_Suite
set VE_SUITE_HOME=C:\VE_Suite

REM location of VTK
set VTK_HOME=C:\VTK

REM
REM Everything below is relative to the above environment so
REM user shouldn't have to modify UNLESS "local" builds(debug for 
REM example) are desired. In this case modify the appropriate variables
REM accordingly
REM 

REM Conductor directory--shouldn't be modified!!
set CONDUCTOR_BASE_DIR=%VE_SUITE_HOME%\VE_Conductor

REM OpenGl performer variable
set PHSHAREDSIZE=534773700

REM juggler debug output level
set VPR_DEBUG_NFY_LEVEL=2

REM Juggler dependencies
set JCCL_DEFINITION_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set VJ_CFG_PATH=%VJ_BASE_DIR%\share\vrjuggler\data\definitions
set NSPR_ROOT=%VJ_DEPS_DIR%

REM OmniORB build environment varibles
set OMNI_HOME=C:\omniORB-4.0.3
set OMNIORB_CONFIG=%VE_SUITE_HOME%\VE_Installer\omniORB4.cfg
set OMNINAMES_LOGDIR=%VE_SUITE_HOME%\VE_Installer

REM Python build environment variables
set PYTHONPATH=%VJ_DEPS_DIR%\lib\python
set Path=%Path%;%OMNI_HOME%\bin\x86_win32;%VJ_DEPS_DIR%\bin;%VJ_DEPS_DIR%\lib;%VJ_BASE_DIR%\lib;%VTK_HOME%\bin\debug;%VE_SUITE_HOME%\bin

REM only change this if your Visual Studio .NET is installed somewhere else
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv.exe" 
