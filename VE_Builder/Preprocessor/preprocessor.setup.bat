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
REM Also, path names(VE_SUITE_HOME is OK) SHOULD NOT contain spaces!!!!!!!!

REM location of VE_Suite directory
REM example C:\VE_Dev\VE_Suite
set VE_SUITE_HOME=C:\VE_Suite

REM location of VTK
set VTK_HOME=C:\usr\local
set Path=%Path%;%VTK_HOME%\bin;%VE_SUITE_HOME%\bin;

REM only change this if your Visual Studio .NET is installed somewhere else
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv.exe" Preprocessor.sln
cmd
