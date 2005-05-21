REM edit this file to reflect your local environment for building
REM
REM location of VTK
set VTK_HOME=C:\VTK_Install

REM location of wxWidgets
set WX_HOME=C:\devEnv\externalAPIs\wxWidgets-2.6.0

set Path=%Path%;%WX_WIDGETS_HOME%\lib\vc_dll;%VTK_HOME%\bin;

REM only change this if your Visual Studio .NET is installed somewhere else
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv.exe" vectorViz.sln 
cmd

