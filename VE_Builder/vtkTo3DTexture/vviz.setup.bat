REM edit this file to reflect your local environment for building
REM
REM location of VTK
set VTK_HOME=E:\VTK_Install

REM location of wxWidgets
set WX_WIDGETS_HOME=E:\wxWindows-2.4.2

set Path=%Path%;%WX_WIDGETS_HOME%\bin;%VTK_HOME%\bin;

REM only change this if your Visual Studio .NET is installed somewhere else
"C:\Program Files\Microsoft Visual Studio .NET 2003\Common7\IDE\devenv.exe" vectorViz.sln 
cmd

