# Microsoft Developer Studio Project File - Name="WinClient" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=WinClient - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "WinClient.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "WinClient.mak" CFG="WinClient - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "WinClient - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "WinClient - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "WinClient - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /compile_only /nologo /warn:nofileopt /winapp
# ADD F90 /compile_only /nologo /warn:nofileopt /winapp
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
F90=df.exe
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt /winapp
# ADD F90 /browser /check:bounds /compile_only /dbglibs /debug:full /libs:dll /nologo /reentrancy:threaded /threads /traceback /warn:argument_checking /warn:nofileopt /winapp
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /w /W0 /Gm /GX /ZI /Od /I "../Plugin" /I "./" /I "../VE_UI/" /I "C:\wxWindows-2.4.2\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D WINVER=0x400 /D "_MT" /D wxUSE_GUI=1 /D "__WXDEBUG__" /D WXDEBUG=1 /D "WXUSINGDLL" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 aced.lib TAOd.lib TAO_CosNamingd.lib TAO_PortableServerd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib wxmsw24d.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept /libpath:"M:\ACE_TAO\Windows-vrac\ACE_wrappers\ace" /libpath:"M:\ACE_TAO\Windows-vrac\ACE_wrappers\TAO\tao" /libpath:"M:\ACE_TAO\Windows-vrac\ACE_wrappers\TAO\orbsvcs\orbsvcs" /libpath:"M:\ACE_TAO\Windows-vrac\ACE_wrappers\TAO\tao\PortableServer" /libpath:"C:/wxWindows-2.4.2/lib"

!ENDIF 

# Begin Target

# Name "WinClient - Win32 Release"
# Name "WinClient - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\App.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Avail_Modules.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Frame.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\GlobalParamDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\Plugin\interface.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\ListTable.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\IDL.old\moduleC.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\IDL.old\moduleS.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\IDL.old\moduleS_T.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Network.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Plugin\Plugin_base.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\PluginLoader.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\PortDialog.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\ResultPanel.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Plugin\string_ops.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\StringParse.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_DataSetTab.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_GeometryTab.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Framework\UI_i.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_NavTab.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_ScalarTab.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_SoundsTab.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_StreamTab.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_Tabs.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_TeacherTab.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_VecTab.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_VisTab.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Plugin\UIDialog.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\VjObsC.cpp

!IF  "$(CFG)" == "WinClient - Win32 Release"

!ELSEIF  "$(CFG)" == "WinClient - Win32 Debug"

# ADD CPP /I "..\IDL.old"

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\Framework\App.h
# End Source File
# Begin Source File

SOURCE=..\Framework\Avail_Modules.h
# End Source File
# Begin Source File

SOURCE=..\Framework\Frame.h
# End Source File
# Begin Source File

SOURCE=.\GlobalParamDialog.h
# End Source File
# Begin Source File

SOURCE=..\Plugin\interface.h
# End Source File
# Begin Source File

SOURCE=..\Framework\ListTable.h
# End Source File
# Begin Source File

SOURCE=.\moduleC.h
# End Source File
# Begin Source File

SOURCE=.\moduleS.h
# End Source File
# Begin Source File

SOURCE=.\moduleS_T.h
# End Source File
# Begin Source File

SOURCE=..\Framework\Network.h
# End Source File
# Begin Source File

SOURCE=..\Plugin\packable.h
# End Source File
# Begin Source File

SOURCE=..\Plugin\Plugin_base.h
# End Source File
# Begin Source File

SOURCE=..\Framework\PluginLoader.h
# End Source File
# Begin Source File

SOURCE=..\Framework\PortDialog.h
# End Source File
# Begin Source File

SOURCE=..\Framework\ResultPanel.h
# End Source File
# Begin Source File

SOURCE=..\Plugin\string_ops.h
# End Source File
# Begin Source File

SOURCE=..\Framework\StringParse.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_DataSetTab.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_GeometryTab.h
# End Source File
# Begin Source File

SOURCE=..\Framework\UI_i.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_NavTab.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_ScalarTab.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_SoundsTab.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_StreamTab.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_Tabs.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_TeacherTab.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_VecTab.h
# End Source File
# Begin Source File

SOURCE=..\VE_UI\UI_VisTab.h
# End Source File
# Begin Source File

SOURCE=..\Plugin\UIDialog.h
# End Source File
# Begin Source File

SOURCE=.\VjObsC.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
