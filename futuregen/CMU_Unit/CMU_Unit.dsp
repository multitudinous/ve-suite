# Microsoft Developer Studio Project File - Name="CMU_Unit" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=CMU_Unit - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "CMU_Unit.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "CMU_Unit.mak" CFG="CMU_Unit - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "CMU_Unit - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "CMU_Unit - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "CMU_Unit - Win32 Release"

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
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "CMU_Unit - Win32 Debug"

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
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "./" /I "../Plugin" /I "../IDL.old" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /FD /GZ /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 aced.lib TAOd.lib TAO_CosNamingd.lib TAO_PortableServerd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept /libpath:"M:\ACE_TAO\Windows-vrac\ACE_wrappers\ace" /libpath:"M:\ACE_TAO\Windows-vrac\ACE_wrappers\TAO\tao" /libpath:"M:\ACE_TAO\Windows-vrac\ACE_wrappers\TAO\orbsvcs\orbsvcs" /libpath:"M:\ACE_TAO\Windows-vrac\ACE_wrappers\TAO\tao\PortableServer" /libpath:"C:/wxWindows-2.4.2/lib"

!ENDIF 

# Begin Target

# Name "CMU_Unit - Win32 Release"
# Name "CMU_Unit - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\cmu_model.cpp
# End Source File
# Begin Source File

SOURCE=..\Plugin\interface.cpp
# End Source File
# Begin Source File

SOURCE=..\IDL.old\moduleC.cpp
# End Source File
# Begin Source File

SOURCE=..\IDL.old\moduleS.cpp
# End Source File
# Begin Source File

SOURCE=..\IDL.old\moduleS_T.cpp
# End Source File
# Begin Source File

SOURCE=..\Plugin\string_ops.cpp
# End Source File
# Begin Source File

SOURCE=.\Unit_client.cpp
# End Source File
# Begin Source File

SOURCE=.\Unit_i.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\CMU_Unit\asu_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\cmn_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\cmu_model.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\coal_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\configs.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\cseq_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\gts_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\igcc_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\init_io.h
# End Source File
# Begin Source File

SOURCE=..\Plugin\interface.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\link_io.h
# End Source File
# Begin Source File

SOURCE=..\WinClient\moduleC.h
# End Source File
# Begin Source File

SOURCE=..\WinClient\moduleS.h
# End Source File
# Begin Source File

SOURCE=..\WinClient\moduleS_T.h
# End Source File
# Begin Source File

SOURCE=..\Plugin\packable.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\prefixes.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\selx_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\srs_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\stk_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\string_ops.h
# End Source File
# Begin Source File

SOURCE=..\Plugin\string_ops.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\tex_io.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\Unit_i.h
# End Source File
# Begin Source File

SOURCE=..\CMU_Unit\wgsr_io.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=..\WinClient\moduleC.i
# End Source File
# Begin Source File

SOURCE=..\WinClient\moduleS.i
# End Source File
# Begin Source File

SOURCE=..\WinClient\moduleS_T.i
# End Source File
# End Target
# End Project
