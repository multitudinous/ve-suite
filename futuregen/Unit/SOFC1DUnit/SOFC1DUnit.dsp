# Microsoft Developer Studio Project File - Name="SOFC1DUnit" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=SOFC1DUnit - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "SOFC1DUnit.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "SOFC1DUnit.mak" CFG="SOFC1DUnit - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "SOFC1DUnit - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "SOFC1DUnit - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "SOFC1DUnit - Win32 Release"

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

!ELSEIF  "$(CFG)" == "SOFC1DUnit - Win32 Debug"

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
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MD /W3 /Gm /GX /ZI /Od /I "$(TAO_ROOT)" /I "$(ACE_ROOT)" /I "$(TAO_ROOT)\orbsvcs" /I "$(XERCESCROOT)\include" /I "..\\" /I "..\..\IDL" /I "..\V21Helper$(TAO_ROOT)" /I "..\V21Helper" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /D "_REENTRANT" /D "_STLP_USE_OWN_NAMESPACE" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 aced.lib TAOd.lib TAO_CosNamingd.lib TAO_PortableServerd.lib xerces-c_2.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /out:"../SOFC1DUnit.exe" /pdbtype:sept /libpath:"$(ACE_ROOT)\ace" /libpath:"$(TAO_ROOT)\tao" /libpath:"$(TAO_ROOT)\orbsvcs\orbsvcs" /libpath:"$(TAO_ROOT)\tao\PortableServer" /libpath:"$(XERCESCROOT)\lib"

!ENDIF 

# Begin Target

# Name "SOFC1DUnit - Win32 Release"
# Name "SOFC1DUnit - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\ThirdParty\Components\AnalyzeCurrentDensity.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\AnodeGasModel.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\CathodeGasModel.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\Electrolyte.cpp
# End Source File
# Begin Source File

SOURCE=.\FC_Process.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\FCMcontroller.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\Gas.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\GasCell.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\GasPhaseModel.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\globals.cpp
# End Source File
# Begin Source File

SOURCE=..\interface.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\matrix.cpp
# End Source File
# Begin Source File

SOURCE=..\..\IDL\moduleC.cpp
# End Source File
# Begin Source File

SOURCE=..\..\IDL\moduleS.cpp
# End Source File
# Begin Source File

SOURCE=..\..\IDL\moduleS_T.cpp
# End Source File
# Begin Source File

SOURCE=..\package.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\PlannerFuelCellModel.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\PlannerSolidOxideFuelCellModel.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\SeparatorPlate.cpp
# End Source File
# Begin Source File

SOURCE=.\SOFC1DUnit_client.cpp
# End Source File
# Begin Source File

SOURCE=.\SOFC1DUnit_i.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\SolidOxideAnodeGasModel.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\SolidOxideCathodeGasModel.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\SolidOxideElectrolyte.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\Sp_dbase.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Steam67\Steam67.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\stream.cpp
# End Source File
# Begin Source File

SOURCE=..\string_ops.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\SummaryValues\summary_values.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\thermo.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\SummaryValues\unit_conversion.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\V21Helper.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\Water.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\ThirdParty\Components\AnodeGasModel.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\CathodeGasModel.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\Electrolyte.h
# End Source File
# Begin Source File

SOURCE=.\FC_Process.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\FCMcontroller.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\Gas.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\GasCell.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\GasPhaseModel.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\globals.h
# End Source File
# Begin Source File

SOURCE=..\interface.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\matrix.h
# End Source File
# Begin Source File

SOURCE=..\..\IDL\moduleC.h
# End Source File
# Begin Source File

SOURCE=..\..\IDL\moduleS.h
# End Source File
# Begin Source File

SOURCE=..\..\IDL\moduleS_T.h
# End Source File
# Begin Source File

SOURCE=..\package.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\PlannerFuelCellModel.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\PlannerSolidOxideFuelCellModel.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\REAL.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\SeparatorPlate.h
# End Source File
# Begin Source File

SOURCE=.\SOFC1DUnit_i.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\SolidOxideAnodeGasModel.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\SolidOxideCathodeGasModel.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\SolidOxideElectrolyte.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\Sp_dbase.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Steam67\Steam67.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\stream.h
# End Source File
# Begin Source File

SOURCE=..\string_ops.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\SummaryValues\summary_values.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\thermo.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\SummaryValues\unit_conversion.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\V21Helper.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\Water.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
