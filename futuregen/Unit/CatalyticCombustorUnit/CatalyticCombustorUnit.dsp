# Microsoft Developer Studio Project File - Name="CatalyticCombustorUnit" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=CatalyticCombustorUnit - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "CatalyticCombustorUnit.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "CatalyticCombustorUnit.mak" CFG="CatalyticCombustorUnit - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "CatalyticCombustorUnit - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "CatalyticCombustorUnit - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "CatalyticCombustorUnit - Win32 Release"

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

!ELSEIF  "$(CFG)" == "CatalyticCombustorUnit - Win32 Debug"

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
# ADD F90 /browser /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MD /W3 /Gm /GX /ZI /Od /I "$(TAO_ROOT)" /I "$(ACE_ROOT)" /I "$(TAO_ROOT)\orbsvcs" /I "$(XERCESCROOT)\include" /I "..\\" /I "..\..\IDL" /I "..\V21Helper" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /D "_REENTRANT" /D "_STLP_USE_OWN_NAMESPACE" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 aced.lib TAOd.lib TAO_CosNamingd.lib TAO_PortableServerd.lib xerces-c_2.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /out:"../CatalyticCombustorUnit.exe" /pdbtype:sept /libpath:"$(ACE_ROOT)\ace" /libpath:"$(TAO_ROOT)\tao" /libpath:"$(TAO_ROOT)\orbsvcs\orbsvcs" /libpath:"$(TAO_ROOT)\tao\PortableServer" /libpath:"$(XERCESCROOT)\lib"

!ENDIF 

# Begin Target

# Name "CatalyticCombustorUnit - Win32 Release"
# Name "CatalyticCombustorUnit - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\ThirdParty\Cvode\band.c
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\cat_comb.cpp
# End Source File
# Begin Source File

SOURCE=.\CatalyticCombustorUnit_client.cpp
# End Source File
# Begin Source File

SOURCE=.\CatalyticCombustorUnit_i.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvband.c
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvbandpre.c
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvdense.c
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvdiag.c
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvode.c
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvspgmr.c
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\dense.c
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\ElementReader.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\expl_dll.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\Gas.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\GasCell.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\InpReader.cpp
# End Source File
# Begin Source File

SOURCE=..\interface.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\iterativ.c
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\llnlmath.c
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\matrix.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\MixedThermoReader.cpp
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

SOURCE=..\ThirdParty\Cvode\nvector.c
# End Source File
# Begin Source File

SOURCE=..\package.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\Reaction.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\reactionObject.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\ReactionReader.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\reks.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\reks_container.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\reks_solve.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\REKS_Thrm_Info.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\SpecieReader.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\SpecieThermo.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\spgmr.c
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

SOURCE=..\ThirdParty\Reks\StringParseUtil.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\SummaryValues\summary_values.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\surf_parser.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\surface_chem.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\thermo.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\thermo_db.cpp
# End Source File
# Begin Source File

SOURCE=..\V21Helper\SummaryValues\unit_conversion.cpp
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\usr_gas.cpp
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

SOURCE=..\ThirdParty\Cvode\band.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Components\cat_comb.h
# End Source File
# Begin Source File

SOURCE=.\CatalyticCombustorUnit_i.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvband.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvbandpre.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvdense.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvdiag.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvode.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\cvspgmr.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\dense.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\Element.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\ElementReader.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\expl_dll.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\Gas.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Datatypes\GasCell.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\GlobalConst.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\InpReader.h
# End Source File
# Begin Source File

SOURCE=..\interface.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\iterativ.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\llnlmath.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\llnltyps.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\matrix.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\MixedThermoReader.h
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

SOURCE=..\ThirdParty\Cvode\nvector.h
# End Source File
# Begin Source File

SOURCE=..\package.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\Reaction.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\reactionObject.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\ReactionReader.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\REAL.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\reks.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\reks_container.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\reks_solve.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\REKS_Thrm_Info.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\Specie.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\SpecieReader.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\SpecieThermo.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Cvode\spgmr.h
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

SOURCE=..\ThirdParty\Reks\StringParseUtil.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\SummaryValues\summary_values.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\surf_parser.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\surface_chem.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\Therm\thermo.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\thermo_db.h
# End Source File
# Begin Source File

SOURCE=..\V21Helper\SummaryValues\unit_conversion.h
# End Source File
# Begin Source File

SOURCE=..\ThirdParty\Reks\usr_gas.h
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
