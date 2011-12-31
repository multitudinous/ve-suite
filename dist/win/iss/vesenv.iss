; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#define VEVERSION "2.2.2"
#define SVNVERSION "16781"
#define x64 "1"
#ifdef x64
#define MSVCVERSION "msvc-9.0-sp1-x64"
#else
#define MSVCVERSION "msvc-9.0-sp1-x86"
#endif
; Define the group name for all the windows links to be
; installed under for the Start Menu
#define VESGROUPNAME "VE-Suite"
#ifdef x64
; Location where all of the ves dependencies are installed
#define DEPENDSINSTALLHOME "C:\dev\deps\msvc90\x64"
; Install prefix of ves
#define VEINSTALLHOME "C:\dev\TSVEG\VE_Suite\install-win64"
; Location if the Microsoft redistributable executable
#define MSREDISTRIBUTABLE "C:\dev\deps\vcredist_x64.exe"
#define MSREDISTRIBUTABLEFILENAME "vcredist_x64.exe"

; Location if the OPC executable
#define OPCVESINSTALLER "C:\dev\deps\OPC Core Components Redistributable (x64) 105\OPC Core Components Redistributable (x64).msi"
#define OPCVESINSTALLERFILENAME "OPC Core Components Redistributable (x64).msi"

#else
; Location where all of the ves dependencies are installed
#define DEPENDSINSTALLHOME "C:\dev\deps\msvc90\x86"
; Install prefix of ves
#define VEINSTALLHOME "C:\dev\TSVEG\VE_Suite\install-win32"
; Location if the Microsoft redistributable executable
#define MSREDISTRIBUTABLE "C:\dev\deps\vcredist_x86.exe"
#define MSREDISTRIBUTABLEFILENAME "vcredist_x86.exe"

; Location if the OPC executable
#define OPCVESINSTALLER "C:\dev\deps\OPC Core Components Redistributable (x86) 105\OPC Core Components Redistributable (x86).msi"
#define OPCVESINSTALLERFILENAME "OPC Core Components Redistributable (x86).msi"

#endif

; Location where you want innosetup to build the installers
#define INSTALLERINSTALLLOCATION "C:\dev\TSVEG\VE_Suite\dist\win\iss"
; Source root location for ves
#define VEDEVHOME "C:\dev\TSVEG\VE_Suite"

; Lib directory for x64 or x86
#ifdef x64
#define LIBDIR "lib64"
#define BUILDDIR "x64"
#define ALUTLIBDIR "x64"
#define DISTDIR "Win64"

; Directory names for all VES dependencies
#define ACETAOHOME "ACETAO_5.7.0_msvc-9.0-sp1-x64"
#define VTKHOME "VTK_5.8.0_msvc-9.0-sp1-x64"
#define OSGHOME "OSG_2.8.5_msvc-9.0-sp1-x64"
#define WXHOME "wxWidgets_2.8.10_msvc-9.0-sp1-x64"
#define XERCESHOME "Xerces_3.1.1_msvc-9.0-sp1-x64"
#define JUGGLERINSTHOME "VRJuggler_3.0.0_msvc-9.0-sp1-x64"
#define MINERVAHOME "Minerva_1.2.0-397_msvc-9.0-sp1-x64"
#define SKEWMATRIXHOME "C:\dev\ves_deps\skewmatrix"
#define OSGALHOME "osgAudio_2.0.0_msvc-9.0-sp1-x64"
#define POCOHOME "POCO_1.4.1_msvc-9.0-sp1-x64"
#define OSGWORKSHOME "osgWorks_1.1.52_msvc-9.0-sp1-x64"
#else
#define LIBDIR "lib"
#define BUILDDIR "Win32"
#define ALUTLIBDIR "x86"
#define DISTDIR "Win32"

; Directory names for all VES dependencies
#define ACETAOHOME "ACETAO_5.7.0_msvc-9.0-sp1-x86"
#define VTKHOME "VTK_5.8.0_msvc-9.0-sp1-x86"
#define OSGHOME "OSG_2.8.5_msvc-9.0-sp1-x86"
#define WXHOME "wxWidgets_2.8.11_msvc-9.0-sp1-x86"
#define XERCESHOME "Xerces_3.1.1_msvc-9.0-sp1-x86"
#define JUGGLERINSTHOME "VRJuggler_3.0.0_msvc-9.0-sp1-x86"
#define MINERVAHOME "MinervaBoost144_1.2.0-07242011_msvc-9.0-sp1-x86"
#define SKEWMATRIXHOME "C:\dev\ves_deps\skewmatrix"
#define OSGALHOME "osgAudio_2.0.0_msvc-9.0-sp1-x86"
#define POCOHOME "POCO_1.4.1_msvc-9.0-sp1-x86"
#define OSGWORKSHOME "osgWorks_1.1.52_msvc-9.0-sp1-x86"
#endif

; Source directories for compiling application specific installers
#define ACETAOSRCHOME "C:\Projects\ves-windows\ACE_wrappers"
;The directory below contains all each apr, apr-iconv, and apr-util
;The versioned directories have been copied to be easier to build
;and less version dependent when running this installer
#define APRINSTALL "C:\Projects\ves-windows\workapr"
; Minerva deps
#define MINERVASRCHOME "C:\dev\deps\auto_deps\minervagis_boost1440\install-32-bit"
; #define FWTOOLS "C:\Program Files\FWTools2.3.0"
#define PROJHOME "C:\dev\deps\proj-4.7.0\install-win32"
#define GDALHOME "C:\dev\deps\gdal-1.6.3\install-win32"
; #define CURL_HOME "C:\dev\ves_deps\curl-7.19.5\build-win32"
; #define OSG3RDPARTY "C:\dev\ves_deps\3rdParty\3rdParty_win32binaries_vs80sp1"
; osgAL deps
#define LIBOGGHOME "C:\dev\deps\libogg"
#define LIBVORBISHOME "C:\dev\deps\vorbis-svn"
#define OPENALHOME "C:\dev\deps\openalsdk"
#ifdef x64
#define OSGALSRCHOME "C:\dev\deps\osgaudio-gc-svn\install-win64"
#else
#define OSGALSRCHOME "C:\dev\deps\osgaudio-gc-svn\install-win32"
#endif
;#define OSGAUDIOROOTHOME "C:\dev\deps\osgaudio-gc-svn"
#define ALUTSRCINSTALL "C:\dev\deps\freealut-1.1.0-bin"
; OSG deps
#define OSGSRCHOME "C:\dev\deps\OpenSceneGraph-2.8-branch\install-win32"
; #define SIMAGEHOME "D:\devEnv\VES-Deps_1.1\prebuiltInstalls\simage-1.6.1"
#define COINHOME "C:\dev\deps\Coin3D"
#define OSG3RDPARTY "C:\dev\deps\3rdParty_Win32Binaries_vc90sp1\3rdParty"
; osgWorks deps
#define OSGWORKSINSTLOCATION "C:\dev\deps\osgworks-gc-svn\install-win32"
; POCO deps
#define POCOSRCHOME "C:\Projects\ves-windows\poco-1.3.5-all"
#define SQLITEHOME "C:\Projects\ves-windows\sqlitedll-3_6_20"
; VR Juggler deps
#ifdef x64
#define VRJUGGLER_INST_LOCATION "C:\dev\deps\vrjuggler-3.0-branch-test\vrjuggler-3.0-branch-test\install-test-x64"
#define VRJUGGLER_DEPS_INST_LOCATION "C:\dev\deps\vrjuggler-3.0-branch-test\vrjuggler-3.0-branch-test\install-deps-test-x64"
#else
#define VRJUGGLER_INST_LOCATION "C:\dev\deps\vrjuggler-3.0-branch-test\vrjuggler-3.0-branch-test\install-test"
#define VRJUGGLER_DEPS_INST_LOCATION "C:\dev\deps\vrjuggler-3.0-branch-test\vrjuggler-3.0-branch-test\install-deps-test"
#endif
#define TRACKDAPIHOME "C:\dev\deps\trackdAPI"
; VTK deps
#define VTKSRCHOME "C:\dev\ves_deps\vtk-5.2.0-install"
; WX deps
#define WXSRCINSTALL "C:\dev\deps\wxMSW-2.8.11"
; xerces deps
#define XERCESSRCINSTALL "C:\Projects\ves-windows\xerces-c-3.0.1-x86-windows-vc-9.0"
; Tecplot SDK installation
#define TECPLOTSDKHOME "C:\Program Files (x86)\Tecplot\TecSDK 1.1"
