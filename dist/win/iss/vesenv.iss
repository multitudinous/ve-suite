; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#define VEVERSION "3.0.0"
#define SVNVERSION "14920"
#define x64 "1"
; Lib directory for x64 or x86
#ifdef x64
#define LIBDIR "lib64"
#define BUILDDIR "x64"
#define DISTDIR "Win64"
#define OGGBUILDDIR "x64"
#define MSVCVERSION "msvc-9.0-sp1-x64"
#else
#define LIBDIR "lib"
#define BUILDDIR "x86"
#define DISTDIR "Win32"
#define OGGBUILDDIR "Win32"
#define MSVCVERSION "msvc-9.0-sp1-x86"
#endif
; Define the group name for all the windows links to be
; installed under for the Start Menu
#define VESGROUPNAME "VE-Suite"
; Location where all of the ves dependencies are installed
#define DEPENDSINSTALLHOME "C:\dev\deps\msvc90"
; Location where you want innosetup to build the installers
#define INSTALLERINSTALLLOCATION "C:\dev\ve-suite-trunk\dist\win\iss"
; Install prefix of ves
#define VEINSTALLHOME "C:\dev\ve-suite-trunk\install-win32-qt"
; Source root location for ves
#define VEDEVHOME "C:\dev\ve-suite-trunk"
; Location if the Microsoft redistributable executable
#define MSREDISTRIBUTABLE "C:\dev\ves_deps\vcredist_x86.exe"
#define MSREDISTRIBUTABLEFILENAME "vcredist_x86.exe"
; Location if the OPC executable
#define OPCVESINSTALLER "C:\dev\ves_deps\OPC_2.02.exe"
#define OPCVESINSTALLERFILENAME "OPC_2.02.exe"

; Directory names for all VES dependencies
#define ACETAOHOME "ACETAO_5.7.0_Pre-Compile_vcmsvc-9.0-sp1-x86"
#define VTKHOME "VTK_5.8.0_msvc-9.0-sp1-x86"
#define OSGHOME "OSG_2.8.5_msvc-9.0-sp1-x86"
#define WXHOME "WXWidgets_2.8.10_msvc-9.0-sp1-x86"
#define XERCESHOME "Xerces_3.0.1_msvc-9.0-sp1-x86"
#define JUGGLERINSTHOME "VRJuggler_3.0.0_msvc-9.0-sp1-x86"
#define MINERVAHOME "Minerva_1.2.0-346_msvc-9.0-sp1-x86"
#define SKEWMATRIXHOME "C:\dev\ves_deps\skewmatrix"
#define OSGALHOME "osgAudio_2.0.0_msvc-9.0-sp1-x86"
#define POCOHOME "POCO_1.3.5_msvc-9.0-sp1-x86"
#define OSGWORKSHOME "osgWorks_1.1.1_msvc-9.0-sp1-x86"

; Source directories for compiling application specific installers
#define VESAUTODEPSDIR "C:\dev\deps\auto_deps"
#ifdef x64
#define VESINSTALLDIR "install-64-bit"
#else
#define VESINSTALLDIR "install-32-bit"
#endif
#define BDFXDIRNAME "bdfx"
#define BULLETDIRNAME "bullet-2.80"
#define PNGDIRNAME "libpng-1.5.2"
#define OSGDIRNAME "osg_2.8.5"
#define OSGBULLETDIRNAME "osgBullet"
#define OSGBULLETPLUSDIRNAME "osgBulletPlus"
#define OSGEPHEMERISDIRNAME "osgEphemeris"
#define OSGWORKSDIRNAME "osgWorks"
#define ZLIBDIRNAME "zlib-1.2.5"
#define VTKDIRNAME "VTK-5.8"
#define COIN3DDIRNAME "Coin-3.1.3"
#ifdef x64
#define ACETAODIRNAME "ACE_wrappers_64-bit"
#define XERCESCDIRNAME "xerces-c-3.1.1-x86_64-windows-vc-9.0"
#else
#define ACETAODIRNAME "ACE_wrappers_32-bit"
#define XERCESCDIRNAME "xerces-c-3.1.1-x86-windows-vc-9.0"
#endif
#define POCODIRNAME "poco"
#define VRJUGGLERDIRNAME "vrjuggler-trunk"




; ACE deps
#define ACETAOSRCHOME "C:\Projects\ves-windows\ACE_wrappers"
;The directory below contains all each apr, apr-iconv, and apr-util
;The versioned directories have been copied to be easier to build
;and less version dependent when running this installer
#define APRINSTALL "C:\Projects\ves-windows\workapr"
; Minerva deps
#define MINERVASRCHOME "C:\dev\deps\auto_deps\minervagis\install-win32"
; #define FWTOOLS "C:\Program Files\FWTools2.3.0"
#define PROJHOME "C:\dev\deps\proj-4.7.0\install-win32"
#define GDALHOME "C:\dev\deps\gdal-1.6.3\install-win32"
; #define CURL_HOME "C:\dev\ves_deps\curl-7.19.5\build-win32"
; #define OSG3RDPARTY "C:\dev\ves_deps\3rdParty\3rdParty_win32binaries_vs80sp1"
; osgAL deps
#define LIBOGGHOME "C:\dev\deps\libogg"
#define LIBVORBISHOME "C:\dev\deps\vorbis-svn"
#define OPENALHOME "C:\Program Files (x86)\OpenAL 1.1 SDK"
#ifdef x64
#define OSGALSRCHOME "C:\dev\deps\osgaudio-gc-svn\install-win64"
#else
#define OSGALSRCHOME "C:\dev\deps\osgaudio-gc-svn\install-win32"
#endif
#define OSGAUDIOROOTHOME "C:\dev\deps\osgaudio-gc-svn"
#define ALUTSRCINSTALL "C:\dev\deps\freealut-1.1.0-bin"
; OSG deps
#define OSGSRCHOME "C:\dev\deps\OpenSceneGraph-2.8-branch\install-win32"
; #define SIMAGEHOME "D:\devEnv\VES-Deps_1.1\prebuiltInstalls\simage-1.6.1"
;#define COINHOME "C:\dev\deps\3rdParty_Win32Binaries_vc90sp1\3rdParty"
;#define OSG3RDPARTY "C:\dev\deps\3rdParty_Win32Binaries_vc90sp1\3rdParty"
; osgWorks deps
#define OSGWORKSINSTLOCATION "C:\dev\deps\osgworks-gc-svn\install-win32"
; POCO deps
#define POCOSRCHOME "C:\Projects\ves-windows\poco-1.3.5-all"
#define SQLITEHOME "C:\Projects\ves-windows\sqlitedll-3_6_20"
; VR Juggler deps
#define VRJUGGLER_INST_LOCATION "C:\dev\deps\juggler-gc-svn\install-win32"
#define VRJUGGLER_DEPS_INST_LOCATION "C:\dev\deps\juggler-gc-svn\deps-install-win32"
#define TRACKDAPIHOME "C:\dev\deps\trackdAPI"
; VTK deps
#define VTKSRCHOME "C:\dev\ves_deps\vtk-5.2.0-install"
; xerces deps
#define XERCESSRCINSTALL "C:\Projects\ves-windows\xerces-c-3.0.1-x86-windows-vc-9.0"
; Tecplot SDK installation
#define TECPLOTSDKHOME "C:\dev\deps\tecplot_sdk"
; Bullet deps
#define BULLETSRCHOME "C:\dev\deps\bullet-2.77\install-win32"
; osgBullet deps
#define OSGBULLETSRCHOME "C:\dev\deps\osgbullet-gc-svn\install-win32"
; osgBulletPlus deps
#define OSGBULLETPLUSSRCHOME "C:\dev\skewmatrix-redmine-svn\skewmatrix\osgBulletPlus\trunk\install-win32"
; backdropFX deps
#define BACKDROPFXSRCHOME "C:\dev\skewmatrix-redmine-svn\skewmatrix\backdropFX\trunk\install-win32"
; osgEphemeris
#define OSGEPHEMERISSRCHOME "C:\dev\skewmatrix-redmine-svn\skewmatrix\external\osgEphemeris\install-win32"
; LEmon deps
#define LEMONINSTALLHOME "C:\Users\mccdo\Desktop\lemon_test_build\lemon-1.2.1\install-win32"
#define CBCSRCHOME "C:\Users\mccdo\Desktop\lemon_test_build\Cbc-2.7.1-win32-msvc9"
#define GLPKSRCHOME "C:\Users\mccdo\Desktop\lemon_test_build\glpk-4.45"
