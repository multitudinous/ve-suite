; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#define VEVERSION "2.1.0"
#define SVNVERSION "14040"
#define MSVCVERSION "msvc-9.0-sp1-x86"
; Define the group name for all the windows links to be
; installed under for the Start Menu
#define VESGROUPNAME "VE-Suite"
; Location where all of the ves dependencies are installed
#define DEPENDSINSTALLHOME "C:\dev\ves_deps"
; Location where you want innosetup to build the installers
#define INSTALLERINSTALLLOCATION "C:\dev\TSVEG\VE_Suite\dist\win\iss"
; Install prefix of ves
#define VEINSTALLHOME "C:\dev\TSVEG\VE_Suite\install-win32"
; Source root location for ves
#define VEDEVHOME "C:\dev\TSVEG\VE_Suite"
; Location if the Microsoft redistributable executable
#define MSREDISTRIBUTABLE "C:\dev\ves_deps\vcredist_x86.exe"
; Directory names for all VES dependencies
#define ACETAOHOME "ACETAO_5.6.8_Pre-Compile_vc8.0_SP1"
#define VTKHOME "VTK_5.2.0_msvc-8.0-sp1-x86"
#define OSGHOME "OSG_2.8.2_msvc-8.0-sp1-x86"
#define WXHOME "WxWidgets_2.8.9_Pre-Compile_vc8.0_SP1"
#define XERCESHOME "Xerces-c_2.8_Pre-Compile"
#define JUGGLERINSTHOME "VRJuggler_2.3.18-21210_msvc-8.0-sp1-x86"
#define APRHOME "apr_1.3_Pre-Compile"
#define APRUTILHOME "apr_1.3_Pre-Compile"
#define APRICONVHOME "apr_1.3_Pre-Compile"
#define MINERVAHOME "Minerva_112_msvc-8.0-sp1-x86"
#define SKEWMATRIXHOME "C:\dev\ves_deps\skewmatrix"
#define OSGALHOME "osgAL_0.6.1-76_msvc-8.0-sp1-x86"
#define POCOHOME "POCO_1.3.5_msvc-8.0-sp1-x86"
#define OSGWORKSHOME "osgWorks_1.0.0_msvc-8.0-sp1-x86"


; Source directories for compiling application specific installers
#define ACETAOSRCHOME "C:\Projects\ves-windows\ACE_wrappers"
;The directory below contains all each apr, apr-iconv, and apr-util
;The versioned directories have been copied to be easier to build
;and less version dependent when running this installer
#define APRINSTALL "C:\Projects\ves-windows\workapr"
; Minerva deps
#define MINERVASRCHOME "C:\dev\ves_deps\minerva-gis-svn\install-win32"
; #define FWTOOLS "C:\Program Files\FWTools2.3.0"
#define PROJHOME "C:\dev\ves_deps\proj-4.6.1\install-win32"
#define GDALHOME "C:\dev\ves_deps\gdal-1.6.2\install-win32"
; #define CURL_HOME "C:\dev\ves_deps\curl-7.19.5\build-win32"
; #define OSG3RDPARTY "C:\dev\ves_deps\3rdParty\3rdParty_win32binaries_vs80sp1"
; osgAL deps
#define LIBOGGHOME "C:\dev\ves_deps\osgAL_Test\libvorbis-1.2.0\ogg"
#define LIBVORBISHOME "C:\dev\ves_deps\osgAL_Test\libvorbis-1.2.0\libvorbis-1.2.0"
#define OPENALHOME "C:\dev\ves_deps\osgAL_Test"
#define OSGALSRCHOME "C:\dev\ves_deps\osgAL_Test\osgal\install-win32-msvs2008"
#define ALUTSRCINSTALL "C:\dev\ves_deps\osgAL_Test\freealut-1.1.0-bin\freealut-1.1.0-bin"
; OSG deps
#define OSGSRCHOME "C:\dev\deps\OpenSceneGraph-2.8.2\install-win32"
; #define SIMAGEHOME "D:\devEnv\VES-Deps_1.1\prebuiltInstalls\simage-1.6.1"
#define COINHOME "C:\dev\deps\Coin3D"
#define OSG3RDPARTY "C:\dev\deps\3rdParty_Win32Binaries_vc90sp1\3rdParty"
; osgWorks deps
#define OSGWORKSINSTLOCATION "C:\Projects\osgWorks"
; POCO deps
#define POCOSRCHOME "C:\Projects\ves-windows\poco-1.3.5-all"
#define SQLITEHOME "C:\Projects\ves-windows\sqlitedll-3_6_20"
; VR Juggler deps
#define VRJUGGLER_INST_LOCATION "C:\dev\ves_deps\vrjuggler-gc-svn\install-win32"
#define VRJUGGLER_DEPS_INST_LOCATION "C:\dev\ves_deps\vrjuggler-gc-svn\build-win32"
; VTK deps
#define VTKSRCHOME "C:\dev\ves_deps\vtk-5.2.0-install"
; WX deps
#define WXSRCINSTALL "C:\wxWidgets-2.8.10"
; xerces deps
#define XERCESSRCINSTALL "C:\Projects\ves-windows\xerces-c-3.0.1-x86-windows-vc-9.0"
