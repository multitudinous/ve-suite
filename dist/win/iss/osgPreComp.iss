; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#include <vesenv.iss>
#define OSGVERSION "2.6.1"
#define MyAppName "OSG Pre-Compile"
#define MyAppVerName "OSG_{#OSGVERSION}_Pre-Compile_vc{#MSVCVERSION}"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define OSGSRCHOME "C:\dev\ves_deps\OpenSceneGraph-2.6.1\install-win32"
; #define SIMAGEHOME "D:\devEnv\VES-Deps_1.1\prebuiltInstalls\simage-1.6.1"
#define COINHOME "C:\dev\ves_deps\Coind3D-2.3.0-0"
#define OSG3RDPARTY "C:\dev\ves_deps\3rdParty\3rdParty_win32binaries_vs80sp1"

[Setup]
AppName={#MyAppName}
AppVerName=OSG_{#OSGVERSION} Pre-Compile_vc{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\OSG_{#OSGVERSION}_Pre-Compile_vc{#MSVCVERSION}
DefaultGroupName={#MyAppName}
OutputBaseFilename=osg{#OSGVERSION}-precompile_{#MSVCVERSION}
Compression=lzma
SolidCompression=true
OutputDir={#INSTALLERINSTALLLOCATION}
WindowVisible=true
WizardImageFile={#VEDEVHOME}\dist\installerImages\velauncher_banner.bmp
BackColor=$a16502
BackColor2=$1b84f7
WizardSmallImageFile={#VEDEVHOME}\dist\installerImages\ve_icon.bmp
WizardImageStretch=false
AllowRootDirectory=true
WizardImageBackColor=clWhite
SetupIconFile={#VEDEVHOME}\dist\installerImages\Ve_icon.ico
EnableDirDoesntExistWarning=true
PrivilegesRequired=none
RestartIfNeededByRun=false

[Languages]
Name: eng; MessagesFile: compiler:Default.isl

[Files]
;OpenSceneGraph
Source: {#OSGSRCHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#OSGSRCHOME}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#OSGSRCHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#OSGSRCHOME}\bin\*.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs

;simage
; Source: {#SIMAGEHOME}\bin\simage*.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs

;OSG 3rd Party libs
Source: {#OSG3RDPARTY}\bin\*.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#OSG3RDPARTY}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#OSG3RDPARTY}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
;Coin libraries
Source: {#COINHOME}\bin\*.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#COINHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#COINHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\OSG.fpc.in; DestDir: {app}\lib\flagpoll; DestName: OSG.fpc; Languages: ; Flags: ignoreversion
