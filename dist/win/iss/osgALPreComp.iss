; osgAL iss installer
#include <vesenv.iss>
#define OSGALVERSION "0.6.1"
#define MyAppName "osgAL Pre-Compile"
#define MyAppVerName "osgAL_{#OSGALVERSION} Pre-Compile_vc{#MSVCVERSION}"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define LIBOGGHOME "C:\TSVEG\Dependencies\libogg-1.1.3"
#define LIBVORBISHOME "C:\TSVEG\Dependencies\libvorbis-1.2.0"
#define OPENALHOME "C:\Program Files\OpenAL 1.1 SDK"
#define OSGALHOME "C:\TSVEG\Dependencies\osgal-0.6.1"

[Setup]
AppName={#MyAppName}
AppVerName=osgAL_{#OSGALVERSION} Pre-Compile_vc{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\osgAL_{#OSGALVERSION}_Pre-Compile_vc{#MSVCVERSION}
DefaultGroupName={#MyAppName}
OutputBaseFilename=osgAL_{#OSGALVERSION}-precompile_{#MSVCVERSION}
Compression=lzma
SolidCompression=true
OutputDir={#DEPENDSINSTALLHOME}
WindowVisible=true
WizardImageFile={#VEDEVHOME}\dist\installerImages\velauncher_banner.bmp
BackColor=$a16502
BackColor2=$1b84f7
WizardSmallImageFile={#VEDEVHOME}\dist\installerImages\ve_icon.bmp
WizardImageStretch=false
AllowRootDirectory=true
WizardImageBackColor=clWhite
SetupIconFile={#VEDEVHOME}\dist\installerImages\ve_icon.ico
EnableDirDoesntExistWarning=true
PrivilegesRequired=none
RestartIfNeededByRun=false

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Files]
; includes
Source: {#LIBOGGHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#LIBVORBISHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#OPENALHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#OSGALHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
; libs
Source: {#LIBOGGHOME}\win32\Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBOGGHOME}\win32\Dynamic_Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#OPENALHOME}\libs\win32\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#OPENALHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#OSGALHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
; dlls
Source: {#LIBOGGHOME}\win32\Dynamic_Debug\*.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#LIBOGGHOME}\win32\Dynamic_Release\*.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Debug\*.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Release\*.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Debug\*.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Release\*.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Debug\*.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Release\*.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#OPENALHOME}\lib\*.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#OSGALHOME}\bin\*.dll; DestDir: {app}\bin; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
