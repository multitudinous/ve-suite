; osgAL iss installer
#include <vesenv.iss>
#define MyAppVer "0.6.1-88"
#define MyAppName "osgAL Pre-Compile"
#define MyAppVerName "osgAL"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define LIBOGGHOME "C:\dev\ves_deps\osgAL_Test\libvorbis-1.2.0\ogg"
#define LIBVORBISHOME "C:\dev\ves_deps\osgAL_Test\libvorbis-1.2.0\libvorbis-1.2.0"
#define OPENALHOME "C:\dev\ves_deps\osgAL_Test"
#define OSGALHOME "C:\dev\ves_deps\osgAL_Test\osgal\install-win32-msvs2008"
#define ALUTSRCINSTALL "C:\dev\ves_deps\osgAL_Test\freealut-1.1.0-bin\freealut-1.1.0-bin"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}_{#MyAppVer}_{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\{#MyAppVerName}_{#MyAppVer}_{#MSVCVERSION}
DefaultGroupName={#VESGROUPNAME}\Uninstallers_{#MSVCVERSION}
AllowNoIcons=true
OutputBaseFilename={#MyAppVerName}_{#MyAppVer}_{#MSVCVERSION}
SetupIconFile={#VEDEVHOME}\dist\installerImages\ves_icon.ico
Compression=lzma
SolidCompression=true
WindowVisible=true
WizardImageFile={#VEDEVHOME}\dist\installerImages\velauncher_banner.bmp
BackColor=$a16502
BackColor2=$1b84f7
WizardImageBackColor=clWhite
WizardSmallImageFile={#VEDEVHOME}\dist\installerImages\ve_icon.bmp
WizardImageStretch=false
OutputDir={#INSTALLERINSTALLLOCATION}
AllowRootDirectory=true
EnableDirDoesntExistWarning=true
PrivilegesRequired=none
RestartIfNeededByRun=false
UsePreviousGroup=false
AppendDefaultGroupName=true
TimeStampsInUTC=true
DisableProgramGroupPage=false
Uninstallable=true
[Languages]
Name: eng; MessagesFile: compiler:Default.isl

[Files]
; includes
Source: {#LIBOGGHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#LIBVORBISHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#OPENALHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#OSGALHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#ALUTSRCINSTALL}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
; libs
;Source: {#LIBOGGHOME}\win32\Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBOGGHOME}\win32\Dynamic_Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion
;Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion
;Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion
;Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#OPENALHOME}\libs\win32\*.lib; DestDir: {app}\lib; Flags: ignoreversion
;Source: {#OPENALHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#OSGALHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#ALUTSRCINSTALL}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
; dlls
;Source: {#LIBOGGHOME}\win32\Dynamic_Debug\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBOGGHOME}\win32\Dynamic_Release\*.dll; DestDir: {app}\lib; Flags: ignoreversion
;Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Debug\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Release\*.dll; DestDir: {app}\lib; Flags: ignoreversion
;Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Debug\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Release\*.dll; DestDir: {app}\lib; Flags: ignoreversion
;Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Debug\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Release\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#OPENALHOME}\libs\win32\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#OSGALHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#ALUTSRCINSTALL}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion
;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\osgAL.fpc.in; DestDir: {app}\lib\flagpoll; DestName: osgAL.fpc; Languages: ; Flags: ignoreversion
[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
