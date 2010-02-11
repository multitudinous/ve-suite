; osgAL iss installer
#include <vesenv.iss>
#define MyAppVer "0.6.1-88"
#define MyAppName "osgAudio Pre-Compile"
#define MyAppVerName "osgAudio"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"

[Setup]
AppName={#MyAppName}_{#MyAppVer}_{#MSVCVERSION}
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
UsePreviousAppDir=false
VersionInfoVersion=1.0.0
VersionInfoCompany=SMDS
VersionInfoProductVersion=1.0.0
AppVersion=1.0.0
UninstallDisplayIcon={#VEDEVHOME}\dist\installerImages\ve_icon.bmp
UninstallDisplayName={#MyAppName}_{#MyAppVer}_{#MSVCVERSION}

[Languages]
Name: eng; MessagesFile: compiler:Default.isl

[Files]
; includes
Source: {#LIBOGGHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#LIBVORBISHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#OPENALHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#OSGALSRCHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#ALUTSRCINSTALL}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
; libs
;Source: {#LIBOGGHOME}\win32\Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBOGGHOME}\win32\VS2008\Win32\Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs; Languages: 
;Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VS2008\Win32\Release\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs
;Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
; Source: {#LIBVORBISHOME}\win32\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs
;Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Debug\*.lib; DestDir: {app}\lib; Flags: ignoreversion
; Source: {#LIBVORBISHOME}\win32\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs
Source: {#OPENALHOME}\libs\Win32\*.lib; DestDir: {app}\lib; Flags: ignoreversion
;Source: {#OPENALHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#OSGALSRCHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#ALUTSRCINSTALL}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
; dlls
;Source: {#LIBOGGHOME}\win32\Dynamic_Debug\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBOGGHOME}\win32\VS2008\Win32\Release\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs
;Source: {#LIBVORBISHOME}\win32\Vorbis_Dynamic_Debug\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#LIBVORBISHOME}\win32\VS2008\Win32\Release\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs
;Source: {#LIBVORBISHOME}\win32\VorbisEnc_Dynamic_Debug\*.dll; DestDir: {app}\lib; Flags: ignoreversion
; Source: {#LIBVORBISHOME}\win32\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs
;Source: {#LIBVORBISHOME}\win32\VorbisFile_Dynamic_Debug\*.dll; DestDir: {app}\lib; Flags: ignoreversion
; Source: {#LIBVORBISHOME}\win32\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs
; Source: {#OPENALHOME}\libs\Win32\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#OSGALSRCHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#ALUTSRCINSTALL}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion
; exe
Source: {#OPENALHOME}\redist\oalinst.exe; DestDir: {app}\share; Flags: ignoreversion
Source: {#OSGALSRCHOME}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion
; data
Source: {#OSGAUDIOROOTHOME}\data\*; DestDir: {app}\share\data; Flags: ignoreversion

;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\osgAL.fpc.in; DestDir: {app}\lib\flagpoll; DestName: osgAL.fpc; Languages: ; Flags: ignoreversion
[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}_{#MyAppVer}_{#MSVCVERSION}}; Filename: {uninstallexe}
[Run]
Filename: {app}\share\oalinst.exe; Flags: runascurrentuser
