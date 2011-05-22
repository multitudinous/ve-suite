; POCO iss installer
; Utility library for db's and other good stuff
#ifndef vesAutoBuild
  #include <vesenv.iss>
#endif

#define MyAppVer "1.4.1"
#define MyAppName "POCO Pre-Compile"
#define MyAppVerName "POCO"
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
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\Foundation\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\XML\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\Util\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\Zip\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\Data\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\Data\MySQL\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\Data\ODBC\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\Data\SQLite\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs

#ifdef x64
; libs
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\lib64\*.lib; DestDir: {app}\lib; Flags: ignoreversion
; dlls
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\bin64\*.dll; DestDir: {app}\lib; Flags: ignoreversion
#else
; libs
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
; dlls
Source: {#VESAUTODEPSDIR}\{#POCODIRNAME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion
#endif
;Source: {#SQLITEHOME}\*.dll; DestDir: {app}\lib; Flags: ignoreversion

;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\POCO.fpc.in; DestDir: {app}\lib\flagpoll; DestName: POCO.fpc; Languages: ; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}_{#MyAppVer}_{#MSVCVERSION}}; Filename: {uninstallexe}
