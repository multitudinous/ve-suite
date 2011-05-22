; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#ifndef vesAutoBuild
  #include <vesenv.iss>
#endif

#define MyAppVer "0.3.0"
#define MyAppName "backdropFX Pre-Compile"
#define MyAppVerName "backdropFX"
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
Source: {#VESAUTODEPSDIR}\{#BDFXDIRNAME}\{#VESINSTALLDIR}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VESAUTODEPSDIR}\{#BDFXDIRNAME}\{#VESINSTALLDIR}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VESAUTODEPSDIR}\{#BDFXDIRNAME}\{#VESINSTALLDIR}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: {#VESAUTODEPSDIR}\{#BDFXDIRNAME}\{#VESINSTALLDIR}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VESAUTODEPSDIR}\{#BDFXDIRNAME}\{#VESINSTALLDIR}\share\*; DestDir: {app}\share; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\backdropfx.fpc.in; DestDir: {app}\lib\flagpoll; DestName: backdropfx.fpc; Languages: ; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}_{#MyAppVer}_{#MSVCVERSION}}; Filename: {uninstallexe}; Languages: 
