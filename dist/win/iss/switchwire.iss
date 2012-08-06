; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#ifndef vesAutoBuild
  #include <vesenv.iss>
#endif

#define MyAppVer "0.2.0"
#define MyAppName "switchwire Pre-Compile"
#define MyAppVerName "switchwire"
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
;Data for binary download
Source: {#VESAUTODEPSDIR}\{#SWITCHWIREDIRNAME}\{#VESINSTALLDIR}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VESAUTODEPSDIR}\{#SWITCHWIREDIRNAME}\{#VESINSTALLDIR}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#VESAUTODEPSDIR}\{#SWITCHWIREDIRNAME}\{#VESINSTALLDIR}\lib\*; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VESAUTODEPSDIR}\{#SWITCHWIREDIRNAME}\{#VESINSTALLDIR}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VESAUTODEPSDIR}\{#SWITCHWIREDIRNAME}\{#VESINSTALLDIR}\share\*; DestDir: {app}\share; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}_{#MyAppVer}_{#MSVCVERSION}}; Filename: {uninstallexe}; Languages: 
