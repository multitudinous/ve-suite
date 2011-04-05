; Minerva installer script
#include <vesenv.iss>
#define MyAppVer "1.2.0-397"
#define MyAppName "Minerva Pre-Compile"
#define MyAppVerName "Minerva"
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
;Minerva
Source: {#MINERVASRCHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#MINERVASRCHOME}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist
Source: {#MINERVASRCHOME}\bin\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#MINERVASRCHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#MINERVASRCHOME}\bin\*.plug; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs

;Proj Files
Source: {#PROJHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#PROJHOME}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#PROJHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#PROJHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#PROJHOME}\share\*; DestDir: {app}\share; Flags: ignoreversion recursesubdirs createallsubdirs

;GDAL Files
Source: {#GDALHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GDALHOME}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GDALHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GDALHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GDALHOME}\data\*; DestDir: {app}\share\gdal\data; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GDALHOME}\html\*; DestDir: {app}\share\gdal\html; Flags: ignoreversion recursesubdirs createallsubdirs

;FWTools
;Source: {#FWTOOLS}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: {#FWTOOLS}\bin\*.exe; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: {#FWTOOLS}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: {#FWTOOLS}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs

;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\Minerva.fpc.in; DestDir: {app}\lib\flagpoll; DestName: Minerva.fpc; Languages: ; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}_{#MyAppVer}_{#MSVCVERSION}}; Filename: {uninstallexe}; Languages: 
