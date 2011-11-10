; Minerva installer script
#include <vesenv.iss>
#define MyAppVer "1.2.1"
#define MyAppName "Lemon Pre-Compile"
#define MyAppVerName "Lemon"
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
;Lemon
Source: {#LEMONINSTALLHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#LEMONINSTALLHOME}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist
Source: {#LEMONINSTALLHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#LEMONINSTALLHOME}\lib\*.pdb; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs

;glpk
Source: {#GLPKSRCHOME}\w32\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GLPKSRCHOME}\w32\*.exe; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GLPKSRCHOME}\w32\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GLPKSRCHOME}\w32\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GLPKSRCHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GLPKSRCHOME}\src\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GLPKSRCHOME}\examples\*; DestDir: {app}\share\examples; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#GLPKSRCHOME}\doc\*; DestDir: {app}\share\doc; Flags: ignoreversion recursesubdirs createallsubdirs

;cbc
Source: {#CBCSRCHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#CBCSRCHOME}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#CBCSRCHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#CBCSRCHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#CBCSRCHOME}\share\*; DestDir: {app}\share; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#CBCSRCHOME}\examples\*; DestDir: {app}\share\examples; Flags: ignoreversion recursesubdirs createallsubdirs

;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\Lemon.fpc.in; DestDir: {app}\lib\flagpoll; DestName: Lemon.fpc; Languages: ; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}_{#MyAppVer}_{#MSVCVERSION}}; Filename: {uninstallexe}; Languages: 
