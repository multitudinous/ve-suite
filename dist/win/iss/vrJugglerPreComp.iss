; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#include <vesenv.iss>
#define MyAppVer "2.3.18-21195"
#define MyAppName "VRJuggler Pre-Compile"
#define MyAppVerName "VRJuggler"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define VRJUGGLER_INST_LOCATION "C:\dev\ves_deps\vrjuggler-gc-svn\install-win32"
#define VRJUGGLER_DEPS_INST_LOCATION "C:\dev\ves_deps\vrjuggler-gc-svn\build-win32"

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
;Get all of VR Juggler
Source: {#VRJUGGLER_INST_LOCATION}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_INST_LOCATION}\include\*.*pp; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_INST_LOCATION}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_INST_LOCATION}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_INST_LOCATION}\lib\*.fpc; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_INST_LOCATION}\lib\*.exp; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_INST_LOCATION}\bin\*; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs skipifsourcedoesntexist
Source: {#VRJUGGLER_INST_LOCATION}\share\*; DestDir: {app}\share; Flags: ignoreversion recursesubdirs createallsubdirs skipifsourcedoesntexist

;Get all of VR Juggler Deps
Source: {#VRJUGGLER_DEPS_INST_LOCATION}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_DEPS_INST_LOCATION}\include\*.*pp; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_DEPS_INST_LOCATION}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_DEPS_INST_LOCATION}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_DEPS_INST_LOCATION}\lib\*.fpc; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_DEPS_INST_LOCATION}\lib\*.exp; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VRJUGGLER_DEPS_INST_LOCATION}\bin\*; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs skipifsourcedoesntexist
Source: {#VRJUGGLER_DEPS_INST_LOCATION}\share\*; DestDir: {app}\share; Flags: ignoreversion recursesubdirs createallsubdirs skipifsourcedoesntexist
Source: {#VRJUGGLER_DEPS_INST_LOCATION}\share\*.fpc; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs skipifsourcedoesntexist


; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}; Languages: 
