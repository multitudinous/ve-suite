; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppVer "3.0.1"
#define MyAppName "xerces-c Pre-Compile"
#define MyAppVerName "Xerces"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"

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
UsePreviousAppDir=false

[Languages]
Name: eng; MessagesFile: compiler:Default.isl

[Files]
;Data for source build
;Source: {#XERCESHOME}\Build\Win32\VC7.1\*.dll; DestDir: {app}\Build\Win32\VC7.1\; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: {#XERCESHOME}\Build\Win32\VC7.1\*.lib; DestDir: {app}\Build\Win32\VC7.1\; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: {#XERCESHOME}\src\*.h*; DestDir: {app}\src; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: {#XERCESHOME}\src\*.c; DestDir: {app}\src; Flags: ignoreversion recursesubdirs createallsubdirs
;Data for binary download
Source: {#XERCESSRCINSTALL}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#XERCESSRCINSTALL}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#XERCESSRCINSTALL}\include\*.h*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#XERCESSRCINSTALL}\include\*.c; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\xerces.fpc.in; DestDir: {app}\lib\flagpoll; DestName: xerces.fpc; Languages: ; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}; Languages: 
