; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define XERCESVERSION "3.0.1"
#define MyAppVer "3.0.1"
#define MyAppName "Xerces-c Pre-Compile"
#define MyAppVerName "xerces-c"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define XERCESSRCINSTALL "E:\dev\ves_deps\xerces-c-3.0.1-x86_64-windows-vc-9.0\xerces-c-3.0.1-x86_64-windows-vc-9.0"
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
SetupIconFile={#VEDEVHOME}\dist\installerImages\Ve_icon.ico
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
PrivilegesRequired=none
UsePreviousGroup=false
EnableDirDoesntExistWarning=true
UsePreviousAppDir=false

[Languages]
Name: english; MessagesFile: compiler:Default.isl

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
Source: {#VEDEVHOME}\dist\win-x64\fpc_deps_files\release\xerces.fpc.in; DestDir: {app}\lib\flagpoll; DestName: xerces.fpc; Languages: ; Flags: ignoreversion
[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
