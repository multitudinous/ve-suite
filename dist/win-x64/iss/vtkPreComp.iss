; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#include <vesenv.iss>
#define VTKVERSION "5.2.0"
#define MyAppName "VTK Pre-Compile"
#define MyAppVerName "VTK_{#VTKVERSION}_Pre-Compile_vc{#MSVCVERSION} "
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define VTKSRCHOME "C:\dev\ves_deps\vtk-5.2.0-install"
[Setup]
AppName={#MyAppName}
AppVerName=VTK_{#VTKVERSION}_Pre-Compile_vc{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\VTK_{#VTKVERSION}
DefaultGroupName={#VESGROUPNAME}
AllowNoIcons=true
OutputBaseFilename=vtkPreCompile_{#VTKVERSION}_{#MSVCVERSION}
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
[Languages]
Name: eng; MessagesFile: compiler:Default.isl

[Files]
Source: {#VTKSRCHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VTKSRCHOME}\lib\*lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VTKSRCHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\VTK.fpc.in; DestDir: {app}\lib\flagpoll; DestName: VTK.fpc; Languages: ; Flags: ignoreversion
