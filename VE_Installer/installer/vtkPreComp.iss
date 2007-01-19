; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#include <vesenv.iss>
#define VTKVERSION "5.0"
#define MyAppName "VTK Pre-Compile"
#define MyAppVerName "VTK_{#VTKVERSION} Pre-Compile_vc{#MSVCVERSION} "
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"

[Setup]
AppName={#MyAppName}
AppVerName=VTK_{#VTKVERSION} Pre-Compile_vc{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\VTK_Pre-Compile_{#VTKVERSION}
DefaultGroupName={#MyAppName}
AllowNoIcons=true
OutputBaseFilename=vtkPreCompile_{#VTKVERSION}_{#MSVCVERSION}
SetupIconFile={#VEHOME}\VE_Installer\installer\installerImages\Ve_icon.ico
Compression=lzma
SolidCompression=yes
WindowVisible=true
WizardImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_banner_1.0.bmp
BackColor=clBlack
BackColor2=$0080ff
WizardImageBackColor=clGray
WizardSmallImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_icon.bmp
WizardImageStretch=false

[Languages]
Name: eng; MessagesFile: compiler:Default.isl

[Files]
Source: ..\..\..\VES.1.0-Deps\vtk-5.0.0_Install_vc8\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: ..\..\..\VES.1.0-Deps\vtk-5.0.0_Install_vc8\lib\*; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: ..\..\..\VES.1.0-Deps\vtk-5.0.0_Install_vc8\bin\*; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
