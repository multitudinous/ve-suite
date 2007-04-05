; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define XERCESVERSION "2.7"
#define MyAppName "Xerces-c Pre-Compile"
#define MyAppVerName "Xerces-c_{#XERCESVERSION}  Pre-Compile_vc{#MSVCVERSION}"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"

[Setup]
AppName={#MyAppName}
AppVerName=Xerces-c_{#XERCESVERSION} Pre-Compile_vc{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\Xerces-c_{#XERCESVERSION}_Pre-Compile
DefaultGroupName={#MyAppName}
AllowNoIcons=true
OutputBaseFilename=xerces-c_PreCompile_{#XERCESVERSION}
Compression=lzma
SolidCompression=yes
OutputDir={#VEHOME}\VE_Installer\installer
WizardImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_banner_1.0.bmp
WizardSmallImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_icon.bmp
WindowVisible=true
WizardImageStretch=false
WizardImageBackColor=clGray
BackColor=clBlack
BackColor2=$0080ff
SetupIconFile={#VEHOME}\VE_Installer\installer\installerImages\Ve_icon.ico


[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Files]
Source: {#XERCESHOME}\Build\Win32\VC7.1\*.dll; DestDir: {app}\Build\Win32\VC7.1\; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#XERCESHOME}\Build\Win32\VC7.1\*.lib; DestDir: {app}\Build\Win32\VC7.1\; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#XERCESHOME}\src\*.h*; DestDir: {app}\src; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#XERCESHOME}\src\*.c; DestDir: {app}\src; Flags: ignoreversion recursesubdirs createallsubdirs

; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}

