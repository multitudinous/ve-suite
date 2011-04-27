; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define GRAYBOXDIR "C:\Users\mccdo\Desktop\graybox_opc_automation_wrapper"
#define MyAppVer "2.02"
#define MyAppName "OPC Graybox"
#define MyAppVerName "OPC"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"

[Setup]
AppName={#MyAppName}_{#MyAppVer}
AppVerName={#MyAppVerName}_{#MyAppVer}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={cf}\{#MyAppVerName}_{#MyAppVer}
DefaultGroupName={#VESGROUPNAME}\Uninstallers
AllowNoIcons=true
OutputBaseFilename={#MyAppVerName}_{#MyAppVer}
SetupIconFile={#VEDEVHOME}\dist\installerImages\ves_icon.ico
Compression=lzma/Max
SolidCompression=true
WindowVisible=true
WizardImageFile={#VEDEVHOME}\dist\installerImages\velauncher_banner.bmp
BackColor=$00A16502
BackColor2=$001B84F7
WizardImageBackColor=clWhite
WizardSmallImageFile={#VEDEVHOME}\dist\installerImages\ve_icon.bmp
WizardImageStretch=false
OutputDir={#INSTALLERINSTALLLOCATION}
AllowRootDirectory=true
EnableDirDoesntExistWarning=false
PrivilegesRequired=admin
RestartIfNeededByRun=false
UsePreviousGroup=false
AppendDefaultGroupName=true
TimeStampsInUTC=true
Uninstallable=true
UsePreviousAppDir=false
VersionInfoVersion=1.0.0
VersionInfoCompany=SMDS
VersionInfoProductVersion=1.0.0
AppVersion=1.0.0
UninstallDisplayName={#MyAppName}_{#MyAppVer}
VersionInfoProductName=OPC Graybox
DisableDirPage=yes

[Files]
Source: {#GRAYBOXDIR}\x86\gbda_aut.dll; DestDir: {sys}; Flags: 32bit regserver sharedfile uninsnosharedfileprompt; 

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}_{#MyAppVer}}; Filename: {uninstallexe}; Languages: 
