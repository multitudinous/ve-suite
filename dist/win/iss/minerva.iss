; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
#include <vesenv.iss>
#define MyAppVer "5469"
#define MyAppName "Minerva Pre-Compile"
#define MyAppVerName "Minerva"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define MINERVASRCHOME "C:\dev\ves_deps\cadkit-svn\install-win32"
#define FWTOOLS "C:\Program Files\FWTools2.3.0"
; #define CURL_HOME "C:\dev\ves_deps\curl-7.19.5\build-win32"
; #define OSG3RDPARTY "C:\dev\ves_deps\3rdParty\3rdParty_win32binaries_vs80sp1"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}_{#MyAppVer}_{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\{#MyAppVerName}_{#MyAppVer}_{#MSVCVERSION}
DefaultGroupName={#VESGROUPNAME}\Uninstallers
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
;Minerva
Source: {#MINERVASRCHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#MINERVASRCHOME}\bin\*.exe; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist
Source: {#MINERVASRCHOME}\bin\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#MINERVASRCHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#MINERVASRCHOME}\bin\*.plug; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs

;FWTools
Source: {#FWTOOLS}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#FWTOOLS}\bin\*.exe; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#FWTOOLS}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#FWTOOLS}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs

;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\Minerva.fpc.in; DestDir: {app}\lib\flagpoll; DestName: Minerva.fpc; Languages: ; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}; Languages: 
