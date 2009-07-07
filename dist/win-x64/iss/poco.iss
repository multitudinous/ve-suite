; MySQL++ iss installer
#include <vesenv.iss>
#define POCOVERSION "1.3.4"
#define MyAppName "POCO Pre-Compile"
#define MyAppVerName "POCO_{#POCOVERSION} Pre-Compile_vc{#MSVCVERSION}"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define POCOHOME "C:\dev\Dependencies\POCO\poco-1.3.4-all"

[Setup]
AppName={#MyAppName}
AppVerName=POCO_{#POCOVERSION} Pre-Compile_vc{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\POCO_{#POCOVERSION}_Pre-Compile_vc{#MSVCVERSION}
DefaultGroupName={#VESGROUPNAME}\Uninstallers
AllowNoIcons=true
OutputBaseFilename=POCO_{#POCOVERSION}-precompile_{#MSVCVERSION}
Compression=lzma
SolidCompression=true
OutputDir={#INSTALLERINSTALLLOCATION}
WindowVisible=true
WizardImageFile={#VEDEVHOME}\dist\installerImages\velauncher_banner.bmp
BackColor=$a16502
BackColor2=$1b84f7
WizardSmallImageFile={#VEDEVHOME}\dist\installerImages\ve_icon.bmp
WizardImageStretch=false
AllowRootDirectory=true
WizardImageBackColor=clWhite
SetupIconFile={#VEDEVHOME}\dist\installerImages\ve_icon.ico
EnableDirDoesntExistWarning=true
PrivilegesRequired=none
RestartIfNeededByRun=false
UsePreviousGroup=false
AppendDefaultGroupName=true
TimeStampsInUTC=true
DisableProgramGroupPage=false
Uninstallable=true

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Files]
; includes
Source: {#POCOHOME}\Foundation\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\Data\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\Data\MySQL\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\Data\ODBC\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs

; libs
Source: {#POCOHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion

; dlls
Source: {#POCOHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion

;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\POCO.fpc.in; DestDir: {app}\lib\flagpoll; DestName: POCO.fpc; Languages: ; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
