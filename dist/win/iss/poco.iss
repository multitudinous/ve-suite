; POCO iss installer
; Utility library for db's and other good stuff
#include <vesenv.iss>
#define MyAppVer "1.3.5"
#define MyAppName "POCO Pre-Compile"
#define MyAppVerName "POCO"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define POCOHOME "C:\Projects\ves-windows\poco-1.3.5-all"
#define SQLITEHOME "C:\Projects\ves-windows\sqlitedll-3_6_20"

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
Name: english; MessagesFile: compiler:Default.isl

[Files]
; includes
Source: {#POCOHOME}\Foundation\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\XML\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\Util\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\Zip\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\Data\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\Data\MySQL\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\Data\ODBC\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs
Source: {#POCOHOME}\Data\SQLite\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs

; libs
Source: {#POCOHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion

; dlls
Source: {#POCOHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#SQLITEHOME}\*.dll; DestDir: {app}\lib; Flags: ignoreversion

;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\POCO.fpc.in; DestDir: {app}\lib\flagpoll; DestName: POCO.fpc; Languages: ; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
