; MySQL++ iss installer
#include <vesenv.iss>
#define MYSQLPPVERSION "3.0.9"
#define MyAppName "MySQL++ Pre-Compile"
#define MyAppVerName "MySQL++_{#MYSQLPPVERSION} Pre-Compile_vc{#MSVCVERSION}"
#define MyAppPublisher "VERG"
#define MyAppURL "www.vesuite.org"
#define MYSQLPPHOME "C:\dev\Dependencies\MySQL++\mysql++-3.0.9_install"
#define MYSQLHOME "C:\Program Files\MySQL\MySQL Server 5.0"

[Setup]
AppName={#MyAppName}
AppVerName=MySQL++_{#MYSQLPPVERSION} Pre-Compile_vc{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\MySQL++_{#MYSQLPPVERSION}_Pre-Compile_vc{#MSVCVERSION}
DefaultGroupName={#VESGROUPNAME}\Uninstallers
AllowNoIcons=true
OutputBaseFilename=MySQL++_{#MYSQLPPVERSION}-precompile_{#MSVCVERSION}
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
Source: {#MYSQLPPHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion
Source: {#MYSQLHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion

; libs
Source: {#MYSQLPPHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion
Source: {#MYSQLHOME}\lib\opt\*.lib; DestDir: {app}\lib; Flags: ignoreversion

; dlls
Source: {#MYSQLPPHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion
Source: {#MYSQLHOME}\lib\opt\*.dll; DestDir: {app}\lib; Flags: ignoreversion

;FPC file integration
Source: {#VEDEVHOME}\dist\win\fpc_deps_files\release\MySQLpp.fpc.in; DestDir: {app}\lib\flagpoll; DestName: MySQLpp.fpc; Languages: ; Flags: ignoreversion

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
