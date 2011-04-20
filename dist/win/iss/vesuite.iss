; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppName "VE-Suite"
#define MyAppVer "2.2.1"
#define MyAppVerName "VE-Suite 2.2.1"
#define MyAppPublisher "Virtural Engineering Research Group"
#define MyAppURL "www.vesuite.org"
#define VELauncher "velauncher.exe"
#define VesIcon "ves_icon.ico"
#define VesDocumentIcon "ves_document.ico"

[InnoIDE_Settings]
LogFile={#VEDEVHOME}\compile.log
LogFileOverwrite=false

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppName}_{#VEVERSION}.{#SVNVERSION}_{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\{#MyAppName}_{#VEVERSION}.{#SVNVERSION}_{#MSVCVERSION}
DefaultGroupName={#VESGROUPNAME}
AllowNoIcons=true
OutputDir={#INSTALLERINSTALLLOCATION}
OutputBaseFilename={#MyAppName}_{#VEVERSION}.{#SVNVERSION}_{#MSVCVERSION}
Compression=lzma
SolidCompression=true
ChangesEnvironment=true
ShowLanguageDialog=yes
AlwaysRestart=false
UninstallFilesDir={app}\bin
UninstallRestartComputer=false
WizardImageFile={#VEDEVHOME}\dist\installerImages\velauncher_banner.bmp
WizardImageStretch=false
WizardSmallImageFile={#VEDEVHOME}\dist\installerImages\ve_icon.bmp
WindowVisible=true
WizardImageBackColor=clWhite
ChangesAssociations=true
BackColor=$a16502
BackColor2=$1b84f7
SetupIconFile={#VEDEVHOME}\dist\installerImages\{#VesIcon}
PrivilegesRequired=none
UsePreviousGroup=false
VersionInfoVersion=1.0.0
VersionInfoCompany=SMDS
VersionInfoProductVersion=1.0.0
AppVersion=1.0.0
UninstallDisplayIcon={#VEDEVHOME}\dist\installerImages\ve_icon.bmp
UninstallDisplayName={#MyAppName}_{#MyAppVer}_{#MSVCVERSION}

[Types]
Name: full; Description: Full installation
Name: custom; Description: Custom installation; Flags: iscustom

[Components]
Name: nameserver; Description: Name Server; Types: full
Name: vexplorer; Description: VE-Xplorer; Types: full
Name: veconductor; Description: VE-Conductor (GUI); Types: full
Name: vebuildenv; Description: Headers and Libs
Name: examples; Description: Example datasets; Types: full
;Name: buildertools; Description: VE-Suite BuilderTools; Types: full

[Registry]
Root: HKCU; Subkey: Software\VE-Suite-Launcher; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\VE-Conductor; ValueType: none; Components: " examples vebuildenv veconductor vexplorer nameserver"; Tasks: " desktopVELauncherIcon"; Flags: uninsdeletekeyifempty
Root: HKCR; SubKey: .ves; ValueType: string; ValueData: VESNetworkfile; Flags: uninsdeletekey createvalueifdoesntexist
Root: HKCR; SubKey: VESNetworkfile; ValueType: string; ValueData: VE-Suite Network file; Flags: uninsdeletekey
Root: HKCR; SubKey: VESNetworkfile\shell\open\command; ValueType: string; ValueData: """{app}\bin\velauncher.exe"" ""%1"""; Flags: uninsdeletevalue
Root: HKCR; Subkey: VESNetworkfile\DefaultIcon; ValueType: string; ValueData: {app}\bin\installerImages\{#VesDocumentIcon}; Flags: uninsdeletevalue; Components: ; Tasks: 
Root: HKCR; Subkey: VESNetworkfile\shell\OpenWithVELauncher; ValueType: string; ValueData: Open with &VE-Launcher
Root: HKCR; Subkey: VESNetworkfile\shell\OpenWithVELauncher\command; ValueType: string; ValueData: """{app}\bin\velauncher.exe"" ""%1"""; Flags: createvalueifdoesntexist uninsdeletekey
[Tasks]
Name: desktopVELauncherIcon; Description: VE-Launcher; GroupDescription: Create Desktop Icon

[Files]
Source: {#VEINSTALLHOME}\{#LIBDIR}\*.dll; DestDir: {app}\{#LIBDIR}; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VEINSTALLHOME}\bin\*; DestDir: {app}\bin; Flags: ignoreversion uninsremovereadonly

Source: {#VEINSTALLHOME}\share\*; DestDir: {app}\share\; Components: examples; Flags: recursesubdirs createallsubdirs
Source: {#VEINSTALLHOME}\include\*; DestDir: {app}\include\; Attribs: readonly; Flags: replacesameversion uninsremovereadonly recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEINSTALLHOME}\{#LIBDIR}\*.lib; DestDir: {app}\{#LIBDIR}; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#VEINSTALLHOME}\bin\installerImages\*; DestDir: {app}\bin\installerImages; Flags: replacesameversion
Source: {#SKEWMATRIXHOME}\*.dll; DestDir: {app}\{#LIBDIR}; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist
Source: {#VEINSTALLHOME}\bin\velauncher.exe; DestDir: {app}\bin
Source: {#MSREDISTRIBUTABLE}; DestDir: {tmp}
Source: {#OPCVESINSTALLER}; DestDir: {tmp}; Flags: skipifsourcedoesntexist
Source: {#INSTALLERINSTALLLOCATION}\VE-Suite_Dependencies_{#VEVERSION}_{#MSVCVERSION}.exe; DestDir: {tmp}; Flags: ignoreversion

[Icons]
Name: {group}\Uninstallers\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
Name: {group}\VE-Suite-{#VEVERSION}; Filename: {app}\bin\{#VELauncher}; WorkingDir: {app}; IconFilename: {app}\bin\installerImages\{#VesIcon}
Name: {commondesktop}\VE-Suite-{#VEVERSION}; Filename: {app}\bin\velauncher.exe; WorkingDir: {app}; IconFilename: {app}\bin\installerImages\{#VesIcon}; Tasks: desktopVELauncherIcon

[Run]
Filename: {tmp}\{#MSREDISTRIBUTABLEFILENAME}; Description: Install Microsoft Runtime Redistributable for SP1 (NOTE: This is REQIURED to run VE-Suite if Microsoft Visual Studio SP1 compatible runtime libraries are not already installed); StatusMsg: Installing Microsoft Runtime Redistributable for SP1...; Flags: postinstall unchecked; Tasks: 
Filename: {tmp}\VE-Suite_Dependencies_{#VEVERSION}_{#MSVCVERSION}; Flags: postinstall runascurrentuser; Description: VE-Suite Dependency Installer; StatusMsg: Installing VE-Suite dependencies
Filename: {tmp}\{#OPCVESINSTALLERFILENAME}; Description: Install OPC for use with VE-PSI; StatusMsg: Installing OPC for use with VE-PSI...; Flags: postinstall unchecked skipifdoesntexist; Tasks: 

[_ISToolPreCompile]
;Name: D:\devEnv\VES\share\scripts\win\buildVELauncher.exe.bat; Parameters: ; Flags: abortonerror

[_ISTool]
UseAbsolutePaths=false
LogFile={#VEDEVHOME}\compile.log
LogFileAppend=false

[UninstallDelete]
Name: {app}; Type: filesandordirs
