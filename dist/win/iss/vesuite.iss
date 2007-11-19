; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppName "VE-Suite"
#define MyAppVerName "VE-Suite 1.1.2"
#define MyAppPublisher "Virtural Engineering Research Group"
#define MyAppURL "www.vesuite.org"
#define VELauncher "velauncher.exe"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}_{#SVNVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\{#MyAppName}_{#VEVERSION}
DefaultGroupName={#MyAppName}
AllowNoIcons=true
OutputDir={#DEPENDSINSTALLHOME}
OutputBaseFilename=vesuite{#VEVERSION}_{#SVNVERSION}
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
SetupIconFile={#VEDEVHOME}\dist\installerImages\Ve_icon.ico

[Types]
Name: full; Description: Full installation
Name: custom; Description: Custom installation; Flags: iscustom


[Components]
Name: nameserver; Description: Name Server; Types: full
Name: vexplorer; Description: VE-Xplorer; Types: full
Name: veconductor; Description: VE-Conductor (GUI); Types: full
Name: vebuildenv; Description: Headers and Libs; Types: full
Name: examples; Description: Example datasets; Types: full
;Name: buildertools; Description: VE-Suite BuilderTools; Types: full
[Registry]
Root: HKCU; Subkey: Software\VE-Suite-Launcher; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\VE-Conductor; ValueType: none; Components: " examples vebuildenv veconductor vexplorer nameserver"; Tasks: " desktopVELauncherIcon"; Flags: uninsdeletekeyifempty
Root: HKCR; SubKey: .ves; ValueType: string; ValueData: VESNetworkfile; Flags: uninsdeletekey createvalueifdoesntexist
Root: HKCR; SubKey: VESNetworkfile; ValueType: string; ValueData: VE-Suite Network file; Flags: uninsdeletekey
Root: HKCR; SubKey: VESNetworkfile\shell\open\command; ValueType: string; ValueData: """{app}\bin\velauncher.exe"" ""%1"""; Flags: uninsdeletevalue
Root: HKCR; Subkey: VESNetworkfile\DefaultIcon; ValueType: string; ValueData: {app}\bin\installerImages\Ve_document.ico; Flags: uninsdeletevalue; Components: ; Tasks: 
Root: HKCR; Subkey: VESNetworkfile\shell\OpenWithVELauncher; ValueType: string; ValueData: Open with &VE-Launcher
Root: HKCR; Subkey: VESNetworkfile\shell\OpenWithVELauncher\command; ValueType: string; ValueData: """{app}\bin\velauncher.exe"" ""%1"""; Flags: createvalueifdoesntexist uninsdeletekey
[Tasks]
Name: desktopVELauncherIcon; Description: VE-Launcher; GroupDescription: Create Desktop Icon

[Files]
Source: {#VEINSTALLHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VEINSTALLHOME}\bin\*; DestDir: {app}\bin; Flags: ignoreversion uninsremovereadonly

Source: {#VEINSTALLHOME}\share\*; DestDir: {app}\share\; Components: examples; Flags: recursesubdirs createallsubdirs
Source: {#VEINSTALLHOME}\include\; DestDir: {app}\include\; Attribs: readonly; Flags: replacesameversion uninsremovereadonly recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEINSTALLHOME}\lib\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#VEINSTALLHOME}\bin\installerImages\*; DestDir: {app}\bin\installerImages; Flags: replacesameversion
Source: {#SKEWMATRIXHOME}\osgPT\trunk\bin\win32\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist
Source: {#VEDEVHOME}\external\vcredist_x86.exe; DestDir: {tmp}
Source: {#VEDEVHOME}\src\apps\launcher\velauncher.exe; DestDir: {app}\bin

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
Name: {group}\VE-Suite-{#VEVERSION}; Filename: {app}\bin\{#VELauncher}; WorkingDir: {app}; IconFilename: {app}\bin\installerImages\VE_icon.ico
Name: {commondesktop}\VE-Suite-{#VEVERSION}; Filename: {app}\bin\velauncher.exe; WorkingDir: {app}; IconFilename: {app}\bin\installerImages\VE_icon.ico; Tasks: desktopVELauncherIcon

[Run]
Filename: {tmp}\vcredist_x86.exe; Description: Install Microsoft Runtime Redistributable for SP1 (NOTE: This is REQIURED to run VE-Suite if Microsoft Visual Studio SP1 compatible runtime libraries are not already installed); StatusMsg: Installing Microsoft Runtime Redistributable for SP1...; Flags: postinstall unchecked; Components: 
[_ISToolPreCompile]
Name: D:\devEnv\VES\share\scripts\win\buildVELauncher.exe.bat; Parameters: ; Flags: abortonerror
[_ISTool]
UseAbsolutePaths=false
LogFile=C:\devEnv\VE_Suite_1.0\VE_Installer\installer\compile.log
LogFileAppend=false
[UninstallDelete]
Name: {app}; Type: filesandordirs
