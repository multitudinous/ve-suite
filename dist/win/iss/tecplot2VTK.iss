; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>

#define MyAppName "tecplot2VTK"
#define MyAppVerName "tecplot2VTK"
#define MyAppPublisher "Virtural Engineering Research Group"
#define MyAppURL "www.vesuite.org"
#define VEVERSION "0.5.0"
#define SVNVERSION "14230"
#define VesIcon "ves_icon.ico"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\{#MyAppName}_{#VEVERSION}.{#SVNVERSION}_{#MSVCVERSION}
DefaultGroupName={#VESGROUPNAME}
AllowNoIcons=true
OutputDir={#INSTALLERINSTALLLOCATION}
OutputBaseFilename=tecplot2VTK_{#VEVERSION}_{#SVNVERSION}
Compression=lzma
SolidCompression=true
ChangesEnvironment=false
ShowLanguageDialog=yes
AlwaysRestart=false
UninstallFilesDir={app}\bin
UninstallRestartComputer=false
WizardImageFile={#VEDEVHOME}\dist\installerImages\velauncher_banner.bmp
WizardImageStretch=false
WizardSmallImageFile={#VEDEVHOME}\dist\installerImages\velauncher_banner.bmp
WindowVisible=true
WizardImageBackColor=clWhite
ChangesAssociations=true
WindowStartMaximized=false
BackColor=$0080ff
BackColor2=$ff0000
SetupIconFile={#VEDEVHOME}\dist\installerImages\ves_icon.ico
CreateAppDir=true
AppID={{59141459-A0E6-4E73-8925-7EAF045F0936}
AppVersion={#VEVERSION}
UninstallDisplayName=tecplot2VTK
DisableDirPage=true
UsePreviousAppDir=false
DirExistsWarning=no
AppendDefaultDirName=false
VersionInfoVersion={#VEVERSION}
VersionInfoCompany={#MyAppPublisher}
VersionInfoProductName={#MyAppName}
VersionInfoProductVersion={#VEVERSION}
UninstallDisplayIcon={app}\bin\installerImages\{#VesIcon}

[Types]
Name: full; Description: Full installation

[Components]
Name: tecplot2VTK; Description: Installs the the tecplot2VTK translator for VE-Suite; Types: full

[Tasks]
Name: desktopVELauncherIcon; Description: tecplot2VTK; GroupDescription: Create Desktop Icon

[Icons]
Name: {group}\Uninstallers\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
Name: {group}\{#MyAppName}; Filename: {app}\bin\tecplotReader.exe; WorkingDir: {app}\bin; IconFilename: {app}\bin\installerImages\{#VesIcon}
Name: {commondesktop}\{#MyAppName}; Filename: {app}\bin\tecplotReader.exe; WorkingDir: {app}\bin; IconFilename: {app}\bin\installerImages\{#VesIcon}; Tasks: desktopVELauncherIcon

[Run]
Filename: {tmp}\vcredist_x64.exe; Description: Install Microsoft Runtime Redistributable; StatusMsg: Installing Microsoft Runtime Redistributable...; Flags: postinstall unchecked; Tasks: 

[Files]
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkzlib.dll; DestDir: {app}\bin; Components: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtktiff.dll; DestDir: {app}\bin; Components: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtksys.dll; DestDir: {app}\bin; Components: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkpng.dll; DestDir: {app}\bin; Components: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkNetCDF.dll; DestDir: {app}\bin; Components: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkmetaio.dll; DestDir: {app}\bin; Components: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkjpeg.dll; DestDir: {app}\bin; Components: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkIO.dll; DestDir: {app}\bin; Components: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkFiltering.dll; DestDir: {app}\bin; Components: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkexpat.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkDICOMParser.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\vtkCommon.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion

Source: {#TECPLOTSDKHOME}\bin\x64\tptoolbox.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#TECPLOTSDKHOME}\bin\x64\tpsdkintegrationmanager.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#TECPLOTSDKHOME}\bin\x64\tpsdkbase.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#TECPLOTSDKHOME}\bin\x64\tecio.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#TECPLOTSDKHOME}\bin\x64\pthreadVC.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#TECPLOTSDKHOME}\bin\x64\libtec.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#TECPLOTSDKHOME}\bin\x64\gltt.dll; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#TECPLOTSDKHOME}\tecplot.fnt; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#TECPLOTSDKHOME}\tecplot.cfg; DestDir: {app}\bin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#TECPLOTSDKHOME}\pub\*; DestDir: {app}\pub; Components: ; Flags: overwritereadonly ignoreversion recursesubdirs

Source: {#VEINSTALLHOME}\bin\installerImages\*; DestDir: {app}\bin\installerImages; Flags: replacesameversion
Source: {#VEINSTALLHOME}\bin\tecplotReader.exe; DestDir: {app}\bin; Flags: replacesameversion

Source: E:\dev\ves_deps\msvc_redist_vc8\vcredist_x64.exe; DestDir: {tmp}
