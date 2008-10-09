; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

; #include <C:\devEnv\VE_Suite_1.0\VE_Installer\installer\vesenv.iss>
#define MyAppName "osgPTExporter"
#define MyAppVerName "osgPTExporter"
#define MyAppPublisher "Virtural Engineering Research Group"
#define MyAppURL "www.vesuite.org"
#define VEVERSION "0.9.2"
#define SVNVERSION "11477"
#define VEHOME "D:\devEnv\VES"
#define OSGHOME "D:\devEnv\VES-Deps_1.1\prebuiltInstalls\OSG_2.6.0_Pre-Compile_vc8.0_SP1"
#define OSGPTEXPORTERHOME "D:\devEnv\VES-Deps_1.1\source\skewmatrix\osgPT\trunk\OSGExport"
[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={code:GetInstallDir}
DefaultGroupName={#MyAppName}
AllowNoIcons=true
OutputDir=./
OutputBaseFilename=osgPTExporter{#VEVERSION}_{#SVNVERSION}
Compression=lzma
SolidCompression=true
ChangesEnvironment=false
ShowLanguageDialog=yes
AlwaysRestart=false
UninstallFilesDir={app}\bin
UninstallRestartComputer=false
WizardImageFile={#VEHOME}\dist\installerImages\velauncher_banner.bmp
WizardImageStretch=false
WizardSmallImageFile={#VEHOME}\dist\installerImages\velauncher_banner.bmp
WindowVisible=true
WizardImageBackColor=clWhite
ChangesAssociations=true
WindowStartMaximized=false
BackColor=$0080ff
BackColor2=$ff0000
SetupIconFile={#VEHOME}\dist\installerImages\Ve_icon.ico
CreateAppDir=true
AppID={{365078F8-DD03-4AC5-A7AF-F4CBD7593651}
AppVersion={#VEVERSION}
UninstallDisplayName=osgPTExporter
DisableDirPage=true
UsePreviousAppDir=false
DirExistsWarning=no
AppendDefaultDirName=false

[Types]
Name: full; Description: Full installation
;Name: custom; Description: Custom installation; Flags: iscustom

[Components]
Name: osgPTExporter; Description: Installs the OpenSceneGraph PolyTrans exporter; Types: full

[Tasks]
;Name: desktopVELauncherIcon; Description: VE-Launcher; GroupDescription: Create Desktop Icon

[Files]
Source: {#OSGHOME}\bin\osg44-osg.dll; DestDir: {app}; Components: 
Source: {#OSGHOME}\bin\osg44-osgUtil.dll; DestDir: {app}; Components: 
Source: {#OSGHOME}\bin\osg44-osgSim.dll; DestDir: {app}; Components: 
Source: {#OSGHOME}\bin\osg44-osgDB.dll; DestDir: {app}; Components: 
Source: {#OSGHOME}\bin\osg44-osgFX.dll; DestDir: {app}; Components: 
Source: {#OSGHOME}\bin\osg44-osgText.dll; DestDir: {app}; Components: 
Source: {#OSGHOME}\bin\osg44-osgTerrain.dll; DestDir: {app}; Components: 
Source: {#OSGHOME}\bin\ot11-OpenThreads.dll; DestDir: {app}; Components: 
Source: {#OSGHOME}\bin\osgPlugins-2.6.0\osgdb_ive.dll; DestDir: {app}; Components: ; Flags: overwritereadonly ignoreversion
Source: {#OSGHOME}\bin\osgPlugins-2.6.0\osgdb_jpeg.dll; DestDir: {app}; Components: ; Flags: overwritereadonly ignoreversion
Source: {#OSGHOME}\bin\osgPlugins-2.6.0\osgdb_gif.dll; DestDir: {app}; Components: ; Flags: overwritereadonly ignoreversion
Source: {#OSGHOME}\bin\osgPlugins-2.6.0\osgdb_png.dll; DestDir: {app}; Components: ; Flags: overwritereadonly ignoreversion
Source: {#OSGHOME}\bin\osgPlugins-2.6.0\osgdb_osg.dll; DestDir: {app}; Components: ; Flags: overwritereadonly ignoreversion
Source: {#OSGHOME}\bin\osgPlugins-2.6.0\osgdb_rgb.dll; DestDir: {app}; Components: ; Flags: overwritereadonly ignoreversion
Source: {#OSGHOME}\bin\osgPlugins-2.6.0\osgdb_tiff.dll; DestDir: {app}; Components: ; Flags: overwritereadonly ignoreversion
Source: {#OSGPTEXPORTERHOME}\exp_isu_osg.dll; DestDir: {app}\vcplugin; Components: ; Flags: overwritereadonly ignoreversion
Source: {#OSGPTEXPORTERHOME}\EXP_ISU_OSG.RTI; DestDir: {app}\vcplugin; Components: ; Flags: overwritereadonly ignoreversion

[Icons]
;Name: {group}\NameService; Filename: {app}\{#NameService}; WorkingDir: {app}; Components: nameserver; Flags: runminimized; IconFilename: {app}\images\VE_icon.ico
;Name: {group}\VE-Conductor; Filename: {app}\{#VEConductor}; WorkingDir: {app}; Components: veconductor; IconFilename: {app}\images\VE_icon.ico; Flags: runminimized
;Name: {group}\VE-Xplorer-OSG; Filename: {app}\{#VEXplorerOSG}; WorkingDir: {app}; Components: vexplorer\osg; IconFilename: {app}\images\VE_icon.ico
;Name: {group}\VE-Xplorer-OSG_VEP; Filename: {app}\{#VEXplorerOSGVEP}; WorkingDir: {app}; Components: vexplorer\osgvep; IconFilename: {app}\images\VE_icon.ico
;Name: {group}\VE-Xplorer-OSG_VEPC; Filename: {app}\{#VEXplorerOSGVEPC}; WorkingDir: {app}; Components: vexplorer\osgvepcluster; IconFilename: {app}\images\vesSmallIcon.bmp
;Name: {group}\VE-Xplorer-PF; Filename: {app}\{#VEXplorerPF}; WorkingDir: {app}; Components: vexplorer\pf; IconFilename: {app}\images\vesSmallIcon.bmp
;Name: {group}\VE-Setup; Filename: {app}\{#VESetupScript}; WorkingDir: {app}; IconFilename: {app}\images\VE_icon.ico

;Name: {commondesktop}\NameService; Filename: {app}\bin\NameService.bat; WorkingDir: {app}; Flags: runminimized; Components: nameserver; IconFilename: {app}\images\VE_icon.ico; Tasks: desktopNSIcon
;Name: {commondesktop}\VE-Conductor; Filename: {app}\bin\VE-Conductor.bat; WorkingDir: {app}; Components: veconductor; IconFilename: {app}\images\VE_icon.ico; Flags: runminimized; Tasks: desktopVECIcon
;Name: {commondesktop}\VE-Xplorer-OSG; Filename: {app}\bin\run.osg.bat; WorkingDir: {app}; Components: vexplorer\osg; IconFilename: {app}\images\VE_icon.ico; Tasks: desktopVXOIcon
;Name: {commondesktop}\VE-Xplorer-OSG_VEP; Filename: {app}\bin\run.osg.vep.bat; WorkingDir: {app}; Components: vexplorer\osgvep; IconFilename: {app}\images\VE_icon.ico; Tasks: desktopVXOVEPIcon
;Name: {commondesktop}\VE-Xplorer-OSG_VEPC; Filename: {app}\bin\run.osg.vep.cluster.bat; WorkingDir: {app}; Components: vexplorer\osgvepcluster; IconFilename: {app}\images\VE_icon.ico; Tasks: desktopVXOVEPCIcon
;Name: {commondesktop}\VE-Xplorer-PF; Filename: {app}\bin\run.pf.bat; WorkingDir: {app}; Components: vexplorer\pf; IconFilename: {app}\images\vesSmallIcon.bmp; Tasks: desktopVXPFIcon

;Name: {commondesktop}\VE-Setup; Filename: {app}\{#VESetupScript}; WorkingDir: {app};IconFilename: {app}\images\VE_icon.ico
;Name: {group}\velauncher; Filename: {app}\velauncher.exe; WorkingDir: {app}; Comment: velauncher; Flags: createonlyiffileexists
[Code]
function GetInstallDir(def: string): string;
var
InstallDir : string;
begin
  Result := 'MyDefaultInstallDir';
  //Check for polytrans first
  if RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Okino Computer Graphics, Inc.\PolyTrans','cwd', InstallDir) then begin
    // Successfully read the value.
    Result := InstallDir;
  end else begin
    // Check for nurgaf now
    if RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Okino Computer Graphics, Inc.\NuGraf','cwd', InstallDir) then begin
      // Successfully read the value.
      Result := InstallDir;
    end;
  end;
end;
[UninstallDelete]
Name: {app}\vcplugin\exp_isu_osg.ini; Type: files; Components: 
