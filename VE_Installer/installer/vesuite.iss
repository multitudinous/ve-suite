; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppName "VE_Suite.1.0.0"
#define MyAppVerName "VE_Suite.1.0.0"
#define MyAppPublisher "Virtural Engineering Research Group"
#define MyAppURL "www.vesuite.org"
#define NameService "bin/NameService.bat"
#define VEConductor "bin/VE-Conductor.bat"
#define VEXplorerPF "bin/run.pf.bat"
#define VEXplorerOSG "bin/run.osg.bat"
#define VEXplorerOSGVEP "bin/run.osg.vep.bat"
#define VEXplorerOSGVEPC "bin/run.osg.vep.cluster.bat"
#define VESetupScript "setup.bat"
#define VELauncher "velauncher.py"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=true
OutputDir={#VEHOME}\VE_Installer\installer
OutputBaseFilename=vesuite{#VEVERSION}
Compression=lzma
SolidCompression=true
ChangesEnvironment=false
ShowLanguageDialog=yes
AlwaysRestart=false
UninstallFilesDir={app}\bin
UninstallRestartComputer=false
WizardImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_suite_banner.bmp
WizardImageStretch=false
WizardSmallImageFile={#VEHOME}\VE_Installer\installer\installerImages\icons.bmp
WindowVisible=true

[Types]
Name: full; Description: Full installation
Name: custom; Description: Custom installation; Flags: iscustom


[Components]
Name: nameserver; Description: Name Server; Types: full
Name: vexplorer; Description: VE-Xplorer; Types: full
;Name: vexplorer\pf; Description: Performer Graphics; Types: full
;Name: vexplorer\pfcluster; Description: Performer Cluster Graphics; Types: full
Name: vexplorer\osg; Description: OSG Graphics; Types: full
Name: vexplorer\osgvep; Description: OSG VE-Patented Graphics; Types: full
;Name: vexplorer\osgcluster; Description: OSG Cluster Graphics; Types: full
Name: vexplorer\osgvepcluster; Description: OSG VE-Patented Cluster Graphics; Types: full
Name: veconductor; Description: VE-Conductor (GUI); Types: full
Name: vebuildenv; Description: Headers and Libs; Types: full
Name: examples; Description: Example datasets; Types: full
Name: velauncher; Description: wxPython Launcher (Requires wxPython to be installed); Types: full
[Registry]
Root: HKCU; Subkey: Software\VE-Suite-Launcher; ValueType: none; Flags: uninsdeletekey
Root: HKCU; Subkey: Software\VE-Conductor; ValueType: none; Flags: uninsdeletekey; Components: ; Tasks: 

[Tasks]
Name: desktopNSIcon; Description: Name Server; GroupDescription: Create Desktop Icon; Flags: unchecked; Components: nameserver
Name: desktopVECIcon; Description: VE-Conductor (GUI); GroupDescription: Create Desktop Icon; Flags: unchecked; Components: veconductor
Name: desktopVXOIcon; Description: VE-Xplorer-OSG; GroupDescription: Create Desktop Icon; Flags: unchecked; Components: vexplorer\osg
Name: desktopVXOVEPIcon; Description: VE-Xplorer-OSG_VEP; GroupDescription: Create Desktop Icon; Flags: unchecked; Components: vexplorer\osgvep
Name: desktopVXOVEPCIcon; Description: VE-Xplorer-OSG_VEPC; GroupDescription: Create Desktop Icon; Flags: unchecked; Components: vexplorer\osgvepcluster
;Name: desktopVXPFIcon; Description: VE-Xplorer-PF; GroupDescription: Create Desktop Icon; Flags: unchecked; Components: vexplorer\pf
Name: desktopVELauncherIcon; Description: VE-Launcher(Requires wxPython Install); GroupDescription: Create Desktop Icon; Flags: unchecked; Components: velauncher

;Name: startMenuNSIcon; Description: Name Server; GroupDescription: Create StartMenu Icon; Flags: unchecked; Components: nameserver
;Name: startMenuVECIcon; Description: VE-Conductor (GUI); GroupDescription: Create StartMenu Icon; Flags: unchecked; Components: veconductor
;Name: startMenuVXOIcon; Description: VE-Xplorer-OSG; GroupDescription: Create StartMenu Icon; Flags: unchecked; Components: vexplorer\osg
;Name: startMenuVXOVEPIcon; Description: VE-Xplorer-OSG_VEP; GroupDescription: Create StartMenu Icon; Flags: unchecked; Components: vexplorer\osgvep
;Name: startMenuVXOVEPCIcon; Description: VE-Xplorer-OSG_VEPC; GroupDescription: Create StartMenu Icon; Flags: unchecked; Components: vexplorer\osgvepcluster
;Name: startMenuVXPFIcon; Description: VE-Xplorer-PF; GroupDescription: Create StartMenu Icon; Flags: unchecked; Components: vexplorer\pf

;Name: quicklaunchicon; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: {#VEHOME}\bin\win32\WinClientd.exe; DestDir: {app}\bin; Components: veconductor; Flags: ignoreversion
;Source: {#VEHOME}\bin\win32\project_taod.exe; DestDir: {app}\bin; Components: vexplorer\pf; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\project_tao_osg_d.exe; DestDir: {app}\bin; Components: vexplorer\osg; Flags: ignoreversion
;Source: {#VEHOME}\bin\win32\project_tao_osg_vep_cluster_d.exe; DestDir: {app}\bin; Components: vexplorer\osgvepcluster; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\project_tao_osg_vep_d.exe; DestDir: {app}\bin; Components: vexplorer\osgvep; Flags: ignoreversion
Source: {#VEHOME}\lib\win32\*_d.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VEHOME}\bin\win32\WinServerd.exe; DestDir: {app}\bin; Components: nameserver; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\WinClientd.exe; DestDir: {app}\bin; Components: veconductor; Flags: ignoreversion
;Source: {#VEHOME}\VE_Installer\installer\run.pf.bat; DestDir: {app}\bin; Components: vexplorer\pf; Flags: ignoreversion confirmoverwrite
Source: {#VEHOME}\VE_Installer\installer\run.osg.bat; DestDir: {app}\bin; Components: vexplorer\osg; Flags: confirmoverwrite
Source: {#VEHOME}\VE_Installer\installer\NameService.bat; DestDir: {app}\bin; Components: nameserver; Flags: ignoreversion
Source: {#VEHOME}\VE_Installer\installer\VE-Conductor.bat; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\VE_Installer\installer\velauncher.py; DestDir: {app}; Flags: ignoreversion; Components: velauncher
;Source: {#VEHOME}\VE_TextureBased\glsl_shaders\*.glsl; DestDir: {app}\glsl_shaders\; Components: vexplorer\osgvep; Flags: ignoreversion uninsremovereadonly overwritereadonly; Attribs: readonly hidden
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: {#VEHOME}\VE_Installer\installer\run.osg.vep.bat; DestDir: {app}\bin; Components: vexplorer\osgvep; Flags: ignoreversion confirmoverwrite
Source: {#VEHOME}\VE_Installer\installer\run.osg.vep.cluster.bat; DestDir: {app}\bin; Components: vexplorer\osgvepcluster; Flags: ignoreversion confirmoverwrite
Source: {#VEHOME}\VE_Installer\installer\setup.bat; DestDir: {app}; Flags: confirmoverwrite
Source: {#VEHOME}\VE_Installer\installer\README.txt; DestDir: {app}; Flags: isreadme uninsremovereadonly overwritereadonly
Source: {#VEHOME}\VE_TestSuite\brick.vea; DestDir: {app}\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\gooch.vea; DestDir: {app}\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\toon.vea; DestDir: {app}\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\x-ray.vea; DestDir: {app}\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\2scl.vtu; DestDir: {app}\exampleDatasets\; Components: examples
Source: {#VEHOME}\VE_TestSuite\3scl2vec.vtu; DestDir: {app}\exampleDatasets\; Components: examples
Source: {#VEHOME}\VE_TestSuite\3scl.vtu; DestDir: {app}\exampleDatasets\; Components: examples
Source: {#VEHOME}\VE_TestSuite\eightCorners.stl; DestDir: {app}\exampleDatasets\; Components: examples
Source: {#VEHOME}\VE_TestSuite\Surface0.75.stl; DestDir: {app}\exampleDatasets\; Components: examples
Source: {#VEHOME}\VE_TestSuite\vtkPolyData.vtk; DestDir: {app}\exampleDatasets; Components: examples
Source: {#VEHOME}\VE_TestSuite\SURFACE1\*; DestDir: {app}\exampleDatasets\SURFACE1\; Components: examples; Flags: recursesubdirs
Source: {#VEHOME}\VE_TestSuite\POST_DATA1\*; DestDir: {app}\exampleDatasets\POST_DATA1; Components: examples; Flags: recursesubdirs
Source: {#VEHOME}\VE_TestSuite\POST_DATA2\*; DestDir: {app}\\exampleDatasets\POST_DATA2; Components: examples; Flags: recursesubdirs
Source: {#VEHOME}\VE_Installer\installer\installerImages\icons.bmp; DestDir: {app}\images; DestName: vesSmallIcon.bmp
Source: {#VEHOME}\bin\win32\project_tao_osg_vep_cluster_d.exe; DestDir: {app}\bin; Flags: ignoreversion; Components: vexplorer\osgvepcluster
Source: {#VEHOME}\bin\win32\WinClientd.exe.manifest; DestDir: {app}\bin
;Source: {#VEHOME}\examples\Model\*; DestDir: {app}\examplePlugins; Flags: overwritereadonly recursesubdirs uninsremovereadonly
Source: {#VEHOME}\VE_CE\*.h; DestDir: {app}\include\VE_CE; Attribs: readonly; Flags: replacesameversion uninsremovereadonly recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Conductor\*.h; DestDir: {app}\include\VE_Conductor; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Xplorer\*.h; DestDir: {app}\include\VE_Xplorer; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Open\skel\*; DestDir: {app}\include\VE_Open\skel; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#VEHOME}\VE_Installer\include\VEConfig.h; DestDir: {app}\include\VE_Installer\include; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#JUGGLERINSTHOME}\lib\dbghelp.dll; DestDir: {app}\bin; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: veconductor vexplorer\osgvepcluster vexplorer\osgvep vexplorer\osg vexplorer nameserver
Source: {#VEHOME}\lib\win32\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#VEHOME}\VE_Xplorer/stereo_desktop/*.jconf; DestDir: {app}\stereo_desktop; Components: nameserver; Flags: ignoreversion recursesubdirs
Source: {#VEHOME}\VE_Installer\installer\dist\*; DestDir: {app}; Flags: ignoreversion; Components: velauncher
Source: {#VEHOME}\VE_Installer\installer\ve_logo.xpm; DestDir: {app}; Flags: replacesameversion
Source: {#VEHOME}\bin\*_d.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
[Icons]
Name: {group}\NameService; Filename: {app}\{#NameService}; WorkingDir: {app}; Components: nameserver; Flags: runminimized; IconFilename: {app}\images\vesSmallIcon.bmp
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
Name: {group}\VE-Conductor; Filename: {app}\{#VEConductor}; WorkingDir: {app}; Components: veconductor; IconFilename: {app}\images\vesSmallIcon.bmp; Flags: runminimized
Name: {group}\VE-Xplorer-OSG; Filename: {app}\{#VEXplorerOSG}; WorkingDir: {app}; Components: vexplorer\osg; IconFilename: {app}\images\vesSmallIcon.bmp
Name: {group}\VE-Xplorer-OSG_VEP; Filename: {app}\{#VEXplorerOSGVEP}; WorkingDir: {app}; Components: vexplorer\osgvep; IconFilename: {app}\images\vesSmallIcon.bmp
Name: {group}\VE-Xplorer-OSG_VEPC; Filename: {app}\{#VEXplorerOSGVEPC}; WorkingDir: {app}; Components: vexplorer\osgvepcluster; IconFilename: {app}\images\vesSmallIcon.bmp
;Name: {group}\VE-Xplorer-PF; Filename: {app}\{#VEXplorerPF}; WorkingDir: {app}; Components: vexplorer\pf; IconFilename: {app}\images\vesSmallIcon.bmp
Name: {group}\VE-Launcher; Filename: {app}\{#VELauncher}; WorkingDir: {app}; IconFilename: {app}\images\vesSmallIcon.bmp; Components: velauncher
Name: {group}\VE-Setup; Filename: {app}\{#VESetupScript}; WorkingDir: {app}; IconFilename: {app}\images\vesSmallIcon.bmp

Name: {commondesktop}\NameService; Filename: {app}\bin\NameService.bat; WorkingDir: {app}; Flags: runminimized; Components: nameserver; IconFilename: {app}\images\vesSmallIcon.bmp; Tasks: desktopNSIcon
Name: {commondesktop}\VE-Conductor; Filename: {app}\bin\VE-Conductor.bat; WorkingDir: {app}; Components: veconductor; IconFilename: {app}\images\vesSmallIcon.bmp; Flags: runminimized; Tasks: desktopVECIcon
Name: {commondesktop}\VE-Xplorer-OSG; Filename: {app}\bin\run.osg.bat; WorkingDir: {app}; Components: vexplorer\osg; IconFilename: {app}\images\vesSmallIcon.bmp; Tasks: desktopVXOIcon
Name: {commondesktop}\VE-Xplorer-OSG_VEP; Filename: {app}\bin\run.osg.vep.bat; WorkingDir: {app}; Components: vexplorer\osgvep; IconFilename: {app}\images\vesSmallIcon.bmp; Tasks: desktopVXOVEPIcon
Name: {commondesktop}\VE-Xplorer-OSG_VEPC; Filename: {app}\bin\run.osg.vep.cluster.bat; WorkingDir: {app}; Components: vexplorer\osgvepcluster; IconFilename: {app}\images\vesSmallIcon.bmp; Tasks: desktopVXOVEPCIcon
;Name: {commondesktop}\VE-Xplorer-PF; Filename: {app}\bin\run.pf.bat; WorkingDir: {app}; Components: vexplorer\pf; IconFilename: {app}\images\vesSmallIcon.bmp; Tasks: desktopVXPFIcon
Name: {commondesktop}\VE-Launcher; Filename: {app}\velauncher.py; WorkingDir: {app}; IconFilename: {app}\images\vesSmallIcon.bmp; Tasks: desktopVELauncherIcon

;Name: {commondesktop}\VE-Setup; Filename: {app}\{#VESetupScript}; WorkingDir: {app};IconFilename: {app}\images\vesSmallIcon.bmp
