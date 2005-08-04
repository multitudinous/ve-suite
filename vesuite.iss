; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "VE_Suite.0.9.0"
#define MyAppVerName "VE_Suite.0.9.0"
#define MyAppPublisher "Complex Systems Virtural Engineering Group"
#define MyAppURL "www.vrac.iastate.edu/~kmbryden/vesuite"
#define NameService "bin/NameService.bat"
#define VEConductor "bin/VE-Conductor.bat"
#define VEXplorerPF "bin/run.pf.bat"
#define VEXplorerOSG "bin/run.osg.bat"
#define VEXplorerOSGVEP "bin/run.osg.vep.bat"



[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\Program Files\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=C:\devEnv\VE_Suite\VE_Installer\installer
OutputBaseFilename=vesuite_setup
Compression=lzma
SolidCompression=true
ChangesEnvironment=true
ShowLanguageDialog=yes
AlwaysRestart=false

[Types]
Name: full; Description: Full installation
Name: custom; Description: Custom installation; Flags: iscustom


[Components]
Name: nameserver; Description: Name Server; Types: full
Name: vexplorer; Description: VE-Xplorer; Types: full
Name: vexplorer\pf; Description: Performer Graphics; Types: full
;Name: vexplorer\pfcluster; Description: Performer Cluster Graphics; Types: full
Name: vexplorer\osg; Description: OSG Graphics; Types: full
Name: vexplorer\osgvep; Description: OSG VE-Patented Graphics; Types: full
;Name: vexplorer\osgcluster; Description: OSG Cluster Graphics; Types: full
;Name: vexplorer\osgvepcluster; Description: OSG VE-Patented Cluster Graphics; Types: full
Name: veconductor; Description: VE-Conductor (GUI); Types: full
Name: examples; Description: Example datasets; Types: full
[Registry]

[Tasks]
;Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
;Name: quicklaunchicon; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: bin\win32\WinClientd.exe; DestDir: {app}\bin; Components: veconductor; Flags: ignoreversion
Source: bin\win32\project_taod.exe; DestDir: {app}\bin; Components: vexplorer\pf; Flags: ignoreversion
Source: bin\win32\project_tao_osg_d.exe; DestDir: {app}\bin; Components: vexplorer\osg; Flags: ignoreversion
;Source: bin\win32\project_tao_osg_vep_cluster_d.exe; DestDir: {app}\bin; Components: vexplorer\osgvepcluster; Flags: ignoreversion
Source: bin\win32\project_tao_osg_vep_d.exe; DestDir: {app}\bin; Components: vexplorer\osgvep; Flags: ignoreversion
Source: lib\win32\*_d.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: bin\win32\WinServerd.exe; DestDir: {app}\bin; Components: nameserver; Flags: ignoreversion
Source: bin\win32\WinClientd.exe; DestDir: {app}\bin; Components: veconductor; Flags: ignoreversion
Source: VE_Installer\installer\run.pf.bat; DestDir: {app}\bin; Components: vexplorer\pf; Flags: ignoreversion
Source: VE_Installer\installer\run.osg.bat; DestDir: {app}\bin; Components: vexplorer\osg; Flags: confirmoverwrite
Source: VE_Installer\installer\run.osg.vep.bat; DestDir: {app}\bin; Components: vexplorer\osgvep; Flags: confirmoverwrite
Source: VE_Installer\installer\NameService.bat; DestDir: {app}\bin; Components: nameserver; Flags: ignoreversion
Source: VE_Installer\installer\VE-Conductor.bat; DestDir: {app}\bin; Flags: ignoreversion
Source: VE_TextureBased\glsl_shaders\*.glsl; DestDir: {app}\glsl_shaders\; Components: vexplorer\osgvep; Flags: ignoreversion uninsremovereadonly; Attribs: readonly
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: VE_Installer\installer\run.osg.vep.bat; DestDir: {app}\bin; Components: vexplorer\osgvep; Flags: ignoreversion
Source: VE_Installer\installer\setup.bat; DestDir: {app}; Components: vexplorer
Source: VE_Installer\installer\README.txt; DestDir: {app}; Flags: isreadme uninsremovereadonly; Attribs: readonly
Source: VE_TestSuite\sswtexture.param; DestDir: {app}\exampleDatasets; Components: examples
Source: VE_TestSuite\simpleScalars\*; DestDir: {app}\exampleDatasets\simpleScalars\; Flags: recursesubdirs; Components: examples
Source: VE_TestSuite\2scl.vtk; DestDir: {app}\exampleDatasets\; Components: examples
Source: VE_TestSuite\3scl2vec.vtk; DestDir: {app}\exampleDatasets\; Components: examples
Source: VE_TestSuite\3scl.vtk; DestDir: {app}\exampleDatasets\; Components: examples
Source: VE_TestSuite\200_to_1000.txt; DestDir: {app}\exampleDatasets\; Components: examples
Source: VE_TestSuite\eightCorners.stl; DestDir: {app}\exampleDatasets\; Components: examples
Source: VE_TestSuite\first-scalar.txt; DestDir: {app}\exampleDatasets\; Components: examples
Source: VE_TestSuite\fourth_param_is_a_scalar.txt; DestDir: {app}\exampleDatasets\; Components: examples
Source: VE_TestSuite\sswtexture.param; DestDir: {app}\exampleDatasets\; DestName: sample.param
Source: VE_TestSuite\steve's_vector.txt; DestDir: {app}\exampleDatasets\; Components: examples
Source: VE_TestSuite\Surface0.75.stl; DestDir: {app}\exampleDatasets\; Components: examples
Source: VE_TestSuite\vtkPolyData.vtk; DestDir: {app}\exampleDatasets; Components: examples
Source: VE_TestSuite\SURFACE1\*; DestDir: {app}\exampleDatasets\SURFACE1\; Components: examples; Flags: recursesubdirs
Source: VE_TestSuite\POST_DATA1\*; DestDir: {app}\exampleDatasets\POST_DATA1; Components: examples; Flags: recursesubdirs
Source: VE_TestSuite\POST_DATA2\*; DestDir: {app}\\exampleDatasets\POST_DATA2; Components: examples; Flags: recursesubdirs
[Icons]
Name: {group}\NameService; Filename: {app}\{#NameService}; WorkingDir: {app}; Components: nameserver
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}

Name: {group}\VE-Conductor; Filename: {app}\{#VEConductor}; WorkingDir: {app}; Components: veconductor

Name: {group}\Xplorer-OSG; Filename: {app}\{#VEXplorerOSG}; WorkingDir: {app}; Components: vexplorer\osg

Name: {group}\Xplorer-OSG_VEP; Filename: {app}\{#VEXplorerOSGVEP}; WorkingDir: {app}; Components: vexplorer\osgvep

Name: {group}\Xplorer-PF; Filename: {app}\{#VEXplorerPF}; WorkingDir: {app}; Components: vexplorer\pf
