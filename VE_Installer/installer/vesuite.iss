; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppName "VE_Suite.1.0.3"
#define MyAppVerName "VE_Suite.1.0.3"
#define MyAppPublisher "Virtural Engineering Research Group"
#define MyAppURL "www.vesuite.org"
#define NameService "bin/NameService.bat"
#define VEConductor "bin/VE-Conductor.bat"
#define VEXplorerPF "bin/run.pf.bat"
#define VEXplorerOSG "bin/run.osg.bat"
#define VEXplorerOSGVEP "bin/run.osg.vep.bat"
#define VEXplorerOSGVEPC "bin/run.osg.vep.cluster.bat"
#define VESetupScript "setup.bat"
#define VELauncher "velauncher.exe"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}_{#SVNVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=true
OutputDir={#VEHOME}\VE_Installer\installer
OutputBaseFilename=vesuite{#VEVERSION}_{#SVNVERSION}
Compression=lzma
SolidCompression=true
ChangesEnvironment=true
ShowLanguageDialog=yes
AlwaysRestart=false
UninstallFilesDir={app}\bin
UninstallRestartComputer=false
WizardImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_banner_1.0.bmp
WizardImageStretch=false
WizardSmallImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_icon.bmp
WindowVisible=true
WizardImageBackColor=clGray
ChangesAssociations=true
BackColor=clBlack
BackColor2=$0080ff
SetupIconFile={#VEHOME}\VE_Installer\installer\installerImages\Ve_icon.ico

[Types]
Name: full; Description: Full installation
Name: custom; Description: Custom installation; Flags: iscustom


[Components]
Name: nameserver; Description: Name Server; Types: full
Name: vexplorer; Description: VE-Xplorer; Types: full
Name: veconductor; Description: VE-Conductor (GUI); Types: full
Name: vebuildenv; Description: Headers and Libs; Types: full
Name: examples; Description: Example datasets; Types: full
[Registry]
Root: HKCU; Subkey: Software\VE-Suite-Launcher; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\VE-Conductor; ValueType: none; Components: " examples vebuildenv veconductor vexplorer nameserver"; Tasks: " desktopVELauncherIcon"; Flags: uninsdeletekeyifempty
Root: HKCR; SubKey: .ves; ValueType: string; ValueData: VESNetworkfile; Flags: uninsdeletekey createvalueifdoesntexist
Root: HKCR; SubKey: VESNetworkfile; ValueType: string; ValueData: VE-Suite Network file; Flags: uninsdeletekey
Root: HKCR; SubKey: VESNetworkfile\shell\open\command; ValueType: string; ValueData: """{app}\bin\velauncher.exe"" ""%1"""; Flags: uninsdeletevalue
Root: HKCR; Subkey: VESNetworkfile\DefaultIcon; ValueType: string; ValueData: {app}\share\installerImages\VE_icon.ico; Flags: uninsdeletevalue; Components: ; Tasks: 
Root: HKCR; Subkey: VESNetworkfile\shell\OpenWithVELauncher; ValueType: string; ValueData: Open with &VE-Launcher
Root: HKCR; Subkey: VESNetworkfile\shell\OpenWithVELauncher\command; ValueType: string; ValueData: """{app}\bin\velauncher.exe"" ""%1"""; Flags: createvalueifdoesntexist uninsdeletekey
[Tasks]
Name: desktopVELauncherIcon; Description: VE-Launcher; GroupDescription: Create Desktop Icon

[Files]
Source: {#VEHOME}\bin\win32\WinClientd.exe; DestDir: {app}\bin; Components: veconductor; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\project_tao_osg_vep_d.exe; DestDir: {app}\bin; Components: vexplorer; Flags: ignoreversion
Source: {#VEHOME}\lib\win32\*.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VEHOME}\bin\win32\WinServerd.exe; DestDir: {app}\bin; Components: nameserver; Flags: ignoreversion
Source: {#VEHOME}\VE_Installer\installer\dist\velauncher.exe; DestDir: {app}\bin; Flags: ignoreversion uninsremovereadonly
Source: {#VEHOME}\VE_Installer\installer\README.txt; DestDir: {app}; Flags: isreadme uninsremovereadonly overwritereadonly
Source: {#VEHOME}\VE_TestSuite\brick.vea; DestDir: {app}\share\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\simpleScalars\*.vti; DestDir: {app}\share\exampleDatasets\simple\texture-based; Components: examples; Flags: recursesubdirs uninsremovereadonly replacesameversion createallsubdirs

Source: {#VEHOME}\VE_TestSuite\gooch.vea; DestDir: {app}\share\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\toon.vea; DestDir: {app}\share\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\x-ray.vea; DestDir: {app}\share\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\2scl.vtu; DestDir: {app}\share\exampleDatasets\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\3scl2vec.vtu; DestDir: {app}\share\exampleDatasets\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\3scl.vtu; DestDir: {app}\share\exampleDatasets\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\eightCorners.stl; DestDir: {app}\share\exampleDatasets\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\Surface0.75.stl; DestDir: {app}\share\exampleDatasets\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\vtkPolyData.vtk; DestDir: {app}\share\exampleDatasets\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\SURFACE1\*; DestDir: {app}\share\exampleDatasets\simple\SURFACE1\; Components: examples; Flags: recursesubdirs
Source: {#VEHOME}\VE_TestSuite\POST_DATA1\*; DestDir: {app}\share\exampleDatasets\simple\POST_DATA1; Components: examples; Flags: recursesubdirs
Source: {#VEHOME}\VE_TestSuite\POST_DATA2\*; DestDir: {app}\share\exampleDatasets\simple\POST_DATA2; Components: examples; Flags: recursesubdirs
Source: {#VEHOME}\VE_CE\*.h; DestDir: {app}\include\VE_CE; Attribs: readonly; Flags: replacesameversion uninsremovereadonly recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Conductor\*.h; DestDir: {app}\include\VE_Conductor; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Xplorer\*.h; DestDir: {app}\include\VE_Xplorer; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Open\skel\*; DestDir: {app}\include\VE_Open\skel; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#VEHOME}\VE_Installer\include\VEConfig.h; DestDir: {app}\include\VE_Installer\include; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#JUGGLERINSTHOME}\lib\dbghelp.dll; DestDir: {app}\bin; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: veconductor vexplorer nameserver
Source: {#VEHOME}\lib\win32\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#VEHOME}\VE_Xplorer/stereo_desktop/*.jconf; DestDir: {app}\share\stereo_desktop; Components: nameserver; Flags: ignoreversion recursesubdirs
Source: {#VEHOME}\VE_Installer\installer\installerImages\ve_logo.xpm; DestDir: {app}\share\installerImages; Flags: replacesameversion
Source: {#VEHOME}\bin\*.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs
Source: {#VEHOME}\VE_Installer\installer\installerImages\VE_icon.ico; DestDir: {app}\share\installerImages; Flags: replacesameversion
Source: {#JUGGLERINSTHOME}\lib\ms*.dll; DestDir: {app}\bin; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: veconductor vexplorer nameserver
Source: {#JUGGLERINSTHOME}\lib\MS*.DLL; DestDir: {app}\bin; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: veconductor vexplorer nameserver
Source: {#VEHOME}\VE_Open\XML\*.h; DestDir: {app}\include\VE_Open\XML; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Builder\*.h; DestDir: {app}\include\VE_Builder; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Installer\installer\dist\MSVCR71.dll; DestDir: {app}; Flags: ignoreversion overwritereadonly
Source: {#VEHOME}\VE_Installer\installer\installerImages\ve_banner_1.0.bmp; DestDir: {app}\installerImages; DestName: velauncher_banner.bmp
Source: {#VEHOME}\VE_Xplorer/dualhead_configs/*.jconf; DestDir: {app}/share/dualhead_configs/; Components: nameserver; Flags: ignoreversion recursesubdirs
Source: {#VEHOME}\VE_Installer\installer\dist\MSVCR71.dll; DestDir: {app}; Flags: ignoreversion
Source: {#VEHOME}\VE_Installer\installer\simple.ves; DestDir: {app}\share\exampleDatasets\simple; Components: examples; Flags: overwritereadonly replacesameversion
Source: {#VEHOME}\VE_Installer\installer\clusterTemplate.txt; DestDir: {app}; Flags: ignoreversion
Source: {#VEHOME}\VE_Installer\installer\VELauncher_Readme.txt; DestDir: {app}; Flags: ignoreversion replacesameversion
Source: {#VEHOME}\VE_Installer\installer\installerImages\ve_logo.xpm; DestDir: {app}\share\installerImages; DestName: ve_logo.xpm

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
Name: {group}\VE-Suite-{#VEVERSION}; Filename: {app}\{#VELauncher}; WorkingDir: {app}; IconFilename: {app}\share\installerImages\VE_icon.ico
Name: {commondesktop}\VE-Suite-{#VEVERSION}; Filename: {app}\bin\velauncher.exe; WorkingDir: {app}; IconFilename: {app}\share\installerImages\VE_icon.ico; Tasks: desktopVELauncherIcon

;Name: {commondesktop}\VE-Setup; Filename: {app}\{#VESetupScript}; WorkingDir: {app};IconFilename: {app}\images\VE_icon.ico
;Name: {group}\velauncher; Filename: {app}\velauncher.exe; WorkingDir: {app}; Comment: velauncher; Flags: createonlyiffileexists
