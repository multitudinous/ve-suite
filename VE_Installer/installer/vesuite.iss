; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppName "VE-Suite"
#define MyAppVerName "VE-Suite 1.1"
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
DefaultDirName=C:\{#MyAppName}_{#VEVERSION}
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
WizardImageFile={#VEHOME}\VE_Installer\installer\installerImages\velauncher_banner.bmp
WizardImageStretch=false
WizardSmallImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_icon.bmp
WindowVisible=true
WizardImageBackColor=clWhite
ChangesAssociations=true
BackColor=$a16502
BackColor2=$1b84f7
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
Name: buildertools; Description: VE-Suite BuilderTools; Types: full
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
Source: {#VEHOME}\bin\win32\WinClient_d.exe; DestDir: {app}\bin; Components: veconductor; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\WinClient.exe; DestDir: {app}\bin; Components: veconductor; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\project_tao_osg_vep_d.exe; DestDir: {app}\bin; Components: vexplorer; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\project_tao_osg_vep.exe; DestDir: {app}\bin; Components: vexplorer; Flags: ignoreversion
Source: {#VEHOME}\lib\win32\*.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs createallsubdirs
Source: {#VEHOME}\bin\win32\WinServer_d.exe; DestDir: {app}\bin; Components: nameserver; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\WinServer.exe; DestDir: {app}\bin; Components: nameserver; Flags: ignoreversion
Source: {#VEHOME}\VE_Installer\installer\dist\velauncher.exe; DestDir: {app}\bin; Flags: ignoreversion uninsremovereadonly
Source: {#VEHOME}\VE_Installer\installer\README.txt; DestDir: {app}; Flags: isreadme uninsremovereadonly overwritereadonly
Source: {#VEHOME}\VE_TestSuite\brick.vea; DestDir: {app}\share\vesuite\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\simpleScalars\*.vti; DestDir: {app}\share\vesuite\examples\simple\simpleScalars; Components: examples; Flags: recursesubdirs uninsremovereadonly replacesameversion createallsubdirs

Source: {#VEHOME}\VE_TestSuite\gooch.vea; DestDir: {app}\share\vesuite\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\toon.vea; DestDir: {app}\share\vesuite\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\x-ray.vea; DestDir: {app}\share\vesuite\shaders\; Components: examples
Source: {#VEHOME}\VE_TestSuite\2scl.vtu; DestDir: {app}\share\vesuite\examples\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\3scl2vec.vtu; DestDir: {app}\share\vesuite\examples\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\3scl.vtu; DestDir: {app}\share\vesuite\examples\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\eightCorners.stl; DestDir: {app}\share\vesuite\examples\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\Surface0.75.stl; DestDir: {app}\share\vesuite\examples\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\vtkPolyData.vtk; DestDir: {app}\share\vesuite\examples\simple; Components: examples
Source: {#VEHOME}\VE_TestSuite\SURFACE1\*; DestDir: {app}\share\vesuite\examples\simple\SURFACE1\; Components: examples; Flags: recursesubdirs
Source: {#VEHOME}\VE_TestSuite\POST_DATA1\*; DestDir: {app}\share\vesuite\examples\simple\POST_DATA1; Components: examples; Flags: recursesubdirs
Source: {#VEHOME}\VE_TestSuite\POST_DATA2\*; DestDir: {app}\share\vesuite\examples\simple\POST_DATA2; Components: examples; Flags: recursesubdirs
Source: {#VEHOME}\VE_CE\*.h; DestDir: {app}\include\VE_CE; Attribs: readonly; Flags: replacesameversion uninsremovereadonly recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Conductor\*.h; DestDir: {app}\include\VE_Conductor; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Xplorer\*.h; DestDir: {app}\include\VE_Xplorer; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Open\skel\*; DestDir: {app}\include\VE_Open\skel; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#VEHOME}\VE_Installer\include\VEConfig.h; DestDir: {app}\include\VE_Installer\include; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#JUGGLERINSTHOME}\lib\dbghelp.dll; DestDir: {app}\bin; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: veconductor vexplorer nameserver
Source: {#VEHOME}\lib\win32\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: vebuildenv
Source: {#VEHOME}/share/dualhead_configs/*.jconf; DestDir: {app}\share\vesuite\vrj_configs\dualhead_configs; Components: nameserver; Flags: recursesubdirs createallsubdirs
Source: {#VEHOME}/share/stereo_desktop/*.jconf; DestDir: {app}\share\vesuite\vrj_configs\stereo_desktop; Components: nameserver; Flags: recursesubdirs createallsubdirs
Source: {#VEHOME}/share/vecr_configs/*.jconf; DestDir: {app}\share\vesuite\vrj_configs\vecr_configs; Components: nameserver; Flags: recursesubdirs createallsubdirs

Source: {#VEHOME}\VE_Installer\installer\installerImages\ve_logo.xpm; DestDir: {app}\bin\installerImages; Flags: replacesameversion
Source: {#VEHOME}\VE_Installer\installer\installerImages\velauncher_banner.xpm; DestDir: {app}\bin\installerImages; Flags: replacesameversion
;Source: {#VEHOME}\bin\*.dll; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs
Source: {#VEHOME}\VE_Installer\installer\installerImages\VE_icon.ico; DestDir: {app}\bin\installerImages; Flags: replacesameversion
;Source: {#JUGGLERINSTHOME}\lib\ms*.dll; DestDir: {app}\bin; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: veconductor vexplorer nameserver
;Source: {#JUGGLERINSTHOME}\lib\MS*.DLL; DestDir: {app}\bin; Attribs: readonly; Flags: uninsremovereadonly replacesameversion; Components: veconductor vexplorer nameserver
Source: {#VEHOME}\VE_Open\XML\*.h; DestDir: {app}\include\VE_Open\XML; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\VE_Builder\*.h; DestDir: {app}\include\VE_Builder; Attribs: readonly; Flags: uninsremovereadonly replacesameversion recursesubdirs createallsubdirs; Components: vebuildenv
;Source: {#VEHOME}\VE_Installer\installer\dist\MSVCR71.dll; DestDir: {app}; Flags: ignoreversion overwritereadonly
Source: {#VEHOME}\VE_Installer\installer\installerImages\ve_banner_1.0.bmp; DestDir: {app}\bin\installerImages; DestName: velauncher_banner.bmp
;Source: {#VEHOME}\VE_Installer\installer\dist\MSVCR71.dll; DestDir: {app}; Flags: ignoreversion
Source: {#VEHOME}\VE_TestSuite\simple.ves; DestDir: {app}\share\vesuite\examples\simple; Components: examples; Flags: overwritereadonly replacesameversion
Source: {#VEHOME}\VE_Installer\installer\clusterTemplate.txt; DestDir: {app}; Flags: ignoreversion
Source: {#VEHOME}\VE_Installer\installer\VELauncher_Readme.txt; DestDir: {app}; Flags: ignoreversion replacesameversion
Source: {#VEHOME}\VE_Installer\installer\installerImages\ve_logo.xpm; DestDir: {app}\bin\installerImages; DestName: ve_logo.xpm
Source: {#VEHOME}\share\fonts\*.ttf; DestDir: {app}\share\vesuite\fonts; Flags: recursesubdirs createallsubdirs
Source: {#VEHOME}\VE_Installer\installer\vebuildertools{#VEVERSION}_{#SVNVERSION}.exe; DestDir: {tmp}
Source: {#VEHOME}\external\loki-0.1.6\include\*.h; DestDir: {app}\include\loki; Flags: recursesubdirs createallsubdirs; Components: vebuildenv
Source: {#VEHOME}\external\loki-0.1.6\lib\*.lib; DestDir: {app}\lib\win32; Components: vebuildenv

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
Name: {group}\VE-Suite-{#VEVERSION}; Filename: {app}\bin\{#VELauncher}; WorkingDir: {app}; IconFilename: {app}\bin\installerImages\VE_icon.ico
Name: {commondesktop}\VE-Suite-{#VEVERSION}; Filename: {app}\bin\velauncher.exe; WorkingDir: {app}; IconFilename: {app}\bin\installerImages\VE_icon.ico; Tasks: desktopVELauncherIcon

;Name: {commondesktop}\VE-Setup; Filename: {app}\{#VESetupScript}; WorkingDir: {app};IconFilename: {app}\images\VE_icon.ico
;Name: {group}\velauncher; Filename: {app}\velauncher.exe; WorkingDir: {app}; Comment: velauncher; Flags: createonlyiffileexists
[Run]
Filename: {tmp}\vebuildertools{#VEVERSION}_{#SVNVERSION}.exe; WorkingDir: {tmp}; Description: Install VE-BuilderTools; StatusMsg: Installing VE-BuilderTools {#VEVERSION}_{#SVNVERSION}; Flags: postinstall; Components: buildertools; Tasks: 
[_ISToolPreCompile]
Name: .\buildVELauncher.exe.bat; Parameters: 
[_ISTool]
UseAbsolutePaths=false
LogFile=C:\devEnv\VE_Suite_1.0\VE_Installer\installer\compile.log
LogFileAppend=false
[UninstallDelete]
Name: {app}; Type: filesandordirs
