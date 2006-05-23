; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "VE-Builder Tools 1.0.0"
#define MyAppVerName "VE-Builder Tools 1.0.0"
#define MyAppPublisher "Virtual Engineering Research Group"
#define MyAppURL "www.vesuite.org"
#define BuilderShell "bin/buildershell.bat"
#include <vesenv.iss>
[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\VE_BuilderTools.1.0.0
DefaultGroupName=VE-Builder_Tools 1.0.0
AllowNoIcons=yes
OutputBaseFilename=vebuildertools{#VEVERSION}
Compression=lzma
SolidCompression=true
WindowVisible=true
WizardImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_suite_banner.bmp
WizardSmallImageFile={#VEHOME}\VE_Installer\installer\installerImages\icons.bmp
WizardImageStretch=false
UninstallFilesDir={app}\bin
OutputDir={#VEHOME}\VE_Installer\installer
[Types]
;Name: optional; Description: VE-Util Library
[Components]
;Name: veutil; Description: VE-Util Library (Optional); Types: optional
[Languages]
Name: eng; MessagesFile: compiler:Default.isl

[Files]
Source: {#VEHOME}\lib\win32\VE_UtilLib_d.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\lib\win32\DataLoader_d.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\lib\win32\cfdTranslatorToVTK_d.dll; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\makeVtkSurface.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\createMiniFlowdata.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\convertVTK2Binary.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\convertVTK2Ascii.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\convertSurfaceFileToStl.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\convertCellDataToPointData.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\compareScalars.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\mergeVertices.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\loaderToVTKd.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\appendVTK.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\addNormals2Poly.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\Translator.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\Transient_Tools.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\Preprocessor.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\moveFieldToPointData.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\removeVtkCellsOutsideBox.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\removeVtkPointData.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\scaleVTK.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\transformVTK.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\combineFluentParticleFile.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\whatIsScalarRange.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\meshViewer.exe; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\bin\win32\vtkTo3DTextured.exe; DestDir: {app}\bin; Flags: ignoreversion; DestName: vtkTo3DTexture.exe
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: {#VEHOME}\bin\win32\AVSTranslatord.exe; DestDir: {app}\bin; Flags: ignoreversion; DestName: AVSTranslator.exe
Source: {#VEHOME}\bin\win32\cfdDICOMTranslator_d.exe; DestDir: {app}\bin; Flags: ignoreversion; DestName: cfdDICOMTranslator.exe
Source: {#VEHOME}\VE_Installer\installer\setup.bat; DestDir: {app}
Source: {#VEHOME}\VE_Installer\installer\buildershell.bat; DestDir: {app}\bin; Flags: ignoreversion
Source: {#VEHOME}\VE_Installer\installer\installerImages\icons.bmp; DestDir: {app}\images; DestName: vesSmallIcon.bmp
Source: {#VEHOME}\VE_Installer\installer\vebuilder.txt; DestDir: {app}; Attribs: readonly; Flags: isreadme overwritereadonly uninsremovereadonly; DestName: README.txt

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
Name: {group}\VE-Builder_Shell; Filename: {app}\{#BuilderShell}; WorkingDir: {app}; IconFilename: {app}\images\vesSmallIcon.bmp
Name: {userdesktop}\VE-Builder_Shell; Filename: {app}\{#BuilderShell}; WorkingDir: {app}; IconFilename: {app}\images\vesSmallIcon.bmp
[Tasks]
Name: BuilderShell; Description: VE-Builder Tools Shell; GroupDescription: Create Desktop Icon; Flags: unchecked
