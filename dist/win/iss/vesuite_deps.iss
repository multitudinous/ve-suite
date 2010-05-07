; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppName "VE-Suite_Dependencies"
#define MyAppVerName "VE-Suite-2.1.0_Dependencies"
#define MyAppPublisher "Virtual Engineering Research Group"
#define MyAppURL "www.vesuite.org"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppName}_{#VEVERSION}_{#MSVCVERSION}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\{#MyAppName}_{#VEVERSION}_{#MSVCVERSION}
DefaultGroupName={#VESGROUPNAME}\Uninstallers_{#MSVCVERSION}
AllowNoIcons=true
OutputBaseFilename={#MyAppName}_{#VEVERSION}_{#MSVCVERSION}
Compression=lzma
SolidCompression=true
OutputDir={#INSTALLERINSTALLLOCATION}
WizardImageFile={#VEINSTALLHOME}\bin\installerImages\velauncher_banner.bmp
WizardSmallImageFile={#VEINSTALLHOME}\bin\installerImages\ve_icon.bmp
WindowVisible=true
WizardImageStretch=false
WizardImageBackColor=clWhite
BackColor=$a16502
BackColor2=$1b84f7
SetupIconFile={#VEINSTALLHOME}\bin\installerImages\ves_icon.ico
PrivilegesRequired=none
UsePreviousGroup=false
UsePreviousAppDir=false

[Languages]
Name: eng; MessagesFile: compiler:Default.isl
[Types]
Name: full; Description: Full installation
Name: custom; Description: Custom installation; Flags: iscustom

[Components]
Name: vtk; Description: Visualiztion ToolKit; Types: full
Name: acetao; Description: ACE/TAO; Types: full
Name: wxwidgets; Description: wxWidgets; Types: full
Name: xercesc; Description: Apache Xerces-C++; Types: full
Name: osg; Description: OpenSceneGraph; Types: full
Name: juggler; Description: VR Juggler; Types: full
;Name: apr; Description: Apache APR; Types: full
Name: osgal; Description: osgAL; Types: full
Name: minerva; Description: Minerva; Types: full
Name: poco; Description: POCO; Types: full
Name: osgworks; Description: osgWorks; Types: full
Name: depsbuildenv; Description: Headers and Libs

[Files]
; OSG files
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\lib\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\bin\osgviewer*.exe; DestDir: {app}\bin; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: osg
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\bin\osgconv*.exe; DestDir: {app}\bin; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: osg
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\lib\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion recursesubdirs createallsubdirs

; VTK Files
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion; Components: vtk; Languages: 
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv

; WX Files
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\include\*.h; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\*.lib; DestDir: {app}\lib; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv; Attribs: readonly
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\*.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\*.h; DestDir: {app}\include\wx; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv

; Juggler Files
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\bin\*; DestDir: {app}\bin; Flags: ignoreversion recursesubdirs; Components: juggler
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs; Components: depsbuildenv; Languages: 
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs; Components: juggler
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\share\*; DestDir: {app}\share; Flags: ignoreversion recursesubdirs createallsubdirs; Components: juggler
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs; Components: depsbuildenv; Languages: 

; Xerces Files
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\include\*.h*; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion recursesubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\include\*.c*; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion recursesubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\lib\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\lib\*.dll; DestDir: {app}\lib; Components: xercesc; Flags: ignoreversion

; APR Files
;Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\lib\libapr*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion skipifsourcedoesntexist; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\lib\libaprutil*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion skipifsourcedoesntexist; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\lib\libapriconv*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion skipifsourcedoesntexist; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\lib\*.dll; DestDir: {app}\lib; Components: apr; Flags: ignoreversion skipifsourcedoesntexist
;Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\lib\*.dll; DestDir: {app}\lib; Components: apr; Flags: ignoreversion skipifsourcedoesntexist
;Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\lib\*.dll; DestDir: {app}\lib; Components: apr; Flags: ignoreversion skipifsourcedoesntexist

; ACE/TAO Files
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion; Components: acetao
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\bin\tao_idl.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\bin\Naming_Service.exe; DestDir: {app}\bin; Components: acetao
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\include\ace\*.h; DestDir: {app}\include\ace; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\include\tao\utils\*.h; DestDir: {app}\include\tao\utils; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\lib\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: recursesubdirs overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\include\ace\*.inl; DestDir: {app}\include\ace; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\include\tao\*.inl; DestDir: {app}\include\tao; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\include\ace\*.cpp; DestDir: {app}\include\ace; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\include\tao\tao\*.cpp; DestDir: {app}\include\tao\tao; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\include\tao\TAO_IDL\*.h; DestDir: {app}\include\TAO_IDL; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\include\tao\orbsvcs\*.h; DestDir: {app}\include\orbsvcs; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\include\tao\tao\*.h; DestDir: {app}\include\tao; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv

; osgAL Files
Source: {#DEPENDSINSTALLHOME}\{#OSGALHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGALHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion skipifsourcedoesntexist; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGALHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion skipifsourcedoesntexist; Components: osgal; Languages: 

; Minerva files
Source: {#DEPENDSINSTALLHOME}\{#MINERVAHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#MINERVAHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion skipifsourcedoesntexist; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#MINERVAHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion skipifsourcedoesntexist; Components: minerva
Source: {#DEPENDSINSTALLHOME}\{#MINERVAHOME}\lib\*.plug; DestDir: {app}\lib; Flags: ignoreversion skipifsourcedoesntexist; Components: minerva
Source: {#DEPENDSINSTALLHOME}\{#MINERVAHOME}\share\*; DestDir: {app}\share; Flags: ignoreversion skipifsourcedoesntexist; Components: minerva

; POCO Files
Source: {#DEPENDSINSTALLHOME}\{#POCOHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#POCOHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion skipifsourcedoesntexist; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#POCOHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion skipifsourcedoesntexist; Components: poco; Languages: 

; osgWorks Files
Source: {#DEPENDSINSTALLHOME}\{#OSGWORKSHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGWORKSHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGWORKSHOME}\lib\*.fpc; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGWORKSHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion; Components: osgworks
Source: {#DEPENDSINSTALLHOME}\{#OSGWORKSHOME}\bin\*; DestDir: {app}\bin; Flags: ignoreversion; Components: osgworks; Languages: 

; README setup for depends info
; Source: {#VEDEVHOME}\dist\win\iss\dependencies.txt; DestDir: {app}; Flags: isreadme; DestName: README.txt

; Old installer files
;Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
;Source: {#OSGHOME}\..\Producer\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
;Source: {#OSGHOME}\..\OpenThreads\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
;Source: {#COINHOME}\include\*.h; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion recursesubdirs; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxbase28d_vc_custom.dll; DestDir: {app}\lib; Flags: ignoreversion; Components: wxwidgets
;Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxmsw28d_core_vc_custom.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
;Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxmsw28d_gl_vc_custom.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
;Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxmsw28d_adv_vc_custom.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\aced.dll; DestDir: {app}\lib; Components: acetao; Flags: ignoreversion
;Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\Build\Win32\VC7.1\Debug\xerces-c_2_7D.dll; DestDir: {app}\lib; Components: xercesc; Flags: ignoreversion
;ource: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\Build\Win32\VC7.1\Debug\xerces-depdom_2_7D.dll; DestDir: {app}\lib; Components: xercesc; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
;Source: {#COINHOME}\bin\coin*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion
;Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\lib\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#COINHOME}\lib\*.lib; DestDir: {app}\lib; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\Debug\libapr*_d.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\Debug\libaprutil*_d.lib; DestDir: {app}\lib; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\Debug\libapriconv*_d.lib; DestDir: {app}\lib; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\Debug\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion
;Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\Debug\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion
;Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\Debug\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion
;Source: {#XERCESHOME}\src\*.c; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#ACETAOHOME}\tao\tao\*.i; DestDir: {app}\include\tao\tao; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\mswd\wx\setup.h; DestDir: {app}\include\wx\; Attribs: readonly; Flags: uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#ACETAOHOME}\lib\ace.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#SIMAGEHOME}\bin\simage*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion
;Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\*.lib; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
;Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\TAO\orbsvcs\Naming_Service\Naming_Service_d.exe; DestDir: {app}\bin; Components: acetao

Source: {#DEPENDSINSTALLHOME}\{#OSGALHOME}\share\oalinst.exe; DestDir: {tmp}; Flags: skipifsourcedoesntexist
[Run]
Filename: {tmp}\oalinst.exe; Flags: runascurrentuser

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
