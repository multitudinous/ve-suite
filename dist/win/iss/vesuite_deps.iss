; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppName "VE_Suite.1.1_Dependencies"
#define MyAppVerName "VE_Suite.1.1_Dependencies"
#define MyAppPublisher "Virtual Engineering Research Group"
#define MyAppURL "www.vesuite.org"

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
OutputBaseFilename=vesuite_deps{#VEVERSION}
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
SetupIconFile={#VEINSTALLHOME}\bin\installerImages\Ve_icon.ico
PrivilegesRequired=none

[Languages]
Name: eng; MessagesFile: compiler:Default.isl
[Types]
Name: full; Description: Full installation
Name: custom; Description: Custom installation; Flags: iscustom

[Components]
Name: vtk; Description: Visualiztion ToolKit; Types: full
Name: acetao; Description: ACE/TAO; Types: full
Name: wxwidgets; Description: wxWidgets; Types: full
Name: xercesc; Description: Xerces-C++; Types: full
Name: osg; Description: OpenSceneGraph; Types: full
Name: juggler; Description: vrJuggler; Types: full
Name: apr; Description: Apache APR; Types: full
Name: osgal; Description: osgAL; Types: full
Name: depsbuildenv; Description: Headers and Libs

[Files]
Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
;Source: {#OSGHOME}\..\Producer\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
;Source: {#OSGHOME}\..\OpenThreads\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion; Components: vtk; Languages: 

Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxbase28d_vc_custom.dll; DestDir: {app}\lib; Flags: ignoreversion; Components: wxwidgets
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxmsw28d_core_vc_custom.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxmsw28d_gl_vc_custom.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxmsw28d_adv_vc_custom.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion; Components: acetao
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\bin\tao_idl.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\aced.dll; DestDir: {app}\lib; Components: acetao; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\Build\Win32\VC7.1\Debug\xerces-c_2_7D.dll; DestDir: {app}\lib; Components: xercesc; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\Build\Win32\VC7.1\Debug\xerces-depdom_2_7D.dll; DestDir: {app}\lib; Components: xercesc; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\TAO\orbsvcs\Naming_Service\Naming_Service.exe; DestDir: {app}\bin; Components: acetao
Source: {#VEDEVHOME}\dist\win\iss\dependencies.txt; DestDir: {app}; Flags: isreadme; DestName: README.txt
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\bin\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion recursesubdirs createallsubdirs
;Source: {#COINHOME}\bin\coin*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs; Components: juggler
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\bin\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs; Components: juggler
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\include\*.h; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\src\*.h*; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion recursesubdirs; Components: depsbuildenv
;Source: {#COINHOME}\include\*.h; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion recursesubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#VTKHOME}\lib\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\lib\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\*.lib; DestDir: {app}\lib; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv; Attribs: readonly
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\Build\Win32\VC7.1\Debug\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#COINHOME}\lib\*.lib; DestDir: {app}\lib; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\ace\*.h; DestDir: {app}\include\ace; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\tao\utils\*.h; DestDir: {app}\include\tao\utils; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\*.h; DestDir: {app}\include\wx; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\Debug\libapr*_d.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\Debug\libaprutil*_d.lib; DestDir: {app}\lib; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\Debug\libapriconv*_d.lib; DestDir: {app}\lib; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\Debug\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\Debug\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\Debug\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion

Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\Release\libapr*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion skipifsourcedoesntexist; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\Release\libaprutil*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion skipifsourcedoesntexist; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\Release\libapriconv*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion skipifsourcedoesntexist; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#APRHOME}\Release\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion skipifsourcedoesntexist
Source: {#DEPENDSINSTALLHOME}\{#APRUTILHOME}\Release\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion skipifsourcedoesntexist
Source: {#DEPENDSINSTALLHOME}\{#APRICONVHOME}\Release\*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion skipifsourcedoesntexist

Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\lib\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: recursesubdirs overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#XERCESHOME}\src\*.c; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\ace\*.inl; DestDir: {app}\include\ace; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\tao\*.inl; DestDir: {app}\include\tao; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#ACETAOHOME}\tao\tao\*.i; DestDir: {app}\include\tao\tao; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\ace\*.cpp; DestDir: {app}\include\ace; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\tao\tao\*.cpp; DestDir: {app}\include\tao\tao; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\bin\osgviewer*.exe; DestDir: {app}\bin; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#OSGHOME}\bin\osgconv*.exe; DestDir: {app}\bin; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\mswd\wx\setup.h; DestDir: {app}\include\wx\; Attribs: readonly; Flags: uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\tao\TAO_IDL\*.h; DestDir: {app}\include\TAO_IDL; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\tao\orbsvcs\*.h; DestDir: {app}\include\orbsvcs; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\tao\tao\*.h; DestDir: {app}\include\tao; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\share\vrjuggler\data\configFiles\*; DestDir: {app}\share\configFiles; Flags: ignoreversion recursesubdirs; Components: juggler
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\share\vrjuggler\data\definitions\*; DestDir: {app}\share\definitions; Flags: ignoreversion recursesubdirs createallsubdirs; Components: juggler
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs; Components: depsbuildenv; Languages: 
Source: {#DEPENDSINSTALLHOME}\{#JUGGLERINSTHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs; Components: depsbuildenv; Languages: 
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\Build\Win32\VC7.1\Release\xerces-c_2_7.dll; DestDir: {app}\lib; Components: xercesc; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\Build\Win32\VC7.1\Release\xerces-depdom_2_7.dll; DestDir: {app}\lib; Components: xercesc; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#XERCESHOME}\Build\Win32\VC7.1\Release\*.lib; DestDir: {app}\lib; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#ACETAOHOME}\lib\ace.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#SIMAGEHOME}\bin\simage*.dll; DestDir: {app}\lib; Components: osg; Flags: ignoreversion

Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxmsw28_adv_vc_custom.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxmsw28_gl_vc_custom.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxmsw28_core_vc_custom.dll; DestDir: {app}\lib; Components: wxwidgets; Flags: ignoreversion
Source: {#DEPENDSINSTALLHOME}\{#WXHOME}\lib\vc_dll\wxbase28_vc_custom.dll; DestDir: {app}\lib; Flags: ignoreversion; Components: wxwidgets
Source: {#DEPENDSINSTALLHOME}\{#ACETAOHOME}\TAO\orbsvcs\Naming_Service\Naming_Service_d.exe; DestDir: {app}\bin; Components: acetao

; osgAL includes
Source: {#DEPENDSINSTALLHOME}\{#OSGALHOME}\include\*.h; DestDir: {app}\include; Flags: ignoreversion recursesubdirs skipifsourcedoesntexist
; osgAL libs
Source: {#DEPENDSINSTALLHOME}\{#OSGALHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion skipifsourcedoesntexist
; osgAL dlls
Source: {#DEPENDSINSTALLHOME}\{#OSGALHOME}\bin\*.dll; DestDir: {app}\bin; Flags: ignoreversion skipifsourcedoesntexist

[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
