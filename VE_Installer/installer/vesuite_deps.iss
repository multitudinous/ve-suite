; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppName "VE_Suite.1.0_Dependencies"
#define MyAppVerName "VE_Suite.1.0_Dependencies"
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
OutputDir={#VEHOME}\VE_Installer\installer
WizardImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_banner_1.0.bmp
WizardSmallImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_icon.bmp
WindowVisible=true
WizardImageStretch=false
WizardImageBackColor=clGray
BackColor=clBlack
BackColor2=$0080ff
SetupIconFile={#VEHOME}\VE_Installer\installer\installerImages\Ve_icon.ico

[Languages]
Name: eng; MessagesFile: compiler:Default.isl
[Types]
Name: full; Description: Full installation
Name: custom; Description: Custom installation; Flags: iscustom


Name: full; Description: All pre-compiled dependencies
Name: vexosg; Description: VE-Xplorer-OSG (OpenSceneGraph based)
Name: vecns; Description: VE-Conductor(GUI) and Name Server
Name: custom; Description: Custom; Flags: iscustom

[Components]
Name: depsbuildenv; Description: Headers and Libs; Types: full
Name: vtk; Description: Visualiztion ToolKit; Types: vexosg  full
Name: acetao; Description: ACE/TAO; Types: vecns vexosg  full
Name: wxwidgets; Description: wxWidgets 2.8.0; Types: vecns full
Name: xercesc; Description: Xerces-C++; Types: vecns vexosg  full
Name: osg; Description: OpenSceneGraph; Types: vexosg full
Name: juggler; Description: vrJuggler; Types: vexosg  full
Name: apr; Description: Apache APR; Types: vexosg vecns full
;Name: opal; Description: Open Physics Abstraction Library OPAL; Types: vexosg full
;Name: ode; Description: Open Dynamics Engine; Types: vexosg full
[Files]
Source: {#APRHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#APRUTILHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#APRICONVHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#OSGHOME}\..\Producer\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#OSGHOME}\..\OpenThreads\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#OSGHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#VTKHOME}\bin\*.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk; Languages: 
;Source: {#VTKHOME}\bin\vtkCommon.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkexpat.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkFiltering.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkfreetype.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkftgl.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkGraphics.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkHybrid.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkImaging.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkIO.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkjpeg.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkParallel.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkPatented.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkpng.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtkRendering.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
;Source: {#VTKHOME}\bin\vtktiff.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#WXHOME}\lib\vc_dll\wxbase28d_vc_custom.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: wxwidgets
Source: {#WXHOME}\lib\vc_dll\wxmsw28d_core_vc_custom.dll; DestDir: {app}\bin; Components: wxwidgets; Flags: ignoreversion
Source: {#WXHOME}\lib\vc_dll\wxmsw28d_gl_vc_custom.dll; DestDir: {app}\bin; Components: wxwidgets; Flags: ignoreversion
Source: {#WXHOME}\lib\vc_dll\wxmsw28d_adv_vc_custom.dll; DestDir: {app}\bin; Components: wxwidgets; Flags: ignoreversion
Source: {#ACETAOHOME}\lib\TAO*.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: acetao
;Source: {#ACETAOHOME}\bin\gperf.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\tao_idl.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\bin\tao_ifr.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\bin\tao_imr.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\ACE_QoSd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\lib\aced.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\ACEXMLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\Kokyud.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_AVd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_BiDirGIOPd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosConcurrencyd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosLifeCycled.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosLoadBalancingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosCodecFactoryd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosNaming_Servd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosNamingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosNotificationd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosPropertyd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosTimed.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_CosTradingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_Domaind.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_DsEventLogAdmind.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_DsLogAdmind.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_DsNotifyLogAdmind.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_DynamicAnyd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_DynamicInterfaced.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_ETCLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_Fault_Toleranced.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_FT_ClientORBd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_FT_ServerORBd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_FTORB_Utilsd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_IDL_BE_DLLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_IDL_FE_DLLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_IFR_BE_DLLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_IFR_Clientd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_IFRServiced.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_IORInterceptord.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_AnyTypeCoded.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_IORTabled.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_Messagingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_ObjRefTemplated.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_PortableGroupd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_PortableServerd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RT_Notificationd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTCORBAd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTCORBAEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTCosSchedulingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTEventLogd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTKokyuEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTOLDEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTPortableServerd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTSchedd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTSchedEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_RTSchedulerd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_Securityd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_SmartProxiesd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_Strategiesd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_Svc_Utilsd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\lib\TAO_TypeCodeFactoryd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
;Source: {#ACETAOHOME}\bin\TAO_Valuetyped.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#XERCESHOME}\Build\Win32\VC7.1\Debug\xerces-c_2_7D.dll; DestDir: {app}\bin; Components: xercesc; Flags: ignoreversion
Source: {#XERCESHOME}\Build\Win32\VC7.1\Debug\xerces-depdom_2_7D.dll; DestDir: {app}\bin; Components: xercesc; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: {#ACETAOHOME}\TAO\orbsvcs\Naming_Service\Naming_Service.exe; DestDir: {app}\bin; Components: acetao
Source: {#VEHOME}\VE_Installer\installer\dependencies.txt; DestDir: {app}; Flags: isreadme; DestName: README.txt
Source: {#OSGHOME}\bin\win32\*.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\..\3rdParty\bin\gdal*.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\..\OpenThreads\bin\win32\*.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\..\Producer\bin\win32\*.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_3dcd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_3dsd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_acd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_bmpd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_ddsd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_dwd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_fltd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_freetyped.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_gdald.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_geod.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_gifd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_hdrd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_ived.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_jpegd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_logod.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_lwod.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_lwsd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_md2d.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_netd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_objd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_osgd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_osgad.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_osgtgzd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_picd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_pngd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_pnmd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_rgbd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_rotd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_scaled.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_stld.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_tgad.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_tgzd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_tiffd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_transd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_txpd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_xd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_zipd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgDBd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgIntrospectiond.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgUtild.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgwrapper_osgd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_pfbd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgSimd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgTextd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgFXd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
;Source: {#OSGHOME}\bin\osgdb_ivd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#COINHOME}\bin\coin*.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#JUGGLERINSTHOME}\lib\*.dll; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs; Components: juggler
Source: {#VTKHOME}\include\*; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#WXHOME}\include\*.h; DestDir: {app}\include; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#XERCESHOME}\src\*.h*; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion recursesubdirs; Components: depsbuildenv
Source: {#COINHOME}\include\*.h; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion recursesubdirs; Components: depsbuildenv
Source: {#VTKHOME}\lib\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#OSGHOME}\lib\win32\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#WXHOME}\lib\vc_dll\*.lib; DestDir: {app}\lib\win32; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv; Attribs: readonly
Source: {#XERCESHOME}\Build\Win32\VC7.1\Debug\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#COINHOME}\lib\*.lib; DestDir: {app}\lib\win32; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#ACETAOHOME}\ace\*.h; DestDir: {app}\include\ace; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#ACETAOHOME}\tao\utils\*.h; DestDir: {app}\include\tao\utils; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#WXHOME}\lib\vc_dll\*.h; DestDir: {app}\include\wx; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#APRHOME}\Debug\libapr*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#APRUTILHOME}\Debug\libaprutil*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#APRICONVHOME}\Debug\libapriconv*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#APRHOME}\Debug\*.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#APRUTILHOME}\Debug\*.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#APRICONVHOME}\Debug\*.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion

;Source: {#ACETAOHOME}\lib\aced.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly replacesameversion; Components: depsbuildenv
;Source: {#ACETAOHOME}\lib\TAOd.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: recursesubdirs overwritereadonly uninsremovereadonly replacesameversion; Components: depsbuildenv
;Source: {#ACETAOHOME}\lib\TAO_CosNamingd.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: recursesubdirs overwritereadonly uninsremovereadonly replacesameversion; Components: depsbuildenv
Source: {#ACETAOHOME}\lib\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: recursesubdirs overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#ACETAOHOME}\lib\TAO_PortableServerd.lib; Attribs: readonly; Flags: recursesubdirs overwritereadonly uninsremovereadonly replacesameversion; Components: depsbuildenv; DestDir: {app}\lib\win32
Source: {#XERCESHOME}\src\*.c; DestDir: {app}\include; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#ACETAOHOME}\ace\*.inl; DestDir: {app}\include\ace; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#ACETAOHOME}\tao\*.inl; DestDir: {app}\include\tao; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#ACETAOHOME}\tao\tao\*.i; DestDir: {app}\include\tao\tao; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#ACETAOHOME}\ace\*.cpp; DestDir: {app}\include\ace; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
;Source: {#ACETAOHOME}\ace\*.i; DestDir: {app}\include\ace; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly replacesameversion
Source: {#ACETAOHOME}\tao\tao\*.cpp; DestDir: {app}\include\tao\tao; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#OSGHOME}\bin\win32\osgviewer*.exe; DestDir: {app}\bin; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion
Source: {#OSGHOME}\bin\win32\osgconv*.exe; DestDir: {app}\bin; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion
;Source: {#OSGHOME}\bin\osgGAd.dll; DestDir: {app}\bin; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion
;Source: {#OSGHOME}\bin\osgProducerd.dll; DestDir: {app}\bin; Attribs: readonly; Flags: overwritereadonly uninsremovereadonly ignoreversion
Source: {#WXHOME}\lib\vc_dll\mswd\wx\setup.h; DestDir: {app}\include\wx\; Attribs: readonly; Flags: uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#ACETAOHOME}\tao\TAO_IDL\*.h; DestDir: {app}\include\TAO_IDL; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#ACETAOHOME}\tao\orbsvcs\*.h; DestDir: {app}\include\orbsvcs; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#ACETAOHOME}\tao\tao\*.h; DestDir: {app}\include\tao; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#JUGGLERINSTHOME}\configFiles\*; DestDir: {app}\share\configFiles; Flags: ignoreversion recursesubdirs createallsubdirs; Components: juggler
Source: {#JUGGLERINSTHOME}\definitions\*; DestDir: {app}\share\definitions; Flags: ignoreversion recursesubdirs createallsubdirs; Components: juggler
Source: {#JUGGLERINSTHOME}\lib\*.lib; DestDir: {app}\lib; Flags: ignoreversion recursesubdirs createallsubdirs; Components: depsbuildenv; Languages: 
Source: {#JUGGLERINSTHOME}\include\*; DestDir: {app}\include; Flags: ignoreversion recursesubdirs createallsubdirs; Components: depsbuildenv; Languages: 
Source: {#XERCESHOME}\Build\Win32\VC7.1\Release\xerces-c_2_7.dll; DestDir: {app}\bin; Components: xercesc; Flags: ignoreversion
Source: {#XERCESHOME}\Build\Win32\VC7.1\Release\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#ACETAOHOME}\lib\ace.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#OSGHOME}\..\Producer\lib\win32\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#OSGHOME}\..\OpenThreads\lib\win32\*.lib; DestDir: {app}\lib\win32; Attribs: readonly; Flags: overwritereadonly recursesubdirs uninsremovereadonly ignoreversion; Components: depsbuildenv
Source: {#OSGHOME}\..\OpenSceneGraph-Data\fonts\*; DestDir: {app}\osg-fonts; Attribs: readonly; Flags: recursesubdirs uninsremovereadonly ignoreversion createallsubdirs; Components: depsbuildenv
Source: {#SIMAGEHOME}\bin\simage*.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion


[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}
