; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#include <vesenv.iss>
#define MyAppName "VE_Suite.0.9.0_Dependencies"
#define MyAppVerName "VE_Suite.0.9.0_Dependencies"
#define MyAppPublisher "Complex Systems Virtual Engineering Group"
#define MyAppURL "www.vrac.iastate.edu/~kmbryden/vesuite"


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
WizardImageFile={#VEHOME}\VE_Installer\installer\installerImages\ve_suite_banner.bmp
WizardSmallImageFile={#VEHOME}\VE_Installer\installer\installerImages\icons.bmp
WindowVisible=true
WizardImageStretch=false

[Languages]
Name: eng; MessagesFile: compiler:Default.isl

[Files]
Source: {#VTKHOME}\bin\vtkzlib.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk; Languages: 
Source: {#VTKHOME}\bin\vtkCommon.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkDICOMParser.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkexpat.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkFiltering.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkfreetype.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkftgl.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkGraphics.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkHybrid.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkImaging.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkIO.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkjpeg.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkParallel.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkPatented.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkpng.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtkRendering.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#VTKHOME}\bin\vtktiff.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: vtk
Source: {#WXHOME}\lib\vc_dll\wxbase26d_vc_custom.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: wxwidgets
Source: {#WXHOME}\lib\vc_dll\wxmsw26d_core_vc_custom.dll; DestDir: {app}\bin; Components: wxwidgets; Flags: ignoreversion
Source: {#WXHOME}\lib\vc_dll\wxmsw26d_gl_vc_custom.dll; DestDir: {app}\bin; Components: wxwidgets; Flags: ignoreversion
Source: {#WXHOME}\lib\vc_dll\wxmsw26d_adv_vc_custom.dll; DestDir: {app}\bin; Components: wxwidgets; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAOd.dll; DestDir: {app}\bin; Flags: ignoreversion; Components: acetao
Source: {#ACETAOHOME}\bin\gperf.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\tao_idl.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\tao_ifr.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\tao_imr.exe; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\ACE_QoSd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\aced.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\ACEXMLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\Kokyud.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_AVd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_BiDirGIOPd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_CosConcurrencyd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_CosEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_CosLifeCycled.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_CosLoadBalancingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_CosNamingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_CosNotificationd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_CosPropertyd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_CosTimed.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_CosTradingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_Domaind.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_DsEventLogAdmind.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_DsLogAdmind.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_DsNotifyLogAdmind.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_DynamicAnyd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_DynamicInterfaced.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_ETCLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_Fault_Toleranced.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_FT_ClientORBd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_FT_ServerORBd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_FTORB_Utilsd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_IDL_BE_DLLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_IDL_FE_DLLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_IFR_BE_DLLd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_IFR_Clientd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_IFRServiced.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_IORInterceptord.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_IORManipd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_IORTabled.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_Messagingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_ObjRefTemplated.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_PortableGroupd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_PortableServerd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RT_Notificationd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTCORBAd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTCORBAEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTCosSchedulingd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTEventLogd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTKokyuEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTOLDEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTPortableServerd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTSchedd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTSchedEventd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_RTSchedulerd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_Securityd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_SmartProxiesd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_Strategiesd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_Svc_Utilsd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_TypeCodeFactoryd.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#ACETAOHOME}\bin\TAO_Valuetyped.dll; DestDir: {app}\bin; Components: acetao; Flags: ignoreversion
Source: {#XERCESHOME}\Build\Win32\VC7\Debug\xerces-c_2_6D.dll; DestDir: {app}\bin; Components: xercesc; Flags: ignoreversion
Source: {#XERCESHOME}\Build\Win32\VC7\Debug\xerces-depdom_2_6D.dll; DestDir: {app}\bin; Components: xercesc; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: {#ACETAOHOME}\TAO\orbsvcs\Naming_Service\Naming_Service.exe; DestDir: {app}\bin; Components: acetao
Source: {#VEHOME}\VE_Installer\installer\dependencies.txt; DestDir: {app}; Flags: isreadme; DestName: README.txt
Source: {#OSGHOME}\bin\Producerd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\gdal12.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\OpenThreadsWin32d.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_3dcd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_3dsd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_acd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_bmpd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_ddsd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_dwd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_fltd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_freetyped.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_gdald.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_geod.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_gifd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_hdrd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_ived.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_jpegd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_logod.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_lwod.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_lwsd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_md2d.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_netd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_objd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_osgd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_osgad.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_osgtgzd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_picd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_pngd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_pnmd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_rgbd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_rotd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_scaled.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_stld.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_tgad.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_tgzd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_tiffd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_transd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_txpd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_xd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_zipd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgDBd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgIntrospectiond.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgUtild.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgwrapper_osgd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgdb_pfbd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgSimd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#OSGHOME}\bin\osgTextd.dll; DestDir: {app}\bin; Components: osg; Flags: ignoreversion
Source: {#JUGGLERINSTHOME}\*; DestDir: {app}\vrJuggler2.0.0; Flags: ignoreversion recursesubdirs; Components: juggler


[Icons]
Name: {group}\{cm:UninstallProgram,{#MyAppName}}; Filename: {uninstallexe}

[Components]
Name: vtk; Description: Visualiztion ToolKit; Types: custom vexosg vexpf full
Name: acetao; Description: ACE/TAO; Types: vecns vexosg vexpf full custom
Name: wxwidgets; Description: wxWidgets 2.6.1; Types: custom vecns full
Name: xercesc; Description: Xerces-C++; Types: custom vecns vexosg vexpf full
Name: osg; Description: OpenSceneGraph; Types: custom vexosg full
Name: juggler; Description: vrJuggler; Types: custom vexosg vexpf full
[Types]
Name: full; Description: All pre-compiled dependencies
Name: vexpf; Description: VE-Xplorer-PF (Performer based)
Name: vexosg; Description: VE-Xplorer-OSG (OpenSceneGraph based)
Name: vecns; Description: VE-Conductor(GUI) and Name Server
Name: custom; Description: Custom; Flags: iscustom
