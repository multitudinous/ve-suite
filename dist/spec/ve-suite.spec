
BuildRequires:  ACE+TAO ACE+TAO-devel OpenProducer OpenProducer-devel OpenSceneGraph OpenSceneGraph-devel VE-Suite-Dependencies Xerces-c Xerces-c-devel aaa_base acl attr audiere audiere-devel autoconf automake bash binutils bison bullet-bin bzip2 coreutils cpio cpp devs diffutils doxygen e2fsprogs e2fsprogs-devel file filesystem findutils gadgeteer gadgeteer-devel gawk gcc gcc-c++ gettext glibc glibc-devel glibc-locale graphviz grep gtk gtk-devel gtk2 gtk2-devel gzip info klogd libacl libapr-util1 libapr-util1-devel libapr1 libapr1-devel libattr libgcc libpng libstdc++ libstdc++-devel libtool m4 make man mktemp module-init-tools openldap2 openldap2-devel openthreads openthreads-devel pango pango-devel patch permissions popt procinfo procps psmisc pwdutils python python-devel readline rpm scons sed sonix sonix-devel sonix-plugin-audiere sysvinit tar texinfo timezone unzip util-linux vpr vpr-devel vrjuggler vrjuggler-devel vrjuggler-opengl vrjuggler-opengl-devel vtk-bin wx28-bin xorg-x11-Mesa xorg-x11-Mesa-devel xorg-x11-devel xorg-x11-libs zlib zlib-devel

Name:         VE-Suite
License:      LGPL
Group:        System/Libraries
Autoreqprov:  on
Version:      1.0.2
Release:      1
Summary:      C++ Framework for Virtual Engineering
Source0:      %{name}-%{version}.tar.bz2
URL:          http://www.vesuite.org
Provides:     ve-suite
Requires:     ve-suite-dependencies
BuildRoot:    %{_tmppath}/%{name}-%{version}-build

%description
VE-Suite is being developed as an open source library of tools that enables the
virtual engineering process to take place.  The goal of virtual engineering is
to develop a decision making environment that provides a first-person
perspective enabling the user to interact with the engineered system in a
natural way and provides the user with a wide range of accessible tools."
Virtual Engineering applications are typically developed by software experts,
using high level languages, but are intended to be used by engineers, managers
and decision-makers that are not experts in graphics, programming, or computer
usage.  

VE-Suite is an extensible software architecture and is composed of
several tools, including VE-Builder, VE-Conductor, VE-Xplorer, VE-CE, and
VE-Designer. These tools are intended to allow the developer to more readily
develop a virtual engineering application. VE-Builder encompasses all the
functionality needed to build the virtual engineering environment. VE-Conductor
orchestrates the various functions of VE-Suite, includes a tablet-based
graphical user interface, and is used to access all the components in VE-Suite.
VE-Xplorer permits analysis and interrogation of data. VE-CE is the
computational engine that allows the integration and data passing mechanisms
for a diverse set of data sources (i.e. experimental data streams, numerical
models, algebraic equations, or any other form of data). VE-Designer, which is
still under development, will contain engineering design tools that allow the
interactive construction and interrogation of virtual models. Currently,
VE-Suite can read, display, and combine multiple steady and transient data sets
including experimental data, CAD data, and CFD/FEA results in both two and
three dimensions.  

VE-Suite is an open source software package. Several
companies and research groups, including NETL (National Energy Technology
Laboratory), REI (Reaction Engineering International), and John Deere are
collaborating with VRAC to improve and extend components of VE-Suite.
Currently, VE-Suite is in use in several large organizations, John Deere, NETL,
and REI. In the past year, Deere started actively using VE-Suite in its design
and development process. Within the next year, VE-Suite will also be in use at
INEEL (Idaho National Engineering and Environmental Laboratory).  

Authors:
-------- 
   Kenneth Mark Bryden, Ph.D <kmbryden@iastate.edu>

%package devel
Summary:      Everything needed for development with VE-Suite
Group:        Development/Libraries
Requires:     VE-Suite 

%description devel
This package contains all files needed for developing with VE-Suite
(headers, *.so symbolic links, etc.)

Authors:
--------
   Kenneth Mark Bryden, Ph.D <kmbryden@iastate.edu>

%package cluster
Summary:      Everything needed for running VE-Suite applications on a graphics cluster.
Group:        Development/Libraries
Requires:     VE-Suite 

%description cluster
This package contains all files needed for running VE-Suite cluster
applications.  (headers, *.so symbolic links, etc.)

Authors:
--------
   Kenneth Mark Bryden, Ph.D <kmbryden@iastate.edu>

#
## Define some generally useful macros
#

# The root macro defines the root location of the ve-suite installation.
# Changing this will change where ALL ve-suite files go.
%define root %{_prefix}/ve-suite-%{version}

# This macro defines the location of the VE-Suite dependencies directory; this
# macro is merely for shorthand.
%define vedeps %{root}/dependencies

# This macro defines the current version of VR Juggler
%define vrjversion 1.0.2

%prep

%setup -q -n %{name}-%{version}

%build

# Setup build environment variables
export SCENE_GRAPH=PF
export TAO_BUILD=TRUE
export VE_SUITE_HOME=$RPM_BUILD_DIR/%{name}-%{version}
export OSG_THREAD_SAFE_REF_UNREF=1
export OMNIORB_CONFI=${VE_SUITE_HOME}/VE_Installer/omniORB4.cfg
export OMNINAMES_LOGDIR=${VE_SUITE_HOME}/VE_Installer
export CFDHOSTTYPE=`head -1 /etc/SuSE-release | awk -F" " '{print $1 "_" $3 "_" $4}'`
export SCENE_GRAPH=OSG
export TAO_BUILD=TRUE
export WX_HOME=%{vedeps}/dependencies
export VTK_BASE_DIR=%{vedeps}
export TAO_HOME=%{_prefix}
export OSG_HOME=%{_prefix}
export PATH=${PATH}:${VE_SUITE_HOME}/Tools/flagpoll
export FLAGPOLL_PATH=${VE_SUITE_HOME}/Tools/flagpoll:${VE_SUITE_HOME}/VE_Installer/fpc:%{vedeps}/bin/pkgconfig
%ifarch %ix86
%define vebuilddir build.linux.2.0-SuSE-%{sles_version}.i686.ia32
%endif
# Run the build.
scons VtkBaseDir=${VTK_BASE_DIR} VtkVersion=5.0 OsgBaseDir=/usr XercesBaseDir=/usr WxWidgetsBaseDir=%{vedeps} BuildDir=%{vebuilddir}

%install
# Copy the files over by hand.
mkdir -p $RPM_BUILD_ROOT%{root}/bin
mkdir -p $RPM_BUILD_ROOT%{root}/include
mkdir -p $RPM_BUILD_ROOT%{root}/lib
mkdir -p $RPM_BUILD_ROOT%{root}/share
# VE_Xplorer
cp -r %{vebuilddir}/VE_Xplorer/GE/project_tao_osg_vep $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Xplorer/TextureBased/libVE_TextureBasedLib_tao_osg_vep.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/TextureBased/libVE_TextureBasedLib_tao_osg_vep.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/SceneGraph/NURBS/libVE_NURBSLib_tao_osg_vep.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/SceneGraph/NURBS/Utilities/libVE_NURBSUtilsLib_tao_osg_vep.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/SceneGraph/NURBS/Utilities/libVE_NURBSUtilsLib_tao_osg_vep.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/SceneGraph/NURBS/libVE_NURBSLib_tao_osg_vep.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/SceneGraph/libVE_SceneGraphLib_tao_osg_vep.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/SceneGraph/libVE_SceneGraphLib_tao_osg_vep.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/SceneGraph/Utilities/libVE_SceneGraph_UtilitiesLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/SceneGraph/Utilities/libVE_SceneGraph_UtilitiesLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/XplorerHandlers/libVE_XplorerLib_tao_osg_vep.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/XplorerHandlers/libVE_XplorerLib_tao_osg_vep.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/GraphicalPlugin/libGraphicalPlugin_tao_osg_vep.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/GraphicalPlugin/libGraphicalPlugin_tao_osg_vep.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/Utilities/libVE_UtilLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/Utilities/libVE_UtilLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/XplorerNetwork/libVE_XplorerNetworkLib_tao_osg_vep.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Xplorer/XplorerNetwork/libVE_XplorerNetworkLib_tao_osg_vep.a $RPM_BUILD_ROOT%{root}/lib
# VE_CE
cp -r %{vebuilddir}/VE_CE/UnitWrapper/libVE_CE_UnitWrapperLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_CE/UnitWrapper/libVE_CE_UnitWrapperLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_CE/Utilities/libVE_CE_UtilitiesLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_CE/Utilities/libVE_CE_UtilitiesLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_CE/Exe_server $RPM_BUILD_ROOT%{root}/bin
# VE_Conductor
cp -r %{vebuilddir}/VE_Conductor/Utilities/libVE_Conductor_UtilitiesLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Conductor/Utilities/libVE_Conductor_UtilitiesLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Conductor/Framework/WinClient $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Conductor/GUIPlugin/libGUIPluginLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Conductor/GUIPlugin/libGUIPluginLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Conductor/Network/libVE_ConductorLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Conductor/Network/libVE_ConductorLib.a $RPM_BUILD_ROOT%{root}/lib
# VE_Open
cp -r %{vebuilddir}/VE_Open/XML/CAD/libVE_CADLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/XML/CAD/libVE_CADLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/XML/Model/libVE_ModelLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/XML/Model/libVE_ModelLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/XML/Shader/libVE_ShaderLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/XML/Shader/libVE_ShaderLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/XML/libVE_XMLLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/XML/libVE_XMLLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/skel/VjObsC.cpp $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/VjObsC.inl $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/VjObsI.cpp $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/VjObsS.cpp $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/VjObsS.inl $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/VjObsS_T.h $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/libVE_OpenModuleLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/skel/VjObsS_T.cpp $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/VjObsS_T.inl $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/libVE_OpenModuleLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Open/skel/VjObsC.h $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/VjObsI.h $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/VjObsS.h $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleC.h $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleI.h $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleS.h $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleS_T.cpp $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleS_T.inl $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleC.cpp $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleC.inl $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleI.cpp $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleS.cpp $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleS.inl $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/VE_Open/skel/moduleS_T.h $RPM_BUILD_ROOT%{root}/share
cp -r %{vebuilddir}/clean.py $RPM_BUILD_ROOT%{root}/bin
# VE_Builder
cp -r %{vebuilddir}/VE_Builder/vtkTo3DTexture/tcGUI/vtkTo3DTexture $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/whatIsScalarRange $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/mergeVertices $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/appendVTK $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/addNormals2Poly $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/scaleVTK $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/combineFluentParticleFile $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/convertVTK2Binary $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/makeVtkSurface $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/removeVtkCellsOutsideBox $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/meshViewer $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/compareScalars $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/convertVTK2Ascii $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/createMiniFlowdata $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/removeVtkPointData $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/convertCellDataToPointData $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/transformVTK $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/convertSurfaceFileToStl $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Utilities/moveFieldToPointData $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Preprocessor/preprocessor $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Translator/cfdTranslatorToVTK/libTranslatorToVTKLib.a $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Builder/Translator/cfdTranslatorToVTK/libTranslatorToVTKLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Builder/Translator/DataLoader/libDataLoaderLib.so $RPM_BUILD_ROOT%{root}/lib
cp -r %{vebuilddir}/VE_Builder/Translator/DataLoader/TestLoader/loaderToVtk $RPM_BUILD_ROOT%{root}/bin
cp -r %{vebuilddir}/VE_Builder/Translator/DataLoader/libDataLoaderLib.a $RPM_BUILD_ROOT%{root}/lib

%post
/sbin/ldconfig

%postun
/sbin/ldconfig

%files
# VE_Xplorer
%{root}/bin/project_tao_osg_vep 
%{root}/lib/libVE_TextureBasedLib_tao_osg_vep.a 
%{root}/lib/libVE_TextureBasedLib_tao_osg_vep.so 
%{root}/lib/libVE_NURBSLib_tao_osg_vep.a 
%{root}/lib/libVE_NURBSUtilsLib_tao_osg_vep.so 
%{root}/lib/libVE_NURBSUtilsLib_tao_osg_vep.a 
%{root}/lib/libVE_NURBSLib_tao_osg_vep.so 
%{root}/lib/libVE_SceneGraphLib_tao_osg_vep.a 
%{root}/lib/libVE_SceneGraphLib_tao_osg_vep.so 
%{root}/lib/libVE_SceneGraph_UtilitiesLib.so 
%{root}/lib/libVE_SceneGraph_UtilitiesLib.a 
%{root}/lib/libVE_XplorerLib_tao_osg_vep.so 
%{root}/lib/libVE_XplorerLib_tao_osg_vep.a 
%{root}/lib/libGraphicalPlugin_tao_osg_vep.so 
%{root}/lib/libGraphicalPlugin_tao_osg_vep.a 
%{root}/lib/libVE_UtilLib.a 
%{root}/lib/libVE_UtilLib.so 
%{root}/lib/libVE_XplorerNetworkLib_tao_osg_vep.so 
%{root}/lib/libVE_XplorerNetworkLib_tao_osg_vep.a 
# VE_CE
%{root}/lib/libVE_CE_UnitWrapperLib.so 
%{root}/lib/libVE_CE_UnitWrapperLib.a 
%{root}/lib/libVE_CE_UtilitiesLib.a 
%{root}/lib/libVE_CE_UtilitiesLib.so 
%{root}/bin/Exe_server 
# VE_Conductor
%{root}/lib/libVE_Conductor_UtilitiesLib.a 
%{root}/lib/libVE_Conductor_UtilitiesLib.so 
%{root}/bin/WinClient 
%{root}/lib/libGUIPluginLib.a 
%{root}/lib/libGUIPluginLib.so 
%{root}/lib/libVE_ConductorLib.so 
%{root}/lib/libVE_ConductorLib.a 
# VE_Open
%{root}/lib/libVE_CADLib.so 
%{root}/lib/libVE_CADLib.a 
%{root}/lib/libVE_ModelLib.a 
%{root}/lib/libVE_ModelLib.so 
%{root}/lib/libVE_ShaderLib.a 
%{root}/lib/libVE_ShaderLib.so 
%{root}/lib/libVE_XMLLib.a 
%{root}/lib/libVE_XMLLib.so 
%{root}/lib/libVE_OpenModuleLib.a 
%{root}/lib/libVE_OpenModuleLib.so 
%{root}/share/VjObsC.cpp 
%{root}/share/VjObsC.inl 
%{root}/share/VjObsI.cpp 
%{root}/share/VjObsS.cpp 
%{root}/share/VjObsS.inl 
%{root}/share/VjObsS_T.h 
%{root}/share/VjObsS_T.cpp 
%{root}/share/VjObsS_T.inl 
%{root}/share/VjObsC.h 
%{root}/share/VjObsI.h 
%{root}/share/VjObsS.h 
%{root}/share/moduleC.h 
%{root}/share/moduleI.h 
%{root}/share/moduleS.h 
%{root}/share/moduleS_T.cpp 
%{root}/share/moduleS_T.inl 
%{root}/share/moduleC.cpp 
%{root}/share/moduleC.inl 
%{root}/share/moduleI.cpp 
%{root}/share/moduleS.cpp 
%{root}/share/moduleS.inl 
%{root}/share/moduleS_T.h 
%{root}/bin/clean.py 
# VE_Builder
%{root}/bin/vtkTo3DTexture 
%{root}/bin/whatIsScalarRange 
%{root}/bin/mergeVertices 
%{root}/bin/appendVTK 
%{root}/bin/addNormals2Poly 
%{root}/bin/scaleVTK 
%{root}/bin/combineFluentParticleFile 
%{root}/bin/convertVTK2Binary 
%{root}/bin/makeVtkSurface 
%{root}/bin/removeVtkCellsOutsideBox 
%{root}/bin/meshViewer 
%{root}/bin/compareScalars 
%{root}/bin/convertVTK2Ascii 
%{root}/bin/createMiniFlowdata 
%{root}/bin/removeVtkPointData 
%{root}/bin/convertCellDataToPointData 
%{root}/bin/transformVTK 
%{root}/bin/convertSurfaceFileToStl 
%{root}/bin/moveFieldToPointData 
%{root}/bin/preprocessor 
%{root}/bin/loaderToVtk 
%{root}/lib/libTranslatorToVTKLib.a 
%{root}/lib/libTranslatorToVTKLib.so 
%{root}/lib/libDataLoaderLib.so 
%{root}/lib/libDataLoaderLib.a 
