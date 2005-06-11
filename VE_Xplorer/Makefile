# ************** <auto-copyright.pl BEGIN do not edit this line> **************
#
# VE-Suite is (C) Copyright 1998-2004 by Iowa State University
#
# Original Development Team:
#   - ISU's Thermal Systems Virtual Engineering Group,
#     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
#   - Reaction Engineering International, www.reaction-eng.com
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.
#
# -----------------------------------------------------------------
# File:          $RCSfile: Makefile,v $
# Date modified: $Date$
# Version:       $Rev$
# -----------------------------------------------------------------
#
# *************** <auto-copyright.pl END do not edit this line> ***************
#
#
#_______________________________CUSTOM OPTIONS__________________________________
#
# CLUSTER_APP now is defined as TRUE or FALSE from the setup script
#_______________________________________________________________________________

CONDUCTOR_BASE_DIR= ${VE_SUITE_HOME}/VE_Conductor

CFDUNAME = $(shell uname)
ifeq ($(SCENE_GRAPH), OSG)
   OSG = _osg
else
   OSG =
endif

# If OMNI_HOME was defined in the script setup.tsh, 
# then define CORBA_DEFINES, and CORBA_INCLUDES.
ifdef OMNI_HOME
   vpath %.cxx    ${CONDUCTOR_BASE_DIR}/impl

   ifeq (${CFDUNAME},Linux)
      CORBA_DEFINES = -D__OSVERSION__=2
      CORBA_DEFINES += -D__x86__ -D__linux__
      OMNI_LIB = ${OMNI_HOME}/lib
   else
      CORBA_DEFINES = -D__OSVERSION__=6
      CORBA_DEFINES += -D__mips__ -D__irix__
      OMNI_LIB = ${OMNI_HOME}/lib
   endif

   OMNILIB = -L${OMNI_LIB} -lomniORB4 -lomnithread -lomniDynamic4
   
   CORBA_INCLUDES = -I${OMNI_HOME}/include 
   EXTRA_INCLUDES+= -I${CONDUCTOR_BASE_DIR}/impl \
                     ${CORBA_INCLUDES}
   EXEC_TAG =
endif

ifeq ($(TAO_BUILD), TRUE)
   EXTRA_PATH_FOR_SOURCES+= $(CONDUCTOR_BASE_DIR)/impl \
                           $(VE_SUITE_HOME)/VEOpen/skel \
                           $(VE_SUITE_HOME)/VE_Conductor/Framework \
                           $(VE_SUITE_HOME)/VE_CE \
									$(VE_SUITE_HOME)/VE_Builder/Translator

   include $(VE_SUITE_HOME)/VE_Installer/dzr.ace_tao.mk

   EXTRA_INCLUDES+= \
                  -I${CONDUCTOR_BASE_DIR}/impl \
                  -I$(VE_SUITE_HOME)/VEOpen/skel \
                  -I$(VE_SUITE_HOME)/VE_Conductor/Framework \
                  -I$(VE_SUITE_HOME)/VE_CE \
						-I$(VE_SUITE_HOME)/VE_Builder/Translator
   EXEC_TAG = _tao
endif

# for osg and patented stuff
ifeq ($(VE_PATENTED), TRUE)
   EXTRA_INCLUDES+= -I${VE_SUITE_HOME}/VE_VolumeVis
   EXTRA_CXXFLAGS+= -D_OSG -DVE_PATENTED
   EXTRA_PATH_FOR_SOURCES+= ${VE_SUITE_HOME}/VE_VolumeVis
ifeq ($(CFD_USE_SHADERS),TRUE)
   EXTRA_CXXFLAGS+= -DCFD_USE_SHADERS
endif
endif

# for xercese includes
ifdef XERCESCROOT
   EXTRA_INCLUDES+= -I${XERCESCROOT}/include
   EXTRA_LIBS+= -L${XERCESCROOT}/lib -lxerces-c
endif

# IRIX needs these flags
ifeq (${CFDUNAME},IRIX64)
   EXTRA_CXXFLAGS+= -D_IRIX
   EXTRA_LIBS+= -L/usr/freeware/lib32
endif
#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   cfd1DTextInput.cxx \
   cfdAnimatedStreamlineCone.cxx \
   cfdAnimatedImage.cxx \
   cfdApp.cxx \
   cfdAppWrapper.cxx \
   cfdAccessoryFunctions.cxx \
   cfdCommandObjects.cxx \
   cfdCommandArray.cxx \
   cfdContour.cxx \
   cfdContourBase.cxx \
   cfdContours.cxx \
   cfdCursor.cxx \
   cfdCuttingPlane.cxx \
   cfdClone.cxx \
   cfdDCS.cxx \
   cfdDataSet.cxx \
   cfdDigitalAnalogGauge.cxx \
   cfdEnvironmentHandler.cxx \
   cfdFILE.cxx \
   cfdFileInfo.cxx \
   cfdGeode.cxx \
   cfdGlobalBase.cxx \
   cfdGraphicsObject.cxx \
   cfdGroup.cxx \
   cfdImage.cxx \
   cfdIsosurface.cxx \
   cfdModel.cxx \
   cfdModelHandler.cxx \
   cfdMomentum.cxx \
   cfdMomentums.cxx \
   cfdNavigate.cxx \
   cfdNode.cxx \
   cfdNodeTraverser.cxx \
   cfdObjects.cxx \
   cfdPlanes.cxx \
   cfdPresetContour.cxx \
   cfdPresetMomentum.cxx \
   cfdPresetVector.cxx \
   cfdQuatCamHandler.cxx \
   cfdQuatCam.cxx \
   cfdReadParam.cxx \
   cfdScalarBarActor.cxx \
   cfdSceneNode.cxx \
   cfdSequence.cxx \
   cfdSound.cxx \
   cfdSoundHandler.cxx \
   cfdSteadyStateVizHandler.cxx \
   cfdStreamers.cxx \
   cfdSwitch.cxx \
   cfdPfSceneManagement.cxx \
   cfdPolyData.cxx \
   cfdTeacher.cxx \
   cfdTempAnimation.cxx \
   cfdTextOutput.cxx \
   cfdThread.cxx \
   cfdVTKFileHandler.cxx \
   cfdVectorBase.cxx \
   cfdVector.cxx \
   cfdVectors.cxx \
   cfdVjObsWrapper.cxx \
   cfdVEXplorer.cxx \
   cfdWriteTraverser.cxx \
   fileIO.cxx \
   readWriteVtkThings.cxx \
   VjObs_i.cxx \
	cfdGrid2Surface.cpp \
	cleanVtk.cpp
			

ifeq ($(VE_PATENTED), TRUE)
ifeq ($(SCENE_GRAPH), OSG)
SRCS+= \
   cfdOSGShaderManager.cxx \
   cfdOSGTransferShaderManager.cxx \
   cfdScalarShaderManager.cxx \
   cfdScalarVolumeVisHandler.cxx \
   cfdSimpleTextureCallback.cxx \
   cfdTextureBasedVizHandler.cxx \
   cfdTextureDataSet.cxx \
   cfdTextureMatrixCallback.cxx \
   cfdTextureManager.cxx \
   cfdUpdateTextureCallback.cpp \
   cfdUpdateableOSGTexture1d.cxx \
   cfdVolumeBillboard.cxx \
   cfdVolumeCenterCallback.cxx \
   cfdVolumeVisNodeHandler.cxx \
   cfdVolumeVisualization.cpp 
endif
endif

ifeq ($(VE_PATENTED), TRUE)
ifeq ($(SCENE_GRAPH), OSG)
ifeq ($(CFD_USE_SHADERS), TRUE)
SRCS+= \
   cfd3DTextureCullCallback.cxx \
   cfdAdvectionSubGraph.cxx \
   cfdCopyTo3DTextureStage.cxx \
   cfdOSGAdvectionShaderManager.cxx \
   cfdOSGPingPongTexture3d.cxx \
   cfdPBufferManager.cxx \
   cfdPBufferQuad.cxx \
   cfdUpdateableOSGNoiseTexture3d.cxx \
   cfdUpdateParameterCallback.cxx \
   cfdVectorVolumeVisHandler.cxx 
endif
endif
endif

ifeq ($(TAO_BUILD), TRUE)
SRCS+= \
   cfdExecutive.cxx \
   cfdExecutiveConfiguration.cxx \
   cfdVEPluginLoader.cxx \
   cfdVEAvailModules.cxx \
   cfdVEBaseClass.cxx \
   interface.cpp \
   moduleC.cpp \
   moduleS.cpp \
   Network_Exec.cpp \
   package.cpp \
   string_ops.cpp \
   VE_i.cpp \
   VjObsS.cpp \
   VjObsC.cpp
else
SRCS+= \
   VjObs.cpp 
endif

ifeq ($(WEB_BUILD), TRUE)
   include $(VE_SUITE_HOME)/VE_Installer/dzr.corona.mk
   EXTRA_CXXFLAGS+= -D_WEB_INTERFACE
endif
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)
EXTRA_INCLUDES+= -I${VE_SUITE_HOME}/VE_SceneGraph
EXTRA_PATH_FOR_SOURCES+= $(VE_SUITE_HOME)/VE_SceneGraph

include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk
include $(VE_SUITE_HOME)/VE_Installer/dzr.wx.mk
#Scene Graph Specific flags
#Can be: _PERFORMER, _OPENSG, _OSG
ifeq ($(SCENE_GRAPH), OSG)
   SRCS+= vtkActorToOSG.cpp
else
   SRCS+= vtkActorToPF.cxx
endif

#LIBS=   $(LIBS_BEGIN) $(SYS_LIBS) $(LIBS_END)
#include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
#include $(DZR_BASE_DIR)/mk/dzr.java-app.mk

#include $(DZR_BASE_DIR)/ext/tweek/dzr.tweek.mk
#include $(TWEEK_BASE_DIR)/share/tweek/tweek.appdefs.mk

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

ifeq ($(CLUSTER_APP), TRUE)
   APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/project_cluster$(EXEC_TAG)$(OSG)
   EXTRA_CXXFLAGS+= -D_CLUSTER
   # object files are being stored in a system-dependent directory so that
   # builds for multiple platforms may be maintained simultaneously.

   #OBJDIR is the place that the object files are stored.
   OBJDIR= $(CFDHOSTTYPE)_cluster$(EXEC_TAG)$(OSG)

   #DEPDIR is the place that the dependency files are stored.
   DEPDIR= $(CFDHOSTTYPE)_cluster$(EXEC_TAG)$(OSG)
   ifeq ($(VE_PATENTED), TRUE)
      #OBJDIR is the place that the object files are stored.
      OBJDIR= $(CFDHOSTTYPE)_cluster$(EXEC_TAG)$(OSG)_patented

      #DEPDIR is the place that the dependency files are stored.
      DEPDIR= $(CFDHOSTTYPE)_cluster$(EXEC_TAG)$(OSG)_patented
   endif
else
   APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/project$(EXEC_TAG)$(OSG)
   # object files are being stored in a system-dependent directory so that builds
   # for multiple platforms may be maintained simultaneously.

   #OBJDIR is the place that the object files are stored.
   OBJDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)

   #DEPDIR is the place that the dependency files are stored.
   DEPDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)
   ifeq ($(VE_PATENTED), TRUE)
      #OBJDIR is the place that the object files are stored.
      OBJDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)_patented

      #DEPDIR is the place that the dependency files are stored.
      DEPDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)_patented
   endif
endif


default: all

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
all:
	mkdir -p $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)
#	$(MAKE) cxx_idl
#	$(MAKE) java_idl
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)
#	$(MAKE) java
#	$(MAKE) NetworkTestBean.jar
#	-$(MAKE) install

srcdir= .

# turn off dependencies for idl stuff, make clean
NO_DEPEND= YES

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
include $(DZR_BASE_DIR)/ext/vpr/dzr.vpr.mk

ifeq ($(SCENE_GRAPH), OSG)
   include $(VE_SUITE_HOME)/VE_Installer/dzr.osg.mk
else
   include $(VE_SUITE_HOME)/VE_Installer/dzr.pf.mk
endif
# list the cpp and h files here
#IDL_CXX_FILES= ${CONDUCTOR_BASE_DIR}/Observer.cpp \
               ${CONDUCTOR_BASE_DIR}/VjObs.cpp \
               ${CONDUCTOR_BASE_DIR}/Observer.h \
               ${CONDUCTOR_BASE_DIR}/VjObs.h 


# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE


#EXTRA_CXXFLAGS+= $(WX_CXX) -DWXUSING_PLUGIN_DLL -DWXUSINGDLL=1
#EXTRA_LIBS+= $(WX_LIB) -L$(VJ_DEPS_DIR)/lib
#EXTRA_DEBUG_FLAGS+= -g -D_DEBUG -gslim 
EXTRA_OPTIM_FLAGS+= -O2 -OPT:Olimit=0 -D_OPT

ifeq ($(TAO_BUILD), TRUE)
   #include ${VE_SUITE_HOME}/VE_Installer/dzr.ace_tao.mk
   #EXTRA_CXXFLAGS+= -D_TAO
   #EXTRA_LIBS+= ${ACE_LIBS} $(WX_LIB)
   #$(VLDLIBS)
else
   EXTRA_CXXFLAGS+= $(TWEEK_CXX_FLAGS) $(CORBA_DEFINES)
   EXTRA_LIBS+= $(TWEEK_LIBS) $(TWEEK_EXTRA_LIBS) $(OMNILIB) 
endif

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

#CLASSPATH= $(TWEEK_JARS)

CXX_GEN_DIR= .

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
#cxx_idl: $(IDL_CXX_FILES)

#%.cpp %.h: %.idl
#	$(CXX_IDL) $(CXX_IDL_OPTS) $(CXX_IDL_INCLUDES) $<

#java_idl: $(IDL_JAVA_FILES)

#%.java: %.idl
#	$(JAVA_IDL) $(JAVA_IDL_OPTS) $(JAVA_IDL_INCLUDES) $<

cxx: $(OBJS)

#java: $(CLASSES)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)
#$(TWEEK_LIBS) 
#NetworkTestBean.jar: $(JAVA_CLASSES)
#	$(JAR) cvf $@ $(ALL_CLASSES)

#install:
#	cp NetworkTestBean.jar $(TWEEK_BASE_DIR)/bin/beans
#	cp $(srcdir)/NetworkTestBean.xml $(TWEEK_BASE_DIR)/bin/beans

# in addition to the created *.o files and any core files, the following files will be removed during "gmake clean"
CLEAN_FILES+= $(APP_NAME)
