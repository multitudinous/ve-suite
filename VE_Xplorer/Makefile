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

WX_CXX = $(shell wx-config --cxxflags)
WX_LIB = $(shell wx-config --libs)
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
   vpath %.cxx    $(CONDUCTOR_BASE_DIR)/impl 
   vpath %.cpp    $(VE_SUITE_HOME)/VESOpen/skel
   vpath %.cpp    $(VE_SUITE_HOME)/VE_Conductor/Framework $(VE_SUITE_HOME)/VE_CE

   #----------------------------------------------------------------------------
   # Include macros and targets
   #----------------------------------------------------------------------------

#   include ${ACE_ROOT}/include/makeinclude/wrapper_macros.GNU
#   include ${ACE_ROOT}/include/makeinclude/macros.GNU
#   include ${TAO_ROOT}/rules.tao.GNU
#   include ${ACE_ROOT}/include/makeinclude/rules.common.GNU
#   include ${ACE_ROOT}/include/makeinclude/rules.nonested.GNU
#   include ${ACE_ROOT}/include/makeinclude/rules.local.GNU
#   include ${TAO_ROOT}/taoconfig.mk

#   ACE_LIBS = -L${ACE_ROOT}/ace -lACE -lTAO_CosNaming -lTAO_Svc_Utils \
               -lTAO_IORTable -lTAO_PortableServer -L${TAO_ROOT}/tao -lTAO \
               -lTAO_BiDirGIOP
#${LDFLAGS}
#   ACE_INCLUDES = -I${TAO_ROOT}/orbsvcs
   include $(VE_SUITE_HOME)/VE_Installer/dzr.ace_tao.mk

   EXTRA_INCLUDES+= \
                  -I${CONDUCTOR_BASE_DIR}/impl \
                  -I$(VE_SUITE_HOME)/VESOpen/skel \
                  -I$(VE_SUITE_HOME)/VE_Conductor/Framework \
                  -I$(VE_SUITE_HOME)/VE_CE
   EXEC_TAG = _tao
endif

ifdef XERCESCROOT
   EXTRA_INCLUDES+= -I${XERCESCROOT}/include
   EXTRA_LIBS+= -L${XERCESCROOT}/lib -lxerces-c
endif

# 64-bit linux needs these flags
ifeq (${CFDUNAME},Linux)
   EXTRA_CXXFLAGS+= -m32
   EXTRA_LIBS+= -m32
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
   cfdDCS.cxx \
   cfdDataSet.cxx \
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
   cfdScalarVolumeVisHandler.cxx \
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
   cfdTextureBasedModel.cxx \
   cfdTextureBasedVizHandler.cxx \
   cfdTextureMatrixCallback.cxx \
   cfdTextureManager.cxx \
   cfdThread.cxx \
   cfdUpdateTextureCallback.cpp \
   cfdVectorBase.cxx \
   cfdVector.cxx \
   cfdVectors.cxx \
   cfdVjObsWrapper.cxx \
   cfdVolumeVisualization.cpp \
   cfdVolumeVisNodeHandler.cxx \
   cfdVEXplorer.cxx \
   cfdWriteTraverser.cxx \
   fileIO.cxx \
   readWriteVtkThings.cxx \
   VjObs_i.cxx 

ifeq ($(TAO_BUILD), TRUE)
SRCS+= \
   VjObsS.cpp \
   VjObsC.cpp \
   cfdExecutiveConfiguration.cxx \
   VE_i.cpp \
   moduleC.cpp \
   moduleS.cpp \
   interface.cpp \
   string_ops.cpp \
   cfdVEPluginLoader.cxx \
   cfdVEAvailModules.cxx \
   cfdVEBaseClass.cxx \
   cfdExecutive.cxx \
   Network_Exec.cpp \
   package.cpp
else
SRCS+= \
   VjObs.cpp 
endif

EXTRA_INCLUDES+= -I${VE_SUITE_HOME}/VE_SceneGraph

include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk
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
else
   APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/project$(EXEC_TAG)$(OSG)
   # object files are being stored in a system-dependent directory so that builds
   # for multiple platforms may be maintained simultaneously.

   #OBJDIR is the place that the object files are stored.
   OBJDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)

   #DEPDIR is the place that the dependency files are stored.
   DEPDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)
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

# list the cpp and h files here
#IDL_CXX_FILES= SliderSubject.cpp SliderSubject.h \
                WhiteboardSubject.cpp WhiteboardSubject.h

# list the corresponding java files that are in directory "networktest"
#IDL_JAVA_FILES= SliderSubject.java \
                 WhiteboardSubject.java

#JAVA_SRCS=  ObserverHolder.java \
            VjObsHolder.java \
            ObserverServant.java \
            send_param.java \
            ack_win.java \
            attrib.java \
            draw_baf.java \
            id_mapper.java \
            WholeNumberField.java \
            client.java \
            my_orb.java 
#            FileInterfaceHolder.java \
#            HelloHolder.java \
# list the corresponding java files that are in directory "networktest"
# These files correspond to the IDL_CXX_FILES listed above
#IDL_JAVA_FILES= Observer.java \
               VjObs.java 

# all class files are used in creating the jar files
#ALL_CLASSES= *.class
#EXTRA_SOURCEPATH_DIRS= ${CONDUCTOR_BASE_DIR}/java_stuff
#CLASSPATH= ${CONDUCTOR_BASE_DIR}/java_stuff
#CLASSPATH_ARG= -classpath "${CONDUCTOR_BASE_DIR}/java_stuff"

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE


EXTRA_CXXFLAGS+= $(WX_CXX) -DWXUSING_PLUGIN_DLL -DWXUSINGDLL=1
EXTRA_LIBS+= $(WX_LIB) -L$(VJ_DEPS_DIR)/lib
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
#CXX_IDL= $(TWEEK_IDL_CXX)
#CXX_IDL_OPTS= $(TWEEK_IDLFLAGS_CXX) \
               $(TWEEK_IDL_GENDIR_CXX)$(CXX_GEN_DIR)
#CXX_IDL_INCLUDES=

#JAVA_GEN_DIR= $(CONDUCTOR_BASE_DIR)/java_stuff
#JAVA_IDL= $(TWEEK_IDL_JAVA)
#JAVA_IDL_OPTS= $(TWEEK_IDLFLAGS_JAVA) \
                $(TWEEK_IDL_GENDIR_JAVA)$(JAVA_GEN_DIR)
#JAVA_IDL_INCLUDES=

#WARN_MSG= I_AM_HERE
#EXTRA_PATH_FOR_SOURCES= ${CONDUCTOR_BASE_DIR} \
                        ${CONDUCTOR_BASE_DIR}/java_stuff \
                        ${CONDUCTOR_BASE_DIR}/java_stuff/ObserverPackage \
                        ${CONDUCTOR_BASE_DIR}/java_stuff/VjObsPackage 

vpath %.cxx $(VE_SUITE_HOME)/VE_SceneGraph
#vpath %.cc ./UMI/skel
#vpath %.h $(CXX_GEN_DIR)  ${CONDUCTOR_BASE_DIR}/skel

#vpath %.idl $(CONDUCTOR_BASE_DIR)
#vpath %.java $(JAVA_GEN_DIR)

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
#CLEAN_FILES+= CorbaManager.o \
      $(addprefix $(srcdir)/, $(IDL_CXX_FILES)) \
      $(addprefix $(srcdir)/networktest/, $(IDL_JAVA_FILES)) \
      $(CONDUCTOR_BASE_DIR)/*.d \
      $(CLASSES) tweek/*.class

#  in addition to the "ii_files" directory, the following directories will be removed during "gmake clean"
#CLEAN_DIRS+= $(srcdir)/tweek

#CLOBBER_FILES+= NetworkTestBean.jar

# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.



# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
