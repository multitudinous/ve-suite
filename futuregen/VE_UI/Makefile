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
# Date modified: $Date: 2004-09-04 10:52:10 -0700 (Sat, 04 Sep 2004) $
# Version:       $Rev: 906 $
# -----------------------------------------------------------------
#
# *************** <auto-copyright.pl END do not edit this line> ***************
#
#
#_______________________________________________________________________________

CONDUCTOR_BASE_DIR= ${VE_SUITE_HOME}/VE_Conductor

WX_CXX = $(shell wx-config --cxxflags)
WX_LIB = $(shell wx-config --libs)
CFDUNAME = $(shell uname )
# If OMNI_HOME was defined in the script setup.tsh, 
# then define CORBA_DEFINES, and CORBA_INCLUDES.
# (To make a non-corba version, don't define OMNI_HOME.)
ifdef OMNI_HOME
   vpath %.cxx    ${CONDUCTOR_BASE_DIR}/impl

   ifeq ($(CFDUNAME),Linux)
      CORBA_DEFINES += -D__x86__ -D__linux__ -D__OSVERSION__=2
      OMNI_LIB = ${OMNI_HOME}/lib
   else
      CORBA_DEFINES += -D__mips__ -D__irix__ -D__OSVERSION__=6 
      OMNI_LIB = ${OMNI_HOME}/lib
   endif

   OMNILIB = -L${OMNI_LIB} -lomniORB4 -lomnithread -lomniDynamic4
   
   CORBA_INCLUDES = -I${OMNI_HOME}/include 
   EXTRA_INCLUDES+=	-I${CONDUCTOR_BASE_DIR}/impl \
                     -I${CONDUCTOR_BASE_DIR}/skel \
                     ${CORBA_INCLUDES}
   EXEC_TAG =
endif

ifdef ACE_ROOT
   FUTUREGEN_HOME = ${VE_SUITE_HOME}/futuregen

   vpath %.cxx    ${CONDUCTOR_BASE_DIR}/impl 
   vpath %.cpp    ${FUTUREGEN_HOME}/IDL ${FUTUREGEN_HOME}/Plugin ${FUTUREGEN_HOME}/Framework ${FUTUREGEN_HOME}/Executive

   #----------------------------------------------------------------------------
   # Include macros and targets
   #----------------------------------------------------------------------------

   include ${ACE_ROOT}/include/makeinclude/wrapper_macros.GNU
   include ${ACE_ROOT}/include/makeinclude/macros.GNU
#   include ${TAO_ROOT}/rules.tao.GNU
   include ${ACE_ROOT}/include/makeinclude/rules.common.GNU
   include ${ACE_ROOT}/include/makeinclude/rules.nonested.GNU
   include ${ACE_ROOT}/include/makeinclude/rules.local.GNU
   include ${TAO_ROOT}/taoconfig.mk

   ACE_LIBS = -L${ACE_ROOT}/ace -lACE -lTAO_CosNaming -lTAO_Svc_Utils -lTAO_IORTable -lTAO_PortableServer -L${TAO_ROOT}/tao -lTAO 
#${LDFLAGS}
   ACE_INCLUDES = -I${TAO_ROOT}/orbsvcs
   EXTRA_INCLUDES+=	\
                  ${ACE_INCLUDES} \
                  -I${CONDUCTOR_BASE_DIR}/impl \
                  -I${CONDUCTOR_BASE_DIR}/skel \
                  -I${FUTUREGEN_HOME}/IDL \
                  -I${FUTUREGEN_HOME}/Plugin \
                  -I${FUTUREGEN_HOME}/Framework \
                  -I${FUTUREGEN_HOME}/Executive
   EXEC_TAG = _tao
endif

ifdef XERCESCROOT
   EXTRA_INCLUDES+=	-I${XERCESCROOT}/include
   EXTRA_LIBS+= -L${XERCESCROOT}/lib -lxerces-c
endif

EXTRA_INCLUDES+=	-I${VE_SUITE_HOME}/VE_Xplorer 

#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   UI_App.cpp \
   UI_TopFrame.cpp \
   UI_Frame.cpp \
   UI_Tabs.cpp \
   UI_NavTab.cpp\
   UI_VisTab.cpp \
   UI_VecTab.cpp \
   UI_StreamTab.cpp \
   UI_SoundsTab.cpp \
   UI_GeometryTab.cpp \
   UI_TeacherTab.cpp \
   UI_TransTab.cpp \
   UI_VertTab.cpp \
   UI_ViewLocTab.cpp \
   UI_DesignParTab.cpp \
   UI_DataSetPanel.cpp \
   UI_ModSelPanel.cpp \
   UI_ModelData.cpp

ifeq ($(TAO_BUILD), TRUE)
SRCS+= \
   VjObsC.cpp 
else
SRCS+= \
   VjObs.cpp 
endif

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/ve_conductor$(EXEC_TAG)
# object files are being stored in a system-dependent directory so that builds
# for multiple platforms may be maintained simultaneously.

#OBJDIR is the place that the object files are stored.
OBJDIR=	$(CFDHOSTTYPE)$(EXEC_TAG)

#DEPDIR is the place that the dependency files are stored.
DEPDIR=	$(CFDHOSTTYPE)$(EXEC_TAG)

default: all

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
all:
	mkdir -p $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

srcdir=		.

# turn off dependencies for idl stuff, make clean
NO_DEPEND=	YES

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
include $(DZR_BASE_DIR)/ext/vrjuggler/dzr.vrjuggler.pfapp.mk


# One of these must be uncommented.
DEBUG_APP?=		TRUE
#OPTIM_APP?=		TRUE

# vtkActorToPF needs VTK4 and VTK44
EXTRA_CXXFLAGS+= $(WX_CXX) -D WXUSING_PLUGIN_DLL
#$(EXTRA_FLAGS) 

#EXTRA_DEBUG_FLAGS+=	-g -D_DEBUG -gslim 
EXTRA_OPTIM_FLAGS+=	-O2 -OPT:Olimit=0 -D_OPT

ifeq ($(TAO_BUILD), TRUE)
   EXTRA_CXXFLAGS+=	-D_TAO ${CPPFLAGS} -I$(BOOST_INCLUDES)
   EXTRA_LIBS+= ${ACE_LIBS} $(WX_LIB)
   #$(VLDLIBS)
else
   EXTRA_CXXFLAGS+=	$(TWEEK_CXX_FLAGS) $(CORBA_DEFINES) -I$(BOOST_INCLUDES)
   EXTRA_LIBS+= $(TWEEK_LIBS) $(TWEEK_EXTRA_LIBS) $(OMNILIB) $(WX_LIB)
endif

LINKALL_ON=		-all
LINKALL_OFF=		

STATIC_ON=		@APP_LINK_STATIC_ON@
STATIC_OFF=		@APP_LINK_STATIC_OFF@

#CLASSPATH=		$(TWEEK_JARS)

CXX_GEN_DIR=		.

vpath %.cpp $(CXX_GEN_DIR) ${CONDUCTOR_BASE_DIR}/skel


cxx: $(OBJS)

#java: $(CLASSES)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

# in addition to the created *.o files and any core files, the following files will be removed during "gmake clean"
CLEAN_FILES+= $(APP_NAME)
