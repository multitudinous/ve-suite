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
# Date modified: $Date: 2004-08-28 12:35:59 -0700 (Sat, 28 Aug 2004) $
# Version:       $Rev: 858 $
# -----------------------------------------------------------------
#
# *************** <auto-copyright.pl END do not edit this line> ***************
#
#

WX_CXX = $(shell wx-config --cxxflags)
WX_LIB = $(shell wx-config --libs)
#XERCESCROOT=/home/yang/newinterface/xerces-c-src_2_5_0
PLUGIN = VEEquilibriumReactor

ifdef ACE_ROOT
   FUTUREGEN_HOME = ${VE_SUITE_HOME}/futuregen

   vpath %.cxx    ${VE_SUITE_HOME}/VE_Xplorer ${VE_SUITE_HOME}/VE_SceneGraph
   vpath %.cpp    ${FUTUREGEN_HOME}/IDL ${FUTUREGEN_HOME}/Plugin

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

   ACE_LIBS = -L${ACE_ROOT}/ace -lACE -lTAO_CosNaming -lTAO_Svc_Utils \
               -lTAO_IORTable -lTAO_PortableServer \
               -L${TAO_ROOT}/tao -lTAO -L$(XERCESCROOT)/lib -lxerces-c
#${LDFLAGS}
   ACE_INCLUDES = -I${TAO_ROOT}/orbsvcs -I$(XERCESCROOT)/include
   EXTRA_INCLUDES+=	\
                  ${ACE_INCLUDES} \
                  -I${VE_SUITE_HOME}/VE_Xplorer \
                  -I${FUTUREGEN_HOME}/IDL \
                  -I${FUTUREGEN_HOME}/Plugin
   EXEC_TAG = _tao
endif

#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   VEEquilibriumReactor.cxx \
   cfdDCS.cxx \
   cfdGeode.cxx \
   cfdGroup.cxx \
   cfdObjects.cxx \
   cfdVEBaseClass.cxx \
   cfdModuleGeometry.cxx \
   interface.cpp

APP_NAME= $(VE_SUITE_HOME)/VE_Models/$(CFDHOSTTYPE)$(EXEC_TAG)/lib$(PLUGIN).so
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
	mkdir -p $(VE_SUITE_HOME)/VE_Models/$(CFDHOSTTYPE)$(EXEC_TAG)
#	$(MAKE) cxx_idl
#	$(MAKE) java_idl
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)
#	$(MAKE) java
#	$(MAKE) NetworkTestBean.jar
#	-$(MAKE) install

srcdir=		.

# turn off dependencies for idl stuff, make clean
NO_DEPEND=	YES

include $(VE_SUITE_HOME)/VE_Installer/dzr.vtk.mk
include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
include $(DZR_BASE_DIR)/ext/vrjuggler/dzr.vrjuggler.pfapp.mk

# One of these must be uncommented.
DEBUG_APP?=		TRUE
#OPTIM_APP?=		TRUE

EXTRA_CXXFLAGS+=	$(WX_CXX) -D WXUSING_PLUGIN_DLL -D_PERFORMER -m32
#$(EXTRA_FLAGS) 

EXTRA_CXXFLAGS+= -D_TAO ${CPPFLAGS} -I${VE_SUITE_HOME}/VE_SceneGraph
#EXTRA_CXXFLAGS+= -I$(BOOST_INCLUDES)
EXTRA_LIBS+= -shared ${ACE_LIBS} $(WX_LIB) -m32

LINKALL_ON=		-all
LINKALL_OFF=		

STATIC_ON=		@APP_LINK_STATIC_ON@
STATIC_OFF=		@APP_LINK_STATIC_OFF@

CXX_GEN_DIR=		.


cxx: $(OBJS)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

# in addition to the created *.o files and any core files, the following files will be removed during "gmake clean"
CLEAN_FILES+= $(APP_NAME)

