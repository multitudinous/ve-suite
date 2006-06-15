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
# Date modified: $Date: 2005-06-27 02:00:28 -0500 (Mon, 27 Jun 2005) $
# Version:       $Rev: 2519 $
# -----------------------------------------------------------------
#
# *************** <auto-copyright.pl END do not edit this line> ***************
#
#
#_______________________________CUSTOM OPTIONS__________________________________
#
# CLUSTER_APP now is defined as TRUE or FALSE from the setup script
#_______________________________________________________________________________

CFDUNAME = $(shell uname)
ifeq ($(SCENE_GRAPH), OSG)
   OSG = _osg
else
   OSG = _pf
endif

EXTRA_PATH_FOR_SOURCES+= $(VE_SUITE_HOME)/VE_Conductor/impl \
                        $(VE_SUITE_HOME)/VE_Open/skel \
                        $(VE_SUITE_HOME)/VE_Conductor/Framework \
                        $(VE_SUITE_HOME)/VE_CE \
                        $(VE_SUITE_HOME)/VE_Xplorer

EXTRA_INCLUDES+= \
               -I$(VE_SUITE_HOME)

ifeq ($(TAO_BUILD), TRUE)
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ace_tao.mk
   EXEC_TAG = _tao
endif

# for osg and patented stuff
ifeq ($(VE_PATENTED), TRUE)
   EXTRA_CXXFLAGS+= -DVE_PATENTED
   PATENTED_TAG=_vep
endif

# for xercese includes
ifdef XERCESCROOT
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk
endif

# IRIX needs these flags
ifeq (${CFDUNAME},IRIX64)
   EXTRA_CXXFLAGS+= -D_IRIX
   EXTRA_LIBS+= -L/usr/freeware/lib32
endif
#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   cfdApp.cxx \
   cfdAppWrapper.cxx \
   cfdVEXplorer.cxx \
   cfdVjObsWrapper.cxx \
   VjObs_i.cxx 
			
ifeq ($(WEB_BUILD), TRUE)
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.corona.mk
   EXTRA_CXXFLAGS+= -D_WEB_INTERFACE
endif

EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)
EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) 
EXTRA_LIBS+= -lGraphicalPlugin$(EXEC_TAG)$(OSG)$(PATENTED_TAG)
EXTRA_LIBS+= -lVE_UtilLib
EXTRA_LIBS+= -lVE_XplorerLib$(EXEC_TAG)$(OSG)$(PATENTED_TAG)
EXTRA_LIBS+= -lVE_SceneGraphLib$(OSG)$(PATENTED_TAG)
EXTRA_LIBS+= -lVE_TextureBasedLib$(OSG)$(PATENTED_TAG)
EXTRA_LIBS+= -lVE_XMLLib
EXTRA_LIBS+= -lVE_CADLib
EXTRA_LIBS+= -lVE_ModelLib
EXTRA_LIBS+= -lVE_OpenModuleLib
EXTRA_LIBS+= -lVE_ShaderLib
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk
#include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.wx.mk

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

ifeq ($(CLUSTER_APP), TRUE)
   APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/project$(EXEC_TAG)$(OSG)$(PATENTED_TAG)_cluster
   EXTRA_CXXFLAGS+= -D_CLUSTER
   # object files are being stored in a system-dependent directory so that
   # builds for multiple platforms may be maintained simultaneously.

   #OBJDIR is the place that the object files are stored.
   OBJDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)_cluster

   #DEPDIR is the place that the dependency files are stored.
   DEPDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)_cluster
else
   APP_NAME= $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)/project$(EXEC_TAG)$(OSG)$(PATENTED_TAG)
   # object files are being stored in a system-dependent directory so that builds
   # for multiple platforms may be maintained simultaneously.

   #OBJDIR is the place that the object files are stored.
   OBJDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

   #DEPDIR is the place that the dependency files are stored.
   DEPDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)
endif

default: all

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
all:
	mkdir -p $(VE_SUITE_HOME)/bin/$(CFDHOSTTYPE)
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

srcdir= .

# turn off dependencies for idl stuff, make clean
NO_DEPEND= YES

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
include $(DZR_BASE_DIR)/ext/vpr/dzr.vpr.mk


ifeq ($(SCENE_GRAPH), OSG)
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.osg.mk
else
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.pf.mk
endif

# One of these must be uncommented.
DEBUG_APP?= TRUE
#OPTIM_APP?= TRUE

#EXTRA_DEBUG_FLAGS+= -g -D_DEBUG -gslim 
EXTRA_OPTIM_FLAGS+= -O2 -OPT:Olimit=0 -D_OPT

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

CXX_GEN_DIR= .

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

# in addition to the created *.o files and any core files, the following files will be removed during "gmake clean"
CLEAN_FILES+= $(APP_NAME) $(OBJDIR)/* 
