# ************** <auto-copyright.pl BEGIN do not edit this line> **************
#
# VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
# Date modified: $Date$
# Version:       $Rev$
# -----------------------------------------------------------------
#
# *************** <auto-copyright.pl END do not edit this line> ***************
#
#

PLUGIN = VEAdiabaticFlameTempGraphicalPlugin

#----------------------------------------------------------------------------
# Include macros and targets
#----------------------------------------------------------------------------

EXTRA_INCLUDES+=	\
               -I${VE_SUITE_HOME}/VE_Xplorer \
               -I${VE_SUITE_HOME} \
               -I${VE_SUITE_HOME}/VE_Conductor/Framework \
               -I${VE_SUITE_HOME}/VE_CE/skel

EXTRA_PATHS_FOR_SOURCES+= ${VE_SUITE_HOME}/VE_Xplorer 

EXEC_TAG = _tao

ifeq ($(SCENE_GRAPH), OSG)
   OSG = _osg
else
   OSG = _pf
endif

# for osg and patented stuff
ifeq ($(VE_PATENTED), TRUE)
   EXTRA_CXXFLAGS+= -DVE_PATENTED
   PATENTED_TAG=_vep
endif

#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   $(PLUGIN).cxx

APP_NAME= ../Plugins/$(CFDHOSTTYPE)/lib$(PLUGIN)$(EXEC_TAG)$(OSG)$(PATENTED_TAG).so
# object files are being stored in a system-dependent directory so that builds
# for multiple platforms may be maintained simultaneously.

#OBJDIR is the place that the object files are stored.
OBJDIR=	$(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

#DEPDIR is the place that the dependency files are stored.
DEPDIR=	$(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

default: all

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
all:
	mkdir -p ../Plugins/$(CFDHOSTTYPE)
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)
	cp $(APP_NAME) ../../Plugins/$(CFDHOSTTYPE)

srcdir=		.

# turn off dependencies for idl stuff, make clean
NO_DEPEND=	YES

include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ace_tao.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.wx.mk

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk

# One of these must be uncommented.
DEBUG_APP?=		TRUE
#OPTIM_APP?=		TRUE

ifeq ($(SCENE_GRAPH), OSG)
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.osg.mk
else
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.pf.mk
endif

EXTRA_LDFLAGS+= -shared 

EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) -lGraphicalPlugin$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

EXTRA_CXXFLAGS+= -DVE_USER_PLUGIN_LIBRARY

LINKALL_ON=		-all
LINKALL_OFF=		

STATIC_ON=		@APP_LINK_STATIC_ON@
STATIC_OFF=		@APP_LINK_STATIC_OFF@

CXX_GEN_DIR=		.

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

# in addition to the created *.o files and any core files, the following files will be removed during "gmake clean"
CLEAN_FILES+= $(APP_NAME)

