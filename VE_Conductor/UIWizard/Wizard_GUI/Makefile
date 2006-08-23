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
# Date modified: $Date: 2006-07-03 14:53:13 -0500 (Mon, 03 Jul 2006) $
# Version:       $Rev: 4871 $
# -----------------------------------------------------------------
#
# *************** <auto-copyright.pl END do not edit this line> ***************
CFDHOSTTYPE?=	$(DZR_HOSTTYPE)

APP_NAME= ModWiz

srcdir= .

all:
#	mkdir -p ../Units/$(CFDHOSTTYPE)
	$(MAKE) NO_DEPEND=0 cxx
	$(MAKE) $(APP_NAME)

SRCS=\
   Wizard.cpp

# Specify the OS-dependent directory name for object files and depends
OBJDIR= $(CFDHOSTTYPE)
DEPDIR= $(CFDHOSTTYPE)

NO_DEPEND= YES

# One of these must be uncommented.
#DEBUG_APP?= TRUE
OPTIM_APP?= TRUE

EXTRA_CFLAGS+=

EXTRA_CXXFLAGS+=

EXTRA_DEFS+=

EXTRA_LIBS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) -lVE_ConductorLib

LINKALL_ON= -all
LINKALL_OFF=

STATIC_ON= @APP_LINK_STATIC_ON@
STATIC_OFF= @APP_LINK_STATIC_OFF@

#IDL_HOME = $(VE_SUITE_HOME)/VEOpen

#EXTRA_PATH_FOR_SOURCES+= $(VE_SUITE_HOME)/VEOpen/skel 

EXTRA_INCLUDES += -I$(VE_SUITE_HOME)/VE_Conductor/Framework 
#EXTRA_INCLUDES += -I$(VE_SUITE_HOME)/VEOpen/skel
EXTRA_INCLUDES += -I$(VE_SUITE_HOME)

include $(DZR_BASE_DIR)/mk/dzr.basicapp.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.wx.mk
#include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ace_tao.mk
#include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
cxx: $(OBJS)

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
${APP_NAME}: $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

#install:
#	cp NetworkTestBean.jar $(TWEEK_BASE_DIR)/bin/beans
#	cp $(srcdir)/NetworkTestBean.xml $(TWEEK_BASE_DIR)/bin/beans

CLEAN_FILES+= $(APP_NAME)

