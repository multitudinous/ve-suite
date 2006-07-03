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
# Date modified: $Date$
# Version:       $Rev$
# -----------------------------------------------------------------
#
# *************** <auto-copyright.pl END do not edit this line> ***************
CFDUNAME = $(shell uname)
#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   Attribute.cxx\
   MaterialHelper.cxx\
   ShaderHelper.cxx
			
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)

$(shell mkdir -p $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE))

include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk

ifeq (${SCENE_GRAPH},OSG)
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.osg.mk
else
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.pf.mk
endif

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

PLUGIN_NAME= $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)/libVE_SceneGraph_UtilitiesLib
#OBJDIR is the place that the object files are stored.
OBJDIR= $(CFDHOSTTYPE)_VE_SceneGraph_UtilitiesLib

#DEPDIR is the place that the dependency files are stored.
DEPDIR= $(CFDHOSTTYPE)_VE_SceneGraph_UtilitiesLib

#BUILD_TYPE=	dbg

EXTRA_CXXFLAGS+= -g
EXTRA_LDFLAGS+= -g

EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)
srcdir= .
DSO_PLUGIN_DEPS+= -L${VE_SUITE_HOME}/lib/${CFDHOSTTYPE} -lVE_XMLLib -lVE_ShaderLib -lVE_CADLib


ifeq (${CFDUNAME},Darwin)
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.mac.lib.mk
else
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.linux.lib.mk
endif

