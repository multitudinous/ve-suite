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
CFDUNAME = $(shell uname)
#__________________________________________________________
ifeq ($(SCENE_GRAPH), OSG)
   OSG = _osg
else
   OSG = _pf
endif
# list the source files that are in current directory
SRCS=\
   ControlPoint.cxx \
   KnotVector.cxx \
   NCurve.cxx \
   NSurface.cxx \
   NURBSNode.cxx \
   NURBSObject.cxx \
			
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)

$(shell mkdir -p $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE))


ifeq (${SCENE_GRAPH},OSG)
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.osg.mk
else
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.pf.mk
endif

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

PLUGIN_NAME= $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)/libVE_NURBSLib$(EXEC_TAG)$(OSG)$(PATENTED_TAG)
#OBJDIR is the place that the object files are stored.
OBJDIR= $(CFDHOSTTYPE)_VE_NURBSLib$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

#DEPDIR is the place that the dependency files are stored.
DEPDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

#BUILD_TYPE=	dbg

ifeq (${BUILD_TYPE},OPT)
EXTRA_CXXFLAGS+= 
EXTRA_LDFLAGS+= 
else
EXTRA_CXXFLAGS+= -g
EXTRA_LDFLAGS+= -g
endif


srcdir= .
DSO_PLUGIN_DEPS+= -L/usr/X11R6/lib -lGLU -lGL

ifeq (${CFDUNAME},Darwin)
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.mac.lib.mk
else
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.linux.lib.mk
endif

