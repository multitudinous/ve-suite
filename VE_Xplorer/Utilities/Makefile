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
CFDUNAME = $(shell uname)
# IRIX needs these flags
ifeq (${CFDUNAME},IRIX64)
   EXTRA_CXXFLAGS+= -D_IRIX
   EXTRA_LIBS+= -L/usr/freeware/lib32
endif
#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   cfdAccessoryFunctions.cxx \
   cfdGrid2Surface.cxx \
   cfdVTKFileHandler.cxx \
   cleanVtk.cxx \
   fileIO.cxx \
   readWriteVtkThings.cxx

			
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)
#EXTRA_INCLUDES+= -I$(VJ_DEPS_DIR)/include

#EXTRA_LDFLAGS+= -L$(VJ_DEPS_DIR)/lib
#DSO_PLUGIN_DEPS+= $(shell vrjuggler-config --extra-libs)
$(shell mkdir -p $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE))

include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.boost.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

PLUGIN_NAME= $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)/libVE_UtilLib

#OBJDIR is the place that the object files are stored.
OBJDIR= $(CFDHOSTTYPE)_VE_UtilLib

#DEPDIR is the place that the dependency files are stored.
DEPDIR= $(CFDHOSTTYPE)_VE_UtilLib

#BUILD_TYPE=	dbg

srcdir= .

ifeq (${CFDUNAME},Darwin)
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.mac.lib.mk
else
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.linux.lib.mk
endif

