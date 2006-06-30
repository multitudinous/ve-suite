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
CFDUNAME = $(shell uname)

#FLUENT_MFIX= TRUE

#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   DataLoader.cxx\
   EnSightTranslator.cxx\
   AVSTranslator.cxx\
   cfdDICOMTranslator.cxx\
   cfdREITranslator.cxx\
   converter.cpp\
   gridConversion.cpp\
   plot3dReader.cpp\
   StarCDTranslator.cxx\
   starReader.cpp

ifeq (${FLUENT_MFIX}, TRUE)
SRCS+=\
   FluentTranslator.cxx\
   MFIXTranslator.cxx
EXTRA_CXXFLAGS+= -DFLUENT_MFIX
endif

EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)
#EXTRA_INCLUDES+= -I$(VJ_DEPS_DIR)/include
EXTRA_PATH_FOR_SOURCES+= $(VE_SUITE_HOME)/VE_Builder/Translator
EXTRA_LDFLAGS+= -g
EXTRA_CXXFLAGS+= -g
#EXTRA_LDFLAGS+= -L$(VJ_DEPS_DIR)/lib
DSO_PLUGIN_DEPS += -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) -lVE_UtilLib
DSO_PLUGIN_DEPS += -lTranslatorToVTKLib
$(shell mkdir -p $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE))
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.boost.mk

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

PLUGIN_NAME= $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)/libDataLoaderLib

#OBJDIR is the place that the object files are stored.
OBJDIR= $(CFDHOSTTYPE)_DataLoader

#DEPDIR is the place that the dependency files are stored.
DEPDIR= $(OBJDIR)

#BUILD_TYPE=	dbg

#default: all

# Create the OS-dependent directory for the executable. If the dir exists, 
# 'mkdir -p' silently ignores, so no test needed. Works with multiple arguments.
#all:
#	mkdir -p $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)
#	$(MAKE) -f Makefile.utillib NO_DEPEND=0 cxx
#	$(MAKE) -f Makefile.utillib $(APP_NAME)

srcdir= .

ifeq (${CFDUNAME},Darwin)
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.mac.lib.mk
else
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.linux.lib.mk
endif

