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

#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   TextureImage.cxx\
   Uniform.cxx\
   Program.cxx\
   Shader.cxx
			
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)
DSO_PLUGIN_DEPS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE) -lVE_XMLLib

$(shell mkdir -p $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE))
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

PLUGIN_NAME= $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)/libVE_ShaderLib

#OBJDIR is the place that the object files are stored.
OBJDIR= $(CFDHOSTTYPE)_VE_ShaderLib

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

