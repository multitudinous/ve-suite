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
# File:          Makefile,v
# Date modified: 2003/03/06 17:09:53
# Version:       1.16
# -----------------------------------------------------------------
#
# *************** <auto-copyright.pl END do not edit this line> ***************

# -----------------------------------------------------------------------------
# This requires GNU make.
# -----------------------------------------------------------------------------

APP_NAME=TSVEGpfSwitcher

# where is StopWatch.h ?
EXTRA_INCLUDES+= -I$(VJ_BASE_DIR)/../vrjuggler-2.0-alpha3.src/modules/vrjuggler/samples/Pf/advanced/pfNav

default: all

all: $(APP_NAME)

# Basic options.
srcdir=	.
SRCS=		TSVEGpfswitchermain.cpp \
         TSVEGpfswitcher.cpp \
         TSVEGpfSwitchAppHandler.cpp \
         cfdNavigate.cxx

include $(VJ_BASE_DIR)/share/vrjuggler/vrj.appdefs.mk
include $(DZR_BASE_DIR)/ext/vrjuggler/dzr.vrjuggler.pfapp.mk

# -----------------------------------------------------------------------------
# Application build targets.
# -----------------------------------------------------------------------------
$(APP_NAME): $(OBJS)
	$(LINK) $(LINK_OUT)$@ $(OBJS) $(EXTRA_LIBS) $(LIBS)

# in addition to the created *.o files and any core files, the following files will be removed during "gmake clean"
CLEAN_FILES+= $(APP_NAME)

