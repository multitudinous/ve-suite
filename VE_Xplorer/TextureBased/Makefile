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
ifeq ($(SCENE_GRAPH), OSG)
   OSG = _osg
else
   OSG = _pf
endif

# for osg and patented stuff
ifeq ($(VE_PATENTED), TRUE)
   EXTRA_CXXFLAGS+= -DVE_PATENTED
   PATENTED_TAG = _vep
endif

# IRIX needs these flags
ifeq (${CFDUNAME},IRIX64)
   EXTRA_CXXFLAGS+= -D_IRIX
   EXTRA_LIBS+= -L/usr/freeware/lib32
endif
#__________________________________________________________
# list the source files that are in current directory
SRCS=\
   cfd3DTextureCullCallback.cxx \
   cfdAdvectionSubGraph.cxx \
   cfdCopyTo3DTextureStage.cxx \
   cfdOSGAdvectionShaderManager.cxx \
   cfdOSGPingPongTexture3d.cxx \
   cfdOSGShaderManager.cxx \
   cfdOSGTransferShaderManager.cxx \
   cfdPBufferManager.cxx \
   cfdPBufferQuad.cxx \
   cfdScalarShaderManager.cxx \
   cfdScalarVolumeVisHandler.cxx \
   cfdSimpleTextureCallback.cxx \
   cfdTextureDataSet.cxx \
   cfdTextureManager.cxx \
   cfdTextureMatrixCallback.cxx \
   cfdUpdateableOSGNoiseTexture3d.cxx \
   cfdUpdateableOSGTexture1d.cxx \
   cfdUpdateParameterCallback.cxx \
   cfdUpdateTextureCallback.cxx \
   cfdVectorVolumeVisHandler.cxx \
   cfdVolumeBillboard.cxx \
   cfdVolumeCenterCallback.cxx \
   cfdVolumeVisNodeHandler.cxx \
   cfdVolumeVisualization.cxx 

			
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)
#EXTRA_INCLUDES+= -I$(VJ_DEPS_DIR)/include
#EXTRA_PATH_FOR_SOURCES+= $(VE_SUITE_HOME)/VE_Builder/Translator

#EXTRA_LDFLAGS+= -L$(VJ_DEPS_DIR)/lib
EXTRA_INCLUDES+= $(shell vrjuggler-config --cxxflags)
DSO_PLUGIN_DEPS+= -L/usr/X11R6/lib -lX11 -lGL
DSO_PLUGIN_DEPS+= -L$(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)
DSO_PLUGIN_DEPS+= -lVE_UtilLib 
$(shell mkdir -p $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE))

ifeq ($(SCENE_GRAPH), OSG)
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.osg.mk
else
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.pf.mk
endif

include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.boost.mk

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

PLUGIN_NAME= $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)/libVE_TextureBasedLib$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

#OBJDIR is the place that the object files are stored.
OBJDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

#DEPDIR is the place that the dependency files are stored.
DEPDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

#BUILD_TYPE=	dbg

srcdir= .

ifeq (${CFDUNAME},Darwin)
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.mac.lib.mk
else
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.linux.lib.mk
endif

