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
ifeq ($(SCENE_GRAPH), OSG)
   OSG = _osg
else
   OSG = _pf
endif

ifeq ($(TAO_BUILD), TRUE)
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ace_tao.mk
   EXEC_TAG = _tao
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
   cfd1DTextInput.cxx \
   cfdAnimatedImage.cxx \
   cfdAnimatedStreamlineCone.cxx \
   cfdCommandArray.cxx \
   cfdCommandObjects.cxx \
   cfdContour.cxx \
   cfdContourBase.cxx \
   cfdContours.cxx \
   cfdCursor.cxx \
   cfdCuttingPlane.cxx \
   cfdDataSet.cxx \
   cfdDigitalAnalogGauge.cxx \
   cfdEnvironmentHandler.cxx \
   cfdFILE.cxx \
   cfdFileInfo.cxx \
   cfdGlobalBase.cxx \
   cfdGraphicsObject.cxx \
   cfdImage.cxx \
   cfdIsosurface.cxx \
   cfdModel.cxx \
   cfdModelHandler.cxx \
   cfdMomentum.cxx \
   cfdMomentums.cxx \
   cfdNavigate.cxx \
   cfdObjects.cxx \
   cfdPlanes.cxx \
   cfdPolyData.cxx \
   cfdPresetContour.cxx \
   cfdPresetMomentum.cxx \
   cfdPresetVector.cxx \
   cfdQuatCamHandler.cxx \
   cfdQuatCam.cxx \
   cfdReadParam.cxx \
   cfdScalarBarActor.cxx \
   cfdSound.cxx \
   cfdSoundHandler.cxx \
   cfdSteadyStateVizHandler.cxx \
   cfdStreamers.cxx \
   cfdTeacher.cxx \
   cfdTextOutput.cxx \
   cfdTextureBasedVizHandler.cxx \
   cfdThread.cxx \
   cfdVectorBase.cxx \
   cfdVector.cxx \
   cfdVectors.cxx \
   cfdRawNodeWriteTraverser.cxx \
   cfdWriteTraverser.cxx \
   cfdObjectHandler.cxx

			
EXTRA_INCLUDES+= -I$(VE_SUITE_HOME)
EXTRA_PATH_FOR_SOURCES+= $(VE_SUITE_HOME)/VE_TextureBased

#EXTRA_LDFLAGS+= -L$(VJ_DEPS_DIR)/lib
#DSO_PLUGIN_DEPS+= $(shell vrjuggler-config --extra-libs)
$(shell mkdir -p $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE))

include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vtk.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.xerces.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.wx.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.vpr.mk
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.gmtl.mk

ifeq ($(SCENE_GRAPH), OSG)
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.osg.mk
else
   include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.pf.mk
endif

# -----------------------------------------------------------------------------
# following is adapted from tweek/test/NetworkTestBean/Makefile and tweek.appdefs.mk

PLUGIN_NAME= $(VE_SUITE_HOME)/lib/$(CFDHOSTTYPE)/libVE_XplorerLib$(EXEC_TAG)$(OSG)$(PATENTED_TAG).so

#OBJDIR is the place that the object files are stored.
OBJDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

#DEPDIR is the place that the dependency files are stored.
DEPDIR= $(CFDHOSTTYPE)$(EXEC_TAG)$(OSG)$(PATENTED_TAG)

#BUILD_TYPE=	dbg

srcdir= ..

ifeq (${CFDUNAME},Darwin)
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.mac.lib.mk
else
include $(VE_SUITE_HOME)/VE_Installer/mk/dzr.ves.linux.lib.mk
endif

