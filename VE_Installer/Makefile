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
# Date modified: $Date$
# Version:       $Rev$
# -----------------------------------------------------------------
#
# *************** <auto-copyright.pl END do not edit this line> ***************

DIRS= ${VE_SUITE_HOME}/VE_Builder/Translator \
      ${VE_SUITE_HOME}/VE_Builder/WX_Translator \
      ${VE_SUITE_HOME}/VE_Builder/Preprocessor \
      ${VE_SUITE_HOME}/VE_Builder/Utilities \
      ${VE_SUITE_HOME}/VE_Conductor \
      ${VE_SUITE_HOME}/VE_Xplorer

# -----------------------------------------------------------------------------
# Build targets.
# -----------------------------------------------------------------------------
default: all

all: $(DIRS)

${VE_SUITE_HOME}/VE_Builder/Translator \
${VE_SUITE_HOME}/VE_Builder/WX_Translator \
${VE_SUITE_HOME}/VE_Builder/Preprocessor \
${VE_SUITE_HOME}/VE_Xplorer::
	cd $@ && $(MAKE) -j

${VE_SUITE_HOME}/VE_Conductor \
${VE_SUITE_HOME}/VE_Builder/Utilities::
	cd $@ && $(MAKE)

# -----------------------------------------------------------------------------
# Clean-up.
# -----------------------------------------------------------------------------
clean:
	@for dir in $(DIRS) ; do		\
            cd $$dir && $(MAKE) clean ;		\
            cd .. ;				\
        done

cleandepend:
	@for dir in $(DIRS) ; do		\
            cd $$dir && $(MAKE) cleandepend ;		\
            cd .. ;				\
        done
