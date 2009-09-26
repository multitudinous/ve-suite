#!/bin/bash
# this bash shell script sets up the environment and starts VE-Suite launcher

# script will report PWD as something like, 
# <location of app>/VE-Suite.app/Contents/Resources
# PWD will be where we put dependencies dir and bin, lib, & share directories

# Define some variables to point to installation locations
MACPORTS_HOME=/opt/ves
VES_BASE_DIR=${PWD}
DEPS_HOME=${PWD}/dependencies
OSG_VERSION=2.8.2
OSG_HOME=${DEPS_HOME}/osg-${OSG_VERSION}
ACE_TAO_HOME=${DEPS_HOME}/ACE+TAO
JUGGLER_HOME=${DEPS_HOME}/juggler

# Some miscellaneous variables used by VR Juggler
export NO_RTRC_PLUGIN=TRUE
export NO_PERF_PLUGIN=TRUE

# Some miscellaneous variables used by OSG
export OSG_THREAD_SAFE_REF_UNREF=1
export OSGNOTIFYLEVEL=DEBUG_INFO

# Setup library search path
export DYLD_LIBRARY_PATH=${DEPS_HOME}/lib:${DEPS_HOME}/lib/vtk-5.4
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${OSG_HOME}/lib
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${OSG_HOME}/lib/osgPlugins-${OSG_VERSION}
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${VES_BASE_DIR}/lib
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${ACE_TAO_HOME}/lib
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${JUGGLER_HOME}/lib
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/ImageIO.framework/Versions/A/Resources
export DYLD_LIBRARY_PATH=${DYLD_LIBRARY_PATH}:${MACPORTS_HOME}/lib

# Setup path for running applications
export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin
export PATH=${MACPORTS_HOME}/bin:${MACPORTS_HOME}/sbin:${DEPS_HOME}/bin:${PATH}
export PATH=${ACE_TAO_HOME}/bin:${VES_BASE_DIR}/bin:${PATH}

exec python2.5 ${VES_BASE_DIR}/bin/velauncher.py --dev
