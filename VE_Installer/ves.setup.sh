#!/bin/sh
# this is a bourne shell script
# sets up environment to build and/or run VE-Suite

# location of of VE-Suite installation location
export VES_BASE_DIR=<location of VES installation>

# Some miscellaneous variables used by VR Juggler, OSG, Performer, and OmniORB
# Performer
#export PFNFYLEVEL=0
#export PFSHAREDSIZE=534773700
# VR Juggler
#export VPR_DEBUG_NFY_LEVEL=0 
#export VPR_DEBUG_ENABLE=1
#export VPR_DEBUG_FILE=${VE_SUITE_HOME}/VE_Xplorer/VPRDebugOutput.txt
#export VPR_DEBUG_ALLOW_CATEGORIES="DBG_ALL DBG_ERROR VES_DBG"
#export VPR_DEBUG_DISALLOW_CATEGORIES="VES_DBG DBG_KERNEL"
export NO_RTRC_PLUGIN=TRUE
export NO_PERF_PLUGIN=TRUE
# OSG
export OSG_THREAD_SAFE_REF_UNREF=1
export OSGNOTIFYLEVEL=DEBUG_INFO
# OmniORB
#export OMNIORB_CONFIG=${VE_SUITE_HOME}/VE_Installer/omniORB4.cfg
#export OMNINAMES_LOGDIR=${VE_SUITE_HOME}/VE_Installer

# Setup flagpoll path to to be utilized by VE-Suite
# Can be used to setup package specific installation variables if desired
# with something like > `flagpoll vtk --get-prefix`
export FLAGPOLL_PATH=<path to gmtl fpc file>
export FLAGPOLL_PATH=${FLAGPOLL_PATH}:<anymore directories needed by flagpoll>

# Setup variables to make configuration of environment easier
export JDK_HOME=<location of Java installation>
export VTK_BASE_DIR=<location of VTK installation>
export TAO_HOME=<location of TAO installation>
export WX_HOME=<location of wxWidgets installation>
export VJ_BASE_DIR=<location of VR Juggler installation>
export OSG_HOME=<location of OSG installation>
export BULLET_HOME=<location of Bullet installation>

# Setup library search path
# Can be LD_LIBRARY_PATH (for linux) and DYLD_LIBRARY_PATH for MacOS
export LD_LIBRARY_PATH=${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}::${TAO_HOME}/lib
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${OSG_HOME}/lib:${OSG_HOME}/lib/osgPlugins
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${WX_HOME}/lib
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${VES_BASE_DIR}/lib
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${BULLET_HOME}/lib
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:<other non-standard library installations>

# Setup path for running applications
export PATH=${TAO_HOME}/bin:${PATH}
export PATH=${VJ_BASE_DIR}/bin:${VES_BASE_DIR}/bin:${PATH}
export PATH=${OSG_HOME}/share/OpenSceneGraph/bin:${PATH}

# Setup this to get access to fonts
export OSG_FILE_PATH=${OSG_HOME}/share/OpenSceneGraph-Data:${VES_BASE_DIR}/share/vesuite

# Another miscellaneous environment variable for building VR Juggler applications
export DZR_BASE_DIR=${VJ_BASE_DIR}/share/Doozer
