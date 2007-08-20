#!/bin/csh
# this is a C shell script
# sets up environment to build and/or run VE-Suite

# location of of VE-Suite installation location
setenv VES_BASE_DIR <location of VES installation>

# Some miscellaneous variables used by VR Juggler, OSG, Performer, and OmniORB
# Performer
#setenv PFNFYLEVEL 0
#setenv PFSHAREDSIZE 534773700
# VR Juggler
#setenv VPR_DEBUG_NFY_LEVEL 0 
#setenv VPR_DEBUG_ENABLE 1
#setenv VPR_DEBUG_FILE ${VE_SUITE_HOME}/VE_Xplorer/VPRDebugOutput.txt
#setenv VPR_DEBUG_ALLOW_CATEGORIES "DBG_ALL DBG_ERROR VES_DBG"
#setenv VPR_DEBUG_DISALLOW_CATEGORIES "VES_DBG DBG_KERNEL"
setenv NO_RTRC_PLUGIN TRUE
setenv NO_PERF_PLUGIN TRUE
# OSG
setenv OSG_THREAD_SAFE_REF_UNREF 1
setenv OSGNOTIFYLEVEL DEBUG_INFO
# OmniORB
#setenv OMNIORB_CONFIG ${VE_SUITE_HOME}/VE_Installer/omniORB4.cfg
#setenv OMNINAMES_LOGDIR ${VE_SUITE_HOME}/VE_Installer

# Setup flagpoll path to to be utilized by VE-Suite
# Can be used to setup package specific installation variables if desired
# with something like > `flagpoll vtk --get-prefix`
setenv FLAGPOLL_PATH <path to gmtl fpc file>
setenv FLAGPOLL_PATH ${FLAGPOLL_PATH}:<anymore directories needed by flagpoll>

# Setup variables to make configuration of environment easier
setenv JDK_HOME <location of Java installation>
setenv VTK_BASE_DIR <location of VTK installation>
setenv TAO_HOME <location of TAO installation>
setenv WX_HOME <location of wxWidgets installation>
setenv VJ_BASE_DIR <location of VR Juggler installation>
setenv OSG_HOME <location of OSG installation>
setenv BULLET_HOME <location of Bullet installation>

# Setup library search path
# Can be LD_LIBRARY_PATH (for linux) and DYLD_LIBRARY_PATH for MacOS
setenv LD_LIBRARY_PATH ${VJ_BASE_DIR}/lib:${VTK_BASE_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}::${TAO_HOME}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${OSG_HOME}/lib:${OSG_HOME}/lib/osgPlugins
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${WX_HOME}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${VES_BASE_DIR}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:${BULLET_HOME}/lib
setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:<other non-standard library installations>

# Setup path for running applications
setenv PATH ${TAO_HOME}/bin:${PATH}
setenv PATH ${VJ_BASE_DIR}/bin:${VES_BASE_DIR}/bin:${PATH}
setenv PATH ${OSG_HOME}/share/OpenSceneGraph/bin:${PATH}

# Setup python path to include wxPython if not installed with the OS
setenv PYTHONPATH <location of wxPython site package>

# Setup this to get access to fonts
setenv OSG_FILE_PATH ${OSG_HOME}/share/OpenSceneGraph-Data:${VES_BASE_DIR}/share/vesuite

# Another miscellaneous environment variable for building VR Juggler applications
setenv DZR_BASE_DIR ${VJ_BASE_DIR}/share/Doozer
