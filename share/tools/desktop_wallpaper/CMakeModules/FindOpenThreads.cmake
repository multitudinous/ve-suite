# OpenThreads is a C++ based threading library. Its largest userbase 
# seems to OpenSceneGraph so you might notice I accept OSGDIR as an
# environment path.
# I consider this part of the Findosg* suite used to find OpenSceneGraph 
# components.
# Each component is separate and you must opt in to each module.
# 
# Locate OpenThreads
# This module defines
# OPENTHREADS_LIBRARY
# OPENTHREADS_FOUND, if false, do not try to link to OpenThreads
# OPENTHREADS_INCLUDE_DIR, where to find the headers
#
# $OPENTHREADSDIR is an environment variable that would
# correspond to the ./configure --prefix=$OPENTHREADSDIR
# used in building osg.
#
# Created by Eric Wing.

# Header files are presumed to be included like
# #include <OpenThreads/Thread>

# For Windows, I have attempted to incorporate the environmental variables
# and registry entries used by Mike E. Weiblen's (mew) OSG installer. 
# 
# On OSX, this will prefer the Framework version (if found) over others.
# People will have to manually change the cache values of 
# the library to override this selection or set the CMake environment
# CMAKE_INCLUDE_PATH to modify the search paths.
#
# I originally had implemented some really nasty hacks to do OS X
# framework detection. CMake per my request has introduced native support
# for this so the code has been simplified. But for this to work,
# you must be using the new CMake code (introduced just before Jan 1st, 2006).

FIND_PATH(OPENTHREADS_INCLUDE_DIR OpenThreads/Thread
    $ENV{OPENTHREADSDIR}/include
    $ENV{OPENTHREADSDIR}
    $ENV{OSGDIR}/include
    $ENV{OSGDIR}
    $ENV{OSG_ROOT}/include
    /usr/local/include
    /usr/include
    /sw/include # Fink
    /opt/local/include # DarwinPorts
    /opt/csw/include # Blastwave
    /opt/include
    [HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Session\ Manager\\Environment;OSG_ROOT]/include
    ~/Library/Frameworks
    /Library/Frameworks
)

FIND_LIBRARY(OPENTHREADS_LIBRARY 
    NAMES OpenThreads OpenThreadsWin32
    PATHS
    $ENV{OPENTHREADSDIR}/lib
    $ENV{OPENTHREADSDIR}
    $ENV{OSGDIR}/lib
    $ENV{OSGDIR}
    $ENV{OSG_ROOT}/lib
    /usr/local/lib
    /usr/lib
    /sw/lib
    /opt/local/lib
    /opt/csw/lib
    /opt/lib
    [HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Session\ Manager\\Environment;OSG_ROOT]/lib
    ~/Library/Frameworks
    /Library/Frameworks
)

SET(OPENTHREADS_FOUND "NO")
IF(OPENTHREADS_LIBRARY)
    SET(OPENTHREADS_FOUND "YES")
ENDIF(OPENTHREADS_LIBRARY)


