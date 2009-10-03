# Locate osgBullet.
#
# This script defines:
#   OSGBULLET_FOUND, set to "YES" or "NO".   
#   OSGBULLET_LIBRARIES
#   OSGBULLET_INCLUDE_DIR
#
# This script will look in standard locations for installed osgBullet. However, if you
# install osgBullet into a non-standard location, you can use the OSGBULLET_ROOT
# variable (in environment or CMake) to specify the location.
#
# You can also use osgBullet out of a source tree by specifying OSGBULLET_SOURCE_DIR
# and OSGBULLET_BUILD_DIR (in environment or CMake).


SET( OSGBULLET_BUILD_DIR "" CACHE PATH "If using osgBullet out of a source tree, specify the build directory." )
SET( OSGBULLET_SOURCE_DIR "" CACHE PATH "If using osgBullet out of a source tree, specify the root of the source tree." )
SET( OSGBULLET_ROOT "" CACHE PATH "Specify non-standard osgBullet install directory. It is the parent of the include and lib dirs." )

MACRO( FIND_OSGBULLET_INCLUDE THIS_OSGBULLET_INCLUDE_DIR THIS_OSGBULLET_INCLUDE_FILE )
    MARK_AS_ADVANCED( ${THIS_OSGBULLET_INCLUDE_DIR} )
    FIND_PATH( ${THIS_OSGBULLET_INCLUDE_DIR} ${THIS_OSGBULLET_INCLUDE_FILE}
        PATHS
            ${OSGBULLET_ROOT}
            $ENV{OSGBULLET_ROOT}
            ${OSGBULLET_SOURCE_DIR}
            $ENV{OSGBULLET_SOURCE_DIR}
            /usr/local
            /usr
            /sw/ # Fink
            /opt/local # DarwinPorts
            /opt/csw # Blastwave
            /opt
            "C:/Program Files/osgBullet"
            "C:/Program Files (x86)/osgBullet"
            ~/Library/Frameworks
            /Library/Frameworks
        PATH_SUFFIXES
            include
            .
    )
ENDMACRO( FIND_OSGBULLET_INCLUDE THIS_OSGBULLET_INCLUDE_DIR THIS_OSGBULLET_INCLUDE_FILE )

FIND_OSGBULLET_INCLUDE( OSGBULLET_INCLUDE_DIR osgBullet/OSGToCollada.h )
# message( STATUS ${OSGBULLET_INCLUDE_DIR} )

MACRO( FIND_OSGBULLET_LIBRARY MYLIBRARY MYLIBRARYNAME )
    MARK_AS_ADVANCED( ${MYLIBRARY} )
    MARK_AS_ADVANCED( ${MYLIBRARY}_debug )
    FIND_LIBRARY( ${MYLIBRARY}
        NAMES
            ${MYLIBRARYNAME}
        PATHS
            ${OSGBULLET_ROOT}
            $ENV{OSGBULLET_ROOT}
            ${OSGBULLET_BUILD_DIR}
            $ENV{OSGBULLET_BUILD_DIR}
            ~/Library/Frameworks
            /Library/Frameworks
            /usr/local
            /usr
            /sw
            /opt/local
            /opt/csw
            /opt
            "C:/Program Files/osgBullet"
            "C:/Program Files (x86)/osgBullet"
            /usr/freeware/lib64
        PATH_SUFFIXES
            lib
            .
    )
    FIND_LIBRARY( ${MYLIBRARY}_debug
        NAMES
            ${MYLIBRARYNAME}d
        PATHS
            ${OSGBULLET_ROOT}
            $ENV{OSGBULLET_ROOT}
            ${OSGBULLET_BUILD_DIR}
            $ENV{OSGBULLET_BUILD_DIR}
            ~/Library/Frameworks
            /Library/Frameworks
            /usr/local
            /usr
            /sw
            /opt/local
            /opt/csw
            /opt
            "C:/Program Files/osgBullet"
            "C:/Program Files (x86)/osgBullet"
            /usr/freeware/lib64
        PATH_SUFFIXES
            lib
            .
    )
#    message( STATUS ${${MYLIBRARY}} ${${MYLIBRARY}_debug} )
#    message( STATUS ${MYLIBRARYNAME} )
    IF( ${MYLIBRARY}_debug )
        SET( OSGBULLET_LIBRARIES ${OSGBULLET_LIBRARIES}
            "optimized" ${${MYLIBRARY}}
            "debug" ${${MYLIBRARY}_debug}
        )
    ELSE( ${MYLIBRARY}_debug )
        SET( OSGBULLET_LIBRARIES ${OSGBULLET_LIBRARIES} ${${MYLIBRARY}} )
    ENDIF( ${MYLIBRARY}_debug )
ENDMACRO(FIND_OSGBULLET_LIBRARY LIBRARY LIBRARYNAME)

FIND_OSGBULLET_LIBRARY( OSGBULLET_LIBRARY osgBullet )

IF( OSGBULLET_LIBRARIES AND OSGBULLET_INCLUDE_DIR )
    SET( OSGULLET_FOUND "YES")
ELSE( OSGBULLET_LIBRARIES AND OSGBULLET_INCLUDE_DIR )
    SET( OSGULLET_FOUND "NO" )
ENDIF( OSGBULLET_LIBRARIES AND OSGBULLET_INCLUDE_DIR )
