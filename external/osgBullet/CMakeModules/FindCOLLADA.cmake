# Locate gdal
# This module defines
# COLLADA_LIBRARY
# COLLADA_FOUND, if false, do not try to link to gdal 
# COLLADA_INCLUDE_DIR, where to find the headers
#
# $COLLADA_DIR is an environment variable that would
# correspond to the ./configure --prefix=$COLLADA_DIR

FIND_PATH( COLLADA_INCLUDE_DIR dae.h
    PATHS
        $ENV{COLLADADIR}
        $ENV{COLLADA_DIR}
        $ENV{BULLET_SOURCE_DIR}/../Extras/COLLADA_DOM
        /usr/local/
        /usr/local//colladadom
        /usr//
        /usr//colladadom
        /sw
        /opt/local # DarwinPorts
        /opt/csw # Blastwave
        /opt/
        /usr/freeware/
        ~/Library/Frameworks
        /Library/Frameworks
    PATH_SUFFIXES
        include
        .
)

FIND_LIBRARY( COLLADA_LIBRARY 
    NAMES collada_dom collada14dom ColladaDom LibColladaDom
    PATHS
        $ENV{COLLADA_DIR}
        $ENV{COLLADADIR}
        /usr/local
        /usr
        /sw # Fink
        /opt/local # DarwinPorts
        /opt/csw # Blastwave
        /opt
        /usr/freeware
        ~/Library/Frameworks
        /Library/Frameworks
        $ENV{BULLET_BUILD_DIR}/Extras/COLLADA_DOM
        $ENV{BULLET_BUILD_DIR}/Extras/COLLADA_DOM/release
    PATH_SUFFIXES
        lib
        lib_dbg
        lib64
        .
)

SET( COLLADA_FOUND "NO" )
IF( COLLADA_LIBRARY AND COLLADA_INCLUDE_DIR )
    SET( COLLADA_FOUND "YES" )
ENDIF( COLLADA_LIBRARY AND COLLADA_INCLUDE_DIR )

