# Locate gdal
# This module defines
# COLLADA_LIBRARY
# COLLADA_FOUND, if false, do not try to link to gdal 
# COLLADA_INCLUDE_DIR, where to find the headers
#
# $COLLADA_DIR is an environment variable that would
# correspond to the ./configure --prefix=$COLLADA_DIR

FIND_PATH( COLLADA_INCLUDE_DIR dae.h
        $ENV{COLLADADIR}
        $ENV{COLLADADIR}
        $ENV{COLLADA_DIR}
        $ENV{COLLADA_DIR}
        $ENV{OSGDIR}
        $ENV{OSGDIR}
        $ENV{OSG_ROOT}
        /usr/local/
        /usr/local//colladadom
        /usr//
        /usr//colladadom
        /sw # Fink
        /opt/local # DarwinPorts
        /opt/csw # Blastwave
        /opt/
        /usr/freeware/
        ~/Library/Frameworks
        /Library/Frameworks
    PATH_SUFFIXES
        include
)

FIND_LIBRARY( COLLADA_LIBRARY 
    NAMES collada_dom collada14dom ColladaDom
    PATHS
        $ENV{COLLADA_DIR}
        $ENV{COLLADADIR}
        $ENV{OSGDIR}
        $ENV{OSG_ROOT}
        /usr/local
        /usr
        /sw # Fink
        /opt/local # DarwinPorts
        /opt/csw # Blastwave
        /opt
        /usr/freeware
        ~/Library/Frameworks
        /Library/Frameworks
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

