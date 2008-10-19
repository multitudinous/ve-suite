# Find Bullet Physics SDK headers and libs
#
# This module defines:
#  BULLET_INCLUDE_DIR - Directory containing the bullet headers
#  BULLET_EXTRAS_INCLUDE_DIR - Directory containing the bullet "Extras" headers
#  BULLET_LIBRARIES - The path to all of the bullet libraries
#
# And paths to individual bullet libraries
#  BULLET_BulletCollision_LIBRARY
#  BULLET_BulletDynamics_LIBRARY
#  BULLET_BulletSoftBody_LIBRARY
#  BULLET_BulletLinearMath_LIBRARY
#  BULLET_BulletColladaConverter_LIBRARY
#
# To be useful, this script requires BULLET_ROOT to be set. If set in the
# environment, CMake will pick it up from there.  Alternately, you can set it
# on the cmake commandline using syntax like the following:
#
#  cmake -DBULLET_ROOT=/home/projects/bullet  ../../MyProject 
#
# If BULLET_ROOT is set on the commandline, it overrides any setting in the
# environment.
# 
# Alternately, you can set BULLET_SOURCE_DIR to point to the tree that bullet
# naturally unzips/unpacks-to, and BULLET_BUILD_DIR to point to wherever you
# built bullet. This is good for native, standalone, out-of-source builds which
# is the preferred method when using cmake.


FIND_PATH( BULLET_INCLUDE_DIR btBulletCollisionCommon.h
            PATHS
                ${BULLET_ROOT}
                $ENV{BULLET_ROOT}
                $ENV{BULLET_SOURCE_DIR}
            PATH_SUFFIXES
                /src
                /include
            )
IF( BULLET_INCLUDE_DIR )
    SET( BULLET_EXTRAS_INCLUDE_DIR ${BULLET_INCLUDE_DIR}/../Extras )
ENDIF( BULLET_INCLUDE_DIR )

MACRO( FIND_BULLET_LIBRARY_DIRNAME LIBNAME DIRNAME )
    FIND_LIBRARY( BULLET_${LIBNAME}_LIBRARY
        NAMES
            Lib${LIBNAME}
        PATHS
            ${BULLET_ROOT}
            $ENV{BULLET_ROOT}
            $ENV{BULLET_BUILD_DIR}
        PATH_SUFFIXES
            ./src/${DIRNAME}
            ./Extras/${DIRNAME}
            ./src/${DIRNAME}/release
            ./Extras/${DIRNAME}/release
            ./libs/${DIRNAME}
            ./libs
            ./lib
        )
    MARK_AS_ADVANCED( BULLET_${LIBNAME}_LIBRARY )
    SET( BULLET_LIBRARIES ${BULLET_LIBRARIES} ${BULLET_${LIBNAME}_LIBRARY} )
ENDMACRO( FIND_BULLET_LIBRARY_DIRNAME LIBNAME )


MACRO( FIND_BULLET_LIBRARY LIBNAME )
    FIND_BULLET_LIBRARY_DIRNAME( ${LIBNAME} ${LIBNAME} )
ENDMACRO( FIND_BULLET_LIBRARY LIBNAME )


FIND_BULLET_LIBRARY( BulletCollision )
FIND_BULLET_LIBRARY( BulletDynamics )
FIND_BULLET_LIBRARY( BulletSoftBody )
FIND_BULLET_LIBRARY( LinearMath )
FIND_BULLET_LIBRARY( BulletColladaConverter )
FIND_BULLET_LIBRARY_DIRNAME( XML LibXML )
FIND_BULLET_LIBRARY_DIRNAME( ColladaDom COLLADA_DOM )

# Hide BULLET_LIBRARY in the GUI, since most users can just ignore it
MARK_AS_ADVANCED( BULLET_LIBRARIES )

SET( BULLET_FOUND "NO" )
IF( BULLET_INCLUDE_DIR AND BULLET_LIBRARIES )
    SET( BULLET_FOUND "YES" )
ENDIF( BULLET_INCLUDE_DIR AND BULLET_LIBRARIES )
