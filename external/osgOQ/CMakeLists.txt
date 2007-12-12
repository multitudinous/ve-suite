PROJECT( osgOQ )

###### external dependencies
FIND_PACKAGE( Doxygen )
INCLUDE (${CMAKE_ROOT}/Modules/FindOpenGL.cmake)

###### other options
# SET( CMAKE_VERBOSE_MAKEFILE 1 ) # spew lots of goodies

###### setup include, lib, bin directories for osg
INCLUDE_DIRECTORIES( ${osgOQ_SOURCE_DIR}/include $ENV{OSG_ROOT}/include $ENV{OSG_INC_DIR} /usr/X11R6/include )
LINK_DIRECTORIES( $ENV{OSG_ROOT}/lib $ENV{OSG_LIB_DIR} )

SET( OUTPUT_BINDIR ${PROJECT_BINARY_DIR}/bin )
MAKE_DIRECTORY( ${OUTPUT_BINDIR} )
SET( EXECUTABLE_OUTPUT_PATH ${OUTPUT_BINDIR} )

SET(CMAKE_MODULE_PATH "${osgOQ_SOURCE_DIR}/CMake;${CMAKE_MODULE_PATH}")
INCLUDE( CMakeMacros )

ADD_SUBDIRECTORY( src/osgOQ )
ADD_SUBDIRECTORY( src/osgdb_osgOQ )
ADD_SUBDIRECTORY( Test/ptViewer )
ADD_SUBDIRECTORY( Test/oqTest00 )
ADD_SUBDIRECTORY( Test/oqTest01 )
