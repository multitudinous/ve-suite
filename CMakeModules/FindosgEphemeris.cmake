# - Find a osgEphemeris installation or build tree.
# The following variables are set if osgEphemeris is found.  If osgEphemeris is not
# found, osgEphemeris_FOUND is set to false.
#  osgEphemeris_FOUND         - Set to true when osgEphemeris is found.
#  osgEphemeris_USE_FILE      - CMake file to use osgEphemeris.
#  osgEphemeris_MAJOR_VERSION - The osgEphemeris major version number.
#  osgEphemeris_MINOR_VERSION - The osgEphemeris minor version number 
#                       (odd non-release).
#  osgEphemeris_BUILD_VERSION - The osgEphemeris patch level 
#                       (meaningless for odd minor).
#  osgEphemeris_INCLUDE_DIRS  - Include directories for osgEphemeris
#  osgEphemeris_LIBRARY_DIRS  - Link directories for osgEphemeris libraries

# The following cache entries must be set by the user to locate osgEphemeris:
#  osgEphemeris_DIR  - The directory containing osgEphemerisConfig.cmake.  
#             This is either the root of the build tree,
#             or the lib directory.  This is the 
#             only cache entry.


# Assume not found.
SET(osgEphemeris_FOUND 0)

# Construct consitent error messages for use below.
SET(osgEphemeris_DIR_DESCRIPTION "directory containing osgEphemerisConfig.cmake.  This is either the root of the build tree, or PREFIX/lib for an installation.")
SET(osgEphemeris_DIR_MESSAGE "osgEphemeris not found.  Set the osgEphemeris_DIR cmake cache entry to the ${osgEphemeris_DIR_DESCRIPTION}")

# Use the Config mode of the find_package() command to find osgEphemerisConfig.
# If this succeeds (possibly because osgEphemeris_DIR is already set), the
# command will have already loaded osgEphemerisConfig.cmake and set osgEphemeris_FOUND.
IF(NOT osgEphemeris_FOUND)
  FIND_PACKAGE(osgEphemeris QUIET NO_MODULE)
ENDIF(NOT osgEphemeris_FOUND)

#-----------------------------------------------------------------------------
IF(NOT osgEphemeris_FOUND)
  # osgEphemeris not found, explain to the user how to specify its location.
  IF(osgEphemeris_FIND_REQUIRED)
    MESSAGE(FATAL_ERROR ${osgEphemeris_DIR_MESSAGE})
  ELSE(osgEphemeris_FIND_REQUIRED)
    IF(NOT osgEphemeris_FIND_QUIETLY)
      MESSAGE(STATUS ${osgEphemeris_DIR_MESSAGE})
    ENDIF(NOT osgEphemeris_FIND_QUIETLY)
  ENDIF(osgEphemeris_FIND_REQUIRED)
ENDIF(NOT osgEphemeris_FOUND)
