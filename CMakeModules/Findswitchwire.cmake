# - Find a switchwire installation or build tree.
# The following variables are set if switchwire is found.  If switchwire is not
# found, switchwire_FOUND is set to false.
#  switchwire_FOUND         - Set to true when switchwire is found.
#  switchwire_USE_FILE      - CMake file to use switchwire.
#  switchwire_MAJOR_VERSION - The switchwire major version number.
#  switchwire_MINOR_VERSION - The switchwire minor version number 
#                       (odd non-release).
#  switchwire_BUILD_VERSION - The switchwire patch level 
#                       (meaningless for odd minor).
#  switchwire_INCLUDE_DIRS  - Include directories for switchwire
#  switchwire_LIBRARY_DIRS  - Link directories for switchwire libraries

# The following cache entries must be set by the user to locate switchwire:
#  switchwire_DIR  - The directory containing switchwireConfig.cmake.  
#             This is either the root of the build tree,
#             or the lib directory.  This is the 
#             only cache entry.


# Assume not found.
SET(switchwire_FOUND 0)

# Construct consitent error messages for use below.
SET(switchwire_DIR_DESCRIPTION "directory containing switchwireConfig.cmake.  This is either the root of the build tree, or PREFIX/lib for an installation.")
SET(switchwire_DIR_MESSAGE "switchwire not found.  Set the switchwire_DIR cmake cache entry to the ${switchwire_DIR_DESCRIPTION}")

# Use the Config mode of the find_package() command to find switchwireConfig.
# If this succeeds (possibly because switchwire_DIR is already set), the
# command will have already loaded switchwireConfig.cmake and set switchwire_FOUND.
IF(NOT switchwire_FOUND)
  FIND_PACKAGE(switchwire QUIET NO_MODULE)
ENDIF(NOT switchwire_FOUND)

#-----------------------------------------------------------------------------
IF(NOT switchwire_FOUND)
  # switchwire not found, explain to the user how to specify its location.
  IF(switchwire_FIND_REQUIRED)
    MESSAGE(FATAL_ERROR ${switchwire_DIR_MESSAGE})
  ELSE(switchwire_FIND_REQUIRED)
    IF(NOT switchwire_FIND_QUIETLY)
      MESSAGE(STATUS ${switchwire_DIR_MESSAGE})
    ENDIF(NOT switchwire_FIND_QUIETLY)
  ENDIF(switchwire_FIND_REQUIRED)
ENDIF(NOT switchwire_FOUND)
