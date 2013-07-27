# - Find a backdropFX installation or build tree.
# The following variables are set if backdropFX is found.  If backdropFX is not
# found, backdropFX_FOUND is set to false.
#  backdropFX_FOUND         - Set to true when backdropFX is found.
#  backdropFX_USE_FILE      - CMake file to use backdropFX.
#  backdropFX_MAJOR_VERSION - The backdropFX major version number.
#  backdropFX_MINOR_VERSION - The backdropFX minor version number 
#                       (odd non-release).
#  backdropFX_BUILD_VERSION - The backdropFX patch level 
#                       (meaningless for odd minor).
#  backdropFX_INCLUDE_DIRS  - Include directories for backdropFX
#  backdropFX_LIBRARY_DIRS  - Link directories for backdropFX libraries

# The following cache entries must be set by the user to locate backdropFX:
#  backdropFX_DIR  - The directory containing backdropFXConfig.cmake.  
#             This is either the root of the build tree,
#             or the lib directory.  This is the 
#             only cache entry.


# Assume not found.
SET(backdropFX_FOUND 0)

# Construct consitent error messages for use below.
SET(backdropFX_DIR_DESCRIPTION "directory containing backdropFXConfig.cmake.  This is either the root of the build tree, or PREFIX/lib for an installation.")
SET(backdropFX_DIR_MESSAGE "backdropFX not found.  Set the backdropFX_DIR cmake cache entry to the ${backdropFX_DIR_DESCRIPTION}")

# Use the Config mode of the find_package() command to find backdropFXConfig.
# If this succeeds (possibly because backdropFX_DIR is already set), the
# command will have already loaded backdropFXConfig.cmake and set backdropFX_FOUND.
IF(NOT backdropFX_FOUND)
  FIND_PACKAGE(backdropFX QUIET NO_MODULE)
ENDIF(NOT backdropFX_FOUND)

#-----------------------------------------------------------------------------
IF(NOT backdropFX_FOUND)
  # backdropFX not found, explain to the user how to specify its location.
  IF(backdropFX_FIND_REQUIRED)
    MESSAGE(FATAL_ERROR ${backdropFX_DIR_MESSAGE})
  ELSE(backdropFX_FIND_REQUIRED)
    IF(NOT backdropFX_FIND_QUIETLY)
      MESSAGE(STATUS ${backdropFX_DIR_MESSAGE})
    ENDIF(NOT backdropFX_FIND_QUIETLY)
  ENDIF(backdropFX_FIND_REQUIRED)
ENDIF(NOT backdropFX_FOUND)
