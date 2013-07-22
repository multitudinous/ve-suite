# - Find a propertystore installation or build tree.
# The following variables are set if propertystore is found.  If propertystore is not
# found, propertystore_FOUND is set to false.
#  propertystore_FOUND         - Set to true when propertystore is found.
#  propertystore_USE_FILE      - CMake file to use propertystore.
#  propertystore_MAJOR_VERSION - The propertystore major version number.
#  propertystore_MINOR_VERSION - The propertystore minor version number 
#                       (odd non-release).
#  propertystore_BUILD_VERSION - The propertystore patch level 
#                       (meaningless for odd minor).
#  propertystore_INCLUDE_DIRS  - Include directories for propertystore
#  propertystore_LIBRARY_DIRS  - Link directories for propertystore libraries

# The following cache entries must be set by the user to locate propertystore:
#  propertystore_DIR  - The directory containing propertystoreConfig.cmake.  
#             This is either the root of the build tree,
#             or the lib directory.  This is the 
#             only cache entry.


# Assume not found.
SET(propertystore_FOUND 0)

# Construct consitent error messages for use below.
SET(propertystore_DIR_DESCRIPTION "directory containing propertystoreConfig.cmake.  This is either the root of the build tree, or PREFIX/lib for an installation.")
SET(propertystore_DIR_MESSAGE "propertystore not found.  Set the propertystore_DIR cmake cache entry to the ${propertystore_DIR_DESCRIPTION}")

# Use the Config mode of the find_package() command to find propertystoreConfig.
# If this succeeds (possibly because propertystore_DIR is already set), the
# command will have already loaded propertystoreConfig.cmake and set propertystore_FOUND.
IF(NOT propertystore_FOUND)
  FIND_PACKAGE(propertystore QUIET NO_MODULE)
ENDIF(NOT propertystore_FOUND)

#-----------------------------------------------------------------------------
IF(NOT propertystore_FOUND)
  # propertystore not found, explain to the user how to specify its location.
  IF(propertystore_FIND_REQUIRED)
    MESSAGE(FATAL_ERROR ${propertystore_DIR_MESSAGE})
  ELSE(propertystore_FIND_REQUIRED)
    IF(NOT propertystore_FIND_QUIETLY)
      MESSAGE(STATUS ${propertystore_DIR_MESSAGE})
    ENDIF(NOT propertystore_FIND_QUIETLY)
  ENDIF(propertystore_FIND_REQUIRED)
ENDIF(NOT propertystore_FOUND)
