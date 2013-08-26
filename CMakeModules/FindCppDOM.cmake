# Find the CppDOM library
#
# If found, will define:
#
#  CPPDOM_FOUND
#  CPPDOM_INCLUDE_DIRS
#  CPPDOM_LIBRARIES

find_package( Flagpoll )

flagpoll_get_include_dirs( cppdom NO_DEPS )
flagpoll_get_library_dirs( cppdom NO_DEPS )
flagpoll_get_library_names( cppdom NO_DEPS )
flagpoll_get_module_version( cppdom NO_DEPS )

find_path( CPPDOM_INCLUDE_DIR cppdom/cppdom.h
           HINTS ${cppdom_FLAGPOLL_INCLUDE_DIRS} )

include( FindPackageHandleStandardArgs )

set( CPPDOM_FAIL_MESSAGE
     "Could NOT find CppDOM. Is the FLAGPOLL_PATH environment variable set?"
     CACHE INTERNAL "" )

if( MSVC )
    set( CPPDOM_LIBRARY_DIRS ${cppdom_FLAGPOLL_LIBRARY_DIRS} )
    string( REGEX REPLACE "[.]" "_" CPPDOM_VERSION_STRING ${cppdom_FLAGPOLL_MODULE_VERSION} )
    find_library( CPPDOM_LIBRARY NAMES "cppdom-${CPPDOM_VERSION_STRING}"
                  HINTS ${cppdom_FLAGPOLL_LIBRARY_DIRS} )
    find_library( CPPDOM_LIBRARY_DEBUG NAMES "cppdom_d-${CPPDOM_VERSION_STRING}"
                  HINTS ${cppdom_FLAGPOLL_LIBRARY_DIRS} )
    find_package_handle_standard_args( CppDOM
                                       REQUIRED_VARS CPPDOM_LIBRARY CPPDOM_INCLUDE_DIR
                                       VERSION_VAR cppdom_FLAGPOLL_MODULE_VERSION
                                       FAIL_MESSAGE ${CPPDOM_FAIL_MESSAGE} )
    if( CPPDOM_LIBRARY_DEBUG )
        set( CPPDOM_LIBRARIES optimized ${CPPDOM_LIBRARY} debug ${CPPDOM_LIBRARY_DEBUG} )
    else()
        set( CPPDOM_LIBRARIES ${CPPDOM_LIBRARY} )
    endif()
    mark_as_advanced( CPPDOM_LIBRARY_DIRS )
    mark_as_advanced( CPPDOM_LIBRARY )
    mark_as_advanced( CPPDOM_LIBRARY_DEBUG )
    link_directories( ${CPPDOM_LIBRARY_DIRS} )
else()    
    find_library( CPPDOM_LIBRARY NAMES ${cppdom_FLAGPOLL_LIBRARY_NAMES}
                  HINTS ${cppdom_FLAGPOLL_LIBRARY_DIRS} )
    find_package_handle_standard_args( CppDOM
                                       REQUIRED_VARS CPPDOM_LIBRARY CPPDOM_INCLUDE_DIR
                                       VERSION_VAR cppdom_FLAGPOLL_MODULE_VERSION
                                       FAIL_MESSAGE ${CPPDOM_FAIL_MESSAGE} )
    set( CPPDOM_LIBRARIES ${CPPDOM_LIBRARY} )
    mark_as_advanced( CPPDOM_LIBRARIES )
endif()

set( CPPDOM_INCLUDE_DIRS ${CPPDOM_INCLUDE_DIR} )

mark_as_advanced( CPPDOM_INCLUDE_DIR )
