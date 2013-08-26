# Find the OpenGL module (libvrj-ogl) module from VR Juggler
#
# If found, will define:
#
#  VRJUGGLER-OPENGL_FOUND
#  VRJUGGLER-OPENGL_LIBRARIES

find_package( Flagpoll )

set( FIND_PACKAGE_ARGS )

if( VRJuggler-OpenGL_FIND_REQUIRED )
    list( APPEND FIND_PACKAGE_ARGS REQUIRED )
endif()

if( VRJuggler-OpenGL_FIND_QUIETLY )
    list( APPEND FIND_PACKAGE_ARGS QUIET )
endif()

find_package( VRJuggler ${FIND_PACKAGE_ARGS} )
find_package( OpenGL ${FIND_PACKAGE_ARGS} )

flagpoll_get_library_dirs( vrjuggler-opengl NO_DEPS )
flagpoll_get_library_names( vrjuggler-opengl NO_DEPS )
flagpoll_get_module_version( vrjuggler-opengl NO_DEPS )

include( FindPackageHandleStandardArgs )

set( VRJ-OGL_FAIL_MESSAGE
     "Could NOT find VRJuggler-OpenGL. Is the FLAGPOLL_PATH environment variable set?"
     CACHE INTERNAL "" )

if( MSVC )
    #set( VRJUGGLER-OPENGL_LIBRARY_DIRS ${vrjuggler-opengl_FLAGPOLL_LIBRARY_DIRS} )
    string( REGEX REPLACE "[.]" "_" VRJ-OGL_VERSION_STRING ${vrjuggler-opengl_FLAGPOLL_MODULE_VERSION} )
    find_library( VRJUGGLER-OPENGL_LIBRARY NAMES "vrj_ogl-${VRJ-OGL_VERSION_STRING}"
                  HINTS ${vrjuggler_FLAGPOLL_LIBRARY_DIRS} )
    find_library( VRJUGGLER-OPENGL_LIBRARY_DEBUG NAMES "vrj_ogl_d-${VRJ-OGL_VERSION_STRING}"
                  HINTS ${vrjuggler_FLAGPOLL_LIBRARY_DIRS} )
    find_package_handle_standard_args( VRJuggler-OpenGL
                                       REQUIRED_VARS VRJUGGLER-OPENGL_LIBRARY
                                       VERSION_VAR vrjuggler-opengl_FLAGPOLL_MODULE_VERSION
                                       FAIL_MESSAGE ${VRJ-OGL_FAIL_MESSAGE} )
    if( VRJUGGLER-OPENGL_LIBRARY_DEBUG )
        set( VRJUGGLER-OPENGL_LIBRARIES optimized ${VRJUGGLER-OPENGL_LIBRARY} debug ${VRJUGGLER-OPENGL_LIBRARY_DEBUG}
                                    ${OPENGL_LIBRARIES}
                                    ${VRJUGGLER_LIBRARIES} )
    else()
        set( VRJUGGLER-OPENGL_LIBRARIES ${VRJUGGLER-OPENGL_LIBRARY}
                                    ${OPENGL_LIBRARIES}
                                    ${VRJUGGLER_LIBRARIES} )
    endif()
    mark_as_advanced( VRJUGGLER-OPENGL_LIBRARY )
    mark_as_advanced( VRJUGGLER-OPENGL_LIBRARY_DEBUG )
    mark_as_advanced( VRJUGGLER-OPENGL_LIBRARY_DIRS )
    #link_directories( ${VRJUGGLER-OPENGL_LIBRARY_DIRS} )
else()
    find_library( VRJUGGLER-OPENGL_LIBRARY NAMES ${vrjuggler-opengl_FLAGPOLL_LIBRARY_NAMES}
                  HINTS ${vrjuggler-opengl_FLAGPOLL_LIBRARY_DIRS} )
    find_package_handle_standard_args( VRJuggler-OpenGL
                                       REQUIRED_VARS VRJUGGLER-OPENGL_LIBRARY
                                       VERSION_VAR vrjuggler-opengl_FLAGPOLL_MODULE_VERSION
                                       FAIL_MESSAGE ${VRJ-OGL_FAIL_MESSAGE} )
    set( VRJUGGLER-OPENGL_LIBRARIES ${VRJUGGLER-OPENGL_LIBRARY}
                                    ${OPENGL_LIBRARIES}
                                    ${VRJUGGLER_LIBRARIES} )
    mark_as_advanced( VRJUGGLER-OPENGL_LIBRARY )
endif()
