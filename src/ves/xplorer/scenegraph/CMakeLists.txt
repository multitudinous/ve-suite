add_definitions( -DVE_SCENEGRAPH_LIBRARY 
                -DVE_SCENEGRAPH_UTILS_LIBRARY 
                -DVE_NURBS_LIBRARY 
                -DVE_NURBS_UTILS_LIBRARY)

file( GLOB_RECURSE srcs "*.cxx" )
file( GLOB_RECURSE hdrs "*.h" )

set(VES_PRIVATE_HEADERS ${hdrs})
#set(VES_PUBLIC_HEADERS ${hdrs} )

set(VES_TARGET_NAME ves_xplorer_scenegraph)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Lib)
file(RELATIVE_PATH VES_INCLUDE_DIRECTORY_NAME "${PROJECT_SOURCE_DIR}/src" ${CMAKE_CURRENT_SOURCE_DIR})

include_directories(${GMTL_INCLUDE_DIRS})
include_directories(${OSG_INCLUDE_DIRS})
include_directories(${switchwire_INCLUDE_DIRS})
include_directories(${propertystore_INCLUDE_DIRS})
include_directories(${VAPOR_INCLUDE_DIRS})
include_directories(${GADGETEER_INCLUDE_DIRS})
include_directories(${VTK_INCLUDE_DIRS})
include_directories(${XERCESC_INCLUDE_DIRS})
include_directories(${osgWorks_INCLUDE_DIRS})
include_directories(${osgBullet_INCLUDE_DIRS})
include_directories(${backdropFX_INCLUDE_DIRS})
include_directories(${latticefx_INCLUDE_DIRS})
include_directories(${VRJUGGLER_INCLUDE_DIRS})
include_directories(${BULLET_INCLUDE_DIRS})

#bullet_options.apply( lib_env )

#if osgal_options.isAvailable():
#   osgal_options.apply( lib_env )
#   lib_env.Append( CPPDEFINES = [ 'VE_SOUND' ] )
#else:
#   headers.remove( pj( 'physics','sound','Material.h') )
#   headers.remove( pj( 'physics','sound','RemoveSoundVisitor.h') )
#   headers.remove( pj( 'physics','sound','SoundTable.h') )
#   headers.remove( pj( 'physics','sound','SoundUtilities.h') )
#   sources.remove( pj( 'physics','sound','Material.cpp') )
#   sources.remove( pj( 'physics','sound','SoundTable.cpp') )
#   sources.remove( pj( 'physics','sound','SoundUtilities.cpp') )

add_library(${VES_TARGET_NAME} SHARED ${srcs} ${VES_PUBLIC_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSG_LIBRARIES} ${switchwire_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${propertystore_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${VAPOR_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_xplorer_eventmanager)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_open_xml)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSGWORKS_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSGBULLET_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${backdropFX_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${latticefx_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BULLET_LIBRARIES})

include(VESModuleInstall)

install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DESTINATION include/ves/xplorer
        FILES_MATCHING PATTERN "*.h"
        PATTERN ".svn" EXCLUDE)
