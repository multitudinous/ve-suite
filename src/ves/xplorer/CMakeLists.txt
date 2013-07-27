add_subdirectory( data )
add_subdirectory( eventmanager )
add_subdirectory( scenegraph )
add_subdirectory( volume )
#add_subdirectory( DefaultGraphicalPlugin )

add_definitions( -DVE_XPLORER_LIBRARY
                 -DVE_XPLORER_NETWORK_LIBRARY
                 -DVE_GRAPHICALPLUGINS_LIBRARY)

file( GLOB srcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.cxx" )
file( GLOB hdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.h" )

file( GLOB_RECURSE tmpSrcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "event/*.cxx" )
file( GLOB_RECURSE tmpHdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "event/*.h" )
list(APPEND srcs ${tmpSrcs})
list(APPEND hdrs ${tmpHdrs})

file( GLOB tmpSrcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "device/*.cxx" )
file( GLOB tmpHdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "device/*.h" )
list(APPEND srcs ${tmpSrcs})
list(APPEND hdrs ${tmpHdrs})

file( GLOB tmpSrcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "environment/*.cxx" )
file( GLOB tmpHdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "environment/*.h" )
list(APPEND srcs ${tmpSrcs})
list(APPEND hdrs ${tmpHdrs})

file( GLOB tmpSrcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "network/*.cxx" )
file( GLOB tmpHdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "network/*.h" )
list(APPEND srcs ${tmpSrcs})
list(APPEND hdrs ${tmpHdrs})

file( GLOB tmpSrcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "plugin/*.cxx" )
file( GLOB tmpHdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "plugin/*.h" )
list(APPEND srcs ${tmpSrcs})
list(APPEND hdrs ${tmpHdrs})

file( GLOB tmpSrcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "behavior/*.cxx" )
file( GLOB tmpHdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "behavior/*.h" )
list(APPEND srcs ${tmpSrcs})
list(APPEND hdrs ${tmpHdrs})

set(VES_PRIVATE_HEADERS ${hdrs})
#set(VES_PUBLIC_HEADERS ${hdrs} )

set(VES_TARGET_NAME ves_xplorer)
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

add_library(${VES_TARGET_NAME} SHARED ${srcs} ${VES_PUBLIC_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSG_LIBRARIES} ${switchwire_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${propertystore_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${VAPOR_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_xplorer_scenegraph)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_open_xml)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_xplorer_volume)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_xplorer_eventmanager)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_xplorer_data)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSGWORKS_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSGBULLET_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${backdropFX_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${latticefx_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BULLET_LIBRARIES})

include(VESModuleInstall)

#install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
#        DESTINATION ves
#        FILES_MATCHING PATTERN "*.h"
#        PATTERN ".svn" EXCLUDE)

#see if we need to compile minerva in
#if minerva_options.isAvailable():
#	minerva_options.apply( lib_env )

#if osgal_options.isAvailable():
#   osgal_options.apply( lib_env )
#   lib_env.Append( CPPDEFINES = [ 'VE_SOUND' ] )

#lib_env.AppendUnique(CPPPATH = [pj(RootDir,'external','osgEphemeris','include')])

#lib_env.AppendUnique(LIBS = ['osgEphemerisVES'])
#lib_env.AppendUnique(LIBPATH = [pj('#',buildDir,'external','osgEphemeris')])

# New Physics work
#ApplyBulletVars(lib_env)

#see if we need to compile minerva in
#if minerva_options.isAvailable():
#	lib_env.AppendUnique( CPPDEFINES = ['MINERVA_GIS_SUPPORT'] )
#	srcDir = os.path.abspath( pj(RootDir, 'src', 'ves', 'xplorer','minerva') )
#	base_plugin_srcs = sca_util.getSourcesRecursive( srcDir )
#	base_header_srcs = sca_util.getHeadersRecursive( srcDir )
#	sources += map(lambda s: pj('minerva', s), base_plugin_srcs)
#	headers += map(lambda s: pj('minerva', s), base_header_srcs)
