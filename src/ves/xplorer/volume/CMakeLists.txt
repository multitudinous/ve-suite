add_definitions( -DVE_TEXTURE_BASED_LIBRARY)

file( GLOB srcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.cxx" )
file( GLOB hdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.h" )

set(VES_TARGET_NAME ves_xplorer_volume)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Lib)
set(VES_INCLUDE_DIRECTORY_NAME "ves/xplorer/volume")

include_directories(${GMTL_INCLUDE_DIRS})
include_directories(${OSG_INCLUDE_DIRS})
include_directories(${VTK_INCLUDE_DIRS})
include_directories(${latticefx_INCLUDE_DIRS})

list(REMOVE_ITEM srcs SQLiteDriver.cxx)
list(REMOVE_ITEM hdrs SQLiteDriver.h)

list(REMOVE_ITEM srcs Database.cxx)
list(REMOVE_ITEM hdrs Database.h)

list(REMOVE_ITEM srcs TextureDataManager.cxx)
list(REMOVE_ITEM hdrs TextureDataManager.h)

list(REMOVE_ITEM srcs VectorData.cxx)
list(REMOVE_ITEM hdrs VectorData.h)

list(REMOVE_ITEM hdrs DatabaseDriver.h)
list(REMOVE_ITEM hdrs ScalarData.h)

set(VES_PUBLIC_HEADERS ${hdrs} )

add_library(${VES_TARGET_NAME} SHARED ${srcs} ${VES_PUBLIC_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSG_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${VTK_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${latticefx_LIBRARIES})

include(VESModuleInstall)

