add_definitions( -DVE_USER_PLUGIN_LIBRARY)

file( GLOB srcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.cxx" )
file( GLOB hdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.h" )

set(VES_TARGET_NAME DefaultGraphicalPlugin)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Plugin)
set(VES_PLUGIN_INSTALL_DIR "xplorer/plugins")

include_directories(${GMTL_INCLUDE_DIRS})
include_directories(${OSG_INCLUDE_DIRS})
include_directories(${XERCESC_INCLUDE_DIRS})

set(VES_PRIVATE_HEADERS ${hdrs} )

add_library(${VES_TARGET_NAME} MODULE ${srcs} ${VES_PRIVATE_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSG_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_xplorer_default)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_xplorer_scenegraph)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${XERCESC_LIBRARIES})

set_target_properties( ${VES_TARGET_NAME} PROPERTIES SUFFIX ".bundle")

include(VESModuleInstall)

