add_definitions( -DVE_CONDUCTOR_DEFAULTPLUGIN_LIBRARY -DQT_PLUGIN)

file( GLOB srcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.cxx" )
#file( GLOB hdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.h" )

set(VES_TARGET_NAME ves_defaultplugin_qtui)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Plugin)
set(VES_PLUGIN_INSTALL_DIR "conductor/plugins")

#include_directories(${GMTL_INCLUDE_DIRS})
#include_directories(${OSG_INCLUDE_DIRS})
#include_directories(${XERCESC_INCLUDE_DIRS})

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

set( MOC_HEADERS
    DefaultPlugin_UIDialog.h
    #DefaultPlugin.h
    DefaultPluginFactory.h)
set(VES_PRIVATE_HEADERS ${MOC_HEADERS} )

QT4_WRAP_CPP(MOCCED ${MOC_HEADERS} OPTIONS -p ./ )
list(APPEND srcs ${MOCCED})

QT4_ADD_RESOURCES( RCCED DefaultPluginIcons.qrc )
list(APPEND srcs ${RCCED})

QT4_WRAP_UI( UICED DefaultPlugin_UIDialog.ui)
list(APPEND srcs ${UICED})

add_library(${VES_TARGET_NAME} MODULE ${srcs} ${VES_PRIVATE_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${QT_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_conductor_qtui)

set_target_properties( ${VES_TARGET_NAME} PROPERTIES SUFFIX ".bundle")

include(VESModuleInstall)

