#add_definitions( -DVE_CONDUCTOR_DEFAULTPLUGIN_LIBRARY -DQT_PLUGIN)

file( GLOB srcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.cxx" )
#file( GLOB hdrs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.h" )

list(APPEND srcs 
    csvparser.cpp
)
set(VES_TARGET_NAME WarrantyUIqt)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Plugin)
set(VES_PLUGIN_INSTALL_DIR "conductor/plugins")

#include_directories(${GMTL_INCLUDE_DIRS})
include_directories(${VAPOR_INCLUDE_DIRS})
include_directories(${OSG_INCLUDE_DIRS})
include_directories(${XERCESC_INCLUDE_DIRS})

include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

set( MOC_HEADERS
    WarrantyToolPlugin_UIDialog.h
    WarrantyToolPluginFactory.h
    QueryResults.h)
set(VES_PRIVATE_HEADERS ${MOC_HEADERS} )

QT4_WRAP_CPP(${VES_TARGET_NAME}_MOCCED ${MOC_HEADERS} OPTIONS -p ./ )
list(APPEND srcs ${${VES_TARGET_NAME}_MOCCED})

QT4_ADD_RESOURCES( ${VES_TARGET_NAME}_RCCED WarrantyToolPluginIcons.qrc )
list(APPEND srcs ${${VES_TARGET_NAME}_RCCED})

QT4_WRAP_UI( UICED WarrantyToolPlugin_UIDialog.ui QueryResults.ui)
list(APPEND srcs ${UICED})

# workaround for conflicts between C++11 and the connect() macro
# in old Qt releases
# see "User-defined literals and whitespace" at:
# http://gcc.gnu.org/gcc-4.7/porting_to.html
if( CMAKE_COMPILER_IS_GNUCXX )
    set_source_files_properties( ${srcs}
                                 PROPERTIES
                                 COMPILE_FLAGS "-std=c++03" )
endif()

add_library(${VES_TARGET_NAME} MODULE ${srcs} ${VES_PRIVATE_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${QT_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_conductor_qtui)

set_target_properties( ${VES_TARGET_NAME} PROPERTIES SUFFIX ".bundle")

include(VESModuleInstall)

