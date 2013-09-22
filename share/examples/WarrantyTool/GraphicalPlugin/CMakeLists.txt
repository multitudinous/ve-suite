file( GLOB srcs "*.cxx" )
file( GLOB hdrs "*.h" )

list(APPEND srcs 
    csvparser.cpp
)

set(VES_PUBLIC_HEADERS ${hdrs} )

set(VES_TARGET_NAME WarrantyToolGP)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Plugin)
#set(VES_PLUGIN_INSTALL_DIR "xplorer/plugins")

include_directories(${QT_INCLUDE_DIRS})
include_directories(${VAPOR_INCLUDE_DIRS})
include_directories(${OSG_INCLUDE_DIRS})
include_directories(${GMTL_INCLUDE_DIRS})
include_directories(${VRJUGGLER_INCLUDE_DIRS})
include_directories(${XERCESC_INCLUDE_DIRS})
include_directories(${propertystore_INCLUDE_DIRS})
include_directories(${BULLET_INCLUDE_DIRS})
message( "osg path ${OSG_INCLUDE_DIRS}")
if(Squirrel_FOUND AND Sqrat_FOUND)
    include_directories(${Sqrat_INCLUDE_DIR})
    include_directories(${Squirrel_INCLUDE_DIR})
endif()

include_directories(${storyteller_INCLUDE_DIRS})

#include_directories(${PROJECT_SOURCE_DIR}/external)
#include_directories(${CMAKE_CURRENT_SOURCE_DIR})
#include_directories(${PROJECT_SOURCE_DIR}/external/qwt-6.0.0-rc5/src)
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/plugin)
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/extendedWidgets)
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/propertyBrowser)
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/minerva)

add_library(${VES_TARGET_NAME} MODULE ${srcs} ${VES_PRIVATE_HEADERS})

target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSG_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${VAPOR_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${QT_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${VRJUGGLER-OPENGL_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${XERCESC_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BULLET_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${propertystore_LIBRARIES})
if(Squirrel_FOUND AND Sqrat_FOUND)
    target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${Squirrel_LIBRARIES})
endif()
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${storyteller_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC qwt)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC osgQtTree)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_open_xml)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_xplorer_default)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_conductor_qtui)

if(APPLE)
    set_target_properties( ${VES_TARGET_NAME} PROPERTIES SUFFIX ".bundle")
endif()

include(VESModuleInstall)

