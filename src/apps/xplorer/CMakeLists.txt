file( GLOB srcs "*.cxx" )
file( GLOB hdrs "*.h" )

add_definitions( -DSVN_VES_REVISION=\"\\\"9999\\\"\" )
if(APPLE)
list(APPEND srcs 
    VESDelegate.mm
    CocoaHelper.mm
    CocoaScreenResolution.mm
    vrjuggler.plist
    Info.plist
    ${PROJECT_SOURCE_DIR}/dist/installerImages/ve_icon128x128.icns
    ${PROJECT_SOURCE_DIR}/dist/darwin/PkgInfo
)

find_library(APPKIT_LIBRARY AppKit)
endif()

#set(VES_PRIVATE_HEADERS ${hdrs})
set(VES_PUBLIC_HEADERS ${hdrs} )

set(VES_TARGET_NAME ves_xplorer)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY App)

include_directories(${QT_INCLUDE_DIRS})
include_directories(${VAPOR_INCLUDE_DIRS})
include_directories(${OSG_INCLUDE_DIRS})
include_directories(${GMTL_INCLUDE_DIRS})
include_directories(${VRJUGGLER_INCLUDE_DIRS})
include_directories(${XERCESC_INCLUDE_DIRS})
include_directories(${propertystore_INCLUDE_DIRS})
include_directories(${BULLET_INCLUDE_DIRS})

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

add_executable(${VES_TARGET_NAME} ${srcs} ${VES_PUBLIC_HEADERS})

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
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${APPKIT_LIBRARY})

include(VESModuleInstall)

if(APPLE)
    set_target_properties(${VES_TARGET_NAME} PROPERTIES MACOSX_BUNDLE TRUE)
    SET_SOURCE_FILES_PROPERTIES(vrjuggler.plist PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
    SET_SOURCE_FILES_PROPERTIES(${PROJECT_SOURCE_DIR}/dist/installerImages/ve_icon128x128.icns PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
    SET_SOURCE_FILES_PROPERTIES(${PROJECT_SOURCE_DIR}/dist/darwin/PkgInfo PROPERTIES MACOSX_PACKAGE_LOCATION Resources/..)
    SET_SOURCE_FILES_PROPERTIES(Info.plist PROPERTIES MACOSX_PACKAGE_LOCATION Resources/..)
endif(APPLE)

