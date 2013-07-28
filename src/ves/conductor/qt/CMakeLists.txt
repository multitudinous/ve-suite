add_definitions(-DVE_GUIPLUGINS_LIBRARY 
                -DVE_CONDUCTOR_QTUI_LIBRARY)

file( GLOB_RECURSE srcs RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.cxx" )
#file( GLOB_RECURSE hdrs "*.h" )

#set(VES_PRIVATE_HEADERS ${hdrs})
set(VES_PUBLIC_HEADERS ${hdrs} )

set(VES_TARGET_NAME ves_conductor_qtui)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Lib)
set(VES_INCLUDE_DIRECTORY_NAME "ves/conductor/qt")

list(REMOVE_ITEM srcs DefaultPlugin/DefaultPlugin.cxx)
list(REMOVE_ITEM srcs DefaultPlugin/DefaultPluginFactory.cxx)
list(REMOVE_ITEM srcs DefaultPlugin/DefaultPlugin_UIDialog.cxx)

include_directories(${QT_INCLUDE_DIRS})
include_directories(${VAPOR_INCLUDE_DIRS})
include_directories(${OSG_INCLUDE_DIRS})
include_directories(${GMTL_INCLUDE_DIRS})
include_directories(${VRJUGGLER_INCLUDE_DIRS})
include_directories(${XERCESC_INCLUDE_DIRS})
include_directories(${propertystore_INCLUDE_DIRS})
include_directories(${BULLET_INCLUDE_DIRS})
include_directories(${Sqrat_INCLUDE_DIR})
include_directories(${Squirrel_INCLUDE_DIR})
include_directories(${storyteller_INCLUDE_DIRS})

include_directories(${PROJECT_SOURCE_DIR}/external)
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${PROJECT_SOURCE_DIR}/external/qwt-6.0.0-rc5/src)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/plugin)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/extendedWidgets)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/propertyBrowser)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/minerva)

include_directories(${PROJECT_BINARY_DIR}/src)

set( MOC_HEADERS
    MoveFrame.h
    IconStack.h
    TreeTab.h
    PreferencesTab.h
    plugin/PluginSelectionTab.h
    #plugin/UIPluginBase.h
    MainWindow.h
    UIElementQt.h
    ExtendedTabWidget.h
    NonScrollGraphicsView.h
    RecentFiles.h
    CameraTab.h
    #QtLogChannel.h
    extendedWidgets/ExtendedToolBar.h
    propertyBrowser/FileEdit.h
    propertyBrowser/NodeEdit.h
    Visualization.h
    Constraints.h
    ScriptingTab.h
    propertyBrowser/VesTreePropertyBrowser.h)

QT4_WRAP_CPP(MOCCED ${MOC_HEADERS} OPTIONS -p ./ )
list(APPEND srcs ${MOCCED})

set( QRC_FILES icons.qrc )
QT4_ADD_RESOURCES( RCCED ${QRC_FILES} )
list(APPEND srcs ${RCCED})

set( UI_FILES
    titlebar.ui
    RecentFiles.ui
    TreeTab.ui
    PreferencesTab.ui
    plugin/PluginSelectionTab.ui
    plugin/PluginPorts.ui
    MainWindow.ui
    Visualization.ui
    Constraints.ui
    CameraTab.ui
    ScriptingTab.ui)

#QT4_WRAP_UI( UICED ${UI_FILES})
FOREACH(it ${UI_FILES})
    get_filename_component(PARENT_DIR ${it} PATH)
    get_filename_component(FNAME ${it} NAME_WE)
    EXECUTE_PROCESS(COMMAND ${QT_UIC_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/${it} -o ${CMAKE_CURRENT_BINARY_DIR}/${PARENT_DIR}/ui_${FNAME}.h)
ENDFOREACH(it)

list(APPEND srcs ${UICED})

add_library(${VES_TARGET_NAME} SHARED ${srcs} ${VES_PUBLIC_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSG_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${VAPOR_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${QT_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${VRJUGGLER_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${XERCESC_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BULLET_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${propertystore_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${Squirrel_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${storyteller_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC qwt)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC osgQtTree)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_open_xml)
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ves_xplorer_default)

include(VESModuleInstall)

#install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
#        DESTINATION include/ves/xplorer
#        FILES_MATCHING PATTERN "*.h"
#        PATTERN ".svn" EXCLUDE)

add_subdirectory(DefaultPlugin)

