add_definitions(-DOSGQTTREE_LIBRARY)

file( GLOB_RECURSE srcs "*.cxx" )
file( GLOB_RECURSE hdrs "*.h" )

#set(VES_PRIVATE_HEADERS ${hdrs})
set(VES_PUBLIC_HEADERS ${hdrs} )

set(VES_TARGET_NAME osgQtTree)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Lib)
#file(RELATIVE_PATH VES_INCLUDE_DIRECTORY_NAME "${PROJECT_SOURCE_DIR}/src" ${CMAKE_CURRENT_SOURCE_DIR})

include_directories(${QT_INCLUDE_DIRS})
include_directories(${OSG_INCLUDE_DIRS})
include_directories(${osgWorks_INCLUDE_DIRS})

include_directories(${PROJECT_SOURCE_DIR}/external)
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

SET( HDRS_TO_MOC treemodel.h)
QT4_WRAP_CPP( MOCCED ${HDRS_TO_MOC} OPTIONS -p ./ )
list(APPEND srcs ${MOCCED})

SET( QRC_FILES treeicons.qrc )
QT4_ADD_RESOURCES( RCCED ${QRC_FILES} )
list(APPEND srcs ${RCCED})

add_library(${VES_TARGET_NAME} SHARED ${srcs} ${VES_PUBLIC_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSG_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${OSGWORKS_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${QT_LIBRARIES})

include(VESModuleInstall)

#install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
#        DESTINATION include/ves/xplorer
#        FILES_MATCHING PATTERN "*.h"
#        PATTERN ".svn" EXCLUDE)
