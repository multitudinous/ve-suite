add_definitions(-DVE_EVENTMANAGER_LIBRARY)

file( GLOB_RECURSE srcs "*.cxx" )
file( GLOB_RECURSE hdrs "*.h" )

set(VES_PRIVATE_HEADERS ${hdrs})
#set(VES_PUBLIC_HEADERS ${hdrs} )

set(VES_TARGET_NAME ves_xplorer_eventmanager)
set(VES_TARGET_VERSION ${ves_VERSION})
set(VES_TARGET_EXPORT ${PROJECT_NAME}-targets)
set(VES_TARGET_CATEGORY Lib)
file(RELATIVE_PATH VES_INCLUDE_DIRECTORY_NAME "${PROJECT_SOURCE_DIR}/src" ${CMAKE_CURRENT_SOURCE_DIR})

include_directories(${switchwire_INCLUDE_DIRS})
include_directories(${VAPOR_INCLUDE_DIRS})
include_directories(${GADGETEER_INCLUDE_DIRS})

add_library(${VES_TARGET_NAME} SHARED ${srcs} ${VES_PUBLIC_HEADERS})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${BOOST_LIBRARIES} ${POCO_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${switchwire_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${propertystore_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${VAPOR_LIBRARIES})
target_link_libraries(${VES_TARGET_NAME} LINK_PUBLIC ${GADGETEER_LIBRARIES})

include(VESModuleInstall)

install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DESTINATION ves/xplorer
        FILES_MATCHING PATTERN "*.h"
        PATTERN ".svn" EXCLUDE)
