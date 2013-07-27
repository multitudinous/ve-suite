file( GLOB_RECURSE hdrs "*.h" )

source_group( "Header Files" FILES ${hdrs})

add_custom_target( HEADER_ONLY_TARGET SOURCES ${hdrs} )

set_target_properties( HEADER_ONLY_TARGET PROPERTIES PROJECT_LABEL "Util Headers" )

set_target_properties( HEADER_ONLY_TARGET PROPERTIES LINKER_LANGUAGE CXX)

install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} 
        DESTINATION include/ves
        FILES_MATCHING PATTERN "*.h"
        PATTERN ".svn" EXCLUDE
        PATTERN "icons" EXCLUDE)
