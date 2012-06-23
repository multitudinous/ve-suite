set( Persistence_LIBS )

set(MONGO_DRIVER_LIBS
    -L${Mongo_driver_PATH}
    -l mongoclient
    )
    
#Lets us address our internal includes as <Persistence/foo.h>
include_directories(
        ${CMAKE_SOURCE_DIR}/src
    )

#Lets us know we're building the library so export definition can be set up
#correctly for win32
IF(WIN32)
ADD_DEFINITIONS(-DPERSISTENCE_LIBRARY)
ENDIF (WIN32)

FILE( GLOB srcs "*.cxx" )
FILE( GLOB hdrs "*.h" )

SET(TARGET_SRC ${srcs})
SET(PUBLIC_HEADERS ${hdrs})

SET(TARGET_NAME Persistence)
ADD_LIBRARY(${TARGET_NAME} SHARED ${TARGET_SRC} ${PUBLIC_HEADERS})
TARGET_LINK_LIBRARIES(${TARGET_NAME} ${MONGO_DRIVER_LIBS} boost_system PocoDataSQLite )


SET(INSTALL_INCDIR include)
SET(INSTALL_BINDIR bin)
IF(WIN32)
    SET(INSTALL_LIBDIR bin)
    SET(INSTALL_ARCHIVEDIR lib)
ELSE()
    SET(INSTALL_LIBDIR lib${LIB_POSTFIX})
    SET(INSTALL_ARCHIVEDIR lib${LIB_POSTFIX})
ENDIF()


INSTALL(
        TARGETS ${TARGET_NAME}
        RUNTIME DESTINATION ${INSTALL_BINDIR}
        LIBRARY DESTINATION ${INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${INSTALL_ARCHIVEDIR}
    )

INSTALL(
        FILES       ${PUBLIC_HEADERS}
        DESTINATION ${INSTALL_INCDIR}/${TARGET_NAME}
    )
