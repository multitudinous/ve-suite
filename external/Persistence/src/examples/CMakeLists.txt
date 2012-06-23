set(MONGO_DRIVER_LIBS
    -L${Mongo_driver_PATH}
    -l mongoclient
    )
    
#Lets us address our internal includes as <Persistence/foo.h>
include_directories(
        ${CMAKE_SOURCE_DIR}/src
    )

#FILE( GLOB srcs "*.cxx" )
set( srcs main.cxx )
#FILE( GLOB hdrs "*.h" )

SET(TARGET_SRC ${srcs})
#SET(PUBLIC_HEADERS ${hdrs})

SET(TARGET_NAME PersistenceTest)
ADD_EXECUTABLE(${TARGET_NAME} ${TARGET_SRC})
TARGET_LINK_LIBRARIES(${TARGET_NAME} Persistence ${MONGO_DRIVER_LIBS} )


#SET(INSTALL_INCDIR include)
SET(INSTALL_BINDIR bin)

INSTALL(
        TARGETS ${TARGET_NAME}
        RUNTIME DESTINATION ${INSTALL_BINDIR}
    )

