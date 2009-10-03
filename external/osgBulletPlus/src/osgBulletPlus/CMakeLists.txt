SET( LIB_NAME osgBulletPlus )

ADD_DEFINITIONS(-DOSGBULLETPLUS_LIBRARY)
SET( HEADER_PATH ${PROJECT_SOURCE_DIR}/include/${LIB_NAME} )

SET( LIB_PUBLIC_HEADERS
    ${HEADER_PATH}/DataLoader.h
    ${HEADER_PATH}/Export.h
    ${HEADER_PATH}/HandNode.h
    ${HEADER_PATH}/OSGToCollada.h
    ${HEADER_PATH}/SaveRestore.h
    ${HEADER_PATH}/VertexAggOp.h
)

ADD_SHARED_LIBRARY_INTERNAL( ${LIB_NAME}
    ${LIB_PUBLIC_HEADERS}
    DataLoader.cpp
    HandNode.cpp
    OSGToCollada.cpp
    SaveRestore.cpp
    VertexAggOp.cpp
)

TARGET_LINK_LIBRARIES(
    ${LIB_NAME}
    ${OSG_LIBRARIES}
    ${BULLET_LIBRARIES}
    ${OSGBULLET_LIBRARIES}
    ${OSGWORKS_LIBRARIES}
)

INCLUDE( ModuleInstall REQUIRED )
