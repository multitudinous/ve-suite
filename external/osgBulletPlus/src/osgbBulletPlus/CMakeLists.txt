SET( LIB_NAME osgbBulletPlus )

ADD_DEFINITIONS(-DOSGBBULLETPLUS_LIBRARY)
SET( HEADER_PATH ${PROJECT_SOURCE_DIR}/include/${LIB_NAME} )

IF( USE_COLLADA )
    ADD_DEFINITIONS(-DOSGBP_USE_COLLADA)
ENDIF( USE_COLLADA )

SET( LIB_PUBLIC_HEADERS
    ${HEADER_PATH}/DataLoader.h
    ${HEADER_PATH}/Export.h
    ${HEADER_PATH}/HandNode.h
    ${HEADER_PATH}/OSGToCollada.h
    ${HEADER_PATH}/SaveRestore.h
    ${HEADER_PATH}/VertexAggOp.h
    ${HEADER_PATH}/OSGToCollada.h
)

ADD_SHARED_LIBRARY_INTERNAL( ${LIB_NAME}
    ${LIB_PUBLIC_HEADERS}
    DataLoader.cpp
    HandNode.cpp
    OSGToCollada.cpp
    PluginLoader.cpp
    SaveRestore.cpp
    VertexAggOp.cpp
    OSGToCollada.cpp
)

TARGET_LINK_LIBRARIES(
    ${LIB_NAME}
    ${OSG_LIBRARIES}
    ${BULLET_LIBRARIES}
    ${OSGBULLET_LIBRARIES}
    ${OSGWORKS_LIBRARIES}
)

INCLUDE( ModuleInstall REQUIRED )
