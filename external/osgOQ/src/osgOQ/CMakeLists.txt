SET( LIB_NAME osgOQ )
ADD_DEFINITIONS( -DOSGOQ_EXPORTS )


SET( HEADER_PATH ${osgOQ_SOURCE_DIR}/include/${LIB_NAME} )
SET( LIB_PUBLIC_HEADERS
    ${HEADER_PATH}/OcclusionQueryNode.h
    ${HEADER_PATH}/OcclusionQueryVisitor.h
    ${HEADER_PATH}/OptionLoader.h
    ${HEADER_PATH}/QueryGeometry.h
    ${HEADER_PATH}/QueryState.h
)

ADD_LIBRARY( ${LIB_NAME} SHARED 
    ${LIB_PUBLIC_HEADERS}
    OcclusionQueryNode.cpp
    OcclusionQueryVisitor.cpp
    OptionLoader.cpp
    QueryGeometry.cpp
    QueryState.cpp
)

TARGET_LINK_LIBRARIES( ${LIB_NAME}
    ${OPENGL_LIBRARIES}
)
LINK_INTERNAL( ${LIB_NAME}
    osgUtil
    osgDB
    osg
    OpenThreads
)
