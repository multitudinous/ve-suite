FIND_PACKAGE( wxWidgets REQUIRED base core gl )

# DYNAMIC_OPENSCENEGRAPH doesn't show up in cmake and isn't defined.
# Comment out the 'if' login and just build dynamic until this is resolved.
# IF   (DYNAMIC_OPENSCENEGRAPH)
    ADD_DEFINITIONS(-DOSGWXTREE_LIBRARY)
# ELSE (DYNAMIC_OPENSCENEGRAPH)
#     ADD_DEFINITIONS(-DOSGBULLET_LIBRARY_STATIC)
# ENDIF(DYNAMIC_OPENSCENEGRAPH)

SET( LIB_NAME osgWxTree )

SET( HEADER_PATH ${PROJECT_SOURCE_DIR}/include/${LIB_NAME} )
SET( LIB_PUBLIC_HEADERS
    ${HEADER_PATH}/Export.h
    ${HEADER_PATH}/PickHandler.h
    ${HEADER_PATH}/TreeControl.h
    ${HEADER_PATH}/Utils.h
)

SN_ADD_SHARED_LIBRARY( ${LIB_NAME}
    ${LIB_PUBLIC_HEADERS}
    PickHandler.cpp
    TreeControl.cpp
    Utils.cpp
)

ADD_DEFINITIONS( ${wxWidgets_DEFINITIONS} )
SN_LINK_LIBRARIES( ${LIB_NAME} ${wxWidgets_LIBRARIES} osg osgDB osgGA osgSim osgUtil osgViewer OpenThreads ${BULLET_LIBRARIES}  )
