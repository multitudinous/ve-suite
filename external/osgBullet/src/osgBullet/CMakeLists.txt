# DYNAMIC_OPENSCENEGRAPH doesn't show up in cmake and isn't defined.
# Comment out the 'if' login and just build dynamic until this is resolved.
# IF   (DYNAMIC_OPENSCENEGRAPH)
    ADD_DEFINITIONS(-DOSGBULLET_LIBRARY)
# ELSE (DYNAMIC_OPENSCENEGRAPH)
#     ADD_DEFINITIONS(-DOSGBULLET_LIBRARY_STATIC)
# ENDIF(DYNAMIC_OPENSCENEGRAPH)

SET( LIB_NAME osgBullet )

SET( HEADER_PATH ${PROJECT_SOURCE_DIR}/include/${LIB_NAME} )
SET( LIB_PUBLIC_HEADERS
    ${HEADER_PATH}/AbsoluteModelTransform.h
    ${HEADER_PATH}/BoundingCone.h
    ${HEADER_PATH}/BoundingCylinder.h
    ${HEADER_PATH}/Chart.h
    ${HEADER_PATH}/ColladaUtils.h
    ${HEADER_PATH}/CollisionShape.h
    ${HEADER_PATH}/CollisionShapes.h
    ${HEADER_PATH}/ComputeCylinderVisitor.h
    ${HEADER_PATH}/ComputeTriMeshVisitor.h
    ${HEADER_PATH}/DebugBullet.h
    ${HEADER_PATH}/Export.h
    ${HEADER_PATH}/GLDebugDrawer.h
    ${HEADER_PATH}/HandNode.h
    ${HEADER_PATH}/MotionState.h
    ${HEADER_PATH}/OSGToCollada.h
    ${HEADER_PATH}/RigidBody.h
    ${HEADER_PATH}/RigidBodyAnimation.h
    ${HEADER_PATH}/Utils.h
)

SN_ADD_SHARED_LIBRARY( ${LIB_NAME}
    ${OPENSCENEGRAPH_USER_DEFINED_DYNAMIC_OR_STATIC}
    ${LIB_PUBLIC_HEADERS}
    AbsoluteModelTransform.cpp
    BoundingCone.cpp
    BoundingCylinder.cpp
    Chart.cpp
    ColladaUtils.cpp
    CollisionShape.cpp
    CollisionShapes.cpp
    ComputeCylinderVisitor.cpp
    ComputeTriMeshVisitor.cpp
    DebugBullet.cpp
    GLDebugDrawer.cpp
    HandNode.cpp
    MotionState.cpp
    OSGToCollada.cpp
    RigidBody.cpp
    RigidBodyAnimation.cpp
    Utils.cpp
)

SN_LINK_LIBRARIES( ${LIB_NAME} osgSim osgText osgViewer osgGA osgDB osgUtil osg OpenThreads ${BULLET_LIBRARIES}  )
