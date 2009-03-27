SET( LIB_NAME osgBullet )

ADD_DEFINITIONS(-DOSGBULLET_LIBRARY)
SET( HEADER_PATH ${PROJECT_SOURCE_DIR}/include/${LIB_NAME} )

SET( LIB_PUBLIC_HEADERS
    ${HEADER_PATH}/AbsoluteModelTransform.h
    ${HEADER_PATH}/BoundingCone.h
    ${HEADER_PATH}/BoundingCylinder.h
    ${HEADER_PATH}/Chart.h
    ${HEADER_PATH}/ColladaUtils.h
    ${HEADER_PATH}/CollectVerticesVisitor.h
    ${HEADER_PATH}/CollisionShape.h
    ${HEADER_PATH}/CollisionShapes.h
    ${HEADER_PATH}/ComputeCylinderVisitor.h
    ${HEADER_PATH}/ComputeTriMeshVisitor.h
    ${HEADER_PATH}/DebugBullet.h
    ${HEADER_PATH}/DecimatorOp.h
    ${HEADER_PATH}/Export.h
    ${HEADER_PATH}/GeometryModifier.h
    ${HEADER_PATH}/GeometryOperation.h
    ${HEADER_PATH}/GLDebugDrawer.h
    ${HEADER_PATH}/HandNode.h
    ${HEADER_PATH}/MotionState.h
    ${HEADER_PATH}/OSGToCollada.h
    ${HEADER_PATH}/RigidBody.h
    ${HEADER_PATH}/RigidBodyAnimation.h
    ${HEADER_PATH}/SimplifierOp.h
    ${HEADER_PATH}/Utils.h
    ${HEADER_PATH}/VertexAggOp.h
)

ADD_SHARED_LIBRARY_INTERNAL( ${LIB_NAME}
    ${OPENSCENEGRAPH_USER_DEFINED_DYNAMIC_OR_STATIC}
    ${LIB_PUBLIC_HEADERS}
    AbsoluteModelTransform.cpp
    BoundingCone.cpp
    BoundingCylinder.cpp
    Chart.cpp
    ColladaUtils.cpp
    CollectVerticesVisitor.cpp
    CollisionShape.cpp
    CollisionShapes.cpp
    ComputeCylinderVisitor.cpp
    ComputeTriMeshVisitor.cpp
    DebugBullet.cpp
    DecimatorOp.cpp
    GeometryModifier.cpp
    GeometryOperation.cpp
    GLDebugDrawer.cpp
    HandNode.cpp
    MotionState.cpp
    OSGToCollada.cpp
    SimplifierOp.cpp
    RigidBody.cpp
    RigidBodyAnimation.cpp
    Utils.cpp
    VertexAggOp.cpp
)

TARGET_LINK_LIBRARIES(
    ${LIB_NAME}
    ${OSG_LIBRARIES}
    ${BULLET_LIBRARIES}
)
