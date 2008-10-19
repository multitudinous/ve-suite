// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_COLLISIONSHAPES_H__
#define __OSGBULLET_COLLISIONSHAPES_H__


#include <osgBullet/Export.h>
#include <osg/Node>

#include <btBulletCollisionCommon.h>

namespace osg {
    class Geode;
}

namespace osgBullet {


    /* Several utility functions to assist with creation of
       Bullet collision shapes from OSG geometry.
    */
enum AXIS
{
    X,
    Y,
    Z,
    _X,
    _Y,
    _Z
};

OSGBULLET_EXPORT btSphereShape*             btSphereCollisionShapeFromOSG( osg::Node* node );
OSGBULLET_EXPORT btBoxShape*                btBoxCollisionShapeFromOSG( osg::Node* node, const osg::BoundingBox* bb=NULL );
OSGBULLET_EXPORT btCylinderShape*           btCylinderCollisionShapeFromOSG( osg::Node*, AXIS axis = Y );
OSGBULLET_EXPORT btTriangleMeshShape*       btTriMeshCollisionShapeFromOSG( osg::Node * );
OSGBULLET_EXPORT btConvexTriangleMeshShape* btConvexTriMeshCollisionShapeFromOSG( osg::Node * );


/* Returns an OSG representation of the given bullet collision shape.
*/
OSGBULLET_EXPORT osg::Node* osgNodeFromBtCollisionShape( const btCollisionShape * btShape, const btTransform& trans = btTransform::getIdentity() );
OSGBULLET_EXPORT osg::Node* osgNodeFromBtCollisionShape( const btBoxShape * btShape, const btTransform& trans = btTransform::getIdentity() );
OSGBULLET_EXPORT osg::Node* osgNodeFromBtCollisionShape( const btSphereShape * btShape, const btTransform& trans = btTransform::getIdentity() );
OSGBULLET_EXPORT osg::Node* osgNodeFromBtCollisionShape( const btCylinderShape * btShape, const btTransform& trans = btTransform::getIdentity() );
OSGBULLET_EXPORT osg::Node* osgNodeFromBtCollisionShape( const btTriangleMeshShape * btShape, const btTransform& trans = btTransform::getIdentity() );
OSGBULLET_EXPORT osg::Node* osgNodeFromBtCollisionShape( const btConvexTriangleMeshShape * btShape, const btTransform& trans = btTransform::getIdentity() );

} // end namespace osgBullet

#endif // __OSGBULLET_COLLISIONSHAPES_H__
