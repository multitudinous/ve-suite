// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.

#ifndef __OSGBULLET_OSG_TO_COLLADA_H__
#define __OSGBULLET_OSG_TO_COLLADA_H__ 1


#include <osg/NodeVisitor>
#include <btBulletDynamicsCommon.h>
#include <osgBullet/CollisionShapes.h>
#include <osgBullet/Export.h>
#include <string>

namespace osgBullet {


// This is a support class for the osgbpp application, so that
// an app can programmatically do the same thing as osgbpp:
// Namely, convert an input scene graph into COLLADA physics data.
class OSGBULLET_EXPORT OSGToCollada
{
public:
    // NOTR: loadedModel is _not_ const. This _will_ alter your scene graph.
    // It flattens static transforms using the osgUtil::Optimizer.
    // It removes loaded ProxyNodes (again with the Optimizer).
    // If simplifyPercent is != 1.0, it runs the simplifier.
    //
    // 'overall' and 'nodeName' work together to determine how the Bullet collision
    // shape is created (which is used to create the rigid body that is written to
    // the Collada file):
    //   overall  nodeName   Results
    //    true     empty     A single collision shape is created based on the bounding volume of the entire 'sg' scene graph.
    //    true    non-empty  'sg' is searched until a node with name 'nodeName' is found, then a single collision shape is created based on the bounding volume of the subgraph rooted at that node.
    //    false    empty     Collision shapes are created for all Geodes in 'sg' and assembled into a compound collision shape.
    //    false   non-empty  'sg' is searched until a node with name 'nodeName' is found, then collision shapes are created for all Geodes below that node and assembled into a compound collision shape.
    // Typical expected usage is 'overall' false and 'nodeName' empty.
    OSGToCollada( 
        osg::Node* sg,
        const BroadphaseNativeTypes shapeType,
        const float mass = 1.f,
        const std::string& outputFileName = std::string( "" ),
        const float simplifyPercent = 1.f,
        const bool overall = true,
        const std::string& nodeName = std::string( "" ),
        const osgBullet::AXIS axis = osgBullet::Z // for cylinder alignment only, ignored otherwise.
        );

    btRigidBody* getRigidBody();

protected:
    btDynamicsWorld* initPhysics();
    btRigidBody* createRigidBody( btScalar mass,
        btCollisionShape* shape, btVector3 centerOfMass, osg::MatrixTransform* mt );


    btRigidBody* _rigidBody;
};


}

#endif
