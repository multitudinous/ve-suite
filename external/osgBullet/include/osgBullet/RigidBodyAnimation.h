// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef OSGBULLET_RIGIDBODYANIMATION
#define OSGBULLET_RIGIDBODYANIMATION

#include <osg/NodeCallback>

#include <osgBullet/Export.h>

namespace osgBullet {

/*!
    This callback repositions an object within the
    Bullet simulation. Attach it as an update callback
    to an OSG MatrixTransform. The MT must have an
    osgBullet::RefRigidBody attached as UserData.
*/

class OSGBULLET_EXPORT RigidBodyAnimation : public osg::NodeCallback
{
public:
    RigidBodyAnimation( void );

    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

protected:
    virtual ~RigidBodyAnimation() { }
};

} // end namespace osgBullet

#endif // OSGBULLET_RIGIDBODYANIMATION
