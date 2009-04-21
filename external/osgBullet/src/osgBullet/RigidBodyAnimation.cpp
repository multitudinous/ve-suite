// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osg/MatrixTransform>

#include <osgBullet/RigidBodyAnimation.h>
#include <osgBullet/RigidBody.h>

#include <osg/io_utils>

#include <iostream>

#include <btBulletCollisionCommon.h>

using namespace osgBullet;

RigidBodyAnimation::RigidBodyAnimation( void )
{
}

void RigidBodyAnimation::operator()( osg::Node* node, osg::NodeVisitor* nv )
{
    osg::MatrixTransform* matTrans = dynamic_cast< osg::MatrixTransform* >( node );

    if( node == NULL )
    {
        return;
    }

    RigidBody * rb = dynamic_cast< RigidBody * >( matTrans->getUserData() );
    if( rb == NULL )
    {
        return;
    }

    btRigidBody * body = rb->getRigidBody();
    if( body->getInvMass() != 0.0 )
    {
        return;
    }

    osg::Matrix mat = matTrans->getMatrix();

    osg::Vec3 o = mat.getTrans();
    btVector3 bo( o.x(), o.y(), o.z() );
    osg::Quat q = mat.getRotate();
    btQuaternion bq( q.x(), q.y(), q.z(), q.w() );

    btTransform tf;
    tf.setOrigin( bo );
    tf.setRotation( bq );

    rb->getRigidBody()->getMotionState()->setWorldTransform( tf );

    traverse( node, nv );
}
