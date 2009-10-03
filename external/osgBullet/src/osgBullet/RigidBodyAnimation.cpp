// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osg/MatrixTransform>

#include <osgBullet/RigidBodyAnimation.h>
#include <osgBullet/RefRigidBody.h>
#include <osgBullet/Utils.h>

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

    RefRigidBody* rb = dynamic_cast< RefRigidBody* >( matTrans->getUserData() );
    if( rb == NULL )
    {
        return;
    }

    btRigidBody* body = rb->getRigidBody();
    if( body->getInvMass() != 0.0 )
    {
        return;
    }

    osg::Matrix mat = matTrans->getMatrix();
    rb->getRigidBody()->getMotionState()->setWorldTransform(
        osgBullet::asBtTransform( mat ) );

    traverse( node, nv );
}
