// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osgBullet/RefRigidBody.h>


namespace osgBullet {

RefRigidBody::RefRigidBody( void )
{
}

RefRigidBody::RefRigidBody( btRigidBody* rigidBody )
  : _rigidBody( rigidBody )
{
}

RefRigidBody::~RefRigidBody( void )
{
}

// osgBullet
}
