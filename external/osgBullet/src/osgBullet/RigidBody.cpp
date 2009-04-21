// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osgBullet/RigidBody.h>

namespace osgBullet {

RigidBody::RigidBody( void )
{
}

RigidBody::RigidBody( btRigidBody* rigidBody )
: _rigidBody( rigidBody )
{
}

RigidBody::~RigidBody( void )
{
}

} // end namespace osgBullet
