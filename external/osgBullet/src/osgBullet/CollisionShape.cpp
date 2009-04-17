// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osgBullet/CollisionShape.h>

namespace osgBullet {

CollisionShape::CollisionShape( void )
{
}

CollisionShape::CollisionShape( btCollisionShape* collisionShape )
: _collisionShape( collisionShape )
{
}

CollisionShape::~CollisionShape( void )
{
}

} // end namespace osgBullet
