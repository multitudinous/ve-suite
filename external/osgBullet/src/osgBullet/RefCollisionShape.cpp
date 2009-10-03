// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#include <osgBullet/RefCollisionShape.h>

namespace osgBullet {

RefCollisionShape::RefCollisionShape( void )
{
}

RefCollisionShape::RefCollisionShape( btCollisionShape* collisionShape )
: _collisionShape( collisionShape )
{
}

RefCollisionShape::~RefCollisionShape( void )
{
}

} // end namespace osgBullet
