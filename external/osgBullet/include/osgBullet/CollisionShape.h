// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_COLLISIONSHAPE_H__
#define __OSGBULLET_COLLISIONSHAPE_H__

#include <osg/Referenced>
#include <BulletCollision/CollisionShapes/btCollisionShape.h>

#include <osgBullet/Export.h>

namespace osgBullet {

/*!
    A reference-counted btCollisionShape (allows it to be added
    as UserData to an OSG Node).
*/

class OSGBULLET_EXPORT CollisionShape
    : public osg::Referenced
{
public:
    CollisionShape( void );
    CollisionShape( btCollisionShape* collisionShape );

    void setCollisionShape( btCollisionShape* collisionShape )
    {
        _collisionShape = collisionShape;
    }

    btCollisionShape* getCollisionShape()
    {
        return( _collisionShape );
    }
    const btCollisionShape* getCollisionShape() const
    {
        return( _collisionShape );
    }

protected:
    ~CollisionShape( void );

    btCollisionShape* _collisionShape;
};

} /* namespace */

#endif // __OSGBULLET_RIGIDBODY_H__
