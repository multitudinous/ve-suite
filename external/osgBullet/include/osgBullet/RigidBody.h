// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_RIGIDBODY_H__
#define __OSGBULLET_RIGIDBODY_H__

#include <osg/Referenced>
#include <BulletDynamics/Dynamics/btRigidBody.h>

#include <osgBullet/Export.h>

namespace osgBullet {

/*!
    A reference-counted btRigidBody (allows it to be added
    as UserData to an OSG Node).
*/

class OSGBULLET_EXPORT RigidBody
    : public osg::Referenced
{
public:
    RigidBody( void );
    RigidBody( btRigidBody* rigidBody );

    void setRigidBody( btRigidBody* rigidBody )
    {
        _rigidBody = rigidBody;
    }

    btRigidBody* getRigidBody()
    {
        return( _rigidBody );
    }
    const btRigidBody * getRigidBody() const
    {
        return( _rigidBody );
    }

protected:
    virtual ~RigidBody( void );

    btRigidBody* _rigidBody;
};

} /* namespace */

#endif // __OSGBULLET_RIGIDBODY_H__
