// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_BOUNDINGCYLINDER_H__
#define __OSGBULLET_BOUNDINGCYLINDER_H__


#include <osg/Vec3>
#include <osgBullet/Export.h>

namespace osgBullet {

/* Used internally to store cylinder parameters. */
/* TBD consider not encapsulating parameters in a class. */
class OSGBULLET_EXPORT BoundingCylinder
{
public:
    BoundingCylinder( void );
    virtual ~BoundingCylinder( void );

    void init()
    {
        length = radius = 0.0f;
    }

    void setAxis( const osg::Vec3 & a )
    {
        axis = a;
        axis.normalize();
    }
    const osg::Vec3 & getAxis() const
    {
        return( axis );
    }

    void setRadius( float r )
    {
        radius = r;
    }
    float getRadius() const
    {
        return( radius );
    }

    void setLength( float l )
    {
        length = l;
    }
    float getLength() const
    {
        return( length );
    }

    void expandBy( const osg::Vec3& v );

    void expandBy( float x,
                   float y,
                   float z );

    void expandBy( const BoundingCylinder& bc );

protected:
    float length;
    float radius;
    osg::Vec3 axis;
};

} // end namespace osgBullet

#endif // __OSGBULLET_BOUNDINGCYLINDER_H__
