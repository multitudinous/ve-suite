/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#ifndef OSGBULLET_BOUNDINGCONE_
#define OSGBULLET_BOUNDINGCONE_    1


#include <osg/Vec3>
#include <osgBullet/Export.h>

namespace osgBullet {

/* Used internally to store cone parameters. */
/* TBD not currently used. when we do support cones, consider not encapsulating parameters in a class. */
class OSGBULLET_EXPORT BoundingCone
{
public:
    BoundingCone( void );
    virtual ~BoundingCone( void );

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

    void expandBy( const BoundingCone& bc );

protected:
    float length;
    float radius;
    osg::Vec3 axis;
};

}

#endif
