/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#include <osgBullet/BoundingCone.h>
#include <osg/io_utils>
#include <iostream>


using namespace osgBullet;

BoundingCone::BoundingCone( void )
{
    axis = osg::Vec3( 0, 1, 0 );
    length = 0.0;
    radius = 0.0;
}

BoundingCone::~BoundingCone( void )
{
}

void BoundingCone::expandBy( const osg::Vec3& v )
{
    float nl, nr;

    nl = v * axis;
    if( ( nl < 0.0 ) && ( nl < -length ) )
    {
        length = nl;
    }

    nr = sqrtf( v.length2() - nl * nl );
    if( nr > radius )
    {
        radius = nr;
    }
}

void BoundingCone::expandBy( float x,
                             float y,
                             float z )
{
    expandBy( osg::Vec3( x, y, z ) );
}

void BoundingCone::expandBy( const BoundingCone& bc )
{
    float a, b;

    a = osg::absolute( bc.getAxis() * axis );
    b = sqrtf( 1 - a * a );

    float nl = a * bc.getLength() + b * bc.getRadius();
    float nr = sqrtf( b * b * bc.getLength() * bc.getLength() + bc.getRadius() * bc.getRadius() );

    if( nl > length )
    {
        length = nl;
    }
    if( nr > radius )
    {
        radius = nr;
    }

    return;
}
