/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#include <osgBullet/BoundingCylinder.h>
#include <osg/io_utils>
#include <iostream>


using namespace osgBullet;

BoundingCylinder::BoundingCylinder( void )
{
    axis = osg::Vec3( 0, 1, 0 );
    length = 0.0;
    radius = 0.0;
}

BoundingCylinder::~BoundingCylinder( void )
{
}

void BoundingCylinder::expandBy( const osg::Vec3& v )
{
    float nl, nr;

    nl = osg::absolute( v * axis );
    if( nl > length )
    {
        length = nl;
    }

    nr = sqrtf( v.length2() - nl * nl );
    if( nr > radius )
    {
        radius = nr;
    }
}

void BoundingCylinder::expandBy( float x,
                                 float y,
                                 float z )
{
    expandBy( osg::Vec3( x, y, z ) );
}

void BoundingCylinder::expandBy( const BoundingCylinder& bc )
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
