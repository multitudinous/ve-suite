/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#include <osgBullet/Utils.h>
#include <osg/Matrix>
#include <LinearMath/btTransform.h>


using namespace osgBullet;


// Convert a btTransform to an OSG Matrix
osg::Matrix
osgBullet::asOsgMatrix( const btTransform& t )
{
    btScalar ogl[ 16 ];
    t.getOpenGLMatrix( ogl );
    osg::Matrix m( ogl );
    return m;
}

btTransform
osgBullet::asBtTransform( const osg::Matrix& m )
{
    const osg::Matrixd::value_type* oPtr = m.ptr();
    btScalar bPtr[ 16 ];
    int idx;
    for (idx=0; idx<16; idx++)
        bPtr[ idx ] = oPtr[ idx ];
    btTransform t;
    t.setFromOpenGLMatrix( bPtr );
    return t;
}


osg::Vec3
osgBullet::asOsgVec3( const btVector3& v )
{
    return osg::Vec3( v.x(), v.y(), v.z() );
}

btVector3
osgBullet::asBtVector3( const osg::Vec3& v )
{
    return btVector3( v.x(), v.y(), v.z() );
}


osg::Vec4
osgBullet::asOsgVec4( const btVector3& v, const double w )
{
    return osg::Vec4( v.x(), v.y(), v.z(), w );
}

osg::Vec4
osgBullet::asOsgVec4( const btVector4& v )
{
    return osg::Vec4( v.x(), v.y(), v.z(), v.w() );
}

btVector4
osgBullet::asBtVector4( const osg::Vec4& v )
{
    return btVector4( v.x(), v.y(), v.z(), v.w() );
}

