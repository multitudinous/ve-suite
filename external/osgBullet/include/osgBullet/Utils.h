// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_UTILS_H__
#define __OSGBULLET_UTILS_H__

#include <osg/Matrix>
#include <osg/Vec3>
#include <osg/Vec4>

#include <LinearMath/btTransform.h>

#include <osgBullet/Export.h>


namespace osgBullet {

OSGBULLET_EXPORT osg::Matrix asOsgMatrix( const btTransform& t );
OSGBULLET_EXPORT btTransform asBtTransform( const osg::Matrix& m );

OSGBULLET_EXPORT osg::Vec3 asOsgVec3( const btVector3& v );
OSGBULLET_EXPORT btVector3 asBtVector3( const osg::Vec3& v );

OSGBULLET_EXPORT osg::Vec4 asOsgVec4( const btVector3& v, const double w );
OSGBULLET_EXPORT osg::Vec4 asOsgVec4( const btVector4& v );
OSGBULLET_EXPORT btVector4 asBtVector4( const osg::Vec4& v );

} // end namespace osgBullet

#endif // __OSGBULLET_UTILS_H__
