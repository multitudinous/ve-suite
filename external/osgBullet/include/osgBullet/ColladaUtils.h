// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC. All rights reserved.

#ifndef __COLLADA_UTILS_H__
#define __COLLADA_UTILS_H__

#include <osgBullet/Export.h>
#include <osg/Node>
#include <string>

class btDynamicsWorld;
class btRigidBody;

namespace osgBullet
{


OSGBULLET_EXPORT btRigidBody* loadDae( osg::Transform* node, const osg::NodePath& np, const std::string& daeName,
    btDynamicsWorld* dw=NULL );

}

#endif
