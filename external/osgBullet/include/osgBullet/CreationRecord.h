// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_CREATION_RECORD_H__
#define __OSGBULLET_CREATION_RECORD_H__ 1


#include <osgBullet/Export.h>
#include <osgBullet/CollisionShapes.h>

#include <osg/Object>
#include <osg/Vec3>

#include <btBulletDynamicsCommon.h>

#include <string>



namespace osgBullet {



// Fill in this struct and pass it to the OSGToCollada constructor
// as a one-step config process, or first configure OSGToCollada
// and then get this record from it, and store it as UserData on
// the rigid body subgraph root node to facilitate saving and
// restoring physics state.
struct OSGBULLET_EXPORT CreationRecord : public osg::Object
{
    CreationRecord();
    CreationRecord( const CreationRecord& rhs, osg::CopyOp copyop=osg::CopyOp::SHALLOW_COPY );

    META_Object(osgBullet,CreationRecord);

    osg::Node* _sceneGraph;

    unsigned int _version;

    osg::Vec3 _com;
    bool _comSet;
    osg::Vec3 _scale;
    BroadphaseNativeTypes _shapeType;
    float _mass;

    // Reserved for future use.
    float _decimatorPercent;
    float _decimatorMaxError;
    bool  _decimatorIgnoreBoundaries;
    float _simplifyPercent;
    unsigned int _vertexAggMaxVerts;
    osg::Vec3 _vertexAggMinCellSize;
    float _reducerGroupThreshold;
    float _reducerMaxEdgeError;
    // END Reserved for future use.

    bool _overall;
    std::string _nodeName;
    osgBullet::AXIS _axis;
};


// namespace osgBullet
}

// __OSGBULLET_CREATION_RECORD_H__
#endif
