// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBBULLETPLUS_DATA_LOADER_H__
#define __OSGBBULLETPLUS_DATA_LOADER_H__ 1

#include "osgbBulletPlus/Export.h"

#include "osgbBulletPlus/OSGToCollada.h"

#include "osgbBullet/PhysicsState.h"
#include "osgbBullet/MotionState.h"
#include "osgbBullet/Utils.h"

#include <osg/NodeCallback>
#include <osg/Transform>
#include <osg/ProxyNode>
#include <osgDB/ReadFile>

#include <OpenThreads/Thread>
#include <OpenThreads/Mutex>
#include <OpenThreads/ScopedLock>

#include <iostream>
#include <string>


namespace osgbBulletPlus {



// Use a thread to call osgDB::readNodeFile.
class LoadNodeThread : public OpenThreads::Thread
{
public:
    LoadNodeThread( const std::string& fileName )
      : _fileName( fileName )
    {}

    ~LoadNodeThread()
    {}

    void run()
    {
        if( _fileName.empty() )
            _node = NULL;
        else
            _node = osgDB::readNodeFile( _fileName );
    }

    const std::string _fileName;
    osg::ref_ptr< osg::Node > _node;
};

// Use a thread to call osgDB::readObjectFile.
class LoadObjectThread : public OpenThreads::Thread
{
public:
    LoadObjectThread( const std::string& fileName )
      : _fileName( fileName )
    {}

    ~LoadObjectThread()
    {}

    void run()
    {
        if( _fileName.empty() )
            _object = NULL;
        else
            _object = osgDB::readObjectFile( _fileName );
    }

    const std::string _fileName;
    osg::ref_ptr< osg::Object > _object;
};



// BEGIN PROTO
//
// The following code was used in prototype development only
// and is not intended for production use.
//

class DataLoader;

class CreatePhysicsThread : public OpenThreads::Thread
{
public:
    CreatePhysicsThread( osg::Node* src, const osgbBullet::PhysicsData* pd )
      : _src( src ),
        _pd( pd )
    {}

    ~CreatePhysicsThread()
    {}

    void run()
    {
        // Copy the subgraph for use with OSGToCollada.
        osg::ref_ptr< osg::Node > sgCopy;
        osg::Group* asGrp = _src->asGroup();
        if( asGrp != NULL )
            sgCopy = new osg::Group( *asGrp, osg::CopyOp::DEEP_COPY_ALL );
        else
            sgCopy = new osg::Node( *_src, osg::CopyOp::DEEP_COPY_ALL );

        const osg::ref_ptr< osgbBullet::CreationRecord > cr( _pd->_cr );
        osgbBulletPlus::OSGToCollada converter( *cr );
        converter.setSceneGraph( sgCopy.get() );

        converter.convert();

        _rb = converter.getRigidBody();
        osgbBullet::MotionState* motion = new osgbBullet::MotionState;
        motion->setCenterOfMass( cr->_com );
        motion->setScale( cr->_scale );
        _rb->setMotionState( motion );
    }

    osg::ref_ptr< osg::Node > _src;
    osg::ref_ptr< const osgbBullet::PhysicsData > _pd;

    btRigidBody* _rb;
};

class OSGBBULLETPLUS_EXPORT DataLoader : public osg::NodeCallback
{
public:
    DataLoader();
    DataLoader( osg::Transform* attachPoint, const std::string& fileName, const osgbBullet::PhysicsData* pd, btDynamicsWorld* bw );
    DataLoader( osg::Node* loadedModel, const osgbBullet::PhysicsData* pd, btDynamicsWorld* bw );
    DataLoader( const DataLoader& rhs, osg::CopyOp copyop=osg::CopyOp::SHALLOW_COPY );

    META_Object(osgbBullet,DataLoader);

    virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

    void incrementStateSafe();
    bool loadComplete() const;

protected:
    // temp hack
    osgwTools::AbsoluteModelTransform* amt;

    ~DataLoader();

    void commonInit();

    osg::ref_ptr< osg::Transform > _attachPoint;
    const std::string _fileName;
    osg::ref_ptr< const osgbBullet::PhysicsData > _pd;
    btDynamicsWorld* _bw;

    enum OpState {
        INITIAL = 0,
        LOAD_MODEL = 1,
        CREATE_PHYSICS = 2,
        COMPLETE = 3,
        IDLE = 4
    };
    unsigned int _state;
    bool _complete;

    osg::ref_ptr< osg::Node > _loadedModel;

    mutable OpenThreads::Mutex _updateMutex;
    mutable OpenThreads::Mutex _completeMutex;
    LoadNodeThread* _lnt;
    CreatePhysicsThread* _cpt;
};

// END PROTO


// namespace osgbBulletPlus
}


// __OSGBBULLETPLUS_DATA_LOADER_H__
#endif