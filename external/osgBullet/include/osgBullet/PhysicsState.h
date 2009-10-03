// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#ifndef __OSGBULLET_PHYSICS_STATE_H__
#define __OSGBULLET_PHYSICS_STATE_H__ 1

#include <osg/Object>
#include <osg/Group>

#include "osgBullet/Export.h"
#include "osgBullet/OSGToCollada.h"

#include "osgTools/RefID.h"

#include <btBulletDynamicsCommon.h>


namespace osgBullet {


class OSGBULLET_EXPORT PhysicsData : public osg::Object
{
public:
    PhysicsData();
    PhysicsData( const PhysicsData& rhs, osg::CopyOp copyop=osg::CopyOp::SHALLOW_COPY );

    PhysicsData& operator=( const PhysicsData& rhs );

    META_Object(osgBullet,PhysicsData);

    unsigned int _version;

    std::string _fileName;
    osg::ref_ptr< osgBullet::CreationRecord > _cr;
    btRigidBody* _body;

    // For save / restore use only
    osg::Matrix _osgTransform;
    osg::Matrix _bodyWorldTransform;
    osg::Vec3 _linearVelocity;
    osg::Vec3 _angularVelocity;

protected:
    ~PhysicsData();
};

class OSGBULLET_EXPORT PhysicsState : public osg::Object
{
public:
    PhysicsState();
    PhysicsState( const osgBullet::PhysicsState& rhs, osg::CopyOp copyop=osg::CopyOp::SHALLOW_COPY );
    ~PhysicsState();

    META_Object(osgBullet,PhysicsState);

    void addPhysicsData( const osgTools::RefID* id, PhysicsData* pd );
    void removePhysicsData( const osgTools::RefID* id );

    typedef std::map< std::string, osg::ref_ptr< PhysicsData > > DataMap;
    const DataMap& getDataMap() const;

    void addPhysicsData( const osgTools::RefID* id, const btRigidBody* body );
    void addPhysicsData( const osgTools::RefID* id, const osgBullet::CreationRecord* cr );
    void addPhysicsData( const osgTools::RefID* id, const std::string& fileName );

    //void addPhysicsData( const osgTools::RefID* id, const btConstraint& constraint );

protected:
    DataMap _dataMap;
};

// namespace osgBullet
}

// __OSGBULLET_PHYSICS_STATE_H__
#endif
