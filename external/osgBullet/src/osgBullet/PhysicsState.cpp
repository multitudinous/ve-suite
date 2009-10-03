// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include <osg/Notify>
#include <osg/Object>
#include <osg/Group>

#include "osgBullet/PhysicsState.h"
#include "osgBullet/OSGToCollada.h"

#include "osgTools/RefID.h"

#include <btBulletDynamicsCommon.h>

namespace osgBullet
{


PhysicsData::PhysicsData()
  : _version( 1 ),
    _fileName( std::string( "" ) ),
    _cr( NULL ),
    _body( NULL )
{
}
PhysicsData::PhysicsData( const PhysicsData& rhs, osg::CopyOp copyop )
{
    (*this) = rhs;
}
PhysicsData::~PhysicsData()
{
}

PhysicsData&
PhysicsData::operator=( const PhysicsData& rhs )
{
    _version = rhs._version;
    _fileName = rhs._fileName;
    _cr = rhs._cr;
    _body = rhs._body;

    return( *this );
}


PhysicsState::PhysicsState()
{
}
PhysicsState::PhysicsState( const osgBullet::PhysicsState& rhs, osg::CopyOp copyop )
{
}
PhysicsState::~PhysicsState()
{
}

void
PhysicsState::addPhysicsData( const osgTools::RefID* id, PhysicsData* pd )
{
    if( _dataMap.find( id->str() ) != _dataMap.end() )
        osg::notify( osg::WARN ) << "Overwriting physics data for RefID " << id->str() << std::endl;

    _dataMap[ id->str() ] = pd;
}

void
PhysicsState::removePhysicsData( const osgTools::RefID* id )
{
    DataMap::iterator it = _dataMap.find( id->str() );
    if( it == _dataMap.end() )
        osg::notify( osg::WARN ) << "Can't erase non-extant RefID (RefID::operator<<() TBD)." << std::endl;
    else
        _dataMap.erase( it );
}

const PhysicsState::DataMap&
PhysicsState::getDataMap() const
{
    return( _dataMap );
}


void
PhysicsState::addPhysicsData( const osgTools::RefID* id, const btRigidBody* body )
{
    DataMap::iterator it = _dataMap.find( id->str() );
    if( it == _dataMap.end() )
    {
        osg::ref_ptr< PhysicsData > pd = new PhysicsData;
        pd->_body = const_cast< btRigidBody* >( body );
        _dataMap[ id->str() ] = pd.get();
    }
    else
    {
        it->second->_body = const_cast< btRigidBody* >( body );
    }
}

void
PhysicsState::addPhysicsData( const osgTools::RefID* id, const osgBullet::CreationRecord* cr )
{
    DataMap::iterator it = _dataMap.find( id->str() );
    if( it == _dataMap.end() )
    {
        osg::ref_ptr< PhysicsData > pd = new PhysicsData;
        pd->_cr = const_cast< CreationRecord* >( cr );
        _dataMap[ id->str() ] = pd.get();
    }
    else
    {
        it->second->_cr = const_cast< CreationRecord* >( cr );
    }
}

void
PhysicsState::addPhysicsData( const osgTools::RefID* id, const std::string& fileName )
{
    DataMap::iterator it = _dataMap.find( id->str() );
    if( it == _dataMap.end() )
    {
        osg::ref_ptr< PhysicsData > pd = new PhysicsData;
        pd->_fileName = fileName;
        _dataMap[ id->str() ] = pd.get();
    }
    else
    {
        it->second->_fileName = fileName;
    }
}


//void
//PhysicsState::addPhysicsData( const osgTools::RefID* id, const btConstraint& constraint )
//{
//}



// namespace osgBullet
}
