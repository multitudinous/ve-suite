// Copyright (c) 2009 Skew Matrix Software LLC. All rights reserved.

#include "osgBullet/CreationRecord.h"

#include <osgBullet/CollisionShapes.h>

#include <osg/Object>
#include <osg/Vec3>

#include <btBulletDynamicsCommon.h>

#include <string>


using namespace osgBullet;


CreationRecord::CreationRecord()
  : _sceneGraph( NULL ),
    _version( 1 ),
    _com( 0., 0., 0. ),
    _comSet( false ),
    _scale( osg::Vec3( 1., 1., 1. ) ),
    _shapeType( BOX_SHAPE_PROXYTYPE ),
    _mass( 1.f ),
    _decimatorPercent( 1.f ),
    _decimatorMaxError( FLT_MAX ),
    _decimatorIgnoreBoundaries( true ),
    _simplifyPercent( 1.f ),
    _vertexAggMaxVerts( 0 ),
    _vertexAggMinCellSize( osg::Vec3( 0., 0., 0.) ),
    _reducerGroupThreshold( 360.f ),
    _reducerMaxEdgeError( 360.f ),
    _overall( true ),
    _nodeName( "" ),
    _axis( osgBullet::Z )
{
}
CreationRecord::CreationRecord( const CreationRecord& rhs, osg::CopyOp copyop )
  : _sceneGraph( rhs._sceneGraph ),
    _version( rhs._version ),
    _com( rhs._com ),
    _comSet( rhs._comSet ),
    _scale( rhs._scale ),
    _shapeType( rhs._shapeType ),
    _mass( rhs._mass ),
    _decimatorPercent( rhs._decimatorPercent ),
    _decimatorMaxError( rhs._decimatorMaxError ),
    _decimatorIgnoreBoundaries( rhs._decimatorIgnoreBoundaries ),
    _simplifyPercent( rhs._simplifyPercent ),
    _vertexAggMaxVerts( rhs._vertexAggMaxVerts ),
    _vertexAggMinCellSize( rhs._vertexAggMinCellSize ),
    _reducerGroupThreshold( rhs._reducerGroupThreshold ),
    _reducerMaxEdgeError( rhs._reducerMaxEdgeError ),
    _overall( rhs._overall ),
    _nodeName( rhs._nodeName ),
    _axis( rhs._axis )
{
}


