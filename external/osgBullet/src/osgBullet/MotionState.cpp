/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#include <osgBullet/MotionState.h>

#include <osg/MatrixTransform>

#include <osgBullet/AbsoluteModelTransform.h>
#include <osgBullet/Utils.h>

#include <osg/Notify>
#include <osg/io_utils>

#include <iostream>

using namespace osgBullet;

MotionState::MotionState( const osg::Matrix& parentTransform,
                         const osg::Vec3& centerOfMass )
  : _parentTransform( parentTransform ),
    _com( centerOfMass )
{
    _transform.setIdentity();
}

void
MotionState::setWorldTransform(const btTransform& worldTrans)
{
    _transform = worldTrans;
    osg::Matrix dt = osgBullet::asOsgMatrix( _transform );
    //osg::Matrix tempScale = osg::Matrix::scale( _scale );
    //std::cout << tempScale << dt << std::endl;
    //tempScale = tempScale * dt;
    //std::cout << tempScale << dt << std::endl;
    osg::Matrix t = _invCom * dt;

    if( _mt.valid() )
        _mt->setMatrix( t );
    else if( _amt.valid() )
        _amt->setMatrix( t );

    if( _debugMT.valid() )
        _debugMT->setMatrix( dt );
    else if( _debugAMT.valid() )
        _debugAMT->setMatrix( dt );
}
void
MotionState::getWorldTransform(btTransform& worldTrans ) const
{
    worldTrans = _transform;
}


void
MotionState::setTransform( osg::Transform* transform )
{
    osg::MatrixTransform* mt( NULL );
    osgBullet::AbsoluteModelTransform* amt( NULL );
    if( mt = dynamic_cast< osg::MatrixTransform* >( transform ) )
        _mt = mt;
    else if( amt = dynamic_cast< osgBullet::AbsoluteModelTransform* >( transform ) )
        _amt = amt;
    else
        osg::notify( osg::WARN ) 
            << "MotionState::setTransform : Unsupported transform type: " 
            << transform->className() << std::endl;
}

osg::Transform*
MotionState::getTransform()
{
    if( _mt.valid() )
        return( _mt.get() );
    else if( _amt.valid() )
        return( _amt.get() );
    else
        return NULL;
}

const osg::Transform*
MotionState::getTransform() const
{
    if( _mt.valid() )
        return( _mt.get() );
    else if( _amt.valid() )
        return( _amt.get() );
    else
        return NULL;
}


void
MotionState::setDebugTransform( osg::Transform* transform )
{
    osg::MatrixTransform* mt( NULL );
    osgBullet::AbsoluteModelTransform* amt( NULL );
    if( mt = dynamic_cast< osg::MatrixTransform* >( transform ) )
        _debugMT = mt;
    else if( amt = dynamic_cast< osgBullet::AbsoluteModelTransform* >( transform ) )
        _debugAMT = amt;
    else
        osg::notify( osg::WARN ) 
            << "MotionState::setDebugTransform : Unsupported transform type: " 
            << transform->className() << std::endl;
}

osg::Transform*
MotionState::getDebugTransform()
{
    if( _debugMT.valid() )
        return( _debugMT.get() );
    else if( _debugAMT.valid() )
        return( _debugAMT.get() );
    else
        return NULL;
}

const osg::Transform*
MotionState::getDebugTransform() const
{
    if( _debugMT.valid() )
        return( _debugMT.get() );
    else if( _debugAMT.valid() )
        return( _debugAMT.get() );
    else
        return NULL;
}


void
MotionState::setParentTransform( const osg::Matrix m )
{
    _parentTransform = m;
    _scale = _parentTransform.getScale();
    resetTransform();
}

osg::Matrix
MotionState::getParentTransform() const
{
    return( _parentTransform );
}


void
MotionState::setCenterOfMass( const osg::Vec3& com )
{
    _com = com;
    _invCom = osg::Matrix::translate( -_com );
    resetTransform();
}

osg::Vec3
MotionState::getCenterOfMass() const
{
    return( _com );
}


void
MotionState::resetTransform()
{
    osg::Matrix comM = osg::Matrix::translate( _com );
    //osg::Matrix tempScale = osg::Matrix::scale( _scale );
    //tempScale = osg::Matrix::inverse( tempScale );
    //setWorldTransform( osgBullet::asBtTransform(  comM * tempScale * _parentTransform ) );
    setWorldTransform( osgBullet::asBtTransform(  comM * _parentTransform ) );
}

