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


using namespace osgBullet;

MotionState::MotionState( const osg::Matrix& parentTransform,
                         const osg::Vec3& centerOfMass )
  : _parentTransform( parentTransform ),
    _com( centerOfMass )
{
    _transform.setIdentity();
}

// Sets both the transformation of the collision shape / rigid body in the
// physics simulation, as well as the matrix of the subgraph's parent
// Transform node for the OSG visual representation.
//
// Called by resetTransform() with the concatenation of the center of mass
// translation and the parent transform. Apps typically call setCenterOfMass()
// and setParentTransform during initialization; these routines then call
// resetTransform(), which calls here. This results in setting the initial position
// of both the rigid body and the visual representation.
//
// Bullet calls this method during the physics simulation to position
// collision shapes / rigid bodies.
//
// Note that the transformation of the collision shape is not the same as the
// transformation for the visual representation. MotionState supports off-origin visual
// representations, and thus subtracts out the center of mass before setting the
// transformation of the visual representation.
void
MotionState::setWorldTransform(const btTransform& worldTrans)
{
    // _transform is the model-to-world transformation used to place collision shapes
    // in the physics simulation. Bullet queries this with getWorldTransform().
    _transform = worldTrans;
    // This OSG matrix will be used to transform OSG debug geometry, if enabled.
    osg::Matrix dt = osgBullet::asOsgMatrix( _transform );

    // Compute the transformation of the OSG visual representation. This is
    // already off-origin, so multiply out the center of mass that is present
    // in dt (_transform).
    osg::Matrix invCom = osg::Matrix::translate( -_com );
    osg::Matrix t = invCom * dt;

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
        osg::notify( osg::WARN ) << "MotionState: Unsupported transform type: " << transform->className() << std::endl;
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
        osg::notify( osg::WARN ) << "MotionState: Unsupported transform type: " << transform->className() << std::endl;
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
    // Concatenate the center of mass and parent transforms, then call setWorldTransform.
    // This creates the initial model-to-world transform for both the collision shape /
    // rigid body and the OSG visual representation.
    osg::Matrix comM = osg::Matrix::translate( _com );
    setWorldTransform( osgBullet::asBtTransform( comM * _parentTransform ) );
}

