/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#include <osgBullet/MotionState.h>
#include <osgBullet/Utils.h>
#include <osgBullet/TripleBuffer.h>

#include <osgTools/AbsoluteModelTransform.h>

#include <osg/MatrixTransform>
#include <osg/Notify>
#include <osg/io_utils>


using namespace osgBullet;

MotionState::MotionState( const osg::Matrix& parentTransform,
                         const osg::Vec3& centerOfMass )
  : _parentTransform( parentTransform ),
    _com( centerOfMass ),
    _scale( osg::Vec3( 1., 1., 1. ) ),
    _tb( NULL ),
    _tbIndex( 0 )
{
    _transform.setIdentity();
}

// Sets both the transformation of the collision shape / rigid body in the
// physics simulation, as well as the matrix of the subgraph's parent
// Transform node for the OSG visual representation.
//
// Called by resetTransform() with the transform (C/S P) -- center of mass
// over scale, concatenated with the initial parent transform.
// Apps typically call setCenterOfMass(), setScale(), and
// setParentTransform() during initialization; these routines then call
// resetTransform(), which calls here. This results in setting the initial
// position of both the rigid body and the visual representation.
//
// Bullet calls this method during the physics simulation to position
// collision shapes / rigid bodies.
//
// Note that the transformation of the collision shape is not the same as the
// transformation for the visual representation. MotionState supports
// off-origin and scaled visual representations, and thus compensates for
// differences between scaled and COM-translated collision shapes and
// unscaled and COM-untranslated OSG subgraphs.
void
MotionState::setWorldTransform(const btTransform& worldTrans)
{
    // Call the callback, if registered.
    if( _mscl.size() > 0 )
    {
        // Call only if position changed.
        const btVector3 delta( worldTrans.getOrigin() - _transform.getOrigin() );
        const float eps( (float)( 1e-5 ) );
        const bool quiescent( osg::equivalent( delta[ 0 ], 0.f, eps ) &&
            osg::equivalent( delta[ 1 ], 0.f, eps ) &&
            osg::equivalent( delta[ 2 ], 0.f, eps ) );
        if( !quiescent )
        {
            MotionStateCallbackList::iterator it;
            for( it = _mscl.begin(); it != _mscl.end(); it++ )
                (**it)( worldTrans );
        }
    }

    // _transform is the model-to-world transformation used to place collision shapes
    // in the physics simulation. Bullet queries this with getWorldTransform().
    _transform = worldTrans;

    if( _tb == NULL )
        setWorldTransformInternal( worldTrans );
    else
    {
        char* addr( _tb->writeAddress() );
        if( addr == NULL )
        {
            osg::notify( osg::WARN ) << "MotionState: No TripleBuffer write address." << std::endl;
            return;
        }
        float* fAddr = reinterpret_cast< float* >( addr + _tbIndex );
        worldTrans.getOpenGLMatrix( fAddr );
    }
}
void
MotionState::getWorldTransform(btTransform& worldTrans ) const
{
    worldTrans = _transform;
}

void
MotionState::setWorldTransformInternal( const btTransform& worldTrans )
{
    // This OSG matrix will be used to transform OSG debug geometry, if enabled.
    osg::Matrix dt = osgBullet::asOsgMatrix( worldTrans );

    // Compute the transformation of the OSG visual representation.
    const osg::Vec3 cs( _com[0]*_scale[0], _com[1]*_scale[1], _com[2]*_scale[2] );
    osg::Matrix negCS = osg::Matrix::translate( -cs );
    osg::Matrix scale = osg::Matrix::scale( _scale );
    osg::Matrix t = scale * negCS * dt;

    if( _mt.valid() )
        _mt->setMatrix( t );
    else if( _amt.valid() )
        _amt->setMatrix( t );
}


void
MotionState::setTransform( osg::Transform* transform )
{
    osg::MatrixTransform* mt( NULL );
    osgTools::AbsoluteModelTransform* amt( NULL );
    if( mt = dynamic_cast< osg::MatrixTransform* >( transform ) )
        _mt = mt;
    else if( amt = dynamic_cast< osgTools::AbsoluteModelTransform* >( transform ) )
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
MotionState::setParentTransform( const osg::Matrix m )
{
//    osg::notify( osg::ALWAYS) << "setParent" << m << std::endl;

    _parentTransform = osg::Matrix::orthoNormal( m );

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
MotionState::setScale( const osg::Vec3& scale )
{
    _scale = scale;
    resetTransform();
}
osg::Vec3
MotionState::getScale() const
{
    return( _scale );
}

MotionStateCallbackList&
MotionState::getCallbackList()
{
    return( _mscl );
}



void
MotionState::resetTransform()
{
    // Divide the center of mass by the scale, concatenate it with the parent transform,
    // then call setWorldTransform.
    // This creates the initial model-to-world transform for both the collision shape /
    // rigid body and the OSG visual representation.
    const osg::Vec3 cs( _com[0]*_scale[0], _com[1]*_scale[1], _com[2]*_scale[2] );
    osg::Matrix csMat = osg::Matrix::translate( cs );
    setWorldTransform( osgBullet::asBtTransform( csMat * _parentTransform ) );
}


void
MotionState::registerTripleBuffer( osgBullet::TripleBuffer* tb )
{
    _tb = tb;
    _tbIndex = tb->reserve( sizeof( float ) * 16 );
}

void
MotionState::updateTripleBuffer( const char* addr )
{
    const float* fAddr = reinterpret_cast< const float* >( addr + _tbIndex );
    btTransform trans;
    trans.setFromOpenGLMatrix( fAddr );
    setWorldTransformInternal( trans );
}

bool
osgBullet::TripleBufferMotionStateUpdate( osgBullet::MotionStateList& msl, osgBullet::TripleBuffer* tb )
{
    const char* addr = tb->beginRead();
    if( addr == NULL )
        // No updated buffer is available. No valid data.
        return( false );

    MotionStateList::const_iterator it;
    for( it = msl.begin(); it != msl.end(); it++ )
        (*it)->updateTripleBuffer( addr );

    tb->endRead();
    return( true );
}

