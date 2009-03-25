/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/CharacterController.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/MatrixTransform>
#include <osg/ShapeDrawable>

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/Character/btKinematicCharacterController.h>

#include <BulletCollision/CollisionDispatch/btGhostObject.h>

#include <BulletCollision/CollisionShapes/btCapsuleShape.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;
namespace vxs = ves::xplorer::scenegraph;

const double OneEightyDivPI = 57.29577951;
const double PIDivOneEighty = 0.0174532925;

////////////////////////////////////////////////////////////////////////////////
CharacterController::CharacterController()
    :
    mActive( false ),
    m1stPersonMode( false ),
    mStepForward( false ),
    mStepBackward( false ),
    mStrafeLeft( false ),
    mStrafeRight( false ),
    mJump( false ),
    mFlying( false ),
    mBufferSize( 0 ),
    mCharacterWidth( 1.83 ),
    mCharacterHeight( 3.83/*5.83*/ ),
    mLookAtOffsetZ( mCharacterHeight * 2.0 ),
    mCameraDistance( 20.0 ),
    mMinCameraDistance( 0.1 ),
    mMaxCameraDistance( 200.0 ),
    mDeltaZoom( 2.0 ),
    //This is the speed of the character in ft/s
    mSpeed( 10.0 ),
    //Average walk speed is 5 km/h -> 0.911344415 ft/s
    mMinSpeed( 1.0 ),
    //Usain Bolt's top 10m split 10m/0.82s -> 40 ft/s
    mMaxSpeed( 40.0 ),
    mTurnAngleX( 0.0 ),
    mTurnAngleZ( 0.0 ),
    mDeltaTurnAngleX( 0.0 ),
    mDeltaTurnAngleZ( 0.0 ),
    mTurnSpeed( 400.0 ),
    mWeightModifier( 0.0 ),
    mTotalWeight( 0.0 ),
    mCameraRotation(),
    mCharacter( NULL ),
    mGhostObject( NULL ),
    mMatrixTransform( NULL )
{
    SetBufferSizeAndWeights( 10, 0.6 );
}
////////////////////////////////////////////////////////////////////////////////
CharacterController::~CharacterController()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Initialize( btDynamicsWorld* dynamicsWorld )
{
    //Create physics mesh representation
    btBroadphaseInterface* broadphase = dynamicsWorld->getBroadphase();
    broadphase->getOverlappingPairCache()->setInternalGhostPairCallback(
        new btGhostPairCallback() );

    btTransform startTransform;
    startTransform.setIdentity();

    mGhostObject = new btPairCachingGhostObject();
    mGhostObject->setWorldTransform( startTransform );

    btConvexShape* capsuleShape =
        new btCapsuleShapeZ( mCharacterWidth, mCharacterHeight );
    mGhostObject->setCollisionShape( capsuleShape );
    mGhostObject->setCollisionFlags( btCollisionObject::CF_CHARACTER_OBJECT );

    btScalar stepHeight = btScalar( 0.4 );
    mCharacter =
        new btKinematicCharacterController(
            mGhostObject, capsuleShape, stepHeight );

    mCharacter->setUpAxis( 2 );
    //No gravity by default
    //This has no effect and has not been implemented in bullet yet
    mCharacter->setFallSpeed( 0.0 );

    //Create graphics mesh representation
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    osg::ref_ptr< osg::Capsule > capsule =
        new osg::Capsule(
            osg::Vec3( 0.0, 0.0, 0.0 ),
            mCharacterWidth, mCharacterHeight );
    osg::ref_ptr< osg::TessellationHints > hints = new osg::TessellationHints();
    osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
        new osg::ShapeDrawable( capsule.get(), hints.get() );

    hints->setDetailRatio( 1.0 );
    shapeDrawable->setColor( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
    geode->addDrawable( shapeDrawable.get() );

    mMatrixTransform = new osg::MatrixTransform();
    mMatrixTransform->addChild( geode.get() );
    vxs::SceneManager::instance()->GetModelRoot()->addChild(
        mMatrixTransform.get() );

    mMatrixTransform->setUpdateCallback(
        new CharacterTransformCallback( mGhostObject ) );

    mMatrixTransform->setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Destroy( btDynamicsWorld* dynamicsWorld )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::FirstPersonMode( bool onOff )
{
    m1stPersonMode = onOff;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::TurnOn()
{
    mMatrixTransform->setNodeMask( 1 );

    btDynamicsWorld* dynamicsWorld =
        vxs::PhysicsSimulator::instance()->GetDynamicsWorld();

    dynamicsWorld->addCollisionObject(
        mGhostObject, btBroadphaseProxy::CharacterFilter,
        btBroadphaseProxy::StaticFilter | btBroadphaseProxy::DefaultFilter );

    dynamicsWorld->addCharacter( mCharacter );

    Reset( dynamicsWorld );

    mActive = true;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::TurnOff()
{
    mMatrixTransform->setNodeMask( 0 );

    btDynamicsWorld* dynamicsWorld =
        vxs::PhysicsSimulator::instance()->GetDynamicsWorld();

    dynamicsWorld->removeCollisionObject( mGhostObject );

    dynamicsWorld->removeCharacter( mCharacter );

    mActive = false;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Jump()
{
    /*
    if( !CanJump() )
    {
        return;
    }

    btTransform xform;
    mRigidBody->getMotionState()->getWorldTransform( xform );
    btVector3 up = xform.getBasis()[ 2 ];
    up.normalize();

    btScalar magnitude =
        ( btScalar( 1.0 ) / mRigidBody->getInvMass() ) * btScalar( 8.0 );
    mRigidBody->applyCentralImpulse( up * magnitude );
    */
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Reset( btDynamicsWorld* dynamicsWorld )
{
    btDispatcher* dispatcher = dynamicsWorld->getDispatcher();
    btBroadphaseInterface* broadphase = dynamicsWorld->getBroadphase();
    broadphase->getOverlappingPairCache()->cleanProxyFromPairs(
        mGhostObject->getBroadphaseHandle(), dispatcher );

    mCharacter->reset();

    //Move the character so that its entire body is above the zero ground plane
    mCharacter->warp( btVector3( 0.0, 0.0, mCharacterHeight ) );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StepForward( bool onOff )
{
    mStepForward = onOff;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StepBackward( bool onOff )
{
    mStepBackward = onOff;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StrafeLeft( bool onOff )
{
    mStrafeLeft = onOff;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StrafeRight( bool onOff )
{
    mStrafeRight = onOff;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Rotate( double dx, double dy )
{
    double x = -dy;
    double z =  dx;
    /*
    double length = ::sqrtf( x * x + z * z );
    if( length != 0.0 )
    {
        double lengthRatio = 1 / length;
        x *= lengthRatio;
        z *= lengthRatio;
    }
    */

    mDeltaTurnAngleX += x * mTurnSpeed * PIDivOneEighty;
    mDeltaTurnAngleZ += z * mTurnSpeed * PIDivOneEighty;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Advance( btScalar dt )
{
    //Get current character transform
    btTransform xform = mGhostObject->getWorldTransform();

    //Calculate character rotation
    {
        //http://www.flipcode.com/archives/Smooth_Mouse_Filtering.shtml

        //Remove the oldest mouse movement from the history buffer
        mHistoryBuffer.pop_back();
        //Put the current mouse movement into the history buffer
        mHistoryBuffer.push_front(
            std::make_pair( mDeltaTurnAngleX, mDeltaTurnAngleZ ) );

        //Use a weighted average for the history buffer contents
        double totalValueX = 0.0;
        double totalValueZ = 0.0;
        for( unsigned int i = 0; i < mBufferSize; ++i )
        {
            totalValueX += mHistoryBuffer.at( i ).first * mWeights.at( i );
            totalValueZ += mHistoryBuffer.at( i ).second * mWeights.at( i );
        }

        mTurnAngleX += totalValueX / mTotalWeight;
        mTurnAngleZ += totalValueZ / mTotalWeight;

        btQuaternion xRotation( btVector3( 1.0, 0.0, 0.0 ), mTurnAngleX );
        btQuaternion zRotation( btVector3( 0.0, 0.0, 1.0 ), mTurnAngleZ );
        mCameraRotation = xRotation * zRotation;

        //QuatSlerp( , xform.getRotation(), dt, result );

        if( m1stPersonMode )
        {
            if( mFlying )
            {
                xform.setRotation( mCameraRotation );
            }
            else
            {
                xform.setRotation( zRotation );
            }

            mGhostObject->setWorldTransform( xform );
        }

        mDeltaTurnAngleX = 0.0;
        mDeltaTurnAngleZ = 0.0;
    }

    //Calculate character translation
    {
        btVector3 forwardDir = xform.getBasis()[ 1 ];
        forwardDir.normalize();

        btVector3 upDir = xform.getBasis()[ 2 ];
        upDir.normalize();

        btVector3 strafeDir = xform.getBasis()[ 0 ];
        strafeDir.normalize();

        btVector3 direction( 0.0, 0.0, 0.0 );
        btScalar speed = mSpeed * dt;

        if( mStepForward )
        {
            direction += forwardDir;
        }

        if( mStepBackward )
        {
            direction -= forwardDir;
        }

        if( mStrafeLeft )
        {
            direction -= strafeDir;
        }

        if( mStrafeRight )
        {
            direction += strafeDir;
        }

        mCharacter->setWalkDirection( direction * speed );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCamera()
{
    btTransform characterWorldTrans = mGhostObject->getWorldTransform();
    characterWorldTrans.setRotation( mCameraRotation );

    btVector3 up = characterWorldTrans.getBasis()[ 2 ];
    up.normalize();
    
	btVector3 backward = -characterWorldTrans.getBasis()[ 1 ];
    backward.normalize();

    btVector3 center = characterWorldTrans.getOrigin();
    btVector3 eye = center + /*up * mLookAtOffsetZ +*/ backward * mCameraDistance;

    btVector3 vVector = eye - center;
    vVector.normalize();
    btVector3 rVector( up.cross( vVector ) );
    rVector.normalize();
    btVector3 uVector( vVector.cross( rVector ) );
    uVector.normalize();

    //"Look at" character matrix
    gmtl::Matrix44d matrix;
    matrix.mData[ 0 ]  =  rVector[ 0 ];
    matrix.mData[ 1 ]  = -vVector[ 0 ];
    matrix.mData[ 2 ]  =  uVector[ 0 ];
    matrix.mData[ 3 ]  =  0.0;

    matrix.mData[ 4 ]  =  rVector[ 1 ];
    matrix.mData[ 5 ]  = -vVector[ 1 ];
    matrix.mData[ 6 ]  =  uVector[ 1 ];
    matrix.mData[ 7 ]  =  0.0;

    matrix.mData[ 8 ]  =  rVector[ 2 ];
    matrix.mData[ 9 ]  = -vVector[ 2 ];
    matrix.mData[ 10 ] =  uVector[ 2 ];
    matrix.mData[ 11 ] =  0.0;

    matrix.mData[ 12 ] = -rVector.dot( eye );
    matrix.mData[ 13 ] =  vVector.dot( eye );
    matrix.mData[ 14 ] = -uVector.dot( eye );
    matrix.mData[ 15 ] =  1.0;

    vxs::SceneManager::instance()->GetActiveNavSwitchNode()->SetMat( matrix );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Zoom( bool inOut )
{
    if( inOut )
    {
        mCameraDistance -= mDeltaZoom;
        if( mCameraDistance < mMinCameraDistance )
        {
            mCameraDistance = mMinCameraDistance;
        }
    }
    else
    {
        mCameraDistance += mDeltaZoom;
        if( mCameraDistance > mMaxCameraDistance )
        {
            mCameraDistance = mMaxCameraDistance;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterController::IsActive()
{
    return mActive;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::QuatSlerp(
    btQuaternion& from, btQuaternion& to, double t, btQuaternion& result )
{
    //btQuaternion to1;
    double to1[ 4 ];
    double omega, cosom, sinom, scale0, scale1;
    //Calculate cosine from the dot product
    //cosom = from.dot( to );
    cosom = from.x() * to.x() + from.y() * to.y() + from.z() * to.z() + from.w() * to.w();

    //Adjust signs if necessary
    if( cosom < 0.0 )
    {
        cosom = -cosom;
        //to1 = -to;
        to1[ 0 ] = -to.x();
        to1[ 1 ] = -to.y();
        to1[ 2 ] = -to.z();
        to1[ 3 ] = -to.w();
    }
    else 
    {
        //to1 = to;
        to1[ 0 ] = to.x();
        to1[ 1 ] = to.y();
        to1[ 2 ] = to.z();
        to1[ 3 ] = to.w();
    }

    //Calculate coefficients
    double delta = 0.1;
    if( ( 1.0 - cosom ) > delta )
    {
        //Standard case slerp
        omega = acos( cosom );
        sinom = sin( omega );
        scale0 = sin( ( 1.0 - t ) * omega ) / sinom;
        scale1 = sin( t * omega ) / sinom;
    }
    else
    {        
        //"from" and "to" quaternions are very close 
        //So we can do a linear interpolation
        scale0 = 1.0 - t;
        scale1 = t;
    }

    //Calculate final values
    //from *= scale0;
    //to1 *= scale1;
    //result = from + to1;
    result.setX( scale0 * from.x() + scale1 * to1[ 0 ] );
    result.setY( scale0 * from.y() + scale1 * to1[ 1 ] );
    result.setZ( scale0 * from.z() + scale1 * to1[ 2 ] );
    result.setW( scale0 * from.w() + scale1 * to1[ 3 ] );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::SetBufferSizeAndWeights(
    size_t bufferSize, double weightModifier )
{
    //Set
    mBufferSize = bufferSize;
    mWeightModifier = weightModifier;

    //Reset
    mTotalWeight = 0.0;
    mHistoryBuffer.clear();
    mHistoryBuffer.assign( mBufferSize, std::make_pair( 0.0, 0.0 ) );
    mWeights.clear();
    mWeights.assign( mBufferSize, 0.0 );
    
    //First weight is worth 100%
    mWeights.at( 0 ) = 1.0;
    mTotalWeight += mWeights.at( 0 );
    //Assign other weights based off weight modifier
    for( size_t i = 1; i < mBufferSize; ++i )
    {
        mWeights.at( i ) = mWeights.at( i - 1 ) * mWeightModifier;
        mTotalWeight += mWeights.at( i );
    }
}
////////////////////////////////////////////////////////////////////////////////
CharacterController::CharacterTransformCallback::CharacterTransformCallback(
    btCollisionObject* collisionObject )
    :
    mCollisionObject( collisionObject )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CharacterController::CharacterTransformCallback::CharacterTransformCallback(
    const CharacterTransformCallback& ctc )
    :
    osg::Object( ctc ),
    osg::NodeCallback( ctc ),
    mCollisionObject( ctc.mCollisionObject )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CharacterController::CharacterTransformCallback::~CharacterTransformCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::CharacterTransformCallback::operator()(
    osg::Node* node, osg::NodeVisitor* nv )
{
    osg::ref_ptr< osg::MatrixTransform > mt =
        static_cast< osg::MatrixTransform* >( node );

    if( mt.valid() && mCollisionObject )
    {
        btScalar ogl[ 16 ];
        mCollisionObject->getWorldTransform().getOpenGLMatrix( ogl );
        mt->setMatrix( osg::Matrixd( ogl ) );
    }

    traverse( node, nv );
}
////////////////////////////////////////////////////////////////////////////////
