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
    mCameraDistanceLERP( false ),
    mCameraDistanceSLERP( false ),
    mOccludeDistanceLERP( false ),
    mPreviousOccluder( false ),
    mBufferSize( 0 ),
    mCharacterWidth( 1.83 ),
    //The average height of a male in the U.S. is 5.83 ft
    mCharacterHeight( 3.83/*5.83*/ ),
    mLookAtOffsetZ( mCharacterHeight * 2.0 ),
    mCameraDistance( 50.0 ),
    mOccludeDistance( 50.0 ),
    mMinCameraDistance( 0.1 ),
    mMaxCameraDistance( 200.0 ),
    mDeltaZoom( 2.0 ),
    mCameraDistanceLERPdt( 0.0 ),
    mCameraDistanceSLERPdt( 0.0 ),
    mOccludeDistanceLERPdt( 0.0 ),
    mDeltaCameraDistanceLERP( 0.02 ),
    mDeltaCameraDistanceSLERP( 0.02 ),
    mDeltaOccludeDistanceLERP( 0.02 ),
    mFromCameraDistance( 0.0 ),
    mToCameraDistance( 0.0 ),
    mFromOccludeDistance( 0.0 ),
    mToOccludeDistance( 0.0 ),
    //This is the speed of the character in ft/s
    mSpeed( 15.0 ),
    //Slow walk speed is 5 km/h ~ 1.0 ft/s
    mMinSpeed( 1.0 ),
    //Usain Bolt's top 10m split 10m/0.82s ~ 40 ft/s
    mMaxSpeed( 40.0 ),
    mTurnAngleX( 0.0 ),
    mTurnAngleZ( 0.0 ),
    mDeltaTurnAngleX( 0.0 ),
    mDeltaTurnAngleZ( 0.0 ),
    mTurnSpeed( 7.0 ),
    mWeightModifier( 0.0 ),
    mTotalWeight( 0.0 ),
    mCameraRotation( 0.0, 0.0, 0.0, 1.0 ),
    mFromCameraRotation( 0.0, 0.0, 0.0, 1.0 ),
    mToCameraRotation( 0.0, 0.0, 0.0, 1.0 ),
    mCharacter( NULL ),
    mGhostObject( NULL ),
    mMatrixTransform( NULL ),
    mLineSegmentIntersector( NULL )
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
    hints->setDetailRatio( 1.0 );
    osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
        new osg::ShapeDrawable( capsule.get(), hints.get() );
    shapeDrawable->setName( "Character" );
    shapeDrawable->setColor( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
    geode->addDrawable( shapeDrawable.get() );

    mMatrixTransform = new osg::MatrixTransform();
    mMatrixTransform->addChild( geode.get() );
    vxs::SceneManager::instance()->GetModelRoot()->addChild(
        mMatrixTransform.get() );

    mMatrixTransform->setUpdateCallback(
        new CharacterTransformCallback( mGhostObject ) );

    mMatrixTransform->setNodeMask( 0 );

    //Used for center to eye occluder test per frame
    mLineSegmentIntersector =
        new osgUtil::LineSegmentIntersector(
            osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) );
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

    Reset();

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
void CharacterController::Reset()
{
    btDynamicsWorld* dynamicsWorld =
        vxs::PhysicsSimulator::instance()->GetDynamicsWorld();
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

    mDeltaTurnAngleX += x;
    mDeltaTurnAngleZ += z;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Advance( btScalar dt )
{
    //Get current character transform
    btTransform xform = mGhostObject->getWorldTransform();

    //Update the device input history buffer
    std::pair< double, double > deltaDeviceInput = UpdateHistoryBuffer();

    //Calculate character rotation
    if( deltaDeviceInput.first != 0.0 || deltaDeviceInput.second != 0.0 )
    {
        mTurnAngleX += deltaDeviceInput.first;
        //Restrict movement about the x-axis from -PI/2 to PI/2
        if( mTurnAngleX < -gmtl::Math::PI_OVER_2 )
        {
            mTurnAngleX = -gmtl::Math::PI_OVER_2;
        }
        else if( mTurnAngleX > gmtl::Math::PI_OVER_2 )
        {
            mTurnAngleX = gmtl::Math::PI_OVER_2;
        }
        mTurnAngleZ += deltaDeviceInput.second;

        btQuaternion xRotation( btVector3( 1.0, 0.0, 0.0 ), mTurnAngleX );
        btQuaternion zRotation( btVector3( 0.0, 0.0, 1.0 ), mTurnAngleZ );
        mCameraRotation = xRotation * zRotation;

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

        //Normalize the movement
        if( direction.length() > 0.0 )
        {
            direction = direction.normalize();
        }

        mCharacter->setWalkDirection( direction * speed );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCamera()
{
    //lerp mCameraDistance if necessary
    if( mCameraDistanceLERP )
    {
        CameraDistanceLERP();
    }

    //slerp mCameraRotation if necessary
    if( mCameraDistanceSLERP )
    {
        CameraDistanceSLERP();
    }

    //Get the current character transform
    btTransform characterWorldTrans = mGhostObject->getWorldTransform();
    //Set the rotation with the camera's rotation
    characterWorldTrans.setRotation( mCameraRotation );

    //Get the up vector of the camera
    btVector3 up = characterWorldTrans.getBasis()[ 2 ];
    up.normalize();
    
    //Get the backward direction of the camera
	btVector3 backward = -characterWorldTrans.getBasis()[ 1 ];
    backward.normalize();

    //Get the center of the character
    btVector3 center = characterWorldTrans.getOrigin();
    //Calculate where the position of the eye is w/ no occluders
    btVector3 eye = center + backward * mCameraDistance;

    //Test for occluder between the eye and "look at" point
    EyeToCenterRayTest( eye, center );

    //If there is an occluder in front of the eye, move directly to it
    //If there is an occluder behind the eye, lerp back to it
    if( mOccludeDistanceLERP )
    {
        OccludeDistanceLERP();

        //Calculate where the position of the eye is w/ occluders
        eye = center + backward * mOccludeDistance;
    }

    //Move the camera to look at the center of the character
    LookAt( eye, center, up );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Zoom( bool inOut )
{
    mFromCameraDistance = mCameraDistance;
    if( mCameraDistanceLERPdt == 0.0 )
    {
        mToCameraDistance = mFromCameraDistance;
    }

    if( inOut )
    {
        mToCameraDistance -= mDeltaZoom;
        if( mToCameraDistance < mMinCameraDistance )
        {
            mToCameraDistance = mMinCameraDistance;
        }
    }
    else
    {
        mToCameraDistance += mDeltaZoom;
        if( mToCameraDistance > mMaxCameraDistance )
        {
            mToCameraDistance = mMaxCameraDistance;
        }
    }

    mCameraDistanceLERPdt = 0.0;
    mCameraDistanceLERP = true;
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterController::IsActive()
{
    return mActive;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::CameraDistanceLERP()
{
    if( mCameraDistanceLERPdt < ( 1.0 - mDeltaCameraDistanceLERP ) )
    {
        mCameraDistanceLERPdt += mDeltaCameraDistanceLERP;

        mCameraDistance =
            mFromCameraDistance +
            ( mToCameraDistance - mFromCameraDistance ) * mCameraDistanceLERPdt;
    }
    else
    {
        mCameraDistance = mToCameraDistance;

        mCameraDistanceLERP = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::CameraDistanceSLERP()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::OccludeDistanceLERP()
{
    if( mOccludeDistanceLERPdt < ( 1.0 - mDeltaOccludeDistanceLERP ) )
    {
        mOccludeDistanceLERPdt += mDeltaOccludeDistanceLERP;

        mOccludeDistance =
            mFromOccludeDistance +
            ( mToOccludeDistance - mFromOccludeDistance ) *
            mOccludeDistanceLERPdt;
    }
    else
    {
        mOccludeDistance = mToOccludeDistance;

        mOccludeDistanceLERP = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::EyeToCenterRayTest(
    btVector3& eye, btVector3& center )
{
    //Bullet implementation: only works for geometry w/ active physics
    /*
    btCollisionWorld::RayResultCallback rayCallback( eye, center );
    btDynamicsWorld* dynamicsWorld =
        vxs::PhysicsSimulator::instance()->GetDynamicsWorld();
    dynamicsWorld->rayTest( center, eye, rayCallback );
    if( rayCallback.hasHit() )
    {
        btCollisionObject* collisionObject = rayCallback.m_collisionObject;
        if( collisionObject != mGhostObject )
        {
            btVector3( rayCallback.m_hitPointWorld - center ).length();
        }
    }
    */

    //OSG implementation: works for all geometry regardless
    osg::Vec3d startPoint( center.x(), center.y(), center.z() );
    osg::Vec3d endPoint( eye.x(), eye.y(), eye.z() );
    mLineSegmentIntersector->reset();
    mLineSegmentIntersector->setStart( startPoint );
    mLineSegmentIntersector->setEnd( endPoint );

    osgUtil::IntersectionVisitor intersectionVisitor(
        mLineSegmentIntersector.get() );

    vxs::SceneManager::instance()->GetRootNode()->accept( intersectionVisitor );

    osgUtil::LineSegmentIntersector::Intersections& intersections =
        mLineSegmentIntersector->getIntersections();

    osg::Drawable* objectHit( NULL );
    osgUtil::LineSegmentIntersector::Intersections::iterator itr =
        intersections.begin();
    for( itr; itr != intersections.end(); ++itr )
    {
        objectHit = itr->drawable.get();
        if( objectHit->getName() != "Character" )
        {
            mToOccludeDistance = osg::Vec3(
                itr->getWorldIntersectPoint() - startPoint ).length();

            if( mToCameraDistance < mMinCameraDistance )
            {
                mToCameraDistance = mMinCameraDistance;
            }

            if( mToOccludeDistance > mOccludeDistance )
            {
                mOccludeDistanceLERPdt = 0.0;
            }
            else
            {
                mOccludeDistanceLERPdt = 1.0;
            }

            mFromOccludeDistance = mOccludeDistance;

            mOccludeDistanceLERP = true;
            mPreviousOccluder = true;

            return;
        }
    }

    if( mPreviousOccluder )
    {
        mToOccludeDistance = mCameraDistance;
        mFromOccludeDistance = mOccludeDistance;
        mOccludeDistanceLERPdt = 0.0;
        mOccludeDistanceLERP = true;
        mPreviousOccluder = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::LookAt(
    btVector3& eye, btVector3& center, btVector3& up )
{
    btVector3 vVector = eye - center;
    vVector.normalize();
    btVector3 rVector( up.cross( vVector ) );
    rVector.normalize();
    btVector3 uVector( vVector.cross( rVector ) );
    uVector.normalize();

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
std::pair< double, double > CharacterController::UpdateHistoryBuffer()
{
    //http://www.flipcode.com/archives/Smooth_Mouse_Filtering.shtml

    //Remove the oldest device input from the history buffer
    mHistoryBuffer.pop_back();
    //Put the current device input into the history buffer
    mHistoryBuffer.push_front(
        std::make_pair( mDeltaTurnAngleX, mDeltaTurnAngleZ ) );

    //Reset the device input increment
    mDeltaTurnAngleX = 0.0;
    mDeltaTurnAngleZ = 0.0;

    //Use a weighted average for the history buffer contents
    double totalValueX( 0.0 );
    double totalValueZ( 0.0 );
    for( size_t i = 0; i < mBufferSize; ++i )
    {
        totalValueX += mHistoryBuffer.at( i ).first * mWeights.at( i );
        totalValueZ += mHistoryBuffer.at( i ).second * mWeights.at( i );
    }

    totalValueX *= mTurnSpeed / mTotalWeight;
    totalValueZ *= mTurnSpeed / mTotalWeight;

    return std::make_pair( totalValueX, totalValueZ );
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
