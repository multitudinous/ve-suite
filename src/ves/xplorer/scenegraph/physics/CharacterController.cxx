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

// --- Bullet Includes --- //
#include <BulletCollision/CollisionShapes/btMultiSphereShape.h>

#include <BulletDynamics/Dynamics/btRigidBody.h>
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <LinearMath/btDefaultMotionState.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CharacterController::CharacterController()
    :
    mHalfHeight( 1.0 ),
    mTurnAngle( 0.0 ),
    mMaxLinearVelocity( 10.0 ),
    //m/s
    mWalkVelocity( 8.0 ),
    //rad/s
    mTurnVelocity( 1.0 ),
    mShape( NULL ),
    mRigidBody( NULL )
{
    mRayLambda[ 0 ] = 1.0;
    mRayLambda[ 1 ] = 1.0;
}
////////////////////////////////////////////////////////////////////////////////
CharacterController::~CharacterController()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Setup(
    btDynamicsWorld* dynamicsWorld, btScalar height, btScalar width )
{
    btVector3 spherePositions[ 2 ];
    btScalar sphereRadii[ 2 ];

    sphereRadii[ 0 ] = width;
    sphereRadii[ 1 ] = width;
    spherePositions[ 0 ] =
        btVector3( 0.0, ( height / btScalar( 2.0 ) - width ), 0.0 );
    spherePositions[ 1 ] =
        btVector3( 0.0, ( -height / btScalar( 2.0 ) + width ), 0.0 );

    mHalfHeight = height / btScalar( 2.0 );

    mShape = new btMultiSphereShape(
        btVector3( width / btScalar( 2.0 ), height / btScalar( 2.0 ), width / btScalar( 2.0 ) ),
        &spherePositions[ 0 ], &sphereRadii[ 0 ], 2 );

    btTransform startTransform;
    startTransform.setIdentity();
    startTransform.setOrigin( btVector3( 0.0, 2.0, 0.0 ) );
    btDefaultMotionState* myMotionState = new btDefaultMotionState( startTransform );
    btRigidBody::btRigidBodyConstructionInfo cInfo( 1.0, myMotionState, mShape );
    mRigidBody = new btRigidBody( cInfo );
    //kinematic vs. static doesn't work
    //mRigidBody->setCollisionFlags( mRigidBody->getCollisionFlags() | btCollisionObject::CF_KINEMATIC_OBJECT );
    mRigidBody->setSleepingThresholds( 0.0, 0.0 );
    mRigidBody->setAngularFactor( 0.0 );
    dynamicsWorld->addRigidBody( mRigidBody );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Destroy( btDynamicsWorld* dynamicsWorld )
{
    if( mShape )
    {
        delete mShape;
    }

    if( mRigidBody )
    {
        dynamicsWorld->removeRigidBody( mRigidBody );

        delete mRigidBody;
    }
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* CharacterController::GetRigidBody()
{
    return mRigidBody;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::PreStep( btDynamicsWorld* dynamicsWorld )
{
    btTransform xform;
    mRigidBody->getMotionState()->getWorldTransform( xform );
    btVector3 down = -xform.getBasis()[ 1 ];
    btVector3 forward = xform.getBasis()[ 2 ];
    down.normalize();
    forward.normalize();

    mRaySource[ 0 ] = xform.getOrigin();
    mRaySource[ 1 ] = xform.getOrigin();

    mRayTarget[ 0 ] =
        mRaySource[ 0 ] + down * mHalfHeight * btScalar( 1.1 );
    mRayTarget[ 1 ] =
        mRaySource[ 1 ] + forward * mHalfHeight * btScalar( 1.1 );

    class ClosestNotMe : public btCollisionWorld::ClosestRayResultCallback
    {
    public:
        ClosestNotMe( btRigidBody* me )
            :
            btCollisionWorld::ClosestRayResultCallback(
                btVector3( 0.0, 0.0, 0.0 ), btVector3( 0.0, 0.0, 0.0 ) )
        {
            m_me = me;
        }

        virtual btScalar AddSingleResult(
            btCollisionWorld::LocalRayResult& rayResult,
            bool normalInWorldSpace )
        {
            if( rayResult.m_collisionObject == m_me )
            {
                return 1.0;
            }

            return ClosestRayResultCallback::addSingleResult(
                rayResult, normalInWorldSpace );
        }
    protected:
        btRigidBody* m_me;

    };

    ClosestNotMe rayCallback( mRigidBody );

    int i = 0;
    for( i = 0; i < 2; ++i )
    {
        rayCallback.m_closestHitFraction = 1.0;
        dynamicsWorld->rayTest(
            mRaySource[ i ], mRayTarget[ i ], rayCallback );
        if( rayCallback.hasHit() )
        {
            mRayLambda[ i ] = rayCallback.m_closestHitFraction;
        }
        else
        {
            mRayLambda[ i ] = 1.0;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::PlayerStep( btScalar dt )
{
    btTransform xform;
    mRigidBody->getMotionState()->getWorldTransform( xform );

    //Handle turning
    if( mTurnLeft )
    {
        mTurnAngle -= dt * mTurnVelocity;
        mTurnLeft = false;
    }

    if( mTurnRight )
    {
        mTurnAngle += dt * mTurnVelocity;
        mTurnRight = false;
    }

    xform.setRotation(
        btQuaternion( btVector3( 0.0, 1.0, 0.0 ), mTurnAngle ) );

    btVector3 linearVelocity = mRigidBody->getLinearVelocity();
    btScalar speed = mRigidBody->getLinearVelocity().length();

    btVector3 forwardDir = xform.getBasis()[ 2 ];
    forwardDir.normalize();
    btVector3 walkDirection = btVector3( 0.0, 0.0, 0.0 );
    btScalar walkSpeed = mWalkVelocity * dt;

    if( mStepForward )
    {
        walkDirection += forwardDir;
        mStepForward = false;
    }

    if( mStepBackward )
    {
        walkDirection -= forwardDir;
        mStepBackward = false;
    }

    if( !mStepForward && !mStepBackward && OnGround() )
    {
        //Dampen when on the ground and not being moved by the player
        linearVelocity *= 0.2;
        mRigidBody->setLinearVelocity( linearVelocity );
    }
    else
    {
        if( speed < mMaxLinearVelocity )
        {
            btVector3 velocity = linearVelocity + walkDirection * walkSpeed;
            mRigidBody->setLinearVelocity( velocity );
        }
    }

    mRigidBody->getMotionState()->setWorldTransform( xform );
    mRigidBody->setCenterOfMassTransform( xform );
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterController::CanJump() const
{
    return OnGround();
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Jump()
{
    if( !CanJump() )
    {
        return;
    }

    btTransform xform;
    mRigidBody->getMotionState()->getWorldTransform( xform );
    btVector3 up = xform.getBasis()[ 1 ];
    up.normalize();
    btScalar magnitude =
        ( btScalar( 1.0 ) / mRigidBody->getInvMass() ) * btScalar( 8.0 );
    mRigidBody->applyCentralImpulse( up * magnitude );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StepForward()
{
    mStepForward = true;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StepBackward()
{
    mStepBackward = true;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::TurnLeft()
{
    mTurnLeft = true;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::TurnRight()
{
    mTurnRight = true;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCharacter(
    btDynamicsWorld* dynamicsWorld, btScalar dt )
{
    PreStep( dynamicsWorld );

    PlayerStep( dt );

    if( mJump )
    {
        Jump();
        mJump = false;
    }

    UpdateCamera();
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCamera()
{
    //Look at the character
    btTransform characterWorldTrans;
    mRigidBody->getMotionState()->getWorldTransform( characterWorldTrans );
    btVector3 upChar = characterWorldTrans.getBasis()[ 1 ];
    btVector3 backward = -characterWorldTrans.getBasis()[ 2 ];
    upChar.normalize();
    backward.normalize();

    btVector3 eye, center, up;
    center = characterWorldTrans.getOrigin();
    eye = center + upChar * 5.0 + backward * 5.0;
    up.setValue( 0.0, 1.0, 0.0 );

    btVector3 fVector = center - eye;
    fVector = fVector / fVector.length();
    btVector3 sVector = fVector.cross( up );
    btVector3 uVector = sVector.cross( fVector );

    gmtl::Matrix44d matrix;
    matrix.mData[ 0 ]  =  sVector[ 0 ];
    matrix.mData[ 1 ]  =  uVector[ 0 ];
    matrix.mData[ 2 ]  = -fVector[ 0 ];
    matrix.mData[ 3 ]  =  0.0;
    matrix.mData[ 4 ]  =  sVector[ 1 ];
    matrix.mData[ 5 ]  =  uVector[ 1 ];
    matrix.mData[ 6 ]  = -fVector[ 1 ];
    matrix.mData[ 7 ]  =  0.0;
    matrix.mData[ 8 ]  =  sVector[ 2 ];
    matrix.mData[ 9 ]  =  uVector[ 2 ];
    matrix.mData[ 10 ] = -fVector[ 2 ];
    matrix.mData[ 11 ] =  0.0;
    matrix.mData[ 12 ] =  0.0;
    matrix.mData[ 13 ] =  0.0;
    matrix.mData[ 14 ] =  0.0;
    matrix.mData[ 15 ] =  1.0;

    vxs::DCS* const cameraDCS =
        vxs::SceneManager::instance()->GetActiveNavSwitchNode();
    cameraDCS->SetMat( matrix );
    
}
////////////////////////////////////////////////////////////////////////////////
bool CharacterController::OnGround() const
{
    return mRayLambda[ 0 ] < btScalar( 1.0 );
}
////////////////////////////////////////////////////////////////////////////////
