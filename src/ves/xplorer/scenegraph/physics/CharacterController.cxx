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
    mStepForward( false ),
    mStepBackward( false ),
    mStrafeLeft( false ),
    mStrafeRight( false ),
    mTurnLeft( false ),
    mTurnRight( false ),
    mJump( false ),
    mCameraHeight( 4.0 ),
    mMinCameraDistance( 3.0 ),
    mMaxCameraDistance( 10.0 ),
    mCharacter( NULL ),
    mGhostObject( NULL )
{
    ;
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
    btScalar characterHeight = 1.75;
    btScalar characterWidth = 1.75;

    btBroadphaseInterface* broadphase = dynamicsWorld->getBroadphase();
    broadphase->getOverlappingPairCache()->setInternalGhostPairCallback(
        new btGhostPairCallback() );

    btTransform startTransform;
    startTransform.setIdentity();

    mGhostObject = new btPairCachingGhostObject();
    mGhostObject->setWorldTransform( startTransform );

    btConvexShape* capsuleShape =
        new btCapsuleShape( characterWidth, characterHeight );
    mGhostObject->setCollisionShape( capsuleShape );
    mGhostObject->setCollisionFlags( btCollisionObject::CF_CHARACTER_OBJECT );

    btScalar stepHeight = btScalar( 0.35 );
    mCharacter =
        new btKinematicCharacterController(
            mGhostObject, capsuleShape, stepHeight );

    mCharacter->setUpAxis( 2 );

    //dynamicsWorld->addCollisionObject(
        //mGhostObject, btBroadphaseProxy::CharacterFilter,
        //btBroadphaseProxy::StaticFilter | btBroadphaseProxy::DefaultFilter );

    //dynamicsWorld->addCharacter( mCharacter );

    Reset( dynamicsWorld );

    //Create graphics mesh representation
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    osg::ref_ptr< osg::Capsule > capsule =
        new osg::Capsule(
            osg::Vec3( 0.0, 0.0, 0.0 ),
            characterWidth, characterHeight );
    osg::ref_ptr< osg::TessellationHints > hints = new osg::TessellationHints();
    osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
        new osg::ShapeDrawable( capsule.get(), hints.get() );

    hints->setDetailRatio( 1.0 );
    shapeDrawable->setColor( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
    geode->addDrawable( shapeDrawable.get() );

    osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform();
    mt->addChild( geode.get() );
    //vxs::SceneManager::instance()->GetModelRoot()->addChild( mt.get() );

    //mt->setUpdateCallback( new CharacterTransformCallback( mGhostObject ) );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Destroy( btDynamicsWorld* dynamicsWorld )
{
    ;
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

    //mCharacter->warp( btVector3( 0, -2.0, 0.0 ) );
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
void CharacterController::TurnLeft( bool onOff )
{
    mTurnLeft = onOff;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::TurnRight( bool onOff )
{
    mTurnRight = onOff;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCharacter(
    btDynamicsWorld* dynamicsWorld, btScalar dt )
{
    if( dynamicsWorld )
    {
        //Set walkDirection for character
        btTransform xform;
        xform = mGhostObject->getWorldTransform();

        btVector3 forwardDir = xform.getBasis()[ 1 ];
        forwardDir.normalize();

        btVector3 upDir = xform.getBasis()[ 2 ];
        upDir.normalize();

        btVector3 strafeDir = xform.getBasis()[ 0 ];
        strafeDir.normalize();

        btVector3 walkDirection = btVector3( 0.0, 0.0, 0.0 );
        //4 km/h -> 1.1 m/s
        btScalar walkVelocity = btScalar( 1.1 ) * 4.0;
        btScalar walkSpeed = walkVelocity * dt;

        if( mStrafeLeft )
        {
            walkDirection -= strafeDir;
        }

        if( mStrafeRight )
        {
            walkDirection += strafeDir;
        }

        if( mStepForward )
        {
            walkDirection += forwardDir;
        }

        if( mStepBackward )
        {
            walkDirection -= forwardDir;
        }

        mCharacter->setWalkDirection( walkDirection * walkSpeed );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCamera()
{
    btTransform characterWorldTrans = mGhostObject->getWorldTransform();

    btVector3 up = characterWorldTrans.getBasis()[ 2 ];
    up.normalize();
    
	btVector3 backward = -characterWorldTrans.getBasis()[ 1 ];
    backward.normalize();

    btVector3 center = characterWorldTrans.getOrigin();
    btVector3 eye = center + up * 2.0 + backward * 12.0;

    btVector3 zVector = eye - center;
    zVector.normalize();
    btVector3 xVector( up.cross( zVector ) );
    xVector.normalize();
    btVector3 yVector( zVector.cross( xVector ) );
    yVector.normalize();

    //"Look at" character matrix
    ves::xplorer::scenegraph::DCS* const cameraDCS =
        vxs::SceneManager::instance()->GetActiveNavSwitchNode();

    gmtl::Matrix44d matrix = cameraDCS->GetMat();
    matrix.mData[ 0 ]  = xVector[ 0 ];
    matrix.mData[ 1 ]  = xVector[ 1 ];
    matrix.mData[ 2 ]  = xVector[ 2 ];
    matrix.mData[ 3 ]  = 0.0;

    matrix.mData[ 4 ]  = zVector[ 0 ];
    matrix.mData[ 5 ]  = zVector[ 1 ];
    matrix.mData[ 6 ]  = zVector[ 2 ];
    matrix.mData[ 7 ]  = 0.0;

    matrix.mData[ 8 ]  = yVector[ 0 ];
    matrix.mData[ 9 ]  = yVector[ 1 ];
    matrix.mData[ 10 ] = yVector[ 2 ];
    matrix.mData[ 11 ] = 0.0;

    matrix.mData[ 12 ] = -eye[ 0 ];
    matrix.mData[ 13 ] = -eye[ 1 ];
    matrix.mData[ 14 ] = -eye[ 2 ];
    matrix.mData[ 15 ] = 1.0;

    cameraDCS->SetMat( matrix );
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
