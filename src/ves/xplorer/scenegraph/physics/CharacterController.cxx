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
    mStepForward( false ),
    mStepBackward( false ),
    mStrafeLeft( false ),
    mStrafeRight( false ),
    mJump( false ),
    mFlying( false ),
    mCameraHeight( 5.0 ),
    mCameraDistance( 20.0 ),
    mMinCameraDistance( 5.0 ),
    mMaxCameraDistance( 200.0 ),
    mDeltaZoom( 2.0 ),
    mSpeed( 10.0 ),
    //Average walk speed is 5 km/h -> 0.911344415 ft/s
    mMinSpeed( 1.0 ),
    //Usain Bolt's top 10m split 10m/0.82s -> 40 ft/s
    mMaxSpeed( 40.0 ),
    mTurnAngleX( 0.0 ),
    mTurnAngleZ( 0.0 ),
    mTurnSpeed( 2.0 ),
    mCameraRotation(),
    mCharacter( NULL ),
    mGhostObject( NULL ),
    mMatrixTransform( NULL )
{
    mCameraRotation.setIdentity();
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
    btScalar characterHeight = 3.83;//5.83;
    btScalar characterWidth = 1.83;

    btBroadphaseInterface* broadphase = dynamicsWorld->getBroadphase();
    broadphase->getOverlappingPairCache()->setInternalGhostPairCallback(
        new btGhostPairCallback() );

    btTransform startTransform;
    startTransform.setIdentity();

    mGhostObject = new btPairCachingGhostObject();
    mGhostObject->setWorldTransform( startTransform );

    btConvexShape* capsuleShape =
        new btCapsuleShapeZ( characterWidth, characterHeight );
    mGhostObject->setCollisionShape( capsuleShape );
    mGhostObject->setCollisionFlags( btCollisionObject::CF_CHARACTER_OBJECT );

    btScalar stepHeight = btScalar( 0.35 );
    mCharacter =
        new btKinematicCharacterController(
            mGhostObject, capsuleShape, stepHeight );

    mCharacter->setUpAxis( 2 );

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
void CharacterController::Turn( double dx, double dy )
{
    double x = -dy;
    double z =  dx;
    double length = ::sqrtf( x * x + z * z );
    if( length != 0.0 )
    {
        double lengthRatio = 1 / length;
        x *= lengthRatio;
        z *= lengthRatio;
    }

    mTurnAngleX += x * mTurnSpeed * PIDivOneEighty;
    mTurnAngleZ += z * mTurnSpeed * PIDivOneEighty;
	
    /*
    btQuaternion xRotation( btVector3( 1.0, 0.0, 0.0 ), mTurnAngleX );
    btQuaternion zRotation( btVector3( 0.0, 0.0, 1.0 ), mTurnAngleZ );
    btQuaternion rotation = xRotation * zRotation;
    mCameraRotation.setRotation( rotation );

    btTransform xform = mGhostObject->getWorldTransform();
    if( mFlying )
    {
        xform.setRotation( rotation );
    }
    else
    {
        xform.setRotation( zRotation );
    }

    mGhostObject->setWorldTransform( xform );
    */
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Update( btScalar dt )
{
    btQuaternion xRotation( btVector3( 1.0, 0.0, 0.0 ), mTurnAngleX );
    btQuaternion zRotation( btVector3( 0.0, 0.0, 1.0 ), mTurnAngleZ );
    btQuaternion rotation = xRotation * zRotation;
    mCameraRotation.setRotation( rotation );

    btTransform xform = mGhostObject->getWorldTransform();
    if( mFlying )
    {
        xform.setRotation( rotation );
    }
    else
    {
        xform.setRotation( zRotation );
    }

    mGhostObject->setWorldTransform( xform );

    btVector3 forwardDir = xform.getBasis()[ 1 ];
    forwardDir.normalize();

    btVector3 upDir = xform.getBasis()[ 2 ];
    upDir.normalize();

    btVector3 strafeDir = xform.getBasis()[ 0 ];
    strafeDir.normalize();

    btVector3 direction( 0.0, 0.0, 0.0 );
    btScalar speed = mSpeed * dt;

    if( mStrafeLeft )
    {
        direction -= strafeDir;
    }

    if( mStrafeRight )
    {
        direction += strafeDir;
    }

    if( mStepForward )
    {
        direction += forwardDir;
    }

    if( mStepBackward )
    {
        direction -= forwardDir;
    }

    mCharacter->setWalkDirection( direction * speed );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCamera()
{
    btTransform characterWorldTrans = mGhostObject->getWorldTransform();
    characterWorldTrans.setRotation( mCameraRotation.getRotation() );

    btVector3 up = characterWorldTrans.getBasis()[ 2 ];
    up.normalize();
    
	btVector3 backward = -characterWorldTrans.getBasis()[ 1 ];
    backward.normalize();

    btVector3 center = characterWorldTrans.getOrigin();
    btVector3 eye = center + up * mCameraHeight + backward * mCameraDistance;

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
