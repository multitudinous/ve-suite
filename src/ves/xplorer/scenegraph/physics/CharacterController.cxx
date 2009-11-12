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
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/FindParentWithNameVisitor.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Switch>
#include <osg/MatrixTransform>
#include <osg/ShapeDrawable>
#include <osgDB/ReadFile>
#include <osg/AutoTransform>

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletCollision/CollisionDispatch/btGhostObject.h>

#include <BulletCollision/CollisionShapes/btCapsuleShape.h>

// --- C/C++ Libraries --- //
#include <iostream>

#define VES_USE_ANIMATED_CHARACTER 1
const btVector3 m_defaultGravity( 0.0, 0.0, -9.8 );

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CharacterController::CharacterController()
    :
    KinematicCharacterController(),
    m_translateType( TranslateType::NONE ),
    m_enabled( false ),
    m1stPersonMode( false ),
    mCameraDistanceLERP( false ),
    mCameraRotationSLERP( false ),
    mOccludeDistanceLERP( false ),
    mPreviousOccluder( false ),
    mBufferSize( 0 ),
    mCameraDistance( 50.0 ),
    mOccludeDistance( 50.0 ),
    mMinCameraDistance( 0.1 ),
    mMaxCameraDistance( 200.0 ),
    mDeltaZoom( 2.0 ),
    mCameraDistanceLERPdt( 0.0 ),
    mCameraRotationSLERPdt( 0.0 ),
    mOccludeDistanceLERPdt( 0.0 ),
    mDeltaCameraDistanceLERP( 0.02 ),
    mDeltaCameraRotationSLERP( 0.02 ),
    mDeltaOccludeDistanceLERP( 0.02 ),
    mFromCameraDistance( 0.0 ),
    mToCameraDistance( 0.0 ),
    mFromOccludeDistance( 0.0 ),
    mToOccludeDistance( 0.0 ),
    //This is the speed of the character in ft/s
    m_forwardBackwardSpeedModifier( 15.0 ),
    m_leftRightSpeedModifier( 15.0 ),
    m_upDownSpeedModifier( 100.0 ),
    //Slow walk speed is 5 km/h ~ 1.0 ft/s
    //mMinSpeed( 1.0 ),
    //Usain Bolt's top 10m split 10m/0.82s ~ 40 ft/s
    //mMaxSpeed( 40.0 ),
    mTurnAngleX( 0.0 ),
    mTurnAngleZ( 0.0 ),
    mDeltaTurnAngleX( 0.0 ),
    mDeltaTurnAngleZ( 0.0 ),
    mFromTurnAngleZ( 0.0 ),
    mToTurnAngleZ( 0.0 ),
    mTurnSpeed( 7.0 ),
    mWeightModifier( 0.0 ),
    mTotalWeight( 0.0 ),
    mLookAtOffsetZ( 0.0, 0.0, m_characterHeight * 0.5 ),
    mCameraRotation( 0.0, 0.0, 0.0, 1.0 ),
    mCameraRotationX( 1.0, 0.0, 0.0, 1.0 ),
    mCameraRotationZ( 0.0, 0.0, 1.0, 1.0 ),
    //m_character( NULL ),
    mCharacterAnimations( NULL ),
    mMatrixTransform( NULL ),
    mLineSegmentIntersector( NULL )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CharacterController::~CharacterController()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Initialize()
{
    SetBufferSizeAndWeights( 10, 0.6 );

    //btScalar stepHeight = btScalar( 1.0 );
    //m_character =
        //new KinematicCharacterController(
            //m_ghostObject, capsuleShape, stepHeight );

    //m_character->setUpAxis( 2 );
    //setUpAxis( 2 );
    //No gravity by default
    //This has no effect and has not been implemented in bullet yet
    //m_character->setFallSpeed( 0.0 );

    mMatrixTransform = new osg::MatrixTransform();
#ifdef VES_USE_ANIMATED_CHARACTER
    //create animated character
    //idle
    osg::ref_ptr< osg::Node > node =
        osgDB::readNodeFile( "osg-data/soccer.idle.osg" );
    //walk forward
    osg::ref_ptr< osg::Node > node1 =
        osgDB::readNodeFile( "osg-data/soccer.wf.osg" );
    //walk backwards
    osg::ref_ptr< osg::Node > node2 =
        osgDB::readNodeFile( "osg-data/soccer.wb.osg" );
    //strafe left
    osg::ref_ptr< osg::Node > node3 =
        osgDB::readNodeFile( "osg-data/soccer.idle.osg" );
    //strafe right
    osg::ref_ptr< osg::Node > node4 =
        osgDB::readNodeFile( "osg-data/soccer.idle.osg" );

    //create switch node
    mCharacterAnimations = new osg::Switch();
    mCharacterAnimations->addChild( node.get() );
    mCharacterAnimations->addChild( node1.get() );
    mCharacterAnimations->addChild( node2.get() );
    mCharacterAnimations->addChild( node3.get() );
    mCharacterAnimations->addChild( node4.get() );
    mCharacterAnimations->setSingleChildOn( 0 );
    mCharacterAnimations->setName( "Character" );

    //for scaling if necessary
    osg::ref_ptr< osg::AutoTransform > scaleDown = new osg::AutoTransform(); 
    scaleDown->addChild( mCharacterAnimations.get() );
    scaleDown->setScale( 0.055 );
    scaleDown->setRotation( osg::Quat( osg::DegreesToRadians( 180.0 ), osg::Vec3f( 0.0, 0.0, 1.0)  ) );

    mMatrixTransform->addChild( scaleDown.get() );
#else
    //Create graphics mesh representation
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    osg::ref_ptr< osg::Capsule > capsule =
        new osg::Capsule(
            osg::Vec3( 0.0, 0.0, 0.0 ),
            m_characterWidth, m_characterHeight );
    osg::ref_ptr< osg::TessellationHints > hints = new osg::TessellationHints();
    hints->setDetailRatio( 1.0 );
    osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
        new osg::ShapeDrawable( capsule.get(), hints.get() );
    shapeDrawable->setName( "Character" );
    shapeDrawable->setColor( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
    geode->addDrawable( shapeDrawable.get() );

    //mMatrixTransform->addChild( geode.get() );
#endif
    SceneManager::instance()->GetModelRoot()->addChild( mMatrixTransform.get() );

    mMatrixTransform->setUpdateCallback(
        new CharacterTransformCallback( m_ghostObject ) );

    mMatrixTransform->setNodeMask( 0 );

    //Used for center to eye occluder test per frame
    mLineSegmentIntersector =
        new osgUtil::LineSegmentIntersector(
            osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Destroy()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::FirstPersonMode( bool onOff )
{
    m1stPersonMode = onOff;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Enable( const bool& enable )
{
    m_enabled = enable;

    btDynamicsWorld* dynamicsWorld =
        PhysicsSimulator::instance()->GetDynamicsWorld();
    if( m_enabled )
    {
        mMatrixTransform->setNodeMask( 1 );

        dynamicsWorld->addCollisionObject(
            m_ghostObject, btBroadphaseProxy::CharacterFilter,
            btBroadphaseProxy::StaticFilter | btBroadphaseProxy::DefaultFilter );

        dynamicsWorld->addCharacter( this );

        Reset();
    }
    else
    {
        mMatrixTransform->setNodeMask( 0 );

        dynamicsWorld->removeCollisionObject( m_ghostObject );

        //dynamicsWorld->removeCharacter( m_character );
        dynamicsWorld->removeCharacter( this );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Jump()
{
    //m_character->StartJump( 15.0 );
    StartJump( 15.0 );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StepForward( bool onOff )
{
    if( onOff )
    {
        m_translateType = m_translateType | TranslateType::STEP_FORWARD;
#ifdef VES_USE_ANIMATED_CHARACTER
        mCharacterAnimations->setSingleChildOn( 1 );
#endif
    }
    else
    {
        m_translateType = m_translateType ^ TranslateType::STEP_FORWARD;
#ifdef VES_USE_ANIMATED_CHARACTER
        mCharacterAnimations->setSingleChildOn( 0 );
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StepBackward( bool onOff )
{
    if( onOff )
    {
        m_translateType = m_translateType | TranslateType::STEP_BACKWARD;
#ifdef VES_USE_ANIMATED_CHARACTER
        mCharacterAnimations->setSingleChildOn( 2 );
#endif
    }
    else
    {
        m_translateType = m_translateType ^ TranslateType::STEP_BACKWARD;
#ifdef VES_USE_ANIMATED_CHARACTER
        mCharacterAnimations->setSingleChildOn( 0 );
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StrafeLeft( bool onOff )
{
    if( onOff )
    {
        m_translateType = m_translateType | TranslateType::STRAFE_LEFT;
#ifdef VES_USE_ANIMATED_CHARACTER
        mCharacterAnimations->setSingleChildOn( 3 );
#endif
    }
    else
    {
        m_translateType = m_translateType ^ TranslateType::STRAFE_LEFT;
#ifdef VES_USE_ANIMATED_CHARACTER
        mCharacterAnimations->setSingleChildOn( 0 );
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StrafeRight( bool onOff )
{
    if( onOff )
    {
        m_translateType = m_translateType | TranslateType::STRAFE_RIGHT;
#ifdef VES_USE_ANIMATED_CHARACTER
        mCharacterAnimations->setSingleChildOn( 4 );
#endif
    }
    else
    {
        m_translateType = m_translateType ^ TranslateType::STRAFE_RIGHT;
#ifdef VES_USE_ANIMATED_CHARACTER
        mCharacterAnimations->setSingleChildOn( 0 );
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StepUp( bool onOff )
{
    if( onOff )
    {
        m_translateType = m_translateType | TranslateType::STEP_UP;
#ifdef VES_USE_ANIMATED_CHARACTER
        //mCharacterAnimations->setSingleChildOn( 4 );
#endif
    }
    else
    {
        m_translateType = m_translateType ^ TranslateType::STEP_UP;
#ifdef VES_USE_ANIMATED_CHARACTER
        //mCharacterAnimations->setSingleChildOn( 0 );
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::StepDown( bool onOff )
{
    if( onOff )
    {
        m_translateType = m_translateType | TranslateType::STEP_DOWN;
#ifdef VES_USE_ANIMATED_CHARACTER
        //mCharacterAnimations->setSingleChildOn( 4 );
#endif
    }
    else
    {
        m_translateType = m_translateType ^ TranslateType::STEP_DOWN;
#ifdef VES_USE_ANIMATED_CHARACTER
        //mCharacterAnimations->setSingleChildOn( 0 );
#endif
    }
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
void CharacterController::SetCameraRotationSLERP( bool onOff )
{
    mCameraRotationSLERP = onOff;

    if( mCameraRotationSLERP )
    {
        mFromTurnAngleZ = mTurnAngleZ;

        //Take the shortest route back to the character rotation
        if( ( mFromTurnAngleZ - mToTurnAngleZ ) > gmtl::Math::PI )
        {
            mFromTurnAngleZ -= gmtl::Math::TWO_PI;
        }
        else if( ( mFromTurnAngleZ - mToTurnAngleZ ) < -gmtl::Math::PI )
        {
            mFromTurnAngleZ += gmtl::Math::TWO_PI;
        }
    }

    mCameraRotationSLERPdt = 0.0;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::SetCharacterRotationFromCamera()
{
    //Get current character transform
    btTransform xform = m_ghostObject->getWorldTransform();
    if( m_fly )
    {
        xform.setRotation( mCameraRotation.inverse() );
    }
    else
    {
        xform.setRotation( mCameraRotationZ.inverse() );
    }
    m_ghostObject->setWorldTransform( xform );

    mToTurnAngleZ = mTurnAngleZ;
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::Advance( btScalar dt )
{
    //Update the character rotation
    UpdateCharacterRotation();

    //Update the character translation
    UpdateCharacterTranslation( dt );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCamera()
{
    //lerp mCameraDistance if necessary
    if( mCameraDistanceLERP )
    {
        CameraDistanceLERP();
    }

    //Get the current character transform
    btTransform characterWorldTrans = m_ghostObject->getWorldTransform();
    //Set the rotation with the camera's rotation
    characterWorldTrans.setRotation( mCameraRotation );

    //Get the up vector of the camera
    btVector3 up = characterWorldTrans.getBasis()[ 2 ];
    up.normalize();
    
    //Get the backward direction of the camera
    btVector3 backward = -characterWorldTrans.getBasis()[ 1 ];
    backward.normalize();

    //Get the center of the character
    btVector3 center = characterWorldTrans.getOrigin() + mLookAtOffsetZ;
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
const bool CharacterController::IsEnabled() const
{
    return m_enabled;
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
void CharacterController::CameraRotationSLERP()
{
    if( mCameraRotationSLERPdt < ( 1.0 - mDeltaCameraRotationSLERP ) )
    {
        mCameraRotationSLERPdt += mDeltaCameraRotationSLERP;

        mTurnAngleZ =
            mFromTurnAngleZ +
            ( mToTurnAngleZ - mFromTurnAngleZ ) * mCameraRotationSLERPdt;
    }
    else
    {
        mTurnAngleZ = mToTurnAngleZ;

        mCameraRotationSLERP = false;
    }

    //Set the camera rotation
    mCameraRotationZ.setZ( sin( 0.5 * mTurnAngleZ ) );
    mCameraRotationZ.setW( cos( 0.5 * mTurnAngleZ ) );
    mCameraRotation = mCameraRotationX * mCameraRotationZ;
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
        PhysicsSimulator::instance()->GetDynamicsWorld();
    dynamicsWorld->rayTest( center, eye, rayCallback );
    if( rayCallback.hasHit() )
    {
        btCollisionObject* collisionObject = rayCallback.m_collisionObject;
        if( collisionObject != m_ghostObject )
        {
            btVector3( rayCallback.m_hitPointWorld - center ).length();
        }
    }
    */

    //OSG implementation: works for all geometry regardless
    ///Need to fix center to be at the head position for the character
    osg::Vec3d startPoint( center.x(), center.y(), center.z() );
    osg::Vec3d endPoint( eye.x(), eye.y(), eye.z() );
    mLineSegmentIntersector->reset();
    mLineSegmentIntersector->setStart( startPoint );
    mLineSegmentIntersector->setEnd( endPoint );

    osgUtil::IntersectionVisitor intersectionVisitor(
        mLineSegmentIntersector.get() );
    //Use bitwise NOT operator to get opposite of ManipulatorManager NodeMask
    //function validNodeMask in NodeVisitor:
    //return ( getTraversalMask() & ( getNodeMaskOverride() | node.getNodeMask() ) ) != 0
    unsigned int traversalMask =
        ~SceneManager::instance()->GetManipulatorManager()->getNodeMask();
    intersectionVisitor.setTraversalMask( traversalMask );

    SceneManager::instance()->GetModelRoot()->accept( intersectionVisitor );

    osgUtil::LineSegmentIntersector::Intersections& intersections =
        mLineSegmentIntersector->getIntersections();

    osg::Drawable* objectHit( NULL );
    osgUtil::LineSegmentIntersector::Intersections::iterator itr =
        intersections.begin();
    for( itr; itr != intersections.end(); ++itr )
    {
        objectHit = itr->drawable.get();

        bool notTheCharacter = false;
#ifndef VES_USE_ANIMATED_CHARACTER 
        if( objectHit->getName() != "Character" )
        {
            notTheCharacter = true;
        }
#else
        osg::Node* tempCharacter = objectHit->getParent( 0 );
        FindParentWithNameVisitor findParent( tempCharacter, "Character" );
        osg::ref_ptr< osg::Node > tempParent = findParent.GetParentNode();
        if( !tempParent.valid() )
        {
            notTheCharacter = true;
        }
#endif

        if( notTheCharacter )
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

    SceneManager::instance()->GetActiveNavSwitchNode()->SetMat( matrix );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::SetBufferSizeAndWeights(
    unsigned int bufferSize, double weightModifier )
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
    for( unsigned int i = 1; i < mBufferSize; ++i )
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
    for( unsigned int i = 0; i < mBufferSize; ++i )
    {
        totalValueX += mHistoryBuffer.at( i ).first * mWeights.at( i );
        totalValueZ += mHistoryBuffer.at( i ).second * mWeights.at( i );
    }

    totalValueX *= mTurnSpeed / mTotalWeight;
    totalValueZ *= mTurnSpeed / mTotalWeight;

    return std::make_pair( totalValueX, totalValueZ );
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCharacterRotation()
{
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
        //Restrict angles about the z-axis from 0 to 2PI
        if( mTurnAngleZ >= gmtl::Math::TWO_PI )
        {
            mTurnAngleZ -= gmtl::Math::TWO_PI;

        }
        else if( mTurnAngleZ < 0.0 )
        {
            mTurnAngleZ += gmtl::Math::TWO_PI;
        }

        //Set the camera rotation about the x-axis
        mCameraRotationX.setX( sin( 0.5 * mTurnAngleX ) );
        mCameraRotationX.setW( cos( 0.5 * mTurnAngleX ) );

        //Set the camera rotation about the z-axis
        mCameraRotationZ.setZ( sin( 0.5 * mTurnAngleZ ) );
        mCameraRotationZ.setW( cos( 0.5 * mTurnAngleZ ) );

        //Set the total camera rotation
        mCameraRotation = mCameraRotationX * mCameraRotationZ;

        if( m1stPersonMode )
        {
            SetCharacterRotationFromCamera();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCharacterTranslation( btScalar dt )
{
    //Calculate character translation
    btVector3 displacement( 0.0, 0.0, 0.0 );
    if( m_translateType | TranslateType::NONE )
    {
        //Get current character transform
        btTransform xform = m_ghostObject->getWorldTransform();
        xform.setRotation( xform.getRotation().inverse() );

        if( m_translateType & TranslateType::STEP_FORWARD_BACKWARD )
        {
            btVector3 forwardBackwardDisplacement( 0.0, 0.0, 0.0 );
            btVector3 forwardDir = xform.getBasis()[ 1 ];
            forwardDir.normalize();
            if( m_translateType & TranslateType::STEP_FORWARD )
            {
                forwardBackwardDisplacement += forwardDir;
            }
            if( m_translateType & TranslateType::STEP_BACKWARD )
            {
                forwardBackwardDisplacement -= forwardDir;
            }

            forwardBackwardDisplacement *= dt * m_forwardBackwardSpeedModifier;
            displacement += forwardBackwardDisplacement;
        }

        if( m_translateType & TranslateType::STRAFE_LEFT_RIGHT )
        {
            btVector3 leftRightDisplacement( 0.0, 0.0, 0.0 );
            btVector3 strafeDir = xform.getBasis()[ 0 ];
            strafeDir.normalize();
            if( m_translateType & TranslateType::STRAFE_LEFT )
            {
                leftRightDisplacement -= strafeDir;
            }
            if( m_translateType & TranslateType::STRAFE_RIGHT )
            {
                leftRightDisplacement += strafeDir;
            }

            leftRightDisplacement *= dt * m_leftRightSpeedModifier;
            displacement += leftRightDisplacement;
        }

        if( m_translateType & TranslateType::STEP_UP_DOWN )
        {
            btVector3 upDownDisplacement( 0.0, 0.0, 0.0 );
            btVector3 upDir = xform.getBasis()[ 2 ];
            upDir.normalize();
            if( m_translateType & TranslateType::STEP_UP )
            {
                upDownDisplacement += upDir;
            }
            if( m_translateType & TranslateType::STEP_DOWN )
            {
                upDownDisplacement -= upDir;
            }

            upDownDisplacement *= dt * m_upDownSpeedModifier;
            displacement += upDownDisplacement;
        }

        //slerp mCameraRotation if necessary
        if( mCameraRotationSLERP )
        {
            CameraRotationSLERP();
        }
    }

    setWalkDirection( displacement );
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
