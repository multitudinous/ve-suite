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
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/FindParentWithNameVisitor.h>

// --- VRJuggler Includes --- //
#include <gmtl/Misc/MatrixConvert.h>
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>

#include <gadget/Type/Position.h>

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

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CharacterController::CharacterController()
    :
    KinematicCharacterController(),
    m_enabled( false ),
    m1stPersonMode( false ),
    mCameraDistanceLERP( false ),
    mCameraRotationSLERP( false ),
    mOccludeDistanceLERP( false ),
    mPreviousOccluder( false ),
    mBufferSize( 0 ),
    m_translateType( TranslateType::NONE ),
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
    mCharacterAnimations( NULL ),
    mMatrixTransform( NULL ),
    mLineSegmentIntersector( NULL )
{
    head.init( "VJHead" );
    wand.init( "VJWand" );

    Initialize();

    Enable( false );
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

    mMatrixTransform = new osg::MatrixTransform();
    mMatrixTransform->setName( "Character Transform" );
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
    mCharacterAnimations->setName( "Character Switch Control" );

    //for scaling if necessary
    osg::ref_ptr< osg::AutoTransform > scaleDown = new osg::AutoTransform(); 
    scaleDown->setName( "Character Auto Transform" );
    scaleDown->addChild( mCharacterAnimations.get() );
    scaleDown->setScale( 0.055 );
    scaleDown->setRotation( osg::Quat(
        osg::DegreesToRadians( 180.0 ), osg::Vec3f( 0.0, 0.0, 1.0 ) ) );

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
    shapeDrawable->setName( "Character Switch Control" );
    shapeDrawable->setColor( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
    geode->addDrawable( shapeDrawable.get() );

    //mMatrixTransform->addChild( geode.get() );
#endif
    SceneManager::instance()->GetModelRoot()->addChild(
        mMatrixTransform.get() );

    mMatrixTransform->setUpdateCallback(
        new CharacterTransformCallback( m_ghostObject ) );

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

    if( m_enabled )
    {
        mMatrixTransform->setNodeMask( NodeMask::CHARACTER );

        m_dynamicsWorld.addCollisionObject(
            m_ghostObject, btBroadphaseProxy::CharacterFilter,
            btBroadphaseProxy::StaticFilter | btBroadphaseProxy::DefaultFilter );

        m_dynamicsWorld.addAction( this );

        Reset();
    }
    else
    {
        mMatrixTransform->setNodeMask( 0 );

        m_dynamicsWorld.removeCollisionObject( m_ghostObject );

        m_dynamicsWorld.removeAction( this );
    }
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
    else if( m_translateType & TranslateType::STEP_FORWARD )
    {
        m_translateType = m_translateType ^ TranslateType::STEP_FORWARD;
        //Need to look at this!!!
        SetRotationFromCamera();
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
    else if( m_translateType & TranslateType::STEP_BACKWARD )
    {
        m_translateType = m_translateType ^ TranslateType::STEP_BACKWARD;
        //Need to look at this!!!
        SetRotationFromCamera();
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
    else if( m_translateType & TranslateType::STRAFE_LEFT )
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
    else if( m_translateType & TranslateType::STRAFE_RIGHT )
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
    else if( m_translateType & TranslateType::STEP_UP )
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
    else if( m_translateType & TranslateType::STEP_DOWN )
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
void CharacterController::SetRotationFromCamera()
{
    //Get current character transform
    btTransform xform = m_ghostObject->getWorldTransform();
    if( m_fly && ( m_translateType & TranslateType::STEP_FORWARD_BACKWARD ) )
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
void CharacterController::Move( btScalar dt )
{
    if( !m_enabled )
    {
        return;
    }

    bool headTracked = false;
    if( headTracked )
    {
        //std::cout << " Move character with head position" << std::endl;
        //const gadget::Position::SampleBuffer_t::buffer_t headSampleBuffer =
        //    head->getPositionPtr()->getPositionDataBuffer();
        //we have access to 5000 previous samples if needed
        const buffer_type& headSampleBuffer =
            head->getPositionPtr()->getPositionDataBuffer();
        iter_type cur_frame_iter = headSampleBuffer.rbegin();
        iter_type prev_frame_iter = cur_frame_iter + 1;
        const unsigned int dev_num(head->getUnit());
        m_vjHeadMat2 = m_vjHeadMat1;
        m_vjHeadMat1 =
            gmtl::convertTo<double>((*cur_frame_iter)[dev_num].getPosition());

        //Every time swap buffers is called the main VR Juggler buffer
        //is cleared and thus all of the history data is lost. We will have to 
        //keep all of the history ourselves with the buffer_type datatype.
        //std::cout << m_vjHeadMat1 << " " << m_vjHeadMat2 << std::endl;
        //std::cout << headSampleBuffer.size() << std::endl;
        /*if( (cur_frame_iter != headSampleBuffer.rend()) &&
           (prev_frame_iter != headSampleBuffer.rend()) )
        {
            m_vjHeadMat1 =
                gmtl::convertTo<double>((*cur_frame_iter)[dev_num].getPosition());
            m_vjHeadMat2 =
                gmtl::convertTo<double>((*prev_frame_iter)[dev_num].getPosition());
            std::cout << m_vjHeadMat1 << " " << m_vjHeadMat2 << std::endl;
            //m_vjHeadMat1 =
            //    gmtl::convertTo<double>( headSampleBuffer[ 0 ][ 0 ].getPosition() );
            //m_vjHeadMat2 =
            //    gmtl::convertTo<double>( headSampleBuffer[ 0 ][ 1 ].getPosition() );
        }
        else
        {
            std::cout << "We do not have a stable VJHead buffer to "
                << "sample for the character position." << std::endl;
        }*/
        //Update the character rotation
        UpdateRotationTrackedHead();

        //Update the character translation
        UpdateTranslationTrackedHead();
    }
    else
    {
        //Update the character rotation
        UpdateRotation();

        //Update the character translation
        UpdateTranslation( dt );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateCamera()
{
    if( !m_enabled )
    {
        return;
    }

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
void CharacterController::EyeToCenterRayTest(
    btVector3& eye, btVector3& center )
{
    //Bullet implementation: only works for geometry w/ active physics
    /*
    btCollisionWorld::RayResultCallback rayCallback( eye, center );
    m_dynamicsWorld.rayTest( center, eye, rayCallback );
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
    osg::Vec3d startPoint( eye.x(), eye.y(), eye.z() );
    osg::Vec3d endPoint( center.x(), center.y(), center.z() );
    mLineSegmentIntersector->reset();
    mLineSegmentIntersector->setStart( startPoint );
    mLineSegmentIntersector->setEnd( endPoint );

    osgUtil::LineSegmentIntersector::Intersections& intersections =
        TestForIntersections(
            *mLineSegmentIntersector.get(),
            SceneManager::instance()->GetGraphicalPluginManager() );

    if( !intersections.empty() )
    {
        osgUtil::LineSegmentIntersector::Intersections::reverse_iterator itr =
            intersections.rbegin();

        mToOccludeDistance =
            osg::Vec3d( itr->getWorldIntersectPoint() - endPoint ).length();

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
void CharacterController::UpdateRotation()
{
    //Update the device input history buffer
    std::pair< double, double > deltaDeviceInput = UpdateHistoryBuffer();

    //Calculate character rotation
    if( deltaDeviceInput.first == 0.0 && deltaDeviceInput.second == 0.0 )
    {
        return;
    }

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
        SetRotationFromCamera();
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateTranslation( btScalar dt )
{
    btVector3 displacement( 0.0, 0.0, 0.0 );

    //Calculate character translation
    if( !m_translateType )
    {
        setDisplacement( displacement );
        return;
    }

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

        forwardBackwardDisplacement *= m_forwardBackwardSpeedModifier;
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

        leftRightDisplacement *= m_leftRightSpeedModifier;
        displacement += leftRightDisplacement;
    }

    if( m_translateType & TranslateType::STEP_UP_DOWN )
    {
        btVector3 upDownDisplacement( 0.0, 0.0, 0.0 );
        btVector3 upDir( 0.0, 0.0, 1.0 );
        if( m_translateType & TranslateType::STEP_UP )
        {
            upDownDisplacement += upDir;
        }
        if( m_translateType & TranslateType::STEP_DOWN )
        {
            upDownDisplacement -= upDir;
        }

        upDownDisplacement *= m_upDownSpeedModifier;
        displacement += upDownDisplacement;
    }

    displacement *= dt;

    //slerp mCameraRotation if necessary
    if( mCameraRotationSLERP )
    {
        CameraRotationSLERP();
    }

    if( m_physicsSimulator.GetIdle() )
    {
        xform.setRotation( xform.getRotation().inverse() );
        xform.setOrigin( xform.getOrigin() + displacement );
        m_ghostObject->setWorldTransform( xform );
    }
    else
    {
        setDisplacement( displacement );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateRotationTrackedHead()
{
    //Set the new delta turn angles based on the current 
    //and previous head locations
    gmtl::Quatd jugglerHeadRot1 =
        gmtl::make< gmtl::Quatd >( m_vjHeadMat1 );
    //Now get the second point
    gmtl::Quatd jugglerHeadRot2 =
        gmtl::make< gmtl::Quatd >( m_vjHeadMat2 );

    mDeltaTurnAngleX += 0.0f;
    mDeltaTurnAngleZ += jugglerHeadRot1[ 1 ] - jugglerHeadRot2[ 1 ];

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
            SetRotationFromCamera();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CharacterController::UpdateTranslationTrackedHead()
{
    //We will populate the translate vector based on information from the 
    //tracker VJHead position
    btVector3 displacement( 0.0, 0.0, 0.0 );

    btTransform xform = m_ghostObject->getWorldTransform();
    xform.setRotation( xform.getRotation().inverse() );
    //std::cout << " UpdateTranslationTrackedHead " << std::endl;
    if( m_translateType & TranslateType::STEP_FORWARD_BACKWARD )
    {
        /*btVector3 forwardBackwardDisplacement( 0.0, 0.0, 0.0 );
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
        displacement += forwardBackwardDisplacement;*/
        gmtl::Vec3d vjVec;
        vjVec.set( 0.0f, 0.0f, -1.0f );
        gmtl::Matrix44d vjMat = gmtl::convertTo< double >( wand->getData() );

        gmtl::xform( vjVec, vjMat, vjVec );
        gmtl::normalize( vjVec );

        //Transform from juggler to osg...
        double dir[ 3 ];
        dir[0] =  vjVec[ 0 ];
        dir[1] = -vjVec[ 2 ];
        dir[2] =  vjVec[ 1 ];
        double translationStepSize = 0.75;
        for( int i = 0; i < 3; ++i )
        {
            //Update the translation movement for the objects
            //How much object should move
            displacement[ i ] += dir[ i ] * translationStepSize;
        }
    }

    //Take the difference between the current head location
    //and the previous location. We could average these points in the future
    //since we actually have the sample buffer for the head
    gmtl::Point3d jugglerHeadPoint1 =
        gmtl::makeTrans< gmtl::Point3d >( m_vjHeadMat1 );
    //Now get the second point
    gmtl::Point3d jugglerHeadPoint2 =
        gmtl::makeTrans< gmtl::Point3d >( m_vjHeadMat2 );

    displacement[ 0 ] += ( jugglerHeadPoint1[ 0 ] - jugglerHeadPoint2[ 0 ]);
    displacement[ 1 ] += (-jugglerHeadPoint1[ 2 ] + jugglerHeadPoint2[ 2 ]);
    displacement[ 2 ] += ( jugglerHeadPoint1[ 1 ] - jugglerHeadPoint2[ 1 ]);

    //std::cout << displacement[ 0 ] << " " << displacement[ 1 ] 
    //    << " " << displacement[ 2 ] << " " << std::endl;
    //slerp mCameraRotation if necessary
    if( mCameraRotationSLERP )
    {
        CameraRotationSLERP();
    }

    if( m_physicsSimulator.GetIdle() )
    {
        xform.setRotation( xform.getRotation().inverse() );
        xform.setOrigin( xform.getOrigin() + displacement );
        m_ghostObject->setWorldTransform( xform );
    }
    else
    {
        setDisplacement( displacement );
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
