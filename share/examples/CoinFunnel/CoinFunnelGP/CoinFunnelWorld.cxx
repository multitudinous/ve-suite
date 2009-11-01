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

// --- My Includes --- //
#include "CoinFunnelWorld.h"
#include "FunnelEntity.h"
#include "MarbleEntity.h"
#include "RailingEntity.h"
#include "SlideEntity.h"
#include "WaterEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/Sound.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

// --- OSG Includes --- //
#include <osg/ShapeDrawable>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>

// --- osgBullet Includes --- //
#include <osgbBullet/MotionState.h>
#include <osgbBullet/CollisionShapes.h>

using namespace funnel;

////////////////////////////////////////////////////////////////////////////////
CoinFunnelWorld::CoinFunnelWorld(
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator,
    ves::xplorer::scenegraph::ResourceManager* resourceManager
#ifdef VE_SOUND
    ,
    osgAL::SoundManager* soundManager
#endif
    )
:
mPhysicsSimulator( physicsSimulator ),
#ifdef VE_SOUND
mSoundManager( soundManager ),
#endif
mPluginDCS( pluginDCS ),
mFunnelEntity( 0 ),
mMarbleEntity( 0 ),
mRailingEntity( 0 ),
mSlideEntity( 0 ),
mWaterEntity( 0 ),
mResourceManager( resourceManager )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CoinFunnelWorld::~CoinFunnelWorld()
{
    if( mFunnelEntity )
    {
        delete mFunnelEntity;
    }

    if( mMarbleEntity )
    {
        delete mMarbleEntity;
    }

    if( mRailingEntity )
    {
        delete mRailingEntity;
    }

    if( mSlideEntity )
    {
        delete mSlideEntity;
    }

    if( mWaterEntity )
    {
        delete mWaterEntity;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelWorld::Initialize()
{
    //mPhysicsSimulator->SetCollisionInformation( true );

    CreateRoom( 150.0 );

    {
        mFunnelEntity = new funnel::FunnelEntity(
            "Models/IVEs/funnel_physics.ive",
            mPluginDCS.get(),
            mPhysicsSimulator,
            mResourceManager );
        mFunnelEntity->SetNameAndDescriptions( "funnel_physics" );

        btCollisionShape* collisionShape =
            osgbBullet::btTriMeshCollisionShapeFromOSG(
                mFunnelEntity->GetDCS() );

        osgbBullet::MotionState* motionState = new osgbBullet::MotionState();
        motionState->setTransform( mFunnelEntity->GetDCS() );
        motionState->setParentTransform( osg::Matrix::identity() );

        btScalar mass( 0.0 );
        btVector3 inertia( 0.0, 0.0, 0.0 );
        btRigidBody::btRigidBodyConstructionInfo rbci(
            mass, motionState, collisionShape, inertia );
        btRigidBody* rigidBody = new btRigidBody( rbci );

        rigidBody->setFriction( 0.5 );
        rigidBody->setRestitution( 0.0 );

        mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( rigidBody );

        /*
        mFunnelEntity->InitPhysics();
        mFunnelEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
        mFunnelEntity->GetPhysicsRigidBody()->StaticConcaveShape();
        mFunnelEntity->GetPhysicsRigidBody()->GetbtRigidBody()->setFriction( 0.5 );
        mFunnelEntity->GetPhysicsRigidBody()->GetbtRigidBody()->setRestitution( 0.0 );
        */
    }

    {
        mMarbleEntity = new funnel::MarbleEntity(
            "Models/IVEs/marble_physics.ive",
            mPluginDCS.get(),
            mPhysicsSimulator,
            mResourceManager
#ifdef VE_SOUND
            ,
            mSoundManager
#endif
            );
        mMarbleEntity->SetNameAndDescriptions( "marble_physics" );

        btCollisionShape* collisionShape =
            osgbBullet::btSphereCollisionShapeFromOSG(
                mMarbleEntity->GetDCS() );

        osgbBullet::MotionState* motionState = new osgbBullet::MotionState();
        motionState->setTransform( mMarbleEntity->GetDCS() );
        motionState->setParentTransform(
            osg::Matrix::translate( 4.85, 2.5, 5.75 ) );

        btScalar mass( 1.0 );
        btVector3 inertia( 0.0, 0.0, 0.0 );
        collisionShape->calculateLocalInertia( mass, inertia );

        btRigidBody::btRigidBodyConstructionInfo rbci(
            mass, motionState, collisionShape, inertia );
        btRigidBody* rigidBody = new btRigidBody( rbci );

        rigidBody->setFriction( 0.5 );
        rigidBody->setRestitution( 0.0 );

        mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( rigidBody );

        /*
        double marblePosition[ 3 ] = { 4.85, 2.5, 5.75 };
        mMarbleEntity->GetDCS()->SetTranslationArray( marblePosition );
        mMarbleEntity->InitPhysics();
        mMarbleEntity->GetPhysicsRigidBody()->SetStoreCollisions( true );
        mMarbleEntity->GetPhysicsRigidBody()->SetMass( 1.0 );
        mMarbleEntity->GetPhysicsRigidBody()->SphereShape( 0.06 );
        mMarbleEntity->GetPhysicsRigidBody()->GetbtRigidBody()->setFriction( 0.5 );
        mMarbleEntity->GetPhysicsRigidBody()->GetbtRigidBody()->setRestitution( 0.0 );
        */
        
    }

    {
        mRailingEntity =
            new funnel::RailingEntity(
                "Models/IVEs/railing_physics.ive",
                mPluginDCS.get(),
                mPhysicsSimulator,
                mResourceManager );
        mRailingEntity->SetNameAndDescriptions( "railing_physics" );

        btCollisionShape* collisionShape =
            osgbBullet::btTriMeshCollisionShapeFromOSG(
                mRailingEntity->GetDCS() );

        osgbBullet::MotionState* motionState = new osgbBullet::MotionState();
        motionState->setTransform( mRailingEntity->GetDCS() );
        motionState->setParentTransform( osg::Matrix::identity() );

        btScalar mass( 0.0 );
        btVector3 inertia( 0.0, 0.0, 0.0 );
        btRigidBody::btRigidBodyConstructionInfo rbci(
            mass, motionState, collisionShape, inertia );
        btRigidBody* rigidBody = new btRigidBody( rbci );

        rigidBody->setFriction( 0.5 );
        rigidBody->setRestitution( 0.0 );

        mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( rigidBody );

        /*
        mRailingEntity->InitPhysics();
        mRailingEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
        mRailingEntity->GetPhysicsRigidBody()->StaticConcaveShape();
        mRailingEntity->GetPhysicsRigidBody()->GetbtRigidBody()->setFriction( 0.5 );
        mRailingEntity->GetPhysicsRigidBody()->GetbtRigidBody()->setRestitution( 0.0 );
        */
    }

    {
        mSlideEntity = new funnel::SlideEntity(
            "Models/IVEs/slide_physics.ive",
            mPluginDCS.get(),
            mPhysicsSimulator,
            mResourceManager );
        mSlideEntity->SetNameAndDescriptions( "slide_physics" );

        btCollisionShape* collisionShape =
            osgbBullet::btTriMeshCollisionShapeFromOSG(
                mSlideEntity->GetDCS() );

        osgbBullet::MotionState* motionState = new osgbBullet::MotionState();
        motionState->setTransform( mSlideEntity->GetDCS() );
        motionState->setParentTransform( osg::Matrix::identity() );

        btScalar mass( 0.0 );
        btVector3 inertia( 0.0, 0.0, 0.0 );
        btRigidBody::btRigidBodyConstructionInfo rbci(
            mass, motionState, collisionShape, inertia );
        btRigidBody* rigidBody = new btRigidBody( rbci );

        rigidBody->setFriction( 0.5 );
        rigidBody->setRestitution( 0.0 );

        mPhysicsSimulator->GetDynamicsWorld()->addRigidBody( rigidBody );

        /*
        mSlideEntity->InitPhysics();
        mSlideEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
        mSlideEntity->GetPhysicsRigidBody()->StaticConcaveShape();
        mSlideEntity->GetPhysicsRigidBody()->GetbtRigidBody()->setFriction( 0.5 );
        mSlideEntity->GetPhysicsRigidBody()->GetbtRigidBody()->setRestitution( 0.0 );
        */
    }

    {
        mWaterEntity = new funnel::WaterEntity(
            "Models/IVEs/water.ive",
            mPluginDCS.get(),
            mResourceManager
#ifdef VE_SOUND
            ,
            mSoundManager
#endif 
            );
        mWaterEntity->SetNameAndDescriptions( "water" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelWorld::PreFrameUpdate()
{
    /*
    if( !mPhysicsSimulator->GetIdle() )
    {
#ifdef VE_SOUND
        if( mMarbleEntity->GetPhysicsRigidBody()->CollisionInquiry(
                mSlideEntity->GetPhysicsRigidBody() ) )
        {
            mMarbleEntity->GetMarbleOnWoodSound()->PushSoundEvent( 10 );
        }
        else if( mMarbleEntity->GetPhysicsRigidBody()->CollisionInquiry(
                    mRailingEntity->GetPhysicsRigidBody() ) )
        {
            mMarbleEntity->GetMarbleOnMetalSound()->PushSoundEvent( 10 );
        }
        else if( mMarbleEntity->GetPhysicsRigidBody()->CollisionInquiry(
                    mFunnelEntity->GetPhysicsRigidBody() ) )
        {
            mMarbleEntity->GetMarbleOnMarbleSound()->PushSoundEvent( 10 );
        }
#endif
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelWorld::CreateRoom( float width )
{
    osg::ref_ptr< osg::Geode > roomGeode = new osg::Geode();
    mPluginDCS->addChild( roomGeode.get() );

    osg::ref_ptr< osg::Box > roomBox =
        new osg::Box( osg::Vec3( 0, 0, 25 ), width );
    osg::ref_ptr< osg::ShapeDrawable > roomShapeDrawable =
        new osg::ShapeDrawable( roomBox.get() );
    roomGeode->addDrawable( roomShapeDrawable.get() );

    osg::ref_ptr< osg::StateSet > roomStateSet = new osg::StateSet();

    roomStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    roomStateSet->setAttribute(
        ( mResourceManager->get< osg::Program, osg::ref_ptr >
        ( "RoomProgram" ) ).get(), osg::StateAttribute::ON );

    roomStateSet->setTextureAttributeAndModes( 1,
        ( mResourceManager->get< osg::TextureCubeMap, osg::ref_ptr >
        ( "CubeMap" ) ).get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > envMapUniform =
        new osg::Uniform( "envMap", 1 );
    roomStateSet->addUniform( envMapUniform.get() );

    roomGeode->setStateSet( roomStateSet.get() );
}
////////////////////////////////////////////////////////////////////////////////
