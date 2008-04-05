/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/xplorer/scenegraph/Sound.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

// --- OSG Includes --- //
#include <osg/ShapeDrawable>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

using namespace funnel;

////////////////////////////////////////////////////////////////////////////////
CoinFunnelWorld::CoinFunnelWorld(
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
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
mWaterEntity( 0 )
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
    mPhysicsSimulator->SetCollisionInformation( true );

    CreateRoom( 150.0 );

    mFunnelEntity = new funnel::FunnelEntity( "Models/IVEs/funnel_physics.ive",
                                              mPluginDCS.get(),
                                              mPhysicsSimulator );
    mFunnelEntity->SetNameAndDescriptions( "funnel_physics" );
    mFunnelEntity->InitPhysics();
    mFunnelEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
    mFunnelEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    mFunnelEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );
    mFunnelEntity->GetPhysicsRigidBody()->StaticConcaveShape();

    mMarbleEntity = new funnel::MarbleEntity( "Models/IVEs/marble_physics.ive",
                                              mPluginDCS.get(),
                                              mPhysicsSimulator
#ifdef VE_SOUND
                                              ,
                                              mSoundManager
#endif
                                              );
    mMarbleEntity->SetNameAndDescriptions( "marble_physics" );
    double marblePosition[ 3 ] = { 4.85, 2.5, 5.75 };
    mMarbleEntity->GetDCS()->SetTranslationArray( marblePosition );
    mMarbleEntity->InitPhysics();
    mMarbleEntity->GetPhysicsRigidBody()->SetStoreCollisions( true );
    mMarbleEntity->GetPhysicsRigidBody()->SetMass( 1.0 );
    mMarbleEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    mMarbleEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );
    mMarbleEntity->GetPhysicsRigidBody()->SphereShape( 0.06 );

    mRailingEntity =
        new funnel::RailingEntity( "Models/IVEs/railing_physics.ive",
                                   mPluginDCS.get(),
                                   mPhysicsSimulator );
    mRailingEntity->SetNameAndDescriptions( "railing_physics" );
    mRailingEntity->InitPhysics();
    mRailingEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
    mRailingEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    mRailingEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );
    mRailingEntity->GetPhysicsRigidBody()->StaticConcaveShape();

    mSlideEntity = new funnel::SlideEntity( "Models/IVEs/slide_physics.ive",
                                            mPluginDCS.get(),
                                            mPhysicsSimulator );
    mSlideEntity->SetNameAndDescriptions( "slide_physics" );
    mSlideEntity->InitPhysics();
    mSlideEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
    mSlideEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    mSlideEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );
    mSlideEntity->GetPhysicsRigidBody()->StaticConcaveShape();

    mWaterEntity = new funnel::WaterEntity( "Models/IVEs/water.ive",
                                            mPluginDCS.get()
#ifdef VE_SOUND
                                            ,
                                            mSoundManager
#endif 
                                            );
    mWaterEntity->SetNameAndDescriptions( "water" );
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelWorld::PreFrameUpdate()
{
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
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelWorld::CreateRoom( float width )
{
    ves::xplorer::scenegraph::ResourceManager* resourceManager =
        ves::xplorer::scenegraph::ResourceManager::instance();

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
        ( resourceManager->get< osg::Program, osg::ref_ptr >
        ( "RoomProgram" ) ).get(), osg::StateAttribute::ON );

    roomStateSet->setTextureAttributeAndModes( 1,
        ( resourceManager->get< osg::TextureCubeMap, osg::ref_ptr >
        ( "CubeMap" ) ).get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > envMapUniform =
        new osg::Uniform( "envMap", 1 );
    roomStateSet->addUniform( envMapUniform.get() );

    roomGeode->setStateSet( roomStateSet.get() );
}
////////////////////////////////////////////////////////////////////////////////
