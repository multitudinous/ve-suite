// --- My Includes --- //
#include "World.h"
#include "FunnelEntity.h"
#include "MarbleEntity.h"
#include "QuarterEntity.h"
#include "RailingEntity.h"
#include "SlideEntity.h"
#include "WaterEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/Sound.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

// --- OSG Includes --- //
#include <osg/Texture2D>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //
#include "btBulletDynamicsCommon.h"

// --- C/C++ Libraries --- //
#include <iostream>
#include <fstream>

namespace demo
{

////////////////////////////////////////////////////////////////////////////////
World::World( ves::xplorer::scenegraph::DCS* pluginDCS,
              ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator
#ifdef VE_SOUND
              , osgAL::SoundManager* soundManager
#endif
              )
:
m_pluginDCS( pluginDCS ),
m_physicsSimulator( physicsSimulator ),
#ifdef VE_SOUND
m_soundManager( soundManager ),
#endif
m_tcm( new osg::TextureCubeMap() ),
m_funnelEntity( 0 ),
m_marbleEntity( 0 ),
m_quarterEntity( 0 ),
m_railingEntity( 0 ),
m_slideEntity( 0 ),
m_waterEntity( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
World::~World()
{
    delete m_funnelEntity;
    delete m_marbleEntity;
    delete m_quarterEntity;
    delete m_railingEntity;
    delete m_slideEntity;
    delete m_waterEntity;
}
////////////////////////////////////////////////////////////////////////////////
void World::Initialize()
{
    m_tcm->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setWrap( osg::Texture::WRAP_R, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setFilter( osg::Texture::MIN_FILTER, osg::Texture::LINEAR );

    m_tcm->setImage( osg::TextureCubeMap::POSITIVE_X,
                     osgDB::readImageFile( "Textures/CloudyHills/right.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::NEGATIVE_X,
                     osgDB::readImageFile( "Textures/CloudyHills/left.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::POSITIVE_Y,
                     osgDB::readImageFile( "Textures/CloudyHills/top.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::NEGATIVE_Y,
                     osgDB::readImageFile( "Textures/CloudyHills/bottom.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::POSITIVE_Z,
                     osgDB::readImageFile( "Textures/CloudyHills/back.tga" ) );
    m_tcm->setImage( osg::TextureCubeMap::NEGATIVE_Z,
                     osgDB::readImageFile( "Textures/CloudyHills/front.tga" ) );

    m_physicsSimulator->SetCollisionInformation( true );

    m_funnelEntity = new demo::FunnelEntity( "Models/IVEs/funnel_physics.ive",
                                             m_pluginDCS.get(),
                                             m_physicsSimulator );
    m_funnelEntity->SetNameAndDescriptions( "funnel_physics" );
    m_funnelEntity->InitPhysics();
    m_funnelEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
    m_funnelEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    m_funnelEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );
    m_funnelEntity->GetPhysicsRigidBody()->StaticConcaveShape();
    m_funnelEntity->SetShaders( m_tcm.get() );

    m_marbleEntity = new demo::MarbleEntity( "Models/IVEs/marble_physics.ive",
                                             m_pluginDCS.get(),
                                             m_physicsSimulator
#ifdef VE_SOUND
                                             , m_soundManager
#endif
                                             );
    m_marbleEntity->SetNameAndDescriptions( "marble_physics" );
    double marblePosition[ 3 ] = { 4.85, 2.5, 5.75 };
    m_marbleEntity->GetDCS()->SetTranslationArray( marblePosition );
    m_marbleEntity->InitPhysics();
    m_marbleEntity->GetPhysicsRigidBody()->SetStoreCollisions( true );
    m_marbleEntity->GetPhysicsRigidBody()->SetMass( 1.0 );
    m_marbleEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    m_marbleEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );
    //The real value of the radius should be 0.05, need to look at this
    m_marbleEntity->GetPhysicsRigidBody()->SphereShape( 0.06 );
    m_marbleEntity->SetShaders( m_tcm.get() );

    m_quarterEntity = new demo::QuarterEntity( "Models/IVEs/quarter_physics.ive",
                                               m_pluginDCS.get(),
                                               m_physicsSimulator );
    m_quarterEntity->SetNameAndDescriptions( "quarter_physics" );
    double quarterPosition[ 3 ] = { -3.5, 0.7, 4.0 };
    m_quarterEntity->GetDCS()->setAttitude( osg::Quat( 90.0, osg::Vec3f( 0, 1, 0 ) ) );
    m_quarterEntity->GetDCS()->SetTranslationArray( quarterPosition );
    m_quarterEntity->InitPhysics();
    m_quarterEntity->GetPhysicsRigidBody()->SetStoreCollisions( true );
    m_quarterEntity->GetPhysicsRigidBody()->SetMass( 1.0 );
    m_quarterEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    m_quarterEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );
    m_quarterEntity->GetPhysicsRigidBody()->ConvexShape();
    m_quarterEntity->GetPhysicsRigidBody()->setActivationState( WANTS_DEACTIVATION );

    m_railingEntity = new demo::RailingEntity( "Models/IVEs/railing_physics.ive",
                                               m_pluginDCS.get(),
                                               m_physicsSimulator );
    m_railingEntity->SetNameAndDescriptions( "railing_physics" );
    m_railingEntity->InitPhysics();
    m_railingEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
    m_railingEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    m_railingEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );
    m_railingEntity->GetPhysicsRigidBody()->StaticConcaveShape();
    m_railingEntity->SetShaders( m_tcm.get() );

    m_slideEntity = new demo::SlideEntity( "Models/IVEs/slide_physics.ive",
                                           m_pluginDCS.get(),
                                           m_physicsSimulator );
    m_slideEntity->SetNameAndDescriptions( "slide_physics" );
    m_slideEntity->InitPhysics();
    m_slideEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
    m_slideEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    m_slideEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );
    m_slideEntity->GetPhysicsRigidBody()->StaticConcaveShape();
    m_slideEntity->SetShaders();

    m_waterEntity = new demo::WaterEntity( "Models/IVEs/water.ive", m_pluginDCS.get() );
    m_waterEntity->SetNameAndDescriptions( "water" );
    m_waterEntity->SetShaders( m_tcm.get() );
}
////////////////////////////////////////////////////////////////////////////////
void World::PreFrameUpdate()
{
    if( m_marbleEntity->GetPhysicsRigidBody()->CollisionInquiry(
        m_slideEntity->GetPhysicsRigidBody() ) )
    {
        m_marbleEntity->GetMarbleOnWoodSound()->PushSoundEvent( 10 );
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* World::GetPluginDCS()
{
    return m_pluginDCS.get();
}
////////////////////////////////////////////////////////////////////////////////

} // end demo