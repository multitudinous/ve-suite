// --- My Includes --- //
#include "World.h"
#include "FunnelEntity.h"
#include "MarbleEntity.h"
#include "QuarterEntity.h"
#include "RampEntity.h"
#include "SlideEntity.h"
#include "WaterEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/Sound.h>

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
World::World( ves::xplorer::scenegraph::DCS* pluginDCS )
:
m_pluginDCS( pluginDCS ),
m_tcm( new osg::TextureCubeMap() ),
m_funnelEntity( 0 ),
m_marbleEntity( 0 ),
m_quarterEntity( 0 ),
m_rampEntity( 0 ),
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
    delete m_rampEntity;
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


    m_funnelEntity = new demo::FunnelEntity( "Models/IVEs/funnel_physics.ive", m_pluginDCS.get() );
    m_funnelEntity->SetNameAndDescriptions( "funnel_physics" );
    m_funnelEntity->InitPhysics();
    m_funnelEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
    m_funnelEntity->GetPhysicsRigidBody()->SetFriction( 0.5 );
    m_funnelEntity->GetPhysicsRigidBody()->SetRestitution( 0.0 );
    m_funnelEntity->GetPhysicsRigidBody()->StaticConcaveShape();
    m_funnelEntity->SetShaders( m_tcm.get() );

    m_marbleEntity = new demo::MarbleEntity( "Models/IVEs/marble_physics.ive", m_pluginDCS.get() );
    m_marbleEntity->SetNameAndDescriptions( "marble_physics" );
    double marblePosition[ 3 ] = { 5.5, 2.0, 5.0 };
    m_marbleEntity->GetDCS()->SetTranslationArray( marblePosition );
    m_marbleEntity->InitPhysics();
    m_marbleEntity->GetPhysicsRigidBody()->SetMass( 1.0 );
    m_marbleEntity->GetPhysicsRigidBody()->SetFriction( 0.5 );
    m_marbleEntity->GetPhysicsRigidBody()->SetRestitution( 0.0 );
    //The real value of the radius should be 0.05, need to look at this
    m_marbleEntity->GetPhysicsRigidBody()->SphereShape( 0.06 );
    m_marbleEntity->SetShaders( m_tcm.get() );
    m_marbleEntity->SetSounds();

    m_quarterEntity = new demo::QuarterEntity( "Models/IVEs/quarter_physics.ive", m_pluginDCS.get() );
    m_quarterEntity->SetNameAndDescriptions( "quarter_physics" );
    double quarterPosition[ 3 ] = { 0, 0, 0 };
    m_quarterEntity->GetDCS()->SetTranslationArray( quarterPosition );
    m_quarterEntity->InitPhysics();
    m_quarterEntity->GetPhysicsRigidBody()->SetMass( 1.0 );
    m_quarterEntity->GetPhysicsRigidBody()->SetFriction( 0.5 );
    m_quarterEntity->GetPhysicsRigidBody()->SetRestitution( 0.0 );
    m_quarterEntity->GetPhysicsRigidBody()->ConvexShape();

    m_rampEntity = new demo::RampEntity( "Models/IVEs/ramp_physics.ive", m_pluginDCS.get() );
    m_rampEntity->SetNameAndDescriptions( "ramp_physics" );
    m_rampEntity->InitPhysics();
    m_rampEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
    m_rampEntity->GetPhysicsRigidBody()->SetFriction( 0.5 );
    m_rampEntity->GetPhysicsRigidBody()->SetRestitution( 0.0 );
    m_rampEntity->GetPhysicsRigidBody()->StaticConcaveShape();

    m_slideEntity = new demo::SlideEntity( "Models/IVEs/slide_physics.ive", m_pluginDCS.get() );
    m_slideEntity->SetNameAndDescriptions( "slide_physics" );
    m_slideEntity->InitPhysics();
    m_slideEntity->GetPhysicsRigidBody()->SetMass( 0.0 );
    m_slideEntity->GetPhysicsRigidBody()->SetFriction( 0.5 );
    m_slideEntity->GetPhysicsRigidBody()->SetRestitution( 0.0 );
    m_slideEntity->GetPhysicsRigidBody()->StaticConcaveShape();

    m_waterEntity = new demo::WaterEntity( "Models/IVEs/water.ive", m_pluginDCS.get() );
    m_waterEntity->SetNameAndDescriptions( "water" );
    m_waterEntity->SetShaders( m_tcm.get() );
}
////////////////////////////////////////////////////////////////////////////////
void World::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* World::GetPluginDCS()
{
    return m_pluginDCS.get();
}
////////////////////////////////////////////////////////////////////////////////

} // end demo