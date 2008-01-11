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
//#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //
#include "btBulletDynamicsCommon.h"
#include <BulletCollision/CollisionShapes/btCylinderShape.h>

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
    
    m_imageMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
void World::Initialize()
{
    m_imageMap.insert( std::make_pair( "right", osgDB::readImageFile(
        "Textures/CubeMaps/NvidiaLobby/nvlobby_new_positive_x.tga" ) ) );
    m_imageMap.insert( std::make_pair( "left", osgDB::readImageFile(
        "Textures/CubeMaps/NvidiaLobby/nvlobby_new_negative_x.tga" ) ) );
    m_imageMap.insert( std::make_pair( "front", osgDB::readImageFile(
        "Textures/CubeMaps/NvidiaLobby/nvlobby_new_positive_y.tga" ) ) );
    m_imageMap.insert( std::make_pair( "back", osgDB::readImageFile(
        "Textures/CubeMaps/NvidiaLobby/nvlobby_new_negative_y.tga" ) ) );
    m_imageMap.insert( std::make_pair( "top", osgDB::readImageFile(
        "Textures/CubeMaps/NvidiaLobby/nvlobby_new_positive_z.tga" ) ) );
    m_imageMap.insert( std::make_pair( "bottom", osgDB::readImageFile(
        "Textures/CubeMaps/NvidiaLobby/nvlobby_new_negative_z.tga" ) ) );

    CreateRoom( 200.0 );

    m_tcm->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setWrap( osg::Texture::WRAP_R, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setFilter( osg::Texture::MIN_FILTER, osg::Texture::LINEAR );

    m_tcm->setImage( osg::TextureCubeMap::POSITIVE_X, m_imageMap[ "right" ] );
    m_tcm->setImage( osg::TextureCubeMap::NEGATIVE_X, m_imageMap[ "left" ] );
    m_tcm->setImage( osg::TextureCubeMap::POSITIVE_Y, m_imageMap[ "front" ] );
    m_tcm->setImage( osg::TextureCubeMap::NEGATIVE_Y, m_imageMap[ "back" ] );
    m_tcm->setImage( osg::TextureCubeMap::POSITIVE_Z, m_imageMap[ "top" ] );
    m_tcm->setImage( osg::TextureCubeMap::NEGATIVE_Z, m_imageMap[ "bottom" ] );

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

    /*
    m_quarterEntity = new demo::QuarterEntity( "Models/IVEs/quarter_physics.ive",
                                               m_pluginDCS.get(),
                                               m_physicsSimulator );
    m_quarterEntity->SetNameAndDescriptions( "quarter_physics" );
    double quarterPosition[ 3 ] = { -3.5, 0.7, 4.0 };
    //m_quarterEntity->GetDCS()->setAttitude( osg::Quat( 90.0, osg::Vec3f( 0, 1, 0 ) ) );
    m_quarterEntity->GetDCS()->SetTranslationArray( quarterPosition );
    m_quarterEntity->InitPhysics();
    m_quarterEntity->GetPhysicsRigidBody()->SetStoreCollisions( true );
    m_quarterEntity->GetPhysicsRigidBody()->SetMass( 1.0 );
    m_quarterEntity->GetPhysicsRigidBody()->setFriction( 0.5 );
    m_quarterEntity->GetPhysicsRigidBody()->setRestitution( 0.0 );

    btCylinderShape* cylinderShape = new btCylinderShape( btVector3( 0.07, 0.07, 0.002 ) );
    m_quarterEntity->GetPhysicsRigidBody()->UserDefinedShape( cylinderShape );
    //m_quarterEntity->GetPhysicsRigidBody()->setActivationState( WANTS_DEACTIVATION );
    m_quarterEntity->SetShaders();
    */

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

    m_waterEntity = new demo::WaterEntity( "Models/IVEs/water.ive",
                                           m_pluginDCS.get()
#ifdef VE_SOUND
                                           , m_soundManager
#endif 
                                          );
    m_waterEntity->SetNameAndDescriptions( "water" );
    m_waterEntity->SetShaders( m_tcm.get() );
}
////////////////////////////////////////////////////////////////////////////////
void World::PreFrameUpdate()
{
#ifdef VE_SOUND
    if( m_marbleEntity->GetPhysicsRigidBody()->CollisionInquiry(
        m_slideEntity->GetPhysicsRigidBody() ) )
    {
        m_marbleEntity->GetMarbleOnWoodSound()->PushSoundEvent( 10 );
    }
    else if( m_marbleEntity->GetPhysicsRigidBody()->CollisionInquiry(
             m_railingEntity->GetPhysicsRigidBody() ) )
    {
        m_marbleEntity->GetMarbleOnMetalSound()->PushSoundEvent( 10 );
    }
    else if( m_marbleEntity->GetPhysicsRigidBody()->CollisionInquiry(
             m_funnelEntity->GetPhysicsRigidBody() ) )
    {
        m_marbleEntity->GetMarbleOnMarbleSound()->PushSoundEvent( 10 );
    }
#endif
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* World::GetPluginDCS()
{
    return m_pluginDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
void World::CreateRoom( float width )
{
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    osg::ref_ptr< osg::Box > room = new osg::Box( osg::Vec3( 0, 0, width * 0.5 ), width );
    osg::ref_ptr< osg::ShapeDrawable > shapeDrawable = new osg::ShapeDrawable( room.get() );

    geode->addDrawable( shapeDrawable.get() );

    char vertexPass[]=
        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "gl_TexCoord[ 0 ].xyz = gl_Vertex.xyz; \n"
        "} \n";

    char fragmentPass[] =
		"uniform samplerCube envMap; \n"

		"void main()  \n"
		"{  \n"
		    "gl_FragColor=textureCube( envMap, gl_TexCoord[ 0 ].xyz ); \n"
		"} \n";

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );

    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    stateset->setAttribute( program.get(), osg::StateAttribute::ON );

    stateset->setTextureAttributeAndModes( 0, m_tcm.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > envMap = new osg::Uniform( "envMap", 0 );
    stateset->addUniform( envMap.get() );

    shapeDrawable->setStateSet( stateset.get() );

    geode->addDrawable( shapeDrawable.get() );
    m_pluginDCS->addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////

} // end demo
