/// --- My Includes --- //
#include "Shaders.h"
#include "Scene.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

// --- OSG Includes --- //
#include <osg/Projection>
#include <osg/AnimationPath>
#include <osg/Texture2D>
#include <osg/Texture3D>
#include <osg/Group>
#include <osg/Geode>
#include <osg/ShapeDrawable>
#include <osg/MatrixTransform>
#include <osg/Light>
#include <osg/LightSource>
#include <osg/PolygonOffset>
#include <osg/Material>
#include <osg/Camera>
#include <osg/TexGenNode>
#include <osg/CullFace>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>

// --- C/C++ Libraries --- //
#include <iostream>
#include <sstream>

using namespace hyperlab;

////////////////////////////////////////////////////////////////////////////////
Scene::Scene( ves::xplorer::scenegraph::DCS* pluginDCS,
              ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
:
shader( new hyperlab::Shaders() ),

m_pluginDCS( pluginDCS ),
m_physicsSimulator( physicsSimulator ),

m_room( 0 ),

m_aluminumParts( 0 ),
m_aluminumPipes( 0 ),
m_black( 0 ),
m_brown( 0 ),
m_ceiling( 0 ),
m_details( 0 ),
m_floor( 0 ),
m_glass( 0 ),
m_lights( 0 ),
m_ltGreen( 0 ),
m_ltGrey( 0 ),
m_orange( 0 ),
m_red( 0 ),
m_redBrown( 0 ),
m_walls( 0 ),
m_whiteDucts( 0 ),
m_whitePipes( 0 ),
m_yellow( 0 ),

m_shadowedScene( 0 ),

m_shadow( 0 ),
m_jitter( 0 ),
m_camera( 0 ),
m_texgenNode( 0 ),

m_light( 0 ),
m_lightSource( 0 ),
m_lightTransform( 0 )
{
    InitScene();

    Defaults();
}
////////////////////////////////////////////////////////////////////////////////
Scene::~Scene()
{
    if( m_room )
    {
        delete m_room;
    }

    if( shader )
    {
        delete shader;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Scene::InitScene()
{
    CreateNodes();

    //Setup the custom lighting for the scene
    CreateLights();

    CreateShadowTexture();

    //CreateJitterTexture();
}
////////////////////////////////////////////////////////////////////////////////
void Scene::CreateLights()
{
    m_light = new osg::Light();
    m_light->setLightNum( 1 );
    m_light->setPosition( osg::Vec4( 0.0f, 0.0f, 10000.0f, 0.0f ) );

    m_lightSource = new osg::LightSource();
    m_lightSource->setLight( m_light.get() );
    m_lightSource->setLocalStateSetModes( osg::StateAttribute::ON );

    m_lightTransform = new osg::MatrixTransform();
    m_lightTransform->setMatrix( osg::Matrix::translate( osg::Vec3( 0.0f, 0.0f, 10000.0f ) ) );
    m_lightTransform->addChild( m_lightSource.get() );

    m_pluginDCS->addChild( m_lightTransform.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Scene::CreateNodes()
{
    //Set up the collision detection nodes for the room
    osg::ref_ptr< osg::Group > roomPhysics = new osg::Group();
    m_room = new ves::xplorer::scenegraph::CADEntity( roomPhysics.get(),
                                                      m_pluginDCS.get(),
                                                      m_physicsSimulator );

    //Load in the geometry for the room
    {
        m_aluminumParts = osgDB::readNodeFile( "./Models/IVEs/Room/AluminumParts.ive" );
        m_room->GetDCS()->addChild( m_aluminumParts.get() );
        m_aluminumPipes = osgDB::readNodeFile( "./Models/IVEs/Room/AluminumPipes.ive" );
        m_room->GetDCS()->addChild( m_aluminumPipes.get() );
        m_black = osgDB::readNodeFile( "./Models/IVEs/Room/Black.ive" );
        m_room->GetDCS()->addChild( m_black.get() );
        m_brown = osgDB::readNodeFile( "./Models/IVEs/Room/Brown.ive" );
        m_room->GetDCS()->addChild( m_brown.get() );
        m_ceiling = osgDB::readNodeFile( "./Models/IVEs/Room/Ceiling.ive" );
        roomPhysics->addChild( m_ceiling.get() );
        m_details = osgDB::readNodeFile( "./Models/IVEs/Room/Details.ive" );
        m_room->GetDCS()->addChild( m_details.get() );
        m_floor = osgDB::readNodeFile( "./Models/IVEs/Room/Floor.ive" );
        roomPhysics->addChild( m_floor.get() );
        m_glass = osgDB::readNodeFile( "./Models/IVEs/Room/Glass.ive" );
        m_room->GetDCS()->addChild( m_glass.get() );
        m_lights = osgDB::readNodeFile( "./Models/IVEs/Room/Lights.ive" );
        m_room->GetDCS()->addChild( m_lights.get() );
        m_ltGreen = osgDB::readNodeFile( "./Models/IVEs/Room/LtGreen.ive" );
        m_room->GetDCS()->addChild( m_ltGreen.get() );
        m_ltGrey = osgDB::readNodeFile( "./Models/IVEs/Room/LtGrey.ive" );
        m_room->GetDCS()->addChild( m_ltGrey.get() );
        m_orange = osgDB::readNodeFile( "./Models/IVEs/Room/Orange.ive" );
        m_room->GetDCS()->addChild( m_orange.get() );
        m_red = osgDB::readNodeFile( "./Models/IVEs/Room/Red.ive" );
        m_room->GetDCS()->addChild( m_red.get() );
        m_redBrown = osgDB::readNodeFile( "./Models/IVEs/Room/RedBrown.ive" );
        m_room->GetDCS()->addChild( m_redBrown.get() );
        m_walls = osgDB::readNodeFile( "./Models/IVEs/Room/Walls.ive" );
        m_room->GetDCS()->addChild( m_whitePipes.get() );
        m_whiteDucts = osgDB::readNodeFile( "./Models/IVEs/Room/WhiteDucts.ive" );
        roomPhysics->addChild( m_walls.get() );
        m_whitePipes = osgDB::readNodeFile( "./Models/IVEs/Room/WhitePipes.ive" );
        m_room->GetDCS()->addChild( m_whiteDucts.get() );
        m_yellow = osgDB::readNodeFile( "./Models/IVEs/Room/Yellow.ive" );
        m_room->GetDCS()->addChild( m_yellow.get() );

        //Set up material properties for the room geometry
        osg::ref_ptr< osg::StateSet > stateset;

        osg::ref_ptr< osg::Material > aluminumPartsMaterial = new osg::Material();
        aluminumPartsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        aluminumPartsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        aluminumPartsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.4f, 0.4f, 0.6f, 1.0f ) );
        aluminumPartsMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        aluminumPartsMaterial->setShininess( osg::Material::FRONT, 5.0f );
        stateset = m_aluminumParts->getOrCreateStateSet();
        stateset->setAttributeAndModes( aluminumPartsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > aluminumPipesMaterial = new osg::Material();
        aluminumPipesMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        aluminumPipesMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        aluminumPipesMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.4f, 0.4f, 0.6f, 1.0f ) );
        aluminumPipesMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        aluminumPipesMaterial->setShininess( osg::Material::FRONT, 5.0f );
        stateset = m_aluminumPipes->getOrCreateStateSet();
        stateset->setAttributeAndModes( aluminumPipesMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > blackMaterial = new osg::Material();
        blackMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        blackMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        blackMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        blackMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        blackMaterial->setShininess( osg::Material::FRONT, 15.0f );
        stateset = m_black->getOrCreateStateSet();
        stateset->setAttributeAndModes( blackMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > brownMaterial = new osg::Material();
        brownMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        brownMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.2f, 0.1f, 0.05f, 1.0f ) );
        brownMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.5f, 0.3f, 0.15f, 1.0f ) );
        brownMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        brownMaterial->setShininess( osg::Material::FRONT, 15.0f );
        stateset = m_brown->getOrCreateStateSet();
        stateset->setAttributeAndModes( brownMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > ceilingMaterial = new osg::Material();
        ceilingMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        ceilingMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.3f, 0.3f, 0.3f, 1.0f ) );
        ceilingMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.3f, 0.3f, 0.3f, 1.0f ) );
        ceilingMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
        ceilingMaterial->setShininess( osg::Material::FRONT, 15.0f );
        stateset = m_ceiling->getOrCreateStateSet();
        stateset->setAttributeAndModes( ceilingMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > detailsMaterial = new osg::Material();
        detailsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 2.0f, 2.0f, 2.0f, 1.0f ) );
        detailsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.8f, 0.8f, 0.8f, 1.0f ) );
        detailsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
        detailsMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
        detailsMaterial->setShininess( osg::Material::FRONT, 15.0f );
        stateset = m_details->getOrCreateStateSet();
        stateset->setAttributeAndModes( detailsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > floorMaterial = new osg::Material();
        floorMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.8f, 1.8f, 1.8f, 1.0f ) );
        floorMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.9f, 0.75f, 0.2f, 1.0f ) );
        floorMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.02f, 0.02f, 0.01f, 1.0f ) );
        floorMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.02f, 0.02f, 0.01f, 1.0f ) );
        floorMaterial->setShininess( osg::Material::FRONT, 5.0f );
        stateset = m_floor->getOrCreateStateSet();
        stateset->setAttributeAndModes( floorMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > glassMaterial = new osg::Material();
        glassMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        glassMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        glassMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        glassMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        glassMaterial->setShininess( osg::Material::FRONT, 15.0f );
        stateset = m_glass->getOrCreateStateSet();
        stateset->setAttributeAndModes( glassMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > lightsMaterial = new osg::Material();
        lightsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        lightsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        lightsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        lightsMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        lightsMaterial->setShininess( osg::Material::FRONT, 15.0f );
        stateset = m_lights->getOrCreateStateSet();
        stateset->setAttributeAndModes( lightsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > ltGreenMaterial = new osg::Material();
        ltGreenMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        ltGreenMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.65f, 0.5f, 1.0f ) );
        ltGreenMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.8f, 0.5f, 1.0f ) );
        ltGreenMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        ltGreenMaterial->setShininess( osg::Material::FRONT, 10.0f );
        stateset = m_ltGreen->getOrCreateStateSet();
        stateset->setAttributeAndModes( ltGreenMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > ltGreyMaterial = new osg::Material();
        ltGreyMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        ltGreyMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        ltGreyMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        ltGreyMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.2f, 0.2f, 0.2f, 1.0f ) );
        ltGreyMaterial->setShininess( osg::Material::FRONT, 5.0f );
        stateset = m_ltGrey->getOrCreateStateSet();
        stateset->setAttributeAndModes( ltGreyMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > orangeMaterial = new osg::Material();
        orangeMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        orangeMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.5f, 0.1f, 0.1f, 1.0f ) );
        orangeMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 1.0f, 0.65f, 0.3f, 1.0f ) );
        orangeMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        orangeMaterial->setShininess( osg::Material::FRONT, 10.0f );
        stateset = m_orange->getOrCreateStateSet();
        stateset->setAttributeAndModes( orangeMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > redMaterial = new osg::Material();
        redMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        redMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        redMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        redMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        redMaterial->setShininess( osg::Material::FRONT, 15.0f );
        stateset = m_red->getOrCreateStateSet();
        stateset->setAttributeAndModes( redMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > redBrownMaterial = new osg::Material();
        redBrownMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        redBrownMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.3f, 0.25f, 0.2f, 1.0f ) );
        redBrownMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.14f, 0.07f, 0.0f, 1.0f ) );
        redBrownMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.01f, 0.01f, 0.01f, 1.0f ) );
        redBrownMaterial->setShininess( osg::Material::FRONT, 10.0f );
        stateset = m_redBrown->getOrCreateStateSet();
        stateset->setAttributeAndModes( redBrownMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > wallsMaterial = new osg::Material();
        wallsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 3.4f, 3.4f, 3.4f, 1.0f ) );
        wallsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.85f, 0.7f, 0.3f, 1.0f ) );
        wallsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.02f, 0.02f, 0.01f, 1.0f ) );
        wallsMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.01f, 0.01f, 0.01f, 1.0f ) );
        wallsMaterial->setShininess( osg::Material::FRONT, 5.0f );
        stateset = m_walls->getOrCreateStateSet();
        stateset->setAttributeAndModes( wallsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > whiteDuctsMaterial = new osg::Material();
        whiteDuctsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        whiteDuctsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        whiteDuctsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.9f, 0.9f, 0.95f, 1.0f ) );
        whiteDuctsMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        whiteDuctsMaterial->setShininess( osg::Material::FRONT, 10.0f );
        stateset = m_whiteDucts->getOrCreateStateSet();
        stateset->setAttributeAndModes( whiteDuctsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > whitePipesMaterial = new osg::Material();
        whitePipesMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        whitePipesMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        whitePipesMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.9f, 0.9f, 0.95f, 1.0f ) );
        whitePipesMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        whitePipesMaterial->setShininess( osg::Material::FRONT, 15.0f );
        stateset = m_whitePipes->getOrCreateStateSet();
        stateset->setAttributeAndModes( whitePipesMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > yellowMaterial = new osg::Material();
        yellowMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        yellowMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.3f, 1.0f ) );
        yellowMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 1.0f, 0.85f, 0.3f, 1.0f ) );
        yellowMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        yellowMaterial->setShininess( osg::Material::FRONT, 10.0f );
        stateset = m_yellow->getOrCreateStateSet();
        stateset->setAttributeAndModes( yellowMaterial.get(), osg::StateAttribute::ON );
    }
        /*
        frame=osgDB::readNodeFile("./Models/IVEs/Frame.ive");
        stateset=frame->getOrCreateStateSet();
        //stateset->setAttributeAndModes(frame_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(frame.get());

        railing=osgDB::readNodeFile("./Models/IVEs/Railing.ive");
        stateset=railing->getOrCreateStateSet();
        //stateset->setAttributeAndModes(railing_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(railing.get());

        plenum_piping=osgDB::readNodeFile("./Models/IVEs/PlenumPiping.ive");
        stateset=plenum_piping->getOrCreateStateSet();
        //stateset->setAttributeAndModes(plenum_piping_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(plenum_piping.get());

        blower_components=osgDB::readNodeFile("./Models/IVEs/BlowerComponents.ive");
        stateset=blower_components->getOrCreateStateSet();
        //stateset->setAttributeAndModes(blower_components_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(blower_components.get());

        brackets=osgDB::readNodeFile("./Models/IVEs/Brackets.ive");
        stateset=brackets->getOrCreateStateSet();
        //stateset->setAttributeAndModes(plenum_piping_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(brackets.get());

        cement_base=osgDB::readNodeFile("./Models/IVEs/CementBase.ive");
        stateset=cement_base->getOrCreateStateSet();
        //stateset->setAttributeAndModes(brackets_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(cement_base.get());

        combustor_piping=osgDB::readNodeFile("./Models/IVEs/CombustorPiping.ive");
        stateset=combustor_piping->getOrCreateStateSet();
        //stateset->setAttributeAndModes(combustor_piping_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(combustor_piping.get());

        compressor_inlet=osgDB::readNodeFile("./Models/IVEs/CompressorInlet.ive");
        stateset=compressor_inlet->getOrCreateStateSet();
        //stateset->setAttributeAndModes(compressor_inlet_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(compressor_inlet.get());

        heat_exchanger=osgDB::readNodeFile("./Models/IVEs/HeatExchanger.ive");
        stateset=heat_exchanger->getOrCreateStateSet();
        //stateset->setAttributeAndModes(heat_exchanger_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(heat_exchanger.get());

        heat_exchanger_sweep=osgDB::readNodeFile("./Models/IVEs/HeatExchangerSweep.ive");
        stateset=heat_exchanger_sweep->getOrCreateStateSet();
        //stateset->setAttributeAndModes(heat_exchanger_sweep_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(heat_exchanger_sweep.get());

        load=osgDB::readNodeFile("./Models/IVEs/Load.ive");
        stateset=load->getOrCreateStateSet();
        //stateset->setAttributeAndModes(load_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(load.get());

        plenum_system=osgDB::readNodeFile("./Models/IVEs/PlenumSystem.ive");
        stateset=plenum_system->getOrCreateStateSet();
        //stateset->setAttributeAndModes(plenum_system_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(plenum_system.get());

        relief_piping=osgDB::readNodeFile("./Models/IVEs/ReliefPiping.ive");
        stateset=relief_piping->getOrCreateStateSet();
        //stateset->setAttributeAndModes(relief_piping_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(relief_piping.get());

        shell=osgDB::readNodeFile("./Models/IVEs/Shell.ive");
        stateset=shell->getOrCreateStateSet();
        //stateset->setAttributeAndModes(shell_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(shell.get());

        stack=osgDB::readNodeFile("./Models/IVEs/Stack.ive");
        stateset=stack->getOrCreateStateSet();
        //stateset->setAttributeAndModes(stack_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(stack.get());

        turbine_exhaust=osgDB::readNodeFile("./Models/IVEs/TurbineExhaust.ive");
        stateset=turbine_exhaust->getOrCreateStateSet();
        //stateset->setAttributeAndModes(turbine_exhaust_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(turbine_exhaust.get());

        turbine_postcombustor=osgDB::readNodeFile("./Models/IVEs/TurbinePostCombustor.ive");
        stateset=turbine_postcombustor->getOrCreateStateSet();
        //stateset->setAttributeAndModes(turbine_postcombustor_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(turbine_postcombustor.get());

        miscellaneous=osgDB::readNodeFile("./Models/IVEs/Instrumentation.ive");
        stateset=miscellaneous->getOrCreateStateSet();
        //stateset->setAttributeAndModes(miscellaneous_material.get(),osg::StateAttribute::ON);
        m_shadowedScene->addChild(miscellaneous.get());

        */

    //Create physics mesh for room
    m_room->InitPhysics();
    m_room->GetPhysicsRigidBody()->SetMass( 0.0 );
    m_room->GetPhysicsRigidBody()->setFriction( 0.5 );
    m_room->GetPhysicsRigidBody()->setRestitution( 0.0 );
    m_room->GetPhysicsRigidBody()->StaticConcaveShape();

    //Collect the showed nodes into a group for easy reference
    m_shadowedScene = new osg::Group();
    m_shadowedScene->addChild( m_aluminumParts.get() );
    m_shadowedScene->addChild( m_aluminumPipes.get() );
    m_shadowedScene->addChild( m_black.get() );
    m_shadowedScene->addChild( m_brown.get() );
    m_shadowedScene->addChild( m_details.get() );
    m_shadowedScene->addChild( m_floor.get() );
    m_shadowedScene->addChild( m_ltGreen.get() );
    m_shadowedScene->addChild( m_ltGrey.get() );
    m_shadowedScene->addChild( m_orange.get() );
    m_shadowedScene->addChild( m_red.get() );
    m_shadowedScene->addChild( m_redBrown.get() );
    m_shadowedScene->addChild( m_walls.get() );
    m_shadowedScene->addChild( m_whiteDucts.get() );
    m_shadowedScene->addChild( m_whitePipes.get() );
    m_shadowedScene->addChild( m_yellow.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Scene::Defaults()
{
    //Set light defaults
    m_light->setAmbient( osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
    m_light->setDiffuse( osg::Vec4( 0.9f, 0.9f, 0.9f, 1.0f ) );
    m_light->setSpecular( osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );

    shader->SetOptions( m_ceiling.get(), false, false, NULL, NULL, &std::string( "WallMap" ) );
    shader->SetOptions( m_details.get(), false, false, NULL, NULL, &std::string( "Decoration" ) );
    shader->SetOptions( m_floor.get(), false, false, NULL, NULL, &std::string( "WallMap" ));
    shader->SetOptions( m_walls.get(), false, false, NULL, NULL, &std::string( "WallMap" ) );

    /*
    shader->SetOptions( m_aluminumParts.get() );
    shader->SetOptions( m_aluminumPipes.get() );
    shader->SetOptions( m_black.get() );
    shader->SetOptions( m_brown.get() );
    shader->SetOptions( m_glass.get() );
    //shader->SetOptions( m_lights.get() );
    shader->SetOptions( m_ltGreen.get() );
    shader->SetOptions( m_ltGrey.get() );
    shader->SetOptions( m_orange.get() );
    shader->SetOptions( m_red.get() );
    shader->SetOptions( m_redBrown.get() );
    shader->SetOptions( m_whiteDucts.get() );
    shader->SetOptions( m_whitePipes.get() );
    shader->SetOptions( m_yellow.get() );
    */


    //Set material defaults
    /*
    frame_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    frame_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.1f,0.1f,0.1f,1.0f));
    frame_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.1f,0.1f,0.1f,1.0f));
    frame_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    frame_material->setShininess(osg::Material::FRONT,12.0f);

    railing_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    railing_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.3f,1.0f));
    railing_material->setDiffuse(osg::Material::FRONT,osg::Vec4(1.0f,0.85f,0.3f,1.0f));
    railing_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    railing_material->setShininess(osg::Material::FRONT,10.0f);

    plenum_piping_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    plenum_piping_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    plenum_piping_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    plenum_piping_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    plenum_piping_material->setShininess(osg::Material::FRONT,10.0f);

    blower_components_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    blower_components_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    blower_components_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    blower_components_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    blower_components_material->setShininess(osg::Material::FRONT,10.0f);

    brackets_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    brackets_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    brackets_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    brackets_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    brackets_material->setShininess(osg::Material::FRONT,10.0f);

    cement_base_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    cement_base_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    cement_base_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    cement_base_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    cement_base_material->setShininess(osg::Material::FRONT,10.0f);

    combustor_piping_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    combustor_piping_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    combustor_piping_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    combustor_piping_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    combustor_piping_material->setShininess(osg::Material::FRONT,10.0f);

    compressor_inlet_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    compressor_inlet_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    compressor_inlet_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    compressor_inlet_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    compressor_inlet_material->setShininess(osg::Material::FRONT,10.0f);

    heat_exchanger_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    heat_exchanger_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    heat_exchanger_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    heat_exchanger_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    heat_exchanger_material->setShininess(osg::Material::FRONT,10.0f);

    heat_exchanger_sweep_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    heat_exchanger_sweep_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    heat_exchanger_sweep_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    heat_exchanger_sweep_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    heat_exchanger_sweep_material->setShininess(osg::Material::FRONT,10.0f);

    load_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    load_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    load_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    load_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    load_material->setShininess(osg::Material::FRONT,10.0f);

    plenum_system_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    plenum_system_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    plenum_system_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    plenum_system_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    plenum_system_material->setShininess(osg::Material::FRONT,10.0f);

    relief_piping_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    relief_piping_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    relief_piping_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    relief_piping_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    relief_piping_material->setShininess(osg::Material::FRONT,10.0f);

    shell_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    shell_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    shell_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    shell_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    shell_material->setShininess(osg::Material::FRONT,10.0f);

    stack_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    stack_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    stack_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    stack_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    stack_material->setShininess(osg::Material::FRONT,10.0f);

    turbine_exhaust_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    turbine_exhaust_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    turbine_exhaust_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    turbine_exhaust_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    turbine_exhaust_material->setShininess(osg::Material::FRONT,10.0f);

    turbine_postcombustor_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    turbine_postcombustor_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    turbine_postcombustor_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    turbine_postcombustor_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    turbine_postcombustor_material->setShininess(osg::Material::FRONT,10.0f);

    miscellaneous_material->setEmission(osg::Material::FRONT,osg::Vec4(1.0f,1.0f,1.0f,1.0f));
    miscellaneous_material->setAmbient(osg::Material::FRONT,osg::Vec4(0.45f,0.45f,0.45f,1.0f));
    miscellaneous_material->setDiffuse(osg::Material::FRONT,osg::Vec4(0.6f,0.6f,0.6f,1.0f));
    miscellaneous_material->setSpecular(osg::Material::FRONT,osg::Vec4(0.5f,0.5f,0.5f,1.0f));
    miscellaneous_material->setShininess(osg::Material::FRONT,10.0f);
    */
}
////////////////////////////////////////////////////////////////////////////////
void Scene::CreateShadowTexture()
{
    m_shadow = new osg::Texture2D();
    m_camera = new osg::Camera();
    m_texgenNode = new osg::TexGenNode();

    unsigned int texWidth = 4096;
    unsigned int texHeight = 4096;

    //Create the shadow texture
    m_shadow->setTextureSize( texWidth, texHeight );
    m_shadow->setInternalFormat( GL_DEPTH_COMPONENT );
    m_shadow->setSourceType( GL_UNSIGNED_INT );

    m_shadow->setShadowComparison( true );
    m_shadow->setShadowCompareFunc( osg::Texture::LEQUAL );

    m_shadow->setShadowTextureMode( osg::Texture::LUMINANCE );
    m_shadow->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    m_shadow->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    m_shadow->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    m_shadow->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );

    //Set up the "render to texture" camera
    {
        //Create the camera
        m_camera->setClearMask( GL_DEPTH_BUFFER_BIT );
        m_camera->setClearColor( osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        m_camera->setComputeNearFarMode( osg::Camera::DO_NOT_COMPUTE_NEAR_FAR );

        //Set viewport
        m_camera->setViewport( 0, 0, texWidth, texHeight );

        osg::ref_ptr< osg::StateSet > localStateset = m_camera->getOrCreateStateSet();
        localStateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

        float factor = 0.0f;
        float units = 1.0f;

        osg::ref_ptr< osg::PolygonOffset > polygonOffset = new osg::PolygonOffset();
        polygonOffset->setFactor( factor );
        polygonOffset->setUnits( units );
        localStateset->setAttribute( polygonOffset.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );
        localStateset->setMode( GL_POLYGON_OFFSET_FILL, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );

        osg::ref_ptr< osg::CullFace > cullFace = new osg::CullFace();
        cullFace->setMode( osg::CullFace::FRONT );
        localStateset->setAttribute( cullFace.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );
        localStateset->setMode( GL_CULL_FACE, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );

        //Set the camera to render before the main camera
        m_camera->setRenderOrder( osg::Camera::PRE_RENDER );

        //Tell the camera to use OpenGL frame buffer object where supported
        m_camera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );

        //Attach the texture and use it as the color buffer
        m_camera->attach( osg::Camera::DEPTH_BUFFER, m_shadow.get() );

        //Add subgraph to render
        m_camera->addChild( m_shadowedScene.get() );

        //Create the texgen node to project the tex coords onto the subgraph  
        m_texgenNode->setTextureUnit( 0 );

        osg::BoundingSphere bs;
        for( unsigned int i = 0; i < m_camera->getNumChildren(); ++i )
        {
            bs.expandBy( m_camera->getChild( i )->getBound() );
        }

        osg::Vec3 position = m_lightTransform->getMatrix().getTrans();

        float centerDistance = ( position - bs.center() ).length();

        float znear = centerDistance - bs.radius();
        float zfar = centerDistance + bs.radius();
        float zNearRatio = 0.001f;
        if( znear < zfar * zNearRatio )
        {
            znear = zfar * zNearRatio;
        }

        float top = ( bs.radius() / centerDistance ) * znear;
        float right = top;

        m_camera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
        m_camera->setProjectionMatrixAsFrustum( -right, right, -top, top, znear, zfar );
        m_camera->setViewMatrixAsLookAt( position, bs.center(), osg::Vec3( 0.0f, 1.0f, 0.0f ) );

        //Compute the matrix which takes a vertex from local coords into tex coords
        osg::Matrix MVPT = m_camera->getViewMatrix() *
                           m_camera->getProjectionMatrix() *
                           osg::Matrix::translate( 1.0f, 1.0f, 1.0f ) *
                           osg::Matrix::scale( 0.5f, 0.5f, 0.5f );

        //Texture Generation
        m_texgenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
        m_texgenNode->getTexGen()->setPlanesFromMatrix( MVPT );
    }

    m_pluginDCS->addChild( m_camera.get() );
    m_pluginDCS->addChild( m_texgenNode.get() );

    WriteOutShadow();
}
////////////////////////////////////////////////////////////////////////////////
// Implementation from Chapter 17, Efficient Soft-Edged Shadows Using Pixel Shader Branching, Yury Uralsky.
// GPU Gems 2, Matt Pharr ed. Addison-Wesley.
//
// Creates a 3D texture containing jittering data used in the shader to take samples of the shadow map.
void Scene::CreateJitterTexture()
{
    m_jitter = new osg::Texture3D();

    //Create a 3D texture with hw mipmapping
    m_jitter->setFilter( osg::Texture3D::MIN_FILTER, osg::Texture3D::NEAREST );
    m_jitter->setFilter( osg::Texture3D::MAG_FILTER, osg::Texture3D::NEAREST );
    m_jitter->setWrap( osg::Texture3D::WRAP_S, osg::Texture3D::REPEAT );
    m_jitter->setWrap( osg::Texture3D::WRAP_T, osg::Texture3D::REPEAT );
    m_jitter->setWrap( osg::Texture3D::WRAP_R, osg::Texture3D::REPEAT );
    m_jitter->setUseHardwareMipMapGeneration( true );

    const unsigned int size = 16;
    const unsigned int gridW = 8;
    const unsigned int gridH = 8;
    unsigned int R = ( gridW * gridH / 2 );
    m_jitter->setTextureSize( size, size, R );

    //Then create the 3d image to fill with jittering data
    osg::ref_ptr< osg::Image > image3D = new osg::Image();
    unsigned char* data3D = new unsigned char[ size * size * R * 4 ];

    for( unsigned int s = 0; s < size; ++s )
    {
        for( unsigned int t = 0; t < size; ++t )
        {
            float v[ 4 ], d[ 4 ];

            for( unsigned int r = 0; r < R; ++r )
            {
                const int x = r % ( gridW / 2 );
                const int y = ( gridH - 1 ) - ( r / ( gridW / 2 ) );

                //Generate points on a  regular gridW x gridH rectangular
                //grid.   We  multiply  x   by  2  because,  we  treat  2
                //consecutive x  each loop iteration.  Add 0.5f  to be in
                //the center of the pixel. x, y belongs to [ 0.0, 1.0 ].
                v[ 0 ] = static_cast< float >( x * 2     + 0.5f ) / gridW;
                v[ 1 ] = static_cast< float >( y         + 0.5f ) / gridH;
                v[ 2 ] = static_cast< float >( x * 2 + 1 + 0.5f ) / gridW;
                v[ 3 ] = v[ 1 ];

                //Jitter positions. ( 0.5f / w ) == ( 1.0f / 2*w )
                v[ 0 ] += ( static_cast< float >( rand() ) * 2.f / RAND_MAX - 1.f ) * ( 0.5f / gridW );
                v[ 1 ] += ( static_cast< float >( rand() ) * 2.f / RAND_MAX - 1.f ) * ( 0.5f / gridH );
                v[ 2 ] += ( static_cast< float >( rand() ) * 2.f / RAND_MAX - 1.f ) * ( 0.5f / gridW );
                v[ 3 ] += ( static_cast< float >( rand() ) * 2.f / RAND_MAX - 1.f ) * ( 0.5f / gridH );

                //Warp to disk; values in [ -1, 1 ]
                d[ 0 ] = sqrtf( v[ 1 ] ) * cosf( 2.f * 3.1415926f * v[ 0 ] );
                d[ 1 ] = sqrtf( v[ 1 ] ) * sinf( 2.f * 3.1415926f * v[ 0 ] );
                d[ 2 ] = sqrtf( v[ 3 ] ) * cosf( 2.f * 3.1415926f * v[ 2 ] );
                d[ 3 ] = sqrtf( v[ 3 ] ) * sinf( 2.f * 3.1415926f * v[ 2 ] );

                //store d into unsigned values [ 0, 255 ]
                const unsigned int tmp = ( ( r * size * size ) + ( t * size ) + s ) * 4;
                data3D[ tmp + 0 ] = static_cast< unsigned char >( ( 1.f + d[ 0 ] ) * 127  );
                data3D[ tmp + 1 ] = static_cast< unsigned char >( ( 1.f + d[ 1 ] ) * 127  );
                data3D[ tmp + 2 ] = static_cast< unsigned char >( ( 1.f + d[ 2 ] ) * 127  );
                data3D[ tmp + 3 ] = static_cast< unsigned char >( ( 1.f + d[ 3 ] ) * 127  );
            }
        }
    }

    //The GPU Gem implementation uses a NV specific internal texture format (GL_SIGNED_RGBA_NV)
    //In order to make it more generic, we use GL_RGBA4 which should be cross platform.
    image3D->setImage( size, size, R, GL_RGBA4, GL_RGBA, GL_UNSIGNED_BYTE, data3D, osg::Image::USE_NEW_DELETE );
    m_jitter->setImage( image3D.get() );

    /*
    ss->setTextureAttributeAndModes((int)_textureUnit + 1, m_jitter, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
    ss->setTextureMode((int)_textureUnit + 1,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
    ss->setTextureMode((int)_textureUnit + 1,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
    ss->setTextureMode((int)_textureUnit + 1,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
    */
}
////////////////////////////////////////////////////////////////////////////////
void Scene::WriteOutShadow()
{
    osg::ref_ptr< osg::Image > image = new osg::Image();
    image->setInternalTextureFormat( GL_DEPTH_COMPONENT );

    class RGB
    {
    public:
        unsigned char r, g, b;
    };

    /*RGB* pixels;
    pixels=new RGB[3*m_shadow->getTextureWidth()*m_shadow->getTextureHeight()*m_shadow->getTextureDepth()];
    glGetTexImage(GL_TEXTURE_2D,0,GL_LUMINANCE,GL_UNSIGNED_BYTE,pixels);

    glPixelStorei(GL_UNPACK_ALIGNMENT,1);
    glDrawPixels(m_shadow->getTextureHeight(),m_shadow->getTextureWidth(),GL_RGB,GL_UNSIGNED_BYTE,pixels);
    image->readPixels(0,0,512,512,GL_LUMINANCE,GL_UNSIGNED_BYTE);
    osgDB::writeImageFile(*image.get(),"./Textures/m_shadow.bmp");*/
}
////////////////////////////////////////////////////////////////////////////////
