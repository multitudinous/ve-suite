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
#include <osg/Geometry>
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
mPhysicsSimulator( physicsSimulator ),

m_room( 0 ),

m_aluminumParts( 0 ),
m_aluminumPipes( 0 ),
m_black( 0 ),
m_brown( 0 ),
m_ceiling( 0 ),
m_coronas( 0 ),
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

m_blowerComponents( 0 ),
m_brackets( 0 ),
m_cableTray( 0 ),
m_cementBase( 0 ),
m_combustorInternals( 0 ),
m_combustorPiping( 0 ),
m_compressorInlet( 0 ),
m_frame( 0 ),
m_groundBolts( 0 ),
m_heatExchanger( 0 ),
m_heatExchangerSweep( 0 ),
m_instrumentation( 0 ),
m_load( 0 ),
m_plenumPiping( 0 ),
m_plenumSystem( 0 ),
m_railing( 0 ),
m_reliefPiping( 0 ),
m_reliefPipingAM( 0 ),
m_shell( 0 ),
m_stack( 0 ),
m_turbineExhaust( 0 ),
m_turbinePostCombustor( 0 ),
m_turbineSupport( 0 ),

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

    //shader->SetOptions( m_pluginDCS.get(), false, false );
    //DefaultVisuals();
    AdvancedVisuals();
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
void Scene::DefaultVisuals()
{
    shader->SetOptions( m_aluminumParts.get(), false, true );
    shader->SetOptions( m_aluminumPipes.get(), false, true );
    shader->SetOptions( m_black.get(), false, true );
    shader->SetOptions( m_brown.get(), false, true );
    shader->SetOptions( m_ceiling.get(), false, false, "WallMap" );
    shader->Lights( m_coronas.get() );
    shader->SetOptions( m_details.get(), false, false, "Decoration" );
    shader->SetOptions( m_floor.get(), false, false, "WallMap" );
    shader->SetOptions( m_glass.get(), false, true );
    shader->SetOptions( m_lights.get(), false, true );
    shader->SetOptions( m_ltGreen.get(), false, true );
    shader->SetOptions( m_ltGrey.get(), false, true );
    shader->SetOptions( m_orange.get(), false, true );
    shader->SetOptions( m_red.get(), false, true );
    shader->SetOptions( m_redBrown.get(), false, true );
    shader->SetOptions( m_walls.get(), false, false, "WallMap" );
    shader->SetOptions( m_whiteDucts.get(), false, true );
    shader->SetOptions( m_whitePipes.get(), false, true );
    shader->SetOptions( m_yellow.get(), false, true );
    
    shader->SetOptions( m_blowerComponents.get(), false, true );
    shader->SetOptions( m_brackets.get(), false, true );
    shader->SetOptions( m_cableTray.get(), false, true );
    shader->SetOptions( m_cementBase.get(), false, true );
    //shader->SetOptions( m_combustorInternals.get(), false, true );
    shader->SetOptions( m_combustorPiping.get(), false, true );
    shader->SetOptions( m_compressorInlet.get(), false, true );
    shader->SetOptions( m_frame.get(), false, true );
    shader->SetOptions( m_groundBolts.get(), false, true );
    shader->SetOptions( m_heatExchanger.get(), false, true );
    shader->SetOptions( m_heatExchangerSweep.get(), false, true );
    shader->SetOptions( m_instrumentation.get(), false, true );
    shader->SetOptions( m_load.get(), false, true );
    shader->SetOptions( m_plenumPiping.get(), false, true );
    shader->SetOptions( m_plenumSystem.get(), false, true );
    shader->SetOptions( m_railing.get(), false, true );
    shader->SetOptions( m_reliefPiping.get(), false, true );
    shader->SetOptions( m_reliefPipingAM.get(), false, true );
    shader->SetOptions( m_shell.get(), false, true );
    shader->SetOptions( m_stack.get(), false, true );
    shader->SetOptions( m_turbineExhaust.get(), false, true );
    shader->SetOptions( m_turbinePostCombustor.get(), false, true );
    shader->SetOptions( m_turbineSupport.get(), false, true );
}
////////////////////////////////////////////////////////////////////////////////
void Scene::AdvancedVisuals()
{
    float reflectionPercentage;

    shader->SetOptions( m_aluminumParts.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_aluminumPipes.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_black.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_brown.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->Lights( m_coronas.get() );
    shader->SetOptions( m_ceiling.get(), false, false, "WallMap" );
    shader->SetOptions( m_details.get(), false, false, "Decoration",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_floor.get(), false, false, "WallMap",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_glass.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_lights.get(), false, true );
    shader->SetOptions( m_ltGreen.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_ltGrey.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_orange.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_red.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_redBrown.get(), false, true );
    shader->SetOptions( m_walls.get(), false, false, "WallMap",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_whiteDucts.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_whitePipes.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_yellow.get(), false, true, "",
                        NULL, m_shadow.get() );

    shader->SetOptions( m_blowerComponents.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_brackets.get(), false, true );
    shader->SetOptions( m_cableTray.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_cementBase.get(), false, true, "",
                        NULL, m_shadow.get() );
    //shader->SetOptions( m_combustorInternals.get(), false, true, "",
                        //NULL, NULL );
    shader->SetOptions( m_combustorPiping.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_compressorInlet.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_frame.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_groundBolts.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_heatExchanger.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_heatExchangerSweep.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_instrumentation.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_load.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_plenumPiping.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_plenumSystem.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_railing.get(), false, true, "",
                        NULL, m_shadow.get() );
    shader->SetOptions( m_reliefPiping.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_reliefPipingAM.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_shell.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_stack.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_turbineExhaust.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_turbinePostCombustor.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
    shader->SetOptions( m_turbineSupport.get(), false, true, "",
                        &( reflectionPercentage = 0.05 ), m_shadow.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Scene::XRay()
{
    shader->SetOptions( m_aluminumParts.get(), true );
    shader->SetOptions( m_aluminumPipes.get(), true );
    shader->SetOptions( m_black.get(), true );
    shader->SetOptions( m_brown.get(), true );
    shader->SetOptions( m_ceiling.get(), true );
    //m_coronas->setNodeMask( 0 );
    shader->SetOptions( m_details.get(), true );
    shader->SetOptions( m_floor.get(), true );
    shader->SetOptions( m_glass.get(), true );
    shader->SetOptions( m_lights.get(), true );
    shader->SetOptions( m_ltGreen.get(), true );
    shader->SetOptions( m_ltGrey.get(), true );
    shader->SetOptions( m_orange.get(), true );
    shader->SetOptions( m_red.get(), true );
    shader->SetOptions( m_redBrown.get(), true );
    shader->SetOptions( m_walls.get(), true );
    shader->SetOptions( m_whiteDucts.get(), true );
    shader->SetOptions( m_whitePipes.get(), true );
    shader->SetOptions( m_yellow.get(), true );

    shader->SetOptions( m_blowerComponents.get(), true );
    shader->SetOptions( m_brackets.get(), true );
    shader->SetOptions( m_cableTray.get(), true );
    shader->SetOptions( m_cementBase.get(), true );
    //shader->SetOptions( m_combustorInternals.get(), true );
    shader->SetOptions( m_combustorPiping.get(), true );
    shader->SetOptions( m_compressorInlet.get(), true );
    shader->SetOptions( m_frame.get(), true );
    shader->SetOptions( m_groundBolts.get(), true );
    shader->SetOptions( m_heatExchanger.get(), true );
    shader->SetOptions( m_heatExchangerSweep.get(), true );
    shader->SetOptions( m_instrumentation.get(), true );
    shader->SetOptions( m_load.get(), true );
    shader->SetOptions( m_plenumPiping.get(), true );
    shader->SetOptions( m_plenumSystem.get(), true );
    shader->SetOptions( m_railing.get(), true );
    shader->SetOptions( m_reliefPiping.get(), true );
    shader->SetOptions( m_reliefPipingAM.get(), true );
    shader->SetOptions( m_shell.get(), true );
    shader->SetOptions( m_stack.get(), true );
    shader->SetOptions( m_turbineExhaust.get(), true );
    shader->SetOptions( m_turbinePostCombustor.get(), true );
    shader->SetOptions( m_turbineSupport.get(), true );
}
////////////////////////////////////////////////////////////////////////////////
osg::Light* Scene::GetLight()
{
    return m_light.get();
}
////////////////////////////////////////////////////////////////////////////////
void Scene::InitScene()
{
    //Setup the custom lighting for the scene
    CreateLights();

    CreateNodes();

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

    //Set light defaults
    m_light->setAmbient( osg::Vec4( 0.63f, 0.63f, 0.40f, 1.0f ) );
    m_light->setDiffuse( osg::Vec4( 0.90f, 0.90f, 0.45f, 1.0f ) );
    m_light->setSpecular( osg::Vec4( 0.78f, 0.78f, 0.5f, 1.0f ) );

    //Add in the corona quads for added effets
    m_coronas = new osg::Geode();
    m_coronas->setCullingActive( false );
    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > positions = new osg::Vec3Array();

    vertices->push_back( osg::Vec3( -1.0,  1.0, 0.0 ) );
    vertices->push_back( osg::Vec3( -1.0, -1.0, 0.0 ) );
    vertices->push_back( osg::Vec3(  1.0, -1.0, 0.0 ) );
    vertices->push_back( osg::Vec3(  1.0,  1.0, 0.0 ) );
    
    //positions->push_back( osg::Vec3( 1.0, 1.0, 500.0 ) );

    geometry->setVertexArray( vertices.get() );
    geometry->setTexCoordArray( 0, positions.get() );

    geometry->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, vertices->size() ) );
    
    m_coronas->addDrawable( geometry.get() );
    //m_pluginDCS->addChild( m_coronas.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Scene::CreateNodes()
{
    //Pointer to set StateSets for the nodes
    osg::ref_ptr< osg::StateSet > stateset;

    //Set up the collision detection nodes for the room
    osg::ref_ptr< osg::Group > roomPhysics = new osg::Group();
    m_room = new ves::xplorer::scenegraph::CADEntity( roomPhysics.get(),
                                                      m_pluginDCS.get(),
                                                      mPhysicsSimulator );

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
        roomPhysics->addChild( m_walls.get() );
        m_whiteDucts = osgDB::readNodeFile( "./Models/IVEs/Room/WhiteDucts.ive" );
        m_room->GetDCS()->addChild( m_whiteDucts.get() );
        m_whitePipes = osgDB::readNodeFile( "./Models/IVEs/Room/WhitePipes.ive" );
        m_room->GetDCS()->addChild( m_whitePipes.get() );
        m_yellow = osgDB::readNodeFile( "./Models/IVEs/Room/Yellow.ive" );
        m_room->GetDCS()->addChild( m_yellow.get() );

        //Set up material properties for the room geometry
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
        floorMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.4f, 1.0f ) );
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
        lightsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
        lightsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
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
        wallsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 3.0f, 3.0f, 3.0f, 1.0f ) );
        wallsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.4f, 1.0f ) );
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

    m_blowerComponents = osgDB::readNodeFile( "./Models/IVEs/BlowerComponents.ive" );
    m_pluginDCS->addChild( m_blowerComponents.get() );
    m_brackets = osgDB::readNodeFile( "./Models/IVEs/Brackets.ive" );
    m_pluginDCS->addChild( m_brackets.get() );
    m_cableTray = osgDB::readNodeFile( "./Models/IVEs/CableTray.ive" );
    m_pluginDCS->addChild( m_cableTray.get() );
    m_cementBase = osgDB::readNodeFile( "./Models/IVEs/CementBase.ive" );
    m_pluginDCS->addChild( m_cementBase.get() );
    //m_combustorInternals = osgDB::readNodeFile( "./Models/IVEs/CombustorInternals.ive" );
    //m_pluginDCS->addChild( m_combustorInternals.get() );
    m_combustorPiping = osgDB::readNodeFile( "./Models/IVEs/CombustorPiping.ive" );
    m_pluginDCS->addChild( m_combustorPiping.get() );
    m_compressorInlet = osgDB::readNodeFile( "./Models/IVEs/CompressorInlet.ive" );
    m_pluginDCS->addChild( m_compressorInlet.get() );
    m_frame = osgDB::readNodeFile( "./Models/IVEs/Frame.ive" );
    m_pluginDCS->addChild( m_frame.get() );
    m_groundBolts = osgDB::readNodeFile( "./Models/IVEs/GroundBolts.ive" );
    m_pluginDCS->addChild( m_groundBolts.get() );
    m_heatExchanger = osgDB::readNodeFile( "./Models/IVEs/HeatExchanger.ive" );
    m_pluginDCS->addChild( m_heatExchanger.get() );
    m_heatExchangerSweep = osgDB::readNodeFile( "./Models/IVEs/HeatExchangerSweep.ive" );
    m_pluginDCS->addChild( m_heatExchangerSweep.get() );
    m_instrumentation = osgDB::readNodeFile( "./Models/IVEs/Instrumentation.ive" );
    m_pluginDCS->addChild( m_instrumentation.get() );
    m_load = osgDB::readNodeFile( "./Models/IVEs/Load.ive" );
    m_pluginDCS->addChild( m_load.get() );
    m_plenumPiping = osgDB::readNodeFile( "./Models/IVEs/PlenumPiping.ive" );
    m_pluginDCS->addChild( m_plenumPiping.get() );
    m_plenumSystem = osgDB::readNodeFile( "./Models/IVEs/PlenumSystem.ive" );
    m_pluginDCS->addChild( m_plenumSystem.get() );
    m_railing = osgDB::readNodeFile( "./Models/IVEs/Railing.ive" );
    m_pluginDCS->addChild( m_railing.get() );
    m_reliefPiping = osgDB::readNodeFile( "./Models/IVEs/ReliefPiping.ive" );
    m_pluginDCS->addChild( m_reliefPiping.get() );
    m_reliefPipingAM = osgDB::readNodeFile( "./Models/IVEs/ReliefPipingAM.ive" );
    m_pluginDCS->addChild( m_reliefPipingAM.get() );
    m_shell = osgDB::readNodeFile( "./Models/IVEs/Shell.ive" );
    m_pluginDCS->addChild( m_shell.get() );
    m_stack = osgDB::readNodeFile( "./Models/IVEs/Stack.ive" );
    m_pluginDCS->addChild( m_stack.get() );
    m_turbineExhaust = osgDB::readNodeFile( "./Models/IVEs/TurbineExhaust.ive" );
    m_pluginDCS->addChild( m_turbineExhaust.get() );
    m_turbinePostCombustor = osgDB::readNodeFile( "./Models/IVEs/TurbinePostCombustor.ive" );
    m_pluginDCS->addChild( m_turbinePostCombustor.get() );
    m_turbineSupport = osgDB::readNodeFile( "./Models/IVEs/TurbineSupport.ive" );
    m_pluginDCS->addChild( m_turbineSupport.get() );

    osg::ref_ptr< osg::Material > blowerComponentsMaterial = new osg::Material();
    blowerComponentsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    blowerComponentsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    blowerComponentsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    blowerComponentsMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    blowerComponentsMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_blowerComponents->getOrCreateStateSet();
    stateset->setAttributeAndModes( blowerComponentsMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > bracketsMaterial = new osg::Material();
    bracketsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    bracketsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    bracketsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    bracketsMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    bracketsMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_brackets->getOrCreateStateSet();
    stateset->setAttributeAndModes( bracketsMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > cableTrayMaterial = new osg::Material();
    cableTrayMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    cableTrayMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    cableTrayMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    cableTrayMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    cableTrayMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_cableTray->getOrCreateStateSet();
    stateset->setAttributeAndModes( cableTrayMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > cementBaseMaterial = new osg::Material();
    cementBaseMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    cementBaseMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    cementBaseMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    cementBaseMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    cementBaseMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_cementBase->getOrCreateStateSet();
    stateset->setAttributeAndModes( cementBaseMaterial.get(), osg::StateAttribute::ON );

    /*
    osg::ref_ptr< osg::Material > combustorInternalsMaterial = new osg::Material();
    combustorInternalsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    combustorInternalsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    combustorInternalsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    combustorInternalsMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    combustorInternalsMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_combustorInternals->getOrCreateStateSet();
    stateset->setAttributeAndModes( combustorInternalsMaterial.get(), osg::StateAttribute::ON );
    */

    osg::ref_ptr< osg::Material > combustorPipingMaterial = new osg::Material();
    combustorPipingMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    combustorPipingMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    combustorPipingMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    combustorPipingMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    combustorPipingMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_combustorPiping->getOrCreateStateSet();
    stateset->setAttributeAndModes( combustorPipingMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > compressorInletMaterial = new osg::Material();
    compressorInletMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    compressorInletMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    compressorInletMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    compressorInletMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    compressorInletMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_compressorInlet->getOrCreateStateSet();
    stateset->setAttributeAndModes( compressorInletMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > frameMaterial = new osg::Material();
    frameMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    frameMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.15f, 0.15f, 0.15f, 1.0f ) );
    frameMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.15f, 0.15f, 0.15f, 1.0f ) );
    frameMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    frameMaterial->setShininess( osg::Material::FRONT, 12.0f );
    stateset = m_frame->getOrCreateStateSet();
    stateset->setAttributeAndModes( frameMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > groundBoltsMaterial = new osg::Material();
    groundBoltsMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    groundBoltsMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
    groundBoltsMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
    groundBoltsMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    groundBoltsMaterial->setShininess( osg::Material::FRONT, 12.0f );
    stateset = m_groundBolts->getOrCreateStateSet();
    stateset->setAttributeAndModes( groundBoltsMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > heatExchangerMaterial = new osg::Material();
    heatExchangerMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    heatExchangerMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    heatExchangerMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    heatExchangerMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    heatExchangerMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_heatExchanger->getOrCreateStateSet();
    stateset->setAttributeAndModes( heatExchangerMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > heatExchangerSweepMaterial = new osg::Material();
    heatExchangerSweepMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    heatExchangerSweepMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    heatExchangerSweepMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    heatExchangerSweepMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    heatExchangerSweepMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_heatExchangerSweep->getOrCreateStateSet();
    stateset->setAttributeAndModes( heatExchangerSweepMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > instrumentationMaterial = new osg::Material();
    instrumentationMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    instrumentationMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    instrumentationMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    instrumentationMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    instrumentationMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_instrumentation->getOrCreateStateSet();
    stateset->setAttributeAndModes( instrumentationMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > loadMaterial = new osg::Material();
    loadMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    loadMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    loadMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    loadMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    loadMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_load->getOrCreateStateSet();
    stateset->setAttributeAndModes( loadMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > plenumPipingMaterial = new osg::Material();
    plenumPipingMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    plenumPipingMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    plenumPipingMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    plenumPipingMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    plenumPipingMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_plenumPiping->getOrCreateStateSet();
    stateset->setAttributeAndModes( plenumPipingMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > plenumSystemMaterial = new osg::Material();
    plenumSystemMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    plenumSystemMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    plenumSystemMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    plenumSystemMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    plenumSystemMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_plenumSystem->getOrCreateStateSet();
    stateset->setAttributeAndModes( plenumSystemMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > railingMaterial = new osg::Material();
    railingMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    railingMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.3f, 1.0f ) );
    railingMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 1.0f, 0.9f, 0.2f, 1.0f ) );
    railingMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    railingMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_railing->getOrCreateStateSet();
    stateset->setAttributeAndModes( railingMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > reliefPipingMaterial = new osg::Material();
    reliefPipingMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    reliefPipingMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    reliefPipingMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    reliefPipingMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    reliefPipingMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_reliefPiping->getOrCreateStateSet();
    stateset->setAttributeAndModes( reliefPipingMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > reliefPipingAMMaterial = new osg::Material();
    reliefPipingAMMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    reliefPipingAMMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    reliefPipingAMMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    reliefPipingAMMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    reliefPipingAMMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_reliefPipingAM->getOrCreateStateSet();
    stateset->setAttributeAndModes( reliefPipingAMMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > shellMaterial = new osg::Material();
    shellMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    shellMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    shellMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    shellMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    shellMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_shell->getOrCreateStateSet();
    stateset->setAttributeAndModes( shellMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > stackMaterial = new osg::Material();
    stackMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    stackMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    stackMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    stackMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    stackMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_stack->getOrCreateStateSet();
    stateset->setAttributeAndModes( stackMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > turbineExhaustMaterial = new osg::Material();
    turbineExhaustMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    turbineExhaustMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    turbineExhaustMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    turbineExhaustMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    turbineExhaustMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_turbineExhaust->getOrCreateStateSet();
    stateset->setAttributeAndModes( turbineExhaustMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > turbinePostCombustorMaterial = new osg::Material();
    turbinePostCombustorMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    turbinePostCombustorMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    turbinePostCombustorMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    turbinePostCombustorMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    turbinePostCombustorMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_turbinePostCombustor->getOrCreateStateSet();
    stateset->setAttributeAndModes( turbinePostCombustorMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > turbineSupportMaterial = new osg::Material();
    turbineSupportMaterial->setEmission( osg::Material::FRONT, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    turbineSupportMaterial->setAmbient( osg::Material::FRONT, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    turbineSupportMaterial->setDiffuse( osg::Material::FRONT, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    turbineSupportMaterial->setSpecular( osg::Material::FRONT, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    turbineSupportMaterial->setShininess( osg::Material::FRONT, 10.0f );
    stateset = m_turbineSupport->getOrCreateStateSet();
    stateset->setAttributeAndModes( turbineSupportMaterial.get(), osg::StateAttribute::ON );

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

    m_shadowedScene->addChild( m_blowerComponents.get() );
    //m_shadowedScene->addChild( m_brackets.get() );
    m_shadowedScene->addChild( m_cableTray.get() );
    m_shadowedScene->addChild( m_cementBase.get() );
    //m_shadowedScene->addChild( m_combustorInternals.get() );
    m_shadowedScene->addChild( m_combustorPiping.get() );
    m_shadowedScene->addChild( m_compressorInlet.get() );
    m_shadowedScene->addChild( m_frame.get() );
    //m_shadowedScene->addChild( m_groundBolts.get() );
    m_shadowedScene->addChild( m_heatExchanger.get() );
    m_shadowedScene->addChild( m_heatExchangerSweep.get() );
    m_shadowedScene->addChild( m_instrumentation.get() );
    m_shadowedScene->addChild( m_load.get() );
    m_shadowedScene->addChild( m_plenumPiping.get() );
    m_shadowedScene->addChild( m_plenumSystem.get() );
    m_shadowedScene->addChild( m_railing.get() );
    m_shadowedScene->addChild( m_reliefPiping.get() );
    m_shadowedScene->addChild( m_reliefPipingAM.get() );
    m_shadowedScene->addChild( m_shell.get() );
    m_shadowedScene->addChild( m_turbineExhaust.get() );
    m_shadowedScene->addChild( m_turbinePostCombustor.get() );
    m_shadowedScene->addChild( m_turbineSupport.get() );
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

        float factor = 2.0f;
        float units = 4.0f;

        osg::ref_ptr< osg::PolygonOffset > polygonOffset = new osg::PolygonOffset();
        polygonOffset->setFactor( factor );
        polygonOffset->setUnits( units );
        localStateset->setAttribute( polygonOffset.get(), osg::StateAttribute::ON );
        localStateset->setMode( GL_POLYGON_OFFSET_FILL, osg::StateAttribute::ON );

        osg::ref_ptr< osg::CullFace > cullFace = new osg::CullFace();
        cullFace->setMode( osg::CullFace::FRONT );
        localStateset->setAttribute( cullFace.get(), osg::StateAttribute::ON );
        localStateset->setMode( GL_CULL_FACE, osg::StateAttribute::ON );

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
    ss->setTextureAttributeAndModes( (int)_textureUnit + 1, m_jitter, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );
    ss->setTextureMode( (int)_textureUnit + 1, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
    ss->setTextureMode( (int)_textureUnit + 1, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
    ss->setTextureMode( (int)_textureUnit + 1, GL_TEXTURE_GEN_R, osg::StateAttribute::ON );
    */
}
////////////////////////////////////////////////////////////////////////////////
