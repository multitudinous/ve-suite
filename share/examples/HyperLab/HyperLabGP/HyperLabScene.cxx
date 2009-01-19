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
#include "Shaders.h"
#include "HyperLabScene.h"

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
HyperLabScene::HyperLabScene(
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator )
    :
    mPluginDCS( pluginDCS ),
    mPhysicsSimulator( physicsSimulator ),

    shader( new hyperlab::Shaders() ),

    mLight( 0 ),
    mLightSource( 0 ),
    mLightTransform( 0 ),
    
    mCamera( 0 ),
    mShadow( 0 ),
    mTexgenNode( 0 ),
    mShadowedScene( 0 ),

    mRoom( 0 ),
    mAluminumParts( 0 ),
    mAluminumPipes( 0 ),
    mBlack( 0 ),
    mBrown( 0 ),
    mCeiling( 0 ),
    mCoronas( 0 ),
    mDetails( 0 ),
    mFloor( 0 ),
    mGlass( 0 ),
    mLights( 0 ),
    mLtGreen( 0 ),
    mLtGrey( 0 ),
    mOrange( 0 ),
    mRed( 0 ),
    mRedBrown( 0 ),
    mWalls( 0 ),
    mWhiteDucts( 0 ),
    mWhitePipes( 0 ),
    mYellow( 0 ),

    mBlowerComponents( 0 ),
    mBrackets( 0 ),
    mCableTray( 0 ),
    mCementBase( 0 ),
    mCombustorInternals( 0 ),
    mCombustorPiping( 0 ),
    mCompressorInlet( 0 ),
    mFrame( 0 ),
    mGroundBolts( 0 ),
    mHeatExchanger( 0 ),
    mHeatExchangerSweep( 0 ),
    mInstrumentation( 0 ),
    mLoad( 0 ),
    mPlenumPiping( 0 ),
    mPlenumSystem( 0 ),
    mRailing( 0 ),
    mReliefPiping( 0 ),
    mReliefPipingAM( 0 ),
    mShell( 0 ),
    mStack( 0 ),
    mTurbineExhaust( 0 ),
    mTurbinePostCombustor( 0 ),
    mTurbineSupport( 0 )
{
    InitializeScene();

    AdvancedVisuals();
}
////////////////////////////////////////////////////////////////////////////////
HyperLabScene::~HyperLabScene()
{
    if( mRoom )
    {
        delete mRoom;
    }

    if( shader )
    {
        delete shader;
    }
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabScene::DefaultVisuals()
{
    shader->SetOptions( mAluminumParts.get(), false, true );
    shader->SetOptions( mAluminumPipes.get(), false, true );
    shader->SetOptions( mBlack.get(), false, true );
    shader->SetOptions( mBrown.get(), false, true );
    shader->SetOptions( mCeiling.get(), false, false, "WallMap" );
    shader->Lights( mCoronas.get() );
    shader->SetOptions( mDetails.get(), false, false, "Decoration" );
    shader->SetOptions( mFloor.get(), false, false, "WallMap" );
    shader->SetOptions( mGlass.get(), false, true );
    shader->SetOptions( mLights.get(), false, true );
    shader->SetOptions( mLtGreen.get(), false, true );
    shader->SetOptions( mLtGrey.get(), false, true );
    shader->SetOptions( mOrange.get(), false, true );
    shader->SetOptions( mRed.get(), false, true );
    shader->SetOptions( mRedBrown.get(), false, true );
    shader->SetOptions( mWalls.get(), false, true, "WallMap" );
    shader->SetOptions( mWhiteDucts.get(), false, true );
    shader->SetOptions( mWhitePipes.get(), false, true );
    shader->SetOptions( mYellow.get(), false, true );
    
    shader->SetOptions( mBlowerComponents.get(), false, true );
    shader->SetOptions( mBrackets.get(), false, true );
    shader->SetOptions( mCableTray.get(), false, true );
    shader->SetOptions( mCementBase.get(), false, true );
    //shader->SetOptions( mCombustorInternals.get(), false, true );
    shader->SetOptions( mCombustorPiping.get(), false, true );
    shader->SetOptions( mCompressorInlet.get(), false, true );
    shader->SetOptions( mFrame.get(), false, true );
    shader->SetOptions( mGroundBolts.get(), false, true );
    shader->SetOptions( mHeatExchanger.get(), false, true );
    shader->SetOptions( mHeatExchangerSweep.get(), false, true );
    shader->SetOptions( mInstrumentation.get(), false, true );
    shader->SetOptions( mLoad.get(), false, true );
    shader->SetOptions( mPlenumPiping.get(), false, true );
    shader->SetOptions( mPlenumSystem.get(), false, true );
    shader->SetOptions( mRailing.get(), false, true );
    shader->SetOptions( mReliefPiping.get(), false, true );
    shader->SetOptions( mReliefPipingAM.get(), false, true );
    shader->SetOptions( mShell.get(), false, true );
    shader->SetOptions( mStack.get(), false, true );
    shader->SetOptions( mTurbineExhaust.get(), false, true );
    shader->SetOptions( mTurbinePostCombustor.get(), false, true );
    shader->SetOptions( mTurbineSupport.get(), false, true );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabScene::AdvancedVisuals()
{
    float reflectionPercentage;

    shader->SetOptions( mAluminumParts.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mAluminumPipes.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mBlack.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mBrown.get(), false, true, "",
                        NULL, mShadow );
    shader->Lights( mCoronas.get() );
    shader->SetOptions( mCeiling.get(), false, false, "WallMap" );
    shader->SetOptions( mDetails.get(), false, false, "Decoration",
                        NULL, mShadow );
    shader->SetOptions( mFloor.get(), false, false, "WallMap",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mGlass.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mLights.get(), false, true );
    shader->SetOptions( mLtGreen.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mLtGrey.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mOrange.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mRed.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mRedBrown.get(), false, true );
    shader->SetOptions( mWalls.get(), false, false, "WallMap",
                        NULL, mShadow );
    shader->SetOptions( mWhiteDucts.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mWhitePipes.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mYellow.get(), false, true, "",
                        NULL, mShadow );

    shader->SetOptions( mBlowerComponents.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mBrackets.get(), false, true );
    shader->SetOptions( mCableTray.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mCementBase.get(), false, true, "",
                        NULL, mShadow );
    //shader->SetOptions( mCombustorInternals.get(), false, true, "",
                        //NULL, NULL );
    shader->SetOptions( mCombustorPiping.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mCompressorInlet.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mFrame.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mGroundBolts.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mHeatExchanger.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mHeatExchangerSweep.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mInstrumentation.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mLoad.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mPlenumPiping.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mPlenumSystem.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mRailing.get(), false, true, "",
                        NULL, mShadow );
    shader->SetOptions( mReliefPiping.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mReliefPipingAM.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mShell.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mStack.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mTurbineExhaust.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mTurbinePostCombustor.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
    shader->SetOptions( mTurbineSupport.get(), false, true, "",
                        &( reflectionPercentage = 0.01 ), mShadow );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabScene::XRay()
{
    shader->SetOptions( mAluminumParts.get(), true );
    shader->SetOptions( mAluminumPipes.get(), true );
    shader->SetOptions( mBlack.get(), true );
    shader->SetOptions( mBrown.get(), true );
    shader->SetOptions( mCeiling.get(), true );
    //mCoronas->setNodeMask( 0 );
    shader->SetOptions( mDetails.get(), true );
    shader->SetOptions( mFloor.get(), true );
    shader->SetOptions( mGlass.get(), true );
    shader->SetOptions( mLights.get(), true );
    shader->SetOptions( mLtGreen.get(), true );
    shader->SetOptions( mLtGrey.get(), true );
    shader->SetOptions( mOrange.get(), true );
    shader->SetOptions( mRed.get(), true );
    shader->SetOptions( mRedBrown.get(), true );
    shader->SetOptions( mWalls.get(), true );
    shader->SetOptions( mWhiteDucts.get(), true );
    shader->SetOptions( mWhitePipes.get(), true );
    shader->SetOptions( mYellow.get(), true );

    shader->SetOptions( mBlowerComponents.get(), true );
    shader->SetOptions( mBrackets.get(), true );
    shader->SetOptions( mCableTray.get(), true );
    shader->SetOptions( mCementBase.get(), true );
    //shader->SetOptions( mCombustorInternals.get(), true );
    shader->SetOptions( mCombustorPiping.get(), true );
    shader->SetOptions( mCompressorInlet.get(), true );
    shader->SetOptions( mFrame.get(), true );
    shader->SetOptions( mGroundBolts.get(), true );
    shader->SetOptions( mHeatExchanger.get(), true );
    shader->SetOptions( mHeatExchangerSweep.get(), true );
    shader->SetOptions( mInstrumentation.get(), true );
    shader->SetOptions( mLoad.get(), true );
    shader->SetOptions( mPlenumPiping.get(), true );
    shader->SetOptions( mPlenumSystem.get(), true );
    shader->SetOptions( mRailing.get(), true );
    shader->SetOptions( mReliefPiping.get(), true );
    shader->SetOptions( mReliefPipingAM.get(), true );
    shader->SetOptions( mShell.get(), true );
    shader->SetOptions( mStack.get(), true );
    shader->SetOptions( mTurbineExhaust.get(), true );
    shader->SetOptions( mTurbinePostCombustor.get(), true );
    shader->SetOptions( mTurbineSupport.get(), true );
}
////////////////////////////////////////////////////////////////////////////////
osg::Light* HyperLabScene::GetLight()
{
    return mLight.get();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabScene::InitializeScene()
{
    CreateLights();
    CreateNodes();
    CreateShadowTexture();
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabScene::CreateLights()
{
    mLight = new osg::Light();
    mLight->setLightNum( 1 );
    mLight->setPosition( osg::Vec4( 0.0f, 0.0f, 5000.0f, 0.0f ) );

    mLightSource = new osg::LightSource();
    mLightSource->setLight( mLight.get() );
    mLightSource->setLocalStateSetModes( osg::StateAttribute::ON );

    mLightTransform = new osg::MatrixTransform();
    mLightTransform->setMatrix(
        osg::Matrix::translate( osg::Vec3( 0.0f, 0.0f, 5000.0f ) ) );
    mLightTransform->addChild( mLightSource.get() );

    mPluginDCS->addChild( mLightTransform.get() );

    //Set light defaults
    mLight->setAmbient( osg::Vec4( 0.63f, 0.63f, 0.40f, 1.0f ) );
    mLight->setDiffuse( osg::Vec4( 0.90f, 0.90f, 0.45f, 1.0f ) );
    mLight->setSpecular( osg::Vec4( 0.78f, 0.78f, 0.5f, 1.0f ) );

    //Add in the corona quads for added effets
    mCoronas = new osg::Geode();
    mCoronas->setCullingActive( false );
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

    geometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, vertices->size() ) );
    
    //mCoronas->addDrawable( geometry.get() );
    //mPluginDCS->addChild( mCoronas.get() );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabScene::CreateNodes()
{
    //Pointer to set StateSets for the nodes
    osg::ref_ptr< osg::StateSet > stateset;

    //Set up the collision detection nodes for the room
    osg::ref_ptr< osg::Group > roomPhysics = new osg::Group();
    mRoom = new ves::xplorer::scenegraph::CADEntity( roomPhysics.get(),
                                                      mPluginDCS.get(),
                                                      mPhysicsSimulator );

    //Load in the geometry for the room
    {
        mAluminumParts =
            osgDB::readNodeFile( "./Models/IVEs/Room/AluminumParts.ive" );
        mRoom->GetDCS()->addChild( mAluminumParts.get() );
        mAluminumPipes =
            osgDB::readNodeFile( "./Models/IVEs/Room/AluminumPipes.ive" );
        mRoom->GetDCS()->addChild( mAluminumPipes.get() );
        mBlack =
            osgDB::readNodeFile( "./Models/IVEs/Room/Black.ive" );
        mRoom->GetDCS()->addChild( mBlack.get() );
        mBrown =
            osgDB::readNodeFile( "./Models/IVEs/Room/Brown.ive" );
        mRoom->GetDCS()->addChild( mBrown.get() );
        mCeiling =
            osgDB::readNodeFile( "./Models/IVEs/Room/Ceiling.ive" );
        mRoom->GetDCS()->addChild( mCeiling.get() );
        mDetails =
            osgDB::readNodeFile( "./Models/IVEs/Room/Details.ive" );
        mRoom->GetDCS()->addChild( mDetails.get() );
        mFloor =
            osgDB::readNodeFile( "./Models/IVEs/Room/Floor.ive" );
        mRoom->GetDCS()->addChild( mFloor.get() );
        mGlass =
            osgDB::readNodeFile( "./Models/IVEs/Room/Glass.ive" );
        mRoom->GetDCS()->addChild( mGlass.get() );
        mLights =
            osgDB::readNodeFile( "./Models/IVEs/Room/Lights.ive" );
        mRoom->GetDCS()->addChild( mLights.get() );
        mLtGreen =
            osgDB::readNodeFile( "./Models/IVEs/Room/LtGreen.ive" );
        mRoom->GetDCS()->addChild( mLtGreen.get() );
        mLtGrey =
            osgDB::readNodeFile( "./Models/IVEs/Room/LtGrey.ive" );
        mRoom->GetDCS()->addChild( mLtGrey.get() );
        mOrange =
            osgDB::readNodeFile( "./Models/IVEs/Room/Orange.ive" );
        mRoom->GetDCS()->addChild( mOrange.get() );
        mRed =
            osgDB::readNodeFile( "./Models/IVEs/Room/Red.ive" );
        mRoom->GetDCS()->addChild( mRed.get() );
        mRedBrown =
            osgDB::readNodeFile( "./Models/IVEs/Room/RedBrown.ive" );
        mRoom->GetDCS()->addChild( mRedBrown.get() );
        mWalls =
            osgDB::readNodeFile( "./Models/IVEs/Room/Walls.ive" );
        mRoom->GetDCS()->addChild( mWalls.get() );
        mWhiteDucts =
            osgDB::readNodeFile( "./Models/IVEs/Room/WhiteDucts.ive" );
        mRoom->GetDCS()->addChild( mWhiteDucts.get() );
        mWhitePipes =
            osgDB::readNodeFile( "./Models/IVEs/Room/WhitePipes.ive" );
        mRoom->GetDCS()->addChild( mWhitePipes.get() );
        mYellow =
            osgDB::readNodeFile( "./Models/IVEs/Room/Yellow.ive" );
        mRoom->GetDCS()->addChild( mYellow.get() );

        //Set up material properties for the room geometry
        osg::ref_ptr< osg::Material > aluminumPartsMaterial =
            new osg::Material();
        aluminumPartsMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        aluminumPartsMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        aluminumPartsMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.4f, 0.4f, 0.6f, 1.0f ) );
        aluminumPartsMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        aluminumPartsMaterial->setShininess( osg::Material::FRONT_AND_BACK, 5.0f );
        stateset = mAluminumParts->getOrCreateStateSet();
        stateset->setAttributeAndModes( aluminumPartsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > aluminumPipesMaterial = new osg::Material();
        aluminumPipesMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        aluminumPipesMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        aluminumPipesMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.4f, 0.4f, 0.6f, 1.0f ) );
        aluminumPipesMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        aluminumPipesMaterial->setShininess( osg::Material::FRONT_AND_BACK, 5.0f );
        stateset = mAluminumPipes->getOrCreateStateSet();
        stateset->setAttributeAndModes( aluminumPipesMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > blackMaterial = new osg::Material();
        blackMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        blackMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        blackMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        blackMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        blackMaterial->setShininess( osg::Material::FRONT_AND_BACK, 15.0f );
        stateset = mBlack->getOrCreateStateSet();
        stateset->setAttributeAndModes( blackMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > brownMaterial = new osg::Material();
        brownMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        brownMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.2f, 0.1f, 0.05f, 1.0f ) );
        brownMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.3f, 0.15f, 1.0f ) );
        brownMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        brownMaterial->setShininess( osg::Material::FRONT_AND_BACK, 15.0f );
        stateset = mBrown->getOrCreateStateSet();
        stateset->setAttributeAndModes( brownMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > ceilingMaterial = new osg::Material();
        ceilingMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        ceilingMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.3f, 0.3f, 0.3f, 1.0f ) );
        ceilingMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.3f, 0.3f, 0.3f, 1.0f ) );
        ceilingMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
        ceilingMaterial->setShininess( osg::Material::FRONT_AND_BACK, 15.0f );
        stateset = mCeiling->getOrCreateStateSet();
        stateset->setAttributeAndModes( ceilingMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > detailsMaterial = new osg::Material();
        detailsMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 2.0f, 2.0f, 2.0f, 1.0f ) );
        detailsMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.8f, 0.8f, 0.8f, 1.0f ) );
        detailsMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
        detailsMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
        detailsMaterial->setShininess( osg::Material::FRONT_AND_BACK, 15.0f );
        stateset = mDetails->getOrCreateStateSet();
        stateset->setAttributeAndModes( detailsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > floorMaterial = new osg::Material();
        floorMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.8f, 1.8f, 1.8f, 1.0f ) );
        floorMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.4f, 1.0f ) );
        floorMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.02f, 0.02f, 0.01f, 1.0f ) );
        floorMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.02f, 0.02f, 0.01f, 1.0f ) );
        floorMaterial->setShininess( osg::Material::FRONT_AND_BACK, 5.0f );
        stateset = mFloor->getOrCreateStateSet();
        stateset->setAttributeAndModes( floorMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > glassMaterial = new osg::Material();
        glassMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        glassMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        glassMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        glassMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        glassMaterial->setShininess( osg::Material::FRONT_AND_BACK, 15.0f );
        stateset = mGlass->getOrCreateStateSet();
        stateset->setAttributeAndModes( glassMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > lightsMaterial = new osg::Material();
        lightsMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        lightsMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
        lightsMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
        lightsMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        lightsMaterial->setShininess( osg::Material::FRONT_AND_BACK, 15.0f );
        stateset = mLights->getOrCreateStateSet();
        stateset->setAttributeAndModes( lightsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > ltGreenMaterial = new osg::Material();
        ltGreenMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        ltGreenMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.65f, 0.5f, 1.0f ) );
        ltGreenMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.8f, 0.5f, 1.0f ) );
        ltGreenMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        ltGreenMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
        stateset = mLtGreen->getOrCreateStateSet();
        stateset->setAttributeAndModes( ltGreenMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > ltGreyMaterial = new osg::Material();
        ltGreyMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        ltGreyMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        ltGreyMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        ltGreyMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.2f, 0.2f, 0.2f, 1.0f ) );
        ltGreyMaterial->setShininess( osg::Material::FRONT_AND_BACK, 5.0f );
        stateset = mLtGrey->getOrCreateStateSet();
        stateset->setAttributeAndModes( ltGreyMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > orangeMaterial = new osg::Material();
        orangeMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        orangeMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.1f, 0.1f, 1.0f ) );
        orangeMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 0.65f, 0.3f, 1.0f ) );
        orangeMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        orangeMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
        stateset = mOrange->getOrCreateStateSet();
        stateset->setAttributeAndModes( orangeMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > redMaterial = new osg::Material();
        redMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        redMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        redMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        redMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        redMaterial->setShininess( osg::Material::FRONT_AND_BACK, 15.0f );
        stateset = mRed->getOrCreateStateSet();
        stateset->setAttributeAndModes( redMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > redBrownMaterial = new osg::Material();
        redBrownMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        redBrownMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.3f, 0.25f, 0.2f, 1.0f ) );
        redBrownMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.14f, 0.07f, 0.0f, 1.0f ) );
        redBrownMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.01f, 0.01f, 0.01f, 1.0f ) );
        redBrownMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
        stateset = mRedBrown->getOrCreateStateSet();
        stateset->setAttributeAndModes( redBrownMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > wallsMaterial = new osg::Material();
        wallsMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 3.0f, 3.0f, 3.0f, 1.0f ) );
        wallsMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.4f, 1.0f ) );
        wallsMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.02f, 0.02f, 0.01f, 1.0f ) );
        wallsMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.01f, 0.01f, 0.01f, 1.0f ) );
        wallsMaterial->setShininess( osg::Material::FRONT_AND_BACK, 5.0f );
        stateset = mWalls->getOrCreateStateSet();
        stateset->setAttributeAndModes( wallsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > whiteDuctsMaterial = new osg::Material();
        whiteDuctsMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        whiteDuctsMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        whiteDuctsMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.9f, 0.9f, 0.95f, 1.0f ) );
        whiteDuctsMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        whiteDuctsMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
        stateset = mWhiteDucts->getOrCreateStateSet();
        stateset->setAttributeAndModes( whiteDuctsMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > whitePipesMaterial = new osg::Material();
        whitePipesMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        whitePipesMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
        whitePipesMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.9f, 0.9f, 0.95f, 1.0f ) );
        whitePipesMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        whitePipesMaterial->setShininess( osg::Material::FRONT_AND_BACK, 15.0f );
        stateset = mWhitePipes->getOrCreateStateSet();
        stateset->setAttributeAndModes( whitePipesMaterial.get(), osg::StateAttribute::ON );

        osg::ref_ptr< osg::Material > yellowMaterial = new osg::Material();
        yellowMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        yellowMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.3f, 1.0f ) );
        yellowMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 0.85f, 0.3f, 1.0f ) );
        yellowMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
        yellowMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
        stateset = mYellow->getOrCreateStateSet();
        stateset->setAttributeAndModes( yellowMaterial.get(), osg::StateAttribute::ON );
    }

    mBlowerComponents =
        osgDB::readNodeFile( "./Models/IVEs/BlowerComponents.ive" );
    mPluginDCS->addChild( mBlowerComponents.get() );
    mBrackets =
        osgDB::readNodeFile( "./Models/IVEs/Brackets.ive" );
    mPluginDCS->addChild( mBrackets.get() );
    mCableTray =
        osgDB::readNodeFile( "./Models/IVEs/CableTray.ive" );
    mPluginDCS->addChild( mCableTray.get() );
    mCementBase =
        osgDB::readNodeFile( "./Models/IVEs/CementBase.ive" );
    mPluginDCS->addChild( mCementBase.get() );
    //mCombustorInternals =
        //osgDB::readNodeFile( "./Models/IVEs/CombustorInternals.ive" );
    //mPluginDCS->addChild( mCombustorInternals.get() );
    mCombustorPiping =
        osgDB::readNodeFile( "./Models/IVEs/CombustorPiping.ive" );
    mPluginDCS->addChild( mCombustorPiping.get() );
    mCompressorInlet =
        osgDB::readNodeFile( "./Models/IVEs/CompressorInlet.ive" );
    mPluginDCS->addChild( mCompressorInlet.get() );
    mFrame =
        osgDB::readNodeFile( "./Models/IVEs/Frame.ive" );
    mPluginDCS->addChild( mFrame.get() );
    mGroundBolts =
        osgDB::readNodeFile( "./Models/IVEs/GroundBolts.ive" );
    mPluginDCS->addChild( mGroundBolts.get() );
    mHeatExchanger =
        osgDB::readNodeFile( "./Models/IVEs/HeatExchanger.ive" );
    mPluginDCS->addChild( mHeatExchanger.get() );
    mHeatExchangerSweep =
        osgDB::readNodeFile( "./Models/IVEs/HeatExchangerSweep.ive" );
    mPluginDCS->addChild( mHeatExchangerSweep.get() );
    mInstrumentation =
        osgDB::readNodeFile( "./Models/IVEs/Instrumentation.ive" );
    mPluginDCS->addChild( mInstrumentation.get() );
    mLoad =
        osgDB::readNodeFile( "./Models/IVEs/Load.ive" );
    mPluginDCS->addChild( mLoad.get() );
    mPlenumPiping =
        osgDB::readNodeFile( "./Models/IVEs/PlenumPiping.ive" );
    mPluginDCS->addChild( mPlenumPiping.get() );
    mPlenumSystem =
        osgDB::readNodeFile( "./Models/IVEs/PlenumSystem.ive" );
    mPluginDCS->addChild( mPlenumSystem.get() );
    mRailing =
        osgDB::readNodeFile( "./Models/IVEs/Railing.ive" );
    mPluginDCS->addChild( mRailing.get() );
    mReliefPiping =
        osgDB::readNodeFile( "./Models/IVEs/ReliefPiping.ive" );
    mPluginDCS->addChild( mReliefPiping.get() );
    mReliefPipingAM =
        osgDB::readNodeFile( "./Models/IVEs/ReliefPipingAM.ive" );
    mPluginDCS->addChild( mReliefPipingAM.get() );
    mShell =
        osgDB::readNodeFile( "./Models/IVEs/Shell.ive" );
    mPluginDCS->addChild( mShell.get() );
    mStack =
        osgDB::readNodeFile( "./Models/IVEs/Stack.ive" );
    mPluginDCS->addChild( mStack.get() );
    mTurbineExhaust =
        osgDB::readNodeFile( "./Models/IVEs/TurbineExhaust.ive" );
    mPluginDCS->addChild( mTurbineExhaust.get() );
    mTurbinePostCombustor =
        osgDB::readNodeFile( "./Models/IVEs/TurbinePostCombustor.ive" );
    mPluginDCS->addChild( mTurbinePostCombustor.get() );
    mTurbineSupport =
        osgDB::readNodeFile( "./Models/IVEs/TurbineSupport.ive" );
    mPluginDCS->addChild( mTurbineSupport.get() );

    osg::ref_ptr< osg::Material > blowerComponentsMaterial = new osg::Material();
    blowerComponentsMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    blowerComponentsMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    blowerComponentsMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    blowerComponentsMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    blowerComponentsMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mBlowerComponents->getOrCreateStateSet();
    stateset->setAttributeAndModes( blowerComponentsMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > bracketsMaterial = new osg::Material();
    bracketsMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    bracketsMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    bracketsMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    bracketsMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    bracketsMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mBrackets->getOrCreateStateSet();
    stateset->setAttributeAndModes( bracketsMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > cableTrayMaterial = new osg::Material();
    cableTrayMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    cableTrayMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    cableTrayMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    cableTrayMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    cableTrayMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mCableTray->getOrCreateStateSet();
    stateset->setAttributeAndModes( cableTrayMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > cementBaseMaterial = new osg::Material();
    cementBaseMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    cementBaseMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    cementBaseMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    cementBaseMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    cementBaseMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mCementBase->getOrCreateStateSet();
    stateset->setAttributeAndModes( cementBaseMaterial.get(), osg::StateAttribute::ON );

    //osg::ref_ptr< osg::Material > combustorInternalsMaterial = new osg::Material();
    //combustorInternalsMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    //combustorInternalsMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    //combustorInternalsMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    //combustorInternalsMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    //combustorInternalsMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    //stateset = mCombustorInternals->getOrCreateStateSet();
    //stateset->setAttributeAndModes( combustorInternalsMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > combustorPipingMaterial = new osg::Material();
    combustorPipingMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    combustorPipingMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    combustorPipingMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    combustorPipingMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    combustorPipingMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mCombustorPiping->getOrCreateStateSet();
    stateset->setAttributeAndModes( combustorPipingMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > compressorInletMaterial = new osg::Material();
    compressorInletMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    compressorInletMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    compressorInletMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    compressorInletMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    compressorInletMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mCompressorInlet->getOrCreateStateSet();
    stateset->setAttributeAndModes( compressorInletMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > frameMaterial = new osg::Material();
    frameMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    frameMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.15f, 0.15f, 0.15f, 1.0f ) );
    frameMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.15f, 0.15f, 0.15f, 1.0f ) );
    frameMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    frameMaterial->setShininess( osg::Material::FRONT_AND_BACK, 12.0f );
    stateset = mFrame->getOrCreateStateSet();
    stateset->setAttributeAndModes( frameMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > groundBoltsMaterial = new osg::Material();
    groundBoltsMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    groundBoltsMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
    groundBoltsMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
    groundBoltsMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    groundBoltsMaterial->setShininess( osg::Material::FRONT_AND_BACK, 12.0f );
    stateset = mGroundBolts->getOrCreateStateSet();
    stateset->setAttributeAndModes( groundBoltsMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > heatExchangerMaterial = new osg::Material();
    heatExchangerMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    heatExchangerMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    heatExchangerMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    heatExchangerMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    heatExchangerMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mHeatExchanger->getOrCreateStateSet();
    stateset->setAttributeAndModes( heatExchangerMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > heatExchangerSweepMaterial = new osg::Material();
    heatExchangerSweepMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    heatExchangerSweepMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    heatExchangerSweepMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    heatExchangerSweepMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    heatExchangerSweepMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mHeatExchangerSweep->getOrCreateStateSet();
    stateset->setAttributeAndModes( heatExchangerSweepMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > instrumentationMaterial = new osg::Material();
    instrumentationMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    instrumentationMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    instrumentationMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    instrumentationMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    instrumentationMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mInstrumentation->getOrCreateStateSet();
    stateset->setAttributeAndModes( instrumentationMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > loadMaterial = new osg::Material();
    loadMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    loadMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    loadMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    loadMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    loadMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mLoad->getOrCreateStateSet();
    stateset->setAttributeAndModes( loadMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > plenumPipingMaterial = new osg::Material();
    plenumPipingMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    plenumPipingMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    plenumPipingMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    plenumPipingMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    plenumPipingMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mPlenumPiping->getOrCreateStateSet();
    stateset->setAttributeAndModes( plenumPipingMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > plenumSystemMaterial = new osg::Material();
    plenumSystemMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    plenumSystemMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    plenumSystemMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    plenumSystemMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    plenumSystemMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mPlenumSystem->getOrCreateStateSet();
    stateset->setAttributeAndModes( plenumSystemMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > railingMaterial = new osg::Material();
    railingMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    railingMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.3f, 1.0f ) );
    railingMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 0.9f, 0.2f, 1.0f ) );
    railingMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    railingMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mRailing->getOrCreateStateSet();
    stateset->setAttributeAndModes( railingMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > reliefPipingMaterial = new osg::Material();
    reliefPipingMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    reliefPipingMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    reliefPipingMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    reliefPipingMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    reliefPipingMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mReliefPiping->getOrCreateStateSet();
    stateset->setAttributeAndModes( reliefPipingMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > reliefPipingAMMaterial = new osg::Material();
    reliefPipingAMMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    reliefPipingAMMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    reliefPipingAMMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    reliefPipingAMMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    reliefPipingAMMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mReliefPipingAM->getOrCreateStateSet();
    stateset->setAttributeAndModes( reliefPipingAMMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > shellMaterial = new osg::Material();
    shellMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    shellMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    shellMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    shellMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    shellMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mShell->getOrCreateStateSet();
    stateset->setAttributeAndModes( shellMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > stackMaterial = new osg::Material();
    stackMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    stackMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    stackMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    stackMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    stackMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mStack->getOrCreateStateSet();
    stateset->setAttributeAndModes( stackMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > turbineExhaustMaterial = new osg::Material();
    turbineExhaustMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    turbineExhaustMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    turbineExhaustMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    turbineExhaustMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    turbineExhaustMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mTurbineExhaust->getOrCreateStateSet();
    stateset->setAttributeAndModes( turbineExhaustMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > turbinePostCombustorMaterial = new osg::Material();
    turbinePostCombustorMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    turbinePostCombustorMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    turbinePostCombustorMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    turbinePostCombustorMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    turbinePostCombustorMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mTurbinePostCombustor->getOrCreateStateSet();
    stateset->setAttributeAndModes( turbinePostCombustorMaterial.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Material > turbineSupportMaterial = new osg::Material();
    turbineSupportMaterial->setEmission( osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
    turbineSupportMaterial->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.45f, 0.45f, 0.45f, 1.0f ) );
    turbineSupportMaterial->setDiffuse( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.6f, 0.6f, 0.6f, 1.0f ) );
    turbineSupportMaterial->setSpecular( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    turbineSupportMaterial->setShininess( osg::Material::FRONT_AND_BACK, 10.0f );
    stateset = mTurbineSupport->getOrCreateStateSet();
    stateset->setAttributeAndModes( turbineSupportMaterial.get(), osg::StateAttribute::ON );

    //Create physics mesh for room
    //mRoom->InitPhysics();
    //mRoom->GetPhysicsRigidBody()->SetMass( 0.0 );
    //mRoom->GetPhysicsRigidBody()->setFriction( 0.5 );
    //mRoom->GetPhysicsRigidBody()->setRestitution( 0.0 );
    //mRoom->GetPhysicsRigidBody()->StaticConcaveShape();

    //Collect the shadowed nodes into a group for easy reference
    mShadowedScene = new osg::Group();
    mShadowedScene->addChild( mAluminumParts.get() );
    mShadowedScene->addChild( mAluminumPipes.get() );
    mShadowedScene->addChild( mBlack.get() );
    mShadowedScene->addChild( mBrown.get() );
    mShadowedScene->addChild( mDetails.get() );
    mShadowedScene->addChild( mFloor.get() );
    mShadowedScene->addChild( mLtGreen.get() );
    mShadowedScene->addChild( mLtGrey.get() );
    mShadowedScene->addChild( mOrange.get() );
    mShadowedScene->addChild( mRed.get() );
    mShadowedScene->addChild( mRedBrown.get() );
    mShadowedScene->addChild( mWalls.get() );
    mShadowedScene->addChild( mWhiteDucts.get() );
    mShadowedScene->addChild( mWhitePipes.get() );
    mShadowedScene->addChild( mYellow.get() );

    mShadowedScene->addChild( mBlowerComponents.get() );
    mShadowedScene->addChild( mBrackets.get() );
    mShadowedScene->addChild( mCableTray.get() );
    mShadowedScene->addChild( mCementBase.get() );
    //mShadowedScene->addChild( mCombustorInternals.get() );
    mShadowedScene->addChild( mCombustorPiping.get() );
    mShadowedScene->addChild( mCompressorInlet.get() );
    mShadowedScene->addChild( mFrame.get() );
    mShadowedScene->addChild( mGroundBolts.get() );
    mShadowedScene->addChild( mHeatExchanger.get() );
    mShadowedScene->addChild( mHeatExchangerSweep.get() );
    mShadowedScene->addChild( mInstrumentation.get() );
    mShadowedScene->addChild( mLoad.get() );
    mShadowedScene->addChild( mPlenumPiping.get() );
    mShadowedScene->addChild( mPlenumSystem.get() );
    mShadowedScene->addChild( mRailing.get() );
    mShadowedScene->addChild( mReliefPiping.get() );
    mShadowedScene->addChild( mReliefPipingAM.get() );
    mShadowedScene->addChild( mShell.get() );
    mShadowedScene->addChild( mTurbineExhaust.get() );
    mShadowedScene->addChild( mTurbinePostCombustor.get() );
    mShadowedScene->addChild( mTurbineSupport.get() );
}
////////////////////////////////////////////////////////////////////////////////
void HyperLabScene::CreateShadowTexture()
{
    mShadow = new osg::Texture2D();
    mCamera = new osg::Camera();
    mTexgenNode = new osg::TexGenNode();

    unsigned int texWidth = 4096;
    unsigned int texHeight = 4096;

    //Create the shadow texture
    mShadow->setTextureSize( texWidth, texHeight );
    mShadow->setInternalFormat( GL_DEPTH_COMPONENT );
    mShadow->setSourceType( GL_UNSIGNED_INT );

    mShadow->setShadowComparison( true );
    mShadow->setShadowCompareFunc( osg::Texture::LEQUAL );

    mShadow->setShadowTextureMode( osg::Texture::LUMINANCE );
    mShadow->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    mShadow->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    mShadow->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    mShadow->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );

    //Set up the "render to texture" camera
    {
        //Create the camera
        mCamera->setClearMask( GL_DEPTH_BUFFER_BIT );
        mCamera->setClearColor( osg::Vec4( 1.0f, 1.0f, 1.0f, 1.0f ) );
        mCamera->setComputeNearFarMode( osg::Camera::DO_NOT_COMPUTE_NEAR_FAR );

        //Set viewport
        mCamera->setViewport( 0, 0, texWidth, texHeight );

        osg::ref_ptr< osg::StateSet > localStateset = mCamera->getOrCreateStateSet();
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
        mCamera->setRenderOrder( osg::Camera::PRE_RENDER );

        //Tell the camera to use OpenGL frame buffer object where supported
        mCamera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );

        //Attach the texture and use it as the color buffer
        mCamera->attach( osg::Camera::DEPTH_BUFFER, mShadow.get() );

        //Add subgraph to render
        mCamera->addChild( mShadowedScene.get() );

        //Create the texgen node to project the tex coords onto the subgraph  
        mTexgenNode->setTextureUnit( 0 );

        osg::BoundingSphere bs;
        for( unsigned int i = 0; i < mCamera->getNumChildren(); ++i )
        {
            bs.expandBy( mCamera->getChild( i )->getBound() );
        }

        osg::Vec3 position = mLightTransform->getMatrix().getTrans();

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

        mCamera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
        mCamera->setProjectionMatrixAsFrustum( -right, right, -top, top, znear, zfar );
        mCamera->setViewMatrixAsLookAt( position, bs.center(), osg::Vec3( 0.0f, 1.0f, 0.0f ) );

        //Compute the matrix which takes a vertex from local coords into tex coords
        osg::Matrix MVPT = mCamera->getViewMatrix() *
                           mCamera->getProjectionMatrix() *
                           osg::Matrix::translate( 1.0f, 1.0f, 1.0f ) *
                           osg::Matrix::scale( 0.5f, 0.5f, 0.5f );

        //Texture Generation
        mTexgenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
        mTexgenNode->getTexGen()->setPlanesFromMatrix( MVPT );
    }

    mPluginDCS->addChild( mCamera.get() );
    mPluginDCS->addChild( mTexgenNode.get() );
}
////////////////////////////////////////////////////////////////////////////////
