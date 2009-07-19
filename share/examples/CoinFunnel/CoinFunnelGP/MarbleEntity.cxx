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
#include "MarbleEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/Sound.h>

// --- OSG Includes --- //
#include <osg/CullFace>
#include <osg/BlendFunc>
#include <osg/Texture2D>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

using namespace funnel;

////////////////////////////////////////////////////////////////////////////////
MarbleEntity::MarbleEntity(
    std::string geomFile,
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator,
    ves::xplorer::scenegraph::ResourceManager* resourceManager
#ifdef VE_SOUND
    ,
    osgAL::SoundManager* soundManager
#endif
    )
:
CADEntity( geomFile, pluginDCS, false, "Off", physicsSimulator ),
mResourceManager( resourceManager )
#ifdef VE_SOUND
,
mMarbleOnWood( new ves::xplorer::scenegraph::Sound(
                   "MarbleOnWood", GetDCS(), soundManager ) ),
mMarbleOnMetal( new ves::xplorer::scenegraph::Sound(
                    "MarbleOnMetal", GetDCS(), soundManager ) ),
mMarbleOnMarble( new ves::xplorer::scenegraph::Sound(
                    "MarbleOnMarble", GetDCS(), soundManager ) )
#endif
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
MarbleEntity::~MarbleEntity()
{
#ifdef VE_SOUND
    delete mMarbleOnWood;
    delete mMarbleOnMetal;
    delete mMarbleOnMarble;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void MarbleEntity::Initialize()
{
    osg::ref_ptr< osg::Node > catsEyeNode =
        osgDB::readNodeFile( "Models/IVEs/marble.ive" );
    GetDCS()->addChild( catsEyeNode.get() );

#ifdef VE_SOUND
    try
    {
        mMarbleOnWood->LoadFile( "Sounds/MarbleOnWood.wav" );
        mMarbleOnMetal->LoadFile( "Sounds/MarbleOnMetal.wav" );
        mMarbleOnMarble->LoadFile( "Sounds/MarbleOnMarble.wav" );
    }
    catch( ... )
    {
        std::cerr << "Could not load sounds" << std::endl;
    }
#endif

    osg::ref_ptr< osg::StateSet > marbleStateSet = new osg::StateSet();
    osg::ref_ptr< osg::StateSet > catsEyeStateSet = new osg::StateSet();

    osg::ref_ptr< osg::BlendFunc > blendFunc = new osg::BlendFunc();
    blendFunc->setFunction( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    marbleStateSet->setAttribute( blendFunc.get(), osg::StateAttribute::ON );

    marbleStateSet->setMode( GL_CULL_FACE, osg::StateAttribute::ON );
    osg::ref_ptr< osg::CullFace > cullFace = new osg::CullFace();
    cullFace->setMode( osg::CullFace::BACK );
    marbleStateSet->setAttribute( cullFace.get(), osg::StateAttribute::ON );

    marbleStateSet->setMode( GL_BLEND, osg::StateAttribute::ON );
    marbleStateSet->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
    marbleStateSet->setAttribute(
        ( mResourceManager->get< osg::Program, osg::ref_ptr >
        ( "MarbleProgram" ) ).get(), osg::StateAttribute::ON );

    catsEyeStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    catsEyeStateSet->setAttribute(
        ( mResourceManager->get< osg::Program, osg::ref_ptr >
        ( "CatsEyeProgram" ) ).get(), osg::StateAttribute::ON );

    marbleStateSet->setTextureAttributeAndModes( 1,
        ( mResourceManager->get< osg::TextureCubeMap, osg::ref_ptr >
        ( "CubeMap" ) ).get(), osg::StateAttribute::ON );

    marbleStateSet->setTextureAttributeAndModes( 2,
        ( mResourceManager->get< osg::Texture2D, osg::ref_ptr >
        ( "Rainbow" ) ).get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > environmentUniform =
        new osg::Uniform( "Environment", 1 );
    marbleStateSet->addUniform( environmentUniform.get() );

    osg::ref_ptr< osg::Uniform > rainbowUniform =
        new osg::Uniform( "Rainbow", 2 );
    marbleStateSet->addUniform( rainbowUniform.get() );
        
    GetNode()->GetNode()->setStateSet( marbleStateSet.get() );
    catsEyeNode->setStateSet( catsEyeStateSet.get() );
}
////////////////////////////////////////////////////////////////////////////////
void MarbleEntity::SetNameAndDescriptions( const std::string& name )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( name );
}
////////////////////////////////////////////////////////////////////////////////
#ifdef VE_SOUND
ves::xplorer::scenegraph::Sound* MarbleEntity::GetMarbleOnWoodSound()
{
    return mMarbleOnWood;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Sound* MarbleEntity::GetMarbleOnMetalSound()
{
    return mMarbleOnMetal;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Sound* MarbleEntity::GetMarbleOnMarbleSound()
{
    return mMarbleOnMarble;
}
#endif
////////////////////////////////////////////////////////////////////////////////
