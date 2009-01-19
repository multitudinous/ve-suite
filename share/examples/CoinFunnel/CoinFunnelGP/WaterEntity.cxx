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
#include "WaterEntity.h"
#include "TimeCallback.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/Sound.h>

// --- osgAL Includes --- //
#ifdef VE_SOUND
#include <osgAL/SoundState>
#endif

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/BlendFunc>
#include <osg/Texture3D>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

using namespace funnel;

////////////////////////////////////////////////////////////////////////////////
WaterEntity::WaterEntity( std::string geomFile,
                          ves::xplorer::scenegraph::DCS* pluginDCS,
                          ves::xplorer::scenegraph::ResourceManager* resourceManager
#ifdef VE_SOUND
                          ,
                          osgAL::SoundManager* soundManager
#endif
                          )
:
CADEntity( geomFile, pluginDCS ),
mResourceManager( resourceManager )
#ifdef VE_SOUND
,
mWaterSound( new ves::xplorer::scenegraph::Sound(
                 "Water", GetDCS(), soundManager ) )
#endif
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
WaterEntity::~WaterEntity()
{
#ifdef VE_SOUND
    delete mWaterSound;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void WaterEntity::Initialize()
{
#ifdef VE_SOUND
    try
    {
        mWaterSound->LoadFile( "Sounds/Water.wav" );
        mWaterSound->GetSoundState()->setLooping( true );
    }
    catch( ... )
    {
        std::cerr << "Could not load sounds" << std::endl;
    }
#endif

    osg::ref_ptr< osg::StateSet > waterStateSet = new osg::StateSet();

    waterStateSet->setMode( GL_BLEND, osg::StateAttribute::ON );
    waterStateSet->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
    waterStateSet->setAttribute(
        ( mResourceManager->get< osg::Program, osg::ref_ptr >
        ( "WaterProgram" ) ).get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::BlendFunc > blendFunc = new osg::BlendFunc();
    blendFunc->setFunction( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    waterStateSet->setAttribute( blendFunc.get(), osg::StateAttribute::ON );

    waterStateSet->setTextureAttributeAndModes( 0,
        ( mResourceManager->get< osg::Texture3D, osg::ref_ptr >
        ( "NoiseVolume" ) ).get(), osg::StateAttribute::ON );

    waterStateSet->setTextureAttributeAndModes( 1,
        ( mResourceManager->get< osg::TextureCubeMap, osg::ref_ptr >
        ( "CubeMap" ) ).get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > noiseUniform =
        new osg::Uniform( "noise", 0 );
    waterStateSet->addUniform( noiseUniform.get() );

    osg::ref_ptr< osg::Uniform > skyBoxUniform =
        new osg::Uniform( "skyBox", 1 );
    waterStateSet->addUniform( skyBoxUniform.get() );

    osg::ref_ptr< osg::Uniform > timeUniform =
        new osg::Uniform( "time", static_cast< float >( 0.0 ) );
    osg::ref_ptr< funnel::TimeCallback > timeCallback =
        new funnel::TimeCallback();
    timeUniform->setUpdateCallback( timeCallback.get() );
    waterStateSet->addUniform( timeUniform.get() );

    GetNode()->GetNode()->setStateSet( waterStateSet.get() );
}
////////////////////////////////////////////////////////////////////////////////
void WaterEntity::SetNameAndDescriptions( const std::string& name )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( name );
}
////////////////////////////////////////////////////////////////////////////////
