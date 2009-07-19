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
#include "FunnelEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

// --- OSG Includes --- //
#include <osg/Texture3D>

#include <osgDB/ReadFile>

using namespace funnel;

////////////////////////////////////////////////////////////////////////////////
FunnelEntity::FunnelEntity(
    std::string geomFile,
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator,
    ves::xplorer::scenegraph::ResourceManager* resourceManager )
:
CADEntity( geomFile, pluginDCS, false, "Off", physicsSimulator ),
mResourceManager( resourceManager )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
FunnelEntity::~FunnelEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FunnelEntity::Initialize()
{
    osg::ref_ptr< osg::Group > baseGroup = new osg::Group();
    osg::ref_ptr< osg::Group > columnGroup = new osg::Group();
    osg::ref_ptr< osg::Node > base =
        osgDB::readNodeFile( "Models/IVEs/base.ive" );
    osg::ref_ptr< osg::Node > column =
        osgDB::readNodeFile( "Models/IVEs/column.ive" );
    osg::ref_ptr< osg::Node > columnTop =
        osgDB::readNodeFile( "Models/IVEs/column_top.ive" );
    osg::ref_ptr< osg::Node > columnBase =
        osgDB::readNodeFile( "Models/IVEs/column_base.ive" );
    osg::ref_ptr< osg::Node > columnDetail =
        osgDB::readNodeFile( "Models/IVEs/column_detail.ive" );

    baseGroup->addChild( base.get() );
    columnGroup->addChild( column.get() );
    columnGroup->addChild( columnTop.get() );
    columnGroup->addChild( columnBase.get() );
    columnGroup->addChild( columnDetail.get() );

    GetDCS()->addChild( baseGroup.get() );
    GetDCS()->addChild( columnGroup.get() );

    osg::ref_ptr< osg::StateSet > baseStateSet = new osg::StateSet();
    osg::ref_ptr< osg::StateSet > columnStateSet = new osg::StateSet();

    baseStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    baseStateSet->setAttribute(
        ( mResourceManager->get< osg::Program, osg::ref_ptr >
        ( "FunnelProgram" ) ).get(), osg::StateAttribute::ON );

    columnStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    columnStateSet->setAttribute(
        ( mResourceManager->get< osg::Program, osg::ref_ptr >
        ( "FunnelProgram" ) ).get(), osg::StateAttribute::ON );

    baseStateSet->setTextureAttributeAndModes( 0,
        ( mResourceManager->get< osg::Texture3D, osg::ref_ptr >
        ( "NoiseVolume" ) ).get(), osg::StateAttribute::ON );

    columnStateSet->setTextureAttributeAndModes( 0,
        ( mResourceManager->get< osg::Texture3D, osg::ref_ptr >
        ( "NoiseVolume" ) ).get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > baseScaleUniform = new osg::Uniform(
        "scale", static_cast< float >( 0.5 ) );
    osg::ref_ptr< osg::Uniform > columnScaleUniform = new osg::Uniform(
        "scale", static_cast< float >( 0.1 ) );
    baseStateSet->addUniform( baseScaleUniform.get() );
    columnStateSet->addUniform( columnScaleUniform.get() );

    osg::ref_ptr< osg::Uniform > noiseUniform = new osg::Uniform( "noise", 0 );
    baseStateSet->addUniform( noiseUniform.get() );
    columnStateSet->addUniform( noiseUniform.get() );

    osg::ref_ptr< osg::Uniform > baseBaseColorUniform = new osg::Uniform(
        "baseColor", osg::Vec4( 1.0, 1.0, 0.842105, 1.0 ) );
    osg::ref_ptr< osg::Uniform > baseVeinColorUniform = new osg::Uniform(
        "veinColor", osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    baseStateSet->addUniform( baseBaseColorUniform.get() );
    baseStateSet->addUniform( baseVeinColorUniform.get() );

    osg::ref_ptr< osg::Uniform > columnBaseColorUniform =
        new osg::Uniform( "baseColor", osg::Vec4( 0.1, 0.1, 0.1, 1.0 ) );
    osg::ref_ptr< osg::Uniform > columnVeinColorUniform =
        new osg::Uniform( "veinColor", osg::Vec4( 0.95, 0.92, 0.84, 1.0 ) );
    columnStateSet->addUniform( columnBaseColorUniform.get() );
    columnStateSet->addUniform( columnVeinColorUniform.get() );

    GetNode()->GetNode()->setStateSet( baseStateSet.get() );
    baseGroup->setStateSet( baseStateSet.get() );
    columnGroup->setStateSet( columnStateSet.get() );
}
////////////////////////////////////////////////////////////////////////////////
void FunnelEntity::SetNameAndDescriptions( const std::string& name )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( name );
}
////////////////////////////////////////////////////////////////////////////////
