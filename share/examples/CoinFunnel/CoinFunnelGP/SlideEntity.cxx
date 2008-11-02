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
#include "SlideEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/CADEntityHelper.h>

// --- OSG Includes --- //
#include <osg/Texture3D>

#include <osgDB/ReadFile>

using namespace funnel;

////////////////////////////////////////////////////////////////////////////////
SlideEntity::SlideEntity(
    std::string geomFile,
    ves::xplorer::scenegraph::DCS* pluginDCS,
    ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator,
    ves::xplorer::scenegraph::ResourceManager* resourceManager )
:
CADEntity( geomFile, pluginDCS, false, false, physicsSimulator ),
mResourceManager( resourceManager )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
SlideEntity::~SlideEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SlideEntity::Initialize()
{
    osg::ref_ptr< osg::Node > slideNode =
        osgDB::readNodeFile( "Models/IVEs/slide.ive" );
    GetDCS()->addChild( slideNode.get() );

    osg::ref_ptr< osg::StateSet > slideStateSet = new osg::StateSet();

    slideStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    slideStateSet->setAttribute(
        ( mResourceManager->get< osg::Program, osg::ref_ptr >
        ( "SlideProgram" ) ).get(), osg::StateAttribute::ON );

    slideStateSet->setTextureAttributeAndModes( 0,
        ( mResourceManager->get< osg::Texture3D, osg::ref_ptr >
        ( "NoiseVolume" ) ).get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > noiseUniform = new osg::Uniform( "Noise", 0 );
    slideStateSet->addUniform( noiseUniform.get() );
        
    GetDCS()->setStateSet( slideStateSet.get() );
    slideNode->setStateSet( slideStateSet.get() );
}
////////////////////////////////////////////////////////////////////////////////
void SlideEntity::SetNameAndDescriptions( const std::string& name )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    GetDCS()->setDescriptions( descriptorsList );
    GetDCS()->setName( name );
}
////////////////////////////////////////////////////////////////////////////////
