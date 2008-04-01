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
#include "CameraPlacementToolScene.h"
#include "CameraPlacementToolShaders.h"
#include "CameraEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Texture2D>
#include <osg/TexGenNode>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //

// --- C/C++ Libraries --- //

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolScene::CameraPlacementToolScene(
    ves::xplorer::scenegraph::DCS* pluginDCS )
:
mShaders(),
mCameraEntity( 0 ),
mPluginDCS( pluginDCS ),
mGrinder( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolScene::~CameraPlacementToolScene()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
cpt::CameraEntity* CameraPlacementToolScene::GetActiveCameraEntity()
{
    return mCameraEntity.get();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolScene::Initialize()
{
    //Initialize the shader programs
    mShaders = cpt::CameraPlacementToolShadersPtr(
        new cpt::CameraPlacementToolShaders() );

    //Initialize the camera entities
    mCameraEntity = new cpt::CameraEntity( mPluginDCS.get() );

    mCameraEntity->SetNameAndDescriptions( std::string( "Camera" ) );

    mGrinder = new ves::xplorer::scenegraph::DCS();

    mGrinder->setPosition( osg::Vec3d( 0.0, 10.0, 0.0 ) );
    mGrinder->setScale( osg::Vec3d( 5.0, 5.0, 5.0 ) );

    osg::ref_ptr< osg::Node > grinder =
        osgDB::readNodeFile( std::string( "Models/toppanel.ive" ) );

    mGrinder->addChild( grinder.get() );

    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    mGrinder->setDescriptions( descriptorsList );
    mGrinder->setName( std::string( "grinder" ) );

    //mGrinder->setStateSet( mCameraEntity->getStateSet() );

    mPluginDCS->AddChild( mGrinder.get() );
}
////////////////////////////////////////////////////////////////////////////////
