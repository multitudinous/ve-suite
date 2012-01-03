/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include <ves/xplorer/network/VE_i.h>
#include "TankGraphicalPlugin.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/device/KeyboardMouse.h>
#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>
#include <gadget/Type/KeyboardMouseInterface.h>

// --- OSG Includes --- //
#include <osg/MatrixTransform>
#include <osg/AnimationPath>
#include <osg/ShapeDrawable>
#include <osg/Sequence>

#include <osgText/Text>

#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

#include <osgSim/ColorRange>
#include <osg/Vec3d>

#include <boost/lexical_cast.hpp>

using namespace opcgp;

////////////////////////////////////////////////////////////////////////////////
TankGraphicalPlugin::TankGraphicalPlugin()
    :
    ves::xplorer::plugin::PluginBase(),
    m_keyboard( 0 )
{
    //DYNSIM
    mObjectName = "DSPlugin";
    //mObjectName = "tank";
    mEventHandlerMap[ "OPCData" ] = this;
    //mEventHandlerMap[ "TANK_CAD" ] = this;
    mEventHandlerMap[ "VALVE_CAD" ] = this;
    m_tankDCS = 0;
}
////////////////////////////////////////////////////////////////////////////////
TankGraphicalPlugin::~TankGraphicalPlugin()
{
}
////////////////////////////////////////////////////////////////////////////////
void TankGraphicalPlugin::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    m_keyboard = 
        dynamic_cast< ves::xplorer::device::KeyboardMouse* >( mDevice );
        
    //do we want to provide default CAD?
    //If so initialize
    m_tankGeometry = osgDB::readNodeFile( "Tank/tank.ive" );
    td = new TankData( m_tankGeometry.get() );
    td->setColorMethod( TankData::COLOR_EXPLICIT );
    td->setExplicitColor( osg::Vec4( .2, 1., .8, .4 ) );
    td->setPercentOfCapacity( 1.0f );
    m_tankDCS = new ves::xplorer::scenegraph::DCS();
    m_tankDCS->addChild( m_tankGeometry.get() );
    td->setPercentOfCapacity( 1.0 );
    double scale[3] = { 3.28, 3.28, 3.28 };
    m_tankDCS->SetScaleArray( scale );
    mDCS->addChild( m_tankDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
void TankGraphicalPlugin::PreFrameUpdate()
{
    //check for TankData and that the value has changed
    //if( m_tanklevel changed )
    if( m_tankDCS != 0 )
    {
        //set how full the tank is 0-1
        td->setPercentOfCapacity( m_tankLevel );
    }
}
////////////////////////////////////////////////////////////////////////////////
void TankGraphicalPlugin::SetCurrentCommand(
    ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }

    //currently there is no functional flowsheet with a tank
    //check for the tank dvp
    if( command->GetDataValuePair("MY_VALVE") )  //change to "MY_TANK"
    {
        //the value for the tank level
        std::string percent;
        //change to "MY_TANK"
        command->GetDataValuePair("MY_VALVE")->GetData( percent );
        m_tankLevel = boost::lexical_cast<double>( percent );
    }

    //load tank CAD
    //currently no tank demo exists so we are using the valve values
    if( command->GetCommandName( ).compare( "VALVE_CAD" ) == 0 )
    //if( command->GetCommandName( ).compare( "TANK_CAD" ) == 0 )
    {
        if( m_tankDCS != 0 )
        {
            mDCS->removeChild( m_tankDCS.get() );
        }
        std::string CAD;
        command->GetDataValuePair("VALVE_BODY")->GetData( CAD );
        //command->GetDataValuePair("TANK")->GetData( CAD );
        m_tankGeometry = osgDB::readNodeFile( CAD.c_str() );

        //instantiate the TankData
        td = new TankData( m_tankGeometry.get() );
        td->setColorMethod( TankData::COLOR_EXPLICIT );
        td->setExplicitColor( osg::Vec4( .2, 1., .8, .4 ) );
        td->setPercentOfCapacity( 1.0f );
        m_tankDCS = new ves::xplorer::scenegraph::DCS();
        m_tankDCS->addChild( m_tankGeometry.get() );
        td->setPercentOfCapacity( 1.0 );
        mDCS->addChild( m_tankDCS.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void TankGraphicalPlugin::SetCurrentCommands(
    std::vector< ves::open::xml::CommandPtr > const& commands )
{
    if( commands.empty() )
    {
        return;
    }
    ves::open::xml::CommandPtr tempPtr = commands.at( commands.size() - 1 );
    SetCurrentCommand( tempPtr );
}
////////////////////////////////////////////////////////////////////////////////
void TankGraphicalPlugin::FindPartNodeAndHighlightNode()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
